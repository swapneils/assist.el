;;; assist.el/assist.el -*- lexical-binding: t; -*-

;; (require 'emacsql)
;; (require 'emacsql-sqlite)
;; (require 'emacsql-sqlite-common)

;; (defvar assistant-folder (concat (if (boundp 'org-roam-mode)
;;                                      org-roam-directory
;;                                    org-directory)
;;                                  "/assistant"))

(defvar assist--internal-execution nil
  "Tracks when assist functions are being called by other
assist functions.")


;;; Event tracking
(cl-defstruct (assist-event
               (:constructor assist--make-event)
               (:type list)
               :named)
  "An event object for `assist.el'"
  (predicate nil :documentation "A function determining if the ")
  (action nil)
  (action-args nil :documentation "A function generating arguments for the action slot.")
  (action-literal-args nil
                       :type 'list
                       :documentation "The initial literal list of arguments provided when creating the event")
  (priority nil :documentation "A value used to sort this event in `assist--event-sorter'")
  (group nil :documentation "An arbitrary label used to better organize and find events."))

(defmacro assist-event (group
                        priority
                        predicate
                        action &rest action-args)
  "A macro for creating `assist' events.

GROUP is an input attaching a group to an event. Provide nil to not add any group.

PRIORITY determines how the event will be sorted in the event queue.
The below determines its behavior if `assist--event-sorter' is not overriden:
- PRIORITY `t' events will always be at the front of the queue.
- PRIORITY `nil' events will always be at the back of the queue.
- PRIORITY set to a number sorts those numbers in decreasing order between the above two groups.
  - Higher numbers will be closer to the front of the queue, and so have priority.

PREDICATE is a function which should return true if the event being watched for has occurred.

ACTION is the function which is called in response to the event.

ACTION-ARGS are a series of forms. These forms will be called in sequence
and their results used as the input arguments to ACTION, _at the time of
calling ACTION_.
For instance, (assist-event nil nil pred action (current-time)) will call
`action' with the time at which it was called, not the time at which the
event was created."
  (concatenate 'list
               `(assist--make-event
                 :predicate ,predicate
                 :action ,action
                 :action-args (lambda () (list ,@action-args))
                 :action-literal-args ',action-args)
               (when group `(:group ,group))
               (when priority `(:priority ,priority))))

(defvar assist-event-queue nil
  "An environment variable tracking the event queue for assist.
Some event loops may be assigned custom values for this variable; see `assist-event-loop'.")
(defvar assist-event-loops nil
  "A list of event loop threads for assist.
May or may not include inactive threads.")
(defvar assist--event-loop-active nil
  "An internal variable used to determine whether to quit an event loop")
(defvar assist-event-loop-delay 2
  "The delay in seconds between executions of an event loop thread.")

(defvar assist-recovery-idle-timer nil
  "The timer used to recover assist-event-loop threads which have been
quit improperly (e.g. via C-g)")
(defvar assist-recovery-idle-time 5
  "The duration of `assist-recovery-idle-timer'")

(defvar assist--event-sorter
  (lambda (a b)
    (cond ((and (eq t a) (eq t b)) t)
          ((and a (not b)) t)
          ((and b (not a)) nil)
          ((and (numberp a) (numberp b)) (>= a b))))
  "A lambda function comparing 2 events for purposes of sorting.

This is stored as a lambda so that it can be easily overriden by users, if desired.")

(defun assist-write-error (str &rest data)
  (with-current-buffer (get-buffer-create "*assist-error-buffer*")
    (emacs-lock-mode 'kill)
    (end-of-buffer)
    (insert (apply #'format str data))
    (newline)))

(defun assist-match-event (queue template-event slot-list)
  "Find all events in QUEUE that match TEMPLATE-EVENT for the slots in SLOT-LIST."
  ;; Filter through the queue
  (cl-remove-if-not (lambda (event)
                      (and
                       ;; Ensure that the member of the queue
                       ;; is an event object
                       (assist-event-p event)
                       ;; Every targeted slot must match
                       ;; between event and template
                       (every (lambda (slot-name)
                                (equal
                                 (cl-struct-slot-value 'assist-event slot-name
                                                       event)
                                 (cl-struct-slot-value 'assist-event slot-name
                                                       template-event)))
                              slot-list)))
                    queue))

(defun assist--kill-event-loop (event-loop-thread)
  (setq assist-event-loops
        (remove-if (lambda (assist-event-loop)
                     (member event-loop-thread
                             assist-event-loop))
                   assist-event-loops)))
(defun assist--event-loop ()
  "Internal function executing an event loop in `assist.el'"
  (condition-case loop-error
      (progn
        (while assist--event-loop-active
          ;; Sort the event queue
          (setq assist-event-queue (sort assist-event-queue assist--event-sorter))

          ;; Find active events in the queue
          (let ((active-queue (cl-remove-if-not (lambda (event)
                                                  ;; Check event predicate
                                                  (funcall (assist-event-predicate event)))
                                                assist-event-queue)))
            ;; For each active event, run the action
            (mapcar (lambda (active-event)
                      (condition-case event-error
                          (apply (assist-event-action active-event)
                                 ;; Generate action arguments
                                 (funcall (assist-event-action-args active-event)))
                        (error (assist-write-error "Error executing event %s:
%s"
                                                   active-event
                                                   event-error))))
                    active-queue)

            ;; Remove active events from the queue
            (setq assist-event-queue
                  (cl-remove-if (lambda (event) (member event active-queue)) assist-event-queue))
            ;; Remove duplicate events from queue, to mitigate possible bad states
            (setq assist-event-queue
                  (cl-remove-duplicates assist-event-queue :test #'equal)))

          ;; Yield to other threads
          (sleep-for assist-event-loop-delay))
        ;; Remove this loop from the list of tracked event loops
        (assist--kill-event-loop (current-thread)))
    (error (assist-write-error "
Error in loop for thread %s.
Queue: %s
Error:
%s

"
                               (current-thread)
                               assist-event-queue
                               loop-error))
    (quit (assist-write-error "
Quit affecting loop for thread %s.
Queue: %s
Error:
%s

"
                              (current-thread)
                              assist-event-queue
                              loop-error))))
(defun assist-event-loop (&optional queue)
  "Run an event loop for assist.

If provided, QUEUE is a list which will be treated as the event queue
for this event loop. Once this queue is empty the thread will exit.
If not provided, the global variable `assist-event-queue' will be used,
and the thread will have to be manually exited by pushing a quit action
to the queue (see `assist-add-quit-to-queue')"
  (interactive)
  (if (and (not assist--internal-execution)
           ;; Check if the specified queue is already being tracked
           (some (lambda (loop-spec) (member queue loop-spec))
                 assist-event-loops))
      (warn "Loop already exists for queue %s, aborting loop creation" queue)
    (let* ((thread-func (if queue
                            ;; If queue is provided, use it instead
                            ;; of the global queue, and add a quit
                            ;; task for when it is emptied.
                            (lambda ()
                              (let ((assist--event-loop-active t)
                                    (assist-event-queue queue))
                                ;; Push quit task to queue
                                (push
                                 (assist-event nil nil
                                               (lambda ()
                                                 ;; Filter out non-event member
                                                 (setq assist-event-queue
                                                       (cl-remove-if-not #'assist-event-p
                                                                         assist-event-queue))
                                                 ;; Check if this is the only member
                                                 (<= (length assist-event-queue) 1))
                                               ;; Quit the event loop
                                               (lambda () (setq assist--event-loop-active nil)))
                                 assist-event-queue)
                                (assist--event-loop)))
                          ;; Otherwise use the global queue
                          (lambda ()
                            (let ((assist--event-loop-active t))
                              (assist--event-loop)))))
           (thread (make-thread thread-func)))
      ;; Clear out dead threads from the loop tracking list
      (setq assist-event-loops (cl-remove-if-not
                                #'thread-live-p assist-event-loops
                                :key #'first))
      ;; Push the new thread to the loop tracking list
      ;; Note that the second value is `nil' iff `queue' is not provided
      (push (list thread queue) assist-event-loops)

      ;; Ensure that the recovery idle timer is running
      (unless (and assist-recovery-idle-timer
                   ;; Check if idle timer is active
                   (member assist-recovery-idle-timer
                           timer-idle-list))
        (setq assist-recovery-idle-timer
              (run-with-idle-timer assist-recovery-idle-time
                                   t
                                   (lambda ()
                                     (let ((assist--internal-execution t))
                                       (mapcar
                                        (lambda (loop-spec)
                                          ;; Restart the loop
                                          (if (second loop-spec)
                                              ;; Specify a queue if one was provided originally
                                              (assist-event-loop
                                               (second loop-spec))
                                            (assist-event-loop)))
                                        ;; Only look at improperly-closed loops
                                        (cl-remove-if #'thread-live-p
                                                      assist-event-loops
                                                      :key #'first)))))))

      ;; Return the newly-created thread
      thread)))

(defun assist-add-quit-to-queue ()
  "Convenience function to push a quit task to `assist-event-queue'"
  (interactive)
  (push (assist-event nil t (lambda () t) (lambda () (setq assist--event-loop-active nil)))
        assist-event-queue))

(provide 'assist)
