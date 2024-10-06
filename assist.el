;;; assist.el/assist.el -*- lexical-binding: t; -*-

;; (require 'emacsql)
;; (require 'emacsql-sqlite)
;; (require 'emacsql-sqlite-common)

;; (defvar assistant-folder (concat (if (boundp 'org-roam-mode)
;;                                      org-roam-directory
;;                                    org-directory)
;;                                  "/assistant"))

(require 'dash)

(defvar assist--internal-execution nil
  "Tracks when assist functions are being called by other
assist functions.")

(require 'cl-lib)

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
  (cl-concatenate 'list
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
(defvar assist-memory nil
  "A global variable storing memory information for assist.")

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
  (let ((msg (apply #'format str data)))
    (with-current-buffer (get-buffer-create "*assist-error-buffer*")
      (emacs-lock-mode 'kill)
      (end-of-buffer)
      (insert msg)
      (newline))
    (message msg)))

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
      (let (spawned-threads)
        (unwind-protect
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
                  (cl-mapcar (lambda (active-event)
                               (push
                                ;; Create a thread to run the action in
                                (make-thread
                                 (lambda ()
                                   (let ((inhibit-quit t))
                                     (condition-case event-error
                                         (apply (assist-event-action active-event)
                                                ;; Generate action arguments
                                                (funcall (assist-event-action-args active-event)))
                                       (error (assist-write-error
                                               "Error executing event %s:
%s"
                                               active-event
                                               event-error)))))
                                 ;; Name the thread after its action
                                 (format "%S with argforms: %S"
                                         (assist-event-action active-event)
                                         (assist-event-action-args active-event)))
                                ;; Add the new thread to the spawned thread list
                                spawned-threads))
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

          ;; Kill any still-running threads spawned by this one
          (mapcar (lambda (thread)
                    (when (thread-live-p thread)
                        (signal thread 'error "manager process aborted")))
                  spawned-threads)))
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
Quit interrupted loop for thread %s.
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

  ;; Ensure that the recovery idle timer is running
  (unless (and assist-recovery-idle-timer
               ;; Check if idle timer is active
               (member assist-recovery-idle-timer
                       timer-idle-list))
    (assist-start-recovery-timer))

  (if (and (not assist--internal-execution)
           ;; Check if the specified queue is already being tracked
           (cl-some (lambda (loop-spec) (member queue loop-spec))
                    assist-event-loops))
      (warn "Loop already exists for queue %s, aborting loop creation" queue)
    (let* ((thread-func (if queue
                            ;; If queue is provided, use it instead
                            ;; of the global queue, and add a quit
                            ;; task for when it is emptied.
                            (lambda ()
                              (let ((inhibit-quit t)
                                    (assist--event-loop-active t)
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
                            (let ((inhibit-quit t)
                                  (assist--event-loop-active t))
                              (assist--event-loop)))))
           (thread (make-thread thread-func)))
      ;; Clear out dead threads from the loop tracking list
      (setq assist-event-loops (cl-remove-if-not
                                #'thread-live-p assist-event-loops
                                :key #'cl-first))
      ;; Push the new thread to the loop tracking list
      ;; Note that the second value is `nil' iff `queue' is not provided
      (push (list thread queue) assist-event-loops)

      ;; Return the newly-created thread
      thread)))
(defun assist-start-recovery-timer ()
  ;; Cancel any previous timers
  (when (and assist-recovery-idle-timer
             (member assist-recovery-idle-timer timer-idle-list))
    (cancel-timer assist-recovery-idle-timer))

  (setq assist-recovery-idle-timer
        (run-with-idle-timer assist-recovery-idle-time
                             ;; Keep running when reaching required idle time
                             t
                             (lambda ()
                               ;; Set environment to keep event loops active
                               (let ((assist--internal-execution t))
                                 (mapcar
                                  (lambda (loop-spec)
                                    ;; Restart the loop
                                    (if (second loop-spec)
                                        ;; Specify a queue if one was originally provided
                                        (assist-event-loop
                                         (second loop-spec))
                                      (assist-event-loop))
                                    (assist-write-error "

Restarted loop for spec:
%s
"
                                                        loop-spec))
                                  ;; Only look at improperly-closed loops
                                  (cl-remove-if #'thread-live-p
                                                assist-event-loops
                                                :key #'cl-first)))))))

(defun assist-add-quit-to-queue ()
  "Convenience function to push a quit task to `assist-event-queue'"
  (interactive)
  (push (assist-event nil t (lambda () t) (lambda () (setq assist--event-loop-active nil)))
        assist-event-queue))

(defmacro assist-define-events (event-group-name &rest chains)
  "Defines a network of events in `assist-event-queue'.

EVENT-GROUP-NAME is a symbol used to label the generated events.

Each member of CHAINS is a list of forms interspersed
with the `->' and `=>' arrows. If a form is preceded by
`=>' it is interpreted to be an action; otherwise it
is interpreted as a predicate.

Each chain is executed in order; a predicate is only
checked, and an action only taken, if its prior predicates
and actions have all been fulfilled.

Predicates are interpreted as the bodies of a single-form
no-argument lambda, which will be used as the predicate
in an `assist-event' object.

Actions are interpreted as a list whose first member is
a function and whose subsequent member are forms which
will be used to generate the arguments of that function."
  (cl-labels
      ((print-equal
        (a b)
        (equal (format "%s" a)
               (format "%s" b)))
       (translate-event-chain
        (chain event-chain-index-symbol)
        (let (form-cache
              event-cache
              (form-next t)
              (action-next-form nil))
          (cl-loop
           for elt in chain
           do
           (progn
             ;; Is `elt' a form or an arrow?
             (if form-next
                 ;; Add the form to our cache of forms
                 (push elt form-cache)
               ;; On a => the next form is an action
               (setq action-next-form (print-equal elt '=>)))

             ;; When at an action form, make an event
             (when (and form-next action-next-form)
               (let ((pred-count
                      ;; Note: incrementing this by one
                      ;; to account for the predicate we
                      ;; add to track the index in this chain
                      (length form-cache))
                     (action (car form-cache))
                     (pred-cache (->>
                                  ;; Sort predicates chronologically
                                  (nreverse (cdr form-cache))
                                  ;; Convert predicates to lambdas
                                  (mapcar (lambda (body) (list 'lambda nil body)))
                                  ;; Start by verifying that prior
                                  ;; events in this chain are already
                                  ;; completed, based on the local
                                  ;; `event-chain-index-symbol' variable
                                  (cons
                                   `(lambda ()
                                      (>= ,event-chain-index-symbol
                                          ,(length event-cache))))))
                     (pred-values (gensym "pred-values")))
                 ;; Push code specifying an event object to event cache
                 (push
                  `(assist-event
                    ',event-group-name 0
                    ;; Predicate function
                    (let ((,pred-values
                           ;; Store a cache value for each predicate
                           ;; to track if it's ever been fulfilled.
                           ;; Once a predicate is fulfilled it doesn't
                           ;; matter if it stays so, so this cache
                           ;; will be used to track prior fulfillment.
                           ',(cl-loop
                              for i below pred-count
                              collect nil)))
                      (lambda ()
                        ;; Update the predicate value list
                        (cl-loop
                         for i below (length ,pred-values)
                         ;; Continue until we reach a predicate
                         ;; that isn't fulfilled
                         while (or
                                ;; Either the predicate was
                                ;; satisfied previously
                                (elt ,pred-values i)
                                ;; Or it is satisfied now
                                (funcall (elt (list ,@pred-cache) i)))
                         ;; Set the tracking variable for the
                         ;; current predicate to `t'
                         do (setf (elt ,pred-values i) t))
                        ;; Check if every predicate has been satisfied
                        (cl-every #'identity ,pred-values)))
                    ;; Action function
                    (lambda (&rest args)
                      ;; Execute the provided action
                      (prog1 (apply ,(car action) args)
                        ;; Increment `event-chain-index-symbol' to track
                        ;; how many events in this event chain
                        ;; have been fulfilled so far
                        (cl-incf ,event-chain-index-symbol)))
                    ;; Action function input forms, if any
                    ,@(cdr action))
                  event-cache))
               ;; Reset form cache
               (setq form-cache nil)
               (message "event cache: %S" event-cache))

             ;; Toggle whether the next element is a form
             (setq form-next (not form-next))))
          ;; Return list of event objects
          event-cache))
       (write-event-chain
        (chain)
        (let* ((event-chain-index-symbol
                (gensym (concat
                         "event-chain-index-symbol" "::"
                         (format "%S" event-group-name)
                         "-")))
               (chain (translate-event-chain chain
                                             event-chain-index-symbol)))
          `(progn
             ;; Initialize `event-chain-index-symbol' for this chain
             (setq ,event-chain-index-symbol 0)
             ;; Insert statements to add individual events
             ;; to `assist-event-queue'
             ,@(cl-loop
                for event in chain
                collect
                `(push ,event assist-event-queue))))))
    (let ((event-chains (mapcar #'write-event-chain chains)))
      `(progn
         ;; Insert all the event chains
         ,@event-chains))))

(provide 'assist)
