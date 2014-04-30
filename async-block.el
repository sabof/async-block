;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun ab-queue (&optional ammount)
  (declare (indent 0))
  "When called with no arguments, it increments an internal counter
by 1, and returns a function that will decrement it by one. If
AMMOUT is provided, the counter will be changed by that number
instead.

Should the counter be 0 after the change, the next form is
executed. A step may contain multiple `ab-queue' calls. Should a
step leave a non-zero counter, the queue won't progress. A
decreasing call to `ab-queue' has to be the last thing a step
does, otherwise the result might not be what you expect."
  (error "Stub which enables eldoc and documentation"))

(defun ab-dequeue ()
  "Same as \(ab-queue -1\)"
  (error "Stub which enables eldoc and documentation"))

(defmacro ab-enqueue (&rest body)
  "Increase the queue variable by 1, and execute the BODY"
  (declare (indent 0))
  `(progn (ab-queue)
          ,@body))

(defmacro ab-wait (interval &rest body)
  (declare (indent 1))
  (if (not body)
      `(run-with-timer ,interval nil (ab-queue))
    (let (( interval-sym (cl-gensym))
          ( predicate-sym (cl-gensym))
          ( buffer-sym (cl-gensym)))
      `(let ((,interval-sym ,interval)
             (,buffer-sym (current-buffer)))
         (cl-labels (( ,predicate-sym ()
                       ,@body)
                     ( func ()
                       (with-current-buffer
                           (if (buffer-live-p ,buffer-sym)
                               ,buffer-sym
                             (current-buffer))
                         (if (,predicate-sym)
                             (ab-queue -1)
                           (run-with-timer ,interval-sym
                                           nil #'func)))))
           (ab-queue)
           (func))))))

(defmacro ab-while (interval test &rest body)
  (declare (indent 2))
  `(ab-wait ,interval
     (if ,test
         (progn
           ,@body
           nil)
       t)))

(defmacro async-block (&rest forms)
  "This macro will only work if `lexical-binding' is enabled. To better

FIXME: Use link syntax

understand how it works, have a look at documentation for `ab-queue'.

When quoting `ab-*' functions, the function quote (#') must be used.

A function `ab-queue', as well as several higher-level constructs
are available within the body of this macro. `ab-queue' has the
following signature:


FIXME: move elsewhere.

You can find examples of `ab-queue' usage, as well as examples
for `ab-wait', `ab-while', `ab-enqueue', `ab-dequeue', in the
same file as the definition of this macro."
  (declare (indent 0))
  (let* (( next-action-sym (cl-gensym))
         ( ab-queue-var-sym (cl-gensym))
         ( current-buffer-sym (cl-gensym))
         ( current-buffer-accessor-sym (cl-gensym))
         ( actions-value
           (cons 'list (mapcar (lambda (form)
                                 (macroexpand-all
                                  `(lambda nil ,form)))
                               forms))))
    `(let* ((,ab-queue-var-sym 0)
            (,current-buffer-sym (current-buffer))
            ,next-action-sym)
       (cl-labels
           (( ab-queue (&optional ammount)
              (cl-incf ,ab-queue-var-sym (or ammount 1))
              (when (cl-minusp ,ab-queue-var-sym)
                (error "The queue variable may not go below 0"))
              (when (zerop ,ab-queue-var-sym)
                (funcall ,next-action-sym))
              (lambda (&rest ignore)
                (ab-queue -1)))
            ( ab-dequeue ()
              (ab-queue -1))
            ( ,current-buffer-accessor-sym (&rest args)
              (if (length args)
                  (setq ,current-buffer-sym (car args))
                ,current-buffer-sym))
            ( ab-seed ()
              (list #'ab-queue
                    #',current-buffer-accessor-sym)))
         (let ((actions ,actions-value))
           (funcall (setq ,next-action-sym
                          (lambda ()
                            (while (and actions (zerop ,ab-queue-var-sym))
                              (with-current-buffer
                                  (if (buffer-live-p ,current-buffer-sym)
                                      ,current-buffer-sym
                                    (current-buffer))
                                (funcall (pop actions))
                                (setq ,current-buffer-sym (current-buffer)))))))
           )))))

(defmacro async-block-continue (seed &rest body)
  "Allows splitting of async blocks across multiple funcitons.
If return value of `ab-seed' from the caller is supplied, the calling block
will wait until the execution of this block is complete.

Should SEED be nil, acts like regular `async-block'"
  (declare (indent 1))
  (let ((seed-sym (cl-gensym)))
    `(let ((,seed-sym ,seed))
       (async-block
         (when ,seed-sym
           (funcall (cl-first ,seed-sym) 1))
         ,@body
         (when ,seed-sym
           (funcall (cl-second ,seed-sym) (current-buffer))
           (funcall (cl-first ,seed-sym) -1))
         ))))

;; FIXED: Use ab-seed to transmit ab-queue
;; FIXME: Add font-locking
;; FIXME: Documentation
;; FIXME: Error handling can be improved?
;; FIXME: Recursive function

(provide 'async-block)
;;; async-block.el ends here
