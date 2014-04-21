;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun ab-queue (&optional ammount))

(defun ab-dequeue ())

(defmacro ab-enqueue (&rest body)
  `(progn (ab-queue)
          ,@body))

(put 'ab-enqueue 'common-lisp-indent-function
     '(&body))
(put 'ab-enqueue 'lisp-indent-function
     0)

(defmacro ab-wait (interval &rest body)
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

(put 'ab-wait 'common-lisp-indent-function
     '(4 &body))
(put 'ab-wait 'lisp-indent-function
     1)

(defmacro ab-while (interval test &rest body)
  `(ab-wait ,interval
     (if ,test
         (progn
           ,@body
           nil)
       t)
     ))

(put 'ab-while 'lisp-indent-function
     2)
(put 'ab-while 'common-lisp-indent-function
     '(4 4 &body))

(defmacro async-block (&rest forms)
  "This macro will only work if `lexical-binding' is enabled.
It executes the top-level forms one by one.

A function `ab-queue', as well as several higher-level constructs
are available within the body of this macro. `ab-queue' has the
following signature:

  (ab-queue &optional AMMOUNT)

When called with no arguments, it increments an internal counter
by 1, and returns a function that will decrement it by one. If
AMMOUT is provided, the counter will be changed by that number
instead.

Should the counter be 0 after the change, the next form is
executed. A step may contain multiple `ab-queue' calls. Should a
step leave a non-zero counter, the queue won't progress. A
decreasing call to `ab-queue' has to be the last thing a step
does, otherwise the result might not be what you expect.

FIXME: move elsewhere.

You can find examples of `ab-queue' usage, as well as examples
for `ab-wait', `ab-while', `ab-enqueue', `ab-dequeue', in the
same file as the definition of this macro."
  (let* (( next-action (cl-gensym))
         ( ab-queue-var (cl-gensym))
         ( actions-value
           (cons 'list (mapcar (lambda (form)
                                 (macroexpand-all
                                  `(lambda nil ,form)))
                               forms))))
    `(let* ((,ab-queue-var 0)
            ,next-action)
       (cl-labels
           (( ab-queue (&optional ammount)
              (cl-incf ,ab-queue-var (or ammount 1))
              (when (zerop ,ab-queue-var)
                (funcall ,next-action))
              (lambda (&rest ignore)
                (ab-queue -1)))
            ( ab-dequeue ()
              (ab-queue -1)))
         (let ((actions ,actions-value)
               (cur-buf (current-buffer)))
           (funcall (setq ,next-action
                          (lambda ()
                            (while (and actions (zerop ,ab-queue-var))
                              (with-current-buffer
                                  (if (buffer-live-p cur-buf)
                                      cur-buf
                                    (current-buffer))
                                (funcall (pop actions))
                                (setq cur-buf (current-buffer)))))))
           )))))

(put 'async-block 'common-lisp-indent-function
     '(&body))
(put 'async-block 'lisp-indent-function
     0)

(provide 'async-block)
;;; async-block.el ends here
