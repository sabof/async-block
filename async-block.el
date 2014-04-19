;; -*- lexical-binding: t -*-

(require 'cl-lib)

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

You can find examples of `ab-queue' usage, as well as examples
for `ab-wait', `ab-while', `ab-with-queue', `ab-dequeue', in the
same file as the definition of this macro."
  (let* (( next-action (cl-gensym))
         ( actions (cl-gensym))
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
         (cl-macrolet
             (( \ab-with-queue (&rest body)
                `(progn (ab-queue)
                        ,@body))
              ( ab-wait (interval &rest body)
                (if (not body)
                    `(run-with-timer ,interval nil (ab-queue))
                  (let (( interval-sym (cl-gensym))
                        ( predicate-sym (cl-gensym)))
                    `(let ((,interval-sym ,interval))
                       (cl-labels (( ,predicate-sym ()
                                     ,@body)
                                   ( func ()
                                     (if (,predicate-sym)
                                         (ab-queue -1)
                                       (run-with-timer ,interval-sym
                                                       nil #'func))))
                         (ab-queue)
                         (func))))))
              ( \ab-while (interval test &rest body)
                `(ab-wait ,interval
                   (if ,test
                       (progn
                         ,@body
                         nil)
                     t)
                   )))
           (let ((,actions ,actions-value))
             (funcall (setq ,next-action
                            (lambda ()
                              (while (and ,actions (zerop ,ab-queue-var))
                                (funcall (pop ,actions)))))))))
       )))

(put 'async-block 'common-lisp-indent-function
     '(&body))
(put 'ab-while 'common-lisp-indent-function
     '(4 4 &body))
(put 'ab-wait 'common-lisp-indent-function
     '(4 &body))
(put 'ab-with-queue 'common-lisp-indent-function
     '(&body))

(put 'async-block 'lisp-indent-function
     0)
(put 'ab-while 'lisp-indent-function
     2)
(put 'ab-wait 'lisp-indent-function
     1)
(put 'ab-with-queue 'lisp-indent-function
     0)

'(progn

  (async-block
    (message "Start")

    ;; Wait 0.5 second before going forward. This format can be used when an
    ;; asynchronous funciton has a callback, and it's arguments are not important.

    (run-with-timer 0.5 nil (ab-queue))

    ;; The above can also be written like this:

    (ab-wait 0.5)

    ;; ab-wait may have a body after the first argument. In that case the queue
    ;; will proceed only when it evaluates to a non-nil value. The test will be
    ;; repeated every INTERVAL (the first argument)

    (let* (done)
      (url-retrieve "http://yahoo.com"
                    (lambda (&rest ignore)
                      (re-search-forward "<title>\\([^<]+\\)")
                      (message (match-string 1))
                      (setq done t)))
      (ab-wait 1
        done))

    (ab-wait 1)

    ;; This format is possible when you want to have a lambda callback.

    (ab-with-queue
      (run-with-timer 1 nil (lambda ()
                              (message "with-queue/dequeue")
                              (ab-dequeue))))

    (ab-wait 1)

    ;; ab-while works like regular while, but it's first argument is an iterval
    ;; which specifies the time between iterations.

    (let ((var 5))
      (ab-while 1 (not (zerop var))
        (message "while: %s" var)
        (cl-decf var)))

    (ab-wait 1)

    (message "Finish"))

  )

(provide 'async-block)
;;; async-block.el ends here
