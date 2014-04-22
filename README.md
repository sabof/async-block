# Async block

A macro that allows transparent integration of synchronous, and asynchronous code.

## Example

```lisp
(defun async-block-test2 (seed)
  (async-block-continue seed
    (ab-wait 0.5)
    (message "c Start")
    (ab-wait 0.5)
    (let ((var 5))
      (ab-while 1 (not (zerop var))
        (message "while: %s" var)
        (cl-decf var)))
    (ab-wait 0.5)
    (set-buffer (get-buffer "async-block.el"))
    (message "c End")))

(defun async-block-test ()
  (async-block
    (ab-wait 0.5)
    (message "Start %s" (current-buffer))
    (async-block-test2 (ab-seed))
    (ab-wait 0.5)
    (message "End %s" (current-buffer))
    ))

(async-block
  (ab-wait 0.5)

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

  ;; This format is probably better when you want to have a lambda callback.

  (ab-enqueue
    (url-retrieve "http://google.com"
                   (lambda (&rest ignore)
                     (re-search-forward "<title>\\([^<]+\\)")
                     (message (match-string 1))
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
```
