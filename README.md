# Async block

A macro that allows transparent integration of synchronous, and asynchronous code.

## Example

```lisp
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

  ;; Result processing can also be done like this:

  (url-retrieve "http://google.com" (ab-queue))
  (re-search-forward "<title>\\([^<]+\\)")
  (message (match-string 1))

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
```
