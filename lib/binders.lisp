(import base (defmacro if ! when car and or
              cdr and pretty print debug /=
              % get-idx defun = n >= error
              progn gensym for list +))
(import base)
(import type (list? empty?))
(import list (cars cadrs caar cadar map cadr
              cdar cddr caddar snoc push-cdr!
              nth))

(import lua/basic (getmetatable ..))

(defun make-vars (x) :hidden
  (if (list? x) x (list x)))

(defun make-binding (xs) :hidden
  (if (= (n xs) 1)
    (car xs)
    (if (>= (n xs) 2)
      `(lambda ,(car xs) ,@(cdr xs))
      (error "Expected binding, got nil."))))

(defun make-let-binding (xs) :hidden
  (if (= (n xs) 2)
    (cadr xs)
    (if (>= (n xs) 3)
      `(lambda ,(cadr xs) ,@(cddr xs))
      (error "Expected binding, got nil."))))

(defmacro let* (vars &body)
  "Bind several variables (given in VARS), then evaluate BODY.
   Variables bound with [[let*]] can refer to variables bound
   previously, as they are evaluated in order.

   ### Example
   ```cl
   (let* [(foo 1)
          (bar (+ foo 1))]
     foo
   ```"
  (base/with (len (n vars))
    (cond
      [(= len 0) `((lambda () ,@body))]
      [(= len 1) `((lambda ,(make-vars (caar vars)) ,@body) ,(make-binding (cdar vars)))]
      [true
       `((lambda ,(make-vars (caar vars))
           (let* ,(cdr vars) ,@body))
          ,(make-binding (cdar vars)))])))

(defmacro with (var &body)
  "Bind the single variable VAR, then evaluate BODY."
  `(let* [,var] ,@body))

(defmacro let (vars &body)
  "Bind several variables (given in VARS), then evaluate BODY.
   In contrast to [[let*]], variables bound with [[let]] can not refer
   to each other.

   ### Example
   ```cl
   (let [(foo 1)
         (bar 2)]
     (+ foo bar))
   ```"
  `((lambda ,(cars vars)
      ,@body)
    ,@(map make-let-binding vars)))

(defmacro when-let (vars &body)
  "Bind VARS, as with [[let]], and check they are all truthy before
   evaluating BODY.

   ```cl
   (when-let [(foo 1)
              (bar nil)]
     foo)
   ```
   Does not evaluate `foo`, while
   ```
   (when-let [(foo 1)
              (bar 2)]
     (+ foo bar))
   ```
   does."
  `((lambda ,(cars vars)
     (when (and ,@(cars vars)) ,@body))
    ,@(map make-let-binding vars)))

(defmacro when-let* (vars &body)
  "Bind each pair of `(name value)` of VARS, checking if the value is
   truthy before binding the next, and finally evaluating BODY. As with
   [[let*]], bindings inside [[when-let*]] can refer to previously bound
   names.

   ### Example
   ```cl
   (when-let* [(foo 1)
               (bar nil)
               (baz 2)
     (+ foo baz))
   ```

   Since `1` is truthy, it is evaluated and bound to `foo`, however,
   since `nil` is falsey, evaluation does not continue."
  (cond
    [(empty? vars) `((lambda () ,@body))]
    [true `((lambda (,(caar vars))
              (cond
                [,(caar vars) (when-let* ,(cdr vars) ,@body)]
                [true nil]))
            ,(make-binding (cdar vars)))]))

(defmacro when-with (var &body)
  "Bind the PAIR var of the form `(name value)`, only evaluating BODY if
   the value is truthy

   ### Example
   ```cl
   (when-with (foo (get-idx bar :baz))
      (print! foo))
   ```

   When `bar` has an index `baz`, it will be bound to `foo` and
   printed. If not, the print statement will not be executed."
  `((lambda (,(car var)) (when ,(car var) ,@body)) ,(cadr var)))

(defun make-setting (var) :hidden
  (if (= (n var) 2)
    `(set! ,(car var) ,(cadr var))
    (if (>= (n var) 3)
      `(set! ,(car var) (lambda ,(cadr var) ,@(cddr var)))
      (error "Expected binding, got nil."))))

;; Pre-declare variable and define it, allowing recursive functions to exist
(defmacro letrec (vars &body)
  "Bind several variables (given in VARS), which may be recursive.

   ### Example
   ```
   > (letrec [(is-even? (lambda (n)
                          (or (= 0 n)
                              (is-odd? (pred n)))))
              (is-odd? (lambda (n)
                         (and (! (= 0 n))
                              (is-even? (pred n)))))]
       (is-odd? 11))
   true
   ```"
  `((lambda ,(cars vars)
      ,@(map make-setting vars)
      ,@body)))

(defun finaliser-for (x) :hidden
  `((or (and (getmetatable ,x)
            (get-idx (getmetatable ,x) :--finalise))
       (get-idx ,x :close)
       (lambda ()))))

(defmacro use (var &body)
  "Bind each variable in VAR, checking for truthyness between bindings,
   execute BODY, then run a finaliser for all the variables bound by
   VAR.

   Potential finalisers might be:
   - `(get-idx (getmetatable FOO) :--finalise)`, where FOO is the
     variable.
   - `(get-idx FOO :close)` where FOO is the variable.

   If there is no finaliser for VAR, then nothing is done for it.

   ### Example:
   ```
   > (use [(file (io/open \"temp\"))] \\
   .   (print! (self file :read \"*a\")))
   *contents of temp*
   ```"
  `(when-let* ,var
     ,@body
     ,@(map finaliser-for
            (cars var))))

(defmacro loop (vs test &body)
  "A general iteration helper.

   ```cl
   (loop [(var0 val0)
          (var1 val1)
          ...]
     [test test-body ...]
     body ...)
   ```

   Bind all the variables given in VS. Each iteration begins by
   evaluating TEST. If it evaluates to a truthy value, TEST-BODY
   is evaluated and the final expression in TEST-BODY is returned.
   In the case that TEST is falsey, the set of expressions BODY is
   evaluated. BODY may contain the \"magic\" form
   `(recur val0 val1 ...)`, which rebinds the respective variables
   in VS and reiterates.


   ### Examples:

   ```cl
   > (loop [(o '())
            (l '(1 2 3))]
   .   [(empty? l) o]
   .   (recur (cons (car l) o) (cdr l)))
   out = (3 2 1)
   ```"
  (when (! vs)
    (error "expected variables, got nil"))
  (when (empty? test)
    (set! test '(false)))
  (let* [(recur-args (map car vs))
         (recur `(lambda ,recur-args
                   (if ,(car test)
                     (progn ,@(cdr test))
                     (progn ,@body))))]
    `(letrec [(,'recur ,recur)]
       (,'recur ,@(map cadr vs)))))
