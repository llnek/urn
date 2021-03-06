"A checker for algebraic properties."

(import lua/math (random randomseed huge))
(import lua/string string)
(import lua/table (concat))
(import lua/os os)
(randomseed (^ (os/time) (os/clock)))

(defun random-string () :hidden
  (let* [(n (random 1 255))
         (random-byte () (string/char (random 32 126)))]
    (concat (map random-byte (range :from 1 :to n)))))

(defun random-number () :hidden
  (random (- 0 (^ 2 32)) (^ 2 32)))

(defun random-boolean () :hidden
  (= 0 (% (random-number) 2)))

(defun random-symbol () :hidden
  { :tag "symbol" :contents (random-string) })

(defun random-type () :hidden
  (let* [(types '("symbol" "boolean" "number" "string" "key" "list" "struct"))]
    (nth types (random 1 (n types)))))

(defun -random-type () :hidden
  (let* [(types '("symbol" "boolean" "number" "string" "key"))]
    (nth types (random 1 (n types)))))

(defun random-of (type) :hidden
  (case type
    ["string" (random-string)]
    ["boolean" (random-boolean)]
    ["number" (random-number)]
    ["symbol" (random-symbol)]
    ["key" (random-string)]
    ["list" (random-list)]
    ["struct" (random-struct)]
    ["any" (random-of (random-type))]))

(defun random-list () :hidden
  (let* [(n (random 1 5))]
    (map (lambda (x) (random-of (-random-type))) (range :from 1 :to n))))

(defun random-struct () :hidden
  (let* [(out {})]
    (for i 1 (random 1 5) 1
      (.<! out (random-of (-random-type)) (random-of (-random-type))))
    out))

(defmacro check (bindings &props)
  "Check a set of properties against a set of random variables 100 times.
   This can be used as a rudimentary algebraic property checker, where
   BINDINGS is the list of universally-quantified variables and PROPS is
   the list of properties you're checking.

   ### Example:
   ```
   > (check [(number a)] \\
   .   (= a a))
   (= a a) passed 100 tests.
   nil
   > (check [(number a)] \\
   .   (= a (+ 1 a)))
   (= a (+ 1 a)) falsified after 1 iteration(s)
   falsifying set of values:
     the number, a, had the value 3867638440
   nil
   ```

   The property is checked against a different set of random values every
   iteration. This library has the ability to generate random numbers,
   strings, symbols, booleans, keys and lists."
  (let* [(generate-binding (binding)
           (destructuring-bind [(?type ?name) binding]
             `(,name (random-of ,(symbol->string type)))))
         (generate-regenerator (binding)
           (destructuring-bind [(?type ?name) binding]
             `(set! ,name (random-of ,(symbol->string type)))))
         (make-printing (binding)
           (destructuring-bind [(?type ?name) binding]
             `(.. ,(.. "  the " (symbol->string type) " `"
                       (symbol->string name) "', had the value ") (pretty ,name))))
         (n (if (number? (car props))
              (car props)
              100))
         (props (if (number? (car props))
                  (cdr props)
                  props))
         (silent (if (eq? (car props) ':verbose)
                   false
                   true))
         (props (if (eq? (car props) ':verbose)
                  (cdr props)
                  props))
         (generate-body (prop)
           (let* [(ctr (gensym))
                  (ok (gensym))]
             `(let* [(,ok true)
                     (,ctr 1)]
                (while (and (>= ,n ,ctr) ,ok)
                  ,@(map generate-regenerator bindings)
                  (if (! ,prop)
                    (progn
                      (set! ,ok false)
                      (error! (.. "Proposition " ,(pretty prop) " falsified after "
                          ,ctr " iteration(s).\n Falsifying set of values:\n"
                          ,@(map make-printing bindings))))
                    (inc! ,ctr)))
                ,(if (! silent)
                   `(progn (print! (.. "Ok. Proposition `" ,(pretty prop) "` passed " ,n " tests."))
                           true)
                   `true)
                )))]
    `(let* ,(map generate-binding bindings)
       ,@(map generate-body props))))

(defmacro tripping (f g x)
  "Express that the composition of the functions F and G (in order!)
   are equivalent to the identity, on the argument X.

   As an example, consider a pair of encoding and decoding functions:
   Decoding an encoded datum should be equivalent to the originally
   encoded datum. Algebraically, we express this as
   `(forall a (=:= (compose f g) id a))`.

   Example:
   ```cl
   (check [(number a)]
     (tripping tonumber tostring))
   ```"
  (let* [(y (gensym))]
    `(let* [(,y ,x)]
       (eq? (,f (,g ,y)) ,y))))

(defmacro =:= (f g x)
  "Express that the functions F and G are equivalent at the point X.

   Example:
   ```cl
   (check [(number a)]
     (=:= id (compose id id) a))
   ```"
  (let* [(y (gensym))]
    `(let* [(,y ,x)]
       (eq? (,f ,y) (,g ,y)))))

(defmacro forall (var prop)
  "Check that PROP holds across all possible points. This is a
   restricted version of [[check]] that does not allow specifying
   several variables.

   VAR may be either a single, in which case it is interpreted as
   being a variable name, or a list with a type and the variable
   name.
   Example:
   ```cl
   (forall a (eq? a (id a)))
   ```"
  (let* [(v (if (list? var)
              var
              `(,'any ,var)))]
    `(check [,v]
       ,prop)))
