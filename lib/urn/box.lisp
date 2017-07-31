(defmacro box (x)
  "Return a suspended form of the computation X.

   Boxes have the following properties:
   - A Boxes for X will evaluate X at most once, and cache
     the result for consecutive applications of [[force]].
   - Pretty-printing a box does not cause an application
     of [[force]]."
  (let* [(pp-param (gensym))]
    `(setmetatable
       { :tag "box"
         :val (lambda () ,x)
         :eval false }
       { :--pretty-print
         (lambda (,pp-param)
           (if (.> ,pp-param :eval)
             (pretty (.> ,pp-param :val))
             "«box»")) })))

(defun force (box)
  "Force BOX, causing evaluation only if it hasn't happened before.

   ### Example
   ```cl
   > (define my-box (box (print! 123)))
   out = «box»
   > (force my-thnk)
   123
   out = nil
   > (force my-thnk)
   out = nil
   ```"
  (if (= (type box) "box")
    (if (.> box :eval)
      (.> box :val)
      (progn
        (.<! box :val ((.> box :val)))
        (.<! box :eval true)
        (.> box :val)))
    box))
