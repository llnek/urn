;;; Defines various methods for gathering and tracking definitions and usages of all variables
;; in the program.

(import table)

(import urn/analysis/visitor visitor)
(import urn/analysis/nodes ())
(import urn/analysis/pass (defpass))

(defun create-state ()
  "Create a new, empty usage state."
  { :vars {}
    :nodes {}})

(defun get-var (state var)
  "Find a VAR entry in the current STATE."
  (with (entry (.> state :vars var))
    (unless entry
      (set! entry
        { :var var
          :usages    '()
          :defs      '()
          :lazy-defs '()
          :active    false })
      (.<! state :vars var entry))
    entry))

(defun get-node (state node)
  "Find a NODE entry in the current STATE."
  (with (entry (.> state :nodes node))
    (unless entry
      (set! entry { :uses '() })
      (.<! state :nodes node entry))
    entry))

(defun add-usage! (state var node)
  "Mark a NODE as using a specific VAR."
  :hidden
  (with (var-meta (get-var state var))
    (push-cdr! (.> var-meta :usages) node)
    (.<! var-meta :active true)))

(defun add-definition! (state var node kind value)
  "Add a definition for a specific VAR."
  :hidden
  (with (var-meta (get-var state var))
    (push-cdr! (.> var-meta :defs) { :tag   kind
                                     :node  node
                                     :value value })))

(defun mark-usages (state nodes)
  (let* [(queue '())
         (active-queue '())
         (visited {})
         (add-val-def! (lambda (var node value)
                    (add-definition! state var node "val" value)
                    (cond
                      [(side-effect? value) true]
                      [true
                       (push-cdr! (.> (get-var state var) :lazy-defs) value)
                       ;; (push-cdr! queue { :val value :var var })
                       false])))
         (add-var-def! (lambda (var node)
                         (add-definition! state var node "var" var)))
         (visitor (lambda (active node visitor)
                    (case (type node)
                      ["symbol"
                       (with (var-meta (get-var state (.> node :var)))
                         (push-cdr! (.> var-meta :usages) node)
                         (when active
                           (unless (.> var-meta :active) (push-cdr! queue var-meta))
                           (.<! var-meta :active true)))]
                      ["string"]
                      ["number"]
                      ["key"]
                      ["list"
                       (let* [(head (car node))
                              (hty (type head))]
                         (cond
                           [(= hty "symbol")
                            (with (func (.> head :var))
                              (cond
                                [(= func (.> builtins :lambda))
                                 (for-each arg (nth node 2) 1
                                  (add-var-def! (.> arg :var) arg))]
                                [(= func (.> builtins :set!))
                                 (add-val-def! (.> node 2 :var) node (nth node 3))]
                                [(or (= func (.> builtins :define)) (= func (.> builtins :define-macro)))
                                 (add-val-def! (.> node :defVar) node (nth node (# node)))]
                                [(= func (.> builtins :define-native))
                                 (add-var-def! (.> node :defVar) node)]
                                [true]))]
                           [(and (= hty "list") (builtin? (car head) :lambda))
                            (let* [(args (nth head 2))
                                   (offset 1)]
                              (for i 1 (# args) 1
                                (let [(arg (nth args i))
                                      (val (nth node (+ i offset)))]
                                  (if (.> arg :var :isVariadic)
                                    (with (count (- (# node) (# args)))
                                      ;; If it's a variable number of args then just skip them
                                      (when (< count 0) (set! count 0))
                                      (set! offset count)
                                      ;; And define as a normal argument
                                      (add-var-def! (.> arg :var) arg)
                                      (for j 1 count 1
                                        (with (elem (nth node (+ i j)))
                                          (if (side-effect? elem)
                                            (push-cdr! (.> (get-var state (.> arg :var)) :lazy-defs) elem)
                                            ;; (push-cdr! queue { :var (.> arg :var) :val elem })
                                            (visitor/visit-node elem visitor)))))
                                    (when (and (/= (add-val-def! (.> arg :var) arg (or val (make-nil))) false) val)
                                      (visitor/visit-node val visitor))))))
                            (visitor/visit-list head 3 visitor)
                            false]
                           [true]))])))
         (visit-active (cut visitor true <> <>))]
    (for-each node nodes
      (visitor/visit-node node visit-active))

    (while (> (# queue) 0)
      (let* [(var-meta (pop-last! queue))
             (lazy (.> var-meta :lazy-defs))]
        (.<! var-meta :lazy-defs '())
        (for-each def lazy (visitor/visit-node def (cut visitor true <> <>)))))

      ))
    ;; (let* [(idx 0)
    ;;        (working true)]
    ;;   (while working
    ;;     (set! working false)
    ;;     (set! idx 1)

    ;;     (print! "Starting iteration" (# queue))

    ;;     (while (<= idx (# queue))
    ;;       (let* [(elem (nth queue idx))
    ;;              (var  (.> elem :var))
    ;;              (active (.> (get-var state var) :active))]

    ;;         ;; (when (= (.> var :name) "foo-bar")
    ;;         ;;   (print! "" (.> var :name) active))
    ;;         ;; (unless (.> var :scope :isRoot)
    ;;         ;;   (print! "" active (.> var :name) (pretty (.> elem :val))))

    ;;         (if active
    ;;           (progn
    ;;             (remove-nth! queue idx)
    ;;             ;; (print! "\tVisiting" (.> var :name) active (pretty (.> elem :val)))
    ;;             (visitor/visit-node (.> elem :val) (cut visitor active <> <>))
    ;;             (set! working true))
    ;;           (inc! idx))))))))

(defpass tag-usage (state nodes lookup)
  "Gathers usage and definition data for all expressions in NODES, storing it in LOOKUP."
  :cat '("tag" "usage")
  (mark-usages lookup nodes))
