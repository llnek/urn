(import urn/backend/lua/emit ())
(import urn/backend/lua/escape (push-escape-var! escape-var) :export)
(import urn/backend/writer w)
(import urn/resolve/state state)
(import urn/timer timer)
(import urn/traceback traceback)

(import extra/assert (assert!))
(import lua/basic (load))
(import lua/debug debug)

(defun create-state (meta) {
                             ;; [[run-pass]] options
                             :level      1
                             :override   {}
                             :timer      timer/void

                             ;; execute-states options
                             :count      0
                             :mappings   {}

                             ;; Various lookup tables
                             :var-cache  {}
                             :var-lookup {}
                             :meta       (or meta {}) })

(defun file (compiler shebang)
  "Generate a complete file using the current COMPILER state.

   Returns the resulting writer, from which you can extract line mappings
   and the resulting contents.

   If SHEBANG is specified then it will be prepended."
  (let* [(state (create-state (.> compiler :lib-meta)))
         (out (w/create))]
    (when shebang
      (w/line! out (.. "#!/usr/bin/env " shebang)))

    (.<! state :trace true)

    (prelude out)

    (w/line! out "local _libs = {}")

    ;; Emit all native libraries
    (for-each lib (.> compiler :libs)
      (let* [(prefix (string/quoted (.> lib :prefix)))
             (native (.> lib :native))]
        (when native
          (w/line! out "local _temp = (function()")
          (for-each line (string/split native "\n")
            (when (/= line "")
              (w/append! out "\t")
              (w/line! out line)))
          (w/line! out "end)()")
          (w/line! out (.. "for k, v in pairs(_temp) do _libs[" prefix ".. k] = v end")))))

    ;; Count the number of active variables
    (with (count 0)
      (for-each node (.> compiler :out)
        (when-with (var (.> node :def-var))
          (inc! count)))

      ;; Pre-escape all variables
      (for-each node (.> compiler :out)
        (when-with (var (.> node :def-var))
          (push-escape-var! var state true)))

      ;; Predeclare all variables. We only do this if we're pretty sure we won't hit the
      ;; "too many local variable" errors. The upper bound is actually 200, but lambda inlining
      ;; will probably bring it up slightly.
      ;; In the future we probably ought to handle this smartly when it is over 150 too.
      (if (between? count 1 150)
        (progn
          (w/append! out "local ")
          (with (first true)
            (for-each node (.> compiler :out)
              (when-with (var (.> node :def-var))
                (if first
                  (set! first false)
                  (w/append! out ", "))
                (w/append! out (escape-var var state)))))
          (w/line! out))
        (w/line! out "local _ENV = setmetatable({}, {__index=ENV or (getfenv and getfenv()) or _G}) if setfenv then setfenv(0, _ENV) end")))

    (block (.> compiler :out) out state 1 "return ")
    out))

(defun execute-states (back-state states global logger)
  "Attempt to execute a series of STATES in the GLOBAL environment.

   BACK-STATE is the backend state, as created with [[create-state]].
   All errors are logged to LOGGER."

  (let* [(state-list '())
         (name-list '())
         (export-list '())
         (escape-list '())]

    (for i (n states) 1 -1
      (with (state (nth states i))
        (unless (= (.> state :stage) "executed")
          (let* [(node (assert! (.> state :node) (.. "State is in " (.> state :stage) " instead")))
                 (var (or (.> state :var) { :name "temp" }))
                 (escaped (push-escape-var! var back-state true))
                 (name (.> var :name))]
            (push-cdr! state-list state)
            (push-cdr! export-list (.. escaped " = " escaped))
            (push-cdr! name-list name)
            (push-cdr! escape-list escaped)))))

    (unless (empty? state-list)
      (let* [(out (w/create))
             (id (.> back-state :count))
             (name (concat name-list ","))]

        ;; Update the new count
        (.<! back-state :count (succ id))

        ;; Setup the function name
        (when (> (n name) 20) (set! name (.. (string/sub name 1 17) "...")))
        (set! name (.. "compile#" id "{" name "}"))

        (prelude out)
        (w/line! out (.. "local " (concat escape-list ", ")))


        ;; Emit all expressions
        (for i 1 (n state-list) 1
          (with (state (nth state-list i))
            (expression
              (.> state :node) out back-state

              ;; If we don't have a variable then we'll assign this to the
              ;; temp variable. Otherwise it will be emitted in the main backend
              (if (.> state :var)
                ""
                (..(nth escape-list i) "= ")))
            (w/line! out)))

        (w/line! out (.. "return { " (concat export-list ", ") "}"))

        (with (str (w/->string out))
          ;; Store the traceback
          (.<! back-state :mappings name (traceback/generate-mappings (.> out :lines)))

          (case (list (load str (.. "=" name) "t" global))
            [(nil ?msg)
             (let* [(buffer '())
                    (lines (string/split str "\n"))
                    (format (.. "%" (n (number->string (n lines))) "d | %s"))]
               (for i 1 (n lines) 1
                 (push-cdr! buffer (string/format format i (nth lines i))))
               (fail! (.. msg ":\n" (concat buffer "\n"))))]
            [(?fun)
             (case (list (xpcall fun debug/traceback))
               [(false ?msg)
                (fail! (traceback/remap-traceback (.> back-state :mappings) msg))]
               [(true ?tbl)
                (for i 1 (n state-list) 1
                  (let* [(state (nth state-list i))
                         (escaped (nth escape-list i))
                         (res (.> tbl escaped))]
                    (state/executed! state res)
                    (when (.> state :var)
                      (.<! global escaped res))))])]))))))

(defun native (meta global)
  "Convert a native META definition into a function."
  (with (out (w/create))
    (prelude out)
    (w/append! out "return ")
    (compile-native out meta)

    (case (list (load (w/->string out) (.. "=" (.> meta :name)) "t" global))
      [(nil ?msg) (fail! (.. "Cannot compile meta " (.> meta :name) ":\n" msg))]
      [(?fun) (fun)])))
