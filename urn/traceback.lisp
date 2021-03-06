(defun unmangle-ident (ident)
  "Attempt to unmangle IDENT, converting it from the escaped form to the unescaped form."
  (with (esc (string/match ident "^(.-)%d+$"))
    (cond
      [(= esc nil) ident]
      [(= (string/sub esc 1 2) "_e")
       ;; This is an escaped keyword
       (string/sub ident 3)]

      [true
       ;; Otherwise, we'll attempt to unmangle the string
       (let* [(buffer '())
              (pos 0)
              (len (n esc))]
         (while (<= pos len)
           (with (char (string/char-at esc pos))
             (cond
               ((= char "_")
                 (case (list (string/find esc "^_[%da-z]+_" pos))
                   [(?start ?end)
                   (inc! pos)
                   (while (< pos end)
                     (push-cdr! buffer (string/char (string->number (string/sub esc pos (succ pos)) 16)))
                     (set! pos (+ pos 2)))]
                   [_
                   (push-cdr! buffer "_")]))
               ((between? char "A" "Z")
                 (push-cdr! buffer "-")
                 (push-cdr! buffer (string/lower char)))
               (true (push-cdr! buffer char))))
           (inc! pos))
         (concat buffer))])))

(defun remap-error (msg)
  "Remap the primary error message MSG, unmangling variable names"
  (with (res (-> msg
               (string/gsub <> "local '([^']+)'" (lambda (x) (.. "local '" (unmangle-ident x) "'")))
               (string/gsub <> "global '([^']+)'" (lambda (x) (.. "global '" (unmangle-ident x) "'")))
               (string/gsub <> "upvalue '([^']+)'" (lambda (x) (.. "upvalue '" (unmangle-ident x) "'")))
               (string/gsub <> "function '([^']+)'" (lambda (x) (.. "function '" (unmangle-ident x) "'")))))
    res))

(defun remap-message (mappings msg)
  "Remap MSG using the line MAPPINGS."
  (case (list (string/match msg "^(.-):(%d+)(.*)$"))
    [(?file ?line ?extra)
      (with (mapping (.> mappings file))
        (if mapping
          (with (range (.> mapping (string->number line)))
            (if range
              (.. range " (" file ":" line ")" (remap-error extra))
              msg))
          msg))]
    [_ msg]))

(defun remap-traceback (mappings msg)
  "Remap the traceback MSG using the line MAPPINGS. Also attempt to unmangle variable names."
  (-> msg
    (string/gsub <> "^([^\n:]-:%d+:[^\n]*)" (cut remap-message mappings <>))
    (string/gsub <> "\t([^\n:]-:%d+:)" (lambda (msg) (.. "\t" (remap-message mappings msg))))
    (string/gsub <> "<([^\n:]-:%d+)>\n" (lambda (msg) (.. "<" (remap-message mappings msg) ">\n")))
    (string/gsub <> "in local '([^']+)'\n" (lambda (x) (.. "in local '" (unmangle-ident x) "'\n")))
    (string/gsub <> "in global '([^']+)'\n" (lambda (x) (.. "in global '" (unmangle-ident x) "'\n")))
    (string/gsub <> "in upvalue '([^']+)'\n" (lambda (x) (.. "in upvalue '" (unmangle-ident x) "'\n")))
    (string/gsub <> "in function '([^']+)'\n" (lambda (x) (.. "in function '" (unmangle-ident x) "'\n")))))

(defun generate-mappings (lines)
  "Generate mappings from the LINES generated by the writer."
  (with (out-lines {})
    (for-pairs (line ranges) lines
      ;; For each line in the line mappings we attempt to find the "dominant"
      ;; range that this block exists for. We do this by listing all files and lines
      ;; this exists for and finding the most common file.
      ;; This could probably be improved by narrowing down the range too.
      (with (range-lists {})
        (for-pairs (pos) ranges
          (let* [(file (.> pos :name))
                 (range-list (.> range-lists :file))]
            (unless range-list
              (set! range-list { :n 0
                                 :min math/huge
                                 :max (- 0 math/huge) })
              (.<! range-lists file range-list))

            (for i (.> pos :start :line) (.> pos :finish :line) 1
              (unless (.> range-list i)
                (.<! range-list :n (succ (.> range-list :n)))
                (.<! range-list i true)
                (when (< i (.> range-list :min)) (.<! range-list :min i))
                (when (> i (.> range-list :max)) (.<! range-list :max i))))))

        (let* [(best-name nil)
               (best-lines nil)
               (best-count 0)]
          (for-pairs (name lines) range-lists
            (when (> (.> lines :n) best-count)
              (set! best-name name)
              (set! best-lines lines)
              (set! best-count (.> lines :n))))

          ;; We either emit a range of lines or a single line
          (.<! out-lines line (if (= (.> best-lines :min) (.> best-lines :max))
                                (string/format "%s:%d" best-name (.> best-lines :min))
                                (string/format "%s:%d-%d" best-name (.> best-lines :min) (.> best-lines :max)))))))
    out-lines))
