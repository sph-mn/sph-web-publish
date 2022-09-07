(define-module (sph web publish markdown))

(use-modules (commonmark) (guile)
  (ice-9 match) (ice-9 sandbox)
  (rnrs io ports) (sph)
  (sph lang scheme) (sph list)
  (sph string) (sph tree)
  (sph vector) (sph web publish helper)
  (sph web publish shtml) ((srfi srfi-1) #:select (find-tail take)))

(export swp-md->shtml swp-md-get-title swp-md-scm-env-new)

(define sph-web-publish-markdown-description
  "support for inline scheme expressions in guile commonmark sxml")

(define (swp-md-scm-env-new)
  (make-sandbox-module
    (append core-bindings string-bindings
      symbol-bindings list-bindings
      number-bindings (q (((sph web publish markdown scm-env) link-files include-files))))))

(define (md-shtml-adjust-heading-structure a)
  "convert an shtml structure (heading other ... heading other ...) to
   ((section h1 (div other ... (section h2 (div other ...)))) (section h1 other ...))"
  (let*
    ( (combine-what-follows-headings
        (l (a) (map-span (negate shtml-heading?) (l a (pair (q div) a)) a)))
      (to-denoted-tree
        (l (a) "-> ((integer:nesting . string:line) ...)"
          (let loop ((a a))
            (if (null? a) a
              (match a
                ( ( (? shtml-heading? h) ((quote div) div ...) rest ...)
                  (let (depth (- (shtml-heading-tag->number (first h)) 1))
                    (append (pair (pair depth h) (map (l (a) (pair (+ 1 depth) a)) div))
                      (if (null? rest) rest (loop rest)))))
                ( ( (? shtml-heading? h) rest ...)
                  (append (list (pair (- (shtml-heading-tag->number (first h)) 1) h))
                    (if (null? rest) rest (loop rest))))
                (else (pair (pair 0 (first a)) (loop (tail a)))))))))
      (wrap-section-around-headings
        (l (a) (map (l (a) (if (shtml-heading? a) (list (q section) a) a)) a)))
      (to-prefix-tree-with-tags
        (l (a)
          (tree-map-lists
            (l (a)
              (match a
                ( ( (? shtml-heading? h) rest ...)
                  (list (q section) h (pair (q div) (wrap-section-around-headings rest))))
                (else a)))
            (denoted-tree->prefix-tree a)))))
    (wrap-section-around-headings
      (to-prefix-tree-with-tags (to-denoted-tree (combine-what-follows-headings a))))))

(define (md-shtml-external-links a)
  "shtml -> shtml
   make links open in a new tab and set class \"external\""
  (tree-map-lists
    (l (a)
      (match a
        (((quote a) ((quote @) ((quote href) href) _ ...) content ...) (shtml-link href content))
        (else a)))
    a))

(define (md-shtml-scm-eval env a directory) "-> (((string:file-path-dependency ...) . shtml) ...)"
  (let*
    ( (scm-prefix?
        (l (a) (and (string? a) (or (string-prefix? "%scm " a) (string-prefix? "`%scm " a)))))
      (escaped-scm-prefix?
        (l (a) (and (string? a) (or (string-prefix? "%%scm " a) (string-prefix? "`%%scm " a)))))
      (scm-eval
        (l (a)
          "multiple four space indented md code blocks come as one block, even with an empty line inbetween.
           this doesnt match indented code blocks with escaped prefix following indented blocks with prefix"
          (let
            (expressions
              (map string->datums
                (string-split-regexp (string-drop (string-trim-both a #\`) 5) "\n%scm ")))
            (map-apply
              (l (identifier . arguments)
                (let
                  (result
                    (eval-in-sandbox (pairs identifier directory arguments) #:time-limit
                      2 #:allocation-limit 100000000 #:module env #:sever-module? #f))
                  "pass file dependencies as first element of a vector"
                  (if (vector? result)
                    (pair (any->list (vector-first result)) (vector-second result))
                    (pair null result))))
              expressions)))))
    ; tree-transform* should be replaced with a simple recursive loop
    (tree-transform* a
      (l (a recurse dependencies)
        "match %scm blocks and also remove the extra <code> tag inside <pre>" (debug-log a)
        (match a
          ( ( (quote pre) ((quote code) (quote (@)) (? scm-prefix? b)))
            (let (result (scm-eval b))
              (list (map tail result) #f (append (map first result) dependencies))))
          ( ( (quote pre) ((quote code) (? scm-prefix? b)))
            (let (result (scm-eval b))
              (list (map tail result) #f (append (apply append (map first result)) dependencies))))
          ( ( (quote code) (? scm-prefix? b))
            (let (result (scm-eval b))
              (list (map tail result) #f (append (apply append (map first result)) dependencies))))
          ( ( (quote pre) ((quote code) (quote (@)) (? escaped-scm-prefix? content)))
            (list (string-drop content 1) #f dependencies))
          ( ( (quote pre) ((quote code) (? escaped-scm-prefix? content)))
            (list (string-drop content 1) #f dependencies))
          (((quote pre) ((quote code) body ...)) (list (pair (q pre) body) #f dependencies))
          (else (list #f #t dependencies))))
      (l a a) (l a a) null)))

(define nonword-regexp (make-regexp "^\\W"))

(define (swp-md-get-title file)
  "sxml -> (false/string:title . string/false:description)
   parse title and description from a markdown file.
   title must be a top level heading on the first line an description
   must be the line following the title. the description must start with a word character"
  (call-with-input-file file
    (l (port)
      (let (a (get-line port))
        (if (or (eof-object? a) (not (string-prefix? "# " a))) (pair #f #f)
          (let (b (get-line port))
            (pair (string-drop a 2) (if (or (eof-object? b) (regexp-exec nonword-regexp b)) #f b))))))))

(define (swp-md->shtml env path directory)
  (apply (l (a dependencies) a)
    (md-shtml-scm-eval env
      (md-shtml-external-links
        (md-shtml-adjust-heading-structure (call-with-input-file path commonmark->sxml)))
      directory)))
