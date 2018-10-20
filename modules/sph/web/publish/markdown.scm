(library (sph web publish markdown)
  (export
    swp-md->shtml
    swp-md-get-title
    swp-md-scm-env-new)
  (import
    (commonmark)
    (guile)
    (ice-9 match)
    (ice-9 sandbox)
    (rnrs io ports)
    (sph)
    (sph lang scheme)
    (sph list)
    (sph string)
    (sph tree)
    (sph web publish helper)
    (sph web publish shtml)
    (only (srfi srfi-1) find-tail take))

  (define sph-web-publish-markdown-description
    "support for itml expressions in guile commonmark sxml")

  (define (swp-md-scm-env-new)
    (make-sandbox-module
      (append core-bindings string-bindings
        symbol-bindings list-bindings
        number-bindings
        (list-q
          ( (sph web publish markdown scm-env) library-short-description library-documentation
            link-files include-files)))))

  (define (md-shtml-adjust-heading-structure a)
    "convert a html structure (heading other ... heading other ...) to
     ((section h1 (div other ... (section h2 (div other ...)))) (section h1 other ...))"
    (let*
      ( (combine-what-follows-headings
          (l (a) (map-span (negate shtml-heading?) (l a (pair (q div) a)) a)))
        (to-denoted-tree
          (l (a)
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

  (define (md-shtml-scm-eval env a directory)
    (let
      ( (scm-prefix? (l (a) (and string? (string-prefix? "%scm " a))))
        (escaped-scm-prefix? (l (a) (and string? (string-prefix? "%%scm " a))))
        (scm-eval
          (l (a)
            "multiple four space indented md code blocks come as one block, even with an empty line inbetween.
            doesnt match indented code block with escaped prefix following indented block with prefix"
            (let
              (expressions (map string->datums (string-split-regexp (string-drop a 5) "\n%scm ")))
              (map-apply
                (l* (identifier . arguments)
                  (eval-in-sandbox (pairs identifier directory arguments) #:time-limit
                    2 #:allocation-limit 100000000 #:module env #:sever-module? #f))
                expressions)))))
      (tree-transform a
        (l (a recurse)
          (match a (((quote pre) ((quote code) (? scm-prefix? b))) (list (scm-eval b) #f))
            (((quote code) (? scm-prefix? b)) (list (scm-eval b) #f))
            ( ( (quote pre) ((quote code) (? escaped-scm-prefix? content)))
              (list (string-drop content 1) #f))
            (((quote code) (? escaped-scm-prefix? content)) (list (string-drop content 1) #f))
            (else (list #f #t))))
        identity identity)))

  (define nonword-regexp (make-regexp "\\W"))

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
    (md-shtml-scm-eval env
      (md-shtml-adjust-heading-structure (call-with-input-file path commonmark->sxml)) directory)))
