(library (sph web publish itml)
  (export
    swp-itml-state-create
    swp-markdown->shtml
    swp-markdown-shtml-get-description)
  (import
    (commonmark)
    (guile)
    (ice-9 match)
    (ice-9 sandbox)
    (sph)
    (sph lang itml eval)
    (sph lang itml eval shtml)
    (sph list)
    (sph tree)
    (only (srfi srfi-1) take))

  (define sph-web-publish-itml-description "support for itml expressions in guile commonmark sxml")

  (define-as swp-itml-module (make-sandbox-module append)
    core-bindings string-bindings
    symbol-bindings
    (list-q
      ( (sph web publish itml env) library-short-description short-description
        links library-documentation link-files include-files)))

  (define (swp-markdown-shtml-get-description a)
    ;(debug-log a)
    null)

  (define (shtml-is-heading? a)
    (and (list? a) (not (null? a)) (containsq? (list-q h1 h2 h3 h4 h5 h6) (first a))))

  (define (shtml-heading-tag->number a) (string->number (string-drop (symbol->string a) 1)))

  (define (swp-shtml-adjust-heading-structure a)
    "convert a html structure (heading other ... heading other ...) to
     ((section h1 (div other ... (section h2 (div other ...)))) (section h1 other ...))"
    (let*
      ( (combine-what-follows-headings
          (l (a) (map-span (negate shtml-is-heading?) (l a (pair (q div) a)) a)))
        (to-denoted-tree
          (l (a)
            (let loop ((a a))
              (if (null? a) a
                (match a
                  ( ( (? shtml-is-heading? h) ((quote div) div ...) rest ...)
                    (let (depth (- (shtml-heading-tag->number (first h)) 1))
                      (append (pair (pair depth h) (map (l (a) (pair (+ 1 depth) a)) div))
                        (if (null? rest) rest (loop rest)))))
                  ( ( (? shtml-is-heading? h) rest ...)
                    (append (list (pair (- (shtml-heading-tag->number (first h)) 1) h))
                      (if (null? rest) rest (loop rest))))
                  (else (pair (first a) (loop (tail a)))))))))
        (wrap-section-around-headings
          (l (a) (map (l (a) (if (shtml-is-heading? a) (list (q section) a) a)) a)))
        (to-prefix-tree-with-tags
          (l (a)
            (tree-map-lists
              (l (a)
                (match a
                  ( ( (? shtml-is-heading? h) rest ...)
                    (list (q section) h (pair (q div) (wrap-section-around-headings rest))))
                  (else a)))
              (denoted-tree->prefix-tree a)))))
      (wrap-section-around-headings
        (to-prefix-tree-with-tags (to-denoted-tree (combine-what-follows-headings a))))))

  (define (swp-markdown->shtml path itml-state)
    (itml-eval-sxml-inline
      (swp-shtml-adjust-heading-structure (call-with-input-file path commonmark->sxml))
      (list (itml-state-stack itml-state) (itml-state-depth itml-state)
        swp-itml-module (vector-copy (itml-state-data itml-state)))))

  (define (swp-itml-state-create directory)
    (itml-state-create #:module swp-itml-module
      #:exceptions #t #:recursion #t #:user-data directory))

  (define (itml-eval-sxml-inline sxml itml-state)
    "evaluate itml expressions in sxml created by commonmark.
     only evaluate itml expressions in code blocks, and only if the first character in the block is #"
    (let (string-and-hash-prefix? (l (a) (and string? (string-prefix? "#" a))))
      (tree-transform sxml
        (l (a recurse)
          (match a
            ( ( (quote pre) ((quote code) (? string-and-hash-prefix? content)))
              (list (simplify-list (itml-shtml-eval-string content itml-state)) #f))
            ( ( (quote code) (? string-and-hash-prefix? content))
              (list (simplify-list (itml-shtml-eval-string content itml-state)) #f))
            (else (list #f #t))))
        identity identity))))
