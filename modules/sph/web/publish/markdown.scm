(library (sph web publish markdown)
  (export
    swp-markdown->shtml
    swp-markdown-get-description)
  (import
    (commonmark)
    (guile)
    (ice-9 match)
    (ice-9 sandbox)
    (sph)
    (sph lang itml eval)
    (sph lang itml eval shtml)
    (sph list)
    (sph string)
    (sph tree)
    (only (srfi srfi-1) take))

  (define sph-web-publish-markdown-description
    "support for itml expressions in guile commonmark sxml")

  (define (shtml-is-heading? a)
    (and (list? a) (not (null? a)) (containsq? (list-q h1 h2 h3 h4 h5 h6) (first a))))

  (define (shtml-heading-tag->number a) (string->number (string-drop (symbol->string a) 1)))

  (define* (string->datums a #:optional (reader read))
    "string -> list
     get all scheme expression from a string"
    (let (a (open-input-string a))
      (let loop () (let (b (reader a)) (if (eof-object? b) (list) (pair b (loop)))))))

  (define-as md-scm-env (make-sandbox-module append)
    core-bindings string-bindings
    symbol-bindings list-bindings
    number-bindings
    (list-q
      ( (sph web publish markdown scm-env) library-short-description short-description
        links library-documentation link-files include-files)))

  (define (md-shtml-adjust-heading-structure a)
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

  (define (md-shtml-scm-eval a directory)
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
                    2 #:allocation-limit 100000000 #:module swp-itml-module #:sever-module? #f))
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

  (define (swp-markdown-shtml-get-description a)
    "get title and description from a markdown content file" null)

  (define (swp-markdown->shtml path directory)
    (md-sxml-scm-eval
      (swp-shtml-adjust-heading-structure (call-with-input-file path commonmark->sxml)) directory))

  #;(define (swp-itml-state-create directory)
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
