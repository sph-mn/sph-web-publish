(library (sph web publish itml)
  (export
    swp-itml-eval-sxml-inline)
  (import
    (guile)
    (ice-9 match)
    (ice-9 sandbox)
    (sph)
    (sph lang itml eval)
    (sph lang itml eval shtml)
    (sph list)
    (only (sph tree) tree-map-lists)
    ;(rnrs eval)
    ;(rnrs exceptions)
    ;(sph documentation)
    ;(sph documentation shtml)
    ;(sph filesystem)
    ;(sph hashtable)
    ;(sph io)
    ;(sph lang itml eval plaintext)
    ;(sph string)
    ;(sph web publish helper)
    )

  (define sph-web-publish-itml-description "support for itml expressions in guile commonmark sxml")

  (define swp-itml-state
    (itml-state-create #:bindings
      (append core-bindings string-bindings
        symbol-bindings
        (list-q
          ( (sph web publish itml env) library-short-description short-description
            ;link-c include-c
            ;links
            library-documentation
            )))
      #:exceptions #t #:recursion #t))

  (define (itml-eval-sxml-inline sxml itml-state)
    "evaluate itml expressions in sxml created by commonmark.
    only evaluate itml expressions in code blocks, and only if the first character in the block is #"
    (let (string-and-hash-prefix? (l (a) (and string? (string-prefix? "#" a))))
      (tree-map-lists
        (l (a) a
          (match a
            ( ( (quote pre) ((quote code) (? string-and-hash-prefix? content)))
              (simplify-list (itml-shtml-eval-string content itml-state)))
            ( ( (quote code) (? string-and-hash-prefix? content))
              (simplify-list (itml-shtml-eval-string content itml-state)))
            (else a)))
        sxml)))

  (define (swp-itml-eval-sxml-inline sxml) (itml-eval-sxml-inline sxml swp-itml-state)))
