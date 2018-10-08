(library (sph-cms other lang itml env)
  (export
    align
    emphasis
    emphasis-strong
    include-c
    library-documentation
    library-short-description
    link
    link-c
    link-to
    links
    short-description)
  (import
    (sph)
    (sph-cms other lang itml))

  (define sph-cms-lib-lang-itml-env-shtml-documentation
    "this module defines bindings available when evaluating itml documents.
     every binding in here must also be defined and exported in itml-env-plaintext for plaintext output to work.
     c: content, i: id, p: parameterised")

  (define-syntax-rule (short-description s a ...) (list-q a ...))
  (define-syntax-rule (link-c s query option ...) (ol-itml-link-c s (q query) option ...))
  (define-syntax-rule (include-c s query option ...) (ol-itml-include-c s (q query) option ...))
  (define-syntax-rule (link-to s key a ...) (oh-itml-link-to s key a ...))

  (define-syntax-rule (links s (option ...) (url/title/description ...) ...)
    (ol-itml-links s (list (quote-triple-second url/title/description ...) ...) option ...))

  (define-syntax-rule (link s target title) (ol-itml-link s target title))

  (define-syntax-rule (emphasis s a ...)
    (list ((ol-itml-state-views-ref-q s emphasis) (list-qq a ...))))

  (define-syntax-rule (align s where a ...)
    ; where: "left"/"right"/"center"
    ((ol-itml-state-views-ref-q s align) (q where) (list-qq a ...)))

  (define-syntax-rule (emphasis-strong s a ...)
    ((ol-itml-state-views-ref-q s strong-emphasis) (list-qq a ...)))

  (define-syntax-rule (library-documentation s name option ...)
    (ol-itml-library-documentation s (q name) option ...))

  (define-syntax-rule (library-short-description s name option ...)
    (ol-itml-library-short-description s (q name) option ...)))
