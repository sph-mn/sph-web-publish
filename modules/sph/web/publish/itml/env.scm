(library (sph web publish itml env)
  (export
    library-documentation
    library-short-description
    short-description
    ;include-files
    ;link-files
    ;links
    )
  (import
    (guile)
    (sph)
    (sph documentation)
    (sph documentation shtml)
    (sph string)
    (sph web shtml))

  (define-syntax-rule (short-description s a ...) (list-q a ...))
  ;(define-syntax-rule (link-file s query option ...) (ol-itml-link-c s (q query) option ...))
  ;(define-syntax-rule (include-file s query option ...) (ol-itml-include-c s (q query) option ...))

  #;(define-syntax-rule (links s (option ...) (url/title/description ...) ...)
    (ol-itml-links s (list (quote-triple-second url/title/description ...) ...) option ...))

  (define-syntax-rule (library-documentation s name option ...)
    (itml-library-documentation s (q name) option ...))

  (define-syntax-rule (library-short-description s name option ...)
    (itml-library-short-description s (q name) option ...))

  (define (itml-library-short-description s module-name)
    (and-let* ((a (module-description module-name))) (first (string-split a #\newline))))

  (define* (itml-library-documentation itml module-name #:key (skip-description-line #t))
    "render documentation for libraries.
     if #:skip-description-line is true, the first line of the module description read from the
     module description variable is left out. this is for combination with shtml-library-short-description
     which is recognised by link creators"
    ; currently shtml only
    (let*
      ( (depth 0)
        (content (pair module-name (doc-shtml-library depth module-name))))
      (apply
        (l (name description index bindings)
          (list
            (and description (not (and skip-description-line (>= 1 (length description))))
              (shtml-section depth "library description"
                (if skip-description-line (tail description) description)
                (q (class "library-description"))))
            (shtml-section depth "import name"
              (list (q p) (any->string name)) (q (class "library-name")))
            (shtml-section depth "exports" (list index bindings) (q (class "library-exports")))))
        content))))
