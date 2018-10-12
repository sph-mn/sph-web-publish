(library (sph web publish itml env)
  (export
    library-documentation
    library-short-description
    links
    short-description
    ;include-files
    ;link-files
    )
  (import
    (guile)
    (rnrs sorting)
    (sph)
    (sph documentation)
    (sph documentation shtml)
    (sph filesystem)
    (sph hashtable)
    (sph lang itml eval shtml)
    (sph list)
    (sph string)
    (sph web publish helper)
    (sph web publish shtml))

  (define-syntax-rule (short-description s a ...) (list-q a ...))
  ;(define-syntax-rule (link-file s query option ...) (itml-link-files s (q query) option ...))
  ;(define-syntax-rule (include-file s query option ...) (itml-include-files s (q query) option ...))

  (define-syntax-rule (links s (option ...) (url/title/description ...) ...)
    (itml-links s (list (quote-triple-second url/title/description ...) ...) option ...))

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
    (let* ((depth 0) (content (pair module-name (doc-shtml-library depth module-name))))
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
        content)))

  (define-as itml-links-name-handlers ht-create-symbol-q
    b (l (a) "basename" (string-replace-string (string-downcase (basename a)) "_" " "))
    n
    (l (a) "basename without suffix"
      (string-replace-string (remove-filename-extension (basename a)) "_" " "))
    h url-hostname u url-drop-www-and-protocol)

  (define* (itml-links itml url/name/description #:key collapsed sorted)
    "hashtable ([string:url symbol/string:name string:description]) _ ... -> sxml
     create hyperlinks with special features for creating the link title.
         auto-create name from target if name is a symbol or result in \"a\" otherwise.
     default supported symbols are:
     b - basename, the basename of the pash of the target
     h - hostname, the second-level domain of the hostname or the full hostname if there is no second-level domain
     u - url, the url with the protocol and www. removed
     n - the basename/file-name of url path without the last filename-extension"
    (let*
      ( (link-data
          (map-apply
            (l* (url #:optional name description)
              (let (name (if (symbol? name) ((ht-ref itml-links-name-handlers name) url) name))
                ; name as first element for sorting
                (list name url description)))
            url/name/description))
        (link-data (if sorted (list-sort-with-accessor string<? first link-data) link-data)))
      (shtml-links link-data collapsed))))
