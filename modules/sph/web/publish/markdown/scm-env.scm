(library (sph web publish markdown scm-env)
  (export
    include-files
    library-documentation
    library-short-description
    link-files)
  (import
    (guile)
    (ice-9 ftw)
    (ice-9 regex)
    (rnrs io ports)
    (rnrs sorting)
    (sph)
    (sph documentation)
    (sph documentation shtml)
    (sph filesystem)
    (sph hashtable)
    (sph io)
    (sph list)
    (sph string)
    (sph web publish helper)
    (sph web publish shtml)
    (sxml match)
    (sxml simple))

  (define-syntax-rule (library-documentation s name option ...)
    (md-scm-library-documentation s (q name) option ...))

  (define-syntax-rule (library-short-description s name option ...)
    (md-scm-library-short-description s (q name) option ...))

  (define html-get-title
    (let (title-regexp (make-regexp "<title>(.)*?</title>"))
      (l (file)
        (and-let* ((a (regexp-exec title-regexp (file->string file))))
          (string-drop (string-drop-right (match:substring a) 8) 7)))))

  (define (link-files directory . paths)
    "accepts file paths with optional wildcard characters like (sph filesystem) filesystem-glob.
     example: directory/**/*.html"
    (let (paths (append-map (l (a) (filesystem-glob (string-append directory a))) paths))
      (shtml-links
        (filter-map
          (l (a)
            (and (not (directory? a))
              (let
                ( (title (and (string-suffix? ".html" a) (html-get-title a)))
                  (target (string-drop-prefix directory a)))
                (list target (or title target) #f))))
          paths)
        #f)))

  (define (include-files directory . paths) "accepts file paths like link-files"
    (let (paths (append-map (l (a) (filesystem-glob (string-append directory a))) paths))
      (filter-map
        (l (a) (and (not (directory? a)) (shtml-include (string-drop-prefix directory a)))) paths)))

  (define (md-scm-library-short-description directory module-name)
    (and-let* ((a (module-description module-name))) (first (string-split a #\newline))))

  (define* (md-scm-library-documentation directory module-name #:key (skip-description-line #t))
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
        content))))
