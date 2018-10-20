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
    (sph alist)
    (sph documentation)
    (sph documentation shtml)
    (sph filesystem)
    (sph hashtable)
    (sph io)
    (sph list)
    (sph string)
    (sph web publish helper)
    (sph web publish markdown)
    (sph web publish shtml)
    (sxml match)
    (sxml simple))

  (define-syntax-rule (library-documentation s name option ...)
    (md-scm-library-documentation s (q name) option ...))

  (define-syntax-rule (library-short-description s name option ...)
    (md-scm-library-short-description s (q name) option ...))

  (define link-files-get-title
    (let*
      ( (html-title-regexp (make-regexp "<title>(.)*?</title>"))
        (html-get-title
          (l (file)
            (let (a (regexp-exec html-title-regexp (file->string file)))
              (pair (and a (string-drop (string-drop-right (match:substring a) 8) 7)) #f)))))
      (l (directory relative-path full-path)
        "string string:relative-path -> false/(string . false/string)
        get title and description from file content"
        (cond
          ( (string-suffix? ".html" relative-path)
            (let
              (source
                (string-append (dirname (dirname directory)) "/"
                  (string-drop-right relative-path 5) ".md"))
              (if (file-exists? source) (swp-md-get-title source) (html-get-title full-path))))
          (else (pair #f #f))))))

  (define (link-files directory . paths)
    "string:compiled-directory string ... -> shtml
     accepts file paths with optional wildcard characters like (sph filesystem) filesystem-glob.
     example: directory/**/*.html"
    (let (paths (append-map (l (a) (filesystem-glob (string-append directory a))) paths))
      (shtml-links
        (filter-map
          (l (a)
            (and (not (directory? a))
              (let*
                ( (relative-path (string-drop-prefix directory a))
                  (title (link-files-get-title directory relative-path a)) (description (tail title))
                  (title (first title)))
                (list relative-path (or title relative-path) description))))
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
