(library (sph web publish itml env)
  (export
    include-files
    library-documentation
    library-short-description
    link-files
    links
    short-description)
  (import
    (guile)
    (ice-9 ftw)
    (ice-9 regex)
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
  ;(define-syntax-rule (link-files s pattern ...) (link-files s pattern ...))

  (define-syntax-rule (include-files s query option ...)
    (itml-include-files s (q query) option ...))

  (define pattern->paths
    (let*
      ( (double-asterisk (make-regexp "^\\*\\*([0-9]+)?$"))
        (parse-skip
          (l (a)
            "**:  of directories
             **n: "
            (and-let* ((a (regexp-exec double-asterisk a)))
              (let (depth (match:substring a 1)) (if depth (string->number depth) (inf))))))
        (parse-match
          (l (a)
            (make-regexp
              (string-append "^"
                (string-replace-string (regexp-quote (string-replace-string a "*" "///")) "///"
                  ".*")
                "$")))))
      (l (root pattern)
        "find files under directory \"root\" matching \"pattern\".
        pattern is a filesystem path with optional wildcard characters.
        * in a file name matches zero or more of any character.
        ? in a file name matches one of any character
        ** skips any sub directories to match the following path. at the end of a path it is the same as **/.*
        **n where n is an integer. like ** but skips nested directories at most n subdirectories deep.
        examples
          directory: directory/**1
          directory/**: directory/**n
          directory/**/test.html"

        (let*
          ( (parsed
              (let
                (pattern (if (string-suffix? "**" pattern) (string-append pattern "/*") pattern))
                (map (l (a) (or (parse-skip a) (parse-match a))) (string-split pattern #\/))))
            (scandir* (l (a) (scandir a (l (a) (not (string-prefix? "." a)))))))
          (let loop
            ( (path (remove-trailing-slash root)) (files (scandir* root)) (result null)
              (directories null) (parsed parsed) (skip 0))
            (if (null? parsed) result
              (if (null? files)
                (append result
                  (let (parsed (if (< 0 skip) parsed (tail parsed)))
                    (append-map
                      (l (path) "directory paths are full paths"
                        (loop path (scandir* path) null null parsed (- skip 1)))
                      directories)))
                (let*
                  ( (pattern (first parsed)) (file (first files))
                    (file-path (string-append path "/" file)))
                  (cond
                    ( (number? pattern)
                      (loop path files result directories (tail parsed) (+ pattern skip)))
                    ( (regexp? pattern)
                      (if (regexp-exec pattern file)
                        (if (directory? file-path)
                          (loop path (tail files) result (pair file-path directories) parsed skip)
                          (loop path (tail files) (pair file-path result) directories parsed skip))
                        (loop path (tail files)
                          result (if (directory? file-path) (pair file-path directories) directories)
                          parsed skip))))))))))))

  (define (link-files s . patterns)
    "** skips sub directories
     * matches files
     directory: directory/**1
     directory/**: directory/**n
     directory/**/test.html"
    (map (l (a) (debug-log (pattern->paths s a))) patterns) "")

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
