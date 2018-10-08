(library (sph-cms other lang itml)
  (export
    ol-itml-content-render
    ol-itml-eval-file
    ol-itml-eval-port
    ol-itml-eval-proc
    ol-itml-eval-string
    ol-itml-exception-obj->string
    ol-itml-exception-obj?
    ol-itml-include-c
    ol-itml-init
    ol-itml-library-documentation
    ol-itml-library-short-description
    ol-itml-line->short-description
    ol-itml-link
    ol-itml-link-c
    ol-itml-link-to
    ol-itml-links
    ol-itml-modules-plaintext
    ol-itml-modules-shtml
    ol-itml-state-create
    ol-itml-state-swa-env
    ol-itml-state-views
    ol-itml-state-views-ref-q
    ol-itml-swa-env-itml-env
    sph-cms-ol-itml-description)
  (import
    (rnrs eval)
    (rnrs exceptions)
    (sph)
    (sph documentation)
    (sph documentation shtml)
    (sph filesystem)
    (sph hashtable)
    (sph io)
    (sph lang itml eval)
    (sph lang itml eval plaintext)
    (sph lang itml eval shtml)
    (sph list)
    (sph string)
    (sph web app)
    (sph web shtml)
    (only (guile)
      basename
      string-downcase
      string-contains
      string-split))

  (define sph-cms-ol-itml-description
    "helpers to convert the itml markup language to the supported output formats.
     the itml-state is needed mostly for tracking nesting depth and to prevent circular inclusion.
     # data structures
     itml-state: (list integer:depth hashtable:{env custom-keys})")

  (define-syntax-rule (ol-itml-state-views-ref-q a key) (ht-ref-q (ol-itml-state-views a) key))
  (define default-mode (q production))
  (define-as itml-eval-ht ht-create-symbol-q plaintext itml-plaintext-eval shtml itml-shtml-eval)

  (define ol-itml-modules-shtml
    (list-q (sph) (sph alist) (sph-cms other lang itml env) (sph lang itml eval env shtml)))

  (define ol-itml-modules-plaintext
    (list-q (sph) (sph alist) (sph-cms other lang itml env) (sph lang itml eval env plaintext)))

  (define-as ol-itml-link-name-handlers ht-create-symbol-q
    b (l (a) "basename" (string-replace-string (string-downcase (basename a)) "_" " "))
    n
    (l (a) "basename without suffix"
      (string-replace-string (remove-filename-extension (basename a)) "_" " "))
    h oh-url-hostname u oh-url-drop-www-and-protocol)

  (define (ol-itml-swa-env-itml-env swa-env format)
    (and-let* ((a (ht-ref (swa-env-data swa-env) (q itml-env)))) (ht-ref a format)))

  (define (ol-itml-state-swa-env a) (ht-ref-q (itml-state-data a) swa-env))
  (define (ol-itml-state-views a) (ht-ref-q (itml-state-data a) views))

  (define* (ol-itml-content-render itml id file-path type #:optional options)
    (if type
      (string-case type ("itml" (ol-itml-eval-file file-path itml))
        ("plaintext" ((ol-itml-state-views-ref-q itml plaintext) (file->string file-path)))
        ("preformatted" ((ol-itml-state-views-ref-q itml preformatted) (file->string file-path)))
        ("csv" ((ol-itml-state-views-ref-q itml csv) (oh-csv->list file-path)))
        ("url"
          (let (url (file->string file-path))
            ((ol-itml-state-views-ref-q itml hyperlink-e) url url)))
        (else
          ( (ol-itml-state-views-ref-q itml reference) type (oh-url-to (q content) id "data")
            id options)))
      ((ol-itml-state-views-ref-q itml hyperlink) (oh-url-to (q content) id "data") "content")))

  (define* (ol-itml-include-c itml query #:key limit sort view-options) "include content"
    ;todo: sort
    (let*
      ( (env-data (swa-env-data (ol-itml-state-swa-env itml)))
        (files (m-content-find env-data query limit)))
      (if (null? files) null
        ( (ol-itml-state-views-ref-q itml include-c)
          (map
            (l (a)
              (ol-itml-content-render itml (m-content-file-id a)
                (m-content-file-path env-data a) (m-content-file-type a) view-options))
            files)))))

  (define (ol-itml-link-c-title-and-description itml title id type tags v-link-c-default-title)
    "# title
     string: use as is
     symbol: title or title-and-description: first line if of type text, tags otherwise
     else: content id"
    (or
      (cond
        ((string? title) (pair title #f))
        ( (symbol? title)
          (case title
            ( (title description)
              (m-content-text-initial-lines (swa-env-data (ol-itml-state-swa-env itml)) id
                (l (a b)
                  (and a
                    (pair a
                      (if (and b (eq? (q description) title) (string-equal? "itml" type))
                        (false-if-exception
                          (string-trim-right (ol-itml-line->short-description b itml) #\.))
                        b))))))
            (else #f)))
        (else #f))
      (pair (v-link-c-default-title id tags) #f)))

  (define*
    (ol-itml-link-c itml query #:key limit (title (q content)) (sub-path "view") exclude-tags)
    "create a list of multiple links to content elements.
     title can be a string or one of the symbols title/description/tags"
    (let*
      ( (env-data (swa-env-data (ol-itml-state-swa-env itml)))
        (files (m-content-find env-data query limit))
        (v-link-c (ol-itml-state-views-ref-q itml link-c))
        (v-link-c-default-title (ol-itml-state-views-ref-q itml link-c-default-title)))
      (if (null? files) null
        (map
          (l (a)
            (let*
              ( (id (m-content-file-id a)) (type (m-content-file-type a))
                (tags (m-content-file-tags a))
                (url
                  (if (string-equal? "url.txt" type)
                    (file->string (m-content-file-path env-data a))
                    (oh-url-to (q content) id sub-path)))
                (thumbnail
                  (and (contains? (list "jpg" "png") type)
                    (preview-images-name->path-public (m-content-file-name a))))
                (title
                  (ol-itml-link-c-title-and-description itml title
                    id type tags v-link-c-default-title)))
              (v-link-c url (first title) (tail title) thumbnail)))
          files))))

  (define* (ol-itml-links itml url/name/description #:key collapsed sorted)
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
          (map
            (l* (url #:optional name description)
              (let (name (if (symbol? name) ((ht-ref ol-itml-link-name-handlers name) url) name))
                ; name as first element for sorting
                (list name url description)))
            url/name/description))
        (link-data (if sorted (list-sort-with-accessor string<? first link-data) link-data)))
      ((ol-itml-state-views-ref-q itml links) link-data collapsed)))

  (define (ol-itml-link itml target title) ((ol-itml-state-views-ref-q itml link) target title))

  (define (ol-itml-library-short-description itml module-name)
    (and-let* ((a (module-description module-name))) (first (string-split a #\newline))))

  (define* (ol-itml-library-documentation itml module-name #:key (skip-description-line #t))
    "render documentation for libraries.
     if #:skip-description-line is true, the first line of the module description read from the
     module description variable is left out. this is for combination with shtml-library-short-description
     which is recognised by link creators"
    ; currently shtml only
    (if (eq? (q shtml) (ht-ref-q (itml-state-data itml) output-format))
      (let*
        ( (depth (itml-state-depth itml))
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
          content))
      (ol-itml-state-views-ref-q itml placeholder)))

  (define (ol-itml-init swa-env)
    (let ((env-data (swa-env-data swa-env)) (env-ht (ht-create-symbol)))
      (ht-set! env-data (q itml-env) env-ht)
      (ht-set! env-ht (q shtml) (apply environment ol-itml-modules-shtml))
      (ht-set! env-ht (q plaintext) (apply environment ol-itml-modules-plaintext))))

  (define* (ol-itml-state-create swa-env output-format views #:key data (depth 0))
    ; depth is also included in the data part to make it easier to access
    (let (mode (or (and swa-env (ht-ref-q (swa-env-config swa-env) mode)) default-mode))
      (itml-state-create depth (ol-itml-swa-env-itml-env swa-env output-format)
        (eq? (q development) mode)
        (let ((itml-eval (ht-ref itml-eval-ht output-format)) (data (or data (ht-create-symbol))))
          (ht-set-multiple-q! data itml-eval
            itml-eval views views depth depth swa-env swa-env output-format output-format)
          data))))

  (define (ol-itml-eval-proc itml-eval-any)
    (l (a itml)
      (itml-eval-call a itml
        (itml-eval-call-proc itml-eval-any (ht-ref (itml-state-data itml) (q itml-eval))))))

  (define ol-itml-eval-port (ol-itml-eval-proc itml-eval-port))
  (define ol-itml-eval-string (ol-itml-eval-proc itml-eval-string))
  (define ol-itml-eval-file (ol-itml-eval-proc itml-eval-file))

  (define (ol-itml-line->short-description a itml)
    (and (string-contains a "short-description")
      (let (r (ol-itml-eval-string a itml)) (and (list? r) (simplify (tail (first r))))))))
