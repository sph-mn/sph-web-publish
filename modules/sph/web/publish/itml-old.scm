(define (itml-eval-f itml-eval-any)
  (l (a itml)
    (itml-eval-call a itml
      (itml-eval-call-f itml-eval-any (ht-ref (itml-state-data itml) (q itml-eval))))))

(define-as itml-eval-port itml-eval-f itml-eval-port)
(define-as itml-eval-string itml-eval-f itml-eval-string)
(define-as itml-eval-file itml-eval-f itml-eval-file)
(define-as itml-modules list-q (sph) (sph alist) (sph web publish itml env))
(define-as itml-modules-shtml append itml-modules (list-q (sph lang itml eval env shtml)))
(define-as itml-modules-plaintext append itml-modules (list-q (sph lang itml eval env plaintext)))

(define (itml-line->short-description a itml)
  (and (string-contains a "short-description")
    (let (b (itml-eval-string a itml)) (and (list? b) (simplify (tail (first b)))))))

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
        (map
          (l* (url #:optional name description)
            (let (name (if (symbol? name) ((ht-ref ol-itml-link-name-handlers name) url) name))
              ; name as first element for sorting
              (list name url description)))
          url/name/description))
      (link-data (if sorted (list-sort-with-accessor string<? first link-data) link-data)))
    ((ol-itml-state-views-ref-q itml links) link-data collapsed)))

(define (itml-link itml target title) ((itml-state-views-ref-q itml link) target title))

(define (itml-init)
  (let (env-ht (ht-create-symbol))
    (ht-set! env-ht (q shtml) (apply environment itml-modules-shtml))
    (ht-set! env-ht (q plaintext) (apply environment itml-modules-plaintext))))

(define* (itml-state-create swa-env output-format views #:key data (depth 0))
  ; depth is also included in the data part to make it easier to access
  (let (mode (or (and swa-env (ht-ref-q (swa-env-config swa-env) mode)) default-mode))
    (itml-state-create depth (itml-swa-env-itml-env swa-env output-format)
      (eq? (q development) mode)
      (let ((itml-eval (ht-ref itml-eval-ht output-format)) (data (or data (ht-create-symbol))))
        (ht-set-multiple-q! data itml-eval
          itml-eval views views depth depth swa-env swa-env output-format output-format)
        data))))

;--old--;
(define-syntax-rule (ol-itml-state-views-ref-q a key) (ht-ref-q (ol-itml-state-views a) key))
(define default-mode (q production))
(define-as itml-eval-ht ht-create-symbol-q plaintext itml-plaintext-eval shtml itml-shtml-eval)

(define (ol-itml-swa-env-itml-env swa-env format)
  (and-let* ((a (ht-ref (swa-env-data swa-env) (q itml-env)))) (ht-ref a format)))

(define (ol-itml-state-swa-env a) (ht-ref-q (itml-state-data a) swa-env))
(define (ol-itml-state-views a) (ht-ref-q (itml-state-data a) views))

(define* (ol-itml-content-render itml id file-path type #:optional options)
  (if type
    (string-case type ("itml" (ol-itml-eval-file file-path itml))
      ("txt" ((ol-itml-state-views-ref-q itml plaintext) (file->string file-path)))
      ("pre.txt" ((ol-itml-state-views-ref-q itml preformatted) (file->string file-path)))
      ("csv" ((ol-itml-state-views-ref-q itml csv) (csv->list file-path)))
      ("url"
        (let (url (file->string file-path)) ((ol-itml-state-views-ref-q itml hyperlink-e) url url)))
      (else
        ((ol-itml-state-views-ref-q itml reference) type (url-to (q content) id "data") id options)))
    ((ol-itml-state-views-ref-q itml hyperlink) (url-to (q content) id "data") "content")))

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
            (m-content-text-initial-lines (swa-env-data (itml-state-swa-env itml)) id
              (l (a b)
                (and a
                  (pair a
                    (if (and b (eq? (q description) title) (string-equal? "itml" type))
                      (false-if-exception
                        (string-trim-right (itml-line->short-description b itml) #\.))
                      b))))))
          (else #f)))
      (else #f))
    (pair (v-link-c-default-title id tags) #f)))

(define* (itml-link-c itml query #:key limit (title (q content)) (sub-path "view") exclude-tags)
  "create a list of multiple links to content elements.
   title can be a string or one of the symbols title/description/tags"
  (let*
    ( (env-data (swa-env-data (itml-state-swa-env itml)))
      (files (m-content-find env-data query limit)) (v-link-c (itml-state-views-ref-q itml link-c))
      (v-link-c-default-title (itml-state-views-ref-q itml link-c-default-title)))
    (if (null? files) null
      (map
        (l (a)
          (let*
            ( (id (m-content-file-id a)) (type (m-content-file-type a))
              (tags (m-content-file-tags a))
              (url
                (if (string-equal? "url.txt" type) (file->string (m-content-file-path env-data a))
                  (url-to (q content) id sub-path)))
              (thumbnail
                (and (contains? (list "jpg" "png") type)
                  (preview-images-name->path-public (m-content-file-name a))))
              (title
                (itml-link-c-title-and-description itml title id type tags v-link-c-default-title)))
            (v-link-c url (first title) (tail title) thumbnail)))
        files))))
