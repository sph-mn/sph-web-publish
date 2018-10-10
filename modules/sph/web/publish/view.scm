(library (sph web publish view)
  (export)
  (import
    (guile)
    (rnrs exceptions)
    (sph)
    (sph-cms other helper)
    (sph hashtable)
    (sph lang itml eval shtml)
    (sph list)
    (sph string)
    (sph web app)
    (sph web shtml)
    (only (sph list) interleave))

  (define (shtml-tag-with-style tag style . content)
    (pairs tag (qq (@ (style (unquote style)))) content))

  (define (lines-if-multiple a)
    "list -> sxml
     only add lines if \"a\" has more than one element"
    (if (< 1 (length a)) (itml-shtml-lines a) a))

  (define placeholder "_")

  (define (page-mtime mtime) "integer -> sxml"
    (qq
      (div (@ (class mtime) (title "last modification time of the current page"))
        (unquote (oh-mtime-string mtime)))))

  (define (v-layout v content)
    (let*
      ( (swa-env (or (v (q swa-env) #f) (raise (q missing-template-variable-swa-env))))
        (config (swa-env-config swa-env)) (data (swa-env-data swa-env)))
      (let
        ( (title (v (q title) (ht-ref-q config default-page-title)))
          (css (map shtml-include-css (v (q css) (list))))
          (js (map shtml-include-javascript (v (q js) (list)))) (top (v (q top) #f))
          (navigation (v (q navigation) #f)))
        (qq
          (html (@ (id top))
            (head (title (unquote title)) (unquote-splicing css)
              (meta (@ (name "viewport") (content "width=device-width,initial-scale=1")))
              (unquote (v (q head))))
            (body
              (unquote (or (and-let* ((a (v (q body-class)))) (qq (@ (class (unquote a))))) ""))
              (unquote
                (if (or navigation top)
                  (qq (div (@ (class top)) (unquote navigation) (unquote top))) ""))
              (unquote (if (null? content) content (qq (div (@ (class middle)) (unquote content)))))
              (unquote
                (or
                  (and-let* ((bottom (v (q bottom) #f)))
                    (qq (div (@ (class bottom)) (unquote bottom))))
                  ""))
              (unquote-splicing js)))))))

  (define (v-view v content)
    (let
      ( (mtime (or (and-let* ((a (v (q mtime) #f))) (page-mtime a)) ""))
        (class (or (and-let* ((a (v (q type-name) #f))) (string-append "content " a)) "content")))
      (list-qq (unquote mtime) (div (@ (class (unquote class))) (unquote content)))))

  (define (v-emphasis a) (shtml-tag-with-style (q span) "font-style:italic" a))
  (define (v-strong-emphasis a) (shtml-tag-with-style (q span) "font-weight:bold" a))

  (define (v-align where a)
    (shtml-tag-with-style (q div) (string-append "text-align:" (symbol->string where)) a))

  (define* (v-navigation link-data) "((name . string:url) ...) -> sxml"
    (if (null? link-data) null
      (let*
        ((links (map (l (a) (link (tail a) (first a))) link-data)) (links (interleave links ", ")))
        (qq (nav (@ (class "main")) (unquote-splicing links))))))

  (define* (v-object url mime-type #:optional (attributes null))
    "string string -> sxml
     sxml for an html <object> tag"
    (qq
      (object (@ (data (unquote url)) (type (unquote mime-type)) (unquote-splicing attributes)) "")))

  (define (v-link-c-default-title id tags)
    (list (qq (span (@ (class "id")) (unquote id))) (string-join tags " ")))

  (define (v-link target title)
    (shtml-hyperlink target title (if (oh-url-external? target) (q ((class "external"))) null)))

  (define* (v-link-c url title description thumbnail)
    (let*
      ( (thumbnail (if thumbnail (qq (img (@ (src (unquote thumbnail)) (class "preview")))) ""))
        (anchor (link url (list thumbnail title))))
      (if description (list (q p) anchor " " description) anchor)))

  (define (v-links link-data collapsed) "link-data: (name url description)"
    (if (null? link-data) null
      (let
        (anchors
          (map-apply
            (l (name url description)
              (if description
                (list (link url name) " "
                  (if (list? description)
                    (interleave (map (l a (pair (q span) a)) description) "|") description))
                (link url name)))
            link-data))
        (if collapsed (interleave anchors ", ") (lines-if-multiple anchors)))))

  (define (v-include-c sxml) (qq (div (@ (class "included")) (unquote-splicing sxml))))
  (define (v-csv data) "(vector ...) -> sxml" (shtml-list->table (map vector->list data)))
  (define (v-plaintext a) (shtml-text->sxml a))
  (define (v-preformatted a) (list (q pre) a))

  #;(
  (define lines itml-shtml-lines)

  (define* (reference type url id #:optional options)
    "integer string -> sxml
     reference content data using an object tag if it is a renderable cms type or link it"
    (let (mime-type (cms-type-ref-first id mime type))
      (if (string-equal? "application/octet-stream" mime-type) (content-hyperlink url "content")
        (shtml-object url mime-type (pair (pair (q title) (dg-file-name id)) (or options null))))))

  (define (reference-multiple ids types format)
    "(integer ...) (integer ...) symbol:html/data -> sxml
     <object> tags for content in an <ul>"
    (pair (q (ul (@ (class content))))
      (map (l (id type) (qq (li (unquote (content-reference type (url-to-c id) id))))) ids types)))
  ))
