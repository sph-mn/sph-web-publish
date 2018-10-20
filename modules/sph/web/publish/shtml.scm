(library (sph web publish shtml)
  (export
    shtml-hyperlink
    shtml-include
    shtml-include-css
    shtml-include-javascript
    shtml-layout
    shtml-lines
    shtml-lines-if-multiple
    shtml-link
    shtml-links
    shtml-object
    shtml-page-mtime
    shtml-section
    shtml-top-bar)
  (import
    (sph)
    (sph hashtable)
    (sph list)
    (sph time)
    (sph time string)
    (sph web publish helper)
    (sph web shtml)
    (only (guile) file-exists? string-drop))

  (define*
    (shtml-layout content #:key (title "") (css null) (js null) head body-class top bottom links
      mtime)
    (let
      ( (top-bar (and links (shtml-top-bar links)))
        (page-mtime (and mtime (shtml-page-mtime mtime))))
      (qq
        (html
          (head (title (unquote title)) (unquote-splicing (map shtml-include-css css))
            (meta (@ (name "viewport") (content "width=device-width,initial-scale=1")))
            (unquote head))
          (body (unquote (if body-class (qq (@ (class (unquote body-class)))) null))
            (unquote
              (if (or top-bar top)
                (qq (div (@ (class top)) (unquote (or top-bar "")) (unquote (or top "")))) null))
            (unquote
              (if (or mtime content)
                (qq (div (@ (class "middle")) (unquote (or page-mtime "")) (unquote (or content ""))))
                null))
            (unquote (if bottom (qq (div (@ (class bottom)) (unquote bottom))) null))
            (unquote-splicing (map shtml-include-javascript js)))))))

  (define (shtml-page-mtime mtime) "integer:utc-seconds -> sxml"
    (qq
      (div (@ (class mtime) (title "last modification time of the current page"))
        (unquote (utc->ymd (s->ns mtime))))))

  (define* (shtml-top-bar link-data)
    "((name path) ...) -> shtml
     create a top navigation bar with link to index.html and feed.xml if the files exist"
    (let (links (interleave (map-apply shtml-link link-data) ", "))
      (if (null? links) null (pair (q nav) links))))

  (define shtml-lines
    (let*
      ( (inline-html-tags
          (ht-from-list
            (map (l (a) (pair a a))
              (list-q span a object img script select button input label select textarea))))
        (inline-html-tag? (l (a) (ht-ref inline-html-tags a))) (line-wrap (l (a) (list (q p) a)))
        (line-list (l (a) (pair (q p) a)))
        (splice-non-tag-lists (l (a) (splice (l (a) (or (null? a) (not (symbol? (first a))))) a))))
      (l (a)
        "list integer -> list
          receives a list of expressions that eventually become separate lines.
          rules:
          * html inline elements are wrapped with <p>
          * html block elements are left as is
          * list contents become <p> contents
          * other elements are wrapped with <p>
          * on the first level of the given list, lists that
          do not correspond to html tags are spliced.
          these lists are assumed to contain result elements
          from dynamic code evaluation"
        (let loop ((a (splice-non-tag-lists a)))
          (if (null? a) a
            (let ((a (first a)) (b (loop (tail a))))
              (if (list? a)
                (let (a (splice-non-tag-lists a))
                  (if (null? a) b
                    (let (prefix (first a))
                      (pair
                        (if (symbol? prefix) (if (inline-html-tag? prefix) (line-wrap a) a)
                          (line-list a))
                        b))))
                (pair (line-wrap a) b))))))))

  (define (shtml-lines-if-multiple a)
    "list -> sxml
     only add lines if \"a\" has more than one element"
    (if (< 1 (length a)) (shtml-lines a) a))

  (define (shtml-link target title)
    (shtml-hyperlink target title
      (if (swp-url-external? target) (q ((class "external") (target "_blank"))) null)))

  (define (shtml-links link-data collapsed) "link-data: (name url description)"
    (if (null? link-data) null
      (let
        (anchors
          (map-apply
            (l (name url description)
              (if description
                (list (q p) (shtml-link url name)
                  " "
                  (if (list? description)
                    (interleave (map (l a (pair (q span) a)) description) "|") description))
                (list (q p) (shtml-link url name))))
            link-data))
        (if collapsed (interleave anchors ", ") (shtml-lines-if-multiple anchors)))))

  (define* (shtml-object url #:optional (attributes null) (params null))
    "string string -> sxml
     sxml for an html <object> tag"
    (qq
      (object (@ (data (unquote url)) (unquote-splicing attributes))
        (unquote
          (map (l (a) (qq (param (@ (name (unquote (first a))) (value (unquote (tail a)))))))
            params)))))

  (define (shtml-include path)
    (qq (div (@ (class "included")) (unquote (shtml-object path (list-q (class "included")))))))

  (define (shtml-csv data) "(vector ...) -> sxml" (shtml-list->table (map vector->list data)))
  (define (shtml-plaintext a) (shtml-text->sxml a))
  (define (shtml-preformatted a) (list (q pre) a)))
