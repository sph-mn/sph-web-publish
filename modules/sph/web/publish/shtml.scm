(library (sph web publish shtml)
  (export
    shtml-layout
    shtml-lines-if-multiple
    shtml-link
    shtml-section
    shtml-include-css
    shtml-include-js
    shtml-links)
  (import
    (sph)
    (sph lang itml eval shtml)
    (sph list)
    (sph web publish helper)
    (sph web shtml))

  (define*
    (shtml-layout content #:key (title "") (css null) (js null) top navigation head body-class
      bottom)
    (qq
      (html
        (head (title (unquote title)) (unquote-splicing (map shtml-include-css css))
          (meta (@ (name "viewport") (content "width=device-width,initial-scale=1"))) (unquote head))
        (body (unquote (if body-class (qq (@ (class (unquote body-class)))) null))
          (unquote
            (if (or navigation top) (qq (div (@ (class top)) (unquote navigation) (unquote top)))
              null))
          (unquote (if content (qq (div (@ (class middle)) (unquote content))) null))
          (unquote (if bottom (qq (div (@ (class bottom)) (unquote bottom))) null))
          (unquote-splicing (map shtml-include-javascript js))))))

  (define (shtml-lines-if-multiple a)
    "list -> sxml
     only add lines if \"a\" has more than one element"
    (if (< 1 (length a)) (itml-shtml-lines a) a))

  (define (shtml-link target title)
    (shtml-hyperlink target title (if (url-external? target) (q ((class "external"))) null)))

  (define (shtml-links link-data collapsed) "link-data: (name url description)"
    (if (null? link-data) null
      (let
        (anchors
          (map-apply
            (l (name url description)
              (if description
                (list (shtml-link url name) " "
                  (if (list? description)
                    (interleave (map (l a (pair (q span) a)) description) "|") description))
                (shtml-link url name)))
            link-data))
        (if collapsed (interleave anchors ", ") (shtml-lines-if-multiple anchors))))))
