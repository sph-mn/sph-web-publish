(library (sph web publish helper)
  (export
    csv->list
    quote-triple-second
    url-drop-www-and-protocol
    url-external?
    url-hostname)
  (import
    (csv csv)
    (guile)
    (ice-9 regex)
    (sph)
    (sph string)
    (sph web atom)
    (sxml simple)
    (web uri))

  (define-syntax-rules quote-triple-second
    ; quote the second element of each triple.
    ; example: (a (quote b) c d (quote e) f)
    (() (quote ())) ((a) (quasiquote ((unquote a))))
    ((a b) (quasiquote ((unquote a) b)))
    ( (a b c d ...)
      (quasiquote ((unquote a) b (unquote c) (unquote-splicing (quote-triple-second d ...))))))

  (define url-external?
    (let (protocol-regexp (make-regexp "[a-zA-Z0-9]+://"))
      (l (a) (and (not (string-prefix? "/" a)) (regexp-exec protocol-regexp a)))))

  (define csv->list
    (let (csv-reader (make-csv-reader #\,))
      (l (file-path) (call-with-input-file file-path csv-reader))))

  (define (url-drop-www-and-protocol a) "string -> string"
    (string-drop-prefix-if-exists "www."
      (if (string-prefix? "https://" a) (string-drop-prefix "https://" a)
        (if (string-prefix? "http://" a) (string-drop-prefix "http://" a) a))))

  (define (url-hostname a) "string -> string"
    (let (a-split (string-split (uri-host (string->uri a)) #\.))
      (if (= 3 (length a-split)) (second a-split) (first a-split))))

  (define* (atom-feed-from-files paths port #:key title rights)
    (let*
      ( (mtimes-and-paths (map (compose stat:mtime stat) paths))
        (most-recent-update (if (null? mtimes-and-paths) 0 (first (first mtimes-and-paths))))
        (sxml
          (atom-feed title title
            most-recent-update #:rights
            rights null
            (map
              (l (a)
                a
                #;(atom-entry id title
                  updated #:key
                  authors categories content contributors link published rights source summary))
              mtimes-and-paths))))
      (display "<?xml version=\"1.0\"?>" port) (sxml->xml sxml port))))
