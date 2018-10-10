(library (sph web publish helper)
  (export
    csv->list
    url-drop-www-and-protocol
    url-external?
    url-hostname)
  (import
    (sph))

  (define url-external?
    (let (protocol-regexp (make-regexp "[a-zA-Z0-9]+://"))
      (l (a) (and (not (string-prefix? "/" a)) (string-match protocol-regexp a)))))

  (define csv->list
    (let (csv-reader (make-csv-reader #\,))
      (l (file-path) (call-with-input-file file-path csv-reader))))

  (define (url-drop-www-and-protocol a) "string -> string"
    (string-drop-prefix-if-exists "www."
      (if (string-prefix? "https://" a) (string-drop-prefix "https://" a)
        (if (string-prefix? "http://" a) (string-drop-prefix "http://" a) a))))

  (define (url-hostname a) "string -> string"
    (let (a-split (string-split (uri-host (string->uri a)) #\.))
      (if (= 3 (length a-split)) (second a-split) (first a-split)))))
