(define-module (sph web publish helper))

(import (srfi srfi-1) (csv csv)
  (ice-9 ftw) (ice-9 regex)
  (sph) (sph alist)
  (sph lang config) (sph lang scheme)
  (sph time) (sph time string)
  (sph list) (sph other) (sph process) (sph string) (sph web atom) (sxml simple) (web uri))

(export quote-triple-second shtml-heading-tag->number
  shtml-heading? swp-atom-feed
  swp-config-read swp-create-thumbnail-proc
  swp-csv->list swp-delete-file-recursively
  swp-file-handlers-normalise swp-file-system-fold swp-recent-changes swp-url-external?)

"quote the second element of each triple."
"example: (a (quote b) c d (quote e) f)"

(define-syntax-rules quote-triple-second (() (quote ()))
  ((a) (quasiquote ((unquote a)))) ((a b) (quasiquote ((unquote a) b)))
  ( (a b c d ...)
    (quasiquote ((unquote a) b (unquote c) (unquote-splicing (quote-triple-second d ...))))))

(define swp-url-external?
  (let (protocol-regexp (make-regexp "[a-zA-Z0-9]+://"))
    (l (a) (and (not (string-prefix? "/" a)) (regexp-exec protocol-regexp a)))))

(define swp-csv->list
  (let (csv-reader (make-csv-reader #\,))
    (l (file-path) (call-with-input-file file-path csv-reader))))

(define (shtml-heading? a)
  (and (list? a) (not (null? a)) (containsq? (q (h1 h2 h3 h4 h5 h6)) (first a))))

(define (shtml-heading-tag->number a) (string->number (string-drop (symbol->string a) 1)))
(define (swp-config-read path) (config-read-file path))

(define* (swp-atom-feed directory port #:key (title "feed") rights)
  (let*
    ( (mtimes-and-paths
        (take* 10
          (list-sort-with-accessor > first
            (swp-file-system-fold directory null
              null (l (path stat result) (pair (pair (stat:mtime stat) path) result))))))
      (most-recent-update (if (null? mtimes-and-paths) 0 (first (first mtimes-and-paths))))
      (sxml
        (apply atom-feed title
          title most-recent-update
          #:rights rights
          (map
            (l (a)
              (let* ((path (tail a)) (name (basename path)))
                (atom-entry name name (first a) #:link (string-drop-prefix directory path))))
            mtimes-and-paths))))
    (display "<?xml version=\"1.0\"?>" port) (sxml->xml sxml port)))

(define* (swp-recent-changes directory port #:key (title "recent") (recent-excludes (list)))
  (let*
    ( (mtimes-and-paths
        (take* 20
          (filter
            (l (a)
              (let*
                ( (path (tail a))
                  (relative-path
                    (string-replace-string path (string-append directory "/.swp/compiled") "")))
                (not
                  (or (string-suffix? relative-path "recent.md")
                    (string-suffix? relative-path "recent.html")
                    (string-contains relative-path "/sources/")
                    (any (l (a) (string-prefix? a relative-path)) recent-excludes)))))
            (list-sort-with-accessor > first
              (swp-file-system-fold (string-append directory "/.swp/compiled") null
                null (l (path stat result) (pair (pair (stat:mtime stat) path) result)))))))
      (most-recent-update (if (null? mtimes-and-paths) 0 (first (first mtimes-and-paths)))))
    (display "# recent updates\n" port)
    (each
      (l (a)
        (let*
          ( (path (tail a)) (name (basename path)) (date-string (utc->ymd (s->ns (first a))))
            (public-path (string-drop-prefix (string-append directory "/.swp/compiled") path)))
          (simple-format port "* [~A](~A), ~A\n" name public-path date-string)))
      mtimes-and-paths)))

(define (swp-file-system-fold file-name ignored-paths init f) "procedure string:path -> boolean"
  (let
    ( (leaf (l (path stat result) (and result (f path stat result))))
      (enter?
        (l (path stat result)
          (and result (every (l (a) (not (string-prefix? a path))) ignored-paths)
            (not (string-prefix? "." (basename path))))))
      (ignore (l (path stat result) result))
      (error
        (l (path stat errno result)
          (format (current-error-port) "warning: ~a: ~a~%" path (strerror errno)) #f)))
    (file-system-fold enter? leaf ignore ignore ignore error init file-name)))

(define (swp-delete-file-recursively file-name)
  (let
    ( (leaf (l (path stat result) (delete-file path))) (up (l (path stat result) (rmdir path)))
      (true (const #t))
      (error
        (l (path stat errno result)
          (format (current-error-port) "warning: ~a: ~a~%" path (strerror errno)) #t)))
    (file-system-fold true leaf true up true error #t file-name)))

(define (swp-create-thumbnail-proc size)
  (let (imagemagick-path (first-or-false (search-env-path (list "magick"))))
    (if imagemagick-path
      (let* ((size (number->string size)) (size-string (string-append size "x" size)))
        (l (path target-path) "only take the first image from animations"
          (execute imagemagick-path (string-append path "[0]")
            "-resize" size-string "-quality" "96" "+profile" "*" target-path)))
      (begin (display-line "warning: gm utility not found. thumbnail processing deactivated")
        (const #t)))))
