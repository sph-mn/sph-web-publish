(define-module (sph web publish markdown scm-env))

(use-modules (srfi srfi-1) (ice-9 regex)
  (sph) (sph filesystem)
  (sph io) (sph list) (sph string) (sph web publish markdown) (sph web publish shtml))

(export include-files link-files
  include-files-reverse link-files-reverse include-images include-images-reverse)

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

(define (include-file-paths directory paths)
  (append-map
    (l (a) (filter (compose not directory?) (filesystem-glob (string-append directory a)))) paths))

(define (link-files directory . paths)
  "string:compiled-directory string ... -> shtml
   accepts file paths with optional wildcard characters like (sph filesystem) filesystem-glob.
   example: directory/**/*.html"
  (let (paths (include-file-paths directory paths))
    (shtml-links
      (list-sort-with-accessor string<? first
        (filter-map
          (l (a)
            (let*
              ( (relative-path (string-drop-prefix directory a))
                (title (link-files-get-title directory relative-path a)) (description (tail title))
                (title (first title)) (web-path (string-append "/" relative-path)))
              (list (or title (basename web-path)) web-path description)))
          paths))
      #f)))

(define (include-files directory . paths) "accepts file paths like link-files"
  (let (paths (include-file-paths directory paths))
    (filter-map (l (a) (shtml-include (string-append "/" (string-drop-prefix directory a)))) paths)))

(define (include-images directory . paths) "accepts file paths like link-files"
  (let*
    ( (image-extensions (list "jpg" "png"))
      (paths
        (filter-map
          (l (a)
            (and (contains? image-extensions (filename-extension a))
              (string-append "/" (string-drop-prefix directory a))))
          (include-file-paths directory paths)))
      (thumbnail-paths (map (l (a) (string-append (dirname a) "/thumbnails/" (basename a))) paths)))
    (map
      (l (path thumbnail-path)
        (qq
          (a (@ (href (unquote path)) (class "thumbnail")) (img (@ (src (unquote thumbnail-path)))))))
      paths thumbnail-paths)))

(define (include-files-reverse . a) (reverse (apply include-files a)))
(define (include-images-reverse . a) (reverse (apply include-images a)))
(define (link-files-reverse . a) (reverse (apply link-files a)))
