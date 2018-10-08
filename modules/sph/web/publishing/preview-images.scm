(library (sph-cms other preview-images)
  (export
    preview-images-create-image
    preview-images-ensure
    preview-images-name->path-public
    preview-images-path
    preview-images-path-public
    sph-cms-lib-preview-images-description)
  (import
    (sph)
    (sph-cms other tagged-files)
    (sph filesystem)
    (sph hashtable)
    (sph list)
    (sph process)
    (sph string)
    (sph web app)
    (only (guile)
      file-exists?
      stat
      stat:mtime)
    (only (sph one) search-env-path))

  (define sph-cms-lib-preview-images-description
    "creates small preview images for all content files with image type png/jpeg/gif")

  (define (create-size-string swa-env) "vector -> string"
    (string-enclose "x" (number->string (or (swa-config-ref swa-env preview-image-size) 64))))

  (define gm-path (first-or-false (search-env-path (list "gm"))))
  (define preview-images-path-public "/image/preview/")

  (define (preview-images-path swa-env)
    (string-append (swa-env-root swa-env) "root/image/preview/"))

  (define (preview-images-name->path-public a) "string:file-name -> string"
    (string-append preview-images-path-public a))

  (define (preview-images-create-image size-string source dest)
    "string:0x0 string string -> unspecified
     create one image"
    (execute gm-path "convert"
      "-size" size-string source "-resize" size-string "+profile" "*" (string-append "jpeg:" dest)))

  (define* (preview-images-ensure swa-env #:optional ids)
    "vector [(integer ...)] -> unspecified
     creates all preview images if they do not already exist.
     if \"ids\" is given, only preview images for content with these ids are ensured"
    (let*
      ( (size-string (create-size-string swa-env))
        (tf-env (ht-ref (swa-env-data swa-env) (q tf-env))) (dest-dir (preview-images-path swa-env))
        (extensions (list "jpg" "png"))
        (paths
          (filter-map
            (l (a)
              (and (not (null? (intersection (tf-file-tags a) extensions)))
                (pair (tf-file-path tf-env a) (string-append dest-dir (tf-file-name a)))))
            (tf-env-files tf-env)))
        (paths
          ; filter existing, up to date destination paths
          (if (file-exists? dest-dir)
            (filter
              (l (a)
                (let ((source (first a)) (dest (tail a)))
                  (or (not (file-exists? dest))
                    (> (stat:mtime (stat source)) (stat:mtime (stat dest))))))
              paths)
            paths)))
      (ensure-directory-structure dest-dir) (debug-log (q preview-images) paths)
      ;(each (l (a) (preview-images-create-image size-string (first a) (tail a))) paths)
      )))
