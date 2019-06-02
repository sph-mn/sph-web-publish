(library (sph web publish)
  (export
    swp-atom-feed-task
    swp-cli-new
    swp-default-cli
    swp-default-config
    swp-default-file-handlers
    swp-env-config
    swp-env-copy-file
    swp-env-create-thumbnail
    swp-env-directory
    swp-env-file-handlers
    swp-env-swp-directory
    swp-env-swp-target-directory
    swp-file-handler-f
    swp-file-handler-last
    swp-file-handler-match
    swp-file-handler-name
    swp-file-handler-path-f
    swp-remove-disabled-file-handlers)
  (import
    (guile)
    (ice-9 ftw)
    (rnrs exceptions)
    (sph)
    (sph alist)
    (sph cli)
    (sph filesystem)
    (sph hashtable)
    (sph lang config)
    (sph lang plcss)
    (sph lang scheme)
    (sph list)
    (sph other)
    (sph process)
    (sph string)
    (sph vector)
    (sph web publish helper)
    (sph web publish markdown)
    (sph web publish shtml)
    (sxml simple)
    (only (srfi srfi-1) unzip3))

  (define swp-env-directory (vector-accessor 1))
  (define swp-env-swp-directory (vector-accessor 2))
  (define swp-env-swp-target-directory (vector-accessor 3))
  (define swp-env-config (vector-accessor 4))
  (define swp-env-file-handlers (vector-accessor 5))
  (define swp-env-copy-file (vector-accessor 6))
  (define swp-env-create-thumbnail (vector-accessor 7))
  (define swp-file-handler-name (vector-accessor 0))
  (define swp-file-handler-match (vector-accessor 1))
  (define swp-file-handler-last (vector-accessor 2))
  (define swp-file-handler-path-f (vector-accessor 3))
  (define swp-file-handler-f (vector-accessor 4))
  (define (swp-file-handler-new name match last path-f f) (vector name match last path-f f))

  (define (swp-file-handlers-normalise . a)
    "accept strings, list of strings, booleans and procedures as
     file name matcher and create a matcher procedure.
     ensure that handler lists for 3 passes are given - only 3 passes are supported"
    (map
      (l (pass)
        (map
          (l (a)
            (let (match (swp-file-handler-match a))
              (if (procedure? match) match
                (swp-file-handler-new (swp-file-handler-name a)
                  (cond
                    ((string? match) (l (path) (string-suffix? match path)))
                    ((list? match) (l (path) (any (l (a) (string-suffix? a path)) match)))
                    ((procedure? match) match)
                    ((boolean? match) (const match))
                    (else (raise (q invalid-file-handler-match))))
                  (swp-file-handler-last a) (swp-file-handler-path-f a) (swp-file-handler-f a)))))
          pass))
      a))

  (define (unzip3* a) "like unzip3 but allows input lists to be shorter"
    (list (fold-right (l (a result) (if (< 0 (length a)) (pair (first a) result) result)) null a)
      (fold-right (l (a result) (if (< 1 (length a)) (pair (second a) result) result)) null a)
      (fold-right (l (a result) (if (< 2 (length a)) (pair (third a) result) result)) null a)))

  (define-as swp-default-file-handlers swp-file-handlers-normalise
    ; result is a list of lists of file handlers, one list per pass.
    ; path-f must not modify the target directory.
    ; path-f is to collect target-paths and check for conflicts.
    ; handler-f can be false for a no-op
    (list
      ; pass 1
      (swp-file-handler-new "thumbnails" (list ".png" ".jpeg" ".jpg")
        #f
        (l (env target-path)
          (let*
            ( (thumbnails-directory-name
                (alist-ref-q (swp-env-config env) thumbnails-directory-name))
              (target-dir
                (let (target-dir (dirname target-path))
                  (string-append
                    (if (string-equal? "." target-dir) ""
                      (ensure-trailing-slash (dirname target-path)))
                    thumbnails-directory-name))))
            (string-append (ensure-trailing-slash target-dir) (basename target-path))))
        (l (env path target-path) "create thumbnails for images"
          ((swp-env-create-thumbnail env) path target-path)))
      (swp-file-handler-new "sources" (list ".md" ".plcss" ".shtml" ".sjs" ".sxml" ".md")
        #f
        (l (env target-path)
          (let*
            ( (sources-directory-name (alist-ref-q (swp-env-config env) sources-directory-name))
              (target-dir
                (let (target-dir (dirname target-path))
                  (string-append
                    (if (string-equal? "." target-dir) ""
                      (ensure-trailing-slash (dirname target-path)))
                    sources-directory-name))))
            (string-append (ensure-trailing-slash target-dir) (basename target-path))))
        (l (env path target-path)
          "for files that are compiled, include the source in a subdirectory next to it.
        for example the source t.md will create t.html and sources/t.md"
          ((swp-env-copy-file env) path target-path)))
      (swp-file-handler-new "plcss" ".plcss"
        #t (l (env target-path) (string-append (string-drop-suffix ".plcss" target-path) ".css"))
        (l (env path target-path)
          (let (data (file->datums path))
            (call-with-output-file target-path (l (port) (plcss->css data port))))))
      (swp-file-handler-new "sxml" ".sxml"
        #t (l (env target-path) (string-append (string-drop-suffix ".sxml" target-path) ".xml"))
        (l (env path target-path)
          (let (data (file->datums path))
            (call-with-output-file target-path (l (port) (sxml->xml data port))))))
      (swp-file-handler-new "shtml" ".shtml"
        #t (l (env target-path) (string-append (string-drop-suffix ".shtml" target-path) ".html"))
        (l (env path target-path)
          (let (data (file->datums path))
            (call-with-output-file target-path
              (l (port) (display "<!doctype html>" port) (sxml->xml data port))))))
      (swp-file-handler-new "ignore" ".md" #t (const #f) #f)
      (swp-file-handler-new "copy" #t
        #f (l (env target-path) target-path)
        (l (env path target-path) ((swp-env-copy-file env) path target-path))))
    (list
      ; pass 2
      (swp-file-handler-new "markdown" ".md"
        #t (l (env target-path) (string-append (string-drop-suffix ".md" target-path) ".html"))
        (l (env path target-path)
          (let*
            ( (config (swp-env-config env)) (layout (alist-ref-q config shtml-layout))
              (shtml
                (swp-md->shtml (alist-ref-q config md-scm-env) path
                  (swp-env-swp-target-directory env)))
              (title (swp-md-get-title path)) (links (alist-ref-q config top-bar-links))
              (mtime (stat:mtime (stat path)))
              (shtml
                (layout shtml #:title
                  (or (and title (first title))
                    (string-drop-suffix-if-exists ".html" (basename path)))
                  #:links links #:mtime mtime)))
            (call-with-output-file target-path
              (l (port) (display "<!doctype html>" port) (sxml->xml shtml port))))))))

  (define (swp-atom-feed-task env)
    (let (target-dir (swp-env-swp-target-directory env))
      (call-with-output-file (string-append target-dir "feed.xml")
        (l (port) (swp-atom-feed target-dir port #:title "feed")))))

  (define (swp-recent-changes-task env)
    (let (target-dir (dirname (swp-env-swp-directory env)))
      (call-with-output-file (string-append target-dir "/recent.md")
        (l (port) (swp-recent-changes target-dir port)))))

  (define-as swp-default-config alist-q
    md-scm-env (swp-md-scm-env-new)
    top-bar-links (list (list "/" "start") (list "/recent.html" "recent"))
    shtml-layout shtml-layout
    file-handlers swp-default-file-handlers
    hooks
    (alist-q before-upload null before-compile (list swp-recent-changes-task) after-compile null)
    sources-directory-name "sources"
    thumbnails-directory-name "thumbnails"
    use-hardlinks #t thumbnail-size 100 rsync-arguments (list "--progress"))

  (define (call-hook env name)
    (every (l (a) (a env)) (or (alists-ref (swp-env-config env) (q hooks) name) null)))

  (define (swp-compile env)
    (and-let*
      ( (source-dir (swp-env-directory env)) (target-dir (swp-env-swp-target-directory env))
        (file-handlers (swp-env-file-handlers env))
        (path->handlers
          (l (path) "match one or more handlers"
            (map
              (l (pass-handlers)
                (let loop ((rest pass-handlers))
                  (if (null? rest) null
                    (let (a (first rest))
                      (if ((swp-file-handler-match a) path)
                        (if (swp-file-handler-last a) (if (swp-file-handler-f a) (list a) null)
                          (if (swp-file-handler-f a) (pair a (loop (tail rest))) (loop (tail rest))))
                        (loop (tail rest)))))))
              file-handlers)))
        (path->target-paths
          (l (relative-path handlers)
            (map
              (l (handlers)
                (map (l (handler) ((swp-file-handler-path-f handler) env relative-path)) handlers))
              handlers)))
        (paths-and-handlers
          (swp-file-system-fold (remove-trailing-slash source-dir)
            (list (remove-trailing-slash (swp-env-swp-directory env))) null
            (l (path stat-info result)
              (or
                (and-let*
                  ( (handlers (path->handlers path))
                    (relative-path (string-drop-prefix source-dir path))
                    (target-paths (path->target-paths relative-path handlers)))
                  (pair
                    (map
                      (l (target-paths handlers)
                        (map (l (a b) (list path (and a (string-append target-dir a)) b))
                          target-paths handlers))
                      target-paths handlers)
                    result))
                result))))
        (paths-and-handlers (map-apply append (unzip3* paths-and-handlers)))
        (paths-and-handlers
          (let*
            ( (target-paths-flat
                (append-map (l (a) (filter-map (l (a) (list-ref a 1)) a)) paths-and-handlers))
              (target-paths-duplicates (duplicates target-paths-flat)))
            (if (null? target-paths-duplicates) paths-and-handlers
              (raise (list (q conflicting-target-paths) (string-join target-paths-duplicates ", ")))))))
      ; paths-and-handlers: ((handler ...):pass ...)
      (and (call-hook env (q before-compile))
        (every
          (l (a)
            (every
              (l (a)
                (apply
                  (l (path target-path handler)
                    (let (exists (and target-path (file-exists? target-path)))
                      (or
                        (and exists (>= (stat:mtime (stat target-path)) (stat:mtime (stat path))))
                        (begin (display-line target-path) (if exists (delete-file target-path))
                          (and
                            (or (not target-path)
                              (ensure-directory-structure (dirname target-path)))
                            ((swp-file-handler-f handler) env path target-path))))))
                  a))
              a))
          paths-and-handlers)
        (call-hook env (q after-compile)))))

  (define (swp-upload env remotes)
    (let*
      ( (config (swp-env-config env)) (rsync-arguments (alist-ref-q config rsync-arguments null))
        (remotes (map string->symbol remotes)) (remotes-config (alist-ref-q config remotes null))
        (configs
          (map
            (l (name)
              (or (assoc name remotes-config)
                (raise (list (q remote-config-not-found) (q name) name))))
            remotes))
        (source (swp-env-swp-target-directory env)))
      (and (call-hook env (q before-upload))
        (every
          (l (a)
            (apply execute "rsync" (append rsync-arguments (list "--recursive" source (tail a)))))
          configs)
        (call-hook env (q after-upload)))))

  (define (swp-clean env)
    (let (dir (swp-env-swp-target-directory env))
      (or (not (file-exists? dir)) (and (swp-delete-file-recursively dir) (mkdir dir)))))

  (define (swp-compile-and-upload env remotes) (and (swp-compile env) (swp-upload env remotes)))

  (define (swp-init directory)
    (let (swp-directory (string-append directory "/.swp/"))
      (if (not (file-exists? swp-directory))
        (begin (mkdir swp-directory) (close (open (string-append swp-directory "config") O_CREAT))
          (mkdir (string-append swp-directory "compiled"))))))

  (define (swp-remove-disabled-file-handlers config a)
    (fold (l (proc a) (proc a)) a
      (list
        (l (a)
          (if (alist-ref-q config sources-directory-name) a
            (pair (remove (l (a) (string-equal? "sources" (swp-file-handler-name a))) (first a))
              (tail a))))
        (l (a)
          (if (alist-ref-q config thumbnails-directory-name) a
            (pair (remove (l (a) (string-equal? "thumbnails" (swp-file-handler-name a))) (first a))
              (tail a)))))))

  (define (swp-env-open directory config) "directory paths in env must end with a slash"
    (let*
      ( (directory (ensure-trailing-slash (realpath* directory)))
        (swp-directory (string-append directory ".swp/")))
      (and (or (file-exists? swp-directory) (raise (q not-a-sph-web-publish-directory)))
        (let*
          ( (config
              (alist-merge swp-default-config
                (alist-merge config (swp-config-read (string-append swp-directory "config")))))
            (copy-file (if (alist-ref-q config use-hardlinks) link copy-file))
            (create-thumbnail (swp-create-thumbnail-proc (alist-ref-q config thumbnail-size)))
            (file-handlers
              (let (a (alist-ref-q config file-handlers))
                (if (eq? swp-default-file-handlers a) (swp-remove-disabled-file-handlers config a)
                  (swp-file-handlers-normalise a))))
            (config (alist-delete (q file-handlers) config)))
          (vector (q swp-env) directory
            swp-directory (string-append swp-directory "compiled/")
            config file-handlers copy-file create-thumbnail)))))

  (define (swp-cli-command-handler config) "(string ...) list ->"
    (l (command options)
      (let*
        ( (directory (or (alist-ref-q options directory) (getcwd)))
          (remotes (or (alist-ref-q options remote) (q ("default"))))
          (env-open (nullary (swp-env-open directory config))))
        (string-case (first command) ("clean" (swp-clean (env-open)))
          ("compile" (swp-compile (env-open)))
          ("compile-and-upload" (swp-compile-and-upload (env-open) remotes))
          ("init" (swp-init directory)) ("upload" (swp-upload (env-open) remotes))
          (else (display-line "invalid-command") #f)))))

  (define (swp-cli-new config)
    "list -> procedure:cli
     return a procedure that when called parses command line arguments and
     executes swp commands using config"
    (cli-create #:null-arguments (list "--help")
      #:about
      "sph-web-publish management utility. static site generator. license gpl3+. http://sph.mn"
      #:options (list-q ((command argument ...)))
      #:command-options (list-q (directory #:value-required? #t))
      #:command-handler
      (swp-cli-command-handler
        (if (eq? swp-default-config config) config (alist-merge swp-default-config config)))
      #:commands
      (list-qq (("clean") #:description "remove compiled files")
        (("compile") #:description "update all files under data/")
        (("compile-and-upload") ((remote ...)) #:description "compile and on success upload")
        ( ("init") #:description
          "initialise the current directory for sph-web-publish. creates a .swp directory")
        (("upload") ((remote ...)) #:description "send files to the configured server"))))

  (define swp-default-cli (swp-cli-new swp-default-config)))
