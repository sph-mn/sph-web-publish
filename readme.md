static site generator

# license
gpl3+

# workflow
* choose a directory and initialise it with `sph-web-publish init`
* add your files to the directory: markdown, html, sxml, css, plcss, javascript, directories, images and anything else
* use markdown files for content pages and special inline expressions to create automatically generated link lists to content, include content and more
* call `sph-web-publish compile`. some file types will be pre-processed. markdown becomes html, plcss becomes css, etc
* call `sph-web-publish upload {remote}` after having added a remote to the config file

# features
* manage website content in a directory and upload it to a server
* create site navigation and content with special expressions in markdown
* custom markdown layout, hooks, markdown inline scm expressions and more
* default handlers for thumbnails, copying sources of compiled files and an atom feed for most recently changed files

# example markdown
```
# test markdown source

    %scm + 1 2 3
    %scm link-files "test.html" "guides/*html"

[link](http://sph.mn)
```

# command line program
```
# sph-web-publish --help
parameters
  options ... command argument ...
options
  --about | -a
  --help | -h
  --interface
options shared by all commands
  --directory=value
commands
  clean
  compile
  compile-and-upload :: remote ...
  init
  upload :: remote ...
```

# dependencies
* [guile](https://www.gnu.org/software/guile/)
* [guile-commonmark](https://github.com/OrangeShark/guile-commonmark)
* [sph-lib](https://github.com/sph-mn/sph-lib)
* optional
  * rsync (for upload)
  * graphicsmagick (for thumbnails)

# installation
* copy or symlink `modules/*` into a directory in guiles load path. for example `cp -rt /usr/share/guile/site modules/*`
* copy or symlink `exe/sph-web-publish` into a directory listed in the `$PATH` environment variable. for example `cp -rt /usr/bin exe/sph-web-publish`

# usage
create a new site directory
```
mkdir mysite
cd mysite
sph-web-publish init
```

use a site directory
```
touch myfile
sph-web-publish compile
```

# compile
* compiled files are saved in `{site-directory}/.swp/compiled`
* `sph-web-publish clean` deletes all the compiled files

## markdown processing
* when using `link-files` with html files that where compiled from markdown then title and description are read from the markdown file. title is read only from the first line when it starts with a level one heading. description is only read from the second line when it follows a title. when the html file is not compiled from markdown, the title is read from the content of the title tag if it exists
* the resulting html5 document structure will be like the following sxml: `((section heading (div subsection/content ...)) ...)`
* scheme expressions are only parsed when the `%scm ` prefix appears right at the beginning of a markdown code block. it does not matter which kind of code block - inline or fenced
* note that multiple four spaces indented code blocks that follow another with only whitespace inbetween get joined by the markdown parser as if they were one code block
* multiple scheme expressions can occur in one code block as long as all expressions begin with `%scm ` as the first characters of a code block line
* the prefix can be escaped with `%%scm`, in which case the unparsed string `%scm` remains
* only a limited set of bindings is available and expressions are evaluated with guiles `eval-in-sandbox`

besides `core-bindings`, `string-bindings`, `symbol-bindings`, `list-bindings` and `number-bindings` the following features are available

### link-files :: paths ...
create a list of links to compiled files. file globbing can be used with `*` and `**` as for `filesystem-glob` of sph-lib `(sph filesystem)`

* `*` matches zero or more of any character in a file name.
* `?` matches one of any character in a file name.
* `**` skips any sub directories to match the rest of the path. at the end of a path it is the same as `**/.*` including `.*`
* `**n` where `n` is an integer. like `**` but skips directories at most n subdirectories deep.

### include-files :: paths ...
like link-files but includes files via an html object tag

# upload
uses rsync which by default uses ssh

# customisation
## by configuration file
edit `{site-directory}/.swp/config`

example configuration file content with all possible options
```
sources-directory-name "sources"
thumbnails-directory-name "thumbnails"
use-hardlinks #t
thumbnail-size 100
remotes
  default "sph-server:/tmp/swp-test"
  local "/tmp/swp-test"
```

the format is scheme expressions for key and value alternatingly, with indent of two spaces per step for nesting

* sources-directory-name: by default, a handler is enabled which copies the sources of compiled files into a directory next to the compiled file. for example t.md would become t.html and sources/t.md. if this option is set to a string then it is the name of the directory, if it is false then the source file copying handler is disabled
* thumbnails-directory-name: similar to source files, thumbnails are saved in a directory next to image files. if false, thumbnail creation is disabled
* use-hardlinks: if true then hardlinks are used to build the temporary upload directory, otherwise source files are eventually fully copied
* thumbnail-size: size of the longest side in pixels
* remotes: a list of remote name and target path associations

## by cli composition
a sph-web-publish command line interface with further customised configuration can be created

example
```
(import (ice-9 sandbox) (sph web publish) (sph web publish shtml))

(define sph-info-md-scm-env
  (make-sandbox-module
    (append core-bindings string-bindings
      symbol-bindings list-bindings
      number-bindings
      (quote
        ( ( (sph web publish markdown scm-env) link-files include-files)
          ( (sph-info markdown-scm-env) sph-info-audio-playlist sph-info-software-list
            sph-info-test-io sph-info-software-list-grouped))))))

(define (sph-info-shtml-layout a . b)
  "content #:title string #:links list #:mtime integer -> sxml
   this layout function extends shtml-layout from (sph web publish shtml)"
  (apply shtml-layout a #:css (list "/css/sph.css") b))

(define sph-info-swp-cli
  (swp-cli-new
    (list
      (cons (quote shtml-layout) sph-info-shtml-layout)
      (cons (quote md-scm-env) sph-info-md-scm-env))))
```

the cli can then be used in a file that when executed will parse the arguments it was called with accordingly
```
#!/usr/bin/guile
!#

(sph-info-swp-cli)
```

all options that are possible in the configuration file plus the following can be set in an association list passed to `swp-cli-new`
```
(list
  (cons (quote md-scm-env) sph-info-md-scm-env)
  (cons (quote top-bar-links) (list (list "/" "start") (list "/feed.xml" "feed")))
  (cons (quote shtml-layout) sph-info-shtml-layout)
  (cons (quote file-handlers) swp-default-file-handlers)
  (cons
    (quote hooks)
    (list
      (pair (quote before-upload) null)
      (pair (quote before-compile) null)
      (pair (quote after-compile) (list swp-atom-feed-task)))))
```

* md-scm-env: a module whose exports will be available in %scm expressions in markdown
* top-bar-links: the default layout creates a small navigation bar at the top to get back to the start page. false to disable
* shtml-layout: a procedure that creates the sxml around the compiled markdown content
* file-handlers: a list of file handlers as described below
* hooks: lists of procedure with the signature `swp-env -> boolean` that are called at certain times. if their result is false then processing is aborted

for unset top-level options the default will be used

### file-handlers
file handlers is a list of lists, one list for each pass. each pass processes all source files. there can be up to three passes which can be used for example for handlers that need all files from previous passes to be already compiled, like for the markdown handler and file linklists

file-handlers: ((file-handler ...):pass ...)

a file-handler is a vector best created with `swp-file-handler-new :: name match last path-f f -> vector`.

* name: a custom string
* match: a string to match filename extensions, a list of strings to match multiple filename extensions, true to match all files or a procedure `string:path -> boolean`
* last: true if no more handlers of the current pass should be used, false if more handlers should possibly match
* path-f: a procedure for generating the full path a handler will write to. used to check for conflicts and will be passed to the handler function. `swp-env string:relative-path -> false/string:target-path`
* f: a file handler procedure `swp-env source-path target-path -> boolean`. all paths are full paths. if result is false, all processing is aborted

for example, here the default handler for sxml
```
(swp-file-handler-new "sxml" ".sxml" #t
  (lambda (env target-path)
    (string-append (string-drop-suffix ".sxml" target-path) ".xml"))
  (lambda (env path target-path)
    (let ((data (file->datums path)))
      (call-with-output-file target-path (lambda (port) (sxml->xml data port))))))
```

### markdown scm expressions
procedures to be used in inline scheme expressions receive the path to the temporary compile target directory as the first argument and return sxml

for example
```
(define (table directory . cells)
  (create-table-shtml cells))
```

syntax is also supported. for example defined by
```
(define-syntax-rule (table directory cell ...)
  (create-table-shtml (quote (cell ...))))
```

# internals
* a directory named `.swp` is added to site directories on initialisation. it contains a config file that can be edited, and eventually compiled data
* target files are not updated unless the source is newer. if the source is newer, the target path is first deleted
* by default, files that are not processed are linked to the target directory
* the program stops with an error message if multiple handlers would create a target file with the same path (for example t.xml and t.sxml would otherwise both become t.xml in the target directory)
* files can be processed via a list of handler procedures. a source path can match multiple handlers until one matching handler has the last flag set. there can be catch-all handlers

the default configuration contains handlers to automatically compile files with the following suffixes
* .md -> html
* .shtml -> html
* .sxml -> xml
* .plcss -> css

# history
sph-web-publish was created after trying to reduce the complexity of a dynamic web application that was the basis of a personal website. it is basically the simplified successor of `sph-cms`

# also check out
* [haunt](https://dthompson.us/projects/haunt.html)
* [tekuti](https://wingolog.org/projects/tekuti/)