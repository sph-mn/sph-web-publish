static site generator

sph-web-publish currently has some personalised code (custom css, sxml rewriting, extra dependencies and similar) but could be made more generic

# workflow
* choose a directory and initialise it with `sph-web-publish init`
* add your files to the directory: markdown, html, sxml, css, plcss, javascript, sescript, directories, images and anything else
* use markdown files for content pages and special inline expressions to create automatically generated link lists to content, include content and more
* call `sph-web-publish compile`. some file types will be pre-processed. markdown becomes html, plcss becomes css, etc
* call `sph-web-publish upload {remote}` after having added a remote to the config file

# features
* create site navigation and content with special expressions in markdown
* thumbnails for images are generated
* source files are by default included in a separate directory next to compiled files

# example markdown
```
# test markdown source

    %scm + 1 2 3
    %scm table
      ("http://example.com" "example.com" "this is a description")
      ("http://example.com" "example.com" "this is a description")
      ("http://example.com" "example.com" "this is a description")

[link](http://sph.mn)

# module

    %scm library-documentation (sph vector)
```

# command line program
```
# sph-web-publish --help
parameters
  options ... command argument ...
options
  --about | -a
  --directory=value
  --help | -h
  --interface
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
* rsync (for upload)

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
* when using link-files with html files that where compiled from markdown, title and description are read from the markdown file. title is read only from the first line when it starts with a level one heading. description is only read from the second line when it follows a title. when the html file is not compiled from markdown, the title is read from the content of the title tag if it exists
* scheme expressions are only parsed when the `%scm ` prefix appears right at the beginning of a markdown code block. it does not matter which kind of code block - inline or fenced
* note that multiple four spaces indented code blocks that follow another with only whitespace inbetween get joined by the markdown parser as if they were one code block
* multiple scheme expressions can occur in one code block as long as all expressions begin with `%scm ` as the first characters of a code block line
* the prefix can be escaped with `%%scm`, in which case the unparsed string `%scm` remains
* only a limited set of bindings is available and expressions are evaluated with guiles `eval-in-sandbox`

besides `core-bindings`, `string-bindings`, `symbol-bindings`, `list-bindings`, `number-bindings` the following features are available

### link-files :: paths ...
create a list of links to compiled files. file globbing can be used with `*` and `**` as for `filesystem-glob` of sph-lib `(sph filesystem)`

### include-files :: paths ...
like link-files but includes files via an html object tag

### library-documentation :: (module name)
list formatted binding names and docstrings of the specified library.
a description is displayed if the library exports a variable named like the parts of the library name joined with `description` at the end, for example `module-name-description`.
if docstrings start with a type signature, they signature is parsed and displayed with formatting, for example `string boolean -> any`

### library-short-description
gets the first line of a description as for library-documentation

# upload
uses rsync which by default uses ssh

# configuration
edit `{site-directory}/.swp/config`

example configuration file content with all options
```
use-hardlinks #t
sources-directory-name "sources"
thumbnails-directory-name "thumbnails"
thumbnail-size 100
remotes
( default "hostname:/tmp/swp-test"
  local "/tmp/swp-test")
```

the format is scheme, as if the content was specified in a quasiquoted list. each pair of expressions is key and value

# current limitations
* because of personalisation, the code currently additionally depends on [sescript](https://github.com/sph-mn/sescript)
* the markdown html layout, the enclosing html, is currently not configurable and defined in `modules/sph/web/publish/shtml.scm`
* source file handlers are currently not configurable and defined in `exe/sph-web-publish`
* atom feed generation is not yet implemented

# internals
* a directory named `.swp` is added to site directories on initialisation. it contains a config file that can be edited, and eventually compiled data
* target files are not updated unless the source is newer. if the source is newer, the target path is first deleted
* by default, files that are not processed are linked to the target directory
* the program stops with an error message if multiple handlers would create a target file with the same path (for example t.xml and t.sxml would otherwise both become t.xml in the target directory)
* files can be processed via a list of handler procedures. a source path can match multiple handlers until one matching handler has the last flag set. there can be catch-all handlers
* source file handlers are matched by file name suffix

the following file types are currently compiled:
* .md -> html
* .shtml -> html
* .sxml -> xml
* .plcss -> css
* .sjs -> js

# license
gpl3+