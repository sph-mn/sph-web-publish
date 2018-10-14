static site generator

sph-web-publish is currently used for a personal website with some specialised code (custom css, sxml rewriting, extra dependencies and similar) but could be made more generic

# workflow
* choose a directory and initialise it with `sph-web-publish init`
* add your files to the directory: markdown, html, sxml, css, plcss, javascript, sescript, directories, images and anything else
* use markdown files for content pages and special inline expressions to create automatically generated link lists to content, include content and more
* call `sph-web-publish compile`. some file types will be pre-processed. markdown becomes html, plcss becomes css, etc
* call `sph-web-publish upload {remote}` after having configured a target directory like with `sph-web-publish remote add ssh://myserver/srv/http`

# features
* create site navigation and content with special expressions in markdown
* thumbnails for images are generated
* source files are by default included in a separate directory next to compiled files

# example markdown
```
# test markdown source

    #(short-description "this is a short description")
    #links (#:sort #t)
      ("http://example.com" "example.com" "this is a link description")
      ("http://example.com" "example.com" "this is a link description")
      ("http://example.com" "example.com" "this is a link description")

[link](http://sph.mn)

# module

    #(library-documentation (sph vector))
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
  compile-and-upload :: [remote ...]
  init
  upload :: [remote ...]
```

# dependencies
* [guile](https://www.gnu.org/software/guile/)
* [guile-commonmark](https://github.com/OrangeShark/guile-commonmark)
* [sph-lib](https://github.com/sph-mn/sph-lib)

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

# configuration
edit `{site-directory}/.swp/config`

example configuration file content with all options
```
use-hardlinks #t
sources-directory-name "sources"
thumbnails-directory-name "thumbnails"
thumbnail-size 100
remotes (
  default "hostname:/tmp/swp-test"
)
```

the format is scheme, as if the content was specified in a quasiquoted list. each pair of expressions is key and value

# current limitations
* because of personalisation, the code additionally depends on [sescript](https://github.com/sph-mn/sescript)
* the markdown html layout, the enclosing html, is currently not configurable and defined in `modules/sph/web/publish/shtml.scm`
* handlers are currently not configurable and defined in exe/sph-web-publish

# planned features
* atom feed generation

# internals
* a directory named `.swp` is added to site directories on initialisation. it contains a config file that can be edited, and eventually compiled data
* target files are not updated unless the source is newer. if the source is newer, the target path is first deleted
* by default, files that are not processed are linked to the target directory
* files can be processed via a list of handler procedures. a source path can match multiple handlers until one matching handler has the last flag set. there can be catch-all handlers
* the program stops with an error message if multiple handlers would create a target file with the same path (for example t.xml and t.sxml would otherwise both become t.xml in the target directory)

## markdown processing
* itml expressions are parsed when at least one itml expression appears at the beginning of a markdown code block. it does not matter which kind of code block - inline or fenced
* itml expressions have one of the following initial patterns: `#identifier ##identifier ###identifier #(identifier ##(identifier`

# license
gpl3+