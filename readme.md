static site generator

*work in progress as of 2018-10*

# workflow
* choose a directory and initialise it with `sph-web-publish init`
* add your files to the directory: markdown, html, sxml, css, plcss, javascript, sescript, directories, images or anything else
* use markdown files for content pages, and use special inline expressions to create automatically generated link lists to content, include content and more
* call `sph-web-publish compile`. some file types will be pre-processed. markdown becomes html, plcss becomes css, etc
* call `sph-web-publish upload {remote}` after having configured a target directory like with `sph-web-publish remote add ssh://myserver/srv/http`

# features
* create site navigation and content with special expressions in markdown
* thumbnails for images are generated
* source files are by default included in a separate directory next to compiled files
* atom feed

# configuration
edit {site-directory}/.sph-web-publish/config

example configuration file content with all options
```
feed-rights "creative commons by-nc"
use-hardlinks #t
sources-directory-name "sources"
thumbnails-directory-name "thumbnails"
thumbnail-size 100
```

# internals
* core modules are installed in the guile module path, the sph-web-publish executable is installed in shell load path
* a `.sph-web-publish` is added to site directories on initialisation. it contains a config file that can be edited and compiled data

the format is scheme, as if the content was specified in a quasiquoted list. each pair of expressions is key and value

# usage
see `./exe/sph-web-publish --help`
