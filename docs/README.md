## Build documentation

cl-docweaver is needed for building the documentation.

Just run `make`.

## Export tutorial to TexInfo

The basic-tutorial.org is in Org format. If it is modified, then it should be exported to TexInfo format and rebuild the documentation.

In emacs, load the Org TexInfo exporter with `(require 'ox-texinfo)`, then use `C-c C-e` on the basic-tutorial.org to export it (with `body only` option enabled!).

Finally, run `make clean` and `make`.
