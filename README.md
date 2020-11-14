# Introduction

The code in `src/net/n01se/clojure_classes.clj` uses Clojure, the
Clojure .java sources, and the Graphviz `dot` program to produce files
`graph-<CLOJURE_VERSION>.dot` and `log-<CLOJURE_VERSION>.txt`.

You must install [GraphViz](http://www.graphviz.org) in order for the
generation of figures to work -- see its home page for downloads and
installation instructions if the following do not work:

* Ubuntu Linux - `sudo apt-get install graphviz`
* Mac OS X
  * If you use Homebrew: `brew install graphviz`
  * If you use MacPorts: `sudo port install graphviz`

I run it like:
```bash
$ clojure -Sdeps '{:deps {org.clojure/clojure {:mvn/version "1.10.1"}}}' -m net.n01se.clojure-classes /Users/andy/clj/clojure generate/
```

There is a script in the `generate` directory that you can use to run
it for multiple versions of Clojure source code:
```bash
$ ./generate/gen.sh
```

You can produce svg, pdf, png, and other graphical file formats from
the dot files using commands like these:

```bash
$ dot -Tsvg graph.dot > graph.svg
$ dot -Tpdf graph.dot > graph.pdf
$ dot -Tpng graph.dot > graph.png
```

## Original notes from chouser on further hand-editing that he did

Adjustments to line thickness in `graph.svg` were made by:
```bash
$ sed -b 's/stroke:#\w*;/&stroke-width:1.5;/g' graph.svg
```

Inkscape is used to add the legend and make other minor manual
adjustments seen in `graph-w-legend.svg` and `graph-w-legend.png`.

The `graph-w-legend.png` has also been run through gimp to reduce the
number of colors and therefore the file size.

--Chouser, Feb 2009
[updated Feb 2011]
