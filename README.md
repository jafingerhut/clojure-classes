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
it for multiple versions of Clojure source code.  You must give it:

+ the root directory of a local git clone of the Clojure source code.
+ the directory where you want output files to be created

The script will make changes to your git clone of the Clojure source
code, using `git checkout` commands.  It does this to change the
Clojure source code to different Clojure release versions.

```bash
$ ./generate/gen.sh /home/andy/clj/clojure generate/
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


## Tweaking the Graphviz file further

There are many, many options to Graphviz that modify how graphs look.
I will not even attempt to list them all here.  Here is the official
Graphviz documentation on node, edge, and graph attributes:
http://graphviz.org/doc/info/attrs.html

The default way of drawing edges is as straight lines when these would
not go over other nodes, or curved splines in order to go around
nodes.  Other choices are described here:
http://graphviz.org/doc/info/attrs.html#d:splines

Adding one of the following lines near the beginning of the `.dot`
output file, after the `digraph {` line, changes the shape of edges:

+ `splines=curved;` - curved lines that curve only "one way", but can
  overlap other nodes.

+ `splines=polyline;` - straight line segments that can have many
  different angles.  The disadvantage is that they can overlap each
  other quite a bit, which can make it confusing which ones are which
  in the overlapping portions.

+ `splines=ortho;` - straight line segments where every segment is
  either horizontal or vertical.  This one does a good job at avoiding
  overlapping lines.  They can be packed fairly close to each other,
  if there are many of them and the nodes are not far apart.
