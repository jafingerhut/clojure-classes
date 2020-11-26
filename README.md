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


## Usage from a REPL

```clojure
user=> (require '[net.n01se.clojure-classes :as cc])
nil

;; You may prefer the look of the drawing by leaving out the key
;; :splines-type, or by including it but changing its value to
;; "polyline".

user=> (def opts {:create-window true :splines-type "ortho" :class-filter cc/not-root-class})
#'user/opts

user=> (def dotstr1 (cc/make-dot-graph [clojure.core.Vec] opts))
#'user/dotstr1
```

A window should pop up with a drawing of the class graph.
`make-dot-graph` also returns a string that can be written to a file,
and used as input to the Graphviz `dot` program.

You may give a sequence of classes as the first argument of
`make-dot-graph`.  If you do, all of their superclasses, and all
interfaces they implement, will be found, and their superclasses,
etc. transitively.

```
user=> (def class-list (map class [[1] (vector-of :long 1)]))
user=> (def dotstr1 (cc/make-dot-graph class-list opts))

;; Write the string to a file named "vector-classes.dot"

user=> (spit "vector-classes.dot" dotstr1)
```

To get a list of all classes in the package `clojure.lang` that are
part of the Clojure implementation, which is the list of classes that
the command line invocation that has the word `all-clojure-classes`
described in the next section uses, you can call the function
`all-clojure-classes`, passing it a string that is the root directory
of a copy of the Clojure source code, e.g. if you were in the
directory `/Users/andy/clj` when you entered this command:

```bash
$ cd /home/andy
$ git clone https://github.com/clojure/clojure
```

then the root directory is `"/Users/andy/clj/clojure"`.

```clojure
user=> (def clj-src-dir "/Users/andy/clj/clojure")
user=> (def clj-class-list (cc/all-clojure-classes clj-src-dir {}))
user=> (def dotstr1 (cc/make-dot-graph clj-class-list opts))
user=> (spit "all-clj-classes.dot" dotstr1)
```

## Usage from command line via Clojure CLI tools

I run it as shown below to generate a graph for all (or at least most)
Clojure classes.  Note that the version of Clojure you run it with
specified via `-Sdeps`, and the version of the Clojure source code in
the last argument you give it, should match, otherwise the output is
likely to be wrong in unpredictable ways.

```bash
$ clojure -Sdeps '{:deps {org.clojure/clojure {:mvn/version "1.10.1"}}}' -m net.n01se.clojure-classes generate/ all-clojure-classes /Users/andy/clj/clojure 
```

Since the graph is quite large for this set of Clojure classes, you
can also specify a list of starting class names on the command line,
and it will start from those classes, searching for their superclasses
and the Java interfaces that they implement, recursively, and ignore
any other Clojure classes.

```bash
$ clojure -Sdeps '{:deps {org.clojure/clojure {:mvn/version "1.10.1"}}}' -m net.n01se.clojure-classes generate/ classes clojure.lang.PersistentVector
```

There is a script in the `generate` directory that you can use to run
it for multiple versions of Clojure source code.  You must give it:

+ the directory where you want output files to be created
+ the root directory of a local git clone of the Clojure source code.

WARNING: The script will make changes to your git clone of the Clojure
source code, using `git checkout` commands.  It does this to change
the Clojure source code to different Clojure release versions.

```bash
$ ./generate/gen.sh generate/ /Users/andy/clj/clojure
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

There are many, many options to Graphviz that modify how graphs are
drawn.  I will not even attempt to list all of those options here.
Here is the official Graphviz documentation on node, edge, and graph
attributes: http://graphviz.org/doc/info/attrs.html

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
