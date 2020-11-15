; clojure-classes.clj - produces graphviz dot graph for Clojure Java classes
;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.n01se.clojure-classes
  (:use [clojure.java.shell :only (sh)])
  (:require [clojure.string :as str])
  (:import (javax.swing JFrame JLabel JScrollPane ImageIcon)
           (clojure.lang PersistentQueue)))

(def log-lines (atom []))

(defn log [line-str]
  (swap! log-lines conj line-str))

(defn write-log [log-fname]
  (spit log-fname (str/join "\n" @log-lines)))

(defn clojure-java-src-dir [clojure-local-root]
  (str clojure-local-root "/src/jvm/clojure/lang/"))

(defn java-source-file-base-names [clojure-java-src]
  (filter identity
          (for [file (.listFiles (java.io.File. clojure-java-src))]
            (let [[base-fname ext] (.split (.getName file) "\\.")]
              (when (= ext "java")
                base-fname)))))

(defn clojure-lang-classes [java-source-file-base-names]
  (for [base-fname java-source-file-base-names]
    (Class/forName (str "clojure.lang." base-fname))))

(defmacro str-for [& for-stuff]
  `(apply str (for ~@for-stuff)))

(def colors ["#d70000" "#d7009e" "#b300d7" "#5a00d7" "#0061d7" "#00d0d7"
             "#00d764" "#76d700" "#d78100"])
; Some lighter colors:
; "#ff817f" "#ff7fea" "#b47fff" "#7fa5ff" "#7ffffb" "#a8ff7f" "#ffd97f"

;; Functions in src/clj/clojure/core.clj as of Clojure 1.10.1 that
;; return (instance? some-class x) are all included in the map 'preds'
;; below.

;; g means that the predicate occurs at least once in the generated graph
;; b means that it is an interface that is in the map 'badges'

(def preds '{
             ;; classes and interfaces in clojure.lang package
             Associative associative?,       ;; g
             Counted counted?,               ;;  b
             Delay delay?,                   ;; g
             Fn fn?,                         ;; g
             IChunkedSeq chunked-seq?,       ;; g
             IFn ifn?,                       ;; g
             IPersistentCollection coll?,    ;; g
             IPersistentList list?,          ;; g
             IPersistentMap map?,            ;; g
             IPersistentSet set?,            ;; g
             IPersistentVector vector?,      ;; g
             ISeq seq?,                      ;; g
             Indexed indexed?,               ;; g
             Keyword keyword?,               ;; g
             Ratio ratio?,                   ;; g
             ReaderConditional reader-conditional?, ;; g
             Reversible reversible?,         ;;  b
             Sequential sequential?,         ;; g
             Sorted sorted?,                 ;; g
             Symbol symbol?,                 ;; g
             TaggedLiteral tagged-literal?,  ;; g
             Var var?,                       ;; g
             Volatile volatile?,             ;; g

             ;; classes and interfaces in java.lang or other java.* package
             BigDecimal decimal?,            ;;    package java.math
             Boolean boolean?,               ;;    package java.lang
             Character char?,                ;;    package java.lang
             Class class?,                   ;;    package java.lang
             Double double?,                 ;;    package java.lang
             Future future?,                 ;;    package java.util.concurrent
             Map$Entry map-entry?            ;;    package java.util
             Number number?,                 ;;    package java.lang
             String string?,                 ;;    package java.lang
             URI uri?,                       ;;    package java.net
             UUID uuid?                      ;;    package java.util
             })

(def ctors '{IteratorSeq iterator-seq PersistentList list ISeq seq
             EnumerationSeq enumeration-seq Var "intern, with-local-vars"
             LazilyPersistentVector "vector, vec"
             PersistentHashMap hash-map PersistentHashSet "hash-set, set"
             PersistentArrayMap array-map
             PersistentTreeMap "sorted-map, sorted-map-by"
             PersistentTreeSet sorted-set
             PersistentStructMap$Def create-struct
             PersistentStructMap "struct-map, struct"
             LazyCons lazy-cons Range range FnSeq fnseq
             MultiFn defmulti Keyword keyword Symbol "symbol, gensym"})

(def clusters '#{})

;; The interfaces that are the keys of the map 'badges' are so
;; commonly implemented by Clojure classes that it would clutter up
;; the graph if they were shown as nodes with edges from other nodes.
;; Instead, label each class with the list of single-character
;; abbreviations in this map.

;; There was a java.util.stream.Streamable in some versions of the
;; JDK, but it was removed before the final release of JDK 8.  I have
;; thus removed Streamable from 'badges'.
;; https://stackoverflow.com/questions/21985854/what-happened-to-java-util-stream-streamable

;; I added IObj and IHashEq to Chouser's original list of badges,
;; since those are now also frequently implemented interfaces.

(def badges
  (apply array-map
   '[IMeta M          ;; package clojure.lang
     IObj W           ;; package clojure.lang
     Iterable T       ;; package java.lang
     Counted 1        ;; package clojure.lang
     IHashEq H        ;; package clojure.lang
     Serializable Z   ;; package java.io
     Reversible R     ;; package clojure.lang
     Named N          ;; package clojure.lang
     Comparable =]))  ;; package java.lang

;; These nodes represent classes, not interfaces, so I don't want to
;; use the badges idea to represent them, but to avoid long edges
;; stretching across the page, putting a class X in this map will
;; cause each edge "A -> X" to be represented as "A -> X copy #n".
;; All edges "X -> B" will still share a single drawn node X.

;;(def nodes-to-duplicate '#{AFn ASeq})
(def nodes-to-duplicate '#{})

(def color-override '{PersistentList "#76d700"
                      PersistentQueue "#0061d7"
                      LazySeq "#d78100"})

;; TBD: See if there is a way to get this class name reliably at run
;; time, without having to know a run-time generated class name like
;; reify__5684.

;;(def aliases '{core$future_call$reify__5684 "(future)"})
(def aliases '{})

;;(def extra-seed-classes [clojure.core$future_call$reify__5684])
(def extra-seed-classes [])

(defn class-filter* [cls]
  (let [package (-> cls .getPackage .getName)]
    (or (= package "clojure.lang")
        (and (.startsWith package "java") (.isInterface cls)))))

(def classes-failing-class-filter (atom #{}))

(defn class-filter [cls]
  (let [ret (class-filter* cls)]
    (when (and (not ret)
               (not (contains? @classes-failing-class-filter cls)))
      (swap! classes-failing-class-filter conj cls)
      (log (str "(class-filter " cls ") -> " ret)))
    ret))

(defn choose-shape [cls]
  (cond
    (not (-> cls .getPackage .getName (.startsWith "clojure"))) "diamond"
    (.isInterface cls) "octagon"
    :else "oval"))

(defn class-name [cls]
  (symbol (.getSimpleName cls)))

(defn class-label [cls]
  (let [clsname (class-name cls)
        a (aliases clsname (str clsname))
        pred (preds clsname)
        ctor (ctors clsname)
        anc (set (map class-name (ancestors cls)))]
    (str a
         ;(when ctor (str (when-not (empty? a) "\\n") ctor))
         (when pred (str \\ \n pred))
         (when-let [badge (seq (filter identity (map badges (map anc (keys badges)))))]
           (str "\\n[" (apply str badge) "]")))))

(defn class-color [cls]
  (color-override (class-name cls)
    (nth colors (rem (Math/abs (hash (str cls))) (count colors)))))

(defn class-graph [clj-classes]
  (loop [found {}
         work (into
                (into PersistentQueue/EMPTY extra-seed-classes)
                (filter #(and % (some class-filter (bases %)))
                        clj-classes))]
    (if (empty? work)
      found
      (let [cls (peek work)
            kids (seq (filter class-filter (bases cls)))]
        (recur (assoc found cls kids)
               (into (pop work) (remove found kids)))))))

(defn inc-freq [frequencies-map k]
  (assoc frequencies-map k (inc (get frequencies-map k 0))))

(def duplicated-node-counts (atom {}))

(defn dotstr [classes graph]
  (str
    "digraph {\n"
    "  rankdir=LR;\n"
    "  dpi=55;\n"
    "  nodesep=0.10;\n"
    "  ranksep=1.2;\n"
    "  mclimit=2500.0;\n"
    ;"  splines=true;\n"
    ;"  overlap=scale;\n"
    "  node[ fontname=Helvetica shape=box ];\n"
    "
  subgraph cluster_legend {
    label=\"Legend\"
    fontname=\"Helvetica Bold\"
    fontsize=19
    bgcolor=\"#dddddd\"
    \"Clojure Interface\" [ shape=octagon fillcolor=\"#ffffff\" style=filled ];
    \"Java Interface\" [ shape=diamond fillcolor=\"#ffffff\" style=filled ];
    \"Clojure class\" [ shape=oval fillcolor=\"#ffffff\" style=filled ];
    "
    (when (seq badges)
      (str "
    badges [
      shape=record
      style=filled
      fillcolor=\"#ffffff\"
      label=\"{{"
       (apply str (interpose "|" (vals badges)))
      "}|{"
       (apply str (interpose "|" (keys badges)))
      "}}\"
    ]"))
    "
  }
"
    (str-for [cls classes]
      (when-not (badges (class-name cls))
        (let [color (class-color cls)
              node (str "  \"" cls "\" [ label=\"" (class-label cls) "\" "
                        "color=\"" color "\" "
                        "shape=\"" (choose-shape cls) "\"];\n")
              cluster (some #(clusters (class-name %))
                            (cons cls (ancestors cls)))]
          (str (when cluster (str "subgraph cluster_" cluster " {\n"))
              node
              (when cluster "}\n")
              (str-for [sub (graph cls)]
                (let [sub-name (class-name sub)]
                  (if (badges sub-name)
                    nil
                    (let [dest-node
                          (if (contains? nodes-to-duplicate sub-name)
                            (str sub " copy #"
                                 (let [new-counts (swap! duplicated-node-counts
                                                         inc-freq sub-name)]
                                   (@duplicated-node-counts sub-name)))
                            (str sub))]
                      (str "  \"" cls "\" -> \"" dest-node "\""
                           " [ color=\"" color "\" ];\n")))))))))
    "}\n"))


(defn -main [& args]
  (when-not (= 2 (count args))
    (println "usage: cmd_name <clojure-local-root-directory> <directory-to-write-output-files>")
    (System/exit 1))
  (let [[clojure-local-root output-dir] args
        srcpath (clojure-java-src-dir clojure-local-root)
        file-base-names (java-source-file-base-names srcpath)
        clj-classes (clojure-lang-classes file-base-names)
        dot-fname (str output-dir "/graph-" (clojure-version) ".dot")
        log-fname (str output-dir "/log-" (clojure-version) ".txt")]

    (log (str "In directory: " srcpath))
    (log (str "found the following " (count file-base-names)
              " files with .java suffix:"))
    (doseq [base-fname (sort file-base-names)]
      (log (str "    " base-fname)))

    (let [graph (class-graph clj-classes)
          classes (sort-by #(.getSimpleName %) (keys graph))
          dot-string (dotstr classes graph)]
      (spit dot-fname dot-string)
      (write-log log-fname))))

(comment
(doto (JFrame. "Clojure Classes")
  (.add (-> (sh "dot" "-Tpng" :in dotstr :out-enc :bytes) :out ImageIcon.
          JLabel. JScrollPane.))
  (.setSize 600 400)
  (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
  (.setVisible true))
)
