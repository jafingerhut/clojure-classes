; clojure-classes.clj - produces graphviz dot graph for Clojure Java classes
;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns net.n01se.clojure-classes
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [clojure.reflect :as refl])
  (:import (javax.swing JFrame JLabel JScrollPane ImageIcon)
           (clojure.lang PersistentQueue)))

;; Ignore exceptions from attempting to require clojure.core.reducers
;; so that this program continues to work for Clojure 1.3.0 and 1.4.0
(try
  (require '[clojure.core.reducers :as red])
  (catch Throwable e
    nil))

;; Add this so code runs in older versions of Clojure
(defn is-boolean? [x]
  (instance? Boolean x))

(def log-lines (atom []))

(defn log [line-str]
  {:pre [(string? line-str)]}
  (swap! log-lines conj line-str))

(defn write-log [log-fname]
  (spit log-fname (str/join "\n" @log-lines)))

(defn reset-log []
  (reset! log-lines []))

(defn clojure-java-src-dir [clojure-local-root]
  {:pre [(string? clojure-local-root)]
   :post [(string? %)]}
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

(def java-package-abbreviations
  [{:full-name "clojure.core", :abbreviation "c.c"}
   {:full-name "java.lang", :abbreviation "j.l"}
   {:full-name "java.util.concurrent", :abbreviation "j.u.c"}
   {:full-name "java.util", :abbreviation "j.u"}])

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
   '[IType          ;; package clojure.lang
     {:abbreviation D, :description "type created by deftype"}
     IMeta          ;; package clojure.lang
     {:abbreviation M, :description "meta"}
     IObj           ;; package clojure.lang
     {:abbreviation W, :description "with-meta"}
     Iterable       ;; package java.lang
     {:abbreviation T, :description ""}
     Counted        ;; package clojure.lang
     {:abbreviation 1, :description "counted?, count is O(1)"}
     IHashEq        ;; package clojure.lang
     {:abbreviation H, :description "hash"}
     Serializable   ;; package java.io
     {:abbreviation Z, :description ""}
     Reversible     ;; package clojure.lang
     {:abbreviation R, :description "reversible?, rseq is O(1)"}
     Named          ;; package clojure.lang
     {:abbreviation N, :description ""}
     Comparable     ;; package java.lang
     {:abbreviation =, :description ""}]))


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

(defn class-named-by-symbol [sym]
  {:pre [(symbol? sym)]
   :post [(or (nil? %) (class? %))]}
  (try
    (eval sym)
    (catch clojure.lang.Compiler$CompilerException e
      nil)))

(def aliases (atom {}))

(defn extra-seed-classes []
  (let [future-call-class (class (future-call (fn [] 5)))
        sym-with-clojure-removed (symbol (clojure.string/replace-first
                                          (.getName future-call-class)
                                          #"^clojure\." ""))]
    (swap! aliases assoc sym-with-clojure-removed "(future)")
    {:future-call-class future-call-class
     :extra-classes
     (vec (filter identity
                  (concat
                   [future-call-class]
                   ;; Types in Clojure's core implementation that are
                   ;; defined via the macro clojure.core/deftype.  We
                   ;; use class-named-by-symbol so that we only keep
                   ;; the ones that exist in the version of Clojure
                   ;; running this program.
                   (map class-named-by-symbol
                        '[clojure.core.reducers.Cat
                          clojure.reflect.JavaReflector
                          clojure.reflect.AsmReflector
                          clojure.core.Eduction
                          clojure.core.VecNode
                          clojure.core.ArrayChunk
                          clojure.core.VecSeq
                          clojure.core.Vec]))))}))

(defn class-filter* [cls]
  {:pre [(class? cls)]
   :post [(is-boolean? %)]}
  ;; Use this body for class-filter* instead if you want to include
  ;; all Java superclasses and interfaces, except java.lang.Object,
  ;; which is a superclass of all other objects, so a waste of space
  ;; in a graph to draw.
  ;;(not= cls java.lang.Object)
  (let [package (-> cls .getPackage .getName)]
    (or (= package "clojure.lang")
        (.startsWith package "clojure.core")
        (.startsWith package "clojure.reflect")
        (and (.startsWith package "java") (.isInterface cls)))))

(def classes-failing-class-filter (atom #{}))

(defn class-filter [cls]
  {:pre [(class? cls)]
   :post [(is-boolean? %)]}
  (let [ret (class-filter* cls)]
    (when (and (not ret)
               (not (contains? @classes-failing-class-filter cls)))
      (swap! classes-failing-class-filter conj cls)
      (log (str "(class-filter " cls ") -> " ret)))
    ret))

(defn all-clojure-classes [clojure-local-root opts]
  (let [srcpath (clojure-java-src-dir clojure-local-root)
        file-base-names (java-source-file-base-names srcpath)
        {:keys [future-call-class extra-classes]} (extra-seed-classes)]
    (when (:log opts)
      (log (str "Found future-call compiled class with name: "
                (.getName future-call-class)))
      (log (str "In directory: " srcpath))
      (log (str "found the following " (count file-base-names)
                " files with .java suffix:"))
      (doseq [base-fname (sort file-base-names)]
        (log (str "    " base-fname))))
    (concat extra-classes
            (filter #(and % (some class-filter (bases %)))
                    (clojure-lang-classes file-base-names)))))

(defn choose-shape [cls]
  {:pre [(class? cls)]
   :post [(string? %)]}
  (if (-> cls .getPackage .getName (.startsWith "clojure"))
    (if (.isInterface cls) "octagon" "oval")         ;; Clojure
    (if (.isInterface cls) "parallelogram" "box")))  ;; non-Clojure

(defn abbreviate-java-package [class-name-str]
  {:pre [(string? class-name-str)]
   :post [(string? %)]}
  (loop [abbrevs java-package-abbreviations]
    (if-let [s (seq abbrevs)]
      (let [{:keys [full-name abbreviation]} (first s)]
        (if (.startsWith class-name-str full-name)
          (str/replace-first class-name-str full-name abbreviation)
          (recur (rest s))))
      class-name-str)))

(defn class->symbol [cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (let [full-name (.getName cls)]
    (symbol
     (if (or (.startsWith full-name "clojure.lang.")
             (.startsWith full-name "clojure.core$")
             (contains? #{"java.lang.Iterable" "java.io.Serializable"
                          "java.lang.Comparable"}
                        full-name))
       (.getSimpleName cls)
       (abbreviate-java-package full-name)))))

(defn class-label [cls]
  {:pre [(class? cls)]
   :post [(string? %)]}
  (let [clsname (class->symbol cls)
        a (@aliases clsname (str clsname))
        pred (preds clsname)
        ctor (ctors clsname)
        anc (set (map class->symbol (ancestors cls)))]
    (str a
         ;(when ctor (str (when-not (empty? a) "\\n") ctor))
         (when pred (str \\ \n pred))
         (when-let [badge (->> (keys badges)
                               (map anc)
                               (map badges)
                               (map :abbreviation)
                               (filter identity)
                               seq)]
           (str "\\n[" (apply str badge) "]")))))

(defn class-color [cls]
  {:pre [(class? cls)]
   :post [(string? %)]}
  ;; Use .hashCode instead of hash for more consistent results across
  ;; Clojure versions before and after 1.6.0, when clojure.core/hash
  ;; was improved.
  (color-override (class->symbol cls)
    (nth colors (rem (Math/abs (.hashCode (str cls))) (count colors)))))

(defn nil-or-class-coll? [x]
  (or (nil? x)
      (and (coll? x)
           (every? class? x))))

(defn class-graph? [graph]
  (and (map? graph)
       (every? class? (keys graph))
       (every? nil-or-class-coll? (vals graph))))

(defn class-graph [clj-classes class-filter]
  {:pre [(every? class? clj-classes)]
   :post [(class-graph? %)]}
  (loop [found {}
         work (into PersistentQueue/EMPTY clj-classes)]
    (if (empty? work)
      found
      (let [cls (peek work)
            kids (seq (filter class-filter (bases cls)))]
        (recur (assoc found cls kids)
               (into (pop work) (remove found kids)))))))

(defn inc-freq [frequencies-map k]
  (assoc frequencies-map k (inc (get frequencies-map k 0))))

(def duplicated-node-counts (atom {}))

(defn dot-for-node-shapes-legend []
   "\"Clojure class\" [ shape=oval fillcolor=\"#ffffff\" style=filled ];
    \"Clojure Interface\" [ shape=octagon fillcolor=\"#ffffff\" style=filled ];
    \"Java class\" [ shape=box fillcolor=\"#ffffff\" style=filled ];
    \"Java Interface\" [ shape=parallelogram fillcolor=\"#ffffff\" style=filled ];
    ")

(defn dot-for-badges-legend [badges]
  (str "
    badges [
      shape=record
      style=filled
      fillcolor=\"#ffffff\"
      label=\"{{"
       (apply str (interpose "|" (map :abbreviation (vals badges))))
       "}|{"
       (apply str (interpose "|" (keys badges)))
       "}|{"
       (apply str (interpose "|" (map :description (vals badges))))
       "}}\"
    ]"))

(defn dot-for-java-pkg-abbrevs-legend [java-pkg-abbrevs]
  (str "
    java_package_abbreviations [
      shape=record
      style=filled
      fillcolor=\"#ffffff\"
      label=\"{{"
       (apply str (interpose "|" (cons "Abbrev."
                                       (map :abbreviation java-pkg-abbrevs))))
       "}|{"
       (apply str (interpose "|" (cons "Java Package"
                                       (map :full-name java-pkg-abbrevs))))
       "}}\"
    ]"))

(defn dotstr [graph opts]
  {:pre [(class-graph? graph)
         (map? opts)]
   :post [(string? %)]}
  (let [default-opts {:class-order (keys graph)
                      :badges badges
                      :java-package-abbreviations java-package-abbreviations
                      :class->sym class->symbol
                      :class->color class-color
                      :class->label class-label
                      :class->shape choose-shape}
        {:keys [class-order badges java-package-abbreviations
                class->sym class->color class->label class->shape]}
        (merge default-opts opts)]
    (str
     "digraph {\n"
     "  rankdir=LR;\n"
     "  dpi=55;\n"
     "  nodesep=0.10;\n"
     "  ranksep=1.2;\n"
     "  mclimit=2500.0;\n"
     ;;"  splines=true;\n"
     ;;"  overlap=scale;\n"
     "  node[ fontname=Helvetica shape=box ];\n"
     "
  subgraph cluster_legend {
    label=\"Legend\"
    fontname=\"Helvetica Bold\"
    fontsize=19
    bgcolor=\"#dddddd\"
    "
     (dot-for-node-shapes-legend)
     (when (seq badges)
       (dot-for-badges-legend badges))
     (when (seq java-package-abbreviations)
       (dot-for-java-pkg-abbrevs-legend java-package-abbreviations))
     "
  }
"
     (str-for [cls class-order]
      (when-not (badges (class->sym cls))
        (let [color (class->color cls)
              node (str "  \"" cls "\" [ label=\"" (class->label cls) "\" "
                        "color=\"" color "\" "
                        "shape=\"" (class->shape cls) "\"];\n")
              cluster (some #(clusters (class->sym %))
                            (cons cls (ancestors cls)))]
          (str
           (when cluster (str "subgraph cluster_" cluster " {\n"))
           node
           (when cluster "}\n")
           (str-for [cls2 (graph cls)]
             (let [cls2-name (class->sym cls2)]
               (if (badges cls2-name)
                 nil
                 (let [dest-node
                       (if (contains? nodes-to-duplicate cls2-name)
                         (str cls2 " copy #"
                              (let [new-counts (swap! duplicated-node-counts
                                                      inc-freq cls2-name)]
                                (@duplicated-node-counts cls2-name)))
                         (str cls2))]
                   (str "  \"" cls "\" -> \"" dest-node "\""
                        " [ color=\"" color "\" ];\n")))))))))
    "}\n")))


(defn cli-strings->classes [cli-strings]
  (let [info (map (fn [s]
                    {:string s, :class (class-named-by-symbol (symbol s))})
                  cli-strings)
        not-a-class (remove #(class? (:class %)) info)]
    (when (seq not-a-class)
      (println "The following strings given on the command line do not name classes:")
      (doseq [x not-a-class]
        (println (str "    " (:string x))))
      (flush)
      (System/exit 1))
    (map :class info)))


(defn make-dot-graph [classes opts]
  (let [cf (get opts :class-filter class-filter)
        graph (class-graph classes cf)
        dot-string (dotstr graph opts)]
    (when (:create-window opts)
      (doto (JFrame. "Clojure Classes")
        (.add (-> (sh/sh "dot" "-Tpng" :in dot-string :out-enc :bytes)
                  :out ImageIcon. JLabel. JScrollPane.))
        (.setSize 600 400)
        (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
        (.setVisible true)))
    dot-string))

(defn -main [& args]
  (when-not (and (>= (count args) 3)
                 (contains? #{"all-clojure-classes" "classes"} (nth args 1)))
    (println "usage:")
    (println "    cmd_name <output-file-directory> all-clojure-classes <clojure-local-root-directory>")
    (println "    cmd_name <output-file-directory> classes <classname1> ...")
    (flush)
    (System/exit 1))
  (let [output-dir (nth args 0)
        dot-fname (str output-dir "/graph-" (clojure-version) ".dot")
        log-fname (str output-dir "/log-" (clojure-version) ".txt")
        class-selection-mode (keyword (nth args 1))
        clj-classes (case class-selection-mode
                      :all-clojure-classes (all-clojure-classes (nth args 2)
                                                                {:log true})
                      :classes (cli-strings->classes (nthrest args 2)))
        graph (class-graph clj-classes class-filter)
        classes (sort-by #(.getSimpleName %) (keys graph))
        opts {:class-order classes}
        dot-string (dotstr graph opts)]

    (spit dot-fname dot-string)
    (write-log log-fname)
    (shutdown-agents)))
