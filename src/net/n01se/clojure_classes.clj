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
            [net.n01se.reflection :as reflect])
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
  [{:full-name "clojure.lang.", :abbreviation ""}
   {:full-name "clojure.core", :abbreviation "c.c"}
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

;; This list of symbols correspond to the names of classes in
;; Clojure's implementation that are defined via the macro
;; clojure.core/deftype.
(def clojure-core-deftype-classes
  '[clojure.core.reducers.Cat
    clojure.reflect.JavaReflector
    clojure.reflect.AsmReflector
    clojure.core.Eduction
    clojure.core.VecNode
    clojure.core.ArrayChunk
    clojure.core.VecSeq
    clojure.core.Vec])

;; Note 1: class-named-by-symbol returns nil if the symbol does not
;; name a class in the version of Clojure that is running this
;; program.  This allows this same Clojure program to work for many
;; versions of Clojure, even older ones that do not define all of the
;; classes in the vector clojure-core-deftype-classes.  The `filter
;; identity` wrapped around this expression, removes the `nil` return
;; values.

(defn extra-seed-classes []
  (let [future-call-class (class (future-call (fn [] 5)))
        sym-with-clojure-removed (symbol (clojure.string/replace-first
                                          (.getName future-call-class)
                                          #"^clojure\." ""))]
    (swap! aliases assoc sym-with-clojure-removed "(future)")
    {:future-call-class future-call-class
     :extra-classes (vec
                     (filter identity
                             (concat [future-call-class]
                                     ;; See Note 1
                                     (map class-named-by-symbol
                                          clojure-core-deftype-classes))))}))

(defn default-class-filter* [cls]
  {:pre [(class? cls)]
   :post [(is-boolean? %)]}
  (let [cls-name (.getName cls)]
    (or (.startsWith cls-name "clojure.lang")
        (.startsWith cls-name "clojure.core")
        (.startsWith cls-name "clojure.reflect")
        (and (.startsWith cls-name "java") (.isInterface cls)))))

(def classes-failing-default-class-filter (atom #{}))

(defn default-class-filter [cls]
  {:pre [(class? cls)]
   :post [(is-boolean? %)]}
  (let [ret (default-class-filter* cls)]
    (when (and (not ret)
               (not (contains? @classes-failing-default-class-filter cls)))
      (swap! classes-failing-default-class-filter conj cls)
      (log (str "(default-class-filter " cls ") -> " ret)))
    ret))

;; Use not-root-class as your filter for classes, instead of
;; default-class-filter, if you want to include all Java superclasses
;; and interfaces, except java.lang.Object.  java.lang.Object is a
;; superclass of all other objects, so a waste of space in a graph to
;; draw.

(defn not-root-class [cls]
  (not= cls java.lang.Object))

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
            (filter #(and % (some default-class-filter (bases %)))
                    (clojure-lang-classes file-base-names)))))

(def default-node-shapes {:node-shape-clojure-class "oval"
                          :node-shape-clojure-interface "octagon"
                          :node-shape-java-class "parallelogram"
                          :node-shape-java-interface "box"})

(defn choose-shape [cls opts]
  {:pre [(class? cls)]
   :post [(string? %)]}
  (let [{:keys [node-shape-clojure-class
                node-shape-clojure-interface
                node-shape-java-class
                node-shape-java-interface]} (merge default-node-shapes opts)]
    (if (-> cls .getName (.startsWith "clojure"))
      (if (.isInterface cls)
        node-shape-clojure-interface
        node-shape-clojure-class)
      (if (.isInterface cls)
        node-shape-java-interface
        node-shape-java-class))))

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

(defn html-one-col-row [s align]
  (str "<tr><td align=\"" align "\">" s "</td></tr>"))

(defn remove-prefix [s prefix]
  (if (.startsWith s prefix)
    (subs s (count prefix))
    s))

(defn default-member-classifier [member-info opts]
  (let [omit-compiler-generated-fields? (get opts
                                             :omit-compiler-generated-fields
                                             false)
        deftype-field? (and (= :field (:kind member-info))
                            (contains? (set (bases (:class member-info)))
                                       clojure.lang.IType))
        name (:name member-info)
        name-str (str name)]
    (cond

      ;; Some classes created with clojure.core/deftype (and maybe
      ;; others?) contain auto-generated fields with names of the
      ;; form "const__<number>" where <number> is a decimal integer.
      ;; It is often nicer to summarize these rather than list them
      ;; out individually.
      (and omit-compiler-generated-fields?
           deftype-field?
           (.startsWith name-str "const__"))
      :const-field

      ;; Similarly some classes created with deftype (and perhaps
      ;; others) contain fields named "__cached_class__<number>".
      (and omit-compiler-generated-fields?
           deftype-field?
           (.startsWith name-str "__cached_class__"))
      :cached-class-field

      ;; The classes and interfaces in the set below each contain a
      ;; little more than 20 different signatures for a method
      ;; named "invoke", differing only in the number of parameters
      ;; that they take.  It makes for a very large node label if all
      ;; of them are listed separately.
      (and (contains? #{clojure.lang.IFn clojure.lang.AFn
                        clojure.lang.RestFn clojure.lang.MultiFn
                        clojure.lang.Keyword clojure.lang.Var
                        clojure.lang.Ref}
                      (:class member-info))
           (= 'invoke name))
      :fn-invoke-method

      ;; This case is similar to the "invoke" method above, but the
      ;; classes in the set below have a little more than 20 different
      ;; signatures for a method named "doInvoke".
      (and (contains? #{clojure.lang.RestFn}
                      (:class member-info))
           (= 'doInvoke (:name member-info)))
      :fn-doinvoke-method

      :else :normal)))

(defn default-grouped-member-transform [classification member-infos]
  "Return a sequence of strings that describe some of the
  members (e.g. fields, constructors, or methods) of a class.

  classification is the return value from a user-defined classifier
  function.

  member-infos is a sequence of maps, one for each member of a
  particular kind (field, constructor, or method) and classification
  within a class.  Each map contains the following keys describing the
  member:

  :class - the class that the member is a part of

  :kind - value is one of the set #{:field :constructor :method}

  :description-str - value is a string describing the member in
      Java-like syntax

  plus everything that clojure.reflect/type-reflect returns for a
  member, which includes:

  :name - the value is either a class object, or a symbol (tbd)

  :flags - a set of keywords.  See net.n01se.reflection namespace vars
      interface-flag-print-order, method-flag-print-order, and
      field-flag-print-order for a list of keywords that are known to
      possibly be included for each of a constructor, method, and
      field.

  :type - only if :kind is :field.  A symbol that is the type of the
      field.

  :return-type - only if :kind is :method.  A symbol that is the
      return type of the method.

  :parameter-types - only if :kind is :constructor or :method.  A
      sequence of symbols that are the types taken by the parameter.
      They can end with one or more sequences of characters <> to
      denote Java arrays.

  :exception-types - only if :kind is :constructor or :method.  I have
      not yet investigated what the value contains.
  "
  (let [n (count member-infos)]
    (cond
      (= classification :normal)
      (map :description-str member-infos)
      
      (= classification :const-field)
      [(str n " const__&lt;num&gt; fields omitted")]

      (= classification :cached-class-field)
      [(str n " __cached_class__&lt;num&gt; fields omitted")]
      
      (= classification :fn-invoke-method)
      [(str n " invoke methods omitted")]

      (= classification :fn-doinvoke-method)
      [(str n " doInvoke methods omitted")]
      
      :else
      [(str "Unknown classification " classification " with " n " members")])))

(defn members-label [cls opts]
  (let [member-descs (reflect/all-description-strs
                      cls {:class-name-abbreviator abbreviate-java-package})
        classifier (get opts :member-classifier (constantly :normal))
        interface? (.isInterface cls)]
    (str/join
     ""
     (for [kind [:field :constructor :method]]
       (let [mems (map #(assoc % :class cls) (member-descs kind))
             n (count mems)
             mems-grouped (group-by #(classifier % opts) mems)
             groups (sort (keys mems-grouped))
             not-normal-groups (remove #(= :normal %) groups)
             mems-grouped2
             (into {}
                   (for [[group mems-in-group] mems-grouped]
                     [group
                      (default-grouped-member-transform group mems-in-group)]))]
         (str/join ""
                   (concat
                    (if (or (and (not interface?) (> n 0))
                            (> n 4))
                      [(html-one-col-row (str n " " (name kind)
                                              (if (> n 1) "s"))
                                         "center")])
                    (map #(html-one-col-row
                           (if interface?
                             (remove-prefix % "public abstract ")
                             %)
                           "left")
                         (concat
                          (mapcat mems-grouped2 not-normal-groups)
                          (mems-grouped2 :normal))))))))))

(defn class-label
  "Given a class `cls`, i.e. a Java object with class java.lang.Class,
  return a string that will be used to label the node in a drawing.

  The string returns should should follow the Graphviz rules that it
  must begin and end with a double quote, and contain a 'regular
  label' string, or it must begin with < and end with >, and contain
  the text of an HTML label string.

  :badges - Default value net.n01se.clojure-classes/badges

  See documentation of function dotstr for more details.

  :show-members - Default value false.

  If true, then include not only the class name, but also a
  description of its member fields, constructors, and methods.  These
  can make the nodes quite large."
  [cls opts]
  {:pre [(class? cls)
         (map? opts)]
   :post [(string? %)]}
  (let [clsname (class->symbol cls)
        badges (get opts :badges badges)
        show-members? (get opts :show-members false)
        html-label? show-members?
        a (@aliases clsname (str clsname))
        pred (preds clsname)
        ctor (ctors clsname)
        anc (set (map class->symbol (ancestors cls)))
        badge (->> (keys badges)
                   (map #(:abbreviation (badges (anc %))))
                   (filter identity)
                   seq)]
    (str (if html-label? "<" "\"")
         (if show-members? "<table border=\"0\">")
         (if show-members? (html-one-col-row a "center") a)
         ;(when ctor (str (when-not (empty? a) "\\n") ctor))
         (when pred
           (str "\\n" (if show-members?
                        (html-one-col-row pred "center")
                        pred)))
         (when badge
           (let [badge-str (str "[" (apply str badge) "]")]
             (if show-members?
               (html-one-col-row badge-str "center")
               (str "\\n" badge-str))))
         (if show-members? (members-label cls opts))
         (if show-members? (str "</table>"))
         (if html-label? ">" "\""))))

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

(defn dot-for-node-shapes-legend [opts]
  (let [{:keys [node-shape-clojure-class
                node-shape-clojure-interface
                node-shape-java-class
                node-shape-java-interface]} (merge default-node-shapes opts)]
    (str
     "\"Clojure class\" [ shape=" node-shape-clojure-class " fillcolor=\"#ffffff\" style=filled ];
    \"Clojure Interface\" [ shape=" node-shape-clojure-interface " fillcolor=\"#ffffff\" style=filled ];
    \"Java class\" [ shape=" node-shape-java-class " fillcolor=\"#ffffff\" style=filled ];
    \"Java Interface\" [ shape=" node-shape-java-interface " fillcolor=\"#ffffff\" style=filled ];
    ")))

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

(defn dotstr
  "Given a class graph `graph`, represented as a map whose keys are
  Java objects with class java.lang.Class, and associated values are
  collections of other java.lang.Class objects, return a string that
  can be given as input to Graphviz's dot commmand to create a drawing
  of this graph.

  The behavior of dotstr can be affected by including the following
  keys in the options map `opts`, but see also the documentation for
  the function net.n01se.clojure-classes/class-label if you use that
  function for creating node labels, as it is passed this same `opts`
  map for controlling its behavior.

  :badges - Default value net.n01se.clojure-classes/badges.

  A Clojure map with keys that are Java interfaces, named as Clojure
  symbols, as returned by the function class->symbol.  The
  corresponding values for each key are themselves Clojure maps with
  keys :abbreviation (a short symbol) and :description (a string
  describing the interface for the legend of the drawing).

  :class-order - Default value (keys graph).

  A sequence of java.lang.Class objects, all of which should be keys
  in the map `graph`, representing the order they they should be
  considered by this function.

  :class->sym - Default value net.n01se.clojure-classes/class->symbol

  A function that takes an instance of class java.lang.Class, and
  returns a symbol whose name will be used as the label for the class
  in the graph.  This symbol is also the key used to look up in the
  map 'badges'.

  :class->color - Default value net.n01se.clojure-classes/class-color

  A function that takes an instance of class java.lang.Class, and
  returns a string naming a Graphviz color, which will be used as the
  color of the node in the drawing.

  :class->label - Default value net.n01se.clojure-classes/class-label

  A function that takes an instance of class java.lang.Class and the
  map `opts`, and returns a string that will be the label of the node
  for this class in the drawing.

  :class->shape - Default value net.n01se.clojure-classes/choose-shape

  A function that takes an instance of class java.lang.Class, and
  returns a string naming a Graphviz node shape.  Useful values
  include \"box\", \"oval\", \"octagon\", \"parallelogram\", but see
  the Graphviz documentation for the node 'shape' attribute for a full
  list.

  :java-package-abbreviations - Default value
  net.n01se.clojure-classes/java-package-abbreviations.

  A sequence of maps, each containing the keys :full-name
  and :abbreviation.  The values for both keys are strings.  The
  string :full-name is a prefix of a full Java class name,
  e.g. \"java.util\", and the string :abbreviation is a shorter string
  that will replace the longer prefix if it is ever found in the name
  of a class in the label of a node for that class, e.g. \"j.u\".

  :rankdir - Default value \"LR\" (left to right).

  The value for the Graphviz dot language 'rankdir' attribute of a
  graph.  Other useful values include \"BT\" (bottom to top) and
  \"TB\" (top to bottom), but see the Graphviz documentation for more.

  :show-legend - Default value true.

  true if the drawing should include a legend for node shapes, badge
  meanings, and Java package abbreviations.

  :splines-type - Default value \"splines\".

  The value for the Graphviz dot language 'splines' attribute of a
  graph.  Other useful values include \"ortho\" and \"polyline\", but
  see the Graphviz documentation for more."
  [graph opts]
  {:pre [(class-graph? graph)
         (map? opts)]
   :post [(string? %)]}
  (let [default-opts {:class-order (keys graph)
                      :badges badges
                      :java-package-abbreviations java-package-abbreviations
                      :class->sym class->symbol
                      :class->color class-color
                      :class->label class-label
                      :class->shape choose-shape
                      :show-legend true
                      :splines-type "splines"
                      :rankdir "LR"}
        {:keys [class-order badges java-package-abbreviations
                class->sym class->color class->label class->shape
                show-legend splines-type rankdir]}
        (merge default-opts opts)]
    (str
     "digraph {\n"
     "  rankdir=" rankdir ";\n"
     "  dpi=55;\n"
     "  nodesep=0.10;\n"
     "  ranksep=1.2;\n"
     "  mclimit=2500.0;\n"
     "  splines=" splines-type ";\n"
     ;;"  overlap=scale;\n"
     "  node[ fontname=Helvetica shape=box ];\n"
     "
  subgraph cluster_legend {
    label=\"Legend\"
    fontname=\"Helvetica Bold\"
    fontsize=19
    bgcolor=\"#dddddd\"
    "
     (if show-legend
       (str
        (dot-for-node-shapes-legend opts)
        (when (seq badges)
          (dot-for-badges-legend badges))
        (when (seq java-package-abbreviations)
          (dot-for-java-pkg-abbrevs-legend java-package-abbreviations))))
     "
  }
"
     (str-for [cls class-order]
      (when-not (badges (class->sym cls))
        (let [color (class->color cls)
              node (str "  \"" cls "\" [ label=" (class->label cls opts)
                        " color=\"" color "\" "
                        "shape=\"" (class->shape cls opts) "\"];\n")
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
  "Create a graph with one node for each Java class that is in the
  that is in the collection `classes`.  Each may also be a Java
  interface, since Java interfaces are represented in the reflection
  API as classes.

  The graph will include a node for each class in `classes`, plus any
  that can be reached from one of those classes by a superclass or
  'class implements interface' relationship from one of those classes.

  Other classes or interfaces reached that are not in `classes` are
  only included if the 'class filter' function returns true for them.
  By default this function is
  net.n01se.clojure-classes/default-class-filter, but you can provide
  a different one as the value of the key :class-filter in the options
  map `opts`.  Another useful class filter function is
  net.n01se.clojure-classes/not-root-class, which returns true for all
  classes except java.lang.Object.

  Options map keys that can modify the behavior are given below.  See
  also the function dotstr for the keys in the options map that affect
  its behavior, as this function calls dotstr to create the Graphviz
  dot language describing the drawing.

  :class-filter - Default value net.n01se.clojure-classes/default-class-filter

  A function that takes as input a JVM class or interface, and returns
  true or false for whether to include the class in the generated
  graph of classes.

  :create-window - Default value false.

  true to use the Graphviz dot command to create a drawing of the
  graph and open a new window containing that drawing."
  (let [cf (get opts :class-filter default-class-filter)
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
        graph (class-graph clj-classes default-class-filter)
        classes (sort-by #(.getSimpleName %) (keys graph))
        opts {:class-order classes}
        dot-string (dotstr graph opts)]

    (spit dot-fname dot-string)
    (write-log log-fname)
    (shutdown-agents)))
