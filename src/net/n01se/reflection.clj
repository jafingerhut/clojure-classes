(ns net.n01se.reflection
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.reflect :as refl]))


;; See file src/clj/clojure/reflect/java.clj in the Clojure
;; implementation for a list of flags it can return for classes,
;; fields, and methods.

(def interface-flag-print-order
  [:public :private :protected :final :interface :abstract :synthetic
   :annotation :enum])

(def method-flag-print-order
  [:public :private :protected :static :final :synchronized
   :bridge :varargs :native :abstract :strict :synthetic])

(def field-flag-print-order
  [:public :private :protected :static :final :volatile
   :transient :synthetic :enum])

(defn method-flags-str [method-flags-set]
  {:pre [(set? method-flags-set)]
   :post [(string? %)]}
  (->> method-flag-print-order
       (filter #(contains? method-flags-set %))
       (map name)
       (str/join " ")))

(defn field-flags-str [field-flags-set]
  {:pre [(set? field-flags-set)]
   :post [(string? %)]}
  (->> field-flag-print-order
       (filter #(contains? field-flags-set %))
       (map name)
       (str/join " ")))

(defn remove-array-markers [^String s]
  {:pre [(string? s)]
   :post [(map? %)]}
  (loop [s s
         suffix ""]
    (if (.endsWith s "<>")
      (recur (subs s 0 (- (count s) 2)) (str "[]" suffix))
      {:type s, :suffix suffix})))

(comment
(remove-array-markers "int<><>")
(remove-array-markers "int")
)

(defn type-str [type-info opts]
  {:pre [(symbol? type-info)
         (map? opts)]
   :post [(or (string? %) (nil? %))]}
  (let [class-name-abbreviator (get opts :class-name-abbreviator identity)]
    (case type-info
      void "void"
      byte "byte"
      short "short"
      int "int"
      long "long"
      char "char"
      float "float"
      double "double"
      boolean "boolean"
      (try
        (let [{:keys [type suffix]} (remove-array-markers (str type-info))]
          (str (class-name-abbreviator type) suffix))
        (catch Throwable e
          (println "Exception when type-str called with type-info=" type-info))))))

(defn constructor-description-str [constructor-info opts]
  (let [{:keys [name parameter-types
                exception-types flags]} constructor-info]
    (str (method-flags-str flags)
         " " name
         " (" (str/join ", " (map #(type-str % opts) parameter-types)) ")")))

(defn method-description-str [method-info opts]
  (let [{:keys [name return-type
                parameter-types exception-types flags]} method-info]
    (str (method-flags-str flags)
         " " (type-str return-type opts)
         " " name
         " (" (str/join ", " (map #(type-str % opts) parameter-types)) ")")))

(defn field-description-str [field-info opts]
  (let [{:keys [name type flags]} field-info]
    (str (field-flags-str flags)
         " " (type-str type opts)
         " " name)))

(defn member-kind
  "Use keys in the map `member-info`, returned as one of the maps in
  the collection of maps that is the value of the :members"
  [member-info]
  (cond
    (contains? member-info :return-type) :method
    (contains? member-info :parameter-types) :constructor
    :else :field))

(defn member-description-str [member-info opts]
  (case (member-kind member-info)
    :method (method-description-str member-info opts)
    :constructor (constructor-description-str member-info opts)
    :field (field-description-str member-info opts)))

(defn all-description-strs [typeref opts]
  {:pre [(or (class? typeref) (symbol? typeref))
         (map? opts)]}
  (let [members (->> (:members (refl/type-reflect typeref))
                     (map (fn [m]
                            (assoc m :kind (member-kind m)
                                   :description-str
                                   (member-description-str m opts)))))
        grp (group-by :kind members)]
    (into {} (for [kind [:field :constructor :method]]
               [kind (sort-by (fn [m]
                                [(:name m) (if (= :field (:kind m))
                                             0
                                             (count (:parameter-types m)))])
                              (grp kind))]))))

(defn show [typeref opts]
  {:pre [(or (class? typeref) (symbol? typeref))]}
  (let [member-descs (all-description-strs typeref opts)]
    (doseq [kind [:field :constructor :method]]
      (when (seq (member-descs kind))
        (println (case kind
                   :field "Fields:"
                   :constructor "Constructors:"
                   :method "Methods:"))
        (pp/pprint (map :description-str (member-descs kind)))))))

(defn show-old [typeref opts]
  {:pre [(or (class? typeref) (symbol? typeref))]}
  (let [members (:members (refl/type-reflect typeref))
        grp (group-by member-kind members)]
    (doseq [kind [:field :constructor :method]]
      (when (seq (get grp kind))
        (println (case kind
                   :field "Fields:"
                   :constructor "Constructors:"
                   :method "Methods:"))
        (pp/pprint (map #(member-description-str % opts)
                        (sort-by :name (get grp kind))))))))

(comment

(require '[net.n01se.clojure-classes :as cc])
(require '[net.n01se.reflection :as ref])
(def show ref/show)

(in-ns 'net.n01se.clojure-classes)
(in-ns 'net.n01se.reflection)
(in-ns 'user)

(def opts {})
(def opts {:class-name-abbreviator cc/abbreviate-java-package})

(show clojure.lang.IPersistentVector opts)
;; TBD: Why is this in list of members?
;; "public bridge synthetic IPersistentCollection cons (j.l.Object)"
;; Is it because IPersistentVector extends IPersistentStack,
;; and IPersistentStack extends IPersistentCollection,
;; and IPersistentCollection contains a method with a signature like that?

;; That is in addition to the following, which I do expect from the Java defn:
;; "public abstract IPersistentVector cons (j.l.Object)"
;; "public abstract int length ()"
;; "public abstract IPersistentVector assocN (int, j.l.Object)")

;; Possible answer: This article demonstrates some class examples with
;; Java source code that can lead to bridge methods being created by
;; the Java compiler:

;; http://stas-blogspot.blogspot.com/2010/03/java-bridge-methods-explained.html#:~:text=Bridge%20methods%20in%20Java%20are,the%20actual%20method%20being%20invoked.

(class clojure.lang.Associative)
(require '[clojure.reflect :as refl])
(refl/type-reflect clojure.lang.Associative)
(refl/type-reflect 'clojure.lang.Associative)
(doc refl/type-reflect)
(source refl/type-reflect)

(ref/show-old clojure.lang.Associative opts)      ;; looks as expected from Java defn

(show clojure.lang.Associative opts)      ;; looks as expected from Java defn
(show clojure.lang.Sequential opts)       ;; looks as expected from Java defn
(show clojure.lang.IPersistentStack opts) ;; looks as expected from Java defn
(show clojure.lang.Reversible opts)       ;; looks as expected from Java defn
(show clojure.lang.Indexed opts)          ;; looks as expected from Java defn

(show clojure.lang.Counted opts)          ;; looks as expected from Java defn

(show clojure.lang.IPersistentCollection opts) ;; looks as expected from Java defn
(show clojure.lang.IFn opts)

(show clojure.core.Vec opts)

)
