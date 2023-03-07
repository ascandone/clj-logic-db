(ns clj-logic-db.core)

(def ^:dynamic *optional-binding*
  false)

(defn- let-project [bindings body]
  `(let [~'* '~bindings
         ~@(-> bindings seq flatten)]
     ~body))


(defn unify [clauses facts bindings]
  (if (empty? clauses)
    (list bindings)
    (let [[clause & rest-clauses] clauses]
      (->> (clause facts bindings)
           (mapcat #(unify rest-clauses facts %))))))

(defn- lvar? [s]
  (and
    (symbol? s)
    (-> s str first (= \?))))


(defn- assoc-if-lvar [map key val]
  (if (lvar? key)
    (assoc map key val)
    map))

(defn- make-pipeline [& ops]
  (->> ops
       (remove nil?)
       (reverse)
       (apply comp)))

(defn select
  [projection options & clauses]
  (let [{:keys [from distinct limit order-by]} options

        bindings-list (unify clauses from {})

        pipeline (make-pipeline
                   (if distinct clojure.core/distinct)
                   (if order-by #(sort-by order-by %))
                   (if limit #(take limit %)))]

    (for [bindings (pipeline bindings-list)]
      (eval (let-project bindings projection)))))


(defn- unify-item [bindings c f]
  (cond
    ;; catchall
    (= c '_) bindings

    ;; bound var
    (and
      (lvar? c)
      (contains? bindings c)) (if (= f (bindings c))
                                bindings
                                nil)

    ;; fresh var
    (lvar? c) (assoc bindings c f)

    ;; literal value that matches
    (= c f) bindings

    ;; literal value that does not match
    :else nil))

(defn where- [c1 c2 c3]
  (let [optional-binding *optional-binding*]
    (fn [facts bindings]
      (letfn [(handle-optional [bindings-list] (if-not (and optional-binding (empty? bindings-list))
                                                 bindings-list
                                                 (-> bindings
                                                     (assoc-if-lvar c1 (bindings c1))
                                                     (assoc-if-lvar c2 (bindings c2))
                                                     (assoc-if-lvar c3 (bindings c3))
                                                     (list))))]
        (->> facts
             (map (fn [[f1 f2 f3]]
                    (some-> bindings
                            (unify-item c1 f1)
                            (unify-item c2 f2)
                            (unify-item c3 f3))))
             (remove nil?)
             (handle-optional))))))


(defn- get-where-sym [s]
  (cond
    (= '_ s) ''_
    (lvar? s) `(quote ~s)
    :else s))

(defn guard- [pred]
  (fn [facts bindings]
    (if (eval (let-project bindings pred))
      (list bindings)
      ())))

(defmacro guard [pred]
  `(guard- '~pred))


(defn unify-clauses [& clauses]
  (fn [facts bindings]
    (unify clauses facts bindings)))

(defmacro optional [& clauses]
  `(binding [*optional-binding* true]
     (unify-clauses ~@clauses)))


(defmacro where [c1 c2 c3]
  `(where-
     ~(get-where-sym c1)
     ~(get-where-sym c2)
     ~(get-where-sym c3)))