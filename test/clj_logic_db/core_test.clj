(ns clj-logic-db.core-test
  (:require [clojure.test :refer :all]
            [clj-logic-db.core :refer :all]))

(def data
  [[:john :age 42]
   [:mary :age 12]
   [:john :name "john"]
   [:mary :name "mary"]
   [:john :id 100]])


(deftest test-select
  (testing "interpolation"
    (let [name "john"]
      (is (=
            '(:john)
            (select '?person {:from data}
                    (where ?person :name name))))))

   (testing "single where clause"

    (is (=
          '(:john :mary :john :mary :john)
          (select '?person {:from data}
                  (where ?person _ _))))

    (is (=
          '({})
          (select '* {:from data}
                  (where :john :age 42))))

    (is (=
          '({?person :john})
          (select '* {:from data}
                  (where ?person :age 42))))

    (is (=
          '()
          (select '* {:from data}
                  (where ?person :notmatch :this))))

    (is (=
          '({?a :a, ?b :b, ?c :c})
          (select '* {:from [[:a :b :c]]}
                  (where ?a ?b ?c))))

    (is (=
          '({?a :a})
          (select '* {:from [[:a :a :a]]}
                  (where ?a ?a ?a))))

    (is (=
          '({?a :a})
          (select '* {:from [[:a :a :a]]}
                  (where ?a ?a ?a))))

    (is (=
          '()
          (select '* {:from [[:a :b :a]]}
                  (where ?a ?a ?a))))

    (is (=
          '()
          (select '* {:from [[:a :b :a]]}
                  (where ?a ?a ?a))))

    (testing "multiple where clauses"

      (is (=
            '({?person :john, ?age 42} {?person :mary, ?age 12})
            (select '{'?person ?person, '?age ?age} {:from data}
                    (where ?person :age ?age)
                    (where ?person :name ?name))))

      (is (=
            '({?person :john, ?age 42} {?person :mary, ?age 12})
            (select '{'?person ?person, '?age ?age} {:from data}
                    (where ?person :age ?age)
                    (where ?person :name ?name)))))

    (testing "guard clause"

      (is (=
            '({?person :john, ?age 42})
            (select '{'?person ?person, '?age ?age} {:from data}
                    (where ?person :age ?age)
                    (where ?person :name ?name)
                    (guard (> ?age 18))))))

    (testing "aggregators"
      (is (=
            '(:john)
            (select '?person {:from [[:john :age 42]
                                     [:john :name "john"]
                                     [:mary :age 17]
                                     [:mary :name "mary"]
                                     [:bob :age 100]]
                              :distinct true
                              :limit 1
                              :order-by '?age}

                    (where ?person :age ?age)
                    (guard (> ?age 18)))))))

  (testing "optional"
    (is (=
          '({?person :john, ?name "john", ?age 42}
            {?person :mary, ?name "mary", ?age nil})
          (select '* {:from [[:john :name "john"]
                             [:john :age 42]
                             [:mary :name "mary"]
                             [:bob :age 100]]}

                  (where ?person :name ?name)
                  (optional
                    (where ?person :age ?age)))))

    (is (=
          '("mary")
          (select '?name {:from [[:john :name "john"]
                                 [:john :age 42]
                                 [:mary :name "mary"]
                                 [:bob :age 100]]}
                  (where ?person :name ?name)
                  (optional
                    (where ?person :age ?age))
                  (guard (nil? ?age)))))))


