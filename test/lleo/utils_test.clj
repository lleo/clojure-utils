(ns lleo.utils-test
  (:require [clojure.test :refer :all]
            [lleo.utils :refer :all]))

(def STR "foo\nbar\n")

(deftest t-substr
  "test substr"
  (is (= "foo" (substr STR 0 3))))

(deftest t-split-fixed
  "test split-fixed"
  (is (= 3 (count (split-fixed STR "\n")))))

(deftest t-split-fixed-limit
  "test split-fixed-limit"
  (is (= 2 (count (split-fixed-limit STR "\n" 2)))))

(deftest t-split--fixed
  "test multimethod split \"foo\\nbar\\n\" \"\\n\""
  (is (= 3 (count (split STR "\n")))))

(deftest t-split--fixed-limit
  "test multimethod split \"foo\\nbar\\n\" \"\\n\""
  (is (= 2 (count (split STR "\n" 2)))))

(deftest t-split--regex
  "test multimethod split \"foo\\nbar\\n\" #\"\\n\""
  (is (= 3 (count (split STR #"\n")))))

(deftest t-split--regex-limit
  "test multimethod split \"foo\\nbar\\n\" #\"\\n\""
  (is (= 2 (count (split STR #"\n" 2)))))
