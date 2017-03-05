(ns biotool.core-test
  (:require [clojure.test :refer :all]
            [clj-biosequence.core :as bs]
            [clojure.java.io :as io]
            [biotool.core :refer :all]))

(import '(java.io BufferedReader StringReader))


;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))

(deftest test-one-sequence 
  (testing "process-fasta-reader on a FASTA file with one sequence"
    (is (= (process-fasta-reader 0 (bs/init-fasta-reader (BufferedReader. (StringReader. ">header\nATGC\nA")) :iupacAminoAcids)) {:num-sequences 1, :num-bases 5, :min-sequence-length 5, :max-sequence-length 5}))))
