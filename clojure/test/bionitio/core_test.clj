;;; Unit tests for bionitio.
;;;
;;; Usage: lein test

(ns bionitio.core-test
  (:require [clojure.test :refer :all]
            [clj-biosequence.core :as bs]
            [clojure.java.io :as io]
            [bionitio.core :refer :all]))

; Convenience wrapper for testing process-fasta-reader
(defn do-test
  [input minlen expected]
  (is (= (process-fasta-reader minlen
           (bs/init-fasta-reader
             (java.io.BufferedReader.
               (java.io.StringReader. input))
           :iupacAminoAcids))
         expected)))

(deftest test-zero-byte-input 
  (testing "Test input containing zero bytes"
    (do-test "" 0 
      {:num-sequences 0,
       :num-bases 0,
       :min-sequence-length nil,
       :max-sequence-length nil})))

(deftest test-single-newline-input 
  (testing "Test input containing a newline (\n) character"
    (do-test "\n" 0 
      {:num-sequences 0,
       :num-bases 0,
       :min-sequence-length nil,
       :max-sequence-length nil})))

;; NB: this is different than the behaviour we get in Python, for example
;; It is not clear what the "correct" thing to do in this case is, because
;; it might be considered an invalid input file
(deftest test-single-greater-than-input 
  (testing "Test input containing a single greater-than (>) character"
    (do-test ">" 0 
      {:num-sequences 0,
       :num-bases 0,
       :min-sequence-length nil,
       :max-sequence-length nil})))

(deftest test-one-sequence
  (testing "Test input containing one sequence"
    (do-test ">header\nATGC\nA" 0 
      {:num-sequences 1,
       :num-bases 5,
       :min-sequence-length 5,
       :max-sequence-length 5})))

(deftest test-two-sequences
  (testing "Test input containing two sequences"
    (do-test ">header1\nATGC\nAGG\n>header2\nTT\n" 0 
      {:num-sequences 2,
       :num-bases 9,
       :min-sequence-length 2,
       :max-sequence-length 7})))

(deftest test-no-header
  (testing "Test input containing sequence without preceding header"
    (do-test "ATGC\n" 0 
      {:num-sequences 0,
       :num-bases 0,
       :min-sequence-length nil,
       :max-sequence-length nil})))

(deftest test-minlen-less-than-all
  (testing "Test input when --minlen is less than 2 out of 2 sequences"
    (do-test ">header1\nATGC\nAGG\n>header2\nTT\n" 2 
      {:num-sequences 2,
       :num-bases 9,
       :min-sequence-length 2,
       :max-sequence-length 7})))

(deftest test-minlen-greater-than-one
  (testing "Test input when --minlen is less than 1 out of 2 sequences"
    (do-test ">header1\nATGC\nAGG\n>header2\nTT\n" 3 
      {:num-sequences 1,
       :num-bases 7,
       :min-sequence-length 7,
       :max-sequence-length 7})))

(deftest test-minlen-greater-than-all
  (testing "Test input when --minlen is greater than 2 out of 2 sequences"
    (do-test ">header1\nATGC\nAGG\n>header2\nTT\n" 8 
      {:num-sequences 0,
       :num-bases 0,
       :min-sequence-length nil,
       :max-sequence-length nil})))
