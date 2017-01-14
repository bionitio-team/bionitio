(ns biotool.core
  (:gen-class)
  (:require [clj-biosequence.core :as bs]))

(defn -main
  "Prints the first sequence from a fasta file"
  [file]
  (with-open [reader (bs/bs-reader (bs/init-fasta-file file :iupacAminoAcids))]
    (doseq [row (bs/biosequence-seq reader)]
      (println (count (:sequence row))))))
