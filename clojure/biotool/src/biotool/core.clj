(ns biotool.core
  (:gen-class)
  (:require [clj-biosequence.core :as bs]))

(defn -main
  "Prints the first sequence from a fasta file"
  [file]
  (with-open [r (bs/bs-reader (bs/init-fasta-file file :iupacAminoAcids))]
    (println
     (bs/bioseq->string (first (bs/biosequence-seq r))))))
