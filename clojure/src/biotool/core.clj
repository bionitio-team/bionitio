(ns biotool.core
  (:gen-class)
  (:require [clj-biosequence.core :as bs]))

(def initial_stats
  "Initial value of stats, before any sequences have been considered"
  {:num_sequences 0
   :num_bases 0
   :min_sequence_length nil
   :max_sequence_length nil})

(defn update_stats
  "Update stats accumulator with information from the next sequence"
  [stats record]
  (let [{:keys [num_sequences num_bases min_sequence_length max_sequence_length]} stats
        sequence (:sequence record)
        sequence_length (count sequence)]
       {:num_sequences (inc num_sequences) 
        :num_bases (+ num_bases sequence_length)
        :min_sequence_length (if min_sequence_length
                                 (min min_sequence_length sequence_length)
                                 sequence_length)
        :max_sequence_length (if max_sequence_length
                                 (max max_sequence_length sequence_length)
                                 sequence_length)}))

(def header "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX")

(defn print_results
  "Display the computed stats in pretty fashion on the standard output"
  [filename stats]
  (println header)
  (let [{:keys [num_sequences num_bases min_sequence_length max_sequence_length]} stats
        average (if (> num_sequences 0)
                    (double (/ num_bases num_sequences))
                    nil)
        average_str (if average (str average) "-")
        num_sequences_str (str num_sequences)
        num_bases_str (str num_bases)
        min_sequence_length_str
          (if min_sequence_length (str min_sequence_length) "-")
        max_sequence_length_str
          (if max_sequence_length (str max_sequence_length) "-")
        output_str (str filename "\t"
                        num_sequences_str "\t"
                        num_bases_str "\t"
                        min_sequence_length_str "\t"
                        average_str "\t"
                        max_sequence_length_str)]
  (println output_str)))

(defn -main
  "Orchestrate the computation" 
  [file]
  (with-open [reader (bs/bs-reader (bs/init-fasta-file file :iupacAminoAcids))]
    (print_results file
      (reduce
        update_stats
        initial_stats 
        (bs/biosequence-seq reader)))))
