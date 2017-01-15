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
  [old_stats this_record]
  (let [old_num_sequences (old_stats :num_sequences)
        old_num_bases (old_stats :num_bases)
        old_min_sequence_length (old_stats :min_sequence_length)
        old_max_sequence_length (old_stats :max_sequence_length)
        this_sequence (:sequence this_record)
        this_sequence_length (count this_sequence)]
       {:num_sequences (inc old_num_sequences) 
        :num_bases (+ old_num_bases this_sequence_length)
        :min_sequence_length (if old_min_sequence_length
                                 (min old_min_sequence_length this_sequence_length)
                                 this_sequence_length)
        :max_sequence_length (if old_max_sequence_length
                                 (max old_max_sequence_length this_sequence_length)
                                 this_sequence_length)}))

(def header "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX")

(defn print_results
  "Display the computed stats in pretty fashion on the standard output"
  [filename stats]
  (println header)
  (let [this_num_sequences (:num_sequences stats)
        this_num_bases (:num_bases stats)
        this_min_sequence_length (:min_sequence_length stats)
        this_max_sequence_length (:max_sequence_length stats)
        average (if (> this_num_sequences 0)
                    (double (/ this_num_bases this_num_sequences))
                    nil)
        average_str (if average (str average) "-")
        num_sequences_str (str this_num_sequences)
        num_bases_str (str this_num_bases)
        min_sequence_length_str
          (if this_min_sequence_length
              (str this_min_sequence_length)
              "-")
        max_sequence_length_str
          (if this_max_sequence_length
              (str this_max_sequence_length)
              "-")
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
