;;; Module:      biotool.core
;;; Description: Reads one or more FASTA files and computes various
;;;              simple statistics about them. Intended as an example
;;;              bioinformatics command line tool 
;;; Copyright:   (c) Bernie Pope, 2017 
;;; License:     MIT 
;;; Maintainer:  bjpope@unimelb.edu.au
;;; Stability:   stable 
;;; Portability: POSIX

;;; The main parts of the program are:
;;; 1. Parse command line arguments. 
;;; 2. Process each FASTA file in sequence
;;; 3. Pretty print output for each file. 
;;;
;;; If no FASTA filenames are specified on the command line then the program
;;; will try to read a FASTA file from standard input.

(ns biotool.core
  (:require [clj-biosequence.core :as bs]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders])
  (:gen-class))

;; Set up logging infrastructure
(timbre/refer-timbre)

(def initial-stats
  "Initial value of stats, before any sequences have been considered"
  {:num-sequences 0
   :num-bases 0
   :min-sequence-length nil
   :max-sequence-length nil})

(defn update-stats
  "Update stats accumulator with information from the next sequence.

  Arguments:
    stats: The current value of the stats mapping.
    record: The next FASTA record from the input file.

  Result:
    A new stats mapping updated to reflect the input FASTA record"

  [stats record]
  (let [{:keys [num-sequences num-bases min-sequence-length max-sequence-length]} stats
        sequence (:sequence record)
        sequence-length (count sequence)]
    {:num-sequences (inc num-sequences)
     :num-bases (+ num-bases sequence-length)
        ;; If min-sequence-length is nil then the current sequence
        ;; is the first we have seen, and thus the shortest.
     :min-sequence-length (if min-sequence-length
                            (min min-sequence-length sequence-length)
                            sequence-length)
        ;; If max-sequence-length is nil then the current sequence
        ;; is the first we have seen, and thus the longest.
     :max-sequence-length (if max-sequence-length
                            (max max-sequence-length sequence-length)
                            sequence-length)}))

(defn print-results
  "Display the computed stats in pretty fashion on the standard output.

  Arguments:
    filename: The string name of the input FASTA file (could also be stdin)
    stats: The final computed value of stats to print.

  Result:
    nil

  Notes:
    - If the number of sequences seen is at least one then we also compute
      the average sequence length and print it out.
    - If any computed stat is nil then we print a single dash (-) in its place."

  [filename stats]
  (let [{:keys [num-sequences num-bases min-sequence-length max-sequence-length]} stats
        average (if (> num-sequences 0)
                  (int (Math/floor (double (/ num-bases num-sequences))))
                  nil)
        average-str (if average (str average) "-")
        num-sequences-str (str num-sequences)
        num-bases-str (str num-bases)
        min-sequence-length-str
        (if min-sequence-length (str min-sequence-length) "-")
        max-sequence-length-str
        (if max-sequence-length (str max-sequence-length) "-")
        ;; Output fields are tab separated.
        output-str (str filename "\t"
                        num-sequences-str "\t"
                        num-bases-str "\t"
                        min-sequence-length-str "\t"
                        average-str "\t"
                        max-sequence-length-str)]
    (println output-str)))

(defn seq-length-gte-minlen?
  "Test if the length of a sequence is >= to a minimum value.

  Arguments:
    minlen: The minimum length sequence allowed (non-negative int).
    record: A FASTA record.

  Result:
    true if the sequence length is >= minlen, false otherwise."

  [minlen record]
  (let [sequence (:sequence record)
        num-bases (count sequence)]
    (>= num-bases minlen)))

(defn process-fasta-reader
  "Compute the statistics for a single input FASTA file from a reader object,
  and pretty print the result to the standard output.

  Arguments:
    minlen: The minimum length sequence allowed (non-negative int). Sequences shorter than
      this will be skipped by the program.
    filename: The name of the file being processed, printed in the output.
    reader: The reader object representing the input FASTA file contents.

  Result:
    nil"

  [minlen filename reader]
  (print-results filename
                 (reduce
                  update-stats
                  initial-stats
                  (filter
                   (fn [record] (seq-length-gte-minlen? minlen record))
                   (bs/biosequence-seq reader)))))

(defn process-fasta-file
  [minlen filename]
  (timbre/info "Processing FASTA file from" filename)
  (with-open [reader (bs/bs-reader (bs/init-fasta-file filename :iupacAminoAcids))]
    (process-fasta-reader minlen filename reader)))

(defn process-stdin
  [minlen]
  (timbre/info "Processing FASTA file from stdin")
  (with-open [reader (bs/init-fasta-reader (java.io.BufferedReader. *in*) :iupacAminoAcids)]
    (process-fasta-reader minlen "stdin" reader)))

(def header "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX")

(defn process-fasta-files
  [minlen files]
  (println header)
  (if (empty? files)
    (process-stdin minlen)
    (doseq [file files]
      (process-fasta-file minlen file))))

(defn usage [options-summary]
  (->> ["Print fasta stats"
        ""
        "Usage: program-name [options] FILES"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(def default-minlen 0)

(def cli-options
  [[nil "--minlen N" "Minimum length sequence to include in stats"
    :default default-minlen :parse-fn #(Integer/parseInt %)]
   [nil "--log LOG-FILE" "record program progress in LOG-FILE"]
   ["-v" "--version"]
   ["-h" "--help"]])

(defn config-logging
  [log-filename]
  (when log-filename
    (let [log-file-name log-filename]
      (timbre/merge-config! {:appenders {:println {:enabled? false}}})
      (timbre/merge-config! {:appenders {:spit (appenders/spit-appender {:fname log-filename})}})
      (timbre/set-level! :info))
    (timbre/info "Program started")))

(defn -main
  "Orchestrate the computation"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    (config-logging (:log options))
    (process-fasta-files (:minlen options) arguments)))
