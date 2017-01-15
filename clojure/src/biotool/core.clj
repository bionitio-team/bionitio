(ns biotool.core
  (:require [clj-biosequence.core :as bs]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def initial-stats
  "Initial value of stats, before any sequences have been considered"
  {:num-sequences 0
   :num-bases 0
   :min-sequence-length nil
   :max-sequence-length nil})

(defn update-stats
  "Update stats accumulator with information from the next sequence"
  [stats record]
  (let [{:keys [num-sequences num-bases min-sequence-length max-sequence-length]} stats
        sequence (:sequence record)
        sequence-length (count sequence)]
       {:num-sequences (inc num-sequences) 
        :num-bases (+ num-bases sequence-length)
        :min-sequence-length (if min-sequence-length
                                 (min min-sequence-length sequence-length)
                                 sequence-length)
        :max-sequence-length (if max-sequence-length
                                 (max max-sequence-length sequence-length)
                                 sequence-length)}))

(defn print-results
  "Display the computed stats in pretty fashion on the standard output"
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
        output-str (str filename "\t"
                        num-sequences-str "\t"
                        num-bases-str "\t"
                        min-sequence-length-str "\t"
                        average-str "\t"
                        max-sequence-length-str)]
  (println output-str)))

(defn length-gte-minlen?
  [minlen record]
  (let [sequence (:sequence record)
        num-bases (count sequence)]
    (>= num-bases minlen)))

(defn process-fasta-reader
  [minlen reader filename]
    (print-results filename
      (reduce
        update-stats
        initial-stats 
        (filter
          (fn [record] (length-gte-minlen? minlen record))
          (bs/biosequence-seq reader)))))

(defn process-fasta-file 
  [minlen file]
  (with-open [reader (bs/bs-reader (bs/init-fasta-file file :iupacAminoAcids))]
    (process-fasta-reader minlen reader file)))

(defn process-stdin
  [minlen]
  (with-open [reader (bs/init-fasta-reader (java.io.BufferedReader. *in*) :iupacAminoAcids)]
    (process-fasta-reader minlen reader "stdin")))

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

(defn -main
  "Orchestrate the computation" 
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    (process-fasta-files (:minlen options) arguments)))
