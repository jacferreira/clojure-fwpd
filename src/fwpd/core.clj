;; In ns below, notice that "gen-class" was removed
(ns fwpd.core
  ;; We haven't gone over require but we will.
  (:require [clojure.string :as s]))

(def filename "resources/suspects.csv")
(def save-filename "resources/new-suspects.csv")

;; Later on we're going to be converting each row in the CSV into a
;; map, like {:name "Edward Cullen" :glitter-index 10}.
;; Since CSV can't store Clojure keywords, we need to associate the
;; textual header from the CSV with the correct keyword.
(def headers->keywords {"Name" :name
                        "Glitter Index" :glitter-index})

(def keywords->headers {:name "Name"
                        :glitter-index "Glitter Index"})

(defn str->int
  [str]
  (Integer. str))

;; CSV is all text, but we're storing numeric data. We want to convert
;; it back to actual numbers.
(def conversions {:name identity
                  :glitter-index str->int})

(def validations {:name string?
                  :glitter-index integer?})

(defn parse
  "Convert a csv into rows of columns"
  [string]
  (map #(s/split % #",")
       (s/split string #"\r\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (let [;; headers becomes the seq (:name :glitter-index)
        headers (map #(get headers->keywords %) (first rows))
        ;; unmapped-rows becomes the seq
        ;; (["Edward Cullen" "10"] ["Bella Swan" "0"] ...)
        unmapped-rows (rest rows)]
    ;; Now let's return a seq of {:name "X" :glitter-index 10}
    (map (fn [unmapped-row]
           ;; We're going to use map to associate each header with its
           ;; column. Since map returns a seq, we use "into" to convert
           ;; it into a map.
           (into {}
                 ;; notice we're passing multiple collections to map
                 (map (fn [header column]
                        ;; associate the header with the converted column
                        [header ((get conversions header) column)])
                      headers
                      unmapped-row)))
         unmapped-rows)))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn vampire-names [minimum-glitter records]
  (map (fn [record]
         (:name record))
       (glitter-filter minimum-glitter records)))

(defn prepend [suspect suspects-list]
  (conj suspects-list suspect))

(defn validate [validations suspect]
  (and ((get validations :name) (:name suspect))
       ((get validations :glitter-index) (:glitter-index suspect))))

(defn suspect-to-string [suspect]
  (str (:name suspect) "," (:glitter-index suspect)))

(defn write [suspects]
  (spit save-filename
        (clojure.string/join
         "\r\n"
         (conj (map suspect-to-string suspects)
               (str (:name keywords->headers) "," (:glitter-index keywords->headers))))))
