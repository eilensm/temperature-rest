(ns temperature-rest.core.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.middleware.json :as middleware]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [iota :as iota]))

(declare read-chart-data)
(declare read-chart-data-with-size)
(declare chartjs-data)
(def time-formatter
  (f/formatter "dd.MM.yyyy HH:mm:ss"))

(defroutes app-routes
  (context "/temperature-rest" [] (defroutes temperature-routes
                                    ;(GET  "/" [] (read-chart-data))
                                    (GET "/" request (read-chart-data-with-size request))
                               (route/resources "/"))))
(def app
  (-> (handler/api app-routes)
    (middleware/wrap-json-body)
    (middleware/wrap-json-response)))

(defn parse-line
  "Parsed eine Zeile der CSV-Datei, für ungültige Datensätze wird nil zurückgegeben"
  [line]
  (if (.startsWith line "Data not good")
    nil
    (let [parts (str/split line #"\|")]
      {:timestamp (parts 0) :temperature (parts 2) :humidity (parts 1)})))

(def filename "C:\\work\\clojure\\temperature-rest\\src\\temperature_rest\\core\\temperatures.csv")
#_(def filename "c:\\Users\\Marc Eilens\\Desktop\\temperatures-test.csv")

(defn read-data
  "Liest die Datensätze mit Temperatur und Feuchtigkeit aus einer csv Datei"
  []
  (with-open [rdr (io/reader filename)]
    (doall (remove nil? (mapv parse-line (line-seq rdr))))))

; test for reading the file with the iota library (but it doesn't seem to be really faster, perhaps the file of my test was not large enough)
(defn read-data-iota
  []
  (->> (iota/seq filename)
    (mapv parse-line)
    (remove nil?)))

(defn filter-days
  "Filtert aus den Daten die Daten für die letzten n Tage heraus"
  [data days]
  (filter #(t/after? (f/parse time-formatter (:timestamp %)) (t/ago (t/days days)))
          data))

(defn read-data2
  ; wie read-data nur mit thread-last macro
  []
  (with-open [rdr (io/reader filename)]
    (->> rdr
      (line-seq)
      (mapv parse-line)
      (remove nil?)
      (into []))))

(defn build-dataset
  "Baut ein dataset für das ChartJs Line-Chart auf"
  [data name dataset-options]
  (merge dataset-options {:label name :data (mapv read-string (map (keyword name) data))}))

(def humidity-options
  {:fillColor "rgba(220,220,220,0.2)"
   :strokeColor "rgba(220,220,220,1)"
   :pointColor "rgba(220,220,220,1)"
   :pointStrokeColor "#fff"
   :pointHighlightFill "#fff"
   :pointHighlightStroke "rgba(220,220,220,1)"})

(def temperature-options
  {:fillColor "rgba(151,187,205,0.2)"
   :strokeColor "rgba(151,187,205,1)"
   :pointColor "rgba(151,187,205,1)"
   :pointStrokeColor "#fff"
   :pointHighlightFill "#fff"
   :pointHighlightStroke "rgba(151,187,205,0.2)"})

(defn chartjs-data
  "Erstellt die Datenstruktur, die ChartJs als Json erwartet" 
  [data]
  {:labels (mapv :timestamp data)
   :datasets [(build-dataset data "humidity" humidity-options)
              (build-dataset data "temperature" temperature-options)]})

(defn avg
  "Ermittelt den Durchschnittswert für eine Liste von Zahlen"
  [operands]
  (/ (apply + operands)
     (count operands)))

(defn avg-time
  "Ermittelt für die Liste der Timestamps (als Strings) einen in der Mitte liegenden Timestamp (als String)"
  [timestamps]
  (let [first (->> timestamps
                (first)
                (f/parse time-formatter)
                (c/to-long))
        last (->> timestamps
                (last)
                (f/parse time-formatter)
                (c/to-long))
        middle (+ first (/ (- last first) 2))]
    (f/unparse time-formatter (c/from-long middle))))

(defn avg-data-list
  "Funktion zum Ermitteln von Durchschnittswerten für die Liste mit den Messwerten."
  [data]
  {:timestamp (avg-time  (map :timestamp data))
   :humidity (->> data
           (map :humidity)
           (map #(Double/parseDouble %))
           (avg)
           (bigdec)
           ((fn [x] (.setScale x 2 BigDecimal/ROUND_HALF_UP)))
           (.toString))
   :temperature (.toString (.setScale (bigdec (avg (map #(Double/parseDouble %) (map :temperature data)))) 2 BigDecimal/ROUND_HALF_UP))})

(defn plane-data
  "Funktion zum 'Glätten' der Daten. Aus n Datensätzen wird die gewünschte Anzahl max-count erzeugt, wobei Partitionen gebildet werden und für jede Partition Durchschnittswerte für die Messgrößen und den Zeitstempel berechnet werden."
  [data max-count]
  (let [size (count data)
        partition-size (int (/ size max-count))]
    (map avg-data-list (partition partition-size data))))

(defn read-chart-data
  "Funktion als Schnittstelle für den REST-Service, die die Auslieferung der Temperatur und Feuchtigkeitsdaten steuert"
  []
  (list (chartjs-data (plane-data (read-data) 40))))

(defn read-chart-data-with-size
  "Schnittstelle für den REST-Service mit Angabe der Anzahl zu liefernden Datensätze (optional) sowie der Anzahl der letzten n anzuzeigenden Tage bis heute (optional)"
  [request]
  (let [size (get-in request [:params :size])
        days (get-in request [:params :days])
        data (if (nil? days) (read-data) (filter-days (read-data) (read-string days)))]
    (->> (if (nil? size) 40 (read-string size))
      (plane-data data)
      (chartjs-data)
      (list))))

