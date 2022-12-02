(ns new-day
  (:require
   [babashka.fs :as fs]
   [babashka.curl :as curl]
   [clojure.string :as str]))

(defn new-day
  {:org.babashka/cli {:coerce {:day :string
                               :year :string}}}
  [{:keys [year day]
    :or {year "22"}}]
  (let [new-year (str "aoc" year)
        new-day (str "day" day)
        day01 (slurp (fs/file "src" "aoc22" "day01.cljc"))
        replaced (str/replace day01 "aoc22" new-year)
        replaced (str/replace replaced "day01" new-day)
        token (slurp ".cookie")]
    (fs/create-dirs "src" new-year)
    (fs/create-dirs "resources" new-year)
    (let [new-day-filename (format "day%02d.txt" (parse-long day))
          new-day-resource (fs/file "resources" new-year new-day-filename)
          url (format "https://adventofcode.com/%d/day/%d/input" (+ 2000 (parse-long year)) (parse-long day))
          resp (curl/get url {:headers {"Cookie" (str "session=" (or token (System/getenv "AOC_TOKEN")))}})]
      (if-not (fs/exists? new-day-resource)
        (spit new-day-resource (:body resp))
        (println (format "Day %s/%s resource already exists." new-year new-day))))
    (let [new-day (fs/file "src" new-year (str new-day ".cljc"))]
      (if-not (fs/exists? new-day)
        (spit new-day replaced)
        (println (format "Day %s/%s already exists." new-year new-day))))))
