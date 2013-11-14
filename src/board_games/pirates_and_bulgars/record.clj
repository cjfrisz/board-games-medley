;;----------------------------------------------------------------------
;; File record.clj
;; Written by Chris Frisz
;; 
;; Created  3 Nov 2013
;; Last modified 13 Nov 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns board-games.pirates-and-bulgars.record
  (:require [clojure.string :as string]))

(defn make-factory-name
  [name]
  (symbol 
   (str "make" 
     (string/lower-case (string/replace name #"([A-Z])" "-$1")))))

(defmacro defrecord+
  [name field* & spec*]
  `(do
     (defrecord ~name ~(vec field*) ~@spec*)
     (defn ~(make-factory-name name)
       ~(vec field*)
       (~(symbol (str name ".")) ~@field*))
     ~@(for [field field*]
         `(def ~(symbol (str "get-" field)) ~(keyword field)))
     ~@(for [field field*]
         `(defn ~(symbol (str "update-" field))
            [record# new-val#]
            (assoc record# ~(keyword field) new-val#)))))
