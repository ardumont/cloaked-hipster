(ns crypto.jce
  "Some stuff around jce"
  (:import [java.security Provider]
           [java.security Security]))

(defn- compute-provider-and-algorithms
  []
  (mapcat
   (fn [provider]
     (let [provider-name     (.getName provider)
           provider-services (.getServices provider)]
       (map
        (fn [service]
          (let [algo (.getAlgorithm service)]
            [provider-name algo]))
        provider-services)))
   (Security/getProviders)))

(defn providers
  [provs-vec]
  (reduce
   (fn [m [provider-name algo]]
     (update-in m [(keyword provider-name)] conj algo))
   {}
   provs-vec))

(def provs (providers (compute-provider-and-algorithms)))
