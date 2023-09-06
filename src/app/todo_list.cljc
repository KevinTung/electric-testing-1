(ns app.todo-list
  #?(:cljs (:require-macros [app.todo-list :refer [with-reagent]]))
  (:require contrib.str
            #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            
            [contrib.missionary-contrib :as mx]
            [missionary.core :as m]
              #?(:cljs ["react" :as react])
            #?(:cljs ["slate" :refer [createEditor]])
            #?(:cljs ["slate-react" :refer [Slate Editable withReact]])
            #?(:cljs [reagent.core :as r])
            #?(:cljs ["react-dom/client" :as ReactDom])
            )
  

  )



#?(:clj (defonce !conn (d/create-conn {}))) ; database on server
(e/def db) ; injected database ref; Electric defs are always dynamic


#?(:cljs (defn create-root
           "See https://reactjs.org/docs/react-dom-client.html#createroot"
           ([node] (create-root node (str (gensym))))
           ([node id-prefix]
            (ReactDom/createRoot node #js {:identifierPrefix id-prefix}))))
#?(:cljs (defn render [root & args]
           (.render root (r/as-element (into [:f>] args)))))
(defmacro with-reagent [& args]
  `(dom/div  ; React will hijack this element and empty it.
    (let [root# (create-root dom/node)]
      (render root# ~@args)
      (e/on-unmount #(.unmount root#)))))

(defn get-text [editor] (.-text (get (.-children (get (.-children editor) 0)) 0)))
(defn block [data on-change]
  #?(:cljs
     (let [[editor] (react/useState (fn [] (withReact (createEditor))))]
       [:> Slate
        {:editor editor
         :initialValue [{:children
                         [{:text (:value data)}],
                         :type "paragraph"}]
         ;; :onChange on-change also works
         :onChange (fn [value]
                     (println "onChange:" (-> value (get 0) (.-children) (get 0) (.-text)) ".  Not a plain function")
                     (on-change value))}
        [:> Editable
         {:onKeyDown (fn [e]
                      
                       (when (= (.-key e) "Enter")
                         (println "onKeyDown: Enter, A plain function. ")
                         
                         )
                      
                       )}]])))

(e/defn TodoItem [id]
  (e/server
    (let [e (d/entity db id)
          status (:task/status e)]
      (e/client
        (dom/div
          (ui/checkbox
            (case status :active false, :done true)
            (e/fn [v]
              (e/server
                (d/transact! !conn [{:db/id id
                                     :task/status (if v :done :active)}])
                nil))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

(e/defn InputSubmit [F]
  ; Custom input control using lower dom interface for Enter handling
  (dom/input (dom/props {:placeholder "Buy milk"})
    (dom/on "keydown" (e/fn [e]
                        (when (= "Enter" (.-key e))
                          (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                            (new F v)
                            (set! (.-value dom/node) "")))))))

(e/defn TodoCreate []
  (e/client
    (let [on-change (m/mbx)]
      (with-reagent block {:uid "test1-uid" :value "test1"} on-change)
      (let [[state v] (e/for-event-pending-switch [v (m/relieve {} (mx/poll-task on-change))]
                        (e/server (prn :onChange v)))]
        (case state (::e/pending ::e/failed) (throw v) (::e/init ::e/ok) v)))))

#?(:clj (defn todo-count [db]
          (count
            (d/q '[:find [?e ...] :in $ ?status
                   :where [?e :task/status ?status]] db :active))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id :task/description]) ...]
                      :where [?e :task/status]] db)
            (sort-by :task/description))))

(e/defn Todo-list []
  (e/server
    (binding [db (e/watch !conn)]
      (e/client
        (dom/link (dom/props {:rel :stylesheet :href "/todo-list.css"}))
        (dom/h1 (dom/text "minimal todo list"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (dom/div (dom/props {:class "todo-list"})
          (TodoCreate.)
          (dom/div {:class "todo-items"}
            (e/server
              (e/for-by :db/id [{:keys [db/id]} (todo-records db)]
                (TodoItem. id))))
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
            (dom/text " items left")))))))