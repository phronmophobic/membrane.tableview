(ns com.phronemophobic.membrane.tableview
 (:require [membrane.ui :as ui]
           [membrane.basic-components :as basic]
           [membrane.alpha.stretch :as stretch]
           [membrane.component :as component
            :refer [defui
                    defeffect]]))

(defn truncate [s n]
  (if (> (count s) n)
    (subs s 0 n)
    s))

;; * table description
;; - cell content
;; - if extra space: left, center, right, top, bottom, etc
;; - cell background
;; - header content
;; - overflow-x, overflow-y
;;   - hide
;;   - scroll
;;   - page
;; - padding
;; - sort
;; - grid-width
;; - grid-height

;; :cell-ground
;; (fn [cell [w h]]) can handle foreground, background and alignment

;; :cell-width, :cell-height
;; (fn [cell-contents]
;;    (fn [colnum] col-width))


(defn tableview-content [data ccontent]
  (into []
        (map (fn [row]
               (into []
                     (map ccontent)
                     row)))
        data))


(defn cell-mouse-position [[row-count col-count] widths heights [mx my]]
  (let [[my row]
        (loop [rownum 0
               my my]
          (if (>= rownum row-count)
            nil
            (let [row-height (heights rownum)]
              (if (< my row-height)
                [my rownum]
                (recur (inc rownum)
                       (- my row-height))))))

        [mx col]
        (loop [colnum 0
               mx mx]
          (if (>= colnum col-count)
            nil
            (let [col-width (widths colnum)]
              (if (< mx col-width)
                [mx colnum]
                (recur (inc colnum)
                       (- mx col-width))))))]
    [[row col] [mx my]]))

(defn table-mouse-events [cells widths heights]
  {:mouse-event
   (fn [mpos button mouse-down? mods]
     (let [[[row col] mpos] (cell-mouse-position [(count cells)
                                                            (count (first cells))]
                                                           widths heights mpos)]
       (when (and col row)
         (ui/mouse-event (-> cells
                             (nth row)
                             (nth col))
                         mpos
                         button
                         mouse-down?
                         mods))))
   :mouse-move
   (fn [mpos]
     (let [[[row col] mpos]
           (cell-mouse-position [(count cells)
                                 (count (first cells))]
                                widths heights mpos)]
       (when (and col row)
         (ui/mouse-move (-> cells
                            (nth row)
                            (nth col))
                        mpos))))})

(defn tableview [data cground ccontent cwidth cheight]
  (let [contents (tableview-content data ccontent)
        
        widths (cwidth contents)
        heights (cheight contents)
        ;; need to separate out cell for mouse events
        cells
        (into []
         (map-indexed
          (fn [rownum row-contents]
            (into
             []
             (map-indexed
              (fn [colnum cell]
                (let [cw (widths colnum)
                      ch (heights rownum)]
                  (cground cell [cw ch]))))
             row-contents)))
         contents)

        table
        (stretch/vlayout
         (map stretch/hlayout)
         cells)
        handlers (table-mouse-events cells widths heights)
        ]
    (ui/on
     :mouse-event
     (:mouse-event handlers)
     :mouse-move
     (:mouse-move handlers)
     table)))

(comment

  (def table-data
    (->> deps-libs
         vals
         (map (juxt  :lib
                     :description
                     :stars
                     :url))
         (take 100)))

  (def table-data3
    (->> deps-libs
         vals
         (map (juxt  :lib
                     :description
                     :stars
                     :url))
         (take 100)))


  ,)


(defn max-col-width [cell-contents]
  (into []
        (map (fn [i]
               (transduce
                (comp (map #(nth % i))
                      (map ui/width))
                max
                0
                cell-contents)))
        (range (count (first cell-contents)))))

(defn max-row-height [cell-contents]
  (into []
        (map (fn [row]
               (transduce
                (map ui/height)
                max
                0
                row)))
        cell-contents))

(defn label-content [datum]
  (with-meta
    (ui/label (truncate (pr-str datum)
                        20))
    {:datum datum}))


(defn pad [sizer p]
  (fn [cells]
    (let [sizes (sizer cells)]
      (fn [i]
        (+ p (sizes i))))))




