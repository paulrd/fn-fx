(ns fn-fx.app-test
  (:require [clojure.test :as t]
            [fn-fx.fx-dom :as dom]
            [fn-fx.set-once-type :refer [defquasitype set-once!]]
            [fn-fx.util.reflect-utils :as ru]
            [fn-fx.render-core :refer [get-getter] :as rc]
            [fn-fx.controls :as ui]
            [fn-fx.diff :refer [component defui render should-update?]]
            [fn-fx.diff :as diff])
  (:import [javafx.scene.text Text TextAlignment]
           [javafx.geometry VPos]
           [javafx.scene.paint Color]
           (java.lang.reflect Constructor Method Parameter Modifier Field)
           ))

(defn get-prop [comp k]
  {:pre [comp k]}
  (let [getter (get-getter (type comp) k)]
    (assert getter)
    (let [v (getter comp)]
      (if (delay? v)
        @v
        v))))

(def c
  (ui/text :text "hi"
           :fill
           (ui/color :red 0.69 :green 0.13 :blue 0.13)
           #_(ui/color :red 100 :green 100  :blue 100)))

(def a (dom/app c))
(def b
  (doto (Text. "hi") (.setFill (Color/rgb 187 195 107))))

(def d @(:root a))

(comment
  d ;This object has :fill with an improper color property
  b ;This has the proper color property

  ;;I need to trace what happens to this (ui/text) component when it gets
  ;; transformed. by dom/app
  c
  ;; The above in an fn_fx.diff.Component object
  (:props c )
  (type c)
  ;; fn-fx.diff/->Component
  ;;it has :props with a :text and :fill attribute. We want to see what happens
  ;; to these attributes when they they get converted to objects by dom/app

  ;; But before looking into that mystery, let's look into the Component object.
  ;; It does get some object initialization
  (def e @(:dom-node c))
  ;; by inspecting the above object, we see that there is a very similar object
  ;; in d:
  d
  (= d e) ; In fact they are the same object
  ;; So I only need to see how this object is made
  (macroexpand-1 '(ui/text :text "hi"
                           :fill (ui/color :red 0.69 :green 0.13 :blue 0.13)))
  (fn-fx.diff/component
   :javafx.scene.text.Text
   {:fill (ui/color :red 0.69 :green 0.13 :blue 0.13), :text "hi"})
  (def f
    (fn-fx.diff/->Component :javafx.scene.text.Text nil
                            {:fill (ui/color :red 0.69 :green 0.13 :blue 0.13), :text "hi"}))
  (def g
    (fn-fx.diff/->Component :javafx.scene.text.Text nil
                            {:fill (ui/color :red 100 :green 100 :blue 100), :text "hi"}))
  (:dom-node f)
  ;; Ahha! d and e were the same object 
  d>a>c ; d depends on a depends on c
  e>c ; e depends on c
  ;; But c was likely modified when it was used in (dom/app)
  ;; Let's see:
  (def c
    (ui/text :text "hi"
             :fill (ui/color :red 0.69 :green 0.13 :blue 0.13)))
  ;; :dom-node is nil
  (dom/app c)
  ;; :dom-node is populated
  ;; looking at c it seems that the wrong constructor may have been chosen but
  ;; but it's hard to know
  ;; :props is a map which contain :fill and :text. :fill is a Value with
  ;; :class-name :args :f
  ;; :f is fn_fx.render_core$get_value_ctors$iter__14725__14729$fn__14730$fn__14741
  ;; and is a method Coror.rgb(int int int)
  ;; so I need to see how the constructor is chosen (again)

  ;; it seems I may not need to see what dom/app does (yet!) as the constructor
  ;; gets selected as a Value before this. What is probably happening is that
  ;; the doubles get truncated to ints in the constructor. It should be choosing
  ;; the constructor with named values
  (macroexpand-1 '(ui/color :red 0.69 :green 0.13 :blue 0.13))
  (macroexpand '(ui/color :red 0.69 :green 0.13 :blue 0.13))
  (fn-fx.render-core/->Value
   javafx.scene.paint.Color
   [0.69 0.13 0.13]
   ((fn-fx.render-core/get-value-ctors javafx.scene.paint.Color)
    [:red :green :blue]))

  (def c ((fn-fx.render-core/get-value-ctors javafx.scene.paint.Color)
          [:red :green :blue]))
  (def d (fn-fx.render-core/get-value-ctors javafx.scene.paint.Color))
  (keys d)
  (count d)
  ;; there is a bug in get-value-ctors if as it seems to be returning duplicate
  ;; functions for various keys.
  (ru/get-value-ctors javafx.scene.paint.Color)

  (def ctors
    (for [{:keys [is-ctor? ^Executable method prop-names-kw prop-types]} (ru/get-value-ctors javafx.scene.paint.Color)]
      [(into prop-names-kw prop-types)
       (if is-ctor?
         (fn [args]
           (let [^objects casted (into-array Object (map rc/convert-value
                                                         args
                                                         prop-types))]
             (.newInstance ^Constructor method casted)))
         (fn [args]
           (let [^objects casted (into-array Object (map rc/convert-value
                                                         args
                                                         prop-types))]
             (.invoke ^Method method nil casted)))
         )])

    )
  (def sa [:a :c :f])
  (def sb [5 4 3])
  (type 0.0)
  (type 100)
  (def c (last (first ctors)))
  (def c (last (last ctors)))
  (fn? c)
  (def c
    (ru/get-value-ctors javafx.scene.paint.Color))
  (def f (.getFields (type c)))
  (parents c)
  (def r (:members (clojure.reflect/reflect c)))
  (def r (clojure.reflect/reflect c))
  (defn ff [m]
    (= (:name m) 'prop_types))
  (def pt (filter ff r))
  (require '[clojure.inspector :as i])
  (i/inspect c)
  (clojure.reflect/reflect (first r))
  ;; It seems that there are two constructors for :red :green :blue. One takes
  ;; doubles and one takes ints.
  ;; The bug is in the last line of get-value-ctors. It makes the list into a
  ;; map and so duplicate keys are dropped.
  ;; How to solve?

  ;; Also: why does using the int version of the constructor not work? In that case
  ;; :fill does not get set at all.

  ;; So I may need to see what dom/app is doing! Which case to look at?
  ;; Let's look at the 'buggy' code path first as it properly sets the
  ;; :fill property
  (def f
    (fn-fx.diff/->Component :javafx.scene.text.Text nil
                            {:fill (ui/color :red 0.69 :green 0.13 :blue 0.13), :text "hi"}))
  (def g
    (fn-fx.diff/->Component :javafx.scene.text.Text nil
                            {:fill (ui/color :red 100
                                             :green 100
                                             :blue 100),
                             :text "hi"}))
  (def fa (dom/app f)) ;fill is good
  (def fg (dom/app g)) ;fill is bad
  (def farf @(:root fa))
  (def fgrf @(:root fg))
  (def good (get farf "fill"))
  (def bad (get fgrf "fill"))
(.fill farf)
(.getFill fgrf)
(.setFill fgrf (Color/rgb 100 100 100))
(instance? Text farf)
(gefarf)

;; So it seems in 'good' setFill gets invoked while in 'bad' it does not.
(def f
  (fn-fx.diff/->Component :javafx.scene.text.Text nil
                          {:fill (ui/color :red 0.69 :green 0.13 :blue 0.13), :text "hi"}))
(def g
  (fn-fx.diff/->Component :javafx.scene.text.Text nil
                          {:fill (ui/color :red 100 :green 100 :blue 100), :text "hi"}))
;; Let's trace the code for creating the objects via dom/app
(def fa (dom/app f))
(def fg (dom/app g))

(def r
  (:node (diff/diff (dom/->FXDom dom/default-handler-fn) nil fg)))
(diff/val-type fa )
(def md (dom/->FXDom dom/default-handler-fn))
(def rn
  (fn [node compo-a compo-b]
    (set-once! compo-b :dom-node node)
    (diff/diff-component md node (:props compo-a) (:props compo-b))
    node)
  )
(def nn (fn [compo]
          (let [node (diff/create-component! md (:type compo))]
            (assert node "No Node returned by create-component!")
            (rn node nil compo))))
(diff/->Created (nn fa))
(nn fa)
(println "hi")
(type fa)
  )

