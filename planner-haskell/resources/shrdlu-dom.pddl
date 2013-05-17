(define (domain shrdlu)
;;;; This is not needed for metric-ff
;;  (:requirements :strips :equality :adl)

  (:predicates (clear ?x)          ;; 'x' is top-most block
               (on ?x ?y)          ;; 'x' is on top of 'y'
               (box ?x)            ;; 'x' is a box
               (inside ?x ?y)      ;; 'x' is inside box 'y'
               (inside-any ?x)     ;; 'x' is inside some box
               (smaller ?x ?y)     ;; 'x' is smaller than 'y'
               (stacked-on ?x ?c)  ;; 'x' is stacked on column 'c'
               (holding-any)       ;; the arm is holding something
               (holding ?x)        ;; 'x' is up in the air
               (frozen)            ;; cant move anything after that
               (above ?x ?y)       ;; 'x' is somewhere above 'y'
               (under ?x ?y)       ;; 'x' is somewhere under 'y'
               (left-of ?x ?y)     ;; 'x' is somewhere left of 'y'
               (right-of ?x ?y)    ;; 'x' is somewhere right of 'y'
               (beside ?x ?y))     ;; 'x' is directly beside 'y'

  ;; pick up object that is in a box
  (:action pick-in
   :parameters   (?obj ?from ?box ?col)
   :precondition (and (not (holding-any))
                      (not (frozen))
                      (clear ?obj)
                      (box ?box)
                      (inside ?obj ?box)
                      (stacked-on ?obj ?col)
                      (stacked-on ?col ?col)
                      (not (stacked-on ?obj ?obj)) ;; not the floor
                      (on ?obj ?from)
                      (inside ?obj ?box))
   :effect       (and (holding-any)
                      (holding ?obj)
                      (not (on ?obj ?from))
                      (not (inside ?obj ?box))
                      (not (inside-any ?obj))
                      (not (stacked-on ?obj ?col))
                      (clear ?from)))

  ;; pick up object NOT in a box
  (:action pick-on
   :parameters (?obj ?from ?col)
   :precondition (and (not (holding-any))
                      (not (frozen))
                      (not (inside-any ?obj))
                      (clear ?obj)
                      (stacked-on ?obj ?col)
                      (stacked-on ?col ?col)
                      (not (stacked-on ?obj ?obj)) ;; not the floor col != obj
                      (on ?obj ?from))
   :effect       (and (holding-any)
                      (holding ?obj)
                      (not (on ?obj ?from))
                      (not (stacked-on ?obj ?col))
                      (clear ?from)))

  ;; drop an object into a box
  (:action drop-in
   :parameters (?obj ?to ?box ?col)
   :precondition (and (holding ?obj)
                      (holding-any)
                      (not (inside ?obj ?box))
                      (clear ?to)
                      (stacked-on ?to ?col)
                      (stacked-on ?box ?col)
                      (stacked-on ?col ?col)
                      (inside ?to ?box)
                      (smaller ?obj ?to))
   :effect       (and (not (holding-any))
                      (not (holding ?obj))
                      (inside ?obj ?box)
                      (inside-any ?obj)
                      (not (clear ?to))
                      (clear ?obj)
                      (stacked-on ?obj ?col)
                      (inside ?obj ?box)
                      (on ?obj ?to)))

  ;; drop an object onto another object NOT in a box
  (:action drop-on
   :parameters (?obj ?to ?col)
   :precondition (and (holding ?obj)
                      (holding-any)
                      (not (inside-any ?to))
                      (clear ?to)
                      (stacked-on ?to ?col)
                      (stacked-on ?col ?col)
                      (smaller ?obj ?to))
   :effect       (and (not (holding-any))
                      (not (holding ?obj))
                      (not (clear ?to))
                      (stacked-on ?obj ?col)
                      (on ?obj ?to)
                      (clear ?obj)))

  ;; set the flag that the object is above some other objects
  (:action set-above
   :parameters (?first ?second ?under)
   :precondition (and (on ?first ?second)
                      (above ?second ?under))
   :effect       (and (above ?first ?second)
                      (above ?first ?under)
                      (frozen)))

  ;; Set a flag that a 'first' is below or under 'second' and 'above'
  (:action set-under
   :parameters (?first ?second ?above)
   :precondition (and (on ?second ?first)
                      (under ?second ?above))
   :effect       (and (under ?first ?second)
                      (under ?first ?above)
                      (frozen)))

  (:action set-left-right
   :parameters (?x ?y ?left-floor ?right-floor)
   :precondition (and (stacked-on ?x ?left-floor)
                      (stacked-on ?y ?right-floor)
                      (right-of ?left-floor ?right-floor))
   :effect       (and (right-of ?x ?y)
                      (left-of  ?y ?x)
                      (frozen)))

  (:action set-beside
   :parameters (?x ?y ?left-floor ?right-floor)
   :precondition (and (stacked-on ?x ?left-floor)
                      (stacked-on ?y ?right-floor)
                      (beside ?left-floor ?right-floor))
   :effect       (and (beside ?x ?y)
                      (beside ?y ?x)
                      (frozen)))
)
