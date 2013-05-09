(define (domain shrdlu)
  (:requirements :strips :equality :adl)

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
               (above ?x ?y))      ;; 'y' is somewhere under 'x'

  ;; pick up object that is in a box
  (:action pick-in
   :parameters   (?obj ?from ?box ?col)
   :precondition (and (not (holding-any))
                      (not (frozen))
                      (clear ?obj)
                      (box ?box)
                      (inside ?obj ?box)
                      (stacked-on ?obj ?col)
                      (stacked-on ?box ?col)
                      (stacked-on ?from ?col)
                      (stacked-on ?col ?col)
                      (not (stacked-on ?obj ?obj)) ;; not the floor
                      (on ?obj ?from)
                      (inside ?obj ?box))
   :effect       (and (holding-any)
                      (holding ?obj)
                      (not (inside ?obj ?box))
                      (not (inside-any ?obj))
                      (clear ?from)))

  ;; pick up object NOT in a box
  (:action pick-on
   :parameters (?obj ?from ?col)
   :precondition (and (not (holding-any))
                      (not (frozen))
                      (not (inside-any ?obj))
                      (clear ?obj)
                      (stacked-on ?obj ?col)
                      (stacked-on ?from ?col)
                      (stacked-on ?col ?col)
                      (not (stacked-on ?obj ?obj)) ;; not the floor col != obj
                      (on ?obj ?from))
   :effect       (and (holding-any)
                      (holding ?obj)
                      (clear ?from)))

  ;; drop an object into a box
  (:action drop-in
   :parameters (?obj ?to ?box ?col)
   :precondition (and (holding ?obj)
                      (not (inside ?obj ?box))
                      (clear ?to)
                      (stacked-on ?to ?col)
                      (stacked-on ?box ?col)
                      (stacked-on ?col ?col)
                      (inside ?to ?box)
                      (smaller ?to ?obj))
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
                      (not (inside-any ?obj))
                      (clear ?to)
                      (stacked-on ?to ?col)
                      (stacked-on ?col ?col)
                      (smaller ?obj ?to))
   :effect       (and (not (holding-any))
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
)






