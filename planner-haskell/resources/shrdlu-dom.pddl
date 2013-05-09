(define (domain shrdlu)
  (:requirements :stripes)

  (predicates (clear ?x)          ;; 'x' is top-most block
              (on ?x ?y)          ;; 'x' is on top of 'y'
              (inside ?x ?y)      ;; 'x' is inside box 'y'
              (smaller ?x ?y)     ;; 'x' is smaller than 'y'
              (stacked-on ?x ?c)  ;; 'x' is stacked on column 'c'
              (holding-any)       ;; the arm is holding something
              (holding ?x))       ;; 'x' is up in the air

  ;; pick up object that is in a box
  (:action pick-in
   :parameters   (?obj ?from ?box ?col)
   :precondition (and (not holding-any)
                      (clear ?obj)
                      (stacked-on ?obj ?col)
                      (not (stacked-on ?obj ?obj)) ;; not the floor
                      (on ?obj ?from)
                      (inside ?obj ?box))
   :effect       (and (holding-any)
                      (holding ?obj)
                      (clear ?from)))

  ;; pick up object NOT in a box
  (:action pick-on
   :parameters (?obj ?from ?col)
   :precondition (and (not holding-any)
                      (clear ?obj)
                      (stacked-on ?obj ?col)
                      (not (stacked-on ?obj ?obj)) ;; not the floor col != obj
                      (on ?obj ?from))
   :effect       (and (holding-any)
                      (holding ?obj)
                      (clear ?from)))

  ;; drop an object into a box
  (:action drop-in
   :parameters (?obj ?to ?box ?col)
   :precondition (and (holding ?obj)
                      (clear ?to)
                      (stacked-on ?to ?col)
                      (inside ?to ?box)
                      (smaller ?to ?obj))
   :effect       (and (not holding-any)
                      (not (clear ?to))
                      (stacked-on ?obj ?col)
                      (inside ?obj ?box)
                      (clear ?obj)))

  ;; drop an object onto another object NOT in a box
  (:action drop-on
   :parameters (?obj ?to ?col)
   :precondition (and (holding ?obj)
                      (clear ?to)
                      (stacked-on ?to ?col)
                      (smaller ?obj ?to))
   :effect       (and (not (holding-any))
                      (not (clear ?to))
                      (stacked-on ?obj ?col)
                      (clear ?obj)))
)






