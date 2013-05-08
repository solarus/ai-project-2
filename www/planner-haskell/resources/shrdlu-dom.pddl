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
                      (not (clear ?obj))
                      (not (on ?obj ?col))
                      (not (inside ?obj ?box))
                      (not (stacked-on ?obj ?col))
                      (clear ?from)))

  ;; pick up object NOT in a box
  (:action pick-on
   :parameters (?obj ?from ?col)
   :precondition (and (not holding-any)
                      (clear ?obj)
                      (stacked-on ?obj ?col)
                      (not (stacked-on ?obj ?obj)) ;; not the floor
                      (on ?obj ?from))


    )
   :effect())

  ;; drop an object into a box
  (:action drop-in
   :parameters ()
   :precondition ()
   :effect())

  ;; drop an object onto another object NOT in a box
  (:action drop-on
   :parameters ()
   :precondition ()
   :effect())
)
