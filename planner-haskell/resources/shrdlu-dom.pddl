(define (domain shrdlu)
  (:functions (moves))

  (:predicates (clear ?x)          ;; 'x' is top-most block
               (on ?x ?y)          ;; 'x' is on top of 'y'
               (box ?x)            ;; 'x' is a box
               (inside ?x ?y)      ;; 'x' is inside box 'y'
               (smaller ?x ?y)     ;; 'x' is smaller than 'y'
               (stacked-on ?x ?c)  ;; 'x' is stacked on column 'c'
               (holding-any)       ;; the arm is holding something
               (holding ?x)        ;; 'x' is up in the air
               (frozen ?x)         ;; 'x' is frozen and cant be moved
               (above ?x ?y)       ;; 'x' is somewhere above 'y'
               (under ?x ?y)       ;; 'x' is somewhere under 'y'
               (left-of ?x ?y)     ;; 'x' is somewhere left of 'y'
               (right-of ?x ?y)    ;; 'x' is somewhere right of 'y'
               (beside ?x ?y))     ;; 'x' is directly beside 'y'

  ;; pick up object
  (:action pick
   :parameters (?obj ?from ?col)
   :precondition (and (not (frozen ?obj))
                      (not (holding-any))
                      (clear ?obj)
                      (on ?obj ?from)
                      (stacked-on ?obj ?col))
   :effect       (and (holding-any)
                      (holding ?obj)
                      (clear ?from)
                      (not (on ?obj ?from))
                      (not (stacked-on ?obj ?col))
                      (increase (moves) 1)))

  ;; drop an object
  (:action drop
   :parameters (?obj ?to ?col)
   :precondition (and (holding ?obj)
                      (clear ?to)
                      (smaller ?obj ?to)
                      (stacked-on ?to ?col))
   :effect       (and (not (holding-any))
                      (not (holding ?obj))
                      (not (clear ?to))
                      (on ?obj ?to)
                      (stacked-on ?obj ?col)
                      (increase (moves) 1)))

  ;; set the flag that the object is above some other objects
  (:action set-above
   :parameters (?first ?second ?under)
   :precondition (and (on ?first ?second)
                      (above ?second ?under))
   :effect       (and (above ?first ?second)
                      (above ?first ?under)
                      (frozen ?first)))

  ;; Set a flag that a 'first' is below or under 'second' and 'above'
  (:action set-under
   :parameters (?first ?second ?above)
   :precondition (and (on ?second ?first)
                      (under ?second ?above))
   :effect       (and (under ?first ?second)
                      (under ?first ?above)
                      (frozen ?above)))

  ;; `x' and `y' are left- and right of each other if `x' is stacked
  ;; on a floor column to the left of `y'
  (:action set-left-right
   :parameters (?x ?y ?left-floor ?right-floor)
   :precondition (and (stacked-on ?x ?left-floor)
                      (stacked-on ?y ?right-floor)
                      (right-of ?left-floor ?right-floor))
   :effect       (and (right-of ?x ?y)
                      (left-of  ?y ?x)
                      (frozen ?x)
                      (frozen ?y)))

  ;; `x' and `y' are beside each other if `x' is stacked on a floor
  ;; column beside of `y'
  (:action set-beside
   :parameters (?x ?y ?left-floor ?right-floor)
   :precondition (and (stacked-on ?x ?left-floor)
                      (stacked-on ?y ?right-floor)
                      (beside ?left-floor ?right-floor))
   :effect       (and (beside ?x ?y)
                      (beside ?y ?x)
                      (frozen ?x)
                      (frozen ?y)))

  ;; `x' is inside `box' if `y' is inside `box' and `x' is on `y'
  (:action set-inside
   :parameters (?x ?y ?box)
   :precondition (and (inside ?y ?box)
                      (on ?x ?y))
                      ;; ;; Removed this for the time being. I.e. atm
                      ;; ;; objects can be inside many boxes.
                      ;; (inside ?y ?y) (not (box ?y))))
   :effect       (and (inside ?x ?box)
                      (frozen ?x)))

)
