(define (problem shrdlu)
  (:domain shrdlu)
  (:objects s1 s2 b1 b2  f1 f2)
  (:init
    (smaller s1 f1)
    (smaller s1 f2)
    (smaller s2 f1)
    (smaller s2 f2)
    (smaller b1 f1)
    (smaller b1 f2)
    (smaller b2 f1)
    (smaller b2 f2)

    (smaller s1 s2)

    (smaller s1 b1)
    (smaller s1 b2)
    (smaller s2 b1)
    (smaller s2 b2)

    (on b1 f1)
    (on b2 f2)
    (stacked-on f1 f1)
    (stacked-on f2 f2)
    (stacked-on b1 f1)
    (stacked-on b2 f2)
    (stacked-on s1 f1)
    (stacked-on s2 f2)

    (box b1)
    (box b2)

    (on b1 f1)

  )
  (:goal)
)
