;; Load the Sketch package with Quicklisp
(ql:quickload :sketch)

;; Define our package
(defpackage :my-sketch-tests
  (:use :cl :sketch))

;; All code below this line is in the aforementioned package
(in-package :my-sketch-tests)

;; Define GUI element
(defclass gui_element ()
  ((x-position :initarg :x :accessor x-position)
   (y-position :initarg :y :accessor y-position))
)

;; Define organism
(defclass organism (gui_element)
  ((health :initarg :health :accessor health)
   (energy :initarg :energy :accessor energy))
)
   
;; Define edible pellet
(defclass pellet (gui_element)
  ()
)
   
;; Define button
(defclass button (gui_element)
  (
    (buttontext :initarg :buttontext :accessor buttontext)
    (effect :initarg :effect :accessor effect)
  )
)

;; Define slider
(defclass slider (gui_element)
  ((parameter :initarg :start_value :accessor parameter))
)
  
(defvar is_paused nil)
(defun pause ()
  (setf is_paused (not is_paused))
)
  
;; Populate organisms
(defvar organisms ())
(dotimes (n 5)
  (push (make-instance 'organism :x (random 800) :y (random 600) :health 100 :energy 100) organisms)
)

;; Populate food
(defvar pellets ())
(dotimes (n 27)
  (push (make-instance 'pellet :x (random 800) :y (random 600)) pellets)
)

;; Buttons
(defvar buttons ())

(push (make-instance 'button :x 975 :y 50 :buttontext "Pause/Resume" :effect #'pause) buttons)
(push (make-instance 'button :x 975 :y 100 :buttontext "Load World" :effect #'pause) buttons)
(push (make-instance 'button :x 975 :y 150 :buttontext "Save World" :effect #'pause) buttons)
(push (make-instance 'button :x 975 :y 200 :buttontext "Load Organism" :effect #'pause) buttons)
(push (make-instance 'button :x 975 :y 250 :buttontext "Save Organism" :effect #'pause) buttons)

;; Simulation loop
(defsketch alife
    ((title "Embodied Intelligence Simulation") (width 1200) (height 700) (i 0))

  (background +white+)

  ;; Render world
  (rect 0 0 900 700)
  (text "Control Panel" 1000 10)
  
  ;; Render buttons
  (dolist (btn buttons)
    (rect (x-position btn) (y-position btn) 150 25)
    (text (buttontext btn) (+ (x-position btn) 20) (y-position btn))
  )
  
  ;; Draw pellets
  (dolist (pellet pellets)
    (with-pen (make-pen :fill +green+)
      (circle (x-position pellet) (y-position pellet) 5))
  )

  ;; Update if unpaused
  (when (not is_paused)
    ;; Update organisms
    (dolist (organism organisms)
      ;; Random Movement: (random 3) gives 0, 1, or 2. Subtracting 1 yields -1, 0, or 1.
      (let ((dx (- (random 3) 1))
            (dy (- (random 3) 1)))
        (incf (x-position organism) dx)
        (incf (y-position organism) dy))
      
      ;; 2. Confine the organism's X position to the bounds [0, 900]
      (setf (x-position organism) (max 0 (min 900 (x-position organism))))
      
      ;; 3. Confine the organism's Y position to the bounds [0, 700]
      (setf (y-position organism) (max 0 (min 700 (y-position organism)))))
  )

  ;; Draw organisms
  (dolist (organism organisms)
    (with-pen (make-pen :fill +blue+)
      (circle (x-position organism) (y-position organism) 10)
    )
    (with-pen (make-pen :fill +cyan+)
      (rect (- (x-position organism) 10) (- (y-position organism) 20) 20 3)
    )
    (with-pen (make-pen :fill +red+)
      (rect (- (x-position organism) 10) (- (y-position organism) 15) 20 3)
    )
  )
)

;; Button click handler
(defmethod on-click ((window alife) x y)
  (dolist (btn buttons)
    ;; Get the button's properties
    (let ((btn-x (x-position btn))
          (btn-y (y-position btn))
          (btn-width 150)
          (btn-height 25))
      
      ;; Check if the click coordinates (x, y) are inside the button's rectangle
      (when (and (>= x btn-x)                       ; Click is to the right of the left edge
                 (<= x (+ btn-x btn-width))         ; Click is to the left of the right edge
                 (>= y btn-y)                       ; Click is below the top edge
                 (<= y (+ btn-y btn-height)))       ; Click is above the bottom edge
        
        ;; If it is a hit, call the button's effect function
        (funcall (effect btn))
        
        ;; Optionally, stop processing buttons once one is hit
        (return-from on-click t)
      )
  )
))

(make-instance 'alife)
