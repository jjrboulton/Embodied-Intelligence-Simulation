(ql:quickload :sketch)
(load "Fuzzy-Circuits/fuzzy-circuits.lisp")

(defpackage :embodied-intelligence-simulation
  (:use :cl :sketch :fuzzy-circuits))

(in-package :embodied-intelligence-simulation)

;;; --- OPTIMIZATION ---
(declaim (optimize (speed 2) (safety 2) (debug 1)))

;;; --- UTILS ---
(defun degrees-to-radians (deg) (* deg (/ (float pi 0.0f0) 180.0f0)))

(defun random-range (min max)
  "Returns a random float between min (inclusive) and max (exclusive)."
  (float (+ min (random (- max min))) 0.0f0))

(defun safe-float-to-string (value precision)
  (let* ((scale (expt 10 precision))
         (rounded-val (round (* (float value) scale))))
    (format nil "~A.~A"
      (floor rounded-val scale)
      (subseq (format nil "~VD" (max 1 precision) (mod rounded-val scale))
              (max 0 (- (length (format nil "~VD" (max 1 precision) (mod rounded-val scale))) precision))))))

(defun clamp (val min max)
  (if (< val min) min (if (> val max) max val)))

(defun angle-diff (a1 a2)
  "Returns smallest difference between two angles in radians."
  (let ((diff (- a1 a2)))
    (setf diff (mod (+ diff (float pi 0.0f0)) (* 2.0f0 (float pi 0.0f0))))
    (- diff (float pi 0.0f0))))

;;; -------------------------------------------
;;; 1. CONSTANTS & CONFIGURATION
;;; -------------------------------------------

(defconstant +time-step-dt+ (/ 1.0f0 20.0f0))
(defconstant +sidebar-width+ 200.0f0)
(defconstant +sim-width+ 1000.0f0)
(defconstant +sim-height+ 750.0f0)

;; Metabolic & Physics
(defconstant +base-metabolic-rate+ 0.05f0) ; Was 0.3f0
(defconstant +torque-cost-multiplier+ 0.005f0) ; Was 0.5f0 (reduced by 100x)
(defconstant +bite-cost+ 5.0f0)
(defconstant +push-cost+ 2.0f0)
(defconstant +vocalization-cost+ 0.01f0)
(defconstant +pregnancy-drain+ 0.1f0)
(defconstant +bite-damage+ 15.0f0)
(defconstant +bite-energy-gain+ 20.0f0) ; Increased gain to make eating worth it

;; Swimmer Body Constants
(defconstant +segment-count+ 8 "Number of body segments (Eel-like)")
(defconstant +segment-radius+ 8.0f0)
(defconstant +segment-spacing+ 12.0f0)
(defconstant +drag-forward+ 0.05f0 "Low drag moving forward")
(defconstant +drag-lateral+ 2.5f0 "High drag moving sideways")
(defconstant +max-torque+ 5.0f0)

;; Vision & Interaction
(defconstant +vision-radius+ 200.0f0)
(defconstant +vision-fov-deg+ 120.0f0)
(defconstant +hearing-radius+ 300.0f0)
(defconstant +push-impulse+ 5.0f0)

;; Reproduction
(defconstant +gestation-period+ 300.0f0)
(defconstant +maturity-age+ 500.0f0)
(defconstant +min-population-for-forced-repro+ 6)
(defconstant +emergency-children-count+ 20)

;; Mutation
(defconstant +mutation-rate-structural+ 0.05f0)
(defconstant +mutation-rate-wiring+ 0.15f0)
(defconstant +mutation-rate-body+ 0.1f0) ; NEW
(defconstant +body-mutation-strength+ 0.15f0)

;; Objects
(defconstant +pellet-energy-value+ 30.0f0)
(defconstant +corpse-energy-value+ 60.0f0)
(defconstant +corpse-decay-time+ 15.0f0)
(defconstant +pellet-spawn-interval+ 8.0f0) ; Spawn pellets every 8 seconds
(defconstant +pellets-per-spawn+ 3) ; Number of pellets to spawn each time

;;; -------------------------------------------
;;; 2. STRUCTS & CLASSES
;;; -------------------------------------------

(defstruct segment
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (vx 0.0f0 :type single-float)
  (vy 0.0f0 :type single-float)
  (angle 0.0f0 :type single-float)
  (va 0.0f0 :type single-float) ;; Angular velocity
  (radius +segment-radius+ :type single-float)
  (parent-idx -1 :type fixnum) ; Index of parent segment (-1 for root)
  (children nil :type list)) ; List of child segment indices

(defstruct gene
  (source-type :sensor :type keyword) ;; :sensor or :gate
  (source-id 0 :type fixnum)
  (target-id 0 :type fixnum)
  (weight 1.0f0 :type single-float))

(defstruct body-gene
  (segments nil :type list) ; List of segment descriptors
  (connections nil :type list) ; List of (parent-idx child-idx) pairs
  (max-torque 5.0f0 :type single-float)
  (mass 1.0f0 :type single-float))

(defstruct segment-descriptor
  (radius 8.0f0 :type single-float)
  (length 12.0f0 :type single-float) ; Distance from parent
  (angle-offset 0.0f0 :type single-float)) ; Relative angle from parent

(defclass gui-element ()
    ((x :initarg :x :accessor x-pos :initform 0.0f0 :type single-float)
     (y :initarg :y :accessor y-pos :initform 0.0f0 :type single-float)))

(defclass physical-object (gui-element)
    ((mass :initarg :mass :accessor mass :initform 1.0f0)
     (radius :initarg :radius :accessor radius :initform 10.0f0)
     (vx :initarg :vx :accessor vx :initform 0.0f0)
     (vy :initarg :vy :accessor vy :initform 0.0f0)
     (friction :initarg :friction :accessor friction :initform 0.1f0)
     (angle :initarg :angle :accessor angle :initform 0.0f0)))

(defclass organism (gui-element)
    ((id :initform (random 100000) :accessor org-id)
     (sex :initarg :sex :accessor sex :initform :female)
     (age :initform 0.0f0 :accessor age)
     ;; Physics Body
     (segments :initarg :segments :accessor segments)
     (mass :initarg :mass :accessor mass :initform 1.0f0)
     ;; Stats
     (health :initarg :health :accessor health :initform 100.0f0)
     (energy :initarg :energy :accessor energy :initform 100.0f0)
     ;; Reproduction
     (pregnant-p :initform nil :accessor pregnant-p)
     (pregnancy-timer :initform 0.0f0 :accessor pregnancy-timer)
     (fetus-genome :initform nil :accessor fetus-genome)
     (fetus-body-gene :initform nil :accessor fetus-body-gene) ; NEW
     ;; Actions/State
     (is-biting :initform nil :accessor is-biting)
     (is-pushing :initform nil :accessor is-pushing)
     (vocal-freq :initform 0.0f0 :accessor vocal-freq)
     ;; Brain
     (genome :initarg :genome :accessor genome)
     (body-gene :initarg :body-gene :accessor body-gene) ; NEW
     (brain :accessor brain)
     (input-map :initform nil :accessor input-map)
     (output-map :initform nil :accessor output-map)
     (max-torque :initarg :max-torque :accessor max-torque :initform +max-torque+)))

;; Accessor Proxies to Head Segment
(defmethod x-pos ((org organism)) (segment-x (aref (segments org) 0)))
(defmethod (setf x-pos) (new-val (org organism)) (setf (segment-x (aref (segments org) 0)) (float new-val 0.0f0)))
(defmethod y-pos ((org organism)) (segment-y (aref (segments org) 0)))
(defmethod (setf y-pos) (new-val (org organism)) (setf (segment-y (aref (segments org) 0)) (float new-val 0.0f0)))
(defmethod radius ((org organism))
  (segment-radius (aref (segments org) 0)))
(defmethod angle ((org organism)) (segment-angle (aref (segments org) 0)))

(defclass pellet (physical-object) ())
(defclass rock (physical-object) ())
(defclass corpse (physical-object)
    ((timer :initarg :timer :accessor decay-timer)))

(defclass button (gui-element)
    ((text :initarg :text :accessor btn-text)
     (action :initarg :action :accessor btn-action)))

;;; -------------------------------------------
;;; 3. GLOBAL STATE
;;; -------------------------------------------

(defvar *organisms* nil)
(defvar *pellets* nil)
(defvar *rocks* nil)
(defvar *corpses* nil)
(defvar *buttons* nil)
(defvar *paused* nil)
(defvar *initialized* nil)
(defvar *selected-org* nil)
(defvar *pellet-spawn-timer* 0.0f0)

;;; -------------------------------------------
;;; 4. SENSORS & VISION
;;; -------------------------------------------

(defun is-visible-p (viewer target)
  (let* ((dx (- (x-pos target) (x-pos viewer)))
         (dy (- (y-pos target) (y-pos viewer)))
         (dist (sqrt (+ (* dx dx) (* dy dy)))))
    (if (> dist +vision-radius+)
        nil
        (let ((angle-to (atan dy dx))
              (fov-rad (degrees-to-radians +vision-fov-deg+)))
          (< (abs (angle-diff angle-to (angle viewer))) (/ fov-rad 2.0f0))))))

(defun get-vision-data (org all-objs)
  (let ((seen-food 0.0f0) (seen-mate 0.0f0) (seen-predator 0.0f0) (seen-obstacle 0.0f0))
    (dolist (o all-objs)
      (unless (eq o org)
        (when (is-visible-p org o)
              (typecase o
                (pellet (incf seen-food 0.2f0))
                (corpse (incf seen-food 0.4f0))
                (rock (incf seen-obstacle 0.5f0))
                (organism
                 (if (eq (sex o) (sex org))
                     (incf seen-predator 0.2f0)
                     (incf seen-mate 0.5f0)))))))
    (list :vis-food (min 1.0f0 seen-food)
          :vis-mate (min 1.0f0 seen-mate)
          :vis-danger (min 1.0f0 seen-predator)
          :vis-obst (min 1.0f0 seen-obstacle))))

(defun get-hearing-data (org all-orgs)
  (let ((low-freq-power 0.0f0) (high-freq-power 0.0f0))
    (dolist (other all-orgs)
      (unless (eq other org)
        (let ((d (sqrt (+ (expt (- (x-pos org) (x-pos other)) 2)
                          (expt (- (y-pos org) (y-pos other)) 2)))))
          (when (< d +hearing-radius+)
                (let ((intensity (/ (- +hearing-radius+ d) +hearing-radius+)))
                  (if (< (vocal-freq other) 0.5f0)
                      (incf low-freq-power intensity)
                      (incf high-freq-power intensity)))))))
    (list :hear-low (min 1.0f0 low-freq-power) :hear-high (min 1.0f0 high-freq-power))))

;;; -------------------------------------------
;;; 5. BRAIN & PHYSICS
;;; -------------------------------------------

(defvar *all-sensor-names* '(:vis-food :vis-mate :vis-danger :vis-obst :hear-low :hear-high :low-energy :pregnant :bias
                                       :joint-0 :joint-1 :joint-2 :joint-3 :joint-4 :joint-5 :joint-6))
(defvar *all-action-names* '(:torque-0 :torque-1 :torque-2 :torque-3 :torque-4 :torque-5 :torque-6
                                       :cmd-bite :cmd-vocal :cmd-push))

(defun random-gene ()
  (make-gene :source-type (if (> (random 1.0f0) 0.4f0) :sensor :gate)
             :source-id (random 30)
             :target-id (random 30)
             :weight (random-range 0.1f0 2.0f0)))

(defun build-brain (genome)
  (let ((ckt (make-circuit))
        (inputs (make-hash-table))
        (outputs (make-hash-table))
        (gates (make-hash-table)))
    ;; Sensors
    (dolist (name *all-sensor-names*)
      (setf (gethash name inputs) (add-component ckt (make-instance 'component :name (string name)))))
    ;; Actuators (OR gates)
    (dolist (name *all-action-names*)
      (setf (gethash name outputs) (add-component ckt (make-instance 'or-gate :name (string name)))))
    ;; Internal Gates
    (let ((max-gate-id 0))
      (dolist (g genome)
        (setf max-gate-id (max max-gate-id (gene-target-id g) (gene-source-id g))))
      (dotimes (i (1+ max-gate-id))
        (setf (gethash i gates) (add-component ckt (make-instance 'and-gate :name (format nil "G~A" i))))))
    ;; Wiring
    (dolist (g genome)
      (let* ((source-id (gene-source-id g))
             (source (if (eq (gene-source-type g) :sensor)
                         (gethash (nth (mod source-id (length *all-sensor-names*)) *all-sensor-names*) inputs)
                         (gethash source-id gates)))
             (target (gethash (gene-target-id g) gates))
             (weight (gene-weight g)))
        (when (and source target)
              (connect source target :weight weight))))
    ;; Output Connections
    (maphash (lambda (id gate)
               (declare (ignore id))
               (let ((out-node (gethash (nth (random (length *all-action-names*)) *all-action-names*) outputs)))
                 (connect gate out-node :weight (random-range 0.5f0 1.5f0))))
             gates)
    (values ckt inputs outputs)))

(defun mutate-genome (genome)
  (let ((new-genome (copy-list genome)))
    (when (< (random 1.0f0) +mutation-rate-structural+) (push (random-gene) new-genome))
    (when (and new-genome (< (random 1.0f0) +mutation-rate-structural+))
          (setf new-genome (remove (nth (random (length new-genome)) new-genome) new-genome :test 'eq)))
    (dolist (g new-genome)
      (when (< (random 1.0f0) +mutation-rate-wiring+)
            (setf (gene-weight g) (random-range 0.1f0 3.0f0))))
    new-genome))

(defun mutate-body-gene (body-gene)
  (let* ((new-gene (copy-structure body-gene))
         (new-segments (mapcar #'copy-structure (body-gene-segments new-gene)))
         (new-connections (copy-list (body-gene-connections new-gene))))

    ;; Mutate existing segment properties
    (dolist (seg new-segments)
      (when (< (random 1.0f0) +mutation-rate-body+)
            (setf (segment-descriptor-radius seg)
              (clamp (* (segment-descriptor-radius seg)
                        (random-range 0.85f0 1.15f0))
                     4.0f0 14.0f0)))
      (when (< (random 1.0f0) +mutation-rate-body+)
            (setf (segment-descriptor-length seg)
              (clamp (* (segment-descriptor-length seg)
                        (random-range 0.85f0 1.15f0))
                     6.0f0 20.0f0)))
      (when (< (random 1.0f0) +mutation-rate-body+)
            (setf (segment-descriptor-angle-offset seg)
              (+ (segment-descriptor-angle-offset seg)
                 (random-range -0.3f0 0.3f0)))))

    ;; Add new segment (branch mutation)
    (when (and (< (random 1.0f0) +mutation-rate-body+)
               (< (length new-segments) 16))
          (let ((parent-idx (random (length new-segments))))
            (push (make-segment-descriptor
                   :radius (random-range 5.0f0 10.0f0)
                   :length (random-range 8.0f0 16.0f0)
                   :angle-offset (random-range -1.57f0 1.57f0))
                  new-segments)
            (push (list parent-idx (1- (length new-segments))) new-connections)))

    ;; Remove segment (pruning mutation)
    (when (and (< (random 1.0f0) (* +mutation-rate-body+ 0.5))
               (> (length new-segments) 4))
          (let ((remove-idx (1+ (random (1- (length new-segments)))))) ; Don't remove root
            (setf new-segments (remove-if (lambda (s) (= (position s new-segments) remove-idx)) new-segments))
            (setf new-connections (remove-if (lambda (c) (or (= (first c) remove-idx)
                                                             (= (second c) remove-idx)))
                                      new-connections))
            ;; Adjust indices
            (setf new-connections
              (mapcar (lambda (c)
                        (list (if (> (first c) remove-idx) (1- (first c)) (first c))
                              (if (> (second c) remove-idx) (1- (second c)) (second c))))
                  new-connections))))

    ;; Mutate torque and mass
    (when (< (random 1.0f0) +mutation-rate-body+)
          (setf (body-gene-max-torque new-gene)
            (clamp (* (body-gene-max-torque new-gene)
                      (random-range 0.85f0 1.15f0))
                   2.0f0 12.0f0)))
    (when (< (random 1.0f0) +mutation-rate-body+)
          (setf (body-gene-mass new-gene)
            (clamp (* (body-gene-mass new-gene)
                      (random-range 0.85f0 1.15f0))
                   0.5f0 3.5f0)))

    (setf (body-gene-segments new-gene) new-segments)
    (setf (body-gene-connections new-gene) new-connections)
    new-gene))

(defun update-brain (org all-orgs world-objs)
  (let ((inputs (input-map org))
        (outputs (output-map org)))
    ;; Vision/Hearing
    (let* ((vis-data (get-vision-data org world-objs))
           (hear-data (get-hearing-data org all-orgs)))
      (set-value (gethash :vis-food inputs) (getf vis-data :vis-food))
      (set-value (gethash :vis-mate inputs) (getf vis-data :vis-mate))
      (set-value (gethash :vis-danger inputs) (getf vis-data :vis-danger))
      (set-value (gethash :vis-obst inputs) (getf vis-data :vis-obst))
      (set-value (gethash :hear-low inputs) (getf hear-data :hear-low))
      (set-value (gethash :hear-high inputs) (getf hear-data :hear-high))
      (set-value (gethash :low-energy inputs) (if (< (energy org) 30.0f0) 1.0f0 0.0f0))
      (set-value (gethash :pregnant inputs) (if (pregnant-p org) 1.0f0 0.0f0))
      (set-value (gethash :bias inputs) 1.0f0)
      ;; Proprioception (Joint Angles)
      (let ((actual-seg-count (length (body-gene-segments (body-gene org)))))
        (loop for i from 0 below (min (1- actual-seg-count) 7) do ; Only access joints that exist
                (let ((s1 (aref (segments org) i))
                      (s2 (aref (segments org) (1+ i)))
                      (key (nth (+ 9 i) *all-sensor-names*)))
                  (when key
                        (let ((diff (angle-diff (segment-angle s1) (segment-angle s2))))
                          (set-value (gethash key inputs) (clamp (+ 0.5f0 (/ diff 6.28318f0)) 0.0f0 1.0f0)))))))
      ;; Compute
      (compute-step (brain org))
      ;; Return outputs
      (let ((output-vals (make-hash-table)))
        (maphash (lambda (key component)
                   (setf (gethash key output-vals) (get-value component)))
                 outputs)
        output-vals))))

(defun apply-torque (org joint-index torque)
  (let* ((segs (segments org))
         (s1 (aref segs joint-index))
         (s2 (aref segs (1+ joint-index))))
    (setf (segment-va s1) (+ (segment-va s1) torque))
    (setf (segment-va s2) (- (segment-va s2) torque))

    ;; Reduced cost application
    (decf (energy org) (* (abs torque) +torque-cost-multiplier+))))

(defun update-physics (org dt)
  (let ((segs (segments org))
        (seg-count (length (body-gene-segments (body-gene org))))
        (connections (body-gene-connections (body-gene org))))

    ;; 1. Update angles
    (dotimes (i seg-count)
      (let ((seg (aref segs i)))
        (incf (segment-angle seg) (* (segment-va seg) dt 5.0f0))
        (setf (segment-angle seg) (mod (segment-angle seg) (* 2.0f0 (float pi 0.0f0))))
        (setf (segment-va seg) (* (segment-va seg) 0.85f0))))

    ;; 2. Anisotropic Drag
    (dotimes (i seg-count)
      (let* ((seg (aref segs i))
             (cos-a (cos (segment-angle seg)))
             (sin-a (sin (segment-angle seg)))
             (v-fwd (+ (* (segment-vx seg) cos-a) (* (segment-vy seg) sin-a)))
             (v-lat (- (* (segment-vy seg) cos-a) (* (segment-vx seg) sin-a))))
        (setf v-fwd (* v-fwd (- 1.0f0 +drag-forward+)))
        (setf v-lat (* v-lat (- 1.0f0 +drag-lateral+)))
        (setf (segment-vx seg) (- (* v-fwd cos-a) (* v-lat sin-a)))
        (setf (segment-vy seg) (+ (* v-fwd sin-a) (* v-lat cos-a)))))

    ;; 2.5. Thrust from undulation
    (dolist (conn connections)
      (let* ((parent-idx (first conn))
             (child-idx (second conn))
             (seg (aref segs child-idx))
             (parent-seg (aref segs parent-idx))
             (angle-diff (angle-diff (segment-angle seg) (segment-angle parent-seg)))
             (thrust-force (* angle-diff 0.8f0)))
        (incf (segment-vx seg) (* thrust-force (- (sin (segment-angle seg)))))
        (incf (segment-vy seg) (* thrust-force (cos (segment-angle seg))))))

    ;; 3. Integrate Position
    (dotimes (i seg-count)
      (let ((seg (aref segs i)))
        (incf (segment-x seg) (* (segment-vx seg) dt 20.0f0))
        (incf (segment-y seg) (* (segment-vy seg) dt 20.0f0))
        (setf (segment-vx seg) (* (segment-vx seg) 0.98f0))
        (setf (segment-vy seg) (* (segment-vy seg) 0.98f0))))

    ;; 3.5. Rock Collision
    (dolist (rock *rocks*)
      (dotimes (i seg-count)
        (let* ((seg (aref segs i))
               (dx (- (segment-x seg) (x-pos rock)))
               (dy (- (segment-y seg) (y-pos rock)))
               (dist (sqrt (+ (* dx dx) (* dy dy))))
               (min-dist (+ (segment-radius seg) (radius rock))))
          (when (< dist min-dist)
                (let* ((overlap (- min-dist dist))
                       (nx (if (zerop dist) 1.0f0 (/ dx dist)))
                       (ny (if (zerop dist) 0.0f0 (/ dy dist))))
                  (incf (segment-x seg) (* nx overlap))
                  (incf (segment-y seg) (* ny overlap))
                  (let ((dot-product (+ (* (segment-vx seg) nx) (* (segment-vy seg) ny))))
                    (setf (segment-vx seg) (* (- (segment-vx seg) (* 2.0f0 dot-product nx)) 0.3f0))
                    (setf (segment-vy seg) (* (- (segment-vy seg) (* 2.0f0 dot-product ny)) 0.3f0))))))))

    ;; 4. Constraint Solving (Tree structure)
    (dotimes (iter 20)
      (dolist (conn connections)
        (let* ((parent-idx (first conn))
               (child-idx (second conn))
               (parent-seg (aref segs parent-idx))
               (child-seg (aref segs child-idx))
               (descriptor (nth child-idx (body-gene-segments (body-gene org))))
               (target-length (segment-descriptor-length descriptor))
               (dx (- (segment-x child-seg) (segment-x parent-seg)))
               (dy (- (segment-y child-seg) (segment-y parent-seg)))
               (dist (sqrt (+ (* dx dx) (* dy dy)))))
          (unless (zerop dist)
            (let* ((diff (- dist target-length))
                   (off-x (* (/ dx dist) diff 0.5f0))
                   (off-y (* (/ dy dist) diff 0.5f0)))
              ;; Move both segments
              (if (= parent-idx 0)
                  ;; Root is heavier, child moves more
                  (progn
                   (incf (segment-x parent-seg) (* off-x 0.2f0))
                   (incf (segment-y parent-seg) (* off-y 0.2f0))
                   (decf (segment-x child-seg) (* off-x 0.8f0))
                   (decf (segment-y child-seg) (* off-y 0.8f0)))
                  ;; Equal distribution
                  (progn
                   (incf (segment-x parent-seg) off-x)
                   (incf (segment-y parent-seg) off-y)
                   (decf (segment-x child-seg) off-x)
                   (decf (segment-y child-seg) off-y)))
              ;; Damp velocities
              (setf (segment-vx parent-seg) (* (segment-vx parent-seg) 0.95f0))
              (setf (segment-vy parent-seg) (* (segment-vy parent-seg) 0.95f0))
              (setf (segment-vx child-seg) (* (segment-vx child-seg) 0.95f0))
              (setf (segment-vy child-seg) (* (segment-vy child-seg) 0.95f0)))))))

    ;; 5. Boundaries
    (dotimes (i seg-count)
      (let ((seg (aref segs i))) ; ADD THIS LET
        (let ((r (segment-radius seg)))
          (when (< (segment-x seg) r)
                (setf (segment-x seg) r)
                (setf (segment-vx seg) (* (abs (segment-vx seg)) 0.5f0)))
          (when (> (segment-x seg) (- +sim-width+ r))
                (setf (segment-x seg) (- +sim-width+ r))
                (setf (segment-vx seg) (* (- (abs (segment-vx seg))) 0.5f0)))
          (when (< (segment-y seg) r)
                (setf (segment-y seg) r)
                (setf (segment-vy seg) (* (abs (segment-vy seg)) 0.5f0)))
          (when (> (segment-y seg) (- +sim-height+ r))
                (setf (segment-y seg) (- +sim-height+ r))
                (setf (segment-vy seg) (* (- (abs (segment-vy seg))) 0.5f0))))))))

(defun resolve-actions (org gate-vals dt)
  ;; Apply Torques
  (let ((actual-seg-count (length (body-gene-segments (body-gene org)))))
    (loop for i from 0 below (min (1- actual-seg-count) 7) do ; Only apply torque to joints that exist
            (let* ((key (nth i *all-action-names*))
                   (val (gethash key gate-vals 0.0f0)))
              (when val
                    (let ((torque (* (- val 0.5f0) 2.0f0 (max-torque org))))
                      (apply-torque org i torque))))))
  ;; Update Physics
  (update-physics org dt)
  ;; Commands
  (setf (is-biting org) (> (gethash :cmd-bite gate-vals 0.0f0) 0.5f0))
  (setf (is-pushing org) (> (gethash :cmd-push gate-vals 0.0f0) 0.5f0))
  (let ((v (gethash :cmd-vocal gate-vals 0.0f0)))
    (setf (vocal-freq org) (if (> v 0.5f0) v -1.0f0)))
  ;; Costs
  (let ((cost (+ +base-metabolic-rate+
                 (* (if (> (vocal-freq org) 0) 1 0) +vocalization-cost+)
                 (if (pregnant-p org) +pregnancy-drain+ 0.0f0))))
    (decf (energy org) (* cost dt))))

;;; -------------------------------------------
;;; 6. BIOLOGY & POPULATION
;;; -------------------------------------------

(defun random-body-gene ()
  "Generate a random branching body structure"
  (let ((segments nil)
        (connections nil)
        (segment-count (+ 4 (random 9)))) ; 4-12 segments

    ;; Create root segment (will be index 0)
    (push (make-segment-descriptor
           :radius (random-range 6.0f0 12.0f0)
           :length 0.0f0
           :angle-offset 0.0f0)
          segments)

    ;; Add segments one at a time, potentially branching
    (dotimes (i (1- segment-count))
      (let* ((current-idx (1+ i)) ; Current segment being added
                                 (parent-idx (random current-idx)) ; Pick random existing segment (0 to current-1)
                                 (branch-angle (if (< (random 1.0f0) 0.3) ; 30% chance of branching
                                                   (random-range -1.57f0 1.57f0) ; ±90 degrees
                                                   0.0f0))) ; Straight continuation
        (push (make-segment-descriptor
               :radius (random-range 5.0f0 10.0f0)
               :length (random-range 8.0f0 16.0f0)
               :angle-offset branch-angle)
              segments)
        (push (list parent-idx current-idx) connections)))

    (make-body-gene
     :segments (nreverse segments)
     :connections (nreverse connections)
     :max-torque (random-range 3.0f0 8.0f0)
     :mass (random-range 0.8f0 2.0f0))))

(defun make-swimmer (x y &optional parent-genome parent-body-gene)
  (let* ((fx (float x 0.0f0))
         (fy (float y 0.0f0))
         (bg (if parent-body-gene
                 (mutate-body-gene parent-body-gene)
                 (random-body-gene)))
         (descriptors (body-gene-segments bg))
         (connections (body-gene-connections bg))
         (seg-count (length descriptors))
         (segs (make-array seg-count :initial-element nil))) ; CHANGED: initialize with nil

    ;; Create root segment
    (setf (aref segs 0)
      (make-segment :x fx :y fy :angle 0.0f0
                    :radius (segment-descriptor-radius (first descriptors))
                    :parent-idx -1))

    ;; Build tree structure - process connections in order
    (dolist (conn connections)
      (let* ((parent-idx (first conn))
             (child-idx (second conn))
             (parent-seg (aref segs parent-idx))
             (descriptor (nth child-idx descriptors))
             (local-angle (+ (segment-angle parent-seg)
                             (segment-descriptor-angle-offset descriptor)))
             (dx (* (segment-descriptor-length descriptor) (cos local-angle)))
             (dy (* (segment-descriptor-length descriptor) (sin local-angle))))
        (setf (aref segs child-idx)
          (make-segment
           :x (+ (segment-x parent-seg) dx)
           :y (+ (segment-y parent-seg) dy)
           :angle local-angle
           :radius (segment-descriptor-radius descriptor)
           :parent-idx parent-idx))
        (push child-idx (segment-children parent-seg))))

    ;; Verify all segments are initialized
    (dotimes (i seg-count)
      (unless (aref segs i)
        (error "Segment ~A not initialized in body structure" i)))

    (let* ((gn (if parent-genome (mutate-genome parent-genome)
                   (loop repeat 25 collect (random-gene))))
           (org (make-instance 'organism
                  :segments segs
                  :genome gn
                  :body-gene bg
                  :mass (body-gene-mass bg)
                  :max-torque (body-gene-max-torque bg))))
      (multiple-value-bind (ckt ins outs) (build-brain gn)
        (setf (brain org) ckt
          (input-map org) ins
          (output-map org) outs))
      (setf (sex org) (if (> (random 1.0f0) 0.5f0) :male :female))
      org)))

(defun create-initial-population (count)
  "Initializes the world with a single population."
  (setf *organisms* nil)
  (dotimes (i count)
    (push (make-swimmer (random-range 100.0f0 900.0f0)
                        (random-range 100.0f0 600.0f0))
          *organisms*)))

(defun emergency-repopulate ()
  "Forces reproduction when population is low."
  (format t "EMERGENCY REPOPULATION~%")
  (let ((parents *organisms*)
        (needed (- +emergency-children-count+ (length *organisms*))))
    (when parents
          (dotimes (i needed)
            (let* ((parent (nth (random (length parents)) parents))
                   (new-org (make-swimmer (random-range 100.0f0 900.0f0)
                                          (random-range 100.0f0 600.0f0)
                                          (genome parent)
                                          (body-gene parent)))) ; Pass body gene
              (setf (health new-org) 60.0f0)
              (setf (energy new-org) 60.0f0)
              (push new-org *organisms*))))))

(defun update-biology (org dt)
  (incf (age org) dt)
  (when (pregnant-p org)
        (decf (pregnancy-timer org) dt)
        (when (<= (pregnancy-timer org) 0.0f0)
              (let ((baby (make-swimmer (x-pos org) (y-pos org)
                                        (fetus-genome org)
                                        (fetus-body-gene org)))) ; Pass body gene
                (setf (health baby) 50.0f0 (energy baby) 50.0f0)
                (setf (pregnant-p org) nil
                  (fetus-genome org) nil
                  (fetus-body-gene org) nil) ; Clear body gene
                baby))))

(defun try-mating (female male)
  (when (and (eq (sex female) :female) (eq (sex male) :male)
             (not (pregnant-p female))
             (> (energy female) 40.0f0) (> (energy male) 40.0f0)
             (> (age female) +maturity-age+) (> (age male) +maturity-age+))
        (setf (pregnant-p female) t)
        (setf (pregnancy-timer female) +gestation-period+)
        ;; Inherit genome and body gene from male
        (setf (fetus-genome female) (genome male))
        (setf (fetus-body-gene female) (body-gene male))
        (decf (energy female) 10.0f0)
        (decf (energy male) 5.0f0)
        t))

(defun apply-push-force (pusher target)
  (let* ((h1 (aref (segments pusher) 0))
         (h2 (aref (segments target) 0))
         (dx (- (segment-x h2) (segment-x h1)))
         (dy (- (segment-y h2) (segment-y h1)))
         (dist (sqrt (+ (* dx dx) (* dy dy)))))
    (unless (zerop dist)
      (let ((nx (/ dx dist)) (ny (/ dy dist)))
        (incf (segment-vx h2) (* nx +push-impulse+))
        (incf (segment-vy h2) (* ny +push-impulse+))
        (decf (energy pusher) +push-cost+)))))

(defun resolve-interactions (o1 o2)
  (let* ((h1 (aref (segments o1) 0))
         (h2 (aref (segments o2) 0))
         (dx (- (segment-x h1) (segment-x h2)))
         (dy (- (segment-y h1) (segment-y h2)))
         (dist-sq (+ (* dx dx) (* dy dy))))
    (when (< dist-sq (expt (+ (radius o1) (radius o2)) 2))
          (try-mating o1 o2)
          (try-mating o2 o1)
          (when (is-biting o1)
                (decf (health o2) +bite-damage+) (incf (energy o1) +bite-energy-gain+))
          (when (is-biting o2)
                (decf (health o1) +bite-damage+) (incf (energy o2) +bite-energy-gain+))
          (when (is-pushing o1) (apply-push-force o1 o2))
          (when (is-pushing o2) (apply-push-force o2 o1))
          ;; Separation
          (let* ((dist (sqrt dist-sq))
                 (overlap (- (+ (radius o1) (radius o2)) dist))
                 (nx (if (zerop dist) 0.0f0 (/ dx dist)))
                 (ny (if (zerop dist) 0.0f0 (/ dy dist))))
            (when (> overlap 0.0f0)
                  (incf (x-pos o1) (* nx overlap 0.5f0))
                  (incf (y-pos o1) (* ny overlap 0.5f0))
                  (decf (x-pos o2) (* nx overlap 0.5f0))
                  (decf (y-pos o2) (* ny overlap 0.5f0)))))))

(defun init-world ()
  (setf *organisms* nil *pellets* nil *rocks* nil *corpses* nil *paused* nil *selected-org* nil *pellet-spawn-timer* 0.0f0)
  (create-initial-population 12)
  (dotimes (i 150) (push (make-instance 'pellet :x (random +sim-width+) :y (random +sim-height+) :radius 5.0f0) *pellets*))
  (dotimes (i 8) (push (make-instance 'rock :x (random +sim-width+) :y (random +sim-height+) :radius (+ 20.0f0 (random 30.0f0))) *rocks*))
  (setf *buttons* (list (make-instance 'button :x 1020 :y 30 :text "Reset" :action #'init-world)
                        (make-instance 'button :x 1020 :y 70 :text "Pause" :action (lambda () (setf *paused* (not *paused*))))
                        (make-instance 'button :x 1020 :y 110 :text "Repopulate" :action #'emergency-repopulate)))
  (setf *initialized* t))

(defun update-world ()
  (unless *paused*
    ;; Periodic pellet spawning
    (incf *pellet-spawn-timer* +time-step-dt+)
    (when (>= *pellet-spawn-timer* +pellet-spawn-interval+)
          (setf *pellet-spawn-timer* 0.0f0)
          (dotimes (i +pellets-per-spawn+)
            (push (make-instance 'pellet
                    :x (random +sim-width+)
                    :y (random +sim-height+)
                    :radius 5.0f0)
                  *pellets*)))
    (let ((new-babies nil)
          (all-objs (append *pellets* *rocks* *organisms* *corpses*)))
      ;; 1. Brain & Physics
      (dolist (o *organisms*)
        (let ((vals (update-brain o *organisms* all-objs)))
          (resolve-actions o vals +time-step-dt+)
          (let ((baby (update-biology o +time-step-dt+)))
            (when baby (push baby new-babies)))))
      ;; 2. Interactions
      (loop for sub on *organisms* do
              (let ((o1 (car sub)))
                (dolist (o2 (cdr sub)) (resolve-interactions o1 o2))))
      ;; 3. Feeding
      (let ((eaten nil))
        (dolist (o *organisms*)
          (let ((h (aref (segments o) 0)))
            (dolist (f (append *pellets* *corpses*))
              (let ((d (sqrt (+ (expt (- (segment-x h) (x-pos f)) 2) (expt (- (segment-y h) (y-pos f)) 2)))))
                (when (< d (+ +segment-radius+ (radius f)))
                      (typecase f
                        (pellet (incf (energy o) +pellet-energy-value+))
                        (corpse (incf (energy o) +corpse-energy-value+)))
                      (push f eaten))))))
        (setf *pellets* (set-difference *pellets* eaten :test 'eq))
        (setf *corpses* (set-difference *corpses* eaten :test 'eq)))
      ;; 4. Cleanup & Population Check
      (let ((survivors nil))
        (dolist (o *organisms*)
          (cond ((or (<= (health o) 0) (<= (energy o) 0))
                  (when (eq o *selected-org*) (setf *selected-org* nil))
                  (push (make-instance 'corpse :x (x-pos o) :y (y-pos o) :radius +segment-radius+ :timer +corpse-decay-time+) *corpses*))
                (t (push o survivors))))
        (setf *organisms* (nconc survivors new-babies)))
      (when (< (length *organisms*) +min-population-for-forced-repro+) (emergency-repopulate))
      (setf *corpses* (remove-if (lambda (c) (decf (decay-timer c) +time-step-dt+) (<= (decay-timer c) 0)) *corpses*)))))

;;; -------------------------------------------
;;; 7. MAIN SKETCH
;;; -------------------------------------------

(defsketch embodied-intelligence-sim
           ((title "Swimmer Evolution") (width 1200) (height 750) (sim-w 1000.0f0) (sim-h 750.0f0))
           (unless *initialized* (init-world))
           (update-world)

           ;; Background
           (background +white+)
           (with-pen (make-pen :fill (rgb 0.15 0.15 0.18))
                     (rect sim-w 0 +sidebar-width+ sim-h))

           ;; Draw World
           (dolist (r *rocks*) (with-pen (make-pen :fill (rgb 0.4 0.4 0.4)) (circle (x-pos r) (y-pos r) (radius r))))
           (dolist (p *pellets*) (with-pen (make-pen :fill (rgb 0 0.8 0)) (circle (x-pos p) (y-pos p) (radius p))))
           (dolist (c *corpses*) (with-pen (make-pen :fill (rgb 0.3 0.1 0.1)) (circle (x-pos c) (y-pos c) (radius c))))

           ;; Draw Eels
           (dolist (o *organisms*)
             (let ((segs (segments o))
                   (seg-count (length (body-gene-segments (body-gene o))))) ; CHANGE THIS LINE
               (when (eq o *selected-org*)
                     (let* ((h (aref segs 0))
                            (start-a (- (angle h) (degrees-to-radians (/ +vision-fov-deg+ 2.0))))
                            (end-a (+ (angle h) (degrees-to-radians (/ +vision-fov-deg+ 2.0)))))
                       (with-pen (make-pen :fill (rgb 1 1 1 0.1))
                                 (line (segment-x h) (segment-y h) (+ (segment-x h) (* +vision-radius+ (cos start-a))) (+ (segment-y h) (* +vision-radius+ (sin start-a))))
                                 (line (segment-x h) (segment-y h) (+ (segment-x h) (* +vision-radius+ (cos end-a))) (+ (segment-y h) (* +vision-radius+ (sin end-a)))))))
               ;; Connectors - draw all connections
               (with-pen (make-pen :stroke (rgb 0.5 0.5 0.5) :weight 3)
                         (dolist (conn (body-gene-connections (body-gene o)))
                           (let ((parent-idx (first conn))
                                 (child-idx (second conn)))
                             (line (segment-x (aref segs parent-idx))
                                   (segment-y (aref segs parent-idx))
                                   (segment-x (aref segs child-idx))
                                   (segment-y (aref segs child-idx))))))
               ;; Segments
               (dotimes (i seg-count)
                 (let ((seg (aref segs i)))
                   (when seg ; ADD THIS CHECK
                         (let ((c (cond ((zerop i) (rgb 0 0 0)) ;; Head colour
                                        ((evenp i) (rgb 0 0 0)) ;; Body colour 1
                                        (t (rgb 0 0 0))))) ;; Body colour 2
                           (with-pen (make-pen :fill c :stroke +black+ :weight 1)
                                     (circle (segment-x seg) (segment-y seg) (segment-radius seg)))))))
               ;; Bars
               (let ((h (aref segs 0)))
                 (with-pen (make-pen :fill +cyan+) (rect (- (segment-x h) 10) (- (segment-y h) 20) (/ (* (energy o) 20) 100) 3))
                 (with-pen (make-pen :fill +red+) (rect (- (segment-x h) 10) (- (segment-y h) 15) (/ (* (health o) 20) 100) 3)))))

           ;; Draw Buttons
           (dolist (b *buttons*)
             (with-pen (make-pen :fill (rgb 0.4 0.4 0.5)) (rect (x-pos b) (y-pos b) 100 30))
             (with-pen (make-pen :fill (rgb 1 1 1)) (text (btn-text b) (+ (x-pos b) 10) (+ (y-pos b) 3))))

           ;; Draw Sidebar Info
           (when *selected-org*
                 (let ((org *selected-org*) (x (+ sim-w 10)) (y 150))
                   (with-pen (make-pen :fill (rgb 1 1 1))
                             (text (format nil "ID: ~A (~A)" (org-id org) (sex org)) x y) (incf y 20)
                             (text (format nil "Age: ~A Health: ~A" (round (age org)) (round (health org))) x y) (incf y 20)
                             (text (format nil "Energy: ~A" (round (energy org))) x y) (incf y 20)
                             (text (format nil "Inputs:") x y) (incf y 20)
                             (maphash (lambda (k v) (text (format nil "~A: ~A" k (safe-float-to-string (get-value v) 1)) x y) (incf y 15)) (input-map org))
                             (incf y 10) (text (format nil "Outputs:") x y) (incf y 20)
                             (maphash (lambda (k v) (text (format nil "~A: ~A" k (safe-float-to-string (get-value v) 1)) x y) (incf y 15)) (output-map org))))))

(defmethod on-click ((app embodied-intelligence-sim) x y)
  (if (> x (slot-value app 'sim-w))
      (dolist (b *buttons*)
        (when (and (> x (x-pos b)) (< x (+ (x-pos b) 100)) (> y (y-pos b)) (< y (+ (y-pos b) 30)))
              (funcall (btn-action b))))
      (progn
       (setf *selected-org* nil)
       (dolist (o *organisms*)
         (when (< (sqrt (+ (expt (- x (x-pos o)) 2) (expt (- y (y-pos o)) 2))) +segment-radius+)
               (setf *selected-org* o) (return))))))

(make-instance 'embodied-intelligence-sim)
