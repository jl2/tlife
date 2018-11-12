;;;; tlife.lisp 
;;
;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :tlife)

(defclass renderer ()
  ())

(defclass text-renderer (renderer)
  ((stream :initform *standard-output* :initarg :stream)))

(defclass gl-renderer (renderer)
  ((viewer :type clgl:viewer :initarg :viewer)))

(defclass 2d-gl-renderer (gl-renderer)
  ((viewer :type clgl:viewer :initform (make-instance 'clgl:viewer :viewport (make-instance 'clgl:2d-viewport)))
   ;;(blocks :type (or null clgl:primitives) :initform nil)
   ))

(defclass 3d-gl-renderer (gl-renderer)
  ((viewer :type clgl:viewer :initform (make-instance 'clgl:viewer :viewport (make-instance 'clgl:rotating-viewport)))))

(defclass game-of-life ()
  ((width :type fixnum)
   (height :type fixnum)
   (depth :type fixnum)
   (grid :initform nil :type (or null (simple-array boolean (* * *))))
   (cur-idx :initform 0 :type fixnum)))

(defclass conways-game-of-life (game-of-life)
  ((width :type fixnum :initform 10 :initarg :width)
   (height :type fixnum :initform 10 :initarg :height)
   (depth :type fixnum :initform 2 :initarg :depth)))

(defclass toroid-life (game-of-life)
  ((width :type fixnum :initform 10 :initarg :width)
   (height :type fixnum :initform 10 :initarg :height)
   (depth :type fixnum :initform 200 :initarg :depth)
   (rotate :type fixnum :initform 0 :initarg :rotate)))

(defmethod print-object ((object game-of-life) stream)
  (format stream "Grid:~%")
  (with-slots (width height depth grid cur-idx) object
    (loop for idx below depth do
         (format stream "  Level: ~a~%" idx)
         (loop for i below height do
              (format stream "    ")
              (loop for j below width do
                   (format stream "~a" (if (aref grid i j idx) "X" " ")))
              (format stream "~%")))
    (format stream "cur-idx: ~a~%" cur-idx)))

(defgeneric clear (game)
  (:documentation "Empty grid."))

(defgeneric initialize (game &key probability &allow-other-keys)
  (:documentation "Render the current game state using the supplied renderer."))

(defgeneric render (game renderer &key &allow-other-keys)
  (:documentation "Render the current game state using the supplied renderer."))

(defgeneric iterate (game &key steps)
  (:documentation "Advance the game by the specified number of steps."))

(defgeneric set-value (game value &key &allow-other-keys)
  (:documentation "Set grid at location to specified value."))

(defmethod clear ((game game-of-life))
  (with-slots (width height depth grid cur-idx) game
    (setf cur-idx 0)
    (when (not grid)
      (setf grid (make-array (list width height depth) :element-type 'boolean :initial-element nil)))
    (loop for k below depth do
         (loop for i below height do
              (loop for j below width do
                   (setf (aref grid i j k) nil))))))

(defmethod initialize ((game game-of-life) &key (probability 0.45))
  (clear game)
  (with-slots (width height grid cur-idx) game
    (loop for i below height do
         (loop for j below width do
              (setf (aref grid i j 0) (< (random 1.0) probability))))
    (setf cur-idx 0))
  game)

(defmethod initialize ((game game-of-life) &key (probability 0.45))
  (clear game)
  (with-slots (width height grid cur-idx) game
    (loop for i below height do
         (loop for j below width do
              (setf (aref grid i j 0) (< (random 1.0) probability))))
    (setf cur-idx 0))
  game)

(defmethod set-value ((game conways-game-of-life) value &key (x) (y))
  (with-slots (grid cur-idx) game
    (setf (aref grid y x cur-idx) value))
  game)

(defmethod set-value ((game toroid-life) value &key (x) (y) (z))
  (with-slots (grid cur-idx) game
    (setf (aref grid y x z) value))
  game)

(defgeneric count-neighbors (game i j k)
  (:documentation "Count neighbors at location i j k"))

(defmethod count-neighbors ((game game-of-life) i j k)
  (with-slots (width height grid) game
    "Count the neighbors of grid location i j k"
    (let ((ip (if (= 0 i)
                  (1- height)
                  (1- i)))
          (jp (if (= 0 j)
                  (1- width)
                  (1- j)))
          (in (if (>= i (1- width))
                  0
                  (1+ i)))
          (jn (if (>= j (1- height))
                  0
                  (1+ j)))
          (count 0))
      (when (aref grid ip jp k) (incf count))
      (when (aref grid ip  j k) (incf count))
      (when (aref grid ip jn k) (incf count))
      (when (aref grid  i jp k) (incf count))
      (when (aref grid  i jn k) (incf count))
      (when (aref grid in jp k) (incf count))
      (when (aref grid in  j k) (incf count))
      (when (aref grid in jn k) (incf count))
      count)))

(defmethod iterate ((game conways-game-of-life) &key (steps 1))
  (with-slots (grid height width cur-idx) game
    (dotimes (cs steps)
      (declare (ignorable cs))
      (let ((nl (if (= cur-idx 0) 1 0)))
        (loop for i from 0 below height do
             (loop for j from 0 below width do
                  (let ((nc (count-neighbors game i j cur-idx)))
                    (cond ((and (aref grid i j cur-idx) (< nc 2))
                           (setf (aref grid i j nl) nil))
                          
                          ((and (aref grid i j cur-idx) (or (= nc 2) (= nc 3)))
                           (setf (aref grid i j nl) t))
                          
                          ((and (aref grid i j cur-idx) (> nc 3))
                           (setf (aref grid i j nl) nil))

                          ((and (not (aref grid i j cur-idx)) (= 3 nc))
                           (setf (aref grid i j nl) t))
                          (t
                           (setf (aref grid i j nl) nil))))))
        (setf cur-idx nl))))
  game)

(defmethod render ((game conways-game-of-life) (renderer text-renderer) &key &allow-other-keys)
  (with-slots (width height grid cur-idx) game
    (with-slots (stream) renderer
      (let ((fs (format nil "~~%~~~a,,,'-a~~%" (1+(* 2 width)))))
        (format stream fs "")
        (dotimes (i height)
          (dotimes (j width)
            (format stream "|~a" (if (aref grid i j cur-idx) "X" " ")))
          (format stream "|")
          (format stream fs "")))))
  game)

(defmethod render ((game conways-game-of-life) (renderer gl-renderer) &key (show nil) &allow-other-keys)
  (with-slots (viewer) renderer
    (let ((blocks (make-instance 'clgl:primitives)))
      (with-slots (width height grid cur-idx) game
        (dotimes (i height)
          (dotimes (j width)
            (when (aref grid j i cur-idx)
              (clgl:add-wire-quad blocks
                                    (vec4 0.0 0.8 0.0 1.0)
                                    (vec3 j i 0)
                                    (vec3 j (1+ i) 0)
                                    (vec3 (1+ j) (1+ i) 0)
                                    (vec3 (1+ j) i 0))))))
    (clgl:add-object viewer 'life blocks)
    (when show (clgl:show-viewer viewer nil))
    viewer)))

(defmethod initialize ((game toroid-life) &key (probability 0.45) &allow-other-keys)
  (with-slots (width height depth grid cur-idx) game
    (if (not grid)
        (setf grid (make-array (list width height depth) :element-type 'boolean :initial-element nil))
        (loop for i below height do
             (loop for j below width do
                  (loop for k below depth do
                       (setf (aref grid i j k) nil)))))
    (setf cur-idx 0))
  game)


(defun spinner-test (&key (steps 5))
  (let ((game (make-instance 'tlife:conways-game-of-life :width 5 :height 5 ))
        (rend (make-instance 'tlife:text-renderer)))
    (tlife:clear game)
    (tlife:set-value game t :x 2 :y 1)
    (tlife:set-value game t :x 2 :y 2)
    (tlife:set-value game t :x 2 :y 3)
    (tlife:render game rend)
    (dotimes (s steps)
      (tlife:iterate game)
      (tlife:render game rend))
    game))

(defun box-test (&key (steps 4))
  (let ((game (make-instance 'tlife:conways-game-of-life :width 4 :height 4 ))
        (rend (make-instance 'tlife:text-renderer)))
    (tlife:clear game)
    (tlife:set-value game t :x 1 :y 1)
    (tlife:set-value game t :x 1 :y 2)
    (tlife:set-value game t :x 2 :y 1)
    (tlife:set-value game t :x 2 :y 2)
    (tlife:render game rend)
    (dotimes (s steps)
      (tlife:iterate game)
      (tlife:render game rend))
    game))

(defun glider-test (&key (width 6) (height 6) (steps 24))
  (let ((game (make-instance 'tlife:conways-game-of-life :width 6 :height 6 ))
        (rend (make-instance 'tlife:text-renderer)))
    (tlife:clear game)
    (tlife:set-value game t :x 2 :y 0)
    (tlife:set-value game t :x 2 :y 1)
    (tlife:set-value game t :x 2 :y 2)
    (tlife:set-value game t :x 1 :y 2)
    (tlife:set-value game t :x 0 :y 1)
    (tlife:render game rend)
    (dotimes (s steps)
      (tlife:iterate game)
      (tlife:render game rend))
    game))


