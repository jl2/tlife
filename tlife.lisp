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

(defclass renderer () ())

(defmethod add-quad (renderer pt1 pt2 pt3 pt4)
  "Draw a quad.")

(defmethod add-cell (renderer
                     pt1 pt2 pt3 pt4
                     pt5 pt6 pt7 pt8)
  "Draw a 3D cell.")

(defclass text-renderer (renderer)
  ((stream :initform *standard-output* :initarg :stream)))

(defclass life-game ()
  ())

(defclass conways-life-game (life-game)
  ((width :initform 10 :initarg :width)
   (height :initform 10 :initarg :height)
   (grid :initform nil :type (or null (simple-array boolean (* * 2))))
   (initial-probability :initform 0.5 :initarg prob :type double-float)
   (current-level :initform 0)))

(defmethod print-object ((object conways-life-game) stream)
  (format stream "Grid:~%")
  (with-slots (width height grid current-level initial-probability) object
    (loop for idx below 2 do
         (format stream "  Level: ~a~%" idx)
         (loop for i below height do
              (format stream "    ")
              (loop for j below width do
                   (format stream "~a" (if (aref grid i j idx) "X" " ")))
              (format stream "~%")))
    (format stream "current-level: ~a~%" current-level)
    (format stream "initial-probability: ~a~%" initial-probability)))

(defclass toroid-life (life-game)
  ((width :initform 10 :initarg :width :type fixnum)
   (height :initform 10 :initarg :height :type fixnum)
   (depth :initform 200 :initarg :depth :type fixnum)
   (grid :initform nil :type (or null (simple-array boolean (* * *))))
   (cur-idx :initform 0 :type fixnum)))


(defgeneric initialize (game)
  (:documentation "Render the current game state using the supplied renderer."))

(defgeneric render (game renderer)
  (:documentation "Render the current game state using the supplied renderer."))

(defgeneric iterate (game &key steps)
  (:documentation "Advance the game by the specified number of steps."))

(defgeneric clear (game)
  (:documentation "Empty grid."))

(defgeneric set-value (game value &key &allow-other-keys)
  (:documentation "Set grid at location to specified value."))

(defmethod clear ((game conways-life-game))
  (with-slots (width height grid current-level) game
    (setf current-level 0)
    (when (not grid)
      (setf grid (make-array (list width height 2) :element-type 'boolean :initial-element nil)))
    (loop for i below height do
         (loop for j below width do
              (setf (aref grid i j 0) nil)
              (setf (aref grid i j 1) nil)))))

(defmethod initialize ((game conways-life-game))
  (clear game)
  (with-slots (width height grid current-level initial-probability) game
    (loop for i below height do
         (loop for j below width do
              (setf (aref grid i j 0) (< (random 1.0) initial-probability))
              (setf (aref grid i j 1) nil)))
    (setf current-level 0))
  game)

(defmethod set-value ((game conways-life-game) value &key (x) (y))
  (with-slots (grid current-level) game
    (setf (aref grid y x current-level) value))
  game)

(defmethod iterate ((game conways-life-game) &key (steps 1))
  (with-slots (grid height width current-level) game
    (flet ((count-neighbors (i j cl)
             "Count the neighbors of grid location i,j"
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
               (when (aref grid ip jp cl) (incf count))
               (when (aref grid ip  j cl) (incf count))
               (when (aref grid ip jn cl) (incf count))
               (when (aref grid  i jp cl) (incf count))
               (when (aref grid  i jn cl) (incf count))
               (when (aref grid in jp cl) (incf count))
               (when (aref grid in  j cl) (incf count))
               (when (aref grid in jn cl) (incf count))
               ;;(format t "Neighbors: ~a~%" count)
               count)))
      (dotimes (cs steps)
        (declare (ignorable cs))
        (let ((nl (if (= current-level 0) 1 0)))
          (loop for i from 0 below height do
               (loop for j from 0 below width do
                    (let ((nc (count-neighbors i j current-level)))
                      (cond ((and (aref grid i j current-level) (< nc 2))
                             (setf (aref grid i j nl) nil))
                            
                            ((and (aref grid i j current-level) (or (= nc 2) (= nc 3)))
                             (setf (aref grid i j nl) t))
                            
                            ((and (aref grid i j current-level) (> nc 3))
                             (setf (aref grid i j nl) nil))

                            ((and (not (aref grid i j current-level)) (= 3 nc))
                             (setf (aref grid i j nl) t))
                            (t
                             (setf (aref grid i j nl) nil))))))
          (setf current-level nl)))))
  game)

(defmethod render ((game conways-life-game) (renderer text-renderer))
  (with-slots (width height grid current-level) game
    (with-slots (stream) renderer
      (let ((fs (format nil "~~%~~~a,,,'-a~~%" (1+(* 2 width)))))
        (format stream fs "")
        (dotimes (i height)
          (dotimes (j width)
            (format stream "|~a" (if (aref grid i j current-level) "X" " ")))
          (format stream "|")
          (format stream fs "")))))
  game)
  

(defmethod initialize ((game toroid-life))
  (with-slots (width height depth grid cur-idx) game
    (if (not grid)
        (setf grid (make-array (list width height depth) :element-type 'boolean :initial-element nil))
        (loop for i below height do
             (loop for j below width do
                  (loop for k below depth do
                       (setf (aref grid i j k) nil)))))
    (setf cur-idx 0))
  game)

(defmethod view-key-press ((viewer gl-renderer) key scancode action mod-keys)
  (declare (ignorable key scancode action mod-keys))
  (clgl:with-viewer-lock (viewer)
    (format t "Got key: ~a in clgl:viewer:view-key-press~%" (list key scancode action mod-keys))
    (with-slots (last-mouse-position first-click-position objects to-cleanup geometry-modified should-close viewport) viewer
      (cond ((and (eq key :escape) (eq action :press) first-click-position)
             (setf first-click-position nil)
             (setf last-mouse-position nil))
            ((and (eq key :escape) (eq action :press))
             (glfw:set-window-should-close))))))

(defun spinner-test (steps)
  (let ((game (make-instance 'tlife:conways-life-game :width 5 :height 5 ))
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

(defun box-test (steps)
  (let ((game (make-instance 'tlife:conways-life-game :width 4 :height 4 ))
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

(defun glider-test (steps)
  (let ((game (make-instance 'tlife:conways-life-game :width 6 :height 6 ))
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
