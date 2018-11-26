;;;; game-of-life.lisp 
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

(defclass game-of-life ()
  ((width :type fixnum)
   (height :type fixnum)
   (depth :type fixnum)
   (grid :initform nil :type (or null (simple-array boolean (* * *))))
   (cur-idx :initform 0 :type fixnum)))

(defgeneric clear (game)
  (:documentation "Empty grid."))

(defgeneric initialize (game &key probability &allow-other-keys)
  (:documentation "Render the current game state using the supplied renderer."))

(defgeneric iterate (game &key steps)
  (:documentation "Advance the game by the specified number of steps."))

(defgeneric set-value (game value &key &allow-other-keys)
  (:documentation "Set grid at location to specified value."))

(defgeneric count-neighbors (game i j k)
  (:documentation "Count neighbors at location i j k"))


(declaim (inline grid-at (setf grid-at)))
(defun grid-at (game i j k)
  (with-slots (grid) game
    (aref grid k i j)))

(defun (setf grid-at) (new-value game i j k)
  (with-slots (grid) game
    (setf (aref grid k i j) new-value)))

(defmethod clear ((game game-of-life))
  (with-slots (width height depth grid cur-idx) game
    (setf cur-idx 0)
    (when (not grid)
      (setf grid (make-array (list depth width height) :element-type 'boolean :initial-element nil)))
    (dotimes (k depth)
      (dotimes (j height)
        (dotimes (i width)
          (setf (grid-at game i j k) nil))))))

(defmethod initialize ((game game-of-life) &key (probability 0.45))
  (clear game)
  (with-slots (width height grid cur-idx) game
    (dotimes (j height)
      (dotimes (i width)
        (setf (grid-at game i j 0) (< (random 1.0) probability))))
    (setf cur-idx 0))
  game)

(defmethod set-value ((game game-of-life) value &key (i) (j) (k))
  (with-slots (grid cur-idx) game
    (setf (grid-at game i j (if k k cur-idx)) value))
  game)

(defmethod count-neighbors ((game game-of-life) i j k)
  (with-slots (width height grid) game
    "Count the neighbors of grid location i j k"
    (let ((ip (if (= 0 i)
                  (1- width)
                  (1- i)))
          (jp (if (= 0 j)
                  (1- height)
                  (1- j)))
          
          (in (if (>= i (1- width))
                  0
                  (1+ i)))
          (jn (if (>= j (1- height))
                  0
                  (1+ j)))
          
          (count 0))
      (when (grid-at game ip jp k) (incf count))
      (when (grid-at game ip  j k) (incf count))
      (when (grid-at game ip jn k) (incf count))
      (when (grid-at game  i jp k) (incf count))
      (when (grid-at game  i jn k) (incf count))
      (when (grid-at game in jp k) (incf count))
      (when (grid-at game in  j k) (incf count))
      (when (grid-at game in jn k) (incf count))
      count)))

