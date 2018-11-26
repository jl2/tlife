;;;; conways-game-of-life.lisp 
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

(defclass conways-game-of-life (game-of-life)
  ((width :type fixnum :initform 20 :initarg :width)
   (height :type fixnum :initform 20 :initarg :height)
   (depth :type fixnum :initform 2 :initarg :depth)))

(defmethod set-value ((game conways-game-of-life) value &key (i) (j))
  (with-slots (grid cur-idx) game
    (setf (grid-at game i j cur-idx) value))
  game)

(defmethod iterate ((game conways-game-of-life) &key (steps 1))
  (with-slots (grid height width cur-idx) game
    (dotimes (cs steps)
      (declare (ignorable cs))
      (let ((nl (if (= cur-idx 0) 1 0)))
        (dotimes (j height)
          (dotimes (i width)
            (let ((nc (count-neighbors game i j cur-idx)))
              (cond ((and (grid-at game i j cur-idx) (< nc 2))
                     (setf (grid-at game i j nl) nil))
                    
                    ((and (grid-at game i j cur-idx) (or (= nc 2) (= nc 3)))
                     (setf (grid-at game i j nl) t))
                    
                    ((and (grid-at game i j cur-idx) (> nc 3))
                     (setf (grid-at game i j nl) nil))

                    ((and (not (grid-at game i j cur-idx)) (= 3 nc))
                     (setf (grid-at game i j nl) t))
                    (t
                     (setf (grid-at game i j nl) nil))))))
        (setf cur-idx nl))))
  game)

(defun create-spinner (game i j)
    (tlife:clear game)
    (tlife:set-value game t :i (+ i 2) :j (+ j 1))
    (tlife:set-value game t :i (+ i 2) :j (+ j 2))
    (tlife:set-value game t :i (+ i 2) :j (+ j 3))
  game)
  
(defun create-box (game i j)
    (tlife:clear game)
    (tlife:set-value game t :i (+ i 1) :j (+ j 1))
    (tlife:set-value game t :i (+ i 1) :j (+ j 2))
    (tlife:set-value game t :i (+ i 2) :j (+ j 1))
    (tlife:set-value game t :i (+ i 2) :j (+ j 2))
    game)

(defun create-glider (game i j)
    (tlife:clear game)
    (tlife:set-value game t :i (+ i 2) :j (+ j 0))
    (tlife:set-value game t :i (+ i 2) :j (+ j 1))
    (tlife:set-value game t :i (+ i 2) :j (+ j 2))
    (tlife:set-value game t :i (+ i 1) :j (+ j 2))
    (tlife:set-value game t :i (+ i 0) :j (+ j 1))
    game)


(defmethod render ((game conways-game-of-life) (renderer text-renderer) &key &allow-other-keys)
  (with-slots (width height grid cur-idx) game
    (with-slots (stream) renderer
      (let ((fs (format nil "~~%~~~a,,,'-a~~%" (1+(* 2 width)))))
        (format stream fs "")
        (dotimes (j height)
          (dotimes (i width)
            (format stream "|~a" (if (grid-at game i j cur-idx) "X" " ")))
          (format stream "|")
          (format stream fs "")))))
  game)

(defmethod render ((game conways-game-of-life) (renderer gl-renderer) &key (show nil) (fill nil) &allow-other-keys)
  (with-slots (viewer) renderer
    (let ((blocks (make-instance 'clgl:primitives)))
      (with-slots (width height grid cur-idx) game
        (dotimes (j height)
          (dotimes (i width)
            (when (grid-at game i j cur-idx)
              (if fill
                  (clgl:add-filled-quad blocks
                                        (vec4 0.0 0.8 0.0 1.0)
                                        (vec3 j i 0)
                                        (vec3 (1+ j) i 0)
                                        (vec3 (1+ j) (1+ i) 0)
                                        (vec3 j (1+ i) 0))
                  (clgl:add-wire-quad blocks
                                      (vec4 0.0 0.8 0.0 1.0)
                                      (vec3 j i 0)
                                      (vec3 (1+ j) i 0)
                                      (vec3 (1+ j) (1+ i) 0)
                                      (vec3 j (1+ i) 0)))))))
      (clgl:add-object viewer 'life blocks)
      (when show (clgl:show-viewer viewer nil))
      viewer)))

(defmethod render ((game conways-game-of-life) (renderer renderman-renderer)
                   &key
                     (rib-file-name "life.rib")
                     (tif-file-name "life.tiff")
                     (frames 120) &allow-other-keys)
  (with-output-to-file (ribs rib-file-name :if-exists :supersede)
    (ribgen:begin ribs rib-file-name)

    (dotimes (n frames)
      (ribgen:frame-begin ribs n)
      (ribgen:display ribs tif-file-name "tiff" "rgba")
      (ribgen:world-begin ribs)
      (with-slots (depth width height grid cur-idx) game
        (dotimes (j height)
          (dotimes (i width)
            (when (grid-at game i j cur-idx)
              (ribgen:transform-begin ribs)
              (ribgen:translate ribs j i 0)
              (ribgen:color ribs 0 255 0)
              (ribgen:polygon ribs (list "P"
                                         0 0 0 
                                         0 1 0
                                         1 1 0
                                         1 0 0))
              (ribgen:transform-end ribs)))))
      (ribgen:world-end ribs)
      (ribgen:frame-end ribs))))
