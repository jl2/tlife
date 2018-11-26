;;;; toroid-life.lisp 
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


(defclass toroid-life (game-of-life)
  ((width :type fixnum :initform 10 :initarg :width)
   (height :type fixnum :initform 10 :initarg :height)
   (depth :type fixnum :initform 200 :initarg :depth)
   (rotate :type fixnum :initform 0 :initarg :rotate)))

(defmethod iterate ((game toroid-life) &key (steps 1))
  (with-slots (grid height width depth cur-idx) game
    (dotimes (cs steps)
      (declare (ignorable cs))
      (let ((nl (if (= cur-idx (1- depth)) 0 (1+ cur-idx))))
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

(defmethod render ((game toroid-life) (renderer gl-renderer) &key (show nil) (fill nil) &allow-other-keys)
  (with-slots (viewer) renderer
    (let ((blocks (make-instance 'clgl:primitives)))
      (with-slots (width height depth grid cur-idx) game
        (dotimes (k depth)
          (dotimes (j height)
            (dotimes (i width)
              (when (grid-at game i j k)
              (if fill
                  (clgl:add-solid-box blocks
                                        (vec4 0.0 0.8 0.0 1.0)
                                        (vec3 j i k)
                                        (vec3 (1+ j) i k)
                                        (vec3 (1+ j) (1+ i) k)
                                        (vec3 j (1+ i) k)

                                        (vec3 j i (1+ k))
                                        (vec3 (1+ j) i (1+ k))
                                        (vec3 (1+ j) (1+ i) (1+ k))
                                        (vec3 j (1+ i) (1+ k)))
                  (clgl:add-wire-box blocks
                                      (vec4 0.0 0.8 0.0 1.0)
                                      (vec3 j i k)
                                      (vec3 (1+ j) i k)
                                      (vec3 (1+ j) (1+ i) k)
                                      (vec3 j (1+ i) k)
                                      (vec3 j i (1+ k))
                                      (vec3 (1+ j) i (1+ k))
                                      (vec3 (1+ j) (1+ i) (1+ k))
                                      (vec3 j (1+ i) (1+ k)))))))))
      (clgl:add-object viewer 'life blocks)
      (when show
        (clgl:show-viewer viewer nil))
      viewer)))

(defmethod render ((game toroid-life) (renderer renderman-renderer)
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
        (dotimes (k depth)
          (dotimes (j height)
            (dotimes (i width)
              (when (grid-at game i j k)
                (ribgen:transform-begin ribs)
                (ribgen:translate ribs j i k)
                (ribgen:color ribs 0 255 0)
                (ribgen:polygon ribs (list "\"P\" ["
                                           0 0 0 
                                           0 1 0
                                           1 1 0
                                           1 0 0 "]"))
                (ribgen:transform-end ribs))))))
      (ribgen:world-end ribs)
      (ribgen:frame-end ribs))))
