;;;; render.lisp 
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
  ((viewer :type clgl:viewer :initform (make-instance 'clgl:viewer :viewport (make-instance 'clgl:2d-viewport)))))

(defclass 3d-gl-renderer (gl-renderer)
  ((viewer :type clgl:viewer :initform (make-instance 'clgl:viewer :viewport (make-instance 'clgl:rotating-viewport)))))

(defclass renderman-renderer (renderer)
  ())

(defmethod render ((game game-of-life) (renderer renderman-renderer)
                   &key
                     (rib-name "life.rib")
                     (tif-name "life.tiff")
                   &allow-other-keys)
  (with-output-to-file (ribs rib-name :if-exists :supersede :if-does-not-exist :create)
    (with-slots (width height depth grid cur-idx) game
      (ribgen:begin ribs rib-name)
      (ribgen:display ribs tif-name "tiff" "rgba")
      (ribgen:world-begin ribs)
      (dotimes (k depth)
        (dotimes (j height)
          (dotimes (i width)
            (when (grid-at game i j k)
              (ribgen:transform-begin ribs)
              (ribgen:translate ribs i j k)
              (ribgen:sphere ribs  0.5 -0.5 0.5 360)
              (ribgen:transform-end ribs)))))
      (ribgen:world-end ribs)
      (ribgen:end ribs))))
