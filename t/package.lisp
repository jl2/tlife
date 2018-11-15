;;;; package.lisp
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

(in-package :cl-user)
(defpackage :tlife.test
  (:use :cl
        :fiveam
        :alexandria
        :tlife))

(in-package :tlife.test)

(def-suite :tlife)
(in-suite :tlife)

(test spinner
  (let* ((game (game (make-instance 'tlife:conways-game-of-life :width 5 :height 5 )))
         (spinner (tlife:create-spinner game 0 0))
         (rend (make-instance 'tlife:text-renderer)))
    (tlife:render spinner rend)
    (dotimes (s 5)
      (tlife:iterate spinner)
      (tlife:render spinner rend))
    (is (equalp spinner (tlife:create-spinner)))))

(test box
  (let* ((game (game (make-instance 'tlife:conways-game-of-life :width 4 :height 4 )))
         (box (tlife:create-box game 0 0))
         (rend (make-instance 'tlife:text-renderer)))
    (tlife:render box rend)
    (dotimes (s 4)
      (tlife:iterate box)
      (tlife:render box rend))
    (is (equalp box (tlife:create-box)))))

(test glider
  (let* ((game (make-instance 'tlife:conways-game-of-life :width 6 :height 6 ))
         (glider (tlife:create-glider game 0 0))
         (rend (make-instance 'tlife:text-renderer)))
    (tlife:render glider rend)
    (dotimes (s 24)
      (tlife:iterate glider)
      (tlife:render glider rend))
    (is (equalp glider (tlife:create-glider)))))
