;; jl-maze.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(in-package :jl-maze)

(defclass maze ()
  ())

(defclass maze-position ()
  ())

(defgeneric neighbors (maze position)
  (:documentation "Returns a list of positions in maze that can be visited from position.")
  (:method ((maze t) (position t))
    (declare (ignorable maze position))
    nil))


(defgeneric to-text (stream maze)
  (:documentation "Writes a text representation of the maze to stream, if possible.")
  (:method (stream (maze t))
    (format stream
            (concatenate 'string
                         "╔╗~%"
                         "╚╝~%"
                         "Maze is too abstract to be visualized as text!~%"))))

(defgeneric to-svg (stream maze)
  (:documentation "Writes an SVG representation of the maze to stream, if possible.")
  (:method (stream (maze t))
    (format stream
            (concatenate 'string
                         "╔╗~%"
                         "╚╝~%"
                         "Maze is too abstract to be visualized as SVG!~%"))))
