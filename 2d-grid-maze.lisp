;; grid-maze.lisp
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

(defclass 2d-grid-maze (maze)
  ((width :initarg :width :initform 100 :type fixnum)
   (height :initarg :height :initform 100 :type fixnum)
   (walls :type (array (integer 0 #2r1111)))))

(defclass 2d-grid-position (maze-positon)
  ((row :type fixnum :initform 0 :initarg :row)
   (column :type fixnumn :initform 0 :initarg :column)))

(defmethod initialize-instance :after ((instance 2d-grid-maze) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (with-slots (width height walls) instance
      (setf walls (make-array `(,width ,height) :initial-element 0 :element-type 'fixnum))))

(defgeneric above (maze position)
  (:documentation "Returns the postion immediately above position.")
  (:method ((maze 2d-grid-maze) (position 2d-grid-position))
    (with-slots (row) position
      (with-slots (height) maze
        (if (< row height)
            (1+ row)
            nil)))))

(defgeneric below (maze position)
  (:documentation "Returns the position immediately below position.")
  (:method ((maze 2d-grid-maze) (position 2d-grid-position))
    (with-slots (row) position
      (if (> row 0)
          (- row 1)
          nil))))

(defgeneric left (maze position)
  (:documentation "Returns the position immediately left of position.")
  (:method ((maze 2d-grid-maze) (position 2d-grid-position))
    (with-slots (column) position
      (if (> column 0)
          (- column 1)
          nil))))

(defgeneric right (maze position)
  (:documentation "Returns the position immediately right of postion.")
  (:method ((maze 2d-grid-maze) (position 2d-grid-position))
    (with-slots (column) position
      (with-slots (width) maze
        (if (< column width)
            (1+ column)
            nil)))))

(defmethod neighbors ((maze 2d-grid-maze) (position 2d-grid-position))
  `#((left maze position)
     (right maze position)
     (above maze position)
     (below maze position)))

(defmethod adjacent-p ((maze 2d-grid-maze) (position-1 2d-grid-position) (position-2 2d-grid-position))
  nil)

(defgeneric connected-p ((maze 2d-grid-maze) (position-1 2d-grid-position) (position-2 2d-grid-position))
  nil)

(defgeneric to-text (stream (maze 2d-grid-maze))
  (:documentation "Writes a text representation of the maze to stream, if possible.")
  (:method (stream (maze t))
    (format stream
            (concatenate 'string
                         "╔╗~%"
                         "╚╝~%"
                         "Maze is too abstract to be visualized as text!~%"))))

(defgeneric to-svg (stream (maze 2d-grid-maze))
  (:documentation "Writes an SVG representation of the maze to stream, if possible.")
  (:method (stream (maze t))
    (format stream
            (concatenate 'string
                         "╔╗~%"
                         "╚╝~%"
                         "Maze is too abstract to be visualized as SVG!~%"))))
