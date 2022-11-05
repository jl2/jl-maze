;; grid-maze-test.lisp
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

(in-package :jl-maze.test)

(def-suite :jl-grid-maze)
(in-suite :jl-grid-maze)

(test test-grid-maze-creation
  (let ((maze (make-instance 'grid-maze :width 10 :height 9)))
    (with-slots ((walls maze::walls)) maze
      (is-true (= (array-dimension walls 0)
                  10))
      (is-true (= (array-dimension walls 1)
                  9))))

  (let ((maze (make-instance 'grid-maze :width 13 :height 19)))
    (with-slots ((walls maze::walls)) maze
      (is-true (= (array-dimension walls 0)
                  13))
      (is-true (= (array-dimension walls 1)
                  19)))))
