;;; zenity-color-picker.el --- Insert and adjust colors using Zenity  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Samuel Laurén

;; Author: Samuel Laurén <samuel.lauren@iki.fi>
;; Keywords: colors
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Insert and adjust colors using Zenity. Zenity
;; (https://help.gnome.org/users/zenity/stable/) is obviously required.

;;; Code:

;; TODO:
;; - Keep track of color presentation
;; - Support three-letter hex colors

(eval-when-compile
  (require 'cl)
  (require 'thingatpt))

(defvar zc-zenity-bin "zenity")

(defun zc-to-color (str)
  (cond ((zc-hex-color-p str) (zc-hex-to-color str))
        ((zc-rgb-color-p str) (zc-rgb-to-color str))
        (t nil)))

(defconst zc-hex-color-regexp
  "#\\([a-f0-9]\\{2\\}\\)\\([a-f0-9]\\{2\\}\\)\\([a-f0-9]\\{2\\}\\)")

(defconst zc-rgb-color-regexp
  "rgb(\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\))")

(defun zc-hex-color-p (str)
  "Check if STR is a valid hex formatted color string"
  (and (string-match-p zc-hex-color-regexp (downcase str)) t))

(defun zc-hex-to-color (str)
  (save-match-data
    (string-match zc-hex-color-regexp str)
    (mapcar
     (lambda (index)
       (string-to-number (match-string index str) 16))
     '(1 2 3))))

(defun zc-rgb-color-p (str)
  "Check if STR is a valid rgb formatted color string"
  (and (string-match-p zc-rgb-color-regexp str) t))

(defun zc-rgb-to-color (str)
  (save-match-data
    (string-match zc-rgb-color-regexp str)
    (mapcar
     (lambda (index)
       (string-to-number (match-string index str)))
     '(1 2 3))))

(defun zc-color-to-hex (color)
  (cl-destructuring-bind (r g b) color
    (format "#%02x%02x%02x" r g b)))

(defun zc-color-picker (&optional initial)
  "Execute zenity color picker.

Returns the selected color as a list of form (RED GREEN BLUE) or
NIL if selection was cancelled."
  (if (executable-find zc-zenity-bin)
      (with-temp-buffer
        (pcase (apply 'call-process
                      (append `(,zc-zenity-bin nil (,(current-buffer) nil) nil "--color-selection")
                              (when initial
                                (list (format "--color=%s" (zc-color-to-hex initial)))))) 
          (`0 (zc-to-color (buffer-string)))))
    (error "Could not find zenity")))

(defun zc-bounds-of-color-at-point ()
  (save-excursion
    (skip-chars-backward "#a-fA-F0-9")
    (when (looking-at zc-hex-color-regexp)
      (cons (point) (match-end 0)))))

(put 'color 'bounds-of-thing-at-point
     'zc-bounds-of-color-at-point)

(defun zc-color-at-point ()
  (let ((color (thing-at-point 'color t)))
    (when color
      (zc-to-color color))))

(defun zc-transform-color-at-point (transform)
  (let ((bounds (zc-bounds-of-color-at-point))
        (old (zc-color-at-point)))
    (when old
      (let ((new (funcall transform old)))
        (when new
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert (zc-color-to-hex new))))))))

;;;###autoload
(defun zc-adjust-color-at-point ()
  "Adjust color at point"
  (interactive)
  (zc-transform-color-at-point #'zc-color-picker))

;;;###autoload
(defun zc-insert-color-at-point ()
  "Insert color at point"
  (interactive)
  (let ((color (zc-color-picker)))
    (insert (zc-color-to-hex color))))

;;;###autoload
(defun zc-color-at-point-dwim ()
  "Adjust or insert color at point"
  (interactive)
  (if (zc-color-at-point)
      (zc-adjust-color-at-point)
    (zc-insert-color-at-point)))

(provide 'zenity-color-picker)
;;; zenity-color-picker.el ends here
