;;; zenity-color-picker.el --- Insert and adjust colors using Zenity  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Samuel Laurén

;; Author: Samuel Laurén <samuel.lauren@iki.fi>
;; Keywords: colors
;; Version: 0.1.0
;; URL: https://bitbucket.org/Soft/zenity-color-picker.el
;; Package-Requires: ((emacs "24.4"))

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

;;; KNOWN ISSUES

;; - Color presentation can change between adjustments
;; - No support for three-letter hex colors


;;; Code:

(require 'cl-lib)
(require 'thingatpt)

(defvar zcp-zenity-bin "zenity")

(defun zcp-to-color (str)
  (cond ((zcp-hex-color-p str) (zcp-hex-to-color str))
        ((zcp-rgb-color-p str) (zcp-rgb-to-color str))
        (t nil)))

(defconst zcp-hex-color-regexp
  "#\\([a-f0-9]\\{2\\}\\)\\([a-f0-9]\\{2\\}\\)\\([a-f0-9]\\{2\\}\\)")

(defconst zcp-rgb-color-regexp
  "rgb(\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\),\\([0-9]\\{1,3\\}\\))")

(defun zcp-hex-color-p (str)
  "Check if STR is a valid hex formatted color string"
  (and (string-match-p zcp-hex-color-regexp (downcase str)) t))

(defun zcp-hex-to-color (str)
  (save-match-data
    (string-match zcp-hex-color-regexp str)
    (mapcar
     (lambda (index)
       (string-to-number (match-string index str) 16))
     '(1 2 3))))

(defun zcp-rgb-color-p (str)
  "Check if STR is a valid rgb formatted color string"
  (and (string-match-p zcp-rgb-color-regexp str) t))

(defun zcp-rgb-to-color (str)
  (save-match-data
    (string-match zcp-rgb-color-regexp str)
    (mapcar
     (lambda (index)
       (string-to-number (match-string index str)))
     '(1 2 3))))

(defun zcp--short-p (n)
  (= (lsh n -4) (logand n #xf)))

(defun zcp-color-to-hex (color &optional short)
  "Convert (R G B) list to hex string. If SHORT is not NIL, try
to use CSS-style shorthand notation."
  (cl-destructuring-bind (r g b) color
    (if (and short (zcp--short-p r) (zcp--short-p g) (zcp--short-p b))
        (format "#%x%x%x"
                (logand r #xf)
                (logand g #xf)
                (logand b #xf))
      (format "#%02x%02x%02x" r g b))))

(defun zcp-color-picker (&optional initial)
  "Execute zenity color picker.

Returns the selected color as a list of form (RED GREEN BLUE) or
NIL if selection was cancelled."
  (if (executable-find zcp-zenity-bin)
      (with-temp-buffer
        (pcase (apply 'call-process
                      (append `(,zcp-zenity-bin nil (,(current-buffer) nil) nil "--color-selection")
                              (when initial
                                (list (format "--color=%s" (zcp-color-to-hex initial)))))) 
          (`0 (zcp-to-color (buffer-string)))))
    (error "Could not find %s" zcp-zenity-bin)))

(defun zcp-bounds-of-color-at-point ()
  (save-excursion
    (skip-chars-backward "#a-fA-F0-9")
    (when (looking-at zcp-hex-color-regexp)
      (cons (point) (match-end 0)))))

(put 'color 'bounds-of-thing-at-point
     'zcp-bounds-of-color-at-point)

(defun zcp-color-at-point ()
  (let ((color (thing-at-point 'color t)))
    (when color
      (zcp-to-color color))))

(defun zcp-transform-color-at-point (transform)
  (let ((bounds (zcp-bounds-of-color-at-point))
        (old (zcp-color-at-point)))
    (when old
      (let ((new (funcall transform old)))
        (when new
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert (zcp-color-to-hex new))))))))

;;;###autoload
(defun zcp-adjust-color-at-point ()
  "Adjust color at point"
  (interactive)
  (zcp-transform-color-at-point #'zcp-color-picker))

;;;###autoload
(defun zcp-insert-color-at-point ()
  "Insert color at point"
  (interactive)
  (let ((color (zcp-color-picker)))
    (insert (zcp-color-to-hex color))))

;;;###autoload
(defun zcp-color-at-point-dwim ()
  "Adjust or insert color at point"
  (interactive)
  (if (zcp-color-at-point)
      (zcp-adjust-color-at-point)
    (zcp-insert-color-at-point)))

(provide 'zenity-color-picker)
;;; zenity-color-picker.el ends here
