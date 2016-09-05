;;; gitter.el --- An Emacs Gitter client  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; URL: https://github.com/xuchunyang/gitter.el
;; Keywords: Gitter, chat, client, Internet
;; Version: 0.0

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

;;

;;; Code:

(require 'json)

(defgroup gitter nil
  "An Emacs Gitter client."
  :group 'comm)

;; FIXME: Use `defcustom' instead
(defvar gitter-token nil
  "Your Gitter Personal Access Token.

To get your token:
1) Visit URL `https://developer.gitter.im'
2) Click Sign in (top right)
3) You will see your personal access token at
   URL `https://developer.gitter.im/apps'

DISCLAIMER
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC.")

(defcustom gitter-curl-program-name "curl"
  "Name/path by which to invoke the curl program."
  :group 'gitter
  :type 'string)

(defvar gitter--root-endpoint "https://api.gitter.im")

(defun gitter--request (method resource &optional params data _noerror)
  ;; PARAMS and DATA should be nil or alist
  (with-current-buffer (generate-new-buffer " *curl*")
    (let* ((p (and params (concat "?" (gitter--url-encode-params params))))
           (d (and data (json-encode-list data)))
           (url (concat gitter--root-endpoint resource p))
           (headers
            (append (and d '("Content-Type: application/json"))
                    (list "Accept: application/json"
                          (format "Authorization: Bearer %s" gitter-token))))
           (args (gitte--curl-args url method headers d)))
      (if (zerop (apply #'call-process gitter-curl-program-name nil t nil args))
          (progn (goto-char (point-min))
                 (re-search-forward "^\r$")
                 (gitter--read-response))
        (error "curl failed")
        (display-buffer (current-buffer))))))

(defun gitter--url-encode-params (params)
  (mapconcat (pcase-lambda (`(,key . ,val))
               (concat (url-hexify-string (symbol-name key)) "="
                       (url-hexify-string val)))
             params "&"))

(defun gitte--curl-args (url method &optional headers _data)
  (let ((args ()))
    (push "-s" args)
    (push "-i" args)
    (push "-X" args)
    (push method args)
    (dolist (h headers)
      (push "-H" args)
      (push h args))
    (nreverse (cons url args))))

(defun gitter--read-response ()
  (let ((json-object-type 'alist)
        (json-array-type  'list)
        (json-key-type    'symbol)
        (json-false       nil)
        (json-null        nil))
    (json-read)))

(defvar gitter--user-rooms nil)

;; FIXME: Testing
(defun gitter-list-rooms ()
  "List rooms the current user is in."
  (interactive)
  (setq gitter--user-rooms (gitter--request "GET" "/v1/rooms"))
  (completing-read "Open room: "
                   (mapcar (lambda (alist) (cdr (assq 'name alist)))
                           gitter--user-rooms)))

(provide 'gitter)
;;; gitter.el ends here
