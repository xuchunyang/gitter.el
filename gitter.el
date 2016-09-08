;;; gitter.el --- An Emacs Gitter client  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; URL: https://github.com/xuchunyang/gitter.el
;; Package-Requires: ((emacs "24.1") (let-alist "1.0.4") (markdown-mode "2.1"))
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

;; TODO Fill
;; TODO Make message read-onl?
;; TODO Markup plain like http://freefoodcamp.com
;; TODO Markup link like [Bulma](http://bulma.io/)
;; TODO Markup @mention
;; TODO Markup link to picture like
;; ![](https://cdn01.gitter.im/_s/S8d79c25/images/emoji/chicken.png)
;; [![blob](https://files.gitter.im/FreeCodeCamp/chinese/2otH/thumb/blob.png)](https://files.gitter.im/FreeCodeCamp/chinese/2otH/blob)
;; TODO Markup multiline code (I guesst it is Github Flavored. Gitter and maybe
;; discourse can highlight code block without a language tag, I don't know this
;; can be done in Emacs)
;; TODO @mention completion

;;; Code:

(require 'json)
(require 'markdown-mode)

(eval-when-compile (require 'let-alist))

;;; Compatibility

(eval-and-compile
  ;; Added in Emacs 24.3
  (defmacro defvar-local (var val &optional docstring)
    "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
    (declare (debug defvar) (doc-string 3))
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'progn (list 'defvar var val docstring)
          (list 'make-variable-buffer-local (list 'quote var))))

  ;; Add in Emacs 24.4
  (unless (featurep 'subr-x)
    (defsubst string-trim-left (string)
      "Remove leading whitespace from STRING."
      (if (string-match "\\`[ \t\n\r]+" string)
          (replace-match "" t t string)
        string))

    (defsubst string-trim-right (string)
      "Remove trailing whitespace from STRING."
      (if (string-match "[ \t\n\r]+\\'" string)
          (replace-match "" t t string)
        string))

    (defsubst string-trim (string)
      "Remove leading and trailing whitespace from STRING."
      (string-trim-left (string-trim-right string)))

    (defsubst string-empty-p (string)
      "Check whether STRING is empty."
      (string= string ""))))

;;; Customization

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

;;; Main

(defvar gitter--debug-p t)
;; TODO Use only one place for debug
(defmacro gitter--debug (format-string &rest args)
  `(when gitter--debug-p
     (message ,(concat "[Gitter] " format-string) ,@args)))

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
           (args (gitter--curl-args url method headers d)))
      (gitter--debug "Calling curl with %S" args)
      (if (zerop (apply #'call-process gitter-curl-program-name nil t nil args))
          (progn (goto-char (point-min))
                 (gitter--read-response))
        (error "curl failed")
        (display-buffer (current-buffer))))))

(defun gitter--url-encode-params (params)
  (mapconcat
   (lambda (pair)
     (pcase-let ((`(,key . ,val) pair))
       (concat (url-hexify-string (symbol-name key)) "="
               (url-hexify-string val))))
   params "&"))

(defun gitter--curl-args (url method &optional headers data)
  (let ((args ()))
    (push "-s" args)
    ;; (push "-i" args)
    (push "-X" args)
    (push method args)
    (dolist (h headers)
      (push "-H" args)
      (push h args))
    (when data
      (push "-d" args)
      (push data args))
    (nreverse (cons url args))))

(defun gitter--read-response ()
  (let ((json-object-type 'alist)
        (json-array-type  'list)
        (json-key-type    'symbol)
        (json-false       nil)
        (json-null        nil))
    (json-read)))

(defvar-local gitter--output-marker nil
  "The marker where process output (i.e., message) should be insert.")

(defvar-local gitter--input-marker nil
  "The markder where input (i.e., composing a new message) begins.")

(defvar-local gitter--last-message nil
  "The last message has been inserted.")

(defvar gitter--prompt
  (concat (propertize "──────────[ Compose Area.  Send M-x gitter-send-message"
                      'face 'font-lock-comment-face)
          "\n"))

(defun gitter--open-room (name id)
  (with-current-buffer (get-buffer-create (concat "#" name))
    (unless (process-live-p (get-buffer-process (current-buffer)))
      ;; Setup markers
      (unless gitter--output-marker
        (insert gitter--prompt)
        (setq gitter--output-marker (point-min-marker))
        (set-marker-insertion-type gitter--output-marker t)
        (setq gitter--input-marker (point-max-marker)))
      (let* ((url (format "https://stream.gitter.im/v1/rooms/%s/chatMessages" id))
             (headers
              (list "Accept: application/json"
                    (format "Authorization: Bearer %s" gitter-token)))
             (proc
              ;; NOTE According to (info "(elisp) Asynchronous Processes")
              ;; we should use a pipe by let-binding `process-connection-type'
              ;; to nil, however, it doesn't working very well on my system
              (apply #'start-process
                     (concat "curl-streaming-process-" name)
                     (current-buffer)
                     gitter-curl-program-name
                     (gitter--curl-args url "GET" headers)))
             ;; Paser response (json) incrementally
             ;; Use a scratch buffer to accumulate partial output
             (parse-buf (generate-new-buffer
                         (concat " *Gitter search parse for " (buffer-name)))))
        (process-put proc 'room-id id)
        (process-put proc 'parse-buf parse-buf)
        (set-process-filter proc #'gitter--output-filter)))
    (switch-to-buffer (current-buffer))))

(defun gitter--output-filter (process output)
  ;; FIXME
  (with-current-buffer (get-buffer-create "*Log*")
    (goto-char (point-max))
    (insert output "\n\n"))

  (let ((results-buf (process-buffer process))
        (parse-buf (process-get process 'parse-buf)))
    (when (buffer-live-p results-buf)
      (with-current-buffer parse-buf
        ;; Insert new data
        (goto-char (point-max))
        (insert output)
        (condition-case err
            (progn
              (goto-char (point-min))
              ;; `gitter--read-response' moves point
              (let* ((response (gitter--read-response)))
                (let-alist response
                  (with-current-buffer results-buf
                    (save-excursion
                      (save-restriction
                        (goto-char (marker-position gitter--output-marker))
                        (if (and gitter--last-message
                                 (string= .fromUser.username
                                          (let-alist gitter--last-message
                                            .fromUser.username)))
                            ;; Delete one newline
                            (delete-char -1)
                          (insert
                           (propertize
                            ;; FIXME Don't use multiline prompt?
                            (format "──────────[ %s @%s"
                                    .fromUser.displayName
                                    .fromUser.username)
                            'face 'font-lock-comment-face)
                           "\n"))
                        (insert
                         (gitter--render-emoji
                          (gitter--fontify-markdown
                           ;; TODO Delete trailing spaces on every line as well
                           (string-trim .text)))
                         "\n"
                         "\n")
                        (setq gitter--last-message response))))))
              (delete-region (point-min) (point)))
          (error
           ;; FIXME
           (with-current-buffer (get-buffer-create "*Debug Gitter Log")
             (goto-char (point-max))
             (insert (format "The error was: %s" err)
                     "\n"
                     output))))))))

(defun gitter--fontify-markdown (text)
  (with-temp-buffer
    ;; Work-around for `markdown-mode'. It looks like markdown-mode treats ":"
    ;; specially (I don't know the reason), this strips the specificity (I don't
    ;; know how either)
    (insert "\n\n")
    (insert text)
    (delay-mode-hooks (markdown-mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-substring 3 (point-max))))

(defun gitter--render-emoji (text)
  (cond ((require 'emojify nil t)
         (with-temp-buffer
           (insert text)
           (emojify-display-emojis-in-region (point-min) (point-max))
           (buffer-string)))
        ((require 'emoji-cheat-sheet-plus nil t)
         (with-temp-buffer
           (insert text)
           (emoji-cheat-sheet-plus--create-cache)
           (emoji-cheat-sheet-plus--display-region (point-min) (point-max))
           (buffer-string)))
        ((require 'company-emoji nil t)
         (with-temp-buffer
           (insert text)
           (goto-char (point-min))
           (let (emoji-list emoji)
             (while (re-search-forward ":[^:]+:" nil t)
               (unless emoji-list
                 (setq emoji-list (company-emoji--create-list)))
               (setq emoji (car (member (match-string 0) emoji-list)))
               (when emoji
                 (replace-match (get-text-property 0 :unicode emoji) t t))))
           (buffer-string)))
        (t text)))

(defvar gitter--user-rooms nil)

;;;###autoload
(defun gitter ()
  "Open a room."
  (interactive)
  (unless gitter--user-rooms
    (setq gitter--user-rooms (gitter--request "GET" "/v1/rooms")))
  ;; FIXME Assuming room name is unique because of `completing-read'
  (let* ((rooms (mapcar (lambda (alist)
                          (let-alist alist
                            (cons .name .id)))
                        gitter--user-rooms))
         (name (completing-read "Open room: " rooms nil t))
         (id (cdr (assoc name rooms))))
    (gitter--open-room name id)))

;; TODO: Setup a gitter major or minor mode for key binding
(defun gitter-send-message ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and proc (process-live-p proc))
      (let* ((id (process-get proc 'room-id))
             (resource (format "/v1/rooms/%s/chatMessages" id))
             (msg (string-trim
                   (buffer-substring
                    (marker-position gitter--input-marker)
                    (point-max)))))
        (if (string-empty-p msg)
            (error "Can't send empty message")
          (gitter--request "POST" resource
                           nil `((text . ,msg)))
          (delete-region (marker-position gitter--input-marker)
                         (point-max)))))))

(provide 'gitter)
;;; gitter.el ends here
