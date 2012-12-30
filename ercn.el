;;; ercn.el --- Flexible ERC notifications

;; Copyright (C) 2012  David Leatherman

;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/ercn
;; Version: 1.0.2

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ercn allows for flexible notification rules in ERC. You can
;; configure it to notify for certain classes of users, query buffers,
;; certain buffers, etc. It utilizes functions (and a small bit of
;; copy pasta) from erc-match to get the job done. See the
;; documentation for `ercn-notify-rules' and `ercn-suppress-rules' to
;; set it up.

;; When a notificaiton is needed, ercn calls the ercn-notify hook so
;; that any notification mechanism available for your system can be
;; utilized with a little elisp.

;; History

;; 1.0.0 - Initial release.  It probably even works.

;; 1.0.1 - save-excursion, to avoid messing with the current line

;; 1.0.2 - fix autoloads

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'erc)
(require 'erc-match)

(defvar ercn-notify-rules
  '((current-nick . all)
    (keyword . all)
    (pal . all)
    (query-buffer . all))
  "An alist containing the rules for when to notify. The format is the
  category followed by either the special symbol 'all, a list of
  buffer names in which to notify, or a function predicate. The
  predicate will be called with two strings, the nickname of the
  sender and the message. If it returns truthy, the notification hook
  will be called (unless it is suppressed).")

(defvar ercn-suppress-rules
  '((dangerous-host . all)
    (fool . all)
    (system . all))
  "An alist containing the rules for when to suppress notification.
  Suppression takes precedent over notification, so if any suppression
  rule matches, the notification hook will not be called.

  The format is the category followed by either the special symbol
  'all (to suppress everywhere), a list of buffer names in which to
  suppress, or a function predicate. The predicate will be called with
  two strings, the nickname of the sender and the message. If it
  returns truthy, the notification will be suppressed.")

(defun ercn-rule-passes-p (rules nick message category)
  "Checks the rules and returns truthy if the notify hook should be called."
  (let ((notify-rule (cdr (assoc category rules))))
    (when notify-rule
      (cond
       ((eq 'all notify-rule) t)
       ((listp notify-rule) (member (buffer-name) notify-rule))
       ((functionp notify-rule) (funcall notify-rule nick message))))))

;;;###autoload
(defun ercn-match ()
  "Extracts information from the buffer and fires the ercn-notify hook
  if needed."
  (save-excursion
    (goto-char (point-min))
    (let* ((vector (erc-get-parsed-vector (point-min)))
           (nickuserhost (erc-get-parsed-vector-nick vector))
           (nickname (and nickuserhost
                          (nth 0 (erc-parse-user nickuserhost))))
           (nick-beg (and nickname
                          (re-search-forward (regexp-quote nickname)
                                             (point-max) t)
                          (match-beginning 0)))
           (nick-end (if nick-beg
                         (progn (goto-char (match-end 0))
                                (search-forward " " nil t 1)
                                (point))
                       (point-min)))
           (message (replace-regexp-in-string
                     "\n" " " (buffer-substring nick-end (point-max))))
           (categories
            (remove nil
                    (list 'message
                          (when (null nickname) 'system)
                          (when (erc-query-buffer-p) 'query-buffer)
                          (when (or (erc-match-fool-p nickuserhost message)
                                    (erc-match-directed-at-fool-p message)) 'fool)
                          (when (erc-match-dangerous-host-p nickuserhost message)
                            'dangerous-host)
                          (when (erc-match-current-nick-p nickuserhost message)
                            'current-nick)
                          (when (erc-match-keyword-p nickuserhost message)
                            'keyword)
                          (when (erc-match-pal-p nickuserhost message) 'pal))))
           (notify-passes
            (remove nil
                    (mapcar
                     (apply-partially 'ercn-rule-passes-p
                                      ercn-notify-rules nickname message)
                     categories)))
           (suppress-passes
            (remove nil
                    (mapcar
                     (apply-partially 'ercn-rule-passes-p
                                      ercn-suppress-rules nickname message)
                     categories))))
      (when (and notify-passes
                 (null suppress-passes))
        (run-hook-with-args 'ercn-notify nickname message)))))

;;;###autoload
(defun ercn-fix-hook-order (&rest _)
  "Notify before timestamps are added"
  (when (member 'erc-add-timestamp erc-insert-modify-hook)
    (remove-hook 'erc-insert-modify-hook 'erc-add-timestamp)
    (remove-hook 'erc-insert-modify-hook 'ercn-match)
    (add-hook 'erc-insert-modify-hook 'ercn-match 'append)
    (add-hook 'erc-insert-modify-hook 'erc-add-timestamp t)))

(define-erc-module ercn nil
  "Flexible erc notifications"
  ((add-hook 'erc-insert-modify-hook 'ercn-match 'append)
   ;; to avoid duplicate messages, remove the erc-match hook
   (remove-hook 'erc-insert-modify-hook 'erc-match)
   (add-hook 'erc-connect-pre-hook 'ercn-fix-hook-order t))
  ((remove-hook 'erc-insert-modify-hook 'ercn-match)
   (remove-hook 'erc-connect-pre-hook 'ercn-fix-hook-order)))

;; For first time use
;;;###autoload
(when (and (boundp 'erc-modules)
           (not (member 'ercn 'erc-modules)))
  (add-to-list 'erc-modules 'ercn))

(provide 'ercn)

;;;###autoload
(eval-after-load 'erc
  '(progn
     (unless (featurep 'ercn (require 'ercn)))
     (add-to-list 'erc-modules 'ercn t)))

;;; ercn.el ends here
