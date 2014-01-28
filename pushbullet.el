;;; pushbullet.el --- Emacs client for the PushBullet Android app

;; Copyright (C) 2013  Abhishek L

;; Author: Abhishek L <abhishekl.2006@gmail.com>
;; URL: http://www.github.com/theanalyst/pushbullet.el
;; Version: 0.2.0
;; Package-Requires:((grapnel "0.5.2") (json "1.3"))
;; Keywords: convenience

;; This file is not a part of GNU Emacs

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Pushbullet is an android app that handles notifications. Luckily
;; there's an API which we can use to push stuff from your favourite
;; editor to your phone
;;
;; At the moment this uses `grapnel' library for http requests. This
;; is just an experiment, any comments and suggestions are more than
;; welcome. Customize the variable `pb/api-key' in the group
;; `pushbullet' to match your api-key. At present calling
;; `pb/send-region' interactively with a selection will send that
;; selection with the user specified title to your android app
;; and calling `pb/send-buffer' will send the whole contents of buffer
;; to the app

;;; History:

;; 0.1.0 - Initial release.

;; 0.2.0 - Adding support for shared devices

;;; Code:

(require 'grapnel)
(require 'json)

(defgroup pushbullet nil
  "An emacs pushbullet client"
  :prefix "pb/"
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom pb/api-key nil
  "API Key for your pushbullet account"
  :type 'string
  :group 'pushbullet)

(defvar pb/device-id-list nil
  "Alist of device_ids.")

(defvar pb/api-url "https://api.pushbullet.com/api/")

(defun pb/get-devices ()
  "Get the devices available for pushing data"
  (let ((grapnel-options (concat "-u " pb/api-key ":")))
    (grapnel-retrieve-url
     (concat pb/api-url "devices")
     `((success . (lambda (res hdrs)
		    (setq pb/device-id-list (pb/extract-devices-all res))))
       (failure . ,(apply-partially 'pb/notify "failure"))
       (error .  ,(apply-partially 'pb/notify "error")))
   "GET")))

(defun pb/push-item (devices text type title)
  "Pushes the item"
  (dolist (device_id devices)
    (let ((grapnel-options (concat "-u " pb/api-key ": ")))
      (grapnel-retrieve-url
       (concat pb/api-url "pushes")
	  `((success . (lambda (res hdrs) (message "success!")))
	    (failure . ,(apply-partially 'pb/notify "failure"))
	    (error . ,(apply-partially 'pb/notify "error")))
	  "POST"
	    nil
	    `(("device_id" . ,(number-to-string device_id))
	      ("type" . ,type)
	      ("title" . ,title)
	      ("body" . ,text))))))

(defun pb/notify (msg &optional res hdrs)
  "Notifies the result of operation"
  (if res (message "%s!! %s"  msg res)
    (message "%s!" msg)))

(defun pb/extract-device-ids (tag devices-json)
  "Make a list of device ids from the received json response"
  (let* ((json-object-type 'alist)
	 (pb-json-response (json-read-from-string devices-json)))
    (mapcar (lambda (x) (cdr (assoc 'id  x)))
	    (cdr (assoc tag pb-json-response)))))

(defun pb/extract-devices-all (devices-json)
  (let* ((devices `((devices  . ,(pb/extract-device-ids 'devices devices-json))
		    (shared   . ,(pb/extract-device-ids 'shared_devices devices-json)))))
    devices))

(defun pb/get-device-ids (all?)
  (unless pb/device-id-list (pb/get-devices))
  (if all? (append (cdr (assoc 'devices pb/device-id-list))
		 (cdr (assoc 'shared pb/device-id-list)))
    (cdr (assoc 'devices pb/device-id-list))))

;;;###autoload
(defun pushbullet (start end all? title)
  "Pushes the selection as a note. Title defaults to buffer-name
   but is accepted as a user input. If there is no selection, the
   entire buffer is sent. With a prefix arg send to shared
   devices as well "
  (interactive
   (let ((push-title
	  (read-string "Title for the push :" (buffer-name) nil (buffer-name))))
     (if mark-active
	 (list (region-beginning) (region-end) current-prefix-arg push-title)
       (list (point-min) (point-max) current-prefix-arg push-title))))
  (let ((selection (buffer-substring-no-properties start end))
	 (devices (pb/get-device-ids all?)))
    (unless (= (length selection) 0)
      (pb/push-item devices selection "note" title))))

(provide 'pushbullet)

;;; pushbullet.el ends here
