;;; pushbullet.el ---

;; Copyright (C) 2013  Abhishek L <abhishekl.2006@gmail.com>

;; Author: Abhishek L <abhishekl.2006@gmail.com>
;; URL: http://www.github.com/theanalyst/revolver
;; Version: 0.1.0

;; This file is not a part of GNU Emacs
;;
;; Keywords: convenience

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

(defun pb/get-devices ()
  "Get the devices available for pushing data"
  (let ((grapnel-options
        (concat "-u " pb/api-key ":")))
    (grapnel-retrieve-url
     "https://www.pushbullet.com/api/devices"
     '((success . (lambda (res hdrs)
                    (setq pb/device-id-list (pb/extract-devices-all res))))
       (failure . (lambda (res hdrs) (message "Failure %s" res)))
       (error . (lambda (res err) (message "err %s" err))))
   "GET")))

(defun pb/push-item (devices text type title)
  "Pushes the item"
  (dolist (device_id devices)
    (let ((grapnel-options
         (concat "-u " pb/api-key ": ")
         ))
      (grapnel-retrieve-url
          "https://www.pushbullet.com/api/pushes"
          `((success . (lambda (res hdrs)
                         (message "success!")))
            (failure . (lambda (res hdrs) (message "failure! %s" hdrs)))
            (error . (lambda (res err) (message "err %s" err))))
          "POST"
            nil
            `(("device_id" . ,(number-to-string device_id))
              ("type" . ,type)
              ("title" . ,title)
              ("body" . ,text))
            ))))

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
  (if all? (nconc (cdr (assoc 'devices pb/device-id-list))
		  (cdr (assoc 'shared pb/device-id-list)))
    (cdr (assoc 'devices pb/device-id-list))))

(defun pb/select-region (start end)
  "Selects a region from start to end"
  (buffer-substring-no-properties start end))

(defun pb/send-region (all? title)
"Pushes a region as an article"
  (interactive "P\ns Enter title for this push:" )
  (let* ((selection (pb/select-region (region-beginning) (region-end)))
	 (devices (pb/get-device-ids all?)))
    (unless (= (length selection) 0)
      (pb/push-item devices selection "note" title))))

(defun pb/send-buffer (all? buffer-name)
  "Pushes the entire buffer as a note"
  (interactive "P\nb Enter the buffer to be pushed:")
  (with-current-buffer buffer-name
    (let ((selection (pb/select-region (point-min) (point-max)))
	  (devices (pb/get-device-ids all?)))
    (unless (= (length selection) 0)
      (pb/push-item devices selection "note" buffer-name)))))

(provide 'pushbullet)
;;; pushbullet.el ends here
