;;; dock.el --- GUI desktop dock status -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Stephane Marks
;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Version: 1.0
;; Package-Requires: ((emacs "31.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use this package to display the following on your supported GUI "dock":
;;
;; `dock-badge' overlays a short string or number on the Emacs dock
;; icon.  You can use this, for example, to indicate the number of
;; unread email messages.
;;
;; `dock-attention' flashes or bounces the Emacs dock icon to indicate
;; that your Emacs session wants attention.  Its behaviors are O/S
;; specific.
;;
;; `dock-progress' overlays a graphical bar on the Emacs dock icon to
;; illustrate progress of a long-running operation.
;;
;; The convenience macros `dotimes-with-dock-progress-reporter' and
;; `dolist-with-dock-progress-reporter' are analogous to their echo-area
;; only counterparts `dotimes-with-progress-reporter' and
;; `dolist-with-progress-reporter', respectively.  These macros display
;; progress in the echo-area and on the dock icon.

;; On GNU/Linux systems, the implementation is via D-Bus to control GUI shells
;; that implement Ubuntu's Unity launcher spec, which see
;; https://wiki.ubuntu.com/Unity/LauncherAPI.  Your Emacs instance should be
;; launched via an appropriate shell "desktop" file such as those distributed
;; with Emacs; e.g., "etc/emacsclient.desktop".
;;
;; On GNUstep and macOS systems (aka NS), native APIs are used.
;;
;; To add support for additional platforms, provide a back end that implements
;; the cl-generic functions below.

;;; Code:

(eval-when-compile
  (require 'dbus))

(defgroup dock nil
  "Display dock icon status changes."
  :group 'convenience)

(defcustom dock-clear-attention-on-frame-focus t
  "Clear the dock attention indicator when any GUI frame is focused.
This is ignored on systems that automatically clear the attention
indicator."
  :type 'boolean)

(defcustom dock-dbus-desktop-file-name "emacsclient"
  "D-Bus desktop file base name used to direct dock messages.
This should be the base name of the desktop file used to launch an Emacs
instance.  For example, if your launcher desktop file is called
\"emacs.desktop\", this option should be \"emacs\"."
  :type 'string)

(defcustom dock-dbus-timeout-ms 25000
  "Number of milliseconds to wait for D-Bus responses.
The default value mirrors the `dbus` default."
  :type 'natnum)

(defvar dock--back-end nil
  "Generic dock method system dispatcher.")

(defun dock--set-back-end ()
  "Determine dock host system type."
  ;; Order matters here. NS builds often have dbus enabled, so NS must
  ;; come first.
  (cond
   ((boundp 'ns-version-string)
    (setq dock--back-end 'ns))
   ((and (featurep 'dbusbind)
         (member "org.freedesktop.login1"
                 (dbus-list-activatable-names :system)))
    (setq dock--back-end 'dbus))
   (t
    (setq dock--back-end nil))))

;;;###autoload
(define-minor-mode dock-mode
  "Display dock icon status changes."
  :global t
  (cond
   (dock-mode
    (if (dock--set-back-end)
        (dock--enable)
      (warn "System does not support `dock'")))
   (t
    (when dock--back-end
      (dock--disable)
      (setq dock--back-end nil)))))

(cl-defgeneric dock--enable ()
  "Enable the dock back end.")

(cl-defgeneric dock--disable ()
  "Disable the dock back end.")

(cl-defgeneric dock-badge (count-or-string)
  "Set the dock icon badge to COUNT-OR-STRING.
If COUNT-OR-STRING is a string on systems which do not support strings,
convert COUNT-OR-STRING to a number, and if not an integer, use 0.
If COUNT-OR-STRING is nil or is an empty string, remove the counter.")

(cl-defgeneric dock-attention (urgency &optional timeout)
  "Set the dock icon to alert the user.
If URGENCY is the symbol \\='informational, normal attention is
requested.
If URGENCY is the symbol \\='critical, urgent attention is requested.
On some systems, \\='critical has the same effect as \\='informational.
If URGENCY is nil, the attention indicator is cleared.
If TIMEOUT is non-nil, the attention indicator will be removed after
TIMEOUT seconds.")

(cl-defgeneric dock-progress (progress)
  "Set the dock icon for SYSTEM to indicate progress.
PROGRESS is a float in the range 0.0 to 1.0.
If PROGRESS is nil, remove the progress indicator.")


;; `progress-reporter' support.

(defun dock--progress-reporter-do-update (orig-fun reporter value &optional suffix)
  (funcall orig-fun reporter value suffix)
  (let* ((parameters (cdr reporter))
         (update-time (aref parameters 0))
         (min-value (aref parameters 1))
         (max-value (aref parameters 2))
         (enough-time-passed
          (or (not update-time)
              (time-less-p update-time nil))))
    (when (and min-value max-value)
      (when enough-time-passed
        (let* ((one-percent (/ (- max-value min-value) 100.0))
               (percentage (if (= max-value min-value)
                               0
                             (truncate (/ (- value min-value)
                                          one-percent)))))
          (dock-progress (/ percentage 100.0))))
      (when (>= value max-value)
        (dock-progress nil)))))

(defvar dock--dotimes-with-dock-progress-reporter-advice-count 0
  "Ensure advice is not removed when more than one active use.")

(defun dock--set-up-progress-reporter ()
  (incf dock--dotimes-with-dock-progress-reporter-advice-count)
  (advice-add #'progress-reporter-do-update
              :around
              #'dock--progress-reporter-do-update))

(defun dock--tear-down-progress-reporter ()
  (when (= 0 (decf dock--dotimes-with-dock-progress-reporter-advice-count))
    (advice-remove #'progress-reporter-do-update
                   #'dock--progress-reporter-do-update)))

(defmacro dotimes-with-dock-progress-reporter (spec reporter-or-message &rest body)
  "Loop over a list and report progress in the echo area and GUI dock.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".

\(fn (VAR LIST [RESULT]) REPORTER-OR-MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  `(unwind-protect
       (progn
         (dock--set-up-progress-reporter)
         (dotimes-with-progress-reporter ,spec ,reporter-or-message ,@body))
     (dock-progress nil)
     (dock--tear-down-progress-reporter)))

(defmacro dolist-with-dock-progress-reporter (spec reporter-or-message &rest body)
  "Loop over a list and report progress in the echo area and GUI dock.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".

\(fn (VAR LIST [RESULT]) REPORTER-OR-MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  `(unwind-protect
       (progn
         (dock--set-up-progress-reporter)
         (dolist-with-progress-reporter ,spec ,reporter-or-message ,@body))
     (dock-progress nil)
     (dock--tear-down-progress-reporter)))


;; D-Bus support.

(eval-and-compile
  (when (and (not (boundp 'ns-version-string))
             (featurep 'dbusbind))
    (require 'dbus)

    (defconst dock--dbus-service "com.canonical.Unity")
    (defconst dock--dbus-interface "com.canonical.Unity.LauncherEntry")

    (defvar dock--dbus-desktop-id nil
      "Constructed D-Bus application desktop id.")

    (defvar dock--dbus-attention nil
      "Non-nil when attention is enabled.")

    ;; Pacify byte compiler.
    (declare-function dock--dbus-send-signal "dock")
    (declare-function dock--dbus-clear-attention-on-frame-focus "dock")

    (defun dock--dbus-send-signal (message)
      (dbus-send-signal
       :session
       dock--dbus-service
       "/"
       dock--dbus-interface
       "Update"
       dock--dbus-desktop-id
       message))

    (defun dock--dbus-clear-attention-on-frame-focus ()
      (when (and dock--dbus-attention
                 (catch :clear
                   (dolist (frame (frame-list))
                     (when (eq (frame-focus-state frame) t)
                       (throw :clear t)))))
        (dock-attention nil)))

    (cl-defmethod dock--enable (&context (dock--back-end (eql 'dbus)))
      (setq dock--dbus-desktop-id
            (format "application://%s.desktop"
                    dock-dbus-desktop-file-name))
      (unless (dbus-ping
               :session
               dock--dbus-service
               dock-dbus-timeout-ms)
        (error "D-Bus service `%s' unavailable" dock--dbus-interface))
      (when dock-clear-attention-on-frame-focus
        (add-function :after after-focus-change-function
                      #'dock--dbus-clear-attention-on-frame-focus)))

    (cl-defmethod dock--disable (&context (dock--back-end (eql 'dbus)))
      (remove-function after-focus-change-function
                       #'dock--dbus-clear-attention-on-frame-focus))

    (cl-defmethod dock-badge (count-or-string
                              &context (dock--back-end (eql 'dbus)))
      ;; Unity does not support string badges, so we make a best effort if
      ;; STRING is an integer, otherwise use 0.
      (when (stringp count-or-string)
        (if (string-empty-p count-or-string)
            (setq count-or-string nil)
          (let ((count (string-to-number count-or-string)))
            (setq count-or-string (if (integerp count) count 0)))))
      (dock--dbus-send-signal
       `((:dict-entry "count-visible"
                      (:variant :boolean ,(not (null count-or-string))))
         (:dict-entry "count"
                      (:variant :uint32 ,(if (null count-or-string) 0
                                           count-or-string))))))

    (cl-defmethod dock-attention (urgency
                                  &context (dock--back-end (eql 'dbus))
                                  &optional timeout)
      ;; Note: Unity does not support differentiated urgency.
      (setq dock--dbus-attention urgency)
      (dock--dbus-send-signal
       `((:dict-entry "urgent"
                      (:variant :boolean ,(not (null urgency))))))
      (when (and urgency timeout)
        (run-with-timer
         timeout
         nil
         #'dock-attention nil)))

    (cl-defmethod dock-progress (progress
                                 &context (dock--back-end (eql 'dbus)))
      (dock--dbus-send-signal
       `((:dict-entry "progress-visible"
                      (:variant :boolean ,(not (null progress))))
         (:dict-entry "progress"
                      (:variant :double ,(if (null progress) 0 progress))))))))


;; NS support.

(eval-and-compile
  (when (boundp 'ns-version-string)

    ;; Pacify byte compiler.
    (declare-function ns-badge "nsfns.m")
    (declare-function ns-request-user-attention "nsfns.m")
    (declare-function ns-progress-indicator "nsfns.m")

    (cl-defmethod dock--enable (&context (dock--back-end (eql 'ns)))
      (ignore))

    (cl-defmethod dock--disable (&context (dock--back-end (eql 'ns)))
      (ignore))

    (cl-defmethod dock-badge (count-or-string
                              &context (dock--back-end (eql 'ns)))
      ;; NS supports only string badges.
      (cond
       ((stringp count-or-string)
        (when (string-empty-p count-or-string)
          (setq count-or-string nil)))
       ((numberp count-or-string)
        (setq count-or-string (number-to-string count-or-string))))
      (ns-badge count-or-string))

    (cl-defmethod dock-attention (urgency
                                  &context (dock--back-end (eql 'ns))
                                  &optional timeout)
      (ns-request-user-attention urgency)
      (when (and urgency timeout)
        (run-with-timer
         timeout
         nil
         #'dock-attention nil)))

    (cl-defmethod dock-progress (progress
                                 &context (dock--back-end (eql 'ns)))
      (ns-progress-indicator progress))))



(provide 'dock)

;;; dock.el ends here
