;;; gif-screencast.el --- One-frame-per-action GIF recording -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://gitlab.com/ambrevar/emacs-gif-screencast
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia, screencast

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
;; Call `gif-screencast' to start a recording.
;;
;; A screenshot is taken for every user action.
;;
;; Call `gif-screencast-stop' (<f9> by default) to finish recording and create
;; the GIF result.

;;; Code:

;; TODO: Capture on scrolling (e.g. program outputting to Eshell buffer).
;; TODO: Add support for on-screen keystroke display, e.g. screenkey.

(defgroup gif-screencast nil
  "Predefined configurations for `gif-screencast'."
  :group 'multimedia)

(defcustom gif-screencast-program (if (eq 'darwin system-type) "screencapture" "scrot")
  "A program for taking screenshots.
See also `gif-screencast-capture-format'."
  :group 'gif-screencast
  :type 'string)

(defcustom gif-screencast-args '("--quality" "25" "--focused")
  "Arguments to `screencast-program'.
\"scrot\" can use `--focused' to restrict the capture to the Emacs frame."
  :group 'gif-screencast
  :type 'string)

(defcustom gif-screencast-log "*gif-screencast-log*"
  "Name of the buffer logging the actions.
The log is made of the standard output and standard error of the
various programs run here."
  :group 'gif-screencast
  :type 'string)

(defvar gif-screencast-convert-program "convert"
  "A program for converting the screenshots to a GIF.")

(defcustom gif-screencast-convert-args '("-delay" "100" "-loop" "0" "-dither" "None" "-colors" "80" "-fuzz" "40%" "-layers" "OptimizeFrame")
  "Arguments to `gif-screencast-convert-program'."
  :group 'gif-screencast
  :type 'string)

(defvar gif-screencast-cropping-program "mogrify"
  "A program for cropping the screenshots.
If `gif-screencast-cropping-program' is not found, cropping will be skipped.")

(defcustom gif-screencast-cropping-args nil
  "Arguments to `gif-screencast-cropping-program'.
Don't specify \"-format\" and \"-crop\" since these commands are used
as the default arguments."
  :group 'gif-screencast
  :type '(repeat string))

(defcustom gif-screencast-want-optimized t
  "If non-nil, run `gif-screencast-optimize' over the resulting GIF."
  :group 'gif-screencast
  :type 'boolean)

(defcustom gif-screencast-optimize-program "gifsicle"
  "A program for optimizing GIF files."
  :group 'gif-screencast
  :type 'string)

(defcustom gif-screencast-optimize-args '("--batch" "--optimize=3")
  "Arguments to `gif-screencast-optimize-program'."
  :group 'gif-screencast
  :type 'string)

(defcustom gif-screencast-countdown 3
  "Countdown before recording.
0 disables countdown."
  :group 'gif-screencast
  :type 'integer)


(defcustom gif-screencast-screenshot-directory (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid))
  "Output directory for temporary screenshots."
  :group 'gif-screencast
  :type 'string)

(defcustom gif-screencast-autoremove-screenshots t
  "If non nil, remove the temporary screenshots after a successful compilation of the GIF."
  :group 'gif-screencast
  :type 'boolean)

(defcustom gif-screencast-output-directory (or (getenv "XDG_VIDEOS_DIR")
                                               (expand-file-name "Videos/emacs/" "~"))
  "Output directory for the GIF file."
  :group 'gif-screencast
  :type 'directory)

(defcustom gif-screencast-capture-format "png"
  "Image format to store the captured images.
If you are a macOS user, \"ppm\" should be specified."
  :group 'gif-screencast
  :type 'string)

(defcustom gif-screencast-title-bar-pixel-height (cdr (alist-get 'title-bar-size (frame-geometry)))
  "Height of title bar for cropping screenshots."
  :group 'gif-screencast
  :type 'integer)

(defvar gif-screencast--frames nil
  "A frame is a plist in the form '(:time :file :offset).")
(defvar gif-screencast--offset 0
  "Delay accumulated by all the pauses.")
(defvar gif-screencast--offset-mark 0
  "Timestamp when user hit pause.")

(defvar gif-screencast-mode-map
  (make-sparse-keymap)
  "Keymap of `gif-screencast-mode'.")

(define-minor-mode gif-screencast-mode
  "gif-screencast bindings"
  :init-value nil
  :global t
  :require 'gif-screencast
  :keymap gif-screencast-mode-map)

(defvar gif-screencast--counter 0
  "Number of running screenshots.")

(defun gif-screencast-capture-sentinel (_proc _status)
  "Sentinel for screen capturing."
  (setq gif-screencast--counter (- gif-screencast--counter 1))
  (gif-screencast--finish))

(defun gif-screencast--finish ()
  "Finish screen capturing."
  (when (and (not gif-screencast-mode) (= gif-screencast--counter 0))
    (if (memq window-system '(mac ns))
        (gif-screencast--crop)
      (gif-screencast--generate-gif nil nil))))

(defun gif-screencast--generate-gif (process event)
  "Generate GIF file."
  (when process
    (gif-screencast-print-status process event))
  (let (delays
        (index 0)
        (frames gif-screencast--frames))
    (while (cdr frames)
      (push (list "(" "-clone" (number-to-string index) "-set" "delay"
                  ;; Converters delays are expressed in centiseconds.
                  (format "%d" (* 100 (float-time
                                       (time-subtract (car (cadr frames)) (caar frames)))))
                  ")" "-swap" (number-to-string index) "+delete")
            delays)
      (setq index (1+ index)
            frames (cdr frames)))
    (message "Compiling GIF with %s..." gif-screencast-convert-program)
    (let ((output (expand-file-name
                   (format-time-string "output-%F-%T.gif" (current-time))
                   (or (and (file-writable-p gif-screencast-output-directory)
                            gif-screencast-output-directory)
                       (read-directory-name "Save output to directory: "))))
          p)
      (setq p (apply 'start-process
                     gif-screencast-convert-program
                     (get-buffer-create gif-screencast-log)
                     gif-screencast-convert-program
                     (append
                      gif-screencast-convert-args
                      (mapcar 'cdr gif-screencast--frames)
                      ;; Delays must come after the file arguments.
                      (apply 'nconc delays)
                      (list output))))
      (set-process-sentinel p (lambda (process event)
                                (gif-screencast-print-status process event)
                                (when (and gif-screencast-want-optimized
                                           (eq (process-status process) 'exit)
                                           (= (process-exit-status process) 0))
                                  (gif-screencast-optimize output))
                                (when (and gif-screencast-autoremove-screenshots
                                           (eq (process-status process) 'exit)
                                           (= (process-exit-status process) 0))
                                  (dolist (f gif-screencast--frames)
                                    (delete-file (cdr f)))))))))

(defun gif-screencast--cropping-region ()
  "Return the cropping region of the captured image."
  (let ((x (car (frame-position)))
        (y (cdr (frame-position)))
        (width (car (alist-get 'outer-size (frame-geometry))))
        (height (+ (frame-pixel-height)
                   (or gif-screencast-title-bar-pixel-height 0)
                   (cdr (alist-get 'tool-bar-size (frame-geometry))))))
    (format "%dx%d+%d+%d" width height x y)))

(defun gif-screencast--crop ()
  "Crop the captured images to the active region of selected frame."
  (when (and (not gif-screencast-mode) (= gif-screencast--counter 0))
    (if (executable-find gif-screencast-cropping-program)
        (progn
          (message "Cropping captured images with %s..."
                   gif-screencast-cropping-program)
          (let ((process-connection-type nil)
                (p (apply 'start-process
                          "cropping"
                          (get-buffer-create gif-screencast-log)
                          gif-screencast-cropping-program
                          (append '("-format")
                                  (list (format "%s" gif-screencast-capture-format))
                                  '("-crop")
                                  (list (gif-screencast--cropping-region))
                                  gif-screencast-cropping-args
                                  (mapcar 'cdr gif-screencast--frames)))))
            (set-process-sentinel p 'gif-screencast--generate-gif)))
      (message "Cropping program '%s' not found (See `gif-screencast-cropping-program')" gif-screencast-cropping-program)
      (gif-screencast--generate-gif nil nil))))

(defun gif-screencast-capture ()
  "Save result of `screencast-program' to `screencast-output-dir'."
  (let* ((time (current-time))
         (file (expand-file-name
                (concat (format-time-string "screen-%F-%T-%3N" time)
                        "."
                        gif-screencast-capture-format)
                gif-screencast-screenshot-directory)))
    (setq gif-screencast--counter (+ gif-screencast--counter 1))
    (let ((p (apply 'start-process gif-screencast-program
                    (get-buffer-create gif-screencast-log)
                    gif-screencast-program
                    (append
                     gif-screencast-args
                     (list file)))))
      (set-process-sentinel p 'gif-screencast-capture-sentinel))
    (push (cons (time-subtract time gif-screencast--offset) file) gif-screencast--frames)))

;;;###autoload
(defun gif-screencast ()
  "Start recording the GIF.
A screenshot is taken before every command runs."
  (interactive)
  (if gif-screencast-mode
      (message "gif-screencast already running")
    (if (not (executable-find gif-screencast-program))
        (message "Screenshot program '%s' not found (See `gif-screencast-program')" gif-screencast-program)
      (dolist (d (list gif-screencast-output-directory gif-screencast-screenshot-directory))
        (unless (file-exists-p d)
          (make-directory d 'parents)))
      (setq gif-screencast--frames '())
      (setq gif-screencast--counter 0)
      (gif-screencast-mode 1)
      (dolist (i (number-sequence gif-screencast-countdown 1 -1))
        (message "Start recording GIF in %s..." i)
        (sleep-for 0.7))
      (message "Go! (Press %s to stop, %s to resume)"
               (substitute-command-keys "\\[gif-screencast-stop]")
               (substitute-command-keys "\\[gif-screencast-toggle-pause]"))
      (add-hook 'pre-command-hook 'gif-screencast-capture))))

(defun gif-screencast-toggle-pause ()
  "Toggle recording of the GIF."
  (interactive)
  (if (memq 'gif-screencast-capture (default-value 'pre-command-hook))
      (progn
        (remove-hook 'pre-command-hook 'gif-screencast-capture)
        (setq gif-screencast--offset-mark (current-time))
        (message "GIF recording paused. (Press %s to stop, %s to resume)"
                 (substitute-command-keys "\\[gif-screencast-stop]")
                 (substitute-command-keys "\\[gif-screencast-toggle-pause]")))
    (setq gif-screencast--offset (time-to-seconds
                                  (time-subtract (current-time) gif-screencast--offset-mark)))
    (add-hook 'pre-command-hook 'gif-screencast-capture)
    (message "GIF recording resumed. (Press %s to stop, %s to pause)"
             (substitute-command-keys "\\[gif-screencast-stop]")
             (substitute-command-keys "\\[gif-screencast-toggle-pause]"))))

(defun gif-screencast-print-status (process event)
  "Output PROCESS EVENT to minibuffer."
  (princ (format "Process '%s' %s"
                 process
                 (progn
                   (while (string-match "\n+\\|\r+" event)
                     (setq event (replace-match "" t t event)))
                   event))))

(defun gif-screencast-optimize (file)
  "Optimize GIF FILE asynchronously."
  (message "Optimizing with %s..." gif-screencast-optimize-program)
  (let ((p (apply 'start-process
                  gif-screencast-optimize-program
                  (get-buffer-create gif-screencast-log)
                  gif-screencast-optimize-program
                  (append
                   gif-screencast-optimize-args
                   (list file)))))
    (set-process-sentinel p 'gif-screencast-print-status)))

(defun gif-screencast-stop ()
  "Stop recording and compile GIF."
  (interactive)
  (remove-hook 'pre-command-hook 'gif-screencast-capture)
  (gif-screencast-mode 0)
  (setq gif-screencast--frames (nreverse gif-screencast--frames))
  (gif-screencast--finish))

(provide 'gif-screencast)

;;; gif-screencast.el ends here
