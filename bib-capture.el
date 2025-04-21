;;; bib-capture.el --- Lightweight BibTeX entry capture via DOI -*- lexical-binding: t; -*-

;; Author: APC
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (biblio "0.3") (citar "1.0"))
;; Keywords: bibtex, citation, convenience
;; URL: https://github.com/apc/bib-capture

;;; Commentary:
;; This package provides a simple interface to insert BibTeX entries from DOIs,
;; confirm and store them in a preferred bibliography file, and associate files.

;;; Code:

(require 'bibtex)
(require 'biblio-doi)
(require 'citar)

(defgroup bib-capture nil
  "BibTeX capture tools via DOI."
  :group 'bibtex)

(defcustom bib-capture-default-target nil
  "Default target file for bib-capture entries."
  :type 'file)

(defcustom bib-capture-source-dir nil
  "Default location where files to be added are saved."
  :type 'file)

(defcustom bib-capture-default-file-provider nil
  "Function or value used to provide a default file path for `bib-capture-add-to-library`.

If this is a function, it should return a string (the path to the file).
If it's a string, it is used directly as the path.
If nil, the user will be prompted."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Function returning a file path")
                 (string :tag "Static file path"))
  :group 'bib-capture)


(defvar bib-capture-buffer-name "*Bib Capture*"
  "Name of the temporary buffer used for BibTeX editing.")

(defvar bib--duplicate-marker nil
  "Marker pointing to the location of a duplicate BibTeX entry.")

(defvar bib-capture-last-stored-marker (make-marker)
  "Marker pointing to the entry most recently stored with `bib-capture'.")

(defvar bib-capture-last-stored-entry nil
  "Cons cell consisting of the buffer and citation key for the last
entry stored with `bib-capture'.")

(defvar bib-capture-mode-hook nil
  "Hooks to be run after setting up the capture buffer.")

(defvar bib-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'bib-capture-confirm)
    (define-key map (kbd "C-c C-k") #'bib-capture-abort)
    (define-key map (kbd "C-c C-u") #'bib-capture-clean-entry)
    map)
  "Keymap for `bib-capture-mode'.")

(define-minor-mode bib-capture-mode
  "Minor mode for reviewing and confirming BibTeX entries."
  :init-value nil
  :lighter " BibCap"
  :keymap bib-capture-mode-map
  (when bib-capture-mode
    (setq header-line-format
          '(:eval (concat
                   (propertize " " 'display '(space :align-to 0))
                   (propertize "Press " 'face 'font-lock-comment-face)
                   (propertize "C-c C-c" 'face 'help-key-binding)
                   (propertize " to confirm, " 'face 'font-lock-comment-face)
                   (propertize "C-c C-u" 'face 'help-key-binding)
                   (propertize " to clean, or " 'face 'font-lock-comment-face)
                   (propertize "C-c C-k" 'face 'help-key-binding)
                   (propertize " to cancel." 'face 'font-lock-comment-face))))
    (run-hooks 'bib-capture-mode-hook)))

(defun bib--capture-get-key (entry)
  "Extract the BibTeX entry key from the ENTRY string."
  (when (string-match "@[A-Za-z]+[{(][[:space:]]*\\([^,\n]+\\)" entry)
    (substring-no-properties (match-string 1 entry))))

(defun bib--capture-uniquify-key (base-key)
  "Generate a unique BibTeX key by appending a number to BASE-KEY."
  (let ((n 1)
        (new-key base-key)
        (existing-keys (with-current-buffer (find-file-noselect bib-capture-default-target)
                         (save-excursion
                           (goto-char (point-min))
                           (let (keys)
                             (while (re-search-forward "@\\w+{\\s-*\\([^,]+\\)" nil t)
                               (push (match-string 1) keys))
                             keys)))))
    (while (member new-key existing-keys)
      (setq new-key (format "%s-%d" base-key n))
      (setq n (1+ n)))
    new-key))


(defun bib-capture-confirm (&optional new-key)
  "Confirm the BibTeX entry and save it to `bib-capture-default-target'."
  (interactive "P")
  (let ((entry (buffer-string))
        (capture-buf (current-buffer))
        (duplicate-found nil)
        (target-file (expand-file-name bib-capture-default-target))
        key)
    (bibtex-clean-entry new-key)
    (setq key (bib--capture-get-key entry))
    (unless key
      (user-error "Could not extract a citation key from the entry."))
    (with-current-buffer (find-file-noselect target-file)
      (save-excursion
        (if-let ((dup-loc (bibtex-find-entry key)))
            (setq duplicate-found dup-loc)
          (goto-char (point-max))
          (unless (save-excursion (forward-line -1) (looking-at-p "^\\s-*$"))
            (insert "\n"))
          (insert entry "\n\n")
          (bibtex-clean-entry)
          (save-buffer)
          (setq bib-capture-last-stored (cons target-file key))
          (kill-buffer capture-buf))))
    (when duplicate-found
      (when (y-or-n-p (format "Duplicate key found: %s. Jump to it? " key))
        (find-file-other-window target-file)
        (goto-char duplicate-found)
        (beginning-of-line)))))



(defun bib-capture-abort ()
  "Cancel BibTeX entry editing without saving."
  (interactive)
  (message "Bib capture canceled.")
  (kill-buffer))

(defun bib-capture-clean-entry ()
  "Call `bibtex-clean-entry' with a non-nil NEW-KEY argument."
  (interactive)
  (bibtex-clean-entry t))

(defun bib-capture---biblio-doi--insert-advice (orig-fn bibtex target-buffer)
  "Advice for `biblio-doi--insert` to move point to the beginning of
buffer after inserting."
  (unwind-protect
      (progn
        (funcall orig-fn bibtex target-buffer)
        (with-current-buffer target-buffer
          (goto-char (point-min))
          (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil))
                    ((symbol-function 'bibtex-progress-message) (lambda (&rest _args) nil)))
            (bibtex-clean-entry t))))
    (advice-remove 'biblio-doi--insert #'bib-capture---biblio-doi--insert-advice)))

(defun bib-capture--resolve-default-file ()
  "Resolve the default file to use for `bib-capture-add-to-library`."
  (let ((provider bib-capture-default-file-provider))
    (cond
     ((stringp provider) provider)
     ((functionp provider)
      (let ((result (funcall provider)))
        (cond
         ((stringp result) result)
         ((and (listp result) (cl-every #'stringp result))
          (completing-read "Select file: " result nil t))
         (t nil))))
     (t nil))))

(defun bib-capture--search-entry-with-buffer (key &optional global start display)
  "Like `bibtex-search-entry` but returns (BUFFER . POSITION)."
  (let ((pos nil)
        (buf nil))
    (if (and global bibtex-files)
        (let ((buffer-list (bibtex-initialize t))
              buffer found)
          (while (and (not found)
                      (setq buffer (pop buffer-list)))
            (with-current-buffer buffer
              (when (cdr (assoc-string key bibtex-reference-keys))
                (setq found (bibtex-search-entry key)))))
          (when found
            (setq buf (current-buffer)
                  pos found)))
      ;; local search
      (setq buf (current-buffer)
            pos (save-excursion
                  (goto-char (or start (point-min)))
                  (if (re-search-forward (concat "^[ \t]*\\("
                                                 bibtex-entry-type
                                                 "\\)[ \t]*[({][ \t\n]*\\("
                                                 (regexp-quote key)
                                                 "\\)[ \t\n]*[,=]")
                                         nil t)
                      (match-beginning 0)))))
    (when (and display pos)
      (switch-to-buffer buf)
      (goto-char pos)
      (bibtex-reposition-window))
    (when pos
      (cons buf pos))))

(defun bib-capture--validate-stored-entry ()
  (let* ((entry bib-capture-last-stored-entry)
         (buf (car entry))
         (key (cdr entry)))
    (unless buf
      (user-error "There is no "))

    )
  (save-window-excursion (switch-to-buffer)))

(defun bib-capture-goto-last-stored (&optional silent)
  "Jump to the last BibTeX entry stored via `bib-capture-confirm`. Uses the information stored in `bib-capture-last-stored-entry'."
  (interactive)
  (let ((marker (and bib-capture-last-stored-marker
                     (marker-buffer bib-capture-last-stored-marker)
                     (buffer-live-p (marker-buffer bib-capture-last-stored-marker))
                     bib-capture-last-stored-marker))
        (key bib-capture-last-stored-key))
    (cond
     (marker
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)
      (bibtex-reposition-window)
      (unless silent
        (message "Jumped to last BibTeX capture: %s" key))
      (when silent key))

     ((and key
           (let ((result (bib-capture--search-entry-with-buffer key t nil t)))
             (when result
               (let ((buf (car result))
                     (pos (cdr result)))
                 (setq marker (set-marker (make-marker) pos buf))
                 (setq bib-capture-last-stored-marker marker)
                 (switch-to-buffer buf)
                 (goto-char pos)
                 (bibtex-reposition-window)
                 (unless silent
                   (message "Located last BibTeX capture by key: %s" key))
                 (when silent key))))))

     ((not silent)
      (message "No BibTeX capture history available.")))))





(defun bib-capture--suppress-parse-messages-once (orig-fun &rest args)
  "Temporarily silence messages from `bibtex-parse-keys` once."
  (let ((inhibit-message t))
    (unwind-protect
        (apply orig-fun args)
      (advice-remove 'bibtex-parse-keys
                     #'bib-capture--suppress-parse-messages-once))))

(defun bib-capture--use-temp-key-advice (orig-fun &rest args)
  (cl-letf (((symbol-function 'bibtex-read-key)
             (lambda (&rest _)
               (format "tmpkey-%s" (substring (md5 (number-to-string (float-time))) 0 8)))))
    (apply orig-fun args)
    (advice-remove 'bibtex-read-key #'bib-capture--use-temp-key-advice)))



;;;###autoload
(defun bib-capture (doi)
  "Insert BibTeX for DOI into a temporary buffer, or run `bibtex-entry` if called with prefix arg."
  (interactive
   (if current-prefix-arg
       (list nil)  ;; No prompt — manual entry
     (list (read-string "DOI (or leave empty to enter manually): "))))
  (let* ((doi (and doi (string-trim doi)))
         (doi-given (and doi (not (string-empty-p doi))))
         (buf (get-buffer-create bib-capture-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (advice-add 'bibtex-parse-keys :around #'bib-capture--suppress-parse-messages-once)
      (bibtex-mode)
      (bib-capture-mode 1)
      (when doi-given
        (advice-add 'biblio-doi--insert :around #'bib-capture---biblio-doi--insert-advice)
        (biblio-doi-insert-bibtex doi)))
    (pop-to-buffer buf)
    (unless doi-given
      (advice-add 'bibtex-entry :around #'bib-capture--use-temp-key-advice)
      (call-interactively #'bibtex-entry))))

;;;###autoload
(defun bib-capture-add-to-library (citekey &optional file noconfirm)
  "Add a file to the library for CITEKEY.

Uses FILE if provided, otherwise uses `bib-capture-default-file-provider`
to determine a file, or prompts the user.

If FILE is provided, or `bib-capture--resolve-default-file'
returns an existing file, a non-nil value of NOCONFIRM will avoid
prompting the user for confirmation."
  (interactive (list (citar-select-ref)))
  (let* ((default-path (bib-capture--resolve-default-file))
       (filepath
        (cond
         (file file)
         ((and default-path noconfirm) default-path)
         (t (read-file-name "Add file: "
                (file-name-directory default-path)
                nil t
                (file-name-nondirectory default-path))))))
    (unless (and citekey (file-exists-p filepath))
      (user-error "Missing citekey or file does not exist: %s" filepath))
    (let ((ext (file-name-extension filepath)))
      (unless ext
        (user-error "Cannot determine file extension for: %s" filepath))
      (let ((source-plist
             (list :write-file
                   (lambda (destfile ok-if-already-exists)
                     (copy-file filepath destfile ok-if-already-exists))
                   :extension ext)))
        (citar-save-file-to-library citekey source-plist)
        (message "Saved %s as a resource for %s" filepath citekey)))))

;;;###autoload
(defun bib-capture-add-file-to-last-capture (&optional file noconfirm)
  "Add a file to the library for the last stored entry.

FILE and NOCONFIRM are as in `bib-capture-add-to-library', which see."
  (interactive)
  (when-let ((key (save-window-excursion (bib-capture-goto-last-stored 'silent))))
    (bib-capture-add-to-library key)))

(provide 'bib-capture)
;;; bib-capture.el ends here
