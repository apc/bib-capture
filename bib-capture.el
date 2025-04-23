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
  "Function or value used to provide a default file path for
`bib-capture-add-to-library`.

If this is a function, it should return a string (the path to the
file). If it's a string, it is used directly as the path. If nil,
the user will be prompted."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Function returning a file path")
                 (string :tag "File path"))
  :group 'bib-capture)


(defvar bib-capture-buffer-name "*Bib Capture*"
  "Name of the temporary buffer used for BibTeX editing.")

(defvar bib-capture-last-stored-marker (make-marker)
  "Marker pointing to the entry most recently stored with
`bib-capture'.")

(defvar bib-capture-last-stored nil
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
                   (substitute-command-keys
    "\\<bib-capture-mode-map>Bib Capture buffer.  Finish \
`\\[bib-capture-confirm]', update `\\[bib-capture-clean-entry]', \
abort `\\[bib-capture-abort]'."))))
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

(defun bib-capture--entry-exists (key &optional show)
  "Check if entry with KEY exists in `bib-capture-default-target’.
Return nil otherwise.

If a duplicate entry exists, this will return the position of the
existing entry, as a marker. If optional argument SHOW is
non-nil, the entry will be displayed in a temporary buffer."
  (let ((target-file (expand-file-name bib-capture-default-target))
        (dup-loc)
        (existing-entry)
        (bufname (format "*BibTeX Duplicate: %s*" key)))
    (with-current-buffer (find-file-noselect target-file)
      (when-let ((dup (bibtex-find-entry key)))
        (let ((dup-marker (copy-marker dup)))
          (setq existing-entry (save-excursion
                                 (goto-char dup-marker)
                                 (bibtex-beginning-of-entry)
                                 (buffer-substring-no-properties
                                  (point)
                                  (progn (bibtex-end-of-entry) (point)))))
          (setq dup-loc dup-marker))))
    (if show
          (when dup-loc
            (with-current-buffer (get-buffer-create bufname)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert existing-entry)
                (view-mode 1)
                (bibtex-mode)))
            (display-buffer bufname))
      dup-loc)))

(defun bib-capture-confirm (&optional new-key)
  "Confirm the BibTeX entry and save it to `bib-capture-default-target'.

If the key already exists, the user is prompted to auto-generate a new one,
enter one manually, or abort (which will kill the current buffer)."
  (interactive "P")
  (bibtex-clean-entry new-key)
  (let* ((capture-buf (current-buffer))
         (target-file (expand-file-name bib-capture-default-target))
         (entry (buffer-string))
         (original-key (bib--capture-get-key entry))
         (final-key original-key))

    (unless original-key
      (user-error "Could not extract a citation key from the entry."))

    ;; Loop until we get a unique key
    (while (bib-capture--entry-exists final-key t)
      (let* ((choice (read-multiple-choice
                      (format "Key \"%s\" already exists. Choose an option: " final-key)
                      '((?a "auto" "Auto-generate a new key")
                        (?m "manual" "Manually enter a new key")
                        (?q "quit" "Abort the capture")))))
        (pcase choice
          (?a
           (setq final-key (bib--capture-uniquify-key final-key)))
          (?m
           (setq final-key (read-string "Enter a new citation key: ")))
          (?q
           (kill-buffer capture-buf)
           (user-error "Aborted BibTeX capture due to duplicate key.")))))

    ;; Replace key in entry if needed
    (unless (equal final-key original-key)
      (setq entry (replace-regexp-in-string
                   (format "\\(@\\w+{\\s-*\\)%s" (regexp-quote original-key))
                   (format "\\1%s" final-key)
                   entry)))

    ;; Insert entry
    (with-current-buffer (find-file-noselect target-file)
      (save-excursion
        (goto-char (point-max))
        (unless (save-excursion (forward-line -1) (looking-at-p "^\\s-*$"))
          (insert "\n"))
        (insert entry "\n\n")
        (bibtex-clean-entry)
        (save-buffer))
      (setq bib-capture-last-stored (cons target-file final-key))
      (if-let (win (get-buffer-window capture-buf))
        (with-selected-window win
          (kill-buffer-and-window))
      (kill-buffer capture-buf))
      (message "Inserted BibTeX entry with key: %s" final-key))))


(defun bib-capture-abort ()
  "Cancel BibTeX entry editing without saving."
  (interactive)
  (message "Bib capture canceled.")
  (let* ((capture-buf (current-buffer))
         (win (get-buffer-window capture-buf)))
    (if win
        (with-selected-window win
          (kill-buffer-and-window))
      (kill-buffer capture-buf))))

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


(defun bib-capture-goto-last-stored (&optional noshow)
  "Go to the last BibTeX entry inserted by `bib-capture-confirm`.

If NOSHOW is non-nil (e.g., called with `C-u` or from Lisp), return a marker
to the entry location instead of jumping to it."
  (interactive "P")
  (unless (and (boundp 'bib-capture-last-stored) bib-capture-last-stored)
    (user-error "No previous BibTeX entry location recorded."))

  (let* ((file (car bib-capture-last-stored))
         (key (cdr bib-capture-last-stored)))
    (unless (file-exists-p file)
      (user-error "File does not exist: %s" file))
    (let ((buf (find-file-noselect file)))
      (with-current-buffer buf
        (if-let ((loc (bibtex-search-entry key)))
            (if noshow
                (point-marker)
              (pop-to-buffer buf)  ;; ensures buffer is displayed
              (goto-char loc))
          (user-error "Entry with key '%s' not found in %s" key file))))))


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
  "Insert BibTeX for DOI into a temporary buffer, or run
`bibtex-entry` if called with prefix arg."
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
(defun bib-capture-add-file (citekey &optional file noconfirm)
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
         (t (read-file-name (format "Add file for %s: " citekey)
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
                     (rename-file filepath destfile ok-if-already-exists))
                   :extension ext)))
        (citar-save-file-to-library citekey source-plist)
        (message "Saved %s as a resource for %s" filepath citekey)))))

;;;###autoload
(defun bib-capture-add-file-to-last-capture (&optional file noconfirm)
  "Add a file to the library for the last stored entry.

FILE and NOCONFIRM are as in `bib-capture-add-to-library', which see."
  (interactive)
  (let ((key (cdr bib-capture-last-stored)))
    (if (bib-capture--entry-exists key)
        (bib-capture-add-file key file noconfirm)
      (user-error "There is no entry for %s in %s" key file))))

(provide 'bib-capture)
;;; bib-capture.el ends here
