;; packaging

(setq native-comp-async-report-warnings-errors nil)
(setq straight-repository-branch "develop")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))

      package-archive-priorities
      '(("melpa" . 10)
        ("melpa-stable" . 5)
        ("gnu" . 1)
        ("jcs-elpa" . 0)))
                          
(package-initialize)

;; use-package
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

;; straigt.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; https://github.com/syl20bnr/spacemacs/issues/12535
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; https://mail.gnu.org/archive/html/help-gnu-emacs/2024-04/msg00211.html
;; (setq package-check-signature nil)

;; func to load other init files
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; store customized vars in separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


(setq
  inhibit-startup-message t ;; Don't show the startup screen
  ring-bell-function 'ignore ;; turn off hells & bells
  column-number-mode t ;; Display line and column numbers
  backup-directory-alist '(("." . "~/.emacs.d/.tmp/backups/")) ;; Make sure all backup files only live in one place
  auto-save-file-name-transforms '((".*" "~/.emacs.d/.tmp/auto-saves/" t)) ;; dont litter autosave files
  auto-save-list-file-prefix "~/.emacs.d/.tmp/auto-saves/.auto-save-list-"
  bookmark-save-flag 1 ;; save bmarks
  load-prefer-newer t ;; always get newest files
  gc-cons-threshold 100000000 ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; gc-cons-threshold most-positive-fixnum
  redisplay-dont-pause t ;; https://www.masteringemacs.org/article/improving-performance-emacs-display-engine
  frame-inhibit-implied-size t ;; Don't resize frame as i use tiling manager (saves startup time)
  read-process-output-max (* 1024 1024) ;; 1mb https://emacs-lsp.github.io/lsp-mode/page/performance/
  auto-window-vscroll nil ;; https://emacs.stackexchange.com/a/28746
  tab-stop-list (number-sequence 4 200 4)
  indent-line-function 'insert-tab
  windmove-wrap-around t
  warning-minimum-level :error ;; don't shout at me if not necessary (esp. native-comp is loud)
  initial-scratch-message
  (concat ";; evaluate & print      C-j\n"
          ";; evaluate defun        C-M-x\n\n"
           ) ;; don't fuck w/ my scratches
)


(setq-default
 bidi-display-reordering nil  ;; should speedup long lines rendering https://emacs.stackexchange.com/a/603
 abbrev-mode nil
 indent-tabs-mode nil
 tab-width 4
 kill-read-only-ok t ;; sta:copy-line (see defuns)
)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; no menu bar
(menu-bar-mode -1)

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; save on exit
(desktop-save-mode 1)

;; sync buffers on disk change
(global-auto-revert-mode t)

;; show mathing paren
(show-paren-mode t)

(global-unset-key (kbd "C-q"))

(use-package company
  :ensure t
  :delight
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0 ;; default is 0.2
   company-selection-wrap-around t)
  (global-company-mode t))

(use-package vertico
  :ensure t
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package vertico-prescient
  :ensure t
  :config
  (setq prescient-filter-method '(literal fuzzy)
        ;; applied in order until one matches
        ;; Value `literal' means the subquery must be a substring of the
        ;; candidate. Supports char folding.

        ;; Value `literal-prefix' means the first subquery must be the
        ;; prefix of the candidate and the remaining subqueries must be
        ;; prefixes of words in the candidate. Supports char folding.

        ;; Value `regexp' means the subquery is interpreted directly as a
        ;; regular expression.

        ;; Value `initialism' means the subquery must match a substring of
        ;; the initials of the candidate.

        ;; Value `fuzzy' means the characters of the subquery must match
        ;; some subset of those of the candidate, in the correct order but
        ;; not necessarily contiguous.

        ;; Value `prefix' means the words (substrings of only word
        ;; characters) match the beginning of words found in the candidate,
        ;; in order, separated by the same non-word characters that separate
        ;; words in the query. This is similar to the completion style
        ;; `partial'.

        ;; Value `anchored' means words are separated by capital letters or
        ;; symbols, with capital letters being the start of a new word. This
        ;; is similar to `prefix', but allows for less typing.

        ;; Value can also be a list of any of the above methods, in which
        ;; case each method will be applied in order until one matches.

        ;; Value can also be a function which returns any of the allowable
        ;; values documented above.
        ;; to make sorting and filtering more intelligent

        prescient-use-char-folding t
        prescient-use-case-folding 'smart
        prescient-sort-full-matches-first t ; Works well with `initialism'.
        prescient-sort-length-enable t)


  (vertico-prescient-mode)
  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

;; (use-package vertico-posframe
;;   :ensure t
;;   :custom
;;   (vertico-posframe-parameters
;;    '((left-fringe . 8)
;;      (right-fringe . 8))))

;;(use-package orderless
;;  :ensure t
;;  :custom
;;  (completion-styles '(orderless basic))
;;  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company-prescient
  :ensure t
  :straight t
  :config
  (company-prescient-mode +1))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 2))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package nlinum
	    :ensure t
	    :init
	    (global-nlinum-mode t)
         (setq nlinum-format "%d ")
)

(use-package buffer-move
  :ensure t
  ;;:bind (("M-S up" . buf-move-up)
  ;;       ("M-S down" . buf-move-down)
  ;;       ("M-S left" . buf-move-left)
  ;;       ("M-S right" . buf-move-right))
  )

(use-package goto-chg
  :ensure t
  ;;:bind ("M-C r" . goto-last-change)
  )

;; apt install shellcheck
(use-package flycheck
  :ensure t
  :config
  :hook
  ((emacs-lisp-mode . flycheck-mode)
   (sh-mode . flycheck-mode)))

;; systemd units support
(use-package systemd
  :ensure t
  :defer
  :mode ("\\.service$" . systemd-mode))

;; ansible
(use-package poly-ansible
  :ensure t
  :defer
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

;; dockerfile
(use-package dockerfile-mode :ensure t :defer)

;; markdown support
(use-package markdown-mode
  :ensure t
  :defer
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; rst support
(use-package rst
  :ensure t
  :defer
  :mode (("\\.txt\\'" . rst-mode)
         ("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;; apt install git
(use-package magit
  :ensure t
  ;; :custom
  ;;(magit-diff-auto-show 't) ; dont show diffs on comit and such
  :bind ("C-x c" . magit-status))

;; undo buffer changes
(use-package undo-tree
  :ensure t
  :delight
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/.tmp/undo")))
  (undo-tree-visualizer-timestamps t)
  :config
  ;; Prevent undo tree files from polluting conf dir
  (global-undo-tree-mode))

;; jump troigh windows
(use-package ace-window
  :ensure t
  :bind ("s-j" . ace-select-window)
  :init (setq aw-dispatch-always t))

;; show help while pressing part of the chord f.e M-q
(use-package which-key
  :ensure t
  :delight
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

;; jumping to last modified lines
(use-package goto-chg
  :ensure t
  :bind ("C-M-r" . goto-last-change))

;; moving buffers
(use-package buffer-move :ensure t)

;; easy line/region duplication
(use-package duplicate-thing
  :ensure t
  :bind ("C-S-d" . duplicate-thing))

(use-package crontab-mode :ensure t)

;; apt install ripgrep
(use-package rg
  :ensure t
  :config
  ;; https://github.com/Wilfred/deadgrep/issues/24#issuecomment-942290197
  (defun sta:deadgrep--include-args (rg-args)
  "Adds flags to rigrep"
  ;;(push "--hidden" rg-args) ;; consider hidden folders/files
  (push "--multiline" rg-args))
  (advice-add 'deadgrep--arguments :filter-return #'sta:deadgrep--include-args))

(use-package deadgrep
  :ensure t
  :pin melpa
  :bind (:map deadgrep-mode-map
              ("t" . sta:deadgrep-file-type)))

(defun sta:deadgrep-file-type ()
  "Prompt the user for a new file type, then restart the search in deadgrep"
  (interactive)
  (let ((new-file-type
         (deadgrep--read-file-type deadgrep--initial-filename)))
    (setq deadgrep--file-type (cons 'type new-file-type)))
  (rename-buffer
   (deadgrep--buffer-name deadgrep--search-term default-directory) t)
  (deadgrep-restart))

(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun sta:go-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))


(global-set-key (kbd "C-q s") 'sta:go-to-scratch-buffer)
(global-set-key (kbd "C-q p") 'package-upgrade-all)
(global-set-key (kbd "C-q j") 'ace-select-window) 
(global-set-key "\C-c\C-k" 'copy-line)
(global-set-key (kbd "<f5>") #'deadgrep)
(global-set-key [?\s-s] #'deadgrep)
