(setq debug-on-error t)

;;---------------------QOL - STARTUP-------------------------------------------------------------------------
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Start in Org mode
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; Set up the visible bell
(setq visible-bell t)

(defvar dc/main-dir (getenv "HOME"))
(defvar dc/org-dir (expand-file-name "org" dc/main-dir))
(defvar dc/roam-dir (expand-file-name "roam" dc/main-dir))

;;---------------------PACKAGE MANAGEMENT-----------------------------------------------------------------

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)                 ; Initializes the package system and prepares it to be used

(unless package-archive-contents     ; Unless a package archive already exists,
  (package-refresh-contents))        ; Refresh package contents so that Emacs knows which packages to load


;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)        ; Unless "use-package" is installed, install "use-package"
  (package-install 'use-package))

(require 'use-package)                            ; Once it's installed, we load it using require

;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
(setq use-package-always-ensure t)


;;--------------------USAGE - NAVIGATION - BINDING CONFIG-------------------------------------------------

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delayo 0))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

; custom bindings
(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 ("M-x" . 'counsel-M-x)
	 ("C-x b" . 'counsel-ibuffer)
	 ("C-x C-f" . 'counsel-find-file)
	 :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; (defun rune/evil-hook()
;;   (dolist (mode '(custom-mode
;; 		  eshell-mode
;; 		  gitrebase-mode
;; 		  erc-mode
;; 		  circe-server-mode
;; 		  circe-chat-mode
;; 		  circe-query-mode
;; 		  term-mode))
;;     (add-to-list 'evil-emacs-state-modes mode)))

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   :hook (evil-mode . rune/evil-hook)
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;;   ;; Use visual line motions even outside of visual-line-mode buffers
;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
;;   (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

;;------------------------------------------PROJECT GIT---------------------------------------------

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (when (file-directory-p "C:\\Users\\Daniel\\projects")
;;     (setq projectile-project-search-path '("C:\\Users\\Daniel\\projects")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

;; (use-package forge)

;;------------------------------------------ PASS ENCRYPT ---------------------------------------------
;; (use-package auth-source)

;; (add-to-list 'auto-mode-alist '("\\.gpg\\'" . authinfo-mode))

;; (setq epa-pinentry-mode 'loopback)

;;--------------------------------------------------------ORG MODE----------------------------------------

(defun add-property-with-date-captured ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a]")))

(add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

(use-package org
  :hook (org-mode . dc/org-mode-setup)
  :bind
  (("C-o" . org-open-at-point-global))
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t)
  (setq org-agenda-files '(dc/org-dir))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-repeat-to-state "READY")
  
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-tag-alist
    '((:Startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("finmodels" . ?f)
       ("emacs" . ?e)
       ("masc" . ?m)
       ("admin" . ?a)
     )
  ) 
					;
  ;Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)(org-agenda-span 14)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("t" "Next/Ready Tasks - Masc"
     ((tags-todo "+masc+TODO=\"ACTIVE\""
		 ((org-agenda-overriding-header "Active Tasks")))
      (tags-todo "+masc+TODO=\"NEXT\""
		((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "+masc+TODO=\"READY\""
		 ((org-agenda-overriding-header "Ready Tasks")))
      (tags-todo "+masc+TODO=\"PLAN\""
	   ((org-agenda-overriding-header "Plan Tasks")))
      (tags-todo "+masc+TODO=\"WAIT\""
		 ((org-agenda-overriding-header "Waiting")))
     ))
    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "TODO"
            ((org-agenda-overriding-header "Assign Workflow")
             (org-agenda-files org-agenda-files)))
      (todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "NEXT"
            ((org-agenda-overriding-header  "Work on Next")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))
  ;; Save Org buffers after refiling!

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp (expand-file-name "Tasks.org" dc/org-dir) "Inbox")
         "* TODO %?\n" :empty-lines 1)

        ("j" "Journal Entries")
        ("jj" "Journal" entry
         (file+olp+datetree (expand-file-name "Journal.org" dc/org-dir))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
        ;; The following sections are commented out, ensure the parentheses match if uncommented
        ;; ("jm" "Meeting" entry
        ;;  (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
        ;;  "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
        ;;  :clock-in :clock-resume
        ;;  :empty-lines 1)
        ;; ("w" "Workflows")
        ;; ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
        ;;  "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
        ;; ("m" "Metrics Capture")
        ;; ("mw" "Mood" table-line (file+headline "C://Users//Daniel//home//emacs//org/Metrics.org" "Weight")
        ;;  "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
        ;; ("f" "Fleeting" entry  (file "~//roam//inbox.org")
        ;;  "* %?\n")
        ))

(define-key global-map (kbd "C-c c")
  (lambda () (interactive) (org-capture nil)))
)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("*" "*" "*" "*" "*" "*" "*")))

(defun dc/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


;;----------------------------------------------------------ORG ROAM------------------------------------------------------------------
(use-package org-roam
  :hook (org-load . org-roam-mode)
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template "${title:*} ${tags:50}")
  :bind
  ("C-t" . org-roam-tag-add)
  :custom
  (org-roam-directory dc/roam-dir)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:")
      :unnarrowed t)
     ("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n#+filetags:")
         :immediate-finish t
         :unnarrowed t)
     ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+ROAM_TAGS: research\n#+ROAM_CUSTOM_ID: {{cite:%k}}")
         :immediate-finish t
         :unnarrowed t)
     
     ("b" "fleeting" entry  (file "/inbox.org")
      "* %?\n")
     ; does the same thing as org-capture slipbox/fleeting
   ))
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  ;; Auto-compile Org Roam notes on startup
  (add-hook 'after-init-hook 'org-roam-db-sync)
  )

(setq org-roam-refile-targets '(("Tasks.org" :maxlevel . 1)))


(defvar dc/excluded-capture-templates '("fleeting")
  "List of capture template names to exclude from creation time logging.")
(defun dc/log-todo-creation-date (&rest ignore)
  "Log TODO creation time in the property drawer under the key 'CREATED'."
  (when (and (org-get-todo-state)
             (not (org-entry-get nil "CREATED"))
	     (not (member (org-capture-get :template) my/excluded-capture-templates))
	     )
    (org-entry-put nil "CREATED" (format-time-string (cdr org-time-stamp-formats)))))

(advice-add 'org-insert-todo-heading :after #'dc/log-todo-creation-date)
(advice-add 'org-insert-todo-heading-respect-content :after #'dc/log-todo-creation-date)
(advice-add 'org-insert-todo-subheading :after #'dc/log-todo-creation-date)

(defun dc/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(set-face-attribute 'default nil :font "Lucida Sans Typewriter" :height 120)					; org mdoe config
(defun dc/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Lucida Sans Typewriter" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way  
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  )

;;--------------------------------------------------------APPEARANCE---------------------------------------------------------
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


;;-------------------------------------------------------WRITING - ACADEMIC---------------------------------------------------
;;------------------------------------------------------ TeX Configuration ---------------------------------------------------	
;; (use-package tex
;;   :ensure auctex)

;; (use-package latex-preview-pane)

;; (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

;; (setq TeX-master t)  ;; compile in tex file directory
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)	
;; (setq-default TeX-master nil)

;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (setq default-directory "C:/Users/Daniel/home/writing")))


;;-------------------------------------------------------- Spell Checking ---------------------------------------------------
;; Path to aspell executable
;;(setq ispell-program-name "C:/Users/Daniel/home/emacs/.emacs.d/elpa/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe") ;; modify this with your path
;; You could set it to either depending on your preference
;;(setq ispell-dictionary "en_CA") ;; or you can use "british"

;; Enable Flyspell mode for Org mode
;;(add-hook 'org-mode-hook 'flyspell-mode)

;; Enable Flyspell for comments and strings in programming modes
;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;----------------------------------------------------------BIB CONFIG ------------------------------------------------------
;; Set bibliography paths so they are the same.
;; (defvar dc/bibs '("C:/Users/Daniel/home/ref_management/masc.bib"))

;; ;; org roam bibtex config
;; (use-package org-roam-bibtex
;;   :after org-roam
;;   :config
;;   (setq org-roam-bibtex-mode 1))

;; ;; bib entry appearannce in completion menu mods
;; (setq bibtex-completion-pdf-symbol "⌘")
;; (setq bibtex-completion-notes-symbol "✎")

;; ;;ivy-bibtex config
;; (autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; (setq ivy-re-builders-alist
;;       '((ivy-bibtex . ivy--regex-ignore-order)
;;         (t . ivy--regex-plus)))

;; (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
;; ;;(ivy-add-actions 'ivy-bibtex '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")))

;; ;; helper functions
;; (defun dc/bibtex-completion-get-pdf (entry)
;;   "Get the path of the first PDF of ENTRY, or nil if none exists."
;;   (let ((pdfs (bibtex-completion-find-pdf entry)))
;;     (when pdfs
;;       (car pdfs))))

;; (defun dc/bibtex-completion-insert-noter-document (entry)
;;   "Get Org property syntax string for :NOTER_DOCUMENT: for ENTRY, or an empty string if no PDF exists."
;;   (if-let ((pdf (dc/bibtex-completion-get-pdf entry)))
;;       (format ":NOTER_DOCUMENT: %s\n" pdf)
;;     ""))

;;===format of citations===
;; (setq bibtex-completion-cite-prompt-for-optional-arguments nil)

;; (defun my/bibtex-completion-format-citation (keys)
;;   (concat "[cite:"
;;           (mapconcat 'identity keys ",")
;;           "]"))

;;(add-to-list 'bibtex-completion-format-citation-functions
;;             '(my/org-mode . my/bibtex-completion-format-citation))



;; (setq bibtex-completion-format-citation-functions
;;   '((org-mode      . my/bibtex-completion-format-citation)
;;     (latex-mode    . bibtex-completion-format-citation-cite)
;;     (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;     (default       . bibtex-completion-format-citation-default)))

;; (setq bibtex-completion-bibliography dc/bibs
;;       bibtex-completion-library-path '("C:/Users/Daniel/home/ref_management/zotfile_repository/")
;;       bibtex-completion-notes-path '"C:/Users/Daniel/home/emacs/roam/reference/"
;;       bibtex-completion-notes-template-multiple-files
;;       (concat
;;        "#+TITLE: ${title}\n"
;;        "#+ROAM_KEY: cite:${=key=}\n"
;;        ":PROPERTIES:\n"
;;        ":AUTHOR: ${author-abbrev}\n"
;;        ":JOURNAL: ${journaltitle}\n"
;;        ":DATE: ${date}\n"
;;        ":YEAR: ${year}\n"
;;        ":DOI:  ${doi}\n"
;;        ":URL:  ${url}\n"
;;        ":END:\n\n"
;;        "* Org-Noter \n"
;;        ":PROPERTIES:\n"
;;        "%(dc/bibtex-completion-insert-noter-document entry)"
;;        ":END:"
;;        )
;;       ;;"* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
;;       bibtex-completion-additional-search-fields '(keywords)
;;       bibtex-completion-display-formats
;;       '((t . " ${year:4}  ${author:30}  ${title:*} ${=has-pdf=:1} ${=has-note=:1}"))
      
;;       bibtex-completion-pdf-open-function 'find-file
;;       )


;;----------------------PDF VIEWING ---------------------------------------
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install)
;;   )

;;----------------------PDF NOTING ---------------------------------------

;; (use-package org-noter
;;   :ensure t)

;; (setq org-noter-notes-search-path '("C:/Users/Daniel/home/emacs/roam/reference"))


;;----------------------DEPRECATED - NOT IN USE---------------------------------------
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(pdf-tools evil-magit which-key visual-fill-column use-package rainbow-delimiters org-roam-bibtex org-ref org-noter org-bullets latex-preview-pane ivy-rich ivy-bibtex helpful gnu-elpa-keyring-update general forge evil-collection doom-themes counsel-projectile auctex)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
