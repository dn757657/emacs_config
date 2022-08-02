;;---------------------QOL - STARTUP-------------------------------------------------------------------------
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)


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

(defun rune/evil-hook()
  (dolist (mode '(custom-mode
		  eshell-mode
		  gitrebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;------------------------------------------PROJECT GIT---------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "C:\\Users\\Daniel\\Projects")
    (setq projectile-project-search-path '("C:\\Users\\Daniel\\Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(use-package forge)

;;--------------------------------------------------------ORG MODE----------------------------------------

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t)
  (setq org-agenda-files
	'("C:\\Users\\Daniel\\Documents\\08_Emacs\\org\\Tasks.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("finmodels" . ?f)
       ("emacs" . ?e)
       ("masc" . ?m)
       ("admin" . ?a)
       )) 
					;
  ;Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
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
     ((todo "WAIT"
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
      ("Tasks.org" :maxlevel . 5)))
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "C:/Users/Daniel/Documents/08_Emacs/org/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "C:/Users/Daniel/Documents/08_Emacs/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      
     ;; ("jm" "Meeting" entry
     ;      (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
     ;      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
     ;      :clock-in :clock-resume
     ;      :empty-lines 1)

     ; ("w" "Workflows")
     ; ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
     ;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Mood" table-line (file+headline "C:/Users/Daniel/Documents/08_Emacs/org/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
      ("s" "Slipbox" entry  (file "C:/Users/Daniel/Documents/08_Emacs/roam/inbox.org")
       "* %?\n")
      ))

  (define-key global-map (kbd "C-c c")
    (lambda () (interactive) (org-capture nil)))
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("*" "*" "*" "*" "*" "*" "*")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


;;----------------------------------------------------------ORG ROAM------------------------------------------------------------------
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template "${title:*} ${tags:50}")
  :bind
  ("C-t" . org-roam-tag-add)
  :custom
  (org-roam-directory "C:/Users/Daniel/Documents/08_Emacs/roam")
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
         (file+head "reference/${title}.org" "#+title: ${title}\n#+filetags: :ref:")
         :immediate-finish t
         :unnarrowed t)
     ;("b" "braindump" entry  (file "/inbox.org")
     ;  "* %?\n")
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
  (org-roam-db-autosync-mode))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(set-face-attribute 'default nil :font "Lucida Sans Typewriter" :height 120)					; org mdoe config
(defun efs/org-font-setup ()
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
;; TeX Configuration ------------------------------------------------------ 	
(use-package tex
  :ensure auctex)

(use-package latex-preview-pane)

(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)	
(setq-default TeX-master nil)


;; Biblio Configuration ------------------------------------------------------
;; Set bibliography paths so they are the same.
(defvar dc/bibs '("C:/Users/Daniel/Documents/08_Emacs/roam/biblio.bib"))

(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

;; bibtex config
(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq org-roam-bibtex-mode 1))

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")

(setq ivy-bibtex-default-action 'ivy-bibtex-edit-notes)
(ivy-add-actions 'ivy-bibtex '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
			       ("i" ivy-bibtex-insert-citation "Insert Citation")))

;;===format of citations===
(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

(setq bibtex-completion-bibliography dc/bibs
      bibtex-completion-library-path '("C:/Users/Daniel/Zotero/storage")
      bibtex-completion-notes-path '"C:/Users/Daniel/Documents/08_Emacs/roam/reference"
      bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-display-formats
      '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}"))

      bibtex-completion-pdf-open-function 'find-file
      )

;; spell checking with ispell and ivy flyspelll
(add-to-list 'exec-path "C:/Users/Daniel/Documents/08_Emacs/pkg/ispell/bin/")

(setq ispell-program-name (locate-file "hunspell"
				       exec-path exec-suffixes 'file-executable-p))
(defvar dc/hunspelldictpath '"C:/Users/Daniel/Documents/08_Emacs/pkg/ispell/share/hunspell/personal.en")
(setq ispell-local-dictionary-alist '((nil
				       "[[:alpha:]]"
				       "[^[:alpha:]]"
				       "[']"
				       t
				       ("-d" "en_GB" "-p" "C:/Users/Daniel/Documents/08_Emacs/pkg/ispell/share/hunspell/personal.en")
				       nil
				       iso-8859-1)

				      ("american"
				       "[[:alpha:]]"
				       "[^[:alpha:]]"
				       "[']"
				       t
				       ("-d" "en_US" "-p" "C:/Users/Daniel/Documents/08_Emacs/pkg/ispell/share/hunspell/personal.en")
				       nil
				       iso-8859-1)
				      ))

;;----------------------DEPRECATED - NOT IN USE---------------------------------------
;; iCalendar Config
(use-package gnus-icalendar)
(gnus-icalendar-setup)

;; to enable optional iCalendar->Org sync functionality
;; NOTE: both the capture file and the headline(s) inside must already exist
(setq gnus-icalendar-org-capture-file "C:/Users/Daniel/Documents/08_Emacs/org/Tasks.org")
(setq gnus-icalendar-org-capture-headline '("Inbox"))
(gnus-icalendar-org-setup)


;(setq org-cite-global-bibliography dc/bibs)

;(use-package citar
;  :bind (("C-c b" . citar-insert-citation)
;         :map minibuffer-local-map
;         ("M-b" . citar-insert-preset))
;  :custom
;  (citar-bibliography dc/bibs))

;; Use `citar' with `org-cite'
;(use-package citar-org
;  :
;after oc
;  :custom
;  (org-cite-insert-processor 'citar)
;  (org-cite-follow-processor 'citar)
;  (org-cite-activate-processor 'citar))

(use-package citar-org
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(forge evil-magit magit counsel-projectile projectile latex-preview-pane citar-org gnus-icalendar which-key visual-fill-column use-package rainbow-delimiters org-roam-bibtex org-bullets ivy-rich helpful general evil-collection doom-themes counsel auctex))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
