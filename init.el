;; ------  Basic  evil mode ------
;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; ----------- Status line package -----------
(unless (package-installed-p 'airline-themes)
  (package-install 'airline-themes))
(require 'airline-themes)
(load-theme 'airline-light t)

;; ---------- Neotree ------
(unless (package-installed-p 'neotree)
  (package-install 'neotree))
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))

;; --------- Dashboard --------
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner  2)

;; ------------ flycheck --------
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

(global-flycheck-mode)

;; ------------ Theme --------
(unless (package-installed-p 'monokai-theme)
  (package-install 'monokai-theme))
(load-theme 'monokai t)

;; --------- Random keybinds and specifics ---------- 
;; linum mode spacing
(setq linum-format "%d ")
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Cursor change in evil
(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "medium sea green")
      evil-visual-state-cursor '(hollow "orange"))

;; Enable mouse
(xterm-mouse-mode 1)

;; Comment uncomment shortcuts
(global-set-key (kbd "C-a") 'comment-region)
(global-set-key (kbd "C-s") 'uncomment-region)

;; ------- Auto generated stuff ------------- 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(airline-themes use-package evil-visualstar evil-surround evil-replace-with-register evil-lion evil-goggles evil-expat evil-exchange evil-commentary evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



