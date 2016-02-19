;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Basic Setting

;; Hide start-up message
(setq inhibit-startup-message t)

;; "yes or no" -> "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; display row line
(setq line-number-mode t)

;; display column line
(setq column-number-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emphasis current line
(global-hl-line-mode)

;; Setting color
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "#002021"))
    (t()))
  "*Face used by hl-line.")

(setq hl-line-face 'hlline-face)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't make back-up file
(setq backup-inhibited t)

;; Erase auto save file
(setq delete-auto-save-files t)

;; Stop blink of cursor
(blink-cursor-mode 0)

;; Hide scroll bar
(set-scroll-bar-mode nil)

;; Display line number
(require 'linum)
(global-linum-mode)
(setq linum-format "%4d ")

;; Option key -> Meta key
(setq mac-option-modifier 'meta)

;; Hide tool bar
(tool-bar-mode 0)

;; Light the paren
(show-paren-mode t)
(setq show-paren-style 'mixed) 

;; Setting of background
(if window-system (progn
		    (setq initial-frame-alist '((width . 155)(height . 57)(top . 0)(left . 58)))
		    (set-background-color "Black")
		    (set-foreground-color "White")
		    (set-cursor-color "Gray")
		    (set-frame-parameter nil 'alpha 85)
		    ))

;; Modify the iedit's bug
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; C-e (move to end of line)
(define-key global-map (kbd "C-e") 'move-end-of-line)


;; load-path
(setq load-path (append
		 (list
		  (expand-file-name "~/.emacs.d/elisp/el-get/tern/emacs")
		  )
		 load-path))


;; Character code
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-file-name-coding-system 'shift_jis)

;; font
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0212
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (cons "Ricty Discord" "iso10646-1"))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C, C++ basic setting

;; C, C++ no tab
(defun my-c-c++-mode-init ()
  (setq indent-tabs-mode nil)
  )

(add-hook 'c-mode-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)

;; setting indent for c++
(defun my-c++-indent ()
  (c-set-offset 'inline-open 0)
  (c-set-offset 'inline-close 0)
  (c-set-offset 'access-label -2)
  )

(add-hook 'c++-mode-hook 'my-c++-indent)

;; C, C++ configuration
(defun my-c-c++-mode-conf ()
  
  ;; style
  (c-set-style "stroustrup")

  ;; setting indent
  (c-set-offset 'case-label 4)
  (c-set-offset 'label 4)
  (c-set-offset 'statement-cont 'c-lineup-math) 
  
  ;; reflect a file written by other editor.
  (auto-revert-mode)
  )

(add-hook 'c-mode-hook 'my-c-c++-mode-conf)
(add-hook 'c++-mode-hook 'my-c-c++-mode-conf)


;; C++ (.h -> C++ file)
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode))
              auto-mode-alist))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake

(require 'flymake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C
(defun flymake-c-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "gcc" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c-mode-hook
          '(lambda ()
             (flymake-mode t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++(.cpp) for C++11
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-std=c++11" "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (flymake-mode t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++(.cc)

(defun flymake-cc2-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(push '("\\.cc$" flymake-cc2-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (flymake-mode t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "flake8" (list local-file))))
(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-python-init)))
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl
(defun flymake-perl-init ()
  (let* ((root (expand-file-name (or (vc-git-root default-directory) 
				     default-directory))))
    (list "perl" (list "-wc"  buffer-file-name) root)
    ))

(push '(".+\\.pl$" flymake-perl-init) flymake-allowed-file-name-masks)
(add-hook 'cperl-mode-hook (lambda () (flymake-mode t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell script
(add-hook 'sh-set-shell-hook 'flymake-shell-load)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change the color of flymake
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Move to error
(global-set-key "\M-n" 'flymake-goto-next-error)
(global-set-key "\M-p" 'flymake-goto-prev-error)

;; For preventing flymake is down
(defadvice flymake-post-syntax-check 
    (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto_complete

(defun my:ac-init ()
  (require 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict/")
  (add-to-list 'ac-dictionary-directories "~/.emacs.d//my_ac-dict/")
  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t)
  )

(add-hook 'c++-mode-hook 'my:ac-init)
(add-hook 'c-mode-hook 'my:ac-init)
(add-hook 'latex-mode-hook 'my:ac-init)
(add-hook 'html-mode 'my:ac-init)
(add-hook 'web-mode 'my:ac-init)
(setq ac-use-fuzzy t)

;; c-header
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc47/4.7.4/lib/gcc/x86_64-apple-darwin12.6.0/4.7.4/include")
  (add-to-list 'achead:include-directories '"/usr/local/Cellar/gcc47/4.7.4/lib/gcc/x86_64-apple-darwin12.6.0/4.7.4/include/c++")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
  )

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  el-get 

;; path
(add-to-list 'load-path (locate-user-emacs-file "elisp/el-get/el-get/"))

;; el-get
(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

;; auto-highlight-symbol
(require 'auto-highlight-symbol-config)

(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1")) 

;; keybind
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbar
(require 'tabbar)
(tabbar-mode 1)

(setq tabbar-ruler-swap-faces t)
(require 'tabbar-ruler)

(global-set-key "\M-]" 'tabbar-forward)  ; next tab
(global-set-key "\M-[" 'tabbar-backward) ; previous tab

;; Erase the left side button
(dolist (btn '(tabbar-buffer-home-button
	       tabbar-scroll-left-button
	       tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
		 (cons "" nil))))


;; Length of tab separator
(setq tabbar-separator '(1.5)) 

;; List of display
(defun my-tabbar-buffer-list ()
  (delq nil
	(mapcar #'(lambda (b)
		    (cond
	     ;; Always include the current buffer.
	     ((eq (current-buffer) b) b)
		     ((buffer-file-name b) b)
		     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
		     ((char-equal ?* (aref (buffer-name b) 0)) nil) 
		     ((buffer-live-p b) b)))
		(buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
(require 'web-mode)
;; setting of extention
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp?$"      . web-mode))

;; indent
(defun my_web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-script-padding 4)
  (setq web-mode-style-padding 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-block-padding 4)
  (setq indent-tabs-mode t)
  )

(add-hook 'web-mode-hook 'my_web-mode-hook)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'tern)

(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 4)
	     (set (make-local-variable 'js2-indent-switch-body) t)
	     (tern-mode t)
	     ))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup))
  )

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl mode
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (append '(("\\.psgi$" . cperl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cgi$" . cperl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.pl$" . cperl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.pm$" . cperl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.t$" . cperl-mode)) auto-mode-alist))

(add-hook 'cperl-mode-hook
          '(lambda ()
	     
	     ;; Setting for indent
             (cperl-set-style "PerlStyle")
             (custom-set-variables
              '(cperl-indent-parens-as-block t)
              '(cperl-close-paren-offset -4)
              '(cperl-indent-subs-specially nil))
	     
             ;; Display documents
             (define-key global-map (kbd "M-p") 'cperl-perldoc)
             ))

;; Look at source
(put 'perl-module-thing 'end-op
     (lambda ()
       (re-search-forward "\\=[a-zA-Z][a-zA-Z0-9_:]*" nil t)))
(put 'perl-module-thing 'beginning-op
     (lambda ()
       (if (re-search-backward "[^a-zA-Z0-9_:]" nil t)
           (forward-char)
         (goto-char (point-min)))))
(defun perldoc-m ()
  (interactive)
  (let ((module (thing-at-point 'perl-module-thing))
        (pop-up-windows t)
        (cperl-mode-hook nil))
    (when (string= module "")
      (setq module (read-string "Module Name: ")))
    (let ((result (substring (shell-command-to-string (concat "perldoc -m " module)) 0 -1))
          (buffer (get-buffer-create (concat "*Perl " module "*")))
          (pop-or-set-flag (string-match "*Perl " (buffer-name))))
      (if (string-match "No module found for" result)
          (message "%s" result)
        (progn
          (with-current-buffer buffer
            (toggle-read-only -1)
            (erase-buffer)
            (insert result)
            (goto-char (point-min))
            (cperl-mode)
            (toggle-read-only 1)
            )
          (if pop-or-set-flag
              (switch-to-buffer buffer)
            (display-buffer buffer)))))))

(global-set-key (kbd "M-m") 'perldoc-m)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP
(require 'php-mode)
(add-hook 'php-mode-hook
         (lambda ()
             (require 'php-completion)
             (php-completion-mode t)
             (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
             (when (require 'auto-complete nil t)
             (make-variable-buffer-local 'ac-sources)
             (add-to-list 'ac-sources 'ac-source-php-completion)
             (auto-complete-mode t))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-gutter
(require 'git-gutter)
(global-git-gutter-mode t)
(add-hook 'c++-mode-hook 'git-gutter-mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For folding

;; C++ style
(add-hook 'c++-mode-hook
	  '(lambda()
	     (hs-minor-mode 1)))

;; Key bind setting
(define-key
  global-map
  (kbd "C-.") 'hs-toggle-hiding)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minimap
(require 'minimap)

(defvar minimap-window nil)
(defun minimap-toggle ()
  (interactive)
  (if (and minimap-window
           (window-live-p minimap-window))
      (minimap-kill)
    (minimap-create)))

(global-set-key (kbd "C-c m") 'minimap-create)
(global-set-key (kbd "C-c d") 'minimap-kill)

;; Display right side
(setq minimap-window-location 'right)

;; set color
(custom-set-faces
 '(minimap-active-region-background ((t (:background "#494949"))) t)
 '(preview-reference-face ((t (:foreground "#00CCCC" :background "#CCCCCC"))) t)
 '(sr-active-path-face ((t (:foreground "#00CCCC" :weight bold :height 120))))
 '(sr-passive-path-face ((t (:foreground "#008888" :weight bold :height 120)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline.el
(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))


(defconst color1 "#666666")
(defconst color2 "999999")
(defconst color3 "#111111")
(defconst color4 "#CDC0B0")

(defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
(defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
(defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
(defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

(setq-default mode-line-format
	      (list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
				     (propertize " " 'display arrow-right-1)))
		     '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
				     (propertize " " 'display arrow-right-2)))

		     ;; Justify right by filling with spaces to right fringe - 16
		     ;; (16 should be computed rahter than hardcoded)
		     '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

		     '(:eval (concat (propertize " " 'display arrow-left-2)
				     (propertize " %p " 'face 'mode-line-color-2)))
		     '(:eval (concat (propertize " " 'display arrow-left-1)
				     (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
		     )) 

(make-face 'mode-line-color-1)
(set-face-attribute 'mode-line-color-1 nil
                    :foreground "#fff"
                    :background color1)

(make-face 'mode-line-color-2)
(set-face-attribute 'mode-line-color-2 nil
                    :foreground "#fff"
                    :background color2)

(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background color3
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fff"
                    :background color4)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-newline

;; c++
(add-hook 'c++-mode-hook
	  (lambda ()
	    (smart-newline-mode t)))

;; c
(add-hook 'c-mode-hook
	  (lambda ()
	    (smart-newline-mode t)))

;; web
(add-hook 'web-mode-hook
	  (lambda ()
	    (smart-newline-mode t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent-guide
(require 'indent-guide)
(add-hook 'c++-mode-hook 'indent-guide-mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Divide a window
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;; kbd C-t
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align code
(require 'align)
(add-to-list 'align-rules-list
	     '(ruby-xmpfilter-mark
	       (regexp . "\\(\\s-*\\)# =>")
	       (modes  . '(ruby-mode))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(eval-after-load "yasnippet"
  '(progn
     (define-key yas-keymap (kbd "<tab>") nil)
     (yas-global-mode 1)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(setq markdown-command "multimarkdown")

(require 'markdown-mode)

;; Set up for markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(setenv "LD_LIBRARY_PATH" "Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib")
;; irony mode
;(require 'irony)
;(require 'ac-irony)

;(defun my:irony-enable()
;  (when (member major-mode irony-known-modes)
;    (irony-mode 1)))

;(add-hook 'c-mode-hook 'my:irony-enable)
;(add-hook 'c++-mode-hook 'my:irony-enable)
