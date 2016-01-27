((apel status "installed" recipe
       (:name apel :website "http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/elisp/APEL/" :description "APEL (A Portable Emacs Library) is a library to support to write portable Emacs Lisp programs." :type github :pkgname "wanderlust/apel" :build
	      (mapcar
	       (lambda
		 (target)
		 (list el-get-emacs
		       (split-string "-batch -q -no-site-file -l APEL-MK -f")
		       target "prefix" "site-lisp" "site-lisp"))
	       '("compile-apel" "install-apel"))
	      :load-path
	      ("site-lisp/apel" "site-lisp/emu")))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
		       (popup fuzzy)
		       :features auto-complete-config :post-init
		       (progn
			 (add-to-list 'ac-dictionary-directories
				      (expand-file-name "dict" default-directory))
			 (ac-config-default))))
 (auto-complete-c-headers status "installed" recipe
			  (:name auto-complete-c-headers :website "https://github.com/mooz/auto-complete-c-headers" :description "An auto-complete source for C/C++ header files." :type github :pkgname "mooz/auto-complete-c-headers" :depends auto-complete :prepare
				 (progn
				   (defun ac--c-headers-init nil
				     (require 'auto-complete-c-headers)
				     (add-to-list 'ac-sources 'ac-source-c-headers))
				   (add-hook 'c-mode-hook 'ac--c-headers-init)
				   (add-hook 'c++-mode-hook 'ac--c-headers-init))))
 (auto-highlight-symbol status "installed" recipe
			(:name auto-highlight-symbol :type github :pkgname "emacsmirror/auto-highlight-symbol" :description "Automatic highlighting current symbol minor mode" :website "https://github.com/emacsmirror/auto-highlight-symbol/"))
 (cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (cperl-mode status "installed" recipe
	     (:name cperl-mode :website "https://github.com/jrockway/cperl-mode" :description "Perl code editing commands for Emacs" :type github :pkgname "jrockway/cperl-mode" :depends mode-compile :compile "cperl-mode.el" :provide cperl-mode :post-init
		    (defalias 'perl-mode 'cperl-mode)))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
		("el-get.*\\.el$" "methods/")
		:features el-get :post-init
		(when
		    (memq 'el-get
			  (bound-and-true-p package-activated-list))
		  (message "Deleting melpa bootstrap el-get")
		  (unless package--initialized
		    (package-initialize t))
		  (when
		      (package-installed-p 'el-get)
		    (let
			((feats
			  (delete-dups
			   (el-get-package-features
			    (el-get-elpa-package-directory 'el-get)))))
		      (el-get-elpa-delete-package 'el-get)
		      (dolist
			  (feat feats)
			(unload-feature feat t))))
		  (require 'el-get))))
 (epl status "installed" recipe
      (:name epl :description "EPL provides a convenient high-level API for various package.el versions, and aims to overcome its most striking idiocies." :type github :pkgname "cask/epl"))
 (flymake-cursor status "installed" recipe
		 (:name flymake-cursor :type github :pkgname "illusori/emacs-flymake-cursor" :description "displays flymake error msg in minibuffer after delay (illusori/github)" :website "http://github.com/illusori/emacs-flymake-cursor"))
 (flymake-easy status "installed" recipe
	       (:name flymake-easy :type github :description "Helpers for easily building flymake checkers" :pkgname "purcell/flymake-easy" :website "http://github.com/purcell/flymake-easy"))
 (flymake-shell status "installed" recipe
		(:name flymake-shell :type github :pkgname "purcell/flymake-shell" :description "A flymake syntax-checker for shell scripts" :website "http://github.com/purcell/flymake-shell" :depends
		       (flymake-easy)
		       :post-init
		       (add-hook 'shell-script-mode-hook 'flymake-shell-load)))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (git-gutter status "installed" recipe
	     (:name git-gutter :description "Emacs port of GitGutter Sublime Text 2 Plugin" :website "https://github.com/syohex/emacs-git-gutter" :type github :pkgname "syohex/emacs-git-gutter"))
 (highlight-symbol status "installed" recipe
		   (:name highlight-symbol :description "Quickly highlight a symbol throughout the buffer and cycle through its locations." :type github :pkgname "nschum/highlight-symbol.el"))
 (iedit status "installed" recipe
	(:name iedit :description "Edit multiple regions with the same content simultaneously." :type emacswiki :features iedit))
 (indent-guide status "installed" recipe
	       (:name indent-guide :description "show vertical lines to guide indentation." :website "https://github.com/zk-phi/indent-guide" :type github :pkgname "zk-phi/indent-guide"))
 (js2-mode status "installed" recipe
	   (:name js2-mode :website "https://github.com/mooz/js2-mode#readme" :description "An improved JavaScript editing mode" :type github :pkgname "mooz/js2-mode" :prepare
		  (autoload 'js2-mode "js2-mode" nil t)))
 (let-alist status "installed" recipe
	    (:name let-alist :description "Easily let-bind values of an assoc-list by their names." :builtin "25.1" :type http :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/let-alist.el"))
 (minimap status "installed" recipe
	  (:name minimap :description "Minimap sidebar for Emacs" :type elpa))
 (mode-compile status "installed" recipe
	       (:name mode-compile :description "Smart command for compiling files according to major-mode." :type http :url "https://raw.github.com/emacsmirror/mode-compile/master/mode-compile.el" :load-path
		      (".")))
 (package status "installed" recipe
	  (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin "24" :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
		 (progn
		   (let
		       ((old-package-user-dir
			 (expand-file-name
			  (convert-standard-filename
			   (concat
			    (file-name-as-directory default-directory)
			    "elpa")))))
		     (when
			 (file-directory-p old-package-user-dir)
		       (add-to-list 'package-directory-list old-package-user-dir)))
		   (setq package-archives
			 (bound-and-true-p package-archives))
		   (mapc
		    (lambda
		      (pa)
		      (add-to-list 'package-archives pa 'append))
		    '(("ELPA" . "http://tromey.com/elpa/")
		      ("melpa" . "http://melpa.org/packages/")
		      ("gnu" . "http://elpa.gnu.org/packages/")
		      ("marmalade" . "http://marmalade-repo.org/packages/")
		      ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (perl-completion status "installed" recipe
		  (:name perl-completion :auto-generated t :type emacswiki :description "- minor mode provides useful features for editing perl codes" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/perl-completion.el"))
 (pkg-info status "installed" recipe
	   (:name pkg-info :description "Provide information about Emacs packages." :type github :pkgname "lunaryorn/pkg-info.el" :depends
		  (dash epl)))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :submodule nil :pkgname "auto-complete/popup-el"))
 (powerline status "installed" recipe
	    (:name powerline :website "https://github.com/milkypostman/powerline" :depends
		   (cl-lib)
		   :description "Powerline for Emacs" :type github :pkgname "milkypostman/powerline" :load-path "." :features powerline))
 (python status "installed" recipe
	 (:name python :description "Python's flying circus support for Emacs (trunk version, hopefully Emacs 24.x compatible)" :type http :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/python.el?h=master"))
 (smart-newline status "installed" recipe
		(:name smart-newline :description "The smart-newline.el provide a stress-less newline command for programmer." :type github :pkgname "ainame/smart-newline.el"))
 (tabbar status "installed" recipe
	 (:name tabbar :description "Display a tab bar in the header line." :type github :pkgname "dholm/tabbar" :lazy t))
 (tabbar-ruler status "installed" recipe
	       (:name tabbar-ruler :website "https://github.com/mlf176f2/tabbar-ruler.el" :description "Tabbar ruler is an emacs package that allows both the tabbar and the ruler to be used together. In addition it allows auto-hiding of the menu-bar and tool-bar." :type github :depends tabbar :pkgname "mlf176f2/tabbar-ruler.el"))
 (tern status "installed" recipe
       (:name tern :description "A JavaScript code analyzer for deep, cross-editor language support." :type github :pkgname "marijnh/tern" :build
	      '(("npm" "--production" "install"))
	      :prepare
	      (add-to-list 'auto-mode-alist
			   '("\\.tern-\\(project\\|\\config\\)\\'" . json-mode))
	      :load-path
	      ("emacs")))
 (web-mode status "installed" recipe
	   (:name web-mode :description "emacs major mode for editing PHP/JSP/ASP HTML templates (with embedded CSS and JS blocks)" :type github :pkgname "fxbois/web-mode")))
