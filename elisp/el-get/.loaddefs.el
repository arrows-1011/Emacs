;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (vassoc set-modified-alist modify-alist remove-alist
;;;;;;  set-alist del-alist put-alist) "apel/site-lisp/apel/alist"
;;;;;;  "apel/site-lisp/apel/alist.el" (21930 33088 0 0))
;;; Generated autoloads from apel/site-lisp/apel/alist.el

(autoload 'put-alist "apel/site-lisp/apel/alist" "\
Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST.

\(fn KEY VALUE ALIST)" nil nil)

(autoload 'del-alist "apel/site-lisp/apel/alist" "\
Delete an element whose car equals KEY from ALIST.
Return the modified ALIST.

\(fn KEY ALIST)" nil nil)

(autoload 'set-alist "apel/site-lisp/apel/alist" "\
Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE.

\(fn SYMBOL KEY VALUE)" nil nil)

(autoload 'remove-alist "apel/site-lisp/apel/alist" "\
Delete an element whose car equals KEY from the alist bound to SYMBOL.

\(fn SYMBOL KEY)" nil nil)

(autoload 'modify-alist "apel/site-lisp/apel/alist" "\
Store elements in the alist MODIFIER in the alist DEFAULT.
Return the modified alist.

\(fn MODIFIER DEFAULT)" nil nil)

(autoload 'set-modified-alist "apel/site-lisp/apel/alist" "\
Store elements in the alist MODIFIER in an alist bound to SYMBOL.
If SYMBOL is not bound, set it to nil at first.

\(fn SYMBOL MODIFIER)" nil nil)

(autoload 'vassoc "apel/site-lisp/apel/alist" "\
Search AVLIST for an element whose first element equals KEY.
AVLIST is a list of vectors.
See also `assoc'.

\(fn KEY AVLIST)" nil nil)

;;;***

;;;### (autoloads (module-installed-p exec-installed-p file-installed-p
;;;;;;  get-latest-path add-latest-path add-path) "apel/site-lisp/apel/path-util"
;;;;;;  "apel/site-lisp/apel/path-util.el" (21930 33088 0 0))
;;; Generated autoloads from apel/site-lisp/apel/path-util.el

(autoload 'add-path "apel/site-lisp/apel/path-util" "\
Add PATH to `load-path' if it exists under `default-load-path'
directories and it does not exist in `load-path'.

You can use following PATH styles:
	load-path relative: \"PATH/\"
			(it is searched from `default-load-path')
	home directory relative: \"~/PATH/\" \"~USER/PATH/\"
	absolute path: \"/HOO/BAR/BAZ/\"

You can specify following OPTIONS:
	'all-paths	search from `load-path'
			instead of `default-load-path'
	'append		add PATH to the last of `load-path'

\(fn PATH &rest OPTIONS)" nil nil)

(autoload 'add-latest-path "apel/site-lisp/apel/path-util" "\
Add latest path matched by PATTERN to `load-path'
if it exists under `default-load-path' directories
and it does not exist in `load-path'.

If optional argument ALL-PATHS is specified, it is searched from all
of load-path instead of default-load-path.

\(fn PATTERN &optional ALL-PATHS)" nil nil)

(autoload 'get-latest-path "apel/site-lisp/apel/path-util" "\
Return latest directory in default-load-path
which is matched to regexp PATTERN.
If optional argument ALL-PATHS is specified,
it is searched from all of load-path instead of default-load-path.

\(fn PATTERN &optional ALL-PATHS)" nil nil)

(autoload 'file-installed-p "apel/site-lisp/apel/path-util" "\
Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `load-path' is used.

\(fn FILE &optional PATHS)" nil nil)

(defvar exec-suffix-list '("") "\
*List of suffixes for executable.")

(autoload 'exec-installed-p "apel/site-lisp/apel/path-util" "\
Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `exec-path' is used.
If suffixes is omitted, `exec-suffix-list' is used.

\(fn FILE &optional PATHS SUFFIXES)" nil nil)

(autoload 'module-installed-p "apel/site-lisp/apel/path-util" "\
Return t if module is provided or exists in PATHS.
If PATHS is omitted, `load-path' is used.

\(fn MODULE &optional PATHS)" nil nil)

;;;***

;;;### (autoloads (richtext-decode richtext-encode) "apel/site-lisp/emu/richtext"
;;;;;;  "apel/site-lisp/emu/richtext.el" (21930 33088 0 0))
;;; Generated autoloads from apel/site-lisp/emu/richtext.el

(autoload 'richtext-encode "apel/site-lisp/emu/richtext" "\


\(fn FROM TO)" nil nil)

(autoload 'richtext-decode "apel/site-lisp/emu/richtext" "\


\(fn FROM TO)" nil nil)

;;;***

;;;### (autoloads (global-auto-complete-mode auto-complete-mode auto-complete)
;;;;;;  "auto-complete/auto-complete" "auto-complete/auto-complete.el"
;;;;;;  (22159 60114 0 0))
;;; Generated autoloads from auto-complete/auto-complete.el

(autoload 'auto-complete "auto-complete/auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

(autoload 'auto-complete-mode "auto-complete/auto-complete" "\
AutoComplete mode

\(fn &optional ARG)" t nil)

(defvar global-auto-complete-mode nil "\
Non-nil if Global-Auto-Complete mode is enabled.
See the command `global-auto-complete-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-complete-mode'.")

(custom-autoload 'global-auto-complete-mode "auto-complete/auto-complete" nil)

(autoload 'global-auto-complete-mode "auto-complete/auto-complete" "\
Toggle Auto-Complete mode in all buffers.
With prefix ARG, enable Global-Auto-Complete mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Complete mode is enabled in all buffers where
`auto-complete-mode-maybe' would do it.
See `auto-complete-mode' for more information on Auto-Complete mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ac-config-default) "auto-complete/auto-complete-config"
;;;;;;  "auto-complete/auto-complete-config.el" (21846 5072 0 0))
;;; Generated autoloads from auto-complete/auto-complete-config.el

(autoload 'ac-config-default "auto-complete/auto-complete-config" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (auto-highlight-symbol-mode global-auto-highlight-symbol-mode)
;;;;;;  "auto-highlight-symbol/auto-highlight-symbol" "auto-highlight-symbol/auto-highlight-symbol.el"
;;;;;;  (21845 50907 0 0))
;;; Generated autoloads from auto-highlight-symbol/auto-highlight-symbol.el

(defvar global-auto-highlight-symbol-mode nil "\
Non-nil if Global-Auto-Highlight-Symbol mode is enabled.
See the command `global-auto-highlight-symbol-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-highlight-symbol-mode'.")

(custom-autoload 'global-auto-highlight-symbol-mode "auto-highlight-symbol/auto-highlight-symbol" nil)

(autoload 'global-auto-highlight-symbol-mode "auto-highlight-symbol/auto-highlight-symbol" "\
Toggle Auto-Highlight-Symbol mode in all buffers.
With prefix ARG, enable Global-Auto-Highlight-Symbol mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Highlight-Symbol mode is enabled in all buffers where
`ahs-mode-maybe' would do it.
See `auto-highlight-symbol-mode' for more information on Auto-Highlight-Symbol mode.

\(fn &optional ARG)" t nil)

(autoload 'auto-highlight-symbol-mode "auto-highlight-symbol/auto-highlight-symbol" "\
Toggle Auto Highlight Symbol Mode

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cperl-perldoc-at-point cperl-perldoc cperl-mode)
;;;;;;  "cperl-mode/cperl-mode" "cperl-mode/cperl-mode.el" (21880
;;;;;;  24623 0 0))
;;; Generated autoloads from cperl-mode/cperl-mode.el
(put 'cperl-indent-level 'safe-local-variable 'integerp)
(put 'cperl-brace-offset 'safe-local-variable 'integerp)
(put 'cperl-continued-brace-offset 'safe-local-variable 'integerp)
(put 'cperl-label-offset 'safe-local-variable 'integerp)
(put 'cperl-continued-statement-offset 'safe-local-variable 'integerp)
(put 'cperl-extra-newline-before-brace 'safe-local-variable 'booleanp)
(put 'cperl-merge-trailing-else 'safe-local-variable 'booleanp)

(autoload 'cperl-mode "cperl-mode/cperl-mode" "\
Major mode for editing Perl code.
Expression and list commands understand all C brackets.
Tab indents for Perl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Various characters in Perl almost always come in pairs: {}, (), [],
sometimes <>.  When the user types the first, she gets the second as
well, with optional special formatting done on {}.  (Disabled by
default.)  You can always quote (with \\[quoted-insert]) the left
\"paren\" to avoid the expansion.  The processing of < is special,
since most the time you mean \"less\".  CPerl mode tries to guess
whether you want to type pair <>, and inserts is if it
appropriate.  You can set `cperl-electric-parens-string' to the string that
contains the parenths from the above list you want to be electrical.
Electricity of parenths is controlled by `cperl-electric-parens'.
You may also set `cperl-electric-parens-mark' to have electric parens
look for active mark and \"embrace\" a region if possible.'

CPerl mode provides expansion of the Perl control constructs:

   if, else, elsif, unless, while, until, continue, do,
   for, foreach, formy and foreachmy.

and POD directives (Disabled by default, see `cperl-electric-keywords'.)

The user types the keyword immediately followed by a space, which
causes the construct to be expanded, and the point is positioned where
she is most likely to want to be.  eg. when the user types a space
following \"if\" the following appears in the buffer: if () { or if ()
} { } and the cursor is between the parentheses.  The user can then
type some boolean expression within the parens.  Having done that,
typing \\[cperl-linefeed] places you - appropriately indented - on a
new line between the braces (if you typed \\[cperl-linefeed] in a POD
directive line, then appropriate number of new lines is inserted).

If CPerl decides that you want to insert \"English\" style construct like

            bite if angry;

it will not do any expansion.  See also help on variable
`cperl-extra-newline-before-brace'.  (Note that one can switch the
help message on expansion by setting `cperl-message-electric-keyword'
to nil.)

\\[cperl-linefeed] is a convenience replacement for typing carriage
return.  It places you in the next line with proper indentation, or if
you type it inside the inline block of control construct, like

            foreach (@lines) {print; print}

and you are on a boundary of a statement inside braces, it will
transform the construct into a multiline and will place you into an
appropriately indented blank line.  If you need a usual
`newline-and-indent' behavior, it is on \\[newline-and-indent],
see documentation on `cperl-electric-linefeed'.

Use \\[cperl-invert-if-unless] to change a construction of the form

	    if (A) { B }

into

            B if A;

\\{cperl-mode-map}

Setting the variable `cperl-font-lock' to t switches on font-lock-mode
\(even with older Emacsen), `cperl-electric-lbrace-space' to t switches
on electric space between $ and {, `cperl-electric-parens-string' is
the string that contains parentheses that should be electric in CPerl
\(see also `cperl-electric-parens-mark' and `cperl-electric-parens'),
setting `cperl-electric-keywords' enables electric expansion of
control structures in CPerl.  `cperl-electric-linefeed' governs which
one of two linefeed behavior is preferable.  You can enable all these
options simultaneously (recommended mode of use) by setting
`cperl-hairy' to t.  In this case you can switch separate options off
by setting them to `null'.  Note that one may undo the extra
whitespace inserted by semis and braces in `auto-newline'-mode by
consequent \\[cperl-electric-backspace].

If your site has perl5 documentation in info format, you can use commands
\\[cperl-info-on-current-command] and \\[cperl-info-on-command] to access it.
These keys run commands `cperl-info-on-current-command' and
`cperl-info-on-command', which one is which is controlled by variable
`cperl-info-on-command-no-prompt' and `cperl-clobber-lisp-bindings'
\(in turn affected by `cperl-hairy').

Even if you have no info-format documentation, short one-liner-style
help is available on \\[cperl-get-help], and one can run perldoc or
man via menu.

It is possible to show this help automatically after some idle time.
This is regulated by variable `cperl-lazy-help-time'.  Default with
`cperl-hairy' (if the value of `cperl-lazy-help-time' is nil) is 5
secs idle time .  It is also possible to switch this on/off from the
menu, or via \\[cperl-toggle-autohelp].  Requires `run-with-idle-timer'.

Use \\[cperl-lineup] to vertically lineup some construction - put the
beginning of the region at the start of construction, and make region
span the needed amount of lines.

Variables `cperl-pod-here-scan', `cperl-pod-here-fontify',
`cperl-pod-face', `cperl-pod-head-face' control processing of POD and
here-docs sections.  With capable Emaxen results of scan are used
for indentation too, otherwise they are used for highlighting only.

Variables controlling indentation style:
 `cperl-tab-always-indent'
    Non-nil means TAB in CPerl mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `cperl-indent-left-aligned-comments'
    Non-nil means that the comment starting in leftmost column should indent.
 `cperl-auto-newline'
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in Perl code.  The following
    \\[cperl-electric-backspace] will remove the inserted whitespace.
    Insertion after colons requires both this variable and
    `cperl-auto-newline-after-colon' set.
 `cperl-auto-newline-after-colon'
    Non-nil means automatically newline even after colons.
    Subject to `cperl-auto-newline' setting.
 `cperl-indent-level'
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `cperl-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if, or body of a while, or just a statement continuation.
 `cperl-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `cperl-continued-statement-offset'.
 `cperl-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `cperl-brace-imaginary-offset'
    An open brace following other text is treated as if it the line started
    this far to the right of the actual line indentation.
 `cperl-label-offset'
    Extra indentation for line that is a label.
 `cperl-min-label-indent'
    Minimal indentation for line that is a label.

Settings for classic indent-styles: K&R BSD=C++ GNU PerlStyle=Whitesmith
  `cperl-indent-level'                5   4       2   4
  `cperl-brace-offset'                0   0       0   0
  `cperl-continued-brace-offset'     -5  -4       0   0
  `cperl-label-offset'               -5  -4      -2  -4
  `cperl-continued-statement-offset'  5   4       2   4

CPerl knows several indentation styles, and may bulk set the
corresponding variables.  Use \\[cperl-set-style] to do this.  Use
\\[cperl-set-style-back] to restore the memorized preexisting values
\(both available from menu).  See examples in `cperl-style-examples'.

Part of the indentation style is how different parts of if/elsif/else
statements are broken into lines; in CPerl, this is reflected on how
templates for these constructs are created (controlled by
`cperl-extra-newline-before-brace'), and how reflow-logic should treat \"continuation\" blocks of else/elsif/continue, controlled by the same variable,
and by `cperl-extra-newline-before-brace-multiline',
`cperl-merge-trailing-else', `cperl-indent-region-fix-constructs'.

If `cperl-indent-level' is 0, the statement after opening brace in
column 0 is indented on
`cperl-brace-offset'+`cperl-continued-statement-offset'.

Turning on CPerl mode calls the hooks in the variable `cperl-mode-hook'
with no args.

DO NOT FORGET to read micro-docs (available from `Perl' menu)
or as help on variables `cperl-tips', `cperl-problems',
`cperl-praise', `cperl-speed'.

\(fn)" t nil)

(autoload 'cperl-perldoc "cperl-mode/cperl-mode" "\
Run `perldoc' on WORD.

\(fn WORD)" t nil)

(autoload 'cperl-perldoc-at-point "cperl-mode/cperl-mode" "\
Run a `perldoc' on the word around point.

\(fn)" t nil)

;;;***

;;;### (autoloads (el-get el-get-self-checksum el-get-checksum el-get-make-recipes
;;;;;;  el-get-cd el-get-reinstall el-get-remove el-get-self-update
;;;;;;  el-get-update-packages-of-type el-get-update-all el-get-update
;;;;;;  el-get-install el-get-version) "el-get/el-get" "el-get/el-get.el"
;;;;;;  (21845 49129 0 0))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-install "el-get/el-get" "\
Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update "el-get/el-get" "\
Update PACKAGE.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-remove "el-get/el-get" "\
Remove any PACKAGE that is know to be installed or required.

\(fn PACKAGE)" t nil)

(autoload 'el-get-reinstall "el-get/el-get" "\
Remove PACKAGE and then install it again.

\(fn PACKAGE)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE)" t nil)

(autoload 'el-get-self-checksum "el-get/el-get" "\
Compute the checksum of the running version of el-get itself.

Also put the checksum in the kill-ring.

\(fn)" t nil)

(autoload 'el-get "el-get/el-get" "\
Ensure that packages have been downloaded once and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure that:

* the packages have been installed
* load-path is set so their elisp files can be found
* Info-directory-list is set so their info files can be found
* Autoloads have been prepared and evaluated for each package
* Any post-installation setup (e.g. `(require 'feature)') happens

When SYNC is nil (the default), all installations run
concurrently, in the background.

When SYNC is 'sync, each package will be installed synchronously,
and any error will stop it all.

Please note that the `el-get-init' part of `el-get' is always
done synchronously. There's `byte-compile' support though, and
the packages you use are welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered.

\(fn &optional SYNC &rest PACKAGES)" nil nil)

;;;***

;;;### (autoloads (el-get-bundle! el-get-bundle el-get-bundle-el-get)
;;;;;;  "el-get/el-get-bundle" "el-get/el-get-bundle.el" (21845 49129
;;;;;;  0 0))
;;; Generated autoloads from el-get/el-get-bundle.el

(autoload 'el-get-bundle-el-get "el-get/el-get-bundle" "\


\(fn SRC)" nil nil)

(autoload 'el-get-bundle "el-get/el-get-bundle" "\
Install PACKAGE and run initialization FORM.

PACKAGE can be either a simple package name or a package name
with a modifier before the name to specify local recipe source
information:

* `<owner>/' : specifies a Github owner name
* `gist:<id>' : specifies a Gist ID
* `<type>:' : specifies a type of the package source

If `FEATURE in PACKAGE' form is used instead of PACKAGE, then
that FEATURE is `require'd after installing PACKAGE.  You can
also use `el-get-bundle!' macro if FEATURE and PACKAGE are the
same.  If you wish to `require' more than one feature, then use
`:features' property in FORM.

The initialization FORM may start with a property list that
describes a local recipe.  The FORM after the property list is
treated as initialization code, which is actually an `:after'
property of the local recipe.

A copy of the initialization code is stored in a directory
specified by `el-get-bundle-init-directory' and its byte-compiled
version is used if `el-get-bundle-byte-compile' is non-nil.

\(fn PACKAGE &rest FORM)" nil t)

(put 'el-get-bundle 'lisp-indent-function 'defun)

(autoload 'el-get-bundle! "el-get/el-get-bundle" "\
Install PACKAGE and run initialization form.
It is the same as `el-get-bundle' except that PACKAGE is explicitly
required.

\(fn PACKAGE &rest ARGS)" nil t)

(put 'el-get-bundle! 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads (el-get-list-packages) "el-get/el-get-list-packages"
;;;;;;  "el-get/el-get-list-packages.el" (21845 49129 0 0))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads (flymake-cursor-mode) "flymake-cursor/flymake-cursor"
;;;;;;  "flymake-cursor/flymake-cursor.el" (21953 49946 0 0))
;;; Generated autoloads from flymake-cursor/flymake-cursor.el

(autoload 'flymake-cursor-mode "flymake-cursor/flymake-cursor" "\
Minor mode to show `flymake-mode' errors for the current line in the
message area.
When called interactively, toggles the minor mode.
With arg, turn Flymake Cursor mode on if and only if arg is positive.

Usually `flymake-cursor-mode' is enabled and disabled automatically with
`flymake-mode' for the current buffer and you will not need to toggle
the mode directly.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (flymake-shell-load) "flymake-shell/flymake-shell"
;;;;;;  "flymake-shell/flymake-shell.el" (22104 2749 0 0))
;;; Generated autoloads from flymake-shell/flymake-shell.el

(autoload 'flymake-shell-load "flymake-shell/flymake-shell" "\
Configure flymake mode to check the current buffer's shell-script syntax.

\(fn)" t nil)

;;;***

;;;### (autoloads (git-gutter:cancel-update-timer git-gutter:start-update-timer
;;;;;;  git-gutter:update-all-windows git-gutter:set-start-revision
;;;;;;  git-gutter:toggle git-gutter:clear git-gutter git-gutter:previous-hunk
;;;;;;  git-gutter:next-hunk git-gutter:popup-hunk git-gutter:stage-hunk
;;;;;;  git-gutter:revert-hunk global-git-gutter-mode git-gutter-mode
;;;;;;  git-gutter:linum-setup) "git-gutter/git-gutter" "git-gutter/git-gutter.el"
;;;;;;  (21907 33960 0 0))
;;; Generated autoloads from git-gutter/git-gutter.el

(autoload 'git-gutter:linum-setup "git-gutter/git-gutter" "\
Setup for linum-mode.

\(fn)" nil nil)

(autoload 'git-gutter-mode "git-gutter/git-gutter" "\
Git-Gutter mode

\(fn &optional ARG)" t nil)

(defvar global-git-gutter-mode nil "\
Non-nil if Global-Git-Gutter mode is enabled.
See the command `global-git-gutter-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.")

(custom-autoload 'global-git-gutter-mode "git-gutter/git-gutter" nil)

(autoload 'global-git-gutter-mode "git-gutter/git-gutter" "\
Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global-Git-Gutter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Git-Gutter mode is enabled in all buffers where
`git-gutter--turn-on' would do it.
See `git-gutter-mode' for more information on Git-Gutter mode.

\(fn &optional ARG)" t nil)

(autoload 'git-gutter:revert-hunk "git-gutter/git-gutter" "\
Revert current hunk.

\(fn)" t nil)

(autoload 'git-gutter:stage-hunk "git-gutter/git-gutter" "\
Stage this hunk like 'git add -p'.

\(fn)" t nil)

(autoload 'git-gutter:popup-hunk "git-gutter/git-gutter" "\
Popup current diff hunk.

\(fn &optional DIFFINFO)" t nil)

(autoload 'git-gutter:next-hunk "git-gutter/git-gutter" "\
Move to next diff hunk

\(fn ARG)" t nil)

(autoload 'git-gutter:previous-hunk "git-gutter/git-gutter" "\
Move to previous diff hunk

\(fn ARG)" t nil)

(autoload 'git-gutter "git-gutter/git-gutter" "\
Show diff information in gutter

\(fn)" t nil)

(autoload 'git-gutter:clear "git-gutter/git-gutter" "\
Clear diff information in gutter.

\(fn)" t nil)

(autoload 'git-gutter:toggle "git-gutter/git-gutter" "\
Toggle to show diff information.

\(fn)" t nil)

(autoload 'git-gutter:set-start-revision "git-gutter/git-gutter" "\
Set start revision. If `start-rev' is nil or empty string then reset
start revision.

\(fn START-REV)" t nil)

(autoload 'git-gutter:update-all-windows "git-gutter/git-gutter" "\
Update git-gutter informations for all visible buffers.

\(fn)" t nil)

(autoload 'git-gutter:start-update-timer "git-gutter/git-gutter" "\


\(fn)" t nil)

(autoload 'git-gutter:cancel-update-timer "git-gutter/git-gutter" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (highlight-symbol-occur highlight-symbol-query-replace
;;;;;;  highlight-symbol-nav-mode highlight-symbol-prev-in-defun
;;;;;;  highlight-symbol-next-in-defun highlight-symbol-prev highlight-symbol-next
;;;;;;  highlight-symbol-count highlight-symbol-list-all highlight-symbol-remove-all
;;;;;;  highlight-symbol highlight-symbol-mode) "highlight-symbol/highlight-symbol"
;;;;;;  "highlight-symbol/highlight-symbol.el" (21845 50937 0 0))
;;; Generated autoloads from highlight-symbol/highlight-symbol.el

(autoload 'highlight-symbol-mode "highlight-symbol/highlight-symbol" "\
Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'.

\(fn &optional ARG)" t nil)

(defalias 'highlight-symbol-at-point 'highlight-symbol)

(autoload 'highlight-symbol "highlight-symbol/highlight-symbol" "\
Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'.

\(fn &optional SYMBOL)" t nil)

(autoload 'highlight-symbol-remove-all "highlight-symbol/highlight-symbol" "\
Remove symbol highlighting in buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-list-all "highlight-symbol/highlight-symbol" "\
List all symbols highlighted in the buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-count "highlight-symbol/highlight-symbol" "\
Print the number of occurrences of symbol at point.

\(fn &optional SYMBOL)" t nil)

(autoload 'highlight-symbol-next "highlight-symbol/highlight-symbol" "\
Jump to the next location of the symbol at point within the buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-prev "highlight-symbol/highlight-symbol" "\
Jump to the previous location of the symbol at point within the buffer.

\(fn)" t nil)

(autoload 'highlight-symbol-next-in-defun "highlight-symbol/highlight-symbol" "\
Jump to the next location of the symbol at point within the defun.

\(fn)" t nil)

(autoload 'highlight-symbol-prev-in-defun "highlight-symbol/highlight-symbol" "\
Jump to the previous location of the symbol at point within the defun.

\(fn)" t nil)

(autoload 'highlight-symbol-nav-mode "highlight-symbol/highlight-symbol" "\
Navigate occurrences of the symbol at point.

When called interactively, toggle `highlight-symbol-nav-mode'.
With prefix ARG, enable `highlight-symbol-nav-mode' if ARG is
positive, otherwise disable it.

When called from Lisp, enable `highlight-symbol-nav-mode' if ARG
is omitted, nil or positive.  If ARG is `toggle', toggle
`highlight-symbol-nav-mode'.  Otherwise behave as if called
interactively.

In `highlight-symbol-nav-mode' provide the following key bindings
to navigate between occurrences of the symbol at point in the
current buffer.

\\{highlight-symbol-nav-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'highlight-symbol-query-replace "highlight-symbol/highlight-symbol" "\
Replace the symbol at point with REPLACEMENT.

\(fn REPLACEMENT)" t nil)

(autoload 'highlight-symbol-occur "highlight-symbol/highlight-symbol" "\
Call `occur' with the symbol at point.
Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.

\(fn &optional NLINES)" t nil)

;;;***

;;;### (autoloads (iedit-rectangle-mode iedit-mode-on-function iedit-mode)
;;;;;;  "iedit/iedit" "iedit/iedit.el" (21846 7178 0 0))
;;; Generated autoloads from iedit/iedit.el

(autoload 'iedit-mode "iedit/iedit" "\
Toggle Iedit mode.
This command behaves differently, depending on the mark, point,
prefix argument and variable `iedit-transient-mark-sensitive'.

If Iedit mode is off, turn Iedit mode on.

When Iedit mode is turned on, all the occurrences of the current
region are highlighted.  If one occurrence is modified, the
change are propagated to all other occurrences simultaneously.

If region is not active, the current symbol (returns from
`current-word') is used as the occurrence by default.  The
occurrences of the current symbol, but not include occurrences
that are part of other symbols, are highlighted.  If you still
want to match all the occurrences, even though they are parts of
other symbols, you may have to mark the symbol first.

In the above two situations, with digit prefix argument 0, only
occurrences in current function are matched.  This is good for
renaming refactoring in programming.

You can also switch to Iedit mode from isearch mode directly. The
current search string is used as occurrence.  All occurrences of
the current search string are highlighted.

With an universal prefix argument, the occurrence when Iedit mode
is turned off last time in current buffer is used as occurrence.
This is intended to recover last Iedit mode which is turned off.
If region active, Iedit mode is limited within the current
region.

With repeated universal prefix argument, the occurrence when
Iedit mode is turned off last time (might be in other buffer) is used
as occurrence.  If region active, Iedit mode is limited within
the current region.

If Iedit mode is on and region is active, Iedit mode is
restricted in the region, e.g. the occurrences outside of the region
is excluded.

If Iedit mode is on and region is active, with an universal
prefix argument, Iedit mode is restricted outside of the region,
e.g. the occurrences in the region is excluded.

Turn off Iedit mode in other situations.

Commands:
\\{iedit-current-keymap}

\(fn &optional ARG)" t nil)

(autoload 'iedit-mode-on-function "iedit/iedit" "\
Toggle Iedit mode on current function.

\(fn)" t nil)

(autoload 'iedit-rectangle-mode "iedit/iedit" "\
Toggle iedit-RECT mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (indent-guide-global-mode indent-guide-mode) "indent-guide/indent-guide"
;;;;;;  "indent-guide/indent-guide.el" (21930 38946 0 0))
;;; Generated autoloads from indent-guide/indent-guide.el

(autoload 'indent-guide-mode "indent-guide/indent-guide" "\
show vertical lines to guide indentation

\(fn &optional ARG)" t nil)

(defvar indent-guide-global-mode nil "\
Non-nil if Indent-Guide-Global mode is enabled.
See the command `indent-guide-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `indent-guide-global-mode'.")

(custom-autoload 'indent-guide-global-mode "indent-guide/indent-guide" nil)

(autoload 'indent-guide-global-mode "indent-guide/indent-guide" "\
Toggle Indent-Guide mode in all buffers.
With prefix ARG, enable Indent-Guide-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Indent-Guide mode is enabled in all buffers where
`(lambda nil (unless (cl-some (quote derived-mode-p) indent-guide-inhibit-modes) (indent-guide-mode 1)))' would do it.
See `indent-guide-mode' for more information on Indent-Guide mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (js2-imenu-extras-mode js2-imenu-extras-setup)
;;;;;;  "js2-mode/js2-imenu-extras" "js2-mode/js2-imenu-extras.el"
;;;;;;  (21876 28835 0 0))
;;; Generated autoloads from js2-mode/js2-imenu-extras.el

(autoload 'js2-imenu-extras-setup "js2-mode/js2-imenu-extras" "\


\(fn)" nil nil)

(autoload 'js2-imenu-extras-mode "js2-mode/js2-imenu-extras" "\
Toggle Imenu support for frameworks and structural patterns.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (js2-mode js2-minor-mode js2-highlight-unused-variables-mode)
;;;;;;  "js2-mode/js2-mode" "js2-mode/js2-mode.el" (21876 28835 0
;;;;;;  0))
;;; Generated autoloads from js2-mode/js2-mode.el

(autoload 'js2-highlight-unused-variables-mode "js2-mode/js2-mode" "\
Toggle highlight of unused variables.

\(fn &optional ARG)" t nil)

(autoload 'js2-minor-mode "js2-mode/js2-mode" "\
Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `js-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'.

\(fn &optional ARG)" t nil)

(autoload 'js2-mode "js2-mode/js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (let-alist) "let-alist/let-alist" "let-alist/let-alist.el"
;;;;;;  (21853 26395 0 0))
;;; Generated autoloads from let-alist/let-alist.el

(autoload 'let-alist "let-alist/let-alist" "\
Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq 'title alist)))
        (.body  (cdr (assq 'body alist)))
        (.site  (cdr (assq 'site alist)))
        (.site.contents (cdr (assq 'contents (cdr (assq 'site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one. You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

\(fn ALIST &rest BODY)" nil t)

(put 'let-alist 'lisp-indent-function '1)

;;;***

;;;### (autoloads (minimap-mode) "minimap/minimap" "minimap/minimap.el"
;;;;;;  (21931 16677 0 0))
;;; Generated autoloads from minimap/minimap.el

(defvar minimap-mode nil "\
Non-nil if Minimap mode is enabled.
See the command `minimap-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `minimap-mode'.")

(custom-autoload 'minimap-mode "minimap/minimap" nil)

(autoload 'minimap-mode "minimap/minimap" "\
Toggle minimap mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mode-compile-kill mode-compile mode-compile-submit-bug-report
;;;;;;  emacs-lisp-byte-compile-dir-interactive-p mode-compile-reading-time
;;;;;;  mode-compile-expert-p mode-compile-after-kill-hook mode-compile-before-kill-hook
;;;;;;  mode-compile-after-compile-hook mode-compile-before-compile-hook
;;;;;;  mode-compile-other-frame-p mode-compile-never-edit-command-p
;;;;;;  mode-compile-always-save-buffer-p mode-compile-save-all-p
;;;;;;  mode-compile-ignore-makerule-regexp mode-compile-prefered-default-makerule
;;;;;;  mode-compile-make-options mode-compile-ignore-makefile-backups
;;;;;;  mode-compile-make-program) "mode-compile/mode-compile" "mode-compile/mode-compile.el"
;;;;;;  (21880 24604 0 0))
;;; Generated autoloads from mode-compile/mode-compile.el

(defvar mode-compile-make-program "make" "\
*The `make' program used to process makefiles.

If you have GNU make installed with name \"gmake\" use it.")

(custom-autoload 'mode-compile-make-program "mode-compile/mode-compile" t)

(defvar mode-compile-ignore-makefile-backups t "\
*Tell mode compile to ignore makefiles backup files when selecting the Makefile to use.")

(custom-autoload 'mode-compile-ignore-makefile-backups "mode-compile/mode-compile" t)

(defvar mode-compile-default-make-options "-k" "\
Default options to give to `make'.")

(defvar mode-compile-make-options (eval mode-compile-default-make-options) "\
*Options to give to `make'.
This could be any form evaluating to a string.

Some people asked me a way to modify the make options everytime a
compilation command is launched, do that:
 (defun my-mode-compile-ask-make-options()
   \"*Hook called by mode-compile, asking for make options.\"
   (interactive)
   (read-string \"Make options: \"
                mode-compile-default-make-options))
 (setq mode-compile-make-options
           'my-mode-compile-ask-make-options)")

(custom-autoload 'mode-compile-make-options "mode-compile/mode-compile" t)

(defvar mode-compile-prefered-default-makerule 'none "\
*Default makerule you would like to see in minibuffer as a default choice
when selecting the make rule to build.

Possible values are:
'none    -- let mode-compile deciding for you.
'all     -- try hard to show you the \"all\" rule.
'default -- try hard to show you the \"default\" rule.
'file    -- try to show you the name of the file which will be
            result of compilation.
The 'none action is taken as default is something fail.")

(custom-autoload 'mode-compile-prefered-default-makerule "mode-compile/mode-compile" t)

(defvar mode-compile-ignore-makerule-regexp nil "\
*Makefile rules which must be ignored when building completion list.

For example if you want to remove all `files rules' set
it to: \"\\\\.\\\\([aoc]\\\\|s[ao][.0-9]*\\\\)\". ")

(custom-autoload 'mode-compile-ignore-makerule-regexp "mode-compile/mode-compile" t)

(defvar mode-compile-save-all-p nil "\
*Non-nil means save ALL the modified buffers without asking
before launching compilation command.")

(custom-autoload 'mode-compile-save-all-p "mode-compile/mode-compile" t)

(defvar mode-compile-always-save-buffer-p nil "\
*Non-nil means save the current buffer without asking
before launching compilation command.")

(custom-autoload 'mode-compile-always-save-buffer-p "mode-compile/mode-compile" t)

(defvar mode-compile-never-edit-command-p nil "\
*Non-nil means never ask to user to edit the compile command.")

(custom-autoload 'mode-compile-never-edit-command-p "mode-compile/mode-compile" t)

(defvar mode-compile-other-frame-p nil "\
*Non-nil means compile in another frame.

A new Emacs FRAME is created and the compilation command is executed
in this other frame.  To specify the frame parameters see also
variable `mode-compile-frame-parameters-alist'.")

(custom-autoload 'mode-compile-other-frame-p "mode-compile/mode-compile" t)

(defvar mode-compile-before-compile-hook nil "\
Hook to be run before compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-before-compile-hook "mode-compile/mode-compile" t)

(defvar mode-compile-after-compile-hook nil "\
Hook to be run after compile command is executed
when `mode-compile' is invoked.")

(custom-autoload 'mode-compile-after-compile-hook "mode-compile/mode-compile" t)

(defvar mode-compile-before-kill-hook nil "\
Hook to be run before killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-before-kill-hook "mode-compile/mode-compile" t)

(defvar mode-compile-after-kill-hook nil "\
Hook to be run after killing compile command is executed
when `mode-compile-kill' is invoked.")

(custom-autoload 'mode-compile-after-kill-hook "mode-compile/mode-compile" t)

(defvar mode-compile-choosen-compiler nil "\
*Global variable containing the name of the compiler
which will be used for compiling without makefile.

 Could be used in combination with
 (cc|c++|ada|f77)-default-compiler-options
to automaticaly choose the compiler specific options.

example:
 (defun my-compiler-get-options()
   (cond
    ((string= mode-compile-choosen-compiler \"gcc\")
      \"-Wall -pedantic-errors\")
    ((string= mode-compile-choosen-compiler \"cc\")
      \"cc options whatever they are...\")
    (t
     (message \"Don't know this compiler: %s\" mode-compile-choosen-compiler)
     (read-string
      (format \"Options for %s compiler: \" mode-compile-choosen-compiler)))))

  (setq cc-default-compiler-options 'my-compiler-get-options)")

(defvar mode-compile-expert-p nil "\
*Non nil means `mode-compile' will not speaks too much.

See also variable variable mode-compile-reading-time.")

(custom-autoload 'mode-compile-expert-p "mode-compile/mode-compile" t)

(defvar mode-compile-reading-time 1 "\
*Seconds to wait in verbose mode after printing a message.

In verbose mode mode-compile print too much messages that it is
allmost impossible to read them. Just setting this delay leave you the
time to read all the messages. If you don't want any delay set it to
`0'.

See also function sit-for.")

(custom-autoload 'mode-compile-reading-time "mode-compile/mode-compile" t)

(defvar emacs-lisp-byte-compile-dir-interactive-p t "\
*Non-nil means when byte-compiling a directory ask for each file
needing to be recompiled or not.")

(custom-autoload 'emacs-lisp-byte-compile-dir-interactive-p "mode-compile/mode-compile" t)

(defconst mode-compile-version "2.28" "\
Current version of mode-compile package.

mode-compile.el,v 2.28 2003/04/01 13:52:47 boubaker Exp
Please send bugs-fixes/contributions/comments to boubaker@cena.fr")

(autoload 'mode-compile-submit-bug-report "mode-compile/mode-compile" "\
*Submit via mail a bug report on mode-compile v2.27.

\(fn)" t nil)

(autoload 'mode-compile "mode-compile/mode-compile" "\
*Compile the file in the current buffer with a dynamically built command.

The command is built according to the current major mode the function
was invoked from.

Running this command preceded by universal-argument (\\[universal-argument])
allows remote compilation, the user is prompted for a host name to run the
compilation command on.

Currently know how to compile in:
 `c-mode' ,              -- function cc-compile.
 `java-mode' ,           -- function java-compile.
 `c++-mode',             -- function c++-compile.
 `ada-mode',             -- function ada-compile.
 `fortran-mode',         -- function f77-compile.
 `emacs-lisp-mode'       -- function elisp-compile.
 `lisp-interaction-mode' -- function elisp-compile.
 `makefile-mode'         -- function makefile-compile.
 `dired-mode'            -- function dired-compile.
 `sh-mode'               -- function sh-compile.
 `csh-mode'              -- function csh-compile.
 `zsh-mode'              -- function zsh-compile.
 `perl-mode'             -- function perl-compile.
 `cperl-mode'            -- function perl-compile.
 `tcl-mode'              -- function tcl-compile.
 `python-mode'           -- function python-compile.
 `ruby-mode'             -- function ruby-compile.
 `fundamental-mode'      -- function guess-compile.
 `text-mode'             -- function guess-compile.
 `indented-text-mode'    -- function guess-compile.
 `compilation-mode'      -- function default-compile.
 The function `guess-compile' is called when mode is unknown.

The variable `mode-compile-modes-alist' contain description of known
modes.  The hooks variables `mode-compile-before-compile-hook' and
`mode-compile-after-compile-hook' are run just before and after
invoking the compile command of the mode.

Use the command `mode-compile-kill' (\\[mode-compile-kill]) to abort a
running compilation.

Bound on \\[mode-compile].

\(fn &optional REMOTE-HOST)" t nil)

(autoload 'mode-compile-kill "mode-compile/mode-compile" "\
*Kill the running compilation launched by `mode-compile' (\\[mode-compile]) command.

The compilation command is killed according to the current major mode
the function was invoked from.

Currently know how to kill compilations from:
 `c-mode' ,              -- function kill-compilation.
 `java-mode' ,           -- function kill-compilation.
 `c++-mode' ,            -- function kill-compilation.
 `ada-mode' ,            -- function kill-compilation.
 `fortran-mode' ,        -- function kill-compilation.
 `emacs-lisp-mode'       -- function keyboard-quit.
 `lisp-interaction-mode' -- function keyboard-quit.
 `makefile-mode'         -- function kill-compilation.
 `dired-mode'            -- function kill-compilation.
 `sh-mode'               -- function kill-compilation.
 `csh-mode'              -- function kill-compilation.
 `zsh-mode'              -- function kill-compilation.
 `perl-mode'             -- function kill-compilation.
 `cperl-mode'            -- function kill-compilation.
 `tcl-mode'              -- function kill-compilation.
 `python-mode'           -- function kill-compilation.
 `ruby-mode'             -- function kill-compilation.
 `fundamental-mode'      -- Bound dynamically.
 `text-mode'             -- Bound dynamically.
 `indented-text-mode'    -- Bound dynamically.
 `compilation-mode'      -- function kill-compilation.

The variable `mode-compile-modes-alist' contain description of ALL
known modes.  The hooks variables `mode-compile-before-kill-hook' and
`mode-compile-after-kill-hook' are run just before and after invoking
the kill compile command of the mode.

Bound on \\[mode-compile-kill].

\(fn)" t nil)

;;;***

;;;### (autoloads (pkg-info-version-info pkg-info-package-version
;;;;;;  pkg-info-defining-library-version pkg-info-defining-library-original-version
;;;;;;  pkg-info-library-version pkg-info-library-original-version)
;;;;;;  "pkg-info/pkg-info" "pkg-info/pkg-info.el" (21853 26408 0
;;;;;;  0))
;;; Generated autoloads from pkg-info/pkg-info.el

(autoload 'pkg-info-library-original-version "pkg-info/pkg-info" "\
Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-library-version "pkg-info/pkg-info" "\
Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-original-version "pkg-info/pkg-info" "\
Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-version "pkg-info/pkg-info" "\
Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-package-version "pkg-info/pkg-info" "\
Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

\(fn PACKAGE &optional SHOW)" t nil)

(autoload 'pkg-info-version-info "pkg-info/pkg-info" "\
Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

\(fn LIBRARY &optional PACKAGE SHOW)" t nil)

;;;***

;;;### (autoloads (powerline-fill powerline-raw defpowerline powerline-concat
;;;;;;  powerline-mouse powerline-hud) "powerline/powerline" "powerline/powerline.el"
;;;;;;  (21930 26488 0 0))
;;; Generated autoloads from powerline/powerline.el

(autoload 'powerline-hud "powerline/powerline" "\
Return an XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH.

\(fn FACE1 FACE2 &optional WIDTH)" nil nil)

(autoload 'powerline-mouse "powerline/powerline" "\
Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING.

\(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil)

(autoload 'powerline-concat "powerline/powerline" "\
Concatonate STRINGS and pad sides by spaces.

\(fn &rest STRINGS)" nil nil)

(autoload 'defpowerline "powerline/powerline" "\
Create function NAME by wrapping BODY with powerline padding an propetization.

\(fn NAME BODY)" nil t)

(autoload 'powerline-raw "powerline/powerline" "\
Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r).

\(fn STR &optional FACE PAD)" nil nil)

(autoload 'powerline-fill "powerline/powerline" "\
Return empty space using FACE and leaving RESERVE space on the right.

\(fn FACE RESERVE)" nil nil)
 (autoload 'powerline-major-mode "powerline")
 (autoload 'powerline-minor-modes "powerline")
 (autoload 'powerline-narrow "powerline")
 (autoload 'powerline-vc "powerline")
 (autoload 'powerline-buffer-size "powerline")
 (autoload 'powerline-buffer-id "powerline")
 (autoload 'powerline-process "powerline")
 (autoload 'powerline-selected-window-active "powerline")

;;;***

;;;### (autoloads (powerline-nano-theme powerline-vim-theme powerline-center-theme
;;;;;;  powerline-default-theme) "powerline/powerline-themes" "powerline/powerline-themes.el"
;;;;;;  (21930 26488 0 0))
;;; Generated autoloads from powerline/powerline-themes.el

(autoload 'powerline-default-theme "powerline/powerline-themes" "\
Setup the default mode-line.

\(fn)" t nil)

(autoload 'powerline-center-theme "powerline/powerline-themes" "\
Setup a mode-line with major and minor modes centered.

\(fn)" t nil)

(autoload 'powerline-vim-theme "powerline/powerline-themes" "\
Setup a Vim-like mode-line.

\(fn)" t nil)

(autoload 'powerline-nano-theme "powerline/powerline-themes" "\
Setup a nano-like mode-line.

\(fn)" t nil)

;;;***

;;;### (autoloads (smart-newline-mode smart-newline) "smart-newline/smart-newline"
;;;;;;  "smart-newline/smart-newline.el" (21930 28989 0 0))
;;; Generated autoloads from smart-newline/smart-newline.el

(autoload 'smart-newline "smart-newline/smart-newline" "\
smart-newline is a newline command which designed for programmer.

\(fn)" t nil)

(autoload 'smart-newline-mode "smart-newline/smart-newline" "\
smart-newline-mode is a minor-mode for using smart-newline command by default key-map.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (tabbar-ruler-move tabbar-ruler-backward tabbar-ruler-forward
;;;;;;  tabbar-ruler-up tabbar-install-faces) "tabbar-ruler/tabbar-ruler"
;;;;;;  "tabbar-ruler/tabbar-ruler.el" (21930 34355 0 0))
;;; Generated autoloads from tabbar-ruler/tabbar-ruler.el

(autoload 'tabbar-install-faces "tabbar-ruler/tabbar-ruler" "\
Installs faces for a frame.

\(fn &optional FRAME)" t nil)

(autoload 'tabbar-ruler-up "tabbar-ruler/tabbar-ruler" "\
Tabbar press up key.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-ruler-forward "tabbar-ruler/tabbar-ruler" "\
Forward ruler. Takes into consideration if the home-key was pressed.
This is based on the variable `tabbar--buffer-show-groups'

\(fn &optional ARG)" t nil)

(autoload 'tabbar-ruler-backward "tabbar-ruler/tabbar-ruler" "\
Backward ruler.  Takes into consideration if the home-key was pressed.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-ruler-move "tabbar-ruler/tabbar-ruler" "\
Start the movement for the tabbar

\(fn)" t nil)

;;;***

;;;### (autoloads (revive-save-desktop revive-desktop resume save-current-configuration
;;;;;;  wipe restore-window-configuration current-window-configuration-printable)
;;;;;;  "tabbar/revive" "tabbar/revive.el" (21845 51687 0 0))
;;; Generated autoloads from tabbar/revive.el

(autoload 'current-window-configuration-printable "tabbar/revive" "\
Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of revive:all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start) tabs).

\(fn)" nil nil)

(autoload 'restore-window-configuration "tabbar/revive" "\
Restore the window configuration.
Configuration CONFIG should be created by
current-window-configuration-printable.

\(fn CONFIG)" nil nil)

(autoload 'wipe "tabbar/revive" "\
Wipe Emacs.

\(fn)" t nil)

(autoload 'save-current-configuration "tabbar/revive" "\
Save current window/buffer configuration into configuration file.

\(fn &optional NUM)" t nil)

(autoload 'resume "tabbar/revive" "\
Resume window/buffer configuration.
Configuration should be saved by save-current-configuration.

\(fn &optional NUM)" t nil)

(autoload 'revive-desktop "tabbar/revive" "\
Restores the application state.
Similar to `resume', though using `desktop' to restore buffers.

\(fn &optional FILE AUTO)" t nil)

(autoload 'revive-save-desktop "tabbar/revive" "\
Save application state with `desktop' and `revive'.
Similar to `save-current-configuration', 
though uses `desktop' to restore buffers.

\(fn &optional FILE AUTO)" t nil)

;;;***

;;;### (autoloads (tabbar-mwheel-mode tabbar-mode tabbar-local-mode
;;;;;;  tabbar-mwheel-switch-group tabbar-mwheel-switch-tab tabbar-mwheel-forward-tab
;;;;;;  tabbar-mwheel-backward-tab tabbar-mwheel-forward-group tabbar-mwheel-backward-group
;;;;;;  tabbar-mwheel-forward tabbar-mwheel-backward tabbar-press-scroll-right
;;;;;;  tabbar-press-scroll-left tabbar-press-home tabbar-forward-tab
;;;;;;  tabbar-backward-tab tabbar-forward-group tabbar-backward-group
;;;;;;  tabbar-forward tabbar-backward) "tabbar/tabbar" "tabbar/tabbar.el"
;;;;;;  (21845 51687 0 0))
;;; Generated autoloads from tabbar/tabbar.el

(autoload 'tabbar-backward "tabbar/tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycle-scope'.

\(fn)" t nil)

(autoload 'tabbar-forward "tabbar/tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycle-scope'.

\(fn)" t nil)

(autoload 'tabbar-backward-group "tabbar/tabbar" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload 'tabbar-forward-group "tabbar/tabbar" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload 'tabbar-backward-tab "tabbar/tabbar" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload 'tabbar-forward-tab "tabbar/tabbar" "\
Select the next visible tab.

\(fn)" t nil)

(autoload 'tabbar-press-home "tabbar/tabbar" "\
Press the tab bar home button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-press-scroll-left "tabbar/tabbar" "\
Press the tab bar scroll-left button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-press-scroll-right "tabbar/tabbar" "\
Press the tab bar scroll-right button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click.

\(fn &optional ARG)" t nil)

(autoload 'tabbar-mwheel-backward "tabbar/tabbar" "\
Select the previous available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-forward "tabbar/tabbar" "\
Select the next available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-backward-group "tabbar/tabbar" "\
Go to selected tab in the previous available group.
If there is only one group, select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-group'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-forward-group "tabbar/tabbar" "\
Go to selected tab in the next available group.
If there is only one group, select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-group'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-backward-tab "tabbar/tabbar" "\
Select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-tab'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-forward-tab "tabbar/tabbar" "\
Select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-tab'.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-switch-tab "tabbar/tabbar" "\
Select the next or previous tab according to EVENT.

\(fn EVENT)" t nil)

(autoload 'tabbar-mwheel-switch-group "tabbar/tabbar" "\
Select the next or previous group of tabs according to EVENT.

\(fn EVENT)" t nil)

(autoload 'tabbar-local-mode "tabbar/tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Tabbar mode is off.

\(fn &optional ARG)" t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mode'.")

(custom-autoload 'tabbar-mode "tabbar/tabbar" nil)

(autoload 'tabbar-mode "tabbar/tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mode-map}

\(fn &optional ARG)" t nil)

(defvar tabbar-mwheel-mode nil "\
Non-nil if Tabbar-Mwheel mode is enabled.
See the command `tabbar-mwheel-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mwheel-mode'.")

(custom-autoload 'tabbar-mwheel-mode "tabbar/tabbar" nil)

(autoload 'tabbar-mwheel-mode "tabbar/tabbar" "\
Toggle use of the mouse wheel to navigate through tabs or groups.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mwheel-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "tern/emacs/tern" "tern/emacs/tern.el" (22183
;;;;;;  29559 0 0))
;;; Generated autoloads from tern/emacs/tern.el

(autoload 'tern-use-server "tern/emacs/tern" "\


\(fn PORT SERVER)" t nil)

(autoload 'tern-mode "tern/emacs/tern" "\
Minor mode binding to the Tern JavaScript analyzer

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "tern/emacs/tern-auto-complete" "tern/emacs/tern-auto-complete.el"
;;;;;;  (22183 29559 0 0))
;;; Generated autoloads from tern/emacs/tern-auto-complete.el

(autoload 'tern-ac-setup "tern/emacs/tern-auto-complete" "\
Setup auto-complete for tern-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "vline/vline" "vline/vline.el" (22185 58128
;;;;;;  0 0))
;;; Generated autoloads from vline/vline.el

(autoload 'vline-mode "vline/vline" "\
Display vertical line mode.

\(fn &optional ARG)" t nil)

(defvar vline-global-mode nil "\
Non-nil if Vline-Global mode is enabled.
See the command `vline-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vline-global-mode'.")

(custom-autoload 'vline-global-mode "vline/vline" nil)

(autoload 'vline-global-mode "vline/vline" "\
Toggle Vline mode in all buffers.
With prefix ARG, enable Vline-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Vline mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (vline-mode 1)))' would do it.
See `vline-mode' for more information on Vline mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (web-mode) "web-mode/web-mode" "web-mode/web-mode.el"
;;;;;;  (21876 30557 0 0))
;;; Generated autoloads from web-mode/web-mode.el

(autoload 'web-mode "web-mode/web-mode" "\
Major mode for editing web templates.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "yasnippet/yasnippet" "yasnippet/yasnippet.el"
;;;;;;  (22187 24372 0 0))
;;; Generated autoloads from yasnippet/yasnippet.el

(autoload 'yas-minor-mode "yasnippet/yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

\(fn &optional ARG)" t nil)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.
See the command `yas-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet/yasnippet" nil)

(autoload 'yas-global-mode "yasnippet/yasnippet" "\
Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("apel/site-lisp/apel/calist.el" "apel/site-lisp/apel/filename.el"
;;;;;;  "apel/site-lisp/apel/install.el" "apel/site-lisp/emu/apel-ver.el"
;;;;;;  "apel/site-lisp/emu/broken.el" "apel/site-lisp/emu/emu.el"
;;;;;;  "apel/site-lisp/emu/inv-23.el" "apel/site-lisp/emu/invisible.el"
;;;;;;  "apel/site-lisp/emu/mcharset.el" "apel/site-lisp/emu/mcs-20.el"
;;;;;;  "apel/site-lisp/emu/mcs-e20.el" "apel/site-lisp/emu/mule-caesar.el"
;;;;;;  "apel/site-lisp/emu/pccl-20.el" "apel/site-lisp/emu/pccl.el"
;;;;;;  "apel/site-lisp/emu/pces-20.el" "apel/site-lisp/emu/pces-e20.el"
;;;;;;  "apel/site-lisp/emu/pces.el" "apel/site-lisp/emu/pcustom.el"
;;;;;;  "apel/site-lisp/emu/poe.el" "apel/site-lisp/emu/poem-e20.el"
;;;;;;  "apel/site-lisp/emu/poem-e20_3.el" "apel/site-lisp/emu/poem.el"
;;;;;;  "apel/site-lisp/emu/product.el" "apel/site-lisp/emu/pym.el"
;;;;;;  "apel/site-lisp/emu/static.el" "auto-complete-c-headers/auto-complete-c-headers.el"
;;;;;;  "auto-complete/auto-complete-pkg.el" "auto-highlight-symbol/auto-highlight-symbol-config.el"
;;;;;;  "dash/dash-functional.el" "dash/dash.el" "el-get/el-get-autoloading.el"
;;;;;;  "el-get/el-get-build.el" "el-get/el-get-byte-compile.el"
;;;;;;  "el-get/el-get-core.el" "el-get/el-get-custom.el" "el-get/el-get-dependencies.el"
;;;;;;  "el-get/el-get-install.el" "el-get/el-get-methods.el" "el-get/el-get-notify.el"
;;;;;;  "el-get/el-get-recipes.el" "el-get/el-get-status.el" "epl/epl.el"
;;;;;;  "flymake-easy/flymake-easy.el" "fuzzy/fuzzy.el" "minimap/minimap-autoloads.el"
;;;;;;  "minimap/minimap-pkg.el" "perl-completion/perl-completion.el"
;;;;;;  "popup/popup.el" "powerline/powerline-separators.el" "tabbar/aquamacs-compat.el"
;;;;;;  "tabbar/aquamacs-tabbar.el" "tabbar/aquamacs-tools.el" "tabbar/one-buffer-one-frame.el"
;;;;;;  "tabbar/tabbar-window.el" "yasnippet-config/yasnippet-config.el"
;;;;;;  "yasnippet/yasnippet-debug.el" "yasnippet/yasnippet-tests.el")
;;;;;;  (22187 24943 917867 0))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
