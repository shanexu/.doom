;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Shane Xu"
      user-mail-address "xusheng0711@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 12.0 :weight 'regular)
      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 16.0 :weight 'regular))

(defun my-set-cjk-font (font font-size)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family font :size font-size))))

(defun my-set-normal-cjk-font ()
  (my-set-cjk-font "Hiragino Sans GB" 14.2))

(defun my-set-big-cjk-font ()
  (my-set-cjk-font "Hiragino Sans GB" 19.0))

(defun my-set-font ()
  (if (display-graphic-p)
      (if doom-big-font-mode
          (my-set-big-cjk-font)
        (my-set-normal-cjk-font))))

(defun my-set-font-delayed ()
  (run-at-time "1 sec" nil #'my-set-font))

(add-hook 'doom-big-font-mode-hook 'my-set-font-delayed)

(remove-hook 'after-setting-font-hook #'doom-init-all-the-icons-fonts-h)

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-feather-dark)
;; (setq doom-theme 'doom-solarized-dark)
;; (setq doom-theme 'kaolin-dark)
;; (setq doom-theme 'doom-xcode)
;; (setq doom-theme 'doom-solarized-dark-high-contrast)
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! doom-themes
  :config
  (setq doom-dark+-blue-modeline t
        doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  )

(after! solaire-mode
  :config (add-to-list 'solaire-mode-themes-to-face-swap "^doom-one$")
  :config (add-to-list 'solaire-mode-themes-to-face-swap "^doom-xcode$")
  ;; :config (add-to-list 'solaire-mode-themes-to-face-swap "^doom-feather-dark")
  ;; :config (add-to-list 'solaire-mode-themes-to-face-swap "^doom-solarized-dark")
  )

(after! org-protocol
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("org-find-file" :protocol "find-file" :function org-protocol-find-file :kill-client nil))

  (defun org-protocol-find-file-fix-path (path)
    "If inside WSL, change Windows-style paths to WSL-style paths."
    (if (string-prefix-p "/" path)
        path
      (concat "~/OneDrive/logseq/" path)))

  (defun org-protocol-find-file (fname)
    "Process org-protocol://find-file?path= style URL."
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file (org-protocol-find-file-fix-path f))
      (raise-frame)
      (select-frame-set-input-focus (selected-frame))))
  )

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))

(defun load-dark-theme ()
  (consult-theme 'doom-one)
  (my-set-font-delayed))

(defun load-light-theme ()
  (consult-theme 'doom-one-light)
  (my-set-font-delayed))

(use-package! auto-dark
  :config
  (setq auto-dark-dark-mode-hook 'load-dark-theme)
  (setq auto-dark-light-mode-hook 'load-light-theme)
  (auto-dark-mode t))

(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "ormolu"))

(with-eval-after-load 'ox-latex
 ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
 ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
 ;; automatically to resolve the cross-references.
 (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
 (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted")))
