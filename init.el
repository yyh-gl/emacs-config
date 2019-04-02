;;
;; init.el
;;

;; /.emacs.d/elpa 配下の.elファイルを読み込む
;; >> 現状は rcodetools.el のため
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; CUI Window Settings
(setq default-frame-alist
      (append (list '(foreground-color . "white") ;azure3
					'(background-color . "black")
					'(border-color . "black")
					'(mouse-color . "white")
					'(cursor-color . "white")
					'(width . 181)
					'(height . 50)
					'(alpha . (80 60 40 40))
					)
			  default-frame-alist))

;; GUI Settings
(if window-system (progn
		    (when (equal system-type 'darwin)
              ;; Window Size
              (set-frame-size (selected-frame) 178 70)
              ;; 文字色
              (set-face-foreground 'default "white")
              )))

;; LANG Japan
(set-language-environment 'Japanese)
(set-keyboard-coding-system 'sjis-mac)
(setq default-buffer-file-coding-system 'euc-jp-unix)
(set-clipboard-coding-system 'sjis-mac)

;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Package Manegement
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; フォント（いらないっぽい）
;(add-to-list 'default-frame-alist '(font . "Ricty Diminished Discord"))

;; 行数の表示と色
(global-linum-mode t)
(set-face-attribute 'linum nil
					:foreground "#ECEFF1"
					:height 0.5)
(setq linum-format "%4d | ")

;; 括弧の範囲内を強調表示
(show-paren-mode t)
; (setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; 括弧の範囲色
;;(set-face-background 'show-paren-match-face "#6e7b8b")

;; 矩形選択
(cua-mode t)

;; C-cやC-vの乗っ取りを阻止
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; 自動補完機能
(require 'auto-complete)
(require 'auto-complete-config)    ; 必須ではないですが一応
(global-auto-complete-mode t)
(setq ac-delay 0)
(setq ac-auto-show-menu 0.05)

;; undoの回数を増やす
(setq undo-limit 100000)
(setq undo-strong-limit 130000)

;; undo-tree
(global-undo-tree-mode t)
;; C-x u : undo-tree-visualize
;; x : 現在地
;; p : 一つ上の変更をたどる
;; n : 一つ下の変更をたどる
;; f，b : 枝の切り替え
;; q : 終了

;; 拡張子と関連付け
;; (add-to-list 'auto-mode-alist '("\\.nc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ma\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))
(add-to-list 'auto-mode-alist '("zprofile" . sh-mode))
(setq load-path (cons (expand-file-name "~/.emacs.d/nesc") load-path))
(autoload 'nesc-mode "nesc.el")
(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))
;; scala mode
(require 'scala-mode)

;; ;; Completion words longer than 4 characters
;; (custom-set-variables
;;   '(ac-ispell-requires 4))
;; (eval-after-load "auto-complete"
;;   '(progn
;;       (ac-ispell-setup)))
;; (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'text-mode-hook 'ac-ispell-ac-setup)

(setq inhibit-startup-message t)

;; ;; フルスクリーン
;; (set-frame-parameter nil 'fullscreen 'fullboth)

;; ;;----- MacでGUIの時、optionをmeta
;; (if window-system (progn
;; 		    (when (equal system-type 'darwin)
;; 		      (setq mac-option-modifier 'meta))
;; 		    ))

;; git-gutter+
;; (global-git-gutter+-mode t)

;; git-gutter-frieng
(global-git-gutter-mode t)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; オートセーブファイルを作らない
(setq auto-save-default nil)

;; コピペ OS同期
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; 括弧補完
(require 'smartparens-config)
(smartparens-global-mode t)

;; Flycheck
(require 'flycheck)
(global-flycheck-mode)
(define-key global-map (kbd "\C-cn") 'flycheck-next-error)
(define-key global-map (kbd "\C-cp") 'flycheck-previous-error)
(define-key global-map (kbd "\C-cd") 'flycheck-list-errors)

;; hlinum
(require 'hlinum)

;; メタキーをコマンドキーに割り当て
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; YaTex
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
				("\\.ltx$" . yatex-mode)
				("\\.sty$" . yatex-mode)) auto-mode-alist))
;; set YaTeX coding system
(setq YaTeX-kanji-code 4) ; UTF-8 の設定
(add-hook 'yatex-mode-hook
		  '(lambda ()
			 (setq YaTeX-use-AMS-LaTeX t) ; align で数式モードになる
			 (setq YaTeX-use-hilit19 nil
				   YateX-use-font-lock t)
			 (setq tex-command "em-latexmk.sh") ; typeset command
			 (setq dvi2-command "evince") ; preview command
			 (setq tex-pdfview-command "xdg-open"))) ; preview command

;; バックスペースを「C-h」に割り当て
(keyboard-translate ?\C-h ?\C-?)

;; emacs上でコンパイル
(require 'smart-compile)
(define-key global-map (kbd "C-c c") 'smart-compile)
(define-key global-map (kbd "C-c C-c") (kbd "C-c c C-m"))

;; カーソル移動のもっさり解消
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)

;; 現在行のハイライト
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")
										;(setq hl-line-face 'hlline-face)
(setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)

;; タブ幅設定
(setq-default tab-width 4 indent-tabs-mode nil)

;; バッファー名の問い合わせで大文字小文字の区別をしない
(setq read-buffer-completion-ignore-case t)

;; ファイル名の問い合わせで大文字小文字の区別をしない
(setq read-file-name-completion-ignore-case t)

;; タイムスタンプの挿入
(define-key global-map [f5]
  '(lambda ()
     (interactive)
     (insert (format-time-string "%Y/%m/%d %H:%M:%S"))))

;; Rails開発モード
(require 'projectile)
(require 'projectile-rails)
(projectile-rails-global-mode)

;; Web系タグ補完
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;(setq web-mode-engines-alist
;;	  '(("php"    . "\\.phtml\\'")
;;		("blade"  . "\\.blade\\.")))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-enable-auto-closing t)
(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style 1)
                                        ;(setq web-mode-enable-auto-pairing t)
;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (c-set-offset 'case-label' 4)
            (c-set-offset 'arglist-intro' 4)
            (c-set-offset 'arglist-cont-nonempty' 4)
            (c-set-offset 'arglist-close' 0)))

;; json-mode
(when (require 'json-mode)
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

;; yaml-mode
(when (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;; markdown-mode
(when (require 'markdown-mode nil t))

;; TeX利用時の句読点自動変換
(defun replace-dot-comma ()
  (let ((curpos (point)))
    (goto-char (point-min))
    (while (search-forward "。" nil t) (replace-match "．"))

    (goto-char (point-min))
    (while (search-forward "、" nil t) (replace-match "，"))
    (goto-char curpos)
    ))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'replace-dot-comma nil 'make-it-local)
             ))

;; Ruby開発時のデバッグをコメント化
;; rcodetools gem 必要
(require 'rcodetools)
(define-key ruby-mode-map (kbd "M-p") 'xmp)

;; yasnippet
;; 定型文挿入
(require 'yasnippet)
(yas-global-mode 1)

;; 前方一行削除
;; カーソル位置から行頭まで削除する
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
;; C-uに設定
(global-set-key (kbd "C-u") 'backward-kill-line)

;; sudo権限でファイルを開き直すコマンド
(defun sudo ()
  "Reopen current buffer-file with sudo using tramp."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (find-alternate-file (concat "/sudo::" file-name))
      (error "Cannot get a file name"))))

;; I-searchを拡張
;; (global-set-key "\C-s" 'swiper)
;; (defvar swiper-include-line-number-in-search t)
;; (setq enable-recursive-minibuffers t)


;; -*-*-*-*-*-追加予定-*-*-*-*-*-
;; ・バックタブ
;; ・タブ関連
;; ・定型文（プログラミング支援系のやつ）
;; -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

;; emacsさんが勝手につけてくれたやつっぽい Thanks

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode yasnippet yaml-mode json-mode swiper go-mode php-mode web-mode yatex undo-tree smartparens smart-compile scala-mode projectile-rails hlinum git-gutter-fringe git-gutter+ flycheck auto-complete 0blayout))))
