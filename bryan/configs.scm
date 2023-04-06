(define-module (bryan configs)
  ;; #:use-module (abcdw feature-lists)
  #:use-module (bryan systems atlas)
  ;; #:use-module (abcdw hosts live)

  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features security-token)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features xdg)
  #:use-module (rde features password-utils)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:use-module (rde features networking)
  #:use-module (rde features clojure)
  #:use-module (contrib features javascript)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (rde home services i2p)
  #:use-module (rde home services emacs)
  #:use-module (rde home services wm)

  #:use-module (gnu home-services ssh)

  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (rde packages aspell) ; needed for strings->packages

  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (ice-9 match))

;;; Service extensions

(define emacs-extra-packages-service
  (simple-service
   'emacs-extra-packages
   home-emacs-service-type
   (home-emacs-extension
    (init-el
     `((with-eval-after-load 'org
         (setq org-use-speed-commands t)
         (define-key org-mode-map (kbd "M-o")
           (lambda ()
             (interactive)
             (org-end-of-meta-data t))))
       (with-eval-after-load 'simple
         (setq-default display-fill-column-indicator-column 80)
         (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))
       (setq copyright-names-regexp
             (format "%s <%s>" user-full-name user-mail-address))
       (add-hook 'after-save-hook (lambda () (copyright-update nil nil)))))
    (elisp-packages
     (append
      (strings->packages
       ;; "emacs-dirvish"
       "emacs-rainbow-mode"
       "emacs-hl-todo"
       "emacs-yasnippet"
       ;; "emacs-company"
       "emacs-consult-dir"
       ;; "emacs-all-the-icons-completion" "emacs-all-the-icons-dired"
       "emacs-kind-icon"
       "emacs-nginx-mode" "emacs-yaml-mode"
       ;; "emacs-lispy"
       "emacs-ytdl"
       "emacs-multitran"
       "emacs-minimap"
       "emacs-ement"
       "emacs-restart-emacs"
       "emacs-org-present"))))))

(define home-extra-packages-service
  (simple-service
   'home-profile-extra-packages
   home-profile-service-type
   (append
    (strings->packages
     "figlet" ;; TODO: Move to emacs-artist-mode
     "calibre"
     "icecat" "nyxt"
     "ungoogled-chromium-wayland" "ublock-origin-chromium"

     "utox" "qtox" "jami"

     "alsa-utils" "yt-dlp" "cozy"
     "pavucontrol" "wev"
     "imagemagick"
     "obs" "obs-wlrobs"
     "recutils" "binutils" "make"
     "fheroes2"

     "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
     "papirus-icon-theme" "arc-theme"
     "thunar" "fd"
     ;; "glib:bin"

     "libreoffice"
     "ffmpeg"
     "ripgrep" "curl"))))


(define %abcdw-features
  (list
   (feature-user-info
    #:user-name "bryan"
    #:full-name "Bryan Paronto"
    #:email "bryan@cableca.digital"
    #:user-initial-password-hash
    "$6$abc$3SAZZQGdvQgAscM2gupP1tC.SqnsaLSPoAnEOb2k6jXMhzQqS1kCSplAJ/vUy2rrnpHtt6frW2Ap5l/tIvDsz."
    ;; (crypt "bob" "$6$abc")

    ;; WARNING: This option can reduce the explorability by hiding
    ;; some helpful messages and parts of the interface for the sake
    ;; of minimalistic, less distractive and clean look.  Generally
    ;; it's not recommended to use it.
    #:emacs-advanced-user? #t)


   (feature-custom-services
    #:feature-name-prefix 'abcdw
    #:home-services
    (list
     emacs-extra-packages-service
     home-extra-packages-service))


   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (music "$HOME/music")
     (videos "$HOME/vids")
     (pictures "$HOME/pics")
     (documents "$HOME/docs")
     (download "$HOME/dl")
     (desktop "$HOME")
     (publicshare "$HOME")
     (templates "$HOME")))



   (feature-emacs-keycast #:turn-on? #t)

   (feature-emacs-tempel
    #:default-templates? #t
    #:templates
    `(fundamental-mode
      ,#~""
      (t (format-time-string "%Y-%m-%d"))))
   (feature-emacs-time)
   (feature-emacs-spelling
    #:spelling-program (@ (gnu packages hunspell) hunspell)
    #:spelling-dictionaries
    (list
     (@ (gnu packages hunspell) hunspell-dict-en)
     (@ (rde packages aspell) hunspell-dict-ru)))
   (feature-emacs-git
    #:project-directory "~/work")
   (feature-emacs-org
    #:org-directory "~/work/private"
    #:org-indent? #f
    #:org-capture-templates
    ;; https://libreddit.tiekoetter.com/r/orgmode/comments/gc76l3/org_capture_inside_notmuch/
    `(("r" "Reply" entry (file+headline "" "Tasks")
       "* TODO Reply %:subject %?\nSCHEDULED: %t\n%U\n%a\n"
       :immediate-finish t)
      ("t" "Todo" entry (file+headline "" "Tasks") ;; org-default-notes-file
       "* TODO %?\nSCHEDULED: %t\n%a\n" :clock-in t :clock-resume t)))
   (feature-emacs-org-roam
    ;; TODO: Rewrite to states
    #:org-roam-directory "~/work/notes/notes")
   (feature-emacs-org-agenda
    #:org-agenda-files '("~/work/private/todo.org"
                         "~/work/rde/TODO"))
   (feature-emacs-elfeed
    #:elfeed-org-files '("~/work/private/rss.org"))

   (feature-javascript)


   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "us"
     #:options '("ctrl:nocaps")))))

;;; atlas

(define-public atlas-config
  (rde-config
   (features
    (append
     %all-features
     %atlas-features
     %bryan-features))))

(define-public atlas-os
  (rde-config-operating-system atlas-config))

(define-public atlas-he
  (rde-config-home-environment atlas-config))

;;; Dispatcher, which helps to return various values based on environment
;;; variable value.

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("home" atlas-he)
      ("system" atlas-os)
      (_ atlas-he))))

(dispatcher)
