(define-module (bryan configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features bittorrent)
  #:use-module (rde features clojure)
  #:use-module (rde features docker)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features finance)
  #:use-module (rde features gnupg)
  ;; #:use-module (rde features image-viewers)
  #:use-module (rde features irc)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features mail)
  #:use-module (rde features password-utils)
  #:use-module (rde features markup)
  #:use-module (rde features mail)
  #:use-module (rde features networking)
  #:use-module (rde features security-token)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features system)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features version-control)
  #:use-module (rde features video)
  #:use-module (rde features virtualization)
  #:use-module (rde features web-browsers)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (contrib features javascript)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu home services xdg)
  #:use-module (rde home services i2p)
  #:use-module (rde home services emacs)
  #:use-module (rde home services wm)

  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (rde packages aspell) ; needed for strings->packages

  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix download)

  #:use-module (guix gexp)
  #:use-module (bryan feature-lists)
  #:use-module (bryan systems atlas)
  #:use-module (ice-9 match))

;;; Service extensions

(define-public %base-features
  (list
   ;; TODO: merge them into feature-base
   (feature-base-services)
   (feature-base-packages)
   (feature-desktop-services)

   (feature-pipewire)
   (feature-backlight #:step 10)
   (feature-networking)

   (feature-irc-settings
    #:irc-accounts (list
                    (irc-account
                     (id 'srht)
                     (network "chat.sr.ht")
                     (bouncer? #t)
                     (nick "cablecardigital"))
                    (irc-account
                     (id 'libera)
                     (network "irc.libera.chat")
                     (nick "cablecardigital"))
                    (irc-account
                     (id 'oftc)
                     (network "irc.oftc.net")
                     (nick "cablecardigital"))))

   (feature-transmission #:auto-start? #f)
   (feature-ungoogled-chromium
    #:default-browser? #t)
   (feature-ledger)
   ;; (feature-imv)
   (feature-mpv
    #:extra-mpv-conf '((speed . 1.61)))))

(define-public %mail-features
  (list
   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)))

(define-public %dev-features
  (list
   (feature-markdown)))

(define-public %virtualization-features
  (list
   (feature-docker)
   (feature-qemu)))

(define-public %cli-features
  (list
   (feature-alacritty
    ;; TODO: Rename to alacritty-yml
    #:config-file (local-file "./config/alacritty/alacritty.yml")
    #:default-terminal? #f
    #:backup-terminal? #t
    #:software-rendering? #f)
   (feature-vterm)
   (feature-tmux
    #:tmux-conf (local-file "./config/tmux/tmux.conf"))
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   ;; (feature-git)
   (feature-ssh)))

(define-public %ui-features
  (list
   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 11 #:weight 'regular)
    ;; #:font-monospace (font "Fira Mono" #:size 14 #:weight 'semi-light)
    ;; #:font-packages (list font-fira-mono)
    #:default-font-size 11)

   ;; https://sr.ht/~tsdh/swayr/
   ;; https://github.com/ErikReider/SwayNotificationCenter
   ;; https://github.com/swaywm/sway/wiki/i3-Migration-Guide

   ;; https://github.com/natpen/awesome-wayland
   (feature-sway)
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   ;; (feature-sway-statusbar
   ;;  #:use-global-fonts? #f)
   (feature-waybar)
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '((screenshots)
                     (effect-blur . 7x5)
                     (clock)))))

(define-public %emacs-features
  (list
   (feature-emacs
    #:default-application-launcher? #t)

   (feature-emacs-appearance)
   (feature-emacs-faces)
   (feature-emacs-modus-themes)

   (feature-emacs-completion
    #:mini-frame? #f
    #:marginalia-align 'right)
   (feature-emacs-corfu
    #:corfu-doc-auto #f)
   (feature-emacs-vertico)

   (feature-emacs-tramp)
   (feature-emacs-project)
   (feature-compile)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)

   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-log? #t
    #:erc-autojoin-channels-alist '((Libera.Chat "#rde")))
   (feature-emacs-telega)
   (feature-emacs-elpher)

   (feature-emacs-pdf-tools)
   (feature-emacs-nov-el)
   (feature-emacs-org-protocol)
   ;; TODO: Remove auctex dependency, which interjects in texinfo-mode.
   ;; (feature-emacs-citar)

   (feature-emacs-smartparens
    #:show-smartparens? #t)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-eglot)))

(define-public %general-features
  (append
   %base-features
   %dev-features
   %cli-features
   %ui-features
   %emacs-features))

(define-public %all-features
  (append
   %base-features
   %dev-features
   %virtualization-features
   %mail-features
   %cli-features
   %ui-features
   %emacs-features))

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
     "git"
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


(define %bryan-features
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
    #:feature-name-prefix 'bryan
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
     ;; (list
     ;;  (feature-base-services)
     ;;  (feature-base-packages)
     ;;  (feature-desktop-services)
     ;;  (feature-networking)
     ;;  )
      ;; %all-features
     %general-features
     ;; %base-features
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
