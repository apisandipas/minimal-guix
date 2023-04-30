;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.
(define-module (bryan bare-bones)
  #:use-module (gnu)
  #:use-module (gnu system nss)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages suckless)
  #:use-module (gnu services desktop)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)
  #:use-module (bryan systems atlas))

(use-service-modules networking ssh)
(use-package-modules screen ssh)

(operating-system
 (host-name "atlas")
 (timezone "America/Chicago")
 (locale "en_US.utf8")

 ;; Use non-free Linux and firmware
 (kernel linux)
 (firmware (list linux-firmware))
 (initrd microcode-initrd)

 (keyboard-layout (keyboard-layout "us" "ctrl:no_caps"))

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets '("/boot/efi"))))

 (swap-devices atlas-swap-devices)
 (file-systems (append atlas-file-systems
                       %base-file-systems))

 ;; This is where user accounts are specified.  The "root"
 ;; account is implicit, and is initially created with the
 ;; empty password.
 (users (cons (user-account
               (name "bryan")
               (comment "It's me")
               (group "users")
               (password (crypt "bryan" "$6$abc"))
               ;; Adding the account to the "wheel" group
               ;; makes it a sudoer.  Adding it to "audio"
               ;; and "video" allows the user to play sound
               ;; and access the webcam.
               (supplementary-groups '("wheel"
                                       "audio" "video")))
              %base-user-accounts))

 ;; Globally-installed packages.
 (packages (append
            (list screen
                  make
                  nss-certs
                  vim
                  emacs
                  picom
                  xterm
                  i3-wm i3status dmenu)
            %base-packages))

 ;; Add services to the baseline: a DHCP client and
 ;; an SSH server.
 (services (append (list (service xfce-desktop-service-type))
                   %desktop-services
                   %base-services)))
