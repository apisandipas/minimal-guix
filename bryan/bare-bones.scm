(define-module (bryan bare-bones)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages suckless)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
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
                  arandr
                  ;; make
                  nss-certs
                  vim
                  emacs
                  picom
                  xterm
                  sway waybar
                  i3-wm i3status dmenu)
            %base-packages))

 ;; Add services to the baseline: a DHCP client and
 ;; an SSH server.
 (services (append
            (list (service xfce-desktop-service-type)
                  (service sddm-service-type))
            (remove (lambda (service)
                      (eq? (service-kink service) gdm-service-type)
                      %desktop-services))

           )))
