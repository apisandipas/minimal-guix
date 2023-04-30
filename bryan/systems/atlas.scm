(define-module (bryan systems atlas)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (ice-9 match))

(define atlas-swap-devices
  (list (swap-space
         (target
          (file-system-label "swap-partition")))))

(define atlas-file-systems
  (list
   (file-system
    (mount-point "/")
    (device (file-system-label "system-root"))
    (type "ext4"))
   (file-system
    (mount-point "/home")
    (device (file-system-label "system-home"))
    (type "ext4"))
   (file-system
    (mount-point "/boot/efi")
    (device (file-system-label "EFI_PART"))
    (type "vfat"))))

(define-public %atlas-features
  (list
   (feature-host-info
    #:host-name "atlas"
    #:timezone  "America/Chicago")
   (feature-file-systems
    ;; #:mapped-devices atlas-mapped-devices
    #:swap-devices atlas-swap-devices
    #:file-systems atlas-file-systems)))
