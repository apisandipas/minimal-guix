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
            (device (file-system-label "system-root"))
            (mount-point "/")
            (type "ext4"))
        ;; (file-system
        ;;     (device (file-system-label "system-home"))
        ;;     (mount-point "/home")
        ;;     (type "ext4"))
        (file-system
            (device (file-system-label "EFI_PART"))
            (mount-point "/boot/efi")
            (type "vfat"))))

(define-public %atlas-features
  (list
   (feature-host-info
    #:host-name "atlas"
    #:timezone  "America/Chicago")
   (feature-file-systems
    ;; #:mapped-devices atlas-mapped-devices
    ;; #:swap-devices atlas-swap-devices
    #:file-systems atlas-file-systems)))
