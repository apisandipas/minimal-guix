(define-module (bryan systems atlas)
  #:use-module (bryan systems base)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "atlas")

 (swap-devices
  (list (swap-space
         (target
          (file-system-label "swap-partition")))))

 (file-systems (cons*
                (file-system
                 (device (file-system-label "system-root"))
                 (mount-point "/")
                 (type "ext4")
                 (dependencies mapped-devices))
                (file-system
                 (device (file-system-label "system-home"))
                 (mount-point "/home")
                 (type "ext4")
                 (dependencies mapped-devices))
                (file-system
                 (device (file-system-label "EFI_PART"))
                 (mount-point "/boot/efi")
                 (type "vfat"))
                %base-file-systems)))
