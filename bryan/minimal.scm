(define-module (bryan minimal)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features shells))


;;; Code:

(define minimal-rde-config
  (rde-config
   (features
    (list
     (feature-user-info
      #:user-name "bryan"
      #:full-name "Bryan Paronto"
      #:email "bryan@cablecar.digital"
      #:emacs-advanced-user? #t)
     (feature-zsh)))))

(rde-config-home-environment minimal-rde-config)
