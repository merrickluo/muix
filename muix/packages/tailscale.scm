(define-module (muix packages tailscale)
  #:use-module (muix source)
  #:use-module (gnu packages base)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages golang)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (semver)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.70.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lh2scy2gfgm9qcc3iml711jdx3bdpx4bfvjsj3p60xgjrlg67xc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "tailscale.com"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-go-mod
            (lambda* (#:key inputs import-path #:allow-other-keys)
              (invoke "tar" "--strip-component=1" "-xvf"
                      (assoc-ref inputs "go-mod") "-C"
                      (string-append "src/" import-path))
              #t))
          (delete 'check)
          (replace 'build
            (lambda* (#:key name version inputs outputs import-path #:allow-other-keys)
              ;; https://github.com/tailscale/tailscale/blob/main/build_dist.sh
              (let ((ver version)
                    (ldflags (string-append "-X tailscale.com/version.longStamp="
                                            "1.70.0-d601f16e1"
                                            " -X tailscale.com/version.shortStamp=" "1.70.0")))
                (setenv "GO111MODULE" "on") ;; tailscale need go mod to compile
                (chdir (string-append "src/" import-path))
                (invoke "go" "install" "-a" "-x" (string-append "./cmd/tailscale"))
                (invoke "go" "install" "-a" "-x" (string-append "./cmd/tailscaled"))
                (chdir "../.."))))
          )))
    (native-inputs `(("tar" ,tar)))
    (inputs
     `(("go-mod" ,(go-mod-vendor-source name version "0bvvzv6kp19pnj1a2d9mamg562q8rjb9l631jpfw5n7c0ymb7lx5"))))
    (propagated-inputs
     (list iptables))
    (synopsis "Tailscale connects your team's devices and development environments for easy access to remote resources.")
    (description
     "Tailscale is a zero config VPN for building secure networks. Install on any device in minutes. Remote access from any network or physical location.")
    (home-page "https://tailscale.com/")
    (license license:bsd-3)))
