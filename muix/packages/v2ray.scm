;;; Copyright © 2024 Merrick Luo <merrick@luois.me>
;;;
;;; This file is NOT part of GNU Guix.
;;;

(define-module (muix packages v2ray)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:prefix license:))

(define* (go-mod-vendor-source package version)
  (let ((pv (string-append package "-" version)))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/Puqns67/gentoo-deps/releases/download/" pv "/" pv "-vendor.tar.xz"))
      (sha256 (base32 "14s918l8cjzdxs2rlg207f7165wz5vbr0cf8ydcnip5pq20wb62k")))))

(define-public v2ray
  (package
    (name "v2ray")
    (version "5.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/v2fly/v2ray-core")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "0jipxrdmcxlrjq0v593m0ci3iyxq5pxk0lszqsnmwzpflanx603f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/v2fly/v2ray-core"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-go-mod
            (lambda* (#:key inputs import-path #:allow-other-keys)
              (invoke "tar" "--strip-component=1" "-xvf"
                      (assoc-ref inputs "go-mod") "-C"
                      (string-append "src/" import-path))
              #t))
          (replace 'build
            (lambda* (#:key outputs import-path #:allow-other-keys)
              (setenv "CGO_ENABLED" "0") ;; disable cgo
              (let ((out (string-append (assoc-ref outputs "out") "/bin/v2ray"))
                    (main (string-append import-path "/main")))
                (invoke "go" "build" "-v" "-x" "-o" out "-trimpath" main)))))))
    (native-inputs
     `(("tar" ,tar)
       ("go-mod" ,(go-mod-vendor-source name version))))
    (home-page "https://github.com/v2fly/v2ray-core")
    (synopsis "A platform for building proxies to bypass network restrictions.")
    (description "Project V is a set of network tools that helps you to build
your own computer network. It secures your network connections and
thus protects your privacy.")
    (license license:expat)))
