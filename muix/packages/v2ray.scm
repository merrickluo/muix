;;; Copyright © 2024 Merrick Luo <merrick@luois.me>
;;;
;;; This file is NOT part of GNU Guix.
;;;

(define-module (muix packages v2ray)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
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

;; TODO replace this in house
(define* (go-mod-vendor-source package version hash)
  (let ((pv (string-append package "-" version)))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/Puqns67/gentoo-deps/releases/download/" pv "/" pv "-vendor.tar.xz"))
      (sha256 (base32 hash)))))

(define* (gh-release-origin repo name version hash)
  ;; v2fly/domain-list-community
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/" repo "/releases/download/" version "/" name))
    (sha256 (base32 hash))))

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
            (lambda* (#:key name inputs outputs import-path #:allow-other-keys)
              (setenv "CGO_ENABLED" "0") ;; disable cgo
              (let* ((out (assoc-ref outputs "out"))
                     (binpath (string-append out "/bin/v2ray"))
                     (main (string-append import-path "/main")))
                ;; build v2ray binary
                (invoke "go" "build" "-v" "-x" "-o" binpath "-trimpath" main)))))))
    (native-inputs `(("tar" ,tar)))
    (inputs
     `(("go-mod" ,(go-mod-vendor-source name version "14s918l8cjzdxs2rlg207f7165wz5vbr0cf8ydcnip5pq20wb62k"))))
    (propagated-inputs (list v2ray-geoip-bin v2ray-geosite-bin))
    (home-page "https://github.com/v2fly/v2ray-core")
    (synopsis "A platform for building proxies to bypass network restrictions.")
    (description "Project V is a set of network tools that helps you to build
your own computer network. It secures your network connections and
thus protects your privacy.")
    (license license:expat)))

(define-public v2ray-geoip-bin
  (package
    (name "v2ray-geoip-bin")
    (version "202407110044")
    (source (gh-release-origin
             "v2fly/geoip"
             "geoip.dat"
             version
             "1dppq7ra4cf7cs2s3kxgd7qipfyyq22iyv3jzv5jz4zz9y5d7j43"))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("geoip.dat" "share/v2ray/"))))
    (home-page "https://github.com/v2fly/geoip")
    (synopsis "GeoIP for V2Ray.")
    (description "Provide GeoIP for V2Ray to use in routing rules.")
    (license license:expat)))

(define-public v2ray-geosite-bin
  (package
    (name "v2ray-geosite-bin")
    (version "20240710044910")
    (source (gh-release-origin
             "v2fly/domain-list-community"
             "dlc.dat"
             version
             "0sjy3s1sgh3yf9xvy5k4bjqnmph3cwiidgpp70wjljmxpfl9mbax"))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("dlc.dat" "share/v2ray/geosite.dat"))))
    (home-page "https://github.com/v2fly/domain-list-community")
    (synopsis "Generated geosite.dat for V2Ray.")
    (description "This project manages a list of domains, to be used as geosites for routing purpose in Project V.")
    (license license:expat)))
