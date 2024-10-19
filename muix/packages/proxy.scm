;;; Copyright © 2024 Merrick Luo <merrick@luois.me>
;;;
;;; This file is NOT part of GNU Guix.
;;;

(define-module (muix packages proxy)
  #:use-module (muix build source)
  #:use-module (guix build-system go)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages compression)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public v2ray
  (package
    (name "v2ray")
    (version "5.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/v2fly/v2ray-core")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "1w9d0f3syxw58ji7c6rb74m6qgnz05hjcnplz59rfpimqn1cb0zm"))))
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
     `(("go-mod" ,(go-mod-vendor-source name version "1hxhpxhmhaa6vr5xqqgy3kairw0km3zabkbxpry9yms0c364g941"))))
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

(define-public simple-obfs
  (let ((commit "486bebd9208539058e57e23a12f23103016e09b4")
        (revision "1"))
    (package
      (name "simple-obfs")
      (version (git-version "0.0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/shadowsocks/simple-obfs")
               (commit commit)
               (recursive? #t)))
         (sha256 (base32 "1mwpy06b00zr5z9wh66vlvg06yadms84kzkjylzs7njmrxwm29bv"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake libtool zlib openssl asciidoc xmlto libev libxml2 docbook-xsl))
      (home-page "https://github.com/shadowsocks/simple-obfs")
      (synopsis "A simple obfuscating tool (Deprecated)")
      (description "Simple-obfs is a simple obfuscating tool, designed as plugin server of shadowsocks.
Deprecated. Followed by v2ray-plugin.")
      (license license:gpl3))))

;; TODO: add dependencies
(define-public rust-build-time-0.1
  (package
    (name "rust-build-time")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "build-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10nk1lqb5mlmwxv0300pylf9bg7mmwhb8s3r9gbvzdr9zhcrq8gi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/AlephAlpha/build-time")
    (synopsis "Simple proc-macros to generate build timestamp string literals.")
    (description synopsis)
    (license license:expat)))

;; TODO: add dependencies
(define-public rust-json5-0.4
  (package
    (name "rust-json5")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "json5" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h9hni897zmn3vcixfbwwkj2gkz27h7z9dah8bk1qv37mwhxpc4n"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/callum-oakley/json5-rs")
    (synopsis "A Rust JSON5 serializer and deserializer which speaks Serde.")
    (description synopsis)
    (license license:isc)))

;; TODO: add dependencies
(define-public rust-log4rs-1.2
  (package
    (name "rust-log4rs")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "log4rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z9kfnba38smyrpmq49pjl82yqbvj2h81m3878cvhycydmwa2v6k"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/estk/log4rs")
    (synopsis "log4rs is a highly configurable logging framework modeled after Java’s Logback and log4j libraries.")
    (description synopsis)
    (license license:isc)))

(define-public rust-base64-0.22
  (package
    (inherit rust-base64-0.21)
    (name "rust-base64")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lbsphibsb0lnyswqr8mjqw6am7zh780yh62ldbbwl8lxipqcxcl"))))))

(define-public rust-clap-4.5
  (package
    (inherit rust-clap-4)
    (name "rust-clap")
    (version "4.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ql4kc5nclygivr0711lzid3z3g26jf1ip3qda9zxhaldn2c3b34"))))))

(define-public rust-ipnet-2.9
  (package
    (inherit rust-ipnet-2)
    (name "rust-ipnet")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ipnet" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hzrcysgwf0knf83ahb3535hrkw63mil88iqc6kjaryfblrqylcg"))))))

(define-public shadowsocks-rust
  (package
    (name "shadowsocks-rust")
    (version "1.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "shadowsocks-rust" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256 (base32 "1ckllmjdqpah9s9pqgbdv5z683sn7d9p91kp1z228fm0scafldas"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:features (list "logging" "hickory-dns" "local" "server" "multi-threaded")
       #:cargo-build-flags '("--no-default-features" "--release")
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-log4rs" ,rust-log4rs-1.2)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-json5" ,rust-json5-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-clap" ,rust-clap-4.5)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ;; ("rust-qrcode" ,rust-qrcode-0.14)
                       ;; ("rust-sysexits" ,rust-sysexits-0.8)
                       ("rust-build-time" ,rust-build-time-0.1)
                       ("rust-directories" ,rust-directories-5)
                       ("rust-xdg" ,rust-xdg-2)
                       ("rust-rpassword" ,rust-rpassword-7)
                       ("rust-libc" ,rust-libc-0.2) ;;, features = ["extra_traits"] }
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-tokio" ,rust-tokio-1) ;;, features = ["rt", "signal"] }
                       ("rust-ipnet" ,rust-ipnet-2.9)
                       ;;("rust-mimalloc" ,rust-mimalloc-rust-0.1)
                       ;;("rust-tcmalloc"  ,rust-tcmalloc-0.3)
                       ("rust-jemallocator"  ,rust-jemallocator-0.5)
                       ;;("rust-snmalloc-rs"  ,rust-snmalloc-rs-0.3)
                       ;;("rust-rpmalloc"  ,rust-rpmalloc-0.2)
                       ;;("rust-shadowsocks-service"  ,rust-shadowsocks-service-0.2)
                       ;;("rust-windows-service"  ,rust-windows-service-0.7)
                       )))
    (home-page "https://github.com/shadowsocks/shadowsocks-rust")
    (synopsis "A Rust port of shadowsocks, this package can't be build right now.")
    (description "Shadowsocks is a fast tunnel proxy that helps you bypass firewalls.")
    (license license:expat)))
