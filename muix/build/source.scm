(define-module (muix build source)
  #:use-module (guix packages)
  #:use-module (guix download))

(define-public (go-mod-vendor-source package version hash)
  (let ((pv (string-append package "-v" version)))
    (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/merrickluo/go-deps/releases/download/" pv "/" pv "-vendor.tar.xz"))
      (sha256 (base32 hash)))))

(define-public (gh-release-origin repo name version hash)
  ;; v2fly/domain-list-community
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/" repo "/releases/download/" version "/" name))
    (sha256 (base32 hash))))
