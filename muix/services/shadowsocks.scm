(define-module (muix services shadowsocks)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages golang-crypto)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (shadowsocks-configuration
            shadowsocks-configuration?
            shadowsocks-go2-service-type))

(define-record-type* <shadowsocks-configuration>
  shadowsocks-configuration make-shadowsocks-configuration
  shadowsocks-configuration?
  (shadowsocks shadowsocks-configuration-shadowsocks ; package + binary path
               (default go-github-com-shadowsocks-go-shadowsocks2))
  (server-address shadowsocks-configuration-server-address
                  (default "0.0.0.0:1080"))
  (local-address shadowsocks-configuration-local-address
                 (default "0.0.0.0:1080"))
  (method shadowsocks-configuration-method
          (default "AEAD_CHACHA20_POLY1305"))
  (password shadowsocks-configuration-password)
  (plugin shadowsocks-configuration-plugin
          (default ""))
  (plugin-opts shadowsocks-configuration-plugin-opts
               (default ""))
  (verbose shadowsocks-configuration-verbose
           (default #t))
  )

(define shadowsocks-go2-shepherd-service
  (match-record-lambda <shadowsocks-configuration>
      (shadowsocks server-address local-address method password plugin plugin-opts verbose)
    (list
     (shepherd-service
      (provision '(shadowsocks))
      (documentation "Run shadowsocks.")
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (append (list (string-append #$shadowsocks "/bin/go-shadowsocks2")
                              "-s" #$server-address
                              "-c" #$local-address
                              "-plugin" #$plugin
                              "-plugin-opts" #$plugin-opts
                              "-cipher" #$method
                              (if #$verbose "-verbose")
                              "-password" #$password))
                #:log-file "/var/log/shadowsocks.log"))
      (respawn? #f)
      (stop #~(make-kill-destructor))))))

(define shadowsocks-go2-service-type
  (service-type (name 'shadowsocks)
                (extensions (list (service-extension shepherd-root-service-type
                                                     shadowsocks-go2-shepherd-service)))
                (description "Run shadowsocks-go2, client or server depends on the config.")))
