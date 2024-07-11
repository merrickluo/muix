(define-module (muix services v2ray)
  #:use-module (muix packages v2ray)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (v2ray-configuration
            v2ray-configuration?
            v2ray-service-type))

(define-record-type* <v2ray-configuration>
  v2ray-configuration make-v2ray-configuration
  v2ray-configuration?
  (v2ray v2ray-configuration-v2ray ;file-like
         (default v2ray))
  (arguments v2ray-configuration-arguments ;list of strings
             (default '()))
  (config-file v2ray-configuration-config-file ;json config file
               (default '())))

(define v2ray-shepherd-service
  (match-record-lambda <v2ray-configuration>
      (v2ray arguments config-file)
    (list
     (shepherd-service
      (provision '(v2ray))
      (documentation "Run v2ray.")
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (append (list (string-append #$v2ray "/bin/v2ray")
                              "run"
                              "-config"
                              '#$config-file)
                        '#$arguments)))
      (respawn? #f)
      (stop #~(make-kill-destructor))))))

(define v2ray-service-type
  (service-type (name 'v2ray)
                (extensions (list (service-extension shepherd-root-service-type
                                                     v2ray-shepherd-service)))
                (description
                 "Run v2ray.")))
