(define-module (muix services v2ray)
  #:use-module (muix packages proxy)
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
  (package v2ray-configuration-package ;file-like
           (default v2ray))
  (arguments v2ray-configuration-arguments ;list of strings
             (default '()))
  (config-file v2ray-configuration-config-file ;json config file
               (default '())))

(define v2ray-shepherd-service
  (match-record-lambda <v2ray-configuration>
                       (package arguments config-file)
                       (list
                        (shepherd-service
                         (provision '(v2ray))
                         (documentation "Run v2ray.")
                         (requirement '(networking))
                         (start #~(make-forkexec-constructor
                                   (append (list (string-append #$package "/bin/v2ray")
                                                 "run"
                                                 "-config"
                                                 '#$config-file)
                                           '#$arguments)
                                   #:log-file "/var/log/v2ray.log"
                                   #:environment-variables
                                   (list "XDG_DATA_DIRS=/run/current-system/profile/share/")))
                         (respawn? #f)
                         (stop #~(make-kill-destructor))))))

(define v2ray-service-type
  (service-type (name 'v2ray)
                (extensions (list (service-extension shepherd-root-service-type
                                                     v2ray-shepherd-service)
                                  (service-extension profile-service-type ;; get the geodata to profile
                                                     (compose list v2ray-configuration-package))))
                (description "Run v2ray.")))
