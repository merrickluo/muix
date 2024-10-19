(define-module (muix services tailscale)
  #:use-module (muix packages vpn)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (gnu packages linux)
  #:export (tailscaled-service-type tailscaled-configuration))

(define-record-type* <tailscaled-configuration>
  tailscaled-configuration make-tailscaled-configuration
  tailscaled-configuration?
  (tailscale tailscaled-configuration-tailscale
			 (default tailscale))
  (listen-port tailscaled-configuration-listen-port
			   (default "41641"))
  (state-file tailscaled-configuration-state-file
			  (default "tailscaled.state")))

(define (tailscaled-activation config)
  "Run tailscaled --cleanup"
  #~(begin
      (system* #$(file-append tailscale "/bin/tailscaled") "--cleanup")))

(define (tailscaled-shepherd-service config)
  "Return a <shepherd-service> for Tailscaled with CONFIG"
  (let ((tailscale
	     (tailscaled-configuration-tailscale config))
	    (listen-port
	     (tailscaled-configuration-listen-port config))
	    (state-file
	     (tailscaled-configuration-state-file config))
	    (environment #~(list (string-append
			                  "PATH=" ; iptables is required for tailscale to work
			                  (string-append #$iptables "/sbin")
			                  ":"
			                  (string-append #$iptables "/bin")))))
    (list
	 (shepherd-service
      (provision '(tailscaled))
	  (requirement '(networking)) ;; services this depends on
      (start #~(make-forkexec-constructor
		        (list #$(file-append tailscale "/bin/tailscaled")
 		              "-state" #$state-file
                      "-port" #$listen-port)
		        #:environment-variables #$environment
		        #:log-file "/var/log/tailscaled.log"))
      (stop #~(make-kill-destructor))))))

(define tailscaled-service-type
  (service-type
   (name 'tailscaled)
   (extensions
    (list (service-extension shepherd-root-service-type
			                 tailscaled-shepherd-service)
          (service-extension activation-service-type
			                 tailscaled-activation)
          (service-extension profile-service-type
                             (compose list tailscaled-configuration-tailscale))))
   (default-value (tailscaled-configuration))
   (description "Launch tailscaled.")))
