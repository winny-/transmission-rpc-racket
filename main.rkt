#lang racket

(require net/http-easy
         (for-syntax syntax/parse racket/base racket/syntax)
         syntax/parse
         racket/syntax)

(provide (all-defined-out))

(define *endpoint* (make-parameter "http://localhost:9091/transmission/rpc"))
(define *session-id* (make-parameter #f))
(define X-TRANSMISSION-SESSION-ID 'X-Transmission-Session-Id)

(define (call method [arguments (hasheq)])
  (unless (*session-id*)
    (*session-id*
     (match (head (*endpoint*))
       [(response #:headers ([X-Transmission-Session-Id session-id]))
        (bytes->string/utf-8 session-id)]
       [bad
        (raise-user-error "Did not get a X-Transmission-Session-Id back from server.")])))
  (match (post (*endpoint*) #:headers (hasheq X-TRANSMISSION-SESSION-ID (*session-id*))
               #:json (hasheq 'method (~a method)
                              'arguments arguments))
    [(response #:json (hash-table ['result "success"] [arguments ret-args]))
     ret-args]
    [(response #:json (hash-table pats ...))
     (raise-user-error "Got weird json response " pats)]))


(begin-for-syntax
  (define (identifier->keyword-syntax stx id)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e id)))))
  (define-syntax-class argument
    (pattern name:id
             #:attr kwarg (identifier->keyword-syntax this-syntax #'name)
             #:attr binding #'name)
    (pattern [name:id default:expr]
             #:attr kwarg (identifier->keyword-syntax this-syntax #'name)
             #:attr binding #'[name default])
    (pattern [name:id]
             #:attr kwarg (identifier->keyword-syntax this-syntax #'name)
             #:attr binding #'[name 'omit])))
(define-syntax (define-rpc-method stx)
  (syntax-parse stx
    [(_ name:id)
     #'(define-rpc-method name ())]
    [(_ name:id (args*:argument ...))
     #'(define name (rpc-method-lambda name (args* ...)))]))

(define-syntax (rpc-method-lambda stx)
  (syntax-parse stx
    [(_ name:id (args*:argument ...))
     #`(lambda ((~@ args*.kwarg args*.binding) ...)
         (call 'name
               (for/hash ([(k v) (in-hash
                                  (hash
                                   (~@ 'args*.name args*.name) ...))]
                          #:when (not (equal? 'omit v)))
                 (values k v))))]))

(define-syntax (define-torrent-rpc-method stx)
  (syntax-parse stx
    [(_ name:id (arg*:argument ...))
     #:with torrent-name (format-id this-syntax "torrent-~a" #'name)
     #'(define-rpc-method torrent-name
         ([ids 'omit]
          (~@ arg* ...)))]
    [(_ name:id)
     (datum->syntax stx (list #'define-torrent-rpc-method #'name #'()))]))

(define-rpc-method session-get)
(define-rpc-method session-stats)
(define-rpc-method blocklist-update)
(define-rpc-method session-close)
(define-rpc-method port-test)
(define-rpc-method free-space (path))
(define-torrent-rpc-method start)
(define-torrent-rpc-method stop)
(define-torrent-rpc-method start-now)
(define-torrent-rpc-method verify)
(define-torrent-rpc-method reannounce)

(define-torrent-rpc-method get (fields
                                [format]))
(define-torrent-rpc-method set ([bandwidthPriority]
                                [downloadLimit]
                                [downloadLimited]
                                [files-unwanted]
                                [files-wanted]
                                [group]
                                [honorsSessionLimits]
                                [labels]
                                [location]
                                [peer-limit]
                                [priority-high]
                                [priority-low]
                                [priority-normal]
                                [queuePosition]
                                [seedIdleLimit]
                                [seedIdleMode]
                                [seedRatioLimit]
                                [seedRatioMode]
                                [trackerAdd]
                                [trackerList]
                                [trackerRemove]
                                [trackerReplace]
                                [uploadLimit]
                                [uploadLimited]))

(define-torrent-rpc-method remove ([delete-local-data]))
(define-torrent-rpc-method set-location (location [move]))
(define-torrent-rpc-method rename-path (path name))

(module+ test
  (require rackunit)
  ;; torrent-verify: unbound identifier
  (test-pred "torrent-verify exists" procedure? torrent-verify))
