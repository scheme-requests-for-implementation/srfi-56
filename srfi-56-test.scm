;;; basic test suite for binary i/o

;; faster but unsafe binary string-port method
; (define (do-read/write n reader writer)
;   (with-input-from-string
;       (with-output-to-string (lambda () (writer n)))
;     reader))

;; temp file method
(define do-read/write
  (let ((file "/tmp/binary-io.dat"))
    (lambda (n reader writer)
;       (if (file-exists? file)
;         (delete-file file))
      (with-output-to-binary-file file (lambda () (writer n)))
      (with-input-from-binary-file file reader))))

(define (fail msg n res)
  (display "fail: ")
  (display msg)
  (display " expected ")
  (display n)
  (display " got ")
  (display res)
  (newline))

(define (pass msg res)
  (display "pass: ")
  (display msg)
  (display " ")
  (display res)
  (newline))

(define (run-tests vary-endian? reader writer msg ls)
  (for-each
   (lambda (n)
     (let ((one (lambda (r w . variant)
                  (let ((res (do-read/write n r w))
                        (msg2 (if (null? variant)
                               msg
                               (string-append msg (car variant)))))
                    (if (not (eqv? n res))
                      (fail msg2 n res)
                      (pass msg2 res))))))
       (one reader writer)
       (cond (vary-endian?
              (one (lambda ( ) (reader   #f    'big-endian))
                   (lambda (n) (writer n #f    'big-endian))
                   " (big-endian)")
              (one (lambda ( ) (reader   #f 'little-endian))
                   (lambda (n) (writer n #f 'little-endian))
                   " (little-endian)")))))
   ls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixnums

(run-tests #f read-binary-uint8 write-binary-uint8
  "read/write-binary-uint8"
  '(0 1 23 31 32 127 128 255))

(run-tests #f read-binary-sint8 write-binary-sint8
  "read/write-binary-sint8"
  '(-128 -127 -23 -1 0 1 23 31 32 127))

(run-tests #t read-binary-uint16 write-binary-uint16
  "read/write-binary-uint16"
  '(0 1 23 31 32 127 128 255 256 65535))

(run-tests #t read-binary-sint16 write-binary-sint16
  "read/write-binary-sint16"
  '(-32768 -32767 -256 -255 -128 -127 -23 -1 0 1 23 31 32 127 128 255 256 32767))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixed size, possibly bignum or unsupported

(run-tests #t read-binary-uint32 write-binary-uint32
  "read/write-binary-uint32"
  '(0 1 23 31 32 127 128 255 256 65535 65536 2147483648 4294967295))

(run-tests #t read-binary-sint32 write-binary-sint32
  "read/write-binary-sint32"
  '(-2147483648 -65536 -65535 -32768 -32767 -256 -255 -128 -127 -23 -1
    0 1 23 31 32 127 128 255 256 32767 65535 65536 2147483647))

(run-tests #t read-binary-uint64 write-binary-uint64
  "read/write-binary-uint64"
  '(0 1 23 31 32 127 128 255 256 65535 65536
    ;;2147483648 4294967295 4294967296
    ;;9223372036854775807 18446744073709551615
    ))

; (run-tests #t read-binary-sint64 write-binary-sint64
;   "read/write-binary-sint64"
;   '(-4294967296 -2147483648 -65536 -65535 -32768 -32767 -256 -255 -128 -127 -23 -1
;     0 1 23 31 32 127 128 255 256 32767 65535 65536
;     ;;2147483647 4294967295 4294967296 9223372036854775807
;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bignums

(run-tests #f read-ber-integer write-ber-integer
  "read/write-ber-integer"
  '(0 1 128 16383 32767
    ;;18446744073709551615 340282366920938463463374607431768211456
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; floating point - these may not pass depending on the internal
;; precision of your Scheme

(run-tests #t read-ieee-float32 write-ieee-float32
  "read/write-ieee-float32"
  `(0.0 1.0 -1.0 0.333333333 ;1/3
;  1.192092896e-7 ,(+ 1 1.192092896e-7)
;     1e-23 -1e-23 3.40282346638528860e+38 -3.40282346638528860e+38
;     1.40129846432481707e-45 -1.40129846432481707e-45
     3.14159265358979323846
    ))

(run-tests #t read-ieee-float64 write-ieee-float64
  "read/write-ieee-float64"
  `(0.0 1.0 -1.0 0.333333333 ;1/3
;  1.192092896e-7 ,(+ 1 1.192092896e-7)
;     1e-23 -1e-23 3.40282346638528860e+38 -3.40282346638528860e+38
;     1.40129846432481707e-45 -1.40129846432481707e-45
     3.14159265358979323846
    ))


