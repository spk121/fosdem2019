(define-module (demo1 uri)
  #:use-module (rnrs bytevectors)
  #:export (uri-encode-string uri-decode-string))

;; This is a scheme to encode / decode scheme procedure calls in a URI
;; friendly format.

;; In the future, I want it to be something like
;; scheme:PROCNAME?ARG1;ARG2
;; where ARG1 is a complete argument
;; but this would require a couple of subparsers like a string argument
;; subparser

;; For now, it is just
;; scheme::PROCNAME%20ARG1%20ARG2
;; where I use regular URI encoding for anything that isn't an unreserved
;; character.

;; A URI scheme for Guile calls

;; guile:PROC[?ARG[;ARG]]

;; reserved gen-delims : / ? # [ ] @
;; reserved sub-delims ! $ & ' ( ) * + , ; =

;; unreserved ALPHA DIGIT - . _ ~

(define *unreserved-cs*
  (string->char-set
   (string-append
   "abcdefghijklmnopqrstuvwxyz"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "0123456789"
   "-._~")))

(define *delims-cs*
  (string->char-set
   (string-append
    "[]"
    "!$&'()*+,=")))

(define (uri-encode-octet x)
  "Stringify integer x as a percent-sign-prefixed hex number"
  (format #f "%~2,'0x" x))

(define (uri-encode-codepoint ch)
  "Converts a character from a scheme expression into a character
for the scheme URI format."
  (cond
   ((char-set-contains? *unreserved-cs* ch)
    (string ch))
   ((char-set-contains? *delims-cs* ch)
    (string ch))
   (else
    (let ((codepoints (bytevector->u8-list (string->utf8 (string ch)))))
      (apply string-append (map uri-encode-octet codepoints))))))

(define (hex-digit-to-int c)
  (cond
   ((and (char<=? #\A c) (char<=? c #\F))
    (+ 10 (- (char->integer c) (char->integer #\A))))
   ((and (char<=? #\a c) (char<=? c #\f))
    (+ 10 (- (char->integer c) (char->integer #\a))))
   ((and (char<=? #\0 c) (char<=? c #\9))
    (- (char->integer c) (char->integer #\0)))))

(define (ascii-digits-to-octet c1 c2)
  (let ((high (hex-digit-to-int c1))
        (low (hex-digit-to-int c2)))
    (+ (* 16 high) low)))

(define (uri-decode-string-port port)
  (let loop ((ch (read-char port))
             (output '()))
    (cond
     ((eof-object? ch)
      (utf8->string (u8-list->bytevector output)))
     ((char=? ch #\%)
      (let* ((c1 (read-char port))
             (c2 (read-char port)))
        (loop (read-char port)
              (append output
                      (list (ascii-digits-to-octet c1 c2))))))
     (else
      (loop (read-char port)
            (append output
                    (list (char->integer ch))))))))

(define (uri-decode-string s)
  (if (and (>= (string-length s) 7)
           (string=? (substring s 0 7) "scheme:"))
      (call-with-input-string (substring s 8) uri-decode-string-port)
      ;; else
      (call-with-input-string s uri-decode-string-port)))

(define (uri-encode-string s)
  ;; Pull off initial and terminal whitespace
  (let* ((s2 (string-trim-both s)))
    (string-append
     "scheme:"
     (apply string-append (map uri-encode-codepoint (string->list s))))))

  
