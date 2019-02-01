(define-module (demo1 main)
  #:use-module (gi)
  #:use-module (gi glib-2)
  #:use-module (gi gtk-3)
  #:use-module ((gi gio-2) #:prefix Gio)
  #:use-module (gi webkit2-4)
  #:use-module (ice-9 rdelim)
  #:use-module (demo1 uri)
  #:use-module (sxml simple)
  #:use-module (rnrs bytevectors)
  #:export (main))

;; (define <DemoAppWindow>
;;   (register-type "DemoAppWindow"
;;                  (get-gtype <GtkApplicationWindow>)))

(define browserholder #f)
(define urlentry #f)

(define (backbutton_clicked self button)
  (send browserholder (go-back)))

(define (refreshbutton_clicked self button)
  (send browserholder (reload)))

(define (enterkey_clicked self button)
  (send browserholder
        (load-uri (send urlentry
                        (get-text)))))

(define (guile-uri-handler request dummy)
  "This is called when the browser is asked to handle URIs with a URI
scheme of 'scheme:"
  (display "BLAMMO!!!!") (newline)
  (let ((path (send request (get-path))))
    ;; Convert the path into a Guile call.
    (let* ((expression-string (uri-decode-string path))
           ;; Make the Guile call, which should return a string of SXML
           (sxml (eval-string expression-string))
           ;; Convert the SXML into HTML, also tweaking any scheme: URI
           (html (sxml->xml sxml)))
      ;; Push the HTML string into a GInputStream
      ;;   Probably a GMemoryInputStream.
      (let* ((data (string->utf8 html))
             (istream (GioMemoryInputStream-new-from-data data (bytevector-length data) #f)))
        ;; If an error is thrown, consider calling UriSchemeRequest:finish-error.
        ;; Otherwise call UriSchemeRequest:finish with the new webpage in the
        ;;   GInputStream
        (send request (finish istream -1 "html"))))))
  
(define (activate app user-data)
  (format #t "In activate ~S ~S~%" app user-data)
  (let ((window (cast (ApplicationWindow-new app) <GtkApplicationWindow>))
        (builder (Builder-new))

        ;; An ephemeral web context means that there will be no local
        ;; data storage.
        (webcontext (WebContext-new-ephemeral))
        )

    ;; Register a handler for guile URIs
    (send webcontext
          (register-uri-scheme
           "scheme"
           guile-uri-handler
           #f
           (lambda (x) #t)))
    
    (write webcontext) (newline)
    (set! browserholder (cast (WebView-new) <WebKitWebView>))
    (send builder (add-from-file "demo1/ui.glade"))
    (connect (send builder (get-object "button1"))
             (clicked backbutton_clicked #f))
    (connect (send builder (get-object "button2"))
             (clicked refreshbutton_clicked #f))
    (connect (send builder (get-object "entry1"))
             (activate enterkey_clicked #f))
    (send window (set-title "Window"))
    (send window (set-default-size 400 400))
    (send window (show-all))
    (send browserholder (set-editable #f))
    (send browserholder (load-uri "https://google.com"))
    ;;(send browserholder (load-html "<html><head><title>blammo</title></head><body><p>Hi, mom</p><p><a href=\"scheme:(//lonelycactus/file.html\">link</a></p></body></html>" "main.html"))
    (set! urlentry (send builder (get-object "entry1")))
    (send urlentry (set-text "TITLE GOES HERE"))
    (let ((scrolled_window (send builder (get-object "scrolledwindow1"))))
      (send scrolled_window (add browserholder))
      (send window (add (send builder (get-object "box1"))))
      (send browserholder (show-all))
      (send window (show-all)))))

(define (main)
  (let ([app (Application-new "com.lonelycactus.fosdem2019.demo1" 0)]
        [argv (command-line)]
        [argn (length (command-line))])
    (connect app (activate activate #f))
    (send app (run argn argv))))

(main)
