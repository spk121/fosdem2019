(define-module (demo1 main)
  #:use-module (gi)
  #:use-module (Gtk)
  #:use-module (Gio)
  #:export (main))

(define (main)
  (let ((app (GtkApplication-new "com.lonelycactus.fosdem2019.demo1" 0)))
    (signal-connect app "activate" activate #f)
    (GioApplication-run app (length (command-line)) (command-line))))

(main)
  
