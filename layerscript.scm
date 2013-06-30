;;; LayerScript - persistent layer effects for GIMP
;;; v. 0.1


;; library (mostly from animstack)
(define lscr (make-environment

(define (string2number str . opt)
  "Replacement for string->number, which throws an uncatchable
exception as of GIMP 2.8. Returns #f if not a number."
  (let ((s2a (string->atom str))
        (fn (if (pair? opt) (car opt) number?)))
    (and (fn s2a) s2a)))

(define (vector-for-each fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i))))

(define (vector-for-each-i fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i) i)))

(define (is-true? fn item)
  ;; does fn return '(TRUE) ?
  (= (car (fn item)) TRUE))

(define (int-round x)
  (inexact->exact (round x)))

(define (string-split string char)
  (let ((res (list)))
    (do ((i (string-length string) (- i 1))
         (chunk (list))
         (new (lambda (chunk) (cons (list->string chunk) res))))
        ((<= i 0) (new chunk))
      (let ((chr (string-ref string (- i 1))))
        (if (char=? chr char)
            (begin (set! res (new chunk)) (set! chunk (list)))
            (set! chunk (cons chr chunk)))))))

))
;; end library


(define (parse-layerscript-tag string)
  (let* ((split1 (lscr::string-split string #\}))
         (tagstr (substring (car split1) 1 (string-length (car split1)))))
    (if (> (length split1) 1)
        (let* ((split2 (lscr::string-split tagstr #\|))
               (pos (+ (string-length tagstr) 1)))
          (cons split2 pos))
        (cons #f (string-length string)))))

(define (extract-layerscript-tags layer . params)
  (let loop ((layer-name-list (string->list (car (gimp-item-get-name layer)))))
    (let ((tagtail (memv #\{ layer-name-list)))
      (if tagtail
          (let* ((parsed (parse-layerscript-tag (list->string tagtail)))
                 (tag (car parsed))
                 (next (list-tail tagtail (cdr parsed))))
            (if (and tag (or (null? params) ((car params) tag)))
                (cons tag (loop next))
                (loop next)))
          (list)))))
