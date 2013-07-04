;;; LayerScript - persistent layer effects for GIMP
;;; v. 0.1

(define lscr (make-environment

;; library of useful functions (mostly from animstack)

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

(define (list-get lst n)
  "like list-ref but returns #f if out of range"
  (let ((len (length lst)))
    (and (< -1 n len) (list-ref lst n))))

(define (make-image-sized-layer img name)
  (car (gimp-layer-new img
                       (car (gimp-image-width img))
                       (car (gimp-image-height img))
                       1 name 100 0)))

;; end library

(define (parse-layerscript-tag string)
  (let* ((split1 (string-split string #\}))
         (tagstr (substring (car split1) 1 (string-length (car split1)))))
    (if (> (length split1) 1)
        (let* ((split2 (string-split tagstr #\|))
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

;; layer tagging

;; gimp-item-get-tattoo
;; gimp-image-get-layer-by-tattoo

;; parasites documentation: http://www.mail-archive.com/gimp-user%40lists.xcf.berkeley.edu/msg20099.html
;; ("parasite-name" 3 "parasite-value") 

;; lscrtag-1 : "tattoo1.1 tattoo1.2 ..."
;; lscrtag-2 : "tattoo2.1 tattoo2.2 ..."

;; if tattoo is missing, create a new layer under the previous tattoo, or failing that, original layer

(define *parasite-prefix* "lscrtag")

(define *layerscript-layer-name-prefix* "LayerScript layer") 

(define (get-parasite item string)
  "returns #f when not found"
  (let ((plist (cadr (gimp-item-get-parasite-list item))))
    (and (member string plist)
         (car (gimp-item-get-parasite item string)))))

(define (get-layer-by-tattoo img tat)
  (and tat
       (string2number tat)
       (let ((layer (car (gimp-image-get-layer-by-tattoo img (string2number tat)))))
         (and (not (= layer -1)) layer))))

(define (layerscript-layer-name srctat tag-index layer-index)
  (string-append *layerscript-layer-name-prefix*
                 " #" srctat
                 "." (number->string tag-index)
                 "." (number->string layer-index)))
  
(define (make-layerscript-layer img pos-layer lname tat)
  (let ((layer (make-image-sized-layer img lname))
        (parent (car (gimp-item-get-parent pos-layer)))
        (pos (+ (car (gimp-image-get-item-position img pos-layer)) 1)))
    (if (< parent 0) (set! parent 0))
    (gimp-image-insert-layer img layer parent pos)
    layer))

(define (get-linked-layer img source-layer tag-index layer-index)
  (let* ((lname (layerscript-layer-name (car (gimp-item-get-tattoo source-layer))
                                        tag-index layer-index))
         (pname (string-append *parasite-prefix* "-" (number->string tag-index)))
         (par (get-parasite source-layer pname))
         (pos-layer source-layer))
    (if par
        (let* ((tattoo-list (string-split (caddr par) #\ ))
               (cur-tat (list-get tattoo-list layer-index))
               (prev-tat (list-get tattoo-list (- layer-index 1)))
               (cur-layer (get-layer-by-tattoo img cur-tat))
               (prev-layer (get-layer-by-tattoo img prev-tat)))
          (or cur-layer
              (begin
                (if prev-layer (set! pos-layer prev-layer))
                ;; make new layer
                ;; update parasite
                ;; return layer
                ))
          )
        ;; make new layer
        ;; update parasite
        ;; return layer

    )))





))
