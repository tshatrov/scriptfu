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

(define (string-join slist delimiter)
  (or (foldr (lambda (s1 s2) (if s1 (string-append s1 delimiter s2) s2)) #f slist) ""))

(define (list-get lst n)
  "like list-ref but returns #f if out of range"
  (let ((len (length lst)))
    (and (< -1 n len) (list-ref lst n))))

(define (make-image-sized-layer img name)
  (car (gimp-layer-new img
                       (car (gimp-image-width img))
                       (car (gimp-image-height img))
                       1 name 100 0)))

(define (walk-layers-recursive-full img test fn)
  "different from walk-layers-recursive from animstack.scm in that it
recurses down a layer group even if it passes the test"
  (let loop ((layers (cadr (gimp-image-get-layers img))))
    (vector-for-each
     (lambda (layer)
       (if (test layer) (fn layer))
       (if (is-true? gimp-item-is-group layer)
           (loop (cadr (gimp-item-get-children layer)))))
     layers)))

(define save-selection #f)
(define restore-selection #f)

(let ((sel #f))
  (set! save-selection
        (lambda (img)
          (set! sel #f)
          (if (is-true? gimp-selection-bounds img)
              (set! sel (car (gimp-selection-save img))))))
  (set! restore-selection
        (lambda (img)
          (if sel
              (begin
                (gimp-selection-load sel)
                (gimp-image-remove-channel img sel))
              (gimp-selection-none img)))))


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

(define (set-parasite item name value)
  (gimp-item-attach-parasite item (list name 3 value)))

(define (get-layer-by-tattoo img tat)
  (and tat
       (string2number tat)
       (let ((layer (car (gimp-image-get-layer-by-tattoo img (string2number tat)))))
         (and (not (= layer -1)) layer))))

(define (layerscript-layer-name srctat tag-index layer-index)
  (string-append *layerscript-layer-name-prefix*
                 " #" (number->string srctat)
                 "." (number->string tag-index)
                 "." (number->string layer-index)))
  
(define (make-layerscript-layer img pos-layer lname tat)
  (let ((layer (make-image-sized-layer img lname))
        (parent (car (gimp-item-get-parent pos-layer)))
        (pos (+ (car (gimp-image-get-item-position img pos-layer)) 1)))
    (if (< parent 0) (set! parent 0))
    (gimp-image-insert-layer img layer parent pos)
    (if tat (gimp-item-set-tattoo layer (string2number tat)))
    layer))

(define (get-linked-layer img source-layer pos-layer tag-index layer-index)
  (let* ((lname (layerscript-layer-name (car (gimp-item-get-tattoo source-layer))
                                        tag-index layer-index))
         (pname (string-append *parasite-prefix* "-" (number->string tag-index)))
         (par (get-parasite source-layer pname)))
    (if par
        (let* ((tattoo-list (string-split (caddr par) #\ ))
               (cur-tat (list-get tattoo-list layer-index))
               (prev-tat (list-get tattoo-list (- layer-index 1)))
               (cur-layer (get-layer-by-tattoo img cur-tat))
               (prev-layer (get-layer-by-tattoo img prev-tat)))
          (or (cons cur-layer #f)
              (begin
                (if prev-layer (set! pos-layer prev-layer))
                (let* ((layer (make-layerscript-layer img pos-layer lname cur-tat))
                       (new-tat (number->string (car (gimp-item-get-tattoo layer)))))
                  (if cur-tat
                      (set-car! (list-tail tattoo-list layer-index) new-tat)
                      (set! tattoo-list (append tattoo-list (list new-tat))))
                  (set-parasite source-layer pname (string-join tattoo-list " "))
                  (cons layer #t)))))
        (let* ((layer (make-layerscript-layer img pos-layer lname #f))
               (new-tat (number->string (car (gimp-item-get-tattoo layer)))))
          (set-parasite source-layer pname new-tat)
          (cons layer #t)))))

;; actions

; with-params
; param parsers: number, color, ...

(define (pop-params n params)
  (let ((pv (make-vector n #f)))
    (do ((i 0 (+ i 1))
         (pl params (cdr pl)))
        ((or (null? pl) (>= i n)) pv)
      (vector-set! pv i (car pl)))))

(define (int-parser s . args)
  (let ((n (string2number s)))
    (if (and n (or (null? args) ((car args) n)))
        (inexact->exact n)
        #f)))

(define *layerscript-param-parsers*
  `((int ,int-parser)
    (pint ,(lambda (s) (int-parser s (lambda (n) (>= n 0)))))
    (string ,(lambda (s) s))))

(define (parse-param s parser)
  (and (> (string-length s) 0)
       (let ((parserfn (cadr (assq parser *layerscript-param-parsers*))))
         (parserfn s))))

(define (process-param-list param-list)
  (let ((count 0))
    (map 
     (lambda (param-def) 
       (prog1
        (let ((pdef-parser (cons param-def 'int)))
          (if (pair? param-def)
              (if (pair? (car param-def))
                  (set! pdef-parser (cons (caar param-def) (cadar param-def)))
                  (set-car! pdef-parser (car param-def))))
          (if (and (pair? param-def) (pair? (cdr param-def)))
              `(,(car pdef-parser) 
                (or (parse-param (vector-ref pv ,count) ',(cdr pdef-parser))
                    (begin ,@(cdr param-def))))
              `(,(car pdef-parser)
                (parse-param (vector-ref pv ,count) ',(cdr pdef-parser)))))
        (set! count (+ count 1))))
     param-list)))

;; (with-params (x y z) ....)
;; 1. param 2. (param default) 3. ((param type)) 4. ((param type) default)   
(macro (with-params form)
  (let ((param-list (cadr form))
        (body (cddr form)))
    `(let* ((pv (pop-params ,(length param-list) params))
            ,@(process-param-list param-list))
       ,@body)))


(define (layerscript-alpha img params)
  (lambda (source target opts)
    ))
  


(define *layerscript-actions*
  `(("alpha" ,layerscript-alpha)))


;; main loop

(define (parse-action img action)
  (let* ((parsed (string-split action #\:))
         (name (car parsed))
         (args (cdr parser))
         (action-fn (cond ((assoc name *layerscript-actions*) => cdr) (else #f))))
    (and action-fn (action-fn img args))))

(define (layerscript-process-tag img layer tag tag-index)
  (save-selection img)
  (let ((opts (list 0 0)) ;; (layer-index current-source)
        (pos-layer layer)
        (max-index 0)
        )
    (for-each 
     (lambda (action-str)
       (let ((action (parse-action img action-str)))
         (if action
             (let* ((layer-index (car opts))
                    (gll (get-linked-layer img layer pos-layer tag-index layer-index))
                    (current-source (- (cadr opts) 1)))
               (when (cdr gll)
                     (set! pos-layer (car gll))
                     (set! visited layer-index))
               (let ((source-layer (if (or (= current-source -1) 
                                           (> current-source max-index))
                                       layer
                                      (car (get-linked-layer img layer pos-layer 
                                                             tag-index current-source)))))
                 (action source-layer (car gll) opts))))))
     tag))
  (restore-selection img))


(define (layerscript-process-layer img layer)
  (let ((tags (extract-layerscript-tags layer)))
    
  ))





))
