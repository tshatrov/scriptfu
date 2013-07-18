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

(define *pi* (* (atan 1.0) 4))

(define (vec-rotate x y angle)
  (let ((len (sqrt (+ (* x x) (* y y))))
        (angle (/ (* *pi* angle) 180)))
    (cons (* len (sin angle)) (- (* len (cos angle))))))

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
       (if (or (not test) (test layer)) (fn layer))
       (if (is-true? gimp-item-is-group layer)
           (loop (cadr (gimp-item-get-children layer)))))
     layers)))

(define save-selection #f)
(define restore-selection #f)
(define rollback-selection #f)

(let ((sel '()))
  (set! save-selection
        (lambda (img)
          (let ((sel-new #f))
            (if (is-true? gimp-selection-bounds img)
                (set! sel-new (car (gimp-selection-save img))))
            (set! sel (cons sel-new sel)))))

  (set! rollback-selection
        (lambda (img)
          (if (pair? sel)
              (let ((s (car sel)))
                (if s
                    (gimp-selection-load s)
                    (gimp-selection-none img))))))

  (set! restore-selection
        (lambda (img)
          (if (pair? sel)
              (let ((s (car sel)))
                (set! sel (cdr sel))
                (if s
                    (begin
                      (gimp-selection-load s)
                      (gimp-image-remove-channel img s))
                    (gimp-selection-none img))))))
  )

(define (multicall . fns)
  (lambda args
    (for-each
     (lambda (fn) (apply fn args))
     fns)))

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

(define (remove-parasite item name)
  (let ((plist (cadr (gimp-item-get-parasite-list item))))
    (and (member string plist)
         (gimp-item-detach-parasite item string))))

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
          (if cur-layer 
              (cons cur-layer #f)
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

(define *parasite-color-prefix* "lscrcolor")

(define (color2string color)
  (let ((cs (map number->string color)))
    (string-append "(" (car cs) "," (cadr cs) "," (caddr cs) ")")))

(define (get-color-from-register layer n)
  (let* ((pname (string-append *parasite-color-prefix* "-" (number->string n)))
         (par (get-parasite layer pname)))
    (if par
        (color-parser (caddr par))
        (let* ((fg (car (gimp-context-get-foreground))))
          (set-parasite layer pname (color2string fg))
          fg))))

(define (layerscript-modify-color-register img layer n color remove)
  (let ((pname (string-append *parasite-color-prefix* "-" (number->string n))))
    (if (= remove TRUE)
        (remove-parasite layer pname)
        (set-parasite layer pname (color2string color)))))

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
        (inexact->exact (round n))
        #f)))

(define pint-parser
  (lambda (s) (int-parser s (lambda (n) (>= n 0)))))

;; color can be either an integer, which means a color register
;; or #hhhhhh or (r,g,b)

(define (hex2decimal char)
  (case char
    ((#\0) 0) ((#\1) 1) ((#\2) 2) ((#\3) 3) ((#\4) 4)
    ((#\5) 5) ((#\6) 6) ((#\7) 7) ((#\8) 8) ((#\9) 9)
    ((#\a) 10) ((#\A) 10) ((#\b) 11) ((#\B) 11) ((#\c) 12) ((#\C) 12)
    ((#\d) 13) ((#\D) 13) ((#\e) 14) ((#\E) 14) ((#\f) 15) ((#\F) 15)
    (else #f)))

(define (hex2rgb s)
  (let ((digits (map hex2decimal (string->list (substring s 1 7)))))
    (if (memv #f digits) #f
        (let loop ((lst digits))
          (if (null? lst) '()
              (cons (+ (* 16 (car lst)) (cadr lst)) (loop (cddr lst))))))))

(define (color-parser s)
  (case (string-ref s 0)
    ((#\#) 
     (and (= (string-length s) 7)
          (hex2rgb s)))
    ((#\()
     (let ((sl (- (string-length s) 1)))
       (and (> sl 0)
            (char=? (string-ref s sl) #\))
            (let ((rgb (string-split (substring s 1 sl) #\,)))
              (and (= (length rgb) 3)
                   (let ((nrgb (map (lambda (s) (int-parser s (lambda (n) (<= 0 n 255)))) rgb)))
                     (if (memv #f nrgb) #f nrgb)))))))
    (else (pint-parser s))))

(define *layerscript-selmode-assoc*
  '(("+" 0) ("add" 0) 
    ("-" 1) ("subtract" 1)
    ("=" 2) ("replace" 2)
    ("^" 3) ("x" 3) ("intersect" 3)))

(define (selmode-parser s)
  (let ((as (assoc s *layerscript-selmode-assoc*)))
    (if as (cadr as)
        (int-parser s (lambda (n) (<= 0 n 3))))))

(define *layerscript-param-parsers*
  `((int ,int-parser)
    (pint ,pint-parser)
    (float ,string2number)
    (string ,(lambda (s) s))
    (color ,color-parser)
    (selmode ,selmode-parser)
    ))

(define (parse-param s parser)
  (and s (> (string-length s) 0)
       (let ((parserfn (cadr (assq parser *layerscript-param-parsers*))))
         (parserfn s))))

(define (add-unparsed-sigil sym)
  (string->symbol (string-append "$" (symbol->string sym))))

(define (process-param-list param-list unparsed)
  (let ((count 0))
    (map 
     (lambda (param-def) 
       (prog1
        (let ((pdef-parser (cons param-def 'int)))
          (if (pair? param-def)
              (if (pair? (car param-def))
                  (set! pdef-parser (cons (caar param-def) (cadar param-def)))
                  (set-car! pdef-parser (car param-def))))
          (if unparsed
              `(,(add-unparsed-sigil (car pdef-parser)) (vector-ref pv ,count))
              (if (and (pair? param-def) (pair? (cdr param-def)))
                  `(,(car pdef-parser) 
                    (or (parse-param (vector-ref pv ,count) ',(cdr pdef-parser))
                        (begin ,@(cdr param-def))))
                  `(,(car pdef-parser)
                    (parse-param (vector-ref pv ,count) ',(cdr pdef-parser))))))
        (set! count (+ count 1))))
     param-list)))

;; (with-params (x y z) ....)
;; 1. param 2. (param default) 3. ((param type)) 4. ((param type) default)   
(macro (with-params form)
  (let ((param-list (cadr form))
        (body (cddr form)))
    `(let* ((pv (pop-params ,(length param-list) params))
            ,@(process-param-list param-list #f)
            ,@(process-param-list param-list #t))
       ,@body)))

(define (stringify value)
  (cond ((pair? value) (color2string value))
        ((number? value) (number->string value))
        (else value)))

(define (make-plist . args)
  (map stringify args))

;; selection actions

(define (layerscript-alpha img params)
  (with-params 
   (((mode selmode) 2))
   (lambda (source target opts)
     (gimp-image-select-item img mode source))))
  
(define (layerscript-all img params)
  (lambda (source target opts)
    (gimp-selection-all img)))

(define (layerscript-none img params)
  (lambda (source target opts)
    (gimp-selection-none img)))

(define (layerscript-invert img params)
  (lambda (source target opts)
    (gimp-selection-invert img)))

(define (layerscript-grow img params)
  (with-params
   ((steps 1))
   (let* ((fn (if (< steps 0) gimp-selection-shrink gimp-selection-grow))
          (steps (abs steps)))
     (lambda (source target opts)
       (fn img steps)))))

(define (layerscript-feather img params)
  (with-params
   (((radius pint) 1))
   (lambda (source target opts)
     (gimp-selection-feather img radius))))

(define (layerscript-move-core img params fn)
  (with-params
   (((x float) 0) ((y float) 0) ((angle float)))
   (if angle
       (let ((vr (vec-rotate x y angle)))
         (set! x (car vr))
         (set! y (cdr vr))))
   (lambda (source target opts)
     (fn img target x y))))

(define (layerscript-move-selection img params)
  (layerscript-move-core 
   img params
   (lambda (img target x y)
     (gimp-selection-translate img x y))))  

(define (select-rectangle img op x y width height)
  (gimp-context-set-feather FALSE)
  (gimp-image-select-rectangle img op x y width height))

(define (layerscript-lbox img params)
  (with-params
   (((mode selmode) 2))
   (lambda (source target opts)
     (let ((xy (gimp-drawable-offsets source))
           (width (car (gimp-drawable-width source)))
           (height (car (gimp-drawable-height source))))
       (select-rectangle img mode (car xy) (cadr xy) width height)))))

(define (layerscript-sbox img params)
  (with-params
   (((mode selmode) 2))
   (lambda (source target opts)
     (let ((bounds (gimp-selection-bounds img)))
       (if (= (car bounds) TRUE)
           (let* ((x (cadr bounds))
                  (y (caddr bounds))
                  (width (- (list-ref bounds 3) x))
                  (height (- (list-ref bounds 4) y)))
             (select-rectangle img mode x y width height)))))))

;; TODO
;; abox (alpha bounding box)

(define (fade-selection img level)
  (let ((sel (car (gimp-image-get-selection img))))
    (gimp-levels sel 0 0 255 1 0 level)))

(define (layerscript-fade img params)
  (with-params
   (((level pint) 128) (check-selection 0))
   (if (> level 255) (set! level 255))
   (lambda (source target opts)
     (if (or (= check-selection 0)
             (is-true? gimp-selection-bounds img))
         (fade-selection img level)))))

;; editing actions

(define (layerscript-copy img params)
  (with-params
   ((check-selection 0))
   (lambda (source target opts)
     (save-selection img)
     (if (and (or (= check-selection 0)
                  (is-true? gimp-selection-bounds img))
               (is-true? gimp-edit-copy source))
         (let ((fl (car (gimp-edit-paste target FALSE))))
           (gimp-floating-sel-anchor fl)))
     (restore-selection img))))

(define (layerscript-fill img params)
  (with-params
   (((color color) 0) (check-selection 0))
   (lambda (source target opts)
     (let ((c (if (pair? color) color 
                  (get-color-from-register (caddr opts) color))))
       (when (or (= check-selection 0)
                 (is-true? gimp-selection-bounds img))
             (gimp-context-push)
             (gimp-context-set-foreground c)
             (gimp-edit-fill target 0)
             (gimp-context-pop))))))

(define (layerscript-clear img params)
  (with-params
   ((check-selection 0))
   (lambda (source target opts)
     (if (or (= check-selection 0)
               (is-true? gimp-selection-bounds img))
         (gimp-edit-clear target)))))

(define (layerscript-blurshape img params)
  (with-params
   (((color color) 0) (init 3) (size 5) (invert 0))
   (lambda (source target opts)
     (gimp-context-push)
     (save-selection img)
     (let ((curcolor (if (pair? color) color (get-color-from-register (caddr opts) color)))
           (k init)
           (i 0))
       (gimp-context-set-foreground curcolor)
       (while 
        (< i size)
        (cond ((> k 0) (gimp-selection-grow img k))
              ((< k 0) (gimp-selection-shrink img (abs k))))
        
        (if (is-true? gimp-selection-bounds img)
            (cond ((= invert 0)
                   (fade-selection img (* (/ 1 (- size i)) 255))
                   (gimp-edit-fill target 0))
                  ((= i 0)
                   (fade-selection img (* (/ (- size 1) size) 255))
                   (gimp-edit-fill target 0))
                  (else
                   (fade-selection img (* (/ 1 (- size i)) 255))
                   (gimp-edit-clear target))))

        (rollback-selection img)
        (set! k (- k 1))
        (set! i (+ i 1))))
     (restore-selection img)
     (gimp-context-pop))))


(define (layerscript-dropshadow img params)
  (with-params
   (((color color) '(0 0 0)) ((opacity float) 75) ((size pint) 5) 
    ((offset-angle float) 120) ((offset-distance float) 5))
   (let* ((f1 (layerscript-alpha img '()))
          (f2 (layerscript-move-selection img (make-plist offset-distance #f offset-angle)))
          (init (ceiling (/ size 2)))
          (f3 (layerscript-blurshape img (make-plist color init size)))
          (f4 (layerscript-opacity img (make-plist opacity))))
     (multicall f1 f2 f3 f4))))


;; layer actions

(define (layerscript-opacity img params)
  (with-params
   (((opacity float) 100))
   (if (< opacity 0) (set! opacity 0))
   (if (> opacity 100) (set! opacity 100))
   (lambda (source target opts)
     (gimp-layer-set-opacity target opacity))))


(define (layerscript-move-layer img params)
  ;; TODO: ensure idempotency
  (layerscript-move-core 
   img params
   (lambda (img target x y)
     (gimp-layer-translate target x y))))

;; meta actions

(define (layerscript-source img params)
  (with-params
   (((src pint) 0))
   (lambda (source target opts)
     (set-car! (cdr opts) src))))

(define (layerscript-next img params)
  (with-params
   (((src pint)))
   (lambda (source target opts)
     (set-car! (cdr opts) (if src src (+ (car opts) 1)))
     (set-car! opts (+ (car opts) 1)))))
         
(define (layerscript-prev img params)
  (with-params
   (((src pint)))
   (lambda (source target opts)
     (let ((li (car opts)))
       (cond ((> li 0)
              (set-car! (cdr opts) (if src src (- li 1)))
              (set-car! opts (- li 1)))
             (src (set-car! (cdr opts) src)))))))



(define *layerscript-actions*
  `(("alpha" ,layerscript-alpha)
    ("all" ,layerscript-all)
    ("none" ,layerscript-none)
    ("invert" ,layerscript-invert)
    ("grow" ,layerscript-grow)
    ("feather" ,layerscript-feather)
    ("smove" ,layerscript-move-selection)
    ("lbox" ,layerscript-lbox)
    ("sbox" ,layerscript-sbox)
    ("fade" ,layerscript-fade)

    ("copy" ,layerscript-copy)
    ("fill" ,layerscript-fill)
    ("clear" ,layerscript-clear)
    ("blurshape" ,layerscript-blurshape)
    ("dropshadow" ,layerscript-dropshadow)
    
    ("opacity" ,layerscript-opacity)
    ("move" ,layerscript-move-layer)

    ("source" ,layerscript-source)
    (">" ,layerscript-next)
    ("next" ,layerscript-next)
    ("<" ,layerscript-prev)
    ("prev" ,layerscript-prev)
    ))


;; main loop

(define (clear-layer img layer)
  (save-selection img)
  (gimp-selection-none img)
  (gimp-edit-clear layer)
  (restore-selection img)
  )

(define (parse-action img action)
  (let* ((parsed (string-split action #\:))
         (name (car parsed))
         (args (cdr parsed))
         (action-fn (cond ((assoc name *layerscript-actions*) => cadr) (else #f))))
    (and action-fn (action-fn img args))))

(define (layerscript-process-tag img layer pos-layer tag tag-index)
  (save-selection img)
  ;; TODO: allow to assign starting selection
  (gimp-selection-none img)

  (let ((opts (list 0 0 layer)) ;; (layer-index current-source master-layer)
        (max-index -1)
        )
    (for-each 
     (lambda (action-str)
       (let ((action (parse-action img action-str)))
         (if action
             (let* ((layer-index (car opts))
                    (gll (get-linked-layer img layer pos-layer tag-index layer-index))
                    (current-source (- (cadr opts) 1)))
               (if (cdr gll)
                   (set! pos-layer (car gll)))
               (when (> layer-index max-index)
                     (set! max-index layer-index)
                     (if (not (cdr gll)) (clear-layer img (car gll))))
               (let ((source-layer (if (or (= current-source -1) 
                                           (> current-source max-index))
                                       layer
                                      (car (get-linked-layer img layer pos-layer 
                                                             tag-index current-source)))))
                 (action source-layer (car gll) opts))))))
     tag)
    (restore-selection img)
    pos-layer))


(define (layerscript-process-layer img layer)
  (let ((tags (extract-layerscript-tags layer))
        (pos-layer layer)
        (tag-index 0))
    (for-each
     (lambda (tag)
       (set! pos-layer (layerscript-process-tag img layer pos-layer tag tag-index))
       (set! tag-index (+ tag-index 1))
       )
     tags)
  ))

(define (layerscript-process-all img)
  (srand (realtime))
  (gimp-image-undo-group-start img)
  (let ((active-layer (car (gimp-image-get-active-layer img))))
    (walk-layers-recursive-full 
     img #f
     (lambda (layer) (layerscript-process-layer img layer)))
    (if (not (= active-layer -1))
        (gimp-image-set-active-layer img active-layer)))
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

))

(define script-fu-layerscript-process-all lscr::layerscript-process-all)

(script-fu-register
 "script-fu-layerscript-process-all"
 "Process LayerScript tags"
 "Process all LayerScript tags"
 "Timofei Shatrov"
 "Copyright 2013"
 "July 7, 2013"
 "RGB RGBA GRAY GRAYA" 
 SF-IMAGE     "Image to use"       0
 )

(script-fu-menu-register "script-fu-layerscript-process-all" "<Image>/Script-Fu/LayerScript")

(define script-fu-layerscript-modify-color-register lscr::layerscript-modify-color-register)

(script-fu-register
 "script-fu-layerscript-modify-color-register"
 "Modify color registers..."
 "Modify a color register attached to active layer"
 "Timofei Shatrov"
 "Copyright 2013"
 "July 8, 2013"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-DRAWABLE  "Current layer"      0
 SF-ADJUSTMENT "Register" '(0 0 255 1 1 0 SF-SPINNER)
 SF-COLOR "Color" '(0 0 0)
 SF-TOGGLE "Clear register" 0
 )

(script-fu-menu-register "script-fu-layerscript-modify-color-register" "<Image>/Script-Fu/LayerScript")
