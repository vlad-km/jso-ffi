;;; -*- mode:lisp; coding:utf-8 -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright © 2017,2018,2022  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/moren-electron
            )     (                   2022, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#


;;; Package :JSO - API JS Object



(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :jso)
    (make-package :jso :use (list 'cl))))


(in-package :jso)

(export '(jscl::oget jscl::concat
          jscl::lisp-to-js jscl::js-to-lisp
          jscl::vector-to-list jscl::list-to-vector))


;;; name convertor from symbol to string
;;;
;;; symbol := (%nc :a) => "A"
;;;           (%nc 'a) => "a"
;;;           (%nc '-a) => "A"
;;;           (%nc 'a-a) => "aA"
;;; (%nc :|a-b|) => "aB"
;;; (%nc :|a b c|) => "a_b_c" - not release
;;; (%nc "AbC" => "AbC")
(defun %nc (name)
  (cond ((stringp name) (return-from %nc name))
        ((symbolp name) (setq name (string-downcase (symbol-name name))))
        (t (error "Bad object property name: ~a" name)))
  (prog ((len)
         (upcase nil)
         (ch)
         (idx 0)
         (result))
     (setq len (length name))
   rdr
     (if (= idx len) (go eol))
     (setq ch (char name idx)
           idx (1+ idx))
     (if (char= #\- ch)
         (progn
           (setq upcase t)
           (go rdr)))
     (if (char= #\space ch) (setq ch #\_))
     (push
      (if upcase
          (progn
            (setq upcase nil)
            (char-upcase ch))
          ch)
      result)
     (go rdr)
   eol
     (if (null result) (error "Bad syntax ~a for object key" name))
     (setq result (apply 'jscl::concat (reverse result)))
   Exit
     (return result)))

;;; make JS object without Object definitions
;;; (%make-obj "a" 1 "b" 2 "c" (%make-obj "f" (lambda nil t)))
;;; => {a: 1, b:2, c: {f: <fn>}}
(defun %make-obj (&rest kv-seq)
  (if (oddp (length kv-seq))
      (error "MAKE-OBJ: too few arguments."))
  (let ((object (jscl::new)))
    (loop for (key value) on kv-seq by (function cddr) do
          (typecase key
            (symbol (setq key (%nc key)))
            (string t)
            (integer t)
            (t (error "MAKE-OBJ: bad object key type ~a." key)))
          (setf (jscl::oget object key) value))
    object))

(jscl::fset 'make-obj (fdefinition '%make-obj))
(export '(jso::make-obj))

(export '(jso::mko))

;;; todo: move to jscl:
;;;       add (setf ) form
(defun %macro-function (symbol &rest environment)
  (let* ((b (jscl::lookup-in-lexenv symbol jscl::*environment* 'function)))
    (if (and b (eql (jscl::binding-type b) `jscl::macro))
        (jscl::binding-value b)
      nil)))

(defun %explore-dob (l)
  (let ((s (car l)))
    (cond ((stringp s)
           ;; ("a")|("a" "b")
           (if (not (jscl::proper-list-length-p l 1 2))
               (error "wrong pair ~s." l))
           (cond ((jscl::singleton-p l)
                  (append l '(nil)))
                 (t l)))
          ((symbolp s)
           ;; (:s) |(:k 1)|(fn) | (fn a1 ... an)
           (if (or (handler-case (symbol-function s) (error (m) nil))
                   (%macro-function s))
               ;; funcall
               ;; length validate for symbolic expr
               (cond ((jscl::proper-list-length-p l 1)
                      (append (list (symbol-name s)) '(nil)))
                     ((jscl::proper-list-length-p l 2)
                      (setf (first l)(symbol-name s))
                      l)
                     (t (error "wrong length pair ~s" l)))))
          (t (error "wrong type for name ~s." l)))))


(defun %prepare-object-slots (slots)
  (let ((pairs
         (jscl::%lmapcar
          (lambda (it)
            (print it)
            (typecase it
              ((symbol) (list (jso::%nc it) nil))
              ((string) (list (jso::%nc it) nil))
              ((list)  (%explore-dob it))
              (t  (error "mko: bad object slot ~a." it))))
          slots)))
    (apply 'append pairs)))

;;; (mko :a (b 22) "c" (d-d)) -> {a: false, b: 22, c: false, dD: false}
(defmacro mko (&rest args)
    (let ((exp (%prepare-object-slots `,args))
          (p))
      (setq exp (push 'list exp))
      (setq p `(apply 'jso::%make-obj   ,exp))
      `(progn
         ,p)))
(export '(jso::mko))


;;; js object iterator
;;; (iter obj  #'(lambda (key val) (print val)) )
;;; "aaa"
;;; "bbb"
;;; nil
(export '(jso::iter))
(defun iter (jso fn)
  (jscl::%lmapcar
   (lambda (key)(funcall fn key (jscl::oget jso key)))
   (jscl::%lmapcar #'jscl::js-to-lisp
                   (jscl::vector-to-list (#j:Object:keys jso)))))


;;; Return object keys
;;; => ("bbb" "aaa")
(export '(jso::keys))
(defun keys (jso)
  ;; only enumerable 
  (jscl::%lmapcar #'jscl::js-to-lisp (jscl::vector-to-list (#j:Object:keys jso))))

(export '(jso::get-own-prop-names))
(defun get-own-prop-names (obj)
  ;; all keys
  (jscl::%lmapcar #'jscl::js-to-lisp (jscl::vector-to-list (#j:Object:getOwnPropertyNames obj))))

;;; js-object to list
;;; => (("aa" 1) ("bb" 2))
;;; JS NULL marked as NIL (by default)
(export '(jso::to-list))
(defun to-list (jso &optional mark-null)
  (let ((raw))
    (jscl::%lmapcar (lambda (k)
                      (setq raw (jscl::oget jso k))
                      (if (jscl::js-null-p raw)(setq raw mark-null))
                      (list k raw))
                    (jscl::%lmapcar #'jscl::js-to-lisp
                                    (jscl::vector-to-list (#j:Object:keys jso))))))

;;; copies js object
;;; returning a new object with inherite properties new <- old
(export '(jso::copy))
(defun copy (jso)
  (#j:Object:assign (jscl::new) jso))

;;; Merge few objects to new object
;;; The identical properties are replaced
;;; See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign
;;; for details
(export '(jso::merge))
(defun merge (&rest jso)
  (apply #j:Object:assign (jscl::new) jso))

;;; delete properties from obj
;;; use (delete-property key obj) from JSCL
;;; todo: ({d} obj "propName")
(export '(jso::delete-prop))
(defun delete-prop (jso prop)
  (jscl::delete-property prop jso))


;;; js object method bind args to current environment and call
;;; todo: ({bc} obj ("p1" "p2" "p3") ...args)
(export '(jso::bind-call))
(defmacro bind-call ((obj &rest methods) &body args)
  `(funcall ((jscl::oget ,obj ,@methods "bind") ,obj ,@args)))

;;; js object method call without binding
;;; todo: ({c} obj ("frob" "detProp") ...args)
(export '(jso::call))
(defmacro call ((obj &rest methods) &body args)
  `((jscl::oget ,obj ,@methods) ,@args))


;;; js object get/set macro

;;; (jso:get-prop obj "aaa" "bbb" "ccc")
;;; => (oget obj "aaa" "bbb" "ccc")
;;; :todo: ({g} o "aaa" "bbb" "c")
(export '(jso::get-prop))
(defmacro get-prop ((obj &rest pathes))
  ;; obj - js object
  ;; pathes - string | string*
  `(jscl::oget ,obj ,@pathes ))

;;; (jso::set-prop (obj "aaa" ) (new))
;;; => (setf (oget obj "aaa") (new))
;;; (jso::set-prop (obj "aaa" "bbb") (new))
;;; (jso::set-prop (obj "aaa" "bbb" "ccc") "place")
;;; obj => {aaa: {bbb: {ccc: "place"}}}
(export '(jso::set-prop))
;;; todo: ({s} o (n1 ..nN) .body)
(defmacro set-prop ((obj &rest pathes) &body values)
  ;; obj - js object
  ;; pathes - string | string*
  `(setf (jscl::oget ,obj ,@pathes) ,@values))


;;; define js object property
;;; details see at:
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
(export '(jso::defprop))
(defun defprop (object prop &key value get set writable enumerable configurable)
    (let ((args)
          (prop-name
            (cond ((listp prop) (setq prop-name (jscl::%lmapcar '%nc prop)))
                  (t (setq prop-name (%nc prop))))))
        (if value (push (list "value" value) args))
        (if writable (push (list "writable" writable) args))
        (if enumerable (push (list "enumerable" enumerable) args))
        (if configurable (push (list "configuarble" configurable) args))
        (if get (push (list "get" get) args))
        (if set (push (list "set" set) args))
        (setq args (apply '%make-obj (apply 'append args)))
        (cond ((listp prop-name)
               (let ((arguments
                       (append prop-name (list args))))
                   (apply #j:Object:defineProperty object arguments)))
              (t (#j:Object:defineProperty object prop-name args))))
  nil)

;;; Get own property descriptor
;;; REPL =>
;;;   (setq obj (jso:make-obj))
;;;   (jso:defprop obj "name" :value "Meister" :writable t)
;;;   (jso:get-own-prop-descr obj)
;;;   => #<JS-OBJECT [object Object]>
;;;   (jso:to-list *)
;;;    => (("value" "Meister") ("writable" T) ("enumerable" NIL) ("configurable" NIL))
;;;
(export '(jso::get-own-prop-descr))
(defun get-own-prop-descr (object prop-name)
  ;; object - js object
  ;; prop-name - string | keyword | symbol
  (#j:Object:getOwnPropertyDescriptor object (%nc prop-name)))

;;; (:constructor (arg arg arg) &body)
(defun %do-constructor-clause (tail)
    `(lambda (,@(car tail)) ,@(cdr tail)))

;;; (:method method-name (args list) &body)
(defun %do-method-clause (tail)
    (let ((code (cdr tail)))
        (values (car tail)
                `(lambda ,(car code) ,@(cdr code)))))

;;; (:prop name &optional value)
(defun %do-prop-clause (tail)
    (values (car tail) (cdr tail)))


;;;  JS THIS for lisp lambda form
;;;
;;; (lambda (n)
;;;  (with-this (self)
;;;     (setf (jscl::oget self "name") n))))
;;;
(export '(jso::with-this))
(defmacro with-this ((name) &rest body)
    `(let ((,name jscl::this))
         ,@body))

;;; setup object prototype method
(defun %set-proto-method (proto item)
    `(defprop ,proto ,(%nc (car `,item)) :value ,(cadr `,item)))

;;; setup object propertie
;;; (:prop temperature :writable t :enumerable t :value 36.6)
;;; (:prop temperature)
(defun %set-props (proto item)
    (let ((name)
          (descr))
        (unless (setq name (car item))
            (error "Prop name must be"))
        (setq descr (cadr item))
        (if descr (push 'list descr))
        (if descr
            `(apply 'defprop ,proto ,(%nc `,name) ,descr)
            `(defprop ,proto `,(%nc `,name)))))


;;; macro defobject
;;;
;;; make native js object
;;;
;;; name::= string  JS object name => #j:xxx
;;; inherit::= keyword pair => :inherit #j:ParentObject
;;; clauses::= :prop | :constructor | :method
;;;    (:prop weight
;;;           :value 0 :writible t :enumerable t :configurable t
;;;           get (lambda () (with-this (self)
;;;                  (call-meth (self "get-weight")))
;;;           set (lambda () (with-this (self)
;;;                  (call-meth (self "get-weight"))))
;;;    ie Object.defineProperties() parameters see
;;;    https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
;;;    (:constructor (name &optional (weight 0) (height 0))
;;;        (with-this (self)
;;;            (set-prop (self "name") name)
;;;            (set-prop (self "weight") weight)
;;;            (set-prop (self "height") height)) ;;  Object.prototype.constructor
;;;    (:method set-weight (s) (set-prop (jscl::this "weight") s))
(export '(jso::defobject))
(defmacro defobject ((name &key inherit) &rest clauses)
    (let ((parent)
          (inherited-code)
          (constructor)
          (own-construct-code)
          (constructor-code)
          (methods)
          (props)
          (owns)
          (tmp))
        (if inherit (setq parent inherit))
        (dolist (it clauses)
            (case (car it)
              (:prop (multiple-value-bind (name code) (%do-prop-clause (cdr it))
                         (push (list name code) props)))
              (:constructor (setq constructor (%do-constructor-clause (cdr it))))
              (:method (multiple-value-bind (name code) (%do-method-clause (cdr it))
                           (push (list name code) methods)))
              (otherwise (error "DEFOBJECT: unknow clause ~a." (car it)))))
        (setq owns (append name (list "prototype")))
        (if constructor
            (setq constructor-code `((setf ,name ,constructor)))
            (progn
                (setq tmp owns)
                (setq owns (append name (list "__proto__")))
                ;; create and add protopype
                (setq constructor-code `((setf ,name (#j:Object:create (jscl::new)))
                                         (setf ,tmp ,owns)) )))

        (if parent
            (setq inherited-code
                  (let ((parent-prototype (append parent (list "prototype")))
                        (owns-proto-constructor (append owns (list "constructor"))))
                      `((setf ,owns (#j:Object:create ,parent-prototype))
                        (setf ,owns-proto-constructor ,name)))))
        `(progn
             ,@constructor-code
             ,@inherited-code
             ,@(jscl::%lmapcar (lambda (item) (%set-proto-method `,owns `,item)) `,methods)
             ,@(jscl::%lmapcar (lambda (it) (%set-props `,owns `,it)) `,props) )))



#+kass (defmacro {} (&rest n) `(jscl::new))
#+kass (defmacro {n} (&rest p) `(jso:make-obj ,@p))
#+kass (defmacro {l} (o) `(jso:to-list ,o))
#+kass (defmacro {i} (o f) `(jso:iter ,o ,f))
#+kass (defmacro {f} (o (&rest n) &rest a) `(funcall ({g} ,o ,@n) ,@a))
#+kass (defmacro {g} (&rest n) (if (null `,n) nil) `(jscl::oget ,(car n) ,@(cdr n)))
#+kass (defmacro {s} ((r &rest n) &rest a) `(setf (jscl::oget ,r ,@n) ,@a))
#+kass (defmacro {JL} (o) `(jscl::js-to-lisp ,o))
#+kass (defmacro {LJ} (o) `(jscl::lisp-to-js ,o))
#+kass (defmacro {VL} (o) `(jscl::%lmapcar #'jscl::js-to-lisp (jscl::vector-to-list ,o)))
#+kass (defmacro {LV} (o) `(jscl::list-to-vector (jscl::%lmpacar #'jscl::lisp-to-js  ,o)))
#+kass (defmacro {mk} (&rest e) `(jso:make-obj ,@e))
#+kass (defmacro {k} (o) `(#j:Object:keys ,o))
#+kass (defmacro {i} (o f) `(jso:iter ,o ,f))

#+kass
(export '(jso::{}
          jso::{n}
          jso::{l}
          jso::{I}
          jso::{f}
          jso::{g}
          jso::{s}
          jso::{JL}
          jso:::{LJ}
          jso::{VL}
          jso::LV
          jso::{MK}
          jso::{k}
          jso::{i}))


(push :jso *features*)
(in-package :cl-user)

;;; EOF
