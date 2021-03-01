(in-package #:hare)

;;; A PRE-MODULE contains a list of toplevel forms that have yet to be
;;; completely parsed, and environments of what is parsed.
(defclass pre-module ()
  (;; A proper list of TOPLEVELs.
   (%toplevels :initform nil :accessor toplevels :type list)
   (%environment :initarg :env :reader environment :type environment)
   (%type-env :initarg :type-env :reader type-env :type type-env)
   ;; An alist from VARIABLEs to INITIALIZERs.
   (%varbinds :initform nil :accessor varbinds :type list)
   ;; An alist from names to toplevel macro functions.
   (%tlexpanders :initform nil :accessor tlexpanders :type list)))

;;; Representation of a toplevel expression. Abstract.
(defclass toplevel () ())

(defclass waiting-on-vars (toplevel)
  ((%waiting-on-vars :initarg :waiting-on-vars :initform nil
                     :accessor waiting-on-vars)))

(defclass waiting-on-types (toplevel)
  ((%waiting-on-types :initarg :waiting-on-types :initform nil
                      :accessor waiting-on-types)))

(defclass waiting-on-initops (toplevel)
  ((%waiting-on-initops :initarg :waiting-on-initops :initform nil
                        :accessor waiting-on-initops)))

(defclass tldefconstant (waiting-on-vars waiting-on-initops toplevel)
  ((%name :initarg :name :reader name :type symbol)
   (%expr :initarg :expr :reader expr)))

(defclass tldefvar (waiting-on-vars waiting-on-initops waiting-on-types
                    toplevel)
  ((%name :initarg :name :reader name :type symbol)
   (%expr :initarg :expr :reader expr)))

(defclass tldefadt (waiting-on-types toplevel)
  ((%name :initarg :name :reader name :type symbol)
   (%pre-def :initarg :pre-def :reader pre-def :type adt-def)
   ;; A proper list of constructor names (symbols).
   (%cnames :initarg :cnames :reader cnames :type list)
   ;; A proper list of type specifiers.
   (%exprs :initarg :exprs :reader exprs :type list)))

(defclass tldeftype (waiting-on-types toplevel)
  ((%name :initarg :name :reader name :type symbol)
   ;; A proper list of TVARs.
   (%parameters :initarg :parameters :reader parameters :type list)
   (%expr :initarg :expr :reader expr)))

(defclass tldefmacro (toplevel)
  ((%name :initarg :name :reader name :type symbol)
   ;; A macro lambda list. mll = (or null symbol (cons mll mll))
   (%mll :initarg :mll :reader mll)
   (%expr :initarg :expr :reader expr)))

(defclass tldefine-symbol-macro (toplevel)
  ((%name :initarg :name :reader name :type symbol)
   (%expr :initarg :expr :reader expr)))

(defclass tldefine-tl-macro (toplevel)
  ((%name :initarg :name :reader name :type symbol)
   ;; A macro lambda list.
   (%mll :initarg :mll :reader mll)
   (%expr :initarg :expr :reader expr)))

(defclass declamation-type (waiting-on-vars waiting-on-types toplevel)
  ((%name :initarg :name :reader name :type symbol)
   ;; A proper list of TVARs.
   (%parameters :initarg :parameters :reader parameters :type list)
   (%type :initarg :type :reader type)))

(defclass declamation-variable (toplevel)
  ((%name :initarg :name :reader name :type symbol)))

(defclass tlunknown (toplevel)
  ((%waiting-on-tlop :initarg :waiting-on-tlop
                     :reader waiting-on-tlop :type symbol)
   (%expr :initarg :expr :reader expr)))
