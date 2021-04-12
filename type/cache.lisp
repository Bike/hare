(in-package #:hare.type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cache for types.
;;; It's convenient for equivalent types to be EQ.
;;;

(defclass type-cache ()
  (;; Hash from nonnegative integers to types. Could be a vector...
   (%ints :initform (make-hash-table)
          :reader type-cache-ints)
   ;; Hash from types to types for pointing to them.
   (%pointers :initform (make-hash-table :test #'eq
                                         #+sbcl :weakness #+sbcl :key)
              :reader type-cache-pointers)
   ;; Hash from lists (return ...params) to function types.
   (%funs :initform (make-hash-table :test #'equal)
          :reader type-cache-funs)
   ;; Hash from types to types of arrays of them to them.
   (%arrays :initform (make-hash-table :test #'eq
                                       #+sbcl :weakness #+sbcl :key)
            :reader type-cache-arrays)
   ;; Hash from lists (adt-def ...types) to adts
   (%adts :initform (make-hash-table :test #'equal)
          :reader type-cache-adts)))

(defvar *type-cache*)

(defmacro with-type-cache ((&key (cache nil cache-p)) &body body)
  (declare (ignore options)) ; for future expansion
  `(let ((*type-cache* ,(if cache-p
                            cache
                            `(if (boundp '*type-cache*)
                                 *type-cache*
                                 (make-instance 'type-cache)))))
     ,@body))

(defmacro cached-int (n)
  `(gethash ,n (type-cache-ints *type-cache*)))
(defmacro cached-pointer (ty)
  `(gethash ,ty (type-cache-pointers *type-cache*)))
(defmacro cached-fun (key)
  `(gethash ,key (type-cache-funs *type-cache*)))
(defmacro cached-array (ty)
  `(gethash ,ty (type-cache-arrays *type-cache*)))
(defmacro cached-adt (key)
  `(gethash ,key (type-cache-adts *type-cache*)))
