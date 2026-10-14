;;;; dexador-curl-impersonate.asd

(defsystem #:dexador-curl-impersonate
  :author ("凉凉")
  :license "MIT"
  :version "0.0.0"
  :description "CL binding of curl-impersonate for dexador backend. "
  :depends-on (:dexador
               :cffi
               :alexandria
               :flexi-streams)
  :defsystem-depends-on (:cffi-grovel)
  :pathname "lisp"
  :serial t
  :components
  ((:file "package")
   (:cffi-wrapper-file "wrappers" :soname "libdexador-curl-shim")
   (:file "ffi")
   (:file "dexador")))

;;;; dexador-curl-impersonate.asd ends here
