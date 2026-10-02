;;;; dexador-curl-impersonate.asd

(defsystem #:dexador-curl-impersonate
  :author ("凉凉")
  :license "LGPL"
  :version "0.0.0"
  :description "CL binding of curl-impersonate for dexador backend. "
  :depends-on (:dexador
               :cffi
               :alexandria
               :trivial-gray-streams
               :trivial-garbage
               :bordeaux-threads)
  :pathname "lisp"
  :serial t
  :components
  ((:file "package")
   (:file "ffi")
   #+todo (:file "dexador")))

;;;; dexador-curl-impersonate.asd ends here
