;;;; package.lisp --- Package definition of Dexador.Backend.Curl-Impersonate

;; Patch of dexador.util
;; dexador.util:symbol-concat is introduced in my dexador fork:
;; https://github.com/li-yiyang/dexador
#.(unless (find-symbol "SYMBOL-CONCAT" :dexador.util)
    `(progn
       (uiop:export* "SYMBOL-CONCAT" :dexador.util)
       (defun ,(intern "SYMBOL-CONCAT" :dexador.util) (&rest things)
         "Concat THINGS as an interned symbol.
Return a symbol of `*package*'. "
         (intern
          (with-output-to-string (sym)
            (dolist (thing things)
              (typecase thing
                (symbol    (write-string (string thing) sym))
                (string    (write-string thing sym))
                (character (write-char   thing sym))
                (t (format sym "~A" thing)))))))))

(uiop:define-package #:dexador.backend.curl-impersonate.ffi
  (:use :common-lisp :cffi)
  (:local-nicknames (:alx :alexandria))
  (:documentation
   "The package `dexador.backend.curl-impersonate.ffi' wraps
curl-impersonate API and provide minimal curl API wrap to
use for the package `dexador.backend.curl-impersonate'. ")
  (:import-from :dexador.util
                #:symbol-concat)
  (:export
   #:libcurl-impersonate
   #:curl-global-init
   #:curl-global-cleanup
   #:curl-easy-init
   #:curl-easy-cleanup
   #:curl-easy-perform
   #:curl-easy-strerror
   #:curl-easy-setopt
   #:curl-version
   #:with-easy-curl-handle
   #:+curl-http-version-1.0+
   #:+curl-http-version-1.1+
   #:+curl-http-version-2.0+
   #:+curl-http-version-3+))

(uiop:define-package #:dexador.backend.curl-impersonate
  (:use
   :common-lisp
   :dexador.backend.curl-impersonate.ffi
   :dexador.restarts
   :dexador.encoding
   :dexador.util)
  (:nicknames
   :dexador.curl-impersonate)
  (:local-nicknames (:alx :alexandria)
                    (:tgs :trivial-gray-streams))
  (:documentation
   "The package `dexador.backend.curl-impersonate' provides
a `dexador:request' backend on curl-impersonate.

Setting the variable `*impersonate*' will change the
`dexador:request' impersonate. It has to be noted that
the `*impersonate*' should be `impersonate-p', which means
it is one of pre-defined browser impersonate or one of
you-defined `impersonate'.

To define your own `impersonate', use
`define-browser-impersonate':

    (define-browser-impersonate IMPERSONATE
      DOCUMENTATION
      )

To get documentation of impersonate, use
(documentation IMPERSONATE 'IMPERSONATE). ")
  (:export
   #:curl-stream
   #:define-browser-impersonate
   #:impersonate
   #:impersonate-p
   #:*impersonate*
   #:request
   #:init
   #:cleanup))


(in-package :dexador.backend.curl-impersonate.ffi)

(define-foreign-library libcurl-impersonate
  (:darwin  (:or "libcurl-impersonate.dylib"
                 #.(asdf:system-relative-pathname
                    :dexador-curl-impersonate
                    "lib/libcurl-impersonate.dylib")))
  (:linux   (:or "libcurl-impersonate.so"
                 #.(asdf:system-relative-pathname
                    :dexador-curl-impersonate
                    "lib/libcurl-impersonate.so")))
  (t        (:default "libcurl-impersonate")))

(use-foreign-library libcurl-impersonate)

;;;; package.lisp ends here
