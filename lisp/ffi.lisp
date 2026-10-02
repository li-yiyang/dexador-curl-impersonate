;;;; ffi.lisp --- FFI binding for curl-impersonate

(in-package :dexador.backend.curl-impersonate.ffi)


;;;; Basic curl warpper

(defun as-curl-init-flags (flags)
  (declare (type (or keyword null (cons keyword list)) flags))
  (flet ((vmap (flag)
           (ecase flag
             (:all     3)
             (:ssl     1)
             (:winsock 2)
             (:nothing 0)
             (:default 3))))
    (etypecase flags
      (null    (vmap :all))
      (keyword (vmap flags))
      (list    (reduce #'logior (mapcar #'vmap flags))))))

(defun curl-global-init (flags)
  "Sets up the program environment that libcurl needs.

Parameter:
+ FLAGS: a list of keywords, or keyword
  + `:all' initialize everything possible,
    this sets all known bits except `:ack-eintr'
  + `:ssl' initialize SSL
  + `:winsock' initialize windows sockets
  + `:nothing' initialize nothing extra
  + `:default' (equal as nil FLAGS)
"
  (let ((res (foreign-funcall ("curl_global_init" :library libcurl-impersonate)
                              :long (as-curl-init-flags flags)
                              :int)))
    (unless (zerop res)
      (error "Failed to initialize curl global environment. "))))

(defcfun (curl-global-cleanup "curl_global_cleanup"
                              :library libcurl-impersonate)
    :void
  "Cleanup curl globally. ")

(defun curl-easy-init ()
  "Return handle pointer to CURL. "
  (let ((curl (foreign-funcall ("curl_easy_init" :library libcurl-impersonate)
                               :pointer)))
    (when (null-pointer-p curl)
      (error "Failed to init curl. "))
    curl))

(defcfun (curl-easy-cleanup "curl_easy_cleanup" :library libcurl-impersonate) :void
  "Clean up curl HANDLE. "
  (handle :pointer))

(defun curl-easy-perform (handle)
  "Perform the curl operation. "
  (declare (type foreign-pointer handle))
  (let ((res (foreign-funcall ("curl_easy_perform" :library libcurl-impersonate)
                              :pointer handle
                              :int)))
    (unless (zerop res)
      (error "Failed to perform request for curl ~A.~%[~D] ~A"
             handle
             res
             (curl-easy-strerror res)))
    (values (zerop res) res)))

(defcfun (curl-easy-strerror "curl_easy_strerror" :library libcurl-impersonate) :string
  "Return error string of error code ERRNO. "
  (errno :int))

(declaim (inline check-curl-easy-setopt))
(defun check-curl-easy-setopt (res)
  (declare (type fixnum res))
  (if (zerop res)
      t
      (error "Failed to set curl option.~%~A" (curl-easy-strerror res))))

(macrolet ((setopt (&body ctype-enums)
             `(progn
                ,@(loop :for (name* . enums) :in ctype-enums
                        :collect (destructuring-bind (name &key (type t) convert (ctype name)
                                                      &allow-other-keys)
                                     (alx:ensure-list name*)
                                   (let ((fn (symbol-concat "%CURL-EASY-SETOPT-" name)))
                                     `(defun ,fn (handle option parameter)
                                        (declare (type foreign-pointer handle)
                                                 (type fixnum option)
                                                 (type ,type parameter))
                                        (foreign-funcall-varargs
                                         ("curl_easy_setopt"
                                          :library libcurl-impersonate)
                                         (:pointer handle :int option)
                                         ,ctype ,(or convert 'parameter)
                                         :int)))))
                (defun curl-easy-setopt (handle option parameter)
                  ,(with-output-to-string (doc)
                     (write-string "Set the OPTION of curl HANDLE with PARAMETER.
Return `t' for success, `nil' otherwise.

Parameters:
+ HANDLE: foreign-pointer
+ OPTION:
"
                                   doc)
                     (loop :for (name* . enums) :in ctype-enums
                           :for docstr := (if (listp name*)
                                              (getf (cdr name*) :documentation (car name*))
                                              name*)
                           :do (format doc "~%  ~A~%" docstr)
                               (loop :for (enum - docstr) :in enums
                                     :do (format doc "  + `~S' ~A~&" enum docstr)))
                     (format doc "~&+ PARAMETER: value set as curl options"))
                  (declare (type foreign-pointer handle)
                           (type keyword option))
                  (check-curl-easy-setopt
                   (ecase option
                     ,@(loop :for (name* . enums) :in ctype-enums
                             :for name  := (alx:ensure-car name*)
                             :for fn    := (symbol-concat "%CURL-EASY-SETOPT-" name)
                             :collect `(,(mapcar #'first enums)
                                        (,fn handle (ecase option ,@(mapcar #'butlast enums)) parameter))))))
                (define-compiler-macro curl-easy-setopt (&whole form handle option parameter)
                  (if (keywordp option)
                      (ecase option
                        ,@(loop :for (name* . enums) :in ctype-enums
                                :for name := (alx:ensure-car name*)
                                :for fn   := (symbol-concat "%CURL-EASY-SETOPT-" name)
                                :collect (list
                                          (mapcar #'first enums)
                                          (list 'check-curl-easy-setopt
                                                (list fn
                                                      'handle
                                                      `(ecase option
                                                         ,@(mapcar #'butlast enums))
                                                      'parameter)))))
                      form)))))
  ;; Syntax
  ;;
  ;;    (setopt
  ;;      ((NAME &key TYPE CONVERT CTYPE DOCUMENTATION) | NAME
  ;;       (OPTION ENUM DOC)))
  (setopt
   ((:bool :ctype         :long
           :convert       (if parameter 1 0)
           :documentation "boolean switch flags")
    (:http-get        80 "use HTTP GET method")
    (:http-post       47 "use HTTP POST method")
    (:follow-location 52 "if or not follow HTTP 3xx Location redirects")
    (:ssl-verify-peer 64 "if or not verify the peer's SSL certificate"))
   ((:bool2 :ctype         :long
            :convert       (if parameter 2 0)
            :documentation "boolean switch flags (0 or 2)")
    (:ssl-verify-host 81 "
    if or not verify the server certificate's name matches host"))

   ((:long :type integer)
    (:http-version       84 "HTTP version to use, possible values:
    + `+curl-http-version-1.0+'
    + `+curl-http-version-1.1+'
    + `+curl-http-version-2.0+'
    + `+curl-http-version-3+'")
    (:post-field-size    60 "size in bytes of the data posted")
    (:connect-timeout    78 "seconds to wait for the connection")
    (:timeout            13 "seconds to wait for the request to complete")
    (:max-redirs         68 "maximum number of redirects to follow,
    -1 for unlimited"))

   ((:url    :ctype   :string
             :type    (or string quri:uri)
             :convert (etypecase parameter
                        (string   parameter)
                        (quri:uri (quri:render-uri parameter))))
    (:url   10002 "target request URL, string or `quri:uri'")
    (:proxy 10004 "proxy URL (scheme://host:port), string or `quri:uri'"))

   ((:string :type string)
    (:key-password    10026 "passphrase for the private key given in `:ssl-key'")
    (:custom-request  10036 "custom HTTP method name to use e.g. \"PATCH\""))

   ((:pointer :type foreign-pointer)
    (:post-fields     10015 "request body to send with a POST"))

   ((:pathname :ctype   :string
               :type    (or string pathname)
               :convert (uiop:native-namestring (truename parameter)))
    (:ssl-cert        10025 "path to the client certificate file")
    (:ssl-key         10087 "path to the client private key file")
    (:ca-path         10097 "
    path to a directory of CA certificates for peer verification"))))

(defcfun (curl-version "curl_version"
                       :library libcurl-impersonate)
    :string)

(defcfun (curl-easy-impersonate "curl_easy_impersonate"
                                :library libcurl-impersonate)
    :int
  "Tell libcurl to impersonate a browser. "
  (handle          :pointer)
  (target          :string)
  (default-headers :int))

(defcfun (curl-slist-append "curl_slist_append"
                            :library libcurl-impersonate)
    :pointer
  "Push STRING to SLIST."
  (slist  :pointer)
  (string :string))

(defcfun (curl-slist-free-all "curl_slist_free_all"
                              :library libcurl-impersonate)
    :void
  "Free the SLIST. "
  (slist  :pointer))

(defun string-list-to-slist (list)
  "Convert lisp LIST into curl_slist.
Return foreign-pointer to curl_slist. "
  (declare (type (or null (cons string list)) list))
  (let ((slist (null-pointer)))
    (handler-case
         (dolist (str list slist)
           (check-type str string)
           (let ((appended (curl-slist-append slist str)))
             (when (null-pointer-p appended)
               (error "Failed to append ~A to curl_slist ~A" str slist))
             (setf slist appended)))
      (error (err)
        (unless (null-pointer-p slist)
          (curl-slist-free-all slist))
        (error err)))))


;;;; Syntax Sugar

(defconstant +curl-http-version-1.0+ 1)
(defconstant +curl-http-version-1.1+ 2)
(defconstant +curl-http-version-2.0+ 3)
(defconstant +curl-http-version-3+   30)

(defmacro with-curl-slist ((slist list) &body body)
  "Create a curl string list SLIST of lisp string LIST.

Syntax:

    (with-curl-slist (SLIST LIST)
      &BODY)

+ SLIST: variable binded with curl slist pointer
+ LIST: a lisp list expression, each element should be a string
"
  `(let ((,slist (string-list-to-slist ,list)))
     (unwind-protect (progn ,@body)
       (curl-slist-free-all ,slist))))

(defmacro with-easy-curl-handle ((handle &rest args &key &allow-other-keys)
                                 &body body)
  "Create a curl HANDLE, and initialize with opt.

Syntax:

    (with-easy-curl-handle (HANDLE &key [OPTION VALUE]*)
      &body)

+ HANDLE: variable for the curl handle pointer
+ OPTION, VALUE: the option keyword and parameter
  which is equal to call `curl-easy-setopt' with
  HANDLE and OPTION, VALUE.
"
  `(let ((,handle (curl-easy-init)))
     (unwind-protect
          (progn
            ,@(loop :for (opt para) :on args :by #'cddr
                    :collect `(curl-easy-setopt ,handle ,opt ,para))
            ,@body)
       (curl-easy-cleanup ,handle))))

;;;; ffi.lisp ends here
