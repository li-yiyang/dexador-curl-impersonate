;;;; ffi.lisp --- FFI binding for curl-impersonate

(in-package :dexador.backend.curl-impersonate.ffi)


;;;; Conditions

(defcfun (curl-easy-strerror "curl_easy_strerror"
                             :library libcurl-impersonate)
    :string
  "Return error string of error code ERRNO. "
  (errno :int))

(define-condition curl-error (error)
  ((errno
    :initarg :errno
    :type    fixnum
    :reader  curl-error-errno
    :documentation
    "Return the error number (curl code) of `curl-error'.
Use `curl-easy-strerror' to get error message about the `curl-error'. "))
  (:report (lambda (condition stream)
             (with-slots (errno) condition
               (format stream "[~D] ~A" errno (curl-easy-strerror errno))))))

(defun check-curl-code (curl-code)
  (declare (type fixnum curl-code))
  (if (zerop curl-code)
      t
      (error 'curl-error :errno curl-code)))


;;;; Callback lookup table

(defvar *curl-callbacks* (make-hash-table :test 'eql)
  "Key: curl handle pointer address
Val: alist of (CALLBACK-NAME . CALLBACK-FUNCTION)")

(defun get-curl-callback (handle callback-name)
  "Return callback function of CALLBACK-NAME for HANDLE. "
  (declare (type foreign-pointer handle)
           (type keyword callback-name))
  (alx:when-let ((alist (gethash (pointer-address handle)
                                 *curl-callbacks*)))
    (the function (alx:assoc-value alist callback-name))))

(defun (setf get-curl-callback) (callback-function handle callback-name)
  (declare (type foreign-pointer handle)
           (type keyword callback-name)
           (type function callback-function))
  (setf (alx:assoc-value (gethash (pointer-address handle)
                                  *curl-callbacks*)
                         callback-name)
        callback-function))


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
  (check-curl-code
   (foreign-funcall
    ("curl_global_init" :library libcurl-impersonate)
    :long (as-curl-init-flags flags)
    :int)))

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

(defun curl-easy-cleanup (handle)
  "Clean up curl HANDLE and callbacks. "
  (declare (type foreign-pointer handle))
  (remhash (pointer-address handle) *curl-callbacks*)
  (foreign-funcall
   ("curl_easy_cleanup" :library libcurl-impersonate)
   :pointer handle))

(defun curl-easy-perform (handle)
  "Perform the curl operation. "
  (declare (type foreign-pointer handle))
  (check-curl-code
   (foreign-funcall
    ("curl_easy_perform" :library libcurl-impersonate)
    :pointer handle
    :int)))

(defparameter *curl-easy-setopt*
  (foreign-symbol-pointer "curl_easy_setopt" :library 'libcurl-impersonate))

(defparameter *curl-easy-getinfo*
  (foreign-symbol-pointer "curl_easy_getinfo" :library 'libcurl-impersonate))

(defun %curl-setopt-string (function handle option parameter)
  (with-foreign-string (pointer parameter)
    (%curl-setopt-pointer function handle option pointer)))

(defcfun (memcpy "memcpy") :pointer
  (destination :pointer)
  (source      :pointer)
  (count       :size))

(defun copy-foreign-to-vector (destination start source count)
  "Copy COUNT octets from SOURCE into DESTINATION at START."
  (declare (type (simple-array (unsigned-byte 8) (*)) destination)
           (type (integer 0) start count)
           (type foreign-pointer source))
  (with-pointer-to-vector-data (pointer destination)
    (memcpy (inc-pointer pointer start) source count))
  destination)

(defcallback curl-writer-function :size
    ((buffer :pointer)
     (size   :size)
     (nmemb  :size)
     (handle :pointer))
  (declare (ignore size))
  (alx:when-let ((callback (get-curl-callback handle :writer-function)))
    (handler-case (funcall (the function callback) buffer nmemb)
      (error () (setf nmemb 0))))
  nmemb)

(defcallback curl-header-function :size
    ((buffer :pointer)
     (size   :size)
     (nitems :size)
     (handle :pointer))
  (declare (ignore size))
  (alx:when-let ((callback (get-curl-callback handle :header-function)))
    (ignore-errors (funcall (the function callback) buffer nitems)))
  nitems)

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
                                        (,(ecase ctype
                                             (:long '%curl-setopt-long)
                                             (:pointer '%curl-setopt-pointer)
                                             (:string '%curl-setopt-string))
                                         *curl-easy-setopt* handle option
                                         ,(or convert 'parameter))))))
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
                  (check-curl-code
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
                                :collect `(,(mapcar #'first enums)
                                           (let ((enum (ecase option
                                                         ,@(mapcar #'butlast enums))))
                                             `(check-curl-code (,',fn ,handle ,enum ,parameter))))))
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
    (:ssl-verify-peer 64 "if or not verify the peer's SSL certificate")
    (:no-body         44 "no body")
    (:verbose         41 "if or not output verbose infomation"))
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

   ((:url :ctype   :string
          :type    (or string quri:uri)
          :convert (etypecase parameter
                     (string   parameter)
                     (quri:uri (quri:render-uri parameter))))
    (:url   10002 "target request URL, string or `quri:uri'")
    (:proxy 10004 "proxy URL (scheme://host:port), string or `quri:uri'"))

   ((:string :type string)
    (:key-password    10026 "passphrase for the private key given in `:ssl-key'")
    (:custom-request  10036 "custom HTTP method name to use e.g. \"PATCH\"")
    (:post-fields     10015 "request body to send with a POST")
    (:accept-encoding 10102 "content encodings curl should accept and
    transparently decode, \"\" for every encoding curl was built with.
    Note: this only enables curl's response decoding -- the
    `Accept-Encoding' request header itself keeps whatever value
    `curl-easy-impersonate' put in the `:http-header' list, so setting
    this does not change the request fingerprint"))

   ((:pathname :ctype   :string
               :type    (or string pathname)
               :convert (uiop:native-namestring (truename parameter)))
    (:ssl-cert 10025 "path to the client certificate file")
    (:ssl-key  10087 "path to the client private key file")
    (:ca-path  10097 "path to a directory of CA certificates for peer verification"))

   (:pointer
    (:http-header  10023 "a foreign-pointer to curl_slist
     use `with-curl-slist' to create a curl_slist from string list")
    (:post-fields* 10015 "a foreign-pointer to raw post fields"))

   ((:header-function
     :ctype   :pointer
     :type    function
     :convert (progn
                (setf (get-curl-callback handle :header-function)
                      (the function parameter))
                ;; header-function => headerdata => 10001
                (%curl-easy-setopt-pointer handle 10029 handle)
                (get-callback 'curl-header-function)))
    (:header-function 20079 "function as writer callback"))

   ((:writer-function
     :ctype   :pointer
     :type    function
     :convert (progn
                (setf (get-curl-callback handle :writer-function)
                      (the function parameter))
                ;; writer-function => writedata => 10001
                (%curl-easy-setopt-pointer handle 10001 handle)
                (get-callback 'curl-writer-function)))
    (:writer-function 20011 "function as writer callback"))
   ))

(defcfun (curl-version "curl_version"
                       :library libcurl-impersonate)
    :string)

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

(defun %curl-easy-getinfo (handle info ctype)
  (with-foreign-object (slot ctype)
    (check-curl-code
     (%curl-getinfo-pointer *curl-easy-getinfo* handle info slot))
    (mem-ref slot ctype)))

(macrolet ((getinfo (&body ctype-enums)
             `(progn
                (defun curl-easy-getinfo (handle info)
                  ,(with-output-to-string (doc)
                     (write-string "Get the INFO of curl HANDLE.

Parameters:
+ HANDLE: foreign-pointer to curl handle
+ INFO:
"
                                   doc)
                     (loop :for (name* . enums) :in ctype-enums
                           :for docstr := (if (listp name*)
                                              (getf (cdr name*) :documentation (car name*))
                                              name*)
                           :do (format doc "~%  ~A~%" docstr)
                               (loop :for (enum - docstr) :in enums
                                     :do (format doc "  + `~S' ~A~&" enum docstr))))
                  (declare (type foreign-pointer handle)
                           (type keyword info))
                  (ecase info
                    ,@(loop :for (name* . enums) :in ctype-enums
                            :collect
                            (destructuring-bind (name &key (ctype name)
                                                 &allow-other-keys)
                                (alx:ensure-list name*)
                              `(,(mapcar #'first enums)
                                (%curl-easy-getinfo
                                 handle
                                 (ecase info
                                   ,@(mapcar #'butlast enums))
                                 ,ctype))))))
                (define-compiler-macro curl-easy-getinfo (&whole form handle info)
                  (if (keywordp info)
                      (ecase info
                        ,@(loop :for (name* . enums) :in ctype-enums
                                :collect
                                (destructuring-bind (name &key (ctype name)
                                                 &allow-other-keys)
                                    (alx:ensure-list name*)
                                  (let ((expr `(ecase info
                                                 ,@(mapcar #'butlast enums))))
                                    `(,(mapcar #'first enums)
                                      (let ((info ,expr))
                                        `(%curl-easy-getinfo ,handle
                                                             ,info
                                                             ,',ctype)))))))
                      form)))))
  (getinfo
   (:long
    (:response-code 2097154 "HTTP response code"))
   (:string
    (:effective-url 1048577 "actual URL")
    (:content-type  1048594 "content type"))))


;;;; impersonate

(declaim (type hash-table *impersonates*))
(defvar *impersonates* (make-hash-table :test 'eq)
  "Key: impersonate name
Val: a plist of OPTION and PARAMETER, the OPTION should be:
+ `:http-version'
+ `:ssl-version'
+ `:ssl-cipher-list'
+ `:ssl-ec-curves'
+ `:http3-ssl-ec-curves'
+ `:ssl-signature-algorithms'
+ `:http3-sig-hash-algs'
+ `:ssl-enable-npn'
+ `:ssl-enable-alpn'
+ `:ssl-enable-alps'
+ `:ssl-enable-ticket'
+ `:ws-ssl-disable-ticket'
+ `:tls-signed-cert-timestamps'
+ `:tls-status-request'
+ `:tls-trust-anchors'
+ `:ssl-permute-extensions'
+ `:ssl-cert-compression'
+ `:ws-ssl-cert-compression'
+ `:http-base-header'
+ `:http2-pseudo-headers-order'
+ `:http2-settings'
+ `:http2-pseudo-headers-order'
+ `:http2-settings'
+ `:http-header-order'
+ `:http3-http-header-order'
+ `:ws-http-header-order'
+ `:http3-pseudo-headers-order'
+ `:http3-settings'
+ `:quic-cid-length'
+ `:quic-initial-packet-number'
+ `:quic-transport-parameters'
+ `:http2-window-update'
+ `:http2-streams'
+ `:http2-no-priority'
+ `:ech'
+ `:tls-grease'
+ `:tls-extension-order'
+ `:http3-tls-extension-order'
+ `:tls-delegated-credentials'
+ `:tls-record-size-limit'
+ `:tls-key-shapes-limit'
+ `:tls-use-new-alps-codepoint'
+ `:stream-weight'
+ `:stream-exclusive'
+ `:proxy-credential-no-reuse'
+ `:split-cookies'
+ `:form-boundary'
")

(declaim (type (cons keyword list) *built-in-impersonate*))
(defvar *built-in-impersonate*
  '(:chrome99
    :chrome100
    :chrome101
    :chrome104
    :chrome107
    :chrome110
    :chrome116
    :chrome119
    :chrome120
    :chrome123
    :chrome124
    :chrome131
    :chrome133a
    :chrome136
    :chrome142
    :chrome145
    :chrome146
    :chrome150
    :chrome99_android
    :chrome131_android

    :edge99
    :edge101

    :safari15_3
    :safari15_5
    :safari153
    :safari155
    :safari17_0
    :safari17_2_ios
    :safari170
    :safari172_ios
    :safari18_0
    :safari18_0_ios
    :safari18_4
    :safari18_4_ios
    :safari180
    :safari180_ios
    :safari184
    :safari184_ios
    :safari26_0
    :safari26_0_1
    :safari26_0_ios
    :safari260
    :safari260_ios
    :safari2601

    :firefox133
    :firefox135
    :firefox144
    :firefox147

    :tor145)
  "A list of built-in browser `impersonate'. ")

(defvar *safari-impersonate*  :safari260)
(defvar *chrome-impersonate*  :chrome146)
(defvar *firefox-impersonate* :firefox147)
(defvar *tor-impersonate*     :tor145)
(defvar *edge-impersonate*    :edge101)

(deftype impersonate ()
  "Browser impersonate names.

Pre-defined browser impersonate can be found in
`*built-in-impersonate*';

Or you can use some alias like `:safari', `:chrome',
`:firefox', `:tor', `:edge';

Or you can also define new browser impersonate
in `*impersonates*' (dev only, currently not supported)."
  '(satisfies impersonate-p))

(defun impersonate-p (impersonate)
  "Test if IMPERSONATE is type of `impersonate'. "
  (and (keywordp impersonate)
       (or (member impersonate '(:safari :chrome :firefox :tor :edge))
           (member impersonate *built-in-impersonate*)
           (and (gethash impersonate *impersonates*) t))))

(defun curl-easy-impersonate (handle impersonate
                              &optional use-default-headers-p)
  "Set curl HANDLE with browser IMPERSONATE. "
  (declare (type foreign-pointer handle)
           (type impersonate impersonate))
  (cond ((member impersonate '(:safari :chrome :firefox :tor :edge))
         (curl-easy-impersonate handle
                                (ecase impersonate
                                  (:safari  *safari-impersonate*)
                                  (:chrome  *chrome-impersonate*)
                                  (:firefox *firefox-impersonate*)
                                  (:tor     *tor-impersonate*)
                                  (:edge    *edge-impersonate*))
                                use-default-headers-p))
        ((member impersonate *built-in-impersonate*)
         (check-curl-code
          (foreign-funcall "curl_easy_impersonate"
                           :pointer handle
                           :string  (format nil "~(~A~)" impersonate)
                           :int     (if use-default-headers-p 1 0)
                           :int)))
        (t
         (let ((info (gethash impersonate *impersonates*)))
           (loop :for (option parameter) :on info :by #'cddr
                 :do (curl-easy-setopt handle option parameter))))))


;;;; Syntax Sugar

(defconstant +curl-http-version-1.0+ 1)
(defconstant +curl-http-version-1.1+ 2)
(defconstant +curl-http-version-2.0+ 3)
(defconstant +curl-http-version-3+   30)
(defconstant +curle-too-many-redirects+ 47
  "`CURLE_TOO_MANY_REDIRECTS': `:max-redirs' was exceeded. ")

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
     (declare (type foreign-pointer ,handle))
     (unwind-protect
          (progn
            ,@(loop :for (opt para) :on args :by #'cddr
                    :collect `(curl-easy-setopt ,handle ,opt ,para))
            ,@body)
       (curl-easy-cleanup ,handle))))

;;;; ffi.lisp ends here
