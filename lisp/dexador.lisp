;;;; dexador.lisp --- cl-curl-impersonate as dexador backend

(in-package :dexador.backend.curl-impersonate)


;;;; Implementation of request

(declaim (type impersonate *impersonate*))
(defvar *impersonate*
  #+darwin :safari
  #-darwin :chrome
  "Currently used browser `impersonate'. ")

;; Maybe TODO:
;; + keep-alive => reuse curl handle
;;
;; TODO:
;; + verbose => use C callback to collect the dbg output,
;;   or maybe write the dbg output to a temp file first

(defun-careful request (uri
                        &rest args
                        &key
                        (method :get)
                        (version 1.1 version?)
                        content headers
                        basic-auth bearer-auth cookie-jar
                        (connect-timeout     *default-connect-timeout*)
                        (read-timeout        *default-read-timeout*)
                        (max-redirects       5)
                        ssl-key-file ssl-cert-file ssl-key-password
                        stream
                        (verbose  *verbose*)
                        force-binary force-string want-stream
                        (proxy    *default-proxy*)
                        (insecure *not-verify-ssl*)
                        ca-path
                        &allow-other-keys
                        &aux
                        (url       (quri:uri uri))
                        (proxy-url (and proxy (quri:uri proxy)))
                        (is-https? (or (string= (quri:uri-scheme url) "https")
                                       (string= (quri:uri-scheme url) "HTTPS"))))
  (declare (ignorable verbose want-stream)
           (type (or string quri:uri) uri)
           (type real version)
           (type (or null (cons (cons string string) list)) headers)
           (type (or null cl-cookie:cookie-jar) cookie-jar)
           (type (integer 0) connect-timeout read-timeout)
           (type (integer 0) max-redirects))
  (macrolet ((header (header)              `(alx:assoc-value headers ,header :test #'string=))
             (fmt (ctrl-string &rest args) `(format nil ,ctrl-string ,@args)))
    (when (and force-string force-binary)
      (error "You should only use one of `~S', `~S'. " :force-string :force-binary))
    (with-easy-curl-handle (curl :url url)
      ;; Configuration
      (curl-easy-impersonate curl *impersonate* t)

      (case method
        (:get      (curl-easy-setopt curl :http-get       nil))
        (:post     (curl-easy-setopt curl :http-post      nil))
        (otherwise (curl-easy-setopt curl :custom-request (string method))))
      (when version?
        (cond ((= version 1.0) (curl-easy-setopt curl :http-version +curl-http-version-1.0+))
              ((= version 1.1) (curl-easy-setopt curl :http-version +curl-http-version-1.1+))
              ((= version 2)   (curl-easy-setopt curl :http-version +curl-http-version-2.0+))
              ((= version 3)   (curl-easy-setopt curl :http-version +curl-http-version-3+))
              (t               (error "Unknown HTTP version ~F. " version))))
      (cond ((zerop max-redirects)
             (curl-easy-setopt curl :follow-location nil))
            (t
             (curl-easy-setopt curl :follow-location t)
             (curl-easy-setopt curl :max-redirs      max-redirects)))
      (when insecure
        (curl-easy-setopt curl :ssl-verify-host nil)
        (curl-easy-setopt curl :ssl-verify-peer nil))
      (when ca-path
        (curl-easy-setopt curl :ca-path (truename ca-path)))
      (when proxy-url
        (curl-easy-setopt curl :proxy proxy-url))

      (curl-easy-setopt curl :connect-timeout connect-timeout)
      (curl-easy-setopt curl :timeout         read-timeout)

      ;; Header relative
      (cond ((and bearer-auth basic-auth)
             (error "You should only use one Authorization header."))
            (basic-auth
             (setf (header "Authorization")
                   (fmt "Basic ~A"
                        (base64:string-to-base64-string
                         (fmt "~A:~A" (car basic-auth) (cdr basic-auth))))))
            (bearer-auth
             (setf (header "Authorization") (fmt "Bearer ~A" bearer-auth))))
      (when cookie-jar
        (setf (header "Cookie")
              (cookie:write-cookie-header
               (cookie:cookie-jar-host-cookies
                cookie-jar
                (quri:uri-host url)
                (or (quri:uri-path url) "/")
                :securep is-https?))))

      ;; HTTPS relative
      (when is-https?
        ;; SSL relative
        (when ssl-cert-file    (curl-easy-setopt curl :ssl-cert     ssl-cert-file))
        (when ssl-key-file     (curl-easy-setopt curl :ssl-key      ssl-key-file))
        (when ssl-key-password (curl-easy-setopt curl :key-password ssl-key-password)))

      ;; Lisp Callbacks
      ;; TODO:
      ;; + implement a callback that write curl response to the stream
      ;; + the stream only support one of octet, character
      ;; + to write response to file, this might be pathname
      ;; + no keep alive
      (with-curl-slist (slist (mapcar (lambda (cons)
                                        (concatenate 'string
                                                     (the string (car cons))
                                                     ": "
                                                     (the string (cdr cons))))
                                      headers))
        (curl-easy-setopt curl :http-header slist)

        (let* ((response-stream (if stream
                                    stream
                                    (flex:make-in-memory-output-stream)))
               (headers-table   (make-hash-table :test 'equal)))
          (flet ((header (buffer size)
                   ;; TODO: parse-header
                   (parse-header
                    (cffi:foreign-string-to-lisp buffer :count size)))
                 (writer (buffer size)
                   (write-sequence
                    (cffi:foreign-array-to-lisp buffer :count size)
                    response-stream)))

            (curl-easy-setopt curl :writer-function #'writer)
            (curl-easy-setopt curl :header-function #'header)

            ;; Perform
            (curl-easy-perform curl)

            ;; Return Status
            (values (if want-stream
                        (if stream
                            stream
                            (flex:make-in-memory-input-stream
                             (flex:get-output-stream-sequence response-stream)))
                        (cond (stream
                               stream)
                              ((or force-string
                                   (and (not force-binary)
                                        (curl-text-content-p curl)))
                               (flex:octets-to-string
                                (flex:get-output-stream-sequence
                                 response-stream)))
                              (t
                               (flex:get-output-stream-sequence
                                response-stream))))
                    (curl-easy-getinfo curl :response-code)
                    headers-table
                    (quri:uri (curl-easy-getinfo curl :effective-url))
                    nil)))))))


;;;; patch of dex:request

;; I hope there would be some thing like Emacs Lisp's advice.
(defvar %dex-request-impl (fdefinition 'dex:request)
  "Save original `dex:request'. ")

(defvar %dex-request-backend dex:*dexador-backend*
  "Save original `dex:*dexador-backend*'. ")

(defun dex-request (uri &rest args
                    &key method version content headers
                      basic-auth bearer-auth
                      cookie-jar connect-timeout read-timeout
                      keep-alive use-connection-pool max-redirects
                      ssl-key-file ssl-cert-file ssl-key-password
                      stream verbose force-binary force-string
                      want-stream proxy insecure ca-path)
  (declare (ignore method version content headers
                   basic-auth bearer-auth cookie-jar
                   connect-timeout read-timeout
                   keep-alive use-connection-pool max-redirects
                   ssl-key-file ssl-cert-file ssl-key-password
                   stream verbose force-binary force-string
                   want-stream proxy insecure ca-path))
  "Create a request using `*dexador-backend*'.
Return values are body-or-stream, status, response-headers, uri.

Parameters:
+ URI: string or `quri:uri' for request URL
+ METHOD: HTTP method, for example
  + `:get'    (see also `dex:get')
  + `:post'   (see also `dex:post')
  + `:head'   (see also `dex:head')
  + `:put'    (see also `dex:put')
  + `:patch'  (see also `dex:patch')
  + `:delete' (see also `dex:delete')
  + `:fetch'  (see also `dex:fetch')
+ VERSION: HTTP version (by default 1.1), possible values:
  + 1.0
  + 1.1
  + 2.0
  + 3
+ CONTENT: request body
  + string
  + pathname
  + octet vector
  + alist
+ HEADERS: alist of request headers
+ BASIC-AUTH: a cons like (USER-NAME . PASSWORD)
  sets the header Authorization: USER-NAME:PASSWORD
+ BEARER-AUTH: sets the header Authorization: Bearer BEARER-AUTH
+ COOKIE-JAR: `cl-cookie:cookie-jar'
+ CONTENT-TIMEOUT: seconds to wait for the connection (default 10s)
+ READ-TIMEOUT: seconds to wait for the whole body (default 10s)
+ KEEP-ALIVE: keep the connection open (default t)
+ USE-CONNECTION-POOL: reuse sockets to the same host
+ MAX-REDIRECTS: redirect limit
+ SSL-KEY-FILE, SSL-CERT-FILE, SSL-KEY-PASSWORD:
  client certificate settings for HTTPS
+ STREAM: existing stream to write the request on
+ VERBOSE: non-nil
+ FORCE-BINARY: never decode the body to a string
+ FORCE-STRING: force body decoded as a string
+ WANT-STREAM: return the body as a stream
+ PROXY: proxy URL, nil for not using proxy
+ INSECURE: non-nil bypass SSL certification verification
+ CA-PATH: a CA certificate file to verify the server's certificate
  for HTTPS requests

Return Values:
+ BODY-OR-STREAM
  + the response body:
    a string when the content-type is text/*,
    or an octet vector
  + if WANT-STREAM is non-nil,
    the body is returned as stream
+ STATUS: http status code
+ RESPONSE-HEADERS:
  a hash table of response headers with downcased keys
+ URI: a `quri:uri'
+ STREAM: a reusable connection stream for the same server or nil
  if KEEP-ALIVE is nil, or server sent Connection: close.
"
  (if (eq dex:*dexador-backend* :curl-impersonate)
      (apply #'request uri args)
      (apply %dex-request-impl uri args)))

(defun init (&key curl-flags)
  "Initialize `dex:request' backend for curl-impersonate.

This will:
+ setup curl global environment (`curl-global-init')
+ patch original `dex:request' implementation to support
  `:curl-impersonate' backend, and the `dex:*dexador-backend*'
  will be set to `:curl-impersonate'.
"
  (curl-global-init curl-flags)
  (setf dex:*dexador-backend* :curl-impersonate)
  (setf (fdefinition 'dex:request) #'dex-request)
  t)

(defun cleanup ()
  "Clean up curl and reset dexador. "
  (curl-global-cleanup)
  (setf (fdefinition 'dex:request) %dex-request-impl
        dex:*dexador-backend*      %dex-request-backend))

;;;; dexador.lisp ends here
