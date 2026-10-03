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
  (declare (type (or string quri:uri) uri)
           (type real version)
           ;; An alist, but dexador's API lets both the name and the
           ;; value be any string designator (`dex:get' is routinely
           ;; called with `((:x-foo . "ppp"))'), so this cannot be
           ;; narrowed to `(cons string string)'.
           (type list headers)
           (type (or null cl-cookie:cookie-jar) cookie-jar)
           (type (integer 0) connect-timeout read-timeout)
           (type (integer 0) max-redirects))
  (macrolet ((header (header)              `(alx:assoc-value headers ,header :test #'string-equal))
             (fmt (ctrl-string &rest args) `(format nil ,ctrl-string ,@args)))
    (when (and force-string force-binary)
      (error "You should only use one of `~S', `~S'. " :force-string :force-binary))
    (when (and bearer-auth basic-auth)
      (error "You should only use one of `~S', `~S' Authorization header." :bearer-auth :basic-auth))
    ;; Normalize CONTENT into an octet-vector body, deciding its
    ;; Content-Type along the way -- mirrors the dispatch in dexador's
    ;; own `usocket' backend (src/backend/usocket.lisp) and
    ;; `dexador.body'.
    (let ((content-type nil))
      (setf content
            (etypecase content
              (null   nil)
              (string (setf content-type "text/plain")
                      (babel:string-to-octets content))
              ((array (unsigned-byte 8) (*))
               (setf content-type "application/octet-stream")
               content)
              (pathname
               (setf content-type (or (content-type content)
                                      "application/octet-stream"))
               (alx:read-file-into-byte-vector content))
              (cons
               (if (find-if #'pathnamep content :key #'cdr)
                   (error "Multipart content (an alist with a `pathname' value) ~
                           is not yet supported by the ~S backend. "
                          :curl-impersonate)
                   (progn
                     (setf content-type "application/x-www-form-urlencoded")
                     (babel:string-to-octets
                      (quri:url-encode-params content)))))))
      (when content
        (unless (header "Content-Type")
          (setf (header "Content-Type") content-type))
        (when (eq method :get)
          (warn "HTTP GET (`~S') but with content:~%~A" method content))))

    (with-easy-curl-handle (curl :url url)
      ;; Configuration
      (curl-easy-impersonate curl *impersonate* t)
      ;; Let curl transparently decode `Content-Encoding'. The
      ;; impersonated header list already asks for gzip/deflate/br/zstd,
      ;; but without this curl does not know *it* requested them and
      ;; hands back the still-compressed bytes. Setting this does not
      ;; change the request fingerprint: the `Accept-Encoding' header
      ;; `curl-easy-impersonate' installed in `:http-header' wins.
      (curl-easy-setopt curl :accept-encoding "")
      (when verbose
        (curl-easy-setopt curl :verbose t))

      (case method
        ;; `:http-get' forces a bodyless GET, and curl would flip back
        ;; to POST as soon as `CURLOPT_POSTFIELDS' is set, so a GET that
        ;; does carry a body has to name the method explicitly.
        (:get      (if content
                       (curl-easy-setopt curl :custom-request "GET")
                       (curl-easy-setopt curl :http-get       t)))
        (:head     (curl-easy-setopt curl :no-body        t))
        (:post     (curl-easy-setopt curl :http-post      t))
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
      (cond (basic-auth
             (setf (header "Authorization")
                   (fmt "Basic ~A"
                        (base64:string-to-base64-string
                         (fmt "~A:~A" (car basic-auth) (cdr basic-auth))))))
            (bearer-auth
             (setf (header "Authorization")
                   (fmt "Bearer ~A" bearer-auth))))
      (when cookie-jar
        ;; An empty jar renders as NIL/"" -- sending `Cookie:' with no
        ;; value would be wrong, so only add the header when there is
        ;; something to send.
        (alx:when-let ((cookies (cookie:write-cookie-header
                                 (cookie:cookie-jar-host-cookies
                                  cookie-jar
                                  (quri:uri-host url)
                                  (or (quri:uri-path url) "/")
                                  :securep is-https?))))
          (unless (zerop (length cookies))
            (setf (header "Cookie") cookies))))

      ;; HTTPS relative
      (when is-https?
        ;; SSL relative
        (when ssl-cert-file    (curl-easy-setopt curl :ssl-cert     ssl-cert-file))
        (when ssl-key-file     (curl-easy-setopt curl :ssl-key      ssl-key-file))
        (when ssl-key-password (curl-easy-setopt curl :key-password ssl-key-password)))

      (with-curl-slist (slist (loop :for (name . value) :in headers
                                    :when value
                                      :collect (fmt "~A: ~A"
                                                    (if (stringp name)
                                                        name
                                                        (string-capitalize name))
                                                    value)))
        (curl-easy-setopt curl :http-header slist)

        (let* ((response-stream (if stream
                                    stream
                                    (flex:make-in-memory-output-stream)))
               (headers-table  (make-hash-table :test 'equal)))
          (flet ((header-callback (buffer size)
                   ;; curl calls this once per raw header line (CRLF
                   ;; terminated), including the status line and the
                   ;; trailing blank line; it re-fires for every
                   ;; redirect/100-continue hop when `:follow-location'
                   ;; is on, so clear the table on each new status
                   ;; line and keep only the final hop's headers.
                   (let ((line (string-trim '(#\Return #\Newline)
                                            (cffi:foreign-string-to-lisp buffer :count size))))
                     (cond ((and (>= (length line) 5)
                                 (string= line "HTTP/" :end1 5))
                            (clrhash headers-table))
                           ((zerop (length line))
                            ;; end of this header block; nothing to do
                            )
                           (t
                            (let ((colon (position #\: line)))
                              (when colon
                                (let ((name  (string-downcase
                                              (string-trim '(#\Space #\Tab)
                                                           (subseq line 0 colon))))
                                      (value (string-trim '(#\Space #\Tab)
                                                          (subseq line (1+ colon)))))
                                  ;; `Set-Cookie' legitimately repeats, so
                                  ;; it accumulates as a list (what
                                  ;; dexador's own callers expect from
                                  ;; `(gethash "set-cookie" headers)');
                                  ;; every other field is last-one-wins.
                                  (if (member name '("set-cookie" "set-cookie2")
                                              :test #'string=)
                                      (alx:appendf (gethash name headers-table)
                                                   (list value))
                                      (setf (gethash name headers-table)
                                            value)))))))))
                 (writer-callback (buffer size)
                   (write-sequence
                    (cffi:foreign-array-to-lisp
                     buffer
                     (list :array :uint8 size)
                     :element-type '(unsigned-byte 8))
                    response-stream)))

            (curl-easy-setopt curl :writer-function #'writer-callback)
            (curl-easy-setopt curl :header-function #'header-callback)

            (flet ((perform ()
                     (handler-case (curl-easy-perform curl)
                       (curl-error (e)
                         ;; Exhausting `:max-redirs' is not an error in
                         ;; dexador's API -- it returns the last response
                         ;; received (the final 3xx) -- so keep whatever
                         ;; the callbacks already captured and report it.
                         (unless (= (the fixnum (curl-error-errno e))
                                    +curle-too-many-redirects+)
                           (error e))))
                     (let* ((octets (unless stream
                                      (flex:get-output-stream-sequence
                                       response-stream)))
                            (status (curl-easy-getinfo curl :response-code))
                            (ctype  (curl-easy-getinfo curl :content-type))
                            (final-uri (quri:uri (curl-easy-getinfo curl :effective-url)))
                            (body
                              (cond
                                (want-stream
                                 (let ((in (or stream
                                               (flex:make-in-memory-input-stream octets))))
                                   (if force-binary
                                       in
                                       ;; dexador hands back a character
                                       ;; stream unless FORCE-BINARY, so
                                       ;; decode with the charset the
                                       ;; response advertised.
                                       (dexador.decoding-stream:make-decoding-stream
                                        in
                                        :encoding (or (detect-charset ctype octets)
                                                      babel:*default-character-encoding*)))))
                                (stream       stream)
                                (force-binary octets)
                                (t (decode-body
                                    ctype octets
                                    :default-charset (and force-string
                                                          babel:*default-character-encoding*))))))
                       (when cookie-jar
                         (alx:when-let
                             ((set-cookies (append (gethash "set-cookie"  headers-table)
                                                   (gethash "set-cookie2" headers-table))))
                           (cookie:merge-cookies
                            cookie-jar
                            (loop :for raw :in set-cookies
                                  :unless (zerop (length raw))
                                    :collect (cookie:parse-set-cookie-header
                                              raw
                                              (quri:uri-host final-uri)
                                              (or (quri:uri-path final-uri) "/"))
                                      :into cookies
                                  :finally (return (remove nil cookies))))))
                       ;; Raise an error when the status is 4xx or 5xx,
                       ;; with the same restarts dexador's own backend
                       ;; establishes so `dex:retry-request' and
                       ;; `dex:ignore-and-continue' work here too.
                       (when (<= 400 status)
                         (restart-case
                             (derr:http-request-failed status
                                                       :body    body
                                                       :headers headers-table
                                                       :uri     final-uri
                                                       :method  method)
                           (retry-request ()
                             :report "Retry the same request."
                             (return-from request (apply #'request uri args)))
                           (ignore-and-continue ()
                             :report "Ignore the error and continue.")))
                       ;; The fifth value is the reusable connection
                       ;; stream; this backend hands the socket to curl
                       ;; and never owns one, so it is always NIL.
                       (values body status headers-table final-uri nil))))
              ;; Send CONTENT (if any). `CURLOPT_POSTFIELDS' does not
              ;; copy its buffer (no `CURLOPT_COPYPOSTFIELDS' here), so
              ;; it must point at memory that stays alive through
              ;; `curl-easy-perform' -- CFFI's `:string' ctype only
              ;; guarantees its marshalled buffer for the duration of
              ;; the single `curl_easy_setopt' call, which is not
              ;; enough. So pin an explicitly `foreign-alloc'd buffer,
              ;; freed via `unwind-protect' once perform returns.
              (let ((size (length content))
                    (ptr  nil))
                (unwind-protect
                     (progn
                       ;; A bare `CURLOPT_POST' with no `POSTFIELDS'
                       ;; makes curl source the body from its read
                       ;; callback, which defaults to stdin -- so an
                       ;; empty POST still has to declare size 0.
                       (when (or content (eq method :post))
                         (setf ptr (if (zerop size)
                                       (cffi:null-pointer)
                                       (cffi:foreign-alloc :uint8 :count size)))
                         (dotimes (i size)
                           (setf (cffi:mem-aref ptr :uint8 i) (aref content i)))
                         (curl-easy-setopt curl :post-field-size size)
                         (curl-easy-setopt curl :post-fields*    ptr))
                       (perform))
                  (when (and ptr (not (cffi:null-pointer-p ptr)))
                    (cffi:foreign-free ptr)))))))))))


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
