(in-package :dexador.backend.curl-impersonate.ffi)

(defwrapper* ("curl_setopt_pointer" %curl-setopt-pointer) :int
    ((function :pointer) (handle :pointer) (option :int) (parameter :pointer))
  "return ((int (*)(void *, int, ...)) function)(handle, option, parameter);")

(defwrapper* ("curl_setopt_long" %curl-setopt-long) :int
    ((function :pointer) (handle :pointer) (option :int) (parameter :long))
  "return ((int (*)(void *, int, ...)) function)(handle, option, parameter);")

(defwrapper* ("curl_getinfo_pointer" %curl-getinfo-pointer) :int
    ((function :pointer) (handle :pointer) (info :int) (result :pointer))
  "return ((int (*)(void *, int, ...)) function)(handle, info, result);")
