(in-package :abstract-os)

(defconstant kCFCompareLessThan -1)
(defconstant kCFCompareEqualTo 0)
(defconstant kCFCompareGreaterThan 1)

(cffi:defcstruct ns::|__CFString|) ;;fixme
(cffi:defcstruct NS::|__CFArray|)
(cffi:defcstruct ns::|__CFURL|) ;;fixme
(cffi:defcstruct ns::|__CFDictionary|) ;;fixme
(cffi:defcstruct ns::|__CFBundle|) ;;fixme
(cffi:defcstruct NS::|__CFNotificationCenter|) ;; fixme

(cffi:defcfun ("CFBundleGetMainBundle" CFBundleGetMainBundle) :pointer)

(cffi:defcfun ("CFBundleGetBundleWithIdentifier" CFBundleGetBundleWithIdentifier) :pointer (identifier :pointer))

(cffi:defcfun ("CFBundleGetDataPointerForName" CFBundleGetDataPointerForName) :pointer (bundle :pointer) (name :pointer))

(cffi:defcfun ("CFBundleCopyResourcesDirectoryURL" CFBundleCopyResourcesDirectoryURL) :pointer (bundle :pointer))

(cffi:defcfun ("CFURLCopyLastPathComponent" CFURLCopyLastPathComponent) :pointer (url :pointer))

(cffi:defcfun ("CFStringCompare" CFStringCompare) :int64 (theString1 :pointer) (theString2 :pointer) (compare-options :int))

(cffi:defcfun ("CFURLGetFileSystemRepresentation" CFURLGetFileSystemRepresentation) :boolean
  (url :pointer) (resolve-against-base :boolean) (buffer :pointer) (max-buffer-length :int64))

(cffi:defcfun ("CFRelease" CFRelease) :void (pointer :pointer))

(cffi:defcfun ("CFStringCreateWithCharacters" CFStringCreateWithCharacters) :pointer (alloc :pointer) (chars :string) (num-chars :int64))

(defun CFSTR (string)
  (CFStringCreateWithCharacters (cffi:null-pointer) string (length string)))

(cffi:defcfun (CFNumberGetValue "CFNumberGetValue") :boolean (number :pointer) (the-type :long) (value-ptr :pointer))

(defparameter kCFAllocatorDefault (cffi:null-pointer))
(defconstant kCFNumberIntType 9)

(cffi:defcfun (CFDictionaryGetValue "CFDictionaryGetValue") :pointer
  (the-dict :pointer)
  (key :pointer))


(cffi:defcfun (CFDictionaryGetValueIfPresent "CFDictionaryGetValueIfPresent") :boolean
  (the-dict :pointer)
  (key :pointer)
  (p-value :pointer))

(cffi:defcfun (CFStringGetMaximumSizeForEncoding "CFStringGetMaximumSizeForEncoding") :long
  (length :long)
  (encoding :unsigned-int))

(cffi:defcfun (CFStringGetLength "CFStringGetLength") :long
  (string :pointer))

(defconstant kCFStringEncodingUTF8 #x08000100)

(cffi:defcfun (CFStringGetCString "CFStringGetCString") :boolean
  (the-string :pointer)
  (buffer :pointer)
  (buffer-size :pointer)
  (encoding :unsigned-int))
