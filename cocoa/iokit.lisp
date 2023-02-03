(in-package :clui)

(cffi:defcfun (IOServiceMatching "IOServiceMatching") :pointer (name :string))
(cffi:defcfun (IOServiceGetMatchingServices "IOServiceGetMatchingServices") :int (main-port :unsigned-int)
	      (matching :pointer) (existing :pointer))

(defconstant MACH_PORT_NULL 0)

(cffi:defcfun (IOIteratorNext "IOIteratorNext") :unsigned-int (iterator :unsigned-int))
(cffi:defcfun (IORegistryEntryCreateCFProperty "IORegistryEntryCreateCFProperty") :int
  (entry :unsigned-int) (key :pointer) (allocator :pointer) (options :unsigned-int))

(cffi:defcfun (IOObjectRelease "IOObjectRelease") :int (object :unsigned-int))

(defconstant kNilOptions 0)
(defconstant kIODisplayOnlyPreferredName #x00000200)
(defparameter kDisplayVendorID "DisplayVendorID")
(defparameter kDisplayProductID "DisplayProductID")
(defparameter kDisplayProductName "DisplayProductName")

(cffi:defcfun (IODisplayCreateInfoDictionary "IODisplayCreateInfoDictionary") :pointer
  (framebuffer :unsigned-int)
  (options :unsigned-int))
