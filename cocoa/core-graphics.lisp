(in-package :abstract-os)

(defconstant kCGEventSourceStatePrivate -1)
(defconstant kCGEventSourceStateCombinedSessionState 0)
(defconstant kCGEventSourceStateHIDSystemState 1)

(cffi:defctype ns::|CGFloat| #+64-bit :double #+32-bit :float)

(cffi:defcstruct ns::|CGPoint|
		 (ns::x ns::|CGFloat|)
		 (ns::y ns::|CGFloat|))

(cffi:defcstruct ns::|CGSize|
		 (ns::width ns::|CGFloat|)
		 (ns::height ns::|CGFloat|))
		 

(cffi:defcstruct ns::|CGRect|
		 (ns::x ns::|CGFloat|)
		 (ns::y ns::|CGFloat|)
		 (ns::width ns::|CGFloat|)
		 (ns::height ns::|CGFloat|))

(cffi:defcstruct ns::|CGPath|) ;;fixme
(cffi:defcstruct ns::|CGContext|) ;;fixme
(cffi:defcstruct ns::|CGAffineTransform|) ;;fixme
(cffi:defcstruct ns::|CGImage|) ;;fixme
(cffi:defcstruct ns::|CGColorSpace|) ;;fixme
(cffi:defcstruct ns::|CGColor|) ;;fixme
(cffi:defcstruct ns::|CGSRegionObject|) ;;fixme
(cffi:defcstruct NS::|__CGEvent|)


(cffi:defcfun (CGEventSourceCreate "CGEventSourceCreate") :pointer (state-id :uint32))

(cffi:defcfun (CGEventSourceSetLocalEventsSuppressionInterval "CGEventSourceSetLocalEventsSuppressionInterval") :void
  (source :pointer) (seconds :double))

(cffi:defcfun (CGGetOnlineDisplayList "CGGetOnlineDisplayList") :unsigned-int
  (max-displays :unsigned-int)
  (active-displays :pointer)
  (matching-display-count :pointer))

(cffi:defcfun (CGDisplayIsAsleep "CGDisplayIsAsleep") :boolean (display :unsigned-int))

(cffi:defcfun (CGDisplayUnitNumber "CGDisplayUnitNumber") :unsigned-int (display :unsigned-int))

(cffi:defcfun (CGDisplayScreenSize "CGDisplayScreenSize") (:struct ns::|CGSize|) (display :unsigned-int))

(cffi:defcfun (CGMainDisplayID "CGMainDisplayID") :unsigned-int)
(cffi:defcfun (CGDisplayBounds "CGDisplayBounds") (:struct ns::|CGRect|) (display :unsigned-int))
(cffi:defcfun (CGDisplayCopyDisplayMode "CGDisplayCopyDisplayMode") :pointer (display :unsigned-int))
(cffi:defcfun (CGDisplayModeGetWidth "CGDisplayModeGetWidth") :int64 (mode :pointer))
(cffi:defcfun (CGDisplayModeGetHeight "CGDisplayModeGetHeight") :int64 (mode :pointer))
(cffi:defcfun (CGDisplayModeGetRefreshRate "CGDisplayModeGetRefreshRate") :double (mode :pointer))
(cffi:defcfun (CGDisplayModeRelease "CGDisplayModeRelease") :void (mode :pointer))

(cffi:defcfun (CGDisplayGammaTableCapacity "CGDisplayGammaTableCapacity") :unsigned-int
  (display :unsigned-int))

(cffi:defcfun (CGGetDisplayTransferByTable "CGGetDisplayTransferByTable") :int
  (display-id :unsigned-int)
  (capacity :unsigned-int)
  (red-table :pointer)
  (green-table :pointer)
  (blue-table :pointer)
  (sample-count :pointer))

(cffi:defcfun (CGSetDisplayTransferByTable "CGSetDisplayTransferByTable") :int
  (display :unsigned-int)
  (table-size :unsigned-int)
  (red-table :pointer)
  (green-table :pointer)
  (blue-table :pointer))


(cffi:defcfun (CGAcquireDisplayFadeReservation "CGAcquireDisplayFadeReservation") :int
  (seconds :float)
  (p-token :pointer))

(cffi:defcfun (CGReleaseDisplayFadeReservation "CGReleaseDisplayFadeReservation") :void
  (token :unsigned-int))

(defconstant kCGDisplayFadeReservationInvalidToken 0)
(defconstant kCGErrorSuccess 0)

(defconstant kCGDisplayBlendNormal 0.0f0)
(defconstant kCGDisplayBlendSolidColor 1.0f0)

(cffi:defcfun (CGDisplayFade "CGDisplayFade") :void
  (token :unsigned-int)
  (duration :float)
  (start-blend :float)
  (end-blend :float)
  (red-blend :float)
  (green-blend :float)
  (blue-blend :float)
  (synchronous? :boolean))





(cffi:defcfun (CGDisplayCopyAllDisplayModes "CGDisplayCopyAllDisplayModes") :pointer
  (display-id :unsigned-int)
  (options :pointer))

(cffi:defcfun (CFArrayGetCount "CFArrayGetCount") :long (the-array :pointer))
(cffi:defcfun (CFArrayGetValueAtIndex "CFArrayGetValueAtIndex") :pointer
  (the-array :pointer)
  (index :long))

(cffi:defcfun (CGDisplayModeGetIOFlags "CGDisplayModeGetIOFlags") :unsigned-int
  (mode :pointer))

(defconstant kDisplayModeValidFlag #x00000001)
(defconstant kDisplayModeSafeFlag #x00000002)
(defconstant kDisplayModeInterlacedFlag #x00000040)
(defconstant kDisplayModeStretchedFlag #x00000800)

(cffi:defcfun (CGOpenGLDisplayMaskToDisplayID "CGOpenGLDisplayMaskToDisplayID") :unsigned-int (display-mask :unsigned-int))

(cffi:defcfun (CGDisplayVendorNumber "CGDisplayVendorNumber") :unsigned-int
  (display :unsigned-int))

(cffi:defcfun (CGDisplayModelNumber "CGDisplayModelNumber") :unsigned-int
  (display :unsigned-int))

(cffi:defcfun (CGDisplaySetDisplayMode "CGDisplaySetDisplayMode") :unsigned-int
  (display :unsigned-int)
  (mode :pointer)
  (options :pointer))
