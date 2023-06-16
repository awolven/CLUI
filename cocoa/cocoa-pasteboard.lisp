(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defun cocoa-copy-string-to-pasteboard (string)
  (let ((pasteboard (ns::|pasteboardWithName:| #@NSPasteboard NSPasteboardNameGeneral)))
    (ns:|declareTypes:owner:| pasteboard (array-with-objects NSPasteboardTypeString) nil)
    (ns:|setString:forType:| pasteboard (objc-runtime::make-nsstring string) NSPasteboardTypeString)
    (values)))

(defun cocoa-copy-string-from-pasteboard ()
  (let* ((pasteboard (ns::|pasteboardWithName:| #@NSPasteboard NSPasteboardNameGeneral))
	 (nsstring (ns:|stringForType:| pasteboard NSPasteboardTypeString)))
    (when nsstring
      (objc-runtime::extract-nsstring nsstring))))
    
