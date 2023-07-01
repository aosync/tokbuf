(defpackage #:tokbuf
  (:use #:cl)
  (:export
   #:tokbuf
   #:peek   
   #:next
   #:region
   #:check
   #:rewind
   #:validate
   #:charbuf))

(in-package #:tokbuf)

(defclass tokbuf ()
  ((point
    :initform 0
    :accessor point
    :documentation
    "Amount of tokens read since the beginning.")
   (points
    :initform '(0)
    :accessor points
    :documentation
    "Stack of points at previous checkpoints.")
   (data
    :accessor data
    :documentation
    "Previously read tokens.")
   (next-op
    :initarg :next-op
    :accessor next-op
    :documentation
    "Function used to fetch the tokens."))
  (:documentation
   "Structure allowing buffered fetches over a source of tokens. The reading point is layered in a stack and can be used to rewind to previous tokens."))

(defmethod initialize-instance :after ((self tokbuf) &key element-type)
  "Continuation of INITIALIZE-INSTANCE to automatically create backing token array."
  (if element-type
      (setf (data self)
	    (make-array
	     0
	     :element-type element-type
	     :adjustable t
	     :fill-pointer 0))))

(defmethod peek ((self tokbuf))
  "Peek the next token from the TOKBUF."
  (with-slots (data point next-op) self
    (if (<
	 point
	 (length data))
	(let* ((value (aref data point)))
	  value)
	(let* ((value (funcall next-op)))
	  (vector-push-extend value data)
	  value))))

(defmethod next ((self tokbuf))
  "Consume the next token from the TOKBUF."
  (let* ((value (peek self)))
    (incf (point self))
    value))

(defmethod region ((self tokbuf))
  (with-slots (point points data) self
    (make-array (- point (car points))
		:element-type (array-element-type data)
		:displaced-to data
		:displaced-index-offset (car points))))

(defmethod check ((self tokbuf))
  "Create a checkpoint in the token sequence."
  (push (point self) (points self)))

(defmethod rewind ((self tokbuf))
  "Rewind the TOKBUF to the last checkpoint."
  (setf (point self) (pop (points self))))

(defmethod validate ((self tokbuf))
  "Destroy last TOKBUF checkpoint."
  (pop (points self)))

(defmacro try-next (self &body body)
  `(progn
     (check ,self)
     (let ((token (block nil ,@body)))
       (if token
	   (validate ,self)
	   (rewind   ,self))
       token)))

(defclass charbuf (tokbuf)
  ((line
    :initform 0
    :accessor line
    :documentation
    "Current line.")
   (column
    :initform 0
    :accessor column
    :documentation
    "Current column.")
   (lincols
    :initform '()
    :accessor lincols
    :documentation
    "Stack of (line column) at previous checkpoints."))
  (:documentation
   "Specialization of TOKBUF that also keeps track of the line and column numbers of each of the characters read."))

(defmethod initialize-instance :after ((self charbuf) &rest rest)
  (declare (ignore rest))
  (setf (data self)
	(make-array 0
		    :adjustable t
		    :fill-pointer 0
		    :element-type 'character)))

(defmethod next :around ((self charbuf))
  "Fetch next token. Update line and column."
  (let* ((char (call-next-method)))
    (if (eq char #\linefeed)
	(progn
	  (setf (column self) 1)
	  (incf (line   self) 1))
	(incf (column self)))
    char))

(defmethod check :after ((self charbuf))
  "Create checkpoint. Save line and column."
  (push (list (line self) (column self)) (lincols self)))

(defmethod rewind :after ((self charbuf))
  "Rewind to last checkpoint. Restore line and column at that checkpoint."
  (destructuring-bind (lin col) (pop (lincols self))
    (setf (line   self) lin)
    (setf (column self) col)))

(defmethod validate :after ((self charbuf))
  "Destroy last checkpoint."
  (pop (lincols self)))
