(defclass tokbuf ()
  ((point
    :initform 0
    :accessor point
    :documentation
    "Amount of tokens read since the beginning.")
   (points
    :initform '()
    :accessor points
    :documentation
    "Stack of points at previous checkpoints.")
   (data
    :accessor data
    :documentation
    "Previously read tokens.")
   (get-op
    :initarg :get-op
    :accessor get-op
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

(defmethod fetch ((self tokbuf))
  "Fetch the next token from the TOKBUF."
  (if (<
       (point self)
       (length (data  self)))
      (let* ((value (aref (data self) (point self))))
	(incf (point self))
	value)
      (let* ((value (funcall (get-op self))))
	(vector-push-extend value (data self))
	(incf (point self))
	value)))

(defmethod check ((self tokbuf))
  "Create a checkpoint in the token sequence."
  (push (point self) (points self)))

(defmethod rewind ((self tokbuf))
  "Rewind the TOKBUF to the last checkpoint."
  (setf (point self) (pop (points self))))

(defmethod validate ((self tokbuf))
  "Destroy last TOKBUF checkpoint."
  (pop (points self)))

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

(defmethod fetch :around ((self charbuf))
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
