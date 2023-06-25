(defstruct tokbuf
  "Structure allowing buffered gets over a source of tokens. The reading point is layered in a stack and can be used to rewind to previous tokens."
  (point '(0))
  data
  source)

(defun tokbuf-create (element-type source)
  "Create a TOKBUF with given element-type and source function."
  (make-tokbuf
   :data (make-array
	  0
	  :element-type element-type
	  :adjustable t
	  :fill-pointer 0)
   :source source))

(defun tokbuf-get (self)
  "Get the next token from the TOKBUF."
  (if (<
       (car (tokbuf-point self))
       (length (tokbuf-data self)))
      (let* ((value (aref (tokbuf-data self) (car (tokbuf-point self)))))
	(incf (car (tokbuf-point self)))
	value)
      (let* ((value (funcall (tokbuf-source self))))
	(vector-push-extend value (tokbuf-data self))
	(incf (car (tokbuf-point self)))
	value)))

(defun tokbuf-check (self)
  "Create a point checkpoint in the TOKBUF."
  (push (car (tokbuf-point self)) (tokbuf-point self)))

(defun tokbuf-rewind (self)
  "Rewind the TOKBUF point to previous checkpoint."
  (pop (tokbuf-point self)))

(defun tokbuf-validate (self)
  "Undo previous TOKBUF checkpoint."
  (let* ((point (pop (tokbuf-point self))))
    (setf (car (tokbuf-point self)) point)))
