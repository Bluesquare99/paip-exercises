;; 13.1
;; Define a version of DEFINE-CLASS that handles inheritance and CALL-NEXT-METHOD

;; Collect the instance variables from the parent class chain and make
;; those affect the DEFINE-CLASS method
;;
;; Make my DEFINE-CLASS macro less verbose, switching only in part on
;; whether there is a parent class


;; x. Check that the parent class has been defined
;; x. Inherit the class variables and confine them via an enclosing LET statement
;; 3. What to do about instance variables?
;; 4. Methods in working order
;;      -- pass through parent-class

;; store the methods portion of the parent class in the hash table

;; actually we should enforce a condition that on each of the child
;; classes the instance variables must be defined for the child classes too
;; so, this would mean removing the name part from CHECKING-ACCOUNT but creating a condition that it is incorrect to define it without
;; '

(defparameter *registered-classes*
  (make-hash-table))

(defstruct defined-class
  (parent-class nil :read-only t)
  (inst-vars nil :read-only t)
  (class-vars nil :read-only t)
  (methods nil :read-only t))

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun make-clause (clause)
  "Translate a message from define-class into a case clause."
  `(,(first clause) #'(lambda ,(second clause) .,(rest2 clause))))

(defun generic-fn-p (fn-name)
  "Is this a generic function?"
  (and (fboundp fn-name)
       (eq (get fn-name 'generic-fn) (symbol-function fn-name))))

(defparameter *closure-1* nil)

(defun get-method (object message)
  "Return the method that implements message for this object."
  (funcall object message))

(defun ensure-generic-fn (message)
  "Define an object-oriented dispatch function for a message,
  unless it has already been defined as one."
  (unless (generic-fn-p message)
    (let ((fn #'(lambda (object &rest args)
		  (print message)
		  (print object)
		  ;; in this case
		  ;; "(get-method object message)"
		  ;; retrieves the function we need -- for NAME that would be
		  ;; "(name () name)"
		  (print (get-method object message))
		  (break)
                  (apply (get-method object message) args))))
      (print fn)
      (break)
      (setf (symbol-function message) fn)
      (setf (get message 'generic-fn) fn))))

(defun accumulate-inst-vars (class accumulated-vars)
  "Create a list of all the instance variables of each class in the
parent class hierarchy ascending from current class."
  (let* ((parent-class (defined-class-parent-class
			  (gethash class *registered-classes*))))
    (if parent-class
	(accumulate-inst-vars
	 parent-class
	 (append accumulated-vars
		 (defined-class-inst-vars
		     ;; Necessary to retrieve DEFINED-CLASS struct
		     ;; stored on this string in *REGISTERED-CLASSES*
		     ;; hash table
		     (gethash parent-class *registered-classes*))))
	accumulated-vars)))

;; Basic DEFINE-CLASS
(defmacro define-class (class inst-vars class-vars &body methods)
  "Define a clasS For object-oriented programming."
  `(let ,class-vars
     (mapcar #'ensure-generic-fn ',(mapcar #'first methods)) 
     (defun ,class ,inst-vars 
       (lambda (message)
	 (case message
	   ,@(mapcar #'make-clause methods))))))

;; With an enclosing LET statement for instance variables of the
;; parent class
(defmacro define-class (class parent-class inst-vars class-vars &body methods)
  "Define a class for object-oriented programming."

  (unless (or (null parent-class)
	      (gethash parent-class *registered-classes*))
    (error "The parent class has not been registered."))

  (setf (gethash class *registered-classes*)
	(make-defined-class
	 :parent-class parent-class
	 :inst-vars inst-vars
	 :class-vars class-vars
	 :methods methods))

  `(let ,(if parent-class
	     (defined-class-class-vars (gethash parent-class *registered-classes*))
	     ())
     (let ,class-vars
       (mapcar #'ensure-generic-fn ',(mapcar #'first methods))
       (defun ,class ,(accumulate-inst-vars class inst-vars)
	 (lambda (message)
	   (case message
	     ,@(mapcar #'make-clause methods)
	     (otherwise (print "yo"))))))))


(define-class account nil (name balance)
    ((interest-rate .06))
  (withdraw (amt) (if (<= amt balance)
		      (decf balance amt)
		      'insufficient-funds))
  (deposit (amt) (incf balance amt))
  (balance () balance)
  (name () name)
  (interest () (incf balance (* interest-rate balance))))

(define-class checking-account account
    ()
    ()
  (withdraw (amt) amt))

(setf acct3 (account "max" 2000.0))
(setf acct4 (checking-account "max" 2000.0))


;; (%abc (+ a b c) 4 5 6)
(defun lambda-reader (stream char)
  "Allows for entry of arbitrary number of arguments."
  (declare (ignore char))
  (let ((lam-vars
	  (mapcar (lambda (c) (read-from-string (string c)))
		  (loop :for c := (read-char stream)
			:until (char= c #\ )
			:collect c)))
	(lam-body (read stream t)))

    `(lambda (,@lam-vars)
       (declare (ignorable ,@lam-vars))
       ,lam-body)))

(set-macro-character #\% 'lambda-reader)
