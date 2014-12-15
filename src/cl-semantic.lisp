;;;; 

(in-package #:cl-semantic)

(defparameter *agraph-base-url* "http://192.168.5.128:10035")

(defparameter *username* "user")
(defparameter *password* "admin")

(defun %request (method request &key content parameters content-type (accept "*/*"))
  (drakma:http-request (format nil "~A~A" *agraph-base-url* request)
                       :method method
                       :accept accept
                       :basic-authorization (list *username* *password*)
                       :parameters parameters
                       :content content
                       :preserve-uri t
                       :content-type content-type))

(defun create-repo (catalog repo)
  (%request :put (format nil "/~A/repositories/~A" catalog repo)))

(defun get-repos ()
  (%request :get "/repositories"))

(defun create-repo-session (catalog repo &optional auto-commit)
  (%request :post (format nil "/catalogs/~A/repositories/~A/session?script=test.cl" 
                          catalog repo)
            :parameters (list (cons "autoCommit" (if auto-commit "true" "false")))))

(defparameter +so-vector+ #x01)
(defparameter +so-string+ #x05)
(defparameter +so-null+ #x07)
(defparameter +so-list+ #x08)
(defparameter +so-pos-int+ #x09)
(defparameter +so-end-of-items+ #x0a)
(defparameter +so-neg-int+ #x0b)
(defparameter +so-byte-vector+ #x0f)

(defun serialize (&rest args)
  (flet ((serialize-int (int)
           (apply #'vector (loop
                              with int = int
                              as lower = (boole boole-and int #x7f) and
                              rest = (ash int -7)                    
                              collect (if (zerop rest) lower (boole boole-ior lower #x80))
                              until (zerop rest)
                              do (setf int rest)))))
    (concatenate 'vector 
                 (reduce (alexandria:curry #'concatenate 'vector)
                         (mapcar (lambda (arg)
                                   (typecase arg
                                     (string (concatenate 'vector (vector +so-string+ (length arg))
                                                          (map 'vector #'char-code arg)))
                                     (vector (concatenate 'vector (vector +so-vector+ (length arg))
                                                          (apply #'serialize (map 'list #'identity arg))))
                                     ((integer 0 *)
                                      (concatenate 'vector (vector +so-pos-int+)
                                                   (serialize-int arg)))
                                     ((integer * -1)
                                      (concatenate 'vector (vector +so-neg-int+)
                                                   (serialize-int (abs arg))))
                                     (null (vector +so-null+))))
                                 args))
                 (vector +so-end-of-items+))))

(defun decode (string)
  (let ((char-to-code '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
                         0 0 0 0 0 0 0 0 0 0 62 63 0 0 0 0 
                         52 53 54 55 56 57 58 59 60 61 0 0 0 0 0 0 
                         0 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 
                         15 16 17 18 19 20 21 22 23 24 25 0 0 0 0 0 
                         0 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 
                         41 42 43 44 45 46 47 48 49 50 51))
        (result ""))
    (flet ((b-and (x y)
             (boole boole-and x y))
           (b-or (x y)
             (boole boole-ior x y))
           (add-char (char)
             (setf result (concatenate 'string result (string (code-char char))))))
      (map nil (let ((state 0)
                     (rem 0)) 
                 (lambda (char)
                   (let* ((val (elt char-to-code (char-code char))))
                     (case state
                       (0
                        (setf rem val))
                       (1
                        (add-char (b-or (ash (b-and val #x3) 6) rem))
                        (setf rem (ash val -2)))
                       (2
                        (add-char (b-or (ash (b-and val #xf) 4) rem))
                        (setf rem (ash val -4)))
                       (3
                        (add-char (b-or (ash val 2) rem))))
                     (when (> (incf state) 3)
                       (setf state 0)))))
           string))
    result))

(defun encode (string)
  (let ((code-to-char '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N 
                         #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b 
                         #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p 
                         #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 
                         #\4 #\5 #\6 #\7 #\8 #\9 #\* #\+))
        (result #()))
    (flet ((b-and (x y)
             (boole boole-and x y))
           (b-or (x y)
             (boole boole-ior x y))
           (add-char (char)
             (setf result (concatenate 'string result (string char)))))
      (map nil (let ((state 0)
                     (rem 0)) 
                 (lambda (code)
                   (case state
                     (0
                      (setf rem (b-and (ash code -6) #x3))
                      (add-char (elt code-to-char (b-and code #x3f))))
                     (1
                      (add-char (elt code-to-char (b-or (ash (b-and code #xf) 2) rem)))
                      (setf rem (b-and (ash code -4) #xf)))
                     (2
                      (add-char (elt code-to-char (b-or (ash (b-and code #x3) 4) rem)))
                      (add-char (elt code-to-char (b-and (ash code -2) #x3f)))))
                   (setf state (mod (1+ state) 3))))
           string))
    result))

(defun register-encoded-id-prefixes (&rest prefix/formats)
  (%request :post "/encodedIds/prefixes"
            :content-type "application/json"
            :content (cl-json:encode-json-to-string (mapcar (lambda (x)
                                                              (list (cons "prefix" (first x))
                                                                    (cons "format" (second x))))
                                                            prefix/formats))))



(defun next-encoded-id (prefix &optional (amount "1"))
  (with-input-from-string (stream (%request :post "/encodedIds"
                                            :content-type "application/json"
                                            :parameters (list (cons "prefix" prefix)
                                                              (cons "amount" amount))))
    (loop for line = (read-line stream nil :eof)
       until (eq line :eof)
       collect line)))

(defun add-json-statements (statements)
  (%request :post "/statements"
            :content-type "application/json"
            :content (cl-json:encode-json-to-string statements)))

(defun close-session ()
  (%request :post "/session/close"))

(defun commit ()
  (%request :post "/commit"))

(defun add-n-triple-statements (repo statements)
  (%request :post (format nil "/repositories/~A/statements" repo)
            :content-type "text/plain"
            :content statements))

(defun query (query)
  (cl-json:decode-json-from-string 
   (flexi-streams:octets-to-string 
    (%request :get (format nil "?query=~A" (hunchentoot::url-encode query))
              :accept "application/json"))))

(defun encode-n-triples (triples)
  (reduce (lambda (a i)
            (concatenate 'string a (encode-n-triple i)))
          triples
          :initial-value ""))

(defun encode-n-triple (triple)
  (format nil "~{~A ~} .~%" triple))


(defmacro with-repo-session ((catalog repo &key auto-commit) &body body)
  `(let ((*agraph-base-url* (create-repo-session ,catalog ,repo ,auto-commit)))
     (prog1
         (progn ,@body)
       (close-session))))


(defun statement-match (&key subject predicate object context)  
  (multiple-value-bind (body status)
      (%request :post "/statements/query"
                :accept "application/json"
                :parameters (append (when subject
                                      (list (cons "subj" subject)))
                                    (when predicate
                                      (list (cons "pred" predicate)))
                                    (when object
                                      (list (cons "obj" object)))
                                    (when context
                                      (list (cons "context" context)))))
    (when (= 200 status)
      (flexi-streams:octets-to-string body))))

(defun decode-json-triples (triples-json)
  (cl-json:decode-json-from-string triples-json))

(defun graph-size (catalog repo graph)
  (with-repo-session (catalog repo :auto-commit t)
    (%request :get "/size"
              :parameters (list (cons "context" (hunchentoot::url-encode graph))))))


