
(defpackage :hs
  (:use :cl)
  (:export
   :def-hs-data
   :hs-record-field
   :with-hs-let-match
   :with-hs-let-match*
   :with-hs-case-of-match
   :with-hs-case-of-match*
   :def-hs-func
   :def-hs-func*
   :def-hs-class
   :def-hs-class*
   :def-hs-instance
   :def-hs-instance*))
(in-package :hs)

(defmacro def-hs-data (type-name &rest clauses)
  (labels ((doc-gen (type-name)
             (format nil "这是CL生成的模拟Haskell类型(~a)" type-name))
           (parse-args (args)
             "一个值构造器中的参数进行解析，返回(参数名 :alias 分配符号 :type 类型)"
             (mapcar #'(lambda (x)
                         (cond ((symbolp x) (list x :alias (gensym) :type nil))
                               ((listp x)
                                (handler-case
                                    (destructuring-bind (arg-name type) x
                                      (list arg-name :alias (gensym) :type type))
                                  (t (err)
                                    (error "参数解析错误,无法解析~a:~a" x err))))
                               (t (error "解析参数名称错误"))))
                     args))
           (parse-clause (clause)
             "从一行语句中分离出构造器名称和参数列表，然后送给parse-args解析"
             (destructuring-bind (name &rest args) clause
               (cons name (parse-args args))))
           (slots-gen (args-parse)
             "将解析后的列表转换成slot定义语句"
             (mapcar #'(lambda (x)
                         (let ((alias (getf (cdr x) :alias))
                               (type (getf (cdr x) :type)))
                           (if (null type)
                               (list alias :initarg (intern (symbol-name alias) :keyword))
                               (list alias :initarg (intern (symbol-name alias) :keyword)
                                           :type type))))
                     args-parse))

           (defclass-gen (type-name slots-clause doc-str)
             "根据类型名、slot语句，文档，生成defclass的语句"
             (declare (symbol type-name) (string doc-str))
             `(defclass ,type-name ()
                ,(cons '(tag :initarg :tag :accessor tag :type symbol) slots-clause)
                (:documentation ,doc-str)))
           (constructor-fun-gen (type-name constru-clause)
             "生成构造函数"
             (destructuring-bind (con-name &rest slots-ls) constru-clause
               (let* ((slot-name-ls `,(mapcar #'car slots-ls))
                      (slot-init-args `,(mapcar #'(lambda (ls) (getf (cdr ls) :initarg)) slots-ls))
                      (slots-with-type `,(remove-if-not
                                          #'(lambda(x) (getf (cdr x) :type)) slots-ls))
                      (type-declares `,(mapcar
                                        #'(lambda (x) `(,(getf (cdr x) :type) ,(car x)))
                                        slots-with-type))
                      (slot-name&init-args (mapcan #'list slot-init-args slot-name-ls)))
                 `(defun ,con-name ,slot-name-ls
                    (declare ,@(if (null type-declares) nil type-declares))
                    ,(format nil "这是Common Lisp生成的~a类型~a构造器" type-name con-name)
                    (make-instance ',type-name ,@slot-name&init-args :tag ',con-name)))))
           (constructor-fun-defs-gen (type-name slots-describe)
             "生成所有的构造函数定义语句"
             (mapcar #'(lambda (fun-clause)
                         (constructor-fun-gen type-name fun-clause))
                     slots-describe))
           (record-field-accessor-gen (type-name parse-list)
             "生成记录语法的槽访问器"
             `(defmethod hs-record-field ((data ,type-name) &rest fileds)
                "这是由Common Lisp生成的函数,可用于模拟记录语法"
                (let* ((tag (tag data))
                       (constru-info (cdr (assoc tag ',parse-list)))
                       (field-alias-ls
                         (mapcar #'(lambda (field)
                                     (getf (cdr (assoc field constru-info)) :alias))
                                 fileds))
                       (field-value-ls
                         (mapcar #'(lambda (alias) (slot-value data alias)) field-alias-ls)))
                  (apply #'values field-value-ls))))
           (accessor-for-hs-match-gen (type-name parse-list)
             "生成用于haskell模式匹配的访问器"
             `(defmethod accessor-for-hs-match ((data ,type-name) tag &rest indeces)
                (if (eql tag (tag data))
                    (let* ((constru-info (cdr (assoc tag ',parse-list)))
                           (field-len (length constru-info))
                           (field-alias-ls
                             (mapcar #'(lambda (indx)
                                         (cond
                                           ((>= indx field-len)
                                            (warn "模式匹配越界,试图访问第~a个字段(实际长度~a)"
                                                  indx field-len))
                                           ((< indx 0)
                                            (warn "不该发生,模式匹配试图访问第~a个字段(实际长度~a)"
                                                  indx field-len))
                                           (t (getf (cdr (nth indx constru-info)) :alias))))
                                     indeces))
                           (field-value-ls
                             (mapcar #'(lambda (alias) (if (not (null alias))
                                                           (slot-value data alias)))
                                     field-alias-ls)))
                      (values field-value-ls field-len))
                    (error "~a类型数据匹配~a构造器时错误" ',type-name tag))))
           (require-hs-all-fields-gen (type-name parse-list)
             "生成用于Haskell中show的默认实现的访问器"
             `(defmethod require-hs-all-fields ((data ,type-name))
                "这是由Common Lisp生成的函数，直接获取代数数据类型所有字段值"
                (let* ((tag (tag data))
                       (constru-info (cdr (assoc tag ',parse-list)))
                       (field-alias-ls (mapcar #'(lambda (x) (getf (cdr x) :alias)) constru-info))
                       (field-value-ls (mapcar #'(lambda (alias) (slot-value data alias))
                                               field-alias-ls)))
                  (values field-value-ls tag)))))
    
    (let* ((parse-list `,(mapcar #'parse-clause clauses))
           (slots `,(slots-gen (reduce #'append (mapcar #'cdr parse-list))))
           (slots-describe `,(mapcar
                              #'(lambda (ls)
                                  "生成具有描述性的slot信息"
                                  (destructuring-bind (name &rest rest) ls
                                    (cons name (slots-gen rest))))
                              parse-list))
           (doc-str `,(doc-gen type-name)))
      (declare (string doc-str))
      `(progn
         ;; 定义common lisp的class
         ,(defclass-gen type-name slots doc-str)
         ;; 定义函数
         ,@(constructor-fun-defs-gen type-name slots-describe)
         ,(record-field-accessor-gen type-name parse-list)
         ,(accessor-for-hs-match-gen type-name parse-list)
         ,(require-hs-all-fields-gen type-name parse-list)))))

(defmacro with-hs-let-match (value-bind-pair-ls &body body)
  "模拟Haskell中的let模式匹配(多行)"
  (labels ((parse-bind-clause (clause body)
             (destructuring-bind (value constru-clause) clause
               (destructuring-bind (constru &rest name-ls) constru-clause
                 (let* ((indeces (loop for elem in name-ls
                                       for indx from 0
                                       unless (eql elem '_)
                                         collect indx))
                        (len (length name-ls)))
                   `(multiple-value-bind ,(remove '_ name-ls)
                        (multiple-value-bind (value-ls full-len)
                            (accessor-for-hs-match ,value ',constru ,@indeces)
                          (when (< ,len full-len)
                            (warn "匹配只给了~a个字段，应有~a个字段" ,len full-len))
                          (apply #'values value-ls))
                      ,@body)))))
           (parse-bind-clauses (clauses body)
             (cond
               ((null clauses) `(progn ,@body))
               ((eql 1 (length clauses)) (parse-bind-clause (car clauses) body))
               (t (parse-bind-clause (car clauses)
                                     (list (parse-bind-clauses (cdr clauses) body)))))))
    (if (null value-bind-pair-ls)
        `(progn ,@body)
        (parse-bind-clauses value-bind-pair-ls  body))))

(defun flatten-nested-clause (clause)
  "解析类似匹配的嵌套语句~@
  (value1 (constru1 value2 (value3 (constru2 _ (_ (constru 3))))))"
  (destructuring-bind (value constru-clause) clause
    (destructuring-bind (constru &rest name-ls) constru-clause
      (multiple-value-bind (name-ls* sub-name-ls)
          (loop with new-name
                for name in name-ls
                if (listp name)
                  do (if (eql '_ (car name)) (setf new-name (gensym))
                         (setf new-name (car name)))
                  and collect new-name into name-ls*
                  and collect (cons new-name (cdr name)) into sub-name-ls
                else if (symbolp name)
                       collect name into name-ls*
                else do
                  (error "~a解析错误，非符号或列表" name)
                finally (return (values name-ls* sub-name-ls)))
        (if (null sub-name-ls)
            `((,value ,(cons constru name-ls*)))
            (let ((sub-name-ls* (mapcar #'flatten-nested-clause sub-name-ls)))
              `((,value ,(cons constru name-ls*)) . ,(car sub-name-ls*))))))))

(defmacro with-hs-let-match* (value-bind-pair-ls &body body)
  "模拟Haskell中的let模式匹配(多行,可嵌套)"
  `(with-hs-let-match ,(mapcan #'flatten-nested-clause value-bind-pair-ls)
     ,@body)) ; 需要把嵌套列表铺平即可

(labels ((parse-pattern-clause (value clause additional-block macro-name)
           (destructuring-bind (constru-clause &rest branch) clause
             (cond
               ((symbolp constru-clause)
                (if (eql '_ constru-clause)
                    `(progn ,@branch)
                    `(let ((,constru-clause ,value))
                       ,@branch)))
               ((listp clause)
                `(handler-case
                     (,macro-name ((,value ,constru-clause)) ,@branch)
                   (t (err) (declare (ignore err))
                     ,additional-block)))
               (t (error "在case-of解析中解析失败:~a不是符号或列表" clause)))))
         (parse-pattern-clauses (value clauses &optional (macro-name 'with-hs-let-match))
           (if (null clauses) `(error "模式匹配未穷尽!~%")
               (parse-pattern-clause value (car clauses)
                                     (parse-pattern-clauses value (cdr clauses) macro-name)
                                     macro-name))))
  (defmacro with-hs-case-of-match (value &rest pattern-clauses)
    "模拟Haskell中的case of模式匹配"
    (parse-pattern-clauses value pattern-clauses 'with-hs-let-match))
  (defmacro with-hs-case-of-match* (value &rest pattern-clauses)
    "模拟Haskell中的case of模式匹配(嵌套版)"
    (parse-pattern-clauses value pattern-clauses 'with-hs-let-match*)))

(labels ((parse-args-matchs (args-match-ls)
           (loop with new-name
                 for arg in args-match-ls
                 if (symbolp arg)
                   if (eql '_ arg)
                     do (setf new-name (gensym))
                         and collect new-name into ignored-ls
                         and collect new-name into arg-ls
                 else
                   collect arg into arg-ls
                 else if (listp arg)
                        do (setf new-name (car arg))
                        and if (eql '_ new-name)
                              do (setf new-name (gensym))
                              and collect new-name into arg-ls
                              and collect (cons new-name (cdr arg)) into args-match-ls*
                 else
                   collect new-name into arg-ls
                   and collect arg into args-match-ls*
                 else
                   do (error "在定义函数入口参数的时候无法解析~a~%" arg)
                 finally (return (values arg-ls ignored-ls args-match-ls*))))
         (parse-pattern-clause (asigned-name clause match-macro)
           (destructuring-bind (branch-key args-match &body body) clause
             (ecase branch-key
               (branch
                (multiple-value-bind (arg-ls ignored-ls args-match*)
                    (parse-args-matchs args-match)
                  `(,asigned-name ,arg-ls (declare (ignore ,@ignored-ls))
                                  (,match-macro ,args-match* ,@body))))
               (until (error "非法的分支结构")))))
         (parse-pattern-clauses (clauses &optional (match-macro 'with-hs-let-match))
           (let (label-name)
             (loop for clause in clauses
                   do (setf label-name (gensym))
                   collect `(,label-name . ,(parse-pattern-clause label-name clause
                                                                  match-macro)))))
         (body-gen (label-ls arg-ls)
           (if (null label-ls) `(error "模式匹配未穷尽(函数定义)!~%")
               `(handler-case (,(car label-ls) ,@arg-ls)
                  (t (err) (declare (ignore err))
                    ,(body-gen (cdr label-ls) arg-ls)))))

         (form-gen (fun-name type-signature form-type use-macro-name branches
                    &optional (check-method #'(lambda (x) (declare (ignore x)) t)))
           "根据上述的帮助函数统一生成函数/方法的表达式"
           (let* ((arg-ls (mapcar #'(lambda (x) (declare (ignore x)) (gensym))
                                  (cadar branches)))
                  (label-pairs (parse-pattern-clauses branches use-macro-name))
                  (label-ls (mapcar #'car label-pairs))
                  (label-defs (mapcar #'cdr label-pairs)))
             (ecase form-type
               (func
                (let ((arg-signature-ls (remove '_ (mapcar #'list type-signature arg-ls)
                                                :test #'(lambda (item x) (eql item (car x))))))
                  `(defun ,fun-name ,arg-ls
                     ,(format nil "这是Common Lisp生成的~a函数,模拟Haskell" fun-name)
                     (declare ,@arg-signature-ls)
                     (labels ,label-defs ,(body-gen label-ls arg-ls)))))
               (method
                (let ((arg-signature-ls (mapcar #'(lambda (type arg)
                                                    (if (eql '_ type) arg `(,arg ,type)))
                                                type-signature arg-ls)))
                  `(defmethod ,fun-name ,arg-signature-ls
                     ,(format nil "这是Common Lisp生成的~a方法,模拟Haskell" fun-name)
                     (every ,check-method (list ,@arg-ls))
                     (labels ,label-defs ,(body-gen label-ls arg-ls)))))))))

  (defmacro def-hs-func (fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配"
    (form-gen fun-name type-signature 'func 'with-hs-let-match branches))

  (defmacro def-hs-func* (fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配(可嵌套版本)"
    (form-gen fun-name type-signature 'func 'with-hs-let-match* branches))
  
  (defmacro def-hs-method (check-func fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配"
    (form-gen fun-name type-signature 'method 'with-hs-let-match branches check-func))

  (defmacro def-hs-method* (check-func fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配(可嵌套版本)"
    (form-gen fun-name type-signature 'method 'with-hs-let-match branches check-func)))

;;;; 以下模拟Haskell类型

(defparameter *class-registry* (make-hash-table))
; key: ClassName -> value: (:funcs (:implmted (func1 func2 func3)
;                                   :not-implmted (func4))
;                           :instances ())

(labels ((generate-hs-class-form
          (class-name func-ls &optional (macro-name 'def-hs-method))
            (let* ((implmted-fun-ls (remove-if-not #'(lambda (x) (> (length x) 2)) func-ls))
                   (not-implmted-fun-ls (remove-if #'(lambda (x) (> (length x) 2)) func-ls))
                   (implmted-names (mapcar #'car implmted-fun-ls))
                   (not-implmted-names (mapcar #'car not-implmted-fun-ls))
                   (check-method `#'(lambda (arg)
                                      (unless
                                          (some #'(lambda (type) (typep arg type))
                                                (getf (gethash ',class-name *class-registry*)
                                                      :instances))
                                        (error "似乎不是类型类~a的实例哦." ',class-name))
                                      t)))
              (setf (gethash class-name *class-registry*)
                    `(:funcs (:implmted ,implmted-names
                              :not-implmted ,not-implmted-names)
                      :instances ()))
              `(progn ,@(mapcar #'(lambda (x)
                                    `(,macro-name
                                      ,check-method
                                      ,@`(,@x (branch ,(mapcar #'(lambda (x)
                                                                   (declare (ignore x)) '_)
                                                               (cadr x))))))
                                not-implmted-fun-ls)
                      ,@(mapcar #'(lambda (x) `(,macro-name ,check-method ,@x))
                                implmted-fun-ls)))))
  
  (defmacro def-hs-class (class-name &rest func-ls)
    (generate-hs-class-form class-name func-ls 'def-hs-method))
  (defmacro def-hs-class* (class-name &rest func-ls)
    (generate-hs-class-form class-name func-ls 'def-hs-method*)))

;(def-hs-class |Class-name|
;  (func1 () ((branch ...) (branch ...)))
;  (func2 () (...))
;  (func3 () (...))
;  (func4 ())) ;第4个不给默认实现

; class ClassName where
;   func1 = ...
;   func2 = ...

(labels ((generate-hs-instance-form
           (class-name type-name func-form-ls &optional (macro-name 'def-hs-method))
           (let ((fun-ls (mapcar #'car func-form-ls))
                 (registry (gethash class-name *class-registry*)))
             (when (null registry) (error "没有类型类~a!~%" class-name))
             (let* ((instance-ls (getf registry :instances))
                    (funcs (getf registry :funcs))
                    (implmted-funcs (getf funcs :implmted))
                    (not-implmted-funcs (getf funcs :not-implmted)))
               (unless (every #'(lambda (f) (member f fun-ls)) not-implmted-funcs)
                 (error "~a类型作为类~a实例化时下列函数其中的某些缺少定义:~{~a. ~}~%"
                        type-name class-name not-implmted-funcs))
               (unless (every #'(lambda (f) (or (member f implmted-funcs)
                                                (member f not-implmted-funcs)))
                              fun-ls)
                 (error "~a类型的某些方法不是~a类的方法!您给出~{~a. ~};~@
                         实际上只有~{~a. ~}(已默认实现); ~{~a. ~}(无默认实现)."
                        type-name class-name fun-ls implmted-funcs not-implmted-funcs))
               (unless (member type-name instance-ls)
                 (push type-name (getf registry :instances))) ; 注册实例
               `(progn ,@(mapcar
                          #'(lambda (x)
                              `(,macro-name #'(lambda (_) (declare (ignore _)) t) ,@x))
                          func-form-ls))))))
  
  (defmacro def-hs-instance (class-name type-name &rest func-form-ls)
    (generate-hs-instance-form class-name type-name func-form-ls 'def-hs-method))
  (defmacro def-hs-instance* (class-name type-name &rest func-form-ls)
    (generate-hs-instance-form class-name type-name func-form-ls 'def-hs-method*)))

;(def-hs-instance |Class-name| |Type-name|
;  (func1 () ((branch ...) (branch ...)))
;  (func2 () (...)) ; func3有默认实现无需定义
;  (func4 () (...)))

; instance ClassName TypeName where
;   func1 = ...
;   func2 = ...
;   func4 = ...

