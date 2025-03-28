
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
                    (error "~a类型数据匹配~a构造器时错误" ',type-name tag)))))
    
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
         ,(accessor-for-hs-match-gen type-name parse-list)))))

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
           (let* (ignored-ls
                  (arg-ls
                    (mapcar #'(lambda (arg)
                                (cond
                                  ((symbolp arg)
                                   (if (eql '_ arg)
                                       (let ((new-arg (gensym)))
                                         (push new-arg ignored-ls) new-arg)
                                       arg))
                                  ((listp arg) (car arg))
                                  (t (error "在定义函数入口参数的时候无法解析~a~%" arg))))
                            args-match-ls))
                  (args-match-ls* (remove-if-not #'listp args-match-ls)))
             (values arg-ls ignored-ls args-match-ls*)))
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
                    ,(body-gen (cdr label-ls) arg-ls))))))

  (defmacro def-hs-func (fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配"
    (let* ((arg-ls (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) (cadar branches)))
           (arg-signature-ls (remove '_ (mapcar #'list type-signature arg-ls)
                                     :test #'(lambda (item x) (eql item (car x)))))
           (label-pairs (parse-pattern-clauses branches 'with-hs-let-match))
           (label-ls (mapcar #'car label-pairs))
           (label-defs (mapcar #'cdr label-pairs)))
      `(defun ,fun-name ,arg-ls ,(format nil "这是Common Lisp生成的~a函数,模拟Haskell" fun-name)
         (declare ,@arg-signature-ls)
         (labels ,label-defs ,(body-gen label-ls arg-ls)))))

  (defmacro def-hs-func* (fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配(可嵌套版本)"
    (let* ((arg-ls (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) (cadar branches)))
           (arg-signature-ls (remove '_ (mapcar #'list type-signature arg-ls)
                                     :test #'(lambda (item x) (eql item (car x)))))
           (label-pairs (parse-pattern-clauses branches 'with-hs-let-match*))
           (label-ls (mapcar #'car label-pairs))
           (label-defs (mapcar #'cdr label-pairs)))
      `(defun ,fun-name ,arg-ls ,(format nil "这是Common Lisp生成的~a函数,模拟Haskell" fun-name)
         (declare ,@arg-signature-ls)
         (labels ,label-defs ,(body-gen label-ls arg-ls)))))
  
  (defmacro def-hs-method (fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配"
    (let* ((arg-ls (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) (cadar branches)))
           (arg-signature-ls (mapcar #'(lambda (type arg)
                                         (if (eql '_ type) arg `(,arg ,type)))
                                     type-signature arg-ls))
           (label-pairs (parse-pattern-clauses branches 'with-hs-let-match))
           (label-ls (mapcar #'car label-pairs))
           (label-defs (mapcar #'cdr label-pairs)))
      `(defmethod ,fun-name ,arg-ls ,(format nil "这是Common Lisp生成的~a方法,模拟Haskell" fun-name)
         (declare ,@arg-signature-ls)
         (labels ,label-defs ,(body-gen label-ls arg-ls)))))

  (defmacro def-hs-method* (fun-name type-signature &rest branches)
    "模拟Haskell函数模式匹配(可嵌套版本)"
    (let* ((arg-ls (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) (cadar branches)))
           (arg-signature-ls (mapcar #'(lambda (type arg)
                                         (if (eql '_ type) arg `(,arg ,type)))
                                     type-signature arg-ls))
           (label-pairs (parse-pattern-clauses branches 'with-hs-let-match*))
           (label-ls (mapcar #'car label-pairs))
           (label-defs (mapcar #'cdr label-pairs)))
      `(defmethod ,fun-name ,arg-ls ,(format nil "这是Common Lisp生成的~a方法,模拟Haskell" fun-name)
         (declare ,@arg-signature-ls)
         (labels ,label-defs ,(body-gen label-ls arg-ls))))))

;;;;;; 以下为测试

; data List a = List {x::a, xs::List a} | []
(def-hs-data |List| ; Haskell的data关键字
  (|List| x (xs |List|))
  ([]))

; head ls = x ls -- 利用记录语法提取字段
; tail ls = xs ls
(defun head (xs) (hs-record-field xs 'x))
(defun tail (xs) (hs-record-field xs 'xs))

; (:) x xs = List x xs
(defun |:| (x xs) (|List| x xs))
; hs-ls = 1 : 2 : []
(let ((hs-ls (|:| 1 (|:| 2 ([])))))
  (format t "##代数数据类型~%")
  (format t "~a~%" (head hs-ls))
  (format t "~a~%" (head (tail hs-ls))))

; let ls = 1 : 2: []
;     (List a b) = ls
;     (List c _) = b
(let ((ls (|:| 1 (|:| 2 ([])))))
  (format t "##模式匹配1~%")
  (with-hs-let-match ((ls (|List| a b))
                       (b (|List| c _)))
    (format t "~a~%" a)
    (format t "~a~%" c)))

; 模式匹配错误（仅产生警告）
(let ((ls (reduce #'(lambda (acc x) (|:| x acc)) (reverse '(1 2 3 4 5))
                  :initial-value ([]))))
  (format t "##模式匹配2：错误的匹配~%")    
  (with-hs-let-match ((ls (|List| a))
                      (ls (|List| _ _ b)))
    (format t "~a~%" a)
    (format t "~a~%" b)))

; let ls = [1,2,3,4,5]
;     (List a _@(List _ _@(List c e))) = ls
;     (List f _) = e
; in ...
(let ((ls (reduce #'(lambda (acc x) (|:| x acc)) (reverse '(1 2 3 4 5))
                  :initial-value ([]))))
  (format t "##模式匹配3~%")
  (format t "~a~%" ls)
  (with-hs-let-match* ((ls (|List| a (_ (|List| _ (_ (|List| c e))))))
                       (e (|List| f _)))
    (format t "~a~%" a)
    (format t "~a~%" c)
    (format t "~a~%" f)))

(format t "##类型错误的构造函数")
(handler-case
    (let ((ls (|List| 1 2)))
      (format t "~a(实际上不会运行)~%" ls))
  (t (err) (warn "错误信息:~a~%" err)))

; case ls of
;   []         -> ...
;   (List a _) -> ...
(let ((ls (reduce #'(lambda (acc x) (|:| x acc)) (reverse '(1 2 3 4 5))
                  :initial-value ([]))))
  (format t "##case-of匹配1~%")
  (with-hs-case-of-match ls
    (([]) nil)
    ((|List| a _)
     (format t "头部=~a~%" a))))

; case ls of
;   []                    -> ...
;   (List a _@(List b _)) -> ...
(let ((ls (reduce #'(lambda (acc x) (|:| x acc)) (reverse '(1 2 3 4 5))
                  :initial-value ([]))))
  (format t "##case-of匹配2(嵌套版)~%")
  (with-hs-case-of-match* ls
    (([]) nil)
    ((|List| a (_ (|List| b _)))
     (format t "头部=~a,接下来=~a~%" a b))))

; case ls of
;   [] -> ...
(let ((ls (reduce #'(lambda (acc x) (|:| x acc)) (reverse '(1 2 3 4 5))
                  :initial-value ([]))))
  (format t "##case-of匹配未穷尽！~%")
  (handler-case
      (with-hs-case-of-match ls
        (([]) nil))
    (t (err) (warn "错误信息:~a~%" err))))

(let ((ls (reduce #'(lambda (acc x) (|:| x acc)) (reverse '(1 2 3 4 5))
                  :initial-value ([]))))
  (format t "##case-of兜底匹配_！~%")
  (with-hs-case-of-match ls
    (([]) nil)
    (_ (format t "我不知道它是什么，反正不是空列表!"))))

; test-ls :: List a -> b -> c
; test-ls (List a _) [] = ...
; test-ls [] []         = ...
(format t "##函数的模式匹配~%")
(def-hs-func test-ls (|List| _)
  (branch ((arg1 (|List| a _))
           (arg2 ([])))
          (format t "第一个列表头部:~a;第二个是空列表~a~%" a arg2))
  (branch ((arg3 ([]))
           (arg4 ([])))
          (format t "第一个是空列表哈哈~%")))
(test-ls (|List| 1 ([])) ([]))
(test-ls ([]) ([]))

; test-ls-even-len :: List a -> b
; test-ls-even-len []                  = ... -- 是偶数
; test-ls-even-len (List _ [])         = ... -- 是奇数
; test-ls-even-len (List _ (List _ a)) = test-ls-len a -- 剥掉两层

(format t "##函数的模式匹配2(嵌套模式匹配)~%")
(def-hs-func* test-ls-even-len (|List|)
  (branch ((ls ([])))
          (format t "长度是偶数呢~%"))
  (branch ((ls (|List| _ (a ([])))))
          (format t "长度是奇数呢~%"))
  (branch ((ls (|List| _ (a (|List| _ b)))))
          (test-ls-even-len b)))
(let* ((ls1 (reduce #'(lambda (acc x) (|:| x acc)) '(5 4 3 2 1) :initial-value ([])))
       (ls2 (|:| 0 ls1)))
  (test-ls-even-len ls1)
  (test-ls-even-len ls2))
