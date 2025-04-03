
;(defpackage :demonstration
;  (:use :cl :hs)
;  (:export))

;(in-package :demonstration)
(in-package :hs)

;;;;;; 以下为测试

; data List a = List {x::a, xs::List a} | []
(def-hs-data |List| ; Haskell的data关键字
  (|List| x (xs |List|))
  ([]))

; head ls = x ls -- 利用记录语法提取字段
; tail ls = xs ls
(defun |head| (xs) (hs-record-field xs 'x))
(defun |tail| (xs) (hs-record-field xs 'xs))

; (:) x xs = List x xs
(defun |:| (x xs) (|List| x xs))
; hs-ls = 1 : 2 : []
(let ((hs-ls (|:| 1 (|:| 2 ([])))))
  (format t "##代数数据类型~%")
  (format t "~a~%" (|head| hs-ls))
  (format t "~a~%" (|head| (|tail| hs-ls))))

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
    (_ (format t "我不知道它是什么，反正不是空列表!~%"))))

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

(format t "##Show类型类及其默认实现~%")
; data Maybe a = Nothing | Just a
(def-hs-data |Maybe|
  (|Just| a)
  (|Nothing|))

; data Either a b = Left a | Right b
(def-hs-data |Either|
  (|Left| left)
  (|Right| right))

(def-hs-class |Show|
  (|show| (_)
    (branch (x)
      (cond
        ((or (numberp x) (stringp x)) (format nil "~a" x))
        (t (multiple-value-bind (field-values constructor-name)
               (require-hs-all-fields x)
             (if (null field-values)
                 (format nil "~a" constructor-name)
                 (format nil "(~a~{ ~a~})" constructor-name
                         (mapcar #'(lambda (v) (|show| v))
                                 field-values)))))))))

(def-hs-instance |Show| number)
(def-hs-instance |Show| string)

(def-hs-instance |Show| |List|
  (|show| (|List|)
    (branch ((_ ([])))
            "[]")
    (branch ((hs-ls (|List| _ _)))
            (labels ((handler (hs-ls &optional (headp nil))
                       (cond
                         ((eql '[] (tag hs-ls)) "]")
                         (t (concatenate 'string (if headp "[" ",")
                                         (|show| (|head| hs-ls)) (handler (|tail| hs-ls)))))))
              (handler hs-ls t)))))
(def-hs-instance |Show| |Maybe|)
(let* ((ls (reduce #'(lambda (acc x) (|:| x acc)) '(5 4 3 2 1) :initial-value ([])))
       (just-ls (|Just| ls))
       (just-nothing (|Just| (|Nothing|))))
  (format t "~a~%" (|show| ls))
  (format t "~a~%" (|show| just-ls))
  (format t "~a~%" (|show| just-nothing)))

(format t "## 函子类型类~%")
(def-hs-class |Functor|
  (|fmap| (_ _)))
(def-hs-instance |Functor| |List|
  (|fmap| (_ |List|)
    (branch (_ (_ ([])))
            ([]))
    (branch (func (_ (|List| x xs)))
            (|List| (funcall func x) (|fmap| func xs)))))
(def-hs-instance |Functor| |Maybe|
  (|fmap| (_ |Maybe|)
          (branch (_ (_ (|Nothing|)))
                  (|Nothing|))
          (branch (func (_ (|Just| a)))
                  (|Just| (funcall func a)))))

; instance Functor (Either a) where 
;   fmap _ (Left x) = Left x
;   fmap func (Right x) = Right $ func x
(def-hs-instance |Functor| |Either|
  (|fmap| (_ |Either|)
          (branch (_ (_ (|Left| x)))
                  (|Left| x))
          (branch (func (_ (|Right| x)))
                  (|Right| (funcall func x)))))

(let* ((ls (reduce #'(lambda (acc x) (|:| x acc)) '(5 4 3 2 1) :initial-value ([])))
       (func #'1+)
       (ls* (|fmap| func ls))
       (just (|Just| 1))
       (just* (|fmap| func just))
       (left (|Left| 1)) (left* (|fmap| func left))
       (right (|Right| 1)) (right* (|fmap| func right)))
  (format t "~a~%" (|show| ls))
  (format t "~a~%" (|show| ls*))
  (format t "~a~%" (|show| just))
  (format t "~a~%" (|show| just*))
  (format t "fmap之前:~a,fmap之后:~a~%" (|show| left) (|show| left*))
  (format t "fmap之前:~a,fmap之后:~a~%" (|show| right) (|show| right*)))

(format t "## 应用函子类型类~%")
(def-hs-class |Applicative|
  (|pure| (_ _))
  (<*> (_ _)))
(def-hs-instance |Applicative| |Maybe|
  (|pure| (_ |Maybe|)
          (branch (x _)
                  (|Just| x)))
  (<*> (|Maybe| |Maybe|)
       (branch ((_ (|Nothing|)) _)
               (|Nothing|))
       (branch ((_ (|Just| func)) something)
               (|fmap| func something))))

(def-hs-func ++* (|List| |List|)
  (branch ((_ ([])) xs)
          xs)
  (branch ((_ (|List| x xs*)) xs)
          (|List| x (++* xs* xs))))
(def-hs-func |concat| (|List|)
  (branch ((_ ([])))
          ([]))
  (branch ((_ (|List| x xs)))
          (++* x (|concat| xs))))

(def-hs-instance |Applicative| |List|
  (|pure| (_ |List|)
          (branch (x _)
                  (|List| x ([]))))
  (<*> (|List| |List|)
       (branch (fs xs)
               (|concat| (|fmap| #'(lambda (f) (|fmap| f xs)) fs)))))
(let* ((just-1+ (|Just| #'1+))
       (just (|Just| 1))
       (just* (|<*>| just-1+ just)))
  (format t "~a~%" (|show| just))
  (format t "~a~%" (|show| just*)))

(format t "## 单子类型类~%")
(def-hs-class |Monad|
  (|return| (_ _))
  (>>= (_ _))
  (|fail| (_))
  (>> (_ _)
      (branch (m n)
              (>>= m (lambda (_) (declare (ignore _)) n)))))
(def-hs-instance |Monad| |Maybe|
  (|return| (_ |Maybe|)
            (branch (x _)
                    (|Just| x)))
  (>>= (|Maybe| _)
       (branch ((_ (|Nothing|)) _)
               (|Nothing|))
       (branch ((_ (|Just| x)) func)
               (funcall func x)))
  (|fail| (_)
          (branch (_)
                  (|Nothing|))))

(def-hs-instance |Monad| |List|
  (|return| (_ |List|)
            (branch (x _)
                    (|List| x ([]))))
  (>>= (|List| _)
       (branch (xs func)
               (|concat| (|fmap| func xs))))
  (|fail| (_)
          (branch (_) ([]))))
(let* ((hs-ls (|:| 3 (|:| 4 (|:| 5 ([])))))
       (func #'(lambda (x) (|:| x (|:| (- x) ([])))))
       (hs-ls* (>>= hs-ls func)))
  (format t "~a~%" (|show| hs-ls))
  (format t "~a~%" (|show| hs-ls*)))

;; do记号
(format t "## do记号~%")
(defmacro with-hs-do (&rest hs-do-clauses)
  (labels ((parse-clause (clause rest)
             (case (car clause)
               (<- (destructuring-bind (x m) (cdr clause)
                     `(>>= ,m (lambda (,x) ,rest))))
               (otherwise `(>> ,clause ,rest))))
           (parse-clauses (hs-do-clauses)
             (if (eql 1 (length hs-do-clauses)) (car hs-do-clauses)
                 (parse-clause (car hs-do-clauses)
                               (parse-clauses (cdr hs-do-clauses))))))
    (parse-clauses hs-do-clauses)))

; do
;   x <- Just 2
;   y <- Just "!"
;   Just (show x ++ y)
(let ((just (with-hs-do
              (<- x (|Just| 2))
              (<- y (|Just| "!"))
              (|Just| (concatenate 'string (|show| x) y)))))
  (format t "~a~%" (|show| just)))

(format t "## guard和列表推导式~%")
(def-hs-func |guard| (boolean)
  (branch (flag)
          (if flag
            (|List| nil ([]))
            ([]))))
(defmacro with-hs-list-comprehension (result &rest clauses)
  (labels ((parse-clause (clause)
             (case (car clause)
               (<- clause)
               (otherwise `(|guard| ,clause)))))
    `(with-hs-do ,@(mapcar #'parse-clause clauses)
       (|return| ,result ([])))))

(let* ((a-ls (|:| 1 (|:| 2 (|:| 3 ([])))))
       (b-ls (|:| 4 (|:| 5 (|:| 6 ([])))))
       (a*b-ls (with-hs-list-comprehension (* a b)
                 (<- a a-ls)
                 (<- b b-ls)
                 (eql 5 (+ a b)))))
  (format t "结果是:~a~%" (|show| a*b-ls)))
