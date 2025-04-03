
(asdf:defsystem :lisp-hs-simulator
  :description "Common Lisp宏模拟Haskell"
  :components ((:file "hs"))
  :depends-on ())

(asdf:defsystem "lisp-hs-simulator/demonstration"
  :description "演示示例"
  :depends-on (:lisp-hs-simulator)
  :components ((:file "main")))
