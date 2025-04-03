
# 这是什么？
这是一个用Common Lisp宏模拟Haskell机制的尝试

## 它有什么/能做什么？
"hs.lisp"提供了以下宏
1. `def-hs-data`：模拟Haskell代数数据类型（Algebraic Data Type），记录（record）语法
2. `with-hs-let-match(*)`：模拟Haskell的let模式匹配，带星号为嵌套模式匹配
3. `with-hs-case-of-match(*)`：模拟Haskell的case of模式匹配，带星号为嵌套模式匹配
4. `def-hs-func(*)`：模拟Haskell的函数定义，带星号为入口参数模式匹配可嵌套表示
5. `def-hs-class(*)`：模拟Haskell的类型类（Type Class）定义，带星号为方法的定义中的入口参数模式匹配可嵌套表示
6. `def-hs-instance(*)`：模拟Haskell的实例（instance）定义，带星号为方法的定义中的入口参数模式匹配可嵌套表示

"main.lisp"进一步展示了如何实现以下宏：
1. `with-hs-do`：模拟Haskell中的do块，基于`|Moand|`类型类的定义
2. `with-hs-list-comprehension`：模拟Haskell中的列表推导式，基于do块和函数`|guard|`的定义

## 它没有什么？
没有实现
1. 与原生Haskell相当的类型检查、推断的能力；因此它不会像Haskell编译器一样阻止使用者犯类型上的错误；
2. 惰性求值；因此无法构造特殊的结构，例如无穷列表等；
3. 柯里化；因此无法令人满意地产生部分应用函数；
4. 其它与真正Haskell语法区别的种种细节；所以你并不能真的用它来编写Haskell程序。

# 依赖
暂时没有。

# 运行示例方法
在repl中读取和加载系统
``` Common Lisp
CL-USER> (load "lisp-hs-simulator.asd")
T
CL-USER> (asdf:load-system :lisp-hs-simulator)
T
```

在此之后：你可以在"main.lisp"文件里面诸行运行；或者也可以一次性加载整个"main.lisp"文件：

``` Common Lisp
CL-USER> (asdf:load-system "lisp-hs-simulator/demonstration")
```

如果你想测试生成的函数/方法，建议这样做：

``` Common Lisp
CL-USER> (in-package :hs)
#<PACKAGE "HS">
HS> 
```

