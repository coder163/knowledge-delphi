#### 1. 前言

 本文档主要是为Delphi开发人员提供一个源代码书写标准，以及程序和文件的命名标准，使他们在编程时有一致格式可遵循。这样，每个编程人员编写的代码能够被其他人理解。

--------------------------------------------------------------------------------

#### 2. 源程序书写规范

##### 2.1. 2.1.通用源代码格式规则

##### 2.2. 缩进

缩进就是每级间有两个空格。不要在源代码中放置制表符。这是因为，制表符的宽度随着不同的设置和代码管理实用程序(打印、文档及版本控制等)而不同。

通过使用Tools|Environment 菜单，在Environment Options 对话框的General页上，不要选中Use Tab Character 和Optional Fill 复选框，这样，制表符就不会被保存。

##### 2.3. 边距

边距设置为80个字符。源代码一般不会因写一个单词而超过边距，但本规则比较灵活。只要可能，长度超过一行的语句应当用逗号或运算符换行。换行后，应缩进两个字符。

begin...end 语句

begin 语句必须单独占一行。例如，下面第一行是错误的，而第二行正确：

```pascal

for i:=0 to 10 do begin // 错, begin 与f o r 在同一行

for i:=0 to 10 do // 对, begin 在另外一行中
begin

```

本规则的一个特殊情况是，当begin 为else 语句的一部分时，例如：

```pascal
if some statement = then
begin
 . . .
end
else begin
 Some Other Statement;
end;
```

> 注意：end 语句总单独一行。当begin 不为else 语句的一部分时，相应的end 语句与begin 语句的缩进量相同。

##### 2.4. 注释

我们通常使用“{...}”类型的块注释，以前的“(*...*)”类型的块注释用于临时注释掉暂不使用的代码，从Delphi 2开始支持“//”行注释，如果决定不在支持Delphi 2.0以下的版本，可以使用“//”注释。

Object Pascal语句格式语句书写规范与用法

##### 2.5. 括号

在左括号与下一字符之间没有空格。同样，右括号与前一字符也没有空格。下面的例子演示了正确与不正确的空格。

```pascal
CallProc( Aparameter ); // 错!
CallProc(Aparameter); // 正确!

```

不要在语句中包含多余的括号。在源代码中，括号只有在确实需要时才使用。下面的例子演示了正确与不正确用法：

```pascal
if (I=42) then // 错，括号是多余的
if (I=42) or (J=42) then // 正确，必须使用括号

```

##### 2.6. 保留字和关键字

Object Pascal 语言的保留字和关键字总是完全的小写。

##### 2.7. 类型

###### 2.7.1. 大小写规则

类型标识符是保留字，应当全部小写。Win32 API 类型常常全部大写，并且遵循诸如Windows.pas或其他API单元中关于特定类型名的规则。对于其他变量名，第一个字母应大写，其他字母则大小写交错。下面是一些例子：

```pascal
var
 MyString: string; // 保留字
 WindowsHandle: HWND; // Win32 API 类型
 I: Integer; //在System单元中引入的类型标识

```
###### 2.7.2. 浮点型

不鼓励使用Real类型，因为它只是为了与老的Pascal代码兼容而保留的。通常情况下，对于浮点数应当使用Double。Double可被处理器优化，是IEEE定义的标准的数据格式。当需要比Double提供的范围更大时，可以使用Extend。Extend是intel专用的类型，Java不支持。当浮点变量的物理字节数很重要时(可能使用其他语言编写DLL)，则应当使用Single。

Variant和OleVariant

一般不建议使用Variant和OleVariant。但是，当数据类型只有在运行期才知道时(常常是在COM和数据库应用的程序中)，这两个类型对编程就有必要。当进行诸如自动化ActiveX控件的COM编程时，应当使用OleVariant；而对于非COM编程，则应当使用Variant。这是因为，Variant能够有效地保存Delphi的原生字符串，而OleVariant则将所有字符串转换为OLE字符串(即WideChar字符串)，且没有引用计数功能。

##### 2.8. 语句

###### 2.8.1. If 语句

在if/then/else语句中，最有可能执行的情况应放在then子句中，不太可能的情况放在else子句中。为了避免出现许多if语句，可以使用case语句代替。如果多于5级，不要使用if语句。请改用更清楚的方法。不要在if语句中使用多余的括号。

如果在if语句中有多个条件要测试，应按照计算的复杂程度从右向左排。这样，可以使代码充分利用编译器的短路估算逻辑。例如，如果Condition1比Condition2快，Condition2比Condition3快，则if语句一般应这样构造：

```pascal
if Condition1 and Condition2 and Condition3 then
```

如果Condition3为False的机会很大，利用短路估算逻辑，我们也可以将Condition3放在最前面：

```pascal
if Condition3 and Condition1 and Condition2 then
```

###### 2.8.2. case 语句

概述

case语句中每种情况的常量应当按数字或字母的顺序排列。每种情况的动作语句应当简短且通常不超过4 - 5 行代码。如果动作太复杂，应将代码单独放在一个过程或函数中。Case语句的else子句只用于默认情况或错误检测。

格式

case语句遵循一般的缩进和命名规则。


###### 2.8.3. while 语句

建议不要使用Exit过程来退出while循环。如果需要的话，应当使用循环条件退出循环。所有对while循环进行初始化的代码应当位于while入口前，且不要被无关的语句隔开。任何业务的辅助工作都应在循环后立即进行。

###### 2.8.4. for 语句

如果循环次数是确定的，应当用for语句代替while语句。

###### 2.8.5. repeat 语句

repeat语句类似于while循环，且遵循同样的规则。


###### 2.8.6. with 语句

概述

with语句应小心使用。要避免过度使用with语句，尤其是在with语句中使用多个对象或记录。例如：

```pascal
with Record1,Record2 do
```

 这些情况很容易迷惑编程人员，且导致调试困难。

格式

with语句也遵循本章关于命名和缩进的规则。

结构化异常处理

概述

 异常处理主要用于纠正错误和保护资源。这意味着，凡是分配资源的地方，都必须使用try...finally来保证资源得到释放。不过，如果是在单元的初始/结束部分或者对象的构造器/析构器中来分配/释放资源则例外。

###### 2.8.7. try...finally的用法

在可能的情况下，每个资源分配应当与try...finally结构匹配，例如，下面代码可能导致错误：

```pascal
SomeClass1 := TSomeClass.Create;
SomeClass2 := TSomeClass.Create;
try
 { do some code }
finally
 SomeClass1.Free;
 SomeClass2.Free;
end;
```

上述资源分配的一个安全方案是：

```pascal
SomeClass1 := TSomeClass.Create;
try
 SomeClass2 := TSomeClass.Create;
 try
   { do some code }
 finally
   SomeClass2.Free;
 end;
finally
 SomeClass1.Free;
end;

```

try...except的用法

如果你希望在发生异常时执行一些任务，可以使用try...except。通常，没有必要为了简单地显示一个错误信息而使用try...except，因为Application对象能够自动根据上下文做到这一点。如果要在子句中激活默认的异常处理，可以再次触发异常。

try...except...else的用法

不鼓励使用带else子句的try...except，因为这将阻塞所有的异常，包括你没有准备处理的异常。

#### 3. 命名规范

##### 3.1. 过程(Procedure)与函数(Function)

命名

过程与函数名应当有意义。进行一个动作的过程最好在名称前加上表示动作的动词为前缀。例如：`procedure FormatHardDrive;`

 设置输入参数值的过程名应当以Set 为其前缀，例如：`procedure SetUserName;`

 获取数值的过程名应当以Get 为其前缀，例如：`function GetUserName:string;`

##### 3.2. 形参

 所有形参的名称都应当表达出它的用途。如果合适的话，形参的名称最好以字母a 为前缀，例如：

`procedure SomeProc(aUserName:string; aUserAge:integer);`

 当参数名与类的特性或字段同名时，前缀a 就有必要了。

##### 3.3. 命名冲突

 当两个单元中含有相同名称的过程时，如果调用该过程，实际被调用的是Uses 子句中较后出现的那个单元中的过程。为避免这种情况，可在方法名前加想要的单元名，例如：

`SysUtils.FindClose(SR);`

`或Windows.FindClose(Handle);`

##### 3.4. 变量(Variable)

变量的名称应当能够表达出它的用途。循环控制变量常常为单个字母，诸如I 、J 或K 。也可以使用更有意义的名称，例如UserIndex。布尔变量名必须能清楚表示出True 和False 值的意义。

###### 3.4.1. 局部变量

局部变量遵循其他变量的命名规则。

###### 3.4.2. 全局变量

全局变量以大写字母“G”打头，并遵循其他变量的命名规则。

##### 3.5. 类型(Type)



###### 3.5.1. 枚举型

枚举类型名必须代表枚举的用途。名称前要加T字符作为前缀，表示这是个数据类型。枚举类型的标识符列表的前缀应包含2 - 3 个小写字符，来彼此关联。例如：

`TSongType=(stRock, stClassical, stCountry, stAlternative, stHeavyMetal, stRB);`

 枚举类型的变量实例的名称与类型相同，但没有前缀T ，也可以给变量一个更加特殊名称，诸如：FavoriteSongTypel、FavoriteSongType2等等。



###### 3.5.2. 数组类型

数组类型名应表达出该数组的用途。类型名必须加字母“T”为前缀。如果要声明一个指向数组类型的指针，则必须加字母P 为前缀，且声明在类型声明之前。例如：

```pascal
type
 PCycleArray = ^TCycleArray;
 TCycleArray=array[1..100] of integer;

```

实际上，数组类型的变量实例与类型名称相同，但没有“T”前缀。


###### 3.5.3. 记录类型

记录类型名应表达出记录的用途。类型名必须加字母T为前缀。如果要声明一个指向记录类型的指计，则必须加字母P为前缀，且其声明在类型声明之前。例如：

```pascal
type
 PEmployee = ^TEmployee;
 TEmployee = record
   EmployeeName: string;
   EmployeeRate: Double;
 end;

```
