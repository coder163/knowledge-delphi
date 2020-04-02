#### 1. 概述

例程(routine)是Pascal 的一个重要概念，例程由一系列语句组成，例程名是唯一的，通过例程名你可以多次调用它，这样程序中只需要一个例程就够了，由此避免了代码多次重复，而且代码也容易修改维护。从这个角度看，你可以认为例程是一种基本的代码封装机制。

#### 2. Pascal 过程与函数

Pascal中的例程有两种形式：过程和函数。理论上说，过程是你要求计算机执行的操作，函数是能返回值的计算。两者突出的不同点在于：函数能返回计算结果，即有一个返回值，而过程没有。两种类型的例程都可以带多个给定类型的参数。

不过实际上函数和过程差别不大，因为你可以调用函数完成一系列操作，跳过其返回值(用可选的出错代码或类似的东西代替返回值)；也可以通过过程的参数传递计算结果

下例定义了一个过程、两个函数，两个函数的语法略有不同，结果是完全相同的。

```pascal
procedure Hello;
begin
  ShowMessage ('Hello world!');
end;

function Double (Value: Integer) : Integer;
begin
  Double := Value * 2;
end;

// or, as an alternative
function Double2 (Value: Integer) : Integer;
begin
  Result := Value * 2;
end;
```

流行的做法是用Result 给函数赋返回值，而不是用函数名，我认为这样的代码更易读。

一旦定义了这些例程，你就可以多次调用，其中调用过程可执行操作；调用函数能计算返回值。如下：

```pascal
procedure TForm1.Button1Click (Sender: TObject);
begin
  Hello;
end;

procedure TForm1.Button2Click (Sender: TObject);
var
  X, Y: Integer;
begin
  X := Double (StrToInt (Edit1.Text));
  Y := Double (X);
  ShowMessage (IntToStr (Y));
end;
```
##### 2.1. 代码封装概念

你调用Double 函数时，你不需要知道该函数的具体实现方法。如果以后发现了更好的双倍数计算方法，你只需要改变函数的代码，而调用函数的代码不必改变（尽管代码执行速度可能会加快！）。Hello 过程也一样，你可以通过改变这个过程的代码，修改程序的输出

当调用一个现有的Delphi 函数、过程或任何VCL方法时，你应该记住参数的个数及其数据类型。不过，只要键入函数或过程名及左括号，Delphi 编辑器中会出现即时提示条，列出函数或过程的参数表供参考。


#### 3. 引用参数

Pascal 例程的传递参数可以是值参也可以是引用参数。值参传递是缺省的参数传递方式：即将值参的拷贝压入栈中，例程使用、操纵的是栈中的拷贝值，不是原始值。

当通过引用传递参数时，没有按正常方式把参数值的拷贝压栈（避免拷贝值压栈一般能加快程序执行速度），而是直接引用参数原始值，例程中的代码也同样访问原始值，这样就能在过程或函数中改变参数的值。引用参数用关键字var 标示。

参数引用技术在大多数编程语言中都有，C语言中虽没有，但C++中引入了该技术。在C++中，用符号 &表示引用；在VB中，没有ByVal 标示的参数都为引用。

下面是利用引用传递参数的例子，引用参数用var关键字标示：

```pascal
procedure DoubleTheValue (var Value: Integer);
begin
  Value := Value * 2;
end;

```

在这种情况下，参数既把一个值传递给过程，又把新值返回给调用过程的代码。当你执行完以下代码时：

```pascal
var
  X: Integer;
begin
  X := 10;
  DoubleTheValue (X);
```

x变量的值变成了20，因为过程通过引用访问了X的原始存储单元，由此改变了X的初始值。

通过引用传递参数对有序类型、传统字符串类型及大型记录类型才有意义。实际上Delphi总是通过值来传递对象，因为Delphi对象本身就是引用。因此通过引用传递对象就没什么意义（除了极特殊的情况），因为这样相当于传递一个引用到另一个引用。


#### 4. 常量参数

除了引用参数外，还有一种参数叫常量参数。由于不允许在例程中给常量参数赋新值，因此编译器能优化常参的传递过程。编译器会选用一种与引用参数相似的方法编译常参（C++术语中的常量引用），但是从表面上看常参又与值参相似，因为常参初始值不受例程的影响。

事实上，如果编译下面有点可笑的代码，Delphi将出现错误：


```pascal
function DoubleTheValue (const Value: Integer): Integer;
begin
  Value := Value * 2;      // compiler error
  Result := Value;
end;
```

#### 5. 开放数组参数

这个类似参数个数可变

与C语言不同，Pascal 函数及过程的参数个数是预定的。如果参数个数预先没有确定，则需要通过开放数组来实现参数传递。

一个开放数组参数就是一个固定类型开放数组的元素。 也就是说，参数类型已定义，但是数组中的元素个数是未知数。见下例：

```pascal
function Sum (const A: array of Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(A) to High(A) do
    Result := Result + A[I];
end;
```

#### 6. 类型变化的开放数组参数

除了类型固定的开放数组外，Delphi 还允许定义类型变化的甚至无类型的开放数组。这种特殊类型的数组元素可随意变化，能很方便地用作传递参数。

array of const 类型的数组就能实现把不同类型、不同个数元素组成的数组一下子传递给例程。如下面Format 函数的定义

```pascal

function Format (const Format: string; const Args: array of const): string;
```

上面第二个参数是个开放数组，该数组元素可随意变化。如你可以按以下方式调用这个函数：

```pascal
N := 20;
S := 'Total:';
Label1.Caption := Format ('Total: %d', [N]);
Label2.Caption := Format ('Int: %d, Float: %f', [N, 12.4]);
Label3.Caption := Format ('%s %d', [S, N * 2]);

```

#### 7. 参数默认值

可以在过程或函数的声明部分(形参列表)为参数指定一个默认值,只允许对有类型的常量参数和值参数指定默认值

格式如下：

 修饰符    参数名称    :    参数类型=默认值;

代码示例：

```pascal
procedure fillArray( a: Integer;value:Integer=0);
begin
    //此处省略部分代码
end;
//调用
begin
    fillArray(100);
    fillArray(100,1);

end;
```

#### 1. Forward 声明(前置声明)

当使用一个标识符（任何类型）时，编译器必须已经知道该标识符指的是什么。为此，你通常需要在例程使用之前提供一个完整的声明。然而在某些情况下可能做不到这一点，例如过程A调用过程B，而过程B又调用过程A，那么你写过程代码时，不得不调用编译器尚未看到其声明的例程。

欲声明一个过程或函数，而且只给出它的名字和参数，不列出其实现代码，需要在句尾加forward 关键字：

```pascal
procedure Hello; forward;

```

该代码例子没有实际意义,仅仅是为帮助理解概念

```pascal
procedure DoubleHello; forward;

procedure Hello;
begin
  if MessageDlg ('Do you want a double message?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    DoubleHello
  else
    ShowMessage ('Hello');
end;

procedure DoubleHello;
begin
  Hello;
  Hello;
end;
```

尽管 forward 过程声明在Delphi中不常见，但是有一个类似的情况却经常出现。当你在一个单元（关于单元的更多内容见下一章）的interface 部分声明一个过程或一个函数时，它被认为是一个forward声明，即使没有forward关键字也一样。实际上你不可能把整个例程的代码放在interface 部分，不过你必须在同一单元中提供所声明例程的实现。

类内部的方法声明也同样是forward声明，当你给窗体或其组件添加事件时， Delphi会自动产生相应的代码。在TForm 类中声明的事件是forward 声明，事件代码放在单元的实现部分。



#### 2. 函数重载

重载的思想很简单：编译器允许你用同一名字定义多个函数或过程，只要它们所带的参数不同。实际上，编译器是通过检测参数来确定需要调用的例程。

声明重载函数有两条原则：

- 每个例程声明后面必须添加overload 关键字。
- 例程间的参数个数或(和)参数类型必须不同，返回值不能用于区分各例程。

示例代码

```pascal
procedure ShowMsg (str: string); overload;
begin
  MessageDlg (str, mtInformation, [mbOK], 0);
end;

procedure ShowMsg (FormatStr: string;
  Params: array of const); overload;
begin
  MessageDlg (Format (FormatStr, Params),
    mtInformation, [mbOK], 0);
end;

procedure ShowMsg (I: Integer; Str: string); overload;
begin
  ShowMsg (IntToStr (I) + ' ' + Str);
end;
```

三个过程分别用三种不同的方法格式化字符串，然后在信息框中显示字符串。下面是三个例程的调用：

```pascal
ShowMsg ('Hello');
ShowMsg ('Total = %d.', [100]);
ShowMsg (10, 'MBytes'
```

#### 3. 返回值

需要注意的是在Delphi中没有类似其他语言的return关键字,如果必须想实现return的效果需要使用result 和exit 联合使用

声明带有返回值函数的格式

function 函数标示符(参数列表):返回值;

##### 3.1. 通过引用传递数据

传递数据的默认值是按值调用的。从字面上看，参数值被传递给子例程参数。然后参照变量值的这个副本。

按引用传递意味着子例程实际上是指传入的变量而不是其值。对值的任何更改都会影响调用者变量。我们声明一个变量，通过var前缀引用来传递。

这种方式在不提供例程输出具体代码的情况下也会改变传入的变量值

```pascal
 procedure DoIt(Var A : Integer);
 begin
   A := A * 2;
   ShowMessageFmt('A in the procedure  = %d',[A]);
 end;
```

##### 3.2. 仅输出参数

```pascal
 procedure DoIt(Out A : Integer);
 begin
   A := 123;
   ShowMessageFmt('A in the procedure  = %d',[A]);
 end;

 procedure TForm1.FormCreate(Sender: TObject);
 var
   A : Integer;
 begin
   ShowMessage('A before the call is unknown');
   // Call the procedure
   DoIt(A);
   ShowMessageFmt('A in program now = %d',[A]);
 end;
```

##### 3.3. 通过Result和者函数名

Result是**函数**特有的局部变量，它的类型与函数的返回类型相同，对它的赋值等于对函数名赋值。与函数名不同，Result变量可以出现在赋值号的右边，如果函数名出现在赋值号的右边时，则意味着递归调用，也就是函数调用它自己。

```pascal
function MyFunc(s:Integer):Integer;
Begin 
    //执行赋值但不返回。
    Result:=Result*s;
    //赋值后程序返回。
    MyFunc:=s*s;

End;

function MyFunc():Integer;
Begin
    Result:=Result+5; //如果使用MyFunc():=MyFunc()+5;将导致死循环。
End;

```






















































