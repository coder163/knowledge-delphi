
Delphi中可以通过函数指针把一个函数作为参数来传递，然后在另外一个函数中调用。


首先，申明函数指针类型TFunctionParameter

```pascal
type
          TFunctionParameter = function(const value : integer) : string; //函数指针

```


定义准备被作为参数传递的函数

```pascal
function One(const value : integer) : string;                            //函数-实例1
begin
  result := IntToStr(value) ;
end;

function Two(const value : integer) : string;                            //函数-实例2
begin
  result := IntToStr(2 * value) ;
end;
```


定义将要使用动态函数指针参数的函数

```pascal
function DynamicFunction(f : TFunctionParameter; const value : integer) : string;
begin
  result := f(value) ;
end;
```

上面这个动态函数的使用实例

```pascal
var
  s : string;
begin
  s := DynamicFunction(One,2006) ;
  ShowMessage(s) ; //will display "2006"

  s := DynamicFunction(Two,2006) ;
  ShowMessage(s) ; // will display "4012"
end;
```




指向非对象（一般的）函数/过程的函数指针

 Pascal 中的过程类型与C语言中的函数指针相似,为了统一说法,以下称函数指针。函数指针的声明只需要参数列表；如果是函数，再加个返回值。例如声明一个过程类型，该类型带一个通过引用传递的整型参数：

```pascal
type
  IntProc = procedure (var Num: Integer);
```

这个过程类型与任何参数完全相同的例程兼容,即用它声明的变量,可以指向任何此类函数,并通过其进行函数的调用。下面是一个兼容例程：

```pascal
procedure DoubleTheValue (var Value: Integer);
begin
Value := Value * 2;
end;
```

函数指针能用于两种不同的目的：声明函数指针类型的变量；或者把函数指针作为参数传递给另一例程。利用上面给定的类型和过程声明，你可以写出下面的代码：

```pascal
var
  IP: IntProc;
  X: Integer;
begin
  IP := DoubleTheValue;
  X := 5;
  IP (X);
end;
```

虽然这种调用方法比直接调用麻烦了,那么我们为什么要用这种方式呢？

1. 因为在某些情况下，调用什么样的函数需要在实际中(运行时)决定,你可以根据条件来判断,实现用同一个表达,调用不同的函数,很是灵活.

2. 利用函数指针我们可以实现委托,委托在.NEt中被发挥的淋漓尽致,但Delphi同样能实现

3. 实现回调机制

函数指针很有用啊,是高级程序员的必修。

例子

```pascal
{********************************************************
 函数指针(指向一般函数和过程)

*********************************************************}
unit DelegateUnit;
interface
     procedure Func1;
     {定义两个函数型构相同但功能不同的函数}
    function    FuncAdd(VarA , VarB : Integer):Integer;
    function    FuncSub(VarA , VarB : Integer):Integer;
type
    DelegateFunc1 = procedure;
    DelegateFuncCalc = function(VarA , VarB : Integer):Integer;
var
  I : Integer;
implementation

procedure Func1;
begin
      Writeln('Func1 was called!');
end;

function    FuncAdd(VarA , VarB : Integer):Integer;
begin
     Result := VarA + VarB;
end;
function    FuncSub(VarA , VarB : Integer):Integer;
begin
   Result := VarA - VarB;
end;
end.

```
客户端调用

```pascal
program Delegate;
{$APPTYPE CONSOLE}
uses
  DelegateUnit;
 var
    ADelegateFunc1 : DelegateFunc1;
    ADelegateFuncCalc : DelegateFuncCalc;
 begin
     {通过函数指针调用过程}
     ADelegateFunc1  := Func1;
     ADelegateFunc1 ;
    {通过同种方式调用不同函数}
    ADelegateFuncCalc  := FuncAdd;
    Writeln(ADelegateFuncCalc(3,5));
    ADelegateFuncCalc  := FuncSub;
    Writeln(ADelegateFuncCalc(3,5));
end.

```
