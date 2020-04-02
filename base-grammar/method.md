
#### 1. 概述

方法是属于一个给定对象的过程和函数，方法反映的是对象的行为而不是数据，前一篇提到的对象的两个重要的方法：构造方法和析构方法。

为了使对象能执行各种功能，你能在对象中定制方法

创建一个方法用两个步骤，首先在对象类型的声明中声明这个方法。然后再用代码定义方法。下面的代码就演示了声明和定义一个方法的步骤

```pascal
type
    TBoogieNights = class
        Dance: Boolean;
        Procedure DoTheHustle;
    end;

procedure TBoogieNights.DoTheHustle;
begin
    Dance:= True;
end;
```

注意：在定义方法体时，必须使用完整的名字，就像在定义方法DoTheHustle时那样。同时也要注意到，这个方法中，对象的Dance域能够被直接访问

#### 2. 方法的类型

对象的方法能定义成静态（static）、虚拟（virtual）、动态（dynamic）或者消息处理（message）。请看下面的例子

```pascal
TFoo = class
    procedure IAmAStatic;
    procedure IAmAVirtual; virtual;
    procedure IAmADynamic; dynamic;
    procedure IAmAMessage(var m: TMessage); message wm_SomeMessage;
end;
```

##### 2.1. 静态方法

IAMAStatic是一个静态方法，静态方法是方法的缺省类型，对它就像通常的过程和函数那样调用。编译器知道这些方法的地址，所以调用一个静态方法时它能进行信息静态地链接进可执行文件。静态方法执行的速度最快，但是它们却不能被覆盖来支持多态性



##### 2.2. 虚拟方法

IAMAVirtual是一个虚拟方法。虚拟方法和静态方法的调用方式相同。由于虚拟方法能被覆盖，在代码中调用一个指定的虚拟方法时编译器并不知道它的地址。因此，编译器通过建立虚拟方法表（VMT）来查找在运行时的函数地址。所有的虚拟方法在运行时通过VMT来调度，一个对象的VMT表中除了自己定义的虚拟方法外，还有它的祖先的所有的虚拟方法，因此虚拟方法比动态方法用的内存要多，但是它执行的比较快

##### 2.3. 动态方法

IAMADynamic是一个动态方法，动态方法跟虚拟方法基本相似，只是它们的调度系统不同。编译器为每个动态方法指定一个独一无二的数字，用这个数字和动态方法的地址构造一个动态方法表（DMT）。不像VMT表，在DMT表中仅有它声明的动态方法，并且这个方法需要祖先的DMT表来访问它其他的动态方法。正因为如此，动态方法比虚拟方法用的内存少，但是执行起来较慢，因为可能要到祖先对象的DMT中查找动态方法

**小结：**

每个类都内含着两个表: 虚方法表(VMT)和动态方法表(DMT);

VMT 表包含着本类与其所有父类的虚方法 - 那一般会是一个比较庞大的表;

DMT 表只包含本类的动态方法 - 如果要调用其上层类的动态方法, 只能逐级查找;

因此, 使用虚方法速度上会有优势, 使用动态方法会节约内存;

在 Delphi 初期只有 virtual 而没有 dynamic ; 后来随着 VCL 日渐庞大, 才有了 dynamic ;譬如类的事件方法一般都是在早期定义, 为了节约空间, 事件方法在 VCL 中基本都定义成了 dynamic ;这样看来: virtual 和 dynamic 并没有太多区别, 一个侧重速度、一个节约空间; 它们是可以互相代替的!另外: 因为它们区别不大, 并且是先有 virtual , 所以人们也习惯于把"虚方法"和"动态方法"都称作"虚方法"

##### 2.4. 消息处理方法

IAMAMessage是一个消息处理方法，在关键字message后面的值指明了这个方法要响应的消息。用消息处理方法来响应Windows消息，这样就不用直接来调用它。


关键字Class

在一个类的方法前面加上关键字class，使得方法向其它通常的过程和函数一样调用而不需要生成一个包好这个方法的类的实例，这个功能是从C++的Static函数借鉴来的。
```pascal
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  MY_MSG = WM_USER + 2;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure IAmAMessage(var m: TMessage); message MY_MSG;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  SendMessage(Self.Handle, MY_MSG, 100, Integer(PChar('你好')));
end;

procedure TForm1.IAmAMessage(var m: TMessage);
begin
  ShowMessage(PChar(m.LParam));
end;

end.

```

##### 2.5. 匿名函数

之前我们可以定义方法类型, 然后通过方法类型的变量来使用方法, 譬如:

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Type
  TFun = function(const num: Integer): Integer; {先定义一个方法类型}

  function MySqr(const num: Integer): Integer;  {再创建一个吻合上面类型的一个方法}
  begin
    Result := num * num;
  end;

{测试}
procedure TForm1.FormCreate(Sender: TObject);
var
  fun: TFun;  {方法变量}
  n: Integer;
begin
  fun := MySqr;             {给变量赋值为相同格式的方法}
  n := fun(9);              {现在这个方法变量可以使用了}
  ShowMessage(IntToStr(n)); {81}
end;

end.
```

之所以这样做, 是因为有时需要把 "方法" 当作参数, 譬如:

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Type
  TFun = function(const num: Integer): Integer; {先定义一个方法类型}

  function MySqr(const num: Integer): Integer;  {再创建一个吻合上面类型的一个方法}
  begin
    Result := num * num;
  end;

  {把方法当作参数的方法}
  procedure MyProc(var x: Integer; fun: TFun);
  begin
    x := fun(x);
  end;

{测试}
procedure TForm1.FormCreate(Sender: TObject);
var
  n: Integer;
begin
  n := 9;
  MyProc(n, MySqr);
  ShowMessage(IntToStr(n)); {81}
end;

end.
```

现在 Delphi 2009 可以使用匿名方法了(使用 reference 定义方法类型, 然后在代码中随用随写方法), 譬如:

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Type
  TFun = reference to function(const num: Integer): Integer; {用 reference 定义匿名方法类型}

procedure TForm1.FormCreate(Sender: TObject);
var
  fun: TFun;
  n: Integer;
begin
  {求平方}
  fun := function(const a: Integer): Integer {注意本行最后不能有 ; 号}
  begin
    Result := a * a;
  end;

  n := fun(9);
  ShowMessage(IntToStr(n)); {81}

  {求倍数}
  fun := function(const a: Integer): Integer
  begin
    Result := a + a;
  end;

  n := fun(9);
  ShowMessage(IntToStr(n)); {18}
end;

end.
```


把匿名方法当作其他方法的参数:

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Type
  TFun = reference to function(const num: Integer): Integer;

  function FunTest(const n: Integer; fun: TFun): string;
  begin
    Result := Format('%d, %d', [n, fun(n)]);
  end;

procedure TForm1.FormCreate(Sender: TObject);
var
  f: TFun;
  s: string;
begin
  f := function(const a: Integer): Integer {注意本行最后不能有 ; 号}
  begin
    Result := a * a;
  end;

  s := FunTest(9, f);

  ShowMessage(s); {9, 81}
end;

end.
```


#### 3. 方法的覆盖

在Delphi覆盖一个方法用来实现OOP的堕胎性概念。通过覆盖使一种方法在不同的派生类间表现出不同的行为。Delphi中能被覆盖的方法是在声明时被标识为virtual或dynamic的方法。为了覆盖一个方法，在派生类的声明中用override代替virtual或dynamic，例如，用下面的代码覆盖IAMAVirtual和IAMADynamic方法：

```pascal
TFooChild = class(TFoo)
    procedure IAmAVirtual; override;
    procedure IAmADynamic; override;
    procedure IAmAMessage(var M: TMessage); message wm_SomeMessage;
end;
```

用了override关键字之后，编译器就会用新的方法代替VMT中原来的方法。

注意：如果用virtual或dynamic替代override重新声明IAMAVirtual和IAMADynamic，将是建立新的方法而不是对祖先的方法进行覆盖。

同样，在派生类中如果企图对一个静态方法进行覆盖，在新对象中的方法完全替换在祖先类中的同名方法。

#### 4. 方法的重载

就像普通的过程和函数，方法也支持重载，使得一个类中有许多同名的方法呆着不同的参数表，能重载的方法必须使用overload指示符标识出来，可以不对第一个方法用overload，下面的代码演示一个类中有三个重载的方法：

```pascal
type
    TSomeClass = class
        procedure AMethod(I: Integer); overload;
        procedure AMethod(S: String); overload;
        procedure AMethod(D: Double); overload;
    end;
```

#### 5. 重新引入方法名称

有时候，需要在派生类中增加一个方法，而这个方法的名称与祖先类中的某个方法名称相同。在这种情况下，没必要覆盖这个方法，只要在派生类中重新声明这个方法。但是编译时，编译器就会发出一个警告，告诉你派生类的方法将隐藏祖先类的同名方法。要解决这个问题，可以在派生类中使用reintroduce指示符，下面的代码演示了reintroduce指示符的正确用法

```pascal
type
    TSomeBase = class
        procedure Cooper;
    end;

    TSomeClass = class(TSomeBase)
        procedure Cooper; reintroduce;
    end;
```





















