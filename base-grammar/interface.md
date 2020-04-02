

#### 1. 抽象类

Delphi中的抽象类只定义行为的类，它规定了由此派生的类必须具备的某些行为。但是抽象类不实现这些行为，而必须由其派生类去实现这些行为。所以它只是一种“抽象”的类，而且，我们无法为抽象类创建实例。比如这段代码

```pascal
TCode = class
protected
    function DeCode(Src: String): String; virtual; abstract;
    function EnCode(Src: String): String; virtual; abstract;
end;
```

这段代码定义了一个有密码的抽象类，它规定了该类的两个行为，即编码和解码。但它只是规定了这两个行为而没有去实现它们，因为具体到不同的加密体系，它们的编码和解码的行为不一样，所以这两个行为交由具体的派生类去实现。

抽象类只定义了行为而没有去实现它们，所以它是"抽象"的。

#### 2. 接口

接口也定义了一系列的行为而没有去实现这些行为，这一点上和抽象类是一样的，准确的说，**接口只是规定了接口的提供者和接口的使用者之间的协议**

既然有了抽象类为什么还要有接口？那一定是接口和抽象类有不同的地方，正是这种不同使它们应用在不同的场合或者满足不同的需求。弄清这些不同之处的过程，也就是我们理解接口这个概念的过程

**接口和抽象类的比较**

接口都以"I"打头（类以"T"打头）

接口的关键字是interface（类的关键字是class）

因为接口中的所有的函数和过程都是虚的，所以没有必要加上virtual的关键字

**但是以上的区别只是形式上的，还有一些实质上的区别**

对接口创建实例的代码在Delphi中是非法的，在编译期间会出现异常

接口中所有的方法都是共有的，因此不能对接口中的域进行定义

在接口中不能申明变量，这是它和类的一个很大的区别，因为接口只定义了行为而对实现这些行为不做规定，但是如果允许在接口中定义变量，则就在某种程度上干预域或者限制实现行为的方法

接口是不变的，也就是说，一旦声明并公开了某个接口之后，就不允许再对它进行修改，我们淘增加或删除它所规定的行为，就只能另外声明一个接口。

抽象了可以有成员变量，可以自己实现其中的一些成员函数，也可以将方法定义为 virtual;abstract; 然后由其派生类实现

但是接口不能有成员变量，而且也不能自己实现接口中的任何一个方法，只能由实现它的类来具体实现这些方法

#### 3. 定义接口

就像多有的Delphi类都派生于TObject一样，所有的接口都派生于一个称谓IUnKnown的接口，IUnKnown在system单元中定义如下

```pascal
type
    IUnKnown = interface
　　　　['{00000000-0000-0000-C000-000000000046}']
        function QueryInterface(const IID: TGUID; out Obj): Integer; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
    end;
```

接口的定义就像是类的定义，最根本的不同是在接口中有一个全局唯一标识符（GUID），它对应每一个接口而言都是不同的。对于IUnKnown的定义来自于MicroSoft的组件对象模(COM)规范。

如果你知道怎么创建Delphi的类，那么定义一个定制的接口是一件简单的事情，下面的代码定义了一个新的接口IFoo，它包含一个称为F1()的方法

```pascal
type
    IFoo = interface<br>　　　　['{2137B60-AA33-11D0-A9BF-9A537A42701}']
        function F1: Integer;
    end;
```

提示：在Delphi的IDE中，按Ctrl+Shift+G快捷键，就可以为接口生成一个新的GUID

下面的代码声明了一个称为IBar的接口，它是从IFoo接口继承来的

```pascal
type
    IBar = interface(IFoo)
        ['{2137BF61-AA33-11D0-A9BF-9A4537A42701}']
        function F2: Integer;
    end;
```

#### 4. 实现接口

下面的代码演示了在一个类TFooBar中怎么实现IFoo和IBar接口

```pascal
type
    TFooBar = class(TInterfacedObject, IFoo, IBar)
        function F1: Integer;
        function F2: Integer;
    end;

function TFooBar.F1:Integer;
begin
    Result:= 0;
end;

function TFooBar.F2: Integer;
begin
    Result:= 0;
end;
```

**一个类实现接口的时候，一定要在class的括号里面加上这个类：TInterfacedObject**

注意，一个类可以实现多个接口，只要在声明这个类的时候，一次列出要实现的接口。编译器通过名称来把接口中的方法与实现接口中的类中的方法对应起来，如果一个类只是声明要实现某个接口，但并没有具体实现这个接口中的方法，编译将会出错

如果一个类要实现多个接口，而这些接口中包含同名方法，必须把同名的方法另取一个别名，例子

```pascal
type
    IFoo = interface
        ['{21313134-DE23-1213-1123-A328301232}']
        function F1: Integer;
    end;

    IBar = interface
        ['{21313134-DE23-1213-1123-A32DE01232}']
        function F1: integer;
    end;

    TFooBar = class(TInterfaceObject, IFoo, IBar)
        //为同名方法取别名
        function IFoo.F1 = FooF1;
        function IBar.F1 = BarF1;
        //接口方法
        function FooF1: Integer;
        function BarF1: Integer;
    end;

function TFooBar.FooF1: Integer;
begin
    Result:= 0;
end;

function TFooBar.BarF1: Integer;
begin
    Result:= 0;
end;
```

#### 5. implements指示符

implements指示符是在Delphi4中引进的，它的作用是委托另一个类或者接口实现接口中的某个方法这个技术有时又被称为委托实现，关于 implements指示符的用法，请看下面的代码：

```pascal
type
    TSomeClass = class(TInterfacedObject, IFoo)
        function GetFoo: TFoo;
        property Foo: TFoo read GetFoo implements IFoo;
    end;
```

在上面的例子中的implements指示符是要求编译器在Foo属性中寻找实现IFoo接口方法。属性的类型必须是一个类，它包含IFoo方法或者类型是IFoo的接口后或者IFoo派生接口。implements指示符后面可以列出几个接口，彼此用逗号隔开

implements指示符在开发中提供了两个好处：

首先，它允许以无冲突的方式进行接口聚合。聚合是COM中的概念，它的作用是把多个类合在一起共同完成一个任务。

其次，它能够延后占用实现接口所需的资源，知道确实需要资源。例如，假设实现一个接口需要分配一个1MB的位图，但是这个接口很少用到，因此，可能平时你不想实现这个接口，因为它太耗费资源了，用implements指示符之后，可以只在属性被访问时才创建一个类来实现接口


#### 6. 使用接口

当在应用程序中使用接口类型的变量时，要用到一些重要的语法规则。**最需要记住的是，一个接口是生存期自管理类型的，这就意味着，它通常被初始化为NIL，它是引用计数的，当获得一个接口时自动增加一个引用计数；当它离开作用域或者赋值为NIL时它被自动释放**。下面的代码演示一个接口变量的生存期自管理机制

```pascal
var
    I : ISomeInterface;
begin
    //I被初始化为NIL
    I:= FunctionReturningAnInterface;    //I的引用计数加1
    I.SomeFunc;
    // I 的引用计数减1，如果为0，则自动释放
end;
```

关于接口变量的另一个规则是，**一个接口变量与实现这个接口的类是赋值兼容的**，例如，下面的代码是合法的：

```pascal
procedure Test(FB: TFooBar)
var
    F:IFoo;
begin
    F:= FB;    //合法，因为FB支持IFoo
    ..
    ..
```

最后，**类型强制转换运算符as可以把一个接口类型的变量强制类型转换为另一个接口**，例子

```pascal
var
    FB: TFooBar;
    F: IFoo;
    B IBar;
begin
    FB:= TFooBar.Create;
    F:= FB;                //合法，因为FB支持IFoo
    B:= F as IBar;      //合法，把F转换为IBar
    ..
```

#### 7. 接口的GUID

　GUID是"全球唯一标识符"的缩写，它借助复杂的算法而保证在全球范围内的唯一性。每个接口都有一个GUID，系统据此注册和调用接口。事实上，上面那段声明接口的代码是不完整的，完整的声明代码看上去应该是这样的：

```pascal
ICode = interface
  ['{56E57F5F-5DD8-42FC-8D27-C35968C4474E}']
  function Decode(Src: String): String;
  function Encode(Src: String): String;
end;
```
好在我们不必去关心GUID是怎样产生的，当我们需要给一个接口创建一个GUID时，我们只要在Delphi的IDE中同时按下Ctrl、Shift和G三个键，光标所在的位置就会出现一个GUID。

其实这不是Delphi自己胡编出来的GUID，而是Delphi调用Windows的API函数：CoCreateGuid创建的，它借助开放软件基金会（OSF）定义的算法，从统计学的角度上说，能保证GUID的全球唯一性

