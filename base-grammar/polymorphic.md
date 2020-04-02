#### 1. 1.运行期类型信息概述

运行期类型信息(RTTI)是一种语言特征，能使应用程序在运行时得到关于对象的信息

运行期类型信息(RTTI)是一种语言特征，能使应用程序在运行时得到关于对象的信息。RTTI是Delphi的组件能够融合到IDE中的关键。它在IDE中不仅仅是一个纯学术的过程。

由于对象都是从TObject继承下来的，因此，对象都包含一个指向它们的RTTI的指针以及几个内建的方法。下面的表列出了TObject的一些方法，用这些方法能获得某个对象实例的信息。

<table style="WIDTH: 574px; HEIGHT: 192px" cellspacing="1" cellpadding="1" width="574" summary="" border="1" class="table table-bordered">
   <tbody>
    <tr>
     <td>函数&nbsp;&nbsp;</td>
     <td>返回类型</td>
     <td>返回值</td>
    </tr>
    <tr>
     <td>ClassName( )&nbsp;&nbsp;</td>
     <td>string</td>
     <td>对象的类名</td>
    </tr>
    <tr>
     <td>ClassType()&nbsp;</td>
     <td>boolean</td>
     <td>对象的类型<br></td>
    </tr>
    <tr>
     <td>InheritsFrom&nbsp;</td>
     <td>boolean&nbsp;&nbsp;&nbsp;&nbsp; </td>
     <td>判断对象是否继承于一个指定的类</td>
    </tr>
    <tr>
     <td>ClassParent()&nbsp;</td>
     <td>TClass</td>
     <td>对象的祖先类型<br></td>
    </tr>
    <tr>
     <td>Instancesize()&nbsp;</td>
     <td>word&nbsp;</td>
     <td>对象实例的长度(字节数)</td>
    </tr>
    <tr>
     <td>ClassInfo()</td>
     <td>Pointer&nbsp;</td>
     <td>指向RTTI的指针</td>
    </tr>
   </tbody>
  </table>

#### 2. 关于as 和 is


Object Pascal提供了两个运算符as和is，用它们通过RTTI能对对象进行比较和强制类型转换。

关键字as是类型转换的一种新的形式。它能把一个基层的对象强制类型转换成它的派生类，如果转换不合法就产生一个异常。假定有一个过程，想让它能够传递任何类型的对象，它应该这样定义：

```pascal
    Procedure Foo(AnObject :Tobject);
```

在这个过程如果要对AnObject进行操作，要把它转换为一个派生对象。假定把AnObject看成是一个TEdit派生类型，并想要改变它所包含的文本，用下列代码：

```pascal
 (AnObject as Tedit).text := 'wudi_1982';
```

能用比较运算符来判断两个对象是否是相兼容的类型，用is运算符把一个未知的对象和一个已知类型或实例进行比较，确定这个未知对象的属性和行为。例如，在对(AnObject 进行强制类型转换前，确定(AnObject 和TEdit是否指针兼容：

```pascal
if (AnObject is Tedit) then
 Tedit(AnObjject).text := 'wudi_1982';
```

 注意在这个例子中不要再使用as进行强制类型转换，这是因为它要大量使用RTTI，另外还因为，在第一行已经判断Foo就是TEdit，可以通过在第2行进行指针转换来优化


#### 3. 判断指针是否为空

delphi函数指针 只有@@p才代表了函数指针本身的地址

assigned(p) 判断是否为空

或者用 @p=nil 来判断函数指针是不是为空

#### 4. 接口的委托实现

```pascal

unit Unit1;
interface

type
  IDemo = interface
    procedure showName();
  end;

  IDemoImpl = class(TInterfacedObject, Idemo)
    procedure showName();
  end;

  DemoImpl2 = class(TInterfacedObject, Idemo)
  private
    FDemo: IDemo;
  public
    property Demo: IDemo read FDemo write FDemo implements IDemo;
  end;

implementation

{ IDemoImpl }

procedure IDemoImpl.showName;
begin

end;

end.

//调用
var
 Interface1:IDemo;
 obj:DemoImpl2;
begin
 obj := DemoImpl2.Create;
 obj.Demo := IDemoImpl.Create;
 Interface1 := obj;
 Interface1.showName();
 readln;
end.

```