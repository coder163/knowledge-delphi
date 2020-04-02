
#### 1. 概述

##### 1.1. 为什么使用异常机制

每一段程序都有可能出错！这是软件业的一个不容置疑的现象和规律。事实上，传统的 if…else…结构完全可以解决所有的错误，使用 Exception 机制也没能够回避在最原始的层次，通过遍历可能的情况来产生异常的做法，那么，为什么还需要异常机制

答案很简单：异常提供了一种更加灵活和开放的方式，使得后来的编程者可以来根据实际的情况处理这种错误，而不是使用预先设定好的处理结果。实际上，这就是异常处理机制的核心


#### 2. 异常的种类

##### 2.1. 设计期异常

这种异常类型发生在设计期，通常是因为给组件的某个属性输入了非法的值。例如，在设计数据库应用程序时指定了一个没有定义的数据库别名。

这种类型的异常比较容易被发现和纠正，因为Delphi能够对属性的值进行合法性检查。一旦发现这种错误，Delphi将弹出一个警告窗口，提示你纠正错误。

##### 2.2. 编译期异常

编译期异常也叫语法错误，当程序代码违反了Object  Pascal的语法规则时将发生这种错误。

如果程序代码中有语法错误，编译就不能通过，代码编辑器的状态栏将给出错误信息提示，并在代码编辑器中突出显示有语法错误的行。

比较常见的语法错误是数据类型不匹配，特别是调用RTL、VCL或Windows的API时容易发生参数不匹配的错误。

##### 2.3. 运行期异常

程序虽然通过了编译，但在运行时失败了，这种错误称为运行期异常。例如，程序试图打开一个不存在的文件，或者在运算时出现了被零除。


#### 3. 异常的处理

Delphi提供的所有异常类都是类Exception的子类。用户也可以从Exception派生一个自定义的异常类。

Exception的一系列构造函数中最重要的参数是显示的错误信息。而数据成员中最重要的也是可被引用的消息字符串(message，messagePtr)。这些信息分别对自定义一个异常类和处理一个异常类有重要作用。

##### 3.1. try…except…end;

在try 体内的代码发生异常时，系统将转向except 部分进行异常的处理。这是Delphi处理异常的最基本的方式之一。

只有当try 体内的代码发生异常时，才会跳转到except 里面的代码进行执行

需要注意的是在excpt...end之间的代码依然有可能存在异常

##### 3.2. try…finally…end;

这种异常处理结构一般用于保护windows的资源分配等方面，它确保了无论try 体内的代码是否发生异常，都需要由系统进行最后的统一处理的一些Windows对象的正确处理

和try…except…end不同，该结构的finally部分总被执行

##### 3.3. 不存在try…except…finally…end 结构

不存在try…except…finally…end 结构来既处理异常，又保护资源分配的结构，但是，try…except…end结构允许嵌套到try…finally…end结构中，从而既处理异常，又保护资源的分配

#### 4. 异常的精确处理

##### 4.1. 定义一个异常

在Delphi中，每个异常都是Exception类的一个派生类。因此，定义一个异常就是定义一个Exception类的派生类

```pascal
type
    EMyException = class(Exception);

```

当然，基类可以是Exception或者是Exception的任何一个任何层次的派生类

##### 4.2. 在程序中抛出一个异常

根据不同的情况抛出异常是使用异常的最基本的模式。在Delphi中，由raise语句来实现

**raise 异常类.Create('异常信息说明');**

注意因为使用了 异常类.Create('异常信息说明'); 所以也就是在内存中创建了一个异常实体。

```pascal
begin
  raise Exception.Create('抛出异常');
end;
```

例2

```pascal
begin
  raise Exception.CreateFmt('%s %d', ['错误代码:', 999]);
end;
```

例3

```pascal
var
  exc: Exception;
begin
  exc := Exception.Create('发现异常');
  raise exc;
end;
```

**直接使用：raise;**

```pascal
var
    iTest: Integer;
    iStr: string;
...
try
    iTest:= StrIntTo(iStr);
except
    raise Exception.Create('抛出异常');
end;
```

##### 4.3. 在try…except…end中更加精确的捕捉异常

可以使用 on E:异常类 do... 结构可以在 do 体内处理特定异常类所抛出的异常。

如果在except中使用 on E: 异常类 do… 的话，在except中的 on E: 异常类 do…之外不能有任何语句，例如下面的语法是正确的

```pascal
try
    ...
except
    on E: Exception do
    begin
        ShowMessage('OK');
        ShowMessage('OK Again');
    end;
end;
```
但是下面的方式就错了

```pascal
try
    ...
except
    on E: Exception do
    begin
        ShowMessage('OK');
    end;
        //如果在except中使用 on E: 异常类 do 的话，在except中的 on E: 异常类 do之外不能有任何语句
        ShowMessage('Not OK');
end;
```

#### 5. 使用场景

异常信息的处理方法（写日志、抛出异常），函数或过程出现异常时，只能抛出异常，禁止弹出对话框架。

函数及过程、控件中发生异常，通常是直接抛出异常，不要显示信息提示框；

界面操作（按钮）中的异常，可以显示提示信息（视具体应用而定），也可以不显示提示而将异常信息保存到日志文件或两种方式同时使用；

DLL文件中的异常：如果是函数或过程，发生异常时就直接抛出异常；如果是界面操作，则按界面操作（按钮）中的异常处理方法处理。








