# 1. 文件结构

每个DLL文件都包含一个导出函数表，这些导出函数由他们的函数名或函数编号与外界联系起来，函数表中还包含了DLL中函数的地址，当应用程序加载DLL模块时,应用程序并不知道DLL中的调用函数的实际地址，只知道函数的名字或编号,系统在加载DLL模块时动态建立一个函数调用与函数地址的对应表,如果重新编译或者重建DLL文件，并不需要修改应用程序,除非改变了导出函数的名字或编号

#### 2. DLL文件搜索位置

如果不指定路径,Windows将遵循下面的搜索顺序来定位DLL

- 包含EXE文件的目录
- 进程的当前工作目录
- Windows系统目录
- 列在Path环境变量中的一系列目录

#### 3. 调用约定

调用约定，是指调用例程时参数的传递顺序。在Delphi中DLL支持的调用约定如下

| 调用约定    | 参数传递顺序   |
| --------- | -----------|
| Register    | 从左到右      |
| PASCAL    | 从左到右      |
| Stdcall    | 从右到左      |
| Cdecl    | 从右到左      |
| safecall    | 从右到左      |

stdcall能保证不同语言写的DLL的兼容性，同时也是WindowsAPI的约定方式。

Delphi的默认调用方式为Register

Cdecl是采用C/C++的调用约定，适用于C++语言编写的DLL；

safecall是适合于声明ole对象中的方法

#### 4. DLL数据作用范围

DLL可以与其相连的多个进程共享数据、变量和分配的内存，对于每个相关的进程来说也可以有其专用的数据、变量和分配的内存。因而，用户可以以每个线程为依据存储数据,从而使一个线程的多个实例具有其专用的数据

DLL函数中的局部变量的作用范围限于定义的函数之中.DLL源代码中的全局变量对每一个与DLL相连的进程都是全局的

为了实现不同进程中的数据共享。可以使用内存映像文件、注册表或者文件的读写、全局钩子、声明一个共享数据段(只适用于Windows9X)、套接字、管道、远程过程调用等技术。  **实际应用中推荐使用内存映像文件的方式实现数据共享**

#### 5. 调用方式

- 通过过程、函数名
- 通过过程、函数的别名
- 通过过程、函数的编号

#### 6. 创建DLL

```pascal
uses
  Vcl.Dialogs,
  System.SysUtils,
  System.Classes;
{$R *.res}
//建立过程
procedure Test;
begin
  ShowMessage('TestDLL.Test');
end;

//建立函数，stdcall为参数入栈方式
function add(a, b: Integer): Integer;stdcall;
begin

  Result := a + b;
end;

//导出列表
exports
  Test name 'my01',
  add;

begin

end.
```

#### 7. 改变项目的输出目录

将exe和dll文件输出到同一个目录中,项目目录结构如下：

```
├── Dll和主程序通讯(ProjectGroup)
    ├── bin
    └── 窗口程序(project)
    └── Dll程序（project）
```

具体步骤

选中项目(project)右击-->options-->Delphi Compiler-->Output Directory--->选择bin

#### 8. 调用测试

##### 8.1. 静态调用方式

主程序在调用该DLL 时，首先在interface 部分声明要调用的函数：

```pascal
//导出过程
procedure Test; external 'ProjectDll.dll';
//导出函数
function add(a, b: Integer): Integer;stdcall; external 'ProjectDll.dll';

procedure TForm1.btn1Click(Sender: TObject);
begin

  ShowMessage(add(10, 20).ToString);
end;

```

> PS：在其他工程调用，如果不在一个工程组，需要在相同目录下、System32下或指定路径；声明可以在实现区或接口区，这里的函数名要一致，甚至大小写。

##### 8.2. 动态调用方式

dll代码

```pascal
function sub(a, b: Integer): Integer; stdcall;
begin

  Result := a - b;
end;

exports
  sub;
```

interface部分声明

```pascal
type
//定义一个函数类型，参数要和需要的函数一致
  sub1 = function(a, b: Integer): Integer; stdcall;

  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
    MB: sub1;  {声明函数 MB}
    inst: LongWord;  {声明一个变量来记录要使用的 DLL 句柄}
  public
    { Public declarations }
  end;

```

调用实现

```pascal
procedure TForm1.btn2Click(Sender: TObject);
begin
  try
    //动态载入DLL ，并返回其句柄
    inst := LoadLibrary('ProjectDll.dll');
    if inst <> 0 then
    begin
      MB := GetProcAddress(inst, 'sub');
      ShowMessage(MB(30, 10).ToString);
    end;
  finally
    //记得释放
    FreeLibrary(inst);
  end;
end;
```

#### 9. 两种方式的优缺点

静态方法实现简单，易于掌握并且一般来说稍微快一点，也更加安全可靠一些；

但是静态方法不能灵活地在运行时装卸所需的DLL，而是在主程序开始运行时就装载指定的DLL直到程序结束时才释放该DLL，另外只有基于编译器和链接器的系统（如Delphi）才可以使用该方法。

动态方法较好地解决了静态方法中存在的不足，可以方便地访问DLL中的函数和过程，甚至一些老版本DLL中新添加的函数或过程；

但动态方法难以完全掌握，使用时因为不同的函数或过程要定义很多很复杂的类型和调用方法。对于初学者，建议使用静态方法，待熟练后再使用动态调用方法。

#### 10. 初始化和退出

一般的DLL不需要做初始化和善后工作，但如果想让DLL在被载入时先作一些初始设定，或者退出时释放资源，则可以用以下方法达到目的。

#### 11. 利用Unit的Initalization与Finalization这两个小节

可以在Unit的这两个小节中安排Unit的进入和退出，但是Program 与 Library并没有这两个部分，所以只能写在Unit中。

#### 12. 利用ExitProc变量释放资源

在Library的begin ..end.中间是可以写代码 的， 这里可以放置DLL初始化代码 。如果想要做善后工作，则可以利用ExitProc变量。我们首先在初始化代码 中 把ExitProc中包含的默认的善后过程地址保存下来，然后把自定义的过程的地址赋给它，这样DLL退出时就会执行我们制定的程序；在 自定义的过程的最后，把ExitProc恢复原来的默认值，以便DLL能够继续完成原来默认的善后工作。下面是示例：

```pascal
　　library MyDLL;
　　...
　　OldExitProc: pointer;
　　...
　　procedure MyExitProc;
　　begin
　　... //善后程序
　　ExitProc := OldExitProc;
　　end;
　　...
　　begin
　　... //初始化程序
　 　OldExitProc := ExitProc;
　　ExitProc := @MyExitProc;
　　end.
```

#### 13. 利用DllProc变量
和ExitProc一样，DllProc也是一个在Systemd单元中预定义的变量。 在使用DLLProc时, 必须先写好一个具有以下原型的程序:

```pascal

procedure DLLHandler(Reason: integer);

```

并在library的begin ..end.之间, 将这个DLLHandler程序的执行地址赋给DLLProc中, 这时就可以根据参数Reason的值分别作出相应的处理。另外注意要 将Windows单元加入uses子句。示例如下:

```pascal
　　library TestDLL;
　　...
procedure MyDLLHandler(Reason: integer);
begin
case Reason of
    DLL_PROCESS_DETACH:
      begin
        ShowMessage('整个DLL的善後程序');
      end;
    DLL_Process_Attach:
      begin
        ShowMessage('整个DLL的初始化代码 ');
      end;

    DLL_Thread_Attach:
      begin
        ShowMessage('当主叫端开始一个Thread时');
      end;
    DLL_Thread_Detach:
      begin
        ShowMessage('当主叫端终止一个Thread时');
      end;
end;

end;


//初始化代码
DLLProc := @MyDLLHandler;
MyDLLHandle(DLL_Process_Attach);
end.

```
当DLL支援多进程(Thread)的处理时,   DllProc非常适合使用。
