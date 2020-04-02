

#### 1. 历史
在计算的早期，所有编程基本上都是单线程的。您通过在卡片或磁带上打孔来创建程序，将您的卡片组提交给本地计算中心，几天后，您又收到了另一副卡片，如果您幸运的话，其中包含所需的结果。所有处理都是批处理，不是时间关键，先到先得，当程序运行时，它只能使用计算机的时间。

事情已经发生了变化。多线程执行的概念首先出现在时间共享系统中，其中不止一个人可以同时登录到中央大型计算机。重要的是确保机器的处理时间在所有用户之间公平分配，并且当时的操作系统使用“过程”和“线程”概念。台式电脑也有类似的进展。早期的DOS和Windows系统都是单一任务。您的程序仅在计算机上运行，​​或者根本不运行。随着越来越复杂的应用程序和对个人计算机的不断增长的需求，特别是在图形和网络领域的高性能方面，多进程和多线程操作系统现在已经司空见惯。

#### 2. 定义

定义的第一个概念是过程的概念。大多数Windows 95,98和NT用户都对进程的内容有一个很好的直观概念。他们将其视为一个在机器上运行的程序，与其他程序共存并共享CPU，磁盘和内存资源。程序员知道一个过程是可执行代码的调用，这样该代码具有唯一的存在，并且该过程执行的指令以有序的方式执行。总的来说，进程是孤立执行的。它们使用的资源（内存，磁盘，I / O，CPU时间）是虚拟化的，因此每个进程都有自己的一组虚拟资源，不受其他进程的影响。操作系统提供此虚拟化。进程执行模块代码 这些可能是不相交的; 从某种意义上说，包含Windows资源管理器和Microsoft Word的代码的可执行模块是不相交的。但是，它们也可以共享，就像DLL的情况一样。DLL的代码通常在许多不同进程的上下文中执行，通常是同时执行。执行指令总体上不是在进程之间排序的：Microsoft Word不会因为打印后台处理程序当前向打印机发送内容而停止打开文档！

线程是在明确希望有一个应用程序以更松散的时间有序的方式执行一系列动作时开发的，可能同时执行多组动作。在某些操作会在一个执行线程中导致相当大的延迟（例如，等待用户做某事）的情况下，通常希望程序仍然同时执行其他操作（例如，后台拼写检查或处理传入网络）消息）。但是，为每个并发操作创建一个全新进程，然后让进程进行通信的开销往往是一个过多的开销。

#### 3. 一个例子

如果需要寻找一个好的多线程示例，那么Windows资源管理器（即Windows Shell）就是一个很好的例子。双击“我的电脑”，然后单击几个子文件夹，随时创建新窗口。现在在其中一个窗口上调用冗长的复制操作。弹出进度条，该特定窗口不响应用户输入。但是，所有其他窗口都是完全可用的。显然，有几件事情同时发生，但只有一份explorer.exe正在运行。这是多线程的本质。

#### 4. 时间切片

在大多数支持多线程的系统中，可能有许多用户在计算机系统上同时发出请求。通常，系统中的物理处理器数量少于可能并行运行的线程数量。大多数系统都支持**时间切片**，也称为**先发制人的多任务处理**，以解决这个问题。在时间切片的系统中，线程会运行一段时间，然后被抢占; 也就是说，硬件计时器触发，导致操作系统重新评估应运行哪些线程，可能停止对当前运行的线程执行，以及运行最近未执行的其他线程。这甚至允许单处理器机器运行多个线程。在PC上，时间片往往大约是五十五毫秒。

#### 5. 为何使用线程？

线程不应该改变程序的语义。他们只是改变了操作的时间。因此，它们几乎总是被用作与性能相关的问题的优雅解决方案。以下是您可能使用线程的一些示例：

- 进行冗长的处理：当Windows应用程序正在计算时，它无法再处理任何消息。结果，无法更新显示。
- 进行后台处理：某些任务可能不是时间关键，但需要连续执行。
- 执行I / O工作：磁盘或网络的I / O可能会出现不可预测的延迟。线程允许您确保I / O延迟不会延迟应用程序的不相关部分。

所有这些示例都有一个共同点：在程序中，某些操作会导致潜在的大延迟或CPU占用，但这种延迟或CPU使用对于其他操作是不可接受的; 他们现在需要得到服务。当然还有其他各种好处，这里有：

- 使用多处理器系统：您不能指望只有一个线程的应用程序可以使用两个或更多处理器！
- 高效的时间共享：使用线程和进程优先级，您可以确保每个人都获得公平的CPU时间分配。

明智地使用线程会将缓慢，笨重，不那么响应的程序变成清晰响应，高效，快速的程序，并且可以从根本上简化各种性能和可用性问题。



#### 6. 多线程API实现


```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function MyFun(p:Pointer):integer;stdcall; {工作线程调入函数，stdcall用于多个线程排序以及系统级别调用加此关键字}
var
  i:integer;
begin
  for i := 0 to 500000 do    
  begin
    with Form1.Canvas do
    begin
      Lock;
      TextOut(50,10,IntToStr(i)); {50和10是坐标X和Y}
      Unlock;
      Application.ProcessMessages;
    end;
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);{主线程}
var
  i:integer;
begin
  for i := 0 to 500000 do  
  begin
    with Form1.Canvas do
    begin
      Lock;
      TextOut(10,10,IntToStr(i)); {10和10是坐标X和Y}
      Unlock;
      Application.ProcessMessages;{加上去才在计数时不会卡住，拖动窗体时，计数会有停顿}
    end;
  end;

end;

procedure TForm1.btn2Click(Sender: TObject);{工作线程，拖动窗口时计数不会停顿，因为和主线程分开工作了}
var
  ID:THandle; {用于接收线程返回句柄，也可以用DWORD}
begin
  CreateThread(nil,0,@MyFun,nil,0,ID);  {API创建线程}
end;

end.
```

1. CriticalSection(临界区)：

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    lst1: TListBox;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  CS:TRTLCriticalSection; {声明临界}

function MyFun(p:Pointer):integer;stdcall;
var
  i:integer;
begin
  EnterCriticalSection(CS);  {我要用了，别人先别用}
  for i := 0 to 100 - 1 do
  begin
    Form1.lst1.Items.Add(IntToStr(i));
  end;
  LeaveCriticalSection(CS);  {我用完了，别可以用了}

end;

procedure TForm1.btn1Click(Sender: TObject);
var
  ID:THandle;
begin
  InitializeCriticalSection(CS); {初始化临界}
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteCriticalSection(CS);  {删除临界}
end;

end.
```

2. Mutex (互斥对象)

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  hMutex:THandle; {声明互斥变量句柄}
  f:Integer;      {用于协调输出位置的变量}

function MyFun(p:Pointer):Integer;stdcall;
var
  i,y:integer;
begin
  Inc(f);  {步进f}
  y:=20*f;
  if WaitForSingleObject(hMutex,INFINITE)=WAIT_OBJECT_0 then   {等待函数}
  begin
    for i := 0 to 500 do
    begin
      with Form1.Canvas do
      begin
        Lock;
        TextOut(10,Y,IntToStr(i));
        Unlock;
        sleep(1); {太快怕忙不过来}
      end;
    end;
    ReleaseMutex(hMutex);
  end;
end;  


procedure TForm1.btn1Click(Sender: TObject);
var
  ID:THandle;
begin
  f:=0; {初始化f为0}
  Repaint; {重画}
  CloseHandle(hMutex); {先关闭句柄}
  hMutex:=CreateMutex(nil,False,nil);  {创建互斥体}
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseHandle(hMutex);  {关闭句柄}
end;

end.
```

3. Semaphore(信号或叫信号量)

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    edt1: TEdit;
    procedure btn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  hsmaphore:THandle; {信号量句柄}
  f:Integer;         {协调输出的变量}

function MyFun(p:Pointer):integer;
var
  i,y:integer;
begin
  Inc(f);
  y:=20*f;
  if WaitForSingleObject(hsmaphore,INFINITE)=WAIT_OBJECT_0 then
  begin
    for i := 0 to 500 do
    begin
      with Form1,Canvas do
      begin
        Lock;
        TextOut(10,y,IntToStr(i));
        Unlock;
        Sleep(1);
      end;
    end;
    ReleaseSemaphore(hsmaphore,1,nil); {释放函数}
  end;
  Result:=0;
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  ID:DWORD;
begin
  CloseHandle(hsmaphore);  {先关闭句柄}
  hsmaphore:=CreateSemaphore(nil,StrToInt(edt1.Text),5,nil); {创建句柄}
  CreateThread(nil,0,@MyFun,nil,0,ID);   {创建线程}
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
  CreateThread(nil,0,@MyFun,nil,0,ID);
end;

procedure TForm1.btn1KeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['1'..'5']) then Key:=#0;  {设置只能输入1到5，并且在控件属性设置宽度为1}
  
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseHandle(hsmaphore);  {关闭句柄}
end;

end.
```

4. Event (事件对象)

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  hEvent:THandle;
  f:integer;

function MyFun (p:Pointer):Integer;
var
  i,y:integer;
begin
  Inc(f);
  y:=20*f;
  for i := 0 to 200000 do
  begin
    if WaitForSingleObject(hEvent,INFINITE)=WAIT_OBJECT_0 then
    begin
      Form1.Canvas.Lock;
      Form1.Canvas.TextOut(10,y,IntToStr(i));
      Form1.Canvas.Unlock;
      
    end;
  end;
  Result:=0;
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  ID:DWORD;
begin
  Repaint;  {重画}
  f:=0;
  CloseHandle(hEvent);{先关闭线程}
  hEvent:=CreateEvent(nil,True,True,nil)  {创建事件}
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  ID:DWORD;
begin
  CreateThread(nil,0,@MyFun,nil,0,ID);  {创建线程}

end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  ResetEvent(hEvent); {暂停,可对当前所有事件相关线程暂停}
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  SetEvent(hEvent);  {启动，可对当前所有事件相关线程启动}
end;

procedure TForm1.btn5Click(Sender: TObject);
begin
  PulseEvent(hEvent); {启动一次再暂停，可对当前所有事件相关线程}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  btn1.Caption := '创建 Event 对象';
  btn2.Caption := '创建线程';
  btn3.Caption := 'ResetEvent';
  btn4.Caption := 'SetEvent';
  btn5.Caption := 'PulseEvent';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseHandle(hEvent); {关闭事件句柄}
end;

end.
```


#### 7. TThread实现

1. 注意事项

要记住Execute()需要经常地检查Terminated属性的值，来确认是否要提前退出。尽管这将意味着当使用线程工作的时候，你必须关心更多的事情，但它能确保在线程结束时，能够完成必要的清除

在某些紧急情况下，你可以使用Win32 API函数 TerminateThread()来终止一个线程。但是，除非没有别的办法了，否则不要使用它。例如，当线程代码陷入死循环中


如果选择这个函数，应该考虑到它的负面影响。首先，此函数在Windows NT与在Windows95/98下并不相同。在Windows95/98下，这个函数能够自动清除线程所占用的栈；而在Windows NT下，在进程被终止前栈仍被保留。其次，无论线程代码中是否有try...finally块，这个函数都会使线程立即终止执行。这意味着，被线程打开的文件没有被关闭、由线程申请的内存也没有被释放等情况。而且，这个函数在终止线程的时候也不通知DLL，当DLL关闭的时候，这也容易出现enti问题

2. 线程类

```pascal
unit UnitThread;

interface

uses
  Vcl.Forms, Vcl.Dialogs, System.SysUtils, System.Classes;

type
  TMyThread = class(TThread)
  protected
    procedure Execute; override;
  end;

implementation

uses
  UnitMain;

{ TMyThread }

procedure TMyThread.Execute;
var
  I: Integer;
begin
  FreeOnTerminate := False;
  I := 1;
  while True do begin
    if FreeOnTerminate then
      Exit;

    Synchronize(
      procedure
      begin
        form1.lbl1.Caption := '线程ID：' + Self.ThreadID.ToString + '：' + I.ToString;
      end);
    TThread.Sleep(300);
    I := I + 1;
  end;

end;

end.

```

3. 界面类

```pascal
unit UnitMain;

interface

uses
  System.Generics.Collections, UnitThread, Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    lbl1: TLabel;
    btn4: TButton;
    btn5: TButton;
    btn6: TButton;
    lbl2: TLabel;
    btn7: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Thread: TMyThread;
  ThreadList: TList<TMyThread>;

implementation

{$R *.dfm}
{启动线程}
procedure TForm1.btn1Click(Sender: TObject);
begin
  Thread := TMyThread.Create(True);
  Thread.Start;
end;

{暂停线程}
procedure TForm1.btn2Click(Sender: TObject);
begin
  Thread.Suspended := True;
end;

{继续线程}
procedure TForm1.btn3Click(Sender: TObject);
begin
  Thread.Resume;
end;


{批量启动多个线程}
procedure TForm1.btn4Click(Sender: TObject);
begin
  ThreadList := TList<TMyThread>.Create;

  TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
      Mthread: TMyThread;
    begin
      for I := 0 to 10 do begin
        Mthread := TMyThread.Create(True);
        Mthread.Start;

        ThreadList.Add(Mthread);
        Form1.lbl2.Caption := I.ToString;
        TThread.Sleep(200);
      end;
    end).Start;

end;

 {批量暂停多个线程}
procedure TForm1.btn5Click(Sender: TObject);
begin

  TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
    begin
      for I := 0 to ThreadList.Count do begin
        ThreadList[I].Suspended := True;
      end;
    end).Start;

end;

{批量继续多个线程}
procedure TForm1.btn6Click(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
    begin
      for I := 0 to ThreadList.Count do begin
        ThreadList[I].Resume;
      end;
    end).Start;
end;

{批量终止多个线程}
procedure TForm1.btn7Click(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
    begin
      for I := 0 to ThreadList.Count do begin
        ThreadList[I].FreeOnTerminate := True;
//      TerminateThread(ThreadList[I].Handle, 0);
      end;
    end).Start;
end;

end.

```

4. 临界区代码

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMyThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override; {执行}
    procedure Run;  {运行}
  end;
  TForm1 = class(TForm)
    btn1: TButton;
    lst1: TListBox;
    procedure btn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



var
  Form1: TForm1;


implementation

{$R *.dfm}

uses SyncObjs;

var
  MyThread:TMyThread;   {声明线程}
  CS:TCriticalSection; {声明临界}


procedure TMyThread.Execute;
begin
  { Place thread code here }
  FreeOnTerminate:=True; {加上这句线程用完了会自动注释}
  Run;     {运行}
end;

procedure TMyThread.Run;
var
  i:integer;
begin
  CS.Enter;  {我要用了，其它人等下}
  for i := 0 to 100 - 1 do
  begin
    Form1.lst1.Items.Add(IntToStr(i));
  end;
  CS.Leave;  {我用完了，下一个}
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  CS:=TCriticalSection.Create;     {实例化临界}
  MyThread:=TMyThread.Create(False); {实例化这个类，为False时立即运行，为True时可加MyThread.Resume用来启动}
  MyThread:=TMyThread.Create(False);
  MyThread:=TMyThread.Create(False);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  CS.Free;{释放临界体}
end;

end.

```

5. Mutex (互斥对象)

uses SyncObjs;用TMutex类的方法处理(把释放语句放在循环内外可以决定执行顺序)

例：互斥输出三个0~2000的数字到窗体在不同位置。


```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMyThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override; {执行}
    procedure Run;  {运行}
  end;
  TForm1 = class(TForm)
    btn1: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



var
  Form1: TForm1;


implementation

{$R *.dfm}

uses SyncObjs;

var
  MyThread:TMyThread;   {声明线程}
  Mutex:TMutex; {声明互斥体}
  f:integer;


procedure TMyThread.Execute;
begin
  { Place thread code here }
  FreeOnTerminate:=True; {加上这句线程用完了会自动注释}
  Run;     {运行}
end;

procedure TMyThread.Run;
var
  i,y:integer;
begin
  Inc(f);
  y:=20*f;
  for i := 0 to 2000  do
  begin
    if Mutex.WaitFor(INFINITE)=wrSignaled then   {判断函数，能用时就用}
    begin
      Form1.Canvas.Lock;
      Form1.Canvas.TextOut(10,y,IntToStr(i));
      Form1.Canvas.Unlock;
      Sleep(1);
      Mutex.Release; {释放，谁来接下去用}
    end;
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  f:=0;
  Repaint;
  Mutex:=TMutex.Create(False);  {参数为是否让创建者拥有该互斥体，一般为False}
  MyThread:=TMyThread.Create(False);
  MyThread:=TMyThread.Create(False);
  MyThread:=TMyThread.Create(False);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Mutex.Free;{释放互斥体}
end;

end.
```

6. Semaphore(信号或叫信号量)

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses SyncObjs;
var
  f: Integer;
  MySemaphore: TSemaphore;

function MyThreadFun(p: Pointer): DWORD; stdcall;
var
  i,y: Integer;
begin
  Inc(f);
  y := 20 * f;
  if MySemaphore.WaitFor(INFINITE) = wrSignaled then
  begin
    for i := 0 to 1000 do
    begin
      Form1.Canvas.Lock;
      Form1.Canvas.TextOut(20, y, IntToStr(i));
      Form1.Canvas.Unlock;
      Sleep(1);
    end;
  end;
  MySemaphore.Release;
  Result := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ThreadID: DWORD;
begin
  if Assigned(MySemaphore) then MySemaphore.Free;
  MySemaphore := TSemaphore.Create(nil, StrToInt(Edit1.Text), 5, ''); {创建，参数一为安全默认为nil，参数2可以填写运行多少线程，参数3是运行总数，参数4可命名用于多进程}

  Self.Repaint;
  f := 0;
  CreateThread(nil, 0, @MyThreadFun, nil, 0, ThreadID);
  CreateThread(nil, 0, @MyThreadFun, nil, 0, ThreadID);
  CreateThread(nil, 0, @MyThreadFun, nil, 0, ThreadID);
  CreateThread(nil, 0, @MyThreadFun, nil, 0, ThreadID);
  CreateThread(nil, 0, @MyThreadFun, nil, 0, ThreadID);
end;

{让 Edit 只接受 1 2 3 4 5 五个数}
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['1'..'5']) then Key := #0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := '1';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(MySemaphore) then MySemaphore.Free;
end;

end.
```

7. Event (事件对象)

注：相比API的处理方式，此类没有启动步进一次后暂停的方法。


```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMyThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
    procedure Run;
  end;

  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses SyncObjs;

var
  f:integer;
  MyEvent:TEvent;
  MyThread:TMyThread;

{ TMyThread }


procedure TMyThread.Execute;
begin
  inherited;
  FreeOnTerminate:=True; {线程使用完自己注销}
  Run;
end;

procedure TMyThread.Run;
var
  i,y:integer;
begin
  Inc(f);
  y:=20*f;

  for i := 0 to 20000 do
  begin
    if MyEvent.WaitFor(INFINITE)=wrSignaled then    {判断事件在用没，配合事件的启动和暂停，对事件相关线程起统一控制}
    begin
      Form1.Canvas.lock;
      Form1.Canvas.TextOut(10,y,IntToStr(i));
      Form1.Canvas.Unlock;
      Sleep(1);
    end;

  end;

end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  Repaint;
  f:=0;
  if Assigned(MyEvent) then MyEvent.Free;    {如果有，就先销毁}

  {参数1安全设置，一般为空；参数2为True时可手动控制暂停，为Flase时对象控制一次后立即暂停
  参数3为True时对象建立后即可运行，为false时对象建立后控制为暂停状态，参数4为对象名称，用于跨进程，不用时默认''}
  MyEvent:=TEvent.Create(nil,True,True,'');   {创建事件}

end;

procedure TForm1.btn2Click(Sender: TObject);
var
  ID:DWORD;
begin
  MyThread:=TMyThread.Create(False);      {创建线程}
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  MyEvent.SetEvent;    {启动}  {事件类没有PulseEvent启动一次后轻描谈写}
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  MyEvent.ResetEvent;  {暂停}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   btn1.Caption:='创建事件';
   btn2.Caption:='创建线程';
   btn3.Caption:='启动';
   btn4.Caption:='暂停';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MyEvent.Free;        {释放}
end;

end.
```
