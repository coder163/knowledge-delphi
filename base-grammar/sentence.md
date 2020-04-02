
如果说数据类型是Pascal 编程的一个基础，那么另一个则是语句。编程语言的语句主要由关键字和操作指令组成。

#### 1. 简单语句和复合语句

Pascal 简单语句中不包含任何别的语句，赋值语句和过程调用即是简单语句的例子。简单语句用分号隔开，如下所示：

```pascal
X := Y + Z;  // 运算赋值
Randomize;   // 过程调用

```

用begin 和end 将简单语句括起来即组成复合语句，复合语句用法与普通的Pascal 语句相同，见下例：

```pascal
begin
  A := B;
  C := A * 2;
end;
```

end之前的最后一条语句末尾分号不是必需的，你可以写成：

```pascal
begin
  A := B;
  C := A * 2
end;
```

这两种写法都是正确的。第一种多了一个无用（但也无害）的分号。分号实际上是一个空语句，也就是说，是一个没有代码的语句。有时，空语句可用在循环体或其他特殊情况中。


> **注意：** 虽然最后一条语句末尾的分号没有用，我却总是加上它，并且建议你也这样做。因为有时你可能需要在末尾添加语句，如果最后没有加分号，你就必须记着加上它，与其如此不如一开始就加上它

#### 2. 赋值语句

在Pascal 语言中赋值语句用冒号-等号操作符“:=”，对使用其他语言的编程人员来说这是一个奇怪的符号。在其他语言中用作赋值符号的“=”在Pascal 中用作关系运算符，用于判断是否相等。

赋值和相等判断使用不同的符号，使Pascal 编译器（象C编译器一样）能更快解译源代码，因为这样就不需要通过检查上下文来判断符号的意义，此外使用不同操作符也使代码更易读。


#### 3. 条件语句

条件语句通过条件检测，判断是否执行该条件语句中包含的语句。条件语句可有两种基本形式：if语句和case语句。


##### 3.1. If语句


对if-then型语句， 仅当条件满足时，语句才执行；对if-then-else型，if语句在两条语句中选择一条执行。条件用布尔表达式建立，这里通过一个简单的Delphi 例子来示范如何写条件语句。

首先，创建一个应用程序，在form上面放两个复选框（check box）和四个按钮（button），不要改变复选框和按钮的名字，双击按钮为其OnClick 事件添加响应程序。下面是第一个按钮事件代码中一条简单的if语句：

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // simple if statement
  if CheckBox1.Checked then
    ShowMessage ('CheckBox1 is checked')
end;
```

如果点击按钮后没有反应，表明复选框未被选中。对于这种情况，最好能交代得更清楚些，为此在第二个按钮的代码中，我用了if-then-else 语句：

```pascal
procedure TForm1.Button2Click(Sender: TObject);
begin
  // if-then-else statement
  if CheckBox2.Checked then
    ShowMessage ('CheckBox2 is checked')
  else
    ShowMessage ('CheckBox2 is NOT checked');
end;
```

要注意的是，不能在第一句之后、else 关键词之前加分号，否则编译器将告知语法错误。实际上，if-then-else 语句是单纯的一条语句，因此不能在语句中间加分号。

if 语句可以很复杂，句子中的条件部分可以是一系列条件（用and、 or 、 not等布尔操作符联接起来），if语句又可以嵌套另一个if语句，见例IfTest中其它两个按钮的示范代码：

```pascal
procedure TForm1.Button3Click(Sender: TObject);
begin
  // statement with a double condition
  if CheckBox1.Checked and CheckBox2.Checked then
    ShowMessage ('Both check boxes are checked')
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  // compound if statement
  if CheckBox1.Checked then
    if CheckBox2.Checked then
      ShowMessage ('CheckBox1 and 2 are checked')
    else
      ShowMessage ('Only CheckBox1 is checked')
  else
    ShowMessage (
      'Checkbox1 is not checked, who cares for Checkbox2?')
end;
```

##### 3.2. Case语句

如果你的if语句变得非常复杂，有时可以用case语句代替它。case语句包括用来选值的表达式、可能值序列或一个取值范围。这些值应该是常量，并且它们必须唯一，而且应属于有序类型。

Case语句最后可以带一个else 语句，当没有一个标签与选择器的值一致时，执行else语句。下面是两个简单的例子：

```pascal
case Number of
  1: Text := 'One';
  2: Text := 'Two';
  3: Text := 'Three';
end;

case MyChar of
  '+' : Text := 'Plus sign';
  '-' : Text := 'Minus sign';
  '*', '/': Text := 'Multiplication or division';
  '0'..'9': Text := 'Number';
  'a'..'z': Text := 'Lowercase character';
  'A'..'Z': Text := 'Uppercase character';
else
  Text := 'Unknown character';
end;

```

#### 4. 循环语句

其它编程语言中使用的循环语句，Pascal语言中都有，它们包括 for、 while 和 repeat 语句。如果你用过其他编程语言，你会发现Pascal中的循环语句没什么特别的，因此这里我只作简要的说明。


##### 4.1. For循环

Pascal 中的for循环严格地建立在计数器基础上，循环每执行一次，计数器不是增加一个值就是减小一个值。下面是一个for语句的简例，用来将前十个数加起来：

```pascal
var
  K, I: Integer;
begin
  K := 0;
  for I := 1 to 10 do
    K := K + I;
```

同样的for语句可以用正好相反的计数器来写：

```pascal
var
  K, I: Integer;
begin
  K := 0;
  for I := 10 downto 1 do
    K := K + I;
```

Pascal 中的for循环语句其灵活性比其他语言小（它不能指定1之外的步长），不过简单也容易理解。

如果需判断的条件比较复杂，或想自定义计数器，你可以用while语句 或 repeat 语句，而不是for循环语句。


##### 4.2. while语句和repeat语句

while-do 循环语句和 repeat-until 语句的不同点在于repeat 循环语句的代码至少要执行一次。从下面的简例很容易理解这一点：

```pascal
while (I <= 100) and (J <= 100) do
begin
  // use I and J to compute something...
  I := I + 1;
  J := J + 1;
end;

repeat
  // use I and J to compute something...
  I := I + 1;
  J := J + 1;
until (I > 100) or (J > 100);
```

从上可见即使 I 或 J 的初始值大于100，repeat-until循环中的代码也仍会执行一次。

> **注意：**  repeat-until 循环的条件是反向的条件，只要不满足这个条件，循环就执行；当条件满足时，循环终止。这正好与while-do 循环相反，while-do 循环当条件是真值时才执行。为此，我不得不在上面代码中用反向条件来获得相同的结果。

##### 4.3. 一个循环语句例子

这个循环例子表现了固定计数器循环和随机计数器循环之间的差别

```pascal
object Form1: TForm1
  Caption = 'Loops'
  object ListBox1: TListBox ...
  object BtnFor: TButton
    Caption = '&For'
    OnClick = BtnForClick
  end
  object BtnWhile: TButton
    Caption = '&While'
    OnClick = BtnWhileClick
  end
end
```

```pascal
procedure TForm1.BtnForClick(Sender: TObject);
var
  I: Integer;
begin
  ListBox1.Items.Clear;
  for I := 1 to 20 do
    Listbox1.Items.Add ('String ' + IntToStr (I));
end;
```

```pascal
procedure TForm1.BtnWhileClick(Sender: TObject);
var
  I: Integer;
begin
  ListBox1.Items.Clear;
  Randomize;
  I := 0;
  while I < 1000 do
  begin
    I := I + Random (100);
    Listbox1.Items.Add ('Random Number: ' + IntToStr (I));
  end;
end;

```

> **注意：** 用 Break 和 Continue 系统过程可以改变循环执行的标准流程。Break 中断循环；Continue直接跳至循环测试句，或使计数器增加一个步长，然后继续循环（除非条件为空或计数器达到最大值）。还有两个系统过程 Exit 和 Halt，让你立即从函数或过程中返回，或者终止程序。


#### 5. With语句

With语句是Pascal编程语言独有的语句，不过最近JavaScript 和Visual Basic也添加了这种语句，它在Delphi程序设计中很有用。

With语句是一种用于简化代码的语句。 如你要访问一个记录类型变量（或一个对象），用With语句就不必每次重复变量的名字。例如对于以下的记录类型代码：

```pascal
type
  Date = record
    Year: Integer;
    Month: Byte;
    Day: Byte;
  end;

var
  BirthDay: Date;

begin
  BirthDay.Year := 1997;
  BirthDay.Month := 2;
  BirthDay.Day := 14;
```

可以用with语句改进后半部分代码，如下：

```pascal
begin
  with BirthDay do
  begin
    Year := 1995;
    Month := 2;
    Day := 14;
  end;
```

在Delphi程序中，这种方法能用于访问控件和类变量。现在通过with语句访问列表框的条目，我们重写上面循环例子的最后部分：

```pascal
procedure TForm1.WhileButtonClick(Sender: TObject);
var
  I: Integer;
begin
  with ListBox1.Items do
  begin
    Clear; // shortcut
    Randomize;
    I := 0;
    while I < 1000 do
    begin
      I := I + Random (100);
      // shortcut:
      Add ('Random Number: ' + IntToStr (I));
    end;
  end;
end;
```

当编写的代码很复杂时，with语句会很有用，也可省去一些临时变量。但是这样做也有缺点，因为这样将使代码的可读性变差，特别对有相似或相同属性的对象。

更严重的是，使用with语句可能会在代码中融入微妙的逻辑错误，甚至连编译器都难以发现。例如


```pascal
with Button1 do
begin
  Width := 200;
  Caption := 'New Caption';
  Color := clRed;
end;
```

这段代码改变了按钮的Caption 和 Width属性，但也改变了窗体的Color属性，而不是按钮的颜色！其原因是 TButton 控件没有Color属性, 又由于执行的代码是针对窗体对象的（我们正在写窗体的方法），所以窗体对象即成为默认的访问对象。如果这样写：

```pascal
Button1.Width := 200;
Button1.Caption := 'New Caption';
Button1.Color := clRed; // error!
```

编译器会给出一个错误。通常，由于with语句在当前的块中定义了新的标识符，省略了原有的标识符，可能引起在同一块内错误地访问另一个标识符（就象上面的这段代码）。即使存在种种缺陷，我还是建议你习惯于使用with语句，因为with语句确实是非常便利，并且有时也会使代码更容易读懂。

然而，你应该避免使用多个with语句，如

```pascal
with ListBox1, Button1 do..

```

这样会使后面的代码非常难读，因为，对该块中定义的每个属性，你都要根据相应的属性以及控件的次序，才能推出所访问的控件。

#### 6. 其他语句：break、Continue、Exit等

##### 6.1. break

强制退出最近的一层循环（注意：只能放在循环里；而且是只能跳出最近的一层循环），用于从for、while、repeat语句中强制退出

类似于C/C++等语言中的break的功能

##### 6.2. Continue

用于从for、while、repeat语句中结束循环内的本次处理,继续从循环体的开始位置继续执行

类似于C/C++等语言中的continue的功能

##### 6.3. Exit

用于从当前代码块中退出。

若该代码是主程序，则终止该程序。

如果是函数或过程，则立即终止该函数或过程


##### 6.4. abort

终止程序需的运行，产生不报错的异常信息。跳出祖先模块。

##### 6.5. halt

用于强行终止应用程序的执行，返回操作系统（非正常退出方式）


##### 6.6. runerror

终止程序的执行，并产生运行错误（返回错误代码）
