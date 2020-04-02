#### 1. 数组类型


##### 1.1. 定义格式

数组类型定义了一组指定类型的元素序列，在方括号中填入下标值就可访问数组中的元素。定义数组时，方括号也用来指定可能的下标值。例如，下面的代码中定义了一个有24个整数的数组：

```pascal
type
  DayTemperatures = array [1..24] of Integer;
```

在数组定义时，你需要在方括号中填入一个子界类型的值，或者用两个有序类型的常量定义一个新的子界类型，子界类型指定了数组的有效索引。由于子界类型指定了数组下标值的上界和下界，那么下标就不必象C、C++、JAVA和其他语言那样必须从零开始。

由于数组下标基于子界类型，因此Delphi 能够对它们进行范围检查。不合法的常量子界类型将导致一个编译时间错误；如果选上编译器范围检查选项，那么超出范围的下标值将导致一个运行时间错误。

##### 1.2. 应用实例

使用上述数组定义方法，定义一个DayTemperatures 类型的变量如下：

```pascal
type
  DayTemperatures = array [1..24] of Integer;

var
  DayTemp1: DayTemperatures;

procedure AssignTemp;
begin
  DayTemp1 [1] := 54;
  DayTemp1 [2] := 52;
  ...
  DayTemp1 [24] := 66;
  // compile-time error
  DayTemp1 [25] := 67;
  //这里是错的，因为每个元素都必须是Integer类型
  DayTemp1 [3] := 1.0;
```
##### 1.3. 定长数组

定长数组也就是长度在声明的时候就确定的，后面是不能改变的，而在定长数组里，起始序号不必从0开始，可以自己定，例如

```pascal
var
    A : Array[2..3] of Integer;
begin
    A[2] := 1;
    //这里会出错，因为定长数组不能再分配
    SetLength(A,3);
end;
```

从上面我们可以看到起始序号是2，但是步长是1，是不能改变的。为什么我们看到很多数组的起始序号是0呢？习惯而已。

来看一个特殊用法

```pascal
type
    THuangJacky = (hjA, hjB, hjC);
const
    //用法1
    B : Array[0..2] of string = ('A', 'B', 'C');
    //用法2
    C : Array[THuangJacky] of string('A', 'B', 'C');
var
    H : THuangJacky;
    S :String;
begin
    S := B[Ord(H)]);
    S := C[H];
    //B[H]和C[1]都会出错
end;
```

从上面的例子可以看出只要是有序数类型都可以当做数组的序号，但是我们用的时候序号就必须是声明的哪种序数类型，所以上面的代码注释中才会写出两种错误的情况


##### 1.4. 不定长数组

不定长数组：动态数组，也就是声明的时候没有说长度是多少，在使用之前必须声明，长度是可以再分配的，序号必须从0开始，例子

> PS：Delphi 4 的Object Pascal中增加了动态数组，所谓动态数组是在运行时动态分配内存改变数组大小。使用动态数组很容易

```pascal
var
    A : Array of Integer;    //定长数组可能的定义是 A :Array[0..10] of Integer
begin
    SetLength(A, 3);    //数组一共有3个元素
    A[0] := 1;
    A[1] := 2;
    A[2] := 3;
    //A[3]没有，因为只有3个元素

    //如果变长长度，直接增加后面的元素
    SetLength(A, 4);
    //现在增加了第四个元素，而前三个元素还是那三个
    A[3] := 4;
    //如果长度变短了，超出部分就会被去掉
    SetLength(A, 3);
    //现在A[3]没有了
end;
```


有时候，大家这样要先设定长度，在赋值，很麻烦，现在介绍一个一气呵成的招数

```pascal
type
    TA = Array of Integer;
var
    A : TA;
begin
    A := TA.Create(1, 2, 3);
    //此招请勿在Delphi 7上面使用

    //使用上面的方法之后，A[0] :=1, A[1] := 2， A[2]:=3
end;
```


##### 1.5. 一维和多维

前面的所有例子，所讨论的都是一维数组，要想弄一个矩阵（多维数组）怎么办？

```pascal
var
    A : Array[0..2, 0..2] of Integer;
    B : Array[0..2] of Array[0..2] of Integer;
begin
    A[0, 0] := 1;
    B[0, 0] := 1;
end;
```

两种方法都是可以的，下面介绍二维数组中的不定长数组

```pascal
var
    B : Array of Array of Integer
begin
    //设置一个3*3的矩阵
    SetLength(B, 3, 3);

    //如果需要实现齿状数组，必须像下面这么做
    SetLength(B, 3);
    SetLength(B[0], 1);
    SetLength(B[1], 2);
    SetLength(B[2], 3);
end;
```

##### 1.6. 序号相关

函数Low()和High()值得信赖，不过我们需要注意的是，他们返回的类型是我们数组的序号的那个类型，并不都是Integer，如前面例子中的THuangJacky

```pascal
var
    A : Array of array of string;
    I, J : Integer;
begin
    SetLength(A, 10);
    for I := Low(A) to High(A) do
    begin
        SetLength(A[I], I);
        for J := Low(A[I]) to High(A[I]) do
            A[I, J] := IntToStr(I) + ',' + IntToStr(J) + ' ';
        end
    end;
end.
```

使用数组时，你总要用标准函数Low和 High来检测它的边界，Low和 High返回下标的下界和上界。强烈建议使用Low和 High操作数组，特别是在循环中，因为这样能使代码与数组范围无关，如果你改变数组下标的范围声明，Low和 High代码不会受影响；否则，如果代码中有一个数组下标循环体，那么当数组大小改变时你就不得不更新循环体的代码。Low和 High将使你的代码更易于维护、更稳定。


> PS：顺便提一下，使用Low和 High不会增加系统运行额外开销。因为在编译时，他们已被转换成常数表达式，而不是实际函数调用。其他简单的系统函数也是这样。


##### 1.7. 数组长度

Length()函数返回的是Integer类型

```pascal
var
    A : Array of Integer;
begin
    SetLength(A, 2);
    Length(A);
end.
```

> 从上面的那个复制的例子我们可以看出来：定长数组变量就是一个变量，所以可以直接用 := 来赋值，而动态数组变量就是一个指针，如果用了 :=来赋值，两个变量就关联在一起了

```pascal
var
    A :Array[0..2] of Integer;
    B :Array of Integer;
begin
    //一样，从地址来看这个数组控件在栈上面
    ShowMessageFmt('A:%8x, A[0]:%8p', [Integer(@A), @A[0]]);
    SetLength(B, 3);
    //一样，这个数据空间在堆上面
    ShowMessageFmt('B:%8p, B[0]:%8p', [B, @B[0]]);
end
```




#### 2. 记录类型

##### 2.1. 定义格式
记录类型用于定义不同类型数据项的固定集合。记录中每个元素，或者说域，有它自己的类型。记录类型定义中列出了所有域，每个域对应一个域名，通过域名可以访问它。

下面简单列举了记录类型的定义、类型变量的声明以及这类变量的使用：

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
end;

```

类和对象可以看作是记录类型的扩展。Delphi 库趋向于用类替代记录类型，不过Windows API中定义了许多记录类型。

记录类型中允许包含variant 域，它表示多个域能公用同一内存区，而且域可以是不同类型（这相应于C语言中的联合union）。换句话说，你可以通过variant 域或说是一组域访问记录中同一个内存位置，但是各个值仍需区别对待。variant类型主要用来存贮相似但又不同的数据，进行与类型映射(typecasting)相似的类型转换（自从typecasting 引入Pascal，已很少用到这种方法了）。虽然Delphi在一些特殊情况下还在用variant 记录类型，但是现在已经被面向对象技术或其他现代技术代替了。

variant 记录类型的应用不符合类型安全原则，因此不提倡在编程中使用，初学者更是如此。实际上，专家级的编程人员确实需要用到variant 记录类型，Delphi 库的核心部分就用到了这一类型。不管怎样，除非你是个Delphi 专家，否则你应避免使用variant记录类型。


#### 3. 文件类型

##### 3.1. 定义格式

另一个Pascal特定的类型构造器是文件类型（file）。文件类型代表物理磁盘文件，无疑是Pascal语言的一个特殊类型。按下面的方式，你可以定义一个新的数据类型：

type
  IntFile = file of Integer;

然后，你就能打开一个与这个结构相应的物理文件、向文件中写入整数、或者从文件中读取当前的值。

Pascal 文件类型的使用很直观，而且Delphi 中也定义了一些控件用于文件保存和装载，以及对数据流和数据库的支持。






















