#### 1. 概述

Pascal 中有多种预定义的数据类型，它们可分为三大类：有序数据类型，实数类型和字符串类型。

Delphi 还包括一种无类型的可变数据类型，称作variant，variant是一种无需类型检测的数据类型，它在Delphi 2 中引入，用于处理OLE Automation（OLE 自动化）。

#### 2. 有序类型

有序类型是建立在概念“顺序”或“序列”基础上的数据类型。你不仅可比较两个有序值的大小，而且可以求取给定有序值的前驱及后继，或者计算它们的最大或最小值。

三种最重要的预定义有序类型是整数类型、布尔类型和字符类型（Integer,Boolean,Char)。各种类型根据其内部表示和取值范围不同又可进一步细分。表3.1列出了表示数字的有序数据类型。

<table border="1" bordercolor="Black" cellspacing="0" cellpadding="5">
<tbody><tr>
	<th width="20%" align="LEFT">大小</th>
	<th width="40%" align="LEFT">有符号值域</th>
	<th width="40%" align="LEFT">无符号值域</th>
</tr>
<tr>
	<td>8 bits</td>
	<td>ShortInt<br>-128 to 127</td>
	<td>Byte<br>0 to 255</td>
</tr>
<tr>
	<td>16 bits</td>
	<td>SmallInt<br>-32768 to 32767</td>
	<td>Word<br>0 to 65,535</td>
</tr>
<tr>
	<td>32 bits</td>
	<td>LongInt<br>-2,147,483,648 to 2,147,483,647</td>
	<td>LongWord (从 Delphi 4)<br>0 to 4,294,967,295</td>
</tr>
<tr>
	<td>64 bits</td>
	<td>Int64</td>
	<td><br></td>
</tr>
<tr>
	<td>16/32 bits</td>
	<td>Integer</td>
	<td>Cardinal</td>
</tr>
</tbody></table>

从表中可看到，不同数据类型与不同的数据表示法相对应，这要取决于数据值的数位和符号位。有符号类型的数值可正可负，但取值范围较小，因为符号位占一个数位。

表中最后一组类型标志着16/32，它表明其数值表示方法在16位和32位Delphi中不同，该组的Integer及Cardinal 类型比较常用，因为它们与CPU内部的数字表示法相对应

Pascal 语言和Delphi System 单元中定义了一系列有序类型操作例程，C++ 程序员会注意到其中的Inc 例程，它可与 ++ 和 += 运算符对应（Dec 例程也同样）

<table border="1" cellspacing="0" cellpadding="3" bordercolor="Black">
<tbody><tr align="LEFT"><th>例程</th><th>作用</th>
</tr><tr><td>Dec</td><td>将例程中的参数值递减1或一个特定的值，其中特定值可在第二个可选参数中定义
</td></tr><tr><td>
Inc</td><td>将例程中的参数值增加1或一个特定的值
</td></tr><tr><td>Odd</td><td>如果参数为奇数返回真
</td></tr><tr><td>Pred</td><td>根据参数在其数据类型定义中的序列，返回参数值的前驱值
</td></tr><tr><td>Succ</td><td>返回参数值的后继值
</td></tr><tr><td>Ord</td><td>返回参数值在其数据类型值集合中的序号
</td></tr><tr><td>Low</td><td>返回参数对应的有序数据类型的最小取值
</td></tr><tr><td>High</td><td>返回参数对应的有序数据类型的最大取值
</td></tr>
</tbody></table>

> 注意，当有些例程用于常量时，编译器会自动用计算值替代例程。例如你调用High(X) ，设定X为一个整数，那么编译器会用整数类型中最大的可能值代替这个表达式。

#### 3. 布尔类型

布尔值不同于布尔类型，平时很少用到。ByteBool、 WordBool 和LongBool这三种布尔类型的布尔值比较特殊，只在Windows API 函数中才用到它们。

在Delphi 3 中，为了与Visual Basic 和 OLE Automation兼容，修改了ByteBool、 WordBool 和LongBool的布尔值，将TRUE值设置为1，FALSE值仍为0；Boolean类型布尔值保持不变（TRUE为1，FALSE为0）。如果在Delphi 2代码中使用了布尔值显式类型转换 ，那么在以后的Delphi中可能会出错。

#### 4. 实数类型

实数类型代表不同格式的浮点数。Single类型占的字节数最小，为4个字节；其次是Double 浮点类型，占8个字节；Extended 浮点类型，占10个字节。这些不同精度的浮点数据类型都与IEEE（ 电气和电子工程师协会）标准的浮点数表示法一致，并且 CPU数字协处理器直接支持这些类型，处理速度也最快

Real 类型在Delphi 2 和 Delphi 3 中的定义与 16 位版本一样，都占 6 个字节。不过Borland公司一直不提倡使用这种类型，而建议用Single、 Double、 Extended 类型代替。这是由于 Real 这种 6 字节的旧格式既不受 Intel CPU 的支持，又没有列在官方的IEEE 实型中。为了完全解决这一问题，Delphi 4 不得不修改 Real 类型的定义，将其改成标准的 8 字节浮点型， 由此引起了兼容性问题，不过如果有必要，你可以采用下面编译指令克服兼容性问题，恢复Delphi 2 和 Delphi 3 的Real 类型定义：

```pascal
{$REALCOMPATIBILITY ON}
```

另外还有两种奇怪的数据类型：Comp 类型和Currency 类型，Comp 类型用 8 个字节描述非常大的整数（这种类型可支持带有 18 位小数的数字）；Currency 类型 (16 位版的Delphi不支持该类型) 表示一个有四位小数位的值，它的小数位长度是固定的，同Comp 类型一样也占 8 个字节。正如名字所示，Currency 数据类型是为了操作很精确的四位小数货币数值才添加的。

对实型数据，我们没办法编一个类似Range的程序，因为High 、Low及 Ord函数不能用于实型值。理论上说实型类型代表一个无限的数字集合；有序类型代表一个有限的数字集合。

因此，如问Char 类型字符 w 的顺序位置是有意义的， 但同样的问题对浮点类型数 7134.1562 就毫无意义。对于一个实型数，你能确切知道有没有比它大的实型数，但是，如想探究给定的实数前到底有多少个实型数（这是Ord 函数的作用），是得不到结果的。

实型类型在用户界面编程中用得不多，但是Delphi从各方面支持实型类型，包括在数据库方面的支持。由于支持IEEE浮点数运算标准，Object Pascal 语言完全适合于各类数值计算编程。如果对这部分感兴趣，你可以参考Delphi 在System单元中提供的算术函数（详细见Delphi 帮助）。

> 注意：Delphi 带有一个Math 单元，其中定义了一些高级数学例程，这些例程包括三角函数（如ArcCosh 函数）、金融函数（如InterestPayment 函数）和统计函数（如MeanAndStdDev 过程）。有些例程，它的名字听起来很怪，如MomentSkewKurtosis 例程，它是作什么用的呢? 还是留你自己查吧。


#### 5. 强制类型转换



强制类型转换时一种技术，通过它能够使编译器把一种类型的变量当做另一种类型。

由于Pascal有定义新类型的功能，因此编译器在调用一个函数时候对形参和实参类型匹配的检查是非常严格的。因此为了能够通过编译器检查，经常需要把一个变量的类型转换为另一个变量的类型。例如：假定要把一个字符类型的值赋给一个byte类型的变量：

```pascal

var
    c: char;
    b: byte;
begin
    c:= 'a';
    b:= c;    //编译器要提示错误
end.
```

在下面的代码中，强制类型转换把c转换成byte类型，事实上强制类型转换是告诉编译器你知道你正在做什么，并要把一种类型转换为另一种类型：

```pascal
var
    c: char;
    b: byte;
begin
    c:= 's';
    b:= byte(c);    //编译器不会报错
end.
```

注意：只有当两个类型的数据长度一样的时候，才能对变量进行强制类型转换。例如，不能把一个Double强制转换为Integer。


为了能把一个浮点型转换为一个整型，要用Trunc()或者Round()函数。为了把整型转换成一个浮点数类型的值，用下面的赋值语句：

```pascal
FloatVar:= intVar;
//直接将Integer型变量赋值给Double型变量
```




#### 6. 特定的Windows 类型

到目前为止，我们所看到的预定义数据类型都是Pascal 语言自身定义的类型。 Delphi 中还包含Windows系统定义的数据类型，这些数据类型不是Pascal语言的组成部分，而是Windows 库的一部分。Windows 类型包括新增的缺省类型（例如DWORD 或UINT）、各种记录（或结构）类型及指针类型等。

##### 6.1. 类型映射及类型转换

正如所知，你不能把一个变量赋给另一个不同类型的变量，如果你需要这么做，有两种方法供选择。第一种方法是采用类型映射（Typecasting），它使用一个带有目标数据类型名的函数符号：

```pascal
var
  N: Integer;
  C: Char;
  B: Boolean;
begin
  N := Integer ('X');
  C := Char (N);
  B := Boolean (0);
```

你可以在字节长度相同的数据类型之间进行类型映射。在有序类型之间或实型数据之间进行类型映射通常是安全的，指针类型及对象之间也可以进行类型映射 ，只要你明白自己在做什么。

然而，一般来说类型映射是一种较危险的编程技术，因为它允许你访问一个似是而非的值，该值好象是其它值的替身。由于数据类型的内部表示法之间通常互相不匹配，所以当遇到错误时会难以追踪，为此你应尽量避免使用类型映射。

第二种方法是使用类型转换例程。表3.4中总结了各种类型转换例程。其中有些例程所涉及的数据类型将在下一节中讨论。 注意表中没有包括特殊类型（如TDateTime 和variant）的转换例程，也没包括用于格式化处理的特殊例程，如Format 和FormatFloat 例程。

<table border="1" cellspacing="0" cellpadding="3" bordercolor="Black">
<tbody><tr align="LEFT"><th>例程</th><th>作用
</th></tr><tr><td>Chr</td><td>将一个有序数据转换为一个ANSI字符 
</td></tr><tr><td>Ord</td><td>将一个有序类型值转换为它的序号
</td></tr><tr><td>Round</td><td>转换一个实型值为四舍五入后的整型值
</td></tr><tr><td>Trunc</td><td>转换一个实型值为小数截断后的整型值
</td></tr><tr><td>Int</td><td>返回浮点数的整数部分
</td></tr><tr><td>IntToStr</td><td>将数值转换为字符串
</td></tr><tr><td>IntToHex</td><td>将数值转换为十六进制数字符串
</td></tr><tr><td>StrToInt</td><td>将字符串转换为一个整型数，如字符串不是一个合法的整型将引发异常
</td></tr><tr><td>StrToIntDef</td><td>将字符串转换为一个整数，如字符串不合法返回一个缺省值
</td></tr><tr><td>Val</td><td>将字符串转换为一个数字（传统Turbo Pascal例程用于向后兼容）
</td></tr><tr><td>Str</td><td>将数字转换为格式化字符串（传统Turbo Pascal例程用于向后兼容）
</td></tr><tr><td>StrPas</td><td>将零终止字符串转换为Pascal类型字符串，在32位Delphi中这种类型转换是自动进行的
</td></tr><tr><td>StrPCopy</td><td>拷贝一个Pascal类型字符串到一个零终止字符串, 在32位Delphi中这种类型转换是自动进行的
</td></tr><tr><td>StrPLCopy</td><td>拷贝Pascal类型字符串的一部分到一个零终止字符串
</td></tr><tr><td>FloatToDecimal</td><td>将一个浮点数转换为包含指数、数字及符号的十进制浮点记录类型
</td></tr><tr><td>FloatToStr</td><td>将浮点值转换为缺省格式的字符串
</td></tr><tr><td>FloatToStrF</td><td>将浮点值转换为特定格式的字符串
</td></tr><tr><td>FloatToText</td><td>使用特定格式，将一个浮点值拷贝到一个字符串缓冲区
</td></tr><tr><td>FloatToTextFmt</td><td>同上面例程，使用特定格式，将一个浮点值拷贝到一个字符串缓冲区
</td></tr><tr><td>StrToFloat</td><td>将一个Pascal字符串转换为浮点数
</td></tr><tr><td>TextToFloat</td><td>将一个零终止字符串转换为浮点数
</td></tr></tbody></table>


> 注意：在最近版本的Delphi Pascal 编译器中，Round 函数是以 CPU 的 FPU (浮点部件) 处理器为基础的。这种处理器采用了所谓的 "银行家舍入法"，即对中间值 (如 5.5、6.5) 实施Round函数时，处理器根据小数点前数字的奇、偶性来确定舍入与否，如 5.5 Round 结果为 6，而 6.5 Round 结果也为6, 因为 6 是偶数。

















