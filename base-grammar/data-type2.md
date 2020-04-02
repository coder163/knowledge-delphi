#### 1. 枚举类型

Pascal程序不仅用于数值处理，还更广泛地用于处理非数值的数据。例如，性别、月份、星期几、颜色、单位名、学历、职业等。

##### 1.1. 枚举类型的定义

格式:　type 枚举类型标识符=(标识符1,标识符2,…,标识符n)

##### 1.2. 枚举类型特点

枚举元素只能是标识符； 定义枚举类型时列出的所有枚举元素构成了这种枚举类型的值域（取值范围）。例如，下列类型定义是合法的：

```pascal
 type
  days=(sun,mon,tue,wed,thu,fri,sat);
  colors=(red,yellow,blue,white,black,green);
```

而下列类型定义是错误的:

```Pascal
  type
  colortype=('red','yellow','blue','white');
  numbers=(1,3,5,7,9);
```

枚举类型属于顺序类型

根据定义类型时各枚举元素的排列顺序确定它们的序号，且序号从0开始。例如，定义type days=(sun,mon,tue,wed,thu,fri,sat); 则， ord(sun)=0,ord(mon)=1,……，以此类推。枚举类型中的第一个元素无前趋，最后一个元素无后继。pred(sat)=fri; succ(sun)=mon; ord(sat)=6;

 同一个枚举元素不能出现在两个或两个以上的枚举类型定义中。如下列定义是错误的:

```Pascal
  type color1=(red,yellow,white);
  color2=(blue,red,black);
  // 因为red属于枚举类型color1和 color2

```

枚举类型变量只能进行赋值运算和关系运算，不能进行算术运算和逻辑运算.　在枚举元素比较时，实际上是对其序号的比较。

例如定义如下：

```Pascal
type
	days=(sun,mon,tue,wed,thu,fri,sat);
　　colors=(red,yellow,blue,white,black,green);
var
	color:colors;
　　 weekday:days;
　//则下面语句是合法的:
　weekday:=mon;
　if weekday=sun then write('rest');
　//而下面语句是不合法的:
	mon:=1;           //错把枚举值当成变量名；
	weekday:=blue；   //枚举值blue不属于枚举变量weekday的值域；
	read(color);      //枚举类型变量 不能用读语句进行赋值；
	write(weekday); writeln(blue);//不能通过写语句输出枚举类型的变量值和枚举值。
```

可以把变量的说明与类型的定义合并在一起，如:



```Pascal
  　　var
  　　	holiday,workday:(sun,mon,tue,wed,thu,fri,sat);
  　　　   color:(red,yellow,blue,white,black,green);


```

对枚举数据的输入与输出可通过间接方式进行。输入时，一般可输入一个代码，通过程序进行转换，输出时，也只是打印出与枚举元素相对应的字符串。这在后面的例题中将有使用示例。

##### 1.3. 应用实例

例1、输入今天是星期几的序号，输出明天是星期几的英文单词（星期天序号为0）。


```Pascal
　　type weekday=(sun,mon,tue,wed,thu,fri,sat);
　　 var i : integer;
　　　　 today,tomorrow : weekday;
　　begin
　　　writeln('What date is it'); readln(i);
　　　case i of                              { 根据输入转换成枚举型 }
　　　　0:today:=sun;
　　　　1:today:=mon;
　　　　2:today:=tue;
　　　　3:today:=wed;
　　　　4:today:=thu;
　　　　5:today:=fri;
　　　　6:today:=sat;
　　　end;
　　　if (today=sat) then tomorrow:=sun  else tomorrow:=succ(today);
　　  write('The tomorrow is ');
　　　case tomorrow of
　　　　sun:writeln('sunday');
　　　　mon:writeln('monday');
　　　　tue:writeln('tuesday');
　　　　wed:writeln('wednesay');
　　　　thu:writeln('thursday');
　　　　fri:writeln('friday');
　　　　sat:writeln('saturday');
　　　end;
　　end.

```

> PS: 枚举类型是一种有序类型，所以枚举类型的变量可以作为循环变量。

#### 2. 子界类型

如果我们定义一个变量为integer型，那么它的取值范围一般为-32768～32767。而事实上，每个程序中所用的变量的值都有一个确定的范围。 例如，人的年龄一般为1到120岁，一年中的月数为1到12月，一月中的天数为1到31天等等。

如果能在程序中对所用的变量的值域作具体规定，就便于检查出那些不合法的数据，这就能更好地保证程序运行的正确性且在一定程度上节省内存空间。

子界类型能很好解决上面的问题。此外，在数组的定义中，常用到子界类型，以规定数组下标的范围。



##### 2.1. 定义格式
 type 子界类型标识符=常量1..常量2

  	 常量1称为子界的下界，常量2称为子界的上界;


注意事项：
​
下界和上界必须是同一顺序类型（该类型称为子界类型的基类型），且上界的序号必须大于下界的序号。 例如

```pascal
type age=1..100;
 　　letter='a' ..'z';
```

可以直接在变量说明中定义子界类型。如：

```pascal
type  letter='a'..' z ';
    var ch1,ch2:letter;
```

可以合并成:

```pascal
var ch1,ch2:'a'..'d';
```

##### 2.2. 运算规则

 凡可使用基类型的运算规则同样适用该类型的子界类型。

例如，可以使用整型变量的地方，也可以使用以整型为基类型的子界类型数据。

对基类型的运算规则同样适用于该类型的子界类型


例如，div，mod要求参加运算的数据为整, 因而也可以为整型的任何子界类型数据。

基类型相同的不同子界类型数据可以进行混合运算。例如：设有如下说明：

```pascal
var  x:1..100;
　　 y:1..500;
　　 z:1..1000;
　   a:integer;
```

则下列语句是合法的：` a:=Sqr(x)+y+z;   z:=x+y`

下列语句: y:=x+z+a; 当x+y+a的值在1～500范围内时是合法的，否则会出错。

##### 2.3. 应用举例


例1、使用子界型情况语句，当输入月、日、年(10 30 1986)，输出30 Oct 1986。

```Pascal
  var
    month: 1..12;
    day: 1..31;
    year: 1900..2003;

  begin
    write('Enter date(mm dd yy):');
    readln(month, day, year);
    write(day);
    case month of
      1:
        write('Jan');
      2:
        Write('Feb');
      3:
        write('Mar');
      4:
        write('Apr');
      5:
        write('May');
      6:
        write('Jun');
      7:
        write('Jul');
      8:
        write('Aug');
      9:
        write('Sep');
      10:
        write('Oct');
      11:
        write('Nov');
      12:
        write('Dec');
    end;
```


#### 3. 集合类型


集合用一种有效的手段来表示一组有序数、字符和枚举值。声明一个集合用关键字set of，并在其后跟上有序类型或一个集合可能值的有限子集。在pascal中，一个集合是由具有同一有序类型的一组数据元素所组成，这一有序类型称为该集合的基类型。

##### 3.1. 集合类型的定义和变量的说明

集合类型的一般形式为： set of 基类型;

基类型可以是任意顺序类型, 而不能是实型或其它构造类型。同时，基类型的数据的序号不得超过255。例如下列说明是合法的:

```pascal
  type
    numbers = set of 0..9;
    ch = set of char;
    day = (sun, mon, tue, wed, thu, fri, sat);
  var
    s: numbers;
    c: ch;
    weekday: day;

```

可以将类型说明与变量说明合并在一起，如:

```pascal
  var s:set of 0..9;                               { 子界型 }
      c:set of char;
      weekday: (sun,mon,tue,wed,thu,fri,sat);      { 枚举型 }

```

> ps  注意：集合的元素个数不超过256个，因此 var s:set of integer; 是错误的。

##### 3.2. 集合的值

集合的值放在一对方括号中，中间各元素之间用逗号隔开。如：[1,2,5] 和 ['a','e','i'] 都是集合。

在集合中可以没有任何元素，这样的集合称为空集。[] 空集

在集合中，如果元素的值是连续的，则可用子界型的表示方法表示。例如：`［1,2,3,4,5, 10,15］　`可以表示成： `［1..5,10,15］`

集合的值与方括号内元素出现的次序无关。例如［1,5,8 ］和［5,1,8］的值相等。

在集合中同一元素的重复出现对集合的值没有影响。例如，［1,8,5,1,8］与［1,5,8］的值相等。

每个元素可用基类型所允许的表达式来表示。如 ［1,1+2,4］、［succ(ch)］

##### 3.3. 集合的运算

 集合类型变量不能进行算术运算，集合是无序的，不能使用ord、pred、succ等函数。

赋值运算:只能通过赋值语句给集合变量赋值，不能通过读语句赋值，也不能通过写语句直接输出集合变量的值。如：

  集合变量赋值:     c:=['2'];  i:=[5];  w:=[];

  集合变量赋子界值: c:=['a'..'z'];  i:=[1..7];

  集合变量赋枚举值: c:=['a','b','d','m'];  i:=[2,4,6,8,10];  

  函数赋值操作：

  添加一个集合元素 Include(s, 1);

  删除一个集合元素 Exclude(s, 1);



**集合的并、交、差运算**

可以对集合进行并(＋)、交(＊)、差 (－)三种运算，每种运算只有一个运算符、两个运算对象，运算结果仍为集合。注意它们与算术运算的区别。

并运算 （关系代数运算符∪）

A，B为两个集合，由集合A中的元素加上集合B中的与A不重复的所有元素组成的集合，称为集合A和B的并。即A+B，如：  

[X，Y，Z]+[X] 为 [X，Y，Z]       { 两个集合中不重复的所有元素 }

[1]+[4] 为[1，4]

**交运算  （关系代数运算符∩）**

A，B为两个集合，由既属于集合A中的元素又属于集合B中的所有元素组成的集合，称为集合A和B的交。即A*B，如：

[X，Y，Z]*[X] 为 [X]          { 两个集合中的相同元素 }

[X，Y，Z]*[M] 为 []

**差运算   （关系代数运算符-）**


A，B为两个集合，由集合A中的元素除去集合B中与A相同的元素组成的集合，称为集合A和B的差。即AB，如：

[X，Y，Z]-[X] 为 [Y，Z ]      { 在集合A中又不在集合B中的所有元素 }

[X，Y，Z]-[M] 为 [X，Y，Z]

**集合的关系运算**： 运算结果为布尔值

关系运算符：= 相等、  <> 不相等  >= 包含，表示前者蕴含后者，相当于集合论中的 <= 包含于，表示前者蕴含于后者，相当于集合论中的 。

例如：[a,b,c]=[b,c,a]   为true，元素个数相同，内容相同，不管排列顺序如何。

[a,b,c]>=[a]      为true；

[a,b]<=[a,b,c]    为true。

in运算：in的右边为集合，左边为与集合基类型相同的表达式，为布尔值。in测试一个元素是否在集合中。相当于集合论中的∈。它们都是二目运算，且前４个运算符的运算对象都是相容　　的集合类型。例如：a in［b,c］  为false。

设集合a:=[1..10]； x 为integer，如x在集合a中即删除a中的元素x，否则把元素x添加到集合a中。程序段如下：

```pascal
  if x in a then a:=a-[x] else a:=a+[x]
```

