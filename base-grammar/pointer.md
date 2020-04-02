#### 1. 概述

大家都认为，C语言之所以强大，以及其自由性，很大部分体现在其灵活的指针运用上。因此，说指针是C语言的灵魂，一点都不为过。同时，这种说法也让很多人产生误解，似乎只有C语言的指针才能算指针。Basic不支持指针，在此不论。其实，Pascal语言本身也是支持指针的。从最初的Pascal发展至今的Object Pascal，可以说在指针运用上，丝毫不会逊色于C语言的指针。  

#### 2. 类型指针的定义

类型指针的定义。对于指向特定类型的指针，在C中是这样定义的：

```c
int *ptr;
char *ptr;
```

与之等价的Object Pascal是如何定义的呢？

```pascal
var
ptr : ^Integer;
ptr : ^char;
```

#### 3. 无类型指针的定义

无类型指针的定义。C中有void *类型，也就是可以指向任何类型数据的指针。Object Pascal为其定义了一个专门的类型：Pointer。于是，`ptr : Pointer; `就与C中的`void *ptr;`等价了。

#### 4. 指针的解除引用

要解除指针引用（即取出指针所指区域的值），C 的语法是 (*ptr)，Object Pascal则是 ptr^

#### 5. 取地址(指针赋值)

取某对象的地址并将其赋值给指针变量，C 的语法是 `ptr = &Object; ` Object Pascal 则是 `ptr := @Object;  `

#### 6. 指针运算

在C中，可以对指针进行移动的运算，如：

```C
char a[20];
char *ptr=a;
ptr++;
ptr+=2;

```
当执行ptr++;时，编译器会产生让ptr前进sizeof(char)步长的代码，之后，ptr将指向a[1]。ptr+=2;这句使得ptr前进两 个sizeof(char)大小的步长。同样，我们来看一下Object Pascal中如何实现

```pascal
var
   a : array [1..20] of Char;
   ptr : PChar; //PChar 可以看作 ^Char
begin
   ptr := @a;
   Inc(ptr); // 这句等价于 C 的 ptr++;
   Inc(ptr, 2); //这句等价于 C 的 ptr+=2;
end;
```
#### 7. 动态内存分配

#### 8. 字符数组的运算

C语言中，是没有字符串类型的，因此，字符串都是用字符数组来实现，于是也有一套str打头的库函数以进行字符数组的运算,

```C
char str[15];
char *pstr;
strcpy(str, "teststr");
strcat(str, "_testok");
pstr = (char*) malloc(sizeof(char) * 15);
strcpy(pstr, str);
printf(pstr);
free(pstr);
```

而在Object Pascal中，有了String类型，因此可以很方便的对字符串进行各种运算。但是，有时我们的Pascal代码需要与C的代码交互（比如：用 Object Pascal的代码调用C写的DLL或者用Object Pascal写的DLL准备允许用C写客户端的代码）的话，就不能使用String类型了，而必须使用两种语言通用的字符数组。其实，Object Pascal提供了完全相似C的一整套字符数组的运算函数，以上那段代码的Object Pascal版本是这样的：

```pascal
var str : array [1..15] of char;
   pstr : PChar; //Pchar 也就是 ^Char
begin
   StrCopy(@str, 'teststr'); //在C中，数组的名称可以直接作为数组首地址指针来用
    //但Pascal不是这样的，因此 str前要加上取地址的运算符
   StrCat(@str, '_testok');
   GetMem(pstr, sizeof(char) * 15);
   StrCopy(pstr, @str);
   Write(pstr);
   FreeMem(pstr);
end;
```

#### 9. 函数指针

函数指针。在动态调用DLL中的函数时，就会用到函数指针。假设用C写的一段代码如下：

```c
typedef int (*PVFN)(int); //定义函数指针类型
int main()
{
   HMODULE hModule = LoadLibrary("test.dll");
   PVFN pvfn = NULL;
   pvfn = (PVFN) GetProcAddress(hModule, "Function1");
   pvfn(2);
   FreeLibrary(hModule);
}
```

就我个人感觉来说，C语言中定义函数指针类型的typedef代码的语法有些晦涩，而同样的代码在Object Pascal中却非常易懂：

```pascal
type PVFN = Function (para : Integer) : Integer;
var
   fn : PVFN;
     //也可以直接在此处定义，如：fn : function (para:Integer):Integer;
   hm : HMODULE;
begin
   hm := LoadLibrary('test.dll');
   fn := GetProcAddress(hm, 'Function1');
   fn(2);
   FreeLibrary(hm);
end;
```


