#### 1. 什么是包？

Delphi中的包是Delphi程序间通讯与模块化的一个方法，其中包可分为设计包（比如控件都有一个设计包）和运行包，包的本质和Win32下的DLL并无本质区别，可以理解为Borland专有的一个DLL，只不过这个“DLL”只能用Delphi开发的程序中。

这有点类似于VC下的可以导出整个类的DLL--这种DLL只能被使用MFC编程的程序使用。Delphi运行包的好处与VC中的专有DLL有相同之处：使用十分方便，开发的程序可以很容易模块化，便于合作开发。

设计包

其实每个VCL控件一般都有一个设计包，可以在 Project| Option...对话框中的Package页中安装、删除。设计包此处从略

运行包

可以显式链接也隐式链接（这一点与DLL几乎完全一模一样！）

#### 2. 包的开发方法

开发方法十分简单：New|...|package即可，可以在包中添加窗体和各种各样的控件以及相应的代码、数据，与平时开发应用程序无任何不同。 唯一不同的一点是生成  .BPL 文件，生成的BPL文件默认位置是在Delphi的安装目录下的Project|BPL中，不过可以另外设定Output目录

另外还有一点不同是：在使用包之前必须要知道包中有哪些类，所有先注册。所以要在每个包中的  initialization部分注册类而在 finalization部分注销类 
如下使用：

```pascal
unit1.pas

initialization
  RegisterClass(TForm2);
finalization
  UnRegisterClass(TForm2);
```


#### 3. 使用方法

##### 3.1. 隐式链接

包含相应的Unit单元，同时在 Project -->Option... 中的 Packages页选中 Build with RunTime Package复选框，并在相应的Edit框中添加要链接的包的文件名，如果是放在和Exe文件同一目录中的话，可以不加路径名

##### 3.2. 显式链接

下面例程演示了如何使用：

```pascal

procedure TForm1.Button1Click(Sender: TObject);
var
  PackageModule: HModule;
  AClass: TPersistentClass;
begin
  PackageModule := LoadPackage('Package1.bpl');//装载包
  if PackageModule &lt;&gt; 0 then
  begin
    AClass := GetClass('TForm2'); //得到包中的相应的类

    if AClass &lt;&gt; nil then
      with TComponentClass(AClass).Create(Application)
        as TCustomForm do
      begin
        ShowModal;
        Free;
      end;

    UnloadPackage(PackageModule);
  end;
end;

```







