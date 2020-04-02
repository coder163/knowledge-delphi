# TActionList 动作列表

> 此控件为开发过程中使用频率比较高的控件，常用的一些操作可以写在 ActionList里面，那里需要执行对应的代码只需要在控件的Action里面选择ActionList中的Action即可。
>
> 此控件和第一个以及第二个控件一样，属于不可视的控件。

1. ##### 添加控件

   1. ![mark](http://imgs.coder163.com/blog/20200402/v9XkveoMrOSo.png?imageslim)
   

2. ##### 设置属性

   1. ![mark](http://imgs.coder163.com/blog/20200402/QvfbnkIzmSBs.png?imageslim)
   2. ![mark](http://imgs.coder163.com/blog/20200402/8CM9IQ4Bka5h.png?imageslim)
   3. ![mark](http://imgs.coder163.com/blog/20200402/xas2K6bIwymr.png?imageslim)

3. ##### 运行效果

   1. ![mark](http://imgs.coder163.com/blog/20200402/dLh3AYHiFuSu.png?imageslim)
   2. ![mark](http://imgs.coder163.com/blog/20200402/RceBUqR3zRas.png?imageslim)

4. ##### 示例代码

   1. ```pascal
      procedure TForm1.Act01Execute(Sender: TObject);
      begin
        ShowMessage('Act01');
      end;
      
      procedure TForm1.Act02Execute(Sender: TObject);
      begin
        ShowMessage('Act02');
      end; 
      ```

      

5. ##### 补充