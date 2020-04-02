# TScrollBar 滚动条

> 老式滚动条，用处还是不少的。

1. 添加控件

  ![mark](http://imgs.coder163.com/blog/20200402/BrY8nIUhiwSv.png?imageslim)

2. 控件属性

   ![mark](http://imgs.coder163.com/blog/20200402/rmBhG9AQvXFi.png?imageslim)

 
   ![mark](http://imgs.coder163.com/blog/20200402/NMLGreb68s6s.png?imageslim)

3. 运行效果

   ![mark](http://imgs.coder163.com/blog/20200402/O9mxoeW68q44.png?imageslim)

4. 示例代码

   1. ```pascal
      procedure TForm1.ScrollBar1Change(Sender: TObject);
      begin
        Edit1.Text:= ScrollBar1.Position.ToString;
      end; 
      ```

      

5. 补充