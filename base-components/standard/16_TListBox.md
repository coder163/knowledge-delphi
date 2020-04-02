# TListBox 列表

列表主要逐行显示，可以选择

1. ##### 添加控件

   ![mark](http://imgs.coder163.com/blog/20200402/PdJBhMJD6kJ0.png?imageslim)

2. ##### 设置内容

   ![mark](http://imgs.coder163.com/blog/20200402/XFN5sVjAgvMa.png?imageslim)

3. ##### 运行效果

   ![mark](http://imgs.coder163.com/blog/20200402/fwAm5loN5VeC.png?imageslim)

4. ##### 示例代码

   1. ```pascal
      procedure TForm1.Button1Click(Sender: TObject);
      begin
        ListBox1.Items.Add('添加一行');
      end;
      
      procedure TForm1.Button2Click(Sender: TObject);
      begin
        ListBox1.DeleteSelected;
      end;
      
      procedure TForm1.Button3Click(Sender: TObject);
      begin
     ListBox1.Clear;
      end;
      
      procedure TForm1.Button4Click(Sender: TObject);
      begin
        ListBox1.Items.Delete(2);     //这里的2，就是第三行，因为编号是从0开始的
      end;
      
      procedure TForm1.Button5Click(Sender: TObject);
      begin
        ListBox1.Items.LoadFromFile('s.txt');
      end;
      
      procedure TForm1.Button6Click(Sender: TObject);
      begin
        ListBox1.Items.SaveToFile('m.txt');
      end;
      
      procedure TForm1.Button7Click(Sender: TObject);
      begin
        Form1.Caption := ListBox1.Items[ListBox1.ItemIndex];
      end;
          
      ```
      
      

5. ##### 补充