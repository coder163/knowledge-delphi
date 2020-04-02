# TComboBox 组合框（也是就下拉框）

下拉框日常中用的比较多，主要记住调用下拉框选项的方法

1. ##### 添加控件

   ![mark](http://imgs.coder163.com/blog/20200402/d2kXkOQe2CKn.png?imageslim)

2. ##### 控件属性

   ![mark](http://imgs.coder163.com/blog/20200402/zCIBH8eYuOlY.png?imageslim)

   ![mark](http://imgs.coder163.com/blog/20200402/sYbmj2qcKii9.png?imageslim)

3. ##### 运行效果（自动补全；自动下拉）

   ![mark](http://imgs.coder163.com/blog/20200402/yk4pUIKBCkMn.png?imageslim)

   ![mark](http://imgs.coder163.com/blog/20200402/N108UHQyqtBq.png?imageslim)

4. ##### 示例代码

   1. ```pascal
      procedure TForm1.Button1Click(Sender: TObject);
      begin
        Form1.Caption:= ComboBox1.Items[ComboBox1.ItemIndex];//这里的用法和ListBox是一样的
      end; 
      ```

      

5. ##### 补充