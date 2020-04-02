# TButton 按钮

> 日常软件操作的主要控件，必须熟练使用的控件，特别是控件对应的事件

1. ##### 添加Button控件到窗口

![mark](http://imgs.coder163.com/blog/20200402/RbxqFdq103r2.png?imageslim)

2. ##### 调整按钮大小

![mark](http://imgs.coder163.com/blog/20200402/p2cBPq4oRuk8.png?imageslim)

3. ##### Caption 修改按钮名称

![mark](http://imgs.coder163.com/blog/20200402/UaWnR0PBMEbA.png?imageslim)

4. ##### Align 对齐：

   1. Align=alButtom

      ![mark](http://imgs.coder163.com/blog/20200402/CfE7R83yduEM.png?imageslim)

   2. Align=alClient

      ![mark](http://imgs.coder163.com/blog/20200402/DmWvcgptcxL3.png?imageslim)

   3. Align=alCustom

      ![mark](http://imgs.coder163.com/blog/20200402/7GGS1EF3X2r0.png?imageslim)

   4. Align=alLeft

      ![mark](http://imgs.coder163.com/blog/20200402/2gYFCuiDp2l9.png?imageslim)

   5. Align=alNone(默认)

      ![mark](http://imgs.coder163.com/blog/20200402/aOz8muqDJzxX.png?imageslim)

   6. Align=alRight

      ![mark](http://imgs.coder163.com/blog/20200402/U1eIKmPyM49L.png?imageslim)

   7. Align=alTop

      ![mark](http://imgs.coder163.com/blog/20200402/d6uE4AdsrQoo.png?imageslim)

5. ##### AutoSize 自动大小

   ![mark](http://imgs.coder163.com/blog/20200402/R9uNGyvFJhUQ.png?imageslim)

   ​	`选择AutoSize后，按钮会根据俺就中文字的长度和高度来自适应按钮的长度和高度`

6. ##### Cursor 光标形状

   ​	此处截图比较麻烦，设置为 crHandPoint ，鼠标移动到按钮上就会显示为手的形状

7. ##### Enable 可用/禁用

   ![mark](http://imgs.coder163.com/blog/20200402/DSwKRqL78055.png?imageslim)

   ​	显示效果：![mark](http://imgs.coder163.com/blog/20200402/BIQLgoHw0iI5.png?imageslim)

8. ##### Font 字体

   1. 设置方式：![mark](http://imgs.coder163.com/blog/20200402/BDrigA0J3FWq.png?imageslim)
   2. 设置效果：![mark](http://imgs.coder163.com/blog/20200402/SStGAo1UNL4l.png?imageslim)

9. ##### Hint 提示信息 ShowHint

   1. 设置提示信息：![mark](http://imgs.coder163.com/blog/20200402/1FBtdvzGPPKN.png?imageslim)
   2. 设置效果：![mark](http://imgs.coder163.com/blog/20200402/1uTPJb2mcGkP.png?imageslim)

10. ##### Left & Top 左 和 上

    左和上用来确定按钮位置

11. ##### Height & Width 高度 和 宽度

    高度和宽度确定按钮大小

12. ##### Name 名字

    1. 控件的名字，日常命令操作的时候使用的就是这个名字

    2. 将按钮的Name 设置为 BtnMain ，然后在命令行中调用

    3. 命令：

       ```pascal
       BtnMain.Caption := '按钮名字';  
       ```

    4. 效果：

       ![mark](http://imgs.coder163.com/blog/20200402/0LBo7DdMaD4j.png?imageslim)

13. ##### Visible 可见性

    1. 设置为True ：按钮可以看到
    2. 设置为False：按钮无法看到
    3. IDE界面：![mark](http://imgs.coder163.com/blog/20200402/CkHT8RHsPIsA.png?imageslim)
    4. 运行效果：![mark](http://imgs.coder163.com/blog/20200402/SxuUw24dpzIr.png?imageslim)

