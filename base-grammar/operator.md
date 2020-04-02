#### 1. 运算符列表

<table border="1" cellpadding="2">
	<tbody>
		<tr>
			<th>分类</th>
			<th>运算符</th>
			<th>操作</th>
			<th>操作数</th>
			<th>结果类型</th>
			<th>范例</th>
		</tr>
		<tr>
			<td rowspan="8">算术运算符（加法、减法和乘法运算符的结果为参加运算的两个数据中的精度高的类型）</td>
			<td>+</td>
			<td>加</td>
			<td>整数，实数</td>
			<td>整数，实数</td>
			<td>X + Y</td>
		</tr>
		<tr>
			<td>-</td>
			<td>减</td>
			<td>整数，实数</td>
			<td>整数，实数</td>
			<td>Result - 1</td>
		</tr>
		<tr>
			<td>*</td>
			<td>乘</td>
			<td>整数，实数</td>
			<td>整数，实数</td>
			<td>P * InterestRate</td>
		</tr>
		<tr>
			<td><span style="color: #ff0000;">/</span></td>
			<td><span style="color: #ff0000;">实数除</span></td>
			<td><span style="color: #ff0000;">整数，实数</span></td>
			<td><span style="color: #ff0000;">实数</span></td>
			<td><span style="color: #ff0000;">X /
					2，不同于C中，C中5/2的结果是整数2，但是在Delphi中5/2的结果是2.5。Delphi中/运算符的结果总是实型数据</span></td>
		</tr>
		<tr>
			<td><span style="color: #ff0000;">div</span></td>
			<td><span style="color: #ff0000;">整数除</span></td>
			<td><span style="color: #ff0000;">整数</span></td>
			<td><span style="color: #ff0000;">整数</span></td>
			<td><span style="color: #ff0000;">只能对两个整数进行除法运算，结果为整型数据。例如5
					div 3 的值为1，而5 div 2.0是不合法的</span></td>
		</tr>
		<tr>
			<td>mod</td>
			<td>取模</td>
			<td>整数</td>
			<td>整数</td>
			<td>
				<p>
					Y mod 6，<span style="color: #ff0000;">两个操作数也都必须是整数，例如5 mod 3
						的值为2</span>
				</p>
			</td>
		</tr>
		<tr>
			<td>+(一元)</td>
			<td>符号等同</td>
			<td>整数，实数</td>
			<td>整数，实数</td>
			<td>+7</td>
		</tr>
		<tr>
			<td>-(一元)</td>
			<td>符号相反</td>
			<td>整数，实数</td>
			<td>整数，实数</td>
			<td>-X</td>
		</tr>
		<tr>
			<td rowspan="4">布尔运算符</td>
			<td>not</td>
			<td>否定</td>
			<td>布尔型</td>
			<td>Boolean</td>
			<td>not (C in MySet)</td>
		</tr>
		<tr>
			<td>and</td>
			<td>与</td>
			<td>布尔型</td>
			<td>Boolean</td>
			<td>Done and (Total &gt; 0)</td>
		</tr>
		<tr>
			<td>or</td>
			<td>或</td>
			<td>布尔型</td>
			<td>Boolean</td>
			<td>A or B</td>
		</tr>
		<tr>
			<td>xor</td>
			<td>异或</td>
			<td>布尔型</td>
			<td>Boolean</td>
			<td>A xor B</td>
		</tr>
		<tr>
			<td rowspan="6">
				<p>逻辑(按位)运算符</p>
				<p>位运算符的操作数必须是整数</p>
				<p>
					<span style="color: #ff0000;">按位运算符通常用来把整数的某个位清0；按位异或运算符通常可以用来把整数的某些位取反，可以用来进行加密和解密，等等；</span>
				</p>
				<p>按位运算在计算机中比加减乘数的运算快很多，因为它比较底层</p>
				<p>比如，写的程序的源码是很多ASCII的字符，可以通过位运算符将这些用01表示的ASCII码进行加密……就可以用来制造病毒</p>
			</td>
			<td>not</td>
			<td>按位否定</td>
			<td>整数</td>
			<td>整数</td>
			<td>not X，如果a的十进制为5，则其二进制为00000101，not a的值为11111010<span
				style="color: #ff0000;">（补码形式）</span>，即十进制的-6
			</td>
		</tr>
		<tr>
			<td>and</td>
			<td>按位与</td>
			<td>整数</td>
			<td>整数</td>
			<td>X and Y</td>
		</tr>
		<tr>
			<td>or</td>
			<td>按位或</td>
			<td>整数</td>
			<td>整数</td>
			<td>X or Y</td>
		</tr>
		<tr>
			<td>xor</td>
			<td>按位异或</td>
			<td>整数</td>
			<td>整数</td>
			<td>X xor Y，二进制两个相同异或为0，不同时候异或为1</td>
		</tr>
		<tr>
			<td>shl</td>
			<td>按位左移</td>
			<td>整数</td>
			<td>整数</td>
			<td>X shl 2，对操作数的二进制数按位左移，<span style="color: #ff0000;">1010左移一位结果是0100，最后的用0来填充</span></td>
		</tr>
		<tr>
			<td>shr</td>
			<td>按位右移</td>
			<td>整数</td>
			<td>整数</td>
			<td>Y shr I，对操作数的二进制按位右移，1011右移一位结果是0101，最前的用0来填充</td>
		</tr>
		<tr>
			<td>字符串运算符</td>
			<td>+</td>
			<td>连接</td>
			<td>字符串、压缩串、字符</td>
			<td>字符串</td>
			<td>'hello'+ 'world'的结果是'helloworld'</td>
		</tr>
		<tr>
			<td rowspan="5">指针运算符</td>
			<td>+</td>
			<td>指针加</td>
			<td>字符指针，整数</td>
			<td>字符指针</td>
			<td>P + I</td>
		</tr>
		<tr>
			<td>-</td>
			<td>指针减</td>
			<td>字符指针，整数</td>
			<td>字符指针，整数</td>
			<td>P - Q</td>
		</tr>
		<tr>
			<td>^</td>
			<td>指针解除参照</td>
			<td>指针</td>
			<td>指针的基类型</td>
			<td>P^</td>
		</tr>
		<tr>
			<td>=</td>
			<td>相等</td>
			<td>指针</td>
			<td>Boolean</td>
			<td>P = Q</td>
		</tr>
		<tr>
			<td>&lt;&gt;</td>
			<td>不等</td>
			<td>指针</td>
			<td>Boolean</td>
			<td>P &lt;&gt; Q</td>
		</tr>
		<tr>
			<td rowspan="8">集合运算符</td>
			<td>+</td>
			<td>并集</td>
			<td>集合</td>
			<td>集合</td>
			<td>Set1 + Set2</td>
		</tr>
		<tr>
			<td>-</td>
			<td>差集</td>
			<td>集合</td>
			<td>集合</td>
			<td>S – T</td>
		</tr>
		<tr>
			<td>*</td>
			<td>交集</td>
			<td>集合</td>
			<td>集合</td>
			<td>S * T</td>
		</tr>
		<tr>
			<td>&lt;=</td>
			<td>子集</td>
			<td>集合</td>
			<td>Boolean</td>
			<td>Q &lt;= MySet</td>
		</tr>
		<tr>
			<td>&gt;=</td>
			<td>超集</td>
			<td>集合</td>
			<td>Boolean</td>
			<td>S1 &gt;= S2</td>
		</tr>
		<tr>
			<td>=</td>
			<td>相等</td>
			<td>集合</td>
			<td>Boolean</td>
			<td>S2 = MySet</td>
		</tr>
		<tr>
			<td>&lt;&gt;</td>
			<td>不等</td>
			<td>集合</td>
			<td>Boolean</td>
			<td>MySet &lt;&gt; S1</td>
		</tr>
		<tr>
			<td>in</td>
			<td>成员</td>
			<td>序数，集合</td>
			<td>Boolean</td>
			<td>A in Set1</td>
		</tr>
		<tr>
			<td rowspan="6">关系运算符</td>
			<td>=</td>
			<td>相等</td>
			<td>简单类型、类、类引用、接口、串、压缩串</td>
			<td>Boolean</td>
			<td>I = Max</td>
		</tr>
		<tr>
			<td>&lt;&gt;</td>
			<td>不等</td>
			<td>简单类型、类、类引用、接口、串、压缩串</td>
			<td>Boolean</td>
			<td>X &lt;&gt; Y</td>
		</tr>
		<tr>
			<td>&lt;</td>
			<td>小于</td>
			<td>简单类型、串、压缩串、PChar</td>
			<td>Boolean</td>
			<td>X &lt; Y</td>
		</tr>
		<tr>
			<td>&gt;</td>
			<td>大于</td>
			<td>简单类型、串、压缩串、PChar</td>
			<td>Boolean</td>
			<td>Len &gt; 0</td>
		</tr>
		<tr>
			<td>&lt;=</td>
			<td>小于或等于</td>
			<td>简单类型、串、压缩串、PChar</td>
			<td>Boolean</td>
			<td>Cnt &lt;= 1</td>
		</tr>
		<tr>
			<td>&gt;=</td>
			<td>大于或等于</td>
			<td>简单类型、串、压缩串、PChar</td>
			<td>Boolean</td>
			<td>I &gt;= 1</td>
		</tr>
		<tr>
			<td rowspan="4">类运算符</td>
			<td>as</td>
			<td>转换</td>
			<td rowspan="2">类和类的实例</td>
			<td>&nbsp;</td>
			<td>&nbsp;</td>
		</tr>
		<tr>
			<td>is</td>
			<td>判断</td>
			<td>&nbsp;</td>
			<td>&nbsp;</td>
		</tr>
		<tr>
			<td>=</td>
			<td>&nbsp;</td>
			<td rowspan="2">关系运算符 = 和 &lt;&gt; 也作用于类</td>
			<td>&nbsp;</td>
			<td>&nbsp;</td>
		</tr>
		<tr>
			<td>&lt;&gt;</td>
			<td>&nbsp;</td>
			<td>&nbsp;</td>
			<td>&nbsp;</td>
		</tr>
		<tr>
			<td rowspan="3">地址(@)运算符</td>
			<td>@X</td>
			<td colspan="4">如果X是一个变量，那么@X返回X的地址。 当编译指示 {$T-}
				有效时，@X是Pointer类型;&nbsp; 而在编译指示 {$T+} 状态下时，@X是 ^T 类型，这里的T是X的类型。</td>
		</tr>
		<tr>
			<td>@F</td>
			<td colspan="4">如果 F 是一个例程(函数或过程)，那么@F返回 F 的入口点，@F的类型总是Pointer。</td>
		</tr>
		<tr>
			<td>@类中方法</td>
			<td colspan="4">当 @ 适用于定义在类中的方法时，方法标识符必需被类的名称限定。例如:
				@TMyClass.DoSomething<br>
			<br>
			</td>
		</tr>


</table>