#### 1. 数据类型列表

<table border="1" cellpadding="2" style="border-collapse: collapse;">
	<tbody>
		<tr>
			<td colspan="4" align="center">分类</td>
			<td align="center">范围</td>
			<td align="center">字节</td>
			<td align="center">备注</td>
		</tr>
		<tr>
			<td rowspan="24" align="center">简单类型</td>
			<td rowspan="17" align="center">序数</td>
			<td rowspan="9" align="center">整数</td>
			<td>Integer</td>
			<td>-2147483648 .. 2147483647</td>
			<td>4</td>
			<td>有符号32位</td>
		</tr>
		<tr>
			<td>Cardinal</td>
			<td>0 .. 4294967295</td>
			<td>4</td>
			<td>无符号32位</td>
		</tr>
		<tr>
			<td>Shortint</td>
			<td>-128 .. 127</td>
			<td>1</td>
			<td>有符号8位</td>
		</tr>
		<tr>
			<td>Smallint</td>
			<td>-32768 .. 32767</td>
			<td>2</td>
			<td>有符号16位</td>
		</tr>
		<tr>
			<td>Longint</td>
			<td>-2147483648 .. 2147483647</td>
			<td>4</td>
			<td>有符号32位</td>
		</tr>
		<tr>
			<td>Int64</td>
			<td>-2<sup>63 </sup>.. 2<sup>63</sup></td>
			<td>8</td>
			<td>有符号64位</td>
		</tr>
		<tr>
			<td>Byte</td>
			<td>0 .. 255</td>
			<td>1</td>
			<td>无符号8位</td>
		</tr>
		<tr>
			<td>Word</td>
			<td>0 .. 65535</td>
			<td>2</td>
			<td>无符号16位</td>
		</tr>
		<tr>
			<td>Longword</td>
			<td>0 .. 4294967295</td>
			<td>4</td>
			<td>无符号32位</td>
		</tr>
		<tr>
			<td rowspan="2" align="center">字符</td>
			<td>AnsiChar(Char)</td>
			<td>ANSI字符集</td>
			<td></td>
			<td>8位</td>
		</tr>
		<tr>
			<td>WideChar</td>
			<td>Unicode字符集</td>
			<td></td>
			<td>16位</td>
		</tr>
		<tr>
			<td rowspan="4" align="center">布尔</td>
			<td>Boolean</td>
			<td>False &lt; True<br> Ord(False) = 0<br> Ord(True) =
				1<br> Succ(False) = True<br> Pred(True) = False
			</td>
			<td>1</td>
			<td></td>
		</tr>
		<tr>
			<td>ByteBool</td>
			<td rowspan="3">False &lt;&gt; True<br> Ord(False) = 0<br>
				Ord(True) &lt;&gt; 0<br> Succ(False) = True<br>
				Pred(False) = True
			</td>
			<td>1</td>
			<td></td>
		</tr>
		<tr>
			<td>WordBool</td>
			<td>2</td>
			<td></td>
		</tr>
		<tr>
			<td>LongBool</td>
			<td>4</td>
			<td></td>
		</tr>
		<tr>
			<td align="center">枚举</td>
			<td></td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">子界</td>
			<td></td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td rowspan="7" align="center">实数</td>
			<td rowspan="7" align="center"></td>
			<td>Real</td>
			<td>5.0×10<sup>-324</sup> .. 1.7×10<sup>308</sup></td>
			<td>8</td>
			<td>[精度]15..16</td>
		</tr>
		<tr>
			<td>Real48</td>
			<td>2.9×10<sup>-39</sup> .. 1.7×10<sup>38</sup></td>
			<td>6</td>
			<td>[精度]11..12;<br> 向后兼容
			</td>
		</tr>
		<tr>
			<td>Single</td>
			<td>1.5×10<sup>-45</sup> .. 3.4×10<sup>38</sup></td>
			<td>4</td>
			<td>[精度]7..8</td>
		</tr>
		<tr>
			<td>Double</td>
			<td>5.0×10<sup>-324</sup> .. 1.7×10<sup>308</sup></td>
			<td>8</td>
			<td>[精度]15..16</td>
		</tr>
		<tr>
			<td>Extended</td>
			<td>3.6×10<sup>-4951</sup> .. 1.1×10<sup>4932</sup></td>
			<td>10</td>
			<td>[精度]19..20</td>
		</tr>
		<tr>
			<td>Comp</td>
			<td>-2<sup>63</sup> + 1 .. 2<sup>63</sup> - 1
			</td>
			<td>8</td>
			<td>[精度]19..20</td>
		</tr>
		<tr>
			<td>Currency</td>
			<td>-922337203685477.5808 ..&nbsp;<br> 922337203685477.5807
			</td>
			<td>8</td>
			<td>[精度]19..20</td>
		</tr>
		<tr>
			<td rowspan="4" align="center">字符串</td>
			<td rowspan="4" align="center"></td>
			<td rowspan="4" align="center"></td>
			<td>ShortString</td>
			<td>255个字符</td>
			<td>2..256B</td>
			<td>向后兼容</td>
		</tr>
		<tr>
			<td>AnsiString</td>
			<td>大约 2<sup>31</sup> 个字符
			</td>
			<td>4B..2GB</td>
			<td>8位(ANSI)字符</td>
		</tr>
		<tr>
			<td>WideString</td>
			<td>大约 2<sup>30</sup> 个字符
			</td>
			<td>4B..2GB</td>
			<td>多用户服务和<br> 多语言应用程序;&nbsp;<br> 和com定义的BSTR兼容
			</td>
		</tr>
		<tr>
			<td>其他</td>
			<td>String<br> String[0..255]<br> PChar<br>
				PAnsiString<br> PWideString
			</td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td rowspan="8" align="center">结构类型</td>
			<td align="center">集合</td>
			<td align="center"></td>
			<td>Set</td>
			<td>最多256个元素[0..255]</td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td rowspan="2" align="center">数组</td>
			<td align="center">静态数组</td>
			<td></td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">动态数组</td>
			<td></td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">记录</td>
			<td align="center"></td>
			<td>Record</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">文件</td>
			<td align="center"></td>
			<td>File</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">类</td>
			<td align="center"></td>
			<td>Class</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">类引用</td>
			<td align="center"></td>
			<td>Class reference</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">接口</td>
			<td align="center"></td>
			<td>Interface</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td rowspan="2" align="center">指针类型</td>
			<td align="center">无类型指针</td>
			<td align="center"></td>
			<td>Pointer</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">有类型指针</td>
			<td align="center">预定义类型指针</td>
			<td>PAnsiString<br> PString<br> PByteArray<br>
				PCurrency<br> PDouble<br> PExtended<br> PSingle<br>
				PInteger<br> POleVariant<br> PShortString<br>
				PTextBuf<br> PVarRec<br> PVariant<br> PWideString<br>
				PWordArray
			</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td rowspan="2" align="center">过程类型</td>
			<td align="center">程序过程类型</td>
			<td align="center"></td>
			<td>Procedural</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center">对象过程类型</td>
			<td align="center"></td>
			<td>Procedural</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td rowspan="2" align="center">变体类型</td>
			<td rowspan="2" align="center"></td>
			<td align="center"></td>
			<td>Variant</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
		<tr>
			<td align="center"></td>
			<td>OleVariant</td>
			<td></td>
			<td></td>
			<td></td>
		</tr>
	</tbody>
</table>
