# PascalScriptUnit

># ◆简易类Pascal脚本             
># ◆2017/06/21  PascalScript1.0    

　　PascalScriptUnit是基于pascal的脚本解释库，旨在建立类pascal语法的运行环境。PascalScript是一种交互式简易脚本，pascal程序可以以此做到简单的外部交互效果。本代码与本条目发表前网上可以搜到的PascalScript的内容无关，Power by shyakocat 2017。pascalscript使用与pascal类似的语法，借鉴了c++、python、lua等。     

>## 变量类型
　　1.0版本中，只有int(longint)、double(real)、psstr(ansistring)三种基本类型。脚本中不能新建也不能使用主程序定义的记录体或类。脚本中可以新建一维数组(类型intarr)，数组大小以第一次索引值为准，可以用[]访问longint数组或字符串，可以在主程序关联(Assign)主程序的longint数组。变量无需申明即可使用，所有未知变量都会以int=0的形式初始化。变量赋值时，类型也会随之赋给对象。      　

>## 运算
　　1.0版本中，运算包括1元运算符not和其他pascal中常见的2元运算符(math库的**，+=等除外)。赋值语句:=也被认为是运算符，返回值为:=右边的值，可以用a:=b:=c这类方法。not在这里和c++的!一致，~在此不原生支持。普遍地，任何非关键字语句都有返回值。由于大部分情况都是不报错的，所以要自觉写正确语法。       

>## 语句
　　语句基本上分两种：表达式和关键字语句。            
　　表达式即仅包含变量、常量、函数、操作符的简单语句。              
　　1.0版本中，含关键字的语句会被隔离出来处理，被称作关键字语句。这里支持begin...end、if...then...else...、for...:=...to...do...、while...do...这几种语句。                 
　　注释//后的语句全忽略。       

>## 基本操作
>>### 构建
　　只需在主程序里定义一个PSLib类即可。在其中可以关联(Assign)脚本中的变量名与值(即对某个变量名赋值)，获取(Get)脚本中的变量名对应的值，执行(Exec)一段脚本等。
>>### 函数
　　新建的PSLib只能进行单纯的算术，想要支持更多函数需要自己定义。1.0版本中PSLib内置了UsesSystem，使用后即可关联一些pascal常用函数。
>>### 语句
　　实际上pascalscript1.0在编写中并未主要考虑token流等思想，因此其代码格式略显死板。基本上一句一句为准，所以回车即可分割语句，当然分号(;)也可以分割语句。逗号理论上也能划分，其作用是返回最右侧表达式的值，但不建议在函数外过度使用。语句中的空格、不可见字符等不相关字符会被忽略。
>>### begin...end
　　begin end作用是指定作用域，可以嵌套。原则上但凡其begin end间的代码都会惰性执行(即一开始不执行，仅仅记录下语句，begin end全配对后再一并执行)，实际实现时主程序中忽略begin end，begin end现只作为一种更直观的分隔语句的手段。
>>### if...then...else...
　　if后必须紧跟一个表达式不能换行。表达式的值非0时if判断为真，反之为假。then或else后的语句都会惰性执行，**作用域为第一个begin...end外的分号(;)前的所有代码（这句话非常重要）**。
>>### for...:=...to...do…
　　for格式必须如上不得换行，后面紧跟一个变量(不能加括号)，然后两个表达式表示始末值。后面的代码惰性执行，参见if。
>>### while...do…
　　while格式必须如上不得换行，表达式布尔判定和惰性执行参见if。

>## 进阶原理
>>### 基本类型
　　基本类型被定义为vtype，包含int、double、psstr、TFunc(函数)、intarr(int数组指针)。这么写是为了不与pascal的基本类型或关键字冲突。PascalScriptUnit还提供了泛型动态数组List，支持pushback添加值、Items[i]访问成员等操作(更多可见源码)。PSLib内部的数组基本都由List实现。pascalscript1.0版本的索引相关操作都是Hash表实现。查询(Find)后返回在List中的位置，返回-1表示未找到。Clear过程是清空所有记录。总体效率一般。
>>### 函数
　　TFunc原型是function(const a:specialize List\<Variant\>):Variant。在自己定义同样参数和返回值的函数后，可以通过Assign(函数名,@函数)来将其关联到PSLib中。UsesSystem中也是通过这种方法添加函数的。函数在处理时，等价于把栈中的连续一段值反转拷贝过来操作。
>>### 解析
　　介绍一些内部用到的方法         
　　　　GetSeek  过滤字符，基本上在以下过程中均有用到      
　　　　GetAlpha  获取名称、数字           
　　　　GetBeta  获取运算符              
　　　　GetGama  匹配字符，比如提取一段单引号括起的字符串             
　　　　GetDelta  计算表达式            
　　　　GetEpsilon  获取合法语句块(表达式、关键字语句)             
　　　　GetLazy  获取语句域（用于惰性执行）           
　　以上再通过与Patt、Args、Script、ScriptDirector等复杂的递归糅合，可以实现脚本解析函数Exec。             
　　新建数组使用newpint，之后将Arr(array of longint)保存到ArrayManage中。使用[]含义是取地址，但只能取intarr和psstr的地址，用LongWord存地址。           
　　TransValue可以将地址转换成指向内容的longint。            
　　关于逗号(,)，可以保留栈中的元素，所以其作用可以认为是返回最右边一个元素的值，但在函数以外使用可能会造成不能正常地弹出栈（这是属于未定义情况而非pascalscript代码上的BUG）。
>>### 报错
　　PascalScript1.0基本没有报错能力，遇到217则是某种溢出或越界或指针非法，可以试着联系shyakocat或调试源码查错。一般的，数组、赋值、地址等错误的情况下ps集成了一些报错（错误码大于1000），故遇到1000以上的错误码则可以对照源码分析错因。pascalscript遇到语法错误，则会以某种错误方式继续执行下去，所以请注意写对。

>## 样例
>>### 斐波那契数列
```
a[10]                                           //定义int数组a[0..10]
a[0]:=a[1]:=1                                   //初始化a[0]和a[1]为1
for i:=2 to 10 do writeln(a[i]:=a[i-1]+a[i-2]); //记得一定要加分号来终止for
```
>>### 提取100内质数
```
for i:=2 to 100 do
 flag:=0      //此处不要加分号，不然for循环会终止
 for j:=2 to trunc(sqrt(i)) do
  if i mod j=0 then
   flag:=1
   break
  ;          //这个分号对应了(if)then语句的终止
 ;           //这个分号对应了for j:=...语句的终止
 if flag=0 then writeln('P(',c:=c+1,') = ',i);  //这个分号对应了(if)then语句的终止
;            //这个分号对应了for i:=...语句的终止
//分号不一定要换行写
```
>>### 角谷猜想
```
x:=39      //为什么选39懂的人肯定知道
while x>1 do
 writeln('X = ',X)
 if x mod 2=0 then x:=x>>1;    //这里要加分号来终止(if)then，不然解释器认为语句未结束
              else x:=x*3+1;;  //第一个分号终止了else，第二个分号终止了while
writeln('X = ',X)
```

>## Q&A
>>### 我怎么运行上面的代码？
　　只要在主程序中使用以下代码即可逐句地分析语句。
```
uses PascalScriptUnit;
var
 a:PSLib;
 s:ansistring;
begin
// assign(input,'Test.in'); reset(input);
 a.UsesSystem;

 while not eof do
 begin
  readln(s);
  a.Exec(s)
 end;

end.
```
>>### 这个东西有什么用？ 
　　PascalScript1.0现在还比较基础，只能实现很初步的语法分析功能。欢迎充实PS的语法和函数。他的用处可以慢慢发掘。              
　　当制作一个工程的时候，我们可以模块化这个工程，把一些要用的功能以类似插件的形式写到脚本中。这样，不用每次更新庞大的主程序，只要发布短短的更新脚本即可。而且有能力的用户（开发者）可以自己写脚本来达到一些简便的、个性化的目的。             
　　比如制作【游戏王】、【三国杀】等卡牌游戏时，由于卡片比较多不能一时写完所有卡片，官方也可能更新卡片的数据。此时，我们可以把每张卡片写成一个脚本，告诉主程序处理方式或者直接替主程序处理，这样后续的更新一定程度上就更方便了。         
             
           
             
            
             
            
             
># ◆2017/10/13  PascalScript2.0    
>## 与PS1.0有什么区别？    
    原本以Variant表示任意值，现在以PSValue表示。（顺便一说，在做SAGalgame.pas时发现Variant的表达对PSValue也适用，PS1.0版本大概在PS2.0中可能可以兼容很多内容）         
    PSValue是以vTypeMode（类型）和pointer（指针）记录一个变量。现阶段暂时未考虑内存的回收（？）    
>## 新增了哪些内容？    
>>### 自定义函数（递归斐波那契数列）    
```
function Fib(n)If(n=0)or(n=1)Then Exit(1);Exit(Fib(n-1)+Fib(n-2));
For i:=1 to 20 Do WriteLn('Fib(',i,') = ',Fib(i));
//由此代码可以测试PascalScript的速度，好像并不是很快
```
>>### 文件操作
```
fileopen('User.in','r')
fileopen('User.out','w')
 a:=''
 readlnf(a)
 writelnf(a)
fileclose('r','w')
 b:=0
 readf(b)
 writef(b)  //关闭后默认在控制台中输入输出
```    
>>### 自定义类
```
object complex(real,imag)
function complex.create(a,b)real:=a,imag:=b;
function complex.add(a,b)real:=real+a,imag:=imag+b;

x:=complex
x.create(1,3)
For i:=1 to 10 Do x.add(x.real,x.imag);
WriteLn(x.real,' ',x.imag)
//暂时不支持编译错误，写码时谨慎
```
>>### 特殊的类（array）
```
function print()For i:=1 to w.Size Do Write(w[i],' ');WriteLn;
{
    新增大括号注释，可以将一长段注释，也可以在语句中间位置注释，相当于空格的存在
    一般不要在关键词语句判定区使用大括号注释（如紧接function,if之后）
}
u:=array
u.Create               {第一种构造数组的方法，先赋成数组array再初始化create}
u.PushBack(2,'xyz',2.7183)
v:=['Test',666,0.10]   {第二种构造数组的方法，用[若干元素]表示数组}
w:=u+v
Print
w.Reverse(1,6)
w.Delete(2,3)
w.Insert(1,'CCS',998244353,-0.1)
Print
```
                 
                
                
                
># ◆2017/11/9 PascalScript新增编译等级
>## 编译等级是什么？
     编译等级其实是解释器解释语句时出错时的处理办法。（在之前的版本中编译错误是直接退出程序的）
     现在处理方式有cl_ignore<直接忽视编译错误，继续执行代码，错误会被记录到ErrorList>,cl_terminate<编译错误发生时，终止正在执行的语句，并且该类会被打上终止的标记，错误会被记录到ErrorList>,cl_halt<编译错误发生时，直接halt(退出程序)>。ErrorList是一个IList（Longint的List，可认为是一个记录错误码的数组）。错误码可以通过查看PSLibError数组得到对应的错因（Ansistring）。
>## 编译等级怎么使用？
```
uses PascalScriptUnit;
var
 f:Text;
 a:PSLib;
 s:ansistring;
 i:Longint;
begin
 a.Clear;                          //初始化
 a.SetComplieLevel(cl_ignore);     //设置编译等级
 a.UsesSystem;                     //导入基础的函数
 assign(f,'Test.in'); reset(f);
 while not eof(F) do begin
  readln(f,s);
  a.Exec(s)
 end;
 Close(f);
 WriteLn(a.DangerTerminate);                                              //输出类是否被终止
 For i:=1 to a.ErrorList.Size Do WriteLn(PSLibError[a.ErrorList[i]])      //依次输出错因
end.
```



