{$mode objfpc}{$H+}
unit PascalScriptUnit;

interface

uses CommonTypeUnit,SysUtils,Classes,Variants;

var
 Pri:array[char(0)..char(255)]of byte;
 acceptset,runset,pattset:set of char;

type

 HashStr=object
  const E:int64=257;
  var Size,P:longint;
   head,next,hsid:specialize List<longint>;
   node:specialize List<Ansistring>;
  function hashcode(const s:ansistring):longint;
  procedure clear;
  procedure Add(const s:ansistring;id:longint);
  function Find(const s:ansistring):longint;
  Function Clone:HashStr;
 end;

Type
 vTypeMode=(PSNone,
            PSInt,
            PSDouble,
            PSChar,
            PSStr,
            PSFunc,
            PSFuncLocal,
            PSObj);
 PSValue=Record tp:vTypeMode; p:pointer End;

 VList=specialize List<PSValue>;
 SList=specialize List<Ansistring>;
 IList=Specialize List<Longint>;
 pVList=^VList;

 TFunc=function(const Param:VList):PSValue;

 OutFunc=Record Caption:Ansistring; main:TFunc End;
 LocalFunc=Record Caption:Ansistring; Carry,Code:SList End;
 LocalObj=Record Caption,FrameRead,FrameWrite:Ansistring; Outer:Pointer; Carry:SList; Data:VList; DataIndex:HashStr End;



 pInt=^longint;
 pDouble=^Extended;
//pChar=^Char;
 pAnsistring=^ansistring;
 pOutFunc=^OutFunc;
 pLocalFunc=^LocalFunc;
 pLocalObj=^LocalObj;
 pPSValue=^PSValue;

Const
 //GetAlpha -> TextContent
 //GetBeta  -> MarkContent
 PSLibError:Array[0..25]Of Ansistring=(
  '[Error000]:Transfer Variable Type Fail',
  '[Error001]:Calculate Variable Fail',
  '[Error002]:Variable Cannot Found',
  '[Error003]:Function Parameter Number Is Wrong',
  '[Error004]:Assign Variable Fail',
  '[Error005]:Object Or ObjectMember Cannot Found',
  '[Error006][KeywordError]:"for ..." Must Follow ":="',
  '[Error007][KeywordError]:"for ...:=" Must Follow "to"',
  '[Error008][KeywordError]:"for ...:=... to ..." Must Follow "do"',
  '[Error009][KeywordError]:"While ..." Must Follow "do"',
  '[Error010][KeywordError]:"function" Must Follow <TextContent>',
  '[Error011][KeywordError]:"function ..." Must Follow "("',
  '[Error012][KeywordError]:"function ...(" Must Follow Some <TextContent>',
  '[Error013][KeywordError]:"function ...(..." Must Follow "," or ")"',
  '[Error014][KeywordError]:"object" Must Follow <TextContent>',
  '[Error015][KeywordError]:"object ..." Must Follow "("',
  '[Error016][KeywordError]:"object ...(" Must Follow Some <TextContent>',
  '[Error017][KeywordError]:"object ...(..." Must Follow "," or ")"',
  '[Error018][SystemFunctionError]:Read() Should Be {Int/Double/Char/String}',
  '[Error019][SystemFunctionError]:Abs() Should Be {Int/Double}',
  '[Error020][SystemFunctionError]:Sqr() Should Be {Int/Double}',
  '[Error021][SystemFunctionError]:Sqrt() Should Be {Int/Double}',
  '[Error022][SystemFunctionError]:Round() Should Be {Int/Double}',
  '[Error023][SystemFunctionError]:Trunc() Should Be {Int/Double}',
  '[Error024][SystemFunctionError]:Copy() Should Be {String,Int,Int}',
  '[Error025][SystemFunctionError]:Pos() Should Be {String,String}');



 cl_ignore=0;
 cl_terminate=1;
 cl_halt=2;

Type

 PSLib=Packed object
   Size:longint;
   Vname:HashStr;
   VData:VList;

   ComplieLevel:ShortInt;
    //0 = Ignore Any Error      And     Record Error To ErrorList
    //1 = Terminate The Code    And     Record Error To ErrorList
    //2 = Halt The Program
   DangerTerminate:Boolean;
   ErrorList:IList;

   NoteFlag:Boolean;
   LazNet:specialize List<longint>;
   Patt,Nest,ForBegin,ForEnd,LazTag:longint;
   Script,Carry:SList;
   ScriptDirector:ansistring;
   Procedure Reg(_E:Longint);
   Procedure SetComplieLevel(_c:ShortInt);
   function Find(const _name:string):longint;
   Function Add(const _name:string;_type:vtypemode):Longint;
   Procedure Assign(Const _Name:String;Const _Value:PSValue);
   procedure Assign(const _name:string;const _value:longint);
   procedure Assign(const _name:string;const _value:Extended);
   procedure Assign(const _name:string;const _value:ansistring);
   procedure Assign(const _name:string;const _value:TFunc);
   procedure Assign(const _name:string;const _value:LocalFunc);
   procedure Assign(const _name:string;const _value:LocalObj);
   Function Assign(Const _Name:String;Const _Param:VList):PSValue;
   Function Get(Const _Name:String):PSValue;
   Function Get(Const _Name:String;Const _Param:VList):PSValue;
   function GetType(const _name:string):vtypemode;
   Function AssignObj(Const _Name:Ansistring;Const _Value:TFunc):Boolean;
   Function AssignObj(Const _Name:Ansistring;Const _Value:PSValue):Boolean;
   Function AddFunc(Const _Name:Ansistring;Const _Carry,_Code:SList):Boolean;
   function Exec(os:ansistring):longint;
   function Exec(var ss:SList):longint;
   Function ExecFunc(Var LF:LocalFunc;Const _Param:VList):PSValue;
   Function ExecObj(Var LO:LocalObj;Const _CallName:Ansistring;Const _Param:VList):PSValue;
   procedure UsesSystem;
   procedure clear;
 end;

 function newpsvalue(const psva:psvalue):psvalue;
 function newpsvalue(pskind:vtypemode;pspointer:pointer):psvalue;
 operator :=(const _v:longint)_r:psvalue;
 operator :=(const _v:extended)_r:psvalue;
 operator :=(const _v:char)_r:psvalue;
 operator :=(const _v:ansistring)_r:psvalue;
 operator :=(const _v:outfunc)_r:psvalue;
 operator :=(const _v:localfunc)_r:psvalue;
 operator :=(const _v:localobj)_r:psvalue;
 operator :=(const a:psvalue)b:longint;
 operator :=(const a:psvalue)b:extended;
 operator :=(const a:psvalue)b:char;
 operator :=(const a:psvalue)b:ansistring;
 operator :=(const a:psvalue)b:outfunc;
 operator :=(const a:psvalue)b:localfunc;
 operator :=(const a:psvalue)b:localobj;
 operator :=(const a:psvalue)b:variant;
 operator -(const a:psvalue)c:psvalue;
 operator +(const a,b:psvalue)c:psvalue;
 operator -(const a,b:psvalue)c:psvalue;
 operator *(const a,b:psvalue)c:psvalue;
 operator /(const a,b:psvalue)c:psvalue;
 operator div(const a,b:psvalue)c:psvalue;
 operator mod(const a,b:psvalue)c:psvalue;
 operator =(const a,b:psvalue)c:psvalue;
 operator >(const a,b:psvalue)c:psvalue;
 operator <(const a,b:psvalue)c:psvalue;
 operator >=(const a,b:psvalue)c:psvalue;
 operator <=(const a,b:psvalue)c:psvalue;
 operator not(const a:psvalue)c:psvalue;
 operator and(const a,b:psvalue)c:psvalue;
 operator or(const a,b:psvalue)c:psvalue;
 operator xor(const a,b:psvalue)c:psvalue;
 operator shl(const a,b:psvalue)c:psvalue;
 operator shr(const a,b:psvalue)c:psvalue;
 procedure free(var a:psvalue);
 procedure swap(var a,b:psvalue);


Const
 CalcSet=[PSInt,PSDouble];
 StrSet=[PSChar,PSStr];
 FuncSet=[PSFunc,PSFuncLocal];

Var
//These are Some Const
 PS0,PS1,PSNil:PSValue;

 ResultList:VList;
 ExitFlag:Boolean=False;
 PublicObjOuter:^Pointer;
 publicPSLib:^PSLib;

 PSStdIn,PSStdOut:Text;

implementation

var
 debug:pointer;

 Operator +(Const a,b:VList)c:VList;
 Var i:Longint;
 Begin
  c.Clear;
  c.ReSize(a.Size+b.Size);
  For i:=1 to a.Size Do c.Items[i]:=a.Items[i];
  For i:=1 to b.Size Do c.Items[i+a.Size]:=b.Items[i];
 End;

 Function NewPSValue(Const PSVa:PSValue):PSValue;
 Var Tmp:Pointer;
 Begin
  With PSVa Do
   Case tp Of
    PSInt      :Exit(PInt(p)^);
    PSDouble   :Exit(PDouble(P)^);
    PSChar     :Exit(PChar(P)^);
    PSStr      :Exit(PAnsistring(P)^);
    PSFunc     :Exit(pOutFunc(P)^);
    PSFuncLocal:Exit(PLocalFunc(P)^);
    PSObj      :Exit(LocalObj(PSva))
   End;
  Exit(PSNil)
 End;

 Function NewPSValue(PSKind:vTypeMode;PSPointer:Pointer):PSValue;
 Begin
  With Result Do
  Begin
   tp:=PSKind;
   P:=PSPointer
  End
 End;

 Operator :=(Const _V:Longint)_R:PSValue;
 Var Tmp:PInt;
 Begin
  New(Tmp);
  Tmp^:=_V;
  Exit(NewPSValue(PSInt,Tmp))
 End;

 Operator :=(Const _V:Extended)_R:PSValue;
 Var Tmp:PDouble;
 Begin
  New(Tmp);
  Tmp^:=_V;
  Exit(NewPSValue(PSDouble,Tmp))
 End;

 Operator :=(Const _V:Char)_R:PSValue;
 Var Tmp:PChar;
 Begin
  New(Tmp);
  Tmp^:=_V;
  Exit(NewPSValue(PSChar,Tmp))
 End;

 Operator :=(Const _V:Ansistring)_R:PSValue;
 Var Tmp:PAnsistring;
 Begin
  New(Tmp);
  Tmp^:=_V;
  Exit(NewPSValue(PSStr,Tmp))
 End;

 Operator :=(Const _V:OutFunc)_R:PSValue;
 Var Tmp:POutFunc;
 Begin
  New(Tmp);
  Tmp^:=_V;
  Exit(NewPSValue(PSFunc,Tmp))
 End;

 Operator :=(Const _V:LocalFunc)_R:PSValue;
 Var Tmp:PLocalFunc;
 Begin
  New(Tmp);
  Tmp^:=_V;
  Exit(NewPSValue(PSFuncLocal,Tmp))
 End;

 Operator :=(Const _V:LocalObj)_R:PSValue;
 Var Tmp:PLocalObj;
 Begin
  New(Tmp);
  Tmp^:=_V;
  Exit(NewPSValue(PSObj,Tmp))
 End;

 Operator :=(Const A:PSVALUE)B:Longint;
 Begin
  If a.tp<>PSInt Then Begin PublicPSLib^.Reg(1); Exit End; //Trans Error
  Exit(PInt(a.p)^)
 End;

 Operator :=(Const A:PSVALUE)B:Extended;
 Begin
  If a.tp=PSInt Then Exit(PInt(a.p)^);
  If a.tp<>PSDouble Then Begin PublicPSLib^.Reg(1); Exit End;
  Exit(PDouble(a.p)^)
 End;

 Operator :=(Const A:PSVALUE)B:Char;
 Begin
  If a.tp<>PSChar Then Begin PublicPSLib^.Reg(1); Exit End;
  Exit(PChar(a.p)^)
 End;

 Operator :=(Const A:PSVALUE)B:Ansistring;
 Begin
  If a.tp=PSChar Then Exit(PChar(a.p)^);
  If a.tp<>PSStr Then Begin PublicPSLib^.Reg(1); Exit End;
  Exit(PAnsistring(a.p)^)
 End;

 Operator :=(Const A:PSVALUE)B:OutFunc;
 Begin
  If a.tp<>PSFunc Then Begin PublicPSLib^.Reg(1); Exit End;
  Exit(POutFunc(a.p)^)
 End;

 Operator :=(Const A:PSVALUE)B:LocalFunc;
 Begin
  If a.tp<>PSFuncLocal Then PublicPSLib^.Reg(1);
  Exit(PLocalFunc(a.p)^)
 End;

{
 Function Clone(Const A:LocalObj):LocalObj;
 Var Fake:pLocalObj;
 Begin
  New(Fake);
  Fake^.Caption:=A.Caption;
  Fake^.Outer:=A.Outer;
  Fake^.Carry:=A.Carry.Clone(1,A.Carry.Size);
  Fake^.Data:=A.Data.Clone(1,A.Data.Size);
  Fake^.DataIndex:=A.DataIndex.Clone;
  Exit(Fake^)
 End;
}

 Operator :=(Const A:PSVALUE)B:LocalObj;
 Begin
  If a.tp<>PSObj then Begin PublicPSLib^.Reg(1); Exit End;
  Exit(pLocalObj(a.p)^)
 End;

 Operator :=(Const A:PSVALUE)B:Variant;
 Begin
  Case a.tp Of
   PSInt:Exit(Longint(A));
   PSDouble:Exit(Extended(A));
   PSChar:Exit(Char(A));
   PSStr:Exit(Ansistring(A));
   Else Exit(HexStr(a.P))
  End
 End;

 Operator -(Const a:PSValue)c:PSValue;
 Begin
  Case a.tp Of
   PSInt   :Exit(PSValue(-Longint(a)));
   PSDouble:Exit(PSVAlue(-Extended(A)))
  End;
  PublicPSLib^.Reg(1)//Calc Error
 End;

 Operator +(Const a,b:PSValue)C:PSValue;
 Var Tmp:pVList;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(Longint(A)+Longint(B)));
  If (a.tp in CalcSet)And(b.tp in CalcSet) Then
   Exit(PSValue(Extended(A)+Extended(B)));
  If (a.tp in StrSet)And(b.tp in StrSet) Then
   Exit(PSValue(Ansistring(A)+Ansistring(B)));
  If (a.tp=PSObj)And(pLocalObj(a.p)^.Caption='array')And
     (b.tp=PSObj)And(pLocalObj(b.p)^.Caption='array') Then
  Begin New(Tmp);
        Tmp^:=pVList(pLocalObj(a.p)^.Outer)^
             +pVList(pLocalObj(b.p)^.Outer)^;
        Result.tp:=PSObj; New(pLocalObj(Result.p));
        pLocalObj(Result.p)^:=pLocalObj(A.p)^; pLocalObj(Result.p)^.Outer:=Tmp;
        Exit End;
  PublicPSLib^.Reg(1)
 End;

 Operator -(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(LongInt(A)-LongInt(B)));
  If (a.tp in CalcSet)And(b.tp in CalcSet) Then
   Exit(PSValue(Extended(A)-Extended(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator *(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(LongInt(A)*LongInt(B)));
  If (a.tp in CalcSet)And(b.tp in CalcSet) Then
   Exit(PSValue(Extended(A)*Extended(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator /(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp in CalcSet)And(b.tp in CalcSet) Then
   Exit(PSValue(Extended(A)/Extended(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator Div(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(LongInt(A) Div LongInt(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator Mod(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(Longint(A) Mod Longint(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator =(Const a,b:PSValue)C:PSValue;
 var bool:Boolean=False;
 Begin
  If a.tp<>b.tp Then Exit(PSValue(0));
  If a.tp in CalcSet Then bool:=Extended(A)=Extended(B) Else
  If a.tp in StrSet  Then bool:=Ansistring(A)=Ansistring(B);
  Exit(PSValue(Ord(bool)))
 End;

 Operator >(Const a,b:PSValue)C:PSValue;
 var bool:Boolean=False;
 Begin
  If a.tp<>b.tp Then Exit(PSValue(0));
  If a.tp in CalcSet Then bool:=Extended(A)>Extended(B) Else
  If a.tp in StrSet  Then bool:=Ansistring(A)>Ansistring(B);
  Exit(PSValue(Ord(bool)))
 End;

 Operator <(Const a,b:PSValue)C:PSValue;
 var bool:Boolean=False;
 Begin
  If a.tp<>b.tp Then Exit(PSValue(0));
  If a.tp in CalcSet Then bool:=Extended(A)<Extended(B) Else
  If a.tp in StrSet  Then bool:=Ansistring(A)<Ansistring(B);
  Exit(PSValue(Ord(bool)))
 End;

 Operator >=(Const a,b:PSValue)C:PSValue;
 var bool:Boolean=False;
 Begin
  If a.tp<>b.tp Then Exit(PSValue(0));
  If a.tp in CalcSet Then bool:=Extended(A)>=Extended(B) Else
  If a.tp in StrSet  Then bool:=Ansistring(A)>=Ansistring(B);
  Exit(PSValue(Ord(bool)))
 End;

 Operator <=(Const a,b:PSValue)C:PSValue;
 var bool:Boolean=False;
 Begin
  If a.tp<>b.tp Then Exit(PSValue(0));
  If a.tp in CalcSet Then bool:=Extended(A)<=Extended(B) Else
  If a.tp in StrSet  Then bool:=Ansistring(A)<=Ansistring(B);
  Exit(PSValue(Ord(bool)))
 End;

 Operator Not(Const a:PSValue)C:PSValue;
 Begin
  If a.tp=PSInt Then
   Exit(PSValue(Not Longint(A)));
  PublicPSLib^.Reg(1)
 End;

 Operator And(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(Longint(A) And Longint(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator Or(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(Longint(A) Or Longint(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator Xor(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(Longint(A) Xor Longint(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator Shl(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(Longint(A) Shl Longint(B)));
  PublicPSLib^.Reg(1)
 End;

 Operator Shr(Const a,b:PSValue)C:PSValue;
 Begin
  If (a.tp=PSInt)And(b.tp=PSInt) Then
   Exit(PSValue(Longint(A) Shr Longint(B)));
  PublicPSLib^.Reg(1)
 End;

 Procedure Free(Var a:PSValue);
 Begin
  a.tp:=PSNone;
  FreeAndNil(A.p)
 End;

 procedure swap(var a,b:PSValue);
 var c:PSValue; begin c:=a; a:=b; b:=c end;


 Function NewOutFunc(Const _Name:Ansistring;Const _main:TFunc):OutFunc;
 Begin
  Result.Caption:=_Name;
  Result.Main:=_Main
 End;

 Function NewLocalFunc(Const _name:Ansistring;Const _carry,_code:SList):LocalFunc;
 Begin
  Result.Caption:=_Name;
  Result.Carry:=_carry.Clone(1,_carry.Size);
  Result.Code:=_code.Clone(1,_code.Size);
 End;

 Function NewLocalObj(Const _Name:Ansistring):LocalObj;
 Begin
  With Result Do
  Begin
   Caption:=_Name;
   FrameRead:='';
   FrameWrite:='';
   Outer:=Nil;
   Carry.Clear;
   Data.Clear;
   DataIndex.Clear;
  End
 End;

 Function NewLocalObj(Const _Name,_FrameR,_FrameW:Ansistring):LocalObj;
 Begin
  With Result Do
  Begin
   Caption:=_Name;
   FrameRead :=_FrameR;
   FrameWrite:=_FrameW;
   Outer:=Nil;
   Carry.Clear;
   Data.Clear;
   DataIndex.Clear;
  End
 End;

 Function NewLocalObj(Const _Name:Ansistring;Const _carry:SList):LocalObj;
 Var i:Longint;
 Begin
  With Result Do
  Begin
   Caption:=_Name;
   FrameRead:='';
   FrameWrite:='';
   Outer:=Nil;
   Carry:=_carry.clone(1,_carry.Size);
   DataIndex.Clear;
   Data.Clear;
   For i:=1 to _carry.Size Do
   Begin
    DataIndex.Add(_carry.Items[i],i);
    Data.PushBack(PSNil)
   End
  End
 End;


 function HashStr.hashcode(const s:ansistring):longint;
 var i:longint;
 begin
  hashcode:=0;
  for i:=1 to length(s) do
   hashcode:=(hashcode*E+ord(s[i]))mod P
 end;

 procedure HashStr.Clear;
 begin
  P:=0;
  Size:=0;
  head.clear;
  next.clear;
  node.clear;
  hsid.clear;
 end;

 procedure HashStr.Add(const s:ansistring;id:longint);
 var u,i:longint;
 begin
  inc(Size);
  node.pushback(s);
  hsid.pushback(id);
  if Size*2>P then begin
   P:=P*2+1;
   head.resize(P);
   head.fill(0,P,0);
   next.clear;
   for i:=1 to Size do
   begin
    u:=hashcode(node.Items[i]);
    next.pushback(head.Items[u]);
    head.Items[u]:=i
   end
  end
  else
  begin
   u:=hashcode(s);
   next.pushback(head.Items[u]);
   head.Items[u]:=Size
  end
 end;

 function HashStr.Find(const s:ansistring):longint;
 var u,i:longint;
 begin
  if Size=0 then exit(-1);
  u:=hashcode(s);
  i:=head.Items[u];
  while i<>0 do
  begin
   if node.Items[i]=s then exit(hsid.Items[i]);
   i:=next.Items[i]
  end;
  exit(-1)
 end;

 Function HashStr.Clone:HashStr;
 Begin
  Clone.Clear;
  Clone.P:=P;
  Clone.Size:=Size;
  Clone.Head:=Head.Clone(1,Head.Size);
  Clone.Next:=Next.Clone(1,NEXT.Size);
  Clone.Node:=Node.Clone(1,NODE.Size);
  Clone.HSID:=HSID.Clone(1,HSID.Size);
 End;

 Function PSLib.Add(const _name:string;_type:vtypemode):Longint;
 Var Tmp:Pointer;
 begin
  inc(Size);
  vname.Add(_name,Size);
  case _type of
   PSNone     :Exit(-1);
   PSInt      :New(Pint(Tmp));
   PSDouble   :New(PDouble(Tmp));
   PSChar     :New(PChar(Tmp));
   PSStr      :New(PAnsiString(Tmp));
   PSFunc     :New(POutFunc(Tmp));
   PSFuncLocal:New(PLocalFunc(Tmp));
   PSObj      :New(PLocalObj(Tmp))
  End;
  VData.PushBack(NewPSValue(_Type,Tmp));
  Exit(Size)
 end;

 procedure PSLib.Clear;
 begin
  Size:=0;
  ComplieLevel:=0;
  DangerTerminate:=False;
  NoteFlag:=False;
  vName.Clear;
  vData.Clear;
  ErrorList.Clear;
  LazNet.Clear;
  Patt:=0;
  Nest:=0;
 end;

 Procedure PSLib.Reg(_E:Longint);
 Begin
  DangerTerminate:=True;
  Case ComplieLevel Of
   cl_ignore   :Begin ErrorList.PushBack(_E); DangerTerminate:=False End;
   cl_terminate:Begin ErrorList.PushBack(_E); DangerTerminate:=True End;
   cl_halt:Halt(_E+1000)
  End
 End;

 Procedure PSLib.SetComplieLevel(_c:ShortInt);
 Begin
  ComplieLevel:=_c
 End;

 function PSLib.Find(const _name:string):longint;
 var i:longint;
 begin
  exit(vname.Find(_name))
 end;

 Procedure PSLib.Assign(Const _Name:String;Const _Value:PSValue);
 Var i:Longint;
 Begin
  i:=Find(_Name);
  If i=-1 Then I:=Add(_Name,PSInt);
  vData.Items[i]:=NewPSValue(_Value)
 End;

 Function PSLib.Assign(Const _Name:String;Const _Param:VList):PSValue;
 Var i:Longint;
 Begin
  i:=Find(_Name); If I=-1 Then Begin Reg(2); Exit End;//Get Fail
  Result:=vData.Items[i];
  Case Result.Tp Of
   PSStr      :If _Param.Size<2 Then Begin Reg(4); Exit End //Parameter Error
               Else Begin pAnsistring(Result.P)^[Longint(_Param.Items[1])]:=Ansistring(_Param.Items[2])[1]; Exit(PS1) End;
   PSObj      :Exit(ExecObj(pLocalObj(Result.p)^,pLocalObj(Result.p)^.FrameWrite,_Param));
  End
 End;

 procedure PSLib.Assign(const _name:string;const _value:longint);
 var i:longint;
 begin
  i:=Find(_Name);
  If i=-1 Then I:=Add(_Name,PSInt);
  vData.Items[i]:=PSValue(_Value)
 end;

 procedure PSLib.Assign(const _name:string;const _value:Extended);
 var i:longint;
 begin
  i:=Find(_Name);
  If i=-1 Then I:=Add(_Name,PSDouble);
  vData.Items[i]:=Extended(_Value)
 end;

 procedure PSLib.Assign(const _name:string;const _value:ansistring);
 var i:longint;
 begin
  i:=Find(_Name);
  If i=-1 Then I:=Add(_Name,PSStr);
  vData.Items[i]:=Ansistring(_Value)
 end;

 procedure PSLib.Assign(const _name:string;const _value:TFunc);
 var i:longint;
 begin
  i:=Find(_Name);
  If i=-1 Then I:=Add(_Name,PSFunc);
  vData.Items[i]:=NewOutFunc(_name,_Value)
 end;

 procedure PSLib.Assign(const _name:string;const _value:LocalFunc);
 var i:longint;
 begin
  i:=Find(_Name);
  If i=-1 Then I:=Add(_Name,PSFuncLocal);
  vData.Items[i]:=LocalFunc(_Value)
 end;

 procedure PSLib.Assign(const _name:string;const _value:LocalObj);
 var i:longint;
 begin
  i:=Find(_Name);
  If i=-1 Then I:=Add(_Name,PSObj);
  vData.Items[i]:=LocalObj(_Value)
 end;

 function PSLib.Get(const _name:string):PSValue;
 var i:longint;
 begin
  i:=Find(_Name); If I=-1 Then Begin Reg(2); Exit End;//Get Fail
  Exit(vData.Items[i])
 end;

 function PSLib.Get(const _name:string;Const _Param:VList):PSVAlue;
 var
  i:longint;
  ObjName,CarName:Ansistring;
 begin
  i:=Pos('.',_name); If i>0 Then
  Begin
   ObjName:=Copy(_Name,1,i-1);
   CarName:=Copy(_Name,1+i,Length(_Name));
   i:=Find(ObjName); if (i=-1)or(vData.Items[i].tp<>PSObj) then Begin Reg(2); Exit(-1) End;
   Exit(ExecObj(pLocalObj(vData.Items[i].p)^,CarName,_Param))
  End;
  i:=Find(_Name); If I=-1 Then Begin Reg(2); Exit End;//Get Fail
  Result:=vData.Items[i];
  Case Result.Tp Of
   PSFunc     :Exit(pOutFunc(Result.p)^.main(_Param));
   PSFuncLocal:Exit(ExecFunc(pLocalFunc(Result.P)^,_Param));
   PSStr      :With _Param.Items[1] Do If tp=PSInt Then
                Exit(Char(pAnsistring(Result.P)^[PInt(P)^]));
   PSObj      :Exit(ExecObj(pLocalObj(Result.p)^,pLocalObj(Result.p)^.FrameRead,_Param));
  End
 end;

 function PSLib.GetType(const _name:string):vtypemode;
 var i:longint;
 begin
  I:=Find(_Name); If i=-1 Then Exit(PSNone);
  Exit(vData.Items[i].tp)
 end;

 Function PSLib.AssignObj(Const _Name:Ansistring;Const _Value:TFunc):Boolean;
 Begin
  AssignObj(_Name,NewOutFunc(_Name,_Value))
 End;

 Function PSLib.AssignObj(Const _Name:Ansistring;Const _Value:PSValue):Boolean;
 Var
  d:Longint;
  ObjName,CarName:Ansistring;
 Begin
  d:=Pos('.',_Name);
  ObjName:=Copy(_Name,1,d-1);
  CarName:=Copy(_Name,1+d,Length(_Name));
  If (ObjName='')Or(CarName='') Then Exit(False);
  d:=Find(ObjName);
  If (D=-1)Or(D<>-1)And(vData.Items[D].tp<>PSObj) Then Exit(False);
  with pLocalObj(vData.Items[D].p)^ Do
  Begin
   d:=DataIndex.Find(CarName);
   If d=-1 Then Begin Data.PushBack(_Value); DataIndex.Add(CarName,Data.Size) End
   Else Data.Items[D]:=_Value
  End;
  Exit(True)
 End;

 Function PSLib.AddFunc(Const _Name:Ansistring;Const _Carry,_Code:SList):Boolean;
 Var d:Longint;
 Begin
  d:=Pos('.',_Name);
  If D>0 Then Exit(AssignObj(_Name,NewLocalFunc(Copy(_Name,D+1,Length(_Name)),_Carry,_Code)));
  Assign(_Name,NewLocalFunc(_Name,_carry,_code));
  Exit(True)
 End;

 Function PSLib.ExecFunc(Var LF:LocalFunc;Const _Param:VList):PSValue;
 Var
  i,d:Longint;
  SrcNam:SList;
  SrcVal:VList;
 Begin
  If DangerTerminate Then Exit(PSNil);
  SrcNam.Clear;
  SrcVal.Clear;
  With LF Do
  Begin
   If _Param.Size<>Carry.Size Then Begin Reg(3); Exit End;
   SrcNam.PushBack('result');
   d:=Find('result');
   If d=-1 Then d:=Add('result',PSInt);
   SrcVal.PushBack(vData.Items[d]);
   For i:=1 to Carry.Size Do
   Begin
    SrcNam.PushBack(Carry.Items[i]);
    d:=Find(Carry.Items[i]);
    If d=-1 Then d:=Add(Carry.Items[i],PSInt);
    SrcVal.PushBack(vData.Items[d]);
    Assign(Carry.Items[i],_Param.Items[i]);
   End;
   Exec(Code);
   ExitFlag:=False;
   Result:=Get('result');
   For i:=1 to SrcNam.Size Do
    Assign(SrcNam.Items[i],SrcVal.Items[i])
  End
 End;

 Function PSLib.ExecObj(Var LO:LocalObj;Const _CallName:Ansistring;Const _Param:VList):PSValue;
 Var
  i,d:Longint;
  SrcNam:SList;
  SrcVal:VList;
  Tmp:PSValue;
  opFlag:^Pointer;
  opPSLib:Pointer;

  Procedure Seto;Begin PublicObjOuter:=opFlag; PublicPSLib:=opPSLib End;

 Begin
  If DangerTerminate Then Exit(PSNil);
  opFlag:=PublicObjOuter;
  opPSLib:=PublicPSLib;
  With LO Do
  Begin
   PublicObjOuter:=@Outer;
   PublicPSLib:=@Self;
   d:=DataIndex.Find(_CallName);
   If D=-1 Then Begin Seto; Exit(PSNil) End;
   Tmp:=Data.Items[d];
   Case Tmp.tp Of
    PSFunc,PSFuncLocal:;
    Else Begin Seto; Exit(Tmp) End
   End;
   SrcNam.Clear;
   SrcVal.Clear;
   For i:=1 to Carry.Size Do
   Begin
    d:=Find(Carry.Items[i]);
    If d=-1 Then d:=Add(Carry.Items[i],PSInt);
    SrcNam.PushBack(Carry.Items[i]);
    SrcVal.PushBack(vData.Items[d]);
    Assign(Carry.Items[i],Data.Items[i])
   End;
   If Tmp.tp=PSFunc Then Result:=pOutFunc(Tmp.p)^.Main(_Param) Else
   If Tmp.tp=PSFuncLocal Then Result:=ExecFunc(pLocalFunc(Tmp.p)^,_Param) Else
   Result:=PSNil;
   For i:=1 to Carry.Size Do
   Begin
    D:=DataIndex.Find(Carry.Items[i]);
    If d<>-1 Then Data.Items[D]:=Get(SrcNam.Items[i]);
    Assign(SrcNam.Items[i],SrcVal.Items[i])
   End
  End;
  Seto
 End;

 function PSLib.Exec(var ss:SList):longint;
 var
  i,_ForB,_ForE:longint;
  _Patt,_Nest:longint;
  _LNet:specialize List<longint>;
  _Script,_Carry:SList;
  _ScriptD:ansistring;
 begin
  Exec:=0;
  If DangerTerminate Then Exit;
  if ss.Size<1 then exit;
  _Patt:=Patt;
  _Nest:=Nest;
  _ForB:=ForBegin;
  _ForE:=ForEnd;
  _LNet:=LazNet.clone(1,LazNet.Size);
  _Script:=SS.clone(1,SS.Size);
  _Carry:=Carry.Clone(1,Carry.Size);
  _ScriptD:=ScriptDirector;
  Patt:=0;
  Nest:=0;
  LazNet.Clear;
  Script.Clear;
  Carry.Clear;
  ScriptDirector:='';
  for i:=1 to _Script.Size do
  If DangerTerminate Then Break Else
   case Exec(_Script.Items[i]) of
    1:begin Exec:=1; break end;
    2:begin Exec:=2; break end;
    3:begin Exec:=3; Break End;
   end;
  Patt:=_Patt;
  Nest:=_Nest;
  ForBegin:=_ForB;
  ForEnd:=_ForE;
  LazNet:=_LNet;
  SS:=_Script;
  Carry:=_Carry.Clone(1,_Carry.Size);
  ScriptDirector:=_ScriptD
 end;

 function PSLib.Exec(os:ansistring):longint;
 var
  i,j,Bias:longint;
  s,t:ansistring;

  procedure GetSeek(const s:ansistring;var i:longint);
  begin
   while (i<=length(s))and(not((s[i]in acceptset)or(s[i]in runset))) do inc(i)
  end;

  function GetAlpha(const s:ansistring;var i:longint):ansistring;
  var Start:longint;
  begin
   GetSeek(s,i);
   Start:=i;
   while (i<=length(s))and(s[i] in acceptset) do inc(i);
   exit(Copy(s,Start,i-Start))
  end;

  function GetAlphaSafe(const s:ansistring;var i:longint):ansistring;
  var Start,j:longint;
  begin
   j:=i;
   GetSeek(s,i);
   Start:=i;
   while (i<=length(s))and(s[i] in acceptset) do inc(i);
   GetAlphaSafe:=Copy(s,Start,i-Start);
   Bias:=i;
   i:=j
  end;

  function GetBeta(const s:ansistring;var i:longint):ansistring;
  var Start:longint;
  begin
   GetSeek(s,i);
   Start:=i;
   if (i<=length(s))and(s[i]in pattset) then begin inc(i); exit(s[Start]) end;
   while (i<=length(s))and((s[i]in runset)and(not(s[i]in pattset))) do inc(i);
   exit(Copy(s,Start,i-Start))
  end;

  function GetBetaSafe(const s:ansistring;var i:longint):ansistring;
  var Start,j:longint;
  begin
   j:=i;
   GetSeek(s,i);
   Start:=i;
   if (i<=length(s))and(s[i]in pattset) then begin i:=j; exit(s[Start]) end;
   while (i<=length(s))and((s[i]in runset)and(not(s[i]in pattset))) do inc(i);
   GetBetaSafe:=Copy(s,Start,i-Start);
   Bias:=i;
   i:=j
  end;

  function GetGama(const s:ansistring;var i:longint;c:char):ansistring;
  var Start:longint;
  begin
   Start:=i;
   while (i<=length(s))and(s[i]<>c) do inc(i);
   GetGama:=Copy(os,Start,i-Start);
   inc(i)
  end;

  function GetDelta(const s:ansistring;var i:longint):PSValue;
  var
   LastAlpha:Boolean=False;
   tmp0:char;
   tmp1,tmp2,tmpo:PSValue;
   Nam:specialize List<ansistring>;
   Vau:specialize List<PSValue>;
   Opt:specialize List<char>;
   now:ansistring;
   mean:char;
   NumA:longint;
   NumB:Extended;
   NumC:ansistring;
   NumParam:VList;
   NumParamBase:specialize List<longint>;
   NumParamName:specialize List<ansistring>;
   d:longint;
   ObjName,CarName:Ansistring;
   obj:LocalObj;
   arr:pVList;

   Procedure NumParamCharge;
   Begin
    NumParam.Clear;
    while Vau.Size>NumParamBase.top do
    begin
     NumParam.pushback(Vau.top);
     Vau.pop; Nam.pop
    end;
    NumParam.Reverse(1,NumParam.Size);
   End;

   procedure Flush(pr:longint);
   var
    tmpnam:ansistring;
   begin
    while (not Opt.isnil)and(pr<=Pri[Opt.top]) do
    begin
     tmp0:=Opt.top;
     Opt.pop;
     if tmp0='~' then
     begin
      tmp1:=Vau.top; Vau.pop; Nam.pop;
      Vau.pushback(tmp1=PS0);
      Nam.pushback(' pending...')
     end
     else
     if tmp0='_' Then
     Begin
      tmp1:=Vau.top; Vau.pop; Nam.pop;
      Vau.pushback(-tmp1);
      Nam.pushback(' pending...')
     End
     Else
     if tmp0='f' then
     begin
      NumParamCharge;
      Vau.pushback(Get(NumParamName.top,NumParam));
      NumParamBase.pop;
      NumParamName.pop;
      If ExitFlag Then
      Begin
       Assign('result',ResultList.top);
       ResultList.pop;
       Exit
      End
     end
     else
     If Tmp0='w' Then
     Begin
      NumParamCharge; tmpNam:=Nam.Top; Nam.Pop;
      if (tmpNam='')or(tmpNam[1]=' ') then Begin Reg(4); Exit End;//Assignment Error
      Assign(tmpNam,NumParam); NumParamBase.Pop; NumParamName.Pop;
     End
     Else
     Begin
      tmp2:=Vau.top; Vau.pop; Nam.pop;
      tmpo:=Vau.top; tmp1:=tmpo; Vau.pop; tmpnam:=Nam.top; Nam.pop;
      case tmp0 of
       '*':Vau.pushback(tmp1*tmp2);
       '/':Vau.pushback(tmp1/tmp2);
       'd':Vau.pushback(tmp1 div tmp2);
       'm':Vau.pushback(tmp1 mod tmp2);
       '&':Vau.pushback(tmp1 and tmp2);
       'r':Vau.pushback(tmp1>>tmp2);
       'l':Vau.pushback(tmp1<<tmp2);
       '+':Vau.pushback(tmp1+tmp2);
       '-':Vau.pushback(tmp1-tmp2);
       '|':Vau.pushback(tmp1  or tmp2);
       '^':Vau.pushback(tmp1 xor tmp2);
       '=':Vau.pushback(tmp1=tmp2);
       'b':Vau.pushback(tmp1<>tmp2);
       '<':Vau.pushback(tmp1<tmp2);
       '>':Vau.pushback(tmp1>tmp2);
       'x':Vau.pushback(tmp1<=tmp2);
       'y':Vau.pushback(tmp1>=tmp2);
       ':':begin if (tmpNam='')or(tmpNam[1]=' ') then Begin Reg(4); Exit End;//Assignment Error
                 if Pos('.',tmpNam)>0 Then AssignObj(tmpNam,tmp2) Else
                 If Tmp2.Tp=PSObj Then Assign(TmpNam,Tmp2)
                 Else Assign(tmpNam,tmp2); Vau.PushBack(NewPSValue(Tmp2)) End
      end
     end;
     Nam.pushback(' pending...');
     If DangerTerminate Then Exit
    end
   end;

  begin
   GetSeek(s,i);
   Nam.clear;
   Vau.clear;
   Opt.clear;
   NumParamBase.Clear;
   NumParamName.Clear;
   repeat
    now:=GetAlphaSafe(s,i);
    if (now='begin')or
       (now='end')or
       (now='if')or
       (now='then')or
       (now='else')or
       (now='for')or
       (now='to')or
       (now='do')or
       (now='while')or
       (now='function')or
       (now='object') then break;  //Key Words Avoid
    now:=GetAlpha(s,i);
    if now<>'' then
    begin
     mean:=#0;
     case now of
      'not':mean:='~';
      'div':mean:='d';
      'mod':mean:='m';
      'and':mean:='&';
      'shl':mean:='l';
      'shr':mean:='r';
       'or':mean:='|';
      'xor':mean:='^';
      else begin
            if now[1]='.' then now:='0'+now;
            if ('0'<=now[1])and(now[1]<='9')or(now[1]in['$','&','%']) then
            begin
             if (pos('.',now)>0)or(pos('e',now)>0) then
                  begin Val(now,NumB); Vau.pushback(Extended(NumB)); Nam.pushback(' pending...') end
             else begin Val(now,NumA); Vau.pushback(PSValue(NumA));  Nam.pushback(' pending...') end
            end
            else
            begin
             d:=Pos('.',Now);
             If D>0 Then Begin
              ObjName:=Copy(Now,1,D-1);
              CarName:=Copy(Now,1+D,Length(Now));
              If (ObjName='')Or(CarName='') Then Begin Reg(5); Exit End;
              D:=Find(ObjName); If (D=-1)Or(vData.Items[d].tp<>PSObj) Then Begin Reg(5); Exit End;
              With pLocalObj(vData.Items[d].p)^ Do
              Begin D:=DataIndex.Find(CarName); If D=-1 Then Begin Reg(5); Exit End End
             End Else Begin
              d:=Find(Now);
              if d=-1 then Assign(now,PS0)
             End;
             if (d<>-1)and(vData.Items[d].tp in FuncSet) then begin Opt.pushback('f');
              NumParamBase.pushback(Vau.Size);
              NumParamName.pushback(Now) end
             else begin
              Nam.pushback(now);
              //TODO:Const Test
              Vau.PushBack(Get(Now))
             end
            end
           end
     end;
     if mean<>#0 then
     begin
      Flush(Pri[mean]);
      Opt.pushback(mean);
      LastAlpha:=False
     end
     Else
      LastAlpha:=True
    end
    else
    begin
     if GetBetaSafe(s,i)='//' then break;
     now:=GetBeta(s,i);
     if (now='')or(now=';') then break;
     If Now='}' Then Continue;
     If Now='{' Then Begin NumC:=GetGama(S,i,'}'); NoteFlag:=I>Length(S)+1; If NoteFlag Then Break; Continue End;
     if now=',' then Begin Flush(Pri['(']+1); LastAlpha:=False End else
     if now=#39 then
     begin
      NumC:='';
      repeat
       NumC:=NumC+GetGama(s,i,#39);
       if (i<=length(s))and(s[i]=#39) then begin NumC:=NumC+#39; inc(i) end
       else break
      until false;
      Vau.pushback(Ansistring(NumC)); Nam.pushback(' pending...');
      LastAlpha:=True
     end
     else
     begin
      case now of
       ':=':mean:=':';
       '<>':mean:='b';
       '<=':mean:='x';
       '>=':mean:='y';
       '>>':mean:='r';
       '<<':mean:='l';
       else mean:=now[1]
      end;
      If Not LastAlpha Then
      If Mean='+' Then Begin                    LastAlpha:=False; Continue End Else
      If Mean='-' Then Begin Opt.PushBack('_'); LastAlpha:=False; Continue End;
      if (mean<>'(')And(mean<>'[')And(mean<>':') then Flush(Pri[mean]);
      If Mean='[' Then Begin NumParamBase.pushback(Vau.Size);
                             if LastAlpha Then NumParamName.pushback(' def...')
                                          Else NumParamName.pushback(' array...') end;
      if (mean=')')or(mean=']') then Opt.pop
                                else Opt.pushback(mean);
      If mean=']' Then
      If GetBetaSafe(S,I)=':='
                  Then Begin Now:=GetBeta(S,I);
                             Mean:=':';
                             Opt.PushBack('w') End
                  Else Begin NumParamCharge;
                             If NumParamName.Top=' def...' Then
                             Begin
                              Tmp1:=Get(Nam.top,NumParam);
                              Nam.Pop; Vau.Pop;
                              Nam.PushBack(' pending...'); Vau.PushBack(Tmp1)
                             End
                             Else
                             Begin
                              obj:=Get('array');
                              New(arr); arr^:=NumParam.Clone(1,NumParam.Size);
                              obj.Outer:=Arr;
                              Nam.PushBack(' pending...'); Vau.PushBack(NewPSValue(PSObj,@obj))
                             End;
                             NumParamBase.Pop; NumParamName.Pop End;
      LastAlpha:=(mean=')')or(mean=']')
     end;
    end;
    If DangerTerminate Then Exit
   until ExitFlag;
   If ExitFlag Then Exit(PSNil);
   Flush(0);
   if Vau.isnil then GetDelta:=NewPSValue(PS0)
                else GetDelta:=NewPSValue(Vau.Items[Vau.Size])
  end;

  function GetDeltaSafe(const s:ansistring;var i:longint):ansistring;
  var
   Start,j:longint;
   now:ansistring;
  begin
   j:=i;
   GetSeek(s,i);
   Start:=i;
   repeat
    now:=LowerCase(GetAlphaSafe(s,i));
    if (now='begin')or
       (now='end')or
       (now='if')or
       (now='then')or
       (now='else')or
       (now='for')or
       (now='to')or
       (now='do')or
       (now='while')or
       (now='function')or
       (now='object') then break;  //Key Words Avoid
    now:=GetAlpha(s,i);
    if now='' then
    begin
     if GetBetaSafe(s,i)='//' then break;
     now:=GetBeta(s,i);
     if (now='')or(now=';') then break;
     if now=#39 then
     repeat
      GetGama(s,i,#39);
      if (i<=length(s))and(s[i]=#39) then inc(i) else break
     until false
    end
   until false;
   GetDeltaSafe:=copy(s,Start,i-Start);
   Bias:=i;
   i:=j
  end;

  function GetEpsilon(const s:ansistring;var i:longint):ansistring;
  var
   t:ansistring;
  begin
   t:=GetAlphaSafe(s,i);
   case LowerCase(t) of
    'begin','end','then','else':exit(GetAlpha(s,i));
    'if':begin i:=bias; t:=GetDeltaSafe(s,i); i:=Bias; exit('if '+t) end;
    'for':begin i:=Bias; GetEpsilon:='for ';
                t:=GetAlpha(s,i); if Find(t)=-1 then Add(t,PSInt); GetEpsilon:=GetEpsilon+t;
                if GetBeta(s,i)<>':=' then Begin Reg(6); Exit End; GetEpsilon:=GetEpsilon+':=';
                GetEpsilon:=GetEpsilon+GetDeltaSafe(s,i); i:=Bias;
                if LowerCase(GetAlpha(s,i))<>'to' then Begin Reg(7); Exit End; GetEpsilon:=GetEpsilon+' to ';
                GetEpsilon:=GetEpsilon+GetDeltaSafe(s,i); i:=Bias;
                if LowerCase(GetAlpha(s,i))<>'do' then Begin Reg(8); Exit End; GetEpsilon:=GetEpsilon+' do' end;
    'while':begin i:=Bias; GetEpsilon:='while ';
                  GetEpsilon:=GetEpsilon+GetDeltaSafe(s,i); i:=Bias;
                  if LowerCase(GetAlpha(s,i))<>'do' then Begin Reg(9); Exit End; GetEpsilon:=GetEpsilon+' do' end;
    'function':Begin i:=Bias; GetEpsilon:='function ';
                     t:=GetAlpha(s,i); if t='' then Begin Reg(10); Exit End; GetEpsilon:=GetEpsilon+t;
                     if GetBeta(s,i)<>'(' Then Begin Reg(11); Exit End; GetEpsilon:=GetEpsilon+'(';
                     While GetBeta(s,i)<>')' Do Begin t:=GetAlpha(s,i); if t='' Then Begin Reg(12); Exit End; GetEpsilon:=GetEpsilon+t;
                     Case GetBetaSafe(s,i) Of ',':GetEpsilon:=GetEpsilon+','; ')':; Else Begin Reg(13); Exit End End; End;
                     GetEpsilon:=GetEpsilon+')' End;
    'object':Begin i:=Bias; GetEpsilon:='object ';
                   t:=GetAlpha(s,i); if t='' then Begin Reg(14); Exit ENd; GetEpsilon:=GetEpsilon+t;
                   if GetBeta(s,i)<>'(' Then Begin Reg(15); Exit ENd; GetEpsilon:=GetEpsilon+'(';
                   While GetBeta(s,i)<>')' Do Begin t:=GetAlpha(s,i); if t='' Then Begin Reg(16); Exit End; GetEpsilon:=GetEpsilon+t;
                   Case GetBetaSafe(s,i) Of ',':GetEpsilon:=GetEpsilon+','; ')':; Else Begin Reg(17); Exit End End; End;
                   GetEpsilon:=GetEpsilon+')' End;
    else begin GetEpsilon:=' '+GetDeltaSafe(s,i); i:=Bias end
   end
  end;

  function GetAlpha:ansistring;begin exit(GetAlpha(s,i)) end;
  function GetAlphaSafe:ansistring;begin exit(GetAlphaSafe(s,i)) end;
  function GetBeta:ansistring;begin exit(GetBeta(s,i)) end;
  function GetBetaSafe:ansistring;begin exit(GetBetaSafe(s,i)) end;
  function GetDelta:PSValue;begin exit(GetDelta(s,i)) end;
  function GetDeltaSafe:ansistring;begin exit(GetDeltaSafe(s,i)) end;
  function GetDelta(const s:ansistring):PSValue;var i:longint=1;begin exit(GetDelta(s,i)) end;
  function GetEpsilon:ansistring;begin exit(GetEpsilon(os,i)) end;


  procedure Release;
  var ii:longint;
  begin
   case ScriptDirector of
    '1':if Patt and 2<>0 then case Exec(Script) of 1:Exec:=1; 2:Exec:=2; 3:Exec:=3 end;
    '0':if Patt and 4<>0 then case Exec(Script) of 1:Exec:=1; 2:Exec:=2; 3:Exec:=3 end;
    else if Patt and 8<>0 then for ii:=ForBegin to ForEnd do begin Assign(ScriptDirector,ii); case Exec(Script) of 1:continue; 2:break; 3:Break; end end
    else if Patt and 16<>0 then while Longint(GetDelta(ScriptDirector))<>0 do case Exec(Script) of 1:continue; 2:break; 3:Break end
    else If Patt And 32<>0 Then AddFunc(ScriptDirector,Carry,Script)
    Else case Exec(Script) of 1:Exec:=1; 2:Exec:=2; 3:Exec:=3 end
   end;
   Patt:=0;
  end;

  function Analysis:longint;
  var
   t:ansistring;
   d:longint;

   Procedure FirstOfAll;
   Begin
    If Patt=0 Then
    Begin
     Script.Clear;
     Carry.Clear;
     LazNet.Clear;
     LazNet.PushBack(1)
    End
   End;

  begin
   t:=GetAlphaSafe;
   case t of
     'continue':exit(1);
     'break':exit(2);
     'begin','end':i:=Bias;
       'if':begin
             i:=Bias;
             if Longint(GetDelta)<>0 then ScriptDirector:='1'
                                     else ScriptDirector:='0';
            end;
     'then':begin
             FirstOfAll;
             i:=Bias;
             Patt:=(Patt or 6)xor 4;
            end;
     'else':begin
             FirstOfAll;
             i:=Bias;
             Patt:=(Patt or 6)xor 2
            end;
      'for':begin
             FirstOfAll;
             i:=Bias;
             t:=GetAlpha;
             d:=Find(t);
             if d=-1 then Add(t,PSInt);
             if GetBeta<>':=' then Begin Reg(6); Patt:=0; Exit End;
             ForBegin:=Longint(GetDelta);
             if GetAlpha<>'to' then Begin Reg(7); Patt:=0; Exit End;
             ForEnd:=Longint(GetDelta);
             ScriptDirector:=t;
             if GetAlpha<>'do' then Begin Reg(8); Patt:=0; Exit End;
             Patt:=Patt or 8
            end;
    'while':begin
             FirstOfAll;
             i:=Bias;
             Patt:=Patt or 16;
             ScriptDirector:=GetDeltaSafe;
             i:=Bias;
             if GetAlpha<>'do' then Begin Reg(9); Patt:=0; Exit End;
            end;
    'function':Begin
             FirstOfAll;
             i:=Bias;
             ScriptDirector:=GetAlpha; If ScriptDirector='' Then Begin Reg(10); Patt:=0; Exit End;
             If GetBeta<>'(' Then Begin Reg(11); Patt:=0; Exit End;
             While GetBeta<>')' Do
             Begin
              t:=GetAlpha;
              If t='' Then Begin Reg(12); Patt:=0; Exit End;
              Case GetBetaSafe Of
               ')',',':;
               Else Begin Reg(13); Patt:=0; Exit End
              End;
              Carry.PushBack(T)
             End;
             Patt:=Patt Or 32
            End;
     'object':Begin
             FirstOfAll;
             i:=Bias;
             ScriptDirector:=GetAlpha; If ScriptDirector='' Then Begin Reg(14); Patt:=0; Exit End;
             If GetBeta<>'(' Then Begin Reg(15); Patt:=0; Exit End;
             While GetBeta<>')' Do
             Begin
              t:=GetAlpha;
              If t='' Then Begin Reg(16); Patt:=0; Exit End;
              Case GetBetaSafe Of
               ')',',':;
               Else Begin Reg(17); Patt:=0; Exit End
              End;
              Carry.PushBack(T)
             End;
             Assign(ScriptDirector,NewLocalObj(ScriptDirector,Carry))
            End;
    else GetDelta
   end;
   If ExitFlag Then Exit(3);
   exit(0)
  end;


  procedure GetLazy;
  var
   ot,t:ansistring;
  begin
   LazTag:=1;
   ot:=GetEpsilon;
   t:=lowercase(ot);
   if t[length(t)]=';' then begin LazTag:=0; if LazNet.top>0 then dec(LazNet.Items[LazNet.Size]) end;
   if t='begin' then begin inc(nest); LazNet.pushback(1) end;
   if t='end' then begin dec(nest); LazNet.pop end;
   if (t='else')or
      (copy(t,1,2)='if')or
      (copy(t,1,3)='for')or
      (copy(t,1,5)='while')or
      (copy(t,1,8)='function') then inc(LazNet.Items[LazNet.Size]);
   Script.pushback(ot);
   if (nest=0)and(LazTag=0)and(LazNet.Size=1)and(LazNet.Items[1]=0) then Release
  end;

 begin
  If DangerTerminate Then Exit;
  PublicPSLib:=@Self;
  If ExitFlag Then Exit(3);
  Exec:=0;
  If NoteFlag Then
  Begin
   i:=Pos('}',Os);
   If i>0 Then Begin Delete(OS,1,i+1); NoteFlag:=False End
   Else Exit
  End;
  s:=lowercase(os);
  i:=1;
  while (i<=length(s))and(GetBetaSafe<>'//') do
  If DangerTerminate Then Exit Else
  if Patt=0 then case Analysis of 1:exit(1); 2:exit(2); 3:exit(3) end
            else begin GetLazy; if Exec<>0 then exit end
 end;


  Function __read(Const a:VList):PSValue;
  Var
   i:Longint;
   _longint:Longint;
   _extended:Extended;
   _char:Char;
   _ansistring:Ansistring;
  Begin
   For i:=1 to a.Size Do
   Case a.Items[i].Tp Of
    PSInt:Begin Read(_Longint); pint(a.Items[i].p)^:=_Longint End;
    PSDouble:Begin Read(_Extended); pdouble(a.Items[i].p)^:=_Extended End;
    PSChar:Begin Read(_Char); pChar(a.Items[i].p)^:=_Char End;
    PSStr:Begin Read(_Ansistring); pAnsistring(a.Items[i].p)^:=_Ansistring End;
    Else Begin PublicPSLib^.Reg(18); Exit End //Read Error
   End;
   Exit(PS1)
  End;

  Function __readln(Const a:VList):PSValue;
  Begin
   __read(a);
   readln
  End;

  function __write(const a:VList):PSValue;
  var i:longint;
  begin
   if a.Size>0 then write(Variant(a.Items[1]));
   for i:=2 to a.Size do write(Variant(a.Items[i]));
   exit(PS0)
  end;

  function __writeln(const a:VList):PSValue;
  begin __write(a); writeln; exit(PS0) end;

  function __max(const a:VList):PSValue;
  var i:longint; begin __max:=a.Items[1]; for i:=2 to a.Size do if Longint(a.Items[i]>__max)=1 then __max:=a.Items[i] end;

  function __min(const a:VList):PSValue;
  var i:longint; begin __min:=a.Items[1]; for i:=2 to a.Size do if Longint(a.Items[i]<__min)=1 then __min:=a.Items[i] end;

  function __odd(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(a.Items[1] and PS1) end;

  function __abs(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        If a.Items[1].Tp=PSInt    Then Exit(Abs(Longint(a.Items[1])));
        If a.Items[1].Tp=PSDouble Then Exit(Abs(Extended(a.Items[1])));
        PublicPSLib^.Reg(19) end;

  function __sqr(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        If a.Items[1].Tp=PSInt    Then exit(sqr(Longint(a.Items[1])));
        If a.Items[1].Tp=PSDouble Then exit(sqr(Extended(a.Items[1])));
        PublicPSLib^.Reg(20) end;

  function __sqrt(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        If a.Items[1].tp in CalcSet Then exit(Extended(sqrt(PSValue(a.Items[1]))));
        PublicPSLib^.Reg(21) end;

  function __round(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        If a.Items[1].Tp in CalcSet Then exit(round(Extended(a.Items[1])));
        PublicPSLib^.Reg(22) end;

  function __trunc(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        If a.Items[1].Tp in CalcSet Then exit(trunc(Extended(a.Items[1])));
        PublicPSLib^.Reg(23) end;

  function __copy(const a:VList):PSValue;
  begin If a.Size<>3 Then Begin PublicPSLib^.Reg(3); Exit End;
        If (a.Items[1].Tp in StrSet)And(a.Items[2].Tp=PSInt)And(a.Items[3].Tp=PSInt) Then
        exit(copy(Ansistring(a.Items[1]),Longint(a.Items[2]),Longint(a.Items[3])));
        PublicPSLib^.Reg(24) end;

  function __pos(const a:VList):PSValue;
  begin If a.Size<>2 Then Begin PublicPSLib^.Reg(3); Exit End;
        If (a.Items[1].Tp In StrSet)And(a.Items[2].Tp IN StrSet) Then
        exit(pos(Ansistring(a.Items[1]),Ansistring(a.Items[2])));
        PublicPSLib^.Reg(25) end;

  function __upcase(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(upcase(Ansistring(a.Items[1]))) end;

  function __lowercase(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(lowercase(Ansistring(a.Items[1]))) end;

  function __Str(const a:VList):PSValue;
  Begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        Exit(IntToStr(Longint(A.Items[1]))) End;

  function __Val(const a:VList):PSValue;
  Begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        Exit(StrToInt(Ansistring(A.Items[1]))) End;

  function __sin(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(sin(Extended(a.Items[1]))) end;

  function __cos(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(cos(Extended(a.Items[1]))) end;

  function __exp(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(exp(Extended(a.Items[1]))) end;

  function __ln(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(ln(Extended(a.Items[1]))) end;

  function __random(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(PSValue(random(Longint(a.Items[1])))) end;

  function __length(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(length(Ansistring(a.Items[1]))) end;

  function __chr(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(chr(Longint(a.Items[1]))) end;

  function __ord(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(ord(Ansistring(a.Items[1])[1])) end;

  function __pred(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(pred(Ansistring(a.Items[1])[1])) end;

  function __succ(const a:VList):PSValue;
  begin If a.Size<1 Then Begin PublicPSLib^.Reg(3); Exit End;
        exit(succ(Ansistring(a.Items[1])[1])) end;

  function __exit(Const a:VList):PSValue;
  Var X:Longint;
  Begin If a.Size=0 Then X:=0 Else
        If a.Size<>1 Then Begin PublicPSLib^.Reg(3); Exit End; X:=a.Items[1];
        ResultList.PushBack(X); ExitFlag:=True; Exit(X) End;

  function __halt(const a:VList):PSValue;
  Begin If a.Size=0 Then Halt; Halt(a.Items[1]) End;

  function __fileopen(Const a:VList):PSValue;
  var filename:Ansistring;
  Begin
   If a.Size<2 Then Exit(PS0);
   filename:=Ansistring(a.Items[1]);
   Case Ansistring(A.Items[2]) Of
    'r':Begin If Not FileExists(Filename) Then Exit(PS0);
              Assign(PSStdin,Filename); Reset(PSStdIn) End;
    'w':Begin Assign(PSStdOut,Filename); Rewrite(PSStdOut) End;
    Else Exit(PS0)
   End;
   Exit(PS1)
  End;

  function __fileclose(Const a:VList):PSValue;
  Var i:Longint;
  Begin
   If a.Size<1 Then Exit(PS0);
   For i:=1 to A.Size Do
   Case Ansistring(A.Items[i]) Of
    'r':Begin Close(PSStdIn);  Assign(PSStdIn,'');  Reset(PSStdIn) End;
    'w':Begin Close(PSStdOut); Assign(PSStdOut,''); Rewrite(PSStdOut) End;
   End;
   Exit(PS1)
  End;

  Function __readf(Const a:VList):PSValue;
  Var
   i:Longint;
   _longint:Longint;
   _extended:Extended;
   _char:Char;
   _ansistring:Ansistring;
  Begin
   For i:=1 to a.Size Do
   Case a.Items[i].Tp Of
    PSInt:Begin Read(PSStdIn,_Longint); pint(a.Items[i].p)^:=_Longint End;
    PSDouble:Begin Read(PSStdIn,_Extended); pdouble(a.Items[i].p)^:=_Extended End;
    PSChar:Begin Read(PSStdIn,_Char); pChar(a.Items[i].p)^:=_Char End;
    PSStr:Begin Read(PSStdIn,_Ansistring); pAnsistring(a.Items[i].p)^:=_Ansistring End;
    Else Begin PublicPSLib^.Reg(18); Exit End;//Read Error
   End;
   Exit(PS1)
  End;

  Function __readlnf(Const a:VList):PSValue;
  Begin
   __readf(a);
   readln(PSStdIn);
   Exit(PS1)
  End;

  function __writef(const a:VList):PSValue;
  var i:longint;
  begin
   if a.Size>0 then write(PSStdOut,Variant(a.Items[1]));
   for i:=2 to a.Size do write(PSStdOut,Variant(a.Items[i]));
   exit(PS0)
  end;

  function __writelnf(const a:VList):PSValue;
  begin __writef(a); writeln(PSStdOut); exit(PS0) end;

  function __array_create(Const A:Vlist):PSValue;
  Var Tmp:pVList; Begin New(Tmp); Tmp^.Clear; PublicObjOuter^:=Tmp; Exit(PS1) End;

  function __array_clear(Const A:VList):PSValue;
  Begin pVList(PublicObjOuter^)^.Clear; Exit(PS1) End;

  function __array_Size(Const A:VList):PSValue;
  Begin Exit(pVList(PublicObjOuter^)^.Size) End;

  function __array_pushback(Const A:VList):PSValue;
  Var i:Longint; tmp:pVList;
  Begin tmp:=PubLicObjOuter^;
        For i:=1 to A.Size Do tmp^.PushBack(A.Items[i]);
        Exit(PS1) End;

  Function __array_top(Const A:VList):PSValue;
  Begin Exit(pVList(PublicObjOuter^)^.Top) End;

  function __array_pop(Const A:VList):PSValue;
  Begin pVList(PublicObjOuter^)^.Pop; Exit(PS1) End;

  Function __array_insert(Const A:VList):PSValue;
  Begin If (A.Size<1)Or(A.Items[1].Tp<>PSInt) Then Exit(PS0);
        pVList(PublicObjOuter^)^.Insert(A.Items[1],A.Clone(2,A.Size)); Exit(PS1) End;

  Function __array_delete(Const A:VList):PSValue;
  Begin if (A.Size<2)Or(A.Items[1].Tp<>PSInt)And(A.Items[2].Tp<>PSInt) Then Exit(PS0);
        pVList(PublicObjOuter^)^.Delete(a.Items[1],a.Items[2]); Exit(PS1) End;

  Function __array_reverse(Const A:VList):PSValue;
  Begin if (A.Size<2)Or(A.Items[1].Tp<>PSInt)And(A.Items[2].Tp<>PSInt) Then Exit(PS0);
        pVList(PublicObjOuter^)^.Reverse(a.Items[1],a.Items[2]); Exit(PS1) End;

  Function __array_clone(Const A:VList):PSValue;
  Var Tmp:pVList;
  Begin if (A.Size<2)Or(A.Items[1].Tp<>PSInt)And(A.Items[2].Tp<>PSInt) Then Exit(PS0);
        Result:=PublicPSLib^.Get('array');
        New(Tmp); Tmp^:=pVList(PublicObjOuter^)^.Clone(a.Items[1],a.Items[2]);
        pLocalObj(Result.p)^.Outer:=Tmp; Exit(PS1) End;

  Function __array_Items(Const A:VList):PSValue;
  Begin If (A.Size<1)Or(A.Items[1].Tp<>PSInt) Then Exit(PSNil);
        Exit(pVList(PublicObjOuter^)^.Items[Longint(a.Items[1])]) End;

  Function __Array_ItemsA(Const A:VList):PSValue;
  Begin If (A.Size<2)Or(A.Items[1].Tp<>PSInt) Then Exit(PSNil);
        pVList(PublicObjOuter^)^.Items[Longint(a.Items[1])]:=NewPSValue(a.Items[2]);
        Exit(PS1) End;


 procedure PSLib.UsesSystem;
 begin
  Assign('read',@__read);
  Assign('readln',@__readln);
  Assign('write',@__write);
  Assign('writeln',@__writeln);
  Assign('max',@__max);
  Assign('min',@__min);
  Assign('odd',@__odd);
  Assign('abs',@__abs);
  Assign('sqr',@__sqr);
  Assign('sqrt',@__sqrt);
  Assign('round',@__round);
  Assign('trunc',@__trunc);
  Assign('copy',@__copy);
  Assign('pos',@__pos);
  Assign('upcase',@__upcase);
  Assign('lowercase',@__lowercase);
  Assign('str',@__str);
  Assign('val',@__val);
  Assign('sin',@__sin);
  Assign('cos',@__cos);
  Assign('exp',@__exp);
  Assign('ln',@__ln);
  Assign('random',@__random);
  Assign('length',@__length);
  Assign('chr',@__chr);
  Assign('ord',@__ord);
  Assign('pred',@__pred);
  Assign('succ',@__succ);
  Assign('exit',@__exit);
  Assign('halt',@__halt);

  Assign('fileopen',@__fileopen);
  Assign('fileclose',@__fileclose);
  Assign('readf',@__readf);
  Assign('readlnf',@__readlnf);
  Assign('writef',@__writef);
  Assign('writelnf',@__writelnf);

  Assign('array',NewLocalObj('array','items','itemsa'));
  AssignObj('array.create',@__array_create);
  AssignObj('array.clear',@__array_clear);
  AssignObj('array.size',@__array_size);
  AssignObj('array.pushback',@__array_pushback);
  AssignObj('array.top',@__array_top);
  AssignObj('array.pop',@__array_pop);
  AssignObj('array.insert',@__array_insert);
  AssignObj('array.delete',@__array_delete);
  AssignObj('array.reverse',@__array_reverse);
  AssignObj('array.items',@__array_Items);
  AssignObj('array.itemsa',@__array_ItemsA);



 end;


begin
 randomize;

 fillchar(Pri,sizeof(Pri),$ff);
 Pri['[']:=10;
 Pri['(']:=10;
 Pri[']']:=15;
 Pri[')']:=15;
 Pri[':']:=20; //:=
 Pri['w']:=20; //[]:=
 Pri['=']:=30;
 Pri['b']:=30; //<>
 Pri['<']:=30;
 Pri['>']:=30;
 Pri['x']:=30; //<=
 Pri['y']:=30; //>=
 Pri['|']:=40; //or
 Pri['^']:=40; //xor
 Pri['+']:=50;
 Pri['-']:=50;
 Pri['&']:=60; //and
 Pri['*']:=70;
 Pri['/']:=70;
 Pri['d']:=70; //div
 Pri['m']:=70; //mod
 Pri['r']:=70; //shr
 Pri['l']:=70; //shl
 Pri['f']:=80; //function
 Pri['~']:=90;  //not
 Pri['_']:=90;  //-

 acceptset:=['A'..'Z','a'..'z','0'..'9','_','$','&','%','.'];
   pattset:=['(',')','[',']',#39,',',';','{','}'];
    runset:=['!'..'/',':'..'@','['..'`','{','}']-['$','&','%','.'];

 PS0:=PSValue(0);
 PS1:=PSValue(1);
 PSNil:=NewPSValue(PSNone,Nil);

 PSStdIn:=Input;
 PSStdOut:=Output;

end.