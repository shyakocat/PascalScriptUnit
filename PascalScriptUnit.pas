{$mode objfpc}
unit PascalScriptUnit;

interface

uses SysUtils,Classes,Variants;

var
 Pri:array[char(0)..char(255)]of byte;
 acceptset,runset,pattset:set of char;

type
 generic List<T>=object
  Size:longint;
  Items:array of T;
  procedure swap(var a,b:T);
  procedure resize(n:longint);
  procedure clear;
  procedure pushback(const value:T);
  procedure pop;
  procedure insert(p:longint;const value:T);
  procedure insert(p:longint;const L:List);
  procedure delete(p,Len:longint);
  procedure reverse(l,r:longint);
  procedure fill(l,r:longint;const x:T);
  function clone(l,r:longint):List;
  function top:T;
  function isnil:boolean;
 end;

 VList=specialize List<Variant>;
 SList=specialize List<Ansistring>;
 TFunc=function(const Param:VList):Variant;

 Arr=array of longint;
 pint=^longint;
 pansistring=^ansistring;
 vtypemode=(int,double,psstr,func,intarr,no);

 HashStr=object
  const E:int64=257;
  var Size,P:longint;
   head,next,hsid:specialize List<longint>;
   node:specialize List<Ansistring>;
  function hashcode(const s:ansistring):longint;
  procedure clear;
  procedure Add(const s:ansistring;id:longint);
  function Find(const s:ansistring):longint;
 end;

 PSLib=object
   size:longint;
   vname:HashStr;
   vtype   :specialize List<vtypemode>;
   vindex  :specialize List<longint>;
   v_int   :specialize List<longint>;
   v_double:specialize List<real>;
   v_psstr :specialize List<ansistring>;
   v_func  :specialize List<TFunc>;
   v_intarr:specialize List<pint>;
   LazNet  :specialize List<longint>;
   Patt,Nest,Args,LazTag:longint;
   Script:SList;
   ScriptDirector:ansistring;
   ArrayManage:specialize List<Arr>;
   function Find(const _name:string):longint;
   procedure Add(const _name:string;_type:vtypemode);
   procedure Assign(const _name:string;const _value:longint);
   procedure Assign(const _name:string;const _value:real);
   procedure Assign(const _name:string;const _value:ansistring);
   procedure Assign(const _name:string;const _value:TFunc);
   procedure Assign(const _name:string;const _value:pint);
   procedure Assign(const _name:string;_index,_value:longint);
   function Get(const _name:string):Variant;
   function Get(const _name:string;_index:longint):Variant;
   function Get(const _name:string;const _param:VList):Variant;
   function GetType(const _name:string):vtypemode;
   function Exec(os:ansistring):longint;
   function Exec(var ss:SList):longint;
   procedure UsesSystem;
   procedure clear;
   function newpint(x:longint):pint;
 end;


implementation

var
 debug:pint;

 procedure swap(var a,b:Variant);
 var c:Variant; begin c:=a; a:=b; b:=c end;

 function TransValue(const a:Variant):Variant;
 var _p:pint;
 begin
  if vartype(a)=varLongWord then
  begin
   _p:=pint(Dword(a));
   exit(_p^)
  end;
  exit(a);
 end;

 function List.isnil:boolean;
 begin
  exit(Size=0)
 end;

 procedure List.clear;
 begin
  Size:=0;
  SetLength(Items,0)
 end;

 procedure List.resize(n:longint);
 begin
  Size:=n;
  if (Size<10)and(high(Items)<10) then setlength(Items,10) else
  if Size>=high(Items) then setlength(Items,Size<<1) else
  if Size<high(Items)>>2 then setlength(Items,Size>>1)
 end;

 procedure List.pushback(const value:T);
 begin
  Resize(size+1);
  Items[Size]:=value
 end;

 procedure List.pop;
 begin
  if Size>0 then dec(Size);
  Resize(Size)
 end;

 function List.top:T;
 begin
  exit(Items[size])
 end;

 procedure List.swap(var a,b:T);
 var c:T; begin c:=a; a:=b; b:=c end;

 procedure List.Reverse(l,r:longint);
 var i:longint;
 begin
  for i:=l to (l+r)>>1 do swap(Items[i],Items[l+r-i])
 end;

 function List.Clone(l,r:longint):List;
 var i:longint;
 begin
  Clone.Clear;
  if l>r then exit;
  for i:=l to r do Clone.pushback(Items[i])
 end;

 procedure List.insert(p:longint;const value:T);
 var i:longint;
 begin
  if p>Size then exit;
  Resize(Size+1);
  for i:=Size downto p+1 do Items[i]:=Items[i-1];
  Items[p]:=value
 end;

 procedure List.insert(p:longint;const L:List);
 var i:longint;
 begin
  if p>Size then exit;
  Resize(Size+L.Size);
  for i:=Size downto p+L.Size do Items[i]:=Items[i-L.Size];
  for i:=1 to L.Size do Items[p-1+i]:=L.Items[i]
 end;

 procedure List.delete(p,Len:longint);
 var i:longint;
 begin
  if p>Size then exit;
  if p-1+Len>=Size then begin Resize(p-1); exit end;
  for i:=p+Len to Size do Items[i-Len]:=Items[i];
  Resize(Size-Len)
 end;

 procedure List.fill(l,r:longint;const x:T);
 var i:longint;
 begin
  for i:=l to r do Items[i]:=x
 end;

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

 function PSLib.newpint(x:longint):pint;
 var a:Arr;
 begin
  setlength(a,x);
  fillchar(a[0],x<<2,0);
  ArrayManage.pushback(A);
  exit(@a[0])
 end;

 procedure PSLib.Add(const _name:string;_type:vtypemode);
 begin
  inc(Size);
  vname.Add(_name,Size);
  vtype.pushback(_type);
  case _type of
   int   :begin v_int   .pushback(0);   vindex.pushback(v_int.Size) end;
   double:begin v_double.pushback(0);   vindex.pushback(v_double.Size) end;
   psstr :begin v_psstr .pushback('');  vindex.pushback(v_psstr.Size) end;
   func  :begin v_func  .pushback(nil); vindex.pushback(v_func.Size) end;
   intarr:begin v_intarr.pushback(nil); vindex.pushback(v_intarr.Size) end;
  end
 end;

 procedure PSLib.Clear;
 begin
  Size:=0;
  vname   .clear;
  vtype   .clear;
  vindex  .clear;
  v_int   .clear;
  v_double.clear;
  v_psstr .clear;
  v_func  .clear;
  v_intarr.clear;
  LazNet  .clear;
  Patt:=0;
  Nest:=0;
  ArrayManage.Clear;
 end;

 function PSLib.Find(const _name:string):longint;
 var i:longint;
 begin
  exit(vname.Find(_name))
 end;

 procedure PSLib.Assign(const _name:string;const _value:longint);
 var i:longint;
 begin
  i:=Find(_name); if i=-1 then begin Add(_name,int); i:=Size end;
  if vtype.Items[vindex.Items[i]]<>int then begin vtype.Items[i]:=int; v_int.pushback(_value); vindex.Items[i]:=v_int.Size end
  else v_int.Items[vindex.Items[i]]:=_value
 end;

 procedure PSLib.Assign(const _name:string;const _value:real);
 var i:longint;
 begin
  i:=Find(_name); if i=-1 then begin Add(_name,double); i:=Size end;
  if vtype.Items[vindex.Items[i]]<>double then begin vtype.Items[i]:=double; v_double.pushback(_value); vindex.Items[i]:=v_double.Size end
  else v_double.Items[vindex.Items[i]]:=_value
 end;

 procedure PSLib.Assign(const _name:string;const _value:ansistring);
 var i:longint;
 begin
  i:=Find(_name); if i=-1 then begin Add(_name,psstr); i:=Size end;
  if vtype.Items[vindex.Items[i]]<>psstr then begin vtype.Items[i]:=psstr; v_psstr.pushback(_value); vindex.Items[i]:=v_psstr.Size end
  else v_psstr.Items[vindex.Items[i]]:=_value
 end;

 procedure PSLib.Assign(const _name:string;const _value:TFunc);
 var i:longint;
 begin
  i:=Find(_name); if i=-1 then begin Add(_name,func); i:=Size end;
  if vtype.Items[vindex.Items[i]]<>func then begin vtype.Items[i]:=func; v_func.pushback(_value); vindex.Items[i]:=v_func.Size end
  else v_func.Items[vindex.Items[i]]:=_value
 end;

 procedure PSLib.Assign(const _name:string;const _value:pint);
 var i:longint;
 begin
  i:=Find(_name); if i=-1 then begin Add(_name,intarr); i:=Size end;
  if vtype.Items[vindex.Items[i]]<>intarr then begin vtype.Items[i]:=intarr; v_intarr.pushback(_value); vindex.Items[i]:=v_intarr.Size end
  else v_intarr.Items[vindex.Items[i]]:=_value
 end;

 procedure PSLib.Assign(const _name:string;_index,_value:longint);
 var i,j:longint;
 begin
  i:=Find(_name); if i=-1 then halt(1001); j:=vindex.Items[i];
  if vtype.Items[i]<>intarr then halt(1002);
  v_intarr.Items[j][_index]:=_value
 end;

 function PSLib.Get(const _name:string):Variant;
 var i,j:longint; t:Variant;
 begin
  i:=Find(_name); if i=-1 then halt(1001); j:=vindex.Items[i];
  case vtype.Items[i] of
      int:t:=v_int   .Items[j];
   double:t:=v_double.Items[j];
    psstr:t:=v_psstr .Items[j];
   intarr:t:=v_intarr.Items[j][0]
  end;
  exit(t)
 end;

 function PSLib.Get(const _name:string;_index:longint):Variant;
 var i,j:longint;
 begin
  i:=Find(_name); if i=-1 then halt(1001); j:=vindex.Items[i];
  if vtype.Items[i]=intarr then exit(v_intarr.Items[j,_index]);
  if vtype.Items[i]=psstr then exit(ansistring(v_psstr.Items[j,_index]));
  halt(1002)
 end;

 function PSLib.Get(const _name:string;const _param:VList):Variant;
 var i,j:longint;
 begin
  i:=Find(_name); if i=-1 then halt(1001); j:=vindex.Items[i];
  if vtype.Items[i]<>func then halt(1002);
  exit(v_func.Items[j](_param))
 end;

 function PSLib.GetType(const _name:string):vtypemode;
 var i:longint;
 begin
  i:=Find(_name); if i=-1 then exit(no);
  exit(vtype.Items[i])
 end;

 function PSLib.Exec(var ss:SList):longint;
 var
  i,_Args:longint;
  _Patt,_Nest:longint;
  _LNet:specialize List<longint>;
  _Script:SList;
  _ScriptD:ansistring;
 begin
  Exec:=0;
  if ss.Size<1 then exit;
  _Patt:=Patt;
  _Nest:=Nest;
  _Args:=Args;
  _LNet:=LazNet.clone(1,LazNet.Size);
  _Script:=SS.clone(1,SS.Size);
  _ScriptD:=ScriptDirector;
  Patt:=0;
  Nest:=0;
  LazNet.Clear;
  Script.Clear;
  ScriptDirector:='';
  for i:=1 to _Script.Size do
   case Exec(_Script.Items[i]) of
    1:begin Exec:=1; break end;
    2:begin Exec:=2; break end;
   end;
  Patt:=_Patt;
  Nest:=_Nest;
  Args:=_Args;
  LazNet:=_LNet;
  SS:=_Script;
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

  function GetDelta(const s:ansistring;var i:longint):Variant;
  var
   pas:boolean=true;
   _p:pint;
   _ps:pansistring;
   tmp0:char;
   tmp1,tmp2,tmpo:Variant;
   Nam:specialize List<ansistring>;
   Vau:specialize List<Variant>;
   Opt:specialize List<char>;
   now:ansistring;
   mean:char;
   NumA:longint;
   NumB:real;
   NumC:ansistring;
   NumD:Variant;
   NumParam:VList;
   NumParamBase:specialize List<longint>;
   NumParamName:specialize List<ansistring>;
   d:longint;

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
      tmp1:=TransValue(Vau.top); Vau.pop; Nam.pop;
      Vau.pushback(longint(ord(tmp1=0)));
      Nam.pushback(' pending...')
     end
     else
     if tmp0='f' then
     begin
      NumParam.Clear;
      while Vau.Size>NumParamBase.top do
      begin
       NumParam.pushback(TransValue(Vau.top));
       Vau.pop; Nam.pop
      end;
      NumParam.Reverse(1,NumParam.Size);
      Vau.pushback(Get(NumParamName.top,NumParam));
      NumParamBase.pop;
      NumParamName.pop
     end
     else
     begin
      tmp2:=TransValue(Vau.top); Vau.pop; Nam.pop;
      tmpo:=Vau.top; tmp1:=TransValue(tmpo); Vau.pop; tmpnam:=Nam.top; Nam.pop;
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
       '=':Vau.pushback(longint((vartype(tmp1)=vartype(tmp2))and(tmp1=tmp2)));
       'b':Vau.pushback(longint((vartype(tmp1)<>vartype(tmp2))or(tmp1<>tmp2)));
       '<':Vau.pushback(longint((vartype(tmp1)=vartype(tmp2))and(tmp1<tmp2)));
       '>':Vau.pushback(longint((vartype(tmp1)=vartype(tmp2))and(tmp1>tmp2)));
       'x':Vau.pushback(longint((vartype(tmp1)=vartype(tmp2))and(tmp1<=tmp2)));
       'y':Vau.pushback(longint((vartype(tmp1)=vartype(tmp2))and(tmp1>=tmp2)));
       ':':begin if tmpNam=' pending...' then halt(1100);
                 if vartype(tmpo)=varLongWord then
                 begin
                  if tmpNam=' system.pint' then
                   if vartype(tmp2)<>varInteger then halt(1102)
                   else begin _p:=pint(Dword(tmpo)); _p^:=tmp2 end else
                  if copy(tmpNam,1,13)=' system.pchar' then
                   if vartype(tmp2)<>varString then halt(1103)
                   else begin _ps:=pansistring(Dword(tmpo)); _ps^[StrToInt(copy(tmpNam,15,length(tmpNam)))]:=ansistring(tmp2)[1] end
                 end
                 else
                 case vartype(tmp2) of
                  varInteger:Assign(tmpNam,longint(tmp2));
                  varDouble:Assign(tmpNam,real(tmp2));
                  varString,varOleStr:Assign(tmpNam,ansistring(tmp2));
                  varQWord:Assign(tmpNam,newpint(tmp2));
                  else halt(1101)
                 end;
                 Vau.pushback(tmp2) end
      end
     end;
     Nam.pushback(' pending...')
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
       (now='while') then break;  //Key Words Avoid
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
                  begin Val(now,NumB); Vau.pushback(NumB); Nam.pushback(' pending...') end
             else begin Val(now,NumA); Vau.pushback(NumA); Nam.pushback(' pending...') end
            end
            else
            begin
             d:=Find(Now);
             if d=-1 then Add(now,int);
             if (d<>-1)and(vtype.Items[d]=func) then begin Opt.pushback('f');
              NumParamBase.pushback(Vau.Size);
              NumParamName.pushback(Now) end
             else begin
              Nam.pushback(now);
              Vau.pushback(Get(now))
             end
            end
           end
     end;
     if mean<>#0 then
     begin
      Flush(Pri[mean]);
      Opt.pushback(mean)
     end;
     pas:=false
    end
    else
    begin
     if GetBetaSafe(s,i)='//' then break;
     now:=GetBeta(s,i);
     if (now='')or(now=';') then break;
     if now=',' then begin Flush(Pri['(']+1); pas:=true end else
     if now=#39 then
     begin
      NumC:='';
      repeat
       NumC:=NumC+GetGama(s,i,#39);
       if (i<=length(s))and(s[i]=#39) then begin NumC:=NumC+#39; inc(i) end
       else break
      until false;
      Vau.pushback(NumC); Nam.pushback(' pending...')
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
      if (mean=')')or(mean=']') then pas:=false;
      if (pas)and((mean='+')or(mean='-')) then Vau.pushback(0);
      if (mean='(')or(mean='[') then pas:=true else
      if mean<>':' then Flush(Pri[mean]+ord(mean=':'));
      if (mean=')')or(mean=']') then begin Opt.pop;
      if mean=']' then begin tmp1:=TransValue(Vau.top);
      if vartype(tmp1)<>varinteger then halt(1101);
      Vau.pop; Nam.pop; Vau.pop;
      d:=Find(Nam.top);
      if (d=-1)or(vtype.Items[d]<>intarr)and(vtype.Items[d]<>psstr) then
       begin Assign(Nam.top,newpint(tmp1+1)); d:=Size end;
      Nam.pop;
      if vtype.Items[d]=intarr then begin Vau.pushback(Longword(@v_intarr.Items[vindex.Items[d]][tmp1])); Nam.pushback(' system.pint') end else
      if vtype.Items[d]=psstr  then begin Vau.pushback(LongWord(@v_psstr.Items[vindex.Items[d]])); Nam.pushback(' system.pchar@'+IntToStr(tmp1)) end
      end end else Opt.pushback(mean)
     end
    end
   until false;
   Flush(0);
   if Vau.isnil then GetDelta:=nil
                else GetDelta:=Vau.Items[Vau.Size]
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
    now:=GetAlphaSafe(s,i);
    if (now='begin')or
       (now='end')or
       (now='if')or
       (now='then')or
       (now='else')or
       (now='for')or
       (now='to')or
       (now='do')or
       (now='while') then break;  //Key Words Avoid
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
   case t of
    'begin','end','then','else':exit(GetAlpha(s,i));
    'if':begin i:=bias; t:=GetDeltaSafe(s,i); i:=Bias; exit('if '+t) end;
    'for':begin i:=Bias; GetEpsilon:='for ';
                t:=GetAlpha(s,i); if Find(t)=-1 then Add(t,int); GetEpsilon:=GetEpsilon+t;
                if GetBeta(s,i)<>':=' then halt(1202); GetEpsilon:=GetEpsilon+':=';
                GetEpsilon:=GetEpsilon+GetDeltaSafe(s,i); i:=Bias;
                if GetAlpha(s,i)<>'to' then halt(1203); GetEpsilon:=GetEpsilon+' to ';
                GetEpsilon:=GetEpsilon+GetDeltaSafe(s,i); i:=Bias;
                if GetAlpha(s,i)<>'do' then halt(1204); GetEpsilon:=GetEpsilon+' do' end;
    'while':begin i:=Bias; GetEpsilon:='while ';
                  GetEpsilon:=GetEpsilon+GetDeltaSafe(s,i); i:=Bias;
                  if GetAlpha(s,i)<>'do' then halt(1205); GetEpsilon:=GetEpsilon+' do' end;
    else begin GetEpsilon:=' '+GetDeltaSafe(s,i); i:=Bias end
   end
  end;

  function GetAlpha:ansistring;begin exit(GetAlpha(s,i)) end;
  function GetAlphaSafe:ansistring;begin exit(GetAlphaSafe(s,i)) end;
  function GetBeta:ansistring;begin exit(GetBeta(s,i)) end;
  function GetBetaSafe:ansistring;begin exit(GetBetaSafe(s,i)) end;
  function GetDelta:Variant;begin exit(GetDelta(s,i)) end;
  function GetDeltaSafe:ansistring;begin exit(GetDeltaSafe(s,i)) end;
  function GetDelta(const s:ansistring):Variant;var i:longint=1;begin exit(GetDelta(s,i)) end;
  function GetEpsilon:ansistring;begin exit(GetEpsilon(s,i)) end;


  procedure Release;
  var ii:longint;
  begin
   case ScriptDirector of
    '1':if Patt and 2<>0 then case Exec(Script) of 1:Exec:=1; 2:Exec:=2 end;
    '0':if Patt and 4<>0 then case Exec(Script) of 1:Exec:=1; 2:Exec:=2 end;
    else if Patt and 8<>0 then for ii:=Get(ScriptDirector) to Args do begin Assign(ScriptDirector,ii); case Exec(Script) of 1:continue; 2:break end end
    else if Patt and 16<>0 then while GetDelta(ScriptDirector)<>0 do case Exec(Script) of 1:continue; 2:break end
    else case Exec(Script) of 1:Exec:=1; 2:Exec:=2 end
   end;
   Patt:=0;
  end;

  function Analysis:longint;
  var
   t:ansistring;
   d:longint;
  begin
   t:=GetAlphaSafe;
   case t of
     'continue':exit(1);
     'break':exit(2);
     'begin','end':i:=Bias;
       'if':begin
             i:=Bias;
             if GetDelta<>0 then ScriptDirector:='1'
                            else ScriptDirector:='0';
            end;
     'then':begin
             if Patt=0 then begin Script.Clear; LazNet.Clear; LazNet.pushback(1) end;
             i:=Bias;
             Patt:=(Patt or 6)xor 4;
            end;
     'else':begin
             if Patt=0 then begin Script.Clear; LazNet.Clear; LazNet.pushback(1) end;
             i:=Bias;
             Patt:=(Patt or 6)xor 2
            end;
      'for':begin
             if Patt=0 then begin Script.Clear; LazNet.Clear; LazNet.pushback(1) end;
             i:=Bias;
             t:=GetAlpha;
             d:=Find(t);
             if d=-1 then Add(t,int);
             if GetBeta<>':=' then halt(1202);
             Assign(t,longint(GetDelta));
             if GetAlpha<>'to' then halt(1203);
             Patt:=Patt or 8;
             Args:=Longint(GetDelta);
             ScriptDirector:=t;
             if GetAlpha<>'do' then halt(1204)
            end;
    'while':begin
             if Patt=0 then begin Script.Clear; LazNet.Clear; LazNet.pushback(1) end;
             i:=Bias;
             Patt:=Patt or 16;
             ScriptDirector:=GetDeltaSafe;
             i:=Bias;
             if GetAlpha<>'do' then halt(1205)
            end;
    else GetDelta
   end;
   exit(0)
  end;


  procedure GetLazy;
  var
   t:ansistring;
  begin
   LazTag:=1;
   t:=GetEpsilon;
   if t[length(t)]=';' then begin LazTag:=0; if LazNet.top>0 then dec(LazNet.Items[LazNet.Size]) end;
   if t='begin' then begin inc(nest); LazNet.pushback(1) end;
   if t='end' then begin dec(nest); LazNet.pop end;
   if (t='else')or
      (copy(t,1,2)='if')or
      (copy(t,1,3)='for')or
      (copy(t,1,5)='while') then inc(LazNet.Items[LazNet.Size]);
   Script.pushback(t);
   if (nest=0)and(LazTag=0)and(LazNet.Size=1)and(LazNet.Items[1]=0) then Release
  end;

 begin
  Exec:=0;
  s:=lowercase(os);
  i:=1;
  while (i<=length(s))and(GetBetaSafe<>'//') do
  if Patt=0 then case Analysis of 1:exit(1); 2:exit(2) end
            else begin GetLazy; if Exec<>0 then exit end
 end;


  function __write(const a:VList):Variant;
  var i:longint;
  begin
   if a.Size>0 then write(TransValue(a.Items[1]));
   for i:=2 to a.Size do write(TransValue(a.Items[i]));
   exit(0)
  end;

  function __writeln(const a:VList):Variant;
  begin __write(a); writeln; exit(0) end;

  function __max(const a:VList):Variant;
  var i:longint; begin __max:=TransValue(a.Items[1]); for i:=2 to a.Size do if TransValue(a.Items[i])>__max then __max:=TransValue(a.Items[i]) end;

  function __min(const a:VList):Variant;
  var i:longint; begin __min:=TransValue(a.Items[1]); for i:=2 to a.Size do if TransValue(a.Items[i])<__min then __min:=TransValue(a.Items[i]) end;

  function __odd(const a:VList):Variant;
  begin exit(longint(a.Items[1] and 1)) end;

  function __abs(const a:VList):Variant;
  begin exit(abs(TransValue(a.Items[1]))) end;

  function __sqr(const a:VList):Variant;
  begin exit(sqr(TransValue(a.Items[1]))) end;

  function __sqrt(const a:VList):Variant;
  begin exit(sqrt(TransValue(a.Items[1]))) end;

  function __round(const a:VList):Variant;
  begin exit(round(TransValue(a.Items[1]))) end;

  function __trunc(const a:VList):Variant;
  begin exit(trunc(TransValue(a.Items[1]))) end;

  function __copy(const a:VList):Variant;
  begin exit(copy(TransValue(a.Items[1]),TransValue(a.Items[2]),TransValue(a.Items[3]))) end;

  function __pos(const a:VList):Variant;
  begin exit(pos(TransValue(a.Items[1]),TransValue(a.Items[2]))) end;

  function __upcase(const a:VList):Variant;
  begin exit(upcase(TransValue(a.Items[1]))) end;

  function __lowercase(const a:VList):Variant;
  begin exit(lowercase(TransValue(a.Items[1]))) end;

  function __Str(const a:VList):Variant;
  begin exit(IntToStr(TransValue(a.Items[1]))) end;

  function __Val(const a:VList):Variant;
  begin exit(StrToInt(TransValue(a.Items[1]))) end;

  function __sin(const a:VList):Variant;
  begin exit(sin(TransValue(a.Items[1]))) end;

  function __cos(const a:VList):Variant;
  begin exit(cos(TransValue(a.Items[1]))) end;

  function __exp(const a:VList):Variant;
  begin exit(exp(TransValue(a.Items[1]))) end;

  function __ln(const a:VList):Variant;
  begin exit(ln(TransValue(a.Items[1]))) end;

  function __random(const a:VList):Variant;
  begin exit(random(TransValue(a.Items[1]))) end;

  function __length(const a:VList):Variant;
  begin exit(length(TransValue(a.Items[1]))) end;

  function __chr(const a:VList):Variant;
  begin exit(ansistring(chr(TransValue(a.Items[1])))) end;

  function __ord(const a:VList):Variant;
  begin exit(longint(ord(ansistring(TransValue(a.Items[1]))[1]))) end;

  function __pred(const a:VList):Variant;
  begin exit(ansistring(pred(char(TransValue(a.Items[1]))))) end;

  function __succ(const a:VList):Variant;
  begin exit(ansistring(succ(char(TransValue(a.Items[1]))))) end;

  function __halt(const a:VList):Variant;
  begin if a.Size=0 then halt; halt(TransValue(a.Items[1])) end;


 procedure PSLib.UsesSystem;
 begin
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
  Assign('exit',@__halt);
  Assign('halt',@__halt);

 end;


begin
 randomize;

 fillchar(Pri,sizeof(Pri),$ff);
 Pri['[']:=10;
 Pri['(']:=10;
 Pri[']']:=15;
 Pri[')']:=15;
 Pri[':']:=20; //:=
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

 acceptset:=['a'..'z','0'..'9','_','$','&','%','.'];
   pattset:=['(',')','[',']',#39,',',';'];
    runset:=['!'..'/',':'..'@','['..'`']-['$','&','%','.'];

end.
