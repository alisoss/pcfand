{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit wwmix;

interface

uses dos,base,access;

var   ss:record PointTo:StringPtr;  {(nil)  at beginning point to this item}
                Abcd,          {(false) alphabetical order in window}
                AscDesc,       {(false) > ascending, < descending }
                Subset,        {(false)}
                ImplAll,       {(false)  implic. the whole set }
                Empty:boolean;   { returned, test before calling SelectStr}
                Size:word;       { returned, subset size  after SelectStr }
                Tag:char;        { returned for each GetSelect }
          end;
const SelMark=#$F0;

procedure PutSelect (s:ScreenStr);
procedure SelectStr(C1,R1:integer;NMsg:word;const LowTxt:string);
function GetSelect :ScreenStr;

function SelFieldList (Nmsg:word;ImplAll:boolean;var FLRoot:FieldList):boolean;

function SelectDiskFile(Path:string; HdMsg:word; OnFace:boolean):PathStr;

function PromptFilter(Txt:string; var Bool:FrmlPtr; var BoolTxt:StringPtr):boolean;
procedure PromptLL(N:word;var Txt:string;I:word;Del:boolean);

function PassWord(TwoTimes:boolean):string20;
procedure SetPassword(FD:FileDPtr; Nr:word; Pw:string20);
function HasPassword(FD:FileDPtr; Nr:word; Pw:string20):boolean;


implementation

uses obaseww,drivers,runfrml,compile,rdrun,runedi;


procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

type ItemPtr=^Item;
     Item=record Chain:ItemPtr;
                 Tag:char;
                 S:string[1] end;
var  sv:record ItemRoot:ItemPtr;
               markp:pointer;
               NItems,MaxItemLen,
               Tabs,TabSize,WwSize,
               Base,iItem:integer end;

procedure PutSelect(s:ScreenStr);
var p:ItemPtr; l:word;
begin
  l:=minw(length(s),46);
  p:=GetStore(sizeof(p^)-1+l); p^.Tag:=' ';
  Move(s[1],p^.S[1],l); p^.S[0]:=char(l); if ss.Empty then begin
    FillChar(sv,sizeof(sv),#0); FillChar(ss.Abcd,sizeof(ss)-5,#0);
    sv.markp:=p end;
  ChainLast(sv.ItemRoot,p);
  inc(sv.NItems); sv.MaxItemLen:=maxw(l,sv.MaxItemLen);
end;

function GetItem(N:word):ItemPtr;
  var p:ItemPtr; i:word;
  begin p:=sv.ItemRoot;for i:=2 to N do p:=p^.Chain;GetItem:=p end;

procedure SelectStr(C1,R1:integer;NMsg:word;const LowTxt:string);
procedure WriteItem(N:word);
  var i,l:word; p:ItemPtr;
  begin with sv do begin i:=N-Base;
        gotoXY((i mod Tabs)*TabSize+2, i div Tabs+1);
        if N>NItems then l:=TabSize-2
        else begin p:=GetItem(N); if ss.subset then write(p^.Tag); write(p^.S);
                   l:=MaxItemLen-length(p^.S) end;
        if l>0 then write(' ':l); end end;
procedure SetAttr(Attr:word);
  begin TextAttr:=Attr;WriteItem(sv.iItem) end;
procedure IVOn;
  begin TextAttr:=colors.sHili;WriteItem(sv.iItem) end;
procedure IVOff;
  begin TextAttr:=colors.sNorm;WriteItem(sv.iItem) end;
procedure DisplWw;
  var i,max:word; c:char;
  begin with sv do begin TextAttr:=colors.sNorm; max:=Base+wwsize-1;
        if Base>1 then c:='' else c:=' ';
        ScrWrChar(WindMin.X,WindMin.Y,c,TextAttr);
        if max>=NItems then c:=' ' else c:='';
        ScrWrChar(WindMax.X,WindMax.Y,c,TextAttr);
        for i:=Base to max do WriteItem(i); SetAttr(colors.sHili) end;
  end;
procedure Right;
  begin with sv do if iItem<NItems then
        begin IVOff;inc(iItem);
              if iItem>=Base+WwSize then begin inc(Base,Tabs);DisplWw end
              else IVOn end end;
procedure Left;
  begin with sv do if iItem>1 then
        begin IVOff;dec(iItem);
              if iItem<Base then begin dec(Base,Tabs);DisplWw end
              else IVOn end end;
procedure Down;
  begin with sv do if iItem+Tabs<=NItems then
        begin IVOff;inc(iItem,Tabs);
              if iItem>=Base+WwSize then begin inc(Base,Tabs);DisplWw end
              else IVOn end end;
procedure Up;
  begin with sv do if iItem>Tabs then
        begin IVOff;dec(iItem,Tabs);
              if iItem<Base then begin dec(Base,Tabs);DisplWw end
              else IVOn end end;
procedure SetTag(c:char);
  var p:ItemPtr;
  begin p:=GetItem(sv.iItem);p^.Tag:=c;TextAttr:=colors.sHili;
        WriteItem(sv.iItem);Right end;
procedure SetAllTags(c:char);
  var p:ItemPtr;
  begin p:=sv.ItemRoot;while p<>nil do begin p^.Tag:=c;p:=p^.Chain end;
        DisplWw end;                                       
procedure Switch(I1,I2:word);
var p1,p2,q1,q2,h:ItemPtr; i:word;
begin
  p1:=ItemPtr(@sv.ItemRoot); for i:=2 to I1 do p1:=p1^.Chain;q1:=p1^.Chain;
  p2:=ItemPtr(@sv.ItemRoot); for i:=2 to I2 do p2:=p2^.Chain;q2:=p2^.Chain;
  h:=q1^.Chain; p1^.Chain:=q2; q1^.Chain:=q2^.Chain;
  if p2=q1 then q2^.Chain:=q1 else begin q2^.Chain:=h; p2^.Chain:=q1 end;
end;
procedure GraspAndMove(schar:char);
  var A:word; p:ItemPtr;
  label 1;
  begin with sv do begin p:=GetItem(iItem);if p^.Tag=' ' then p^.Tag:=schar;
      SetAttr(colors.sHili+$80); A:=colors.sHili; colors.sHili:=colors.sHili+$80;
    1:case ReadKbd of
       _left_ :if iItem>1 then begin Switch(iItem-1,iItem);Left end;
       _right_:if iItem<NItems then begin Switch(iItem,iItem+1);Right end;
       _down_ :if iItem+Tabs<=NItems then begin Switch(iItem,iItem+Tabs);Down end;
       _up_   :if iItem>Tabs then begin Switch(iItem-Tabs,iItem);Up end;
       _F9_,_ESC_:begin colors.sHili:=A;SetAttr(A);exit end;
      end; goto 1 end end;
procedure AbcdSort;
var p,q,r:ItemPtr; sorted:boolean;
begin
  repeat r:=ItemPtr(@sv.ItemRoot) ; p:=sv.ItemRoot; q:=p^.Chain; sorted:=true;
    while q<>nil do begin
      if CompLexStr(p^.S,q^.S)=ord(_gt) then begin
        r^.Chain:=q; p^.Chain:=q^.Chain; q^.Chain:=p;
        r:=q; q:=p^.Chain; sorted:=false end
      else begin r:=p;p:=q;q:=q^.Chain end end;
  until sorted;
end;
procedure SetFirstiItem;
var p:ItemPtr;
begin
  sv.iItem:=1; if ss.pointto=nil then exit; p:=sv.ItemRoot;
  while p<>nil do begin
    if p^.s=ss.pointto^ then exit; inc(sv.iItem); p:=p^.Chain end;
end;
function MouseInItem(var I:integer):boolean;
var x,ix:integer;
begin
  MouseInItem:=false; x:=Event.Where.X-WindMin.X-1; if x<0 then exit;
  ix:=x div sv.TabSize; if ix>=sv.Tabs then exit;
  if (Event.Where.Y<WindMin.Y) or (Event.Where.Y>WindMax.Y) then exit;
  I:=(Event.Where.Y-WindMin.Y)*sv.Tabs+ix+sv.Base; if I>sv.NItems then exit;
  MouseInItem:=true;
end;
var pw:pointer; cols,rows,c2,r2,maxbase:word; w2:longint;
    schar:char; b:integer; p:ItemPtr; i,iOld:integer;
label 1,2,3;
begin with sv do begin                                                          
  pw:=PushScr(1,TxtRows,TxtCols,TxtRows);
  if ss.subset then
    if ss.ascdesc then WrLLMsg(135) else WrLLMsg(134)
  else WrLLMsg(152);
  rows:=5; if TxtCols>52 then cols:=50 else cols:=TxtCols-2; RdMsg(NMsg);
  c2:=cols; if c1<>0 then c2:=c1+cols+1;
  r2:=rows; if r1<>0 then r2:=r1+rows+1; TextAttr:=colors.sNorm;
  w2:=PushWFramed(c1,r1,c2,r2,TextAttr,MsgLine,LowTxt,WHasFrame+WDoubleFrame+WShadow+WPushPixel);
  if ss.Empty then begin repeat ReadKbd until KbdChar=_ESC_;goto 3 end;
  TabSize:=MaxItemLen+2; if ss.subset then inc(TabSize);
  Tabs:=cols div TabSize; WwSize:=Tabs*Rows;
  MaxBase:=1; while MaxBase+WwSize<=NItems do inc(MaxBase,Tabs);
  if ss.abcd then AbcdSort;
  if ss.ascdesc then schar:='<' else schar:=^p;
  SetFirstiItem; Base:=iItem-(iItem-1) mod WwSize; DisplWw; iOld:=0;
1:GetEvent; case Event.What of
    evMouseMove:
      if (iOld<>0) and MouseInItem(i) and (i<>iOld) then begin
          Switch(i,iOld); iItem:=i; DisplWw; iOld:=i end;
    evMouseDown:
      if MouseInItem(i) then
        if ss.Subset then begin
          p:=GetItem(i); if p^.Tag=' ' then p^.Tag:=schar else p^.Tag:=' ';
          iItem:=i; iOld:=i; DisplWw end
        else goto 2
      else if ss.SubSet and ((Event.Buttons and mbDoubleClick)<>0) then begin
2:      KbdChar:=_M_; iItem:=i; goto 3 end;
    evMouseUp:iOld:=0;
    evKeyDown: begin
      KbdChar:=Event.KeyCode;
      case KbdChar of
       _M_,
       _ESC_  :begin
3:             ClrEvent; PopW(w2); PopScr(pw); ReleaseStore(pw);
               if ss.Empty then exit; ss.Empty:=true; ss.Pointto:=nil;
               ss.size:=0; p:=ItemRoot; while p<>nil do begin
                 if p^.Tag<>' ' then inc(ss.size); p:=p^.Chain end;
               if ss.subset and ss.implall and (ss.size=0) then begin
                 p:=ItemRoot; while p<>nil do begin
                   if p^.S[1]<>SelMark then begin p^.Tag:=schar; inc(ss.size) end;
                   p:=p^.Chain end end;
               if KbdChar=_ESC_ then ReleaseStore(markp);
               exit end;
       _left_ :Left;
       _right_:Right;
       _up_   :Up;
       _down_ :Down;
       _PgUp_ :if Base>1 then
               begin IVOff;b:=Base-WwSize;if b<1 then b:=1;
                     dec(iItem,Base-b);Base:=b;DisplWw;IVOn end;
       _PgDn_ :if Base<MaxBase then
               begin IVOff;b:=Base+WwSize;if b>MaxBase then b:=MaxBase;
                     inc(iItem,b-Base);if iItem>NItems then dec(iItem,Tabs);
                     Base:=b;DisplWw;IVOn end;
       _Z_    :if Base<MaxBase then begin
                 IVOff; Base:=Base+Tabs; if iItem<Base then iItem:=iItem+Tabs;
                 if iItem>Nitems then iItem:=NItems; DisplWw; IVOn end;
       _W_    :if Base>1 then begin
                 IVOff; Base:=Base-Tabs;
                 if iItem>=Base+WwSize then iItem:=iItem-Tabs; DisplWw; IVOn end;
       _CtrlPgUp_,_Home_:if iItem>1 then
               begin IVOff;iItem:=1;if Base>1 then begin Base:=1;DisplWw end;
                     IVOn end;
       _CtrlPgDn_,_End_:if iItem<NItems then
               begin IVOff;iItem:=NItems;
                     if Base<MaxBase then begin Base:=MaxBase;DisplWw end;
                     IVOn end;
       else if ss.subset then
        case KbdChar of
         _F2_    :SetTag(schar);
         _CtrlF2_:SetAllTags(schar);
          62 {>} :if ss.ascdesc then SetTag('>');
         _F3_    :SetTag(' ');
         _CtrlF3_:SetAllTags(' ');
         _F9_    :begin ClrEvent; GraspAndMove(schar) end end;
    end end end;
    ClrEvent; goto 1;
end end;

function GetSelect:ScreenStr;
  var p:ItemPtr absolute sv{.ItemRoot};
  begin if not ss.subset then
        begin p:=GetItem(sv.iItem);GetSelect:=p^.S;ReleaseStore(sv.markp);exit end;
        while (p<>nil) and (p^.Tag=' ') do p:=p^.Chain;
        if p=nil then begin ss.Tag:=' ';GetSelect:='';exit end;
        ss.Tag:=p^.Tag;GetSelect:=p^.S;p:=p^.Chain end;

{---------------------------------------------------------------------------}

function SelFieldList (Nmsg:word;ImplAll:boolean;var FLRoot:FieldList):boolean;
  var F:FieldDPtr; FL:FieldList; s:ScreenStr;
  label 1;
  begin FLRoot:=nil;SelFieldList:=true;if ss.Empty then exit;
        ss.subset:=true;ss.implall:=ImplAll; SelectStr(0,0,Nmsg,CFile^.Name);
        if KbdChar=_ESC_ then begin SelFieldList:=False; exit end;
      1:s:=GetSelect;
        if s<>'' then
        begin F:=CFile^.FldD;if s[1]=SelMark then s:=copy(s,2,255);
              while F<>nil do
              if s=F^.Name then
                 begin FL:=GetStore(SizeOf(FL^)); ChainLast(FLRoot,FL);
                       FL^.FldD:=F; goto 1 end
              else F:=F^.Chain;
              goto 1 end;
  end;

{---------------------------------------------------------------------------}

function SelectDiskFile(Path:string; HdMsg:word; OnFace:boolean):PathStr;
var mask,s:string; SR:SearchRec; w:longint;
    p:PathStr; d:DirStr; n:NameStr; ext,e:ExtStr; ne:string[12];
    c1,c2,r1,r2,c11,r11:word;
label 1,2,3;
begin
  SelectDiskFile:=''; c1:=0; r1:=0; c2:=22; r2:=1; c11:=0; r11:=0;
  if OnFace then begin
    c1:=43; r1:=6; c2:=67; r2:=8; c11:=28; r11:=4 end;
  if Path='' then ext:='.*' else if Path[1]='.' then ext:=Path
  else begin FSplit(FExpand(Path),d,n,e); ne:=n+e;
    if ne='' then ne:='*.*'; goto 3 end;
  mask:='*'+ext;
1:RdMsg(HdMsg);
  w:=PushWFramed(c1,r1,c2,r2,colors.sMask,MsgLine,'',WHasFrame+WShadow+WPushPixel);
2:GotoXY(1,1);
  EditTxt(mask,1,sizeof(mask)-1,22,'A',true,false,true,false,0);
  if KbdChar=_ESC_ then begin PopW(w); exit end;
  if pos(' ',mask)<>0 then begin WrLLF10Msg(60); goto 2 end;
  FSplit(FExpand(mask),d,n,e);
  if e='' then e:=ext
  else if (ext='.RDB') and (e<>'.RDB') then begin
    WrLLF10Msg(005);goto 2 end;
  PopW(w);
  if n='' then n:='*'; ne:=n+e;
  if (pos('*',ne)=0) and (pos('?',ne)=0) then begin
    SelectDiskFile:=d+ne; exit end;
3:p:=d+ne; FindFirst(p+#00,0,SR);
  if not(DosError in [0,18]) then begin
    SetMsgPar(p); mask:=p; WrLLF10Msg(811); goto 1 end;
  while DosError=0 do begin
    PutSelect(SR.name); FindNext(SR) end;
  FindFirst(d+'*.*'+#00,Directory,SR);
  while DosError=0 do begin
    if ((SR.Attr and Directory)<>0) and
       ((pos('\',d)<>length(d)) or (SR.name<>'..')) and
       (SR.name<>'.') then PutSelect('\'+SR.name);
    FindNext(SR) end;
  ss.Abcd:=true; SelectStr(c11,r11,HdMsg,p);
  if KbdChar=_ESC_ then exit;
  s:=GetSelect;
  if s[1]='\' then begin
    delete(s,1,1);
    if s='..' then repeat dec(d[0]) until d[length(d)]='\'
    else d:=d+s+'\';
    goto 3 end;
  SelectDiskFile:=d+s;
end;

function PromptFilter(Txt:string; var Bool:FrmlPtr; var BoolTxt:StringPtr):boolean;
  var p:pointer; er:ExitRecord; I:word; Msg:ScreenStr; Del:boolean; cf:FileDPtr;
  label 1,2,3;
  begin MarkStore(p);I:=1;NewExit(Ovr,er);goto 3; PromptFilter:=true; Del:=true;
        ResetCompilePars; cf:=CFile;
      1:PromptLL(113,Txt,I,Del); Bool:=nil; BoolTxt:=nil;
        if KbdChar=_ESC_ then begin PromptFilter:=false;goto 2 end;
        if length(Txt)=0 then goto 2;
        SetInpStr(Txt);RdLex; Bool:=RdBool;
        if Lexem<>^z then Error(21);
        BoolTxt:=GetStore(length(Txt)+1);Move(Txt,BoolTxt^,length(Txt)+1);
      2:RestoreExit(er);exit;
      3:Msg:=MsgLine;I:=CurrPos;SetMsgPar(Msg); WrLLF10Msg(110);
        IsCompileErr:=false;ReleaseStore(p);CFile:=cf;Del:=false; goto 1;
  end;

procedure PromptLL(N:word;var Txt:string;I:word;Del:boolean);
  var w:longint;
  begin w:=PushW(1,TxtRows,TxtCols,TxtRows);
        GotoXY(1,TxtRows);TextAttr:=colors.pTxt;ClrEol;
        RdMsg(N);write(MsgLine);
        TextAttr:=colors.pNorm;
        EditTxt(Txt,I,255,TxtCols-whereX,'A',Del,false,true,false,0);
        PopW(w) end;

function PassWord(TwoTimes:boolean):string20;
  var w:longint; Txt,Txt1:string;  MsgNr,col:word;
  label 1,2;
  begin col:=(TxtCols-21)shr 1; w:=PushW(col,TxtRows-2,col+21,TxtRows-2);
        MsgNr:=628;
     1: TextAttr:=colors.pNorm or $80; gotoxy(1,1); ClrEol; RdMsg(MsgNr);
        write(MsgLine:(length(MsgLine)+22)div 2);
        KbdBuffer:=char(ReadKbd)+KbdBuffer;
        TextAttr:=colors.pNorm;gotoxy(2,1);Txt:='';
        EditTxt(Txt,1,20,20,'A',true,true,true,false,0);
        if KbdChar=_ESC_ then begin Txt:=''; goto 2 end;
        if TwoTimes then
          if MsgNr=628 then begin MsgNr:=637; Txt1:=Txt; goto 1 end
          else if Txt<>Txt1 then begin WrLLF10Msg(638); MsgNr:=628; goto 1 end;
     2: PopW(w); Password:=Txt;
  end;
procedure SetPassword(FD:FileDPtr; Nr:word; Pw:string20);
  var p:pointer;
  begin if Nr=1 then p:=@FD^.TF^.PwCode else p:=@FD^.TF^.Pw2Code;
        FillChar(p^,20,'@'); Move(Pw[1],p^,length(Pw)); Code(p^,20) end;
function HasPassword(FD:FileDPtr; Nr:word; Pw:string20):boolean;
  var X:PwCodeArr;
  begin with FD^.TF^ do if Nr=1 then X:=PwCode else X:=Pw2Code;
        Code(X,20); HasPassword:=Pw=TrailChar('@',X) end;



end.