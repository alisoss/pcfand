{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit runedi;

interface

uses dos,
     drivers,base,access,
{$ifdef FandSQL} channel, {$endif}
     rdrun,wwmenu,wwmix,
     compile,rdedit,
     editor;

var TxtEdCtrlUBrk,TxtEdCtrlF4Brk:boolean;
function EditTxt(var s:string; pos,maxlen,maxcol:word;
         typ:char; del,star,upd,ret:boolean; Delta:word):word;


 function CRec:longint;
 function TestIsNewRec:boolean;        { used only in RunProj,RunFrml }
procedure SetSelectFalse;
procedure PopEdit;
procedure WrEStatus;
procedure RdEStatus;
procedure SetNewCRec(N:longint;withRead:boolean);
procedure GotoRecFld(NewRec:longint;NewFld:EFldDPtr);
function EditFreeTxt(F:FieldDPtr;ErrMsg:ScreenStr;Ed:boolean;var Brk:word):boolean;
procedure DisplEditWw;
 function OpenEditWw:boolean;
procedure RunEdit(PX:XStringPtr;var Brk:word);
var CFld:EFldDPtr;

function PromptB(const S:string;Impl:FrmlPtr;F:FieldDPtr):boolean;
function PromptS(const S:string;Impl:FrmlPtr;F:FieldDPtr):string;
function PromptR(const S:string;Impl:FrmlPtr;F:FieldDPtr):float;

procedure EditDataFile(FD:FileDPtr;EO:EditOptPtr);
function SelFldsForEO(EO:EditOptPtr;LD:LinkDPtr):boolean;
procedure UpdateEdTFld(S:LongStrPtr);
function StartExit(X:EdExitDPtr;Displ:boolean):boolean;



implementation

uses obaseww,oaccess,runfrml,rdfildcl,sort,genrprt,runproc,runproj
 {$ifdef FandGraph},rungraph {$endif}
     ;


procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

var E: EditDPtr absolute EditDRoot;

var FirstEmptyFld:EFldDPtr; VK:KeyDPtr; WK:WKeyDPtr;
    BaseRec:longint; IRec:byte;
    IsNewRec,Append,Select,WasUpdated,EdRecVar,
    AddSwitch,ChkSwitch,WarnSwitch,Subset,NoDelTFlds,WasWK,
    NoDelete,VerifyDelete,NoCreate,F1Mode,OnlyAppend,OnlySearch,
    Only1Record,OnlyTabs,NoESCPrompt,MustESCPrompt,Prompt158,
    NoSrchMsg,WithBoolDispl,Mode24,NoCondCheck,F3LeadIn,
    LUpRDown,MouseEnter,TTExit,
    MakeWorkX,NoShiftF7Msg,MustAdd,MustCheck,SelMode: boolean;

var UpdCount,CPage:word; RT:ERecTxtDPtr;
    HasIndex,HasTF,NewDisplLL:boolean;

procedure PopEdit;
  begin E:=E^.PrevE end;
function TestIsNewRec:boolean;
  begin TestIsNewRec:= IsNewRec end;
procedure SetSelectFalse;
  begin Select:=false end;


{$I runedit1}
{$I runedit2}
{$I runedit3}


function EditTxt(var s:string; pos,maxlen,maxcol:word;
          typ:char; del,star,upd,ret:boolean; Delta:word{;Mask:StringPtr}):word;
var sLen:byte absolute s; base,cx,cy,cx1,cy1:word;
procedure DelBlk;
  begin while (sLen>0) and (s[sLen]=' ')  and (pos<=sLen) do dec(sLen) end;
procedure WriteStr;
var BuffLine:array [1..MaxTxtCols] of word;
    i:word; x:record chr:char;attr:byte end; item:word absolute x;
begin
  if pos<=base then base:=pos-1 else if pos>base+maxcol then begin
    base:=pos-maxcol; if pos>maxlen then dec(base) end;
  if (pos=base+1) and (base>0) then dec(base); DelBlk;
  for i:=1 to maxcol do begin
    x.attr:=TextAttr;
    if base+i<=sLen then begin
      if star then x.chr:='*' else x.chr:=s[base+i];
      if x.chr<' ' then begin
        x.chr:=char(ord(x.chr)+64);x.attr:=colors.tCtrl end end
    else x.chr:=' ';
    BuffLine[i]:=item end;
  ScrWrBuf(cx1,cy1,BuffLine,maxcol);
  gotoxy(cx+pos-base-1,cy);
end;
var EndTime:longint;  InsMode:boolean;
label 0,1,2,3,4,5,6,7,8;
begin
  InsMode:=true; base:=0; if pos>maxlen+1 then pos:=maxlen+1;
  cx:=whereX; cx1:=cx+WindMin.X-1; cy:=whereY; cy1:=cy+WindMin.Y-1;
  CrsNorm; WriteStr;
1:case WaitEvent(Delta) of
   1{flags}:goto 1;
   2{timer}:begin KbdChar:=_ESC_; goto 6 end end;
  case Event.What of
    evMouseDown:
      if MouseInRect(cx1,cy1,maxcol,1) then begin ClrEvent; KbdChar:=_M_; goto 6 end;
    evKeyDown: begin
      KbdChar:=Event.KeyCode; ClrEvent;
      if del then begin
        if KbdChar in [$20..$FE] then begin pos:=1;sLen:=0;WriteStr end;
        del:=false end;
      case KbdChar of
       _ins_,_V_  :InsMode:=not InsMode;
       _U_        :if TxtEdCtrlUBrk then goto 6;
       _CtrlF4_   :if TxtEdCtrlF4Brk then goto 6;
       _ESC_,_M_  :begin
6:                 DelBlk; CrsHide; TxtEdCtrlUBrk:=false; TxtEdCtrlF4Brk:=false;
                   EditTxt:=0; exit end;
       _left_,_S_ :if (pos>1) then dec(pos);
       _right_,_D_:if pos<=maxlen then
                   begin if (pos>sLen) and (sLen<maxlen) then s:=s+' ';
                         inc(pos) end;
       _Q_        :case ReadKbd of _S_:goto 3; _D_:goto 4 end;
       _home_     :
3:                 pos:=1;
       _end_      :
4:                 pos:=sLen+1;
       _H_        :if upd and (pos>1) then begin dec(pos);goto 2 end;
       _del_,_G_  :if upd and (pos<=sLen) then begin
2:                   if sLen>pos then move(s[pos+1],s[pos],sLen-pos);
                     dec(sLen) end;
       _P_        :if upd then begin ReadKbd; if KbdChar in [0..31] then goto 5 end;
       _F4_       :if upd and (Typ='A') and (pos<=sLen) then begin
                      s[pos]:=ToggleCS(s[pos]) end;
       $20..$FF   :if upd then begin case typ of
                     'N':if not(char(KBdChar) in ['0'..'9']) then goto 7;
                     'F':if not(char(KbdChar) in ['0'..'9','.',',','-']) then goto 7;
                     'R':if not(char(KbdChar) in ['0'..'9','.',',','-','+','e','E']) then goto 7;
                     end;
5:           if pos>maxlen then begin beep; goto 7 end;
             if InsMode then begin
               if sLen=MaxLen then
                 if s[sLen]=' ' then dec(sLen) else begin beep; goto 7 end;
               move(s[pos],s[pos+1],sLen-pos+1); inc(sLen) end
             else if pos>sLen then inc(sLen);
             s[pos]:=char(KbdChar); inc(pos);
7:           end;
        else if ret and ((KbdChar<$20) or (KbdChar>=$100)) then begin
          Event.What:=evKeyDown; goto 8 end
        end;
        WriteStr;
    end end;
    ClrEvent; if not ret then goto 1;
8:  EditTxt:=pos;
end;



procedure WrPromptTxt(const S:string;Impl:FrmlPtr;F:FieldDPtr;
                      var Txt:string;var R:float);
var x,y,d,LWw:word; SS,T:string; RR:float; BB:boolean;
begin
  WrStyleStr(S,ProcAttr); T:=''; x:=WhereX; y:=WhereY; d:=WindMax.X-WindMin.X+1;
  if x+F^.L-1>d then LWw:=d-x else LWw:=F^.L;  TextAttr:=colors.dHili;
  if Impl<>nil then begin
    case F^.FrmlTyp of 'R':RR:=RunReal(Impl);
                       'S':SS:=RunShortStr(Impl); else BB:=RunBool(Impl) end;
    DecodeFieldRSB(F,F^.L,RR,SS,BB,T) end;
  GotoXY(x,y); FieldEdit(F,nil,LWw,1,T,R,true,true,false,0);
  TextAttr:=ProcAttr;
  if KbdChar=_ESC_ then begin EscPrompt:=true; writeln end
  else begin EscPrompt:=false; Txt:=T; T[0]:=char(LWw);
  gotoxy(x,y); writeln(T) end;
end;
function PromptB(const S:string;Impl:FrmlPtr;F:FieldDPtr):boolean;
  var Txt:string; R:float;
  begin WrPromptTxt(S,Impl,F,Txt,R);PromptB:=Txt[1]=AbbrYes;
     if KbdChar=_ESC_ then
     if Impl<>nil then PromptB:=RunBool(Impl) else PromptB:=false end;
function PromptS(const S:string;Impl:FrmlPtr;F:FieldDPtr):string;
  var Txt:string; R:float;
  begin WrPromptTxt(S,Impl,F,Txt,R);PromptS:=Txt;
     if KbdChar=_ESC_ then
     if Impl<>nil then PromptS:=RunShortStr(Impl) else PromptS:='' end;
function PromptR(const S:string;Impl:FrmlPtr;F:FieldDPtr):float;
  var Txt:string; R:float;
  begin WrPromptTxt(S,Impl,F,Txt,R);PromptR:=R;
     if KbdChar=_ESC_ then
     if Impl<>nil then PromptR:=RunReal(Impl) else PromptR:=0 end;


end.