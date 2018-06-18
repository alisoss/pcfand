procedure Error(N:integer);
var ErrMsg:string; HdTxt:string[40]; p,p1:pointer; upd:boolean;
    l,i:word; w:longint;
begin
  RdMsg(1000+N); ErrMsg:=MsgLine;
  if N=1 then
    if ExpChar>=' ' then ErrMsg:=ErrMsg+' '+ExpChar
    else begin
      case ExpChar of
       _assign:MsgLine:=':=';
       _addass:MsgLine:='+=';
       _equ:MsgLine:='=';
       _number:RdMsg(1004);
       _identifier:RdMsg(1005);
       _quotedstr:RdMsg(1013);
      end;
      ErrMsg:=ErrMsg+' '+MsgLine end;
  dec(CurrPos); ClearKbdBuf; l:=InpArrLen; i:=CurrPos;
  if IsTestRun and
     ((PrevCompInp<>nil) and (InpRdbPos.R<>CRdb) { $include higher Rdb}  or
      (InpRdbPos.R=ptr(0,1)) {LongStr + ShowErr})
     and (StoreAvail>l+TxtCols*TxtRows*2+50) then begin
    MarkStore(p1); w:=PushW(1,1,TxtCols,TxtRows); TextAttr:=colors.tNorm;
    p:=GetStore(l); move(InpArrPtr^,p^,l);
    if PrevCompInp<>nil then RdMsg(63) else RdMsg(61); HdTxt:=MsgLine;
    SimpleEditText('T',ErrMsg,HdTxt,p,$fff,l,i,upd);
    PopW(w); ReleaseStore(p1) end;
  EdRecKey:=ErrMsg; LastExitCode:=i+1;
  IsCompileErr:=true; MsgLine:=ErrMsg; GoExit;
end;

procedure SetInpStr(var S:string);
  begin InpArrLen:=length(S);InpArrPtr:=CharArrPtr(@S[1]);
        if InpArrLen=0 then ForwChar:=^z else ForwChar:=InpArrPtr^[1];
        CurrPos:=1; fillchar(InpRdbPos,sizeof(InpRdbPos),0);
  end;
procedure SetInpLongStr(S:LongStrPtr;ShowErr:boolean);
begin
  InpArrLen:=S^.LL; InpArrPtr:=CharArrPtr(@S^.A);
  if InpArrLen=0 then ForwChar:=^z else ForwChar:=InpArrPtr^[

  1];
  CurrPos:=1; InpRdbPos.R:=nil; if ShowErr then InpRdbPos.R:=ptr(0,1);
  InpRdbPos.IRec:=0;
end;
procedure SetInpTTPos(Pos:longint;Decode:boolean);
var s:LongStrPtr;
begin
  s:=CFile^.TF^.Read(2,Pos); if Decode then CodingLongStr(s);
  InpArrLen:=s^.LL; InpArrPtr:=CharArrPtr(@s^.A);
  if InpArrLen=0 then ForwChar:=^z else ForwChar:=InpArrPtr^[1];
  CurrPos:=1 end;
procedure SetInpTT(RP:RdbPos;FromTxt:boolean);
var Pos:longint;  CF:FileDPtr; CR:pointer; s:LongStrPtr;
begin
  if RP.IRec=0 then begin
    SetInpLongStr(RunLongStr(FrmlPtr(RP.R)),true); exit end;
  InpRdbPos:=RP; CF:=CFile; CR:=CRecPtr; CFile:=RP.R^.FD; CRecPtr:=GetRecSpace;
  ReadRec(RP.IRec);
  if FromTxt then Pos:=_t(ChptTxt) else Pos:=_t(ChptOldTxt);
  SetInpTTPos(Pos,RP.R^.Encrypted);
  ReleaseStore(CRecPtr); CFile:=CF; CRecPtr:=CR;
end;
procedure SetInpTTxtPos(FD:FileDPtr);
var pos:word; r:RdbDPtr;
begin
  SetInpTT(FD^.ChptPos,true); pos:=FD^.TxtPosUDLI; r:=FD^.ChptPos.R;
  if pos>InpArrLen then ForwChar:=^z else ForwChar:=InpArrPtr^[pos];
  CurrPos:=pos;
end;

{$S-}
procedure ReadChar;
begin
   CurrChar:=ForwChar;
   if CurrPos<InpArrLen then
     begin inc(CurrPos); ForwChar:=InpArrPtr^[CurrPos] end
   else if CurrPos=InpArrLen then begin inc(CurrPos);ForwChar:=^z end;
end;
{$S+}
function RdDirective(var b:boolean):word;
procedure RdForwName(var s:string);
begin
  s[0]:=#0; while (length(s)<12) and
    (IsLetter(ForwChar) or IsDigit(ForwChar)) do begin
    inc(s[0]); s[length(s)]:=ForwChar; ReadChar end;
end;
const Dirs:array[0..5] of string[7]=
     ('define','ifdef','ifndef','else','endif','include');
var i,j:word; s:string[12]; r:RdbDPtr; res:boolean;
label 1;
begin
  ReadChar; RdForwName(s);
  for i:=0 to 5 do if SEquUpcase(s,Dirs[i]) then goto 1; Error(158);
1:if i<=2 then begin
    while ForwChar=' ' do ReadChar;
    if i=0 then begin
      Switches[0]:=#0; while (length(Switches)<sizeof(Switches)-1) and
        (ForwChar in ['A'..'Z']) do begin
      inc(Switches[0]); Switches[length(Switches)]:=ForwChar; ReadChar end end
    else begin
      if not (ForwChar in ['A'..'Z']) then Error(158); ReadChar; b:=false;
      for j:=1 to length(Switches) do
        if Switches[j]=CurrChar then b:=true;
      if i=2 then b:=not b; i:=1 end end
  else if i=5 then begin
    while ForwChar=' ' do ReadChar; RdForwName(s);
    r:=CRdb; if (PtrRec(InpRdbPos.R).Seg<>0) then CRdb:=InpRdbPos.R;
    res:=FindChpt('I',s,false,ChptIPos); CRdb:=r;
    if not res then Error(37) end;
  if ForwChar<>'}' then Error(158); ReadChar; RdDirective:=i;
end;
procedure SkipLevel(withElse:boolean);
var begLevel,n:word; b:boolean;
label 1,2,3;
begin
  begLevel:=SwitchLevel;
1:case ForwChar of       { skip to directive }
   '''': repeat ReadChar until ForwChar in ['''',^z];
   '{':  begin ReadChar; if ForwChar='$' then goto 3; n:=1;
2:       case ForwChar of
          '{': inc(n); ^z:Error(11);
          '}':begin dec(n); if n=0 then begin ReadChar; goto 1 end end end;
         ReadChar; goto 2 end;
    ^z:  Error(11) end;
  ReadChar; goto 1;
3:case RdDirective(b) of
    1{if}  : inc(SwitchLevel);
    3{else}: if SwitchLevel=begLevel then if withElse then exit else Error(159);
    4{end} : begin if SwitchLevel=0 then Error(159); dec(SwitchLevel);
             if SwitchLevel<begLevel then exit end end;
  goto 1;
end;
procedure SkipBlank(toNextLine:boolean);
var CC:char; n:word; b:boolean; ci:CompInpDPtr;
label 1,2;
begin
  CC:=CurrChar;
1:case ForwChar of
   ^z:if PrevCompInp<>nil then begin
        ci:=PrevCompInp; move(ci^.ChainBack,PrevCompInp,sizeof(CompInpD));
        if CurrPos<=InpArrLen then ForwChar:=InpArrPtr^[CurrPos];
        goto 1 end;
   ^@..' ':
       if toNextLine and (ForwChar=^m) then begin
         ReadChar; if ForwChar=^j then ReadChar end
       else begin ReadChar; goto 1 end;
   '{':begin ReadChar;
       if ForwChar='$' then begin
          n:=RdDirective(b); case n of
            0:;
            1:begin inc(SwitchLevel); if not b then SkipLevel(true) end;
            5:begin ci:=GetStore2(sizeof(CompInpD));
               move(PrevCompInp,ci^,sizeof(CompInpD));
               PrevCompInp:=ci; SetInpTT(ChptIPos,true) end;
            else if SwitchLevel=0 then Error(159);
               if n=3 then SkipLevel(false) else dec(SwitchLevel) end;
          goto 1 end
       else begin n:=1;
     2:case ForwChar of
        '{': inc(n); ^z:Error(11);
        '}':begin dec(n); if n=0 then begin ReadChar; goto 1 end end end;
       ReadChar; goto 2 end end;
  end;
  CurrChar:=CC;
end;
procedure OldError(N:integer);
  begin CurrPos:=OldErrPos; Error(N) end;

procedure RdBackSlashCode;
var i,n:word; Num:string[3];
begin
  if ForwChar='\' then begin ReadChar; exit end;
  Num:=''; while (ForwChar in ['0'..'9']) and (length(Num)<3) do begin
    ReadChar; Num:=Num+CurrChar end;
  if Num='' then exit; val(Num,n,i);
  if n>255 then Error(7); CurrChar:=char(n);
end;
procedure RdLex;
  var i:word;
  begin OldErrPos:=CurrPos; SkipBlank(false); ReadChar; Lexem:=CurrChar;
    if IsLetter(CurrChar) then
      begin Lexem:=_identifier; LexWord[1]:=CurrChar; i:=1;
        while IsLetter(ForwChar) or IsDigit(ForwChar) do
        begin inc(i);if i>32 then Error(2);ReadChar;LexWord[i]:=CurrChar end;
        LexWord[0]:=char(i) end
    else if IsDigit(CurrChar) then
      begin Lexem:=_number; LexWord[1]:=CurrChar;i:=1;
        while IsDigit(ForwChar) do
        begin inc(i);if i>15 then Error(6);ReadChar;LexWord[i]:=CurrChar end;
        LexWord[0]:=char(i) end
    else case CurrChar of
      '''':
          begin Lexem:=_quotedstr;
              ReadChar;LexWord:='';
              while (CurrChar<>'''') or (ForwChar='''') do
              begin if CurrChar=^z then Error(17);
                    if length(LexWord)=sizeof(LexWord)-1 then Error(6);
                    if CurrChar='''' then ReadChar
                    else if CurrChar='\' then RdBackSlashCode;
                    LexWord:=LexWord+CurrChar; ReadChar end;
           end;
     ':':
         if ForwChar='=' then begin ReadChar;Lexem:=_assign end;
     '.':
         if ForwChar='.' then begin ReadChar;Lexem:=_subrange end;
     '=':
         if ForwChar='>' then begin ReadChar;Lexem:=_limpl end
         else Lexem:=_equ;
     '+':
         if ForwChar='=' then begin ReadChar;Lexem:=_addass end;
     '<':
         case ForwChar of
          '>':begin ReadChar;Lexem:=_ne end;
          '=':begin ReadChar;
                    if ForwChar='>' then
                       begin ReadChar;Lexem:=_lequ end
                    else Lexem:=_le;
              end;
          else Lexem:=_lt;
         end;
     '>':
         if ForwChar='=' then begin ReadChar;Lexem:=_ge end
         else Lexem:=_gt;
    end;
{   write('CurrChar:',CurrChar,'/Lexem:');
    case lexem of _quotedstr:write('quotedstr');_identifier:write('identifier');
              _number:write('number');_subrange:write('subrange');
              else write(Lexem) end;
    writeln('/LexWord:',LexWord,'/  ',CurrPos); }
  end;
{$S-}
function IsForwPoint:boolean;
  begin IsForwPoint:= (ForwChar='.') and (InpArrPtr^[CurrPos+1]<>'.') end;
procedure TestIdentif;
  begin if Lexem<>_identifier then Error(29) end;
procedure TestLex(X:char);
  begin if Lexem<>X then begin ExpChar:=X;Error(1) end end;
procedure Accept(X:char); assembler;
asm  mov al,X; cmp al,Lexem; je @1;
     mov ExpChar,al; mov ax,1; push ax; call Error;
@1:  call RdLex;
end;
{$S+}
function RdInteger:integer;
  var I,J:integer;
  begin Val(LexWord,I,J);if J<>0 then Lexem:=#0{<>_number};Accept(_number);
        RdInteger:=I;
  end;
function RdRealConst:float;
procedure ValofS(const S:string);
var I:integer; R:float;
begin
  Val(S,R,I); if I<>0 then begin
    R:=ValDate(S,'DD.MM.YY'); if R=0 then begin
      R:=ValDate(S,'DD.MM.YYYY'); if R=0 then begin
        R:=ValDate(S,'hh:mm:ss.tt'); if R=0 then Error(7) end end end;
  RdRealConst:=R;
end;
var S:string;
label 1;
begin
  if Lexem='-' then begin S:='-'; RdLex end else S:='';
  TestLex(_number); S:=S+LexWord;
1:if (ForwChar in ['.',':']) then begin
    RdLex; if (Lexem<>_subrange) and (ForwChar in ['0'..'9']) then begin
      S:=S+Lexem; RdLex; S:=S+LexWord; goto 1 end;
    ValofS(S); exit  end;
  if (ForwChar in ['E','e']) and (InpArrPtr^[CurrPos+1] in ['-','0'..'9']) then begin
    S:=S+'e'; ReadChar; if ForwChar='-' then begin ReadChar; S:=S+'-' end;
    RdLex; TestLex(_number); S:=S+LexWord end;
  RdLex; ValofS(S);
end;
function EquUpcase(const S:string):boolean; assembler;
asm  lea si,LexWord; les di,S; cld; xor ch,ch; mov cl,[si]; cmpsb; jnz @3;
     jcxz @2; xor bh,bh;
@1:  mov bl,[si]; mov al,BYTE PTR UpcCharTab[bx]; mov bl,es:[di];
     cmp al,BYTE PTR UpcCharTab[bx]; jnz @3; inc si; inc di; loop @1;
@2:  mov ax,1; jmp @4;
@3:  xor ax,ax;
@4:  end;
function TestKeyWord(const S:string):boolean;
  begin TestKeyWord:=(Lexem=_identifier)and EquUpcase(S) end;
function IsKeyWord(const S:string):boolean; assembler;  { S=uppercase}
asm  cmp Lexem,_identifier; jne @3;
     lea si,LexWord; les di,S; cld; xor ch,ch; mov cl,[si]; cmpsb; jnz @3;
     jcxz @2; xor bh,bh;
@1:  mov bl,[si]; mov al,BYTE PTR UpcCharTab[bx]; cmp al,es:[di]; jnz @3;
     inc si; inc di; loop @1;
@2:  call RdLex; mov ax,1; jmp @4;
@3:  xor ax,ax;
@4:  end;
procedure AcceptKeyWord(const S:string);
  begin if TestKeyWord(S) then RdLex else
        begin SetMsgPar(S);Error(33) end end;
function IsOpt(const S:string):boolean; assembler; {S=uppercase}
asm  cmp Lexem,_identifier; jne @3;
     lea si,LexWord; les di,S; cld; xor ch,ch; mov cl,[si]; cmpsb; jnz @3;
     jcxz @2; xor bh,bh;
@1:  mov bl,[si]; mov al,BYTE PTR UpcCharTab[bx]; cmp al,es:[di]; jnz @3;
     inc si; inc di; loop @1;
@2:  call RdLex; mov ax,_equ; push ax; call Accept; mov ax,1; jmp @4;
@3:  xor ax,ax;
@4:  end;
function IsDigitOpt(const S:string;var N:word):boolean;
begin
  if (Lexem=_identifier) and (length(Lexword)=length(S)+1) and
     SEquUpcase(copy(Lexword,1,length(S)),S)
     and (LexWord[length(LexWord)] in ['0'..'9']) then begin
    N:=ord(LexWord[length(LexWord)])-ord('0'); RdLex;
    Accept(_equ); IsDigitOpt:=true end
  else IsDigitOpt:=false;
end;
function RdStrConst:StringPtr;
  var S:StringPtr;
  begin S:=StoreStr(LexWord); Accept(_quotedstr); RdStrConst:=S end;
function Rd1Char:char;
  begin if (Lexem<>_identifier) or (length(LexWord)<>1) then Error(124);
        Rd1Char:=LexWord[1]; RdLex end;
function RdQuotedChar:char;
  begin if (Lexem<>_quotedstr) or (length(LexWord)<>1) then Error(15);
        RdQuotedChar:=LexWord[1]; RdLex end;
function IsIdentifStr(const S:string):boolean;
var i:word;
begin
  IsIdentifStr:= false;
  if (length(S)=0) or not IsLetter(S[1]) then exit;
  for i:=2 to length(S) do
    if not(IsLetter(S[i]) or IsDigit(S[i])) then exit;
  IsIdentifStr:=true;
end;