procedure TestBool(FTyp:char);
  begin if FTyp<>'B' then OldError(18) end;
procedure TestString(FTyp:char);
  begin if FTyp<>'S' then OldError(19) end;
procedure TestReal(FTyp:char);
  begin if FTyp<>'R' then OldError(20) end;
function RdFormula(var FTyp:char):FrmlPtr;forward;
function RdAdd(var FTyp:char):FrmlPtr;forward;
function RdPrim(var FTyp:char):FrmlPtr;
function FindFuncD(var ZZ:FrmlPtr):boolean;
var fc:FuncDPtr; fl:FrmlList; typ:char; i,n:word; z:FrmlPtr; lv:LocVarPtr;
begin
  FindFuncD:=false; fc:=FuncDRoot;
  while fc<>nil do begin
    if EquUpcase(fc^.Name) then begin
      RdLex; RdLex; z:=GetOp(_userfunc,8); z^.FC:=fc;
      lv:=fc^.LVB.Root; n:=fc^.LVB.NParam;
      for i:=1 to n do begin
        fl:=GetStore(sizeof(fl^)); ChainLast(z^.FrmlL,fl);
        fl^.Frml:=RdFormula(typ); if typ<>lv^.FTyp then OldError(12);
        lv:=lv^.Chain; if i<n then Accept(',') end;
      Accept(')');
      ZZ:=z; FindFuncD:=true; exit end;
    fc:=fc^.Chain end;
end;

 const MaxLen=9;
function IsFun(var XFun;N:byte;var XCode;var FunCode:char):boolean; assembler;
asm  les bx,XFun; lea dx,LexWord[1]; mov ch,LexWord.byte; xor cl,cl; cld;
@1:  mov ah,es:[bx]; cmp ah,ch; jne @4; mov si,dx; mov di,bx; inc di;
@2:  lodsb; cmp al,41H; jb @3; cmp al,5AH; ja @3; add al,20H; { lowercase }
@3:  cmp al,es:[di]; jb @5; ja @4; inc di; dec ah; jnz @2; jmp @6;
@4:  add bx,MaxLen+1; inc cl; cmp cl,N; jb @1;              { next string }
@5:  mov ax,0; jmp @7;                                      { not found }
@6:  xor ch,ch; mov bx,cx; les di,XCode; mov al,es:[di+bx];  { found }
     les di,FunCode; mov es:[di],al;
     call RdLex; mov ax,1;
@7:
end;
const R0FunN=16;          { alphabetic ordered lower case names }
      R0Fun:array[1..R0FunN]of string[MaxLen]=('cprinter','currtime',
               'edrecno','exitcode','getmaxx','getmaxy',
               'maxcol','maxrow','memavail','mousex','mousey',
               'pi','random','today','txtpos','txtxy');
      R0Code:array[1..R0FunN]of char=(_cprinter,_currtime,
               _edrecno,_exitcode,_getmaxx,_getmaxy,
               _maxcol,_maxrow,_memavail,_mousex,_mousey,
               _pi,_random,_today,_txtpos,_txtxy);
      RCFunN=5;
      RCFun:array[1..RCFunN]of string[MaxLen]=('edbreak','edirec',
           'menux','menuy','usercode');
      RCCode:array[1..RCFunN]of char=(#3,#4,#5,#6,#7);
      S0FunN=12;
      S0Fun:array[1..S0FunN]of string[MaxLen]=('accright','clipbd','edbool','edfield',
            'edfile','edkey','edreckey','keybuf',
            'password','readkey','username','version');
      S0Code:array[1..S0FunN]of char=(_accright,_clipbd,_edbool,_edfield,
            _edfile,_edkey,_edreckey,_keybuf,_password,_readkey,_username,_version);
      B0FunN=2;
      B0Fun:array[1..B0FunN]of string[MaxLen]=('isnewrec','testmode');
      B0Code:array[1..B0FunN]of char=(_isnewrec,_testmode);
      S1FunN=5;
      S1Fun:array[1..S1FunN]of string[MaxLen]=('char','getenv',
                'lowcase','nodiakr','upcase');
      S1Code:array[1..S1FunN]of char=(_char,_getenv,_lowcase,_nodiakr,_upcase);
      R1FunN=12;
      R1Fun:array[1..R1FunN]of string[MaxLen]=('abs','arctan','color','cos','exp','frac',
                                'int','ln','sin','sqr','sqrt','typeday');
      R1Code:array[1..R1FunN]of char=(_abs,_arctan,_color,_cos,_exp,_frac,
                               _int,_ln,_sin,_sqr,_sqrt,_typeday);
      R2FunN=4;
      R2Fun:array[1..R2FunN]of string[MaxLen]=('addmonth','addwdays','difmonth',
                               'difwdays');
      R2Code:array[1..R2FunN]of char=(_addmonth,_addwdays,_difmonth,_difwdays);
      RS1FunN=5;
      RS1Fun:array[1..RS1FunN]of string[MaxLen]=
                                  ('diskfree','length','linecnt','ord','val');
      RS1Code:array[1..RS1FunN]of char=(_diskfree,_length,_linecnt,_ord,_val);
      S3FunN=3;
      S3Fun:array[1..S3FunN]of string[MaxLen]=('copy','str','text');
      S3Code:array[1..S3FunN]of char=(_copy,_str,_str);
  var FunCode:char;
      Z,Z1,Z2,Z3:FrmlPtr; Typ:char;
      I,N:integer; B:BytePtr;
      Options:string[5];
  label 0,1,2,3,4,5,6,7,8;
  begin
    case Lexem of
     _identifier:begin SkipBlank(false);
          if IsFun(R0Fun,R0FunN,R0Code,FunCode) then
             begin Z:=GetOp(FunCode,0);FTyp:='R' end
          else if IsFun(RCFun,RCFunN,RCCode,FunCode) then
             begin Z:=GetOp(_getwordvar,1);Z^.N01:=ord(FunCode);FTyp:='R' end
          else if IsFun(S0Fun,S0FunN,S0Code,FunCode) then
            begin Z:=GetOp(FunCode,0);FTyp:='S' end
          else if IsFun(B0Fun,B0FunN,B0Code,FunCode) then
            begin Z:=GetOp(FunCode,0);FTyp:='B' end
          else if IsKeyWord('TRUE') then
             begin Z:=GetOp(_const,1);Z^.B:=true;FTyp:='B' end
          else if IsKeyWord('FALSE') then
             begin Z:=GetOp(_const,1);FTyp:='B' end
          else if not EquUpcase('OWNED') and (ForwChar='(') then
           if FindFuncD(Z) then FTyp:=Z^.FC^.FTyp
           else if IsFun(S3Fun,S3FunN,S3Code,FunCode) then
            begin RdLex; Z:=GetOp(FunCode,0); Z^.P1:=RdAdd(FTyp);
               if FunCode=_copy then TestString(FTyp)
               else begin TestReal(FTyp); FTyp:='S' end;
               Accept(','); Z^.P2:=RdAdd(Typ);
               if (FunCode=_str) and (Typ='S') then goto 0;
               TestReal(Typ); Accept(',');
               Z^.P3:=RdAdd(Typ);TestReal(Typ);
0:               Accept(')');
             end
           else if IsKeyWord('COND') then
             begin
                RdLex;Z2:=nil;
1:              Z:=GetOp(_cond,0);
                if not IsKeyWord('ELSE') then
                  begin Z^.P1:=RdFormula(Typ);TestBool(Typ) end;
                Accept(':');
                Z^.P2:=RdAdd(Typ);
                if Z2=nil then
                   begin Z1:=Z;FTyp:=Typ;if Typ='B' then OldError(70) end
                else begin Z2^.P3:=Z;
                      if FTyp='S' then TestString(Typ) else TestReal(Typ) end;
                if (Z^.P1<>nil) and (Lexem=',') then
                   begin RdLex;Z2:=Z;goto 1 end;
                Accept(')'); Z:=Z1;
             end
           else if IsKeyWord('MODULO') then
            begin RdLex; Z1:=RdAdd(Typ); TestString(Typ);
              Z:=GetOp(_modulo,2); Z^.P1:=Z1; N:=0;
              repeat Accept(',');B:=GetStore(2);WordPtr(B)^:=RdInteger;
                     inc(N);
              until Lexem<>',';
              Accept(')');Z^.W11:=N;FTyp:='B';
            end
           else if IsKeyWord('SUM') then
            begin RdLex;if FrmlSumEl<>nil then OldError(74);
               if @ChainSumEl=nil then Error(28);
               FrmlSumEl:=GetStore(SizeOf(SumElem));
               FrstSumVar:=True; FrmlSumEl^.Op:=_const; FrmlSumEl^.R:=0;
               FrmlSumEl^.Frml:=RdAdd(FTyp); TestReal(FTyp);
               Accept(')');Z:=FrmlPtr(@FrmlSumEl^.Op);
               ChainSumEl; FrmlSumEl:=nil;
            end
           else if IsKeyWord('DTEXT') then
            begin RdLex; Z1:=RdAdd(Typ);TestReal(Typ);
               Accept(','); TestLex(_identifier);
               for I:= 1 to Length(LexWord) do LexWord[I]:=upcase(LexWord[I]);
               goto 2 end
           else if IsKeyWord('STRDATE') then
            begin RdLex; Z1:=RdAdd(Typ);TestReal(Typ);
               Accept(','); TestLex(_quotedstr);
2:             Z:=GetOp(_strdate,length(LexWord)+1);Z^.P1:=Z1;
               Z^.Mask:=LexWord;RdLex;
               Accept(')');FTyp:='S'  end
           else if IsKeyWord('DATE') then
            begin RdLex;Z:=GetOp(_valdate,9);Z^.Mask:='DD.MM.YY';
                  goto 3 end
           else if IsKeyWord('VALDATE') then
            begin RdLex;Z1:=RdAdd(Typ);TestString(Typ);
               Accept(','); TestLex(_quotedstr);
               Z:=GetOp(_valdate,length(LexWord)+1);Z^.P1:=Z1;
               Z^.Mask:=LexWord;RdLex;goto 4 end
           else if IsKeyWord('REPLACE') then begin
             RdLex; Z1:=RdAdd(Typ);TestString(Typ);
             Accept(','); Z2:=RdAdd(Typ);TestString(Typ); Accept(',');
             Z3:=RdAdd(FTyp);TestString(FTyp);FunCode:=_replace; goto 8 end
           else if IsKeyWord('POS') then begin
             RdLex; Z1:=RdAdd(Typ); TestString(Typ); Accept(',');
             Z2:=RdAdd(Typ); TestString(Typ);
             Z3:=nil; FunCode:=_pos; FTyp:='R';
8:           Options:=''; if Lexem=',' then begin RdLex;
                   if Lexem<>',' then
                     begin TestLex(_quotedstr); Options:=LexWord; RdLex end;
                   if (FunCode=_pos) and (Lexem=',') then
                     begin RdLex; Z3:=RdAdd(Typ); TestReal(Typ) end end;
               Z:=GetOp(FunCode,length(Options)+1);
               Z^.P1:=Z1; Z^.P2:=Z2; Z^.P3:=Z3; Z^.Options:=Options;
               Accept(')') end
           else if IsFun(RS1Fun,RS1FunN,RS1Code,FunCode) then
            begin RdLex;Z:=GetOp(FunCode,0);
3:                Z^.P1:=RdAdd(Typ);TestString(Typ); goto 4 end
           else if IsFun(R1Fun,R1FunN,R1Code,FunCode) then
            begin RdLex; Z:=GetOp(FunCode,0); Z^.P1:=RdAdd(Typ); TestReal(Typ);
4:                FTyp:='R'; Accept(')') end
           else if IsFun(R2Fun,R2FunN,R2Code,FunCode) then
            begin RdLex;Z:=GetOp(FunCode,1);Z^.P1:=RdAdd(Typ);TestReal(Typ);
                  Accept(',');Z^.P2:=RdAdd(Typ);TestReal(Typ);
                  if (Z^.Op in [_addwdays,_difwdays]) and (Lexem=',') then
                    begin RdLex;Z^.N21:=RdInteger;if Z^.N21>3 then OldError(136) end;
                  goto 4 end
           else if IsKeyWord('LEADCHAR') then
            begin Z:=GetOp(_leadchar,2);goto 5 end
           else if IsKeyWord('TRAILCHAR') then
            begin Z:=GetOp(_trailchar,2);
5:                RdLex; Z^.N11:=ord(RdQuotedChar); Accept(',');
                  Z^.P1:=RdAdd(Typ);TestString(Typ); if Lexem=',' then begin
                    RdLex;Z^.N12:=ord(RdQuotedChar) end;
                  goto 6 end
           else if IsKeyWord('COPYLINE') then
            begin Z:=GetOp(_copyline,0);goto 7 end
           else if IsKeyWord('REPEATSTR') then
            begin Z:=GetOp(_repeatstr,0);
7:                RdLex; Z^.P1:=RdAdd(Typ);TestString(Typ); Accept(',');
                  Z^.P2:=RdAdd(Typ);TestReal(Typ);
                  if (Lexem=',') and (Z^.Op=_copyline) then begin
                    RdLex; Z^.P3:=RdAdd(Typ);TestReal(Typ) end;
                  goto 6 end
           else if IsFun(S1Fun,S1FunN,S1Code,FunCode) then
            begin Z:=GetOp(FunCode,0); RdLex; Z^.P1:=RdAdd(FTyp);
                  if FunCode=_char then TestReal(FTyp) else TestString(FTyp);
6:                FTyp:='S'; Accept(')') end
           else if IsKeyWord('TRUST') then begin
             Z:=GetOp(_trust,0);RdByteListInStore;FTyp:='B' end
           else if IsKeyWord('EQUMASK') then begin
             Z:=GetOp(_equmask,0); FTyp:='B'; RdLex;
             Z^.P1:=RdAdd(Typ);TestString(Typ); Accept(',');
             Z^.P2:=RdAdd(Typ);TestString(Typ); Accept(')') end
           else if @RdFunction<>nil then Z:=RdFunction(FTyp)
           else Error(75)
         else begin if @RdFldNameFrml=nil then Error(110);
           Z:=RdFldNameFrml(FTyp);
           if (Z^.Op<>_access) or (Z^.LD<>nil) then FrstSumVar:=False end;
         end;
     '^':begin RdLex;Z:=GetOp(_lneg,0);Z^.P1:=RdPrim(FTyp);TestBool(FTyp) end;
     '(':begin RdLex;Z:=RdFormula(FTyp);Accept(')') end;
     '-':begin RdLex; if Lexem='-' then Error(7); Z:=GetOp(_unminus,0);
               Z^.P1:=RdPrim(FTyp); TestReal(FTyp) end;
     '+':begin RdLex; if Lexem='+' then Error(7); Z:=RdPrim(FTyp);
               TestReal(FTyp) end;
     _quotedstr:
           begin Z:=GetOp(_const,length(LexWord)+1);
                 FTyp:='S'; Z^.S:=LexWord; RdLex end;
     else FTyp:='R';Z:=GetOp(_const,SizeOf(float));
     Z^.R:=RdRealConst;
    end;
    RdPrim:=Z;
  end;
const QQdiv:string[3]='div';
      QQmod:string[3]='mod';
      QQround:string[5]='round';
function RdMult(var FTyp:char):FrmlPtr;
  var Z,Z1:FrmlPtr; N:word;
  label 1,2;
  begin
      Z:=RdPrim(FTyp);
    1:Z1:=Z;
      case Lexem of
       '*':begin Z:=GetOp(_times,0);goto 2 end;
       '/':begin Z:=GetOp(_divide,0);
               2:TestReal(FTyp);RdLex;Z^.P1:=Z1;Z^.P2:=RdPrim(FTyp);;
                 TestReal(FTyp); goto 1 end;
       _identifier:if EquUpcase(QQdiv) then begin Z:=GetOp(_div,0);goto 2 end
           else if EquUpcase(QQmod) then begin Z:=GetOp(_mod,0);goto 2 end
           else if EquUpcase(QQround) then
           begin TestReal(FTyp); Z:=GetOp(_round,0); RdLex;
                 Z^.P1:=Z1;
                 Z^.P2:=RdPrim(FTyp);
                 TestReal(FTyp) end;
      end;
      RdMult:=Z;
  end;
function RdAdd(var FTyp:char):FrmlPtr;
  var Z,Z1:FrmlPtr;
  label 1,2;
  begin
      Z:=RdMult(FTyp);
    1:case Lexem of
       '+':begin Z1:=Z;
              if FTyp='R' then begin Z:=GetOp(_plus,0);goto 2 end
              else begin Z:=GetOp(_concat,0);TestString(FTyp);RdLex;Z^.P1:=Z1;
                 Z^.P2:=RdMult(FTyp);TestString(FTyp);goto 1 end;
           end;
       '-':begin Z1:=Z;Z:=GetOp(_minus,0);TestReal(FTyp);
            2:RdLex;Z^.P1:=Z1;Z^.P2:=RdMult(FTyp);TestReal(FTyp);goto 1 end;
      end;
      RdAdd:=Z;
  end;
function RdComp(var FTyp:char):FrmlPtr;
  var Z:FrmlPtr; R:float; S:string;
function RdPrecision:word;
  var n:word;
  begin n:=5; if (Lexem='.') and (ForwChar in ['0'..'9']) then
           begin RdLex;n:=RdInteger; if n>10 then OldError(21) end;
        RdPrecision:=n end;
function RdTilde:word;
  begin if Lexem='~' then begin RdLex;RdTilde:=1 end else RdTilde:=0 end;
procedure RdInConst;
  begin if FTyp='S' then
          begin if Z^.N11=1{tilde} then S:=TrailChar(' ',LexWord)
                else S:=LexWord;  Accept(_quotedstr) end
        else R:=RdRealConst;
  end;
procedure StoreConst;
  var RPtr:FloatPtr;
  begin case FTyp of
     'S':StoreStr(S);
     'R':begin RPtr:=GetStore(SizeOf(RPtr^)); RPtr^:=R end;
     end;
  end;
  var B:^byte; N:integer; Z1:FrmlPtr;
  label 1;
  begin Z:=RdAdd(FTyp);Z1:=Z;
    if Lexem in [_equ.._ne] then
        if FTyp='R' then
           begin Z:=GetOp(_compreal,2);Z^.P1:=Z1;
                 Z^.N21:=ord(Lexem);RdLex;Z^.N22:=RdPrecision;
                 Z^.P2:=RdAdd(FTyp);TestReal(FTyp);FTyp:='B' end
        else begin TestString(FTyp);Z:=GetOp(_compstr,2);Z^.P1:=Z1;
                 Z^.N21:=ord(Lexem);RdLex; Z^.N22:=RdTilde;
                 Z^.P2:=RdAdd(FTyp);TestString(FTyp);FTyp:='B' end
    else if (Lexem=_identifier) and IsKeyWord('IN') then
      begin if FTyp='R' then
               begin Z:=GetOp(_inreal,1);Z^.N11:=RdPrecision end
            else begin TestString(FTyp);Z:=GetOp(_instr,1);Z^.N11:=RdTilde end;
            Z^.P1:=Z1;Accept('[');N:=0;
          1:RdInConst;
            if Lexem=_subrange then
              begin if N<>0 then begin B^:=N;N:=0 end;
                    B:=GetStore(sizeof(B^));B^:=$FF;StoreConst;
                    RdLex; RdInConst; StoreConst;
              end
            else begin if N=0 then B:=GetStore(sizeof(B^));
                       inc(N); StoreConst end;
            if Lexem<>']' then begin Accept(',');goto 1 end;
            RdLex;
            if N<>0 then B^:=N;B:=GetStore(sizeof(B^));B^:=0;
            FTyp:='B';
      end;               
    RdComp:=Z;
  end;
function BOperation(Typ,Fun:char;Frml:FrmlPtr):FrmlPtr;
  var Z:FrmlPtr;
  begin TestBool(Typ);Z:=GetOp(Fun,0);
        RdLex;Z^.P1:=Frml;BOperation:=Z end;
function RdBAnd(var FTyp:char):FrmlPtr;
  var Z:FrmlPtr;
  begin Z:=RdComp(FTyp);
       while Lexem='&' do
       begin Z:=BOperation(FTyp,_and,Z);Z^.P2:=RdComp(FTyp);TestBool(FTyp) end;
       RdBAnd:=Z;
  end;
function RdBOr(var FTyp:char):FrmlPtr;
  var Z:FrmlPtr;
  begin Z:=RdBAnd(FTyp);
        while Lexem='|' do
        begin Z:=BOperation(FTyp,_or,Z);Z^.P2:=RdBAnd(FTyp);TestBool(FTyp) end;
        RdBOr:=Z;
  end;
function RdFormula(var FTyp:char):FrmlPtr;
  var Z:FrmlPtr;
  begin
       Z:=RdBOr(FTyp);
       while Lexem in [_limpl,_lequ] do
       begin Z:=BOperation(FTyp,Lexem,Z);Z^.P2:=RdBOr(FTyp);TestBool(FTyp) end;
       RdFormula:=Z;
  end;

function RdKeyInBool(var KIRoot:KeyInDPtr;NewMyBP,FromRdProc:boolean;var SQLFilter:boolean):FrmlPtr;
function MyBPContext(Z:FrmlPtr;NewMyBP:boolean):FrmlPtr;
var Z1:FrmlPtr;
begin if NewMyBP then begin
    Z1:=GetOp(_setmybp,0); Z1^.P1:=Z; Z:=Z1 end;
  MyBPContext:=Z;
end;
function RdFL(NewMyBP:boolean;FL1:FrmlList):FrmlList;
var FTyp:char; KF,KF2:KeyFldDPtr; FL,FLRoot:FrmlList; FVA,b:boolean;
label 1;
begin
  KF:=CViewKey^.KFlds; FLRoot:=nil; KF2:=KF^.Chain;
  FVA:=FileVarsAllowed; FileVarsAllowed:=false; b:=FL1<>nil;
  if KF2<>nil then Accept('(');
1:FL:=GetStore(sizeof(FL^)); ChainLast(FLRoot,FL);
  FL^.Frml:=MyBPContext(RdFrml(FTyp),NewMyBP);
  if FTyp<>KF^.FldD^.FrmlTyp then OldError(12); KF:=KF^.Chain;
  if b then begin
    FL1:=FL1^.Chain; if FL1<>nil then begin Accept(','); goto 1 end end
  else if (KF<>nil) and (Lexem=',') then begin RdLex; goto 1 end;
  if KF2<>nil then Accept(')'); RdFL:=FLRoot;
  FileVarsAllowed:=FVA;
end;
var KI:KeyInDPtr; l:word; FTyp:char; Z:FrmlPtr; FVA:boolean;
label 1,2;
begin
  RdKeyInBool:=nil; KIRoot:=nil; SQLFilter:=false;
  if FromRdProc then begin
    FVA:=FileVarsAllowed; FileVarsAllowed:=true;
    if (Lexem=_identifier) and (ForwChar='(') and
       (EquUpcase('EVALB') or EquUpcase('EVALS') or EquUpcase('EVALR')) then
       FileVarsAllowed:=false end;
  if IsKeyWord('KEY') then begin
    AcceptKeyWord('IN');
    if (CFile^.Typ<>'X') or (CViewKey=nil) then OldError(118);
    if CViewKey^.KFlds=nil then OldError(176);
    Accept('['); l:=CViewKey^.IndexLen+1;
1:  KI:=GetZStore(sizeof(KeyInD)); ChainLast(KIRoot,KI);
    KI^.X1:=GetZStore(l); KI^.X2:=GetZStore(l+1);
    KI^.FL1:=RdFL(NewMyBP,nil); if Lexem=_subrange then begin
      RdLex; KI^.FL2:=RdFL(NewMyBP,KI^.FL1) end;
    if Lexem=',' then begin RdLex; goto 1 end;
    Accept(']');
    if Lexem='&' then begin RdLex; goto 2 end end
  else begin
2:  FrmlSumEl:=nil; Z:=RdFormula(FTyp);
    if CFile^.typSQLFile and (FTyp='S') then SQLFilter:=true
    else begin TestBool(FTyp);
      if Z^.Op=_eval then Z^.EvalFD:=CFile end;
    RdKeyInBool:=MyBPContext(Z,NewMyBP and (Z^.Op<>_eval)) end;
  if FromRdProc then FileVarsAllowed:=FVA;
end;

{$S-}
function RdFrml(var FTyp:char):FrmlPtr;
  begin FrmlSumEl:=nil;RdFrml:=RdFormula(FTyp) end;
function RdBool:FrmlPtr;
  var FTyp:char;
  begin FrmlSumEl:=nil;RdBool:=RdFormula(FTyp);TestBool(FTyp) end;
function RdRealFrml:FrmlPtr;
  var FTyp:char;
  begin FrmlSumEl:=nil;RdRealFrml:=RdAdd(FTyp);TestReal(FTyp) end;
function RdStrFrml:FrmlPtr;
  var FTyp:char;
  begin FrmlSumEl:=nil;RdStrFrml:=RdAdd(FTyp);TestString(FTyp) end;
{$S+}