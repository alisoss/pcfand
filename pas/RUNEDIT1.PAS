function CRec:longint;
  begin CRec:=BaseRec+IRec-1 end;
function CNRecs:longint;
var n:longint;
begin
  if EdRecVar then begin CNRecs:=1; exit end;
  if Subset then n:=WK^.NRecs else if HasIndex then n:=VK^.NRecs
  else n:=CFile^.NRecs;
  if IsNewRec then inc(n); CNRecs:=n;
end;
function AbsRecNr(N:longint):longint;
var md:LockMode;
begin
  if EdRecVar {$ifdef FandSQL} or CFile^.IsSQLFile {$endif} then begin
    if IsNewRec then AbsRecNr:=0 else AbsRecNr:=1; exit end;
  if IsNewRec then begin
    if (N=CRec) and (N=CNRecs) then begin AbsRecNr:=0; exit end;
    if N>CRec then dec(N) end;
  if SubSet then N:=WK^.NrToRecNr(N) else if HasIndex then begin
    md:=NewLMode(RdMode); TestXFExist; N:=VK^.NrToRecNr(N); OldLMode(md) end;
  AbsRecNr:=N;
end;
function LogRecNo(N:longint):longint;
var md:LockMode;
begin
  LogRecNo:=0; if (N<=0) or (N>CFile^.NRecs) then exit; md:=NewLMode(RdMode);
  ReadRec(N);
  if not DeletedFlag then
    if SubSet then LogRecNo:=WK^.RecNrToNr(N)
    else if HasIndex then begin TestXFExist; LogRecNo:=VK^.RecNrToNr(N) end
    else LogRecNo:=N;
  OldLMode(md);
end;
function IsSelectedRec(I:word):boolean;
var x:XString; cr:pointer; n:longint;
begin
  IsSelectedRec:=false; if (E^.SelKey=nil) or (I=IRec) and IsNewRec then exit;
  n:=AbsRecNr(BaseRec+I-1); cr:=CRecPtr;
  if (I=IRec) and WasUpdated then CRecPtr:=E^.OldRecPtr;
  IsSelectedRec:=E^.SelKey^.RecNrToPath(x,n); CRecPtr:=cr;
end;
function EquOldNewRec:boolean;
begin
  EquOldNewRec:=(CompArea(CRecPtr,E^.OldRecPtr,CFile^.RecLen)=ord(_equ));
end;

procedure RdRec(N:longint);
var md:LockMode; x:XString;
begin
  if EdRecVar then exit;  {$ifdef FandSQL}
  if CFile^.IsSQLFile then begin
    if IsNewRec and (N>CRec) then dec(N); x.S:=WK^.NrToStr(N);
    Strm1^.KeyAcc(WK,@x) end else {$endif}
  begin md:=NewLMode(RdMode);ReadRec(AbsRecNr(N));OldLMode(md) end;
end;


function CheckOwner(E:EditDPtr):boolean; forward;
function CheckKeyIn(E:EditDPtr):boolean; forward;
procedure DisplRec(I:word); forward;
procedure DisplRecNr(N:longint); forward;

function ELockRec(E:EditDPtr;N:longint;IsNewRec,Subset:boolean):boolean;
var md:LockMode;
label 1;
begin
  ELockRec:=true; if E^.IsLocked then exit; E^.LockedRec:=N;
  if IsNewRec then exit;
  if not E^.EdRecVar {$ifdef FandSQL} and not CFile^.IsSQLFile {$endif} then
  if CFile^.NotCached then begin
    if not TryLockN(N,1{withESC}) then begin ELockRec:=false; exit end;
    md:=NewLMode(RdMode); ReadRec(N); OldLMode(md);
    if Subset and not
       ((NoCondCheck or RunBool(E^.Cond) and CheckKeyIn(E)) and CheckOwner(E)) then begin
      WrLLF10Msg(150); goto 1 end end
  else if DeletedFlag then begin WrLLF10Msg(148);
1:    UnLockN(N); ELockRec:=false; exit end;
  E^.IsLocked:=true;
end;

function LockRec(Displ:boolean):boolean;
var b:boolean;
begin
  if E^.IsLocked then begin LockRec:=true; exit end;
  b:=ELockRec(E,AbsRecNr(CRec),IsNewRec,Subset);
  LockRec:=b;
  if b and not IsNewRec and not EdRecVar and CFile^.NotCached and Displ then
    DisplRec(IRec);
end;
procedure UnLockRec(E:EditDPtr);
begin with E^ do begin
  if FD^.IsShared and IsLocked and not EdRecVar then UnLockN(LockedRec);
  IsLocked:=false;
end end;

procedure NewRecExit;
var X:EdExitDPtr;
begin
  X:=E^.ExD; while X<>nil do begin if X^.AtNewRec then begin
      EdBreak:=18; LastTxtPos:=-1; StartExit(X,false) end;
    X:=X^.Chain end;
end;




procedure SetWasUpdated;
begin
  if not WasUpdated then begin
  if EdRecVar then SetUpdFlag;
  Move(E^.NewRecPtr^,E^.OldRecPtr^,CFileRecSize); WasUpdated:=true end end;
procedure SetCPage;
  var i:word;
  begin CPage:=CFld^.Page;
        RT:=ERecTxtDPtr(@E^.RecTxt);for i:=1 to CPage do RT:=RT^.Chain end;
procedure AdjustCRec;
begin
  if CRec<=CNRecs then exit; while CRec>CNRecs do begin
    if IRec>1 then dec(IRec) else dec(BaseRec) end;
  if BaseRec=0 then begin
    BaseRec:=1;
    if not IsNewRec then begin
      IsNewRec:=true; Append:=true; FirstEmptyFld:=CFld; ZeroAllFlds;
      SetWasUpdated; NewRecExit end
    else SetWasUpdated;
    NewDisplLL:=true end;
  UnlockRec(E); LockRec(false); DisplRecNr(CRec);
end;

procedure WrEStatus;
begin
  E^.CFld:=CFld;
  Move(FirstEmptyFld,E^.FirstEmptyFld,ofs(SelMode)-ofs(FirstEmptyFld)+1) end;
procedure RdEStatus;
var md:LockMode;
begin
  Move(E^.FirstEmptyFld,FirstEmptyFld,ofs(SelMode)-ofs(FirstEmptyFld)+1);
  if VK=nil then OnlySearch:=false;
  CFile:=E^.FD; CRecPtr:=E^.NewRecPtr; CFld:=E^.CFld;
  if CFile^.XF<>nil then HasIndex:=true else HasIndex:=false;
  if CFile^.TF<>nil then HasTF:=true else HasTF:=false;
  SetCPage;
end;



procedure AssignFld(F:FieldDPtr;Z:FrmlPtr);
begin
  SetWasUpdated; AssgnFrml(F,Z,false,false);
end;
procedure DuplFld(FD1,FD2:FileDPtr;RP1,RP2,RPt:pointer;F1,F2:FieldDPtr);
var ss:LongStrPtr; s:string; r:float; b:boolean; cf:FileDPtr; cr:pointer;
begin
  cf:=CFile; cr:=CRecPtr; CFile:=FD1; CRecPtr:=RP1;
  case F1^.FrmlTyp of
   'S':if F1^.Typ='T' then begin ss:=_LongS(F1);
         CFile:=FD2; CRecPtr:=RP2;
         if RPt=nil then DelTFld(F2) else DelDifTFld(RP2,RPt,F2);
         longs_(F2,ss); ReleaseStore(ss) end
       else begin s:=_ShortS(F1); CFile:=FD2;CRecPtr:=RP2; s_(F2,s) end;
   'R':begin r:=_r(F1); CFile:=FD2;CRecPtr:=RP2; r_(F2,r) end;
   'B':begin b:=_b(F1); CFile:=FD2;CRecPtr:=RP2; b_(F2,b) end;
  end;
  CFile:=cf; CRecPtr:=cr end;

function TestMask(var S:string;Mask:StringPtr;TypeN:boolean):boolean;
var i,ii,j,v,ls,lm:word; c:char;
label 1,2,3,4;
begin
  TestMask:=true; if Mask=nil then exit;
  v:=0; i:=0; ls:=length(S); j:=0; lm:=length(Mask^);
1:if j=lm then begin while i<ls do begin
    inc(i); if S[i]<>' ' then goto 4 end; exit end;
  inc(j);
  case Mask^[j] of
    ']',')': v:=0; '[': begin v:=1; ii:=i end; '(': begin v:=2; ii:=i end;
    '|': repeat inc(j) until Mask^[j]=')';
    else if i=ls then goto 4; inc(i); c:=S[i];
    case Mask^[j] of
      '#','9': if not IsDigit(c) then goto 3;
      '@': if not IsLetter(c) then goto 3;
      '?': ;
      '$': if not IsLetter(c) then goto 3 else goto 2;
      '!':
2:         S[i]:=UpcCharTab[c];
      else if c<>Mask^[j] then goto 3 end end;
  goto 1;
3:case v of
    1: begin repeat inc(j) until Mask^[j]=']'; v:=0; i:=ii; goto 1 end;
    2: begin repeat inc(j) until Mask^[j] in ['|',')']; i:=ii;
       if Mask^[j]='|' then goto 1 end end;
4:TestMask:=false; SetMsgPar(Mask^); WrLLF10Msg(653);
end;

function FieldEdit(F:FieldDPtr;Impl:FrmlPtr;
         LWw:word;iPos:word;var Txt:string;var RR:float;del,upd,ret:boolean;Delta:word):word;
var I,N,L,M,Col,Row:word; cc:char; Mask,Msk:StringPtr; s:ScreenStr; r:float;
    T:ScreenStr; b:boolean;
const C999='999999999999999';
label 0,1,11,2,3,4;
begin
  Col:=WhereX; Row:=whereY; if F^.Typ='B' then begin
    if Txt='' then write(' ') else write(Txt); gotoxy(col,row); CrsNorm;
0:  GetEvent; case Event.What of
      evKeyDown:begin
         KbdChar:=Event.KeyCode; ClrEvent; if KbdChar=_ESC_ then begin CrsHide; exit end;
         if KbdChar=_M_ then begin
11:        if (length(Txt)>0) and (Txt[1]=AbbrYes) then cc:=AbbrYes else cc:=AbbrNo;
           goto 1 end;
         cc:=upcase(char(KbdChar)); if (cc=AbbrYes) or (cc=AbbrNo) then goto 1 end;
      evMouseDown:
         if MouseInRect(WindMin.X+WhereX-1,WindMin.Y+WhereY-1,1,1) then begin
           ClrEvent; KbdChar:=_M_; goto 11 end end;
    ClrEvent; goto 0;
1:  write(cc); Txt:=cc; CrsHide; FieldEdit:=0; exit end;
  L:=F^.L;M:=F^.M; Mask:=FieldDMask(F);
  if (F^.Flg and f_Mask<>0) and (F^.Typ='A') then Msk:=Mask else Msk:=nil;      {!!!!}
2:
  iPos:=EditTxt(Txt,iPos,L,LWw,F^.Typ,del,false,upd,(F^.FrmlTyp='S') and ret,Delta);
  FieldEdit:=iPos; if iPos<>0 then exit;
  if (KbdChar=_ESC_) or not upd then exit;
  del:=true; iPos:=1; R:=0;
  if (length(Txt)=0)and(Impl<>nil) then begin
    AssignFld(F,Impl); DecodeField(F,L,Txt) end;
  case F^.Typ of
   'F','R':begin T:=LeadChar(' ',TrailChar(' ',Txt));
         I:=pos(',',T); if I>0 then begin T:=copy(T,1,I-1)+'.'+copy(T,I+1,255) end;
         if length(T)=0 then r:=0.0
         else begin val(T,r,I);
           if F^.Typ='F' then begin
             N:=L-2-M; if M=0 then inc(N);
             if (I<>0) or (abs(r)>=Power10[N]) then begin
               s:=copy(C999,1,N)+'.'+copy(C999,1,M);
               Set2MsgPar(s,s); WrLLF10Msg(617); goto 4 end end
            else {'R'} if I<>0 then begin WrLLF10Msg(639); goto 4 end end;
         if F^.Typ='F' then begin
          str(r:L:M,Txt); if F^.Flg and f_Comma<>0 then begin r:=r*Power10[M];
          if r>=0 then r:=r+0.5 else r:=r-0.5; r:=int(r) end end
         else {'R'} str(r:L,Txt);
         RR:=r end;
    'A':begin cc:=' '; goto 3 end;
    'N':begin cc:='0';
3:      if M=LeftJust then while length(Txt)<L do Txt:=Txt+cc
        else while length(Txt)<L do Txt:=cc+Txt;
        if (Msk<>nil) and not TestMask(Txt,Msk,true) then goto 4;
        end;
    'D':begin T:=LeadChar(' ',TrailChar(' ',Txt)); if T='' then r:=0 else begin
          r:=ValDate(T,Mask^);
          if (r=0) and (T<>LeadChar(' ',TrailChar(' ',StrDate(r,Mask^)))) then
            begin SetMsgPar(Mask^); WrLLF10Msg(618);
4:                GotoXY(Col,Row); goto 2 end end;
        Txt:=StrDate(r,Mask^); RR:=r end end;
end;



function IsFirstEmptyFld:boolean;
  begin IsFirstEmptyFld:=IsNewRec and (CFld=FirstEmptyFld) end;
function FldRow(D:EFldDPtr;I:word):word;
  begin FldRow:=E^.FrstRow+E^.NHdTxt+(I-1)*RT^.N+D^.Ln-1 end;
procedure SetFldAttr(D:EFldDPtr;I:word;Attr:word);
  begin ScrColor(D^.Col-1,FldRow(D,I)-1,D^.L,Attr) end;
function RecAttr(I:word):word;
var b:boolean;
begin
  b:=(I<>IRec) or not IsNewRec;
  if not IsNewRec and DeletedFlag then RecAttr:=E^.dDel else
  if b and Select and RunBool(E^.Bool) then RecAttr:=E^.dSubset else
  if b and IsSelectedRec(I) then RecAttr:=E^.dSelect
  else RecAttr:=E^.dNorm end;
procedure IVoff;
  begin SetFldAttr(CFld,IRec,RecAttr(IRec)) end;
procedure IVon;
  begin ScrColor(CFld^.Col-1,FldRow(CFld,IRec)-1,CFld^.L,E^.dHili) end;

function HasTTWw(F:FieldDPtr):boolean;
  begin HasTTWw:=(F^.Typ='T') and (F^.L>1) and not E^.IsUserForm end;

procedure DisplFld(D:EFldDPtr;I:word);
procedure Wr1Line(F:FieldDPtr);
  var Txt:string; s:LongStrPtr; max,l:word;
  begin s:=CopyLine(_longs(F),1,1);
        max:=F^.L-2; l:=s^.LL; if l>255 then l:=255;
        Move(s^.A,Txt[1],l); Txt[0]:=char(l); l:=LenStyleStr(Txt);
        if l>max then
           begin l:=max; Txt[0]:=char(LogToAbsLenStyleStr(Txt,l)) end;
        WrStyleStr(Txt,E^.dNorm);
        ReleaseStore(s); TextAttr:=E^.dNorm;
        if l<max then write(' ':max-l)  end;
var Txt:string; j,r:word; F:FieldDPtr;
begin
  r:=FldRow(D,I); GotoXY(D^.Col,r); F:=D^.FldD;
  DecodeField(F,D^.L,Txt); for j:=1 to length(Txt) do
    if Txt[j]<' ' then Txt[j]:=char(ord(Txt[j])+$40);
  write(Txt); if HasTTWw(F) then begin GotoXY(D^.Col+2,r); Wr1Line(F) end;
end;

procedure DisplEmptyFld(D:EFldDPtr;I:word);
var j:word; c:char;
begin
  GotoXY(D^.Col,FldRow(D,I)); if D^.FldD^.Flg and f_Stored<>0 then c:='.' else c:=' ';
  for j:=1 to D^.L do write(c);
  if HasTTWw(D^.FldD) then write(' ':D^.FldD^.L-1);
end;
procedure SetRecAttr(I:word);
var TA:word; D:EFldDPtr;
begin
  TA:=RecAttr(I); D:=E^.FirstFld; while D<>nil do begin
    if D^.Page=CPage then SetFldAttr(D,I,TA); D:=D^.Chain end;
end;
procedure DisplRec(I:word);
var D:EFldDPtr; IsCurrNewRec,NewFlds:boolean; a:word; N:longint; p:pointer;
label 1;
begin
  a:=E^.dNorm; N:=BaseRec+I-1; IsCurrNewRec:=IsNewRec and (I=IRec);
  p:=GetRecSpace;
  if (N>CNRecs) and not IsCurrNewRec then begin NewFlds:=true;goto 1 end;
  if I=IRec then CRecPtr:=E^.NewRecPtr else begin CRecPtr:=p;RdRec(N) end;
  NewFlds:=false; if not IsNewRec then a:=RecAttr(I);
1:D:=E^.FirstFld;while D<>nil do begin
    if IsCurrNewRec and (D=FirstEmptyFld) and (D^.Impl=nil) then NewFlds:=True;
    TextAttr:=a;
    if D^.Page=CPage then if NewFlds then DisplEmptyFld(D,I) else DisplFld(D,I);
    if IsCurrNewRec and (D=FirstEmptyFld) then NewFlds:=True;
    D:=D^.Chain end;
  ClearRecSpace(p); ReleaseStore(p);CRecPtr:=E^.NewRecPtr;
end;
procedure DisplTabDupl;
var D:EFldDPtr;
begin
  D:=E^.FirstFld; TextAttr:=E^.dTab;
  while D<>nil do begin
    if D^.Page=CPage then begin
      GotoXY(D^.Col+D^.L,FldRow(D,1));
      if D^.Tab then if D^.Dupl then write(^_) else write(^q)
      else if D^.Dupl then write(^y) else write(' ') end;
    D:=D^.Chain end;
end;


procedure DisplRecNr(N:longint);
begin
  if E^.RecNrLen>0 then begin
    GotoXY(E^.RecNrPos,1); TextAttr:=colors.fNorm; write(N:E^.RecNrLen) end;
end;
procedure DisplSysLine;
var i,j:word; m,s,x,z:string; point:boolean;
begin
  s:=E^.Head^; if s='' then exit; GotoXY(1,1); TextAttr:=colors.fNorm; ClrEol;
  i:=1; x:=''; while i<=length(s) do
    if s[i]='_' then begin
      m:=''; point:=false; while (i<=length(s)) and (s[i] in ['_','.']) do begin
        if s[i]='.' then point:=true;m:=m+s[i];inc(i) end;
      if point then begin
        if m='__.__.__' then x:=x+StrDate(today,'DD.MM.YY') else
        if m='__.__.____' then x:=x+StrDate(today,'DD.MM.YYYY') else
        x:=x+m  end
      else if length(m)=1 then x:=x+m
      else begin E^.RecNrLen:=length(m);E^.RecNrPos:=i-length(m);
        for j:=1 to length(m) do x:=x+' ' end end
    else begin x:=x+s[i];inc(i) end;
  if length(x)>TxtCols then x[0]:=char(TxtCols); write(x);
  DisplRecNr(CRec);
end;
procedure DisplBool;
var s:string;
begin
  if not WithBoolDispl then exit; GotoXY(1,2); TextAttr:=E^.dSubSet; ClrEol;
  if Select then begin
    s:=E^.BoolTxt^; if length(s)>TxtCols then s[0]:=char(TxtCols);
    GotoXY((TxtCols-length(s))div 2 +1,2); write(s) end;
end;
procedure DisplAllWwRecs;
var i,n:word; md:LockMode;
begin
  n:=E^.NRecs; if (n>1) and not EdRecVar then md:=NewLMode(RdMode);
  AdjustCRec; if not IsNewRec and not WasUpdated then RdRec(CRec);
  for i:=1 to n do DisplRec(i); IVon;
  if (n>1) and not EdRecVar then OldLMode(md);
end;
procedure SetNewWwRecAttr;
var I:word;
label 1;
begin
  CRecPtr:=GetRecSpace; for I:=1 to E^.NRecs do begin
    if BaseRec+I-1>CNRecs then goto 1;
    if not IsNewRec or (I<>IRec) then begin
      RdRec(BaseRec+I-1); SetRecAttr(I) end end;
1:IVon; ClearRecSpace(CRecPtr);ReleaseStore(CRecPtr); CRecPtr:=E^.NewRecPtr;
end;

procedure MoveDispl(From,Where,Number:word);
var D:EFldDPtr; i,r1,r2:word;
begin
  for i:=1 to Number do begin
    D:=E^.FirstFld; while D<>nil do begin
      r1:=FldRow(D,From)-1; r2:=FldRow(D,Where)-1;
      ScrMove(D^.Col-1,r1,D^.Col-1,r2,D^.L);
      if HasTTWw(D^.FldD) then
        ScrMove(D^.Col+1,r1,D^.Col+1,r2,D^.FldD^.L-2);
      D:=D^.Chain end;
    if From<Where then begin dec(From);dec(Where) end
    else begin inc(From);inc(Where) end   end;
end;

procedure SetNewCRec(N:longint;withRead:boolean);
var Max,I:longint;
begin
  Max:=E^.NRecs; I:=N-BaseRec+1;
  if I>Max then begin inc(BaseRec,I-Max);IRec:=Max end
  else if I<=0 then begin dec(BaseRec,abs(I)+1);IRec:=1 end
  else IRec:=I;
  if withRead then RdRec(CRec);
end;

procedure WriteSL(SL:StringList);
var Row:word;
begin
  while SL<>nil do begin
    Row:=WhereY; WrStyleStr(SL^.S,E^.Attr);
    GotoXY(E^.FrstCol,Row+1); SL:=SL^.Chain end;
end;
procedure DisplRecTxt;
var i:word;
begin
  gotoxy(E^.FrstCol,E^.FrstRow+E^.NHdTxt);
  for i:=1 to E^.NRecs do WriteSL(RT^.SL);
end;

procedure DisplEditWw;
var i,x,y:word;
begin
  with E^.V do begin
    if E^.ShdwY=1 then ScrColor(C1+1,R2,C2-C1+E^.ShdwX-1,colors.ShadowAttr);
    if E^.ShdwX>0 then for i:=R1 to R2 do ScrColor(C2,i,E^.ShdwX,colors.ShadowAttr);
    window(C1,R1,C2,R2); TextAttr:=E^.Attr; ClrScr end;
  WriteWFrame(E^.WFlags,E^.Top^,''); window(1,1,TxtCols,TxtRows);
  DisplSysLine; DisplBool;
  gotoxy(E^.FrstCol,E^.FrstRow); WriteSL(E^.HdTxt);
  DisplRecTxt; DisplTabDupl; NewDisplLL:=true;
  DisplAllWwRecs;
end;
procedure DisplWwRecsOrPage;
var min,max:word;
begin
  if CPage<>CFld^.Page then begin
    SetCPage; TextAttr:=E^.Attr; min:=word(WindMin); max:=word(WindMax);
    window(E^.FrstCol,E^.FrstRow+E^.NHdTxt,E^.LastCol,E^.FrstRow+E^.Rows-1);
    ClrScr; word(WindMin):=min; word(WindMax):=max;
    DisplRecTxt; DisplTabDupl end;
  DisplAllWwRecs; DisplRecNr(CRec);
end;

procedure DuplOwnerKey;
var KF,Arg:KeyFldDPtr;
begin
  if not E^.DownSet or (E^.OwnerTyp='i') then exit;
  KF:=E^.DownLD^.ToKey^.KFlds; Arg:=E^.DownLD^.Args;
  while Arg<>nil do begin
    DuplFld(E^.DownLD^.ToFD,CFile,E^.DownRecPtr,E^.NewRecPtr,E^.OldRecPtr,
            KF^.FldD,Arg^.FldD);
    Arg:=Arg^.Chain; KF:=KF^.Chain end;
end;
function CheckOwner(E:EditDPtr):boolean;
var X,X1:XString;
begin
  CheckOwner:=true; if E^.DownSet and (E^.OwnerTyp<>'i') then begin
    X.PackKF(E^.DownKey^.KFlds);
    CFile:=E^.DownLD^.ToFD; CRecPtr:=E^.DownRecPtr;
    X1.PackKF(E^.DownLD^.ToKey^.KFlds);
    X.S[0]:=char(minw(length(X.S),length(X1.S)));
    if X.S<>X1.S then CheckOwner:=false;
    CFile:=E^.FD; CRecPtr:=E^.NewRecPtr end;
end;
function CheckKeyIn(E:EditDPtr):boolean;
var k:KeyInDPtr; X:XString; p1,p2:StringPtr;
begin
  CheckKeyIn:=true; k:=E^.KIRoot; if k=nil then exit;
  X.PackKF(E^.VK^.KFlds); while k<>nil do begin
    p1:=k^.X1; p2:=k^.X2; if p2=nil then p2:=p1;
    if (p1^<=X.S) and (X.S<=(p2^+#$ff)) then exit;
    k:=k^.Chain end;
  CheckKeyIn:=false;
end;
function TestDuplKey(K:KeyDPtr):boolean;
var x:XString; N:longint;
begin
  x.PackKF(K^.KFlds);
  TestDuplKey:=K^.Search(x,false,N) and (IsNewRec or (E^.LockedRec<>N));
end;
procedure DuplKeyMsg(K:KeyDPtr);
begin
  SetMsgPar(K^.Alias^); WrLLF10Msg(820);
end;

{$ifdef FandSQL}
function OnlyKeyArgFlds(K:KeyDPtr):KeyDPtr;
var froot,f,f1:FieldDPtr; n,l:word; kfroot,kf,kf1:KeyFldDPtr;
begin
  kfroot:=nil; froot:=nil; l:=1; kf:=K^.KFlds; while kf<>nil do begin
    f:=kf^.FldD; if f^.Flg and f_Stored=0 then exit;
    n:=sizeof(f^)-1+length(f^.Name);
    if f^.Flg and f_Mask<>0 then inc(n,length(FieldDMask(f)^)+1);
    f1:=GetStore(n); Move(f^,f1^,n); f1^.Displ:=l; inc(l,f^.NBytes);
    ChainLast(froot,f1);
    kf1:=GetStore(sizeof(kf^)); move(kf^,kf1^,sizeof(kf^)); kf1^.FldD:=f1;
    ChainLast(kfroot,kf1); kf:=kf^.Chain end;
  CFile^.RecLen:=l; CFile^.FldD:=froot; K^.KFlds:=kfroot;
end; {$endif}
procedure BuildWork;
var Scan,Scan2:XScanPtr; p:pointer; K:KeyDPtr; KF:KeyFldDPtr;
    xx:XString; dupl,intvl,ok:boolean; wk2:WKeyDPtr; ki:KeyInDPtr;
    bool:FrmlPtr; l:word; f:FieldDPtr; er:ExitRecord;
label 1;
begin
  K:=nil; KF:=nil; if CFile^.Keys<>nil then KF:=CFile^.Keys^.KFlds;
  dupl:=true; intvl:=false; if HasIndex then begin
    K:=VK; KF:=K^.KFlds; dupl:=K^.Duplic; intvl:=K^.Intervaltest end;
  WK^.Open(KF,dupl,intvl);
  if OnlyAppend then exit;
  bool:=E^.Cond; ki:=E^.KIRoot; wk2:=nil;
  MarkStore(p); ok:=false; f:=nil; NewExit(Ovr,er); goto 1;
  if E^.DownSet then begin
    New(Scan,Init(CFile,E^.DownKey,nil,false));
    if E^.OwnerTyp='i' then Scan^.ResetOwnerIndex(E^.DownLD,E^.DownLV,bool)
    else begin CFile:=E^.DownLD^.ToFD; CRecPtr:=E^.DownRecPtr;
      xx.PackKF(E^.DownLD^.ToKey^.KFlds); CFile:=E^.FD; CRecPtr:=E^.NewRecPtr;
      Scan^.ResetOwner(@xx,bool) end;
    if ki<>nil then begin
      wk2:=GetZStore(sizeof(WK2^)); wk2^.Open(KF,true,false);
      CreateWIndex(Scan,wk2,'W'); New(Scan2,Init(CFile,wk2,ki,false));
      Scan2^.Reset(nil,false); Scan:=Scan2 end end
  else begin                                                  {$ifdef FandSQL}
    if CFile^.IsSQLFile and (bool=nil) then begin
      l:=CFile^.RecLen; f:=CFile^.FldD; OnlyKeyArgFlds(WK) end;{$endif}
    if {$ifdef FandSQL}CFile^.IsSQLFile or {$endif}(bool<>nil) then
      if (K<>nil) and not K^.InWork and (ki=nil) then K:=nil;
    New(Scan,Init(CFile,K,ki,false)); Scan^.Reset(bool,E^.SQLFilter) end;
  CreateWIndex(Scan,WK,'W');
  Scan^.Close; if wk2<>nil then wk2^.Close; ok:=true;
1:if f<>nil then begin CFile^.FldD:=f; WK^.KFlds:=KF; CFile^.RecLen:=l end;
  RestoreExit(er); if not ok then GoExit;
  ReleaseStore(p);
end;

procedure SetStartRec;
var n:longint; k:KeyDPtr; kf:KeyFldDPtr;
label 1;
begin
  k:=VK; if SubSet then k:=WK; kf:=nil; if k<>nil then kf:=k^.KFlds;
  if (E^.StartRecKey<>nil) and (k<>nil) then begin
    if k^.FindNr(XStringPtr(E^.StartRecKey)^,n) then goto 1 end
  else if E^.StartRecNo>0 then begin
    n:=LogRecNo(E^.StartRecNo);
1:  n:=maxl(1,minl(n,CNRecs));
    IRec:=maxw(1,minw(E^.StartIRec,E^.NRecs)); BaseRec:=n-IRec+1;
    if BaseRec<=0 then begin inc(IRec,BaseRec-1); BaseRec:=1 end end;
  if Only1Record then begin
    if CNRecs>0 then begin RdRec(CRec); n:=AbsRecNr(CRec) end else n:=0;
    if SubSet then WK^.Close; SubSet:=true;
    if n=0 then WK^.Open(nil,true,false) else WK^.OneRecIdx(kf,n);
    BaseRec:=1; IRec:=1 end;
end;

function OpenEditWw:boolean;
var md,md1,md2:LockMode; n:longint;
label 1,2,3;
begin
  CFile:=E^.Journal; if CFile<>nil then OpenCreateF(Shared);
  RdEStatus;
  if EdRecVar then if OnlyAppend then goto 2 else goto 3; {$ifdef FandSQL}
  if not CFile^.IsSQLFile then {$endif} OpenCreateF(Shared);
  E^.OldMd:=E^.FD^.LMode; UpdCount:=0;                    {$ifdef FandSQL}
  if CFile^.IsSQLFile then begin
    if (VK=nil) or not VK^.InWork then SubSet:=true end else {$endif}
  begin
    if HasIndex then TestXFExist; md:=NoDelMode;
    if OnlyAppend or (E^.Cond<>nil) or (E^.KIRoot<>nil) or E^.DownSet or
    MakeWorkX and HasIndex and CFile^.NotCached and not Only1Record then
    begin SubSet:=true; if HasIndex then md:=NoExclMode else md:=NoCrMode end
    else if (VK<>nil) and VK^.InWork then md:=NoExclMode end;
  if SubSet or Only1Record then WK:=GetZStore(sizeof(WK^));
  if not TryLMode(md,md1,1) then begin EdBreak:=15; goto 1 end;
  md2:=NewLMode(RdMode);
  if E^.DownSet and (E^.OwnerTyp='F') then begin
    CFile:=E^.DownLD^.ToFD; CRecPtr:=E^.DownRecPtr;
    md1:=NewLMode(RdMode); n:=E^.OwnerRecNo;
    if (n=0) or (n>CFile^.NRecs) then RunErrorM(E^.OldMd,611); ReadRec(n);
    OldLMode(md1); CFile:=E^.FD; CRecPtr:=E^.NewRecPtr end;
  if SubSet then BuildWork;
  if not Only1Record and HasIndex and VK^.InWork then begin
    if not SubSet then WK:=WKeyDPtr(VK); VK:=CFile^.Keys; WasWK:=true; SubSet:=true end;
  {$ifdef FandSQL}
  if CFile^.IsSQLFile then Strm1^.DefKeyAcc(WK);  {$endif}
  if not OnlyAppend then SetStartRec;
  if CNRecs=0 then
    if NoCreate then begin
      if Subset then CFileMsg(107,'0') else CFileMsg(115,'0');
      EdBreak:=13;
1:    if SubSet and not WasWK then WK^.Close;
      OldLMode(E^.OldMd); OpenEditWw:=false; exit end
    else begin
2:    IsNewRec:=true; Append:=true;
      LockRec(false); ZeroAllFlds; DuplOwnerKey; SetWasUpdated end
  else RdRec(CRec);
3:MarkStore(E^.AfterE); DisplEditWw; OpenEditWw:=true;
  if not EdRecVar then OldLMode(md2);
  if IsNewRec then NewRecExit;
end;

procedure RefreshSubset;
var md:LockMode;
begin
  md:=NewLMode(RdMode);
  if Subset and not (OnlyAppend or Only1Record or WasWK) then begin
    WK^.Close;
    BuildWork end;
  DisplAllWwRecs; OldLMode(md);
end;

procedure GotoRecFld(NewRec:longint;NewFld:EFldDPtr);
var
  NewIRec,NewBase,D,Delta:longint; i,Max:word; md:LockMode;
label 1,2;
begin
  IVoff; CFld:=NewFld; if NewRec=CRec then begin
    if CPage<>CFld^.Page then DisplWwRecsOrPage else IVon; exit end;
  if not EdRecVar then md:=NewLMode(RdMode);
  if NewRec>CNRecs then NewRec:=CNRecs; if NewRec<=0 then NewRec:=1;
  if Select then SetRecAttr(IRec);
  CFld:=NewFld; Max:=E^.NRecs;
  Delta:=NewRec-CRec; NewIRec:=IRec+Delta;
  if (NewIRec>0) and (NewIRec<=Max) then begin
    IRec:=NewIRec; RdRec(CRec); goto 1 end;
  NewBase:=BaseRec+Delta;
  if NewBase+Max-1>CNRecs then NewBase:=CNRecs-pred(Max);
  if NewBase<=0 then NewBase:=1;
  IRec:=NewRec-NewBase+1; D:=NewBase-BaseRec; BaseRec:=NewBase;
  RdRec(CRec);
  if abs(D)>=Max then begin DisplWwRecsOrPage;goto 2 end;
  if D>0 then begin
    MoveDispl(D+1,1,Max-D); for i:=Max-D+1 to Max do DisplRec(i) end
  else begin
    D:=-D;MoveDispl(Max-D,Max,Max-D); for i:=1 to D do DisplRec(i) end;
1:DisplRecNr(CRec); IVOn;
2:if not EdRecVar then OldLMode(md);
end;


procedure UpdMemberRef(POld,PNew:pointer);
var LD:LinkDPtr; x,xnew,xold:XString; Scan:XScanPtr; cf:FileDPtr;
    cr,p,p2:pointer; sql:boolean; k:KeyDPtr; kf,kf1,kf2,Arg:KeyFldDPtr;
label 1,2;
begin
  cf:=CFile; cr:=CRecPtr; LD:=LinkDRoot; while LD<>nil do begin
    if (LD^.MemberRef<>0) and (LD^.ToFD=cf) and
       ((PNew<>nil) or (LD^.MemberRef<>2)) then begin
      CFile:=cf; kf2:=LD^.ToKey^.KFlds; CRecPtr:=POld; xold.PackKF(kf2);
      if PNew<>nil then begin
        CRecPtr:=PNew; xnew.PackKF(kf2); if xnew.s=xold.s then goto 2 end;
      CFile:=LD^.FromFD; {$ifdef FandSQL} sql:=CFile^.IsSQLFile; {$endif}
      k:=GetFromKey(LD); kf1:=k^.KFlds;
      p:=GetRecSpace; CRecPtr:=p; if PNew<>nil then p2:=GetRecSpace;
      New(Scan,Init(CFile,k,nil,true)); Scan^.ResetOwner(@xold,nil);
      {$ifdef FandSQL}if not sql then {$endif} ScanSubstWIndex(Scan,kf1,'W');
1:    CRecPtr:=p; Scan^.GetRec;
      if not Scan^.EOF then begin
        {$ifdef FandSQL}if sql then x.PackKF(kf1);{$endif}
        if PNew=nil then begin
          RunAddUpdte1('-',nil,false,nil,LD); UpdMemberRef(p,nil); {$ifdef FandSQL}
          if sql then Strm1^.DeleteXRec(k,@x,false) else {$endif}
          DeleteXRec(Scan^.RecNr,true) end
        else begin
          Move(CRecPtr^,p2^,CFile^.RecLen);
          CRecPtr:=p2; kf:=kf2; Arg:=LD^.Args; while kf<>nil do begin
            DuplFld(cf,CFile,PNew,p2,nil,kf^.FldD,Arg^.FldD);
            Arg:=Arg^.Chain; kf:=kf^.Chain end;
          RunAddUpdte1('d',p,false,nil,LD);
          UpdMemberRef(p,p2);
          {$ifdef FandSQL}
          if sql then Strm1^.UpdateXRec(k,@x,false) else {$endif}
          OverWrXRec(Scan^.RecNr,p,p2) end;
        goto 1 end;
      Scan^.Close; ClearRecSpace(p); ReleaseStore(p);
    end;
2:  LD:=LD^.Chain end;
  CFile:=cf; CRecPtr:=cr;
end;

procedure WrJournal(Upd:char;RP:pointer;Time:float);
var RPOfs:word absolute RP; l:word; F:FieldDPtr; n:longint; md:LockMode;
label 1;
begin
  if E^.Journal=nil then goto 1;   l:=CFile^.RecLen; n:=AbsRecNr(CRec);
  if (CFile^.XF<>nil) then begin inc(RPOfs);dec(l) end;
  CFile:=E^.Journal; CRecPtr:=GetRecSpace; F:=CFile^.FldD;
  s_(F,Upd);F:=F^.Chain;r_(F,int(n));F:=F^.Chain;
  r_(F,int(UserCode));F:=F^.Chain;r_(F,time);F:=F^.Chain;
  Move(RP^,ptr(seg(CRecPtr^),ofs(CRecPtr^)+F^.Displ)^,l);
  md:=NewLMode(CrMode); IncNRecs(1); WriteRec(CFile^.NRecs); OldLMode(md);
  ReleaseStore(CRecPtr); CFile:=E^.FD; CRecPtr:=E^.NewRecPtr;
1:inc(UpdCount); if UpdCount=E^.SaveAfter then begin SaveFiles; UpdCount:=0 end;
end;

function CompChk(D:EFldDPtr;Typ:char):ChkDPtr;forward;
procedure DisplChkErr(C:ChkDPtr);forward;
procedure UndoRecord; forward;


function LockForMemb(FD:FileDPtr;Kind:word;NewMd:LockMode;var md:LockMode):boolean;
var ld:LinkDPtr; md1:LockMode; {0-ExLMode,1-lock,2-unlock}
begin
  LockForMemb:=false; ld:=LinkDRoot; while ld<>nil do begin
    if (ld^.ToFD=FD) and
       ((NewMd<>DelMode) and (ld^.MemberRef<>0) or (ld^.MemberRef=1)) and
       (ld^.FromFD<>FD) then begin
      CFile:=ld^.FromFD; case Kind of
        0: CFile^.TaLMode:=CFile^.LMode;
        1: begin md:=NewMd; if not TryLMode(NewMd,md1,2) then exit end;
        2: OldLMode(CFile^.TaLMode) end;
      if not LockForAdd(CFile,Kind,true,md) then exit;
      if not LockForMemb(ld^.FromFD,Kind,NewMd,md) then exit end;
    ld:=ld^.Chain end;
  LockForMemb:=true;
end;
function LockWithDep(CfMd,MembMd:LockMode;var OldMd:LockMode):boolean;
var cf,cf2:FileDPtr; b:boolean; w,w1:longint; md:LockMode;
label 1,2,3,4;
begin
  LockWithDep:=true; if EdRecVar then exit;
  cf:=CFile; w:=0;
  LockForAdd(cf,0,true,md); LockForMemb(cf,0,MembMd,md);
1:CFile:=cf; if not TryLMode(CfMd,OldMd,1) then begin
    md:=CfMd; goto 3 end;
  if not LockForAdd(cf,1,true,md) then begin
    cf2:=CFile; goto 2 end;
  if MembMd=NullMode then goto 4;
  if not LockForMemb(cf,1,MembMd,md) then begin
    cf2:=CFile; LockForMemb(cf,2,MembMd,md);
2:  LockForAdd(cf,2,true,md); CFile:=cf; OldLMode(OldMd); CFile:=cf2;
3:  SetCPathVol; Set2MsgPar(CPath,LockModeTxt[md]); w1:=PushWrLLMsg(825,true);
    if w=0 then w:=w1 else TWork.Delete(w1); LockBeep;
    if KbdTimer(spec.NetDelay,1) then goto 1; LockWithDep:=false end;
4:CFile:=cf; if w<>0 then PopW(w);
end;
procedure UnLockWithDep(OldMd:LockMode);
var cf:FileDPtr; md:LockMode;
begin
  if EdRecVar then exit;
  cf:=CFile; OldLMode(OldMd); LockForAdd(cf,2,true,md);
  LockForMemb(cf,2,md,md); CFile:=cf;
end;


function DeleteRecProc:boolean;
function CleanUp:boolean;
var X:EdExitDPtr; ok,b:boolean; ld:LinkDPtr;
begin
  CleanUp:=false; if HasIndex and DeletedFlag then exit;
  X:=E^.ExD; while X<>nil do begin
    if X^.AtWrRec then begin
      EdBreak:=17; ok:=EdOk; EdOk:=true; LastTxtPos:=-1;
      if not StartExit(X,false) or not EdOk then begin EdOk:=ok; exit end;
      EdOk:=ok; WasUpdated:=false end;
    X:=X^.Chain end;
  if AddSwitch then begin
    ld:=LinkDRoot; while ld<>nil do begin
      if (ld^.MemberRef=2) and (ld^.ToFD=CFile) and
         (Owned(nil,nil,ld)>0) then begin WrLLF10Msg(662); exit end;
      ld:=ld^.Chain end;
    if not RunAddUpdte1('-',nil,false,nil,nil) then exit;
    UpdMemberRef(CRecPtr,nil) end;
  if not ChptDel then exit;
  WrJournal('-',CRecPtr,today+currtime); CleanUp:=true end;
function DelIndRec(I,N:longint):boolean;
var x:XString;
begin
  DelIndRec:=false; if CleanUp then begin                {$ifdef FandSQL}
    if CFile^.IsSQLFile then begin
      x.PackKF(VK^.KFlds); Strm1^.DeleteXRec(VK,@x,false) end else {$endif}
    DeleteXRec(N,true);
    if (E^.SelKey<>nil) and E^.SelKey^.Delete(N) then dec(E^.SelKey^.NR);
    if SubSet then WK^.DeleteAtNr(I); DelIndRec:=true; E^.EdUpdated:=true end;
end;
var I,J,N,oBaseRec:longint; oIRec:word; Group,fail:boolean; OldMd:LockMode;
    b:boolean;
label 1,2;
begin
  DeleteRecProc:=false; Group:=false;
  if Select then begin
    F10SpecKey:=_ESC_; Group:=PromptYN(116); if KbdChar=_ESC_ then exit end;
  if not Group then if VerifyDelete and not PromptYN(109) then exit;
  if not LockWithDep(DelMode,DelMode,OldMd) then exit;
  UndoRecord; N:=AbsRecNr(CRec); RdRec(CRec);
  oIRec:=IRec; oBaseRec:=BaseRec;    { exit proc uses CRec for locking etc.}
  if HasIndex {$ifdef FandSQL} or CFile^.IsSQLFile {$endif} then begin
    TestXFExist; if Group then begin
      IRec:=1; BaseRec:=1; while BaseRec<=CNRecs do begin
        N:=AbsRecNr(BaseRec); ClearDeletedFlag;{prevent err msg 148}
        if not ELockRec(E,N,false,Subset) then goto 1;
        RdRec(BaseRec);
        if RunBool(E^.Bool) then b:=DelIndRec(BaseRec,N)
        else begin b:=true; inc(BaseRec) end;
        UnLockRec(E); if not b then goto 1 end;
1:    end
    else begin
      if not ELockRec(E,N,false,SubSet) then goto 1;
      DelIndRec(CRec,N); UnLockRec(E) end end
  else if Group then begin
    J:=0; fail:=false; BaseRec:=1; IRec:=1; E^.EdUpdated:=true;
    for I:=1 to CFile^.NRecs do begin
      ReadRec(I); if fail then goto 2;
      if SubSet then with WK^ do begin
        if (BaseRec>NRecs) or (NrToRecNr(BaseRec)<>J+1) then goto 2 end
      else BaseRec:=I;
      if RunBool(E^.Bool) then begin
        if not CleanUp then begin fail:=true; goto 2 end;
        if SubSet then with WK^ do begin
          DeleteAtNr(BaseRec); AddToRecNr(J+1,-1) end;
        DelAllDifTFlds(CRecPtr,nil) end
      else begin
        if SubSet then inc(BaseRec);
2:      inc(J); WriteRec(J) end end;
    DecNRecs(CFile^.NRecs-J) end
  else if CleanUp then begin
    E^.EdUpdated:=true;
    if SubSet then with WK^ do begin DeleteAtNr(CRec); AddToRecNr(N,-1) end;
    DeleteRec(N) end;
  CFld:=E^.FirstFld; IRec:=oIRec; BaseRec:=oBaseRec;
  ClearDeletedFlag; AdjustCRec;
  if IsNewRec then DuplOwnerKey else RdRec(CRec);
  DisplWwRecsOrPage;
  UnLockWithDep(OldMd); DeleteRecProc:=true;
end;


function WriteCRec(MayDispl:boolean;var Displ:boolean):boolean;

function ExitCheck:boolean;
var X:EdExitDPtr; ok:boolean;
begin
  ExitCheck:=false; X:=E^.ExD; while X<>nil do begin
    if X^.AtWrRec then begin
      EdBreak:=16; ok:=EdOk; EdOk:=true; LastTxtPos:=-1;
      if StartExit(X,MayDispl) and EdOk then EdOk:=ok else begin
        EdOk:=ok; exit end end;
    X:=X^.Chain end;
  ExitCheck:=true end;
function UpdateIndexes:longint;
var K:KeyDPtr; KSel:WKeyDPtr; N,NNew:longint; x:XString;
begin
  NNew:=E^.LockedRec; KSel:=E^.SelKey;
  if IsNewRec then begin NNew:=CFile^.NRecs+1; inc(CFile^.XF^.NRecs) end
  else if KSel<>nil then begin CRecPtr:=E^.OldRecPtr;
     if KSel^.RecNrToPath(x,NNew) then begin
       KSel^.DeleteOnPath; CRecPtr:=E^.NewRecPtr; KSel^.Insert(NNew,false) end;
     CRecPtr:=E^.NewRecPtr end;
  if VK^.RecNrToPath(x,E^.LockedRec) and not WasWK then begin
    if IsNewRec then begin
    VK^.InsertOnPath(x,NNew); if SubSet then WK^.InsertAtNr(CRec,NNew) end;
    N:=CRec end
  else begin
    if not IsNewRec then begin
      CRecPtr:=E^.OldRecPtr; VK^.Delete(E^.LockedRec);
      if SubSet then WK^.DeleteAtNr(CRec);
      CRecPtr:=E^.NewRecPtr; x.PackKF(VK^.KFlds);
      VK^.Search(x,true,N) end;
    N:=VK^.PathToNr; VK^.InsertOnPath(x,NNew);
    if VK^.InWork then inc(WKeyDPtr(VK)^.NR);
    if SubSet then N:=WK^.InsertGetNr(NNew) end;
  UpdateIndexes:=N;
  K:=CFile^.Keys; while K<>nil do begin
    if K<>VK then begin
      if not IsNewRec then begin CRecPtr:=E^.OldRecPtr; K^.Delete(E^.LockedRec) end;
      CRecPtr:=E^.NewRecPtr; K^.Insert(NNew,true) end;
    K:=K^.Chain end;
  CRecPtr:=E^.NewRecPtr;
end;
{$ifdef FandSQL}
function UpdSQLFile:boolean;
var n:longint; x:XString;
begin
  UpdSQLFile:=false; if IsNewRec then begin
    if AddSwitch and not RunAddUpdte1('+',nil,false,nil,nil) then exit;
    if not Strm1^.InsertRec(false,false) then begin
      RunAddUpdte('-',nil,nil); exit end end
  else begin
    if AddSwitch and not RunAddUpdte1('d',E^.OldRecPtr,false,nil,nil) then exit;
    CRecPtr:=E^.OldRecPtr; x.PackKF(VK^.KFlds); CRecPtr:=E^.NewRecPtr;
    if not Strm1^.UpdateXRec(VK,@x,false) then begin
      RunAddUpdte1('d',E^.OldRecPtr,true,nil,nil);exit end;
    WK^.DeleteAtNr(CRec) end;
  n:=WK^.InsertGetNr(0); if n<>CRec then begin SetNewCRec(n,true); Displ:=true end;
  UpdSQLFile:=true;
end;
{$endif}
function OldRecDiffers:boolean;
var x:XString; f:FieldDPtr;
label 1,2;
begin
  OldRecDiffers:=false;
  if IsCurrChpt or ({$ifdef FandSQL}not CFile^.IsSQLFile and {$endif}not CFile^.NotCached) then exit;
  CRecPtr:=GetRecSpace;  {$ifdef FandSQL}
  if CFile^.IsSQLFile then begin
    x.S:=WK^.NrToStr(CRec); Strm1^.KeyAcc(WK,@x); f:=CFile^.FldD;
    while f<>nil do begin
      with f^ do if (Flg and f_Stored<>0) and (Typ<>'T') and
       (CompArea(Pchar(CRecPtr)+Displ,Pchar(E^.OldRecPtr)+Displ,NBytes)<>ord(_equ)) then
       goto 1;
      f:=f^.Chain end;
    goto 2 end else {$endif}
  ReadRec(E^.LockedRec);
  if CompArea(CRecPtr,E^.OldRecPtr,CFile^.RecLen)<>ord(_equ) then begin
1:  DelAllDifTFlds(E^.NewRecPtr,E^.OldRecPtr);
    Move(CRecPtr^,E^.NewRecPtr^,CFile^.RecLen); WasUpdated:=false;
    OldRecDiffers:=true end;
2:ClearRecSpace(CRecPtr); ReleaseStore(CRecPtr); CRecPtr:=E^.NewRecPtr;
end;

var N,CNew:longint; ID:ImplDPtr; time:float; s:LongStrPtr;
    D:EFldDPtr; C:ChkDPtr; OldMd:LockMode; K:KeyDPtr;
label 1,2;
begin
  Displ:=false;
  if not WasUpdated or not IsNewRec and EquOldNewRec then begin
    IsNewRec:=false; WasUpdated:=false; WriteCRec:=true; UnlockRec(E); exit end;
  WriteCRec:=false;
  if IsNewRec then begin
  ID:=E^.Impl; while ID<>nil do begin
      AssgnFrml(ID^.FldD,ID^.Frml,true,false);ID:=ID^.Chain end end;
  if MustCheck then begin   { repeat field checking }
    D:=E^.FirstFld; while D<>nil do begin
      C:=CompChk(D,'F');if C<>nil then begin
        if MayDispl then GotoRecFld(CRec,D) else CFld:=D;
        DisplChkErr(C); exit end;
      D:=D^.Chain end end;
  if IsNewRec then begin
    if not LockWithDep(CrMode,NullMode,OldMd) then exit end
  else if not EdRecVar then begin
    if not LockWithDep(WrMode,WrMode,OldMd) then exit;
    if OldRecDiffers then begin
      UnLockRec(E); UnLockWithDep(OldMd); WrLLF10Msg(149);
      DisplRec(CRec); IVOn; exit end end;
  if Subset and not(NoCondCheck or RunBool(E^.Cond) and CheckKeyIn(E)) then begin
    UnLockWithDep(OldMd); WrLLF10Msg(823); exit end;
  if E^.DownSet then begin DuplOwnerKey; Displ:=true end;
  if not ExitCheck then goto 1;
  if EdRecVar then goto 2;                    {$ifdef FandSQL}
  if CFile^.IsSQLFile then begin
    if UpdSQLFile then goto 2 else goto 1 end; {$endif}
  if HasIndex then begin   { test duplicate keys }
    K:=CFile^.Keys; while K<>nil do begin
    if not K^.Duplic and TestDuplKey(K) then begin
      UnLockWithDep(OldMd); DuplKeyMsg(K); exit end;
    K:=K^.Chain end end;
  ClearDeletedFlag;
  if HasIndex then begin
    TestXFExist;
    if IsNewRec then begin
      if AddSwitch and not RunAddUpdte1('+',nil,false,nil,nil) then goto 1;
      CNew:=UpdateIndexes; CreateRec(CFile^.NRecs+1) end
    else begin
      if AddSwitch then begin
        if not RunAddUpdte1('d',E^.OldRecPtr,false,nil,nil) then goto 1;
        UpdMemberRef(E^.OldRecPtr,CRecPtr) end;
      CNew:=UpdateIndexes; WriteRec(E^.LockedRec) end;
    if CNew<>CRec then begin
      SetNewCRec(CNew,true);if E^.NRecs>1 then Displ:=true end end
  else if IsNewRec then begin
    N:=E^.LockedRec; if N=0 then begin
      N:=CRec; if N=CNRecs then N:=CFile^.NRecs+1 else
      if SubSet then N:=WK^.NrToRecNr(N) end;
    if AddSwitch and not RunAddUpdte1('+',nil,false,nil,nil) then goto 1;
    if ChptWriteCRec<>0 then goto 1;
    CreateRec(N);
    if SubSet then with WK^ do begin
      AddToRecNr(N,1); InsertAtNr(CRec,N) end end
  else begin
    if AddSwitch then begin
      if not RunAddUpdte1('d',E^.OldRecPtr,false,nil,nil) then goto 1;
      UpdMemberRef(E^.OldRecPtr,CRecPtr) end;
    case ChptWriteCRec of
      1:goto 1;
      2:begin if (LongintPtr(Pchar(E^.OldRecPtr)+ChptTxt^.Displ)^<>LongintPtr(Pchar(CRecPtr)+ChptTxt^.Displ)^)
        and PromptYN(157) then begin
          s:=_LongS(ChptTxt); TWork.Delete(ClpBdPos);
          ClpBdPos:=TWork.Store(s); ReleaseStore(s) end;
        UndoRecord; goto 1 end end;
    WriteRec(E^.LockedRec) end;
  time:=today+currtime;
  if IsNewRec then WrJournal('+',CRecPtr,time)
  else begin WrJournal('O',E^.OldRecPtr,time); WrJournal('N',CRecPtr,time) end;
2:if not IsNewRec and not NoDelTFlds then DelAllDifTFlds(E^.OldRecPtr,E^.NewRecPtr);
  E^.EdUpdated:=true; NoDelTFlds:=false;
  IsNewRec:=false; WasUpdated:=false; WriteCRec:=true; UnlockRec(E);
1:UnLockWithDep(OldMd);
end;

procedure UndoRecord;
var md:LockMode; f:FieldDPtr;
begin if not IsNewRec and WasUpdated then begin
  if HasTF then if NoDelTFlds then begin f:=CFile^.FldD; while f<>nil do begin
    if (f^.Flg and f_Stored<>0) and (f^.Typ='T') then
     LongintPtr(Pchar(E^.OldRecPtr)+f^.Displ)^:=LongintPtr(Pchar(CRecPtr)+f^.Displ)^;
    f:=f^.Chain end end
  else DelAllDifTFlds(E^.NewRecPtr,E^.OldRecPtr);
  Move(E^.OldRecPtr^,E^.NewRecPtr^,CFile^.RecLen);
  WasUpdated:=false; NoDelTFlds:=false; UnlockRec(E); DisplRec(IRec); IVon;
end end;


procedure DuplFromPrevRec;
var F:FieldDPtr; md:LockMode; cr:pointer;
begin if CFld^.Ed(IsNewRec) then begin
  F:=CFld^.FldD; md:=RdMode; if F^.Typ='T' then md:=WrMode;
  md:=NewLMode(md); SetWasUpdated;
  cr:=CRecPtr; CRecPtr:=GetRecSpace; RdRec(CRec-1);
  DuplFld(CFile,CFile,CRecPtr,E^.NewRecPtr,E^.OldRecPtr,F,F);
  ClearRecSpace(CRecPtr); ReleaseStore(CRecPtr); CRecPtr:=cr; OldLMode(md);
end end;
