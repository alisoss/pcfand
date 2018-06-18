const Alloc=2048;


procedure IncNRecs(N:longint);
begin with CFile^ do begin
{$ifdef FandDemo}
  if NRecs>100 then RunError(884);
{$endif}
  inc(NRecs,N);
  SetUpdHandle(Handle); if Typ='X' then SetUpdHandle(XF^.Handle);
end end;

procedure DecNRecs(N:longint);
begin with CFile^ do begin
  dec(NRecs,N); SetUpdHandle(Handle); if Typ='X' then SetUpdHandle(XF^.Handle);
  WasWrRec:=true;
end end;

procedure SeekRec(N:longint);
  begin CFile^.IRec:=N;
       if CFile^.XF=nil then CFile^.EOF:=N>=CFile^.NRecs
       else CFile^.EOF:=N>=CFile^.XF^.NRecs end;

procedure PutRec;
begin with CFile^ do begin
  inc(NRecs);
  RdWrCache(false,Handle,NotCached,longint(IRec)*RecLen+FrstDispl,RecLen,CRecPtr^);
  inc(IRec); EOF:=true;
end end;

procedure ReadRec(N:longint);
begin with CFile^ do
  RdWrCache(true,Handle,NotCached,(N-1)*RecLen+FrstDispl,RecLen,CRecPtr^);
end;

procedure WriteRec(N:longint);
begin with CFile^ do begin
  RdWrCache(false,Handle,NotCached,(N-1)*RecLen+FrstDispl,RecLen,CRecPtr^);
  WasWrRec:=true;
end end;


procedure CreateRec(N:longint);
var i:longint; cr:pointer;
begin
  IncNRecs(1); cr:=CRecPtr; CRecPtr:=GetRecSpace;
  for i:=CFile^.NRecs-1 downto N do begin
    ReadRec(i); WriteRec(i+1) end;
  ReleaseStore(CRecPtr); CRecPtr:=cr; WriteRec(N);
end;
procedure DeleteRec(N:longint);
var i:longint;
begin
  DelAllDifTFlds(CRecPtr,nil); for i:=N to CFile^.NRecs-1 do begin
    ReadRec(i+1); WriteRec(i) end;
  DecNRecs(1);
end;

function LinkLastRec(FD:FileDPtr;var N:longint;WithT:boolean):boolean;
var md:LockMode;
label 1;
begin
  CFile:=FD; CRecPtr:=GetRecSpace; md:=NewLMode(RdMode); LinkLastRec:=true;
{$ifdef FandSQL} if FD^.IsSQLFile then
    if Strm1^.SelectXRec(nil,nil,_equ,WithT) then N:=1 else goto 1 else {$endif}
  begin
    N:=CFile^.NRecs; if N=0 then begin
1:    ZeroAllFlds; LinkLastRec:=false; N:=1 end
    else ReadRec(N) end;
  OldLMode(md);
end;

procedure AsgnParFldFrml(FD:FileDPtr;F:FieldDPtr;Z:FrmlPtr;Ad:boolean);
var cf:FileDPtr; cr,p:pointer; N:longint; md:LockMode; b:boolean;
begin
  cf:=CFile; cr:=CRecPtr; CFile:=FD;
{$ifdef FandSQL} if CFile^.IsSQLFile then begin
    CRecPtr:=GetRecSpace; ZeroAllFlds; AssgnFrml(F,Z,true,Ad);
    Strm1^.UpdateXFld(nil,nil,F); ClearRecSpace(CRecPtr) end else {$endif}
  begin md:=NewLMode(WrMode);
    if not LinkLastRec(CFile,N,true) then begin IncNRecs(1); WriteRec(N) end;
    AssgnFrml(F,Z,true,Ad); WriteRec(N); OldLMode(md) end;
  ReleaseStore(CRecPtr); CFile:=cf; CRecPtr:=cr;
end;

function SearchKey(var XX:XString;Key:KeyDPtr;var NN:longint):boolean;
var L,R,N:longint; Result:integer; KF:KeyFldDPtr; x:XString;
label 1;
begin
  SearchKey:=false; L:=1; Result:=ord(_gt); NN:=CFile^.NRecs; N:=NN;
  if N=0 then exit; KF:=Key^.KFlds;
  repeat
    if Result=ord(_gt) then R:=N else L:=N+1;
    N:=(L+R) div 2; ReadRec(N); x.PackKF(KF);
    Result:=CompStr(x.S,XX.S);
  until (L>=R)or(Result=ord(_equ));
  if (N=NN) and (Result=ord(_lt)) then inc(NN)
  else begin
    if Key^.Duplic and (Result=ord(_equ)) then
    while N>1 do begin
      dec(N); ReadRec(N); x.PackKF(KF);
      if CompStr(x.S,XX.S)<>ord(_equ) then begin
        inc(N); ReadRec(N); goto 1 end end;
1:  NN:=N end;
  if (Result=ord(_equ)) or Key^.IntervalTest and (Result=ord(_gt)) then
    SearchKey:=true;
end;

function LinkUpw(LD:LinkDPtr;var N:longint;WithT:boolean):boolean;
var K:KeyDPtr; Arg,KF:KeyFldDPtr;
  CF,ToFD:FileDPtr; F,F2:FieldDPtr; CP,RecPtr:pointer;
  LU:boolean; md:LockMode;
  s:string; r:float; b:boolean;  x:XString absolute s;
label 1,2;
begin
  ToFD:=LD^.ToFD; CF:=CFile; CP:=CRecPtr;
  K:=LD^.ToKey; Arg:=LD^.Args; x.PackKF(Arg);
  CFile:=ToFD; RecPtr:=GetRecSpace; CRecPtr:=RecPtr;
{$ifdef FandSQL} if CFile^.IsSQLFile then begin
    LU:=Strm1^.SelectXRec(K,@X,_equ,WithT); N:=1; if LU then goto 2 else goto 1 end;
{$endif}
  md:=NewLMode(RdMode);
  if ToFD^.Typ='X' then begin TestXFExist; LU:=K^.SearchIntvl(x,false,N) end
  else if CFile^.NRecs=0 then begin LU:=false; N:=1 end
  else LU:=SearchKey(x,K,N);
  if LU then ReadRec(N) else begin
1:  ZeroAllFlds; KF:=K^.KFlds; while Arg<>nil do begin
      F:=Arg^.FldD; F2:=KF^.FldD; CFile:=CF; CRecPtr:=CP;
      if F2^.Flg and f_Stored<>0 then case F^.FrmlTyp of
       'S':begin s:=_shorts(F); CFile:=ToFD; CRecPtr:=RecPtr; s_(F2,s) end;
       'R':begin r:=_r(F); CFile:=ToFD; CRecPtr:=RecPtr; r_(F2,r) end;
       'B':begin b:=_b(F); CFile:=ToFD; CRecPtr:=RecPtr; b_(F2,b) end;
      end;
      Arg:=Arg^.Chain; KF:=KF^.Chain end;
    CFile:=ToFD; CRecPtr:=RecPtr end;
2:LinkUpw:=LU;
{$ifdef FandSQL} if not CFile^.IsSQLFile then {$endif} OldLMode(md);
end;

procedure AssignNRecs(Add:boolean;N:longint);
var i,OldNRecs:longint; md:LockMode;
label 1;
begin                                                     {$ifdef FandSQL}
  if CFile^.IsSQLFile then begin
    if (N=0) and not Add then Strm1^.DeleteXRec(nil,nil,false); exit end; {$endif}
  md:=NewLMode(DelMode); OldNRecs:=CFile^.NRecs;
  if Add then N:=N+OldNRecs;
  if (N<0) or (N=OldNRecs) then goto 1;
  if (N=0) and (CFile^.TF<>nil) then CFile^.TF^.SetEmpty;
  if CFile^.Typ='X' then
    if N=0 then begin
      CFile^.NRecs:=0; SetUpdHandle(CFile^.Handle); XFNotValid; goto 1 end
    else begin SetMsgPar(CFile^.Name);RunErrorM(md,821) end;
  if N<OldNRecs then begin DecNRecs(OldNRecs-N); goto 1 end;
  CRecPtr:=GetRecSpace; ZeroAllFlds; SetDeletedFlag;
  IncNRecs(N-OldNRecs); for i:=OldNRecs+1 to N do WriteRec(i);
  ReleaseStore(CRecPtr);
1:OldLMode(md);
end;

               { FIELD ACCESS }

const FirstDate:float=6.97248E+5;

function IsNullValue(p:pointer;l:word):boolean; assembler;
asm les di,P; mov cx,l; mov al,0ffH; cld;
    repe scasb; mov ax,0; jne @1; mov ax,1
@1: end;

function _ShortS(F:FieldDPtr):string;
  var P:pointer; POfs:word absolute P;
      S:string;  ss:LongStrPtr; l:word;
  begin if F^.Flg and f_Stored<>0 then
      begin l:=F^.L; S[0]:=char(l); P:=CRecPtr; inc(POfs,F^.Displ);
        case F^.Typ of
          'A','N':
              if F^.Typ='A' then begin
                Move(P^,S[1],l); if F^.Flg and f_Encryp<>0 then Code(S[1],l);
                if IsNullValue(@S[1],l) then FillChar(S[1],l,' ')  end
              else if IsNullValue(p,F^.NBytes) then FillChar(S[1],l,' ')
              else Unpack(P^,S[1],l);
          'T':begin ss:=_LongS(F);
              if ss^.LL>255 then S[0]:=#255 else S[0]:=chr(ss^.LL);
              Move(ss^.A,S[1],length(S)); ReleaseStore(ss) end;
        end;
        _ShortS:=S;
     end
   else _ShortS:=RunShortStr(F^.Frml);
  end;

function _LongS(F:FieldDPtr):LongStrPtr;
var P:pointer;POfs:word absolute P;
    LP:^longint absolute P;
    S:LongStrPtr; Pos:longint; err:integer; md:LockMode; l:word;
begin
  if F^.Flg and f_Stored<>0 then begin
    P:=CRecPtr; inc(POfs,F^.Displ); l:=F^.L;
    case F^.Typ of
     'A','N': begin S:=GetStore(l+2); S^.LL:=l;
        if F^.Typ='A' then begin
         Move(P^,S^.A,l); if F^.Flg and f_Encryp<>0 then Code(S^.A,l);
         if IsNullValue(@S^.A,l) then begin S^.LL:=0; ReleaseAfterLongStr(S) end end
        else if IsNullValue(p,F^.NBytes) then begin
          S^.LL:=0; ReleaseAfterLongStr(S) end
        else Unpack(P^,S^.A,l) end;
     'T':begin if HasTWorkFlag then S:=TWork.Read(1,_t(F)) else begin
         md:=NewLMode(RdMode); S:=CFile^.TF^.Read(1,_t(F)); OldLMode(md) end;
         if F^.Flg and f_Encryp<>0 then Code(S^.A,S^.LL);
         if IsNullValue(@S^.A,S^.LL) then begin S^.LL:=0; ReleaseAfterLongStr(S) end end;
    end;
    _LongS:=S; end
   else _LongS:=RunLongStr(F^.Frml);
  end;
function _RforD(F:FieldDPtr;P:pointer):float;
var r:float; s:string; i,err:integer;
begin
  r:=0; s[0]:=chr(F^.NBytes); Move(P^,s[1],length(s));
  case F^.Typ of
    'F':begin ReplaceChar(s,',','.');
          if F^.Flg and f_Comma<>0 then begin
            i:=Pos('.',s); if i>0 then Delete(s,i,1) end;
          val(LeadChar(' ',TrailChar(' ',s)),r,err) end;
    'D':r:=ValDate(s,'YYYYMMDD'); end;
  _RforD:=r;
end;
function _R(F:FieldDPtr):float;
var p:pointer; r:float; O:word absolute p; IP:^integer absolute p;
label 1;
begin
  if F^.Flg and f_Stored<>0 then begin
    p:=CRecPtr; inc(O,F^.Displ);
    if CFile^.Typ='D' then _R:=_RforD(F,p) else
    case F^.Typ of
     'F':begin r:=RealFromFix(p^,F^.NBytes);
           if F^.Flg and f_Comma=0 then _R:=r/Power10[F^.M] else _R:=r end;
     'D':if CFile^.Typ='8' then
           if IP^=0 then _R:=0.0 else _R:=IP^+FirstDate
         else goto 1;
     'R':
1:       if IsNullValue(p,F^.NBytes) then _R:=0. else _R:=RealPtr(p)^ end;
    end
  else _R:=RunReal(F^.Frml);
end;

function _B(F:FieldDPtr):boolean;
var p:pointer; O:word absolute p; CP:^char absolute p;
begin
  if F^.Flg and f_Stored<>0 then begin
    p:=CRecPtr;inc(O,F^.Displ);
    if CFile^.Typ='D' then _B:=CP^ in ['Y','y','T','t'] else
    if (CP^=#0) or (CP^=#$ff) then _B:=false else _B:=true end
  else _B:=RunBool(F^.Frml);
end;

function _T(F:FieldDPtr):longint;
type
  A10=array[1..10]of char;
var p:pointer; n:longint; err:integer;
  O:word absolute p; LP:^longint absolute p; P10:^A10 absolute p;
begin
  p:=CRecPtr; inc(O,F^.Displ);
  if CFile^.Typ='D' then begin n:=0; val(LeadChar(' ',P10^),n,err);_T:=n end
  else if IsNullValue(p,4) then _T:=0 else _T:=LP^;
end;

procedure S_(F:FieldDPtr;S:string);
var
  p:pointer; O:word absolute p; RP:^float absolute p;
  i,L,M:integer; Pos:longint; ss:LongStrPtr;
const LeftJust=1;
begin
  if F^.Flg and f_Stored<>0 then begin
    p:=CRecPtr; inc(O,F^.Displ); L:=F^.L; M:=F^.M;
    case F^.Typ of
     'A':begin while length(S)<L do
           if M=LeftJust then S:=S+' ' else S:=' '+S; i:=1;
           if (length(S)>L) and (M<>LeftJust) then i:=length(S)+1-L;
           move(S[i],P^,L); if F^.Flg and f_Encryp<>0 then Code(P^,L) end;
     'N':begin while length(S)<L do
           if M=LeftJust then S:=S+'0' else S:='0'+S; i:=1;
           if (length(S)>L) and (M<>LeftJust) then i:=length(S)+1-L;
           Pack(S[i],P^,L) end;
     'T':begin ss:=CopyToLongStr(S); LongS_(F,ss); ReleaseStore(ss) end;
end end end;

procedure LongS_(F:FieldDPtr;S:LongStrPtr);
var Pos:longint; md:LockMode;
label 1;
begin
  if F^.Flg and f_Stored<>0 then if S^.LL=0 then t_(F,0) else begin
   if F^.Flg and f_Encryp<>0 then Code(S^.A,S^.LL); {$ifdef FandSQL}
   if CFile^.IsSQLFile then begin SetTWorkFlag; goto 1 end else  {$endif}
   if HasTWorkFlag then
1:     Pos:=TWork.Store(S) else begin
   md:=NewLMode(WrMode); Pos:=CFile^.TF^.Store(S); OldLMode(md) end;
   if F^.Flg and f_Encryp<>0 then Code(S^.A,S^.LL); t_(F,Pos) end;
end;

procedure R_(F:FieldDPtr;R:float);
var p:pointer; s:string; m:word; l:longint;
    O:word absolute P; IP:^integer absolute P;
begin
  if F^.Flg and f_Stored<>0 then begin
    p:=CRecPtr; inc(O,F^.Displ); m:=F^.M;
    case F^.Typ of
     'F':begin
         if CFile^.Typ='D' then begin
           if F^.Flg and f_Comma<>0 then R:=R/Power10[m];
           str(R:F^.NBytes:m,s); move(s[1],p^,F^.NBytes) end
         else begin if F^.Flg and f_Comma=0 then R:=R*Power10[m];
           FixFromReal(R,P^,F^.NBytes) end end;
     'D':case CFile^.Typ of
          '8':if trunc(R)=0 then IP^:=0 else IP^:=trunc(R-FirstDate);
          'D':begin s:=StrDate(R,'YYYYMMDD'); move(s[1],p^,8) end;
         else RealPtr(p)^:=R end;
     'R': RealPtr(p)^:=R;
end end end;

procedure B_(F:FieldDPtr;B:boolean);
var p:pointer;
    O:word absolute p; BP:^boolean absolute p; CP:^char absolute p;
begin
  if (F^.Typ='B') and (F^.Flg and f_Stored<>0) then begin
    p:=CRecPtr; inc(O,F^.Displ);
    if CFile^.Typ='D' then if B then CP^:='T' else CP^:='F'
    else BP^:=B end;
end;

procedure T_(F:FieldDPtr;Pos:longint);
var p:pointer; s:string;
    O:word absolute p; LP:^longint absolute p;
begin
  if (F^.Typ='T') and (F^.Flg and f_Stored<>0) then begin
    p:=CRecPtr; inc(O,F^.Displ);
    if CFile^.Typ='D' then
      if Pos=0 then FillChar(p^,10,' ')
      else begin str(Pos:10,s); move(s[1],p^,10) end
    else LP^:=Pos end
  else RunError(906);
end;

procedure ZeroAllFlds;
var F:FieldDPtr;
begin
  fillchar(CRecPtr^,CFile^.RecLen,0);
  F:=CFile^.FldD; while F<>nil do begin
    if (F^.Flg and f_Stored<>0) and (F^.Typ='A') then s_(F,'');
    F:=F^.Chain end;
end;

procedure DelTFld(F:FieldDPtr);
var n:longint; md:LockMode;
begin
  n:=_t(F);
  if HasTWorkFlag then TWork.Delete(n) else begin
  md:=NewLMode(WrMode); CFile^.TF^.Delete(n); OldLMode(md) end;
  t_(F,0);
end;

procedure DelDifTFld(Rec,CompRec:pointer;F:FieldDPtr);
var n:longint; cr:pointer;
begin
  cr:=CRecPtr; CRecPtr:=CompRec; n:=_t(F); CRecPtr:=Rec;
  if n<>_t(F) then DelTFld(F); CRecPtr:=cr;
end;
procedure ClearRecSpace(p:pointer);
var f:FieldDPtr; cr:pointer;
begin
  if CFile^.TF<>nil then begin cr:=CRecPtr; CRecPtr:=p;
    if HasTWorkFlag then begin f:=CFile^.FldD; while f<>nil do begin
        if (f^.Flg and f_Stored<>0) and (f^.Typ='T') then begin
          TWork.Delete(_t(f)); t_(f,0) end;
        f:=f^.Chain end end;
    CRecPtr:=cr end;
end;
procedure DelAllDifTFlds(Rec,CompRec:pointer);
var F:FieldDPtr;
begin
  F:=CFile^.FldD; while F<>nil do begin
    if (F^.Typ='T') and (F^.Flg and f_Stored<>0) then DelDifTFld(Rec,CompRec,F);
    F:=F^.Chain end;
end;

procedure DelTFlds;
var F:FieldDPtr;
begin
  F:=CFile^.FldD; while F<>nil do begin
    if (F^.Flg and f_Stored<>0) and (F^.Typ='T') then DelTFld(F); F:=F^.Chain end;
end;
procedure CopyRecWithT(p1,p2:pointer);
var F:FieldDPtr; s:LongStrPtr; pos:longint; tf1,tf2:TFilePtr;
begin
  Move(p1^,p2^,CFile^.RecLen); F:=CFile^.FldD; while F<>nil do begin
    if (F^.Typ='T') and (F^.Flg and f_Stored<>0) then begin
      tf1:=CFile^.TF; tf2:=tf1; CRecPtr:=p1;
      if (tf1^.Format<>T00Format) then begin
        s:=_LongS(F); CRecPtr:=p2; LongS_(F,s); ReleaseStore(s) end
      else begin
        if HasTWorkFlag then tf1:=@TWork; pos:=_t(F);
        CRecPtr:=p2; if HasTWorkFlag then tf2:=@TWork;
        pos:=CopyTFString(tf2,CFile,tf1,pos); t_(F,pos) end end;
    F:=F^.Chain end;
end;

