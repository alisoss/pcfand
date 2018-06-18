{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}

unit runproc;

interface

uses access,rdrun;

procedure ReportProc(RO:RprtOptPtr;Save:boolean);
procedure RunProcedure(PDRoot:pointer);
procedure CallProcedure(PD:InstrPtr);
procedure RunMainProc(RP:RdbPos;NewWw:boolean);
procedure RunInstr(PD:InstrPtr);
procedure ResetCatalog;

implementation

uses dos,
{$ifdef FandGraph} graph, {$endif}
     drivers,base,obase,obaseww,oaccess,olongstr,
{$ifdef FandSQL}   channel, {$endif}
     runfrml,wwmenu,wwmix,
     compile,rdfildcl,rdmerg,rdrprt,rdproc,
     sort,runmerg,runrprt,genrprt,printtxt,
     editor,expimp,runedi,runproj,runfand
{$ifdef FandGraph} ,rungraph {$endif}
{$ifdef FandProlog},rdprolg,runprolg {$endif}
     ;

procedure Ovr; far;
assembler;
asm   pop ax; pop ax; pop ax{bp}; push ax; push ax; call StackOvr;
      pop bp; pop ds; pop ax; pop dx; pop sp; push cs; push ax;
end;

procedure UserHeadLine(UserHeader:string);
var n,l,maxlen:word; p:pointer;
begin
  p:=PushWParam(1,1,TxtCols,1,true); TextAttr:=colors.fNorm; ClrEol;
  maxlen:=TxtCols-10; l:=LenStyleStr(UserHeader);
  if l>=maxlen then begin
    UserHeader[0]:=char(maxlen);l:=LenStyleStr(UserHeader) end;
  n:=(TxtCols-l)div 2; if n>0 then write(' ':n);
  WrStyleStr(UserHeader,colors.fNorm);
  GotoXY(TxtCols-10,1);write(StrDate(Today,'DD.MM.YYYY'));
  PopWParam(p); ReleaseStore(p);
end;


procedure ReportProc(RO:RprtOptPtr; save:boolean);
procedure PromptAutoRprt(RO:RprtOptPtr);
  var FL:FieldList; F:FieldDPtr; RO2:RprtOptPtr;
  begin RO2:=GetStore(sizeof(RO^));Move(RO^,RO2^,sizeof(RO^));
       FL:=RO^.Flds;while FL<>nil do
           begin F:=FL^.FldD;
                 if F^.Flg and f_Stored<>0 then PutSelect(F^.Name)
                 else PutSelect(SelMark+F^.Name);
                 FL:=FL^.Chain end;
        CFile:=RO^.FDL.FD; if not SelFieldList(36,true,RO2^.Flds) then exit;
        if (RO^.FDL.Cond=nil) and
           not PromptFilter('',RO2^.FDL.Cond,RO2^.CondTxt) then exit;
        if SelForAutoRprt(RO2) then RunAutoReport(RO2);
  end;
var p,p2:pointer; md:char; w:longint; er:ExitRecord;
label 1,2;
begin
  MarkBoth(p,p2); PrintView:=false; with RO^ do begin
    if Flds=nil then begin
      SetInpTT(RprtPos,true);
      if SyntxChk then begin
        IsCompileErr:=false; NewExit(Ovr,er); goto 1; ReadReport(RO);
        LastExitCode:=0;
1:      RestoreExit(er); IsCompileErr:=false; goto 2 end;
      ReadReport(RO); RunReport(RO) end
    else begin
      if WidthFrml<>nil then Width:=RunInt(WidthFrml);
      if Head<>nil then HeadTxt:=RunLongStr(Head);
      if RO^.UserSelFlds then PromptAutoRprt(RO) else RunAutoReport(RO) end;
    if Edit then md:='T' else md:='V' end;
  if save then SaveFiles;
  if PrintView then begin
    w:=PushW(1,1,TxtCols,TxtRows); SetPrintTxtPath;
    EditTxtFile(nil,md,'',nil,0,0,nil,0,'',0,nil); PopW(w) end;
2:ReleaseBoth(p,p2);
end;


procedure AssignField(PD:InstrPtr);
var N:longint; md:LockMode; msg:word; F:FieldDPtr;
label 1;
begin
  CFile:=PD^.FD; md:=NewLMode(WrMode); F:=PD^.FldD;
  N:=RunInt(PD^.RecFrml);
  if (N<=0) or (N>CFile^.NRecs) then begin msg:=640;goto 1 end;
  CRecPtr:=GetRecSpace; ReadRec(N);
  if PD^.Indexarg and not DeletedFlag then begin
    msg:=627;
1:  Set2MsgPar(CFile^.Name,F^.Name); RunErrorM(md,msg) end;
  AssgnFrml(F,PD^.Frml,true,PD^.Add); WriteRec(N);
  ReleaseStore(CRecPtr); OldLMode(md);
end;
procedure AssignRecVar(LV1,LV2:LocVarPtr;A:AssignDPtr);
const EmptyStr:string[1]='';
var F:FieldDPtr; Z:FrmlPtr; S:LongStrPtr; ss:string; b:boolean; r:float;
    FD1,FD2:FileDPtr; RP1,RP2:pointer;
begin
  FD1:=LV1^.FD; FD2:=LV2^.FD; RP1:=LV1^.RecPtr; RP2:=LV2^.RecPtr;
  while A<>nil do begin
  case A^.Kind of
   _zero:begin
     F:=A^.FldD; CFile:=FD1; CRecPtr:=RP1; case F^.FrmlTyp of
     'S':s_(F,EmptyStr);'R':r_(F,0.0);else b_(F,False) end end;
   _output:begin
       CFile:=FD1; CRecPtr:=RP1; A^.Frml^.NewRP:=RP2;
       AssgnFrml(A^.OFldD,A^.Frml,false,false) end end;
  A:=A^.Chain end;
  CFile:=FD1; CRecPtr:=RP1; SetUpdFlag;
end;
procedure AssignRecFld(PD:InstrPtr);
var F:FieldDPtr;
begin
  F:=PD^.RecFldD; with PD^.AssLV^ do begin CFile:=FD; CRecPtr:=RecPtr end;
  SetUpdFlag; AssgnFrml(F,PD^.Frml,HasTWorkFlag,PD^.Add);
end;

procedure SortProc(FD:FileDPtr;SK:KeyFldDPtr);
  var md:LockMode;
  begin CFile:=FD; md:=NewLMode(ExclMode);
        SortAndSubst(SK); CFile:=FD; OldLMode(md); SaveFiles end;
procedure MergeProc(PD:InstrPtr);
var p,p2:pointer;
begin
  MarkBoth(p,p2); SetInpTT(PD^.Pos,true);
  ReadMerge; RunMerge; SaveFiles; ReleaseBoth(p,p2);
end;
procedure WritelnProc(PD:InstrPtr);
var S:LongStrPtr; i:word; c:char; LF:byte; W:WrLnDPtr; t,x:string; r:float;
label 1,2,3;
begin
  W:=@PD^.WD; LF:=PD^.LF; t[0]:=#0;
  TextAttr:=ProcAttr; while W<>nil do begin
    case W^.Typ of
     'S':begin if LF>=2 then t:=t+RunShortStr(W^.Frml) else begin
           S:=RunLongStr(W^.Frml); WrLongStyleStr(S,ProcAttr);
           ReleaseStore(S) end;
         goto 1 end;
     'B':if RunBool(W^.Frml) then x:=AbbrYes else x:=AbbrNo;
     'F':begin r:=RunReal(W^.Frml); if W^.M=255 then str(r:W^.N,x)
         else str(r:W^.N:W^.M,x) end;
     'D':x:=StrDate(RunReal(W^.Frml),W^.Mask^) end;
    if LF>=2 then t:=t+x else write(x);
1:  W:=W^.Chain end;
2:case LF of
    1:writeln;
    3:begin F10SpecKey:=_F1_; goto 3 end;
    2:begin
3:    SetMsgPar(t); WrLLF10Msg(110); if KbdChar=_F1_ then begin
        Help(PD^.mHlpRdb,RunShortStr(PD^.mHlpFrml),false); goto 2 end end end;
end;
procedure DisplayProc(R:RdbDPtr;IRec:word);
var S:LongStrPtr; p:pointer; i:word;
label 1;
begin
  MarkStore(p);
  if IRec=0 then begin
    S:=GetHlpText(CRdb,RunShortStr(FrmlPtr(R)),true,i); if S=nil then goto 1 end
  else begin
    CFile:=R^.FD; CRecPtr:=Chpt^.RecPtr; ReadRec(IRec);
    S:=CFile^.TF^.Read(1,_t(ChptTxt));
    if R^.Encrypted then CodingLongStr(S) end;
  WrLongStyleStr(S,ProcAttr);
1:ReleaseStore(p);
end;
procedure ClrWwProc(PD:InstrPtr);
var v:WRect; a:word; s:string; c:char;
begin
  RunWFrml(PD^.W,0,v); a:=RunWordImpl(PD^.Attr,colors.uNorm); c:=' ';
  if PD^.FillC<>nil then begin
    s:=RunShortStr(PD^.FillC); if length(s)>0 then c:=s[1] end;
  ScrClr(v.C1-1,v.R1-1,v.C2-v.C1+1,v.R2-v.R1+1,c,a);
end;
procedure ExecPgm(PD:InstrPtr);
var s:string; Prog:PathStr; i:word; x,y:byte; b:boolean;
    wmin,wmax:word; w,crs:longint;
begin
  wmin:=word(WindMin); wmax:=word(WindMax); crs:=CrsGet;
  w:=PushW(1,1,TxtCols,1);
  word(WindMin):=wmin; word(WindMax):=wmax; CrsSet(crs);
  s:=RunShortStr(PD^.Param); i:=PD^.ProgCatIRec; CVol:='';
  if i<>0 then Prog:=RdCatField(i,CatPathName) else Prog:=PD^.ProgPath^;
  b:=OSShell(Prog,s,PD^.NoCancel,PD^.FreeMm,PD^.LdFont,PD^.TextMd);
  asm mov ah,3; mov bh,0; push bp; int 10H; pop bp; mov x,dl; mov y,dh end;
  PopW(w); gotoxy(x-WindMin.X+1,y-WindMin.Y+1);
  if not b then GoExit;
end;
procedure CallRdbProc(PD:InstrPtr);
var b:boolean; p:pointer; bp:ProcStkPtr;
begin
  MarkStore(p); bp:=MyBP;
  b:=EditExecRdb(PD^.RdbNm^,PD^.ProcNm^,PD^.ProcCall);
  SetMyBP(bp); ReleaseStore(p); if not b then GoExit;
end;

procedure IndexfileProc(FD:FileDPtr;Compress:boolean);
  var FD2,cf:FileDPtr; I:longint; md:LockMode;
  begin cf:=CFile; CFile:=FD; md:=NewLMode(ExclMode);
    XFNotValid; CRecPtr:=GetRecSpace;
    if Compress then begin FD2:=OpenDuplF(false); for I:=1 to FD^.NRecs do
      begin CFile:=FD;ReadRec(I);if not DeletedFlag then
            begin CFile:=FD2;PutRec end end;
      if not SaveCache(0) then GoExit;CFile:=FD;SubstDuplF(FD2,false) end;
    CFile^.XF^.NoCreate:= false; TestXFExist;
    OldLMode(md); SaveFiles;
    ReleaseStore(CRecPtr); CFile:=cf end;
procedure MountProc(CatIRec:word;NoCancel:boolean);
  var er:ExitRecord;
  label 1;
  begin NewExit(Ovr,er);goto 1;SaveFiles;
        RdCatPathVol(CatIRec); TestMountVol(CPath[1]);
        LastExitCode:=0; RestoreExit(er); exit;
      1:RestoreExit(er);
        if NoCancel then LastExitCode:=1 else GoExit end;
procedure EditProc(PD:InstrPtr);
var EO:EditOptPtr;
begin
  EdUpdated:=false; SaveFiles; CFile:=PD^.EditFD;
  EO:=GetStore(sizeof(EO^)); Move(PD^.EO^,EO^,sizeof(EO^));
  if not EO^.UserSelFlds or SelFldsForEO(EO,nil) then EditDataFile(CFile,EO);
  SaveFiles; ReleaseStore(EO);
end;
procedure EditTxtProc(PD:InstrPtr);
function GetStr(Z:FrmlPtr):StringPtr;
var s:string;
begin
  if Z=nil then GetStr:=nil else begin s:=RunShortStr(Z); GetStr:=StoreStr(s) end;
end;
var i:longint; v:WRect; pv:WRectPtr; a:byte; lp:LongintPtr;
    MsgS:MsgStr; p:pointer; msg:string;
begin
  MarkStore(p);
  i:=1; if PD^.TxtPos<>nil then i:=RunInt(PD^.TxtPos); EdUpdated:=false;
  a:=RunWordImpl(PD^.Atr,0);
  pv:=nil; if PD^.Ww.C1<>nil then begin RunWFrml(PD^.Ww,PD^.WFlags,v); pv:=@v end;
  with MsgS do begin Head:=GetStr(PD^.Head);Last:=GetStr(PD^.Last);
     CtrlLast:=GetStr(PD^.CtrlLast); ShiftLast:=GetStr(PD^.ShiftLast);
     AltLast:=GetStr(PD^.AltLast) end;
  if PD^.TxtLV<>nil then lp:=Ptr(Seg(MyBP^),Ofs(MyBP^)+PD^.TxtLV^.BPOfs)
  else begin SetTxtPathVol(PD^.TxtPath,PD^.TxtCatIRec); lp:=nil end;
  msg:=''; if PD^.ErrMsg<>nil then msg:=RunShortStr(PD^.ErrMsg);
  EditTxtFile(lp,PD^.EdTxtMode,msg,PD^.ExD,I,RunInt(PD^.TxtXY),pv,a,RunShortStr(PD^.Hd),PD^.WFlags,@MsgS);
  ReleaseStore(p);
end;
procedure PrintTxtProc(PD:InstrPtr);
var s:LongStrPtr;
begin with PD^ do begin
  if TxtLV<>nil then begin
    s:=TWork.Read(1,LongintPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+TxtLV^.BPOfs))^);
    PrintArray(@s^.A,s^.LL,false); ReleaseStore(s) end
  else begin SetTxtPathVol(TxtPath,TxtCatIRec); PrintTxtFile(0) end;
end end;

function SrchXKey(K:KeyDPtr;var X:XString;var N:longint):boolean;
var cr:pointer;
begin
  if CFile^.Typ='X' then begin
    TestXFExist; SrchXKey:=K^.SearchIntvl(x,false,N) end
  else begin cr:=CRecPtr; CRecPtr:=GetRecSpace;
    SrchXKey:=SearchKey(x,K,N); ReleaseStore(CRecPtr); CRecPtr:=cr end;
end;

procedure DeleteRecProc(PD:InstrPtr);
var md:LockMode; n:longint; x:XString;
label 1,2;
begin
  CFile:=PD^.RecFD; CRecPtr:=GetRecSpace;
  if PD^.ByKey then begin x.S:=RunShortStr(PD^.RecNr);
{$ifdef FandSQL} if CFile^.IsSQLFile then begin
     Strm1^.DeleteXRec(PD^.Key,@x,PD^.AdUpd); goto 2 end;
{$endif}  end;
  md:=NewLMode(DelMode);
  if PD^.ByKey then begin if not SrchXKey(PD^.Key,x,n) then goto 1 end
  else begin n:=RunInt(PD^.RecNr); if (n<=0) or (n>CFile^.NRecs) then goto 1 end;
  ReadRec(n);
  if PD^.AdUpd and not DeletedFlag then LastExitCode:=ord(not RunAddUpdte('-',nil,nil));
  if CFile^.Typ='X' then begin if not DeletedFlag then DeleteXRec(n,true) end
  else DeleteRec(n);
1:OldLMode(md);
2:ReleaseStore(CRecPtr);
end;
procedure RecallRecProc(PD:InstrPtr);
var md:LockMode; n:longint;
begin
  CFile:=PD^.RecFD; if CFile^.Typ<>'X' then exit;
  n:=RunInt(PD^.RecNr); CRecPtr:=GetRecSpace;
  md:=NewLMode(CrMode); if (N>0) and (N<=CFile^.NRecs) then begin
    ReadRec(N); if DeletedFlag then begin RecallRec(N);
      if PD^.AdUpd then LastExitCode:=ord(not RunAddUpdte('+',nil,nil)) end end;
  OldLMode(md); ReleaseStore(CRecPtr);
end;
procedure AppendRecProc;
var md:LockMode;
begin
  md:=NewLMode(CrMode); CRecPtr:=GetRecSpace; ZeroAllFlds;
  SetDeletedFlag; CreateRec(CFile^.NRecs+1);
  ReleaseStore(CRecPtr); OldLMode(md);
end;
procedure UpdRec(CR:pointer;N:longint;AdUpd:boolean);
var cr2:pointer; del:boolean;
begin
  cr2:=GetRecSpace; CRecPtr:=cr2; ReadRec(N); del:=DeletedFlag; CRecPtr:=cr;
  if AdUpd then
    if del then LastExitCode:=ord(not RunAddUpdte('+',nil,nil))
    else LastExitCode:=ord(not RunAddUpdte('d',cr2,nil));
  if CFile^.Typ='X' then OverWrXRec(N,cr2,cr) else WriteRec(N);
  if not del then DelAllDifTFlds(cr2,nil);
  ReleaseStore(cr2);
end;
procedure ReadWriteRecProc(IsRead:boolean;PD:InstrPtr);
var N:longint; app,ad:boolean; md:LockMode; cr:pointer; x:XString;
    k:KeyDPtr; msg:word;
label 0,1,2,3,4;
begin with PD^.LV^ do begin
  CFile:=FD; CRecPtr:=RecPtr; N:=1; k:=PD^.Key; ad:=PD^.AdUpd;
  md:=CFile^.LMode; app:=false; cr:=GetRecSpace;
  if PD^.ByKey then begin
    x.S:=RunShortStr(PD^.RecNr);                            {$ifdef FandSQL}
    if CFile^.IsSQLFile then begin
      if IsRead then if Strm1^.SelectXRec(k,@x,PD^.CompOp,true) then goto 4 else goto 2
      else if Strm1^.UpdateXRec(k,@x,ad) then goto 4 else goto 2 end; {$endif}
    end
  else N:=RunInt(PD^.RecNr);
  if IsRead then
    if N=0 then goto 0 else NewLMode(RdMode)
  else if N=0 then begin                                       {$ifdef FandSQL}
    if CFile^.IsSQLFile then begin Strm1^.InsertRec(ad,true); goto 4 end; {$endif}
    goto 1 end
  else NewLMode(WrMode);
  if PD^.ByKey then begin
    if k=nil{IsParFile} then begin
      if CFile^.NRecs=0 then
        if IsRead then begin
0:        DelTFlds; ZeroAllFlds; goto 4 end
        else begin
1:        NewLMode(CrMode); TestXFExist; IncNRecs(1); app:=true end;
      n:=CFile^.NRecs end
    else if not SrchXKey(k,x,n) then begin
2:    if IsRead then begin DelTFlds; ZeroAllFlds; SetDeletedFlag; goto 4 end;
      msg:=613; goto 3 end end
  else if (n<=0) or (n>CFile^.NRecs) then begin msg:=641;
3:  SetMsgPar(Name); RunErrorM(md,msg) end;
  if IsRead then begin
    CRecPtr:=cr; ReadRec(N);
    CRecPtr:=RecPtr; DelTFlds; CopyRecWithT(cr,RecPtr) end
  else begin
    CopyRecWithT(RecPtr,cr);
    if app then begin
      CRecPtr:=cr; if CFile^.Typ='X' then RecallRec(N) else WriteRec(N);
      if ad then LastExitCode:=ord(not RunAddUpdte('+',nil,nil)) end
    else UpdRec(cr,N,ad) end;
4:ReleaseStore(cr); OldLMode(md);
end end;
procedure LinkRecProc(PD:InstrPtr);
var p,r2,lr2:pointer; cf:FileDPtr; cr:pointer; ld:LinkDPtr; n:longint;
begin
  cf:=CFile; cr:=CRecPtr; MarkStore(p);
  ld:=PD^.LinkLD; CRecPtr:=PD^.RecLV1^.RecPtr; lr2:=PD^.RecLV2^.RecPtr;
  CFile:=ld^.ToFD; ClearRecSpace(lr2); CFile:=ld^.FromFD;
  if LinkUpw(ld,n,true) then LastExitCode:=0 else LastExitCode:=1;
  r2:=CRecPtr; CRecPtr:=lr2; DelTFlds; CopyRecWithT(r2,lr2);
  ReleaseStore(p); CFile:=cf; CRecPtr:=cr;
end;
procedure ForAllProc(PD:InstrPtr);
var FD:FileDPtr; Key,k:KeyDPtr; Bool:FrmlPtr; LD:LinkDPtr; KI:KeyInDPtr;
    cr,p,lr:pointer;  Scan:XScanPtr; md,md1:LockMode; xx:XString;
    KF:KeyFldDPtr; LVi,LVr:LocVarPtr; lk,b:boolean;
    {$ifdef FandSQL} sql:boolean; {$endif}
label 1;
begin
  MarkStore(p); FD:=PD^.CFD; Key:=PD^.CKey; LVi:=PD^.CVar; LVr:=PD^.CRecVar;
  LD:=PD^.CLD; KI:=PD^.CKIRoot; Bool:=RunEvalFrml(PD^.CBool); lk:=false;
  {$ifdef FandSQL}if PD^.inSQL and not FD^.IsSQLFile then exit; {$endif}
  if LD<>nil then begin CFile:=LD^.ToFD; KF:=LD^.ToKey^.KFlds;
    case PD^.COwnerTyp of
     'r':begin CRecPtr:=PD^.CLV^.RecPtr; xx.PackKF(KF) end;
     'F':begin md:=NewLMode(RdMode); CRecPtr:=GetRecSpace;
               ReadRec(RunInt(FrmlPtr(PD^.CLV))); xx.PackKF(KF);
               ReleaseStore(p); OldLMode(md) end end end;
  CFile:=FD; {$ifdef FandSQL}sql:=CFile^.IsSQLFile;{$endif}
  md:=NewLMode(RdMode);
  cr:=GetRecSpace; CRecPtr:=cr; lr:=cr;
  New(Scan,Init(CFile,Key,KI,true));             {$ifdef FandSQL}
  if PD^.inSQL then Scan^.ResetSQLTxt(Bool) else {$endif}
  if LD<>nil then
    if PD^.COwnerTyp='i' then Scan^.ResetOwnerIndex(LD,PD^.CLV,Bool)
    else Scan^.ResetOwner(@xx,Bool)
  else Scan^.Reset(Bool,PD^.CSQLFilter);         {$ifdef FandSQL}
  if not CFile^.IsSQLFile then                   {$endif}
  if Key<>nil then
    if PD^.CWIdx then ScanSubstWIndex(Scan,Key^.KFlds,'W')
    else begin inc(CFile^.XF^.UpdLockCnt); lk:=true end;
  if LVr<>nil then lr:=LVr^.RecPtr; k:=CFile^.Keys;
  b:=PD^.CProcent; if b then RunMsgOn('F',Scan^.NRecs);
1:{$ifdef FandSQL}if sql then CRecPtr:=lr else {$endif}CRecPtr:=cr;
  Scan^.GetRec; if b then RunMsgN(Scan^.IRec);
  if not Scan^.EOF then begin  {$ifdef FandSQL}
    if sql then begin ClearUpdFlag; if k<>nil then xx.PackKF(k^.KFlds) end else{$endif}
    if LVr<>nil then begin
      CRecPtr:=lr; ClearUpdFlag; DelTFlds; CopyRecWithT(cr,lr) end;
    if LVi<>nil then FloatPtr(LocVarAd(LVi))^:=Scan^.RecNr;
    RunInstr(PD^.CInstr); CFile:=FD; CRecPtr:=lr;          {$ifdef FandSQL}
    if sql then begin if HasUpdFlag and not PD^.inSQL then begin
      if k=nil then CFileError(650); Strm1^.UpdateXRec(k,@xx,CFile^.Add<>nil) end end else    {$endif}
    begin OpenCreateF(Shared);
      if (LVr<>nil) and (LVi=nil) and HasUpdFlag then begin
        md1:=NewLMode(WrMode); CopyRecWithT(lr,cr);
        UpdRec(cr,Scan^.RecNr,true); OldLMode(md1) end end;
    if not (ExitP or BreakP) then begin
      if {$ifdef FandSQL}not sql and {$endif}(Key=nil) and (Scan^.NRecs>CFile^.NRecs) then begin
          dec(Scan^.IRec); dec(Scan^.NRecs) end;
      goto 1 end end;
  if lk then dec(CFile^.XF^.UpdLockCnt);
  Scan^.Close; OldLMode(md); if b then RunMsgOff;
  ReleaseStore(p); BreakP:=false;
end;

procedure HeadLineProc(Z:FrmlPtr);
  begin UserHeadLine(RunShortStr(Z)) end;
procedure SetKeyBufProc(Z:FrmlPtr);
  begin KbdBuffer:=RunShortStr(Z) end;
procedure SetWwViewPort;
var x1,x2,y1,y2:word;
begin {$ifdef FandGraph}
  if IsGraphMode then begin
    RectToPixel(WindMin.X,WindMin.Y,WindMax.X,WindMax.Y,x1,y1,x2,y2);
    SetViewPort(x1,y1,x2,y2,true) end; {$endif}
end;
procedure WithWindowProc(PD:InstrPtr);
var PAttr:byte; w1:longint; v:WRect;
begin
  PAttr:=ProcAttr; with PD^ do begin
    ProcAttr:=RunWordImpl(Attr,colors.uNorm);
    RunWFrml(W,WithWFlags,v);
    w1:=PushWFramed(v.C1,v.R1,v.C2,v.R2,ProcAttr,RunShortStr(Top),'',WithWFlags);
    if (WithWFlags and WNoClrScr)=0 then ClrScr;
    SetWwViewPort;
    RunInstr(WwInstr);
    PopW2(w1,(WithWFlags and WNoPop)=0); SetWwViewPort end;
  ProcAttr:=PAttr;
end;
procedure WithLockedProc(PD:InstrPtr);
procedure UnLck(Ld1:LockDPtr;Op:PInstrCode);
var ld:LockDPtr;
begin
  ld:=@PD^.WLD; while ld<>Ld1 do begin
    CFile:=ld^.FD; if CFile^.IsShared then begin
      if op=_withlocked then UnLockN(ld^.N); OldLMode(ld^.OldMd) end;
    ld:=ld^.Chain end;
end;
var op:PInstrCode; ld:LockDPtr; w,w1:longint;
    msg:word; ntxt:string[10]; md:LockMode;
label 1,2,3;
begin
  op:=PD^.Kind; if op=_withlocked then begin ld:=@PD^.WLD; while ld<>nil do begin
    ld^.N:=RunInt(ld^.Frml); ld:=ld^.Chain end end;
  w:=0;
1:ld:=@PD^.WLD; while ld<>nil do begin
    CFile:=ld^.FD;
    if CFile^.Handle=$FF then
      if OpenF1(Shared) then
        if TryLMode(RdMode,md,2) then begin OpenF2; OldLMode(NullMode) end
        else begin CloseClearHCFile; goto 2 end
      else OpenCreateF(Shared);
    if CFile^.IsShared then begin
      if op=_withlocked then begin if TryLockN(ld^.N,2) then goto 3 end
      else begin if TryLMode(ld^.Md,ld^.OldMd,2) then goto 3 end;
2:    UnLck(ld,op);
      if PD^.WasElse then begin RunInstr(PD^.WElseInstr); exit end;
      CFile:=ld^.FD; SetCPathVol;
      if op=_withlocked then begin
        msg:=839; str(ld^.N,ntxt); Set2MsgPar(ntxt,CPath) end
      else begin msg:=825; Set2MsgPar(CPath,LockModeTxt[ld^.Md]) end;
      w1:=PushWrLLMsg(msg,false);
      if w=0 then w:=w1 else TWork.Delete(w1);
      beep; KbdTimer(spec.NetDelay,0); goto 1 end;
3:  ld:=ld^.Chain end;
  if w<>0 then PopW(w);
  RunInstr(PD^.WDoInstr);
  UnLck(nil,op);
end;
procedure HelpProc(PD:InstrPtr);
begin
  Help(PD^.HelpRdb,RunShortStr(PD^.Frml),true);
end;
function OpenHForPutTxt(PD:InstrPtr):word;
var m:FileOpenMode; h:word;
begin
  SetTxtPathVol(PD^.TxtPath,PD^.TxtCatIRec); TestMountVol(CPath[1]);
  m:=_isoverwritefile; if PD^.App then m:=_isoldnewfile;
  h:=OpenH(m,Exclusive); TestCPathError;
  if PD^.App then SeekH(h,FileSizeH(h)); OpenHForPutTxt:=h;
end;
procedure PutTxt(PD:InstrPtr);
var h:word; s:LongStrPtr; z:FrmlPtr; pth:string;
begin
  z:=PD^.Txt; if CanCopyT(nil,z) then begin
    h:=OpenHForPutTxt(PD); pth:=CPath; CopyTFStringToH(h); CPath:=pth end
  else begin s:=RunLongStr(z); h:=OpenHForPutTxt(PD);
    WriteH(h,S^.LL,S^.A); ReleaseStore(s) end;
  CPath:=pth; TestCPathError; WriteH(h,0,h){trunc}; CloseH(h);
end;
procedure AssgnCatFld(PD:InstrPtr);
begin with PD^ do begin
  CFile:=FD3; if CFile<>nil then CloseFile;
  WrCatField(CatIRec,CatFld,RunShortStr(Frml3));
end end;
procedure AssgnAccRight(PD:InstrPtr);
begin AccRight:=RunShortStr(PD^.Frml) end;
procedure AssgnUserName(PD:InstrPtr);
begin UserName:=RunShortStr(PD^.Frml) end;

procedure ReleaseDriveProc(Z:FrmlPtr);
var s:string; c:char;
begin
  SaveFiles; s:=RunShortStr(Z); c:=upcase(s[1]);
  if c=spec.CPMDrive then ReleaseDrive(FloppyDrives) else
  if (c='A') or (c='B') then ReleaseDrive(ord(c)-ord('@'));
end;
procedure WithGraphicsProc(PD:InstrPtr);
var p:pointer;
begin
  MarkStore(p);
  if IsGraphMode then RunInstr(PD) else begin
    ScrGraphMode(true,0); SetWwViewPort;
    RunInstr(PD); ScrTextMode(true,false) end;
  ReleaseStore(p);
end;                                {$ifdef FandGraph}
procedure DrawProc(PD:InstrPtr);
var i1,i2,i3,i4,i5,i6,i7,wSize,wDir:integer; op:PInstrCode; c:word;
label 1;
begin with PD^ do begin
  if not IsGraphMode then exit; op:=Kind; HideMaus;
  i1:=RunInt(Par1); i2:=RunInt(Par2);
  if op<>_outtextxy then i3:=RunInt(Par3);
  if op=_putpixel then begin PutPixel(i1,i2,i3); goto 1 end;
  i4:=RunInt(Par4); i5:=RunInt(Par5);
  if op=_floodfill then begin
    if i3>0 then begin SetFillStyle(i3,i5); FloodFill(i1,i2,i4) end;
    goto 1 end;
  SetLineStyle(((i5 div 16) and $3),0,(i5 div 64)+2);
  c:=GetColor; SetColor(i5); case op of
    _line:      graph.Line(i1,i2,i3,i4);
    _rectangle: graph.Rectangle(i1,i2,i3,i4);
    _ellipse:   begin i6:=0; i7:=360; if PD^.Par6<>nil then begin
                   i6:=RunInt(Par6); i7:=RunInt(Par7) end;
                graph.Ellipse(i1,i2,i6,i7,i3,i4) end;
    _outtextxy: begin if Par6=nil then wSize:=4 else wSize:=RunInt(Par6);
                if Par7=nil then wDir:=0 else wDir:=RunInt(Par7);
                SetTextJustify(Lefttext,BottomText);
                SetTextStyle(maxw(1,minw(i4{Font},2)),wDir,wSize);
                if PD^.Par8<>nil then
                  SetUserCharSize(RunInt(Par8),RunInt(Par9),RunInt(Par10),RunInt(Par11));
                OutTextXY(i1,i2,RunShortStr(Par3));
                end end;
  SetColor(c);
  SetLineStyle(1,0,1);
1:ShowMaus;
end end;                                {$endif}

procedure ResetCatalog;
var cf:FileDPtr; r:RdbDPtr;
begin
  cf:=CFile; r:=CRdb; while CRdb<>nil do begin
    CFile:=CRdb^.FD^.Chain; while CFile<>nil do begin
      CloseFile; with CFile^ do CatIRec:=GetCatIRec(Name,Typ='0');
      {$ifdef FandSQL}SetIsSQLFile;{$endif} CFile:=CFile^.Chain end;
    CRdb:=CRdb^.ChainBack end;
  CFile:=cf; CRdb:=r;
end;  {$ifdef FandSQL}
procedure SQLProc(Z:FrmlPtr);
var s:LongStrPtr;
begin
  if Strm1=nil then exit;
  s:=RunLongStr(Z); Strm1^.SendTxt(s,true); ReleaseStore(s);
end;
procedure StartLogIn(Nm,Pw:FrmlPtr);
begin
  if Strm1<>nil then Strm1^.Login(RunShortStr(Nm),RunShortStr(Pw));
end;
procedure SQLRdWrTxt(PD:InstrPtr);
var xx:XString;
begin
  if Strm1<>nil then with PD^ do begin
  SetTxtPathVol(TxtPath,TxtCatIRec); TestMountVol(CPath[1]);
  xx.S:=RunShortStr(sqlXStr); CFile:=sqlFD;
  if IsRead then Strm1^.ReadTFld(sqlKey,@xx,sqlFldD)
  else Strm1^.WriteTFld(sqlKey,@xx,sqlFldD) end
end; {$endif}
procedure PortOut(IsWord:boolean;Port:word;What:word); assembler;
  asm mov dx,Port; mov ax,What; cmp IsWord,0; je @1; out dx,ax; jmp @2;
@1:   out dx,al;
@2: end;

procedure WaitProc;
var w:word;
begin repeat GetEvent; w:=Event.What; ClrEvent
      until (w=evKeyDown) or (w=evMouseDown);
end;

{$ifndef FandRunV}
procedure MemDiagProc;
  begin writeln(hexptr(HeapPtr),'/',hexptr(HeapEnd),'/',
                hexptr(CacheEnd),'/',hexptr(Stack2Ptr),'/',hexw(sptr),'/',
                longint(XMSCachePages)*CachePageSize);wait end;
{$endif}


procedure RunInstr(PD:InstrPtr);
var s:LongStrPtr;
begin
  while not ExitP and not BreakP and (PD<>nil) do begin
  case PD^.Kind of
   _ifthenelseP:
      with PD^ do if RunBool(Bool) then RunInstr(Instr) else RunInstr(ElseInstr);
   _whiledo:with PD^ do begin while not ExitP and not BreakP and RunBool(Bool) do
            RunInstr(Instr); BreakP:=false end;
   _repeatuntil:with PD^ do begin repeat RunInstr(Instr) until
           ExitP or BreakP or RunBool(Bool);  BreakP:=false end;
   _menubox:MenuBoxProc(PD);
   _menubar:MenuBarProc(PD);
   _forall:ForAllProc(PD);
   _window:WithWindowProc(PD);
   _break:BreakP:=true;
   _exit:ExitP:=true;
   _cancel:GoExit;
   _save:SaveFiles;
   _clrscr:begin TextAttr:=ProcAttr;ClrScr end;
   _clrww:ClrWwProc(PD);
   _clreol:begin TextAttr:=ProcAttr;ClrEol end;
   _exec:ExecPgm(PD);
   _proc:CallProcedure(PD);
   _call:CallRdbProc(PD);
   _copyfile:CopyFile(PD^.CD);
   _headline:HeadLineProc(PD^.Frml);
   _setkeybuf:SetKeyBufProc(PD^.Frml);
   _writeln:WriteLnProc(PD);
   _gotoxy:with PD^ do gotoxy(RunInt(GoX),RunInt(GoY));
   _merge:MergeProc(PD);                     {$ifdef FandProlog}
   _lproc:RunProlog(PD^.lpPos,PD^.lpName);   {$endif}
   _report:ReportProc(PD^.RO,true);
   _sort:with PD^ do SortProc(SortFD,SK);
   _edit:EditProc(PD);
   _asgnloc:with PD^ do LVAssignFrml(LV,MyBP,Add,Frml);
   _asgnrecfld:AssignRecFld(PD);
   _asgnrecvar:with PD^ do AssignRecVar(RecLV1,RecLV2,Ass);
   _asgnpar:with PD^ do AsgnParFldFrml(FD,FldD,Frml,Add);
   _asgnfield: AssignField(PD);
   _asgnnrecs:with PD^ do begin CFile:=FD; AssignNRecs(Add,RunInt(Frml)) end;
   _appendrec:begin CFile:=PD^.RecFD; AppendRecProc end;
   _deleterec:DeleteRecProc(PD);
   _recallrec:RecallRecProc(PD);
   _readrec:ReadWriteRecProc(true,PD);
   _writerec:ReadWriteRecProc(false,PD);
   _linkrec:LinkRecProc(PD);
   _withshared,_withlocked:WithLockedProc(PD);
   _edittxt:EditTxtProc(PD);
   _printtxt:PrintTxtProc(PD);
   _puttxt:PutTxt(PD);
   _asgncatfield:AssgnCatFld(PD);
   _asgnusercode:begin UserCode:=RunInt(PD^.Frml);
       AccRight[0]:=#1; AccRight[1]:=char(UserCode) end;
   _asgnaccright: AssgnAccRight(PD);
   _asgnusername: AssgnUserName(PD);
   _asgnusertoday: userToday:=RunReal(PD^.Frml);
   _asgnclipbd:begin s:=RunLongStr(PD^.Frml); TWork.Delete(ClpBdPos);
       ClpBdPos:=TWork.Store(s); ReleaseStore(s) end;
   _asgnedok:EdOk:=RunBool(PD^.Frml);
   _turncat:with PD^ do begin CFile:=NextGenFD;
       TurnCat(FrstCatIRec,NCatIRecs,RunInt(TCFrml)) end;
   _releasedrive:ReleaseDriveProc(PD^.Drive);
   _setprinter:SetCurrPrinter(abs(RunInt(PD^.Frml)));
   _indexfile:with PD^ do IndexfileProc(IndexFD,Compress);
   _display:with PD^ do DisplayProc(Pos.R,Pos.IRec);
   _mount:with PD^ do MountProc(MountCatIRec,MountNoCancel);
   _clearkeybuf:ClearKbdBuf;
   _help:HelpProc(PD);
   _wait:WaitProc;
   _beep:beep;
   _delay:delay((RunInt(PD^.Frml)+27)div 55);
   _sound:sound(RunInt(PD^.Frml));
   _nosound:nosound;                                {$ifdef FandGraph}
   _graph:RunBGraph(PD^.GD,false);
   _putpixel,_line,_rectangle,_ellipse,
   _floodfill,_outtextxy: DrawProc(PD);             {$endif}
   _withgraphics: WithGraphicsProc(PD^.WDoInstr);   {$ifndef FandRunV}
   _memdiag:MemDiagProc;                            {$endif}
   _closefds:begin CFile:=PD^.clFD; if CFile=nil then ForAllFDs(ClosePassiveFD)
       else with CFile^ do if not IsShared or (LMode=NullMode) then CloseFile end;
   _backup:with PD^ do Backup(IsBackup,NoCompress,BrCatIRec,BrNoCancel);
   _backupm:BackupM(PD);
   _resetcat:ResetCatalog;
   _setedittxt:SetEditTxt(PD);
   _getindex:GetIndex(PD);
   _setmouse:with PD^ do SetMouse(RunInt(MouseX),RunInt(MouseY),RunBool(Show));
   _checkfile:with PD^ do begin SetTxtPathVol(cfPath,cfCatIRec); CheckFile(cfFD) end;
   {$ifdef FandSQL}
   _sql:SQLProc(PD^.Frml);
   _login:with PD^ do StartLogIn(liName,liPassWord);
   _sqlrdwrtxt:SQLRdWrTxt(PD);  {$endif}
   _asgnrand:RandSeed:=RunInt(PD^.Frml);
   _randomize:Randomize;
   _asgnxnrecs:PD^.xnrIdx^.Release;
   _portout:PortOut(RunBool(PD^.IsWord),word(RunInt(PD^.Port)),word(RunInt(PD^.PortWhat)));
  end;
  PD:=PD^.Chain end;
end;

procedure RunProcedure(PDRoot:pointer);
var ExP,BrkP:boolean;
begin
  ExP:=ExitP; BrkP:=BreakP; ExitP:=false; BreakP:=false;
  RunInstr(InstrPtr(PDRoot));
  ExitP:=ExP; BreakP:=BrkP;
end;

procedure CallProcedure(PD:InstrPtr);
var p,p1,p2,oldbp,oldprocbp:pointer; lv,lv1,lvroot:LocVarPtr; i,j,n:word;
    z:FrmlPtr; l:longint; pd1:InstrPtr; ld:LinkDPtr; lstFD:FileDPtr;
    kf1,kf2:KeyFldDPtr;
label 1,2;
begin
  if PD=nil then exit; MarkBoth(p1,p2); oldprocbp:=ProcMyBP; ld:=LinkDRoot;
  lstFD:=LastInChain(FileDRoot);
  SetInpTT(PD^.Pos,true);
  ReadProcHead;
  n:=LVBD.NParam; lvroot:=LVBD.Root; oldbp:=MyBP; PushProcStk;
  if (n<>PD^.N) and not((n=PD^.N-1) and PD^.ExPar) then begin
1:  CurrPos:=1; Error(119) end;
  lv:=lvroot; for i:=1 to n do with PD^.TArg[i] do begin
    if FTyp<>lv^.FTyp then goto 1;
    case FTyp of
      'r','i':begin if lv^.FD<>FD then goto 1; lv^.RecPtr:=RecPtr end;
      'f':begin
          if RecPtr<>nil then begin
            p:=SaveCompState; SetInpLongStr(RunLongStr(TxtFrml),true);
            RdFileD(Name^,'6','$'); RestoreCompState(p) end
          else CFile:=FD;
           lv1:=lv; while lv1<>nil do begin
           if (lv1^.FTyp in ['i','r']) and (lv1^.FD=lv^.FD) then lv1^.FD:=CFile;
           lv1:=lv1^.Chain end;
          lv^.FD:=CFile; FDLocVarAllowed:=true end;
      else z:=Frml;
         if lv^.IsRetPar and (z^.Op<>_getlocvar) or
            FromProlog and (IsRetPar<>lv^.IsRetPar) then goto 1;
         LVAssignFrml(lv,oldbp,false,Frml) end;
    lv:=lv^.Chain end;
  lv1:=lv; while lv<>nil do begin
    if lv^.FTyp='r' then begin CFile:=lv^.FD; CRecPtr:=GetRecSpace;
      SetTWorkFlag; ZeroAllFlds; ClearDeletedFlag; lv^.RecPtr:=CRecPtr end;
    lv:=lv^.Chain end;
  ProcMyBP:=MyBP; pd1:=ReadProcBody; FDLocVarAllowed:=false;
  lv:=lv1; while lv<>nil do begin
    if lv^.FTyp='i' then with WKeyDPtr(lv^.RecPtr)^ do begin
      if KFlds=nil then KFlds:=lv^.FD^.Keys^.KFlds; Open(KFlds,true,false) end;
    lv:=lv^.Chain end;
  ReleaseStore2(p2);
  RunProcedure(pd1);
  lv:=lvroot; i:=1; while lv<>nil do begin
    if lv^.IsRetPar then begin z:=PD^.TArg[i].Frml;
      case lv^.FTyp of
       'R': FloatPtr(Ptr(Seg(oldbp^),Ofs(oldbp^)+z^.BPOfs))^:=
            FloatPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+lv^.BPOfs))^;
       'S': begin l:=LongintPtr(Ptr(Seg(oldbp^),Ofs(oldbp^)+z^.BPOfs))^;
            LongintPtr(Ptr(Seg(oldbp^),Ofs(oldbp^)+z^.BPOfs))^:=
            LongintPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+lv^.BPOfs))^;
            LongintPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+lv^.BPOfs))^:=l end;
       'B': BooleanPtr(Ptr(Seg(oldbp^),Ofs(oldbp^)+z^.BPOfs))^:=
            BooleanPtr(Ptr(Seg(MyBP^),Ofs(MyBP^)+lv^.BPOfs))^ end end;
    if i>n then case lv^.FTyp of
      'r':begin CFile:=lv^.FD; ClearRecSpace(lv^.RecPtr) end;
      'i':begin CFile:=lv^.FD; WKeyDPtr(lv^.RecPtr)^.Close end end;
    inc(i); lv:=lv^.Chain end;
  PopProcStk;  ProcMyBP:=oldprocbp; LinkDRoot:=ld;
  CFile:=lstFD^.Chain;
  while CFile<>nil do begin CloseFile; CFile:=CFile^.Chain end;
  lstFD^.Chain:=nil; ReleaseBoth(p1,p2);
end;

procedure RunMainProc(RP:RdbPos;NewWw:boolean);
var PD:InstrPtr; p1,p2:pointer; lv:LocVarPtr;
begin
  if NewWw then begin
    ProcAttr:=colors.uNorm; window(1,2,TxtCols,TxtRows);
    TextAttr:=ProcAttr; ClrScr; UserHeadLine(''); MenuX:=1; MenuY:=2 end;
  PD:=GetPInstr(_proc,sizeof(RdbPos)+2); PD^.Pos:=RP;
  CallProcedure(PD);
  ReleaseStore(PD); if NewWw then window(1,1,TxtCols,TxtRows);
end;

end.