{$I switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L+}
unit oaccess;

interface

uses dos,base,access;


procedure SaveFiles;
procedure ClosePassiveFD;
procedure CloseFANDFiles(FromDML:boolean);
procedure OpenFANDFiles(FromDML:boolean);

 function OpenF1(UM:FileUseMode):boolean;
 function OpenF2:boolean;
 function OpenF(UM:FileUseMode):boolean;
procedure OpenCreateF(UM:FileUseMode);
 function RewriteF(Append:boolean):LockMode;
procedure CloseFile;
procedure CloseFAfter(FD:FileDPtr);

 function TestMountVol(DriveC:char):word;
procedure ReleaseDrive(D:word);

procedure SetCPathForH(handle:word);  {$ifdef FandSQL}
procedure SetIsSQLFile;               {$endif}
 function GetCatIRec(Name:NameStr;MultiLevel:boolean):word;
 function Generation:word;
procedure TurnCat(Frst,N:word;I:integer);
 function RdCatField(CatIRec:word;CatF:FieldDPtr):string;
procedure WrCatField(CatIRec:word;CatF:FieldDPtr;Txt:string);
procedure RdCatPathVol(CatIRec:word);
procedure SetCPathVol;
procedure SetTxtPathVol(Path:StringPtr;CatIRec:word);

 function OpenDuplF(CrTF:boolean):FileDPtr;
procedure SubstDuplF(TempFD:FileDPtr;DelTF:boolean);
procedure DelDuplF(TempFD:FileDPtr);

implementation

uses drivers,obaseww,runfrml,wwmix,
{$ifdef FandSQL} channel, {$endif}
     expimp;



procedure OpenXWorkH;
var m:FileOpenMode;
begin
  CVol:=''; m:=_isoldnewfile; if XWork.MaxPage=0 then m:=_isoverwritefile;
  CPath:=FandWorkXName; XWork.Handle:=OpenH(m,Exclusive); XWork.TestErr;
  if FileSizeH(XWork.Handle)=0 then begin XWork.FreeRoot:=0; XWork.MaxPage:=0 end;
end;

procedure OpenTWorkH;
begin
  CVol:='';
  if TWork.MaxPage=0 then begin
    CPath:=FandWorkTName; TWork.IsWork:=true; TWork.Create end
  else begin
    CPath:=FandWorkTName; CVol:='';
    TWork.Handle:=OpenH(_isoldnewfile,Exclusive); TWork.TestErr end;
end;

procedure SaveFD; far;
  begin WrPrefixes; if CFile^.Typ='X' then CFile^.XF^.NoCreate:=false end;
procedure SaveFiles;
var b:boolean; i:word; cf:FileDPtr;
begin
  if not CacheExist then exit; cf:=CFile; CFile:=CatFD; WrPrefixes;
  ForAllFDs(SaveFD); b:=SaveCache(0); FlushHandles;
  CFile:=cf; if not b then GoExit;
end;
procedure ClosePassiveFD;
begin with CFile^ do
  if (Typ<>'0') and (LMode=NullMode) then CloseFile;
end;

procedure CloseFANDFiles(FromDML:boolean);
var RD:RdbDPtr;
begin
  RD:=CRdb; while RD<>nil do begin
    CFile:=RD^.FD; while CFile<>nil do begin
      if not FromDML then with CFile^ do ExLMode:=LMode;
      CloseFile; CFile:=CFile^.Chain end;
    RD:=RD^.ChainBack end;
  if CRdb<>nil then begin CFile:=CatFD; CloseFile end;
  CFile:=HelpFD; CloseFile;
  CloseH(TWork.Handle); CloseH(XWork.Handle);
end;

procedure OpenFANDFiles(FromDML:boolean);
var RD:RdbDPtr; md:LockMode;
begin                                                            
  OpenXWorkH; OpenTWorkH; CFile:=HelpFD; OpenF(RdOnly);
  if CRdb=nil then exit; CFile:=CatFD; OpenF(Exclusive);
  RD:=CRdb; while RD<>nil do begin
    CFile:=RD^.FD; if IsTestRun then OpenF(Exclusive) else OpenF(RdOnly);
    CFile:=CFile^.Chain; while not FromDML and (CFile<>nil) do begin
      with CFile^ do if ExLMode<>NullMode then begin
        OpenF(Shared); md:=NewLMode(ExLMode) end;
      CFile:=CFile^.Chain end;
    RD:=RD^.ChainBack end;
end;

{===========================================================================}

procedure SetCPathMountVolSetNet(UM:FileUseMode);
begin
  SetCPathVol; with CFile^ do begin
    UMode:=UM; Drive:=TestMountVol(CPath[1]);
    if not IsNetCVol or (CFile=Chpt) then case UM of
      RdShared:UMode:=RdOnly; Shared:UMode:=Exclusive end
    else if (UM=Shared) and SEquUpcase(CVol,'#R') then UMode:=RdShared end;
end;

function OpenF1(UM:FileUseMode):boolean;
var b:boolean; n:word;
label 1,2,3,4;
begin
  with CFile^ do begin
    OpenF1:=true; LMode:=NullMode;
    SetCPathMountVolSetNet(UM);  b:=(CFile=Chpt) or (CFile=CatFD);
    if b and (IsTestRun or IsInstallRun)
       and ((GetFileAttr and 1{RdOnly})<>0) then begin
      SetFileAttr(GetFileAttr and $26);
      if HandleError=5 then HandleError:=79; TestCFileError; WasRdOnly:=true end;
1:  Handle:=OpenH(_isoldfile,UMode);
    if (HandleError<>0) and WasRdOnly then begin
      SetFileAttr((GetFileAttr and $27) or $1{RdONly}); TestCFileError end;
    if (HandleError=5) and (UMode=Exclusive) then begin UMode:=RdOnly; goto 1 end;
    if HandleError=2 then begin OpenF1:=false; exit end;
{$ifndef FandNetV}
    if (HandleError in [5,$21]) and
       ((CVol='#') or (CVol='##') or SEquUpcase(CVol,'#R')) then CFileError(842);
{$endif}
    TestCFileError;
    if TF<>nil then with TF^ do begin
      CExtToT; if WasRdOnly then SetFileAttr(GetFileAttr and $26);
2:    Handle:=OpenH(_isoldfile,UMode);
      if HandleError=2 then begin
        if Format=DbtFormat then begin
          Format:=FptFormat; CExt:='.FPT'; CPath:=CDir+CName+CExt; goto 2 end;
        if IsDynFile then begin
          CloseClearH(CFile^.Handle); OpenF1:=false; exit end end;
      if HandleError<>0 then goto 4 end;
    if Typ='X' then with XF^ do begin CExtToX;
3:    Handle:=OpenH(_isoldfile,UMode);
      if HandleError=2 then begin
        Handle:=OpenH(_isoverwritefile,Exclusive); if HandleError<>0 then goto 4;
        SetNotValid; CloseH(Handle);
        goto 3 end;
      if HandleError<>0 then begin
4:        n:=HandleError; CloseClearHCFile; HandleError:=n; TestCPathError end;
      if FileSizeH(Handle)<512 then SetNotValid end;
end end;
function OpenF2:boolean;
var FS,n,l:longint; Signum,rLen:word; md:LockMode;
label 1,2,3;
begin with CFile^ do begin
  FS:=FileSizeH(Handle); NRecs:=0; OpenF2:=false;
  if FS<FrstDispl then goto 1;
  rLen:=RdPrefix; n:=(FS-FrstDispl) div RecLen;
  if rLen<>$ffff then
    if IsDynFile then begin CloseClearHCFile; exit end
    else begin if OldToNewCat(FS) then goto 3;
     CFileMsg(883,' '); l:=longint(NRecs)*rLen+FrstDispl;
     if (l=FS) or not PromptYN(885) then CloseGoExit;
     if (NRecs=0) or (l shr CachePageShft<>FS shr CachePageShft) then begin
       WrLLF10Msg(886); NRecs:=n end;
     goto 2 end;
  if n<NRecs then begin SetCPathVol; SetMsgPar(CPath);
    if PromptYN(882) then begin NRecs:=n;
1:    if IsShared and (LMode<ExclMode) then ChangeLMode(ExclMode,0,false); LMode:=ExclMode;
2:    SetUpdHandle(Handle); WrPrefix end
    else CloseGoExit end;
3:if TF<>nil then
    if FS<FrstDispl then TF^.SetEmpty else begin
    TF^.RdPrefix(true); if (CFile^.Typ='0') and not IsActiveRdb(CFile)
      and not HasPassword(CFile,1,'') then begin
      CFileMsg(616,' '); CloseGoExit end end;
  if Typ='X' then with XF^ do
    if FS<FrstDispl then SetNotValid else begin
    RdWrCache(true,Handle,NotCached,0,2,Signum); RdPrefix;
    if not NotValid and
      ((Signum<>$04FF) or (NRecsAbs<>CFile^.NRecs) or (FreeRoot>MaxPage)
       or ((longint(MaxPage+1) shl XPageShft)>FileSizeH(Handle)))
      or (NrKeys<>0) and (NrKeys<>CFile^.GetNrKeys) then begin
      if not SEquUpcase(getEnv('FANDMSG830'),'NO') then CFileMsg(830,'X');
      if IsShared and (LMode<ExclMode) then ChangeLMode(ExclMode,0,false); LMode:=ExclMode;
      SetNotValid end end;
  SeekRec(0); OpenF2:=true;
end end;
function OpenF(UM:FileUseMode):boolean;
begin with CFile^ do begin
  OpenF:=true; if Handle<>$FF then exit;
  if OpenF1(UM) then begin
    if {$ifdef FandSQL} not IsSQLFile and {$endif} IsShared then begin
      ChangeLMode(RdMode,0,false); LMode:=RdMode end;
    OpenF:=OpenF2; OldLMode(NullMode) end
  else OpenF:=false;
end end;

procedure CreateF;
begin
  with CFile^ do begin
    SetCPathMountVolSetNet(Exclusive);
    Handle:=OpenH(_isoverwritefile,Exclusive);
    TestCFileError; NRecs:=0;
    if TF<>nil then with TF^ do begin CExtToT; Create end;
    if Typ='X' then with XF^ do begin CExtToX;
      Handle:=OpenH(_isoverwritefile,Exclusive); TestErr; {SetNotValid} SetEmpty  end;
    SeekRec(0); SetUpdHandle(Handle);
end end;

procedure OpenCreateF(UM:FileUseMode);
begin with CFile^ do
  if not OpenF(UM) then begin
    CreateF; if (UM=Shared) or (UM=RdShared) then begin
      WrPrefixes; SaveCache(0); CloseClearH(Handle);
      if Typ='X' then CloseClearH(XF^.Handle);
      if TF<>nil then CloseClearH(TF^.Handle);
      OpenF(UM) end end;
end;

function RewriteF(Append:boolean):LockMode;
begin
  with CFile^ do begin
  if Append then begin
    RewriteF:=NewLMode(CrMode); SeekRec(NRecs);
    if XF<>nil then begin XF^.FirstDupl:=true; TestXFExist end;
    exit end;
  RewriteF:=NewLMode(ExclMode); NRecs:=0; SeekRec(0); SetUpdHandle(Handle);
  XFNotValid; if Typ='X' then XF^.NoCreate:=true; if TF<>nil then TF^.SetEmpty;
end end;

procedure TruncF;
var md:LockMode; sz:longint;
begin with CFile^ do begin
    if UMode=RdOnly then exit;  md:=NewLMode(RdMode);
    TruncH(Handle,UsedFileSize); if HandleError<>0 then CFileMsg(700+HandleError,'0');
    if TF<>nil then with TF^ do begin
      TruncH(Handle,UsedFileSize); TestErr end;
    if (Typ='X') then with XF^ do begin
      sz:=UsedFileSize; if NotValid then sz:=0;
      TruncH(Handle,sz); TestErr end;
    OldLMode(md);
end end;

procedure CloseFile;
label 1;
begin
  with CFile^ do begin
    if Handle=$FF then exit;
    if IsShared then OldLMode(NullMode) else WrPrefixes;
    SaveCache(0); TruncF;
    if Typ='X' then with XF^ do if Handle<>$FF then begin
      CloseClearH(Handle);
      if not IsShared then
        if NotValid then goto 1
        else if (NRecs=0) or (CFile^.NRecs=0) then begin
          CFile^.NRecs:=0;
1:        SetCPathVol; CExtToX; DeleteFile(CPath) end end;
    if TF<>nil then with TF^ do if Handle<>$FF then begin
      CloseClearH(Handle);
      if not IsShared and (CFile^.NRecs=0) and (Typ<>'D') then begin
        SetCPathVol; CExtToT; DeleteFile(CPath) end end;
    CloseClearH(Handle); LMode:=NullMode;
    if not IsShared and (NRecs=0) and (Typ<>'D') then begin
      SetCPathVol;  DeleteFile(CPath) end;
    if WasRdOnly then begin
      WasRdOnly:=false;
      SetCPathVol; SetFileAttr((GetFileAttr and $27) or $1{RdONly});
      if TF<>nil then begin
        CExtToT; SetFileAttr((GetFileAttr and $27) or $1{RdONly}) end end;
end end;

procedure CloseFAfter(FD:FileDPtr);
  begin CFile:=FD;
        while CFile<>nil do begin CloseFile; CFile:=CFile^.Chain end end;


{===========================================================================}


function ActiveRdbOnDrive(D:word):boolean;
  var R:RdbDPtr;
  begin ActiveRdbOnDrive:=true;R:=CRdb; while R<>nil do
         begin if R^.FD^.Drive=D then exit;R:=R^.ChainBack end;
        ActiveRdbOnDrive:=false end;
procedure CloseFilesOnDrive(D:word);
  var R:RdbDPtr; CF:FileDPtr; b:boolean;
  begin R:=CRdb; CF:=CFile; while R<>nil do
      begin CFile:=R^.FD; while CFile<>nil do
        begin if CFile^.Drive=D then CloseFile;
              CFile:=CFile^.Chain end;
        R:=R^.ChainBack end; CFile:=CF end;

function TestMountVol(DriveC:char):word;
var S:SearchRec; D,i:word; Vol:VolStr;
const Drive:string[1]='A';
label 1,2,3;
begin TestMountVol:=0; if IsNetCVol then exit;
    D:=ord(upcase(DriveC))-ord('@');
    if D>=FloppyDrives then
      if upcase(CDir[1])=spec.CPMDrive then D:=FloppyDrives else exit;
    if (CVol='') or SEquUpcase(MountedVol[D],CVol) then goto 3;
    Drive[1]:=DriveC;
    if ActiveRdbOnDrive(D) then
       begin Set3MsgPar(Drive,CVol,MountedVol[D]);RunError(812) end;
    Vol:=CVol; CloseFilesOnDrive(D); CVol:=Vol;
  1:F10SpecKey:=_ESC_;Set2MsgPar(Drive,CVol);WrLLF10Msg(808);
    if KbdChar=_ESC_ then if PromptYN(21) then GoExit else goto 1;
    if D=FloppyDrives then FindFirst(Drive+':\*.VOL',0,S)
    else FindFirst(Drive+':\*.*',VolumeID,S);
    case DosError of
      18{label missing}: begin WrLLF10Msg(809); goto 1 end;
      0: ;
      else                      WrLLF10Msg(810); goto 1 end;
    i:=pos('.',S.Name);
    if D=FloppyDrives then delete(S.Name,i,255)
    else if i<>0 then delete(S.Name,i,1);
    if not SEquUpcase(S.Name,CVol) then
       begin SetMsgPar(S.Name);WrLLF10Msg(817);goto 1 end;
  2:MountedVol[D]:=CVol;
  3:TestMountVol:=D end;

procedure ReleaseDrive(D:word);
  var Drive:string[1];
  begin if MountedVol[D]='' then exit;
      if D=FloppyDrives then Drive:=spec.CPMDrive else Drive:=char(D+ord('@'));
      if ActiveRdbOnDrive(D) then begin SetMsgPar(Drive);RunError(813) end;
      CloseFilesOnDrive(D); Set2MsgPar(MountedVol[D],Drive);WrLLF10Msg(818);
      MountedVol[D]:='' end;


{===========================================================================}

procedure SetCPathForH(handle:word);
  var RD:RdbDPtr; cf:FileDPtr;
  label 1;
  begin cf:=CFile; RD:=CRDB; while RD<>nil do
        begin CFile:=RD^.FD;while CFile<>nil do
          begin if CFile^.Handle=handle then begin SetCPathVol;goto 1 end;
                if (CFile^.XF<>nil) and (CFile^.XF^.Handle=handle) then
                   begin SetCPathVol;CExtToX;goto 1 end;
                if (CFile^.TF<>nil) and (CFile^.TF^.Handle=handle) then
                   begin SetCPathVol;CExtToT;goto 1 end;
                CFile:=CFile^.Chain end; RD:=RD^.ChainBack end;
        RdMsg(799);CPath:=MsgLine;
      1:CFile:=cf;
  end;
{$ifdef FandSQL}
procedure SetIsSQLFile;
var b:boolean;
begin
  b:=false; if CFile^.typSQLFile then
  b:= (Strm1<>nil) and SEquUpcase(RdCatField(CFile^.CatIRec,CatVolume),'#SQL');
  CFile^.IsSQLFile:=b;
end; {$endif}

function GetCatIRec(Name:NameStr;MultiLevel:boolean):word;
  var i:longint; CF:FileDPtr; R:RdbDPtr; CR:pointer;
  label 1,2;
  begin GetCatIRec:=0;
       if CatFD^.Handle=$FF then exit; if CRdb=nil then exit;
        CF:=CFile;CR:=CRecPtr; CFile:=CatFD; CRecPtr:=GetRecSpace;
        R:=CRdb;
      1:for i:=1 to CatFD^.NRecs do
        begin ReadRec(i);
          if SEquUpcase(trailchar(' ',_shorts(CatRdbName)),R^.FD^.Name) and
             SEquUpcase(trailchar(' ',_shorts(CatFileName)),Name) then
             begin GetCatIRec:=i;goto 2 end end;
        R:=R^.ChainBack;if (R<>nil) and Multilevel then goto 1;
      2:CFile:=CF;ReleaseStore(CRecPtr);CRecPtr:=CR;
  end;
function Generation:word;
  var i,j:word; s:string[2];
  begin Generation:=0;if CFile^.CatIRec=0 then exit;
        RdCatPathVol(CFile^.CatIRec);
        s:=copy(CExt,3,2); val(s,i,j); if j=0 then Generation:=i end;
procedure TurnCat(Frst,N:word;I:integer);
  var p,q:pointer; j,last:word;
  begin if CFile<>nil then CloseFile;
      CFile:=CatFD; p:=GetRecSpace; q:=GetRecSpace; CRecPtr:=q; last:=Frst+N-1;
      if I>0 then
      while I>0 do begin
         ReadRec(Frst); CRecPtr:=p;
         for j:=1 to N-1 do begin ReadRec(Frst+j);WriteRec(Frst+j-1) end;
         CRecPtr:=q; WriteRec(last); dec(I) end
      else
      while I<0 do begin
         ReadRec(last); CRecPtr:=p;
         for j:=1 to N-1 do begin ReadRec(last-j);WriteRec(last-j+1) end;
         CRecPtr:=q; WriteRec(Frst); inc(I) end;
      ReleaseStore(p) end;

function RdCatField(CatIRec:word;CatF:FieldDPtr):string;
  var CF:FileDPtr; CR:pointer;
  begin CF:=CFile;CR:=CRecPtr; CFile:=CatFD; CRecPtr:=GetRecSpace;
      ReadRec(CatIRec); RdCatField:=trailchar(' ',_shorts(CatF));
      ReleaseStore(CRecPtr); CFile:=CF;CRecPtr:=CR end;
procedure WrCatField(CatIRec:word;CatF:FieldDPtr;Txt:string);
  var CF:FileDPtr; CR:pointer;
  begin CF:=CFile;CR:=CRecPtr; CFile:=CatFD; CRecPtr:=GetRecSpace;
      ReadRec(CatIRec); s_(CatF,Txt); WriteRec(CatIRec);
      ReleaseStore(CRecPtr); CFile:=CF;CRecPtr:=CR end;
procedure RdCatPathVol(CatIRec:word);
  begin CPath:=FExpand(RdCatField(CatIRec,CatPathName));
      FSplit(CPath,CDir,CName,CExt);
      CVol:=RdCatField(CatIRec,CatVolume) end;
function SetContextDir(var D:DirStr;var IsRdb:boolean):boolean;
var R:RdbDPtr; F:FileDPtr;
label 1;
begin
  R:=CRdb; SetContextDir:=true; IsRdb:=false;
  while R<>nil do begin F:=R^.FD;
    if (CFile=F) and (CFile^.CatIRec<>0) then begin
       D:=R^.RdbDir; IsRdb:=true; exit end;
    while F<>nil do begin
      if CFile=F then begin
        if (CFile=R^.HelpFD) or (CFile^.Typ='0'{.RDB}) then
        D:=R^.RdbDir else D:=R^.DataDir; exit end;
      F:=F^.Chain end; R:=R^.ChainBack end;
  SetContextDir:=false;
end;
procedure GetCPathForCat(I:word);
var d:DirStr; isRdb:boolean;
begin
  CVol:=RdCatField(i,CatVolume); CPath:=RdCatField(i,CatPathName);
  if (CPath[2]<>':') and SetContextDir(d,isRdb) then begin
    if isRdb then begin FSplit(CPath,CDir,CName,CExt);
      AddBackSlash(d); CDir:=d; CPath:=CDir+CName+CExt; exit end;
    if CPath[1]='\' then CPath:=copy(d,1,2)+CPath else begin
      AddBackSlash(d); CPath:=d+CPath end end
  else CPath:=FExpand(CPath);
  FSplit(CPath,CDir,CName,CExt);
end;
procedure SetCPathVol;
var i:word; isRdb:boolean;
label 2,3,4;
begin
  CVol:=''; if CFile^.Typ='C' then begin
    CDir:=getenv('FANDCAT'); if CDir='' then
      if TopDataDir='' then CDir:=TopRdbDir else CDir:=TopDataDir;
    AddBackSlash(CDir); CName:=CatFDName; CExt:='.CAT'; goto 4 end;
  i:=CFile^.CatIRec; if i<>0 then begin
    GetCPathForCat(i); if CFile^.Name='@' then goto 3 else goto 4 end;
  case CFile^.Typ of
    '0':CExt:='.RDB'; '8':CExt:='.DTA'; 'D':CExt:='.DBF';
    else CExt:='.000' end;
  if SetContextDir(CDir,isRdb) then goto 2;
  if CFile=HelpFD then begin
    CDir:=FandDir;
    CName:={$ifdef FandRunV} 'UFANDHLP' {$else} 'FANDHLP' {$endif}; goto 4 end;
  CExt:='.100'; CDir:=CRdb^.DataDir;
2:AddBackSlash(CDir);
3:CName:=CFile^.Name;
4:CPath:=CDir+CName+CExt;
end;
procedure SetTxtPathVol(Path:StringPtr;CatIRec:word);
begin
  if CatIRec<>0 then RdCatPathVol(CatIRec)
  else begin CPath:=FExpand(Path^); CVol:='' end;
end;

{===========================================================================}

procedure SetTempCExt(Typ:char;IsNet:boolean);
var Nr:char;
begin
  if Typ='T' then begin Nr:='2';
    case CFile^.Typ of '0':CExt:='.TTT'; 'D':CExt:='.DBT' end end
  else begin Nr:='1';
    case CFile^.Typ of '0':CExt:='.RDB'; 'D':CExt:='.DBF' end end;
  if length(CExt)<2 then CExt:='.0'; CExt[2]:=Nr;
  if IsNet then CPath:=WrkDir+CName+CExt { work files are local }
  else CPath:=CDir+CName+CExt end;

function OpenDuplF(CrTF:boolean):FileDPtr;
  var OldFD,FD:FileDPtr; N:word; Len:integer; net:boolean;
  begin
    SetCPathVol; net:=IsNetCVol;
    N:=sizeof(FileD)-1+length(CFile^.Name); OldFD:=CFile;
    FD:=GetStore(N); move(OldFD^,FD^,N); CFile:=FD;
    with FD^ do
    begin SetTempCExt('0',net); CVol:='';
          Handle:=OpenH(_isoverwritefile,Exclusive);TestCFileError;
          NRecs:=0; IRec:=0; EOF:=true; UMode:=Exclusive;
          if Typ='X' then
             begin XF:=GetStore(sizeof(XFile)); XF^.Handle:=$FF;
                   XF^.NoCreate:=true;
                   {else xfile name identical with orig file} end end;
    if CrTF and(FD^.TF<>nil) then
      begin
        FD^.TF:=GetStore(sizeof(TFile));
        Move(OldFD^.TF^,FD^.TF^,sizeof(TFile));
        with FD^.TF^ do
          begin
            SetTempCExt('T',net);
            Handle:=OpenH(_isoverwritefile,Exclusive); TestErr;
            CompileAll:=true; SetEmpty;
          end;
      end;
    OpenDuplF:=FD;
  end;

procedure CopyDuplF(TempFD:FileDPtr;DelTF:boolean);
procedure CopyH(h1,h2:word);
const BufSize=32768;
var p:pointer; sz:longint;
begin
  ClearCacheH(h1); ClearCacheH(h2);
  p:=GetStore(BufSize); sz:=FileSizeH(h1); SeekH(h1,0); SeekH(h2,0);
  while sz>BufSize do begin
    ReadH(h1,BufSize,p^); WriteH(h2,BufSize,p^); dec(sz,BufSize) end;
  ReadH(h1,sz,p^); WriteH(h2,sz,p^);
  CloseH(h1); DeleteFile(CPath); ReleaseStore(p);
end;
var h1,h2:word; cf:FileDPtr;
begin
  cf:=CFile; CFile:=TempFD; WrPrefixes; CFile:=cf;
  SaveCache(0); SetTempCExt('0',true); CopyH(TempFD^.Handle,CFile^.Handle);
  if (CFile^.TF<>nil) and DelTF then begin
    h1:=TempFD^.TF^.Handle; h2:=CFile^.TF^.Handle; SetTempCExt('T',true);
    Move(TempFD^.TF^,CFile^.TF^,sizeof(TFile)); CFile^.TF^.Handle:=h2;
    CopyH(h1,h2) end;
  RdPrefixes;
end;

procedure SubstDuplF(TempFD:FileDPtr;DelTF:boolean);
procedure TestDelErr(var P:PathStr);
begin
  if HandleError<>0 then begin SetMsgPar(P); RunError(827) end;
end;
var PrimFD,FD:FileDPtr;  MD:TFilePtr;
    p,ptmp,pt:PathStr;  xf2:XFilePtr; um:FileUseMode; net:boolean;
begin
  XFNotValid; SetCPathVol;
  if IsNetCVol then begin CopyDuplF(TempFD,DelTF); exit end;
  SaveCache(0); PrimFD:=CFile; p:=CPath; CExtToT; pt:=CPath;
  with PrimFD^ do begin
    CloseClearH(Handle); DeleteFile(p); TestDelErr(p);
    FD:=Chain; MD:=TF; xf2:=XF; um:=UMode;
    move(TempFD^,PrimFD^,sizeof(FileD)-2); Chain:=FD; XF:=xf2; UMode:=um;
    CloseClearH(Handle);
    SetTempCExt('0',false); ptmp:=CPath;  RenameFile56(ptmp,p,true);
    CPath:=p; Handle:=OpenH(_isoldfile,UMode); SetUpdHandle(Handle);
    if (MD<>nil) and DelTF then begin
      CloseClearH(MD^.Handle); DeleteFile(pt); TestDelErr(pt);
      move(TF^,MD^,sizeof(TFile));TF:=MD;
      CloseClearH(MD^.Handle);
      CPath:=ptmp; SetTempCExt('T',false); RenameFile56(CPath,pt,true);
      CPath:=pt; MD^.Handle:=OpenH(_isoldfile,UMode);
      SetUpdHandle(MD^.Handle) end;
    TF:=MD end;
end;

procedure DelDuplF(TempFD:FileDPtr);
  begin CloseClearH(TempFD^.Handle); SetCPathVol;
        SetTempCExt('0',CFile^.IsShared); DeleteFile(CPath) end;
end.