{$I Switches}
{$A-,B-,F+,G-,I+,O+,R-,S+,V+,X+,L-,D-}

unit rungraph;
interface
uses dos,graph,
	 drivers,base,access,rdrun,
	 GrGlob,GrInit,GrMenu,GrMenu1,GrPoly,Gr2D,Gr3DQ,Gr3DD,GrTransf;

procedure RunBGraph(GD:GraphDPtr;AutoGraph:boolean);
procedure RunAutoGraph(FL:FieldList;VK:KeyDPtr;Bool:FrmlPtr);

implementation

uses obaseww,runfrml,runfand;

procedure RunBGraph1(GD:GraphDPtr);
label 99,98;
var
  Par:Parametry;
  ParStr:ComStr;
  Max:Maxima;
  Par3:Param3;
  i,j,k:integer;
  naz:extStr;
  RGBp:GraphRGBDPtr;
  REGS:Registers;
  VP:ViewPortType;
  path:string;

function KontrFilePCX(assigned:FrmlPtr):boolean;
var c,cn:byte;
    path,filePCX:string;
    fp:file;
begin
  KontrFilePCX:=true;
  if assigned=nil then Par.filePCX:='GRAPH.PCX' else Par.filePCX:=RunShortStr(Assigned);
  path:='';
  if pos('\',par.filePCX)>0 then
	begin
	  for c:=1 to length(par.filePCX) do if par.filePCX[c]='\' then cn:=c;
	  path:=copy(par.filePCX,1,cn);
	  par.filePCX:=copy(par.filePCX,cn+1,length(par.filePCX));
	end;
  if pos('.',par.filePCX)>9 then Par.FilePCX:=copy(Par.filePCX,1,8);
  if pos('.',par.filePCX)=0 then
	if length(par.filePCX)>8 then par.filePCX:=copy(par.filePCX,1,8)+'.PCX'
							 else par.filePCX:=par.filePCX+'.PCX';
  if (pos('.',Par.filePCX)>0) and(length(copy(Par.filePCX,pos('.',Par.filePCX)+1,4))>3) then
	Par.filePCX:=copy(par.filePCX,1,pos('.',Par.filePCX)+3);
  par.filePCX:=path+par.filePCX;
  if assigned<>nil then AssignPCX:=par.filePCX else AssignPCX:='';

  if (GD^.GF=nil) or (GFpath='') then exit;
  filePCX:=GFpath;
  if pos('.',filePCX)=0 then  filePCX:=filePCX+'.PCX';
  { test existence souboru }
  assign(fp,filePCX);
  {$I-}
  reset(fp);
  {$I+}
  if ioresult<>0 then
    begin
      SetMsgPar(filePCX);
      WrLLf10Msg(202);
      DelAllParWInTWork;
      KontrFilePCX:=false;
    end
   else close(fp) ;
end;

begin
  if GD^.GF<>nil then GFpath:=RunShortStr(GD^.GF) else GFpath:='';
  GetViewSettings(VP);
  GrDriv:=GraphDriver; GrMode:=GraphMode;
  GetPalette(OldPalette);            { uschovat puvodni paletu }
  if (IsDACRegisters)  then begin
	IsDACReg:=true;
	REGS.AX:=$1017;
	REGS.BX:=0;          { precist paletu 256 barev }
	REGS.CX:=256;        { pocet registru }
	REGS.ES:=SEG(DACReg);
	REGS.DX:=OFS(DACReg);
	INTR($10,REGS);
  end;
  if GD=nil then GD:=GetZStore(sizeof(GraphD));
  NoPCX:=false;
  GRexit:=false;
  Poprve:=true;
  PoprveInit:=true;
  MenuInit:=1;
  key:=0;
  Par.Interact:=false;
  Par.VyberVet:=false;
  if not KontrFilePCX(GD^.assign) then exit;
  Par.Print:='DH200Y';
  for i:=0 to 15 do begin RGB[i].R:=-1;RGB[i].G:=0;RGB[i].B:=0;end; {neprovede se SetRGBPalette}
  for ParWPN:=MaxWinGr downto 1 do ParWPU[ParWPN]:=0;

  if (GD^.FD=nil)and(not GD^.Interact)and(GD^.GF=nil) then
	begin Par.Interact:=true;Par.UdajX:='';end { start bez parametru }
   else TxWPar(GD,true);     { hleda v parametrech parametry pro text.okna }

  if GD^.P=nil then Par.Print:='' else Par.Print:=RunShortStr(GD^.P);

  RGBp:=GD^.RGB;
  if GD^.RGB<>nil then
	repeat
	  if RunShortStr(RGBp^.Barva)='@' then begin
		ViewRGB(Par);
		ClrScrG(Par.Barvy);
		RestorePalette;
		exit;
	  end;
	  RGBp:=RGBp^.Chain;
	until RGBp=nil;

  SetViewPort(0,0,GetmaxX,GetMaxY,ClipOn);
  GetViewSettings(LastViewPort);

  if GD^.WW<>nil then with GD^.WW^ do RunWFrml(W,WFlags,ViewPortGD);
  if GD^.GF<>nil then
   begin
        if (GD<>nil) and (GD^.WW<>nil) then {zobrazeni stinu bez ramecku}
         with GD^.WW^ do
          begin
            RunWFrml(W,WFlags,WR); with WR do begin
              if (WFlags and WShadow)<>0 then ShowOnlyShadow(C1,R1,C2,R2);
            end;
          end;
	if GFpath='' then begin HideMouse; NoPCX:=true;ViewPCX(Par,GD);{GrStop(Par,GD);} ShowMouse;
	   with VP do SetViewPort(x1,y1,x2,y2,ClipOn);
	   exit;end
	 else
	  begin HideMouse; PCX:=true; ViewPCX(Par,GD); ShowMouse;
			with VP do SetViewPort(x1,y1,x2,y2,ClipOn);
			exit;end;
   end;
99:
  RestorePalette;
  CtiPar(Par,Poprve,Par3,Max,GD);
  if GrDriv=EGAMONO then
	begin
	  if (Par.Color[1]='C')or(Par.Color[1]='M')or(Par.Color[1]='O') then
		Par.Color[1]:='F';
	end;
  HideMouse;
  if GRexit then goto 98;
  if Par.Interact then ClearDevice;
  ShowFrame(Par.Border);
  if (GD<>nil) and (GD^.WW<>nil) then {zobrazeni stinu bez ramecku}
   with GD^.WW^ do
    begin
      RunWFrml(W,WFlags,WR); with WR do begin
        if (WFlags and WShadow)<>0 then ShowOnlyShadow(C1,R1,C2,R2);
      end;
    end;
  if (GD^.WW^.WFlags and WNoClrScr)=0 then ClrScrG(Par.Barvy);
  for i:=0 to 15 do if RGB[i].R>-1 then SetRGBPalette(ConvCol[i],RGB[i].R*63 div 100,RGB[i].G*63 div 100,
						 RGB[i].B*63 div 100);
  with Par,Max,Par3 do
  begin
	if PopX='' then PopX:='H';
	if (Typ='2D')or(Typ='3D') then
	 begin
	   VypMax(Par,Par3,Max);
	   Poprve:=false;
	   if GRexit then goto 98;
	   if (Typ='3D')and((Maximum<>0)or(Minimum<>0)) then Sloupce3(Par,Max);
	   Osy(Par,Max);    { ver3.2 }
	   if (Typ='2D')and((Maximum<>0)or(Minimum<>0)) then Sloupce2(Par,Max);
	   TextW;
	   Stop(Par);
	 end;
	if Typ='Cir' then
	  begin
		for i:=1 to pocvet-1 do
          if UY[i]<0 then
			begin
              WrLLf10Msg(205);
              if not Par.Interact then goto 98;
              goto 99;
            end;
        VypMax(Par,Par3,Max);
        Poprve:=false;
        if GRexit then goto 98;
        Circular(Par,Max);
		TextW;
        Stop(Par);
      end;
	if Typ[1]='L' then
	  begin
		VypMax(Par,Par3,Max);
		if UL[pocvet]>1E19 then begin naz:='X';SetMsgPar(naz);WrLLf10Msg(206);GRExit:=true;goto 99;end;
		Poprve:=false;
		if GRexit then goto 98;
		Osy(Par,Max);
		OsaX(Par,Max);
		if ((Typ='LP')or(Typ='LP3'))and((Maximum<>0)or(Minimum<>0)) then
		  begin
			for i:=pocvet downto 1 do UL[i]:=UL[i]-UL[1];
			for i:=1 to pocvet do UL[i]:=UL[i]/UL[pocvet]*100;
			PolyLin(Par,Max);
		  end;
		if (Typ[2]<>'P')and((Maximum<>0)or(Minimum<>0)) then Linear(Par,Max);
		TextW;
		Stop(Par);
	  end;
	if (Typ='DS')or(Typ='DL')or(Typ='SB') then
	  begin
		VypMax(Par,Par3,Max);
		Poprve:=false;
		if GRexit then goto 98;
		OsyD(Par,Par3,Max);
		if ((Maximum<>0)or(Minimum<>0))and(Typ='SB') then
          SloupSB(Par,Par3,Max);
        if ((Maximum<>0)or(Minimum<>0))and(Typ='DS') then
          SloupDS(Par,Par3,Max);
        if ((Maximum<>0)or(Minimum<>0))and(Typ='DL') then
          SloupDL(Par,Par3,Max);
        TextW;
        Stop(Par);
      end;
	if (Typ='QS')or(Typ='QL') then
      begin
		VypMax(Par,Par3,Max);
		Poprve:=false;
        if GRexit then goto 98;
		OsyQ(Par,Par3,Max);
		if ((Maximum<>0)or(Minimum<>0))and(Typ='QS') then
		  SloupQS(Par,Par3,Max);
        if ((Maximum<>0)or(Minimum<>0))and(Typ='QL') then
		  SloupQL(Par,Par3,Max);
        TextW;
		Stop(Par);
	  end;
    Poprve:=False;
	ShowMouse;
	if Par.Interact then goto 99;
98: DelAllParWInTWork;
	ShowMouse;
	RestorePalette;
	with LastViewPort do SetViewPort(x1,y1,x2,y2,Clip);
	SetColor(colors.dNorm);
	with VP do SetViewPort(x1,y1,x2,y2,ClipOn);
end;
end;

procedure RunBGraph(GD:GraphDPtr;AutoGraph:boolean);
var p,p1:pointer; pos:longint; b,small,pix:boolean; i,x,y:word;
	Pr:ComStr;
begin
  AutoGr:=AutoGraph;
  MarkStore(p1);  wd:=GD^.WW;  small:=(GD<>nil) and (wd<>nil);
  GraphShadow:=((wd^.WFlags and WPushPixel)<>0) or ((wd^.WFlags and WNoClrScr)<>0);
  pix:=(wd^.WFlags and WPushPixel)<>0;
  if GD^.P<>nil then Pr:=copy(RunShortStr(GD^.P),1,5) else Pr:='';
  for i:=1 to length(Pr) do Pr[i]:=upcase(Pr[i]);if copy(Pr,1,3)='PAU' then ExitPCX:=true else ExitPCX:=false;
  if not small or GD^.interact then pos:=PushW1(1,1,TxtCols,TxtRows,pix,true)
  else with wd^ do begin
	RunWFrml(W,WFlags,WR); with WR do begin
	  x:=0; y:=0;
	  if (wd^.WFlags and WShadow)<>0 then begin
		x:=minw(2,TxtCols-C2); y:=minw(1,TxtRows-R2) end;
	  pos:=PushW1(C1,R1,C2+x,R2+y,pix,true);
	   end end;
  b:=IsGraphMode; if not b then ScrGraphMode(small,0);
  RunBGraph1(GD);
  if not b then ScrTextMode(small,false);
  PopW2(pos,(Pr<>'PAU00') and not small or ((wd^.WFlags and WNoPop)=0));
  ReleaseStore(p1);
end;

procedure RunAutoGraph(FL:FieldList;VK:KeyDPtr;Bool:FrmlPtr);
var i:byte;
	GD:GraphDPtr;
begin
  AutoGr:=true;
  FLLoc:=FL;
  GD:=GetZStore(sizeof(GraphD));
  GD^.FD:=CFile;
  GD^.GF:=nil;
  GD^.X:=nil;
  GD^.Y:=nil;
  GD^.Z:=nil;
  for i:=0 to 9 do GD^.ZA[i]:=nil;
  for i:=0 to 9 do GD^.HZA[i]:=nil;
  GD^.T:=nil;
  GD^.H:=nil;
  GD^.HX:=nil;
  GD^.HY:=nil;
  GD^.HZ:=nil;
  GD^.C:=nil;
  GD^.D:=nil;
  GD^.R:=nil;
  GD^.P:=nil;
  GD^.CO:=nil;
  GD^.Cond:=Bool;
  GD^.S:=nil;
  GD^.RS:=nil;
  GD^.RN:=nil;
  GD^.Max:=nil;
  GD^.Min:=nil;
  GD^.SP:=nil;
  GD^.Interact:=true;
  GD^.V:=nil;
  GD^.W:=nil;
  GD^.RGB:=nil;
  GD^.KeyIn:=nil;
  GD^.ViewKey:=VK;
  GD^.WW:=nil;
  RunBGraph(GD,true);
end;

end.
