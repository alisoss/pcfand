unit GrGlob;

{$A-,B-,D-,F+,G-,I+,L-,O+,R-,S+,V+,X+}

interface
uses dos,graph,drivers,access,base,RdRun,GrTransf;

const

  max = 64;  {max pocet vet do grafu }
  MaxWinGr=128; {max.pocet textovych oken}
  BarvyDef = 'BYrgmOGRbMCaAc';
  BarvyPasDef:string = 'BGCRMOaAbgcrmyw';
  ConvCol: array [0..15] of byte = (0,1,2,3,4,5,20,7,56,57,58,59,60,61,62,63);
  IsDACReg:boolean=false;
  AutoGr:boolean=false;
  MausVisibleGr:boolean=false;
  PCX:boolean=false;
  NoPCX:boolean=false;

type
  str1 = string[1];
  str9 = string[9];
  str14 = string[14];
  str15 = string[15];
  str30 = string[30];
  str80 = string[80];
  typUX = array [1..max] of str30;
  typUY = array [1..max] of real;
  FilText = array [1..2048] of char;
  LongStrAll = record LL:word; A:array [1..2048] of char;end;
  Parametry = record
	Typ,
	Print,
	NazevF,
	UdajX,
	UdajY,
	UdajZ,
	Hlavicka,
	NazevX,
	NazevY,
	NazevZ:str80;
	Color,
	Rastr,
	PopX:str9;
	UX:typUX;
	UY,UZ,UL:typUY;
	PocVet:byte;
	Interact:boolean;
	Barvy:str14;
	FileParNaz:str30;
	FilePCX:string;
	StartVeta:longint;
	VyberVet:boolean;
	Border:char;
	Pomer:byte;
   end;
  Maxima = record
	MaxExp  : integer;
	Maximum,
	Minimum,
	MaximumC,
	MaximumVst,
	MinimumVst : real;
   end;
  ParamW = record
				XZ,
				YZ,
				XK,
				YK,
				BarPoz,
				BarPis:integer;
{                FTxt:LongStrPtr;}
				FTxt:LongStrAll;
                Text:str80;
              end;

  Param3 = record
			 Udaj3,
			 Nazev3:array [0..9] of str80;
             U3:array [0..9] of TypUY;
			 PocZ:shortint;
           end;

   RGBrec = record R,G,B : integer;end;

   type qadr = array [1..4] of PointType;

   var m:shortint;
	   StupPoly,StupPolyP:shortint;
	   GRexit,Poprve:boolean;
	   FileFPtr:fileDptr;
	   UdajXPtr,UdajYPtr,UdajZPtr:FieldDPtr;
	   Udaj3Ptr:array [0..9] of FieldDptr;
	   MenuInit:word;
	   MemAloc:pointer;
	   RGB:array [0..15] of RGBrec;
	   MinimumC,MinimumVstP:real;
	   PoprveInit:boolean;
	   iores:integer;
	   ParWPU:array [1..128] of longint;
	   ParWPN,psnx,psny:byte;
	   OldPalette : PaletteType ;
	   DACReg : array [0..767] of byte;
	   Reg    : Registers;
	   LastViewPort,ViewPort:ViewPortType;
	   ViewPort80x25:record x1,y1,x2,y2:byte; end;
	   ViewPortGD:WRect absolute ViewPort80x25;
	   GMaxX:integer;
	   GMaxY:integer;
	   SizeLetter:byte;
	   ColorWWFrame,ColorWWBack,ColorWWFor:char;
       MakeCls:boolean;
       LastColor:word;
	   HeadFrame:comstr;
	   WWShadow:boolean;
	   psx,psy:byte;
	   GrDriv,GrMode:integer;
	   smallPCX:boolean;
	   key:word;
	   FLLoc,FLLoc1:FieldList;
	   NazevFStart:str9;
	   Mask1,Mask2:byte;
	   wd:WinGPtr;
	   top:string;
	   GraphShadow:boolean;
	   ExitPCX:boolean;
	   FTx:LongStrPtr;
	   ftn:integer;
	   ParW:ParamW;
           AssignPCX:PathStr;
           GFpath:PathStr;

{---------------------------}
   procedure Beep;
   procedure BarFrame(qad:qadr);
   procedure Bar3Q(x1,y1,x2,y2,z,zx:word;top:boolean);
   procedure hod(x,y:word;h,MaximumC,maxExp:real);
   function co(c:byte;B:str14):byte;
   function promptstr(Pis:str80;typt:str1;delka,zust:byte;Def:str80):str80;
   function LeftUpCh(st:str80):str80;
   procedure CursorOn;
   procedure CursorOff;
   function Nazev(par:ComStr;parametr:ComStr):ComStr;
   procedure ParRad(var Param:Parametry;var Par3:Param3;var Maxim:Maxima;GD:GraphDPtr);
   procedure TxWPar(GD:GraphDPtr;stop:boolean);
   procedure StoreParWInTWork{(var ParW:ParamW)};
   procedure ReadParWInTWork{(var ParW:ParamW)};
   procedure DelParWInTWork;
   procedure DelAllParWInTWork;
   procedure ClrScrG(Color:str14);
   procedure RectToPix(c1,r1,c2,r2:word; var x1,y1,x2,y2:integer);
   function K(Size:word):word;
   procedure OutTextXYC(X,Y:integer;str:string);
   procedure SetWindow(ViewPort:ViewPortType);
   function KonB(KP:str9):integer;
   procedure ShowFrame(Border:char);
   procedure ShowShadow;
   procedure ShowOnlyShadow(c1,r1,c2,r2:integer);

implementation

uses obaseww,runfrml;

procedure RectToPix(c1,r1,c2,r2:word; var x1,y1,x2,y2:integer);
var mx,my:word;
begin
  mx:=GetMaxX;
  if GraphMode=VGAHi {GrDriv=VGA} then my:=GetMaxY-5 else my:=GetMaxY;
  x1:=((c1-1)*mx)div TxtCols;
  y1:=((r1-1)*my)div TxtRows;
  x2:=(c2*mx)div TxtCols;
  y2:=(r2*my)div TxtRows;
  if c1>1 then x1:=x1+1;
  if r1>1 then y1:=y1+1;
  GMaxX:=x2-x1;
  GMaxY:=y2-y1;
end;

procedure StoreParWInTWork{(var ParW:ParamW)};{ zapise ParW do TWORK a prida jeho pozici doParWPPoz retez ParWPU }
var LStrPtr:LongStrPtr;P:pointer;
begin
  if ParWPN>MaxWinGr then exit;
  MarkStore(P);
  LStrPtr:=GetStore({SizeOf(ParW)}2142+2);
  LStrPtr^.LL:={SizeOf(ParW)}2142;
  move(ParW,LStrPtr^.A[1],{SizeOf(ParW)}2142);
  ParWPU[ParWPN]:=StoreInTWork(LStrPtr);
  inc(ParWPN);
  ReleaseStore(P);
end;
procedure ReadParWInTWork{(var ParW:ParamW)}; { cte do ParW zaznam v TWORK na jehoz cislo ukazuje ParWPU }
var LStrPtr:LongStrPtr;P:pointer;
begin
  if ParWPU[ParWPN]=0 then exit;
  MarkStore(P);
  LStrPtr:=ReadDelInTWork(ParWPU[ParWPN]);
  ParWPU[ParWPN]:=StoreInTWork(LStrPtr);
  move(LStrPtr^.A[1],ParW,{SizeOf(ParW)}2142);
  ReleaseStore(P);
end;

procedure DelParWInTWork; { v TWORK zrusi zaznam ParWP }
var LStrPtr:LongStrPtr;P:pointer;i:byte;
begin
  MarkStore(P);
  LStrPtr:=ReadDelInTWork(ParWPU[ParWPN]);
  ParWPU[128]:=0;
  for i:=ParWPN to MaxWinGr-1 do ParWPU[i]:=ParWPU[i+1];
  ReleaseStore(P);
end;

procedure DelAllParWInTWork;
begin
  for ParWPN:=MaxWinGr downto 1 do DelParWInTWork;
end;

PROCEDURE CursorOn;
BEGIN
  CrsShow;
END;

PROCEDURE CursorOff;
BEGIN
   CrsHide;
END;

procedure Beep;
  begin
    ScrBeep; {Mark***}
    {if spec.BEEP then begin sound(1800);delay(50);nosound;delay(50) end; }
  end;

procedure BarFrame(qad:qadr);
begin
	if upcase(ColorWWFor)='L' then
     begin
      line(qad[1].x,qad[1].y,qad[2].x,qad[2].y);
      line(qad[2].x,qad[2].y,qad[3].x,qad[3].y);
      line(qad[3].x,qad[3].y,qad[4].x,qad[4].y);
      line(qad[1].x,qad[1].y,qad[4].x,qad[4].y);
     end;
end;

procedure Bar3Q(x1,y1,x2,y2,z,zx:word;top:boolean);
  var
    qad:qadr;
	ColInf : FillSettingsType;
    xp,yp:word;

  begin
    if y1<y2 then begin qad[1].y:=y1;y1:=y2;y2:=qad[1].y;end; {setrideni}
    if x1>x2 then begin qad[1].x:=x1;x1:=x2;x2:=qad[1].x;end;

    qad[1].x:=x1;
    qad[1].y:=y1;
    qad[2].x:=x2;
    qad[2].y:=y1;
    qad[3].x:=x2;
    qad[3].y:=y2;
    qad[4].x:=x1;
    qad[4].y:=y2;
    FillPoly(4,qad);
    BarFrame(qad);

    xp:=zx;
    yp:=z;{round(z*(Xasp/Yasp));}
	qad[1].x:=x2;
    qad[1].y:=y1;
    qad[2].x:=x2+xp;
    qad[2].y:=y1-yp;
    qad[3].x:=x2+xp;
    qad[3].y:=y2-yp;
	qad[4].x:=x2;
    qad[4].y:=y2;
    FillPoly(4,qad);
    BarFrame(qad);

    qad[1].x:=x2;
    qad[1].y:=y2;
    qad[2].x:=x2+xp;
    qad[2].y:=y2-yp;
    qad[3].x:=x1+xp;
    qad[3].y:=y2-yp;
	qad[4].x:=x1;
    qad[4].y:=y2;
    FillPoly(4,qad);
	BarFrame(qad);
  end;

procedure hod(x,y:word;h,MaximumC,maxExp:real);
var
    qad : qadr;
	pstr : NameStr;
    XM,YM :word;
    delitel:real;
begin
  XM:=GMaxX;
  YM:=GMaxY;
  SetTextStyle(SmallFont,VertDir,K(4));
  SetTextJustify(CenterText,CenterText);
  if MinimumC>0 then h:=h+MinimumVstP;
  if h>=0 then
    y:=y-YM div 50
   else
    y:=y+YM div 20;
  SetFillStyle(1,KonB(ColorWWBack));
  qad[1].x:=x-XM div 80;
  qad[1].y:=y+YM div 30;
  qad[2].x:=x+XM div 60;
  qad[3].x:=x+XM div 60;
  qad[3].y:=y-YM div 20;
  qad[4].x:=x-XM div 80;
  if Mask1>=5 then begin qad[1].y:=y+YM div 30;
                         qad[3].y:=y-YM div 10;end;
  qad[2].y:=qad[1].y;
  qad[4].y:=qad[3].y;
  FillPoly(4,qad);
  BarFrame(qad);
  delitel:=exp(MaxExp*ln(10));
  h:=h/delitel;
  if MaximumC-MinimumC>=10 then
    str(h:5:0,pstr)
   else
    str(h:4:1,pstr);
  if MaximumC-MinimumC<1 then
    str(h:3:2,pstr);
  if Mask1>0 then str(h:Mask1:Mask2,pstr);
  if Mask1>=5 then OutTextXYC(x,y-YM div 30,pstr)
              else OutTextXYC(x,y,pstr);
end;

function co(c:byte;B:str14):byte;
var PP : char;
  begin
	B:=B+copy(BarvyDef,length(B)+1,14-length(B));
    B:=B[14]+copy(B,1,13);
    pp:=B[c];
    case pp of
      'L','l':co:=0;
      'B':co:=1;
      'G':co:=2;
      'C':co:=3;
      'R':co:=4;
      'M':co:=5;
      'O','o':co:=6;
      'a':co:=7;
	  'A':co:=8;
      'b':co:=9;
      'g':co:=10;
      'c':co:=11;
      'r':co:=12;
	  'm':co:=13;
	  'Y','y':co:=14;
      'W','w':co:=15;
    end;
  end;

function promptstr(Pis:str80;typt:str1;delka,zust:byte;Def:str80):str80;
 var
  BegX,BegY,poz,i,konec:byte;
  poprve:boolean;
  pol:str80;
  key:word;
  label 33;
  begin
    textcolor(colors.uNorm);textbackground(colors.uNorm div 16);
	write(Pis);
    textcolor(colors.dHiLi);textbackground(colors.dHiLi div 16);
	CursorOff;
    BegX:=wherex;
    BegY:=wherey;
    for i:=length(Def)+1 to delka do Def[i]:=' ';
	Def[0]:=chr(delka);
    write(Def);
    gotoxy(BegX+delka,BegY);
    pol:=Def;
    poz:=1;
    poprve:=true;
   repeat
    repeat
      key:=readkbd;
    until (key=_M_)or(key=_ESC_)or(key>=32)or(key=_H_)or(lo(key)=0);
    if lo(key)<>0 then CursorOn;
    if key=_M_ then
      begin
		textbackground(colors.dNorm div 16);textcolor(colors.dNorm);
		gotoxy(BegX-length(Pis),BegY);write(Pis,pol);
        for i:=delka downto 1 do if pol[length(pol)]=' ' then
          pol:=copy(pol,1,length(pol)-1);
        promptstr:=pol;
        if zust=0 then
          begin
			pol[0]:=chr(delka);
            pol:=pis+pol;
            for i:=1 to length(pol) do pol[i]:=' ';
            gotoxy(BegX-length(Pis),BegY);write(pol);
          end;
        CursorOff;
        exit;
      end;
    if key=_ESC_ then
      begin
		textbackground(colors.dNorm div 16);textcolor(colors.dNorm);
        gotoxy(BegX-length(Pis),BegY);write(Pis,Def);
        for i:=delka downto 1 do if Def[length(Def)]=' ' then
          Def:=copy(Def,1,length(Def)-1);
		promptstr:=Def;
        if zust=0 then
          begin
            pol[0]:=chr(delka);
            pol:=pis+pol;
            for i:=1 to length(pol) do pol[i]:=' ';
			gotoxy(BegX-length(Pis),BegY);write(pol);
          end;
        CursorOff;
        exit;
      end;
    if (lo(key)<>0)and(key>=32) then
      if not(poprve) then
	   if (poz>delka)or(konec>delka) then begin sound(1000);delay(300);nosound;end
       else begin
        for i:=delka downto poz do if i>1 then pol[i]:=pol[i-1];
        pol[poz]:=char(key);
        gotoxy(BegX,BegY);
        write(pol);
        poz:=poz+1;
		gotoxy(BegX+poz-1,BegY);
       end
      else
      begin
       for i:=1 to delka do pol[i]:=' ';
       pol[0]:=chr(delka);
	   poprve:=false;
       pol[1]:=char(key);
       gotoxy(BegX,BegY);
       write(pol);
	   poz:=poz+1;
       gotoxy(BegX+poz-1,BegY);
      end;
    if (key=_H_)and(poz>1) then {BackSpace}
        begin
          poprve:=false;
          poz:=poz-1;
          for i:=poz to delka-1 do pol[i]:=pol[i+1];
          pol[delka]:=' ';
          gotoxy(BegX,BegY);
		  write(pol);
          gotoxy(BegX+poz-1,BegY);
        end;
    if lo(key)=0 then
     begin
      if (key<>_home_)and(key<>_End_) then CursorOn;
	  if key=_Del_ then {Del}
		begin
          poprve:=false;
          for i:=poz to delka-1 do pol[i]:=pol[i+1];
          pol[delka]:=' ';
          gotoxy(BegX,BegY);
          write(pol);
          gotoxy(BegX+poz-1,BegY);
        end;
      if key=_F4_ then {F4}
        begin
          poprve:=false;
          pol[poz]:=ToggleCS(pol[poz]);
          gotoxy(BegX,BegY);
		  write(pol);
          gotoxy(BegX+poz-1,BegY);
        end;
      if key=_Ins_ then {Ins}
		begin
          gotoxy(BegX,BegY);
		  poprve:=false;
        end;
      if (key=_Right_)and(poz<=delka) then {->}
       begin
         poprve:=false;
         poz:=poz+1;
         gotoxy(BegX+poz-1,BegY);
       end;
      if (key=_Left_)and(poz>1) then     {<-}
       begin
         poprve:=false;
         poz:=poz-1;
         gotoxy(BegX+poz-1,BegY);
       end;
	if key=_Home_ then
	 if (poz>1)and not poprve then
       begin
         poprve:=false;
         poz:=1;
         gotoxy(BegX+poz-1,BegY);
	   end;
    if key=_End_ then     {End}
     if (poz<=delka) and not poprve then
       begin
         poprve:=false;
         poz:=konec+1;
         gotoxy(BegX+poz-1,BegY);
       end;
     end;
     for konec:=delka downto 1 do if pol[konec]<>' ' then goto 33;
   33:
   until false;
   CursorOff;
  end;

  function LeftUpCh(st:str80):str80;
    var l:byte;
    begin
      l:=length(st)+1;
      repeat
		dec(l);
        if st[l]=' ' then st[0]:=chr(ord(st[0])-1);
      until st[l]<>' ';
      for l:=1 to length(st) do st[l]:=upcase(st[l]);
      LeftUpCh:=st;
    end;

function Nazev(par:ComStr;parametr:ComStr):ComStr;
var i:integer;
	str,parU,par1:ComStr;
    ParFile:text;

begin
  ParU[0]:=Par[0];
  for i:=1 to length(par) do
    parU[i]:=upcase(par[i]);
    begin
      assign(ParFile,parU);
      {$I-}
      reset(ParFile);
	  if IOresult<>0 then
        begin
          SetMsgPar(parU);
          WrLLf10Msg(251);
          GrExit:=true;
          exit;
		end;
      Nazev:='';
      repeat
        readln(ParFile,Par1);
        iores:=ioresult;
        parU:='';
        for i:=1 to length(par1) do
          if (par1[i]<>''''){and(par1[i]<>' ')} then parU:=parU+par1[i];
		par1:=parU;
        for i:=1 to length(par1) do
          parU[i]:=upcase(par1[i]);
        str:='';
        i:=pos(parametr,parU);
        if i=0 then
		  begin
            Nazev:=str;
          end
		else
         begin
           i:=i+length(parametr);
           while (par1[i]<>',')and(i<=length(par1)) do
             begin
               str:=str+par1[i];
               i:=i+1;
             end;
           Nazev:=str;
         end;
      until (eof(ParFile)or(str<>''))or(iores<>0);
	  close(ParFile);
	end;
end; {Nazev}

{------------- ParRad }

procedure ParRad(var Param:Parametry;var Par3:Param3;var Maxim:Maxima;GD:GraphDPtr);

label 44;

var
  par,naz:ComStr;
  i:integer;
  RealPom:Real;
  RGBPtr:GraphRGBDPtr;

function KonParChar(ZN:ExtStr):integer;

  begin
    case ZN[1] of
	  'L','l':KonParChar:=0;
	  'B':KonParChar:=1;
	  'G':KonParChar:=2;
	  'C':KonParChar:=3;
	  'R':KonParChar:=4;
	  'M':KonParChar:=5;
	  'O','o':KonParChar:=6;
      'a':KonParChar:=7;
      'A':KonParChar:=8;
      'b':KonParChar:=9;
      'g':KonParChar:=10;
      'c':KonParChar:=11;
      'r':KonParChar:=12;
      'm':KonParChar:=13;
      'Y','y':KonParChar:=14;
      'W','w':KonParChar:=15
     else
	   KonParChar:=15;
    end;
  end;

begin
  with Param,Par3 do
    begin
		begin                    {inicializace parametru}
		  Typ:='2D';
		  Color:='C1';
		  Print:='DH200Y';
		  NazevF:='';
		  UdajX:='';
		  UdajY:='';
		  UdajZ:='';
		  Hlavicka:='';
		  NazevX:='';
		  NazevY:='';
		  NazevZ:='';
		  Rastr:='N';
		  PopX:='H';
		  Barvy:='';
		  FileParNaz:='';
		  for i:=0 to 9 do
			begin
			  Udaj3[i]:='';
			  Nazev3[i]:='';
			end;
		  Pomer:=50;
		  Border:='B';
		  HeadFrame:='';
		  MakeCls:=true;
		  StupPoly:=0;StupPolyP:=0;
		  MinimumVstP:=0;
		  MinimumC:=0;
		  Mask1:=0;Mask2:=0;
		  for i:=0 to 15 do begin RGB[i].R:=-1;RGB[i].G:=0;RGB[i].B:=0;end; {neprovede se SetRGBPalette}
		end;
	 if GD^.T=nil then Typ:='' else Typ:=RunShortStr(GD^.T);
	 for i:=1 to length(Typ) do Typ[i]:=upcase(Typ[i]);
	 if Typ='2DBAR' then Typ:='2D';
	 if Typ='GROUP' then Typ:='DS';
	 if Typ='STACKBAR' then Typ:='SB';
	 if Typ='GROUPLINE' then Typ:='DL';
	 if Typ='3DBAR' then Typ:='QS';
	 if Typ='3DLINE' then Typ:='QL';
	 if Typ='APPROX' then Typ:='Lin';
	 if Typ='POLYREG' then Typ:='LP';
	 if Typ='CIRCLE' then Typ:='Cir';
	 if Typ='CIR' then Typ:='Cir';    { kvuli kopatibilite }
	 if Typ='LIN' then Typ:='Lin';

	 if not((Typ='2D') or (Typ='3D') or                     { povolene typy}
			(Typ='Lin') or (Typ='L3') or (Typ='Cir') or
			(Typ='LP') or (Typ='LP3') or
			(Typ='DS') or (Typ='QS') or (Typ='DL') or (Typ='QL')
					   or(Typ='SB')) then
										 Typ:='2D';

	 if GD^.H=nil then Hlavicka:=GD^.FD^.name else Hlavicka:=RunShortStr(GD^.H);
	 NazevF:=GD^.FD^.name;
	 if (Typ[1]='L')and(GD^.X^.Typ<>'F')and(GD^.X^.Typ<>'D')and(GD^.X^.Typ<>'N')and(not Interact) then
	   begin naz:='X';SetMsgPar(naz);WrLLf10Msg(206);GrExit:=true;exit;end;
	 if (Typ[1]<>'L')and(GD^.X^.Typ='T')and(not Interact) then
	   begin naz:='X';SetMsgPar(naz);WrLLf10Msg(206);GrExit:=true;exit;end;
	 if AutoGr then Interact:=true;
	 if (GD^.X=nil)and(not Interact) then
	   begin
		 naz:='X';
		 SetMsgPar(naz);
		 WrLLf10Msg(204);
		 GRexit:=true;
		 exit;
	   end;
	 if GD^.X<>nil then UdajX:=GD^.X^.name else UdajX:='';
	 UdajZ:='';
	 if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
	   begin
		 GD^.Y:=GD^.ZA[0];
		 GD^.Z:=GD^.ZA[1];
		 if (GD^.Y=nil)and(not Interact) then
		   begin
			 naz:='Z';
			 SetMsgPar(naz);
			 WrLLf10Msg(204);
			 GRexit:=true;
			 exit;
		   end;
		 if GD^.Y<>nil then UdajY:=GD^.Y^.name else UdajY:='';
	   end;
	 if ((Typ='3D')or(Typ='L3')or(Typ='LP3'))and(GD^.Z=nil)and(not Interact) then
	   begin
		 naz:='Z';
		 SetMsgPar(naz);
		 WrLLf10Msg(204);
		 GRexit:=true;
		 exit;
	   end;
	 if GD^.Z<>nil then UdajZ:=GD^.Z^.name else UdajZ:='';
	 if (Typ='2D')and(UdajZ<>'') then Typ:='3D';
	 if (Typ='Lin')and(UdajZ<>'') then Typ:='L3';
	 if (Typ='LP')and(UdajZ<>'') then Typ:='LP3';
	 if GD^.HX=nil then NazevX:=UdajX else NazevX:=RunShortStr(GD^.HX);
	 if GD^.HY=nil then NazevY:=UdajY else NazevY:=RunShortStr(GD^.HY);
	 if GD^.HZ=nil then NazevZ:=UdajZ else NazevZ:=RunShortStr(GD^.HZ);
	 PocZ:=0;
{     if (Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL') then}
	   begin
		 for m:=0 to 9 do
		   begin
			 if GD^.ZA[m]<>nil then Udaj3[m]:=GD^.ZA[m]^.name else
									Udaj3[m]:='';
			 if (m=0)and(Udaj3[m]='') then
				if GD^.Z<>nil then Udaj3[m]:=GD^.Z^.name else Udaj3[m]:='';
			 if (m=0)and(Udaj3[m]='')and(not Interact) then
			   begin
				 naz:='Z';
				 SetMsgPar(naz);
				 WrLLf10Msg(204);
				 GRexit:=true;
				 exit;
               end;
			 if Udaj3[m]<>'' then
			  begin
			   if GD^.HZA[m]<>nil then Nazev3[m]:=RunShortStr(GD^.HZA[m]) else Nazev3[m]:=Udaj3[m];
			   if (m=0)and(Nazev3[m]='') then Nazev3[m]:=UdajZ;
			   if Nazev3[m]='' then Nazev3[m]:=Udaj3[m];
			   if Udaj3[m]<>'' then PocZ:=PocZ+1;
			  end;
		   end;
		 PocZ:=PocZ-1;
	   end;
	 if GD^.S<>nil then RealPom:=RunReal(GD^.S) else RealPom:=0;
	 if (RealPom=0) and (Typ='2D') then RealPom:=100;
	 if (RealPom=0) and (Typ='DS') then RealPom:=80;
	 if (RealPom=0) and (Typ='SB') then RealPom:=80;
	 if (RealPom>100)or(RealPom<1) then Pomer:=50 else Pomer:=trunc(RealPom);
	 if GD^.C<>nil then Color:=RunShortStr(GD^.C) else
       if (GrDriv=1)or(GrDriv=2)or(GrDriv=5)or(GrDriv=7)
		then Color:='F'
		 else Color:='C';
     Color[1]:=upcase(Color[1]);
    if length(Color)<2 then
	  if typ[1]='Q' then Color:=Color+'3' else Color:=Color+'1'
	 else
	  if (Color[2]<>'1')and(Color[2]<>'2')and
                                (Color[2]<>'3') then begin
													   Color[2]:=' ';
													   Color[0]:=char(1);
													 end;
	 if Color='' then Color:='C';
     PopX:='';
     if GD^.D<>nil then PopX:=RunShortStr(GD^.D) else PopX:=' ';
     PopX[1]:=upcase(PopX[1]);
	 if (PopX[1]<>'H')and(PopX[1]<>'V')and(PopX[1]<>'I')and(PopX[1]<>' ') then PopX:='H';
     if GD^.R<>nil then Rastr:=RunShortStr(GD^.R) else
	   if Typ[1]='C' then Rastr:='Y' else Rastr:='N';
     if length(Rastr)>=3 then val(Rastr[3],Mask2,i);if i>0 then Mask2:=0;
	 if Mask2=0 then Mask1:=0 else Mask1:=Mask2+3;
	 Rastr:=copy(Rastr,1,1);
	 Rastr[1]:=upcase(Rastr[1]);
     if (Rastr[1]<>'Y')and(Rastr[1]<>'N')and(Rastr[1]<>'H') then Rastr:='N';
     if GD^.CO<>nil then Barvy:=RunShortStr(GD^.CO) else Barvy:='';
	 Interact:=GD^.Interact;
	 if GD^.SP<>nil then RealPom:=RunReal(GD^.SP) else RealPom:=0;
	 if (RealPom>64)or(RealPom<0) then StupPoly:=0 else StupPoly:=round(RealPom);

	 VyberVet:=false;
	 if GD^.RS<>nil then begin RealPom:=RunReal(GD^.RS);VyberVet:=true;end else RealPom:=0;
	 if RealPom<1 then StartVeta:=1 else StartVeta:=round(RealPom);
	 if GD^.RN<>nil then begin RealPom:=RunReal(GD^.RN);VyberVet:=true;end else RealPom:=0;
     if RealPom<1 then PocVet:=max else PocVet:=round(RealPom);
	 if PocVet>max then PocVet:=max;
	 if GD^.MAX<>nil then RealPom:=RunReal(GD^.MAX) else RealPom:=0;
     if RealPom<1 then Maxim.MaximumVst:=0 else Maxim.MaximumVst:=RealPom;
     if GD^.MIN<>nil then RealPom:=RunReal(GD^.MIN) else RealPom:=0;
     if RealPom<-1 then Maxim.MinimumVst:=0 else Maxim.MinimumVst:=RealPom;

     if GD^.P<>nil then Print:=RunShortStr(GD^.P) else Print:='';
	 if Print='' then Print:='DH200Y' else Print:=Print+'        ';
	 if upcase(Print[1])='P' then goto 44;
     if (upcase(Print[1])<>'D')and(upcase(Print[1])<>'M')and(upcase(Print[1])<>'H')
        then Print[1]:='D';
	 if upcase(Print[2])='V' then Print[2]:='V' else Print[2]:='H';  { smer tisku }
	 if (upcase(Print[3])='1')or(upcase(Print[3])='2')or(upcase(Print[3])='3')
		 then  else Print[3]:='0';                                   { hustota }
	 if (ord(Print[4])>=ord('0'))and(ord(Print[4])<=ord('9'))
		 then  else Print[4]:='0';                                   { cm od leva }
	 if (ord(Print[5])>=ord('0'))and(ord(Print[5])<=ord('9'))
		 then  else Print[5]:='0';                                   { radku od hor.okr}
	 if upcase(Print[6])='N' then Print[6]:='N' else Print[6]:='Y';  { odstrankovat po }

44:
	 RGBPtr:=GD^.RGB;
	 while RGBPtr<>nil do
	   begin
		 RGB[KonParChar(RunShortStr(RGBPtr^.Barva))].R:=round(RunReal(RGBPtr^.R));
		 RGB[KonParChar(RunShortStr(RGBPtr^.Barva))].G:=round(RunReal(RGBPtr^.G));
		 RGB[KonParChar(RunShortStr(RGBPtr^.Barva))].B:=round(RunReal(RGBPtr^.B));
		 RGBPtr:=RGBPtr^.Chain;
	   end;

{	if Interact then exit; }
	with ViewPort do
	  begin

		if GD^.WW<>nil then with GD^.WW^ do
		  begin
			RunWFrml(W,WFlags,ViewPortGD);
			if ColFrame<>nil then
			  begin naz:=RunShortStr(ColFrame); ColorWWFrame:=naz[1];end else ColorWWFrame:=BarvyPasDef[Colors.uNorm];
			if ColBack<>nil then
			  begin naz:=RunShortStr(ColBack);ColorWWBack:=naz[1];end else ColorWWBack:='L';
			if ColFor<>nil then
			  begin naz:=RunShortStr(ColFor);ColorWWFor:=naz[1];end else ColorWWFor:='w';
			MakeCls:=(WFlags and WNoPop)=0;
			if (WFlags and WHasFrame)=0 then Border:='N' else
			if (WFlags and WDoubleFrame)=0 then Border:=' ' else Border:='=';
			HeadFrame:=RunShortStr(Top);  WWShadow:=(WFlags and WShadow)<>0;
		  end
		else
		  begin
			with ViewPort80x25 do
			   begin
				x1:=1;y1:=1;x2:=80;y2:=25;
				Clip:=ClipOn;
			   end;

			Border:='B';
			HeadFrame:='';
			WWShadow:=false;
			ColorWWFrame:=BarvyPasDef[Colors.uNorm];
			ColorWWBack:='L';
			ColorWWFor:='w';
			naz:=copy(Print,1,5);
			for i:=1 to length(naz) do naz[i]:=upcase(naz[i]);
			MakeCls:=naz<>'PAU00';
		  end;
{		if (Hlavicka=NazevF)and(HeadFrame<>'') then Hlavicka:=HeadFrame;}
		RectToPix(ViewPort80x25.x1,ViewPort80x25.y1,
				  ViewPort80x25.x2,ViewPort80x25.y2,x1,y1,x2,y2);
		SetViewPort(x1,y1,x2,y2,ClipOn);
	  end;
  end;
end;

procedure TxWPar(GD:GraphDPtr;stop:boolean);

var
{  ParW:ParamW;}
  WPtr:GraphWDPtr;
  VPtr:GraphVDPtr;
  n1,n2:ComStr;
  LstrPtr:LongStrPtr;
  i:integer;

{label 20,30,40,50,9;}

procedure Zavri;
  begin
    DelAllParWInTWork;
{    if stop then begin GRexit:=true;exit;end;}
  end;

function KonParChar(ZN:ExtStr):integer;

  begin
    case ZN[1] of
      'L','l':KonParChar:=0;
      'B':KonParChar:=1;
      'G':KonParChar:=2;
      'C':KonParChar:=3;
      'R':KonParChar:=4;
      'M':KonParChar:=5;
	  'O','o':KonParChar:=6;
	  'a':KonParChar:=7;
	  'A':KonParChar:=8;
      'b':KonParChar:=9;
	  'g':KonParChar:=10;
      'c':KonParChar:=11;
      'r':KonParChar:=12;
	  'm':KonParChar:=13;
	  'Y','y':KonParChar:=14;
	  'W','w':KonParChar:=15
     else
       KonParChar:=15;
    end;
      
  end; {KonParChar}
{----------------------- }
begin
   with ParW do
   begin
     WPtr:=GD^.W;
	 while WPtr<>nil do
	   begin
         if (RunReal(WPtr^.XZ)<1)or(RunReal(WPtr^.XZ)>80) then
            begin n1:='XZ';n2:='W';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;exit;end
		   else XZ:=round(RunReal(WPtr^.XZ));
         if (RunReal(WPtr^.YZ)<1)or(RunReal(WPtr^.YZ)>25) then
            begin n1:='YZ';n2:='W';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;exit;end
		   else YZ:=round(RunReal(WPtr^.YZ));
		 if (RunReal(WPtr^.XK)<1)or(RunReal(WPtr^.XK)>80) then
			begin n1:='XK';n2:='W';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;exit;end
           else XK:=round(RunReal(WPtr^.XK));
         if (RunReal(WPtr^.YK)<1)or(RunReal(WPtr^.YK)>25) then
            begin n1:='YK';n2:='W';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;exit;end
           else YK:=round(RunReal(WPtr^.YK));
		 BarPoz:=KonParChar(RunShortStr(WPtr^.BarPoz));
         BarPis:=KonParChar(RunShortStr(WPtr^.BarPis));
         if (BarPis=Barpoz)and(BarPoz=15) then BarPoz:=0;
         if (GrDriv=HercMono)or(GrDriv=EGAMONO) then BarPoz:=0;
         Text:='';
		 FTx:=RunLongStr(WPtr^.Text);
{		 FTxt:=RunLongStr(WPtr^.Text);}
		 Ftxt.LL:=FTx^.LL;
		 for ftn:=1 to FTxt.LL do Ftxt.A[ftn]:=FTx^.A[ftn];
		 ReleaseStore(FTx);
		 StoreParWInTWork{(ParW)};
		 WPtr:=WPtr^.Chain;
	   end;
	 VPtr:=GD^.V;
	 while VPtr<>nil do
	   begin
		 if (RunReal(VPtr^.XZ)<1)or(RunReal(VPtr^.XZ)>80) then
			begin n1:='XZ';n2:='V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;exit;end
		   else XZ:=round(RunReal(VPtr^.XZ));
		 if (RunReal(VPtr^.YZ)<1)or(RunReal(VPtr^.YZ)>25) then
			begin n1:='YZ';n2:='V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;exit;end
		   else YZ:=round(RunReal(VPtr^.YZ));
		 if (RunReal(VPtr^.Velikost)<1)or(RunReal(VPtr^.Velikost)>9) then
			BarPoz:=3 else BarPoz:=round(RunReal(VPtr^.Velikost));
		 BarPis:=KonParChar(RunShortStr(VPtr^.BarPis));
		 Text:=RunShortStr(VPtr^.Text);
		 FTxt.LL:=0; {FTxt:=nil;}
		 StoreParWInTWork{(ParW)};
		 VPtr:=VPtr^.Chain;
	   end;
   end;

end;

procedure ClrScrG(Color:str14);
var qad : array [1..4] of PointType;
	LastColor:word;
begin
  with ViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
  qad[1].x:=0;
  qad[1].y:=0;
  qad[2].x:=GMaxX+1;
  qad[2].y:=0;
  qad[3].x:=GMaxX+1;
  qad[3].y:=GMaxY+2;
  qad[4].x:=0;
  qad[4].y:=GMaxY+2;
  SetFillStyle(1,KonB(ColorWWBack));
  LastColor:=GetColor;
  SetColor(KonB(ColorWWBack));
  FillPoly(4,qad);
  SetColor(LastColor);
{    with ViewPort80x25 do
	 begin
	  if y2<25 then ScrColor(x1+1,y2,x2-x1,colors.ShadowAttr);
	  if x2<80 then for i:=y1 to y2 do ScrColor(x2,i,2,colors.ShadowAttr);
	 end;}
end;

function K(Size:word):word;
var XMp:integer;
begin
	XMp:=round(GMaxX/3*2);
    if GMaxY<XMp then XMp:=GMaxY;
	SizeLetter:=round(Size*XMp*0.8/round(GetMaxY/3*2));
    if (GrDriv>=3)and(GrDriv<=7)and(SizeLetter>1) then SizeLetter:=SizeLetter-1;
	K:=SizeLetter;
end;

procedure OutTextXYC(X,Y:integer;str:string);
var TextSet:TextSettingsType;
	i:byte;
begin
{  if Fonts.VFont=foLatin2 then
	for i:=1 to length(str) do ConvKamenLatin(str[i],1,false);  ver3.2 zruseno- fonty}
  GetTextSettings(TextSet);
  if (TextSet.Font=TriplexFont)and(SizeLetter=0) then
	begin
	  SetTextStyle(SmallFont,TextSet.Direction,3);
	  graph.OutTextXY(X,Y,str);
	end;
  if SizeLetter>0 then graph.OutTextXY(X,Y,str);
end;

procedure SetWindow(ViewPort:ViewPortType);
begin
  with ViewPort do SetViewPort(x1,y1,x2,y2,Clip);
end;

function KonB(KP:str9):integer;
   var KPC:char;
begin
	KPC:=KP[1];
	case KPC of
	  'l','L':KonB:=0;
	  'B':KonB:=1;
	  'G':KonB:=2;
	  'C':KonB:=3;
	  'R':KonB:=4;
	  'M':KonB:=5;
	  'O','o':KonB:=6;
	  'a':KonB:=7;
	  'A':KonB:=8;
	  'b':KonB:=9;
	  'g':KonB:=10;
	  'c':KonB:=11;
	  'r':KonB:=12;
	  'm':KonB:=13;
	  'y','Y':KonB:=14;
	  'w','W':KonB:=15
	 else
	   KonB:=15;
	end;
end;

procedure ShowShadow;
function  min(a,b : integer): integer;
begin
 if a>b then min:=b else min:=a ;
end;

var x1,y1,x2,y2,i,j,x,y:integer;
	LastColor:word;
  begin

	if GraphShadow then
	  begin
		LastColor:=GetColor;
		LastViewPort:=ViewPort;
		RectToPix(ViewPort80x25.x2,min(ViewPort80x25.y1+1,25),
			min(ViewPort80x25.x2+2,80),min(ViewPort80x25.y2+1,25),x1,y1,x2,y2);
		SetViewPort(x1,y1,x2,y2,ClipOff);
		if ViewPort80x25.x2<80 then GrayWin;
		RectToPix(min(ViewPort80x25.x1+2,80),min(ViewPort80x25.y2+1,25),
			 min(ViewPort80x25.x2+2,80),min(ViewPort80x25.y2+1,25),x1,y1,x2,y2);
		SetViewPort(x1,y1,x2,y2,ClipOff);
		if ViewPort80x25.y2<25 then GrayWin;
		with LastViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
		SetColor(LastColor);
	  end
	 else
	  begin
		x:=min(2,TxtCols-ViewPort80x25.x2);
		if x>0 then for i:=ViewPort80x25.y1 to ViewPort80x25.y2 do
		  ScrColor(ViewPort80x25.x2,i,x,colors.ShadowAttr);
		y:=min(1,TxtRows-ViewPort80x25.y2);
		if y=1 then ScrColor(ViewPort80x25.x1+1,ViewPort80x25.y2,ViewPort80x25.x2-ViewPort80x25.x1+x-1,colors.ShadowAttr);
	  end;
  end;

procedure ShowOnlyShadow(c1,r1,c2,r2:integer);

function  min(a,b : integer): integer;
begin
 if a>b then min:=b else min:=a ;
end;

var x1,y1,x2,y2,i,j,x,y:integer;
  begin
	if GraphShadow then
	  begin
		LastColor:=GetColor;
		LastViewPort:=ViewPort;
		RectToPix(c2+1,min(r1+1,25),min(c2+2,80),min(r2+1,25),x1,y1,x2,y2);
		SetViewPort(x1,y1,x2,y2,ClipOff);
		if c2<80 then GrayWin;
		RectToPix(min(c1+2,80),min(r2+1,25),min(c2+2,80),min(r2+1,25),x1,y1,x2,y2);
		SetViewPort(x1,y1,x2,y2,ClipOff);
		if r2<25 then GrayWin;
		with LastViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
		SetColor(LastColor);
	  end
	 else
	  begin
		x:=min(2,TxtCols-c2);
		if x>0 then for i:=r1 to r2 do ScrColor(c2,i,x,colors.ShadowAttr);
		y:=min(1,TxtRows-r2);
		if y=1 then ScrColor(c1+1,r2,c2-c1-1,colors.ShadowAttr);
	  end;
  end;

procedure ShowFrame(Border:char);
var i,n,cols,rows:word;
procedure WrHd(const Hd:string;Row,MaxCols:word);
var s:ScreenStr;
begin
  if Hd='' then exit; s:=' '+Hd+' ';
  if length(s)>MaxCols then s[0]:=char(MaxCols);
  gotoxy((MaxCols-length(s)) div 2+2,Row); write(s);
end;

begin
  n:=0;
  LastColor:=GetColor;
  TextAttr:=KonB(ColorWWFrame);
  if (Border=' ')or(Border='=') then
	begin
	  with ViewPort80x25 do
		begin
		  if WWShadow then ShowShadow;
		  if Border='=' then n:=9;
		  cols:=x2-x1+1; rows:=y2-y1+1;
		  ScrWrFrameLn(X1-1,Y1-1,N,cols,KonB(ColorWWFrame));
		  for i:=1 to Rows-2 do
			begin
			  ScrWrChar(X1-1,Y1-1+i,FrameChars[N+6],KonB(ColorWWFrame));
			  ScrWrChar(X1-2+cols,Y1-1+i,FrameChars[N+6],KonB(ColorWWFrame));
			end;
		  ScrWrFrameLn(X1-1,Y2-1,N+3,cols,KonB(ColorWWFrame));
		  Window(x1,y1,x2,y2);
		  x1:=x1+1;y1:=y1+1;x2:=x2-1;y2:=y2-1;
		  RectToPix(x1,y1,x2,y2,ViewPort.x1,ViewPort.y1,ViewPort.x2,ViewPort.y2);
		  with ViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
		  WrHd(HeadFrame,1,cols-2);
		end;
	end;
  if Border='B' then
	begin
	  SetLineStyle(0,0,1);
{	  if GrDriv=EGAMONO then SetColor(15) else SetColor(KonB(ColorWWFrame));}
	  if GrDriv=EGAMONO then SetColor(15) else SetColor(KonB(BarvyPasDef[Colors.Unorm and $0F]));
	  Rectangle(0,0,GMaxX,GMaxY);
	  Rectangle(1,1,GMaxX-1,GMaxY-1);
	  Rectangle(2,2,GMaxX-2,GMaxY-2);
	  with ViewPort do begin x1:=x1+3;y1:=y1+3;x2:=x2-3;y2:=y2-3;
	  SetViewPort(x1,y1,x2,y2,ClipOn); end;
	  Border:=' ';
	end;
  SetColor(LastColor);
end;

end.
