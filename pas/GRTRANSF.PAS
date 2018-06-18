{$A-,B-,F+,I+,R-,S+,V+,X+}
{$G-  286inst}
{$D-  -degub}
{$L-  -local symbols}
{$O+  overlay}
{ GRTransf                    MK - upraveno zhasinani obraz.pri 256 barvach
  -posun a inverze oken
  -zobrazeni souboru PCX v okne s posunem
  -pomocne funkce : IsMode256Col,IsDACRegisters
}
unit GrTransf;

interface

procedure MovePict(x,y,w,h, xn,yn : integer);
procedure Inverze(x1,y1,x2,y2 : integer);
procedure InverzeWin;
procedure GrayWin;
function  IsMode256Col:boolean;
function  IsDACRegisters: BOOLEAN;

function  ShowPCX(NameFile: string) :integer;
function  ShowPCXAt(NameFile:string; x1,y1,x2,y2: integer):integer;
function  SavePCX(NameFile: string) :integer;

{* Chybova hlaseni ShowPCX :
  0 .. vse ok
  1 .. soubor nelze otevrit (nenalezen)
  2 .. nerozpoznan format PCX souboru
  3 .. mozne zobrazit pouze na VGA karte (256 barev)

 * Chybova hlaseni SavePCX :
  0 .. vse OK
  1 ..do souboru nelze zapisovat
}

const
 { bitove masky pro zobrazovani na okrajich obrazku }

 fbits : array [0..7] of byte = ( $FF,$7F,$3F,$1F,$0F,$07,$03,$01 ) ;
 ebits : array [0..7] of byte = ( $80,$C0,$E0,$F0,$F8,$FC,$FE,$FF ) ;

 HOMEKEY = $47;                       { kody klaves }
 ARROWUP = $48;
 PAGEUP  = $49;
 ARROWDOWN = $50;
 PAGEDOWN = $51;
 ARROWLEFT = $4b;
 ARROWRIGHT = $4d;
 ENDKEY   = $4f;
 ESC      = $1b;
 ENTER    = $D;
									  { prirustky pri posunu okna}
 St_Up = 16; St_Down = 16;            { lib. hodnoty }
 St_Right = 24; St_Left = 24;         { pouze nasobky 8 }

type


  TPL = array[1..4] of ^Byte;   { jeden obrazovy radek (ma 4 roviny)}
  TL  = array[0..0] of Byte;    { jedna fiktivni bit.rovina z radku }
  TLinesOff = array [0..0] of longint;

  {
    Tshpcx  slouzi dekompresi souboru ve formatu PCX,otvira
    soubor, cte a kotroluje hlavicku,metoda getbyte vraci byte a
    posouva virt.ukazatel v souboru,pri cteni za koncem souboru
	vraci 0 ;
  }
  {Tshpcx = OBJECT}
var
       sh_fp	    : file ;
       sh_grDriver     : integer;

	   sh_Xwo,sh_Ywo,sh_Ww,sh_Hw,            { rozmery a pozice okna }
       sh_Xfo,sh_Yfo,sh_Wf,sh_Hf   :integer; { rozmery a pozice obrazku }

       sh_LinesOff : ^longint;      { pole obsahujici pocatky radku }
       sh_LineBeg  : longint;

	   sh_fbuf : array[1..80] of byte; { buffer pro cteni ze souboru }
       sh_pbuf : word ;             { ukazatel v bufferu na akt.pozici }
       sh_count  : word ;           { pro dekodovani metody RLE }
	   sh_value  : byte ;           { pro RLE }

       sh_fBPL     : word ;         { bajtu na radek }
       sh_fbPP     : byte ;         { bitu na pixel }
       sh_fPlanes  : byte ;         { pocet bitovych rovin }
	   sh_bMode13  : BOOLEAN;       { obrazek s 256 barv,300x200 }

								 { pro zobrazovani na okrajich obr. }
       sh_fb,sh_fBt,			 { prvni bit,bajt patrici do obr. }
       sh_eb,sh_eBt	: word ;	 { posledni bit,bajt patrici do obr.}
       sh_buf  	: word ;	 { vyr.pamet pro zdvihy k zarovnani sl.}
       sh_PtrE	: word ;	 { posl.obsazeny bit v buf 15,14 .. 0}

	   sh_PL    : TPL;        { pole pro jeden radek }
	   sh_actPlane,                 { akt.bitova rovina 1,2,3,4 }
	   sh_actByte     : integer;    { por. cislo bajtu (0,1,...) }
	   sh_ScrDelay    : word; {doplneno MK 30.110.94}


	   function    sh_Init(NameFile:string; x,y,w,h : integer) : integer;
       procedure   sh_CheckWinSize(x,y,w,h : integer);
       procedure   sh_SetFbuf;		{ cteni do bufferu }
       function    sh_GetByte : byte;
	   procedure   sh_StartShow;    { inicializace a prvni vykresleni }
	   procedure   sh_ReadLine;
(*       procedure   ReadHCGLine;*)
       procedure   sh_ReadLineNo(line:integer);
	   procedure   sh_ShowLine(Xscreen,Yscreen,Xfile,w:integer);

       procedure   sh_Loop;

	   procedure   sh_ReadShow(x,y,w,h,xs,ys : integer);
       function    sh_outbyte : byte ;
	   procedure   sh_InitBuf(Xfile:integer);	 { pro vnitrni pouziti }
{  END ;}


implementation

  uses dos,graph,drivers,grglob,grinit,base,obaseww;

{ pomocne funkce **}

{ MovePict
  presune obrazek z pozice x,y, o rozmerech w,h
  na pozici xn,yn  (rozmery zachova)
}
procedure MovePict(x,y,w,h, xn,yn : integer);

 procedure MovePict256;
 var
  i : integer;
  Pscr,Pdst : ^Byte;
  P : ^Byte;
  video : word;
 begin

   if x=xn then                      { VERTIKALNI POSUNY v^ }
	 if yn<y then
        for i:=0 to h-1 do begin
		   Pscr:=Ptr($A000+(320 shr 4)*(i+y),x);
           Pdst:=Ptr($A000+(320 shr 4)*(i+yn),x);
		   Move(Pscr^,Pdst^,w);
		end
	 else
		for i:=h-1 downto 0 do begin
		   Pscr:=Ptr($A000+(320 shr 4)*(i+y),x);
		   Pdst:=Ptr($A000+(320 shr 4)*(i+yn),x);
		   Move(Pscr^,Pdst^,w);
		end
   else                             { HORIZONTALNI POSUNY  <> }
	 if xn<x then
		for i:=0 to h-1 do begin
		   Pscr:=Ptr($A000+(320 shr 4)*(i+y),x);
		   Pdst:=Ptr($A000+(320 shr 4)*(i+y),xn);
		   Move(Pscr^,Pdst^,w);
		end
	 else begin
		GetMem(P,w);
		for i:=h-1 downto 0 do begin
		   Pscr:=Ptr($A000+(320 shr 4)*(i+y),x);
		   Pdst:=Ptr($A000+(320 shr 4)*(i+yn),xn);
		   Move(Pscr^,P^,w);
		   Move(P^,Pdst^,w);
		end;
		FreeMem(P,w);
	 end;
  end;


var
  P      : pointer;
  i,s      : integer;
begin
 if (w<=0) OR (h<=0) then begin
   {beep;}
   exit;                           { okno je mensi nez kostanty St_Left,... }
 end;
 if IsMode256Col then
   MovePict256
 else begin
   w:=w-1;						  {19.3.94}
   h:=h-1;						  {w/h  od tohoto bodu neudava sirku/vysku}
								  {ale hodnotu, ktera po pricteni k x/y }
								  {da bod x2/y2 viz GetImage() !!!!!!!!!}
   s:=ImageSize(0,0,w,0);
   GetMem(P,s);                      { vyr.pamet pro jeden radek }
   if x=xn then                      { VERTIKALNI POSUNY v^ }
	 if yn<y then
		for i:=0 to h do begin
		   GetImage(x,y+i,x+w,y+i,P^);
		   PutImage(x,yn+i,P^, NormalPut);
		end
	 else
		for i:=h downto 0 do begin
		   GetImage(x,y+i,x+w,y+i,P^);
		   PutImage(x,yn+i,P^, NormalPut);
		end
   else                              { HORIZONTALNI POSUNY  <> }
	 for i:=0 to h do begin
		GetImage(x,y+i,x+w,y+i,P^);
		PutImage(xn,y+i,P^, NormalPut);
	 end;
   FreeMem(P,s);
 end;
end; { MovePict }


function IsMode256Col:boolean;
var
  REGS : Registers;
begin
  REGS.AH:=$0f;
  INTR($10,REGS);
  IsMode256Col:=REGS.AL=$13;
end;

function  min(a,b : integer): integer;
begin
 if a>b then min:=b else min:=a ;
end;

function  max(a,b : integer): integer;
begin
 if a<b then max:=b else max:=a ;
end;

{ InverzeWin
  podle nastaveni (SetViewport) urci vyrez na obrazovce a provede jeho
  inverzi
}
procedure InverzeWin;
var
  vp     : ViewportType;
begin
  GetViewSettings(vp);
  with vp do
	Inverze(x1,y1,x2,y2);
end;



{ Inverze
  provede inverzi okna x1,y1  x2,y2
  pro EGA,VGA256,HERCmono je inverze implementovana primym
  pristupem do pameti, jinak pouziva sluzby Pascalu
}
procedure Inverze(x1,y1,x2,y2 : integer);
var
  b : byte;
  x,y,p : integer;
  fb,eb,fBt,eBt : integer;
  maskR,maskW   : word ;
  gm,gd  : integer;
  vp     : ViewportType;
  video,o : word ;
begin
  DetectGraph(gd,gm);
  fb := x1 mod 8; fBt := x1 div 8 ;   { prvni bit/bajt na radku }
  eb := x2 mod 8; eBt := x2 div 8 ;   { posledni  ... }
  case gd of
   EgaMono,VGA,EGA: begin
    video := $A000;
    if IsMode256Col then  begin                 { rezim 320x200,256 barev }
      x1:=min(x1,319);y1:=min(y1,199);
	  x2:=min(x2,319);y2:=min(y2,199);
	  INC(video,(320 shr 4)*y1);
      for y:=y1 to y2 do begin
        for x:=x1 to x2 do
          mem[video:x]:=NOT mem[video:x];
        INC(video,320 shr 4);
      end;
    end
    else begin
      INC(video,(80 shr 4)*y1) ;               { inverze okraju }
	  for y:=y1 to y2 do begin
		maskW := $0100 ;
        maskR := $0000 ;
        for p:=1 to 4 do begin
          portw[$3C4]:= $0002 OR maskW ;
          portw[$3CE]:= $0004 OR maskR ;
		  maskW := maskW shl 1 ;
		  INC(maskR,$0100);
          if fBt=eBt then begin
			b:=mem[video:fBt];
            mem[video:fBt]:=   b and NOT(fbits[fb] and ebits[eb])  or
                            NOT b and (fbits[fb] and ebits[eb]);
          end
          else begin
            b:=mem[video:fBt];             { levy okraj }
            mem[video:fBt]:=(b and NOT fbits[fb]) or (NOT b and fbits[fb]);
            b:=mem[video:eBt];             { pravy okraj }
            mem[video:eBt]:=(b and NOT ebits[eb]) or (NOT b and ebits[eb]);
          end;
		end;
		INC(video,80 shr 4);
      end;
	  portw[$3C4]:=$0F02 ;  { povolit zapis do vsech rovin - default nast.}
	  portw[$3CE]:=$0004 ;
      video := $A000 + (80 shr 4)*y1 ;
      portw[$3CE] :=$1803;                     { rychla inverze vnitrku }
      for y:=y1 to y2 do begin
        for x:=fBt+1 to eBt-1 do begin
		  p:=mem[video:x];
          mem[video:x]:=$FF;
        end;
        INC(video,80 SHR 4);
      end;
      portw[$3CE]:=$0003;
    end;
  end;
  HERCmono: begin
    for y:=y1 to y2 do begin
	  o:=90*(y SHR 2)+((y AND 3) SHL 13);
	  if fBt=eBt then begin
        b:=mem[$B000:o+fBt];
        mem[$B000:o+fBt]:=   b and NOT(fbits[fb] and ebits[eb])  or
                        NOT b and (fbits[fb] and ebits[eb]);
      end
      else begin
        b:=mem[$B000:o+fBt];             { levy okraj }
        mem[$B000:o+fBt]:=(b and NOT fbits[fb]) or (NOT b and fbits[fb]);
		for x:=fBt+1 to eBt-1 do
          mem[$B000:o+x]:=NOT mem[$B000:o+x];
        b:=mem[$B000:o+eBt];             { pravy okraj }
        mem[$B000:o+eBt]:=(b and NOT ebits[eb]) or (NOT b and ebits[eb]);
      end;
    end
  end;
  else
	for y:=y1 to y2 do
      for x:=x1 to x2 do
		putpixel(x,y,NOT getpixel(x,y));
 end;
end; { InverzeWin }


procedure GrayWin;
{ GrayWin
  podle nastaveni (SetViewport) urci vyrez na obrazovce a provede jeho
  sedou-inverzi, (barvy 0-7=>0, 8-15=>15);
  pro EGA,VGA256,HERCmono je inverze implementovana primym
  pristupem do pameti, jinak pouziva sluzby Pascalu
}

const
  fmask : array [0..7] of byte = ( $00,$80,$C0,$E0,$F0,$F8,$FC,$FE ) ;
  emask : array [0..7] of byte = ( $7F,$3F,$1F,$0F,$07,$03,$01,$00 ) ;

var
  x,y : integer;
  fb,eb,fBt,eBt : integer;
  vp     : ViewportType;
  video  : word ;


  procedure  EGAGray;     {!!! navrzeno pouze pro 15 barev == 4 bit planes}
  var count,x,y : integer;
      p0,p1,p2,p3 : byte;
      pvideo : ^Byte;

	procedure FillP0123(x : integer);
	begin
      portw[$3CE]:= $0004; p0:=mem[video:x]; { cteni z 0. b.rov }
      portw[$3CE]:= $0104; p1:=mem[video:x]; { cteni z 1. b.rov }
      portw[$3CE]:= $0204; p2:=mem[video:x]; { cteni z 2. b.rov }
      portw[$3CE]:= $0304; p3:=mem[video:x]; { cteni z 3. b.rov }
    end;

  begin
	with vp do begin
	  video := $A000+(80 shr 4)*y1 ;       { zsednuti okraju }
      if fBt=eBt then
        for y:=y1 to y2 do begin
          FillP0123(fBt);
          portw[$3C4]:= $0102;             { zapis do 0. b.rov }
          mem[video:fBt]:= p0 and (fmask[fb] or emask[eb]);
		  portw[$3C4]:= $0202;             { zapis do 1. b.rov }
		  mem[video:fBt]:= p1 and (fmask[fb] or emask[eb]);
		  portw[$3C4]:= $0402;             { zapis do 2. b.rov }
          mem[video:fBt]:= p2 and (fmask[fb] or emask[eb]);

          INC(video,80 shr 4);
        end
      else begin
        for y:=y1 to y2 do begin
          FillP0123(fBt);                  { LEVY OKRAJ }
          portw[$3C4]:= $0102;             { zapis do 0. b.rov }
          mem[video:fBt]:=(p0 and fmask[fb]) ;
		  portw[$3C4]:= $0202;             { zapis do 1. b.rov }
		  mem[video:fBt]:=(p1 and fmask[fb]);
          portw[$3C4]:= $0402;             { zapis do 2. b.rov }
          mem[video:fBt]:=(p2 and fmask[fb]);
										   { PRAVY OKRAJ }
		  FillP0123(eBt);
          portw[$3C4]:= $0102;             { zapis do 0. b.rov }
          mem[video:eBt]:=(p0 and emask[eb]);
          portw[$3C4]:= $0202;             { zapis do 1. b.rov }
		  mem[video:eBt]:=(p1 and emask[eb]);
          portw[$3C4]:= $0402;             { zapis do 2. b.rov }
          mem[video:eBt]:=(p2 and emask[eb]);

          INC(video,80 shr 4);
        end;
        video := $A000 + (80 shr 4)*y1;      { zsednuti vnitrku }
        count :=eBt-fBt-1;                   { pocet bytu na radku }
        x :=fBt+1;
        for y:=y1 to y2 do begin
		  pvideo :=  Ptr(video,x);
		  portw[$3C4]:= $0102;FillChar(pvideo^,count,$00);{ zapis do 0. b.rov }
		  portw[$3C4]:= $0202;FillChar(pvideo^,count,$00);{ zapis do 1. b.rov }
          portw[$3C4]:= $0402;FillChar(pvideo^,count,$00);{ zapis do 2. b.rov }
          { zapis do treti b.rov. neni treba, obsah se nemeni }
          INC(video,80 SHR 4);
        end;
      end;
    end; {with}
	portw[$3C4]:=$0F02 ;  { povolit zapis do vsech rovin - default nast.}
    portw[$3CE]:=$0004 ;  { cteni z 0. b.roviny }
  end;  { EGAGRAY }


  procedure  HERCGray;
  var  y,x : integer;
       o   : word;   {offset}
	   b   : byte;   {data}
  begin
	with vp do
	  for y:=y1 to y2 do begin
        o:=90*(y SHR 2)+((y AND 3) SHL 13);
        if fBt=eBt then begin
          b:=mem[$B000:o+fBt];
          mem[$B000:o+fBt]:=   b and (fmask[fb] or emask[eb])  or
                            NOT(b or (fmask[fb] or emask[eb]));
        end
        else begin
		  b:=mem[$B000:o+fBt];             { levy okraj }
          mem[$B000:o+fBt]:=(b and fmask[fb]) or NOT(b or fmask[fb]);
          for x:=fBt+1 to eBt-1 do
            mem[$B000:o+x]:=NOT mem[$B000:o+x];
          b:=mem[$B000:o+eBt];             { pravy okraj }
		  mem[$B000:o+eBt]:=(b and emask[eb]) or NOT(b and emask[eb]);
		end;
      end
  end; { HERCGray }

var
  grdriv,grm: integer;
begin
  DetectGraph(grdriv,grm);
  GetViewSettings(vp);
  with vp do begin
    fb := x1 mod 8; fBt := x1 div 8 ;   { prvni bit/bajt na radku }
    eb := x2 mod 8; eBt := x2 div 8 ;   { posledni  ... }

	case grdriv of
      VGA, EGA: EGAGray;
	  HERCmono: InverzeWin {HERCGray};
	  else                           { pro ostatni adaptery pomalu,ale jiste}
                for y:=y1 to y2 do
                  for x:=x1 to x2 do
                     putpixel(x,y,1+getpixel(x,y));

    end;
  end;
end; { GrayWin }




{ IsDACRegisters
  vraci TRUE, obsahuje-li video-karta DAC registry pro nastaveni hodnot
  slozek RGB
}

function IsDACRegisters : BOOLEAN ;
var  Reg : Registers;
begin
  Reg.AX := $1A00 ;             { AH=$1A, cteni kombinace obr.systemu }
  Intr($10,Reg);
  IsDACRegisters := (
           (Reg.BL = $07)           { VGA s anal. 2.bar. zobrazovacem }
        OR (Reg.BL = $08)           { VGA s anal. bar. zobrazovacem }
        OR (Reg.BL = $0A)           { MCGA s dig. bar. zobrazovacem }
		OR (Reg.BL = $0B)           { VGA s anal. 2.bar. zobrazovacem }
		OR (Reg.BL = $0C) );        { VGA s anal. bar. zobrazovacem }
end;{IsDACRegisters}



{ zobrazovani PCX souboru **}

{var  sh       : Tshpcx;}

{ ShowPCXAt
  zobrazi soubor NameFile (typu .pcx) v danem vyrezu na obrazovce
  x1,y1 pocatek okna, x2,y2 konec okna
}
function  ShowPCXAt(NameFile:string; x1,y1,x2,y2: integer):integer;
var
  ret : integer;
begin
  SetViewPort(x1,x2,y1,y2,FALSE);
  {ClearViewPort;} {19.3.94}
				 {vymazat Viewport aby tam byly svisle cary na EGA karte}
  {SetViewPort(0,0,GetMaxX,GetMaxY,FALSE); }
  ret:=sh_Init(NameFile,x1,y1,x2-x1+1,y2-y1+1); {x2-x1+1,  tj sirka}
												{y2-y1+1,  tj vyska}
  if ret=0 then begin
	 sh_StartShow;
  with viewport do setviewport(x1,y1,x2,y2,false);
	 if not IsMode256Col then TextW;
  setviewport(0,0,GetMaxX,GetmaxY,false);
	 sh_Loop;
  end;
  ShowPCXAt:=ret;
end;


{ ShowPCX
  zobrazi soubor do okna nastaveneho v viewport
}
function  ShowPCX(NameFile: string) :integer;
var
  vp     : ViewportType;
begin
  GetViewSettings(vp);
  SetViewPort(0,0,GetMaxX,GetMaxY,FALSE);
  with vp do
	ShowPCX:=ShowPCXAt(NameFile,x1,y1,x2,y2);
end;



{------------------------------------
  Tshpcx      implementace metod
}


procedure sh_CheckWinSize(x,y,w,h : integer);
begin
  sh_Xwo:=min(max(x,0),GetMaxX-1);
  sh_Ywo:=min(max(y,0),GetMaxY-1);
  sh_Ww:=min(GetMaxX-sh_Xwo+1,w);
  sh_Hw:=min(GetMaxY-sh_Ywo+1,h);
end;


procedure sh_Check256Size(x,y,w,h : integer);
begin
  { GetMaxY/X vraci spatnou hodnotu pro rezim 320*200*256
	proto se tyto hodnoty dosadi 'natvrdo'
	reseni:
	pouzivat spravny bgi driver a nepristupovat do video pameti ve
	vlastni rezii
  }
  sh_Xwo:=min(max(x,0),319-1);
  sh_Ywo:=min(max(y,0),199-1);
  sh_Ww:=min(319-sh_Xwo+1,w);
  sh_Hw:=min(199-sh_Ywo+1,h);
end;


function sh_Init(NameFile:string;x,y,w,h : integer) : integer;
var
  i,gm : integer ;
  color : byte;
  tInit : integer;
  Palette:PaletteType;
  Head  : record
			sign,verz,encd,bPP  : byte;
			xmin,ymin,xmax,ymax : integer;
			vres,hres           : integer;
			pal  : array [0..15] of record  R,G,B : byte; end;
			vmod                : byte;
			planes              : byte;
			BPL                 : integer;
			palinfo             : integer;
			dummy : array [1..58] of byte;
  end;
  RGB256  : array [0..767] of byte;
  REGS    : Registers;
label FASTEXIT;
begin
  DetectGraph(sh_grDriver, gm);

  sh_CheckWinSize(x,y,w,h); { nastaveni rozmeru okna }
  sh_Xfo:=0;                { zobrazovani od pocatku souboru }
  sh_Yfo:=0;

  tInit:=0 ;
  assign(sh_fp,NameFile ) ;
  {$I-}
  filemode:=0;
  reset(sh_fp,1 ) ;     {$I+}
  if IOResult<>0 then begin
	sh_Init:=1 ;
    exit ;
  end;

  blockread(sh_fp,Head,sizeof(Head),i);   { cteni hlavicky 128 bytu }
  if (Head.sign<>$0A) or (i<sizeof(Head)) then begin
	 tInit:=2;
     goto FASTEXIT;
  end;
  sh_Wf:=Head.xmax-Head.xmin+1;           { rozmery obrazku, pocet sloupcu }
  sh_Hf:=Head.ymax-Head.ymin+1;           { pocet radku }

  sh_fbPP := Head.bPP;                    { bitu na pixel }
  sh_fBPL := Head.BPL;                    { bajtu na radek }
  sh_fPlanes := Head.planes;              { pocet bitovych rovin }


  if (sh_Wf<=0) OR (sh_Hf<=0) OR
     (sh_fbPP<>1) AND (sh_fbPP<>2) AND (sh_fbPP<>8) OR
     (sh_fPlanes<>1) AND (sh_fPlanes<>4)then  begin
     tInit:=2 ;
     goto FASTEXIT;
  end;

  if ( (Head.verz<>0)                  { bez inf. o bar.palete }
   AND (Head.verz<>2)                  { s inf. o bar.palete 16 }
   AND (Head.verz<>3)                  { bez inf. o bar.palete }
   AND (Head.verz<>5)) then begin      { s inf. o bar.palete 256 }
	  tInit:=2;                        { neznama verze formatu }
      goto FASTEXIT;
  end;

  if (sh_grdriver<>VGA) and (sh_fbPP=8) and (sh_fPlanes=1) then begin
	  tinit:=3;
	  goto FASTEXIT;
  end;

                                         { nastavit bar.mapu }
  if (Head.verz=2) and (sh_fPlanes=4) OR
	 (Head.verz=5) and (sh_fPlanes=4) then begin
    GetPalette(Palette);
    if (IsDACRegisters) then begin
	   for i:=0 to 15 do
         SetRGBPalette(Palette.Colors[i],Head.pal[i].R SHR 2,
										 Head.pal[i].G SHR 2,
                                         Head.pal[i].B SHR 2);
    end
    else if (sh_grdriver=VGA) or (sh_grdriver=EGA) then begin
	   for i:=0 to 15 do begin
         color := 0;                      { nastavit barvu rgbRGB }
         case (Head.pal[i].R SHR 6) of    { nastavit bity rR }
		  1 : INC(color,4);
		  2 : INC(color,32);
          3 : INC(color,32+4);
		 end;
         case (Head.pal[i].G SHR 6) of    { nastavit bity gG }
          1 : INC(color,2);
          2 : INC(color,16);
		  3 : INC(color,16+2);
         end;
		 case (Head.pal[i].G SHR 6) of    { nastavit bity bB }
          1 : INC(color);
          2 : INC(color,8);
		  3 : INC(color,8+1);
         end;
         Palette.Colors[i] := color ;
       end;
       SetAllPalette(Palette);
	end;
  end;

  sh_ScrDelay:=spec.ScreenDelay;                 {oprava MK 30.10.94}

  { 8 bitu na pixel a zaroven 1 bitova rovina => rezim 320x200,256barev}

  if (sh_fbPP=8) and (sh_fPlanes=1) then  begin
  if smallPCX then                              {doplneno 4.9.94 MK}
	begin
		tinit:=4;
		goto FASTEXIT;
	end;

	spec.ScreenDelay:=0;

	sh_bMode13:=TRUE;
	REGS.AX:=$0013;
	INTR($10,REGS);
    if Head.verz=5 then begin
       Seek(sh_fp,FileSize(sh_fp)-768);
       blockread(sh_fp,RGB256,768,i);
       for i:=0 to 767 do
         RGB256[i] := RGB256[i] shr 2;
	   REGS.AX:=$1012;
	   REGS.BX:=0;          { nastavit paletu 256 barev }
	   REGS.CX:=256;        { 256 registru }
	   REGS.ES:=SEG(RGB256);
	   REGS.DX:=OFS(RGB256);
	   INTR($10,REGS);
	end;
	sh_Check256Size(x,y,w,h);    { MS opr.19.02 - orezani obrazku}
  end
  else
    sh_bMode13:=FALSE;

  seek(sh_fp,128);
  sh_SetFBuf;
  sh_count:=0 ;

  GetMem(sh_LinesOff,sh_Hf*sizeof(longint)); { pole pocatku radku }
											 { jeden obrazovy radek }
  GetMem(sh_PL[1],sh_fBPL); fillchar(sh_PL[1]^,sh_fBPL,0);
  GetMem(sh_PL[2],sh_fBPL); fillchar(sh_PL[2]^,sh_fBPL,0);
  GetMem(sh_PL[3],sh_fBPL); fillchar(sh_PL[3]^,sh_fBPL,0);
  GetMem(sh_PL[4],sh_fBPL); fillchar(sh_PL[4]^,sh_fBPL,0);

FASTEXIT:
  sh_Init:= tInit ;
  if tInit<>0 then
	close(sh_fp);
  filemode:=2;
end;        { sh_Init }


{ StartShow
  vykresli obrazek do okna a inicializuje pole LinesOff
  obsahujici zacatky radku v souboru
}
procedure  sh_StartShow;
var
  i, linelen : integer;
begin
 linelen:=min(sh_Ww,sh_Wf);
 for i:=0 to sh_Hf-1 do begin
   TLinesOff(sh_LinesOff^)[i]:=sh_LineBeg+longint(sh_pBuf-1);
   sh_ReadLine;
   if i<sh_Hw then                   { vykreslovat v mezich okna }
	 sh_ShowLine(sh_Xwo,sh_Ywo+i,0,linelen);
 end
end;


procedure  sh_SetFbuf ;
var   howmuch : word ;
begin
  sh_LineBeg:=FilePos(sh_fp);
  blockread( sh_fp,sh_fbuf,sizeof(sh_fbuf), howmuch ) ;
  if howmuch<sizeof(sh_fbuf) then
    for howmuch:=howmuch+1 to sizeof(sh_fbuf) do
     sh_fbuf[howmuch]:=0;
  sh_pbuf:=1 ;
end;{ sh_SetFbuf }


function  sh_getbyte : byte ;
var    b : byte ;
begin
 if sh_count>0 then begin
   sh_getbyte := sh_value ;
   DEC(sh_count);
 end
 else begin
   if sh_pbuf>sizeof(sh_fbuf) then sh_SetFbuf ;
   b := sh_fbuf[sh_pbuf] ;
   INC(sh_pbuf) ;
   if ((b AND $C0) = $C0) then begin
     sh_count := b AND (NOT $C0) ;
     if sh_pbuf>sizeof(sh_fbuf) then sh_SetFbuf ;
	 sh_value := sh_fbuf[sh_pbuf] ;
     INC(sh_pbuf);
	 sh_getbyte := sh_value;
     DEC(sh_count);
   end
   else
     sh_getbyte := b ;
 end;
end;          { sh_getbyte }



procedure  sh_ReadLine;
var  p,i : integer;
begin
  for p:=1 to sh_fPlanes do
	for i:=0 to sh_fBPL-1 do
       TL(sh_PL[p]^)[i]:=sh_getbyte;
  sh_actPlane:=1;
end;


procedure  sh_ReadLineNo(line:integer);
begin
  seek(sh_fp,TLinesOff(sh_LinesOff^)[line]);
  sh_SetFBuf;
  sh_ReadLine;
end;



procedure   sh_ShowLine(Xscreen,Yscreen,Xfile,w:integer);
var x     : integer;
    XX    : integer;
	pBPL  : integer;
    maskW     : word ;
    maskR     : word ;
    p,o,video : word;
    b     : byte;
    Pvideo: ^Byte;
begin

 sh_fb   := Xscreen mod 8 ; sh_fBt := Xscreen div 8 ;
 sh_eb   := (Xscreen+w-1) mod 8 ; sh_eBt := (Xscreen+w-1) div 8 ;
 {19.3.94 w je siraka, ale protoze se pricita k x-souradnici }
		  {je treba odecist 1}
 pBPL := sh_eBt-sh_fBt ;
 sh_PtrE := 8-sh_fb ;

 case sh_grDriver of

	EgaMono,VGA,EGA : begin

	 if sh_bMode13 then begin              { rezim 320x200,256 barev }
	   Pvideo:=Ptr($A000+(320 shr 4)*Yscreen,Xscreen);
	   Move(TL(sh_PL[1]^)[Xfile],Pvideo^,w);
(*         Move(sh_PL[1,Xfile],Pvideo^,w);*)
     end

     else  begin                        { 4 bitove roviny }
       video:=$A000+(80 shr 4)*Yscreen;
       if (sh_fb=0)and(sh_eb=7) then begin    { rychla verze bez bitovych pos.}
         XX:=sh_eBt-sh_fBt+1;
         maskW := $0100;
         for p:=1 to 4 do begin
           portw[$3C4]:= $0002 OR maskW ;
		   maskW := maskW shl 1 ;
		   Pvideo:=Ptr(video,sh_fBt);
		   if (sh_fPlanes=4) then
			 MOVE(TL(sh_PL[p]^)[Xfile div 8],Pvideo^,XX)
             (*MOVE(sh_PL[p,Xfile div 8],Pvideo^,XX) *)
           else
             MOVE(TL(sh_PL[1]^)[Xfile div 8],Pvideo^,XX);{ zkopirovat 1.b.rovinu do vsech ostatnich }
         end;
       end
       else begin                       { treba provadet bitove posuny }
		 XX:=sh_eBt-1;
         maskW := $0100 ; maskR := $0000 ;
         for p:=1 to 4 do begin
           if (sh_fPlanes=4) then
             sh_actPlane:=p
           else
			 sh_actPlane:=1;             { zkopirovat 1.b.rovinu do vsech ostatnich }
           portw[$3C4]:= $0002 OR maskW ;
		   portw[$3CE]:= $0004 OR maskR ;
           maskW := maskW shl 1 ;
		   INC(maskR,$0100);
		   sh_InitBuf(Xfile div 8);
           if pBPL=0 then begin
			 b:=mem[video:sh_fBt];
             mem[video:sh_fBt]:= b and NOT(fbits[sh_fb] AND ebits[sh_eb])  OR
                                 sh_outbyte AND (fbits[sh_fb] AND ebits[sh_eb]);
           end
           else begin
			 b:=mem[video:sh_fBt];            { levy okraj }
             mem[video:sh_fBt]:=b and NOT fbits[sh_fb] or sh_outbyte and fbits[sh_fb];
             for x:=sh_fBt+1 to XX do
               mem[video:x]:=sh_outbyte ;
			 b:=mem[video:sh_eBt];            { pravy okraj }
             mem[video:sh_eBt]:=b and NOT ebits[sh_eb] or sh_outbyte and ebits[sh_eb];
		   end;
         end;
         portw[$3C4]:=$0F02 ; portw[$3CE]:=$0004 ;
       end;
     end;
  end;
  HERCmono: begin

     if (sh_fb=0)and(sh_eb=7) then begin
	   Pvideo:=Ptr($B000,90*(Yscreen SHR 2)+((Yscreen AND 3) SHL 13)+sh_fBt);
       MOVE(TL(sh_PL[1]^)[Xfile div 8],Pvideo^,sh_eBt-sh_fBt+1);
     end
	 else  begin                            { verze s bitovymi posuny }
       XX:=sh_eBt-1;
	   sh_InitBuf(Xfile div 8);
       o:=(90*(Yscreen SHR 2)+((Yscreen AND 3) SHL 13));
	   if pBPL=0 then begin
         b:=mem[$B000:o+sh_fBt];
         mem[$B000:o+sh_fBt]:= b and NOT(fbits[sh_fb] AND ebits[sh_eb])  OR
                             sh_outbyte AND (fbits[sh_fb] AND ebits[sh_eb]) ;
       end
       else begin
         b:=mem[$B000:o+sh_fBt];            { levy okraj }
         mem[$B000:o+sh_fBt]:=b and NOT fbits[sh_fb] or sh_outbyte and fbits[sh_fb];
		 for x:=sh_fBt+1 to XX do
		   mem[$B000:o+x]:=sh_outbyte ;
         b:=mem[$B000:o+sh_eBt];            { pravy okraj }
         mem[$B000:o+sh_eBt]:=b and NOT ebits[sh_eb] or sh_outbyte and ebits[sh_eb];
       end;
	 end;
  end;
 end;
end;   { sh_Show }



(*
procedure  sh_ReadHCGLine;
var  p,i : integer;
begin
  for i:=0 to fBPL-1 do
   PL[1,i]:=0;
  for p:=1 to fPlanes do
	for i:=0 to fBPL-1 do
	  PL[1,i]:=PL[1,i] OR sh_getbyte;
  actPlane:=1;
end;
*)


procedure  sh_InitBuf(Xfile : integer);
begin
  sh_buf := ($0000 or TL(sh_PL[sh_actPlane]^)[Xfile]) shl sh_PtrE;          { e.g.(8-fb) }
  sh_actByte:=Xfile+1;
end;	  { sh_InitBuf }


function  sh_outbyte : byte ;
begin
  sh_outbyte := HI( sh_buf ) ;
  sh_buf := ((sh_buf shl sh_fb) or TL(sh_PL[sh_actPlane]^)[sh_actByte]) shl sh_PtrE ;
  INC(sh_actByte);
end;   { sh_outbyte }


{ ReadShow
  precte obrazek ze souboru z pozice X,Y (radek) o rozmerech
  w,h, a zobrazi na obrazovku od pozice XS,YS ve stejne velikosti
}

procedure sh_ReadShow(x,y,w,h,xs,ys : integer);
var
 i : integer;
begin
 if (xs<sh_Xwo) OR (ys<sh_Ywo) OR
    (xs+w>sh_Xwo+sh_Ww) OR (ys+h>sh_Ywo+sh_Hw) then begin
   beep;
   exit;
 end;
 h:=min(h-1,min(sh_Hf,sh_Hw));
 w:=min(w,sh_Wf);

 sh_ReadLineNo(y);           { radky v souboru se cisluji od 0 }
 sh_ShowLine(xs,ys,x,w);
 for i:=1 to h do begin
    sh_ReadLine;
    sh_ShowLine(xs,ys+i,x,w);
 end;
end;



{ Loop
  smycka pro ovladani zobrazovani obrazku
  ESC,ENTER ukonci zobr.
}
procedure  sh_Loop;
var
  st    : integer;
  tmp:longint;
  a     : integer;  { korekce pri horizontalnim posunu }
begin
  if (not spec.AutoRprtPrint) and (not ExitPCX) then
  repeat
	key:=readkbd;
	case key of
	   _down_: begin
				  st := min(St_Up,max(0,sh_Hf-(sh_Yfo+sh_Hw)));
				  if st>0 then begin
					MovePict(sh_Xwo,sh_Ywo+st,sh_Ww,sh_Hw-st,sh_Xwo,sh_Ywo);
					sh_ReadShow(sh_Xfo,sh_Yfo+sh_Hw,sh_Ww,st,{ ze souboru }
							 sh_Xwo,sh_Ywo+sh_Hw-st);    { na obrazovku }
					INC(sh_Yfo,st);
				  end
				  else
					beep;
				end;
	   _UP_ :   begin
				  st := min(St_Down,sh_Yfo);
				  if st>0 then begin
					MovePict(sh_Xwo,sh_Ywo,sh_Ww,sh_Hw-st,sh_Xwo,sh_Ywo+st);
					sh_ReadShow(sh_Xfo,sh_Yfo-st,sh_Ww,st, { ze souboru }
							 sh_Xwo,sh_Ywo);         { na obrazovku }
					DEC(sh_Yfo,st);
				  end
				  else
					beep;
				end;
	   _PgDn_  :begin
				  st:=max(sh_Hf-(sh_Yfo+sh_Hw),0);
				  if st>=sh_Hw then begin     { cele prekreslit }
{					sh_Yfo:=max(0,sh_Hf-sh_Hw);}
					sh_Yfo:=max(0,sh_Yfo+sh_Hw);
					sh_ReadShow(sh_Xfo,sh_Yfo,sh_Ww,sh_Hw, sh_Xwo,sh_Ywo);
				  end
				  else
					if st>0 then begin     { posun+prekreslit }
					   MovePict(sh_Xwo,sh_Ywo+st,sh_Ww,sh_Hw-st,sh_Xwo,sh_Ywo);
					   sh_ReadShow(sh_Xfo,sh_Yfo+sh_Hw,sh_Ww,st,{ ze souboru }
								sh_Xwo,sh_Ywo+sh_Hw-st);    { na obrazovku }
					   INC(sh_Yfo,st);
					end
					else beep;
				  end;
		  _PGUP_: begin
				  st:=sh_Yfo;
				  if st>=sh_Hw then           { prekreslit cele }
				   begin
					 sh_ReadShow(sh_Xfo,sh_Yfo-sh_Hw,sh_Ww,sh_Hw, sh_Xwo,sh_Ywo);
					 DEC(sh_Yfo,sh_Hw);
				   end
				  else
					if st>0 then begin
					  MovePict(sh_Xwo,sh_Ywo,sh_Ww,sh_Hw-st,sh_Xwo,sh_Ywo+st);
					  sh_ReadShow(sh_Xfo,sh_Yfo-st,sh_Ww,st,   sh_Xwo,sh_Ywo);
					  sh_Yfo:=0;
					end else beep;
				  end;
				  {*** HORIZONTALNI POSUNY }
				  { za souboru lze zobrazovat data zacinajici pouze
					na rozhrani bajtu => omezeni : pravy okraj
					muze byt orezan tak, aby se v okne zacinaly
					zobrazovat data z hranice bajtu (tj. ztrata 0-7
					bodu na pravem okraji souboru
				  }
	  _right_: begin
				  st := min(St_Left,max(0,sh_Wf-(sh_Xfo+sh_Ww)));
				  st := st-(st mod 8);
				  if st>0 then begin
					MovePict(sh_Xwo+st,sh_Ywo,sh_Ww-st,sh_Hw,sh_Xwo,sh_Ywo);
					a:=(sh_Xfo+sh_Ww) mod 8;
					sh_ReadShow(sh_Xfo+sh_Ww-a,sh_Yfo,st+a,sh_Hw,  sh_Xwo+sh_Ww-st-a,sh_Ywo);
					INC(sh_Xfo,st);
				  end
				  else
					beep;
				  end;
	   _left_: begin
				  st := min(St_Right,sh_Xfo);
				  if st>0 then begin
					MovePict(sh_Xwo,sh_Ywo,sh_Ww-st,sh_Hw,sh_Xwo+st,sh_Ywo);
					DEC(sh_Xfo,st);
					sh_ReadShow(sh_Xfo,sh_Yfo,st,sh_Hw,      sh_Xwo,sh_Ywo);
				  end
				  else
					beep;
				  end;
  _Ctrlright_: begin
				  st := min(sh_Ww,max(0,sh_Wf-(sh_Xfo+sh_Ww)));
				  st := st-(st mod 8);
				  if st>0 then begin
					MovePict(sh_Xwo+st,sh_Ywo,sh_Ww-st,sh_Hw,sh_Xwo,sh_Ywo);
					a:=(sh_Xfo+sh_Ww) mod 8;
					sh_ReadShow(sh_Xfo+sh_Ww-a,sh_Yfo,st+a,sh_Hw,  sh_Xwo+sh_Ww-st-a,sh_Ywo);
					INC(sh_Xfo,st);
				  end
				  else beep;
			   end;
  _Ctrlleft_: begin
				  st := min(sh_Ww,sh_Xfo);
				  st := st-(st mod 8);
				  if st>0 then begin

					MovePict(sh_Xwo,sh_Ywo,sh_Ww-st,sh_Hw,sh_Xwo+st,sh_Ywo);
					DEC(sh_Xfo,st);
					sh_ReadShow(sh_Xfo,sh_Yfo,st,sh_Hw,      sh_Xwo,sh_Ywo);
				  end
				  else beep;
			  end;
	   _end_:    begin
				  st:=max(sh_Wf-(sh_Xfo+sh_Ww),0);
				  if st>=sh_Ww then begin     { cele prekreslit }
					sh_Xfo:=max(0,sh_Wf-sh_Ww);
					sh_Xfo:=sh_Xfo-(sh_Xfo mod 8);  { cist lze pouze z hranice bajtu}
					sh_ReadShow(sh_Xfo,sh_Yfo,sh_Ww,sh_Hw, sh_Xwo,sh_Ywo);
				  end
				  else begin
					st:=st-(st mod 8);
					if st>0 then begin     { posun+prekreslit }

					   MovePict(sh_Xwo+st,sh_Ywo,sh_Ww-st,sh_Hw,sh_Xwo,sh_Ywo);
					   a:=(sh_Xfo+sh_Ww) mod 8;
					   sh_ReadShow(sh_Xfo+sh_Ww-a,sh_Yfo,st+a,sh_Hw, sh_Xwo+sh_Ww-st-a,sh_Ywo);
					   INC(sh_Xfo,st);
					end
					else beep;
				  end
				  end;
	   _home_:   begin

				  st:=sh_Xfo;
				  if st>=sh_Ww then           { prekreslit cele }
					 sh_ReadShow(0,sh_Yfo,sh_Ww,sh_Hw, sh_Xwo,sh_Ywo)
				  else
					if st>0 then begin
					  MovePict(sh_Xwo,sh_Ywo,sh_Ww-st,sh_Hw,sh_Xwo+st,sh_Ywo);
					  sh_ReadShow(sh_Xfo-st,sh_Yfo,st,sh_Hw,   sh_Xwo,sh_Ywo);
					end else beep;
				  sh_Xfo:=0;
				  end;
(*     dodelat volani gr. editoru
			 _F5_:begin if MausVisibleGr then HideMouse else ShowMouse;
						MausVisibleGr:=not MausVisibleGr;
				  end;   *)
	end;
   until (key=_F6_)or			{ Tisk }
		 (key=_ShiftF6_)or			{ Tisk okna }
		 (key=_F9_)or 		        { Zapis *.PCX }
		 (key=_ShiftF9_)or 		        { Zapis *.PCX okna }
		 (key=_F4_)or                       { Inverze }
		 (key=_ShiftF4_)or                    { Inverze okna }
	 (key=_ESC_);                  { Esc }

  spec.ScreenDelay:=sh_ScrDelay;
  FreeMem(sh_PL[4],sh_fBPL);
  FreeMem(sh_PL[3],sh_fBPL);
  FreeMem(sh_PL[2],sh_fBPL);
  FreeMem(sh_PL[1],sh_fBPL);
  FreeMem(sh_LinesOff,sh_Hf*sizeof(longint));
  Close(sh_fp);
  filemode:=2;
end;  {Loop}



{  SavePCX
   ulozi obsah obrazovky do souboru
}
function  SavePCX( NameFile : string):integer;
type
    Thead = record
	   sign,verz,encd,bPP  : byte;
       xmin,ymin,xmax,ymax : integer;
       hres,vres           : integer;
       pal  : array [0..15] of record  R,G,B : byte; end;
       vmod                : byte;
       planes              : byte;
       BPL                 : integer;
       palinfo             : integer;
       dummy : array [1..58] of byte;
    end;

var
	grDriver,gm : integer;
	fp :  file;
	Old,New : byte ;       { promenne pro kompresi graf. dat }
	Count   : byte ;
	bSTOP   : boolean ;    { TRUE jestlize jiz byl predan posledni byte }
	video   : word ;
	Palette : PaletteType;
	Reg :     Registers;
	ColRed,
	ColGreen,
	ColBlue : byte;

	buf      : array [0..2047] of byte ;
	Head     : Thead absolute  buf ;
	bufCount : word ;
	actLine,
	actPlane,              { ukazovatka do pole PL (jeden radek obrazovky) }
	actByte  : integer;
	planes   : integer;         { pocet bit.rovin }
	PL       : array [1..4,1..320] of byte;
	vp       : Viewporttype;
	fBt,eBt  : integer;
	fb,PtrE  : integer;         { prvni,posledni bit }
	ShBuf    : word;            { buffer pro posun }
	YY,res,i : integer;
	p        : pointer;
	SizeIm   : word;
	PocVolaniOutByte,           { pro nasilne preruseni kodovani }
	DelkaRadku :integer;        { na konci radku }


 procedure ReadLine;
 var p,x    : integer;
	 pvideo : ^byte;
 begin
   if grDriver=HERCmono then begin
	 pvideo:=Ptr($B000,90*(actLine SHR 2)+((actLine AND 3) SHL 13));
	 move(pvideo^,PL[1,1],90);
   end
   else begin
	 pvideo:=Ptr(video,0);
	 for p:=1 to 4 do begin
	   portw[$3CE]:= $0004 OR ((p-1) shl 8);
	   move(pvideo^,PL[p,1],80);
	 end;
	 INC(video,80 shr 4);
   end;
 end;

 function  getbyte : byte;
 begin
   getbyte := PL[actPlane,actByte];
   INC(actByte);
   if actByte>eBt then begin                    { dalsi bajt na radku }
	 actByte:=fBt;
	 INC(actPlane);
	 if actPlane>planes then begin              { dalsi bitova rovina }
	   actPlane:=1;
	   INC(actLine);
	   if actLine>YY then                       { dalsi radek }
		 bSTOP:=TRUE                            { konec prenosu }
	   else
		 ReadLine;
	 end;
   end;
 end;


 procedure InitShBuf;
 begin
   PocVolaniOutByte:=0;
   DelkaRadku:=Head.BPL*Head.planes;
   ShBuf:=getbyte shl 8;
 end;


 function  outbyte : byte;
 begin
   ShBuf:=(ShBuf or ($00ff and getbyte)) shl fb;
   outbyte:=HI(ShBuf);
   INC(PocVolaniOutByte);
   ShBuf:=ShBuf shl PtrE;   { e.g. 8-fb }
 end;



 procedure put(b : byte);
 begin
  if bufCount = sizeof(buf) then begin
	blockwrite( fp,buf,1) ;
	bufCount := 0 ;
  end ;
  buf[ bufCount ] := b ;
  INC(bufCount);
 end;  { put }


label FASTEXIT;
begin  { SavePCX  }
  fillchar(buf,sizeof(buf),0);
  DetectGraph(grDriver,gm);

  if IsMode256Col then
	exit;

  {$I-}
  assign(fp,NameFile) ;
  rewrite(fp,sizeof(buf)) ;  {$I+}
  if IOResult<>0 then begin
	 beep ;
	 SavePCX:=1;
	 exit ;
  end;

  GetViewSettings(vp);

  SizeIm:=ImageSize(0,GetMaxY-30,76,GetMaxY);
  SetViewPort(0,0,GetmaxX,GetMaxY,true);
  p:=GetStore(SizeIm);
  GetImage(0,GetMaxY-30,76,GetMaxY,p^);
  SetWindow(ViewPort);
  RunMsgOn('C',vp.y2);

  Head.sign := $0A ; 	  	{ format PCX }
  if (grDriver<>EGAMono) and (grDriver<>HercMono) then
	Head.verz := $02 	  	{ verze   s barevnou paletou }
  else
	Head.verz := $00; 	  	{ verze  bez barevne palety  }
  Head.encd := $01 ;  		{ kodovani  run length }
  Head.bPP := 1;
  with vp do begin
	if (x1<0)or(x2<0)or(y1<0)or(y2<0)or(x2>GetMaxX)or(y2>GetMaxY) then begin
	   res:=2;
	   goto FASTEXIT;
	end;
	fb := x1 mod 8; PtrE := 8-fb;
	fBt:= x1 div 8+1;
	eBt:= x2 div 8+1;
	Head.xmin := x1;           { levy horni roh X }
	Head.ymin := y1;           { levy horni roh Y }
	Head.xmax := x2;           { pravy dolni roh X }
	Head.ymax := y2;           { pravy dolni roh Y }
	{ DEBUG
	Setviewport(0,0,GetMaxX,GetMaxY,TRUE);
	rectangle(x1,y1,x2,y2);
	Setviewport(x1,y1,x2,y2,TRUE);
	}
  end;
  Head.hres := GetMaxX+1 ;
  Head.vres := GetMaxY+1 ;            { vertikalni rozlisitelnost }
  if (grdriver<>HERCmono) and (grdriver<>EgaMono) then
	Head.planes := 4     	{ pocet bitovych rovin }
  else
	Head.planes := 1;
  planes:=Head.planes;
  Head.BPL    := eBt-fBt+1;     { pocet bajtu na radek }
  Head.palinfo:= 1 ;            { barvy interpretovat jako barvy }

  GetPalette(Palette);
  for i:=0 to 15 do begin
	if (IsDACRegisters) then begin
	  Reg.AX := $1015 ;           { AH=$10, AL=$15 cteni registru DAC }
	  Reg.BX := Palette.Colors[i] ;           { cislo registru 0..255 }
	  Intr($10,Reg);
	  ColRed := Reg.DH SHL 2;
	  ColGreen := Reg.CH SHL 2;
	  ColBlue := Reg.CL SHL 2;
	  end
	else
	  with Palette do begin
		ColRed := 0; ColGreen := 0; ColBlue := 0;
		if (Colors[i] AND 4  = 4)  then ColRed := 65;
		if (Colors[i] AND 32 = 32) then INC(ColRed,129);
		if (Colors[i] AND 2  = 2)  then ColGreen := 65;
		if (Colors[i] AND 16 = 16) then INC(ColGreen,129);
		if (Colors[i] AND 1  = 1)  then ColBlue := 65;
		if (Colors[i] AND 8  = 8)  then INC(ColBlue,129);
	end;
	Head.pal[i].R := ColRed ;
	Head.pal[i].G := ColGreen ;
	Head.pal[i].B := ColBlue ;
  end;

  bufCount := 128 ;
						{ prenos + komprese grafickych dat }
  bSTOP := false ;              { pro fce GetNextByte	  }

  actLine:=vp.y1;
  if grdriver<>HERCmono then    { e.g. VGA or EGA }
	   video:=$A000+(80 shr 4)*actLine;

  ReadLine;
  actPlane:=1;
  actByte:=fBt;

  YY:=vp.y2;

  InitShBuf;
  Old:=outbyte;
  Count:=1 ;
  repeat
	New:=outbyte;
	if (Old = New) AND (PocVolaniOutByte<=DelkaRadku)
				   AND NOT bSTOP
				   then  begin
	  INC(Count);
	  if Count=64 then begin
		put(63 OR $C0); put(Old);    {63 -> uz nezapisovat New}
		Count:=1;
	  end;
	end
	else begin
	  if (Count = 1) AND  ((Old AND $C0)<>$C0)  then
		put( Old )
	  else  begin
		put( Count OR $C0 ); put( Old );
		Count := 1;
	  end
	end;
	if (PocVolaniOutByte>DelkaRadku) then {nasilne preruseni kodovani}
		PocVolaniOutByte:=1;              {na rozhrani radku}
	if actLine <YY-30 then              
	  RunMsgN(actLine);                   {zobrazeni % }
	if actline =YY-30 then begin
	  RunMsgN(YY);RunMsgOff;RunMsgClear;
	  SetViewPort(0,0,GetmaxX,GetMaxY,true);
	  PutImage(0,GetMaxY-30,p^,NormalPut);ReleaseStore(p);
	  SetWindow(ViewPort);
	end;
	Old := New;
  until  bSTOP;
  put( 1 OR $C0 ); put( New );          {vypsat New}
  put( 1 OR $C0 ); put( outbyte );      {vyprazdnit buffer}

  for i:=bufcount to sizeof(buf)-1 do   { vymazat nevyuzity zbytek }
	buf[i]:=0;
  blockwrite(fp,buf,1);

  if (grdriver<>HERCmono) then
	portw[$3CE]:=$0004;                 { default settings EGA registers}

  res:=0;
 FASTEXIT:
  SavePCX:=res;
  close(fp);
  filemode:=2;
  if res<>0 then erase(fp);
  {nosound;}
  beep;beep;

end; {SavePCX}

end.