unit GrMenu;       { cteni parametru z menu }

{$A-,B-,D-,F+,G-,I+,L-,O+,R-,S+,V+,X+}

interface
  uses GrGlob,RDRun,GrMenu1,WWmenu,WWmix,RunEdi;

  procedure MenuPar(var Param:Parametry;var Par3:Param3;Poprve:boolean;var Maxim:Maxima;GD:GraphDPtr);
  procedure Menu1(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure Menu2(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure Menu3(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure Menu4(var Param:Parametry;var Par3:Param3);
  procedure Menu5(var Param:Parametry;var Par3:Param3);
  procedure Menu6(var Param:Parametry;var Par3:Param3);
  procedure SetTxtPar1(NPar:byte;Txt:str30);
  procedure SetTxtPar2(NPar:byte;Txt:str30);
  procedure WritePar1(var Param:Parametry;var Par3:Param3);
  procedure WritePar2(var Param:Parametry;var Par3:Param3; Mask:boolean);
  function Menu(MsgNr,IStart:word):word;

implementation
  uses dos,drivers,graph,access,base,obaseww,grinit,compile;

  var
	TxtPar1,TxtPar2:string[74];
    ViewPar2:boolean;
    pomret:str15;

procedure SetTxtPar1(NPar:byte;Txt:str30); {zasune do spravneho mista text parametru}
  var
    i:byte;
    p:array [1..5] of str30;
  begin
    for i:=1 to 5 do
      begin
        p[i]:=copy(TxtPar1,1,pos(',',TxtPar1)-1);
		TxtPar1:=copy(TxtPar1,pos(',',TxtPar1)+1,80);
      end;
	p[NPar]:=Txt;
    TxtPar1:=p[1]+','+p[2]+','+p[3]+','+p[4]+','+p[5]+','+
	 '                                                                          ';
  end;

procedure SetTxtPar2(NPar:byte;Txt:str30); {zasune do spravneho mista text parametru}
  var
    i:byte;
    p:array [1..5] of str30;
  begin
    for i:=1 to 5 do
      begin
        p[i]:=copy(TxtPar2,1,pos(',',TxtPar2)-1);
        TxtPar2:=copy(TxtPar2,pos(',',TxtPar2)+1,80);
      end;
    p[NPar]:=Txt;
	TxtPar2:=p[1]+','+p[2]+','+p[3]+','+p[4]+','+p[5]+','+
     '                                                                          ';
  end;

procedure WritePar1(var Param:Parametry;var Par3:Param3);
type str1=string[1];
var ps:str1;
  begin
	with Param,Par3 do
      begin
        textcolor(colors.mHili);
        textbackground(1);
        pomret:=Typ;
        if Typ='3D' then pomret:='''2DBAR''';
        if Typ='L3' then pomret:='''APPROX''';
        if Typ='LP3' then pomret:='''POLYREG''';
        if Typ='2D' then pomret:='''2DBAR''';
        if Typ='DS' then pomret:='''GROUP''';
		if Typ='DL' then pomret:='''GROUPLINE''';
        if Typ='SB' then pomret:='''STACKBAR''';
        if Typ='QS' then pomret:='''3DBAR''';
        if Typ='QL' then pomret:='''3DLINE''';
		if Typ='Lin' then pomret:='''APPROX''';
        if Typ='LP' then pomret:='''POLYREG''';
		if Typ='Cir' then pomret:='''CIRCLE''';
        SetTxtPar1(1,'TYPE='+pomret);
        SetTxtPar1(2,'FILL='''+Color+'''');
        SetTxtPar1(3,'DIRX='''+PopX+'''');
		str(Mask2:1,ps);
        if Mask2=0 then SetTxtPar1(4,'GRID='''+Rastr+'''')
                   else SetTxtPar1(4,'GRID='''+Rastr+':'+ps+'''');
        SetTxtPar1(5,'PRINT='''+Print+'''');

		window(1,1,TxtCols,TxtRows-1);
		gotoxy(4,TxtRows-3);
        write(TxtPar1);
	  end;
  end;

procedure WritePar2(var Param:Parametry;var Par3:Param3; Mask:boolean);
var i:byte;
  begin
    if ViewPar2 or Mask then
	 with Param,Par3 do
      begin
        SetTxtPar2(1,'FGP='+FileParNaz);
        SetTxtPar2(2,'PALETTE='''+Barvy+'''');
        str(Pomer:3,pomret);
		for i:=1 to 3 do if pomret[1]=' ' then pomret:=copy(pomret,2,3);
        SetTxtPar2(3,'WIDTH='+pomret);
        str(StartVeta:9,pomret);
        for i:=1 to 9 do if pomret[1]=' ' then pomret:=copy(pomret,2,9);
        SetTxtPar2(4,'RECNO='+pomret);
        str(PocVet:2,pomret);
		for i:=1 to 2 do if pomret[1]=' ' then pomret:=copy(pomret,2,2);
        SetTxtPar2(5,'NRECS='+pomret);

        textcolor(8);textbackground(7);
        gotoxy(5,TxtRows-2);
        write(TxtPar2);
		textcolor(colors.mHili);
        textbackground(1);
	  end;
    ViewPar2:=true;
  end;

function Menu(MsgNr,IStart:word):word;
var w:PMenuBoxS;
begin
  RdMsg(MsgNr); New(w,Init(4,3,StringPtr(@MsgLine)));
  Menu:=w^.Exec(IStart); w^.Done; ReleaseStore(w);
end;

procedure ColNorm;
begin
  textcolor(colors.dNorm);textbackground(colors.dNorm div 16);
end;

procedure ColHiLi;
begin
  textcolor(colors.dHiLi);textbackground(colors.dHiLi div 16);
end;

{---------------------------------------------------------------------}
procedure MenuPar(var Param:Parametry;var Par3:Param3;Poprve:boolean;var Maxim:Maxima;GD:GraphDPtr);

label 333;

var r:str1;
    ano:boolean;
	PrgPtr:RdbDPtr;

function promptb(Pis:str80):boolean;
var
   key:word;
   BegX,BegY,i:integer;
 begin
	ColNorm;
	BegX:=wherex;
    BegY:=wherey;
	write(Pis);
    ColHiLi;
    repeat
      key:=readkbd;
    until (key=_M_)or(key=_ESC_)or(char(key) in ['A','a','n','N']);
    if (key=_M_)or(upcase(char(key))='A') then promptb:=true else promptb:=false;
    ColNorm;
	Pis:=Pis+' ';
	for i:=1 to length(Pis) do
    Pis[i]:=' ';
    gotoxy(BegX,BegY);write(Pis);
  end;


label
  95,97,98,99,61,62,63,64,65,66;

var
  par,naz:ComStr;
  i:integer;
  volba,volbaPosl:word;
  pom:ExtStr;
  mb:PMenuBoxS;
  p:pointer; s:StringPtr; sofs:word absolute s;
  W:PMenuBoxS;
  TM:PMenuBoxS;

begin
  with Param,Par3 do
	begin
	  Volba:=MenuInit;
	  GrExit:=false;
	  if not Poprve then goto 95;
	  Interact:=true;
	  Volba:=1;
	  if PopX='' then PopX:='H';
	  if length(Color)<2 then
        if typ[1]='Q' then Color:=Color+'3' else Color:=Color+'1';
	  if UdajX='' then
        begin
          Typ:='2D';
		  if (GrDriv=1)or(GrDriv=2)or(GrDriv=5)or(GrDriv=7)
           then Color:='F3'
            else Color:='C3';
          Print:='DH200Y';
          if not AutoGr then FLLoc:=nil;
          if AutoGr and (FLLoc<>nil) then NazevF:=Cfile^.Name else NazevF:=''; {ver3.2}
          if AutoGr and (FLLoc<>nil) then UdajX:=FLLoc^.FldD^.Name else UdajX:='';
		  UdajZ:='';
          Hlavicka:='';
		  NazevX:='';
		  NazevY:='';
		  NazevZ:='';
          Rastr:='N';
		  PopX:='H';
		  Barvy:='';
          FileParNaz:='';
		  FilePCX:='';
		  StartVeta:=1;
          PocVet:=max;
          Txtpar1:='TYPE=''2DBAR'',FILL=''C2'',DIRX=''H'',GRID=''N'',PRINT=''DH200Y'',                        ';
          Txtpar2:='FGP=GRAPH1.FGP,PALETTE=''BYrgmOGRbMCaAc'',WIDTH=50,RECNO=1,NRECS=64,            ';
          ViewPar2:=false;
          FLLoc1:=FLLoc;
          NazevFStart:=NazevF;
          for i:=0 to 9 do
            begin
              if FLLoc<>nil then FLLoc:=FLLoc^.Chain;
			  if AutoGr and (FLLoc<>nil) then
                Udaj3[i]:=FLLoc^.FldD^.Name else Udaj3[i]:='';
			  if AutoGr and (FLLoc<>nil) then
                Nazev3[i]:=FLLoc^.FldD^.Name else Nazev3[i]:='';
            end;
		  if AutoGr then
			begin
			  if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
				  begin
					UdajY:=Udaj3[0];
                    UdajZ:=Udaj3[1];
                    UdajYPtr:=Udaj3Ptr[0];
                    UdajZPtr:=Udaj3Ptr[1];
                  end;
              if Hlavicka='' then Hlavicka:=NazevF;
              if NazevX='' then NazevX:=UdajX;
              if NazevY='' then NazevY:=UdajY;
              if NazevZ='' then NazevZ:=UdajZ;
			  for i:=0 to 9 do if Nazev3[i]='' then Nazev3[i]:=Udaj3[i];
            end;
		  Pomer:=100;
		  Border:='B';
          HeadFrame:='';
          WWShadow:=false;
		  MakeCls:=true;
		  StupPoly:=0;StupPolyP:=0;
          MinimumVstP:=0;
		  Maxim.MinimumVst:=0;
          Maxim.MaximumVst:=0;
          MinimumC:=0;
          for i:=0 to 15 do begin RGB[i].R:=-1;RGB[i].G:=0;RGB[i].B:=0;end; {neprovede se SetRGBPalette}

		with ViewPort do
      begin
		with ViewPort80x25 do
           begin x1:=1;y1:=1;x2:=80;y2:=25;Clip:=ClipOn;end;
		ColorWWFrame:=BarvyPasDef[Colors.uNorm];
        ColorWWBack:='L';
        ColorWWFor:='w';
        MakeCls:=true;
		RectToPix(ViewPort80x25.x1,ViewPort80x25.y1,
				  ViewPort80x25.x2,ViewPort80x25.y2,x1,y1,x2,y2);
		SetViewPort(x1,y1,x2,y2,ClipOn);
	  end;

        end;
	  Poprve:=false;
95:
{      HideMouse;}
 	  CrsHide;
	  TextAttr:=colors.DeskTopColor;
	  window(1,1,TxtCols,TxtRows);ClrScr;
	  window(1,1,TxtCols,TxtRows-1);
	  WriteWFrame(WHasFrame+WDoubleFrame,'','');
	  ScrClr(1,1,TxtCols-2,TxtRows-13,#$b1,TextAttr);
      ScrClr(1,TxtRows-12,TxtCols-2,10,#$b2,TextAttr);
      MarkStore(p){mk95};
      ResFile.Get(FandFace,p); s:=p; inc(sofs);
      ReleaseStore(p){mk95};
      for i:=-11 to -6 do begin
		s^[0]:=#78; ScrWrStr(1,TxtRows+i,s^,TextAttr); inc(sofs,82) end;
	  TextAttr:=colors.mHiLi; ScrClr(3,TxtRows-4,TxtCols-6,1,' ',TextAttr);
	  TextAttr:=colors.DeskTopColor;
      RdMsg(220);

	  New(TM,Init(4,3,StringPtr(@MsgLine)));

99:
	  WritePar1(Param,Par3);
      if ViewPar2 then WritePar2(Param,Par3,false);

{******** HLAVNI MENU }
{ShowMouse;
ShowMouse;}
volbaPosl:=volba;
RdMsg(220);
{volba:=Menu(220,VolbaPosl);}
volba:=TM^.Exec(VolbaPosl);

      case volba of
        0 : begin
             ColNorm;
			 if Spec.ESCverify then
			 begin
              window(1,1,TxtCols,TxtRows);
			  gotoxy(2,TxtRows);
			  RdMsg(250);
              ano:=promptb(MsgLine);
			  if not(ano) then
                begin
                  gotoxy(2,TxtRows);write('                                         ');
                  goto 99;
                end;
             end;
			 GrExit:=true;
			 TM^.Done;
             RestorePalette;
{             ShowMouse;}
             ColNorm;
             ClrScr;
			 ClearDevice;
			 exit;
            end;
{********** VSTUPY }
		1 : begin menu2(Param,Par3,Maxim);goto 99;end;
{*********** BARVA TVAR }
		2 : begin menu3(Param,Par3,Maxim);goto 99;end;
{************ PREPINACE }
		3 : begin menu5(Param,Par3);goto 99;end;
{************ NASTAVENI TISKU }
		4 : begin menu6(Param,Par3);goto 99;end;
{************ SOUBOR PARAMETRU }
		5 : begin menu7(Param,Par3,Maxim,GD);goto 99;end;
{********** POPISY TEXTY }
		6 : begin menu4(Param,Par3);goto 99;end;
{************ TEXTY }
		7 : begin menu8(Param,Par3);goto 99;end;
{*********** ZOBRAZENI }
		8 :begin
			 PocZ:=9;
			 while (Udaj3[PocZ]='')and(PocZ<>0) do PocZ:=PocZ-1;
			 if NazevF='' then begin WrLLf10Msg(210);goto 99;end;

			 UdajY:=Udaj3[0];
			 UdajZ:=Udaj3[1];
			 if (Typ='2D')and(UdajZ<>'') then Typ:='3D';
			 if (Typ='Lin')and(UdajZ<>'') then Typ:='L3';
			 if (Typ='LP')and(UdajZ<>'') then Typ:='LP3';
		 begin
			 PrgPtr:=CRdb;
		 333:FileFPtr:=PrgPtr^.FD;
			 while not((FileFPtr=nil)or(LeftUpCh(FileFPtr^.Name)=LeftUpCh(NazevF)))
			   do FileFPtr:=FileFPtr^.Chain;
			 if FileFptr=nil then
			   if PrgPtr^.ChainBack<>nil then
				 begin
				   PrgPtr:=PrgPtr^.ChainBack;
				   goto 333;
				 end
				else
				 begin
				   SetMsgPar(NazevF);WrLLf10Msg(211);
				   goto 99;
				 end;
			 if UdajX='' then begin pom:='X';SetMsgPar(pom);WrLLf10Msg(204);goto 99;end;
			 UdajXPtr:=FileFPtr^.FldD;
			 while (UdajXPtr<>nil)and
				   (LeftUpch(UdajXptr^.Name)<>LeftUpCh(UdajX))
				   {and(UdajXPtr^.Typ<>'D') ???} do
			   UdajXPtr:=UdajXPtr^.Chain;
			 if UdajXPtr=nil then
				 begin
				   SetMsgPar(UdajX);
				   WrLLf10Msg(212);
				   goto 99;
				 end;

	 if (Typ[1]='L')and(UdajXPtr^.Typ<>'F')and(UdajXPtr^.Typ<>'D')and(UdajXPtr^.Typ<>'N') then
	   begin naz:='X';SetMsgPar(naz);WrLLf10Msg(206);goto 99;end;
	 if (Typ[1]<>'L')and(GD^.X^.Typ='T')and(not Interact) then
	   begin naz:='X';SetMsgPar(naz);WrLLf10Msg(206);GrExit:=true;exit;end;


			 if not((Typ='DS')or(Typ='DL')or(Typ='QS')or(Typ='QL')or(Typ='SB')) then
			  begin
			   if UdajY='' then begin pom:='Z';SetMsgPar(pom);WrLLf10Msg(204);goto 99;end;
			   UdajYPtr:=FileFPtr^.FldD;
			   while (UdajYPtr<>nil)and
					 (LeftUpch(UdajYptr^.Name)<>LeftUpCh(UdajY))
					 {and(UdajYPtr^.Typ<>'D') ???} do
				 UdajYPtr:=UdajYPtr^.Chain;
			   if UdajYPtr=nil then
				   begin
					 SetMsgPar(UdajY);
					 WrLLf10Msg(212);
					 goto 99;
				   end;
			  end;

			 if (Typ[1]='3')or(Typ[2]='3')or((length(Typ)>2)and(Typ[3]='3')) then
			  begin
			   if UdajZ='' then begin pom:='Z';SetMsgPar(pom);WrLLf10Msg(204);goto 99;end;
			   UdajZPtr:=FileFPtr^.FldD;
			   while (UdajZPtr<>nil)and
					 (LeftUpch(UdajZptr^.Name)<>LeftUpCh(UdajZ))
					 {and(UdajZPtr^.Typ<>'D') ???} do
				 UdajZPtr:=UdajZPtr^.Chain;
			   if UdajZPtr=nil then
				   begin
					 SetMsgPar(UdajZ);
					 WrLLf10Msg(212);
					 goto 99;
				   end;
			  end;

			 if (Typ='DS')or(Typ='DL')or(Typ='QS')or(Typ='QL')or(Typ='SB') then
			   for i:=0 to PocZ do
				 begin
				   str(i:1,r);
				   if Udaj3[i]='' then begin pom:='Z'+r;SetMsgPar(pom);WrLLf10Msg(204);goto 99;end;
				   Udaj3Ptr[i]:=FileFPtr^.FldD;
				   while (Udaj3Ptr[i]<>nil)and
						 (LeftUpch(Udaj3ptr[i]^.Name)<>LeftUpCh(Udaj3[i]))
						 {and(Udaj3Ptr[i]^.Typ<>'D') ???} do
					 Udaj3Ptr[i]:=Udaj3Ptr[i]^.Chain;
				   if Udaj3Ptr[i]=nil then
					   begin
						 SetMsgPar(Udaj3[i]);
						 WrLLf10Msg(212);
						 goto 99;
					   end;
				 end;
		 end;
			 if NazevF='' then begin
							   WrLLf10Msg(210);
							   goto 99;
							 end;
		   end;
		 end;
	  if (Typ<>'3D')and(Typ<>'L3')and(Typ<>'LP3') then UdajZ:='';
	  ColNorm;
	  ClrScr;
	  ClearDevice;
	  key:=0;
          TM^.Done;
	end;
end;

{--------------Menu1 ******** TYP grafu }
procedure Menu1(var Param:Parametry;var Par3:Param3; var Maxim:Maxima);
  var
    i,volba:integer;
    pom:ComStr;
    TM:PMenuBoxS;
		begin
          with Param,Par3 do
            begin
              pom:=Typ;
              RdMsg(221);
              New(TM,Init(8,8,StringPtr(@MsgLine)));
              volba:=TM^.Exec(1);
              case volba of
				0 : ;
				1 : if UdajZ<>'' then Typ:='3D' else Typ:='2D';
				2 : Typ:='Cir';
				3 : if UdajZ<>'' then Typ:='L3' else Typ:='Lin';
				4 : if UdajZ<>'' then Typ:='LP3'else Typ:='LP';
				5 : Typ:='DS';
				6 : Typ:='DL';
				7 : Typ:='SB';
				8 : Typ:='QS';
				9 : Typ:='QL';
			  end;
{              if volba>=5 then
				begin
				  Udaj3[0]:=UdajY;Udaj3[1]:=UdajZ;
				  Maxim.Maximum:=0;Maxim.MaximumVst:=0;Maxim.MinimumVst:=0;
				  Startveta:=1;PocVet:=max;NazevY:='';NazevZ:='';
				  Udaj3Ptr[0]:=UdajYPtr;Udaj3Ptr[1]:=UdajZPtr;
				  if UdajZ<>'' then PocZ:=1 else PocZ:=0;
                end;}
              WritePar1(Param,Par3);
              TM^.Done;
            end;
		end;

{------------- menu2 *************  VSTUPY}

procedure Menu2(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  var
	PrgPtr:RdbDPtr;
	pom:ComStr;
	breakEsc:boolean;
	c1,c2,r1,r2,c11,r11:word;
	w,wo:longint;

 function FHelp:Str9;
  begin
	c1:=43; r1:=6; c2:=67; r2:=8; c11:=28; r11:=4;
	w:=PushWFramed(c1,r1,c2,r2,colors.sMask,MsgLine,'',WHasFrame+WShadow);
	PrgPtr:=CRdb;
	FileFPtr:=PrgPtr^.FD;
	if FileFPtr<>nil then FileFPtr:=FileFPtr^.Chain;
	while FileFPtr<>nil do
	  begin
		PutSelect(FileFPtr^.name);
		FileFPtr:=FileFPtr^.Chain;
	  end;
	ss.Abcd:=true; SelectStr(28,4,35,'');
	if KbdChar<>_ESC_ then FHelp:=GetSelect else FHelp:='*';
	PopW(w);
  end;

function SelectFile(OnFace:boolean;Name:Str9):Str9;
var mask,s:string; SR:SearchRec; w:longint;
	p:PathStr; d:DirStr; n:NameStr; ext,e:ExtStr; ne:string[12];
	c1,c2,r1,r2:word;
	pom:word;
begin
  SelectFile:=''; c1:=0; r1:=0; c2:=22; r2:=1;
  if OnFace then begin c1:=43; r1:=6; c2:=67; r2:=8; end;
  RdMsg(35);
  wo:=PushWFramed(c1,r1,c2,r2,colors.sMask,MsgLine,'',WShadow+WHasFrame);
  gotoxy(1,1);
  {SelectFile:=promptsU('','A',8,1,Name,true);}
  Mask:=name;
  pom:=EditTxt(Mask,1,pred(sizeof(Mask)),22,'A',true,false,true,false,0);
  if KbdChar=_ESC_ then begin SelectFile:='*';PopW(wo);exit;end;
  if Mask[1]='*' then Mask:=FHelp;
  SelectFile:=Mask;
  PopW(wo);
end;


  label 333,51,52,53,54,55,56,101,102,555;
  var
	i,volba:integer;
	pomret:str15;
	naz:ComStr;
	mask,s:string; SR:SearchRec;
	p:PathStr; d:DirStr; n:NameStr; ext,e:ExtStr; ne:string[12];
	Name,NazevFpom:Str9;
	TM:PMenuBoxS;
	Flds,FldsP:FieldList;
	po:pointer;



		begin
		  with Param,Par3 do
		begin
		  volba:=1;
		  if AutoGr then RdMsg(230) else RdMsg(229);
		  New(TM,Init(6,5,StringPtr(@MsgLine)));

56:       volba:=TM^.Exec(volba);
		  case volba of
		  0:TM^.Done;
		  1:begin
			 if NazevF='' then NazevF:='*';
51:                                           { zadani souboru }
			 nazevFpom:=NazevF;
			 if not(AutoGr) then NazevF:=SelectFile(true,'*');
			 if KbdChar=_ESC_ then begin NazevF:=NazevFpom;goto 56;end;

			 Maxim.Maximum:=0;
			 Maxim.MaximumVst:=0;
			 Maxim.MinimumVst:=0;
			 Startveta:=1;
			 PocVet:=max;
			 NazevX:='';
			 NazevY:='';
			 NazevZ:='';
			 Hlavicka:='';
			 pomret:='        ';

			 if NazevF<>'' then
			   begin
				 PrgPtr:=CRdb;
			 333:FileFPtr:=PrgPtr^.FD;
				 while not((FileFPtr=nil)or(LeftUpCh(FileFPtr^.Name)=LeftUpCh(NazevF)))
				   do FileFPtr:=FileFPtr^.Chain;
				 if FileFptr=nil then
				   if PrgPtr^.ChainBack<>nil then
					 begin
					   PrgPtr:=PrgPtr^.ChainBack;
					   goto 333;
					 end
                    else
					 begin
					   SetMsgpar(NazevF);
					   WrLLf10Msg(211);
					   goto 51;
					 end;
			   end
			  else
			   begin
				 goto 56;
			   end;
										   { zadani udaju }
  CFile:=FileFPtr;
  MarkStore(po);
  Flds:=GetStore(sizeof(Flds^));
  FldsP:=Flds;
  UdajXPtr:=FileFPtr^.FldD;
  while UdajXPtr<>nil do
	begin
	  if UdajXPtr^.Flg and f_Stored<>0 then PutSelect(UdajXPtr^.Name) else PutSelect(SelMark+UdajXPtr^.Name); {'ð'}
	  Flds^.FldD:=UdajXPtr;
	  UdajXPtr:=UdajXPtr^.Chain;
	  Flds^.Chain:=GetStore(sizeof(Flds^));
	  if UDajXPtr<>nil then Flds:=Flds^.Chain;
	end;
  Flds^.Chain:=nil;
  Flds:=FldsP;
  if not SelFieldList(36,true,Flds) then goto 56;

  UdajX:='';UdajXPtr:=nil;NazevX:='';
  if Flds<>nil then
	begin
	  UdajX:=Flds^.FldD^.Name;
	  UdajXPtr:=Flds^.FldD;
	  NazevX:=UdajX;
	end;
  Flds:=Flds^.Chain;
  for i:=0 to 9 do
	if Flds<>nil then
	  begin
		Udaj3[i]:=Flds^.FldD^.Name;
		Udaj3Ptr[i]:=Flds^.FldD;
		Nazev3[i]:=Udaj3[i];
		Flds:=Flds^.Chain;
	  end
	 else
	  begin
		Udaj3[i]:='';
		Udaj3Ptr[i]:=nil;
		Nazev3[i]:='';
	  end;

  ReleaseStore(po);
			 PocZ:=9;
			 while (Udaj3[PocZ]='')and(PocZ<>0) do PocZ:=PocZ-1;
			 if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
			   begin
				 UdajY:=Udaj3[0];
				 UdajZ:=Udaj3[1];
				 UdajYPtr:=Udaj3Ptr[0];
				 UdajZPtr:=Udaj3Ptr[1];
			   end;

555:         if (Typ='2D')and(UdajZ<>'') then Typ:='3D';
			 if (Typ='Lin')and(UdajZ<>'') then Typ:='L3';
			 if (Typ='LP')and(UdajZ<>'') then Typ:='LP3';
			 if (Typ='3D')and(UdajZ='') then Typ:='2D';
			 if (Typ='L3')and(UdajZ='') then Typ:='Lin';
			 if (Typ='LP3')and(UdajZ='') then Typ:='LP';
			 if Hlavicka='' then Hlavicka:=NazevF;
			 if NazevX='' then NazevX:=UdajX;
			 if NazevY='' then NazevY:=UdajY;
			 if NazevZ='' then NazevZ:=UdajZ;
			 for i:=0 to 9 do if Nazev3[i]='' then Nazev3[i]:=Udaj3[i];
			 goto 56;
			end;
		  2:begin
			  TextAttr:=colors.dNorm;
			  ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
			  str(StartVeta:9,naz);
			  gotoxy(4,TxtRows-3);
			  RdMsg(254);
		  naz:=promptstr(MsgLine+' : ','A',9,1,naz);
			  if pos('-',naz)<>0 then naz:='1';
			  val(naz,StartVeta,i);
			  if (StartVeta<1)or(i<>0) then StartVeta:=1;
			  gotoxy(4,TxtRows-2);
			  str(PocVet:2,naz);
			  RdMsg(256);
		  naz:=promptstr(MsgLine+' : ','A',2,1,naz);
			  val(naz,PocVet,i);
			  if (PocVet<1)or(PocVet>max)or(i<>0) then PocVet:=max;
			  if (StartVeta>1) or (PocVet<max) then VyberVet:=true else VyberVet:=false;
			  ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
			  WritePar1(Param,Par3);
			  WritePar2(Param,Par3,true);
			  goto 56;
			end;
		  3:begin
102:
			  MinimumVstP:=0;
			  TextAttr:=colors.dNorm;
			  ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
			  if (Maxim.MinimumVst>=1E12)or
					((Maxim.MinimumVst<0.001)and(Maxim.MinimumVst<>0))
			   then str(Maxim.MinimumVst:12,naz)
			   else  str(Maxim.MinimumVst:12:3,naz);
			  gotoxy(4,TxtRows-3);
			  RdMsg(258);
		  naz:=promptstr(MsgLine+' : ','A',12,1,naz);
			  val(naz,Maxim.MinimumVst,i);
			  if (i<>0)or(Maxim.MinimumVst<-1) then Maxim.MinimumVst:=0;

			  gotoxy(4,TxtRows-2);
			  str(Maxim.MaximumVst:12:3,naz);
			  if (Maxim.MaximumVst>=1E12)or
					((Maxim.MaximumVst<0.001)and(Maxim.MaximumVst<>0))
			   then str(Maxim.MaximumVst:12,naz)
			   else  str(Maxim.MaximumVst:12:3,naz);
			  RdMsg(257);
		  naz:=promptstr(MsgLine+' : ','A',12,1,naz);
              if pos('-',naz)<>0 then naz:='0';
              val(naz,Maxim.MaximumVst,i);
              if i<>0 then Maxim.maximumVst:=0;

              if (Maxim.MaximumVst<>0) then
               if Maxim.MaximumVst<=Maxim.MinimumVst then
                begin
                  WrLLf10Msg(259);
                  goto 102;
				end;
			  ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
			  WritePar1(Param,Par3);
              goto 56;
            end;
          end;
		 if NazevF[1]='*' then NazevF:='';
	   end;
	   end;

{-------------- menu3 ************* BARVA TVAR }

procedure Menu3(var Param:Parametry;var Par3:Param3; var Maxim:Maxima);
  type gr = array [1..16] of integer;
  const gray : gr = (0,1,2,3,4,5,6,8,7,9,10,11,12,13,14,15);
  label 61,62,63,64;
  var
    i,volba,vol,vol1,pom:integer;
    pomret:str15;
    w:longint;
	naz:ComStr;
    TM,TM1,TM2:PMenuBoxS;
        begin
          with Param,Par3 do
        begin
          vol:=1;
          RdMsg(231);
          New(TM,Init(6,6,StringPtr(@MsgLine)));

61:
{              vol:=Menu(231,vol);}
              vol:=TM^.Exec(vol);
               case vol of
                 0 : TM^.Done;
                 1 : begin
                       menu1(Param,Par3,Maxim);
                       goto 61;
                     end;
				 2 : begin
                      RdMsg(233);
                      New(TM1,Init(8,9,StringPtr(@MsgLine)));
                      volba:=TM1^.Exec(1);
                       case volba of
                         1 : Color[1]:='N';
                         2 : Color[1]:='C';
                         3 : Color[1]:='F';
                         4 : Color[1]:='M';
                         5 : Color[1]:='O';
                       end;
                      TM1^.Done;
                      WritePar1(Param,Par3);
                      goto 61;
                     end;
                 3 : begin
                      RdMsg(235);
                      New(TM1,Init(8,10,StringPtr(@MsgLine)));
					  volba:=TM1^.Exec(1);
                       Color[0]:=char(2);
                       case volba of
                         1 : Color[2]:='1';
                         2 : Color[2]:='2';
                         3 : Color[2]:='3';
                         else Color[0]:=char(1);
                       end;
                      TM1^.Done;
                      WritePar1(Param,Par3);
                      goto 61;
                     end;
                 4 : begin
                      RdMsg(237);
                      New(TM1,Init(8,11,StringPtr(@MsgLine)));
                      volba:=TM1^.Exec(1);
                       case volba of
                         0 : Pomer:=50;
						 1 : Pomer:=10;
                         2 : Pomer:=20;
                         3 : Pomer:=30;
                         4 : Pomer:=40;
                         5 : Pomer:=50;
                         6 : Pomer:=60;
                         7 : Pomer:=70;
                         8 : Pomer:=80;
                         9 : Pomer:=90;
                         10: Pomer:=100;
                       end;
                      TM1^.Done;
                      WritePar2(Param,Par3,true);
                      goto 61;
                     end;
                 5 : begin
                      str(StupPolyP:2,naz);
                      RdMsg(240);
					  TextAttr:=colors.dNorm;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                      gotoxy(4,TxtRows-3);
                      naz:=promptstr(MsgLine+' ','A',2,0,naz);
                      val(naz,StupPoly,i);
                      if i<>0 then StupPoly:=0;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                      WritePar1(Param,Par3);
					  goto 61;
                     end;
                 6 : begin

                      RdMsg(312);
                      TextAttr:=colors.dNorm;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                      gotoxy(4,TxtRows-3);
                      if Barvy='' then Barvy:=promptstr(MsgLine+': ','A',14,1,BarvyDef)
                                  else Barvy:=promptstr(MsgLine+': ','A',14,1,Barvy);
					  ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                      WritePar1(Param,Par3);
                      WritePar2(Param,Par3,true);
                      goto 61;
                     end;
                 7 : begin
					   RdMsg(328);
					   New(TM1,Init(8,14,StringPtr(@MsgLine)));
					   vol1:=1;
62:                    vol1:=TM1^.Exec(vol1);
					   case vol1 of
						0:TM1^.Done;
						1:begin
						   RdMsg(330);
						   New(TM2,Init(18,6,StringPtr(@MsgLine)));
						   volba:=1;
				   63:     volba:=TM2^.Exec(volba);
							if volba<>0 then
							begin
							 TextAttr:=colors.dNorm;
							 ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
							 RdMsg(333);
							 ColNorm;
							 gotoxy(15,TxtRows-3);write(MsgLine);
							 gotoxy(4,TxtRows-3);
							 str(RGB[gray[volba]].R,naz);
							naz:=promptstr('R : ','A',3,1,naz);
							 TextAttr:=colors.dNorm;
							 ScrClr(11,TxtRows-4,TxtCols-13,1,' ',TextAttr);
							 val(naz,pom,i);
							 if pom>100 then pom:=100;
							 if (i=0)and(pom>=-1)and(pom<=100) then RGB[gray[volba]].R:=pom;
							 str(RGB[gray[volba]].G,naz);
							 gotoxy(14,TxtRows-3);
							naz:=promptstr('G : ','A',3,1,naz);
							 val(naz,pom,i);
							 if pom>100 then pom:=100;
							 if (i=0)and(pom>=0)and(pom<=100) then RGB[gray[volba]].G:=pom;
							 str(RGB[gray[volba]].B,naz);
							 gotoxy(24,TxtRows-3);
							naz:=promptstr('B : ','A',3,1,naz);
							 val(naz,pom,i);
							 if pom>100 then pom:=100;
							 if (i=0)and(pom>=0)and(pom<=100) then RGB[gray[volba]].B:=pom;
							 ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
							 WritePar1(Param,Par3);
							 goto 63;
							end;
							TM2^.Done;
							goto 62;
						  end;
						2:begin
							RdMsg(331);
							New(TM2,Init(18,7,StringPtr(@MsgLine)));
							volba:=1;
				   64:      volba:=TM2^.Exec(volba);
							if volba<>0 then
							begin
							 TextAttr:=colors.dNorm;
							 ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
							 RdMsg(333);
							 ColNorm;
							 gotoxy(15,TxtRows-3);write(MsgLine);
							 gotoxy(4,TxtRows-3);
							 str(RGB[gray[8+volba]].R,naz);
							naz:=promptstr('R : ','A',3,1,naz);
							 ScrClr(11,TxtRows-4,TxtCols-13,1,' ',TextAttr);
							 val(naz,pom,i);
							 if pom>100 then pom:=100;
							 if (i=0)and(pom>=-1)and(pom<=100) then RGB[gray[8+volba]].R:=pom;
							 str(RGB[gray[8+volba]].G,naz);
							 gotoxy(14,TxtRows-3);
							naz:=promptstr('G : ','A',3,1,naz);
							 val(naz,pom,i);
							 if pom>100 then pom:=100;
							 if (i=0)and(pom>=0)and(pom<=100) then RGB[gray[8+volba]].G:=pom;
							 str(RGB[gray[8+volba]].B,naz);
							 gotoxy(24,TxtRows-3);
							naz:=promptstr('B : ','A',3,1,naz);
							 val(naz,pom,i);
							 if pom>100 then pom:=100;
							 if (i=0)and(pom>=0)and(pom<=100) then RGB[gray[8+volba]].B:=pom;
							 ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
							 WritePar1(Param,Par3);
							 goto 64;
							end;
                            TM2^.Done;
                            goto 62;
                          end;
						3:begin
                           w:=PushW(1,1,TxtCols,TxtRows);
						   ViewRGB(Param);
                           ClrScrG(Param.Barvy);
                           RestorePalette;
                           PopW(w);
                           goto 62;
						  end;
                        end;
                      goto 61;
                     end;
                end;
             end;
        end;
{------------- menu4 *************  POPISY }

  procedure Menu4(var Param:Parametry;var Par3:Param3);
  label 98,66;
  var
    i,volba,vol,pos:integer;
    pomret:str15;
    naz:ComStr;
    TM,TM1:PMenuBoxS;
        begin
          with Param,Par3 do
		begin
          vol:=1;
          RdMsg(241);
          New(TM,Init(6,10,StringPtr(@MsgLine)));
98:
          vol:=TM^.Exec(vol);
              case vol of
                0 : ;
                1 : begin
                      TextAttr:=colors.dNorm;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                      gotoxy(4,TxtRows-3);
                      RdMsg(243);Hlavicka:=promptstr(MsgLine+' ','A',65,0,Hlavicka);
                      ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                      WritePar1(Param,Par3);
                      goto 98;
                    end;
                2 : begin
					  TextAttr:=colors.dNorm;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                      gotoxy(4,TxtRows-3);
                      RdMsg(244);NazevX:=promptstr(MsgLine+' ','A',58,0,NazevX);
                      ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                      WritePar1(Param,Par3);
                      goto 98;
                    end;
                3 : begin
                      TextAttr:=colors.dNorm;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                      gotoxy(4,TxtRows-3);
                      RdMsg(245);NazevY:=promptstr(MsgLine+' ','A',58,0,NazevY);
                      ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                      WritePar1(Param,Par3);
                      goto 98;
                    end;
                4 : begin
					  RdMsg(247);
                      New(TM1,Init(18,3,StringPtr(@MsgLine)));
                      volba:=1;
66:
                      volba:=TM1^.Exec(volba);
                      case volba of
                         1 : begin
                              if not((Typ='DS')or(Typ='DL')or(Typ='QS')or(Typ='QL')or(Typ='SB')) then
                               begin
                                 TextAttr:=colors.dNorm;
                                 ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                                 gotoxy(4,TxtRows-3);
                                 RdMsg(246);NazevZ:=promptstr(MsgLine+' ','A',58,0,NazevZ);
                                 ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                                 WritePar1(Param,Par3);
                                 goto 66;
                               end
                              else
							   begin
                                 TextAttr:=colors.dNorm;
                                 ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                                 gotoxy(4,TxtRows-3);
                                 RdMsg(249);Nazev3[0]:=promptstr(MsgLine+'0: ','A',58,0,Nazev3[0]);
                                 ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                                 WritePar1(Param,Par3);
                                 goto 66;
                               end
                             end;
                       2..10:begin
                               str(volba-1,naz);
                               TextAttr:=colors.dNorm;
                               ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                               gotoxy(4,TxtRows-3);
                               RdMsg(249);Nazev3[volba-1]:=promptstr(MsgLine+naz+': ','A',57,0,Nazev3[volba-1]);
                               ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                               WritePar1(Param,Par3);
							   goto 66;
                             end;

                      end;
                      TM1^.Done;
                    goto 98;
                    end;
                5 : begin
                      RdMsg(262);
                      New(TM1,Init(8,16,StringPtr(@MsgLine)));
                      volba:=TM1^.Exec(1);
                      case volba of
                       1 : PopX:='H';
                       2 : PopX:='V';
                       3 : PopX:='I';
                      end;
                     TM1^.Done;
                     WritePar1(Param,Par3);
					 goto 98;
                    end;
              end;
              TM^.Done;
            end;
        end;

{------------ menu5 ************* PREPINACE }

procedure Menu5(var Param:Parametry;var Par3:Param3);
  var fp:file; {file PCX}
      pom:byte;
  label 62;

  var
    i,vol,volba:integer;
    pomret:str15;
    naz:ComStr;
	TM,TM1:PMenuBoxS;
  begin
    with Param,Par3 do
      begin
        vol:=1;
        RdMsg(260);
        New(TM,Init(6,7,StringPtr(@MsgLine)));
62:
        vol:=TM^.Exec(vol);
        case vol of
          0: TM^.Done;
          1: begin
               RdMsg(264);
               New(TM1,Init(8,9,StringPtr(@MsgLine)));
               volba:=TM1^.Exec(1);
               case volba of
                 1 : begin Rastr:='Y';Mask2:=0;Mask1:=0;end;
                 2 : begin Rastr:='N';Mask2:=0;Mask1:=0;end;
				 3 : Rastr:='H';
                 4 : begin
                      Rastr:='H';
                      str(Mask2:1,naz);
                      RdMsg(242);
                      TextAttr:=colors.dNorm;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                      gotoxy(4,TxtRows-3);
                      naz:=promptstr(MsgLine+' ','N',1,0,naz);
                      val(naz,Mask2,i);
                      if i<>0 then Mask2:=0;
                      if Mask2=0 then Mask1:=0 else Mask1:=Mask2+3;
                      ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                     end;
               end;
               TM1^.Done;
               WritePar1(Param,Par3);
               goto 62;
			 end;
{************ Export do graf.souboru .PCX }
          2:begin
              if length(filePCX)=0 then
                begin
                  RdMsg(273);
                  New(TM1,Init(8,10,StringPtr(@MsgLine)));
                  volba:=TM1^.Exec(1);
                end
               else
                begin
                  RdMsg(272);
                  New(TM1,Init(8,10,StringPtr(@MsgLine)));
                  volba:=TM1^.Exec(1);
                end;
              case volba of
                1:                          { ** ano zapis ** }
                  begin
        	    filePCX:=copy(filePCX,1,pos('.',filePCX)-1);
                    if length(filePCX)=0 then
                      if length(FileParNaz)<>0 then
                           filePCX:=copy(FileParNaz,1,pos('.',FileParNaz)-1)
                        else
                           filePCX:='GRAPH1' ;

                    gotoxy(4,TxtRows-3);
                    RdMsg(304);
                    TextAttr:=colors.dNorm;
                    ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                    filePCX:=promptstr(MsgLine+' : ','A',8,1,FilePCX);
                    if pos('.',filePCX)=0 then
                      filePCX:=concat(filePCX,'.PCX')
                     else
                      filePCX:=concat(copy(filePCX,1,pos('.',filePCX)-1),'.PCX');
(*                    { test existence souboru vyrazen 12.1.95}
                    assign(fp,filePCX);
					{$I-}
                    reset(fp);
                    if ioresult=0 then
                      begin
                        close(fp);
                        gotoxy(23,TxtRows-3);
                        RdMsg(274);
                        naz:=promptstr(MsgLine+' ','A',1,0,' ');
                        if upcase(naz[1])='N' then filePCX:='';
                      end;
                    {$I+}*)
                    ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                    WritePar1(Param,Par3);
                    end;
                2:			{ * nezapisovat * }
                    filePCX:='' ;
              end;  { of case volba (zapis ano ne) }
              TM1^.Done;
			  goto 62 ;
            end;
          3:begin
			 with ViewPort80x25 do
             begin
              TextAttr:=colors.dNorm;
              ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
              gotoxy(4,TxtRows-4);
              str(x1:2,naz);
              RdMsg(297);
	      naz:=promptstr(MsgLine+' X1 : ','A',2,1,naz);
              if pos('-',naz)<>0 then naz:='1';
              val(naz,x1,i);
              if (x1<1)or(i<>0) then x1:=1;

              gotoxy(19,TxtRows-4);
              str(y1:2,naz);
	      naz:=promptstr('Y1 : ','A',2,1,naz);
			  if pos('-',naz)<>0 then naz:='1';
              val(naz,y1,i);
              if (y1<1)or(i<>0) then y1:=1;

              gotoxy(29,TxtRows-4);
              str(x2:2,naz);
              RdMsg(297);
	      naz:=promptstr('X2 : ','A',2,1,naz);
              if pos('-',naz)<>0 then naz:='80';
              val(naz,x2,i);
              if (x2<1)or(i<>0)or(x2>80) then x2:=80;

              gotoxy(39,TxtRows-4);
              str(y2:2,naz);
	      naz:=promptstr('Y2 : ','A',2,1,naz);
              if pos('-',naz)<>0 then naz:='25';
              val(naz,y2,i);
              if (y2<1)or(i<>0)or(y2>25) then y2:=25;

              gotoxy(55,TxtRows-4);
              RdMsg(310);
			  if (Border='N')or(Border='B') then Border:=' ';
	      naz:=promptstr(MsgLine+' ( ,=,-) : ','A',1,1,Border);
              Border:='N';
              if naz='=' then Border:='=';
              if naz='-' then Border:=' ';

              gotoxy(4,TxtRows-3);
              RdMsg(243);
	      naz:=promptstr(MsgLine+' ','A',60,1,HeadFrame);
              for i:=length(naz) downto 1 do if naz[i]=' ' then naz:=copy(naz,1,length(naz)-1);
              HeadFrame:=naz;

              gotoxy(4,TxtRows-2);
              RdMsg(312);
              write(MsgLine);

              gotoxy(15,TxtRows-2);
              RdMsg(299);
	      naz:=promptstr(MsgLine+' : ','A',1,1,ColorWWBack);
              ColorWWBack:=naz[1];

              gotoxy(32,TxtRows-2);
              RdMsg(300);
	      naz:=promptstr(MsgLine+' : ','A',1,1,ColorWWFor);
              ColorWWFor:=naz[1];

              gotoxy(46,TxtRows-2);
			  RdMsg(310);
		  naz:=promptstr(MsgLine+' : ','A',1,1,ColorWWFrame);
			  ColorWWFrame:=naz[1];

			  TextAttr:=colors.dNorm;
			  ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
			  WritePar1(Param,Par3);
			  goto 62;
			 end;
			end;
		end;
	  end;
  end;

{------------ menu6 ************* NASTAVENI TISKU }

procedure Menu6(var Param:Parametry;var Par3:Param3);
  label 97;
  var
    i,vol,volba:integer;
    pomret,printpom:str15;
    naz:ComStr;
    TM,TM1:PMenuBoxS;

    begin
      with Param,Par3 do
		begin
          vol:=1;
          Print:=copy(Print,1,12);
          if upcase(Print[1])<>'P' then printpom:=Print else printpom:='DH200Y';
          RdMsg(275);
          New(TM,Init(6,8,StringPtr(@MsgLine)));
97:
          vol:=TM^.Exec(vol);
            case vol of
              0 :TM^.Done;
              1 :begin
                   RdMsg(287);
                   New(TM1,Init(8,10,StringPtr(@MsgLine)));
                   volba:=TM1^.Exec(1);
                   case volba of
                     1: Print[1]:='D';
                     2: Print[1]:='M';
                     3: Print[1]:='H';
                   end;
                   TM1^.Done;
                   WritePar1(Param,Par3);
                   goto 97;
                 end;
              2 :begin
                   RdMsg(280);
                   New(TM1,Init(8,11,StringPtr(@MsgLine)));
                   volba:=TM1^.Exec(1);
                     case volba of
                       1 : Print[2]:='H';
                       2 : Print[2]:='V';
                     end;
                     TM1^.Done;
                     WritePar1(Param,Par3);
                     goto 97;
                  end;
              3 : begin
                    RdMsg(282);
                    New(TM1,Init(8,12,StringPtr(@MsgLine)));
                    volba:=TM1^.Exec(1);
                    case volba of
                     1 : Print[3]:='1';
                     2 : Print[3]:='2';
                     3 : Print[3]:='3';
                    end;
                    TM1^.Done;
                    WritePar1(Param,Par3);
                    goto 97;
                  end;
              4 : begin
                    RdMsg(284);
                    New(TM1,Init(8,13,StringPtr(@MsgLine)));
                    volba:=TM1^.Exec(1);
                    case volba of
                      1 : Print[4]:='0';
                      2 : Print[4]:='1';
                      3 : Print[4]:='2';
                      4 : Print[4]:='3';
                      5 : Print[4]:='4';
                      6 : Print[4]:='5';
                      7 : Print[4]:='6';
                      8 : Print[4]:='7';
                      9 : Print[4]:='8';
                     10 : Print[4]:='9';
                    end;
                    TM1^.Done;
                    WritePar1(Param,Par3);
                    goto 97;
                  end;
              5 : begin
                    RdMsg(286);
                    New(TM1,Init(8,14,StringPtr(@MsgLine)));
                    volba:=TM1^.Exec(1);
                    case volba of
                      1 : Print[5]:='0';
                      2 : Print[5]:='1';
                      3 : Print[5]:='2';
                      4 : Print[5]:='3';
                      5 : Print[5]:='4';
                      6 : Print[5]:='5';
                      7 : Print[5]:='6';
                      8 : Print[5]:='7';
                      9 : Print[5]:='8';
                     10 : Print[5]:='9';
                    end;
                    TM1^.Done;
                    WritePar1(Param,Par3);
                    goto 97;
                  end;
              6 :begin
                   RdMsg(283);
                   New(TM1,Init(8,15,StringPtr(@MsgLine)));
                   volba:=TM1^.Exec(1);
                   case volba of
                     1 : Print[6]:='Y';
                     2 : Print[6]:='N';
                   end;
                   TM1^.Done;
                   WritePar1(Param,Par3);
                   goto 97;
                 end;

              7 :begin
                   if Print[1]<>'P' then printpom:=Print;
                   RdMsg(289);
                   New(TM1,Init(8,16,StringPtr(@MsgLine)));
                   volba:=TM1^.Exec(1);
                   case volba of
                       1 : begin
                             TextAttr:=colors.dNorm;
                             ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
                             gotoxy(4,TxtRows-3);
                             Print:=promptstr('','A',8,1,'PAU10');
                             if Print[1]<>'P' then Print:='PAU10    ';
                             TextAttr:=colors.dNorm;
                             ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
                           end;
                       2 : Print:=printpom;
                     end;
                     TM1^.Done;
                     WritePar1(Param,Par3);
                     goto 97;
            end;
          end;
        end;
     end;
end.

end.
