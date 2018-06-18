unit GrMenu1;

{$A-,B-,D-,F+,G-,I+,L-,O+,R-,S+,V+,X+}

interface
  uses GrGlob,GrInit,RDRun,WWMix;
  procedure Menu7(var Param:Parametry;var Par3:Param3;var Maxim:Maxima;GD:GraphDPtr);
  procedure Menu8(var Param:Parametry;var Par3:Param3);

implementation

  uses dos,drivers,graph,access,base,obaseww,WWMenu,Grmenu,editor;

procedure Menu7(var Param:Parametry;var Par3:Param3;var Maxim:Maxima;GD:GraphDPtr);
  label 64,54,120,121,47,126,127;
  var
    i,j,ch,Delka,vol,volba:integer;
    par,naz:ComStr;
    FP:text;
{	ParW:ParamW;}
    zn,zn1,zn2,zn3:char;
    Pomstr:string[80];
	stop:boolean;
    WP:ViewPortType;

procedure msg(naz1:NameStr);
var naz2:NameStr;
  begin
    naz2:='RGB';
    Set2MsgPar(naz1,naz2);
    WrLLf10Msg(208);
  end;

procedure msgW(naz1:NameStr);
var naz2:NameStr;
  begin
	naz2:='WW';
    Set2MsgPar(naz1,naz2);
    WrLLf10Msg(208);
  end;

function KonB(KP:integer):char;
begin
    case KP of
	  0:KonB:='L';
      1:KonB:='B';
      2:KonB:='G';
	  3:KonB:='C';
      4:KonB:='R';
      5:KonB:='M';
      6:KonB:='O';
      7:KonB:='a';
	  8:KonB:='A';
      9:KonB:='b';
      10:KonB:='g';
	  11:KonB:='c';
      12:KonB:='r';
      13:KonB:='m';
	  14:KonB:='y';
      15:KonB:='w'
     else
       KonB:='w';
    end;
end;

function KonBrev(KP:char):integer;
begin
    case KP of
      'L':KonBrev:=0;
	  'B':KonBrev:=1;
      'G':KonBrev:=2;
      'C':KonBrev:=3;
      'R':KonBrev:=4;
      'M':KonBrev:=5;
      'O':KonBrev:=6;
	  'a':KonBrev:=7;
	  'A':KonBrev:=8;
	  'b':KonBrev:=9;
	  'g':KonBrev:=10;
	  'b':KonBrev:=11;
	  'r':KonBrev:=12;
	  'm':KonBrev:=13;
	  'y':KonBrev:=14;
	  'w':KonBrev:=15
	 else
	   KonBrev:=99;
	end;
end;

procedure TxWParFil(Par:ComStr;var stop:boolean);

var
  i,j:integer;
  ParP:ComStr;
{  ParWw:ParamW;}
  ParFile:text; { soubor parameru FandGrafu }
  Znak:char;
  pom,n1,n2:Str30;
  zavreno:boolean;
  TextP:str80;
  pp,P:pointer;
  Buffer:string[10];

label 20,30,40,50,9,44;

procedure Zavri;
  begin
    Zavreno:=true;
  end;

function KonPar(ParP:Comstr;var poz:integer;Zprava:str30):integer;

  var pom:ComStr;
	  KP,ErrorCode:integer;

  label 10;

  begin
    pom:='';
    while parP[poz]<>',' do
      begin
		if (parP[poz]=')') or (poz=length(parP)) then goto 10;
		pom:=pom+parP[poz];poz:=poz+1;
      end;
	poz:=poz+1;
	if (length(pom)>2)or(pom[1]='-') then
	  begin n1:=Zprava;n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);zavri;exit;end;
    val(pom,KP,ErrorCode);
    if ErrorCode<>0 then
      10:begin n1:=Zprava;n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);zavri;exit;end;
	KonPar:=KP;
  end; {KonPar}

function KonParChar(ParP:Comstr;var poz:integer;Zprava:str30):integer;

  var pom:ComStr;
      ErrorCode:integer;
      KP:char;

  begin
	pom:='';
    while parP[poz]<>',' do
	  begin
		if (parP[poz]=')') or (poz=length(parP)) then
          begin n1:=Zprava;n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);zavri;exit;end;
		pom:=pom+parP[poz];poz:=poz+1;
	  end;
    poz:=poz+1;
    if (length(pom)>1)or(pom[1]='-') then
      begin n1:=Zprava;n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);zavri;exit;end;
    KP:=pom[1];
    case KP of
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

  procedure Buf(zn:char);
  var ku:byte;
	begin
      for ku:=1 to length(Buffer)-1 do Buffer[ku]:=Buffer[ku+1];
	  Buffer[length(Buffer)]:=zn;
	end;

{----------------------- cteni z param.souboru }
begin
   with ParW do
   begin
	 Zavreno:=false;
	 stop:=false;
	 ParP[0]:=Par[0];
	 for i:=0 to length(par) do parP[i]:=upcase(par[i]);
	  begin
		DelAllParWInTWork;
		assign(ParFile,parP);
		{$I-}
		reset(ParFile);
        if IOresult<>0 then
		  begin
			SetMsgPar(parP);WrLLf10Msg(202);
			Zavri;
            stop:=true;
            goto 44;
          end;
	 30:Buffer:='      ';
        repeat
         read(ParFile,Znak);Buf(upcase(znak));
		until (pos('TXTWIN',Buffer)<>0) or (eof(ParFile));
		if eof(ParFile) then begin close(ParFile);end
         else
          begin
			read(ParFile,Znak);
			if Znak<>'=' then goto 30;
			if eof(ParFile) then begin close(ParFile);end;
            if Znak='=' then
			  begin
				read(ParFile,Znak);
                if Znak<>'(' then
				   begin n1:='(';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
              end;
			pom:='';
			repeat
			  read(ParFile,Znak);
              if (Znak<>#13)and(Znak<>#10) then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
			j:=1;
			XZ:=(KonPar(pom,j,'XZ'));
			if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
            repeat
              read(ParFile,Znak);
              if (Znak<>#13)and(Znak<>#10) then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
			j:=1;
            YZ:=KonPar(pom,j,'YZ');
			if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
			repeat
			  read(ParFile,Znak);
              if (Znak<>#13)and(Znak<>#10) then pom:=pom+Znak;
            until (eof(ParFile))or(Znak=',')or(Znak=')');
            j:=1;
			XK:=KonPar(pom,j,'XK');
            if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
            repeat
              read(ParFile,Znak);
			  if (Znak<>#13)and(Znak<>#10) then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
			j:=1;
            YK:=KonPar(pom,j,'YK');
            if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
            repeat
              read(ParFile,Znak);
			  if (Znak<>#13)and(Znak<>#10)and(Znak<>'''') then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
            j:=1;
			BarPoz:=KonParChar(pom,j,'BarPoz');
			if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
			repeat
			  read(ParFile,Znak);
              if (Znak<>#13)and(Znak<>#10)and(Znak<>'''') then pom:=pom+Znak;
            until (eof(ParFile))or(Znak=',')or(Znak=')');
            j:=1;
			BarPis:=KonParChar(pom,j,'BarPis');
			if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			if (BarPis=Barpoz)and(BarPoz=15) then BarPoz:=0;
			if (GrDriv=HercMono)or(GrDriv=EGAMONO) then BarPoz:=0;
			i:=1;
			FTx:=GetStore(2050);
			repeat
			  read(ParFile,FTx^.A[i]);
			  if FTx^.A[i]='''' then FTx^.A[i]:=' ' else i:=i+1;
			until (eof(ParFile))or(Ftx^.A[i-1]=')');
			FTx^.LL:=i-1;
			if eof(ParFile) then
			  if Ftx^.A[i-1]<>')' then
				begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			Ftx^.A[i-1]:=' ';
			Text:='';
			Ftxt.LL:=FTx^.LL;
			for ftn:=1 to FTxt.LL do Ftxt.A[ftn]:=FTx^.A[ftn];
			ReleaseStore(FTx);
			StoreParWInTWork{(ParW)};
			read(ParFile,Znak);
			if not(eof(ParFile)) then goto 30;
			close(ParFile);
		  end;

		reset(ParFile);
	 50:Buffer:='      ';
		repeat
		 read(ParFile,Znak);Buf(upcase(znak));
		until (pos('TXT',Buffer)<>0) or (eof(ParFile));
		if eof(ParFile) then begin close(ParFile);end
	   else
		  begin
			read(ParFile,Znak);
			if Znak<>'=' then goto 50;
			if eof(ParFile) then begin close(ParFile);end;;
			if Znak='=' then
			  begin
				read(ParFile,Znak);
				if Znak<>'(' then
				begin n1:='(';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			  end;
			pom:='';
			repeat
			  read(ParFile,Znak);
			  if (Znak<>#13)and(Znak<>#10) then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
			j:=1;
			XZ:=(KonPar(pom,j,'XZ'));
			if eof(ParFile) then
			  begin n1:='(';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
			repeat
			  read(ParFile,Znak);
			  if (Znak<>#13)and(Znak<>#10) then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
			j:=1;
			YZ:=KonPar(pom,j,'YZ');
			if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
			repeat
			  read(ParFile,Znak);
			  if (Znak<>#13)and(Znak<>#10) then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
			j:=1;
			BarPoz:=KonPar(pom,j,'Vel');
			if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			pom:='';
			repeat
			  read(ParFile,Znak);
			  if (Znak<>#13)and(Znak<>#10)and(Znak<>'''') then pom:=pom+Znak;
			until (eof(ParFile))or(Znak=',')or(Znak=')');
			j:=1;
			BarPis:=KonParChar(pom,j,'Barva');
			if eof(ParFile) then
			  begin n1:='';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			Text:='';
			i:=1;
			repeat
			  read(ParFile,Text[i]);
			  i:=i+1;
			until (eof(ParFile))or(Text[i-1]=')')or(i=81);
			if eof(ParFile) then
			  if Text[i-1]<>')' then
			  begin n1:=')';n2:='W= nebo V';Set2MsgPar(n1,n2);WrLLf10Msg(208);GRexit:=true;zavri;end;
			Text[0]:=chr(i-2);

			Textp:='';
			for i:=1 to length(Text) do
			  if Text[i]<>'''' then Textp:=Textp+Text[i];
			Text:=Textp;
			FTxt.LL:=0; {FTxt:=nil;}
			StoreParWInTWork{(ParW)};
			read(ParFile,Znak);
			if not(eof(ParFile)) then goto 50;
			close(ParFile);
		  end;
	 end;
  end;
44:
 if Zavreno then DelAllParWInTWork;
end;  { TXWparfil }

{--------- zacatek menu7 }
var
  TM:PmenuBoxS;
  nazPCX:PathStr;

  begin
	with Param,Par3 do
	  begin
		RdMsg(290);
		New(TM,Init(6,9,StringPtr(@MsgLine)));
		vol:=1;
        if FileParNaz='' then FileParNaz:='GRAPH1.FGP';
64: {$I+}
		vol:=TM^.Exec(vol);
          case vol of
			1 : begin
                  for i:=1 to length(FileParNaz) do FileParNaz[i]:=upcase(FileParNaz[i]);
                  if copy(FileParNaz,length(FileParNaz)-3,4)='.FGP' then
                    FileParNaz:=copy(FileParNaz,1,length(FileParNaz)-4);
				  TextAttr:=colors.dNorm;
				  ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
				  gotoxy(4,TxtRows-3);
                  RdMsg(313);
                  FileParNaz:=promptstr(MsgLine+' : ','A',21,1,FileParNaz);
				  ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
				  WritePar1(Param,Par3);
				  WritePar2(Param,Par3,true);
				  if pos('.',FileParNaz)=0 then FileParNaz:=FileParNaz+'.FGP';
				  goto 64;
				end;
	 		 4: begin {zobrazeni PCX}
                              nazPCX:=SelectDiskFile('',35,false);
                              if nazPCX<>'' then ViewPCXInteract(Param,nazPCX);
    			    end;
			 2:
{******** cteni parametru }
				begin
				 TxWParFil(FileParNaz,stop);
				 if stop then goto 64;
                                 {reset parametru}
                                 NazevF:='';
                                 UdajX:='';
                                 UdajY:='';
                                 UdajZ:='';
                                 for i:=0 to 9 do Udaj3[i]:='';PocZ:=0;
                                 NazevX:='';
                                 NazevY:='';
                                 NazevZ:='';
                                 for i:=0 to 9 do Nazev3[i]:='';
                                 Typ:='2DBAR';
                                 Hlavicka:='';
                                 Pomer:=50;
                                 Color:='M';
                                 PopX:='H';
                                 Rastr:='N';
                                 Barvy:='';
                                 StupPoly:=0;
                                 Print:='';
                                 StartVeta:=1;
                                 PocVet:=max;

				 with Param,Par3 do
				   begin
					{$I-}
					par:=FileParNaz;
					NazevF:=Nazev(par,'GRAPH(');
					assign(FP,FileParNaz);
					reset(FP);
{					if ioresult<>0 then begin SetMsgPar(FileParNaz);WrLLf10Msg(251); goto 120;end;}
					if ioresult<>0 then goto 120;
					repeat read(FP,zn);iores:=ioresult until (zn='(')or(eof(FP))or(iores<>0);
					if eof(FP)or(iores<>0) then begin naz:='X';SetMsgPar(naz);WrLLf10Msg(204);goto 120;end;
					repeat read(FP,zn);iores:=ioresult until (zn='(')or(eof(FP))or(iores<>0);
					if eof(FP)or(iores<>0) then begin naz:='X';SetMsgPar(naz);WrLLf10Msg(204);goto 120;end;
					naz:='';
					repeat begin read(FP,zn);iores:=ioresult;if zn>' ' then naz:=naz+zn;end
					until (zn=',')or(zn=')')or eof(FP)or(iores<>0);
					if eof(FP)or(iores<>0) then begin naz:='X';SetMsgPar(naz);WrLLf10Msg(204);goto 120;end;
					UdajX:=copy(naz,1,length(naz)-1);
					i:=0;
					repeat
					  naz:='';
					  repeat begin read(FP,zn);iores:=ioresult;if zn>' ' then naz:=naz+zn;end
					  until (zn=',')or(zn=')')or eof(FP)or(iores<>0);
					  if eof(FP)or(iores<>0) then begin naz:='X nebo Z';SetMsgPar(naz);WrLLf10Msg(204);goto 120;end;
                      Udaj3[i]:=copy(naz,1,length(naz)-1);
					  i:=i+1;
                    until zn=')';
					close(FP);
					iores:=ioresult;
                    Typ:=Nazev(par,'TYPE=');
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
                                      or (Typ='SB')) then
                                                          Typ:='2D';
					Hlavicka:=Nazev(par,'HEAD=');
                    if Hlavicka='' then Hlavicka:=NazevF;
                    if not((Typ[1]='D')or(Typ[1]='Q')) then
                      begin
                        UdajY:=Udaj3[0];
                        UdajZ:=Udaj3[1];
					  end;
					if (Typ='2D')and(UdajZ<>'') then Typ:='3D';
					if (Typ='Lin')and(UdajZ<>'') then Typ:='L3';
					if (Typ='LP')and(UdajZ<>'') then Typ:='LP3';
                    NazevX:=Nazev(par,'HEADX=');
                    if NazevX='' then NazevX:=UdajX;
					NazevY:=Nazev(par,'HEADY=');
                    if NazevY='' then NazevY:=UdajY;
					NazevZ:=Nazev(par,'HEADZ=');
                    if NazevZ='' then NazevZ:=UdajZ;
					PocZ:=0;
                    if (Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB') then
					  begin
						for m:=0 to 9 do
						  begin
							str(m:1,naz);
							if Udaj3[m]<>'' then
							 begin
							  Nazev3[m]:=Nazev(par,'HEADZ'+naz+'=');
							  if (m=0)and(Nazev3[m]='') then Nazev3[m]:=Nazev(par,'HEADZ=');
							  if Nazev3[m]='' then Nazev3[m]:=Udaj3[m];
							  if Udaj3[m]<>'' then PocZ:=PocZ+1;
							 end;
						  end;
						PocZ:=PocZ-1;
						for i:=1 to 9 do if Udaj3[PocZ]='' then PocZ:=PocZ-1;
						for i:=1 to 9 do if (pocZ<9)and(Udaj3[PocZ+1]<>'') then PocZ:=PocZ+1;
                      end;
 		    naz:=Nazev(par,'WIDTH=');
		    val(naz,Pomer,i);
{		    if (Pomer=0) and (Typ='2D') then Pomer:=100;
		    if (Pomer=0) and (Typ='DS') then Pomer:=80;
		    if (Pomer=0) and (Typ='SB') then Pomer:=80;}
                    if (Pomer>100)or(Pomer<5)or(i<>0) then Pomer:=50;
                    Color:='   ';
                    Color:=Nazev(par,'FILL=');
                    Color[1]:=upcase(Color[1]);
                    if Color='' then Color:='M';
					PopX:='';
                    PopX:=Nazev(par,'DIRX=');
					PopX[1]:=upcase(PopX[1]);
					Rastr:='N';
					Rastr:=Nazev(par,'GRID=');
                    if length(Rastr)>=3 then val(Rastr[3],Mask2,i);if i>0 then Mask2:=0;
					if Mask2=0 then Mask1:=0 else Mask1:=Mask2+3;
                    Rastr:=copy(Rastr,1,1);
					Rastr[1]:=upcase(Rastr[1]);
                    if (Rastr<>'Y')and(Rastr<>'N')and(rastr<>'H') then
					  if Typ[1]='C' then Rastr:='Y' else Rastr:='N';
                    Barvy:=Nazev(par,'PALETTE=');
					naz:=Nazev(par,'GRPOLY=');
                    if naz='' then StupPoly:=0 else val(copy(naz,1,2),StupPoly,i);
                    if i<>0 then StupPoly:=0;
					Print:=Nazev(par,'PRINT=');
					if Print='' then Print:='DH200Y' else Print:=Print+'        ';
                    if upcase(Print[1])='P' then goto 47;
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

				47:  naz:=Nazev(par,'RECNO=');
                    if pos('-',naz)<>0 then naz:='1';
                    val(naz,StartVeta,i);
                    if (StartVeta<1)or(i<>0) then StartVeta:=1;
                    naz:=Nazev(par,'NREC=');
                    if pos('-',naz)<>0 then naz:='64';
                    val(naz,PocVet,i);
                    if (PocVet<1)or(PocVet>max)or(i<>0) then PocVet:=max;
				{ WW= }
                    with ViewPort80x25 do begin x1:=1;y1:=1;x2:=80;y2:=25;end;
					with ViewPort do begin
					RectToPix(ViewPort80x25.x1,ViewPort80x25.y1,
					  ViewPort80x25.x2,ViewPort80x25.y2,x1,y1,x2,y2);
					 SetViewPort(x1,y1,x2,y2,ClipOn);
					end;

					naz:=Nazev(par,'WW=');
                    if length(naz)>0 then
					  begin
						Border:='B';
                        Pomstr:='                            ';
                        assign(FP,FileParNaz);
						reset(FP);
						if ioresult<>0 then goto 120;
                        i:=1;
						zn1:=' ';
						zn2:=' ';
						repeat
                          zn1:=zn2;
						  zn2:=zn;
                          read(FP,zn);
                        until ((upcase(zn1)='W')and(upcase(zn2)='W')and(upcase(zn)='='))or eof(FP);
                        if eof(FP) then goto 127;
						repeat
                          read(FP,zn);
						  if (zn>' ')and(zn<>'''') then begin PomStr[i]:=zn;i:=i+1;end;
                        until (eof(FP))or(i>80);
						close(FP);
						i:=0;
				126:        i:=i+1;
                           if Pomstr[1]='(' then
						   begin
                             if Pomstr[2]='(' then begin MakeCls:=false;i:=i+1;end
                                                else MakeCls:=true;
                             naz:='';
                             repeat
							   i:=i+1;
                               if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
							 until (Pomstr[i]=',')or(i>=80);
                             if i>=80 then begin msgw('x1');goto 127;end;
                             val(naz,WP.x1,ch);
							 if ch<>0 then begin msgw('x1');goto 127;end;
							 naz:='';
							 repeat
							   i:=i+1;
							   if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
							 until (Pomstr[i]=',')or(i>=80);
                             if i>=80 then begin msgw('y1');goto 127;end;
                             val(naz,WP.y1,ch);
                             if ch<>0 then begin msgw('y1');goto 127;end;
							 naz:='';
                             repeat
                               i:=i+1;
                               if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
							 until (Pomstr[i]=',')or(i>=80);
							 if i>=80 then begin msgw('x2');goto 127;end;
                             val(naz,WP.x2,ch);
							 if ch<>0 then begin msgw('x2');goto 127;end;
                             naz:='';
                             repeat
                               i:=i+1;
                               if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
                             until (Pomstr[i]=',')or(Pomstr[i]=')')or(i>=80);
							 if i>=80 then begin msgw('y2');goto 127;end;
                             val(naz,WP.y2,ch);
							 if ch<>0 then begin msgw('y2');goto 127;end;
                             with ViewPort80x25 do begin
							   x1:=WP.x1;
							   y1:=WP.y1;
							   x2:=WP.x2;
                               y2:=WP.y2;
                             end;
                             with ViewPort do begin
							  RectToPix(ViewPort80x25.x1-1,ViewPort80x25.y1-1,
								ViewPort80x25.x2,ViewPort80x25.y2,x1,y1,x2,y2);
							 { x1:=TransX(ViewPort80x25.x1-1)+1;
							  y1:=TransY(ViewPort80x25.y1-1)+1;
							  x2:=TransX(ViewPort80x25.x2);
							  y2:=TransY(ViewPort80x25.y2);
							  GMaxX:=x2-x1-1;
							  GMaxY:=y2-y1-1; }
							  SetViewPort(x1,y1,x2,y2,ClipOn);
							 end;
							 if Pomstr[i]=')' then goto 127;
							 naz:='';
							 repeat
							   i:=i+1;
							   if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
							 until (Pomstr[i]=',')or(Pomstr[i]=')')or(i>=80);
							 if i>=80 then begin msgw('Ramecek');goto 127;end;
							 if length(naz)=0 then Border:='N'
							   else begin
									  if naz[1]='=' then begin Border:='=';naz:=copy(naz,2,length(naz));end
													   else Border:=' ';
									  HeadFrame:=naz;
									end;
							 if Pomstr[i]=')' then goto 127;

							 naz:='';
							 repeat
                               i:=i+1;
							   if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
                             until (Pomstr[i]=',')or(Pomstr[i]=')')or(i>=80);
							 if i>=80 then begin msgw('Pozadi');goto 127;end;
                             if naz<>'' then ColorWWBack:=naz[1];
							 if Pomstr[i]=')' then goto 127;
							 naz:='';
							 repeat
							   i:=i+1;
                               if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
                             until (Pomstr[i]=',')or(Pomstr[i]=')')or(i>=80);
                             if i>=80 then begin msgw('Osy');goto 127;end;
							 if naz<>'' then ColorWWFor:=naz[1];
                             if Pomstr[i]=')' then goto 127;
							 naz:='';
                             repeat
                               i:=i+1;
                               if Pomstr[i]<>')' then naz:=naz+Pomstr[i];
                             until (Pomstr[i]=')')or(i>=80);
							 if i>=80 then begin msgw('Ramecek');goto 127;end;
							 if naz<>'' then ColorWWFrame:=naz[1];
                             naz:='';
						   end;
                     end;
{                    if (Hlavicka=NazevF)and(HeadFrame<>'') then Hlavicka:=HeadFrame;}
 		    Maxim.MinimumVst:=0;
                    Maxim.MaximumVst:=0;

                127: naz:=Nazev(par,'RGB=');
                    if length(naz)>0 then
                      begin
						assign(FP,FileParNaz);
						reset(FP);
						if ioresult<>0 then goto 120;
                        zn1:=' ';
                        zn2:=' ';
                        zn3:=' ';
                       repeat
                        repeat
						  zn1:=zn2;
                          zn2:=zn3;
						  zn3:=zn;
                          read(FP,zn);
                        until ((upcase(zn1)='R')and(upcase(zn2)='G')and(upcase(zn3)='B')and(upcase(zn)='='))or eof(FP);
                        if eof(FP) then goto 120;
						i:=1;
						Pomstr:='';
						repeat
						  read(FP,zn);
						  if (zn>' ')and(zn<>'''') then begin PomStr:=Pomstr+zn;i:=i+1;end;
						until (eof(FP))or(i>19)or(zn=')');

						i:=0;
				121:     i:=i+1;
						  if Pomstr[i]='(' then
						   begin
							 i:=i+1;
							 j:=KonBrev(Pomstr[i]);i:=i+1;
							 if j=99 then begin RdMsg(312);msg(MsgLine);goto 120;end;
							 naz:='';
							 repeat
							   i:=i+1;
							   if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
							 until (Pomstr[i]=',')or(i>=18);
							 if i>=18 then begin RGB[j].R:=-1;msg('R');goto 120;end;
							 val(naz,RGB[j].R,ch);
							 if ch<>0 then begin RGB[j].R:=-1;msg('R');goto 120;end;

							 naz:='';
							 repeat
							   i:=i+1;
							   if Pomstr[i]<>',' then naz:=naz+Pomstr[i];
							 until (Pomstr[i]=',')or(i>=18);
							 if i>=18 then begin RGB[j].R:=-1;msg('G');goto 120;end;
							 val(naz,RGB[j].G,ch);
							 if ch<>0 then begin RGB[j].R:=-1;msg('G');goto 120;end;
							 naz:='';
							 repeat
							   i:=i+1;
							   if Pomstr[i]<>')' then naz:=naz+Pomstr[i];
							 until (Pomstr[i]=')')or(i>=18);
							 if i>=18 then begin RGB[j].R:=-1;msg('B');goto 120;end;
							 val(naz,RGB[j].B,ch);
							 if ch<>0 then begin RGB[j].R:=-1;msg('B');goto 120;end;
						   end;
						until eof(FP);
					 end;
				120: {$I+} sound(800);delay(50);nosound;
				end;
				goto 64;
				end;

{******** zapis parametru }
			 3: begin
				  assign(FP,FileParNaz);
				  {$I-}
				  reset(FP);
				  if ioresult=0 then
					begin
                      close(FP);
                      window(1,1,TxtCols,TxtRows);
                      gotoxy(3,TxtRows);
                      RdMsg(292);
                      naz:=promptstr(MsgLine+' ','A',1,0,' ');
                      if not((naz[1]='a')or(naz[1]='A')) then goto 64;
                    end;
                  rewrite(FP);
				  if ioresult<>0 then begin WrLLf10Msg(293);goto 64;end;
                  write(FP,'GRAPH');
				  write(FP,'(',NazevF,',');
				  write(FP,'(',UdajX);
				  PocZ:=9;
				  while (Udaj3[PocZ]='')and(PocZ<>0) do PocZ:=PocZ-1;
                  for i:=0 to PocZ do if Udaj3[i]<>'' then write(FP,',',Udaj3[i]);
                  writeln(FP,'),');

                  naz:=Typ;
                  if (Typ='3D')or(Typ='2D') then naz:='2DBAR';
                  if (Typ='L3')or(Typ='Lin') then naz:='APPROX';
                  if (Typ='LP3')or(Typ='LP') then naz:='POLYREG';
				  if Typ='DS' then naz:='GROUP';
                  if Typ='SB' then naz:='STACKBAR';
                  if Typ='DL' then naz:='GROUPLINE';
				  if Typ='QS' then naz:='3DBAR';
				  if Typ='QL' then naz:='3DLINE';
                  if Typ='Cir' then naz:='CIRCLE';
                  writeln(FP,'TYPE=''',naz,''',');

                  if Color<>'M' then writeln(FP,'FILL=''',Color,''',');
                  writeln(FP,'DIRX=''',PopX,''',');
				  if Rastr='Y' then writeln(FP,'GRID=''Y'',');
                  if Rastr='H' then if Mask2>0 then writeln(FP,'GRID=''H:',Mask2:1,''',')
                                               else writeln(FP,'GRID=''H'',');
{		  if (Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')or(Typ[1]='L') then}
                  if Pomer<>50 then
                      begin str(Pomer,naz);writeln(FP,'WIDTH=',naz,',');end;
				  if Print<>'DH200Y' then writeln(FP,'PRINT=''',Print,''',');
				  if Barvy<>'' then writeln(FP,'PALETTE=''',Barvy,''',');
				  if ((Typ='LP3')or(Typ='LP'))and(StupPoly>0) then writeln(FP,'GRPOLY=',StupPoly:2,',');

                  if Hlavicka='' then writeln(FP,'HEAD='',') else
                    if Hlavicka<>NazevF then writeln(FP,'HEAD=''',Hlavicka,''',');
                  if (NazevX<>'')and(NazevX<>UdajX) then writeln(FP,'HEADX=''',NazevX,''',');
                  if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
                    if (NazevY<>'')and(NazevY<>UdajY) then writeln(FP,'HEADY=''',NazevY,''',');
                  if (Typ[1]='3')or(Typ[2]='3') then
					if (NazevZ<>'')and(NazevZ<>UdajZ) then writeln(FP,'HEADZ=''',NazevZ,''',');
                  if (Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB') then
                    for i:=0 to PocZ do if (Nazev3[i]<>'')and(Nazev3[i]<>Udaj3[i]) then
					  begin str(i,naz);writeln(FP,'HEADZ',naz,'=''',Nazev3[i],''',');end;

                  if VyberVet then
                    begin
                      str(StartVeta,naz);
                      writeln(FP,'RECNO=',naz,',');
                      str(PocVet,naz);
                      writeln(FP,'NRECS=',naz,',');
                    end;
                  with ViewPort do
					begin
                      if {(x1<>LastViewPort.x1)or
                         (x2<>LastViewPort.x2)or
						 (y1<>LastViewPort.y1)or
						 (y2<>LastViewPort.y2)}
                         (ViewPort80x25.x1<>1)or
                         (ViewPort80x25.y1<>1)or
                         (ViewPort80x25.x2<>80)or
                         (ViewPort80x25.y2<>25)or
						 (Border='=')or(Border='N')or
                         (HeadFrame<>'')or
						 (upcase(ColorWWFrame)<>BarvyPasDef[Colors.uNorm])or
                         (upcase(ColorWWBack)<>'L')or
						 (upcase(ColorWWFor)<>'W') then
                       with ViewPort80x25 do
                        begin
						 write(FP,'WW=(');
						 if not MakeCls then write(FP,'(');
                         write(FP,x1,',',y1,',',x2,',',y2);
                         if (Border='N')or(Border=' ') then write(FP,',');
                         if Border='=' then write(FP,',=');
                         if (Border=' ')or(Border='=') then write(FP,'''',HeadFrame,'''');
						 write(FP,',''',ColorWWBack,''',''',ColorWWFor,''',''',ColorWWFrame,''')');
                         if not MakeCls then write(FP,')');
						 writeln(FP,',');
                        end;
					 end;

				  for i:=0 to 15 do if RGB[i].R>-1 then
					writeln(FP,'RGB=(''',KonB(i),''',',RGB[i].R,',',RGB[i].G,',',
										RGB[i].B,'),');
                  close(FP);
                  if ioresult<>0 then begin WrLLf10Msg(293);goto 64;end;

				  ParWPN:=1;
				  if ParWPU[ParWPN]<>0 then
					begin
					  append(FP);
					  repeat
						ReadParWInTWork{(ParW)};inc(ParWPN);
						with ParW do
						  if Text='' then
							begin
							  write(FP,'TXTWIN=(');
							  str(XZ,naz);write(FP,naz,',');
							  str(YZ,naz);write(FP,naz,',');
							  str(XK,naz);write(FP,naz,',');
							  str(YK,naz);write(FP,naz,',');
							  write(FP,'''',KonB(BarPoz),''',');
							  write(FP,'''',KonB(BarPis),''',''');
							  Delka:=FTxt.LL;
							  for i:=1 to Delka do
								write(FP,FTxt.A[i]);
							  write(FP,''')');
							  if ParWPU[ParWPN]<>0 then writeln(FP,',') else writeln(FP);
							end
						   else
							begin
							  write(FP,'TXT=(');
							  str(XZ,naz);write(FP,naz,',');
							  str(YZ,naz);write(FP,naz,',');
							  str(BarPoz,naz);write(FP,BarPoz,',');
							  write(FP,'''',KonB(BarPis),''',''');
							  write(FP,Text,''')');
							  if ParWPU[ParWPN]<>0 then writeln(FP,',') else writeln(FP);
							end;
						until (ParWPU[ParWPN]=0)or(ParWPN>MaxWinGr);
						close(FP);
						if ioresult<>0 then begin WrLLf10Msg(293);goto 64;end;
						append(FP);
						writeln(FP,');');
						close(FP);
						if ioresult<>0 then begin WrLLf10Msg(293);goto 64;end;
					end;
			  54:  {$I+}
				   sound(800);delay(50);nosound;delay(100);
				   sound(800);delay(50);nosound;delay(100);
				   goto 64;
				  end;
           end;
           TM^.Done;
  end;
 end;

{*************  TEXTY, OKNA }
procedure Menu8(var Param:Parametry;var Par3:Param3);
  label 98,66,55,56,57,58,59,88;
  var
    i,j,volba,pos:integer;
    pomret:str15;
    naz:ComStr;
{	ParW:ParamW;}
    key:word;

  function KonBChar(KP:integer):char;
   begin
    case KP of
      0:KonBChar:='L';
      1:KonBChar:='B';
      2:KonBChar:='G';
      3:KonBChar:='C';
      4:KonBChar:='R';
      5:KonBChar:='M';
      6:KonBChar:='O';
      7:KonBChar:='a';
      8:KonBChar:='A';
      9:KonBChar:='b';
      10:KonBChar:='g';
      11:KonBChar:='c';
      12:KonBChar:='r';
      13:KonBChar:='m';
      14:KonBChar:='y';
      15:KonBChar:='w'
     else
       KonBChar:='w';

    end;
    end;

  procedure ViewClear;
   begin
      ScrClr(1,TxtRows-5,TxtCols-2,3,#$b2,TextAttr);
   end;

  procedure ViewClear1;
   begin
      TextAttr:=colors.dNorm;
      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
   end;

  procedure ViewV{(var ParW:ParamW)};
   begin
	with ParW do
    begin
      ViewClear1;
      TextAttr:=colors.dNorm;
      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
      gotoxy(3,TxtRows-4);RdMsg(314);write(msgLine);
      gotoxy(22,TxtRows-4);write(XZ:2);gotoxy(29,TxtRows-4);write(YZ:2);gotoxy(52,TxtRows-4);write(BarPoz:1);
        gotoxy(64,TxtRows-4);write(KonBChar(BarPis));
      gotoxy(3,TxtRows-3);write('TEXT: --------------------------------------------------------------------');
      gotoxy(3,TxtRows-2);write(Text);
    end;
   end;

  procedure EditV{(var ParW:ParamW)};
   var naz:str9;
       ch,i,j:integer;

   begin
	with ParW do
    begin
	  ViewV{(ParW)};
      str(XZ:2,naz);
      gotoxy(22,TxtRows-4);naz:=promptstr('','A',2,1,naz);
	  val(naz,i,ch);if ch=0 then XZ:=i;

	  str(YZ:2,naz);
	  gotoxy(29,TxtRows-4);naz:=promptstr('','A',2,1,naz);
	  val(naz,i,ch);if ch=0 then YZ:=i;

      str(BarPoz:1,naz);
      gotoxy(52,TxtRows-4);naz:=promptstr('','A',1,1,naz);
      val(naz,i,ch);if ch=0 then BarPoz:=i;

      gotoxy(64,TxtRows-4);BarPis:=KonB(promptstr('','A',1,1,KonBChar(BarPis)));

      gotoxy(3,TxtRows-2);Text:=promptstr('','A',74,1,Text);
	  for j:=length(Text) downto 1 do if Text[length(Text)]=' ' then
							Text:=copy(Text,1,length(Text)-1);
	  if Text='' then Text:=' ';
    end;
   end;

  procedure ViewW{(var ParW:ParamW)};
   var
     Delka,Zac,i,j:integer;
	 RadT:str80;
     Pole:array [1..25] of str80;
   label 1;
   begin
	with ParW do
    begin
      ViewClear1;
      TextAttr:=colors.dNorm;
      ScrClr(1,TxtRows-5,TxtCols-2,3,' ',TextAttr);
      Delka:=80;
      Zac:=1;
      for i:=1 to 25 do Pole[i]:='                                                                                ';
      for i:=1 to 2 do
        begin
          RadT:='                                                                                ';
          RadT:='';
          for j:=Zac to Zac+Delka-1 do
		   if j<=FTxt.LL then
			begin
			  if Ftxt.A[j]=';' then begin Zac:=j+1;goto 1;end;
			  if Ftxt.A[j]=#13 then begin Zac:=j+2;goto 1;end;
			  RadT:=RadT+Ftxt.A[j];
			  Zac:=j+1;
			end;
			  1:Pole[i]:=RadT;
		end;
	  gotoxy(3,TxtRows-4);
	  RdMsg(315);Write(MsgLine);
	  gotoxy(21,TxtRows-4);write(XZ:2);gotoxy(26,TxtRows-4);write(YZ:2);gotoxy(39,TxtRows-4);write(XK:2);
	  gotoxy(44,TxtRows-4);write(YK:2);
	  gotoxy(64,TxtRows-4);write(KonBChar(BarPoz));gotoxy(75,TxtRows-4);write(KonBchar(BarPis));
	  gotoxy(3,TxtRows-3);write('TEXT: --------------------------------------------------------------------');
	  gotoxy(3,TxtRows-2);write(copy(Pole[1],1,74));

	end;
   end;

  procedure EditW{(var ParW:ParamW)};
   var
	   Pole:array [1..25] of str80;
       Delka,Zac,i,j,k,l,ch:integer;
       RadT:str80;
       key,PInd:word;
       Srch,Upd:boolean;
   label 1,2;
   begin
	with ParW do
    begin
      Delka:=80;
      Zac:=1;
      for i:=1 to 25 do Pole[i]:='                                                                                ';
      for i:=1 to 25 do
		begin
          RadT:='                                                                                ';
		  RadT:='';
		  for j:=Zac to Zac+Delka-1 do
		   if j<=FTxt.LL then
			begin
			  if Ftxt.A[j]=';' then begin Zac:=j+1;goto 1;end;
			  if Ftxt.A[j]=#13 then begin Zac:=j+2;goto 1;end;
			  RadT:=RadT+Ftxt.A[j];
			  Zac:=j+1;
			end;
			  1:Pole[i]:=RadT;
		end;

	  ViewW{(ParW)};
	  str(XZ:2,naz);
	  gotoxy(21,TxtRows-4);naz:=promptstr('','A',2,1,naz);
	  val(naz,i,ch);if ch=0 then XZ:=i;

	  str(YZ:2,naz);
	  gotoxy(26,TxtRows-4);naz:=promptstr('','A',2,1,naz);
	  val(naz,i,ch);if ch=0 then YZ:=i;

	  str(XK:2,naz);
	  gotoxy(39,TxtRows-4);naz:=promptstr('','A',2,1,naz);
	  val(naz,i,ch);if ch=0 then XK:=i;

	  str(YK:2,naz);
	  gotoxy(44,TxtRows-4);naz:=promptstr('','A',2,1,naz);
	  val(naz,i,ch);if ch=0 then YK:=i;

	  gotoxy(64,TxtRows-4);BarPoz:=KonB(promptstr('','A',1,1,KonBChar(BarPoz)));

	  gotoxy(75,TxtRows-4);BarPis:=KonB(promptstr('','A',1,1,KonBChar(BarPis)));

	  PInd:=1;Srch:=false;
	  window(2,TxtRows-4,TxtCols-2,TxtRows-2);
	  FTx:=GetStore(2050);
	  FTx^.LL:=FTxt.LL;
	  for ftn:=1 to FTxt.LL do Ftx^.A[ftn]:=FTxt.A[ftn];
	  SimpleEditText('T','','',@FTx^.A,2050,FTx^.LL,PInd,Upd);
{      EditText('T','','',@FTxt^.A,FTxt^.LL,PInd,'',Srch,Upd,2050,0,0);}
	  FTxt.LL:=FTx^.LL;
	  for ftn:=1 to FTxt.LL do Ftxt.A[ftn]:=FTx^.A[ftn];
	  ReleaseStore(Ftx);
	  window(1,1,TxtCols,TxtRows);
	  TextAttr:=colors.DeskTopColor;
	  gotoxy(1,1);Write('ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»');
	  ViewW{(ParW)};

	end;
   end;{ of EditW }

const _CtrlY_=25;

var xf,yf:word;pom:str9;w:longint;o:byte;
	p:pointer; s:StringPtr; sofs:word absolute s;
	TM:PMenuBoxS;

begin  { of Menu8 }
 with Param,Par3 do
  begin
    xf:=46;yf:=4;
    begin
55:
      pos:=0;
	  TextColor(colors.sMask);TextBackground(colors.sMask div 16);
      gotoxy(xf,yf);  write('ÚÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿');
      gotoxy(xf,yf+1);write('³                      ³');
      gotoxy(xf,yf+2);write('³                 PgUp ³');
      gotoxy(xf,yf+3);write('³                 PgDn ³');
	  gotoxy(xf,yf+4);write('³                      ³');
	  gotoxy(xf,yf+5);write('³                 Ins  ³');
      gotoxy(xf,yf+6);write('³                 F2   ³');
	  gotoxy(xf,yf+7);write('³                 ^Y   ³');
      gotoxy(xf,yf+8);write('³                      ³');
      gotoxy(xf,yf+9);write('³                 Esc  ³');
      gotoxy(xf,yf+10);write('ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ');
	  ScrColor(xf+1,yf+10,24,colors.ShadowAttr);
      for o:=yf to yf+10 do ScrColor(xf+23,o,2,colors.ShadowAttr);
      gotoxy(xf+6,yf); RdMsg(322);write(MsgLine,' ');
      gotoxy(xf+2,yf+2); RdMsg(323);write(MsgLine);
      gotoxy(xf+2,yf+3);RdMsg(324);write(MsgLine);
	  gotoxy(xf+2,yf+5);RdMsg(325);write(MsgLine);
	  gotoxy(xf+2,yf+6);RdMsg(326);write(MsgLine);
      gotoxy(xf+2,yf+7);RdMsg(327);write(MsgLine);
	  gotoxy(xf+2,yf+9);RdMsg(321);write(MsgLine);

	  ParWPN:=1;
	  if ParWPU[ParWPN]=0 then
		begin
		  key:=_F2_;goto 56;
		end;
	  ReadParWInTWork{(ParW)};
	  pos:=pos+1;
	  if ParW.Text<>'' then ViewV{(ParW)} else ViewW{(ParW)};
	  TextColor(colors.sMask);TextBackground(colors.sMask div 16);
	  if ParW.Text<>'' then
		begin
		  gotoxy(xf+7,yf+10);write(' Text : ',pos:2,' ');
		end
	   else
		begin
		  RdMsg(297);
		  gotoxy(xf+7,yf+10);write(' ',MsgLine,' : ',pos:2,' ');
		end;
	  repeat
	   repeat key:=readkbd until (lo(key)=0)or(key=_ESC_)or(key=_CtrlY_);
	   if key=_ESC_ then
88:     begin
          TextAttr:=colors.DeskTopColor;
          ScrClr(xf-1,yf-1,26,10,#$b1,TextAttr);
		  ScrClr(xf-1,yf+9,26,1,#$b2,TextAttr);
		  ResFile.Get(FandFace,p); s:=p; inc(sofs);
		  s^[0]:=#78; ScrWrStr(1,14,s^,TextAttr);
          ViewClear;
		  exit;
        end;
56:    if key=_F2_ then {F2}
		begin
		  ParWPN:=1;while ParWPU[ParWPN]<>0 do inc(ParWPN);
		  if ParWPN>MaxWinGr then begin WrLLf10Msg(294);goto 55;end;
		  with ParW do  {nulovani}
			begin
			  XZ:=0;YZ:=0;XK:=0;YK:=0;BarPoz:=0;BarPis:=0;
			  Text:='';
			  for j:=1 to 2048 do FTxt.A[j]:=' ';
			  FTxt.LL:=0;
			end;
          RdMsg(296);
		  New(TM,Init(20,11,StringPtr(@MsgLine)));
		  volba:=TM^.Exec(1);
          TM^.Done;
          if volba=1 then
            begin
              gotoxy(xf+7,yf+10);write(' Text : ',ParWPN:2,' ');
			end
		   else
			begin
			  RdMsg(297);
			  gotoxy(xf+7,yf+10);write(' ',MsgLine,' : ',ParWPN:2,' ');
			end;
		  if volba=0 then goto 57;
		  ParW.BarPis:=15;
		  if volba=1 then EditV{(ParW)} else EditW{(ParW)};
		  with ParW do
			begin
			  StoreParWInTWork{(ParW)};
			  dec(ParWPN);
			  pos:=ParWPN;
			end;
		  TextColor(colors.sMask);TextBackground(colors.sMask div 16);
		  gotoxy(xf,yf);  write('ÚÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿');
          gotoxy(xf,yf+1);write('³                      ³');
          gotoxy(xf,yf+2);write('³                 PgUp ³');
		  gotoxy(xf,yf+3);write('³                 PgDn ³');
          gotoxy(xf,yf+4);write('³                      ³');
          gotoxy(xf,yf+5);write('³                 Ins  ³');
          gotoxy(xf,yf+6);write('³                 F2   ³');
		  gotoxy(xf,yf+7);write('³                 ^Y   ³');
          gotoxy(xf,yf+8);write('³                      ³');
		  gotoxy(xf,yf+9);write('³                 Esc  ³');
		  gotoxy(xf,yf+10);write('ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ');
		  ScrColor(xf+1,yf+10,24,colors.ShadowAttr);
		  for o:=yf to yf+10 do ScrColor(xf+23,o,2,colors.ShadowAttr);
          gotoxy(xf+6,yf); RdMsg(322);write(MsgLine,' ');
          gotoxy(xf+2,yf+2); RdMsg(323);write(MsgLine);
		  gotoxy(xf+2,yf+3);RdMsg(324);write(MsgLine);
          gotoxy(xf+2,yf+5);RdMsg(325);write(MsgLine);
		  gotoxy(xf+2,yf+6);RdMsg(326);write(MsgLine);
		  gotoxy(xf+2,yf+7);RdMsg(327);write(MsgLine);
		  gotoxy(xf+2,yf+9);RdMsg(321);write(MsgLine);
		  if volba=1 then
			begin
			  gotoxy(xf+7,yf+10);write(' Text : ',pos:2,' ');
            end
           else
            begin
              RdMsg(297);
			  gotoxy(xf+7,yf+10);write(' ',MsgLine,' : ',pos:2,' ');
			end;
		 end;
57:    if key=_PgDn_ then {PgDn}
		begin
		  i:=1;while ParWPU[i]<>0 do inc(i);
		  if pos<i-1 then
			begin
			  ParWPN:=pos+1;
			  ReadParWInTWork{(ParW)};
			  pos:=pos+1;
			  if ParW.Text<>'' then ViewV{(ParW)} else ViewW{(ParW)};
			end
		   else begin sound(1000);delay(300);nosound;end;
		end;
	   if key=_PgUp_ then {PgUp}
        if pos>1 then
		 begin
		   ParWPN:=pos-1;
		   ReadParWInTWork{(ParW)};
           pos:=pos-1;
		   if ParW.Text<>'' then ViewV{(ParW)} else ViewW{(ParW)};
		 end else begin sound(1000);delay(300);nosound;end;
       if key=_Ins_ then {Ins}
		begin
		  if ParW.Text<>'' then EditV{(ParW)} else EditW{(ParW)};
		  StoreParWInTWork{(parW)};
          dec(ParWPN);
		  key:=_PgDn_;goto 57;
		end;
	   if key=_CtrlY_ then {Del}
		begin
		  DelParWInTWork;
		  if ParWPU[ParWPN]=0 then
			begin
			  TextAttr:=colors.DeskTopColor;
			  ScrClr(xf-1,yf-1,26,10,#$b1,TextAttr);
			  ScrClr(xf-1,yf+9,26,1,#$b2,TextAttr);
			  ResFile.Get(FandFace,p); s:=p; inc(sofs);
			  s^[0]:=#78; ScrWrStr(1,14,s^,TextAttr);
			  ViewClear;
			end;
		  goto 55;
        end;
       if pos=0 then goto 88;
       TextColor(colors.sMask);TextBackground(colors.sMask div 16);
	   if ParW.Text<>'' then
         begin
           gotoxy(xf+7,yf+10);write(' Text : ',pos:2,' ');
         end
		else
         begin
           RdMsg(297);
           gotoxy(xf+7,yf+10);write(' ',MsgLine,' : ',pos:2,' ');
         end;
      until false;
     end;
    end;
  end;

end.
