unit GrInit;

{$A-,B-,D+,F+,G-,I+,L+,O+,R-,S+,V+,X+} {d+,l+}

interface
  uses drivers,GrGlob,base,access,RdRun,sort,GrTransf,obase;

  procedure VypMax(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure Osy(var Param:Parametry;var Maxim:Maxima);
  procedure OsaX(Param:Parametry;Maxim:Maxima);
  procedure Stop(var Param:Parametry);
  procedure TextW;
  procedure CtiPar(var Param:Parametry;Poprve:boolean;var Par3:Param3;var Maxim:Maxima;GD:GraphDPtr);
{  procedure GrStop(var Par:Parametry;var GD:GraphDPtr);}
  procedure ViewPCX(var Par:Parametry;var GD:GraphDPtr);
  procedure ViewPCXInteract(var Par:Parametry;var filePCX:string);
  procedure ViewRGB(var Par:Parametry);
  procedure RestorePalette;

implementation
  uses dos,graph,obaseww,oaccess,runfrml,GrMenu;

{ p©epo‡¡t  sou©adnice z rastru 80x25 do sou‡asn‚ho gr.modu }
function TransX(Pos:integer):integer;
  begin
	TransX:=trunc(Pos/80*GetMaxX);
  end;

function TransY(Pos:integer):integer;
  begin
	TransY:=trunc(Pos/25*GetMaxY);
	if GraphMode=VGA then
	  TransY:=trunc(Pos/25*(GetMaxY-5));
  end;

procedure VypMax(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
var                        { MaximumC ... mantisa skut.nejv.cisla }
 i:Integer;                { MaximumP ... skut.nejvetsi cislo }
 MaximumP,MaximumC:real;   { maximum ... maximum pro graf }
 p,naz:ExtStr;
 U3sum:real;

begin
  with Param,Par3,Maxim do
 begin
  if Typ='Cir' then
    begin
      Maximum:=0;
      Minimum:=1E18;
      for i:=1 to pocvet do
		begin
		  Maximum:=Maximum+UY[i];
          if UY[i]<Minimum then Minimum:=UY[i];
        end;
	  if Minimum<0 then
        begin
		  WrLLf10Msg(205);
		  GrExit:=true;
          exit;
        end;
    end
  else
 begin
  MaxExp:=0;
  maximum:=-10E32;
  if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
    begin
      for i:=1 to pocvet do if UY[i]>maximum then maximum:=UY[i];
      if maximum>1E19 then begin naz:='Z';SetMsgPar(naz);WrLLf10Msg(206);GRExit:=true;exit;end;
	  if UdajZ<>'' then
		for i:=1 to pocvet do if UZ[i]>maximum then maximum:=UZ[i];
      if maximum>1E19 then begin naz:='Z';SetMsgPar(naz);WrLLf10Msg(206);GRExit:=true;exit;end;
    end
   else
	begin
      for i:=1 to pocvet do
	   if Typ='SB' then
        begin
          U3sum:=0;
          for m:=0 to PocZ do U3sum:=U3sum+U3[m,i];
          if U3sum>maximum then maximum:=U3sum;
        end
       else
        for m:=0 to PocZ do if U3[m,i]>maximum then maximum:=U3[m,i];

      str(m:1,p);
      if maximum>1E19 then begin naz:='Z'+p;SetMsgPar(naz);WrLLf10Msg(206);GRExit:=true;exit;end;
    end;
  minimum:=10E32;
  if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
    begin
      for i:=1 to pocvet do if UY[i]<minimum then minimum:=UY[i];
	  if UdajZ<>''
        then for i:=1 to pocvet do if UZ[i]<minimum then minimum:=UZ[i];
	end
   else
     for m:=0 to PocZ do
       for i:=1 to pocvet do if U3[m,i]<minimum then minimum:=U3[m,i];
   if (Typ='SB')and(Minimum<0) then
     begin
       WrLLf10Msg(205);
       GrExit:=true;
       exit;
     end;
  if Maximum<MaximumVst then maximum:=maximumVst;
  if (Minimum<MinimumVst)and(MinimumVst>0) then MinimumVst:=Minimum;
  if (Minimum>0)and(MinimumVst=0) then Minimum:=0;
  if MinimumVst=-1 then
    begin
      if MaximumVst<Maximum then MaximumVst:=Maximum+(Maximum-Minimum)/10;
	  MinimumVst:=Minimum-(Maximum-Minimum)/10;
      if MaximumVst=MinimumVst then
        begin
          MinimumVst:=MaximumVst*0.9;
          MaximumVst:=MaximumVst*1.1;
        end;
    end;
  MaximumP:=maximum;
  MaximumC:=maximum;

  if (maximum>0)and(minimum>=0) then   { kladny graf }
    begin
      i:=-20;
      while not(((exp (i*ln(10) )) >= maximum) and (maximum>=(exp((i-1)*ln(10) ) ) )) do i:=i+1;
      if Maximum<=MaximumVst then maximum:=maximumVst else
        maximum:=exp(i*ln(10));
      MaximumC:=Maximum;
      while maximumC>10000 do
        begin
          maximumC:=maximumC/10;
          MaxExp:=MaxExp+1;
		end;
	  while maximumC<1 do
		begin
          maximumC:=maximumC*10;
          MaxExp:=MaxExp-1;
        end;
    end;

	if minimum<0 then       { zaporny graf }
    begin
      if abs(minimum)>maximum then maximum:=abs(minimum);
      MaximumP:=Maximum;
      i:=-20;
      while not(((exp (i*ln(10) )) >= maximum) and (maximum>=(exp((i-1)*ln(10) ) ) )) do i:=i+1;
      if Maximum<=MaximumVst then maximum:=maximumVst else
        maximum:=exp(i*ln(10));
      MaximumC:=Maximum;
      while maximumC>1000 do
        begin
		  maximumC:=maximumC/10;
		  MaxExp:=MaxExp+1;
        end;
      while maximumC<1 do
        begin
          maximumC:=maximumC*10;
          MaxExp:=MaxExp-1;
		end;
  end;
   if MaximumVst=0 then
    if Maximum<>0 then
      begin
        if (MaximumP/Maximum>0.2)and(MaximumP/Maximum<=0.5) then
          begin
            Maximum:=Maximum*0.5;
            MaximumC:=MaximumC*0.5;
          end;
        if (MaximumP/Maximum>0.1)and(MaximumP/Maximum<=0.2) then
          begin
			Maximum:=Maximum*0.2;
			MaximumC:=MaximumC*0.2;
          end;
        if MaximumP/Maximum<=0.1 then
          begin
            Maximum:=Maximum*0.1;
            MaximumC:=MaximumC*0.1;
		  end;
      end;
 end;
 end; {VypMax}
end;

procedure Osy(var Param:Parametry;var Maxim:Maxima);
var
  p,XM,YM:integer;
  dil:real;
  MaxStr:string[14];
  NazevP:string[80];

  procedure DilY(Y:integer;Hodnota:str9);
    begin
      SetLineStyle(0,0,1);
      Line(XM div 11,Y,XM div 10,Y);
      SetTextJustify(RightText,CenterText);
      SetTextStyle(SmallFont,HorizDir,K(4));
	  OutTextXYC(XM div 12,Y,Hodnota);
    end; {DilY}

begin
  with Param,Maxim do
 begin
  XM:=GMaxX;
  YM:=GMaxY;
{----------------------- hlavicka }
  SetColor(KonB(ColorWWFor));
  SetTextJustify(CenterText,CenterText);
  if length(Hlavicka)>55 then
	SetTextStyle(TriplexFont,HorizDir,K(2))
  else
    SetTextStyle(TriplexFont,HorizDir,K(3));
  OutTextXYC(XM div 2,YM div 18,Hlavicka);
{----------------------- osy }
  SetLineStyle(0,0,1);
  Line(XM div 10,YM div 8,XM div 10,YM-YM div 8); { osa Y }
  Line(XM div 10,YM-YM div 8,XM-XM div 10,Ym-Ym div 8); { osa X }
{----------------------- nazev X }
  if (upcase(PopX[1])='V')or((upcase(PopX[1])='I')and(Typ[1]='L')) then
    begin
      SetTextJustify(CenterText,LeftText);
	  SetTextStyle(SmallFont,VertDir,K(6));
      OutTextXYC(XM-XM div 20,YM-YM div 25,NazevX);
    end
   else
    begin
      SetTextJustify(RightText,CenterText);
	  SetTextStyle(SmallFont,HorizDir,K(6));
	  OutTextXYC(XM-XM div 10,YM-YM div 20,NazevX);
    end;
{----------------------- nazev Y }
  if UdajZ='' then
    begin
      SetTextJustify(CenterText,RightText);
	  SetTextStyle(SmallFont,VertDir,K(6));
      OutTextXYC(XM div 50,YM div 8,NazevY);
    end;
  if UdajZ<>'' then
    begin
      SetTextJustify(CenterText,RightText);
      if length(NazevY)+length(NazevZ)<13 then
		SetTextStyle(SmallFont,VertDir,K(6))
       else
        SetTextStyle(SmallFont,VertDir,K(4));
      if Typ[1]<>'L' then NazevP:='L- '+NazevY else NazevP:=NazevY;
      OutTextXYC(XM div 50,YM div 8,NazevP);
	  SetTextJustify(CenterText,LeftText);
	  if Typ[1]<>'L' then NazevP:='P- '+NazevZ else NazevP:=NazevZ;
	  OutTextXYC(XM div 50,YM - YM div 8,NazevP);
	  SetTextJustify(CenterText,RightText);
	end;
{----------------------- dilky Y }
 if Minimum>=0 then                    
  {----------------------- uprava maxima pro zadane minimum }
  if (MinimumVst>0)and(Maximum>0) then
	begin
	 MinimumC:=MinimumVst*MaximumC/Maximum;
	 MinimumVstP:=MinimumVst;
	 Maximum:=Maximum-MinimumVst;
	 for p:=1 to PocVet do begin UY[p]:=UY[p]-MinimumVst;UZ[p]:=UZ[p]-MinimumVst;end;
	 dil:=(YM - (YM/4))/10;
	 for p:= 0 to 10 do
	  begin
	   if Maximum<1 then
		 str((MaximumC-p*(MaximumC-MinimumC)/10):3:2,MaxStr)
		else
		 str((MaximumC-p*(MaximumC-MinimumC)/10):4:1,MaxStr);
	   if frac(Maximum/10)=0  then
		 str((MaximumC-p*(MaximumC-MinimumC)/10):5:0,MaxStr);

	   DilY(round(YM/8 + p*dil),MaxStr);
	  end;
	end
  else
  begin
   dil:=(YM - (YM/4))/10;
   for p:= 0 to 10 do
	 begin
	   if Maximum<1 then
		 str((MaximumC-p*MaximumC/10):3:2,MaxStr)
		else
		 str((MaximumC-p*MaximumC/10):4:1,MaxStr);
	   if frac(Maximum/10)=0  then
		 str((MaximumC-p*MaximumC/10):5:0,MaxStr);
	   if p=10 then DilY(round(YM/8 + p*dil),MaxStr) else
		 DilY(round(YM/8 + p*dil),MaxStr);
	 end;
   end
  else
  begin
   dil:=(YM - (YM/4))/20;
   for p:= 0 to 20 do
	 begin
	   if Maximum<1 then
		 str((MaximumC-p*MaximumC/10):3:2,MaxStr)
		else
		 str((MaximumC-p*MaximumC/10):4:1,MaxStr);
	   if frac(Maximum/10)=0  then
		 str((MaximumC-p*MaximumC/10):5:0,MaxStr);
	   if p=20 then DilY(round(YM/8 + p*dil),MaxStr) else
		 DilY(round(YM/8 + p*dil),MaxStr);
	   if p=10 then
		 Line(XM div 10,round(YM/8 + p*dil),XM - XM div 10,round(YM/8 + p*dil));
	 end;
   end;
{----------------------- rastr }
  if (Rastr='Y')or(Rastr='y') then
	begin
	  SetLineStyle(1,0,1);
	  if Minimum>=0 then
		for p:=0 to 9 do
		  Line(XM div 10,round(YM/8 + p*dil),XM - XM div 10,round(YM/8 + p*dil))
		 else
		  for p:=0 to 19 do
			Line(XM div 10,round(YM/8 + p*dil),XM - XM div 10,round(YM/8 + p*dil));
      SetLineStyle(0,0,1);
    end;
{----------------------- tisk koeficientu Y }
  if MaxExp<>0 then
    begin
     if MaxExp>0
	  then
       if MaxExp<9 then       {a}
        begin
         str(exp(MaxExp*ln(10)):6:0,MaxStr);
         MaxStr:='x'+MaxStr;
         OutTextXYC(XM div 12,YM div 12,MaxStr);
        end
       else
        begin
         str(exp(MaxExp*ln(10)):9:0,MaxStr);
         MaxStr:='x'+MaxStr;
         OutTextXYC(XM div 10,YM div 12,MaxStr);
        end                   {a}
      else
       if MaxExp>-6 then      {b}
        begin
          str(exp(MaxExp*ln(10)):1:5,MaxStr);
          MaxStr:='x'+MaxStr;
          OutTextXYC(XM div 12,YM div 12,MaxStr);
        end
	   else
        begin
          str(exp(MaxExp*ln(10)):1:12,MaxStr);
          MaxStr:='x'+MaxStr;
          OutTextXYC(XM div 8,YM div 12,MaxStr);
        end                    {b}
    end;
    SetColor(LastColor);
 end; {Osy}
end;

procedure OsaX(Param:Parametry;Maxim:Maxima);
var
  XMp,YMp,pol:real;
  St,Ex:ComStr;
  i,XM,YM:integer;
function L(s:ComStr):ComStr;
begin
  while s[1]=' ' do s:=copy(s,2,length(s)-1);
  L:=s;
end;

begin
  with Param,Maxim do
 begin
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  SetLineStyle(0,0,1);
  SetTextJustify(CenterText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  LastColor:=GetColor;
  SetColor(KonB(ColorWWFor));

  for i:=1 to 11 do
   begin
    line(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
         round(YMp-YMp/9),
         round(XMp/10+(XMp-XMp/5)/10*(i-1)),
		 round(YMp-YMp/8));
    if (Rastr='Y')or(Rastr='y') then
	  begin
		SetLineStyle(1,0,1);
		if i>1 then
		 line(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
			  round(YMp/8),
			  round(XMp/10+(XMp-XMp/5)/10*(i-1)),
			  round(YMp-YMp/8));
		SetLineStyle(0,0,1);
	  end;
	end;
  if PopX='H' then
   begin
	SetTextJustify(CenterText,CenterText);
	SetTextStyle(SmallFont,HorizDir,K(4));
	for i:=1 to 11 do
	 begin
	 if UdajXPtr^.Typ='D' then
	  begin
	   St:=StrDate(UL[1]+(UL[pocvet]-UL[1])/10*(i-1),FieldDMask(UdajXPtr)^);
	   OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
		 round(YMp - YMp/10 ),st);
	  end
	  else
	   if (UL[pocvet]>9.999999999E9)or((abs(UL[1])<0.001)and(abs(UL[1])<>0))or(abs(UL[1])>9.999999999E9) then
		begin
		 str(UL[1]+(UL[pocvet]-UL[1])/10*(i-1),St);
		 OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
				   round(YMp - YMp/10 + YMp*(i mod 2)/35),st);
		end
		 else
		  if (UL[pocvet]>999)and(abs(UL[1])>=1) then
		   begin
			str(UL[1]+(UL[pocvet]-UL[1])/10*(i-1):10:0,St);
			St:=L(St);
			if UL[PocVet]>9999999 then
			  OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
						round(YMp - YMp/10 + YMp*(i mod 2)/35),st)
			 else
			  OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
						 round(YMp - YMp/10 ),st);
		   end
			else
			 begin
			  begin
			   if (UL[pocvet]>99)and(abs(UL[1])>=0{.01}) then
				str(UL[1]+(UL[pocvet]-UL[1])/10*(i-1):3:2,St)
				 else
				 str(UL[1]+(UL[pocvet]-UL[1])/10*(i-1):2:3,St);
			  end;
			  OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
						 round(YMp - YMp/10 ),st);
			 end;
	 end;
   end;
  if (PopX='V')or(PopX='I') then
   begin
	SetTextStyle(SmallFont,VertDir,K(4));
	SetTextJustify(RightText,RightText);
	for i:=1 to 11 do
	begin
	 if UdajXPtr^.Typ='D' then
	  begin
	   St:=StrDate(UL[1]+(UL[pocvet]-UL[1])/10*(i-1),FieldDMask(UdajXPtr)^);
	   OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)+2),
				  round(YMp - YMp/9+1 ),st);
	  end
	  else
	   if (UL[pocvet]>9999)or((abs(UL[1])<0.001)and(abs(UL[1])<>0))or(abs(UL[1])>9999) then
		begin
		 SetTextJustify(BottomText,TopText);
		 SetTextStyle(SmallFont,VertDir,K(2));
		 str(UL[1]+(UL[pocvet]-UL[1])/10*(i-1),St);
		 St:=copy(St,1,9)+copy(St,13,5);
		 OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)),
				   round(YMp - YMp/10 - YMp/50),st);
		end
		else
		begin
		 str(UL[1]+(UL[pocvet]-UL[1])/10*(i-1):5:3,St);
		 OutTextXYC(round(XMp/10+(XMp-XMp/5)/10*(i-1)+2),
				round(YMp - YMp/9+1 ),st);
		end;
	end;
   end;
   SetColor(LastColor);

 end; {OsaX}
end;

{------------ stop }

procedure Stop(var Param:Parametry);
var
  pix,x,y,p,ErrorCode:word;
  k,XM,YM,ne:integer;
  MaxColor : word ;
  naz:extstr;

{  key:word;}

procedure Tisk;

{ Print 1  N,M,H kvalita  Normal,Medium,Hight
        2  H,V smer
        3  0-3 hustota
		4  0-9 cm od leveho okraje
		5  0-9 pocet radku od hor. okr.
		6  Y,N odstrankovat po tisku}
type
  colors = record
             c: array [0..15] of byte ;
           end ;
const
  gray : array [0..7] of colors =
     ((c:($00,$00,$01,$11,$11,$31,$31,$35,$ca,$ce,$ce,$ee,$ee,$fe,$ff,$ff)),
      (c:($00,$40,$40,$40,$48,$4a,$4a,$ca,$35,$b5,$b5,$b7,$bf,$bf,$bf,$ff)),
      (c:($00,$04,$04,$84,$85,$85,$a5,$a5,$5a,$5a,$7a,$7a,$7b,$fb,$fb,$ff)),
	  (c:($00,$00,$10,$10,$30,$30,$32,$32,$cd,$cd,$cf,$cf,$ef,$ef,$ff,$ff)),
	  (c:($00,$00,$80,$81,$81,$85,$85,$95,$6a,$7a,$7a,$7e,$7e,$7f,$ff,$ff)),
	  (c:($00,$20,$20,$24,$24,$24,$68,$69,$96,$97,$db,$db,$db,$df,$df,$ff)),
      (c:($00,$02,$02,$02,$82,$92,$92,$92,$6d,$6d,$6d,$7d,$fb,$fd,$fd,$ff)),
	  (c:($00,$00,$48,$48,$48,$48,$4d,$4d,$b2,$b2,$b7,$b7,$b7,$b7,$ff,$ff)));

   bit : array [0..7] of byte = (128,64,32,16,8,4,2,1) ;
   bit2: array [0..3] of byte = ($C0,$30,$0C,$03) ;
   bit4: array [0..1] of byte = ($F0,$0F) ;

    barplat :array [0..6,0..15] of byte =
  (($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF),
   ($00,$00,$00,$00,$FF ,$00,$00,$00,$00,$00,$00,$00,$FF ,$00,$00,$00),
   ($00,$FF ,$00,$00,$00,$00,$00,$00,$00,$FF ,$00,$00,$00,$00,$00,$00),
   ($00,$00,$00,$00,$00,$FF ,$00,$00,$00,$00,$00,$00,$00,$FF ,$00,$00),
   ($00,$00,$00,$00,$00,$00,$00,$FF ,$FF ,$00,$00,$00,$00,$00,$FF ,$00),
   ($00,$00,$00,$FF ,$00,$00,$FF ,$00,$00,$00,$00,$FF ,$00,$00,$00,$00),
   ($00,$00,$FF ,$00,$00,$00,$00,$00,$00,$00,$FF ,$00,$00,$00,$00,$00));

var
  x,y:word;
  col,a,p,m,n,j,l,b,pb:byte;
  duplicate : integer ;
  k,u,i,XM,YM,ErrorCode:integer;
  cp:boolean;
  TestLst:boolean;
  ViewPortP:ViewPortType;
  ptr:pointer;
  SizeIm:word;

function pix(x,y:integer ; bit:byte):byte;
var
	 color : byte ;
begin
 if (x>XM)or(y>YM) then	        { mimo obrazovku }
	pix:= $00
  else
	begin
	  color := GetPixel(x,y) ;
	   if upcase(Param.Color[1]) = 'C'  then
		  pix := gray[bit].c[ color ]
	   else
		if color <> Black then
		   pix := $ff
		else
		   pix := $00
	end ;
end; {pix}

procedure PrintByte(B:byte);
  const timeout=$01;errorio=$08;
		selected=$10;outofpaper=$20;acknowledge=$40;notbusy=$80;
  var r:registers;  N:word;
	  P:pointer;Vel:word;
  label 2,3;

  begin
    with printer[prCurr] do  {tisk na logicky port}
      if ToHandle then begin WriteH(Handle,1,B); exit end;

    if not TestLst then exit;
	r.AH:=0;r.AL:=B;r.DX:=printer[prCurr].Lpti-1;Intr($17,r);
		if ((r.AH and errorio)<>0)or((r.AH and outofpaper)<>0)
		 or((r.AH and timeout)<>0) then
	begin
	 repeat
		r.AH:=0;r.AL:=B;r.DX:=printer[prCurr].Lpti-1;Intr($17,r);
		if ((r.AH and errorio)<>0)or((r.AH and outofpaper)<>0)
		 or((r.AH and timeout)<>0) then
		   begin
			 if (r.AH and errorio)<>0 then
				if (r.AH and outofpaper)<>0 then N:=11 {out of paper}
				else N:=12 {turn printer on}
			 else if (r.AH and timeout)<>0 then
				   N:=13; {printer offline}
			 vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
			 P:=GetStore(Vel);
			 SetViewPort(0,0,GetmaxX,GetMaxY,true);
			 GetImage(0,TransY(24)-3,GetMaxX,TransY(25),P^);
			 SetWindow(ViewPortP);
			 F10SpecKey:=_ESC_;WrLLF10Msg(N);
			 SetViewPort(0,0,GetmaxX,GetMaxY,true);
			 PutImage(0,TransY(24)-3,P^,NormalPut);
			 SetWindow(ViewPortP);
			 ReleaseStore(P);
				if KbdChar=_ESC_ then
				  begin
					TestLst:=false;
					exit;
				  end;
		   end;
	 until not(((r.AH and errorio)<>0)or((r.AH and outofpaper)<>0)
		 or((r.AH and timeout)<>0));
	end;
  end;

procedure WritePRNCfg(rad:ComStr);
var i:byte;
begin
  for i:=1 to length(rad) do PrintByte(ord(rad[i]));
end;

procedure WriteLnPRNCfg(rad:ComStr);
var i:byte;
begin
  for i:=1 to length(rad) do PrintByte(ord(rad[i]));
  PrintByte($0D);PrintByte($0A);
end;

function  LstAbort : boolean ;
var u   : integer ;
	key,Vel : word ;
	P:pointer;
begin
  LstAbort := FALSE ;
  if keypressed then  begin
	key:=readkey;
	if key=_ESC_ then  begin { konec , stisknuto Esc }

	vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
	P:=GetStore(Vel);
	SetViewPort(0,0,GetmaxX,GetMaxY,true);
	GetImage(0,TransY(24)-3,GetMaxX,TransY(25),P^);
	SetWindow(ViewPortP);
	if not(PromptYN(22)) then
	  begin
		SetViewPort(0,0,GetmaxX,GetMaxY,true);
		PutImage(0,TransY(24)-3,P^,NormalPut);
		SetWindow(ViewPortP);
		ReleaseStore(P);
		exit;
	  end;
	 SetViewPort(0,0,GetmaxX,GetMaxY,true);
	 PutImage(0,TransY(24)-3,P^,NormalPut);
	 SetWindow(ViewPortP);
	 ReleaseStore(P);
	 if Printer[prCurr].Typ='L' then
	  begin
	   WritePRNCfg(chr(27)+'*rB') ;
	   if Param.Print[6]='Y'
		then
		 WritePRNCfg(chr(27)+'E')
	  end
	 else
	  begin
	   if Param.Print[6]='Y' then	 { odstrankovat ? }
		 WriteLnPRNCfg(chr(12));      { PF }
	   WritePRNCfg(PrTab(PrLine72)+chr(12));   { nastaveni radkovani na 1/6 inch }
	   WriteLnPRNCfg(PrTab(PrReset));
	  end;
	 LstAbort := TRUE ;
	end;
  end;
end;  { LstAbort }

procedure PrintExit;
begin
  if ptr<>nil then
	begin
	  RunMsgOff;RunMsgClear;
	  SetViewPort(0,0,GetmaxX,GetMaxY,true);
	  PutImage(0,GetMaxY-30,ptr^,NormalPut);ReleaseStore(ptr);
	  SetWindow(ViewPortP);
	  ptr:=nil;
	end;
end;

procedure TiskLSJetQ;
var
   rezolution : string[3] ;
   strBytesPL : string[3] ;
   strX,strY  : string[4] ;
   x,y,k,i,ErrCode : integer ;
   b,bt            : byte ;
begin
  with Param do begin
   val(Print[4],y,ErrCode);		{ y [cm]  od leveho okraje }
   val(Print[5],x,ErrCode);		{ x [radku] od horniho okr.}
   str(TRUNC(y*11.76*10),strX) ;
   str(TRUNC(x*11.76*4.64),strY) ;
   case Print[3] of                     { rozliseni-hustota }
	'1': rezolution := '100' ;
	'2': rezolution := '150' ;
	'3': rezolution := '300' ;
	else rezolution := '75' ;
   end;
   WritePRNCfg(chr(27)+'E') ;		{ reset }
   if not TestLst then begin PrintExit;exit;end;
   WritePRNCfg(PrTab(PrLine72));    { nastaveni radkovani }
   if not TestLst then begin PrintExit;exit;end;

   duplicate:=1;
 case upcase(Print[1]) of               { kvalita }
 'M'  : case m of
		0     : begin
				duplicate := 2 ;
				WritePRNCfg(chr(27)+'*t150R') ;
				if Print[2]='H' then str(TRUNC(XM/4)+1,strBytesPL)
				  else str(TRUNC(YM/4)+1,strBytesPL);
				end;
		1,2   : begin
				duplicate := 2 ;
				WritePRNCfg(chr(27)+'*t300R') ;
				if Print[2]='H' then str(TRUNC(XM/4)+1,strBytesPL)
				  else str(TRUNC(YM/4)+1,strBytesPL);
				end;
		3     : begin
				WritePRNCfg(chr(27)+'*t300R') ;
				if Print[2]='H' then str(TRUNC(XM/8)+1,strBytesPL)
				  else str(TRUNC(YM/8)+1,strBytesPL);
				end;
		end;
 'H'  : case m of
		0     : begin
				duplicate := 4 ;
				WritePRNCfg(chr(27)+'*t300R') ;
                if Print[2]='H' then str(TRUNC(XM/2)+1,strBytesPL)
				  else str(TRUNC(YM/2)+1,strBytesPL);
				end;
		1,2   : begin
				duplicate := 2 ;
				WritePRNCfg(chr(27)+'*t300R') ;
				if Print[2]='H' then str(TRUNC(XM/4)+1,strBytesPL)
				  else str(TRUNC(YM/4)+1,strBytesPL);
				end ;
		3     : begin
				WritePRNCfg(chr(27)+'*t300R') ;
				if Print[2]='H' then str(TRUNC(XM/8)+1,strBytesPL)
				  else str(TRUNC(YM/8)+1,strBytesPL);
				end;
		end;

 else
		   begin
				WritePRNCfg(chr(27)+'*t'+rezolution+'R') ;
				if Print[2]='H' then str(TRUNC(XM/8)+1,strBytesPL)
				  else str(TRUNC(YM/8)+1,strBytesPL);
		   end;
 end;

   if not TestLst then begin PrintExit;exit;end;
   WritePRNCfg(chr(27)+'*p'+strX+'X') ;        { umisteni X }
   if not TestLst then begin PrintExit;exit;end;
   WritePRNCfg(chr(27)+'*p'+strY+'Y') ;        { umisteni Y }
   if not TestLst then begin PrintExit;exit;end;
   WritePRNCfg(chr(27)+'*r1A');		{ start graphics }
   if not TestLst then begin PrintExit;exit;end;
   case Print[2] of
   'H':
	   begin
		 RunMsgOn('P',trunc(YM));
	   for y:=0 to YM do                { pres radky }
	   begin
		if y<YM-40 then RunMsgN(y);  {   zobrazeni % }
		if y=YM-40 then
		  begin RunMsgN(YM);delay(100);RunMsgOff;RunMsgClear;
				SetViewPort(0,0,GetmaxX,GetMaxY,true);
				PutImage(0,GetMaxY-30,ptr^,NormalPut);ReleaseStore(ptr);ptr:=nil;
				SetWindow(ViewPortP);
				end;

		for i:=1 to duplicate do begin       { kvalita }
		   if LstAbort then begin PrintExit;exit;end;
		   WritePRNCfg(chr(27)+'*b'+strBytesPL+'W'); { ridici kod radku }
		   if not TestLst then begin PrintExit;exit;end;

		   case duplicate of
		   1:for x:=1 to TRUNC(XM/8)+1 do begin      { data radku }
				b := $00 ;
				for k:=0 to 7 do
				  b := b OR pix((x-1)*8+k,y,y MOD 8) AND bit[k] ;
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
			 end;
		   2:for x:=1 to TRUNC(XM/4)+1 do begin
				b := $00 ;
				for k:=0 to 3 do begin
				  if (pix((x-1)*4+k,y,y MOD 8) AND bit[ ((x-1)*4+k) mod 8 ])<>$00 then
					 b:=b OR bit2[k] ;
				end;
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
			end;
		  4:for x:=1 to TRUNC(XM/2)+1 do begin
				b := $00 ;
				for k:=0 to 1 do begin
				  if (pix((x-1)*2+k,y,y MOD 8) AND bit[((x-1)*2+k) mod 8]) <> 0 then
					b := b OR bit4[k] ;
				end;
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
		   end;
		 end;
		end;
		end;
	   end;
   'V':
	   begin
		RunMsgOn('P',trunc(XM));
		for x:=XM downto 0 do               { pres radky }
		begin
		 if x>72 then RunMsgN(XM-x);  {   zobrazeni % }
		 if x=72 then
		   begin RunMsgN(XM);delay(100);RunMsgOff;RunMsgClear;
				 SetViewPort(0,0,GetmaxX,GetMaxY,true);
				 PutImage(0,GetMaxY-30,ptr^,NormalPut);ReleaseStore(ptr);ptr:=nil;
				 SetWindow(ViewPortP);
				 end;
		for i:=1 to duplicate do  begin { kvalita }
		   if LstAbort then begin PrintExit;exit;end;
		   WritePRNCfg(chr(27)+'*b'+strBytesPL+'W'); { ridici kod radku }
		   if not TestLst then begin PrintExit;exit;end;

		   case duplicate of
		   1:for y:=1 to TRUNC(YM/8)+1 do begin      { data radku }
				b := $00 ;
				for k:=0 to 7 do
				  b := b OR pix(x,(y-1)*8+k,x mod 8 ) AND bit[((y-1)*8+k) MOD 8] ;
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
			 end;
		   2:for y:=1 to TRUNC(YM/4)+1 do begin
				b := $00 ;
				for k:=0 to 3 do begin
				  if (pix(x,(y-1)*4+k,x MOD 8) AND bit[((y-1)*4+k) mod 8 ])<>$00 then
					 b:=b OR bit2[k] ;
				end;
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
			 end;
		   4:for y:=1 to TRUNC(YM/2)+1 do begin
				b := $00 ;
				for k:=0 to 1 do begin
				  if (pix(x,(y-1)*2+k,x MOD 8) AND bit[((y-1)*2+k) mod 8]) <> 0 then
					b := b OR bit4[k] ;
				end;
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
			 end;
		   end;         { case duplicate }
		 end;           { next i }
	   end;
	 end;

   end;                 { case Print[2] }
   WritePRNCfg(chr(27)+'*rB');			{ end graphics }
   if not TestLst then begin PrintExit;exit;end;
   if upcase(Print[6])='Y' then
	 WritePRNCfg(chr(27)+'E');
   if not TestLst then begin PrintExit;exit;end;
   beep;
 end;
end;  { TiskLSJetQ }

procedure SetRezolution( m,dots : integer );
begin
 duplicate := 1; {$I-}
 case upcase(Param.Print[1]) of               { kvalita }
 'M'  : case m of
		0     : begin
				duplicate := 2 ;
				WritePRNCfg(PrTab(PrDen120)+chr(lo(2*dots))+chr(hi(2*dots)));
				end;
		1,2   : begin
				duplicate := 2 ;
				WritePRNCfg(PrTab(PrDen240)+chr(lo(2*dots))+chr(hi(2*dots)));
                end;
		3     : WritePRNCfg(PrTab(PrDen240)+chr(lo(dots))+chr(hi(dots)));
        end;
 'H'  : case m of
        0     : begin
				duplicate := 4 ;
				WritePRNCfg(PrTab(PrDen240)+chr(lo(4*dots))+chr(hi(4*dots)));
				end;
		1,2   : begin
				duplicate := 2 ;
                WritePRNCfg(PrTab(PrDen240)+chr(lo(2*dots))+chr(hi(2*dots)));
				end ;
		3     : WritePRNCfg(PrTab(PrDen240)+chr(lo(dots))+chr(hi(dots)));
		end;

 else   case m of
		0    :  WritePRNCfg(PrTab(PrDen60)+chr(lo(dots))+chr(hi(dots)));
		1,2  :  WritePRNCfg(PrTab(PrDen120)+chr(lo(dots))+chr(hi(dots)));
		3    :  WritePRNCfg(PrTab(PrDen120)+chr(lo(dots))+chr(hi(dots)));
		end;
  if not TestLst then begin PrintExit;exit;end;
 end;
end;  { SetRezolution }

var Po:pointer;Vel:word;

begin   { of procedure Tisk }
  with printer[prCurr] do begin {test tisku pres manager}
      if ToMgr then begin {beep;}                             {Mark***}
          vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
          Po:=GetStore(Vel);
          SetViewPort(0,0,GetmaxX,GetMaxY,true);
          GetImage(0,TransY(24)-3,GetMaxX,TransY(25),Po^);
          SetWindow(ViewPortP);
          {F10SpecKey:=_ESC_;} WrLLF10Msg(276);
          SetViewPort(0,0,GetmaxX,GetMaxY,true);
          PutImage(0,TransY(24)-3,Po^,NormalPut);
          SetWindow(ViewPortP);
          ReleaseStore(Po);
          exit end;
      if ToHandle then Handle:=OpenLPTHandle; 
      end;

  with Param do
  begin
  TestLst:=true;
  ptr:=nil;
  GetViewSettings(ViewPortP);
  XM:=ViewPortP.x2-ViewPortP.x1;
  YM:=ViewPortP.y2-ViewPortP.y1;

  SizeIm:=ImageSize(0,GetMaxY-30,76,GetMaxY);
  ptr:=GetStore(SizeIm);
  SetViewPort(0,0,GetmaxX,GetMaxY,true);
  GetImage(0,GetMaxY-30,76,GetMaxY,ptr^);
  SetWindow(ViewPortP);

  m:=2;                                 { default hustota 2 .. 120 dpi }
  if Print[3]='1' then m:=0;            { hustota  60 dpi }
  if Print[3]='3' then m:=3;            { hustota 240 dpi }
  if Print[1]='H' then                  { kvalita tisku }
	 n:=4 else if Print[1]='M' then n:=2 else n:=1;

  vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
  Po:=GetStore(Vel);
  SetViewPort(0,0,GetmaxX,GetMaxY,true);
  GetImage(0,TransY(24)-3,GetMaxX,TransY(25),Po^);
  SetWindow(ViewPortP);
  F10SpecKey:=_ESC_;WrLLF10Msg(10);
  SetViewPort(0,0,GetmaxX,GetMaxY,true);
  PutImage(0,TransY(24)-3,Po^,NormalPut);
  SetWindow(ViewPortP);
  ReleaseStore(Po);
	 if KbdChar=_ESC_ then
	   begin
		 TestLst:=false;
		 exit;
	   end;

  if Printer[prCurr].Typ='L' then begin   { tisk na laserove tiskarne }
	TiskLSJetQ;
	exit;
  end;

  RunMsgOn('P',0);

  if Printer[prCurr].Typ='C' then  { tisk na barevne maticove tiskarne }
	cp:=true else cp:=false;
  {$I-}
  if Print[5]<>'0' then begin           { pocet radku od horniho okraje }
	val(Print[5],p,ErrorCode);
	for i:=1 to p do WriteLnPRNCfg('');
  end;

  WriteLnPRNCfg(PrTab(PrLine72)+chr(12)); { nastaveni radkovani }
  val(Print[4],p,ErrorCode);		{ vynechat zleva }

  WritePRNCfg(PrTab(PrLine72)+chr(8));   { nastaveni radkovani pro grafiku }
  if Print[2]='H' then begin            { tisk v horz. smeru }
	 RunMsgOff;RunMsgClear;
	 SetViewPort(0,0,GetmaxX,GetMaxY,true);
	 PutImage(0,GetMaxY-30,ptr^,NormalPut);{ReleaseStore(ptr);ptr:=nil;}
	 SetWindow(ViewPortP);
	 RunMsgOn('P',trunc(YM/8));
	 for i:=0 to trunc(YM/8) do        { radky na tiskarne }
	   begin
	   if i<trunc(YM/8)-2 then RunMsgN(i);  {   zobrazeni % }
	   if i=trunc(YM/8)-2 then
		 begin RunMsgN(trunc(YM/8));delay(100);RunMsgOff;RunMsgClear;
			   SetViewPort(0,0,GetmaxX,GetMaxY,true);
			   PutImage(0,GetMaxY-30,ptr^,NormalPut);ReleaseStore(ptr);ptr:=nil;
			   SetWindow(ViewPortP);
			   end;
		for j:=1 to n do                { opakovani s posunem - kvalita }
		 begin
		 if cp then
		  begin
		   pb:=6;
		   col:=0;
		   for k:=0 to XM do
			for l:=0 to 7 do
			  case getpixel(k,i*8+l) of
				4,12:col  := col or 1;
				1,9:col   := col or 2;
				5,13:col  := col or 4;
				7,8,14:col:= col or 8;
				3,6,11:col:= col or 16;
				2,10:col  := col or 32;
			  end;
		  end
		   else pb:=0;
		 for a:=0 to pb do
		  if (a=0)or
			 ((a=1)and((col and 1)>0))or
			 ((a=2)and((col and 2)>0))or
			 ((a=3)and((col and 4)>0))or
			 ((a=4)and((col and 8)>0))or
			 ((a=5)and((col and 16)>0))or
			 ((a=6)and((col and 32)>0)) then
		begin                          { body v radku }
		  if Print[4]<>'0' then         { vynechat zleva }
		   begin
			WritePRNCfg(PrTab(PrDen60)+chr(p*24)+chr(0));
			if not TestLst then begin PrintExit;exit;end;
			for k:=1 to p*24 do WritePRNCfg(chr(0));
			if not TestLst then begin PrintExit;exit;end;
		   end;

		  if LstAbort then begin PrintExit;exit;end;
		  if cp then WritePRNCfg(PrTab(PrColor)+chr(a));
		  SetRezolution(m,XM+1) ;
		  if not TestLst then begin PrintExit;exit;end;
		  for k:=0 to XM do            { body v radku }
		   begin
			b:=0;

			if cp then
			 for l:=0 to 7 do
			  b:=b or (barplat[a,getpixel(k,i*8+l)] and bit[l])
			else
			 for l:=0 to 7 do
			  b:=b OR (pix(k,i*8+l,k MOD 8) AND bit[l]) ; { byte }

			if b=$1B then  b:=$19;
			WritePRNCfg(chr(b));
			if not TestLst then begin PrintExit;exit;end;
			if duplicate>1 then
			 begin
			  WritePRNCfg(chr(b));
			  if not TestLst then begin PrintExit;exit;end;
			  if duplicate=4 then
			   begin
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
				WritePRNCfg(chr(b));
				if not TestLst then begin PrintExit;exit;end;
			   end;
			 end;
		   end;                { next k to XM }
		  if cp then WritePRNCfg(chr(13)); { navrat na zacatek radku pro dalsi barvu }
		 end;
		  if n=4 then begin WritePRNCfg(PrTab(PrLine216)+chr(1));WriteLnPRNCfg(''); end;
		  if not TestLst then begin PrintExit;exit;end;
		  if n=2 then begin WritePRNCfg(PrTab(PrLine216)+chr(2));WriteLnPRNCfg(''); end;
		  if not TestLst then begin PrintExit;exit;end;
		 end;                  { next j to n  .. kvalita }
		if n>1 then WritePRNCfg(PrTab(PrLine216)+chr(20));
		if not TestLst then begin PrintExit;exit;end;
		WriteLnPRNCfg('');
		if not TestLst then begin PrintExit;exit;end;
	  end;                    { next i to lines }
	end

   else  begin                          { vertikalni graf }
	 begin
	   RunMsgOff;RunMsgClear;
	   SetViewPort(0,0,GetmaxX,GetMaxY,true);
	   PutImage(0,GetMaxY-30,ptr^,NormalPut);{ReleaseStore(ptr);ptr:=nil;}
	   SetWindow(ViewPortP);
	 end;
	 for i:=0 to trunc(XM/8)  do       { radky na tiskarne }
	   begin
	   if i=9 then
		 begin
		   RunMsgOn('P',trunc(XM/8));
		   RunMsgN(9);
		 end;
	   if (i>9)and(i<trunc(XM/8)) then RunMsgN(i);  {   zobrazeni % }
	   if i=trunc(XM/8) then
		 begin RunMsgN(trunc(XM/8));delay(100);RunMsgOff;RunMsgClear;
			   SetViewPort(0,0,GetmaxX,GetMaxY,true);
			   PutImage(0,GetMaxY-30,ptr^,NormalPut);ReleaseStore(ptr);ptr:=nil;
			   SetWindow(ViewPortP);
			   end;
		for j:=1 to n do                { opakovani s posunem - kvalita }
		 begin
		 if cp then
		  begin
		   pb:=6;
		   col:=0;
		   for k:=YM downto 0 do
			for l:=0 to 7 do
			  case getpixel(i*8+l,k) of
				4,12:col  := col or 1;
				1,9:col   := col or 2;
				5,13:col  := col or 4;
				7,8,14:col:= col or 8;
				3,6,11:col:= col or 16;
				2,10:col  := col or 32;
			  end;
		  end
		   else pb:=0;
		 for a:=0 to pb do
		  if (a=0)or
			 ((a=1)and((col and 1)>0))or
			 ((a=2)and((col and 2)>0))or
			 ((a=3)and((col and 4)>0))or
			 ((a=4)and((col and 8)>0))or
			 ((a=5)and((col and 16)>0))or
			 ((a=6)and((col and 32)>0)) then
		begin
		  if Print[4]<>'0' then
		   begin
			WritePRNCfg(PrTab(PrDen60)+chr(p*24)+chr(0));
			if not TestLst then begin PrintExit;exit;end;
			for k:=1 to p*24 do
			  WritePRNCfg(chr(0));
			if not TestLst then begin PrintExit;exit;end;
		   end;

		  if LstAbort then begin PrintExit;exit;end;
		  if cp then WritePRNCfg(PrTab(PrColor)+chr(a));
		  SetRezolution(m,YM+1) ;
		  if not TestLst then begin PrintExit;exit;end;
		  for k:=YM downto 0 do            { body v radku }
			begin
			 b:=0 ;
			if cp then
			 for l:=0 to 7 do
			  b:=b or (barplat[a,getpixel(i*8+l,k)] and bit[l])
			else
			 for l:=0 to 7 do
			b:=b OR pix(i*8+l,k,k MOD 8) AND bit[l]; { byte }
             if b=$1B then b:=$19;
             WritePRNCfg(chr(b));
             if not TestLst then begin PrintExit;exit;end;
             if duplicate>1 then
			  begin
			   WritePRNCfg(chr(b));
               if not TestLst then begin PrintExit;exit;end;
               if duplicate=4 then
				begin
				 WritePRNCfg(chr(b));
                 if not TestLst then begin PrintExit;exit;end;
                 WritePRNCfg(chr(b));
                 if not TestLst then begin PrintExit;exit;end;
                end;
              end;
            end;                                  { next k from YM to 0 }
          if cp then WritePRNCfg(chr(13)); { navrat na zacatek radku pro dalsi barvu }
		  if not TestLst then begin PrintExit;exit;end;
		end;
          if n=4 then begin WritePRNCfg(PrTab(PrLine216)+chr(1));WriteLnPRNCfg(''); end;
          if not TestLst then begin PrintExit;exit;end;
          if n=2 then begin WritePRNCfg(PrTab(PrLine216)+chr(2));WriteLnPRNCfg(''); end;
		  if not TestLst then begin PrintExit;exit;end;
		 end;                                    { next j to n }
		if n>1 then WritePRNCfg(PrTab(PrLine216)+chr(20));
        if not TestLst then begin PrintExit;exit;end;
		WriteLnPRNCfg('');
		if not TestLst then begin PrintExit;exit;end;
     end;                                       { next i to lines }
   end;                                         { konec vert.grafu }

  if Print[6]='Y' then
    WriteLnPRNCfg(chr(12));
  if not TestLst then exit;
  WritePRNCfg(PrTab(PrLine72)+chr(12));   { nastaveni radkovani na 1/6 inch }
  WritePRNCfg(PrTab(PrReset));
  if not TestLst then exit;
  {$I+}
 end;
end; {Tisk}

var vol:boolean;Vel,i:word;Po:pointer;PViewPort:ViewPortType;fp:file;
    h,m,s,t:word;s1,s2:longint;Mys:boolean;
label 11,12;

begin                   { of procedure stopo }
 with Param do begin
  XM:=GMaxX;
  YM:=GMaxY;
{  if WWShadow then ShowShadow;}
 {------------------------- pauza a konec pri P=Txx }
  if (upcase(Print[1])='P')and(upcase(Print[2])='A')and(upcase(Print[3])='U') then
	begin
	  val(copy(Print,4,2),p,ErrorCode);
	  if ErrorCode<>0 then val(copy(Print,4,1),p,ErrorCode);
	  if (ErrorCode=0)and(P>=0)and(P<=99) then
	   begin
             gettime(h,m,s,t);s1:=h*360000+m*6000+s*100+t;
		repeat
			if keypressed then
			  begin
				key:=readkey;
				if key=27 then exit;
				repeat sound(500);delay(10);nosound;delay(1200) until keypressed;
				key:=readkey;
				if key=27 then exit;
			  end;
                  gettime(h,m,s,t);s2:=h*360000+m*6000+s*100+t;
                until (s2>s1+p*100) or (s2<s1);
                exit;
           end;
        end;
{  if spec.AutoRprtPrint then
   begin
     key := _F6_ ;
     Print[1]:='N' ;
   end;}
  While  TRUE do begin
     begin
	  if key=0 then
	   repeat
         key:=readkbd;
	   until (key=_F6_)or			{ Tisk }
             (key=_ShiftF6_)or			{ Tisk okna }
			 (key=_F9_)or 		        { Zapis *.PCX }
			 (key=_ShiftF9_)or 		        { Zapis *.PCX okna }
			 (key=_F4_)or                       { Inverze }
			 (key=_ShiftF4_)or                    { Inverze okna }
			 (key=_F5_)or                       { prepina kurzor mysi }
		 (key=_ESC_);                  { Esc }
	  case key of
		_ESC_   : begin                         { konec .. pouze Esc }
					if MakeCls then if not(Interact) then
					  ClrScrG(Barvy);
					if GrDriv = VGA then
					  begin
						LastColor:=GetColor;
						SetColor(0);
						PViewPort:=ViewPort;
						ViewPort.x1:=0;
						ViewPort.y1:=GetMaxY-5;
						ViewPort.x2:=GetMaxX;
						ViewPort.y2:=GetMaxY;
						with ViewPort do SetViewPort(x1,y1,x2,y2,true);
						for i:=1 to 5 do
						  line(0,i,GetMaxX,i);
						ViewPort:=PViewPort;
						SetColor(LastColor);
					  end;
					ViewPort:=LastViewPort;
					with LastViewPort do SetViewPort(x1,y1,x2,y2,true);
					exit;
				  end;
		_F9_: begin
					 if GraphMode=VGAHi then SetViewPort(0,0,GetmaxX,GetMaxY-5,true)
					  else SetViewPort(0,0,GetmaxX,GetMaxY,true);
					 if (length(filePCX)<>0) then
					   begin
						 if (not Interact) and PCX then filePCX:='GRAPH1.PCX';
                                                 if AssignPCX<>'' then filePCX:=AssignPCX;
						 vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
						 Po:=GetStore(Vel);
						 GetImage(0,TransY(24)-3,GetMaxX,TransY(25),Po^);
						 vol:=PromptYN(214);
						 PutImage(0,TransY(24)-3,Po^,NormalPut);
						 ReleaseStore(Po);
						 if vol then
						  begin
                                                    { test existence souboru vlozeno 12.1.95}
                                                    assign(fp,filePCX);
				                                        {$I-}
                                                    reset(fp);
                                                    if ioresult=0 then
                                                      begin
                                                        close(fp);
						        vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
						        Po:=GetStore(Vel);
						        GetImage(0,TransY(24)-3,GetMaxX,TransY(25),Po^);
						        vol:=PromptYN(274);
						        PutImage(0,TransY(24)-3,Po^,NormalPut);
						        ReleaseStore(Po);
                                                        if not vol then goto 11;
                                                      end;
                                                    {$I+}
                                                   HideMouse;
                                                   Mys:=MausExist;
                                                   MausExist:=false;
						   ne:=SavePCX(filePCX);
                                                   MausExist:=Mys;
                                                   if Interact then ShowMouse;
						   if ne<>0 then
							 begin str(ne:1,naz);SetMsgPar(naz);WrLLf10Msg(213);end;
11:						  end;
					   end;
					 SetWindow(ViewPort);
				  end;
		_ShiftF9_: if (length(filePCX)<>0) then
					   begin
						 if (not Interact) and PCX then filePCX:='GRAPH1.PCX';
                                                 if AssignPCX<>'' then filePCX:=AssignPCX;
						 SetViewPort(0,0,GetmaxX,GetMaxY,true);
						 vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
						 Po:=GetStore(Vel);
						 GetImage(0,TransY(24)-3,GetMaxX,TransY(25),Po^);
						 vol:=PromptYN(214);
						 PutImage(0,TransY(24)-3,Po^,NormalPut);
						 SetWindow(ViewPort);
						 if vol then
						  begin
                                                    { test existence souboru vlozeno 12.1.95}
                                                    assign(fp,filePCX);
				                                        {$I-}
                                                    reset(fp);
                                                    if ioresult=0 then
                                                      begin
                                                        close(fp);
						        vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
						        Po:=GetStore(Vel);
						        GetImage(0,TransY(24)-3,GetMaxX,TransY(25),Po^);
						        vol:=PromptYN(274);
						        PutImage(0,TransY(24)-3,Po^,NormalPut);
						        ReleaseStore(Po);
                                                        if not vol then goto 12;
                                                      end;
                                                    {$I+}
                                                   HideMouse;
                                                   Mys:=MausExist;
                                                   MausExist:=false;
						   ne:=SavePCX(filePCX);
                                                   MausExist:=Mys;
                                                   if Interact then ShowMouse;
						   if ne<>0 then
							 begin str(ne:1,naz);SetMsgPar(naz);WrLLf10Msg(213);end;
						 SetViewPort(0,0,GetmaxX,GetMaxY,true);
						 PutImage(0,TransY(24)-3,Po^,NormalPut);
						 ReleaseStore(Po);
						 SetWindow(ViewPort);
12:						  end;
					   end;
		_F4_    : begin
					if GraphMode=VGAHi then SetViewPort(0,0,GetmaxX,GetMaxY-5,true)
					  else SetViewPort(0,0,GetmaxX,GetMaxY,true);
					InverzeWin ;
					SetWindow(ViewPort);
				  end;
		_ShiftF4_ : InverzeWin ;
		_F6_ : begin
				if GraphMode=VGAHi then SetViewPort(0,0,GetmaxX,GetMaxY-5,true)
					  else SetViewPort(0,0,GetmaxX,GetMaxY,true);
                                HideMouse;
                                Mys:=MausExist;
                                MausExist:=false;
				Tisk ;
                                with printer[prCurr] do {Mark***}
                                     if ToHandle then CloseH(Handle) else
                                     if OpCls then CloseH(OpenLPTHandle);
                                MausExist:=Mys;
                                if Interact then ShowMouse;
				SetWindow(ViewPort);
			   end;
		_ShiftF6_    :
			   begin
{				 SetViewPort(0,0,GetmaxX,GetMaxY,true);
				 vel:=ImageSize(0,TransY(24)-3,GetMaxX,TransY(25));
				 Po:=GetStore(Vel);
				 GetImage(0,TransY(24)-3,GetMaxX,TransY(25),Po^);}
                                 HideMouse;
                                 Mys:=MausExist;
                                 MausExist:=false;
				 Tisk;
                                 with printer[prCurr] do {Mark***}
                                      if ToHandle then CloseH(Handle) else
                                      if OpCls then CloseH(OpenLPTHandle);
                                 MausExist:=Mys;
                                 if Interact then ShowMouse;
{				 PutImage(0,TransY(24)-3,Po^,NormalPut);
				 SetWindow(ViewPort);}
			   end;
(*   dodelat volani gr. editoru (i do grtransf)
		_F5_:begin if MausVisibleGr then HideMouse else ShowMouse;
				   MausVisibleGr:=not MausVisibleGr;
			 end; *)
	  end;
	  key:=0;
	end;
{      if spec.AutoRprtPrint then key := _ESC_ ;}
  end;  { while True }
 end;   { with Param... }
end; {Stop*}

{--------- textw }

procedure TextW;
  var
{    ParW : ParamW;}
	LastColor:word;
procedure Win(XZ,YZ,XK,YK:integer;BarPoz,BarPis:byte;tx:LongStrAll);

var
  i : byte;
  j,Zac,Delka :integer;
  RadT : ComStr;
  XZp,YZp,XKp,YKp:integer;

procedure FillQad(x1,y1,x2,y2,x3,y3,x4,y4,col:word);
  var  qad : array [1..4] of PointType;
  begin
	qad[1].x:=x1;
	qad[1].y:=y1;
	qad[2].x:=x2;
	qad[2].y:=y2;
	qad[3].x:=x4;
	qad[3].y:=y4;
	qad[4].x:=x3;
	qad[4].y:=y3;
	SetFillStyle(1,col);
	FillPoly(4,qad);
  end;

label 1;

begin
  begin
	  if XK>80 then XK:=80;
      if XK<1 then XK:=1;
      if XZ>80 then XZ:=80;
	  if XZ<1 then XZ:=1;
      if YK>25 then YK:=25;
	  if YK<1 then YK:=1;
	  if YZ>25 then YZ:=25;
	  if YZ<1 then YZ:=1;
	  if XZ>XK then XK:=XZ+1;
	  if YZ>YK then YK:=YK+1;

	  if BarPoz>15 then BarPoz:=15;
	  if BarPis>15 then BarPis:=15;
      if (BarPoz=0)and(BarPis=0) then BarPis:=15;
      if (BarPoz=15)and(BarPis=15) then BarPoz:=0;
      if BarPis=BarPoz then BarPis:=BarPis+1;

      XZ:=XZ-1;
      YZ:=YZ-1;
      Delka:=XK-XZ-1;
	  SetLineStyle(0,0,1);
      if (BarPoz=15)and(BarPis=0) then SetColor(15) else SetColor(BarPis);

	  XZp:=round(TransX(XZ)*int(GMaxX)/int(GetMaxX));
	  YZp:=round(TransY(YZ)*int(GMaxY)/int(GetMaxY));
	  XKp:=round(TransX(XK)*int(GMaxX)/int(GetMaxX));
{	  if GrDriv=VGA then
		YKp:=round(TransY(YK)*int(GMaxY-5)/int(GetMaxY))
	   else }    {Vyrazeno pro hlasenou chybu}
		YKp:=round(TransY(YK)*int(GMaxY)/int(GetMaxY));

	  rectangle(XZp,YZp,XKp,YKp);
	  if not((BarPoz=0)and(BarPis=15)) then
	  FillQad(XZp+1,YZp+1,XKp-1,YZp+1,
			  XZp+1,YKp-1,XKp-1,YKp-1,BarPoz);
	  SetTextStyle(SmallFont,HorizDir,K(5));
	  SetTextJustify(LeftText,TopText);
	  SetColor(BarPis);

	  Zac:=1;
	  if Delka>=0 then
      for i:=YZ to YK-1 do
        begin
          RadT:='                                                                                ';
          RadT:='';
		  YZp:=round(TransY(i)*int(GMaxY)/int(GetMaxY));
          for j:=Zac to Zac+Delka do
		   if j<=tx.LL then
			begin
			  if tx.A[j]=';' then begin Zac:=j+1;goto 1;end;
			  if tx.A[j]=#13 then begin Zac:=j+2;goto 1;end;
			  RadT:=RadT+tx.A[j];
			  Zac:=j+1;
			end else Zac:=j+1;
		  1:OutTextXYC(XZp+3,YZp,RadT)
		end;
	end;
end;

procedure Txt(XZ,YZ:integer;Vel,BarPis:byte;txt:str80);

var
  i : byte;
  j,Zac,Delka :integer;
  RadT : ComStr;

label 1;

begin
    begin
{	  if XZ=0 then XZ:=1;
	  if YZ=0 then YZ:=1;}
	  if XZ>80 then XZ:=80;
	  if YZ>25 then YZ:=25;

      if Vel=0 then Vel:=1;
      if Vel>10 then Vel:=10;
      if BarPis>15 then BarPis:=15;

	  XZ:=round(TransX(XZ)*int(GMaxX)/int(GetMaxX));
	  YZ:=round(int(TransY(YZ))*int(GMaxY)/int(GetMaxY));
      SetTextStyle(TriplexFont,HorizDir,K(Vel));
      SetTextJustify(LeftText,BottomText);
      SetColor(BarPis);
      OutTextXYC(XZ,YZ,Txt)
    end;
end;

begin
  ParWPN:=1;
  while ParWPU[ParWPN]<>0 do
  begin
{        setviewport(0,0,GetMaxX,GetMaxY,true);}
	LastColor:=GetColor;
	ReadParWInTWork{(ParW)};
        with ParW do
        if Text='' then
	  Win(XZ,YZ,XK,YK,BarPoz,Barpis,FTxt)
	 else
          Txt(XZ,YZ,BarPoz,BarPis,Text);
	inc(ParWPN);
    SetColor(LastColor);
  end;
end;

{---------- param }
procedure CtiPar(var Param:Parametry;Poprve:boolean;var Par3:Param3;var Maxim:Maxima;GD:GraphDPtr);

label 10;
var i,ch,volba,delkaX:integer;
	par,naz:ComStr;
	Xf,Isdata:boolean;
	Scan:XscanPtr;
	pp:pointer;
	md:LockMode;
	SK:KeyFldDPtr;
	Bool:FrmlPtr;
	aaa:string;
	m:byte;

{----------------------- prikazovy radek }
begin
with Param,Par3 do
 begin
  if (Poprve)and(not Interact) then
   begin
	 ParRad(Param,Par3,Maxim,GD);
	 if GRexit then exit;
	 FileFPtr:=GD^.FD;
	 if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
       begin
         GD^.Y:=GD^.ZA[0];
		 GD^.Z:=GD^.ZA[1];
       end
      else GD^.Z:=GD^.ZA[0];
     UdajXPtr:=GD^.X;
     UdajYPtr:=GD^.Y;
	 UdajZPtr:=GD^.Z;
	 for m:=0 to 9 do Udaj3Ptr[m]:=GD^.ZA[m];
	 Interact:=GD^.Interact;
   end;
10:if Interact then
	begin
	  if (Border=' ')or(Border='=') then
		with ViewPort80x25 do
		  begin
			x1:=x1-1;y1:=y1-1;x2:=x2+1;y2:=y2+1;
			RectToPix(x1,y1,x2,y2,ViewPort.x1,ViewPort.y1,ViewPort.x2,ViewPort.y2);
			with ViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
		  end;
	  with LastViewPort do SetViewPort(x1,y1,x2,y2,Clip);
	  MenuPar(Param,Par3,Poprve,Maxim,GD);
	  with ViewPort do
	  begin
		RectToPix(ViewPort80x25.x1,ViewPort80x25.y1,
				  ViewPort80x25.x2,ViewPort80x25.y2,x1,y1,x2,y2);
		SetViewPort(x1,y1,x2,y2,ClipOn);
	  end;
	  if GrExit then exit;
	end;
{----------------------- nacteni udaju }
  begin
   for i:=1 to max do
	 begin
	   UX[i]:='';UY[i]:=0;UZ[i]:=0;UL[i]:=0;
	   for m:=0 to 9 do U3[m,i]:=0;
	 end;
   CFile:=FileFPtr;
   CrecPtr:=GetRecSpace;
   OpenCreateF(Shared);
   md:=NewLMode(RdMode);
   if PocVet>max then Pocvet:=max;
   if Cfile^.XF=nil then GD^.ViewKey:=nil;
   if (CFile^.XF<>nil)and(GD^.ViewKey=nil) then GD^.ViewKey:=Cfile^.Keys;
{ sort pro lin a LP}
   if not ((Typ='Lin')or(Typ='L3')or(Typ='LP')or(Typ='LP3')) then
     SK:=nil
   else
	 if (GD^.ViewKey=nil)or(GD^.ViewKey^.KFlds^.FldD<>UdajXPtr) then
		begin SK:=GetZStore(sizeof(SK^)); SK^.FldD:=UdajXPtr; end
	else
	  SK:=nil;
   New(Scan,Init(CFile,GD^.ViewKey,GD^.KeyIn,false));
   Bool:=RunEvalFrml(GD^.Cond);
   Scan^.ResetSort(SK,Bool,md,GD^.SQLFilter);

   Isdata:=false;
   i:=0;
   if (not Scan^.EOF) and (StartVeta>1) then
	 repeat
       Scan^.GetRec;
       if (not Scan^.EOF) and RunBool(Bool) then i:=i+1;
     until (Scan^.EOF)or(i>=StartVeta-1);
   i:=0;
   if not Scan^.EOF then
   repeat
	 Scan^.GetRec;
	 if (not Scan^.EOF) and RunBool(Bool) then
	  begin
	   i:=i+1;
	   Isdata:=true;
	   if (Typ[1]<>'L') then
        begin
         if UdajXPtr^.Typ='F' then str(_R(UdajXPtr):UdajXPtr^.L:UdajXPtr^.M,UX[i]);
         if UdajXPtr^.Typ='D' then UX[i]:=StrDate(_R(UdajXPtr),FieldDMask(UdajXPtr)^);
         if UdajXPtr^.Typ='B' then if _B(UdajXPtr) then UX[i]:='Y' else UX[i]:='N';
		 if UdajXPtr^.Typ='A' then UX[i]:=_ShortS(UdajXPtr);
         if UdajXPtr^.Typ='N' then UX[i]:=_ShortS(UdajXPtr);
        end
       else
        begin
         if UdajXPtr^.Typ='F' then UL[i]:=_R(UdajXPtr);
         if UdajXPtr^.Typ='D' then UL[i]:=_R(UdajXPtr);
		 if UdajXPtr^.Typ='N' then val(_ShortS(UdajXPtr),UL[i],ch);
		end;
	   if not((Typ='DS')or(Typ='QS')or(Typ='DL')or(Typ='QL')or(Typ='SB')) then
		 begin
		   if (UdajYPtr^.Typ='A')or(UdajYPtr^.Typ='N') then
			 begin
			   val(_ShortS(UdajYPtr),UY[i],ch);
			   if ch<>0 then UY[i]:=0;
             end;
           if (UdajYPtr^.Typ='B') then if _B(UdajYPtr) then UY[i]:=1 else UY[i]:=0;
		   if (UdajYPtr^.Typ='F') or (UdajYPtr^.Typ='D') then UY[i]:=_R(UdajYPtr);
           if UdajZ<>'' then
             if (UdajZPtr^.Typ='A')or(UdajZPtr^.Typ='N') then
               begin
                 val(_ShortS(UdajZPtr),UZ[i],ch);
                 if ch<>0 then UZ[i]:=0;
               end;
             if (UdajZPtr^.Typ='B') then if _B(UdajZPtr) then UZ[i]:=1 else UZ[i]:=0;
			 if (UdajZPtr^.Typ='F') or (UdajZPtr^.Typ='D') then UZ[i]:=_R(UdajZPtr);
		 end
		else
         begin
          for m:=0 to PocZ do
		   begin
             if (Udaj3Ptr[m]^.Typ='A')or(Udaj3Ptr[m]^.Typ='N') then
			   begin
				 val(_ShortS(Udaj3Ptr[m]),U3[m,i],ch);
				 if ch<>0 then U3[m,i]:=0;
               end;
             if (Udaj3Ptr[m]^.Typ='B') then if _B(Udaj3Ptr[m]) then U3[m,i]:=1 else U3[m,i]:=0;
             if (Udaj3Ptr[m]^.Typ='F') or (Udaj3Ptr[m]^.Typ='D') then U3[m,i]:=_R(Udaj3Ptr[m]);
           end;
           if Udaj3Ptr[0]=nil then
            begin
             if (UdajZPtr^.Typ='A')or(UdajZPtr^.Typ='N') then
			   begin
				 val(_ShortS(UdajZPtr),U3[0,i],ch);
                 if ch<>0 then U3[0,i]:=0;
			   end;
             if (UdajZPtr^.Typ='B') then if _B(UdajZPtr) then U3[0,i]:=1 else U3[0,i]:=0;
			 if (UdajZPtr^.Typ='F') or (UdajZPtr^.Typ='D') then U3[0,i]:=_R(UdajZPtr);
            end;
         end;
	  end;
   until (Scan^.EOF)or(i=PocVet);
   PocVet:=i;
   Scan^.Close;
   OldLMode(md);
   ReleaseStore(Scan);
   if Pocvet=0 then
	 begin
	   SetMsgPar(CFile^.Name);WrLLf10Msg(107);
	   if Interact then goto 10 else GRExit:=true;
	   exit;
	 end;
  end;
{----------------------- zkraceni UX dle poctu vet }
if Typ[2]='D' then
begin
{ if (PopX='I')or(PopX='i') then for i:=1 to pocvet do UX[i]:=copy(UX[i],1,15);}
 if (PopX='V')or(PopX='v') then
   for i:=1 to pocvet do
	 begin
	   if (GrDriv=VGA) and (ViewPort80x25.y2-ViewPort80x25.y1>20) then UX[i]:=copy(UX[i],1,8) else UX[i]:=copy(UX[i],1,6);
	 end;
 if (PopX='H')or(PopX='h') then for i:=1 to pocvet do UX[i]:=copy(UX[i],1,trunc(64/pocvet));
 if PopX=' ' then
   if pocvet>32 then begin PopX:='V';for i:=1 to pocvet do UX[i]:=copy(UX[i],1,6);end
	 else
	   begin
		 delkax:=0;
		 for i:=1 to pocvet do if length(UX[i])>delkax then delkax:=length(UX[i]);
		 if delkax*pocvet < 64 then PopX:='H'
		   else
			 begin
			   PopX:='I';
			 end;
	   end;
end;
if Typ='Cir' then
 begin
  if pocvet>32 then pocvet:=32;
  for i:=1 to pocvet do UX[i]:=copy(UX[i],1,12);
  if PopX=' ' then PopX:='V';
 end;

if (Typ[1]='L')and(PopX=' ') then PopX:='H';
if (Typ='LP')or(Typ='LP3') then
  if pocvet>32 then pocvet:=32;

if (Typ='DS')or(Typ='DL')or(Typ='QS')or(Typ='QL')or(Typ='SB') then
begin
 if PopX=' ' then
   if pocvet>32 then begin PopX:='V';for i:=1 to pocvet do UX[i]:=copy(UX[i],1,6);end
     else
	   begin
		 delkax:=0;
         for i:=1 to pocvet do if length(UX[i])>delkax then delkax:=length(UX[i]);
		 if delkax*pocvet < 64 then PopX:='H'
           else
			 begin
			   PopX:='I';
               for i:=1 to pocvet do UX[i]:=copy(UX[i],1,15);
			 end;
	   end;
 if (PopX='I')or(PopX='i')or(PopX='V')or(PopX='v') then
   for i:=1 to pocvet do
	if (Typ='QS')or(Typ='QL') then
     begin
       if GrDriv=VGA then UX[i]:=copy(UX[i],1,8) else UX[i]:=copy(UX[i],1,5);
     end
	else
	 begin
       if GrDriv=VGA then UX[i]:=copy(UX[i],1,13) else UX[i]:=copy(UX[i],1,8);
     end;
 if (PopX='H')or(PopX='h') then for i:=1 to pocvet do UX[i]:=copy(UX[i],1,trunc(64/pocvet));
end;

{------------------- Konec cteni - veskere udaje nacteny (pocvet,PocZ) }
end;

end;

(*procedure GrStop(var Par:Parametry;var GD:GraphDPtr);
var
  naz    : extstr;
begin
  MakeCls:=false;
  with LastViewPort do begin x1:=0;y1:=0;x2:=GetMaxX;y2:=GetMaxY;end;
  GetViewSettings(ViewPort);
	  with Par do begin
		  if GD^.WW<>nil then with GD^.WW^ do
			begin
			  with ViewPortGD do RunWFrml(W,WFlags,ViewPortGD);
			  MakeCls:=(WFlags and WNoClrScr)=0;
			if (WFlags and WHasFrame)=0 then Border:='N' else
			if (WFlags and WDoubleFrame)=0 then Border:=' ' else Border:='=';
			HeadFrame:=RunShortStr(Top);  WWShadow:=(WFlags and WShadow)<>0;
			  if (WFlags and WHasFrame)=0 then begin Border:='N';psx:=0;psy:=0;end else
			  if (WFlags and WDoubleFrame)=0 then begin Border:=' ';psx:=2;psy:=2;end else
				begin Border:='=';psx:=4;psy:=4;end;
			  if ColFrame<>nil then
				begin naz:=RunShortStr(ColFrame);ColorWWFrame:=naz[1];end else ColorWWFrame:=BarvyPasDef[Colors.uNorm];
			  if ColBack<>nil then
				begin naz:=RunShortStr(ColBack);ColorWWBack:=naz[1];end else ColorWWBack:='L';
			  HideMouse;
			  with ViewPort do
			  begin
				RectToPix(ViewPort80x25.x1,ViewPort80x25.y1,
				  ViewPort80x25.x2,ViewPort80x25.y2,x1,y1,x2,y2);
				SetViewPort(x1,y1,x2,y2,ClipOn);
				if Border<>'N' then ShowFrame(Border);
				if MakeCls then ClrScrG(Barvy);
				HeadFrame:=RunShortStr(Top);
				{ if WWShadow then ShowShadow; }
			  end;
			end;

		  TextW;
		  Stop(Par);
		  DelAllParWInTWork;
	{	  if MakeCls then ClrScrG(Barvy);}
		  RestorePalette;
		  ShowMouse;
		end;
end;*)

procedure ViewPCX(var Par:Parametry;var GD:GraphDPtr);
var
  fp:file;
  OldPalette : PaletteType ;
  i      : integer;
  Reg    : Registers;
  GraphError,k : integer;
  naz    : extstr;
  j:byte;
  tmp : char;
  p,ErrorCode:word;
  h,m,s,t:word;s1,s2:longint;

label 17;
begin
  with LastViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
	  with Par do begin
                if NoPCX then goto 17;
		filePCX:=GFpath; {RunShortStr(GD^.GF);}
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
			   exit;
			end;

17:		  Reg.AH:=$0F;
		  Intr($10,Reg);

                  GMaxX:=GetMaxX; GMaxY:=GetMaxY; {28.12.95 MK}
		  smallPCX:=false;
                  GraphError:=0;
		  if GD^.WW<>nil then with GD^.WW^ do
			begin
			  smallPCX:=true;

			  MakeCls:=(WFlags and WNoPop)=0;
			  {with ViewPortGD do} RunWFrml(W,WFlags,ViewPortGD);
			  if (WFlags and WHasFrame)=0 then begin Border:='N';psx:=0;psy:=0;end else
			  if (WFlags and WDoubleFrame)=0 then begin Border:=' ';psx:=3;psy:=2;end else
				begin Border:='=';psx:=5;psy:=4;end;
			  if ColFrame<>nil then
				begin naz:=RunShortStr(ColFrame);ColorWWFrame:=naz[1];end else ColorWWFrame:=BarvyPasDef[Colors.uNorm];
			  if ColBack<>nil then
				begin naz:=RunShortStr(ColBack);ColorWWBack:=naz[1];end else ColorWWBack:='L';
			  WWShadow:=(WFlags and WShadow)<>0;
			  HideMouse;
			  HeadFrame:=RunShortStr(GD^.WW^.Top);
			  with ViewPort do
				RectToPix(ViewPort80x25.x1,ViewPort80x25.y1,
							ViewPort80x25.x2,ViewPort80x25.y2,x1,y1,x2,y2);

			  if (WFlags and WNoClrScr)=0 then ClrScrG(Par.Barvy); {opraveno 29.11.94}
			  ShowFrame(Border);
			  with ViewPort do if not NoPCX then GraphError:=ShowPCXAt(filePCX,x1,y1,x2,y2);
			end
		   else
                        begin
      			  if not NoPCX then GraphError:=ShowPCX(filePCX);
                          setviewport(0,0,GetMaxX,GetMaxY,true);
                        end;

		  {$I-}
		  if not NoPCX then close(fp);
		  {$I+}
		  with {Last} ViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);

		  if GraphError=0 then
			begin
			  { OK obr. je nakreslen }

{             test ulozeni okna
			  beep;
			  SWPtr:=PushWinToTWork(0,0,Getmaxx,getmaxy);
			  PopWinFromTWork(SWPtr);
			  ReleaseStore(SWPtr);
			  beep;readln;}

			  if not IsMode256Col then begin
                                if NoPCX then
                                  begin
                                    if not SmallPCX then setviewport(0,0,GetMaxX,GetMaxY,true);
				    TextW;	   {28.12.95 MK}
                                  end;
	  			tmp:=Par.Color[1];         {01.07.94 Skacel, nastaveni parametru }
				Par.Color[1]:='C';
				Stop(Par);     {Par.Color[1]:='C' aby se pri tisku prevadely
							   {barvy na stupne sedi}
				Par.Color[1]:=tmp;
			  end
			  else begin
				{repeat until readkbd=_ESC_ ;}
  if (upcase(Print[1])='P')and(upcase(Print[2])='A')and(upcase(Print[3])='U') then
	begin
	  val(copy(Print,4,2),p,ErrorCode);
	  if ErrorCode<>0 then val(copy(Print,4,1),p,ErrorCode);
	  if (ErrorCode=0)and(P>=0)and(P<=99) then
	   begin
             gettime(h,m,s,t);s1:=h*360000+m*6000+s*100+t;
                repeat
			if keypressed then
			  begin
				key:=readkey;
				if key=27 then
                                  begin
                                    Reg.AH:=$00;
		                    Intr($10,Reg);
                                    exit;
                                  end;
				repeat sound(500);delay(10);nosound;delay(1200) until keypressed;
				key:=readkey;
				if key=27 then
                                  begin
                                    Reg.AH:=$00;
		                    Intr($10,Reg);
                                    exit;
                                  end;
			  end;
                  gettime(h,m,s,t);s2:=h*360000+m*6000+s*100+t;
                until (s2>s1+p*100) or (s2<s1);
                Reg.AH:=$00;
		Intr($10,Reg);
                exit;
           end;
        end;
				Reg.AH:=$00;
				Intr($10,Reg);
			  end;                   {23.11.94 MK bylo nutno 2x Esc}
			  DelAllParWInTWork;
{			  if MakeCls then ClearDevice; ClrScrG(Barvy);}
			  ShowMouse;
			  RestorePalette;
			  exit;
			end
		   else
			begin
{			  if MakeCls then ClearDevice;ClrScrG(Barvy);}
			  RestorePalette;
			  ShowMouse;
			  str(GraphError:1,naz);SetMsgPar(naz);WrLLf10Msg(203);
			 end;

		end;
end;

procedure ViewPCXInteract(var Par:Parametry;var filePCX:string);
label ex;
var
  fp:file;
  OldPalette : PaletteType ;
  i      : integer;
  Reg    : Registers;
  GraphError,k : integer;
  naz    : extstr;
  j:byte;
  tmp : char;
  p,ErrorCode:word;
  sc:longint;
  p1:pointer;

begin
  MarkStore(p1);
  sc:=PushW1(1,1,TxtCols,TxtRows,true,true);
  with LastViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
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
	     goto ex;
	  end;

    Reg.AH:=$0F;
    Intr($10,Reg);

    smallPCX:=false;
    GraphError:=ShowPCX(filePCX);

    {$I-}
    close(fp);
    {$I+}

    with {Last} ViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);

    if GraphError=0 then
	  begin
	    if not IsMode256Col then begin
		  {TextW;}	   {01.07.94 Skacel, nastaveni parametru }
		  tmp:=Par.Color[1];
		  Par.Color[1]:='C';
		  Stop(Par);     {Par.Color[1]:='C' aby se pri tisku prevadely
					     {barvy na stupne sedi}
		  Par.Color[1]:=tmp;
	    end
	    else begin
		  {repeat until readkbd=_ESC_ ;}
		  Reg.AH:=$00;
		  Intr($10,Reg);
	    end;                   {23.11.94 MK bylo nutno 2x Esc}
	    DelAllParWInTWork;
	    ShowMouse;
	    RestorePalette;
	    goto ex;
	  end
     else
	  begin
	    RestorePalette;
	    ShowMouse;
	    str(GraphError:1,naz);SetMsgPar(naz);WrLLf10Msg(203);
	   end;
ex:
  PopW2(sc,true);
  ReleaseStore(p1);
end;

procedure ViewRGB(var Par:Parametry);
var Scr:array [1..4] of PointType;
	R,G,B:integer;
    key:word;
function s(R:integer;C:extstr):pathstr;
var ps:extstr;
begin
  Str(R:3,ps);
  s:=C+ps;
end;
begin
  CrsHide;
  SetTextStyle(DefaultFont,HorizDir,K(1));
  if not Par.Interact then Poprve:=false;
  if GRexit then exit;
  with LastViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
  Scr[1].x:=0;Scr[1].y:=0;
  Scr[2].x:=GetMaxX;Scr[2].y:=0;
  Scr[3].x:=GetMaxX;Scr[3].y:=GetMaxY * 8 div 10;
  Scr[4].x:=0;Scr[4].y:=GetMaxY * 8 div 10;
  SetFillStyle(1,1);
  SetRgbPalette(1,0,0,0);
  FillPoly(4,Scr);
  R:=0;G:=0;B:=0;
  Scr[1].x:=0;Scr[1].y:=GMaxY * 8 div 10;
  Scr[2].x:=GMaxX;Scr[2].y:=GMaxY * 8 div 10;
  Scr[3].x:=GMaxX;Scr[3].y:=GMaxY;
  Scr[4].x:=0;Scr[4].y:=GMaxY;
  SetFillStyle(1,0);
  FillPoly(4,Scr);
  OutTextXYC(GMaxX*8 div 10,round(GMaxY*8.5) div 10,chr(24)+'  4  5  6');
  OutTextXYC(GMaxX*8 div 10,GMaxY*9 div 10,  '   R  G  B');
  OutTextXYC(GMaxX*8 div 10,round(GMaxY*9.5) div 10,chr(25)+'  1  2  3');
  OutTextXYC(GMaxX*1 div 12,GMaxY*9 div 10,s(R,'R:'));
  OutTextXYC(GMaxX*4 div 12,GMaxY*9 div 10,s(G,'G:'));
  OutTextXYC(GMaxX*7 div 12,GMaxY*9 div 10,s(B,'B:'));
  SetRgbPalette(2,0,0,0);
  repeat
    key:=readkey;
    SetColor(2);
	OutTextXYC(GMaxX*1 div 12,GMaxY*9 div 10,s(R,'R:'));
	OutTextXYC(GMaxX*4 div 12,GMaxY*9 div 10,s(G,'G:'));
	OutTextXYC(GMaxX*7 div 12,GMaxY*9 div 10,s(B,'B:'));
	SetColor(15);
    case key of
     ord('1'):if R>0 then R:=R-1;
     ord('2'):if G>0 then G:=G-1;
     ord('3'):if B>0 then B:=B-1;
     ord('4'):if R<100 then R:=R+1;
     ord('5'):if G<100 then G:=G+1;
     ord('6'):if B<100 then B:=B+1;
    end;
	OutTextXYC(GMaxX*1 div 12,GMaxY*9 div 10,s(R,'R:'));
	OutTextXYC(GMaxX*4 div 12,GMaxY*9 div 10,s(G,'G:'));
	OutTextXYC(GMaxX*7 div 12,GMaxY*9 div 10,s(B,'B:'));

    SetRgbPalette(1,round(r*63/100),round(g*63/100),round(b*63/100));
  until key=_ESC_;
  with ViewPort do SetViewPort(x1,y1,x2,y2,ClipOn);
end;

procedure RestorePalette;
var i:integer;
    REGS : Registers;
begin
  if IsDACReg  then begin
    REGS.AX:=$1012;
    REGS.BX:=0;          { obnovit paletu 256 barev }
    REGS.CX:=256;        { pocet registru }
    REGS.ES:=SEG(DACReg);
	REGS.DX:=OFS(DACReg);
    INTR($10,REGS);
  end;
  SetAllPalette(OldPalette);         { obnovit paletu barev }
end;

end.
