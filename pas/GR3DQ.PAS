unit Gr3DQ;

{$A-,B-,D+,F+,G-,I+,L+,O+,R-,S+,V+,X+}

interface
  uses GrGlob;
  procedure OsyQ(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure SloupQS(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure SloupQL(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);

implementation
  uses dos,graph;


procedure OsyQ(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);

var
  p,X,Y,XM,YM,x0,y0,xd,yd,xz,yz,i:integer;
  dil,MaximumC:real;
  MaxStr:string[14];
  NazevP:string[80];

procedure DilY(Y:integer;Hodnota:str9);
    begin
      SetLineStyle(1,0,1);
      Line(XM div 11,Y,XM div 10,Y);
      Line(x0,y,x0+xz,y-yz);
      Line(x0+xz,y-yz,x0+xz+xd+3,y-yz);
      SetLineStyle(0,0,1);
      SetTextJustify(RightText,CenterText);
      SetTextStyle(SmallFont,HorizDir,K(4));
      OutTextXYC(XM div 12,Y,Hodnota);
      SetTextJustify(leftText,CenterText);
      OutTextXYC(x0+xd+xz+xd div 20,Y-yz,Hodnota);
      SetTextJustify(RightText,CenterText);
    end; {DilY}

begin
  with Param,Par3,Maxim do
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
  x0:=XM div 10;
  y0:=YM - round(YM/8);
  xd:=round(XM/20*11);
  yd:=round(YM/10*4);
  xz:=round(xd/3);
  yz:=round(yd/1.3);
  Line(x0,y0,x0,y0-yd); { osa Y }
  if minimum>=0 then Line(x0,y0,x0+xd,y0); { osa X }
  Line(x0,y0,x0+xz,y0-yz); { osa Z }
  Line(x0+xz,y0-yz,x0+xz,y0-yd-yz);
  Line(x0+xz,y0-yz,x0+xd+xz,y0-yz);
  Line(x0,y0-yd,x0+xz,y0-yz-yd);
  if minimum>=0 then Line(x0+xd,y0,x0+xz+xd,y0-yz);
  Line(x0+xz,y0-yz-yd,x0+xz+xd,y0-yz-yd);
  Line(x0+xz+xd,y0-yz,x0+xz+xd,y0-yz-yd);
{----------------------- nazev X }
  if (PopX='H')or(PopX='h') then
    begin
      SetTextJustify(RightText,CenterText);
      SetTextStyle(SmallFont,HorizDir,K(6));
      OutTextXYC(XM-XM div 5,YM-YM div 20,NazevX);
    end
   else
    begin
	  SetTextJustify(CenterText,LeftText);
	  SetTextStyle(SmallFont,VertDir,K(6));
	  OutTextXYC(XM-round(XM/15),YM-YM div 25,NazevX);
	end;
{----------------------- nazev Y }
    begin
	  SetTextJustify(CenterText,RightText);
	  SetTextStyle(SmallFont,VertDir,K(6));
	  OutTextXYC(XM div 40,round(YM/10*4),NazevY);
	end;
{----------------------- dilky Y *******}
 if Minimum>=0 then
  {----------------------- uprava maxima pro zadane minimum }
  if (MinimumVst>0)and(Maximum>0) then
    begin
     MinimumC:=MinimumVst*MaximumC/Maximum;
     MinimumVstP:=MinimumVst;
     Maximum:=Maximum-MinimumVst;
     for i:=0 to 9 do
       for p:=1 to PocVet do U3[i,p]:=U3[i,p]-MinimumVst;
	 dil:=yd/10;
	 for p:= 0 to 10 do
	  begin
	   if Maximum>=10 then
         str((MaximumC-p*(MaximumC-MinimumC)/10):5:0,MaxStr)
        else
         str((MaximumC-p*(MaximumC-MinimumC)/10):4:1,MaxStr);
       if Maximum<1 then
         str((MaximumC-p*(MaximumC-MinimumC)/10):3:2,MaxStr);
       DilY(round(y0 - yd + p*dil),MaxStr);
      end;
    end
  else
  begin
   dil:=yd/10;
   for p:= 0 to 10 do
     begin
       if Maximum>=10 then
         str((MaximumC-p*MaximumC/10):5:0,MaxStr)
        else
		 str((MaximumC-p*MaximumC/10):4:1,MaxStr);
	   if Maximum<1 then
		 str((MaximumC-p*MaximumC/10):3:2,MaxStr);
	   DilY(round(y0 - yd + p*dil),MaxStr);
     end;
   end
  else
  begin
   dil:=yd/10;
   for p:= 0 to 10 do
     begin
	   if Maximum>=10 then
		 str((MaximumC-p*MaximumC/5):5:0,MaxStr)
		else
		 str((MaximumC-p*MaximumC/5):4:1,MaxStr);
	   if Maximum<1 then
		 str((MaximumC-p*MaximumC/5):3:2,MaxStr);
	   DilY(round(y0-yd + p*dil),MaxStr);
	   if p=5 then
		begin
		 SetLineStyle(0,0,3);
		 Line(x0,round(y0-yd + p*dil),x0+xd,round(y0-yd + p*dil));
		 Line(x0+xd,round(y0-yd + p*dil),x0+xd+xz,round(y0-yd + p*dil-yz));
		 Line(x0,round(y0-yd + p*dil),x0+xz,round(y0-yd + p*dil)-yz);
		 Line(x0+xz,round(y0-yd + p*dil)-yz,x0+xz+xd+3,round(y0-yd + p*dil)-yz);
		 SetLineStyle(0,0,1);
		end;
	 end;
   end;
{----------------------- rastr
  if (Rastr='Y')or(Rastr='y') then
	begin
	  SetLineStyle(1,0,1);
	  if Minimum>=0 then
		for p:=0 to 9 do
		  Line(XM div 10,round(YM/10*3 + p*dil),XM - XM div 4,round(YM/10*3 + p*dil))
		 else
		  for p:=0 to 19 do
			Line(XM div 10,round(YM/10*3 + p*dil),XM - XM div 4,round(YM/10*3 + p*dil));
	  SetLineStyle(0,0,1);
	end;
 ----------------------- tisk koeficientu Y }
  if MaxExp<>0 then
	begin
     if MaxExp>0
      then
       if MaxExp<9 then       {a}
        begin
         str(exp(MaxExp*ln(10)):6:0,MaxStr);
         MaxStr:='x'+MaxStr;
         OutTextXYC(XM div 10,round(YM/2.8),MaxStr);
        end
       else
        begin
         str(exp(MaxExp*ln(10)):9:0,MaxStr);
         MaxStr:='x'+MaxStr;
         OutTextXYC(XM div 10,round(YM/2.8),MaxStr);
        end                   {a}
      else
       if MaxExp>-6 then      {b}
        begin
		  str(exp(MaxExp*ln(10)):1:5,MaxStr);
          MaxStr:='x'+MaxStr;
		  OutTextXYC(XM div 10,round(YM/2.8),MaxStr);
        end
       else
        begin
          str(exp(MaxExp*ln(10)):1:12,MaxStr);
          MaxStr:='x'+MaxStr;
          OutTextXYC(XM div 8,round(YM/2.8),MaxStr);
        end                    {b}
    end;
    SetColor(LastColor);

 end; {OsyQS}
end;
procedure SloupQS(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
var
  D,A,B,G:real;
  XM,YM,X1,X2,Y1,YC,Y2,i,C,E:integer;
  x0,y0,xd,yd,xz,yz:word;

procedure Bar3C(x1,y1,x2,y2,z:word;top:boolean;m:byte);
  var
	qad : qadr;
    ColInf : FillSettingsType;
    Xasp,Yasp,xp,yp,xs,ypol:word;

  begin
    if y1<y2 then begin qad[1].y:=y1;y1:=y2;y2:=qad[1].y;end; {setrideni}
    if x1>x2 then begin qad[1].x:=x1;x1:=x2;x2:=qad[1].x;end;
    GetAspectRatio(Xasp,Yasp);

	xp:=round(int((x2-x1)/2));     {polomer x}
	yp:=round((z/2)*(Xasp/Yasp));; {posunuti z}
	xs:=xp+round(z*0.8/2);         {posunuti stredu v ose x}
	ypol:=round(xp/2*(Xasp/Yasp)); {polomer y}
	FillEllipse(x1+xs,y1-yp,xp,ypol);
	qad[1].x:=x1+xs-xp;
	qad[1].y:=y1-yp;
	qad[2].x:=x2+xs-xp;
	qad[2].y:=y1-yp;
	qad[3].x:=x2+xs-xp;
	qad[3].y:=y2-yp;
	qad[4].x:=x1+xs-xp;
	qad[4].y:=y2-yp;
	FillPoly(4,qad);
	if upcase(ColorWWFor)='L' then
	 begin
	  line(qad[2].x,qad[2].y,qad[3].x,qad[3].y);
	  line(qad[1].x,qad[1].y,qad[4].x,qad[4].y);
	 end;
	LastColor:=GetColor;
	if (param.Color[1]='C') or (param.Color[1]='M') then SetColor(co((m mod 14)+2,param.barvy));
	if param.Color[1]='F' then SetColor(15);
	if param.Color[1]='N' then SetColor(KonB(ColorWWBack));
	if param.Color[1]='O' then SetColor(co(2,param.Barvy));
	SetLineStyle(0,0,1);
	Line(x1+1+xs-xp,y1-yp,x2-1+xs-xp,y1-yp);
	SetColor(LastColor);
	FillEllipse(x1+xs,y2-yp,xp,ypol);
  end;

procedure Bar3P(x1,y1,x2,y2,z,zx:word;top:boolean);
  var
	qad : array [1..3] of PointType;
	ColInf : FillSettingsType;
	Xasp,Yasp,xp,yp,x3,y3:word;

  begin
	GetAspectRatio(Xasp,Yasp);
	xp:=zx;
	yp:=z;
	x3:=x1+round((x2-x1)/2);
	y3:=y2-round(z/2);


	qad[1].x:=x1;
	qad[1].y:=y1;
	qad[2].x:=x1+xp;
	qad[2].y:=y1-yp;
	qad[3].x:=x3 + xp div 2;
	qad[3].y:=y3;
	FillPoly(3,qad);
	if upcase(ColorWWFor)='L' then
	 begin
	  line(qad[1].x,qad[1].y,qad[2].x,qad[2].y);
	  line(qad[2].x,qad[2].y,qad[3].x,qad[3].y);
	  line(qad[3].x,qad[3].y,qad[1].x,qad[1].y);
	 end;

	qad[1].x:=x1+xp;
	qad[1].y:=y1-yp;
	qad[2].x:=x2+xp;
	qad[2].y:=y1-yp;
	qad[3].x:=x3 + xp div 2;
	qad[3].y:=y3;
	FillPoly(3,qad);
	if upcase(ColorWWFor)='L' then
	 begin
	  line(qad[1].x,qad[1].y,qad[2].x,qad[2].y);
	  line(qad[2].x,qad[2].y,qad[3].x,qad[3].y);
	  line(qad[3].x,qad[3].y,qad[1].x,qad[1].y);
	 end;

	qad[1].x:=x1;
	qad[1].y:=y1;
	qad[2].x:=x2;
	qad[2].y:=y1;
	qad[3].x:=x3 + xp div 2;
	qad[3].y:=y3;
	FillPoly(3,qad);
	if upcase(ColorWWFor)='L' then
	 begin
	  line(qad[1].x,qad[1].y,qad[2].x,qad[2].y);
	  line(qad[2].x,qad[2].y,qad[3].x,qad[3].y);
	  line(qad[3].x,qad[3].y,qad[1].x,qad[1].y);
	 end;

	qad[1].x:=x2;
	qad[1].y:=y1;
	qad[2].x:=x2+xp;
	qad[2].y:=y1-yp;
	qad[3].x:=x3 + xp div 2;
	qad[3].y:=y3;
	FillPoly(3,qad);
	if upcase(ColorWWFor)='L' then
	 begin
	  line(qad[1].x,qad[1].y,qad[2].x,qad[2].y);
	  line(qad[2].x,qad[2].y,qad[3].x,qad[3].y);
	  line(qad[3].x,qad[3].y,qad[1].x,qad[1].y);
	 end;
  end;


begin
  with Param,Par3,Maxim do
 begin
  if pocvet>24 then Rastr:='';
  SetColor(KonB(ColorWWFor));

  XM:=GMaxX;
  YM:=GMaxY;
  x0:=XM div 10;
  y0:=YM - round(YM/8);
  xd:=round(XM/20*11);
  yd:=round(YM/10*4);
  xz:=round(xd/3);
  yz:=round(yd/1.3);
  SetTextJustify(LeftText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetFillStyle(1,1);
  for i:=1 to Pocvet do
  begin
            A:= xd/pocvet*(100-Pomer)/100;          {sirka mezery}
            B:= xd/pocvet-A;                        {sirka jedne vety}
            C:= round(yz/(PocZ+1)*Pomer/100);       {velikost sloupce v ose z}
            D:= xz/(PocZ+1)*Pomer/100;              {posunuti v ose x}
            E:= round(yz/(PocZ+1)*(100-Pomer)/100); {velikost mezery v ose z}
            G:= xz/(PocZ+1)*(100-Pomer)/100;        {velikost mezery v ose X}
   if Minimum>=0 then
     begin
       for m:=PocZ downto 0 do
		begin
			X1:=round(x0 + A/2 + xz - (PocZ-m)*xz/(PocZ+1) + (i-1)*(A+B) - D - G/2);
			X2:=round(x0 + A/2 + xz - (PocZ-m)*xz/(PocZ+1) + (i-1)*(A+B) - D - G/2 + B);
			YC:=yd;
			Y1:=round(y0 - 1 + E/2 - yz + (PocZ-m)*yz/(PocZ+1) + C) ;
			Y2:=round(y0 - 1 + E/2 - yz + (PocZ-m)*yz/(PocZ+1) -YC*U3[m,i]/maximum + C);
		  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
		  if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
		  if Color[1]='M'then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
		  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
		  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
		  if (Color[2]='3')or(Color[2]='')or(length(Color)=1) then
			bar3Q(X1,Y1,X2,Y2,C,round(D),true);
		  if Color[2]='1' then
			bar3C(X1,Y1,X2,Y2,C,true,m);
		  if Color[2]='2' then
			bar3P(X1,Y1,X2,Y2,C,round(D),true);
		  if Rastr='H' then hod(X1+round(D/2)+round((X2-X1)/2),Y2-round(C/1.5),U3[m,i],MaximumC,MaxExp);
		end;
     end;
   if Minimum<0 then
     begin
       for m:=PocZ downto 0 do
        begin
            X1:=round(x0 + A/2 + xz - (PocZ-m)*xz/(PocZ+1) + (i-1)*(A+B) - D - G/2);
            X2:=round(x0 + A/2 + xz - (PocZ-m)*xz/(PocZ+1) + (i-1)*(A+B) - D - G/2 + B);
            YC:=round(yd/2);
			Y1:=round(y0 - YC + E/2 - 1 - yz + (PocZ-m)*yz/(PocZ+1) + C) ;
			Y2:=round(y0 - YC + E/2 - 1 - yz + (PocZ-m)*yz/(PocZ+1) -YC*U3[m,i]/maximum + C);
		  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
		  if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
		  if Color[1]='M'then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
		  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
		  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
		  if (Color[2]='3')or(Color[2]='')or(length(Color)=1) then
		   begin
			bar3Q(X1,Y1,X2,Y2,C,round(D),true);
			if Rastr='H' then
			  if U3[m,i]>=0 then
				hod(X1+round(D/2)+round((X2-X1)/2),Y2-round(C/1.5),U3[m,i],MaximumC,MaxExp)
			   else
				hod(X1+round((X2-X1)/2),Y2-round(C/2),U3[m,i],MaximumC,MaxExp);
		   end;
		  if Color[2]='1' then
		   begin
			bar3C(X1,Y1,X2,Y2,C,true,m);
			if Rastr='H' then
			hod(X1+round(D/2)+round((X2-X1)/2),Y2-round(C/1.5),U3[m,i],MaximumC,MaxExp)
		   end;
		  if Color[2]='2' then
		   begin
			bar3P(X1,Y1,X2,Y2,C,round(D),true);
			if Rastr='H' then
			  if U3[m,i]>=0 then
				hod(X1+round(D/2)+round((X2-X1)/2),Y2-round(C/1.5),U3[m,i],MaximumC,MaxExp)
			   else
				hod(X1+round(D/2)+round((X2-X1)/2),Y2-round(C/3),U3[m,i],MaximumC,MaxExp);
		   end;
		  SetLineStyle(0,0,3);
		  Line(x0,y0-YC,x0+xd,y0-YC);
		  Line(x0+xd,y0-YC,x0+xd+xz{ div 2},y0-YC-yz{ div 2});
		  SetLineStyle(0,0,1);
		end;
	 end;
	   LastColor:=GetColor;
	   SetColor(KonB(ColorWWFor));
	   if (PopX='H')or(PopX='h') then
        begin
         Y1:=YM-round(YM/12);
         SetTextStyle(SmallFont,HorizDir,K(4));
		 SetTextJustify(CenterText,CenterText);
		 OutTextXYC(X1+round(B/2),Y1,UX[i]);
        end;
        if (PopX='V')or(PopX='v')or(PopX='I')or(PopX='i') then
          begin
           Y1:=YM-round(YM/9);
           SetTextJustify(CenterText,RightText);
           SetTextStyle(SmallFont,VertDir,K(4));
           OutTextXYC(X1+round(B/2),Y1-2,UX[i]);
        end;

    end;
       for m:=PocZ downto 0 do                  {popis osy z}
        begin
			X1:=round(x0+xd + xz - (PocZ-m)*xz/(PocZ+1));
            Y1:=round(y0 - yz +E/2 + (PocZ-m)*yz/(PocZ+1) + C) ;
            SetTextJustify(LeftText,CenterText);
			SetTextStyle(SmallFont,HorizDir,K(4));
            OutTextXYC(X1,Y1,Nazev3[m]);
		end;
       SetColor(LastColor);
 end; { SloupQS }
end;
procedure SloupQL(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
var
  A,B:real;
  XMp,YMp:real;
  XM,YM,X1,X2,Y1,YC,Y2,Y3,Y4,Y5,i,C,D,E,G:integer;
  x0,y0,xd,yd,xz,yz,F:word;

procedure Bar3L(x1,y1,x2,y2,y3,z,zx:word;top:boolean);
  var
	qad : qadr;
    ColInf : FillSettingsType;
	xp,yp:word;

  begin
	xp:=zx;
    yp:=z;

    if Param.Color[2]='3' then
     begin
      qad[1].x:=x1;
      qad[1].y:=y1;
      qad[2].x:=x2;
      qad[2].y:=y1;
      qad[3].x:=x2+xp;
      qad[3].y:=y1-yp;
      qad[4].x:=x1+xp;
      qad[4].y:=y1-yp;
	  FillPoly(4,qad);
      BarFrame(qad);
     end;

    qad[1].x:=x2;
	qad[1].y:=y3;
    qad[2].x:=x2+xp;
    qad[2].y:=y3-yp;
    qad[3].x:=x1+xp;
    qad[3].y:=y2-yp;
    qad[4].x:=x1;
    qad[4].y:=y2;
    FillPoly(4,qad);
    BarFrame(qad);

    if Param.Color[2]='3' then
     begin
      qad[1].x:=x1;
	  qad[1].y:=y1;
      qad[2].x:=x2;
      qad[2].y:=y1;
      qad[3].x:=x2;
      qad[3].y:=y3;
	  qad[4].x:=x1;
      qad[4].y:=y2;
      FillPoly(4,qad);
      BarFrame(qad);

      qad[1].x:=x2;
      qad[1].y:=y1;
      qad[2].x:=x2+xp;
      qad[2].y:=y1-yp;
      qad[3].x:=x2+xp;
      qad[3].y:=y3-yp;
      qad[4].x:=x2;
      qad[4].y:=y3;
	  FillPoly(4,qad);
      BarFrame(qad);
     end;
  end;

procedure Bar3U(x1,x2,y2,y3,y4,y5,z,zx:word;top:boolean);
  var
    qad : qadr;
    ColInf : FillSettingsType;
    xp,yp:word;

  begin
    xp:=zx;
    yp:=z;

    qad[1].x:=x1;
    qad[1].y:=y2;
    qad[2].x:=x2;
	qad[2].y:=y3;
    qad[3].x:=x2-xp;
    qad[3].y:=y5;
    qad[4].x:=x1-xp;
    qad[4].y:=y4;
	FillPoly(4,qad);
    BarFrame(qad);
  end;


begin
  with Param,Par3,Maxim do
 begin
  if pocvet>24 then Rastr:='';
  SetColor(KonB(ColorWWFor));
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  x0:=XM div 10;
  y0:=YM - round(YM/8);
  xd:=round(XM/20*11);
  yd:=round(YM/10*4);
  xz:=round(xd/3);
  yz:=round(yd/1.3);

  SetTextJustify(LeftText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetFillStyle(1,1);
  if Pocvet<2 then exit;
  for i:=1 to Pocvet do
  begin
   if Color[2]='2' then Pomer:=100;
            B:= xd/(pocvet-1);                      {sirka jedne vety}
            C:= round(yz/(PocZ+1)*Pomer/100);       {velikost sloupce v ose z}
            F:= round(yz/(PocZ+1)*Pomer/100);
			D:= round(xz/(PocZ+1)*Pomer/100);       {posunuti v ose x}
            E:= round(yz/(PocZ+1)*(100-Pomer)/100); {velikost zbytku na ose y po sloupec v ose z}
            G:= round(xz/(PocZ+1)*(100-Pomer)/100); {velikost zbytku na ose x po sloupec v ose z}
   if (Minimum>=0)and(i<pocvet) then
     begin
	   for m:=PocZ downto 0 do
		begin
			X1:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ+1) + (i-1)*B - D );
			X2:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ+1) + i*B - D );
			YC:=yd;
			Y1:=round(y0 - 1 + E/2 - yz + (PocZ-m)*yz/(PocZ+1) + C) ;
			Y2:=round(y0 - 1 + E/2 - yz + (PocZ-m)*yz/(PocZ+1) -YC*U3[m,i]/maximum + C);
			Y3:=round(y0 - 1 + E/2 - yz + (PocZ-m)*yz/(PocZ+1) -YC*U3[m,i+1]/maximum + C);
		  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
		  if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
		  if Color[1]='M'then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
		  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
		  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
          if Color[2]='2' then
           begin
            if m>0 then
             begin
              D:= round(xz/PocZ*Pomer/100);       {posunuti v ose x}
              X1:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ-0) + (i-1)*B );
              X2:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ-0) + i*B );
              Y2:=round(y0 - 1 +E/2 - yz + (PocZ-m)*yz/(PocZ-0) -YC*U3[m,i]/maximum );
              Y3:=round(y0 - 1 +E/2 - yz + (PocZ-m)*yz/(PocZ-0) -YC*U3[m,i+1]/maximum );
              Y4:=round(y0 - 1 +E/2 - yz + (PocZ-m+1)*yz/(PocZ-0) -YC*U3[m-1,i]/maximum );
              Y5:=round(y0 - 1 +E/2 - yz + (PocZ-m+1)*yz/(PocZ-0) -YC*U3[m-1,i+1]/maximum );
              if Rastr='H' then
                begin
                  hod(X1,Y2-YM div 30,U3[m,i],MaximumC,MaxExp);
                  if (i=pocvet-1) then
                    hod(X2,Y3-YM div 30,U3[m,i+1],MaximumC,MaxExp);
				end;
                  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
				  if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
				  if Color[1]='M'then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
				  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
				  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
				  Bar3U(X1,X2,Y2,Y3,Y4,Y5,F,D,true);
			   if Rastr='H' then
				  if m=1 then
					begin
					  hod(X1-D,Y4-YM div 30,U3[m-1,i],MaximumC,MaxExp);
					  if (i=pocvet-1) then
						hod(X2-D,Y4-YM div 30,U3[m-1,i+1],MaximumC,MaxExp);
					end;
			 end;
		   end
		   else
			begin
			 Bar3L(x1,y1,x2,y2,y3,F,D,true);
             if Rastr='H' then
               begin
                 hod(X1,Y2-C,U3[m,i],MaximumC,MaxExp);
				 if (i=pocvet-1) then
                   hod(X2,Y3-C,U3[m,i+1],MaximumC,MaxExp);
               end;
            end;
        end;
     end;
   if (Minimum<0)and(i<pocvet) then
     begin
       for m:=PocZ downto 0 do
        begin
            X1:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ+1) + (i-1)*B - D );
            X2:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ+1) + i*B - D );
            YC:=round(yd/2);
            Y1:=round(y0 - YC + E/2 - 1 - yz + (PocZ-m)*yz/(PocZ+1) + C) ;
            Y2:=round(y0 - YC + E/2 - 1 - yz + (PocZ-m)*yz/(PocZ+1) -YC*U3[m,i]/maximum + C);
            Y3:=round(y0 - YC + E/2 - 1 - yz + (PocZ-m)*yz/(PocZ+1) -YC*U3[m,i+1]/maximum + C);
          if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
          if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
		  if Color[1]='M'then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
		  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
		  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
          if Color[2]='2' then
           begin
            if m>0 then
             begin
              D:= round(xz/PocZ*Pomer/100);       {posunuti v ose x}
              X1:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ-0) + (i-1)*B );
              X2:=round(x0 + xz - G/2 - (PocZ-m)*xz/(PocZ-0) + i*B );
              Y2:=round(y0 - YC - 1 +E/2 - yz + (PocZ-m)*yz/(PocZ-0) -YC*U3[m,i]/maximum );
              Y3:=round(y0 - YC - 1 +E/2 - yz + (PocZ-m)*yz/(PocZ-0) -YC*U3[m,i+1]/maximum );
              Y4:=round(y0 - YC - 1 +E/2 - yz + (PocZ-m+1)*yz/(PocZ-0) -YC*U3[m-1,i]/maximum );
              Y5:=round(y0 - YC - 1 +E/2 - yz + (PocZ-m+1)*yz/(PocZ-0) -YC*U3[m-1,i+1]/maximum );
              if Rastr='H' then
                begin
                  hod(X1,Y2-YM div 60,U3[m,i],MaximumC,MaxExp);
                  if (i=pocvet-1) then
                    hod(X2,Y3-YM div 60,U3[m,i+1],MaximumC,MaxExp);
				end;
				  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
				  if Color[1]='F' then SetFillStyle(((m+1) mod 9)+1,15);
				  if Color[1]='M'then SetFillStyle(((m+1) mod 9)+1,co((m mod 14)+2,Barvy));
				  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
				  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
                  Bar3U(X1,X2,Y2,Y3,Y4,Y5,F,D,true);
              if Rastr='H' then
                  if m=1 then
                    begin
                      hod(X1-D,Y4-YM div 60,U3[m-1,i],MaximumC,MaxExp);
                      if (i=pocvet-1) then
                        hod(X2-D,Y4-YM div 60,U3[m-1,i+1],MaximumC,MaxExp);
                    end;
             end;
           end
           else
            begin
             Bar3L(x1,y1,x2,y2,y3,F,D,true);
             if Rastr='H' then
               begin
                 hod(X1,Y2-C,U3[m,i],MaximumC,MaxExp);
				 if (i=pocvet-1) then
                   hod(X2,Y3-C,U3[m,i+1],MaximumC,MaxExp);
               end;
            end;
          SetLineStyle(0,0,3);
          Line(x0,y0-YC,x0+xd,y0-YC);
          Line(x0+xd,y0-YC,x0+xd+xz div 2,y0-YC-yz div 2);
          SetLineStyle(0,0,1);
        end;
     end;
       LastColor:=GetColor;
       SetColor(KonB(ColorWWFor));
       X1:=round(x0 + (i-1)*B);
       if (PopX='H')or(PopX='h') then
        begin
         Y1:=YM-round(YM/12);
         SetTextStyle(SmallFont,HorizDir,K(4));
         SetTextJustify(CenterText,CenterText);
		 OutTextXYC(X1,Y1,UX[i]);
        end;
        if (PopX='V')or(PopX='v')or(PopX='I')or(PopX='i') then
          begin
           Y1:=YM-round(YM/9);
           SetTextJustify(CenterText,RightText);
           SetTextStyle(SmallFont,VertDir,K(4));
           OutTextXYC(X1,Y1-2,UX[i]);
          end;

    end;
       for m:=PocZ downto 0 do                  {popis osy z}
        begin
            X1:=round(x0+xd + xz - (PocZ-m)*xz/(PocZ+1));
            Y1:=round(y0 - yz +E/2 + (PocZ-m)*yz/(PocZ+1) + C) ;
            SetTextJustify(LeftText,CenterText);
            SetTextStyle(SmallFont,HorizDir,K(4));
            OutTextXYC(X1,Y1,Nazev3[m]);
		end;
       SetColor(LastColor);
 end; { SloupQL }
end;

end.
