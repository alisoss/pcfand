unit Gr2D;

{$A-,B-,D-,F+,G-,I+,L-,O+,R-,S+,V+,X+}

interface
  uses GrGlob;
  procedure Sloupce2(var Param:Parametry;var Maxim:Maxima);
  procedure Sloupce3(var Param:Parametry;var Maxim:Maxima);
  procedure Circular(var Param:Parametry;var Maxim:Maxima);
  procedure Linear(var Param:Parametry;var Maxim:Maxima);

implementation
  uses dos,graph,drivers;

procedure Sloupce2(var Param:Parametry;var Maxim:Maxima);
var
  XMp,YMp,A:real;
  XM,YM,X1,X2,Y1,Y2,pol,i:integer;
begin
  with Param,Maxim do
 begin
  if (Pocvet>24)and(Rastr='H') then Rastr:='N';
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  LastColor:=GetColor;
  SetColor(KonB(ColorWWFor));
  SetTextJustify(LeftText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetFillStyle(1,1);
  A:=(XMp-XMp/5)/pocvet*(100-Pomer)/100; {sirka mezery}
  if A>((XMp-XMp/5)/pocvet) then A:=((XMp-XMp/5)/pocvet);
  for i:=1 to PocVet do
  begin

   X1:=round(XMp/10 +A/2 + ((XMp-XMp/5)/pocvet)*(i-1));
   X2:=round(XMp/10 -A/2 + ((XMp-XMp/5)/pocvet)*i);
   Y1:=YM - YM div 8;
   if Minimum>=0 then
	 begin
	  Y2:=round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum);
	  if Color[1]='N'then SetFillStyle(1,KonB(ColorWWBack));
	  if Color[1]='C'then SetFillStyle(1,co((i mod 14)+1,Barvy));
	  if Color[1]='F'then SetFillStyle((i mod 10)+1,15);
	  if Color[1]='O'then SetFillStyle(1,co(2,Barvy));
	  if Color[1]='M'then SetFillStyle((i mod 10)+1,co((i mod 14)+1,Barvy));
	  if Color[2]='3' then
	   bar3q(X1,Y1,X2,Y2,round(YM/30),round(XM/30),true)
	  else
	   bar(X1,Y1,X2,Y2);
	  rectangle(X1,Y1,X2,Y2);
	  if Rastr='H' then hod(X1+round((X2-X1)/2),Y2-round(YM/40),UY[i],MaximumC,MaxExp);
	 end;
   if Minimum<0 then
	 begin
	  pol:=round((YM-YM/4)/2+1);
	  Y2:=round(YMp/8+ YMp-YMp/4 -pol -(YMp-YMp/4)*UY[i]/(2*maximum));
	  if Color[1]='N'then SetFillStyle(1,KonB(ColorWWBack));
	  if Color[1]='C'then SetFillStyle(1,co((i mod 14)+1,Barvy));
	  if Color[1]='F'then SetFillStyle((i mod 10)+1,15);
	  if Color[1]='O'then SetFillStyle(1,co(2,Barvy));
	  if Color[1]='M'then SetFillStyle((i mod 10)+1,co((i mod 14)+1,Barvy));
	  if Color[2]='3' then
	   bar3q(X1,Y1-pol,X2,Y2,round(YM/30),round(XM/30),true)
	  else
	   bar(X1,Y1-pol,X2,Y2);
	  rectangle(X1,Y1-pol,X2,Y2);
	  if Rastr='H' then
		if (UY[i]<0)and(abs(UY[i])<maximum/2) then
		  hod(X1+round((X2-X1)/2),Y2+round(YM/40),UY[i],MaximumC,MaxExp)
		 else
		  hod(X1+round((X2-X1)/2),Y2-round(YM/40),UY[i],MaximumC,MaxExp);
	 end;
	   if (PopX='H')or(PopX='h') then
		begin
		 SetTextJustify(CenterText,CenterText);
		 SetTextStyle(SmallFont,HorizDir,K(4));
		 OutTextXYC(round(X1+round((X2-X1)/2)),
						 YM - YM div 10,UX[i]);
		end;
       if (PopX='I')or(PopX='i') then
         begin
		  SetTextJustify(CenterText,LeftText);
		  SetTextStyle(SmallFont,VertDir,K(4));
		 if Minimum>=0 then
		  if (UY[i]=0)or(Maximum/UY[i]>2) then
			OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet))),
					  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum-YMp/10),UX[i])
		   else
			begin
			 SetFillStyle(1,KonB(ColorWWBack));
			 bar(round(XMp/10 +2 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet)))-5,
					   YM - YM div 8-1,
				 round(XMp/10 +2 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet)))+5,
				 round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum)+10);
			 OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet))),
					  YM - YM div 8 - 6,UX[i]);
			end;
		 if Minimum<0 then
		  if (UY[i]<0) then
			OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet))),
					  round(YM - YM div 8 - pol -6),UX[i])
		   else
			 OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet))),
					  YM - YM div 8 - 6,UX[i]);
		  end;
		if (PopX='V')or(PopX='v') then
		  begin
		   SetTextJustify(CenterText,RightText);
		   SetTextStyle(SmallFont,VertDir,K(4));
		   OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet))),
						   YM - YM div 8 +2,UX[i]);
		end;

	end;
    SetColor(LastColor);
 end; { Sloupce2 }
end;

procedure Sloupce3(var Param:Parametry;var Maxim:Maxima);
var
  XMp,YMp,A:real;
  XM,YM,X1,Y1,X2,Y2,Xp1,Yp1,Xp2,Yp2,pol,i:integer;
begin
  with Param,Maxim do
 begin
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  LastColor:=GetColor;
  SetColor(KonB(ColorWWFor));

  SetTextJustify(LeftText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetFillStyle(1,1);
  A:= (XMp-XMp/5)/pocvet*(100-Pomer)/100; {sirka mezery}
  if A>((XMp-XMp/5)/pocvet{-4}) then A:=((XMp-XMp/5)/pocvet{-4});
  for i:=1 to PocVet do
  begin
   X1:=round(XMp/10 {+2} +A/2 + ((XMp-XMp/5)/pocvet)*(i-1));
   X2:=round(XMp/10 {-A/4} + ((XMp-XMp/5)/pocvet)*i-(XMp-XMp/5)/pocvet/2);
   Xp1:=round(XMp/10 {+A/4} + ((XMp-XMp/5)/pocvet)*(i-1)+(XMp-XMp/5)/pocvet/2);
   Xp2:=round(XMp/10 {-2} -A/2 + ((XMp-XMp/5)/pocvet)*i);
   if Minimum>=0 then
	 begin
	  Y1:=YM - YM div 8;
	  Y2:=round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum);
	  Yp1:=Y1;
	  Yp2:=round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/maximum);
	  if Color[1]='N'then SetFillStyle(1,KonB(ColorWWBack));
	  if Color[1]='C' then SetFillStyle(1,co((i mod 14)+1,Barvy));
	  if Color[1]='F' then SetFillStyle((i mod 10)+1,15);
	  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
	  if Color[1]='M'then SetFillStyle((i mod 10)+1,co((i mod 14)+1,Barvy));
	  if Color[2]='3' then
	  begin
	   bar3q(X1,Y1,X2,Y2,trunc(YMp/50),trunc(XMp/50),true);
	   if (Color[1]='O')or(Color[1]='o') then SetFillStyle(1,co(5,Barvy));
	   bar3q(Xp1,Yp1,Xp2,Yp2,trunc(YMp/50),trunc(XMp/50),true);
	  end
	  else
	  begin
	   bar(X1,Y1,X2,Y2);
	   if (Color[1]='O')or(Color[1]='o') then SetFillStyle(1,co(5,Barvy));
	   bar(Xp1,Yp1,Xp2,Yp2);
	  end;
	  rectangle(X1,Y1,X2,Y2);
	  rectangle(Xp1,Yp1,Xp2,Yp2);
	  if Rastr='H' then hod(X1+round((X2-X1)/2),Y2-round(YM/40),UY[i],MaximumC,MaxExp);
	  if Rastr='H' then hod(Xp1+round((Xp2-Xp1)/2),Yp2-round(YM/40),UZ[i],MaximumC,MaxExp);
	 end;
   if Minimum<0 then
	 begin
	  pol:=round((YM-YM/4)/2-1);
	  Y1:=round(YM - YM/8 - pol);
	  Y2:=round(YMp/8+ YMp-YMp/4 -pol -(YMp-YMp/4)*UY[i]/(2*maximum));
	  Yp1:=Y1;
	  Yp2:=round(YMp/8+ YMp-YMp/4 -pol -(YMp-YMp/4)*UZ[i]/(2*maximum));
	  if Color[1]='N'then SetFillStyle(1,KonB(ColorWWBack));
	  if Color[1]='C' then SetFillStyle(1,co((i mod 14)+1,Barvy));
	  if Color[1]='F' then SetFillStyle((i mod 10)+1,15);
	  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
	  if Color[1]='M'then SetFillStyle((i mod 10)+1,co((i mod 14)+1,Barvy));
	  if Color[2]='3' then
	  begin
	   bar3q(X1,Y1,X2,Y2,trunc(YMp/50),trunc(XMp/50),true);
	  if (Color[1]='O')or(Color[1]='o') then SetFillStyle(1,co(5,Barvy));
	   bar3q(Xp1,Yp1,Xp2,Yp2,trunc(YMp/50),trunc(XMp/50),true);
	  end
	  else
	  begin
	   bar(X1,Y1,X2,Y2);
	  if (Color[1]='O')or(Color[1]='o') then SetFillStyle(1,co(5,Barvy));
	   bar(Xp1,Yp1,Xp2,Yp2);
	  end;
	  rectangle(X1,Y1,X2,Y2);
	  rectangle(Xp1,Yp1,Xp2,Yp2);
	  if Rastr='H' then
		if (UY[i]<0)and(abs(UY[i])<maximum/2) then
		  hod(X1+round((X2-X1)/2),Y2+round(YM/40),UY[i],MaximumC,MaxExp)
		 else
		  hod(X1+round((X2-X1)/2),Y2-round(YM/40),UY[i],MaximumC,MaxExp);
	  if Rastr='H' then
		if (UZ[i]<0)and(abs(UZ[i])<maximum/2) then
		  hod(Xp1+round((Xp2-Xp1)/2),Yp2+round(YM/40),UZ[i],MaximumC,MaxExp)
		 else
		  hod(Xp1+round((Xp2-Xp1)/2),Yp2-round(YM/40),UZ[i],MaximumC,MaxExp);
	 end;
	   if (PopX='H')or(PopX='h') then
		begin
         SetTextJustify(CenterText,CenterText);
         OutTextXYC(round(XMp/10 +2 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet))),
                         YM - YM div 10,UX[i]);
        end;
       if (PopX='I')or(PopX='i') then
		 begin
		  SetTextJustify(CenterText,LeftText);
		   SetTextStyle(SmallFont,VertDir,K(4));
		 if Minimum>=0 then
		  if (UY[i]=0)or(Maximum/UY[i]>2) then
			OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(4*pocvet))),
					  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum)-8,UX[i])
		   else
			begin
			 SetFillStyle(1,KonB(ColorWWBack));
			 bar(round(XMp/10 +2 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(4*pocvet)))-5,
					   YM - YM div 8-1,
				 round(XMp/10 +2 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(4*pocvet)))+5,
				 round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum)+10);
			 OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(4*pocvet))),
					  YM - YM div 8 - 6,UX[i]);
			end;
		 if Minimum<0 then
		  if (UY[i]<0) then
			OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(4*pocvet))),
					  round(YM - YM div 8 - pol -6),UX[i])
		   else
			 OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(4*pocvet))),
					  YM - YM div 8 - 6,UX[i]);
		  end;
		if (PopX='V')or(PopX='v') then
		  begin
		   SetTextJustify(CenterText,RightText);
		   SetTextStyle(SmallFont,VertDir,K(4));
		   OutTextXYC(round(XMp/10 + ((XMp-XMp/5)/pocvet)*(i-1)+((XMp-XMp/5)/(2*pocvet))),
						   YM - YM div 8 +2,UX[i]);
		end;

	end;
	SetColor(LastColor);
 end; { Sloupce3 }
end;

{-------- cir }
const V=10;
	  Po=12;
procedure Circular(var Param:Parametry;var Maxim:Maxima);
var
  EaglePos,Eagle,XM,YM,XMp,X0,Y0,X,Y,RadC,RadT,Radp,Asp,i:integer;
  Xasp,Yasp:word;
  Ret:ComStr;
  NazevP:string[80];
  W:boolean;

procedure GetXY(Uh,Rad:word;var X,Y:integer);
var
  Radian:real;
begin
  Radian:=Uh*Pi/180;
  X:=X0+round(Rad*sin(Radian));
  Y:=Y0-(round(Rad*cos(Radian)*Xasp) div Yasp);
end; {GetXY}

procedure GetXYel(Uh,Rad:real;var X,Y:integer);
var
  Radian:real;
begin
  Radian:=Uh*Pi/180;
  X:=X0+round(Rad*sin(Radian));
  Y:=Y0-((round(Rad/2*cos(Radian)*Xasp) div Yasp));
end; {GetXY}

procedure WC(n:integer);
var
  qad : array [1..4] of PointType;

begin with Param,Maxim do
begin
  LastColor:=GetColor;
  SetColor(KonB(ColorWWFor));
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetTextJustify(LeftText,CenterText);
  if n<=16 then
	begin
	  if Minimum/Maximum*100>=Po then
		begin X:=XM-round(XM/6);Y:=(n+2)*(YM div 20);end
	   else
		begin X:=XM-round(XM/4.3);Y:=(n+2)*(YM div 20);end;
	end
   else
	begin
	  X:=XM div 17;Y:=(n-16+2)*(YM div 20);
	end;
  str(UY[i]/Maximum*100:3:2,Ret);
  if Minimum/Maximum*100>=Po then
	OutTextXYC(X+XM div 20,Y,UX[i])
  else
	OutTextXYC(X+XM div 20,Y,UX[i]+' '+Ret+'%');
  if Color[1]='C' then SetFillStyle(1,co((i mod 14)+1,Barvy));
  if Color[1]='F' then SetFillStyle((i mod 10)+1,15);
  if Color[1]='M'then SetFillStyle((i mod 10)+1,co((i mod 14)+1,Barvy));
  qad[1].x:=X-XM div 30;
  qad[1].y:=Y-YM div 50;
  qad[2].x:=X+XM div 25;
  qad[2].y:=Y-YM div 50;
  qad[3].x:=X+XM div 25;
  qad[3].y:=Y+YM div 40;
  qad[4].x:=X-XM div 30;
  qad[4].y:=Y+YM div 40;
  FillPoly(4,qad);
  rectangle(qad[1].x,qad[1].y,qad[3].x,qad[3].y);
  SetColor(LastColor);
end;
end;

begin
 with Param,Maxim do
  begin
    XM:=GMaxX;
    YM:=GMaxY;
    XMp:=round(XM/3*2);
	if YM<XMp then Radp:=YM else Radp:=XMp;
    if not((length(Color)>1)and(Color[2]='3')) then
     begin
       {----------------------- hlavicka }
	   SetTextJustify(CenterText,CenterText);
       SetColor(KonB(ColorWWFor));
       if length(Hlavicka)>55 then
		 SetTextStyle(TriplexFont,HorizDir,K(2))
       else
         SetTextStyle(TriplexFont,HorizDir,K(3));
       OutTextXYC(XM div 2,YM div 18,Hlavicka);
{       SetTextJustify(LeftText,CenterText);
	   SetTextStyle(SmallFont,HorizDir,K(6));
	   NazevP:=NazevY;
       OutTextXYC(XM div 20,YM-YM div 20,NazevP);}
       SetTextJustify(RightText,CenterText);
       SetTextStyle(SmallFont,HorizDir,K(6));
       OutTextXYC(XM-XM div 20,YM-YM div 20,NazevX);
     {----------------------- vypocet }
       if Maximum>=100
             then
              str(Maximum:11:0,Ret)
             else
              str(Maximum:2:2,Ret);
	   if Maximum<0.01 then str(Maximum:1:9,Ret);
       Ret:=NazevY+' '+Ret+' = 100 %';
	   SetTextJustify(LeftText,CenterText);
	   SetTextStyle(SmallFont,HorizDir,K(6));
       OutTextXYC(XM div 20,YM-YM div 20,Ret);
       SetColor(LastColor);
       if Maximum<>0 then
       SetTextStyle(SmallFont,HorizDir,K(4));
	   EaglePos:=0;
       if (Minimum/Maximum*100<Po)and(pocvet<=16) then
         X0:= round(XM/2.7)
        else
         X0:= XM div 2;
       Y0:= (YM div 2)+(YM div 20);
       GetAspectRatio(Xasp,Yasp);
	   RadC:= round(Radp/2.5);  { Hercules }
       RadT:= round(Radp/2.3);
       if GrDriv=VGA then
         begin
           RadC:= round(Radp/4);
		   RadT:= round(Radp/3);
         end;
       if (GrDriv>=3)and(GrDriv<=7) then
		 begin
           RadC:= round(RadP/3.3);
           RadT:= round(RadP/2.6);
		 end;
       SetLineStyle(0,0,1);
	   if KonB(ColorWWBack)=15 then SetColor(0) else SetColor(15);
	   Circle(X0,Y0,RadC);
	   GetXY(0,RadC,X,Y);
	   line(X0,Y0,X,Y);
	   if Pocvet>10 then
		 begin
		   LastColor:=GetColor;
		   SetColor(KonB(ColorWWFor));
		   line(X,Y-YM div 40,X+XM div 30,Y-YM div 40);
		   line(X+XM div 30,Y-YM div 40,X+XM div 60,Y-YM div 40-YM div 90);
		   line(X+XM div 30,Y-YM div 40,X+XM div 60,Y-YM div 40+YM div 90);
		  if Minimum/Maximum*100>=Po then
		  begin
		   line(XM-round(XM/4.5),YM div 7,XM-round(XM/4.5),YM div 5);
		   line(XM-round(XM/4.5),YM div 5,XM-round(XM/4.6),YM div 6);
		   line(XM-round(XM/4.5),YM div 5,XM-round(XM/4.4),YM div 6);
		  end
		  else
		  begin
		   line(XM-round(XM/3.5),YM div 7,XM-round(XM/3.5),YM div 5);
		   line(XM-round(XM/3.5),YM div 5,XM-round(XM/3.6),YM div 6);
		   line(XM-round(XM/3.5),YM div 5,XM-round(XM/3.4),YM div 6);
		  end;
		   SetColor(LastColor);
		 end;
	   if Maximum<>0 then
		for i:=1 to pocvet do
		 begin
		   if KonB(ColorWWBack)=15 then SetColor(0) else SetColor(15);
		   Eagle:=round(UY[i]/Maximum*360);
		   GetXY(EaglePos+Eagle,RadC,X,Y);
		   if i<pocvet then line(X0,Y0,X,Y);
		  if UY[i]>=0 then
		  begin
		   if Color[1]='C' then SetFillStyle(1,co((i mod 14)+1,Barvy));
		   if Color[1]='F' then SetFillStyle((i mod 10)+1,15);
		   if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
		   if Color[1]='M'then SetFillStyle((i mod 10)+1,co((i mod 14)+1,Barvy));
		   if EaglePos+Eagle<=360 then      { korekce chyby zaokrouhlovani }
			 GetXY(EaglePos+Eagle-(Eagle div 2),RadC-2,X,Y)
			else
			 GetXY(EaglePos+Eagle-(Eagle div 2)-1,RadC-2,X,Y);
		   if (Color[1]<>'N') and (Eagle>1) then
			 if KonB(ColorWWBack)=15 then FloodFill(X,Y,0) else FloodFill(X,Y,15);
		  end;
		   LastColor:=GetColor;
		   SetColor(KonB(ColorWWFor));
		   if (PopX='H')or(Color[1]='O')or(Color[1]='N') then
			 begin
			  if Minimum/Maximum*100>=Po then
			  begin
			   GetXY(EaglePos+Eagle-(Eagle div 2),RadT,X,Y);
			   if EaglePos+Eagle-(Eagle div 2)>180 then
				 SetTextJustify(RightText,CenterText)
				else
				 SetTextJustify(LeftText,CenterText);
			   str(UY[i]/Maximum*100:3:2,Ret);
			   Ret:=Ret+'% '+UX[i];
			   LastColor:=GetColor;
			   SetColor(KonB(ColorWWFor));
			   OutTextXYC(X,Y,Ret);
			   SetColor(LastColor);
			  end
			  else
			   WC(i);
			 end;
		   if (PopX='I')and(Color[1]<>'O')and(Color[1]<>'N') then
			 begin
			  if Minimum/Maximum*100>=Po then
			  begin
			   LastColor:=GetColor;
			   if Color[1]='F' then SetColor(14) else SetColor(15);
			   GetXY(EaglePos+Eagle-(Eagle div 2),RadC-round(RadC/3),X,Y);
			   rectangle(X-XM div 30,Y-YM div 50,X+XM div 25,Y+YM div 40);
			   SetFillStyle(0,0);
			   if Color[1]='F' then FloodFill(X,Y,14) else FloodFill(X,Y,15);
			   SetColor(15);
			   rectangle(X-XM div 30,Y-YM div 50,X+XM div 25,Y+YM div 40);
			   SetTextStyle(SmallFont,HorizDir,K(4));
			   SetTextJustify(CenterText,CenterText);
			   str(UY[i]/Maximum*100:3:2,Ret);
			   Ret:=' '+Ret+'%';
			   OutTextXYC(X,Y,Ret);
			   SetColor(LastColor);
			  end;
			   WC(i);
			 end;
		   if (PopX='V')and(Color[1]<>'O')and(Color[1]<>'N') then
			 begin
			  if Minimum/Maximum*100>=Po then
			  begin
			   GetXY(EaglePos+Eagle-(Eagle div 2),RadT+RadT div 12,X,Y);
{               rectangle(X-XM div 30,Y-YM div 50,X+XM div 25,Y+YM div 40);
			   SetFillStyle(0,0); ramecek
			   FloodFill(X,Y,15);}
			   SetTextStyle(SmallFont,HorizDir,K(4));
			   SetTextJustify(CenterText,CenterText);
			   str(UY[i]/Maximum*100:3:2,Ret);
			   Ret:=' '+Ret+'%';
			   LastColor:=GetColor;
			   SetColor(KonB(ColorWWFor));
			   OutTextXYC(X,Y,Ret);
			   SetColor(LastColor);
			  end;
			   WC(i);
			 end;
		  EaglePos:=EaglePos+Eagle;
		 end;

{       if upcase(ColorWWFor)<>'L' then}
		begin
		  EaglePos:=0;
		  SetColor(KonB(ColorWWFor));
		  Circle(X0,Y0,RadC);
		  GetXY(0,RadC,X,Y);
		  line(X0,Y0,X,Y);
		  if Maximum<>0 then
		   for i:=1 to pocvet do
			begin
			  Eagle:=round(UY[i]/Maximum*360);
			  GetXY(EaglePos+Eagle,RadC,X,Y);
			  if i<pocvet then line(X0,Y0,X,Y);
			  EaglePos:=EaglePos+Eagle;
            end;
        end;
     end
   else       { 3D cir }
	 begin
       {----------------------- hlavicka }
	   SetColor(KonB(ColorWWFor));
       SetTextJustify(CenterText,CenterText);
       if length(Hlavicka)>55 then
         SetTextStyle(TriplexFont,HorizDir,K(2))
	   else
         SetTextStyle(TriplexFont,HorizDir,K(3));
	   OutTextXYC(XM div 2,YM div 18,Hlavicka);
{       SetTextJustify(LeftText,CenterText);
       SetTextStyle(SmallFont,HorizDir,K(6));
	   NazevP:=NazevY;
       OutTextXYC(XM div 20,YM-YM div 20,NazevP);}
       SetTextJustify(RightText,CenterText);
       SetTextStyle(SmallFont,HorizDir,K(6));
	   OutTextXYC(XM-XM div 20,YM-YM div 20,NazevX);
     {----------------------- vypocet }
       if Maximum>=100
             then
			  str(Maximum:11:0,Ret)
             else
			  str(Maximum:2:2,Ret);
       if Maximum<0.01 then str(Maximum:1:9,Ret);
       Ret:=NazevY+' '+Ret+' = 100 %';
	   SetTextJustify(LeftText,CenterText);
       SetTextStyle(SmallFont,HorizDir,K(6));
	   OutTextXYC(XM div 20,YM-YM div 20,Ret);
       SetColor(LastColor);
       if Maximum<>0 then
       SetTextStyle(SmallFont,HorizDir,K(4));
       EaglePos:=0;
	   if (Minimum/Maximum*100<Po)and(pocvet<=16) then
{       if (PopX<>'H')and(pocvet<=16) then}
		 X0:= round(XM/2.7)
		else
		 X0:= XM div 2;
	   Y0:= (YM div 2)-(YM div 20);
	   GetAspectRatio(Xasp,Yasp);
	   RadC:= round(Radp/2.2);
	   RadT:= round(Radp/2);
	   if GrDriv=VGA then
		 begin
		   RadC:= round(RadP/4);
		   RadT:= round(RadP/3);
		 end;
	   if (GrDriv>=3)and(GrDriv<=7) then
		 begin
		   RadC:= round(RadP/3.3);
		   RadT:= round(RadP/2.6);
		 end;
	   SetLineStyle(0,0,1);
	   if KonB(ColorWWBack)=15 then SetColor(0) else SetColor(15);
	   Ellipse(X0,Y0,0,360,RadC,(RadC*Xasp div Yasp) div 2);
	   Y0:=Y0+YM div V;
	   Ellipse(X0,Y0,180,360,RadC,(RadC*Xasp div Yasp) div 2);
	   Y0:=Y0-YM div V;
	   GetXYel(90,RadC,X,Y);
	   line(X,Y,X,Y+YM div V);
	   GetXYel(270,RadC,X,Y);
	   line(X,Y,X,Y+YM div V);
	   GetXYel(0,RadC,X,Y);
	   line(X0,Y0,X,Y);
	   if Pocvet>10 then
		 begin
		   LastColor:=GetColor;
		   SetColor(KonB(ColorWWFor));
		   line(X,Y-YM div 40,X+XM div 30,Y-YM div 40);
		   line(X+XM div 30,Y-YM div 40,X+XM div 60,Y-YM div 40-YM div 90);
		   line(X+XM div 30,Y-YM div 40,X+XM div 60,Y-YM div 40+YM div 90);
		  if Minimum/Maximum*100>=Po then
		  begin
		   line(XM-round(XM/4.5),YM div 7,XM-round(XM/4.5),YM div 5);
		   line(XM-round(XM/4.5),YM div 5,XM-round(XM/4.6),YM div 6);
		   line(XM-round(XM/4.5),YM div 5,XM-round(XM/4.4),YM div 6);
		  end
		  else
		  begin
		   line(XM-round(XM/3.5),YM div 7,XM-round(XM/3.5),YM div 5);
		   line(XM-round(XM/3.5),YM div 5,XM-round(XM/3.6),YM div 6);
		   line(XM-round(XM/3.5),YM div 5,XM-round(XM/3.4),YM div 6);
		  end;
		  SetColor(LastColor);
		 end;
	   if Maximum<>0 then
		for i:=1 to pocvet do
		 begin
		   if KonB(ColorWWBack)=15 then SetColor(0) else SetColor(15);
		   Eagle:=round(UY[i]/Maximum*360);
		   GetXYel(EaglePos+Eagle,RadC,X,Y);
		   if i<pocvet then
			 begin
			   line(X0,Y0,X,Y);
			   if (EaglePos+Eagle>90)and(EaglePos+Eagle<270) then
				 line(X,Y,X,Y+YM div V);
			 end;
		  if UY[i]>=0 then
		  begin
		   if Color[1]='C' then SetFillStyle(1,co((i mod 14)+1,Barvy));
		   if Color[1]='F' then SetFillStyle((i mod 10)+1,15);
		   if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
		   if Color[1]='M'then SetFillStyle((i mod 10)+1,co((i mod 14)+1,Barvy));
		   if EaglePos+Eagle<=360 then      { korekce chyby zaokrouhlovani }
			 GetXYel(EaglePos+Eagle-(Eagle div 2),RadC-3,X,Y)
			else
			 GetXYel(EaglePos+Eagle-(Eagle div 2)-1,RadC-3,X,Y);
		   if (Color[1]<>'N')and(Eagle>1) then
			 if KonB(ColorWWBack)=15 then FloodFill(X,Y,0) else FloodFill(X,Y,15);

		   if (EaglePos+Eagle>100)and(EaglePos+Eagle<270) {and(Rastr<>'N')} then
			 begin
			   GetXYel(EaglePos+Eagle,RadC,X,Y);
			   if (Color[1]<>'N')and(Eagle>1) then
				 if KonB(ColorWWBack)=15 then FloodFill(X+1,Y+YM div (2*V),0) else FloodFill(X+1,Y+YM div (2*V),15);
			 end;
		   if (EaglePos<260)and(EaglePos>100)and(EaglePos+Eagle>270) {and(Rastr<>'N')} then
			 begin
			   GetXYel(EaglePos,RadC,X,Y);
			   if (Color[1]<>'N')and(Eagle>1) then
				if KonB(ColorWWBack)=15 then FloodFill(X-1,Y+YM div (2*V),0) else FloodFill(X-1,Y+YM div (2*V),15);
			 end;
		   if (EaglePos<100)and(EaglePos+Eagle>260) {and(Rastr<>'N')} then
			 begin
			   GetXYel(EaglePos+(Eagle div 2),RadC,X,Y);
			   if (Color[1]<>'N')and(Eagle>1) then
				 if KonB(ColorWWBack)=15 then FloodFill(X-1,Y+YM div (2*V),0) else FloodFill(X-1,Y+YM div (2*V),15);
			 end;
		   if (PopX='H')or(Color[1]='O')or(Color[1]='N') then
			 begin
			  if Minimum/Maximum*100>=Po then
			  begin
			   GetXYel(EaglePos+Eagle-(Eagle div 2),RadT,X,Y);
			   if EaglePos+Eagle-(Eagle div 2)>180 then
				 SetTextJustify(RightText,CenterText)
				else
				 SetTextJustify(LeftText,CenterText);
			   if ((EaglePos+Eagle-(Eagle div 2))>90)and
				  ((EaglePos+Eagle-(Eagle div 2))<270) then Y:=Y+3*(Y0 div V)
													   else Y:=Y-(Y0 div V);
               str(UY[i]/Maximum*100:3:2,Ret);
			   Ret:=Ret+'% '+UX[i];
			   LastColor:=GetColor;
               SetColor(KonB(ColorWWFor));
               OutTextXYC(X,Y,Ret);
			   SetColor(LastColor);
			  end
			  else
			   WC(i);
			 end;
		   if (PopX='I')and(Minimum/Maximum*100<12) then PopX:='V';
		   if (PopX='I')and(Color[1]<>'O')and(Color[1]<>'N') then
			 begin
			  if Minimum/Maximum*100>=Po then
			  begin
			   LastColor:=GetColor;
			   if Color[1]='F' then SetColor(14) else SetColor(15);
			   GetXYel(EaglePos+Eagle-(Eagle div 2),RadC-round(RadC/3),X,Y);
			   rectangle(X-XM div 30,Y-YM div 50,X+XM div 25,Y+YM div 40);
			   SetFillStyle(0,0);
			   if Color[1]='F' then FloodFill(X,Y,14) else FloodFill(X,Y,15);
			   SetColor(15);
			   rectangle(X-XM div 30,Y-YM div 50,X+XM div 25,Y+YM div 40);
			   SetTextStyle(SmallFont,HorizDir,K(4));
			   SetTextJustify(CenterText,CenterText);
			   str(UY[i]/Maximum*100:3:2,Ret);
			   Ret:=' '+Ret+'%';
			   OutTextXYC(X,Y,Ret);
			   SetColor(LastColor);
			  end;
			   WC(i);
             end;
		   if (PopX='V')and(Color[1]<>'O')and(Color[1]<>'N') then
			 begin
              if Minimum/Maximum*100>=Po then
              begin
               GetXYel(EaglePos+Eagle-(Eagle div 2),RadT+RadT div 11,X,Y);
               if ((EaglePos+Eagle-(Eagle div 2))>90)and
				  ((EaglePos+Eagle-(Eagle div 2))<270) then Y:=Y+2*(Y0 div V);
{               rectangle(X-XM div 30,Y-YM div 50,X+XM div 25,Y+YM div 40);
			   SetFillStyle(0,0);     ramecek
			   FloodFill(X,Y,ColorWWFor);}
               SetTextStyle(SmallFont,HorizDir,K(4));
               SetTextJustify(CenterText,CenterText);
               str(UY[i]/Maximum*100:3:2,Ret);
               Ret:=' '+Ret+'%';
               LastColor:=GetColor;
               SetColor(KonB(ColorWWFor));
               OutTextXYC(X,Y,Ret);
			   SetColor(LastColor);
			  end;
			   WC(i);
			 end;
		  end;
		   EaglePos:=EaglePos+Eagle;
		 end;

{       if upcase(ColorWWFor)<>'L' then}
		begin
		  EaglePos:=0;
		  SetColor(KonB(ColorWWFor));
          Ellipse(X0,Y0,0,360,RadC,(RadC*Xasp div Yasp) div 2);
          Y0:=Y0+YM div V;
          Ellipse(X0,Y0,180,360,RadC,(RadC*Xasp div Yasp) div 2);
          Y0:=Y0-YM div V;
          GetXYel(90,RadC,X,Y);
          line(X,Y,X,Y+YM div V);
          GetXYel(270,RadC,X,Y);
          line(X,Y,X,Y+YM div V);
          GetXYel(0,RadC,X,Y);
          line(X0,Y0,X,Y);
          if Maximum<>0 then
           for i:=1 to pocvet do
            begin
              Eagle:=round(UY[i]/Maximum*360);
              GetXYel(EaglePos+Eagle,RadC,X,Y);
              if i<pocvet then
                begin
                  line(X0,Y0,X,Y);
                  if (EaglePos+Eagle>90)and(EaglePos+Eagle<270) then
                    line(X,Y,X,Y+YM div V);
                end;
              EaglePos:=EaglePos+Eagle;
            end;
        end;

     end;
    Maximum:=0;
  end;
end;
procedure Linear(var Param:Parametry;var Maxim:Maxima);

var
  XMp,YMp,pol:real;
  XM,YM,i:integer;
  s,LastColor:word;
  Patt:byte;

begin
  with Param,Maxim do
 begin
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  if Pomer<50 then s:=1 else s:=3;
  if Pocvet<2 then exit;
  LastColor:=GetColor;
  if (Color[1]='F')or(Color[1]='M') then Patt:=1 else Patt:=0;
  if UdajZ<>'' then
   begin
	if(Color[1]='C')or(Color[1]='M') then SetColor(co(2,Barvy))
	 else SetColor(14);
	SetLineStyle(0,0,s);
	line(XM div 60,YM div 20,XM div 20,YM div 20);
	if(Color[1]='C')or(Color[1]='M') then SetColor(co(3,Barvy))
	 else SetColor(14);
	SetLineStyle(Patt,0,s);
    line(XM div 60,YM - YM div 20,XM div 20,YM - YM div 20);
   end;
  if ((Minimum=0)and(Maximum=0))or(UL[pocvet]-UL[1]=0) then exit;
  for i:=1 to PocVet-1 do
  begin
   if Minimum>=0 then
    begin
    if(Color[1]='C')or(Color[1]='M') then SetColor(co(2,Barvy))
     else SetColor(14);
	 SetLineStyle(0,0,s);
     line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum),
                  round(XMp/10 +(UL[i+1]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i+1]/maximum));
     if UdajZ<>'' then
      begin
       if(Color[1]='C')or(Color[1]='M') then SetColor(co(3,Barvy))
        else SetColor(14);
	   SetLineStyle(Patt,0,s);
       line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                    round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/maximum),
                    round(XMp/10 +(UL[i+1]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                    round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i+1]/maximum));
      end;
    end;
   if Minimum<0 then
     begin
      pol:=(YM-YM/4)/2+1;
       if(Color[1]='C')or(Color[1]='M') then SetColor(co(2,Barvy))
        else SetColor(14);
	  SetLineStyle(0,0,s);
      line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/(2*maximum)-pol),
                  round(XMp/10 +(UL[i+1]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -pol -(YMp-YMp/4)*UY[i+1]/(2*maximum)));
      if UdajZ<>'' then
       begin
        if(Color[1]='C')or(Color[1]='M') then SetColor(co(3,Barvy))
         else SetColor(14);
		SetLineStyle(Patt,0,s);
        line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                    round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/(2*maximum)-pol),
                    round(XMp/10 +(UL[i+1]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                    round(YMp/8+ YMp-YMp/4 -pol -(YMp-YMp/4)*UZ[i+1]/(2*maximum)));
       end;
     end;
    end;
    SetColor(LastColor);
 end; { Linear }
end;

end.

