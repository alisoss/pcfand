unit Gr3DD;

{$A-,B-,D-,F+,G-,I+,L-,O+,R-,S+,V+,X+}

interface
  uses GrGlob;
  procedure OsyD(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure SloupDS(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure SloupDL(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
  procedure SloupSB(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);

implementation
  uses dos,graph;

procedure OsyD(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);

var
  p,X,Y,XM,YM,i:integer;
  dil,MaximumC:real;
  MaxStr:string[14];
  NazevP:string[80];

procedure WCD(n:integer);
var
  qad : array [1..4] of PointType;

begin with Param,Par3,Maxim do
begin
  LastColor:=GetColor;
  SetColor(KonB(ColorWWFor));
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetTextJustify(LeftText,CenterText);
  begin X:=XM-XM div 6;Y:=(n+7)*(YM div 20);end;
  OutTextXYC(X+XM div 20,Y,copy(Nazev3[n],1,10));
  if (Typ='DS')or(Typ='SB') then
	begin
	  if Color[1]='C' then SetFillStyle(1,co((n mod 14)+2,Barvy));
	  if Color[1]='F' then SetFillStyle(((n+1) mod 10)+1,15);
	  if Color[1]='M'then SetFillStyle(((n+1) mod 10)+1,co((n mod 14)+2,Barvy));
	  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
	  qad[1].x:=X-XM div 40;
	  qad[1].y:=Y-YM div 60;
	  qad[2].x:=X+XM div 35;
	  qad[2].y:=Y-YM div 60;
	  qad[3].x:=X+XM div 35;
	  qad[3].y:=Y+YM div 50;
	  qad[4].x:=X-XM div 40;
	  qad[4].y:=Y+YM div 50;
	  FillPoly(4,qad);
	  rectangle(qad[1].x,qad[1].y,qad[3].x,qad[3].y);
	  SetColor(LastColor);
	end
  else
	begin
	  if (Color[1]='C')or(Color[1]='M') then SetColor(co((m mod 14)+2,Barvy))
		else SetColor(15);
	  if (Color[1]='F')or(Color[1]='M') then SetLineStyle(m mod 3,0,3)
		else SetLineStyle(0,0,3);
	  Line(X-XM div 40,Y,X+XM div 35,Y);
    end;
   SetColor(LastColor);
end;
end;


  procedure DilY(Y:integer;Hodnota:str9);
    begin
      SetLineStyle(0,0,1);
      Line(XM div 11,Y,XM div 10,Y);
      SetTextJustify(RightText,CenterText);
      SetTextStyle(SmallFont,HorizDir,K(4));
      OutTextXYC(XM div 12,Y,Hodnota);
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
  Line(XM div 10,round(YM/10*3),XM div 10,YM-round(YM/10*2)); { osa Y }
  Line(XM div 10,YM-round(YM/10*2),XM-XM div 4,Ym-round(Ym/10*2)); { osa X }
{----------------------- nazev X }
  if (PopX='H')or(PopX='h') then
    begin
      SetTextJustify(RightText,CenterText);
      SetTextStyle(SmallFont,HorizDir,K(6));
      OutTextXYC(XM-round(XM/4.5),YM-YM div 20,NazevX);
    end
   else
    begin
      SetTextJustify(CenterText,LeftText);
      SetTextStyle(SmallFont,VertDir,K(6));
      OutTextXYC(XM-round(XM/4.5),YM-YM div 25,NazevX);
    end;
{----------------------- nazev Y }
    begin
      SetTextJustify(CenterText,RightText);
      SetTextStyle(SmallFont,VertDir,K(6));
      OutTextXYC(XM div 50,round(YM/10*3),NazevY);
    end;
{----------------------- dilky Y }
 if Minimum>=0 then
  {----------------------- uprava maxima pro zadane minimum }
  if (MinimumVst>0)and(Maximum>0) then
    begin
     MinimumC:=MinimumVst*MaximumC/Maximum;
     MinimumVstP:=MinimumVst;
     Maximum:=Maximum-MinimumVst;
     for i:=0 to 9 do
       for p:=1 to PocVet do U3[i,p]:=U3[i,p]-MinimumVst;
     dil:=(YM - (YM/2))/10;
     for p:= 0 to 10 do
      begin
       if Maximum>=10 then
         str((MaximumC-p*(MaximumC-MinimumC)/10):5:0,MaxStr)
        else
         str((MaximumC-p*(MaximumC-MinimumC)/10):4:1,MaxStr);
       if Maximum<1 then
         str((MaximumC-p*(MaximumC-MinimumC)/10):3:2,MaxStr);
       DilY(round(YM/10*3 + p*dil),MaxStr);
      end;
    end
  else
  begin
   dil:=(YM - (YM/2))/10;
   for p:= 0 to 10 do
     begin
       if Maximum>=10 then
         str((MaximumC-p*(MaximumC-MinimumC)/10):5:0,MaxStr)
        else
         str((MaximumC-p*(MaximumC-MinimumC)/10):4:1,MaxStr);
       if Maximum<1 then
         str((MaximumC-p*(MaximumC-MinimumC)/10):3:2,MaxStr);
       DilY(round(YM/10*3 + p*dil),MaxStr);
     end;
   end
  else
  begin
   dil:=(YM - (YM/2))/20;
   for p:= 0 to 20 do
     begin
       if Maximum>=10 then
         str((MaximumC-p*MaximumC/10):5:0,MaxStr)
        else
         str((MaximumC-p*MaximumC/10):4:1,MaxStr);
       if Maximum<1 then
         str((MaximumC-p*MaximumC/10):3:2,MaxStr);
       DilY(round(YM/10*3 + p*dil),MaxStr);
       if p=10 then
         Line(XM div 10,round(YM/10*3 + p*dil),XM - XM div 4,round(YM/10*3 + p*dil));
     end;
   end;
{----------------------- rastr }
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
{----------------------- tisk koeficientu Y }
  if MaxExp<>0 then
    begin
     if MaxExp>0
      then
       if MaxExp<9 then       {a}
        begin
         str(exp(MaxExp*ln(10)):6:0,MaxStr);
         MaxStr:='x'+MaxStr;
         OutTextXYC(XM div 12,YM div 4,MaxStr);
        end
       else
        begin
         str(exp(MaxExp*ln(10)):9:0,MaxStr);
         MaxStr:='x'+MaxStr;
         OutTextXYC(XM div 10,YM div 4,MaxStr);
        end                   {a}
      else
       if MaxExp>-6 then      {b}
        begin
          str(exp(MaxExp*ln(10)):1:5,MaxStr);
          MaxStr:='x'+MaxStr;
          OutTextXYC(XM div 12,YM div 4,MaxStr);
        end
       else
        begin
          str(exp(MaxExp*ln(10)):1:12,MaxStr);
          MaxStr:='x'+MaxStr;
          OutTextXYC(XM div 8,YM div 4,MaxStr);
        end                    {b}
    end;
    for m:=0 to PocZ do
      WCD(m);
    SetColor(LastColor);

 end; {Osy}
end;
procedure SloupDS(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
var
  D,A,B:real;
  XMp,YMp:real;
  XM,YM,X1,X2,Y1,YC,Y2,i:integer;
begin
  with Param,Par3,Maxim do
 begin
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  SetColor(KonB(ColorWWFor));

  SetTextJustify(LeftText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetFillStyle(1,1);
  for i:=1 to Pocvet do
  begin
   if Minimum>=0 then
	 begin
	   for m:=0 to PocZ do
		begin
			D:= XM-(XM div 10) - (XM div 4);  {delka osy Y}
			A:= D/pocvet*(100-Pomer)/100; {sirka mezery}
			B:= D/pocvet-A;                   {sirka jedne vety}
			X1:=round(XM div 10 + A/2 + m*B/(PocZ+1) + (i-1)*(B+A));
			X2:=round(XM div 10 + A/2 + (m+1)*B/(PocZ+1) + (i-1)*(B+A));
			Y1:=YM-round(YM/10*2)-1;
			YC:=YM-round(YM/10*5);
			Y2:=YM-round(YM/10*2)-1-round(YC*U3[m,i]/maximum);
		  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
		  if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
		  if Color[1]='M' then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
		  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
		  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
		  if Color[2]='3' then
		   begin
			 bar3Q(X1,Y1,X2,Y2,YC div 20,YC div 20,true);
		   end
		  else
		   begin
			 bar(X1,Y1,X2,Y2);
			 rectangle(X1,Y1,X2,Y2);
		   end;
		end;
	 end;
   if Minimum<0 then
	 begin
	   for m:=0 to PocZ do
		begin
			D:= XM-(XM div 10) - (XM div 4);
			A:= D/pocvet*(100-Pomer)/100; {sirka mezery}
			B:= D/pocvet-A;
			X1:=round(XM div 10 + A/2 + m*B/(PocZ+1) + (i-1)*(B+A));
			X2:=round(XM div 10 + A/2 + (m+1)*B/(PocZ+1) + (i-1)*(B+A));
			YC:=round((YM-round(YM/10*5))/2);
			Y1:=YM-round(YM/10*2)-1-YC;
			Y2:=YM-round(YM/10*2)-1-YC-round(YC*U3[m,i]/maximum);
		  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
		  if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
		  if Color[1]='M'then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
		  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
		  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
		  if Color[2]='3' then
		   begin
			 bar3Q(X1,Y1,X2,Y2,YC div 20,YC div 20,true);
		   end
		  else
		   begin
			 bar(X1,Y1,X2,Y2);
			 rectangle(X1,Y1,X2,Y2);
		   end;
		end;
	 end;
	   if Color[2]='3' then OsyD(Param,Par3,Maxim);
	   LastColor:=GetColor;
	   SetColor(KonB(ColorWWFor));
	   if (PopX='H')or(PopX='h') then
		begin
			A:= D/pocvet*(100-Pomer)/100; {sirka mezery}
{			A:= D/pocvet/20;}
			B:= D/pocvet-A;
			X1:=round(XM div 10 + (i-1)*(B+A) + (B+A)/2);
			Y1:=YM-round(YM/10*1.5);
		 SetTextJustify(CenterText,CenterText);
		 OutTextXYC(X1,Y1,UX[i]);
		end;
		if (PopX='V')or(PopX='v')or(PopX='I')or(PopX='i') then
		  begin
			A:= D/pocvet*(100-Pomer)/100; {sirka mezery}
			B:= D/pocvet-A;
			X1:=round(XM div 10 + (i-1)*(B+A) + (B+A)/2);
			Y1:=YM-round(YM/10*1.8);
		   SetTextJustify(CenterText,RightText);
		   SetTextStyle(SmallFont,VertDir,K(4));
		   OutTextXYC(X1,Y1,UX[i]);
		end;
		SetColor(LastColor);

	end;
 end; { SloupDS }
end;

procedure SloupDL(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
var
  D,A,B:real;
  XMp,YMp:real;
  XM,YM,X1,X2,Y1,YC,Y2,i:integer;
  s:word;
begin
  with Param,Par3,Maxim do
 begin
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  if Pomer<50 then s:=1 else s:=3;
  SetColor(KonB(ColorWWFor));

  SetTextJustify(LeftText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetFillStyle(1,1);
  if Pocvet<2 then exit;
  for i:=1 to Pocvet do
  begin
   if (Minimum>=0)and(i<pocvet) then
     begin
	   for m:=0 to PocZ do
        begin
            D:= XM-(XM div 10) - (XM div 4);  {delka osy Y}
            B:= D/(pocvet-1);                 {sirka jedne vety}
            X1:=round(XM div 10 + (i-1)*B);
            X2:=round(XM div 10 + i*B);
            YC:=YM-round(YM/10*5);
            Y1:=YM-round(YM/10*2)-round(YC*U3[m,i]/maximum);
            Y2:=YM-round(YM/10*2)-round(YC*U3[m,i+1]/maximum);
          if (Color[1]='C')or(Color[1]='M') then SetColor(co((m mod 14)+2,Barvy))
			else SetColor(15);
          if (Color[1]='F')or(Color[1]='M') then SetLineStyle(m mod 3,0,s)
            else SetLineStyle(0,0,s);
          Line(X1,Y1,X2,Y2);
          SetColor(15);
        end;
     end;
   if (Minimum<0)and(i<pocvet) then
     begin
	   for m:=0 to PocZ do
        begin
            D:= XM-(XM div 10) - (XM div 4);  {delka osy Y}
            B:= D/(pocvet-1);                 {sirka jedne vety}
            X1:=round(XM div 10 + (i-1)*B);
            X2:=round(XM div 10 + i*B);
            YC:=round((YM-round(YM/10*5))/2);
            Y1:=YM-round(YM/10*2)-YC-round(YC*U3[m,i]/maximum);
			Y2:=YM-round(YM/10*2)-YC-round(YC*U3[m,i+1]/maximum);
          if (Color[1]='C')or(Color[1]='M') then SetColor(co((m mod 14)+2,Barvy))
            else SetColor(15);
          if (Color[1]='F')or(Color[1]='M') then SetLineStyle(m mod 3,0,s)
            else SetLineStyle(0,0,s);
          Line(X1,Y1,X2,Y2);
          SetColor(15);
        end;
     end;
       B:= D/(pocvet-1);
       X1:=round(XM div 10 + (i-1)*B);
	   LastColor:=GetColor;
       SetColor(KonB(ColorWWFor));
       if (PopX='H')or(PopX='h') then
        begin
         Y1:=YM-round(YM/10*1.5);
         SetTextJustify(CenterText,CenterText);
		 OutTextXYC(X1,Y1,UX[i]);
        end;
        if (PopX='V')or(PopX='v')or(PopX='I')or(PopX='i') then
          begin
           Y1:=YM-round(YM/10*1.8);
           SetTextJustify(CenterText,RightText);
           SetTextStyle(SmallFont,VertDir,K(4));
           OutTextXYC(X1,Y1,UX[i]);
          end;
        if Rastr='Y' then
          begin
            YC:=YM-round(YM/10*5);
            Y1:=YM-round(YM/10*2);
			Y2:=YM-round(YM/10*2)-YC;
            SetLineStyle(1,0,1);
            line(X1,Y1,X1,Y2);
          end;
		  SetColor(LastColor);
    end;
 end; { SloupDL }
end;

procedure SloupSB(var Param:Parametry;var Par3:Param3;var Maxim:Maxima);
var
  D,A,B:real;
  XMp,YMp,U3sum:real;
  XM,YM,X1,X2,Y1,YC,Y2,i:integer;
begin
  with Param,Par3,Maxim do
 begin
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  SetColor(KonB(ColorWWFor));

  SetTextJustify(LeftText,CenterText);
  SetTextStyle(SmallFont,HorizDir,K(4));
  SetFillStyle(1,1);
  U3sum:=0;
  D:= XM-(XM div 10) - (XM div 4);  {delka osy Y}
  A:= D/pocvet*(100-Pomer)/100; {sirka mezery}
  B:= D/pocvet-A;
  for i:=1 to Pocvet do
  begin
   if Minimum>=0 then
	 begin
	   for m:=PocZ downto 0 do U3sum:=U3sum+U3[m,i];
	   Y1:=YM-round(YM/10*2)-1;
	   for m:=0 to PocZ do
		begin
(*			D:= XM-(XM div 10) - (XM div 4);  {delka osy Y}
			A:= D/pocvet/10;                  {mezera mezi vetami}
			B:= D/pocvet-A;                   {sirka jedne vety}*)
			X1:=round(XM div 10 + A/2 + (i-1)*(B+A));
			X2:=round(XM div 10 + A/2 + B + (i-1)*(B+A));
			YC:=YM-round(YM/10*5);
			Y2:=Y1-round(YC*U3[m,i]/maximum);
		  if Color[1]='C' then SetFillStyle(1,co((m mod 14)+2,Barvy));
		  if Color[1]='F' then SetFillStyle(((m+1) mod 10)+1,15);
		  if Color[1]='M' then SetFillStyle(((m+1) mod 10)+1,co((m mod 14)+2,Barvy));
		  if Color[1]='N' then SetFillStyle(1,KonB(ColorWWBack));
		  if Color[1]='O' then SetFillStyle(1,co(2,Barvy));
          if Color[2]='3' then
           begin
             bar3Q(X1,Y1,X2,Y2,YC div 20,YC div 20,true);
           end
          else
		   begin
             bar(X1,Y1,X2,Y2);
			 rectangle(X1,Y1,X2,Y2);
           end;
		  Y1:=Y2;
        end;
	 end;
   if Minimum<0 then
     begin
        {chyba } beep;beep;
	 end;
	   if Color[2]='3' then OsyD(Param,Par3,Maxim);
	   LastColor:=GetColor;
	   SetColor(KonB(ColorWWFor));
	   if (PopX='H')or(PopX='h') then
		begin
			A:= D/pocvet*(100-Pomer)/100; {sirka mezery}
			B:= D/pocvet-A;
			X1:=round(XM div 10 + (i-1)*(B+A) + (B+A)/2);
			Y1:=YM-round(YM/10*1.5);
		 SetTextJustify(CenterText,CenterText);
		 OutTextXYC(X1,Y1,UX[i]);
		end;
		if (PopX='V')or(PopX='v')or(PopX='I')or(PopX='i') then
		  begin
			A:= D/pocvet*(100-Pomer)/100; {sirka mezery}
			B:= D/pocvet-A;
			X1:=round(XM div 10 + (i-1)*(B+A) + (B+A)/2);
			Y1:=YM-round(YM/10*1.8);
		   SetTextJustify(CenterText,RightText);
		   SetTextStyle(SmallFont,VertDir,K(4));
		   OutTextXYC(X1,Y1,UX[i]);
		end;
		SetColor(LastColor);

	end;
 end; { SloupSB }
end;

end.
