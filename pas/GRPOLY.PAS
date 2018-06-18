unit GrPoly;

{$A-,B-,D-,F+,G-,I+,L-,O+,R-,S+,V+,X+}

interface
  uses GrGlob;
  procedure PolyLin(var Param:Parametry;var Maxim:Maxima);

implementation
  uses dos,graph,base;


{******************************************************************}
procedure PolyLin(var Param:Parametry;var Maxim:Maxima);

const
  maxP = 32;

type
  TAPtr=^TA;
  TA = array [1..maxP+1,1..maxP+1] of real;
  TB = array [1..maxP+1] of real;
  TM = typUY;

var
  XMp,YMp,pol:real;
  XM,YM,i,j:integer;
  s,c:word;
  B:TB;
  st,stZ:integer;
  Yv:real;
  col:char;
  A:TAPtr;
  PP:pointer;
  Patt:byte;

procedure Polynom(X,Y:TM;n:integer; var stup:integer);

var
  R:array [1..max] of real;
  i,j,k,np:integer;
  M,OM,G,AX,G1,P,AL,BT,DL,pp:real;
  C:TAPtr;


begin
  C:=GetStore(SizeOf(TA));
  for i:=1 to MaxP+1 do for j:=1 to MaxP+1 do C^[i,j]:=0;
  A^[1,1]:=1;M:=0;OM:=0;G:=0;
  for i:=1 to n do
	begin
	  M:=M+X[i];
      G:=G+1;
      OM:=OM+Y[i];
	end;
  B[1]:=OM/G;
  A^[2,1]:=-M/G;
  A^[2,2]:=1;
  for i:=1 to n-1 do
    begin
      AX:=0;
      G1:=0;
      OM:=0;
	  for k:=1 to n do
        begin
		  P:=A^[i+1,i+1];
		  for j:=i-1 downto 0 do P:=P*X[k]+A^[i+1,j+1];
          G1:=P*P+G1;
          OM:=OM+P*Y[k];
          AX:=X[k]*P*P+AX;
		  C^[i+1,k+1]:=P;
		end;
	  AL:=AX/G1;
      BT:=G1/G;
      B[i+1]:=OM/G1;
      G:=G1;
      A^[i+2,1]:=-AL*A^[i+1,1]-BT*A^[i,1];
      for j:=1 to i-1 do A^[i+2,j+1]:=A^[i+1,j]-AL*A^[i+1,j+1]-BT*A^[i,j+1];
      A^[i+2,i+1]:=A^[i+1,i]-AL*A^[i+1,i+1];
      A^[i+2,i+2]:=A^[i+1,i+1];
	end;
  for k:=1 to n do R[k]:=Y[k]-B[1];
  pp:=0;
  for j:=0 to n-2 do
	begin
	  DL:=0;
	  for k:=1 to n do
		begin
		  DL:=DL+R[k]*R[k];
		  R[k]:=R[k]-B[j+2]*C^[j+2,k+1];
		end;
{      writeln(lst,'sigma ',j,' kvadrat ',DL/(n-j-1));}
	   stup:=0;
	   if DL/(n-j-1)<Maxim.Maximum/10 then stup:=j;
	end;
	pp:=-100001;	np:=0; { zjisti pocet bodu na ose x - neopakujici se hodnoty }
	for j:=1 to n do
	 if X[j]<>pp then
	  begin inc(np);pp:=X[j];end;
	if (stup=0)or(stup>4) then stup:=4;
	if StupPoly<0 then StupPoly:=0;
	if stup>=np then stup:=np-1;
	if StupPoly>=np then StupPoly:=np-1;
	if StupPoly>0 then stup:=StupPoly ;
	StupPolyP:=Stup;
end;  { end Polynom }

function PolyHod( xv:real;stup:integer):real;
var
  yv,P:real;
  i,j:integer;

begin
  yv:=B[1];
  for i:=1 to stup do
    begin
      P:=A^[i+1,i+1];
	  for j:=i-1 downto 0 do P:=P*xv+A^[i+1,j+1];
      yv:=yv+B[i+1]*P;
    end;
  PolyHod:=yv;
end; { end PolyHod }

procedure PutPixelBig(X,Y:integer;Pixel:word);
begin
  putpixel(X  ,Y,Pixel);
  putpixel(X-1,Y,Pixel);
  putpixel(X+1,Y,Pixel);
  putpixel(X  ,Y-1,Pixel);
  putpixel(X-1,Y-1,Pixel);
  putpixel(X+1,Y-1,Pixel);
  putpixel(X  ,Y+1,Pixel);
  putpixel(X-1,Y+1,Pixel);
  putpixel(X+1,Y+1,Pixel);
end;

begin                            { Procedure PolyLin }
  with Param,Maxim do
 begin
  MarkStore(PP);
  A:=GetStore(SizeOf(TA));
  for i:=1 to MaxP+1 do for j:=1 to MaxP+1 do A^[i,j]:=0;
  XM:=GMaxX;
  YM:=GMaxY;
  XMp:=XM;
  YMp:=YM;
  c:=15;
  col:=Color[1];
  if GrDriv=HercMono then Color[1]:='F';
  if Pomer<50 then s:=1 else s:=3;
  if Pocvet<2 then exit;
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


  for i:=1 to Pocvet do           { zakresleni bodu }
  begin
{ ---------------- kladny graf }
   if Minimum>=0 then
	begin
	if(Color[1]='C')or(Color[1]='M') then SetColor(co(2,Barvy))
	 else SetColor(14);
	 SetLineStyle(0,0,s);
     line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5))-s-2,
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum),
          round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5))+s+2,
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum));
     line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum)-s-2,
		  round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/maximum)+s+2);

     if UdajZ<>'' then
      begin
    if(Color[1]='C')or(Color[1]='M') then SetColor(co(3,Barvy))
     else SetColor(14);
	   SetLineStyle(0,0,s);
       line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5))-s-2,
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/maximum),
		  round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5))+s+2,
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/maximum));
       line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/maximum)-s-2,
          round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/maximum)+s+2);

      end;
	end;
                         { -------------  zaporny graf }
   if Minimum<0 then
     begin
      pol:=(YM-YM/4)/2+1;
      if(Color[1]='C')or(Color[1]='M') then SetColor(co(2,Barvy))
       else SetColor(14);
	  SetLineStyle(0,0,s);
      line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)-s-2),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/(2*maximum)-pol),
				  round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5))+s+2,
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/(2*maximum)-pol));
      line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/(2*maximum)-pol-s-2),
                  round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UY[i]/(2*maximum)-pol)+s+2);
      if UdajZ<>'' then
       begin
		if(Color[1]='C')or(Color[1]='M') then SetColor(co(3,Barvy))
         else SetColor(14);
		SetLineStyle(0,0,s);
        line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)-s-2),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/(2*maximum)-pol),
                  round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5))+s+2,
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/(2*maximum)-pol));
        line(round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
                  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/(2*maximum)-pol-s-2),
                  round(XMp/10 +(UL[i]-UL[1])/(UL[pocvet]-UL[1])*(XMp-XMp/5)),
				  round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*UZ[i]/(2*maximum)-pol)+s+2);
       end;
     end;
    end;

  Polynom(UL,UY,PocVet,st);       { polynomicka regrese 1 }

  for i:=1 to round(XMp-XMp/5) do
  begin
                     { ---------------- kladny graf }
   if Minimum>=0 then
    begin
	 SetLineStyle(0,0,s);
    if(Color[1]='C')or(Color[1]='M') then c:=co(2,Barvy)
     else c:=14;
     Yv:=PolyHod(UL[1]+i*(UL[pocvet]-UL[1])/(XMp-XMp/5),st);
	 if abs(Yv)<=maximum*1.01 then
     if Pomer<50 then
	   putpixel(round(XMp/10) + i,
                round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/maximum),c)
      else
       putpixelbig(round(XMp/10) + i,
                round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/maximum),c);

    end;


						 { -------------  zaporny graf }
   if Minimum<0 then
	 begin
	  pol:=(YM-YM/4)/2+1;
	 if(Color[1]='C')or(Color[1]='M') then c:=co(2,Barvy);
	  SetLineStyle(0,0,s);
	  Yv:=PolyHod(UL[1]+i*(UL[pocvet]-UL[1])/(XMp-XMp/5),st);

	  if abs(Yv)<=maximum*1.01 then
	  if Pomer<50 then
        putpixel(round(XMp/10) + i,
                round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/(2*maximum)-pol),c)
       else
        putpixelbig(round(XMp/10) + i,
                round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/(2*maximum)-pol),c);

	 end;
	end;

  if UdajZ<>'' then
  begin
  Polynom(UL,UZ,PocVet,st);       { polynomicka regrese 2 }

  for i:=1 to round(XMp-XMp/5) do
  begin
					 { ---------------- kladny graf }
   if Minimum>=0 then
	begin
	 SetLineStyle(Patt,0,s);
	 if(Color[1]='C')or(Color[1]='M') then c:=co(3,Barvy);
	Yv:=PolyHod(UL[1]+i*(UL[pocvet]-UL[1])/(XMp-XMp/5),st);

	 if abs(Yv)<=maximum*1.01 then
	 if Pomer<50 then
	  if (i mod 4 = 1)or(i mod 4 = 2)or(Patt=0) then
	   putpixel(round(XMp/10) + i,
				round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/maximum),c) else
	  else
	  if (i mod 8 = 1)or(i mod 8 = 2)or(i mod 8 = 3)or(i mod 8 = 4)or(Patt=0) then
	   putpixelbig(round(XMp/10) + i,
				round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/maximum),c);

	end;


						 { -------------  zaporny graf }
   if Minimum<0 then
	 begin
	  pol:=(YM-YM/4)/2+1;
	  SetLineStyle(0,0,s);
	  if(Color[1]='C')or(Color[1]='M') then c:=co(3,Barvy);
	  Yv:=PolyHod(UL[1]+i*(UL[pocvet]-UL[1])/(XMp-XMp/5),st);
	  if abs(Yv)<=maximum*1.01 then
	  if Pomer<50 then
	   if (i mod 4 = 1)or(i mod 4 = 2)or(Patt=0) then
		putpixel(round(XMp/10) + i,
				round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/(2*maximum)-pol),c) else
	   else
	   if (i mod 8 = 1)or(i mod 8 = 2)or(i mod 8 = 3)or(i mod 8 = 4)or(Patt=0) then
		putpixelbig(round(XMp/10) + i,
				round(YMp/8+ YMp-YMp/4 -(YMp-YMp/4)*Yv/(2*maximum)-pol),c);

	 end;
	end;
	end;
	Color[1]:=col;
	ReleaseStore(PP);
 end; { PolyLin }
end;
end.

