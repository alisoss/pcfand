{$A-}

type string1=string[1];   string10=string[10];
     string2=string[2];   string12=string[12];
     string3=string[3];   string20=string[20];
     string4=string[4];   string22=string[22];
     string5=string[5];   string30=string[30];
     string6=string[6];   string31=string[31];
     string7=string[7];   string11=string[11];
     string8=string[8];
     string9=string[9];

const MonoHidd=$00;     { Monochrom colors }
      MonoNorm=$02; MonoUndL=$01; MonoInvr=$70; MonoHiLi=$0A; MonoHiLiUndL=$09;
      MonoNormBl=$82; MonoUndLBl=$81; MonoInvrBl=$F0; MonoHiLiBl=$8A; MonoHiLiUndLBl=$89;

type  double=real;
      VerType=array[1..4]of char;
      TVideoFont = (foAscii,foLatin2,foKamen);
      SpecType=record UpdCount:byte;
                      AutoRprtWidth,AutoRprtLimit,CpLines:byte;
                      AutoRprtPrint,ChoosePrMsg,TxtInsPg:boolean;
                      TxtCharPg:char;
                      ESCverify:boolean;
                      NoPrompt158:boolean;
                      F10Enter:boolean;
                      RDBcomment:boolean;
                      CPMdrive:char;
                      RefreshDelay,NetDelay:word;
                      LockDelay, LockRetries: byte;
                      Beep,LockBeepAllowed:boolean;
                      XMSMaxKb:word;
                      NoCheckBreak:boolean;
                      KbdTyp:byte;
                      NoMouseSupport:boolean;
                      MouseReverse:boolean;
                      DoubleDelay:byte;
                      RepeatDelay:byte;
                      CtrlDelay:byte;
                      OverWrLabeledDisk:boolean;
                      ScreenDelay:word ;
                      OffDefaultYear:byte;
                      WithDiskFree : boolean;
               end;
      VideoType=record address:word;
                       TxtRows:byte ;
                       ChkSnow:boolean;
                       cursOn,cursOff,cursBig:word;
                end;
      ColorType=record
                    userColor:array[0..15] of byte;
                    mNorm,mHili,mFirst,mDisabled, {menu}
                    sNorm,sHili,sMask, {select}
                    pNorm,pTxt, {prompt,verify,password}
                    zNorm, {message}
                    lNorm,lFirst,lSwitch, {last line}
                    fNorm, {first line}
                    tNorm,tCtrl,tBlock, {text edit}
                    tUnderline,tItalic,tDWidth,tDStrike,tEmphasized,tCompressed,tElite,
                    dNorm,dHili,dSubset,dTxt,dDeleted,dSelect, {data edit}
                    uNorm, {user screen}
                    hNorm,hHili,hMenu,hSpec,
                    nNorm,
                    ShadowAttr,
                    DesktopColor:byte;
             end;
      FontsType=record VFont:TVideoFont ;
                       LoadVideoAllowed,NoDiakrSupported:boolean ;
                end ;
      CharTable=array[0..255] of byte;
      PrinterType=record Ul1,Ul2, {^S Underline}
                         Kv1,Kv2, {^N Italic}
                         Br1,Br2, {^Q Double-width}
                         Db1,Db2, {^D Double-strike}
                         Bd1,Bd2, {^B Emphasised}
                         Kp1,Kp2, {^E Compressed}
                         El1,El2, {^A Elite}
                         PrintReset,PageSizeNN, EndStrLaser,
                         LeftMargin, EndLeftMargin,
                         Us11,Us12,
                         Us21,Us22,
                         Us31,Us32,
                         Line72,Line216,
                         Den60,Den120,
                         Den240,Color,
                         PrClose: string11;
                  end;

const Version:VerType='4.02';
      Spec:SpecType=(UpdCount:20;
                     AutoRprtWidth:80; AutoRprtLimit:69; CpLines:3;
                     AutoRprtPrint:false;
                     ChoosePrMsg:false;
                     TxtInsPg:true;
                     TxtCharPg:#250;
                     ESCverify:false;
                     NoPrompt158:false;
                     F10Enter:false;
                     RDBcomment:true;
                     CPMdrive:' ';
                     RefreshDelay:5;NetDelay:54;
                     LockDelay:1; LockRetries:0;
                     Beep:true;
                     LockBeepAllowed:true;
                     XMSMaxKb:200;
                     NoCheckBreak:false;
                     KbdTyp:0;
                     NoMouseSupport:false;
                     MouseReverse:false;
                     DoubleDelay:8;
                     RepeatDelay:8;
                     CtrlDelay:15;
                     OverwrLabeledDisk:true;
                     ScreenDelay:18*60*3;
                     OffDefaultYear:0;
                     WithDiskFree:true);
      MonoVideo:VideoType=(address:$B000;
                           TxtRows:0 ;
                           ChkSnow:false;
                           cursOn:$0B0C; cursOff:$2000; cursBig:$000C);
      MonoColors:ColorType=(userColor:(MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm,
                                       MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm,MonoNorm);
                            mNorm:MonoNorm;mHili:MonoInvr;mFirst:MonoHili;mDisabled:MonoNorm;
                            sNorm:MonoInvr;sHili:MonoNorm;sMask:MonoInvr;
                            pNorm:MonoNorm;pTxt:MonoInvr;
                            zNorm:MonoHili;
                            lNorm:MonoNorm;lFirst:MonoHili;lSwitch:MonoInvr;
                            fNorm:MonoInvr;
                            tNorm:MonoNorm;tCtrl:MonoHili;tBlock:MonoInvr;
                            tUnderline:MonoUndL;tItalic:MonoNorm;tDWidth:MonoInvr;tDStrike:MonoHili;
                            tEmphasized:MonoHili;tCompressed:MonoInvr;tElite:MonoNorm;
                            dNorm:MonoNorm;dHili:MonoInvr;dSubset:MonoHili;
                            dTxt:MonoNorm;dDeleted:MonoInvr;dSelect:MonoInvr;
                            uNorm:MonoNorm;
                            hNorm:MonoNorm;hHili:MonoInvr;hMenu:MonoUndL;hSpec:MonoHili;
                            nNorm:MonoHili;
                            ShadowAttr:MonoInvr;
                            DesktopColor:MonoNorm);

      CGAVideo:VideoType=(address:$B800;
                          TxtRows:0 ;
                          ChkSnow:true;
                          cursOn:$0607; cursOff:$2000; cursBig:$0007);
      CGAcolors:ColorType=(userColor:($03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03);
                           mNorm:$30;mHili:$1E;mFirst:$34;mDisabled:$38;
                           sNorm:$70;sHili:$4E;sMask:$70;
                           pNorm:$30;pTxt:$03;
                           zNorm:$4F;
                           lNorm:$30;lFirst:$34;lSwitch:$03;
                           fNorm:$30;
                           tNorm:$07;tCtrl:$0F;tBlock:$70;
                           tUnderline:$02;tItalic:$03;tDWidth:$1E;tDStrike:$04;
                           tEmphasized:$05;tCompressed:$40;tElite:$06;
                           dNorm:$07;dHili:$70;dSubset:$0F;dTxt:$03;dDeleted:$30;dSelect:$0A;
                           uNorm:$03;
                           hNorm:$07;hHili:$1E;hMenu:$02;hSpec:$05;
                           nNorm:$78;
                           ShadowAttr:$08;
                           DesktopColor:$07);

      EGAVideo:VideoType=(address:$B800;
                          TxtRows:0 ;
                          ChkSnow:false;
                          cursOn:$0607; cursOff:$2000; cursBig:$0007);
      EGAcolors:ColorType=(userColor:($03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03);
                           mNorm:$30;mHili:$1E;mFirst:$34;mDisabled:$38;
                           sNorm:$70;sHili:$4E;sMask:$30;
                           pNorm:$30;pTxt:$03;
                           zNorm:$4F;
                           lNorm:$30;lFirst:$34;lSwitch:$03;
                           fNorm:$30;
                           tNorm:$07;tCtrl:$0F;tBlock:$70;
                           tUnderline:$0A;tItalic:$0B;tDWidth:$1E;tDStrike:$0C;
                           tEmphasized:$0D;tCompressed:$4E;tElite:$0E;
                           dNorm:$07;dHili:$70;dSubset:$0F;dTxt:$03;dDeleted:$30;dSelect:$0A;
                           uNorm:$03;
                           hNorm:$70;hHili:$1E;hMenu:$71;hSpec:$74;
                           nNorm:$70;
                           ShadowAttr:$08;
                           DesktopColor:$07);
      Fonts:FontsType=(VFont:foKamen;LoadVideoAllowed:true;
                       NoDiakrSupported:false);
      OrdKam:CharTable=

($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
 $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
 $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
 $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
 $40,$41,$42,$43,$45,$46,$47,$48,$49,$4b,$4c,$4d,$4e,$4f,$50,$51,
 { @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O }
 $52,$53,$54,$56,$58,$59,$5a,$5b,$5c,$5d,$5e,$75,$76,$77,$78,$79,
 { P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ }
 $7A,$41,$42,$43,$45,$46,$47,$48,$49,$4b,$4c,$4d,$4e,$4f,$50,$51,
 {  `  a   b   c   d   e   f   g   h   i   j   k   l   m   n   o }
 $52,$53,$54,$56,$58,$59,$5a,$5b,$5c,$5d,$5e,$7B,$7C,$7D,$7E,$7F,
 (*p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   *)
 $44,$59,$46,$45,$41,$45,$58,$44,$46,$46,$4e,$4b,$4e,$4e,$41,$41,
 { Ä   Å   Ç   É   Ñ   Ö   Ü   á   à   â   ä   ã   å   ç   é   è }
 $46,$5f,$5f,$51,$51,$51,$59,$59,$5d,$51,$59,$57,$4e,$5d,$55,$58,
 { ê   ë   í   ì   î   ï   ñ   ó   ò   ô   ö   õ   ú   ù   û   ü }
 $41,$4b,$51,$59,$50,$50,$59,$51,$57,$55,$55,$55,$ac,$ad,$ae,$af,
 { †   °   ¢   £   §   •   ¶   ß   ®   ©   ™   ´   ¨   ≠   Æ   Ø }
 $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
 $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
 $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
 $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
 $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

      UpcKam:CharTable=

(  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
  64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
 { @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O }
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
 { P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ }
  96, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
 { '   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o }
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,123,124,125,126,127,
 (*p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   *)
 128,154,144,133,142,133,134,128,137,137,138,139,156,138,142,143,
 { Ä   Å   Ç   É   Ñ   Ö   Ü   á   à   â   ä   ã   å   ç   é   è }
 144,146,146,167,153,149,166,151,157,153,154,155,156,157,158,134,
 { ê   ë   í   ì   î   ï   ñ   ó   ò   ô   ö   õ   ú   ù   û   ü }
 143,139,149,151,165,165,166,167,155,158,171,171,172,173,174,175,
 { †   °   ¢   £   §   •   ¶   ß   ®   ©   ™   ´   ¨   ≠   Æ   Ø }
 176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,
 208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,
 240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255);

      prMax : byte=1 ;
      prName:string[8]='STANDARD';
      Ul1:string3=^['-'#1; Ul2:string3=^['-'#0; {^S Underline}
      Kv1:string2=^['4';   Kv2:string2=^['5';   {^N Italic}
      Br1:string3=^['W'#1; Br2:string3=^['W'#0; {^Q Double-width}
      Db1:string2=^['G';   Db2:string2=^['H';   {^D Double-strike}
      Bd1:string2=^['E';   Bd2:string2=^['F';   {^B Emphasised}
      Kp1:string1=#15;     Kp2:string1=#18;     {^E Compressed}
      El1:string2=^['M';   El2:string2=^['P';   {^A Elite}
      PrintReset:string2=^['@';
      PageSizeNN:string2=^['C';
      EndStrLaser:char= #0 ;
      LeftMarg:string2=^['l';
      EndLeftMarg:char= #0 ;
      Us11:string3=#$1B#$68#$01; Us12:string3=#$1B#$68#$00;
      Us21:string3=#$1B#$68#$02; Us22:string3=#$1B#$68#$00;
      Us31:string2=#$1B#$30; Us32:string2=#$1B#$32;
      Line72:string2=#$1B#$41; Line216:string2=#$1b#$33 ;
      Den60:string2=#$1B#$4B; Den120:string2=#$1b#$59 ;
      Den240:string2=#$1B#$5A; Color:string5='((C))';
      PrClose:char= #0;

      zero:byte=$FF;
      pType:char='M';
      PrinterCode:Char=' ';     { hodnoty ' ', 'L', 'S' }
      No:byte=1 ;
      pTmOut:byte=7;

      NWDaysTab:word=12; WDaysFirst:double=726103; WDaysLast:double=726462;
      WDaysTab:array[1..12]of record Typ:byte;Nr:word end=
      ((Typ:3;Nr:0   {1.1.}), (Typ:0;Nr:69 {11.3.}), (Typ:3;Nr:84 {26.3.}),
       (Typ:3;Nr:85 {27.3.}), (Typ:3;Nr:120 {1.5.}), (Typ:0;Nr:125 {6.5.}),
       (Typ:1;Nr:126 {7.5.}), (Typ:2;Nr:127 {8.5.}), (Typ:3;Nr:128 {9.5.}),
       (Typ:3;Nr:300{28.10.}),(Typ:3;Nr:358{25.12.}),(Typ:3;Nr:359{26.12.}));

      InstallACode:byte=1;


function PtrToAddr (p:pointer):LongInt;
  begin PtrToAddr:=(LongInt(seg(p^)) shl 4)+ofs(p^) end;

var l:word; f:file;

begin l:=PtrToAddr(@InstallACode)-PtrToAddr(@Version)+1;
      writeln('Create FAND.CFG at length ',l);
      assign(f,'FAND.CFG'); rewrite(f,1);
      BlockWrite(f,Version,l); close(f);
end.
