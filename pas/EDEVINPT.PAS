{ function MyTestEvent - in EDSCREEN }

function MyGetEvent:boolean;
{$S-}
  procedure CtrlShiftAlt;
    procedure ScrollPress;
      type BP=^byte;
      var old,fyz:boolean;
          L1:longint;
      begin old:=Scroll;fyz:=((BP(ptr(0,$417))^ and $10)<>0);
        if fyz=old then FirstScroll:=false;
        Scroll:=(fyz or FirstScroll)and(Mode<>HelpM);
        HelpScroll:=Scroll or(Mode=HelpM);L1:=LineAbs(ScrL);
        if old<>Scroll then
          begin if Scroll then
                  begin WrStatusLine;TestKod;CrsHide;
                    PredScLn:=LineAbs(LineL);PredScPos:=Posi;
                    if UpdPHead then
                      begin SetPart(1);SimplePrintHead;
                        DekFindLine(MaxL(L1,PHNum+1)) end else
                      begin DekFindLine(MaxL(L1,PHNum+1)) end;
                    ScrL:=LineL;RScrL:=NewRL(ScrL);
                    if L1<>LineAbs(ScrL) then ChangeScr:=true;{DekodLine;}
                    BCol:=Column(BPos);Colu:=Column(Posi);
                    ColScr:=Part.ColorP;SetColorOrd(ColScr,1,ScrI) end else
                  begin
{                     if (PredScLn in[L1..L1+PageS-1])and
                       (PredScPos in [BPos+1..BPos+LineS]) then
                      PosDekFindLine(PredScLn,PredScPos)
                    else begin PosDekFindLine(L1,BPos+1);ScrL:=LineL end;}
{ ????                   if not(PredScLn in[L1..L1+PageS-1]) then PredScLn:=L1;}
                    if (PredScLn<L1)or(PredScLn>=L1+PageS) then PredScLn:=L1;
                    if not(PredScPos in [BPos+1..BPos+LineS]) then
                      PredScPos:=BPos+1;
                    PosDekFindLine(PredScLn,PredScPos,false);
                    if Mode in [ViewM,SinFM,DouFM,DelFM,NotFM] then CrsBig
                    else CrsNorm end;
            Background;
          end;
      end;
    procedure DisplLL(Flags:word);
      begin
        if Flags and $04<>0 then              { Ctrl }
          WrLLMargMsg(CtrlLastS,CtrlLastNr)
        else if Flags and $03<>0 then         { Shift }
          WrLLMargMsg(ShiftLastS,0)
        else if Flags and $08<>0 then          { Alt }
          WrLLMargMsg(AltLastS,0)
      end;
    var Ctrl:boolean; Delta,flgs:word;
    label 1;
    begin Ctrl:=false;  Delta:=0;flgs:=0;
(*    1:MyTestEvent;*)
      1:WaitEvent(Delta);
      {1:}if Mode<>HelpM then ScrollPress;
      if LLKeyFlags<>0 then      { mouse }
        begin flgs:=LLKeyFlags;DisplLL(LLKeyFlags);Ctrl:=true end
      else
      if KbdFlgs and $0F<>0 then { Ctrl Shift Alt pressed }
        begin if not Ctrl then
          if Delta>0 then begin flgs:=KbdFlgs;DisplLL(KbdFlgs);Ctrl:=true end
          else Delta:=spec.CtrlDelay
        end
      else if Ctrl then begin flgs:=0;WrLLMargMsg(LastS,LastNr);
                          Ctrl:=false;Delta:=0 end;
{      WaitEvent(Delta);}
      if not (Event.What in [evKeyDown,evMouseDown]) then
         begin ClrEvent;if not IsWrScreen then Background;goto 1 end;
      if flgs<>0 then begin LLKeyFlags:=0;WrLLMargMsg(LastS,LastNr);
                        AddCtrlAltShift(flgs) end;
    end;
{$S+}
  const OrigS:string[4]='    ';
  procedure Wr(s:string);
    begin if Mode<>HelpM then
            begin if s='' then s:=OrigS else
                    begin ScrRdBuf(0,0,OrigS[1],2);move(OrigS[3],OrigS[2],1);
                      OrigS[0]:=chr(2) end;
              ScrWrStr(0,0,s,SysLColor) end end;
  function My2GetEvent:boolean;
    begin ClrEvent;GetEvent;
      if Event.What<>evKeyDown then
        begin ClrEvent;My2GetEvent:=false;exit end;
      with Event do if upcase(chr(KeyCode)) in['A'..'Z'] then
        begin KeyCode:=ord(upcase(chr(KeyCode)))-ord('@');
          if (KeyCode in[_Y_,_Z_])and(spec.KbdTyp in [CsKbd,SlKbd]) then
            case KeyCode of _Z_:KeyCode:=_Y_;_Y_: KeyCode:=_Z_ end;
        end;
      My2GetEvent:=true;
    end;

  var ww:word;
  function ScrollEvent:boolean;
    var X:EdExitDPtr;
    begin ScrollEvent:=false;
    if Event.What<>evKeyDown then exit;
      with Event do case KeyCode of
        _ESC_,_left_,_right_,_up_,_down_,_pgup_,_pgdn_,
        _ctrlpgup_,_ctrlpgdn_,{_ctrlhome_,_ctrlend_,}_CtrlF5_,_altF8_
        :ScrollEvent:=true
      else if (lo(KeyCode)=$00)and(pos(chr(hi(KeyCode)),Breaks)<>0) then
             ScrollEvent:=true else
             begin X:=ExitD;
               while X<>nil do if TestExitKey(KeyCode,X) then
                 begin ScrollEvent:=true;exit end else X:=X^.Chain
             end;
      end;
    end;
  function ViewEvent:boolean;
    begin ViewEvent:=ScrollEvent;
    if Event.What<>evKeyDown then exit;
      case Event.KeyCode of
        _QF_,_L_,_F7_,_F8_,_KP_,_QB_,_QK_,_CtrlF5_,_AltF8_,_CtrlF3_,
        _home_,_end_,_ctrlleft_,_ctrlright_,_QX_,_QE_,_Z_,_W_,
        _CtrlF6_,
        _KW_,_KB_,_KK_
        :ViewEvent:=true end;
    end;
  function HelpEvent:boolean;
    begin HelpEvent:=false;
    if Event.What=evKeyDown then
      with Event do case KeyCode of
        _ESC_,_left_,_right_,_up_,_down_,_pgdn_,_pgup_,_M_:HelpEvent:=true
        else if (lo(KeyCode)=$00)and(pos(chr(hi(KeyCode)),Breaks)<>0) then
          HelpEvent:=true;
      end;
    if Event.What=evMouseDown then HelpEvent:=true;
    end;
  begin CtrlShiftAlt;
(***Prekodovani klaves***)
    GetEvent;
    if Event.What=evKeyDown then with Event do case KeyCode of
    _S_:KeyCode:=_left_;                       _D_:KeyCode:=_right_;
    _E_:KeyCode:=_up_;                         _X_:KeyCode:=_down_;
    _R_:KeyCode:=_pgup_;                       _C_:KeyCode:=_pgdn_;
    _A_:KeyCode:=_ctrlleft_;                   _F_:KeyCode:=_ctrlright_;
    _V_:KeyCode:=_ins_;
    _P_:begin Wr('^P');ww:=KeyCode;
          if My2GetEvent then
            begin Wr('');
              if KeyCode<=$31 then KeyCode:=(ww shl 8)or KeyCode;
            end;
        end;
    _Q_:begin Wr('^Q');ww:=KeyCode;
          if My2GetEvent then
            begin Wr('');
              case KeyCode of
                _S_:KeyCode:=_home_;               _D_:KeyCode:=_end_;
                _R_:KeyCode:=_ctrlpgup_;           _C_:KeyCode:=_ctrlpgdn_;
                _E_,_X_,_Y_,_L_,_B_,_K_,_I_,_F_,_A_,$2D{-},$2F{/},$3D{=}
                  :KeyCode:=(ww shl 8)or KeyCode;
                else KeyCode:=0;
              end;
            end;
        end;
    _K_:begin Wr('^K');ww:=KeyCode;
          if My2GetEvent then
            begin Wr('');
{              case KeyCode of
                _B_:KeyCode:=_F7_;                     _K_:KeyCode:=_F8_;
                _B_,_K_,_H_,_S_,_Y_,_C_,_V_,_W_,_R_,_P_,_F_,_U_,_L_,_T_
                  :KeyCode:=(ww shl 8)or KeyCode;
                else KeyCode:=0;
              end;}
              if KeyCode in[_B_,_K_,_H_,_S_,_Y_,_C_,_V_,_W_,_R_,_P_,_F_,_U_,_L_,_N_]
              then KeyCode:=(ww shl 8)or KeyCode else KeyCode:=0;
            end;
        end;
    _O_:begin Wr('^O');ww:=KeyCode;
          if My2GetEvent then
            begin Wr('');
              case KeyCode of
                _W_,_R_,_L_,_J_,_C_:KeyCode:=(ww shl 8)or KeyCode;
                else KeyCode:=0;
              end;
            end;
        end;
    end;
(***Rezim-test***)
    case Mode of
      HelpM:MyGetEvent:=HelpEvent;
      ViewM:if Scroll then MyGetEvent:=ScrollEvent else MyGetEvent:=ViewEvent;
      TextM:if Scroll then MyGetEvent:=ScrollEvent else MyGetEvent:=true;
    end
  end;
