# Ostatní hesla

<a name="t875"></a>
## Windows

<pre>
                                 <b>WINDOWS</b>
   ──────────────────────────────────────────────────────────────────────────

   Sítě Windows For Workgroups (WFW) a Windows 95, případně smíšené jsou
   typu peer to peer. Přesto lze z důvodů bezpečnosti doporučit vyhrazení 
   jednoho počítače jako datového serveru. Není třeba používat program SHARE.

   Určitou slabinou (oproti Novellu) je nedostatek informací o interních
   strukturách systému (.INI soubory), nutných pro řešení občasných problémů.

   Známé problémy:

   ■ Při tisku na <b>síťovou</b> tiskárnu pod WFW nelze nastavit timeout. Jde o
     analogii parametru TI novelské utility CAPTURE. Implicitně platí 45 sec.
     což znamená, že tisk nastane až zhruba minutu po té, co byl odeslán z
     PC FANDu (aplikace).
     Od verze PC FANDu 4.0 je toto řešeno nastavením FANDINST - TISKARNY -
     - TIMEOUT=255. Viz. <a href="prostredi.md#t222">instalace tiskárny</a>.

   ■ Občasné problémy při tisku nastávají i ve WIN95. Většinou jde o ztrátu
     několika znaků na konci sestavy. Není pozorován ani dokumentován 
     jednoznačný vliv instalačních parametrů tiskárny ve WIN95 na tyto
     problémy. Většinou pomůže znovu instalovat tiskárnu a při tom si dát
     pozor na povolení tisku z DOS-aplikací.

     Nastavení tiskárny -  volba Start / Settings / Printer / Properties /
                           Details.
     Pozor na:
     Port  Settings / MS-DOS print  job properties   (doporučujeme vypnout)
     Port  Settings /  Check port before printing (zapnout)
     Timeout settings - čekací doby (např. 15 a 45)

   ■ Nastavení HARDWARE, doporučujeme projít nastavení BIOSu pomocí <b>SETUP</b>u.
     U nových desek věnujte pozornost volbě "Peripheral Setup" z voleb "SPP"
     (standard pararel port), "ECP" (extended capabilities port)  vyzkoušejte
     "ECP". (CHIP 2/96,str.144)

   ■ Při připojení stanice s WIN95 k serveru Novell nelze sdílet RdOnly 
     soubory (např. úlohy). Jde o stejný problém, který se řeší v konfigurač-
     ním souboru NET.CFG parametrem READ ONLY COMPATIBILITY=OFF. Ten ovšem 
     nelze v distribuční verzi WIN95 nastavit.
     Řešení : - Ve FANDu 4.0 se úlohy při provedení otevírají rovnou RdOnly
                bez testování atributů souboru.
              - Nové 32-bit. drivery pro Novell pod WIN95 jsou dostupné na
                Internetu. V nich lze parametr Read Only Compatibility nastavit.

   ■ DOS - klient pro sítě WINxy má více variant. Při použití základní verze
     NET START <b>BASIC</b> mohou být problémy s textovými soubory, pokud jsou jinde 
     než na lokálním disku.
     Startujte proto síť pomocí NET START <b>FULL</b> nebo je třeba v souboru
     SYSTEM.INI nastavit správné implicitní hodnoty pro klienta, to jest
     [network]
     AutoStart=Full
     PreferredRedir=Full

   ■ Windows 95 / 98
     Při problémech s datovými soubory zkuste nastavení parametrů:
     Ovládací panely -&gt; Systém -&gt; Výkon -&gt; Systém souborů -&gt;
     Co dělat v nesnázích.
     Zde naleznete 6 zaškrtávacích polí, jejichž nastavením lze ovlivnit
     souborový systém Windows. Obecné optimální nastavení není známo,
     je třeba prostě něco změnit a ověřit chování aplikace.

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t874">Novell</a>
</pre>
