# Procedury (kapitola P) a jejich příkazy

<a name="t263"></a>
## Příkazy procedury

<pre>
                             <b>PŘÍKAZY PROCEDURY</b>
    ────────────────────────────────────────────────────────────────────────

    <b>Přiřazovací příkaz</b>       ObjektPřiřazení <b>:=</b> Výraz
                             ObjektPřiřazení <b>+=</b> Výraz  (a+=b odpovídá a:=a+b)

    <b>Řídící příkazy</b>           složený příkaz   <a href="#t275">Begin</a>, <a href="#t275">End</a>
                             skoky            <a href="#t422">Break</a>, <a href="#t422">Cancel</a>, <a href="#t288">Exit</a>
                             větvení          <a href="#t433">Case</a> , <a href="#t278">If</a>
                             cykly            <a href="#t427">For</a>, <a href="#t441">ForAll</a>, <a href="#t427">Repeat</a>, <a href="#t427">While</a>

    <b>Komunikace s uživatelem</b>  zvuky            <a href="#t487">Beep</a>, <a href="#t487">Sound</a>, <a href="#t487">NoSound</a>
                             čekání           <a href="#t487">Delay</a>, <a href="#t487">Wait</a>
                             klávesnice       <a href="#t487">ClearKeyBuf</a>, <a href="#t487">SetKeyBuf</a>
                             myš              <a href="#t495">SetMouse</a>

    <b>Výstup na obrazovku</b>      <a href="#t474">With Window</a>, <a href="#t466">ClrScr</a>, <a href="#t466">ClrEol</a>, <a href="prostredi.md#t457">Display</a>, <a href="#t459">GoToXY</a>,
                             <a href="prostredi.md#t457">Help</a>, <a href="prostredi.md#t457">HeadLine</a>, <a href="prostredi.md#t457">Message</a>, <a href="#t302">Write</a>, <a href="#t302">WriteLn</a>

    <b>Uživatelské nabídky</b>      <a href="#t451">Menu</a>, <a href="#t451">MenuLoop</a>, <a href="#t451">MenuBar</a>

    <b>Editace a uložení změn</b>   <a href="prostredi.md#t282">Edit</a>
    <b>Sestava</b>                  <a href="formular-sestava.md#t556">Report</a>
    <b>Transformace</b>             <a href="formular-sestava.md#t566">Merge</a>
    <b>Práce s texty</b>            <a href="editory.md#t595">EditTxt</a>, <a href="#t588">PrintTxt</a>, <a href="#t721">PutTxt</a>
    <b>Vyvolání procedury</b>       <a href="#t436">Proc</a>
    <b>Třídění a indexování</b>     <a href="#t286">Sort</a>, <a href="#t501">IndexFile</a>, <a href="#t415">GetIndex</a>

    <b>Přístup k větám</b>          <a href="#t572">AppendRec</a>, <a href="#t572">DeleteRec</a>, <a href="#t572">RecallRec</a>, <a href="#t403">ReadRec</a>,
                             <a href="#t404">WriteRec</a>, <a href="#t574">LinkRec</a>

    <b>Katalog,diskové jednotky</b> <a href="#t506">Close</a>, <a href="#t303">Save</a>, <a href="#t631">Mount</a>, <a href="#t631">ReleaseDrive</a>
                             <a href="#t640">TurnCat</a>, <a href="prostredi.md#t633">ResetCatalog</a>, <a href="#t629">CheckFile</a>

    <b>Archivace</b>                <a href="#t613">Backup</a>, <a href="#t614">Restore</a>, <a href="#t620">BackupM</a>, <a href="#t620">RestoreM</a>
    <b>Externí programy</b>         <a href="#t626">Exec</a>
    <b>Kopie a převody souborů</b>  <a href="prostredi.md#t609">CopyFile</a>

    <b>Grafika</b>                  <a href="#t751">With Graphics</a>, <a href="rozhrani.md#t753">Graph</a>, <a href="#t463">GetMaxX</a>, <a href="#t463">GetMaxY</a>,
                             <a href="#t745">PutPixel</a>, <a href="#t293">Line</a>, <a href="#t747">Ellipse</a>, <a href="#t745">Rectangle</a>, <a href="#t746">FloodFill</a>,
                             <a href="#t748">OutTextXY</a>

    <b>Práce s porty</b>            <a href="#t498">PortOut</a>

    <b>Síťové příkazy</b>           <a href="prostredi.md#t867">With Shared</a>, <a href="prostredi.md#t871">With Locked</a>

    ────────────────────────────────────────────────────────────────────────
</pre>

<a name="t275"></a>
## end

*Též:* `begin`

<pre>
                  <b>BEGIN</b>, <b>END</b> - duplicitní klíčová slova
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>begin</b> <b>end</b> ... <a href="#t428">řídící příkazy</a>: programové závorky (složený příkaz)
               ... <a href="prostredi.md#t379">PopisnáČástÚrovně</a>: výpočetní část v tiskových sestavách
               ... <a href="formular-sestava.md#t396">PříkazováČástVýstupu</a>: výstup transformace

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t278"></a>
## else

*Též:* `if`, `then`

<pre>
               <b>IF</b>, <b>THEN</b>, <b>ELSE</b> - duplicitní klíčová slova
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>if then else</b> ... <a href="#t433">větvení programu</a>: podmíněný příkaz jestliže-pak-jinak
                  ... <a href="formular-sestava.md#t396">PříkazováČástVýstupu</a>: transformace
                  ... <a href="prostredi.md#t379">PopisnáČástÚrovně</a>: výpočetní část v tiskových sestavách

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t286"></a>
## sort

<pre>
                      <b>SORT</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ report(...<b>sort</b>) ..... <a href="prostredi.md#t562">parametry automatické sestavy</a>: dočasné třídění
                                                          v sestavě
   ■ <b>sort(...)</b> ........... <a href="#t567">třídění</a>: třídění vyvolané z procedury

   ■ <a href="#t415">getindex</a>(...<b>sort</b>) ... vytvoření pracovního indexu

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t288"></a>
## exit

<pre>
                      <b>EXIT</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>exit</b> ................. <a href="#t428">řídící příkazy</a>: ukončení procedury
   ■ edit(...<b>exit</b>...) ..... <a href="editory.md#t536">přerušení editace</a>: exit-procedura
   ■ edittxt(...<b>exit</b>...) .. <a href="editory.md#t596">přerušení editace textu</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t293"></a>
## line

<pre>
                      <b>LINE</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>line</b>, <b>.line</b> .... číslo vystupujícího řádku, <a href="jazyk.md#t383">speciální funkce v sestavě</a>
   ■ <b>line(....)</b> ..... příkaz pro kreslení čáry mezi dvěma krajními body
                     <a href="#t745"> line</a>
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t300"></a>
## nocancel

<pre>
                    <b>NOCANCEL</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   Parametr <b>NoCancel</b> vesměs zajišťuje, že havárie při provedení příkazu 
   neukončí celý program ale program pokračuje za příkazem a je nastaveno
   <a href="#t626">exitcode</a>&lt;&gt;0 .

   ■ <a href="#t626">exec</a>(...,<b>nocancel</b>)................. volání externího programu
   ■ <a href="prostredi.md#t609">copyfile</a>(...,<b>nocancel</b>) ............ kopírování  souborů
   ■ <a href="#t631">mount</a>(...,<b>nocancel</b>) ............... vložení diskety do jednotky
   ■ <a href="#t614">restore</a>,<a href="#t613">backup</a>(...,<b>nocancel</b>)
     <a href="#t620">restorem</a>, <a href="#t620">backupm</a>(...,<b>nocancel</b>) ... zálohování dat

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t302"></a>
## writeln

*Též:* `write`

<pre>
                 <b>WRITE, WRITELN</b> - duplicitní klíčová slova
   ──────────────────────────────────────────────────────────────────────────

   Příkazy <b>write</b> a <b>writeln</b> provedou výpis údajů na obrazovku, jejich syntaxe
   je však různá v kapitole P (procedury) a L (logic).

   ■ <b>write</b>(...)............. <a href="#t459">výstup na obrazovku</a> (procedury)
   ■ <b>write</b>(...) ............ výstup na obrazovku - kapitola L <a href="dalsi-kapitoly.md#t911">write*</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t303"></a>
## save

<pre>
                      <b>SAVE</b> - duplicitní klíčová slova
   ──────────────────────────────────────────────────────────────────────────

   ■<a href="#t506"> save</a>............. příkaz pro uložení změn souborů (z cache na disk)
   ■ <b>save</b>(...)........ PROLOG - <a href="dalsi-kapitoly.md#t916">uložení databáze</a> do údaje typu T

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t403"></a>
## readrec

<pre>
                       <b>READREC</b> - načtení věty souboru
   ───────────────────────────────────────────────────────────────────────────
   Příkaz čtení věty souboru má dvě základní varianty dle způsobu identifikace
   věty : fyzickým číslem věty nebo hodnotou jednoho z vlastních klíčů souboru.

   ██ syntaxe:       <b>READREC (</b> RecordProměnná <b>,</b> ČísloVěty <b>)</b>
                     <b>READREC (</b> RecordProměnná [ <b>/</b>NázevKlíče ]
                               <b>,</b>[ Srovnání ] HodnotaKlíče <b>)</b>

   ■  <b>RecordProměnná</b> Lokální proměnná typu <a href="#t405">record</a> of soubor. Z deklarace této
                     proměnné se odvodí, ze kterého souboru se věta čte.
   1. varianta
   ■  <b>ČísloVěty</b>..... Číselný výraz - fyzické číslo věty. 
                     Readrec(RecVar,<b>0</b>) provede inicializaci (nulování) proměnné.
   2. varianta
   ■  <b>NázevKlíče</b> ... Vlastní klíč souboru. Implicitně <b>@</b>.
   ■  <b>Srovnání</b> ..... Operace srovnání. Pro běžný indexovaný soubor lze použít
                     pouze <b>=</b> (implicitní) - tj. načte se věta přesně dle 
                     požadované hodnoty klíče nebo žádná.
                     Pro <a href="sql.md#t885">SQL</a> lze použít <b>&gt;</b>,<b>&gt;=</b>,<b>&lt;</b>,<b>&lt;=</b>.
                     Operace srovnání nezajistí nalezení další věty dle klíče,  
                     ale věty s hodnotou klíčových údajů splňujících podmínku.
   ■  <b>HodnotaKlíče</b>   Textový výraz, hodnota klíče v interní podobě. Viz.<a href="jazyk.md#t573">keyof</a>
   Nenajde-li se hodnota klíče, bude věta vynulována a označena jako zrušená.

   <a href="#t404">writerec</a>    <a href="#t572">isdeleted</a>    <a href="#t572">deleterec</a>    <a href="#t574">linkrec</a>    <a href="soubory.md#t336">klíče</a>     <a href="#t572">recallrec</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  P Ukazka *  VAR   veta : record of ADRESY ;
                            r : real ;
                         klic : string ;
                   BEGIN
       {1.způsob}  readrec( veta,1) ;    {načtení fyzicky první věty souboru}
                   ...
       {2.způsob}  readrec( veta,0) ;                          {inicializace}
                   veta.prijmeni := 'Novák' ;
                   s:=keyof(veta/DlePrijmeni) ;    {generování hodnoty klíče}
                   readrec( veta/DlePrijmeni,s) ;                     {čtení}
                   if isdeleted(veta) then  message('Novák není ... ');
                   ...
                   END ;

   ██  P Ukazka2 * VAR p : record of PARAM ;
                   ...
                   readrec(p,'');     {čtení poslední věty parametr. souboru}
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t404"></a>
## writerec

<pre>
                      <b>WRITEREC</b> - zápis věty do souboru
   ──────────────────────────────────────────────────────────────────────────
   Příkaz  pro zápis věty  do souboru má dvě základní  varianty podle způsobu
   identifikace věty : fyzickým číslem věty nebo hodnotou jednoho z vlastních 
   klíčů souboru.

   ██ syntaxe:       <b>WRITEREC (</b> RecordProměnná <b>,</b> ČísloVěty [ <b>,+</b> ] <b>)</b>
                     <b>WRITEREC (</b> RecordProměnná [ <b>/</b>NázevKlíče ]
                                <b>,</b> HodnotaKlíče [ <b>,+</b> ] <b>)</b>

   ■  <b>RecordProměnná</b> Lokální proměnná typu <a href="#t405">record</a> of soubor. Z deklarace této
                     proměnné se odvodí, do kterého  souboru  se věta zapíše.
   ■  <b>+</b> ............ Provedení aditivních změn. V případě havárie se aditivní 
                     změny neprovedou a nastaví se <a href="#t626">exitcode</a>=1.
   1. varianta
   ■  <b>ČísloVěty</b>..... Číselný výraz - fyzické číslo věty. Writerec(RecVar,<b>0</b>)
                     provede zápis nové věty (na konec souboru).
   2. varianta
   ■  <b>NázevKlíče</b> ... Vlastní klíč souboru. Implicitně <b>@</b>.
   ■  <b>HodnotaKlíče</b>   Textový výraz, hodnota klíče v interní podobě. 
                     Slouží k identifikaci  věty a může být jiný než v record
                     proměnné ( přepsání věty se změnou klíčových údajů).
                     Viz. <a href="sql.md#t885">SQL</a>, <a href="jazyk.md#t573">keyof</a>.

   <a href="#t403">readrec</a>     <a href="#t572">isdeleted</a>     <a href="#t572">deleterec</a>     <a href="#t574">linkrec</a>     <a href="soubory.md#t336">klíče</a>      <a href="#t572">recallrec</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>


   ██  P Ukazka1  * VAR p : record of PARAM ;
                    ...
                    writerec(p,'');   {zápis poslední věty parametr. souboru}

   ██  F soubor.x   klic : N,3 ; polozka : A,10 ;
                    #K @ klic ;
       P Ukazka2  * VAR rec : record of soubor ;   i : real ;
                    BEGIN
                    ...                        { tohle neprojde neboť nelze }
                    soubor[i].klic := '111' ;  { zapsat do klíčového údaje  }
                    ...                        { platné věty                }
                    readrec(rec,i);
                    rec.klic:='111';          { takto dojde i k aktualizaci }
                    writerec(rec,i);          { indexů věty !!!             }
                    ...
                    END ;

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t405"></a>
## record

<pre>
                                   <b>RECORD</b>
   ──────────────────────────────────────────────────────────────────────────
   Klíčové slovo <b>record</b> lze použít ve dvou kontextech.

   ■ Lokální proměnná nebo parametr procedury typu <b>record of soubor</b>
     tzv. <a href="formular-sestava.md#t407">record proměnná</a>.

   ■ <a href="editory.md#t536">Přerušení editace</a> akcí typu <b>record</b>, tj. změna(zrušení) editované věty

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t411"></a>
## procedura

*Též:* `P`

<pre>
                           <b>PROCEDURA - kapitola P</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Procedura</b> kombinuje volání již připravených  kapitol projektu a komunikaci
   s uživatelem. Start celé úlohy začíná procedurou, která se povinně jmenuje 
   <b>MAIN</b>.

   ██ Syntaxe:       [ <b>( </b>ParametryProcedury<b> )</b> ]
                     [ <a href="formular-sestava.md#t391">DeklaraceLokálníchProměnných</a> ]
                       <b>begin</b>
                     { Příkaz <b>;</b> }
                       <b>end ;</b>

   ██ <b>ParametryProcedury</b>
      Seznam parametrů oddělených středníkem. Celý seznam parametrů je 
      uzavřený v kulatých závorkách. Pro tyto parametry se používá termín
      <b>formální parametr</b>.  Při volání procedury se na místo formálního 
      parametru dosadí <b>skutečný parametr</b>.

      <b>( </b>[<b>VAR</b>] NázevParametru <b>:</b> Typ  [{ <b>;</b>[<b>var</b>] NázevParametru <b>:</b> Typ }] <b>)</b>

   ■  <b>Typ</b> ... Povolené typ parametrů : integer, real, string,
                                       <a href="#t405">record</a> of soubor,
                                       <a href="#t414">index</a> of soubor,
                                       <a href="soubory.md#t417">file</a>

   ■  <b>VAR</b> ... Tento prefix znamená, že při volání procedury se do proměnné,
              použité jako parametr procedury, dosadí hodnota, kterou tento
              parametr získal v proceduře. Jde o tzv. <b>volání parametru odkazem</b>.
              Lze použít jen pro typy  <b>real</b>, <b>string</b>, <b>boolean</b>.
              Poznámka : typy RECORD of soubor a INDEX of soubor jsou vždy
                         volány odkazem.

      Při volání  z jiné  procedury musí  souhlasit počet  a typy parametrů v
      deklaraci volané procedury a v příkazu volání <a href="#t436">proc</a>.  Před parametr typu
      FILE musíme vložit znak '<b>@</b>'.
      Příklad:    proc( Uzaverka,(<b>@</b>Ucty1)) ;


   ██ <b>Tělo procedury</b>         begin
                             Příkazy ;
                             end  ;

   <a href="#t428">řídící příkazy</a>          <a href="#t510">volání editoru</a>            <a href="jazyk.md#t654">výrazy</a>
   <a href="#t442">přiřazovací příkaz</a>      <a href="formular-sestava.md#t556">volání sestavy</a>            <a href="editory.md#t595">příkazy pro editaci textu</a>
   <a href="#t444">komunikační příkazy</a>     <a href="formular-sestava.md#t566">volání transformace</a>       <a href="prostredi.md#t609">kopírování a převody</a>
   <a href="rozhrani.md#t753">grafy</a>                   <a href="#t567">třídění</a>                   <a href="#t626">externí programy</a>
   <a href="#t504">speciální příkazy</a>       <a href="#t568">přímý přístup k datům</a>     <a href="prostredi.md#t638">katalog v proceduře</a>


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  Minimální zápis procedury:          P  Ukazka1   *   BEGIN
                                                            END ;
       Nejčastěji se pro takovou proceduru používá výraz <b>prázdná procedura</b>.
       Vlastě neudělá vůbec nic. To se ale může někdy docela hodit.

   ██  Procedura s parametry a lokálními proměnnými:

       P  Ukazka2 *  ( rec:record of Soubor1; r:real; VAR s:string) {parametry}
                     VAR i,j : real;
                           b : boolean ;                   { lokální proměnné }
                     BEGIN
                     s:='';
                     for i:=1 to int(r) do s:=s+char(i);
                     rec.Poznamka:=s ;
                     ....
                     end ;

       V proceduře Ukazka2 se vytvoří řetězec znaků s kódy 1,2,...<b>r</b> (parametr)
       a ten se předá přes (VAR)parametr <b>s</b> ven z procedury. Kromě toho se
       zapíše string i do údaje record-proměnné <b>rec</b>, kde bude přístupný i po
       ukončení procedury.

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t414"></a>
## index

*Též:* `index of`, `pracovní index`

<pre>
                              <b>INDEX</b> of Soubor
   ──────────────────────────────────────────────────────────────────────────
   Index je strukturovaný typ, který lze použít při deklaraci <b>parametrů</b> nebo
   <b>lokálních proměnných procedury</b>. Je to pracovní index indexovaného souboru
   (přípona .X) nebo do SQL-souboru s vlastním klíčem (přípona .SQL).

   ██ <b>Syntaxe:</b> ... : <b>INDEX OF</b> Soubor [ <b>(</b> TřídícíÚdaje <b>)</b> ]

   ■ Přístup do souboru přes pracovní index <b>index of soubor</b> má dvě základní
     charakteristiky:  <b>podmnožina</b> souboru
                       <b>třídění</b> souboru (podpora pro <b>vyhledávání</b> dle klíče),
                               třídění lze buď zahrnout přímo do deklarace
                               nebo definovat později příkazem <a href="#t415">getindex</a>

   ■ <b>TřídícíÚdaje</b> ... setřídění pracovního indexu (nemá vliv na data)
                      speciální znaky před názvy třídících údajů:
                        <b>&gt;</b> ... sestupné setřídění
                        <b>~</b> ... lexikální setřídění.
                      Pokud je třídění uvedeno v deklaraci, lze použít pro
                      takový index konstrukci <a href="jazyk.md#t440">key in</a> a funkce procedury
                      <a href="jazyk.md#t285">recno</a>, <a href="jazyk.md#t579">recnoabs</a>, <a href="jazyk.md#t579">recnolog</a> a <a href="jazyk.md#t573">keyof</a>.

   ■ <b>Použití:</b> Lze ho použít všude <b>v proceduře</b>, kde je možno psát <b>/NázevKlíče</b>.

   ■ <b>Inicializace:</b> Po deklaraci je pracovní index prázdný

   ■ <b>Naplnění:</b>     Základním prostředkem pro práci s pracovním indexem je 
                   příkaz procedury <a href="#t415">GetIndex</a>. Další možnost aktualizace
                   pracovního indexu je v příkazu 
                       edit(Soubor/LokPromTypuIndex, ... ) ;
                   Pokud jsou v editu přidány nebo zrušeny věty, je po
                   ukončení editace aktualizován i pracovní index. Toto
                   však neplatí při variantách editace, kdy si editor 
                   vytváří vlastní pracovní index, tj. při editaci s cond,
                   owner, mode='F2', mode='WX'.

                   Ruční výběr do pracovního indexu je možný pomocí příkazu
                   <b>edit</b> s parametrem <a href="editory.md#t529">sel</a>.

   ■ <b>Počet vět pracovního indexu:</b> pomocí konstrukce <b>.nrecs</b>, tj.
                                  PracovníIndex.nrecs
   ■ <b>Zrušení pracovního indexu:</b>   pomocí konstrukce 
                                  PracovniIndex<b>.nrecs:=0</b>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ P Ukazka1   *  VAR WorkIndex : index of Pracov (~Jmeno) ;
                     BEGIN
                     GetIndex(WorkIndex, Pracov/DleJmena, cond=(JeMuz)) ;
                                { vybere z pracovníků jen muže v třídění dle
                                  jmen }
                     ....
                     GetIndex(WorkIndex, Pracov/WorkIndex, cond=(vek&gt;60) ) ;
                                { postupné zúžení podmnožiny na pracující
                                  důchodce - muže }

                     edit( Pracov / WorkIndex, .... ) ;
                     report( Pracov / WorkIndex, Sestava, .... ) ;
                     ...
                     END ;

   ██ Příklad vytvoření uživatelského indexu.
                     ...
                     s:=prompt('zadej podmnožinu:':A,60) ;
                     GetIndex( WorkIndex, Pracov, cond=(<a href="jazyk.md#t740">evalb</a>(s)));
                     ...
   ──────────────────────────────────────────────────────────────────────────
   <a href="soubory.md#t318">indexy</a>       <a href="#t405">record</a>      <a href="soubory.md#t417">file</a>
</pre>

<a name="t415"></a>
## getindex

<pre>
                                  <b>GETINDEX</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz procedury  GetIndex  vytváří pracovní index v lokální proměnné resp.
   parametru procedury typu <a href="#t414">index</a> of soubor. Pomocí alternativní verze
   příkazu lze i upravovat již vytvořený pracovní index.

   ██ Syntaxe<b>1</b>:    <b>GETINDEX( </b>LokPromTypuIndex<b>,</b>
                             Soubor [<b> / </b>NázevKlíče | LokPromTypuIndex ]
                           [ <b>,cond = (</b> LogickýVýraz <b>)</b> ] 
                           [ <b>,sort = (</b> TřídícíÚdaje <b>)</b> ] 
                           [ <b>,owner = </b>VazbaOwner      ]  <b>)</b>

   ■ Soubor/NázevKlíče ..... Soubor je název kapitoly F, přístup podle klíče
                             použijeme ze dvou důvodů:
                             1. případné použití <a href="jazyk.md#t440">key in</a> v parametru <b>cond</b>
                             2. při shodě popisu klíče s tříděním indexu dojde
                                k výrazné úspoře času
                   <b>Poznámka:</b> Ve verzi 3.2 se podle NázevKlíče přebralo třídění
                             do pracovního indexu. Ve verzi 3.3 nikoliv.

   ■ <b>Cond</b>=(LogickýVýraz) ... Pracovní index prezentuje podmnožinu souboru.
                             V podmínce se může vyskytnout <a href="jazyk.md#t440">key in</a>, v případě
                             souboru .SQL lze použít i textový výraz
                             ( pro SELECT WHERE )

   ■ <b>Sort</b>=(TřídícíÚdaje) ... Syntaxe je shodná s tříděním v deklaraci <a href="#t414">index</a>u.
                             Parametr  <b>sort</b>  lze  použít jen  tehdy, není-li
                             třídění uvedeno v deklaraci  a jde-li  o lokální
                             proměnnou procedury (nikoliv parametr procedury).
                             Z posledního faktu plyne důležité omezení a sice:
                             <b>ve volané proceduře nelze změnit třídění pracov.</b>
                             <b>indexu, který tam byl předán jako parametr.</b>

   ■ <b>owner</b> = VazbaOwner .... Generování podmnožiny podle vazby owner. Syntaxe
                             viz. popis <a href="soubory.md#t437">owner</a>.
                             POZOR ! U distribuční verze 4.0 chybně funguje
                                     varianta OWNER Soubor[FyzČísloVěty]
                                     Pouze u příkazu GetIndex, jinak OK.

   ■ Vytvářená lokální proměnná typu index může být stejná jako /...Index.
     Tak lze realizovat postupné zúžení podmnožiny.


   ██ Syntaxe<b>2</b>:    <b>GETINDEX( </b>LokPromTypuIndex<b>, +</b> | <b>- ,</b> ČísloVěty <b>)</b>

   ■ <b>+</b>  ... Příkaz přidá do pracovního indexu větu, danou fyzickým číslem 
            ve třetím parametru. Neprovede nic, pokud je číslo věty neplatné,
            věta je neplatná nebo je již v indexu zařazena (se stejnou
            hodnotou klíče).

   ■ <b>-</b>  ... Příkaz vyřadí danou větu z indexu. Pokud věta není v indexu nebo
            tam je s jinou hodnotou klíče, neprovede se žádná akce.

   ■ <b>ČísloVěty</b> ... číselný výraz, fyzické číslo věty, která se má přidat nebo
                   vyjmout z pracovního indexu.

   ──────────────────────────────────────────────────────────────────────────
   Příklad viz. <a href="#t414">index</a>.
</pre>

<a name="t419"></a>
## složený příkaz

<pre>
                               <b>SLOŽENÝ PŘÍKAZ</b>
   ──────────────────────────────────────────────────────────────────────────

   Složený  příkaz je sekvencí několika  příkazů , ohraničených  programovými
   závorkami  <b>begin</b>  a  <b>end</b>.  Složený  příkaz se v kontextu  chápe jako jeden
   příkaz.  Pomocí <b>begin</b> a <b>end</b> se provede i vnější ohraničení těla procedury.
   Se stejnou  syntaxí jako v proceduře  se složený příkaz  může vyskytnout v
   reportu nebo transformaci.

   ██ Syntaxe:      <b>begin</b> { Příkaz <b>;</b> } <b>end ;</b>

   ■  <b>Příkaz</b> ...... Může být libovolný jiný příkaz včetně složeného příkazu.

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t422"></a>
## cancel

*Též:* `break`, ` exit`

<pre>
                             <b>PŘERUŠENÍ PROGRAMU</b>
   ──────────────────────────────────────────────────────────────────────────
   Tyto příkazy umožňují okamžité  ukončení cyklu nebo určité části programu.

   Příkaz  <b>break</b>  provede  ukončení provádění  příkazů cyklu ( repeat, while,
   forall, for) a cyklických nabídek (menuloop,menubar). Zpracování pokračuje
   za těmito příkazy. Jinde má stejný efekt jako <b>exit</b>.

   ██ Syntaxe:      <b>BREAK</b>
   ──────────────────────

   Příkaz <b>exit</b>  provede skok na konec právě prováděné procedury. Předá řízení
   volající proceduře - pokud existuje. Jinde má efekt příkazu <b>cancel</b>.

   ██ Syntaxe:      <b>EXIT</b>
   ─────────────────────

   Ukončení úlohy - tj. návrat do volající nadřízené úlohy nebo konec se 
   provede příkazem cancel.

   ██ Syntaxe:      <b>CANCEL</b>

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t428">řídící příkazy</a>
</pre>

<a name="t427"></a>
## for

*Též:* `while`, `repeat`, `until`, `to`

<pre>
                               <b>PŘÍKAZY CYKLU</b>
   ──────────────────────────────────────────────────────────────────────────
   Jednotlivé typy cyklů se od sebe liší především umístěním a typem podmínky.

   Příkaz  <b>while</b>  se provádí  dokud platí logická  podmínka, která se testuje
   před tělem příkazu (smyčkou). To znamená, že příkaz uvnitř cyklu se nemusí
   provést ani jednou.

   ██ Syntaxe:       <b>WHILE</b> LogVýraz <b>DO</b> Příkaz

   Sekvence příkazů mezi <b>repeat</b> a <b>until</b> se provádí,dokud je <b>ne</b>splněna podmínka
   na konci těla příkazu. To znamená, že příkaz se provede vždy alespoň jednou
   (pokud není ukončen jinak).

   ██ Syntaxe:       <b>REPEAT</b> { Příkaz <b>;</b> } <b>UNTIL</b> LogVýraz

   Příkaz cyklu <b>for</b> je řízen lokální řídící proměnnou typu real, která se na
   počátku příkazu inicializuje na hodnotu <b>ČísVýraz1</b> a na konci každé smyčky
   se zvýší o 1. Před každou smyčkou se provede srovnání <b>&lt;=</b> na <b>ČísVýraz2</b>.
   V podstatě jde o speciální případ příkazu <b>while</b>.

   ██ Syntaxe:       <b>FOR</b> LokálníProměnná <b>:=</b> ČísVýraz1 <b>TO</b> ČísVýraz2 <b>DO</b> Příkaz
   ──────────────────────────────────────────────────────────────────────────
   speciální cyklus pro věty souboru  <a href="#t441">forall</a>                   <a href="#t428">řídící příkazy</a>
</pre>

<a name="t428"></a>
## řídící příkazy

<pre>
                               <b>ŘÍDÍCÍ PŘÍKAZY</b>
   ──────────────────────────────────────────────────────────────────────────
   Řídící příkazy slouží k ovlivnění standardního pořadí a způsobu zpracování 
   příkazů v proceduře, které je jinak přísně sekvenční.Tj. dle pořadí zápisu
   příkazů v proceduře.

   ■ <a href="#t275">begin</a>, <a href="#t275">end</a>; ... <a href="#t419">složený příkaz</a> (programové závorky)
   <b>větvení programu</b>
   ■<a href="#t433"> if</a>,<a href="#t433"> then</a>,<a href="#t433"> else</a>. podmíněný příkaz (maximálně dvě větve)
   ■ <a href="#t433">case</a> .......... podmíněný příkaz s více větvemi
   <b>přerušení programu</b>
   ■ <a href="#t422">break</a> ......... ukončení cyklu nebo procedury
   ■<a href="#t422"> exit</a> .......... ukončení procedury (návrat do volající procedury)
   ■ <a href="#t422">cancel</a> ........ ukončení úlohy (návrat do volající úlohy nebo konec)
   <b>cykly</b>
   ■ <a href="#t427">while</a> ......... cyklus s podmínkou před začátkem smyčky
   ■ <a href="#t427">repeat</a>,<a href="#t427">until</a>... cyklus s podmínkou na konci smyčky
   ■ <a href="#t427">for</a>,<a href="#t427">to</a> ........ cyklus s řídící reálnou proměnnou a konstantním krokem 1
   ■ <a href="#t441">forall</a> ........ cyklus pro věty souboru
   <b>volání procedur a podprogramů</b>
   ■ <a href="#t436">proc</a> .......... volání další procedury
   ■<a href="#t436"> call</a> .......... volání subúlohy
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t451">uživatelská nabídka</a>       <a href="jazyk.md#t491">ismouse</a>
</pre>

<a name="t433"></a>
## case

*Též:* ` if`, ` else`, ` then`, `větvení programu`

<pre>
                              <b>VĚTVENÍ PROGRAMU</b>
   ──────────────────────────────────────────────────────────────────────────
   Pro větvení programu, jako jednu ze základních programátorských konstrukcí
   lze použít podmíněný příkaz pro jednoduché větvení <b>if</b> ... <b>else</b> nebo příkaz
   <b>case</b> pro vícenásobné větvení.

   ██ Syntaxe:        <b>IF</b> LogVýraz <b>THEN</b> Příkaz1 [ <b>ELSE</b> Příkaz2 ]

   ■  Jestliže je splněna podmínka <b>LogVýraz</b> (=TRUE), provede se <b>Příkaz1</b>. Pokud
      splněna není, provede se (je-li uvedena) větev <b>else</b> - tedy <b>Příkaz2</b>.

   ───────────────

   Vícenásobné větvení lze použít příkaz <b>case</b>. Postupně se vyhodnocují logické 
   podmínky  a provede se příkaz u první splněné  podmínky. Pokud není splněná
   žádná podmínka, provede se příkaz ve větvi <b>else</b>, příp. žádný. Příkazy mohou 
   být i složené (<b>begin</b> .. <b>end</b>).

   ██ Syntaxe:        <b>CASE</b>   LogVýraz <b>:</b> Příkaz<b>;</b>
                           { LogVýraz <b>:</b> Příkaz<b>;</b> }
                    [ <b>ELSE</b>   Příkaz<b>;</b> { Příkaz } ]
                      <b>END</b>;
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t428">řídící příkazy</a>
</pre>

<a name="t436"></a>
## volání procedur a podprogramů

*Též:* ` call`, `proc`

<pre>
                       <b>VOLÁNÍ PROCEDUR A PODPROGRAMŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz volání procedury.

   ██ syntaxe:         <b>PROC ( </b>NázevProcedury | <b>[</b>DynamickáDeklarace<b>]</b>
                            [ <b>,(</b>Parametry<b>)</b> ] <b>)</b>

   ■ <b>NázevProcedury</b> .. Identifikátor, název kapitoly P.
   ■ <b>Parametry</b> ....... Seznam parametrů.  Obsahuje výrazy odpovídající co do
                       typu a pořadí parametrům v deklaraci volané procedury
                       Parametry typu <b>real</b>, <b>string</b>, <b>boolean</b> mohou být volány
                       hodnotou nebo odkazem. Parametry strukturovaných typů
                       ( <a href="#t405">record</a> of, <a href="#t414">index</a> of ) jsou volané vždy odkazem ( po
                       návratu do volající  procedury se přenášejí  případné
                       změny hodnot, viz. <a href="#t411">procedura</a> ). Parametr typu <a href="soubory.md#t417">file</a> je
                       nutno označit prefixem <b>@</b> ( <b>Proc(Ukazka,(@Ucty));</b> ) .
   ■ <b>DynamickáDeklarace</b> ... Textový výraz, který obsahuje zdrojový kód podle
                            syntaxe kapitoly P. Viz. <a href="soubory.md#t554">dynamické deklarace</a>.

   Volání podúlohy  (volání v L kapitole<a href="#t908">  call</a>).

   ██ syntaxe:         <b>CALL (</b>[<b>\</b>]NázevPodúlohy [<b>,</b>[ Procedura [<b>(</b> Parametry <b>)</b>]]<b>)</b>
   
   Pokud není uvedeno jméno procedury,volá se procedura MAIN. V podúloze jsou
   použitelné všechny kapitoly deklarované v nadřízených úlohách (pokud nejsou
   překryty stejnojmennou deklarací v podúloze). Podúloha může volat další
   podúlohu atd.

   Po ukončení vnořené procedury nebo podúlohy se vrací řízení na místo volání
   a úloha pokračuje dalším příkazem.

   <b>\</b> ... Z libovolné úlohy ve strukturovaném projektu je vyvolána úloha, která
         je přímo podřízená hlavní úloze. Oproti základnímu způsobu volání se
         modifikuje alokace souborů, které nemají v katalogu úplnou cestu, 
         jako by byla volána přímo z hlavní úlohy.

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>


   ██    menuloop 'ÚLOHY v PC FANDu' of
                  'SKLADY - příjem ze skladu': call(SKLADY,prijem);
                  '       - výdej ze skladu' : call(SKLADY,vydej);
                  '       - celé zpracování' : call(SKLADY);
                  'MZDY': call(MZDY);
                  'UCETNICTVI': call(UCTY);
         end;

   ██    call( SKLADY, vydej('MTZ'));        { volání procedury z podúlohy
                                               z parametrem }

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t441"></a>
## forall

<pre>
                               <b>FORALL CYKLUS</b>
   ───────────────────────────────────────────────────────────────────────────
   <b>Forall</b> cyklus prochází platné věty celého souboru nebo jeho části a provádí
   požadované akce. Na věty souboru se odkazuje přímým přístupem  podle <b>řídící</b>
   <b>proměnné</b> cyklu  typu  <b>real</b>,  která  postupně  nabývá hodnot fyzických čísel
   procházených vět, nebo  pomocí  <b>record  proměnné</b>  typu  <b>record of</b>, která se
   postupně naplňuje jednotlivými větami souboru nebo kombinací obou způsobů.

   ██ Syntaxe:      <b>FORALL</b>    RecordProměnná |
                              RealProměnná <b>in</b> RecordProměnná |
                              RealProměnná <b>in</b> NázevSouboru
                            [ <b>/@</b> | <b>/</b>NázevKlíče | VazbaOwner ]
                            [ <b>(</b> LogickáPodmínka <b>)</b> ] [ <b>!</b> ] [ <b>%</b> ] <b>do</b> Příkaz
                                                
   ■ <b>RecordProměnná</b> ... Lokální proměnná typu <a href="#t405">record</a> of zpracovávaný soubor.
                        Aktualizované věty jsou automaticky vypsány do souboru
                        včetně aditivních změn. O který soubor jde, je zřejmé
                        z deklarace proměnné. V této variantě se není třeba
                        starat o čtení a zápis vět.

   ■ <b>RealProměnná</b> ..... Obsahuje fyzické číslo zpracovávané věty, které lze
                        použít pro přímý přístup <b>Soubor[i].Údaj</b> nebo pro
                        operace typu rušení věty <a href="#t572">deleterec</a>. 

   ■ <b>/@</b>,<b>/NázevKlíče</b> ... Věty se zpracují v pořadí vlastního resp. alternativ-
                        ního klíče. Nelze kombinovat s <a href="soubory.md#t437">owner</a>. Implicitně se 
                        soubor prochází ve fyzickém pořadí (<a href="#t414">index of</a>).

   ■ <b>LogickáPodmínka</b>... Vstupní filtr. Logický výraz, ve kterém lze použít
                        údaje souboru. Je efektivnější než rozhodování až 
                        v těle cyklu. Je povolena i konstrukce <a href="jazyk.md#t440">key in</a>.
                        Pro <a href="sql.md#t885">SQL</a> je možno podmínku realizovat na serveru.

   ■ <b>Vazba</b> <a href="soubory.md#t437">Owner</a> ...... Skupina vět podřízeného souboru podle viditelné věty
                        nadřízeného souboru.

   ■ <b>!</b> ......... Píše se při <b>/klíč</b> nebo <b>owner</b>, když chceme rušit,zařadit věty
                 či měnit indexy (hodnoty klíče). Pro korektní provedení cyklu 
                 se vygeneruje virtuální index.

   ■ <b>%</b> ......... Automatické zobrazení  procenta zpracování  souboru v dolním
                 levém rohu (jako merge,report). Implicitně definuje okno pro
                 výstup, které je v případě potřeby nutno předefinovat v těle
                 cyklu.

   <b>Charakteristika variant:</b>
   <b>RealProměnná in Soubor</b>  se s výhodou použije při jednoduché změně několika
   (1-3) údajů - např. vynulování  proměnné.  Velmi výhodný  je v případě, že
   soubor obsahuje volné texty, se kterými se ale v cyklu nepracuje.
   Pokud se pracuje s více údaji, bude výhodnější varianta s <b>record proměnnou</b>,
   sníží počet fyzických přístupů na disk a zápis je přehlednější.


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ { kolik pracovníků má vyšší mzdu než 10.000 Kč }
      Kolik:=0 ;
      forall i in PRACOV (mzda&gt;10000) do  Kolik+=1 ;

      forall i in PRACOV do                                { takto lze také }
             if PRACOV[i].mzda&gt;10000 then Kolik+=1 ; { ale bude to pomalejší}


   ██ { zpracování podle indexu }
      var r:record of PRACOV ;  i:real ;
      forall i in DATA/@ (key in [...]) do ...
      forall r/PodleJmena do ...

   ██ { vazba owner - výběr podřízených vět pro jednu nadřízenou }
      var VětaPod:record of POD;
          VětaNad:record of NAD;
      VětaNad.Klíč:=...;
      forall VětaPod owner VětaNad do ...;
      forall VětaPod owner NAD[i] do ... ;

   ──────────────────────────────────────────────────────────────────────────
   <a href="soubory.md#t338">vlastní klíč</a>             <a href="formular-sestava.md#t407">record proměnná</a>             <a href="#t568">přímý přístup k datům</a>
</pre>

<a name="t442"></a>
## přiřazovací příkaz

<pre>
                             <b>PŘIŘAZOVACÍ PŘÍKAZ</b>
   ──────────────────────────────────────────────────────────────────────────
   Použití: v <b>procedurách</b> a příkazové části <b>sestav</b> a <b>transformací</b>.

   ██ <b>ObjektPřiřazení:=Výraz</b> ... výraz musí být odpovídajícího typu
   ██ <b>ObjektPřiřazení+=Výraz</b> ... např. a<b>+=</b>b je rychlejší varianta a<b>:=</b>a+b

                            objekt přiřazení

   ■ <a href="formular-sestava.md#t391">lokální proměnné</a> ........... proměnné deklarované v této kapitole
   ■ <a href="jazyk.md#t340">globální proměnné</a> .......... údaje parametrických souborů
   ■ RecordProměnná.Údaj ........ naplnění položky <a href="#t405">record</a> - proměnné
   ■ NázevSouboru[Číslo].Údaj ... <a href="#t568">přímý přístup k datům</a>
   ■ <a href="jazyk.md#t741">Interní proměnné</a> ........... vnitřní proměnné PC FANDu
   ───────────────────

   V proceduře lze přiřadit i celou strukturu <a href="#t405">record</a> proměnné :
   ██ <b>RecordProměnná1</b>:=<b>RecordProměnná2</b>
   Record proměnné nemusí být ze stejných souborů, přiřazení se provede
   by-name - tj. údaje se stejným názvem a typem se přiřadí, ostatní zůstanou
   nezměněny (neresetují se) - podobně jako v <a href="formular-sestava.md#t566">merge</a>.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t419">složený příkaz</a>             <a href="prostredi.md#t641">katalog</a>
</pre>

<a name="t444"></a>
## komunikační příkazy

<pre>
                       <b>KOMUNIKAČNÍ PŘÍKAZY PROCEDURY</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <a href="#t451">menu</a> .......... uživatelská nabídka (menu)
   ■ <a href="#t451">menuloop</a> ...... nabídka v cyklu
   ■ <a href="#t451">menubar</a> ....... vodorovná nabídka
   ■ <a href="#t302">write</a> ......... výstup na obrazovku
   ■ <a href="#t302">writeln</a> ....... s odřádkováním
   ■ <a href="#t459">gotoxy</a> ........ nastavení kurzoru
   ■ <a href="#t466">clrscr</a> ........ mazání obrazovky nebo okna
   ■ <a href="#t466">clreol</a> ........ mazání konce řádku
   ■ <a href="prostredi.md#t457">display</a> ....... uživatelská obrazovky
   ■ <a href="prostredi.md#t457">help</a> .......... vyvolání nápovědy
   ■ <a href="prostredi.md#t457">headline</a> ...... první řádek obrazovky
   ■ <a href="prostredi.md#t457">message</a> ....... hlášení v posledním řádku
   ■ <a href="#t487">setkeybuf</a> ..... naplnění bufferu klávesnice
   ■ <a href="#t487">clearkeybuf</a> ... vyprázdnění bufferu
   ■ <a href="#t487">wait</a> .......... čekání na vstup z klávesnice
   ■ <a href="#t487">delay</a> ......... pauza
   ■ <b>with</b> <a href="#t474">window</a> <b>do</b> ... definice okna pro další výstup

   příkazy pro <b>zvuk</b>
   ■ <a href="#t487">sound</a> ......... zapne zvuk se zadanou výškou
   ■ <a href="#t487">nosound</a> ....... vypne zvuk
   ■ <a href="#t487">beep</a> .......... jednorázové instalovatelné pípnutí

   komunikační <b>funkce</b>
   ■ <a href="#t487">readkey</a> ....... ošetření klávesnice
   ■ <a href="#t487">keybuf</a>
   ■ <a href="#t487">keypressed</a>
   ■ <a href="#t463">maxcol</a> ........ rozměry obrazovky
   ■ <a href="#t463">maxrow</a>
   ■ <a href="jazyk.md#t471">prompt</a> ........ dotaz uživateli - zadání hodnoty z klávesnice
   ■ <a href="jazyk.md#t471">escprompt</a>
   ■ <a href="jazyk.md#t471">promptyn</a> ...... dotaz na 25.řádku
   ■ <a href="jazyk.md#t467">getpath</a> ....... dotaz uživateli - výběr souboru z disku
   ■ <a href="jazyk.md#t737">selectstr</a> ..... dotaz uživateli - výběr z pole textů
   ■ <a href="jazyk.md#t550">edbool</a> ........ text výrazu podmnožiny v datovém editoru

   Příkazy a funkce pro <b>myš</b>
   ■ <a href="#t495">setmouse</a> ...... nastavení pozice myši
   ■ <a href="rozhrani.md#t490">mouseevent</a> .... čtení událostí z fronty
   ■ <a href="jazyk.md#t491">ismouse</a> ....... větvení dle druhu události
   ■ <a href="#t495">mousein</a> ....... testování pozice myši
   ■ <a href="#t495">mousex</a> ........ souřadnice kurzoru myši
   ■ <a href="#t495">mousey</a> ........       -  "  -

 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t496">░ komunikace v proceduře</a>
</pre>

<a name="t451"></a>
## uživatelská nabídka

*Též:* `menu`, `menuloop`, `menubar`, `menux`, `menuy`, `pulldown`

<pre>
                         <b>UŽIVATELSKÁ NABÍDKA - MENU</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Nabídka</b> definuje větvení programu podle interaktivní volby uživatele. 
   Existují tři typy nabídek s obdobnou syntaxí:
   ■ <b>menu</b> ....... jednorázová nabídka
   ■ <b>menuloop</b> ... nabídka se opakuje v cyklu, ukončí se klávesou <b>Esc</b>
   ■ <b>menubar</b> .... vodorovná nabídka (většinou hlavní nabídka programu), chová
                  se jako <b>pulldown</b>.
                
   ██ syntaxe: <b>MENU</b>[<b>LOOP</b>] [<b>(</b>[x<b>,</b>y][<b>;</b>a1<b>,</b>a2<b>,</b>a3<b>,</b>a4]<b>)</b>] [<b>!</b>] [<b>pulldown</b>] [hlavička] <b>of</b>
                          <a href="#t453">Seznam voleb</a>
                          <b>end;</b>

   ■  <b>x,y</b> ............čís.výrazy-levý horní roh (jinak implicitně dle kontextu)
   ■  <b>a1,a2,a3,a4</b> ... atributy barev (normální,vybraná,důraz,nefunkční),
                      Ctrl-znaky nebo čís.výrazy, <a href="prostredi.md#t221">instalace barev</a>
   ■  <b>!</b> ............. stín okna menu
   ■  <b>pulldown</b> ...... nabídka navazuje na předchozí nabídku (jinak uprostřed
                      nebo podle souřadnic) a okno zůstane na obrazovce během 
                      provádění příkazu. Navazující pozice viz. MenuX, MenuY.
   ■  <b>hlavička</b> ...... Textový výraz, text v záhlaví okna s nabídkou (jinak 
                      bez hlavičky)

   ██ syntaxe:        <b>MENUX</b> : real      funkce,vracejí sloupec a řádek hor.
                      <b>MENUY</b> : real      levého rohu pro pulldown.
   ───────────

   ██ syntaxe:        <b>MENUBAR</b> [<b>(</b>y[<b>,</b>x<b>,</b>xsize][<b>;</b>a1<b>,</b>a2<b>,</b>a3<b>,</b>a4]<b>)</b>] <b>of</b>
                              <a href="#t453">Seznam voleb</a>
                              <b>end;</b>

   ■  <b>y</b> ............. Číselný výraz, menu je umístěno na řádku y
   ■  <b>x,xsize</b>........ Číselné výrazy, počáteční sloupec,délka

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██    menubar (3,10,60) of
         'Finance',Finance   : menuloop ! pulldown  'FINANCE' of
                                        'Peněžní deník': proc(Deník);
                                        'Závazky': proc(Závazky);
                                        end;
         'Inventář',Inventář : menuloop ! pulldown 'INVENTÁŘ' of
                                        'ZP': proc(ZP);
                                        'DKP': proc(DKP);
                                         end;
         'Ostatní','Ostatní volby',trust(3):
                   menuloop ! pulldown of
                   'Systém',,trust(1)!: with window (1,1,80,25) do exec('','');
                   'Nápověda':  menuloop (0,10) ! pulldown of
                                         'Prohlížení': help('root');
                                         'Tisk': report(HELP,Help);
                                         end;
                   end;
         escape : if prompt(' opravdu skončit (A/N)': B) then break ;
         end;

   ──────────────────────────────────────────────────────────────────────────
   <a href="rozhrani.md#t204">volba z nabídky</a>           <a href="#t444">komunikační příkazy</a>
</pre>

<a name="t453"></a>
## seznam voleb

*Též:* `escape`

<pre>
                                <b>SEZNAM VOLEB</b>
   ──────────────────────────────────────────────────────────────────────────
   Seznam voleb  v nabídce  obsahuje text volby, jak se objeví na uživatelské
   obrazovce, a odpovídající příkazy.

   ██ syntaxe :      { TextVolby [<b>,</b>Nápověda [<b>,</b>Podmínka[<b>!</b>]]] <b>:</b> Příkaz <b>;</b> }
                     [ <b>Escape :</b> Příkaz <b>;</b> ]

   ■  <b>TextVolby</b> ... Textový výraz, zobrazí se jako řádek menu.
                    Mezi znaky <b>^W</b> (zadání Ctrl-P Ctrl-W) může být uzavřeno 
                    písmeno = "hot klávesa" pro rychlou volbu jedním písmenem 
                    (jiná barva) implicitně první písmeno každé volby.
   ■  <b>Nápověda</b> .... Identifikátor nebo textová konstanta (v apostrofech).
                    Podle hesla nápovědy se zobrazí odpovídající text ze
                    souboru .HLP. V 25. řádku jako krátká nápověda nebo
                    po <b>F1</b> celý. Viz. <a href="prostredi.md#t322">konstrukce nápovědy</a>.
   ■  <b>Podmínka</b> .... Logický výraz, pokud není splněn, volba se potlačí.
                    Je-li uveden znak <b>!</b>, bude volba nefunkční ale zůstane
                    v menu, ovšem zobrazená odlišnou barvou (čtvrtý atribut).
   ■  <b>Escape</b> ...... Uvedený příkaz se provede při <b>ESC</b>. Pokud není uvedeno,
                    ukončí ESC práci v menu.
   ■  <b>Příkaz</b> ...... Může být i složený (<b>begin</b> - <b>end</b>)
   ■ '':; ......... <b>prázdná volba</b>: na obrazovce vodorovná čára,
                    která se při pohybu po menu přeskakuje
   Maximální počet voleb je dán rozměry obrazovky.
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t459"></a>
## výstup na obrazovku

*Též:* `gotoxy`

<pre>
                            <b>VÝSTUP NA OBRAZOVKU</b>
   ──────────────────────────────────────────────────────────────────────────
   Základními příkazy  pro výstup  na obrazovku jsou  <b>write</b> a  <b>writeln</b>, který
   navíc provede odřádkování.Výstup se provede do aktuálního okna od aktuální 
   pozice kurzoru.

   ██ syntaxe:          <b>WRITE( </b>SeznamPrvků <b>)</b>
                      <b>WRITELN( </b>SeznamPrvků <b>)</b>

   ■ prvek seznamu prvků ...... <b>TextVýraz</b>, <b>LogVýraz</b>, <b>ČísVýraz</b> s maskou
   ■ masky pro výstup čísla ... ČísVýraz<b>:m</b> nebo ČísVýraz<b>:m:n</b>
                                <b>m</b> celková délka zobrazení čísla (počet míst)
                                <b>n</b> je počet míst za čárkou
                                  při <b>n=-1</b> se použije exponenciální tvar
     výstup datumu a času ..... ČísVýraz<b>:Maska</b> (<a href="prostredi.md#t330">maska pro datum</a>)
   ───────────────

   Nastavení aktuální pozice kurzoru.

   ██ syntaxe:         <b>GOTOXY( </b>Sloupec<b>,</b>Řádek <b>)</b>
   
   ■  <b>Sloupec</b>,<b>Řádek</b>... Číselné výrazy, relativní vzhledem k aktuálnímu oknu.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t487">ošetření klávesnice</a>    <a href="#t444">komunikační příkazy</a>    <a href="#t474">okna pro výstup</a>    <a href="#t748">outtextxy</a>
   <a href="#t466">vymazání obrazovky</a>
</pre>

<a name="t463"></a>
## maxrow

*Též:* `getmaxx`, `getmaxy`, `maxcol`

<pre>
                             <b>ROZMĚRY OBRAZOVKY</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce vrací rozměry obrazovky (<b>textový</b> režim).

   ██ syntaxe:     <b>MAXCOL</b> : real   ....... počet sloupců obrazovky (stand.80)
                   <b>MAXROW</b> : real   ....... počet řádků obrazovky (stand.25)


   <a href="#t459">gotoxy</a>, <a href="dalsi-kapitoly.md#t920">Funkce v L kapitole</a>
   ───────────────

   Rozměry aktuálního okna v pixelech ( <b>grafický</b> režim ).

   ██ syntaxe:     <b>GETMAXX</b> : real  ........ Maximální souřadnice X
                   <b>GETMAXY</b> : real  ........ Maximální souřadnice Y

      <b>Poznámka :</b>   Pozor na rozdíl grafiky oproti textu. V grafice se počítá
                   od 0. Např. pro VGA, pro celou obrazovku je getmaxx=639.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t444">komunikační příkazy</a>
</pre>

<a name="t466"></a>
## vymazání obrazovky

*Též:* `clrscr`, `clreol`

<pre>
                             <b>VYMAZÁNÍ OBRAZOVKY</b>
   ──────────────────────────────────────────────────────────────────────────

   Příkaz  <b>clrscr</b>  provede vymazání okna.  Pomocí parametrů lze ovlivnit, zda
   jde o aktuální nebo explicitně souřadnicemi zadané okno.  Místo výmazu lze
   vyplnit okno zadaným znakem.

   ██ Syntaxe:          <b>CLRSCR</b> [ <b>(</b> souřadnice [ <b>,</b>Atr [<b>,</b>Znak ] ] <b>)</b> ]

   ■  <b>Souřadnice</b> ... Sloupec1,Řádek1,Sloupec2,Řádek2 ... souřadnice levého
                     horního a pravého dolního rohu okna, které má být mazáno.
                     Když nejsou zadány, převezmou se dle aktuálního okna.
   ■  <b>Atr</b> .......... atribut <a href="formular-sestava.md#t241">barvy</a> nebo Ctrl-znak, pro mazání se bere
                     složka pozadí. Když není zadán, převezme se dle
                     1: nejsou-li souřadnice - dle aktuálního okna
                     2:   jsou-li souřadnice - dle <a href="prostredi.md#t221">instalace barev</a> -
                          parametr "Uživatelská obrazovka"
   ■  <b>Znak</b> ......... textový výraz, okno se vyplní zadaným znakem
   ───────────────

   Mazání od pozice kurzoru do konce řádku.
 
   ██ Syntaxe:          <b>CLREOL</b>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  clrscr;                     { mazání aktuálního okna, podle aktuálního }
                                   { okna převezme souřadnice i atribut       }
       clrscr(10,5,30,10);             { mazání stanoveného okna, atribut dle }
                                       { instalace "Uživatelská obrazovka"    }
       clrscr(10,5,30,10,32)                          { mazání zelenou barvou }
       clrscr(1,1,80,25,^e,'░')           { vyplnění obrazovky zadaným znakem }

 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t444">komunikační příkazy</a>                       <a href="#t474">okna pro výstup</a>
</pre>

<a name="t474"></a>
## okna pro výstup

*Též:* `with window`, `window`

<pre>
                       <b>OKNO PRO VÝSTUP</b> - with window
   ──────────────────────────────────────────────────────────────────────────
   Při provádění příkazu  <b>with window</b>  se výstupy  zapisují do okna ( příkazy
   <a href="#t302">write</a>, <a href="#t302">writeln</a>, <a href="prostredi.md#t457">display</a>, <a href="jazyk.md#t471">prompt</a>,<a href="#t466">clrscr</a>), po skončení se obnoví starý obsah
   obrazovky.  Volání grafu, datového  a textového  editoru má vlastní systém
   oken nezávislý na příkazu <b>with window</b>. Tyto příkazy mají parametr <a href="soubory.md#t281">ww</a>,který
   definuje okno příkazu a má téměř totožnou syntaxi jako with window.
   ( případné odlišnosti jsou v definici barev )

   ██ syntaxe:          <b>WITH WINDOW</b> [<b>(</b>] <b>(</b>DefiniceOkna<b>)</b> [<b>)</b>] <b>DO</b> Příkaz ;
   
   ■  <b>DefiniceOkna</b> ...  Souřadnice  [ <b>,</b> Rámeček [ <b>,</b> Atribut ] ]

   ■  <b>Souřadnice</b> .....  Sloupec1 <b>,</b> Řádek1 <b>,</b> Sloupec2 <b>,</b> Řádek2
                        Číselné výrazy pro horní levý a dolní pravý roh okna.
                        Zadává se v rozsahu 80 x 25.
                        Sloupec1=0 resp. Řádek1=0 centruje v příslušném směru.

   ■  <b>Rámeček</b> ........  [ <b>@</b> ] [ <b>*</b> ] [ <b>=</b> ] TextHlavičky [ <b>!</b> ]
                        Definice rámečku okolo okna a inicializace pracovní 
                        plochy okna:
                        <b>@</b>  Plocha okna se nevyplní základní barvou (<b>Atribut</b>)
                           tj. zůstane původní obsah.
                        <b>*</b>  Původní obsah okna se uchová ( a po zrušení okna
                           obnoví) i v grafickém režimu.
                        <b>=</b>  Okolo okna bude dvojitý rámeček. Pokud není uveden
                           bude rámeček jednoduchý nebo, pokud není uveden
                           ani <b>TextHlavičky</b>, bude okno bez rámečku.
                        <b>TextHlavičky</b>  Textový výraz, text pro horní hranu 
                           rámečku (centrovaný).
                        <b>!</b>  Okno se stínem. Atribut stínu dle <a href="prostredi.md#t221">instalace barev</a>

   ■  <b>Atribut</b> ........  Barva rámečku a obsahu okna. Číselný výraz <a href="formular-sestava.md#t241">barvy</a>
                        nebo Ctrl-znak (<a href="editory.md#t242">barvy a typy písma</a>). 
                        Pokud se nezadá, nasadí se barva dle instalace.

   ██ <b>with window ((...)) do Příkaz;</b> ... dvojité závorky místo jednoduchých
   Po skončení příkazu okno zůstane na obrazovce a neobnoví se původní obsah.
   

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>


   with window (10,10,60,20,=''!) do     { okno s dvojitým rámečkem a stínem}
   with window (0,0,50,10,='') do        { centrované okno bez stínu - stejné
                                           rozměry jako předchozí }

   with window (1,1,80,25) do            { maximální velikost okna v běžném
                                           textovém režimu }

   with window (10,10,40,20,@,^b) do    { okno nepřemaže původní obsah,nasadí
                                          barvu dle definice ^b, po ukončení
                                          okna obnoví předchozí stav 
                                          tj. např. výpis údajů do masky }
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t475">definice okna</a>            <a href="#t444">komunikační příkazy</a>
</pre>

<a name="t475"></a>
## definice okna

<pre>
                               <b>DEFINICE OKNA</b>
   ──────────────────────────────────────────────────────────────────────────
   Kromě explicitního příkazu <a href="#t474">with window</a> pro definici okna je možno ovlivnit
   i parametry  oken v dalších příkazech,  které používají okno ke komunikaci
   s uživatelem.

   V příkazech <a href="rozhrani.md#t753">graph</a>, <a href="editory.md#t595">edittxt</a>, <a href="prostredi.md#t282">edit</a> a v uživatelských pohledech <a href="soubory.md#t350">#U</a> lze použít
   pro definici okna parametr <a href="soubory.md#t281">ww</a>, který má téměř totožnou syntaxi jako příkaz
   with window. Liší  se pouze různým počtem atributů barev.  V příkazu graph
   se deklarují tři barvy, edittxt jedna a v datovém editoru ( a #U ) 7 barev.
   Vesměs platí, že pokud se atribut nedefinuje, použije se barva dle
   <a href="prostredi.md#t221">instalace barev</a> pro daný objekt.

   Další skupina příkazů  pracujících s oknem již neumožňuje jeho plnou defi-
   nici ale dle kontextu je možno některé parametry okna  definovat a jiné se
   dosadí automaticky.
   Jde o příkazy a funkce <a href="#t451">menu</a>, <a href="#t451">menuloop</a>, <a href="#t451">menubar</a>, <a href="#t466">clrscr</a>, <a href="jazyk.md#t737">selectstr</a>, <a href="jazyk.md#t467">getpath</a> 
   a interní okna PC FANDu (výběr údajů,...)

   <b>░░░░░░░░░░░░</b>       clrscr(10,5,70,15,^Q)
   <b>░░příklady░░</b>       edit(DATA,(),ww=(1,3,40,12,,^A,23,0,^S));
   <b>░░░░░░░░░░░░</b>       menubar (3,10,60) of ... ;
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t444">komunikační příkazy</a>             <a href="#t476">░ okna</a>
</pre>

<a name="t476"></a>
## ░ okna

<pre>
                               <b>PŘÍKLAD - OKNA</b>
 ──────────────────────────────────────────────────────────────────────────────
                                  definice oken

 ░ edit(...,ww=(1,1,80,25))                                    {celá obrazovka}
 ░ edit(...,ww=(0,0,20,10))                        {centrované okno 20 krát 10}
 ░ with window (10,10,40,20,=''!) do ...         {s dvojitým rámečkem a stínem}
 ░ edit(...(1,3,80,24,'POŘÍZENÍ',^q,^e,13,20))          {s hlavičkou a barvami}
 ░ edittxt(...,((10,10,40,20)))             {okno bez obnovení původního stavu}
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t487"></a>
## ošetření klávesnice

*Též:* `setkeybuf`, `clearkeybuf`, `readkey`, `keybuf`, `keypressed`, `wait`, `sound`, `nosound`, `beep`, `delay`

<pre>
                            <b>OŠETŘENÍ KLÁVESNICE</b>
   ──────────────────────────────────────────────────────────────────────────
   Buffer klávesnice - kromě systémového  bufferu klávesnice obsahuje PC FAND
   i vlastní  buffer  klávesnice, ze kterého  čte znaky  přednostně. Pokud je
   buffer PC FANDu prázdný, čte znaky ze systémového bufferu.

   ██ <b>SETKEYBUF(</b>TextVýraz<b>)</b>... naplnění PC FAND-bufferu klávesnice
   ██ <b>CLEARKEYBUF</b> ........... vyprázdní oba buffery klávesnice
   ██ <b>KEYBUF</b> : string........ přečte znaky ze systémového bufferu a připojí
                              je k bufferu PC FANDu, jehož spojený obsah 
                              vrátí jako hodnotu funkce. Bezprostředně po
                              <b>keybuf</b> nebo <b>clearkeybuf</b> je <b>keypressed</b>=false.

      vstup z klávesnice

   ██ <b>READKEY</b> :string ...... vrací znak z klávesnice (příp. včetně prefixu'\0')
   ██ <b>KEYPRESSED</b> :boolean... stiskl uživatel klávesu ?
   ██ <b>WAIT</b> ................. čeká na stisk libovolné klávesy (nebo myši)

      zvuk

   ██ <b>SOUND(</b>ČísVýraz<b>)</b> ...... zapne zvukový signál (parametr=výška tónu v <b>Hz</b>)
   ██ <b>NOSOUND</b> .............. vypne zvuk
   ██ <b>BEEP</b> ................. pípne (totéž co write('\7')),
                              lze vypnout - viz. <a href="prostredi.md#t223">Instalace konstant</a>

      pauza
   ██ <b>DELAY(</b>ČísVýraz<b>)</b> ...... pauza v programu (parametr je v milisekundách),
                             na některých typech procesorů však měří chybně a 
                             je třeba čas čekání upravit vhodnou konstantou.
   ──────────────────────────────────────────────────────────────────────────
   <a href="rozhrani.md#t488">kódy řídících a funkčních kláves</a>    <a href="#t444">komunikační příkazy</a>    <a href="rozhrani.md#t490">myš v proceduře</a>
</pre>

<a name="t495"></a>
## mousein

*Též:* `mousex`, `mousey`, `setmouse`

<pre>
                              <b>MYŠ V PROCEDUŘE</b>
   ──────────────────────────────────────────────────────────────────────────
   Test na pozici kurzoru myši. Funkce testuje, zda je v zadaném obdélníku.

   ██ syntaxe:      <b>MOUSEIN (</b> x1<b>,</b> y1<b>,</b> x2<b>,</b> y2 <b>)</b> : boolean

   ■  <b>x1</b>,<b>y1</b>,<b>x2</b>,<b>y2</b>   Číselné výrazy, <b>absolutní textové</b> souřadnice levého 
                     horního a pravého dolního rohu obdélníku. V grafickém
                     režimu jsou souřadnice v grafických bodech.

   ■  Kapitoly .... P,D
   ────────────────────

   <b>Příkaz</b> pro nastavení pozice kurzoru myši. 

   ██ syntaxe:      <b>SETMOUSE (</b> x<b>, </b>y <b>,</b> JeVidět <b>)</b>

   ■  <b>x</b>,<b>y</b> ......... Číselné výrazy. Požadované souřadnice - <b>absolutní,textové</b>.
                    V grafickém režimu jsou souřadnice v grafických bodech.
   ■  <b>JeVidět</b> ..... Logický výraz. Určuje zda bude kurzor myši viditelný.
   ■  Kapitoly .... P,D
   ────────────────────

   ██ syntaxe:    <b>MOUSEX</b> : real    .....  Souřadnice kurzoru myši.
                  <b>MOUSEY</b> : real           <b>!</b> Korektní až po <b>mouseevent</b>
                                          V grafickém režimu jsou souřadnice
                                          v grafických bodech, jinak textové.
   ──────────────────────────────────────────────────────────────────────────
   <a href="rozhrani.md#t490">myš v proceduře</a>      <a href="rozhrani.md#t490">mouseevent</a>      <a href="jazyk.md#t491">ismouse</a>
</pre>

<a name="t496"></a>
## ░ komunikace v proceduře

<pre>
                      <b>PŘÍKLAD - KOMUNIKACE V PROCEDUŘE</b>
   ──────────────────────────────────────────────────────────────────────────
   ░ begin
   ░   writeln('Ukázka komunikace');
   ░   menuloop 'VÝBĚR Z NABÍDKY' of
   ░     'Zobraz instrukce': display(Nápověda);
   ░     'Napiš dnešní datum': writeln(today:'DD.MM.YYYY');
   ░     'Napiš přesný čas': writeln(currtime:'hh:mm:ss.tt');
   ░     'Vymaž obrazovku': clrscr;
   ░     'Jdi na poslední řádek': gotoxy(1,24);
   ░     escape: begin message ('Na shledanou');
   ░                   clrscr; break;
   ░             end;
   ░   end;
   ░ end;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t498"></a>
## portin

*Též:* `portout`

<pre>
                              <b>PRÁCE S PORTEM</b>
   ──────────────────────────────────────────────────────────────────────────
   Pro práci na úrovni vstupních/výstupních portů, tj. na úrovni instrukcí
   procesoru <b>IN</b> a <b>OUT</b> lze použít funkci <b>portin</b> pro příjem a příkaz <b>portout</b> 
   pro výstup informací. Např. je tedy možno řešit častý problém komunikace 
   přes sériové porty COMx.


   ██ syntaxe:        <b>PORTIN (</b> Slovo<b>,</b> AdresaPortu <b>)</b> : real

                      Funkce vrací hodnotu byte nebo slova, které bylo
                      přečteno z portu.
   ■  <b>Slovo</b> ......... Logický výraz, pokud je <b>true</b>, čte se z portu slovo
                      (dva byte), jinak se čte jeden byte.
   ■  <b>AdresaPortu</b> ... Číselný výraz s adresou portu, např. pro COM1 se
                      zadává 1016 (hexadecimálně 3F8).

   ■  Kapitoly .... P,D
   ────────────────────

   Příkaz
   ██ syntaxe:        <b>PORTOUT (</b> Slovo<b>, </b>AdresaPortu<b>,</b> Výstup <b>)</b>

   ■  <b>Slovo</b> ......... Logický výraz, viz. <b>portin</b>.
   ■  <b>AdresaPortu</b> ... Číselný výraz s adresou portu, viz. <b>portin</b>
   ■  <b>Výstup</b> ........ Číselný výraz, hodnota slova nebo byte, který vystupuje

   ■  Kapitoly .... P


   ██ Poznámka:  Použití těchto instrukcí předpokládá znalosti na úrovni 
                 programování v assembleru a znalost popisu odpovídajících
                 komunikačních rozhraní.
                 Příklad na komunikaci mezi dvěma počítači pomocí portů 
                 COM1 a COM2 je uveden v distribuci PC FANDu verze 4.0 pod 
                 názvem PORT.
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t508">inttsr</a>
</pre>

<a name="t501"></a>
## indexfile

*Též:* `compress`

<pre>
                           <b>EXPLICITNÍ INDEXOVÁNÍ</b>
   ──────────────────────────────────────────────────────────────────────────
   Indexy se vytvářejí a udržují automaticky. V některých situacích je však
   na vhodné "donutit" PC FAND k indexování souboru. Např. tehdy, když byly
   indexy zrušeny (výstup merge), a je lépe, když se indexuje ihned a ne až
   před použitím souboru ( třeba edit ). Indexování potom nezdržuje 
   uživatele.

   ██ syntaxe:         <b>INDEXFILE(</b> NázevSouboru [<b>,compress</b>] <b>)</b>

   ■  <b>NázevSouboru</b> ... název kapitoly F
   ■  <b>compress</b> ....... nejprve odstraní neplatné věty
 

   <b>Poznámka:</b> Jiným způsobem lze donutit  PC FAND k novému indexování souborů
             tak, že prostě  zrušíme  indexové  soubory ( příkaz  <b>del *.X00</b>,
             manažer). Ty pak jsou obnoveny při prvním pokusu o práci s nimi
             v úloze.  Toto indexování však nemá efekt <b>compress</b>. Ale, což je
             důležité, jde použít i pro uzavřenou (zaheslenou) úlohu.

   ──────────────────────────────────────────────────────────────────────────
   <a href="soubory.md#t318">indexy</a>     <a href="#t572">deleterec</a>
</pre>

<a name="t504"></a>
## speciální příkazy

*Též:* `memavail`, `memdiag`

<pre>
                         <b>SPECIÁLNÍ PŘÍKAZY A FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────
   Při ladění větších úloh či spouštění externích programů může  být důležitá
   informace  o velikosti  volné paměti.  Volná  paměť  se používá pro  <a href="prostredi.md#t219">cache</a>
   (stránkování přístupu na  disk).  Podle potřeby  se do ní překládají další
   subúlohy nebo volají externí programy.

   ██  syntaxe:       <b>MEMAVAIL</b>  : real 
   
   Vrací velikost volné paměti v bytech
   ────────────

   ██  syntaxe:       <b>MEMDIAG</b>  
   
   Ladící procedura vypíše na obrazovku obsah důležitých pointerů:
   dolní zásobník - začátek a konec     (PC FAND pracuje se dvěma zásobníky)
   horní zásobník - začátek a konec     (proti sobě místo standardního Heapu)
   Stack Pointer procesoru
   použitelná XMS paměť
  ┌────────────────────────────────────────────────────────────────────────┐
  │<b>!!!</b> Memdiag je použitelný pouze při spuštění programátorské verze, to   │
  │    jest FAND(C).EXE. Uživatelský run-time UFANDx.EXE ho "nezná".       │
  └────────────────────────────────────────────────────────────────────────┘
</pre>

<a name="t506"></a>
## close

*Též:* ` save`

<pre>
                      <b>ULOŽENÍ ZMĚN A UZAVŘENÍ SOUBORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Během přímého přístupu k datům a práce s globálními proměnnými se výsledky
   kvůli rychlosti  uchovávají  ve vyrovnávací paměti ( <a href="prostredi.md#t219">cache</a> ). Příkaz  <b>save</b>
   zajistí výpis  informací  z paměti  na disk  včetně  aktualizace  hlavičky
   souboru a adresářových záznamů (fyzická délka souboru). 
   Po ukončení příkazů jako např. edit, merge se činnost příkazu save provede
   implicitně.

   ██ syntaxe:         <b>SAVE</b>

   ───────────
   Uzavírání souborů je (relativně) pomalá operace,  proto PC FAND implicitně
   uzavírá  datové  soubory  až při  ukončení  úlohy ( otevírá  je při prvním
   použití ). To znamená , že během  provádění  úlohy jsou  současně otevřeny
   všechny použité soubory.  Omezení počtu současně otevřených souborů zajistí
   příkaz <b>close</b>.

   ██ syntaxe:         <b>CLOSE</b> [ <b>(</b>NázevSouboru<b>)</b> ]
   
   ■  <b>NázevSouboru</b> ... Název kapitoly F. Příkaz uzavře uvedený soubor. Pokud
                       není jméno  souboru uvedeno,  uzavře všechny otevřené
                       datové soubory.
   <b>Poznámka:</b>
   Fyzické soubory se uzavřou ihned po použití. Uzavření <b>všech</b> souborů se
   provede také jako vedlejší efekt příkazu <a href="#t626">exec</a>.
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t510"></a>
## volání editoru

<pre>
                                <b>EDITACE DAT</b>
 ──────────────────────────────────────────────────────────────────────────────
 Vyvolání datového editoru procedurou <b>edit</b>, obdobně uživatelské pohledy.

 ██ syntaxe:       <b>EDIT (</b> Soubor [ <b>/</b>Klíč ] <b>,</b> DruhEditace
                        [<b>,</b><a href="prostredi.md#t519">Parametry editace</a>]
                        [<b>,</b><a href="editory.md#t529">Procedurální parametry editace</a>] <b>)</b>
                   <b>EDIT (</b> RecordProměnná<b>,</b>... <b>)</b>

 ■ <b>Soubor</b> ........ Název kapitoly F nebo <a href="formular-sestava.md#t407">record proměnná</a>, 
                   viz. <a href="#t511">editace record proměnné</a>
 ■ <b>Klíč</b> .......... Název vlastního klíče souboru, pouze při indexové podpoře.
                   Implicitně <b>@</b>. Věty souboru se zobrazí v pořadí dle daného
                   klíče. Věty se stejnou hodnotou klíče (duplicity) se zobrazí
                   podle fyzického pořadí. Věty neindexového souboru se zobrazí
                   dle fyzického pořadí.

 ■ <b>DruhEditace</b> ... 4 základní varianty zadání
                   1... [<b>^</b>] <b>(</b> [ SeznamÚdajů ] [ <b>?</b> ] [ <b>!</b> ] <b>)</b>
                        <a href="formular-sestava.md#t538">Seznam údajů</a> ... automatický rozvrh obrazovky editoru
                        <b>^</b> negace: jen údaje neuvedené v seznamu
                        <b>?</b> interaktivní výběr údajů ze souboru
                        <b>!</b> všechny údaje včetně vypočítaných (#C)
                   
                   2... NázevFormuláře   
                        editace podle formuláře (kapitola <a href="formular-sestava.md#t361">E</a>)
                   3... <b>U=</b>NázevPohledu 
                        editace podle uživatelského pohledu <a href="soubory.md#t350">#U</a>
                   4... <b>[</b> DynamickáDeklarace <b>]</b>  
                        Textový výraz v hranatých závorkách, který obsahuje 
                        text podle syntaxe kapitoly E (viz.<a href="soubory.md#t554">Dynamické deklarace</a>)

 ■ <a href="prostredi.md#t519">parametry editace</a>  
   <b>cond</b> ..... editace vybraných vět       <b>head</b> .................. první řádek
   <b>tab</b> ....... nastavení tabelátorů       <b>journal</b> ...sledování změn v žurnálu
   <b>dupl</b> ..... automatická duplikace       <b>watch</b>, <b>refresh</b> ....... lokální sítě
   <b>mode</b> ............... mód editace       <b>exit</b> ............ přerušení editace
   <b>saveafter</b>... uložení změn na disk      <b>ww</b> ................. <a href="#t524">editace v okně</a>
   <b>last</b>,<b>ctrl</b>,<b>alt</b>,<b>shift</b>..alternativní nápovědy 

 ■ <a href="editory.md#t529">procedurální parametry editace</a>
   <b>recno</b>, <b>reckey</b>, <b>field</b>, <b>irec</b> ... počáteční nastavení
   <b>owner</b> ........................ editace podřízených vět
   <b>check</b> ........................ kontrola syntaxe dynamické deklarace

 <a href="jazyk.md#t550">funkce datového editoru</a> ... <b>edrecno</b> <b>edreckey</b> <b>edirec</b> <b>edfield</b> <b>edbreak</b> <b>edupdated</b>
 ──────────────────────────────────────────────────────────────────────────────
 <a href="formular-sestava.md#t361">formulář</a>       <a href="#t551">░ volání editoru</a>     <a href="jazyk.md#t789">»P</a>                                     <a href="sql.md#t885">SQL</a>
</pre>

<a name="t511"></a>
## editace record proměnné

<pre>
                          <b>EDITACE RECORD PROMĚNNÉ</b>
   ───────────────────────────────────────────────────────────────────────────

   Při editaci <a href="#t405">record</a> proměnné se edituje jen daná věta. Neprovádí se aditivní
   změny resp. změny pro referenční integritu.

   <b>Nelze zadat parametry :</b> journal, cond, refresh, saveafter, owner,recno,
                           reckey,irec,exit(..:report..).
                           (případně. převzaté z U=.. se ignorují)
   <b>mode</b>='01' je implicitní
   <b>klávesy</b>   F2,F3,F6,CtrlF2,CtrlF3 jsou nefunkční.

   ───────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t519">parametry editace</a>       <a href="editory.md#t529">procedurální parametry editace</a>       <a href="#t510">volání editoru</a>
</pre>

<a name="t524"></a>
## editace v okně

<pre>
   Ctrl-Home                  <b>EDITACE V OKNĚ</b>
   ──────────────────────────────────────────────────────────────────────────
   Implicitně (při neuvedení parametru <b>ww</b>) se otevře editační okno na celou
   obrazovku. Tj. ww=(1,1,80,25).

   ██ <b>WW = (</b>[<b>(</b>] Souřadnice [ <b>,</b>Rámeček [ <b>,</b>Atributy ] ] [<b>)</b>] <b>)</b>
   
   ■  <b>Souřadnice</b>,
      <b>Rámeček</b> ....... Syntaxe i význam jsou stejné jako v příkaze <a href="#t474">with window</a>.
                      Viz. také <a href="#t475">definice okna</a>.

   ■  <b>Atributy</b> ...... Atr<b>,</b>Norm<b>,</b>Hili [<b>,</b>Subset [<b>,</b>Deleted [<b>,</b>Tab [<b>,</b>Select ]]]]
                      Minimálně 3, max. 7 atributů barev. Zadávají se buď
                      kódem <a href="formular-sestava.md#t241">barvy</a> (číselný výraz) nebo Ctrl-znakem. 
                      <b>Atr</b> ...... barva výplně okna, rámečku
                      <b>Norm</b> ..... barva pro zobrazení dat
                      <b>Hili</b> ..... barva pro zvýrazněná data (kurzor)
                      <b>Subset</b> ... barva vybrané podmnožiny
                      <b>Deleted</b>... barva pro zrušené věty (<a href="prostredi.md#t829">LAN</a>)
                      <b>Tab</b> ...... barva pro znak tabelátoru
                      <b>Select</b> ... barva pro výběr do pracovního indexu (<a href="editory.md#t529">sel</a>)
                      Je-li hodnota <b>0</b>, doplní se implicitně dle <a href="prostredi.md#t221">instalace barev</a>


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  edit(DATA,(jméno,adresa,tlf),ww=(5,15,55,20));   {minimální varianta}

       edit(DATA,(jméno,adresa,tlf),
            ww=(5,15,55,20,'',7,20,5,12,0));            {maximální varianta}

       edit(DATA,Formulář,                        { změna první barvy(Atr) }
            ww=((1,3,80,24,'DATA',^Q,0,0,0)));    { ostatní implicitní     }
                                                  { okno bez obnovy        }

   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t789">»P</a>          <a href="#t476">░ okna</a>
</pre>

<a name="t551"></a>
## ░ volání editoru

<pre>
                          <b>PŘÍKLAD - VOLÁNÍ EDITORU</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ menuloop 'EDITACE SOUBORU DATA' of
 ░   'Automatický rozvrh obrazovky': edit(DATA,());
 ░   'Podle formuláře':edit(DATA,Formulář);
 ░   'Jen vybrané údaje':edit(DATA,(jméno,adresa,telefon));
 ░   'Ruční výběr údajů':edit(DATA,(?));
 ░   'Vybraná skupina vět':edit(DATA,(),cond=(mzda&gt;3000));
 ░ end;

 ░ edit(DATA,Formulář,mode='F2');                         {pořízení nových vět}
 ░ edit(DATA,Formulář,mode='01',recno=PARAM.Věta);         {editace jedné věty}
 ░ edit(DATA,Formulář,mode='^n');                   {zákaz pořízení nových vět}
 ░ edit(DATA,Formulář,mode='!!');                      {jen prohlížení souboru}

 ░ edit(DATA,(!),tab=(jméno,oddělení,telefon),dupl=(oddělení),saveafter=10);
     {i vypočítané údaje, tabelátor a duplikace, automatický save po 10 větách}
 ░ edit(DATA,(),mode='^y#L#A');
                          {zákaz mazat věty, vypínat kontroly a aditivní změny}
 ░ edit(DATA,Formulář,tab=(telefon,fax),noed=^(telefon,fax));
                                  {možno měnit jen vybrané údaje s tabelátorem}
 ──────────────────────────────────────────────────────────────────────────────
 <a href="formular-sestava.md#t563">░ volání sestavy</a>
</pre>

<a name="t567"></a>
## třídění

<pre>
                                  <b>TŘÍDĚNÍ</b>
  ──────────────────────────────────────────────────────────────────────────
   Třídění datového souboru.

   ██ Syntaxe:     <b>SORT (</b> NázevSouboru<b>,( </b>TřídícíÚdaje<b> ))</b> 

   ■  <b>NázevSouboru</b> ... Název kapitoly F.
   ■  <b>TřídícíÚdaje</b> ... [<b>&gt;</b>] [<b>~</b>] NázevÚdaje [{<b>,</b> [<b>&gt;</b>] [<b>~</b>] NázevÚdaje }]
                       Seznam třídících údajů je uzavřen v kulatých závorkách
                       a obsahuje alespoň jeden údaj. Speciální znaky před
                       názvy třídících údajů:
                       <b>&gt;</b> ... sestupné setřídění (jinak vzestupné)
                       <b>~</b> ... <a href="jazyk.md#t668">lexikální třídění</a> (u textových údajů)

   <b>░░░░░░░░░░░░</b>    sort(PRAC,(&gt;Věk))                 {sestupně podle věku}
   <b>░░příklady░░</b>    sort(PRAC,(OsobníČíslo))          {vzestupně podle čísla}
   <b>░░░░░░░░░░░░</b>    sort(PRAC,(~Příjmení,~Jméno))     {lexikálně}

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t568"></a>
## přímý přístup k datům

<pre>
                           <b>PŘÍMÝ PŘÍSTUP K DATŮM</b>
   ──────────────────────────────────────────────────────────────────────────
   Kromě implicitních  datových vazeb, deklarovaných v kapitole F, lze s daty
   pracovat procedurálními  prostředky pro přímý  přístup k datům.  Základním
   pojmem je zde <a href="#t575">číslo věty</a>, které  jednoznačně identifikuje data, se kterými
   chceme pracovat. Lze použít dvě úrovně přístupu:

   ■ <b>přístup k jednotlivým údajům</b>... <b>NázevSouboru[FyzČísVěty].NázevÚdaje</b>
                                     Hranaté závorky se skutečně píší.
                                     <b>FyzČísVěty</b> je číselný výraz. Výsledkem
                                     je výraz typu, který odpovídá typu údaje.
                                     Může se vyskytovat na levé straně přiřa-
                                     zovacího příkazu a ve výrazech.

   ■ <b>přístup k větám</b> ......... Je preferovaný  v novějších verzích  (od 3.0).
                               Základní jednotkou pro přístup na disk je celá
                               věta (<a href="formular-sestava.md#t407"> record</a> ) souboru. Přístup k údajům věty
                               se realizuje v rámci operační paměti.
                               <a href="#t403">readrec</a>     <a href="#t572">deleterec</a>    <a href="#t572">appendrec</a>     <a href="jazyk.md#t573">keyof</a>
                               <a href="#t404">writerec</a>    <a href="#t572">isdeleted</a>    <a href="#t572">recallrec</a>     <a href="#t574">linkrec</a>
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t654">výrazy</a>         <a href="jazyk.md#t658">typy výrazů</a>         <a href="editory.md#t327">typy údajů</a>         <a href="#t441">forall</a>
</pre>

<a name="t572"></a>
## deleterec

*Též:* `isdeleted`, `recallrec`, `appendrec`

<pre>
                  <b>ZALOŽENÍ, RUŠENÍ A OBNOVENÍ VĚTY SOUBORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz pro zrušení věty souboru má dvě varianty podle způsobu identifikace
   věty: fyzickým číslem věty nebo hodnotou vlastního klíče. U neindexovaných
   souborů dojde k <b>fyzickému</b> zrušení věty v souboru ( věty za zrušenou se po-
   sunou "dopředu"). U indexovaných souborů se věta pouze označí jako <b>neplatná</b>
   a vyřadí se všechny její indexy.  Fyzicky bude věta vyloučena  při celkové
   reorganizaci (<a href="#t501">indexfile</a>-<a href="#t501">compress</a>,výstup merge). Odtud plyne možnost obnovy
   zrušené věty (<a href="#t572">recallrec</a>).  Test platnosti věty indexového  souboru provede
   funkce <b>isdeleted</b>. Obnovu zrušené (zneplatněné) věty indexového souboru
   provede příkaz <b>recallrec</b>.

   ██ syntaxe:   <b>DELETEREC (</b>NázevSouboru <b>,</b>ČísloVěty [<b>,+</b>] <b>)</b>
                 <b>DELETEREC (</b>NázevSouboru [ <b>/</b>NázevKlíče ] <b>,</b>HodnotaKlíče [<b>,+</b>] <b>)</b>

       funkce    <b>ISDELETED (</b>NázevSouboru <b>,</b>ČísloVěty <b>)</b>  : boolean
       funkce    <b>ISDELETED (</b>RecordProměnná<b>)</b>            : boolean

                 <b>RECALLREC (</b>NázevSouboru <b>,</b>ČísloVěty [<b>,+</b>] <b>)</b>

                 <b>APPENDREC (</b>NázevSouboru<b>)</b>

   ■  <b>ČísloVěty</b> ... Fyzické <a href="#t575">číslo věty</a>, číselný výraz.
   ■  <b>NázevKlíče</b> .. Vlastní klíč souboru. Implicitně <b>@</b>.
   ■  <b>HodnotaKlíče</b>  Textový výraz, hodnota klíče v interní podobě. 
                    Viz. <a href="jazyk.md#t573">KeyOf</a>, <a href="sql.md#t885">SQL</a>
   ■  <b>+</b> ........... Provedení aditivních změn. V případě havárie se aditivní
                    změny neprovedou a nastaví se <a href="#t626">exitcode</a>=1.

   <b>IsDeleted</b> je funkce definovaná pro indexové soubory (<a href="soubory.md#t324">soubor formátu .DBF</a>)
   Vrací  <b>true</b>  pokud  je  věta <b>neplatná</b>.  Druhá  varianta  testuje výsledek
   příkazu <a href="#t403">readrec</a>.  Vrací true když je přečtená věta  neplatná , respektive
   se hledaná hodnota klíče nenašla.
   U indexovaných souborů příznak <b>IsDeleted</b> udává, zda je věta platná (false)
   nebo neplatná (true). Přidávání (rušení) vět u neindexovaných souborů zna-
   mená reorganizaci celého souboru, zatímco u indexovaných pouze nastavení
   příznaku.

   <b>RecallRec</b> označí větu indexového souboru za platnou, nastaví prefix věty
   <b>IsDeleted</b>:=false, zkontroluje duplicitu a zařadí větu do indexů.

   <b>AppendRec</b> připojí prázdnou větu za konec souboru. U indexového souboru
   bude věta označena jako neplatná (isdeleted:=true). Příkaz je poněkud
   archaickým pozůstatkem ze starších verzí (kompatibilita).


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  SKLAD[x].ks+= -Mnozstvi;                    {přímý přístup do souboru}
       SKLAD[SKLAD.NRecs].Cena:=10.5;
       appendrec(DATA)                                      {připojování vět}
       deleterec(SKLAD,1)                                        {mazání vět}
       deleterec(SKLAD,SKLAD.nrecs)
 
   ██  appendrec(DATA);           { zařazení nové věty do indexového souboru}
       věta:=DATA.nrecsabs;       { Tento způsob ovšem silně voní nostalgií }
       DATA[věta].údaj1:=...      { po verzi 2.2 }
       DATA[věta].údaj2:=...
       ...
       recallrec(DATA,věta);

       { <b>lepší způsob</b> }
       VAR  rec : record of soubor ;
       ....
       rec.udaj1:= ... ;               { méně fyzických přístupů do souboru }
       rec.udaj2:= ... ;               { "měkčí" na blokování v LAN }
       <b>writerec</b>( rec,0) ;

   ██  i:=1;                     {obnovení zrušených vět v indexovém souboru}
       repeat if isdeleted(DATA,i) then recallrec(DATA,i)
              i:=i+1;
       until i=DATA.nrecsabs
   ─────────────────────────────────────────────────────────────────────────
   <a href="#t403">readrec</a>       <a href="#t574">linkrec</a>        <a href="soubory.md#t336">klíče</a>                  <a href="#t568">přímý přístup k datům</a>
   <a href="#t404">writerec</a>      <a href="#t572">appendrec</a>      <a href="formular-sestava.md#t407">record proměnná</a>
</pre>

<a name="t574"></a>
## linkrec

<pre>
                                  <b>LINKREC</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz  procedury,  který  pro  větu  podřízeného  souboru vyhledá a načte
   odpovídající větu nadřízeného souboru.

   ██ Syntaxe:    <b>LINKREC (</b> RecordProměnná1<b>,</b>
                          [ NázevSpojení<b>(</b>] RecordProměnná2 [<b>)</b>]<b> )</b>

   ■  <b>RecordProměnná1</b> ... věta souboru, ke které hledáme "nadřízenou" větu
   ■  <b>RecordProměnná2</b> ... sem bude (v případě úspěchu) načtena věta nadříz.
                          souboru
   ■  <b>NázevSpojení</b> ...... Druh vazby na nadřízený soubor (viz. <a href="jazyk.md#t346">cizí klíč</a>)
                          NázevSouboru | NázevKlíče | NázevRole

   ■  <b>Provedení příkazu</b>   Pokud příkaz proběhl úspěšně (věta byla nalezena)
                          bude <a href="#t626">exitcode</a> = 0, 
                               exitcode = 1  v opačném případě

   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t580">link</a>      <a href="#t403">readrec</a>      <a href="#t404">writerec</a>      <a href="jazyk.md#t573">keyof</a>     <a href="sql.md#t885">SQL</a>
</pre>

<a name="t575"></a>
## číslo věty

<pre>
                                 <b>ČÍSLO VĚTY</b>
   ───────────────────────────────────────────────────────────────────────────
   Při hledání podle <b>vlastního</b> nebo <b>cizího klíče</b> se pracuje s čísly vět, které 
   lze chápat v různém kontextu.
   
   ■ <b>fyzické číslo</b> ... Jednoznačná identifikace věty pořadovým číslem jejího
                       fyzického uložení v souboru (na disku). Fyzickým číslem
                       je možno "adresovat" věty i v souboru bez klíče, 
                       v indexovém souboru i zrušené (zneplatněné) věty.

   ■ <b>logické číslo</b> ... Vždy se vztahuje k určitému vlastnímu klíči souboru.
                       Je to pořadové číslo věty od počátku v pořadí podle
                       některého z vlastních klíčů. Pokud jsou v klíči povoleny
                       duplicity <b>nemusí být</b> obecně logické číslo <b>jednoznačné</b>.

   ■ <b>interní string klíče</b> ... Kompatibilita s prostředím <a href="sql.md#t885">SQL</a> vedla k podpoře
                       manipulace s interními hodnotami klíčů ( aniž bychom 
                       se museli zabývat jejich přesnou interní strukturou).
                       Nutnou podmínkou je <b>indexová podpora</b> souborů.
                       Odkazy: <a href="jazyk.md#t573">keyof</a> 

   Převod logického na fyzické <a href="jazyk.md#t579">recnoabs</a> a naopak <a href="jazyk.md#t579">recnolog</a>. Fyzické číslo věty
   s určitou hodnotou klíče vrací funkce <a href="jazyk.md#t579">  recno</a>. Funkce <a href="jazyk.md#t580">link</a> vrátí fyzické
   číslo nadřízené věty.
   Kombinovaný přístup pomocí fyzického čísla i klíče : <a href="#t403">readrec</a>, <a href="#t404">writerec</a>,
                                                        <a href="#t572">deleterec</a>
   ──────────────────────────────────────────────────────────────────────────
   <a href="soubory.md#t336">klíče</a>             <a href="jazyk.md#t789">»P</a>         <a href="#t568">přímý přístup k datům</a>
</pre>

<a name="t588"></a>
## příkazy pro tisk textu

*Též:* `setprinter`, `cprinter`, `printtxt`

<pre>
                          <b>PŘÍKAZY  PRO TISK TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   Základním způsobem tisku v PC FANDu je  <a href="editory.md#t240">tisk textu</a>  z prostředí  textového
   editoru. Tisk textu lze vyvolat i explicitně příkazem procedury <b>printtxt</b>.

   ██ syntaxe:       <b>PRINTTXT  (</b> NázevTextSouboru | LokálníPromTypuString <b>)</b>

   ───────────

   V PC FANDu je možno instalovat až 10 tiskáren, ze kterých lze interaktivně
   vybrat jednu  aktivní tiskárnu.  Pro manipulaci  se seznamem instalovaných
   tiskáren je možno použít příkazy a funkce procedury:
   <b>setprinter</b> .... příkaz pro nastavení aktivní tiskárny,implic.první z .CFG
   <b>cprinter</b> ...... funkce typu real, vrací číslo aktivní tiskárny (0,1,..)

   ██ syntaxe:       <b>SETPRINTER ( </b>ČísVýraz <b>)</b>
                     <b>CPRINTER</b> : real

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t222">instalace tiskárny</a>             <a href="editory.md#t240">tisk textu</a>           <a href="#t605">░ editace a tisk textu</a>
</pre>

<a name="t605"></a>
## ░ editace a tisk textu

<pre>
                       <b>PŘÍKLAD - EDITACE A TISK TEXTU</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ edittxt('POZNAMKY.TXT');
 ░ edittxt('NAVOD.DOC',noedit);                                {jen prohlížení}
 ░ edittxt(TEXT);                                 {název souboru je v katalogu}
 ░ edittxt('DOPIS.TXT',ww=(10,3,70,22));                       {editace v okně}
 
 ░ begin                                                    {nastavení kurzoru}
 ░   edittxt('FAND.DOC',txtpos=PARAM.Pozice);
 ░   PARAM.Pozice:=txtpos;
 ░ end;

 ░ printtxt('POZNAMKY.TXT');
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t613"></a>
## backup

*Též:* `nocompress`

<pre>
                                   <b>BACKUP</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz procedury pro <a href="prostredi.md#t616">zálohování</a> dat.

   ██ Syntaxe:           <b>BACKUP(</b> NázevArchivace [<b>,nocancel</b>] [<b>,nocompress</b>] <b>)</b>

   ■  <b>NázevArchivace</b> ... Označení úrovně archivace, která se má provést.
                         Viz. <a href="prostredi.md#t616">zálohování</a>, <a href="prostredi.md#t641">katalog</a>

   ■  <b>nocancel</b> ......... Při chybě nedojde k ukončení programu, ale po návratu
                         z archivace lze testovat úspěšné ukončení <a href="#t626">exitcode</a>=0.
   ■  <b>nocompress</b> ....... Data jsou zálohována bez komprese, vede k urychlení
                         a k snížení nároku na paměť při provedení příkazu.

   ■  <b>LAN</b> ... mód sdílení Rd - pro síťové datové soubory, deklarované v 
                               nějakém aktivním .RDB
              ReadOnly ( open-modus) - ostatní datové a textové soubory
   
   Při zálohování se rovněž kopírují ( s kompresí ) případné soubory  volných
   textů -  ale pozor  - jen pro aktivní datové soubory - tj. pro kapitoly  F
   aktuální a  nadřízené úlohy (resp.úloh). Soubory se kopírují v pořadí  dle
   čísel archivace  (nejdříve s  číslem Ar  a potom  další v  seznamu) a  dle
   pořadí výskytu v katalogu. Kopírují se i prázdné soubory.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t614">restore</a>        <a href="prostredi.md#t223">instalace konstant</a>
</pre>

<a name="t614"></a>
## restore

<pre>
                                  <b>RESTORE</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz procedury pro obnovu zálohovaných dat.

   ██ Syntaxe:           <b>RESTORE(</b> NázevArchivace [<b>,nocancel</b>] [<b>,nocompress</b>] <b>)</b>

   ■  <b>NázevArchivace</b> ... Označení úrovně archivace, která se má provést.
                         Kopíruje i soubory .T00.  Indexové soubory zruší.
                         Kontroluje délku věty srovnáním na deklaraci.
                         Viz. <a href="prostredi.md#t616">zálohování</a>, <a href="prostredi.md#t641">katalog</a>.

   ■  <b>nocancel</b> ......... Při chybě nedojde k ukončení programu, ale po návratu
                         z obnovy dat lze testovat úspěšné ukončení <a href="#t626">exitcode</a>=0.

   ■  <b>nocompress</b> ....... Data jsou zálohována bez komprese, vede k urychlení
                         a k snížení nároku na paměť při provedení příkazu.

   ■  <b>LAN</b> ... mód sdílení Excl - pro síťové datové soubory, deklarované v
                                 nějakém aktivním .RDB
              Exclusive (open modus) - ostatní datové a textové soubory 

   ██ <b>Důležité:</b>  seznam  souborů  s odpovídajícími  archivačními čísly se mezi
      backup a restore nesmí změnit.Jestliže příkaz <b>restore</b> nenajde požadovaný 
      soubor, přeruší obnovení. <a href="#t613">Backup</a> zálohuje i prázdné soubory !
   ───────────────────────────────────────────────────────────────────────────
</pre>

<a name="t620"></a>
## restorem

*Též:* `overwrite`, `subdir`, `backupm`

<pre>
                            <b>FYZICKÉ ZÁLOHOVÁNÍ</b>
   ──────────────────────────────────────────────────────────────────────────
   Fyzické <a href="prostredi.md#t616">zálohování</a> v PC FANDu probíhá podle podobného schématu jako u zná-
   mých  obecných utilit  typu ZIP, ARJ apod.  Příkazem <b>backupm</b> se komprimují
   soubory určitého adresáře, případně jeho podadresářů  do jednoho společného
   souboru, přičemž lze pomocí masky vybrat jen určité soubory. Příkazem 
   <b>restorem</b> se zálohované soubory obnoví.

   ██ Syntaxe:    <b>BACKUPM(</b> NázevArchivace<b>,</b>adresář<b>,</b> SeznamMasek
                           [<b>,nocancel</b>] [<b>,nocompress</b>] [<b>,subdir</b>] <b>)</b>

                  <b>RESTOREM(</b>NázevArchivace<b>,</b>Adresář
                           [<b>,subdir</b>][<b>,overwrite</b>][<b>,nocancel</b>][<b>,nocompress</b>])

   ■  <b>Název archivace</b> ... položka katalogu s názvem úlohy <b>ARCHIVES</b> a názvem
                          archivace v údaji pro název souboru. Dále je zadána
                          cesta a návěští, kam se zapisuje archivní soubor.
                          Podporují se pokračovací diskety (jako při <a href="#t613">backup</a>)

   ■  <b>Adresář</b> ........... textový výraz pro adresář, ve kterém se nacházejí
                          archivované resp. obnovované soubory

   ■  <b>Seznam masek</b> ...... textový výraz, <b>backupm</b> vybere pro archivaci jen
                          ty soubory, které vyhovují zadaným maskám. Jednotlivé
                          masky jsou odděleny čárkou nebo mezerou.
                          Jsou povoleny znaky <b>*</b> a <b>?</b> s obvyklým významem.

   ■  <b>subdir</b> ............ příkaz <b>backupm</b> kopíruje i podadresáře
                          příkaz <b>restorem</b> automaticky vytváří hlavní adresář 
                          i podadresáře
                          
   ■  <b>overwrite</b> ......... příkaz <b>restorem</b> automaticky přepisuje případné 
                          existující soubory, jinak přepis existujícího 
                          souboru jen s dotazem

   ■  <b>nocancel</b> ......... Při chybě nedojde k ukončení programu, ale po návratu
                         z obnovy dat lze testovat úspěšné ukončení <a href="#t626">exitcode</a>=0.

   ■  <b>nocompress</b> ....... Data jsou zálohována bez komprese, vede k urychlení
                         a k snížení nároku na paměť při provedení příkazu.


   <b>LAN</b>
   Při fyzickém zálohování jsou všechny zálohované soubory otevírány v režimu
   exclusive. To znamená, že se zálohovanými nebo obnovovanými soubory není
   možno z jiné stanice vůbec pracovat, ani je mít třeba jen otevřené.
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t614">restore</a>      <a href="#t613">backup</a>
</pre>

<a name="t626"></a>
## externí programy

*Též:* `exec`, `exitcode`, `freemem`, `loadfont`, `textmode`

<pre>
                              <b>EXTERNÍ PROGRAMY</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkazem <b>exec</b> voláme vnější program (.COM, .EXE), příkaz MS-DOSu nebo .BAT
   proceduru.  Volanému programu lze předat parametry.  Při předání řízení je
   možno ovlivnit velikost volné paměti a mód obrazovky.

   ██ Syntaxe:        <b>EXEC (</b> Program <b>,</b> ParamProgramu
                           [<b>,nocancel</b>] [<b>,freemem</b>] [<b>,loadfont</b>] [<b>,textmode</b>] <b>)</b>

   ██ Základní        <b>exec</b>(Program, Param,...).. volání programu s parametry
   ██ varianty:       <b>exec</b>('',<b>DOSPříkaz</b>, ...) .. interní příkaz DOSu nebo .BAT
                      <b>exec</b>('','',...)        ... odskok do DOSu

   ■ <b>Program</b> ........ fyz.jméno vč.cesty v apostrofech nebo log.jméno (<a href="prostredi.md#t641">katalog</a>)
                      při hledání programu se zohledňuje systémová proměnná <b>PATH</b>
   ■ <b>ParamProgramu</b>... TextovýVýraz (parametry volaného programu)
   ■ <b>DOSPříkaz</b> ...... TextovýVýraz (copy, time, del apod. včetně parametrů)
   ■ <b>nocancel</b> ....... potlačí ukončení úlohy při chybě a číslo chyby je 
                      přístupné funkcí <b>exitcode</b>. Číslo chyby nastavuje program
                      a většinou platí: Exitcode=0 =&gt; bez chyby
   ■ <b>freemem</b> ........ uvolňuje maximum paměti - nelze použít pro DML-programy
   ■ <b>loadfont</b> ....... při návratu do PC FANDu se obnoví fonty
   ■ <b>textmode</b> ....... pokud PC FAND pracuje v grafickém módu a volaný program
                      komunikuje v textovém módu.

   Chyba v prováděném programu (tj. ukončení s nenulovým návratovým kódem) má  
   za následek chybové hlášení a přerušení  FANDovské aplikace. Pokud se však
   použije parametr <b>nocancel</b>, aplikace se nepřeruší a pouze nastaví návratový
   kód <b>exitcode</b>.

   Při nenalezení programu nebo nedostatku paměti však je aplikace přerušena
   bez ohledu na parametr <b>nocancel</b>.
 
   ██ <b>EXITCODE</b> .. funkce typu <b>real</b>, návratový kód posledního volaného příkazu.
                  Nastavují příkazy  <a href="#t626">exec</a>,<a href="#t631">mount</a>,<a href="#t721">gettxt</a>,<a href="#t404">writerec</a>,<a href="#t572">deleterec</a>,
                                     <a href="#t572">recallrec</a>, <a href="#t574">linkrec</a>, <a href="#t613">backup</a>, <a href="#t614">restore</a>,
                                     <a href="jazyk.md#t737">selectstr</a>,<a href="#t629">checkfile</a>, <a href="jazyk.md#t740">evalb</a>, <a href="jazyk.md#t740">evalr</a>, <a href="jazyk.md#t740">evals</a>
                                     <a href="prostredi.md#t609">copyfile</a>, <a href="#t620">backupm</a>, <a href="#t620">restorem</a>
                                     report a edit s parametrem <a href="soubory.md#t554">check</a>
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t627">░ externí programy</a>
</pre>

<a name="t627"></a>
## ░ externí programy

<pre>
                         <b>PŘÍKLAD - EXTERNÍ PROGRAMY</b>
   ──────────────────────────────────────────────────────────────────────────

   ██ { příklad odskoku z úlohy do MS-DOSu - v obecném případě je vhodné
        použít parametr <b>textmode</b>, neboť např. na obrazovce HERCULES je podpora
        diakritiky realizována v grafickém režimu }

       exec( '','',textmode, freemem ) ;


   ██ { další běžné příklady }
      menuloop 'EXTERNÍ PROGRAMY' of
        'Program v DML': exec('\PASCAL\VYPOCET','');
        'Služební program': exec('C:\XTPRO\XTPRO','',freemem);
        'Formátování disket': exec('C:\SYSTEM\FORMAT','A:');
        'Výpis adresáře': exec('','dir /w');
        'Volání .BAT procedury PROC.BAT': exec('','proc') ;
        'Volání operačního systému': exec('','',freemem);
      end;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t629"></a>
## checkfile

<pre>
                                 <b>CHECKFILE</b>
   ───────────────────────────────────────────────────────────────────────────

   Příkaz procedury <b>checkfile</b> provede test, zda zadaný fyzický soubor odpovídá
   deklaraci datového souboru PC FANDu.  Výsledek testu vrací prostřednictvím
   funkce exitcode. Nelze používat pro deklarace typu .SQL,.DBF,.DTA.

   ██ Syntaxe:         <b>CHECKFILE (</b> NázevSouboru<b>,</b> Cesta <b>)</b>

   ■  <b>NázevSouboru</b> ... Název kapitoly F, identifikátor.
   ■  <b>Cesta</b> .......... Fyzická cesta k souboru (v apostrofech) nebo název
                       dle katalogu (identifikátor).
   ■  <a href="#t626">ExitCode</a> ....... <b>0</b> - O.K.
                       <b>1</b> - fyzický soubor neexistuje(prázdný)
                       <b>2</b> - chybná cesta a pod.
                       <b>3</b> - nesoulad délky věty (deklarace &amp; hlavička)
                           nebo fyz.délka souboru &lt; dle hlavičky
                       <b>4</b> - pro volné texty neex. .T00

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t641">katalog</a>       <a href="jazyk.md#t728">filesize</a>
</pre>

<a name="t631"></a>
## releasedrive

*Též:* `mount`

<pre>
                           <b>MANIPULACE S DISKETAMI</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz pro explicitní montování diskety. Implicitně se tento příkaz vyvolá 
   před použitím prvního souboru z diskety

   ██ Syntaxe:             <b>MOUNT (</b> NázevDleKatalogu [ <b>,nocancel</b> ] <b>)</b>

   ■  <b>NázevDleKatalogu</b> ... Soubor musí být v katalogu definován jako disketový
                           včetně návěští ( identifikace diskety ).
   ■  <a href="#t300">nocancel</a> ........... Potlačí ukončení práce po stisku ESC a nastaví
                           návratový kód <a href="#t626">exitcode</a>=1
   ──────────────────────

   Nasazenou disketu lze vyjmout z jednotky jen na výzvu programu. To se
   implicitně stane až při ukončení úlohy nebo požadavku na jinou disketu.
   Explicitně lze korektní vyjmutí diskety zajistit příkazem <b>releasedrive</b>.

   ██ Syntaxe:             <b>RELEASEDRIVE (</b> Jednotka <b>)</b>
   
   ■  <b>Jednotka</b> .... Textový výraz (<b>A:</b>,<b>B:</b>,CPM-jednotka)

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t641">katalog</a>      <a href="prostredi.md#t638">katalog v proceduře</a>
</pre>

<a name="t640"></a>
## generační soubory

*Též:* `turncat`

<pre>
                             <b>GENERAČNÍ SOUBORY</b>
  ───────────────────────────────────────────────────────────────────────────
  <b>Generační soubory</b>  slouží k fyzickému  rozdělení dat do více  souborů podle
  nějakého kritéria, nejčastěji časového - např. měsíce roku ( 12 generací ).
  Potom musí být v katalogu odpovídající počet zápisů se stejným názvem úlohy
  a souboru. Aktuálně se použije první zápis. <b>Číslo generace</b> je uloženo jako
  <b>poslední dva znaky</b> v příponě fyzické cesty k souboru (Path). (<a href="prostredi.md#t641">katalog</a>)
  Cyklická změna pořadí generačních souborů -tzv. <b>otočení katalogu</b>-se provede
  příkazem <b>turncat</b>.
  
  ██ Syntaxe:     <b>TURNCAT (</b> NázevDleKatalogu <b>,</b>Číslo <b>)</b>

  ■  <b>Číslo</b> ...... Číselný výraz. Udává směr a počet generací posunu.

  S číslem generace lze v proceduře pracovat pomocí metod přímého přístupu
  do katalogu, viz. <a href="prostredi.md#t638">generation</a>.

  <b>░░░░░░░░░░░░</b>
  <b>░░příklady░░</b>
  <b>░░░░░░░░░░░░</b>

  ██  { obsah katalogu }

      <b>NazUlohy NazSouboru  Ar   Cesta                      Navesti </b>
      FAKTURY   DOKLADY    00  C:\ASR\DOKLADY.001
      FAKTURY   DOKLADY    00  C:\ASR\DOKLADY.002
      FAKTURY   DOKLADY    00  C:\ASR\DOKLADY.003
      FAKTURY   DOKLADY    00  C:\ASR\DOKLADY.004
      FAKTURY   DOKLADY    00  C:\ASR\DOKLADY.005
      ........  ........   ..  ........................... ...........

      { procedura }
      menuloop 'Generace' of
         'přejdi k další generaci':turncat(DOKLADY,1);
         'vrať se k minulé generaci':turncat(DOKLADY,-1);
         'vypiš číslo generace': writeln('generace: ',DOKLADY.generation:3);
     end;
  ────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t721"></a>
## puttxt

*Též:* `gettxt`

<pre>
                       <b>ZÁPIS A ČTENÍ TEXTU ZE SOUBORU</b>
   ──────────────────────────────────────────────────────────────────────────

   Načtení textu  z textového souboru  od zadané pozice.  Načte se celý text,
   nebo, pokud je zadán třetí parametr, blok této délky. Pozor na to, že
   pracuje i s texty delšími než 65 000 byte, které načte celé jen při přímém
   přiřazení do lokální proměnné typu string nebo do údaje souboru typu T.
   Pokud nemůže načíst celý soubor až do konce (resp. celý požadovaný úsek), 
   nastaví <a href="#t626">exitcode</a>=1, jinak platí exitcode=0.

   ██ syntaxe :        <b>GETTXT (</b> NázevSouboru [<b>,</b>PoziceOd [ <b>,Délka</b>]] <b>)</b> : string

   ■  <b>NázevSouboru</b> ... fyzické jméno v apostrofech nebo jméno z katalogu
   ■  Kapitoly ....... P,D

   Příklad:
   var s:string;
   begin
   s:=gettxt('soubor'); {zde je exitcode vždy =0, neboť do s se přečte i 
                         dlouhý string do 2GB}

   s:=gettxt('soubor')+'něco k tomu';
   if exicode=1 then 
     message('ze souboru se nepřečetlo vše, neboť výraz typu string &lt;= 65000B');

   ───────────────────────

   Zápis textu do textového souboru.

   ██ syntaxe :        <b>PUTTXT (</b> NázevSouboru <b>,</b> TextVýraz [ <b>,append</b> ] <b>)</b>

   ■  <b>NázevSouboru</b> ... fyzické jméno v apostrofech nebo jméno z katalogu
   ■  <b>append</b> ......... připojení textu za textový soubor
   ■  Kapitoly ....... P

   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t724">zpracování textu</a>     <a href="prostredi.md#t641">katalog</a>
</pre>

<a name="t745"></a>
## putpixel

*Též:* ` line`, `rectangle`

<pre>
                      <b>ELEMENTÁRNÍ GRAFICKÉ KONSTRUKCE</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz  <b>putpixel</b> vykreslí bod.   <b>Line</b> kreslí přímku , která je jednoznačně
   určena zadanými  krajními body. <b>Rectangle</b>  vykreslí obdélník dle  zadaného
   levého horního a pravého dolního rohu.  

   ██ syntaxe:        <b>PUTPIXEL (</b> X<b>,</b> Y<b>,</b> Barva <b>)</b>
                          <b>LINE (</b> X1<b>,</b> Y1<b>,</b> X2<b>,</b> Y2<b>,</b> Barva+Typ+Síla <b>)</b>
                     <b>RECTANGLE (</b> X1<b>,</b> Y1<b>,</b> X2<b>,</b> Y2<b>,</b> Barva+Typ+Síla <b>)</b>

   ■  <b>X</b>,<b>Y</b>,<b>X1</b>,<b>Y1</b>,<b>X2</b>,<b>Y2</b> ... Souřadnice se zásadně zadávají v grafických bodech
                          (pixel) relativně v aktuálním okně.
   ■  <b>Barva</b> ............. Číselný výraz - kód barvy nebo Ctrl-znak.
   ■  <b>Typ</b> ............... 16 ... tečkovaná čára
                          32 ... čerchovaná čára
                          48 ... čárkovaná čára
   ■  <b>Síla</b> .............  64 ... tlustá čára

   Parametry <b>typ</b> a <b>síla</b> pro příkazy <b>line</b> a <b>rectangle</b> jsou zavedeny od verze
   3.3d, jejich součet spolu s parametrem <b>barva</b>

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t751">grafické příkazy</a>
</pre>

<a name="t746"></a>
## floodfill

<pre>
                    <b>FLOODFILL</b> - vyplnění omezené oblasti
   ──────────────────────────────────────────────────────────────────────────
   Příkaz  <b>floodfill</b>  vyplní oblast omezenou zadanou hraniční barvou určitým
   předdefinovaným vzorem v požadované barvě.

   ██ syntaxe:     <b>FLOODFILL (</b> X<b>,</b> Y<b>,</b> Vzor<b>,</b> HrBarva<b>,</b> Barva <b>)</b>

   ■  <b>X</b>,<b>Y</b> ....... Číselné výrazy, bod uvnitř oblasti
   ■  <b>Vzor</b> ...... Číselný výraz, kód typu výplně. Povolené hodnoty 0..11
   ■  <b>HrBarva</b> ... Číselný výraz, kód barvy (Ctrl-znak), která ohraničuje 
                  zadanou oblast. Hranice musí být souvislá.
   ■  <b>Barva</b> ..... Číselný výraz - kód barvy (nebo Ctrl-znak) vzoru, kterým
                  se vyplní oblast.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t751">grafické příkazy</a>
</pre>

<a name="t747"></a>
## ellipse

<pre>
                <b>ELLIPSE</b> - kreslení elipsy, kružnice, oblouku
   ──────────────────────────────────────────────────────────────────────────
   Příkazem pro vykreslení elipsy lze dle zadaných hodnot parametrů vykreslit
   kružnici nebo oblouk (část kružnice nebo elipsy).

   ██ syntaxe:         <b>ELLIPSE (</b> X<b>,</b> Y<b>,</b> RX<b>,</b> RY<b>,</b> Barva [ <b>,</b>StÚhel<b>,</b> KonÚhel ] <b>)</b>

   ■  <b>X</b>,<b>Y</b> ..... Souřadnice se zásadně zadávají v grafických bodech (pixel)
                relativně v aktuálním okně.
   ■  <b>RX</b>,<b>RY</b> ... Číselné výrazy, poloměry pro osu X a osu Y (pixel). 
                Pro vykreslení kružnice nemusí nutně postačovat <b>RX=RY</b>, neboť
                obecně nemusí být "pixel souřadný systém" symetrický ( poměr 
                počtu bodů a rozměrů obrazovky )
   ■  <b>Barva</b> ... Číselný výraz - kód barvy nebo Ctrl-znak.
   ■  <b>StÚhel</b>, 
      <b>KonÚhel</b>.. Počáteční a koncový úhel, implicitně 0..360°

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t751">grafické příkazy</a>
</pre>

<a name="t748"></a>
## outtextxy

<pre>
                                 <b>OUTTEXTXY</b>
   ──────────────────────────────────────────────────────────────────────────
   Příkaz <b>outtextxy</b> vypíše zadaný text v grafickém režimu do aktuálního okna.
   Text  je umístěn na zadanou  pozici, použije se zadaný druh písma (font) v
   požadované velikosti, barvě a směru výpisu.

   ██ syntaxe:      <b>OUTTEXTXY (</b> X<b>,</b> Y<b>,</b> Text<b>,</b> Font<b>,</b> Barva
                              [ <b>,</b>Velikost [ <b>,</b>Směr ]
                              [ <b>,</b>MultX <b>,</b>DivX <b>,</b>MultY <b>,</b>DivY ] ] <b>)</b>

   ■  <b>X</b>,<b>Y</b> ......... Číselné výrazy, "pixel" souřadnice levého dolního okraje
                    výpisu textu. Jsou relativní k aktuálnímu oknu.
   ■  <b>Text</b> ........ Textový výraz, který se vypíše na obrazovku.
   ■  <b>Font</b> ........ Číselný výraz, <b>0</b>,<b>1</b> - Triplex , <b>2</b> - Small
   ■  <b>Barva</b> ....... Číselný výraz - kód barvy, nebo Ctrl-znak. <a href="formular-sestava.md#t241">Barvy</a>
   ■  <b>Velikost</b> .... Číselný výraz - velikost písma, rozsah <b>1..10</b>,
                    implicitně=4, dosadí se i při použití 0 .
   ■  <b>Směr</b> ........ Číselný výraz - <b>0</b> = horizontální výpis (<b>implicitně</b>)
                                    <b>1</b> = vertikální výpis
   ■  <b>MultX</b>,<b>DivX</b>
      <b>MultY</b>,<b>DivY</b>... Číselné výrazy. Aplikují se jako zlomky <b>MultX/DivX</b> a 
                    <b>MultY/DivY</b>. Zjemní a zvýší rozsah parametru <b>velikost</b> v
                    horizontálním i vertikálním směru. V případě jejich 
                    použití je <b>velikost</b> ignorována.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t751">grafické příkazy</a>           <a href="#t459">výstup na obrazovku</a>
</pre>

<a name="t751"></a>
## grafické příkazy

*Též:* `graphics`, `with graphics`

<pre>
                              <b>GRAFICKÉ PŘÍKAZY</b>
   ──────────────────────────────────────────────────────────────────────────
   Grafické příkazy  jsou definovány pouze v módu 80x25.  Kromě příkazu <b>graph</b>
   musí být před použitím grafického příkazu aktivován grafický mód  příkazem
   <b>with graphics</b>. Pokud tomu tak není, příkaz se ignoruje.

   ██ syntaxe:    <b>WITH GRAPHICS DO</b> Příkaz 
   
   ■  <b>Příkaz</b> bude proveden v grafickém módu dle typu obrazovky. Platí pro EGA
             (640x350), VGA (640x450)  Hercules (720x348).  Příkaz může být i
             složený (begin ... end).

   ──────────────────
   <b>Další příkazy pro grafický režim:</b>

   ■ <a href="rozhrani.md#t753">graph</a> ....... grafické zobrazení dat, obchodní (bussines) grafika
   ■ <a href="#t745">putpixel</a> .... kreslení bodu
   ■ <a href="#t745"> line</a> ....... kreslení čáry
   ■ <a href="#t745">rectangle</a> ... kreslení obdélníku
   ■ <a href="#t747">ellipse</a> ..... kreslení elipsy (kružnice)
   ■ <a href="#t746">floodfill</a> ... vyplnění ohraničené oblasti vzorem
   ■ <a href="#t748">outtextxy</a> ... výpis textu zadaným typem, velikostí a směrem písma
   ■ <a href="#t463">getmaxx</a>, <a href="#t463">getmaxy</a> ... maximální grafické souřadnice aktuálního okna
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t908"></a>
##   call

<pre>
                                     <b>call</b>
    ───────────────────────────────────────────────────────────────────────
    <b>call(</b>String_Kapitola<b>,</b>String_Predikát<b>)</b>


         Vyvolá  uvedený predikát  logické procedury.  Volání  je  ukončeno
    neúspěchem pokud  neúspěchem skončí i volání  uvedeného predikátu, nebo
    ve volané proceduře nastala chyba (běhová i error).

    <b>String_Kapitola</b>   Jméno volané logické procedury.
    <b>String_Predikát</b>   Jméno volaného predikátu.

    Příklad:

    main:-call('Firma','main').
    ───────────────────────────────────────────────────────────────────────
    <a href="soubory.md#t397">error</a>    <a href="dalsi-kapitoly.md#t894">syntaxe</a>
</pre>
