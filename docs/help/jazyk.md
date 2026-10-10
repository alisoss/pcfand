# Jazyk: identifikátory, výrazy, funkce

<a name="t264"></a>
## Standardní funkce

<pre>
                    <b>STANDARDNÍ FUNKCE A INTERNÍ PROMĚNNÉ</b>
    ────────────────────────────────────────────────────────────────────────
    Standardní funkce jsou  na rozdíl od uživatelských funkcí ( kapitola D )
    zabudovány  přímo do  PC FANDu.  Jejich  použití  je možné jen  v jejich
    definičním oboru. To znamená, že některé funkce jsou použitelné ve všech
    typech kapitol a některé funkce mají individuální omezení. Z tohoto 
    hlediska je možno vymezit dva <b>základní</b> okruhy funkcí :
    ■ funkce s úplným definičním oborem (použitelné ve všech typech kapitol)
    ■ procedurální funkce - použitelné jen v kapitole P - procedury
    Existují ovšem i funkce, použitelné např. jen v kapitole R.
    <a href="#t741">Interní proměnné</a> mohou na rozdíl  od funkcí  vystupovat i na levé straně
    přiřazovacího příkazu.

    <b>Aritmetické funkce</b>           <a href="#t682">Abs</a>, <a href="#t682">Int</a>, <a href="#t682">Frac</a>, <a href="#t682">Sqr</a>, <a href="#t682">Sqrt</a>, <a href="#t686">Random</a>,
                                 <a href="#t682">Ln</a>, <a href="#t682">Exp</a>, <a href="#t682">Pi</a>, <a href="#t682">Sin</a>, <a href="#t682">Cos</a>, <a href="#t682">ArcTan</a>

    <b>Zpracování textu</b>   řetězce   <a href="#t712">Copy</a>, <a href="#t719">CopyLine</a>, <a href="procedury.md#t721">GetTxt</a>, <a href="#t715">LeadChar</a>, <a href="#t715">TrailChar</a>,
                                 <a href="#t705">Length</a>, <a href="#t719">LineCnt</a>, <a href="#t710">NoDiakr</a>, <a href="#t717">Pos</a>, <a href="#t712">RepeatStr</a>
                                 <a href="#t710">UpCase</a>, <a href="#t710">LowCase</a>, <a href="#t715">Replace</a>
                       znaky     <a href="#t705">Char</a>, <a href="#t705">Ord</a>
                       převody   <a href="#t707">Str</a>, <a href="prostredi.md#t691">StrDate</a>, <a href="#t707">Val</a>
                       editace   <a href="#t299">TxtPos</a>
                         výběr   <a href="#t737">SelectStr</a>
                   vyhodnocení   <a href="#t740">EvalS</a>, <a href="#t740">EvalB</a>, <a href="#t740">EvalR</a>
                         barvy   <a href="#t722">Color</a>

    <b>Stav datového editoru</b>        <a href="#t550">EdBreak</a>, <a href="#t550">EdField</a>, <a href="#t550">EdFile</a>, <a href="#t550">EdIRec</a>, <a href="#t550">EdRecNo</a>, 
                                 <a href="#t550">EdUpdated</a>, <a href="#t550">EdRecKey</a>, <a href="#t550">EdBool</a>, <a href="#t550">IsNewRec</a>

    <b>Komunikace, klávesnice</b>       <a href="#t471">Prompt</a>, <a href="#t471">EscPrompt</a>, <a href="procedury.md#t451">MenuX</a>, <a href="procedury.md#t451">MenuY</a>,
                                 <a href="procedury.md#t487">KeyPressed</a>, <a href="procedury.md#t487">ReadKey</a>, <a href="procedury.md#t487">KeyBuf</a>

    <b>Myš</b>                          <a href="rozhrani.md#t490">MouseEvent</a>, <a href="#t491">IsMouse</a>, <a href="procedury.md#t495">MouseIn</a>, <a href="procedury.md#t495">MouseX</a>, <a href="procedury.md#t495">MouseY</a>

    <b>Zpracování datumu, času</b>      <a href="prostredi.md#t691">CurrTime</a>, <a href="prostredi.md#t691">Today</a>, <a href="prostredi.md#t691">StrDate</a>, <a href="#t696">AddWDays</a>, <a href="#t696">AddMonth</a>,
                                 <a href="#t694">TypeDay</a>, <a href="prostredi.md#t691">ValDate</a>, <a href="#t698">DifWDays</a>, <a href="#t698">DifMonth</a>

    <b>Přístup k větě souboru</b>       <a href="#t580">Link</a>, <a href="procedury.md#t572">IsDeleted</a>, <a href="#t285">Recno</a>, <a href="#t579">RecNoAbs</a>, <a href="#t579">RecNoLog</a>
                                 <a href="#t573">KeyOf</a>, <a href="procedury.md#t574">LinkRec</a>

    <b>Přímý přístup k údajům</b>       NázevSouboru[ČísloVěty].NázevÚdaje,
                                 <a href="prostredi.md#t638">Path</a>, <a href="prostredi.md#t638">Volume</a>, <a href="prostredi.md#t638">Generation</a>, <a href="#t268">Archives</a>

    <b>Podpora přístupových práv</b>    <a href="#t649">AccRight</a>, <a href="#t733">Password</a>, <a href="#t649">Trust</a>, <a href="#t649">UserName</a>, <a href="#t649">UserCode</a>

    <b>Vyhodnocení logické podmínky</b> <a href="#t440">Key In</a>, <a href="#t346">Exist</a>, <a href="#t279">Cond</a>, <a href="#t740">EvalB</a>, <a href="#t717">EquMask</a>, <a href="#t471">PromptYN</a>

    <b>Chybové stavy</b>                <a href="procedury.md#t626">ExitCode</a>,

    <b>Oper.systém,soubory,disk</b>     <a href="#t728">DiskFree</a>, <a href="#t728">FileSize</a>, <a href="#t734">GetEnv</a>, <a href="#t467">GetPath</a>,
                                 <a href="#t583">LastUpdate</a>, <a href="procedury.md#t463">MaxCol</a>, <a href="procedury.md#t463">MaxRow</a>, <a href="procedury.md#t504">MemAvail</a>
                                 <a href="#t291">NRecs</a>, <a href="#t583">NRecsAbs</a>

    <b>Speciální funkce a proměnné</b>  <a href="#t726">Modulo</a>, <a href="#t271">Sum</a>, <a href="#t733">Version</a>, <a href="#t508">IntTsr</a>, <a href="#t438">Owned</a>, <a href="#t733">TestMode</a>
                                 <a href="procedury.md#t498">PortIn</a>

    <a href="#t383">Speciální funkce v sestavě</a>

    ────────────────────────────────────────────────────────────────────────
</pre>

<a name="t268"></a>
## archives

<pre>
                        <b>ARCHIVES</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>NázevDleKatalogu.Archives</b> ..... přímý přístup k údajům katalogu v 
                                     proceduře (<a href="prostredi.md#t638"> archives</a>)
   ■ <b>spec. název úlohy v katalogu</b>... speciální jméno úlohy <b>archives</b> v
                                     katalogu označuje položku pro definici
                                     názvu archivace - viz <a href="prostredi.md#t616">zálohování</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t271"></a>
## sum

<pre>
                       <b>SUM</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>sum</b>(ČísVýraz) ... funkce typu <b>real</b>, kumulace číselného výrazu za skupinu 
                       vět
                   ... <a href="prostredi.md#t379">PopisnáČástÚrovně</a>: součtování v <b>sestavě</b>
                   ... <a href="formular-sestava.md#t396">PříkazováČástVýstupu</a>: součtování v <b>transformaci</b>
                   ... <a href="prostredi.md#t282">edit</a>: součtování v <b>automatických sestavách</b>

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t279"></a>
## cond

<pre>
                       <b>COND</b> - duplicitní klíčové slovo
  ──────────────────────────────────────────────────────────────────────────

   ■ <b>cond</b> ................. <a href="#t733">speciální funkce</a>: speciální výběrová funkce
   ■ edit(...<b>cond</b>...) ..... <a href="prostredi.md#t519">parametry editace</a>: editace vybrané podmnožiny
   ■ graph( ...<b>cond</b>...) ... grafické zobrazení dat, viz. <a href="rozhrani.md#t773"> cond</a>
   ■ report(...<b>cond</b>...) ... <a href="prostredi.md#t560">parametry sestavy</a>: sestava z vybrané podmnožiny

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t280"></a>
## in

<pre>
                       <b>IN</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>in</b> ............. <a href="#t664">speciální operátory</a>: test na seznam intervalů nebo 
                                           konstant
   ■ <a href="procedury.md#t441">forall</a> i <b>in</b> .... forall cyklus

   ■ <a href="#t440">key</a> <b>in</b> ......... čtení souborů podle indexů

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t285"></a>
## recno

<pre>
                      <b>RECNO</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ edit(...<b>recno</b>) .... <a href="editory.md#t529">procedurální parametry editace</a>: nastavení na větu
   ■ <b>recno(...)</b> ........ <a href="#t579">hledání podle klíče</a>, přímý přístup podle vlastního 
                         klíče
   ■ graph(...<b>recno</b>).... číslo první zobrazené věty v grafu, (<a href="rozhrani.md#t773"> recno</a> )

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t291"></a>
## nrecs

<pre>
                      <b>NRECS</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ NázevSouboru.<b>nrecs</b> ...... <a href="#t583">údaje o souboru</a> : počet platných v vět souboru
   ■ NázevPracIndexu.<b>nrecs</b> ... počet vět pracovního indexu, <a href="procedury.md#t414">index of</a>
   ■ graph(...<b>nrecs</b>...) ...... počet vět pro zobrazení v grafu, <a href="rozhrani.md#t773"> nrecs</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t299"></a>
## txtpos

<pre>
                     <b>TXTPOS</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ <b>txtpos</b> ....................... pozice kurzoru v rámci textu po ukončení
                                    editace - <a href="editory.md#t591">funkce textového editoru</a>
   ■ <a href="editory.md#t595">edittxt</a>(...,<b>TxtPos</b>=,...) ..... nastavení počáteční pozice kurzoru pro
                                    editaci - <a href="editory.md#t595">příkazy pro editaci textu</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t310"></a>
## funkce

*Též:* `D`, `function`

<pre>
                                   <b>FUNKCE</b>
   ─────────────────────────────────────────────────────────────────────────
   Kromě předdefinovaných funkcí (viz. <a href="#t264">standardní funkce</a>) je možno definovat
   vlastní, tzv.  <b>uživatelské funkce</b>.  Pro jejich  deklaraci slouží kapitola
   typu <b>D</b>, která nesmí mít název. Konstrukce funkcí je velice podobná proce-
   durám. Před použitím názvu funkce v úloze musí být tato funkce deklarována.
   Je povolena (přímá) rekurze.

   ██ Syntaxe:  [{ <b>function</b> NázevFunkce<b>(</b> [ DeklaraceParametrů ] <b>) :</b> Typ ;
                          [ <a href="formular-sestava.md#t391">DeklaraceLokálníchProměnných</a> ]
                            <b>begin</b> [{ Příkaz ; }] <b>end ;</b> }]

   ■ <b>DeklaraceParametrů</b> .. seznam parametrů  <b>Název : Typ ;</b>

   ■ <b>Povolené typy</b> ....... pro parametry, lokální proměnné a typ funkce
                           jsou povoleny "pouze" typy  real, string, boolean

   ■ <b>Název funkce</b> ........ je implicitně deklarován jako lokální proměnná.
                           Hodnota funkce se definuje tak, že v těle funkce
                           je této proměnné přiřazen požadovaný výsledek.

   ■ <b>Povolené příkazy</b> .... v těle funkce jsou povoleny příkazy if, while,
                           for, repeat, case, break, cancel, begin, end,
                           přiřazovací příkaz (:=,+=)
  
   ■ <b>Definiční obor</b> ...... Uživatelské funkce lze použít v každém výrazu,
                           jsou tedy přístupné i uživateli. Uživatelská
                           funkce se stejným názvem jako standardní funkce
                           tuto funkci znepřístupní. Podobně deklarace 
                           funkce v podúloze překryje stejnojmennou funkci 
                           z nadřízené úlohy.

   ■ <b>Omezení</b> ............. V jedné kapitole D je možno  deklarovat libovolný
                           počet funkcí (omezeno jen rozsahem volného textu).
                           Počet kapitol D není omezen. Celkový počet funkcí
                           je omezen jen velikostí paměti (všechny uživatel-
                           ské funkce jsou umístěny v paměti po celou dobu 
                           zpracování úlohy.
                           V těle funkce nejsou povoleny vedlejší efekty,
                           např. zápis do údajů souboru, práce s proměnnou
                           <b>record of soubor</b> ...

   ■ <b>Volání funkce</b> ....... Analogicky jako standardní funkce, uvedením
                           identifikátoru funkce ve výrazu. Při vyvolání
                           musí souhlasit počet a typ parametrů z deklarace 
                           funkce.


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>


   ██  funkce bez     function <b>DatumACas</b>() : real ;
       parametru               begin
                               DatumACas := today + currtime ;
                               end ;
                      { vrátí v jedné hodnotě aktuální datum a čas }


   ██  funkce s       function <b>MIN</b> ( prvni : real ; druhy : real ) : real ;
       parametry               begin
                               MIN := cond( prvni&lt;=druhy : prvni , 
                                                    else : druhy ) ;
                               end ;
                      { vrátí minimum ze dvou čísel }


   ██  rekurzivní     function <b>faktorial</b> (n:real) : real ;
       funkce                  var i : real ;
                               begin
                               if frac(n)&gt;0 then faktorial:=0
                               { faktorial pouze pro celá čísla }
                               else if n&gt;1 then faktorial:=n*faktorial(n-1)
                                           else faktorial:=1 ;
                               end ;
                      { faktorial n = 1 * 2 * ... * (n-1) * n ;
                      je klasický příklad použití rekurzivní funkce }

   ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t340"></a>
## parametrický klíč

*Též:* `globální proměnné`

<pre>
                             <b>PARAMETRICKÝ KLÍČ</b>
   ──────────────────────────────────────────────────────────────────────────
   Datový soubor s definovaným  parametrickým  klíčem je <b>parametrický soubor</b>.
   Jeho poslední věta je <b>viditelná</b> (tj. jeho údaje se mohou vyskytovat pomocí
   tečkové notace ve <b>výrazech</b> i na levé straně přiřazovacího příkazu) v celém
   projektu.

   █ <b>@ @</b> ....................... definice <b>parametrického klíče</b>

   Parametrický soubor může být dále v projektu zpracováván jako obyčejný
   datový soubor a může mít více vět, většinou má však pouze jednu větu. Když
   je parametrický soubor prázdný, PC FAND automaticky vytváří při prvním
   přístupu jednu větu a naplní ji nulami a mezerami.

   Údaje poslední věty parametrického souboru nazýváme <b>globální proměnné</b>. 
   Na globální proměnné se v celém projektu odkazujeme <b>tečkovou notací</b>
   a mohou vystupovat ve výrazech i na levé straně <b>přiřazovacího příkazu</b>.

   █ <b>NázevSouboru.NázevÚdaje</b> ... globální proměnná


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ Deklarace parametrického souboru PARAM

      F  PARAM    Datum : D ;
                  ....
                  #K <b>@ @</b> ;

      Odkaz na globální proměnnou PARAM.Datum kdekoliv v projektu
      PARAM.Datum:=today;

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t654">výrazy</a>         <a href="soubory.md#t336">klíče</a>
</pre>

<a name="t346"></a>
## cizí klíč

*Též:* `exist`, `role`, `název spojení`, `viditelný údaj`, `referenční integrita`

<pre>
                                 <b>CIZÍ KLÍČ</b>
   ───────────────────────────────────────────────────────────────────────────
   <b>Cizí klíč</b> definuje vazbu z podřízeného do nadřízeného souboru, resp.do sebe
   sama. V nadřízeném souboru musí existovat <b>vlastní klíč</b> , potom lze vybrat v 
   podřízeném  souboru odpovídající <b>klíčové údaje</b> (musí souhlasit jejich počet
   a typy) a definovat je jako <b>cizí klíč</b>. Tato vazba  definuje <b>viditelnou větu</b>
   nadřízeného souboru, což je věta se stejnými hodnotami vlastního klíče,jako 
   jsou hodnoty cizího klíče v podřízeném souboru. V kontextu věty podřízeného 
   souboru  lze používat  údaje  z viditelné  věty ve výrazech  pomocí <b>tečkové</b>
   <b>notace</b>. Většinou  jde o vazbu <b>1:N</b> a jedné nadřízené  větě odpovídá  skupina
   podřízených vět se stejným cizím klíčem.
   
   █ <b>NázevSouboru</b> SeznamÚdajů ... vazba do nadřízeného souboru podle <b>vlastního</b>
                                 <b> klíče</b>
   █ <b>NázevKlíče</b>   SeznamÚdajů ... vazba podle <b>alternativního vlastního klíče</b>
   █ <b>NázevRole(NázevSouboru)</b> SeznamÚdajů ... <b>role</b>: pojmenování cizího klíče
   █ <b>NázevRole(NázevKlíče)</b> SeznamÚdajů       při vícenásobném spojení souborů

   ■ prefix NázevSouboru, NázevKlíče nebo NázevRole nazveme <b>NázevSpojení</b>

   <b>Role</b> ............... Role se používá pro vytvoření vícenásobné vazby do
                        jednoho souboru podle jednoho klíče v nadříz. souboru.

   <b>Vazba typu</b> <a href="soubory.md#t437">owner</a> ... speciální typ vazby mezi soubory, který vytváří
                        podporu pro další konstrukce v PC FANDu. Většinou
                        jde o urychlení výběru podmnožiny v podříz. souboru.

   █ NázevSpojení <b>!</b> SeznamÚdajů ......... <b>Referenční integrita</b>
                        Datový editor hlídá referenční integritu,i rekurzivně.  
                        Vazba musí být typu OWNER. Při vyřazení nadřízené věty 
                        se vyřadí i všechny podřízené. Při změně klíče nadř.
                        věty se změní i odpovídající hodnoty podříz. souboru.
                        <b>Omezení:</b> všechny položky cizího klíče musí být uložené
                                 údaje.
                        Přepínač aditivních změn v datovém editoru vypne 
                        i referenční integritu.

   █ NázevSpojení <b>!!</b> SeznamÚdajů 
                        Varianta <b>referenční integrity</b>, která odmítne pokus o
                        zrušení věty v datovém editoru, pokud pro tuto větu
                        existují podřízené věty (podle definice vazby owner
                        obdobně jako předchozí případ).


   █ NázevSpojení.<b>exist</b> : boolean .......... funkce pro test viditelné věty

   Cizí klíč umožňuje <b>navigaci nahoru</b> (<b>F7</b> v podřízeném souboru). Pokud je
   podřízený soubor podporován indexy, a jeho vlastní klíč je rozšířením
   cizího klíče, funguje také <b>navigace dolů</b> (<b>Ctrl-F7</b> z nadřízeného souboru).

   █ NázevSpojení<b>.</b>NázevÚdaje ... tzv. <b>viditelný údaj</b>, použitelný ve výrazech
                                 pro vypočítané údaje, a dále v kapitolách <b>R</b> 
                                 a <b>M</b>. V proceduře pro record-proměnnou.
                                 Místo názvu údaje lze použít i funkci <a href="#t438">owned</a>

   ■ Odkaz na viditelný údaj může být víceúrovňový.
     NázevSpojení1.NázevSpojení2. ... NázevSpojeníX.NázevÚdaje


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ Deklarace nadřízeného souboru   
      F  MESTA.X    Mesto : A,30 ;
                    PSC   : N,5  ;
                    #K          @   PSC   ;                  { vlastní klíč }
                       DleNazvu(@) ~Mesto ;     { alternativní vlastní klíč }


      Deklarace podřízeného souboru
      F  ADRESY.X   Jméno    : A,25 ;
                    PSC      : N,5  ;
                    MatkaPSC : N,5  ;      { směrovací číslo bydliště matky }
                    MestoOtce: A,30 ;

                    #K              @  *PSC  ; { vlastní klíč s duplicitami }
                                MESTA <b>!</b> PSC  ;                  { <b>cizí klíč</b> }
                       NaMatku(MESTA)   MatkaPSC  ;         { definice <b>role</b> }
                       NaOtce(DleNazvu) MestoOtce ;         { definice <b>role</b> }

                    #C Mesto:=MESTA.Mesto : A,30 ;          { využití vazby }
                       MestoMatky:=NaMatku.Mesto : A,30 ;
                       PscOtce:=NaOtce.PSC : N,5 ;

      F  UCTY       JménoMajitele:A,25;
                    Kolik:F,6.2;
                    #K ADRESY JménoMajitele ;
                    #C Mesto:=ADRESY.MESTA.Mesto:A,30 ; {víceúrovňový odkaz}

      KOMENTÁŘ : Cizí klíč je vazba typu owner s referenční integritou.
                 Vazby NaOtce a NaMatku nejsou typu owner. 
                 Vazba NaMatku musí být formou role, neboť jde o druhou vazbu
                 přes primární klíč souboru MĚSTA.


   ─────────────────────────────────────────────────────────

   ██ Vazby do vlastního souboru / evidence občanů (rekurze)

      F LIDI.x   rc : N,6 ;
                 jmeno : A,20 ;
                 rc_matka : N,6 ;
                 rc_otec : N,6 ;
                 PocetDeti : F,2.0;

                 #K @ rc ;
                 AlterM (@) * rc_matka;
                 AlterO (@) * rc_otec ;
                 NaMatku(lidi) ! rc_matka ;   {<b>!!! referenční integrita</b>}
                 NaOtce(lidi) ! rc_otec ;     {<b>!!! referenční integrita</b>}

                 {<b> údaje z jiných vět toho samého souboru </b>}
                 #C JmenoMatky := cond( <b>NaMatku.exist</b>: NaMatku.Jmeno,
                                        else:'neuvedena') : A,20 ;
                    ...
                    JmenoBabiM  := cond( NaMatku.exist: <b>NaMatku.JmenoMatky</b>,
                    {rekurzivní odkaz}   else:'neuvedena') : A,20 ;

                 #A NaMatku.PocetDeti+= cond( IsNewRec: 1 );

   ──────────────────────────────────────────────────────────────────────────
   <a href="soubory.md#t338">vlastní klíč</a>     <a href="editory.md#t251">navigace</a>     <a href="soubory.md#t318">indexy</a>     <a href="soubory.md#t336">klíče</a>     <a href="soubory.md#t785">»F</a>
</pre>

<a name="t383"></a>
## speciální funkce v sestavě

*Též:* `page`, `notatend`, `notsolo`

<pre>
                         <b>SPECIÁLNÍ FUNKCE V SESTAVĚ</b>
 ──────────────────────────────────────────────────────────────────────────────
 V popisné části sestavy mohou kromě obvyklých konstrukcí vystupovat  <b>speciální</b>
 <b>funkce</b> (všechny jsou typu <b>real</b>):
 █ <b>page</b> ............ číslo právě vystupující tiskové strany
 █ <b>line</b> ............ číslo právě vystupujícího řádku na straně
 █ <b>pagelimit</b> ....... počet řádků (kromě <b>#PF</b>) na jedné tiskové straně sestavy

 █ <b>sum</b>(ČísVýraz) ... kumulace číselného výrazu za skupinu vět
                     jen v úrovních ukončujících skupinu vět (<b>#RF</b>, <b>#PF</b>, <b>#CF</b>)
 █ <a href="#t438">owned</a> ........... přístup k podřízeným souborům

 █ <b>group</b> :real ............  ─┐
 █ <b>Ii.count</b> :real  ........   │
 █ <b>Ii.error</b> :boolean ......   │
 █ <b>Ii.errortext</b> :string ...   ├ viz. <a href="soubory.md#t400">speciální funkce v transformaci</a>
 █ <b>Ii.warning</b> :boolean ....  ─┘

 Funkce <b>sum</b>, <b>group</b> a <b>count</b> jsou  obdobně  použitelné  také  ve výpočetní  části
 transformace (kapitola M).

 Před seznamem výrazů (ovšem za případnou výpočetní částí) popisné části se 
 mohou vyskytovat <b>speciální příkazy</b>:
 █ <b>.line:=ČísVýraz</b> ... před výstupem úrovně přejde na zadaný řádek
 █ <b>.page:=ČísVýraz</b> ... nastaví interní počítadlo stran (pro funkci <b>page</b>)
                       V úrovni RH nastaví přečíslování od první strany sestavy
 █ <b>.line&lt;=ČísVýraz</b> ... při překročení hranice (řádku) před výstupem odstránkuje
 █ <b>.notatend</b> ......... pouze úroveň PF, netiskne se na poslední stránce sestavy
 █ <b>.notsolo</b> .......... pouze pro úroveň DH, vystupuje pouze při nové stránce 
                       nebo výstupu CH, resp.CF. Zamezí se tak výstupu úrovně
                       DH při změně třídících údajů (CHi).
 ──────────────────────────────────────────────────────────────────────────────
 <a href="prostredi.md#t379">PopisnáČástÚrovně</a>     <a href="formular-sestava.md#t395">VýstupTransformace</a>     <a href="formular-sestava.md#t401">░ transformace</a>     <a href="formular-sestava.md#t788">»M</a>
</pre>

<a name="t438"></a>
## owned

<pre>
                                <b>FUNKCE OWNED</b>
   ───────────────────────────────────────────────────────────────────────────
   Speciální  funkce <b>owned</b> je definována jako prostředek pro přenos  informací
   z podřízeného  souboru do  nadřízeného.  Vazba mezi  soubory  musí být typu
   <a href="soubory.md#t437">owner</a>. Syntaxí zápisu a způsobem použití <b>owned</b> připomíná implicitně deklaro-
   vaný vypočítaný údaj číselného typu (<a href="soubory.md#t333">#C</a>).

  ██ <b>owned(</b>SpojeníDolů [<b>.</b>NázevÚdaje][<b>:</b>LogickýVýraz]<b>)</b> : real

   ■ <b>SpojeníDolů</b> ... NázevPodřízSouboru | NázevSpojení(NázevPodřízSouboru)

   ■ <b>NázevÚdaje</b> .... Musí být číselného typu(F,R,D). Pokud je uveden, vrátí 
                     součet daného údaje pro tyto věty. Jestliže chybí, vrátí 
                     počet podřízených vět (splňujících logickou podmínku).

   ■ <b>LogickýVýraz</b>... Je definován nad větami podřízeného souboru. 
                     Nelze v něm použít lokální proměnné.


   <b>Použití v kapitolách:</b>
   ────────────────────
   ■ <b>R</b>,<b>M</b> ... Použití v jednotlivých úrovních analogicky jako implicitní
             vypočtený údaj (#C). Pokud chybí specifikace <b>Ii</b> pak bere první
             z možných implicitních souborů. Při neúspěchu na rozdíl od
             názvu údaje nehledá v dalších souborech.

   ■ <b>P</b> ..... Nelze použít přímý odkaz Soubor[i].owned(..), v seznamech
             údajů (pro editaci,třídění).
             Jinak analogicky jako vypočtený údaj, např. položka record-
             proměnné nebo v podmínce <b>cond</b>.

   ■ <b>F</b>,<b>E</b>,<b>D</b>.. Nelze použít.

   ■ Funkce je přístupná i uživateli, např. <b>F6-Nová podmnožina</b> nebo 
     <b>CtrlF5-Kalkulačka</b> v datovém editoru.


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   F ZANRY.x   zanr:A,1;  nazev:A,10;
               #K @ ~zanr;

   F TITULY.x  zanr:A,1;  cislo:N,4;                  {identifikace titulu}
               KolikKazet:F,3.0;  VypujcniCena:F,3.2;
               ...
               #K @ ~zanr,cislo;
                  ZANRY zanr;          {vazba typu <b>owner</b>}

   R Rowned    #I_ZANRY
               #RH;
               žánr   název      počet      počet    počet titulů s více
                                 titulů     kazet    než 10 kazetami
               #DE zanr,nazev, <b>owned(tituly)</b>, <b>owned(tituly.KolikKazet)</b>,
                   <b>owned(tituly:KolikKazet&gt;10)</b> ;
               _    __________   ______   ________      ________
               #RF sum(<b>owned(tituly)</b>),sum(<b>owned(tituly.KolikKazet)</b>),
                   sum(<b>owned(tituly:KolikKazet&gt;10)</b>);
                    .............................................
                        Celkem:  ______   ________

   P Powned    var : recZ:record of ZANRY;
               begin
               edit(ZANRY,(),head='  jen skupiny s více než 100 tituly',
                    cond=(<b>owned(tituly)</b>&gt;100));
               ...
               writeln('Počty kazet dle žánrů');
               forall recZ/@ do writeln(recZ.zanr,'  ',<b>recZ.owned(tituly)</b>);
               wait;
               ...
               { nelze např. edit(ZANRY,(zanr,owned(tituly)),...);
                 nebo        getindex(ix,ZANRY, sort=( owned(tituly) )); }
               end;
   ───────────────────────────────────────────────────────────────────────────
</pre>

<a name="t440"></a>
## key in

*Též:* `key`

<pre>
                              <b>PODMÍNKA KEY IN</b>
   ──────────────────────────────────────────────────────────────────────────
   Často lze  dosáhnout  řádového  zrychlení tím,  že se čtení vět ze souboru
   podle indexu omezuje na vybrané hodnoty klíče resp. intervaly klíče. Místo 
   obvyklého logického výrazu pro vstupní podmínku lze použít:

   ██ <b>key in</b> [SeznamIntervalůNeboHodnot] ... výběr podle indexu
   ■ prvky seznamu: <b>Hodnota</b> nebo <b>DolníHodnota..HorníHodnota</b> (interval hodnot)

   Slovo key se vztahuje na klíč, podle kterého se soubor  čte  (při fyzickém
   pořadí není key in povolen).  Hodnoty  v seznamu  mohou být konstanty nebo
   výrazy. Má-li klíč více údajů, píše se seznam výrazů v závorkách-lze zadat
   i jen prvních n údajů (pro zbytek údajů chceme všechny hodnoty ). Podmínku
   key in lze kombinovat s běžnou vstupní podmínkou:
 
   ■ <b>key in</b> [...] &amp; LogickýVyraz ... filtr pro věty vybrané podmínkou key in
   ■ key in nelze kombinovat s podmínkou <a href="soubory.md#t437">owner</a>

   <b>Použití podmínky key in v projektu</b>
   ■ vstupní podmínka sestavy: <a href="formular-sestava.md#t369">VstupSestavy</a> - kapitola <b>R</b> nebo příkaz <a href="formular-sestava.md#t556">report</a>
   ■ vstupní podmínka transformace: <a href="formular-sestava.md#t392">VstupTransformace</a> - kapitola <b>M</b>
   ■ vstupní podmínka příkazů procedury <a href="prostredi.md#t282">edit</a>, <a href="formular-sestava.md#t556">report</a>, <a href="rozhrani.md#t753">graph</a>, <a href="procedury.md#t441">forall</a>, <a href="procedury.md#t415">getindex</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   key in ['200'..'499','623','900'..'999']       {seznam intervalů a hodnot}
   key in [1.9.39..8.5.45]
   key in ['Praha','Brno','Bratislava']
   key in [PARAM.Od..PARAM.Do]
   key in [i,j,k..l]
   key in [(1.1.92,5000)..(31.12.92,10000)]
   key in [('010','000','00')..('020','999','99')]

   edit(DATA,(),cond=(key in [...]));                    {použití v projektu}
   edit(DATA,(),cond=(key in [...] &amp; Středisko='005'));
   report(DATA/@,(),cond=(key in [...]));
   forall Věta/Klíč (key in [...]) do ...;

   #I_DATA/@ (key in [...])                                   {vstup sestavy}

   #I2_DATA/Klíč (key in [...] &amp; Datum&gt;0)                {vstup transformace}
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t467"></a>
## getpath

<pre>
                              <b>DOTAZ UŽIVATELI</b>
  ──────────────────────────────────────────────────────────────────────────
   Interaktivní výběr cesty k souboru v systému adresářů.

   ██ syntaxe:     <b>GETPATH</b> [ <b>(</b> Maska <b>)</b> ] : string

   ■  <b>Maska</b> ...... Textový výraz. Zadání vhodné masky umožňuje :
                   -   zúžit výběr na soubory s určitou příponou
                   -   zvolit vhodný adresář pro začátek hledání
                   -   nastavení požadovaného disku
                   V masce je možno použít speciální znaky :
                   <b>?</b>   nahradí se libovolným znakem - jen pro danou pozici
                   <b>*</b>   nahradí se libovolnou kombinací znaků (řetězcem)
                   Implicitní maska je '*.*' ( tj. všechny soubory)
   ■  Vrací ...... Funkce vrací <b>úplnou</b> cestu k potvrzenému souboru (ENTER)
                   nebo prázdný string při <b>ESC</b>.
   ■  Kapitoly ... P, D

   <a href="procedury.md#t444">komunikační příkazy</a>

   <b>░░░░░░░░░░░░</b>      ██    s:=getpath('*.TXT');
   <b>░░příklady░░</b>            TEXT.Path:=s;
   <b>░░░░░░░░░░░░</b>            printtxt(TEXT);  {tisk vybraného souboru}
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t471"></a>
## dotaz uživateli

*Též:* `promptyn`, `prompt`, `escprompt`

<pre>
                              <b>DOTAZ UŽIVATELI</b>
   ──────────────────────────────────────────────────────────────────────────
   Zadání hodnoty z klávesnice.

   ██ syntaxe:     <b>PROMPT (</b> Dotaz <b>:</b> Typ1 [ <b>:=</b> Implicitně ] <b>)</b> :  Typ2

   ■  <b>Dotaz</b> ...... Textový výraz, který se vypíše na aktuální pozici obrazovky.
   ■  <b>Typ1</b> ....... Použije se jeden z typů kapitoly F (<b>F</b>,<b>A</b>,<b>N</b>,<b>B</b>,<b>D</b>). Komunikace
                   při editaci potom odpovídá editaci jednoho údaje tohoto 
                   typu datovým editorem. Tomuto typu odpovídá i typ vrácené 
                   hodnoty <b>Typ2</b>:   <b>string</b>  pro typ1=A,N
                                   <b>real</b>             F,D
                                   <b>boolean</b>          B
   ■  <b>Implicitně</b>.. Nepovinné zadání implicitní hodnoty odpovídajícího typu.
   ■  Kapitoly ... P,D
   ───────────────────

   Test na ukončení posledního PROMPTu klávesou <b>Esc</b> ?
   ██ syntaxe:     <b>ESCPROMPT</b> : boolean ... 

   Dotaz typu A/N na 25. řádku obrazovky.
   ██ syntaxe:     <b>PROMPTYN (</b> Dotaz <b>)</b> : boolean

   <a href="procedury.md#t444">komunikační příkazy</a>      <a href="editory.md#t327">typy údajů</a>     <a href="#t658">typy výrazů</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  BEGIN
       ....
       r:=prompt('Zadej datum a čas':D,'DD.MM.YY hh:mm':=today+currtime);
       if escprompt then 
          if prompt('Ukončit program ? (A/N) ':B) then cancel
                                                  else ...
       ...
       END ;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t491"></a>
## ismouse

<pre>
                              <b>MYŠ V PROCEDUŘE</b>
   ──────────────────────────────────────────────────────────────────────────

   Větvení programu podle druhu události. Funkce zpracuje aktuální událost,
   která byla načtena funkcí mouseevent.

   ██ syntaxe:      <b>ISMOUSE( </b>MaskaUdálostí <b>, </b>MaskaKlávesy <b>)</b> : boolean

   ■  <b>MaskaUdálostí</b> Celé číslo, konstanta, pro test druhu události.
                           <b>1</b>     stisk  klávesy
                           <b>2</b>     pustit klávesu
                           <b>4</b>     přesun myši
   ■  <b>MaskaKlávesy</b>  Celé číslo, konstanta, maska  pro klávesu ( sčítáme čísla
                    podmínek, které musí být splněny)
                           <b>1</b>     levá klávesa
                           <b>2</b>     pravá klávesa
                         <b>256</b>     dvojité stisknutí
                    Pro událost <b>2</b> (pustit klávesu) lze použít jen masku
                    klávesy <b>0</b>. Tedy IsMouse(2,0).
   ■  Kapitoly .... P,D

   ──────────────────────────────────────────────────────────────────────────
   <a href="rozhrani.md#t490">myš v proceduře</a>    <a href="procedury.md#t495">setmouse</a>     <a href="rozhrani.md#t490">mouseevent</a>    <a href="procedury.md#t495">mousex</a>     <a href="procedury.md#t495">mousey</a>    <a href="procedury.md#t495">mousein</a>
</pre>

<a name="t508"></a>
## komunikace s rezidentním programem

*Též:* `inttsr`

<pre>
                     <b>KOMUNIKACE S REZIDENTNÍM PROGRAMEM</b>
    ─────────────────────────────────────────────────────────────────────────
    Komunikaci s <b>rezidentními programy</b> zajišťuje speciální funkce <b>inttsr</b> typu
    <b>real</b>, která vyvolá přerušení s číslem prvního argumentu, hodnotou druhého
    argumentu  v registru  <b>AX</b>  a adresou lokální  proměnné v <b>DS:DX</b>.  Hodnotou
    funkce je hodnota registru <b>AX</b> po návratu z přerušení.

    ██ syntaxe:   <b>INTTSR( </b>ČísloInt <b>,</b>RegAX <b>,</b>RecordProměnná | Výraz <b>)</b> : real

    ■  <b>ČísloInt</b> ......... číslo přerušení (Interrupt)
    ■  <b>RegAX</b> ............ hodnota registru AX
    ■  <b>RecordProměnná</b> ... předá se adresa pro čtení a zápis dat ve formátu 
                          PC FANDu
    ■  <b>Výraz</b> ............ Výraz typu  string, real nebo  boolean. FAND předá
                          pointer na  údaj ( ve formátu  PASCALu ), který je
                          hodnotou výrazu. Parametr je volán odkazem, což 
                          znamená, že po volaný interrupt může aktualizovat
                          hodnotu parametru.

    Rezidentní program musí zpracovat dané přerušení, pomocí něhož zjistí kód
    požadované  činnosti a adresy, kam má psát ( nebo kde má převzít ) data z
    úlohy.
    ─────────────────────────────────────────────────────────────────────────
    <a href="formular-sestava.md#t407">record proměnná</a>
</pre>

<a name="t550"></a>
## funkce datového editoru

*Též:* `edrecno`, `edreckey`, `edirec`, `edfield`, `clipbd`, `isnewrec`, `edbool`, `edupdated`, `edbreak`, `edfile`, `edkey`

<pre>
                          <b>FUNKCE DATOVÉHO EDITORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Po ukončení  nebo přerušení  editace dat  se nastavují hodnoty ( přístupné
   přes následující funkce), které udávají stav při opuštění editoru.

   ██ <b>CLIPBD</b> : string ... Interní proměnná, která programátorsky zpřístupní
                          obsah "clipboardu". Může býti rovněž na levé straně
                          přiřazovacího příkazu <b>clipbd:=TextVýraz</b>.
                          Dále viz. textový editor - <a href="editory.md#t234">bloky</a>.

   ██ <b>EDBOOL</b> : string ... Vrátí text výrazu pro výběr podmnožiny v datovém
                          editoru. Musí se použít v exit-proceduře a vztahuje
                          se na právě přerušenou editaci. Pokud takový výběr
                          není, vrátí ''.

   ██ <b>EDBREAK</b>  : real ...... <b>Návratový kód</b> editoru podle způsobu ukončení
                             editace:
      0     ... běžné ukončení editace klávesou &lt;Esc&gt;
      11    ... přerušení při překročení času - <a href="prostredi.md#t519">watch</a> (<a href="prostredi.md#t829">lokální sítě</a>)
      12    ... přerušení na údaji klávesou &lt;Enter&gt;, &lt;F4&gt; (<a href="procedury.md#t288">exit</a>)
      13    ... opuštění editoru při prázdné množině vět a módu '^n'
      14    ... návrat z vložené editace klávesou &lt;CtrlF4&gt; s přenosem hodnoty
      15    ... ukončení klávesou &lt;Esc&gt; za stavu "soubor je blokován" (sítě)
      16,17 ... přerušení typu <b>record</b> před zápisem, změnou (16)
                nebo mazáním věty (17) (<a href="procedury.md#t288">exit</a>)
      18    ... vstup do pořízení nové věty (viz. <a href="editory.md#t536">newrec</a>)

      Přerušení <b>funkčními klávesami</b> při definovaném <a href="procedury.md#t288">exit</a>:
       1 až 10 ... ShiftF1 až ShiftF10
      21 až 30 ... F1 až F10
      31 až 40 ... CtrlF1 až CtrlF10
      41 až 50 ... AltF1 až AltF10

      Přerušení <b>kurzorovými klávesami</b> při definovaném <a href="procedury.md#t288">exit</a>:
      51 ... Home     57 ... Right     71 ... CtrlLeft       77 ... Tab
      52 ... Up       59 ... End       72 ... CtrlRight      78 ... ShiftTab
      53 ... PgUp     60 ... Down      73 ... CtrlEnd        79 ... CtrlN
      55 ... Left     61 ... PgDn      74 ... CtrlPgDn       80 ... CtrlY
                      62 ... INS       75 ... CtrlHome       81 ... Esc
                                       76 ... CtrlPgUp       82 ... CtrlP


   ██ <b>EDFIELD</b>  : string .... <b>Název údaje</b> pod kurzorem při opuštění editoru.
                             Souvislosti: parametr editace <a href="editory.md#t529">field</a>

   ██ <b>EDFILE</b> : string ...... <b>Název editovaného souboru</b>. Má význam pro psaní
                             obecné <a href="editory.md#t532">exit-procedury</a>. Mimo datový editor má
                             význam jen v <b>ladícím režimu</b>, kde vrací název 
                             zpracovávané úlohy.

   ██ <b>EDIREC</b>   : real ...... <b>Pořadové číslo</b> aktuální věty na obrazovce při
                             opuštění editoru. 
                             Souvislosti: parametr editace <a href="editory.md#t529">irec</a>.

   ██ <b>EDKEY</b> : string ....... <b>Název klíče při editaci</b>. Při editaci podle
                             pracovního indexu nebo při editaci souboru bez
                             indexů vrací <b>''</b> (prázdný string). Tato funkce
                             je korektně definována v exit-proceduře, tj.
                             v průběhu editace.

   ██ <b>EDRECKEY</b> : string .... <b>Interní hodnota klíče</b> poslední editované věty.
                             Vrací hodnotu před případnou změnou klíče věty.
                             Souvislosti: parametr editace <a href="editory.md#t529">reckey</a>,
                                          funkce <a href="#t573">keyof</a>, <a href="sql.md#t885">SQL</a>-server.
                             Speciální použití - text chyby při kontrole 
                             syntaxe dynamické deklarace, viz. <a href="soubory.md#t554">check</a>

   ██ <b>EDRECNO</b>  : real ...... <b>Fyzické číslo věty</b> při opuštění editoru.
                             Při opuštění v módu pořízení vrací 0 !
                             Souvislosti: parametr editace a funkce <a href="#t285">recno</a>

   ██ <b>EDUPDATED</b>: boolean ... Test provedení změn v souboru během editace
                             udává, zda byl <b>soubor</b> (v exit procedurách <b>věta</b>)
                             změněn. Je funkční pro <b>datový</b> i <b>textový</b> editor.
                             Změnou může být i změna třídění souboru (F6).

   ██ <b>ISNEWREC</b> : boolean ... Vrací TRUE pokud je právě editovaná věta nově
                             pořízenou. Má smysl jen v datovém editoru - tedy
                             v kapitole F nebo při volání <a href="editory.md#t532">exit-procedury</a>.



   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ Pomocí funkcí <b>edrecno</b>,<b>edirec</b>,<b>edfield</b> a <b>edreckey</b> spolu s odpovídajícími
      parametry editace <b>recno</b>, <b>irec</b>, <b>field</b> a <b>reckey</b> lze po opuštění datového
      editoru  zajistit návrat na přesně  stejné místo. Tj. návrat na stejný
      údaj stejné věty včetně pozice v okně.

   ██ Funkce <b>edbreak</b> se nejčastěji použije pro rozhodnutí o následné akci 
      po ukončení nebo přerušení datového editoru.

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t573"></a>
## keyof

<pre>
                                   <b>KEYOF</b>
   ───────────────────────────────────────────────────────────────────────────
   Funkce procedury, která  vrací  interní  string klíče  zadané věty souboru.
   Jde o jednu z funkcí , které podporují  práci  s větami souborů. Lze použít
   dva formáty.

   ██ Syntaxe:     <b>KEYOF (</b> RecordProměnná [ <b>/</b>NázevKlíče ] <b>)</b>           : string
                   <b>KEYOF (</b> Soubor[<b>/</b>NázevKlíče] <b>,</b>SeznamArgumentůKlíče<b>)</b> : string

   ■  <b>RecordProměnná</b> ......... proměnná typu record of <b>soubor</b>
   ■  <b>NázevKlíče</b> ............. vlastní (alternativní) klíč <b>souboru</b>
   ■  <b>SeznamArgumentůKlíče</b> ... odpovídá co do počtu a typu položek deklaraci
                               klíče. Podobně jako <a href="#t579">  recno</a>.

   ■  <b>Souvislosti</b> ............ funkce            <a href="#t550">edreckey</a>,
                               příkazy           <a href="procedury.md#t403">readrec</a>, <a href="procedury.md#t404">writerec</a>, <a href="procedury.md#t572">deleterec</a>,
                               parametr editace  <a href="editory.md#t529">reckey</a>
                               <a href="sql.md#t885">SQL</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ F  PSC.x    *  psc : N,5 ; #K @ psc ;                     { seznam psc }
      P  Ukazka1  *  VAR   rec : record of PSC ;
                             s : string ;
                     BEGIN rec.psc := prompt(' zadej psč: ':N,5) ;
                           s:=<b>keyof</b>(rec) ;                   { hodnota klice }
                           readrec( rec,s) ;              { pokus o precteni }
                           if <a href="procedury.md#t572">IsDeleted</a>(rec)                { vysledek cteni }
                              then message('psč není v seznamu')
                              else message('psč je v seznamu') ;
                           ...
                     END ;
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t579">hledání podle klíče</a>
</pre>

<a name="t579"></a>
## hledání podle klíče

*Též:* `  recno`, `recnoabs`, `recnolog`

<pre>
                            <b>HLEDÁNÍ PODLE KLÍČE</b>
   ───────────────────────────────────────────────────────────────────────────
   Funkce <b>recno</b> vrací fyzické  <a href="procedury.md#t575">číslo věty</a>  identifikované  určitým klíčem. Lze
   použít pro  indexové soubory nebo  neindexové s vlastním klíčem.  Ve druhém
   případě dává funkce  korektní výsledek  jen tehdy , je-li soubor dle tohoto
   klíče <b>fyzicky</b> setříděn. Pokud věta s touto hodnotou klíče neexistuje, vrací 
   záporné číslo nejbližší vyšší věty.

   ██ Syntaxe: <b>RECNO(</b> NázevSouboru [<b>/</b>NázevKlíče] <b>,</b>SeznamArgumentůKlíče<b>)</b> : real

   ■  <b>NázevSouboru</b> ........... Název kapitoly F.
   ■  <b>NázevKlíče</b> ............. Název vlastního (alternativního) <a href="soubory.md#t336">klíče</a> souboru.
                               Implicitně se použije hlavní <a href="soubory.md#t338">vlastní klíč</a> (<b>@</b>)
   ■  <b>SeznamArgumentůKlíče</b> ... Počet prvků seznamu musí přesně co do pořadí,
                               počtu a typů odpovídat deklaraci klíče.
                               Jednotlivé prvky seznamu jsou číselné výrazy
                               odpovídajícího typu, oddělené čárkou.
   ───────────────

   ██ Syntaxe:   <b>RECNOABS (</b> NázevSouboru [<b>/</b>NázevKlíče] <b>,</b>LogickéČíslo<b>)</b> : real
                 <b>RECNOLOG (</b> NázevSouboru [<b>/</b>NázevKlíče] <b>,</b>FyzickéČíslo<b>)</b> : real

   Funkce <b>recnoabs</b> převádí logické pořadové číslo věty podle daného klíče
   na fyzické číslo věty. Funkce <b>recnolog</b> provádí právě opačný převod. Pokud
   věta s fyzickým číslem neexistuje nebo je neplatná, vrací <b>0</b>.

   ■ <b>LogickéČíslo</b> a <b>FyzickéČíslo</b> jsou číselné výrazy.

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   i:=recno(SKLAD,Cislo)               {bez indexů nebo podle primárního klíče}
   i:=recno(SKLAD,Cislo,Datum)                        {několik klíčových údajů}
   i:=recno(SKLAD,POM.Zbozi,int(today))                         {obecné výrazy}
   i:=recno(SKLAD/Klic2,Cislo)                       {podle sekundárního klíče}


   i:=recnoabs(DATA/Klic, 1)                           {první věta podle klíče}
   i:=recnoabs(DATA/Klic,DATA.nrecs)                {poslední věta podle klíče}

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t789">»P</a>
</pre>

<a name="t580"></a>
## link

<pre>
                                    <b>LINK</b>
   ──────────────────────────────────────────────────────────────────────────
   Funkce  <b>link</b>  provede  hledání podle  <b>cizího klíče</b>  a vrátí  <b>fyzické číslo</b>
   nadřízené věty.  Tj. podle definice  cizího  klíče vyhledá v odpovídajícím
   nadřízeném souboru příslušnou (nadřízenou ) větu a vrátí její číslo. Pokud
   taková věta není nalezena , vrací záporné číslo nejbližší vyšší věty ( dle
   daného klíče). Funkce má dvě varianty dle typu identifikace podřízené věty.


   ██ Syntaxe :      <b>LINK (</b> NázevSouboru<b>[</b>FyzČísloVěty<b>] ,</b>NázevSpojení <b>)</b> : real
                     <b>LINK (</b> RecordProměnná <b>,</b> NázevSpojení <b>)</b>            : real

   ■  <b>NázevSouboru</b> ... Název kapitoly F, identifikátor.
   ■  <b>FyzČísloVěty</b> ... Číselný výraz. Číslo věty podřízeného souboru, k níž se
                       hledá viditelná věta
   ■  <b>NázevSpojení</b> ... <b>NázevSouboru</b>, <b>NázevRole</b> nebo <b>NázevKlíče</b> (<a href="#t346">Cizí klíč</a>)


   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>    i:=link(POD[10],NAD);                   {podle čísla věty}
   <b>░░░░░░░░░░░░</b>    i:=link(Věta,NAD);          {podle obsahu record proměnné}

   ──────────────────────────────────────────────────────────────────────────
   <a href="procedury.md#t574">LinkRec</a>      <a href="procedury.md#t575">Číslo věty</a>    <a href="#t579">hledání podle klíče</a>
</pre>

<a name="t583"></a>
## údaje o souboru

*Též:* `nrecsabs`, `lastupdate`

<pre>
                              <b>ÚDAJE O SOUBORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Parametry každého datového souboru lze zjistit v následujících interních
   proměnných a funkcích : <b>nrecs</b>, <b>nrecsabs</b>, <b>lastupdate</b>. 
   Fyzickou délku souboru v bytech lze zjistit pomocí funkce <a href="#t728">filesize</a>.

   ██ <b>LASTUPDATE</b> .............Datum a čas poslední aktualizace souboru
                              (ve formátu typu D).
      Syntaxe:    NázevSouboru | NázevDleKatalogu  .<b>lastupdate</b> : real

      Poznámka:   Před lastupdate je vhodné dát <b>close</b>[(Soubor)], neboť
                  aktualizace časového údaje se provádí při uzavření souboru.
   ───────────

   ██ NázevSouboru.<b>NRECS</b> :real ...... počet platných vět v souboru
   ██ NázevSouboru.<b>NRECSABS</b> :real ... počet fyzických vět (včetně neplatných)

      Neplatné věty mohou být pouze v <b>indexovém</b> nebo <b>.DBF</b> souboru, proto pro
      ostatní (neindexové) soubory platí <b>nrecs=nrecsabs</b>. Konstrukce <b>nrecs</b> může
      stát na levé  straně přiřazovacího příkazu:

   ■  <b>NázevSouboru</b> ... název kapitoly F
   ■  Interní proměnná NázevSouboru.<b>nrecs</b> může být i na levé straně přiřazova-
      cího příkazu. Tím lze docílit změnu počtu vět souboru na zadanou hodnotu.
      Tj. zkrácení, zrušení nebo prodloužení souboru.
      <b>NázevSouboru.nrecs:=ČísVýraz</b>
      Pro indexové soubory je přípustné pouze NázevSouboru.nrecs:=0. (zrušení)
      Toto je povoleno i pro <a href="procedury.md#t414">pracovní index</a>.

   ■  Fyzická interpretace příkazu <b>soubor.NRECS:=0</b> je poněkud odlišná pro
      jednotlivé typy datových souborů:
      - běžný <b>lokální</b> datový soubor bude zrušen
      - <b>sdílený</b> datový soubor bude míti délku 6 byte - tj prefix.
      - soubor <b>.DBF</b> - zůstane hlavička (délka dle deklarace).
      - <a href="procedury.md#t414">pracovní index</a> bude zrušen.
      

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ Výpis informací o souboru.
      BEGIN
      writeln('Informace o souboru DATA');
      writeln('... počet platných vět: ',DATA.nrecs:6);
      writeln('... počet neplatných vět: ',DATA.nrecsabs-DATA.nrecs:6);
      writeln('... počet fyzických vět: ',DATA.nrecsabs:6);
      writeln('... poslední aktualizace: ',DATA.lastupdate:'DD.MM.YY hh:mm');
      writeln('... fyzické jméno: ',DATA.path);  {pouze pro soubory v katalogu}
      POMOCNY.Path:=DATA.Path; close(DATA);      {POMOCNY je katalogizovaný
                                                  soubor bez kapitoly F     }
      writeln('... délka v bytech:',filesize(POMOCNY));
      writeln('... návěští: ',DATA.volume);
      END;

   ██ Zjištění času aktualizace libovolného souboru.
      V katalogu úlohy musí být založena věta s názvem souboru např. 'PRACOV', 
      který nekoresponduje na žádnou kapitolu typu F.
      BEGIN
      PRACOV.Path:=prompt(' zadej cestu k souboru :':A,79) ;
      write('poslední aktualizace: ', PRACOV.lastupdate:'DD.MM.YY hh:mm');
      END ;

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t641">katalog</a>     <a href="soubory.md#t318">indexy</a>
</pre>

<a name="t649"></a>
## práva přístupu k datům

*Též:* `username`, `usercode`, `accright`, `trust`

<pre>
                           <b>PRÁVA PŘÍSTUPU K DATŮM</b>
   ──────────────────────────────────────────────────────────────────────────
   Následujícími funkcemi  se lze  dotázat na  <b>jméno</b>, <b>kód</b> a <b>přístupová  práva</b>
   aktuálního uživatele podle kapitoly <b>U</b> (závisí na <b>heslu</b>, kterým se uživatel
   přihlásil) a  podle nich  měnit chování  úlohy pro různé uživatele.  Během
   zpracování lze  všechny hodnoty  měnit přiřazovacím příkazem bez  nutnosti
   nového nastartování úlohy a přihlášení se pod novým heslem.

   █ <b>USERNAME</b>: string ... interní proměnná, <b>jméno</b> aktuálního uživatele,
                          (lze i přiřadit username:=TextVýraz)

   █ <b>USERCODE</b>: real ..... interní proměnná, <b>kód</b> aktuálního uživatele
                          (lze i přiřadit usercode:=ČísVýraz)
                          POZOR: při usercode:=číslo se naplní stejnou
                                 hodnotou i accright (accright:='\číslo')
   █ <b>ACCRIGHT</b>: string ... interní proměnná, <b>přístupová práva</b> kódovaná do textu
                          Seznam čísel je kódován jako ordinální hodnoty
                          jednotlivých bytů stringu, maxim. délka je 255 znaků
                          (lze i přiřadit accright:=TextVýraz)

   █ <b>TRUST</b>(SeznamČísel): boolean ... test práva přístupu
                          funkce vrátí true, když aktuální seznam přístupových 
                          práv a množina čísel v parametru nejsou disjunktní.
                          V parametru <b>SeznamČísel</b> lze použít i interval.

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██ Použitím kapitoly U se inicializuje implicitní mechanismus přístupových
      práv, který lze ovlivnit výše uvedenými funkcemi a proměnnými. Kapitolu
      U je možné i úplně vypustit a přesto pomocí těchto funkcí používat
      přístupová práva.

      Kapitolu U s obsahem :
      'Jana' , (2) , 'obsluha' , ( 1, 5, 20, 100, 221 ) ;

      lze rovnocenně nahradit příkazy procedury (spustíme při startu úlohy):

      if <b>password</b> = 'obsluha' then begin
                                   <b>usercode</b> := 2 ;
                                   <b>username</b> := 'Jana' ;
                                   <b>accright</b> := '\001\005\020\100\221' ;
                                   end ;

      Poznámka: Pokud umístíme údaje z kapitoly U např. do datového souboru,
                přináší nám to zhruba tyto výhody:
                ■ změna uživatele za běhu úlohy
                ■ privilegovaný uživatel má z úlohy přístup k údajům
                  o ostatních uživatelích ( různé statistiky jejich práce)
                ■ možnost práce se seznamem uživatelů "zevnitř" úlohy

   ──────────────────────────────────────────────────────────────────────────
   <a href="dalsi-kapitoly.md#t644">seznam uživatelů</a>      <a href="#t733">heslo</a>     <a href="soubory.md#t350">#U</a>     <a href="prostredi.md#t270">U</a>
</pre>

<a name="t652"></a>
## identifikátor

<pre>
                               <b>IDENTIFIKÁTOR</b>
   ──────────────────────────────────────────────────────────────────────────
   Identifikátory slouží k označení  jednotlivých konstrukcí v PC FANDu. Může
   jít jak o předdefinované  konstrukce ( tzv. klíčová slova - názvy příkazů,
   operací, funkcí ... ), tak i o konstrukce, vytvořené programátorem ( názvy
   údajů souborů , proměnných, procedur, sestav , transformací, uživatelských
   funkcí, ...).

   <b>Pravidla pro tvorbu identifikátorů :</b>

   ■  Identifikátory jsou v PC FANDu tvořeny písmeny, číslicemi a znakem 
      '_' (podtržítko).

   ■  První znak musí být písmeno.

   ■  Malá a velká písmena se nerozlišují.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t654">výrazy</a>     <a href="#t660">operátory</a>       <a href="#t659">konstanty</a>
</pre>

<a name="t654"></a>
## výrazy

*Též:* `výraz`

<pre>
                                   <b>VÝRAZY</b>
 ──────────────────────────────────────────────────────────────────────────────
 <b>Výrazy</b> jsou naznačené  výpočty  a  objevují  se  na  mnoha  místech  projektu.
 Vytvářejí se obvyklým způsobem z konstant, proměnných, operátorů a funkcí.

 ■ <a href="#t658">typy výrazů</a> ......... číselný - <b>real</b>, textový - <b>string</b> a logický - <b>boolean</b>
 ■ <a href="#t659">konstanty</a> ........... všech typů
 ■ <a href="formular-sestava.md#t391">lokální proměnné</a> .... definované v proceduře nebo transformaci
 ■ <a href="#t340">globální proměnné</a> ... údaje parametrického souboru přístupné v celé úloze
 ■ <a href="#t660">operátory</a> ........... číselné, textové, logické, srovnávací

 ■ <a href="#t682">aritmetické funkce</a> ....... reálné, goniometrické, logaritmické
 ■ <a href="#t692">funkce pro datum a čas</a> ... systémové datum a čas, převody mezi datumy a texty
                              pracovní dny, kalendářní výjimky
 ■ <a href="#t724">zpracování textu</a> ......... vyhledávání, převod mezi texty a čísly,
                              podřetězce, textové soubory a volné texty
 ■ <a href="#t733">speciální funkce</a> ......... verze, heslo, místo na disku, <b>cond</b>

 Jednotlivé kapitoly mají ještě některé speciální konstrukce, které lze  použít
 ve výrazech pouze uvnitř této kapitoly. Všude  je  použitelná  tečková  notace
 <b>NázSouboru[ČísVýraz].NázÚdaje</b> (<a href="soubory.md#t336">klíče</a>).
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t669">░ výrazy</a>
</pre>

<a name="t658"></a>
## typy výrazů

*Též:* `real`, `string`, `boolean`

<pre>
                                <b>TYPY VÝRAZŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   (vpravo kompatibilní <b>typy údajů</b> z deklarace souboru)

   ■ <b>real</b> ...... <b>číselný</b> - (pascalský 6-bytový real)-11 platných míst   <b>F D R</b>
                           ve verzi pro koprocesor 15 platných míst
   ■ <b>string</b> .... <b>textový</b> - až 65 000 B (jako volný text)                <b>A N T</b>
                           Od verze 4.1 v některých případech pracuje
                           i s texty až 2GB. A to sice v případě přiřazení
                           mezi proměnnými typu string a údaji souborů
                           typu T (volný text). Nesmí jít o obecný
                           textový výraz.

   ■ <b>boolean</b> ... <b>logický</b> - true/false (Ano/Ne)                          <b>B</b>

   Typy real, string, boolean  jsou tzv.  <b>jednoduché typy</b>  a odpovídají typům
   v deklaraci  <b>lokálních proměnných</b>  a <b>parametrů procedur</b>, kde lze ale navíc
   použít i tzv. <b>strukturované typy</b>. 

   <b>Strukturované typy:</b>
   ■ <a href="procedury.md#t405">record</a> of Soubor      ─┐
   ■ <a href="procedury.md#t414">index</a> of Soubor        ├─  typ lokální proměnné nebo parametru procedury
   ■ <a href="soubory.md#t417">file</a>                  ─┘
   ──────────────────────────────────────────────────────────────────────────
   <a href="editory.md#t327">typy údajů</a>     <a href="#t669">░ výrazy</a>
</pre>

<a name="t659"></a>
## konstanty

<pre>
                                 <b>KONSTANTY</b>
   ──────────────────────────────────────────────────────────────────────────
   ■ <b>číselná konstanta</b> ... celé číslo (příp. se znaménkem)
                       ... číslo s pevnou řádovou čárkou
                       ... exponenciální tvar čísla
                       ... datum ve tvaru <b>DD.MM.YY</b> nebo <b>DD.MM.YYYY</b>
                       ... čas ve tvaru <b>hh:mm</b>, <b>hh:mm:ss</b> nebo <b>hh:mm:ss.tt</b>

   ■ <b>textová konstanta</b> ... řetězec libovolných znaků v apostrofech
                           apostrof uvnitř konstanty: dva apostrofy <b>''</b>
                           znak zadaný dekadickým kódem znaku: <b>\nnn</b>
  
   ■ <b>logická konstanta</b> ... <b>false</b>, <b>true</b>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  datum 12.7.1995 lze zadat konstantou 12.7.1995   nebo  12.7.95
             12.7.95                        12.7.0095
              1.1.1                          1.1.0001
              1.1.1996                       1.1.1996   nebo   1.1.96
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t330">maska pro datum</a>     <a href="rozhrani.md#t488">kódy řídících a funkčních kláves</a>
</pre>

<a name="t660"></a>
## operátory

<pre>
                                 <b>OPERÁTORY</b>
 ──────────────────────────────────────────────────────────────────────────────
 logické                  číselné                      srovnávací
 ───────────────────      ──────────────────────       ──────────────────
 negace      (NOT) <b>^</b>      sčítání              <b>+</b>       rovnost          <b>=</b>
 konjunkce   (AND) <b>&amp;</b>      odčítání             <b>-</b>       nerovnost        <b>&lt;&gt;</b>
 alternativa (OR)  <b>|</b>      násobení             <b>*</b>       větší            <b>&gt;</b>
 implikace         <b>=&gt;</b>     dělení               <b>/</b>       větší nebo roven <b>&gt;=</b>
 ekvivalence      <b>&lt;=&gt;</b>     celočíselné dělení  <a href="#t664">div</a>      menší            <b>&lt;</b>
                          zbytek po dělení    <a href="#t664">mod</a>      menší nebo roven <b>&lt;=</b>
 textové                  zaokrouhlování     <a href="#t664">round</a>     prvek intervalu  <a href="#t280">in</a>
 ──────────────
 zřetězení    <b>+</b>

 ■ srovnávací operátory slouží k porovnání čísel i textů a mohou být doplněny:
 - <b>Operátor.Číslo</b> ... přesnost číselného srovnání (implicitně 5 des.míst)
 - <b>Operátor~</b> ........ lexikální textové srovnání v národní abecedě
                      s ignorováním mezer zprava

 ░ 1.234 &gt; 1.2   ale   1.234 =.1 1.2
 ░ 'Praha' &lt;&gt; 'Praha '   ale   'Praha' =~ 'PRAHA   '       'baba'=~'bÁBa'
 ──────────────────────────────────────────────────────────────────────────────
 <a href="#t668">lexikální třídění</a>     <a href="#t665">priorita operátorů</a>     <a href="#t669">░ výrazy</a>
</pre>

<a name="t664"></a>
## speciální operátory

*Též:* `div`, `mod`, `round`

<pre>
                            <b>SPECIÁLNÍ OPERÁTORY</b>
  ────────────────────────────────────────────────────────────────────────────
  █ ČísVýraz <b>DIV</b> ČísVýraz :real ..... celočíselné <b>dělení</b>
  █ ČísVýraz <b>MOD</b> ČísVýraz :real ..... <b>zbytek</b> po celočíselném dělení

  ░ 10 div 3 = 3
  ░ 10 mod 3 = 1

  █ ČísVýraz1 <b>ROUND</b> ČísVýraz2 :real ... <b>zaokrouhlení</b> ČísVýraz1 na určený počet
                                        míst dle hodnoty ČísVýraz2 (0..10)

  ░ pi round 2 = 3.14
  ░ 1.6 round 0 = 2

  █ Výraz <b>IN</b> [SeznamArgumentů] :boolean ... <b>množinový test</b>
    testuje, zda výraz je prvkem množiny konstant nebo intervalů konstant
    konstanty a výraz jsou stejného typu (real nebo string)
  ■ <b>in.Číslo</b> ...<b>přesnost</b> číselného srovnání (počet des.míst, implicitně 5)
  ■ <b>in~</b> ....... <b>lexikální</b> textové srovnání v národní abecedě

  ░ Účet in ['300'..'399','520','800'..'999']
  ░ Rok in [1914..1918,1939..1945,1968,1989]
  ────────────────────────────────────────────────────────────────────────────
  <b>in</b> - též <a href="procedury.md#t441">forall</a> cyklus        <a href="#t666">░ speciální konstrukce</a>
</pre>

<a name="t665"></a>
## priorita operátorů

<pre>
                             <b>PRIORITA OPERÁTORŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   Pokud není uzávorkováním určeno jinak, operátory mají následující priority 
   (od nejvyšší k nejnižší):

                        1.  - (unární mínus)  ^
                        2.  *  /  div  mod  round
                        3.  +  -
                        4.  =  &lt;&gt;  &gt;  &gt;=  &lt;  &lt;=
                        5.  &amp;
                        6.  |
                        7.  =&gt;  &lt;=&gt;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t666"></a>
## ░ speciální konstrukce

<pre>
                       <b>PŘÍKLAD - SPECIÁLNÍ KONSTRUKCE</b>
 ──────────────────────────────────────────────────────────────────────────────
 ░ přídavky:=cond (děti  = 1:  200,                               {funkce cond}
 ░                 děti  = 2:  430,
 ░                 děti  = 3:  680,
 ░                 děti  = 4: 1240,
 ░                 děti &gt;= 5: 1240+(děti-4)*350);
 ░ cond(Muž:'pan '+jméno,
 ░      else:'paní '+jméno);

 ░ cond(účet in ['100'..'199','450','700'..'999']:MáDáti-Dal,     {operátor in}
 ░      účet in ['200'..'399']:Dal-MáDáti);
 ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t668"></a>
## lexikální třídění

*Též:* `lexikální porovnání`

<pre>
                             <b>LEXIKÁLNÍ TŘÍDĚNÍ</b>
   ──────────────────────────────────────────────────────────────────────────
   PC FAND může pracovat s instalovatelnou tabulkou  národní abecedy. Podpora
   pro práci s národní abecedou se týká <b>třídění</b> a <b>porovnávání</b> a funkce <a href="#t710">upcase</a> 
   resp. <a href="#t710">lowcase</a>.  Syntakticky  se  v projektu odlišuje  lexikální  relace od
   běžného třídění (podle kódu znaku) uvedením vlnovky<b>~</b> za operátor nebo před
   klíčový (řídící, třídící) údaj.
 
   Při lexikálním <b>třídění</b> je správně zatříděna dvojhláska <b>CH</b>, jsou rovnocenná 
   ( tj. jejich pořadí je náhodné ) <b>malá</b> a <b>velká písmena</b> a písmena s <b>čárkou</b> a
   bez čárky. Naproti tomu písmeno s <b>háčkem</b> je  zatříděno  až  za písmeno bez  
   háčku. Při lexikálním <b>srovnání</b> se ignorují mezery zprava. Lexikální relace
   zasahuje do těchto míst projektu:

   ■ <b>třídění</b> podle národní abecedy (ruční třídění v <b>dat. editoru</b>, příkaz <a href="procedury.md#t286">sort</a>)
   ■ <b>klíčové údaje</b> typu A v definici <b>klíčů</b>
   ■ <b>řídící</b> a <b>třídící údaje</b> v <b>sestavě</b> a <b>transformaci</b>
   ■ <b>srovnávací operátory</b> pro porovnávání textů, operátor <b>in</b>, funkce <b>pos</b>
   ■ <b>hledání</b> v textovém editoru

   ░ Název=~''   #K @ ~Jméno    #I1_DATA ~ŘídÚdaj;~TřídÚdaj   Místo in~ [...]
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t225">instalace abecedy</a>    <a href="#t660">operátory</a>       <a href="rozhrani.md#t205">výběr ze seznamu</a>    <a href="formular-sestava.md#t392">VstupTransformace</a>
   <a href="#t710">upcase</a>               <a href="soubory.md#t338">vlastní klíč</a>    <a href="formular-sestava.md#t369">VstupSestavy</a>        <a href="#t280">in</a>    <a href="editory.md#t238">vyhledávání</a>
</pre>

<a name="t669"></a>
## ░ výrazy

<pre>
                              <b>PŘÍKLAD - VÝRAZY</b>
   ──────────────────────────────────────────────────────────────────────────
   ░ 3.14                         {číselné výrazy}
   ░ číslo+1
   ░ Today-DatumNarození
   ░ množství * číselník.cena
   ░ 12:30:01 + 24.12.89
   ░ sin(1.23456789)+exp(2*int(x))

   ░ 'Praha'                      {textové výrazy}
   ░ jméno+' '+příjmení
   ░ copy(upcase(text),5,10)
   ░ str(číselník.cena,6,2)+' Kč'
   ░ leadchar(' ',Název)

   ░ true                         {logické výrazy
   ░ mzda &gt; 3000
   ░ (jméno='Novák' &amp; místo='Praha')
   ░ narozen &lt; 1.1.50
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t682"></a>
## aritmetické funkce

*Též:* `abs`, `int`, `frac`, `sqr`, `sqrt`, `ln`, `exp`, `pi`, `sin`, `cos`, `arctan`

<pre>
                             <b>ARITMETICKÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────
   Uvedené funkce jsou typu <a href="#t658">real</a>, jejich výsledkem je tedy číselná hodnota.

   █ <b>abs</b>(ČísVýraz) ...... absolutní hodnota
   █ <b>int</b>(ČísVýraz) ...... celá část čísla (před desetinnou tečkou)
   █ <b>frac</b>(ČísVýraz) ..... desetinná část čísla (za desetinnou tečkou)
   █ <b>sqr</b>(ČísVýraz) ...... druhá mocnina
   █ <b>sqrt</b>(ČísVýraz) ..... druhá odmocnina
   █ <a href="#t686">random</a> ............. <a href="#t686">náhodné číslo</a> z intervalu &lt;0,1)

   █ <b>ln</b>(ČísVýraz) ....... přirozený logaritmus
   █ <b>exp</b>(ČísVýraz) ...... exponenciální funkce

   █ <b>pi</b> ................. Ludolfovo číslo
   █ <b>sin</b>(ČísVýraz) ...... sinus
   █ <b>cos</b>(ČísVýraz) ...... cosinus
   █ <b>arctan</b>(ČísVýraz) ... arcus tangens
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t686"></a>
## náhodné číslo

*Též:* `random`, `randseed`, `randomize`

<pre>
                               <b>NÁHODNÉ ČÍSLO</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>random</b> vrací náhodné číslo z intervalu &lt;0,1).  Protože však PC FAND
   interně také používá  generátor náhodných čísel, je třeba při specifických
   požadavcích  generátor  inicializovat.  Procedura  <b>randomize</b>  inicializuje
   generátor  náhodných čísel  náhodnou  hodnotou , odvozenou  ze systémových
   hodin. Interní proměnná <b>randseed</b> obsahuje startovací hodnotu generátoru.
   Po nastavení randseed na určitou hodnotu jsou generována náhodná čísla v
   naprosto konkrétním pořadí.


   ██ Syntaxe:        <b>RANDOM</b>   : real              .... funkce

                      <b>RANDSEED</b> := ČísVýraz         .... interní proměnná

                      <b>RANDOMIZE</b>                    .... příkaz procedury

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t682">aritmetické funkce</a>
</pre>

<a name="t692"></a>
## funkce pro datum a čas

<pre>
                           <b>FUNKCE PRO DATUM A ČAS</b>
   ───────────────────────────────────────────────────────────────────────────
   PC FAND obsahuje  datumovou aritmetiku , tj. funkce pro počítání  a převody
   datumu a času. Zahrnuje i práci s interním kalendářem - tabulkou pracovních
   dní, viz. <a href="prostredi.md#t220">Instalační program</a>.  Pokud  funkce pracují s datumem a časem jako
   číselným  výrazem ( typ <b>real</b> ) , jde o stejný  formát jako  FANDovský typ <b>D</b>
   (celá část - pořadové číslo dne od 01.01.0001 a desetinná část - část dne).

   <b>Přehled funkcí:</b>
   ■ <a href="prostredi.md#t691">today</a> ......... aktuální (dnešní) datum
   ■ <a href="prostredi.md#t691">currtime</a> ...... vrací aktuální čas
   ■ <a href="prostredi.md#t691">strdate</a> ....... převod datumu a času na string
   ■ <a href="prostredi.md#t691">valdate</a> ....... převod datumu a času v textové podobě na interní (real)
   ■ <a href="#t696">addwdays</a> ...... přičtení počtu dní daného typu k počátečnímu datu
   ■ <a href="#t696">addmonth</a> ...... k počátečnímu datu přičte zadaný počet měsíců
   ■ <a href="#t698">difwdays</a> ...... počet dní daného typu v časovém období
   ■ <a href="#t698">difmonth</a> ...... počet měsíců v časovém období

   ───────────────────────────────────────────────────────────────────────────
</pre>

<a name="t694"></a>
## typeday

*Též:* `typ dne`

<pre>
                                  <b>TYP DNE</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>typeday</b> vrací typ zadaného dne.

   ██ Syntaxe:     <b>TYPEDAY (</b> Datum <b>)</b> : real

   ■  <b>Datum</b> ...... Číselný výraz, datum v interním formátu PC FANDu.
   ■  <b>Typ dne</b> .... Návratová hodnota :
                   <b>0</b> - pracovní den
                   <b>1</b> - sobota, den pracovního volna
                   <b>2</b> - neděle, den pracovního klidu
                   <b>3</b> - svátek

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t692">funkce pro datum a čas</a>           <a href="prostredi.md#t220">instalační program</a>          <a href="prostredi.md#t701">░ datum a čas</a>
</pre>

<a name="t696"></a>
## addmonth

*Též:* `addwdays`

<pre>
                               <b>PŘIČTENÍ ČASU</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>addwdays</b>  přičte ( resp. odečte ) k počátečnímu datumu zadaný počet
   dní daného typu.  Obdobně funkce <b>addmonth</b> přičte zadaný počet měsíců.
   Návratové hodnoty jsou v interním datovém formátu PC FANDu.

   ██ Syntaxe:       <b>ADDWDAYS (</b> PočDatum <b>,</b> OKolikDní [<b>,</b>Typ] <b>)</b> : real
                     <b>ADDMONTH (</b> PočDatum <b>,</b> OKolikMěsíců <b>)</b>     : real

   ■  <b>PočDatum</b> ..... Číselný výraz, počáteční datum v interním formátu.
   ■  <b>OkolikDní</b> .... Číselný výraz, o kolik dní (zadaného typu) se má datum
                     posunout. Když je záporný, provede se odečtení.
   ■  <b>OKolikMěsíců</b>.. obdobně
   ■  <b>Typ</b> .......... <a href="#t694">Typ dne</a> o které se má datum posunout, pokud není zadán,
                     použije se <b>typ 0</b> (pracovní den). Na výsledek funkce
                     addwdays má tedy vliv i instalovaná tabulka pracovních 
                     dní viz. <a href="prostredi.md#t220">Instalační program</a>.
   ■  Omezení ...... <b>AddWDays</b> má horní hranici 2020 (obrana proti zacyklení)

   Inverzní funkce : <a href="#t698">difwdays</a>, <a href="#t698">difmonth</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t692">funkce pro datum a čas</a>            <a href="prostredi.md#t701">░ datum a čas</a>
</pre>

<a name="t698"></a>
## difmonth

*Též:* `difwdays`

<pre>
                               <b>ODEČTENÍ ČASU</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>difwdays</b>  vrací počet pracovních dnů ( resp. dnů daného typu ) mezi
   dvěma daty. Analogicky <b>difmonth</b>  vrací rozdíl mezi daty v měsících. Funkce
   fungují v intervalu 1.1.1910 .. 31.12.2019, nezapočítává Datum1
              
   ██ Syntaxe:       <b>DIFWDAYS (</b> PočDatum <b>,</b>KoncDatum [<b>,</b>Typ] <b>)</b> : real
                     <b>DIFMONTH (</b> PočDatum <b>,</b>KoncDatum <b>)</b>        : real

   ■  <b>PočDatum</b>       Číselné výrazy, počáteční a koncové datum v interním
   ■  <b>KoncDatum</b> .... formátu. Lze i KoncDatum &lt; PočDatum.
   ■  <b>Typ</b> .......... <a href="#t694">Typ dne</a> o které se započítají do výsledku, pokud není 
                     zadán, použije se <b>typ 0</b> (pracovní den). Na výsledek 
                     funkce difwdays má tedy vliv i instalovaná tabulka 
                     pracovních dní viz. <a href="prostredi.md#t220">Instalační program</a>.

   Inverzní funkce:  <a href="#t696">addwdays</a>, <a href="#t696">addmonth</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t692">funkce pro datum a čas</a>             <a href="prostredi.md#t701">░ datum a čas</a>
</pre>

<a name="t705"></a>
## length

*Též:* `ord`, `char`

<pre>
                               <b>TEXTOVÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────

   Délka řetězce.

   ██ syntax:   <b>LENGTH ( </b>TextovýVýraz <b>)</b> : real

   ■  Kapitoly ........ F,M,R,P,D  (<a href="dalsi-kapitoly.md#t920">Funkce v L kapitole</a>)
   ──────────────────────────────

   Práce s ASCII kódem znaků. Funkce CHAR vrací znak s určitým kódem, zohlední
   aktuální tabulku znaků.  Funkce  ORD naopak vrací ASCII kód určeného znaku.
   Tyto funkce umožňují mimo jiné i vyhodnocení obecného BYTE-streamu.

   ██ syntax:   <b>CHAR (</b> Kód <b>)</b>       : string
                <b>ORD  (</b> TextVýraz <b>)</b> : real

   ■  <b>Kód</b> ......... ASCII kód znaku. Přepočte se modulo 256.
   ■  <b>TextVýraz</b> ... Vezme se první znak řetězce a ORD vrátí jeho kód.
                    ord('') = 0  viz. <a href="rozhrani.md#t794">Tabulky znaků</a>, <a href="prostredi.md#t225">instalace abecedy</a> 
   ■  Kapitoly .... F,M,R,P,D

   <a href="#t724">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██    char( 65 )  = A  ;          { = char ( 321 ) }
         ord('Jan')  = 74 ;
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t707"></a>
## val

*Též:* `str`

<pre>
                          <b>KONVERZNÍ TEXTOVÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────
   Funkce <b>val</b> převede text na číslo. Lze ji použít v kapitolách F,M,R,P,D.
   Pro převod textového zápisu datumu nebo(a) času na číslo použijte <a href="prostredi.md#t691">ValDate</a>.

   ██ syntaxe :         <b>VAL ( </b>TextVýraz <b>)</b> : real
   
   ────────────  (<a href="dalsi-kapitoly.md#t920">Funkce v L kapitole</a>)

   Převod číselného výrazu na text.

   ██ syntaxe   1:      <b>STR (</b> ČísVýraz<b>,</b> Šířka<b>,</b> PočetDesMíst<b> )</b> : string
                2:      <b>STR (</b> ČísVýraz<b>,</b> Maska <b>)</b> : string

   ■  <b>Šířka</b> ..........  Číselný výraz, celková délka výsledného řetězce.
   ■  <b>PočetDesMíst</b> ...  Číselný výraz, počet míst za desetinnou tečkou.
                        Dle počtu des. míst dojde k případnému zaokrouhlení.
                        Naopak při přetečení před des. tečkou je výsledkem
                        řetězec v plné délce.
                        Je-li <b>-1</b>, použije se exponenciální tvar.
   ■  <b>Maska</b> ... Textový výraz. Následující znaky mají speciální význam.
                '<b>,</b>'  desetinná čárka (potlačena, pokud před ní nebudou cifry)
                '<b>.</b>'  tečka oddělující skupiny cifer
                '<b>_</b>'  významné cifry, znaménko zleva nebo mezera
                '<b>-</b>'  mezery, pokud je číslo kladné
            '<b>*</b>','<b>0</b>'  významné cifry nebo znaménko zleva. Všechny '<b>_</b>' vpravo 
                od '<b>*</b>','<b>0</b>' budou nahrazeny cifrou. Pokud je číslo nenulové
                budou cifry vystupovat počínaje nejpozději prvním místem
                před čárkou. Místa za čárkou musí být ve výrazu jako zlomek.
                Nemaskované cifry resp. znaménko se připojí zleva (přetečení).
   ■  Kapitoly ........ F,M,R,P,D

   <a href="#t724">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>     ██  str(625.128,5,2) = '625.13'
   <b>░░příklady░░</b>         str(625.128,1,0) = '625'
   <b>░░░░░░░░░░░░</b>         str(625,6,0)     = '   625'
                        str(1234.25,'__.___,__') = ' 1.234,25'
                        str(1234,'000.000') = '001.234'
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t710"></a>
## nodiakr

*Též:* `upcase`, `lowcase`

<pre>
                       <b>PRÁCE S DIAKRITIKOU A ABECEDOU</b>
   ──────────────────────────────────────────────────────────────────────────

   Převod malých písmen na velká a naopak. 

   ██ syntaxe:      <b>UPCASE ( </b> TextVýraz <b>)</b> : string
                    <b>LOWCASE( </b> TextVýraz <b>)</b> : string

   ■  Upcase převede všechna malá písmena ve výrazu na velká, lowcase
      naopak velká na malá.
   ■  Kapitoly .... F,M,R,P,D
   ─────────────────────────────

   Odstranění diakritiky z textu. Použití v kapitolách F,M,R,P,D.

   ██ syntaxe:      <b>NODIAKR( </b> TextVýraz <b>)</b> : string

   <b>Poznámka:</b> Důležité je, že tyto  funkce  zohledňují instalované  národní
             prostředí ( kód Kamen. a Latin 2). Tzn. že pro  některé znaky
             s diakritikou jsou výsledky vizuálně shodné, ale interní kódy
             převedených znaků se mohou lišit.

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t724">zpracování textu</a>
</pre>

<a name="t712"></a>
## copy

*Též:* `repeatstr`

<pre>
                               <b>TEXTOVÉ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────
  
   Výběr podřetězce z textu.Funkce vrátí podřetězec textu <b>TextVýraz</b> délky
   <b>Délka</b>, který se vybere od zadané pozice.

   ██ syntaxe:      <b>COPY (</b> TextVýraz<b> ,</b>OdPozice<b> ,</b>Délka<b> )</b> : string

   ■  Kapitoly .... F,M,R,P,D  (<a href="dalsi-kapitoly.md#t920">Funkce v L kapitole</a>)

   ──────────────────────────

   Konstrukce řetězce opakováním.

   ██ syntaxe :     <b>REPEATSTR (</b> TextVýraz <b>,</b>PočetOpakování <b>)</b> : string

   ■  Kapitoly .... F,M,R,P,D

   <a href="#t724">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>           ██   copy('Jan Novák',5,5) = 'Novák'
   <b>░░░░░░░░░░░░</b>
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t715"></a>
## replace

*Též:* `leadchar`, `trailchar`

<pre>
                             <b>NAHRAZENÍ V TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   Odstranění nebo nahrazení vedoucích (LEADCHAR) nebo ukončujících
   (TRAILCHAR) znaků v textu.

   ██ syntaxe:      <b>LEADCHAR  ( '</b>Zn1<b>'</b> <b>,</b> Text [<b>,'</b>Zn2<b>'</b> ] ) : string
                    <b>TRAILCHAR ( '</b>Zn1<b>'</b> <b>,</b> Text [<b>,'</b>Zn2<b>'</b> ] ) : string
   ■  <b>Zn1</b>,<b>Zn2</b> ..... Znak Zn1 je odstraněn z textu <b>Text</b>, nebo, pokud je
                    uveden znak Zn2, je tímto nahrazen.
   ■  Kapitoly .... F,M,R,P,D  (<a href="dalsi-kapitoly.md#t920">Funkce v L kapitole</a>)

   ──────────────────────────

   Nahrazení podřetězce v textu jiným řetězcem. Na rozdíl od předchozích
   funkcí jsou nahrazeny všechny výskyty, nejen první či poslední.

   ██ syntaxe:      <b>REPLACE (</b> Co<b> ,</b> Kde <b>, </b>Čím [<b>,'</b>Podmínky<b>'</b>] <b>)</b> : string

   ■ <b>Co</b>,<b>Kde</b>,<b>Čím</b> ... Textové výrazy, význam evidentní z názvu.
                    Omezení: délka text.výrazů <b>Co</b> a <b>Kde</b> může být max.255 zn.
   ■ <b>Podmínky</b> ..... viz. funkce <a href="#t717">pos</a>


   <a href="#t724">Zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>


   ██   leadchar('x',trailchar('x','xxxAAAAxx')) = 'AAAA'

   ██   Funkce leadchar a trailchar se nejčastěji používají k odstranění
        přebytečných mezer z údajů typu A,n , což jsou textové řetězce
        pevné délky.

        F  Adresy   *   Prijmeni  : A,20 ;
                        ...

                        editace všech Nováků se musí zadat
                        edit( ADRESY,...., 
                              cond=(trailchar(' ',Prijmeni)='Novák'),...) ;

        Podmínka Prijmeni='Novák'  by vybrala prázdnou množinu, neboť při
        porovnání musí souhlasit i délky řetězců. Mezera je také znak.
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t717"></a>
## equmask

*Též:* `pos`

<pre>
                        <b>POROVNÁNÍ TEXTOVÝCH ŘETĚZCŮ</b>
   ──────────────────────────────────────────────────────────────────────────
   Hledání pozice podřetězce textu podle stanovených podmínek porovnání.

   ██ syntaxe:   <b>POS( </b>Podřetězec <b>,</b>Text [<b>,'</b>Podmínky<b>'</b> [<b>,</b>KterýVýskyt]] <b>)</b> : real

   ■  <b>Podřetězec</b> ... Textový výraz, který hledáme v řetězci <b>text</b>.
                     Omezení: vyhodnotí se maximálně v délce 255 znaků.
   ■  <b>Text</b> ......... Textový výraz, který je prohledán.
   ■  <b>Podmínky</b> ..... Textová konstanta - apostrofy se píší (nikoliv výraz).
                     Může obsahovat tyto znaky (jeden nebo kombinace ):
                     <b>w</b> ... jen celá slova, ne složeniny
                     <b>~</b> ... <a href="#t668">lexikální porovnání</a> podle národní abecedy
                     <b>u</b> ... bez rozlišení malých a velkých písmen (upcase)
   ■  <b>KterýVýskyt</b> .. Kolikátý výskyt od počátku hledáme.
   ■  Kapitoly ....... F,M,R,P,D (<a href="dalsi-kapitoly.md#t920">Funkce v L kapitole</a>)
   ──────────────────
   Porovnání textového řetězce na masku. Lze použít ve všech kapitolách.

   ██ syntaxe:   <b>EQUMASK( </b>Text <b>,</b> Maska <b>)</b> : boolean

   ■  <b>maska</b> .... Textový výraz. Je to vzor, na který se provede porovnání
                 <b>textu</b>. Maska nemusí být jednoznačná, ale mohou se v ní 
                 vyskytovat speciální znaky (tzv. wildcards) :
                 <b>?</b>   nahradí se libovolným znakem - jen pro danou pozici
                 <b>*</b>   nahradí se libovolnou kombinací znaků (řetězcem)
   ■  Kapitoly ..... F,M,R,P,D

   <a href="#t724">zpracování textu</a>

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  pos( 'lhota', 'Dolní Lhota')      = 0 ;
       pos( 'lhota', 'Dolní Lhota','<b>u</b>' ) = 7 ;
       pos( 'dolni', 'Dolní Lhota','<b>u</b>' ) = 0 ;              { vadí <b>i</b> &amp; <b>í</b> }
       pos( 'dolní', 'Dolní Lhota','<b>~</b>' ) = 1 ;
       pos( 'lh', 'Dolní Lhota','<b>w~</b>' )   = 0 ;          { jen celá slova }
       pos( 'lh', 'Dolní Lhota','<b>u</b>' )    = 7 ;
       pos( 'lh', 'Dolní Lhota','<b>u</b>',2 )  = 0 ;    { lh tam je jen jednou }

   ██  equmask( 'Dolní Lhota',  'Lhota' ) = FALSE ;
       equmask( 'Dolní Lhota', '*Lhota' ) = TRUE ;
       equmask( 'Dolní Lhota', '?Lhota' ) = FALSE ;
       equmask( 'Dolní Lhota', '??????Lhota' ) = TRUE ;

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t719"></a>
## linecnt

*Též:* `copyline`

<pre>
                           <b>ZPRACOVÁNÍ ŘÁDKU TEXTU</b>
   ──────────────────────────────────────────────────────────────────────────
   Délka  <b>volných textů</b>  ( a lokálních proměnných typu <b>string</b> ) je omezena na
   65 000 B. Parametry funkcí jsou textové a číselné <b>výrazy</b>.

   Výběr jednoho nebo více řádků z textu od zadaného počátečního řádku.

   ██ syntaxe :        <b>COPYLINE (</b> Text <b>,</b> ČísloŘádku [ <b>,</b>Počet ] <b>)</b> : string
   ■  Kapitoly ....... F,M,R,P,D
   ─────────────────────────────

   Počet řádků textu.
   
   ██ syntaxe :        <b>LINECNT (</b> TextVýraz <b>)</b> : real

   ■  Kapitoly ....... F,M,R,P,D

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t724">zpracování textu</a>
</pre>

<a name="t722"></a>
## color

<pre>
                             <b>INSTALOVANÉ BARVY</b>
   ──────────────────────────────────────────────────────────────────────────
   Pro všechny konstrukce  PC FANDu, které pracují s barvami na obrazovce, je
   možno tyto barvy instalovat programem FANDINST. Dále je v instalaci obsaže-
   no i 16 uživatelských barev pro volné použití programátorem. Všechny kódy
   barev z instalace jsou dostupné pomocí funkce <b>color</b>.

   ██ syntaxe :        <b>COLOR (</b> PořadíBarvy <b>)</b> : real

   Návratová hodnota je kódem barvy se složkou popředí i pozadí - viz. <a href="formular-sestava.md#t241">barvy</a>

   ■  <b>PořadíBarvy</b> ... pořadí daného parametru v tabulce barev programu 
                      FANDINST. Celkem 54 barev:

     0-15  uživatelská barva č.0 až 15      34              kurzíva
       16  MENU normální text               35              široký text
       17       aktivní volba               36              dvojitý text
       18       první písmeno               37              mastný text
       19       nepovolená volba            38              zhuštěný text
       20  VÝBĚR normální text              39              písmo elite
       21        aktivní položka            40  EDITOR DAT data
       22        maska pro soubor           41             datový kurzor
       23  ZADÁNÍ zadávaný text             42             podmnožina
       24         nápověda                  43             ostatní text
       25  HLÁŠENÍ na posledním řádku       44             zrušené věty
       26  POSLEDNÍ ŘÁDEK nápověda          45             výběr (F8,ShiftF8)
       27                 symboly kláves    46  UŽIVATELSKÁ OBRAZOVKA
       28                 přepínače         47  NÁPOVĚDA text
       29  PRVNÍ ŘÁDEK systém.informace     48           vybrané téma
       30  EDITOR TEXTU text                49           ostatní témata
       31               ctrl-znaky          50           zvýrazněný text
       32               blok                51  KRÁTKÁ NÁPOVĚDA
       33  DRUHY PÍSMA podtržený text       52  OKNA  stíny
                                            53  DESKTOP

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t221">instalace barev</a>
</pre>

<a name="t724"></a>
## zpracování textu

*Též:* `funkce pro zpracování textu`

<pre>
                <b>ZPRACOVÁNÍ TEXTU</b> - přehled funkcí a příkazů
    ─────────────────────────────────────────────────────────────────────────
    <a href="#t722">Color</a> .............. kód barvy podle instalace
    <a href="#t712">Copy</a> ............... výběr podřetězce
    <a href="#t719">CopyLine</a> ........... načtení určitého řádku z víceřádkového textu
    <a href="#t550">EdBool</a> ............. vrací text aktuální podmnožiny datového editoru (F6)
    <a href="editory.md#t595">EditTxt</a> ............ varianta příkazu edit pro editaci textové proměnné
    <a href="#t717">EquMask</a> ............ porovnání řetězce na masku
    <a href="#t740">EvalB</a>,<a href="#t740">EvalR</a>,<a href="#t740">EvalS</a> .. vyhodnocení textu výrazu daného typu
    <a href="procedury.md#t721">GetTxt</a> ............. načtení úseku textu ze souboru
    <a href="#t705">Char</a> ............... vrací znak s určitým ASCII kódem
    <a href="#t715">LeadChar</a> ........... odstranění nebo nahrazení vedoucích znaků
    <a href="#t705">Length</a> ............. délka textu
    <a href="#t719">LineCnt</a> ............ počet řádků textu
    <a href="#t710">LowCase</a> ............ převod velkých písmen na malé v textu
    <a href="#t710">NoDiakr</a> ............ odstranění diakritiky z textu
    <a href="#t705">Ord</a> ................ hodnota ASCII kódu znaku
    <a href="#t717">Pos</a> ................ pozice výskytu podřetězce v textu
    <a href="procedury.md#t721">PutTxt</a> ............. příkaz pro zápis textu do (fyzického) souboru
    <a href="#t712">RepeatStr</a> .......... generování textu opakováním řetězce (nebo znaku)
    <a href="#t715">Replace</a> ............ nahrazení podřetězce v textu jiným řetězcem
    <a href="#t737">SelectStr</a> .......... výběr z pole textů (jednoho nebo i podmnožiny)
    <a href="#t707">Str</a> ................ převod čísla na text
    <a href="prostredi.md#t691">StrDate</a> ............ převod datumu (času) z číselné do textové formy
   <a href="editory.md#t591"> TxtPos</a> ............. určení pozice při (po) editaci textu
    <a href="#t715">TrailChar</a> .......... odstranění nebo nahrazení ukončujících znaků
    <a href="#t710">UpCase</a> ............. převod malých písmen na velké v textu
    <a href="#t707">Val</a> ................ převod textového zápisu čísla na číselný typ (real)
    ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t726"></a>
## modulo

<pre>
                                   <b>MODULO</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>modulo</b> vrací <b>true</b> pokud kontrola vstupních parametrů metodou 
   "modulo n" vyhovuje.

   ██ Syntaxe:       <b>MODULO (</b> TextVýraz <b>,</b>n<b>,</b>w1<b>,</b>w2<b>,</b>..wm <b>)</b> : boolean

   ■  <b>TextVýraz</b> .... řetězec cifer délky m+1,  ci (i=1..m) jsou jeho cifry 
                     a cp je kontrolní cifra na pozici m+1
   ■  <b>w1</b>,...,<b>wm</b> .... tzv. váhy

   ■  definice:      ( n - sum ( ci * wi ) mod n ) mod 10 = cp

                     Pokud platí tato rovnost, je výsledek TRUE.

   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t728"></a>
## filesize

*Též:* `diskfree`

<pre>
                     <b>OBSAZENÍ DISKU A VELIKOST SOUBORU</b>
   ──────────────────────────────────────────────────────────────────────────
   Funkce <b>diskfree</b> vrací volné místo na disku v byte. Při chybě vrací -1.

   ██ Syntaxe:        <b>DISKFREE (</b> Disk <b>)</b> : real

   ■  <b>Disk</b> .......... Textový výraz, označení diskové jednotky (A,B,C,...),
                      Rozhoduje první znak výrazu.



   Funkce <b>filesize</b> vrací délku souboru v bytech (-1 když neexistuje).
   Aktivní datový soubor (kapitola F) je nejprve nutno uzavřít pomocí <a href="procedury.md#t506">close</a>.

   ██ Syntaxe:        <b>FILESIZE (</b> JménoSouboru <b>)</b> : real

   ■  <b>JménoSouboru</b>... Buď fyzická cesta v apostrofech nebo název dle katalogu.

   <b>░░░░░░░░░░░░</b>      edit( soubor,(),...) ;
   <b>░░příklady░░</b>      close ;
   <b>░░░░░░░░░░░░</b>      writeln('fyzická délka=', filesize('soubor.000')) ;
                     { POZOR! - takto lze jen, není-li soubor katalogizován }
   ──────────────────────────────────────────────────────────────────────────
   <a href="#t583">údaje o souboru</a>
</pre>

<a name="t733"></a>
## speciální funkce

*Též:* `heslo`, `password`, `version`, `testmode`

<pre>
                              <b>SPECIÁLNÍ FUNKCE</b>
   ──────────────────────────────────────────────────────────────────────────

   Funkce <b>cond</b> je obecná funkce  pro vyhodnocení  podmíněné hodnoty typu <b>real</b>
   nebo <b>string</b>. Typ vrácené hodnoty závisí na typu parametrů.

   ██ Syntaxe:   <b>COND (</b> LogVýraz1 <b>:</b> Výraz1<b>,</b>
                        LogVýraz2 <b>:</b> Výraz2<b>,</b>
                              ...
                             <b>else : </b> Výraz <b>)</b>   : real | string
                        
      Výsledkem cond je výraz u prvního splněného logického výrazu. Není-li 
      ani jeden splněn, výsledkem cond je výraz za else (nepovinná větev)
      nebo prázdný text resp. nula.
   ─────────────────

   ██ <b>password</b>: string ... vyvolá dotaz na heslo a předá jej jako výsledek

   ██ <b>version</b>: string .... číslo verze PC FANDu

   ██ <b>testmode</b>: boolean... true, pokud je PC FAND v ladícím režimu

  ──────────────────────────────────────────────────────────────────────────────
</pre>

<a name="t734"></a>
## getenv

<pre>
                         <b>HODNOTA SYSTÉMOVÉ PROMĚNNÉ</b>
   ──────────────────────────────────────────────────────────────────────────

   Hodnota systémové proměnné (<a href="prostredi.md#t216">SET-parametry</a>) z oblasti prostředí systému.

   ██ syntaxe:         <b>GETENV(</b> TextVýraz <b>)</b> : string
   

   ■  <b>TextovýVýraz</b> ... Název SET-parametru, jehož hodnotu chceme zjistit.
                       Pokud je výraz prázdný (<b>''</b>), vrátí funkce plnou
                       cestu ke spuštěnému programu .EXE ... tedy např.
                       C:\FAND\FAND.EXE.

   <b>Poznámka:</b>  Getenv('')  je jediný  korektní způsob  jak  zjistit, z kterého
              adresáře je spuštěn FAND.EXE (tzv. FAND-adresář ). Pro zjištění
              adresáře spuštěné úlohy (.RDB) je možno  využít faktu, že tento
              adresář je aktivní jako DOS-aktuální adresář(current directory).
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t737"></a>
## selectstr

*Též:* `foot`, `delim`

<pre>
                                 <b>SELECTSTR</b>
    ────────────────────────────────────────────────────────────────────────
    Funkce pro výběr z množiny znakových řetězců.  Vybrat lze jeden řetězec,
    podmnožinu řetězců nebo nic.  Pozici  výběrového okna  a třídění položek
    v okně  lze stanovit  pomocí parametrů.

    ██ syntaxe:  <b>SELECTSTR(</b> X<b>, </b>Y<b>,</b> Vstup
                 [<b>, head=</b> ] [<b>, foot=</b>] [<b>, delim=</b>] [<b>, mode=</b>] <b>)</b>  : string

    ■  <b>X</b>,<b>Y</b> ..... Číselné výrazy, souřad.levého horního rohu okna (0 centruje)
    ■  <b>Vstup</b> ... Textový výraz, který obsahuje seznam vstupních textových
                 řetězců, oddělených oddělovačem. Oddělovač je implicitně
                 nový řádek, lze ho předefinovat pomocí parametru <b>delim</b>.
                 Jednotlivé položky bere v maximální délce 46 znaků,
                 u delších jen začátek.
    ■  <a href="prostredi.md#t284">head</a> .... Textový výraz, obsahuje text pro horní hranu okna.
    ■  <b>foot</b> .... Textový výraz, obsahuje text pro spodní hranu okna.
    ■  <b>delim</b> ... Textová konstanta, znak v apostrofech.
    ■  <a href="prostredi.md#t283">mode</a> .... Textový výraz, mohou se vyskytovat znaky ve významu :
                       <b>A</b>   abecedně setřídit
                       <b>S</b>   vybírá se množina řetězců (subset)
                       <b>I</b>   implicitně vybrat všechny (při Enter bez výběru)
   Hodnotou funkce je seznam vybraných řetězců oddělených znakem nový řádek
   ( '\13' - vždy ). Pokud uživatel zadá ESC je seznam prázdný a <a href="procedury.md#t626">exitcode</a>=1.
   ─────────────────────────────────────────────────────────────────────────
</pre>

<a name="t740"></a>
## evals

*Též:* `evalb`, `evalr`

<pre>
                          <b>EVALB</b>, <b>EVALR</b>, <b>EVALS</b>
   ─────────────────────────────────────────────────────────────────────────
   Funkce pro vyhodnocení výrazu daného typu.

   ██ syntax:   <b>evalb(</b> TextovýVýraz <b>)</b> : boolean
                <b>evalr(</b> TextovýVýraz <b>)</b> : real
                <b>evals(</b> TextovýVýraz <b>)</b> : string

   ■  použití:  kapitola P .

   ■  <b>TextovýVýraz</b> ... Text výrazu daného typu, který se při výpočtu funkce
                       přeloží a pak vyhodnocuje. 

   Úspěšný překlad výrazu nastaví <a href="procedury.md#t626">exitcode</a>=0. Při chybě ve výrazu vrátí 
   nulovou hodnotu (FALSE,0,''). Pak je <a href="procedury.md#t626">exitcode</a>=1 a<a href="editory.md#t591"> txtpos</a> udává pozici
   chyby.

   Syntaxe výrazu odpovídá výrazům v proceduře. Při použití pro definici
   filtru (podmínky <a href="#t279">cond</a>) v dalších příkazech se mohou vyskytovat i údaje
   daného souboru (<a href="procedury.md#t441">forall</a>,<a href="prostredi.md#t282">edit</a>,<a href="formular-sestava.md#t556">report</a>,<a href="rozhrani.md#t753">graph</a>). V tomto případě ji nelze 
   v podmínce kombinovat s další podmínkou (nesmí být vnořená).
   Doporučuje se používat jen omezeně a zkušenějším programátorem .
   
   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   ██  Vyvolání editoru s libovolnou podmínkou :
       P Ukazka1 *  VAR   podminka : string ;
                    BEGIN
                    podminka:=prompt(' zadejte podmínku : ':A,60) ;
                    edit( soubor,(), cond=(evalB(podminka))) ;
                    END ;

       <b>Chybné použití:</b>
                    BEGIN
                    edit( soubor,(),
                          cond=(evalb(<b>prompt</b>(' zadejte podmínku : ':A,60))));
                    END ;
       V tomto případě se výraz překládá v kontextu věty souboru, nelze tedy
       použít funkce, vyhrazené pro procedury.

   ██  <b>evalb('') = true</b>                      { vyhodnocení prázdné podmínky }
   ──────────────────────────────────────────────────────────────────────────
</pre>

<a name="t741"></a>
## interní proměnné

<pre>
                              <b>INTERNÍ PROMĚNNÉ</b>
   ──────────────────────────────────────────────────────────────────────────
   Interní proměnné  jsou vnitřní proměnné  PC FANDu, které jsou zpřístupněny
   programátorovi.  Na rozdíl od  <b>funkcí</b>  mohou  vystupovat i na  levé straně
   přiřazovacího příkazu. 

   Je třeba upozornit na to, že přiřazení hodnoty  interní proměnné  většinou
   vyvolá další akce dle konkrétní proměnné a má spíše charakter příkazu.

        <a href="#t649">accright</a>                            <a href="#t686">randseed</a>
        soubor.<a href="#t268">archives</a>                     <a href="#t649">usercode</a>
        <a href="#t550">clipbd</a>                              <a href="#t649">username</a>
        soubor.<a href="#t291">nrecs</a>                        soubor.<a href="prostredi.md#t638">volume</a>
        soubor.<a href="prostredi.md#t638">path</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="#t264">standardní funkce</a>
</pre>

<a name="t784"></a>
## syntaktické diagramy

<pre>
                            <b>SYNTAKTICKÉ DIAGRAMY</b>
 ──────────────────────────────────────────────────────────────────────────────
 Syntaktické diagramy popisují formální syntaxi jednotlivých kapitol  projektu.
 V zápisu se používají následující speciální znaky:
 
 ■ <b>{ }</b>     kladná iterace, konstrukce uvnitř složených závorek se
           může jednou nebo vícekrát opakovat.
 
 ■ <b>[ ]</b>     nepovinný výskyt, konstrukce uvnitř hranatých závorek se
           vyskytuje jednou nebo vůbec.
 
 ■ <b>|</b>       alternativa, vyskytne se buď konstrukce vlevo od svislé
           čáry nebo konstrukce vpravo od ní.
 
 ■ <b>[{ }]</b>   iterace, konstrukce uvnitř závorek se může libovolně-krát
           opakovat nebo může být vynechána.
 ──────────────────────────────────────────────────────────────────────────────
 <a href="soubory.md#t785">»F</a> ... deklarace souboru       <a href="formular-sestava.md#t788">»M</a> ... transformace           <a href="#t790">»D</a> ... funkce
 <a href="formular-sestava.md#t786">»E</a> ... formulář                <a href="#t789">»P</a> ... procedura
 <a href="formular-sestava.md#t787">»R</a> ... sestava                 <a href="#t791">»U</a> ... seznam uživatelů
</pre>

<a name="t789"></a>
## »P

<pre>
                       <b>SYNTAXE kapitoly P - PROCEDURA</b>
 ──── Ctrl-Home ───────────────────────────────────────────────────────────────
                                                 <a href="procedury.md#t411">procedura</a>
 PopisProcedury ::=
      [ DeklaraceParametrů ]
      [ DeklaraceLokálníchProměnných ]
      begin { Příkaz ; } end;
 
 DeklaraceParametrů ::=
      ( NázevParametru : TypParametru [{ ; NázevParametru : TypParametru }] )
 
 DeklaraceLokálníchProměnných ::=                <a href="formular-sestava.md#t391">DeklaraceLokálníchProměnných</a>
      var { NázevProměnné [{ , NázevProměnné }]
            : TypProměnné [ = Konstanta ] ; }

 DeklaraceRecordProměnných ::=                   <a href="formular-sestava.md#t407">record proměnná</a>
      var { NázevProměnné [{ , NázevProměnné }]
            : record of NázevSouboru ; }

 Příkaz ::=
    PřiřazovacíPříkaz |
    ŘídícíPříkaz | PgmZávorky |
    PodmíněnýPříkaz | CasePříkaz |
    WhileCyklus | RepeatCyklus | ForCyklus | ForallCyklus |
    VoláníDatovéhoEditoru |
    AutomatickáSestava | DefinovanáSestava |
    EditaceTextu | TiskTextu |
    SpuštěníTransformace |
    VoláníProcedury |
    Třídění | Indexování |
    PřístupKVětám | 
    VoláníSubúlohy |
    PosunVKatalogu | DiskJednotky |
    Okno | Klávesnice | VýstupNaObrazovku |
    Menu | MenuBar |
    VoláníJinéhoPgmu | PřevodSouborů |
    SpeciálníPříkaz |
    SíťovéPříkazy |
    Grafy |
    Zálohování |
    Obnova |
    InicializaceGenerátoruNáhodnýchČísel |
    GenerováníPracovníhoIndexu |
    VýstupNaPort
 
 PřiřazovacíPříkaz ::=                           <a href="procedury.md#t442">přiřazovací příkaz</a>
      ObjektPřiřazení := Výraz | ObjektPřiřazení += Výraz
 
 ŘídícíPříkaz ::=                                <a href="procedury.md#t428">řídící příkazy</a>
      cancel | exit | break
 
 PgmZávorky ::=
      begin { Příkaz ; } end
 
 PodmíněnýPříkaz ::=
      if LogVýraz then Příkaz [ else Příkaz ]

 CasePříkaz ::=
      case LogVýraz : Příkaz
           { ; LogVýraz : Příkaz }
           [ else PosloupnostPříkazů ]
      end

 WhileCyklus ::=
      while LogVýraz do Příkaz
 
 RepeatCyklus ::=
      repeat { Příkaz ; } until LogVýraz

 ForCyklus ::=
      for LokálníProměnná := ČísVýraz1 to ČísVýraz2 do Příkaz
 
 ForallCyklus ::=                                   <a href="procedury.md#t441">forall</a>
      forall
        RecordProměnná |
        LokProměnná in RecordProměnná |
        LokProměnná in NázevSouboru
      [ / NázevKlíče | VazbaOwner ]
      [ ( LogVýraz ) ]  [ ! ] [ % ] do Příkaz

 NázevKlíče ::= @ |
                NázevAlternativníhoVlastníhoKlíče         |
                PracovníIndex

 PracovníIndex:: LokálníProměnnáProceduryTypuIndexOfSoubor |
                 ParametrProceduryTypuIndexOfSoubor

 VazbaOwner ::=
      owner RecordProměnná |
      owner PracovníIndex  |
      owner NázevSpojení ( RecordProměnná ) |
      owner NázevSpojení ( PracovníIndex )  |
      owner NázevSpojení [ ČísVýraz ]

 VoláníDatovéhoEditoru ::=                               <a href="procedury.md#t510">volání editoru</a>
      edit( NázevSouboru [ / NázevKlíče ] ,
            DruhEditace
            [ ParametryEditace ] [ ParametryProcEditace ] )
 
 DruhEditace ::=
      [ ^ ] ( [ SeznamÚdajů ] [ ! ] [ ? ] ) | 
      NázevFormuláře |
      DynamickáDeklaraceFormuláře |
      U= NázevPohledu

 SeznamÚdajů ::=
      [ NázevÚdaje ] { [ , NázevÚdaje ] }
 
 ParametryEditace ::=                            <a href="prostredi.md#t519">parametry editace</a>
      [ ,dupl= [ ^ ] ( SeznamÚdajů ) ] [ ,tab= [ ^ ] ( SeznamÚdajů ) ]
      [ ,noed= [ ^ ] ( SeznamÚdajů ) ]
      [ ,mode=' Volby ' ] [ ,cond=( LogVýraz ) ]
      [ ,journal= NázevSouboru ] [ ,saveafter= ČísVýraz ]
      [ ,head= TextVýraz ] [ ,last= TextVýraz ]
      [ ,refresh= ČísVýraz ] [ ,watch= ČísVýraz ]
      [ ,ctrl=TextVýraz ] [ ,alt=TextVýraz]  [ ,shift=TextVýraz]
      [ ,exit=( SeznamPřerušení ) ]
      [ ,ww=( [ ( ] Souřadnice
            [ , [ [ = ] ' TextHlavičky ' ] [ ! ] [ Barvy ] ] [ ) ] ) ]

 ParametryProcEditace ::=                        <a href="editory.md#t529">procedurální parametry editace</a>
      [ ,recno= ČísVýraz ]  [ ,reckey=TextVýraz ]
      [ ,irec= ČísVýraz  ]  [ ,field= TextVýraz ]
      [ ,owner= VazbaOwner ]
      [ ,sel=PracovníIndex ]
      [ ,check ]

 SeznamPřerušení ::=                             <a href="editory.md#t536">přerušení editace</a>
      ZpůsobPřerušení : AkcePřerušení [{ ; ZpůsobPřerušení : AkcePřerušení }]
 ZpůsobPřerušení ::=
      SeznamÚdajůAKláves | record
 AkcePřerušení ::=
      NázevProcedury | NázevProcedury (SeznamAktuálParam) |
      DynamickáDeklarace | DynamickáDeklarace(SeznamAktuálParam) |
      report ( DeklaraceReportu [ ,edit ] [ ,assign=UrčeníVýstupu ] ) |
      quit
 Barvy ::= Atr,Norm,Hili [,Subset [,Deleted [,Tab [,Select ]]]]

 AutomatickáSestava ::=                          <a href="formular-sestava.md#t556">volání sestavy</a>
      report( NázevSouboru [ / DruhKlíče ] ,
         ( [ SeznamÚdajů ] [ ? ] )
         [ ParametrySestavy ] [ ParametryAutSestavy ] )
 
 DefinovanáSestava ::=
      report( SeznamSouborů ,
              NázevKapitolyR | DynamickáDeklarace
            [ ParametrySestavy ] )

 SeznamSouborů ::=
          NázevSouboru [ / NázevKlíče ] [ ( LogVýraz ) ]  |
        ( NázevSouboru [ / NázevKlíče ] [ ( LogVýraz ) ]
      { , NázevSouboru [ / NázevKlíče ] [ ( LogVýraz ) ] } )

 DynamickáDeklarace ::= 
      [ TextovýVýraz ]                           <b>hranaté závorky se píší</b>
 
 ParametrySestavy ::=                            <a href="prostredi.md#t560">parametry sestavy</a>
      [ ,cond=( [ PodmínkaKeyIn ] LogVýraz ) ]
      [ ,assign= UrčeníVýstupu] [ ,times= ČísVýraz ]
      [ ,edit ] [ ,printctrl ] [ ,check ]
 
 ParametryAutSestavy ::=                         <a href="prostredi.md#t562">parametry automatické sestavy</a>
      [ ,mode= MódSestavy ]
      [ ,ctrl=( SeznamÚdajů ) ] [ ,sum=( SeznamÚdajů ) ]
      [ ,width= ČísVýraz ] [ ,style= TypPísma ]
      [ ,sort= ( TřídícíÚdaje ) ]
 
 TřídícíÚdaje ::=                                <a href="prostredi.md#t249">řídící a třídící údaje</a>
      [ &gt; ] [ ~ ] NázevÚdaje [{ , [ &gt; ] [ ~ ] NázevÚdaje }]
 
 EditaceTextu ::=                                <a href="editory.md#t595">příkazy pro editaci textu</a>
      edittxt( TxtSoubor [ ,noedit ] 
         [ ,txtpos= ČísVýraz ]  [ ,txtxy= ČísVýraz ]
         [ ,last=TextVýraz ]
         [ ,ctrl=TextVýraz ] [ ,alt=TextVýraz]  [ ,shift=TextVýraz]
         [ ,exit=( SeznamPřerušeníTE ) ]
         [ ,ww=( [ ( ] Souřadnice                              
         [ , [ [ = ] ' TextHlavičky ' ] [ , Atr ] ] [ ) ] ) ] )

 SeznamPřerušeníTE ::=                           <a href="editory.md#t596">přerušení editace textu</a>
      SeznamKláves : AkcePřerušeníTE
      [{ ; SeznamKláves : AkcePřerušeníTE }]

 AkcePřerušeníTE ::=
      NázevProcedury | NázevProcedury (SeznamAktuálParam) |
      DynamickáDeklarace | DynamickáDeklarace(SeznamAktuálParam) |
      quit

 TiskTextu ::=
      printtxt( TxtSoubor )
 
 SpuštěníTransformace ::=                        <a href="formular-sestava.md#t566">volání transformace</a>
      merge( NázevTransformace | 
             DynamickáDeklarace )

 VoláníProcedury ::=                             <a href="procedury.md#t436">volání procedur a podprogramů</a>
      proc( NázevProcedury | DynamickáDeklarace
          [ ,( SeznamAktuálParam ) ] )
 
 Třídění ::=                                     <a href="procedury.md#t567">třídění</a>
      sort( NázevSouboru , ( TřídícíÚdaje ))
 
 Indexování ::=                                  <a href="procedury.md#t501">indexfile</a>
      indexfile( NázevSouboru [ ,compress ] )
 
 PřístupKVětám ::=                               <a href="procedury.md#t568">přímý přístup k datům</a>
      appendrec( NázevSouboru ) |
      recallrec( NázevSouboru , ČísVýraz [ ,+ ] ) |
      deleterec( NázevSouboru , ČísVýraz [ ,+ ] ) |
      readrec( RecordProměnná , ČísVýraz ) |
      writerec( RecordProměnná , ČísVýraz [ ,+ ] )

 VoláníSubúlohy ::=                              <a href="procedury.md#t436">volání procedur a podprogramů</a>
      call( NázevSubúlohy [ , NázevProcedury [ ,( SeznamAktuálParametrů) ] )
 
 PosunVKatalogu ::=                              <a href="prostredi.md#t638">katalog v proceduře</a>
      turncat( NázevDleKatalogu , ČísVýraz )
 
 DiskJednotky ::=
      releasedrive(' Jednotka ') |
      mount( NázevDleKatalogu [ ,nocancel ] )
 
 Okno ::=                                        <a href="procedury.md#t474">okna pro výstup</a>
      with window( [ ( ] Souřadnice                                   
          [ , [ [=]'TextHlavičky' ] [ ,Atr ]] [ ) ] ) do Příkaz
 
 Klávesnice ::=                                  <a href="procedury.md#t487">ošetření klávesnice</a>
      clearkeybuf  |
      setkeybuf( TextVýraz ) |
      wait 
 
 VýstupNaObrazovku ::=                           <a href="procedury.md#t459">výstup na obrazovku</a>
      display( NázevHelpu ) |
      help( TextVýraz ) |
      headline( TextVýraz ) |
      message( SeznamPrvků [ , HesloHelpu ] ) |
      clrscr [ ( Souřadnice [ , Atr [ , Znak ] ] ) ] |
      clreol |
      gotoxy( Souřadnice ) |
      write( SeznamPrvků ) |
      writeln( SeznamPrvků )
 
 Menu ::=                                        <a href="procedury.md#t451">uživatelská nabídka</a>
      menu[ loop ] [ ( Souřadnice [ ; Atr ) ] [ ! ] [ pulldown ]
          [ ' TextHlavičky ' ] of
          { ' TextVolby ' [ , Heslo ] [ , LogVýraz [?] ] : Příkaz ; }
             [ escape: Příkaz ] end

 MenuBar ::=                                     <a href="procedury.md#t451">uživatelská nabídka</a>
      menubar [ ( SouřadniceY [ SouřadniceX, VelikostX ] [ ; Atr ] ) ] of
          { ' TextVolby ' [ , Heslo ] [ , LogVýraz [?] ] : Příkaz ; }
             [ escape: Příkaz ] end
 
 VoláníJinéhoPgmu ::=                            <a href="procedury.md#t626">externí programy</a>
      exec( JménoPgmu , Parametry [ ,nocancel ] [ ,freemem ]
                                  [ ,loadfont ] [ ,textmode ] ) |
      exec('', DOSPříkaz  [ ,nocancel ] [ ,freemem ]
                          [ ,loadfont ] [ ,textmode ] ) |
      exec('','' [ ,nocancel ] [ ,freemem ] [ ,loadfont ] [ ,textmode ] )
 
 PřevodSouborů ::=                               <a href="prostredi.md#t609">kopírování a převody</a>
      copyfile( Soubor1 [ / Formát ] , Soubor2 [ / Formát ]
                [ ,head= GlobálníProměnná ]
                [ ,nocancel ] [ ,append ] 
                [ ,mode='KL' | 'LK' | 'KN' | 'LN' | 'KW' | 'LW' | 'WL'] )

 SpeciálníPříkaz ::=
      save |
      close |
      memdiag |
      sound( ČísVýraz ) |
      nosound |
      beep |
      delay( ČísVýraz )

 SíťovéPříkazy ::=
      BlokováníSouborů |
      ZamykáníVět
                                                 
 BlokováníSouborů ::=                            <a href="prostredi.md#t867">With shared</a>
      with shared NázevSouboru(MódSdílení) {,NázevSouboru(MódSdílení)} do
          Příkaz1
      [ else Příkaz2 ]
                                                 
 ZamykáníVět ::=                                 <a href="prostredi.md#t871">With locked</a>
      with locked NázevSouboru[ČísVýraz] {,NázevSouboru[ČísVýraz]} do
          Příkaz1
      [ else Příkaz2 ];

 Grafy ::=                                       <a href="rozhrani.md#t753">Grafy</a>
      graph |
      graph( GF='TextVýraz'[ ,ww= [ ( ] ( DefiniceOkna ) [ ) ] 
                           [ ,Print=TextVýraz ]
                           [ ,Assign=TextVýraz ]
                           [ {,Txt=(XZ,YZ,VelikostPísma,BarvaPísma,Text)} ]
                           [ {,TxtWin=(XZ,YZ,XK,YK,BarvaPozadí,
                                            BarvaPísma,{ Text [ ; ] } ) } ] ) |
      graph( ZadáníGrafuSouboru )

 ZadáníGrafuSouboru ::=
      NázevSouboru  [ /NázevKlíče ], (SeznamÚdajů),
                    [, cond=[(]LogVýraz[)] ]
                    [, Type=TextVýraz ]    [, Head=TextVýraz ]
                    [, HeadX=TextVýraz ]   [, HeadY=TextVýraz ]
                    [, HeadZn=TextVýraz ]  [, Fill=TextVýraz ]
                    [, DirX=TextVýraz ]    [, Grid=TextVýraz ]
                    [, Palette=TextVýraz ] [, Width=ČísVýraz ]
                    [, Recno=ČísVýraz ]    [, NRec=ČísVýraz ]
                    [, Max=ČísVýraz ]      [,Min=ČísVýraz ]
                    [, GrPoly=ČísVýraz ]   [, Interact ]
                    [ {,Txt=(XZ,YZ,VelikostPísma,BarvaPísma,Text) } ]
                    [ {,TxtWin=(XZ,YZ,XK,YK,BarvaPozadí,
                                      BarvaPísma,{ Text [ ; ] } ) } ]
                    [ {,RGB=(KódBarvy,ČísVýraz,ČísVýraz,ČísVýraz) } ]
                    [ ,ww= [ ( ] ( DefiniceOkna ) [ ) ]
                    [ ,Print=TextVýraz ]
                    [ ,Assign=TextVýraz ]

 Zálohování ::=
      backup( NázevArchivace [ ,nocancel ] )   |
      backupm(NázevArchivace, adresář, SeznamMasek
             [,nocancel] [,nocompress] [,subdir] )

 Obnova ::=
      restore( NázevArchivace [ ,nocancel ] )    |
      restorem(NázevArchivace,Adresář
               [,subdir][,overwrite][,nocancel][,nocompress])

 InicializaceGenerátoruNáhodnýchČísel ::=
      randomize |
      randseed := ČísVýraz

 GenerováníPracovníhoIndexu ::=
      getindex(   PracovníIndex,
                  NázevSouboru[/Index]
               [ ,sort=(TřídícíÚdaje) ]
               [ ,owner= VazbaOwner ]
               [ ,cond=(LogVýraz)] )

 VýstupNaPort ::=
      PortOut( LogVýraz, ČísVýraz, ČísVýraz )

 ───── Ctrl-End ───────────────────────────────────────────────────────────────
</pre>

<a name="t790"></a>
## »D

<pre>
                  <b>SYNTAXE kapitoly D - UŽIVATELSKÉ FUNKCE</b>
 ──── Ctrl-Home ───────────────────────────────────────────────────────────────
                                                 <a href="#t310">funkce</a>
 PopisProcedury ::=
     [ { function NázevFunkce ( [ DeklaraceParametrů ] ) : Typ ;
         [ DeklaraceLokálníchProměnných ]
         begin [{ Příkaz ; }] end; } ]
 
 DeklaraceParametrů ::=
     ( [ NázevParametru : TypParametru [{ ; NázevParametru : TypParametru }]] )

 DeklaraceLokálníchProměnných ::=                <a href="formular-sestava.md#t391">DeklaraceLokálníchProměnných</a>
      var { NázevProměnné [{ , NázevProměnné }]
            : TypProměnné [ = Konstanta ] ; }

 Příkaz ::=                                      { viz. syntaxe kapitoly P }
    PřiřazovacíPříkaz |
    ŘídícíPříkaz | PgmZávorky |
    PodmíněnýPříkaz | CasePříkaz |
    WhileCyklus | RepeatCyklus | ForCyklus
 
 ──── Ctrl-End ───────────────────────────────────────────────────────────────
</pre>

<a name="t791"></a>
## »U

<pre>
                   <b>SYNTAXE kapitoly U - SEZNAM UŽIVATELŮ</b>
 ──────────────────────────────────────────────────────────────────────────────
                                                 <a href="dalsi-kapitoly.md#t644">seznam uživatelů</a>
 SeznamUživatelů ::=
    { ' JménoUživatele ', KódUživatele , ' Heslo ' , SeznamPřístupovýchPráv ; }

 ──────────────────────────────────────────────────────────────────────────────
</pre>
