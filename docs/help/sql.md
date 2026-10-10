# SQL a ODBC

<a name="t884"></a>
## ODBC

*Též:* `login`, `NULL-hodnoty`, `select`, `sqlrdtxt`, `sqlwrtxt`, `where`, `RDA`

<pre>
                                   <b>ODBC</b>
    ───────────────────────────────────────────────────────────────────────
    Pod zkratkou ODBC (<b>O</b>pen <b>D</b>ata<b>b</b>ase <b>C</b>onnectivity) se skrývá mechanismus
    otevřeného rozhraní (interface) pro práci s databázemi (datovýni soubory).
    
    Pro aplikační program přináší zabudování rozhraní ODBC možnost zpracování
    dat uložených v libovolné databázi, pro kterou existuje tzv. <b>ODBC-ovladač</b>.
    V podstatě jde o technologii zpracování <b>client-server</b>.
    Pro FANDovské datové soubory existuje ODBC ovladač pro platformy
    16 a 32 bit. Windows.
    To znamená, že z libovolné Windows-aplikace, která podporuje ODBC se
    lze "dostat" na datové soubory provozovaných FAND-aplikací. Pro příklad
    uveďme balík Microsoft Office, WinBase602, Delphi, Java atd.

    <b>ODBC ovladač pro soubory PC FANDu</b>
    je samostatný produkt, distribuovaný firmou ALIS spol.s.r.o. 
    Demoverze je dostupná na <b>www.alis.cz</b>.
    Jelikož datové soubory PC FANDu neobsahují deklaraci, je pro přístup
    přes ODBC potřeba textový soubor s deklaracemi FAND-souborů, a to
    v syntaxi jazyka SQL. 
    Je to proto, že celá filosofie ODBC je založena na jazyce <b>SQL</b> jako
    obecném databázovém jazyce.
    Pro tento definiční soubor je vyhrazena přípona <b>RDA</b>. Pro vytvoření
    tohoto souboru je většinou nutná spolupráce s autorem FAND-aplikace.
    Kromě deklarace dat obsahuje soubor RDA i definice uživatelů 
    a přístupových práv k souborům - tabulkám.

    Možnosti použití ODBC ovladače vyplývají z obecných vlastností
    technologie ODBC. Z hlediska aplikace PC FANDu je důležité to, že 
    přes ODBC ovladač je možno sdílet datové soubory spuštěné FAND-aplikace.
    Podrobnosti k instalaci jsou uvedeny v dokumetaci, která je i součástí 
    demoverze ODBC-FAND-ovladače.

    POZOR !
    Nezaměňovat ODBC ovladač s podporou <a href="#t885">SQL</a> serveru z FANDu.
    * ODBC-ovladač slouží pro přístup k datovým souborům PC FANDu (v.3.0
      a výše) z prostředí programů pro Windows.
    * SQL podpora ve FANDu (od verze 3.2) slouží k přístupu z DOS-FANDu
      k SQL-databázím. Po verzi 3.2 byl již tento směr vývoje ukončen.
    ───────────────────────────────────────────────────────────────────────
</pre>

<a name="t885"></a>
## SQL

<pre>
                                   <b>SQL</b>
    ───────────────────────────────────────────────────────────────────────
    Tato  část  nápovědy  poskytuje  ucelenou  úvodní  informaci  o podpoře
    komunikace PC FANDu s SQL serverem.

         <b>Rozšíření syntaxe příkazů.</b>

         Základní princip komunikace SQL serveru s klientem je následující:
    PC FAND (klient) vyšle příkaz na  výběr  množiny  vět  z  tabulky.  SQL
    server  provede  výběr  vět(y)  a  při   výběru   automaticky   provádí
    optimalizaci vyhledávacího  výrazu.  Vybraná  množina  vět  je  předána
    klientu  (proměnné  typu  záznam)  a   může   být   zpracována   pomocí
    standardního příkazu PC FANDu.  Po  změně  je  upravená  proměnná  typu
    záznam přenesena a zapsána do databáze SQL serveru.


         V prostředí PC FANDu lze tabulku SQL  serveru  zpřístupnit  pomocí
    souboru typu "SQL". SQL server pracuje s  tabulkou  pomocí  množinových
    operací a neumožňuje identifikaci  vět  pomocí  čísla  věty.  Větu  lze
    identifikovat  pouze  pomocí  jednoznačného   klíče.  Toto  omezení  se
    projevuje například při editaci souboru - editovat lze soubor pouze dle
    jednoznačného klíče.



         <b>Deklarace souboru typu SQL.</b>

         Soubor typu SQL je definován pomocí  přípony ".SQL" názvu kapitoly
    "F". Typ uložených údajů musí být slučitelný s existující  tabulkou  na
    SQL serveru. Délka  údaje nemusí  přesně odpovídat,  ale  různé délky a
    přesnosti mohou způsobit ztrátu dat.

                      Podporované typy:
    PC FAND   SQL

      <b>F</b>,<b>R</b>     int, smallint, tinyint. Myšlená řádová čárka ve Fandu.
              float, real. Skutečná řádová čárka ve Fandu.

      <b>A</b>,<b>N</b>     char(n) nebo varchar(n). Při různé  délce  se dle   zarovnání
              ve Fandu odříznou  a doplňují vedoucí nebo ukončující znaky.

      <b>D</b>       datetime,  smalldatetime (smalldatetime může vést ke   ztrátě
              přesnosti).

      <b>B</b>       bit.

      <b>T</b>       text. Maximálně 65 KB.

         Soubor typu SQL je  obdobou  indexového  souboru.  Podporuje  více
    vlastních  klíčů  i  parametrický  klíč.  Při   změně   hodnoty   údaje
    parametrického souboru  je prováděna  změna ve  všech  větách  souboru.
    Text delší než 32 KB lze přenést pomocí příkazů <b>sqlrdtxt</b> a <b>sqlwrtxt</b>.



         <b>Hodnota NULL.</b>

         PC FAND  nepodporuje hodnotu null, vyjímkou je pouze typ D a T.  U
    ostatních typů nelze hodnotu null přenést a platí následující  omezení.
    Hodnota  představuje nevyplněný  údaj. Při  přenosu z  SQL  serveru  je
    hodnota null převedena na 0, resp. prázdný řetězec.
         Klíčové  údaje nemohou obsahovat hodnotu null. Pro PC FAND by věty
    s tímto klíčem  byly neviditelné.  V datovém editoru by věty obsahující
    tuto hodnotu byly zobrazeny jako smazané a obarveny modrou barvou ( dle
    instalace ). Čtení a následný zápis věty z  PC FANDu změní hodnoty null
    na hodnotu 0, resp. ''.



         <b>Navázání spojení s SQL serverem.</b>

         Soubor typu SQL může být  lokální  nesdílený  soubor  (v  katalogu
    nevyplněný údaj návěští), sdílený soubor (v údaji  návěští  znak  "#").
    Uvedením "#SQL" do údaje návěští definujeme  umístění  souboru  na  SQL
    serveru.Zda bude soubor umístěn na SQL serveru,je určeno hodnotou údaje
    návěští v  okamžiku  kompilace  úlohy.  Pozdější  změna  údaje  návěští
    neprovede   změnu   umístění   souboru:   tabulka   SQL   serveru   &lt;-&gt;
    sdílený/nesdílený soubor.  Provedené  změny  umístění  souboru  zplatní
    pouze příkaz <a href="prostredi.md#t633">resetcatalog</a>.

         Při  startu úlohy  je vyhledávána  hodnota "#SQL" v údaji  návěští
    katalogu. V případě  nalezení  následuje  pokus  o navázání  komunikace
    s SQL serverem. Při neúspěchu navázat spojení je vyvolána běhová  chyba
    a chod  aplikace je ukončen.  V případě  úspěšného spojení se pokračuje
    v provádění aplikace.

         Spojení s SQL serverem musí být vyvoláno v okamžiku startu  úlohy.
    Nelze jej vyvolat  zápisem  "#SQL"  do  katalogu  a  vyvoláním  příkazu
    <a href="prostredi.md#t633">resetcatalog</a>.
    


    <b>██     Rozšíření syntaxe příkazů PC FANDu pro práci s SQL serverem.</b>

    <a href="jazyk.md#t573">keyof</a> <b>(</b> RecordProměnná[/NázevKlíče] <b>)</b> : string

    <a href="jazyk.md#t573">keyof</a> <b>(</b> SOUBOR[/NázevKlíče]<b>,</b>SeznamKlíčÚdajů <b>)</b> : string

    <a href="jazyk.md#t550">edreckey</a> :string

    <a href="procedury.md#t572">isdeleted</a> <b>(</b> RecordProměnná <b>)</b>  :  boolean

    <a href="procedury.md#t574">linkrec</a> <b>(</b>RecordProměnná1<b>,</b>[NázevSpojení<b>(</b>]RecordProměnná2[<b>)</b>]<b>)</b>

    <a href="procedury.md#t403">readrec</a> <b>(</b> RecordProměnná[<b>/</b>NázevKlíče]<b>,</b>
             [OperaceSrovnání] TextVýraz|ČíselnýVýraz <b>)</b>
       
             Při neexistenci čtené věty je proměnná typu věta vynulována a
             označena jako neplatná.


    <a href="procedury.md#t404">writerec</a> <b>(</b> RecordProměnná[<b>/</b>NázevKlíče]<b>,</b>TextVýraz|ČíselnýVýraz [<b>,+</b>] <b>)</b>

    <a href="procedury.md#t572">deleterec</a> <b>(</b> NázevSouboru[<b>/</b>NázevKlíče]<b>,</b>TextVyraz|ČíselnýVýraz[<b>,+</b>] <b>)</b>


    <b>sql(</b>TextovýVýraz<b>)</b>[:real]

         Provedení SQL  příkazu  SQL  serverem.  Lze  použít  pro  vyvolání
    uložených procedur a příkazů, které nevracejí tabulku.  Příkaz  použitý
    jako funkce vrací chybový návratový kód  a  negeneruje  běhovou  chybu.
    Příkaz použitý jako procedura generuje v případě chyby na  SQL  serveru
    běhovou chybu.


    <a href="procedury.md#t441">forall</a> RecordProměnná <b>in sql(</b>TextovýVýraz<b>) do</b> Příkaz

         Provedení  SQL   příkazu  uloženého  v  TextovýVýraz  generujícího
    výstupní tabulku SQL serverem. Sloupce tabulky musí odpovídat deklaraci
    údajů  v RecordProměnné,  vztahující se  na popis  souboru typu  "SQL".
    Výsledkem  SQL příkazu může být pouze jedna tabulka, zpracovaná  forall
    cyklem.
         Příkaz <b>break</b> ukončí provádění cyklu. Tabulka je PC FANDem  dočtena
    do konce.


    <b>login(</b> TextVýraz1<b>,</b>TextVýraz2 <b>)</b>

         Explicitní  přihlášení  k  SQL  serveru  jako  uživatel  s  jménem
    TextVýraz1 a heslem TextVýraz2. Není vytvořeno další spojení, ale jde o
    odhlášení a nové přihlášení.

         Příkaz  nelze vyvolat  v okamžiku  provádění akce,  např.  v  exit
    proceduře, forall in sql atd.

         Příkaz  se  provádí, jen  když  existuje  spojení, tj. v okamžiku
    kompilace je v údaji návěští katalogu uvedeno "#SQL".
         Pokud  katalog úlohy  obsahuje  návěští  "#SQL",  FAND automaticky
    vysílá login  při překladu  hlavní úlohy.  Jméno a  heslo uživatele  je
    čteno z kapitoly U).


    <b>sqlrdtxt(</b>NázevBinSouboru<b>,</b>NázevDatSouboru[<b>/</b>Klíč]<b>,</b>Údaj<b>,</b>TextVýraz<b>)</b>
    <b>sqlwrtxt(</b>NázevBinSouboru<b>,</b>NázevDatSouboru[<b>/</b>Klíč]<b>,</b>Údaj<b>,</b>TextVýraz<b>)</b>

         Čtení  či zápis údaje Údaj typu Text z tabulky NázevDatSouboru SQL
    serveru.  Údaj je uložen do binárního souboru NázevBinSouboru. Věta  je
    identifikována pomocí klíče uloženého v TextVýraz.

         Maximální délka textu je 1800 MB. 

    
    <a href="prostredi.md#t633">resetcatalog</a>

         Příkaz zajišťuje zohlednění změn v katalogu. Uzavírá všechny
    soubory mimo aktivních projektů. Změní umístění souborů ( tabulka SQL
    serveru &lt;-&gt; sdílený/nesdílený soubor) dle hodnoty údaje návěští.


    <b>Příkaz edit</b>

         Při  editaci tabulky  je vždy  použit pracovní index. Na  počátku
    editace FAND  čte hodnoty  klíčových údajů  pro  vytvoření  pracovního
    indexu.  Po ukončení  výběru je  pracovní index setříděn. Při  vlastní
    editaci FAND zasílá příkaz pro výběr, zápis a mazání jednotlivých  vět
    identifikovaných dle hodnoty pracovního indexu.

         Pro SQL soubory nelze zadávat následující  parametry  :  journal=,
    refresh=, saveafter=, owner=, recno=, irec=.

         Blokování   vět  realizované   při  sdílené   editaci  údaje  není
    realizováno.  Změněná věta  je ukládána  až při ukončení editace  věty.
    Před vlastním zápisem se provádí kontrolní čtení věty, a porovnává se s
    původním  obsahem  před  začátkem  editace.  Při  nerovnosti  následuje
    hlášení  o změně věty, zobrazení aktuálního načteného stavu a  uživatel
    může pokračovat v editaci.

         Při editaci údaje je chyba SQL serveru hlášena pouze číslem,  ale
    nevyvolá  běhovou chybu a ukončení FANDu. Časté chyby tvoří  duplicita
    klíče a vkládání nepovolené hodnoty.


         <b>Vstupní podmínka.</b>

         Vstupní výběrová podmínka  zadávaná  pomocí  logického  výrazu  je
    realizovaná na  lokální  stanici  (z  SQL  souboru  je  přenesena  celá
    tabulka). Výběr vět na SQL serveru  lze  realizovat  zadáním  textového
    výrazu namísto logické podmínky u kapitol  "R",  "M"  a  příkazů  edit,
    graph, forall, report. Výběrový výraz odpovídá klauzuli  <b>WHERE</b>  příkazu
    <b>SELECT</b>.

        

         <b>Třídění a automatické třídění reportu a merge.</b>

         Třídění je  realizováno  na  SQL  serveru.  Součástí  řídících  a
    třídících údajů  nemůže být vypočítaný údaj. Při použití  vypočítaného
    údaje SQL server generuje běhovou chybu neznámého názvu údaje.



         <a href="formular-sestava.md#t389">Transformace</a>

         Pro vstupní  i výstupní  soubory je  vytvořen  samostatný  proud.
    Vstup  je  realizován  jako  výběr  vět  příkazem  <b>SELECT</b>.  Výstup  je
    realizován  komunikačním serverem  jako <b>INSERT</b>  nebo  bulkcopy.  Chyby
    vznikající při  zápisu jsou  komunikačním  serverem  kumulovány  a  po
    ukončení   transformace  předány   FANDu,  zobrazeny   a  následně  je
    generována běhová chyba.



         <b>Vytváření aplikace pro různá prostředí.</b>

         <a href="prostredi.md#t262">Direktivy</a>  kompilátoru  umožňují podmíněný  překlad, tj. vypuštění
    části zdrojového kódu.  V praxi  lze například  jednu aplikaci vytvářet
    pro použití s SQL  serverem, nebo  jako klasickou aplikaci se  sdílením
    souborů.
         Před   použitím    direktiv  si    pečlivě  prostudujte   omezení.
    Příkladem použití může být vstupní podmínka. U SQL verze je  výhodnější
    zadávat podmínku  v textovém  výrazu a  pro klasickou  verzi  je  nutno
    zadat logický výraz.

    Příklad:
    edit(ORGANIZ,U=ORGANIZ_FIRMA,cond=(
           {$ifdef S} 'Firma like "'+trailchar(' ',PARAMADR.Text30)+'%"'
           {$elsif} Firma =~ PARAMADR.Text30
           {$endif}));



         <b>SQL příkazy a provoz SQL aplikace bez SQL serveru.</b>

         Popsané  příkazy  lze  používat   i   v   aplikačních   programech
    pracujících se sdílenými soubory. Vyjímkou jsou příkazy :

    <b>sql</b>, <b>forall</b>  rp <b>in</b> <b>sql</b>, <b>sqlrdtxt</b>, <b>sqlwrtxt</b> a textově zadávaná  vstupní
    podmínka.

         Tyto příkazy jsou ignorovány. Při vytváření aplikace používané pro
    práci  s SQL  serverem i  bez SQL  serveru  v  prostředí  lokální  sítě
    doporučujeme  programovat   univerzální  projekt   a  využít   direktiv
    kompilátoru pro vložení odpovídajících příkazů.



         <b>Příklady použití souborů typu "SQL" v PC FANDu.</b>

    ■ Vstupní a  výstupní  soubor  reportu,  merge,  copyfile  a  writerec,
      readrec, deleterec,  edit, forall, konstrukce NadřazenýSoubor.Údaj  a
      ParametrSoubor.Udaj...



         <b>Omezení</b>

    ■ Soubory typu SQL nelze použít v příkazech pracujících s číslem věty
    ■ DML nepodporuje přístup k souborům typu  SQL.  Lze realizovat  pomocí
      knihovny funkcí dodávané společně s SQL serverem
    ■ Copyfile z/do binárního souboru není povoleno
    ■ Násobný výstup #O*_ nelze aplikovat při vstupním souboru typu SQL
    ■ Transformace "na místě" není dovolena
    ■ SQL soubor lze editovat pouze dle jednoznačného vlastního klíče
    ■ Aditivní  přiřazení  ":="  není  přípustné  pro  SQL soubor
    ■ Přerušení  vytváření reportu  stiskem klávesy  ESC přeruší  vytváření
      sestavy, ale tabulka z SQL serveru je čtena až do konce
    ■ SOUBOR.nrecs:= nelze použít
    ■ nelze použít owner=ProměnnáTypuIndex


         <b>Převod do SQL FANDu</b>

         Převod hotových aplikačních programů  na  platformu  klient-server
    je umožněn s minimálním množstvím změn v zdrojovém kódu. Jde  především
    o  náhradu  příkazů  realizujících  přímý   přístup   a   používajících
    identifikační číslo věty  tvarem  využívajícím  identifikaci  věty  dle
    jednoznačného klíče.

         Prvním  krokem  by  měla  být  analýza  datové  základny  a  změna
    duplicitních  klíčů  na  jednoznačné.  Většina  indexovaných souborů má
    primární klíč  jednoznačný a  alternativní klíč duplicitní.

         SQL  server SYBASE  neumožňuje indexaci  dle  vypočítaného  údaje,
    údaje  typu bit  (B) a  text (text) ! V deklaraci souboru typu SQL  lze
    definovat klíč s vypočítaným údajem i typem B.

         V některých případech jsou alternativní klíče používány pouze  pro
    zrychlení  výběru a dle alternativního klíče není prováděna editace.  V
    tomto případě  postačí definice klíčů na SQL serveru a v PC FANDu  není
    nutno tyto klíče definovat. Výběr zadaný textovým výrazem je zaslán  na
    server, který provede výběr dle optimálního indexu.
         V případě  požadavku editace  dle klíče  je nutno  klíč doplnit  o
    jednoznačný primární klíč.

    Příklad klíče adresáře:
   #K @ Cislo;                    { Primární klíč }
   ORGANIZ_MISTO(@) ~Misto,Cislo; { Alternativní klíče }
   ORGANIZ_PRIJM(@) ~Jmeno,Cislo;

         Po  opravě klíčů  změňte typ  souboru na  "SQL".  Kontrola  pomocí
    překladu  odhalí především  používání identifikátoru čísla věty,  které
    je nutno nahradit identifikací klíčem.

         Dále převeďte vstupní podmínky do textového tvaru. Tuto část  není
    možno odladit bez propojení s SQL Serverem. Textové podmínky nejsou  PC
    FANDem kontrolovány a je nutno je ověřit provedením.

         Náročnější  částí  je  využití  a  definice  uložených   procedur,
    triggerů atd. Programování SQL serveru je hlavním nástrojem pro zvýšení
    bezpečnosti a rychlosti zpracování.

    ───────────────────────────────────────────────────────────────────────
    <a href="#t884">ODBC</a>
</pre>
