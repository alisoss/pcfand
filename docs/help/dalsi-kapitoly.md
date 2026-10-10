# Uživatelé, vkládané texty, PROLOG (U, I, L)

<a name="t267"></a>
## kapitola I

*Též:* `vkládané texty`, `include`

<pre>
                             <b>VKLÁDANÉ KAPITOLY</b>
   ──────────────────────────────────────────────────────────────────────────
   Text kapitoly typu I se kopíruje během překladu na dané místo ( kapitola I
   musí být ve stejné nebo nadřízené úloze). To znamená, že syntaxe  kapitoly 
   I odpovídá  syntaxi kapitoly, kam bude text vložen. Nemusí  však obsahovat
   syntakticky uzavřený  celek. Jakákoli  změna takové  kapitoly vyvolá  nový
   překlad celé úlohy. Kapitoly I použijeme tehdy, když se několikrát opakuje 
   stejný úsek textu programu.
   
   ■ Místa, kam se má vložit text kapitoly I se určí speciální direktivou
     kompilátoru:
                         <b>{$include NázevKapitolyI}</b>

   ■ Pozor na mezery okolo složených závorek (jsou významné).
   ■ Podmínky použití v kapitole F:
     Druhá část kapitoly F (#U/D/L/I) nesmí začít instrukci {$include ..}.

   ■ Při nenávratném zaheslení úlohy (<b>Alt-F10</b>) se text kapitoly I substituuje 
     a <b>kapitoly I se zruší</b>. Pozor na dvě úskalí:
     - po substituci nesmí být překročena maximální délka volného textu 
       65000 B (jinak běhové chyby).
     - nejprve zaheslit podřízené úlohy
   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t262">direktivy</a>
</pre>

<a name="t305"></a>
## call

<pre>
                      <b>CALL</b> - duplicitní klíčové slovo
   ──────────────────────────────────────────────────────────────────────────

   ■ volání podřízené úlohy ..... viz. <a href="procedury.md#t436">volání procedur a podprogramů</a>

   ■ volání predikátu z jiné L-kapitoly.... viz.<a href="procedury.md#t908">  call</a>

   ──────────────────────────────────────────────────────────────────────────
   <a href="prostredi.md#t306">duplicitní klíčová slova</a>
</pre>

<a name="t644"></a>
## seznam uživatelů

<pre>
                       <b>SEZNAM UŽIVATELŮ - kapitola U</b>
   ──────────────────────────────────────────────────────────────────────────
   <b>Seznam uživatelů</b> je nepovinná kapitola typu <b>U</b> na začátku úlohy. Nemá jméno,
   musí být jako první v projektu a ve strukturovaných úlohách může být pouze
   v hlavní úloze. Obsahuje seznam oprávněných uživatelů úlohy.Každý uživatel  
   má přiřazeno <b>jméno</b> (text), <b>kód</b> (číslo), <b>heslo</b> a seznam <b>přístupových práv</b>.

   █ '<b>Jméno</b>',<b>Kód</b>,<b>Heslo</b>,(<b>SeznamPřístupovýchPráv</b>); ... jedna položka seznamu
 
   ■ <b>Jméno</b>:  jméno aktuálního uživatele pro identifikaci v projektu
   ■ <b>Kód</b>:    číslo aktuálního uživatele pro identifikaci v projektu
   ■ <b>Heslo</b>:  tímto heslem se uživatel přihlašuje při startu úlohy
   ■ <b>Seznam</b>: seznam čísel (1..255) pro rozlišení přístupových práv v projektu
             lze použít i interval

   Pokud  je úloha opatřena kapitolou U, před vstupem do úlohy musí  uživatel
   zadat jedno  z povolených hesel, jinak úloha nebude spuštěna. Po  správném
   zadání hesla se naplní <b>jméno</b>, <b>kód</b> a <b>přístupová práva</b> aktuálního  uživatele
   a podle  nich lze  měnit  chování  programu  v  dalším  zpracování.  Podle
   kapitoly U se řídí i omezení při <b>navigaci po databázi</b> podle  <b>uživatelských</b>
   <b>pohledů</b>. <b>Prázdná kapitola U</b> má stejný efekt, jako když kapitola neexistuje
   (lze dodatečně instalovat z hlavního menu PC FANDu bez zásahu do programu).

   <b>░░░░░░░░░░░░</b>
   <b>░░příklady░░</b>
   <b>░░░░░░░░░░░░</b>

   {obsah kapitoly U}
   'Vladimír Grosmut',1,'VG',(1,2,4..6,9,11);
   'Luděk Galbavý',2,'LG',(1,3);
   'Iva Jablonská',3,'IJ',(10..11);
   ──────────────────────────────────────────────────────────────────────────
   <a href="jazyk.md#t649">práva přístupu k datům</a>      <a href="editory.md#t251">navigace po databázi</a>       <a href="soubory.md#t350">uživatelské pohledy</a>
   <a href="jazyk.md#t733">heslo</a>                       <a href="jazyk.md#t791">»U</a>                         <a href="kontextova-napoveda.md#t9">instalace úlohy</a>
</pre>

<a name="t887"></a>
## Prolog

*Též:* `L`

<pre>
                                 <b>FAND PROLOG</b>
    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a> : #<a href="#t898">DOMAINS</a>                <a href="#t888">Identifikátory</a>        <a href="#t889">Term</a>
              #<a href="#t899">CONSTANTS</a>              <a href="#t891">Predikát</a>              <a href="#t890">Proměnné</a>
              #<a href="#t901">DATABASE</a>               <a href="#t892">Provádění predikátů</a>
              #<a href="#t902">PREDICATES</a>             <a href="#t918">OperaceSrovnání</a>
              #<a href="#t903">CLAUSES</a>                <a href="#t920">Funkce v L kapitole</a>

    <b>Implicitní predikáty:</b>
    <a href="#t904">!</a>             <a href="#t936">fandkey</a>           <a href="#t914">retract</a>
    <a href="#t927">abbrev</a>        <a href="#t937">fandkeyfield</a>      <a href="#t916">  save</a>
    <a href="#t928">add</a>_          <a href="#t938">fandlink</a>          <a href="#t909">self</a>
    <a href="#t929">all</a>_          <a href="#t939">fandlinkfield</a>     <a href="#t912">trace</a>
    <a href="#t913">assert</a>        <a href="#t925">getlex</a>            <a href="#t921">union</a>_
    <a href="procedury.md#t436"> call</a>         <a href="#t923">inter</a>_            <a href="#t911">write*</a>
    <a href="#t919">concat</a>        <a href="#t931">inv</a>_
    <a href="#t917">consult</a>       <a href="#t932">len</a>_
    <a href="#t930"> del</a>_         <a href="#t924">loadlex</a>
    <a href="#t907"> error</a>        <a href="#t933">mem</a>_
    <a href="#t906">fail</a>          <a href="#t922">minus</a>_
    <a href="#t935">fandfield</a>     <a href="#t926">nextlex</a>
    <a href="#t934">fandfile</a>      <a href="#t905">not</a>
    ───────────────────────────────────────────────────────────────────────
</pre>

<a name="t888"></a>
## Identifikátory

<pre>
                           <b>Identifikátory, komentář</b>
    ───────────────────────────────────────────────────────────────────────
         Kompilátor  důsledně  rozlišuje  velikost  písmen  identifikátorů.
    Zvláštní  význam  má  velikost  prvního  znaku  identifikátoru.  Velkým
    písmenem  se označují  proměnné  a  typy  dat.  Malá  písmena  označují
    konstanty a predikáty.

         Identifikátor  musí začínat  písmenem nebo  znakem "_" a na  další
    pozici může být i číslice. Celková délka identifikátoru nesmí být větší
    než 32 znaků.

         Za komentář je považována posloupnost znaků ohraničených znaky "{"
    a "}".
    ───────────────────────────────────────────────────────────────────────
    <a href="#t889">Term</a>   <a href="#t891">Predikát</a>   <a href="#t892">Provádění predikátů</a>   <a href="#t888">Identifikátory</a>   <a href="#t890">Proměnné</a>
</pre>

<a name="t889"></a>
## Term

<pre>
                                    <b>Term</b>
    ───────────────────────────────────────────────────────────────────────
         Pojmem term  označujeme datovou  strukturu daného  typu. Může  být
    tvořen konstantou, proměnnou i jejich kombinací.

         Termy  typu Integer, Real, String, LongString, seznamové typy a  z
    nich odvozené typy mohou obsahovat:

    Změna pořadí vyhodnocování : kulaté závorky

    Aritmetické operátory       : +  -  *  /

    Bitové operátory            : ^   negace
                                  ||  disjunkce
                                  &amp;&amp;  konjunkce
         Použití bitových operátorů je omezeno pro typ Integer.

    Zřetězení stringu a seznamu : +

    Výraz typu  String může  obsahovat i  proměnné typu LongString a  výraz
    typu Real může obsahovat i proměnné typu Integer. Platí i obráceně, tj.
    výraz typu  LongString může  obsahovat  proměnné typu String a  výraz
    typu Integer může obsahovat  proměnné typu Real.

Příklad:    
    term typu Integer:     120
              String :     'Auto'
              Real   :     125.624
              Seznam :     [ H|Ostatní ]    
    zřetězení        :     '0.'+'125'

    ───────────────────────────────────────────────────────────────────────
    <a href="#t891">Predikát</a>     <a href="#t892">Provádění predikátů</a>     <a href="#t888">Identifikátory</a>     <a href="#t890">Proměnné</a>
</pre>

<a name="t890"></a>
## Proměnné

<pre>
                                   <b>Proměnné</b>
    ───────────────────────────────────────────────────────────────────────
         Proměnné  jsou určeny  pro uchování  dat.  Identifikátor  proměnné
    začíná  velkým  písmenem.  Proměnná  může  obsahovat  libovolný  datový
    objekt.

        Proměnné   se  předem   nedefinují,  zavádí  se  implicitně  prvním
    použitím.  Typ proměnné  je určen  prvním výskytem. Obsah proměnné  lze
    naplnit pouze jednou, dále jej již nelze modifikovat. Naplnění proměnné
    je  označováno jako  vázání.  Proměnná  jejíž  obsah  nás  nezajímá  je
    označena znakem "_".

         Platnost  svázání  proměnné  je  vždy  jen  v  rámci  jedné  větve
    programu.  Stejný název  v další  větvi označuje  jinou proměnnou.  Při
    zpětném chodu  se vázání  proměnné uvolní  a proměnná  může  být  znovu
    svázána s jinou hodnotou.

         Vázání proměnné lze realizovat i přiřazením ve tvaru:

         Proměnná[<b>:</b>Typ]<b>=</b> Term

    <b>Proměnná</b>  Jméno nevázané proměnné.
    <b>Typ</b>       Typ  proměnné.  Nutno  uvést  pokud  proměnná  nebyla  předem
              deklarovaná.  Deklarovaná   může  být   pouze  jako  výstupní
              parametr v hlavičce predikátu.
    <b>Term</b>      Term obsahující vázené proměnné a konstanty.

Příklad:

     is_:-X:Integer=10+12,write('Součet 10+12 = ',X),wait.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t889">Term</a>   <a href="#t891">Predikát</a>   <a href="#t888">Identifikátory</a>   <a href="#t898">DOMAINS</a>
</pre>

<a name="t891"></a>
## Predikát

<pre>
                                  <b>Predikát</b>
    ───────────────────────────────────────────────────────────────────────
         Zápis programu  i dat  je tvořen posloupností predikátů.  Predikát
    může být  zapsán ve  tvaru faktu  nebo pravidla. Před vlastním  popisem
    predikátů je nutno uvést krátkou zmínku o syntaktickém zápisu.  Logická
    procedura  je  rozdělena  do  odstavců  s  pevně  daným  formátem  (viz
    <a href="#t894">syntaxe</a>).

         Predikáty lze dělit do skupin:
    ■ Databázové  predikáty -  lze  zapsat  pouze  jako  fakt,  z  běžícího
      programu je lze modifikovat.   
    ■ Programové predikáty - mohou být zapsány ve tvaru pravidla nebo faktu.
    ■ Implicitní predikáty - predikáty realizované překladačem.


         Programový predikát je zapsán ve tvaru:

    Definice pravidla :  Hlava:-Tělo.

         Tělo tvoří  posloupnost příkazů  oddělených znakem  ",". Pokud  je
    tělo prázdné lze využít zápisu : Hlava.

         Hlava má následující tvar:

    Identifikátor[(Term{,Term})]

         Provádění  predikátu  je  označováno  jako  volání.  Při  vyvolání
    predikátu   se  nejdříve  substitují  vstupní  parametry  (termy).  Při
    substituci je nutno rozlišit tvar předávaných termů a termů  vyvolaného
    predikátu.
         Mohou nastat následující kombinace:

    ╔═════════════════╦══════════════════════╦════════════════════════════╗
    ║ Volání          ║   Predikát           ║  Popis                     ║
    ╠═════════════════╬══════════════════════╬════════════════════════════╣
    ║ Konstanta       ║   Vstupní prom.      ║  Vázání hodnoty prom.      ║
    ║ Vázaná prom.    ║   Vstupní prom.      ║  Vázání hodnoty prom.      ║
    ║ Nevázaná prom.  ║   Výstupní prom.     ║  Po vyhodn. pred. vázání   ║
    ║ Váz.prom.,konst.║   Výstupní prom.     ║  Test shodnosti po vyhodn. ║
    ║ Váz.prom.,konst.║   Konst.             ║  Test shodnosti            ║
    ╚═════════════════╩══════════════════════╩════════════════════════════╝
                                          
         V případě že nás nezajímá hodnota výstupního parametru zapíšeme do
    volání znak "_".

         Srovnaní  a přiřazení proměnných je nazýváno <b>unifikace</b>.  Vytvoření
    konstantního  termu pomocí  substituce  proměnných  jejich  obsahem  je
    označováno kopírování.

         Při úspěchu  unifikace vstupních  parametrů se zpracuje tělo  (tj.
    pravá strana) predikátu a při zdárném ukončení těla predikátu následuje
    návrat k volajícímu s přiřazením výstupních parametrů - substituce.

         Při neúspěchu unifikace  hlavičky větve  se přejde k další  větvi.
    Není-li další větev, vrátí s neúspěchem.

         Po  neúspěchu a  návratu k předchozímu bodu se v tomto bodu  hledá
    další alternativní řešení, tj. nezačíná se od počátku.

         Metodu zpracování popisuje  strom zpracování.  Je tvořen z uzlů  a
    hran.  Uzel je bod tvořený voláním predikátu. Hrany vycházejí z uzlu  a
    představují větve predikátu. Neúspěch vyvolá návrat do posledního  uzlu
    a hledání další větve.


Příklad:

    Fakty:

    a(1).
    a(2).
    b(2).
    sezn_prvku.

    Pravidlo:

    sezn_prvku:- mem_Firma(X,[]), write(X,'  '),fail.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a>    <a href="#t892">Provádění predikátů</a>    <a href="#t889">Term</a>    <a href="#t890">Proměnné</a>
</pre>

<a name="t892"></a>
## Provádění predikátů

<pre>
                        <b>Provádění - volání  predikátů.</b>
    ───────────────────────────────────────────────────────────────────────
         Posloupnost příkazů A, B, C je ve standardních jazycích  provedena
    postupně  v   uvedeném  pořadí.  Program  je  zapsán  jako  posloupnost
    predikátů. Seznam  je prováděn  z levé  strany a prováděný predikát  je
    vyhledáván  sekvenčně. Při  vyhledávání odpovídající větve se  nejdříve
    porovnávají hlavičky. Při neúspěchu se pokračuje v hledání další větve.
    Při  nalezení pravidla  je prováděno  jeho tělo.  Po úspěšném  nalezení
    faktu se pokračuje zpracováním dalšího příkazu. Pokud vyhovující  větev
    není nalezena jde o situaci nazývanou neúspěch.

         Neúspěch při  volání predikátu  vyvolá návrat  k nejbližšímu  bodu
    větvení  a hledání  další  větve.  Příklad  volání  predikátů  A, B, C:
    Neúspěch predikátu C vyvolá návrat k B a hledání další větve  predikátu
    B,  po splnění  se dále pokračuje hledáním větve predikátu C. Pokud  by
    nebyla nalezena alternativní větev B, je proveden návrat k A. Pokud ani
    v tomto případě nebudou cíle splněny, bude celkový výsledek nesplněn.

         Realizace  strategie vyhledávání  cílů vyžaduje ukládání odkazů  a
    proměnných platné  větve. Nárokovaná  paměť se  uvolní až po návratu  z
    zpracované větve !

Vyjímka:
         Paměť se  při návratu  z predikátu  uvolní pouze  v  případě,  kdy
    volané  predikáty  i  aktuální  volaný  predikát  nemají  nezpracovanou
    alternativní větev.


Příklad:

    Databázové predikáty:

    a(1)
    a(2)
    b(2)

    Programový predikát c je definovaný:

    c:- a(X),b(X),write(X).

         Provádění:

    A-1/ Vyhledávání predikátu a. Vyhledání je úspěšné a  proměnná X vázána
         s hodnotou 1.
    B-1/ Volání predikátu b s  parametrem X1.  Hledání faktu b(1) skončí  s
         neúspěchem a  nastává  návrat  k  poslednímu  bodu  větvení.  
    A-1/ Vyhledávání další  větve  predikátu  a.  Vyhledání  je  úspěšné  a
         proměnná je vázána s hodnotou 2.
    B-2/ Úspěšné hledání .

    W-1/ Implicitní predikát write vypíše na obrazovku obsah proměnné X.


Alternativní příklad (fail - neúspěch):

    a(1)
    a(2)
    b(1)
    b(2)

    c:-   a(X) ,     b(X),  fail.        
               |
         a()   |     b()
    ++++++++++++++++++++
         (1)   |
    ────■────────■── (1)   »
        │      | └── (2)   »
        │(2)   |
        └────────■── (1)   »
               | └── (2)   »

         Predikát fail vyvolá neúspěch a návrat (4 x zpětný chod). Vzhledem
    k neexistenci platné větve bude volání predikátu c neúspěšné.

Další příklad:

    faktorial(1,1).
    faktorial(X,Y*X):- faktorial(X-1,Y).

         Predikát faktorial provádí výpočet faktoriálu a má dva  parametry.
    První parametr je  vstupní, druhý  výstupní. Při vyvolání predikátu  se
    faktorial(2,X)  porovná s  hlavou  faktorial(1,1).  První  parametr  je
    vstupní  proměnná zadána  ve formě  konstanty, což  vyvolá porovnání  s
    výsledkem neúspěch (1&lt;&gt;2). PROLOG pokračuje vyhledáním dalšího  záznamu
    a nalezne  faktorial(X,Y*X). Proměnná  X je  svázána  s  hodnotou  2  a
    výstupní proměnná Y se vyhodnotí až v okamžiku ukončení pravidla.  Tělo
    pravidla   je  tvořeno  rekurzivním  vyvoláním  predikátu  faktorial  s
    parametry (1,Y). Nalezen fakt faktorial(1,1,) následuje porovnání  hlav
    predikátů.  První parametr  zadaný hodnotou  vyvolá  úspěšné  porovnání
    (1=1).  Druhý výstupní  parametr je  svázán  s  konstantou  1.  Úspěšné
    porovnání znamená splnění cíle  a návrat  k předchozímu cíli, který  je
    také splněn. Vyhodnotí se druhý výstupní parametr Y*X (2*1) a předá  se
    hodnota 2.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a> <a href="#t891">Predikát</a>
</pre>

<a name="t894"></a>
## Syntaxe

*Též:* `LProc`

<pre>
                          <b>Syntaxe  PC FAND PROLOGu</b>
    ───────────────────────────────────────────────────────────────────────
         Program  se zapisuje  do kapitoly  typu "L"  a lze  jej vyvolat  z
    procedury pomocí příkazu lproc.

    lproc(Název[,NázevPredikátu])

    Název          Název kapitoly "L".
    NázevPredikátu Název prováděného predikátu. Implicitně "main".

         Po provedení příkazu je nastavena hodnota proměnné edbreak:

         0  úspěch
         1  program skončil neúspěchem (fail)
         2  skok na konec (error)

         Zápis  programu se  podobá zápisu  TURBO PROLOGu a má  následující
    strukturu dělenou do odstavců:

    #<a href="#t898">DOMAINS</a>     Deklarace datových typů
    #<a href="#t899">CONSTANTS</a>   Deklarace konstant
    #<a href="#t901">DATABASE</a>    Deklarace databázových predikátů
    #<a href="#t902">PREDICATES</a>  Deklarace programových predikátu
    #<a href="#t903">CLAUSES</a>     Definice predikátů

         Odstavce  se mohou  vyskytovat i  několikrát v různém pořadí,  ale
    deklarace typu a predikátu  musí být  uvedena vždy před svým  použitím.
    Vyjímku tvoří pouze deklarace typu uvnitř jednoho odstavce DOMAINS.
    ───────────────────────────────────────────────────────────────────────
     <a href="#t889">Term</a>   <a href="#t891">Predikát</a>    <a href="#t892">Provádění predikátů</a>
</pre>

<a name="t898"></a>
## DOMAINS

*Též:* `Výčtový typ`, `Seznam`, `Datové typy`

<pre>
                           <b>#DOMAINS - Definice typů</b>
    ───────────────────────────────────────────────────────────────────────
         Odstavec deklaruje  nové datové  typy. Název  datového  typu  musí
    začínat velkým  písmenem. Současně  lze  definovat  více  typů  (seznam
    oddělený znakem ","). Při definici lze použít i typ deklarovaný později
    v rámci téhož odstavce #DOMAINS a rekurzivní deklarování.

         Zápis deklarace má tvar:

    { NázevTypu{,NázevTypuX} = DeklaraceTypu }

         jde o zkrácený zápis:

    NázevTypu = DeklaraceTypu
    NázevTypuX = NázevTypu

    NázevTypu     Název nově deklarovaného typu.
    NázevTypuX    Název dalšího deklarovaného typu.
    DeklaraceTypu Deklarace nového typu.

    
         Implicitně deklarované typy jsou:

    ┌──────────────┬──────────────────────────┬──────────────────────┐
    │ název        │ min. -max. hodnota       │ popis                │
    ├──────────────┼──────────────────────────┼──────────────────────┤
    │ Integer      │ -32768   až +32767       │ celé číslo           │
    │ Real         │                          │ desetinné číslo      │
    │              │ -2.9e-39 až +1.7e8       │ standardní verze     │
    │              │ -5e-324  až +1.7e308     │ koprocesorová verze  │
    │ Boolean      │ false, true              │ logická hodnota      │
    │ String       │ 0 - 255 znaků            │ řetězec              │
    │ LongString   │ 0 - 65000 znaků          │ dlouhý řetězec       │
    └──────────────┴──────────────────────────┴──────────────────────┘

         Přesnost typu  Real se liší u verze bez koprocesoru (11 míst) a  u
    verze s  koprocesorem (15  míst). Term  typu  LongString  se  ukládá  v
    souboru  FANDWORK.$$$ a práce s  ním bude  pomalejší než u typu  String
    uloženého v operační paměti.
         Konstanty typu  Real a Integer lze zadat i v hexadecimálním  tvaru
    začínajícím znakem "$".


         Odvozený typ.

    NovýTyp = JinýTyp

         Predikáty  pracující s typem JinýTyp budou pracovat i s  odvozeným
    typem NovýTyp.



         Seznam.

    Automaticky pro každý typ Typ je deklarován seznam označený L_Typ.

         Seznam  označujeme pomocí  hranatých závorek  a  jednotlivé  prvky
    oddělujeme znakem  ",". Znakem  "|" se  odděluje hlava od těla  seznamu.
    Výraz na pravé straně je považován za tělo. Hlava obsahuje  vyjmenované
    prvky  seznamu a  tělo je  zbývající část  seznamu, který  může  být  i
    prázdný nebo obsahovat několik prvků. "[]" označuje prázdný seznam.
    
         Při zápisu lze použít tvaru [Prvek1{, Prvekx}|ZbytekSeznamu].

Příklad seznamu:
    ['Alis','Derby','T7']     Seznam termů typu String
    [X|Y]                     Hlava  X, tělo   Y

    Svázání  [X|Y] s hodnotou ['Alis','Derby','T7'] sváže proměnné
    X   s hodnotou   'Alis'
    Y   s hodnotou   ['Derby','T7']


         Výčtový typ.

    VýčtovýTyp =  SeznamTypůOddělStředníkem

         Definujeme seznam hodnot, kterých může typ nabývat.
         

         Struktura.

    StrTyp[{,StrTyp}] = název(SeznamOddělČárkou)

    název               Název funktoru
    SeznamOddělČárkou   Seznam prvků.


         FAND  PROLOG umožňuje  definování  i  složitých  datových  útvarů.
    Struktura je obdobou recordu jazyka PASCAL. Prvkem struktury může být i
    další struktura.

    

Příklad:

    Výčtový typ :  Organiz = alis; kibo
    Odvozený typ:  Firma,Zamestnanec = String
    Struktura   :  Strukt=sez(L_Firma,Real,Strukt)
                                          - rekurzivní odkaz na vlastní typ
    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a>
</pre>

<a name="t899"></a>
## CONSTANTS

<pre>
                       <b>#CONSTANTS - Deklarace konstant</b>
    ───────────────────────────────────────────────────────────────────────
    #CONSTANTS

    NázevTypu: NázevKonst=Term ,{NázevKonst=Term}

    NázevKonst   Identifikátor začínající malým písmenem.

         Odstavec  slouží pro  deklaraci  konstant.  Konstanty  mohou  být
    použity na pozici vázáných proměnných a konstant.
         Deklarace   konstant    oproti   přímému    zápisu  konstanty   do
    zdrojového  kódu projektu  umožňuje snadnou  změnu  hodnoty  konstanty.
    Změnu postačí provést pouze na jednom místě.
    
Příklad:

    #CONSTANTS

    Zamestnanec: ala='Ala',josef='Pepa', ptr='Petr'
    Integer:jedna=1,dve=2, tri=3

    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a>
</pre>

<a name="t901"></a>
## DATABASE

*Též:* `databáze znalostí`

<pre>
                             <b>#DATABASE - Popis dat</b>
    ───────────────────────────────────────────────────────────────────────
    <b>#DATABASE</b> [<b>-</b>JmDatabáze]


    NázevPredikátu[(NázevTypu{,NázevTypu})]

    @NázevPredikátu(NázevÚdaje/NázevTypu[{,NázevÚdaje/NázevTypu}])

         RDB projekt  může obsahovat  více databází. Shodné jméno  databáze
    může být  použito v  několika  "L"  kapitolách.  Pokud  se  bude  obsah
    databáze přenášet vzájemně z několika L kapitol pomocí uložení a obnovy
    databáze  do globální  proměnné musí  souhlasit popis použitých typů  v
    odstavci #<a href="#t898">DOMAINS</a>.

         Kapitola "L" může obsahovat více odstavců #DATABASE.

         Znak  @ naznačuje,  že se jedná o pohled do datového souboru.  Typ
    údaje musí být určen explicitně. Ve FAND PROLOGU může být  zpřístupněna
    pouze část vyjmenovaných údajů datového souboru. Zpřístupněn může být i
    vypočítaný údaj.
         Při   vyhledávání    predikátu   je    vždy  prováděno   sekvenční
    prohledávání.  Hledání v datovém souboru s vhodným klíčem je  prováděno
    pomocí indexu.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a>
</pre>

<a name="t902"></a>
## PREDICATES

<pre>
                                  <b>#PREDICATES</b>
    ───────────────────────────────────────────────────────────────────────
     NázevPredikátu[([&amp;]NázevTypu[{,[&amp;]NázevTypu}])]

     @NázevPredikátu [['ZdrojovýKódProc']] [([&amp;]NázevTypu[{,[&amp;]NázevTypu}])]

         Deklarace hlaviček predikátů.

         Deklarace  predikátů   rozlišuje  vstupní   a  výstupní parametry.
    Výstupní  parametry   jsou  označeny   pomocí  znaku   "&amp;".  Znak   "@"
    označuje proceduru  PC FANDu s parametry typu Integer a Real - ve FANDu
    Real, Boolean  - v  FANDu Boolean  nebo String  a LongString - ve FANDu
    String. Seznamové a záznamové typy budou proceduře předány ve  vnitřním
    tvaru (ve FANDu typ String) a procedura je může uložit do typu string a
    volný text. Za  názvem predikátu  může být uvedena dynamická  deklarace
    predikátu umístěná  v hranatých závorkách.

         Znak  "&amp;" se  skutečně zapisuje.  Při popisu  predikátů je  tohoto
    znaku použito pro označení výstupních termů. Při volání predikátu  není
    znak "&amp;" součástí zápisu.

         Odstavec  definuje pouze hlavičku predikátu. Větev predikátu  musí
    být alespoň jednou uvedena v následujícím odstavci CLAUSES.

         Implicitně je  deklarován predikát "main" bez parametrů, který  je
    implicitně volán při vyvolání příkazu lproc(NázevLKap).

Příklad:

    secti(Integer,Integer,&amp;Integer)

    @upcase['(a:string;var b:string) begin b:=upcase(a); end;'] 
                                                           (String,&amp;String)
    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a>
</pre>

<a name="t903"></a>
## CLAUSES

<pre>
                                   <b>#CLAUSES</b>
    ───────────────────────────────────────────────────────────────────────
         Fakt:
    Predikát[(Term[{,Term})].

         Pravidlo:
    Predikát[(Term{,Term})] :- Instrukce [{,Instrukce}].


         Deklarace    predikátů.   Hlavička    programového  predikátu   je
    deklarovaná  v odstavci  #<a href="#t902">PREDICATES</a> a  pro lokální predikáty může  být
    deklarovaná obdobně i v části CLAUSES následujícím tvarem:

        <b>:</b> DeklaracePredikátu

         Každý programový predikát včetně predikátu "main" musí mít uvedenu
    alespoň jednu větev, ale může jich být i více.

         Databázové predikáty  mohou být  zapsány  pouze  ve  tvaru  faktu.
    Deklarovány  jsou v  odstavci #<a href="#t901">DATABASE</a>.  Při běhu programu lze  pomocí
    predikátů <a href="#t913">assert</a> a <a href="#t914">retract</a> fakt zrušit i doplnit nový.


Příklad:

    sezn_prvku:- mem_Firma(X,[]), write(X,'  '),fail.
    sezn_prvku.

    :is_  is_:-X:Integer=10+12,write('Součet 10+12 = ',X),wait.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a>
</pre>

<a name="t904"></a>
## !

<pre>
                              <b>! - operátor řezu</b>
    ───────────────────────────────────────────────────────────────────────
    !

         Predikát ! je označován jako operátor řezu. Zabraňuje navracení  k
    dalším  variantám před  řezem  -  zpětnému  chodu  tj.  rekurze.  Zákaz
    navracení se uplatňuje pouze na predikát, v kterém je řez uveden.


         Některé  kompilátory testují, zda jde o poslední větev programu. U
    poslední větve  není nutno  uchovávat informace  pro  pozdější  možnost
    navracení, což má  za následek  uvolnění paměti. FAND PROLOG  existenci
    poslední  větve  netestuje.  Při  uvedení  řezu  v  poslední  větvi  se
    neuchovávají návratové  odkazy a  při neúspěchu  se zbytečně  nehledají
    další větve. Programátor takto může zmenšit paměťové nároky i  urychlit
    aplikaci.


Příklad zákazu navracení:

    d:- a(X),!,b(X).
    d.

    e:- xx,d.
         Po  neúspěšném volání predikátu "a" je vyvolán zpětný chod,  který
    by znamenal vyhledání další větve. Při neúspěchu predikátu "b"  možnost
    zpětného chodu již neexistuje a je vyvolán neúspěšný návrat z predikátu
    d. Neúspěch vyvolá hledání další větve predikátu xx.

Příklad "ruční" optimalizace:

    ddd(S,'plus '+R):- S&lt;0 ,int_str(S,R),!.
    ddd(S,'minus '+R):-S&gt;0 ,int_str(S,R),!.
    ddd(_,'nula 0'):-!.

         Predikát  "ddd" má  definovány tři  větve. Jde  o  obdobu  větvení
    realizovaného  v klasickém  jazyce pomocí  case. Splněna  může být  ale
    pouze  jedna větev.  Pro kladná čísla to bude první větev. U  záporných
    druhá  a pro  nulu třetí. U kladného čísla by si bez použití řezu  FAND
    PROLOG  ukládal odkaz  na návratový  bod. Ukládání  odkazu nemá  smysl,
    žádná další vyhovující větev již nemůže být nalezena.

    ───────────────────────────────────────────────────────────────────────
    <a href="#t894">Syntaxe</a>
</pre>

<a name="t905"></a>
## not

<pre>
                                 <b>not - negace</b>
    ───────────────────────────────────────────────────────────────────────
    <b>not(</b>predikát[<b>(</b>Term{<b>,</b>Term}<b>)</b>]<b>)</b>

         Volá uvedený <b>predikát</b>. Při úspěšném návratu z vyvolaného predikátu
    vyvolá zpětný chod. Při neúspěchu predikátu pokračuje další  instrukcí.
    Na pozici výstupních parametrů musí být podtržítko. Výstupní  parametry
    nelze přebírat, přebírání umožňuje později probíraný predikát <a href="#t929">all</a>.

         Negaci  nelze aplikovat  u  standardních  predikátů  write,  save,
    consult, wait, !, fail, error ,trace, assert, retract, loadlex.

Příklad:

    je_v_sezn(X):- mem_String(X,['Alis','Derby','Pussa']).
    neni_v_sezn:- not(je_v_sezn('S')),writeln('Prvek není v seznamu'),wait.
                                                                           
    ───────────────────────────────────────────────────────────────────────
    <a href="#t929">all</a>
</pre>

<a name="t906"></a>
## fail

<pre>
                               <b>fail - neúspěch</b>
    ───────────────────────────────────────────────────────────────────────
    <b>fail</b>

         Predikát fail vyvolá neúspěch - návrat k poslednímu bodu větvení a
    zrušení všech  mezivýsledků mimo  <a href="#t913">assert</a>/<a href="#t914">retract</a> nebo  efektů z  volání
    procedur FANDu.
         Slouží  k   zajištění  průchodu  všemi  větvemi.  Podobně  jako  u
    predikátu <a href="#t905">not</a>  nelze přebírat  výstupní parametry volaných predikátů  -
    při návratu  je proměnná  uvolněna. Uchování  výsledku  lze  realizovat
    pomocí predikátu  <a href="#t913">assert</a>. Tento  způsob je  časově  náročný,  rychlejší
    řešení přinese predikát <a href="#t929">all</a>.

Příklad:


    e:- a(_) , fail.

         Při   provádění  predikátu  "e"  je  vyhledána  struktura  "a".
    Predikát "fail" vyvolá  nesplnění a  návrat zpět. Bude vyhledána  další
    větev "a".  Po nalezení  všech větví  je výsledkem nesplnění cíle.  Cíl
    může být splněn druhou větví predikátu "e".
    ───────────────────────────────────────────────────────────────────────
    <a href="#t909">self</a>    <a href="soubory.md#t397">error</a>
</pre>

<a name="t907"></a>
##  error

<pre>
                    <b>Ukončení provádění L kapitoly - error</b>
    ───────────────────────────────────────────────────────────────────────
    <b>error(</b>Číslo[<b>,</b>ProměnnáInteger]{<b>,</b>ProměnnáString|<b>'</b>KonstantníText<b>'</b>}<b> )</b>
    <b>error(</b>0 [<b>,</b>ProměnnáInteger]{<b>,</b>ProměnnáString|<b>'</b>KonstantníText<b>'</b>}<b>)</b>

         Ukončí zpracování logické procedury, nastaví <a href="jazyk.md#t550">edbreak</a> na hodnotu  2
    a předá následující parametry:

    <a href="jazyk.md#t550">edreckey</a>   Text  zprávy, v  první variantě  načtený ze  zpráv PC  FANDu
               uložených v FANDRES. Číslo načítané zprávy je určeno součtem
               3000 +  <b>Číslo</b>. Parametry  zprávy  budou  čteny  z  parametrů
               <b>KonstantníText</b> a <b>ProměnnáString</b>.
               V druhé  variantě definován  parametrem  <b>KonstantníText</b>  nebo 
               <b>ProměnnáString</b>.
    <a href="procedury.md#t626">exitcode</a>   Implicitně  pozice   z  aktuálního  lexemu,  explicitně  lze
               definovat parametrem <b>ProměnnáInteger</b> .

Příklad:

    main:- error(0,'Chyba .......   ').
    ───────────────────────────────────────────────────────────────────────
    <a href="#t906">fail</a>    <a href="#t305">call</a>
</pre>

<a name="t909"></a>
## self

<pre>
                             <b>self</b>
    ───────────────────────────────────────────────────────────────────────

         Instrukce musí být uvedena poslední ve větvi. Nejdříve provede '!'
    a pak  se předá řízení zpět  k začátku  predikátu, tj k hlavičce  první
    větve.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t910">autorekurze</a> <a href="#t904">!</a>
</pre>

<a name="t910"></a>
## autorekurze

<pre>
                                 <b>autorekurze</b>                                 
    ───────────────────────────────────────────────────────────────────────


         Autorekurzi lze použít u predikátů s prvním parametrem vstupním  a
    neskalárním. V  poslední  větvi  zapíšeme  pouze  hlavičku.  Na  pozici
    prvního parametru  se zadá znak '!', který může být uveden i na  pozici
    výstupního parametru shodného typu.Ostatní výstupní parametry musí také
    párově    odpovídat  jednomu    vstupnímu  parametru.    V  obou   sobě
    odpovídajících  pozicích píšeme  stejný název proměnné. Další  případné
    vstupní pozice vyplníme znakem "_".    

         Predikát  se vyvolává  automaticky rekursivně  pro  každý  subterm
    první  úrovně a  stejného typu  v  termu  prvního  parametru.  Výstupní
    parametr s '!'  se sestavuje  automaticky z dílčích subtermů  získaných
    při  volání. Existuje-li více subtermů pro které se predikát  rekusivně
    volá, pak  mezi jednotlivými  voláními  přenese  výsledky  z  ostatních
    výstupních parametrů do jim odpovídajícím vstupním parametrům.

         <b>Autorekurze pro seznamový typ</b>

         Speciálním případem  je autorekurze  pro seznamový  typ,  v  tomto
    případě má  smysl použít  výstupní parametry  bez párovaných  vstupních
    parametrů. Při autorekurzi s prázdným seznamem se výstupu přiřadí nula,
    prázdný řetězec nebo seznam. Pro jiný typ se předá jen výstup z  volání
    posledního argumentu.

    predikát<b>(</b>...<b>,</b>L<b>):-</b> větev<b>;</b> L<b>+=</b>Term[<b>,</b>self]<b>.</b>

    L  je nepřiřazená proměnná. Při uvedení self se provádí návrat z  konce
    větve pomocí self namísto fail.
    ───────────────────────────────────────────────────────────────────────
</pre>

<a name="t911"></a>
## write*

<pre>
                                  <b>write[ln]</b>
    ───────────────────────────────────────────────────────────────────────
    <b>write</b>[<b>ln</b>]({Proměnná|<b>'</b>TextKonst<b>'</b>}[{<b>,</b>Proměnná|<b>'</b>TextKonst<b>'</b>}]<b>)</b>

         Výpis  textových   konstant,  konstantních   termů  a  přiřazených
    proměnných.  Writeln po  výpisu přejde  na nový  řádek. Umožňuje  výpis
    termu libovolné struktury.

    Příklad:

    maz:- retract(a(X)),write(X).
    ───────────────────────────────────────────────────────────────────────
</pre>

<a name="t912"></a>
## trace

<pre>
                                    <b>trace</b>
    ───────────────────────────────────────────────────────────────────────
    <b>trace(</b>Integer<b>)</b>

    <b>Integer</b>: 0                  Vypnutí trasováni.
             kladná hodnota     Zapnutí trasováni,  hodnota určuje  hloubku
                                trasování.   Např.  při   hodnotě  1   bude
                                trasován     pouze    aktuální    predikát,
                                vyvolávané predikáty již trasovány nebudou.
                                Při hodnotě 2 se trasují dvě úrovně atd.

         Řízení trasování.  Lze jej  vložit do těla libovolného  predikátu.
    Vypisuje  na obrazovku  průběh volání  a návratů  predikátů  a  hodnoty
    parametrů.  Automaticky se  vypíná při  opuštění predikátu v němž  bylo
    definováno.
         Běh trasovaného programu lze přerušit stlačením klávesy "ESC".

    Příklad:

    spoj:- trace(1), concat('A','B','AB'),
           concat('A','B',X), write(X),
           concat('A',Y,'AB'), write(Y),
           concat(Z,'B','AB'), write(Z).
    ───────────────────────────────────────────────────────────────────────
</pre>

<a name="t913"></a>
## assert

<pre>
                                    <b>assert</b>
    ───────────────────────────────────────────────────────────────────────
    <b>assert(</b>predikát[<b>(</b>Term[{<b>,</b>Term}]<b>)</b>]<b>)</b>

    <b>predikát</b>  Vkládaný databázový predikát.
    <b>Term</b>      Konstanta nebo vázaná proměnná.

         Predikát assert ukládá do databáze databázový predikát  definovaný
    jako  parametr. Termy smějí obsahovat jen konstanty a vázané  proměnné.
    Predikát se ukládá na konec databáze.

         Nelze  použít pro  predikáty definované  jako soubory  PC FANDu  s
    indexovou podporou  v cyklu čtení téhož predikátu.

    Příklad:

    uloz:- assert(a(1)), assert(a(2)), assert(b(2)).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t914">retract</a>
</pre>

<a name="t914"></a>
## retract

<pre>
                                   <b>retract</b>
    ───────────────────────────────────────────────────────────────────────
    <b>retract(</b>predikát[<b>(</b>Term{<b>,</b>Term}<b>)</b>]<b>)</b>

    <b>predikát</b>  Jméno odstraňovaného predikátu z databáze.
    <b>Term</b>      Vázaná proměnná a konstanta, nevázaná proměnná a "_".

         Při   použití  vázaných   a  volných   proměnných  odstraní  první
    vyhovující predikát.Termy mohou obsahovat volné proměnné jako  výstupní
    parametry. Při použití "_" odstraní všechny výskyty.


    Příklad:

    maz:- retract(a(X)),write(X).    
    ───────────────────────────────────────────────────────────────────────
    <a href="#t913">assert</a>
</pre>

<a name="t916"></a>
##   save

*Též:* `uložení databáze`

<pre>
                                     <b>save</b>
    ───────────────────────────────────────────────────────────────────────
    <b>save(</b>NázevDatabáze<b>,</b>ParamSoub.UdajTypuT<b>)</b>

    <b>NázevDatabáze</b>       Jméno databáze. Uvedeno za klíčovým slovem #<a href="#t901">DATABASE</a>.
    <b>ParamSoub.UdajTypuT</b> Globální proměnná.

         Uložení současného obsahu databáze do globální proměnné typu T.

    Příklad:

    vloz:-save(FIRMA,PROLPAR.KapB).  
    ───────────────────────────────────────────────────────────────────────
    <a href="#t917">consult</a>
</pre>

<a name="t917"></a>
## consult

<pre>
                                   <b>consult</b>
    ───────────────────────────────────────────────────────────────────────
    <b>consult(</b>NázevDatabáze<b>,</b>ParamSoub.UdajTypuT<b>)</b>

    <b>NázevDatabáze</b>       Jméno databáze. Uvedeno za klíčovým slovem #<a href="#t901">DATABASE</a>.
    <b>ParamSoub.UdajTypuT</b> Globální proměnná.

         Připojení  obsahu databáze  uložené v  globální proměnné typu T  k
    současným     záznamům    databáze.      Obsah    globální     proměnné
    ParamSoub.UdajTypuT musí být uložen pomocí predikátu <a href="procedury.md#t303">save</a>.

         Deklarace  uložené  databáze a databáze,  k níž je obsah připojen,
    musí  být   totožná.  Není  prováděna  úplná  kontrola  shodnosti.  Při
    nesouladu může  dojít  k  zhroucení  systému.  Pokud  provádíte  přenos
    databáze z  jedné logické  procedury do  druhé  doporučujeme  deklaraci
    konstant, typů a databáze umístit do vkládané kapitoly {$<a href="#t267">Include</a> ...}.

       Pozor - údaj může být chybně naplněn pomocí prostředků FANDu  (např.
    přímý přístup) nekorektní hodnotou !

    Příklad:

    obnov:-consult(FIRMA,PROLPAR.KapB).
    ───────────────────────────────────────────────────────────────────────
    <a href="procedury.md#t303">save</a>
</pre>

<a name="t918"></a>
## OperaceSrovnání

<pre>
                               <b>Srovnání  termů</b>
    ───────────────────────────────────────────────────────────────────────
    Proměnná <b>OperaceSrovnání</b> Term

    <b>Proměnná</b>           Vázaná proměnná shodného typu jako <b>Term</b>.
    <b>Term</b>               Výraz neobsahující nevázané proměnné.
    <b>OperaceSrovnání</b>    Pro typ Real a Integer:  &lt; , &lt;=, &gt;, &gt;=, =, &lt;&gt;
                       Pro ostatní typy      :  =, &lt;&gt;

         Porovnávat  lze výrazy obsahující konstanty a přiřazené  proměnné.
    Nesplnění podmínky vyvolá neúspěch a zpětný chod.

    Příklad:

    comp:- a(X), X &lt; 5.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t891">Predikát</a>    <a href="#t892">Provádění predikátů</a>
</pre>

<a name="t919"></a>
## concat

<pre>
                                   <b>concat</b>
    ───────────────────────────────────────────────────────────────────────
    <b>concat(</b>[<b>&amp;</b>]Term1<b>,</b>[<b>&amp;</b>]Term2<b>,</b>[<b>&amp;</b>]Term3<b>)</b>

    <b>Term</b>      Term typu String.

          Spojení  nebo porovnání textových termů. Porovnávání založeno  na
    výrazu  Term1+Term2=Term3.   Dle  kombinace   vstupních  a   výstupních
    proměnných lze použít pro porovnávání, spojení i výběr řetězce.

         Umožněny jsou následující kombinace parametrů (i=vstup o=vystup):
    (i,i,i)        - test
    (i,i,o)        - spojení
    (o,i,i)        - výběr řetězce
    (i,o,i)        - výběr řetězce

    Příklad:

    spoj:- trace(1),concat('A','B','AB'),
           concat('A','B',X), write(X),
           concat('A',Y,'AB'), write(Y),
           concat(Z,'B','AB'), write(Z).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t925">getlex</a>
</pre>

<a name="t920"></a>
## funkce v L kapitole

<pre>
                                   <b>funkce</b>
    ───────────────────────────────────────────────────────────────────────

                             <b>Funkce typu Integer</b>

    <b>max(</b>Integer_1<b>,</b>Integer_2<b>)</b> Funkce vracející maximum.
    <b>min(</b>Integer_1<b>,</b>Integer_2<b>)</b> Funkce vracející minimum.
    <b>val(</b>String<b>)</b>              Převod čísla z typu String do typu Integer.
    <b>length(</b>String<b>)</b>           Délka řetězce.
    <b>pos(</b>String_1<b>,</b>String_2<b>)</b>   Pozice podřetězce String_1 v řetězci String_2.
    <b>maxcol</b>                   Počet sloupců obrazovky.
    <b>maxrow</b>                   Počet řádků obrazovky.
                                                  
                              <b>Funkce typu String</b>

    <b>copy(</b>String<b>,</b>Integer_1<b>,</b>Integer_2<b>)</b> Výběr  podřetězce z řetězce String  od
                                     pozice Integer_1 v délce Integer_2.
    <b>str(</b>Integer<b>)</b>                     Převod z typu Integer do String.
    <b>repeatstr(</b>String<b>,</b>Integer<b>)</b>        Opakování řetězce String Integer  krát.
    <b>leadchar(</b>ZnakKonst<b>,</b>String<b>)</b>·······Odstranění úvodních znaků ZnakKonst  z
                                     řetězce String.
    <b>trailchar(</b>ZnakKonst<b>,</b>String<b>)</b>······Odstranění     ukončujících      znaků
                                     ZnakKonst z řetězce String.


    <b>Integer</b>, <b>Integer_1</b>, <b>Integer_2</b>
               Termy typu Integer neobsahující nepřiřazené proměnné.
    <b>String</b>, <b>String_1</b>, <b>String_2</b>
               Termy typu String neobsahující nepřiřazené proměnné.
    <b>ZnakKonst</b>  Znaková konstanta.

    Příklady:

    vetsi:- X:Integer=max(15,10),writeln(' 10  a 15   =&gt; větší je',X).
    mensi:- X:Integer=min(10,105), writeln('Min: ',X).
    fce1:- X:Integer=val(str(2)+copy('0001',1,3))+length('abc')*100 + pos('d','fff d'), writeln(X),
          Mc:Integer=maxcol, Mr:Integer=maxrow,
          write('Obrazovka : ',Mc,' x ',Mr).
    fce2:- X:String=repeatstr('d',2)+'X',Y:String=leadchar('d',X), writeln(Y).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t891">predikát</a>    <a href="#t889">term</a>    <a href="#t890">proměnné</a>
</pre>

<a name="t921"></a>
## union

<pre>
                                    <b>union_</b>
    ───────────────────────────────────────────────────────────────────────
    <b>union_</b>NázevTypu<b>(</b>L_NázevTypu_1<b>,</b>L_NázevTypu_2<b>,</b>&amp;L_NázevTypu_3<b>)</b>

    <b>L_NázevTypu_1</b>    Vstupní seznam, ke kterému budou připojovány nové prvky.
    <b>L_NázevTypu_2</b>    Připojované prvky.
    <b>L_NázevTypu_3</b>    Výsledný seznam.

    
         Připojí k  prvnímu seznamu  L_NázevTypu_1 prvky z druhého  seznamu
    L_NázevTypu_2,  které v prvním seznamu neexistují a výsledek je  vrácen
    ve výstupním parametru L_NázevTypu_3.

    Příklad:

    uni:-union_String(['A'],['A','B'],X),writeln(X),
         union_String(['B','A'],['A','B','C'],Y),writeln(Y).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t922">minus</a>    <a href="#t923">inter</a>
</pre>

<a name="t922"></a>
## minus

<pre>
                                    <b>minus_</b>
    ───────────────────────────────────────────────────────────────────────
    <b>minus_</b>NázevTypu<b>(</b>L_NázevTypu_1<b>,</b>L_NázevTypu_2<b>,</b>&amp;L_NázevTypu_3<b>)</b>

    <b>L_NázevTypu_1</b>    Vstupní seznam.
    <b>L_NázevTypu_2</b>    Odstraňované prvky.
    <b>L_NázevTypu_3</b>    Výsledný seznam.

    
         Vynechá z  prvního seznamu  L_NázevTypu_1 všechny členy, které  se
    vyskytují i  v druhém  seznamu L_NázevTypu_2  a výsledek  je vrácen  ve
    výstupním  parametru L_NázevTypu_3.  Prvky druhého  seznamu,  které  se
    nevyskytují v prvním jsou ignorovány.

    Příklad:

    mi:-minus_String(['A'],['A','B'],X),writeln(X),
         minus_String(['B','A'],['A'],Y),writeln(Y).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t923">inter</a>    <a href="#t921">union</a>
</pre>

<a name="t923"></a>
## inter

<pre>
                                    <b>inter_</b>
    ───────────────────────────────────────────────────────────────────────
    <b>inter_</b>NázevTypu<b>(</b>L_NázevTypu_1<b>,</b>L_NázevTypu_2<b>,</b>&amp;L_NázevTypu_3<b>)</b>

    <b>L_NázevTypu_1</b>    První vstupní seznam.
    <b>L_NázevTypu_2</b>    Druhý vstupní seznam.
    <b>L_NázevTypu_3</b>    Výsledný seznam.

    
         Prvky  prvního seznamu,  které existují  i v  druhém seznamu  jsou
    přeneseny do výstupního seznamu L_NázevTypu_3.

    Příklad:

    intr:-inter_String(['A'],['A','B'],X),writeln(X),
         inter_String(['B','A'],['a'],Y),writeln(Y).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t922">minus</a>    <a href="#t921">union</a>
</pre>

<a name="t924"></a>
## loadlex

<pre>
                                    <b>loadlex</b>
    ───────────────────────────────────────────────────────────────────────
    <b>loadlex(</b>ParamSoub.ÚdajTypuT<b>)</b>

         Čte obsah  globální proměnné  typu  T,  provede  lexikální  rozbor
    a vytvoří interně  uložený seznam  L_Lexem. Domain Lexem je  implicitně
    deklarovaný s následující strukturou:

    <b>Lexem  = lex(Integer_Pos,Integer_Typ,String)</b>
       <b>Integer_Pos</b>  Pozice v textu.
       <b>Integer_Typ</b>  Typ : 0  oddělovač + ostatní
                          1  slovo tvořené písmeny
                          2  identifikátor začínající písmenem a obsahující
                             písmena, číslice a znak "_"
                          3  celé číslo
       <b>String</b>       Vlastní lexem

         Bílá  místa   tvořená  posloupností   znaků  ' '  a  komentáři  se
    vynechávají a  slouží pouze  k oddělení lexemu. Neukončené  komentářové
    závorky vyvolávají na konci textu chybu error(503).

         Závěr seznamu tvoří vždy dodatečný prvek lex(PoziceKonce,0,'').

         Vytvořený  interně    uložený  seznam    lze  zpracovávat   pomocí
    implicitních predikátů  <a href="#t926">nextlex</a>  a  <a href="#t925">getlex</a>.  Tyto  predikáty  se  smějí
    používat  ve větvi  po  loadlex.  V  opačném  případě  hrozí  zhroucení
    programu.

    Příklad:

    vyb_slov:- trace(1),loadlex(PROLPAR.Slova),
               getlex(X),writeln('Term celkem:',X),
               nextlex,getlex(Y),writeln('Term zkr.:',Y).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t926">nextlex</a> <a href="#t925">getlex</a>
</pre>

<a name="t925"></a>
## getlex

<pre>
                                    <b>getlex</b>
    ───────────────────────────────────────────────────────────────────────
    <b>getlex(</b>Lexem<b>)</b>

         Vrací aktuální  seznam  lexemů  z  interní  paměti.  Predikát  lze
    použít pouze ve větvi po loadlex.


    <b>Lexem</b>  Term typu L_Lexem.
    
    ───────────────────────────────────────────────────────────────────────
    <a href="#t926">nextlex</a> <a href="#t924">loadlex</a>
</pre>

<a name="t926"></a>
## nextlex

<pre>
                                   <b>nextlex</b>
    ───────────────────────────────────────────────────────────────────────
    <b>nextlex</b>

         Zkrátí seznam  lexemů v  interní paměti vypuštěním prvního  prvku.
    Predikát lze použít pouze ve větvi po loadlex.

    ───────────────────────────────────────────────────────────────────────
    <a href="#t925">getlex</a> <a href="#t924">loadlex</a>
</pre>

<a name="t927"></a>
## abbrev

<pre>
                                   <b>abbrev</b>
    ───────────────────────────────────────────────────────────────────────
    <b>abbrev(</b>String_1<b>,</b>&amp;String_2<b>)</b>

    <b>String_1</b>    Vstupní string tvořený posloupností slov.
    <b>String_2</b>    Výstupní string tvořený posloupností zkrácených slov.

         Provádí zkrácení  slov  Vstupního  parametru  String_1,  tvořeného
    posloupností  slov. Slova  se zkracují  tak, že se vynechávají  písmena
    po poslední  souhlásce po níž následuje jen jedna skupina samohlásek  a
    jedna skupina souhlásek. Za vytvořenou zkratku je umístěna tečka. Pokud
    nelze string zkrátit vynechají se alespoň ukončující mezery.

    Příklad:

    abbr:-abbrev('podvojné účetnictví',X),writeln(X).
    ───────────────────────────────────────────────────────────────────────
</pre>

<a name="t928"></a>
## add

<pre>
                <b>Vyhledání, případně doplnění prvku do seznamu</b>
    ───────────────────────────────────────────────────────────────────────
    <b>add_</b>NázevTypu<b>(</b>Prvek<b>,</b>Seznam_Vstup<b>,</b>&amp;Seznam_Výstup<b>)</b>

    <b>Prvek</b>          Hledaný prvek v seznamu typu NázevTypu.
    <b>Seznam_Vstup</b>   Vstupní seznam prvků typu L_NázevTypu.
    <b>Seznam_Výstup</b>  Výstupní seznam obsahující zařazený prvek.


         Predikát vyhledává term v seznamu. Při nalezení je výstupní seznam
    kopie vstupního seznamu. Pokud není nalezen je prvek doplněn na začátek
    seznamu.

    Příklad:

    hl_prvek:-add_String('A',['A','B'],X),add_String('A',['B'],Y),
              write('X:',X,'    Y:',Y).

         V prvním volání je prvek 'A' v seznamu nalezen (X=['A','B']) , ve 
    druhém případě nalezen není a je do seznamu doplněn (Y=['A','B']) .
    ───────────────────────────────────────────────────────────────────────
    <a href="#t932">len</a>_ <a href="#t929">all</a>_ <a href="#t933">mem</a>_ <a href="#t930"> del</a>_ <a href="#t931">inv</a>_
</pre>

<a name="t929"></a>
## all

<pre>
                          <b>Cyklické volání predikátu</b>
    ───────────────────────────────────────────────────────────────────────
    <b>all_</b>NázevTypu<b>(</b>Predik<b>(</b>[&amp;]Term{<b>,</b>[&amp;]Term}<b>),</b>
                            Výsl<b>,</b> &amp;Seznam_Výsl<b>)</b>

    <b>Predik(Term{,Term})</b> Vyvolávaný predikát.
    <b>Výsl</b>                Term zapisovaný do seznamu.
    <b>Seznam_Výsl</b>         Výstupní seznam tvořený z termů Výsl.

         Umožňuje   cyklické  volání   predikátů  a   sestavení  seznamu  z
    výstupních parametrů.
         Predikát Predik je opakovaně vyvoláván až do okamžiku nesplnění  -
    fail. Po úspěšném návratu  predikátu Predik  je vyhodnocen term Výsl  a
    zařazen do seznamu Seznam_Výsl.

         Opakované volání lze naprogramovat i pomocí implicitního predikátu
    fail.  Záporem naprogramovaného  cyklu je  nemožnost předat  přiřazenou
    proměnnou. Při zpětném chodu je výsledek predikátů mimo efektu  <a href="#t913">assert</a>,
    <a href="#t914">retract</a> a procedur FANDu anulován.

    Příklad:

    cykl:- all_String(fandfile(X,_,_,_),X+' &lt;--' ,Y),writeln(Y).

         Cyklické   vyvolávání  predikátu  fandfile.  K  jménu  souboru  je
    připojen  ukazatel a výsledek je zařazen do seznamu, který je na  závěr
    vypsán.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t932">len</a>_ <a href="#t928">add</a>_ <a href="#t933">mem</a>_ <a href="#t930"> del</a>_ <a href="#t931">inv</a>_
</pre>

<a name="t930"></a>
##  del

<pre>
                           <b>Zrušení prvku v seznamu</b>
    ───────────────────────────────────────────────────────────────────────
    <b>del_</b>NázevTypu<b>(</b>[&amp;]Prvek<b>,</b>Seznam_Vstup<b>,</b>[&amp;]Seznam_Výst<b>)</b>


    <b>Prvek</b>               Odstraňovaný prvek.
    <b>Seznam_Vstup</b>        Vstupní seznam.
    <b>Seznam_Výst</b>         Výstupní seznam bez odstraněného prvku.


         Predikát odstraní  první nalezený  prvek ze  vstupního  seznamu  a
    seznam uloží do výstupního seznamu.

         Umožněna následující  kombinace parametrů (i=vstup o=vystup):

    (i,i,i)        - test seznamů
    (i,i,o)        - odstranění prvku ze seznamu
    (o,i,i)        - predikát splněn při existujícím vhodném prvku
    (o,i,o)        - postupné odstraňování jednoho prvku seznamu

    Příklad:

    zrus_s1:-del_String(S,['A','B','C','A'],X), write(S,X), fail.
    zrus_s1.

    zrus_s2:-del_String('A',['A','B','C','A'],X), write(X).

    zrus_s3:-del_String('A',['A','B'],['B']), write('Prvek odstraněn !'), !.
    zrus_s3:-write('Prvek nebyl v seznamu nalezen').

    ───────────────────────────────────────────────────────────────────────
    <a href="#t932">len</a>_ <a href="#t928">add</a>_ <a href="#t933">mem</a>_ <a href="#t931">inv</a>_ <a href="#t929">all</a>_
</pre>

<a name="t931"></a>
## inv

<pre>
                               <b>Inverze seznamu</b>
    ───────────────────────────────────────────────────────────────────────
    <b>inv_</b>NázevTypu<b>(</b>Seznam_Vstup<b>,</b> &amp;Seznam_Výstup<b>)</b>

    <b>Seznam_Vstup</b>        Vstupní seznam typu NázevTypu.
    <b>Seznam_Výstup</b>       Invertovaný seznam.

         Prvky vstupního  seznamu uloží  do výstupního  seznamu  v  opačném
    pořadí.

    Příklad:

    inverze:-inv_Integer([1,12,15,19,259,10000],X), writeln(X).

    ───────────────────────────────────────────────────────────────────────
    <a href="#t932">len</a>_ <a href="#t928">add</a>_ <a href="#t933">mem</a>_ <a href="#t930"> del</a>_ <a href="#t929">all</a>_
</pre>

<a name="t932"></a>
## len

<pre>
                            <b>Zjištění délky seznamu</b>
    ───────────────────────────────────────────────────────────────────────
    <b>len_</b>NázevTypu<b>(</b>Seznam<b>,</b> &amp;Integer<b>)</b>

    <b>Seznam</b>    Vstupní seznam tvořený prvky typu NázevTypu.
    <b>Integer</b>   Výstupní hodnota určující počet prvků v seznamu.


         Spočítá  počet  prvků  v  seznamu.  Je  rychlejší  než  uživatelem
    definovaný predikát provádějící sečtení počtu prvků.

    Příklad:

    delka_sezn:- len_String(['ff','f'],Del), write(Del).
    ───────────────────────────────────────────────────────────────────────
        <a href="#t932">len</a>_ <a href="#t928">add</a>_ <a href="#t933">mem</a>_ <a href="#t930"> del</a>_ <a href="#t929">all</a>_
</pre>

<a name="t933"></a>
## mem

<pre>
                              <b>Prvek seznamu</b>
    ───────────────────────────────────────────────────────────────────────
    <b>mem_</b>NázevTypu<b>(</b>[&amp;]TermNázev<b>,</b>TermL_Název<b>)</b>

    <b>TermNázev</b>    Term typu NázevTypu.
    <b>TermL_Název</b>  Term typu L_NázevTypu, tj. seznam termů typu NázevTypu.

         Podmínkou činnosti je podmínka odpovídajícího pojmenování typů.  K
    uživatelskému  typu  je  automaticky  definován  typ  seznam  s  jménem
    doplněným předponou "L_".

         Funkčnost   predikátu  je  určena  typem  prvního  parametru.  Při
    vstupním  parametru zjišťuje, zda je term prvkem seznamu. U  výstupních
    parametrů   postupně  unifikuje   všechny  prvky   seznamu  s  uvedenou
    proměnnou.


    Příklad:

    Firma,Zamestnanec = String

    { Test  existence prvku v seznamu }
    je_prvek:- mem_Firma('Alis',['Alis','Derby','Pussa']),
               write('Je prvkem seznamu'),!.
    je_prvek:- write('Neni prvkem seznamu').

    { Příklad  výpisu seznamu }

    sezn_prvku:- mem_Firma(X,['Alis','Derby','Pussa']), write(X,'  '),fail.
    sezn_prvku.

    ───────────────────────────────────────────────────────────────────────
    <a href="#t932">len</a>_ <a href="#t928">add</a>_ <a href="#t930"> del</a>_ <a href="#t931">inv</a>_ <a href="#t929">all</a>_
</pre>

<a name="t934"></a>
## fandfile

<pre>
                                   <b>fandfile</b>
    ───────────────────────────────────────────────────────────────────────
    <b>fandfile(</b>[&amp;]String_Soubor<b>,</b>&amp;String_Typ<b>,</b>&amp;String_Rdb<b>,</b>&amp;String_Cesta<b>)</b>

    <b>String_Soubor</b>   Logické jméno souboru PC FANDu.
    <b>String_Typ</b>      Typ souboru:  ''    - standardní soubor
                                  'SQL' - SQL tabulka
                                  'X'   - s podporou indexů
                                  'DBF' - DBF soubor
                                  'DTA' - formát 8-bitového FANDu.
    <b>String_Rdb</b>      Úloha v které je soubor definován.
    <b>String_Cesta</b>    Úplná cesta k souboru.

    <a href="#t940">vstupní_parametry</a>

         Zjištění  informací  o  popisu  souborů  projektu.  Popis  dalších
    souborů  je vrácen při prohledávání další větve. Seznam popisů  souboru
    lze vytvořit vyvoláním pomocí predikátu <a href="#t929">all</a>.

         Vrácen je popis  všech viditelných  souborů, tj. včetně souborů  z
    nadřízených   úloh.    Překryté  soubory    nadřízených  úloh   nebudou
    zpřístupněny.

    

    Příklad:

    soubor:-trace(1),fandfile(Soubor,'X','PROLOG',Cesta),
            write('Soubor:',Soubor,'    Cesta:',Cesta),fail.

         Výpis indexovaných souborů projektu "PROLOG".
    ───────────────────────────────────────────────────────────────────────
    <a href="#t935">fandfield</a> <a href="#t936">fandkey</a> <a href="#t937">fandkeyfield</a> <a href="#t938">fandlink</a> <a href="#t939">fandlinkfield</a>
</pre>

<a name="t935"></a>
## fandfield

<pre>
                                  <b>fandfield</b>
    ───────────────────────────────────────────────────────────────────────
    <b>fandfield(</b>String_Soubor<b>,</b>[&amp;]String_Název<b>,</b>&amp;String_Typ<b>,</b>&amp;Integer_Delka
         <b>,</b>&amp;Integer_DesM<b>,</b>&amp;Integer_Flags<b>,</b>&amp;String_Maska<b>)</b>

    <b>String_Soubor</b>  Název souboru.
    <b>String_Název</b>   Název údaje.
    <b>String_Typ</b>     Typ údaje ('F','A','N','D','B','T','R').
    <b>Integer_Delka</b>  Délka, resp. počet míst před desetinnou tečkou.
    <b>Integer_DesM</b>   Počet míst za desetinnou tečkou.
    <b>Integer_Flags</b>  Maska s následujícím významem bitů:
                   1 - uložen (1)
                   2 - zakódován (2)
                   3 - s maskou (4)
                   4 - myšlená čárka (8)
                   5 - zarovnání vlevo pro typy A,N (16)
    <b>String_Maska</b>   Maska údaje. U údajů bez masky vrací ''.

    <a href="#t940">vstupní_parametry</a>

         Zjištění  jména a typu údaje určeného souboru včetně popisu  masky
    údaje.
         Maska  obsahuje binárně kódovanou informaci. Například při  výběru
    uložených  údajů je nutno vybrat  údaje s  hodnotou prvního bitu 1,  tj
    liché hodnoty.  Zjištění  obsahu  bitu  lze  provést  pomocí  binárního
    operátoru &amp;&amp;. 

    Příklad:

    udaj:-fandfield('HLP',Název,Typ,_,_,_,_),write(Název,Typ).
    ───────────────────────────────────────────────────────────────────────
    <a href="#t934">fandfile</a> <a href="#t936">fandkey</a> <a href="#t937">fandkeyfield</a> <a href="#t938">fandlink</a> <a href="#t939">fandlinkfield</a>
</pre>

<a name="t936"></a>
## fandkey

<pre>
                                   <b>fandkey</b>
    ───────────────────────────────────────────────────────────────────────
    <b>fandkey(</b>String_Soubor<b>,</b>&amp;String_Klíč<b>,</b>&amp;Boolean_Int<b>,</b>
         &amp;Boolean_Dupl<b>)</b>

    <b>String_Soubor</b>  Název souboru.
    <b>String_Klíč</b>    Název vlastního klíče. Primární klíč označen "@".
    <b>Boolean_Int</b>    Intervalový klíč.
    <b>Boolean_Dupl</b>   Duplicitní klíč.

    <a href="#t940">vstupní_parametry</a>

         Zjistí  názvy a typy vlastních  klíčů.

    Příklad:

    klic:-fandkey('ZAMESTN',Klic,_,_),
          fandkeyfield('ZAMESTN',Klic,Udaj,_,_), write(Klic,Udaj).

         Výpis prvního údaje prvního klíče.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t935">fandfield</a> <a href="#t934">fandfile</a> <a href="#t937">fandkeyfield</a> <a href="#t938">fandlink</a> <a href="#t939">fandlinkfield</a>
</pre>

<a name="t937"></a>
## fandkeyfield

<pre>
                                 <b>fandkeyfield</b>
    ───────────────────────────────────────────────────────────────────────
    <b>fandkeyfield(</b>String_Soubor<b>,</b>String_Klíč<b>,</b>&amp;String_Údaj<b>,</b>
         &amp;Boolean_Lex<b>,</b>&amp;Boolean_Sest<b>)</b>

    <b>String_Soubor</b>  Název souboru.
    <b>String_Klíč</b>    Název vlastního klíče.
    <b>String_Údaj</b>    Název údaje.
    <b>Boolean_Lex</b>    Lexikální klíč.
    <b>Boolean_Sest</b>   Sestupné třídění.

    <a href="#t940">vstupní_parametry</a>

         Zjistí jméno údaje tvořícího primární klíč a typ třídění.

    Příklad:

    klic:-fandkey('ZAMESTN',Klic,_,_),
          fandkeyfield('ZAMESTN',Klic,Udaj,_,_), write(Klic,Udaj).

         Výpis prvního údaje prvního klíče.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t935">fandfield</a> <a href="#t934">fandfile</a> <a href="#t936">fandkey</a> <a href="#t938">fandlink</a> <a href="#t939">fandlinkfield</a>
</pre>

<a name="t938"></a>
## fandlink

<pre>
                                   <b>fandlink</b>
    ───────────────────────────────────────────────────────────────────────
    <b>fandlink(</b>String_Soubor<b>,</b>[&amp;]String_Spojení<b>,</b>[&amp;]String_Soub<b>,</b>
            &amp;String_Klíč<b>,</b>&amp;String_Own<b>,</b>&amp;Integer_Fl<b>)</b>

    <b>String_Soubor</b>       Název podřízeného souboru.
    <b>String_Spojení</b>······Název  spojení do nadřízeného souboru. Může to  být
         jméno role, klíče, nebo nadřízeného souboru.
    <b>String_Soub</b>         Jméno nadřízeného souboru.
    <b>String_Klíč</b>         Název klíče v nadřízeném souboru.
    <b>String_Own</b>··········Název  vlastního   klíče,  který   lze  použít  pro
         vytvoření vazby owner z nadřízeného souboru do podřízeného.
    <b>Integer_Fl</b>          Maska s následujícím významem bitů:
                        1 - referenční integrita  !      (1)
                        2 - referenční integrita  !!     (2)
                        3 - možno vytvořit owner spojení (4)

    
         Predikát   vyhledává  cizí  klíče  v  uvedeném  souboru.  Výstupní
    parametry popisují uvedený klíč.

         Podporovány   jsou  následující  dvě  kombinace  vstupních  (i)  a
    výstupních (o) parametrů: (i,i,o,o,o,o) a (i,o,i,o,o,o)


    Příklad:

    c_klic:-fandlink('UZIV',Spoj,Soub,Klíč,_,_),
            fandlinkfield('UZIV',Spoj,Udaj),
            write(Soub,Klíč,Udaj).

         Vyhledá  první   údaj  prvního  cizího  klíče  souboru  "UZIV".
    ───────────────────────────────────────────────────────────────────────
    <a href="#t935">fandfield</a> <a href="#t934">fandfile</a> <a href="#t936">fandkey</a> <a href="#t937">fandkeyfield</a> <a href="#t939">fandlinkfield</a> <a href="#t940">vstupní_parametry</a>
</pre>

<a name="t939"></a>
## fandlinkfield

<pre>
                                <b>fandlinkfield</b>
    ───────────────────────────────────────────────────────────────────────
    <b>fandlinkfield(</b>String_Soub<b>,</b>String_Spoj<b>,</b>&amp;String_Údaj<b>)</b>

    <b>String_Soub</b>         Jméno podřízeného souboru.
    <b>String_Spoj</b>         Název spojení.
    <b>String_Údaj</b>         Jméno klíčového údaje.

    <a href="#t940">vstupní_parametry</a>

          Vyhledá klíčové údaje určeného cizího klíče podřízeného souboru.

   Příklad:

    c_klic:-fandlink('UZIV',Spoj,Soub,Klíč,_,_),
            fandlinkfield('UZIV',Spoj,Udaj),
            write(Soub,Klíč,Udaj).

         Vyhledá  první údaj  prvního cizího  klíče  z  souboru  "UZIV"  do
    nadřízeného souboru.
    ───────────────────────────────────────────────────────────────────────
    <a href="#t935">fandfield</a> <a href="#t934">fandfile</a> <a href="#t936">fandkey</a> <a href="#t937">fandkeyfield</a> <a href="#t938">fandlink</a>
</pre>

<a name="t940"></a>
## vstupní_parametry

<pre>
                     <b>Vstupní parametry predikátů fand...</b>
    ───────────────────────────────────────────────────────────────────────

         U vstupních  parametrů  predikátů  fandfield,  fandfile,  fandkey,
    fandkeyfield a  fandlink zadaných vázanou proměnnou nebo konstantou  se
    neprovádí rozlišování  malých a  velkých písmen. Tento způsob  odpovídá
    konvenci identifikátorů v PC FANDu.

        Na  pozici výstupních  parametrů se  provádí  standardní  unifikace
    parametrů, tj. vždy dochází k rozlišení malých a velkých písmen.

    ───────────────────────────────────────────────────────────────────────
    <a href="#t935">fandfield</a> <a href="#t934">fandfile</a> <a href="#t936">fandkey</a> <a href="#t937">fandkeyfield</a> <a href="#t938">fandlink</a>
</pre>
