# PC FAND 4.2x

PC FAND je původní české prostředí pro vývoj databázových aplikací v prostředí DOS z dílny firmy ALIS Česká Lípa. Po ukončení podpory DOS prostředí byl PC FAND dán k dispozici komunitě ve formě open source pod MIT licencí.

## Poslední verze

Poslední verzí vývojového prostředí PC FAND, která byla distribuovaná komerčními kanály, byla verze 4.2. Verze 4.2x obsahuje dodatečné úpravy verze 4.2 pro fungování v prostředí VDOS (https://www.vdos.info) a další opravy.

## Adresářová struktura

* BP7 - nastavení prostředí
* FANDCFG - zdroje pro konfigurační soubor FAND.CFG
* FANDRES - zdroje pro zdrojový soubor FAND.RES, překlady hlášek do němčiny a angličtiny
* HELP - nápověda pro programátorský i uživatelský runtime
* PAS - zdroje PC FANDu

## Překlad

PC FAND byl vyvíjen a překládán do binárního tvaru pomocí vývojového prostředí Borland Pascal 7.

## Úpravy knihoven

Z důvodu úpravy práce s HEAPem (shora a zezdola stack, uprostřed heap), využití paměti grafické karty a realizace konstruktu "long jump" podle vzoru z jazyka C++ byly upraveny některé runtime knihovny Borland Pascalu. Z tohoto důvodu je sice možné zdrojáky PC FANDu přeložit, ale výsledná aplikace nepůjde spustit.

## Poznámky

* V adresáři `/PAS/Mb160` je nezveřejněná úprava, která zvyšuje limit fyzické velikosti sdíleného datového souboru `.000` nad 160 MB.

## Licence

PC FAND je uvolněn pod MIT licencí.
Copyright (c) 1990 - 2018 ALIS spol. s r.o. Česká Lípa