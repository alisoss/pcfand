Nezverejnena oprava - uprava, 
zvyseni hranice pro sdileni datovych souboru.

prosinec 2007, byla prekrocena hranice pro fyzickou velikost sdileneho souboru 160 MB.
A000501H , viz. zdrojak FileAcc.PAS, konstanty 

const TransLock=$0A000501;  { locked while state transition }
      ModeLock =$0A000000;  { base for mode locking }
      RecLock  =$0B000000;  { base for record locking }

Oprava spociva ve zvyseni techto konstant.

Upravenou verzi testovalo nekolik zajemcu, nebyly problemy. 
Ale jeji zverejneni by si vynutilo zvyseni verze FANDu, i kdyz jinak zbytecne. Slo o nekompatibilitu ve sdileni souboru na vsechny dosavadni verze FANDu (do 4.2 vcetne)
