000100 01  W232232.                                                             
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 KDBEHX               PIC X.                                       
000500      88 ORDER               VALUE 'O'.                                   
000600      88 ANNULLATION         VALUE 'A'.                                   
000700      88 MAN-JUST-PLUS       VALUE 'P'.                                   
000800      88 MAN-JUST-MINUS      VALUE 'M'.                                   
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 KDOI                 PIC S9(3)           COMP-3.                  
001200      88 OING-PROGNOSPAV-C1  VALUE +11.                                   
001300      88 OING-DIVERSE-C1     VALUE +13.                                   
001400      88 OING-SATS-C1        VALUE +14.                                   
001500      88 OING-PROGNOSPAV-C2  VALUE +21.                                   
001600      88 OING-DIVERSE-C2     VALUE +23.                                   
001700      88 OING-SATS-C2        VALUE +24.                                   
001800*                                 ORDERINGÅNGSTYP                         
001900     03 TIAAPP-AVBOK         PIC S9(5)           COMP-3.                  
002000*                                 AVBOKNINGSPERIOD (ÅÅPP)                 
002100     03 KVOI                 PIC S9(7)           COMP-3.                  
002200*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002300     03 KVOT                 PIC S9(7)           COMP-3.                  
002400*                                 ANTAL ORDERTRÄFF                        
002500*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
