000100 01  KMRK-WDM221.                                                         
000200*                                 KAMPANJREGISTER                         
000300*                                 BESKRIVNING AV MARKNADER                
000400*                                 FYSISK NYCKEL: WDM221KY                 
000500*                                 (IDDISTR-FOM + IDDISTR-TOM +            
000600*                                  IDKUNDNR-FOM+ IDKUNDNR-TOM)            
000700*                                                                         
000800     03 KMRK-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
000900*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001000*                                 LOWEST DISTRICT NUMBER                  
001100     03 KMRK-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
001200*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001300*                                 HIGHEST DISTRICT NUMBER                 
001400     03 KMRK-IDKUNDNR-FOM    PIC S9(7)           COMP-3.                  
001500*                                 LÄGSTA KUNDNUMMER I INTERVALL           
001600*                                 LOWEST CUSTOMER NUMBER                  
001700     03 KMRK-IDKUNDNR-TOM    PIC S9(7)           COMP-3.                  
001800*                                 HÖGSTA KUNDNUMMER I INTERVALL           
001900*                                 HIGHEST CUSTOMER NUMBER                 
002000     03 KMRK-KVBEART-KAMP    PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
002200*                                 ORDERED QUANTITY FOR A CAMPAIGN         
002300     03 KMRK-KVBEART-KUND    PIC S9(7)           COMP-3.                  
002400*                                 AV KUND BESTÄLLT KVANTITET              
002500*                                 ORDERED QUANTITY BY A CUSTOMER          
002600*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
