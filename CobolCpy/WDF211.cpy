000100 01  DIR-WDF211.                                                          
000200*                                 DIREKTLEV. STYRNING                     
000300*                                 STYRTABELL                              
000400*                                 FYSISK-NYCKEL: WDF211KY                 
000500*                                 (IDDISTR-FOM + IDDISTR-TOM +            
000600*                                 (IDKUNDNR-FOM + IDKUNDNR-TOM)           
000700     03 DIR-IDDISTR-FOM      PIC S9(5)           COMP-3.                  
000800*                                 LÄGSTA DISTRIKTNR I INTERVALL           
000900*                                 LOWEST DISTRICT NUMBER                  
001000     03 DIR-IDDISTR-TOM      PIC S9(5)           COMP-3.                  
001100*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001200*                                 HIGHEST DISTRICT NUMBER                 
001300     03 DIR-IDKUNDNR-FOM     PIC S9(7)           COMP-3.                  
001400*                                 LÄGSTA KUNDNUMMER I INTERVALL           
001500*                                 LOWEST CUSTOMER NUMBER                  
001600     03 DIR-IDKUNDNR-TOM     PIC S9(7)           COMP-3.                  
001700*                                 HÖGSTA KUNDNUMMER I INTERVALL           
001800*                                 HIGHEST CUSTOMER NUMBER                 
001900     03 DIR-KVBEART-GRP      OCCURS 5 TIMES.                              
002000*                                 INDEX 1 = KDORDKL 0                     
002100*                                 INDEX 2 = KDORDKL 1                     
002200*                                 INDEX 3 = KDORDKL 2                     
002300*                                 INDEX 4 = KDORDKL 3                     
002400*                                 INDEX 5 = KDORDKL 4                     
002500        05 DIR-FLDDGS        PIC X.                                       
002600*                                 VISAR OM DDGS REGEL ÖVERLAPPAS          
002700*                                 SHOWS IF DDGS RULE IS OVERRIDED         
002800        05 DIR-KDDDGS        PIC X.                                       
002900*                                 REGEL HUR DDGS ART LEVERERAS            
003000*                                 RULE DDGS PART ABOUT DELIVERY           
003100        05 DIR-KVBEART-MIN   PIC S9(7)           COMP-3.                  
003200*                                 BESTÄLLT ANTAL MIN-KVANTITET            
003300*                                 MINIMUM ORDERED QUANTITY.               
003400        05 DIR-IDDC          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600*                                 WAREHOUSE IDENTIFIER                    
003700*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
