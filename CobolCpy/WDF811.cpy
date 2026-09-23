000100 01  DSPR-WDF811.                                                         
000200*                                 STYRNING SPÄRREGLER                     
000300*                                 DITRIKT OCH KUND                        
000400*                                 FYSISK-NYCKEL: WDF811KY                 
000500*                                 (IDDISTR-FOM + IDDISTR-TOM +            
000600*                                 (IDKUNDNR-FOM + IDKUNDNR-TOM)           
000700*                                                                         
000800     03 DSPR-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
000900*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001000*                                 LOWEST DISTRICT NUMBER                  
001100     03 DSPR-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
001200*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001300*                                 HIGHEST DISTRICT NUMBER                 
001400     03 DSPR-IDKUNDNR-FOM    PIC S9(7)           COMP-3.                  
001500*                                 LÄGSTA KUNDNUMMER I INTERVALL           
001600*                                 LOWEST CUSTOMER NUMBER                  
001700     03 DSPR-IDKUNDNR-TOM    PIC S9(7)           COMP-3.                  
001800*                                 HÖGSTA KUNDNUMMER I INTERVALL           
001900*                                 HIGHEST CUSTOMER NUMBER                 
002000     03 DSPR-ORDERKLASS-GRP  OCCURS 5 TIMES.                              
002100*                                 INDEX 1 = KDORDKL 0                     
002200*                                 INDEX 2 = KDORDKL 1                     
002300*                                 INDEX 3 = KDORDKL 2                     
002400*                                 INDEX 4 = KDORDKL 3                     
002500*                                 INDEX 5 = KDORDKL 4                     
002600        05 DSPR-FLMARKSP     PIC X.                                       
002700*                                 MARKNADSSPÄRR                           
002800*                                 MARKET BLOCKING CODE                    
002900        05 DSPR-TISTADAT     PIC S9(7)           COMP-3.                  
003000*                                 GENERELLT STARTDATUM                    
003100*                                 GENERAL START DATE                      
003200*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
