000100 01  KUND-W330KUND.                                                       
000200*                                 LÄNKAREA NR 1 FÖR KOMMUNIKATION         
000300*                                 MELLAN W330KUND                         
000400*                                 OCH DESS BATCH-HUVUDPROGRAM             
000500     03 KUND-RAD             OCCURS 1600 TIMES                            
000600                             DESCENDING KEY IS KUND-IDDISTR               
000700                             INDEXED KUND-IX.                             
000800        05 KUND-IDDISTR      PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 KUND-IDPROMR.                                                  
001100*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001200           07 KUND-IDMARKBO  PIC X.                                       
001300*                                 MARKNADSBOLAGSKOD                       
001400           07 KUND-IDPROMRN  PIC X(2).                                    
001500*                                 PRISOMRÅDE LÖPNUMMER                    
001600        05 KUND-KDMARK-BUDG  PIC S9(3)           COMP-3.                  
001700*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001800        05 KUND-BEMARK-BUDG  PIC X(15).                                   
001900*                                 NAMN PÅ     BUDGET 96 MARKNADER         
002000     03 KUND-KDSVAR          PIC X.                                       
002100*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002200*** END OF VILMAII-COPY LENGTH= 36801 BYTES                               
