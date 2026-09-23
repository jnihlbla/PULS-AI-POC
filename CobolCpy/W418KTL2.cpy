000100 01  W418KTL2.                                                            
000200*                                 LÄNKCOPYTEXT FRÅN PGM W41810 TI         
000300*                                 LL W418KTL2                             
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 IDRAPPNR             PIC 9(7).                                    
000900*                                 RAPPORT NUMMER                          
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 IDRADNR              PIC S9(5)           COMP-3.                  
001300*                                 RADNUMMER                               
001400     03 KVLEVANM-BEKR        PIC S9(7)           COMP-3.                  
001500*                                 BEKRÄFTAT RETURANTAL                    
001600     03 KDSVAR               PIC X.                                       
001700      88 KDSVAR-OK           VALUE ' '.                                   
001800      88 KDSVAR-FEL          VALUE 'F'.                                   
001900*                                                       KDSVAR-88         
002000*                                 SVARSKOD FRÅN SUBPROGRAM                
002100     03 IDFELKOD             OCCURS 50 TIMES                              
002200                             PIC X(3).                                    
002300*                                 FELKOD                                  
002400*** END OF VILMAII-COPY LENGTH= 177 BYTES                                 
