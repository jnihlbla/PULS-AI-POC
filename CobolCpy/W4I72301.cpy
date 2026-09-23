000100 01  MID-W4I72301.                                                        
000200*                                 MID-COPYTEXT FÖR W40737                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDRAPPNR-IN      PIC X(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 MID-IDRAPPNR-UT      PIC X(7).                                    
001400*                                 RAPPORT NUMMER                          
001500     03 MID-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-IDRADNR-IN       PIC X(4).                                    
002000*                                 RADNUMMER                               
002100     03 MID-IDRADNR-UT       PIC X(4).                                    
002200*                                 RADNUMMER                               
002300     03 MID-INPUT.                                                        
002400*                                 INMATNINGSFÄLT                          
002500        05 MID-TEANMNOT-REG  OCCURS 3 TIMES                               
002600                             PIC X(70).                                   
002700*                                 FRI TEXT FRÅN REGISTRERINGEN            
002800        05 MID-TEANMNOT-DLR  OCCURS 3 TIMES                               
002900                             PIC X(70).                                   
003000*                                 FRI TEXT FRÅN ADMINISTRATION TI         
003100*                                 LL DEALERN                              
003200        05 MID-TEANMNOT-ADM  OCCURS 3 TIMES                               
003300                             PIC X(70).                                   
003400*                                 FRI TEXT FRÅN ADMINISTRATION            
003500        05 MID-TEANMNOT-REM  OCCURS 3 TIMES                               
003600                             PIC X(70).                                   
003700*                                 FRI TEXT FRÅN REMISSINSTANS             
003800        05 MID-TEANMNOT-RET  OCCURS 3 TIMES                               
003900                             PIC X(70).                                   
004000*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
004100*** END OF VILMAII-COPY LENGTH= 1110 BYTES                                
