000100 01  W4O72301.                                                            
000200*                                 MODCOPYTEXT TILL W40723.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDDISTR-IN           PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR-IN          PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 IDRAPPNR-IN          PIC X(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 IDARTNR-IN           PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 IDRADNR-IN           PIC X(4).                                    
001600*                                 RADNUMMER                               
001700     03 IDDISTR-UT           PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 IDKUNDNR-UT          PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 IDRAPPNR-UT          PIC X(7).                                    
002200*                                 RAPPORT NUMMER                          
002300     03 IDARTNR-UT           PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 IDRADNR-UT           PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 INPUT.                                                            
002800*                                                                         
002900        05 TEANMNOT-REG      OCCURS 3 TIMES                               
003000                             PIC X(70).                                   
003100*                                 FRI TEXT FRÅN REGISTRERINGEN            
003200        05 TEANMNOT-DLR      OCCURS 3 TIMES                               
003300                             PIC X(70).                                   
003400*                                 FRI TEXT FRÅN ADM. TILL DEALER          
003500        05 TEANMNOT-ADM      OCCURS 3 TIMES                               
003600                             PIC X(70).                                   
003700*                                 FRI TEXT FRÅN ADMINISTRATION            
003800        05 TEANMNOT-REM      OCCURS 3 TIMES                               
003900                             PIC X(70).                                   
004000*                                 FRI TEXT FRÅN REMISSINSTANS             
004100        05 TEANMNOT-RET      OCCURS 3 TIMES                               
004200                             PIC X(70).                                   
004300*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
004400     03 TEMFSINF             PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END OF VILMAII-COPY LENGTH= 1209 BYTES                                
