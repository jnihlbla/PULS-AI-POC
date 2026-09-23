000100 01  W418360.                                                             
000200*                                 360 UPPDATERING RETURTILLSTÅND          
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 SORTAREA             PIC X(25).                                   
000700     03 FILLER REDEFINES SORTAREA.                                        
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 KDCLAGER          PIC S9              COMP-3.                  
001300*                                 CENTRALLAGERKOD                         
001400        05 IDORDNR           PIC S9(5)           COMP-3.                  
001500*                                 ORDERNUMMER                             
001600        05 IDARTNR           PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800        05 KONVDATUM         PIC S9(7)           COMP-3.                  
001900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002000        05 FILLER            PIC X(5).                                    
002100     03 KVLEVART             PIC S9(7)           COMP-3.                  
002200*                                 LEVERERAT ANTAL ARTIKLAR                
002300     03 KDANMORS             PIC S9(3)           COMP-3.                  
002400*                                 ORSAK TILL LEVERANSANMÄRKNING           
002500     03 KDNIVAA4             PIC S9(3)           COMP-3.                  
002600*                                 NIVÅNUMMER-4                            
002700*** END COPY W418360CC0  LENGTH=36                                        
