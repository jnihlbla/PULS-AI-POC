000100 01  W2364000.                                                            
000200*                                                                         
000300*                                 ARTIKLAR SOM                            
000400*                                 INTE HAR BLIVIT                         
000500*                                 INLEVERERADE I TID                      
000600*                                                                         
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDLEVNR              PIC S9(5)           COMP-3.                  
001000*                                 LEVERANTÖRNUMMER                        
001100     03 IDANSK               PIC S9(3)           COMP-3.                  
001200*                                 ANSKAFFARNUMMER                         
001300     03 TEXT-BEART           PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500     03 KVAVROP              PIC S9(7)           COMP-3.                  
001600*                                 AVROPSKVANTITET                         
001700     03 TIAVROP-INL          PIC S9(5)           COMP-3.                  
001800*                                 INLEVERANSDATUM (PLANERAD)              
001900*                                 (ÅÅVV)                                  
002000     03 TIAVROP-AVS          PIC S9(5)           COMP-3.                  
002100*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002200*                                 (ÅÅVV)                                  
002300     03 KDAVRPRIO            PIC S9              COMP-3.                  
002400*                                 PRIORITET VID FÖRSENAT AVROP            
002500     03 KDHF                 PIC S9              COMP-3.                  
002600*                                 HUVUDFÖRRÅDSMÄRKNING                    
002700*** END COPY W2364000C0  LENGTH=47                                        
