000100 01  MID-W2I43301.                                                        
000200*                                 MID-COPYTEXT F÷R W2043300               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT.                                                        
001200*                                                                         
001300        05 MID-PRMATRL       PIC X(10).                                   
001400*                                 FAST PRIS UNDER L÷PANDE ≈R              
001500        05 MID-KVPB-REF      PIC X(8).                                    
001600*                                 PERIODBEHOV REFILLING                   
001700        05 MID-TIREFMPB      PIC X(6).                                    
001800*                                 DATUM MANUELL PROGNOS REFILLING         
001900        05 MID-DAPUBL        PIC 9(5).                                    
002000*                                 ≈R - VECKA - DAG   (≈≈VVD)              
002100        05 MID-TILEVBEG      PIC X(4).                                    
002200*                                 ≈R - VECKA  (≈≈VV)                      
002300        05 MID-KVPROG        PIC X(7).                                    
002400*                                 ≈RSPROGNOS                              
002500        05 MID-IDINK         PIC X(3).                                    
002600*                                 INK÷PARNUMMER                           
002700        05 MID-IDANSK        PIC X(3).                                    
002800*                                 ANSKAFFARNUMMER                         
002900*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
