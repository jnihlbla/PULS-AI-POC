000100 01  W90404O1.                                                            
000200*                                 COPYTEXT FÖR MOD W90404O1               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE              PIC X(41).                                   
000600*                                 MEDDELANDE                              
000700     03 FILLER               PIC X(9).                                    
000800     03 FILLER               PIC X(9).                                    
000900     03 BLAEDRING-IDARTNR    PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 AREA-OUTPUT.                                                      
001200*                                                                         
001300        05 RADER             OCCURS 13 TIMES.                             
001400*                                                                         
001500           07 IDARTNR        PIC Z(8)9.                                   
001600*                                 ARTIKELNUMMER                           
001700           07 DIERS-ERS      PIC -(6)9.9(3).                              
001800*                                 ANTAL FÖR ERSATT  DIERS-ERS-002         
001900           07 KDERS-C1       PIC Z(2)9(2).                                
002000*                                 ERSÄTTNINGSKOD C1     KDERS-002         
002100           07 KDERS-C2       PIC Z(2)9(2).                                
002200*                                 ERSÄTTNINGSKOD C1     KDERS-002         
002300           07 DIERS-TILLK    PIC -(6)9.9(3).                              
002400*                                                 DIERS-TILLK-002         
002500*                                 ANTAL FÖR TILLKOMMANDE                  
002600           07 FILLER         PIC X.                                       
002700           07 TEXT-KOMMENTAR PIC X(14).                                   
002800           07 BEART          PIC X(25).                                   
002900*                                 ARTIKELBENÄMNING                        
003000     03 LINE23.                                                           
003100        05 MESSAGE-BOTTOM    PIC X(79).                                   
003200*                                 MEDDELANDEFÄLT PÅ RAD 23                
003300*** END OF VILMAII-COPY LENGTH= 1178 BYTES                                
