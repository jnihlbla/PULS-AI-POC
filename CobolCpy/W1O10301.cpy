000100 01  W1O10301.                                                            
000200*                                 COPYTEXT FÖR MOD W1010301               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE              PIC X(41).                                   
000600*                                 MEDDELANDE                              
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 BLAEDRING-IDARTNR    PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 AREA-OUTPUT.                                                      
001400*                                                                         
001500        05 RADER             OCCURS 13 TIMES.                             
001600*                                                                         
001700           07 IDARTNR        PIC Z(8)9.                                   
001800*                                 ARTIKELNUMMER                           
001900           07 DIERS-ERS      PIC -(6)9.9(3).                              
002000*                                 ANTAL FÖR ERSATT  DIERS-ERS-002         
002100           07 KDERS-C1       PIC Z(2)9(2).                                
002200*                                 ERSÄTTNINGSKOD C1     KDERS-002         
002300           07 KDERS-C2       PIC Z(2)9(2).                                
002400*                                 ERSÄTTNINGSKOD C1     KDERS-002         
002500           07 DIERS-TILLK    PIC -(6)9.9(3).                              
002600*                                                 DIERS-TILLK-002         
002700*                                 ANTAL FÖR TILLKOMMANDE                  
002800           07 FILLER         PIC X.                                       
002900           07 TEXT-KOMMENTAR PIC X(14).                                   
003000           07 BEART          PIC X(25).                                   
003100*                                 ARTIKELBENÄMNING                        
003200     03 LINE23.                                                           
003300        05 MESSAGE-BOTTOM    PIC X(79).                                   
003400*                                 MEDDELANDEFÄLT PÅ RAD 23                
003500*** END COPY W1O10301C0  LENGTH=1178                                      
