000100 01  MOD-W2O15801.                                                        
000200*                                 MOD-COPYTEXT F÷R W2015800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-TIAAVV-IN        PIC X(4).                                    
001200*                                 ≈R - VECKA  (≈≈VV)                      
001300     03 MOD-TIAAVV-UT        PIC X(4).                                    
001400*                                 ≈R - VECKA  (≈≈VV)                      
001500     03 MOD-COL-INFO         OCCURS 5 TIMES.                              
001600        05 MOD-LINE-INFO     OCCURS 14 TIMES.                             
001700           07 MOD-IDDC       PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900           07 MOD-KVPB       PIC Z(5)9.9.                                 
002000*                                 PERIODBEHOV (PROGNOS)                   
002100     03 MOD-TEMFSINF         PIC X(55).                                   
002200*                                 INFORMATIONSMEDDELANDE                  
002300*** END OF VILMAII-COPY LENGTH= 825 BYTES                                 
