000100 01  MOD-W2O12801.                                                        
000200*                                 MOD-COPYTEXT FÖR W2012800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-LINE-INFO        OCCURS 14 TIMES.                             
001200        05 MOD-TIAAVV        PIC 9(4).                                    
001300*                                 ÅR - VECKA  (ÅÅVV)                      
001400        05 MOD-KVPB-SEP      PIC Z(5)9.9.                                 
001500*                                 SEPARAT PERIODBEHOV                     
001600        05 MOD-KVPB-SATS     PIC Z(5)9.9.                                 
001700*                                 SATS-PERIODBEHOV                        
001800        05 MOD-KVPB-TPO      PIC Z(5)9.9.                                 
001900*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
002000        05 MOD-KVPB-SDC      PIC Z(5)9.9.                                 
002100*                                 PERIODBEHOV FÖR SAMTL SDC:ER            
002200        05 MOD-KVPB-NDC      PIC Z(5)9.9.                                 
002300*                                 PERIODBEHOV (PROGNOS)                   
002400        05 MOD-KVPB-TREND    PIC -(6)9.9.                                 
002500*                                 PERIODTRENDVÄRDE                        
002600        05 MOD-KVPB-TOT      PIC Z(6)9.9.                                 
002700*                                 TOTALT PERIODBEHOV                      
002800        05 MOD-KVPB-PLAN     PIC Z(6)9.9.                                 
002900*                                 PLANERAT PERIODBEHOV                    
003000     03 MOD-TEMFSINF         PIC X(55).                                   
003100*                                 INFORMATIONSMEDDELANDE                  
003200*** END OF VILMAII-COPY LENGTH= 1111 BYTES                                
