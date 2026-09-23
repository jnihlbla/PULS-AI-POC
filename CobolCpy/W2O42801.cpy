000100 01  MOD-W2O42801.                                                        
000200*                                 MOD-COPYTEXT FÖR W2042800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR.                                                      
001000        05 MOD-IDARTNR-UT    PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 MOD-DASH-1        PIC X.                                       
001300        05 MOD-REKSIFFR      PIC X.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-LINE-INFO        OCCURS 14 TIMES.                             
002000        05 MOD-TIAAVV        PIC 9(4).                                    
002100*                                 ÅR - VECKA  (ÅÅVV)                      
002200        05 MOD-KVPB-REF      PIC Z(5)9.9.                                 
002300*                                 PERIODBEHOV REFILLING                   
002400        05 MOD-KVPB-REFILL   PIC Z(5)9.9.                                 
002500*                                 PERIODBEHOV (PROGNOS)                   
002600        05 MOD-KVPB-CDC      PIC Z(5)9.9.                                 
002700*                                 PERIODBEHOV (PROGNOS)                   
002800        05 MOD-KVPB-GLOBAL   PIC Z(5)9.9.                                 
002900*                                 PERIODBEHOV (PROGNOS)                   
003000        05 MOD-KVPB-TREND    PIC -(5)9.9.                                 
003100*                                 PERIODTRENDVÄRDE                        
003200        05 MOD-KVPB-TOT      PIC Z(5)9.9.                                 
003300*                                 TOTALT PERIODBEHOV                      
003400        05 MOD-KVPB-PLAN     PIC Z(5)9.9.                                 
003500*                                 PLANERAT PERIODBEHOV                    
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 963 BYTES                                 
