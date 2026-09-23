000100 01  MID-W2I39301.                                                        
000200*                                 MID-COPYTEXT FÖR W2I393                 
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INPUT.                                                        
000800*                                 MID-INDATA 2393                         
000900        05 MID-IDLEVNR       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100        05 MID-KVSPANT       PIC X(7).                                    
001200*                                 SPÄRRAT ANTAL                           
001300        05 MID-IDPERSON-BUY  PIC X(3).                                    
001400*                                 PERSONKOD REFILLANSVARIG                
001500        05 MID-TIFINLV       PIC X(4).                                    
001600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001700     03 MID-KVPB-PLAN        PIC X(8).                                    
001800*                                 PLANERAT PERIODBEHOV                    
001900     03 MID-TIPBPLAN         PIC 9(6).                                    
002000*                                 DATUM KVPB-PLAN GILTIG I EN PER         
002100*                                 IOD                                     
002200     03 MID-TEARTNOT.                                                     
002300*                                 MID-INDATA 2393                         
002400        05 MID-TEARTNOT-1    PIC X(40).                                   
002500*                                 ARTIKEL NOTERING                        
002600        05 MID-TEARTNOT-2    PIC X(40).                                   
002700*                                 ARTIKEL NOTERING                        
002800*** END OF VILMAII-COPY LENGTH= 131 BYTES                                 
