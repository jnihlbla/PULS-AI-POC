000100 01  MID-W2I13601.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INPUT.                                                        
000800*                                 KVPB OCH FLMPB FÖRÄNDRING               
000900        05 MID-KVPB-PLAN     PIC X(8).                                    
001000*                                 PLANERAT PERIODBEHOV                    
001100        05 MID-TIPBPLAN      PIC 9(6).                                    
001200*                                 DATUM KVPB-PLAN GILTIG I EN PER         
001300*                                 IOD                                     
001400        05 MID-KVPB-PLAN-JUST1                                            
001500                             PIC X(8).                                    
001600*                                 PLANERAT PERIODBEHOV JUST1              
001700        05 MID-TIPBPLAN-JUST1-FOM                                         
001800                             PIC X(4).                                    
001900*                                 FOM PB-PLAN DATUM - JUST1               
002000        05 MID-TIPBPLAN-JUST1-TOM                                         
002100                             PIC 9(6).                                    
002200*                                 TOM PB-PLAN DATUM - JUST1               
002300        05 MID-KVPB-PLAN-JUST2                                            
002400                             PIC X(8).                                    
002500*                                 PLANERAT PERIODBEHOV JUST2              
002600        05 MID-TIPBPLAN-JUST2-FOM                                         
002700                             PIC X(4).                                    
002800*                                 FOM PB-PLAN DATUM - JUST2               
002900        05 MID-TIPBPLAN-JUST2-TOM                                         
003000                             PIC 9(6).                                    
003100*                                 TOM PB-PLAN DATUM - JUST2               
003200*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
