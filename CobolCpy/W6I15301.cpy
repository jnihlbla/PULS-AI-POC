000100 01  MID-W6I15301.                                                        
000200*                                                                         
000300     03 MID-ADTRDEST-IN      PIC X(3).                                    
000400*                                 TRANSPORTDESTINATION                    
000500     03 MID-ADTRDEST-UT      PIC X(3).                                    
000600*                                 TRANSPORTDESTINATION                    
000700     03 MID-IDTRPTNR-IN      PIC X(5).                                    
000800*                                 TRANSPORTIDENTITET                      
000900     03 MID-IDTRPTNR-UT      PIC X(5).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 MID-INPUT.                                                        
001200*                                                                         
001300        05 MID-RAD           OCCURS 13 TIMES.                             
001400*                                                                         
001500           07 MID-CMD        PIC X.                                       
001600        05 MID-LOSSNING-KLAR PIC X.                                       
001700        05 MID-ADINLOMR-LPL  PIC X(4).                                    
001800*                                 LOSSNINGSPLATS                          
001900        05 MID-PRINTER       PIC X(4).                                    
002000*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
