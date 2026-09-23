000100 01  MOD-W6O15501.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-AVSDAT-IN        PIC X(6).                                    
001200*                                 DATUM ENLIGT KDDATFORM                  
001300     03 MOD-AVSDAT-UT        PIC X(6).                                    
001400*                                 DATUM ENLIGT KDDATFORM                  
001500     03 MOD-ADTRDEST-IN      PIC X(3).                                    
001600*                                 TRANSPORTDESTINATION                    
001700     03 MOD-ADTRDEST-UT      PIC X(3).                                    
001800*                                 TRANSPORTDESTINATION                    
001900     03 MOD-IDTRPTNR-IN      PIC X(5).                                    
002000*                                 TRANSPORTIDENTITET                      
002100     03 MOD-IDTRPTNR-UT      PIC X(5).                                    
002200*                                 TRANSPORTIDENTITET                      
002300     03 MOD-TABELLRAD        OCCURS 15 TIMES.                             
002400*                                 GRUPP MED TABELLRADER                   
002500        05 MOD-IDARTNR       PIC Z(8)9.                                   
002600*                                 ARTIKELNUMMER                           
002700        05 MOD-KVANTAL       PIC Z(5)9.                                   
002800*                                 ANTAL                                   
002900        05 MOD-ADTRDEST      PIC X(3).                                    
003000*                                 TRANSPORTDESTINATION                    
003100        05 MOD-AVSDAT        PIC X(6).                                    
003200*                                 DATUM ENLIGT KDDATFORM                  
003300        05 MOD-TITRPMOT      PIC X(6).                                    
003400*                                 MOTTAGNINGSDATUM                        
003500        05 MOD-IDTRPTNR      PIC Z(4)9.                                   
003600*                                 TRANSPORTIDENTITET                      
003700        05 MOD-ADINLOMR-LPL  PIC X(4).                                    
003800*                                 LOSSNINGSPLATS                          
003900        05 MOD-IDUSER-TRP    PIC X(8).                                    
004000*                                 ANVÄNDAR-ID SENASTE UPPDATERING         
004100        05 MOD-TETRPMED      PIC X(20).                                   
004200*                                 TEXT VID TRANSPORTBEGÄRAN               
004300     03 MOD-TEMFSINF         PIC X(55).                                   
004400*                                 INFORMATIONSMEDDELANDE                  
004500*** END OF VILMAII-COPY LENGTH= 1150 BYTES                                
