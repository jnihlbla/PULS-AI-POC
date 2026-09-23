000100 01  MOD-W6O15301.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-ADTRDEST-IN      PIC X(3).                                    
000800*                                 TRANSPORTDESTINATION                    
000900     03 MOD-ADTRDEST-UT      PIC X(3).                                    
001000*                                 TRANSPORTDESTINATION                    
001100     03 MOD-IDTRPTNR-IN      PIC X(5).                                    
001200*                                 TRANSPORTIDENTITET                      
001300     03 MOD-IDTRPTNR-UT      PIC X(5).                                    
001400*                                 TRANSPORTIDENTITET                      
001500     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
001600*                                 GRUPP MED TABELLRADER                   
001700        05 MOD-CMD-ATTR      PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-CMD           PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100        05 MOD-IDTRPTNR      PIC Z(4)9.                                   
002200*                                 TRANSPORTIDENTITET                      
002300        05 MOD-TIDATUM       PIC 9(6).                                    
002400*                                 DATUM ENLIGT KDDATFORM                  
002500        05 MOD-IDARTNR       PIC Z(8)9.                                   
002600*                                 ARTIKELNUMMER                           
002700        05 MOD-KVANTAL       PIC Z(5)9.                                   
002800*                                 ANTAL                                   
002900        05 MOD-ADLAGOMR      PIC Z9.                                      
003000*                                 LAGEROMRÅDE                             
003100        05 MOD-ADGANG        PIC Z9.                                      
003200*                                 GÅNG                                    
003300        05 MOD-ADPLATS       PIC Z(4)9.                                   
003400*                                 LAGERPLATSNUMMER                        
003500        05 MOD-TETRPMED      PIC X(20).                                   
003600*                                 TEXT VID TRANSPORTBEGÄRAN               
003700     03 MOD-LOSSNING-KLAR-ATTR                                            
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-LOSSNING-KLAR    PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200     03 MOD-ADINLOMR-LPL     PIC X(4).                                    
004300*                                 LOSSNINGSPLATS                          
004400     03 MOD-PRINTER-ATTR     PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-PRINTER          PIC X(4).                                    
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 896 BYTES                                 
