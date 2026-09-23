000100 01  MID-W4I26701.                                                        
000200*                                 MID-COPYTEXT FÖR W4026700               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR7-IN      PIC X(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR7-UT      PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-INPUT.                                                        
002000*                                 UPPDATERINGSFÄLT                        
002100        05 MID-PRAVDRAG      PIC X(10).                                   
002200*                                 AVDRAGSBELOPP (KR)                      
002300        05 MID-REAVDRAG      PIC X(4).                                    
002400*                                 AVDRAGSPROCENT                          
002500        05 MID-PRFRAKT       PIC X(10).                                   
002600*                                 FRAKTKOSTNAD (KR)                       
002700        05 MID-PRFOERS       PIC X(10).                                   
002800*                                 FÖRSÄKRINGSPREMIE (KR)                  
002900        05 MID-REFOERS       PIC X(6).                                    
003000*                                 PROCENT             REFOERS-002         
003100        05 MID-REOVKOFF      PIC X(4).                                    
003200*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
003300        05 MID-PRLEGKST      PIC X(10).                                   
003400*                                 LEGALISERINSKOSTNAD (KR)                
003500        05 MID-KDVALUTA      PIC 9(3).                                    
003600*                                 VALUTAKOD                               
003700        05 MID-PRKURS        PIC X(11).                                   
003800*                                 VALUTAKURS                              
003900        05 MID-BELOSORT      PIC X(17).                                   
004000*                                 LOSSNININGSORT NAMN                     
004100        05 MID-TEBANKTO      PIC X(20).                                   
004200*                                 BANKKONTO                               
004300        05 MID-TEBANK-RAD1   PIC X(72).                                   
004400*                                 BANKINFORMATION                         
004500        05 MID-TEBANK-RAD2   PIC X(72).                                   
004600*                                 BANKINFORMATION                         
004700*** END COPY W4I26701C0  LENGTH=301                                       
