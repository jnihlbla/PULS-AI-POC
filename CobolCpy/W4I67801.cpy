000100 01  MID-W4I67801.                                                        
000200*                                 MID-COPYTEXT FÖR W40678                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 MID-IDLBBET-IN       PIC X(12).                                   
000600*                                 LASTBÄRARBETECKNING                     
000700     03 MID-FLFARLIG-IN      PIC X.                                       
000800*                                 FARLIGT GODS-FLAGGA                     
000900     03 MID-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-FLSEPINV         PIC X.                                       
001200*                                 SEPARAT FAKTURA                         
001300     03 MID-PRFRAKT          PIC X(10).                                   
001400*                                 FRAKTKOSTNAD                            
001500     03 MID-LEGKST.                                                       
001600*                                                                         
001700        05 MID-PRLEGKST      PIC X(10).                                   
001800*                                 LEGALISERINSKOSTNAD                     
001900        05 MID-RELEGKST      PIC X(4).                                    
002000*                                 LEGALISERINGSKOSTNAD PROCENT            
002100     03 MID-EMBHNT.                                                       
002200*                                                                         
002300        05 MID-PREMBHNT      PIC X(10).                                   
002400*                                 EMBALLAGE O HANTERINGSKOST              
002500        05 MID-REEMBHNT      PIC X(4).                                    
002600*                                 EMB OCH HANTERINGSKOST (%)              
002700     03 MID-FOERS.                                                        
002800*                                                                         
002900        05 MID-PRFOERS       PIC X(10).                                   
003000*                                 FÖRSÄKRINGSPREMIE                       
003100        05 MID-REFOERS-OVKOFF.                                            
003200*                                                                         
003300           07 MID-REFOERS    PIC X(6).                                    
003400*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
003500           07 MID-REOVKOFF   PIC X(4).                                    
003600*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
003700     03 MID-AVDRAG.                                                       
003800*                                                                         
003900        05 MID-PRAVDRAG      PIC X(10).                                   
004000*                                 AVDRAGSBELOPP                           
004100        05 MID-REAVDRAG      PIC X(4).                                    
004200*                                 AVDRAGSPROCENT                          
004300*** END OF VILMAII-COPY LENGTH= 91 BYTES                                  
