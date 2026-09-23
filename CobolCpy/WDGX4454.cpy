000100 01  4454-WDGX4454.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 PRINTERTABELL                           
000400*                                 FYSISK NYCKEL                           
000500*                                 KDSEGKEY = 1                            
000600     03 4454-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 4454-KDPRTGEN-PU     PIC X(3).                                    
001000*                                 PRINTERKOD PACKUNDERLAG                 
001100*                                 PRINTERCODE PACKING UNIT                
001200     03 4454-KDPRTGEN-PLE    PIC X(3).                                    
001300*                                 PRINTERKOD PLOCKETIKETTER               
001400*                                 PRINTERCODE PICKING LABLES              
001500     03 4454-OMR             OCCURS 99 TIMES.                             
001600        05 4454-KDPRT-PU     PIC X(3).                                    
001700*                                 PRINTERKOD PACKUNDERLAG                 
001800*                                 PRINTERCODE PACKING UNIT                
001900        05 4454-KDPRT-PLE    PIC X(3).                                    
002000*                                 PRINTERKOD PLOCKETIKETTER               
002100*                                 PRINTERCODE PICKING LABLES              
002200        05 4454-KDSS-PU      PIC X.                                       
002300*                                 SIDOSKIPSKOD                            
002400*                                 CODE FOR PAGESKIP                       
002500        05 4454-KDSS-PLE     PIC X.                                       
002600*                                 SIDOSKIPSKOD                            
002700*                                 CODE FOR PAGESKIP                       
002800        05 4454-IDPRC        PIC X(4).                                    
002900*                                 PRODUKTIONSKANAL                        
003000*                                 PRODUCTION CHANNEL                      
003100     03 4454-FILLER          PIC X(5).                                    
003200*** END COPY WDGX4454C0  LENGTH=1200                                      
