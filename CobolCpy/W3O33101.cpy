000100 01  MOD-W3O33101.                                                        
000200*                                 MOD-COPYTEXT FÖR W3033100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-URVAL.                                                        
000800        05 MOD-IDPROMR-GRP   OCCURS 3 TIMES.                              
000900           07 MOD-IDPROMR-ATTR                                            
001000                             PIC X(2).                                    
001100*                                 MFS ATTRIBUTFÄLT                        
001200           07 MOD-IDPROMR.                                                
001300*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001400              09 MOD-IDMARKBO                                             
001500                             PIC X.                                       
001600*                                 MARKNADSBOLAGSKOD                       
001700*                                                                         
001800              09 MOD-IDPROMRN                                             
001900                             PIC X(2).                                    
002000*                                 PRISOMRÅDE LÖPNUMMER                    
002100        05 MOD-IDFKN-GRP     OCCURS 4 TIMES.                              
002200           07 MOD-IDFKNGRP-FOM-ATTR                                       
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500           07 MOD-IDFKNGRP-FOM                                            
002600                             PIC Z(3)9.                                   
002700*                                 FUNKTIONSGRUPP                          
002800           07 MOD-IDFKNGRP-TOM-ATTR                                       
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100           07 MOD-IDFKNGRP-TOM                                            
003200                             PIC Z(3)9.                                   
003300*                                 FUNKTIONSGRUPP                          
003400        05 MOD-IDDISTR-GRP   OCCURS 4 TIMES.                              
003500           07 MOD-IDDISTR-FOM-ATTR                                        
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800           07 MOD-IDDISTR-FOM                                             
003900                             PIC Z(3)9.                                   
004000*                                 LÄGSTA DISTRIKTNR I INTERVALL           
004100           07 MOD-IDDISTR-TOM-ATTR                                        
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400           07 MOD-IDDISTR-TOM                                             
004500                             PIC Z(3)9.                                   
004600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
004700        05 MOD-KDPROD-GRP    OCCURS 4 TIMES.                              
004800           07 MOD-KDPRODSL-FOM-ATTR                                       
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100           07 MOD-KDPRODSL-FOM                                            
005200                             PIC Z9.                                      
005300*                                 PRODUKTSLAG                             
005400           07 MOD-KDPRODSL-TOM-ATTR                                       
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700           07 MOD-KDPRODSL-TOM                                            
005800                             PIC Z9.                                      
005900*                                 PRODUKTSLAG                             
006000        05 MOD-KDERS-GRP     OCCURS 4 TIMES.                              
006100           07 MOD-KDERS-FOM-ATTR                                          
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400           07 MOD-KDERS-FOM  PIC Z9.                                      
006500*                                 ERSÄTTNINGSKOD                          
006600           07 MOD-KDERS-TOM-ATTR                                          
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900           07 MOD-KDERS-TOM  PIC Z9.                                      
007000*                                 ERSÄTTNINGSKOD                          
007100     03 MOD-KDSORT1-ATTR     PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-KDSORT1          PIC X.                                       
007400*                                 SORTERINGSKOD                           
007500     03 MOD-FLAGGA-EXCE-ATTR PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-FLAGGA-EXCE      PIC X.                                       
007800*                                 ALLMÄN FLAGGA                           
007900     03 MOD-TEMFSINF         PIC X(55).                                   
008000*                                 INFORMATIONSMEDDELANDE                  
008100*** END OF VILMAII-COPY LENGTH= 280 BYTES                                 
