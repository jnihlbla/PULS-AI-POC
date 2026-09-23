000100 01  MID-W3I33101.                                                        
000200*                                 MID-COPYTEXT FÖR W3033100               
000300     03 MID-URVAL.                                                        
000400*                                                                         
000500        05 MID-IDPROMR       OCCURS 3 TIMES.                              
000600*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
000700           07 MID-IDMARKBO   PIC X.                                       
000800*                                 MARKNADSBOLAGSKOD                       
000900*                                                                         
001000           07 MID-IDPROMRN   PIC X(2).                                    
001100*                                 PRISOMRÅDE LÖPNUMMER                    
001200        05 MID-IDFKNGRP      OCCURS 4 TIMES.                              
001300*                                                                         
001400           07 MID-IDFKNGRP-FOM                                            
001500                             PIC 9(4).                                    
001600*                                 FUNKTIONSGRUPP-FROM                     
001700           07 MID-IDFKNGRP-TOM                                            
001800                             PIC 9(4).                                    
001900*                                 FUNKTIONSGRUPP-TOM                      
002000        05 MID-IDDIST        OCCURS 4 TIMES.                              
002100*                                                                         
002200           07 MID-IDDISTR-FOM                                             
002300                             PIC 9(4).                                    
002400*                                 LÄGSTA DISTRIKTNR I INTERVALL           
002500           07 MID-IDDISTR-TOM                                             
002600                             PIC 9(4).                                    
002700*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002800        05 MID-KDPRODSL      OCCURS 4 TIMES.                              
002900*                                                                         
003000           07 MID-KDPRODSL-FOM                                            
003100                             PIC 9(2).                                    
003200*                                 PRODUKTSLAG                             
003300           07 MID-KDPRODSL-TOM                                            
003400                             PIC 9(2).                                    
003500*                                 PRODUKTSLAG                             
003600        05 MID-KDERS         OCCURS 4 TIMES.                              
003700*                                                                         
003800           07 MID-KDERS-FOM  PIC 9(2).                                    
003900*                                 ERSÄTTNINGSKOD                          
004000           07 MID-KDERS-TOM  PIC 9(2).                                    
004100*                                 ERSÄTTNINGSKOD                          
004200     03 MID-KDSORT1          PIC X.                                       
004300*                                 SORTERINGSKOD                           
004400     03 MID-FLAGGA-EXCE      PIC X.                                       
004500*                                 ALLMÄN FLAGGA                           
004600*** END OF VILMAII-COPY LENGTH= 107 BYTES                                 
