000100 01  W33522.                                                              
000200*                                 COPY-TEXT FÖR FILEN W33522              
000300     03 RAD1.                                                             
000400        05 IDUSER            PIC X(8).                                    
000500*                                 ANVÄNDARENS SÄKERHETS ID                
000600        05 FLAGGA            PIC X.                                       
000700*                                 ALLMÄN FLAGGA                           
000800        05 KDSORT1           PIC 9.                                       
000900*                                 SORTERINGSKOD                           
001000        05 FILLER            PIC X(70).                                   
001100     03 RAD2 REDEFINES RAD1.                                              
001200        05 IDPROMR           OCCURS 3 TIMES.                              
001300*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001400           07 IDMARKBO       PIC X.                                       
001500*                                 MARKNADSBOLAGSKOD                       
001600*                                                                         
001700           07 IDPROMRN       PIC X(2).                                    
001800*                                 PRISOMRÅDE LÖPNUMMER                    
001900        05 IDFKNGRP          OCCURS 4 TIMES.                              
002000           07 IDFKNGRP-FOM   PIC 9(4).                                    
002100*                                 FUNKTIONSGRUPP-FROM                     
002200           07 IDFKNGRP-TOM   PIC 9(4).                                    
002300*                                 FUNKTIONSGRUPP-TOM                      
002400        05 IDDISTGRP         OCCURS 4 TIMES.                              
002500           07 IDDISTR-FOM    PIC 9(4).                                    
002600*                                 LÄGSTA DISTRIKTNR I INTERVALL           
002700           07 IDDISTR-TOM    PIC 9(4).                                    
002800*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002900        05 FILLER            PIC X(7).                                    
003000     03 RAD3 REDEFINES RAD1.                                              
003100        05 KDPRODSLGRP       OCCURS 4 TIMES.                              
003200           07 KDPRODSL-FOM   PIC 9(2).                                    
003300*                                 PRODUKTSLAG                             
003400           07 KDPRODSL-TOM   PIC 9(2).                                    
003500*                                 PRODUKTSLAG                             
003600        05 KDERSGRP          OCCURS 4 TIMES.                              
003700           07 KDERS-FOM      PIC 9(2).                                    
003800*                                 ERSÄTTNINGSKOD                          
003900           07 KDERS-TOM      PIC 9(2).                                    
004000*                                 ERSÄTTNINGSKOD                          
004100        05 FLEXCEL           PIC X.                                       
004200*                                 ALLMÄN FLAGGA                           
004300        05 FLMAIL            PIC X.                                       
004400*                                 ALLMÄN FLAGGA                           
004500        05 FILLER            PIC X(46).                                   
004600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
