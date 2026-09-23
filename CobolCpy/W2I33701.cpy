000100 01  MID-W2I33701.                                                        
000200*                                 MID-COPYTEXT FÖR W20337                 
000300     03 MID-IDSPRGRP-IN      PIC X(10).                                   
000400*                                 SPÄRRADE GRUPPER                        
000500     03 MID-IDSPRGRP-UT      PIC X(10).                                   
000600*                                 SPÄRRADE GRUPPER                        
000700     03 MID-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-INDATA.                                                       
001200*                                 GRUPP MED INMATNINGSFÄLT                
001300        05 MID-INDATA-RAD    OCCURS 12 TIMES.                             
001400*                                 TABELLRADER                             
001500           07 MID-KDCMD-A    PIC X.                                       
001600           07 MID-KDCMD-U    PIC X.                                       
001700           07 MID-KDCMD-P    PIC X.                                       
001800           07 MID-KDCMD-F    PIC X.                                       
001900        05 MID-KDCMD-E       PIC X.                                       
002000        05 MID-IDARTNR-E     PIC 9(9).                                    
002100*                                 ARTIKELNUMMER                           
002200        05 MID-TISTADAT-A-E  PIC 9(6).                                    
002300*                                 GENERELLT STARTDATUM                    
002400        05 MID-KDARTURS-E    PIC X(2).                                    
002500*                                 ARTIKELURSPRUNGSKOD                     
002600        05 MID-TISTADAT-U-E  PIC 9(6).                                    
002700*                                 GENERELLT STARTDATUM                    
002800        05 MID-KDPRODSL-FOM-E                                             
002900                             PIC X(2).                                    
003000*                                 PRODUKTSLAG                             
003100        05 MID-KDPRODSL-TOM-E                                             
003200                             PIC X(2).                                    
003300*                                 PRODUKTSLAG                             
003400        05 MID-TISTADAT-P-E  PIC 9(6).                                    
003500*                                 GENERELLT STARTDATUM                    
003600        05 MID-IDFKNGRP-FOM-E                                             
003700                             PIC X(4).                                    
003800*                                 FUNKTIONSGRUPP-FROM                     
003900        05 MID-IDFKNGRP-TOM-E                                             
004000                             PIC X(4).                                    
004100*                                 FUNKTIONSGRUPP-TOM                      
004200        05 MID-TISTADAT-F-E  PIC 9(6).                                    
004300*                                 GENERELLT STARTDATUM                    
004400     03 MID-INDATA-RAD.                                                   
004500*                                 NYCKLAR FRÅN VALD RAD                   
004600        05 MID-NYCKLARFRNRADEN                                            
004700                             OCCURS 12 TIMES.                             
004800*                                 UPPDATERINGSRAD                         
004900           07 MID-IDARTNR    PIC 9(9).                                    
005000*                                 ARTIKELNUMMER                           
005100           07 MID-KDARTURS   PIC X(2).                                    
005200*                                 ARTIKELURSPRUNGSKOD                     
005300           07 MID-KDPRODSL-FOM                                            
005400                             PIC 9(2).                                    
005500*                                 PRODUKTSLAG                             
005600           07 MID-KDPRODSL-TOM                                            
005700                             PIC 9(2).                                    
005800*                                 PRODUKTSLAG                             
005900           07 MID-IDFKNGRP-FOM                                            
006000                             PIC 9(4).                                    
006100*                                 FUNKTIONSGRUPP                          
006200           07 MID-IDFKNGRP-TOM                                            
006300                             PIC 9(4).                                    
006400*                                 FUNKTIONSGRUPP                          
006500*** END OF VILMAII-COPY LENGTH= 410 BYTES                                 
