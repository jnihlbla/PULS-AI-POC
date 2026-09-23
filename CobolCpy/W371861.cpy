000100 01  W37186.                                                              
000200*                                 STYRREG FÖR BYTES-DISTRIKTEN            
000300*                                                                         
000400     03 SORTKEY.                                                          
000500*                                                                         
000600        05 IDPTYP            PIC X(3).                                    
000700*                                 POSTTYP                                 
000800        05 FILLER            PIC X.                                       
000900        05 IDLOPNR           PIC 9(3).                                    
001000*                                 LÖPNUMMER                               
001100        05 FILLER            PIC X.                                       
001200     03 IDDISTR-FOM          PIC 9(5).                                    
001300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001400     03 FILLER               PIC X.                                       
001500     03 IDDISTR-TOM          PIC 9(5).                                    
001600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001700     03 FILLER               PIC X.                                       
001800     03 IDKUNDNR-FOM         PIC 9(7).                                    
001900*                                 KUNDNUMMER                              
002000     03 FILLER               PIC X.                                       
002100     03 IDKUNDNR-TOM         PIC 9(7).                                    
002200*                                 KUNDNUMMER                              
002300     03 FILLER               PIC X.                                       
002400     03 IDSKYLT              PIC X(3).                                    
002500*                                 NATIONALITETSTECKEN                     
002600     03 FILLER               PIC X.                                       
002700     03 KVVECKOR             PIC 9(3).                                    
002800*                                                    KVVECKOR-BYT         
002900*                                 JUSTERAT ANTAL VECKOR SVV               
003000*                                 ( + ELLER - )                           
003100     03 FILLER               PIC X.                                       
003200     03 IDFKNGRP-FOM         PIC 9(5).                                    
003300*                                 FUNKTIONSGRUPP                          
003400     03 FILLER               PIC X.                                       
003500     03 IDFKNGRP-TOM         PIC 9(5).                                    
003600*                                 FUNKTIONSGRUPP                          
003700     03 FILLER               PIC X.                                       
003800     03 IDDISTR-NYTT         PIC 9(5).                                    
003900*                                 NYTT DISTRIKTSNR    IDDISTR-002         
004000     03 FILLER               PIC X.                                       
004100     03 KDANDRING            PIC X.                                       
004200*                                 ÄNDRINGSKOD                             
004300*                                 TILLÄGG   = T                           
004400*                                 BORTTAG   = B                           
004500*                                 JUSTERING = J                           
004600     03 FILLER               PIC X(17).                                   
004700*** END COPY W371861CC0  LENGTH=80                                        
