000100 01  W37186.                                                              
000200*                                 STYRREG FÖR BYTES-DISTRIKTEN            
000300*                                                                         
000400     03 SORTKEY.                                                          
000500*                                                                         
000600        05 IDPTYP            PIC X(3).                                    
000700*                                 POSTTYP                                 
000800        05 IDLOPNR           PIC S9(3)           COMP-3.                  
000900*                                 LÖPNUMMER                               
001000     03 IDDISTR-FOM          PIC S9(5)           COMP-3.                  
001100*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001200     03 IDDISTR-TOM          PIC S9(5)           COMP-3.                  
001300*                                 HÖGSTA DISTRIKTNR I INTERVALL           
001400     03 IDKUNDNR-FOM         PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 IDKUNDNR-TOM         PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800     03 IDSKYLT              PIC X(3).                                    
001900*                                 NATIONALITETSTECKEN                     
002000     03 KVVECKOR             PIC S9(3)           COMP-3.                  
002100*                                 ANTAL VECKOR                            
002200     03 IDFKNGRP-FOM         PIC S9(5)           COMP-3.                  
002300*                                 FUNKTIONSGRUPP                          
002400     03 IDFKNGRP-TOM         PIC S9(5)           COMP-3.                  
002500*                                 FUNKTIONSGRUPP                          
002600     03 IDDISTR-NYTT         PIC S9(5)           COMP-3.                  
002700*                                 NYTT DISTRIKTSNR    IDDISTR-002         
002800     03 KDANDRING            PIC X.                                       
002900*                                 ÄNDRINGSKOD                             
003000*                                 TILLÄGG   = T                           
003100*                                 BORTTAG   = B                           
003200*                                 JUSTERING = J                           
003300*** END COPY W37186CCC0  LENGTH=34                                        
