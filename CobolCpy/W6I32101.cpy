000100 01  MID-W6I32101.                                                        
000200*                                 COPYTEXT FÖR MID W6I32101               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDARTNR-UT       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT-RETUR.                                                  
001200*                                                                         
001300        05 MID-IDDC-SEND-TO  PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500        05 MID-KVRETUR-BEORD PIC X(7).                                    
001600*                                 ANTAL I RETUR                           
001700        05 MID-KDORDKL       PIC X.                                       
001800*                                 ORDERKLASS                              
001900        05 MID-RETUR-TEXT    PIC X(60).                                   
002000     03 MID-INPUT-SKROT.                                                  
002100*                                                                         
002200        05 MID-KVSKROT       PIC X(7).                                    
002300*                                 ANTAL SENASTE SKROTORDER                
002400        05 MID-SKROT-TEXT    PIC X(60).                                   
002500        05 MID-IDPERSON-QUAL PIC X(3).                                    
002600*                                 PERSONKOD                               
002700        05 MID-IDPERSON-ESC  PIC X(3).                                    
002800*                                 PERSONKOD                               
002900        05 MID-IDKONTO       PIC X(10).                                   
003000*                                 KONTO                                   
003100        05 MID-IDANALYS      PIC X(12).                                   
003200*                                 ANALYSNUMMER                            
003300     03 MID-FLJUSTBUFF       PIC X.                                       
003400*                                 JUSTERA BUFFERT                         
003500*** END OF VILMAII-COPY LENGTH= 188 BYTES                                 
