000100 01  MID-W2I35901-CTX.                                                    
000200*                                 COPYTEXT FÖR MID W2I35901               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDARTNR-UT       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-W2I35901-001-GRP.                                             
001200*                                                                         
001300        05 MID-IDDC-SEND-TO  PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500        05 MID-KVRETUR-TRANSFER                                           
001600                             PIC X(6).                                    
001700*                                 ANTAL I RETUR                           
001800        05 MID-KDORDKL       PIC X.                                       
001900*                                 ORDERKLASS                              
002000        05 MID-KDFRAKT       PIC X(2).                                    
002100*                                 FRAKTSÄTT DC TILL KUND                  
002200        05 MID-FLTOT         PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400        05 MID-TRANSFER-TEXT-1                                            
002500                             PIC X(30).                                   
002600        05 MID-TRANSFER-TEXT-2                                            
002700                             PIC X(30).                                   
002800     03 MID-W2I35901-002-GRP.                                             
002900*                                                                         
003000        05 MID-KVSKROT-KVAR  PIC X(7).                                    
003100*                                 KVARLIGGANDE ANTAL                      
003200        05 MID-SKROT-TEXT    PIC X(20).                                   
003300     03 MID-W2I35901-003-GRP.                                             
003400*                                                                         
003500        05 MID-IDKONTO       PIC X(10).                                   
003600*                                 KONTO                                   
003700        05 MID-IDANALYS      PIC X(12).                                   
003800*                                 ANALYSNUMMER                            
003900     03 MID-W2I35901-004-GRP.                                             
004000*                                                                         
004100        05 MID-TISKROT-AUTO-IN                                            
004200                             PIC 9(6).                                    
004300*                                 STOPDATE AUTO-SKROTNING                 
004400     03 MID-FLSKROT-BEORD-IN PIC X.                                       
004500*                                 SKROTNING BEORDRAD AV ANSK              
004600     03 MID-FLTEXT           PIC X.                                       
004700*                                 FINNS TEXTINFORMATION ?                 
004800*** END OF VILMAII-COPY LENGTH= 151 BYTES                                 
