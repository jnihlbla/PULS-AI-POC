000100 01  RESP-W4O31901.                                                       
000200*                                 RESPONS FROM PGM W40319                 
000300     03 RESP-IDDC            PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSTNR        PIC X(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 RESP-IDPRCPLK        PIC X(4).                                    
000800*                                 ID FÖR EN PLOCKRUNDA                    
000900     03 RESP-IDLOTNR-PLK     PIC 9(3).                                    
001000*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
001100     03 RESP-KVRADER         PIC 9(5).                                    
001200*                                 ANTAL RADER                             
001300     03 RESP-RAD             OCCURS 500 TIMES.                            
001400        05 RESP-IDPRODNR     PIC 9(7).                                    
001500*                                 PRODUKTIONSNUMMER                       
001600        05 RESP-IDPLKLST     PIC 9(3).                                    
001700*                                 PLOCKLISTNUMMER                         
001800        05 RESP-IDKOLLI      PIC 9(5).                                    
001900*                                 KOLLINUMMER                             
002000        05 RESP-IDARTNR      PIC 9(9).                                    
002100*                                 ARTIKELNUMMER                           
002200        05 RESP-ADLAGOMR     PIC 9(2).                                    
002300*                                 LAGEROMRÅDE                             
002400        05 RESP-IDRADNR      PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600        05 RESP-FLNOLLJ      PIC X.                                       
002700*                                 UPPDATERAD AV NOLLJAGARE                
002800        05 RESP-KVAVBART     PIC 9(6).                                    
002900*                                 AVBOKAT ANTAL ARTIKLAR                  
003000        05 RESP-KVORAPP      PIC 9(6).                                    
003100*                                 EJ-RAPPORTERAT-ANTAL                    
003200        05 RESP-IDMSG-ERR-LINE                                            
003300                             PIC X(3).                                    
003400*                                 FELMEDDELANDE ID                        
003500        05 RESP-FLSVAR       PIC X.                                       
003600*                                 ALLMÄN SVARSFLAGGA                      
003700*** END OF VILMAII-COPY LENGTH= 23519 BYTES                               
