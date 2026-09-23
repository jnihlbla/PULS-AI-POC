000100 01  MOD-W6O15201.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-ADLASTPL-IN      PIC X(3).                                    
000800*                                 LASTPLATS                               
000900     03 MOD-ADLASTPL-UT      PIC X(3).                                    
001000*                                 LASTPLATS                               
001100     03 MOD-KDTRPSTA-IN      PIC X.                                       
001200*                                 TRANSPORTSTATUS                         
001300     03 MOD-KDTRPSTA-UT      PIC X.                                       
001400*                                 TRANSPORTSTATUS                         
001500     03 MOD-IDARTNR-IN       PIC Z(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC Z(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
002000*                                 GRUPP MED TABELLRADER                   
002100        05 MOD-CMD-ATTR      PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-CMD           PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500        05 MOD-IDARTNR       PIC Z(8)9.                                   
002600*                                 ARTIKELNUMMER                           
002700        05 MOD-KVANTAL       PIC Z(5)9.                                   
002800*                                 ANTAL                                   
002900        05 MOD-ADLAGOMR      PIC Z9.                                      
003000*                                 LAGEROMRÅDE                             
003100        05 MOD-ADGANG        PIC Z9.                                      
003200*                                 GÅNG                                    
003300        05 MOD-ADPLATS       PIC Z(4)9.                                   
003400*                                 LAGERPLATSNUMMER                        
003500        05 MOD-BEFT          PIC Z9.                                      
003600*                                 FÖRPACKNINGSTYP                         
003700        05 MOD-TIDATUM       PIC 9(6).                                    
003800*                                 DATUM ENLIGT KDDATFORM                  
003900        05 MOD-KDTRPSTA-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-KDTRPSTA      PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300        05 MOD-IDUSER        PIC X(8).                                    
004400*                                 ANVÄNDARENS SÄKERHETS ID                
004500        05 MOD-TETRPMED      PIC X(20).                                   
004600*                                 TEXT VID TRANSPORTBEGÄRAN               
004700     03 MOD-KVANTAL-NY-ATTR  PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KVANTAL-NY       PIC Z(5)9.                                   
005000*                                 ANTAL                                   
005100     03 MOD-LAST-KLAR-ATTR   PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-LAST-KLAR        PIC X(2).                                    
005400*                                 MFS BEHANDLING AV INPUTFÄLT             
005500     03 MOD-PRINTER-ATTR     PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-PRINTER          PIC X(4).                                    
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 1027 BYTES                                
