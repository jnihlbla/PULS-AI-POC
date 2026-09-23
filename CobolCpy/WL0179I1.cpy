000100 01  REQU-WL0179I1.                                                       
000200*                                 REQUEST TP PGM WL0179                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-ADLAGOMR-KEY    PIC 9(2).                                    
000600*                                 LAGEROMRÅDE                             
000700     03 REQU-ADGANG-KEY      PIC 9(2).                                    
000800*                                 GÅNG                                    
000900     03 REQU-ADPLATS-KEY     PIC 9(5).                                    
001000*                                 LAGERPLATSNUMMER                        
001100     03 REQU-IDPRTOMG-KEY    PIC 9.                                       
001200*                                 PRINT OMGÅNG FÖR AUT.JUSTERING          
001300     03 REQU-FLINVSKR-KEY    PIC X.                                       
001400*                                 INVENTERINGSANMODAN UTSKRIVEN           
001500     03 REQU-KVINVSKR-5309   PIC 9(2).                                    
001600*                                 BEGÄRDA INVENTERINGSUNDERLAG            
001700     03 REQU-KVRADER         PIC 9(5).                                    
001800*                                 ANTAL RADER                             
001900     03 REQU-INV-ART-GRP     OCCURS 500 TIMES.                            
002000*                                 INVENTERINGSARTIKELGRUPP                
002100        05 REQU-IDARTNR-UTSKR                                             
002200                             PIC 9(9).                                    
002300*                                 ARTIKELNUMMER                           
002400        05 REQU-KDINVPRIO-UTSKR                                           
002500                             PIC X.                                       
002600*                                 INVENTERING PRIORITET                   
002700        05 REQU-KDINVKAT-UTSKR                                            
002800                             PIC 9(2).                                    
002900*                                 INVENTERINGSKATEGORI                    
003000*** END OF VILMAII-COPY LENGTH= 6020 BYTES                                
