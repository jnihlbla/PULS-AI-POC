000100 01  MID-W4I67601.                                                        
000200*                                 MID-COPYTEXT FÖR W40676                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 MID-IDLBBET-IN       PIC X(12).                                   
000600*                                 LASTBÄRARBETECKNING                     
000700     03 MID-FLFARLIG-IN      PIC X.                                       
000800*                                 FARLIGT GODS-FLAGGA                     
000900     03 MID-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-FLSEPINV         PIC X.                                       
001200*                                 SEPARAT FAKTURA                         
001300     03 MID-PRFRAKT          PIC X(10).                                   
001400*                                 FRAKTKOSTNAD                            
001500     03 MID-FLFRAKT-UPD      PIC X.                                       
001600     03 MID-FLCREDL          PIC X.                                       
001700     03 MID-VALUTA.                                                       
001800*                                                                         
001900        05 MID-KDVALUTA-MAN  PIC X(2).                                    
002000*                                 VALUTAKOD MANUELL                       
002100        05 MID-PRKURS-MAN    PIC X(11).                                   
002200*                                 VALUTAKURS ANGIVEN AV ANVÄNDARE         
002300     03 MID-LEGKST.                                                       
002400*                                                                         
002500        05 MID-PRLEGKST      PIC X(10).                                   
002600*                                 LEGALISERINSKOSTNAD                     
002700        05 MID-RELEGKST      PIC X(4).                                    
002800*                                 LEGALISERINGSKOSTNAD PROCENT            
002900     03 MID-EMBHNT.                                                       
003000*                                                                         
003100        05 MID-PREMBHNT      PIC X(10).                                   
003200*                                 EMBALLAGE O HANTERINGSKOST              
003300        05 MID-REEMBHNT      PIC X(4).                                    
003400*                                 EMB OCH HANTERINGSKOST (%)              
003500     03 MID-FOERS.                                                        
003600*                                                                         
003700        05 MID-PRFOERS       PIC X(10).                                   
003800*                                 FÖRSÄKRINGSPREMIE                       
003900        05 MID-REFOERS-OVKOFF.                                            
004000*                                                                         
004100           07 MID-REFOERS    PIC X(6).                                    
004200*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
004300           07 MID-REOVKOFF   PIC X(4).                                    
004400*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
004500     03 MID-AVDRAG.                                                       
004600*                                                                         
004700        05 MID-PRAVDRAG      PIC X(10).                                   
004800*                                 AVDRAGSBELOPP                           
004900        05 MID-REAVDRAG      PIC X(4).                                    
005000*                                 AVDRAGSPROCENT                          
005100     03 MID-IDSIGILL         PIC X(15).                                   
005200*                                 FÄRDVÄG, DESTINATION                    
005300     03 MID-TISKEPPN-MAN     PIC X(6).                                    
005400*                                 SKEPPNINGSDATUM MAN  (ÅÅMMDD)           
005500     03 MID-IDLC             PIC X(15).                                   
005600*                                 LC-NUMMER                               
005700     03 MID-IDLICENS         PIC X(15).                                   
005800*                                 LICENSNUMMER                            
005900     03 MID-IDBOKN           PIC X(15).                                   
006000*                                 BOKNINGSNUMMER                          
006100     03 MID-IDVCERT          PIC X(16).                                   
006200*                                 VARUCERTIFIKATNUMMER                    
006300     03 MID-BESLULEV         PIC X(5).                                    
006400*                                 SLUTLEVERANS                            
006500     03 MID-KDLEVVIL         PIC X.                                       
006600*                                 LEVERANSVILLKOR                         
006700*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 
