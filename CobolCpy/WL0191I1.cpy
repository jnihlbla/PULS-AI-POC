000100 01  REQU-WL0191I1.                                                       
000200*                                 REQUEST COPYTEXT FOR WL0191             
000300     03 REQU-IDTRPTNR-KEY    PIC 9(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 REQU-IDLBBET-KEY     PIC X(12).                                   
000600*                                 LASTBÄRARBETECKNING                     
000700     03 REQU-FLFARLIG-KEY    PIC X.                                       
000800*                                 FARLIGT GODS-FLAGGA                     
000900     03 REQU-IDDC-KEY        PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 REQU-IDDISTR         PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 REQU-IDKUNDNR        PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 REQU-IDSHIPM         PIC 9(7).                                    
001600*                                 SKEPPNINGSNUMMER                        
001700     03 REQU-FLSKRIV-NU      PIC X.                                       
001800*                                 J/Y = SKRIV BEGÄRD LISTA                
001900     03 REQU-FLSEPINV        PIC X.                                       
002000*                                 SEPARAT FAKTURA                         
002100     03 REQU-PRFRAKT         PIC X(10).                                   
002200*                                 FRAKTKOSTNAD                            
002300     03 REQU-FLCREDL         PIC X.                                       
002400     03 REQU-VALUTA.                                                      
002500*                                                                         
002600        05 REQU-KDVALUTA-MAN PIC X(2).                                    
002700*                                 VALUTAKOD MANUELL                       
002800        05 REQU-PRKURS-MAN   PIC X(11).                                   
002900*                                 VALUTAKURS ANGIVEN AV ANVÄNDARE         
003000     03 REQU-LEGKST.                                                      
003100*                                                                         
003200        05 REQU-PRLEGKST     PIC X(10).                                   
003300*                                 LEGALISERINSKOSTNAD                     
003400        05 REQU-RELEGKST     PIC X(4).                                    
003500*                                 LEGALISERINGSKOSTNAD PROCENT            
003600     03 REQU-EMBHNT.                                                      
003700*                                                                         
003800        05 REQU-PREMBHNT     PIC X(10).                                   
003900*                                 EMBALLAGE O HANTERINGSKOST              
004000        05 REQU-REEMBHNT     PIC X(4).                                    
004100*                                 EMB OCH HANTERINGSKOST (%)              
004200     03 REQU-FOERS.                                                       
004300*                                                                         
004400        05 REQU-PRFOERS      PIC X(10).                                   
004500*                                 FÖRSÄKRINGSPREMIE                       
004600        05 REQU-REFOERS-OVKOFF.                                           
004700*                                                                         
004800           07 REQU-REFOERS   PIC X(6).                                    
004900*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
005000           07 REQU-REOVKOFF  PIC X(4).                                    
005100*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
005200     03 REQU-AVDRAG.                                                      
005300*                                                                         
005400        05 REQU-PRAVDRAG     PIC X(10).                                   
005500*                                 AVDRAGSBELOPP                           
005600        05 REQU-REAVDRAG     PIC X(4).                                    
005700*                                 AVDRAGSPROCENT                          
005800     03 REQU-IDSIGILL        PIC X(15).                                   
005900*                                 FÄRDVÄG, DESTINATION                    
006000     03 REQU-TISKEPPN-MAN    PIC X(6).                                    
006100*                                 SKEPPNINGSDATUM MAN  (ÅÅMMDD)           
006200     03 REQU-IDLC            PIC X(15).                                   
006300*                                 LC-NUMMER                               
006400     03 REQU-IDLICENS        PIC X(15).                                   
006500*                                 LICENSNUMMER                            
006600     03 REQU-IDBOKN          PIC X(15).                                   
006700*                                 BOKNINGSNUMMER                          
006800     03 REQU-IDVCERT         PIC X(16).                                   
006900*                                 VARUCERTIFIKATNUMMER                    
007000     03 REQU-BESLULEV        PIC X(5).                                    
007100*                                 SLUTLEVERANS                            
007200     03 REQU-KDLEVVIL        PIC X.                                       
007300*                                 LEVERANSVILLKOR                         
007400*** END OF VILMAII-COPY LENGTH= 226 BYTES                                 
