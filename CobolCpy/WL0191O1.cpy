000100 01  RESP-WL0191O1.                                                       
000200*                                 RESPONSE COPYTEXT FOR WL0191            
000300     03 RESP-IDTRPTNR-KEY    PIC Z(2)9.                                   
000400*                                 TRANSPORTIDENTITET                      
000500     03 RESP-IDLBBET-KEY     PIC X(12).                                   
000600*                                 LASTBÄRARBETECKNING                     
000700     03 RESP-FLFARLIG-KEY    PIC X.                                       
000800*                                 FARLIGT GODS-FLAGGA                     
000900     03 RESP-IDDC-KEY        PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 RESP-IDDISTR         PIC Z(3)9.                                   
001200*                                 DISTRIKTNUMMER                          
001300     03 RESP-IDKUNDNR        PIC Z(5)9.                                   
001400*                                 KUNDNUMMER                              
001500     03 RESP-IDSHIPM         PIC Z(6)9.                                   
001600*                                 SKEPPNINGSNUMMER                        
001700     03 RESP-FLSEPINV        PIC X.                                       
001800*                                 SEPARAT FAKTURA                         
001900     03 RESP-CURRENCY-MC     PIC X(11).                                   
002000     03 RESP-KDVALISO-MC     PIC X(3).                                    
002100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002200     03 RESP-RADER.                                                       
002300*                                                                         
002400        05 RESP-PRFRAKT      PIC Z(6)9.9(2).                              
002500*                                 FRAKTKOSTNAD                            
002600        05 RESP-PRFOERS      PIC Z(6)9.9(2).                              
002700*                                 FÖRSÄKRINGSPREMIE                       
002800        05 RESP-REFOERS      PIC Z9.9(3).                                 
002900*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
003000        05 RESP-REOVKOFF     PIC Z9.9.                                    
003100*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
003200        05 RESP-PRLEGKST     PIC Z(6)9.9(2).                              
003300*                                 LEGALISERINSKOSTNAD                     
003400        05 RESP-RELEGKST     PIC Z9.9.                                    
003500*                                 LEGALISERINGSKOSTNAD PROCENT            
003600        05 RESP-PREMBHNT     PIC Z(6)9.9(2).                              
003700*                                 EMBALLAGE O HANTERINGSKOST              
003800        05 RESP-REEMBHNT     PIC Z9.9.                                    
003900*                                 EMB OCH HANTERINGSKOST (%)              
004000        05 RESP-PRAVDRAG     PIC Z(6)9.9(2).                              
004100*                                 AVDRAGSBELOPP                           
004200        05 RESP-REAVDRAG     PIC Z9.9.                                    
004300*                                 AVDRAGSPROCENT                          
004400        05 RESP-FLCREDL      PIC X.                                       
004500        05 RESP-KDVALUTA-MAN PIC 9(2).                                    
004600*                                 VALUTAKOD MANUELL                       
004700        05 RESP-KDVALISO-MAN PIC X(3).                                    
004800*                                 VALUTAKOD ENLIGT ISO-ST, ANGIVE         
004900*                                 N AV ANVÄNDARE                          
005000        05 RESP-PRKURS-MAN   PIC Z(5)9.9(4).                              
005100*                                 VALUTAKURS ANGIVEN AV ANVÄNDARE         
005200        05 RESP-IDSIGILL     PIC X(15).                                   
005300*                                 FÄRDVÄG, DESTINATION                    
005400        05 RESP-TISKEPPN-MAN PIC 9(6).                                    
005500*                                 SKEPPNINGSDATUM MAN  (ÅÅMMDD)           
005600        05 RESP-IDLC         PIC X(15).                                   
005700*                                 LC-NUMMER                               
005800        05 RESP-IDLICENS     PIC X(15).                                   
005900*                                 LICENSNUMMER                            
006000        05 RESP-IDBOKN       PIC X(15).                                   
006100*                                 BOKNINGSNUMMER                          
006200        05 RESP-IDVCERT      PIC X(16).                                   
006300*                                 VARUCERTIFIKATNUMMER                    
006400        05 RESP-BESLULEV     PIC X(5).                                    
006500*                                 SLUTLEVERANS                            
006600        05 RESP-KDLEVVIL     PIC 9.                                       
006700*                                 LEVERANSVILLKOR                         
006800        05 RESP-VKORDBTO     PIC Z(5)9.9.                                 
006900*                                 ORDERVIKT BRUTTO (KG)                   
007000        05 RESP-VLORDBTO     PIC Z(3)9.9(3).                              
007100*                                 ORDERVOLYM BRUTTO (M3)                  
007200        05 RESP-SUORDV       PIC Z(8)9.9(2).                              
007300*                                 SUMMA ORDERVÄRDE                        
007400*** END OF VILMAII-COPY LENGTH= 270 BYTES                                 
