000100 01  MOD-W4O26701-CTX.                                                    
000200*                                 MOD-COPYTEXT FÖR W4026700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDARTNR-IN       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDDISTR-UT       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR7-UT      PIC X(7).                                    
002000*                                 ORDERNUMMER                             
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-TEDDI            PIC X(11).                                   
002400*                                 TEXTFÄLT DDI                            
002500     03 MOD-SUORDV           PIC Z(8)9.9(2).                              
002600*                                 SUMMA ORDERVÄRDE                        
002700     03 MOD-TEASTRIX-SUORDV  PIC X.                                       
002800*                                 ASTERISK                                
002900     03 MOD-SUFOBV           PIC Z(8)9.9(2).                              
003000*                                 VARUVÄRDE + FRAKTKOSTNAD                
003100     03 MOD-TEASTRIX-SUFOBV  PIC X.                                       
003200*                                 ASTERISK                                
003300     03 MOD-SUFKTBEL         PIC Z(8)9.9(2).                              
003400*                                 SUMMA FAKTURERAT BELOPP                 
003500     03 MOD-TEASTRIX-SUFKTB  PIC X.                                       
003600*                                 ASTERISK                                
003700     03 MOD-PRMOMS           PIC Z(6)9.9(2).                              
003800*                                 MERVÄRDESSKATT                          
003900     03 MOD-PREMBHNT         PIC Z(6)9.9(2).                              
004000*                                 EMBALLAGE O HANTERINGSKOST              
004100     03 MOD-PRAVDRAG-ATTR    PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-PRAVDRAG         PIC Z(6)9.9(2).                              
004400*                                 AVDRAGSBELOPP                           
004500     03 MOD-REAVDRAG-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-REAVDRAG         PIC Z9.9.                                    
004800*                                 AVDRAGSPROCENT                          
004900     03 MOD-PRFRAKT-ATTR     PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-PRFRAKT          PIC Z(6)9.9(2).                              
005200*                                 FRAKTKOSTNAD                            
005300     03 MOD-PRFOERS-ATTR     PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-PRFOERS          PIC Z(6)9.9(2).                              
005600*                                 FÖRSÄKRINGSPREMIE                       
005700     03 MOD-REFOERS-ATTR     PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-REFOERS          PIC Z9.9(3).                                 
006000*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
006100     03 MOD-REOVKOFF-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-REOVKOFF         PIC Z9.9.                                    
006400*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
006500     03 MOD-PRLEGKST-ATTR    PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-PRLEGKST         PIC Z(6)9.9(2).                              
006800*                                 LEGALISERINSKOSTNAD                     
006900     03 MOD-KDVALUTA-ATTR    PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-KDVALUTA         PIC Z(2)9.                                   
007200*                                 VALUTAKOD                               
007300     03 MOD-PRKURS-ATTR      PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-PRKURS           PIC Z(5)9.9(4).                              
007600*                                 VALUTAKURS                              
007700     03 MOD-SUFKTBEL-UTL     PIC Z(8)9.9(2).                              
007800*                                 SUMMA FAKTURERAT BELOPP                 
007900     03 MOD-BEVALUTA         PIC X(3).                                    
008000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008100     03 MOD-BELOSORT-ATTR    PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-BELOSORT         PIC X(17).                                   
008400*                                 LOSSNININGSORT NAMN                     
008500     03 MOD-TEBANKTO-ATTR    PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-TEBANKTO         PIC X(20).                                   
008800*                                 BANKKONTO                               
008900     03 MOD-TEBANK-RAD1-ATTR PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-TEBANK-RAD1      PIC X(72).                                   
009200*                                 BANKINFORMATION                         
009300     03 MOD-TEBANK-RAD2-ATTR PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-TEBANK-RAD2      PIC X(72).                                   
009600*                                 BANKINFORMATION                         
009700     03 MOD-TEMFSINF         PIC X(55).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END OF VILMAII-COPY LENGTH= 511 BYTES                                 
