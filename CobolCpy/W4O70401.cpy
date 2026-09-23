000100 01  MOD-W4O70401.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4070400           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-RELANDCO-ATTR    PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-RELANDCO         PIC Z(2)9.9(2).                              
002200*                                 LANDING COST PROCENT                    
002300     03 MOD-PRFRAKT-ATTR     PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-PRFRAKT          PIC Z(6)9.9(2).                              
002600*                                 FRAKTKOSTNAD                            
002700     03 MOD-PRFOERS-ATTR     PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-PRFOERS          PIC Z(6)9.9(2).                              
003000*                                 FÖRSÄKRINGSPREMIE                       
003100     03 MOD-PRLEGKST-ATTR    PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-PRLEGKST         PIC Z(6)9.9(2).                              
003400*                                 LEGALISERINSKOSTNAD                     
003500     03 MOD-KDVALISO         PIC X(3).                                    
003600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003700     03 MOD-LEVANM-KLAR-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-LEVANM-KLAR      PIC X.                                       
004000*                                 ALLMÄN FLAGGA                           
004100     03 MOD-TEMFSINF         PIC X(55).                                   
004200*                                 INFORMATIONSMEDDELANDE                  
004300     03 MOD-KVRADER          PIC 9(3).                                    
004400*                                 ANTAL RADER                             
004500     03 MOD-INFO-RAD         OCCURS 1 TO 999 TIMES                        
004600                             DEPENDING ON MOD-KVRADER.                    
004700*                                 RADINFORMATION                          
004800        05 MOD-IDORDNR5-ATTR PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-IDORDNR5      PIC Z(4)9.                                   
005100*                                 ORDERNUMMER                             
005200        05 MOD-IDKOLLI-ATTR  PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-IDKOLLI       PIC Z(4)9.                                   
005500*                                 KOLLINUMMER                             
005600        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-IDARTNR       PIC Z(8)9.                                   
005900*                                 ARTIKELNUMMER                           
006000        05 MOD-KVLEVANM-ATTR PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-KVLEVANM      PIC Z(5)9.                                   
006300*                                 LEVERANSANMÄRKNINGSANTAL                
006400        05 MOD-KDANMORS-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-KDANMORS      PIC X(2).                                    
006700*                                 ORSAK TILL LEVERANSANMÄRKNING           
006800        05 MOD-KDEMBLEV-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-KDEMBLEV      PIC 9.                                       
007100*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
007200        05 MOD-PRARTBTO-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-PRARTBTO      PIC Z(6)9.9(2).                              
007500*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
007600        05 MOD-KDFAKTYP-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KDFAKTYP      PIC X.                                       
007900*                                 FAKTURATYP                              
008000        05 MOD-IDFAKT-ATTR   PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-IDFAKT        PIC Z(6)9.                                   
008300*                                 FAKTURANUMMER                           
008400        05 MOD-IDDC-ATTR     PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-IDDC          PIC X(2).                                    
008700*                                 IDENTIFIERARE LAGER                     
008800        05 MOD-IDFELKOD-1    PIC X(3).                                    
008900*                                 FELKOD                                  
009000        05 MOD-IDFELKOD-2    PIC X(3).                                    
009100*                                 FELKOD                                  
009200        05 MOD-IDFELKOD-3    PIC X(3).                                    
009300*                                 FELKOD                                  
009400*** END OF VILMAII-COPY LENGTH= 77109 BYTES                               
