000100 01  MOD-W4O20401.                                                        
000200*                                 MODCOPYTEXT TILL W40204.                
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
001500     03 MOD-IDKUNDRF-IN      PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MOD-IDKUNDRF-UT      PIC X(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-IDARTNR-IN       PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MOD-IDARTNR-UT       PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-TEDDI            PIC X(11).                                   
002800*                                 TEXTFÄLT DDI                            
002900     03 MOD-IDORDER-ENTER    PIC 9(7).                                    
003000*                                 VOLVO PARTS ORDERNUMMER                 
003100     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
003200*                                 VOLVO PARTS ORDERNUMMER                 
003300     03 MOD-IDDC-ENTER       PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-IDDC-NEXT        PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700     03 MOD-ADLAGOMR-ENTER   PIC Z(2)9.                                   
003800*                                 LAGEROMRÅDE                             
003900     03 MOD-ADLAGOMR-NEXT    PIC Z(2)9.                                   
004000*                                 LAGEROMRÅDE                             
004100     03 MOD-ADGANG-ENTER     PIC Z(2)9.                                   
004200*                                 GÅNG                                    
004300     03 MOD-ADGANG-NEXT      PIC Z(2)9.                                   
004400*                                 GÅNG                                    
004500     03 MOD-ADPLATS-ENTER    PIC 9(5).                                    
004600*                                 LAGERPLATSNUMMER                        
004700     03 MOD-ADPLATS-NEXT     PIC 9(5).                                    
004800*                                 LAGERPLATSNUMMER                        
004900     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
005000*                                 ARTIKELNUMMER                           
005100     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
005200*                                 ARTIKELNUMMER                           
005300     03 MOD-IDLOPNR-ENTER    PIC 9(3).                                    
005400*                                 LÖPNUMMER                               
005500     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
005600*                                 LÖPNUMMER                               
005700     03 MOD-FLAGGA-UPDATE-ATTR                                            
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-FLAGGA-UPDATE    PIC X.                                       
006100*                                 JA/NEJ-FLAGGA                           
006200     03 MOD-KVORDRAD         PIC Z(4)9.                                   
006300*                                 ANTAL ORDERRADER                        
006400     03 MOD-KVRADER          PIC Z(4)9.                                   
006500*                                 ANTAL RADER                             
006600     03 MOD-ADLAGOMR-SPAR    OCCURS 6 TIMES                               
006700                             PIC 9(3).                                    
006800*                                 LAGEROMRÅDE                             
006900     03 MOD-ADGANG-SPAR      OCCURS 6 TIMES                               
007000                             PIC 9(3).                                    
007100*                                 GÅNG                                    
007200     03 MOD-KDVALISO         PIC X(3).                                    
007300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007400     03 MOD-UPDATE           OCCURS 6 TIMES.                              
007500*                                 TABELL-UPDATE                           
007600        05 MOD-CMD-UPDATE-ATTR                                            
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-CMD-UPDATE    PIC X.                                       
008000     03 MOD-IDARTNR-RAD      OCCURS 6 TIMES                               
008100                             PIC 9(9).                                    
008200*                                 ARTIKELNUMMER                           
008300     03 MOD-KVBEART-Q-RAD    OCCURS 6 TIMES                               
008400                             PIC Z(6)9.                                   
008500*                                 ANTAL ARTNR PER BRYTBEGREPP             
008600     03 MOD-PRARTNTO-RAD     OCCURS 6 TIMES                               
008700                             PIC Z(6)9.9(2).                              
008800*                                 ARTIKELPRIS NETTO                       
008900     03 MOD-TEASTRIX-RAD     OCCURS 6 TIMES                               
009000                             PIC X.                                       
009100*                                 ASTERISK                                
009200     03 MOD-BEART-RAD        OCCURS 6 TIMES                               
009300                             PIC X(13).                                   
009400     03 MOD-OREF-RAD         OCCURS 6 TIMES                               
009500                             PIC 9(7).                                    
009600*                                 ORDERNUMMER                             
009700     03 MOD-BERADREF-RAD     OCCURS 6 TIMES                               
009800                             PIC X(10).                                   
009900*                                 KUNDENS RADREFERENS                     
010000     03 MOD-IDDC-RAD         OCCURS 6 TIMES                               
010100                             PIC X(2).                                    
010200*                                 IDENTIFIERARE LAGER                     
010300     03 MOD-UPDATE           OCCURS 6 TIMES.                              
010400*                                 TABELL-UPDATE                           
010500        05 MOD-KVBEART-Q-UPDATE-ATTR                                      
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-KVBEART-Q-UPDATE                                           
010900                             PIC X(2).                                    
011000*                                 MFS BEHANDLING AV INPUTFÄLT             
011100     03 MOD-UPDATE           OCCURS 6 TIMES.                              
011200*                                 TABELL-UPDATE                           
011300        05 MOD-PRARTNTO-UPDATE-ATTR                                       
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 MOD-PRARTNTO-UPDATE                                            
011700                             PIC X(2).                                    
011800*                                 MFS BEHANDLING AV INPUTFÄLT             
011900     03 MOD-UPDATE           OCCURS 6 TIMES.                              
012000*                                 TABELL-UPDATE                           
012100        05 MOD-BERADREF-UPDATE-ATTR                                       
012200                             PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400        05 MOD-BERADREF-UPDATE                                            
012500                             PIC X(2).                                    
012600*                                 MFS BEHANDLING AV INPUTFÄLT             
012700     03 MOD-IDLOPNR-SPAR     OCCURS 6 TIMES                               
012800                             PIC 9(3).                                    
012900*                                 LÖPNUMMER                               
013000     03 MOD-ADPLATS-SPAR     OCCURS 6 TIMES                               
013100                             PIC 9(5).                                    
013200*                                 LAGERPLATSNUMMER                        
013300     03 MOD-IDARTNR-SPAR     OCCURS 6 TIMES                               
013400                             PIC 9(9).                                    
013500*                                 ARTIKELNUMMER                           
013600     03 MOD-TEMFSINF         PIC X(55).                                   
013700*                                 INFORMATIONSMEDDELANDE                  
013800*** END OF VILMAII-COPY LENGTH= 828 BYTES                                 
