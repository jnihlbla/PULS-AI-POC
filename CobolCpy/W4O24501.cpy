000100 01  MOD-W4O24501.                                                        
000200*                                 MODCOPYTEXT TILL W40245.                
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
002700     03 MOD-IDORDER-ENTER    PIC 9(7).                                    
002800*                                 VOLVO PARTS ORDERNUMMER                 
002900     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
003000*                                 VOLVO PARTS ORDERNUMMER                 
003100     03 MOD-IDDC-ENTER       PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MOD-IDDC-NEXT        PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-ADLAGOMR-ENTER   PIC Z(2)9.                                   
003600*                                 LAGEROMRÅDE                             
003700     03 MOD-ADLAGOMR-NEXT    PIC Z(2)9.                                   
003800*                                 LAGEROMRÅDE                             
003900     03 MOD-ADGANG-ENTER     PIC Z(2)9.                                   
004000*                                 GÅNG                                    
004100     03 MOD-ADGANG-NEXT      PIC Z(2)9.                                   
004200*                                 GÅNG                                    
004300     03 MOD-ADPLATS-ENTER    PIC 9(5).                                    
004400*                                 LAGERPLATSNUMMER                        
004500     03 MOD-ADPLATS-NEXT     PIC 9(5).                                    
004600*                                 LAGERPLATSNUMMER                        
004700     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
004800*                                 ARTIKELNUMMER                           
004900     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
005000*                                 ARTIKELNUMMER                           
005100     03 MOD-IDLOPNR-ENTER    PIC 9(3).                                    
005200*                                 LÖPNUMMER                               
005300     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
005400*                                 LÖPNUMMER                               
005500     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
005600*                                 PRODUKTIONSNUMMER                       
005700     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
005800*                                 PRODUKTIONSNUMMER                       
005900     03 MOD-IDPLKLST-ENTER   PIC 9(3).                                    
006000*                                 PLOCKLISTNUMMER                         
006100     03 MOD-IDPLKLST-NEXT    PIC 9(3).                                    
006200*                                 PLOCKLISTNUMMER                         
006300     03 MOD-IDPURAD-ENTER    PIC 9(5).                                    
006400*                                 RADNUMMER                               
006500     03 MOD-IDPURAD-NEXT     PIC 9(5).                                    
006600*                                 RADNUMMER                               
006700     03 MOD-FLAGGA-UPDATE-ATTR                                            
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-FLAGGA-UPDATE    PIC X.                                       
007100*                                 JA/NEJ-FLAGGA                           
007200     03 MOD-KVORDRAD         PIC Z(4)9.                                   
007300*                                 ANTAL ORDERRADER                        
007400     03 MOD-KVRADER          PIC Z(4)9.                                   
007500*                                 ANTAL RADER                             
007600     03 MOD-ADLAGOMR-SPAR    OCCURS 6 TIMES                               
007700                             PIC 9(3).                                    
007800*                                 LAGEROMRÅDE                             
007900     03 MOD-ADGANG-SPAR      OCCURS 6 TIMES                               
008000                             PIC 9(3).                                    
008100*                                 GÅNG                                    
008200     03 MOD-UPDATE           OCCURS 6 TIMES.                              
008300*                                 TABELL-UPDATE                           
008400        05 MOD-CMD-UPDATE-ATTR                                            
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 MOD-CMD-UPDATE    PIC X.                                       
008800     03 MOD-IDARTNR-RAD      OCCURS 6 TIMES                               
008900                             PIC 9(9).                                    
009000*                                 ARTIKELNUMMER                           
009100     03 MOD-KVBEART-Q-RAD    OCCURS 6 TIMES                               
009200                             PIC Z(6)9.                                   
009300*                                 ANTAL ARTNR PER BRYTBEGREPP             
009400     03 MOD-BEART-RAD        OCCURS 6 TIMES                               
009500                             PIC X(25).                                   
009600*                                 ARTIKELBENÄMNING                        
009700     03 MOD-OREF-RAD         OCCURS 6 TIMES                               
009800                             PIC 9(7).                                    
009900*                                 ORDERNUMMER                             
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
011100     03 MOD-IDLOPNR-SPAR     OCCURS 6 TIMES                               
011200                             PIC 9(3).                                    
011300*                                 LÖPNUMMER                               
011400     03 MOD-ADPLATS-SPAR     OCCURS 6 TIMES                               
011500                             PIC 9(5).                                    
011600*                                 LAGERPLATSNUMMER                        
011700     03 MOD-IDARTNR-SPAR     OCCURS 6 TIMES                               
011800                             PIC 9(9).                                    
011900*                                 ARTIKELNUMMER                           
012000     03 MOD-IDPRODNR-SPAR    OCCURS 6 TIMES                               
012100                             PIC 9(7).                                    
012200*                                 PRODUKTIONSNUMMER                       
012300     03 MOD-IDPLKLST-SPAR    OCCURS 6 TIMES                               
012400                             PIC 9(3).                                    
012500*                                 PLOCKLISTNUMMER                         
012600     03 MOD-IDPURAD-SPAR     OCCURS 6 TIMES                               
012700                             PIC 9(5).                                    
012800*                                 RADNUMMER                               
012900     03 MOD-TEMFSINF         PIC X(55).                                   
013000*                                 INFORMATIONSMEDDELANDE                  
013100*** END OF VILMAII-COPY LENGTH= 832 BYTES                                 
