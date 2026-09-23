000100 01  MOD-W6O30301.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFAKT-IN        PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900     03 MOD-IDFAKT-UT        PIC X(7).                                    
001000*                                 FAKTURANUMMER                           
001100     03 MOD-IDKUNDRF-IN      PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300     03 MOD-IDKUNDRF-UT      PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDKOLLI-IN       PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDSPRAK-IN       PIC X(3).                                    
002400*                                 NATIONALITETSTECKEN                     
002500*                                 SPRÅKIDENTIFIKATION                     
002600     03 MOD-IDSPRAK-UT       PIC X(3).                                    
002700*                                 NATIONALITETSTECKEN                     
002800*                                 SPRÅKIDENTIFIKATION                     
002900     03 MOD-IDDC-IN          PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MOD-IDDC-UT          PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MOD-DAINLEV-ENTER    PIC 9(16).                                   
003800*                                 INLEVERANS NUMMER                       
003900     03 MOD-DAINLEV-NEXT     PIC 9(16).                                   
004000*                                 INLEVERANS NUMMER                       
004100     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
004200*                                 GRUPP MED TABELLRADER                   
004300        05 MOD-KVAVIS        PIC Z(5)9.                                   
004400*                                 AVISERAT ANTAL                          
004500        05 MOD-IDARTNR       PIC Z(8)9.                                   
004600*                                 ARTIKELNUMMER                           
004700        05 MOD-BEART         PIC X(25).                                   
004800*                                 ARTIKELBENÄMNING                        
004900        05 MOD-KDPRIO        PIC X.                                       
005000     03 MOD-INPUT.                                                        
005100*                                 GRUPP MED TABELLRADER                   
005200        05 MOD-KOLLI-KLART-ATTR                                           
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-KOLLI-KLART   PIC X.                                       
005600        05 MOD-IDUSER-003-ATTR                                            
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-IDUSER-003    PIC X(5).                                    
006000*                                 ANSVARIGT USERID INLÄGGN.(R32)          
006100        05 MOD-TAB           OCCURS 12 TIMES.                             
006200*                                 GRUPP MED TABELLRADER INPUT             
006300           07 MOD-CMD-ATTR   PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500           07 MOD-CMD-IN     PIC X(3).                                    
006600           07 MOD-KVANTMOT-ATTR                                           
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900           07 MOD-KVANTMOT-IN                                             
007000                             PIC Z(5)9.                                   
007100*                                 ANTAL MOTTAGET                          
007200           07 MOD-ADLAGOMR-ATTR                                           
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500           07 MOD-ADLAGOMR   PIC Z9.                                      
007600*                                 LAGEROMRÅDE                             
007700           07 MOD-ADGANG-ATTR                                             
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000           07 MOD-ADGANG     PIC Z9.                                      
008100*                                 GÅNG                                    
008200           07 MOD-ADPLATS-ATTR                                            
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500           07 MOD-ADPLATS    PIC Z(4)9.                                   
008600*                                 LAGERPLATSNUMMER                        
008700           07 MOD-KVSKROT-ATTR                                            
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000           07 MOD-KVSKROT    PIC Z(6)9.                                   
009100*                                 ANTAL SENASTE SKROTORDER                
009200     03 MOD-NEW.                                                          
009300*                                 RAD FÖR NYUPPLÄGG                       
009400        05 MOD-KVANTMOT-INM-ATTR                                          
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-KVANTMOT-INM  PIC Z(5)9.                                   
009800*                                 ANTAL MOTTAGET                          
009900        05 MOD-IDARTNR-INM-ATTR                                           
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-IDARTNR-INM   PIC X(9).                                    
010300*                                 ARTIKELNUMMER                           
010400        05 MOD-ADLAGOMR-INM-ATTR                                          
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-ADLAGOMR-INM  PIC X(2).                                    
010800*                                 LAGEROMRÅDE                             
010900        05 MOD-ADGANG-INM-ATTR                                            
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200        05 MOD-ADGANG-INM    PIC X(2).                                    
011300*                                 GÅNG                                    
011400        05 MOD-ADPLATS-INM-ATTR                                           
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700        05 MOD-ADPLATS-INM   PIC X(5).                                    
011800*                                 LAGERPLATSNUMMER                        
011900        05 MOD-CMD-INM-ATTR  PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100        05 MOD-CMD-INM       PIC X(3).                                    
012200        05 MOD-KVSKROT-INM-ATTR                                           
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500        05 MOD-KVSKROT-INM   PIC X(7).                                    
012600*                                 ANTAL SENASTE SKROTORDER                
012700     03 MOD-TEMFSINF         PIC X(55).                                   
012800*                                 INFORMATIONSMEDDELANDE                  
012900*** END OF VILMAII-COPY LENGTH= 1209 BYTES                                
