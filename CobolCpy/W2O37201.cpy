000100 01  MOD-W2O37201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2037200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDDC-UT          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDTYPE-IN        PIC X.                                       
001800     03 MOD-IDTYPE-UT        PIC X(5).                                    
001900     03 MOD-IDPERSON-BUY-IN  PIC X(3).                                    
002000*                                 PERSONKOD REFILLANSVARIG                
002100     03 MOD-IDPERSON-BUY-UT  PIC X(3).                                    
002200*                                 PERSONKOD REFILLANSVARIG                
002300     03 MOD-IDSTATUS-IN      PIC X.                                       
002400     03 MOD-IDSTATUS-UT      PIC X(12).                                   
002500     03 MOD-IDREFTYP-IN      PIC X.                                       
002600*                                 TYP AV REFILLORDER                      
002700     03 MOD-IDREFTYP-UT      PIC X(5).                                    
002800     03 MOD-BEART            PIC X(22).                                   
002900     03 MOD-ORDERSTATUS      PIC X(21).                                   
003000     03 MOD-ANT-REVIEW       PIC Z(4)9.                                   
003100     03 MOD-FOREG-AAR        PIC X(4).                                    
003200     03 MOD-KVOI-FOREG-AAR   PIC Z(6)9.                                   
003300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003400     03 MOD-IAAR             PIC X(4).                                    
003500     03 MOD-KVOI-IAAR        PIC Z(6)9.                                   
003600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003700     03 MOD-GRP-RULL         OCCURS 12 TIMES.                             
003800        05 MOD-TIPP          PIC X(2).                                    
003900*                                 PLANERINGSPERIOD (PP)                   
004000*                                 12 PER ÅR                               
004100        05 MOD-TIVV-FOM-TOM  PIC X(5).                                    
004200        05 MOD-KVOI-RULL     PIC Z(6)9.                                   
004300*                                 ORDERINGÅNG TILL SDC                    
004400     03 MOD-VECKA            PIC 9.                                       
004500     03 MOD-KVOI-INNEV       PIC Z(6)9.                                   
004600*                                 ORDERINGÅNG TILL DC INNEV PER           
004700     03 MOD-TIREFEFT         PIC 9(6).                                    
004800*                                 DATUM SENAST EFTERFRÅGAD                
004900     03 MOD-TIINLINL         PIC 9(6).                                    
005000*                                 RAPPORTERINGSDATUM INLAGD (R32)         
005100     03 MOD-TIORDREG         PIC 9(6).                                    
005200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005300     03 MOD-LOCATION-GRP.                                                 
005400        05 MOD-ADLAGOMR      PIC Z9.                                      
005500*                                 LAGEROMRÅDE                             
005600        05 MOD-ADGANG        PIC Z9.                                      
005700*                                 GÅNG                                    
005800        05 MOD-ADPLATS       PIC Z(4)9.                                   
005900*                                 LAGERPLATSNUMMER                        
006000     03 MOD-KVREFPKT-ATTR    PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-KVREFPKT         PIC Z(5)9.                                   
006300     03 MOD-KVREFBER         PIC Z(6)9.                                   
006400*                                 BERÄKNAD REFILLINGKVANTITET             
006500     03 MOD-KVPB-REF-ATTR    PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-KVPB-REF         PIC X(8).                                    
006800     03 MOD-BALANCE          PIC -(6)9.                                   
006900*                                 LAGERSALDO                              
007000     03 MOD-KVAKS-SDC        PIC Z(6)9.                                   
007100*                                 DEL AV AK SOM LIGGER I SDC              
007200     03 MOD-ORDERED          PIC -(6)9.                                   
007300*                                 BESTÄLLT ANTAL STYCKEN                  
007400     03 MOD-KVROS            PIC -(6)9.                                   
007500*                                 RESTORDERSALDO                          
007600     03 MOD-CRIT-KVROS-NDC-CDC                                            
007700                             PIC Z(5)9.                                   
007800     03 MOD-SUPERWEEK        PIC Z(2)9.                                   
007900     03 MOD-PURCHQTY-GRP.                                                 
008000        05 MOD-PURCHQTY-ATTR PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-PURCHQTY      PIC X(7).                                    
008300     03 MOD-QUALBLOCK        PIC X.                                       
008400     03 MOD-IDFKNGRP         PIC Z(4)9.                                   
008500*                                 FUNKTIONSGRUPP                          
008600     03 MOD-MODEL            OCCURS 3 TIMES                               
008700                             PIC X(5).                                    
008800     03 MOD-FREEZECODE       PIC X.                                       
008900     03 MOD-KDPRODSL         PIC Z9.                                      
009000*                                 PRODUKTSLAG                             
009100     03 MOD-TIFINLV          PIC 9(5).                                    
009200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
009300     03 MOD-TIURPROD         PIC 9(4).                                    
009400*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
009500     03 MOD-FLREFBEO-ATTR    PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 MOD-FLREFBEO         PIC X(2).                                    
009800*                                 MFS BEHANDLING AV INPUTFÄLT             
009900     03 MOD-FLWILSON         PIC X.                                       
010000*                                 WILSONFORMEL                            
010100     03 MOD-VKART            PIC Z(6)9.                                   
010200*                                 ARTIKELVIKT (G)                         
010300     03 MOD-REPLACES         PIC X(9).                                    
010400*                                 ARTIKELNUMMER                           
010500     03 MOD-VLARTNTO         PIC Z(7)9.9.                                 
010600*                                 ARTIKELVOLYM (CM3)                      
010700     03 MOD-REPL-BY          PIC X(9).                                    
010800*                                 ARTIKELNUMMER                           
010900     03 MOD-KDERS            PIC Z9.                                      
011000*                                 ERSÄTTNINGSKOD                          
011100     03 MOD-KVQPACK-0        PIC Z(4)9.                                   
011200*                                 ANTAL I Q0 FÖRPACKNING                  
011300     03 MOD-PRICE-TYPE       PIC X(3).                                    
011400     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
011500*                                 ARTIKELSTANDARDPRIS                     
011600     03 MOD-KVQPACK-1        PIC Z(4)9.                                   
011700*                                 ANTAL I Q1 FÖRPACKNING                  
011800     03 MOD-AIR-COST-SEK     PIC 9(7).                                    
011900     03 MOD-KVQPACK-3        PIC Z(4)9.                                   
012000*                                 ANTAL I Q3 FÖRPACKNING                  
012100     03 MOD-KVPBREOI-ATTR    PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300     03 MOD-KVPBREOI         PIC X(8).                                    
012400     03 MOD-DAPUBL           PIC 9(5).                                    
012500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
012600     03 MOD-FLORDSP-EJRO     PIC X.                                       
012700*                                 ORDERSPÄRR EJ RESTNOTERING              
012800     03 MOD-KVQPACK-4        PIC Z(4)9.                                   
012900*                                 ANTAL I Q4 FÖRPACKNING                  
013000     03 MOD-IDDC-REF         PIC X(2).                                    
013100*                                 SÄNDANDE LAGER FÖR REFILL               
013200     03 MOD-SEASON           PIC X.                                       
013300     03 MOD-FLAGGA-FCD-ATTR  PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500     03 MOD-FLAGGA-FCD       PIC X(2).                                    
013600*                                 MFS BEHANDLING AV INPUTFÄLT             
013700     03 MOD-KVPALL           PIC Z(6)9.                                   
013800*                                 ANTAL I PALL                            
013900     03 MOD-KVAVIS           PIC Z(5)9.                                   
014000*                                 AVISERAT ANTAL                          
014100     03 MOD-AVAIL            PIC -(7)9.                                   
014200*                                 LAGERSALDO                              
014300     03 MOD-KVAKS-CDC        PIC Z(6)9.                                   
014400*                                 DEL AV AK SOM LIGGER I CDC              
014500     03 MOD-KVROS-CDC        PIC -(6)9.                                   
014600*                                 RESTORDERSALDO                          
014700     03 MOD-KVPB-CDC         PIC Z(6)9.9.                                 
014800*                                 PERIODBEHOV (PROGNOS)                   
014900     03 MOD-KVROS-NDC-CDC    PIC -(6)9.                                   
015000*                                 RESTORDERSALDO                          
015100     03 MOD-REDIRLEV         PIC 9.9(2).                                  
015200*                                 DIREKTLEVERANSANDEL                     
015300     03 MOD-ETA-INFO-GRP     OCCURS 2 TIMES.                              
015400        05 MOD-KVAVIS-SUM    PIC Z(5)9.                                   
015500*                                 AVISERAT ANTAL                          
015600        05 MOD-TIBERANK      PIC 9(6).                                    
015700*                                 BERÄKNAD ANKOMSTDATUM                   
015800     03 MOD-COMMENT-GRP      OCCURS 2 TIMES.                              
015900        05 MOD-COMMENT-ATTR  PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100        05 MOD-COMMENT       PIC X(36).                                   
016200     03 MOD-TEMFSINF         PIC X(55).                                   
016300*                                 INFORMATIONSMEDDELANDE                  
016400*** END OF VILMAII-COPY LENGTH= 791 BYTES                                 
