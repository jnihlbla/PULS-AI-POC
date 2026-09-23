000100 01  MOD-W2O39201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2039200               
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
001300     03 MOD-IDDC-REF-IN-ATTR PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDDC-REF-IN      PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-REF-UT-ATTR PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-IDDC-REF-UT      PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDTYPE-IN        PIC X.                                       
002200     03 MOD-IDTYPE-UT        PIC X(5).                                    
002300     03 MOD-IDPERSON-BUY-IN  PIC X(3).                                    
002400*                                 PERSONKOD REFILLANSVARIG                
002500     03 MOD-IDPERSON-BUY-UT  PIC X(3).                                    
002600*                                 PERSONKOD REFILLANSVARIG                
002700     03 MOD-IDSTATUS-IN      PIC X.                                       
002800     03 MOD-IDSTATUS-UT      PIC X(12).                                   
002900     03 MOD-IDREFTYP-IN      PIC X.                                       
003000*                                 TYP AV REFILLORDER                      
003100     03 MOD-IDREFTYP-UT      PIC X(5).                                    
003200     03 MOD-BEART            PIC X(22).                                   
003300     03 MOD-ORDERSTATUS      PIC X(21).                                   
003400     03 MOD-ANT-REVIEW       PIC Z(4)9.                                   
003500     03 MOD-FOREG-AAR        PIC X(4).                                    
003600     03 MOD-KVOI-FOREG-AAR   PIC Z(6)9.                                   
003700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003800     03 MOD-IAAR             PIC X(4).                                    
003900     03 MOD-KVOI-IAAR        PIC Z(6)9.                                   
004000*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004100     03 MOD-GRP-RULL         OCCURS 12 TIMES.                             
004200        05 MOD-TIPP          PIC X(2).                                    
004300*                                 PLANERINGSPERIOD (PP)                   
004400*                                 12 PER ÅR                               
004500        05 MOD-TIVV-FOM-TOM  PIC X(5).                                    
004600        05 MOD-KVOI-RULL     PIC Z(6)9.                                   
004700*                                 ORDERINGÅNG TILL SDC                    
004800     03 MOD-VECKA            PIC 9.                                       
004900     03 MOD-KVOI-INNEV       PIC Z(6)9.                                   
005000*                                 ORDERINGÅNG TILL DC INNEV PER           
005100     03 MOD-TIREFEFT         PIC 9(6).                                    
005200*                                 DATUM SENAST EFTERFRÅGAD                
005300     03 MOD-TIINLINL         PIC 9(6).                                    
005400*                                 RAPPORTERINGSDATUM INLAGD (R32)         
005500     03 MOD-TIORDREG         PIC 9(6).                                    
005600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005700     03 MOD-LOCATION-GRP.                                                 
005800        05 MOD-ADLAGOMR      PIC Z9.                                      
005900*                                 LAGEROMRÅDE                             
006000        05 MOD-ADGANG        PIC Z9.                                      
006100*                                 GÅNG                                    
006200        05 MOD-ADPLATS       PIC Z(4)9.                                   
006300*                                 LAGERPLATSNUMMER                        
006400     03 MOD-KVREFPKT-ATTR    PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-KVREFPKT         PIC Z(6)9.                                   
006700     03 MOD-KVREFBER         PIC Z(6)9.                                   
006800*                                 BERÄKNAD REFILLINGKVANTITET             
006900     03 MOD-KVPB-SEP-ATTR    PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-KVPB-SEP         PIC X(8).                                    
007200     03 MOD-BALANCE          PIC -(7)9.                                   
007300*                                 LAGERSALDO                              
007400     03 MOD-KVAKS-CDC        PIC -(7)9.                                   
007500*                                 DEL AV AK SOM LIGGER I CDC              
007600     03 MOD-ORDERED          PIC -(7)9.                                   
007700*                                 BESTÄLLT ANTAL STYCKEN                  
007800     03 MOD-KVROS            PIC -(7)9.                                   
007900*                                 RESTORDERSALDO                          
008000     03 MOD-CRIT-KVROS-NDC-CDC                                            
008100                             PIC Z(7)9.                                   
008200     03 MOD-SUPERWEEK        PIC Z(2)9.                                   
008300     03 MOD-PURCHQTY-GRP.                                                 
008400        05 MOD-PURCHQTY-ATTR PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-PURCHQTY      PIC X(7).                                    
008700     03 MOD-QUALBLOCK        PIC X.                                       
008800     03 MOD-IDFKNGRP         PIC Z(4)9.                                   
008900*                                 FUNKTIONSGRUPP                          
009000     03 MOD-MODEL            OCCURS 3 TIMES                               
009100                             PIC X(5).                                    
009200     03 MOD-FLAGGA-FCD-ATTR  PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-FLAGGA-FCD       PIC X(2).                                    
009500*                                 MFS BEHANDLING AV INPUTFÄLT             
009600     03 MOD-KDPRODSL         PIC Z9.                                      
009700*                                 PRODUKTSLAG                             
009800     03 MOD-TIFINLV          PIC 9(5).                                    
009900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
010000     03 MOD-TIURPROD         PIC 9(4).                                    
010100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
010200     03 MOD-FLREFBEO-ATTR    PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-FLREFBEO         PIC X(2).                                    
010500*                                 MFS BEHANDLING AV INPUTFÄLT             
010600     03 MOD-FLWILSON         PIC X.                                       
010700*                                 WILSONFORMEL                            
010800     03 MOD-VKART            PIC Z(6)9.                                   
010900*                                 ARTIKELVIKT (G)                         
011000     03 MOD-REPLACES         PIC X(9).                                    
011100*                                 ARTIKELNUMMER                           
011200     03 MOD-VLARTNTO         PIC Z(7)9.9.                                 
011300*                                 ARTIKELVOLYM (CM3)                      
011400     03 MOD-REPL-BY          PIC X(9).                                    
011500*                                 ARTIKELNUMMER                           
011600     03 MOD-TIERSDAT-PREL    PIC X(4).                                    
011700     03 MOD-KDERS            PIC Z9.                                      
011800*                                 ERSÄTTNINGSKOD                          
011900     03 MOD-KVQPACK-0        PIC Z(4)9.                                   
012000*                                 ANTAL I Q0 FÖRPACKNING                  
012100     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
012200*                                 ARTIKELSTANDARDPRIS                     
012300     03 MOD-KVQPACK-1        PIC Z(4)9.                                   
012400*                                 ANTAL I Q1 FÖRPACKNING                  
012500     03 MOD-AIR-COST-SEK     PIC Z(6)9.                                   
012600     03 MOD-KVQPACK-3        PIC Z(4)9.                                   
012700*                                 ANTAL I Q3 FÖRPACKNING                  
012800     03 MOD-KVPB-TOT         PIC Z(5)9.9.                                 
012900*                                 TOTALT PERIODBEHOV                      
013000     03 MOD-DAPUBL           PIC X(5).                                    
013100*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
013200     03 MOD-KVQPACK-4        PIC Z(4)9.                                   
013300*                                 ANTAL I Q4 FÖRPACKNING                  
013400     03 MOD-IDDC-REF         PIC X(2).                                    
013500*                                 SÄNDANDE LAGER FÖR REFILL               
013600     03 MOD-SEASON           PIC X.                                       
013700     03 MOD-SEASON-FCPL      PIC X.                                       
013800     03 MOD-KVPALL           PIC Z(6)9.                                   
013900*                                 ANTAL I PALL                            
014000     03 MOD-KVAVIS           PIC Z(6)9.                                   
014100*                                 AVISERAT ANTAL                          
014200     03 MOD-AVAIL            PIC -(7)9.                                   
014300*                                 LAGERSALDO                              
014400     03 MOD-KVAKS-SDC        PIC Z(6)9.                                   
014500*                                 DEL AV AK SOM LIGGER I SDC              
014600     03 MOD-KVROS-SDC        PIC -(6)9.                                   
014700*                                 RESTORDERSALDO                          
014800     03 MOD-KVPB-SDC         PIC Z(5)9.9.                                 
014900*                                 PERIODBEHOV (PROGNOS)                   
015000     03 MOD-KVROS-NDC-CDC    PIC -(6)9.                                   
015100*                                 RESTORDERSALDO                          
015200     03 MOD-REDIRLEV         PIC 9.9(2).                                  
015300*                                 DIREKTLEVERANSANDEL                     
015400     03 MOD-ETA-INFO-GRP     OCCURS 2 TIMES.                              
015500        05 MOD-KVAVIS-SUM    PIC Z(5)9.                                   
015600*                                 AVISERAT ANTAL                          
015700        05 MOD-TIBERANK      PIC 9(6).                                    
015800*                                 BERÄKNAD ANKOMSTDATUM                   
015900     03 MOD-COMMENT-GRP      OCCURS 2 TIMES.                              
016000        05 MOD-COMMENT-ATTR  PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200        05 MOD-COMMENT       PIC X(36).                                   
016300     03 MOD-TEMFSINF         PIC X(55).                                   
016400*                                 INFORMATIONSMEDDELANDE                  
016500*** END OF VILMAII-COPY LENGTH= 800 BYTES                                 
