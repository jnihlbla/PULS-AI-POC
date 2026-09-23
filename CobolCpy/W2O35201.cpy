000100 01  MOD-W2O35201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2035200               
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
001300     03 MOD-BEART            PIC X(19).                                   
001400     03 MOD-IDPERSON-BUY-IN  PIC X(3).                                    
001500*                                 PERSONKOD REFILLANSVARIG                
001600     03 MOD-IDPERSON-BUY-UT  PIC X(3).                                    
001700*                                 PERSONKOD REFILLANSVARIG                
001800     03 MOD-IDDC-IN          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-IDDC-UT          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD-IDTYPE-IN        PIC X.                                       
002300     03 MOD-IDTYPE-UT        PIC X(5).                                    
002400     03 MOD-IDSTATUS-IN      PIC X.                                       
002500     03 MOD-IDSTATUS-UT      PIC X(12).                                   
002600     03 MOD-IDREFTYP-IN      PIC X.                                       
002700*                                 TYP AV REFILLORDER                      
002800     03 MOD-IDREFTYP-UT      PIC X(5).                                    
002900     03 MOD-IDDC-GROUP       OCCURS 4 TIMES                               
003000                             PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MOD-ORDERSTATUS      PIC X(21).                                   
003300     03 MOD-ANT-REVIEW       PIC Z(4)9.                                   
003400     03 MOD-FOREG-AAR        PIC X(4).                                    
003500     03 MOD-KVOI-FOREG-AAR   OCCURS 4 TIMES                               
003600                             PIC Z(6)9.                                   
003700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003800     03 MOD-IAAR             PIC X(4).                                    
003900     03 MOD-KVOI-IAAR        OCCURS 4 TIMES                               
004000                             PIC Z(6)9.                                   
004100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004200     03 MOD-GRP-RULL         OCCURS 3 TIMES.                              
004300        05 MOD-TIPP          PIC X(2).                                    
004400*                                 PLANERINGSPERIOD (PP)                   
004500*                                 12 PER ÅR                               
004600        05 MOD-TIVV-FOM-TOM  PIC X(5).                                    
004700        05 MOD-KVOI-RULL     OCCURS 4 TIMES                               
004800                             PIC Z(6)9.                                   
004900*                                 ORDERINGÅNG TILL SDC                    
005000     03 MOD-VECKA            PIC 9.                                       
005100     03 MOD-KVOI-INNEV       OCCURS 4 TIMES                               
005200                             PIC Z(6)9.                                   
005300*                                 ORDERINGÅNG TILL DC INNEV PER           
005400     03 MOD-KVPB-REF-GRP     OCCURS 4 TIMES.                              
005500        05 MOD-KVPB-REF-ATTR PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-KVPB-REF      PIC X(7).                                    
005800     03 MOD-KVPBREOI-GRP     OCCURS 4 TIMES.                              
005900        05 MOD-KVPBREOI-ATTR PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KVPBREOI      PIC X(7).                                    
006200     03 MOD-IDDC-REF         OCCURS 4 TIMES                               
006300                             PIC X(2).                                    
006400*                                 SÄNDANDE LAGER FÖR REFILL               
006500     03 MOD-BALANCE          OCCURS 4 TIMES                               
006600                             PIC -(6)9.                                   
006700*                                 LAGERSALDO                              
006800     03 MOD-KVAKS-SDC        OCCURS 4 TIMES                               
006900                             PIC Z(6)9.                                   
007000*                                 DEL AV AK SOM LIGGER I SDC              
007100     03 MOD-ORDERED          OCCURS 4 TIMES                               
007200                             PIC -(6)9.                                   
007300*                                 BESTÄLLT ANTAL STYCKEN                  
007400     03 MOD-KVROS            OCCURS 4 TIMES                               
007500                             PIC -(6)9.                                   
007600*                                 RESTORDERSALDO                          
007700     03 MOD-IDDC-FROM-GRP    OCCURS 4 TIMES.                              
007800        05 MOD-IDDC-FROM-ATTR                                             
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-IDDC-FROM     PIC X(2).                                    
008200*                                 IDENTIFIERARE LAGER                     
008300     03 MOD-SUPERWEEK        OCCURS 4 TIMES                               
008400                             PIC Z(2)9.                                   
008500     03 MOD-PURCHQTY-GRP     OCCURS 4 TIMES.                              
008600        05 MOD-PURCHQTY-ATTR PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-PURCHQTY      PIC X(7).                                    
008900     03 MOD-MODEL            OCCURS 3 TIMES                               
009000                             PIC X(5).                                    
009100     03 MOD-REDIRLEV         PIC 9.9(2).                                  
009200*                                 DIREKTLEVERANSANDEL                     
009300     03 MOD-QUALBLOCK        OCCURS 4 TIMES                               
009400                             PIC X.                                       
009500     03 MOD-FLREFBEO-GRP     OCCURS 4 TIMES.                              
009600        05 MOD-FLREFBEO-ATTR PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-FLREFBEO      PIC X(2).                                    
009900*                                 MFS BEHANDLING AV INPUTFÄLT             
010000     03 MOD-FREEZECODE       OCCURS 4 TIMES                               
010100                             PIC X.                                       
010200     03 MOD-IDFKNGRP         PIC Z(4)9.                                   
010300*                                 FUNKTIONSGRUPP                          
010400     03 MOD-KDPRODSL         PIC Z9.                                      
010500*                                 PRODUKTSLAG                             
010600     03 MOD-REPLACES         PIC X(9).                                    
010700*                                 ARTIKELNUMMER                           
010800     03 MOD-TIFINLV          PIC 9(5).                                    
010900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
011000     03 MOD-REPL-BY          PIC X(9).                                    
011100*                                 ARTIKELNUMMER                           
011200     03 MOD-KDERS            PIC Z9.                                      
011300*                                 ERSÄTTNINGSKOD                          
011400     03 MOD-KVQPACK-0        PIC Z(4)9.                                   
011500*                                 ANTAL I Q0 FÖRPACKNING                  
011600     03 MOD-PRMATRL          PIC Z(6)9.9(2).                              
011700*                                 FAST PRIS UNDER LÖPANDE ÅR              
011800     03 MOD-KVQPACK-1        PIC Z(4)9.                                   
011900*                                 ANTAL I Q1 FÖRPACKNING                  
012000     03 MOD-TIURPROD         PIC 9(4).                                    
012100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
012200     03 MOD-KVQPACK-3        PIC Z(4)9.                                   
012300*                                 ANTAL I Q3 FÖRPACKNING                  
012400     03 MOD-AIR-COST         PIC Z(4)9.                                   
012500     03 MOD-FLCDART-GRP      OCCURS 4 TIMES.                              
012600        05 MOD-FLCDART-ATTR  PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800        05 MOD-FLCDART       PIC X.                                       
012900*                                 CROSS-DOCKING PART                      
013000     03 MOD-AVAIL            PIC -(7)9.                                   
013100*                                 LAGERSALDO                              
013200     03 MOD-KVAKS-CDC        PIC Z(6)9.                                   
013300*                                 DEL AV AK SOM LIGGER I CDC              
013400     03 MOD-KVROS-CDC        PIC -(6)9.                                   
013500*                                 RESTORDERSALDO                          
013600     03 MOD-KVAVIS           PIC Z(5)9.                                   
013700*                                 AVISERAT ANTAL                          
013800     03 MOD-KVPB-CDC         PIC Z(6)9.9.                                 
013900*                                 PERIODBEHOV (PROGNOS)                   
014000     03 MOD-SEASON           OCCURS 4 TIMES                               
014100                             PIC X.                                       
014200     03 MOD-FLFLYG-GRP       OCCURS 4 TIMES.                              
014300        05 MOD-FLFLYG-ATTR   PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500        05 MOD-FLFLYG        PIC X.                                       
014600*                                 FLYGARTIKEL                             
014700     03 MOD-KVROS-NDC-CDC    OCCURS 4 TIMES                               
014800                             PIC -(6)9.                                   
014900*                                 RESTORDERSALDO                          
015000     03 MOD-CRIT-KVROS-NDC-CDC                                            
015100                             OCCURS 4 TIMES                               
015200                             PIC Z(5)9.                                   
015300     03 MOD-FLFLYG-GRP       OCCURS 4 TIMES.                              
015400        05 MOD-KVREFPKT-ATTR PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600        05 MOD-KVREFPKT      PIC Z(5)9.                                   
015700     03 MOD-COMMENT-ATTR     PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900     03 MOD-COMMENT          PIC X(30).                                   
016000     03 MOD-TEMFSINF         PIC X(55).                                   
016100*                                 INFORMATIONSMEDDELANDE                  
016200*** END OF VILMAII-COPY LENGTH= 951 BYTES                                 
