000100 01  MOD-W2O32201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2032200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-URVAL.                                                        
000800        05 MOD-IDPERSON-FOM-ATTR                                          
000900                             PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100        05 MOD-IDPERSON-FOM  PIC Z(3).                                    
001200*                                 PERSONKOD                               
001300        05 MOD-IDPERSON-TOM-ATTR                                          
001400                             PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600        05 MOD-IDPERSON-TOM  PIC Z(3).                                    
001700*                                 PERSONKOD                               
001800        05 MOD-IDPERSON-GRP  OCCURS 5 TIMES.                              
001900           07 MOD-IDPERSON-ATTR                                           
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200           07 MOD-IDPERSON   PIC Z(3).                                    
002300*                                 PERSONKOD                               
002400        05 MOD-FLAGGA-IDBERED-ATTR                                        
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-FLAGGA-IDBERED                                             
002800                             PIC X.                                       
002900*                                 ALLMÄN FLAGGA                           
003000        05 MOD-FLAGGA-IDLEVNR-SHIP-ATTR                                   
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-FLAGGA-IDLEVNR-SHIP                                        
003400                             PIC X.                                       
003500*                                 ALLMÄN FLAGGA                           
003600        05 MOD-IDLEVNR-GRP   OCCURS 9 TIMES.                              
003700           07 MOD-IDLEVNR-ATTR                                            
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000           07 MOD-IDLEVNR    PIC X(5).                                    
004100*                                 LEVERANTÖRNUMMER                        
004200        05 MOD-KDERS-FOM-ATTR                                             
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-KDERS-FOM     PIC Z(2).                                    
004600*                                 ERSÄTTNINGSKOD                          
004700        05 MOD-KDERS-TOM-ATTR                                             
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-KDERS-TOM     PIC Z(2).                                    
005100*                                 ERSÄTTNINGSKOD                          
005200        05 MOD-KDERS-GRP     OCCURS 5 TIMES.                              
005300           07 MOD-KDERS-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500           07 MOD-KDERS      PIC Z(2).                                    
005600*                                 ERSÄTTNINGSKOD                          
005700        05 MOD-KDOTFREK-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-KDOTFREK      PIC X.                                       
006000*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
006100        05 MOD-IDFKNGRP-FOM-ATTR                                          
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-IDFKNGRP-FOM  PIC X(4).                                    
006500*                                 FUNKTIONSGRUPP                          
006600        05 MOD-IDFKNGRP-TOM-ATTR                                          
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-IDFKNGRP-TOM  PIC X(4).                                    
007000*                                 FUNKTIONSGRUPP                          
007100        05 MOD-FKN-GRP       OCCURS 4 TIMES.                              
007200           07 MOD-IDFKNGRP-ATTR                                           
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500           07 MOD-IDFKNGRP   PIC Z(3)9.                                   
007600*                                 FUNKTIONSGRUPP                          
007700        05 MOD-BEFT-GRP      OCCURS 4 TIMES.                              
007800           07 MOD-BEFT-ATTR  PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000           07 MOD-BEFT       PIC Z9.                                      
008100*                                 FÖRPACKNINGSTYP                         
008200        05 MOD-BEART-SOEK-ATTR                                            
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-BEART-SOEK    PIC X(25).                                   
008600*                                 ARTIKELBENÄMNING                        
008700        05 MOD-KDPRODSL-FOM-ATTR                                          
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-KDPRODSL-FOM  PIC Z9.                                      
009100*                                 PRODUKTSLAG                             
009200        05 MOD-KDPRODSL-TOM-ATTR                                          
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500        05 MOD-KDPRODSL-TOM  PIC Z9.                                      
009600*                                 PRODUKTSLAG                             
009700        05 MOD-IDPROJ-URV-ATTR                                            
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 MOD-IDPROJ-URV    PIC X(4).                                    
010100*                                 PARTS PROJEKTIDENTITET                  
010200        05 MOD-ADLAGOMR-ATTR PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-ADLAGOMR      PIC Z9.                                      
010500*                                 LAGEROMRÅDE                             
010600        05 MOD-ADGANG-FOM-ATTR                                            
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900        05 MOD-ADGANG-FOM    PIC Z9.                                      
011000*                                 LAGEROMRÅDE                             
011100        05 MOD-ADGANG-TOM-ATTR                                            
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-ADGANG-TOM    PIC Z9.                                      
011500*                                 LAGEROMRÅDE                             
011600        05 MOD-KVVECKOR-KVPB-ATTR                                         
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900        05 MOD-KVVECKOR-KVPB PIC Z9.                                      
012000*                                 ANTAL VECKOR                            
012100        05 MOD-KVVECKOR-AVROP-ATTR                                        
012200                             PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400        05 MOD-KVVECKOR-AVROP                                             
012500                             PIC Z9.                                      
012600*                                 ANTAL VECKOR                            
012700     03 MOD-LISTA.                                                        
012800        05 MOD-FLAGGA-IDLEVNR-ATTR                                        
012900                             PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100        05 MOD-FLAGGA-IDLEVNR                                             
013200                             PIC X.                                       
013300*                                 ALLMÄN FLAGGA                           
013400        05 MOD-FLAGGA-LEVBET-ATTR                                         
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700        05 MOD-FLAGGA-LEVBET PIC X.                                       
013800*                                 ALLMÄN FLAGGA                           
013900        05 MOD-FLAGGA-BEART-S-ATTR                                        
014000                             PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200        05 MOD-FLAGGA-BEART-S                                             
014300                             PIC X.                                       
014400*                                 ALLMÄN FLAGGA                           
014500        05 MOD-FLAGGA-BEART-GB-ATTR                                       
014600                             PIC X(2).                                    
014700*                                 MFS ATTRIBUTFÄLT                        
014800        05 MOD-FLAGGA-BEART-GB                                            
014900                             PIC X.                                       
015000*                                 ALLMÄN FLAGGA                           
015100        05 MOD-FLAGGA-STONHAND-ATTR                                       
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400        05 MOD-FLAGGA-STONHAND                                            
015500                             PIC X.                                       
015600*                                 ALLMÄN FLAGGA                           
015700        05 MOD-FLAGGA-KVAKS-ATTR                                          
015800                             PIC X(2).                                    
015900*                                 MFS ATTRIBUTFÄLT                        
016000        05 MOD-FLAGGA-KVAKS  PIC X.                                       
016100*                                 ALLMÄN FLAGGA                           
016200        05 MOD-FLAGGA-VOR-ROS-ATTR                                        
016300                             PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500        05 MOD-FLAGGA-VOR-ROS                                             
016600                             PIC X.                                       
016700*                                 ALLMÄN FLAGGA                           
016800        05 MOD-FLAGGA-KVSLAP-ATTR                                         
016900                             PIC X(2).                                    
017000*                                 MFS ATTRIBUTFÄLT                        
017100        05 MOD-FLAGGA-KVSLAP PIC X.                                       
017200*                                 ALLMÄN FLAGGA                           
017300        05 MOD-FLAGGA-LEVBESK-ATTR                                        
017400                             PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600        05 MOD-FLAGGA-LEVBESK                                             
017700                             PIC X.                                       
017800*                                 ALLMÄN FLAGGA                           
017900        05 MOD-FLAGGA-KVART-FORAVIS-ATTR                                  
018000                             PIC X(2).                                    
018100*                                 MFS ATTRIBUTFÄLT                        
018200        05 MOD-FLAGGA-KVART-FORAVIS                                       
018300                             PIC X.                                       
018400*                                 ALLMÄN FLAGGA                           
018500        05 MOD-FLAGGA-TPO-ATTR                                            
018600                             PIC X(2).                                    
018700*                                 MFS ATTRIBUTFÄLT                        
018800        05 MOD-FLAGGA-TPO    PIC X.                                       
018900*                                 ALLMÄN FLAGGA                           
019000        05 MOD-FLAGGA-KVVECKOR-LT-ATTR                                    
019100                             PIC X(2).                                    
019200*                                 MFS ATTRIBUTFÄLT                        
019300        05 MOD-FLAGGA-KVVECKOR-LT                                         
019400                             PIC X.                                       
019500*                                 ALLMÄN FLAGGA                           
019600        05 MOD-FLAGGA-KDERS-ATTR                                          
019700                             PIC X(2).                                    
019800*                                 MFS ATTRIBUTFÄLT                        
019900        05 MOD-FLAGGA-KDERS  PIC X.                                       
020000*                                 ALLMÄN FLAGGA                           
020100        05 MOD-FLAGGA-IDANSK-ATTR                                         
020200                             PIC X(2).                                    
020300*                                 MFS ATTRIBUTFÄLT                        
020400        05 MOD-FLAGGA-IDANSK PIC X.                                       
020500*                                 ALLMÄN FLAGGA                           
020600        05 MOD-FLAGGA-DIRLEV-ATTR                                         
020700                             PIC X(2).                                    
020800*                                 MFS ATTRIBUTFÄLT                        
020900        05 MOD-FLAGGA-DIRLEV PIC X.                                       
021000*                                 ALLMÄN FLAGGA                           
021100        05 MOD-FLAGGA-AVSDAG-ATTR                                         
021200                             PIC X(2).                                    
021300*                                 MFS ATTRIBUTFÄLT                        
021400        05 MOD-FLAGGA-AVSDAG PIC X.                                       
021500*                                 ALLMÄN FLAGGA                           
021600        05 MOD-FLAGGA-TREND-ATTR                                          
021700                             PIC X(2).                                    
021800*                                 MFS ATTRIBUTFÄLT                        
021900        05 MOD-FLAGGA-TREND  PIC X.                                       
022000*                                 ALLMÄN FLAGGA                           
022100        05 MOD-FLAGGA-SAESONG-ATTR                                        
022200                             PIC X(2).                                    
022300*                                 MFS ATTRIBUTFÄLT                        
022400        05 MOD-FLAGGA-SAESONG                                             
022500                             PIC X.                                       
022600*                                 ALLMÄN FLAGGA                           
022700        05 MOD-FLAGGA-TISINLV-ATTR                                        
022800                             PIC X(2).                                    
022900*                                 MFS ATTRIBUTFÄLT                        
023000        05 MOD-FLAGGA-TISINLV                                             
023100                             PIC X.                                       
023200*                                 ALLMÄN FLAGGA                           
023300        05 MOD-FLAGGA-KVPB-ATTR                                           
023400                             PIC X(2).                                    
023500*                                 MFS ATTRIBUTFÄLT                        
023600        05 MOD-FLAGGA-KVPB   PIC X.                                       
023700*                                 ALLMÄN FLAGGA                           
023800        05 MOD-FLAGGA-KVOI-RULL-ATTR                                      
023900                             PIC X(2).                                    
024000*                                 MFS ATTRIBUTFÄLT                        
024100        05 MOD-FLAGGA-KVOI-RULL                                           
024200                             PIC X.                                       
024300*                                 ALLMÄN FLAGGA                           
024400        05 MOD-FLAGGA-KVOI-IAR-PL-5-ATTR                                  
024500                             PIC X(2).                                    
024600*                                 MFS ATTRIBUTFÄLT                        
024700        05 MOD-FLAGGA-KVOI-IAR-PL-5                                       
024800                             PIC X.                                       
024900*                                 ALLMÄN FLAGGA                           
025000        05 MOD-FLAGGA-KDLEVPLF-ATTR                                       
025100                             PIC X(2).                                    
025200*                                 MFS ATTRIBUTFÄLT                        
025300        05 MOD-FLAGGA-KDLEVPLF                                            
025400                             PIC X.                                       
025500*                                 ALLMÄN FLAGGA                           
025600        05 MOD-FLAGGA-IDINK-ATTR                                          
025700                             PIC X(2).                                    
025800*                                 MFS ATTRIBUTFÄLT                        
025900        05 MOD-FLAGGA-IDINK  PIC X.                                       
026000*                                 ALLMÄN FLAGGA                           
026100        05 MOD-FLAGGA-PRARTBES-ATTR                                       
026200                             PIC X(2).                                    
026300*                                 MFS ATTRIBUTFÄLT                        
026400        05 MOD-FLAGGA-PRARTBES                                            
026500                             PIC X.                                       
026600*                                 ALLMÄN FLAGGA                           
026700        05 MOD-FLAGGA-BESTREST-ATTR                                       
026800                             PIC X(2).                                    
026900*                                 MFS ATTRIBUTFÄLT                        
027000        05 MOD-FLAGGA-BESTREST                                            
027100                             PIC X.                                       
027200*                                 ALLMÄN FLAGGA                           
027300        05 MOD-FLAGGA-AVTAL-ATTR                                          
027400                             PIC X(2).                                    
027500*                                 MFS ATTRIBUTFÄLT                        
027600        05 MOD-FLAGGA-AVTAL  PIC X.                                       
027700*                                 ALLMÄN FLAGGA                           
027800        05 MOD-FLAGGA-FLFORP-SI-ATTR                                      
027900                             PIC X(2).                                    
028000*                                 MFS ATTRIBUTFÄLT                        
028100        05 MOD-FLAGGA-FLFORP-SI                                           
028200                             PIC X.                                       
028300*                                 ALLMÄN FLAGGA                           
028400        05 MOD-FLAGGA-VKART-VLARTNTO-ATTR                                 
028500                             PIC X(2).                                    
028600*                                 MFS ATTRIBUTFÄLT                        
028700        05 MOD-FLAGGA-VKART-VLARTNTO                                      
028800                             PIC X.                                       
028900*                                 ALLMÄN FLAGGA                           
029000        05 MOD-FLAGGA-TIREFSTO-ATTR                                       
029100                             PIC X(2).                                    
029200*                                 MFS ATTRIBUTFÄLT                        
029300        05 MOD-FLAGGA-TIREFSTO                                            
029400                             PIC X.                                       
029500*                                 ALLMÄN FLAGGA                           
029600        05 MOD-FLAGGA-IDPROJ-ATTR                                         
029700                             PIC X(2).                                    
029800*                                 MFS ATTRIBUTFÄLT                        
029900        05 MOD-FLAGGA-IDPROJ PIC X.                                       
030000*                                 ALLMÄN FLAGGA                           
030100        05 MOD-FLAGGA-TIFINLV-ATTR                                        
030200                             PIC X(2).                                    
030300*                                 MFS ATTRIBUTFÄLT                        
030400        05 MOD-FLAGGA-TIFINLV                                             
030500                             PIC X.                                       
030600*                                 ALLMÄN FLAGGA                           
030700        05 MOD-FLAGGA-TIURPROD-ATTR                                       
030800                             PIC X(2).                                    
030900*                                 MFS ATTRIBUTFÄLT                        
031000        05 MOD-FLAGGA-TIURPROD                                            
031100                             PIC X.                                       
031200*                                 ALLMÄN FLAGGA                           
031300        05 MOD-FLAGGA-DISPLS-ATTR                                         
031400                             PIC X(2).                                    
031500*                                 MFS ATTRIBUTFÄLT                        
031600        05 MOD-FLAGGA-DISPLS PIC X.                                       
031700*                                 ALLMÄN FLAGGA                           
031800        05 MOD-FLAGGA-KDPRODSL-ATTR                                       
031900                             PIC X(2).                                    
032000*                                 MFS ATTRIBUTFÄLT                        
032100        05 MOD-FLAGGA-KDPRODSL                                            
032200                             PIC X.                                       
032300*                                 ALLMÄN FLAGGA                           
032400        05 MOD-FLAGGA-IDFKNGRP-ATTR                                       
032500                             PIC X(2).                                    
032600*                                 MFS ATTRIBUTFÄLT                        
032700        05 MOD-FLAGGA-IDFKNGRP                                            
032800                             PIC X.                                       
032900*                                 ALLMÄN FLAGGA                           
033000        05 MOD-FLAGGA-FLIART-ATTR                                         
033100                             PIC X(2).                                    
033200*                                 MFS ATTRIBUTFÄLT                        
033300        05 MOD-FLAGGA-FLIART PIC X.                                       
033400*                                 ALLMÄN FLAGGA                           
033500        05 MOD-FLAGGA-STYRPARAM-ATTR                                      
033600                             PIC X(2).                                    
033700*                                 MFS ATTRIBUTFÄLT                        
033800        05 MOD-FLAGGA-STYRPARAM                                           
033900                             PIC X.                                       
034000*                                 ALLMÄN FLAGGA                           
034100        05 MOD-FLAGGA-SERVICE-ATTR                                        
034200                             PIC X(2).                                    
034300*                                 MFS ATTRIBUTFÄLT                        
034400        05 MOD-FLAGGA-SERVICE                                             
034500                             PIC X.                                       
034600*                                 ALLMÄN FLAGGA                           
034700        05 MOD-FLAGGA-KDSORT-ATTR                                         
034800                             PIC X(2).                                    
034900*                                 MFS ATTRIBUTFÄLT                        
035000        05 MOD-FLAGGA-KDSORT PIC X.                                       
035100*                                 ALLMÄN FLAGGA                           
035200        05 MOD-FLAGGA-KVANTER-ATTR                                        
035300                             PIC X(2).                                    
035400*                                 MFS ATTRIBUTFÄLT                        
035500        05 MOD-FLAGGA-KVANTER                                             
035600                             PIC X.                                       
035700*                                 ALLMÄN FLAGGA                           
035800        05 MOD-FLAGGA-FORPINFO-ATTR                                       
035900                             PIC X(2).                                    
036000*                                 MFS ATTRIBUTFÄLT                        
036100        05 MOD-FLAGGA-FORPINFO                                            
036200                             PIC X.                                       
036300*                                 ALLMÄN FLAGGA                           
036400        05 MOD-FLAGGA-KVSPANT-ATTR                                        
036500                             PIC X(2).                                    
036600*                                 MFS ATTRIBUTFÄLT                        
036700        05 MOD-FLAGGA-KVSPANT                                             
036800                             PIC X.                                       
036900*                                 ALLMÄN FLAGGA                           
037000        05 MOD-FLAGGA-S-LAGER-ATTR                                        
037100                             PIC X(2).                                    
037200*                                 MFS ATTRIBUTFÄLT                        
037300        05 MOD-FLAGGA-S-LAGER                                             
037400                             PIC X.                                       
037500*                                 ALLMÄN FLAGGA                           
037600        05 MOD-FLAGGA-KVMP-ATTR                                           
037700                             PIC X(2).                                    
037800*                                 MFS ATTRIBUTFÄLT                        
037900        05 MOD-FLAGGA-KVMP   PIC X.                                       
038000*                                 ALLMÄN FLAGGA                           
038100        05 MOD-FLAGGA-IDKR-ATTR                                           
038200                             PIC X(2).                                    
038300*                                 MFS ATTRIBUTFÄLT                        
038400        05 MOD-FLAGGA-IDKR   PIC X.                                       
038500*                                 ALLMÄN FLAGGA                           
038600        05 MOD-FLAGGA-KAMPANJ-ATTR                                        
038700                             PIC X(2).                                    
038800*                                 MFS ATTRIBUTFÄLT                        
038900        05 MOD-FLAGGA-KAMPANJ                                             
039000                             PIC X.                                       
039100*                                 ALLMÄN FLAGGA                           
039200        05 MOD-FLAGGA-ADART-ATTR                                          
039300                             PIC X(2).                                    
039400*                                 MFS ATTRIBUTFÄLT                        
039500        05 MOD-FLAGGA-ADART  PIC X.                                       
039600*                                 ALLMÄN FLAGGA                           
039700        05 MOD-FLAGGA-ADINPORT-ATTR                                       
039800                             PIC X(2).                                    
039900*                                 MFS ATTRIBUTFÄLT                        
040000        05 MOD-FLAGGA-ADINPORT                                            
040100                             PIC X.                                       
040200*                                 ALLMÄN FLAGGA                           
040300        05 MOD-FLAGGA-KDUART-ATTR                                         
040400                             PIC X(2).                                    
040500*                                 MFS ATTRIBUTFÄLT                        
040600        05 MOD-FLAGGA-KDUART PIC X.                                       
040700*                                 ALLMÄN FLAGGA                           
040800        05 MOD-FLAGGA-URSPRUNG-ATTR                                       
040900                             PIC X(2).                                    
041000*                                 MFS ATTRIBUTFÄLT                        
041100        05 MOD-FLAGGA-URSPRUNG                                            
041200                             PIC X.                                       
041300*                                 ALLMÄN FLAGGA                           
041400        05 MOD-FLAGGA-KDOTFREK-ATTR                                       
041500                             PIC X(2).                                    
041600*                                 MFS ATTRIBUTFÄLT                        
041700        05 MOD-FLAGGA-KDOTFREK                                            
041800                             PIC X.                                       
041900*                                 ALLMÄN FLAGGA                           
042000     03 MOD-LIST REDEFINES MOD-LISTA.                                     
042100        05 MOD-LST           OCCURS 52 TIMES.                             
042200           07 MOD-FLAGGA-ATTR                                             
042300                             PIC X(2).                                    
042400*                                 MFS ATTRIBUTFÄLT                        
042500           07 MOD-FLAGGA     PIC X.                                       
042600*                                 ALLMÄN FLAGGA                           
042700     03 MOD-KDARBTYP-IN-ATTR PIC X(2).                                    
042800*                                 MFS ATTRIBUTFÄLT                        
042900     03 MOD-KDARBTYP-IN      PIC X(4).                                    
043000*                                 TYP AV ARBETE                           
043100     03 MOD-IDPERSON-IN-ATTR PIC X(2).                                    
043200*                                 MFS ATTRIBUTFÄLT                        
043300     03 MOD-IDPERSON-IN      PIC X(3).                                    
043400*                                 PERSONKOD                               
043500     03 MOD-TEMFSINF         PIC X(55).                                   
043600*                                 INFORMATIONSMEDDELANDE                  
043700*** END OF VILMAII-COPY LENGTH= 514 BYTES                                 
