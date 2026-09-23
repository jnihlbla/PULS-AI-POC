000100 01  MOD-W2O42201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2032200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-URVAL.                                                        
000800        05 MOD-IDDC-ATTR     PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000        05 MOD-IDDC          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200        05 MOD-IDPERSON-FOM-ATTR                                          
001300                             PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-IDPERSON-FOM  PIC Z(3).                                    
001600*                                 PERSONKOD                               
001700        05 MOD-IDPERSON-TOM-ATTR                                          
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-IDPERSON-TOM  PIC Z(3).                                    
002100*                                 PERSONKOD                               
002200        05 MOD-IDPERSON-GRP  OCCURS 5 TIMES.                              
002300           07 MOD-IDPERSON-ATTR                                           
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600           07 MOD-IDPERSON   PIC Z(3).                                    
002700*                                 PERSONKOD                               
002800        05 MOD-FLAGGA-IDBERED-ATTR                                        
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-FLAGGA-IDBERED                                             
003200                             PIC X.                                       
003300*                                 ALLMÄN FLAGGA                           
003400        05 MOD-FLAGGA-IDLEVNR-SHIP-ATTR                                   
003500                             PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-FLAGGA-IDLEVNR-SHIP                                        
003800                             PIC X.                                       
003900*                                 ALLMÄN FLAGGA                           
004000        05 MOD-IDLEVNR-GRP   OCCURS 5 TIMES.                              
004100           07 MOD-IDLEVNR-ATTR                                            
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400           07 MOD-IDLEVNR    PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600        05 MOD-KDERS-FOM-ATTR                                             
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KDERS-FOM     PIC Z(2).                                    
005000*                                 ERSÄTTNINGSKOD                          
005100        05 MOD-KDERS-TOM-ATTR                                             
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-KDERS-TOM     PIC Z(2).                                    
005500*                                 ERSÄTTNINGSKOD                          
005600        05 MOD-KDERS-GRP     OCCURS 5 TIMES.                              
005700           07 MOD-KDERS-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900           07 MOD-KDERS      PIC Z(2).                                    
006000*                                 ERSÄTTNINGSKOD                          
006100        05 MOD-FLERSDAT-VIPS-ATTR                                         
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-FLERSDAT-VIPS PIC X.                                       
006500        05 MOD-IDFKNGRP-FOM-ATTR                                          
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-IDFKNGRP-FOM  PIC X(4).                                    
006900*                                 FUNKTIONSGRUPP                          
007000        05 MOD-IDFKNGRP-TOM-ATTR                                          
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-IDFKNGRP-TOM  PIC X(4).                                    
007400*                                 FUNKTIONSGRUPP                          
007500        05 MOD-FKN-GRP       OCCURS 5 TIMES.                              
007600           07 MOD-IDFKNGRP-ATTR                                           
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900           07 MOD-IDFKNGRP   PIC Z(3)9.                                   
008000*                                 FUNKTIONSGRUPP                          
008100        05 MOD-BEFT-GRP      OCCURS 4 TIMES.                              
008200           07 MOD-BEFT-ATTR  PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400           07 MOD-BEFT       PIC Z9.                                      
008500*                                 FÖRPACKNINGSTYP                         
008600        05 MOD-BEART-SOEK-ATTR                                            
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-BEART-SOEK    PIC X(25).                                   
009000*                                 ARTIKELBENÄMNING                        
009100        05 MOD-KDPRODSL-FOM-ATTR                                          
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400        05 MOD-KDPRODSL-FOM  PIC Z9.                                      
009500*                                 PRODUKTSLAG                             
009600        05 MOD-KDPRODSL-TOM-ATTR                                          
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900        05 MOD-KDPRODSL-TOM  PIC Z9.                                      
010000*                                 PRODUKTSLAG                             
010100        05 MOD-IDPROJ-URV-ATTR                                            
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-IDPROJ-URV    PIC X(4).                                    
010500*                                 PARTS PROJEKTIDENTITET                  
010600        05 MOD-ADLAGOMR-ATTR PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-ADLAGOMR      PIC Z9.                                      
010900*                                 LAGEROMRÅDE                             
011000        05 MOD-ADGANG-FOM-ATTR                                            
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 MOD-ADGANG-FOM    PIC Z9.                                      
011400*                                 LAGEROMRÅDE                             
011500        05 MOD-ADGANG-TOM-ATTR                                            
011600                             PIC X(2).                                    
011700*                                 MFS ATTRIBUTFÄLT                        
011800        05 MOD-ADGANG-TOM    PIC Z9.                                      
011900*                                 LAGEROMRÅDE                             
012000        05 MOD-KVVECKOR-KVPB-ATTR                                         
012100                             PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300        05 MOD-KVVECKOR-KVPB PIC Z9.                                      
012400*                                 ANTAL VECKOR                            
012500        05 MOD-KVVECKOR-AVROP-ATTR                                        
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800        05 MOD-KVVECKOR-AVROP                                             
012900                             PIC Z9.                                      
013000*                                 ANTAL VECKOR                            
013100     03 MOD-LISTA.                                                        
013200        05 MOD-FLAGGA-IDLEVNR-ATTR                                        
013300                             PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500        05 MOD-FLAGGA-IDLEVNR                                             
013600                             PIC X.                                       
013700*                                 ALLMÄN FLAGGA                           
013800        05 MOD-FLAGGA-BELEV-ATTR                                          
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100        05 MOD-FLAGGA-BELEV  PIC X.                                       
014200*                                 ALLMÄN FLAGGA                           
014300        05 MOD-FLAGGA-BEART-GB-ATTR                                       
014400                             PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600        05 MOD-FLAGGA-BEART-GB                                            
014700                             PIC X.                                       
014800*                                 ALLMÄN FLAGGA                           
014900        05 MOD-FLAGGA-KVLS-ATTR                                           
015000                             PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200        05 MOD-FLAGGA-KVLS   PIC X.                                       
015300*                                 ALLMÄN FLAGGA                           
015400        05 MOD-FLAGGA-KVAKS-ATTR                                          
015500                             PIC X(2).                                    
015600*                                 MFS ATTRIBUTFÄLT                        
015700        05 MOD-FLAGGA-KVAKS  PIC X.                                       
015800*                                 ALLMÄN FLAGGA                           
015900        05 MOD-FLAGGA-VOR-ROS-ATTR                                        
016000                             PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200        05 MOD-FLAGGA-VOR-ROS                                             
016300                             PIC X.                                       
016400*                                 ALLMÄN FLAGGA                           
016500        05 MOD-FLAGGA-KVAVROP-ATTR                                        
016600                             PIC X(2).                                    
016700*                                 MFS ATTRIBUTFÄLT                        
016800        05 MOD-FLAGGA-KVAVROP                                             
016900                             PIC X.                                       
017000*                                 ALLMÄN FLAGGA                           
017100        05 MOD-FLAGGA-IDLEVBSK-ATTR                                       
017200                             PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400        05 MOD-FLAGGA-IDLEVBSK                                            
017500                             PIC X.                                       
017600*                                 ALLMÄN FLAGGA                           
017700        05 MOD-FLAGGA-KVAVIS-ATTR                                         
017800                             PIC X(2).                                    
017900*                                 MFS ATTRIBUTFÄLT                        
018000        05 MOD-FLAGGA-KVAVIS PIC X.                                       
018100*                                 ALLMÄN FLAGGA                           
018200        05 MOD-FLAGGA-KVVECKOR-LT-ATTR                                    
018300                             PIC X(2).                                    
018400*                                 MFS ATTRIBUTFÄLT                        
018500        05 MOD-FLAGGA-KVVECKOR-LT                                         
018600                             PIC X.                                       
018700*                                 ALLMÄN FLAGGA                           
018800        05 MOD-FLAGGA-IDANSK-ATTR                                         
018900                             PIC X(2).                                    
019000*                                 MFS ATTRIBUTFÄLT                        
019100        05 MOD-FLAGGA-IDANSK PIC X.                                       
019200*                                 ALLMÄN FLAGGA                           
019300        05 MOD-FLAGGA-TILEVDAG-ATTR                                       
019400                             PIC X(2).                                    
019500*                                 MFS ATTRIBUTFÄLT                        
019600        05 MOD-FLAGGA-TILEVDAG                                            
019700                             PIC X.                                       
019800*                                 ALLMÄN FLAGGA                           
019900        05 MOD-FLAGGA-INLEV-ATTR                                          
020000                             PIC X(2).                                    
020100*                                 MFS ATTRIBUTFÄLT                        
020200        05 MOD-FLAGGA-INLEV  PIC X.                                       
020300*                                 ALLMÄN FLAGGA                           
020400        05 MOD-FLAGGA-IDINK-ATTR                                          
020500                             PIC X(2).                                    
020600*                                 MFS ATTRIBUTFÄLT                        
020700        05 MOD-FLAGGA-IDINK  PIC X.                                       
020800*                                 ALLMÄN FLAGGA                           
020900        05 MOD-FLAGGA-KDAVT-ATTR                                          
021000                             PIC X(2).                                    
021100*                                 MFS ATTRIBUTFÄLT                        
021200        05 MOD-FLAGGA-KDAVT  PIC X.                                       
021300*                                 ALLMÄN FLAGGA                           
021400        05 MOD-FLAGGA-KVPB-ATTR                                           
021500                             PIC X(2).                                    
021600*                                 MFS ATTRIBUTFÄLT                        
021700        05 MOD-FLAGGA-KVPB   PIC X.                                       
021800*                                 ALLMÄN FLAGGA                           
021900        05 MOD-FLAGGA-TREND-ATTR                                          
022000                             PIC X(2).                                    
022100*                                 MFS ATTRIBUTFÄLT                        
022200        05 MOD-FLAGGA-TREND  PIC X.                                       
022300*                                 ALLMÄN FLAGGA                           
022400        05 MOD-FLAGGA-SEASON-ATTR                                         
022500                             PIC X(2).                                    
022600*                                 MFS ATTRIBUTFÄLT                        
022700        05 MOD-FLAGGA-SEASON PIC X.                                       
022800*                                 ALLMÄN FLAGGA                           
022900        05 MOD-FLAGGA-KVOI-ATTR                                           
023000                             PIC X(2).                                    
023100*                                 MFS ATTRIBUTFÄLT                        
023200        05 MOD-FLAGGA-KVOI   PIC X.                                       
023300*                                 ALLMÄN FLAGGA                           
023400        05 MOD-FLAGGA-KVOI-IAR-PL-5-ATTR                                  
023500                             PIC X(2).                                    
023600*                                 MFS ATTRIBUTFÄLT                        
023700        05 MOD-FLAGGA-KVOI-IAR-PL-5                                       
023800                             PIC X.                                       
023900*                                 ALLMÄN FLAGGA                           
024000        05 MOD-FLAGGA-KDERS-ATTR                                          
024100                             PIC X(2).                                    
024200*                                 MFS ATTRIBUTFÄLT                        
024300        05 MOD-FLAGGA-KDERS  PIC X.                                       
024400*                                 ALLMÄN FLAGGA                           
024500        05 MOD-FLAGGA-TIERSDAT-VIPS-ATTR                                  
024600                             PIC X(2).                                    
024700*                                 MFS ATTRIBUTFÄLT                        
024800        05 MOD-FLAGGA-TIERSDAT-VIPS                                       
024900                             PIC X.                                       
025000*                                 ALLMÄN FLAGGA                           
025100        05 MOD-FLAGGA-PRMATRL-ATTR                                        
025200                             PIC X(2).                                    
025300*                                 MFS ATTRIBUTFÄLT                        
025400        05 MOD-FLAGGA-PRMATRL                                             
025500                             PIC X.                                       
025600*                                 ALLMÄN FLAGGA                           
025700        05 MOD-FLAGGA-KDFPKPRI-ATTR                                       
025800                             PIC X(2).                                    
025900*                                 MFS ATTRIBUTFÄLT                        
026000        05 MOD-FLAGGA-KDFPKPRI                                            
026100                             PIC X.                                       
026200*                                 ALLMÄN FLAGGA                           
026300        05 MOD-FLAGGA-KDSORT-ATTR                                         
026400                             PIC X(2).                                    
026500*                                 MFS ATTRIBUTFÄLT                        
026600        05 MOD-FLAGGA-KDSORT PIC X.                                       
026700*                                 ALLMÄN FLAGGA                           
026800        05 MOD-FLAGGA-TIREFSTO-LOC-ATTR                                   
026900                             PIC X(2).                                    
027000*                                 MFS ATTRIBUTFÄLT                        
027100        05 MOD-FLAGGA-TIREFSTO-LOC                                        
027200                             PIC X.                                       
027300*                                 ALLMÄN FLAGGA                           
027400        05 MOD-FLAGGA-VKART-VLARTNTO-ATTR                                 
027500                             PIC X(2).                                    
027600*                                 MFS ATTRIBUTFÄLT                        
027700        05 MOD-FLAGGA-VKART-VLARTNTO                                      
027800                             PIC X.                                       
027900*                                 ALLMÄN FLAGGA                           
028000        05 MOD-FLAGGA-TIFINLV-ATTR                                        
028100                             PIC X(2).                                    
028200*                                 MFS ATTRIBUTFÄLT                        
028300        05 MOD-FLAGGA-TIFINLV                                             
028400                             PIC X.                                       
028500*                                 ALLMÄN FLAGGA                           
028600        05 MOD-FLAGGA-DAPUBL-ATTR                                         
028700                             PIC X(2).                                    
028800*                                 MFS ATTRIBUTFÄLT                        
028900        05 MOD-FLAGGA-DAPUBL PIC X.                                       
029000*                                 ALLMÄN FLAGGA                           
029100        05 MOD-FLAGGA-TIURPROD-ATTR                                       
029200                             PIC X(2).                                    
029300*                                 MFS ATTRIBUTFÄLT                        
029400        05 MOD-FLAGGA-TIURPROD                                            
029500                             PIC X.                                       
029600*                                 ALLMÄN FLAGGA                           
029700        05 MOD-FLAGGA-KDPRODSL-ATTR                                       
029800                             PIC X(2).                                    
029900*                                 MFS ATTRIBUTFÄLT                        
030000        05 MOD-FLAGGA-KDPRODSL                                            
030100                             PIC X.                                       
030200*                                 ALLMÄN FLAGGA                           
030300        05 MOD-FLAGGA-IDFKNGRP-ATTR                                       
030400                             PIC X(2).                                    
030500*                                 MFS ATTRIBUTFÄLT                        
030600        05 MOD-FLAGGA-IDFKNGRP                                            
030700                             PIC X.                                       
030800*                                 ALLMÄN FLAGGA                           
030900        05 MOD-FLAGGA-SERVICE-ATTR                                        
031000                             PIC X(2).                                    
031100*                                 MFS ATTRIBUTFÄLT                        
031200        05 MOD-FLAGGA-SERVICE                                             
031300                             PIC X.                                       
031400*                                 ALLMÄN FLAGGA                           
031500        05 MOD-FLAGGA-PARM-ATTR                                           
031600                             PIC X(2).                                    
031700*                                 MFS ATTRIBUTFÄLT                        
031800        05 MOD-FLAGGA-PARM   PIC X.                                       
031900*                                 ALLMÄN FLAGGA                           
032000        05 MOD-FLAGGA-KVREFBER-ATTR                                       
032100                             PIC X(2).                                    
032200*                                 MFS ATTRIBUTFÄLT                        
032300        05 MOD-FLAGGA-KVREFBER                                            
032400                             PIC X.                                       
032500*                                 ALLMÄN FLAGGA                           
032600        05 MOD-FLAGGA-IDARTNR-EMB-ATTR                                    
032700                             PIC X(2).                                    
032800*                                 MFS ATTRIBUTFÄLT                        
032900        05 MOD-FLAGGA-IDARTNR-EMB                                         
033000                             PIC X.                                       
033100*                                 ALLMÄN FLAGGA                           
033200        05 MOD-FLAGGA-KVREFOVL-ATTR                                       
033300                             PIC X(2).                                    
033400*                                 MFS ATTRIBUTFÄLT                        
033500        05 MOD-FLAGGA-KVREFOVL                                            
033600                             PIC X.                                       
033700*                                 ALLMÄN FLAGGA                           
033800        05 MOD-FLAGGA-KVSPANT-ATTR                                        
033900                             PIC X(2).                                    
034000*                                 MFS ATTRIBUTFÄLT                        
034100        05 MOD-FLAGGA-KVSPANT                                             
034200                             PIC X.                                       
034300*                                 ALLMÄN FLAGGA                           
034400        05 MOD-FLAGGA-KVSLAGER-ATTR                                       
034500                             PIC X(2).                                    
034600*                                 MFS ATTRIBUTFÄLT                        
034700        05 MOD-FLAGGA-KVSLAGER                                            
034800                             PIC X.                                       
034900*                                 ALLMÄN FLAGGA                           
035000        05 MOD-FLAGGA-KDARTURS-ATTR                                       
035100                             PIC X(2).                                    
035200*                                 MFS ATTRIBUTFÄLT                        
035300        05 MOD-FLAGGA-KDARTURS                                            
035400                             PIC X.                                       
035500*                                 ALLMÄN FLAGGA                           
035600        05 MOD-FLAGGA-ADLAGOMR-ATTR                                       
035700                             PIC X(2).                                    
035800*                                 MFS ATTRIBUTFÄLT                        
035900        05 MOD-FLAGGA-ADLAGOMR                                            
036000                             PIC X.                                       
036100*                                 ALLMÄN FLAGGA                           
036200        05 MOD-FLAGGA-FLJIT-ATTR                                          
036300                             PIC X(2).                                    
036400*                                 MFS ATTRIBUTFÄLT                        
036500        05 MOD-FLAGGA-FLJIT  PIC X.                                       
036600*                                 ALLMÄN FLAGGA                           
036700        05 MOD-FLAGGA-IDPROJ-IDKAT-ATTR                                   
036800                             PIC X(2).                                    
036900*                                 MFS ATTRIBUTFÄLT                        
037000        05 MOD-FLAGGA-IDPROJ-IDKAT                                        
037100                             PIC X.                                       
037200*                                 ALLMÄN FLAGGA                           
037300        05 MOD-FLAGGA-FLLSRDEL-ATTR                                       
037400                             PIC X(2).                                    
037500*                                 MFS ATTRIBUTFÄLT                        
037600        05 MOD-FLAGGA-FLLSRDEL                                            
037700                             PIC X.                                       
037800*                                 ALLMÄN FLAGGA                           
037900        05 MOD-FLAGGA-IDKR-ATTR                                           
038000                             PIC X(2).                                    
038100*                                 MFS ATTRIBUTFÄLT                        
038200        05 MOD-FLAGGA-IDKR   PIC X.                                       
038300*                                 ALLMÄN FLAGGA                           
038400     03 MOD-LIST REDEFINES MOD-LISTA.                                     
038500        05 MOD-LST           OCCURS 45 TIMES.                             
038600           07 MOD-FLAGGA-ATTR                                             
038700                             PIC X(2).                                    
038800*                                 MFS ATTRIBUTFÄLT                        
038900           07 MOD-FLAGGA     PIC X.                                       
039000*                                 ALLMÄN FLAGGA                           
039100     03 MOD-KDARBTYP-UT-ATTR PIC X(2).                                    
039200*                                 MFS ATTRIBUTFÄLT                        
039300     03 MOD-KDARBTYP-UT      PIC X(4).                                    
039400*                                 TYP AV ARBETE                           
039500     03 MOD-IDPERSON-UT-ATTR PIC X(2).                                    
039600*                                 MFS ATTRIBUTFÄLT                        
039700     03 MOD-IDPERSON-UT      PIC X(3).                                    
039800*                                 PERSONKOD                               
039900     03 MOD-TEMFSINF         PIC X(55).                                   
040000*                                 INFORMATIONSMEDDELANDE                  
040100*** END OF VILMAII-COPY LENGTH= 475 BYTES                                 
