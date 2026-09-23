000100 01  MID-W2I42201.                                                        
000200*                                 MID-COPYTEXT FÖR W2042200               
000300     03 MID-KDARBTYP-IN      PIC X(4).                                    
000400*                                 TYP AV ARBETE                           
000500     03 MID-IDPERSON-IN      PIC X(3).                                    
000600*                                 PERSONKOD                               
000700     03 MID-URVAL.                                                        
000800*                                                                         
000900        05 MID-IDDC          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 MID-IDPERSON-FOM  PIC X(3).                                    
001200*                                 PERSONKOD                               
001300        05 MID-IDPERSON-TOM  PIC X(3).                                    
001400*                                 PERSONKOD                               
001500        05 MID-IDPERSON      OCCURS 5 TIMES                               
001600                             PIC X(3).                                    
001700*                                 PERSONKOD                               
001800        05 MID-FLAGGA-IDBERED                                             
001900                             PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100        05 MID-FLAGGA-IDLEVNR-SHIP                                        
002200                             PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400        05 MID-IDLEVNR       OCCURS 5 TIMES                               
002500                             PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700        05 MID-KDERS-FOM     PIC X(2).                                    
002800*                                 ERSÄTTNINGSKOD                          
002900        05 MID-KDERS-TOM     PIC X(2).                                    
003000*                                 ERSÄTTNINGSKOD                          
003100        05 MID-KDERS         OCCURS 5 TIMES                               
003200                             PIC X(2).                                    
003300*                                 ERSÄTTNINGSKOD                          
003400        05 MID-FLERSDAT-VIPS PIC X.                                       
003500        05 MID-IDFKNGRP-FOM  PIC X(4).                                    
003600*                                 FUNKTIONSGRUPP                          
003700        05 MID-IDFKNGRP-TOM  PIC X(4).                                    
003800*                                 FUNKTIONSGRUPP                          
003900        05 MID-IDFKNGRP      OCCURS 5 TIMES                               
004000                             PIC X(4).                                    
004100*                                 FUNKTIONSGRUPP                          
004200        05 MID-BEFT          OCCURS 4 TIMES                               
004300                             PIC X(2).                                    
004400*                                 FÖRPACKNINGSTYP                         
004500        05 MID-BEART-SOEK    PIC X(25).                                   
004600*                                 ARTIKELBENÄMNING                        
004700        05 MID-KDPRODSL-FOM  PIC X(2).                                    
004800*                                 PRODUKTSLAG                             
004900        05 MID-KDPRODSL-TOM  PIC X(2).                                    
005000*                                 PRODUKTSLAG                             
005100        05 MID-IDPROJ-URV    PIC X(4).                                    
005200*                                 PARTS PROJEKTIDENTITET                  
005300        05 MID-ADLAGOMR      PIC X(2).                                    
005400*                                 LAGEROMRÅDE                             
005500        05 MID-ADGANG-FOM    PIC X(2).                                    
005600*                                 GÅNG                                    
005700        05 MID-ADGANG-TOM    PIC X(2).                                    
005800*                                 GÅNG                                    
005900        05 MID-KVVECKOR-KVPB PIC X(2).                                    
006000*                                 ANTAL VECKOR                            
006100        05 MID-KVVECKOR-AVROP                                             
006200                             PIC X(2).                                    
006300*                                 ANTAL VECKOR                            
006400     03 MID-LISTA.                                                        
006500*                                                                         
006600        05 MID-FL-IDLEVNR    PIC X.                                       
006700*                                 ALLMÄN FLAGGA                           
006800        05 MID-FL-BELEV      PIC X.                                       
006900*                                 ALLMÄN FLAGGA                           
007000        05 MID-FL-BEART-GB   PIC X.                                       
007100*                                 ALLMÄN FLAGGA                           
007200        05 MID-FL-KVLS       PIC X.                                       
007300*                                 ALLMÄN FLAGGA                           
007400        05 MID-FL-KVAKS      PIC X.                                       
007500*                                 ALLMÄN FLAGGA                           
007600        05 MID-FL-VOR-ROS    PIC X.                                       
007700*                                 ALLMÄN FLAGGA                           
007800        05 MID-FL-KVAVROP    PIC X.                                       
007900*                                 ALLMÄN FLAGGA                           
008000        05 MID-FL-IDLEVBSK   PIC X.                                       
008100*                                 ALLMÄN FLAGGA                           
008200        05 MID-FL-KVAVIS     PIC X.                                       
008300*                                 ALLMÄN FLAGGA                           
008400        05 MID-FL-KVVECKOR-LT                                             
008500                             PIC X.                                       
008600*                                 ALLMÄN FLAGGA                           
008700        05 MID-FL-IDANSK     PIC X.                                       
008800*                                 ALLMÄN FLAGGA                           
008900        05 MID-FL-TILEVDAG   PIC X.                                       
009000*                                 ALLMÄN FLAGGA                           
009100        05 MID-FL-INLEV      PIC X.                                       
009200*                                 ALLMÄN FLAGGA                           
009300        05 MID-FL-IDINK      PIC X.                                       
009400*                                 ALLMÄN FLAGGA                           
009500        05 MID-FL-KDAVT      PIC X.                                       
009600*                                 ALLMÄN FLAGGA                           
009700        05 MID-FL-KVPB       PIC X.                                       
009800*                                 ALLMÄN FLAGGA                           
009900        05 MID-FL-TREND      PIC X.                                       
010000*                                 ALLMÄN FLAGGA                           
010100        05 MID-FL-SEASON     PIC X.                                       
010200*                                 ALLMÄN FLAGGA                           
010300        05 MID-FL-DEMHIST    PIC X.                                       
010400*                                 ALLMÄN FLAGGA                           
010500        05 MID-FL-DEMHIST-YEAR                                            
010600                             PIC X.                                       
010700*                                 ALLMÄN FLAGGA                           
010800        05 MID-FL-KDERS      PIC X.                                       
010900*                                 ALLMÄN FLAGGA                           
011000        05 MID-FL-TIERSDAT-VIPS                                           
011100                             PIC X.                                       
011200*                                 ALLMÄN FLAGGA                           
011300        05 MID-FL-PRMATRL-PRAVCOST                                        
011400                             PIC X.                                       
011500*                                 ALLMÄN FLAGGA                           
011600        05 MID-FL-KDFPKPRI   PIC X.                                       
011700*                                 ALLMÄN FLAGGA                           
011800        05 MID-FL-KDSORT     PIC X.                                       
011900*                                 ALLMÄN FLAGGA                           
012000        05 MID-FL-TIREFSTO-LOC                                            
012100                             PIC X.                                       
012200*                                 ALLMÄN FLAGGA                           
012300        05 MID-FL-VKART-VLARTNTO                                          
012400                             PIC X.                                       
012500*                                 ALLMÄN FLAGGA                           
012600        05 MID-FL-TIFINLV    PIC X.                                       
012700*                                 ALLMÄN FLAGGA                           
012800        05 MID-FL-DAPUBL     PIC X.                                       
012900*                                 ALLMÄN FLAGGA                           
013000        05 MID-FL-TIURPROD   PIC X.                                       
013100*                                 ALLMÄN FLAGGA                           
013200        05 MID-FL-KDPRODSL   PIC X.                                       
013300*                                 ALLMÄN FLAGGA                           
013400        05 MID-FL-IDFKNGRP   PIC X.                                       
013500*                                 ALLMÄN FLAGGA                           
013600        05 MID-FL-SERVICE    PIC X.                                       
013700*                                 ALLMÄN FLAGGA                           
013800        05 MID-FL-PARAMETER  PIC X.                                       
013900*                                 ALLMÄN FLAGGA                           
014000        05 MID-FL-QUANT      PIC X.                                       
014100*                                 ALLMÄN FLAGGA                           
014200        05 MID-FL-IDARTB-EMB PIC X.                                       
014300*                                 ALLMÄN FLAGGA                           
014400        05 MID-FL-KVREFOVL   PIC X.                                       
014500*                                 ALLMÄN FLAGGA                           
014600        05 MID-FL-KVSPANT    PIC X.                                       
014700*                                 ALLMÄN FLAGGA                           
014800        05 MID-FL-KVSLAGER   PIC X.                                       
014900*                                 ALLMÄN FLAGGA                           
015000        05 MID-FL-KDARTUTS   PIC X.                                       
015100*                                 ALLMÄN FLAGGA                           
015200        05 MID-FL-ADLAGOMR   PIC X.                                       
015300*                                 ALLMÄN FLAGGA                           
015400        05 MID-FL-FLJIT      PIC X.                                       
015500*                                 ALLMÄN FLAGGA                           
015600        05 MID-FL-IDPROJ     PIC X.                                       
015700*                                 ALLMÄN FLAGGA                           
015800        05 MID-FL-FLLSRDEL   PIC X.                                       
015900*                                 ALLMÄN FLAGGA                           
016000        05 MID-FL-KR         PIC X.                                       
016100*                                 ALLMÄN FLAGGA                           
016200     03 MID-LIST REDEFINES MID-LISTA.                                     
016300*                                                                         
016400        05 MID-FLAGGA        OCCURS 45 TIMES                              
016500                             PIC X.                                       
016600*                                 ALLMÄN FLAGGA                           
016700*** END OF VILMAII-COPY LENGTH= 196 BYTES                                 
