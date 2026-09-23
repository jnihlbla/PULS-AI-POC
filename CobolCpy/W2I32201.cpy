000100 01  MID-W2I32201.                                                        
000200*                                 MID-COPYTEXT FÖR W2032200               
000300     03 MID-URVAL.                                                        
000400*                                                                         
000500        05 MID-IDPERSON-FOM  PIC X(3).                                    
000600*                                 PERSONKOD                               
000700        05 MID-IDPERSON-TOM  PIC X(3).                                    
000800*                                 PERSONKOD                               
000900        05 MID-IDPERSON      OCCURS 5 TIMES                               
001000                             PIC X(3).                                    
001100*                                 PERSONKOD                               
001200        05 MID-FLAGGA-IDBERED                                             
001300                             PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500        05 MID-FLAGGA-IDLEVNR-SHIP                                        
001600                             PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800        05 MID-IDLEVNR       OCCURS 9 TIMES                               
001900                             PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 MID-KDERS-FOM     PIC X(2).                                    
002200*                                 ERSÄTTNINGSKOD                          
002300        05 MID-KDERS-TOM     PIC X(2).                                    
002400*                                 ERSÄTTNINGSKOD                          
002500        05 MID-KDERS         OCCURS 5 TIMES                               
002600                             PIC X(2).                                    
002700*                                 ERSÄTTNINGSKOD                          
002800        05 MID-KDOTFREK      PIC X.                                       
002900*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
003000        05 MID-IDFKNGRP-FOM  PIC X(4).                                    
003100*                                 FUNKTIONSGRUPP                          
003200        05 MID-IDFKNGRP-TOM  PIC X(4).                                    
003300*                                 FUNKTIONSGRUPP                          
003400        05 MID-IDFKNGRP      OCCURS 4 TIMES                               
003500                             PIC X(4).                                    
003600*                                 FUNKTIONSGRUPP                          
003700        05 MID-BEFT          OCCURS 4 TIMES                               
003800                             PIC X(2).                                    
003900*                                 FÖRPACKNINGSTYP                         
004000        05 MID-BEART-SOEK    PIC X(25).                                   
004100*                                 ARTIKELBENÄMNING                        
004200        05 MID-KDPRODSL-FOM  PIC X(2).                                    
004300*                                 PRODUKTSLAG                             
004400        05 MID-KDPRODSL-TOM  PIC X(2).                                    
004500*                                 PRODUKTSLAG                             
004600        05 MID-IDPROJ-URV    PIC X(4).                                    
004700*                                 PARTS PROJEKTIDENTITET                  
004800        05 MID-ADLAGOMR      PIC X(2).                                    
004900*                                 LAGEROMRÅDE                             
005000        05 MID-ADGANG-FOM    PIC X(2).                                    
005100*                                 GÅNG                                    
005200        05 MID-ADGANG-TOM    PIC X(2).                                    
005300*                                 GÅNG                                    
005400        05 MID-KVVECKOR-KVPB PIC X(2).                                    
005500*                                 ANTAL VECKOR                            
005600        05 MID-KVVECKOR-AVROP                                             
005700                             PIC X(2).                                    
005800*                                 ANTAL VECKOR                            
005900     03 MID-LISTA.                                                        
006000*                                                                         
006100        05 MID-FLAGGA-IDLEVNR                                             
006200                             PIC X.                                       
006300*                                 ALLMÄN FLAGGA                           
006400        05 MID-FLAGGA-LEVBET PIC X.                                       
006500*                                 ALLMÄN FLAGGA                           
006600        05 MID-FLAGGA-BEART-S                                             
006700                             PIC X.                                       
006800*                                 ALLMÄN FLAGGA                           
006900        05 MID-FLAGGA-BEART-GB                                            
007000                             PIC X.                                       
007100*                                 ALLMÄN FLAGGA                           
007200        05 MID-FLAGGA-STONHAND                                            
007300                             PIC X.                                       
007400*                                 ALLMÄN FLAGGA                           
007500        05 MID-FLAGGA-KVAKS  PIC X.                                       
007600*                                 ALLMÄN FLAGGA                           
007700        05 MID-FLAGGA-VOR-ROS                                             
007800                             PIC X.                                       
007900*                                 ALLMÄN FLAGGA                           
008000        05 MID-FLAGGA-KVSLAP PIC X.                                       
008100*                                 ALLMÄN FLAGGA                           
008200        05 MID-FLAGGA-LEVBESK                                             
008300                             PIC X.                                       
008400*                                 ALLMÄN FLAGGA                           
008500        05 MID-FLAGGA-KVART-FORAVIS                                       
008600                             PIC X.                                       
008700*                                 ALLMÄN FLAGGA                           
008800        05 MID-FLAGGA-TPO    PIC X.                                       
008900*                                 ALLMÄN FLAGGA                           
009000        05 MID-FLAGGA-KVVECKOR-LT                                         
009100                             PIC X.                                       
009200*                                 ALLMÄN FLAGGA                           
009300        05 MID-FLAGGA-KDERS  PIC X.                                       
009400*                                 ALLMÄN FLAGGA                           
009500        05 MID-FLAGGA-IDANSK PIC X.                                       
009600*                                 ALLMÄN FLAGGA                           
009700        05 MID-FLAGGA-DIRLEV PIC X.                                       
009800*                                 ALLMÄN FLAGGA                           
009900        05 MID-FLAGGA-AVSDAG PIC X.                                       
010000*                                 ALLMÄN FLAGGA                           
010100        05 MID-FLAGGA-TREND  PIC X.                                       
010200*                                 ALLMÄN FLAGGA                           
010300        05 MID-FLAGGA-SAESONG                                             
010400                             PIC X.                                       
010500*                                 ALLMÄN FLAGGA                           
010600        05 MID-FLAGGA-TISINLV                                             
010700                             PIC X.                                       
010800*                                 ALLMÄN FLAGGA                           
010900        05 MID-FLAGGA-KVPB   PIC X.                                       
011000*                                 ALLMÄN FLAGGA                           
011100        05 MID-FLAGGA-KVOI-RULL                                           
011200                             PIC X.                                       
011300*                                 ALLMÄN FLAGGA                           
011400        05 MID-FLAGGA-KVOI-IAR-PL-5                                       
011500                             PIC X.                                       
011600*                                 ALLMÄN FLAGGA                           
011700        05 MID-FLAGGA-KDLEVPLF                                            
011800                             PIC X.                                       
011900*                                 ALLMÄN FLAGGA                           
012000        05 MID-FLAGGA-IDINK  PIC X.                                       
012100*                                 ALLMÄN FLAGGA                           
012200        05 MID-FLAGGA-PRARTBES                                            
012300                             PIC X.                                       
012400*                                 ALLMÄN FLAGGA                           
012500        05 MID-FLAGGA-BESTREST                                            
012600                             PIC X.                                       
012700*                                 ALLMÄN FLAGGA                           
012800        05 MID-FLAGGA-AVTAL  PIC X.                                       
012900*                                 ALLMÄN FLAGGA                           
013000        05 MID-FLAGGA-FLFORP-SI                                           
013100                             PIC X.                                       
013200*                                 ALLMÄN FLAGGA                           
013300        05 MID-FLAGGA-VKART-VLARTNTO                                      
013400                             PIC X.                                       
013500*                                 ALLMÄN FLAGGA                           
013600        05 MID-FLAGGA-TIREFSTO                                            
013700                             PIC X.                                       
013800*                                 ALLMÄN FLAGGA                           
013900        05 MID-FLAGGA-IDPROJ PIC X.                                       
014000*                                 ALLMÄN FLAGGA                           
014100        05 MID-FLAGGA-TIFINLV                                             
014200                             PIC X.                                       
014300*                                 ALLMÄN FLAGGA                           
014400        05 MID-FLAGGA-TIURPROD                                            
014500                             PIC X.                                       
014600*                                 ALLMÄN FLAGGA                           
014700        05 MID-FLAGGA-KVDISP PIC X.                                       
014800*                                 ALLMÄN FLAGGA                           
014900        05 MID-FLAGGA-KDPRODSL                                            
015000                             PIC X.                                       
015100*                                 ALLMÄN FLAGGA                           
015200        05 MID-FLAGGA-IDFKNGRP                                            
015300                             PIC X.                                       
015400*                                 ALLMÄN FLAGGA                           
015500        05 MID-FLAGGA-FLIART PIC X.                                       
015600*                                 ALLMÄN FLAGGA                           
015700        05 MID-FLAGGA-STYRPARAM                                           
015800                             PIC X.                                       
015900*                                 ALLMÄN FLAGGA                           
016000        05 MID-FLAGGA-SERVICE                                             
016100                             PIC X.                                       
016200*                                 ALLMÄN FLAGGA                           
016300        05 MID-FLAGGA-KDSORT PIC X.                                       
016400*                                 ALLMÄN FLAGGA                           
016500        05 MID-FLAGGA-KVANTER                                             
016600                             PIC X.                                       
016700*                                 ALLMÄN FLAGGA                           
016800        05 MID-FLAGGA-FORPINFO                                            
016900                             PIC X.                                       
017000*                                 ALLMÄN FLAGGA                           
017100        05 MID-FLAGGA-KVSPANT                                             
017200                             PIC X.                                       
017300*                                 ALLMÄN FLAGGA                           
017400        05 MID-FLAGGA-S-LAGER                                             
017500                             PIC X.                                       
017600*                                 ALLMÄN FLAGGA                           
017700        05 MID-FLAGGA-KVMP   PIC X.                                       
017800*                                 ALLMÄN FLAGGA                           
017900        05 MID-FLAGGA-IDKR   PIC X.                                       
018000*                                 ALLMÄN FLAGGA                           
018100        05 MID-FLAGGA-KAMPANJ                                             
018200                             PIC X.                                       
018300*                                 ALLMÄN FLAGGA                           
018400        05 MID-FLAGGA-ADART  PIC X.                                       
018500*                                 ALLMÄN FLAGGA                           
018600        05 MID-FLAGGA-ADINPORT                                            
018700                             PIC X.                                       
018800*                                 ALLMÄN FLAGGA                           
018900        05 MID-FLAGGA-KDUART PIC X.                                       
019000*                                 ALLMÄN FLAGGA                           
019100        05 MID-FLAGGA-URSPRUNG                                            
019200                             PIC X.                                       
019300*                                 ALLMÄN FLAGGA                           
019400        05 MID-FLAGGA-KDOTFREK                                            
019500                             PIC X.                                       
019600*                                 ALLMÄN FLAGGA                           
019700     03 MID-LIST REDEFINES MID-LISTA.                                     
019800*                                                                         
019900        05 MID-FLAGGA        OCCURS 52 TIMES                              
020000                             PIC X.                                       
020100*                                 ALLMÄN FLAGGA                           
020200     03 MID-KDARBTYP-IN      PIC X(4).                                    
020300*                                 TYP AV ARBETE                           
020400     03 MID-IDPERSON-IN      PIC X(3).                                    
020500*                                 PERSONKOD                               
020600*** END OF VILMAII-COPY LENGTH= 217 BYTES                                 
