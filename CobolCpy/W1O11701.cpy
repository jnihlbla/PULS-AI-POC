000100 01  MOD-W1O11701.                                                        
000200*                                 MOD COPYTEXT FÖR W1011700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BEART            PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 MOD-IDBERED-ATTR     PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDBERED          PIC Z9.                                      
001600*                                 BEREDARENUMMER                          
001700     03 MOD-KDPRODSL-ATTR    PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-KDPRODSL         PIC Z9.                                      
002000*                                 PRODUKTSLAG                             
002100     03 MOD-KDSORT-ATTR      PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-KDSORT           PIC X(2).                                    
002400*                                 SORT-KOD                                
002500     03 MOD-IDPROENH-GRP     OCCURS 3 TIMES                               
002600                             INDEXED MOD-IDPROENH-IND.                    
002700        05 MOD-IDPROENH-ATTR PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-IDPROENH      PIC X(8).                                    
003000*                                 PRODUKTIONSENHET                        
003100     03 MOD-IDBERED-IN-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-IDBERED-IN       PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500     03 MOD-KDPRODSL-IN-ATTR PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDPRODSL-IN      PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900     03 MOD-KDSORT-IN-ATTR   PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDSORT-IN        PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-IDPROENH-IN-GRP  OCCURS 3 TIMES                               
004400                             INDEXED MOD-IDPROENH-IN-IND.                 
004500        05 MOD-IDPROENH-IN-ATTR                                           
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-IDPROENH-IN   PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000     03 MOD-KDUART-ATTR      PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KDUART           PIC X.                                       
005300*                                 UNDANTAGSARTIKEL                        
005400     03 MOD-IDPROJ-ATTR      PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDPROJ           PIC X(4).                                    
005700*                                 PARTS PROJEKTIDENTITET                  
005800     03 MOD-FLPISK-ATTR      PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-FLPISK           PIC X.                                       
006100*                                 PISK ARTIKEL                            
006200     03 MOD-IDKAT-GRP        OCCURS 3 TIMES                               
006300                             INDEXED MOD-IDKAT-IND.                       
006400        05 MOD-IDKAT-ATTR    PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-IDKAT         PIC X(5).                                    
006700*                                 KATALOGBETECKNING                       
006800     03 MOD-KDUART-IN-ATTR   PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-KDUART-IN        PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200     03 MOD-IDPROJ-IN-ATTR   PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-IDPROJ-IN        PIC X(2).                                    
007500*                                 MFS BEHANDLING AV INPUTFÄLT             
007600     03 MOD-FLPISK-IN-ATTR   PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-FLPISK-IN        PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000     03 MOD-IDKAT-IN-GRP     OCCURS 3 TIMES                               
008100                             INDEXED MOD-IDKAT-IN-IND.                    
008200        05 MOD-IDKAT-IN-ATTR PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-IDKAT-IN      PIC X(2).                                    
008500*                                 MFS BEHANDLING AV INPUTFÄLT             
008600     03 MOD-FLLSRDEL-ATTR    PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-FLLSRDEL         PIC X.                                       
008900*                                 LEVERERAS SOM RESDEL                    
009000     03 MOD-IDPROJK-ATTR     PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 MOD-IDPROJK          PIC X(4).                                    
009300*                                 PROJEKTIDENTITET KONSTRUKTION           
009400     03 MOD-KDBPSR-ATTR      PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-KDBPSR           PIC 9.                                       
009700*                                 BASLAGERFÖRSLAGSNIVÅ                    
009800     03 MOD-TIFINLV-ATTR     PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-TIFINLV          PIC 9(5).                                    
010100*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
010200     03 MOD-TISOP-ATTR       PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-TISOP            PIC 9(5).                                    
010500*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
010600     03 MOD-FLLSRDEL-IN-ATTR PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-FLLSRDEL-IN      PIC X(2).                                    
010900*                                 MFS BEHANDLING AV INPUTFÄLT             
011000     03 MOD-IDPROJK-IN-ATTR  PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-IDPROJK-IN       PIC X(2).                                    
011300*                                 MFS BEHANDLING AV INPUTFÄLT             
011400     03 MOD-KDBPSR-IN-ATTR   PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-KDBPSR-IN        PIC X(2).                                    
011700*                                 MFS BEHANDLING AV INPUTFÄLT             
011800     03 MOD-TISOP-IN-ATTR    PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-TISOP-IN         PIC X(2).                                    
012100*                                 MFS BEHANDLING AV INPUTFÄLT             
012200     03 MOD-IDFKNGRP-ATTR    PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
012500*                                 FUNKTIONSGRUPP                          
012600     03 MOD-IDPROJUP-ATTR    PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800     03 MOD-IDPROJUP         PIC X(8).                                    
012900*                                 PROJEKTUPPDRAG                          
013000     03 MOD-IDARTNR-MOTSV-ATTR                                            
013100                             PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300     03 MOD-IDARTNR-MOTSV    PIC Z(9).                                    
013400*                                 ARTIKELNUMMER                           
013500     03 MOD-IDFKNGRP-IN-ATTR PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 MOD-IDFKNGRP-IN      PIC X(2).                                    
013800*                                 MFS BEHANDLING AV INPUTFÄLT             
013900     03 MOD-IDPROJUP-IN-ATTR PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-IDPROJUP-IN      PIC X(2).                                    
014200*                                 MFS BEHANDLING AV INPUTFÄLT             
014300     03 MOD-IDARTNR-MOTSV-IN-ATTR                                         
014400                             PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600     03 MOD-IDARTNR-MOTSV-IN PIC X(2).                                    
014700*                                 MFS BEHANDLING AV INPUTFÄLT             
014800     03 MOD-FLBYTES-ATTR     PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000     03 MOD-FLBYTES          PIC X.                                       
015100*                                 BYTESARTIKEL                            
015200     03 MOD-IDRITN-ATTR      PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400     03 MOD-IDRITN           PIC X(10).                                   
015500*                                 RITNINGSNUMMER                          
015600     03 MOD-KVARTVAGN-ATTR   PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800     03 MOD-KVARTVAGN        PIC Z(3).                                    
015900*                                 ANTAL ARTIKLAR PER VAGN                 
016000     03 MOD-KVPROG-ATTR      PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200     03 MOD-KVPROG           PIC Z(6)9.                                   
016300*                                 ÅRSPROGNOS                              
016400     03 MOD-KDAGE-ATTR       PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 MOD-KDAGE            PIC X.                                       
016700*                                 AGE-CODE                                
016800     03 MOD-FLBYTES-IN-ATTR  PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-FLBYTES-IN       PIC X(2).                                    
017100*                                 MFS BEHANDLING AV INPUTFÄLT             
017200     03 MOD-IDRITN-IN-ATTR   PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400     03 MOD-IDRITN-IN        PIC X(2).                                    
017500*                                 MFS BEHANDLING AV INPUTFÄLT             
017600     03 MOD-KVARTVAGN-IN-ATTR                                             
017700                             PIC X(2).                                    
017800*                                 MFS ATTRIBUTFÄLT                        
017900     03 MOD-KVARTVAGN-IN     PIC X(2).                                    
018000*                                 MFS BEHANDLING AV INPUTFÄLT             
018100     03 MOD-KVPROG-IN-ATTR   PIC X(2).                                    
018200*                                 MFS ATTRIBUTFÄLT                        
018300     03 MOD-KVPROG-IN        PIC X(2).                                    
018400*                                 MFS BEHANDLING AV INPUTFÄLT             
018500     03 MOD-KDAGE-IN-ATTR    PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700     03 MOD-KDAGE-IN         PIC X(2).                                    
018800*                                 MFS BEHANDLING AV INPUTFÄLT             
018900     03 MOD-IDAO-GRP         OCCURS 5 TIMES                               
019000                             INDEXED MOD-IDAO-IND.                        
019100        05 MOD-IDAO-ATTR     PIC X(2).                                    
019200*                                 MFS ATTRIBUTFÄLT                        
019300        05 MOD-IDAO          PIC X(10).                                   
019400*                                 ÄNDRINGSORDERNUMMER                     
019500     03 MOD-IDAO-IN-GRP      OCCURS 5 TIMES                               
019600                             INDEXED MOD-IDAO-IN-IND.                     
019700        05 MOD-IDAO-IN-ATTR  PIC X(2).                                    
019800*                                 MFS ATTRIBUTFÄLT                        
019900        05 MOD-IDAO-IN       PIC X(2).                                    
020000*                                 MFS BEHANDLING AV INPUTFÄLT             
020100     03 MOD-TEORSAK-IN-UT-ATTR                                            
020200                             PIC X(2).                                    
020300*                                 MFS ATTRIBUTFÄLT                        
020400     03 MOD-TEORSAK-IN-UT    PIC X(50).                                   
020500*                                 INFO OM SLAG AV ÅTGÄRD                  
020600     03 MOD-TEVARNOT-IN-UT-ATTR                                           
020700                             PIC X(2).                                    
020800*                                 MFS ATTRIBUTFÄLT                        
020900     03 MOD-TEVARNOT-IN-UT   PIC X(40).                                   
021000*                                 ARTIKEL NOTERING                        
021100     03 MOD-TEMFSINF         PIC X(55).                                   
021200*                                 INFORMATIONSMEDDELANDE                  
021300*** END OF VILMAII-COPY LENGTH= 565 BYTES                                 
