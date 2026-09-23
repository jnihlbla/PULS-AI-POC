000100 01  MOD-W90410O1.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MOD-IDARTNR-UT       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDBERED-LAEST    PIC X(2).                                    
001100*                                 BEREDARENUMMER                          
001200     03 MOD-IDAO-LAEST       PIC X(10).                                   
001300*                                 ÄNDRINGSORDERNUMMER                     
001400     03 MOD-IDAO-VALD        PIC X(10).                                   
001500*                                 ÄNDRINGSORDERNUMMER                     
001600     03 MOD-IDPROJ-VALD      PIC X(4).                                    
001700*                                 PARTS PROJEKTIDENTITET                  
001800     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-IDARTNR-NY       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDBERED-UT       PIC Z9.                                      
002300*                                 BEREDARENUMMER                          
002400     03 MOD-KDPRODSL-UT      PIC Z9.                                      
002500*                                 PRODUKTSLAG                             
002600     03 MOD-KDSORT-UT        PIC X(2).                                    
002700*                                 SORT-KOD                                
002800     03 MOD-IDPROENH-1-UT    PIC X(8).                                    
002900*                                 PRODUKTIONSENHET                        
003000     03 MOD-IDPROENH-2-UT    PIC X(8).                                    
003100*                                 PRODUKTIONSENHET                        
003200     03 MOD-IDPROENH-3-UT    PIC X(8).                                    
003300*                                 PRODUKTIONSENHET                        
003400     03 MOD-IDBERED-ATTR     PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-IDBERED-IN       PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-KDPRODSL-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KDPRODSL-IN      PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200     03 MOD-KDSORT-ATTR      PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-KDSORT-IN        PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-IDPROENH-1-ATTR  PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-IDPROENH-1-IN    PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000     03 MOD-IDPROENH-2-ATTR  PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-IDPROENH-2-IN    PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400     03 MOD-IDPROENH-3-ATTR  PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDPROENH-3-IN    PIC X(2).                                    
005700*                                 MFS BEHANDLING AV INPUTFÄLT             
005800     03 MOD-KDYTBEH-UT       PIC Z9.                                      
005900*                                 YTBEHANDLINGSKOD                        
006000     03 MOD-IDPROJ-UT        PIC X(4).                                    
006100*                                 PARTS PROJEKTIDENTITET                  
006200     03 MOD-KDFARLIG-UT      PIC 9.                                       
006300*                                 KOD FÖR FARLIGT GODS                    
006400     03 MOD-KDBPSR-UT        PIC 9.                                       
006500*                                 BASLAGERFÖRSLAGSNIVÅ                    
006600     03 MOD-IDKAT-1-UT       PIC X(5).                                    
006700*                                 KATALOGBETECKNING                       
006800     03 MOD-IDKAT-2-UT       PIC X(5).                                    
006900*                                 KATALOGBETECKNING                       
007000     03 MOD-IDKAT-3-UT       PIC X(5).                                    
007100*                                 KATALOGBETECKNING                       
007200     03 MOD-KDYTBEH-ATTR     PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-KDYTBEH-IN       PIC X(2).                                    
007500*                                 MFS BEHANDLING AV INPUTFÄLT             
007600     03 MOD-IDPROJ-ATTR      PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-IDPROJ-IN        PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000     03 MOD-KDFARLIG-ATTR    PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-KDFARLIG-IN      PIC X(2).                                    
008300*                                 MFS BEHANDLING AV INPUTFÄLT             
008400     03 MOD-KDBPSR-ATTR      PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-KDBPSR-IN        PIC X(2).                                    
008700*                                 MFS BEHANDLING AV INPUTFÄLT             
008800     03 MOD-IDKAT-1-ATTR     PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDKAT-1-IN       PIC X(2).                                    
009100*                                 MFS BEHANDLING AV INPUTFÄLT             
009200     03 MOD-IDKAT-2-ATTR     PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-IDKAT-2-IN       PIC X(2).                                    
009500*                                 MFS BEHANDLING AV INPUTFÄLT             
009600     03 MOD-IDKAT-3-ATTR     PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-IDKAT-3-IN       PIC X(2).                                    
009900*                                 MFS BEHANDLING AV INPUTFÄLT             
010000     03 MOD-KDUART-UT        PIC X.                                       
010100*                                 UNDANTAGSARTIKEL                        
010200     03 MOD-IDPROJK-UT       PIC X(4).                                    
010300*                                 PROJEKTIDENTITET KONSTRUKTION           
010400     03 MOD-FLPISK-UT        PIC X.                                       
010500*                                 PISK ARTIKEL                            
010600     03 MOD-IDAO-UT          PIC X(10).                                   
010700*                                 ÄNDRINGSORDERNUMMER                     
010800     03 MOD-TISOP-UT         PIC Z(4)9.                                   
010900*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
011000     03 MOD-KDUART-ATTR      PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-KDUART-IN        PIC X(2).                                    
011300*                                 MFS BEHANDLING AV INPUTFÄLT             
011400     03 MOD-IDPROJK-ATTR     PIC X(2).                                    
011500     03 MOD-IDPROJK-IN       PIC X(2).                                    
011600*                                 MFS BEHANDLING AV INPUTFÄLT             
011700     03 MOD-FLPISK-ATTR      PIC X(2).                                    
011800     03 MOD-FLPISK-IN        PIC X(2).                                    
011900*                                 MFS BEHANDLING AV INPUTFÄLT             
012000     03 MOD-IDAO-ATTR        PIC X(2).                                    
012100     03 MOD-IDAO-IN          PIC X(2).                                    
012200*                                 MFS BEHANDLING AV INPUTFÄLT             
012300     03 MOD-TISOP-ATTR       PIC X(2).                                    
012400     03 MOD-TISOP-IN         PIC X(2).                                    
012500*                                 MFS BEHANDLING AV INPUTFÄLT             
012600     03 MOD-FLLSRDEL-UT      PIC X.                                       
012700*                                 LEVERERAS SOM RESDEL                    
012800     03 MOD-IDPROJUP-UT      PIC X(8).                                    
012900*                                 PROJEKTUPPDRAG                          
013000     03 MOD-BEART-UT         PIC X(25).                                   
013100*                                 ARTIKELBENÄMNING                        
013200     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 MOD-IDSKYLT-IN       PIC X(2).                                    
013500*                                 MFS BEHANDLING AV INPUTFÄLT             
013600     03 MOD-FLLSRDEL-ATTR    PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800     03 MOD-FLLSRDEL-IN      PIC X(2).                                    
013900*                                 MFS BEHANDLING AV INPUTFÄLT             
014000     03 MOD-IDPROJUP-ATTR    PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200     03 MOD-IDPROJUP-IN      PIC X(2).                                    
014300*                                 MFS BEHANDLING AV INPUTFÄLT             
014400     03 MOD-BEART-ATTR       PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600     03 MOD-BEART-IN         PIC X(2).                                    
014700*                                 MFS BEHANDLING AV INPUTFÄLT             
014800     03 MOD-FLRSBEART-ATTR   PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000     03 MOD-FLRSBEART-IN     PIC X(2).                                    
015100*                                 MFS BEHANDLING AV INPUTFÄLT             
015200     03 MOD-IDFKNGRP-UT      PIC Z(3)9.                                   
015300*                                 FUNKTIONSGRUPP                          
015400     03 MOD-IDLEVNR-UT       PIC X(5).                                    
015500*                                 LEVERANTÖRNUMMER                        
015600     03 MOD-BELEV-UT         PIC X(30).                                   
015700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
015800     03 MOD-KVPROG-UT        PIC Z(6)9.                                   
015900*                                 ÅRSPROGNOS                              
016000     03 MOD-IDFKNGRP-ATTR    PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200     03 MOD-IDFKNGRP-IN      PIC X(2).                                    
016300*                                 MFS BEHANDLING AV INPUTFÄLT             
016400     03 MOD-IDLEVNR-ATTR     PIC X(2).                                    
016500     03 MOD-IDLEVNR-IN       PIC X(2).                                    
016600*                                 MFS BEHANDLING AV INPUTFÄLT             
016700     03 MOD-BELEV-ATTR       PIC X(2).                                    
016800     03 MOD-BELEV-IN         PIC X(2).                                    
016900*                                 MFS BEHANDLING AV INPUTFÄLT             
017000     03 MOD-KVPROG-ATTR      PIC X(2).                                    
017100*                                 MFS ATTRIBUTFÄLT                        
017200     03 MOD-KVPROG-IN        PIC X(2).                                    
017300*                                 MFS BEHANDLING AV INPUTFÄLT             
017400     03 MOD-TEORSAK-1-ATTR   PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600     03 MOD-TEORSAK-1        PIC X(50).                                   
017700*                                 INFO OM SLAG AV ÅTGÄRD                  
017800     03 MOD-IDRITN-UT        PIC X(10).                                   
017900*                                 RITNINGSNUMMER                          
018000     03 MOD-KVARTVAGN-UT     PIC Z(3).                                    
018100*                                 ANTAL ARTIKLAR PER VAGN                 
018200     03 MOD-TEARTNOT-2-ATTR  PIC X(2).                                    
018300*                                 MFS ATTRIBUTFÄLT                        
018400     03 MOD-TEARTNOT-2       PIC X(40).                                   
018500*                                 ARTIKEL NOTERING                        
018600     03 MOD-IDRITN-ATTR      PIC X(2).                                    
018700*                                 MFS ATTRIBUTFÄLT                        
018800     03 MOD-IDRITN-IN        PIC X(2).                                    
018900*                                 MFS BEHANDLING AV INPUTFÄLT             
019000     03 MOD-KVARTVAGN-ATTR   PIC X(2).                                    
019100*                                 MFS ATTRIBUTFÄLT                        
019200     03 MOD-KVARTVAGN-IN     PIC X(2).                                    
019300*                                 MFS BEHANDLING AV INPUTFÄLT             
019400     03 MOD-TEARTNOT-7-ATTR  PIC X(2).                                    
019500*                                 MFS ATTRIBUTFÄLT                        
019600     03 MOD-TEARTNOT-7       PIC X(40).                                   
019700*                                 ARTIKEL NOTERING                        
019800     03 MOD-IDARTNR-MOTSV-UT PIC Z(9).                                    
019900*                                 MOTSVARANDE ARTIKEL                     
020000     03 MOD-FLBYTES-UT       PIC X.                                       
020100*                                 BYTESARTIKEL                            
020200     03 MOD-TEARTNOT-4-ATTR  PIC X(2).                                    
020300*                                 MFS ATTRIBUTFÄLT                        
020400     03 MOD-TEARTNOT-4       PIC X(40).                                   
020500*                                 ARTIKEL NOTERING                        
020600     03 MOD-IDARTNR-MOTSV-ATTR                                            
020700                             PIC X(2).                                    
020800*                                 MFS ATTRIBUTFÄLT                        
020900     03 MOD-IDARTNR-MOTSV-IN PIC X(2).                                    
021000*                                 MFS BEHANDLING AV INPUTFÄLT             
021100     03 MOD-FLBYTES-ATTR     PIC X(2).                                    
021200*                                 MFS ATTRIBUTFÄLT                        
021300     03 MOD-FLBYTES-IN       PIC X(2).                                    
021400*                                 MFS BEHANDLING AV INPUTFÄLT             
021500     03 MOD-FLGAMART-ATTR    PIC X(2).                                    
021600*                                 MFS ATTRIBUTFÄLT                        
021700     03 MOD-FLGAMART-IN      PIC X(2).                                    
021800*                                 MFS BEHANDLING AV INPUTFÄLT             
021900     03 MOD-TEMFSINF         PIC X(55).                                   
022000*                                 INFORMATIONSMEDDELANDE                  
022100*** END OF VILMAII-COPY LENGTH= 637 BYTES                                 
