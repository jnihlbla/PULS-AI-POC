000100 01  MOD-W2O34801.                                                        
000200*                                 MOD-COPYTEXT FÖR W2034800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDARBTYP-ATTR    PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-KDARBTYP         PIC X(4).                                    
001000*                                 TYP AV ARBETE                           
001100     03 MOD-IDPERSON-ATTR    PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDPERSON         PIC X(3).                                    
001400*                                 PERSONKOD                               
001500     03 MOD-IDDC-ATTR        PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDDC             PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-FLKVROS-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-FLKVROS          PIC X(2).                                    
002200*                                 MFS BEHANDLING AV INPUTFÄLT             
002300     03 MOD-IDPERSON-BUY-FOM-ATTR                                         
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-IDPERSON-BUY-FOM PIC X(3).                                    
002700*                                 PERSONKOD REFILLANSVARIG                
002800     03 MOD-IDPERSON-BUY-TOM-ATTR                                         
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-IDPERSON-BUY-TOM PIC X(3).                                    
003200*                                 PERSONKOD REFILLANSVARIG                
003300     03 MOD-IDPERSON-BUY2-ATTR                                            
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-IDPERSON-BUY2    PIC X(3).                                    
003700*                                 PERSONKOD REFILLANSVARIG                
003800     03 MOD-IDPERSON-BUY3-ATTR                                            
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDPERSON-BUY3    PIC X(3).                                    
004200*                                 PERSONKOD REFILLANSVARIG                
004300     03 MOD-IDPERSON-BUY4-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDPERSON-BUY4    PIC X(3).                                    
004700*                                 PERSONKOD REFILLANSVARIG                
004800     03 MOD-FLONORDER-ATTR   PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-FLONORDER        PIC X(2).                                    
005100*                                 MFS BEHANDLING AV INPUTFÄLT             
005200     03 MOD-IDPROJ-GRP       OCCURS 3 TIMES.                              
005300        05 MOD-IDPROJ-ATTR   PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-IDPROJ        PIC X(4).                                    
005600*                                 PARTS PROJEKTIDENTITET                  
005700     03 MOD-FLAK-DC-ATTR     PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-FLAK-DC          PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100     03 MOD-IDLEVNR-CDC-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-IDLEVNR-CDC      PIC X(5).                                    
006400*                                 LEVERANTÖRNUMMER                        
006500     03 MOD-KDPSLLOC-ATTR    PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-KDPSLLOC         PIC X(2).                                    
006800*                                 PRODUKTSLAG LOKALT                      
006900     03 MOD-FLASEAS-ATTR     PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-FLASEAS          PIC X(2).                                    
007200*                                 MFS BEHANDLING AV INPUTFÄLT             
007300     03 MOD-IDLEVNR-DC-ATTR  PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-IDLEVNR-DC       PIC X(5).                                    
007600*                                 LEVERANTÖRNUMMER                        
007700     03 MOD-IDFKNGRP-FOM-ATTR                                             
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-IDFKNGRP-FOM     PIC X(4).                                    
008100*                                 FUNKTIONSGRUPP-FROM                     
008200     03 MOD-IDFKNGRP-TOM-ATTR                                             
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-IDFKNGRP-TOM     PIC X(4).                                    
008600*                                 FUNKTIONSGRUPP-TOM                      
008700     03 MOD-FLREFILL-ATTR    PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-FLREFILL         PIC X(2).                                    
009000*                                 MFS BEHANDLING AV INPUTFÄLT             
009100     03 MOD-PRISRAD-ATTR     PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 MOD-PRISRAD          PIC X(2).                                    
009400     03 MOD-PBRAD-ATTR       PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-PBRAD            PIC X(2).                                    
009700*                                 MFS BEHANDLING AV INPUTFÄLT             
009800     03 MOD-FLREFBEO-ATTR    PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-FLREFBEO         PIC X(2).                                    
010100*                                 MFS BEHANDLING AV INPUTFÄLT             
010200     03 MOD-IDREFTAB-ATTR    PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-IDREFTAB         PIC X(2).                                    
010500*                                 MFS BEHANDLING AV INPUTFÄLT             
010600     03 MOD-VKART-TKN-ATTR   PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-VKART-TKN        PIC X(2).                                    
010900*                                 MFS BEHANDLING AV INPUTFÄLT             
011000     03 MOD-VKART-ATTR       PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-VKART            PIC X(7).                                    
011300*                                 ARTIKELVIKT (G)                         
011400     03 MOD-KVPB-FOM-ATTR    PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-KVPB-FOM         PIC X(7).                                    
011700*                                 PERIODBEHOV FOM (PROGNOS)               
011800     03 MOD-KVPB-TOM-ATTR    PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-KVPB-TOM         PIC X(7).                                    
012100*                                 PERIODBEHOV TOM (PROGNOS)               
012200     03 MOD-VLARTNTO-TKN-ATTR                                             
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 MOD-VLARTNTO-TKN     PIC X(2).                                    
012600*                                 MFS BEHANDLING AV INPUTFÄLT             
012700     03 MOD-VLARTNTO-ATTR    PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900     03 MOD-VLARTNTO         PIC X(9).                                    
013000*                                 ARTIKELVOLYM NETTO (CM3)                
013100     03 MOD-ADLAGOMR-ATTR    PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300     03 MOD-ADLAGOMR         PIC X(2).                                    
013400*                                 LAGEROMRÅDE                             
013500     03 MOD-ADGANG-ATTR      PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 MOD-ADGANG           PIC X(2).                                    
013800*                                 GÅNG                                    
013900     03 MOD-ADPLATS-FOM-ATTR PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-ADPLATS-FOM      PIC X(5).                                    
014200*                                 LAGERPLATSNUMMER                        
014300     03 MOD-ADPLATS-TOM-ATTR PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500     03 MOD-ADPLATS-TOM      PIC X(5).                                    
014600*                                 LAGERPLATSNUMMER                        
014700     03 MOD-KDERS-ATTR       PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900     03 MOD-KDERS            PIC X(2).                                    
015000*                                 ERSÄTTNINGSKOD                          
015100     03 MOD-PRARTSTD-TKN-ATTR                                             
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400     03 MOD-PRARTSTD-TKN     PIC X(2).                                    
015500*                                 MFS BEHANDLING AV INPUTFÄLT             
015600     03 MOD-PRARTSTD-ATTR    PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800     03 MOD-PRARTSTD         PIC X(9).                                    
015900*                                 ARTIKELSTANDARDPRIS                     
016000     03 MOD-KVLS-TKN-ATTR    PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200     03 MOD-KVLS-TKN         PIC X(2).                                    
016300*                                 MFS BEHANDLING AV INPUTFÄLT             
016400     03 MOD-KVLS-ATTR        PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 MOD-KVLS             PIC X(7).                                    
016700*                                 LAGERSALDO                              
016800     03 MOD-TIFINLV-TKN-ATTR PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-TIFINLV-TKN      PIC X(2).                                    
017100*                                 MFS BEHANDLING AV INPUTFÄLT             
017200     03 MOD-TIFINLV-ATTR     PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400     03 MOD-TIFINLV          PIC X(5).                                    
017500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
017600     03 MOD-TIREFEFT-TKN-ATTR                                             
017700                             PIC X(2).                                    
017800*                                 MFS ATTRIBUTFÄLT                        
017900     03 MOD-TIREFEFT-TKN     PIC X(2).                                    
018000*                                 MFS BEHANDLING AV INPUTFÄLT             
018100     03 MOD-TIREFEFT-ATTR    PIC X(2).                                    
018200*                                 MFS ATTRIBUTFÄLT                        
018300     03 MOD-TIREFEFT         PIC X(6).                                    
018400*                                 DATUM SENAST EFTERFRÅGAD                
018500     03 MOD-BEART-ATTR       PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700     03 MOD-BEART            PIC X(25).                                   
018800*                                 ARTIKELBENÄMNING                        
018900     03 MOD-BEMODELL-ATTR    PIC X(2).                                    
019000*                                 MFS ATTRIBUTFÄLT                        
019100     03 MOD-BEMODELL         PIC X(15).                                   
019200*                                 BILENS MODELLBESKRIVNING.               
019300     03 MOD-KDREFSTA-ATTR    PIC X(2).                                    
019400*                                 MFS ATTRIBUTFÄLT                        
019500     03 MOD-KDREFSTA         PIC X.                                       
019600*                                 STATUS REFILLARTIKEL                    
019700     03 MOD-SUPERWEEK-TKN-ATTR                                            
019800                             PIC X(2).                                    
019900*                                 MFS ATTRIBUTFÄLT                        
020000     03 MOD-SUPERWEEK-TKN    PIC X(2).                                    
020100*                                 MFS BEHANDLING AV INPUTFÄLT             
020200     03 MOD-SUPERWEEK-ATTR   PIC X(2).                                    
020300*                                 MFS ATTRIBUTFÄLT                        
020400     03 MOD-SUPERWEEK        PIC Z(2)9.                                   
020500     03 MOD-FLFLYG-ATTR      PIC X(2).                                    
020600*                                 MFS ATTRIBUTFÄLT                        
020700     03 MOD-FLFLYG           PIC X.                                       
020800*                                 FLYGARTIKEL                             
020900     03 MOD-TEMFSINF         PIC X(55).                                   
021000*                                 INFORMATIONSMEDDELANDE                  
021100*** END OF VILMAII-COPY LENGTH= 405 BYTES                                 
