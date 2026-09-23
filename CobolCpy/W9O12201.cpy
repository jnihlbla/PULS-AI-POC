000100 01  MOD-W9O12201.                                                        
000200*                                 MOD-COPYTEXT PGM W90122                 
000300*                                 PROJECT REGISTRATION                    
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPROJ-IN        PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDPROJ-UT        PIC X(4).                                    
001100*                                 PARTS PROJEKTIDENTITET                  
001200     03 MOD-KDBASLM-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-KDBASLM-UT       PIC X(6).                                    
001500*                                 BASLAGERMARKNAD                         
001600     03 MOD-IDSKYLT-IN       PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001900*                                 NATIONALITETSTECKEN                     
002000     03 MOD-IDARTNR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDARTNR-UT       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-STRECK-UT        PIC X.                                       
002500     03 MOD-REKSIFFR-UT      PIC 9.                                       
002600*                                 KONTROLLSIFFRA                          
002700     03 MOD-KDPRODSL-UT      PIC X(2).                                    
002800*                                 PRODUKTSLAG                             
002900     03 MOD-KDBPSR-MIN       PIC X(2).                                    
003000*                                 MFS BEHANDLING AV INPUTFÄLT             
003100     03 MOD-KDBPSR-MAX       PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300     03 MOD-IDFKNGRP-MIN     PIC 9(4).                                    
003400*                                 FUNKTIONSGRUPP                          
003500     03 MOD-CURSOR.                                                       
003600*                                 CURSOR PLACERING                        
003700        05 MOD-CURSOR-RAD    PIC S9(4)           COMP.                    
003800*                                 CURSORPLACERING RAD                     
003900        05 MOD-CURSOR-KOL    PIC S9(4)           COMP.                    
004000*                                 CURSORPLACERING KOLUMN                  
004100     03 MOD-KDPRODSL         PIC Z9.                                      
004200*                                 PRODUKTSLAG                             
004300     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
004400*                                 FUNKTIONSGRUPP                          
004500     03 MOD-KDERS            PIC 9(2).                                    
004600*                                 ERSÄTTNINGSKOD                          
004700     03 MOD-TIBASLM          PIC 9(6).                                    
004800*                                 MARKNADSUPPDATERINGSDATUM               
004900     03 MOD-TEXT-BEART       PIC X(25).                                   
005000*                                 ARTIKELBENÄMNING                        
005100     03 MOD-KDBPSR           PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300     03 MOD-KDDEALER-UT-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-KDDEALER-UT      PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700     03 MOD-KDDEALER-IN-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-KDDEALER-IN      PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100     03 MOD-TEARTNOT         PIC X(40).                                   
006200*                                 ARTIKEL NOTERING                        
006300     03 MOD-TEARTNOT-BASL    PIC X(40).                                   
006400*                                 ARTIKEL NOTERING                        
006500     03 MOD-KVARTVAGN        PIC Z(3).                                    
006600*                                 ANTAL ARTIKLAR PER VAGN                 
006700     03 MOD-ERS-IDARTNR      PIC Z(8)9.                                   
006800*                                 ARTIKELNUMMER                           
006900     03 MOD-STRECK-ERS       PIC X.                                       
007000     03 MOD-REKSIFFR-ERS     PIC 9.                                       
007100*                                 KONTROLLSIFFRA                          
007200     03 MOD-IDARTNR-MOTSV    PIC Z(8)9.                                   
007300*                                 ARTIKELNUMMER                           
007400     03 MOD-STRECK-MOTSV     PIC X.                                       
007500     03 MOD-REKSIFFR-MOTSV   PIC 9.                                       
007600*                                 KONTROLLSIFFRA                          
007700     03 MOD-PRARTBTO-EXP-DEL PIC Z(6)9.9(2).                              
007800*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
007900     03 MOD-PRARTBTO-EXP-CP  PIC Z(6)9.9(2).                              
008000*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
008100     03 MOD-KVBASLM-UT-ATTR  PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-KVBASLM-UT       PIC Z(6)9.                                   
008400*                                 BASLAGER TOTAL PER MARKNAD              
008500     03 MOD-KVBASLM-IN-ATTR  PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-KVBASLM-IN       PIC X(2).                                    
008800*                                 MFS BEHANDLING AV INPUTFÄLT             
008900     03 MOD-RAD              OCCURS 6 TIMES.                              
009000        05 MOD-IDDISTR       PIC 9(4).                                    
009100*                                 DISTRIKTNUMMER                          
009200        05 MOD-REBLFORD      PIC 9(2).                                    
009300*                                 PROCENTFÖRDELNING PER DISTRIKT          
009400        05 MOD-KVBASLMD-UT-ATTR                                           
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-KVBASLMD-UT   PIC 9(7).                                    
009800*                                 BASLAGER DISTRIKT KVANTITET             
009900        05 MOD-KVBASLMD-IN-ATTR                                           
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-KVBASLMD-IN   PIC X(2).                                    
010300*                                 MFS BEHANDLING AV INPUTFÄLT             
010400     03 MOD-KVBLKIT          PIC Z(2)9.                                   
010500*                                 ÅTERFÖRSÄLJARSATSER PER MARKNAD         
010600     03 MOD-KVBASLKIT-UT-ATTR                                             
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-KVBASLKIT-UT     PIC Z(6)9.                                   
011000*                                 ANTAL I ÅTERFÖLSÄLJARSATS               
011100     03 MOD-KVBASLKIT-IN-ATTR                                             
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400     03 MOD-KVBASLKIT-IN     PIC X(2).                                    
011500*                                 MFS BEHANDLING AV INPUTFÄLT             
011600     03 MOD-TEARTNOT-MARK-ATTR                                            
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-TEARTNOT-MARK    PIC X(40).                                   
012000*                                 ARTIKEL NOTERING                        
012100     03 MOD-TEMFSINF         PIC X(55).                                   
012200*                                 INFORMATIONSMEDDELANDE                  
012300*** END COPY W9O12201    LENGTH=504                                       
