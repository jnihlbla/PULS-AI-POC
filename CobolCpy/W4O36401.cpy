000100 01  MOD-W4O36401.                                                        
000200*                                 COPYTEXT FÖR MOD W4O36401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPRC-IN.                                                     
000800*                                 PRODUKTIONSKANAL                        
000900        05 MOD-IDPRCBAS      PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 MOD-IDPRCVAR      PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 MOD-IDPRC-UT.                                                     
001400*                                 PRODUKTIONSKANAL                        
001500        05 MOD-IDPRCBAS      PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 MOD-IDPRCVAR      PIC X.                                       
001800*                                 PRC-VARIANT                             
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-FLAGGA-BORTTAG   PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MOD-FLAGGA-SPAR      PIC X.                                       
002600*                                 ALLMÄN FLAGGA                           
002700     03 MOD-IDPRC-UPDATE-ATTR                                             
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDPRC-UPDATE     PIC X(2).                                    
003100*                                 MFS BEHANDLING AV INPUTFÄLT             
003200     03 MOD-BEPRC-ATTR       PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-BEPRC-RAD        PIC X(15).                                   
003500*                                 PRODUKTIONKANALSNAMN                    
003600     03 MOD-KDPRCGRP-ATTR    PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDPRCGRP-RAD     PIC X(5).                                    
003900*                                 PRODUKTIONSKANALSGRUPP                  
004000     03 MOD-KDPRCGRP-UPDATE-ATTR                                          
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-KDPRCGRP-UPDATE  PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500     03 MOD-KDPRODKL-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KDPRODKL-RAD     PIC X.                                       
004800*                                 PRODUKTIONSKLASS                        
004900     03 MOD-KDPRCTYP-ATTR    PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-KDPRCTYP-RAD     PIC X.                                       
005200*                                 PRODUKTIONSKANALSTYP                    
005300     03 MOD-IDPRC-HUV-ATTR   PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-IDPRC-HUV-RAD.                                                
005600*                                 HUVUDPRODUKTIONSKANAL                   
005700        05 MOD-IDPRCBAS-HUV  PIC X(3).                                    
005800*                                 PRC-BAS                                 
005900        05 MOD-IDPRCVAR-HUV  PIC X.                                       
006000*                                 PRC-VARIANT                             
006100     03 MOD-VLKOLGR-ATTR     PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-VLKOLGR-RAD      PIC 9.9(2).                                  
006400*                                 NETTOGRÄNS FÖR EGET KOLLI I M3          
006500     03 MOD-KDPRODKL-UPDATE-ATTR                                          
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KDPRODKL-UPDATE  PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000     03 MOD-KDPRCTYP-UPDATE-ATTR                                          
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-KDPRCTYP-UPDATE  PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500     03 MOD-IDPRC-HUV-UPDATE-ATTR                                         
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-IDPRC-HUV-UPDATE PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000     03 MOD-VLKOLGR-UPDATE-ATTR                                           
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-VLKOLGR-UPDATE   PIC X(2).                                    
008400*                                 MFS BEHANDLING AV INPUTFÄLT             
008500     03 MOD-RAD              OCCURS 10 TIMES.                             
008600*                                 TABELL-RADER                            
008700        05 MOD-IDPRC-SUB-RAD-ATTR                                         
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-IDPRC-SUB-RAD.                                             
009100*                                 PICKUP PRODUKTIONSKANAL                 
009200           07 MOD-IDPRCBAS-SUB                                            
009300                             PIC X(3).                                    
009400*                                 PRC-BAS                                 
009500           07 MOD-IDPRCVAR-SUB                                            
009600                             PIC X.                                       
009700*                                 PRC-VARIANT                             
009800     03 MOD-UPDATE           OCCURS 10 TIMES.                             
009900*                                 TABELL-UPDATE                           
010000        05 MOD-IDPRC-SUB-UPDATE-ATTR                                      
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300        05 MOD-IDPRC-SUB-UPDATE.                                          
010400*                                 PRODUKTIONSKANAL                        
010500           07 MOD-IDPRCBAS   PIC X(3).                                    
010600*                                 PRC-BAS                                 
010700           07 MOD-IDPRCVAR   PIC X.                                       
010800*                                 PRC-VARIANT                             
010900     03 MOD-KVVTID-ATTR      PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100     03 MOD-KVVTID-RAD       PIC X(5).                                    
011200*                                 ORDER VÄNTETID I PRC (TTMM)             
011300     03 MOD-RESPLIT-ATTR     PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-RESPLIT-RAD      PIC 9.9(2).                                  
011600*                                 FAKTOR FÖR PLOCKSATSSTORLEK             
011700     03 MOD-FLSTJORD-UT      PIC X.                                       
011800*                                 FLAGGA FÖR STJÄRNORDER PBV              
011900     03 MOD-KVVTID-UPDATE-ATTR                                            
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200     03 MOD-KVVTID-UPDATE    PIC X(2).                                    
012300*                                 MFS BEHANDLING AV INPUTFÄLT             
012400     03 MOD-RESPLIT-UPDATE-ATTR                                           
012500                             PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-RESPLIT-UPDATE   PIC X(2).                                    
012800*                                 MFS BEHANDLING AV INPUTFÄLT             
012900     03 MOD-FLSTJORD-UPDATE-ATTR                                          
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-FLSTJORD-UPDATE  PIC X.                                       
013300*                                 FLAGGA FÖR STJÄRNORDER PBV              
013400     03 MOD-FLAGGA-UT-ATTR   PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600     03 MOD-FLAGGA-UT        PIC X.                                       
013700*                                 ALLMÄN FLAGGA                           
013800     03 MOD-RAD              OCCURS 10 TIMES.                             
013900*                                 TABELL-RADER                            
014000        05 MOD-ADLAGOMR-RAD-ATTR                                          
014100                             PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300        05 MOD-ADLAGOMR-RAD  PIC Z9.                                      
014400*                                 LAGEROMRÅDE                             
014500     03 MOD-UPDATE           OCCURS 10 TIMES.                             
014600*                                 TABELL-UPDATE                           
014700        05 MOD-ADLAGOMR-UPDATE-ATTR                                       
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000        05 MOD-ADLAGOMR-UPDATE                                            
015100                             PIC X(2).                                    
015200*                                 MFS BEHANDLING AV INPUTFÄLT             
015300     03 MOD-KVORDER-ATTR     PIC X(2).                                    
015400*                                 MFS ATTRIBUTFÄLT                        
015500     03 MOD-KVORDER-RAD      PIC Z(6)9.                                   
015600*                                 ANTAL ORDER                             
015700     03 MOD-KVRADER-ATTR     PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900     03 MOD-KVRADER-RAD      PIC Z(4)9.                                   
016000*                                 ANTAL RADER                             
016100     03 MOD-VKORDNTO-ATTR    PIC X(2).                                    
016200*                                 MFS ATTRIBUTFÄLT                        
016300     03 MOD-VKORDNTO-RAD     PIC Z(5)9.9.                                 
016400*                                 ORDERVIKT NETTO (KG)                    
016500     03 MOD-VLORDNTO-ATTR    PIC X(2).                                    
016600*                                 MFS ATTRIBUTFÄLT                        
016700     03 MOD-VLORDNTO-RAD     PIC Z(3)9.9(3).                              
016800*                                 ORDERVOLYM NETTO (M3)                   
016900     03 MOD-KVORDER-UPDATE-ATTR                                           
017000                             PIC X(2).                                    
017100*                                 MFS ATTRIBUTFÄLT                        
017200     03 MOD-KVORDER-UPDATE   PIC X(2).                                    
017300*                                 MFS BEHANDLING AV INPUTFÄLT             
017400     03 MOD-KVRADER-UPDATE-ATTR                                           
017500                             PIC X(2).                                    
017600*                                 MFS ATTRIBUTFÄLT                        
017700     03 MOD-KVRADER-UPDATE   PIC X(2).                                    
017800*                                 MFS BEHANDLING AV INPUTFÄLT             
017900     03 MOD-VKORDNTO-UPDATE-ATTR                                          
018000                             PIC X(2).                                    
018100*                                 MFS ATTRIBUTFÄLT                        
018200     03 MOD-VKORDNTO-UPDATE  PIC X(2).                                    
018300*                                 MFS BEHANDLING AV INPUTFÄLT             
018400     03 MOD-VLORDNTO-UPDATE-ATTR                                          
018500                             PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700     03 MOD-VLORDNTO-UPDATE  PIC X(2).                                    
018800*                                 MFS BEHANDLING AV INPUTFÄLT             
018900     03 MOD-KVPLSRAD-ATTR    PIC X(2).                                    
019000*                                 MFS ATTRIBUTFÄLT                        
019100     03 MOD-KVPLSRAD-RAD     PIC Z(4)9.                                   
019200*                                 ANTAL RADER                             
019300     03 MOD-VKPLSNTO-ATTR    PIC X(2).                                    
019400*                                 MFS ATTRIBUTFÄLT                        
019500     03 MOD-VKPLSNTO-RAD     PIC Z(5)9.9.                                 
019600*                                 ORDERVIKT NETTO (KG)                    
019700     03 MOD-VLPLSNTO-ATTR    PIC X(2).                                    
019800*                                 MFS ATTRIBUTFÄLT                        
019900     03 MOD-VLPLSNTO-RAD     PIC Z(3)9.9(3).                              
020000*                                 ORDERVOLYM NETTO (M3)                   
020100     03 MOD-KVPLSRAD-UPDATE-ATTR                                          
020200                             PIC X(2).                                    
020300*                                 MFS ATTRIBUTFÄLT                        
020400     03 MOD-KVPLSRAD-UPDATE  PIC X(2).                                    
020500*                                 MFS BEHANDLING AV INPUTFÄLT             
020600     03 MOD-VKPLSNTO-UPDATE-ATTR                                          
020700                             PIC X(2).                                    
020800*                                 MFS ATTRIBUTFÄLT                        
020900     03 MOD-VKPLSNTO-UPDATE  PIC X(2).                                    
021000*                                 MFS BEHANDLING AV INPUTFÄLT             
021100     03 MOD-VLPLSNTO-UPDATE-ATTR                                          
021200                             PIC X(2).                                    
021300*                                 MFS ATTRIBUTFÄLT                        
021400     03 MOD-VLPLSNTO-UPDATE  PIC X(2).                                    
021500*                                 MFS BEHANDLING AV INPUTFÄLT             
021600     03 MOD-TEMFSINF         PIC X(55).                                   
021700*                                 INFORMATIONSMEDDELANDE                  
021800*** END OF VILMAII-COPY LENGTH= 498 BYTES                                 
