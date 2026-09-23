000100 01  MOD-W4O38401.                                                        
000200*                                 COPYTEXT FOR MOD W4O38401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRCGRP-IN      PIC X(5).                                    
000800*                                 PRODUKTIONSKANALSGRUPP                  
000900     03 MOD-KDPRODKL-IN      PIC X.                                       
001000*                                 PRODUKTIONSKLASS                        
001100     03 MOD-IDPRC-IN.                                                     
001200*                                 PRODUKTIONSKANAL                        
001300        05 MOD-IDPRCBAS      PIC X(3).                                    
001400*                                 PRC-BAS                                 
001500        05 MOD-IDPRCVAR      PIC X.                                       
001600*                                 PRC-VARIANT                             
001700     03 MOD-IDDC-IN          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-KDPRCGRP-UT      PIC X(5).                                    
002000*                                 PRODUKTIONSKANALSGRUPP                  
002100     03 MOD-KDPRODKL-UT      PIC X.                                       
002200*                                 PRODUKTIONSKLASS                        
002300     03 MOD-IDPRC-UT.                                                     
002400*                                 PRODUKTIONSKANAL                        
002500        05 MOD-IDPRCBAS      PIC X(3).                                    
002600*                                 PRC-BAS                                 
002700        05 MOD-IDPRCVAR      PIC X.                                       
002800*                                 PRC-VARIANT                             
002900     03 MOD-IDDC-UT          PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MOD-KDPRCGRP-PFE     PIC X(5).                                    
003200*                                 PRODUKTIONSKANALSGRUPP                  
003300     03 MOD-KDPRODKL-PFE     PIC X.                                       
003400*                                 PRODUKTIONSKLASS                        
003500     03 MOD-IDPRC-PFE.                                                    
003600*                                 PRODUKTIONSKANAL                        
003700        05 MOD-IDPRCBAS      PIC X(3).                                    
003800*                                 PRC-BAS                                 
003900        05 MOD-IDPRCVAR      PIC X.                                       
004000*                                 PRC-VARIANT                             
004100     03 MOD-KDPRCGRP-PF8     PIC X(5).                                    
004200*                                 PRODUKTIONSKANALSGRUPP                  
004300     03 MOD-KDPRODKL-PF8     PIC X.                                       
004400*                                 PRODUKTIONSKLASS                        
004500     03 MOD-IDPRC-PF8.                                                    
004600*                                 PRODUKTIONSKANAL                        
004700        05 MOD-IDPRCBAS      PIC X(3).                                    
004800*                                 PRC-BAS                                 
004900        05 MOD-IDPRCVAR      PIC X.                                       
005000*                                 PRC-VARIANT                             
005100     03 MOD-RAD              OCCURS 13 TIMES.                             
005200*                                 TABLE-LINES                             
005300        05 MOD-IDPRC-RAD-ATTR                                             
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-IDPRC-RAD.                                                 
005700*                                 PRODUKTIONSKANAL                        
005800           07 MOD-IDPRCBAS   PIC X(3).                                    
005900*                                 PRC-BAS                                 
006000           07 MOD-IDPRCVAR   PIC X.                                       
006100*                                 PRC-VARIANT                             
006200        05 MOD-BEPRC-RAD-ATTR                                             
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-BEPRC-RAD     PIC X(15).                                   
006600*                                 PRODUKTIONKANALSNAMN                    
006700        05 MOD-KDPRODKL-RAD-ATTR                                          
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-KDPRODKL-RAD  PIC X.                                       
007100*                                 PRODUKTIONSKLASS                        
007200        05 MOD-KVARBTID-RAD-ATTR                                          
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-KVARBTID-RAD  PIC Z9.9.                                    
007600*                                 ANTAL MANTIMMAR                         
007700        05 MOD-KVBEMAN-ORD-RAD-ATTR                                       
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-KVBEMAN-ORD-RAD                                            
008100                             PIC Z9.9.                                    
008200*                                 BEMANNING, KAPACITET ORDINARIE          
008300        05 MOD-KVBEMAN-EXT-RAD-ATTR                                       
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KVBEMAN-EXT-RAD                                            
008700                             PIC Z9.9.                                    
008800*                                 BEMANNING, KAPACITET (EXTRA)            
008900     03 MOD-INPUT.                                                        
009000*                                 INDATA FOR INSERTION                    
009100        05 MOD-IDPRC-ATTR    PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-IDPRC.                                                     
009400*                                 PRODUKTIONSKANAL                        
009500           07 MOD-IDPRCBAS   PIC X(3).                                    
009600*                                 PRC-BAS                                 
009700           07 MOD-IDPRCVAR   PIC X.                                       
009800*                                 PRC-VARIANT                             
009900        05 MOD-KVARBTID-ATTR PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-KVARBTID      PIC X(4).                                    
010200*                                 ANTAL MANTIMMAR                         
010300        05 MOD-KVBEMAN-ORD-ATTR                                           
010400                             PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600        05 MOD-KVBEMAN-ORD   PIC X(4).                                    
010700*                                 BEMANNING, KAPACITET ORDINARIE          
010800        05 MOD-KVBEMAN-EXT-ATTR                                           
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-KVBEMAN-EXT   PIC X(4).                                    
011200*                                 BEMANNING, KAPACITET (EXTRA)            
011300     03 MOD-TEMFSINF         PIC X(55).                                   
011400*                                 INFORMATIONSMEDDELANDE                  
