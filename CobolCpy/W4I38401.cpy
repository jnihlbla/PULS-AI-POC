000100 01  MID-W4I38401.                                                        
000200*                                 COPYTEXT FOR MID W4I38401               
000300     03 MID-KDPRCGRP-IN      PIC X(5).                                    
000400*                                 PRODUKTIONSKANALSGRUPP                  
000500     03 MID-KDPRCGRP-UT      PIC X(5).                                    
000600*                                 PRODUKTIONSKANALSGRUPP                  
000700     03 MID-KDPRODKL-IN      PIC X.                                       
000800*                                 PRODUKTIONSKLASS                        
000900     03 MID-KDPRODKL-UT      PIC X.                                       
001000*                                 PRODUKTIONSKLASS                        
001100     03 MID-IDPRC-IN.                                                     
001200*                                 PRODUKTIONSKANAL                        
001300        05 MID-IDPRCBAS      PIC X(3).                                    
001400*                                 PRC-BAS                                 
001500        05 MID-IDPRCVAR      PIC X.                                       
001600*                                 PRC-VARIANT                             
001700     03 MID-IDPRC-UT.                                                     
001800*                                 PRODUKTIONSKANAL                        
001900        05 MID-IDPRCBAS      PIC X(3).                                    
002000*                                 PRC-BAS                                 
002100        05 MID-IDPRCVAR      PIC X.                                       
002200*                                 PRC-VARIANT                             
002300     03 MID-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MID-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MID-KDPRCGRP-PFE     PIC X(5).                                    
002800*                                 PRODUKTIONSKANALSGRUPP                  
002900     03 MID-KDPRODKL-PFE     PIC X.                                       
003000*                                 PRODUKTIONSKLASS                        
003100     03 MID-IDPRC-PFE.                                                    
003200*                                 PRODUKTIONSKANAL                        
003300        05 MID-IDPRCBAS      PIC X(3).                                    
003400*                                 PRC-BAS                                 
003500        05 MID-IDPRCVAR      PIC X.                                       
003600*                                 PRC-VARIANT                             
003700     03 MID-KDPRCGRP-PF8     PIC X(5).                                    
003800*                                 PRODUKTIONSKANALSGRUPP                  
003900     03 MID-KDPRODKL-PF8     PIC X.                                       
004000*                                 PRODUKTIONSKLASS                        
004100     03 MID-IDPRC-PF8.                                                    
004200*                                 PRODUKTIONSKANAL                        
004300        05 MID-IDPRCBAS      PIC X(3).                                    
004400*                                 PRC-BAS                                 
004500        05 MID-IDPRCVAR      PIC X.                                       
004600*                                 PRC-VARIANT                             
004700     03 MID-IDPRC-RAD        OCCURS 13 TIMES.                             
004800*                                 PRODUKTIONSKANAL                        
004900        05 MID-IDPRCBAS      PIC X(3).                                    
005000*                                 PRC-BAS                                 
005100        05 MID-IDPRCVAR      PIC X.                                       
005200*                                 PRC-VARIANT                             
005300     03 MID-INPUT.                                                        
005400*                                 INDATA FOR UPPDATERING                  
005500        05 MID-IDPRC.                                                     
005600*                                 PRODUKTIONSKANAL                        
005700           07 MID-IDPRCBAS   PIC X(3).                                    
005800*                                 PRC-BAS                                 
005900           07 MID-IDPRCVAR   PIC X.                                       
006000*                                 PRC-VARIANT                             
006100        05 MID-KVARBTID      PIC X(4).                                    
006200*                                 ANTAL MANTIMMAR                         
006300        05 MID-KVBEMAN-ORD   PIC X(4).                                    
006400*                                 BEMANNING, KAPACITET ORDINARIE          
006500        05 MID-KVBEMAN-EXT   PIC X(4).                                    
006600*                                 BEMANNING, KAPACITET (EXTRA)            
