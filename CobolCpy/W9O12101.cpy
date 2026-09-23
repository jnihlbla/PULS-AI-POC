000100 01  MOD-W9O12101.                                                        
000200*                                 MOD-COPYTEXT PGM W90121                 
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
002000     03 MOD-KDBPSR-1-IN      PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-KDBPSR-1-UT      PIC X.                                       
002300*                                 BASLAGERFÖRSLAGSNIVÅ                    
002400     03 MOD-KDBPSR-2-IN      PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-KDBPSR-2-UT      PIC X.                                       
002700*                                 BASLAGERFÖRSLAGSNIVÅ                    
002800     03 MOD-KDPRODSL-IN      PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-KDPRODSL-UT      PIC X(2).                                    
003100*                                 PRODUKTSLAG                             
003200     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
003300*                                 ARTIKELNUMMER                           
003400     03 MOD-IDARTNR-PF8      PIC 9(9).                                    
003500*                                 ARTIKELNUMMER                           
003600     03 MOD-IDFKNGRP-ENTER   PIC 9(4).                                    
003700*                                 FUNKTIONSGRUPP                          
003800     03 MOD-IDFKNGRP-PF8     PIC 9(4).                                    
003900*                                 FUNKTIONSGRUPP                          
004000     03 MOD-RAD              OCCURS 13 TIMES.                             
004100        05 MOD-KDSVAR-KVAR   PIC X.                                       
004200*                                 SVARSKOD : MER DATA KVAR                
004300        05 MOD-IDARTNR       PIC Z(8)9.                                   
004400*                                 ARTIKELNUMMER                           
004500        05 MOD-STRECK        PIC X.                                       
004600        05 MOD-REKSIFFR      PIC 9.                                       
004700*                                 KONTROLLSIFFRA                          
004800        05 MOD-BEART         PIC X(25).                                   
004900*                                 ARTIKELBENÄMNING                        
005000        05 FILLER            PIC X(2).                                    
005100        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
005200*                                 FUNKTIONSGRUPP                          
005300        05 FILLER            PIC X(5).                                    
005400        05 MOD-TISTOMREG     PIC 9(6).                                    
005500*                                 STOPPTID MARKNADSREGISTRERING           
005600        05 FILLER            PIC X(2).                                    
005700        05 MOD-KDPRODSL      PIC Z(2)9.                                   
005800*                                 PRODUKTSLAG                             
005900        05 FILLER            PIC X(3).                                    
006000        05 MOD-KDBPSR        PIC 9.                                       
006100*                                 BASLAGERFÖRSLAGSNIVÅ                    
006200        05 FILLER            PIC X(3).                                    
006300        05 MOD-FL-TEARTNOT-MARK                                           
006400                             PIC X.                                       
006500        05 FILLER            PIC X(5).                                    
006600        05 MOD-FLBLMQ        PIC X.                                       
006700*                                 ARTIKEL TIDIGARE PÅ BASLAGERKÖ?         
006800     03 MOD-TEMFSINF         PIC X(55).                                   
006900*                                 INFORMATIONSMEDDELANDE                  
007000*** END COPY W9O12101    LENGTH=1103                                      
