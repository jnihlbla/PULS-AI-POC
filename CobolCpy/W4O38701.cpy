000100 01  MOD-W4O38701.                                                        
000200*                                 COPYTEXT FOR MOD W4O38701               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-PFI-KDPRCGRP     PIC X(5).                                    
000800*                                 PRODUKTIONSKANALSGRUPP                  
000900     03 MOD-PFI-KDPRODKL-01  PIC X.                                       
001000*                                 PRODUKTIONSKLASS                        
001100     03 MOD-PFI-KDPRODKL-02  PIC X.                                       
001200*                                 PRODUKTIONSKLASS                        
001300     03 MOD-PFI-IDPRC.                                                    
001400*                                 PRODUKTIONSKANAL                        
001500        05 MOD-IDPRCBAS      PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 MOD-IDPRCVAR      PIC X.                                       
001800*                                 PRC-VARIANT                             
001900     03 MOD-PFI-TIRFS-01     PIC 9(6).                                    
002000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002100     03 MOD-PFI-TIRFS-02     PIC 9(6).                                    
002200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002300     03 MOD-PFI-KDKALK       PIC X.                                       
002400*                                 EJ UPPDATERAD BESTÄLLNINGSREST          
002500     03 MOD-PFI-IDDC         PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-PF7-KDPRCGRP     PIC X(5).                                    
002800*                                 PRODUKTIONSKANALSGRUPP                  
002900     03 MOD-PF7-KDPRODKL-01  PIC X.                                       
003000*                                 PRODUKTIONSKLASS                        
003100     03 MOD-PF7-KDPRODKL-02  PIC X.                                       
003200*                                 PRODUKTIONSKLASS                        
003300     03 MOD-PF7-IDPRC.                                                    
003400*                                 PRODUKTIONSKANAL                        
003500        05 MOD-IDPRCBAS      PIC X(3).                                    
003600*                                 PRC-BAS                                 
003700        05 MOD-IDPRCVAR      PIC X.                                       
003800*                                 PRC-VARIANT                             
003900     03 MOD-PF7-TIRFS-01     PIC 9(6).                                    
004000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004100     03 MOD-PF7-TIRFS-02     PIC 9(6).                                    
004200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004300     03 MOD-PF7-KDKALK       PIC X.                                       
004400*                                 EJ UPPDATERAD BESTÄLLNINGSREST          
004500     03 MOD-PF7-IDDC         PIC X(2).                                    
004600*                                 IDENTIFIERARE LAGER                     
004700     03 MOD-PFE-IDPRC.                                                    
004800*                                 PRODUKTIONSKANAL                        
004900        05 MOD-IDPRCBAS      PIC X(3).                                    
005000*                                 PRC-BAS                                 
005100        05 MOD-IDPRCVAR      PIC X.                                       
005200*                                 PRC-VARIANT                             
005300     03 MOD-PF8-IDPRC.                                                    
005400*                                 PRODUKTIONSKANAL                        
005500        05 MOD-IDPRCBAS      PIC X(3).                                    
005600*                                 PRC-BAS                                 
005700        05 MOD-IDPRCVAR      PIC X.                                       
005800*                                 PRC-VARIANT                             
005900     03 MOD-RAD              OCCURS 12 TIMES.                             
006000*                                 TABLE-LINES                             
006100        05 MOD-IDPRC-RAD-ATTR                                             
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-IDPRC-RAD.                                                 
006500*                                 PRODUKTIONSKANAL                        
006600           07 MOD-IDPRCBAS   PIC X(3).                                    
006700*                                 PRC-BAS                                 
006800           07 MOD-IDPRCVAR   PIC X.                                       
006900*                                 PRC-VARIANT                             
007000        05 MOD-KVRADER-RAD-ATTR                                           
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-KVRADER-RAD   PIC Z(4)9.                                   
007400*                                 ANTAL RADER                             
007500        05 MOD-KVRADER-DAG-RAD-ATTR                                       
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KVRADER-DAG-RAD                                            
007900                             PIC Z(4)9.                                   
008000*                                 ANTAL RADER                             
008100        05 MOD-KVRADER-RST-RAD-ATTR                                       
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-KVRADER-RST-RAD                                            
008500                             PIC Z(4)9.                                   
008600*                                 ANTAL RADER                             
008700        05 MOD-KVRADER-REG-RAD-ATTR                                       
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-KVRADER-REG-RAD                                            
009100                             PIC Z(4)9.                                   
009200*                                 ANTAL RADER                             
009300        05 MOD-KVRADER-UT-RAD-ATTR                                        
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-KVRADER-UT-RAD                                             
009700                             PIC Z(4)9.                                   
009800*                                 ANTAL RADER                             
009900        05 MOD-KVRADER-KAN-RAD-ATTR                                       
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-KVRADER-KAN-RAD                                            
010300                             PIC Z(4)9.                                   
010400*                                 ANTAL RADER                             
010500        05 MOD-KVRADER-PLMI-ATTR                                          
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-KVRADER-PLMI-RAD                                           
010900                             PIC -(4)9.9.                                 
011000*                                 BEMANNING, KAPACITET                    
011100        05 MOD-KVRADER-META-RAD-ATTR                                      
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-KVRADER-META-RAD                                           
011500                             PIC Z(6).                                    
011600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
011700        05 MOD-KVRADER-METB-RAD-ATTR                                      
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-KVRADER-METB-RAD                                           
012100                             PIC Z(6).                                    
012200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
012300        05 MOD-KVRADER-METC-RAD-ATTR                                      
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-KVRADER-METC-RAD                                           
012700                             PIC Z(6).                                    
012800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
012900     03 MOD-TOTAL.                                                        
013000*                                 TABLE-LINES                             
013100        05 MOD-KVRADER-TOT-ATTR                                           
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400        05 MOD-KVRADER-TOT   PIC Z(4)9.                                   
013500*                                 ANTAL RADER                             
013600        05 MOD-KVRADER-DAG-TOT-ATTR                                       
013700                             PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900        05 MOD-KVRADER-DAG-TOT                                            
014000                             PIC Z(4)9.                                   
014100*                                 ANTAL RADER                             
014200        05 MOD-KVRADER-RST-TOT-ATTR                                       
014300                             PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500        05 MOD-KVRADER-RST-TOT                                            
014600                             PIC Z(4)9.                                   
014700*                                 ANTAL RADER                             
014800        05 MOD-KVRADER-REG-TOT-ATTR                                       
014900                             PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100        05 MOD-KVRADER-REG-TOT                                            
015200                             PIC Z(4)9.                                   
015300*                                 ANTAL RADER                             
015400        05 MOD-KVRADER-UT-TOT-ATTR                                        
015500                             PIC X(2).                                    
015600*                                 MFS ATTRIBUTFÄLT                        
015700        05 MOD-KVRADER-UT-TOT                                             
015800                             PIC Z(4)9.                                   
015900*                                 ANTAL RADER                             
016000        05 MOD-KVRADER-KAN-TOT-ATTR                                       
016100                             PIC X(2).                                    
016200*                                 MFS ATTRIBUTFÄLT                        
016300        05 MOD-KVRADER-KAN-TOT                                            
016400                             PIC Z(4)9.                                   
016500*                                 ANTAL RADER                             
016600        05 MOD-KVRADER-PLMI-ATTR                                          
016700                             PIC X(2).                                    
016800*                                 MFS ATTRIBUTFÄLT                        
016900        05 MOD-KVRADER-PLMI-TOT                                           
017000                             PIC -(4)9.9.                                 
017100*                                 BEMANNING, KAPACITET                    
017200        05 MOD-KVRADER-META-TOT-ATTR                                      
017300                             PIC X(2).                                    
017400*                                 MFS ATTRIBUTFÄLT                        
017500        05 MOD-KVRADER-META-TOT                                           
017600                             PIC Z(6).                                    
017700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
017800        05 MOD-KVRADER-METB-TOT-ATTR                                      
017900                             PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100        05 MOD-KVRADER-METB-TOT                                           
018200                             PIC Z(6).                                    
018300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
018400        05 MOD-KVRADER-METC-TOT-ATTR                                      
018500                             PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700        05 MOD-KVRADER-METC-TOT                                           
018800                             PIC Z(6).                                    
018900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
019000     03 MOD-TEMFSINF         PIC X(55).                                   
019100*                                 INFORMATIONSMEDDELANDE                  
