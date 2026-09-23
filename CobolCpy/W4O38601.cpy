000100 01  MOD-W4O38601.                                                        
000200*                                 COPYTEXT FOR MOD W4O38601               
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
007000        05 MOD-SUPTID-RAD-ATTR                                            
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-SUPTID-RAD    PIC Z(4)9.                                   
007400*                                 ANTAL RADER                             
007500        05 MOD-SUPTID-DAG-RAD-ATTR                                        
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-SUPTID-DAG-RAD                                             
007900                             PIC Z(4)9.                                   
008000*                                 ANTAL RADER                             
008100        05 MOD-SUPTID-RST-RAD-ATTR                                        
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-SUPTID-RST-RAD                                             
008500                             PIC Z(4)9.                                   
008600*                                 ANTAL RADER                             
008700        05 MOD-SUPTID-REG-RAD-ATTR                                        
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-SUPTID-REG-RAD                                             
009100                             PIC Z(4)9.                                   
009200*                                 ANTAL RADER                             
009300        05 MOD-SUPTID-UT-RAD-ATTR                                         
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-SUPTID-UT-RAD PIC Z(4)9.                                   
009700*                                 ANTAL RADER                             
009800        05 MOD-SUPTID-KAN-RAD-ATTR                                        
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-SUPTID-KAN-RAD                                             
010200                             PIC Z(4)9.                                   
010300*                                 ANTAL RADER                             
010400        05 MOD-SUPTID-PLMI-ATTR                                           
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-SUPTID-PLMI-RAD                                            
010800                             PIC -(4)9.9.                                 
010900*                                 BEMANNING, KAPACITET                    
011000        05 MOD-SUPTID-META-RAD-ATTR                                       
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 MOD-SUPTID-META-RAD                                            
011400                             PIC Z(2)9.9(2).                              
011500*                                 TOTAL PRODUKTIONSTID TIM+MIN            
011600        05 MOD-SUPTID-METB-RAD-ATTR                                       
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900        05 MOD-SUPTID-METB-RAD                                            
012000                             PIC Z(2)9.9(2).                              
012100*                                 TOTAL PRODUKTIONSTID TIM+MIN            
012200        05 MOD-SUPTID-METC-RAD-ATTR                                       
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500        05 MOD-SUPTID-METC-RAD                                            
012600                             PIC Z(2)9.9(2).                              
012700*                                 TOTAL PRODUKTIONSTID TIM+MIN            
012800     03 MOD-TOTAL.                                                        
012900*                                 TABLE-LINES                             
013000        05 MOD-SUPTID-TOT-ATTR                                            
013100                             PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300        05 MOD-SUPTID-TOT    PIC Z(4)9.                                   
013400*                                 ANTAL RADER                             
013500        05 MOD-SUPTID-DAG-TOT-ATTR                                        
013600                             PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800        05 MOD-SUPTID-DAG-TOT                                             
013900                             PIC Z(4)9.                                   
014000*                                 ANTAL RADER                             
014100        05 MOD-SUPTID-RST-TOT-ATTR                                        
014200                             PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400        05 MOD-SUPTID-RST-TOT                                             
014500                             PIC Z(4)9.                                   
014600*                                 ANTAL RADER                             
014700        05 MOD-SUPTID-REG-TOT-ATTR                                        
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000        05 MOD-SUPTID-REG-TOT                                             
015100                             PIC Z(4)9.                                   
015200*                                 ANTAL RADER                             
015300        05 MOD-SUPTID-UT-TOT-ATTR                                         
015400                             PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600        05 MOD-SUPTID-UT-TOT PIC Z(4)9.                                   
015700*                                 ANTAL RADER                             
015800        05 MOD-SUPTID-KAN-TOT-ATTR                                        
015900                             PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100        05 MOD-SUPTID-KAN-TOT                                             
016200                             PIC Z(4)9.                                   
016300*                                 ANTAL RADER                             
016400        05 MOD-SUPTID-PLMI-ATTR                                           
016500                             PIC X(2).                                    
016600*                                 MFS ATTRIBUTFÄLT                        
016700        05 MOD-SUPTID-PLMI-TOT                                            
016800                             PIC -(4)9.9.                                 
016900*                                 BEMANNING, KAPACITET                    
017000        05 MOD-SUPTID-META-TOT-ATTR                                       
017100                             PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300        05 MOD-SUPTID-META-TOT                                            
017400                             PIC Z(2)9.9(2).                              
017500*                                 TOTAL PRODUKTIONSTID TIM+MIN            
017600        05 MOD-SUPTID-METB-TOT-ATTR                                       
017700                             PIC X(2).                                    
017800*                                 MFS ATTRIBUTFÄLT                        
017900        05 MOD-SUPTID-METB-TOT                                            
018000                             PIC Z(2)9.9(2).                              
018100*                                 TOTAL PRODUKTIONSTID TIM+MIN            
018200        05 MOD-SUPTID-METC-TOT-ATTR                                       
018300                             PIC X(2).                                    
018400*                                 MFS ATTRIBUTFÄLT                        
018500        05 MOD-SUPTID-METC-TOT                                            
018600                             PIC Z(2)9.9(2).                              
018700*                                 TOTAL PRODUKTIONSTID TIM+MIN            
018800     03 MOD-TEMFSINF         PIC X(55).                                   
018900*                                 INFORMATIONSMEDDELANDE                  
