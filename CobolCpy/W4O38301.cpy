000100 01  MOD-W4O38301.                                                        
000200*                                 COPYTEXT FOR MOD W4O38301               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-PFI-IDPRC.                                                    
000800*                                 PRODUKTIONSKANAL                        
000900        05 MOD-IDPRCBAS      PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 MOD-IDPRCVAR      PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 MOD-PFI-TIRFS-AAMMDD PIC 9(6).                                    
001400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001500     03 MOD-PFI-TIRFS-HHMM   PIC 9(4).                                    
001600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
001700     03 MOD-PFI-IDTRP.                                                    
001800*                                 TRANSPORTIDENTITET                      
001900        05 MOD-IDTRPLOS      PIC X(3).                                    
002000*                                 TRANSPORTLÖSNING                        
002100        05 MOD-IDTRPVAR      PIC X(2).                                    
002200*                                 TRANSPORTLÖSNINGSGRUPP                  
002300     03 MOD-PFI-TITRPAVT-AAMMDD                                           
002400                             PIC 9(6).                                    
002500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002600     03 MOD-PFI-TITRPAVT-HHMM                                             
002700                             PIC 9(4).                                    
002800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002900     03 MOD-PFI-IDDC         PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MOD-PF7-IDPRC.                                                    
003200*                                 PRODUKTIONSKANAL                        
003300        05 MOD-IDPRCBAS      PIC X(3).                                    
003400*                                 PRC-BAS                                 
003500        05 MOD-IDPRCVAR      PIC X.                                       
003600*                                 PRC-VARIANT                             
003700     03 MOD-PF7-TIRFS-AAMMDD PIC 9(6).                                    
003800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003900     03 MOD-PF7-TIRFS-HHMM   PIC 9(4).                                    
004000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004100     03 MOD-PF7-IDTRP.                                                    
004200*                                 TRANSPORTIDENTITET                      
004300        05 MOD-IDTRPLOS      PIC X(3).                                    
004400*                                 TRANSPORTLÖSNING                        
004500        05 MOD-IDTRPVAR      PIC X(2).                                    
004600*                                 TRANSPORTLÖSNINGSGRUPP                  
004700     03 MOD-PF7-TITRPAVT-AAMMDD                                           
004800                             PIC 9(6).                                    
004900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005000     03 MOD-PF7-TITRPAVT-HHMM                                             
005100                             PIC 9(4).                                    
005200*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005300     03 MOD-PF7-IDDC         PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500     03 MOD-PF8-IDORDER      PIC 9(7).                                    
005600*                                 VOLVO PARTS ORDERNUMMER                 
005700     03 MOD-PF8-IDPRODNR     PIC 9(7).                                    
005800*                                 PRODUKTIONSNUMMER                       
005900     03 MOD-PF8-IDPLKLST     PIC 9(3).                                    
006000*                                 PLOCKLISTNUMMER                         
006100     03 MOD-PF8-TIRFS        PIC 9(10).                                   
006200*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
006300     03 MOD-PF8-IDPRCVAR     PIC X.                                       
006400*                                 PRC-VARIANT                             
006500     03 MOD-PFE-IDORDER      PIC 9(7).                                    
006600*                                 VOLVO PARTS ORDERNUMMER                 
006700     03 MOD-PFE-IDPRODNR     PIC 9(7).                                    
006800*                                 PRODUKTIONSNUMMER                       
006900     03 MOD-PFE-IDPLKLST     PIC 9(3).                                    
007000*                                 PLOCKLISTNUMMER                         
007100     03 MOD-PFE-TIRFS        PIC 9(10).                                   
007200*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
007300     03 MOD-PFE-IDPRCVAR     PIC X.                                       
007400*                                 PRC-VARIANT                             
007500     03 MOD-PFX-IDPRC.                                                    
007600*                                 PRODUKTIONSKANAL                        
007700        05 MOD-IDPRCBAS      PIC X(3).                                    
007800*                                 PRC-BAS                                 
007900        05 MOD-IDPRCVAR      PIC X.                                       
008000*                                 PRC-VARIANT                             
008100     03 MOD-RAD              OCCURS 14 TIMES.                             
008200*                                 TABLE-LINES                             
008300        05 MOD-TILST-OD-RAD-ATTR                                          
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-TILST-OD-RAD  PIC 9(6)B9(4).                               
008700*                                 SENASTE STARTTIDPUNKT ORDERDEL          
008800        05 MOD-SUPTID-RAD-ATTR                                            
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-SUPTID-RAD    PIC Z(2)9.9(2).                              
009200*                                 TOTAL PRODUKTIONSTID TIM+MIN            
009300        05 MOD-TIRFS-RAD-ATTR                                             
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-TIRFS-RAD     PIC 9(6)B9(4).                               
009700*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
009800        05 MOD-KVRADER-RAD-ATTR                                           
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-KVRADER-RAD   PIC Z(4)9.                                   
010200*                                 ANTAL RADER                             
010300        05 MOD-KVRADER-MEZ-RAD-ATTR                                       
010400                             PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600        05 MOD-KVRADER-MEZ-RAD                                            
010700                             PIC Z(4)9.                                   
010800*                                 ANTAL RADER                             
010900        05 MOD-VKORDNTO-RAD-ATTR                                          
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200        05 MOD-VKORDNTO-RAD  PIC Z(4)9.9.                                 
011300*                                 ORDERVIKT NETTO (KG)                    
011400        05 MOD-VLORDNTO-RAD-ATTR                                          
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700        05 MOD-VLORDNTO-RAD  PIC Z(2)9.9(3).                              
011800*                                 ORDERVOLYM NETTO (M3)                   
011900        05 MOD-KVORDER-RAD-ATTR                                           
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200        05 MOD-KVORDER-RAD   PIC Z(6)9.                                   
012300*                                 ANTAL ORDER                             
012400        05 MOD-IDTRP-RAD-ATTR                                             
012500                             PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700        05 MOD-IDTRP-RAD.                                                 
012800*                                 TRANSPORTIDENTITET                      
012900           07 MOD-IDTRPLOS   PIC X(3).                                    
013000*                                 TRANSPORTLÖSNING                        
013100           07 MOD-IDTRPVAR   PIC X(2).                                    
013200*                                 TRANSPORTLÖSNINGSGRUPP                  
013300     03 MOD-RESULT.                                                       
013400*                                 TABLE-LINES                             
013500        05 MOD-KVBEMAN-DORD-ATTR                                          
013600                             PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800        05 MOD-KVBEMAN-DORD  PIC Z(2)9.                                   
013900*                                 LAGEROMRÅDE                             
014000        05 MOD-KVPU-DORD-ATTR                                             
014100                             PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300        05 MOD-KVPU-DORD     PIC Z(2)9.                                   
014400*                                 LAGEROMRÅDE                             
014500     03 MOD-PF8-TILST        PIC 9(10).                                   
014600*                                 SENASTE STARTTIDPUNKT                   
014700     03 MOD-PFE-TILST        PIC 9(10).                                   
014800*                                 SENASTE STARTTIDPUNKT                   
014900     03 MOD-TEMFSINF         PIC X(55).                                   
015000*                                 INFORMATIONSMEDDELANDE                  
