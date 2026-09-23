000100 01  RESP-WL0125O1.                                                       
000200*                                 RESPONS FROM PGM WL0125                 
000300     03 RESP-PFI-IDDC-KEY    PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-PFI-IDPRC-KEY.                                               
000600*                                 PRODUKTIONSKANAL                        
000700        05 RESP-IDPRCBAS     PIC X(3).                                    
000800*                                 PRC-BAS                                 
000900        05 RESP-IDPRCVAR     PIC X.                                       
001000*                                 PRC-VARIANT                             
001100     03 RESP-PFI-TIRFS-AAMMDD-KEY                                         
001200                             PIC 9(6).                                    
001300*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001400     03 RESP-PFI-TIRFS-HHMM-KEY                                           
001500                             PIC 9(4).                                    
001600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
001700     03 RESP-PFI-IDTRP-KEY.                                               
001800*                                 TRANSPORTIDENTITET                      
001900        05 RESP-IDTRPLOS     PIC X(3).                                    
002000*                                 TRANSPORTL÷SNING                        
002100        05 RESP-IDTRPVAR     PIC X(2).                                    
002200*                                 TRANSPORTL÷SNINGSGRUPP                  
002300     03 RESP-PFI-TITRPAVT-AAMMDD-KEY                                      
002400                             PIC 9(6).                                    
002500*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002600     03 RESP-PFI-TITRPAVT-HHMM-KEY                                        
002700                             PIC 9(4).                                    
002800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002900     03 RESP-RESULT.                                                      
003000*                                 TABLE-LINES                             
003100        05 RESP-KVBEMAN-DORD PIC Z9.                                      
003200*                                 BEMANNING, KAPACITET DAGORDER           
003300        05 RESP-KVPU-DORD    PIC Z(2)9.                                   
003400*                                 ANTAL PLOCKENHETER                      
003500     03 RESP-KVRADER         PIC Z(4)9.                                   
003600*                                 ANTAL RADER                             
003700     03 RESP-RAD             OCCURS 500 TIMES.                            
003800*                                 TABLE-LINES                             
003900        05 RESP-TILST-OD-RAD PIC 9(6)B9(4).                               
004000*                                 SENASTE STARTTIDPUNKT ORDERDEL          
004100        05 RESP-SUPTID-RAD   PIC Z(2)9.9(2).                              
004200*                                 TOTAL PRODUKTIONSTID TIM+MIN            
004300        05 RESP-TIRFS-RAD    PIC 9(6)B9(4).                               
004400*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
004500        05 RESP-KVRADER-RAD  PIC Z(4)9.                                   
004600*                                 ANTAL RADER                             
004700        05 RESP-KVRADER-MEZ-RAD                                           
004800                             PIC Z(4)9.                                   
004900*                                 ANTAL RADER                             
005000        05 RESP-VKORDNTO-RAD PIC Z(5)9.9.                                 
005100*                                 ORDERVIKT NETTO (KG)                    
005200        05 RESP-VLORDNTO-RAD PIC Z(3)9.9(3).                              
005300*                                 ORDERVOLYM NETTO (M3)                   
005400        05 RESP-KVORDER-RAD  PIC Z(6)9.                                   
005500*                                 ANTAL ORDER                             
005600        05 RESP-IDTRP-RAD.                                                
005700*                                 TRANSPORTIDENTITET                      
005800           07 RESP-IDTRPLOS  PIC X(3).                                    
005900*                                 TRANSPORTL÷SNING                        
006000           07 RESP-IDTRPVAR  PIC X(2).                                    
006100*                                 TRANSPORTL÷SNINGSGRUPP                  
006200*** END OF VILMAII-COPY LENGTH= 33041 BYTES                               
