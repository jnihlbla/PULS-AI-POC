000100 01  REQU-WL0125I1.                                                       
000200*                                 REQUEST TO PGM  WL0125                  
000300     03 REQU-PFI-IDDC-KEY    PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-PFI-IDPRC-KEY.                                               
000600*                                 PRODUKTIONSKANAL                        
000700        05 REQU-IDPRCBAS     PIC X(3).                                    
000800*                                 PRC-BAS                                 
000900        05 REQU-IDPRCVAR     PIC X.                                       
001000*                                 PRC-VARIANT                             
001100     03 REQU-PFI-TIRFS-AAMMDD-KEY                                         
001200                             PIC 9(6).                                    
001300*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001400     03 REQU-PFI-TIRFS-HHMM-KEY                                           
001500                             PIC 9(4).                                    
001600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
001700     03 REQU-PFI-IDTRP-KEY.                                               
001800*                                 TRANSPORTIDENTITET                      
001900        05 REQU-IDTRPLOS     PIC X(3).                                    
002000*                                 TRANSPORTL÷SNING                        
002100        05 REQU-IDTRPVAR     PIC X(2).                                    
002200*                                 TRANSPORTL÷SNINGSGRUPP                  
002300     03 REQU-PFI-TITRPAVT-AAMMDD-KEY                                      
002400                             PIC 9(6).                                    
002500*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002600     03 REQU-PFI-TITRPAVT-HHMM-KEY                                        
002700                             PIC 9(4).                                    
002800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002900*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
