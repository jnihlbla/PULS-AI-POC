000100 01  MID-W4I38301.                                                        
000200*                                 COPYTEXT FOR MID W4I38301               
000300     03 MID-PFI-IDPRC.                                                    
000400*                                 PRODUKTIONSKANAL                        
000500        05 MID-IDPRCBAS      PIC X(3).                                    
000600*                                 PRC-BAS                                 
000700        05 MID-IDPRCVAR      PIC X.                                       
000800*                                 PRC-VARIANT                             
000900     03 MID-PFI-TIRFS-AAMMDD PIC X(6).                                    
001000*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001100     03 MID-PFI-TIRFS-HHMM   PIC X(4).                                    
001200     03 MID-PFI-IDTRP.                                                    
001300*                                 TRANSPORTIDENTITET                      
001400        05 MID-IDTRPLOS      PIC X(3).                                    
001500*                                 TRANSPORTL÷SNING                        
001600        05 MID-IDTRPVAR      PIC X(2).                                    
001700*                                 TRANSPORTL÷SNINGSGRUPP                  
001800     03 MID-PFI-TITRPAVT-AAMMDD                                           
001900                             PIC X(6).                                    
002000*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002100     03 MID-PFI-TITRPAVT-HHMM                                             
002200                             PIC X(4).                                    
002300     03 MID-PFI-IDDC         PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MID-PF7-IDPRC.                                                    
002600*                                 PRODUKTIONSKANAL                        
002700        05 MID-IDPRCBAS      PIC X(3).                                    
002800*                                 PRC-BAS                                 
002900        05 MID-IDPRCVAR      PIC X.                                       
003000*                                 PRC-VARIANT                             
003100     03 MID-PF7-TIRFS-AAMMDD PIC 9(6).                                    
003200*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
003300     03 MID-PF7-TIRFS-HHMM   PIC 9(4).                                    
003400*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003500     03 MID-PF7-IDTRP.                                                    
003600*                                 TRANSPORTIDENTITET                      
003700        05 MID-IDTRPLOS      PIC X(3).                                    
003800*                                 TRANSPORTL÷SNING                        
003900        05 MID-IDTRPVAR      PIC X(2).                                    
004000*                                 TRANSPORTL÷SNINGSGRUPP                  
004100     03 MID-PF7-TITRPAVT-AAMMDD                                           
004200                             PIC 9(6).                                    
004300*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
004400     03 MID-PF7-TITRPAVT-HHMM                                             
004500                             PIC 9(4).                                    
004600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004700     03 MID-PF7-IDDC         PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900     03 MID-PFX-KVBEMAN-DORD PIC X(3).                                    
005000     03 MID-PFX-KVPU-DORD    PIC X(3).                                    
005100     03 MID-PF8-IDORDER      PIC 9(7).                                    
005200*                                 VOLVO PARTS ORDERNUMMER                 
005300     03 MID-PF8-IDPRODNR     PIC 9(7).                                    
005400*                                 PRODUKTIONSNUMMER                       
005500     03 MID-PF8-IDPLKLST     PIC 9(3).                                    
005600*                                 PLOCKLISTNUMMER                         
005700     03 MID-PF8-TIRFS        PIC 9(10).                                   
005800*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
005900     03 MID-PF8-IDPRCVAR     PIC X.                                       
006000*                                 PRC-VARIANT                             
006100     03 MID-PFE-IDORDER      PIC 9(7).                                    
006200*                                 VOLVO PARTS ORDERNUMMER                 
006300     03 MID-PFE-IDPRODNR     PIC 9(7).                                    
006400*                                 PRODUKTIONSNUMMER                       
006500     03 MID-PFE-IDPLKLST     PIC 9(3).                                    
006600*                                 PLOCKLISTNUMMER                         
006700     03 MID-PFE-TIRFS        PIC 9(10).                                   
006800*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
006900     03 MID-PFE-IDPRCVAR     PIC X.                                       
007000*                                 PRC-VARIANT                             
007100     03 MID-PFX-IDPRC.                                                    
007200*                                 PRODUKTIONSKANAL                        
007300        05 MID-IDPRCBAS      PIC X(3).                                    
007400*                                 PRC-BAS                                 
007500        05 MID-IDPRCVAR      PIC X.                                       
007600*                                 PRC-VARIANT                             
007700     03 MID-PF8-TILST        PIC 9(10).                                   
007800*                                 SENASTE STARTTIDPUNKT                   
007900     03 MID-PFE-TILST        PIC 9(10).                                   
008000*                                 SENASTE STARTTIDPUNKT                   
