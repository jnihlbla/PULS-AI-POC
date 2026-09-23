000100 01  MID-W4I35301.                                                        
000200*                                 MID-COPYTEXT F÷R BILD  4353             
000300*                                 MANUELLT UTTAG AV PLOCKSATS             
000400     03 MID-IDPRC-IN.                                                     
000500*                                 PRODUKTIONSKANAL                        
000600        05 MID-IDPRCBAS      PIC X(3).                                    
000700*                                 PRC-BAS                                 
000800        05 MID-IDPRCVAR      PIC X.                                       
000900*                                 PRC-VARIANT                             
001000     03 MID-IDPRC-UT.                                                     
001100*                                 PRODUKTIONSKANAL                        
001200        05 MID-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BAS                                 
001400        05 MID-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MID-IDPLKLST-IN      PIC X(3).                                    
001700*                                 PLOCKLISTNUMMER                         
001800     03 MID-IDPLKLST-UT      PIC X(3).                                    
001900*                                 PLOCKLISTNUMMER                         
002000     03 MID-IDDC-IN          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MID-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MID-IDTRP-IN.                                                     
002500*                                 TRANSPORTIDENTITET                      
002600        05 MID-IDTRPLOS      PIC X(3).                                    
002700*                                 TRANSPORTL÷SNING                        
002800        05 MID-IDTRPVAR      PIC X(2).                                    
002900*                                 TRANSPORTL÷SNINGSGRUPP                  
003000     03 MID-IDTRP-UT.                                                     
003100*                                 TRANSPORTIDENTITET                      
003200        05 MID-IDTRPLOS      PIC X(3).                                    
003300*                                 TRANSPORTL÷SNING                        
003400        05 MID-IDTRPVAR      PIC X(2).                                    
003500*                                 TRANSPORTL÷SNINGSGRUPP                  
003600     03 MID-TIAAMMDD-IN      PIC X(6).                                    
003700*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
003800     03 MID-TIAAMMDD-UT      PIC X(6).                                    
003900*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
004000     03 MID-TIHHMM-IN        PIC X(4).                                    
004100*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004200     03 MID-TIHHMM-UT        PIC X(4).                                    
004300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004400     03 MID-IDDISTR-IN       PIC X(4).                                    
004500*                                 DISTRIKTNUMMER                          
004600     03 MID-IDDISTR-UT       PIC X(4).                                    
004700*                                 DISTRIKTNUMMER                          
004800     03 MID-IDKUNDNR-IN      PIC X(6).                                    
004900*                                 KUNDNUMMER                              
005000     03 MID-IDKUNDNR-UT      PIC X(6).                                    
005100*                                 KUNDNUMMER                              
005200     03 MID-IDORDNR7-IN      PIC X(7).                                    
005300*                                 ORDERNUMMER                             
005400     03 MID-IDORDNR7-UT      PIC X(7).                                    
005500*                                 ORDERNUMMER                             
005600     03 MID-KDPRT-PU         PIC X(3).                                    
005700*                                 PRINTERKOD PACKUNDERLAG                 
005800     03 MID-KDPRT-PLE        PIC X(3).                                    
005900*                                 PRINTERKOD PLOCKETIKETTER               
006000     03 MID-MIXAT            PIC X.                                       
006100*                                 ALLMƒN SVARSFLAGGA                      
006200     03 MID-ANTPLK           PIC 9(6).                                    
006300*                                 ANTAL ALLMƒNT                           
006400     03 MID-WDQ3BSEQ.                                                     
006500*                                 B-INDEX P≈ Q3-BASEN                     
006600        05 MID-IDDC-BSEQ     PIC X(2).                                    
006700*                                 IDENTIFIERARE LAGER                     
006800        05 MID-IDPRCBAS-BSEQ PIC X(3).                                    
006900*                                 PRC-BAS                                 
007000        05 MID-TIUTSKR-BSEQ  PIC 9(6).                                    
007100*                                 UTSKRIFTDATUM  (≈≈MMDD)                 
007200        05 MID-TIUTSTID-BSEQ PIC 9(6).                                    
007300*                                 UTSKRIFTSTID (TTMMSS)                   
007400        05 MID-TIRFS-BSEQ    PIC 9(10).                                   
007500*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
007600        05 MID-TILST-O-BSEQ  PIC 9(10).                                   
007700*                                 SENASTE STARTTIDPUNKT F÷R ORDER         
007800        05 MID-IDPRCVAR-BSEQ PIC X.                                       
007900*                                 PRC-VARIANT                             
008000     03 MID-WDQ3ASEQ.                                                     
008100*                                 A-INDEX P≈ Q3-BASEN                     
008200        05 MID-IDDC-ASEQ     PIC X(2).                                    
008300*                                 IDENTIFIERARE LAGER                     
008400        05 MID-IDTRP-ASEQ.                                                
008500*                                 TRANSPORTIDENTITET                      
008600           07 MID-IDTRPLOS   PIC X(3).                                    
008700*                                 TRANSPORTL÷SNING                        
008800           07 MID-IDTRPVAR   PIC X(2).                                    
008900*                                 TRANSPORTL÷SNINGSGRUPP                  
009000        05 MID-TIAAMMDD-ASEQ PIC 9(6).                                    
009100*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
009200        05 MID-TIHHMM-ASEQ   PIC 9(4).                                    
009300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
009400        05 MID-TIRFS-ASEQ    PIC 9(10).                                   
009500*                                 KLART F÷R TRANSPORT ≈≈MMDDTTMM          
009600        05 MID-TILST-O-ASEQ  PIC 9(10).                                   
009700*                                 SENASTE STARTTIDPUNKT F÷R ORDER         
009800        05 MID-IDPRC-ASEQ.                                                
009900*                                 PRODUKTIONSKANAL                        
010000           07 MID-IDPRCBAS   PIC X(3).                                    
010100*                                 PRC-BAS                                 
010200           07 MID-IDPRCVAR   PIC X.                                       
010300*                                 PRC-VARIANT                             
