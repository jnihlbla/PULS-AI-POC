000100 01  RESP-W60126O1.                                                       
000200*                                 UTDATA F÷R PROGRAM W60126               
000300*                                 PROGRAMMET VISAR PLACERINGS-            
000400*                                 HISTORIK F÷R INLEVERANS                 
000500     03 RESP-IDRADNR-START   PIC 9(4).                                    
000600*                                 RADNUMMER                               
000700     03 RESP-DAREGDAT-START  PIC 9(8).                                    
000800*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
000900     03 RESP-TIKLOCK-START   PIC 9(8).                                    
001000*                                 KLOCKSLAG (TTMMSSTH)                    
001100     03 RESP-IDRADNR-NEXT    PIC 9(4).                                    
001200*                                 RADNUMMER                               
001300     03 RESP-DAREGDAT-NEXT   PIC 9(8).                                    
001400*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001500     03 RESP-TIKLOCK-NEXT    PIC 9(8).                                    
001600*                                 KLOCKSLAG (TTMMSSTH)                    
001700     03 RESP-KVRADER         PIC Z(4)9.                                   
001800*                                 ANTAL RADER                             
001900     03 RESP-RAD-GRUPP       OCCURS 100 TIMES.                            
002000*                                 RADER SOM VISAR PLACERINGS-             
002100*                                 HISTORIK                                
002200        05 RESP-IDRADNR      PIC X(4).                                    
002300*                                 RADNUMMER                               
002400        05 RESP-TIREGDAT     PIC X(6).                                    
002500*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002600        05 RESP-TIHHMM       PIC X(5).                                    
002700*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002800        05 RESP-KDINLSTA     PIC X(3).                                    
002900*                                 SYSTEMSTATUS INLEVERANS                 
003000        05 RESP-ADINLOMR     PIC X(4).                                    
003100*                                 INLEVERANSOMR≈DE                        
003200        05 RESP-ADINLOMR-NXT PIC X(4).                                    
003300*                                 INLEVERANSOMR≈DE                        
003400        05 RESP-KVINLART     PIC X(6).                                    
003500*                                 ANTAL I PARTIRAD                        
003600        05 RESP-IDUSER       PIC X(8).                                    
003700*                                 ANVƒNDARENS SƒKERHETS ID                
003800*** END OF VILMAII-COPY LENGTH= 4045 BYTES                                
