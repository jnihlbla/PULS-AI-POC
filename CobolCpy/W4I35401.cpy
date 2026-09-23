000100 01  MID-W4I35401.                                                        
000200*                                 MID-COPYTEXT FÖR BILD  4354             
000300*                                 FRÅGA PÅ PRODUKTIONSKANAL               
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
001600     03 MID-IDUSER-IN        PIC X(8).                                    
001700*                                 ANVÄNDARENS SÄKERHETS ID                
001800     03 MID-IDUSER-UT        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 MID-IDORDNSB-ENTER   PIC 9(4).                                    
002100*                                 SATSORDERNUMMER-BAS                     
002200     03 MID-IDORDNSS-ENTER   PIC 9.                                       
002300*                                 SATSORDERNUMMER-SUFFIX                  
002400     03 MID-IDORDNSB-NEXT    PIC 9(4).                                    
002500*                                 SATSORDERNUMMER-BAS                     
002600     03 MID-IDORDNSS-NEXT    PIC 9.                                       
002700*                                 SATSORDERNUMMER-SUFFIX                  
002800     03 MID-SUACKPTI-ENTER   PIC 9(5)V9(2).                               
002900*                                 ACK PRODUKTIONSTID SATS TIM MIN         
003000     03 MID-SUACKPTI-NEXT    PIC 9(5)V9(2).                               
003100*                                 ACK PRODUKTIONSTID SATS TIM MIN         
003200     03 MID-IDORDNSB-BYGGBAR PIC 9(4).                                    
003300*                                 SATSORDERNUMMER-BAS                     
003400     03 MID-IDORDNSS-BYGGBAR PIC 9.                                       
003500*                                 SATSORDERNUMMER-SUFFIX                  
003600     03 MID-TABELLRAD        OCCURS 13 TIMES.                             
003700*                                 GRUPP MED TABELL RADER                  
003800        05 MID-KDCMD         PIC X.                                       
003900*                                 RAD-UPPDATERINGSKOMMANDO                
004000        05 MID-IDORDNSB      PIC 9(4).                                    
004100*                                 SATSORDERNUMMER-BAS                     
004200        05 MID-IDORDNSS      PIC 9.                                       
004300*                                 SATSORDERNUMMER-SUFFIX                  
004400*** END OF VILMAII-COPY LENGTH= 131 BYTES                                 
