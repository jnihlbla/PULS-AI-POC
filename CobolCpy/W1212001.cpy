000100 01  HOM-W1212001.                                                        
000200*                                 BENÄMNINGAR FRÅN TECHLA.                
000300*                                 DENNA POST FÖRST I VARJE GRUPP          
000400*                                 HAR SPRÅKKOD = BLANK OCH                
000500*                                 INNEHÅLLER HOMONYMTEXT                  
000600*                                                                         
000700*                                 DESCRIPTIONS FROM TECHLA.               
000800*                                 THIS FIRST RECORD IN EACH               
000900*                                 GROUP HAS LANGUAGE=BLANK                
001000*                                 AND CONTAINS HOMONYM TEXT               
001100*                                                                         
001200     03 HOM-W12120-ID.                                                    
001300*                                 POST-GRUPPS ID                          
001400*                                 RECORD GROUP ID                         
001500        05 HOM-IDTECHLA      PIC X(10).                                   
001600*                                 TECHLA-BENÄMNINGID                      
001700*                                 TECHLA ID                               
001800        05 HOM-IDSPRAK       PIC X(2).                                    
001900*                                 2-STÄLLIG ISO SPRÅKKOD                  
002000*                                 2-LETTER ISO LANGUAGE CODE              
002100        05 HOM-KDHOMONYM     PIC 9.                                       
002200*                                 HOMONYMKOD                              
002300*                                 HOMONYMOUS CODE                         
002400     03 HOM-TECHLA.                                                       
002500*                                 HOMONYM FRÅN TECHLA                     
002600*                                 IDENTIFIKATION OCH TEHOMONYM-1          
002700*                                                                         
002800        05 FILLER            PIC X(2).                                    
002900        05 HOM-TECHLA-COMPANY                                             
003000                             PIC X(5).                                    
003100        05 HOM-TEHOMONYM-1   PIC X(60).                                   
003200*                                 HOMONYMTEXT                             
003300     03 HOM-TEHOMONYM-2      PIC X(60).                                   
003400*                                 HOMONYMTEXT                             
003500*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 
