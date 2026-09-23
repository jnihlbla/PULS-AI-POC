000100 01  BEN-W12120.                                                          
000200*                                 BENÄMNINGAR FRÅN TECHLA.                
000300*                                 EN POST FÖRST I VARJE GRUPP             
000400*                                 HAR SPRÅKKOD = BLANK OCH                
000500*                                 INNEHÅLLER HOMONYMTEXT                  
000600*                                                                         
000700*                                 DESCRIPTIONS FROM TECHLA.               
000800*                                 ONE FIRST RECORD IN EACH                
000900*                                 GROUP HAS LANGUAGE=BLANK                
001000*                                 AND CONTAINS HOMONYM TEXT               
001100*                                                                         
001200     03 BEN-W12120-ID.                                                    
001300*                                 POST-GRUPPS ID                          
001400*                                 RECORD GROUP ID                         
001500        05 BEN-IDTECHLA      PIC X(10).                                   
001600*                                 TECHLA-BENÄMNINGID                      
001700*                                 TECHLA ID                               
001800        05 BEN-IDSPRAK       PIC X(2).                                    
001900*                                 2-STÄLLIG ISO SPRÅKKOD                  
002000*                                 2-LETTER ISO LANGUAGE CODE              
002100        05 BEN-KDHOMONYM     PIC 9.                                       
002200*                                 HOMONYMKOD                              
002300*                                 HOMONYMOUS CODE                         
002400     03 BEN-BETECHLA         PIC X(60).                                   
002500*                                 BENÄMNING FRÅN TECHLA                   
002600*                                 NAME/DESCRIPTION FROM TECHLA            
002700     03 BEN-TEHOMONYM-1 REDEFINES BEN-BETECHLA                            
002800                             PIC X(60).                                   
002900*                                 HOMONYMTEXT                             
003000     03 BEN-TEHOMONYM-2      PIC X(60).                                   
003100*                                 HOMONYMTEXT                             
003200*** END OF VILMAII-COPY LENGTH= 133 BYTES                                 
