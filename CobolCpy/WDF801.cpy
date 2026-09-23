000100 01  GSPR-WDF801.                                                         
000200*                                 STYRNING AV SPÄRREGLER                  
000300*                                 GRUPPER                                 
000400*                                 FYSISK NYCKEL: IDSPRGRP                 
000500*                                                                         
000600     03 GSPR-IDSPRGRP        PIC X(10).                                   
000700*                                 SPÄRRADE GRUPPER                        
000800*                                 BLOCKED GROUP                           
000900     03 GSPR-FLAUTUPD        PIC X.                                       
001000*                                 AUT. SPÄRR PER REGEL/ARTIKEL            
001100*                                 AUTOMATIC BLOCKED PART OR NOT           
001200     03 GSPR-IDUSER          PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400*                                 USER SECURITY-IDENTITY                  
001500     03 GSPR-KDMARKBLK       PIC S9(3)           COMP-3.                  
001600*                                 MARKNADSSPÄRR                           
001700*                                 MARKET BLOCK CODE                       
001800     03 GSPR-TENOTE          PIC X(40).                                   
001900*                                 NOTERINGSFÄLT                           
002000*                                 NOTE FIELD                              
002100     03 GSPR-TENOTE-60       PIC X(60).                                   
002200*                                 NOTERINGSFÄLT                           
002300*                                 NOTE FIELD                              
002400     03 GSPR-TISTADAT        PIC S9(7)           COMP-3.                  
002500*                                 GENERELLT STARTDATUM                    
002600*                                 GENERAL START DATE                      
002700*** END OF VILMAII-COPY LENGTH= 125 BYTES                                 
