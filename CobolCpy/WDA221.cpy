000100 01  TXT-WDA221.                                                          
000200*                                 RAD FRI TEXT                            
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400     03 TXT-KDSEGKEY         PIC X.                                       
000500*                                 TEKNISK SEGMENT-NYCKEL                  
000600*                                 TECHNICAL SEGMENT KEY                   
000700     03 TXT-TEANMNOT-REG-GRP.                                             
000800        05 TXT-TEANMNOT-REG  OCCURS 3 TIMES                               
000900                             PIC X(70).                                   
001000*                                 FRI TEXT FRÅN REGISTRERINGEN            
001100*                                 FREE TEXT FROM REGISTRATION             
001200     03 TXT-TEANMNOT-ADM-GRP.                                             
001300        05 TXT-TEANMNOT-ADM  OCCURS 3 TIMES                               
001400                             PIC X(70).                                   
001500*                                 FRI TEXT FRÅN ADMINISTRATION            
001600*                                 FREE TEXT FROM ADMINISTRATION           
001700     03 TXT-TEANMNOT-REM-GRP.                                             
001800        05 TXT-TEANMNOT-REM  OCCURS 3 TIMES                               
001900                             PIC X(70).                                   
002000*                                 FRI TEXT FRÅN REMISSINSTANS             
002100*                                 TEXT FROM PERS. WHO CONSIDERED          
002200     03 TXT-TEANMNOT-RET-GRP.                                             
002300        05 TXT-TEANMNOT-RET  OCCURS 3 TIMES                               
002400                             PIC X(70).                                   
002500*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
002600*                                 FREE TEXT FROM RETURNDEPARTMENT         
002700     03 TXT-TEANMNOT-DLR-GRP.                                             
002800        05 TXT-TEANMNOT-DLR  OCCURS 3 TIMES                               
002900                             PIC X(70).                                   
003000*                                 FRI TEXT FRÅN ADMINISTRATION TI         
003100*                                 LL DEALERN                              
003200*                                 FREE TEXT FROM ADMINISTRATION T         
003300*                                 O DEALER                                
003400*** END OF VILMAII-COPY LENGTH= 1051 BYTES                                
