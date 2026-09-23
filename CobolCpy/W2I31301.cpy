000100 01  MID-W2I31301.                                                        
000200*                                 MID-COPYTEXT FÖR W2031300               
000300     03 MID-IDKAMP-IN        PIC X(7).                                    
000400*                                 SERVICEKAMPANJ                          
000500     03 MID-IDKAMP-UT        PIC X(7).                                    
000600*                                 SERVICEKAMPANJ                          
000700     03 MID-IDKAMP-GRP-IN    PIC X(7).                                    
000800*                                 ID FÖR KAMPANJGRUPPER                   
000900     03 MID-IDKAMP-GRP-UT    PIC X(7).                                    
001000*                                 ID FÖR KAMPANJGRUPPER                   
001100     03 MID-INPUT.                                                        
001200        05 MID-GRP.                                                       
001300           07 MID-CMD        OCCURS 7 TIMES                               
001400                             PIC X.                                       
001500           07 MID-TISTADAT-KAMP-RAD                                       
001600                             OCCURS 7 TIMES                               
001700                             PIC X(6).                                    
001800*                                 STARTDATUM FÖR KAMPANJ                  
001900           07 MID-TISTODAT-KAMP-RAD                                       
002000                             OCCURS 7 TIMES                               
002100                             PIC X(6).                                    
002200*                                 STOPPDATUM FÖR KAMPANJ                  
002300           07 MID-RERESPRT-IN                                             
002400                             PIC X(3).                                    
002500*                                                                         
002600           07 MID-RERESPRT-UT                                             
002700                             PIC X(3).                                    
002800*                                                                         
002900        05 MID-NOTE1         PIC X(79).                                   
003000        05 MID-NOTE2         PIC X(79).                                   
003100        05 MID-REG-NY-RAD.                                                
003200           07 MID-KDKAMP     PIC X.                                       
003300*                                                                         
003400           07 MID-TISTADAT-KAMP                                           
003500                             PIC X(6).                                    
003600*                                 STARTDATUM FÖR KAMPANJ                  
003700           07 MID-TISTODAT-KAMP                                           
003800                             PIC X(6).                                    
003900*                                 STOPPDATUM FÖR KAMPANJ                  
004000*** END OF VILMAII-COPY LENGTH= 296 BYTES                                 
