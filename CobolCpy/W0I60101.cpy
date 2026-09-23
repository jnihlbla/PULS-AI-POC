000100 01  MID-W0I60101.                                                        
000200*                                 COPYTEXT FÖR MID W0I60101               
000300     03 MID-IDLTERM-IN       PIC X(8).                                    
000400*                                 LOGISKT TERMINALNAMN                    
000500     03 MID-IDLTERM-UT       PIC X(8).                                    
000600*                                 LOGISKT TERMINALNAMN                    
000700     03 MID-IDLIST-IN        PIC X(10).                                   
000800*                                 LISTIDENTITET                           
000900     03 MID-IDLIST-UT        PIC X(10).                                   
001000*                                 LISTIDENTITET                           
001100     03 MID-TIREGDAT-IN      PIC X(6).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001300     03 MID-TIREGDAT-UT      PIC X(6).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500     03 MID-IDLTERM-BACKUP   PIC X(8).                                    
001600*                                 LOGISKT TERMINALNAMN                    
001700     03 MID-TIKLOCK-9KOMPL-ENTER                                          
001800                             PIC 9(9).                                    
001900*                                 TID LAGRAT SOM 9-KOMPLEMENT             
002000*                                                                         
002100     03 MID-TIKLOCK-9KOMPL-NEXT                                           
002200                             PIC 9(9).                                    
002300*                                 TID LAGRAT SOM 9-KOMPLEMENT             
002400*                                                                         
002500     03 MID-RAD              OCCURS 12 TIMES.                             
002600        05 MID-KDSVAR        PIC X.                                       
002700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002800        05 MID-TIKLOCK-RAD   PIC 9(9).                                    
002900*                                 KLOCKSLAG (TTMMSSTH)                    
003000*** END COPY W0I60101C0  LENGTH=194                                       
