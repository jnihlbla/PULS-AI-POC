000010*** EDIT ALLOWED                                                          
000100 01  WWLAND03.                                                            
000200*                                 ANVÄNDS VID TEST AV GODKÄNDA            
000300*                                 SPRÅK (DISPLAY/SKRIV-BARA)              
000400*                                 WWLAND01   ANVÄNDS I LIST-PGM           
000500     03 WWLAND03-MAX-ANTAL   PIC S9(9)           COMP                     
000600                             VALUE +11.                                   
000700     03 WWLAND03-IDSKYLTVARDE.                                            
000800        05 FILLER            PIC X(3)                                     
000900                             VALUE 'D  '.                                 
001000        05 FILLER            PIC X(3)                                     
001100                             VALUE 'E  '.                                 
001200        05 FILLER            PIC X(3)                                     
001300                             VALUE 'F  '.                                 
001400        05 FILLER            PIC X(3)                                     
001500                             VALUE 'GB '.                                 
001600        05 FILLER            PIC X(3)                                     
001700                             VALUE 'I  '.                                 
001800        05 FILLER            PIC X(3)                                     
001900                             VALUE 'MAL'.                                 
001910        05 FILLER            PIC X(3)                                     
001920                             VALUE 'NL '.                                 
002000        05 FILLER            PIC X(3)                                     
002100                             VALUE 'P  '.                                 
002200        05 FILLER            PIC X(3)                                     
002300                             VALUE 'S  '.                                 
002400        05 FILLER            PIC X(3)                                     
002500                             VALUE 'SF '.                                 
002600        05 FILLER            PIC X(3)                                     
002700                             VALUE 'USA'.                                 
002800     03 FILLER REDEFINES WWLAND03-IDSKYLTVARDE.                           
002900        05 WWLAND03-IDSKYLT-RAD                                           
003000                             OCCURS 11 TIMES                              
003100                             INDEXED WWLAND03-IX.                         
003200           07 WWLAND03-IDSKYLT                                            
003300                             PIC X(3).                                    
003400*                                 NATIONALITETSTECKEN                     
003500*** END COPY WWLAND03    LENGTH=34                                        
