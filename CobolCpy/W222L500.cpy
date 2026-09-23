000100 01  0-W222L500.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W22250         
000300     03 0-KDCALL             PIC S9(3)           COMP-3                   
000400                             VALUE ZEROS.                                 
000500*                                 ANROPSTYP                               
000600     03 0-LAES-ROT-WDD601    PIC S9(3)           COMP-3                   
000700                             VALUE +1.                                    
000800     03 0-LAES-WDD631        PIC S9(3)           COMP-3                   
000900                             VALUE +2.                                    
001000     03 0-LAES-WDD660        PIC S9(3)           COMP-3                   
001100                             VALUE +3.                                    
001200     03 0-IDARTNR            PIC S9(9)           COMP-3                   
001300                             VALUE ZEROS.                                 
001400*                                 ARTIKELNUMMER                           
001500     03 0-KDCLAGER           PIC S9              COMP-3                   
001600                             VALUE ZERO.                                  
001700*                                 CENTRALLAGERKOD                         
001800     03 0-KDSVAR             PIC X                                        
001900                             VALUE SPACE.                                 
002000*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002100     03 0-KDSVAR-OK          PIC X                                        
002200                             VALUE ' '.                                   
002300*                                 SVARSKOD : OK                           
002400     03 0-KDSVAR-FEL         PIC X                                        
002500                             VALUE 'F'.                                   
002600*                                 SVARSKOD: FEL                           
002700*                                                                         
002800*** END COPY W222L500    LENGTH=17                                        
