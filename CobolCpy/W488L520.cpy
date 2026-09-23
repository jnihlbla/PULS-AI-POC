000100 01  AREA.                                                                
000200     03 KDCALL               PIC S9(3)           COMP-3                   
000300                             VALUE ZEROS.                                 
000400*                                 ANROPSTYP                               
000500     03 LAES-ARTIKELBAS      PIC S9(3)           COMP-3                   
000600                             VALUE +1.                                    
000700     03 LAES-ARTIKELBEN      PIC S9(3)           COMP-3                   
000800                             VALUE +2.                                    
000900     03 LAES-SALDO           PIC S9(3)           COMP-3                   
001000                             VALUE +3.                                    
001100     03 REPL-SALDO           PIC S9(3)           COMP-3                   
001200                             VALUE +4.                                    
001300     03 KDSVAR               PIC X                                        
001400                             VALUE SPACE.                                 
001500*                                 SVARSKOD FRÅN SUBPROGRAM                
001600     03 KDSVAR-OK            PIC X                                        
001700                             VALUE ' '.                                   
001800*                                 SVARSKOD : OK                           
001900     03 KDSVAR-FEL           PIC X                                        
002000                             VALUE 'F'.                                   
002100*                                 SVARSKOD: FEL                           
002200*                                                                         
002300*** END COPY W488L520C0  LENGTH=13                                        
