000100 01  W488L220.                                                            
000200*                                 STYRPARAMETRAR FÖR                      
000300*                                 KOMMUNIKATION MELLAN                    
000400*                                 W4882200 OCH DESS SUBPGM                
000500     03 KDCALL               PIC S9(3)           COMP-3                   
000600                             VALUE ZEROS.                                 
000700*                                 ANROPSTYP                               
000800     03 LAS-HTR-TID          PIC S9(3)           COMP-3                   
000900                             VALUE +1.                                    
001000     03 UPPDATERA-HTR-TID    PIC S9(3)           COMP-3                   
001100                             VALUE +2.                                    
001200     03 UPPDATERA-HTR-FEL    PIC S9(3)           COMP-3                   
001300                             VALUE +3.                                    
001400     03 LAS-SALDO            PIC S9(3)           COMP-3                   
001500                             VALUE +4.                                    
001600     03 UPPDATERA-SALDO      PIC S9(3)           COMP-3                   
001700                             VALUE +5.                                    
001800     03 RESTART              PIC S9(3)           COMP-3                   
001900                             VALUE +6.                                    
002000     03 CHECKPOINT           PIC S9(3)           COMP-3                   
002100                             VALUE +7.                                    
002200     03 KDSVAR               PIC X                                        
002300                             VALUE SPACE.                                 
002400*                                 SVARSKOD FRÅN SUBPROGRAM                
002500     03 KDSVAR-OK            PIC X                                        
002600                             VALUE ' '.                                   
002700*                                 SVARSKOD : OK                           
002800     03 KDSVAR-FEL           PIC X                                        
002900                             VALUE 'F'.                                   
003000*                                 SVARSKOD: FEL                           
003100*                                                                         
003200*** END COPY W488L220C0  LENGTH=19                                        
