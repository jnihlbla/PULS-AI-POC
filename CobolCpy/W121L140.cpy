000100 01  W121L140-CTX.                                                        
000200*                                 LÄNKAREA TILL PROGRAMMET W12114         
000300     03 KDCALL               PIC S9(3)           COMP-3                   
000400                             VALUE ZEROS.                                 
000500*                                 ANROPSTYP                               
000600     03 LAS-BENAEMNINGSNR    PIC S9(3)           COMP-3                   
000700                             VALUE +1.                                    
000800     03 LAS-ARTIKELSEGMENT   PIC S9(3)           COMP-3                   
000900                             VALUE +2.                                    
001000     03 DLET-BENAEMNING      PIC S9(3)           COMP-3                   
001100                             VALUE +3.                                    
001200     03 KDSVAR               PIC X                                        
001300                             VALUE SPACE.                                 
001400*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001500     03 KDSVAR-OK            PIC X                                        
001600                             VALUE ' '.                                   
001700*                                 SVARSKOD : OK                           
001800     03 KDSVAR-FEL           PIC X                                        
001900                             VALUE 'F'.                                   
002000*                                 SVARSKOD: FEL                           
002100*                                                                         
002200     03 IDBENNR              PIC S9(7)           COMP-3                   
002300                             VALUE ZEROS.                                 
002400*                                 BENÄMNINGSNUMMER                        
002500*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
