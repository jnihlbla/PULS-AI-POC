000100 01  MARK-W510MARK.                                                       
000200*                                 LÄNKAREA VID ANROP AV SUBPGM            
000300*                                 W510MARK.                               
000400*                                                                         
000500*                                 KDCALL IFYLLS ALLTID.                   
000600*                                 -KDCALL=0 :SÖK MED MARK-IDDISTR         
000700*                                  SVAR: MARK-BEMARKN,                    
000800*                                        MARK-KDMARK-BUDG                 
000900*                                 -KDCALL=1 :SÖK MED MARK-KDMARK-         
001000*                                            BUDG.                        
001100*                                  SVAR: MARK-BEMARKN.                    
001200*                                                                         
001300*                                  MÖJLIGA MARK-KDSVAR:                   
001400*                                  - 0 => ALLT ÄR OK                      
001500*                                  - 1 => SÖKT DISTRIKT FINNS EJ,         
001600*                                    MARKNAD 95 ERHÅLLES.                 
001700*                                  - 2 => SÖKT MARKNAD FINNS EJ.          
001800*                                                                         
001900     03 MARK-KDCALL          PIC S9(3)           COMP-3.                  
002000*                                 ANROPSTYP                               
002100     03 MARK-TIAA            PIC S9(3)           COMP-3.                  
002200*                                 ÅR    (ÅÅ)                              
002300     03 MARK-IDDISTR         PIC S9(5)           COMP-3.                  
002400*                                 DISTRIKTNUMMER                          
002500     03 MARK-KDMARK-BUDG     PIC S9(3)           COMP-3.                  
002600*                                 MARKNADSKOD BUDGET 96 MARKNADER         
002700     03 MARK-BEMARKN         PIC X(24).                                   
002800*                                 MARKNADSBENÄMNING                       
002900     03 MARK-KDSVAR          PIC X.                                       
003000*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003100*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
