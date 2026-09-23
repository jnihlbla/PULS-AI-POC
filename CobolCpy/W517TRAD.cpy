000100 01  TRAD-W517TRAD.                                                       
000200*                                 LÄNKAREA VID ANROP AV SUBPGM            
000300*                                 W517TRAD.                               
000400*                                                                         
000500*                                 KDCALL IFYLLS ALLTID.                   
000600*                                 -KDCALL=0 :SÖK MED TRAD-KDTRADP         
000700*                                  SVAR: TRAD-IDDISTR,                    
000800*                                                                         
000900*                                  MÖJLIGA MARK-KDSVAR:                   
001000*                                  - 0 => ALLT ÄR OK                      
001100*                                  - 1 => SÖKT KDTRADP  FINNS EJ,         
001200*                                    DISTR  0   ERHÅLLES.                 
001300*                                                                         
001400     03 TRAD-KDCALL          PIC S9(3)           COMP-3.                  
001500*                                 ANROPSTYP                               
001600     03 TRAD-KDTRADP         PIC X(4).                                    
001700*                                 TRADING PARTNER                         
001800     03 TRAD-IDDISTR         PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000     03 TRAD-KDSVAR          PIC X.                                       
002100*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002200*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
