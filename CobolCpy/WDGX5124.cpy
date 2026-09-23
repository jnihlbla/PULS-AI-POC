000100 01  5124-WDGX5124.                                                       
000200*                                 EKONOMISKA PARAMETRAR                   
000300*                                 PARM PER KUND                           
000400*                                 WDG2                                    
000500*                                 FYSISK NYCKEL: KY5124                   
000600*                                 (IDDISTR + IDKUNDNR)                    
000700     03 5124-IDDISTR         PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000     03 5124-IDKUNDNR        PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300     03 5124-IDFTG           PIC 9(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500*                                 COMPANY IDENTITY ACCOUNTING             
001600     03 5124-KDTRADP         PIC X(4).                                    
001700*                                 TRADING PARTNER                         
001800*                                 TRADING PARTNER                         
001900     03 5124-IDUSER          PIC X(8).                                    
002000*                                 ANVÄNDARENS SÄKERHETS ID                
002100*                                 USER SECURITY-IDENTITY                  
002200     03 5124-TIREGDAT        PIC S9(7)           COMP-3.                  
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400*                                 REGISTRATION DATE (YYMMDD)              
002500     03 FILLER               PIC X(15).                                   
002600*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
