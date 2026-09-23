000100 01  PBTO-W222PBTO.                                                       
000200*                                 LÄNKAREA TILL W222PBTO.                 
000300*                                                                         
000400*                                 INSTRUKTION:                            
000500*                                                                         
000600*                                 FYLL I INDATA:                          
000700*                                 ARTNR        (OBLIGATORISKT)            
000800*                                                                         
000900*                                                                         
001000*                                 ANROPA W222PBTO USING                   
001100*                                        PBTO-W222PBTO                    
001200*                                                                         
001300*                                 OM OK        (KDSVAR=J),                
001400*                                                                         
001500     03 PBTO-IN-UTDATA.                                                   
001600        05 PBTO-INDATA.                                                   
001700           07 PBTO-IDARTNR   PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900        05 PBTO-UTDATA.                                                   
002000           07 PBTO-RESEASON-PLAN                                          
002100                             OCCURS 12 TIMES                              
002200                             PIC S9V9(2)         COMP-3.                  
002300*                                 SÄSONGSINDEX INKLUSIVE REFILL           
002400           07 PBTO-KVPB-PLAN PIC S9(6)V9(1)      COMP-3.                  
002500*                                 PLANERAT PERIODBEHOV                    
002600     03 PBTO-KDSVAR          PIC X.                                       
002700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002800*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
