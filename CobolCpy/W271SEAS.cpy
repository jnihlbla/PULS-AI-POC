000100 01  SEAS-W271SEAS.                                                       
000200*                                 LÄNKAREA TILL W271SEAS.                 
000300*                                                                         
000400*                                 INSTRUKTION:                            
000500*                                                                         
000600*                                 FYLL I INDATA:                          
000700*                                 ARTNR        (OBLIGATORISKT)            
000800*                                 DC           (OBLIGATORISKT)            
000900*                                 FLKVARTAL    (OBLIGATORISKT)            
001000*                                                                         
001100*                                 ANROPA W271SEAS USING                   
001200*                                        SEAS-W271SEAS                    
001300*                                                                         
001400*                                 OM OK        (KDSVAR=SPACE),            
001500*                                                                         
001600     03 SEAS-IN-UTDATA.                                                   
001700        05 SEAS-INDATA.                                                   
001800           07 SEAS-IDARTNR   PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000           07 SEAS-IDDC      PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200           07 SEAS-FLKVARTAL PIC X.                                       
002300        05 SEAS-UTDATA.                                                   
002400           07 SEAS-RESEASON  OCCURS 12 TIMES                              
002500                             PIC 9(4).                                    
002600           07 SEAS-KVOI      OCCURS 12 TIMES                              
002700                             PIC S9(7)           COMP-3.                  
002800*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002900           07 SEAS-ANT-HIST-AR                                            
003000                             PIC 9.                                       
003100           07 SEAS-ANT-HIST-MAN                                           
003200                             PIC 9(2).                                    
003300           07 SEAS-OSAKERHET PIC 9(3)V9(1).                               
003400           07 SEAS-SEASON-ARTIKEL                                         
003500                             PIC X.                                       
003600     03 SEAS-KDSVAR          PIC X.                                       
003700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003800*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
