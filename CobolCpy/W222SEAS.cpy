000100 01  SEAS-W222SEAS.                                                       
000200*                                 LÄNKAREA TILL W222SEAS.                 
000300*                                                                         
000400*                                 INSTRUKTION:                            
000500*                                                                         
000600*                                 FYLL I INDATA:                          
000700*                                 ARTNR        (OBLIGATORISKT)            
000800*                                 FLKVARTAL    (OBLIGATORISKT)            
000900*                                                                         
001000*                                 ANROPA W222SEAS USING                   
001100*                                        SEAS-W222SEAS                    
001200*                                                                         
001300*                                 OM OK        (KDSVAR=SPACE),            
001400*                                                                         
001500     03 SEAS-IN-UTDATA.                                                   
001600        05 SEAS-INDATA.                                                   
001700           07 SEAS-IDARTNR   PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900           07 SEAS-FLKVARTAL PIC X.                                       
002000           07 SEAS-IDFKNGRP  PIC S9(5)           COMP-3.                  
002100*                                 FUNKTIONSGRUPP                          
002200        05 SEAS-UTDATA.                                                   
002300           07 SEAS-RESEASON  OCCURS 12 TIMES                              
002400                             PIC 9(4).                                    
002500           07 SEAS-KVOI      OCCURS 12 TIMES                              
002600                             PIC S9(7)           COMP-3.                  
002700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002800           07 SEAS-ANT-HIST-AR                                            
002900                             PIC 9.                                       
003000           07 SEAS-OSAKERHET PIC 9(3)V9(1).                               
003100           07 SEAS-SEASON-ARTIKEL                                         
003200                             PIC X.                                       
003300     03 SEAS-KDSVAR          PIC X.                                       
003400*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003500*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
