000100 01  4202-WDGX4202-CTX.                                                   
000200*                                 ÅTERSTARTS REGISTER                     
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4202-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4202-KVRESTART-W41283                                             
000900                             PIC S9(7)           COMP-3.                  
001000*                                 POSTRÄKNARE VID BMP-OMSTART             
001100*                                 RECORD COUNTER FOR BMP RESTART          
001200     03 4202-KVRESTART-W4128A                                             
001300                             PIC S9(7)           COMP-3.                  
001400*                                 POSTRÄKNARE VID BMP-OMSTART             
001500*                                 RECORD COUNTER FOR BMP RESTART          
001600     03 4202-TIUPPDAT        PIC S9(7)           COMP-3.                  
001700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001800*                                 UPDATING DATE     (YYMMDD)              
001900     03 4202-TIUPPTID        PIC S9(9)           COMP-3.                  
002000*                                 UPPDATERINGSTID  (TTMMSSTH)             
002100*                                 UPDATING TIME    (HHMMSSTH)             
002200*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
