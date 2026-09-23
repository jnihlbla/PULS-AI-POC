000100 01  4452-WDGX4452.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 PRODUKTIONSTIDS TABELL                  
000400*                                 FYSISK NYCKEL                           
000500*                                 KDSEGKEY = 1                            
000600     03 4452-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 4452-KDSORT-Y        PIC X(2).                                    
001000*                                 SORT-KOD Y-AXEL                         
001100*                                 SORT-CODE Y-AXIS                        
001200     03 4452-KDSORT-X        PIC X(2).                                    
001300*                                 SORT-KOD X-AXEL                         
001400*                                 SORT-CODE X-AXIS                        
001500     03 4452-RAD             OCCURS 10 TIMES                              
001600                             INDEXED 4452-Y-IX.                           
001700        05 4452-KVPTSORT-Y   PIC S9(4)V9(1)      COMP-3.                  
001800*                                 ANTAL I PTIDSTABELL Y-AXEL              
001900*                                 QUANTITY IN PTIME TABLE Y-AXIS          
002000        05 4452-KVPTSORT-X   PIC S9(4)V9(1)      COMP-3.                  
002100*                                 ANTAL I PTIDSTABELL X-AXEL              
002200*                                 QUANTITY IN PTIME TABLE X-AXIS          
002300        05 4452-KOL          OCCURS 10 TIMES                              
002400                             INDEXED 4452-X-IX.                           
002500           07 4452-KVPTID    PIC S9(2)V9(1)      COMP-3.                  
002600*                                 GENOMSNITTLIG TID/RAD MINUTER           
002700*                                 AVERIDGE TIME/LINE MINUTES              
002800     03 4452-FILLER          PIC X(35).                                   
002900*** END COPY WDGX4452C0  LENGTH=300                                       
