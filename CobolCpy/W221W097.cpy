000010*** EDIT ALLOWED                                                          
000100************************************************************              
000200*                                                                         
000300*    TABELL FÖR FRAMTAGNING AV VILKET INDEX (I TABELLEN                   
000400*           FÖR KDPRODSL,KDPRISKL,KDFREKKL, W221W098)                     
000410*           SOM ETT VISST PRODUKTSLAG SKALL HA I                          
000500*           TABELLEN                                                      
000510*    SOM ANVÄNDS FÖR BERÄKNING AV SÄKERHETSLAGER                          
000600*    (INGÅR I PGM W221PUNK)                                               
000700*                                                                         
000800************************************************************              
000810                                                                          
000820 01  T-I-IX                  PIC S9(3) COMP-3 VALUE ZERO.                 
000830 01  T-I-IX-MAX              PIC S9(3) COMP-3 VALUE 25.                   
000900                                                                          
000901*     TABELL SAMBAND PRODUKTSLAG OCH TABELL-INDEX                         
000920                                                                          
001000 01  TABELL-PRODSL-IX.                                                    
001100     03 FILLER               PIC S9(3)  COMP-3  VALUE 11.                 
001101     03 FILLER               PIC S9(3)  COMP-3  VALUE  1.                 
001102     03 FILLER               PIC S9(3)  COMP-3  VALUE 14.                 
001103     03 FILLER               PIC S9(3)  COMP-3  VALUE  2.                 
001104     03 FILLER               PIC S9(3)  COMP-3  VALUE 15.                 
001105     03 FILLER               PIC S9(3)  COMP-3  VALUE  3.                 
001106     03 FILLER               PIC S9(3)  COMP-3  VALUE 16.                 
001107     03 FILLER               PIC S9(3)  COMP-3  VALUE  4.                 
001108     03 FILLER               PIC S9(3)  COMP-3  VALUE 17.                 
001109     03 FILLER               PIC S9(3)  COMP-3  VALUE  5.                 
001110     03 FILLER               PIC S9(3)  COMP-3  VALUE 18.                 
001120     03 FILLER               PIC S9(3)  COMP-3  VALUE  6.                 
001130     03 FILLER               PIC S9(3)  COMP-3  VALUE 19.                 
001140     03 FILLER               PIC S9(3)  COMP-3  VALUE  7.                 
001150     03 FILLER               PIC S9(3)  COMP-3  VALUE 21.                 
001160     03 FILLER               PIC S9(3)  COMP-3  VALUE  1.                 
001170     03 FILLER               PIC S9(3)  COMP-3  VALUE 24.                 
001180     03 FILLER               PIC S9(3)  COMP-3  VALUE  2.                 
001190     03 FILLER               PIC S9(3)  COMP-3  VALUE 25.                 
001200     03 FILLER               PIC S9(3)  COMP-3  VALUE  3.                 
001300     03 FILLER               PIC S9(3)  COMP-3  VALUE 26.                 
001400     03 FILLER               PIC S9(3)  COMP-3  VALUE  4.                 
001500     03 FILLER               PIC S9(3)  COMP-3  VALUE 27.                 
001600     03 FILLER               PIC S9(3)  COMP-3  VALUE  5.                 
001700     03 FILLER               PIC S9(3)  COMP-3  VALUE 28.                 
001800     03 FILLER               PIC S9(3)  COMP-3  VALUE  6.                 
001900     03 FILLER               PIC S9(3)  COMP-3  VALUE 29.                 
001910     03 FILLER               PIC S9(3)  COMP-3  VALUE  7.                 
001920     03 FILLER               PIC S9(3)  COMP-3  VALUE 31.                 
001930     03 FILLER               PIC S9(3)  COMP-3  VALUE  8.                 
001940     03 FILLER               PIC S9(3)  COMP-3  VALUE 34.                 
001950     03 FILLER               PIC S9(3)  COMP-3  VALUE  9.                 
001960     03 FILLER               PIC S9(3)  COMP-3  VALUE 35.                 
001970     03 FILLER               PIC S9(3)  COMP-3  VALUE 10.                 
001971     03 FILLER               PIC S9(3)  COMP-3  VALUE 38.                 
001972     03 FILLER               PIC S9(3)  COMP-3  VALUE 11.                 
001973     03 FILLER               PIC S9(3)  COMP-3  VALUE 91.                 
001974     03 FILLER               PIC S9(3)  COMP-3  VALUE  1.                 
001975     03 FILLER               PIC S9(3)  COMP-3  VALUE 94.                 
001976     03 FILLER               PIC S9(3)  COMP-3  VALUE  2.                 
001977     03 FILLER               PIC S9(3)  COMP-3  VALUE 95.                 
001978     03 FILLER               PIC S9(3)  COMP-3  VALUE  3.                 
001979     03 FILLER               PIC S9(3)  COMP-3  VALUE 96.                 
001980     03 FILLER               PIC S9(3)  COMP-3  VALUE  4.                 
001981     03 FILLER               PIC S9(3)  COMP-3  VALUE 97.                 
001982     03 FILLER               PIC S9(3)  COMP-3  VALUE  5.                 
001983     03 FILLER               PIC S9(3)  COMP-3  VALUE 98.                 
001984     03 FILLER               PIC S9(3)  COMP-3  VALUE  6.                 
001985     03 FILLER               PIC S9(3)  COMP-3  VALUE 99.                 
001986     03 FILLER               PIC S9(3)  COMP-3  VALUE  7.                 
001987                                                                          
001988******************************************************************        
001989*         GILTIGA PRODUKTSLAG OCH DESS INDEX I TAB-W221W098               
001990******************************************************************        
001991                                                                          
002000 01  FILLER  REDEFINES TABELL-PRODSL-IX.                                  
002100     03  FILLER                      OCCURS 25.                           
002110         05  TAB-KDPRODSL            PIC S9(3) COMP-3.                    
002111         05  TAB-IX-KDPRODSL         PIC S9(3) COMP-3.                    
