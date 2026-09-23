000010*** EDIT ALLOWED                                                          
000100 01  WWLAND06.                                                            
000200*                            ANVÄNDS VID TEST AV GODKÄNDA SPRÅK.          
000300*                            ALLA PÅ BENÄMNINGSREGISTRET.                 
000400*                            INKL. EJ-EDITERBARA/EJ-VISNINGSBARA          
000401*                            ICKE-EUROPEISKA SPRÅKEN.                     
000410*                                                                         
000500     03 WWLAND06-MAX-ANTAL   PIC S9(9)           COMP                     
000600                             VALUE +26.                                   
000700     03 WWLAND06-IDSKYLTVARDE.                                            
000900        05 FILLER            PIC X(3)  VALUE 'CZ '.                       
001000        05 FILLER            PIC X(3)  VALUE 'D  '.                       
001010        05 FILLER            PIC X(3)  VALUE 'DK '.                       
001100        05 FILLER            PIC X(3)  VALUE 'E  '.                       
001300        05 FILLER            PIC X(3)  VALUE 'F  '.                       
001500        05 FILLER            PIC X(3)  VALUE 'GB '.                       
001600        05 FILLER            PIC X(3)  VALUE 'GR '.                       
001610        05 FILLER            PIC X(3)  VALUE 'H  '.                       
001700        05 FILLER            PIC X(3)  VALUE 'I  '.                       
001710        05 FILLER            PIC X(3)  VALUE 'IR '.                       
001720        05 FILLER            PIC X(3)  VALUE 'J  '.                       
001740        05 FILLER            PIC X(3)  VALUE 'KOR'.                       
001760        05 FILLER            PIC X(3)  VALUE 'MAL'.                       
001900        05 FILLER            PIC X(3)  VALUE 'NL '.                       
002100        05 FILLER            PIC X(3)  VALUE 'P  '.                       
002110        05 FILLER            PIC X(3)  VALUE 'PL '.                       
002120        05 FILLER            PIC X(3)  VALUE 'RC '.                       
002130        05 FILLER            PIC X(3)  VALUE 'RCN'.                       
002131        05 FILLER            PIC X(3)  VALUE 'RO '.                       
002140        05 FILLER            PIC X(3)  VALUE 'RUS'.                       
002300        05 FILLER            PIC X(3)  VALUE 'S  '.                       
002500        05 FILLER            PIC X(3)  VALUE 'SF '.                       
002520        05 FILLER            PIC X(3)  VALUE 'T  '.                       
002540        05 FILLER            PIC X(3)  VALUE 'TR '.                       
002700        05 FILLER            PIC X(3)  VALUE 'USA'.                       
002701        05 FILLER            PIC X(3)  VALUE 'YU '.                       
002702                                                                          
002800     03 FILLER REDEFINES WWLAND06-IDSKYLTVARDE.                           
002900        05 WWLAND06-IDSKYLT-RAD                                           
003000                             OCCURS 26 TIMES                              
003100                             INDEXED WWLAND06-IX.                         
003200           07 WWLAND06-IDSKYLT                                            
003300                             PIC X(3).                                    
003400*                                 NATIONALITETSTECKEN                     
003500*** END COPY WWLAND06    LENGTH=53                                        
