000010*** EDIT ALLOWED                                                          
000100 01  FSGFAKT.                                                             
000200*                                 TABELL FAKTORER ONORMAL FÖR-            
000300*                                 SÄLJNING. ANVÄNDS FÖR ATT AV-           
000400*                                 GÖRA OM ONORMAL OI I NÅGON              
000500*                                 PERIOD VID PROGNOSBERÄKNING.            
000600*                                 (A * PROGNOS) + B                       
000700*                                                                         
000800     03 S1-PRISKLASSER.                                                   
000900        05 FILLER PIC X(15) VALUE '000001000 01510'.                      
001000        05 FILLER PIC X(15) VALUE '000003000 01205'.                      
001100        05 FILLER PIC X(15) VALUE '000010000 01004'.                      
001200        05 FILLER PIC X(15) VALUE '000030000 01003'.                      
001300        05 FILLER PIC X(15) VALUE '000100000 01002'.                      
001400        05 FILLER PIC X(15) VALUE '000300000 01002'.                      
001500        05 FILLER PIC X(15) VALUE '999999999 01001'.                      
002100                                                                          
002200     03 S2-PRISKLASSER.                                                   
002300        05 FILLER PIC X(15) VALUE '000001000 01510'.                      
002400        05 FILLER PIC X(15) VALUE '000003000 01205'.                      
002500        05 FILLER PIC X(15) VALUE '000010000 01004'.                      
002600        05 FILLER PIC X(15) VALUE '000030000 01003'.                      
002700        05 FILLER PIC X(15) VALUE '000100000 01002'.                      
002800        05 FILLER PIC X(15) VALUE '000300000 01002'.                      
002900        05 FILLER PIC X(15) VALUE '999999999 01001'.                      
002901                                                                          
002910 01  FILLER REDEFINES FSGFAKT.                                            
003000     03 S-FSGFAKT         OCCURS 2.                                       
003100        05 S-FAKT             OCCURS 7.                                   
003200           07 S-PRARTSTD-MAX  PIC 9(7)V9(2).                              
003300           07 FILLER          PIC X.                                      
003400           07 A               PIC 9(2)V9.                                 
003500           07 B               PIC 9(2).                                   
003510                                                                          
003600*** END COPY FSGFAKT     LENGTH=196                                       
