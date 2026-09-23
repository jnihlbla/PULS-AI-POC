000100 01  W330140.                                                             
000200     03 PERIOD-GRUPP.                                                     
000300        05 PERIOD-VARDEN     OCCURS 24 TIMES.                             
000400           07 SULEVANT       PIC S9(9)           COMP-3.                  
000500*                                 SUMMA LEVERERAT ANTAL                   
000600*                                 AV 1 ARTIKEL                            
000700           07 SUARTFSG       PIC S9(9)V9(2)      COMP-3.                  
000800*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
000900*                                                                         
001000           07 SUARTSJK       PIC S9(9)V9(2)      COMP-3.                  
001100*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
001200           07 SULEVANT-RAB   PIC S9(9)           COMP-3.                  
001300*                                 ANTAL LEV. ART TILL RABATT              
001400*                                 PER FKNGRP                              
001500           07 SUARTFSG-RAB   PIC S9(9)V9(2)      COMP-3.                  
001600*                                 SUMMA FSG/ART TILL RABATT               
001700*                                 PER FKNGRP                              
001800           07 SULEVANT-SPEC  PIC S9(9)           COMP-3.                  
001900*                                 ANTAL LEV. ART TILL                     
002000*                                 SPECIALPRIS                             
002100           07 SUARTFSG-SPEC  PIC S9(9)V9(2)      COMP-3.                  
002200*                                 SUMMA FSG/ARTIKEL TILL                  
002300*                                 SPECIALPRIS                             
002400           07 SULEVANT-MAN   PIC S9(9)           COMP-3.                  
002500*                                 ANTAL LEV. ART TILL                     
002600*                                 MANUELLT PRIS                           
002700           07 SUARTFSG-MAN   PIC S9(9)V9(2)      COMP-3.                  
002800*                                 SUMMA FSG/ARTIKEL TILL                  
002900*                                 PRIS I RAD                              
003000           07 SULEVANT-KRE   PIC S9(9)           COMP-3.                  
003100*                                 ANTAL LEV. ART. SOM                     
003200*                                 KREDITERATS                             
003300           07 SUARTFSG-KRE   PIC S9(9)V9(2)      COMP-3.                  
003400*                                 SUMMA KREDITERAD FSG/                   
003500*                                 ARTIKEL                                 
003600           07 SULEVANT-DO    PIC S9(9)           COMP-3.                  
003700*                                 ANTAL LEVERERADE ARTIKLAR               
003800*                                 PÅ DAGORDER                             
003900           07 SUARTFSG-DO    PIC S9(9)V9(2)      COMP-3.                  
004000*                                 SUMMA FSG/ARTIKEL PÅ                    
004100*                                 DAGORDER                                
004200*** END COPY W330140     LENGTH=1728                                      
