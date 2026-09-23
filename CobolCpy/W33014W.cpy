000100 01  W33014W.                                                             
000200     03 IDARTNR              PIC S9(9)           COMP-3.                  
000300*                                 ARTIKELNUMMER                           
000400     03 DISTR-RAD            OCCURS 1600 TIMES.                           
000500        05 IDDISTR           PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700        05 IDKONCNR          PIC S9(3)           COMP-3.                  
000800*                                 KONCERNNUMMER                           
000900        05 KDMARK-BUDG       PIC S9(3)           COMP-3.                  
001000*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001100        05 BEMARK-BUDG       PIC X(15).                                   
001200*                                 NAMN PÅ     BUDGET 96 MARKNADER         
001300        05 PERIOD-GRUPP.                                                  
001400           07 PERIOD-VARDEN  OCCURS 24 TIMES.                             
001500              09 SULEVANT    PIC S9(9)           COMP-3.                  
001600*                                 SUMMA LEVERERAT ANTAL                   
001700*                                 AV 1 ARTIKEL                            
001800              09 SUARTFSG    PIC S9(9)V9(2)      COMP-3.                  
001900*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
002000*                                                                         
002100              09 SUARTSJK    PIC S9(9)V9(2)      COMP-3.                  
002200*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
002300              09 SULEVANT-RAB                                             
002400                             PIC S9(9)           COMP-3.                  
002500*                                 ANTAL LEV. ART TILL RABATT              
002600*                                 PER FKNGRP                              
002700              09 SUARTFSG-RAB                                             
002800                             PIC S9(9)V9(2)      COMP-3.                  
002900*                                 SUMMA FSG/ART TILL RABATT               
003000*                                 PER FKNGRP                              
003100              09 SULEVANT-SPEC                                            
003200                             PIC S9(9)           COMP-3.                  
003300*                                 ANTAL LEV. ART TILL                     
003400*                                 SPECIALPRIS                             
003500              09 SUARTFSG-SPEC                                            
003600                             PIC S9(9)V9(2)      COMP-3.                  
003700*                                 SUMMA FSG/ARTIKEL TILL                  
003800*                                 SPECIALPRIS                             
003900              09 SULEVANT-MAN                                             
004000                             PIC S9(9)           COMP-3.                  
004100*                                 ANTAL LEV. ART TILL                     
004200*                                 MANUELLT PRIS                           
004300              09 SUARTFSG-MAN                                             
004400                             PIC S9(9)V9(2)      COMP-3.                  
004500*                                 SUMMA FSG/ARTIKEL TILL                  
004600*                                 PRIS I RAD                              
004700              09 SULEVANT-KRE                                             
004800                             PIC S9(9)           COMP-3.                  
004900*                                 ANTAL LEV. ART. SOM                     
005000*                                 KREDITERATS                             
005100              09 SUARTFSG-KRE                                             
005200                             PIC S9(9)V9(2)      COMP-3.                  
005300*                                 SUMMA KREDITERAD FSG/                   
005400*                                 ARTIKEL                                 
005500              09 SULEVANT-DO PIC S9(9)           COMP-3.                  
005600*                                 ANTAL LEVERERADE ARTIKLAR               
005700*                                 PÅ DAGORDER                             
005800              09 SUARTFSG-DO PIC S9(9)V9(2)      COMP-3.                  
005900*                                 SUMMA FSG/ARTIKEL PÅ                    
006000*                                 DAGORDER                                
006100*** END OF VILMAII-COPY LENGTH= 2800005 BYTES                             
