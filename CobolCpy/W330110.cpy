000100 01  W330110.                                                             
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 TIFSGVV              PIC S9(5)           COMP-3.                  
000700*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
001100*                                 ARTIKELNS SJÄLVKOSTNAD                  
001200     03 SULEVANT             PIC S9(9)           COMP-3.                  
001300*                                 SUMMA LEVERERAT ANTAL                   
001400*                                 AV 1 ARTIKEL                            
001500     03 SUARTFSG             PIC S9(9)V9(2)      COMP-3.                  
001600*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
001700*                                                                         
001800     03 SULEVANT-SPEC        PIC S9(9)           COMP-3.                  
001900*                                 ANTAL LEV. ART TILL                     
002000*                                 SPECIALPRIS                             
002100     03 SUARTFSG-SPEC        PIC S9(9)V9(2)      COMP-3.                  
002200*                                 SUMMA FSG/ARTIKEL TILL                  
002300*                                 SPECIALPRIS                             
002400     03 SULEVANT-RAB         PIC S9(9)           COMP-3.                  
002500*                                 ANTAL LEV. ART TILL RABATT              
002600*                                 PER FKNGRP                              
002700     03 SUARTFSG-RAB         PIC S9(9)V9(2)      COMP-3.                  
002800*                                 SUMMA FSG/ART TILL RABATT               
002900*                                 PER FKNGRP                              
003000     03 SULEVANT-KRE         PIC S9(9)           COMP-3.                  
003100*                                 ANTAL LEV. ART. SOM                     
003200*                                 KREDITERATS                             
003300     03 SUARTFSG-KRE         PIC S9(9)V9(2)      COMP-3.                  
003400*                                 SUMMA KREDITERAD FSG/                   
003500*                                 ARTIKEL                                 
003600     03 SULEVANT-MAN         PIC S9(9)           COMP-3.                  
003700*                                 ANTAL LEV. ART TILL                     
003800*                                 MANUELLT PRIS                           
003900     03 SUARTFSG-MAN         PIC S9(9)V9(2)      COMP-3.                  
004000*                                 SUMMA FSG/ARTIKEL TILL                  
004100*                                 PRIS I RAD                              
004200     03 SULEVANT-DO          PIC S9(9)           COMP-3.                  
004300*                                 ANTAL LEVERERADE ARTIKLAR               
004400*                                 PÅ DAGORDER                             
004500     03 SUARTFSG-DO          PIC S9(9)V9(2)      COMP-3.                  
004600*                                 SUMMA FSG/ARTIKEL PÅ                    
004700*                                 DAGORDER                                
004800*** END COPY W330110CC0  LENGTH=85                                        
