000100 01  W33096.                                                              
000200*                                 ARTIKELSTATISTIK  - ÅRSPOST             
000300*                                 FÖRSÄLJNING SUMMERAD PÅ                 
000400*                                 ARTIKEL/DISTRIKT/ÅR                     
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 TIAA                 PIC S9(3)           COMP-3.                  
001000*                                 ÅR    (ÅÅ)                              
001100     03 SUARTSJK             PIC S9(9)V9(2)      COMP-3.                  
001200*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
001300     03 SULEVANT             PIC S9(9)           COMP-3.                  
001400*                                 SUMMA LEVERERAT ANTAL                   
001500*                                 AV 1 ARTIKEL                            
001600     03 SUARTFSG             PIC S9(9)V9(2)      COMP-3.                  
001700*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
001800*                                                                         
001900     03 SULEVANT-DO          PIC S9(9)           COMP-3.                  
002000*                                 ANTAL LEVERERADE ARTIKLAR               
002100*                                 PÅ DAGORDER                             
002200     03 SUARTFSG-DO          PIC S9(9)V9(2)      COMP-3.                  
002300*                                 SUMMA FSG/ARTIKEL PÅ                    
002400*                                 DAGORDER                                
002500     03 SULEVANT-RAB         PIC S9(9)           COMP-3.                  
002600*                                 ANTAL LEV. ART TILL RABATT              
002700*                                 PER FKNGRP                              
002800     03 SUARTFSG-RAB         PIC S9(9)V9(2)      COMP-3.                  
002900*                                 SUMMA FSG/ART TILL RABATT               
003000*                                 PER FKNGRP                              
003100     03 SULEVANT-SPEC        PIC S9(9)           COMP-3.                  
003200*                                 ANTAL LEV. ART TILL                     
003300*                                 SPECIALPRIS                             
003400     03 SUARTFSG-SPEC        PIC S9(9)V9(2)      COMP-3.                  
003500*                                 SUMMA FSG/ARTIKEL TILL                  
003600*                                 SPECIALPRIS                             
003700     03 SULEVANT-MAN         PIC S9(9)           COMP-3.                  
003800*                                 ANTAL LEV. ART TILL                     
003900*                                 MANUELLT PRIS                           
004000     03 SUARTFSG-MAN         PIC S9(9)V9(2)      COMP-3.                  
004100*                                 SUMMA FSG/ARTIKEL TILL                  
004200*                                 PRIS I RAD                              
004300     03 SULEVANT-KRE         PIC S9(9)           COMP-3.                  
004400*                                 ANTAL LEV. ART. SOM                     
004500*                                 KREDITERATS                             
004600     03 SUARTFSG-KRE         PIC S9(9)V9(2)      COMP-3.                  
004700*                                 SUMMA KREDITERAD FSG/                   
004800*                                 ARTIKEL                                 
004900*** END COPY W33096CCC0  LENGTH=82                                        
