000100 01  W33071.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 URVALSPOSTER AV VA OCH VP TYP           
000400     03 001-GRUPP.                                                        
000500*                                 ARTIKELSTATISTIK                        
000600*                                 IDENTIFIERING AV URVAL                  
000700*                                 OBS DENNA GRUPP ANVÄNDS I               
000800*                                 FLERA COPYTEXTER                        
000900        05 IDUSER            PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100        05 DAREGDAT          PIC 9(8).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001300        05 TIREGTID          PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSTID                        
001500        05 IDFSGURV          PIC X(8).                                    
001600*                                 URVALS IDENTITET                        
001700        05 IDPTYP            PIC X(3).                                    
001800*                                 POSTTYP                                 
001900        05 IDGTYP            PIC S9              COMP-3.                  
002000*                                 GRUPPTYP                                
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 BEART-SVE            PIC X(25).                                   
002400*                                 SVENSK ARTIKELBENÄMNING                 
002500     03 002-GRUPP.                                                        
002600*                                 ARTIKELSTATISTIK                        
002700*                                 OBS DENNA GRUPP ANVÄNDS I               
002800*                                 FLERA COPYTEXTER                        
002900        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003000*                                 PRODUKTSLAG                             
003100        05 BEPRODSL          PIC X(15).                                   
003200*                                 PRODUKTSLAGSBENÄMNING                   
003300        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
003400*                                 FUNKTIONSGRUPP                          
003500        05 BEFKNGRP          PIC X(50).                                   
003600*                                 FUNKTIONSGRUPPSBENÄMNING                
003700        05 IDDISTR           PIC S9(5)           COMP-3.                  
003800*                                 DISTRIKTNUMMER                          
003900        05 IDKONCNR          PIC S9(3)           COMP-3.                  
004000*                                 KONCERNNUMMER                           
004100        05 KDMARK-BUDG       PIC S9(3)           COMP-3.                  
004200*                                 MARKNADSKOD BUDGET 96 MARKNADER         
004300        05 BEMARK-BUDG       PIC X(15).                                   
004400*                                 NAMN PÅ     BUDGET 96 MARKNADER         
004500     03 006-GRUPP.                                                        
004600*                                 ARTIKELSTATISTIK                        
004700*                                 OBS DENNA GRUPP ANVÄNDS I               
004800*                                 FLERA COPYTEXTER                        
004900        05 DAFSGVV           PIC 9(6).                                    
005000*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
005100        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
005200*                                 ARTIKELNS SJÄLVKOSTNAD                  
005300        05 SULEVANT          PIC S9(9)           COMP-3.                  
005400*                                 SUMMA LEVERERAT ANTAL                   
005500*                                 AV 1 ARTIKEL                            
005600        05 SUARTFSG          PIC S9(9)V9(2)      COMP-3.                  
005700*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
005800*                                                                         
005900*** END OF VILMAII-COPY LENGTH= 176 BYTES                                 
