000100 01  W33073.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 URVALSPOSTER AV VA1 TYP                 
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
002500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700     03 BEPRODSL             PIC X(15).                                   
002800*                                 PRODUKTSLAGSBENÄMNING                   
002900     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003000*                                 FUNKTIONSGRUPP                          
003100     03 BEFKNGRP             PIC X(50).                                   
003200*                                 FUNKTIONSGRUPPSBENÄMNING                
003300     03 006-GRUPP.                                                        
003400*                                 ARTIKELSTATISTIK                        
003500*                                 OBS DENNA GRUPP ANVÄNDS I               
003600*                                 FLERA COPYTEXTER                        
003700        05 DAFSGVV           PIC 9(6).                                    
003800*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
003900        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
004000*                                 ARTIKELNS SJÄLVKOSTNAD                  
004100        05 SULEVANT          PIC S9(9)           COMP-3.                  
004200*                                 SUMMA LEVERERAT ANTAL                   
004300*                                 AV 1 ARTIKEL                            
004400        05 SUARTFSG          PIC S9(9)V9(2)      COMP-3.                  
004500*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
004600*                                                                         
004700*** END OF VILMAII-COPY LENGTH= 154 BYTES                                 
