000100 01  W33075.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 SUMMERADE POSTER AV VA1-TYP             
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
003300     03 007-GRUPP.                                                        
003400*                                 ARTIKELSTATISTIK VECKO-URVAL            
003500*                                  VV=VALT INTERVALL INNEV. ÅR            
003600*                                 FVV=VALT INTERVALL FÖREG. ÅR            
003700*                                 OBS DENNA GRUPP ANVÄNDS I               
003800*                                 FLERA COPYTEXTER                        
003900        05 SULEVANT-VV       PIC S9(9)           COMP-3.                  
004000*                                 SUMMA LEVERERAT ANTAL                   
004100*                                 AV 1 ARTIKEL                            
004200        05 SUARTFSG-VV       PIC S9(9)V9(2)      COMP-3.                  
004300*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
004400*                                                                         
004500        05 RETOTBV-VV        PIC S9(2)V9(1)      COMP-3.                  
004600*                                 BRUTTOVINSTPROCENT                      
004700        05 SUARTSJK-VV       PIC S9(9)V9(2)      COMP-3.                  
004800*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
004900        05 SULEVANT-FVV      PIC S9(9)           COMP-3.                  
005000*                                 SUMMA LEVERERAT ANTAL                   
005100*                                 AV 1 ARTIKEL                            
005200        05 SUARTFSG-FVV      PIC S9(9)V9(2)      COMP-3.                  
005300*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
005400*                                                                         
005500        05 RETOTBV-FVV       PIC S9(2)V9(1)      COMP-3.                  
005600*                                 BRUTTOVINSTPROCENT                      
005700        05 SUARTSJK-FVV      PIC S9(9)V9(2)      COMP-3.                  
005800*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
005900*** END OF VILMAII-COPY LENGTH= 170 BYTES                                 
