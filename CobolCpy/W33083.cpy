000100 01  W33083.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 SUMMERADE POSTER AV VP1-TYP             
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
002100     03 002-GRUPP.                                                        
002200*                                 ARTIKELSTATISTIK                        
002300*                                 OBS DENNA GRUPP ANVÄNDS I               
002400*                                 FLERA COPYTEXTER                        
002500        05 KDPRODSL          PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700        05 BEPRODSL          PIC X(15).                                   
002800*                                 PRODUKTSLAGSBENÄMNING                   
002900        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
003000*                                 FUNKTIONSGRUPP                          
003100        05 BEFKNGRP          PIC X(50).                                   
003200*                                 FUNKTIONSGRUPPSBENÄMNING                
003300        05 IDDISTR           PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500        05 IDKONCNR          PIC S9(3)           COMP-3.                  
003600*                                 KONCERNNUMMER                           
003700        05 KDMARK-BUDG       PIC S9(3)           COMP-3.                  
003800*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003900        05 BEMARK-BUDG       PIC X(15).                                   
004000*                                 NAMN PÅ     BUDGET 96 MARKNADER         
004100     03 007-GRUPP.                                                        
004200*                                 ARTIKELSTATISTIK VECKO-URVAL            
004300*                                  VV=VALT INTERVALL INNEV. ÅR            
004400*                                 FVV=VALT INTERVALL FÖREG. ÅR            
004500*                                 OBS DENNA GRUPP ANVÄNDS I               
004600*                                 FLERA COPYTEXTER                        
004700        05 SULEVANT-VV       PIC S9(9)           COMP-3.                  
004800*                                 SUMMA LEVERERAT ANTAL                   
004900*                                 AV 1 ARTIKEL                            
005000        05 SUARTFSG-VV       PIC S9(9)V9(2)      COMP-3.                  
005100*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
005200*                                                                         
005300        05 RETOTBV-VV        PIC S9(2)V9(1)      COMP-3.                  
005400*                                 BRUTTOVINSTPROCENT                      
005500        05 SUARTSJK-VV       PIC S9(9)V9(2)      COMP-3.                  
005600*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
005700        05 SULEVANT-FVV      PIC S9(9)           COMP-3.                  
005800*                                 SUMMA LEVERERAT ANTAL                   
005900*                                 AV 1 ARTIKEL                            
006000        05 SUARTFSG-FVV      PIC S9(9)V9(2)      COMP-3.                  
006100*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
006200*                                                                         
006300        05 RETOTBV-FVV       PIC S9(2)V9(1)      COMP-3.                  
006400*                                 BRUTTOVINSTPROCENT                      
006500        05 SUARTSJK-FVV      PIC S9(9)V9(2)      COMP-3.                  
006600*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
006700*** END OF VILMAII-COPY LENGTH= 162 BYTES                                 
