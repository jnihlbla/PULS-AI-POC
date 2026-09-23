000100 01  W33077.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 URVALSPOSTER AV VA2-TYP                 
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
002100     03 003-GRUPP.                                                        
002200*                                 ARTIKELSTATISTIK                        
002300*                                 OBS DENNA GRUPP ANVÄNDS I               
002400*                                 FLERA COPYTEXTER                        
002500        05 IDARTNR           PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700        05 BEART-SVE         PIC X(25).                                   
002800*                                 SVENSK ARTIKELBENÄMNING                 
002900        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003000*                                 PRODUKTSLAG                             
003100        05 BEPRODSL          PIC X(15).                                   
003200*                                 PRODUKTSLAGSBENÄMNING                   
003300        05 IDDISTR           PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500        05 IDKONCNR          PIC S9(3)           COMP-3.                  
003600*                                 KONCERNNUMMER                           
003700        05 KDMARK-BUDG       PIC S9(3)           COMP-3.                  
003800*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003900        05 BEMARK-BUDG       PIC X(15).                                   
004000*                                 NAMN PÅ     BUDGET 96 MARKNADER         
004100     03 006-GRUPP.                                                        
004200*                                 ARTIKELSTATISTIK                        
004300*                                 OBS DENNA GRUPP ANVÄNDS I               
004400*                                 FLERA COPYTEXTER                        
004500        05 DAFSGVV           PIC 9(6).                                    
004600*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
004700        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
004800*                                 ARTIKELNS SJÄLVKOSTNAD                  
004900        05 SULEVANT          PIC S9(9)           COMP-3.                  
005000*                                 SUMMA LEVERERAT ANTAL                   
005100*                                 AV 1 ARTIKEL                            
005200        05 SUARTFSG          PIC S9(9)V9(2)      COMP-3.                  
005300*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
005400*                                                                         
005500*** END OF VILMAII-COPY LENGTH= 123 BYTES                                 
