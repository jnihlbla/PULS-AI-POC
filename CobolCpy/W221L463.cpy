000100 01  W221L463.                                                            
000200*                                 LƒNKAREA F÷R IMS-CALL F÷R PGM           
000300*                                 W22146 MOT ORDERING≈NGS                 
000400*                                 REGISTRET WDL8                          
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAS-ORDING          VALUE +463.                                  
000800*                                 ANROPSTYP       KDCALL-W221-002         
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 FLJANEJ-ANROP        PIC X.                                       
001200      88 ANROP-OK            VALUE 'J'.                                   
001300      88 ANROP-FEL           VALUE 'N'.                                   
001400*                                 JA/NEJ-FLAGGA F÷R R2XX                  
001500     03 TIAAAA               PIC 9(4).                                    
001600*                                 ≈RTAL (≈≈≈≈)                            
001700     03 IOAREA               OCCURS 36 TIMES.                             
001800        05 TIAAPP            PIC S9(5)           COMP-3.                  
001900*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
002000*                                 12 PER ≈R                               
002100        05 KVOI-PROG         PIC S9(7)           COMP-3.                  
002200*                                 ORDERING≈NG PROGNOSP≈VERKANDE           
002300        05 KVOI-DC           PIC S9(7)           COMP-3.                  
002400*                                 ORDERING≈NG I STYCK PER TIDSENH         
002500        05 KVOI-DIV          PIC S9(7)           COMP-3.                  
002600*                                 ORDERING≈NG DIVERSE OCH TPO             
002700        05 KVOI-SATS         PIC S9(7)           COMP-3.                  
002800*                                 ORDERING≈NG SATSF÷RBRUKNING             
002900*** END OF VILMAII-COPY LENGTH= 696 BYTES                                 
