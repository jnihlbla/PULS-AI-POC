000100 01  W37164.                                                              
000200*                                 UPPDATERINGSPOSTER ETC. - BYTES         
000300*                                 POÄNG                                   
000400*                                 UPDATE RECORDS ETC. - EXCHANGE-         
000500*                                 POINT                                   
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001000*                                 FUNKTIONSGRUPP                          
001100*                                 FUNCTION GROUP                          
001200     03 DAXPOINT             PIC 9(8).                                    
001300*                                 POÄNGÄNDRINGSDATUM (ÅÅÅÅMMDD)           
001400*                                 POINT CHANGE DATE (YYYYMMDD)            
001500     03 KDBEH                PIC X.                                       
001600*                                 BEHANDLINGSKOD                          
001700     03 KDEXCHA              PIC S9(3)           COMP-3.                  
001800*                                 EXCHANGE ACCOUNT CODE                   
001900     03 KVPOINT              PIC S9(7)           COMP-3.                  
002000*                                 POINT VALUE                             
002100     03 KVLS                 PIC S9(7)           COMP-3.                  
002200*                                 LAGERSALDO                              
002300*                                 STOCK BALANCE                           
002400     03 PRREF                PIC S9(7)V9(2)      COMP-3.                  
002500*                                 REFERENCE PRICE                         
002600     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
002700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
002800*                                 GROSS SALES PRICE (SEK)                 
002900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 ARTIKELNS SJÄLVKOSTNAD                  
003100*                                 COST OF SALES                           
003200*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
