000100 01  W27162.                                                              
000200*                                 LIGGER TILL GRUND FÖR                   
000300*                                 RESTORDERLISTOR                         
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 IDLANDX2             PIC X(2).                                    
000900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001000*                                 2-LETTER CODE FOR COUNTRY               
001100     03 DARODAT              PIC S9(9)           COMP-3.                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 BEART                PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700*                                 PART DESCRIPTION                        
001800     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001900*                                 FUNKTIONSGRUPP                          
002000*                                 FUNCTION GROUP                          
002100     03 FREEZECODE           PIC X.                                       
002200     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
002300*                                 PERSONKOD REFILLANSVARIG                
002400*                                 REFILL RESPONSIBLE ID                   
002500     03 KVBEART              PIC S9(7)           COMP-3.                  
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700*                                 ORDERED QUANTITY                        
002800     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
002900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003000*                                 AVERAGE COST FOREIGN CURRENCY           
003100     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
003200*                                 PERIODBEHOV REFILLING                   
003300*                                 FORECAST REFILLING                      
003400     03 ONHAND               PIC S9(7)           COMP-3.                  
003500     03 ANTAL-ORDRAD         PIC S9(7)           COMP-3.                  
003600     03 ANTAL-QTY            PIC S9(7)           COMP-3.                  
003700     03 KDERS                PIC S9(3)           COMP-3.                  
003800*                                 ERSÄTTNINGSKOD                          
003900*                                 SUPERSESSION CODE                       
004000     03 AVAIL-CDC            PIC S9(7)           COMP-3.                  
004100     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
004200*                                 DEL AV AK SOM LIGGER I SDC              
004300*                                 PART OF AK IN THE SDC                   
004400*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
