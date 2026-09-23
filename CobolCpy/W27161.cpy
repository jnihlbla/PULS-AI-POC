000100 01  W27161.                                                              
000200*                                 LIGGER TILL GRUND FÖR                   
000300*                                 RESTORDERLISTOR                         
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 IDLANDX2             PIC X(2).                                    
000900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001000*                                 2-LETTER CODE FOR COUNTRY               
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 BEART                PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600*                                 PART DESCRIPTION                        
001700     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001800*                                 FUNKTIONSGRUPP                          
001900*                                 FUNCTION GROUP                          
002000     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002200*                                 ORDERED QUANTITY ADAPTED                
002300*                                  ITEMS                                  
002400     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
002500*                                 PERSONKOD REFILLANSVARIG                
002600*                                 REFILL RESPONSIBLE ID                   
002700     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
002800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002900*                                 AVERAGE COST FOREIGN CURRENCY           
003000     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
003100*                                 PERIODBEHOV REFILLING                   
003200*                                 FORECAST REFILLING                      
003300     03 ONHAND               PIC S9(7)           COMP-3.                  
003400     03 KVBEART              PIC S9(7)           COMP-3.                  
003500*                                 BESTÄLLT ANTAL STYCKEN                  
003600*                                 ORDERED QUANTITY                        
003700     03 DARODAT              PIC S9(9)           COMP-3.                  
003800     03 FREEZECODE           PIC X.                                       
003900     03 KDERS                PIC S9(3)           COMP-3.                  
004000*                                 ERSÄTTNINGSKOD                          
004100*                                 SUPERSESSION CODE                       
004200     03 AVAIL-CDC            PIC S9(7)           COMP-3.                  
004300     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
004400*                                 DEL AV AK SOM LIGGER I SDC              
004500*                                 PART OF AK IN THE SDC                   
004600*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
