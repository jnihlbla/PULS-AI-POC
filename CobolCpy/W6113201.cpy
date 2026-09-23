000100 01  W6113201.                                                            
000200*                                 INFO OM KOMMANDE GODS CDC(DC11)         
000300*                                 FÖR ANVÄNDNING VI PRODUKTIONS-          
000400*                                 PLANERING, URVAL FRÅN LEVERANS-         
000500*                                 PLAN KOMPLETERAT MED                    
000600*                                 AVISERINGAR, TILL VIOS                  
000700     03 ADINLOMR             PIC X(4).                                    
000800*                                 INLEVERANSOMRÅDE                        
000900*                                 RECEIVING AREA                          
001000     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001100*                                 LAGEROMRÅDE                             
001200*                                 AREA                                    
001300     03 BEFT                 PIC S9(3)           COMP-3.                  
001400*                                 FÖRPACKNINGSTYP                         
001500*                                 PACKAGING TYPE                          
001600     03 FLFORAVI             PIC X.                                       
001700*                                 FÖRAVISERAD INLEVERANS                  
001800*                                 PREADVICE DELIVERY                      
001900     03 IDARTNR              PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 IDLEVNR              PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
002600*                                 FUNKTIONSGRUPP                          
002700*                                 FUNCTION GROUP                          
002800     03 KVINLART             PIC S9(7)           COMP-3.                  
002900*                                 ANTAL I PARTIRAD                        
003000*                                 QTY/LINE IN A LOT                       
003100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELSTANDARDPRIS                     
003300*                                 STANDARD PRICE                          
003400     03 VKART                PIC S9(7)           COMP-3.                  
003500*                                 ARTIKELVIKT (G)                         
003600*                                 PART WEIGHT (G)                         
003700     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003800*                                 ARTIKELVOLYM NETTO (CM3)                
003900*                                 PART NET VOLUME    (CM3)                
004000     03 TIANKVV              PIC S9(5)           COMP-3.                  
004100*                                 ÅR - VECKA  (ÅÅVV)                      
004200*                                 YEAR - WEEK  (YYWW)                     
004300     03 IDDC                 PIC X(2).                                    
004400*                                 IDENTIFIERARE LAGER                     
004500*                                 WAREHOUSE IDENTIFIER                    
004600*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
