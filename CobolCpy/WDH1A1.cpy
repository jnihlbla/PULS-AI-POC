000100 01  SEQA-WDH1A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDH1               
000300*                                 INVENTERINGSREGISTER                    
000400*                                 FYSISK-NYCKEL: WDH1A1KY                 
000500*                                  (IDDC + ADLAGOMR + ADGANG              
000600*                                  + ADPLATS + KDINVPRI + KDVVKL          
000700*                                  + IDARTNR + KDINVKAT                   
000800*                                  + TISEGKEY)                            
000900*                                 SEKUNDÄR NYCKEL: WDH1ASEQ               
001000*                                  (IDDC + ADLAGOMR + ADGANG              
001100*                                  + ADPLATS + KDINVPRI + KDVVKL)         
001200     03 SEQA-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 SEQA-ADART.                                                       
001600*                                 ARTIKELADRESS I LAGRET                  
001700*                                 PARTS-ADRESS                            
001800        05 SEQA-ADLAGOMR     PIC S9(3)           COMP-3.                  
001900*                                 LAGEROMRÅDE                             
002000*                                 AREA                                    
002100        05 SEQA-ADGANG       PIC S9(3)           COMP-3.                  
002200*                                 GÅNG                                    
002300*                                 AISLE                                   
002400        05 SEQA-ADPLATS      PIC S9(5)           COMP-3.                  
002500*                                 LAGERPLATSNUMMER                        
002600*                                 LOCATION                                
002700     03 SEQA-KDINVPRIO       PIC S9              COMP-3.                  
002800*                                 INVENTERING PRIORITET                   
002900*                                 STOCKTAKING PRIORITY                    
003000     03 SEQA-KDVVKL          PIC S9              COMP-3.                  
003100*                                 VOLYMVÄRDESKLASS                        
003200*                                 VOLUME VALUE CLASS                      
003300     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600     03 SEQA-KDINVKAT        PIC S9(3)           COMP-3.                  
003700*                                 INVENTERINGSKATEGORI                    
003800*                                 STOCKTAKING CATEGORY                    
003900     03 SEQA-TISEGKEY        PIC S9(9)           COMP-3.                  
004000*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
004100*                                 TECHNICAL SEGMENT KEY                   
004200     03 SEQA-FLINVBEH        PIC X.                                       
004300*                                 INV. ONLINE JUSTERING                   
004400*                                 STOCKTAKING ADJUSTMENT ONLINE           
004500     03 SEQA-FLINVSKR        PIC X.                                       
004600*                                 INVENTERINGSANMODAN UTSKRIVEN           
004700*                                 STOCKTAKING ORDER WRITTEN               
004800     03 SEQA-IDFKNGRP        PIC S9(5)           COMP-3.                  
004900*                                 FUNKTIONSGRUPP                          
005000*                                 FUNCTION GROUP                          
005100     03 SEQA-KDPRODSL        PIC S9(3)           COMP-3.                  
005200*                                 PRODUKTSLAG                             
005300*                                 PRODUCT GROUP                           
005400*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
