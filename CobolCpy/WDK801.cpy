000100 01  ART-WDK801.                                                          
000200*                                 DSP/VR ARTIKEL-INFORMATION              
000300*                                 FYSISK NYCKEL: WDK801KY                 
000400*                                 (IDARTNR + KDSEGKEY + KDVRMGRP          
000500*                                  + IDKUNDNR)                            
000600     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 ART-KDSEGKEY         PIC X.                                       
001000*                                 TEKNISK SEGMENT-NYCKEL                  
001100*                                 TECHNICAL SEGMENT KEY                   
001200     03 ART-IDAIGRP.                                                      
001300*                                 ARTIKEL/LAGERINFO GRUPP                 
001400*                                 PARTS LOCATOR GROUP                     
001500        05 ART-IDLANDX2      PIC X(2).                                    
001600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001700*                                 2-LETTER CODE FOR COUNTRY               
001800        05 ART-KDAIKTYP      PIC X.                                       
001900*                                 KUNDTYP ARTIKEL/LAGER DSP/VR            
002000*                                 CUSTOMERTYPE PARTS LOCATOR              
002100     03 ART-IDKUNDNR         PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400     03 ART-KVBEST           PIC S9(7)           COMP-3.                  
002500*                                 BESTÄLLT ANTAL                          
002600     03 ART-KVLS             PIC S9(7)           COMP-3.                  
002700*                                 LAGERSALDO                              
002800*                                 STOCK BALANCE                           
002900     03 ART-KVROS            PIC S9(7)           COMP-3.                  
003000*                                 RESTORDERSALDO                          
003100*                                 BACKORDER QTY                           
003200     03 ART-KVLEVART         OCCURS 2 TIMES                               
003300                             PIC S9(7)           COMP-3.                  
003400*                                 LEVERERAT ANTAL STYCK                   
003500*                                 DELIVERED QUANTITY                      
003600     03 ART-KVLEVART-INNEV   PIC S9(7)           COMP-3.                  
003700*                                 LEVERERAT ANTAL ARTIKLAR                
003800*                                 INNEVARANDE ÅR                          
003900*                                 NUMBER OF DELIVERED QTY                 
004000*                                 THIS YEAR                               
004100     03 ART-KVLEVART-FOREG   PIC S9(7)           COMP-3.                  
004200*                                 LEVERERAT ANTAL ARTIKLAR                
004300*                                 FÖREGÅENDE ÅR                           
004400*                                 NUMBER OF DELIVERED QTY                 
004500*                                 LAST YEAR                               
004600     03 ART-KVLEVART-F-FOREG PIC S9(7)           COMP-3.                  
004700*                                 LEVERERAT ANTAL ARTIKLAR                
004800*                                 FÖRRFÖRRA ÅRET                          
004900*                                 NUMBER OF DELIVERED QTY                 
005000*                                 YEAR MINUS 2                            
005100     03 ART-KVVRPROGN        PIC S9(7)V9(2)      COMP-3.                  
005200*                                 FÖRSÄLJNINGSPROGNOS I VR                
005300*                                 SALES FORECAST IN VR                    
005400     03 ART-KDSHELFL         PIC 9(2).                                    
005500*                                 HYLLVÄRMARKOD DSP/VR                    
005600*                                 SHELF LIFE CODE DSP/VR                  
005700*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
