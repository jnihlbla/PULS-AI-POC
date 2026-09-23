000100 01  SEQA-WDQ4A1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ401             
000400*                                 FYSISK NYCKEL: WDQ4A1KY                 
000500*                                 (IDORDER,  IDARTNR,  IDLOPNR,           
000600*                                  IDDC, ADLAGOMR, ADGANG,                
000700*                                  ADPLATS)                               
000800*                                 SECONDARY NYCKEL: WDQ4ASEQ              
000900*                                 (IDORDER,  IDARTNR,  IDLOPNR)           
001000     03 SEQA-IDORDER         PIC S9(7)           COMP-3.                  
001100*                                 VOLVO PARTS ORDERNUMMER                 
001200*                                 VOLVO PARTS ORDER NUMBER                
001300     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 SEQA-IDLOPNR         PIC S9(3)           COMP-3.                  
001700*                                 LÖPNUMMER                               
001800*                                 SEQUENCE NUMBER                         
001900     03 SEQA-IDDC            PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 SEQA-ADLAGOMR        PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMRÅDE                             
002400*                                 AREA                                    
002500     03 SEQA-ADGANG          PIC S9(3)           COMP-3.                  
002600*                                 GÅNG                                    
002700*                                 AISLE                                   
002800     03 SEQA-ADPLATS         PIC S9(5)           COMP-3.                  
002900*                                 LAGERPLATSNUMMER                        
003000*                                 LOCATION                                
003100     03 SEQA-IDWDQ401        PIC X(20).                                   
003200*                                 NYCKEL TILL WDQ401                      
003300*                                 KEY TO WDQ401                           
003400*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
