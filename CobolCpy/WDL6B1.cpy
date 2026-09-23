000100 01  SEQB-WDL6B1.                                                         
000200*                                 INLEVERANS HISTORIK SDC                 
000300*                                 SEKUNDÄRT INDEX TILL WDL622             
000400*                                 IDDC-IDILIST INGÅNG                     
000500*                                 FYSISK NYCKEL: WDL6B1KY                 
000600*                                  (IDDC, IDILIST, ADART,                 
000700*                                  (IDARTNR, DAINLEV)                     
000800*                                 SEC. NYCKEL: WDL6BSEQ                   
000900*                                  (IDDC, IDILIST, ADART,                 
001000*                                  (IDARTNR, DAINLEV)                     
001100     03 SEQB-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQB-IDILIST         PIC 9(5).                                    
001500*                                 INLÄGGNINGSLISTEIDENTITET               
001600*                                 REPORTINGLIST-IDENTITY                  
001700     03 SEQB-ADART.                                                       
001800*                                 ARTIKELADRESS I LAGRET                  
001900*                                 PARTS-ADRESS                            
002000        05 SEQB-ADLAGOMR     PIC S9(3)           COMP-3.                  
002100*                                 LAGEROMRÅDE                             
002200*                                 AREA                                    
002300        05 SEQB-ADGANG       PIC S9(3)           COMP-3.                  
002400*                                 GÅNG                                    
002500*                                 AISLE                                   
002600        05 SEQB-ADPLATS      PIC S9(5)           COMP-3.                  
002700*                                 LAGERPLATSNUMMER                        
002800*                                 LOCATION                                
002900     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100*                                 PART NUMBER                             
003200     03 SEQB-DAINLEV         PIC 9(16).                                   
003300*                                 INLEVERANS NUMMER                       
003400*                                 CONSIGNMENT IDENTITY                    
003500*                                 (YYYYMMDD+HHMMSSTH)                     
003600*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
