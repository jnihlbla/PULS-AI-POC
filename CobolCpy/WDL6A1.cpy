000100 01  SEQA-WDL6A1.                                                         
000200*                                 INLEVERANS HISTORIK SDC                 
000300*                                 FAKTURAINGÅNG                           
000400*                                 ENDAST R30 OCH 310                      
000500*                                 FYSISK NYCKEL: WDL6A1KY                 
000600*                                  (IDFAKT, IDKUNDRF,                     
000700*                                  (IDKUNDNR, IDKOLLI,                    
000800*                                  (IDARTNR, DAINLEV)                     
000900*                                 SEC. NYCKEL: WDL6ASEQ                   
001000*                                  (IDFAKT, IDKUNDRF,                     
001100*                                  (IDKUNDNR, IDKOLLI)                    
001200*                                 SÖKBEGREPP IDDC                         
001300*                                            IDPTYP                       
001400     03 SEQA-IDFAKT          PIC S9(7)           COMP-3.                  
001500*                                 FAKTURANUMMER                           
001600*                                 INVOICE NO.                             
001700     03 SEQA-IDKUNDRF        PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900*                                 CUSTOMER REFERENCE (ORDER ID)           
002000     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300     03 SEQA-IDKOLLI         PIC S9(5)           COMP-3.                  
002400*                                 KOLLINUMMER                             
002500*                                 CASE NUMBER                             
002600     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900     03 SEQA-DAINLEV         PIC 9(16).                                   
003000*                                 INLEVERANS NUMMER                       
003100*                                 CONSIGNMENT IDENTITY                    
003200     03 SEQA-IDDC            PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400*                                 WAREHOUSE IDENTIFIER                    
003500     03 SEQA-IDPTYP          PIC X(3).                                    
003600*                                 POSTTYP                                 
003700*                                 RECORD TYPE                             
003800     03 SEQA-KDKOLLI         PIC X(8).                                    
003900*                                 KOLLIKOD                                
004000*                                 KOLLI CODE                              
004100*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
