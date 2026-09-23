000100 01  SEQD-WDL6D1.                                                         
000200*                                 INLEVERANS HISTORIK XDC                 
000300*                                 KOLLI INGÅNG (EJ R32)                   
000400*                                 FYSISK NYCKEL: WDL6D1KY                 
000500*                                  (IDDC,IDDISTR,IDKUNDNR,                
000600*                                   IDKUNDRF, IDKOLLI,                    
000700*                                   IDARTNR, DAINLEV)                     
000800*                                 SEC. NYCKEL: WDL6DSEQ                   
000900*                                  (IDDC,IDDISTR,IDKUNDNR,                
001000*                                   IDKUNDRF, IDKOLLI)                    
001100     03 SEQD-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQD-IDDISTR         PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700     03 SEQD-IDKUNDNR        PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000     03 SEQD-IDKUNDRF        PIC X(10).                                   
002100*                                 KUNDENS REFERENS (ORDERID)              
002200*                                 CUSTOMER REFERENCE (ORDER ID)           
002300     03 SEQD-IDKOLLI         PIC S9(5)           COMP-3.                  
002400*                                 KOLLINUMMER                             
002500*                                 CASE NUMBER                             
002600     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900     03 SEQD-DAINLEV         PIC 9(16).                                   
003000*                                 INLEVERANS NUMMER                       
003100*                                 CONSIGNMENT IDENTITY                    
003200*                                 (YYYYMMDD+HHMMSSTH)                     
003300     03 SEQD-IDFAKT          PIC S9(7)           COMP-3.                  
003400*                                 FAKTURANUMMER                           
003500*                                 INVOICE NO.                             
003600*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
