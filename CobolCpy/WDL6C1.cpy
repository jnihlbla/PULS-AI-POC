000100 01  SEQC-WDL6C1.                                                         
000200*                                 INLEVERANS HISTORIK XDC                 
000300*                                 FAKTURAINGÅNG                           
000400*                                 ENDAST R32                              
000500*                                 FYSISK NYCKEL: WDL6C1KY                 
000600*                                  (IDFAKT, IDDC,                         
000700*                                  (IDARTNR, DAINLEV)                     
000800*                                 SEC. NYCKEL: WDL6CSEQ                   
000900*                                  (IDFAKT, IDDC)                         
001000     03 SEQC-IDFAKT          PIC S9(7)           COMP-3.                  
001100*                                 FAKTURANUMMER                           
001200*                                 INVOICE NO.                             
001300     03 SEQC-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 SEQC-DAINLEV         PIC 9(16).                                   
002000*                                 INLEVERANS NUMMER                       
002100*                                 CONSIGNMENT IDENTITY                    
002200*                                 (YYYYMMDD+HHMMSSTH)                     
002300*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
