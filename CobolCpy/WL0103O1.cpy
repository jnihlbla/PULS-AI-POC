000100 01  RESP-WL0103O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0103         
000300*                                 LDC BINNING                             
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-IDFAKT-KEY      PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 RESP-IDKUNDRF-KEY    PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200*                                 CUSTOMER REFERENCE (ORDER ID)           
001300     03 RESP-IDKUNDNR-KEY    PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 RESP-IDKOLLI-KEY     PIC X(5).                                    
001700*                                 KOLLINUMMER                             
001800*                                 CASE NUMBER                             
001900     03 RESP-KOLLI-KLART     PIC X.                                       
002000     03 RESP-IDUSER-003      PIC X(5).                                    
002100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
002200     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
002300*                                 MAX INDEX KOPPLAT TILL OCCURS N         
002400*                                 EDAN.                                   
002500     03 RESP-NEW-PART.                                                    
002600*                                 GRUPP MED NEW PART INFO                 
002700        05 RESP-KVANTMOT-INM PIC X(6).                                    
002800*                                 ANTAL MOTTAGET                          
002900*                                 QUANTITY RECEIVED                       
003000        05 RESP-IDARTNR-INM  PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200*                                 PART NUMBER                             
003300        05 RESP-ADLAGOMR-INM PIC X(2).                                    
003400*                                 LAGEROMRÅDE                             
003500*                                 AREA                                    
003600        05 RESP-ADGANG-INM   PIC X(2).                                    
003700*                                 GÅNG                                    
003800*                                 AISLE                                   
003900        05 RESP-ADPLATS-INM  PIC X(5).                                    
004000*                                 LAGERPLATSNUMMER                        
004100*                                 LOCATION                                
004200        05 RESP-CMD-INM      PIC X(3).                                    
004300        05 RESP-KVSKROT-INM  PIC X(7).                                    
004400*                                 ANTAL SENASTE SKROTORDER                
004500*                                 QUANTITY LAST SCRAPORDER                
004600        05 RESP-IDMSG-ERROR-INM                                           
004700                             PIC X(3).                                    
004800*                                 FELMEDDELANDE ID                        
004900*                                 ERROR MESSAGE ID                        
005000     03 RESP-TABELLRAD       OCCURS 1 TO 1100 TIMES                       
005100                             DEPENDING ON RESP-KVRADER-MAX1.              
005200*                                 GRUPP MED TABELLRADER                   
005300        05 RESP-CMD-IN       PIC X(3).                                    
005400        05 RESP-KVANTMOT-IN  PIC X(6).                                    
005500*                                 ANTAL MOTTAGET                          
005600*                                 QUANTITY RECEIVED                       
005700        05 RESP-ADLAGOMR     PIC 9(2).                                    
005800*                                 LAGEROMRÅDE                             
005900*                                 AREA                                    
006000        05 RESP-ADGANG       PIC 9(2).                                    
006100*                                 GÅNG                                    
006200*                                 AISLE                                   
006300        05 RESP-ADPLATS      PIC 9(5).                                    
006400*                                 LAGERPLATSNUMMER                        
006500*                                 LOCATION                                
006600        05 RESP-KVSKROT-IN   PIC X(7).                                    
006700*                                 ANTAL SENASTE SKROTORDER                
006800*                                 QUANTITY LAST SCRAPORDER                
006900        05 RESP-IDMSG-ERROR-LINE                                          
007000                             PIC X(3).                                    
007100*                                 FELMEDDELANDE ID                        
007200*                                 ERROR MESSAGE ID                        
007300        05 RESP-KVAVIS       PIC Z(5)9.                                   
007400*                                 AVISERAT ANTAL                          
007500*                                 QUANTITY NOTIFIED                       
007600        05 RESP-IDARTNR      PIC Z(8)9.                                   
007700*                                 ARTIKELNUMMER                           
007800*                                 PART NUMBER                             
007900        05 RESP-BEART        PIC X(100).                                  
008000*                                 ARTIKELBENÄMNING                        
008100*                                 PART DESCRIPTION                        
008200        05 RESP-KDPRIO       PIC X.                                       
008300*                                 PRIORITETSKOD                           
008400*                                 PRIORITY CODE                           
008500        05 RESP-DAINLEV      PIC 9(16).                                   
008600*                                 INLEVERANS NUMMER                       
008700*                                 CONSIGNMENT IDENTITY                    
008800*                                 (YYYYMMDD+HHMMSSTH)                     
008900        05 RESP-TEKVAINF-EXT OCCURS 7 TIMES                               
009000                             PIC X(79).                                   
009100*                                 KVALITETS INFORMATION EXTERNT           
009200*                                 QUALITY INFORMATION PART NUMBER         
009300*                                  EXTERNAL                               
009400        05 RESP-FLFARLIG     PIC X.                                       
009500*                                 FARLIGT GODS-FLAGGA                     
009600*                                 DENGEROUS GOODS FLAG                    
009700*** END OF VILMAII-COPY LENGTH= 785478 BYTES                              
