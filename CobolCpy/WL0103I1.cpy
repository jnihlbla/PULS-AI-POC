000100 01  REQU-WL0103I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0103             
000300*                                 LDC BINNING                             
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDFAKT-KEY      PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 REQU-IDKUNDRF-KEY    PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200*                                 CUSTOMER REFERENCE (ORDER ID)           
001300     03 REQU-IDKUNDNR-KEY    PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 REQU-IDKOLLI-KEY     PIC X(5).                                    
001700*                                 KOLLINUMMER                             
001800*                                 CASE NUMBER                             
001900     03 REQU-KOLLI-KLART     PIC X.                                       
002000     03 REQU-IDUSER-003      PIC X(5).                                    
002100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
002200     03 REQU-NEW-PART.                                                    
002300*                                                                         
002400        05 REQU-CMD-INM      PIC X(3).                                    
002500        05 REQU-KVANTMOT-INM PIC X(6).                                    
002600*                                 ANTAL MOTTAGET                          
002700*                                 QUANTITY RECEIVED                       
002800        05 REQU-IDARTNR-INM  PIC X(9).                                    
002900*                                 ARTIKELNUMMER                           
003000*                                 PART NUMBER                             
003100        05 REQU-ADLAGOMR-INM PIC X(2).                                    
003200*                                 LAGEROMRÅDE                             
003300*                                 AREA                                    
003400        05 REQU-ADGANG-INM   PIC X(2).                                    
003500*                                 GÅNG                                    
003600*                                 AISLE                                   
003700        05 REQU-ADPLATS-INM  PIC X(5).                                    
003800*                                 LAGERPLATSNUMMER                        
003900*                                 LOCATION                                
004000        05 REQU-KVSKROT-INM  PIC X(7).                                    
004100*                                 ANTAL SENASTE SKROTORDER                
004200*                                 QUANTITY LAST SCRAPORDER                
004300     03 REQU-KVRADER-MAX1    PIC 9(5).                                    
004400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
004500*                                 EDAN.                                   
004600     03 REQU-IDDISTR-KEY     PIC 9(5).                                    
004700*                                 DISTRIKTNUMMER                          
004800*                                 DISTRICT NUMBER                         
004900     03 REQU-INPUT           OCCURS 1100 TIMES.                           
005000*                                                                         
005100        05 REQU-CMD-IN       PIC X(3).                                    
005200        05 REQU-KVANTMOT-IN  PIC X(6).                                    
005300*                                 ANTAL MOTTAGET                          
005400*                                 QUANTITY RECEIVED                       
005500        05 REQU-IDARTNR      PIC 9(9).                                    
005600*                                 ARTIKELNUMMER                           
005700*                                 PART NUMBER                             
005800        05 REQU-ADLAGOMR     PIC X(2).                                    
005900*                                 LAGEROMRÅDE                             
006000*                                 AREA                                    
006100        05 REQU-ADGANG       PIC X(2).                                    
006200*                                 GÅNG                                    
006300*                                 AISLE                                   
006400        05 REQU-ADPLATS      PIC X(5).                                    
006500*                                 LAGERPLATSNUMMER                        
006600*                                 LOCATION                                
006700        05 REQU-KVSKROT-IN   PIC X(7).                                    
006800*                                 ANTAL SENASTE SKROTORDER                
006900*                                 QUANTITY LAST SCRAPORDER                
007000        05 REQU-DAINLEV      PIC 9(16).                                   
007100*                                 INLEVERANS NUMMER                       
007200*                                 CONSIGNMENT IDENTITY                    
007300*                                 (YYYYMMDD+HHMMSSTH)                     
007400*** END OF VILMAII-COPY LENGTH= 55080 BYTES                               
