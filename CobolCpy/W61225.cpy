000100 01  W61225.                                                              
000200*                                 INLEV FAKTURAINFO TILL DATALAKE         
000300*                                 WDGX6302 SEGMENTET PÅ WDR5              
000400     03 IDDC-REC             PIC X(2).                                    
000500*                                 MOTTAGANDE LAGER                        
000600*                                 RECEIVING WAREHOUSE                     
000700     03 DABERANK             PIC 9(8).                                    
000800*                                 BERÄKNAD ANKOMSTDATUM                   
000900*                                 ESTIMATED RECEIVING DATE                
001000     03 IDFAKT               PIC 9(7).                                    
001100*                                 FAKTURANUMMER                           
001200*                                 INVOICE NO.                             
001300     03 ADINLOMR             PIC X(4).                                    
001400*                                 INLEVERANSOMRÅDE                        
001500*                                 RECEIVING AREA                          
001600     03 IDDC-SEND            PIC X(2).                                    
001700*                                 SÄNDANDE LAGER                          
001800*                                 SENDING WAREHOUSE                       
001900     03 IDDISTR              PIC 9(5).                                    
002000*                                 DISTRIKTNUMMER                          
002100*                                 DISTRICT NUMBER                         
002200     03 IDKUNDNR             PIC 9(7).                                    
002300*                                 KUNDNUMMER                              
002400*                                 CUSTOMER NO                             
002500     03 IDKUNDRF             PIC X(10).                                   
002600*                                 KUNDENS REFERENS (ORDERID)              
002700*                                 CUSTOMER REFERENCE (ORDER ID)           
002800     03 IDLBBET              PIC X(12).                                   
002900*                                 LASTBÄRARBETECKNING                     
003000*                                 TRAILER NUMBER                          
003100     03 IDLEVNR              PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003400     03 KDTRPSTA             PIC X.                                       
003500*                                 TRANSPORTSTATUS                         
003600*                                 TRANSPORT STATUS                        
003700     03 KVKOLLI-FAKT         PIC 9(4).                                    
003800*                                 ANTAL FAKTURERADE KOLLIN                
003900*                                 QUANTITY CASES INVOICED                 
004000     03 KVKOLLI-MOT          PIC 9(4).                                    
004100*                                 ANTAL MOTTAGNA KOLLIN                   
004200*                                 QUANTITY CASES RECEIVED                 
004300     03 KVRADER-FAKT         PIC 9(5).                                    
004400*                                 ANTAL RADER PER FAKTURA                 
004500*                                 NUMBER OF LINES PER INVOICE             
004600     03 KVRADER-MOT          PIC 9(5).                                    
004700*                                 ANTAL MOTTAGNA  RADER                   
004800*                                 NUMBER OF LINES RECEIVED                
004900     03 KVRADER-PRIO         PIC 9(5).                                    
005000*                                 ANTAL PRIORITERADE RADER                
005100*                                 NUMBER OF PRIORITY LINES                
005200     03 TIFAKT               PIC 9(6).                                    
005300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
005400*                                 INVOICING DATE   (YYMMDD)               
005500     03 IDSHIPM              PIC 9(7).                                    
005600*                                 SKEPPNINGSNUMMER                        
005700*                                 SHIPMENT NO                             
005800     03 IDDC-LEV             PIC X(2).                                    
005900*                                 LEVERERANDE DC I EXPORTFLÖDET           
006000*                                 DELIVERY DC IN EXPORT FLOW              
006100     03 IDBOKN               PIC X(15).                                   
006200*                                 BOKNINGSNUMMER                          
006300*                                 BOOKING NUMBER                          
006400     03 BETRPFIR             PIC X(15).                                   
006500*                                 TRANSPORTFIRMANS NAMN                   
006600*                                 NAME OF THE TRANSPORTCOMPANY            
006700     03 FLMANETA             PIC X.                                       
006800*                                 MANUALLY UPDATE ETA FLAG (Y/N)          
006900     03 IDUSER-MANETA        PIC X(8).                                    
007000*                                 USERID FOR MANUAL ETA DATE              
007100     03 DABERANK-DISCH       PIC 9(6).                                    
007200*                                 DISCHARGED ETA DATUM                    
007300     03 DABERANK-PROP        PIC 9(6).                                    
007400*                                 PROPOSED ETA FROM P44 & PULS            
007500     03 TILST-CALLP44        PIC 9(6).                                    
007600*                                 DATE LATEST CALL PROJECT44              
007700     03 TILST-PUSHEVNT       PIC 9(6).                                    
007800*                                 DATE LATEST RECEIVED PUSHEVENT          
007900*                                 FROM PROJECT44                          
008000     03 IDSUBSCR             PIC 9(11).                                   
008100*                                 SUBSCRIPTION ID FROM PROJECT44          
008200     03 IDCONTNR             PIC 9(11).                                   
008300*                                 UNIQUE ID OF CONTAINER FROM             
008400*                                 PROJECT44                               
008500     03 DABERANK-LIFDEPPL    PIC 9(6).                                    
008600*                                 PLANNED ETA DATE FROM PROJECT44         
008700     03 DABERANK-LIFDEPAC    PIC 9(6).                                    
008800*                                 ACTUAL ETA DATE FROM PROJECT44          
008900     03 DABERANK-PODDEPPL    PIC 9(6).                                    
009000*                                 PLANNED ETA DATE FROM PROJECT44         
009100     03 DABERANK-PODDEPAC    PIC 9(6).                                    
009200*                                 ACTUAL ETA DATE FROM PROJECT4           
009300     03 DABERANK-DLVDELPL    PIC 9(6).                                    
009400*                                 PLANNED ETA DATE FROM PROJECT44         
009500     03 DABERANK-DLVDELAC    PIC 9(6).                                    
009600*                                 ACTUAL ETA DATE FROM PROJECT4           
009700     03 DABERANK-PODDISPL    PIC 9(6).                                    
009800*                                 PLANNED ETA DATE FROM PROJECT44         
009900     03 DABERANK-PODDISAC    PIC 9(6).                                    
010000*                                 ACTUAL ETA DATE FROM PROJECT4           
010100     03 DABERANK-PODARRPL    PIC 9(6).                                    
010200*                                 PLANNED ETA DATE FROM PROJECT44         
010300     03 DABERANK-LIFARRAC    PIC 9(6).                                    
010400*                                 ACTUAL ETA DATE FROM PROJECT4           
010500*** END OF VILMAII-COPY LENGTH= 246 BYTES                                 
