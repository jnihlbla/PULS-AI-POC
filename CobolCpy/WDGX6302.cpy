000100 01  6302-WDGX6302.                                                       
000200*                                 SDC  + NDC INLEV FAKTURAINFO            
000300*                                 FYSISK NYCKEL: KEY6302                  
000400*                                    (DABERANK, IDFAKT)                   
000500*                                 SÖKBEGREPP: IDFAKT                      
000600*                                  IDLBBET IDGMTREF KDTRPSTA              
000700*                                  IDDISTR IDKUNDNR IDKUNDRF              
000800     03 6302-DABERANK        PIC 9(8).                                    
000900*                                 BERÄKNAD ANKOMSTDATUM                   
001000*                                 ESTIMATED RECEIVING DATE                
001100     03 6302-IDFAKT          PIC S9(7)           COMP-3.                  
001200*                                 FAKTURANUMMER                           
001300*                                 INVOICE NO.                             
001400     03 6302-ADINLOMR        PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE                        
001600*                                 RECEIVING AREA                          
001700     03 6302-IDDC-SEND       PIC X(2).                                    
001800*                                 SÄNDANDE LAGER                          
001900*                                 SENDING WAREHOUSE                       
002000     03 6302-IDGMTREF.                                                    
002100*                                 GODSMOTTAGAREREFERENS                   
002200*                                 GOODS RECEIVER REFERENS                 
002300        05 6302-IDDISTR      PIC S9(5)           COMP-3.                  
002400*                                 DISTRIKTNUMMER                          
002500*                                 DISTRICT NUMBER                         
002600        05 6302-IDKUNDNR     PIC S9(7)           COMP-3.                  
002700*                                 KUNDNUMMER                              
002800*                                 CUSTOMER NO                             
002900        05 6302-IDKUNDRF-GRP.                                             
003000*                                 KUNDENS REFERENS (ORDERID)              
003100*                                 CUSTOMER REFERENCE (ORDER ID)           
003200           07 6302-IDKUNDRF  PIC X(10).                                   
003300*                                 KUNDENS REFERENS (ORDERID)              
003400*                                 CUSTOMER REFERENCE (ORDER ID)           
003500           07 6302-IDORDNR5-FILLER REDEFINES 6302-IDKUNDRF.               
003600              09 6302-IDORDNR5                                            
003700                             PIC 9(5).                                    
003800*                                 ORDERNUMMER                             
003900*                                 ORDER NUMBER                            
004000              09 FILLER      PIC X(5).                                    
004100           07 6302-IDORDNR7-FILLER REDEFINES 6302-IDKUNDRF.               
004200              09 6302-IDORDNR7                                            
004300                             PIC 9(7).                                    
004400*                                 ORDERNUMMER                             
004500*                                 ORDER NUMBER                            
004600              09 FILLER      PIC X(3).                                    
004700     03 6302-IDLBBET         PIC X(12).                                   
004800*                                 LASTBÄRARBETECKNING                     
004900*                                 TRAILER NUMBER                          
005000     03 6302-IDLEVNR         PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005300     03 6302-KDTRPSTA        PIC X.                                       
005400*                                 TRANSPORTSTATUS                         
005500*                                 TRANSPORT STATUS                        
005600     03 6302-KVKOLLI-FAKT    PIC S9(5)           COMP-3.                  
005700*                                 ANTAL FAKTURERADE KOLLIN                
005800*                                 QUANTITY CASES INVOICED                 
005900     03 6302-KVKOLLI-MOT     PIC S9(5)           COMP-3.                  
006000*                                 ANTAL MOTTAGNA KOLLIN                   
006100*                                 QUANTITY CASES RECEIVED                 
006200     03 6302-KVRADER-FAKT    PIC S9(5)           COMP-3.                  
006300*                                 ANTAL RADER PER FAKTURA                 
006400*                                 NUMBER OF LINES PER INVOICE             
006500     03 6302-KVRADER-MOT     PIC S9(5)           COMP-3.                  
006600*                                 ANTAL MOTTAGNA  RADER                   
006700*                                 NUMBER OF LINES RECEIVED                
006800     03 6302-KVRADER-PRIO    PIC S9(5)           COMP-3.                  
006900*                                 ANTAL PRIORITERADE RADER                
007000*                                 NUMBER OF PRIORITY LINES                
007100     03 6302-TIFAKT          PIC S9(7)           COMP-3.                  
007200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
007300*                                 INVOICING DATE   (YYMMDD)               
007400     03 6302-IDSHIPM         PIC 9(7).                                    
007500*                                 SKEPPNINGSNUMMER                        
007600*                                 SHIPMENT NO                             
007700     03 6302-IDDC-LEV        PIC X(2).                                    
007800*                                 LEVERERANDE DC I EXPORTFLÖDET           
007900*                                 DELIVERY DC IN EXPORT FLOW              
008000     03 6302-IDBOKN          PIC X(15).                                   
008100*                                 BOKNINGSNUMMER                          
008200*                                 BOOKING NUMBER                          
008300     03 6302-BETRPFIR        PIC X(15).                                   
008400*                                 TRANSPORTFIRMANS NAMN                   
008500*                                 NAME OF THE TRANSPORTCOMPANY            
008600     03 6302-FLMANETA        PIC X.                                       
008700*                                 MANUALLY UPDATE ETA FLAG (Y/N)          
008800     03 6302-IDUSER-MANETA   PIC X(8).                                    
008900*                                 USERID FOR MANUAL ETA DATE              
009000     03 6302-DABERANK-DISCH  PIC S9(7)           COMP-3.                  
009100*                                 DISCHARGED ETA DATUM                    
009200     03 6302-DABERANK-PROP   PIC S9(7)           COMP-3.                  
009300*                                 PROPOSED ETA FROM P44 & PULS            
009400     03 6302-TILST-CALLP44   PIC S9(7)           COMP-3.                  
009500*                                 DATE LATEST CALL PROJECT44              
009600     03 6302-TILST-PUSHEVNT  PIC S9(7)           COMP-3.                  
009700*                                 DATE LATEST RECEIVED PUSHEVENT          
009800*                                 FROM PROJECT44                          
009900     03 6302-IDSUBSCR        PIC S9(11)          COMP-3.                  
010000*                                 SUBSCRIPTION ID FROM PROJECT44          
010100     03 6302-IDCONTNR        PIC S9(11)          COMP-3.                  
010200*                                 UNIQUE ID OF CONTAINER FROM             
010300*                                 PROJECT44                               
010400     03 6302-DABERANK-LIFDEPPL                                            
010500                             PIC S9(7)           COMP-3.                  
010600*                                 PLANNED ETA DATE FROM PROJECT44         
010700     03 6302-DABERANK-LIFDEPAC                                            
010800                             PIC S9(7)           COMP-3.                  
010900*                                 ACTUAL ETA DATE FROM PROJECT44          
011000     03 6302-DABERANK-PODDEPPL                                            
011100                             PIC S9(7)           COMP-3.                  
011200*                                 PLANNED ETA DATE FROM PROJECT44         
011300     03 6302-DABERANK-PODDEPAC                                            
011400                             PIC S9(7)           COMP-3.                  
011500*                                 ACTUAL ETA DATE FROM PROJECT4           
011600     03 6302-DABERANK-DLVDELPL                                            
011700                             PIC S9(7)           COMP-3.                  
011800*                                 PLANNED ETA DATE FROM PROJECT44         
011900     03 6302-DABERANK-DLVDELAC                                            
012000                             PIC S9(7)           COMP-3.                  
012100*                                 ACTUAL ETA DATE FROM PROJECT4           
012200     03 6302-DABERANK-PODDISPL                                            
012300                             PIC S9(7)           COMP-3.                  
012400*                                 PLANNED ETA DATE FROM PROJECT44         
012500     03 6302-DABERANK-PODDISAC                                            
012600                             PIC S9(7)           COMP-3.                  
012700*                                 ACTUAL ETA DATE FROM PROJECT4           
012800     03 6302-DABERANK-PODARRPL                                            
012900                             PIC S9(7)           COMP-3.                  
013000*                                 PLANNED ETA DATE FROM PROJECT44         
013100     03 6302-DABERANK-LIFARRAC                                            
013200                             PIC S9(7)           COMP-3.                  
013300*                                 ACTUAL ETA DATE FROM PROJECT4           
013400*** END OF VILMAII-COPY LENGTH= 188 BYTES                                 
