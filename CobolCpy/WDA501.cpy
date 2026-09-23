000100 01  RAD-WDA501.                                                          
000200*                                 ORDERRADREGISTER                        
000300*                                 RADINFORMATION                          
000400*                                 FYSISK NYCKEL: WDA501KY                 
000500*                                  (IDDISTR + IDKUNDNR + IDKUNDRF         
000600*                                   + IDARTNR + IDLOPNR)                  
000700     03 RAD-IDGMTREF.                                                     
000800*                                 GODSMOTTAGAREREFERENS                   
000900*                                 GOODS RECEIVER REFERENS                 
001000        05 RAD-IDDISTR       PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300        05 RAD-IDKUNDNR      PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600        05 RAD-IDKUNDRF-GRP.                                              
001700*                                 KUNDENS REFERENS (ORDERID)              
001800*                                 CUSTOMER REFERENCE (ORDER ID)           
001900           07 RAD-IDKUNDRF   PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100*                                 CUSTOMER REFERENCE (ORDER ID)           
002200           07 RAD-IDORDNR5-FILLER REDEFINES RAD-IDKUNDRF.                 
002300              09 RAD-IDORDNR5                                             
002400                             PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700              09 FILLER      PIC X(5).                                    
002800           07 RAD-IDORDNR7-FILLER REDEFINES RAD-IDKUNDRF.                 
002900              09 RAD-IDORDNR7                                             
003000                             PIC 9(7).                                    
003100*                                 ORDERNUMMER                             
003200*                                 ORDER NUMBER                            
003300              09 FILLER      PIC X(3).                                    
003400     03 RAD-IDARTNR          PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600*                                 PART NUMBER                             
003700     03 RAD-IDLOPNR          PIC S9(3)           COMP-3.                  
003800*                                 LÖPNUMMER                               
003900*                                 SEQUENCE NUMBER                         
004000     03 RAD-BERADREF         PIC X(10).                                   
004100*                                 KUNDENS RADREFERENS                     
004200*                                 CUSTOMERS ITEM REF.                     
004300     03 RAD-FLERS            PIC X.                                       
004400*                                 TILLKOMMANDE ARTIKEL ?                  
004500     03 RAD-IDANSK           PIC S9(3)           COMP-3.                  
004600*                                 ANSKAFFARNUMMER                         
004700*                                 PROCURER NO.                            
004800     03 RAD-IDANALYS         PIC X(12).                                   
004900*                                 ANALYSNUMMER                            
005000*                                 ANALYSIS NUMBER                         
005100     03 RAD-IDKONTO          PIC S9(11)          COMP-3.                  
005200*                                 KONTO                                   
005300*                                 ACCOUNT                                 
005400     03 RAD-IDKST            PIC X(10).                                   
005500*                                 KOSTNADSSTÄLLE                          
005600*                                 COST CENTRE                             
005700     03 RAD-IDKUNDRF-LEV     PIC X(10).                                   
005800*                                 KUND REF PÅ LEVERANSORDERN              
005900*                                 CUST REF ON DELIVERY ORDER              
006000     03 RAD-IDDC             PIC X(2).                                    
006100*                                 IDENTIFIERARE LAGER                     
006200*                                 WAREHOUSE IDENTIFIER                    
006300     03 RAD-IDDC-RO          PIC X(2).                                    
006400*                                 LAGER DÄR RESTORDER FÅR SKE             
006500*                                 WAREHOUSE FOR BACKORDERS                
006600     03 RAD-KDDSP            PIC S9              COMP-3.                  
006700*                                 PÅVERKAN PÅ DSP                         
006800*                                 AFFECT ON DSP                           
006900     03 RAD-KDFAKTYP         PIC X.                                       
007000*                                 FAKTURATYP                              
007100*                                 INVOICE TYPE                            
007200     03 RAD-KDFRAKT          PIC S9(3)           COMP-3.                  
007300*                                 FRAKTSÄTT DC TILL KUND                  
007400*                                 FREIGHT CODE                            
007500     03 RAD-KDKVBRYT         PIC S9              COMP-3.                  
007600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
007700*                                 BREAK BULKPACK CODE                     
007800     03 RAD-KDOI             PIC X(2).                                    
007900*                                 ORDERINGÅNGSTYP                         
008000*                                 TYPE OF INCOMING ORDER                  
008100     03 RAD-KDORDING         PIC S9              COMP-3.                  
008200*                                 UPPDATERING ORDERINGÅNG                 
008300*                                 ORDER STATISTICS                        
008400     03 RAD-KDORDKL          PIC S9              COMP-3.                  
008500*                                 ORDERKLASS                              
008600*                                 ORDER CLASS                             
008700     03 RAD-KDPRODSL         PIC S9(3)           COMP-3.                  
008800*                                 PRODUKTSLAG                             
008900*                                 PRODUCT GROUP                           
009000     03 RAD-KDRAPRIO         PIC S9(3)           COMP-3.                  
009100*                                 PRIORITETSKOD PÅ RADEN                  
009200*                                 PRIORITY CODE ON THE LINE               
009300     03 RAD-KDROO            PIC S9              COMP-3.                  
009400*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
009500*                                 REASON CODE FOR WAITING LINE            
009600     03 RAD-KDSTARAD         PIC X.                                       
009700*                                 RADSTATUSKOD                            
009800*                                 LINE STATUS CODE                        
009900     03 RAD-KDTPOTYP         PIC S9              COMP-3.                  
010000*                                 TYP AV TIDPLANERAD ORDER                
010100*                                 TYPE OF TIME PLANNED ORDER              
010200     03 RAD-KVART            PIC S9(7)           COMP-3.                  
010300*                                 ANTAL ARTNR PER BRYTBEGREPP             
010400*                                 NO OF PARTNOS PER TYPE                  
010500     03 RAD-KDVRINFO         PIC S9              COMP-3.                  
010600*                                 PÅVERKAN I VR/DSP SYSTEM                
010700*                                 VR/DSP UP-DATE                          
010800     03 RAD-KVRO             PIC S9(7)           COMP-3.                  
010900*                                 ANTAL RESTNOTERADE ARTIKLAR             
011000*                                 BACKORDERED QTY                         
011100     03 RAD-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
011200*                                 ARTIKELPRIS NETTO                       
011300*                                 NET PRICE EACH   (FOB NET)              
011400     03 RAD-REKSIFFR         PIC S9              COMP-3.                  
011500*                                 KONTROLLSIFFRA                          
011600*                                 PART NO CHECK DIGIT                     
011700     03 RAD-TIAVBOKN         PIC S9(7)           COMP-3.                  
011800*                                 LAGERAVBOKNINGSDATUM                    
011900*                                 STOCK ALLOCATION DATE                   
012000     03 RAD-TIREGDAT         PIC S9(7)           COMP-3.                  
012100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
012200*                                 REGISTRATION DATE (YYMMDD)              
012300     03 RAD-TIRES            PIC S9(7)           COMP-3.                  
012400*                                 RESERVATIONSDATUM                       
012500*                                 RESERVATION DATE                        
012600     03 RAD-DARODAT          PIC 9(8).                                    
012700*                                 RESTORDERDATUM       (ÅÅÅÅMMDD)         
012800*                                 BACK ORDER DATE      (YYYYMMDD)         
012900     03 RAD-TITPO            PIC S9(7)           COMP-3.                  
013000*                                 PLANERAD ORDERDATUM                     
013100*                                 PLANNED ORDER DATE                      
013200     03 RAD-KDPRTYP          PIC X.                                       
013300*                                 TYP AV PRISTILLÄMPNING                  
013400*                                 TYPE OF PRICING                         
013500     03 RAD-BEVOLREF         PIC X(10).                                   
013600*                                 VOLVO REFERENS                          
013700*                                 VOLVO REFERENCE                         
013800     03 RAD-FLINVEST         PIC X.                                       
013900*                                 BYTES INVENTERINGSFLAGGA                
014000*                                 EXCHANGE INVESTMENT FLAG                
014100     03 RAD-FLPRTILL         PIC X.                                       
014200*                                 PRISTILLÄGGS FLAGGA                     
014300*                                 PRICE PENALTY FLAG                      
014400     03 RAD-FLTPOBEK         PIC X.                                       
014500*                                 TPO-RAD BEKRÄFTAD                       
014600*                                 TPO ORDER LINE CONFIRMED                
014700     03 RAD-BEKUNDRF         PIC X(15).                                   
014800*                                 KUNDENS REFERENS                        
014900*                                 CUSTOMERS REFERENCE                     
015000     03 RAD-IDKAMPRF         PIC S9(7)           COMP-3.                  
015100*                                 KAMPANJREFERENS                         
015200*                                 CAMPAIGN REFERENCE                      
015300     03 RAD-IDLEVNR          PIC X(5).                                    
015400*                                 LEVERANTÖRNUMMER                        
015500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
015600     03 RAD-IDSYSTEM         PIC X(4).                                    
015700*                                 VOLVO VCCS SYSTEMNUMMER                 
015800*                                 VOLVO VCCS SYSTEM NUMBER                
015900     03 RAD-KVBEART-Q        PIC S9(7)           COMP-3.                  
016000*                                 BESTÄLLT KVANTANPASSAT ANTAL            
016100*                                 ORDERED QUANTITY ADAPTED                
016200*                                  ITEMS                                  
016300     03 RAD-TIREGTID         PIC S9(7)           COMP-3.                  
016400*                                 REGISTRERINGSTID                        
016500*                                 GENERAL REGISTRATION TIME               
016600     03 RAD-DASENBEK.                                                     
016700*                                 SENASTE BEKRÄFTELSETIDPUNKT             
016800*                                 LATEST CONFIRMATION DATE+TIME           
016900        05 RAD-DASENDAT      PIC 9(8).                                    
017000*                                 SENASTE BEKRÄFTELSEDATUM                
017100*                                 LATEST CONFIRMATION DATE                
017200*                                 (YYYYMMDD)                              
017300        05 RAD-TISENBEK-KL   PIC 9(6).                                    
017400*                                 TIM - MIN - SEK   (HHMMSS)              
017500*                                 HOUR - MINUTE - SEC (HHMMSS)            
017600     03 RAD-DEAL-PR-LINE.                                                 
017700*                                 DEALERPRIS (RAD)                        
017800        05 RAD-IDPRQUES      PIC 9(7).                                    
017900*                                 PRISFRÅGA NR                            
018000*                                 PRICE QUESTION NO                       
018100        05 RAD-PRARTNTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
018200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
018300*                                 NET PRICE EACH LOCAL CURRENCY           
018400        05 RAD-PRARTNTO-LOCPREL                                           
018500                             PIC S9(7)V9(2)      COMP-3.                  
018600*                                 PREL NETTO SLUTKUNDSPRIS I              
018700*                                 LOKAL VALUTA                            
018800*                                 PREL NET PRICE - LOCAL CURRENCY         
018900        05 RAD-PRARTBTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
019000*                                 PRIS I LOKAL VALUTA                     
019100*                                 LOCAL GROSS SALES PRICE                 
019200        05 RAD-KDVALISO      PIC X(3).                                    
019300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
019400*                                 CURRENCY CODE BY ISO-STANDARD.          
019500        05 RAD-KDVAT         PIC X(2).                                    
019600*                                 MOMSKOD                                 
019700*                                 VAT CODE                                
019800        05 RAD-RERAB         PIC S9(2)V9(1)      COMP-3.                  
019900*                                 RABATTSATS (PROCENT)                    
020000        05 RAD-KDRAB         PIC X(5).                                    
020100*                                 RABATTKOD                               
020200        05 RAD-BEART-VIPS    PIC X(25).                                   
020300*                                 VIPS ARTIKELBENÄMNING                   
020400*                                 PÅ DEALERNS SPRÅK                       
020500     03 RAD-KDORDTYP-LDC     PIC X(2).                                    
020600*                                 ORDERTYP HOS DEALER                     
020700     03 RAD-IDKUNDRF-WIP     PIC X(10).                                   
020800*                                 REPARATIONS ORDERNR, LDC KUND           
020900*                                 WORK ORDER NUMBER, LDC DEALER           
021000     03 RAD-TIREPDAT         PIC S9(7)           COMP-3.                  
021100*                                 REPAIR DATE                             
021200*                                 REPAIR DATE                             
021300     03 RAD-CLEARGROUP.                                                   
021400*                                 CLEARINGAREA FÖR ORDERINGÅNG            
021500        05 RAD-CLEARAREA     OCCURS 7 TIMES.                              
021600*                                 CLEARINGAREA FÖR ORDERINGÅNG            
021700           07 RAD-IDDC-CLEAR PIC X(2).                                    
021800*                                 LAGERPRIORITERING VID                   
021900*                                 ORDERCLEARING                           
022000*                                 WAREHOUSE PRIORITY FOR ORDER            
022100*                                 CLEARING                                
022200           07 RAD-FLLF       PIC X.                                       
022300*                                 ARTIKEL LAGERFÖRES                      
022400*                                 PART IN STOCK                           
022500           07 RAD-FLCLEAR    PIC X.                                       
022600*                                 ORDERRAD CLEAR FLAGGA                   
022700*                                 CLEARING FLAG FOR ORDER LINE            
022800     03 RAD-PRAVCOST         PIC S9(7)V9(2)      COMP-3.                  
022900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
023000*                                 AVERAGE COST FOREIGN CURRENCY           
023100     03 RAD-KDROPACK         PIC X.                                       
023200*                                 FRISLÄPPNINGSKOD RO/DO                  
023300*                                 CONSOLIDATION BO/DO                     
023400     03 RAD-IDARBREF         PIC X(10).                                   
023500*                                 ARBETSORDER SOFT/DÖSKALLE               
023600*                                 WORK ORDER SOFT/DUMMY ORDERHEAD         
023700     03 RAD-FILLER           PIC X(10).                                   
023800*** END OF VILMAII-COPY LENGTH= 327 BYTES                                 
