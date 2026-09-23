000100 01  RAD-W44060.                                                          
000200*                                 KOMPLETT KOPIA AV RO-REGISTER           
000300*                                                                         
000400     03 RAD-WDA501.                                                       
000500*                                 ORDERRADREGISTER                        
000600*                                 RADINFORMATION                          
000700*                                 FYSISK NYCKEL: WDA501KY                 
000800*                                  (IDDISTR + IDKUNDNR + IDKUNDRF         
000900*                                   + IDARTNR + IDLOPNR)                  
001000        05 RAD-IDGMTREF.                                                  
001100*                                 GODSMOTTAGAREREFERENS                   
001200*                                 GOODS RECEIVER REFERENS                 
001300           07 RAD-IDDISTR    PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600           07 RAD-IDKUNDNR   PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900           07 RAD-IDKUNDRF-GRP.                                           
002000*                                 KUNDENS REFERENS (ORDERID)              
002100*                                 CUSTOMER REFERENCE (ORDER ID)           
002200              09 RAD-IDKUNDRF                                             
002300                             PIC X(10).                                   
002400*                                 KUNDENS REFERENS (ORDERID)              
002500*                                 CUSTOMER REFERENCE (ORDER ID)           
002600              09 RAD-IDORDNR5-FILLER REDEFINES RAD-IDKUNDRF.              
002700                 11 RAD-IDORDNR5                                          
002800                             PIC 9(5).                                    
002900*                                 ORDERNUMMER                             
003000*                                 ORDER NUMBER                            
003100                 11 FILLER   PIC X(5).                                    
003200              09 RAD-IDORDNR7-FILLER REDEFINES RAD-IDKUNDRF.              
003300                 11 RAD-IDORDNR7                                          
003400                             PIC 9(7).                                    
003500*                                 ORDERNUMMER                             
003600*                                 ORDER NUMBER                            
003700                 11 FILLER   PIC X(3).                                    
003800        05 RAD-IDARTNR       PIC S9(9)           COMP-3.                  
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100        05 RAD-IDLOPNR       PIC S9(3)           COMP-3.                  
004200*                                 LÖPNUMMER                               
004300*                                 SEQUENCE NUMBER                         
004400        05 RAD-BERADREF      PIC X(10).                                   
004500*                                 KUNDENS RADREFERENS                     
004600*                                 CUSTOMERS ITEM REF.                     
004700        05 RAD-FLERS         PIC X.                                       
004800*                                 TILLKOMMANDE ARTIKEL ?                  
004900        05 RAD-IDANSK        PIC S9(3)           COMP-3.                  
005000*                                 ANSKAFFARNUMMER                         
005100*                                 PROCURER NO.                            
005200        05 RAD-IDANALYS      PIC X(12).                                   
005300*                                 ANALYSNUMMER                            
005400*                                 ANALYSIS NUMBER                         
005500        05 RAD-IDKONTO       PIC S9(11)          COMP-3.                  
005600*                                 KONTO                                   
005700*                                 ACCOUNT                                 
005800        05 RAD-IDKST         PIC X(10).                                   
005900*                                 KOSTNADSSTÄLLE                          
006000*                                 COST CENTRE                             
006100        05 RAD-IDKUNDRF-LEV  PIC X(10).                                   
006200*                                 KUND REF PÅ LEVERANSORDERN              
006300*                                 CUST REF ON DELIVERY ORDER              
006400        05 RAD-IDDC          PIC X(2).                                    
006500*                                 IDENTIFIERARE LAGER                     
006600*                                 WAREHOUSE IDENTIFIER                    
006700        05 RAD-IDDC-RO       PIC X(2).                                    
006800*                                 LAGER DÄR RESTORDER FÅR SKE             
006900*                                 WAREHOUSE FOR BACKORDERS                
007000        05 RAD-KDDSP         PIC S9              COMP-3.                  
007100*                                 PÅVERKAN PÅ DSP                         
007200*                                 AFFECT ON DSP                           
007300        05 RAD-KDFAKTYP      PIC X.                                       
007400*                                 FAKTURATYP                              
007500*                                 INVOICE TYPE                            
007600        05 RAD-KDFRAKT       PIC S9(3)           COMP-3.                  
007700*                                 FRAKTSÄTT DC TILL KUND                  
007800*                                 FREIGHT CODE                            
007900        05 RAD-KDKVBRYT      PIC S9              COMP-3.                  
008000*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
008100*                                 BREAK BULKPACK CODE                     
008200        05 RAD-KDOI          PIC X(2).                                    
008300*                                 ORDERINGÅNGSTYP                         
008400*                                 TYPE OF INCOMING ORDER                  
008500        05 RAD-KDORDING      PIC S9              COMP-3.                  
008600*                                 UPPDATERING ORDERINGÅNG                 
008700*                                 ORDER STATISTICS                        
008800        05 RAD-KDORDKL       PIC S9              COMP-3.                  
008900*                                 ORDERKLASS                              
009000*                                 ORDER CLASS                             
009100        05 RAD-KDPRODSL      PIC S9(3)           COMP-3.                  
009200*                                 PRODUKTSLAG                             
009300*                                 PRODUCT GROUP                           
009400        05 RAD-KDRAPRIO      PIC S9(3)           COMP-3.                  
009500*                                 PRIORITETSKOD PÅ RADEN                  
009600*                                 PRIORITY CODE ON THE LINE               
009700        05 RAD-KDROO         PIC S9              COMP-3.                  
009800*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
009900*                                 REASON CODE FOR WAITING LINE            
010000        05 RAD-KDSTARAD      PIC X.                                       
010100*                                 RADSTATUSKOD                            
010200*                                 LINE STATUS CODE                        
010300        05 RAD-KDTPOTYP      PIC S9              COMP-3.                  
010400*                                 TYP AV TIDPLANERAD ORDER                
010500*                                 TYPE OF TIME PLANNED ORDER              
010600        05 RAD-KVART         PIC S9(7)           COMP-3.                  
010700*                                 ANTAL ARTNR PER BRYTBEGREPP             
010800*                                 NO OF PARTNOS PER TYPE                  
010900        05 RAD-KDVRINFO      PIC S9              COMP-3.                  
011000*                                 PÅVERKAN I VR/DSP SYSTEM                
011100*                                 VR/DSP UP-DATE                          
011200        05 RAD-KVRO          PIC S9(7)           COMP-3.                  
011300*                                 ANTAL RESTNOTERADE ARTIKLAR             
011400*                                 BACKORDERED QTY                         
011500        05 RAD-PRARTNTO      PIC S9(7)V9(2)      COMP-3.                  
011600*                                 ARTIKELPRIS NETTO                       
011700*                                 NET PRICE EACH   (FOB NET)              
011800        05 RAD-REKSIFFR      PIC S9              COMP-3.                  
011900*                                 KONTROLLSIFFRA                          
012000*                                 PART NO CHECK DIGIT                     
012100        05 RAD-TIAVBOKN      PIC S9(7)           COMP-3.                  
012200*                                 LAGERAVBOKNINGSDATUM                    
012300*                                 STOCK ALLOCATION DATE                   
012400        05 RAD-TIREGDAT      PIC S9(7)           COMP-3.                  
012500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
012600*                                 REGISTRATION DATE (YYMMDD)              
012700        05 RAD-TIRES         PIC S9(7)           COMP-3.                  
012800*                                 RESERVATIONSDATUM                       
012900*                                 RESERVATION DATE                        
013000        05 RAD-DARODAT       PIC 9(8).                                    
013100*                                 RESTORDERDATUM       (ÅÅÅÅMMDD)         
013200*                                 BACK ORDER DATE      (YYYYMMDD)         
013300        05 RAD-TITPO         PIC S9(7)           COMP-3.                  
013400*                                 PLANERAD ORDERDATUM                     
013500*                                 PLANNED ORDER DATE                      
013600        05 RAD-KDPRTYP       PIC X.                                       
013700*                                 TYP AV PRISTILLÄMPNING                  
013800*                                 TYPE OF PRICING                         
013900        05 RAD-BEVOLREF      PIC X(10).                                   
014000*                                 VOLVO REFERENS                          
014100*                                 VOLVO REFERENCE                         
014200        05 RAD-FLINVEST      PIC X.                                       
014300*                                 BYTES INVENTERINGSFLAGGA                
014400*                                 EXCHANGE INVESTMENT FLAG                
014500        05 RAD-FLPRTILL      PIC X.                                       
014600*                                 PRISTILLÄGGS FLAGGA                     
014700*                                 PRICE PENALTY FLAG                      
014800        05 RAD-FLTPOBEK      PIC X.                                       
014900*                                 TPO-RAD BEKRÄFTAD                       
015000*                                 TPO ORDER LINE CONFIRMED                
015100        05 RAD-BEKUNDRF      PIC X(15).                                   
015200*                                 KUNDENS REFERENS                        
015300*                                 CUSTOMERS REFERENCE                     
015400        05 RAD-IDKAMPRF      PIC S9(7)           COMP-3.                  
015500*                                 KAMPANJREFERENS                         
015600*                                 CAMPAIGN REFERENCE                      
015700        05 RAD-IDLEVNR       PIC X(5).                                    
015800*                                 LEVERANTÖRNUMMER                        
015900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
016000        05 RAD-IDSYSTEM      PIC X(4).                                    
016100*                                 VOLVO VCCS SYSTEMNUMMER                 
016200*                                 VOLVO VCCS SYSTEM NUMBER                
016300        05 RAD-KVBEART-Q     PIC S9(7)           COMP-3.                  
016400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
016500*                                 ORDERED QUANTITY ADAPTED                
016600*                                  ITEMS                                  
016700        05 RAD-TIREGTID      PIC S9(7)           COMP-3.                  
016800*                                 REGISTRERINGSTID                        
016900*                                 GENERAL REGISTRATION TIME               
017000        05 RAD-DASENBEK.                                                  
017100*                                 SENASTE BEKRÄFTELSETIDPUNKT             
017200*                                 LATEST CONFIRMATION DATE+TIME           
017300           07 RAD-DASENDAT   PIC 9(8).                                    
017400*                                 SENASTE BEKRÄFTELSEDATUM                
017500*                                 LATEST CONFIRMATION DATE                
017600*                                 (YYYYMMDD)                              
017700           07 RAD-TISENBEK-KL                                             
017800                             PIC 9(6).                                    
017900*                                 TIM - MIN - SEK   (HHMMSS)              
018000*                                 HOUR - MINUTE - SEC (HHMMSS)            
018100        05 RAD-DEAL-PR-LINE.                                              
018200*                                 DEALERPRIS (RAD)                        
018300           07 RAD-IDPRQUES   PIC 9(7).                                    
018400*                                 PRISFRÅGA NR                            
018500*                                 PRICE QUESTION NO                       
018600           07 RAD-PRARTNTO-LOC                                            
018700                             PIC S9(7)V9(2)      COMP-3.                  
018800*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
018900*                                 NET PRICE EACH LOCAL CURRENCY           
019000           07 RAD-PRARTNTO-LOCPREL                                        
019100                             PIC S9(7)V9(2)      COMP-3.                  
019200*                                 PREL NETTO SLUTKUNDSPRIS I              
019300*                                 LOKAL VALUTA                            
019400*                                 PREL NET PRICE - LOCAL CURRENCY         
019500           07 RAD-PRARTBTO-LOC                                            
019600                             PIC S9(7)V9(2)      COMP-3.                  
019700*                                 PRIS I LOKAL VALUTA                     
019800*                                 LOCAL GROSS SALES PRICE                 
019900           07 RAD-KDVALISO   PIC X(3).                                    
020000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
020100*                                 CURRENCY CODE BY ISO-STANDARD.          
020200           07 RAD-KDVAT      PIC X(2).                                    
020300*                                 MOMSKOD                                 
020400*                                 VAT CODE                                
020500           07 RAD-RERAB      PIC S9(2)V9(1)      COMP-3.                  
020600*                                 RABATTSATS (PROCENT)                    
020700           07 RAD-KDRAB      PIC X(5).                                    
020800*                                 RABATTKOD                               
020900           07 RAD-BEART-VIPS PIC X(25).                                   
021000*                                 VIPS ARTIKELBENÄMNING                   
021100*                                 PÅ DEALERNS SPRÅK                       
021200        05 RAD-KDORDTYP-LDC  PIC X(2).                                    
021300*                                 ORDERTYP HOS DEALER                     
021400        05 RAD-IDKUNDRF-WIP  PIC X(10).                                   
021500*                                 REPARATIONS ORDERNR, LDC KUND           
021600*                                 WORK ORDER NUMBER, LDC DEALER           
021700        05 RAD-TIREPDAT      PIC S9(7)           COMP-3.                  
021800*                                 REPAIR DATE                             
021900*                                 REPAIR DATE                             
022000        05 RAD-CLEARGROUP.                                                
022100*                                 CLEARINGAREA FÖR ORDERINGÅNG            
022200           07 RAD-CLEARAREA  OCCURS 7 TIMES.                              
022300*                                 CLEARINGAREA FÖR ORDERINGÅNG            
022400              09 RAD-IDDC-CLEAR                                           
022500                             PIC X(2).                                    
022600*                                 LAGERPRIORITERING VID                   
022700*                                 ORDERCLEARING                           
022800*                                 WAREHOUSE PRIORITY FOR ORDER            
022900*                                 CLEARING                                
023000              09 RAD-FLLF    PIC X.                                       
023100*                                 ARTIKEL LAGERFÖRES                      
023200*                                 PART IN STOCK                           
023300              09 RAD-FLCLEAR PIC X.                                       
023400*                                 ORDERRAD CLEAR FLAGGA                   
023500*                                 CLEARING FLAG FOR ORDER LINE            
023600        05 RAD-PRAVCOST      PIC S9(7)V9(2)      COMP-3.                  
023700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
023800*                                 AVERAGE COST FOREIGN CURRENCY           
023900        05 RAD-KDROPACK      PIC X.                                       
024000*                                 FRISLÄPPNINGSKOD RO/DO                  
024100*                                 CONSOLIDATION BO/DO                     
024200        05 RAD-IDARBREF      PIC X(10).                                   
024300*                                 ARBETSORDER SOFT/DÖSKALLE               
024400*                                 WORK ORDER SOFT/DUMMY ORDERHEAD         
024500        05 RAD-FILLER        PIC X(10).                                   
024600*** END OF VILMAII-COPY LENGTH= 327 BYTES                                 
