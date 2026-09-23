000100 01  RAD-W44060.                                                          
000200*                                 W4406X-CTX IS USED IN PROGRAM W         
000300*                                 4406000                                 
000400     03 RAD-IDDISTR          PIC Z(3)9.                                   
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 RAD-IDKUNDNR         PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900*                                 CUSTOMER NO                             
001000     03 RAD-IDKUNDRF         PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200*                                 CUSTOMER REFERENCE (ORDER ID)           
001300     03 RAD-IDARTNR          PIC Z(7)9.                                   
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 RAD-IDLOPNR          PIC Z9.                                      
001700*                                 LÖPNUMMER                               
001800*                                 SEQUENCE NUMBER                         
001900     03 RAD-BERADREF         PIC X(10).                                   
002000*                                 KUNDENS RADREFERENS                     
002100*                                 CUSTOMERS ITEM REF.                     
002200     03 RAD-FLERS            PIC X.                                       
002300*                                 TILLKOMMANDE ARTIKEL ?                  
002400     03 RAD-IDANSK           PIC Z(2)9.                                   
002500*                                 ANSKAFFARNUMMER                         
002600*                                 PROCURER NO.                            
002700     03 RAD-IDANALYS         PIC X(12).                                   
002800*                                 ANALYSNUMMER                            
002900*                                 ANALYSIS NUMBER                         
003000     03 RAD-IDKONTO          PIC Z(9)9.                                   
003100*                                 KONTO                                   
003200*                                 ACCOUNT                                 
003300     03 RAD-IDKST            PIC X(10).                                   
003400*                                 KOSTNADSSTÄLLE                          
003500*                                 COST CENTRE                             
003600     03 RAD-IDKUNDRF-LEV     PIC X(10).                                   
003700*                                 KUND REF PÅ LEVERANSORDERN              
003800*                                 CUST REF ON DELIVERY ORDER              
003900     03 RAD-IDDC             PIC X(2).                                    
004000*                                 IDENTIFIERARE LAGER                     
004100*                                 WAREHOUSE IDENTIFIER                    
004200     03 RAD-IDDC-RO          PIC X(2).                                    
004300*                                 LAGER DÄR RESTORDER FÅR SKE             
004400*                                 WAREHOUSE FOR BACKORDERS                
004500     03 RAD-KDDSP            PIC 9.                                       
004600*                                 PÅVERKAN PÅ DSP                         
004700*                                 AFFECT ON DSP                           
004800     03 RAD-KDFAKTYP         PIC X.                                       
004900*                                 FAKTURATYP                              
005000*                                 INVOICE TYPE                            
005100     03 RAD-KDFRAKT          PIC Z9.                                      
005200*                                 FRAKTSÄTT DC TILL KUND                  
005300*                                 FREIGHT CODE                            
005400     03 RAD-KDKVBRYT         PIC 9.                                       
005500*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005600*                                 BREAK BULKPACK CODE                     
005700     03 RAD-KDOI             PIC X(2).                                    
005800*                                 ORDERINGÅNGSTYP                         
005900*                                 TYPE OF INCOMING ORDER                  
006000     03 RAD-KDORDING         PIC 9.                                       
006100*                                 UPPDATERING ORDERINGÅNG                 
006200*                                 ORDER STATISTICS                        
006300     03 RAD-KDORDKL          PIC 9.                                       
006400*                                 ORDERKLASS                              
006500*                                 ORDER CLASS                             
006600     03 RAD-KDPRODSL         PIC Z9.                                      
006700*                                 PRODUKTSLAG                             
006800*                                 PRODUCT GROUP                           
006900     03 RAD-KDRAPRIO         PIC Z(2)9.                                   
007000*                                 PRIORITETSKOD PÅ RADEN                  
007100*                                 PRIORITY CODE ON THE LINE               
007200     03 RAD-KDROO            PIC 9.                                       
007300*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
007400*                                 REASON CODE FOR WAITING LINE            
007500     03 RAD-KDSTARAD         PIC X.                                       
007600*                                 RADSTATUSKOD                            
007700*                                 LINE STATUS CODE                        
007800     03 RAD-KDTPOTYP         PIC 9.                                       
007900*                                 TYP AV TIDPLANERAD ORDER                
008000*                                 TYPE OF TIME PLANNED ORDER              
008100     03 RAD-KVART            PIC Z(6)9.                                   
008200*                                 ANTAL ARTNR PER BRYTBEGREPP             
008300*                                 NO OF PARTNOS PER TYPE                  
008400     03 RAD-KDVRINFO         PIC 9.                                       
008500*                                 PÅVERKAN I VR/DSP SYSTEM                
008600*                                 VR/DSP UP-DATE                          
008700     03 RAD-KVRO             PIC Z(5)9.                                   
008800*                                 ANTAL RESTNOTERADE ARTIKLAR             
008900*                                 BACKORDERED QTY                         
009000     03 RAD-PRARTNTO         PIC Z(6)9.9(2).                              
009100*                                 ARTIKELPRIS NETTO                       
009200*                                 NET PRICE EACH   (FOB NET)              
009300     03 RAD-REKSIFFR         PIC 9.                                       
009400*                                 KONTROLLSIFFRA                          
009500*                                 PART NO CHECK DIGIT                     
009600     03 RAD-TIAVBOKN         PIC 9(6).                                    
009700*                                 LAGERAVBOKNINGSDATUM                    
009800*                                 STOCK ALLOCATION DATE                   
009900     03 RAD-TIREGDAT         PIC 9(6).                                    
010000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010100*                                 REGISTRATION DATE (YYMMDD)              
010200     03 RAD-TIRES            PIC 9(6).                                    
010300*                                 RESERVATIONSDATUM                       
010400*                                 RESERVATION DATE                        
010500     03 RAD-DARODAT          PIC 9(8).                                    
010600*                                 RESTORDERDATUM       (ÅÅÅÅMMDD)         
010700*                                 BACK ORDER DATE      (YYYYMMDD)         
010800     03 RAD-TITPO            PIC 9(6).                                    
010900*                                 PLANERAD ORDERDATUM                     
011000*                                 PLANNED ORDER DATE                      
011100     03 RAD-KDPRTYP          PIC X.                                       
011200*                                 TYP AV PRISTILLÄMPNING                  
011300*                                 TYPE OF PRICING                         
011400     03 RAD-BEVOLREF         PIC X(10).                                   
011500*                                 VOLVO REFERENS                          
011600*                                 VOLVO REFERENCE                         
011700     03 RAD-FLINVEST         PIC X.                                       
011800*                                 BYTES INVENTERINGSFLAGGA                
011900*                                 EXCHANGE INVESTMENT FLAG                
012000     03 RAD-FLPRTILL         PIC X.                                       
012100*                                 PRISTILLÄGGS FLAGGA                     
012200*                                 PRICE PENALTY FLAG                      
012300     03 RAD-FLTPOBEK         PIC X.                                       
012400*                                 TPO-RAD BEKRÄFTAD                       
012500*                                 TPO ORDER LINE CONFIRMED                
012600     03 RAD-BEKUNDRF         PIC X(15).                                   
012700*                                 KUNDENS REFERENS                        
012800*                                 CUSTOMERS REFERENCE                     
012900     03 RAD-IDKAMPRF         PIC Z(6)9.                                   
013000*                                 KAMPANJREFERENS                         
013100*                                 CAMPAIGN REFERENCE                      
013200     03 RAD-IDLEVNR          PIC X(5).                                    
013300*                                 LEVERANTÖRNUMMER                        
013400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
013500     03 RAD-IDSYSTEM         PIC X(4).                                    
013600*                                 VOLVO VCCS SYSTEMNUMMER                 
013700*                                 VOLVO VCCS SYSTEM NUMBER                
013800     03 RAD-KVBEART-Q        PIC Z(5)9.                                   
013900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
014000*                                 ORDERED QUANTITY ADAPTED                
014100*                                  ITEMS                                  
014200     03 RAD-TIREGTID         PIC 9(6).                                    
014300*                                 REGISTRERINGSTID                        
014400*                                 GENERAL REGISTRATION TIME               
014500     03 RAD-DASENBEK.                                                     
014600*                                 SENASTE BEKRÄFTELSETIDPUNKT             
014700*                                 LATEST CONFIRMATION DATE+TIME           
014800        05 RAD-DASENDAT      PIC 9(8).                                    
014900*                                 SENASTE BEKRÄFTELSEDATUM                
015000*                                 LATEST CONFIRMATION DATE                
015100*                                 (YYYYMMDD)                              
015200        05 RAD-TISENBEK-KL   PIC 9(6).                                    
015300*                                 TIM - MIN - SEK   (HHMMSS)              
015400*                                 HOUR - MINUTE - SEC (HHMMSS)            
015500     03 RAD-DEAL-PR-LINE1.                                                
015600*                                 DEALERPRIS (RAD)                        
015700        05 RAD-IDPRQUES      PIC Z(6)9.                                   
015800*                                 PRISFRÅGA NR                            
015900*                                 PRICE QUESTION NO                       
016000        05 RAD-PRARTNTO-LOC  PIC Z(6)9.9(2).                              
016100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
016200*                                 NET PRICE EACH LOCAL CURRENCY           
016300        05 RAD-PRARTNTO-LOCPREL                                           
016400                             PIC Z(6)9.9(2).                              
016500*                                 PREL NETTO SLUTKUNDSPRIS I              
016600*                                 LOKAL VALUTA                            
016700*                                 PREL NET PRICE - LOCAL CURRENCY         
016800        05 RAD-PRARTBTO-LOC  PIC Z(6)9.9(2).                              
016900*                                 PRIS I LOKAL VALUTA                     
017000*                                 LOCAL GROSS SALES PRICE                 
017100        05 RAD-KDVALISO      PIC X(3).                                    
017200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
017300*                                 CURRENCY CODE BY ISO-STANDARD.          
017400        05 RAD-KDVAT         PIC X(2).                                    
017500*                                 MOMSKOD                                 
017600*                                 VAT CODE                                
017700        05 RAD-RERAB         PIC Z(2)9.9.                                 
017800*                                 RABATTSATS (PROCENT)                    
017900        05 RAD-KDRAB         PIC X(5).                                    
018000*                                 RABATTKOD                               
018100        05 RAD-BEART-VIPS    PIC X(25).                                   
018200*                                 VIPS ARTIKELBENÄMNING                   
018300*                                 PÅ DEALERNS SPRÅK                       
018400     03 RAD-KDORDTYP-LDC     PIC X(2).                                    
018500*                                 ORDERTYP HOS DEALER                     
018600     03 RAD-IDKUNDRF-WIP     PIC X(10).                                   
018700*                                 REPARATIONS ORDERNR, LDC KUND           
018800*                                 WORK ORDER NUMBER, LDC DEALER           
018900     03 RAD-TIREPDAT         PIC 9(6).                                    
019000*                                 REPAIR DATE                             
019100*                                 REPAIR DATE                             
019200     03 RAD-CLEARGROUP.                                                   
019300*                                 CLEARINGAREA FÖR ORDERINGÅNG            
019400        05 RAD-CLEARAREA     OCCURS 7 TIMES.                              
019500*                                 CLEARINGAREA FÖR ORDERINGÅNG            
019600           07 RAD-IDDC-CLEAR PIC X(2).                                    
019700*                                 LAGERPRIORITERING VID                   
019800*                                 ORDERCLEARING                           
019900*                                 WAREHOUSE PRIORITY FOR ORDER            
020000*                                 CLEARING                                
020100           07 RAD-FLLF       PIC X.                                       
020200*                                 ARTIKEL LAGERFÖRES                      
020300*                                 PART IN STOCK                           
020400           07 RAD-FLCLEAR    PIC X.                                       
020500*                                 ORDERRAD CLEAR FLAGGA                   
020600*                                 CLEARING FLAG FOR ORDER LINE            
020700     03 RAD-PRAVCOST         PIC Z(6)9.9(2).                              
020800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
020900*                                 AVERAGE COST FOREIGN CURRENCY           
021000     03 RAD-KDROPACK         PIC X.                                       
021100*                                 FRISLÄPPNINGSKOD RO/DO                  
021200*                                 CONSOLIDATION BO/DO                     
021300     03 RAD-IDARBREF         PIC X(10).                                   
021400*                                 ARBETSORDER SOFT/DÖSKALLE               
021500*                                 WORK ORDER SOFT/DUMMY ORDERHEAD         
021600     03 RAD-FILLER           PIC X(10).                                   
021700*** END OF VILMAII-COPY LENGTH= 389 BYTES                                 
