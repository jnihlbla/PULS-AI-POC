000100 01  ORAD-WDE411.                                                         
000200*                                 KUNDORDERREG                            
000300*                                 RADSEGMENT                              
000400*                                 FYSISK NYCKEL: IDPURAD                  
000500     03 ORAD-IDPURAD         PIC S9(5)           COMP-3.                  
000600*                                 RADNUMMER PÅ PACKUNDERLAG               
000700*                                 LINENO IN PACKINGDOCUMENT               
000800     03 ORAD-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 ORAD-REKSIFFR        PIC S9              COMP-3.                  
001200*                                 KONTROLLSIFFRA                          
001300*                                 PART NO CHECK DIGIT                     
001400     03 ORAD-ADLAGOMR        PIC S9(3)           COMP-3.                  
001500*                                 LAGEROMRÅDE                             
001600*                                 AREA                                    
001700     03 ORAD-ADLEVPL         PIC S9(3)           COMP-3.                  
001800*                                 LEVERANSPLATS                           
001900*                                 DELIVERY PLACE                          
002000     03 ORAD-BEART           PIC X(25).                                   
002100*                                 ARTIKELBENÄMNING                        
002200*                                 PART DESCRIPTION                        
002300     03 ORAD-BERADREF        PIC X(10).                                   
002400*                                 KUNDENS RADREFERENS                     
002500*                                 CUSTOMERS ITEM REF.                     
002600     03 ORAD-BEVOLREF        PIC X(10).                                   
002700*                                 VOLVO REFERENS                          
002800*                                 VOLVO REFERENCE                         
002900     03 ORAD-FLDIRLEV        PIC X.                                       
003000*                                 DIREKTLEVERANS ?                        
003100*                                 DIRECT DELIVERY ?                       
003200     03 ORAD-FLFYSAVV        PIC X.                                       
003300*                                 FLAGGA FYSISKA AVVIKELSER               
003400*                                 PHYSICAL DEVIATION FLAG                 
003500     03 ORAD-FLINVEST        PIC X.                                       
003600*                                 BYTES INVENTERINGSFLAGGA                
003700*                                 EXCHANGE INVESTMENT FLAG                
003800     03 ORAD-FLNOLLJ         PIC X.                                       
003900*                                 UPPDATERAD AV NOLLJAGARE                
004000*                                 UPPDATED BY ZERO-HUNTER                 
004100     03 ORAD-FLPRTILL        PIC X.                                       
004200*                                 PRISTILLÄGGS FLAGGA                     
004300*                                 PRICE PENALTY FLAG                      
004400     03 ORAD-FLRESTN         PIC X.                                       
004500*                                 RESTNOTERING ?                          
004600*                                 BACKORDERED ?                           
004700     03 ORAD-FLTILLK         PIC X.                                       
004800*                                 TILLKOMMANDE ARTIKEL ?                  
004900*                                 REPLACEMENT PART FLAG                   
005000     03 ORAD-IDANALYS        PIC X(12).                                   
005100*                                 ANALYSNUMMER                            
005200*                                 ANALYSIS NUMBER                         
005300     03 ORAD-IDDC-RO         PIC X(2).                                    
005400*                                 LAGER DÄR RESTORDER FÅR SKE             
005500*                                 WAREHOUSE FOR BACKORDERS                
005600     03 ORAD-IDKAMPRF        PIC S9(7)           COMP-3.                  
005700*                                 KAMPANJREFERENS                         
005800*                                 CAMPAIGN REFERENCE                      
005900     03 ORAD-IDKONTO         PIC S9(11)          COMP-3.                  
006000*                                 KONTO                                   
006100*                                 ACCOUNT                                 
006200     03 ORAD-IDKST           PIC X(10).                                   
006300*                                 KOSTNADSSTÄLLE                          
006400*                                 COST CENTRE                             
006500     03 ORAD-IDKUNDRF-RO     PIC X(10).                                   
006600*                                 KUND REF PÅ RO                          
006700*                                 CUST REF RO                             
006800     03 ORAD-IDLEVNR         PIC X(5).                                    
006900*                                 LEVERANTÖRNUMMER                        
007000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007100     03 ORAD-IDLOPNR-RO      PIC S9(3)           COMP-3.                  
007200*                                 LÖPNUMMER RO/TPO                        
007300*                                 SEQUENCE NUMBER BO/TPO                  
007400     03 ORAD-IDPRODNR        PIC S9(7)           COMP-3.                  
007500*                                 PRODUKTIONSNUMMER                       
007600*                                 PRODUCTION NUMBER                       
007700     03 ORAD-IDPSN           PIC 9(3).                                    
007800*                                 PROPER SHIPPING NAME                    
007900*                                 PROPER SHIPPING NAME                    
008000     03 ORAD-IDSYSTEM        PIC X(4).                                    
008100*                                 VOLVO VCCS SYSTEMNUMMER                 
008200*                                 VOLVO VCCS SYSTEM NUMBER                
008300     03 ORAD-KDANNULL        PIC X.                                       
008400*                                 CANCELLATION CODE                       
008500*                                 CANCELLATION CODE                       
008600     03 ORAD-KDARTURS        PIC X(2).                                    
008700*                                 ARTIKELURSPRUNGSKOD                     
008800*                                 COUNTRY OF ORIGIN                       
008900     03 ORAD-KDDSP           PIC S9              COMP-3.                  
009000*                                 PÅVERKAN PÅ DSP                         
009100*                                 AFFECT ON DSP                           
009200     03 ORAD-KDFARLIG        PIC S9              COMP-3.                  
009300*                                 KOD FÖR FARLIGT GODS                    
009400*                                 DANGEROUS GOODS CODE                    
009500     03 ORAD-KDFRAKT         PIC S9(3)           COMP-3.                  
009600*                                 FRAKTSÄTT DC TILL KUND                  
009700*                                 FREIGHT CODE                            
009800     03 ORAD-KDFTG           PIC S9(3)           COMP-3.                  
009900*                                 UTGÅTT BYT TILL IDFTG                   
010000*                                 COMPANY CODE                            
010100     03 ORAD-KDKVBRYT        PIC S9              COMP-3.                  
010200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
010300*                                 BREAK BULKPACK CODE                     
010400     03 ORAD-KDOFFERT        PIC S9              COMP-3.                  
010500*                                 PROFORMATYP                             
010600*                                 PROFORMA TYPE                           
010700     03 ORAD-KDOI            PIC X(2).                                    
010800*                                 ORDERINGÅNGSTYP                         
010900*                                 TYPE OF INCOMING ORDER                  
011000     03 ORAD-KDORDING        PIC S9              COMP-3.                  
011100*                                 UPPDATERING ORDERINGÅNG                 
011200*                                 ORDER STATISTICS                        
011300     03 ORAD-KDORDKL         PIC S9              COMP-3.                  
011400*                                 ORDERKLASS                              
011500*                                 ORDER CLASS                             
011600     03 ORAD-KDORDTYP        PIC S9              COMP-3.                  
011700*                                 ORDERTYP                                
011800     03 ORAD-KDPRODSL        PIC S9(3)           COMP-3.                  
011900*                                 PRODUKTSLAG                             
012000*                                 PRODUCT GROUP                           
012100     03 ORAD-KDPRTYP         PIC X.                                       
012200*                                 TYP AV PRISTILLÄMPNING                  
012300*                                 TYPE OF PRICING                         
012400     03 ORAD-KDQPACK         PIC S9              COMP-3.                  
012500*                                 TYP AV KVANT-FÖRPACKNING                
012600*                                 TYPE OF QUANTITY PACK                   
012700     03 ORAD-KDRADSTA        PIC S9              COMP-3.                  
012800*                                 STATUS PÅ ORDERRAD                      
012900*                                 ORDERLINE STATUS                        
013000     03 ORAD-KDTVA           PIC S9(3)           COMP-3.                  
013100*                                 TVA BELGISK MOMS                        
013200*                                 VAT CODE                                
013300     03 ORAD-KDVRINFO        PIC S9              COMP-3.                  
013400*                                 PÅVERKAN I VR/DSP SYSTEM                
013500*                                 VR/DSP UP-DATE                          
013600     03 ORAD-KVANNANT        PIC S9(7)           COMP-3.                  
013700*                                 ANNULLERAT ANTAL ARTIKLAR               
013800*                                 CANCELLED QUANTITY                      
013900     03 ORAD-KVAVBART        PIC S9(7)           COMP-3.                  
014000*                                 AVBOKAT ANTAL ARTIKLAR                  
014100*                                 ALLOCATED QUANTITY                      
014200     03 ORAD-KVBEART         PIC S9(7)           COMP-3.                  
014300*                                 BESTÄLLT ANTAL STYCKEN                  
014400*                                 ORDERED QUANTITY                        
014500     03 ORAD-KVFLAMP         PIC S9(3)           COMP-3.                  
014600*                                 FLAMPUNKT FÖR FARLIGT GODS              
014700*                                 FLASH POINT FOR DANGEROUS GOODS         
014800     03 ORAD-KVLEVART        PIC S9(7)           COMP-3.                  
014900*                                 LEVERERAT ANTAL STYCK                   
015000*                                 DELIVERED QUANTITY                      
015100     03 ORAD-KVSLATT         PIC S9(7)           COMP-3.                  
015200*                                 BERÄKNAD SLATTGRÄNS                     
015300*                                                                         
015400     03 ORAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
015500*                                 ARTIKELPRIS NETTO                       
015600*                                 NET PRICE EACH   (FOB NET)              
015700     03 ORAD-PRARTULL        PIC S9(7)V9(2)      COMP-3.                  
015800*                                 TULLPRIS PER ARTIKEL                    
015900*                                 CUSTOMS VALUE,                          
016000     03 ORAD-SUEQFG          PIC S9(3)V9(4)      COMP-3.                  
016100*                                 EQ-VÄRDE FARLIGT GODS                   
016200*                                 EQ VALUE DANGEROUS GODS                 
016300     03 ORAD-TIPRIS          PIC S9(7)           COMP-3.                  
016400*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
016500*                                 PRICE ADAPTION DATE    (YYMMDD)         
016600     03 ORAD-TIRODAT         PIC S9(7)           COMP-3.                  
016700*                                 RESTORDERDATUM         (ÅÅMMDD)         
016800*                                 BACK ORDER DATE        (YYMMDD)         
016900     03 ORAD-TISLULEV        PIC S9(7)           COMP-3.                  
017000*                                 LEVERANSFÖRSENING (ÅÅMMDD)              
017100*                                 DELAYED DELIVERY (YYMMDD)               
017200     03 ORAD-TIUTSKR         PIC S9(7)           COMP-3.                  
017300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
017400*                                 PRINTING DATE  (YYMMDD)                 
017500     03 ORAD-VKART-FG        PIC S9(7)           COMP-3.                  
017600*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
017700*                                 NET WEIGHT EXPLOSIVES                   
017800     03 ORAD-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
017900*                                 ARTIKELVIKT NETTO (KG) MED EMB          
018000*                                 PART NET WEIGHT (KG) W/ PACKAGE         
018100     03 ORAD-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
018200*                                 ARTIKELVOLYM (CM3)                      
018300*                                 PART VOLUME    (CM3)                    
018400     03 ORAD-VLFG            PIC S9(4)V9(3)      COMP-3.                  
018500*                                 VOLYM FARLIGT GODS                      
018600*                                 VOLUME DANGEROUS GOODS                  
018700     03 ORAD-FLSDCLEV        PIC X.                                       
018800*                                 LEVERANSSTYRNING SDC                    
018900*                                 DELIVERY ONLY SDC                       
019000     03 ORAD-IDBIL.                                                       
019100*                                 BILIDENTITET                            
019200*                                 CAR IDENTITY                            
019300        05 ORAD-IDBILTYP     PIC X(3).                                    
019400*                                 BILTYP                                  
019500*                                 CAR TYPE                                
019600        05 ORAD-TIAAAA       PIC X(4).                                    
019700*                                 ÅRTAL (ÅÅÅÅ)                            
019800*                                 YEAR  (YYYY)                            
019900        05 ORAD-IDCHASSI-PIE PIC X(6).                                    
020000*                                 CHASSINUMMER PIE                        
020100*                                 CHASSI NUMBER PIE                       
020200     03 ORAD-IDARBREF        PIC X(10).                                   
020300*                                 ARBETSORDER VADIS                       
020400*                                 WORK ORDER VADIS                        
020500     03 ORAD-IDKLIENT        PIC X(10).                                   
020600*                                 VADIS KLIENT                            
020700*                                 VADIS CLIENT                            
020800     03 ORAD-IDVIN           PIC X(17).                                   
020900*                                 VIN ID FORDON                           
021000*                                 VEHICLE VIN ID                          
021100     03 ORAD-DEAL-PR-LINE.                                                
021200*                                 DEALERPRIS (RAD)                        
021300        05 ORAD-IDPRQUES     PIC 9(7).                                    
021400*                                 PRISFRÅGA NR                            
021500*                                 PRICE QUESTION NO                       
021600        05 ORAD-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
021700*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
021800*                                 NET PRICE EACH LOCAL CURRENCY           
021900        05 ORAD-PRARTNTO-LOCPREL                                          
022000                             PIC S9(7)V9(2)      COMP-3.                  
022100*                                 PREL NETTO SLUTKUNDSPRIS I              
022200*                                 LOKAL VALUTA                            
022300*                                 PREL NET PRICE - LOCAL CURRENCY         
022400        05 ORAD-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
022500*                                 PRIS I LOKAL VALUTA                     
022600*                                 LOCAL GROSS SALES PRICE                 
022700        05 ORAD-KDVALISO     PIC X(3).                                    
022800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
022900*                                 CURRENCY CODE BY ISO-STANDARD.          
023000        05 ORAD-KDVAT        PIC X(2).                                    
023100*                                 MOMSKOD                                 
023200*                                 VAT CODE                                
023300        05 ORAD-RERAB        PIC S9(2)V9(1)      COMP-3.                  
023400*                                 RABATTSATS (PROCENT)                    
023500        05 ORAD-KDRAB        PIC X(5).                                    
023600*                                 RABATTKOD                               
023700        05 ORAD-BEART-VIPS   PIC X(25).                                   
023800*                                 VIPS ARTIKELBENÄMNING                   
023900*                                 PÅ DEALERNS SPRÅK                       
024000     03 ORAD-IDKUNDRF-WIP    PIC X(10).                                   
024100*                                 REPARATIONS ORDERNR, LDC KUND           
024200*                                 WORK ORDER NUMBER, LDC DEALER           
024300     03 ORAD-CLEARGROUP.                                                  
024400*                                 CLEARINGAREA FÖR ORDERINGÅNG            
024500        05 ORAD-CLEARAREA    OCCURS 7 TIMES.                              
024600*                                 CLEARINGAREA FÖR ORDERINGÅNG            
024700           07 ORAD-IDDC-CLEAR                                             
024800                             PIC X(2).                                    
024900*                                 LAGERPRIORITERING VID                   
025000*                                 ORDERCLEARING                           
025100*                                 WAREHOUSE PRIORITY FOR ORDER            
025200*                                 CLEARING                                
025300           07 ORAD-FLLF      PIC X.                                       
025400*                                 ARTIKEL LAGERFÖRES                      
025500*                                 PART IN STOCK                           
025600           07 ORAD-FLCLEAR   PIC X.                                       
025700*                                 ORDERRAD CLEAR FLAGGA                   
025800*                                 CLEARING FLAG FOR ORDER LINE            
025900     03 ORAD-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
026000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
026100*                                 AVERAGE COST FOREIGN CURRENCY           
026200     03 ORAD-KDVALISO-EXP    PIC X(3).                                    
026300*                                 VALUTAKOD I EXP.FLÖDE(LOK. VAL)         
026400*                                 CURRENCY FOR EXPORT (LOC. CURR)         
026500     03 ORAD-VKART-NTO-KG    PIC S9(4)V9(3)      COMP-3.                  
026600*                                 ART. NETTOVIKT I KG UTAN EMB            
026700*                                 PART NET WEIGHT KG NO PACKAGING         
026800*** END OF VILMAII-COPY LENGTH= 380 BYTES                                 
