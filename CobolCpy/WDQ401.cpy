000100 01  ORAD-WDQ401.                                                         
000200*                                 ORDERRADSREGISTER KÖ                    
000300*                                 FYSISK NYCKEL: WDQ401KY                 
000400*                                 (IDORDER, IDDC,  ADLAGOMR,              
000500*                                  ADGANG,  ADPLATS,   IDARTNR,           
000600*                                  IDLOPNR)                               
000700     03 ORAD-IDORDER         PIC S9(7)           COMP-3.                  
000800*                                 VOLVO PARTS ORDERNUMMER                 
000900*                                 VOLVO PARTS ORDER NUMBER                
001000     03 ORAD-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 ORAD-ADLAGOMR        PIC S9(3)           COMP-3.                  
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600     03 ORAD-ADGANG          PIC S9(3)           COMP-3.                  
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900     03 ORAD-ADPLATS         PIC S9(5)           COMP-3.                  
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200     03 ORAD-IDARTNR         PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 ORAD-IDLOPNR         PIC S9(3)           COMP-3.                  
002600*                                 LÖPNUMMER                               
002700*                                 SEQUENCE NUMBER                         
002800     03 ORAD-BERADREF        PIC X(10).                                   
002900*                                 KUNDENS RADREFERENS                     
003000*                                 CUSTOMERS ITEM REF.                     
003100     03 ORAD-BEVOLREF        PIC X(10).                                   
003200*                                 VOLVO REFERENS                          
003300*                                 VOLVO REFERENCE                         
003400     03 ORAD-FLAKPLOC        PIC X.                                       
003500*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
003600*                                 ORDER LINE FROM "AK" QUEUE              
003700     03 ORAD-FLINVEST        PIC X.                                       
003800*                                 BYTES INVENTERINGSFLAGGA                
003900*                                 EXCHANGE INVESTMENT FLAG                
004000     03 ORAD-FLOBTRAN        PIC X.                                       
004100*                                 ORDERBEKRÄFTELSETRANSAKTION             
004200*                                 ORDERCONFIRMATIONTRANSACTION            
004300     03 ORAD-FLPRTILL        PIC X.                                       
004400*                                 PRISTILLÄGGS FLAGGA                     
004500*                                 PRICE PENALTY FLAG                      
004600     03 ORAD-FLRESTN         PIC X.                                       
004700*                                 RESTNOTERING ?                          
004800*                                 BACKORDERED ?                           
004900     03 ORAD-FLSDCLEV        PIC X.                                       
005000*                                 LEVERANSSTYRNING SDC                    
005100*                                 DELIVERY ONLY SDC                       
005200     03 ORAD-FLTILLK         PIC X.                                       
005300*                                 TILLKOMMANDE ARTIKEL ?                  
005400*                                 REPLACEMENT PART FLAG                   
005500     03 ORAD-IDDC-RO         PIC X(2).                                    
005600*                                 LAGER DÄR RESTORDER FÅR SKE             
005700*                                 WAREHOUSE FOR BACKORDERS                
005800     03 ORAD-IDGMTREF.                                                    
005900*                                 GODSMOTTAGAREREFERENS                   
006000*                                 GOODS RECEIVER REFERENS                 
006100        05 ORAD-IDDISTR      PIC S9(5)           COMP-3.                  
006200*                                 DISTRIKTNUMMER                          
006300*                                 DISTRICT NUMBER                         
006400        05 ORAD-IDKUNDNR     PIC S9(7)           COMP-3.                  
006500*                                 KUNDNUMMER                              
006600*                                 CUSTOMER NO                             
006700        05 ORAD-IDKUNDRF-GRP.                                             
006800*                                 KUNDENS REFERENS (ORDERID)              
006900*                                 CUSTOMER REFERENCE (ORDER ID)           
007000           07 ORAD-IDKUNDRF  PIC X(10).                                   
007100*                                 KUNDENS REFERENS (ORDERID)              
007200*                                 CUSTOMER REFERENCE (ORDER ID)           
007300           07 ORAD-IDORDNR5-FILLER REDEFINES ORAD-IDKUNDRF.               
007400              09 ORAD-IDORDNR5                                            
007500                             PIC 9(5).                                    
007600*                                 ORDERNUMMER                             
007700*                                 ORDER NUMBER                            
007800              09 FILLER      PIC X(5).                                    
007900           07 ORAD-IDORDNR7-FILLER REDEFINES ORAD-IDKUNDRF.               
008000              09 ORAD-IDORDNR7                                            
008100                             PIC 9(7).                                    
008200*                                 ORDERNUMMER                             
008300*                                 ORDER NUMBER                            
008400              09 FILLER      PIC X(3).                                    
008500     03 ORAD-IDKAMPRF        PIC S9(7)           COMP-3.                  
008600*                                 KAMPANJREFERENS                         
008700*                                 CAMPAIGN REFERENCE                      
008800     03 ORAD-IDLEVNR         PIC X(5).                                    
008900*                                 LEVERANTÖRNUMMER                        
009000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
009100     03 ORAD-IDLOPNR-RO      PIC S9(3)           COMP-3.                  
009200*                                 LÖPNUMMER RO/TPO                        
009300*                                 SEQUENCE NUMBER BO/TPO                  
009400     03 ORAD-IDKUNDRF-RO     PIC X(10).                                   
009500*                                 KUND REF PÅ RO                          
009600*                                 CUST REF RO                             
009700     03 ORAD-IDSPECEMB       PIC 9(4).                                    
009800*                                 SPECIALEMBALLAGEID                      
009900*                                 SPECIAL PACKING ID                      
010000     03 ORAD-IDSYSTEM        PIC X(4).                                    
010100*                                 VOLVO VCCS SYSTEMNUMMER                 
010200*                                 VOLVO VCCS SYSTEM NUMBER                
010300     03 ORAD-KDARTURS        PIC X(2).                                    
010400*                                 ARTIKELURSPRUNGSKOD                     
010500*                                 COUNTRY OF ORIGIN                       
010600     03 ORAD-KDDSP           PIC S9              COMP-3.                  
010700*                                 PÅVERKAN PÅ DSP                         
010800*                                 AFFECT ON DSP                           
010900     03 ORAD-KDFARLIG        PIC S9              COMP-3.                  
011000*                                 KOD FÖR FARLIGT GODS                    
011100*                                 DANGEROUS GOODS CODE                    
011200     03 ORAD-KDKVBRYT        PIC S9              COMP-3.                  
011300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
011400*                                 BREAK BULKPACK CODE                     
011500     03 ORAD-KDOI            PIC X(2).                                    
011600*                                 ORDERINGÅNGSTYP                         
011700*                                 TYPE OF INCOMING ORDER                  
011800     03 ORAD-KDORDING        PIC S9              COMP-3.                  
011900*                                 UPPDATERING ORDERINGÅNG                 
012000*                                 ORDER STATISTICS                        
012100     03 ORAD-KDORDKL         PIC S9              COMP-3.                  
012200*                                 ORDERKLASS                              
012300*                                 ORDER CLASS                             
012400     03 ORAD-KDPRODSL        PIC S9(3)           COMP-3.                  
012500*                                 PRODUKTSLAG                             
012600*                                 PRODUCT GROUP                           
012700     03 ORAD-KDPRTYP         PIC X.                                       
012800*                                 TYP AV PRISTILLÄMPNING                  
012900*                                 TYPE OF PRICING                         
013000     03 ORAD-KDSPEEMB        PIC 9.                                       
013100*                                 SPECIALEMBALLAGEKOD                     
013200*                                 SPECIAL PACKING CODE                    
013300     03 ORAD-KDTPOTYP        PIC S9              COMP-3.                  
013400*                                 TYP AV TIDPLANERAD ORDER                
013500*                                 TYPE OF TIME PLANNED ORDER              
013600     03 ORAD-KDVRINFO        PIC S9              COMP-3.                  
013700*                                 PÅVERKAN I VR/DSP SYSTEM                
013800*                                 VR/DSP UP-DATE                          
013900     03 ORAD-KVBEART         PIC S9(7)           COMP-3.                  
014000*                                 BESTÄLLT ANTAL STYCKEN                  
014100*                                 ORDERED QUANTITY                        
014200     03 ORAD-KVBEART-Q       PIC S9(7)           COMP-3.                  
014300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
014400*                                 ORDERED QUANTITY ADAPTED                
014500*                                  ITEMS                                  
014600     03 ORAD-KVPREAVB        PIC S9(7)           COMP-3.                  
014700*                                 PREL-AVB KVANT                          
014800*                                 PREL-RES QUANT                          
014900     03 ORAD-KVPRERO         PIC S9(7)           COMP-3.                  
015000*                                 PRELIMINÄR RO-KVANT                     
015100*                                 PRELIMINARY BO-QUANT                    
015200     03 ORAD-KVSLATT         PIC S9(7)           COMP-3.                  
015300*                                 BERÄKNAD SLATTGRÄNS                     
015400*                                                                         
015500     03 ORAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
015600*                                 ARTIKELPRIS NETTO                       
015700*                                 NET PRICE EACH   (FOB NET)              
015800     03 ORAD-PRBPRIS         PIC S9(7)V9(2)      COMP-3.                  
015900*                                 BASPRIS                                 
016000     03 ORAD-REKSIFFR        PIC S9              COMP-3.                  
016100*                                 KONTROLLSIFFRA                          
016200*                                 PART NO CHECK DIGIT                     
016300     03 ORAD-RERF-RAD        PIC S9V9(4)         COMP-3.                  
016400*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
016500*                                 RATIONING FACTOR ON ORDERLINE           
016600     03 ORAD-TIPRIS          PIC S9(7)           COMP-3.                  
016700*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
016800*                                 PRICE ADAPTION DATE    (YYMMDD)         
016900     03 ORAD-TIREGDAT        PIC S9(7)           COMP-3.                  
017000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
017100*                                 REGISTRATION DATE (YYMMDD)              
017200     03 ORAD-TIREGTID        PIC S9(7)           COMP-3.                  
017300*                                 REGISTRERINGSTID                        
017400*                                 GENERAL REGISTRATION TIME               
017500     03 ORAD-TIRODAT         PIC S9(7)           COMP-3.                  
017600*                                 RESTORDERDATUM         (ÅÅMMDD)         
017700*                                 BACK ORDER DATE        (YYMMDD)         
017800     03 ORAD-TITPO           PIC S9(7)           COMP-3.                  
017900*                                 PLANERAD ORDERDATUM                     
018000*                                 PLANNED ORDER DATE                      
018100     03 ORAD-VKART           PIC S9(7)           COMP-3.                  
018200*                                 ARTIKELVIKT (G)                         
018300*                                 PART WEIGHT (G)                         
018400     03 ORAD-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
018500*                                 ARTIKELVOLYM (CM3)                      
018600*                                 PART VOLUME    (CM3)                    
018700     03 ORAD-IDBIL.                                                       
018800*                                 BILIDENTITET                            
018900*                                 CAR IDENTITY                            
019000        05 ORAD-IDBILTYP     PIC X(3).                                    
019100*                                 BILTYP                                  
019200*                                 CAR TYPE                                
019300        05 ORAD-TIAAAA       PIC X(4).                                    
019400*                                 ÅRTAL (ÅÅÅÅ)                            
019500*                                 YEAR  (YYYY)                            
019600        05 ORAD-IDCHASSI-PIE PIC X(6).                                    
019700*                                 CHASSINUMMER PIE                        
019800*                                 CHASSI NUMBER PIE                       
019900     03 ORAD-IDARBREF        PIC X(10).                                   
020000*                                 ARBETSORDER VADIS                       
020100*                                 WORK ORDER VADIS                        
020200     03 ORAD-IDKLIENT        PIC X(10).                                   
020300*                                 VADIS KLIENT                            
020400*                                 VADIS CLIENT                            
020500     03 ORAD-IDVIN           PIC X(17).                                   
020600*                                 VIN ID FORDON                           
020700*                                 VEHICLE VIN ID                          
020800     03 ORAD-DEAL-PR-LINE.                                                
020900*                                 DEALERPRIS (RAD)                        
021000        05 ORAD-IDPRQUES     PIC 9(7).                                    
021100*                                 PRISFRÅGA NR                            
021200*                                 PRICE QUESTION NO                       
021300        05 ORAD-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
021400*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
021500*                                 NET PRICE EACH LOCAL CURRENCY           
021600        05 ORAD-PRARTNTO-LOCPREL                                          
021700                             PIC S9(7)V9(2)      COMP-3.                  
021800*                                 PREL NETTO SLUTKUNDSPRIS I              
021900*                                 LOKAL VALUTA                            
022000*                                 PREL NET PRICE - LOCAL CURRENCY         
022100        05 ORAD-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
022200*                                 PRIS I LOKAL VALUTA                     
022300*                                 LOCAL GROSS SALES PRICE                 
022400        05 ORAD-KDVALISO     PIC X(3).                                    
022500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
022600*                                 CURRENCY CODE BY ISO-STANDARD.          
022700        05 ORAD-KDVAT        PIC X(2).                                    
022800*                                 MOMSKOD                                 
022900*                                 VAT CODE                                
023000        05 ORAD-RERAB        PIC S9(2)V9(1)      COMP-3.                  
023100*                                 RABATTSATS (PROCENT)                    
023200        05 ORAD-KDRAB        PIC X(5).                                    
023300*                                 RABATTKOD                               
023400        05 ORAD-BEART-VIPS   PIC X(25).                                   
023500*                                 VIPS ARTIKELBENÄMNING                   
023600*                                 PÅ DEALERNS SPRÅK                       
023700     03 ORAD-IDKUNDRF-WIP    PIC X(10).                                   
023800*                                 REPARATIONS ORDERNR, LDC KUND           
023900*                                 WORK ORDER NUMBER, LDC DEALER           
024000     03 ORAD-CLEARGROUP.                                                  
024100*                                 CLEARINGAREA FÖR ORDERINGÅNG            
024200        05 ORAD-CLEARAREA    OCCURS 7 TIMES.                              
024300*                                 CLEARINGAREA FÖR ORDERINGÅNG            
024400           07 ORAD-IDDC-CLEAR                                             
024500                             PIC X(2).                                    
024600*                                 LAGERPRIORITERING VID                   
024700*                                 ORDERCLEARING                           
024800*                                 WAREHOUSE PRIORITY FOR ORDER            
024900*                                 CLEARING                                
025000           07 ORAD-FLLF      PIC X.                                       
025100*                                 ARTIKEL LAGERFÖRES                      
025200*                                 PART IN STOCK                           
025300           07 ORAD-FLCLEAR   PIC X.                                       
025400*                                 ORDERRAD CLEAR FLAGGA                   
025500*                                 CLEARING FLAG FOR ORDER LINE            
025600     03 ORAD-FLORDING        PIC X.                                       
025700*                                 ORDERINGÅNG REDAN BERÄKNAD J/N          
025800*                                 ORDER STATISTICS                        
025900     03 ORAD-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
026000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
026100*                                 AVERAGE COST FOREIGN CURRENCY           
026200     03 ORAD-KVOKS-PREL      PIC S9(7)           COMP-3.                  
026300*                                 PREL ORDERKÖSALDO VERKST.ORDER          
026400*                                 PREL ORDER QUEUE BALANCE                
026500     03 ORAD-VKART-NTO       PIC S9(9)           COMP-3.                  
026600*                                 ARTIKELNS NETTOVIKT                     
026700*                                 PART NET WEIGHT                         
026800*** END OF VILMAII-COPY LENGTH= 335 BYTES                                 
