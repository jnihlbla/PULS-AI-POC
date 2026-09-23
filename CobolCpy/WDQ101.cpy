000100 01  OBKR-WDQ101.                                                         
000200*                                 ORDERBEKRÄFTELSE REGISTER               
000300*                                 FYSISK NYCKEL: WDQ101KY:                
000400*                                 IDORDER  + IDARTNR + IDLOPNR +          
000500*                                 IDSEKVNR + IDDC    + KDORDBEK           
000600     03 OBKR-IDORDER         PIC S9(7)           COMP-3.                  
000700*                                 VOLVO PARTS ORDERNUMMER                 
000800*                                 VOLVO PARTS ORDER NUMBER                
000900     03 OBKR-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 OBKR-IDLOPNR         PIC S9(3)           COMP-3.                  
001300*                                 LÖPNUMMER                               
001400*                                 SEQUENCE NUMBER                         
001500     03 OBKR-IDSEKVNR        PIC S9(3)           COMP-3.                  
001600*                                 GENERELLT SEKVENSNUMMER                 
001700*                                 GENERAL SEQUENCE NUMBER                 
001800     03 OBKR-IDDC            PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 OBKR-KDORDBEK        PIC 9(2).                                    
002200*                                 ORDERBEKRÄFTELSEKOD                     
002300*                                 ORDERCONFIMATIONCODE                    
002400     03 OBKR-BEERS           PIC X(20).                                   
002500*                                 ERSÄTTNINGSTEXT                         
002600*                                 REPLACEMENT TEXT                        
002700     03 OBKR-BEKUNDRF        PIC X(15).                                   
002800*                                 KUNDENS REFERENS                        
002900*                                 CUSTOMERS REFERENCE                     
003000     03 OBKR-BERADREF        PIC X(10).                                   
003100*                                 KUNDENS RADREFERENS                     
003200*                                 CUSTOMERS ITEM REF.                     
003300     03 OBKR-BEVOLREF        PIC X(10).                                   
003400*                                 VOLVO REFERENS                          
003500*                                 VOLVO REFERENCE                         
003600     03 OBKR-DIERS-KVOT      PIC S9(4)V9(3)      COMP-3.                  
003700*                                 KVOT MELLAN                             
003800*                                 DIERS-TILLK OCH DIERS-ERS               
003900*                                 QUOTIENT BETWEEN                        
004000*                                 DIERS-TILLK AND DIERS-ERS               
004100     03 OBKR-FLAKPLOC        PIC X.                                       
004200*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
004300*                                 ORDER LINE FROM "AK" QUEUE              
004400     03 OBKR-FLINVEST        PIC X.                                       
004500*                                 BYTES INVENTERINGSFLAGGA                
004600*                                 EXCHANGE INVESTMENT FLAG                
004700     03 OBKR-FLOBOK          PIC X.                                       
004800*                                 GODKÄND ORDERBEKRÄFTELSERAD             
004900*                                 ACCEPTED ORDERCONFIRMATIONLINE          
005000     03 OBKR-FLOBTRAN        PIC X.                                       
005100*                                 ORDERBEKRÄFTELSETRANSAKTION             
005200*                                 ORDERCONFIRMATIONTRANSACTION            
005300     03 OBKR-FLOBPRT         PIC X.                                       
005400*                                 ORDERBEKRÄFTELSERADEN PRINTAD ?         
005500*                                 ORDERCONFIRMATION LINE PRINTED          
005600*                                 ?                                       
005700     03 OBKR-FLPRTILL        PIC X.                                       
005800*                                 PRISTILLÄGGS FLAGGA                     
005900*                                 PRICE PENALTY FLAG                      
006000     03 OBKR-FLRESTN         PIC X.                                       
006100*                                 RESTNOTERING ?                          
006200*                                 BACKORDERED ?                           
006300     03 OBKR-FLSLATT         PIC X.                                       
006400*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
006500*                                 LL BERÄKNAS ELLER EJ                    
006600*                                 OM FLRESTN = J OCH FLSLATT = J,         
006700*                                  DÅ BERÄKNAS KVSLATT                    
006800     03 OBKR-FLTILLK         PIC X.                                       
006900*                                 TILLKOMMANDE ARTIKEL ?                  
007000*                                 REPLACEMENT PART FLAG                   
007100     03 OBKR-IDARTNR-TILLK   PIC S9(9)           COMP-3.                  
007200*                                 TILLKOMMANDE ARTIKELNUMMER              
007300*                                 REPLACEMENT PART NO.                    
007400     03 OBKR-IDDC-RO         PIC X(2).                                    
007500*                                 LAGER DÄR RESTORDER FÅR SKE             
007600*                                 WAREHOUSE FOR BACKORDERS                
007700     03 OBKR-IDGMTREF.                                                    
007800*                                 GODSMOTTAGAREREFERENS                   
007900*                                 GOODS RECEIVER REFERENS                 
008000        05 OBKR-IDDISTR      PIC S9(5)           COMP-3.                  
008100*                                 DISTRIKTNUMMER                          
008200*                                 DISTRICT NUMBER                         
008300        05 OBKR-IDKUNDNR     PIC S9(7)           COMP-3.                  
008400*                                 KUNDNUMMER                              
008500*                                 CUSTOMER NO                             
008600        05 OBKR-IDKUNDRF-GRP.                                             
008700*                                 KUNDENS REFERENS (ORDERID)              
008800*                                 CUSTOMER REFERENCE (ORDER ID)           
008900           07 OBKR-IDKUNDRF  PIC X(10).                                   
009000*                                 KUNDENS REFERENS (ORDERID)              
009100*                                 CUSTOMER REFERENCE (ORDER ID)           
009200           07 OBKR-IDORDNR5-FILLER REDEFINES OBKR-IDKUNDRF.               
009300              09 OBKR-IDORDNR5                                            
009400                             PIC 9(5).                                    
009500*                                 ORDERNUMMER                             
009600*                                 ORDER NUMBER                            
009700              09 FILLER      PIC X(5).                                    
009800           07 OBKR-IDORDNR7-FILLER REDEFINES OBKR-IDKUNDRF.               
009900              09 OBKR-IDORDNR7                                            
010000                             PIC 9(7).                                    
010100*                                 ORDERNUMMER                             
010200*                                 ORDER NUMBER                            
010300              09 FILLER      PIC X(3).                                    
010400     03 OBKR-IDKAMPRF        PIC S9(7)           COMP-3.                  
010500*                                 KAMPANJREFERENS                         
010600*                                 CAMPAIGN REFERENCE                      
010700     03 OBKR-IDPGM           PIC X(8).                                    
010800*                                 PROGRAM IDENTITET                       
010900*                                 PROGRAM INTENTITY                       
011000     03 OBKR-FILLERX2        PIC X(2).                                    
011100     03 OBKR-IDKUNDRF-RO     PIC X(10).                                   
011200*                                 KUND REF PÅ RO                          
011300*                                 CUST REF RO                             
011400     03 OBKR-IDLEVNR         PIC X(5).                                    
011500*                                 LEVERANTÖRNUMMER                        
011600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
011700     03 OBKR-IDLOPNR-RO      PIC S9(3)           COMP-3.                  
011800*                                 LÖPNUMMER RO/TPO                        
011900*                                 SEQUENCE NUMBER BO/TPO                  
012000     03 OBKR-IDSYSTEM        PIC X(4).                                    
012100*                                 VOLVO VCCS SYSTEMNUMMER                 
012200*                                 VOLVO VCCS SYSTEM NUMBER                
012300     03 OBKR-KDDSP           PIC S9              COMP-3.                  
012400*                                 PÅVERKAN PÅ DSP                         
012500*                                 AFFECT ON DSP                           
012600     03 OBKR-KDERS           PIC S9(3)           COMP-3.                  
012700*                                 ERSÄTTNINGSKOD                          
012800*                                 SUPERSESSION CODE                       
012900     03 OBKR-KDKVBRYT        PIC S9              COMP-3.                  
013000*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
013100*                                 BREAK BULKPACK CODE                     
013200     03 OBKR-KDOI            PIC X(2).                                    
013300*                                 ORDERINGÅNGSTYP                         
013400*                                 TYPE OF INCOMING ORDER                  
013500     03 OBKR-KDPRTYP         PIC X.                                       
013600*                                 TYP AV PRISTILLÄMPNING                  
013700*                                 TYPE OF PRICING                         
013800     03 OBKR-KDTPOTYP        PIC S9              COMP-3.                  
013900*                                 TYP AV TIDPLANERAD ORDER                
014000*                                 TYPE OF TIME PLANNED ORDER              
014100     03 OBKR-KDVRINFO        PIC S9              COMP-3.                  
014200*                                 PÅVERKAN I VR/DSP SYSTEM                
014300*                                 VR/DSP UP-DATE                          
014400     03 OBKR-KVANNANT        PIC S9(7)           COMP-3.                  
014500*                                 ANNULLERAT ANTAL ARTIKLAR               
014600*                                 CANCELLED QUANTITY                      
014700     03 OBKR-KVAVBART        PIC S9(7)           COMP-3.                  
014800*                                 AVBOKAT ANTAL ARTIKLAR                  
014900*                                 ALLOCATED QUANTITY                      
015000     03 OBKR-KVBEART         PIC S9(7)           COMP-3.                  
015100*                                 BESTÄLLT ANTAL STYCKEN                  
015200*                                 ORDERED QUANTITY                        
015300     03 OBKR-KVBEART-Q       PIC S9(7)           COMP-3.                  
015400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
015500*                                 ORDERED QUANTITY ADAPTED                
015600*                                  ITEMS                                  
015700     03 OBKR-KVBEART-TILLK   PIC S9(7)           COMP-3.                  
015800*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
015900*                                 ORD. QUANT NEW PART                     
016000     03 OBKR-KVPREAVB        PIC S9(7)           COMP-3.                  
016100*                                 PREL-AVB KVANT                          
016200*                                 PREL-RES QUANT                          
016300     03 OBKR-KVPRERO         PIC S9(7)           COMP-3.                  
016400*                                 PRELIMINÄR RO-KVANT                     
016500*                                 PRELIMINARY BO-QUANT                    
016600     03 OBKR-KVQPACK         PIC S9(5)           COMP-3.                  
016700*                                 ANTAL KVANTITETFÖRPACKNINGAR            
016800*                                 NO OF BULK PACKS                        
016900     03 OBKR-KVRO            PIC S9(7)           COMP-3.                  
017000*                                 ANTAL RESTNOTERADE ARTIKLAR             
017100*                                 BACKORDERED QTY                         
017200     03 OBKR-KVSLATT         PIC S9(7)           COMP-3.                  
017300*                                 BERÄKNAD SLATTGRÄNS                     
017400*                                                                         
017500     03 OBKR-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
017600*                                 ARTIKELPRIS NETTO                       
017700*                                 NET PRICE EACH   (FOB NET)              
017800     03 OBKR-PRBPRIS         PIC S9(7)V9(2)      COMP-3.                  
017900*                                 BASPRIS                                 
018000     03 OBKR-REKSIFFR        PIC S9              COMP-3.                  
018100*                                 KONTROLLSIFFRA                          
018200*                                 PART NO CHECK DIGIT                     
018300     03 OBKR-REKSIFFR-TILLK  PIC S9              COMP-3.                  
018400*                                 TILLKOMMANDE KONTROLLSIFFRA             
018500*                                 CHECK DIGIT REPLACING PART              
018600     03 OBKR-RERF-RAD        PIC S9V9(4)         COMP-3.                  
018700*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
018800*                                 RATIONING FACTOR ON ORDERLINE           
018900     03 OBKR-TIDISPIN        PIC S9(7)           COMP-3.                  
019000*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
019100*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
019200     03 OBKR-TIORDREG        PIC S9(7)           COMP-3.                  
019300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
019400*                                 ORDER REGISTRATION DATE  YYMMDD         
019500     03 OBKR-TIPRIS          PIC S9(7)           COMP-3.                  
019600*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
019700*                                 PRICE ADAPTION DATE    (YYMMDD)         
019800     03 OBKR-TIREGDAT        PIC S9(7)           COMP-3.                  
019900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
020000*                                 REGISTRATION DATE (YYMMDD)              
020100     03 OBKR-TIREGTID        PIC S9(7)           COMP-3.                  
020200*                                 REGISTRERINGSTID                        
020300*                                 GENERAL REGISTRATION TIME               
020400     03 OBKR-TIRODAT         PIC S9(7)           COMP-3.                  
020500*                                 RESTORDERDATUM         (ÅÅMMDD)         
020600*                                 BACK ORDER DATE        (YYMMDD)         
020700     03 OBKR-TITIREGD-9KOMPL PIC S9(9)           COMP-3.                  
020800*                                 DATUMETS 9-KOMPLEMENT                   
020900*                                 DATES 9-COMPLEMENT                      
021000     03 OBKR-TITPO           PIC S9(7)           COMP-3.                  
021100*                                 PLANERAD ORDERDATUM                     
021200*                                 PLANNED ORDER DATE                      
021300     03 OBKR-TITIORDD-9KOMPL PIC S9(9)           COMP-3.                  
021400*                                 DATUMETS 9-KOMPLEMENT                   
021500*                                 DATES 9-COMPLEMENT                      
021600     03 OBKR-KDFRAKT         PIC S9(3)           COMP-3.                  
021700*                                 FRAKTSÄTT DC TILL KUND                  
021800*                                 FREIGHT CODE                            
021900     03 OBKR-KDORDKL         PIC S9              COMP-3.                  
022000*                                 ORDERKLASS                              
022100*                                 ORDER CLASS                             
022200     03 OBKR-IDBIL.                                                       
022300*                                 BILIDENTITET                            
022400*                                 CAR IDENTITY                            
022500        05 OBKR-IDBILTYP     PIC X(3).                                    
022600*                                 BILTYP                                  
022700*                                 CAR TYPE                                
022800        05 OBKR-TIAAAA       PIC X(4).                                    
022900*                                 ÅRTAL (ÅÅÅÅ)                            
023000*                                 YEAR  (YYYY)                            
023100        05 OBKR-IDCHASSI-PIE PIC X(6).                                    
023200*                                 CHASSINUMMER PIE                        
023300*                                 CHASSI NUMBER PIE                       
023400     03 OBKR-DEAL-PR-LINE.                                                
023500*                                 DEALERPRIS (RAD)                        
023600        05 OBKR-IDPRQUES     PIC 9(7).                                    
023700*                                 PRISFRÅGA NR                            
023800*                                 PRICE QUESTION NO                       
023900        05 OBKR-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
024000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
024100*                                 NET PRICE EACH LOCAL CURRENCY           
024200        05 OBKR-PRARTNTO-LOCPREL                                          
024300                             PIC S9(7)V9(2)      COMP-3.                  
024400*                                 PREL NETTO SLUTKUNDSPRIS I              
024500*                                 LOKAL VALUTA                            
024600*                                 PREL NET PRICE - LOCAL CURRENCY         
024700        05 OBKR-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
024800*                                 PRIS I LOKAL VALUTA                     
024900*                                 LOCAL GROSS SALES PRICE                 
025000        05 OBKR-KDVALISO     PIC X(3).                                    
025100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
025200*                                 CURRENCY CODE BY ISO-STANDARD.          
025300        05 OBKR-KDVAT        PIC X(2).                                    
025400*                                 MOMSKOD                                 
025500*                                 VAT CODE                                
025600        05 OBKR-RERAB        PIC S9(2)V9(1)      COMP-3.                  
025700*                                 RABATTSATS (PROCENT)                    
025800        05 OBKR-KDRAB        PIC X(5).                                    
025900*                                 RABATTKOD                               
026000        05 OBKR-BEART-VIPS   PIC X(25).                                   
026100*                                 VIPS ARTIKELBENÄMNING                   
026200*                                 PÅ DEALERNS SPRÅK                       
026300     03 OBKR-KDORDTYP-LDC    PIC X(2).                                    
026400*                                 ORDERTYP HOS DEALER                     
026500     03 OBKR-IDKUNDRF-WIP    PIC X(10).                                   
026600*                                 REPARATIONS ORDERNR, LDC KUND           
026700*                                 WORK ORDER NUMBER, LDC DEALER           
026800     03 OBKR-TIREPDAT        PIC S9(7)           COMP-3.                  
026900*                                 REPAIR DATE                             
027000*                                 REPAIR DATE                             
027100     03 OBKR-CLEARGROUP.                                                  
027200*                                 CLEARINGAREA FÖR ORDERINGÅNG            
027300        05 OBKR-CLEARAREA    OCCURS 7 TIMES.                              
027400*                                 CLEARINGAREA FÖR ORDERINGÅNG            
027500           07 OBKR-IDDC-CLEAR                                             
027600                             PIC X(2).                                    
027700*                                 LAGERPRIORITERING VID                   
027800*                                 ORDERCLEARING                           
027900*                                 WAREHOUSE PRIORITY FOR ORDER            
028000*                                 CLEARING                                
028100           07 OBKR-FLLF      PIC X.                                       
028200*                                 ARTIKEL LAGERFÖRES                      
028300*                                 PART IN STOCK                           
028400           07 OBKR-FLCLEAR   PIC X.                                       
028500*                                 ORDERRAD CLEAR FLAGGA                   
028600*                                 CLEARING FLAG FOR ORDER LINE            
028700     03 OBKR-TIDLEVDAT       PIC S9(7)           COMP-3.                  
028800*                                 OMRÄKNAT DATUM FÖR DDGS-ORDER           
028900*                                 CHANGED DATE FOR DDGS-ORDER             
029000     03 OBKR-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
029100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
029200*                                 AVERAGE COST FOREIGN CURRENCY           
029300*** END OF VILMAII-COPY LENGTH= 373 BYTES                                 
