000100 01  WDQ101EX.                                                            
000200*                                 A COPY OF WDQ101 TO GENERATE EP         
000300*                                 LUSCPY                                  
000400*                                 WDQ101EX WHICH IS USED WITH LIN         
000500*                                 E-                                      
000600*                                 COMMAND EPLX IN VILMA FUNCTION          
000700*                                 FILLI                                   
000800*                                 TO GENERATE E+ PROGRAM TO CREAT         
000900*                                 E A                                     
001000*                                 TAB-SEPARATED FILE TO SEND TO T         
001100*                                 HE                                      
001200*                                 DATALAKE.                               
001300*                                                                         
001400*                                 NOTE THAT IF WDQ101 IS MODIFIED         
001500*                                 ,THIS                                   
001600*                                 COPYBOOK AND EPLUS NEEDS REGENE         
001700*                                 RATION                                  
001800     03 IDSEGM               PIC X(6).                                    
001900*                                 SEGMENT                                 
002000     03 FILLERX2-IDSEGM      PIC X(2).                                    
002100     03 IDORDER-KEY          PIC S9(7)           COMP-3.                  
002200*                                 VOLVO PARTS ORDERNUMMER                 
002300*                                 VOLVO PARTS ORDER NUMBER                
002400     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700     03 IDLOPNR-KEY          PIC S9(3)           COMP-3.                  
002800*                                 LÖPNUMMER                               
002900*                                 SEQUENCE NUMBER                         
003000     03 IDSEKVNR-KEY         PIC S9(3)           COMP-3.                  
003100*                                 GENERELLT SEKVENSNUMMER                 
003200*                                 GENERAL SEQUENCE NUMBER                 
003300     03 IDDC-KEY             PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500*                                 WAREHOUSE IDENTIFIER                    
003600     03 KDORDBEK-KEY         PIC 9(2).                                    
003700*                                 ORDERBEKRÄFTELSEKOD                     
003800*                                 ORDERCONFIMATIONCODE                    
003900     03 FILLER               PIC X(21).                                   
004000     03 IDORDER              PIC S9(7)           COMP-3.                  
004100*                                 VOLVO PARTS ORDERNUMMER                 
004200*                                 VOLVO PARTS ORDER NUMBER                
004300     03 IDARTNR              PIC S9(9)           COMP-3.                  
004400*                                 ARTIKELNUMMER                           
004500*                                 PART NUMBER                             
004600     03 IDLOPNR              PIC S9(3)           COMP-3.                  
004700*                                 LÖPNUMMER                               
004800*                                 SEQUENCE NUMBER                         
004900     03 IDSEKVNR             PIC S9(3)           COMP-3.                  
005000*                                 GENERELLT SEKVENSNUMMER                 
005100*                                 GENERAL SEQUENCE NUMBER                 
005200     03 IDDC                 PIC X(2).                                    
005300*                                 IDENTIFIERARE LAGER                     
005400*                                 WAREHOUSE IDENTIFIER                    
005500     03 KDORDBEK             PIC 9(2).                                    
005600*                                 ORDERBEKRÄFTELSEKOD                     
005700*                                 ORDERCONFIMATIONCODE                    
005800     03 BEERS                PIC X(20).                                   
005900*                                 ERSÄTTNINGSTEXT                         
006000*                                 REPLACEMENT TEXT                        
006100     03 BEKUNDRF             PIC X(15).                                   
006200*                                 KUNDENS REFERENS                        
006300*                                 CUSTOMERS REFERENCE                     
006400     03 BERADREF             PIC X(10).                                   
006500*                                 KUNDENS RADREFERENS                     
006600*                                 CUSTOMERS ITEM REF.                     
006700     03 BEVOLREF             PIC X(10).                                   
006800*                                 VOLVO REFERENS                          
006900*                                 VOLVO REFERENCE                         
007000     03 DIERS-KVOT           PIC S9(4)V9(3)      COMP-3.                  
007100*                                 KVOT MELLAN                             
007200*                                 DIERS-TILLK OCH DIERS-ERS               
007300*                                 QUOTIENT BETWEEN                        
007400*                                 DIERS-TILLK AND DIERS-ERS               
007500     03 FLAKPLOC             PIC X.                                       
007600*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
007700*                                 ORDER LINE FROM "AK" QUEUE              
007800     03 FLINVEST             PIC X.                                       
007900*                                 BYTES INVENTERINGSFLAGGA                
008000*                                 EXCHANGE INVESTMENT FLAG                
008100     03 FLOBOK               PIC X.                                       
008200*                                 GODKÄND ORDERBEKRÄFTELSERAD             
008300*                                 ACCEPTED ORDERCONFIRMATIONLINE          
008400     03 FLOBTRAN             PIC X.                                       
008500*                                 ORDERBEKRÄFTELSETRANSAKTION             
008600*                                 ORDERCONFIRMATIONTRANSACTION            
008700     03 FLOBPRT              PIC X.                                       
008800*                                 ORDERBEKRÄFTELSERADEN PRINTAD ?         
008900*                                 ORDERCONFIRMATION LINE PRINTED          
009000*                                 ?                                       
009100     03 FLPRTILL             PIC X.                                       
009200*                                 PRISTILLÄGGS FLAGGA                     
009300*                                 PRICE PENALTY FLAG                      
009400     03 FLRESTN              PIC X.                                       
009500*                                 RESTNOTERING ?                          
009600*                                 BACKORDERED ?                           
009700     03 FLSLATT              PIC X.                                       
009800*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
009900*                                 LL BERÄKNAS ELLER EJ                    
010000*                                 OM FLRESTN = J OCH FLSLATT = J,         
010100*                                  DÅ BERÄKNAS KVSLATT                    
010200     03 FLTILLK              PIC X.                                       
010300*                                 TILLKOMMANDE ARTIKEL ?                  
010400*                                 REPLACEMENT PART FLAG                   
010500     03 IDARTNR-TILLK        PIC S9(9)           COMP-3.                  
010600*                                 TILLKOMMANDE ARTIKELNUMMER              
010700*                                 REPLACEMENT PART NO.                    
010800     03 IDDC-RO              PIC X(2).                                    
010900*                                 LAGER DÄR RESTORDER FÅR SKE             
011000*                                 WAREHOUSE FOR BACKORDERS                
011100     03 IDGMTREF.                                                         
011200*                                 GODSMOTTAGAREREFERENS                   
011300*                                 GOODS RECEIVER REFERENS                 
011400        05 IDDISTR           PIC S9(5)           COMP-3.                  
011500*                                 DISTRIKTNUMMER                          
011600*                                 DISTRICT NUMBER                         
011700        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
011800*                                 KUNDNUMMER                              
011900*                                 CUSTOMER NO                             
012000        05 IDKUNDRF-GRP.                                                  
012100*                                 KUNDENS REFERENS (ORDERID)              
012200*                                 CUSTOMER REFERENCE (ORDER ID)           
012300           07 IDKUNDRF       PIC X(10).                                   
012400*                                 KUNDENS REFERENS (ORDERID)              
012500*                                 CUSTOMER REFERENCE (ORDER ID)           
012600           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
012700              09 IDORDNR5    PIC 9(5).                                    
012800*                                 ORDERNUMMER                             
012900*                                 ORDER NUMBER                            
013000              09 FILLER      PIC X(5).                                    
013100           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
013200              09 IDORDNR7    PIC 9(7).                                    
013300*                                 ORDERNUMMER                             
013400*                                 ORDER NUMBER                            
013500              09 FILLER      PIC X(3).                                    
013600     03 IDKAMPRF             PIC S9(7)           COMP-3.                  
013700*                                 KAMPANJREFERENS                         
013800*                                 CAMPAIGN REFERENCE                      
013900     03 IDPGM                PIC X(8).                                    
014000*                                 PROGRAM IDENTITET                       
014100*                                 PROGRAM INTENTITY                       
014200     03 FILLERX2             PIC X(2).                                    
014300     03 IDKUNDRF-RO          PIC X(10).                                   
014400*                                 KUND REF PÅ RO                          
014500*                                 CUST REF RO                             
014600     03 IDLEVNR              PIC X(5).                                    
014700*                                 LEVERANTÖRNUMMER                        
014800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
014900     03 IDLOPNR-RO           PIC S9(3)           COMP-3.                  
015000*                                 LÖPNUMMER RO/TPO                        
015100*                                 SEQUENCE NUMBER BO/TPO                  
015200     03 IDSYSTEM             PIC X(4).                                    
015300*                                 VOLVO VCCS SYSTEMNUMMER                 
015400*                                 VOLVO VCCS SYSTEM NUMBER                
015500     03 KDDSP                PIC S9              COMP-3.                  
015600*                                 PÅVERKAN PÅ DSP                         
015700*                                 AFFECT ON DSP                           
015800     03 KDERS                PIC S9(3)           COMP-3.                  
015900*                                 ERSÄTTNINGSKOD                          
016000*                                 SUPERSESSION CODE                       
016100     03 KDKVBRYT             PIC S9              COMP-3.                  
016200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
016300*                                 BREAK BULKPACK CODE                     
016400     03 KDOI                 PIC X(2).                                    
016500*                                 ORDERINGÅNGSTYP                         
016600*                                 TYPE OF INCOMING ORDER                  
016700     03 KDPRTYP              PIC X.                                       
016800*                                 TYP AV PRISTILLÄMPNING                  
016900*                                 TYPE OF PRICING                         
017000     03 KDTPOTYP             PIC S9              COMP-3.                  
017100*                                 TYP AV TIDPLANERAD ORDER                
017200*                                 TYPE OF TIME PLANNED ORDER              
017300     03 KDVRINFO             PIC S9              COMP-3.                  
017400*                                 PÅVERKAN I VR/DSP SYSTEM                
017500*                                 VR/DSP UP-DATE                          
017600     03 KVANNANT             PIC S9(7)           COMP-3.                  
017700*                                 ANNULLERAT ANTAL ARTIKLAR               
017800*                                 CANCELLED QUANTITY                      
017900     03 KVAVBART             PIC S9(7)           COMP-3.                  
018000*                                 AVBOKAT ANTAL ARTIKLAR                  
018100*                                 ALLOCATED QUANTITY                      
018200     03 KVBEART              PIC S9(7)           COMP-3.                  
018300*                                 BESTÄLLT ANTAL STYCKEN                  
018400*                                 ORDERED QUANTITY                        
018500     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
018600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
018700*                                 ORDERED QUANTITY ADAPTED                
018800*                                  ITEMS                                  
018900     03 KVBEART-TILLK        PIC S9(7)           COMP-3.                  
019000*                                 BESTÄLLT ANTAL TILLKOMMANDE ART         
019100*                                 ORD. QUANT NEW PART                     
019200     03 KVPREAVB             PIC S9(7)           COMP-3.                  
019300*                                 PREL-AVB KVANT                          
019400*                                 PREL-RES QUANT                          
019500     03 KVPRERO              PIC S9(7)           COMP-3.                  
019600*                                 PRELIMINÄR RO-KVANT                     
019700*                                 PRELIMINARY BO-QUANT                    
019800     03 KVQPACK              PIC S9(5)           COMP-3.                  
019900*                                 ANTAL KVANTITETFÖRPACKNINGAR            
020000*                                 NO OF BULK PACKS                        
020100     03 KVRO                 PIC S9(7)           COMP-3.                  
020200*                                 ANTAL RESTNOTERADE ARTIKLAR             
020300*                                 BACKORDERED QTY                         
020400     03 KVSLATT              PIC S9(7)           COMP-3.                  
020500*                                 BERÄKNAD SLATTGRÄNS                     
020600*                                                                         
020700     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
020800*                                 ARTIKELPRIS NETTO                       
020900*                                 NET PRICE EACH   (FOB NET)              
021000     03 PRBPRIS              PIC S9(7)V9(2)      COMP-3.                  
021100*                                 BASPRIS                                 
021200     03 REKSIFFR             PIC S9              COMP-3.                  
021300*                                 KONTROLLSIFFRA                          
021400*                                 PART NO CHECK DIGIT                     
021500     03 REKSIFFR-TILLK       PIC S9              COMP-3.                  
021600*                                 TILLKOMMANDE KONTROLLSIFFRA             
021700*                                 CHECK DIGIT REPLACING PART              
021800     03 RERF-RAD             PIC S9V9(4)         COMP-3.                  
021900*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
022000*                                 RATIONING FACTOR ON ORDERLINE           
022100     03 TIDISPIN             PIC S9(7)           COMP-3.                  
022200*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
022300*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
022400     03 TIORDREG             PIC S9(7)           COMP-3.                  
022500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
022600*                                 ORDER REGISTRATION DATE  YYMMDD         
022700     03 TIPRIS               PIC S9(7)           COMP-3.                  
022800*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
022900*                                 PRICE ADAPTION DATE    (YYMMDD)         
023000     03 TIREGDAT             PIC S9(7)           COMP-3.                  
023100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
023200*                                 REGISTRATION DATE (YYMMDD)              
023300     03 TIREGTID             PIC S9(7)           COMP-3.                  
023400*                                 REGISTRERINGSTID                        
023500*                                 GENERAL REGISTRATION TIME               
023600     03 TIRODAT              PIC S9(7)           COMP-3.                  
023700*                                 RESTORDERDATUM         (ÅÅMMDD)         
023800*                                 BACK ORDER DATE        (YYMMDD)         
023900     03 TITIREGD-9KOMPL      PIC S9(9)           COMP-3.                  
024000*                                 DATUMETS 9-KOMPLEMENT                   
024100*                                 DATES 9-COMPLEMENT                      
024200     03 TITPO                PIC S9(7)           COMP-3.                  
024300*                                 PLANERAD ORDERDATUM                     
024400*                                 PLANNED ORDER DATE                      
024500     03 TITIORDD-9KOMPL      PIC S9(9)           COMP-3.                  
024600*                                 DATUMETS 9-KOMPLEMENT                   
024700*                                 DATES 9-COMPLEMENT                      
024800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
024900*                                 FRAKTSÄTT DC TILL KUND                  
025000*                                 FREIGHT CODE                            
025100     03 KDORDKL              PIC S9              COMP-3.                  
025200*                                 ORDERKLASS                              
025300*                                 ORDER CLASS                             
025400     03 IDBIL.                                                            
025500*                                 BILIDENTITET                            
025600*                                 CAR IDENTITY                            
025700        05 IDBILTYP          PIC X(3).                                    
025800*                                 BILTYP                                  
025900*                                 CAR TYPE                                
026000        05 TIAAAA            PIC X(4).                                    
026100*                                 ÅRTAL (ÅÅÅÅ)                            
026200*                                 YEAR  (YYYY)                            
026300        05 IDCHASSI-PIE      PIC X(6).                                    
026400*                                 CHASSINUMMER PIE                        
026500*                                 CHASSI NUMBER PIE                       
026600     03 DEAL-PR-LINE.                                                     
026700*                                 DEALERPRIS (RAD)                        
026800        05 IDPRQUES          PIC 9(7).                                    
026900*                                 PRISFRÅGA NR                            
027000*                                 PRICE QUESTION NO                       
027100        05 PRARTNTO-LOC      PIC S9(7)V9(2)      COMP-3.                  
027200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
027300*                                 NET PRICE EACH LOCAL CURRENCY           
027400        05 PRARTNTO-LOCPREL  PIC S9(7)V9(2)      COMP-3.                  
027500*                                 PREL NETTO SLUTKUNDSPRIS I              
027600*                                 LOKAL VALUTA                            
027700*                                 PREL NET PRICE - LOCAL CURRENCY         
027800        05 PRARTBTO-LOC      PIC S9(7)V9(2)      COMP-3.                  
027900*                                 PRIS I LOKAL VALUTA                     
028000*                                 LOCAL GROSS SALES PRICE                 
028100        05 KDVALISO          PIC X(3).                                    
028200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
028300*                                 CURRENCY CODE BY ISO-STANDARD.          
028400        05 KDVAT             PIC X(2).                                    
028500*                                 MOMSKOD                                 
028600*                                 VAT CODE                                
028700        05 RERAB             PIC S9(2)V9(1)      COMP-3.                  
028800*                                 RABATTSATS (PROCENT)                    
028900        05 KDRAB             PIC X(5).                                    
029000*                                 RABATTKOD                               
029100        05 BEART-VIPS        PIC X(25).                                   
029200*                                 VIPS ARTIKELBENÄMNING                   
029300*                                 PÅ DEALERNS SPRÅK                       
029400     03 KDORDTYP-LDC         PIC X(2).                                    
029500*                                 ORDERTYP HOS DEALER                     
029600     03 IDKUNDRF-WIP         PIC X(10).                                   
029700*                                 REPARATIONS ORDERNR, LDC KUND           
029800*                                 WORK ORDER NUMBER, LDC DEALER           
029900     03 TIREPDAT             PIC S9(7)           COMP-3.                  
030000*                                 REPAIR DATE                             
030100*                                 REPAIR DATE                             
030200     03 IDDC-CLEAR-C0        PIC X(2).                                    
030300*                                 LAGERPRIORITERING VID                   
030400*                                 ORDERCLEARING                           
030500*                                 WAREHOUSE PRIORITY FOR ORDER            
030600*                                 CLEARING                                
030700     03 FLLF-C0              PIC X.                                       
030800*                                 ARTIKEL LAGERFÖRES                      
030900*                                 PART IN STOCK                           
031000     03 FLCLEAR-C0           PIC X.                                       
031100*                                 ORDERRAD CLEAR FLAGGA                   
031200*                                 CLEARING FLAG FOR ORDER LINE            
031300     03 IDDC-CLEAR-C1        PIC X(2).                                    
031400*                                 LAGERPRIORITERING VID                   
031500*                                 ORDERCLEARING                           
031600*                                 WAREHOUSE PRIORITY FOR ORDER            
031700*                                 CLEARING                                
031800     03 FLLF-C1              PIC X.                                       
031900*                                 ARTIKEL LAGERFÖRES                      
032000*                                 PART IN STOCK                           
032100     03 FLCLEAR-C1           PIC X.                                       
032200*                                 ORDERRAD CLEAR FLAGGA                   
032300*                                 CLEARING FLAG FOR ORDER LINE            
032400     03 IDDC-CLEAR-C2        PIC X(2).                                    
032500*                                 LAGERPRIORITERING VID                   
032600*                                 ORDERCLEARING                           
032700*                                 WAREHOUSE PRIORITY FOR ORDER            
032800*                                 CLEARING                                
032900     03 FLLF-C2              PIC X.                                       
033000*                                 ARTIKEL LAGERFÖRES                      
033100*                                 PART IN STOCK                           
033200     03 FLCLEAR-C2           PIC X.                                       
033300*                                 ORDERRAD CLEAR FLAGGA                   
033400*                                 CLEARING FLAG FOR ORDER LINE            
033500     03 IDDC-CLEAR-C3        PIC X(2).                                    
033600*                                 LAGERPRIORITERING VID                   
033700*                                 ORDERCLEARING                           
033800*                                 WAREHOUSE PRIORITY FOR ORDER            
033900*                                 CLEARING                                
034000     03 FLLF-C3              PIC X.                                       
034100*                                 ARTIKEL LAGERFÖRES                      
034200*                                 PART IN STOCK                           
034300     03 FLCLEAR-C3           PIC X.                                       
034400*                                 ORDERRAD CLEAR FLAGGA                   
034500*                                 CLEARING FLAG FOR ORDER LINE            
034600     03 IDDC-CLEAR-C4        PIC X(2).                                    
034700*                                 LAGERPRIORITERING VID                   
034800*                                 ORDERCLEARING                           
034900*                                 WAREHOUSE PRIORITY FOR ORDER            
035000*                                 CLEARING                                
035100     03 FLLF-C4              PIC X.                                       
035200*                                 ARTIKEL LAGERFÖRES                      
035300*                                 PART IN STOCK                           
035400     03 FLCLEAR-C4           PIC X.                                       
035500*                                 ORDERRAD CLEAR FLAGGA                   
035600*                                 CLEARING FLAG FOR ORDER LINE            
035700     03 IDDC-CLEAR-C5        PIC X(2).                                    
035800*                                 LAGERPRIORITERING VID                   
035900*                                 ORDERCLEARING                           
036000*                                 WAREHOUSE PRIORITY FOR ORDER            
036100*                                 CLEARING                                
036200     03 FLLF-C5              PIC X.                                       
036300*                                 ARTIKEL LAGERFÖRES                      
036400*                                 PART IN STOCK                           
036500     03 FLCLEAR-C5           PIC X.                                       
036600*                                 ORDERRAD CLEAR FLAGGA                   
036700*                                 CLEARING FLAG FOR ORDER LINE            
036800     03 IDDC-CLEAR-C6        PIC X(2).                                    
036900*                                 LAGERPRIORITERING VID                   
037000*                                 ORDERCLEARING                           
037100*                                 WAREHOUSE PRIORITY FOR ORDER            
037200*                                 CLEARING                                
037300     03 FLLF-C6              PIC X.                                       
037400*                                 ARTIKEL LAGERFÖRES                      
037500*                                 PART IN STOCK                           
037600     03 FLCLEAR-C6           PIC X.                                       
037700*                                 ORDERRAD CLEAR FLAGGA                   
037800*                                 CLEARING FLAG FOR ORDER LINE            
037900     03 TIDLEVDAT            PIC S9(7)           COMP-3.                  
038000*                                 OMRÄKNAT DATUM FÖR DDGS-ORDER           
038100*                                 CHANGED DATE FOR DDGS-ORDER             
038200     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
038300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
038400*                                 AVERAGE COST FOREIGN CURRENCY           
038500*** END OF VILMAII-COPY LENGTH= 419 BYTES                                 
