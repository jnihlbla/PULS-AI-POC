000100 01  GMT-WDB201.                                                          
000200*                                 KUNDREGISTER                            
000300*                                 GODSMOTTAGARE                           
000400*                                 FYSISK NYCKEL: IDGMT                    
000500*                                 (IDDISTR + IDKUNDNR)                    
000600     03 GMT-IDGMT.                                                        
000700*                                 GODSMOTTAGARE                           
000800*                                 GOODS RECEIVER                          
000900        05 GMT-IDDISTR       PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200        05 GMT-IDKUNDNR      PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500     03 GMT-ADGMT.                                                        
001600*                                 GODSMOTTAGARADRESS                      
001700*                                 GOODS RECEIVER ADDRESS                  
001800        05 GMT-ADGMT-GATA    PIC X(35).                                   
001900*                                 GODSMOTTAGARADRESS GATA                 
002000*                                 GOODS RECEIVER ADDRESS STREET           
002100        05 GMT-ADGMT-PADR    PIC X(35).                                   
002200*                                 GODSMOTTAGARADRESS POSTADRESS           
002300*                                 GOODS RECEIVER ADDRESS TOWN             
002400        05 GMT-ADPOST-PNRORT REDEFINES GMT-ADGMT-PADR.                    
002500*                                 POSTNUMMER + ORT                        
002600*                                 POSTAL CODE + CITY                      
002700           07 GMT-ADPOSTNR   PIC X(10).                                   
002800*                                 POSTNUMMER I ADRESS                     
002900*                                 POSTAL CODE IN ADDRESS                  
003000           07 GMT-ADCITY     PIC X(25).                                   
003100*                                 BENÄMNING PÅ STAD                       
003200*                                 CITY                                    
003300        05 GMT-ADPOST-ORTPNR REDEFINES GMT-ADGMT-PADR.                    
003400*                                 ORT + POSTNUMMER                        
003500*                                 CITY + POSTAL CODE                      
003600           07 GMT-ADCITY     PIC X(25).                                   
003700*                                 BENÄMNING PÅ STAD                       
003800*                                 CITY                                    
003900           07 GMT-ADPOSTNR   PIC X(10).                                   
004000*                                 POSTNUMMER I ADRESS                     
004100*                                 POSTAL CODE IN ADDRESS                  
004200        05 GMT-ADGMT-LAND    PIC X(35).                                   
004300*                                 GODSMOTTAGARADRESS LAND                 
004400*                                 GOODS RECEIVER ADDRESS COUNTRY          
004500     03 GMT-ADGMT-OVR.                                                    
004600*                                 GODSMOTTAGARADRESS (EJ LATIN)           
004700*                                 GOODS RECEIVER ADDRESS                  
004800        05 GMT-ADGMT-OVR-GATA                                             
004900                             PIC X(35).                                   
005000*                                 GODSMOTTAGARADRESS GATA                 
005100*                                 GOODS RECEIVER ADDRESS STREET           
005200        05 GMT-ADGMT-OVR-PADR                                             
005300                             PIC X(35).                                   
005400*                                 GODSMOTTAGARADRESS POSTADRESS           
005500*                                 GOODS RECEIVER ADDRESS TOWN             
005600        05 GMT-ADGMT-OVR-LAND                                             
005700                             PIC X(35).                                   
005800*                                 GODSMOTTAGARADRESS LAND                 
005900*                                 GOODS RECEIVER ADDRESS COUNTRY          
006000     03 GMT-BEGMT.                                                        
006100*                                 GODSMOTTAGARNAMN                        
006200*                                 GOODS RECEIVER NAME                     
006300        05 GMT-BEGMT-RAD1    PIC X(35).                                   
006400*                                 GODSMOTTAGARNAMN RAD 1                  
006500*                                 GOODS RECEIVER NAME LINE 1              
006600        05 GMT-BEGMT-RAD2    PIC X(35).                                   
006700*                                 GODSMOTTAGARNAMN RAD 2                  
006800*                                 GOODS RECEIVER NAME LINE 2              
006900     03 GMT-BEGMT-OVR.                                                    
007000*                                 GODSMOTTAGARNAMN (EJ LATIN)             
007100*                                 GOODS RECEIVER NAME (NOT LATIN)         
007200        05 GMT-BEGMT-OVR-RAD1                                             
007300                             PIC X(35).                                   
007400*                                 GODSMOTTAGARNAMN RAD 1                  
007500*                                 GOODS RECEIVER NAME LINE 1              
007600        05 GMT-BEGMT-OVR-RAD2                                             
007700                             PIC X(35).                                   
007800*                                 GODSMOTTAGARNAMN RAD 2                  
007900*                                 GOODS RECEIVER NAME LINE 2              
008000     03 GMT-BETEXT           PIC X(35).                                   
008100     03 GMT-FLCOD            PIC X.                                       
008200*                                 KONTANTBETALANDE KUND                   
008300*                                 CASH ON DELIVERY CUSTOMER               
008400     03 GMT-FLDNDAP          PIC X.                                       
008500*                                 FLAGGA DNOT VIA D&P-KUND                
008600*                                 DNOT VIA D&P CUSTOMER FLAG              
008700     03 GMT-FLFAKURS         PIC X.                                       
008800*                                 URSPRUNGLAND I FAKTURA                  
008900*                                 ORIGIN COUNTRY IN INVOICE               
009000     03 GMT-FLFAKVKT         PIC X.                                       
009100*                                 VIKT ANGES I FAKTURAN                   
009200*                                 WEIGHT IN INVOICE                       
009300     03 GMT-FLOBKR-TACD      PIC X.                                       
009400*                                 ORDERBEKRÄFTELSE TILL TACDIS            
009500*                                 ORDER CONFIRMATION TACDIS               
009600     03 GMT-FLOKFAK-G        PIC X.                                       
009700*                                 FLAGGA FAKTURATYP G GODKÄND             
009800*                                 INVOICETYPE G ALLOWED                   
009900     03 GMT-FLOKFAK-K        PIC X.                                       
010000*                                 FLAGGA FAKTURATYP K GODKÄND             
010100*                                 INVOICETYPE K ALLOWED                   
010200     03 GMT-FLOKFAK-N        PIC X.                                       
010300*                                 FLAGGA FAKTURATYP N GODKÄND             
010400*                                 INVOICETYPE N ALLOWED                   
010500     03 GMT-FLOKFAK-R        PIC X.                                       
010600*                                 FLAGGA FAKTURATYP R GODKÄND             
010700*                                 INVOICETYPE R ALLOWED                   
010800     03 GMT-FLORDTIL-KL1     PIC X.                                       
010900*                                 TVINGANDE TILÄGG ORDER KLASS 1          
011000*                                 MANDATORY CL-1 ORDER ADDITION           
011100     03 GMT-FLORDTIL-KL2     PIC X.                                       
011200*                                 TVINGANDE TILÄGG ORDER KLASS 2          
011300*                                 MANDATORY CL-2 ORDER ADDITION           
011400     03 GMT-FLORDTIL-KL3     PIC X.                                       
011500*                                 TVINGANDE TILÄGG ORDER KLASS 3          
011600*                                 MANDATORY CL-3 ORDER ADDITION           
011700     03 GMT-FLORDTIL-KL4     PIC X.                                       
011800*                                 TVINGANDE TILÄGG ORDER KLASS 4          
011900*                                 MANDATORY CL-4 ORDER ADDITION           
012000     03 GMT-FLPRELRO         PIC X.                                       
012100*                                 PRELIMINÄR RESTORDERFLAGGA              
012200*                                 PRELIMINAR BACK ORDER FLAG              
012300     03 GMT-FLRESTN          PIC X.                                       
012400*                                 RESTNOTERING ?                          
012500*                                 BACKORDERED ?                           
012600     03 GMT-FLRETFG          PIC X.                                       
012700*                                 FARLIGT GODS RETUR FLAGGA               
012800*                                 HAZ MAT RETURN FLAG                     
012900     03 GMT-FLRETUR          PIC X.                                       
013000*                                 FLAGGA RETUR OK.                        
013100*                                 RETURN PART FLAG                        
013200     03 GMT-FLSWCONS         PIC X.                                       
013300*                                 FLAGGA KONSOLIDERING SW-ORDER           
013400*                                 CONSOLIDATION OF SW-ORDERS              
013500     03 GMT-IDDC-BULK        OCCURS 99 TIMES                              
013600                             PIC X(2).                                    
013700*                                 IDENTIFIERARE BULKORDERLAGER            
013800*                                 WAREHOUSE IDENTIFIER BULK               
013900*                                 ORDERS                                  
014000     03 GMT-IDDC-DAY         OCCURS 99 TIMES                              
014100                             PIC X(2).                                    
014200*                                 IDENTIFIERARE DAGORDERLAGER             
014300*                                 WAREHOUSE IDENTIFIER DAILY              
014400*                                 ORDERS                                  
014500     03 GMT-IDDC-VOR         OCCURS 99 TIMES                              
014600                             PIC X(2).                                    
014700*                                 IDENTIFIERARE VORORDERLAGER             
014800*                                 WAREHOUSE IDENTIFIER VOR ORDERS         
014900     03 GMT-IDDC-RET         PIC X(2).                                    
015000*                                 MOTTAGANDE LAGER FÖR RETURER            
015100*                                 RECEIVING WAREHOUSE FOR RETURNS         
015200     03 GMT-IDDEALER-VIPS    PIC X(6).                                    
015300*                                 VIPS ÅTERFÖRSÄLJARE                     
015400*                                 VIPS DEALER                             
015500     03 GMT-IDDEPOT          PIC X(2).                                    
015600*                                 TRANSPORT DEPOT                         
015700*                                 TRANSPORT DEPOT                         
015800     03 GMT-IDFTG            PIC 9(2).                                    
015900*                                 FÖRETAGSID EKONOM REDOVISNING           
016000*                                 COMPANY IDENTITY ACCOUNTING             
016100     03 GMT-IDKUNDNR-HEAD    PIC S9(7)           COMP-3.                  
016200*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
016300*                                 CUSTOMER NO TO HEAD OFFICE              
016400     03 GMT-IDLANDX2         PIC X(2).                                    
016500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
016600*                                 2-LETTER CODE FOR COUNTRY               
016700     03 GMT-IDLATITUDE       PIC X(10).                                   
016800*                                 LATITUDE                                
016900*                                 GPS COORDINATES                         
017000     03 GMT-IDLONGITUDE      PIC X(10).                                   
017100*                                 LONGITUDE                               
017200*                                 GPS COORDINATES                         
017300     03 GMT-IDLEVNR          PIC X(5).                                    
017400*                                 LEVERANTÖRNUMMER                        
017500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
017600     03 GMT-IDPARTNR         PIC X(9).                                    
017700*                                 FINANCIELL KUND                         
017800*                                 FINANCIAL CUST                          
017900     03 GMT-IDPARTNER        PIC X(9).                                    
018000*                                 PARTNER ID                              
018100*                                 PARTNER ID                              
018200     03 GMT-IDROUTE          PIC X.                                       
018300*                                 TRANSPORT ROUTE                         
018400*                                 TRANSPORT ROUTE                         
018500     03 GMT-IDSKYLT          PIC X(3).                                    
018600*                                 NATIONALITETSTECKEN                     
018700*                                 SPRÅKIDENTIFIKATION                     
018800*                                 NATIONALITY SIGN                        
018900*                                 LANGUAGE IDENTIFIER                     
019000     03 GMT-IDTFN            PIC X(20).                                   
019100*                                 TELEFONNUMMER EXTERNT                   
019200*                                 TELEPHONE NUMBER  EXTERNAL              
019300     03 GMT-IDUSER-DCUPD     PIC X(8).                                    
019400*                                 USER SOM UPPDATERAT DC-STYRNING         
019500*                                 USER WHO UPDATED THE DC-CLEAR           
019600     03 GMT-IDZON            PIC X(2).                                    
019700*                                 TRANSPORTVÄG (RUTT,ZON)                 
019800*                                 TRANSPORT ROUTE (ZONE)                  
019900     03 GMT-KDBEKALT         PIC S9              COMP-3.                  
020000*                                 ORDERBEKRÄFTELSEALTERNATIV              
020100*                                 ORDER CONFIRMATION ALTERNATIVE          
020200     03 GMT-KDGENFAK         PIC X.                                       
020300*                                 NORMAL FAKTURATYP                       
020400*                                 DEFAULT INVOICE TYPE                    
020500     03 GMT-KDHBLKRV         PIC S9              COMP-3.                  
020600*                                 HANDELSBLOCK                            
020700*                                 TRADE BLOCK                             
020800     03 GMT-KDKUNDKAT        PIC 9(2).                                    
020900*                                 TYP OF KUND I KUND TABELL - WDB         
021000*                                 201                                     
021100*                                    01 = DEALER                          
021200*                                                                         
021300*                                    02 = POLESTAR                        
021400*                                                                         
021500*                                    03 = LYNK                            
021600*                                                                         
021700*                                    04 = IMPORTER                        
021800*                                                                         
021900*                                    05 = INTERNAL CUSTOMER               
022000*                                                                         
022100*                                    06 = SALES COMPANY                   
022200*                                                                         
022300*                                    07 = SUPPLIER                        
022400*                                                                         
022500*                                    08 = REFILL                          
022600*                                                                         
022700*                                    09 = TRANSFER                        
022800*                                                                         
022900*                                    10 = EXTENDED REFILL                 
023000*                                                                         
023100*                                    11 = RETURNS                         
023200*                                                                         
023300*                                    12 = QUALITY RETURNS                 
023400*                                                                         
023500*                                    13 = SCRAPL                          
023600*                                                                         
023700*                                    14 = QUALITY SCRAP                   
023800*                                                                         
023900*                                    15 = MIXED STOCK                     
024000*                                                                         
024100*                                    16 = INTERNAL EXCHANGE ORDER         
024200*                                                                         
024300*                                    17 = EMBALLAGE                       
024400*                                                                         
024500     03 GMT-KDPOSTNR         PIC X.                                       
024600*                                 OM/HUR POSTNUMMER JUSTERATS             
024700*                                 IF/HOW POSTAL CODE IS JUSTIFIED         
024800     03 GMT-KDSPRAK          PIC S9              COMP-3.                  
024900*                                 SPRÅKKOD                                
025000*                                 LANGUAGE CODE                           
025100     03 GMT-KDSTATNR         PIC S9              COMP-3.                  
025200*                                 STATNUMMER TYP                          
025300*                                 1 = NORSKT                              
025400*                                 2 = ENGELSKT                            
025500*                                 3 = BELGISKT                            
025600*                                 4 = PERUANSKT                           
025700*                                 5 = SVENSKT                             
025800*                                 6 =                                     
025900*                                 STATISTICS TYPE                         
026000     03 GMT-KVDAGAR-DOW      PIC S9(3)           COMP-3.                  
026100*                                 ANTAL DAGAR FÖRE DC CLEARING            
026200*                                 NUMBER OF DAYS REFERRAL                 
026300     03 GMT-KVDAGAR-RTATG    PIC S9(3)           COMP-3.                  
026400*                                 ANTAL DAGAR ETT RETURTILLSTÅND          
026500*                                 MÅSTE ÅTGÄRDAS                          
026600*                                 EFTER PÅMINNELSE                        
026700     03 GMT-KVDAGAR-RTPMN    PIC S9(3)           COMP-3.                  
026800*                                 ANTAL DAGAR EFTER PÅMINNELSE FÖ         
026900*                                 R RETURTILLSTÅND SKRIVS                 
027000     03 GMT-REAVDRAG         PIC S9(2)V9(1)      COMP-3.                  
027100*                                 AVDRAGSPROCENT                          
027200*                                 DEDUCTION PERCENT                       
027300     03 GMT-REEMBHNT         PIC S9(2)V9(1)      COMP-3.                  
027400*                                 EMB OCH HANTERINGSKOST (%)              
027500*                                 PACKING AND HANDLING (%)                
027600     03 GMT-RESLATT          PIC S9(3)           COMP-3.                  
027700*                                 SLATTGRÄNS                              
027800*                                 DROP CODE                               
027900     03 GMT-TIAAMMDD-DCUPD   PIC S9(7)           COMP-3.                  
028000*                                 DATUM FÖR SISTA DC-STYR UPPDAT          
028100*                                 DATE WHEN DC-CLEAR WAS UPDATED          
028200     03 GMT-TIFAKT           PIC S9(7)           COMP-3.                  
028300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
028400*                                 INVOICING DATE   (YYMMDD)               
028500     03 GMT-TISTADAT         PIC S9(7)           COMP-3.                  
028600*                                 GENERELLT STARTDATUM                    
028700*                                 GENERAL START DATE                      
028800     03 GMT-TISTADAT-COD     PIC S9(7)           COMP-3.                  
028900*                                 STARTDATUM FÖR KONTANTBETALANDE         
029000*                                 KUND                                    
029100*                                 START DATE FOR CASH ON DELIVERY         
029200*                                 CUSTOMER                                
029300     03 GMT-TISTATID-COD     PIC S9(7)           COMP-3.                  
029400*                                 STARTTIDPUNKT FÖR                       
029500*                                 KONTANTBETALANDE KUND                   
029600*                                 START TIME FOR CASH ON DELIVERY         
029700*                                 CUSTOMER                                
029800     03 GMT-TISTODAT         PIC S9(7)           COMP-3.                  
029900*                                 GENERELLT STOPPDATUM                    
030000*                                 GENERAL STOP DATE YYMMDD                
030100     03 GMT-TISTODAT-COD     PIC S9(7)           COMP-3.                  
030200*                                 STOPPDATUM FÖR KONTANTBETALANDE         
030300*                                 KUND                                    
030400*                                 STOP DATE FOR CASH ON DELIVERY          
030500*                                 CUSTOMER                                
030600     03 GMT-TISTOTID-COD     PIC S9(7)           COMP-3.                  
030700*                                 STOPPTIDPUNKT FÖR                       
030800*                                 KONTANTBETALANDE KUND                   
030900*                                 STOP TIME FOR CASH ON DELIVERY          
031000*                                 CUSTOMER                                
031100     03 GMT-DIST.                                                         
031200*                                 DISTRIKTS INFORMATION                   
031300        05 GMT-FLAUTORD      PIC X.                                       
031400*                                 FLAGGAN STYR OM AUTOMATORDER-           
031500*                                 NUMMER SKALL SKAPAS                     
031600*                                 FLAG THAT STATES IF AUTOMATIC           
031700*                                 ORDER NUMBER SHALL BE GENERATED         
031800        05 GMT-FLNC          PIC X.                                       
031900*                                 NEW CONCEPT FLAGGA                      
032000*                                 NEW CONCEPT FLAG                        
032100        05 GMT-FLPRERS       PIC X.                                       
032200*                                 PRISERSÄTTNINGSFLAGGA                   
032300*                                 PRICE REPLACEMENT FLAG                  
032400        05 GMT-FLSAMFAK      PIC X.                                       
032500*                                 SAMFAKTURERING NEW CONCEPT              
032600*                                 CO-INVOICING NEW CONCEPT                
032700        05 GMT-FLVR          PIC X.                                       
032800*                                 ANSLUTEN TILL VR-SYST                   
032900*                                 ASSOCIATED TO VR SYST                   
033000        05 GMT-FLURSRAP      PIC X.                                       
033100*                                 URSPRUNGSRAPPORTERING VID               
033200*                                 PACKNING                                
033300*                                 FLAG FOR REPORTING OF ORIGIN            
033400        05 GMT-IDRFTAB       PIC X(3).                                    
033500*                                 RANSONERINGSFAKTORTABELL                
033600*                                 TABLE WITH RATIONING FACTORS            
033700        05 GMT-KDORDING      PIC S9              COMP-3.                  
033800*                                 UPPDATERING ORDERINGÅNG                 
033900*                                 ORDER STATISTICS                        
034000        05 GMT-KVVECKOR-OB   PIC S9(3)           COMP-3.                  
034100*                                 LAGRINGSTID I VECKOR, ORDBEKR.          
034200*                                 STORAGE TIME ORDER CONFIRMATION         
034300        05 GMT-IDDC-TVSVOR   OCCURS 16 TIMES                              
034400                             PIC X(2).                                    
034500*                                 TVÅNGSSTYRNING AV VOR-SLÄPP             
034600*                                 FRÅN DC                                 
034700*                                 VOR-RELEASE FORCED TO DC                
034800*                                                                         
034900     03 GMT-LDC-KUND.                                                     
035000*                                 LDC-KUND INFORMATION                    
035100        05 GMT-IDRFSDGRDC    OCCURS 4 TIMES.                              
035200*                                 GRUPP FÖR RFS DAGAR PER DC              
035300*                                 GROUP OF RFS DAYS PER DC                
035400           07 GMT-IDDC-RFS   PIC X(2).                                    
035500*                                 DC FÖR RFS DAGAR FÖRE REPDAG            
035600*                                 DC OF RFS DAYS BEFORE REPAIR            
035700           07 GMT-KVDAGAR-RFS                                             
035800                             PIC S9              COMP-3.                  
035900*                                 ANT DGR FÖR RFS/DC FÖRE REPDAT          
036000*                                 NO DAYS OF RFS/DC BEFORE REPAIR         
036100        05 GMT-KVDAGAR-RFS-DEF                                            
036200                             PIC S9              COMP-3.                  
036300*                                 DEFAULT DAGAR RFS FÖRE REPDAT           
036400*                                 DEFAULT DAYS RFS BEFORE REPAIR          
036500        05 GMT-FLLDCKND      PIC X.                                       
036600*                                 FL LDC-KUND                             
036700*                                 FL LDC CUSTOMER                         
036800        05 GMT-FLKVBRYT-ORDKL1                                            
036900                             PIC X.                                       
037000*                                 KVANTITET BRYTES                        
037100*                                 QUANTITY BREAK                          
037200        05 GMT-FLKVBRYT-ORDKL2                                            
037300                             PIC X.                                       
037400*                                 KVANTITET BRYTES                        
037500*                                 QUANTITY BREAK                          
037600        05 GMT-FLKVBRYT-ORDKL3                                            
037700                             PIC X.                                       
037800*                                 KVANTITET BRYTES                        
037900*                                 QUANTITY BREAK                          
038000        05 GMT-FLKVBRYT-ORDKL4                                            
038100                             PIC X.                                       
038200*                                 KVANTITET BRYTES                        
038300*                                 QUANTITY BREAK                          
038400        05 GMT-IDDC-DAY-ALT  PIC X(2).                                    
038500*                                 DAG DC BARA UNDER GIVNA TIDER           
038600*                                 DAY DC UNDER SPECIFIED HOURS            
038700        05 GMT-IDDC-RET72    OCCURS 3 TIMES                               
038800                             PIC X(2).                                    
038900*                                 MOTTAGANDE LAGER FÖR 72-RETURER         
039000*                                 RECEIVING DC FOR 72-RETURNS             
039100        05 GMT-TIHHMM-START  PIC S9(5)           COMP-3.                  
039200*                                 KLOCKSLAG (TIMMAR/MIN.) START           
039300*                                 TIME IN HOUR AND MINUTE START           
039400        05 GMT-TIHHMM-STOP   PIC S9(5)           COMP-3.                  
039500*                                 KLOCKSLAG (TIMMAR/MIN.) STOP            
039600*                                 TIME IN HOUR AND MINUTE STOP            
039700        05 GMT-KVDAGAR-CDC   PIC S9(3)           COMP-3.                  
039800*                                 CDC-DAGAR                               
039900        05 GMT-KVDAGAR-SDC   PIC S9(3)           COMP-3.                  
040000*                                 SDC-DAGAR                               
040100        05 GMT-IDDC-PREPLAN  OCCURS 8 TIMES                               
040200                             PIC X(2).                                    
040300*                                 PRE-PLANNED RELEASE TO DC               
040400*                                 PRE-PLANNED RELEASE TO DC               
040500*                                                                         
040600     03 GMT-FLAUTREM         PIC X.                                       
040700*                                 AUTOMATISK REMISS (Y/N)                 
040800*                                 REMISS AUTO APPROVE (Y/N)               
040900     03 GMT-FILLER24         PIC X(24).                                   
041000*** END OF VILMAII-COPY LENGTH= 1272 BYTES                                
