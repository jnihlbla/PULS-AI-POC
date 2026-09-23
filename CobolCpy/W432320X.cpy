000100 01  GMT-WDB201X.                                                         
000200*                                 THIS IS COPY OF WDB201 FOR WXTR         
000300*                                  FILE                                   
000400*                                 KUNDREGISTER                            
000500*                                 GODSMOTTAGARE                           
000600*                                 FYSISK NYCKEL: IDGMT                    
000700*                                 (IDDISTR + IDKUNDNR)                    
000800     03 GMT-IDGMT.                                                        
000900*                                 GODSMOTTAGARE                           
001000*                                 GOODS RECEIVER                          
001100        05 GMT-IDDISTR       PIC Z(3)9.                                   
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400        05 GMT-IDKUNDNR      PIC Z(5)9.                                   
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 GMT-ADGMT-GATA       PIC X(35).                                   
001800*                                 GODSMOTTAGARADRESS GATA                 
001900*                                 GOODS RECEIVER ADDRESS STREET           
002000     03 GMT-ADPOSTNR         PIC X(10).                                   
002100*                                 POSTNUMMER I ADRESS                     
002200*                                 POSTAL CODE IN ADDRESS                  
002300     03 GMT-ADCITY           PIC X(20).                                   
002400     03 GMT-ADGMT-LAND       PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS LAND                 
002600*                                 GOODS RECEIVER ADDRESS COUNTRY          
002700     03 GMT-BEGMT.                                                        
002800*                                 GODSMOTTAGARNAMN                        
002900*                                 GOODS RECEIVER NAME                     
003000        05 GMT-BEGMT-RAD1    PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 1                  
003200*                                 GOODS RECEIVER NAME LINE 1              
003300        05 GMT-BEGMT-RAD2    PIC X(35).                                   
003400*                                 GODSMOTTAGARNAMN RAD 2                  
003500*                                 GOODS RECEIVER NAME LINE 2              
003600     03 GMT-BETEXT           PIC X(35).                                   
003700     03 GMT-FLCOD            PIC X.                                       
003800*                                 KONTANTBETALANDE KUND                   
003900*                                 CASH ON DELIVERY CUSTOMER               
004000     03 GMT-FLDNDAP          PIC X.                                       
004100*                                 FLAGGA DNOT VIA D&P-KUND                
004200*                                 DNOT VIA D&P CUSTOMER FLAG              
004300     03 GMT-FLFAKURS         PIC X.                                       
004400*                                 URSPRUNGLAND I FAKTURA                  
004500*                                 ORIGIN COUNTRY IN INVOICE               
004600     03 GMT-FLFAKVKT         PIC X.                                       
004700*                                 VIKT ANGES I FAKTURAN                   
004800*                                 WEIGHT IN INVOICE                       
004900     03 GMT-FLOBKR-TACD      PIC X.                                       
005000*                                 ORDERBEKRÄFTELSE TILL TACDIS            
005100*                                 ORDER CONFIRMATION TACDIS               
005200     03 GMT-FLOKFAK-G        PIC X.                                       
005300*                                 FLAGGA FAKTURATYP G GODKÄND             
005400*                                 INVOICETYPE G ALLOWED                   
005500     03 GMT-FLOKFAK-K        PIC X.                                       
005600*                                 FLAGGA FAKTURATYP K GODKÄND             
005700*                                 INVOICETYPE K ALLOWED                   
005800     03 GMT-FLOKFAK-N        PIC X.                                       
005900*                                 FLAGGA FAKTURATYP N GODKÄND             
006000*                                 INVOICETYPE N ALLOWED                   
006100     03 GMT-FLOKFAK-R        PIC X.                                       
006200*                                 FLAGGA FAKTURATYP R GODKÄND             
006300*                                 INVOICETYPE R ALLOWED                   
006400     03 GMT-FLORDTIL-KL1     PIC X.                                       
006500*                                 TVINGANDE TILÄGG ORDER KLASS 1          
006600*                                 MANDATORY CL-1 ORDER ADDITION           
006700     03 GMT-FLORDTIL-KL2     PIC X.                                       
006800*                                 TVINGANDE TILÄGG ORDER KLASS 2          
006900*                                 MANDATORY CL-2 ORDER ADDITION           
007000     03 GMT-FLORDTIL-KL3     PIC X.                                       
007100*                                 TVINGANDE TILÄGG ORDER KLASS 3          
007200*                                 MANDATORY CL-3 ORDER ADDITION           
007300     03 GMT-FLORDTIL-KL4     PIC X.                                       
007400*                                 TVINGANDE TILÄGG ORDER KLASS 4          
007500*                                 MANDATORY CL-4 ORDER ADDITION           
007600     03 GMT-FLPRELRO         PIC X.                                       
007700*                                 PRELIMINÄR RESTORDERFLAGGA              
007800*                                 PRELIMINAR BACK ORDER FLAG              
007900     03 GMT-FLRESTN          PIC X.                                       
008000*                                 RESTNOTERING ?                          
008100*                                 BACKORDERED ?                           
008200     03 GMT-FLRETFG          PIC X.                                       
008300*                                 FARLIGT GODS RETUR FLAGGA               
008400*                                 HAZ MAT RETURN FLAG                     
008500     03 GMT-FLRETUR          PIC X.                                       
008600*                                 FLAGGA RETUR OK.                        
008700*                                 RETURN PART FLAG                        
008800     03 GMT-FLSWCONS         PIC X.                                       
008900*                                 FLAGGA KONSOLIDERING SW-ORDER           
009000*                                 CONSOLIDATION OF SW-ORDERS              
009100     03 GMT-IDDC-BULK        OCCURS 99 TIMES                              
009200                             PIC X(2).                                    
009300*                                 IDENTIFIERARE BULKORDERLAGER            
009400*                                 WAREHOUSE IDENTIFIER BULK               
009500*                                 ORDERS                                  
009600     03 GMT-IDDC-DAY         OCCURS 99 TIMES                              
009700                             PIC X(2).                                    
009800*                                 IDENTIFIERARE DAGORDERLAGER             
009900*                                 WAREHOUSE IDENTIFIER DAILY              
010000*                                 ORDERS                                  
010100     03 GMT-IDDC-VOR         OCCURS 99 TIMES                              
010200                             PIC X(2).                                    
010300*                                 IDENTIFIERARE VORORDERLAGER             
010400*                                 WAREHOUSE IDENTIFIER VOR ORDERS         
010500     03 GMT-IDDC-RET         PIC X(2).                                    
010600*                                 MOTTAGANDE LAGER FÖR RETURER            
010700*                                 RECEIVING WAREHOUSE FOR RETURNS         
010800     03 GMT-IDDEALER-VIPS    PIC X(6).                                    
010900*                                 VIPS ÅTERFÖRSÄLJARE                     
011000*                                 VIPS DEALER                             
011100     03 GMT-IDDEPOT          PIC X(2).                                    
011200*                                 TRANSPORT DEPOT                         
011300*                                 TRANSPORT DEPOT                         
011400     03 GMT-IDFTG            PIC 9(2).                                    
011500*                                 FÖRETAGSID EKONOM REDOVISNING           
011600*                                 COMPANY IDENTITY ACCOUNTING             
011700     03 GMT-IDKUNDNR-HEAD    PIC Z(5)9.                                   
011800*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
011900*                                 CUSTOMER NO TO HEAD OFFICE              
012000     03 GMT-IDLANDX2         PIC X(2).                                    
012100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
012200*                                 2-LETTER CODE FOR COUNTRY               
012300     03 GMT-IDLATITUDE       PIC X(10).                                   
012400*                                 LATITUDE                                
012500*                                 GPS COORDINATES                         
012600     03 GMT-IDLONGITUDE      PIC X(10).                                   
012700*                                 LONGITUDE                               
012800*                                 GPS COORDINATES                         
012900     03 GMT-IDLEVNR          PIC X(5).                                    
013000*                                 LEVERANTÖRNUMMER                        
013100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
013200     03 GMT-IDPARTNR         PIC X(9).                                    
013300*                                 FINANCIELL KUND                         
013400*                                 FINANCIAL CUST                          
013500     03 GMT-IDPARTNER        PIC X(9).                                    
013600*                                 PARTNER ID                              
013700*                                 PARTNER ID                              
013800     03 GMT-IDROUTE          PIC X.                                       
013900*                                 TRANSPORT ROUTE                         
014000*                                 TRANSPORT ROUTE                         
014100     03 GMT-IDSKYLT          PIC X(3).                                    
014200*                                 NATIONALITETSTECKEN                     
014300*                                 SPRÅKIDENTIFIKATION                     
014400*                                 NATIONALITY SIGN                        
014500*                                 LANGUAGE IDENTIFIER                     
014600     03 GMT-IDTFN            PIC X(20).                                   
014700*                                 TELEFONNUMMER EXTERNT                   
014800*                                 TELEPHONE NUMBER  EXTERNAL              
014900     03 GMT-IDUSER-DCUPD     PIC X(8).                                    
015000*                                 USER SOM UPPDATERAT DC-STYRNING         
015100*                                 USER WHO UPDATED THE DC-CLEAR           
015200     03 GMT-IDZON            PIC X(2).                                    
015300*                                 TRANSPORTVÄG (RUTT,ZON)                 
015400*                                 TRANSPORT ROUTE (ZONE)                  
015500     03 GMT-KDBEKALT         PIC 9.                                       
015600*                                 ORDERBEKRÄFTELSEALTERNATIV              
015700*                                 ORDER CONFIRMATION ALTERNATIVE          
015800     03 GMT-KDGENFAK         PIC X.                                       
015900*                                 NORMAL FAKTURATYP                       
016000*                                 DEFAULT INVOICE TYPE                    
016100     03 GMT-KDHBLKRV         PIC 9.                                       
016200*                                 HANDELSBLOCK                            
016300*                                 TRADE BLOCK                             
016400     03 GMT-KDKUNDKAT        PIC 9(2).                                    
016500*                                 TYP OF KUND I KUND TABELL - WDB         
016600*                                 201                                     
016700*                                    01 = DEALER                          
016800*                                                                         
016900*                                    02 = POLESTAR                        
017000*                                                                         
017100*                                    03 = LYNK                            
017200*                                                                         
017300*                                    04 = IMPORTER                        
017400*                                                                         
017500*                                    05 = INTERNAL CUSTOMER               
017600*                                                                         
017700*                                    06 = SALES COMPANY                   
017800*                                                                         
017900*                                    07 = SUPPLIER                        
018000*                                                                         
018100*                                    08 = REFILL                          
018200*                                                                         
018300*                                    09 = TRANSFER                        
018400*                                                                         
018500*                                    10 = EXTENDED REFILL                 
018600*                                                                         
018700*                                    11 = RETURNS                         
018800*                                                                         
018900*                                    12 = QUALITY RETURNS                 
019000*                                                                         
019100*                                    13 = SCRAPL                          
019200*                                                                         
019300*                                    14 = QUALITY SCRAP                   
019400*                                                                         
019500*                                    15 = MIXED STOCK                     
019600*                                                                         
019700*                                    16 = INTERNAL EXCHANGE ORDER         
019800*                                                                         
019900*                                    17 = EMBALLAGE                       
020000*                                                                         
020100     03 GMT-KDPOSTNR         PIC X.                                       
020200*                                 OM/HUR POSTNUMMER JUSTERATS             
020300*                                 IF/HOW POSTAL CODE IS JUSTIFIED         
020400     03 GMT-KDSPRAK          PIC 9.                                       
020500*                                 SPRÅKKOD                                
020600*                                 LANGUAGE CODE                           
020700     03 GMT-KDSTATNR         PIC 9.                                       
020800*                                 STATNUMMER TYP                          
020900*                                 1 = NORSKT                              
021000*                                 2 = ENGELSKT                            
021100*                                 3 = BELGISKT                            
021200*                                 4 = PERUANSKT                           
021300*                                 5 = SVENSKT                             
021400*                                 6 =                                     
021500*                                 STATISTICS TYPE                         
021600     03 GMT-KVDAGAR-DOW      PIC Z9.                                      
021700*                                 ANTAL DAGAR FÖRE DC CLEARING            
021800*                                 NUMBER OF DAYS REFERRAL                 
021900     03 GMT-KVDAGAR-RTATG    PIC Z(2)9.                                   
022000*                                 ANTAL DAGAR ETT RETURTILLSTÅND          
022100*                                 MÅSTE ÅTGÄRDAS                          
022200*                                 EFTER PÅMINNELSE                        
022300     03 GMT-KVDAGAR-RTPMN    PIC Z(2)9.                                   
022400*                                 ANTAL DAGAR EFTER PÅMINNELSE FÖ         
022500*                                 R RETURTILLSTÅND SKRIVS                 
022600     03 GMT-REAVDRAG         PIC Z9.9.                                    
022700*                                 AVDRAGSPROCENT                          
022800*                                 DEDUCTION PERCENT                       
022900     03 GMT-REEMBHNT         PIC Z9.9.                                    
023000*                                 EMB OCH HANTERINGSKOST (%)              
023100*                                 PACKING AND HANDLING (%)                
023200     03 GMT-RESLATT          PIC Z9.                                      
023300*                                 SLATTGRÄNS                              
023400*                                 DROP CODE                               
023500     03 GMT-TIAAMMDD-DCUPD   PIC 9(6).                                    
023600*                                 DATUM FÖR SISTA DC-STYR UPPDAT          
023700*                                 DATE WHEN DC-CLEAR WAS UPDATED          
023800     03 GMT-TIFAKT           PIC 9(6).                                    
023900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
024000*                                 INVOICING DATE   (YYMMDD)               
024100     03 GMT-TISTADAT         PIC 9(6).                                    
024200*                                 GENERELLT STARTDATUM                    
024300*                                 GENERAL START DATE                      
024400     03 GMT-TISTADAT-COD     PIC 9(6).                                    
024500*                                 STARTDATUM FÖR KONTANTBETALANDE         
024600*                                 KUND                                    
024700*                                 START DATE FOR CASH ON DELIVERY         
024800*                                 CUSTOMER                                
024900     03 GMT-TISTATID-COD     PIC 9(6).                                    
025000*                                 STARTTIDPUNKT FÖR                       
025100*                                 KONTANTBETALANDE KUND                   
025200*                                 START TIME FOR CASH ON DELIVERY         
025300*                                 CUSTOMER                                
025400     03 GMT-TISTODAT         PIC 9(6).                                    
025500*                                 GENERELLT STOPPDATUM                    
025600*                                 GENERAL STOP DATE YYMMDD                
025700     03 GMT-TISTODAT-COD     PIC 9(6).                                    
025800*                                 STOPPDATUM FÖR KONTANTBETALANDE         
025900*                                 KUND                                    
026000*                                 STOP DATE FOR CASH ON DELIVERY          
026100*                                 CUSTOMER                                
026200     03 GMT-TISTOTID-COD     PIC 9(6).                                    
026300*                                 STOPPTIDPUNKT FÖR                       
026400*                                 KONTANTBETALANDE KUND                   
026500*                                 STOP TIME FOR CASH ON DELIVERY          
026600*                                 CUSTOMER                                
026700     03 GMT-DIST.                                                         
026800*                                 DISTRIKTS INFORMATION                   
026900        05 GMT-FLAUTORD      PIC X.                                       
027000*                                 FLAGGAN STYR OM AUTOMATORDER-           
027100*                                 NUMMER SKALL SKAPAS                     
027200*                                 FLAG THAT STATES IF AUTOMATIC           
027300*                                 ORDER NUMBER SHALL BE GENERATED         
027400        05 GMT-FLNC          PIC X.                                       
027500*                                 NEW CONCEPT FLAGGA                      
027600*                                 NEW CONCEPT FLAG                        
027700        05 GMT-FLPRERS       PIC X.                                       
027800*                                 PRISERSÄTTNINGSFLAGGA                   
027900*                                 PRICE REPLACEMENT FLAG                  
028000        05 GMT-FLSAMFAK      PIC X.                                       
028100*                                 SAMFAKTURERING NEW CONCEPT              
028200*                                 CO-INVOICING NEW CONCEPT                
028300        05 GMT-FLVR          PIC X.                                       
028400*                                 ANSLUTEN TILL VR-SYST                   
028500*                                 ASSOCIATED TO VR SYST                   
028600        05 GMT-FLURSRAP      PIC X.                                       
028700*                                 URSPRUNGSRAPPORTERING VID               
028800*                                 PACKNING                                
028900*                                 FLAG FOR REPORTING OF ORIGIN            
029000        05 GMT-IDRFTAB       PIC X(3).                                    
029100*                                 RANSONERINGSFAKTORTABELL                
029200*                                 TABLE WITH RATIONING FACTORS            
029300        05 GMT-KDORDING      PIC 9.                                       
029400*                                 UPPDATERING ORDERINGÅNG                 
029500*                                 ORDER STATISTICS                        
029600        05 GMT-KVVECKOR-OB   PIC Z(2)9.                                   
029700*                                 LAGRINGSTID I VECKOR, ORDBEKR.          
029800*                                 STORAGE TIME ORDER CONFIRMATION         
029900        05 GMT-IDDC-TVSVOR   OCCURS 16 TIMES                              
030000                             PIC X(2).                                    
030100*                                 TVÅNGSSTYRNING AV VOR-SLÄPP             
030200*                                 FRÅN DC                                 
030300*                                 VOR-RELEASE FORCED TO DC                
030400*                                                                         
030500     03 GMT-LDC-KUND.                                                     
030600*                                 LDC-KUND INFORMATION                    
030700        05 GMT-IDRFSDGRDC    OCCURS 4 TIMES.                              
030800*                                 GRUPP FÖR RFS DAGAR PER DC              
030900*                                 GROUP OF RFS DAYS PER DC                
031000           07 GMT-IDDC-RFS   PIC X(2).                                    
031100*                                 DC FÖR RFS DAGAR FÖRE REPDAG            
031200*                                 DC OF RFS DAYS BEFORE REPAIR            
031300           07 GMT-KVDAGAR-RFS                                             
031400                             PIC 9.                                       
031500*                                 ANT DGR FÖR RFS/DC FÖRE REPDAT          
031600*                                 NO DAYS OF RFS/DC BEFORE REPAIR         
031700        05 GMT-KVDAGAR-RFS-DEF                                            
031800                             PIC 9.                                       
031900*                                 DEFAULT DAGAR RFS FÖRE REPDAT           
032000*                                 DEFAULT DAYS RFS BEFORE REPAIR          
032100        05 GMT-FLLDCKND      PIC X.                                       
032200*                                 FL LDC-KUND                             
032300*                                 FL LDC CUSTOMER                         
032400        05 GMT-FLKVBRYT-ORDKL1                                            
032500                             PIC X.                                       
032600*                                 KVANTITET BRYTES                        
032700*                                 QUANTITY BREAK                          
032800        05 GMT-FLKVBRYT-ORDKL2                                            
032900                             PIC X.                                       
033000*                                 KVANTITET BRYTES                        
033100*                                 QUANTITY BREAK                          
033200        05 GMT-FLKVBRYT-ORDKL3                                            
033300                             PIC X.                                       
033400*                                 KVANTITET BRYTES                        
033500*                                 QUANTITY BREAK                          
033600        05 GMT-FLKVBRYT-ORDKL4                                            
033700                             PIC X.                                       
033800*                                 KVANTITET BRYTES                        
033900*                                 QUANTITY BREAK                          
034000        05 GMT-IDDC-DAY-ALT  PIC X(2).                                    
034100*                                 DAG DC BARA UNDER GIVNA TIDER           
034200*                                 DAY DC UNDER SPECIFIED HOURS            
034300        05 GMT-IDDC-RET72    OCCURS 3 TIMES                               
034400                             PIC X(2).                                    
034500*                                 MOTTAGANDE LAGER FÖR 72-RETURER         
034600*                                 RECEIVING DC FOR 72-RETURNS             
034700        05 GMT-TIHHMM-START  PIC Z9.9(2).                                 
034800*                                 KLOCKSLAG (TIMMAR/MIN.) START           
034900*                                 TIME IN HOUR AND MINUTE START           
035000        05 GMT-TIHHMM-STOP   PIC Z9.9(2).                                 
035100*                                 KLOCKSLAG (TIMMAR/MIN.) STOP            
035200*                                 TIME IN HOUR AND MINUTE STOP            
035300        05 GMT-KVDAGAR-CDC   PIC Z9.                                      
035400*                                 CDC-DAGAR                               
035500        05 GMT-KVDAGAR-SDC   PIC Z9.                                      
035600*                                 SDC-DAGAR                               
035700        05 GMT-IDDC-PREPLAN  OCCURS 8 TIMES                               
035800                             PIC X(2).                                    
035900*                                 PRE-PLANNED RELEASE TO DC               
036000*                                 PRE-PLANNED RELEASE TO DC               
036100*                                                                         
036200     03 GMT-FLAUTREM         PIC X.                                       
036300*                                 AUTOMATISK REMISS (Y/N)                 
036400*                                 REMISS AUTO APPROVE (Y/N)               
036500     03 GMT-FILLER24         PIC X(24).                                   
036600*** END OF VILMAII-COPY LENGTH= 1124 BYTES                                
