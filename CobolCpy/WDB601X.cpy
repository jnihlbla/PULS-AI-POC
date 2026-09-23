000100 01  DCS-WDB601X.                                                         
000200*                                 DC STYRREGISTER                         
000300*                                                                         
000400*                                 FYSISK NYCKEL: IDDC                     
000500*                                                                         
000600     03 DCS-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 DCS-BEGMT.                                                        
001000*                                 GODSMOTTAGARNAMN                        
001100*                                 GOODS RECEIVER NAME                     
001200        05 DCS-BEGMT-RAD1    PIC X(35).                                   
001300*                                 GODSMOTTAGARNAMN RAD 1                  
001400*                                 GOODS RECEIVER NAME LINE 1              
001500        05 DCS-BEGMT-RAD2    PIC X(35).                                   
001600*                                 GODSMOTTAGARNAMN RAD 2                  
001700*                                 GOODS RECEIVER NAME LINE 2              
001800     03 DCS-ADGMT.                                                        
001900*                                 GODSMOTTAGARADRESS                      
002000*                                 GOODS RECEIVER ADDRESS                  
002100        05 DCS-ADGMT-GATA    PIC X(35).                                   
002200*                                 GODSMOTTAGARADRESS GATA                 
002300*                                 GOODS RECEIVER ADDRESS STREET           
002400        05 DCS-ADGMT-PADR    PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS POSTADRESS           
002600*                                 GOODS RECEIVER ADDRESS TOWN             
002700        05 DCS-ADPOST-PNRORT REDEFINES DCS-ADGMT-PADR.                    
002800*                                 POSTNUMMER + ORT                        
002900*                                 POSTAL CODE + CITY                      
003000           07 DCS-ADPOSTNR   PIC X(10).                                   
003100*                                 POSTNUMMER I ADRESS                     
003200*                                 POSTAL CODE IN ADDRESS                  
003300           07 DCS-ADCITY     PIC X(25).                                   
003400*                                 BENÄMNING PÅ STAD                       
003500*                                 CITY                                    
003600        05 DCS-ADPOST-ORTPNR REDEFINES DCS-ADGMT-PADR.                    
003700*                                 ORT + POSTNUMMER                        
003800*                                 CITY + POSTAL CODE                      
003900           07 DCS-ADCITY     PIC X(25).                                   
004000*                                 BENÄMNING PÅ STAD                       
004100*                                 CITY                                    
004200           07 DCS-ADPOSTNR   PIC X(10).                                   
004300*                                 POSTNUMMER I ADRESS                     
004400*                                 POSTAL CODE IN ADDRESS                  
004500        05 DCS-ADGMT-LAND    PIC X(35).                                   
004600*                                 GODSMOTTAGARADRESS LAND                 
004700*                                 GOODS RECEIVER ADDRESS COUNTRY          
004800     03 DCS-FLDCRET          PIC X.                                       
004900*                                 GODKÄND FÖR RETUR                       
005000*                                 RETURN APPROVED                         
005100     03 DCS-FLEXCP1-PRIO     PIC X.                                       
005200*                                 UNDANTAGSREGEL I PRIOBERÄKNING          
005300*                                 EXC. RULE WHEN CALC. PRIORITY           
005400     03 DCS-FLTRANS-PAS      PIC X.                                       
005500*                                 TRANSFER PASSIV ART OK?                 
005600*                                 TRANSFER PASSIVE PART OK?               
005700     03 DCS-FLTRANS-ERS      PIC X.                                       
005800*                                 TRANSFER OK OM VISS ERSÄTTNING?         
005900*                                 TRANSFER OK FOR SOME SUPERSESS?         
006000     03 DCS-FLRETUR-PAS      PIC X.                                       
006100*                                 RETUR OK FÖR PASSIV ART?                
006200*                                 RETURN OK FOR PASSIVE PART?             
006300     03 DCS-FLSKROT-PAS      PIC X.                                       
006400*                                 OK ATT SKROTA PASSIV ART?               
006500*                                 OK TO SCRAP PASSIVE PART?               
006600     03 DCS-FLEXCP1-REFBEO   PIC X.                                       
006700*                                 UND.REGEL AKT. REFILLBEORDRING          
006800*                                 EXC. RULE AKT. REF. ORDERING            
006900     03 DCS-FLEXCP2-REFBEO   PIC X.                                       
007000*                                 UND.REGEL AKT. REFILLBEORDRING          
007100*                                 EXC. RULE AKT. REF. ORDERING            
007200     03 DCS-FLEXCP3-REFBEO   PIC X.                                       
007300*                                 UND.REGEL AKT. REFILLBEORDRING          
007400*                                 EXC. RULE AKT. REF. ORDERING            
007500     03 DCS-FLEXCP4-REFBEO   PIC X.                                       
007600*                                 UND.REGEL AKT. REFILLBEORDRING          
007700*                                 EXC. RULE AKT. REF. ORDERING            
007800     03 DCS-FLEXCP1-REFBER   PIC X.                                       
007900*                                 UND.REGEL I REFILLBERÄKNINGEN           
008000*                                 EXC. RULE CALC. REFILLORDERS            
008100     03 DCS-FLEXCP2-REFBER   PIC X.                                       
008200*                                 UND.REGEL I REFILLBERÄKNINGEN           
008300*                                 EXC. RULE CALC. REFILLORDERS            
008400     03 DCS-FLINLHIST        PIC X.                                       
008500*                                 OM INLEVERANSHIST. SKA SKAPAS           
008600*                                 IF HIST. OF INBOUND BE CREATED          
008700     03 DCS-FLFSEDEL         PIC X.                                       
008800*                                 FÖLJESEDELS FLAGGA                      
008900*                                 J = FÖLJESEDEL SKALL SKAPAS             
009000*                                 N = FÖLJESEDEL SKALL EJ SKAPAS          
009100     03 DCS-FLINLREP         PIC X.                                       
009200*                                 FLAGGA AVVIKELSERAPPORTER               
009300*                                 DEVIATION REPORT FLAG                   
009400     03 DCS-FLBINNUT         PIC X.                                       
009500*                                 BINNING RAPPORT UTSKRIFTFLAGGA          
009600*                                 FLAG FOR BINNING REPORT                 
009700     03 DCS-FLPRISSPR        PIC X.                                       
009800*                                 PRISSPÄRRSFLAGGA                        
009900*                                 BLOCKED PRICE MARK                      
010000     03 DCS-FLSAMPAK         PIC X.                                       
010100*                                 SAMPACKNING AV KOLLI                    
010200*                                 CO-PACKING OF CASE                      
010300     03 DCS-FLSEASBER        PIC X.                                       
010400*                                 SÄSONGSBERÄKNGSFLAGGA                   
010500*                                 SEASON MARK COMPUTING                   
010600     03 DCS-FLTYP6JU         PIC X.                                       
010700*                                 TYPE-6 ADJUSTMENT                       
010800*                                 TYPE-6 ADJUSTMENT                       
010900     03 DCS-FLRSI            PIC X.                                       
011000*                                 FLAGGA SKEPPNINGSINFO TILL VIPS         
011100*                                 FLAG SHIPMENT INFO TO VIPS              
011200     03 DCS-FLWEBDC          PIC X.                                       
011300*                                 DC MED WEB GRÄNSSNITT                   
011400*                                 DC WITH WEB INTERFACE                   
011500     03 DCS-IDDISTR-REFILL   PIC 9(5).                                    
011600*                                 REFILL DISTRIKT                         
011700*                                 REFILL DISTRICT                         
011800     03 DCS-IDDISTR-RETUR    PIC 9(5).                                    
011900*                                 RETUR DISTRIKT                          
012000*                                 RETUR DISTRICT                          
012100     03 DCS-IDDISTR-QRETUR   PIC 9(5).                                    
012200*                                 KAVLITET RETUR DISTRIKT                 
012300*                                 QUALITY RETURN DISTRICT                 
012400     03 DCS-IDDISTR-SKROT    PIC 9(5).                                    
012500*                                 SKROT DISTRIKT                          
012600*                                 SCRAP DISTRICT                          
012700     03 DCS-IDDISTR-QSKROT   PIC 9(5).                                    
012800*                                 KVALITET SKROT DISTRIKT                 
012900*                                 QUALITY SCRAP DISTRICT                  
013000     03 DCS-IDDISTR-RSKROT   PIC 9(5).                                    
013100*                                 SKROT DISTRIKT FÖR RETURER              
013200*                                 SCRAP DISTRICT FOR RETURNS              
013300     03 DCS-IDKUNDNR-BPS     PIC 9(7).                                    
013400*                                 KUNDNUMMER FÖR BYPASSORDER              
013500*                                 CUSTOMER NO FOR BYPASSORDER             
013600     03 DCS-IDKUNDNR-SBPS    PIC 9(7).                                    
013700*                                 KUND FÖR SNABB BYPASSORDER              
013800*                                 CUSTOMER FOR FAST BYPASSORDER           
013900     03 DCS-IDKUNDNR-RETUR   PIC 9(7).                                    
014000*                                 KUNDNUMMER FÖR RETUR                    
014100*                                 CUSTOMER NO FOR RETURN                  
014200     03 DCS-IDKUNDNR-QRETUR  PIC 9(7).                                    
014300*                                 KUNDNUMMER FÖR KVALITETSRETUR           
014400*                                 CUSTOMER NO FOR QUALITY RETURN          
014500     03 DCS-IDKUNDNR-SRETUR  PIC 9(7).                                    
014600*                                 KUNDNUMMER FÖR SNABBRETUR               
014700*                                 CUSTOMER NO FOR FASTRETURN              
014800     03 DCS-IDKUNDNR-SQRET   PIC 9(7).                                    
014900*                                 KUNDNUMMER FÖR SNABB KVAL.RETUR         
015000*                                 CUSTOMER NO FOR FAST QUALRETURN         
015100     03 DCS-IDKUNDNR-TRETUR  PIC 9(7).                                    
015200*                                 KUNDNUMMER FÖR TOTALRETUR               
015300*                                 CUSTOMER NO FOR TOTALRETURN             
015400     03 DCS-IDKUNDNR-SKROT   PIC 9(7).                                    
015500*                                 KUNDNUMMER FÖR SKROT                    
015600*                                 CUSTOMER NO FOR SCRAP                   
015700     03 DCS-IDKUNDNR-QSKROT  PIC 9(7).                                    
015800*                                 KUNDNUMMER FÖR KVALITET SKROT           
015900*                                 CUSTOMER NO FOR QUALITY SCRAP           
016000     03 DCS-IDKUNDNR-RSKROT  PIC 9(7).                                    
016100*                                 KUNDNUMMER FÖR SKROT AV RETUR           
016200*                                 CUSTOMER NO SCRAP OF RETURNS            
016300     03 DCS-IDKUNDNR-SORD    PIC 9(7).                                    
016400*                                 KUNDNUMMER FÖR SNABBORDER               
016500*                                 CUSTOMER NO FOR FAST ORDER              
016600     03 DCS-IDLANDX2         PIC X(2).                                    
016700      88 DCS-EMIRATES        VALUE 'AE'.                                  
016800      88 DCS-AUSTRALIA       VALUE 'AU'.                                  
016900      88 DCS-AUSTRIA         VALUE 'AT'.                                  
017000      88 DCS-BELGIUM         VALUE 'BE'.                                  
017100      88 DCS-BRASIL          VALUE 'BR'.                                  
017200      88 DCS-CANADA          VALUE 'CA'.                                  
017300      88 DCS-SWIZERLAND      VALUE 'CH'.                                  
017400      88 DCS-CHINA           VALUE 'CN'.                                  
017500      88 DCS-GERMANY         VALUE 'DE'.                                  
017600      88 DCS-SPAIN           VALUE 'ES'.                                  
017700      88 DCS-FINLAND         VALUE 'FI'.                                  
017800      88 DCS-FRANCE          VALUE 'FR'.                                  
017900      88 DCS-ENGLAND         VALUE 'GB'.                                  
018000      88 DCS-HUNGARY         VALUE 'HU'.                                  
018100      88 DCS-INDIA           VALUE 'IN'.                                  
018200      88 DCS-ITALY           VALUE 'IT'.                                  
018300      88 DCS-JAPAN           VALUE 'JP'.                                  
018400      88 DCS-KOREA           VALUE 'KR'.                                  
018500      88 DCS-MALAYSIA        VALUE 'MY'.                                  
018600      88 DCS-MAROCKO         VALUE 'MA'.                                  
018700      88 DCS-MEXICO          VALUE 'MX'.                                  
018800      88 DCS-HOLLAND         VALUE 'NL'.                                  
018900      88 DCS-NORWAY          VALUE 'NO'.                                  
019000      88 DCS-POLAND          VALUE 'PL'.                                  
019100      88 DCS-RUSSIA          VALUE 'RU'.                                  
019200      88 DCS-SWEDEN          VALUE 'SE'.                                  
019300      88 DCS-THAILAND        VALUE 'TH'.                                  
019400      88 DCS-TURKEY          VALUE 'TR'.                                  
019500      88 DCS-TAIWAN          VALUE 'TW'.                                  
019600      88 DCS-USA             VALUE 'US'.                                  
019700      88 DCS-SOUTH-AFRICA    VALUE 'ZA'.                                  
019800      88 DCS-LAND-NON-VCC-OWNED                                           
019900                             VALUE 'AE'                                   
020000                             'BR'                                         
020100                             'CN'                                         
020200                             'IN'                                         
020300                             'KR'                                         
020400                             'MY'                                         
020500                             'MX'                                         
020600                             'RU'                                         
020700                             'TH'                                         
020800                             'TR'                                         
020900                             'TW'                                         
021000                             'ZA'.                                        
021100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
021200*                                 2-LETTER CODE FOR COUNTRY               
021300     03 DCS-IDLISTNR         PIC 9(3).                                    
021400*                                 LISTNUMMER                              
021500*                                 LISTNUMMER                              
021600     03 DCS-IDPRTLST-INL     PIC X(8).                                    
021700*                                 LOGISK PRINTER+LISTA IDENTITET          
021800*                                 LOGICAL PRINTER+LIST IDENTITY           
021900     03 DCS-IDPRTLST-INLA    PIC X(8).                                    
022000*                                 LOGISK PRINTER+LISTA IDENTITET          
022100*                                 LOGICAL PRINTER+LIST IDENTITY           
022200     03 DCS-IDPRTLST-INVA    PIC X(8).                                    
022300*                                 PRINTER FÖR INV.ANMODAN                 
022400*                                 IDENTITY OF INVENTORY PRINTER           
022500     03 DCS-IDPRTLST-INVAB   PIC X(8).                                    
022600*                                 PRINTER FÖR INV.ANMODAN/BYTES           
022700*                                 IDENTITY OF INVENTORY/? PRINTER         
022800     03 DCS-IDTIDZON         PIC 9(2).                                    
022900*                                 TIDZONER PÅ JORDEN.                     
023000*                                 TIME ZONE ON EARTH                      
023100     03 DCS-IDVAT            PIC X(17).                                   
023200*                                 MOMSREGISTRERINGSNUMMER                 
023300*                                 VAT REGISTRATION NUMBER                 
023400     03 DCS-KDDC             PIC X(2).                                    
023500      88 DCS-CDC             VALUE 'C '.                                  
023600      88 DCS-DDC             VALUE 'D '.                                  
023700      88 DCS-NDC             VALUE 'NA'                                   
023800                             'NC'                                         
023900                             'NP'                                         
024000                             'NX'.                                        
024100      88 DCS-NDC-NA          VALUE 'NA'.                                  
024200      88 DCS-NDC-PF          VALUE 'NP'.                                  
024300      88 DCS-NDC-CN          VALUE 'NC'.                                  
024400      88 DCS-NDC-OTHERS      VALUE 'NX'.                                  
024500      88 DCS-SDC             VALUE 'S '.                                  
024600      88 DCS-CDC-TR          VALUE 'TR'.                                  
024700*                                 TYP AV DISTR. LAGER                     
024800*                                 TYPE OF DELIV. CENTER                   
024900     03 DCS-KDDCSTYR-BUY     PIC 9(5).                                    
025000*                                 REGELVERK VID BUYERTILLDELNING          
025100*                                 RULES OF BUYER                          
025200     03 DCS-KDDCSTYR-KUND    PIC 9(5).                                    
025300*                                 REGELVERK FÖR TILLD. AV KUNDNR          
025400*                                 RULES OF CUSTOMER NO                    
025500     03 DCS-KDDCSTYR-REFTAB  PIC 9(5).                                    
025600*                                 REGELVERK TILLD AV REFILLTABELL         
025700*                                 RULES WHICH REFILLTABLE                 
025800     03 DCS-KDFRAKT-BPS      PIC 9(3).                                    
025900*                                 FRAKTSÄTT DC TILL KUND                  
026000*                                 FREIGHT CODE                            
026100     03 DCS-KDFRAKT-SBPS     PIC 9(3).                                    
026200*                                 FRAKTSÄTT DC TILL KUND                  
026300*                                 FREIGHT CODE                            
026400     03 DCS-KDPORDL          PIC X.                                       
026500*                                 PACKAD ORDERLISTA MÖJLIG                
026600*                                 PACKED ORDER LIST POSSIBLE              
026700     03 DCS-KDSKRMET         PIC 9.                                       
026800*                                 SKROTMETOD                              
026900*                                 SCRAP METHOD                            
027000     03 DCS-KDVALISO         PIC X(3).                                    
027100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
027200*                                 CURRENCY CODE BY ISO-STANDARD.          
027300     03 DCS-FLINVACS         PIC X.                                       
027400*                                 ACS-INVENTERING?                        
027500*                                 ACS INVENTORY?                          
027600     03 DCS-IDFTG            PIC 9(2).                                    
027700      88 DCS-FTG-US          VALUE 53.                                    
027800      88 DCS-FTG-CA          VALUE 54.                                    
027900      88 DCS-FTG-PV          VALUE 57.                                    
028000      88 DCS-FTG-CN          VALUE 60.                                    
028100      88 DCS-FTG-IN          VALUE 61.                                    
028200      88 DCS-FTG-TH          VALUE 63.                                    
028300      88 DCS-FTG-TW          VALUE 64.                                    
028400      88 DCS-FTG-KR          VALUE 65.                                    
028500      88 DCS-FTG-MY          VALUE 66.                                    
028600      88 DCS-FTG-RU          VALUE 81.                                    
028700      88 DCS-FTG-BR          VALUE 82.                                    
028800      88 DCS-FTG-MX          VALUE 83.                                    
028900      88 DCS-FTG-ZA          VALUE 85.                                    
029000      88 DCS-FTG-TR          VALUE 86.                                    
029100      88 DCS-FTG-AE          VALUE 87.                                    
029200*                                 FÖRETAGSID EKONOM REDOVISNING           
029300*                                 COMPANY IDENTITY ACCOUNTING             
029400     03 DCS-KVINVAUT         PIC 9(9).                                    
029500*                                 GRÄNS AUTOMATISK INVENTERING            
029600*                                 LIMIT FOR AUTOMATIC INV.                
029700     03 DCS-SUINVGRANS       PIC 9(9).                                    
029800*                                 GRÄNS INVENTERING DESCREPENCY           
029900*                                 LIMIT FOR INVENTORY DESCREPENCY         
030000     03 DCS-KDRT-MIX         PIC 9(2).                                    
030100*                                 REDOVISN. JUSTERING/BLAND. ART          
030200*                                 JUST ORDER ACC WITH MIXED PARTS         
030300     03 DCS-REQXBRYT         PIC 9(2).                                    
030400*                                 KVANTBRYTNING                           
030500*                                 NUMBER                                  
030600     03 DCS-REWILSON         PIC 9V9(2).                                  
030700*                                 PROCENTREGEL FÖR WILSONFORMEL           
030800     03 DCS-IDLEVNR-DC       PIC X(5).                                    
030900*                                 DC LEVERANTÖR                           
031000*                                 DC SUPPLIER                             
031100     03 DCS-FLKNDVAL         PIC X.                                       
031200*                                 STYRNING PÅ KUND ELLER DISTRIKT         
031300*                                 STEERING CUSTOMOR OR DISTRICT           
031400     03 DCS-TIHHMM-START     PIC 9(4).                                    
031500*                                 KLOCKSLAG (TIMMAR/MIN.) START           
031600*                                 TIME IN HOUR AND MINUTE START           
031700     03 DCS-TIHHMM-READY     PIC 9(4).                                    
031800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
031900*                                 AVSLUTNING                              
032000*                                 TIME IN HOUR AND MINUTE FINISH          
032100     03 DCS-KDFAKTDC         PIC X.                                       
032200*                                 FAKT. STATISTIK URVAL PER DC            
032300*                                 IF INVOICE STATISTIC PER DC             
032400     03 DCS-FLARTDC          PIC X.                                       
032500*                                 INDIKERAR OM ART SKA VARA PÅ DC         
032600*                                 INDICATES IF PART MUST BE ON DC         
032700     03 DCS-IDDC-REF         PIC X(2).                                    
032800*                                 SÄNDANDE LAGER FÖR REFILL               
032900*                                 SENDING WAREHOUSE FOR REFILL            
033000     03 DCS-IDPERSON-REM     PIC 9(3).                                    
033100*                                 PERSONKOD REMISS                        
033200*                                 STAFF CODE CONSIDERATION                
033300     03 DCS-IDDISTR-JUST     PIC 9(5).                                    
033400*                                 DISTRIKT FÖR JUSTERINGSORDER            
033500*                                 JUSTIFY ORDER DISTRICT                  
033600     03 DCS-IDDISTR-MIX      PIC 9(5).                                    
033700*                                 DISTRIKT JUSTERING/BLAND.ART.           
033800*                                 JUSTIFY ORDER WITH MIXED PARTS          
033900     03 DCS-IDKUNDNR-JUST    PIC 9(7).                                    
034000*                                 KUND FÖR JUSTERINGSORDER                
034100*                                 JUSTIFY ORDER CUSTOMER                  
034200     03 DCS-IDKUNDNR-MIX     PIC 9(7).                                    
034300*                                 KUND JUSTERING/BLAND.ART.               
034400*                                 JUSTIFY ORDER WITH MIXED PARTS          
034500     03 DCS-IDKST-JUST       PIC X(10).                                   
034600*                                 KOSTNADSST. FÖR JUSTERINGSORDER         
034700*                                 JUSTIFY ORDER / COST CENTER             
034800     03 DCS-IDKST-MIX        PIC X(10).                                   
034900*                                 KOSTNADSST. BLANDADE ARTIKLAR           
035000*                                 COST CENTER FOR MIXED PARTS             
035100     03 DCS-IDKST-SKROT      PIC X(10).                                   
035200*                                 KOSTNADSST. FÖR SKROTORDER              
035300*                                 COST CENTER FOR SCRAP ORDERS            
035400     03 DCS-IDKONTO-JUST     PIC 9(10).                                   
035500*                                 KONTO FÖR JUSTERINGSORDER               
035600*                                 JUSTIFY ORDER ACCOUNT                   
035700     03 DCS-IDKONTO-MIX      PIC 9(10).                                   
035800*                                 KONTO FÖR JUSTERING/BLAND.ART.          
035900*                                 JUSTIFY ORDER WITH MIXED PARTS          
036000     03 DCS-IDKONTO-SKROT    PIC 9(10).                                   
036100*                                 KONTO FÖR SKROTORDER                    
036200*                                 SCRAP ORDER ACCOUNT                     
036300     03 DCS-IDANALYS-JUST    PIC X(12).                                   
036400*                                 ANALYSNR FÖR JUSTERINGSORDER            
036500*                                 JUSTIFY ORDER ANALYSES NO.              
036600     03 DCS-IDANALYS-MIX     PIC X(12).                                   
036700*                                 ANALYSNR JUSTERING/BLAND.ART.           
036800*                                 JUSTIFY ORDER WITH MIXED PARTS          
036900     03 DCS-IDANALYS-SKROT   PIC X(12).                                   
037000*                                 ANALYSNR FÖR SKROTORDER                 
037100*                                 SCRAP ORDER ANALYSES NO.                
037200     03 DCS-TID-RETOS        PIC 9.                                       
037300*                                 VECKODAG FÖR AUTOMATRETUR MÅN=1         
037400*                                 WEEK DAY FOR AUTOM.RETURNS              
037500     03 DCS-TID-RET98        PIC 9.                                       
037600*                                 VECKODAG FÖR AUTOM.RETUR LO 98          
037700*                                 WEEKDAY AUTOM.RETURNS AREA 98           
037800     03 DCS-FLOVRLAGBER      PIC X.                                       
037900*                                 SDC ÖVERLAGER INKL CDC                  
038000*                                 SDC OVERSTOCK INCL CDC                  
038100     03 DCS-KVDAGAR-CROSS    PIC 9(3).                                    
038200*                                 ANTAL DAGAR FÖRDR. CROSS-DOCK           
038300     03 DCS-TIUPPDAT         PIC 9(6).                                    
038400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
038500*                                 UPDATING DATE     (YYMMDD)              
038600     03 DCS-IDUSER           PIC X(8).                                    
038700*                                 ANVÄNDARENS SÄKERHETS ID                
038800*                                 USER SECURITY-IDENTITY                  
038900     03 DCS-IDDC-TPAS-1      PIC X(2).                                    
039000*                                 DC 1 FÖR TRANSFER AV PASS ART           
039100*                                 DC 1 FOR TRANSFER OF PASS PART          
039200     03 DCS-IDDC-TPAS-2      PIC X(2).                                    
039300*                                 DC 2 FÖR TRANSFER AV PASS ART           
039400*                                 DC 2 FOR TRANSFER OF PASS PART          
039500     03 DCS-IDDC-TPAS-3      PIC X(2).                                    
039600*                                 DC 3 FÖR TRANSFER AV PASS ART           
039700*                                 DC 3 FOR TRANSFER OF PASS PART          
039800     03 DCS-KVPB-LIM         PIC 9(6)V9(1).                               
039900*                                 PERIODBEHOV GRÄNS                       
040000*                                 FORECAST LIMIT                          
040100     03 DCS-KVPERIOD-TPAS    PIC 9(3).                                    
040200*                                 ANTAL PERIODER VID BERÄKNING AV         
040300*                                 TRANSFER FÖR PASSIV ARTIKEL             
040400*                                 NBR OF PERIODS WHEN COMPUTING           
040500*                                 TRANSFER OF PASSIVE PART                
040600     03 DCS-KVSKROT-SPAS     PIC 9(3).                                    
040700*                                 MAX GRÄNS FÖR SKROT PASSIVE ART         
040800*                                 MAX LIMIT FOR SCRAPING OF PARTS         
040900     03 DCS-KVVECKOR-RPAS    PIC 9(3).                                    
041000*                                 ANTAL VECKOR KONTROLL OI VID            
041100*                                 RETUR AV PASSIV ARTIKEL                 
041200*                                 NO. WEEKS MONITOR ORDER STATS           
041300*                                 DURING RETURN OF PASSIVE PART           
041400     03 DCS-KVVECKOR-TPAS    PIC 9(3).                                    
041500*                                 ANTAL VECKOR KONTROLL OI VID            
041600*                                 TRANSFER AV PASSIV ARTIKEL              
041700*                                 NO. WEEKS MONITOR ORDER STATS           
041800*                                 DURING TRANSFER OF PASSIVE PART         
041900     03 DCS-SUARTMIN-RPAS    PIC 9(6).                                    
042000*                                 MIN LV FÖR RETUR PASSIV ART             
042100*                                 MIN STOCK VALUE FOR RETURN              
042200*                                 OF PASSIVE PART                         
042300     03 DCS-SUARTMIN-TPAS    PIC 9(6).                                    
042400*                                 MIN LV FÖR TRANSFER AV EN               
042500*                                 PASSIV ARTIKEL                          
042600*                                 MIN STOCK VALUE FOR TRANSFER            
042700*                                 OF PASSIVE PART                         
042800     03 DCS-SUVARLIM-TPAS    PIC 9(6).                                    
042900*                                 VÄRDEGRÄNS PASSIV TRANSFER              
043000*                                 VALUE LIMIT PASSIVE TRANSFER            
043100     03 DCS-TID-TRS          PIC 9.                                       
043200*                                 VECKODAG AUTOM.TRANSF,RET,SKROT         
043300*                                 WEEKDAY AUTOM.TRANSF,RET,SCRAP          
043400     03 DCS-TIVV             PIC 9(2).                                    
043500*                                 VECKA  (VV)                             
043600*                                 WEEK   (WW)                             
043700     03 DCS-FLTRLO88-MAN     PIC X.                                       
043800*                                 FLAGGA LO-88 TRANSFER-MÅN OK?           
043900*                                 FLAG TRANSFER-MON TO AREA-88 OK         
044000     03 DCS-FLTRLO88-TIS     PIC X.                                       
044100*                                 FLAGGA LO-88 TRANSFER-TIS OK?           
044200*                                 FLAG TRANSFER-TUE TO AREA-88 OK         
044300     03 DCS-FLTRLO88-ONS     PIC X.                                       
044400*                                 FLAGGA LO-88 TRANSFER-ONS OK?           
044500*                                 FLAG TRANSFER-WEN TO AREA-88 OK         
044600     03 DCS-FLTRLO88-TOR     PIC X.                                       
044700*                                 FLAGGA LO-88 TRANSFER-TOR OK?           
044800*                                 FLAG TRANSFER-THU TO AREA-88 OK         
044900     03 DCS-FLTRLO88-FRE     PIC X.                                       
045000*                                 FLAGGA LO-88 TRANSFER-FRE OK?           
045100*                                 FLAG TRANSFER-FRI TO AREA-88 OK         
045200     03 DCS-KDRT-JUST        PIC 9(2).                                    
045300*                                 REDOVISN.TYP JUSTERINGSORDER            
045400*                                 JUSTIFY ORDER ACCOUNT TYPE              
045500     03 DCS-KDRT-SKROT       PIC 9(2).                                    
045600*                                 REDOVISNINGSTYP SKROTORDER              
045700*                                 SCRAP ORDER ACCOUNT TYPE                
045800     03 DCS-KVVECKOR-BIN     PIC 9(3).                                    
045900*                                 ANTAL VECKOR FÖR LAGERUTTAG             
046000*                                 NO. OF WEEKS FOR DESTOCKING             
046100     03 DCS-KVVECKOR-SPAS    PIC 9(3).                                    
046200*                                 ANTAL VECKOR SKROT PASSIV ART           
046300*                                 WEEKS FOR SCRAP PASSIVE PART            
046400     03 DCS-IDTECKEN-SPAS    PIC X.                                       
046500*                                 TECKEN ( >,=,< ) SKROT ARTIKLAR         
046600*                                 SIGN (>,=,<) FOR SCRAPING PARTS         
046700     03 DCS-PRARTSTD-SPAS    PIC 9(7)V9(2).                               
046800*                                 PRISGRÄNS FÖR SKROTN.AV PAS-ART         
046900*                                 PRICE LIMIT FOR SCRAPING PARTS          
047000     03 DCS-ADLAGOMR-SPAS    PIC 9(2).                                    
047100*                                 LAGEROMRÅDE SKROTNING PASS-ART          
047200*                                 AREA FOR SCRAP PASSIVE PARTS            
047300     03 DCS-IDPERSON-SPAS    PIC 9(3).                                    
047400*                                 PERSON FÖR SKROT AV PASSIV ART          
047500*                                 STAFF SCRAPING PASSIVE PARTS            
047600     03 DCS-KDPRODSL-SPAS    PIC 9(3).                                    
047700*                                 PRODUKTSLAG SKROT PASSIV ART            
047800*                                 PRODUCT GR SCRAP PASSIVE PART           
047900     03 DCS-IDLEVNR-EMB      PIC X(5).                                    
048000*                                 ALT.LEV PER DC FÖR EMBALLAGE            
048100*                                 ALT.DC SUPPL FOR PACK REPORT            
048200     03 DCS-PRARTSTD-SKRLO98 PIC 9(7)V9(2).                               
048300*                                 PRISGRÄNS FÖR SKROTNING PÅ LO98         
048400*                                 PRICE LIMIT FOR SCRAPING AREA98         
048500     03 DCS-FLMAINDC         PIC X.                                       
048600*                                 FLAGGA FÖR HUVUD DC PER IDFTG           
048700*                                 FLAG FOR MAIN DC PER COMPANY            
048800     03 DCS-IDPARTNR         PIC X(9).                                    
048900*                                 FINANCIELL KUND                         
049000*                                 FINANCIAL CUST                          
049100     03 DCS-KVDAGAR-POKS     PIC 9(3).                                    
049200*                                 ANT DGR FÖRE RFS ATT NOLL OKSPR         
049300*                                 DAYS BEFORE RFS FOR MOVE OKSPR          
049400     03 DCS-IDLEGSEL         PIC X(4).                                    
049500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
049600*                                 LEGAL SELLER IDENTITY                   
049700     03 DCS-KDTRADP          PIC X(4).                                    
049800*                                 TRADING PARTNER                         
049900*                                 TRADING PARTNER                         
050000     03 DCS-KVDAGAR-PP       PIC 9(3).                                    
050100*                                 DAGAR FÖR REPARAIONSDATUM               
050200*                                 DAYS BEFORE REPAIR DATE                 
050300     03 DCS-KDAKDISP-DAG     PIC 9.                                       
050400*                                 HUR AKS RÄKNAS I DISPONIBELT            
050500*                                 HOW AKS COUNTS IN AVAILABILITY          
050600     03 DCS-KDAKDISP-BULK    PIC 9.                                       
050700*                                 HUR AKS RÄKNAS I DISPONIBELT            
050800*                                 HOW AKS COUNTS IN AVAILABILITY          
050900     03 DCS-FLCLEAR-BULK     PIC X.                                       
051000*                                 BULK ORDERRAD CLEAR FLAGGA              
051100*                                 CLEARING FLAG FOR BULK ORDER            
051200     03 DCS-FLTRACK          PIC X.                                       
051300*                                 FLAG FOR TRACKING-ID FOR A DC           
051400*                                                                         
051500*                                 FLAG FOR TRACKING-ID FOR A DC           
051600*                                                                         
051700     03 DCS-FLSTOREF         PIC X.                                       
051800*                                 STOPP REFILL FLAGGA                     
051900*                                                                         
052000*                                 STOP REFILL FLAG                        
052100*                                                                         
052200     03 DCS-FLSTOFC          PIC X.                                       
052300*                                 STOPP FC OMRÄKNING FLAGGA               
052400*                                                                         
052500*                                 STOP FC RECALC FLAG                     
052600*                                                                         
052700     03 DCS-FILLER           PIC X(71).                                   
052800*** END OF VILMAII-COPY LENGTH= 773 BYTES                                 
