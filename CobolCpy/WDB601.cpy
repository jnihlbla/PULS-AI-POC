000100 01  DCS-WDB601.                                                          
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
011500     03 DCS-IDDISTR-REFILL   PIC S9(5)           COMP-3.                  
011600*                                 REFILL DISTRIKT                         
011700*                                 REFILL DISTRICT                         
011800     03 DCS-IDDISTR-RETUR    PIC S9(5)           COMP-3.                  
011900*                                 RETUR DISTRIKT                          
012000*                                 RETUR DISTRICT                          
012100     03 DCS-IDDISTR-QRETUR   PIC S9(5)           COMP-3.                  
012200*                                 KAVLITET RETUR DISTRIKT                 
012300*                                 QUALITY RETURN DISTRICT                 
012400     03 DCS-IDDISTR-SKROT    PIC S9(5)           COMP-3.                  
012500*                                 SKROT DISTRIKT                          
012600*                                 SCRAP DISTRICT                          
012700     03 DCS-IDDISTR-QSKROT   PIC S9(5)           COMP-3.                  
012800*                                 KVALITET SKROT DISTRIKT                 
012900*                                 QUALITY SCRAP DISTRICT                  
013000     03 DCS-IDDISTR-RSKROT   PIC S9(5)           COMP-3.                  
013100*                                 SKROT DISTRIKT FÖR RETURER              
013200*                                 SCRAP DISTRICT FOR RETURNS              
013300     03 DCS-IDKUNDNR-BPS     PIC S9(7)           COMP-3.                  
013400*                                 KUNDNUMMER FÖR BYPASSORDER              
013500*                                 CUSTOMER NO FOR BYPASSORDER             
013600     03 DCS-IDKUNDNR-SBPS    PIC S9(7)           COMP-3.                  
013700*                                 KUND FÖR SNABB BYPASSORDER              
013800*                                 CUSTOMER FOR FAST BYPASSORDER           
013900     03 DCS-IDKUNDNR-RETUR   PIC S9(7)           COMP-3.                  
014000*                                 KUNDNUMMER FÖR RETUR                    
014100*                                 CUSTOMER NO FOR RETURN                  
014200     03 DCS-IDKUNDNR-QRETUR  PIC S9(7)           COMP-3.                  
014300*                                 KUNDNUMMER FÖR KVALITETSRETUR           
014400*                                 CUSTOMER NO FOR QUALITY RETURN          
014500     03 DCS-IDKUNDNR-SRETUR  PIC S9(7)           COMP-3.                  
014600*                                 KUNDNUMMER FÖR SNABBRETUR               
014700*                                 CUSTOMER NO FOR FASTRETURN              
014800     03 DCS-IDKUNDNR-SQRET   PIC S9(7)           COMP-3.                  
014900*                                 KUNDNUMMER FÖR SNABB KVAL.RETUR         
015000*                                 CUSTOMER NO FOR FAST QUALRETURN         
015100     03 DCS-IDKUNDNR-TRETUR  PIC S9(7)           COMP-3.                  
015200*                                 KUNDNUMMER FÖR TOTALRETUR               
015300*                                 CUSTOMER NO FOR TOTALRETURN             
015400     03 DCS-IDKUNDNR-SKROT   PIC S9(7)           COMP-3.                  
015500*                                 KUNDNUMMER FÖR SKROT                    
015600*                                 CUSTOMER NO FOR SCRAP                   
015700     03 DCS-IDKUNDNR-QSKROT  PIC S9(7)           COMP-3.                  
015800*                                 KUNDNUMMER FÖR KVALITET SKROT           
015900*                                 CUSTOMER NO FOR QUALITY SCRAP           
016000     03 DCS-IDKUNDNR-RSKROT  PIC S9(7)           COMP-3.                  
016100*                                 KUNDNUMMER FÖR SKROT AV RETUR           
016200*                                 CUSTOMER NO SCRAP OF RETURNS            
016300     03 DCS-IDKUNDNR-SORD    PIC S9(7)           COMP-3.                  
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
023800                             'NS'                                         
023900                             'NC'                                         
024000                             'NP'                                         
024100                             'NX'.                                        
024200      88 DCS-NDC-NA          VALUE 'NA'.                                  
024300      88 DCS-NDC-SA          VALUE 'NS'.                                  
024400      88 DCS-NDC-PF          VALUE 'NP'.                                  
024500      88 DCS-NDC-CN          VALUE 'NC'.                                  
024600      88 DCS-NDC-OTHERS      VALUE 'NX'.                                  
024700      88 DCS-SDC             VALUE 'S '.                                  
024800      88 DCS-CDC-TR          VALUE 'TR'.                                  
024900*                                 TYP AV DISTR. LAGER                     
025000*                                 TYPE OF DELIV. CENTER                   
025100     03 DCS-KDDCSTYR-BUY     PIC S9(5)           COMP-3.                  
025200*                                 REGELVERK VID BUYERTILLDELNING          
025300*                                 RULES OF BUYER                          
025400     03 DCS-KDDCSTYR-KUND    PIC S9(5)           COMP-3.                  
025500*                                 REGELVERK FÖR TILLD. AV KUNDNR          
025600*                                 RULES OF CUSTOMER NO                    
025700     03 DCS-KDDCSTYR-REFTAB  PIC S9(5)           COMP-3.                  
025800*                                 REGELVERK TILLD AV REFILLTABELL         
025900*                                 RULES WHICH REFILLTABLE                 
026000     03 DCS-KDFRAKT-BPS      PIC S9(3)           COMP-3.                  
026100*                                 FRAKTSÄTT DC TILL KUND                  
026200*                                 FREIGHT CODE                            
026300     03 DCS-KDFRAKT-SBPS     PIC S9(3)           COMP-3.                  
026400*                                 FRAKTSÄTT DC TILL KUND                  
026500*                                 FREIGHT CODE                            
026600     03 DCS-KDPORDL          PIC X.                                       
026700*                                 PACKAD ORDERLISTA MÖJLIG                
026800*                                 PACKED ORDER LIST POSSIBLE              
026900     03 DCS-KDSKRMET         PIC S9              COMP-3.                  
027000*                                 SKROTMETOD                              
027100*                                 SCRAP METHOD                            
027200     03 DCS-KDVALISO         PIC X(3).                                    
027300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
027400*                                 CURRENCY CODE BY ISO-STANDARD.          
027500     03 DCS-FLINVACS         PIC X.                                       
027600*                                 ACS-INVENTERING?                        
027700*                                 ACS INVENTORY?                          
027800     03 DCS-IDFTG            PIC 9(2).                                    
027900      88 DCS-FTG-US          VALUE 53.                                    
028000      88 DCS-FTG-CA          VALUE 54.                                    
028100      88 DCS-FTG-PV          VALUE 57.                                    
028200      88 DCS-FTG-CN          VALUE 60.                                    
028300      88 DCS-FTG-IN          VALUE 61.                                    
028400      88 DCS-FTG-TH          VALUE 63.                                    
028500      88 DCS-FTG-TW          VALUE 64.                                    
028600      88 DCS-FTG-KR          VALUE 65.                                    
028700      88 DCS-FTG-MY          VALUE 66.                                    
028800      88 DCS-FTG-RU          VALUE 81.                                    
028900      88 DCS-FTG-BR          VALUE 82.                                    
029000      88 DCS-FTG-MX          VALUE 83.                                    
029100      88 DCS-FTG-ZA          VALUE 85.                                    
029200      88 DCS-FTG-TR          VALUE 86.                                    
029300      88 DCS-FTG-AE          VALUE 87.                                    
029400*                                 FÖRETAGSID EKONOM REDOVISNING           
029500*                                 COMPANY IDENTITY ACCOUNTING             
029600     03 DCS-KVINVAUT         PIC S9(9)           COMP-3.                  
029700*                                 GRÄNS AUTOMATISK INVENTERING            
029800*                                 LIMIT FOR AUTOMATIC INV.                
029900     03 DCS-SUINVGRANS       PIC S9(9)           COMP-3.                  
030000*                                 GRÄNS INVENTERING DESCREPENCY           
030100*                                 LIMIT FOR INVENTORY DESCREPENCY         
030200     03 DCS-KDRT-MIX         PIC S9(3)           COMP-3.                  
030300*                                 REDOVISN. JUSTERING/BLAND. ART          
030400*                                 JUST ORDER ACC WITH MIXED PARTS         
030500     03 DCS-REQXBRYT         PIC S9(2)           COMP-3.                  
030600*                                 KVANTBRYTNING                           
030700*                                 NUMBER                                  
030800     03 DCS-REWILSON         PIC S9V9(2)         COMP-3.                  
030900*                                 PROCENTREGEL FÖR WILSONFORMEL           
031000     03 DCS-IDLEVNR-DC       PIC X(5).                                    
031100*                                 DC LEVERANTÖR                           
031200*                                 DC SUPPLIER                             
031300     03 DCS-FLKNDVAL         PIC X.                                       
031400*                                 STYRNING PÅ KUND ELLER DISTRIKT         
031500*                                 STEERING CUSTOMOR OR DISTRICT           
031600     03 DCS-TIHHMM-START     PIC S9(5)           COMP-3.                  
031700*                                 KLOCKSLAG (TIMMAR/MIN.) START           
031800*                                 TIME IN HOUR AND MINUTE START           
031900     03 DCS-TIHHMM-READY     PIC S9(5)           COMP-3.                  
032000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
032100*                                 AVSLUTNING                              
032200*                                 TIME IN HOUR AND MINUTE FINISH          
032300     03 DCS-KDFAKTDC         PIC X.                                       
032400*                                 FAKT. STATISTIK URVAL PER DC            
032500*                                 IF INVOICE STATISTIC PER DC             
032600     03 DCS-FLARTDC          PIC X.                                       
032700*                                 INDIKERAR OM ART SKA VARA PÅ DC         
032800*                                 INDICATES IF PART MUST BE ON DC         
032900     03 DCS-IDDC-REF         PIC X(2).                                    
033000*                                 SÄNDANDE LAGER FÖR REFILL               
033100*                                 SENDING WAREHOUSE FOR REFILL            
033200     03 DCS-IDPERSON-REM     PIC S9(3)           COMP-3.                  
033300*                                 PERSONKOD REMISS                        
033400*                                 STAFF CODE CONSIDERATION                
033500     03 DCS-IDDISTR-JUST     PIC S9(5)           COMP-3.                  
033600*                                 DISTRIKT FÖR JUSTERINGSORDER            
033700*                                 JUSTIFY ORDER DISTRICT                  
033800     03 DCS-IDDISTR-MIX      PIC S9(5)           COMP-3.                  
033900*                                 DISTRIKT JUSTERING/BLAND.ART.           
034000*                                 JUSTIFY ORDER WITH MIXED PARTS          
034100     03 DCS-IDKUNDNR-JUST    PIC S9(7)           COMP-3.                  
034200*                                 KUND FÖR JUSTERINGSORDER                
034300*                                 JUSTIFY ORDER CUSTOMER                  
034400     03 DCS-IDKUNDNR-MIX     PIC S9(7)           COMP-3.                  
034500*                                 KUND JUSTERING/BLAND.ART.               
034600*                                 JUSTIFY ORDER WITH MIXED PARTS          
034700     03 DCS-IDKST-JUST       PIC X(10).                                   
034800*                                 KOSTNADSST. FÖR JUSTERINGSORDER         
034900*                                 JUSTIFY ORDER / COST CENTER             
035000     03 DCS-IDKST-MIX        PIC X(10).                                   
035100*                                 KOSTNADSST. BLANDADE ARTIKLAR           
035200*                                 COST CENTER FOR MIXED PARTS             
035300     03 DCS-IDKST-SKROT      PIC X(10).                                   
035400*                                 KOSTNADSST. FÖR SKROTORDER              
035500*                                 COST CENTER FOR SCRAP ORDERS            
035600     03 DCS-IDKONTO-JUST     PIC S9(11)          COMP-3.                  
035700*                                 KONTO FÖR JUSTERINGSORDER               
035800*                                 JUSTIFY ORDER ACCOUNT                   
035900     03 DCS-IDKONTO-MIX      PIC S9(11)          COMP-3.                  
036000*                                 KONTO FÖR JUSTERING/BLAND.ART.          
036100*                                 JUSTIFY ORDER WITH MIXED PARTS          
036200     03 DCS-IDKONTO-SKROT    PIC S9(11)          COMP-3.                  
036300*                                 KONTO FÖR SKROTORDER                    
036400*                                 SCRAP ORDER ACCOUNT                     
036500     03 DCS-IDANALYS-JUST    PIC X(12).                                   
036600*                                 ANALYSNR FÖR JUSTERINGSORDER            
036700*                                 JUSTIFY ORDER ANALYSES NO.              
036800     03 DCS-IDANALYS-MIX     PIC X(12).                                   
036900*                                 ANALYSNR JUSTERING/BLAND.ART.           
037000*                                 JUSTIFY ORDER WITH MIXED PARTS          
037100     03 DCS-IDANALYS-SKROT   PIC X(12).                                   
037200*                                 ANALYSNR FÖR SKROTORDER                 
037300*                                 SCRAP ORDER ANALYSES NO.                
037400     03 DCS-TID-RETOS        PIC S9              COMP-3.                  
037500*                                 VECKODAG FÖR AUTOMATRETUR MÅN=1         
037600*                                 WEEK DAY FOR AUTOM.RETURNS              
037700     03 DCS-TID-RET98        PIC S9              COMP-3.                  
037800*                                 VECKODAG FÖR AUTOM.RETUR LO 98          
037900*                                 WEEKDAY AUTOM.RETURNS AREA 98           
038000     03 DCS-FLOVRLAGBER      PIC X.                                       
038100*                                 SDC ÖVERLAGER INKL CDC                  
038200*                                 SDC OVERSTOCK INCL CDC                  
038300     03 DCS-KVDAGAR-CROSS    PIC S9              COMP-3.                  
038400*                                 ANTAL DAGAR FÖRDR. CROSS-DOCK           
038500     03 DCS-TIUPPDAT         PIC 9(6).                                    
038600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
038700*                                 UPDATING DATE     (YYMMDD)              
038800     03 DCS-IDUSER           PIC X(8).                                    
038900*                                 ANVÄNDARENS SÄKERHETS ID                
039000*                                 USER SECURITY-IDENTITY                  
039100     03 DCS-IDDC-TPAS-1      PIC X(2).                                    
039200*                                 DC 1 FÖR TRANSFER AV PASS ART           
039300*                                 DC 1 FOR TRANSFER OF PASS PART          
039400     03 DCS-IDDC-TPAS-2      PIC X(2).                                    
039500*                                 DC 2 FÖR TRANSFER AV PASS ART           
039600*                                 DC 2 FOR TRANSFER OF PASS PART          
039700     03 DCS-IDDC-TPAS-3      PIC X(2).                                    
039800*                                 DC 3 FÖR TRANSFER AV PASS ART           
039900*                                 DC 3 FOR TRANSFER OF PASS PART          
040000     03 DCS-KVPB-LIM         PIC S9(6)V9(1)      COMP-3.                  
040100*                                 PERIODBEHOV GRÄNS                       
040200*                                 FORECAST LIMIT                          
040300     03 DCS-KVPERIOD-TPAS    PIC S9(3)           COMP-3.                  
040400*                                 ANTAL PERIODER VID BERÄKNING AV         
040500*                                 TRANSFER FÖR PASSIV ARTIKEL             
040600*                                 NBR OF PERIODS WHEN COMPUTING           
040700*                                 TRANSFER OF PASSIVE PART                
040800     03 DCS-KVSKROT-SPAS     PIC S9(3)           COMP-3.                  
040900*                                 MAX GRÄNS FÖR SKROT PASSIVE ART         
041000*                                 MAX LIMIT FOR SCRAPING OF PARTS         
041100     03 DCS-KVVECKOR-RPAS    PIC S9(3)           COMP-3.                  
041200*                                 ANTAL VECKOR KONTROLL OI VID            
041300*                                 RETUR AV PASSIV ARTIKEL                 
041400*                                 NO. WEEKS MONITOR ORDER STATS           
041500*                                 DURING RETURN OF PASSIVE PART           
041600     03 DCS-KVVECKOR-TPAS    PIC S9(3)           COMP-3.                  
041700*                                 ANTAL VECKOR KONTROLL OI VID            
041800*                                 TRANSFER AV PASSIV ARTIKEL              
041900*                                 NO. WEEKS MONITOR ORDER STATS           
042000*                                 DURING TRANSFER OF PASSIVE PART         
042100     03 DCS-SUARTMIN-RPAS    PIC S9(7)           COMP-3.                  
042200*                                 MIN LV FÖR RETUR PASSIV ART             
042300*                                 MIN STOCK VALUE FOR RETURN              
042400*                                 OF PASSIVE PART                         
042500     03 DCS-SUARTMIN-TPAS    PIC S9(7)           COMP-3.                  
042600*                                 MIN LV FÖR TRANSFER AV EN               
042700*                                 PASSIV ARTIKEL                          
042800*                                 MIN STOCK VALUE FOR TRANSFER            
042900*                                 OF PASSIVE PART                         
043000     03 DCS-SUVARLIM-TPAS    PIC S9(7)           COMP-3.                  
043100*                                 VÄRDEGRÄNS PASSIV TRANSFER              
043200*                                 VALUE LIMIT PASSIVE TRANSFER            
043300     03 DCS-TID-TRS          PIC S9              COMP-3.                  
043400*                                 VECKODAG AUTOM.TRANSF,RET,SKROT         
043500*                                 WEEKDAY AUTOM.TRANSF,RET,SCRAP          
043600     03 DCS-TIVV             PIC S9(3)           COMP-3.                  
043700*                                 VECKA  (VV)                             
043800*                                 WEEK   (WW)                             
043900     03 DCS-FLTRLO88-MAN     PIC X.                                       
044000*                                 FLAGGA LO-88 TRANSFER-MÅN OK?           
044100*                                 FLAG TRANSFER-MON TO AREA-88 OK         
044200     03 DCS-FLTRLO88-TIS     PIC X.                                       
044300*                                 FLAGGA LO-88 TRANSFER-TIS OK?           
044400*                                 FLAG TRANSFER-TUE TO AREA-88 OK         
044500     03 DCS-FLTRLO88-ONS     PIC X.                                       
044600*                                 FLAGGA LO-88 TRANSFER-ONS OK?           
044700*                                 FLAG TRANSFER-WEN TO AREA-88 OK         
044800     03 DCS-FLTRLO88-TOR     PIC X.                                       
044900*                                 FLAGGA LO-88 TRANSFER-TOR OK?           
045000*                                 FLAG TRANSFER-THU TO AREA-88 OK         
045100     03 DCS-FLTRLO88-FRE     PIC X.                                       
045200*                                 FLAGGA LO-88 TRANSFER-FRE OK?           
045300*                                 FLAG TRANSFER-FRI TO AREA-88 OK         
045400     03 DCS-KDRT-JUST        PIC S9(3)           COMP-3.                  
045500*                                 REDOVISN.TYP JUSTERINGSORDER            
045600*                                 JUSTIFY ORDER ACCOUNT TYPE              
045700     03 DCS-KDRT-SKROT       PIC S9(3)           COMP-3.                  
045800*                                 REDOVISNINGSTYP SKROTORDER              
045900*                                 SCRAP ORDER ACCOUNT TYPE                
046000     03 DCS-KVVECKOR-BIN     PIC S9(3)           COMP-3.                  
046100*                                 ANTAL VECKOR FÖR LAGERUTTAG             
046200*                                 NO. OF WEEKS FOR DESTOCKING             
046300     03 DCS-KVVECKOR-SPAS    PIC S9(3)           COMP-3.                  
046400*                                 ANTAL VECKOR SKROT PASSIV ART           
046500*                                 WEEKS FOR SCRAP PASSIVE PART            
046600     03 DCS-IDTECKEN-SPAS    PIC X.                                       
046700*                                 TECKEN ( >,=,< ) SKROT ARTIKLAR         
046800*                                 SIGN (>,=,<) FOR SCRAPING PARTS         
046900     03 DCS-PRARTSTD-SPAS    PIC S9(7)V9(2)      COMP-3.                  
047000*                                 PRISGRÄNS FÖR SKROTN.AV PAS-ART         
047100*                                 PRICE LIMIT FOR SCRAPING PARTS          
047200     03 DCS-ADLAGOMR-SPAS    PIC S9(3)           COMP-3.                  
047300*                                 LAGEROMRÅDE SKROTNING PASS-ART          
047400*                                 AREA FOR SCRAP PASSIVE PARTS            
047500     03 DCS-IDPERSON-SPAS    PIC S9(3)           COMP-3.                  
047600*                                 PERSON FÖR SKROT AV PASSIV ART          
047700*                                 STAFF SCRAPING PASSIVE PARTS            
047800     03 DCS-KDPRODSL-SPAS    PIC S9(3)           COMP-3.                  
047900*                                 PRODUKTSLAG SKROT PASSIV ART            
048000*                                 PRODUCT GR SCRAP PASSIVE PART           
048100     03 DCS-IDLEVNR-EMB      PIC X(5).                                    
048200*                                 ALT.LEV PER DC FÖR EMBALLAGE            
048300*                                 ALT.DC SUPPL FOR PACK REPORT            
048400     03 DCS-PRARTSTD-SKRLO98 PIC S9(7)V9(2)      COMP-3.                  
048500*                                 PRISGRÄNS FÖR SKROTNING PÅ LO98         
048600*                                 PRICE LIMIT FOR SCRAPING AREA98         
048700     03 DCS-FLMAINDC         PIC X.                                       
048800*                                 FLAGGA FÖR HUVUD DC PER IDFTG           
048900*                                 FLAG FOR MAIN DC PER COMPANY            
049000     03 DCS-IDPARTNR         PIC X(9).                                    
049100*                                 FINANCIELL KUND                         
049200*                                 FINANCIAL CUST                          
049300     03 DCS-KVDAGAR-POKS     PIC S9(3)           COMP-3.                  
049400*                                 ANT DGR FÖRE RFS ATT NOLL OKSPR         
049500*                                 DAYS BEFORE RFS FOR MOVE OKSPR          
049600     03 DCS-IDLEGSEL         PIC X(4).                                    
049700*                                 FAKTURERANDE FÖRETAG TEX VCCS           
049800*                                 LEGAL SELLER IDENTITY                   
049900     03 DCS-KDTRADP          PIC X(4).                                    
050000*                                 TRADING PARTNER                         
050100*                                 TRADING PARTNER                         
050200     03 DCS-KVDAGAR-PP       PIC S9(3)           COMP-3.                  
050300*                                 DAGAR FÖR REPARAIONSDATUM               
050400*                                 DAYS BEFORE REPAIR DATE                 
050500     03 DCS-KDAKDISP-DAG     PIC 9.                                       
050600*                                 HUR AKS RÄKNAS I DISPONIBELT            
050700*                                 HOW AKS COUNTS IN AVAILABILITY          
050800     03 DCS-KDAKDISP-BULK    PIC 9.                                       
050900*                                 HUR AKS RÄKNAS I DISPONIBELT            
051000*                                 HOW AKS COUNTS IN AVAILABILITY          
051100     03 DCS-FLCLEAR-BULK     PIC X.                                       
051200*                                 BULK ORDERRAD CLEAR FLAGGA              
051300*                                 CLEARING FLAG FOR BULK ORDER            
051400     03 DCS-FLTRACK          PIC X.                                       
051500*                                 FLAG FOR TRACKING-ID FOR A DC           
051600*                                                                         
051700*                                 FLAG FOR TRACKING-ID FOR A DC           
051800*                                                                         
051900     03 DCS-FLSTOREF         PIC X.                                       
052000*                                 STOPP REFILL FLAGGA                     
052100*                                                                         
052200*                                 STOP REFILL FLAG                        
052300*                                                                         
052400     03 DCS-FLSTOFC          PIC X.                                       
052500*                                 STOPP FC OMRÄKNING FLAGGA               
052600*                                                                         
052700*                                 STOP FC RECALC FLAG                     
052800*                                                                         
052900     03 DCS-KVPB-LIM-LF      PIC S9(6)V9(1)      COMP-3.                  
053000*                                 GRÄNS PERIODBEHOV LÅG FREKVENT          
053100*                                 LIMIT FORECAST FOR LOW FREQ             
053200     03 DCS-KVPB-LIM-HF      PIC S9(6)V9(1)      COMP-3.                  
053300*                                 GRÄNS PERIODBEHOV HÖG FREKVENT          
053400*                                 LIMIT FORECAST FOR HIGH FREQ            
053500     03 DCS-KVOT             PIC S9(7)           COMP-3.                  
053600*                                 ANTAL ORDERTRÄFF                        
053700*                                 NO OF ORDERHITS                         
053800     03 DCS-FLLPO            PIC X.                                       
053900*                                 DC MED LOKALANSKAFFNING REFILL          
054000*                                 DC WITH LOCAL PURCHASING REFILL         
054100     03 DCS-IDSKYLT-DB       PIC X(3).                                    
054200      88 DCS-UNICODE-IDSKYLT VALUE 'CZ '                                  
054300                             'GR '                                        
054400                             'H  '                                        
054500                             'IR '                                        
054600                             'J  '                                        
054700                             'KOR'                                        
054800                             'PL '                                        
054900                             'RC '                                        
055000                             'RCN'                                        
055100                             'RO '                                        
055200                             'RUS'                                        
055300                             'T  '                                        
055400                             'TR '                                        
055500                             'YU '.                                       
055600*                                 NATIONALITETSTECKEN                     
055700*                                 SPRÅKIDENTIFIKATION                     
055800*                                 NATIONALITY SIGN                        
055900*                                 LANGUAGE IDENTIFIER                     
056000     03 DCS-FLARTADD         PIC X.                                       
056100*                                 VISAR OM ART SKA ISRT PÅ K711           
056200*                                 SHOWS IF PART MUST ISRT ON K711         
056300     03 DCS-KVVECKOR-FTL     PIC S9(3)           COMP-3.                  
056400*                                 MAX VECKOR FÖR LÅGA LEDTIDER            
056500*                                 MAX NO WEEKS FOR LOW LEADTIME           
056600     03 DCS-KVVECKOR-FTM     PIC S9(3)           COMP-3.                  
056700*                                 MAX VECKOR FÖR MEDEL LEDTIDER           
056800*                                 MAX WEEKS FOR MED LEADTIME              
056900     03 DCS-KVVECKOR-FTH     PIC S9(3)           COMP-3.                  
057000*                                 MAX VECKOR FÖR HÖGA LEDTIDER            
057100*                                 MAX WEEKS FOR HIGH LEADTIME             
057200     03 DCS-KVOT-RULL12HF    PIC S9(7)           COMP-3.                  
057300*                                 ANTAL ORDERINGÅNG HÖGFREKVENTA          
057400*                                 NBR ORDER HITS FOR HIGH FREQ            
057500     03 DCS-IDRT             PIC X(3).                                    
057600*                                 RETURTERMINAL                           
057700*                                 RETURN TERMINAL                         
057800     03 DCS-FILLER           PIC X(41).                                   
057900*** END OF VILMAII-COPY LENGTH= 657 BYTES                                 
