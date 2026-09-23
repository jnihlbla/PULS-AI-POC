000100 01  CLDC-W411CLDC.                                                       
000200*                                 LÄNKAREA TILL W411CLDC -                
000300*                                 HÄMTAR WDB601-SEGMENTEN FÖR             
000400*                                 DC11 OCH SAMTLIGA CLEARING-DC           
000500     03 CLDC-IDDC-CLEAR-GRP.                                              
000600*                                 GRUPP AV IDDC-CLEAR                     
000700        05 CLDC-IDDC-CLEAR   OCCURS 99 TIMES                              
000800                             PIC X(2).                                    
000900*                                 LAGERPRIORITERING VID                   
001000*                                 ORDERCLEARING                           
001100     03 CLDC-WDB601          OCCURS 99 TIMES.                             
001200*                                 DC STYRREGISTER                         
001300*                                                                         
001400*                                 FYSISK NYCKEL: IDDC                     
001500*                                                                         
001600        05 CLDC-IDDC         PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800        05 CLDC-BEGMT.                                                    
001900*                                 GODSMOTTAGARNAMN                        
002000           07 CLDC-BEGMT-RAD1                                             
002100                             PIC X(35).                                   
002200*                                 GODSMOTTAGARNAMN RAD 1                  
002300           07 CLDC-BEGMT-RAD2                                             
002400                             PIC X(35).                                   
002500*                                 GODSMOTTAGARNAMN RAD 2                  
002600        05 CLDC-ADGMT.                                                    
002700*                                 GODSMOTTAGARADRESS                      
002800           07 CLDC-ADGMT-GATA                                             
002900                             PIC X(35).                                   
003000*                                 GODSMOTTAGARADRESS GATA                 
003100           07 CLDC-ADGMT-PADR                                             
003200                             PIC X(35).                                   
003300*                                 GODSMOTTAGARADRESS POSTADRESS           
003400           07 CLDC-ADPOST-PNRORT REDEFINES CLDC-ADGMT-PADR.               
003500*                                 POSTNUMMER + ORT                        
003600              09 CLDC-ADPOSTNR                                            
003700                             PIC X(10).                                   
003800*                                 POSTNUMMER I ADRESS                     
003900              09 CLDC-ADCITY PIC X(25).                                   
004000*                                 BENÄMNING PÅ STAD                       
004100           07 CLDC-ADPOST-ORTPNR REDEFINES CLDC-ADGMT-PADR.               
004200*                                 ORT + POSTNUMMER                        
004300              09 CLDC-ADCITY PIC X(25).                                   
004400*                                 BENÄMNING PÅ STAD                       
004500              09 CLDC-ADPOSTNR                                            
004600                             PIC X(10).                                   
004700*                                 POSTNUMMER I ADRESS                     
004800           07 CLDC-ADGMT-LAND                                             
004900                             PIC X(35).                                   
005000*                                 GODSMOTTAGARADRESS LAND                 
005100        05 CLDC-FLDCRET      PIC X.                                       
005200*                                 GODKÄND FÖR RETUR                       
005300        05 CLDC-FLEXCP1-PRIO PIC X.                                       
005400*                                 UNDANTAGSREGEL I PRIOBERÄKNING          
005500        05 CLDC-FLTRANS-PAS  PIC X.                                       
005600*                                 TRANSFER PASSIV ART OK?                 
005700        05 CLDC-FLTRANS-ERS  PIC X.                                       
005800*                                 TRANSFER OK OM VISS ERSÄTTNING?         
005900        05 CLDC-FLRETUR-PAS  PIC X.                                       
006000*                                 RETUR OK FÖR PASSIV ART?                
006100        05 CLDC-FLSKROT-PAS  PIC X.                                       
006200*                                 OK ATT SKROTA PASSIV ART?               
006300        05 CLDC-FLEXCP1-REFBEO                                            
006400                             PIC X.                                       
006500*                                 UND.REGEL AKT. REFILLBEORDRING          
006600        05 CLDC-FLEXCP2-REFBEO                                            
006700                             PIC X.                                       
006800*                                 UND.REGEL AKT. REFILLBEORDRING          
006900        05 CLDC-FLEXCP3-REFBEO                                            
007000                             PIC X.                                       
007100*                                 UND.REGEL AKT. REFILLBEORDRING          
007200        05 CLDC-FLEXCP4-REFBEO                                            
007300                             PIC X.                                       
007400*                                 UND.REGEL AKT. REFILLBEORDRING          
007500        05 CLDC-FLEXCP1-REFBER                                            
007600                             PIC X.                                       
007700*                                 UND.REGEL I REFILLBERÄKNINGEN           
007800        05 CLDC-FLEXCP2-REFBER                                            
007900                             PIC X.                                       
008000*                                 UND.REGEL I REFILLBERÄKNINGEN           
008100        05 CLDC-FLINLHIST    PIC X.                                       
008200*                                 OM INLEVERANSHIST. SKA SKAPAS           
008300        05 CLDC-FLFSEDEL     PIC X.                                       
008400*                                 FÖLJESEDELS FLAGGA                      
008500*                                 J = FÖLJESEDEL SKALL SKAPAS             
008600*                                 N = FÖLJESEDEL SKALL EJ SKAPAS          
008700        05 CLDC-FLINLREP     PIC X.                                       
008800*                                 FLAGGA AVVIKELSERAPPORTER               
008900        05 CLDC-FLBINNUT     PIC X.                                       
009000*                                 BINNING RAPPORT UTSKRIFTFLAGGA          
009100        05 CLDC-FLPRISSPR    PIC X.                                       
009200*                                 PRISSPÄRRSFLAGGA                        
009300        05 CLDC-FLSAMPAK     PIC X.                                       
009400*                                 SAMPACKNING AV KOLLI                    
009500        05 CLDC-FLSEASBER    PIC X.                                       
009600*                                 SÄSONGSBERÄKNGSFLAGGA                   
009700        05 CLDC-FLTYP6JU     PIC X.                                       
009800*                                 TYPE-6 ADJUSTMENT                       
009900        05 CLDC-FLRSI        PIC X.                                       
010000*                                 FLAGGA SKEPPNINGSINFO TILL VIPS         
010100        05 CLDC-FLWEBDC      PIC X.                                       
010200*                                 DC MED WEB GRÄNSSNITT                   
010300        05 CLDC-IDDISTR-REFILL                                            
010400                             PIC S9(5)           COMP-3.                  
010500*                                 REFILL DISTRIKT                         
010600        05 CLDC-IDDISTR-RETUR                                             
010700                             PIC S9(5)           COMP-3.                  
010800*                                 RETUR DISTRIKT                          
010900        05 CLDC-IDDISTR-QRETUR                                            
011000                             PIC S9(5)           COMP-3.                  
011100*                                 KAVLITET RETUR DISTRIKT                 
011200        05 CLDC-IDDISTR-SKROT                                             
011300                             PIC S9(5)           COMP-3.                  
011400*                                 SKROT DISTRIKT                          
011500        05 CLDC-IDDISTR-QSKROT                                            
011600                             PIC S9(5)           COMP-3.                  
011700*                                 KVALITET SKROT DISTRIKT                 
011800        05 CLDC-IDDISTR-RSKROT                                            
011900                             PIC S9(5)           COMP-3.                  
012000*                                 SKROT DISTRIKT FÖR RETURER              
012100        05 CLDC-IDKUNDNR-BPS PIC S9(7)           COMP-3.                  
012200*                                 KUNDNUMMER FÖR BYPASSORDER              
012300        05 CLDC-IDKUNDNR-SBPS                                             
012400                             PIC S9(7)           COMP-3.                  
012500*                                 KUND FÖR SNABB BYPASSORDER              
012600        05 CLDC-IDKUNDNR-RETUR                                            
012700                             PIC S9(7)           COMP-3.                  
012800*                                 KUNDNUMMER FÖR RETUR                    
012900        05 CLDC-IDKUNDNR-QRETUR                                           
013000                             PIC S9(7)           COMP-3.                  
013100*                                 KUNDNUMMER FÖR KVALITETSRETUR           
013200        05 CLDC-IDKUNDNR-SRETUR                                           
013300                             PIC S9(7)           COMP-3.                  
013400*                                 KUNDNUMMER FÖR SNABBRETUR               
013500        05 CLDC-IDKUNDNR-SQRET                                            
013600                             PIC S9(7)           COMP-3.                  
013700*                                 KUNDNUMMER FÖR SNABB KVAL.RETUR         
013800        05 CLDC-IDKUNDNR-TRETUR                                           
013900                             PIC S9(7)           COMP-3.                  
014000*                                 KUNDNUMMER FÖR TOTALRETUR               
014100        05 CLDC-IDKUNDNR-SKROT                                            
014200                             PIC S9(7)           COMP-3.                  
014300*                                 KUNDNUMMER FÖR SKROT                    
014400        05 CLDC-IDKUNDNR-QSKROT                                           
014500                             PIC S9(7)           COMP-3.                  
014600*                                 KUNDNUMMER FÖR KVALITET SKROT           
014700        05 CLDC-IDKUNDNR-RSKROT                                           
014800                             PIC S9(7)           COMP-3.                  
014900*                                 KUNDNUMMER FÖR SKROT AV RETUR           
015000        05 CLDC-IDKUNDNR-SORD                                             
015100                             PIC S9(7)           COMP-3.                  
015200*                                 KUNDNUMMER FÖR SNABBORDER               
015300        05 CLDC-IDLANDX2     PIC X(2).                                    
015400         88 CLDC-EMIRATES    VALUE 'AE'.                                  
015500         88 CLDC-AUSTRALIA   VALUE 'AU'.                                  
015600         88 CLDC-AUSTRIA     VALUE 'AT'.                                  
015700         88 CLDC-BELGIUM     VALUE 'BE'.                                  
015800         88 CLDC-BRASIL      VALUE 'BR'.                                  
015900         88 CLDC-CANADA      VALUE 'CA'.                                  
016000         88 CLDC-SWIZERLAND  VALUE 'CH'.                                  
016100         88 CLDC-CHINA       VALUE 'CN'.                                  
016200         88 CLDC-GERMANY     VALUE 'DE'.                                  
016300         88 CLDC-SPAIN       VALUE 'ES'.                                  
016400         88 CLDC-FINLAND     VALUE 'FI'.                                  
016500         88 CLDC-FRANCE      VALUE 'FR'.                                  
016600         88 CLDC-ENGLAND     VALUE 'GB'.                                  
016700         88 CLDC-HUNGARY     VALUE 'HU'.                                  
016800         88 CLDC-INDIA       VALUE 'IN'.                                  
016900         88 CLDC-ITALY       VALUE 'IT'.                                  
017000         88 CLDC-JAPAN       VALUE 'JP'.                                  
017100         88 CLDC-KOREA       VALUE 'KR'.                                  
017200         88 CLDC-MALAYSIA    VALUE 'MY'.                                  
017300         88 CLDC-MAROCKO     VALUE 'MA'.                                  
017400         88 CLDC-MEXICO      VALUE 'MX'.                                  
017500         88 CLDC-HOLLAND     VALUE 'NL'.                                  
017600         88 CLDC-NORWAY      VALUE 'NO'.                                  
017700         88 CLDC-POLAND      VALUE 'PL'.                                  
017800         88 CLDC-RUSSIA      VALUE 'RU'.                                  
017900         88 CLDC-SWEDEN      VALUE 'SE'.                                  
018000         88 CLDC-THAILAND    VALUE 'TH'.                                  
018100         88 CLDC-TURKEY      VALUE 'TR'.                                  
018200         88 CLDC-TAIWAN      VALUE 'TW'.                                  
018300         88 CLDC-USA         VALUE 'US'.                                  
018400         88 CLDC-SOUTH-AFRICA                                             
018500                             VALUE 'ZA'.                                  
018600         88 CLDC-LAND-NON-VCC-OWNED                                       
018700                             VALUE 'AE'                                   
018800                             'BR'                                         
018900                             'CN'                                         
019000                             'IN'                                         
019100                             'KR'                                         
019200                             'MY'                                         
019300                             'MX'                                         
019400                             'RU'                                         
019500                             'TH'                                         
019600                             'TR'                                         
019700                             'TW'                                         
019800                             'ZA'.                                        
019900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
020000        05 CLDC-IDLISTNR     PIC 9(3).                                    
020100*                                 LISTNUMMER                              
020200        05 CLDC-IDPRTLST-INL PIC X(8).                                    
020300*                                 LOGISK PRINTER+LISTA IDENTITET          
020400        05 CLDC-IDPRTLST-INLA                                             
020500                             PIC X(8).                                    
020600*                                 LOGISK PRINTER+LISTA IDENTITET          
020700        05 CLDC-IDPRTLST-INVA                                             
020800                             PIC X(8).                                    
020900*                                 PRINTER FÖR INV.ANMODAN                 
021000        05 CLDC-IDPRTLST-INVAB                                            
021100                             PIC X(8).                                    
021200*                                 PRINTER FÖR INV.ANMODAN/BYTES           
021300        05 CLDC-IDTIDZON     PIC 9(2).                                    
021400*                                 TIDZONER PÅ JORDEN.                     
021500        05 CLDC-IDVAT        PIC X(17).                                   
021600*                                 MOMSREGISTRERINGSNUMMER                 
021700        05 CLDC-KDDC         PIC X(2).                                    
021800         88 CLDC-CDC         VALUE 'C '.                                  
021900         88 CLDC-DDC         VALUE 'D '.                                  
022000         88 CLDC-NDC         VALUE 'NA'                                   
022100                             'NC'                                         
022200                             'NP'                                         
022300                             'NX'.                                        
022400         88 CLDC-NDC-NA      VALUE 'NA'.                                  
022500         88 CLDC-NDC-PF      VALUE 'NP'.                                  
022600         88 CLDC-NDC-CN      VALUE 'NC'.                                  
022700         88 CLDC-NDC-OTHERS  VALUE 'NX'.                                  
022800         88 CLDC-SDC         VALUE 'S '.                                  
022900         88 CLDC-CDC-TR      VALUE 'TR'.                                  
023000*                                 TYP AV DISTR. LAGER                     
023100        05 CLDC-KDDCSTYR-BUY PIC S9(5)           COMP-3.                  
023200*                                 REGELVERK VID BUYERTILLDELNING          
023300        05 CLDC-KDDCSTYR-KUND                                             
023400                             PIC S9(5)           COMP-3.                  
023500*                                 REGELVERK FÖR TILLD. AV KUNDNR          
023600        05 CLDC-KDDCSTYR-REFTAB                                           
023700                             PIC S9(5)           COMP-3.                  
023800*                                 REGELVERK TILLD AV REFILLTABELL         
023900        05 CLDC-KDFRAKT-BPS  PIC S9(3)           COMP-3.                  
024000*                                 FRAKTSÄTT DC TILL KUND                  
024100        05 CLDC-KDFRAKT-SBPS PIC S9(3)           COMP-3.                  
024200*                                 FRAKTSÄTT DC TILL KUND                  
024300        05 CLDC-KDPORDL      PIC X.                                       
024400*                                 PACKAD ORDERLISTA MÖJLIG                
024500        05 CLDC-KDSKRMET     PIC S9              COMP-3.                  
024600*                                 SKROTMETOD                              
024700        05 CLDC-KDVALISO     PIC X(3).                                    
024800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
024900        05 CLDC-FLINVACS     PIC X.                                       
025000*                                 ACS-INVENTERING?                        
025100        05 CLDC-IDFTG        PIC 9(2).                                    
025200         88 CLDC-FTG-US      VALUE 53.                                    
025300         88 CLDC-FTG-CA      VALUE 54.                                    
025400         88 CLDC-FTG-PV      VALUE 57.                                    
025500         88 CLDC-FTG-CN      VALUE 60.                                    
025600         88 CLDC-FTG-IN      VALUE 61.                                    
025700         88 CLDC-FTG-TH      VALUE 63.                                    
025800         88 CLDC-FTG-TW      VALUE 64.                                    
025900         88 CLDC-FTG-KR      VALUE 65.                                    
026000         88 CLDC-FTG-MY      VALUE 66.                                    
026100         88 CLDC-FTG-RU      VALUE 81.                                    
026200         88 CLDC-FTG-BR      VALUE 82.                                    
026300         88 CLDC-FTG-MX      VALUE 83.                                    
026400         88 CLDC-FTG-ZA      VALUE 85.                                    
026500         88 CLDC-FTG-TR      VALUE 86.                                    
026600         88 CLDC-FTG-AE      VALUE 87.                                    
026700*                                 FÖRETAGSID EKONOM REDOVISNING           
026800        05 CLDC-KVINVAUT     PIC S9(9)           COMP-3.                  
026900*                                 GRÄNS AUTOMATISK INVENTERING            
027000        05 CLDC-SUINVGRANS   PIC S9(9)           COMP-3.                  
027100*                                 GRÄNS INVENTERING DESCREPENCY           
027200        05 CLDC-KDRT-MIX     PIC S9(3)           COMP-3.                  
027300*                                 REDOVISN. JUSTERING/BLAND. ART          
027400        05 CLDC-REQXBRYT     PIC S9(2)           COMP-3.                  
027500*                                 KVANTBRYTNING                           
027600        05 CLDC-REWILSON     PIC S9V9(2)         COMP-3.                  
027700*                                 PROCENTREGEL FÖR WILSONFORMEL           
027800        05 CLDC-IDLEVNR-DC   PIC X(5).                                    
027900*                                 DC LEVERANTÖR                           
028000        05 CLDC-FLKNDVAL     PIC X.                                       
028100*                                 STYRNING PÅ KUND ELLER DISTRIKT         
028200        05 CLDC-TIHHMM-START PIC S9(5)           COMP-3.                  
028300*                                 KLOCKSLAG (TIMMAR/MIN.) START           
028400        05 CLDC-TIHHMM-READY PIC S9(5)           COMP-3.                  
028500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
028600*                                 AVSLUTNING                              
028700        05 CLDC-KDFAKTDC     PIC X.                                       
028800*                                 FAKT. STATISTIK URVAL PER DC            
028900        05 CLDC-FLARTDC      PIC X.                                       
029000*                                 INDIKERAR OM ART SKA VARA PÅ DC         
029100        05 CLDC-IDDC-REF     PIC X(2).                                    
029200*                                 SÄNDANDE LAGER FÖR REFILL               
029300        05 CLDC-IDPERSON-REM PIC S9(3)           COMP-3.                  
029400*                                 PERSONKOD REMISS                        
029500        05 CLDC-IDDISTR-JUST PIC S9(5)           COMP-3.                  
029600*                                 DISTRIKT FÖR JUSTERINGSORDER            
029700        05 CLDC-IDDISTR-MIX  PIC S9(5)           COMP-3.                  
029800*                                 DISTRIKT JUSTERING/BLAND.ART.           
029900        05 CLDC-IDKUNDNR-JUST                                             
030000                             PIC S9(7)           COMP-3.                  
030100*                                 KUND FÖR JUSTERINGSORDER                
030200        05 CLDC-IDKUNDNR-MIX PIC S9(7)           COMP-3.                  
030300*                                 KUND JUSTERING/BLAND.ART.               
030400        05 CLDC-IDKST-JUST   PIC X(10).                                   
030500*                                 KOSTNADSST. FÖR JUSTERINGSORDER         
030600        05 CLDC-IDKST-MIX    PIC X(10).                                   
030700*                                 KOSTNADSST. BLANDADE ARTIKLAR           
030800        05 CLDC-IDKST-SKROT  PIC X(10).                                   
030900*                                 KOSTNADSST. FÖR SKROTORDER              
031000        05 CLDC-IDKONTO-JUST PIC S9(11)          COMP-3.                  
031100*                                 KONTO FÖR JUSTERINGSORDER               
031200        05 CLDC-IDKONTO-MIX  PIC S9(11)          COMP-3.                  
031300*                                 KONTO FÖR JUSTERING/BLAND.ART.          
031400        05 CLDC-IDKONTO-SKROT                                             
031500                             PIC S9(11)          COMP-3.                  
031600*                                 KONTO FÖR SKROTORDER                    
031700        05 CLDC-IDANALYS-JUST                                             
031800                             PIC X(12).                                   
031900*                                 ANALYSNR FÖR JUSTERINGSORDER            
032000        05 CLDC-IDANALYS-MIX PIC X(12).                                   
032100*                                 ANALYSNR JUSTERING/BLAND.ART.           
032200        05 CLDC-IDANALYS-SKROT                                            
032300                             PIC X(12).                                   
032400*                                 ANALYSNR FÖR SKROTORDER                 
032500        05 CLDC-TID-RETOS    PIC S9              COMP-3.                  
032600*                                 VECKODAG FÖR AUTOMATRETUR MÅN=1         
032700        05 CLDC-TID-RET98    PIC S9              COMP-3.                  
032800*                                 VECKODAG FÖR AUTOM.RETUR LO 98          
032900        05 CLDC-FLOVRLAGBER  PIC X.                                       
033000*                                 SDC ÖVERLAGER INKL CDC                  
033100        05 CLDC-KVDAGAR-CROSS                                             
033200                             PIC S9              COMP-3.                  
033300*                                 ANTAL DAGAR FÖRDR. CROSS-DOCK           
033400        05 CLDC-TIUPPDAT     PIC 9(6).                                    
033500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
033600        05 CLDC-IDUSER       PIC X(8).                                    
033700*                                 ANVÄNDARENS SÄKERHETS ID                
033800        05 CLDC-IDDC-TPAS-1  PIC X(2).                                    
033900*                                 DC 1 FÖR TRANSFER AV PASS ART           
034000        05 CLDC-IDDC-TPAS-2  PIC X(2).                                    
034100*                                 DC 2 FÖR TRANSFER AV PASS ART           
034200        05 CLDC-IDDC-TPAS-3  PIC X(2).                                    
034300*                                 DC 3 FÖR TRANSFER AV PASS ART           
034400        05 CLDC-KVPB-LIM     PIC S9(6)V9(1)      COMP-3.                  
034500*                                 PERIODBEHOV GRÄNS                       
034600        05 CLDC-KVPERIOD-TPAS                                             
034700                             PIC S9(3)           COMP-3.                  
034800*                                 ANTAL PERIODER VID BERÄKNING AV         
034900*                                 TRANSFER FÖR PASSIV ARTIKEL             
035000        05 CLDC-KVSKROT-SPAS PIC S9(3)           COMP-3.                  
035100*                                 MAX GRÄNS FÖR SKROT PASSIVE ART         
035200        05 CLDC-KVVECKOR-RPAS                                             
035300                             PIC S9(3)           COMP-3.                  
035400*                                 ANTAL VECKOR KONTROLL OI VID            
035500*                                 RETUR AV PASSIV ARTIKEL                 
035600        05 CLDC-KVVECKOR-TPAS                                             
035700                             PIC S9(3)           COMP-3.                  
035800*                                 ANTAL VECKOR KONTROLL OI VID            
035900*                                 TRANSFER AV PASSIV ARTIKEL              
036000        05 CLDC-SUARTMIN-RPAS                                             
036100                             PIC S9(7)           COMP-3.                  
036200*                                 MIN LV FÖR RETUR PASSIV ART             
036300        05 CLDC-SUARTMIN-TPAS                                             
036400                             PIC S9(7)           COMP-3.                  
036500*                                 MIN LV FÖR TRANSFER AV EN               
036600*                                 PASSIV ARTIKEL                          
036700        05 CLDC-SUVARLIM-TPAS                                             
036800                             PIC S9(7)           COMP-3.                  
036900*                                 VÄRDEGRÄNS PASSIV TRANSFER              
037000        05 CLDC-TID-TRS      PIC S9              COMP-3.                  
037100*                                 VECKODAG AUTOM.TRANSF,RET,SKROT         
037200        05 CLDC-TIVV         PIC S9(3)           COMP-3.                  
037300*                                 VECKA  (VV)                             
037400        05 CLDC-FLTRLO88-MAN PIC X.                                       
037500*                                 FLAGGA LO-88 TRANSFER-MÅN OK?           
037600        05 CLDC-FLTRLO88-TIS PIC X.                                       
037700*                                 FLAGGA LO-88 TRANSFER-TIS OK?           
037800        05 CLDC-FLTRLO88-ONS PIC X.                                       
037900*                                 FLAGGA LO-88 TRANSFER-ONS OK?           
038000        05 CLDC-FLTRLO88-TOR PIC X.                                       
038100*                                 FLAGGA LO-88 TRANSFER-TOR OK?           
038200        05 CLDC-FLTRLO88-FRE PIC X.                                       
038300*                                 FLAGGA LO-88 TRANSFER-FRE OK?           
038400        05 CLDC-KDRT-JUST    PIC S9(3)           COMP-3.                  
038500*                                 REDOVISN.TYP JUSTERINGSORDER            
038600        05 CLDC-KDRT-SKROT   PIC S9(3)           COMP-3.                  
038700*                                 REDOVISNINGSTYP SKROTORDER              
038800        05 CLDC-KVVECKOR-BIN PIC S9(3)           COMP-3.                  
038900*                                 ANTAL VECKOR FÖR LAGERUTTAG             
039000        05 CLDC-KVVECKOR-SPAS                                             
039100                             PIC S9(3)           COMP-3.                  
039200*                                 ANTAL VECKOR SKROT PASSIV ART           
039300        05 CLDC-IDTECKEN-SPAS                                             
039400                             PIC X.                                       
039500*                                 TECKEN ( >,=,< ) SKROT ARTIKLAR         
039600        05 CLDC-PRARTSTD-SPAS                                             
039700                             PIC S9(7)V9(2)      COMP-3.                  
039800*                                 PRISGRÄNS FÖR SKROTN.AV PAS-ART         
039900        05 CLDC-ADLAGOMR-SPAS                                             
040000                             PIC S9(3)           COMP-3.                  
040100*                                 LAGEROMRÅDE SKROTNING PASS-ART          
040200        05 CLDC-IDPERSON-SPAS                                             
040300                             PIC S9(3)           COMP-3.                  
040400*                                 PERSON FÖR SKROT AV PASSIV ART          
040500        05 CLDC-KDPRODSL-SPAS                                             
040600                             PIC S9(3)           COMP-3.                  
040700*                                 PRODUKTSLAG SKROT PASSIV ART            
040800        05 CLDC-IDLEVNR-EMB  PIC X(5).                                    
040900*                                 ALT.LEV PER DC FÖR EMBALLAGE            
041000        05 CLDC-PRARTSTD-SKRLO98                                          
041100                             PIC S9(7)V9(2)      COMP-3.                  
041200*                                 PRISGRÄNS FÖR SKROTNING PÅ LO98         
041300        05 CLDC-FLMAINDC     PIC X.                                       
041400*                                 FLAGGA FÖR HUVUD DC PER IDFTG           
041500        05 CLDC-IDPARTNR     PIC X(9).                                    
041600*                                 FINANCIELL KUND                         
041700        05 CLDC-KVDAGAR-POKS PIC S9(3)           COMP-3.                  
041800*                                 ANT DGR FÖRE RFS ATT NOLL OKSPR         
041900        05 CLDC-IDLEGSEL     PIC X(4).                                    
042000*                                 FAKTURERANDE FÖRETAG TEX VCCS           
042100        05 CLDC-KDTRADP      PIC X(4).                                    
042200*                                 TRADING PARTNER                         
042300        05 CLDC-KVDAGAR-PP   PIC S9(3)           COMP-3.                  
042400*                                 DAGAR FÖR REPARAIONSDATUM               
042500        05 CLDC-KDAKDISP-DAG PIC 9.                                       
042600*                                 HUR AKS RÄKNAS I DISPONIBELT            
042700        05 CLDC-KDAKDISP-BULK                                             
042800                             PIC 9.                                       
042900*                                 HUR AKS RÄKNAS I DISPONIBELT            
043000        05 CLDC-FLCLEAR-BULK PIC X.                                       
043100*                                 BULK ORDERRAD CLEAR FLAGGA              
043200        05 CLDC-FLTRACK      PIC X.                                       
043300*                                 FLAG FOR TRACKING-ID FOR A DC           
043400*                                                                         
043500        05 CLDC-FLSTOREF     PIC X.                                       
043600*                                 STOPP REFILL FLAGGA                     
043700*                                                                         
043800        05 CLDC-FLSTOFC      PIC X.                                       
043900*                                 STOPP FC OMRÄKNING FLAGGA               
044000*                                                                         
044100        05 CLDC-KVPB-LIM-LF  PIC S9(6)V9(1)      COMP-3.                  
044200*                                 GRÄNS PERIODBEHOV LÅG FREKVENT          
044300        05 CLDC-KVPB-LIM-HF  PIC S9(6)V9(1)      COMP-3.                  
044400*                                 GRÄNS PERIODBEHOV HÖG FREKVENT          
044500        05 CLDC-KVOT         PIC S9(7)           COMP-3.                  
044600*                                 ANTAL ORDERTRÄFF                        
044700        05 CLDC-FLLPO        PIC X.                                       
044800*                                 DC MED LOKALANSKAFFNING REFILL          
044900        05 CLDC-IDSKYLT-DB   PIC X(3).                                    
045000         88 CLDC-UNICODE-IDSKYLT                                          
045100                             VALUE 'CZ '                                  
045200                             'GR '                                        
045300                             'H  '                                        
045400                             'IR '                                        
045500                             'J  '                                        
045600                             'KOR'                                        
045700                             'PL '                                        
045800                             'RC '                                        
045900                             'RCN'                                        
046000                             'RO '                                        
046100                             'RUS'                                        
046200                             'T  '                                        
046300                             'TR '                                        
046400                             'YU '.                                       
046500*                                 NATIONALITETSTECKEN                     
046600*                                 SPRÅKIDENTIFIKATION                     
046700        05 CLDC-FLARTADD     PIC X.                                       
046800*                                 VISAR OM ART SKA ISRT PÅ K711           
046900        05 CLDC-FILLER       PIC X(54).                                   
047000*** END OF VILMAII-COPY LENGTH= 65241 BYTES                               
