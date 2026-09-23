000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL016500.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   APRIL 2005.                                              
000600                                                                          
000700     REMARKS.                                                             
000800* WL016500 PROGRAM IS A REPLICA OF W4023100 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*    NAMN:       CARPARTS.LDC.SPECORDERHEAD                               
001200*                                                                         
001300*                                                                         
001400*    FUNKTION.                                                            
001500*        PROGRAMMET HANTERAR UPPLÄGGNING AV SPECIAL-ORDERHUVUD I          
001600*        ORDERKÖN. REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA            
001700*        HÄMTAS FRÅN KUNDREGISTRET.                                       
001800*        EFTER UPPLÄGGNING AV GODKÄNT ORDERHUVUD SKER UTHOPP TILL         
001900*        REGISTRERING AV ORDERRADER BILD - WL016600                       
002000*                                                                         
002100*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002200*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)  ORDERHUVUD                  
002300*                   LÄSER      WLBETC (WDB1)  KUNDREGISTER                
002400*                   LÄSER      WLGMTA (WDB2)  KUNDREGISTER                
002500*                   LÄSER      WLGMTB (WDB3)  KUNDREGISTER                
002600*                   LÄSER      WLGMTC (WDB5)  KUNDREGISTER                
002700*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)  ORDERNUMMERREGISTER         
002800*        PROGRAMMET LÄSER             (WDM2)  KAMPANJREGISTER             
002900*                                                                         
003000*    INDATA.                                                              
003100*        TRANSAKTION: WL0165U                                             
003200*        REQUEST:     WL0165I1                                            
003300*                                                                         
003400*    UTDATA.                                                              
003500*        RESPONSE:    WL0165O1                                            
003600*                                                                         
003700*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
003800*                                                                         
003900     EJECT                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)   VALUE 'WL016500'.             
004700 77  JA                          PIC X(1)   VALUE 'J'.                    
004800 77  YES                         PIC X(1)   VALUE 'Y'.                    
004900 77  NEJ                         PIC X(1)   VALUE 'N'.                    
005000                                                                          
005100*--  VÄRDEN FÖR KDBEH                                                     
005200 77  PACKING-ADJUSTMENT          PIC X(1)   VALUE 'P'.                    
005300 77  MIX-STOCK                   PIC X(1)   VALUE 'M'.                    
005400 77  SCRAP                       PIC X(1)   VALUE 'S'.                    
005500 77  CORE-SCRAP                  PIC X(1)   VALUE 'O'.                    
005600 77  LOCAL-REMAN                 PIC X(1)   VALUE 'L'.                    
005700 77  EXCHANGE-CONV               PIC X(1)   VALUE 'C'.                    
005800                                                                          
005900 01  ALL-PLUS.                                                            
006000     03 FILLER                   PIC X(30)  VALUE                         
006100        '++++++++++++++++++++++++++++++'.                                 
006200                                                                          
006300* -- HÅRDKODAT VÄRDE SOM BORDE HA FUNNITS PÅ WDB6.                        
006400* -- DET GÄLLER ALLA VARIANTERNA AV ORDER SOM GENERERAS                   
006500 77  DCS-KDORDKL-ALLA            PIC 9(1)  VALUE  1.                      
006600                                                                          
006700* -- TEMPORÄRT HÅRDKODADE VÄRDEN FÖR AUSTRALIEN                           
006800*    -- CORE SCRAPPING                                                    
006900 77  DCS-IDDISTR-OSKROT          PIC 9(4)  VALUE  82.                     
007000 77  DCS-IDKUNDNR-OSKROT         PIC 9(6)  VALUE  0.                      
007100 77  DCS-IDKONTO-OSKROT          PIC 9(10) VALUE  482311.                 
007200 77  DCS-IDANALYS-OSKROT         PIC X(12) VALUE  '158600000316'.         
007300 77  DCS-IDKST-OSKROT            PIC X(10) VALUE  SPACE.                  
007400                                                                          
007500*    -- LOCAL REMANUFACUTER                                               
007600*    -- DISTRIKT MATAS ALLTID IN                                          
007800 77  DCS-IDKUNDNR-LRENOV         PIC 9(6)  VALUE  0.                      
007900 77  DCS-IDKONTO-LRENOV          PIC 9(10) VALUE  483105.                 
008000 77  DCS-IDANALYS-LRENOV         PIC X(12) VALUE  '158600000810'.         
008100 77  DCS-IDKST-LRENOV            PIC X(10) VALUE  SPACE.                  
008200                                                                          
008300*    -- EXCHANGE CONVERSION                                               
008400 77  DCS-IDDISTR-BKONV           PIC 9(4)  VALUE  82.                     
008500 77  DCS-IDKUNDNR-BKONV          PIC 9(6)  VALUE  0.                      
008600 77  DCS-IDKONTO-BKONV           PIC 9(10) VALUE  483104.                 
008700 77  DCS-IDANALYS-BKONV          PIC X(12) VALUE  '158600000798'.         
008800 77  DCS-IDKST-BKONV             PIC X(10) VALUE  SPACE.                  
008900                                                                          
009000* -- TEMPORÄRT HÅRDKODADE VÄRDEN FÖR CANADA                               
009100*    -- CORE SCRAPPING                                                    
009200 77  DCS-IDDISTR-OSKROT-NA       PIC 9(4)  VALUE  8480.                   
009400 77  DCS-KDFRAKT-OSKROT-NA       PIC 9(2)  VALUE  11.                     
009500                                                                          
009600*    -- LOCAL REMANUFACUTER                                               
009700*    -- DISTRIKT MATAS ALLTID IN                                          
009900 77  DCS-IDKUNDNR-LRENOV-NA      PIC 9(6)  VALUE  0.                      
010000 77  DCS-KDFRAKT-LRENOV-NA       PIC 9(2)  VALUE  11.                     
010100                                                                          
011400* -- TEMPORÄRT HÅRDKODADE VÄRDEN FÖR KINA                                 
011500*    -- CORE SCRAPPING                                                    
011600 77  DCS-IDDISTR-OSKROT-NON-VCC  PIC 9(4)  VALUE  8480.                   
011800 77  DCS-KDFRAKT-OSKROT-NON-VCC  PIC 9(2)  VALUE  68.                     
011900                                                                          
012000*    -- LOCAL REMANUFACUTER                                               
012100*    -- DISTRIKT MATAS ALLTID IN                                          
012300 77  DCS-IDKUNDNR-LRENOV-NON-VCC PIC 9(6)  VALUE  0.                      
012400 77  DCS-KDFRAKT-LRENOV-NON-VCC  PIC 9(2)  VALUE  68.                     
012500                                                                          
013200 77  OBEHORIG                    PIC X(1)   VALUE 'F'.                    
013400 77  HOPP                        PIC X(1)   VALUE 'N'.                    
013500                                                                          
013600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
013700 77  FILLER                      PIC X(08) VALUE 'AJAJAJAJ'.              
013800 77  ERROR-TEXT                  PIC X(32) VALUE SPACE.                   
013900 77  PGM-POS                     PIC X(32) VALUE SPACE.                   
014000 77  KDRC-DISPLAY                PIC Z(5).                                
014100 77  KDRC-DISP                   PIC 9(4)   VALUE ZERO.                   
014200                                                                          
014300 77  WS-IDDISTR                  PIC S9(4) VALUE ZERO COMP-3.             
014400 77  WS-IDKUNDNR                 PIC S9(6) VALUE ZERO COMP-3.             
014500 77  WS-KDORDKL                  PIC S9(1) VALUE ZERO COMP-3.             
014600                                                                          
014700 01  WS-IDFTG                    PIC XX.                                  
014800                                                                          
014900 01  WS-OHFK-IDKONTO          PIC X(10).                                  
015000 01  WS-IDKONTO               PIC 9(10).                                  
015100                                                                          
015200 01  WS-OHFK-IDANALYS         PIC X(12).                                  
015300 01  WS-IDANALYS              PIC X(12).                                  
015400                                                                          
015500 01  WS-OHFK-IDKST            PIC X(10)      VALUE SPACE.                 
015600 01  WS-IDKST                 PIC X(10)      VALUE SPACE.                 
015700                                                                          
015800 01  WS-OHFK-KDFRAKT          PIC X(2).                                   
015900 01  WS-KDFRAKT               PIC 9(2).                                   
016000                                                                          
016100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
016200     88  ALLT-OK                             VALUE 'J'.                   
016300                                                                          
016400 01  WS-DATUM-TID                PIC 9(10)  VALUE ZERO.                   
016500 01  FILLER REDEFINES WS-DATUM-TID.                                       
016600     03  WS-DATUM                PIC 9(6).                                
016700     03  WS-TID                  PIC 9(4).                                
016800 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
016900 01  FILLER REDEFINES WS-TIHHMMSS.                                        
017000     03  WS-TIHHMM               PIC 9(4).                                
017100     03  FILLER                  PIC 9(2).                                
017200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
017300 01  FILLER REDEFINES DAGENS-DATUM.                                       
017400     03  DAGENS-AA               PIC 9(2).                                
017500     03  DAGENS-MM               PIC 9(2).                                
017600     03  DAGENS-DD               PIC 9(2).                                
017700                                                                          
017800 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
017900                                                                          
018000     EJECT                                                                
018100 01  WS-ALFA-5.                                                           
018200     03  WS-NUM-5                PIC 9(5).                                
018300 01  WS-ALFA-10.                                                          
018400     03  WS-NUM-10               PIC 9(10).                               
018500 01  WS-IDKONTO-NUM              PIC 9(8).                                
018600                                                                          
018700     EJECT                                                                
018800 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
018900*                                                                         
019000 01  FILLER REDEFINES TEST-IDDISTR.                                       
019100*    03 -COPY WWDIST20                                                    
019200     EJECT                                                                
019300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019400 01  GENERELLA-SUBPROGRAM.                                                
019500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
019700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020000     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
020100     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
020200*                                                                         
020300*    --- PARAMETERS TO ABEND                                              
020400                                                                          
020500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
020700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
020800     SKIP2                                                                
020900 01  GEMENSAMMA-SUBPROGRAM.                                               
021000     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
021100*        FORMELLA KONTROLLER                                              
021200     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
021300*        LÄSNING AV KUNDREGISTRET                                         
021400     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
021500*        LOGISKA KONTROLLER                                               
021600     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
021700*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
021800*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
021900*   -COPY WSECAREA                                                        
022000     EJECT                                                                
022100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
022200*   -COPY WMEDAREA                                                        
022300     EJECT                                                                
022400 01  MESSAGE-CODES.                                                       
022500     03  ERR-OBEHORIG            PIC X(3)    VALUE '324'.                 
022600     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '030'.                 
022700     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
022800     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
022900     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '295'.                 
023000     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '296'.                 
023100     03  ERR-SAKNAS-BET          PIC X(3)    VALUE '297'.                 
023200     03  ERR-FIELD-IS-INVALID    PIC X(3)    VALUE '023'.                 
023300     03  ERR-INVALID-KEY-FIELDS  PIC X(3)    VALUE '022'.                 
023400     EJECT                                                                
023500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
023600*   -COPY W411OHFK                                                        
023700     EJECT                                                                
023800*   -COPY W411KREG                                                        
023900     EJECT                                                                
024000*   -COPY W411OHLK                                                        
024100     EJECT                                                                
024200*   -COPY W411ORDN                                                        
024300     EJECT                                                                
024400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
024600     SKIP3                                                                
024700*01  -COPY WZ01SUB                                                        
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
025000     SKIP3                                                                
025100 01  REQU-AREA.                                                           
025200*    03  -COPY WZ01REQU                                                   
025300*    03  -COPY WL0165I1                                                   
025400     EJECT                                                                
025500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
025600     SKIP3                                                                
025700 01  RESP-AREA.                                                           
025800*    03  -COPY WZ01RESP                                                   
025900*    03  -COPY WL0165O1                                                   
026000*                                                                         
026100*01  -COPY WL01TIDZ                                                       
026200     EJECT                                                                
026300***********************************************************               
026400     SKIP3                                                                
026500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026700 01  NYCKLAR-TILL-DLI.                                                    
026800     03  W-WDQ2CSEQ-X.                                                    
026900         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
027000         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
027100         05  W-IDKUNDRF.                                                  
027200           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
027300           07  FILLER            PIC X(3)    VALUE SPACE.                 
027400     03  W-IDDC-B6-X.                                                     
027500         05 W-IDDC-B6            PIC X(2).                                
027600     EJECT                                                                
027700*    --- STATUS-KOD FRÅN IMS                                              
027800 01  STATUS-WS                   PIC XX.                                  
027900     88  SEGMENT-FINNS                       VALUE '  '.                  
028000     SKIP2                                                                
028100 01  GODK-STATUSKODER.                                                    
028200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028300     SKIP3                                                                
028400 01  SSA1                        PIC X(64).                               
028500     SKIP2                                                                
028600*    --- IMS FUNKTIONSKODER                                               
028700*01  -COPY W0003                                                          
028800     SKIP2                                                                
028900*    ---  DLI INPUT-OUTPUT AREA                                           
029000     SKIP3                                                                
029100 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
029200 01  DLI-IO-AREA-OHUV.                                                    
029300     03  WLORQI01.                                                        
029400*        05  -COPY WDQ201                                                 
029500     SKIP2                                                                
029600 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
029700 01  DLI-IO-AREA-ARB.                                                     
029800     03  WLORQI12.                                                        
029900*        05  -COPY WDQ212                                                 
030000     SKIP2                                                                
030100 01  FILLER                      PIC X(08)   VALUE 'LINKAGE:'.            
030200*                                                                         
030300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030400 01   DLI-IO-AREA-B601.                                                   
030500*     03  -COPY WDB601                                                    
030600     EJECT                                                                
030700 LINKAGE SECTION.                                                         
030800*01  -COPY W0009   -PRE MSG-                                              
030900     SKIP2                                                                
031000*01  -COPY W0008   -PRE ORQI-                                             
031100     05  FILLER                  PIC X.                                   
031200     SKIP2                                                                
031300 01  KREG-GMTA-PCB               PIC X.                                   
031400 01  KREG-GMTB-PCB               PIC X.                                   
031500 01  KREG-GMTC-PCB               PIC X.                                   
031600 01  KREG-BETC-PCB               PIC X.                                   
031700 01  OHLK-WDM2-PCB               PIC X.                                   
031800 01  ORDN-XXKP-PCB               PIC X.                                   
031900 01  ORDN-ORQL-PCB               PIC X.                                   
032000 01  ORDN-PROC-PCB               PIC X.                                   
032100 01  ORDN-ORQI-PCB               PIC X.                                   
032200 01  SAP-SAPC-PCB                PIC X.                                   
032300*01  -COPY W0008  -PRE WDB6-                                              
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600 PROCEDURE DIVISION  USING MSG-PCB                                        
032700                     ORQI-PCB                                             
032800                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
032900                     KREG-BETC-PCB                                        
033000                     OHLK-WDM2-PCB                                        
033100                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
033200                     ORDN-ORQI-PCB                                        
033300                     SAP-SAPC-PCB WDB6-PCB.                               
033400 MAIN SECTION.                                                            
033500     ENTRY 'DLITCBL' USING MSG-PCB                                        
033600                     ORQI-PCB                                             
033700                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
033800                     KREG-BETC-PCB                                        
033900                     OHLK-WDM2-PCB                                        
034000                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
034100                     ORDN-ORQI-PCB                                        
034200                     SAP-SAPC-PCB WDB6-PCB.                               
034300                                                                          
034400     SKIP3                                                                
034500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
034600     IF SUB-KDRC = 0                                                      
034700       IF REQU-KDPGMACT = 'E' OR 'S'                                      
034800         PERFORM A-INIT                                                   
034900                                                                          
035000         IF REQU-KDPGMACT = 'E' AND ALLT-OK                               
035100            PERFORM B-KOLLA-O-KOMPLETTERA-INDATA                          
035200            PERFORM AA-KOLLA-BEHORIGHET                                   
035300                                                                          
035400            IF ALLT-OK                                                    
035500               PERFORM C-BESTAM-ORDERNUMMER                               
035600               PERFORM D-SKAPA-ORDERHUVUD                                 
035700            END-IF                                                        
035800         END-IF                                                           
035900                                                                          
036000       END-IF                                                             
036100                                                                          
036200       PERFORM S02-RETURN-RESPONSE                                        
036300     END-IF                                                               
036400                                                                          
036500     MOVE +0 TO RETURN-CODE                                               
036600     GOBACK                                                               
036700     .                                                                    
036800     EJECT                                                                
036900 A-INIT SECTION.                                                          
037000     MOVE 'STA A-INIT        ' TO PGM-POS                                 
037100                                                                          
037200     MOVE JA                   TO ALLT-SW                                 
037300     MOVE NEJ                  TO HOPP                                    
037400                                                                          
037500     MOVE ALL '+'              TO RESP-AREA                               
037600     MOVE 001                  TO RESP-IDMSGVER                           
037700     MOVE SPACE                TO RESP-IDMSG-ERROR                        
037800                                  RESP-IDMSG-INFO                         
037900                                  RESP-IDELMT-ERROR                       
038000     MOVE ALL-PLUS             TO RESP-IDFTG                              
038100                                  RESP-IDKONTO                            
038200                                  RESP-IDANALYS                           
038300                                  RESP-IDKST                              
038400                                                                          
038500     ACCEPT DAGENS-DATUM FROM DATE                                        
038600     ACCEPT DAGENS-TID   FROM TIME                                        
038700                                                                          
038800     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
038900     PERFORM IMS-GU-WDB601                                                
039000     PERFORM AB-DEFAULT-FROM-WDB6-TO-RESP                                 
039100                                                                          
039200     MOVE 'END A-INIT        ' TO PGM-POS                                 
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 AA-KOLLA-BEHORIGHET SECTION.                                             
039700     MOVE 'STA AA-KOLLA-BEHORIGHET       ' TO PGM-POS                     
039800                                                                          
039900     MOVE REQU-IDUSER             TO SEC-IDUSER                           
040000     MOVE '0166'                  TO SEC-IDTRANS                          
040100     MOVE WS-IDDISTR              TO SEC-IDKEY                            
040200                                                                          
040300     CALL WSECURIT USING SEC-IDUSER                                       
040400                         SEC-IDTRANS                                      
040500                         SEC-IDKEY                                        
040600                         SEC-KDSVAR                                       
040700                                                                          
040800     IF SEC-KDSVAR = OBEHORIG                                             
040900        MOVE NEJ                  TO ALLT-SW                              
041000        MOVE ERR-OBEHORIG         TO RESP-IDMSG-ERROR                     
041100        MOVE 'IDANSTNR'           TO RESP-IDELMT-ERROR                    
041200     END-IF                                                               
041300     MOVE 'END AA-KOLLA-BEHORIGHET       ' TO PGM-POS                     
041400     .                                                                    
041500     EJECT                                                                
041600 AB-DEFAULT-FROM-WDB6-TO-RESP SECTION.                                    
041700     MOVE 'AB-DEFAULT-FROM-WDB6' TO PGM-POS                               
041800                                                                          
041900     MOVE DCS-IDDISTR-JUST       TO RESP-IDDISTR-JUST                     
042000     MOVE DCS-IDKUNDNR-JUST      TO RESP-IDKUNDNR-JUST                    
042100                                                                          
042200     MOVE DCS-IDDISTR-MIX        TO RESP-IDDISTR-MIX                      
042300     MOVE DCS-IDKUNDNR-MIX       TO RESP-IDKUNDNR-MIX                     
042400                                                                          
042500     MOVE DCS-IDDISTR-RSKROT     TO RESP-IDDISTR-RSKROT                   
042600     MOVE DCS-IDKUNDNR-RSKROT    TO RESP-IDKUNDNR-RSKROT                  
042700                                                                          
042800     MOVE DCS-IDDISTR-QSKROT     TO RESP-IDDISTR-QSKROT                   
042900     MOVE DCS-IDKUNDNR-QSKROT    TO RESP-IDKUNDNR-QSKROT                  
043000                                                                          
043100*--- TEMP HÅRDKODADE VÄRDEN AUSTRALIEN                                    
043200*--- SE WORKING-STORAGE                                                   
043300     IF DCS-IDLANDX2 = 'AU' OR 'JP'                                       
043400*      MOVE REQU-IDDISTR-LRENOV  TO RESP-IDDISTR-LRENOV                   
043500       MOVE DCS-IDKUNDNR-LRENOV  TO RESP-IDKUNDNR-LRENOV                  
043600                                                                          
043700       MOVE DCS-IDDISTR-BKONV    TO RESP-IDDISTR-BKONV                    
043800       MOVE DCS-IDKUNDNR-BKONV   TO RESP-IDKUNDNR-BKONV                   
043900                                                                          
044000       MOVE DCS-IDDISTR-OSKROT   TO RESP-IDDISTR-OSKROT                   
044100       MOVE DCS-IDKUNDNR-OSKROT  TO RESP-IDKUNDNR-OSKROT                  
044200     ELSE                                                                 
044300       IF DCS-CANADA OR DCS-USA                                           
044400*        MOVE REQU-IDDISTR-LRENOV    TO RESP-IDDISTR-LRENOV               
044500         MOVE ZERO               TO RESP-IDKUNDNR-LRENOV                  
044600                                                                          
044700         MOVE ZERO               TO RESP-IDDISTR-BKONV                    
044800         MOVE ZERO               TO RESP-IDKUNDNR-BKONV                   
044900                                                                          
045000         MOVE DCS-IDDISTR-OSKROT-NA TO RESP-IDDISTR-OSKROT                
045100         MOVE REQU-IDDC-KEY      TO RESP-IDKUNDNR-OSKROT                  
045200       ELSE                                                               
045300         IF DCS-LAND-NON-VCC-OWNED                                        
045400*          MOVE REQU-IDDISTR-LRENOV  TO RESP-IDDISTR-LRENOV               
045500           MOVE ZERO             TO RESP-IDKUNDNR-LRENOV                  
045600                                                                          
045700           MOVE ZERO             TO RESP-IDDISTR-BKONV                    
045800           MOVE ZERO             TO RESP-IDKUNDNR-BKONV                   
045900                                                                          
046000           MOVE DCS-IDDISTR-OSKROT-NON-VCC TO RESP-IDDISTR-OSKROT         
046100           MOVE REQU-IDDC-KEY    TO RESP-IDKUNDNR-OSKROT                  
046200         ELSE                                                             
048300*---     DESSA TYPER ANVÄNDS EJ AV ANDRA LAGER                            
048401           MOVE ZERO             TO RESP-IDDISTR-LRENOV                   
048501           MOVE ZERO             TO RESP-IDKUNDNR-LRENOV                  
048601           MOVE ZERO             TO RESP-IDDISTR-BKONV                    
048701           MOVE ZERO             TO RESP-IDKUNDNR-BKONV                   
048801           MOVE ZERO             TO RESP-IDDISTR-OSKROT                   
048901           MOVE ZERO             TO RESP-IDKUNDNR-OSKROT                  
049001         END-IF                                                           
049002       END-IF                                                             
049201     END-IF                                                               
049501     .                                                                    
049601                                                                          
049701     EJECT                                                                
049801 B-KOLLA-O-KOMPLETTERA-INDATA SECTION.                                    
049901     MOVE 'STA B-KOLLA-O-KOMPLETTERA-INDA' TO PGM-POS                     
050001                                                                          
050101     PERFORM BC-DEFAULT-FROM-WDB6                                         
050201     IF ALLT-OK                                                           
050301                                                                          
050401       PERFORM BA-KONTROLLERA-FORMELLA-FEL                                
050501       IF ALLT-OK                                                         
050601                                                                          
050701          PERFORM BB-LAS-KUNDREGISTRET                                    
050801          IF ALLT-OK                                                      
050901                                                                          
051001             PERFORM BD-KONTROLLERA-LOGISKA-FEL                           
051101                                                                          
051201          END-IF                                                          
051301       END-IF                                                             
051401     END-IF                                                               
051501     MOVE 'END B-KOLLA-O-KOMPLETTERA-INDA' TO PGM-POS                     
051601     .                                                                    
051701     EJECT                                                                
051801                                                                          
051901 BA-KONTROLLERA-FORMELLA-FEL SECTION.                                     
052001     MOVE 'STA BA-KONTROLLERA-FORMELLA-FE' TO PGM-POS                     
052101                                                                          
052201     MOVE 'IMS '               TO OHFK-IDSYSTEM                           
052301     MOVE WS-IDDISTR           TO OHFK-IDDISTR                            
052401     MOVE WS-IDKUNDNR          TO OHFK-IDKUNDNR                           
052501     MOVE ALL '+'              TO OHFK-IDORDNR                            
052601     MOVE WS-KDORDKL           TO OHFK-KDORDKL                            
052701     MOVE WS-OHFK-KDFRAKT      TO OHFK-KDFRAKT                            
052801     MOVE JA                   TO OHFK-FLAUTFAK                           
052901     MOVE NEJ                  TO OHFK-FLFORBI                            
053001     MOVE JA                   TO OHFK-FLORDSPE                           
053101     MOVE JA                   TO OHFK-FLLSBOK                            
053201     MOVE JA                   TO OHFK-FLAUTPAC                           
053301     MOVE NEJ                  TO OHFK-FLVORKO                            
053401     MOVE NEJ                  TO OHFK-FLRESTN                            
053501     MOVE SPACE                TO OHFK-IDBIPREF                           
053601     MOVE WS-IDFTG             TO OHFK-IDFTG                              
053701     MOVE WS-OHFK-IDKONTO      TO OHFK-IDKONTO                            
053801     MOVE WS-OHFK-IDANALYS     TO OHFK-IDANALYS                           
053901     MOVE WS-OHFK-IDKST        TO OHFK-IDKST                              
054001     MOVE ZERO                 TO OHFK-IDKAMPRF                           
054101     MOVE REQU-IDDC-KEY        TO OHFK-IDDC                               
054201     MOVE ALL '+'              TO OHFK-KDFAKTYP                           
054301                                                                          
054401******** ADAPT DATE AND TIME FOR TIMEZONES                                
054501     MOVE '011'                    TO MSGI-KDCALL                         
054601     MOVE DCS-IDTIDZON             TO MSGI-IDTIDZON                       
054601     MOVE DCS-IDDC                 TO MSGI-IDDC                           
054701     MOVE DAGENS-DATUM             TO MSGI-TILOKDAT                       
054801     MOVE DAGENS-TID               TO MSGI-TILOKTID                       
054901     CALL WL01TIDZ USING              MSGI-WL01TIDZ                       
055001     MOVE MSGI-TILOKDAT(1:6)       TO DAGENS-DATUM                        
055101     MOVE MSGI-TILOKTID(1:4)       TO DAGENS-TID(1:4)                     
055201     MOVE FUNCTION CURRENT-DATE(13:2) TO WS-TIHHMMSS(5:2)                 
055301********                                                                  
055401     MOVE DAGENS-DATUM    TO OHFK-TIREGDAT                                
055501     MOVE DAGENS-TID(1:4) TO OHFK-TIHHMM                                  
055601     MOVE ALL '+'              TO OHFK-KDROPACK                           
055701                                  OHFK-KDTPOTYP                           
055801                                  OHFK-TITPO                              
055901                                  OHFK-TIRFSDAT                           
056001                                  OHFK-TIRFSTID                           
056101                                  OHFK-IDSKYLT                            
056201                                  OHFK-KDTULLVE                           
056301                                  OHFK-KDVRINFO                           
056401                                  OHFK-TIFORDAT                           
056501                                  OHFK-KDPROTYP                           
056601                                                                          
056701     CALL W411OHFK USING OHFK-W411OHFK                                    
056801     PERFORM BAA-KOLLA-FEL-FK                                             
056901     MOVE 'END BA-KONTROLLERA-FORMELLA-FE' TO PGM-POS                     
057001     .                                                                    
057101     EJECT                                                                
057201 BAA-KOLLA-FEL-FK SECTION.                                                
057301     MOVE 'STA BA-KONTROLLERA-FORMELLA-FE' TO PGM-POS                     
057401                                                                          
057501     IF OHFK-IDDISTR-OK = NEJ                                             
057601        MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                     
057701        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
057801        MOVE NEJ                 TO ALLT-SW                               
057901     ELSE                                                                 
058001        MOVE OHFK-IDDISTR        TO W-IDDISTR                             
058101                                    TEST-IDDISTR                          
058201     END-IF                                                               
058301                                                                          
058401     IF OHFK-IDKUNDNR-OK = NEJ                                            
058501        MOVE 'IDKUNDNR'          TO RESP-IDELMT-ERROR                     
058601        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
058701        MOVE NEJ                 TO ALLT-SW                               
058801     ELSE                                                                 
058901        MOVE OHFK-IDKUNDNR       TO W-IDKUNDNR                            
059001     END-IF                                                               
059101                                                                          
059201     MOVE ZERO                   TO W-IDORDNR                             
059301     SKIP2                                                                
059401                                                                          
059501     IF OHFK-KDORDKL-OK = NEJ                                             
059601        MOVE 'KDORDKL'           TO RESP-IDELMT-ERROR                     
059701        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
059801        MOVE NEJ                 TO ALLT-SW                               
059901     END-IF                                                               
060001                                                                          
060101     IF OHFK-KDFRAKT-OK = NEJ                                             
060201        MOVE 'KDFRAKT'           TO RESP-IDELMT-ERROR                     
060301        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
060401        MOVE NEJ                 TO ALLT-SW                               
060501     END-IF                                                               
060601                                                                          
060701     IF OHFK-IDFTG-OK = NEJ                                               
060801        MOVE 'IDFTG'             TO RESP-IDELMT-ERROR                     
060901        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
061001        MOVE NEJ                 TO ALLT-SW                               
061101     END-IF                                                               
061201     SKIP2                                                                
061301                                                                          
061401     IF OHFK-IDKONTO-OK = NEJ                                             
061501        MOVE 'IDKONTO'           TO RESP-IDELMT-ERROR                     
061601        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
061701        MOVE NEJ                 TO ALLT-SW                               
061801     END-IF                                                               
061901                                                                          
062001     IF OHFK-IDANALYS-OK = NEJ                                            
062101        MOVE 'IDANALYS'          TO RESP-IDELMT-ERROR                     
062201        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
062301        MOVE NEJ                 TO ALLT-SW                               
062401     END-IF                                                               
062501                                                                          
062601     IF OHFK-IDKST-OK = NEJ                                               
062701        MOVE 'IDKST'             TO RESP-IDELMT-ERROR                     
062801        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
062901        MOVE NEJ                 TO ALLT-SW                               
063001     END-IF                                                               
063101                                                                          
063201     IF OHFK-IDDC-OK = NEJ                                                
063301        MOVE 'IDDC'           TO RESP-IDELMT-ERROR                        
063401        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
063501        MOVE NEJ                 TO ALLT-SW                               
063601     END-IF                                                               
063701                                                                          
063801     IF OHFK-KDFAKTYP-OK = NEJ                                            
063901        MOVE 'KDFAKTYP'          TO RESP-IDELMT-ERROR                     
064001        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
064101        MOVE NEJ                 TO ALLT-SW                               
064201     END-IF                                                               
064301     MOVE 'END BA-KONTROLLERA-FORMELLA-FE' TO PGM-POS                     
064401     .                                                                    
064501     EJECT                                                                
064601 BB-LAS-KUNDREGISTRET SECTION.                                            
064701     MOVE 'STA BB-LAS-KUNDREGISTRET      ' TO PGM-POS                     
064801                                                                          
064901     MOVE W-IDDISTR            TO KREG-IDDISTR                            
065001     MOVE W-IDKUNDNR           TO KREG-IDKUNDNR                           
065101     MOVE 'IMS '               TO KREG-IDSYSTEM                           
065201     MOVE REQU-IDDC-KEY        TO KREG-IDDC-TVS                           
065301     MOVE WS-KDFRAKT           TO KREG-KDFRAKT-IN                         
065401     MOVE WS-KDORDKL           TO KREG-KDORDKL                            
065501     MOVE SPACE                TO KREG-KDFAKTYP-IN                        
065601                                                                          
065701     MOVE NEJ                  TO KREG-FLVORKO                            
065801                                  KREG-FLVORFK                            
065901                                                                          
066001     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB KREG-GMTB-PCB        
066101                                       KREG-GMTC-PCB KREG-BETC-PCB        
066201                                                                          
066301     IF KREG-KDKREDSP = '1'                                               
066401        MOVE ERR-KUND-SPAERRAD   TO RESP-IDMSG-ERROR                      
066501        MOVE NEJ                 TO ALLT-SW                               
066601     ELSE                                                                 
066701        IF KREG-IDVAT-OK = NEJ                                            
066801          MOVE ERR-MOMS-REGNR-FEL TO RESP-IDMSG-ERROR                     
066901          MOVE NEJ                TO ALLT-SW                              
067001        ELSE                                                              
067101          IF KREG-IDPARTNR-OK = NEJ                                       
067201            MOVE ERR-SAKNAS-BET      TO RESP-IDMSG-ERROR                  
067301            MOVE NEJ                 TO ALLT-SW                           
067401          END-IF                                                          
067501        END-IF                                                            
067601     END-IF                                                               
067701                                                                          
067801     PERFORM BBA-KOLLA-FEL-KREG                                           
067901     MOVE 'END BB-LAS-KUNDREGISTRET      ' TO PGM-POS                     
068001     .                                                                    
068101     EJECT                                                                
068201 BBA-KOLLA-FEL-KREG SECTION.                                              
068301     MOVE 'STA BBA-KOLLA-FEL-KREG        ' TO PGM-POS                     
068401                                                                          
068501     IF KREG-IDDISTR-OK = NEJ                                             
068601        MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                     
068701        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
068801        MOVE NEJ                 TO ALLT-SW                               
068901     END-IF                                                               
069001                                                                          
069101     IF KREG-IDKUNDNR-OK = NEJ                                            
069201        MOVE 'IDKUNDNR'          TO RESP-IDELMT-ERROR                     
069301        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
069401        MOVE NEJ                 TO ALLT-SW                               
069501     END-IF                                                               
069601                                                                          
069701     IF KREG-IDDC-OK = NEJ                                                
069801        MOVE 'IDDC'               TO RESP-IDELMT-ERROR                    
069901        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
070001        MOVE NEJ                  TO ALLT-SW                              
070101     END-IF                                                               
070201                                                                          
070301     IF KREG-KDFRAKT-OK = NEJ                                             
070401        MOVE 'KDFRAKT'           TO RESP-IDELMT-ERROR                     
070501        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
070601        MOVE NEJ                 TO ALLT-SW                               
070701     END-IF                                                               
070801     MOVE 'END BBA-KOLLA-FEL-KREG        ' TO PGM-POS                     
070901     .                                                                    
071001                                                                          
071101     EJECT                                                                
071201 BC-DEFAULT-FROM-WDB6         SECTION.                                    
071301* -- SÄTT VÄRDEN BEROENDE PÅ VILKEN RADIO-BUTTON SOM VALDES               
071401* -- OCH SIMULERA ATT VÄRDENA MATATS IN MANUELLT                          
071501*                                                                         
071601* -- OBS: VISSA VARIANTER ÄR JUST NU HÅRDKODADE I WORKING-STORAGE         
071701* -- OCH SAMMA ORDERKLASS ANVÄNDS FÖR ALLA VARIANTER                      
071801     EVALUATE REQU-KDBEH                                                  
071901                                                                          
072001     WHEN PACKING-ADJUSTMENT                                              
072101         MOVE DCS-KDORDKL-ALLA        TO WS-KDORDKL                       
072201                                                                          
072301         MOVE DCS-IDDISTR-JUST        TO WS-IDDISTR                       
072401         MOVE DCS-IDKUNDNR-JUST       TO WS-IDKUNDNR                      
072501         MOVE DCS-IDKONTO-JUST        TO WS-OHFK-IDKONTO                  
072601                                         WS-IDKONTO                       
072701         MOVE DCS-IDANALYS-JUST       TO WS-OHFK-IDANALYS                 
072801                                         WS-IDANALYS                      
072901         MOVE DCS-IDKST-JUST          TO WS-OHFK-IDKST                    
073001                                         WS-IDKST                         
073101         MOVE ALL '+'                 TO WS-OHFK-KDFRAKT                  
073201         MOVE ZERO                    TO WS-KDFRAKT                       
073301                                                                          
073401     WHEN MIX-STOCK                                                       
073501         MOVE DCS-KDORDKL-ALLA        TO WS-KDORDKL                       
073601                                                                          
073701         MOVE DCS-IDDISTR-MIX         TO WS-IDDISTR                       
073801         MOVE DCS-IDKUNDNR-MIX        TO WS-IDKUNDNR                      
073901         MOVE DCS-IDKONTO-MIX         TO WS-OHFK-IDKONTO                  
074001                                         WS-IDKONTO                       
074101         MOVE DCS-IDANALYS-MIX        TO WS-OHFK-IDANALYS                 
074201                                         WS-IDANALYS                      
074301         MOVE DCS-IDKST-MIX           TO WS-OHFK-IDKST                    
074401                                         WS-IDKST                         
074501         MOVE ALL '+'                 TO WS-OHFK-KDFRAKT                  
074601         MOVE ZERO                    TO WS-KDFRAKT                       
074701                                                                          
074801     WHEN SCRAP                                                           
074901         MOVE DCS-KDORDKL-ALLA        TO WS-KDORDKL                       
075001                                                                          
075101*--- NÄR DET ÄR INTERNSKROT OCH KINA DC SKALL MAN ANVÄNDA DISTRIKT        
075201*--- SCR-R (8497) ISTÄLLET FÖR SCR-Q. SUSSI 2012-04-03                    
075301         IF DCS-LAND-NON-VCC-OWNED OR DCS-CANADA OR DCS-USA               
075401           MOVE DCS-IDDISTR-RSKROT    TO WS-IDDISTR                       
075501           MOVE DCS-IDKUNDNR-RSKROT   TO WS-IDKUNDNR                      
075601         ELSE                                                             
075701           MOVE DCS-IDDISTR-QSKROT    TO WS-IDDISTR                       
075801           MOVE DCS-IDKUNDNR-QSKROT   TO WS-IDKUNDNR                      
075901         END-IF                                                           
076001         MOVE DCS-IDKONTO-SKROT       TO WS-OHFK-IDKONTO                  
076101                                         WS-IDKONTO                       
076201         MOVE DCS-IDANALYS-SKROT      TO WS-OHFK-IDANALYS                 
076301                                         WS-IDANALYS                      
076401         MOVE DCS-IDKST-SKROT         TO WS-OHFK-IDKST                    
076501                                         WS-IDKST                         
076601         MOVE ALL '+'                 TO WS-OHFK-KDFRAKT                  
076701         MOVE ZERO                    TO WS-KDFRAKT                       
076801                                                                          
076901     WHEN CORE-SCRAP                                                      
077001         MOVE DCS-KDORDKL-ALLA          TO WS-KDORDKL                     
077101         IF DCS-CANADA OR DCS-USA                                         
077201           MOVE DCS-IDDISTR-OSKROT-NA   TO WS-IDDISTR                     
077301           MOVE REQU-IDDC-KEY           TO WS-IDKUNDNR                    
077401           MOVE DCS-KDFRAKT-OSKROT-NA   TO WS-OHFK-KDFRAKT                
077501                                           WS-KDFRAKT                     
077601           MOVE ALL '+'                 TO WS-OHFK-IDANALYS               
077701           MOVE SPACE                   TO WS-IDANALYS                    
077801           MOVE ALL '+'                 TO WS-OHFK-IDKONTO                
077901           MOVE ZERO                    TO WS-IDKONTO                     
078001           MOVE ALL '+'                 TO WS-OHFK-IDKST                  
078101           MOVE SPACE                   TO WS-IDKST                       
078201         ELSE                                                             
078301           IF DCS-LAND-NON-VCC-OWNED                                      
078401             MOVE DCS-IDDISTR-OSKROT-NON-VCC TO WS-IDDISTR                
078501             MOVE REQU-IDDC-KEY              TO WS-IDKUNDNR               
078601             MOVE DCS-KDFRAKT-OSKROT-NON-VCC TO WS-OHFK-KDFRAKT           
078701                                                WS-KDFRAKT                
078801             MOVE ALL '+'                    TO WS-OHFK-IDANALYS          
078901             MOVE SPACE                      TO WS-IDANALYS               
079001             MOVE ALL '+'                    TO WS-OHFK-IDKONTO           
079101             MOVE ZERO                       TO WS-IDKONTO                
079201             MOVE ALL '+'                    TO WS-OHFK-IDKST             
079301             MOVE SPACE                      TO WS-IDKST                  
081801           ELSE                                                           
081901             MOVE DCS-IDDISTR-OSKROT    TO WS-IDDISTR                     
082001             MOVE DCS-IDKUNDNR-OSKROT   TO WS-IDKUNDNR                    
082101             MOVE DCS-IDKONTO-OSKROT    TO WS-OHFK-IDKONTO                
082201                                           WS-IDKONTO                     
082301             MOVE DCS-IDANALYS-OSKROT   TO WS-OHFK-IDANALYS               
082401                                           WS-IDANALYS                    
082501             MOVE DCS-IDKST-OSKROT      TO WS-OHFK-IDKST                  
082601                                           WS-IDKST                       
082701             MOVE ALL '+'               TO WS-OHFK-KDFRAKT                
082801             MOVE ZERO                  TO WS-KDFRAKT                     
083101           END-IF                                                         
083201         END-IF                                                           
083301                                                                          
083401     WHEN LOCAL-REMAN                                                     
083501         MOVE DCS-KDORDKL-ALLA        TO WS-KDORDKL                       
083601                                                                          
083701*        -- FÖR DENNA VARIANT FINNS INGET DEFAULTVÄRDE FÖR                
083801*        -- DISTRIKT UTAN DET MÅSTE MATAS IN                              
083901         IF REQU-IDDISTR-LRENOV NUMERIC                                   
084001           MOVE REQU-IDDISTR-LRENOV   TO WS-IDDISTR                       
084101         ELSE                                                             
084201           MOVE NEJ                   TO ALLT-SW                          
084301           MOVE ERR-INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                
084401           MOVE 'IDDISTR'             TO RESP-IDELMT-ERROR                
084501         END-IF                                                           
084601         IF DCS-CANADA OR DCS-USA                                         
084701           MOVE DCS-IDKUNDNR-LRENOV-NA  TO WS-IDKUNDNR                    
084801           MOVE DCS-KDFRAKT-LRENOV-NA   TO WS-IDKONTO                     
084901           MOVE DCS-KDFRAKT-LRENOV-NA   TO WS-OHFK-KDFRAKT                
085001                                           WS-KDFRAKT                     
085101           MOVE ALL '+'                 TO WS-OHFK-IDANALYS               
085201           MOVE SPACE                   TO WS-IDANALYS                    
085301           MOVE ALL '+'                 TO WS-OHFK-IDKONTO                
085401           MOVE ZERO                    TO WS-IDKONTO                     
085501           MOVE ALL '+'                 TO WS-OHFK-IDKST                  
085601           MOVE SPACE                   TO WS-IDKST                       
085701         ELSE                                                             
087001           IF DCS-LAND-NON-VCC-OWNED                                      
087101             MOVE DCS-IDKUNDNR-LRENOV-NON-VCC TO WS-IDKUNDNR              
087201             MOVE DCS-KDFRAKT-LRENOV-NON-VCC  TO WS-IDKONTO               
087301             MOVE DCS-KDFRAKT-LRENOV-NON-VCC  TO WS-OHFK-KDFRAKT          
087401                                                 WS-KDFRAKT               
087501             MOVE ALL '+'                     TO WS-OHFK-IDANALYS         
087601             MOVE SPACE                       TO WS-IDANALYS              
087701             MOVE ALL '+'                     TO WS-OHFK-IDKONTO          
087801             MOVE ZERO                        TO WS-IDKONTO               
087901             MOVE ALL '+'                     TO WS-OHFK-IDKST            
088001             MOVE SPACE                       TO WS-IDKST                 
088101           ELSE                                                           
088201             MOVE DCS-IDKUNDNR-LRENOV     TO WS-IDKUNDNR                  
088301             MOVE DCS-IDKONTO-LRENOV      TO WS-OHFK-IDKONTO              
088401                                             WS-IDKONTO                   
088501             MOVE DCS-IDANALYS-LRENOV     TO WS-OHFK-IDANALYS             
088601                                             WS-IDANALYS                  
088701             MOVE DCS-IDKST-LRENOV        TO WS-OHFK-IDKST                
088801                                             WS-IDKST                     
088901             MOVE ALL '+'                 TO WS-OHFK-KDFRAKT              
089001             MOVE ZERO                    TO WS-KDFRAKT                   
089201           END-IF                                                         
089301         END-IF                                                           
089401                                                                          
089501     WHEN EXCHANGE-CONV                                                   
089601         MOVE DCS-KDORDKL-ALLA        TO WS-KDORDKL                       
089701                                                                          
089801         MOVE DCS-IDDISTR-BKONV       TO WS-IDDISTR                       
089901         MOVE DCS-IDKUNDNR-BKONV      TO WS-IDKUNDNR                      
090001         MOVE DCS-IDKONTO-BKONV       TO WS-OHFK-IDKONTO                  
090101                                         WS-IDKONTO                       
090201         MOVE DCS-IDANALYS-BKONV      TO WS-OHFK-IDANALYS                 
090301                                         WS-IDANALYS                      
090401         MOVE DCS-IDKST-BKONV         TO WS-OHFK-IDKST                    
090501                                         WS-IDKST                         
090601         MOVE ALL '+'                 TO WS-OHFK-KDFRAKT                  
090701         MOVE ZERO                    TO WS-KDFRAKT                       
090801                                                                          
090901     WHEN OTHER                                                           
091001        MOVE NEJ                      TO ALLT-SW                          
091101        MOVE ERR-FIELD-IS-INVALID     TO RESP-IDMSG-ERROR                 
091201        MOVE 'KDBEHADJ'               TO RESP-IDELMT-ERROR                
091301                                                                          
091401     END-EVALUATE                                                         
091501                                                                          
091601     IF ALLT-OK                                                           
091701       IF REQU-IDFTG = ALL '+'                                            
091801         MOVE DCS-IDFTG                TO WS-IDFTG                        
091901       ELSE                                                               
092001         IF REQU-IDFTG   NUMERIC                                          
092101           MOVE REQU-IDFTG             TO WS-IDFTG                        
092201         ELSE                                                             
092301           MOVE NEJ                    TO ALLT-SW                         
092401           MOVE ERR-INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                
092501           MOVE 'IDFTG'                TO RESP-IDELMT-ERROR               
092601         END-IF                                                           
092701       END-IF                                                             
092801                                                                          
092901       IF REQU-IDKONTO NOT = ALL '+'                                      
093001         IF REQU-IDKONTO NUMERIC                                          
093101           MOVE REQU-IDKONTO           TO WS-OHFK-IDKONTO                 
093201                                          WS-IDKONTO                      
093301         ELSE                                                             
093401           MOVE NEJ                    TO ALLT-SW                         
093501           MOVE ERR-INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                
093601           MOVE 'IDKONTO'              TO RESP-IDELMT-ERROR               
093701         END-IF                                                           
093801       END-IF                                                             
093901                                                                          
094001       IF REQU-IDKST NOT = ALL '+'                                        
094101          MOVE REQU-IDKST             TO WS-OHFK-IDKST                    
094201                                         WS-IDKST                         
094301       END-IF                                                             
094401                                                                          
094501       IF REQU-IDANALYS NOT = ALL '+'                                     
094601         MOVE REQU-IDANALYS            TO WS-OHFK-IDANALYS                
094701                                          WS-IDANALYS                     
094801         IF WS-IDANALYS = ZERO OR SPACE                                   
094901           MOVE NEJ                    TO ALLT-SW                         
095001           MOVE ERR-INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                
095101           MOVE 'IDANALYS'             TO RESP-IDELMT-ERROR               
095201         END-IF                                                           
095301       END-IF                                                             
095401                                                                          
095501     END-IF                                                               
095601                                                                          
095701* FIX                                                                     
095801*    DISPLAY ' '                                                          
095901*    DISPLAY FUNCTION CURRENT-DATE                                        
096001*    DISPLAY 'WL0165 DISTR  ' WS-IDDISTR                                  
096101*    DISPLAY 'WL0165 KUND   ' WS-IDKUNDNR                                 
096201*    DISPLAY 'WL0165 FTG  : ' WS-IDFTG                                    
096301*    DISPLAY 'WL0165 KONTO: ' WS-IDKONTO                                  
096401*    DISPLAY 'WL0165 ANALYS:' WS-IDANALYS                                 
096501*    DISPLAY 'WL0165 KST:   ' WS-IDKST                                    
096601* END-FIX                                                                 
096701                                                                          
096801     MOVE WS-IDDISTR   TO W-IDDISTR                                       
096901     MOVE WS-IDKUNDNR  TO W-IDKUNDNR                                      
097001     .                                                                    
097101     EJECT                                                                
097201 BD-KONTROLLERA-LOGISKA-FEL SECTION.                                      
097301     MOVE 'STA BD-KONTROLLERA-LOGISKA-FEL' TO PGM-POS                     
097401                                                                          
097501     MOVE 'IMS '               TO OHLK-IDSYSTEM                           
097601     MOVE W-IDDISTR            TO OHLK-IDDISTR                            
097701     MOVE W-IDKUNDNR           TO OHLK-IDKUNDNR                           
097801     MOVE W-IDORDNR            TO OHLK-IDORDNR                            
097901     MOVE WS-KDORDKL           TO OHLK-KDORDKL                            
098001     MOVE REQU-IDDC-KEY        TO OHLK-IDDC                               
098101                                  OHLK-IDDC-TVS                           
098201     MOVE SEC-KDSVAR           TO OHLK-SEC-KDSVAR                         
098301     MOVE KREG-FLAUTORD        TO OHLK-FLAUTORD                           
098401     MOVE WS-IDFTG             TO OHLK-IDFTG                              
098501     MOVE WS-IDKONTO           TO OHLK-IDKONTO                            
098601*    IF REQU-IDKONTO = ALL '+'                                            
098701*       MOVE +0                TO OHLK-IDKONTO                            
098801*    ELSE                                                                 
098901*       MOVE REQU-IDKONTO      TO WS-ALFA-10                              
099001*       MOVE REQU-IDKONTO      TO WS-NUM-10                               
099101*       PERFORM S03-KONVERTERA-IDKONTO                                    
099201*       MOVE WS-NUM-10         TO OHLK-IDKONTO                            
099301*    END-IF                                                               
099401     MOVE WS-IDANALYS          TO OHLK-IDANALYS                           
099501*    IF REQU-IDANALYS = ALL '+'                                           
099601*       MOVE SPACE             TO OHLK-IDANALYS                           
099701*    ELSE                                                                 
099801*       MOVE REQU-IDANALYS     TO OHLK-IDANALYS                           
099901*    END-IF                                                               
100001     MOVE WS-IDKST             TO OHLK-IDKST                              
100101*    IF REQU-IDKST = ALL '+'                                              
100201*       MOVE +0                TO OHLK-IDKST                              
100301*    ELSE                                                                 
100401*       MOVE REQU-IDKST        TO WS-ALFA-5                               
100501*       MOVE WS-NUM-5          TO OHLK-IDKST                              
100601*    END-IF                                                               
100701     EJECT                                                                
100801     MOVE +0                   TO OHLK-IDKAMPRF                           
100901     MOVE KREG-FLOKFAK-G       TO OHLK-FLOKFAK-G                          
101001     MOVE KREG-FLOKFAK-N       TO OHLK-FLOKFAK-N                          
101101     MOVE KREG-FLOKFAK-R       TO OHLK-FLOKFAK-R                          
101201     MOVE KREG-FLOKFAK-K       TO OHLK-FLOKFAK-K                          
101301     MOVE JA                   TO OHLK-FLORDSPE                           
101401     MOVE NEJ                  TO OHLK-FLVORKO                            
101501     MOVE KREG-KDGENFAK        TO OHLK-KDFAKTYP                           
101601     MOVE +0                   TO OHLK-KDTPOTYP                           
101701     MOVE +0                   TO OHLK-TITPO                              
101801     CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB ORDN-XXKP-PCB        
101901                                       KREG-GMTA-PCB                      
102001                                       SAP-SAPC-PCB                       
102101                                       WDB6-PCB                           
102201                                                                          
102301     PERFORM BDA-KOLLA-FEL-LK                                             
102401     MOVE 'END BD-KONTROLLERA-LOGISKA-FEL' TO PGM-POS                     
102501     .                                                                    
102601     EJECT                                                                
102701 BDA-KOLLA-FEL-LK SECTION.                                                
102801     MOVE 'STA BDA-KOLLA-FEL-LK          ' TO PGM-POS                     
102901                                                                          
103001     IF OHLK-IDDISTR-OK = NEJ                                             
103101        MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                     
103201        MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                     
103301        MOVE NEJ                 TO ALLT-SW                               
103401     END-IF                                                               
103501     IF ALLT-OK                                                           
103601       IF OHLK-IDORDNR-OK = NEJ                                           
103701          MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR                   
103801          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
103901          MOVE NEJ                 TO ALLT-SW                             
104001       END-IF                                                             
104101     END-IF                                                               
104201     IF ALLT-OK                                                           
104301       IF OHLK-KDORDKL-OK = NEJ                                           
104401          MOVE 'KDORDKL'           TO RESP-IDELMT-ERROR                   
104501          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
104601          MOVE NEJ                 TO ALLT-SW                             
104701       END-IF                                                             
104801     END-IF                                                               
104901     IF ALLT-OK                                                           
105001       IF OHLK-IDFTG-OK = NEJ                                             
105101          MOVE 'IDFTG'             TO RESP-IDELMT-ERROR                   
105201          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
105301          MOVE NEJ                 TO ALLT-SW                             
105401       END-IF                                                             
105501     END-IF                                                               
105601                                                                          
105701     IF ALLT-OK                                                           
105801       IF OHLK-IDKONTO-OK = NEJ                                           
105901          MOVE 'IDKONTO'           TO RESP-IDELMT-ERROR                   
106001          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
106101          MOVE NEJ                 TO ALLT-SW                             
106201       END-IF                                                             
106301     END-IF                                                               
106401     IF ALLT-OK                                                           
106501       IF OHLK-IDANALYS-OK = NEJ                                          
106601          MOVE 'IDANALYS'          TO RESP-IDELMT-ERROR                   
106701          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
106801          MOVE NEJ                 TO ALLT-SW                             
106901       END-IF                                                             
107001     END-IF                                                               
107101     IF ALLT-OK                                                           
107201       IF OHLK-IDKST-OK = NEJ                                             
107301          MOVE 'IDKST'             TO RESP-IDELMT-ERROR                   
107401          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
107501          MOVE NEJ                 TO ALLT-SW                             
107601       END-IF                                                             
107701     END-IF                                                               
107801     IF ALLT-OK                                                           
107901       IF OHLK-KDFAKTYP-OK = NEJ                                          
108001          MOVE 'KDFAKTYP'          TO RESP-IDELMT-ERROR                   
108101          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
108201          MOVE NEJ                 TO ALLT-SW                             
108301       END-IF                                                             
108401     END-IF                                                               
108501     IF ALLT-OK                                                           
108601       IF OHLK-IDDC-TVS-OK = NEJ                                          
108701          MOVE 'IDDC'           TO RESP-IDELMT-ERROR                      
108801          MOVE ERR-FIELD-IS-INVALID TO RESP-IDMSG-ERROR                   
108901          MOVE NEJ                 TO ALLT-SW                             
109001       END-IF                                                             
109101     END-IF                                                               
109201     MOVE 'END BDA-KOLLA-FEL-LK          ' TO PGM-POS                     
109301     .                                                                    
109401     EJECT                                                                
109501 C-BESTAM-ORDERNUMMER SECTION.                                            
109601     MOVE 'STA C-BESTAM-ORDERNUMMER      ' TO PGM-POS                     
109701                                                                          
109801     MOVE 'IMS '               TO ORDN-IDSYSTEM                           
109901                                                                          
110001     MOVE W-IDDISTR            TO ORDN-IDDISTR                            
110101     MOVE W-IDKUNDNR           TO ORDN-IDKUNDNR                           
110201                                                                          
110301     MOVE ZERO                 TO ORDN-IDORDNR-IN                         
110401                                                                          
110501     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB                      
110601                         ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB        
110701                                                                          
110801     MOVE ORDN-IDORDNR-UT           TO W-IDORDNR                          
110901                                       RESP-IDORDNR-KEY                   
111001     MOVE 'END C-BESTAM-ORDERNUMMER      ' TO PGM-POS                     
111101     .                                                                    
111201     EJECT                                                                
111301 D-SKAPA-ORDERHUVUD SECTION.                                              
111401     MOVE 'STA D-SKAPA-ORDERHUVUD        ' TO PGM-POS                     
111501                                                                          
111601     PERFORM DA-REDIGERA-OHUV                                             
111701     PERFORM IMS-ISRT-ORQI-WDQ201                                         
111801                                                                          
111901     PERFORM DB-REDIGERA-ARBETSTABELL                                     
112001     MOVE ARB-KDFRAKT   TO RESP-KDFRAKT-KEY                               
112101     PERFORM IMS-ISRT-ORQI-WDQ212                                         
112201     MOVE 'END D-SKAPA-ORDERHUVUD        ' TO PGM-POS                     
112301     .                                                                    
112401     EJECT                                                                
112501 DA-REDIGERA-OHUV SECTION.                                                
112601     MOVE 'STA DA-REDIGERA-OHUV          ' TO PGM-POS                     
112701                                                                          
112801     MOVE KREG-IDDEPOT             TO OHUV-IDDEPOT                        
112901     MOVE KREG-IDROUTE             TO OHUV-IDROUTE                        
113001     MOVE KREG-IDZON               TO OHUV-IDZON                          
113101                                                                          
113201     MOVE '4231'                   TO OHUV-IDSYSTEM                       
113301     MOVE OHLK-IDDISTR             TO OHUV-IDDISTR                        
113401                                      RESP-IDDISTR-KEY                    
113501     MOVE OHLK-IDKUNDNR            TO OHUV-IDKUNDNR                       
113601                                      RESP-IDKUNDNR-KEY                   
113701     MOVE W-IDKUNDRF               TO OHUV-IDKUNDRF                       
113801     MOVE REQU-IDDC-KEY            TO OHUV-IDDC-TVS                       
113901     MOVE ORDN-IDORDER-UT          TO OHUV-IDORDER                        
114001     MOVE OHLK-KDORDKL             TO OHUV-KDORDKL                        
114101                                      RESP-KDORDKL-KEY                    
114201     MOVE REQU-IDUSER              TO OHUV-IDUSER                         
114301     MOVE KREG-ADBETRAD-1          TO OHUV-ADBETRAD-1                     
114401     MOVE KREG-ADBETRAD-2          TO OHUV-ADBETRAD-2                     
114501     MOVE KREG-BEBETRAD-1          TO OHUV-BEBETRAD-1                     
114601     MOVE KREG-BEBETRAD-2          TO OHUV-BEBETRAD-2                     
114701     MOVE KREG-ADGMT               TO OHUV-ADGMT                          
114801     MOVE KREG-BEGMT               TO OHUV-BEGMT                          
114901     MOVE SPACE                    TO OHUV-BEVARREF                       
115001     MOVE SPACE                    TO OHUV-BEKUNDRF                       
115101     MOVE ZERO                     TO OHUV-IDDEPT                         
115201     MOVE SPACE                    TO OHUV-BELAGINS-DEL1                  
115301     MOVE SPACE                    TO OHUV-BELAGINS-DEL2                  
115401     MOVE JA                       TO OHUV-FLAUTFAK                       
115501                                                                          
115601     MOVE NEJ                      TO OHUV-FLFORBI                        
115701     MOVE KREG-FLPRELRO            TO OHUV-FLPRELRO                       
115801     MOVE KREG-FLPRERS             TO OHUV-FLPRERS                        
115901     MOVE KREG-KVDAGAR-DOW         TO OHUV-KVDAGAR-DOW                    
116001     MOVE KREG-RESLATT             TO OHUV-RESLATT                        
116101     MOVE NEJ                      TO OHUV-FLKLAR                         
116201     MOVE JA                       TO OHUV-FLLSBOK                        
116301                                                                          
116401     MOVE JA                       TO OHUV-FLORDSPE                       
116501                                                                          
116601     MOVE JA                       TO OHUV-FLAUTPAC                       
116701*TL OM BILLIT-FAKTURERING OCH FLAUTPAC = J SÅ SKALL                       
116801*   FLAUTFAK = J. ANNARS FÅR KOLLIT INGET TRP-NR                          
116901     MOVE JA                       TO OHUV-FLAUTFAK                       
117001*                                                                         
117101                                                                          
117201     IF DIST20-EMBALLAGE                                                  
117301        MOVE JA                    TO OHUV-FLEMBORD                       
117401     ELSE                                                                 
117501        MOVE NEJ                   TO OHUV-FLEMBORD                       
117601     END-IF                                                               
117701     MOVE NEJ                      TO OHUV-FLOVRLEV                       
117801     MOVE NEJ                      TO OHUV-FLRESTN                        
117901     MOVE NEJ                      TO OHUV-FLVORKO                        
118001     MOVE KREG-IDRFTAB             TO OHUV-IDRFTAB                        
118101     MOVE KREG-IDDC                TO OHUV-IDDC-PRIM                      
118501     MOVE +0                       TO OHUV-IDKAMPRF                       
118601     MOVE SPACE                    TO OHUV-IDBIPREF                       
118701     MOVE OHLK-IDFTG               TO OHUV-IDFTG                          
118801     MOVE OHLK-IDKONTO             TO OHUV-IDKONTO                        
118901     MOVE OHLK-IDANALYS            TO OHUV-IDANALYS                       
119001     MOVE OHLK-IDKST               TO OHUV-IDKST                          
119101     MOVE KREG-IDSKYLT             TO OHUV-IDSKYLT                        
119201     MOVE KREG-KDGENFAK            TO OHUV-KDFAKTYP                       
119301     MOVE KREG-KDORDING            TO OHUV-KDORDING                       
119401     MOVE ZERO                     TO OHUV-KDTPOTYP                       
119501     MOVE KREG-KDTULLVE            TO OHUV-KDTULLVE                       
119601     MOVE +0                       TO OHUV-KDVRINFO                       
119701     MOVE DAGENS-DATUM             TO OHUV-TIREGDAT                       
119801     MOVE DAGENS-TID(1:4)          TO WS-TIHHMM                           
119901     MOVE DAGENS-TID(1:6)          TO WS-TIHHMMSS(1:6)                    
120001     MOVE WS-TIHHMMSS              TO OHUV-TIREGTID                       
120101     MOVE +0                       TO OHUV-TIREGDAT-STO                   
120201     MOVE +0                       TO OHUV-TIREGTID-STO                   
120301     MOVE +0                       TO OHUV-TITPO                          
120401     MOVE JA                       TO OHUV-FLOBTRAN                       
120501     MOVE NEJ                      TO OHUV-FLBORT                         
120601     MOVE SPACE                    TO OHUV-KDORDTYP-LDC                   
120701     MOVE SPACE                    TO OHUV-IDLEVNR-EJLS                   
120801     MOVE ZERO                     TO OHUV-TIREPDAT                       
120901     MOVE NEJ                      TO OHUV-FLORDTIL                       
121001     MOVE NEJ                      TO OHUV-FLSOFT                         
121101     MOVE NEJ                      TO OHUV-FLVORFK                        
121201     MOVE ZERO                     TO OHUV-KVORDTIL                       
121401                                      OHUV-IDGROSS                        
121501     MOVE SPACE                    TO OHUV-IDBILREG                       
121601                                      OHUV-IDVIN                          
121701                                      OHUV-IDCISNR                        
121801                                                                          
121901     MOVE 'END DA-REDIGERA-OHUV          ' TO PGM-POS                     
122001     .                                                                    
122101     EJECT                                                                
122201 DB-REDIGERA-ARBETSTABELL SECTION.                                        
122301     MOVE 'STA DB-REDIGERA-ARBETSTABELL  ' TO PGM-POS                     
122401                                                                          
122501     MOVE KREG-IDDC            TO ARB-IDDC                                
122601     MOVE KREG-BEGMRK          TO ARB-BEGMRK                              
122701     MOVE NEJ                  TO ARB-FLODELUT                            
122801     MOVE +0                   TO ARB-IDRADNR-SISTA                       
122901     MOVE ZERO                 TO ARB-IDTRP                               
123001                                  ARB-IDTRP-ALT                           
123101     MOVE +0                   TO ARB-IDPLKLST-SISTA                      
123201     MOVE KREG-KDFDKRAV        TO ARB-KDFDKRAV                            
123301     MOVE KREG-KDFRAKT         TO ARB-KDFRAKT                             
123401     MOVE +0                   TO ARB-KDROPACK                            
123501     MOVE KREG-KDTRPKAT        TO ARB-KDTRPKAT                            
123601     MOVE +0                   TO ARB-KVSEMBRA                            
123701     MOVE DAGENS-DATUM             TO WS-DATUM                            
123801     MOVE DAGENS-TID(1:4)          TO WS-TID                              
123901                                                                          
124001     MOVE WS-DATUM-TID         TO ARB-TIRFS                               
124101     MOVE ZERO                 TO ARB-DATRPAVD                            
124201     MOVE +0                   TO ARB-TIHHMM                              
124202     MOVE SPACE                TO ARB-KDORDSTA-O                          
124204     MOVE 'E'                  TO ARB-KDORDSTA                            
125601     MOVE 'END DB-REDIGERA-ARBETSTABELL  ' TO PGM-POS                     
125701     .                                                                    
125801     EJECT                                                                
125901                                                                          
126001*    --- DISPATCHER SECTIONS                                              
126101 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
126201     MOVE 'STA S01-FETCH-REQUEST-ARGUMEN'  TO PGM-POS                     
126301                                                                          
126401     MOVE 'GETARG'               TO SUB-KDFUNC                            
126501     MOVE 'CARPARTS.LDC.SPECORDERHEAD'     TO SUB-ADDISPABS               
126601     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
126701                                                                          
126801     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
126901                                                                          
127001     IF SUB-KDRC > 0                                                      
127101       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
127201       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
127301       DELIMITED BY SIZE INTO ERROR-TEXT                                  
127401       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
127501     END-IF                                                               
127601     MOVE 'END S01-FETCH-REQUEST-ARGUMEN'  TO PGM-POS                     
127701     .                                                                    
127801     SKIP3                                                                
127901 S02-RETURN-RESPONSE SECTION.                                             
128001     MOVE 'STA S02-RETURN-RESPONSE      '  TO PGM-POS                     
128101                                                                          
128201     MOVE 'RETURN'                   TO SUB-KDFUNC                        
128301     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
128401                                                                          
128501     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
128601                                                                          
128701     IF SUB-KDRC > 0                                                      
128801       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
128901       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
129001       DELIMITED BY SIZE INTO ERROR-TEXT                                  
129101       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
129201     END-IF                                                               
129301     MOVE 'END S02-RETURN-RESPONSE      '  TO PGM-POS                     
129401     .                                                                    
129501     EJECT                                                                
129601*S03-KONVERTERA-IDKONTO SECTION.                                          
129701*    MOVE ZERO TO TALLY                                                   
129801*    INSPECT REQU-IDKONTO TALLYING TALLY                                  
129901*                 FOR CHARACTERS BEFORE INITIAL SPACE                     
130001*    IF TALLY = ZERO                                                      
130101*      MOVE ZERO TO WS-NUM-10                                             
130201*    ELSE                                                                 
130301*      MOVE REQU-IDKONTO(1:TALLY) TO WS-NUM-10                            
130401*    END-IF                                                               
130501*    .                                                                    
130601*    EJECT                                                                
130701* --- IMS SEKTIONER ---                                                   
130801 IMS-ISRT-ORQI-WDQ201 SECTION.                                            
130901     MOVE 'STA IMS-ISRT-ORQI-WDQ201     '  TO PGM-POS                     
131001                                                                          
131101     MOVE 'WLORQI01 '          TO SSA1                                    
131201     MOVE '    '               TO GODK-STATUSKODER                        
131301     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-OHUV SSA1               
131401     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
131501     PERFORM IMS-STATUSKONTROLL                                           
131601     .                                                                    
131701     SKIP3                                                                
131801 IMS-ISRT-ORQI-WDQ212 SECTION.                                            
131901     MOVE 'STA IMS-ISRT-ORQI-WDQ212     '  TO PGM-POS                     
132001                                                                          
132101     MOVE 'WLORQI12 '          TO SSA1                                    
132201     MOVE '    '               TO GODK-STATUSKODER                        
132301     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-ARB SSA1                
132401     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
132501     PERFORM IMS-STATUSKONTROLL                                           
132601     .                                                                    
132701     SKIP3                                                                
132801                                                                          
132901 IMS-GU-WDB601    SECTION.                                                
133001     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
133101          DELIMITED BY SIZE INTO SSA1                                     
133201     MOVE '  ' TO GODK-STATUSKODER                                        
133301     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
133401     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
133501     PERFORM IMS-STATUSKONTROLL                                           
133601     .                                                                    
133701     SKIP3                                                                
133801                                                                          
133901 IMS-STATUSKONTROLL SECTION.                                              
134001                                                                          
134101     SET STATUS-IX TO 1                                                   
134201     SEARCH GODK-STATUS                                                   
134301       AT END CALL FELLOG                                                 
134401       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
134501     END-SEARCH                                                           
135000     .                                                                    
