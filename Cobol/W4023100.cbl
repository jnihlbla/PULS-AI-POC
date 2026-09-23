000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4023100.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   DEC  -90.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR UPPLÄGGNING AV SPECIAL-ORDERHUVUD I          
001100*        ORDERKÖN. REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA            
001200*        HÄMTAS FRÅN KUNDREGISTRET.                                       
001300*        EFTER UPPLÄGGNING AV GODKÄNT ORDERHUVUD SKER UTHOPP TILL         
001400*        REGISTRERING AV ORDERRADER BILD - 4232.                          
001500*                                                                         
001600*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
001700*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)  ORDERHUVUD                  
001800*                   LÄSER      WLBETC (WDB1)  KUNDREGISTER                
001900*                   LÄSER      WLGMTA (WDB2)  KUNDREGISTER                
002000*                   LÄSER      WLGMTB (WDB3)  KUNDREGISTER                
002100*                   LÄSER      WLGMTC (WDB5)  KUNDREGISTER                
002200*                   LÄSER      WDF101         LEVERANTÖRREGISTER          
002300*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)  ORDERNUMMERREGISTER         
002400*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
002500*                   LÄSER      WDB6           DC-REGISTER                 
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T231                                              
002900*        MID:         W4I23101                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O23101                                            
003300*                                                                         
003400*    ÄNDRINGAR:                                                           
003500*        2008-02-XX  ETRACKER 5174148.  DIRECT ORDER SHOULD UPDATE        
003600*                THE SUPPLIER GIVEN IN NEW INPUT FIELD.                   
003700*                                                                         
003800*                                                                         
003900     EJECT                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)   VALUE 'W4023100'.             
004700 77  JA                          PIC X(1)   VALUE 'J'.                    
004800 77  YES                         PIC X(1)   VALUE 'Y'.                    
004900 77  NEJ                         PIC X(1)   VALUE 'N'.                    
005000 77  OBEHORIG                    PIC X(1)   VALUE 'F'.                    
005200 77  HOPP                        PIC X(1)   VALUE 'N'.                    
005300                                                                          
005400 77  WS-ERROR                    PIC X(8)   VALUE SPACE.                  
005500                                                                          
005600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005700     88  ALLT-OK                             VALUE 'J'.                   
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '4231'.                
006100                                                                          
006200 01  WS-DATUM-TID                PIC 9(10)  VALUE ZERO.                   
006300 01  FILLER REDEFINES WS-DATUM-TID.                                       
006400     03  WS-DATUM                PIC 9(6).                                
006500     03  WS-TID                  PIC 9(4).                                
006600 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES WS-TIHHMMSS.                                        
006800     03  WS-TIHHMM               PIC 9(4).                                
006900     03  FILLER                  PIC 9(2).                                
007000     EJECT                                                                
007100 01  WS-ALFA-4.                                                           
007200     03  WS-NUM-4                PIC 9(4).                                
007300 01  WS-ALFA-5.                                                           
007400     03  WS-NUM-5                PIC 9(5).                                
007500 01  WS-ALFA-6.                                                           
007600     03  WS-NUM-6                PIC 9(6).                                
007700 01  WS-ALFA-10.                                                          
007800     03  WS-NUM-10               PIC 9(10).                               
007900     EJECT                                                                
008000                                                                          
008100 01  WS-TIREGDAT-9KOMPL          PIC 9(9)    VALUE ZERO.                  
008200                                                                          
008300 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
008400*                                                                         
008500 01  FILLER REDEFINES TEST-IDDISTR.                                       
008600*    03 -COPY WWDIST03                                                    
008700     EJECT                                                                
008800 01  FILLER REDEFINES TEST-IDDISTR.                                       
008900*    03 -COPY WWDIST20                                                    
009000 01  FILLER REDEFINES TEST-IDDISTR.                                       
009100*    ---DISTR-DEALER-PRICE-----                                           
009200*    03 -COPY WWDIST79                                                    
009300     EJECT                                                                
009400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009500 01  GENERELLA-SUBPROGRAM.                                                
009600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
010000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010100*                                                                         
010200 01  GEMENSAMMA-SUBPROGRAM.                                               
010300     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
010400*        FORMELLA KONTROLLER                                              
010500     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
010600*        LÄSNING AV KUNDREGISTRET                                         
010700     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
010800*        LOGISKA KONTROLLER                                               
010900     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
011000*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
011100*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
011200*   -COPY WSECAREA                                                        
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011500*   -COPY WMSGINIT                                                        
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011800*   -COPY WMEDAREA                                                        
011900     EJECT                                                                
012000 01  MESSAGE-CODES.                                                       
012100     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
012200     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '065'.                 
012300     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
012400     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
012500     03  ERR-LEVNR-FEL           PIC X(3)    VALUE '092'.                 
012600     03  ERR-SAKNAS-BET          PIC X(3)    VALUE '145'.                 
012700     03  ERR-FAELT-FEL           PIC X(3)    VALUE '194'.                 
012800     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
012900     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
013000     03  ERR-LEVNR-KRAEVS        PIC X(3)    VALUE '273'.                 
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
013300*   -COPY W411OHFK                                                        
013400     EJECT                                                                
013500*   -COPY W411KREG                                                        
013600     EJECT                                                                
013700*   -COPY W411OHLK                                                        
013800     EJECT                                                                
013900*   -COPY W411ORDN                                                        
014000     EJECT                                                                
014100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014300     SKIP3                                                                
014400*01  MID -COPY W4I23101                                                   
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014700     SKIP3                                                                
014800*01  -COPY WMSGAREA                                                       
014900     EJECT                                                                
015000*    03  MOD -COPY W4O23101   -RED MSG-AREA.                              
015100     EJECT                                                                
015200*    03  -COPY W4O23201 -PRE 4232-  -RED MSG-AREA.                        
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015500     SKIP3                                                                
015600*01  -COPY WMFSAREA                                                       
015700     EJECT                                                                
015800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000 01  NYCKLAR-TILL-DLI.                                                    
016100     03  W-WDQ2CSEQ-X.                                                    
016200         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
016300         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
016400         05  W-IDKUNDRF.                                                  
016500           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
016600           07  FILLER            PIC X(3)    VALUE SPACE.                 
016700*                                                                         
016800     03  W-IDGMT-X.                                                       
016900         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
017000         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
017100*                                                                         
017200     03  W-IDGMT-MIN-X.                                                   
017300         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
017400         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
017500*                                                                         
017600     03  W-IDGMT-MAX-X.                                                   
017700         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
017800         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
017900                                                                          
018000     03  W-IDDC-B6-X.                                                     
018100         05 W-IDDC-B6                  PIC X(2).                          
018200                                                                          
018300     03  W-IDLEVNR-X.                                                     
018400         05 W-IDLEVNR                  PIC X(5).                          
018500     EJECT                                                                
018600                                                                          
018700 77  WS-IDLEVNR             PIC X(5).                                     
018800     88  WS-IDLEVNR-999X              VALUES ARE '9990 '                  
018900                                                 '9991 '                  
019000                                                 '9992 '                  
019100                                                 '9993 '                  
019200                                                 '9994 '                  
019300                                                 '9995 '                  
019400                                                 '9996 '                  
019500                                                 '9997 '                  
019600                                                 '9998 '                  
019700                                                 '9999 '.                 
019800     EJECT                                                                
019900*    --- STATUS-KOD FRÅN IMS                                              
020000 01  STATUS-WS                   PIC XX.                                  
020100     88  SEGMENT-FINNS                       VALUE '  '.                  
020200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020300     SKIP2                                                                
020400 01  GODK-STATUSKODER.                                                    
020500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020600     SKIP3                                                                
020700 01  SSA1                        PIC X(64).                               
020800     EJECT                                                                
020900*    --- IMS FUNKTIONSKODER                                               
021000*01  -COPY W0003                                                          
021100     EJECT                                                                
021200*    ---  DLI INPUT-OUTPUT AREA                                           
021300     SKIP3                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
021500 01  DLI-IO-AREA-OHUV.                                                    
021600     03  WLORQI01.                                                        
021700*        05  -COPY WDQ201                                                 
021800     EJECT                                                                
021900 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
022000 01  DLI-IO-AREA-ARB.                                                     
022100     03  WLORQI12.                                                        
022200*        05  -COPY WDQ212                                                 
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
022500 01  DLI-IO-AREA-WDB201.                                                  
022600     03  WLGMTA01.                                                        
022700*        05  -COPY WDB201                                                 
022800                                                                          
022900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
023000 01   DLI-IO-AREA-B601.                                                   
023100*     03  -COPY WDB601                                                    
023200     EJECT                                                                
023300                                                                          
023400 01  FILLER               PIC X(16)   VALUE 'WDF101 AREA'.                
023500 01  DLI-IO-AREA-WDF101.                                                  
023600*     03  -COPY WDF101                                                    
023700     EJECT                                                                
023800*---MSG-AREA FÖR HOPP TILL 4232-UPPLÄGG AV ORDERRADER I ORDERKÖN          
023900 01  FILLER                PIC X(16)  VALUE '4232-MSG-IO-AREA'.           
024000 01  4232-MSG-IO-AREA.                                                    
024100     03  4232-LL              PIC S9(4)  VALUE +791 COMP SYNC.            
024200     03  4232-Z1              PIC X.                                      
024300     03  4232-Z2              PIC X.                                      
024400     03  4232-TRANSKOD        PIC X(8)   VALUE 'W4T232  '.                
024500     03  4232-IDTRANS         PIC X(4)   VALUE '4231'.                    
024600     03  4232-SPRAK           PIC X.                                      
024700     03  4232-DATA.                                                       
024800       05  4232-IDDISTR           PIC X(4).                               
024900       05  4232-IDKUNDNR          PIC X(6).                               
025000       05  4232-IDORDNR           PIC X(5).                               
025400       05  4232-KDORDKL           PIC X.                                  
025500       05  4232-KDFRAKT           PIC Z9.                                 
025600       05  4232-RESTEN            PIC X(756).                             
025700                                                                          
025800 LINKAGE SECTION.                                                         
025900*01  -COPY W0009   -PRE MSG-                                              
026000     EJECT                                                                
026100*01  -COPY W0009   -PRE 4232-                                             
026200                                                                          
026300*01  -COPY W0008   -PRE USEA-                                             
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008   -PRE ORQL-                                             
026700     05  FILLER                  PIC X.                                   
026800*01  -COPY W0008   -PRE ORQI-                                             
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100*01  -COPY W0008   -PRE WDB6-                                             
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008   -PRE WDB2-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008   -PRE WDF1-                                             
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000 01  KREG-GMTA-PCB               PIC X.                                   
028100 01  KREG-GMTB-PCB               PIC X.                                   
028200 01  KREG-GMTC-PCB               PIC X.                                   
028300 01  KREG-BETC-PCB               PIC X.                                   
028400 01  OHLK-WDM2-PCB               PIC X.                                   
028500 01  OHLK-WDB6-PCB               PIC X.                                   
028600 01  ORDN-XXKP-PCB               PIC X.                                   
028700 01  ORDN-ORQL-PCB               PIC X.                                   
028800 01  ORDN-PROC-PCB               PIC X.                                   
028900 01  ORDN-ORQI-PCB               PIC X.                                   
029000 01  SAP-SAPC-PCB                PIC X.                                   
029100     EJECT                                                                
029200 PROCEDURE DIVISION  USING MSG-PCB 4232-PCB USEA-PCB                      
029300                     ORQL-PCB ORQI-PCB WDB2-PCB WDB6-PCB                  
029400                     WDF1-PCB                                             
029500                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
029600                     KREG-BETC-PCB                                        
029700                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
029800                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
029900                     ORDN-ORQI-PCB                                        
030000                     SAP-SAPC-PCB.                                        
030100 MAIN SECTION.                                                            
030200     ENTRY 'DLITCBL' USING MSG-PCB 4232-PCB USEA-PCB                      
030300                     ORQL-PCB ORQI-PCB WDB2-PCB WDB6-PCB                  
030400                     WDF1-PCB                                             
030500                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
030600                     KREG-BETC-PCB                                        
030700                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
030800                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
030900                     ORDN-ORQI-PCB                                        
031000                     SAP-SAPC-PCB.                                        
031100                                                                          
031200     SKIP3                                                                
031300     PERFORM IMS-GET-MSG                                                  
031400     IF SEGMENT-FINNS                                                     
031500        PERFORM A-INIT                                                    
031600        IF ALLT-OK                                                        
031700           PERFORM B-KOLLA-O-KOMPLETTERA-INDATA                           
031800           IF ALLT-OK                                                     
031900              PERFORM C-BESTAM-ORDERNUMMER                                
032000              PERFORM D-SKAPA-ORDERHUVUD                                  
032100              PERFORM E-HOPPA-TILL-RADREGISTRERING                        
032200           END-IF                                                         
032300        END-IF                                                            
032400        IF HOPP = NEJ                                                     
032500           PERFORM Z-FINIT                                                
032600           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O23101 + 4                  
032700           PERFORM IMS-INSERT-MSG                                         
032800        END-IF                                                            
032900     END-IF                                                               
033000                                                                          
033100     MOVE +0 TO RETURN-CODE                                               
033200     GOBACK                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 A-INIT SECTION.                                                          
033600                                                                          
033700     MOVE SPACE                TO MED-IDMFSFEL                            
033800     MOVE JA                   TO ALLT-SW                                 
033900     MOVE NEJ                  TO HOPP                                    
034000                                                                          
034100     IF MSG-DUBBLA-TRANSKODER                                             
034200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I23101                 
034300       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
034400       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
034500     ELSE                                                                 
034600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I23101                  
034700       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
034800       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
034900     END-IF                                                               
035000                                                                          
035100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
035200     MOVE MSG-IDPFK            TO MFS-IDPFK                               
035300     MOVE MFS-IDTRANS          TO W-IDTRANS                               
035400                                                                          
035500     MOVE LOW-VALUE            TO MSG-AREA                                
035600     MOVE 'W4O23101'           TO MFS-IDMOD                               
035700     MOVE '4231'               TO MOD-IDTRANS                             
035800     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
035900                                                                          
036000     IF ENGLISH-TEXT                                                      
036100       MOVE 'GB '              TO MED-IDSKYLT                             
036200     ELSE                                                                 
036300       MOVE 'S  '              TO MED-IDSKYLT                             
036400     END-IF                                                               
036500     EJECT                                                                
036600     IF NOT EGEN-MID                                                      
036700     AND W-IDTRANS NOT = '4224'                                           
036800     AND W-IDTRANS NOT = '4225'                                           
036900        PERFORM MFS-RENSA-BILD                                            
037000        MOVE NEJ               TO ALLT-SW                                 
037100     ELSE                                                                 
037200        PERFORM AB-INITIERA-DIV-AREOR                                     
037300        PERFORM AA-KOLLA-BEHORIGHET                                       
037400     END-IF                                                               
037500     .                                                                    
037600     EJECT                                                                
037700                                                                          
037800 AA-KOLLA-BEHORIGHET SECTION.                                             
037900                                                                          
038000     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
038100        MOVE MSG-SIGNON-USERID    TO SEC-IDUSER                           
038200        MOVE '4231'               TO SEC-IDTRANS                          
038300        MOVE MID-IDDISTR          TO SEC-IDKEY                            
038400                                                                          
038500        CALL WSECURIT USING SEC-IDUSER                                    
038600                            SEC-IDTRANS                                   
038700                            SEC-IDKEY                                     
038800                            SEC-KDSVAR                                    
038900                                                                          
039000        IF SEC-KDSVAR = OBEHORIG                                          
039100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR                     
039200           MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                         
039300           MOVE NEJ               TO ALLT-SW                              
039400        END-IF                                                            
039500     ELSE                                                                 
039600        MOVE OBEHORIG             TO SEC-KDSVAR                           
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 AB-INITIERA-DIV-AREOR SECTION.                                           
040100                                                                          
040200     MOVE SPACE                TO MED-IDMFSFEL                            
040300                                  MOD-FLVORKO                             
040400     MOVE JA                   TO ALLT-SW                                 
040500     MOVE NEJ                  TO HOPP                                    
040600                                                                          
040700     INSPECT MID-IDDISTR  REPLACING LEADING SPACE BY ZERO                 
040800     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
040900     INSPECT MID-IDORDNR  REPLACING LEADING SPACE BY ZERO                 
041000                                                                          
041100     IF MID-FLVORKO = JA                                                  
041200     OR MID-FLVORKO = YES                                                 
041300       PERFORM S01-VISA-BILD                                              
041400       IF   ALLT-OK                                                       
041500       AND (W-IDTRANS = '4224'                                            
041600       OR   W-IDTRANS = '4225')                                           
041700         MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDDC-TVS-ATTR                   
041800         MOVE NEJ TO ALLT-SW                                              
041900       END-IF                                                             
042000     ELSE                                                                 
042100       IF  MID-FLVORKO NOT = JA                                           
042200       AND MID-FLVORKO NOT = YES                                          
042300       AND MID-IDDISTR NOT NUMERIC                                        
042400         PERFORM MFS-RENSA-BILD                                           
042500         MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                             
042600                                 MOD-TEMFSINF                             
042700         MOVE SPACE          TO MED-IDMFSFEL                              
042800         MOVE 'V' TO ALLT-SW                                              
042900       END-IF                                                             
043000                                                                          
043100       MOVE NEJ               TO MID-FLVORKO                              
043200                                                                          
043300     END-IF                                                               
043400                                                                          
043500     PERFORM S02-FLYTTA-VOR-RADER-TILL-MOD                                
043600     .                                                                    
043700     EJECT                                                                
043800 B-KOLLA-O-KOMPLETTERA-INDATA SECTION.                                    
043900                                                                          
044000     PERFORM BA-KONTROLLERA-FORMELLA-FEL                                  
044100     IF ALLT-OK                                                           
044200                                                                          
044300        PERFORM BB-LAS-KUNDREGISTRET                                      
044400        IF ALLT-OK                                                        
044500                                                                          
044600           PERFORM BC-KONTROLLERA-OM-ORDER-FINNS                          
044700           IF ALLT-OK                                                     
044800                                                                          
044900              PERFORM BD-KONTROLLERA-LOGISKA-FEL                          
045000              IF ALLT-OK                                                  
045100                                                                          
045200                PERFORM BE-FIXA-LOKAL-TID                                 
045300              END-IF                                                      
045400           END-IF                                                         
045500        END-IF                                                            
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900                                                                          
046000 BA-KONTROLLERA-FORMELLA-FEL SECTION.                                     
046100                                                                          
046200     MOVE 'IMS '               TO OHFK-IDSYSTEM                           
046300     MOVE MID-IDDISTR          TO OHFK-IDDISTR                            
046400     MOVE MID-IDKUNDNR         TO OHFK-IDKUNDNR                           
046500     MOVE MID-IDORDNR          TO OHFK-IDORDNR                            
046600     MOVE MID-KDORDKL          TO OHFK-KDORDKL                            
046700     MOVE MID-KDFRAKT          TO OHFK-KDFRAKT                            
046800     MOVE MID-FLAUTFAK         TO OHFK-FLAUTFAK                           
046900     MOVE NEJ                  TO OHFK-FLFORBI                            
047000     MOVE JA                   TO OHFK-FLORDSPE                           
047100     MOVE MID-FLLSBOK          TO OHFK-FLLSBOK                            
047200     MOVE MID-FLAUTPAC         TO OHFK-FLAUTPAC                           
047300     MOVE MID-FLVORKO          TO OHFK-FLVORKO                            
047400     MOVE NEJ                  TO OHFK-FLRESTN                            
047500     MOVE SPACE                TO OHFK-IDBIPREF                           
047600     MOVE MID-IDFTG            TO OHFK-IDFTG                              
047700     MOVE MID-IDKONTO          TO OHFK-IDKONTO                            
047800     MOVE MID-IDANALYS         TO OHFK-IDANALYS                           
047900     MOVE MID-IDKST            TO OHFK-IDKST                              
048000     MOVE ZERO                 TO OHFK-IDKAMPRF                           
048100     MOVE MID-IDDC-TVS         TO OHFK-IDDC                               
048200     MOVE MID-KDFAKTYP         TO OHFK-KDFAKTYP                           
048300     ACCEPT OHFK-TIREGDAT      FROM DATE                                  
048400     ACCEPT OHFK-TIHHMM        FROM TIME                                  
048500     MOVE ALL '+'              TO OHFK-KDROPACK                           
048600                                  OHFK-KDTPOTYP                           
048700                                  OHFK-TITPO                              
048800                                  OHFK-TIRFSDAT                           
048900                                  OHFK-TIRFSTID                           
049000                                  OHFK-IDSKYLT                            
049100                                  OHFK-KDTULLVE                           
049200                                  OHFK-KDVRINFO                           
049300                                  OHFK-TIFORDAT                           
049400                                  OHFK-KDPROTYP                           
049500                                                                          
049600     CALL W411OHFK USING OHFK-W411OHFK                                    
049700     PERFORM BAA-KOLLA-FEL-FK                                             
049800     .                                                                    
049900     EJECT                                                                
050000 BAA-KOLLA-FEL-FK SECTION.                                                
050100                                                                          
050120                                                                          
050200     IF OHFK-IDDISTR-OK = NEJ                                             
050300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
050400        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
050500        MOVE NEJ                 TO ALLT-SW                               
050600     ELSE                                                                 
050700        MOVE OHFK-IDDISTR        TO W-IDDISTR                             
050800                                    TEST-IDDISTR                          
050900     END-IF                                                               
051000                                                                          
051011     IF DIST79-ECOM-PRICE                                                 
051030        MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                           
051040        MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-ATTR                       
051050        MOVE NEJ                TO ALLT-SW                                
051060     END-IF                                                               
051070                                                                          
051100     IF DIST79-DEALER-PRICE                                               
051200        IF ENGLISH-TEXT                                                   
051300           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
051400        ELSE                                                              
051500           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
051600        END-IF                                                            
051700     ELSE                                                                 
051800        MOVE SPACES              TO MOD-TEDDI                             
051900     END-IF                                                               
052000                                                                          
052100     IF OHFK-IDKUNDNR-OK = NEJ                                            
052200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
052300        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
052400        MOVE NEJ                 TO ALLT-SW                               
052500     ELSE                                                                 
052600        IF MID-IDKUNDNR = ALL '+'                                         
052700           MOVE ZERO             TO MOD-IDKUNDNR                          
052800                                    W-IDKUNDNR                            
052900        ELSE                                                              
053000           MOVE OHFK-IDKUNDNR    TO W-IDKUNDNR                            
053100        END-IF                                                            
053200     END-IF                                                               
053300                                                                          
053400     IF OHFK-IDORDNR-OK = NEJ                                             
053500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
053600        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
053700        MOVE NEJ                 TO ALLT-SW                               
053800     ELSE                                                                 
053900        IF MID-IDORDNR = ALL '+'                                          
054000           MOVE MID-IDDC-TVS    TO W-IDDC-B6                              
054100           PERFORM IMS-GU-WDB601                                          
054200           IF MID-FLLSBOK = NEJ AND DCS-CDC                               
054300             MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                     
054400             MOVE ' LSBOK'  TO WS-ERROR                                   
054500*TEST                                                                     
054600             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                 
054700             MOVE NEJ                 TO ALLT-SW                          
054800           ELSE                                                           
054900             MOVE ZERO             TO W-IDORDNR                           
055000             MOVE MFS-RENSA-FAELT  TO MOD-IDORDNR                         
055100           END-IF                                                         
055200        ELSE                                                              
055300           MOVE OHFK-IDORDNR     TO W-IDORDNR                             
055400        END-IF                                                            
055500     END-IF                                                               
055600     EJECT                                                                
055700                                                                          
055800     IF OHFK-KDORDKL-OK = NEJ                                             
055900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
056000        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
056100        MOVE NEJ                 TO ALLT-SW                               
056200     END-IF                                                               
056300                                                                          
056400     IF OHFK-KDFRAKT-OK = NEJ                                             
056500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
056600        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
056700        MOVE NEJ                 TO ALLT-SW                               
056800     END-IF                                                               
056900                                                                          
057000     IF OHFK-FLLSBOK-OK = NEJ                                             
057100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
057200        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLLSBOK-ATTR                      
057300        MOVE NEJ                 TO ALLT-SW                               
057400     END-IF                                                               
057500                                                                          
057600     IF OHFK-FLAUTPAC-OK = NEJ                                            
057700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
057800        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLAUTPAC-ATTR                     
057900        MOVE NEJ                 TO ALLT-SW                               
058000     END-IF                                                               
058100                                                                          
058200     IF OHFK-FLAUTFAK-OK = NEJ                                            
058300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
058400        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLAUTFAK-ATTR                     
058500        MOVE NEJ                 TO ALLT-SW                               
058600     END-IF                                                               
058700                                                                          
058800     IF OHFK-IDFTG-OK = NEJ                                               
058900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
059000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
059100        MOVE NEJ                 TO ALLT-SW                               
059200     END-IF                                                               
059300     EJECT                                                                
059400                                                                          
059500     IF OHFK-IDKONTO-OK = NEJ                                             
059600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
059700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
059800        MOVE NEJ                 TO ALLT-SW                               
059900     END-IF                                                               
060000                                                                          
060100     IF OHFK-IDANALYS-OK = NEJ                                            
060200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
060300        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
060400        MOVE NEJ                 TO ALLT-SW                               
060500     END-IF                                                               
060600                                                                          
060700     IF OHFK-IDKST-OK = NEJ                                               
060800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
060900        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
061000        MOVE NEJ                 TO ALLT-SW                               
061100     END-IF                                                               
061200                                                                          
061300     IF OHFK-IDDC-OK = NEJ                                                
061400        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
061500        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-TVS-ATTR                     
061600        MOVE NEJ                 TO ALLT-SW                               
061700     END-IF                                                               
061800                                                                          
061900     IF OHFK-KDFAKTYP-OK = NEJ                                            
062000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
062100        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDFAKTYP-ATTR                     
062200        MOVE NEJ                 TO ALLT-SW                               
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 BB-LAS-KUNDREGISTRET SECTION.                                            
062700                                                                          
062800     MOVE W-IDDISTR            TO KREG-IDDISTR                            
062900     MOVE W-IDKUNDNR           TO KREG-IDKUNDNR                           
063000     IF (W-IDTRANS = '4224' AND MID-FLVORKO = YES)                        
063100     OR (W-IDTRANS = '4225' AND MID-FLVORKO = JA)                         
063200        MOVE '4224'            TO KREG-IDSYSTEM                           
063300     ELSE                                                                 
063400        MOVE 'IMS '            TO KREG-IDSYSTEM                           
063500     END-IF                                                               
063600     MOVE MID-IDDC-TVS         TO KREG-IDDC-TVS                           
063700     IF MID-KDFRAKT = ALL '+'                                             
063800        MOVE +0                TO KREG-KDFRAKT-IN                         
063900     ELSE                                                                 
064000        MOVE MID-KDFRAKT       TO KREG-KDFRAKT-IN                         
064100     END-IF                                                               
064200     MOVE MID-KDORDKL          TO KREG-KDORDKL                            
064300     IF MID-KDFAKTYP = ALL '+'                                            
064400        MOVE SPACE             TO KREG-KDFAKTYP-IN                        
064500     ELSE                                                                 
064600        MOVE MID-KDFAKTYP      TO KREG-KDFAKTYP-IN                        
064700     END-IF                                                               
064800                                                                          
064900     MOVE NEJ                  TO KREG-FLVORKO                            
065000                                                                          
065100     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB KREG-GMTB-PCB        
065200                                       KREG-GMTC-PCB KREG-BETC-PCB        
065300                                                                          
065400     IF KREG-KDKREDSP = '1'                                               
065500        MOVE ERR-KUND-SPAERRAD TO MED-IDMFSFEL                            
065600        MOVE NEJ                 TO ALLT-SW                               
065700     ELSE                                                                 
065800        IF KREG-IDVAT-OK = NEJ                                            
065900          MOVE ERR-MOMS-REGNR-FEL TO MED-IDMFSFEL                         
066000          MOVE NEJ                TO ALLT-SW                              
066100        ELSE                                                              
066200          IF KREG-IDPARTNR-OK = NEJ                                       
066300            MOVE ERR-SAKNAS-BET      TO MED-IDMFSFEL                      
066400            MOVE NEJ                 TO ALLT-SW                           
066500          END-IF                                                          
066600        END-IF                                                            
066700     END-IF                                                               
066800                                                                          
066900     PERFORM BBA-KOLLA-FEL-KREG                                           
067000     .                                                                    
067100     EJECT                                                                
067200 BBA-KOLLA-FEL-KREG SECTION.                                              
067300                                                                          
067400     IF KREG-IDDISTR-OK = NEJ                                             
067500        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
067600        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
067700        MOVE NEJ                 TO ALLT-SW                               
067800     END-IF                                                               
067900                                                                          
068000     IF KREG-IDKUNDNR-OK = NEJ                                            
068100        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
068200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
068300        MOVE NEJ                 TO ALLT-SW                               
068400     END-IF                                                               
068500                                                                          
068600     IF KREG-IDDC-OK = NEJ                                                
068700        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
068800        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-TVS-ATTR                     
068900        MOVE NEJ                 TO ALLT-SW                               
069000     END-IF                                                               
069100                                                                          
069200     IF KREG-KDFRAKT-OK = NEJ                                             
069300        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
069400        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
069500        MOVE NEJ                 TO ALLT-SW                               
069600     END-IF                                                               
069700     .                                                                    
069800     EJECT                                                                
069900 BC-KONTROLLERA-OM-ORDER-FINNS SECTION.                                   
070000                                                                          
070100     IF MID-IDORDNR NOT = ALL '+'                                         
070200                                                                          
070300        PERFORM IMS-GU-ORQL-WDQ201                                        
070400                                                                          
070500        IF SEGMENT-FINNS                                                  
070600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDORDNR-ATTR                     
070700           MOVE ERR-ORDER-FINNS   TO MED-IDMFSFEL                         
070800           MOVE NEJ               TO ALLT-SW                              
070900        END-IF                                                            
071000     END-IF                                                               
071100     .                                                                    
071200     EJECT                                                                
071300 BD-KONTROLLERA-LOGISKA-FEL SECTION.                                      
071400                                                                          
071500     MOVE 'IMS '               TO OHLK-IDSYSTEM                           
071600     MOVE W-IDDISTR            TO OHLK-IDDISTR                            
071700     MOVE W-IDKUNDNR           TO OHLK-IDKUNDNR                           
071800     MOVE W-IDORDNR            TO OHLK-IDORDNR                            
071900     MOVE MID-KDORDKL          TO OHLK-KDORDKL                            
072000     MOVE MID-IDDC-TVS         TO OHLK-IDDC                               
072100                                  OHLK-IDDC-TVS                           
072200     MOVE SEC-KDSVAR           TO OHLK-SEC-KDSVAR                         
072300     MOVE KREG-FLAUTORD        TO OHLK-FLAUTORD                           
072400     IF MID-IDFTG = ALL '+'                                               
072500        MOVE ZERO              TO OHLK-IDFTG                              
072600     ELSE                                                                 
072700        MOVE MID-IDFTG         TO OHLK-IDFTG                              
072800     END-IF                                                               
072900     IF MID-IDKONTO = ALL '+'                                             
073000        MOVE +0                TO OHLK-IDKONTO                            
073100     ELSE                                                                 
073200        MOVE MID-IDKONTO       TO WS-ALFA-10                              
073300        MOVE WS-NUM-10         TO OHLK-IDKONTO                            
073400     END-IF                                                               
073500     IF MID-IDANALYS = ALL '+'                                            
073600        MOVE SPACE             TO OHLK-IDANALYS                           
073700     ELSE                                                                 
073800        MOVE MID-IDANALYS      TO OHLK-IDANALYS                           
073900     END-IF                                                               
074000     IF MID-IDKST = ALL '+'                                               
074100        MOVE SPACE             TO OHLK-IDKST                              
074200     ELSE                                                                 
074300        MOVE MID-IDKST         TO OHLK-IDKST                              
074500     END-IF                                                               
074600     EJECT                                                                
074700     MOVE +0                   TO OHLK-IDKAMPRF                           
074800     MOVE KREG-FLOKFAK-G       TO OHLK-FLOKFAK-G                          
074900     MOVE KREG-FLOKFAK-N       TO OHLK-FLOKFAK-N                          
075000     MOVE KREG-FLOKFAK-R       TO OHLK-FLOKFAK-R                          
075100     MOVE KREG-FLOKFAK-K       TO OHLK-FLOKFAK-K                          
075200     MOVE JA                   TO OHLK-FLORDSPE                           
075300     MOVE MID-FLVORKO          TO OHLK-FLVORKO                            
075400                                                                          
075500     IF MID-KDFAKTYP = ALL '+'                                            
075600        MOVE KREG-KDGENFAK     TO OHLK-KDFAKTYP                           
075700     ELSE                                                                 
075800        MOVE MID-KDFAKTYP      TO OHLK-KDFAKTYP                           
075900     END-IF                                                               
076000                                                                          
076100     MOVE +0                   TO OHLK-KDTPOTYP                           
076200     MOVE +0                   TO OHLK-TITPO                              
076300                                                                          
076400     CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB ORDN-XXKP-PCB        
076500                                       KREG-GMTA-PCB                      
076600                                       SAP-SAPC-PCB                       
076700                                       OHLK-WDB6-PCB                      
076800                                                                          
076900     PERFORM BDA-KOLLA-FEL-LK                                             
077000     .                                                                    
077100     EJECT                                                                
077200 BDA-KOLLA-FEL-LK SECTION.                                                
077300                                                                          
077400     IF OHLK-IDDISTR-OK = NEJ                                             
077500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
077600        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
077700        MOVE NEJ                 TO ALLT-SW                               
077800     END-IF                                                               
077900                                                                          
078000     IF OHLK-IDORDNR-OK = NEJ                                             
078100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
078200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
078300        MOVE NEJ                 TO ALLT-SW                               
078400     END-IF                                                               
078500                                                                          
078600     IF OHLK-KDORDKL-OK = NEJ                                             
078700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
078800        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
078900        MOVE NEJ                 TO ALLT-SW                               
079000     END-IF                                                               
079100                                                                          
079200     IF OHLK-IDFTG-OK = NEJ                                               
079300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
079400        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
079500        MOVE NEJ                 TO ALLT-SW                               
079600     END-IF                                                               
079700     EJECT                                                                
079800                                                                          
079900     IF OHLK-IDKONTO-OK = NEJ                                             
080000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
080100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
080200        MOVE NEJ                 TO ALLT-SW                               
080300     END-IF                                                               
080400                                                                          
080500     IF OHLK-IDANALYS-OK = NEJ                                            
080600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
080700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
080800        MOVE NEJ                 TO ALLT-SW                               
080900     END-IF                                                               
081000                                                                          
081100     IF OHLK-IDKST-OK = NEJ                                               
081200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
081300        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
081400        MOVE NEJ                 TO ALLT-SW                               
081500     END-IF                                                               
081600                                                                          
081700     IF OHLK-KDFAKTYP-OK = NEJ                                            
081800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
081900        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDFAKTYP-ATTR                     
082000        MOVE NEJ                 TO ALLT-SW                               
082100     END-IF                                                               
082200                                                                          
082300     IF OHLK-IDDC-TVS-OK = NEJ                                            
082400*FIX LASSI FÖR ATT KOLLA HUR SOFTWARE FRÅN NDC:ER FUNKAR. NÄR ALLT        
082500*    SER BRA UT SKA BEKUNDRF LÄGGAS TILL I OHLK-CTX ISTÄLLER              
082600       IF MID-BEKUNDRF = 'SOFTWARE'                                       
082700          CONTINUE                                                        
082800       ELSE                                                               
082900*FIX LASSI SLUT ...PLUS EN END-IF NEDAN                                   
083000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
083100        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-TVS-ATTR                     
083200        MOVE NEJ                 TO ALLT-SW                               
083300       END-IF                                                             
083400     END-IF                                                               
083500                                                                          
083600* KOLLA INMATNING AV LEVNR FÖR DIREKT-ORDER SPECIAL. EJ AVBOK             
083700*                                          ETRACKER 5174148               
083800     IF MID-FLLSBOK = 'N'                                                 
083900       IF MID-BEKUNDRF = 'SOFTWARE'                                       
084000*        --- UNDANTAG FÖR REGELN ATT LEVNR KRÄVS VID AVBOK=N              
084100         CONTINUE                                                         
084200       ELSE                                                               
084300         IF MID-IDLEVNR-EJLS = ALL '+' OR SPACE                           
084400           MOVE ERR-LEVNR-KRAEVS TO MED-IDMFSFEL                          
084500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-EJLS-ATTR               
084600           MOVE NEJ              TO ALLT-SW                               
084700         ELSE                                                             
084800           MOVE MID-IDLEVNR-EJLS TO W-IDLEVNR                             
084900                                        WS-IDLEVNR                        
085000           PERFORM IMS-GET-WDF101-UNIK                                    
085100           IF SEGMENT-SAKNAS                                              
085200           OR WS-IDLEVNR-999X                                             
085300             MOVE ERR-LEVNR-FEL    TO MED-IDMFSFEL                        
085400             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-EJLS-ATTR             
085500             MOVE NEJ              TO ALLT-SW                             
085600           ELSE                                                           
085700             CONTINUE                                                     
085800           END-IF                                                         
085900         END-IF                                                           
086000       END-IF                                                             
086100     ELSE                                                                 
086200       IF MID-IDLEVNR-EJLS = ALL '+'       OR SPACE                       
086300         CONTINUE                                                         
086400       ELSE                                                               
086500         MOVE ERR-FAELT-FEL      TO MED-IDMFSFEL                          
086600         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-EJLS-ATTR                 
086700         MOVE NEJ                TO ALLT-SW                               
086800       END-IF                                                             
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 BE-FIXA-LOKAL-TID SECTION.                                               
087300                                                                          
087400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
087500     MOVE '001'             TO MSGI-KDCALL                                
087600     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
087700     MOVE MID-IDDC-TVS      TO MSGI-IDUSER (6:2)                          
087800     MOVE '4231'            TO MSGI-IDTRANS                               
087900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
088000                                                                          
088100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
088200     .                                                                    
088300     EJECT                                                                
088400 C-BESTAM-ORDERNUMMER SECTION.                                            
088500                                                                          
088600     MOVE 'IMS '               TO ORDN-IDSYSTEM                           
088700                                                                          
088800     MOVE W-IDDISTR            TO ORDN-IDDISTR                            
088900     MOVE W-IDKUNDNR           TO ORDN-IDKUNDNR                           
089000                                                                          
089100     IF MID-IDORDNR = ALL '+'                                             
089200        MOVE ZERO              TO ORDN-IDORDNR-IN                         
089300     ELSE                                                                 
089400        MOVE W-IDORDNR         TO ORDN-IDORDNR-IN                         
089500     END-IF                                                               
089600                                                                          
089700     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB                      
089800                         ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB        
089900                                                                          
090000     MOVE ORDN-IDORDNR-UT           TO W-IDORDNR                          
090100     .                                                                    
090200     EJECT                                                                
090300 D-SKAPA-ORDERHUVUD SECTION.                                              
090400                                                                          
090500     PERFORM DA-REDIGERA-OHUV                                             
090600     PERFORM IMS-ISRT-ORQI-WDQ201                                         
090700                                                                          
090800     PERFORM DB-REDIGERA-ARBETSTABELL                                     
090900     PERFORM IMS-ISRT-ORQI-WDQ212                                         
091000     .                                                                    
091100     EJECT                                                                
091200 DA-REDIGERA-OHUV SECTION.                                                
091300                                                                          
091400     MOVE W-IDDISTR              TO W-IDDISTR-WDB2                        
091500                                    W-IDDISTR-WDB2-MIN                    
091600                                    W-IDDISTR-WDB2-MAX                    
091700     MOVE W-IDKUNDNR             TO W-IDKUNDNR-WDB2                       
091800     PERFORM IMS-GET-WDB201-UNIK                                          
091900     IF SEGMENT-FINNS                                                     
092000        CONTINUE                                                          
092100     ELSE                                                                 
092200        PERFORM IMS-GU-WDB201                                             
092300     END-IF                                                               
092400     MOVE KREG-IDDEPOT             TO OHUV-IDDEPOT                        
092500     MOVE KREG-IDROUTE             TO OHUV-IDROUTE                        
092600     MOVE KREG-IDZON               TO OHUV-IDZON                          
092700                                                                          
092800     MOVE '4231'                   TO OHUV-IDSYSTEM                       
092900     MOVE OHLK-IDDISTR             TO OHUV-IDDISTR                        
093000     MOVE OHLK-IDKUNDNR            TO OHUV-IDKUNDNR                       
093100     MOVE W-IDKUNDRF               TO OHUV-IDKUNDRF                       
093200     MOVE MID-IDDC-TVS             TO OHUV-IDDC-TVS                       
093300     MOVE ORDN-IDORDER-UT          TO OHUV-IDORDER                        
093400     MOVE OHLK-KDORDKL             TO OHUV-KDORDKL                        
093500     MOVE MSG-SIGNON-USERID        TO OHUV-IDUSER                         
093600     MOVE KREG-ADBETRAD-1          TO OHUV-ADBETRAD-1                     
093700     MOVE KREG-ADBETRAD-2          TO OHUV-ADBETRAD-2                     
093800     MOVE KREG-BEBETRAD-1          TO OHUV-BEBETRAD-1                     
093900     MOVE KREG-BEBETRAD-2          TO OHUV-BEBETRAD-2                     
094000     IF MID-ADGMT = ALL '+'                                               
094100        MOVE KREG-ADGMT            TO OHUV-ADGMT                          
094200     ELSE                                                                 
094300        IF MID-ADGMT-GATA = ALL '+'                                       
094400           MOVE SPACE              TO OHUV-ADGMT-GATA                     
094500        ELSE                                                              
094600           MOVE MID-ADGMT-GATA     TO OHUV-ADGMT-GATA                     
094700        END-IF                                                            
094800        IF MID-ADGMT-PADR = ALL '+'                                       
094900           MOVE SPACE              TO OHUV-ADGMT-PADR                     
095000        ELSE                                                              
095100           MOVE MID-ADGMT-PADR     TO OHUV-ADGMT-PADR                     
095200        END-IF                                                            
095300        IF MID-ADGMT-LAND = ALL '+'                                       
095400           MOVE SPACE              TO OHUV-ADGMT-LAND                     
095500        ELSE                                                              
095600           MOVE MID-ADGMT-LAND     TO OHUV-ADGMT-LAND                     
095700        END-IF                                                            
095800        INSPECT OHUV-ADGMT REPLACING ALL '+' BY SPACE                     
095900     END-IF                                                               
096000     IF MID-BEGMT = ALL '+'                                               
096100        MOVE KREG-BEGMT            TO OHUV-BEGMT                          
096200     ELSE                                                                 
096300        MOVE MID-BEGMT             TO OHUV-BEGMT                          
096400        INSPECT OHUV-BEGMT REPLACING ALL '+' BY SPACE                     
096500     END-IF                                                               
096600     IF MID-BEVARREF = ALL '+'                                            
096700        MOVE SPACE                 TO OHUV-BEVARREF                       
096800     ELSE                                                                 
096900        MOVE MID-BEVARREF          TO OHUV-BEVARREF                       
097000     END-IF                                                               
097100     IF MID-BEKUNDRF = ALL '+'                                            
097200        MOVE SPACE                 TO OHUV-BEKUNDRF                       
097300     ELSE                                                                 
097400        MOVE MID-BEKUNDRF          TO OHUV-BEKUNDRF                       
097500     END-IF                                                               
097600     MOVE ZERO                     TO OHUV-IDDEPT                         
097700     MOVE SPACE                    TO OHUV-BELAGINS-DEL1                  
097800     MOVE SPACE                    TO OHUV-BELAGINS-DEL2                  
097900     IF MID-FLAUTFAK = '+'                                                
098000        MOVE JA                    TO OHUV-FLAUTFAK                       
098100     ELSE                                                                 
098200        MOVE MID-FLAUTFAK          TO OHUV-FLAUTFAK                       
098300        IF OHUV-FLAUTFAK = 'Y'                                            
098400           MOVE JA                 TO OHUV-FLAUTFAK                       
098500        END-IF                                                            
098600     END-IF                                                               
098700     IF DIST03-SVERIGE-EJ-778                                             
098800        MOVE JA                    TO OHUV-FLAUTFAK                       
099500        MOVE MID-IDDC-TVS          TO W-IDDC-B6                           
099600        PERFORM IMS-GU-WDB601                                             
099700        IF DIST03-AS AND DCS-SDC AND DCS-IDLANDX2 = 'NL'                  
099800         MOVE JA                   TO OHUV-FLAUTFAK                       
099900        END-IF                                                            
100000     END-IF                                                               
100100                                                                          
100200     MOVE NEJ                      TO OHUV-FLFORBI                        
100300     MOVE KREG-FLPRELRO            TO OHUV-FLPRELRO                       
100400     MOVE KREG-FLPRERS             TO OHUV-FLPRERS                        
100500     MOVE KREG-KVDAGAR-DOW         TO OHUV-KVDAGAR-DOW                    
100600     MOVE KREG-RESLATT             TO OHUV-RESLATT                        
100700     MOVE NEJ                      TO OHUV-FLKLAR                         
100800                                                                          
100900     IF MID-FLLSBOK = ALL '+'                                             
101000        MOVE JA                    TO OHUV-FLLSBOK                        
101100     ELSE                                                                 
101200        IF MID-FLLSBOK = 'Y'                                              
101300           MOVE JA                 TO OHUV-FLLSBOK                        
101400        ELSE                                                              
101500           MOVE MID-FLLSBOK        TO OHUV-FLLSBOK                        
101600        END-IF                                                            
101700     END-IF                                                               
101800                                                                          
101900     IF MID-FLLSBOK = 'N'                                                 
102000       IF MID-IDLEVNR-EJLS NOT = ALL '+' AND SPACE                        
102100         MOVE MID-IDLEVNR-EJLS TO OHUV-IDLEVNR-EJLS                       
102200       END-IF                                                             
102300     ELSE                                                                 
102400       MOVE SPACE                   TO OHUV-IDLEVNR-EJLS                  
102500     END-IF                                                               
102600                                                                          
102700     IF MID-BEKUNDRF = 'SOFTWARE'                                         
102800        MOVE NEJ                   TO OHUV-FLLSBOK                        
102900        MOVE SPACE                 TO OHUV-IDLEVNR-EJLS                   
103000     END-IF                                                               
103100                                                                          
103200     MOVE JA                       TO OHUV-FLORDSPE                       
103300                                                                          
103400     IF MID-FLAUTPAC = ALL '+'                                            
103500        MOVE JA                    TO OHUV-FLAUTPAC                       
103600     ELSE                                                                 
103700        IF MID-FLAUTPAC = 'Y'                                             
103800           MOVE JA                 TO OHUV-FLAUTPAC                       
103900        ELSE                                                              
104000           MOVE MID-FLAUTPAC       TO OHUV-FLAUTPAC                       
104100        END-IF                                                            
104200     END-IF                                                               
104300                                                                          
104400     IF DIST20-EMBALLAGE                                                  
104500        MOVE JA                    TO OHUV-FLEMBORD                       
104600     ELSE                                                                 
104700        MOVE NEJ                   TO OHUV-FLEMBORD                       
104800     END-IF                                                               
104900     MOVE NEJ                      TO OHUV-FLOVRLEV                       
105000     MOVE NEJ                      TO OHUV-FLRESTN                        
105100     MOVE MID-FLVORKO              TO OHUV-FLVORKO                        
105200     MOVE KREG-IDRFTAB             TO OHUV-IDRFTAB                        
105300     MOVE KREG-IDDC                TO OHUV-IDDC-PRIM                      
105700     MOVE +0                       TO OHUV-IDKAMPRF                       
105800     MOVE SPACE                    TO OHUV-IDBIPREF                       
105900     MOVE OHLK-IDFTG               TO OHUV-IDFTG                          
106000     MOVE OHLK-IDKONTO             TO OHUV-IDKONTO                        
106100     MOVE OHLK-IDANALYS            TO OHUV-IDANALYS                       
106200     MOVE OHLK-IDKST               TO OHUV-IDKST                          
106300     MOVE KREG-IDSKYLT             TO OHUV-IDSKYLT                        
106400     IF MID-KDFAKTYP = ALL '+'                                            
106500        MOVE KREG-KDGENFAK         TO OHUV-KDFAKTYP                       
106600     ELSE                                                                 
106700        MOVE MID-KDFAKTYP          TO OHUV-KDFAKTYP                       
106800     END-IF                                                               
106900     MOVE KREG-KDORDING            TO OHUV-KDORDING                       
107000     MOVE ZERO                     TO OHUV-KDTPOTYP                       
107100     MOVE KREG-KDTULLVE            TO OHUV-KDTULLVE                       
107200     MOVE +0                       TO OHUV-KDVRINFO                       
107300     MOVE MSGI-TILOKDAT            TO OHUV-TIREGDAT                       
107400                                      WS-TIREGDAT-9KOMPL                  
107500     MOVE MSGI-TILOKTID            TO WS-TIHHMM                           
107600     MOVE WS-TIHHMMSS              TO OHUV-TIREGTID                       
107700     MOVE +0                       TO OHUV-TIREGDAT-STO                   
107800     MOVE +0                       TO OHUV-TIREGTID-STO                   
107900     MOVE +0                       TO OHUV-TITPO                          
108000     MOVE JA                       TO OHUV-FLOBTRAN                       
108100     MOVE NEJ                      TO OHUV-FLBORT                         
108200     MOVE NEJ                      TO OHUV-FLSOFT                         
108300     MOVE NEJ                      TO OHUV-FLVORFK                        
108400     IF OHUV-KDORDKL = 1 AND                                              
108500        GMT-FLLDCKND = JA                                                 
108600        MOVE 'FW'                  TO OHUV-KDORDTYP-LDC                   
108700        MOVE ZERO                  TO OHUV-TIREPDAT                       
108800     ELSE                                                                 
108900        MOVE SPACE                 TO OHUV-KDORDTYP-LDC                   
109000        MOVE ZERO                  TO OHUV-TIREPDAT                       
109100     END-IF                                                               
109200     MOVE NEJ                      TO OHUV-FLORDTIL                       
109400     COMPUTE OHUV-TIREGDAT-9KOMPL = 9999999 - WS-TIREGDAT-9KOMPL          
109500     MOVE ZERO                     TO OHUV-KVORDTIL                       
109600                                      OHUV-IDGROSS                        
109700     MOVE SPACE                    TO OHUV-IDBILREG                       
109800                                      OHUV-IDVIN                          
109810                                      OHUV-IDCISNR                        
109900     .                                                                    
110000     EJECT                                                                
110100 DB-REDIGERA-ARBETSTABELL SECTION.                                        
110200                                                                          
110300     MOVE KREG-IDDC            TO ARB-IDDC                                
110400     IF MID-BEGMRK = ALL '+'                                              
110500        MOVE KREG-BEGMRK       TO ARB-BEGMRK                              
110600     ELSE                                                                 
110700        MOVE MID-BEGMRK        TO ARB-BEGMRK                              
110800        INSPECT ARB-BEGMRK REPLACING ALL '+' BY SPACE                     
110900     END-IF                                                               
111000     MOVE NEJ                  TO ARB-FLODELUT                            
111100     MOVE +0                   TO ARB-IDRADNR-SISTA                       
111200     MOVE ZERO                 TO ARB-IDTRP                               
111300                                  ARB-IDTRP-ALT                           
111400     MOVE +0                   TO ARB-IDPLKLST-SISTA                      
111500     MOVE KREG-KDFDKRAV        TO ARB-KDFDKRAV                            
111600     IF MID-KDFRAKT = ALL '+'                                             
111700        MOVE KREG-KDFRAKT      TO ARB-KDFRAKT                             
111800     ELSE                                                                 
111900        MOVE MID-KDFRAKT       TO ARB-KDFRAKT                             
112000     END-IF                                                               
112100     MOVE +0                   TO ARB-KDROPACK                            
112200     MOVE KREG-KDTRPKAT        TO ARB-KDTRPKAT                            
112300     MOVE +0                   TO ARB-KVSEMBRA                            
112400     MOVE MSGI-TILOKDAT        TO WS-DATUM                                
112500     MOVE MSGI-TILOKTID        TO WS-TID                                  
112600     MOVE WS-DATUM-TID         TO ARB-TIRFS                               
112700     MOVE ZERO                 TO ARB-DATRPAVD                            
112800     MOVE +0                   TO ARB-TIHHMM                              
112801     MOVE 'E'                  TO ARB-KDORDSTA                            
112802     MOVE SPACE                TO ARB-KDORDSTA-O                          
114200     .                                                                    
114300     EJECT                                                                
114400                                                                          
114500 E-HOPPA-TILL-RADREGISTRERING SECTION.                                    
114510                                                                          
114520     MOVE '001'              TO MSGI-KDCALL                               
114530     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
114540     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
114550     MOVE '4231'             TO MSGI-IDTRANS                              
114560     MOVE MID-IDDISTR        TO MSGI-IDDISTR                              
114570     MOVE MID-IDKUNDNR       TO MSGI-IDKUNDNR                             
114580     MOVE W-IDORDNR          TO MSGI-IDKUNDRF                             
114590     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
114600                                                                          
114700     MOVE MFS-KDMFSFOR           TO 4232-SPRAK                            
114800     MOVE ALL '+'                TO 4232-DATA                             
114900     MOVE W-IDDISTR              TO WS-NUM-4                              
115000     MOVE WS-NUM-4               TO 4232-IDDISTR                          
115200                                                                          
115300     MOVE W-IDKUNDNR             TO WS-NUM-6                              
115400     MOVE WS-NUM-6               TO 4232-IDKUNDNR                         
115600                                                                          
115700     MOVE W-IDORDNR              TO WS-NUM-5                              
115800     MOVE WS-NUM-5               TO 4232-IDORDNR                          
116000                                                                          
116100     MOVE MID-KDORDKL            TO 4232-KDORDKL                          
116200     MOVE ARB-KDFRAKT            TO 4232-KDFRAKT                          
116300                                                                          
116400     PERFORM IMS-INSERT-4232-MSG                                          
116500     MOVE JA                     TO HOPP                                  
116600     .                                                                    
116700     EJECT                                                                
116800 Z-FINIT SECTION.                                                         
116900                                                                          
117000     IF MED-IDMFSFEL NOT = SPACE                                          
117100         CALL WMEDKONV USING MED-WMEDAREA                                 
117200                                                                          
117300         IF WS-ERROR = SPACE                                              
117400           MOVE MED-MFSFEL  TO MOD-TEMFSFEL                               
117500         ELSE                                                             
117600           STRING MED-MFSFEL(1:20) WS-ERROR                               
117700           DELIMITED BY SIZE                                              
117800                        INTO MOD-TEMFSFEL                                 
117900         END-IF                                                           
118000     END-IF                                                               
118100     IF  W-IDTRANS NOT = '4224'                                           
118200     AND W-IDTRANS NOT = '4225'                                           
118300       PERFORM MFS-ROER-EJ-BILD                                           
118400     END-IF                                                               
118500     .                                                                    
118600     EJECT                                                                
118700 S01-VISA-BILD SECTION.                                                   
118800                                                                          
118900     IF MID-IDDISTR NOT = ALL '+'                                         
119000       MOVE MID-IDDISTR         TO MOD-IDDISTR                            
119100       INSPECT MOD-IDDISTR REPLACING LEADING ZERO BY SPACE                
119200     ELSE                                                                 
119300       MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR                           
119400     END-IF                                                               
119500                                                                          
119600     IF MID-IDKUNDNR NOT = ALL '+'                                        
119700       MOVE MID-IDKUNDNR         TO MOD-IDKUNDNR                          
119800       INSPECT MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
119900     ELSE                                                                 
120000       MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR                          
120100     END-IF                                                               
120200                                                                          
120300     IF MID-IDORDNR NOT = ALL '+'                                         
120400       MOVE MID-IDORDNR          TO MOD-IDORDNR                           
120500       INSPECT MOD-IDORDNR REPLACING LEADING ZERO BY SPACE                
120600     ELSE                                                                 
120700       MOVE MFS-RENSA-FAELT      TO MOD-IDORDNR                           
120800     END-IF                                                               
120900                                                                          
121000     MOVE MFS-RENSA-FAELT        TO MOD-KDFRAKT                           
121100                                                                          
121200     IF MID-KDORDKL NOT = ALL '+'                                         
121300       MOVE MID-KDORDKL          TO MOD-KDORDKL                           
121400     ELSE                                                                 
121500       MOVE MFS-RENSA-FAELT      TO MOD-KDORDKL                           
121600     END-IF                                                               
121700                                                                          
121800     PERFORM MFS-NOLLA-BILD                                               
121900                                                                          
122000     .                                                                    
122100     EJECT                                                                
122200 S02-FLYTTA-VOR-RADER-TILL-MOD SECTION.                                   
122300                                                                          
122400     MOVE MID-FLVORKO            TO MOD-FLVORKO                           
122500                                                                          
122600     IF MID-IDDC-TVS = ALL '+' AND                                        
122700                 MID-IDDISTR NOT = ALL '+'                                
122800         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-TVS-ATTR                     
122900         MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                           
123000         MOVE NEJ TO ALLT-SW                                              
123100     END-IF                                                               
123200     .                                                                    
123300     EJECT                                                                
123400 MFS-RENSA-BILD SECTION.                                                  
123500                                                                          
123600     MOVE MFS-RENSA-FAELT  TO   MOD-IDDISTR                               
123700                                MOD-IDKUNDNR                              
123800                                MOD-IDORDNR                               
123900                                MOD-KDORDKL                               
124000                                MOD-KDFRAKT                               
124100                                MOD-IDDC-TVS                              
124200                                MOD-KDFAKTYP                              
124300                                MOD-BEKUNDRF                              
124400                                MOD-FLLSBOK                               
124500                                MOD-FLAUTPAC                              
124600                                MOD-FLAUTFAK                              
124700                                MOD-BEGMT-RAD1                            
124800                                MOD-BEGMT-RAD2                            
124900                                MOD-ADGMT-GATA                            
125000                                MOD-ADGMT-PADR                            
125100                                MOD-ADGMT-LAND                            
125200                                MOD-BEGMRK-RAD1                           
125300                                MOD-BEGMRK-RAD2                           
125400                                MOD-IDFTG                                 
125500                                MOD-IDKONTO                               
125600                                MOD-IDANALYS                              
125700                                MOD-IDKST                                 
125800                                MOD-BEVARREF                              
125900                                MOD-IDLEVNR-EJLS                          
126000     .                                                                    
126100     EJECT                                                                
126200 MFS-NOLLA-BILD SECTION.                                                  
126300                                                                          
126400     MOVE MFS-RENSA-FAELT  TO   MOD-KDFRAKT                               
126500                                MOD-IDDC-TVS                              
126600                                MOD-KDFAKTYP                              
126700                                MOD-BEKUNDRF                              
126800                                MOD-FLLSBOK                               
126900                                MOD-FLAUTPAC                              
127000                                MOD-FLAUTFAK                              
127100                                MOD-BEGMT-RAD1                            
127200                                MOD-BEGMT-RAD2                            
127300                                MOD-ADGMT-GATA                            
127400                                MOD-ADGMT-PADR                            
127500                                MOD-ADGMT-LAND                            
127600                                MOD-BEGMRK-RAD1                           
127700                                MOD-BEGMRK-RAD2                           
127800                                MOD-IDFTG                                 
127900                                MOD-IDKONTO                               
128000                                MOD-IDANALYS                              
128100                                MOD-IDKST                                 
128200                                MOD-BEVARREF                              
128300     .                                                                    
128400     EJECT                                                                
128500 MFS-ROER-EJ-BILD SECTION.                                                
128600                                                                          
128700     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDISTR                               
128800                                MOD-IDKUNDNR                              
128900                                MOD-IDORDNR                               
129000                                MOD-KDORDKL                               
129100                                MOD-KDFRAKT                               
129200                                MOD-IDDC-TVS                              
129300                                MOD-KDFAKTYP                              
129400                                MOD-BEKUNDRF                              
129500                                MOD-FLLSBOK                               
129600                                MOD-FLAUTPAC                              
129700                                MOD-FLAUTFAK                              
129800                                MOD-BEGMT-RAD1                            
129900                                MOD-BEGMT-RAD2                            
130000                                MOD-ADGMT-GATA                            
130100                                MOD-ADGMT-PADR                            
130200                                MOD-ADGMT-LAND                            
130300                                MOD-BEGMRK-RAD1                           
130400                                MOD-BEGMRK-RAD2                           
130500                                MOD-IDFTG                                 
130600                                MOD-IDKONTO                               
130700                                MOD-IDANALYS                              
130800                                MOD-IDKST                                 
130900                                MOD-BEVARREF                              
131000                                MOD-IDLEVNR-EJLS                          
131100     .                                                                    
131200     EJECT                                                                
131300* --- IMS SEKTIONER ---                                                   
131400 IMS-GET-MSG SECTION.                                                     
131500                                                                          
131600     MOVE '  QC' TO GODK-STATUSKODER                                      
131700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
131800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131900     PERFORM IMS-STATUSKONTROLL                                           
132000     .                                                                    
132100     SKIP2                                                                
132200 IMS-INSERT-MSG SECTION.                                                  
132300                                                                          
132400     IF ENGLISH-TEXT                                                      
132500       MOVE 'N' TO MFS-KDHUVOMR                                           
132600     END-IF                                                               
132700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
132800     MOVE SPACE TO GODK-STATUSKODER                                       
132900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
133000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300     SKIP2                                                                
133400 IMS-INSERT-4232-MSG SECTION.                                             
133500                                                                          
133600     IF ENGLISH-TEXT                                                      
133700       MOVE 'N' TO MFS-KDHUVOMR                                           
133800     END-IF                                                               
133900     MOVE LOW-VALUE TO 4232-Z1 4232-Z2                                    
134000     MOVE SPACE TO GODK-STATUSKODER                                       
134100     CALL CBLTDLI USING ISRT 4232-PCB 4232-MSG-IO-AREA                    
134200     MOVE 4232-STATUS-CODE TO STATUS-WS                                   
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     EJECT                                                                
134600 IMS-GU-ORQL-WDQ201 SECTION.                                              
134700                                                                          
134800     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
134900          DELIMITED BY SIZE INTO SSA1                                     
135000     MOVE '  GE'               TO GODK-STATUSKODER                        
135100     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-AREA-OHUV SSA1                 
135200     MOVE ORQL-STATUS-CODE    TO STATUS-WS                                
135300     PERFORM IMS-STATUSKONTROLL                                           
135400     .                                                                    
135500     EJECT                                                                
135600 IMS-ISRT-ORQI-WDQ201 SECTION.                                            
135700                                                                          
135800     MOVE 'WLORQI01 '          TO SSA1                                    
135900     MOVE '    '               TO GODK-STATUSKODER                        
136000     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-OHUV SSA1               
136100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
136200     PERFORM IMS-STATUSKONTROLL                                           
136300     .                                                                    
136400     SKIP3                                                                
136500 IMS-ISRT-ORQI-WDQ212 SECTION.                                            
136600                                                                          
136700     MOVE 'WLORQI12 '          TO SSA1                                    
136800     MOVE '    '               TO GODK-STATUSKODER                        
136900     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-ARB SSA1                
137000     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300     SKIP3                                                                
137400 IMS-GET-WDB201-UNIK SECTION.                                             
137500                                                                          
137600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
137700          DELIMITED BY SIZE INTO SSA1                                     
137800     MOVE '  GE'               TO GODK-STATUSKODER                        
137900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
138000     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
138100     PERFORM IMS-STATUSKONTROLL                                           
138200     .                                                                    
138300     SKIP2                                                                
138400 IMS-GU-WDB201 SECTION.                                                   
138500                                                                          
138600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
138700                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
138800            DELIMITED BY SIZE INTO SSA1                                   
138900     MOVE '  GE'               TO GODK-STATUSKODER                        
139000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
139100     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     SKIP2                                                                
139500 IMS-GU-WDB601    SECTION.                                                
139600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
139700          DELIMITED BY SIZE INTO SSA1                                     
139800     MOVE '  GE' TO GODK-STATUSKODER                                      
139900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
140000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
140100     PERFORM IMS-STATUSKONTROLL                                           
140200     IF SEGMENT-SAKNAS                                                    
140300         MOVE SPACE TO DCS-KDDC                                           
140400     END-IF                                                               
140500     .                                                                    
140600 IMS-GET-WDF101-UNIK SECTION.                                             
140700                                                                          
140800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
140900          DELIMITED BY SIZE INTO SSA1                                     
141000     MOVE '  GE'               TO GODK-STATUSKODER                        
141100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-WDF101 SSA1               
141200     MOVE WDF1-STATUS-CODE     TO STATUS-WS                               
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     SKIP2                                                                
141600 IMS-STATUSKONTROLL SECTION.                                              
141700                                                                          
141800     SET STATUS-IX TO 1                                                   
141900     SEARCH GODK-STATUS                                                   
142000       AT END CALL FELLOG                                                 
142100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
142200     END-SEARCH                                                           
142300     .                                                                    
