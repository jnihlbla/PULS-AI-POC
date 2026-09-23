000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4022100.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   SEPTEMBER -90.                                           
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR REGISTRERING AV ORDERHUVUD                   
001100*        FÖR FÖRBIORDER SAMT VOR/FÖRBIORDER IFRÅN VOR-KÖN.                
001200*                                                                         
001300*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
001400*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)  ORDERHUVUD                  
001600*        PROGRAMMET LÄSER      WLGMTA (WDB2)  KUNDREGISTER                
001700*        PROGRAMMET LÄSER      WLGMTB (WDB3)  KUNDREGISTER                
001800*        PROGRAMMET LÄSER      WLGMTC (WDB5)  KUNDREGISTER                
001900*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)  ORDERNUMMERREGISTER         
002000*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
002100*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTREGISTER           
002200*        PROGRAMMET LÄSER      WDB6           DC-REGISTER                 
002300*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T221 W4T221U                                      
002700*        MID:         W4I22101                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O22101                                            
003010*                                                                         
003011*    FUNKTION.                                                            
003020*        ETRACKER 5174148: NEW FIELD TO INITIATE IN WDQ201                
003100                                                                          
003200     EJECT                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)   VALUE 'W4022100'.             
004000 77  JA                          PIC X(1)   VALUE 'J'.                    
004100 77  YES                         PIC X(1)   VALUE 'Y'.                    
004200 77  NEJ                         PIC X(1)   VALUE 'N'.                    
004300 77  SPEC-FORBI                  PIC X(1)   VALUE 'S'.                    
004400 77  OBEHORIG                    PIC X(1)   VALUE 'F'.                    
004500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  HOPP                        PIC X(1)   VALUE 'N'.                    
004800 77  WS-IDTRANS                  PIC X(4)   VALUE SPACE.                  
005000                                                                          
005100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200                                                                          
005300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005400     88  ALLT-OK                             VALUE 'J'.                   
005500                                                                          
005600 77  SVAR-SW                     PIC X       VALUE ' '.                   
005700     88  GODK-SVAR                           VALUE 'J' 'Y' 'N'.           
005800                                                                          
005810 77  LYNK-ORDER-SW               PIC X       VALUE 'J'.                   
005820     88  LYNK-ORDER                          VALUE 'J'.                   
005830                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '4221'.                
006100     88  GODK-MID                            VALUE '4221' '4222'          
006200                                                   '4223' '4224'          
006300                                                   '4225'.                
006400 01  WS-ALFA-4.                                                           
006500     03  WS-NUM-4                PIC 9(4).                                
006600 01  WS-ALFA-5.                                                           
006700     03  WS-NUM-5                PIC 9(5).                                
006800 01  WS-ALFA-6.                                                           
006900     03  WS-NUM-6                PIC 9(6).                                
007000 01  WS-ALFA-10.                                                          
007100     03  WS-NUM-10               PIC 9(10).                               
007200     EJECT                                                                
007300 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES WS-TIHHMMSS.                                        
007500     03  WS-TIHHMM               PIC 9(4).                                
007600     03  FILLER                  PIC 9(2).                                
007700     SKIP2                                                                
007800 01  WS-KONTROLL-IDUSER          PIC X(8)    VALUE SPACE.                 
007900 01  FILLER REDEFINES WS-KONTROLL-IDUSER.                                 
008000     03  WS-IDUSER               PIC X(2).                                
008100     03  FILLER                  PIC X(6).                                
008200     EJECT                                                                
008300                                                                          
008400 01  WS-TIREGDAT-9KOMPL          PIC 9(9)    VALUE ZERO.                  
008401 01  WS-SPAR-IDORDNR             PIC 9(7)    VALUE ZERO.                  
008402                                                                          
008403 01  WS-IDORDNR-OLD              PIC X(05).                               
008404 01  FILLER REDEFINES WS-IDORDNR-OLD.                                     
008405     03  WS-IDORDNR-OLD-NUM      PIC 9(5).                                
008407                                                                          
008408 01  WS-ADBETRAD-1               PIC X(35) VALUE SPACE.                   
008409 01  WS-ADBETRAD-2               PIC X(35) VALUE SPACE.                   
008410 01  WS-BEBETRAD-1               PIC X(35) VALUE SPACE.                   
008411 01  WS-BEBETRAD-2               PIC X(35) VALUE SPACE.                   
008412                                                                          
008413                                                                          
008420 01  WS-DATUM-TID                PIC 9(10).                               
008430 01  FILLER REDEFINES WS-DATUM-TID.                                       
008440     03  WS-DATUM                PIC 9(6).                                
008450     03  WS-TID                  PIC 9(4).                                
008500                                                                          
008600                                                                          
008700 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
008800 01  FILLER REDEFINES TEST-IDDISTR.                                       
008900*    03 -COPY WWDIST03                                                    
009000     EJECT                                                                
009100 01  FILLER REDEFINES TEST-IDDISTR.                                       
009200*    03 -COPY WWDIST20                                                    
009300     EJECT                                                                
009400 01  FILLER REDEFINES TEST-IDDISTR.                                       
009500*    03 -COPY WWDIST34                                                    
009600     EJECT                                                                
009700 01  FILLER REDEFINES TEST-IDDISTR.                                       
009800*    ----DISTR-DEALER-PRICE-----                                          
009900*    03 -COPY WWDIST79                                                    
010000     EJECT                                                                
010100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010200 01  GENERELLA-SUBPROGRAM.                                                
010300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
010700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010800*                                                                         
010900 01  GEMENSAMMA-SUBPROGRAM.                                               
011000     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
011100*        FORMELLA KONTROLLER                                              
011200     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
011300*        LÄSNING AV KUNDREGISTRET                                         
011400     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
011500*        LOGISKA KONTROLLER                                               
011600     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
011700*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
011800     03  W411TRAN                PIC X(8)    VALUE 'W411TRAN'.            
011900*        BESTÄM TRANSPORT                                                 
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
012200*   -COPY WSECAREA                                                        
012300     EJECT                                                                
012400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012500*01 -COPY WMSGINIT                                                        
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012800*   -COPY WMEDAREA                                                        
012900     EJECT                                                                
013000 01  MESSAGE-CODES.                                                       
013100     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
013200     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
013300     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '065'.                 
013400     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
013500     03  ERR-TRANSPORT-FEL       PIC X(3)    VALUE '087'.                 
013600     03  ERR-KUND-SPAERRAD       PIC X(3)    VALUE '213'.                 
013700     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
013800     03  ERR-SAKNAS-BET          PIC X(3)    VALUE '145'.                 
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
014100 01  FILLER                      PIC X(16)   VALUE 'W411OHFK'.            
014200*   -COPY W411OHFK                                                        
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'W411KREG'.            
014500*   -COPY W411KREG                                                        
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'W411OHLK'.            
014800*   -COPY W411OHLK                                                        
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
015100*   -COPY W411ORDN                                                        
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'W411TRAN'.            
015400*   -COPY W411TRAN                                                        
015500     EJECT                                                                
015600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015900     SKIP3                                                                
016000*01  MID -COPY W4I22101    -PRE MID-                                      
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016300     SKIP3                                                                
016400*01  -COPY WMSGAREA                                                       
016500     EJECT                                                                
016600*    03  MOD -COPY W4O22101   -RED MSG-AREA -PRE MOD-.                    
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016900     SKIP3                                                                
017000*01  -COPY WMFSAREA                                                       
017100     EJECT                                                                
017200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500                                                                          
017600 01  NYCKLAR-TILL-DLI.                                                    
017700     03  W-WDQ2CSEQ-X.                                                    
017800         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
017900         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
018000         05  W-IDKUNDRF.                                                  
018100           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
018200           07  FILLER            PIC X(3)    VALUE SPACE.                 
018300*                                                                         
018400     03  W-IDGMT-X.                                                       
018500         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
018600         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
018700*                                                                         
018800     03  W-IDGMT-MIN-X.                                                   
018900         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
019000         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
019100*                                                                         
019200     03  W-IDGMT-MAX-X.                                                   
019300         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
019400         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
019500                                                                          
019900     03  W-IDDC-B6-X.                                                     
020000         05 W-IDDC-B6                  PIC X(2).                          
020100     EJECT                                                                
020200*    --- STATUS-KOD FRÅN IMS                                              
020300 01  STATUS-WS                   PIC XX.                                  
020400     88  SEGMENT-FINNS                       VALUE '  '.                  
020500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020700     SKIP2                                                                
020800 01  GODK-STATUSKODER.                                                    
020900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021000     SKIP3                                                                
021100 01  SSA1                        PIC X(64).                               
021200 01  SSA2                        PIC X(64).                               
021300     EJECT                                                                
021400*    --- IMS FUNKTIONSKODER                                               
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700*    ---  DLI INPUT-OUTPUT AREA                                           
021800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021900     SKIP3                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
022100 01  DLI-IO-AREA-OHUV.                                                    
022200     03  WLORQI01.                                                        
022300*        05  -COPY WDQ201                                                 
022400     EJECT                                                                
022500 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
022600 01  DLI-IO-AREA-ARB.                                                     
022700     03  WLORQI12.                                                        
022800*        05  -COPY WDQ212                                                 
022900     EJECT                                                                
023000 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
023100 01  DLI-IO-AREA-WDB201.                                                  
023200     03  WLGMTA01.                                                        
023300*        05  -COPY WDB201                                                 
023400     EJECT                                                                
024000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024100 01   DLI-IO-AREA-B601.                                                   
024200*     03  -COPY WDB601                                                    
024300     EJECT                                                                
024400*---MSG-AREA FÖR HOPP TILL 4222-RADREGISTRERING                           
024500 01  FILLER                  PIC X(16)  VALUE '4222-MSG-IO-AREA'.         
024600 01  4222-MSG-IO-AREA.                                                    
024700     03  4222-LL               PIC S9(4)  VALUE +793 COMP SYNC.           
024800     03  4222-Z1               PIC X.                                     
024900     03  4222-Z2               PIC X.                                     
025000     03  4222-TRANSKOD         PIC X(8)   VALUE 'W4T222  '.               
025100     03  4222-IDTRANS          PIC X(4)   VALUE '4221'.                   
025200     03  4222-SPRAK            PIC X.                                     
025300     03  4222-DATA.                                                       
025400       05 4222-IDDISTR         PIC X(4).                                  
025500       05 4222-IDKUNDNR        PIC X(6).                                  
025600       05 4222-IDORDNR         PIC X(5).                                  
026000       05 4222-KDORDKL         PIC X.                                     
026100       05 4222-KDFRAKT         PIC X(2).                                  
026200       05 4222-FLVORKO         PIC X.                                     
026300       05 4222-FLFORBI         PIC X.                                     
026400       05 4222-RESTEN          PIC X(756).                                
026500     EJECT                                                                
026600 LINKAGE SECTION.                                                         
026700                                                                          
026800*01  -COPY W0009      -PRE MSG-                                           
026900*01  -COPY W0009      -PRE 4222-                                          
027000     EJECT                                                                
027100 01  USEA-PCB                    PIC X.                                   
027200     SKIP2                                                                
027300*01  -COPY W0008      -PRE ORQL-                                          
027400     05  FILLER                  PIC X.                                   
027500     EJECT                                                                
027600*01  -COPY W0008      -PRE ORQI-                                          
027700     05  FILLER                  PIC X.                                   
027800     EJECT                                                                
027900*01  -COPY W0008      -PRE WDB2-                                          
028000     05  FILLER                  PIC X.                                   
028100     EJECT                                                                
028200*01  -COPY W0008      -PRE WDB6-                                          
028300     05  FILLER                  PIC X.                                   
028400     EJECT                                                                
028500 01  KREG-GMTA-PCB               PIC X.                                   
028600 01  KREG-GMTB-PCB               PIC X.                                   
028700 01  KREG-GMTC-PCB               PIC X.                                   
028800 01  KREG-BETC-PCB               PIC X.                                   
028900 01  OHLK-WDM2-PCB               PIC X.                                   
029000 01  OHLK-WDB6-PCB               PIC X.                                   
029100 01  ORDN-XXKP-PCB               PIC X.                                   
029200 01  ORDN-ORQL-PCB               PIC X.                                   
029300 01  ORDN-PROC-PCB               PIC X.                                   
029400 01  ORDN-ORQI-PCB               PIC X.                                   
029500 01  TRAN-XXKB-PCB               PIC X.                                   
029600 01  SAP-SAPC-PCB                PIC X.                                   
029700     EJECT                                                                
029800 PROCEDURE DIVISION  USING MSG-PCB 4222-PCB USEA-PCB ORQL-PCB             
029900                     ORQI-PCB WDB2-PCB WDB6-PCB                           
030000                     KREG-GMTA-PCB KREG-GMTB-PCB                          
030100                     KREG-GMTC-PCB KREG-BETC-PCB                          
030200                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
030300                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
030400                     ORDN-ORQI-PCB                                        
030500                     TRAN-XXKB-PCB                                        
030600                     SAP-SAPC-PCB.                                        
030700 MAIN SECTION.                                                            
030800     ENTRY 'DLITCBL' USING MSG-PCB 4222-PCB USEA-PCB ORQL-PCB             
030900                     ORQI-PCB WDB2-PCB WDB6-PCB                           
031000                     KREG-GMTA-PCB KREG-GMTB-PCB                          
031100                     KREG-GMTC-PCB KREG-BETC-PCB                          
031200                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
031300                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
031400                     ORDN-ORQI-PCB                                        
031500                     TRAN-XXKB-PCB                                        
031600                     SAP-SAPC-PCB.                                        
031700                                                                          
031800     EJECT                                                                
031900     PERFORM IMS-GET-MSG                                                  
032000     IF SEGMENT-FINNS                                                     
032100        PERFORM A-INIT                                                    
032200        IF ALLT-OK                                                        
032300           PERFORM B-KOLLA-O-KOMPLETTERA-INDATA                           
032400           IF ALLT-OK                                                     
032500              PERFORM C-BESTAM-ORDERNUMMER                                
032600              PERFORM D-SKAPA-ORDERHUVUD                                  
032700              PERFORM H-HOPPA-TILL-RADREGISTRERING                        
032800           END-IF                                                         
032900        END-IF                                                            
032910        IF W-IDTRANS = 'V412'                                             
032920          CONTINUE                                                        
032930        ELSE                                                              
033000          IF HOPP = NEJ                                                   
033100             PERFORM Z-FINIT                                              
033200             MOVE MAX-MOD-LAENGD TO MSG-KVLL                              
033300             PERFORM IMS-INSERT-MSG                                       
033400          END-IF                                                          
033410        END-IF                                                            
033500     END-IF                                                               
033600                                                                          
033700     MOVE +0 TO RETURN-CODE                                               
033800     GOBACK                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 A-INIT SECTION.                                                          
034200     MOVE JA TO ALLT-SW                                                   
034300     IF MSG-DUBBLA-TRANSKODER                                             
034400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I22101                 
034500       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
034600       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
034700     ELSE                                                                 
034800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO  MID-W4I22101                 
034900       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
035000       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
035100     END-IF                                                               
035200                                                                          
035300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
035400     MOVE MSG-IDPFK            TO MFS-IDPFK                               
035500     MOVE MFS-IDTRANS          TO W-IDTRANS                               
035600                                                                          
035700     MOVE LOW-VALUE            TO MSG-AREA                                
035800     MOVE 'W4O22101'           TO MFS-IDMOD                               
035900     MOVE '4221'               TO MOD-IDTRANS                             
036000     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
036100     MOVE SPACE                TO MED-IDMFSFEL                            
036200     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O22101 + 4                  
036300                                                                          
036400     IF NOT EGEN-MID                                                      
036500       MOVE SPACE              TO MFS-KDTRTYP                             
036600       MOVE ' '                TO MFS-IDPFK                               
036700     END-IF                                                               
036800                                                                          
036900     IF ENGLISH-TEXT                                                      
037000       MOVE +2                 TO SPRAK-IX                                
037100       MOVE 'GB '              TO MED-IDSKYLT                             
037200     ELSE                                                                 
037300       MOVE +1                 TO SPRAK-IX                                
037400       MOVE 'S  '              TO MED-IDSKYLT                             
037500     END-IF                                                               
037600                                                                          
037700     IF NOT EGEN-MID                                                      
037800     AND W-IDTRANS NOT = '4224'                                           
037900     AND W-IDTRANS NOT = '4225'                                           
037910     AND W-IDTRANS NOT = 'V412'                                           
038000        PERFORM MFS-RENSA-BILD                                            
038100        MOVE NEJ TO ALLT-SW                                               
038200     ELSE                                                                 
038300       PERFORM AB-INITIERA-DIV-AREOR                                      
038310       IF W-IDTRANS NOT = 'V412'                                          
038400          PERFORM AA-KOLLA-BEHORIGHET                                     
038410       END-IF                                                             
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800 AA-KOLLA-BEHORIGHET SECTION.                                             
038900                                                                          
039000     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
039100       MOVE MSG-SIGNON-USERID  TO SEC-IDUSER                              
039200                                  WS-KONTROLL-IDUSER                      
039300       MOVE '4221'             TO SEC-IDTRANS                             
039400       MOVE MID-IDDISTR        TO SEC-IDKEY                               
039500                                                                          
039600       CALL WSECURIT USING SEC-IDUSER                                     
039700                           SEC-IDTRANS                                    
039800                           SEC-IDKEY                                      
039900                           SEC-KDSVAR                                     
040000                                                                          
040100       IF SEC-KDSVAR = OBEHORIG                                           
040200          MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR                      
040300          MOVE ERR-OBEHORIG    TO MED-IDMFSFEL                            
040400          MOVE NEJ             TO ALLT-SW                                 
040500       END-IF                                                             
040600     ELSE                                                                 
040700       MOVE OBEHORIG           TO SEC-KDSVAR                              
040800     END-IF                                                               
040900     .                                                                    
041000     EJECT                                                                
041100 AB-INITIERA-DIV-AREOR SECTION.                                           
041200                                                                          
041300     MOVE SPACE                TO MED-IDMFSFEL                            
041400                                  MOD-FLVORKO                             
041500                                  MOD-FLFORBI                             
041600                                  MOD-NORMALORDER-JA-NEJ                  
041700                                   WS-IDTRANS                             
041800     MOVE JA                   TO ALLT-SW                                 
041900     MOVE NEJ                  TO HOPP                                    
041910     MOVE NEJ                  TO LYNK-ORDER-SW                           
042000                                                                          
042100     INSPECT MID-IDDISTR  REPLACING LEADING SPACE BY ZERO                 
042200     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
042300     INSPECT MID-IDORDNR5 REPLACING LEADING SPACE BY ZERO                 
042400                                                                          
042500     IF MID-FLFORBI = '1'                                                 
042600        MOVE JA                TO MID-FLFORBI                             
042700        MOVE '4224'            TO WS-IDTRANS                              
042800     ELSE                                                                 
042900        IF MID-FLFORBI = '2'                                              
043000           MOVE NEJ            TO MID-FLFORBI                             
043100           MOVE '4224'         TO WS-IDTRANS                              
043200        END-IF                                                            
043300     END-IF                                                               
043400                                                                          
043500     IF MID-FLVORKO = JA                                                  
043600     OR MID-FLVORKO = YES                                                 
043700       IF MID-FLFORBI = JA                                                
043800          MOVE NEJ      TO MID-NORMALORDER-JA-NEJ                         
043900       ELSE                                                               
044000          MOVE JA       TO MID-NORMALORDER-JA-NEJ                         
044100       END-IF                                                             
044200       PERFORM S01-VISA-BILD                                              
044300       IF ALLT-OK                                                         
044400       AND (W-IDTRANS = '4224'                                            
044500        OR  W-IDTRANS = '4225')                                           
044600         MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDDC-ATTR                       
044700         MOVE NEJ TO ALLT-SW                                              
044800       END-IF                                                             
044900     ELSE                                                                 
045000       IF  MID-FLVORKO NOT = JA                                           
045100       AND MID-FLVORKO NOT = YES                                          
045200       AND MID-IDDISTR NOT NUMERIC                                        
045300         PERFORM MFS-RENSA-BILD                                           
045400         MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                             
045500                                 MOD-TEMFSINF                             
045600         MOVE SPACE          TO MED-IDMFSFEL                              
045700         MOVE 'V' TO ALLT-SW                                              
045800       END-IF                                                             
045900                                                                          
046000       MOVE NEJ               TO MID-FLVORKO                              
046100                                                                          
046200       IF MID-NORMALORDER-JA-NEJ = '+'  OR SPACE                          
046300          MOVE NEJ TO MID-NORMALORDER-JA-NEJ                              
046400       END-IF                                                             
046500     END-IF                                                               
046600                                                                          
046700     IF W-IDTRANS NOT = '4221'                                            
046800        MOVE MFS-STAENG-FAELT TO MOD-NORMALORDER-ATTR                     
046900     END-IF                                                               
047000                                                                          
047100     PERFORM S02-FLYTTA-VOR-RADER-TILL-MOD                                
047200     .                                                                    
047300     EJECT                                                                
047400                                                                          
047500                                                                          
047600 B-KOLLA-O-KOMPLETTERA-INDATA SECTION.                                    
047700                                                                          
047800     PERFORM BA-KONTROLLERA-FORMELLA-FEL                                  
047900     IF ALLT-OK                                                           
048000        PERFORM BB-LAS-KUNDREGISTRET                                      
048100        IF ALLT-OK                                                        
048200           PERFORM BC-KONTROLLERA-OM-ORDER-FINNS                          
048300           IF ALLT-OK                                                     
048400              PERFORM BD-KONTROLLERA-LOGISKA-FEL                          
048500              IF ALLT-OK                                                  
048600                 PERFORM BE-BESTAM-TRANSPORT                              
048700              END-IF                                                      
048800           END-IF                                                         
048900        END-IF                                                            
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 BA-KONTROLLERA-FORMELLA-FEL SECTION.                                     
049400                                                                          
049500     MOVE 'IMS '               TO OHFK-IDSYSTEM                           
049600     MOVE MID-IDDISTR          TO OHFK-IDDISTR                            
049700     MOVE MID-IDKUNDNR         TO OHFK-IDKUNDNR                           
049800     MOVE MID-IDORDNR5         TO OHFK-IDORDNR                            
049900     MOVE MID-KDORDKL          TO OHFK-KDORDKL                            
050000     MOVE MID-KDFRAKT          TO OHFK-KDFRAKT                            
050100     MOVE MID-IDDC-TVS         TO OHFK-IDDC                               
050200     MOVE MID-KDFAKTYP         TO OHFK-KDFAKTYP                           
050300     MOVE MID-FLFORBI          TO OHFK-FLFORBI                            
050400     MOVE MID-IDKONTO          TO OHFK-IDKONTO                            
050500     MOVE MID-IDANALYS         TO OHFK-IDANALYS                           
050600     MOVE MID-IDKST            TO OHFK-IDKST                              
050700     MOVE NEJ                  TO OHFK-FLORDSPE                           
050800     MOVE JA                   TO OHFK-FLLSBOK                            
050900                                  OHFK-FLAUTPAC                           
051000     MOVE MID-FLVORKO          TO OHFK-FLVORKO                            
051100     MOVE NEJ                  TO OHFK-FLRESTN                            
051200     MOVE MID-IDFTG            TO OHFK-IDFTG                              
051300     ACCEPT OHFK-TIREGDAT      FROM DATE                                  
051400     ACCEPT OHFK-TIHHMM        FROM TIME                                  
051410     MOVE MID-TIRFS-DAT        TO OHFK-TIRFSDAT                           
051420     MOVE MID-TIRFS-TID        TO OHFK-TIRFSTID                           
051500     MOVE ALL '+'              TO OHFK-IDSKYLT                            
051600                                  OHFK-KDTULLVE                           
051700                                  OHFK-KDVRINFO                           
051800                                  OHFK-TIFORDAT                           
051900                                  OHFK-FLAUTFAK                           
052000                                  OHFK-IDBIPREF                           
052100                                  OHFK-IDKAMPRF                           
052200                                  OHFK-KDROPACK                           
052300                                  OHFK-KDTPOTYP                           
052400                                  OHFK-TITPO                              
052700                                  OHFK-KDPROTYP                           
052800                                                                          
052900     INSPECT OHFK-IDDISTR REPLACING LEADING SPACE BY ZERO                 
053000     INSPECT OHFK-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
053100     INSPECT OHFK-IDORDNR REPLACING LEADING SPACE BY ZERO                 
053200     INSPECT OHFK-KDFRAKT REPLACING LEADING SPACE BY ZERO                 
053300                                                                          
053400     CALL W411OHFK USING OHFK-W411OHFK                                    
053500     PERFORM BAA-KOLLA-FEL-FK                                             
053600     .                                                                    
053700     EJECT                                                                
053800 BAA-KOLLA-FEL-FK SECTION.                                                
053900                                                                          
054000     IF OHFK-IDDISTR-OK = NEJ                                             
054100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
054200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
054300        MOVE NEJ                 TO ALLT-SW                               
054400     ELSE                                                                 
054500        MOVE OHFK-IDDISTR        TO W-IDDISTR                             
054600                                    TEST-IDDISTR                          
054700     END-IF                                                               
054800                                                                          
054900     IF DIST79-DEALER-PRICE                                               
055000        IF ENGLISH-TEXT                                                   
055100           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
055200        ELSE                                                              
055300           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
055400        END-IF                                                            
055500     ELSE                                                                 
055600        MOVE SPACES              TO MOD-TEDDI                             
055700     END-IF                                                               
055800                                                                          
055900     IF OHFK-IDKUNDNR-OK = NEJ                                            
056000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
056100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
056200        MOVE NEJ                 TO ALLT-SW                               
056300     ELSE                                                                 
056400        IF MID-IDKUNDNR = ALL '+'                                         
056500           MOVE ZERO             TO MOD-IDKUNDNR                          
056600                                    W-IDKUNDNR                            
056700        ELSE                                                              
056800           MOVE OHFK-IDKUNDNR    TO W-IDKUNDNR                            
056900        END-IF                                                            
057000     END-IF                                                               
057100                                                                          
057200     IF OHFK-IDORDNR-OK = NEJ                                             
057300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
057400        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR7-ATTR                     
057500        MOVE NEJ                 TO ALLT-SW                               
057600     ELSE                                                                 
057700        IF MID-IDORDNR5 = ALL '+'                                         
057800           MOVE ZERO             TO W-IDORDNR                             
057900           MOVE MFS-RENSA-FAELT  TO MOD-IDORDNR5                          
058000        ELSE                                                              
058100           MOVE OHFK-IDORDNR     TO W-IDORDNR                             
058200        END-IF                                                            
058300     END-IF                                                               
058400                                                                          
058500     IF OHFK-KDORDKL-OK = NEJ                                             
058600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
058700        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
058800        MOVE NEJ                 TO ALLT-SW                               
058900     END-IF                                                               
059000                                                                          
059100     IF OHFK-KDFRAKT-OK = NEJ                                             
059200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
059300        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
059400        MOVE NEJ                 TO ALLT-SW                               
059500     END-IF                                                               
059600                                                                          
059700     IF OHFK-IDFTG-OK = NEJ                                               
059800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
059900        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
060000        MOVE NEJ                 TO ALLT-SW                               
060100     END-IF                                                               
060200                                                                          
060300     IF OHFK-IDKONTO-OK = NEJ                                             
060400        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
060500        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
060600        MOVE NEJ                 TO ALLT-SW                               
060700     END-IF                                                               
060800                                                                          
060900     IF OHFK-IDANALYS-OK = NEJ                                            
061000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
061100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
061200        MOVE NEJ                 TO ALLT-SW                               
061300     END-IF                                                               
061400                                                                          
061500     IF OHFK-IDKST-OK = NEJ                                               
061600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
061700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
061800        MOVE NEJ                 TO ALLT-SW                               
061900     END-IF                                                               
062000                                                                          
062100     IF OHFK-IDDC-OK = NEJ                                                
062200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
062300        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-ATTR                        
062400        MOVE NEJ                 TO ALLT-SW                               
062500     END-IF                                                               
062600                                                                          
062700     IF OHFK-KDFAKTYP-OK = NEJ                                            
062800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
062900        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDFAKTYP-ATTR                     
063000        MOVE NEJ                 TO ALLT-SW                               
063100     END-IF                                                               
063110                                                                          
063120     IF OHFK-TIRFSDAT-OK = NEJ                                            
063130        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
063140        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-DAT-ATTR                    
063150        MOVE NEJ                 TO ALLT-SW                               
063160     END-IF                                                               
063170                                                                          
063180     IF OHFK-TIRFSTID-OK = NEJ                                            
063190        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
063192        MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-TID-ATTR                    
063193        MOVE NEJ                 TO ALLT-SW                               
063194     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 BB-LAS-KUNDREGISTRET SECTION.                                            
063500                                                                          
063600     MOVE W-IDDISTR            TO KREG-IDDISTR                            
063700     MOVE W-IDKUNDNR           TO KREG-IDKUNDNR                           
063800     IF  WS-IDTRANS = '4224'                                              
063900     AND (MID-FLVORKO = JA                                                
064000      OR  MID-FLVORKO = YES)                                              
064100        MOVE '4224'            TO KREG-IDSYSTEM                           
064200     ELSE                                                                 
064300        MOVE 'IMS '            TO KREG-IDSYSTEM                           
064400     END-IF                                                               
064500     MOVE MID-IDDC-TVS         TO KREG-IDDC-TVS                           
064600     IF MID-KDFRAKT = ALL '+'                                             
064700        MOVE +0                TO KREG-KDFRAKT-IN                         
064800     ELSE                                                                 
064900        MOVE MID-KDFRAKT       TO KREG-KDFRAKT-IN                         
065000     END-IF                                                               
065100     MOVE MID-KDORDKL          TO KREG-KDORDKL                            
065200     IF MID-KDFAKTYP = ALL '+'                                            
065300        MOVE SPACE             TO KREG-KDFAKTYP-IN                        
065400     ELSE                                                                 
065500        MOVE MID-KDFAKTYP      TO KREG-KDFAKTYP-IN                        
065600     END-IF                                                               
065700                                                                          
065800     MOVE MID-FLVORKO          TO KREG-FLVORKO                            
065810     MOVE NEJ                  TO KREG-FLVORFK                            
065900                                                                          
066000     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB KREG-GMTB-PCB        
066100                                       KREG-GMTC-PCB KREG-BETC-PCB        
066200                                                                          
066300     IF KREG-KDKREDSP = '1'                                               
066400        MOVE ERR-KUND-SPAERRAD   TO MED-IDMFSFEL                          
066500        MOVE NEJ                 TO ALLT-SW                               
066600     ELSE                                                                 
066700        IF KREG-IDVAT-OK = NEJ                                            
066800           MOVE ERR-MOMS-REGNR-FEL TO MED-IDMFSFEL                        
066900           MOVE NEJ                TO ALLT-SW                             
067000        ELSE                                                              
067100          IF KREG-IDPARTNR-OK = NEJ                                       
067200            MOVE ERR-SAKNAS-BET      TO MED-IDMFSFEL                      
067300            MOVE NEJ                 TO ALLT-SW                           
067400          END-IF                                                          
067500        END-IF                                                            
067600     END-IF                                                               
067700                                                                          
067800     PERFORM BBA-KOLLA-FEL-KREG                                           
067900     IF KREG-KDTRPKAT = 'C'                                               
068000        MOVE '00000'             TO KREG-IDTRP                            
068100                                    KREG-IDTRP-ALT                        
068200     END-IF                                                               
068300     .                                                                    
068400     EJECT                                                                
068500 BBA-KOLLA-FEL-KREG SECTION.                                              
068600                                                                          
068700     IF KREG-IDDISTR-OK = NEJ                                             
068800        MOVE ERR-SAKNAS-KREG     TO MED-IDMFSFEL                          
068900        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
069000        MOVE NEJ                 TO ALLT-SW                               
069100     END-IF                                                               
069200                                                                          
069300     IF KREG-IDKUNDNR-OK = NEJ                                            
069400        MOVE ERR-SAKNAS-KREG     TO MED-IDMFSFEL                          
069500        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
069600        MOVE NEJ                 TO ALLT-SW                               
069700     END-IF                                                               
069800                                                                          
069900     IF KREG-IDDC-OK = NEJ                                                
070000        MOVE ERR-SAKNAS-KREG     TO MED-IDMFSFEL                          
070100        MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDDC-ATTR                       
070200        MOVE NEJ                 TO ALLT-SW                               
070300     END-IF                                                               
070400                                                                          
070500     IF KREG-KDFRAKT-OK = NEJ                                             
070600        MOVE ERR-SAKNAS-KREG     TO MED-IDMFSFEL                          
070700        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
070800        MOVE NEJ                 TO ALLT-SW                               
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200 BC-KONTROLLERA-OM-ORDER-FINNS SECTION.                                   
071300                                                                          
071400     IF MID-IDORDNR5 NOT = ALL '+'                                        
071500                                                                          
071600        PERFORM IMS-01-GU-WDQ2C-WDQ201                                    
071700                                                                          
071800        IF SEGMENT-FINNS                                                  
071900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDORDNR7-ATTR                    
072000           MOVE ERR-ORDER-FINNS   TO MED-IDMFSFEL                         
072100           MOVE NEJ               TO ALLT-SW                              
072200        END-IF                                                            
072300     END-IF                                                               
072400     .                                                                    
072500     EJECT                                                                
072600 BD-KONTROLLERA-LOGISKA-FEL SECTION.                                      
072700                                                                          
072800     MOVE 'IMS '               TO OHLK-IDSYSTEM                           
072900     MOVE W-IDDISTR            TO OHLK-IDDISTR                            
073000     MOVE W-IDKUNDNR           TO OHLK-IDKUNDNR                           
073100     MOVE W-IDORDNR            TO OHLK-IDORDNR                            
073200     MOVE MID-KDORDKL          TO OHLK-KDORDKL                            
073300     MOVE MID-IDDC-TVS         TO OHLK-IDDC                               
073400                                  OHLK-IDDC-TVS                           
073500     MOVE SEC-KDSVAR           TO OHLK-SEC-KDSVAR                         
073600     MOVE KREG-FLAUTORD        TO OHLK-FLAUTORD                           
073700     IF MID-IDKONTO = ALL '+'                                             
073800        MOVE +0                TO OHLK-IDKONTO                            
073900     ELSE                                                                 
074000        MOVE MID-IDKONTO       TO WS-ALFA-10                              
074100        MOVE WS-NUM-10         TO OHLK-IDKONTO                            
074200     END-IF                                                               
074300     IF MID-IDANALYS = ALL '+'                                            
074400        MOVE SPACE             TO OHLK-IDANALYS                           
074500     ELSE                                                                 
074600        MOVE MID-IDANALYS      TO OHLK-IDANALYS                           
074700     END-IF                                                               
074800     IF MID-IDKST = ALL '+'                                               
074900        MOVE SPACE             TO OHLK-IDKST                              
075000     ELSE                                                                 
075100        MOVE MID-IDKST         TO OHLK-IDKST                              
075300     END-IF                                                               
075400     IF MID-IDFTG = ALL '+'                                               
075500        MOVE ZERO              TO OHLK-IDFTG                              
075600     ELSE                                                                 
075700        MOVE MID-IDFTG         TO OHLK-IDFTG                              
075800     END-IF                                                               
075900     MOVE +0                   TO OHLK-IDKAMPRF                           
076000     EJECT                                                                
076100     MOVE KREG-FLOKFAK-G       TO OHLK-FLOKFAK-G                          
076200     MOVE KREG-FLOKFAK-N       TO OHLK-FLOKFAK-N                          
076300     MOVE KREG-FLOKFAK-R       TO OHLK-FLOKFAK-R                          
076400     MOVE KREG-FLOKFAK-K       TO OHLK-FLOKFAK-K                          
076500     IF MID-KDFAKTYP = ALL '+'                                            
076600        MOVE KREG-KDGENFAK     TO OHLK-KDFAKTYP                           
076700     ELSE                                                                 
076800        MOVE MID-KDFAKTYP      TO OHLK-KDFAKTYP                           
076900     END-IF                                                               
077000     MOVE NEJ                  TO OHLK-FLORDSPE                           
077100     MOVE MID-FLVORKO          TO OHLK-FLVORKO                            
077200     MOVE +0                   TO OHLK-KDTPOTYP                           
077300     MOVE +0                   TO OHLK-TITPO                              
077400                                                                          
077500     CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB ORDN-XXKP-PCB        
077600                                       KREG-GMTA-PCB                      
077700                                       SAP-SAPC-PCB                       
077800                                       OHLK-WDB6-PCB                      
077900                                                                          
078000     PERFORM BDA-KOLLA-FEL-LK                                             
078100     .                                                                    
078200     EJECT                                                                
078300 BDA-KOLLA-FEL-LK SECTION.                                                
078400                                                                          
078500     IF OHLK-IDDISTR-OK = NEJ                                             
078600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
078700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
078800        MOVE NEJ                 TO ALLT-SW                               
078900     END-IF                                                               
079000                                                                          
079100     IF OHLK-IDORDNR-OK = NEJ                                             
079200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
079300        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR7-ATTR                     
079400        MOVE NEJ                 TO ALLT-SW                               
079500     END-IF                                                               
079600                                                                          
079700     IF OHLK-KDORDKL-OK = NEJ                                             
079800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
079900        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
080000        MOVE NEJ                 TO ALLT-SW                               
080100     END-IF                                                               
080200                                                                          
080300     IF OHLK-IDFTG-OK = NEJ                                               
080400        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
080500        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
080600        MOVE NEJ                 TO ALLT-SW                               
080700     END-IF                                                               
080800     EJECT                                                                
080900     IF OHLK-IDKONTO-OK = NEJ                                             
081000        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
081100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
081200        MOVE NEJ                 TO ALLT-SW                               
081300     END-IF                                                               
081400                                                                          
081500     IF OHLK-IDANALYS-OK = NEJ                                            
081600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
081700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
081800        MOVE NEJ                 TO ALLT-SW                               
081900     END-IF                                                               
082000                                                                          
082100     IF OHLK-IDKST-OK = NEJ                                               
082200        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
082300        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
082400        MOVE NEJ                 TO ALLT-SW                               
082500     END-IF                                                               
082600                                                                          
082700     IF OHLK-KDFAKTYP-OK = NEJ                                            
082800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
082900        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDFAKTYP-ATTR                     
083000        MOVE NEJ                 TO ALLT-SW                               
083100     END-IF                                                               
083200                                                                          
083300     IF OHLK-IDDC-TVS-OK = NEJ                                            
083400        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
083500        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-ATTR                         
083600        MOVE NEJ                 TO ALLT-SW                               
083700     END-IF                                                               
083800     .                                                                    
083900     EJECT                                                                
084000 BE-BESTAM-TRANSPORT SECTION.                                             
084100                                                                          
084200     MOVE 'IMS '               TO TRAN-IDSYSTEM                           
084300     MOVE KREG-IDTRP           TO TRAN-IDTRP                              
084400     MOVE MID-IDDC-TVS         TO TRAN-IDDC                               
084500     MOVE OHLK-KDORDKL         TO TRAN-KDORDKL                            
084600     MOVE KREG-KDTRPKAT        TO TRAN-KDTRPKAT                           
084700     MOVE KREG-KVLEDTIM-0      TO TRAN-KVLEDTIM-0                         
084800     MOVE KREG-KVLEDTIM-1      TO TRAN-KVLEDTIM-1                         
084900     MOVE KREG-KVLEDTIM-2      TO TRAN-KVLEDTIM-2                         
085000     MOVE KREG-KVLEDTIM-3      TO TRAN-KVLEDTIM-3                         
085100     MOVE KREG-KVLEDTIM-4      TO TRAN-KVLEDTIM-4                         
085200     MOVE NEJ                  TO TRAN-FLORDSPE                           
085300     MOVE NEJ                  TO TRAN-FLOVRLEV                           
085400     PERFORM BEA-FIXA-LOKAL-TID                                           
085500     MOVE MSGI-TILOKDAT        TO TRAN-TIREGDAT                           
085600     MOVE MSGI-TILOKTID        TO TRAN-TIHHMM-REG                         
085700     IF MID-TIRFS-DAT = ALL '+'                                           
085710        MOVE +0                TO TRAN-TIRFS                              
085720     ELSE                                                                 
085730        MOVE OHFK-TIRFSDAT     TO WS-DATUM                                
085740        MOVE OHFK-TIRFSTID     TO WS-TID                                  
085750        MOVE WS-DATUM-TID      TO TRAN-TIRFS                              
085760     END-IF                                                               
085800     MOVE +0                   TO TRAN-KDTPOTYP                           
085900                                                                          
086000     CALL W411TRAN USING TRAN-W411TRAN TRAN-XXKB-PCB                      
086100                                                                          
086200     IF TRAN-KDSVAR = '1'                                                 
086300        MOVE ERR-TRANSPORT-FEL      TO MED-IDMFSFEL                       
086400        MOVE NEJ                    TO ALLT-SW                            
086410     ELSE                                                                 
086420       IF TRAN-KDSVAR = '2' OR '3' OR '4'                                 
086430          MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                        
086440          MOVE MFS-NUM-FAELT-FEL   TO MOD-TIRFS-DAT-ATTR                  
086450                                      MOD-TIRFS-TID-ATTR                  
086460          MOVE NEJ                 TO ALLT-SW                             
086470       END-IF                                                             
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800 BEA-FIXA-LOKAL-TID SECTION.                                              
086900                                                                          
087000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
087100     MOVE '001'             TO MSGI-KDCALL                                
087200     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
087300     MOVE MID-IDDC-TVS      TO MSGI-IDUSER(6:2)                           
087400     MOVE '4221'            TO MSGI-IDTRANS                               
087500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
087600                                                                          
087700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
087800     .                                                                    
087900     EJECT                                                                
088000                                                                          
088001                                                                          
088010 C-BESTAM-ORDERNUMMER SECTION.                                            
088100                                                                          
088200     MOVE 'IMS '               TO ORDN-IDSYSTEM                           
088300     MOVE W-IDDISTR            TO ORDN-IDDISTR                            
088400     MOVE W-IDKUNDNR           TO ORDN-IDKUNDNR                           
088500                                                                          
088600     IF MID-IDORDNR5 = ALL '+'                                            
088700        MOVE ZERO              TO ORDN-IDORDNR-IN                         
088800     ELSE                                                                 
088900        MOVE W-IDORDNR         TO ORDN-IDORDNR-IN                         
089000     END-IF                                                               
089100                                                                          
089200     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB                      
089300                         ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB        
089400                                                                          
089500     MOVE ORDN-IDORDNR-UT      TO W-IDORDNR                               
089600     .                                                                    
089700     EJECT                                                                
089800 D-SKAPA-ORDERHUVUD SECTION.                                              
089900                                                                          
090000     PERFORM DA-REDIGERA-OHUV                                             
090100     PERFORM IMS-ISRT-ORQI-WDQ201                                         
090200                                                                          
090300     PERFORM DB-REDIGERA-ARBETSTABELL                                     
090400     PERFORM IMS-ISRT-ORQI-WDQ212                                         
090500     .                                                                    
090600     EJECT                                                                
090700 DA-REDIGERA-OHUV SECTION.                                                
090800                                                                          
090810                                                                          
090820     PERFORM DAA-READ-OLD-ORDER                                           
090830                                                                          
090900     MOVE W-IDDISTR                 TO W-IDDISTR-WDB2                     
091000                                       W-IDDISTR-WDB2-MIN                 
091100                                       W-IDDISTR-WDB2-MAX                 
091200     MOVE W-IDKUNDNR             TO W-IDKUNDNR-WDB2                       
091300     PERFORM IMS-GET-WDB201-UNIK                                          
091400     IF SEGMENT-SAKNAS                                                    
091500        PERFORM IMS-GU-WDB201                                             
091600     END-IF                                                               
091700                                                                          
091800     MOVE KREG-IDDEPOT             TO OHUV-IDDEPOT                        
091900     MOVE KREG-IDROUTE             TO OHUV-IDROUTE                        
092000     MOVE KREG-IDZON               TO OHUV-IDZON                          
092100                                                                          
092200     MOVE ORDN-IDORDER-UT          TO OHUV-IDORDER                        
092201     IF LYNK-ORDER                                                        
092210       MOVE WS-ADBETRAD-1          TO OHUV-ADBETRAD-1                     
092220       MOVE WS-ADBETRAD-2          TO OHUV-ADBETRAD-2                     
092230     ELSE                                                                 
092300       MOVE KREG-ADBETRAD-1        TO OHUV-ADBETRAD-1                     
092400       MOVE KREG-ADBETRAD-2        TO OHUV-ADBETRAD-2                     
092410     END-IF                                                               
092500     IF MID-ADGMT = ALL '+'                                               
092600        MOVE KREG-ADGMT            TO OHUV-ADGMT                          
092700     ELSE                                                                 
092800        IF MID-ADGMT-GATA = ALL '+'                                       
092900           MOVE SPACE              TO OHUV-ADGMT-GATA                     
093000        ELSE                                                              
093100           MOVE MID-ADGMT-GATA     TO OHUV-ADGMT-GATA                     
093200        END-IF                                                            
093300        IF MID-ADGMT-PADR = ALL '+'                                       
093400           MOVE SPACE              TO OHUV-ADGMT-PADR                     
093500        ELSE                                                              
093600           MOVE MID-ADGMT-PADR     TO OHUV-ADGMT-PADR                     
093700        END-IF                                                            
093800        IF MID-ADGMT-LAND = ALL '+'                                       
093900           MOVE SPACE              TO OHUV-ADGMT-LAND                     
094000        ELSE                                                              
094100           MOVE MID-ADGMT-LAND     TO OHUV-ADGMT-LAND                     
094200        END-IF                                                            
094300        INSPECT OHUV-ADGMT REPLACING ALL '+' BY SPACE                     
094400     END-IF                                                               
094410     IF LYNK-ORDER                                                        
094440       MOVE WS-BEBETRAD-1          TO OHUV-BEBETRAD-1                     
094450       MOVE WS-BEBETRAD-2          TO OHUV-BEBETRAD-2                     
094460     ELSE                                                                 
094500       MOVE KREG-BEBETRAD-1        TO OHUV-BEBETRAD-1                     
094600       MOVE KREG-BEBETRAD-2        TO OHUV-BEBETRAD-2                     
094610     END-IF                                                               
094700     IF MID-BEGMT = ALL '+'                                               
094800        MOVE KREG-BEGMT            TO OHUV-BEGMT                          
094900     ELSE                                                                 
095000        MOVE MID-BEGMT             TO OHUV-BEGMT                          
095100        INSPECT OHUV-BEGMT REPLACING ALL '+' BY SPACE                     
095200     END-IF                                                               
095300     IF MID-BEKUNDRF = ALL '+'                                            
095400        MOVE SPACE                 TO OHUV-BEKUNDRF                       
095500     ELSE                                                                 
095600        MOVE MID-BEKUNDRF          TO OHUV-BEKUNDRF                       
095700     END-IF                                                               
095800     MOVE ZERO                     TO OHUV-IDDEPT                         
095900     IF MID-BELAGINS-DEL1 = ALL '+'                                       
096000        MOVE SPACE                 TO OHUV-BELAGINS-DEL1                  
096100     ELSE                                                                 
096200        MOVE MID-BELAGINS-DEL1     TO OHUV-BELAGINS-DEL1                  
096300     END-IF                                                               
096400     IF MID-BELAGINS-DEL2 = ALL '+'                                       
096500        MOVE SPACE                 TO OHUV-BELAGINS-DEL2                  
096600     ELSE                                                                 
096700        MOVE MID-BELAGINS-DEL2     TO OHUV-BELAGINS-DEL2                  
096800     END-IF                                                               
096900     IF MID-BEVARREF = ALL '+'                                            
097000        MOVE SPACE                 TO OHUV-BEVARREF                       
097100     ELSE                                                                 
097200        MOVE MID-BEVARREF          TO OHUV-BEVARREF                       
097300     END-IF                                                               
097400     IF DIST03-SVERIGE-EJ-778                                             
097500        AND NOT DIST03-EJ-AUTFAK                                          
097600        AND NOT DIST03-S                                                  
097700        MOVE JA                    TO OHUV-FLAUTFAK                       
097800     ELSE                                                                 
097900        MOVE NEJ                   TO OHUV-FLAUTFAK                       
098000     END-IF                                                               
098100     MOVE NEJ                      TO OHUV-FLAUTPAC                       
098200     MOVE NEJ                      TO OHUV-FLBORT                         
098300     IF DIST20-EMBALLAGE                                                  
098400        MOVE JA                    TO OHUV-FLEMBORD                       
098500     ELSE                                                                 
098600        MOVE NEJ                   TO OHUV-FLEMBORD                       
098700     END-IF                                                               
098800     MOVE KREG-FLPRELRO            TO OHUV-FLPRELRO                       
098900     MOVE KREG-FLPRERS             TO OHUV-FLPRERS                        
099000     MOVE MID-IDDC-TVS             TO W-IDDC-B6                           
099100     PERFORM IMS-GU-WDB601                                                
099200     IF (DIST34-NDC-NA OR DIST34-KINA-NDC) AND DCS-CDC AND                
099300         WS-IDUSER = 'PH'                                                 
099400       MOVE NEJ                    TO OHUV-FLFORBI                        
099500     ELSE                                                                 
099600       MOVE MID-FLFORBI            TO OHUV-FLFORBI                        
099700     END-IF                                                               
099800     MOVE NEJ                      TO OHUV-FLKLAR                         
099900     MOVE JA                       TO OHUV-FLLSBOK                        
100000     MOVE JA                       TO OHUV-FLOBTRAN                       
100100     MOVE NEJ                      TO OHUV-FLORDSPE                       
100200     MOVE NEJ                      TO OHUV-FLOVRLEV                       
100300     MOVE NEJ                      TO OHUV-FLRESTN                        
100400     MOVE MID-FLVORKO              TO OHUV-FLVORKO                        
100500     MOVE SPACE                    TO OHUV-IDBIPREF                       
100600     MOVE KREG-IDDC                TO OHUV-IDDC-PRIM                      
100700     MOVE MID-IDDC-TVS             TO OHUV-IDDC-TVS                       
100800     MOVE OHLK-IDFTG               TO OHUV-IDFTG                          
100810     IF LYNK-ORDER                                                        
100820        MOVE 'LYNV'                TO OHUV-IDSYSTEM                       
100830     ELSE                                                                 
100902        IF DIST79-ECOM-PRICE                                              
100903          MOVE 'ECOM'            TO OHUV-IDSYSTEM                         
100904        ELSE                                                              
100905          MOVE '4221'            TO OHUV-IDSYSTEM                         
100906        END-IF                                                            
100908     END-IF                                                               
101000     MOVE OHLK-IDDISTR             TO OHUV-IDDISTR                        
101100     MOVE OHLK-IDKUNDNR            TO OHUV-IDKUNDNR                       
101200     MOVE W-IDKUNDRF               TO OHUV-IDKUNDRF                       
101300     MOVE +0                       TO OHUV-IDKAMPRF                       
101400     MOVE KREG-IDRFTAB             TO OHUV-IDRFTAB                        
101500     MOVE OHLK-IDKONTO             TO OHUV-IDKONTO                        
101600     MOVE OHLK-IDANALYS            TO OHUV-IDANALYS                       
101700     MOVE OHLK-IDKST               TO OHUV-IDKST                          
101800     MOVE MSG-SIGNON-USERID        TO OHUV-IDUSER                         
101900     MOVE KREG-IDSKYLT             TO OHUV-IDSKYLT                        
102000     IF MID-KDFAKTYP = ALL '+'                                            
102100        MOVE KREG-KDGENFAK         TO OHUV-KDFAKTYP                       
102200     ELSE                                                                 
102300        MOVE MID-KDFAKTYP          TO OHUV-KDFAKTYP                       
102400     END-IF                                                               
102500     MOVE KREG-KDORDING            TO OHUV-KDORDING                       
102600     MOVE OHLK-KDORDKL             TO OHUV-KDORDKL                        
102900     MOVE +0                       TO OHUV-KDTPOTYP                       
103000     MOVE KREG-KDTULLVE            TO OHUV-KDTULLVE                       
103100     MOVE +0                       TO OHUV-KDVRINFO                       
103200     MOVE KREG-KVDAGAR-DOW         TO OHUV-KVDAGAR-DOW                    
103300     MOVE KREG-RESLATT             TO OHUV-RESLATT                        
103400     MOVE MSGI-TILOKDAT            TO OHUV-TIREGDAT                       
103500                                      WS-TIREGDAT-9KOMPL                  
103600     MOVE MSGI-TILOKTID            TO WS-TIHHMM                           
103700     MOVE WS-TIHHMMSS              TO OHUV-TIREGTID                       
103800     MOVE +0                       TO OHUV-TIREGDAT-STO                   
103900     MOVE +0                       TO OHUV-TIREGTID-STO                   
104000     MOVE +0                       TO OHUV-TITPO                          
104100     IF OHUV-KDORDKL = 1 AND                                              
104200        GMT-FLLDCKND = JA                                                 
104300        MOVE 'FW'                  TO OHUV-KDORDTYP-LDC                   
104400        MOVE ZERO                  TO OHUV-TIREPDAT                       
104500     ELSE                                                                 
104600        MOVE SPACE                 TO OHUV-KDORDTYP-LDC                   
104700        MOVE ZERO                  TO OHUV-TIREPDAT                       
104800     END-IF                                                               
104900     MOVE NEJ                      TO OHUV-FLORDTIL                       
105100     COMPUTE OHUV-TIREGDAT-9KOMPL = 9999999 - WS-TIREGDAT-9KOMPL          
105110     MOVE ZERO                     TO OHUV-KVORDTIL                       
105111                                      OHUV-IDGROSS                        
105120     MOVE SPACE                    TO OHUV-IDLEVNR-EJLS                   
105130     MOVE NEJ                      TO OHUV-FLSOFT                         
105140     MOVE NEJ                      TO OHUV-FLVORFK                        
105150     MOVE SPACE                    TO OHUV-IDBILREG                       
105160                                      OHUV-IDVIN                          
105170                                      OHUV-IDCISNR                        
105200     .                                                                    
105210     EJECT                                                                
105220 DAA-READ-OLD-ORDER       SECTION.                                        
105230                                                                          
105231     MOVE SPACE              TO WS-ADBETRAD-1                             
105232     MOVE SPACE              TO WS-ADBETRAD-2                             
105233     MOVE SPACE              TO WS-BEBETRAD-1                             
105234     MOVE SPACE              TO WS-BEBETRAD-2                             
105236                                                                          
105237     MOVE W-IDORDNR          TO WS-SPAR-IDORDNR                           
105238     MOVE MID-BEKUNDRF(1:5)  TO WS-IDORDNR-OLD                            
105239     MOVE WS-IDORDNR-OLD-NUM TO W-IDORDNR                                 
105240                                                                          
105241     PERFORM IMS-01-GU-WDQ2C-WDQ201                                       
105242     IF SEGMENT-FINNS                                                     
105243        IF OHUV-IDSYSTEM(1:3) = 'LYN'                                     
105244          MOVE JA               TO LYNK-ORDER-SW                          
105245          MOVE OHUV-ADBETRAD-1  TO WS-ADBETRAD-1                          
105246          MOVE OHUV-ADBETRAD-2  TO WS-ADBETRAD-2                          
105247          MOVE OHUV-BEBETRAD-1  TO WS-BEBETRAD-1                          
105248          MOVE OHUV-BEBETRAD-2  TO WS-BEBETRAD-2                          
105249        END-IF                                                            
105250     END-IF                                                               
105251                                                                          
105252     MOVE WS-SPAR-IDORDNR    TO W-IDORDNR                                 
105253                                                                          
105254     .                                                                    
105260     EJECT                                                                
105400 DB-REDIGERA-ARBETSTABELL SECTION.                                        
105500                                                                          
105610     MOVE MID-IDDC-TVS         TO ARB-IDDC                                
105700     IF MID-BEGMRK = ALL '+'                                              
105800        MOVE KREG-BEGMRK       TO ARB-BEGMRK                              
105900     ELSE                                                                 
106000        MOVE MID-BEGMRK        TO ARB-BEGMRK                              
106100        INSPECT ARB-BEGMRK REPLACING ALL '+' BY SPACE                     
106200     END-IF                                                               
106300     MOVE NEJ                  TO ARB-FLODELUT                            
106400     MOVE +0                   TO ARB-IDRADNR-SISTA                       
106500     MOVE KREG-IDTRP           TO ARB-IDTRP                               
106600     MOVE KREG-IDTRP-ALT       TO ARB-IDTRP-ALT                           
106700     MOVE +0                   TO ARB-IDPLKLST-SISTA                      
106800     MOVE KREG-KDFDKRAV        TO ARB-KDFDKRAV                            
106900     IF MID-KDFRAKT = ALL '+'                                             
107000        MOVE KREG-KDFRAKT      TO ARB-KDFRAKT                             
107100     ELSE                                                                 
107200        MOVE MID-KDFRAKT       TO ARB-KDFRAKT                             
107300     END-IF                                                               
107310     IF OHUV-ADGMT     EQUAL GMT-ADGMT AND                                
107320        OHUV-BEGMT     EQUAL GMT-BEGMT                                    
107330                                                                          
107340        MOVE +0                TO ARB-KDROPACK                            
107350     ELSE                                                                 
107360        MOVE '3'               TO ARB-KDROPACK                            
107370     END-IF                                                               
107500     MOVE KREG-KDTRPKAT        TO ARB-KDTRPKAT                            
107600     MOVE +0                   TO ARB-KVSEMBRA                            
107700     MOVE TRAN-TIRFS           TO ARB-TIRFS                               
107800     MOVE TRAN-TIAAMMDD        TO ARB-DATRPAVD                            
107900     IF TRAN-TIAAMMDD NOT = ZERO                                          
108000       IF TRAN-TIAAMMDD < 500000                                          
108100         MOVE 20               TO ARB-DATRPAVD (1:2)                      
108200       ELSE                                                               
108300         IF TRAN-TIAAMMDD < 999999                                        
108400           MOVE 19             TO ARB-DATRPAVD (1:2)                      
108500         ELSE                                                             
108600           MOVE 99999999       TO ARB-DATRPAVD                            
108700         END-IF                                                           
108800       END-IF                                                             
108900     END-IF                                                               
109000     MOVE TRAN-TIHHMM          TO ARB-TIHHMM                              
109001     MOVE 'E'                  TO ARB-KDORDSTA                            
109002     MOVE SPACE                TO ARB-KDORDSTA-O                          
110400     .                                                                    
110410     EJECT                                                                
110500                                                                          
110610 H-HOPPA-TILL-RADREGISTRERING SECTION.                                    
110700                                                                          
110701     MOVE '001'              TO MSGI-KDCALL                               
110702     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
110703     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
110704     MOVE '4221'             TO MSGI-IDTRANS                              
110705     MOVE MID-IDDISTR        TO MSGI-IDDISTR                              
110706     MOVE MID-IDKUNDNR       TO MSGI-IDKUNDNR                             
110707     MOVE W-IDORDNR          TO MSGI-IDKUNDRF                             
110708     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
110710                                                                          
110800     MOVE MFS-KDMFSFOR           TO 4222-SPRAK                            
110900     MOVE ALL '+'                TO 4222-DATA                             
111000     MOVE W-IDDISTR              TO WS-NUM-4                              
111100     MOVE WS-NUM-4               TO 4222-IDDISTR                          
111300                                                                          
111400     MOVE W-IDKUNDNR             TO WS-NUM-6                              
111500     MOVE WS-NUM-6               TO 4222-IDKUNDNR                         
111700                                                                          
111800     MOVE W-IDORDNR              TO WS-NUM-5                              
111900     MOVE WS-NUM-5               TO 4222-IDORDNR                          
112100                                                                          
112200     MOVE MID-KDORDKL            TO 4222-KDORDKL                          
112300     MOVE MID-KDFRAKT            TO 4222-KDFRAKT                          
112400     MOVE MID-FLVORKO            TO 4222-FLVORKO                          
112500     MOVE MID-FLFORBI            TO 4222-FLFORBI                          
112501                                                                          
112510     IF W-IDTRANS = 'V412'                                                
112520       MOVE 'V412'               TO 4222-IDTRANS                          
112530     END-IF                                                               
112700     PERFORM IMS-INSERT-4222-MSG                                          
112800     MOVE JA                     TO HOPP                                  
112900     .                                                                    
113000     EJECT                                                                
113100 S01-VISA-BILD SECTION.                                                   
113200                                                                          
113300     IF MID-IDDISTR NOT = ALL '+'                                         
113400       MOVE MID-IDDISTR          TO MOD-IDDISTR-1-IN                      
113500       INSPECT MOD-IDDISTR-1-IN REPLACING LEADING ZERO BY SPACE           
113600     ELSE                                                                 
113700       MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-1-IN                      
113800     END-IF                                                               
113900                                                                          
114000     IF MID-IDKUNDNR NOT = ALL '+'                                        
114100       MOVE MID-IDKUNDNR         TO MOD-IDKUNDNR                          
114200       INSPECT MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
114300     ELSE                                                                 
114400       MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR                          
114500     END-IF                                                               
114600                                                                          
114700     IF MID-IDORDNR5 NOT = ALL '+'                                        
114800       MOVE MID-IDORDNR5         TO MOD-IDORDNR5                          
114900       INSPECT MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
115000     ELSE                                                                 
115100       MOVE MFS-RENSA-FAELT      TO MOD-IDORDNR5                          
115200     END-IF                                                               
115300                                                                          
115400     MOVE MFS-RENSA-FAELT        TO MOD-KDFRAKT                           
115500                                                                          
115600     IF MID-KDORDKL NOT = ALL '+'                                         
115700       MOVE MID-KDORDKL          TO MOD-KDORDKL                           
115800     ELSE                                                                 
115900       MOVE MFS-RENSA-FAELT      TO MOD-KDORDKL                           
116000     END-IF                                                               
116100                                                                          
116110     IF MID-TIRFS-DAT NOT = ALL '+'                                       
116120       MOVE MID-TIRFS-DAT        TO MOD-TIRFS-DAT                         
116130     ELSE                                                                 
116140       MOVE MFS-RENSA-FAELT      TO MOD-TIRFS-DAT                         
116150     END-IF                                                               
116151                                                                          
116152     IF MID-TIRFS-TID NOT = ALL '+'                                       
116153       MOVE MID-TIRFS-TID        TO MOD-TIRFS-TID                         
116154     ELSE                                                                 
116155       MOVE MFS-RENSA-FAELT      TO MOD-TIRFS-TID                         
116156     END-IF                                                               
116157                                                                          
116159     IF MID-BEKUNDRF  NOT = ALL '+'                                       
116161       MOVE MID-BEKUNDRF         TO MOD-BEKUNDRF                          
116162     ELSE                                                                 
116164       MOVE MFS-RENSA-FAELT      TO MOD-BEKUNDRF                          
116165     END-IF                                                               
116170                                                                          
116200     PERFORM MFS-NOLLA-BILD                                               
116300                                                                          
116400     .                                                                    
116500     EJECT                                                                
116600                                                                          
116700 S02-FLYTTA-VOR-RADER-TILL-MOD SECTION.                                   
116800                                                                          
116900     MOVE MID-NORMALORDER-JA-NEJ TO SVAR-SW                               
117000                                                                          
117100     IF GODK-SVAR                                                         
117200        IF MID-NORMALORDER-JA-NEJ = 'J' OR 'Y'                            
117300           MOVE NEJ              TO MID-FLFORBI                           
117400        ELSE                                                              
117500           MOVE JA               TO MID-FLFORBI                           
117600        END-IF                                                            
117700     ELSE                                                                 
117800        MOVE MFS-ALFA-FAELT-FEL TO MOD-NORMALORDER-ATTR                   
117900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
118000        MOVE NEJ TO ALLT-SW                                               
118100     END-IF                                                               
118200                                                                          
118300     IF  (MID-FLVORKO = JA                                                
118400      OR  MID-FLVORKO = YES)                                              
118500     AND (W-IDTRANS = '4224'                                              
118600      OR  W-IDTRANS = '4225')                                             
118700        IF MID-FLFORBI = JA                                               
118800           MOVE '1'              TO MID-FLFORBI                           
118900        ELSE                                                              
119000           IF MID-FLFORBI = NEJ                                           
119100              MOVE '2'           TO MID-FLFORBI                           
119200           END-IF                                                         
119300        END-IF                                                            
119400     END-IF                                                               
119500                                                                          
119600     MOVE MID-FLVORKO            TO MOD-FLVORKO                           
119700                                    4222-FLVORKO                          
119800     MOVE MID-FLFORBI            TO MOD-FLFORBI                           
119900                                    4222-FLFORBI                          
120000                                                                          
120100     IF MID-NORMALORDER-JA-NEJ = JA                                       
120200        IF ENGLISH-TEXT                                                   
120300           MOVE 'Y'              TO MOD-NORMALORDER-JA-NEJ                
120400        ELSE                                                              
120500           MOVE 'J'              TO MOD-NORMALORDER-JA-NEJ                
120600        END-IF                                                            
120700     ELSE                                                                 
120800        MOVE MID-NORMALORDER-JA-NEJ TO MOD-NORMALORDER-JA-NEJ             
120900     END-IF                                                               
121000                                                                          
121100     IF MID-IDDC-TVS = ALL '+' AND                                        
121200                 MID-IDDISTR NOT = ALL '+'                                
121300         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                         
121400         MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                           
121500         MOVE NEJ TO ALLT-SW                                              
121600     END-IF                                                               
121700     .                                                                    
121800     EJECT                                                                
121900                                                                          
124000 Z-FINIT SECTION.                                                         
124100                                                                          
124200     IF MED-IDMFSFEL NOT = SPACE                                          
124300         CALL WMEDKONV USING MED-WMEDAREA                                 
124400         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
124500     END-IF                                                               
124600                                                                          
124700     IF  W-IDTRANS NOT = '4224'                                           
124800     AND W-IDTRANS NOT = '4225'                                           
124900        IF W-IDTRANS = '4223'                                             
125000           PERFORM MFS-RENSA-BILD                                         
125100           MOVE NEJ TO MOD-NORMALORDER-JA-NEJ                             
125200        ELSE                                                              
125300          PERFORM MFS-ROER-EJ-BILD                                        
125400*          MOVE NEJ TO MOD-NORMALORDER-JA-NEJ                             
125500        END-IF                                                            
125600     END-IF                                                               
125700     .                                                                    
125800     EJECT                                                                
125900 MFS-ROER-EJ-BILD SECTION.                                                
126000                                                                          
126100     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDISTR-1-IN                          
126200                                MOD-IDKUNDNR                              
126300                                MOD-IDORDNR5                              
126400                                MOD-KDORDKL                               
126500                                MOD-KDFRAKT                               
126600                                MOD-IDDC-TVS                              
126700                                MOD-KDFAKTYP                              
126710                                MOD-TIRFS-DAT                             
126720                                MOD-TIRFS-TID                             
126800                                MOD-BEKUNDRF                              
126900                                MOD-BELAGINS-DEL1                         
127000                                MOD-BELAGINS-DEL2                         
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
128500 MFS-RENSA-BILD SECTION.                                                  
128600                                                                          
128700     MOVE MFS-RENSA-FAELT   TO  MOD-IDDISTR-1-IN                          
128800                                MOD-IDKUNDNR                              
128900                                MOD-IDORDNR5                              
129000                                MOD-KDORDKL                               
129100                                MOD-KDFRAKT                               
129200                                MOD-IDDC-TVS                              
129300                                MOD-KDFAKTYP                              
129310                                MOD-TIRFS-DAT                             
129320                                MOD-TIRFS-TID                             
129400                                MOD-BEKUNDRF                              
129500                                MOD-BELAGINS-DEL1                         
129600                                MOD-BELAGINS-DEL2                         
129700                                MOD-BEGMT-RAD1                            
129800                                MOD-BEGMT-RAD2                            
129900                                MOD-ADGMT-GATA                            
130000                                MOD-ADGMT-PADR                            
130100                                MOD-ADGMT-LAND                            
130200                                MOD-BEGMRK-RAD1                           
130300                                MOD-BEGMRK-RAD2                           
130400                                MOD-IDFTG                                 
130500                                MOD-IDKONTO                               
130600                                MOD-IDANALYS                              
130700                                MOD-IDKST                                 
130800                                MOD-BEVARREF                              
130900     .                                                                    
131000     EJECT                                                                
131100 MFS-NOLLA-BILD SECTION.                                                  
131200                                                                          
131300     MOVE MFS-RENSA-FAELT   TO  MOD-IDDC-TVS                              
131400                                MOD-KDFAKTYP                              
131600                                MOD-BELAGINS-DEL1                         
131700                                MOD-BELAGINS-DEL2                         
131800                                MOD-BEGMT-RAD1                            
131900                                MOD-BEGMT-RAD2                            
132000                                MOD-ADGMT-GATA                            
132100                                MOD-ADGMT-PADR                            
132200                                MOD-ADGMT-LAND                            
132300                                MOD-BEGMRK-RAD1                           
132400                                MOD-BEGMRK-RAD2                           
132500                                MOD-IDFTG                                 
132600                                MOD-IDKONTO                               
132700                                MOD-IDANALYS                              
132800                                MOD-IDKST                                 
132900                                MOD-BEVARREF                              
132910                                MOD-TIRFS-DAT                             
132920                                MOD-TIRFS-TID                             
133000     .                                                                    
133100     EJECT                                                                
133200* --- IMS SEKTIONER ---                                                   
133300     SKIP3                                                                
133400 IMS-GET-MSG SECTION.                                                     
133500                                                                          
133600     MOVE '  QC' TO GODK-STATUSKODER                                      
133700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
133800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133900     PERFORM IMS-STATUSKONTROLL                                           
134000     .                                                                    
134100     SKIP2                                                                
134200 IMS-INSERT-MSG SECTION.                                                  
134300                                                                          
134400     IF ENGLISH-TEXT                                                      
134500       MOVE 'N' TO MFS-KDHUVOMR                                           
134600     END-IF                                                               
134700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
134800     MOVE SPACE TO GODK-STATUSKODER                                       
134900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
135000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135100     PERFORM IMS-STATUSKONTROLL                                           
135200     .                                                                    
135300     SKIP2                                                                
135400 IMS-GET-WDB201-UNIK SECTION.                                             
135500                                                                          
135600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
135700          DELIMITED BY SIZE INTO SSA1                                     
135800     MOVE '  GE'               TO GODK-STATUSKODER                        
135900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
136000     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     SKIP2                                                                
136400 IMS-GU-WDB201 SECTION.                                                   
136500                                                                          
136600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
136700                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
136800            DELIMITED BY SIZE INTO SSA1                                   
136900     MOVE '  GE'               TO GODK-STATUSKODER                        
137000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
137100     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
137200     PERFORM IMS-STATUSKONTROLL                                           
137300     .                                                                    
137400     SKIP2                                                                
137500 IMS-INSERT-4222-MSG SECTION.                                             
137600                                                                          
137700     IF ENGLISH-TEXT                                                      
137800       MOVE 'N' TO MFS-KDHUVOMR                                           
137900     END-IF                                                               
138000     MOVE LOW-VALUE TO 4222-Z1 4222-Z2                                    
138100     MOVE SPACE TO GODK-STATUSKODER                                       
138200     CALL CBLTDLI USING ISRT 4222-PCB 4222-MSG-IO-AREA                    
138300     MOVE 4222-STATUS-CODE TO STATUS-WS                                   
138400     PERFORM IMS-STATUSKONTROLL                                           
138500     .                                                                    
138600     EJECT                                                                
138700 IMS-01-GU-WDQ2C-WDQ201 SECTION.                                          
138800                                                                          
138900     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
139000          DELIMITED BY SIZE INTO SSA1                                     
139100     MOVE '  GE'               TO GODK-STATUSKODER                        
139200     CALL CBLTDLI USING GU ORQL-PCB DLI-IO-AREA-OHUV SSA1                 
139300     MOVE ORQL-STATUS-CODE    TO STATUS-WS                                
139400     PERFORM IMS-STATUSKONTROLL                                           
139500     .                                                                    
139600     SKIP3                                                                
139700 IMS-ISRT-ORQI-WDQ201 SECTION.                                            
139800                                                                          
139900     MOVE 'WLORQI01 '          TO SSA1                                    
140000     MOVE '    '               TO GODK-STATUSKODER                        
140100     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-OHUV SSA1               
140200     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
140300     PERFORM IMS-STATUSKONTROLL                                           
140400     .                                                                    
140500     SKIP3                                                                
140600 IMS-ISRT-ORQI-WDQ212 SECTION.                                            
140700                                                                          
140800     MOVE 'WLORQI12 '          TO SSA1                                    
140900     MOVE '    '               TO GODK-STATUSKODER                        
141000     CALL CBLTDLI USING ISRT ORQI-PCB DLI-IO-AREA-ARB SSA1                
141100     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
141200     PERFORM IMS-STATUSKONTROLL                                           
141300     .                                                                    
141400                                                                          
141500 IMS-GU-WDB601    SECTION.                                                
141600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
141700          DELIMITED BY SIZE INTO SSA1                                     
141800     MOVE '  GE' TO GODK-STATUSKODER                                      
141900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
142000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
142100     PERFORM IMS-STATUSKONTROLL                                           
142200     IF SEGMENT-SAKNAS                                                    
142300         MOVE SPACE TO DCS-KDDC                                           
142400     END-IF                                                               
142500     .                                                                    
142600     EJECT                                                                
142700 IMS-STATUSKONTROLL SECTION.                                              
142800                                                                          
142900     SET STATUS-IX TO 1                                                   
143000     SEARCH GODK-STATUS                                                   
143100       AT END CALL FELLOG                                                 
143200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
143300     END-SEARCH                                                           
143400     .                                                                    
