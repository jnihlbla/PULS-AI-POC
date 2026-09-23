000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4026100.                                                
000300 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000400 DATE-WRITTEN.   APRIL-91.                                                
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        PROGRAMMET HANTERAR UPPLÄGGNING AV PROFORMA-ORDERHUVUD           
001000*        REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA                      
001100*        HÄMTAS FRÅN KUNDREGISTRET.                                       
001200*        EFTER UPPLÄGGNING AV GODKÄNT ORDERHUVUD SKER UTHOPP TILL         
001300*        REGISTRERING AV PROFORMA-ORDERRADER (4262)                       
001400*                                                                         
001500*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
001600*        PROGRAMMET UPPDATERAR WLORQI (WDE8)  PROFORMA-HUVUD              
001800*        PROGRAMMET LÄSER      WLGMTB (WDB3)  DC-REG                      
001900*        PROGRAMMET LÄSER      WLGMTC (WDB5)  FRAKTKOD-REG                
002100*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)  ORDERNUMMER-REG             
002200*        PROGRAMMET LÄSER              WDM2   KAMPANJ-REG                 
002300*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORT-REG               
002400*        PROGRAMMET LÄSER      WLXXKN (WDR1)  KALENDER-REG                
002500*        PROGRAMMET LÄSER      WDG2           VALUTA-REG                  
002600*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W4T261                                              
003000*        MID:         W4I26101                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W4O26101                                            
003400                                                                          
003500     EJECT                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700                                                                          
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000*    -COPY WY2000W1                                                       
004100     SKIP3                                                                
004200 77  IDPGM                       PIC X(08)   VALUE 'W4026100'.            
004300 77  FELTEXT                     PIC X(70)   VALUE SPACE.                 
004400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +33   COMP SYNC.        
004500*77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +448  COMP SYNC.        
004600 77  4262-MOD-LAENGD             PIC S9(4)  VALUE +111  COMP SYNC.        
004700 77  WS-INDEX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
004900 77  DAGENS-DATUM-PLUS-SJU-DAGAR PIC 9(6)   VALUE ZERO.                   
005000 77  YES                         PIC X(1)   VALUE 'Y'.                    
005100 77  W-IDDISTR                   PIC S9(5)   VALUE +0 COMP-3.             
005200 77  W-IDKUNDNR                  PIC S9(7)   VALUE +0 COMP-3.             
005300 77  W-IDORDNR                   PIC 9(7)    VALUE ZERO.                  
005400 77  WS-TEDDI                    PIC X(11)  VALUE SPACE.                  
005410 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005420 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005500                                                                          
005600 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
005700 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
006100                                                                          
006200 01  WS-IDORDNR-X7.                                                       
006300   03  FILLER                    PIC X(2).                                
006400   03  WS-IDORDNR-X5             PIC X(5).                                
006500                                                                          
006600 77  OK-BEHANDLAD                PIC X(3)    VALUE '101'.                 
006700 77  STARTAD-AV-DISPATCHEN-SW    PIC X       VALUE 'N'.                   
006800     88  STARTAD-AV-DISPATCHEN               VALUE 'J'.                   
006900                                                                          
007000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007100     88  ALLT-OK                             VALUE 'J'.                   
007200                                                                          
007300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007400     88  EGEN-MID                            VALUE '4261'.                
007500                                                                          
007600 01  DATUM-AR-DAGNR              PIC 9(5).                                
007700 01  FILLER REDEFINES DATUM-AR-DAGNR.                                     
007800     03  DATUM-AR                PIC 9(2).                                
007900     03  DATUM-DAGNR             PIC 9(3).                                
008000                                                                          
008100 01  MAX-FORF-DATUM              PIC 9(6)   VALUE ZERO.                   
008200 01  FILLER REDEFINES MAX-FORF-DATUM.                                     
008300     03  MAX-AR                  PIC 9(2).                                
008400     03  MAX-MAN                 PIC 9(2).                                
008500     03  FILLER                  PIC 9(2).                                
008600                                                                          
008700 01  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
008800 01  FILLER1 REDEFINES DAGENS-TID.                                        
008900     03 WS-DAGENS-TID            PIC 9(6).                                
009000     03 FILLER                   PIC 9(2).                                
009100 01  FILLER2 REDEFINES DAGENS-TID.                                        
009200     03 WS-DAGENS-TID-HHMM       PIC 9(4).                                
009300     03 FILLER                   PIC 9(4).                                
009400     EJECT                                                                
009500                                                                          
009600 01  WS-ALFA-1.                                                           
009700     03  WS-NUM-1                PIC 9(1).                                
009800 01  WS-ALFA-2.                                                           
009900     03  WS-NUM-2                PIC 9(2).                                
010000 01  WS-ALFA-4.                                                           
010100     03  WS-NUM-4                PIC 9(4).                                
010200 01  WS-ALFA-6.                                                           
010300     03  WS-NUM-6                PIC 9(6).                                
010400 01  WS-ALFA-7.                                                           
010500     03  WS-NUM-7                PIC 9(7).                                
010600 01  WS-ALFA-10.                                                          
010700     03  WS-NUM-10               PIC 9(10).                               
010800 01  WS-ALFA-1V3.                                                         
010900     03  WS-ALFA-HELTAL          PIC X(1).                                
011000     03  WS-ALFA-PUNKT           PIC X(1).                                
011100     03  WS-ALFA-DECIMAL         PIC X(3).                                
011200 01  WS-NUM-1V3                  PIC 9V9(3).                              
011300 01  FILLER REDEFINES WS-NUM-1V3.                                         
011400     03  WS-NUM-HELTAL           PIC 9(1).                                
011500     03  WS-NUM-DECIMAL          PIC 9(3).                                
011600 01  WS-REOMRTAL                 PIC X(5)    VALUE SPACE.                 
011700 01  WS-REOMRTAL-NUM    REDEFINES WS-REOMRTAL                             
011800                                 PIC 9(1).9(3).                           
011900     EJECT                                                                
012000*    ----DISTR-DEALER-PRICE---------                                      
012100*01  -COPY WWDIST79                                                       
012200*                                                                         
012600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012700 01  GENERELLA-SUBPROGRAM.                                                
012800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013200     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
013300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
013410     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
013500*                                                                         
013600*                                                                         
013700*                                                                         
013800 01  GEMENSAMMA-SUBPROGRAM.                                               
013900     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
014000*        FORMELLA KONTROLLER                                              
014100     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
014200*        LÄSNING AV KUNDREGISTRET                                         
014300     03  W411OHLK                PIC X(8)    VALUE 'W411OHLK'.            
014400*        LOGISKA KONTROLLER                                               
014500     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
014600*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
014900 01  FILLER                PIC X(16) VALUE 'WDATKONV-IO-AREA'.            
015000*   -COPY WDATAREA                                                        
015100     EJECT                                                                
015200 01  FILLER                PIC X(16) VALUE 'WSECURIT-IO-AREA'.            
015300*   -COPY WSECAREA                                                        
015400     EJECT                                                                
015500*   -COPY W402W001                                                        
015600     EJECT                                                                
015700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015800*   -COPY WMEDAREA                                                        
015900     EJECT                                                                
016000 01  MESSAGE-CODES.                                                       
016100     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
016200     03  ERR-SAKNAS-KREG         PIC X(3)    VALUE '063'.                 
016300     03  ERR-ORDERHUVUD-FORMELLT-FEL PIC X(3) VALUE '094'.                
016400     03  ERR-ORDERHUVUD-LOGISKT-FEL  PIC X(3) VALUE '095'.                
016500     03  ERR-MOMS-REGNR-FEL      PIC X(3)    VALUE '223'.                 
016600     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
016700     EJECT                                                                
016800*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
016900*                                                                         
017000*                 WDECEDIT                                                
017100*                                                                         
017200*   -COPY WDECAREA                                                        
017300     EJECT                                                                
017400                                                                          
017500*   -COPY W411OHFK                                                        
017600*                                                                         
017700     EJECT                                                                
017800*                                                                         
017900 01 FILLER                       PIC X(8)    VALUE 'W411KREG'.            
018000*   -COPY W411KREG                                                        
018100*                                                                         
018200     EJECT                                                                
018300*                                                                         
018400 01 FILLER                       PIC X(8)    VALUE 'W411OHLK'.            
018500*   -COPY W411OHLK                                                        
018600*                                                                         
018700     EJECT                                                                
018800*                                                                         
018900 01 FILLER                       PIC X(8)    VALUE 'W411ORDN'.            
019000*   -COPY W411ORDN                                                        
019100*                                                                         
019200*                                                                         
019300     EJECT                                                                
019400*                                                                         
019500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019800     SKIP3                                                                
019900*01  MID -COPY W4I26101                                                   
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020200     SKIP3                                                                
020300*01  -COPY WMSGAREA                                                       
020400     EJECT                                                                
020500*    03  MOD -COPY W4O26101   -RED MSG-AREA.                              
020600     EJECT                                                                
020700*    03  -COPY W4O26201 -PRE 4262- -RED MSG-AREA.                         
020800     EJECT                                                                
020900 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
021000     SKIP3                                                                
021100 01  KOM-IO-AREA.                                                         
021200*03  -COPY WMSGKOM                                                        
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021500     SKIP3                                                                
021600*01  -COPY WMFSAREA                                                       
021700     EJECT                                                                
021800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100                                                                          
022200*    --- STATUS-KOD FRÅN IMS                                              
022300 01  STATUS-WS                   PIC XX.                                  
022400     88  SEGMENT-FINNS                       VALUE '  '.                  
022500     SKIP2                                                                
022600 01  GODK-STATUSKODER.                                                    
022700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022800     SKIP3                                                                
022900 01  SSA1                        PIC X(64).                               
023000 01  SSA2                        PIC X(64).                               
023100     EJECT                                                                
025900*    --- PARAMETRAR TILL W930VAL                                          
026000*01 -COPY W930VAL                                                         
026100     EJECT                                                                
026110*    --- PARAMETRAR TILL W510CURR                                         
026120*01  -COPY W510CURR                                                       
026130     EJECT                                                                
026200*    --- IMS FUNKTIONSKODER                                               
026300*01  -COPY W0003                                                          
026400     EJECT                                                                
026500*    ---  DLI INPUT-OUTPUT AREA                                           
026600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026700     SKIP3                                                                
026800 01  DLI-IO-AREA.                                                         
026900     03  IO-AREA                 PIC X(3000) VALUE SPACE.                 
027000     03  WLPROC01 REDEFINES IO-AREA.                                      
027100*        05  -COPY WDE801                                                 
027200     EJECT                                                                
027300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA3'.         
027400     SKIP3                                                                
029200 LINKAGE SECTION.                                                         
029300                                                                          
029400*01  -COPY W0009      -PRE MSG-                                           
029500     EJECT                                                                
029600*01  -COPY W0009      -PRE DISP-                                          
029700     EJECT                                                                
029800*01  -COPY W0008      -PRE PROC-                                          
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100*01  -COPY W0008     -PRE WDG2-                                           
030200     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
031000     SKIP3                                                                
031100 01  KREG-GMTA-PCB               PIC X.                                   
031200 01  KREG-GMTB-PCB               PIC X.                                   
031300 01  KREG-GMTC-PCB               PIC X.                                   
031400 01  KREG-BETC-PCB               PIC X.                                   
031500 01  OHLK-WDM2-PCB               PIC X.                                   
031600 01  OHLK-WDB6-PCB               PIC X.                                   
031700 01  ORDN-XXKP-PCB               PIC X.                                   
031800 01  ORDN-ORQL-PCB               PIC X.                                   
031900 01  ORDN-PROC-PCB               PIC X.                                   
032000 01  SAP-SAPC-PCB                PIC X.                                   
032100     EJECT                                                                
032200 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB PROC-PCB                      
032300                     WDG2-PCB                                             
032400                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
032500                     KREG-BETC-PCB                                        
032600                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
032700                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
032800                     SAP-SAPC-PCB.                                        
032900 MAIN SECTION.                                                            
033000     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB PROC-PCB                      
033100                     WDG2-PCB                                             
033200                     KREG-GMTA-PCB KREG-GMTB-PCB KREG-GMTC-PCB            
033300                     KREG-BETC-PCB                                        
033400                     OHLK-WDM2-PCB OHLK-WDB6-PCB                          
033500                     ORDN-XXKP-PCB ORDN-ORQL-PCB ORDN-PROC-PCB            
033600                     SAP-SAPC-PCB.                                        
033700     SKIP2                                                                
033800     PERFORM IMS-GET-MSG                                                  
033900     IF SEGMENT-FINNS                                                     
034000        PERFORM IMS-GN-MSG                                                
034100        IF SEGMENT-FINNS                                                  
034200           MOVE JA TO STARTAD-AV-DISPATCHEN-SW                            
034300        END-IF                                                            
034400        PERFORM A-INIT                                                    
034500        IF ALLT-OK                                                        
034600           PERFORM B-KOLLA-O-KOMPLETTERA-INDATA                           
034700           IF ALLT-OK                                                     
034800              PERFORM C-BESTAM-ORDERNUMMER                                
034900              PERFORM D-SKAPA-ORDERHUVUD                                  
035000              IF ALLT-OK                                                  
035100                IF NOT STARTAD-AV-DISPATCHEN                              
035200                   PERFORM E-HOPPA-TILL-RADREGISTRERING                   
035300                END-IF                                                    
035400              END-IF                                                      
035500           END-IF                                                         
035600        END-IF                                                            
035700        IF (NOT ALLT-OK) OR STARTAD-AV-DISPATCHEN                         
035800           PERFORM Z-FINIT                                                
035900        END-IF                                                            
036000     END-IF                                                               
036100                                                                          
036200     MOVE +0 TO RETURN-CODE                                               
036300     GOBACK                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 A-INIT SECTION.                                                          
036700                                                                          
036800     MOVE SPACE                TO MED-IDMFSFEL                            
036900     MOVE JA                   TO ALLT-SW                                 
037000     ACCEPT DAGENS-DATUM FROM DATE                                        
037100     ACCEPT DAGENS-TID   FROM TIME                                        
037200                                                                          
037300     IF MSG-DUBBLA-TRANSKODER                                             
037400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26101                 
037500       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
037600       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
037700     ELSE                                                                 
037800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I26101                  
037900       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
038000       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
038100     END-IF                                                               
038200                                                                          
038300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
038400     MOVE MSG-IDPFK            TO MFS-IDPFK                               
038500     MOVE MFS-IDTRANS          TO W-IDTRANS                               
038600                                                                          
038700     MOVE LOW-VALUE            TO MSG-AREA                                
038800     MOVE 'W4O26101'           TO MFS-IDMOD                               
038900     MOVE '4261'               TO MOD-IDTRANS                             
039000     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
039100                                                                          
039200     IF ENGLISH-TEXT                                                      
039300       MOVE 'GB '              TO MED-IDSKYLT                             
039400     ELSE                                                                 
039500       MOVE 'S  '              TO MED-IDSKYLT                             
039600     END-IF                                                               
039700     EJECT                                                                
039800     IF NOT EGEN-MID                                                      
039900        MOVE NEJ               TO ALLT-SW                                 
040000        PERFORM MFS-RENSA-BILD                                            
040100     ELSE                                                                 
040200        IF NOT STARTAD-AV-DISPATCHEN                                      
040300           PERFORM AA-KOLLA-BEHORIGHET                                    
040400        END-IF                                                            
040500     END-IF                                                               
040600     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
040610     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
040700     .                                                                    
040800     EJECT                                                                
040900                                                                          
041000 AA-KOLLA-BEHORIGHET SECTION.                                             
041100                                                                          
041200     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
041300        MOVE MSG-SIGNON-USERID    TO SEC-IDUSER                           
041400        MOVE '4261'               TO SEC-IDTRANS                          
041500        MOVE MID-IDDISTR          TO SEC-IDKEY                            
041600                                                                          
041700        CALL WSECURIT USING SEC-IDUSER                                    
041800                            SEC-IDTRANS                                   
041900                            SEC-IDKEY                                     
042000                            SEC-KDSVAR                                    
042100                                                                          
042200        IF SEC-KDSVAR = OBEHORIG                                          
042300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR                     
042400           MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                         
042500           MOVE NEJ               TO ALLT-SW                              
042600        END-IF                                                            
042700     ELSE                                                                 
042800        MOVE OBEHORIG             TO SEC-KDSVAR                           
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 B-KOLLA-O-KOMPLETTERA-INDATA SECTION.                                    
043300                                                                          
043400     PERFORM BA-KONTROLLERA-FORMELLA-FEL                                  
043500     IF ALLT-OK                                                           
043600        PERFORM BB-LAS-KUNDREGISTRET                                      
043700        IF ALLT-OK                                                        
043800           PERFORM BC-KONTROLLERA-LOGISKA-FEL                             
043900           IF (NOT ALLT-OK) AND STARTAD-AV-DISPATCHEN                     
044000            MOVE ERR-ORDERHUVUD-LOGISKT-FEL TO MSG-KOM-IDMFSMED           
044100              MOVE '4'                TO MSG-KOM-KDSVAR                   
044200           END-IF                                                         
044300        ELSE                                                              
044400          IF STARTAD-AV-DISPATCHEN                                        
044500             MOVE ERR-SAKNAS-KREG     TO MSG-KOM-IDMFSMED                 
044600             MOVE '4'                 TO MSG-KOM-KDSVAR                   
044700          END-IF                                                          
044800        END-IF                                                            
044900     ELSE                                                                 
045000       IF STARTAD-AV-DISPATCHEN                                           
045100          MOVE ERR-ORDERHUVUD-FORMELLT-FEL TO MSG-KOM-IDMFSMED            
045200          MOVE '4'                    TO MSG-KOM-KDSVAR                   
045300       END-IF                                                             
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700 BA-KONTROLLERA-FORMELLA-FEL SECTION.                                     
045800                                                                          
045900     MOVE 'PROF'               TO OHFK-IDSYSTEM                           
046000     MOVE MID-IDDISTR          TO OHFK-IDDISTR                            
046100     MOVE MID-IDKUNDNR         TO OHFK-IDKUNDNR                           
046200     MOVE MID-IDORDNR          TO WS-IDORDNR-X7                           
046300     MOVE WS-IDORDNR-X5        TO OHFK-IDORDNR                            
046400     MOVE MID-KDORDKL          TO OHFK-KDORDKL                            
046500     IF MID-KDFRAKT = SPACE                                               
046600        MOVE ALL '+'           TO MID-KDFRAKT                             
046700     END-IF                                                               
046800     MOVE MID-KDFRAKT          TO OHFK-KDFRAKT                            
046900     MOVE MID-KDPROTYP         TO OHFK-KDPROTYP                           
047000     MOVE NEJ                  TO OHFK-FLAUTFAK                           
047100                                  OHFK-FLFORBI                            
047200                                  OHFK-FLORDSPE                           
047300                                  OHFK-FLAUTPAC                           
047400                                  OHFK-FLVORKO                            
047500                                  OHFK-FLRESTN                            
047600     IF OHFK-KDPROTYP NOT = 'F'                                           
047700        MOVE JA                TO OHFK-FLLSBOK                            
047800     ELSE                                                                 
047900        MOVE NEJ               TO OHFK-FLLSBOK                            
048000     END-IF                                                               
048100     MOVE SPACE                TO OHFK-IDBIPREF                           
048200     IF MID-IDFTG = SPACE                                                 
048300        MOVE ALL '+'           TO MID-IDFTG                               
048400     END-IF                                                               
048500     MOVE MID-IDFTG            TO OHFK-IDFTG                              
048600     MOVE MID-IDKONTO          TO OHFK-IDKONTO                            
048700     MOVE MID-IDANALYS         TO OHFK-IDANALYS                           
048800     MOVE MID-IDKST            TO OHFK-IDKST                              
048900     MOVE '0000000'            TO OHFK-IDKAMPRF                           
049000     MOVE ALL '+'              TO OHFK-IDDC                               
049100     IF MID-KDFAKTYP = SPACE                                              
049200        MOVE ALL '+'           TO MID-KDFAKTYP                            
049300     END-IF                                                               
049400     IF MID-KDFAKTYP NOT = ALL '+'                                        
049500        MOVE MID-KDFAKTYP      TO OHFK-KDFAKTYP                           
049600     ELSE                                                                 
049700        MOVE '+'               TO OHFK-KDFAKTYP                           
049800     END-IF                                                               
049900     EJECT                                                                
050000     MOVE ALL '+'              TO OHFK-KDROPACK                           
050100                                  OHFK-KDTPOTYP                           
050200                                  OHFK-TITPO                              
050300                                  OHFK-TIRFSDAT                           
050400                                  OHFK-TIRFSTID                           
050500                                  OHFK-KDTULLVE                           
050600                                  OHFK-KDVRINFO                           
050700     MOVE DAGENS-DATUM         TO OHFK-TIREGDAT                           
050800     MOVE WS-DAGENS-TID-HHMM   TO OHFK-TIHHMM                             
050900     MOVE MID-IDSKYLT          TO OHFK-IDSKYLT                            
051000                                                                          
051100     IF MID-KDPROTYP = 'F'                                                
051200        PERFORM BAC-SAETT-FORF-DATUM                                      
051300        MOVE DAGENS-DATUM-PLUS-SJU-DAGAR                                  
051400                               TO OHFK-TIFORDAT                           
051500     ELSE                                                                 
051600        MOVE MID-FORFDAT       TO OHFK-TIFORDAT                           
051700     END-IF                                                               
051800     CALL W411OHFK USING OHFK-W411OHFK                                    
051900                                                                          
052000     PERFORM BAA-KOLLA-FEL-FK                                             
052100     .                                                                    
052200     EJECT                                                                
052300 BAA-KOLLA-FEL-FK SECTION.                                                
052400                                                                          
052500     IF OHFK-IDDISTR-OK = NEJ                                             
052600        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
052700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
052800        MOVE NEJ                 TO ALLT-SW                               
052900     ELSE                                                                 
053000        MOVE OHFK-IDDISTR        TO W-IDDISTR                             
053100     END-IF                                                               
053200                                                                          
053300     MOVE W-IDDISTR              TO DIST79-IDDISTR                        
053400     IF DIST79-DEALER-PRICE                                               
053500        IF ENGLISH-TEXT                                                   
053600           MOVE 'DEALERPRICE'    TO MOD-TEDDI                             
053700                                    WS-TEDDI                              
053800        ELSE                                                              
053900           MOVE '    ÅF PRIS'    TO MOD-TEDDI                             
054000                                    WS-TEDDI                              
054100        END-IF                                                            
054200     ELSE                                                                 
054300        MOVE SPACES              TO MOD-TEDDI                             
054400                                    WS-TEDDI                              
054500     END-IF                                                               
054600                                                                          
054700     IF OHFK-IDKUNDNR-OK = NEJ                                            
054800        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
054900        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
055000        MOVE NEJ                 TO ALLT-SW                               
055100     ELSE                                                                 
055200        IF MID-IDKUNDNR = ALL '+'                                         
055300           MOVE ZERO             TO MOD-IDKUNDNR                          
055400                                    W-IDKUNDNR                            
055500        ELSE                                                              
055600           MOVE OHFK-IDKUNDNR    TO W-IDKUNDNR                            
055700        END-IF                                                            
055800     END-IF                                                               
055900                                                                          
056000     IF OHFK-IDORDNR-OK = NEJ                                             
056100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
056200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
056300        MOVE NEJ                 TO ALLT-SW                               
056400     ELSE                                                                 
056500        IF MID-IDORDNR = ALL '+'                                          
056600           MOVE ZERO             TO W-IDORDNR                             
056700           MOVE MFS-RENSA-FAELT  TO MOD-IDORDNR                           
056800        ELSE                                                              
056900           MOVE OHFK-IDORDNR     TO W-IDORDNR                             
057000        END-IF                                                            
057100     END-IF                                                               
057200     EJECT                                                                
057300                                                                          
057400     IF OHFK-KDORDKL-OK = NEJ                                             
057500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
057600        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
057700        MOVE NEJ                 TO ALLT-SW                               
057800     END-IF                                                               
057900                                                                          
058000     IF OHFK-KDFRAKT-OK = NEJ                                             
058100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
058200        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
058300        MOVE NEJ                 TO ALLT-SW                               
058400     END-IF                                                               
058500                                                                          
058600     IF OHFK-KDPROTYP-OK = NEJ                                            
058700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
058800        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPROTYP-ATTR                      
058900        MOVE NEJ                 TO ALLT-SW                               
059000     END-IF                                                               
059100                                                                          
059200     IF OHFK-IDSKYLT-OK = NEJ                                             
059300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
059400        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDSKYLT-ATTR                      
059500        MOVE NEJ                 TO ALLT-SW                               
059600     END-IF                                                               
059700                                                                          
059800     IF OHFK-IDFTG-OK = NEJ                                               
059900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
060000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
060100        MOVE NEJ                 TO ALLT-SW                               
060200     END-IF                                                               
060300                                                                          
060400     IF OHFK-IDKONTO-OK = NEJ                                             
060500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
060600        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
060700        MOVE NEJ                 TO ALLT-SW                               
060800     END-IF                                                               
060900                                                                          
061000     IF OHFK-IDANALYS-OK = NEJ                                            
061100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
061200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
061300        MOVE NEJ                 TO ALLT-SW                               
061400     END-IF                                                               
061500                                                                          
061600     IF OHFK-IDKST-OK = NEJ                                               
061700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
061800        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
061900        MOVE NEJ                 TO ALLT-SW                               
062000     END-IF                                                               
062100                                                                          
062200     IF OHFK-TIFORDAT-OK = NEJ                                            
062300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
062400        MOVE MFS-NUM-FAELT-FEL   TO MOD-FORFDAT-ATTR                      
062500        MOVE NEJ                 TO ALLT-SW                               
062600     ELSE                                                                 
062700        PERFORM BAAA-KOLLA-DATUM                                          
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 BAAA-KOLLA-DATUM SECTION.                                                
063200      MOVE DAGENS-DATUM        TO MAX-FORF-DATUM                          
063300      ADD 6                    TO MAX-MAN                                 
063400      IF MAX-MAN > 12                                                     
063500         SUBTRACT 12           FROM MAX-MAN                               
063600         ADD 1                 TO MAX-AR                                  
063700      END-IF                                                              
063800                                                                          
063900      IF MID-FORFDAT NUMERIC                                              
064000        MOVE MID-FORFDAT      TO TMP1-YYMMDD                              
064100        MOVE MAX-FORF-DATUM   TO TMP2-YYMMDD                              
064200        PERFORM WY2000P1                                                  
064300        IF TMP1-YYMMDD > TMP2-YYMMDD                                      
064400          MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                           
064500          MOVE MFS-NUM-FAELT-FEL TO MOD-FORFDAT-ATTR                      
064600          MOVE NEJ                TO ALLT-SW                              
064700        END-IF                                                            
064800      END-IF                                                              
064900     .                                                                    
065000     EJECT                                                                
065100 BAC-SAETT-FORF-DATUM  SECTION.                                           
065200                                                                          
065300     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
065400     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
065500                                                                          
065600                                                                          
065700     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
065800                         DAT-O-TIDATUM, DAT-KDSVAR                        
065900     IF DAT-KDSVAR-OK                                                     
066000        MOVE DAT-TIAADDD        TO DATUM-AR-DAGNR                         
066100     ELSE                                                                 
066200*       DATUMKONVERTERING HAR GÅTT FEL                                    
066300        MOVE                                                              
066400*       'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL'                    
066500        'KONVERTERING NUMMER ETT GETT RETURKOD > NOLL'                    
066600                                     TO FELTEXT                           
066700        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
066800     END-IF                                                               
066900                                                                          
067000     COMPUTE DATUM-DAGNR = DATUM-DAGNR + 7                                
067100     IF DATUM-DAGNR > 365                                                 
067200        COMPUTE DATUM-DAGNR = DATUM-DAGNR - 365                           
067300        ADD 1 TO DATUM-AR                                                 
067400     END-IF                                                               
067500                                                                          
067600     MOVE DATUM-AR-DAGNR     TO DAT-I-TIDATUM                             
067700     MOVE 'AADDD'            TO DAT-KDDATFORM                             
067800                                                                          
067900     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
068000                         DAT-O-TIDATUM, DAT-KDSVAR                        
068100     IF DAT-KDSVAR-OK                                                     
068200        MOVE DAT-TIAAMMDD       TO DAGENS-DATUM-PLUS-SJU-DAGAR            
068300     ELSE                                                                 
068400*       DATUMKONVERTERING HAR GÅTT FEL                                    
068500        MOVE                                                              
068600        'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL'                    
068700                                     TO FELTEXT                           
068800        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 BB-LAS-KUNDREGISTRET SECTION.                                            
069300                                                                          
069400     MOVE W-IDDISTR            TO KREG-IDDISTR                            
069500     MOVE W-IDKUNDNR           TO KREG-IDKUNDNR                           
069600     MOVE 'PROF'               TO KREG-IDSYSTEM                           
069700     MOVE SPACE                TO KREG-IDDC-TVS                           
069800     IF MID-KDFRAKT = ALL '+'                                             
069900        MOVE +0                TO KREG-KDFRAKT-IN                         
070000     ELSE                                                                 
070100        MOVE MID-KDFRAKT       TO WS-ALFA-2                               
070200        MOVE WS-NUM-2          TO KREG-KDFRAKT-IN                         
070300     END-IF                                                               
070400     MOVE MID-KDORDKL          TO KREG-KDORDKL                            
070500     IF MID-KDFAKTYP = ALL '+'                                            
070600        MOVE SPACE             TO KREG-KDFAKTYP-IN                        
070700     ELSE                                                                 
070800        MOVE MID-KDFAKTYP      TO KREG-KDFAKTYP-IN                        
070900     END-IF                                                               
071000                                                                          
071100     MOVE NEJ                  TO KREG-FLVORKO                            
071200                                  KREG-FLVORFK                            
071300                                                                          
071400     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB                      
071500                                       KREG-GMTB-PCB                      
071600                                       KREG-GMTC-PCB                      
071700                                       KREG-BETC-PCB                      
071800                                                                          
071900     PERFORM BBA-KOLLA-FEL-KREG                                           
072000     .                                                                    
072100     EJECT                                                                
072200 BBA-KOLLA-FEL-KREG SECTION.                                              
072300                                                                          
072400     IF KREG-IDVAT-OK = NEJ                                               
072500        MOVE ERR-MOMS-REGNR-FEL  TO MED-IDMFSFEL                          
072600        MOVE NEJ                 TO ALLT-SW                               
072700     END-IF                                                               
072800                                                                          
072900     IF KREG-IDDISTR-OK = NEJ                                             
073000        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
073100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
073200        MOVE NEJ                 TO ALLT-SW                               
073300     END-IF                                                               
073400                                                                          
073500     IF KREG-IDKUNDNR-OK = NEJ                                            
073600        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
073700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR                     
073800        MOVE NEJ                 TO ALLT-SW                               
073900     END-IF                                                               
074000                                                                          
074100     IF KREG-KDFRAKT-OK = NEJ                                             
074200        MOVE ERR-SAKNAS-KREG  TO MED-IDMFSFEL                             
074300        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFRAKT-ATTR                      
074400        MOVE NEJ                 TO ALLT-SW                               
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 BC-KONTROLLERA-LOGISKA-FEL SECTION.                                      
074900                                                                          
075000     IF STARTAD-AV-DISPATCHEN                                             
075100        MOVE 'XCEL'            TO OHLK-IDSYSTEM                           
075200     ELSE                                                                 
075300        MOVE 'PROF'            TO OHLK-IDSYSTEM                           
075400     END-IF                                                               
075500     MOVE W-IDDISTR            TO OHLK-IDDISTR                            
075600     MOVE W-IDKUNDNR           TO OHLK-IDKUNDNR                           
075700     MOVE W-IDORDNR            TO OHLK-IDORDNR                            
075800     MOVE MID-KDORDKL          TO WS-ALFA-1                               
075900     MOVE WS-NUM-1             TO OHLK-KDORDKL                            
076000     MOVE KREG-IDDC            TO OHLK-IDDC                               
076100     MOVE SPACE                TO OHLK-IDDC-TVS                           
076200     MOVE SEC-KDSVAR           TO OHLK-SEC-KDSVAR                         
076300     MOVE JA                   TO OHLK-FLAUTORD                           
076400     MOVE NEJ                  TO OHLK-FLVORKO                            
076500     MOVE NEJ                  TO OHLK-FLORDSPE                           
076600     IF MID-IDKONTO = ALL '+'                                             
076700        MOVE +0                TO OHLK-IDKONTO                            
076800     ELSE                                                                 
076900        MOVE MID-IDKONTO       TO WS-ALFA-10                              
077000        MOVE WS-NUM-10         TO OHLK-IDKONTO                            
077100     END-IF                                                               
077200     IF MID-IDANALYS = ALL '+'                                            
077300        MOVE SPACE             TO OHLK-IDANALYS                           
077400     ELSE                                                                 
077500        MOVE MID-IDANALYS      TO OHLK-IDANALYS                           
077600     END-IF                                                               
077700     IF MID-IDKST = ALL '+'                                               
077800        MOVE SPACE             TO OHLK-IDKST                              
077900     ELSE                                                                 
078000        MOVE MID-IDKST         TO OHLK-IDKST                              
078100     END-IF                                                               
078200     EJECT                                                                
078300     IF MID-IDFTG = ALL '+'                                               
078400        MOVE ZERO              TO OHLK-IDFTG                              
078500     ELSE                                                                 
078600        MOVE MID-IDFTG         TO OHLK-IDFTG                              
078700     END-IF                                                               
078800     MOVE +0                   TO OHLK-IDKAMPRF                           
078900     MOVE KREG-FLOKFAK-G       TO OHLK-FLOKFAK-G                          
079000     MOVE KREG-FLOKFAK-N       TO OHLK-FLOKFAK-N                          
079100     MOVE KREG-FLOKFAK-R       TO OHLK-FLOKFAK-R                          
079200     MOVE KREG-FLOKFAK-K       TO OHLK-FLOKFAK-K                          
079300     IF MID-KDPROTYP = 'F'                                                
079400        IF KREG-KDGENFAK NOT = 'N'                                        
079500           MOVE KREG-KDGENFAK  TO OHLK-KDFAKTYP                           
079600        ELSE                                                              
079700           MOVE 'R'            TO OHLK-KDFAKTYP                           
079800        END-IF                                                            
079900     ELSE                                                                 
080000        MOVE OHFK-KDFAKTYP     TO OHLK-KDFAKTYP                           
080100     END-IF                                                               
080200     MOVE +0                   TO OHLK-KDTPOTYP                           
080300                                                                          
080400     MOVE +0                   TO OHLK-TITPO                              
080500                                                                          
080600     CALL W411OHLK USING OHLK-W411OHLK OHLK-WDM2-PCB ORDN-XXKP-PCB        
080700                                       KREG-GMTA-PCB                      
080800                                       SAP-SAPC-PCB                       
080900                                       OHLK-WDB6-PCB                      
081000                                                                          
081100     PERFORM BCA-KOLLA-FEL-LK                                             
081200     .                                                                    
081300     EJECT                                                                
081400 BCA-KOLLA-FEL-LK SECTION.                                                
081500                                                                          
081600     IF OHLK-IDDISTR-OK = NEJ                                             
081700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
081800        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                      
081900        MOVE NEJ                 TO ALLT-SW                               
082000     END-IF                                                               
082100                                                                          
082200     IF OHLK-IDORDNR-OK = NEJ                                             
082300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
082400        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDORDNR-ATTR                      
082500        MOVE NEJ                 TO ALLT-SW                               
082600     END-IF                                                               
082700                                                                          
082800     IF OHLK-KDORDKL-OK = NEJ                                             
082900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
083000        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDORDKL-ATTR                      
083100        MOVE NEJ                 TO ALLT-SW                               
083200     END-IF                                                               
083300                                                                          
083400     IF OHLK-KDFAKTYP-OK = NEJ                                            
083500        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
083600        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDFAKTYP-ATTR                     
083700        MOVE NEJ                 TO ALLT-SW                               
083800     END-IF                                                               
083900                                                                          
084000     IF OHLK-IDFTG-OK = NEJ                                               
084100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
084200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                        
084300        MOVE NEJ                 TO ALLT-SW                               
084400     END-IF                                                               
084500                                                                          
084600     IF OHLK-IDKONTO-OK = NEJ                                             
084700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
084800        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                      
084900        MOVE NEJ                 TO ALLT-SW                               
085000     END-IF                                                               
085100                                                                          
085200     IF OHLK-IDANALYS-OK = NEJ                                            
085300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
085400        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANALYS-ATTR                     
085500        MOVE NEJ                 TO ALLT-SW                               
085600     END-IF                                                               
085700                                                                          
085800     IF OHLK-IDKST-OK = NEJ                                               
085900        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
086000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                        
086100        MOVE NEJ                 TO ALLT-SW                               
086200     END-IF                                                               
086300     .                                                                    
086400     EJECT                                                                
086500 C-BESTAM-ORDERNUMMER SECTION.                                            
086600                                                                          
086700     MOVE 'PROF'               TO ORDN-IDSYSTEM                           
086800     MOVE W-IDDISTR            TO ORDN-IDDISTR                            
086900     MOVE W-IDKUNDNR           TO ORDN-IDKUNDNR                           
087000     MOVE W-IDORDNR            TO ORDN-IDORDNR-IN                         
087100                                                                          
087200     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB                      
087300                         ORDN-ORQL-PCB ORDN-PROC-PCB                      
087400                                                                          
087500     MOVE ORDN-IDORDNR-UT       TO W-IDORDNR                              
087600     .                                                                    
087700     EJECT                                                                
087800 D-SKAPA-ORDERHUVUD SECTION.                                              
087900                                                                          
088000     PERFORM DA-REDIGERA-ORDERHUVUD                                       
088100     IF ALLT-OK                                                           
088200       PERFORM IMS-INSERT-PROFORMA-ORDERHUVUD                             
088300     END-IF                                                               
088400     .                                                                    
088500     EJECT                                                                
088600 DA-REDIGERA-ORDERHUVUD SECTION.                                          
088700                                                                          
088800     MOVE OHLK-IDDISTR             TO PHUV-IDDISTR                        
088900     MOVE OHLK-IDKUNDNR            TO PHUV-IDKUNDNR                       
089000     MOVE ORDN-IDORDNR-UT          TO PHUV-IDKUNDRF                       
089100     IF MID-ADBET = ALL '+'                                               
089200        MOVE KREG-ADBETRAD-1       TO PHUV-ADBETRAD-1                     
089300        MOVE KREG-ADBETRAD-2       TO PHUV-ADBETRAD-2                     
089400     ELSE                                                                 
089500        MOVE MID-ADBET             TO PHUV-ADBET                          
089600        INSPECT PHUV-ADBET  REPLACING ALL '+' BY SPACE                    
089700     END-IF                                                               
089800                                                                          
089900     IF MID-ADGMT = ALL '+'                                               
090000        MOVE KREG-ADGMT-GATA      TO PHUV-ADGMT-GATA                      
090100        MOVE KREG-ADGMT-PADR      TO PHUV-ADGMT-PADR                      
090200        MOVE KREG-ADGMT-LAND      TO PHUV-ADGMT-LAND                      
090300     ELSE                                                                 
090400        MOVE MID-ADGMT            TO PHUV-ADGMT                           
090500        INSPECT PHUV-ADGMT    REPLACING ALL '+' BY SPACE                  
090600     END-IF                                                               
090700                                                                          
090800     IF MID-BEBET = ALL '+'                                               
090900        MOVE KREG-BEBETRAD-1       TO PHUV-BEBETRAD-1                     
091000        MOVE KREG-BEBETRAD-2       TO PHUV-BEBETRAD-2                     
091100     ELSE                                                                 
091200        MOVE MID-BEBET             TO PHUV-BEBET                          
091300        INSPECT PHUV-BEBET  REPLACING ALL '+' BY SPACE                    
091400     END-IF                                                               
091500                                                                          
091600     IF MID-BEGMT = ALL '+'                                               
091700        MOVE KREG-BEGMT-RAD1       TO PHUV-BEGMT-RAD1                     
091800        MOVE KREG-BEGMT-RAD2       TO PHUV-BEGMT-RAD2                     
091900     ELSE                                                                 
092000        MOVE MID-BEGMT             TO PHUV-BEGMT                          
092100        INSPECT PHUV-BEGMT    REPLACING ALL '+' BY SPACE                  
092200     END-IF                                                               
092300                                                                          
092400     IF MID-BEKUNDRF = ALL '+'                                            
092500        MOVE SPACE                 TO PHUV-BEKUNDRF                       
092600     ELSE                                                                 
092700        MOVE MID-BEKUNDRF          TO PHUV-BEKUNDRF                       
092800     END-IF                                                               
092900     EJECT                                                                
093000                                                                          
093100     MOVE SPACE                    TO PHUV-BELOSORT                       
093200                                                                          
093300     IF MID-BEVARREF = ALL '+'                                            
093400        MOVE SPACE                 TO PHUV-BEVARREF                       
093500     ELSE                                                                 
093600        MOVE MID-BEVARREF          TO PHUV-BEVARREF                       
093700     END-IF                                                               
093800                                                                          
093900     MOVE NEJ                      TO PHUV-FLBORT                         
094000                                                                          
094100     IF MID-REOMRTAL NOT = ALL '+'                                        
094200       MOVE W-IDDISTR             TO DIST79-IDDISTR                       
094300       IF DIST79-DEALER-PRICE                                             
094400          MOVE 1.000              TO PHUV-REOMRTAL                        
094500                                     MOD-REOMRTAL                         
094600       END-IF                                                             
094700     END-IF                                                               
094800                                                                          
094900     IF MID-REOMRTAL = ALL '+'                                            
095000        MOVE 1.000                TO PHUV-REOMRTAL                        
095100                                     MOD-REOMRTAL                         
095200     ELSE                                                                 
095300       MOVE MID-REOMRTAL          TO DEC-IDFRIDATA                        
095400       MOVE +1                    TO DEC-KVHELTAL                         
095500       MOVE +3                    TO DEC-KVDECIMAL                        
095600       CALL WDECEDIT USING DEC-WDECAREA                                   
095700       MOVE DEC-IDEDITDATA        TO WS-REOMRTAL-NUM                      
095800       IF DEC-KDSVAR-FEL                                                  
095900         MOVE NEJ                 TO ALLT-SW                              
096000         MOVE MFS-ROER-EJ-FAELT   TO MOD-REOMRTAL                         
096100         MOVE MFS-NUM-FAELT-FEL   TO MOD-REOMRTAL-ATTR                    
096200         MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                         
096300         ELSE                                                             
096400         MOVE WS-REOMRTAL-NUM     TO PHUV-REOMRTAL                        
096500         MOVE WS-REOMRTAL         TO MOD-REOMRTAL                         
096600       END-IF                                                             
096700     END-IF                                                               
096800                                                                          
096900     IF KREG-FLRESTN = YES                                                
097000        MOVE JA                    TO PHUV-FLRESTN                        
097100     ELSE                                                                 
097200        MOVE KREG-FLRESTN          TO PHUV-FLRESTN                        
097300     END-IF                                                               
097400     MOVE OHLK-IDKONTO             TO PHUV-IDKONTO                        
097500     MOVE OHLK-IDANALYS            TO PHUV-IDANALYS                       
097600     MOVE OHLK-IDKST               TO PHUV-IDKST                          
097700     MOVE ORDN-IDORDER-UT          TO PHUV-IDORDER                        
097800                                                                          
097900     IF MID-IDSKYLT = ALL '+'                                             
098000        MOVE KREG-IDSKYLT          TO PHUV-IDSKYLT                        
098100     ELSE                                                                 
098200        MOVE OHFK-IDSKYLT          TO PHUV-IDSKYLT                        
098300     END-IF                                                               
098400     EJECT                                                                
098500                                                                          
098600     MOVE 'PROF'                   TO PHUV-IDSYSTEM                       
098700     MOVE MSG-SIGNON-USERID        TO PHUV-IDUSER                         
098800     MOVE OHLK-IDFTG               TO PHUV-IDFTG                          
098900                                                                          
099000     MOVE ORDN-IDORDNR-UT          TO PHUV-IDKUNDRF-ING(1)                
099100     MOVE +2                       TO WS-INDEX                            
099200     PERFORM UNTIL WS-INDEX > 10                                          
099300        MOVE '0000000   '          TO                                     
099400                    PHUV-IDKUNDRF-ING(WS-INDEX)                           
099500        ADD +1                     TO WS-INDEX                            
099600     END-PERFORM                                                          
099700                                                                          
099800     MOVE ZERO                     TO PHUV-KDLEVVIL                       
099900     MOVE KREG-KDMOMSIN            TO PHUV-KDMOMSIN                       
100000     MOVE OHLK-KDFAKTYP            TO PHUV-KDFAKTYP                       
100100     IF MID-KDFRAKT = ALL '+'                                             
100200        MOVE KREG-KDFRAKT          TO PHUV-KDFRAKT                        
100300     ELSE                                                                 
100400        MOVE KREG-KDFRAKT-IN       TO PHUV-KDFRAKT                        
100500     END-IF                                                               
100600                                                                          
100700     MOVE +0                       TO PHUV-KDORDING                       
100800                                                                          
100900                                                                          
101000     MOVE OHLK-KDORDKL             TO PHUV-KDORDKL                        
101100     MOVE OHFK-KDPROTYP            TO PHUV-KDPROTYP                       
101200                                                                          
101210     MOVE SPACE                    TO 4262-MOD-KDVALISO                   
101400                                                                          
102400     MOVE +0                       TO PHUV-KDVALUTA                       
102500     MOVE +1                       TO TAB-IX                              
102600     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
102700       IF TAB-KDVALISO(TAB-IX) = KREG-KDVALISO                            
102800         MOVE TAB-KDVALUTA(TAB-IX) TO PHUV-KDVALUTA                       
102900         MOVE TAB-IX-MAX           TO TAB-IX                              
103000       END-IF                                                             
103100       ADD +1                      TO TAB-IX                              
103200     END-PERFORM                                                          
103300                                                                          
103500     MOVE KREG-KDVALISO            TO CURR-KDVALISO-ROW                   
103800                                                                          
103810     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
103820     MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                   
103830     MOVE 'M'                      TO CURR-KDVALTYP                       
103840                                                                          
103850     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
104000     IF CURR-KDSVAR = ' '                                                 
104100        MOVE CURR-PRKURS-NEW       TO PHUV-PRKURS                         
104200     ELSE                                                                 
104300        MOVE ZERO                  TO PHUV-PRKURS                         
104400     END-IF                                                               
104500                                                                          
104600     MOVE +0                       TO PHUV-KVKAROSS                       
104700     MOVE +0                       TO PHUV-KVKVBRYT                       
104800     MOVE +0                       TO PHUV-KVMOTOR                        
104900     MOVE +0                       TO PHUV-PRAVDRAG                       
105000     MOVE +0                       TO PHUV-PREMBHNT                       
105100     MOVE +0                       TO PHUV-PRFOERS                        
105200     MOVE +0                       TO PHUV-PRFRAKT                        
105300     MOVE KREG-PRLEGKST            TO PHUV-PRLEGKST                       
105400     MOVE KREG-REAVDRAG            TO PHUV-REAVDRAG                       
105500     MOVE KREG-REEMBHNT            TO PHUV-REEMBHNT                       
105600     MOVE KREG-REFOERS             TO PHUV-REFOERS                        
105700     EJECT                                                                
105800     MOVE +0                       TO PHUV-REOVKOFF                       
105900     MOVE +0                       TO PHUV-SUORDV                         
106000     MOVE +0                       TO PHUV-SUORDV-LOC                     
106100     MOVE +0                       TO PHUV-SUORDV-LOCPREL                 
106200     MOVE +1                       TO WS-INDEX                            
106300     PERFORM UNTIL WS-INDEX > 2                                           
106400        MOVE SPACE                 TO PHUV-TEBANK(WS-INDEX)               
106500        ADD +1                     TO WS-INDEX                            
106600     END-PERFORM                                                          
106700                                                                          
106800     MOVE SPACE                    TO PHUV-TEBANKTO                       
106900                                      PHUV-KDVALISO                       
107000     MOVE +1                       TO WS-INDEX                            
107100     PERFORM UNTIL WS-INDEX > 4                                           
107200        MOVE SPACE                 TO PHUV-TEBETVIL(WS-INDEX)             
107300                                      PHUV-TEGILTIG(WS-INDEX)             
107400                                      PHUV-TEFRITT(WS-INDEX)              
107500        ADD +1                     TO WS-INDEX                            
107600     END-PERFORM                                                          
107700                                                                          
107800     MOVE +1                       TO WS-INDEX                            
107900     PERFORM UNTIL WS-INDEX > 3                                           
108000        MOVE SPACE                 TO PHUV-TELEVVIL(WS-INDEX)             
108100        ADD +1                     TO WS-INDEX                            
108200     END-PERFORM                                                          
108300                                                                          
108400     MOVE SPACE                    TO PHUV-TEPACK                         
108500     MOVE OHFK-TIFORDAT            TO PHUV-TIFORDAT                       
108600     MOVE PHUV-TIFORDAT            TO PHUV-TIGILTIG                       
108700     MOVE +0                       TO PHUV-TIORDDAT                       
108800     MOVE DAGENS-DATUM             TO PHUV-TIREGDAT                       
108900     MOVE WS-DAGENS-TID            TO PHUV-TIREGTID                       
109000     MOVE DAGENS-DATUM             TO PHUV-TIUPPDAT                       
109100     MOVE DAGENS-TID               TO PHUV-TIUPPTID                       
109200     MOVE +0                       TO PHUV-VKORDBTO                       
109300     MOVE +0                       TO PHUV-VKORDNTO                       
109400     MOVE +0                       TO PHUV-VLORDBTO                       
109500     .                                                                    
109600     EJECT                                                                
109700 E-HOPPA-TILL-RADREGISTRERING SECTION.                                    
109800                                                                          
109900     MOVE 'W4O26201'            TO MFS-IDMOD                              
110000                                                                          
110300     MOVE '4262'                TO 4262-MOD-IDTRANS                       
110400     MOVE MFS-RENSA-FAELT       TO 4262-MOD-TEMFSFEL                      
110500                                   4262-MOD-IDDISTR-IN                    
110600                                   4262-MOD-IDKUNDNR-IN                   
110700                                   4262-MOD-IDORDNR-IN                    
110800                                   4262-MOD-IDARTNR-IN                    
110900     MOVE W-IDDISTR             TO WS-NUM-4                               
111000     MOVE WS-NUM-4              TO 4262-MOD-IDDISTR-UT                    
111100     INSPECT 4262-MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
111200     MOVE W-IDKUNDNR            TO WS-NUM-6                               
111300     MOVE WS-NUM-6              TO 4262-MOD-IDKUNDNR-UT                   
111400     INSPECT 4262-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
111500     MOVE W-IDORDNR             TO WS-NUM-7                               
111600     MOVE WS-NUM-7              TO 4262-MOD-IDORDNR-UT                    
111700     INSPECT 4262-MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE          
111800     MOVE MID-KDORDKL           TO 4262-MOD-KDORDKL-UT                    
111900     MOVE PHUV-KDFRAKT          TO 4262-MOD-KDFRAKT-UT                    
112000     MOVE PHUV-KDPROTYP         TO 4262-MOD-KDPROTYP-UT                   
112100     MOVE WS-TEDDI              TO 4262-MOD-TEDDI                         
112200     MOVE MFS-ADD-SAETT-CURSOR  TO 4262-MOD-IDARTNR-ATTR(1)               
112300                                                                          
112400     MOVE 4262-MOD-LAENGD       TO MSG-KVLL                               
112500     PERFORM IMS-INSERT-MSG                                               
112600     .                                                                    
112700     EJECT                                                                
116900 Z-FINIT SECTION.                                                         
117000                                                                          
117100     IF MED-IDMFSFEL NOT = SPACE                                          
117200         CALL WMEDKONV USING MED-WMEDAREA                                 
117300         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
117400     END-IF                                                               
117500     PERFORM MFS-ROER-EJ-BILD                                             
117600                                                                          
117700     IF STARTAD-AV-DISPATCHEN                                             
117800*    SKRIV FEL/KLAR MEDDELANDE TILL MPP DISPATCHERN                       
117900        IF MSG-KOM-IDMFSMED = SPACE                                       
118000           MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                     
118100        END-IF                                                            
118200                                                                          
118300        PERFORM IMS-INSERT-DISP-MSG                                       
118400     ELSE                                                                 
118500        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O26101 + 4                     
118600        PERFORM IMS-INSERT-MSG                                            
118700     END-IF                                                               
118800     .                                                                    
118900     EJECT                                                                
119000 MFS-RENSA-BILD SECTION.                                                  
119100                                                                          
119200     MOVE MFS-RENSA-FAELT  TO   MOD-IDDISTR                               
119300                                MOD-IDKUNDNR                              
119400                                MOD-IDORDNR                               
119500                                MOD-KDORDKL                               
119600                                MOD-KDFRAKT                               
119700                                MOD-KDPROTYP                              
119800                                MOD-KDFAKTYP                              
119900                                MOD-BEKUNDRF                              
120000                                MOD-IDFTG                                 
120100                                MOD-BEVARREF                              
120200                                MOD-IDKONTO                               
120300                                MOD-IDSKYLT                               
120400                                MOD-FORFDAT                               
120500                                MOD-IDANALYS                              
120600                                MOD-IDKST                                 
120700                                MOD-BEGMT-RAD1                            
120800                                MOD-BEGMT-RAD2                            
120900                                MOD-ADGMT-GATA                            
121000                                MOD-ADGMT-PADR                            
121100                                MOD-ADGMT-LAND                            
121200                                MOD-BEBETRAD-1                            
121300                                MOD-BEBETRAD-2                            
121400                                MOD-ADBETRAD-1                            
121500                                MOD-ADBETRAD-2                            
121600                                MOD-ADBETRAD-3                            
121700                                                                          
121800     MOVE 1.000            TO   MOD-REOMRTAL                              
121900     .                                                                    
122000     EJECT                                                                
122100 MFS-ROER-EJ-BILD SECTION.                                                
122200                                                                          
122300     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDISTR                               
122400                                MOD-IDKUNDNR                              
122500                                MOD-IDORDNR                               
122600                                MOD-KDORDKL                               
122700                                MOD-KDFRAKT                               
122800                                MOD-KDPROTYP                              
122900                                MOD-KDFAKTYP                              
123000                                MOD-BEKUNDRF                              
123100                                MOD-IDFTG                                 
123200                                MOD-REOMRTAL                              
123300                                MOD-BEVARREF                              
123400                                MOD-IDKONTO                               
123500                                MOD-IDSKYLT                               
123600                                MOD-FORFDAT                               
123700                                MOD-IDANALYS                              
123800                                MOD-IDKST                                 
123900                                MOD-BEGMT-RAD1                            
124000                                MOD-BEGMT-RAD2                            
124100                                MOD-ADGMT-GATA                            
124200                                MOD-ADGMT-PADR                            
124300                                MOD-ADGMT-LAND                            
124400                                MOD-BEBETRAD-1                            
124500                                MOD-BEBETRAD-2                            
124600                                MOD-ADBETRAD-1                            
124700                                MOD-ADBETRAD-2                            
124800                                MOD-ADBETRAD-3                            
124900     .                                                                    
125000     EJECT                                                                
125100* --- IMS SEKTIONER ---                                                   
125200     SKIP3                                                                
125300 IMS-GET-MSG SECTION.                                                     
125400                                                                          
125500     MOVE '  QC' TO GODK-STATUSKODER                                      
125600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
125700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
125800     PERFORM IMS-STATUSKONTROLL                                           
125900     .                                                                    
126000     EJECT                                                                
126100 IMS-GN-MSG SECTION.                                                      
126200                                                                          
126300     MOVE '  QD'   TO GODK-STATUSKODER                                    
126400     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
126500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126900 IMS-INSERT-DISP-MSG SECTION.                                             
127000                                                                          
127100     MOVE '  '  TO GODK-STATUSKODER                                       
127200     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
127300     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
127400     PERFORM IMS-STATUSKONTROLL                                           
127500     .                                                                    
127600     SKIP3                                                                
127700 IMS-INSERT-MSG SECTION.                                                  
127800                                                                          
127900     IF ENGLISH-TEXT                                                      
128000       MOVE 'N' TO MFS-KDHUVOMR                                           
128100     END-IF                                                               
128200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
128300     MOVE SPACE TO GODK-STATUSKODER                                       
128400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
128500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
128600     PERFORM IMS-STATUSKONTROLL                                           
128700     .                                                                    
128800     EJECT                                                                
128900 IMS-INSERT-PROFORMA-ORDERHUVUD SECTION.                                  
129000                                                                          
129100     MOVE 'WLPROC01 '          TO SSA1                                    
129200     MOVE '    '               TO GODK-STATUSKODER                        
129300     CALL CBLTDLI USING ISRT PROC-PCB DLI-IO-AREA SSA1                    
129400     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
129500     PERFORM IMS-STATUSKONTROLL                                           
129600     .                                                                    
129700     EJECT                                                                
134100 IMS-STATUSKONTROLL SECTION.                                              
134200                                                                          
134300     SET STATUS-IX TO 1                                                   
134400     SEARCH GODK-STATUS                                                   
134500       AT END CALL FELLOG                                                 
134600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
134700     END-SEARCH                                                           
134800     .                                                                    
134900     EJECT                                                                
135000*    -COPY WY2000P1                                                       
