000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011700.                                                
000300 AUTHOR.         EVA LUNDELL.                                             
000400 DATE-WRITTEN.   95/10/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        STANSBILD FÖR UPPLÄGGNING AV R34-TRANSAR. PROGRAMMET             
000900*        SKICKAR TRANSAR TILL PROGRAM W611C.                              
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001200*        PROGRAMMET LÄSER              WDK7                               
001300*        PROGRAMMET LÄSER              WDB6                               
001400*                                                                         
001500*        PROGRAMMET ANROPAR    W411SAP FÖR KONTROLL AV                    
001600*                              KONTERINGSINFO, WLSAPC (WDH3)              
001700*    INDATA.                                                              
001800*        TRANSAKTION: W6T117                                              
001900*        MID:         W6I11701                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W6O11701                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900     SKIP2                                                                
003000 77  IDPGM                       PIC X(8)    VALUE 'W6011700'.            
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003210 77  WS-IDFTG-B6                 PIC 9(2)    VALUE ZERO.                  
003300 77  INDX                        PIC S9(3)   VALUE +0 COMP-3.             
003400 77  MAX-INDX                    PIC S9(3)   VALUE +14 COMP-3.            
003500 77  611C-IX                     PIC S9(4)   VALUE +0 COMP SYNC.          
003600 77  611C-MAX                    PIC S9(4)   VALUE +22 COMP SYNC.         
003700 77  TEST-KDRT                   PIC 9(2)    VALUE ZERO.                  
003800 77  WS-IDTTYP                   PIC X(3)    VALUE SPACE.                 
003900 77  WS-IDKONTO                  PIC X(10)   VALUE SPACE.                 
004000 77  WS-IDKONTO-NUM              PIC 9(10)   VALUE ZERO.                  
004100 77  WS-IDANALYS                 PIC X(12)   VALUE SPACE.                 
004110 77  WS-IDKST                    PIC X(10)   VALUE SPACE.                 
004200 77  WS-IDLAND-SPR               PIC X(2)    VALUE SPACE.                 
004300 77  WS-VKART-K7                 PIC 9(7)    VALUE ZERO.                  
004400 77  WS-VLARTNTO-K7              PIC 9(8)V9(1) VALUE ZERO.                
004500 77  WS-KDARTURS-K7              PIC X(2) VALUE SPACE.                    
004600 01  WS-AVIS.                                                             
004700     03 WS-KVAVIS OCCURS 14      PIC S9(8)   VALUE +0 COMP-3.             
004800 01  WS-LOPNR-R34.                                                        
004900     03  WS-VV                   PIC 9(2)  VALUE ZERO.                    
005000     03  WS-D                    PIC 9(1)  VALUE ZERO.                    
005100     03  WS-LOPNR-ZERO           PIC 9(5)  VALUE ZERO.                    
005200 01  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17 COMP SYNC.         
005300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM.                                       
005500     03  DAGENS-AA               PIC 9(2).                                
005600     03  DAGENS-MM               PIC 9(2).                                
005700     03  DAGENS-DD               PIC 9(2).                                
005800                                                                          
005900 01  WS-IDDC-LOCAL.                                                       
006000     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
006100     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
006200     03  FILLER                  PIC X(1)   VALUE SPACE.                  
006300                                                                          
006400 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
006500                                                                          
006600 01  TEST-DATUM                PIC 9(6)    VALUE ZERO.                    
006700 01  FILLER REDEFINES TEST-DATUM.                                         
006800     03  TEST-AA                 PIC 9(2).                                
006900     03  TEST-MM                 PIC 9(2).                                
007000     03  TEST-DD                 PIC 9(2).                                
007100                                                                          
007200 01  TMP1-YYMMDD               PIC 9(6)    VALUE ZERO.                    
007300 01  FILLER REDEFINES TMP1-YYMMDD.                                        
007400     03  TMP1-YY                 PIC 9(2).                                
007500     03  TMP1-MM                 PIC 9(2).                                
007600     03  TMP1-DD                 PIC 9(2).                                
007700                                                                          
007800 01  TMP2-YYMMDD               PIC 9(6)    VALUE ZERO.                    
007900 01  FILLER REDEFINES TMP2-YYMMDD.                                        
008000     03  TMP2-YY                 PIC 9(2).                                
008100     03  TMP2-MM                 PIC 9(2).                                
008200     03  TMP2-DD                 PIC 9(2).                                
008300                                                                          
008400 01  TEST-IDARTNR                PIC 9(9) COMP-3.                         
008500                                                                          
008600 01  BUMPER-IDARTNR REDEFINES TEST-IDARTNR PIC S9(9) COMP-3.              
008700       88  BUMPER            VALUE  3207235                               
008800                                    3207236                               
008900                                    3296330                               
009000                                    3296873                               
009100                                    3342319                               
009200                                    3342320                               
009300                                    3342321                               
009400                                    3342322                               
009500                                    3343633                               
009600                                    3343634                               
009700                                    3433204                               
009800                                    3433205                               
009900                                    3434364                               
010000                                    3434368                               
010100                                    3434675                               
010200                                    3445266                               
010300                                    3445278                               
010400                                    3472189                               
010500                                    3472190                               
010600                                    3472267.                              
010700                                                                          
010800                                                                          
010900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
011000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
011100                                                                          
011200                                                                          
011300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011400                                                                          
011500                                                                          
011600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011700     88  NYCKLAR-OK                          VALUE 'J'.                   
011800     88  NYCKLAR-FEL                         VALUE 'N'.                   
011900                                                                          
012000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012100     88  INDATA-OK                           VALUE 'J'.                   
012200     88  INDATA-FEL                          VALUE 'N'.                   
012300                                                                          
012400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012500     88  EGEN-MID                            VALUE '6117'.                
012600     88  GODK-MID                            VALUE '6111' '6112'          
012700                                                   '6113' '6114'          
012800                                                   '6115' '6116'          
012900                                                   '6117' '6118'          
013000                                                   '6119'.                
013100     88  HELP-MID                            VALUE '0551'.                
013200     EJECT                                                                
013300*      --- VALID ID-C CODES                                               
013400*                                                                         
013500*01    -COPY WWDC99                                                       
013600       EJECT                                                              
013700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013800 01  GENERELLA-SUBPROGRAM.                                                
013900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
014500     03  W411SAP                 PIC X(8)    VALUE 'W411SAP'.             
014600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
014900*01 -COPY WDATAREA                                                        
015000     EJECT                                                                
015100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015200*01 -COPY WMEDAREA                                                        
015300     SKIP3                                                                
015400 01  MESSAGE-CODES.                                                       
015500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015600     03  ERR-CORR-FIELDS         PIC X(3)    VALUE '001'.                 
015700     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
015800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015900     03  ERR-ART-UTG             PIC X(3)    VALUE '018'.                 
016000     03  ERR-PRIS-SAKNAS         PIC X(3)    VALUE '301'.                 
016100     03  ERR-TRYCK-PF11          PIC X(3)    VALUE '003'.                 
016200     03  ERR-PLACE-MISSING       PIC X(3)    VALUE '764'.                 
016300     03  ERR-WEIGHT-MISSING      PIC X(3)    VALUE '792'.                 
016400     03  ERR-VOLUME-MISSING      PIC X(3)    VALUE '793'.                 
016500     03  ERR-ORIGIN-MISSING      PIC X(3)    VALUE '794'.                 
016600     EJECT                                                                
016700*01  -COPY WDECAREA                                                       
016800     EJECT                                                                
016900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017000*                                                                         
017100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017200     SKIP3                                                                
017300*01 -COPY WMSGINIT                                                        
017400     SKIP3                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'W411SAP '.            
017600     SKIP3                                                                
017700*01 -COPY W411SAP                                                         
017800     SKIP3                                                                
017900*01  -COPY WWIDFTG                                                        
018000     EJECT                                                                
018100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018200*                                                                         
018300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018400     SKIP3                                                                
018500*01  MID -COPY W6I11701                                                   
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
018800     SKIP3                                                                
018900 01  KOM-MSG-IO-AREA.                                                     
019000*03  -COPY WMSGKOM                                                        
019100     EJECT                                                                
019200 01  FILLER -COPY WMSGSNUF -PRE SNUF-                                     
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019500     SKIP3                                                                
019600*01  -COPY WMSGAREA                                                       
019700     EJECT                                                                
019800     03  MOD REDEFINES MSG-AREA.                                          
019900*      05  -COPY W6O11701                                                 
020000     EJECT                                                                
020100 01  MID611C-START               PIC X(16) VALUE                          
020200                                      'MID611C-START'.                    
020300 01  611C-MSG-IO-AREA.                                                    
020400     03  611C-LL                 PIC S9(4) VALUE +0 COMP SYNC.            
020500     03  611C-Z1                 PIC X     VALUE LOW-VALUE.               
020600     03  611C-Z2                 PIC X     VALUE LOW-VALUE.               
020700     03  611C-TRANSKOD           PIC X(8)  VALUE 'W6T11CX '.              
020800     03  611C-IDTRANS            PIC X(4)  VALUE '611C'.                  
020900     03  611C-KDMFSFOR           PIC X.                                   
021000     03  FILLER.                                                          
021100*       05 -COPY W6I11C01    -PRE MOD611C-                                
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021400     SKIP3                                                                
021500*01  -COPY WMFSAREA                                                       
021600     EJECT                                                                
021700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021800*                                                                         
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100     SKIP3                                                                
022200 01  NYCKLAR-TILL-DLI.                                                    
022300     03  W-IDARTNR-X.                                                     
022400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
022500     03  W-IDARTNR-WDK7-X.                                                
022600         05  W-IDARTNR-WDK7      PIC S9(9)   VALUE ZERO COMP-3.           
022700     03  W-IDDC-X.                                                        
022800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
022900     03  W-IDLAND-X.                                                      
023000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
023100     03  W-KDSEGKEY-X.                                                    
023200         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
023300     03  W-WDG6KEY-X.                                                     
023400         05  W-WDG6KEY           PIC X(18)    VALUE SPACE.                
023500     03  W-KDSEGKEY-X.                                                    
023600         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
023700     SKIP2                                                                
023800*    --- STATUS-KOD FRÅN IMS                                              
023900 01  STATUS-WS                   PIC XX.                                  
024000     88  SEGMENT-FINNS                       VALUE '  '.                  
024100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024300     SKIP2                                                                
024400 01  GODK-STATUSKODER.                                                    
024500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024600     SKIP3                                                                
024700 01  SSA1                        PIC X(64).                               
024800 01  SSA2                        PIC X(64).                               
024900     EJECT                                                                
025000*    --- IMS FUNKTIONSKODER                                               
025100*01  -COPY W0003                                                          
025200     EJECT                                                                
025300*    ---  DLI INPUT-OUTPUT AREA                                           
025400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025500     SKIP3                                                                
025600 01  DLI-IO-AREA.                                                         
025700     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
025800     SKIP3                                                                
025900     03  WLARTC01 REDEFINES IO-AREA.                                      
026000*        05  -COPY WDK601                                                 
026100     SKIP3                                                                
026200     03  WLARTC11 REDEFINES IO-AREA.                                      
026300*        05  -COPY WDK611                                                 
026400     EJECT                                                                
026500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
026600     SKIP3                                                                
026700 01  DLI-IO-AREA2.                                                        
026800     03  IO-AREA2                PIC X(600)  VALUE SPACE.                 
026900     SKIP3                                                                
027000     03  WLARTC01 REDEFINES IO-AREA2.                                     
027100*        05  -COPY WDK701                                                 
027200     SKIP3                                                                
027300     03  WLARTC11 REDEFINES IO-AREA2.                                     
027400*        05  -COPY WDK711                                                 
027500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK712'.        
027600 01  DLI-IO-WDK712.                                                       
027700*    03  -COPY WDK712                                                     
027800     EJECT                                                                
027900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDB601'.        
028000 01  DLI-IO-WDB601.                                                       
028100*    03  -COPY WDB601                                                     
028200     EJECT                                                                
028300 LINKAGE SECTION.                                                         
028400                                                                          
028500*01  -COPY W0009   -PRE MSG-                                              
028600*01  -COPY W0009   -PRE DISP-                                             
028700*01  -COPY W0009   -PRE 611C-                                             
028800     EJECT                                                                
028900*01  -COPY W0008   -PRE KOMA-                                             
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200     EJECT                                                                
029300*01  -COPY W0008  -PRE ARTC-                                              
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE WDK7-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE USEA-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE WDB6-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE SAPC-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB 611C-PCB KOMA-PCB             
030900                           ARTC-PCB WDK7-PCB USEA-PCB                     
031000                           WDB6-PCB SAPC-PCB.                             
031100     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 611C-PCB KOMA-PCB             
031200                           ARTC-PCB WDK7-PCB USEA-PCB                     
031300                           WDB6-PCB SAPC-PCB.                             
031400                                                                          
031500     PERFORM IMS-GET-MSG                                                  
031600     IF SEGMENT-FINNS                                                     
031700       PERFORM A-INIT                                                     
031800       PERFORM B-KOLLA-NYCKLAR-INDATA                                     
031900       IF NYCKLAR-OK                                                      
032000         IF MFS-UPDATE OR MFS-UPD-V                                       
032100             PERFORM G-KOLLA-INPUT                                        
032200                                                                          
032300             IF INDATA-OK                                                 
032400                 PERFORM H-SKAPA-SKICKA-TRANS                             
032500                 PERFORM MFS-RENSA-FAELT-IN                               
032600                 PERFORM MFS-RENSA-FAELT-UT                               
032700                 PERFORM S01-VISA-DEFAULT                                 
032800                 MOVE INF-UPDATE-DONE TO MED-IDMFSMED                     
032900                 CALL WMEDKONV USING MED-WMEDAREA                         
033000                 MOVE MED-TEMFSMED TO MOD-TEMFSINF                        
033100             END-IF                                                       
033200         ELSE                                                             
033300             MOVE ERR-TRYCK-PF11  TO MED-IDMFSFEL                         
033400             CALL WMEDKONV USING MED-WMEDAREA                             
033500             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
033600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
033700             IF MFS-ENTER                                                 
033800                 IF EGEN-MID OR GODK-MID                                  
033900                     IF MID-R34POST (1) = ALL '+'                         
034000                         PERFORM S01-VISA-DEFAULT                         
034100                     END-IF                                               
034200                 END-IF                                                   
034300             ELSE                                                         
034400                 PERFORM S01-VISA-DEFAULT                                 
034500             END-IF                                                       
034600         END-IF                                                           
034700       END-IF                                                             
034800       PERFORM IMS-INSERT-MSG                                             
034900     END-IF                                                               
035000                                                                          
035100     MOVE ZERO TO RETURN-CODE                                             
035200     GOBACK                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 A-INIT SECTION.                                                          
035600                                                                          
035700     IF MSG-DUBBLA-TRANSKODER                                             
035800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11701                 
035900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036100     ELSE                                                                 
036200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11701                  
036300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
036400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036500     END-IF                                                               
036600                                                                          
036700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
036800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
036900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037000                                                                          
037100     MOVE LOW-VALUE TO MSG-AREA                                           
037200     MOVE 'W6O117N1' TO MFS-IDMOD                                         
037300     MOVE '6117' TO MOD-IDTRANS                                           
037400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
037500                                                                          
037600     COMPUTE MSG-KVLL = LENGTH OF MOD-W6O11701 + 4                        
037700                                                                          
037800     ACCEPT DAGENS-DATUM FROM DATE                                        
037900     ACCEPT DAGENS-TID   FROM TIME                                        
038000     IF EGEN-MID OR HELP-MID                                              
038100       CONTINUE                                                           
038200     ELSE                                                                 
038300       MOVE SPACE TO MFS-KDTRTYP                                          
038400       MOVE '7' TO MFS-IDPFK                                              
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800 B-KOLLA-NYCKLAR-INDATA SECTION.                                          
038900     SKIP2                                                                
039000     PERFORM BA-KOLLA-NYCKLAR                                             
039100     IF NYCKLAR-OK                                                        
039200       CONTINUE                                                           
039300     ELSE                                                                 
039400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
039500       CALL WMEDKONV USING MED-WMEDAREA                                   
039600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 BA-KOLLA-NYCKLAR SECTION.                                                
040200     SKIP2                                                                
040300                                                                          
040400     MOVE ALL '+'  TO        MSGI-WMSGINIT                                
040500     MOVE '001'    TO        MSGI-KDCALL                                  
040600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040800                                                                          
040900     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
041000                             WS-IDLAND-SPR                                
041100                                                                          
041200     MOVE JA TO NYCKLAR-SW                                                
041300*    INSPECT MSGI-IDDC REPLACING LEADING SPACE BY ZERO                    
041400*    IF MSGI-IDDC NUMERIC                                                 
041500     MOVE MSGI-IDDC  TO WS-IDDC                                           
041600     IF GOOD-DC                                                           
041700         MOVE MSGI-IDDC TO W-IDDC                                         
041800                           WS-IDDC                                        
041900                           MOD-IDDC-IN                                    
042000     ELSE                                                                 
042100         MOVE NEJ TO NYCKLAR-SW                                           
042200         MOVE SPACE TO WS-IDDC                                            
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 G-KOLLA-INPUT SECTION.                                                   
042700     SKIP2                                                                
042800     IF MID-TIAVIDAT = ALL '+'                                            
042900       MOVE MFS-ALFA-FAELT-FEL   TO MOD-TIAVIDAT-ATTR                     
043000       MOVE NEJ                  TO INDATA-SW                             
043100     ELSE                                                                 
043200       INSPECT MID-TIAVIDAT REPLACING ALL SPACE BY ZERO                   
043300       IF MID-TIAVIDAT NOT NUMERIC                                        
043400         MOVE MFS-ALFA-FAELT-FEL TO MOD-TIAVIDAT-ATTR                     
043500         MOVE NEJ                TO INDATA-SW                             
043600       END-IF                                                             
043700     END-IF                                                               
043800                                                                          
043900     IF INDATA-OK                                                         
044000       PERFORM GA-KOLLA-RADER                                             
044100     END-IF                                                               
044200                                                                          
044300     IF INDATA-OK                                                         
044400         PERFORM GB-FORMELLA-KONTROLLER                                   
044500         IF INDATA-OK                                                     
044600            CONTINUE                                                      
044700         ELSE                                                             
044800           CALL WMEDKONV USING MED-WMEDAREA                               
044900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
045000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
045100         END-IF                                                           
045200     ELSE                                                                 
045300         IF SAP-BEFEL = SPACE                                             
045400            MOVE ERR-CORR-FIELDS TO MED-IDMFSFEL                          
045500            CALL WMEDKONV USING MED-WMEDAREA                              
045600            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
045700         END-IF                                                           
045800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 GA-KOLLA-RADER SECTION.                                                  
046300     SKIP2                                                                
046400     MOVE +1 TO INDX                                                      
046500     MOVE JA        TO INDATA-SW                                          
046600     PERFORM UNTIL INDX > MAX-INDX                                        
046700       IF MID-IDARTNR (INDX)       = ALL '+'                              
046800       AND MID-IDLEVNR (INDX)      = ALL '+'                              
046900       AND MID-IDKONTO (INDX)      = ALL '+'                              
047000       AND MID-IDAVINR (INDX)      = ALL '+'                              
047100       AND MID-KVAVIS (INDX)       = ALL '+'                              
047200       AND MID-IDARTNR-FROM (INDX) = ALL '+'                              
047300       AND MID-IDANALYS (INDX)     = ALL '+'                              
047400       AND MID-IDKST (INDX)        = ALL '+'                              
047500           IF INDX = +1                                                   
047600               MOVE NEJ TO INDATA-SW                                      
047700               MOVE ERR-CORR-FIELDS TO MED-IDMFSFEL                       
047800           END-IF                                                         
047900           MOVE +99 TO INDX                                               
048000       ELSE                                                               
048100         IF MID-IDLEVNR (INDX)  = ALL '+'                                 
048200            MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-ATTR (INDX)           
048300            MOVE NEJ TO INDATA-SW                                         
048400         END-IF                                                           
048500                                                                          
048600         IF MID-KDRT (INDX) = ALL '+'                                     
048700             MOVE MFS-NUM-FAELT-FEL TO MOD-KDRT-ATTR (INDX)               
048800             MOVE NEJ TO INDATA-SW                                        
048900         ELSE                                                             
049000             INSPECT MID-KDRT (INDX) REPLACING ALL SPACE BY ZERO          
049100             IF NDC-NA                                                    
049200               IF MID-KDRT (INDX) = '40' OR '41' OR '42' OR '43'          
049300                 OR '44' OR '45' OR '46' OR '47'                          
049310                 OR '80' OR '92'                                          
049400                 CONTINUE                                                 
049500               ELSE                                                       
049600                 MOVE NEJ TO INDATA-SW                                    
049700                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDRT-ATTR (INDX)           
049800               END-IF                                                     
049900             ELSE                                                         
050000               IF MID-KDRT (INDX) = '06'                                  
050200                 CONTINUE                                                 
050300               ELSE                                                       
050400                 MOVE NEJ TO INDATA-SW                                    
050500                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDRT-ATTR (INDX)           
050600               END-IF                                                     
050700             END-IF                                                       
050800         END-IF                                                           
050900                                                                          
051000         IF MID-IDAVINR (INDX) = ALL '+'                                  
051100             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAVINR-ATTR (INDX)           
051200             MOVE NEJ TO INDATA-SW                                        
051300         ELSE                                                             
051400             INSPECT MID-IDAVINR (INDX) REPLACING LEADING SPACE           
051500                 BY ZERO                                                  
051600             IF MID-IDAVINR (INDX) NOT NUMERIC OR                         
051700                MID-IDAVINR (INDX) = ZERO                                 
051800                 MOVE MFS-ALFA-FAELT-FEL TO                               
051900                        MOD-IDAVINR-ATTR (INDX)                           
052000                 MOVE NEJ TO INDATA-SW                                    
052100             END-IF                                                       
052200         END-IF                                                           
052300                                                                          
052400                                                                          
052500         IF MID-IDARTNR (INDX) = ALL '+'                                  
052600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)           
052700             MOVE NEJ TO INDATA-SW                                        
052800         ELSE                                                             
052900             INSPECT MID-IDARTNR(INDX) REPLACING ALL SPACE                
053000                 BY ZERO                                                  
053100             IF MID-IDARTNR (INDX) NOT NUMERIC                            
053200                 MOVE NEJ TO INDATA-SW                                    
053300                 MOVE MFS-ALFA-FAELT-FEL TO                               
053400                     MOD-IDARTNR-ATTR (INDX)                              
053500             END-IF                                                       
053600         END-IF                                                           
053700                                                                          
053800         IF MID-KVAVIS (INDX) NOT = ALL '+'                               
053900            MOVE MID-KVAVIS (INDX) TO DEC-IDFRIDATA                       
054000            MOVE 7                 TO DEC-KVHELTAL                        
054100            MOVE 0                 TO DEC-KVDECIMAL                       
054200                                                                          
054300            CALL WDECEDIT USING DEC-WDECAREA                              
054400                                                                          
054500            IF DEC-KDSVAR-OK                                              
054600              IF NDC-NA                                                   
054700                IF DEC-IDEDITDATA > ZERO                                  
054800                   MOVE DEC-IDEDITDATA TO WS-KVAVIS(INDX)                 
054900                ELSE                                                      
055000                   MOVE NEJ TO INDATA-SW                                  
055100                   MOVE MFS-ALFA-FAELT-FEL TO                             
055200                       MOD-KVAVIS-ATTR (INDX)                             
055300                END-IF                                                    
055400              ELSE                                                        
055500                IF DEC-IDEDITDATA = ZERO                                  
055600                  MOVE NEJ TO INDATA-SW                                   
055700                  MOVE MFS-ALFA-FAELT-FEL TO                              
055800                       MOD-KVAVIS-ATTR (INDX)                             
055900                ELSE                                                      
056000                  MOVE DEC-IDEDITDATA TO WS-KVAVIS (INDX)                 
056100                END-IF                                                    
056200              END-IF                                                      
056300            ELSE                                                          
056400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KVAVIS-ATTR (INDX)           
056500              MOVE NEJ         TO INDATA-SW                               
056600            END-IF                                                        
056700         ELSE                                                             
056800            MOVE MFS-ALFA-FAELT-FEL TO MOD-KVAVIS-ATTR (INDX)             
056900            MOVE NEJ         TO INDATA-SW                                 
057000         END-IF                                                           
057100                                                                          
057200         IF NDC-NA                                                        
057400            MOVE +0 TO WS-IDKONTO-NUM                                     
057500            MOVE SPACE TO WS-IDANALYS WS-IDKST                            
057600         ELSE                                                             
057700           IF MID-IDKONTO(INDX) NOT = ALL '+'                             
057800             MOVE MID-IDKONTO(INDX)  TO WS-IDKONTO                        
057900             INSPECT WS-IDKONTO REPLACING LEADING SPACE BY ZERO           
058000             IF WS-IDKONTO NUMERIC                                        
058100               MOVE WS-IDKONTO       TO WS-IDKONTO-NUM                    
058200               MOVE WS-IDKONTO-NUM   TO SAP-IDKONTO                       
058300             ELSE                                                         
058400               MOVE ZERO             TO SAP-IDKONTO                       
058500             END-IF                                                       
058600           ELSE                                                           
058700             MOVE ZERO               TO SAP-IDKONTO                       
058800           END-IF                                                         
058900           IF MID-IDANALYS(INDX) NOT = ALL '+'                            
059000             MOVE MID-IDANALYS(INDX) TO WS-IDANALYS                       
059100             INSPECT WS-IDANALYS REPLACING LEADING SPACE BY ZERO          
059200             IF WS-IDANALYS = ALL '0'                                     
059300               MOVE SPACE            TO SAP-IDANALYS                      
059400             ELSE                                                         
059500               MOVE WS-IDANALYS      TO SAP-IDANALYS                      
059600             END-IF                                                       
059700           ELSE                                                           
059800             MOVE SPACE              TO SAP-IDANALYS                      
059900           END-IF                                                         
060000           IF MID-IDKST(INDX) NOT = ALL '+'                               
060100             MOVE MID-IDKST(INDX)    TO SAP-IDKST                         
060110                                        WS-IDKST                          
060200           ELSE                                                           
060300             MOVE SPACE              TO SAP-IDKST                         
060400           END-IF                                                         
060500                                                                          
060600           MOVE MSGI-IDFTG           TO WS-IDFTG                          
060700                                        WS-IDFTG-B6                       
061510           IF IDFTG-NON-VCC                                               
061520             PERFORM IMS-GU-WDB601-FTG                                    
061530             IF SEGMENT-FINNS                                             
061540               MOVE DCS-KDTRADP      TO SAP-KDTRADP                       
061550             ELSE                                                         
061560               MOVE SPACES           TO SAP-KDTRADP                       
061570             END-IF                                                       
061580           ELSE                                                           
061590             MOVE 'SEPV'             TO SAP-KDTRADP                       
061591           END-IF                                                         
061600           MOVE ZERO                 TO SAP-IDDISTR                       
061700           MOVE SPACE                TO SAP-KDFAKTYP                      
061800           MOVE ZERO                 TO SAP-IDFTG                         
061900           MOVE SPACE                TO SAP-IDPROFIT                      
062000           MOVE +2                   TO SAP-KDCALL                        
062100                                                                          
062200           CALL W411SAP USING SAP-W411SAP SAPC-PCB                        
062300                                                                          
062400           IF SAP-BEFEL NOT = SPACE                                       
062500             MOVE SAP-BEFEL            TO MOD-TEMFSFEL                    
062600             MOVE NEJ                  TO INDATA-SW                       
062700             IF SAP-IDKONTO-OK = NEJ                                      
062800               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKONTO-ATTR (INDX)         
062900             ELSE                                                         
063000               IF SAP-IDANALYS-OK = NEJ                                   
063100                 MOVE MFS-ALFA-FAELT-FEL                                  
063200                                       TO MOD-IDANALYS-ATTR (INDX)        
063300               ELSE                                                       
063400                 IF SAP-IDKST-OK = NEJ                                    
063500                   MOVE MFS-ALFA-FAELT-FEL                                
063600                                       TO MOD-IDKST-ATTR (INDX)           
063700                 END-IF                                                   
063800               END-IF                                                     
063900             END-IF                                                       
064000           END-IF                                                         
064100         END-IF                                                           
064200                                                                          
064300         IF MID-IDARTNR-FROM (INDX) = ALL '+'                             
064400             INSPECT MID-IDARTNR-FROM (INDX)                              
064500                    REPLACING ALL '+' BY ZERO                             
064600         ELSE                                                             
064700             INSPECT MID-IDARTNR-FROM (INDX)                              
064800                    REPLACING ALL SPACE BY ZERO                           
064900             IF MID-IDARTNR-FROM (INDX) NOT NUMERIC                       
065000                 MOVE NEJ TO INDATA-SW                                    
065100                 MOVE MFS-ALFA-FAELT-FEL TO                               
065200                     MOD-IDARTNR-FROM-ATTR (INDX)                         
065300             END-IF                                                       
065400         END-IF                                                           
065500      END-IF                                                              
065600      ADD +1 TO INDX                                                      
065700     END-PERFORM                                                          
065800     .                                                                    
065900     EJECT                                                                
066000 GB-FORMELLA-KONTROLLER SECTION.                                          
066100     SKIP2                                                                
066200                                                                          
066300     PERFORM GBA-KONTROLL-AVSDAT                                          
066400                                                                          
066500     MOVE +1 TO INDX                                                      
066600     PERFORM UNTIL INDX > MAX-INDX                                        
066700     OR INDATA-FEL                                                        
066800     OR (MID-IDARTNR (INDX)      = ALL '+' AND                            
066900         MID-IDLEVNR (INDX)      = ALL '+' AND                            
067000         MID-IDKONTO (INDX)      = ALL '+' AND                            
067100         MID-IDAVINR (INDX)      = ALL '+' AND                            
067200         MID-KVAVIS  (INDX)       = ALL '+' AND                           
067300         MID-IDARTNR-FROM (INDX) = ALL '+' AND                            
067400         MID-IDANALYS (INDX)     = ALL '+' AND                            
067500         MID-IDKST (INDX)        = ALL '+')                               
067600                                                                          
067700         MOVE MID-IDARTNR (INDX) TO W-IDARTNR                             
067800                                    W-IDARTNR-WDK7                        
067900                                                                          
068000         IF NOT CDC-SE                                                    
068100             PERFORM IMS-GU-WDK711                                        
068200             IF SEGMENT-SAKNAS                                            
068300               MOVE NEJ TO INDATA-SW                                      
068400               MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                    
068500               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)         
069000             END-IF                                                       
069100             PERFORM IMS-GU-WDB601                                        
069200             MOVE DCS-IDLANDX2 TO W-IDLAND                                
069300             PERFORM IMS-GU-WDK712                                        
069400             IF SEGMENT-FINNS                                             
069500               MOVE LART-VKART     TO WS-VKART-K7                         
069600               MOVE LART-VLARTNTO  TO WS-VLARTNTO-K7                      
069700               MOVE LART-KDARTURS  TO WS-KDARTURS-K7                      
069800             ELSE                                                         
069900               MOVE ZERO           TO WS-VKART-K7                         
070000                                      WS-VLARTNTO-K7                      
070100               MOVE SPACE          TO WS-KDARTURS-K7                      
070200             END-IF                                                       
070300         END-IF                                                           
070400         IF INDATA-OK                                                     
070500             PERFORM IMS-GU-ARTC01                                        
070600             IF SEGMENT-SAKNAS                                            
070700               MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDARTNR-ATTR (INDX)        
070800               MOVE NEJ    TO INDATA-SW                                   
070900               MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                    
071000             ELSE                                                         
071100               IF ART-KDERS-UTG > 20                                      
071200                   MOVE MFS-ALFA-FAELT-FEL TO                             
071300                                           MOD-IDARTNR-ATTR (INDX)        
071400                   MOVE ERR-ART-UTG   TO MED-IDMFSFEL                     
071500                   MOVE NEJ    TO INDATA-SW                               
071600               ELSE                                                       
071700                 PERFORM IMS-GNP-ARTC11                                   
071800                 IF SEGMENT-SAKNAS                                        
071900                   MOVE MFS-ALFA-FAELT-FEL TO                             
072000                                           MOD-IDARTNR-ATTR (INDX)        
072100                   MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                
072200                   MOVE NEJ    TO INDATA-SW                               
072300                 ELSE                                                     
072400                   IF CLAG-PRARTSTD = +0                                  
072500                       MOVE MFS-ALFA-FAELT-FEL TO                         
072600                                     MOD-IDARTNR-ATTR (INDX)              
072700                       MOVE ERR-PRIS-SAKNAS TO MED-IDMFSFEL               
072800                     MOVE NEJ    TO INDATA-SW                             
072900                   ELSE                                                   
073000                     IF CDC-SE                                            
073100                       PERFORM GBC-KOLLA-VIKT-VOLYM-URSPR                 
073200                     ELSE                                                 
073300                       PERFORM GBD-KOLLA-VIKT-VOLYM-URSPR                 
073400                     END-IF                                               
073500                   END-IF                                                 
073600                 END-IF                                                   
073700               END-IF                                                     
073800             END-IF                                                       
073900             IF INDATA-OK AND (NOT NDC-NA)                                
074000                PERFORM GBB-R34-KONTROLLER                                
074100             END-IF                                                       
074200         END-IF                                                           
074300         ADD +1 TO INDX                                                   
074400     END-PERFORM                                                          
074500     .                                                                    
074600     EJECT                                                                
074700 GBA-KONTROLL-AVSDAT SECTION.                                             
074800                                                                          
074900     MOVE MID-TIAVIDAT        TO TEST-DATUM                               
075000                                 TMP1-YYMMDD                              
075100     MOVE DAGENS-DATUM        TO TMP2-YYMMDD                              
075200                                                                          
075300     PERFORM WY2000P1                                                     
075400     IF TMP1-YYMMDD  > TMP2-YYMMDD  OR                                    
075500        TMP1-MM      > 12           OR                                    
075600        TMP1-DD      > 31           OR                                    
075700        TMP1-YY      < TMP2-YY - 1                                        
075800        MOVE MFS-ALFA-FAELT-FEL TO MOD-TIAVIDAT-ATTR                      
075900        MOVE ERR-CORR-FIELDS TO MED-IDMFSFEL                              
076000        MOVE NEJ    TO INDATA-SW                                          
076100     END-IF                                                               
076200     .                                                                    
076300     EJECT                                                                
076400 GBB-R34-KONTROLLER SECTION.                                              
076500                                                                          
076600     MOVE MID-KDRT (INDX) TO TEST-KDRT                                    
076700     EVALUATE TEST-KDRT                                                   
076800       WHEN 0                                                             
076900          CONTINUE                                                        
077000       WHEN 6                                                             
077100          IF MID-IDLEVNR (INDX) = ('0    ' OR 'BP2TC'                     
077200                                OR '8265 ' OR '9999 ' OR '1013 ')         
077300            CONTINUE                                                      
077400          ELSE                                                            
077500            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDRT-ATTR (INDX)               
077600                                       MOD-IDLEVNR-ATTR (INDX)            
077700            MOVE NEJ    TO INDATA-SW                                      
077800            MOVE ERR-CORR-FIELDS TO MED-IDMFSFEL                          
077900          END-IF                                                          
078000     END-EVALUATE                                                         
078100     .                                                                    
078200     EJECT                                                                
078300 GBC-KOLLA-VIKT-VOLYM-URSPR SECTION.                                      
078400     IF CLAG-VKART = ZERO                                                 
078500         MOVE MFS-ALFA-FAELT-FEL TO                                       
078600              MOD-IDARTNR-ATTR (INDX)                                     
078700         MOVE '792' TO MED-IDMFSFEL                                       
078800         MOVE NEJ TO INDATA-SW                                            
078900     ELSE                                                                 
079000       IF CLAG-VLARTNTO = ZERO                                            
079100           MOVE MFS-ALFA-FAELT-FEL TO                                     
079200                MOD-IDARTNR-ATTR (INDX)                                   
079300           MOVE '793' TO MED-IDMFSFEL                                     
079400           MOVE NEJ TO INDATA-SW                                          
079500       ELSE                                                               
079600         IF CLAG-KDARTURS = SPACE                                         
079700             MOVE MFS-ALFA-FAELT-FEL TO                                   
079800                  MOD-IDARTNR-ATTR (INDX)                                 
079900             MOVE '794' TO MED-IDMFSFEL                                   
080000             MOVE NEJ TO INDATA-SW                                        
080100         ELSE                                                             
080200           IF CLAG-ADLAGOMR = ZERO AND MFS-UPDATE                         
080300               MOVE MFS-ALFA-FAELT-FEL TO                                 
080400                    MOD-IDARTNR-ATTR (INDX)                               
080500               MOVE '764' TO MED-IDMFSFEL                                 
080600               MOVE NEJ TO INDATA-SW                                      
080700           ELSE                                                           
080800             IF CLAG-ADLAGOMR-SVS = ZERO AND MFS-UPD-V                    
080900                 MOVE MFS-ALFA-FAELT-FEL TO                               
081000                      MOD-IDARTNR-ATTR (INDX)                             
081100                 MOVE '764' TO MED-IDMFSFEL                               
081200                 MOVE NEJ TO INDATA-SW                                    
081300             END-IF                                                       
081400           END-IF                                                         
081500         END-IF                                                           
081600       END-IF                                                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 GBD-KOLLA-VIKT-VOLYM-URSPR SECTION.                                      
082100     IF CLAG-VKART = ZERO AND                                             
082200        WS-VKART-K7 = ZERO                                                
082300         MOVE MFS-ALFA-FAELT-FEL TO                                       
082400              MOD-IDARTNR-ATTR (INDX)                                     
082500         MOVE '792' TO MED-IDMFSFEL                                       
082600         MOVE NEJ TO INDATA-SW                                            
082700     ELSE                                                                 
082800       IF CLAG-VLARTNTO = ZERO AND                                        
082900          WS-VLARTNTO-K7 = ZERO                                           
083000           MOVE MFS-ALFA-FAELT-FEL TO                                     
083100                MOD-IDARTNR-ATTR (INDX)                                   
083200           MOVE '793' TO MED-IDMFSFEL                                     
083300           MOVE NEJ TO INDATA-SW                                          
083400       ELSE                                                               
083500         IF CLAG-KDARTURS = SPACE AND                                     
083600            WS-KDARTURS-K7 = SPACE                                        
083700             MOVE MFS-ALFA-FAELT-FEL TO                                   
083800                  MOD-IDARTNR-ATTR (INDX)                                 
083900             MOVE '794' TO MED-IDMFSFEL                                   
084000             MOVE NEJ TO INDATA-SW                                        
084100         ELSE                                                             
084200           IF SLAG-ADLAGOMR = ZERO                                        
084300               MOVE MFS-ALFA-FAELT-FEL TO                                 
084400                    MOD-IDARTNR-ATTR (INDX)                               
084500               MOVE '764' TO MED-IDMFSFEL                                 
084600               MOVE NEJ TO INDATA-SW                                      
084700           END-IF                                                         
084800         END-IF                                                           
084900       END-IF                                                             
085000     END-IF                                                               
085100     .                                                                    
085200     EJECT                                                                
085300 H-SKAPA-SKICKA-TRANS SECTION.                                            
085400                                                                          
085500     MOVE ALL '+' TO MOD611C-MID-W6I11C01                                 
085600     MOVE +1 TO INDX                                                      
085700     PERFORM UNTIL INDX > MAX-INDX                                        
085800     OR (MID-IDARTNR (INDX)      = ALL '+' AND                            
085900         MID-IDLEVNR (INDX)      = ALL '+' AND                            
086000         MID-IDKONTO (INDX)      = ALL  '+' AND                           
086100         MID-IDAVINR (INDX)      = ALL '+' AND                            
086200         MID-KVAVIS (INDX)        = ALL '+' AND                           
086300         MID-IDARTNR-FROM (INDX) = ALL '+' AND                            
086400         MID-IDANALYS (INDX)     = ALL '+' AND                            
086500         MID-IDKST (INDX)        = ALL '+')                               
086600                                                                          
086700         ADD +1         TO 611C-IX                                        
086800         MOVE W-IDDC    TO MOD611C-MID-IDDC                               
086900                                                                          
087000         MOVE MID-IDLEVNR (INDX) TO                                       
087100                          MOD611C-MID-IDLEVNR (611C-IX)                   
087200         MOVE MID-IDAVINR (INDX) TO                                       
087300                          MOD611C-MID-IDAVINR (611C-IX)                   
087400         MOVE MID-TIAVIDAT       TO                                       
087500                          MOD611C-MID-TIAVIDAT (611C-IX)                  
087600         MOVE MID-IDARTNR (INDX) TO                                       
087700                          MOD611C-MID-IDARTNR (611C-IX)                   
087800         MOVE WS-KVAVIS (INDX)   TO                                       
087900                          MOD611C-MID-KVAVIS (611C-IX)                    
088000         MOVE MID-KDRT (INDX)    TO MOD611C-MID-KDRT (611C-IX)            
088100         MOVE WS-IDKONTO-NUM     TO MOD611C-MID-IDKONTO (611C-IX)         
088200         MOVE WS-IDANALYS        TO MOD611C-MID-IDANALYS(611C-IX)         
088300         MOVE WS-IDKST           TO MOD611C-MID-IDKST(611C-IX)            
088400         IF MID-IDARTNR-FROM (INDX) = ZERO                                
088500*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
088600*W005INIT. DETTA SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC               
088700*                                                                         
088800             MOVE ALL '+'         TO MSGI-WMSGINIT                        
088900             MOVE '001'           TO MSGI-KDCALL                          
089000             MOVE W-IDDC          TO WS-IDDC-LOCAL-DATE                   
089100                                                                          
089200             MOVE WS-IDDC-LOCAL   TO MSGI-IDUSER                          
089300                                                                          
089400             CALL W005INIT  USING  MSGI-WMSGINIT USEA-PCB                 
089500             MOVE MSGI-TILOKDAT TO DAT-I-TIDATUM                          
089600             MOVE 'AAMMDD'      TO DAT-KDDATFORM                          
089700             CALL WDATKONV USING   DAT-KDDATFORM                          
089800                                   DAT-I-TIDATUM                          
089900                                   DAT-O-TIDATUM                          
090000                                   DAT-KDSVAR                             
090100             IF DAT-KDSVAR-OK                                             
090200                 MOVE DAT-TIVV  TO WS-VV                                  
090300                 MOVE DAT-TID   TO WS-D                                   
090400             ELSE                                                         
090500                 MOVE ZERO TO WS-VV                                       
090600                              WS-D                                        
090700             END-IF                                                       
090800                 MOVE WS-LOPNR-R34 TO                                     
090900                               MOD611C-MID-IDARTNR-FROM (INDX)            
091000         ELSE                                                             
091100             MOVE MID-IDARTNR-FROM (INDX) TO                              
091200                          MOD611C-MID-IDARTNR-FROM (611C-IX)              
091300         END-IF                                                           
091400         IF 611C-IX = 611C-MAX                                            
091500            PERFORM S04-STARTA-R34-TRANS                                  
091600            MOVE +0 TO 611C-IX                                            
091700         END-IF                                                           
091800        ADD +1 TO INDX                                                    
091900     END-PERFORM                                                          
092000                                                                          
092100                                                                          
092200     IF 611C-IX > +0                                                      
092300     AND 611C-IX < 611C-MAX                                               
092400         PERFORM S04-STARTA-R34-TRANS                                     
092500     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
092800 S01-VISA-DEFAULT SECTION.                                                
092900     SKIP2                                                                
093000     MOVE +1 TO INDX                                                      
093100     PERFORM UNTIL INDX > MAX-INDX                                        
093200         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDRT-ATTR (INDX)               
093300         IF NDC-NA                                                        
093400            CONTINUE                                                      
093500         ELSE                                                             
093600            MOVE 06  TO MOD-KDRT (INDX)                                   
093700         END-IF                                                           
093800                                                                          
093900         ADD +1 TO INDX                                                   
094000     END-PERFORM                                                          
094100     MOVE DAGENS-DATUM TO MOD-TIAVIDAT                                    
094200     .                                                                    
094300     EJECT                                                                
094400 S04-STARTA-R34-TRANS  SECTION.                                           
094500                                                                          
094600                                                                          
094700     MOVE IDPGM              TO MOD611C-MID-IDPGM                         
094800     MOVE 611C-IX            TO MOD611C-MID-KVPOST                        
094900     IF MFS-UPD-V                                                         
095000       MOVE 'J'              TO MOD611C-MID-FLSVS                         
095100     ELSE                                                                 
095200       MOVE 'N'              TO MOD611C-MID-FLSVS                         
095300     END-IF                                                               
095400     COMPUTE 611C-LL = LENGTH OF 611C-MSG-IO-AREA                         
095500                                                                          
095600     PERFORM IMS-ISRT-MSG-ALT-611C                                        
095700                                                                          
095800     MOVE +0                   TO 611C-IX                                 
095900     .                                                                    
096000     EJECT                                                                
096100 MFS-RENSA-FAELT-UT SECTION.                                              
096200                                                                          
096300     MOVE MFS-RENSA-FAELT TO MOD-TIAVIDAT                                 
096400*    --- ALLA UTDATA-FÄLT                                                 
096500     MOVE +1 TO INDX                                                      
096600         PERFORM UNTIL INDX > MAX-INDX                                    
096700         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR (INDX)                       
096800                                 MOD-KDRT (INDX)                          
096900                                 MOD-IDAVINR (INDX)                       
097000                                 MOD-IDARTNR (INDX)                       
097100                                 MOD-KVAVIS (INDX)                        
097200                                 MOD-IDKONTO (INDX)                       
097300                                 MOD-IDARTNR-FROM (INDX)                  
097400                                 MOD-IDANALYS (INDX)                      
097500                                 MOD-IDKST    (INDX)                      
097600         ADD +1 TO INDX                                                   
097700     END-PERFORM                                                          
097800     .                                                                    
097900     SKIP3                                                                
098000 MFS-RENSA-FAELT-IN SECTION.                                              
098100                                                                          
098200     MOVE MFS-RENSA-FAELT TO MOD-TIAVIDAT                                 
098300     MOVE +1 TO INDX                                                      
098400     PERFORM UNTIL INDX > MAX-INDX                                        
098500         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR (INDX)                       
098600                                 MOD-KDRT (INDX)                          
098700                                 MOD-IDAVINR (INDX)                       
098800                                 MOD-IDARTNR (INDX)                       
098900                                 MOD-KVAVIS (INDX)                        
099000                                 MOD-IDKONTO (INDX)                       
099100                                 MOD-IDARTNR-FROM (INDX)                  
099200                                 MOD-IDANALYS (INDX)                      
099300                                 MOD-IDKST    (INDX)                      
099400         ADD +1 TO INDX                                                   
099500     END-PERFORM                                                          
099600     .                                                                    
099700     EJECT                                                                
099800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
099900                                                                          
100000     MOVE MFS-ROER-EJ-FAELT TO MOD-TIAVIDAT                               
100100     MOVE +1 TO INDX                                                      
100200     PERFORM UNTIL INDX > MAX-INDX                                        
100300         MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR (INDX)                     
100400                                   MOD-KDRT (INDX)                        
100500                                   MOD-IDAVINR (INDX)                     
100600                                   MOD-IDARTNR (INDX)                     
100700                                   MOD-KVAVIS (INDX)                      
100800                                   MOD-IDKONTO (INDX)                     
100900                                   MOD-IDARTNR-FROM (INDX)                
101000                                   MOD-IDANALYS (INDX)                    
101100                                   MOD-IDKST    (INDX)                    
101200         ADD +1 TO INDX                                                   
101300     END-PERFORM                                                          
101400     .                                                                    
101500     EJECT                                                                
101600* -COPY WY2000P1                                                          
101700                                                                          
101800* --- IMS SEKTIONER ---                                                   
101900     SKIP3                                                                
102000 IMS-GET-MSG SECTION.                                                     
102100                                                                          
102200     MOVE '  QC' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
102400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     SKIP3                                                                
102800 IMS-INSERT-MSG SECTION.                                                  
102900                                                                          
103000     IF WS-IDLAND-SPR NOT = 'GB'                                          
103100       MOVE '0' TO MFS-KDHUVOMR                                           
103200     END-IF                                                               
103300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
103400     MOVE SPACE TO GODK-STATUSKODER                                       
103500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     .                                                                    
103900     EJECT                                                                
104000 IMS-ISRT-MSG-ALT-611C SECTION.                                           
104100     SKIP2                                                                
104200     MOVE SPACE TO GODK-STATUSKODER                                       
104300     CALL CBLTDLI   USING  ISRT                                           
104400                           611C-PCB                                       
104500                           611C-MSG-IO-AREA                               
104600     MOVE 611C-STATUS-CODE TO STATUS-WS                                   
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     .                                                                    
104900     EJECT                                                                
105000 IMS-GU-ARTC01   SECTION.                                                 
105100                                                                          
105200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
105300          DELIMITED BY SIZE INTO SSA1                                     
105400     MOVE '  GE' TO GODK-STATUSKODER                                      
105500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
105600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
105700     PERFORM IMS-STATUSKONTROLL                                           
105800     .                                                                    
105900     EJECT                                                                
106000 IMS-GNP-ARTC11   SECTION.                                                
106100                                                                          
106200     MOVE 'WLARTC11 ' TO SSA1                                             
106300     MOVE '  GE' TO GODK-STATUSKODER                                      
106400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
106500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
106600     PERFORM IMS-STATUSKONTROLL                                           
106700     .                                                                    
106800     EJECT                                                                
106900 IMS-GU-WDK711   SECTION.                                                 
107000                                                                          
107100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
107200          DELIMITED BY SIZE INTO SSA1                                     
107300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
107400          DELIMITED BY SIZE INTO SSA2                                     
107500     MOVE '  GE' TO GODK-STATUSKODER                                      
107600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA2 SSA1 SSA2                
107700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     .                                                                    
108000     EJECT                                                                
108100 IMS-GU-WDK712   SECTION.                                                 
108200                                                                          
108300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
108400          DELIMITED BY SIZE INTO SSA1                                     
108500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
108600          DELIMITED BY SIZE INTO SSA2                                     
108700     MOVE '  GE' TO GODK-STATUSKODER                                      
108800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
108900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     .                                                                    
109200     EJECT                                                                
109300 IMS-GU-WDB601   SECTION.                                                 
109400                                                                          
109500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
109600          DELIMITED BY SIZE INTO SSA1                                     
109700     MOVE '    ' TO GODK-STATUSKODER                                      
109800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
109900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
110000     PERFORM IMS-STATUSKONTROLL                                           
110100     .                                                                    
110200     EJECT                                                                
110210 IMS-GU-WDB601-FTG SECTION.                                               
110220     STRING 'WDB601  (IDFTG    =' WS-IDFTG-B6 ')'                         
110230            DELIMITED BY SIZE INTO SSA1                                   
110240     MOVE '  GE'                 TO GODK-STATUSKODER                      
110250     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
110260     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
110270     PERFORM IMS-STATUSKONTROLL                                           
110280     .                                                                    
110290     SKIP3                                                                
110291                                                                          
110300 IMS-STATUSKONTROLL SECTION.                                              
110400                                                                          
110500     SET STATUS-IX TO 1                                                   
110600     SEARCH GODK-STATUS                                                   
110700       AT END                                                             
110800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
110900         DELIMITED BY SIZE INTO FELTEXT                                   
111000         CALL FELLOG                                                      
111100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
111200         CONTINUE                                                         
111300     END-SEARCH                                                           
111400     .                                                                    
