000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2012800.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   94/11/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR TOTALBEHOV PER ARTIKEL. VISAR BEHOV INNEVARANDE VEC        
001000*        KA OCH FÖLJANDE 13 VECKOR.                                       
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W2T128                                              
001600*        MID:         W2I12801                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W2O12801                                            
002000*                                                                         
002100*   ÄNDRINGAR:                                                            
002200*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002300*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002400*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002500*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300*    -- CHECKED BY WY2000                                                 
003400     SKIP3                                                                
003500*    -COPY WY2000W3                                                       
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(08)   VALUE 'W2012800'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004500 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
004600                                                                          
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  INDX1                       PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  INDX-T                      PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
005100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005600                                                                          
005700                                                                          
005800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005900     88  NYCKLAR-OK                          VALUE 'J'.                   
006000     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100                                                                          
006200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006300     88  EGEN-MID                            VALUE '2128'.                
006400     88  GODK-MID                            VALUE '2121' '2122'          
006500                                                   '2123' '2124'          
006600                                                   '2125' '2126'          
006700                                                   '2127' '2128'.         
006800     88  HELP-MID                            VALUE '0551'.                
006900     EJECT                                                                
007000*    --- ARBETSFÄLT                                                       
007100 01  ARBETSFAELT.                                                         
007200     03  ENDAST-SEPARATBEHOV     PIC  X(2)   VALUE '01'.                  
007300     03  ENDAST-SATSBEHOV        PIC  X(2)   VALUE '02'.                  
007400     03  ENDAST-LEVBEHOV         PIC  X(2)   VALUE '04'.                  
007500     03  ENDAST-SDCBEHOV         PIC  X(2)   VALUE '10'.                  
007600     03  ENDAST-NDCBEHOV         PIC  X(2)   VALUE '15'.                  
007700     03  SEP-SATS-TPO-LEV-SDC-NDC                                         
007800                                 PIC  X(2)   VALUE '19'.                  
007900     03  W-TIAAVV-NUM            PIC   9(4)  VALUE ZERO.                  
008000     03  W-TIAAVV                PIC  S9(5)  VALUE ZERO  COMP-3.          
008100     03  W-TIME                  PIC   9(8)  VALUE ZERO.                  
008200     03  W-VECKO-SEP-BEHOV  PIC  S9(7)V9(2)  VALUE ZERO  COMP-3.          
008300     03  W-DAG-SEP-BEHOV    PIC  S9(7)V9(2)  VALUE ZERO  COMP-3.          
008400     03  W-ANTAL-VECKOR          PIC   9(3)   VALUE ZERO COMP-3.          
008500     03  W-KVDAGAR-KVAR          PIC   9(3)   VALUE ZERO COMP-3.          
008600     03  W-TIFINLV-AAVV          PIC S9(5)               COMP-3.          
008700     03  W-FAKTOR                PIC S9(1)V9(3)          COMP-3.          
008800     03  WS-IDLEVNR-8            PIC X(8)  VALUE SPACE.                   
008900     03  SPAR-KVPB-TOT           PIC S9(8)V9(2) VALUE ZERO COMP-3.        
009000                                                                          
009100     03  WS-CURRENT-DATE.                                                 
009200         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
009300         05  FILLER              PIC 9(4)   VALUE ZERO.                   
009400         05  FILLER              PIC 9(6)   VALUE ZERO.                   
009500                                                                          
009600     03  FILLER REDEFINES WS-CURRENT-DATE.                                
009700*-----   INKLUSIVE SEKEL                                                  
009800         05  WS-DAGENS-DATUM     PIC 9(8).                                
009900         05  WS-DAGENS-TID.                                               
010000             07 WS-DAGENS-TIMME  PIC 9(2).                                
010100             07 WS-DAGENS-MINUT  PIC 9(2).                                
010200             07 WS-DAGENS-SEKUND PIC 9(2).                                
010300                                                                          
010400     03  WS-KVPB-TREND           PIC S9(6)V9.                             
010500     03  MAX-KVVECKOR-TREND      PIC S9(3)  VALUE ZERO.                   
010600     03  WS-KVPB-TREND-RAD1      PIC S9(6)V9(1) VALUE ZERO COMP-3.        
010700                                                                          
010800 01      W-KVPB-TOT-TAB.                                                  
010900     03  W-KVPB-TOT         PIC  S9(8)V9(2)   COMP-3 OCCURS 14.           
011000                                                                          
011100*      --- VALID IDDC CODES                                               
011200*                                                                         
011300*01    -COPY WWDCKONS                                                     
011400       EJECT                                                              
011500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011600 01  GENERELLA-SUBPROGRAM.                                                
011700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
012200     03  W22222                  PIC X(8)    VALUE 'W22222'.              
012300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012400     EJECT                                                                
012500*    *************************************                                
012600*    **  LINK-AREA                      **                                
012700*    **  BEHOVSTABELL                   **                                
012800*    *************************************                                
012900*01  AREA  -COPY W222L222   -PRE LINK-.                                   
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013200*01 -COPY WMEDAREA                                                        
013300     EJECT                                                                
013400 01  MESSAGE-CODES.                                                       
013500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013600     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
013700     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
013800     03  ERR-NO-LINE-SELECTED    PIC X(3)    VALUE '362'.                 
013900     EJECT                                                                
014000*01  -COPY WDATAREA                                                       
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014300*01  -COPY WMSGINIT                                                       
014400     EJECT                                                                
014500 01  FILLER                PIC X(16)   VALUE 'MID TILL 2158 '.            
014600 01  ALT-MSG-AREA.                                                        
014700     03  ALT-LL            PIC S9(4)   VALUE +0 COMP SYNC.                
014800     03  FILLER            PIC X(2)    VALUE LOW-VALUE.                   
014900     03  ALT-KDTRANS       PIC X(8)    VALUE 'W2T158  '.                  
015000     03  ALT-IDTRANS       PIC X(4)    VALUE '2128'.                      
015100     03  ALT-KDMFSFOR      PIC X(1)    VALUE SPACE.                       
015200     03  MID -COPY W2I15801    -PRE ALT-                                  
015300     EJECT                                                                
015400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015700     SKIP3                                                                
015800*01  MID -COPY W2I12801                                                   
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016100     SKIP3                                                                
016200*01  -COPY WMSGAREA                                                       
016300     EJECT                                                                
016400     03  MOD REDEFINES MSG-AREA.                                          
016500*      05  -COPY W2O12801                                                 
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016800     SKIP3                                                                
016900*01  -COPY WMFSAREA                                                       
017000     EJECT                                                                
017100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017200*                                                                         
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500     SKIP3                                                                
017600 01  NYCKLAR-TILL-DLI.                                                    
017700     03  W-IDARTNR-X.                                                     
017800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017900     SKIP2                                                                
018000*    --- STATUS-KOD FRÅN IMS                                              
018100 01  STATUS-WS                   PIC XX.                                  
018200     88  SEGMENT-FINNS                       VALUE '  '.                  
018300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018500     SKIP2                                                                
018600 01  GODK-STATUSKODER.                                                    
018700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018800     SKIP3                                                                
018900 01  SSA1                        PIC X(64).                               
019000 01  SSA2                        PIC X(64).                               
019100     EJECT                                                                
019200*    --- IMS FUNKTIONSKODER                                               
019300*01  -COPY W0003                                                          
019400     EJECT                                                                
019500*    ---  DLI INPUT-OUTPUT AREA                                           
019600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
019700     SKIP3                                                                
019800 01  DLI-IO-AREA1.                                                        
019900     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
020000     SKIP3                                                                
020100     03  WLARTC01 REDEFINES IO-AREA1.                                     
020200*        05  -COPY WDK601                                                 
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
020500     SKIP3                                                                
020600 01  DLI-IO-AREA2.                                                        
020700     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
020800     SKIP3                                                                
020900     03  WLARTC11 REDEFINES IO-AREA2.                                     
021000*        05  -COPY WDK611                                                 
021100     EJECT                                                                
021200 LINKAGE SECTION.                                                         
021300                                                                          
021400*01  -COPY W0009   -PRE MSG-                                              
021500     EJECT                                                                
021600*    PSB - JUMP TO 2158                                                   
021700*01  -COPY W0009  -PRE ALT-                                               
021800                                                                          
021900*01  -COPY W0008  -PRE USEA-                                              
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008  -PRE ARTC1-                                             
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500 01  W222-WDK6-PCB               PIC X.                                   
022600 01  W222-WDK7-PCB               PIC X.                                   
022700 01  W222-ARTM-PCB               PIC X.                                   
022800 01  W222-2501-PCB               PIC X.                                   
022900 01  W222-WDB6R-PCB              PIC X.                                   
023000 01  W222-WDK7R-PCB              PIC X.                                   
023100 01  W222-WDB6-PCB               PIC X.                                   
023200 01  W222-WDD7-PCB               PIC X.                                   
023300 01  W222-WDK7E-PCB              PIC X.                                   
023400 01  UTIL-WDK6-PCB               PIC X.                                   
023500 01  UTIL-WDK7-PCB               PIC X.                                   
023600 01  UTIL-WDB6-PCB               PIC X.                                   
023700 01  UTUP-WDK7-PCB               PIC X.                                   
023800 01  UTUP-WDB6-PCB               PIC X.                                   
023900 01  UTUP-UTIL-WDK6-PCB          PIC X.                                   
024000 01  UTUP-UTIL-WDK7-PCB          PIC X.                                   
024100 01  UTUP-UTIL-WDB6-PCB          PIC X.                                   
024200     EJECT                                                                
024300 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB        USEA-PCB                
024400                                   ARTC1-PCB      W222-WDK6-PCB           
024500                                   W222-WDK7-PCB  W222-ARTM-PCB           
024600                                   W222-2501-PCB  W222-WDB6R-PCB          
024700                                   W222-WDK7R-PCB W222-WDB6-PCB           
024800                                   W222-WDD7-PCB  W222-WDK7E-PCB          
024900                                   UTIL-WDK6-PCB                          
025000                                   UTIL-WDK7-PCB                          
025100                                   UTIL-WDB6-PCB                          
025200                                   UTUP-WDK7-PCB                          
025300                                   UTUP-WDB6-PCB                          
025400                                   UTUP-UTIL-WDK6-PCB                     
025500                                   UTUP-UTIL-WDK7-PCB                     
025600                                   UTUP-UTIL-WDB6-PCB                     
025700                                   .                                      
025800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB        USEA-PCB                
025900                                   ARTC1-PCB      W222-WDK6-PCB           
026000                                   W222-WDK7-PCB  W222-ARTM-PCB           
026100                                   W222-2501-PCB  W222-WDB6R-PCB          
026200                                   W222-WDK7R-PCB W222-WDB6-PCB           
026300                                   W222-WDD7-PCB  W222-WDK7E-PCB          
026400                                   UTIL-WDK6-PCB                          
026500                                   UTIL-WDK7-PCB                          
026600                                   UTIL-WDB6-PCB                          
026700                                   UTUP-WDK7-PCB                          
026800                                   UTUP-WDB6-PCB                          
026900                                   UTUP-UTIL-WDK6-PCB                     
027000                                   UTUP-UTIL-WDK7-PCB                     
027100                                   UTUP-UTIL-WDB6-PCB                     
027200                                   .                                      
027300                                                                          
027400     PERFORM IMS-GET-MSG                                                  
027500     IF SEGMENT-FINNS                                                     
027600       PERFORM A-INIT                                                     
027700       PERFORM B-KOLLA-NYCKLAR                                            
027800       IF NYCKLAR-OK                                                      
027900         PERFORM F-LAES-VISA-INFO                                         
028000       END-IF                                                             
028100       IF MFS-SPLIT                                                       
028200          PERFORM I-JUMP-TO-SCREEN-2158                                   
028300       ELSE                                                               
028400          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
028500          PERFORM IMS-INSERT-MSG                                          
028600       END-IF                                                             
028700     END-IF                                                               
028800                                                                          
028900     MOVE ZERO TO RETURN-CODE                                             
029000     GOBACK                                                               
029100     .                                                                    
029200     EJECT                                                                
029300 A-INIT SECTION.                                                          
029400     MOVE 'A-INIT SECTION '  TO CURRENT-SECTION                           
029500                                                                          
029600     IF MSG-DUBBLA-TRANSKODER                                             
029700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I12801                 
029800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
029900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
030000     ELSE                                                                 
030100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I12801                  
030200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
030300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030400     END-IF                                                               
030500                                                                          
030600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
030700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
030800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030900                                                                          
031000     MOVE LOW-VALUE       TO MSG-AREA                                     
031100     MOVE 'W2O128N1'      TO MFS-IDMOD                                    
031200     MOVE '2128'          TO MOD-IDTRANS                                  
031300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031400                                                                          
031500     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W2O12801 + 4                  
031600                                                                          
031700     IF EGEN-MID OR HELP-MID                                              
031800       CONTINUE                                                           
031900     ELSE                                                                 
032000       MOVE SPACE TO MFS-KDTRTYP                                          
032100       MOVE '7' TO MFS-IDPFK                                              
032200     END-IF                                                               
032300                                                                          
032400     MOVE +2                 TO SPRAK-IX                                  
032500     MOVE 'GB '              TO MED-IDSKYLT                               
032600                                                                          
032700     MOVE FUNCTION CURRENT-DATE                                           
032800                             TO WS-CURRENT-DATE                           
032900                                                                          
033000     MOVE 'IDAG'             TO DAT-KDDATFORM                             
033100                                                                          
033200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
033300                     DAT-O-TIDATUM DAT-KDSVAR                             
033400                                                                          
033500     IF DAT-KDSVAR-OK                                                     
033600        MOVE DAT-TIAAVV-GRP      TO W-TIAAVV-NUM                          
033700        MOVE W-TIAAVV-NUM        TO LINK-TIAAVV-AKTUELL                   
033800                                    LINK-TIBEHOV-START                    
033900        MOVE 1                   TO W-ANTAL-VECKOR                        
034000        CALL W009VADD USING    LINK-TIBEHOV-START  W-ANTAL-VECKOR         
034100        MOVE DAT-TID             TO LINK-TID-AKTUELL                      
034200        MOVE SPACE               TO LINK-IDDC                             
034300     ELSE                                                                 
034400        CALL FELLOG                                                       
034500     END-IF                                                               
034600                                                                          
034700     MOVE +1                        TO INDX                               
034800     PERFORM UNTIL INDX             >  MAX-INDX                           
034900        MOVE ZERO                   TO W-KVPB-TOT(INDX)                   
035000        ADD +1                      TO INDX                               
035100     END-PERFORM                                                          
035200     .                                                                    
035300     EJECT                                                                
035400 B-KOLLA-NYCKLAR SECTION.                                                 
035500     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
035600                                                                          
035700     MOVE JA TO NYCKLAR-SW                                                
035800                                                                          
035900*    -- KONTROLL AV IDARTNR                                               
036000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
036100                                                                          
036200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
036300     MOVE '001'             TO MSGI-KDCALL                                
036400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
036600     MOVE '2128'            TO MSGI-IDTRANS                               
036700     IF MFS-IDTRANS = '2128'                                              
036800     OR (MID-IDARTNR-IN NUMERIC                                           
036900     AND MID-IDARTNR-IN > ZERO)                                           
037000         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
037100     END-IF                                                               
037200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037300     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
037400     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
037500                                                                          
037600     IF MID-IDARTNR-IN       = ALL '+'                                    
037700       CONTINUE                                                           
037800     ELSE                                                                 
037900       MOVE '7'              TO MFS-IDPFK                                 
038000       MOVE SPACE            TO MFS-KDTRTYP                               
038100     END-IF                                                               
038200                                                                          
038300     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
038400       MOVE WS-IDARTNR       TO W-IDARTNR                                 
038500                                LINK-IDARTNR                              
038600     ELSE                                                                 
038700       MOVE NEJ              TO NYCKLAR-SW                                
038800     END-IF                                                               
038900                                                                          
039000     IF GODK-MID OR NYCKLAR-OK                                            
039100       MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                            
039200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
039300     ELSE                                                                 
039400       MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR-UT                            
039500     END-IF                                                               
039600                                                                          
039700     IF NYCKLAR-FEL                                                       
039800       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
039900       CALL WMEDKONV USING MED-WMEDAREA                                   
040000       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
040100       PERFORM MFS-RENSA-FAELT-UT                                         
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 F-LAES-VISA-INFO SECTION.                                                
040600     MOVE 'F-LAES-VISA-INFO '  TO CURRENT-SECTION                         
040700                                                                          
040800     PERFORM FA-LAES-GRUNDDATA                                            
040900                                                                          
041000*    --- TESTA DATABASCALL FRÅN FA- SEKTIONEN                             
041100     IF SEGMENT-SAKNAS                                                    
041200        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
041300        CALL WMEDKONV USING MED-WMEDAREA                                  
041400        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
041500        PERFORM MFS-RENSA-FAELT-UT                                        
041600     ELSE                                                                 
041700*       --- KOLLA OM BEHÖRIG ANVÄNDARE                                    
041800        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
041900        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
042000*       --- BEHÖRIG !                                                     
042100           PERFORM FB-VECKONR                                             
042200           PERFORM FC-SEPARAT-BEHOV                                       
042300           PERFORM FD-SATS-BEHOV                                          
042400           PERFORM FE-TPO-BEHOV                                           
042500           PERFORM FF-SDC-BEHOV                                           
042600           PERFORM FG-NDC-BEHOV                                           
042700           PERFORM FJ-TREND                                               
042800           PERFORM FH-TOT-BEHOV                                           
042900           PERFORM FI-PBPLAN                                              
043000        ELSE                                                              
043100*          --- EJ BEHÖRIG ANVÄNDARE                                       
043200           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
043300           CALL WMEDKONV USING MED-WMEDAREA                               
043400           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
043500           PERFORM MFS-RENSA-FAELT-UT                                     
043600        END-IF                                                            
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 FA-LAES-GRUNDDATA SECTION.                                               
044100     MOVE 'FA-LAES-GRUNDDATA ' TO CURRENT-SECTION                         
044200                                                                          
044300     PERFORM IMS-GU-ARTC01                                                
044400                                                                          
044500     IF SEGMENT-FINNS                                                     
044600        MOVE ART-IDLEVNR  TO WS-IDLEVNR-8                                 
044700        PERFORM IMS-GNP-ARTC11                                            
044800     END-IF                                                               
044900     .                                                                    
045000     EJECT                                                                
045100 FB-VECKONR        SECTION.                                               
045200     MOVE 'FB-VECKONR '  TO CURRENT-SECTION                               
045300                                                                          
045400     MOVE 1                      TO W-ANTAL-VECKOR                        
045500     MOVE +1                   TO INDX                                    
045600     MOVE W-TIAAVV-NUM         TO MOD-TIAAVV(INDX)                        
045700     ADD +1                    TO INDX                                    
045800     MOVE LINK-TIBEHOV-START   TO MOD-TIAAVV(INDX)                        
045900                                  W-TIAAVV                                
046000                                                                          
046100     ADD +1                    TO INDX                                    
046200     PERFORM UNTIL INDX        >  MAX-INDX                                
046300                                                                          
046400        CALL W009VADD USING W-TIAAVV W-ANTAL-VECKOR                       
046500        MOVE W-TIAAVV          TO MOD-TIAAVV(INDX)                        
046600        ADD +1                 TO INDX                                    
046700     END-PERFORM                                                          
046800                                                                          
046900     .                                                                    
047000     EJECT                                                                
047100 FC-SEPARAT-BEHOV  SECTION.                                               
047200     MOVE 'FC-SEPARAT-BEHOV '  TO CURRENT-SECTION                         
047300                                                                          
047400     PERFORM FCA-SEP-BEHOV-INNEV-VECKA                                    
047500     PERFORM FCB-SEP-BEHOV-OVRIGA-VECKOR                                  
047600                                                                          
047700     .                                                                    
047800     EJECT                                                                
047900 FCA-SEP-BEHOV-INNEV-VECKA  SECTION.                                      
048000     MOVE 'FCA-SEP-BEHOV-INNEV-VECKA '  TO CURRENT-SECTION                
048100                                                                          
048200     MOVE +1                   TO INDX                                    
048300     ACCEPT W-TIME             FROM TIME                                  
048400     COMPUTE W-VECKO-SEP-BEHOV ROUNDED  =  CLAG-KVPB-SEP / 4.33           
048500     COMPUTE W-DAG-SEP-BEHOV   ROUNDED  =  W-VECKO-SEP-BEHOV / 5          
048600     DIVIDE ART-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                       
048700     MOVE W-TIAAVV-NUM         TO TMP1-YYWW                               
048800     MOVE W-TIFINLV-AAVV       TO TMP2-YYWW                               
048900     PERFORM WY2000P3                                                     
049000                                                                          
049100     IF  DAT-TID = 6 OR                                                   
049200         DAT-TID = 7 OR                                                   
049300        (DAT-TID = 5 AND W-TIME(1:4) > 1700)                              
049400     OR  TMP1-YYWW < TMP2-YYWW                                            
049500         MOVE ZERO              TO MOD-KVPB-SEP(INDX)                     
049600     ELSE                                                                 
049700         COMPUTE W-KVDAGAR-KVAR =  5 - DAT-TID                            
049800         IF W-TIME(1:4)         <= 1700                                   
049900            ADD +1              TO W-KVDAGAR-KVAR                         
050000         END-IF                                                           
050100                                                                          
050200         MOVE +1                TO W-FAKTOR                               
050300         SUBTRACT CLAG-REDIRLEV FROM W-FAKTOR                             
050400                                                                          
050500         COMPUTE MOD-KVPB-SEP(INDX) = W-DAG-SEP-BEHOV *                   
050600                                      W-KVDAGAR-KVAR  *                   
050700                                      W-FAKTOR                            
050800                                                                          
050900         COMPUTE W-KVPB-TOT(INDX)   = (W-DAG-SEP-BEHOV *                  
051000                                       W-KVDAGAR-KVAR  *                  
051100                                       W-FAKTOR)         +                
051200                                       W-KVPB-TOT(INDX)                   
051300     END-IF                                                               
051400                                                                          
051500     .                                                                    
051600     EJECT                                                                
051700 FCB-SEP-BEHOV-OVRIGA-VECKOR SECTION.                                     
051800     MOVE 'FCB-SEP-BEHOV-OVRIGA-VECKOR' TO CURRENT-SECTION                
051900                                                                          
052000     IF ART-KDERS-UTG = +0                                                
052100        MOVE 13                        TO LINK-KVVECKOR-BEHOV             
052200        MOVE ENDAST-SEPARATBEHOV       TO LINK-KDBEHOV                    
052300        MOVE NEJ                       TO LINK-FLINKLDIRLEV               
052400        CALL W22222 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB           
052500                                    W222-ARTM-PCB W222-2501-PCB           
052600                                    W222-WDB6R-PCB                        
052700                                    W222-WDK7R-PCB                        
052800                                    W222-WDB6-PCB W222-WDD7-PCB           
052900                                    W222-WDK7E-PCB                        
053000                                    UTIL-WDK6-PCB                         
053100                                    UTIL-WDK7-PCB                         
053200                                    UTIL-WDB6-PCB                         
053300                                    UTUP-WDK7-PCB                         
053400                                    UTUP-WDB6-PCB                         
053500                                    UTUP-UTIL-WDK6-PCB                    
053600                                    UTUP-UTIL-WDK7-PCB                    
053700                                    UTUP-UTIL-WDB6-PCB                    
053800        IF LINK-ANROP-FEL                                                 
053900           PERFORM S01-NOLLA-W22222                                       
054000        END-IF                                                            
054100     ELSE                                                                 
054200        PERFORM S01-NOLLA-W22222                                          
054300     END-IF                                                               
054400                                                                          
054500     MOVE +2                           TO INDX                            
054600     MOVE +1                           TO INDX1                           
054700     PERFORM UNTIL INDX                >  MAX-INDX                        
054800        MOVE LINK-KVBEHOV-VECKA(INDX1) TO MOD-KVPB-SEP(INDX)              
054900        ADD  LINK-KVBEHOV-VECKA(INDX1) TO W-KVPB-TOT(INDX)                
055000        ADD +1                         TO INDX                            
055100                                          INDX1                           
055200     END-PERFORM                                                          
055300     .                                                                    
055400     EJECT                                                                
055500 FD-SATS-BEHOV  SECTION.                                                  
055600     MOVE 'FD-SATS-BEHOV '  TO CURRENT-SECTION                            
055700                                                                          
055800     IF ART-FLIART                 =  JA                                  
055900        MOVE 13                    TO LINK-KVVECKOR-BEHOV                 
056000        MOVE ENDAST-SATSBEHOV      TO LINK-KDBEHOV                        
056100        MOVE NEJ                   TO LINK-FLINKLDIRLEV                   
056200                                                                          
056300        IF ART-KDERS-UTG = +0                                             
056400          CALL W22222 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB         
056500                                      W222-ARTM-PCB W222-2501-PCB         
056600                                      W222-WDB6R-PCB                      
056700                                      W222-WDK7R-PCB                      
056800                                      W222-WDB6-PCB W222-WDD7-PCB         
056900                                      W222-WDK7E-PCB                      
057000                                      UTIL-WDK6-PCB                       
057100                                      UTIL-WDK7-PCB                       
057200                                      UTIL-WDB6-PCB                       
057300                                      UTUP-WDK7-PCB                       
057400                                      UTUP-WDB6-PCB                       
057500                                      UTUP-UTIL-WDK6-PCB                  
057600                                      UTUP-UTIL-WDK7-PCB                  
057700                                      UTUP-UTIL-WDB6-PCB                  
057800                                                                          
057900          IF LINK-ANROP-FEL                                               
058000            PERFORM S01-NOLLA-W22222                                      
058100          END-IF                                                          
058200        ELSE                                                              
058300          PERFORM S01-NOLLA-W22222                                        
058400        END-IF                                                            
058500                                                                          
058600        MOVE +1                     TO INDX                               
058700        MOVE LINK-KVBEHOV-DESSUTOM  TO MOD-KVPB-SATS(INDX)                
058800        ADD  LINK-KVBEHOV-DESSUTOM  TO W-KVPB-TOT(INDX)                   
058900                                                                          
059000        MOVE +2                     TO INDX                               
059100        MOVE +1                     TO INDX1                              
059200        PERFORM UNTIL INDX                >  MAX-INDX                     
059300           MOVE LINK-KVBEHOV-VECKA(INDX1) TO MOD-KVPB-SATS(INDX)          
059400           ADD LINK-KVBEHOV-VECKA(INDX1)  TO W-KVPB-TOT(INDX)             
059500           ADD +1                         TO INDX                         
059600                                             INDX1                        
059700        END-PERFORM                                                       
059800     ELSE                                                                 
059900        MOVE +1                     TO INDX                               
060000        PERFORM UNTIL INDX          >  MAX-INDX                           
060100           MOVE ZERO                TO MOD-KVPB-SATS(INDX)                
060200           ADD +1                   TO INDX                               
060300        END-PERFORM                                                       
060400     END-IF                                                               
060500                                                                          
060600     .                                                                    
060700     EJECT                                                                
060800 FE-TPO-BEHOV  SECTION.                                                   
060900     MOVE 'FE-TPO-BEHOV '  TO CURRENT-SECTION                             
061000                                                                          
061100     MOVE 13                       TO LINK-KVVECKOR-BEHOV                 
061200     MOVE ENDAST-LEVBEHOV          TO LINK-KDBEHOV                        
061300     MOVE NEJ                      TO LINK-FLINKLDIRLEV                   
061400                                                                          
061500     IF ART-KDERS-UTG = +0                                                
061600       CALL W22222 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB            
061700                                   W222-ARTM-PCB W222-2501-PCB            
061800                                   W222-WDB6R-PCB                         
061900                                   W222-WDK7R-PCB                         
062000                                   W222-WDB6-PCB W222-WDD7-PCB            
062100                                   W222-WDK7E-PCB                         
062200                                   UTIL-WDK6-PCB                          
062300                                   UTIL-WDK7-PCB                          
062400                                   UTIL-WDB6-PCB                          
062500                                   UTUP-WDK7-PCB                          
062600                                   UTUP-WDB6-PCB                          
062700                                   UTUP-UTIL-WDK6-PCB                     
062800                                   UTUP-UTIL-WDK7-PCB                     
062900                                   UTUP-UTIL-WDB6-PCB                     
063000                                                                          
063100       IF LINK-ANROP-FEL                                                  
063200          PERFORM S01-NOLLA-W22222                                        
063300       END-IF                                                             
063400     ELSE                                                                 
063500       PERFORM S01-NOLLA-W22222                                           
063600     END-IF                                                               
063700                                                                          
063800     MOVE +1                        TO INDX                               
063900     MOVE LINK-KVBEHOV-DESSUTOM     TO MOD-KVPB-TPO(INDX)                 
064000     ADD  LINK-KVBEHOV-DESSUTOM     TO W-KVPB-TOT(INDX)                   
064100                                                                          
064200     MOVE +2                        TO INDX                               
064300     MOVE +1                        TO INDX1                              
064400                                                                          
064500     PERFORM UNTIL INDX                  >  MAX-INDX                      
064600        MOVE LINK-KVBEHOV-VECKA(INDX1)   TO MOD-KVPB-TPO(INDX)            
064700        ADD LINK-KVBEHOV-VECKA(INDX1)    TO W-KVPB-TOT(INDX)              
064800        ADD +1                           TO INDX                          
064900                                            INDX1                         
065000     END-PERFORM                                                          
065100     .                                                                    
065200     EJECT                                                                
065300 FF-SDC-BEHOV  SECTION.                                                   
065400     MOVE 'FF-SDC-BEHOV '  TO CURRENT-SECTION                             
065500                                                                          
065600     IF SEGMENT-FINNS AND                                                 
065700        CLAG-FLREFILL              =  JA                                  
065800        MOVE 13                    TO LINK-KVVECKOR-BEHOV                 
065900        MOVE ENDAST-SDCBEHOV       TO LINK-KDBEHOV                        
066000        MOVE NEJ                   TO LINK-FLINKLDIRLEV                   
066100                                                                          
066200        IF ART-KDERS-UTG = +0                                             
066300          CALL W22222 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB         
066400                                      W222-ARTM-PCB W222-2501-PCB         
066500                                      W222-WDB6R-PCB                      
066600                                      W222-WDK7R-PCB                      
066700                                      W222-WDB6-PCB W222-WDD7-PCB         
066800                                      W222-WDK7E-PCB                      
066900                                      UTIL-WDK6-PCB                       
067000                                      UTIL-WDK7-PCB                       
067100                                      UTIL-WDB6-PCB                       
067200                                      UTUP-WDK7-PCB                       
067300                                      UTUP-WDB6-PCB                       
067400                                      UTUP-UTIL-WDK6-PCB                  
067500                                      UTUP-UTIL-WDK7-PCB                  
067600                                      UTUP-UTIL-WDB6-PCB                  
067700                                                                          
067800           IF LINK-ANROP-FEL                                              
067900              PERFORM S01-NOLLA-W22222                                    
068000           END-IF                                                         
068100        ELSE                                                              
068200          PERFORM S01-NOLLA-W22222                                        
068300        END-IF                                                            
068400                                                                          
068500        MOVE +1                     TO INDX                               
068600        MOVE LINK-KVBEHOV-DESSUTOM  TO MOD-KVPB-SDC(INDX)                 
068700        ADD  LINK-KVBEHOV-DESSUTOM  TO W-KVPB-TOT(INDX)                   
068800                                                                          
068900        MOVE +2                            TO INDX                        
069000        MOVE +1                            TO INDX1                       
069100        PERFORM UNTIL INDX                 >  MAX-INDX                    
069200           MOVE LINK-KVBEHOV-VECKA(INDX1)  TO MOD-KVPB-SDC(INDX)          
069300           ADD LINK-KVBEHOV-VECKA(INDX1)   TO W-KVPB-TOT(INDX)            
069400           ADD +1                          TO INDX                        
069500                                              INDX1                       
069600        END-PERFORM                                                       
069700     ELSE                                                                 
069800        MOVE +1                     TO INDX                               
069900        PERFORM UNTIL INDX          >  MAX-INDX                           
070000           MOVE ZERO                TO MOD-KVPB-SDC(INDX)                 
070100           ADD +1                   TO INDX                               
070200        END-PERFORM                                                       
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 FG-NDC-BEHOV  SECTION.                                                   
070700     MOVE 'FG-NDC-BEHOV '  TO CURRENT-SECTION                             
070800                                                                          
070900     IF SEGMENT-FINNS                                                     
071000        MOVE 13                    TO LINK-KVVECKOR-BEHOV                 
071100        MOVE ENDAST-NDCBEHOV       TO LINK-KDBEHOV                        
071200        MOVE NEJ                   TO LINK-FLINKLDIRLEV                   
071300                                                                          
071400        IF ART-KDERS-UTG = +0                                             
071500          CALL W22222 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB         
071600                                      W222-ARTM-PCB W222-2501-PCB         
071700                                      W222-WDB6R-PCB                      
071800                                      W222-WDK7R-PCB                      
071900                                      W222-WDB6-PCB W222-WDD7-PCB         
072000                                      W222-WDK7E-PCB                      
072100                                      UTIL-WDK6-PCB                       
072200                                      UTIL-WDK7-PCB                       
072300                                      UTIL-WDB6-PCB                       
072400                                      UTUP-WDK7-PCB                       
072500                                      UTUP-WDB6-PCB                       
072600                                      UTUP-UTIL-WDK6-PCB                  
072700                                      UTUP-UTIL-WDK7-PCB                  
072800                                      UTUP-UTIL-WDB6-PCB                  
072900                                                                          
073000          IF LINK-ANROP-FEL                                               
073100            PERFORM S01-NOLLA-W22222                                      
073200          END-IF                                                          
073300        ELSE                                                              
073400          PERFORM S01-NOLLA-W22222                                        
073500        END-IF                                                            
073600                                                                          
073700        MOVE +1                     TO INDX                               
073800        MOVE LINK-KVBEHOV-DESSUTOM  TO MOD-KVPB-NDC(INDX)                 
073900        ADD  LINK-KVBEHOV-DESSUTOM  TO W-KVPB-TOT(INDX)                   
074000                                                                          
074100        MOVE +2                            TO INDX                        
074200        MOVE +1                            TO INDX1                       
074300        PERFORM UNTIL INDX                 >  MAX-INDX                    
074400           MOVE LINK-KVBEHOV-VECKA(INDX1)  TO MOD-KVPB-NDC(INDX)          
074500           ADD LINK-KVBEHOV-VECKA(INDX1)   TO W-KVPB-TOT(INDX)            
074600           ADD +1                          TO INDX                        
074700                                              INDX1                       
074800        END-PERFORM                                                       
074900     ELSE                                                                 
075000        MOVE +1                     TO INDX                               
075100        PERFORM UNTIL INDX          >  MAX-INDX                           
075200           MOVE ZERO                TO MOD-KVPB-NDC(INDX)                 
075300           ADD +1                   TO INDX                               
075400        END-PERFORM                                                       
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075800 FH-TOT-BEHOV       SECTION.                                              
075900     MOVE 'FH-TOT-BEHOV '  TO CURRENT-SECTION                             
076000                                                                          
076100     MOVE +1                        TO INDX                               
076200     PERFORM UNTIL INDX             >  MAX-INDX                           
076300        IF W-KVPB-TOT(INDX) > ZERO                                        
076400           MOVE W-KVPB-TOT(INDX)    TO MOD-KVPB-TOT(INDX)                 
076500        ELSE                                                              
076600           MOVE ZERO                TO MOD-KVPB-TOT(INDX)                 
076700        END-IF                                                            
076800        ADD +1                      TO INDX                               
076900     END-PERFORM                                                          
077000     .                                                                    
077100     EJECT                                                                
077200 FI-PBPLAN  SECTION.                                                      
077300     MOVE 'FI-PBPLAN '  TO CURRENT-SECTION                                
077400                                                                          
077500                                                                          
077600     IF SEGMENT-FINNS AND                                                 
077700      ((CLAG-DAPBPLAN > WS-DAGENS-DATUM OR                                
077800        CLAG-DAPBPLAN = WS-DAGENS-DATUM)OR                                
077900       (CLAG-TIPBPLAN-JUST1-FOM > +0)   OR                                
078000       (CLAG-DASEASON > WS-DAGENS-DATUM OR                                
078100        CLAG-DASEASON = WS-DAGENS-DATUM))                                 
078200                                                                          
078300        MOVE 13                    TO LINK-KVVECKOR-BEHOV                 
078400        MOVE SEP-SATS-TPO-LEV-SDC-NDC                                     
078500                                   TO LINK-KDBEHOV                        
078600        MOVE NEJ                   TO LINK-FLINKLDIRLEV                   
078700                                                                          
078800        IF ART-KDERS-UTG = +0                                             
078900          CALL W22222 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB         
079000                                      W222-ARTM-PCB W222-2501-PCB         
079100                                      W222-WDB6R-PCB                      
079200                                      W222-WDK7R-PCB                      
079300                                      W222-WDB6-PCB W222-WDD7-PCB         
079400                                      W222-WDK7E-PCB                      
079500                                      UTIL-WDK6-PCB                       
079600                                      UTIL-WDK7-PCB                       
079700                                      UTIL-WDB6-PCB                       
079800                                      UTUP-WDK7-PCB                       
079900                                      UTUP-WDB6-PCB                       
080000                                      UTUP-UTIL-WDK6-PCB                  
080100                                      UTUP-UTIL-WDK7-PCB                  
080200                                      UTUP-UTIL-WDB6-PCB                  
080300                                                                          
080400          IF LINK-ANROP-FEL                                               
080500            PERFORM S01-NOLLA-W22222                                      
080600          END-IF                                                          
080700        ELSE                                                              
080800          PERFORM S01-NOLLA-W22222                                        
080900        END-IF                                                            
081000                                                                          
081100        MOVE +1                     TO INDX                               
081200                                                                          
081300        ACCEPT W-TIME               FROM TIME                             
081400        COMPUTE W-VECKO-SEP-BEHOV ROUNDED = CLAG-KVPB-SEP / 4.33          
081500        COMPUTE W-DAG-SEP-BEHOV ROUNDED =  W-VECKO-SEP-BEHOV / 5          
081600                                                                          
081700        DIVIDE ART-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                    
081800        MOVE W-TIAAVV-NUM         TO TMP1-YYWW                            
081900        MOVE W-TIFINLV-AAVV       TO TMP2-YYWW                            
082000        PERFORM WY2000P3                                                  
082100                                                                          
082200        IF DAT-TID = 6 OR                                                 
082300            DAT-TID = 7 OR                                                
082400           (DAT-TID = 5 AND W-TIME(1:4) > 1700)                           
082500        OR  TMP1-YYWW < TMP2-YYWW                                         
082600                                                                          
082700            CONTINUE                                                      
082800        ELSE                                                              
082900            COMPUTE W-KVDAGAR-KVAR = 5 - DAT-TID                          
083000            IF W-TIME(1:4)      <= 1700                                   
083100               ADD +1           TO W-KVDAGAR-KVAR                         
083200            END-IF                                                        
083300                                                                          
083400*---FINNS BARA SEP-BEHOV OCH DET ÄR NEGATIV TREND SÅ SÄTTER               
083500*---BEHOVSMODULEN ZERO I LINK-KVBEHOV-DESSUTOM.                           
083600            IF LINK-KVBEHOV-DESSUTOM = ZERO                               
083700              ADD  WS-KVPB-TREND-RAD1 TO LINK-KVBEHOV-DESSUTOM            
083800            END-IF                                                        
083900                                                                          
084000            MOVE +1 TO W-FAKTOR                                           
084100            SUBTRACT CLAG-REDIRLEV FROM W-FAKTOR                          
084200            COMPUTE LINK-KVBEHOV-DESSUTOM ROUNDED =                       
084300                    LINK-KVBEHOV-DESSUTOM +                               
084400                                        (W-DAG-SEP-BEHOV *                
084500                                         W-KVDAGAR-KVAR  *                
084600                                         W-FAKTOR)                        
084700        END-IF                                                            
084800        MOVE LINK-KVBEHOV-DESSUTOM  TO MOD-KVPB-PLAN(INDX)                
084900                                                                          
085000        MOVE +2                            TO INDX                        
085100        MOVE +1                            TO INDX1                       
085200        PERFORM UNTIL INDX                 >  MAX-INDX                    
085300           MOVE LINK-KVBEHOV-VECKA(INDX1)  TO MOD-KVPB-PLAN(INDX)         
085400           ADD +1                          TO INDX                        
085500                                              INDX1                       
085600        END-PERFORM                                                       
085700     ELSE                                                                 
085800        MOVE +1                     TO INDX                               
085900        PERFORM UNTIL INDX          >  MAX-INDX                           
086000           MOVE ZERO                TO MOD-KVPB-PLAN(INDX)                
086100           ADD +1                   TO INDX                               
086200        END-PERFORM                                                       
086300     END-IF                                                               
086400     .                                                                    
086500     EJECT                                                                
086600 FJ-TREND   SECTION.                                                      
086700     MOVE 'FJ-TREND '  TO CURRENT-SECTION                                 
086800                                                                          
086900     IF SEGMENT-FINNS                                                     
087000        IF CLAG-KVPB-TREND NOT = ZERO                                     
087100           MOVE CLAG-KVVECKOR-TREND    TO MAX-KVVECKOR-TREND              
087200           MOVE +1                     TO INDX                            
087300           MOVE +1                     TO INDX-T                          
087400           PERFORM UNTIL INDX          >  MAX-INDX                        
087500              PERFORM FJA-TRENDBER                                        
087600              COMPUTE SPAR-KVPB-TOT ROUNDED =                             
087700                 W-KVPB-TOT (INDX) / 2                                    
087800              ADD  WS-KVPB-TREND   TO W-KVPB-TOT (INDX)                   
087900*                  ÄR DET RÄTT TECKEN ??????                              
088000              IF W-KVPB-TOT (INDX) < SPAR-KVPB-TOT                        
088100                 MOVE SPAR-KVPB-TOT TO W-KVPB-TOT (INDX)                  
088200              END-IF                                                      
088300              ADD +1           TO INDX                                    
088400              ADD +1           TO INDX-T                                  
088500           END-PERFORM                                                    
088600        ELSE                                                              
088700           MOVE +1                     TO INDX                            
088800           PERFORM UNTIL INDX          >  MAX-INDX                        
088900              MOVE ZERO        TO MOD-KVPB-TREND (INDX)                   
089000              ADD +1           TO INDX                                    
089100           END-PERFORM                                                    
089200        END-IF                                                            
089300     ELSE                                                                 
089400           MOVE +1                     TO INDX                            
089500           PERFORM UNTIL INDX          >  MAX-INDX                        
089600              MOVE ZERO        TO MOD-KVPB-TREND (INDX)                   
089700              ADD +1           TO INDX                                    
089800           END-PERFORM                                                    
089900     END-IF                                                               
090000     .                                                                    
090100     EJECT                                                                
090200 FJA-TRENDBER SECTION.                                                    
090300     MOVE 'FJA-TRENDBER '  TO CURRENT-SECTION                             
090400                                                                          
090500******************************************************************        
090600*                                                                *        
090700*    BERÄKNING AV TRENDBEHOV                                     *        
090800*                                                                *        
090900******************************************************************        
091000                                                                          
091100        IF DAT-TID > 5 OR (DAT-TID = 5 AND WS-DAGENS-TIMME > 17)          
091200***        VECKOHELG ?                                                    
091300           IF INDX-T NOT < MAX-KVVECKOR-TREND                             
091400              MOVE MAX-KVVECKOR-TREND TO INDX-T                           
091500           END-IF                                                         
091600           COMPUTE WS-KVPB-TREND ROUNDED =                                
091700                  INDX-T * CLAG-KVPB-TREND / 4.33                         
091800           END-COMPUTE                                                    
091900           MOVE    WS-KVPB-TREND       TO                                 
092000                   MOD-KVPB-TREND (INDX)                                  
092100        ELSE                                                              
092200***        MITT I VECKAN                                                  
092300           IF INDX-T NOT < MAX-KVVECKOR-TREND                             
092400              MOVE MAX-KVVECKOR-TREND TO INDX-T                           
092500           END-IF                                                         
092600           COMPUTE WS-KVPB-TREND ROUNDED =                                
092700                  INDX-T * CLAG-KVPB-TREND / 4.33                         
092800           END-COMPUTE                                                    
092900           IF INDX = 1                                                    
093000              IF DAT-TID = 2                                              
093100                 COMPUTE WS-KVPB-TREND ROUNDED =                          
093200                         WS-KVPB-TREND * 0.80                             
093300              END-IF                                                      
093400              IF DAT-TID = 3                                              
093500                 COMPUTE WS-KVPB-TREND ROUNDED =                          
093600                         WS-KVPB-TREND * 0.60                             
093700              END-IF                                                      
093800              IF DAT-TID = 4                                              
093900                 COMPUTE WS-KVPB-TREND ROUNDED =                          
094000                         WS-KVPB-TREND * 0.40                             
094100              END-IF                                                      
094200              IF DAT-TID = 5                                              
094300                 COMPUTE WS-KVPB-TREND ROUNDED =                          
094400                         WS-KVPB-TREND * 0.20                             
094500              END-IF                                                      
094600           END-IF                                                         
094700           MOVE WS-KVPB-TREND     TO MOD-KVPB-TREND (INDX)                
094800           IF INDX = 1                                                    
094900             MOVE WS-KVPB-TREND   TO WS-KVPB-TREND-RAD1                   
095000           END-IF                                                         
095100        END-IF                                                            
095200     .                                                                    
095300     EJECT                                                                
095400 I-JUMP-TO-SCREEN-2158 SECTION.                                           
095500     MOVE 'I-JUMP-TO-SCREEN-2158' TO CURRENT-SECTION                      
095600                                                                          
095700     COMPUTE INDX = MID-CURSOR-RAD - 5                                    
095800     END-COMPUTE                                                          
095900                                                                          
096000     IF INDX > 0 AND INDX < 15                                            
096100        COMPUTE ALT-LL = LENGTH OF ALT-MID-W2I15801 + 17                  
096200        MOVE ALL '+'               TO ALT-MID                             
096300        MOVE MSGI-IDARTNR          TO ALT-MID-IDARTNR-IN                  
096400        MOVE MID-TIAAVV(INDX)      TO ALT-MID-TIAAVV-IN                   
096500        MOVE MFS-KDMFSFOR          TO ALT-KDMFSFOR                        
096600                                                                          
096700        PERFORM IMS-INSERT-ALT-2158                                       
096800     ELSE                                                                 
096900        MOVE ERR-NO-LINE-SELECTED    TO MED-IDMFSFEL                      
097000        CALL WMEDKONV             USING MED-WMEDAREA                      
097100        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
097200        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O12801 + 4                     
097300        PERFORM IMS-INSERT-MSG                                            
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700 S01-NOLLA-W22222 SECTION.                                                
097800     MOVE 'S01-NOLLA-W22222 '  TO CURRENT-SECTION                         
097900                                                                          
098000     MOVE +0                         TO LINK-KVBEHOV-SUMMA                
098100                                        LINK-KVBEHOV-DESSUTOM             
098200                                        LINK-TIBEHOV-FIRST                
098300                                                                          
098400     MOVE +1                         TO INDX                              
098500     PERFORM UNTIL INDX >  MAX-INDX                                       
098600       MOVE +0                      TO LINK-KVBEHOV-VECKA(INDX)           
098700       ADD +1                        TO INDX                              
098800     END-PERFORM                                                          
098900     .                                                                    
099000     EJECT                                                                
099100 MFS-RENSA-FAELT-UT SECTION.                                              
099200                                                                          
099300*    --- ALLA UTDATA-FÄLT                                                 
099400     MOVE 1                 TO INDX                                       
099500     PERFORM UNTIL INDX     >  MAX-INDX                                   
099600       MOVE MFS-RENSA-FAELT TO MOD-TIAAVV   (INDX)                        
099700                               MOD-KVPB-SEP (INDX)                        
099800                               MOD-KVPB-SATS(INDX)                        
099900                               MOD-KVPB-TPO (INDX)                        
100000                               MOD-KVPB-SDC (INDX)                        
100100                               MOD-KVPB-NDC (INDX)                        
100200                               MOD-KVPB-TOT (INDX)                        
100300       ADD 1                TO INDX                                       
100400     END-PERFORM                                                          
100500     .                                                                    
100600     EJECT                                                                
100700* --- IMS SEKTIONER ---                                                   
100800     SKIP3                                                                
100900 IMS-GET-MSG SECTION.                                                     
101000                                                                          
101100     MOVE '  QC' TO GODK-STATUSKODER                                      
101200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
101300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101400     PERFORM IMS-STATUSKONTROLL                                           
101500     .                                                                    
101600     SKIP3                                                                
101700 IMS-INSERT-MSG SECTION.                                                  
101800                                                                          
101900     IF ENGLISH-TEXT                                                      
102000       MOVE 'N' TO MFS-KDHUVOMR                                           
102100     END-IF                                                               
102200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
102300     MOVE SPACE TO GODK-STATUSKODER                                       
102400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
102500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102600     PERFORM IMS-STATUSKONTROLL                                           
102700     .                                                                    
102800     EJECT                                                                
102900 IMS-GU-ARTC01 SECTION.                                                   
103000     MOVE 'IMS-GU-ARTC01 '  TO DBS-SECTION                                
103100                                                                          
103200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
103300          DELIMITED BY SIZE INTO SSA1                                     
103400     MOVE '  GE' TO GODK-STATUSKODER                                      
103500     CALL CBLTDLI USING GU ARTC1-PCB DLI-IO-AREA1 SSA1                    
103600     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     .                                                                    
103900     SKIP3                                                                
104000 IMS-GNP-ARTC11 SECTION.                                                  
104100     MOVE 'IMS-GNP-ARTC11 '  TO DBS-SECTION                               
104200                                                                          
104300     MOVE 'WLARTC11 ' TO SSA1                                             
104400     MOVE '  GE' TO GODK-STATUSKODER                                      
104500     CALL CBLTDLI USING GNP ARTC1-PCB DLI-IO-AREA2 SSA1                   
104600     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     .                                                                    
104900     EJECT                                                                
105000 IMS-INSERT-ALT-2158 SECTION.                                             
105100     MOVE 'IMS-INSERT-ALT-2158      ' TO DBS-SECTION                      
105200                                                                          
105300     MOVE SPACE           TO GODK-STATUSKODER                             
105400     CALL CBLTDLI      USING ISRT ALT-PCB ALT-MSG-AREA                    
105500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
105600     PERFORM IMS-STATUSKONTROLL                                           
105700     .                                                                    
105800                                                                          
105900 IMS-STATUSKONTROLL SECTION.                                              
106000                                                                          
106100     SET STATUS-IX TO 1                                                   
106200     SEARCH GODK-STATUS                                                   
106300       AT END                                                             
106400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
106500         DELIMITED BY SIZE INTO FELTEXT                                   
106600         CALL FELLOG                                                      
106700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
106800         CONTINUE                                                         
106900     END-SEARCH                                                           
107000     .                                                                    
107100     EJECT                                                                
107200*    -COPY WY2000P3                                                       
