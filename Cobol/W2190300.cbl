000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2190300.                                                
000400 AUTHOR.         SVANTE BJÖRKBERG                                         
000500 DATE-WRITTEN.   91/10/30.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        TÄCKNING AV KAMPANJER.                                           
001100*        PROGRAMMET GÅR VARJE NATT (LÄSER WDM2) OCH RESERVERAR            
001200*        DET ANTAL ARTIKLAR (PÅ WLARTC OCH WLARTM) EN KUND HAR            
001300*        BEGÄRT FÖR EN KAMPANJ.                                           
001400*                                                                         
001500*        THE PROGRAM READS   WDM2                                         
001600*        THE PROGRAM UPDATES WLARTC (WDK6)                                
001700*        THE PROGRAM UPDATES WLARTM (WDK9)                                
001800*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300     SKIP2                                                                
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600         SELECT W21902       ASSIGN TO W21903D1.                          
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000                                                                          
003100 FD  W21902                                                               
003200     LABEL RECORD STANDARD                                                
003300     RECORDING F                                                          
003400     BLOCK CONTAINS 0.                                                    
003500                                                                          
003600 01  IN-POST.                                                             
003700   03 -COPY W219001 -L.                                                   
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000     SKIP2                                                                
004100*    -COPY WY2000W1                                                       
004200     SKIP3                                                                
004300 01  IDPGM                       PIC X(8)    VALUE 'W2190300'.            
004400 01  JA                          PIC X       VALUE 'J'.                   
004500 01  NEJ                         PIC X       VALUE 'N'.                   
004600 01  WS-DAGENS-DATUM             PIC 9(6).                                
004700 01  WS-DIFF                     PIC S9(7)   COMP-3.                      
004800 01  WS-KVDISP                   PIC S9(7)   COMP-3.                      
004900 01  WS-KVDISP-KAMP              PIC S9(7)   COMP-3.                      
005000 01  WS-SUBEART-KAMP             PIC S9(9)   COMP-3.                      
005100 01  WS-SURESS-KAMP              PIC S9(9)   COMP-3.                      
005200 01  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005300 01  ABENDMSG1                   PIC X(80)   VALUE                        
005400     'KONVERTERING I WDATKONV BLEV FELAKTIG.SE PARAM.TILL DATK'.          
005500 01  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
005600 01  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005700 01  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
005800 01  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
005900 01  CHKP-ANT                    PIC S9(3)   COMP-3 VALUE ZERO.           
006000 01  CHKP-MAX                    PIC S9(3)   COMP-3 VALUE 50.             
006100 01  W21902-EOF                  PIC X       VALUE 'N'.                   
006200                                                                          
006300 01  WS-DABEHOV                  PIC 9(6).                                
006400 01  FILLER REDEFINES WS-DABEHOV.                                         
006500     03  WS-DABEHOV-SEKEL        PIC 9(2).                                
006600     03  WS-DABEHOV-YEAR         PIC 9(2).                                
006700     03  WS-DABEHOV-WEEK         PIC 9(2).                                
006800*      --- VALID IDDC CODES                                               
006900*                                                                         
007000*01    -COPY WWDC99                                                       
007100       EJECT                                                              
007200                                                                          
007300 01  GENERAL-SUBPROGRAM.                                                  
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007900                                                                          
008000 01  RKOD-ABEND                  PIC S9(4) VALUE +33 COMP SYNC.           
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
008300     SKIP3                                                                
008400     -COPY WDATAREA                                                       
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'POSTSUM '.            
008700     SKIP3                                                                
008800     -COPY W0005   -PRE POSTSUM-.                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IN-AREA '.            
009100     SKIP3                                                                
009200 01  IN-AREA.                                                             
009300   03 -COPY W219001 -PRE IN-.                                             
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700*    --- STATUS-CODE FROM IMS                                             
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FINNS                       VALUE '  '.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUS-KODER.                                                   
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP2                                                                
010700 01  SSA1                        PIC X(96).                               
010800 01  SSA2                        PIC X(96).                               
010900 01  SSA3                        PIC X(96).                               
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'NYCKLAR '.            
011200     SKIP3                                                                
011300 01  KEYS-TO-DLI.                                                         
011400     03  W-WDM201-X.                                                      
011500         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
011600         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
011700                                                                          
011800     03  W-WDM211-IDARTNR-X.                                              
011900         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
012000                                                                          
012100     03  W-IDARTNR-WLARTC01-X.                                            
012200         05  W-IDARTNR-WLARTC01  PIC S9(09)   VALUE ZERO COMP-3.          
012300                                                                          
012400     03  W-IDARTNR-WLARTM01-X.                                            
012500         05  W-IDARTNR-WLARTM01  PIC S9(09)   VALUE ZERO COMP-3.          
012600                                                                          
012700     03  W-WDK911KY-X.                                                    
012800         05  W-DABEHOV-WLARTM11  PIC 9(06)   VALUE ZERO.                  
012900     EJECT                                                                
013000*    --- IMS FUNCTION CODES                                               
013100 01  FILLER                      PIC X(16)   VALUE 'IMS-CALL'.            
013200     SKIP3                                                                
013300 01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
013700 01  DLI-IO-WDM201.                                                       
013800*    03 -COPY WDM201                                                      
013900     EJECT                                                                
014000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
014100 01  DLI-IO-WDM211.                                                       
014200*    03 -COPY WDM211                                                      
014300     EJECT                                                                
014400                                                                          
014500 01  FILLER                      PIC X(16)   VALUE 'WLARTC11   '.         
014600     SKIP3                                                                
014700     -COPY WDK611                                                         
014800     EJECT                                                                
014900                                                                          
015000 01  FILLER                      PIC X(16)   VALUE 'WLARTM01   '.         
015100     SKIP3                                                                
015200     -COPY WDK901                                                         
015300     EJECT                                                                
015400                                                                          
015500 01  FILLER                      PIC X(16)   VALUE 'WLARTM11   '.         
015600     SKIP3                                                                
015700     -COPY WDK911                                                         
015800     EJECT                                                                
015900*                                                                         
016000 LINKAGE SECTION.                                                         
016100                                                                          
016200 01  -COPY W0009      -PRE MSG-                                           
016300     EJECT                                                                
016400 01  -COPY W0008      -PRE WDM2-                                          
016500     05  FILLER                  PIC X.                                   
016600     EJECT                                                                
016700 01  -COPY W0008      -PRE ARTC-                                          
016800     05  FILLER                  PIC X.                                   
016900     EJECT                                                                
017000 01  -COPY W0008      -PRE ARTM-                                          
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING MSG-PCB WDM2-PCB ARTC-PCB ARTM-PCB.            
017400     ENTRY 'DLITCBL' USING MSG-PCB WDM2-PCB ARTC-PCB ARTM-PCB.            
017500                                                                          
017600     PERFORM A-INIT                                                       
017700     PERFORM B-LAS-W21902                                                 
017800                                                                          
017900     PERFORM UNTIL W21902-EOF = JA                                        
018000       PERFORM C-LAS-ARTREG                                               
018100       IF SEGMENT-FINNS                                                   
018200                                                                          
018300         MOVE IN-IDKAMPRF                  TO W-KAMP-IDKAMPRF             
018400         MOVE IN-IDDC                      TO W-KAMP-IDDC                 
018500         MOVE IN-IDARTNR                   TO W-KART-IDARTNR              
018600                                                                          
018700         PERFORM D-RESERVERA-OCH-ADDERA-RESKAMP                           
018800         IF CHKP-ANT > CHKP-MAX                                           
018900           PERFORM IMS-CHECKPOINT                                         
019000         END-IF                                                           
019100         PERFORM E-ADD-RESERVERAT-FOR-KAMPANJ                             
019200         IF CHKP-ANT > CHKP-MAX                                           
019300           PERFORM IMS-CHECKPOINT                                         
019400         END-IF                                                           
019500       END-IF                                                             
019600       PERFORM B-LAS-W21902                                               
019700     END-PERFORM                                                          
019800                                                                          
019900     PERFORM Z-FINIT                                                      
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT                                  SECTION.                         
020400                                                                          
020500     ACCEPT WS-DAGENS-DATUM FROM DATE                                     
020600     OPEN INPUT W21902                                                    
020700     PERFORM IMS-RESTART                                                  
020800     .                                                                    
020900     EJECT                                                                
021000 B-LAS-W21902                            SECTION.                         
021100                                                                          
021200     READ W21902   INTO IN-AREA                                           
021300                   AT END MOVE JA TO W21902-EOF                           
021400     END-READ                                                             
021500                                                                          
021600     IF W21902-EOF = NEJ                                                  
021700       MOVE IDPGM           TO POSTSUM-PROGNAMN                           
021800       MOVE 'W21902'        TO POSTSUM-FDNAMN                             
021900       MOVE 'W21902D1'      TO POSTSUM-DDNAMN2                            
022000       MOVE SPACE           TO POSTSUM-TRANSTYP                           
022100                                                                          
022200       CALL POSTSUM USING POSTSUM-PARM                                    
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 C-LAS-ARTREG                            SECTION.                         
022700                                                                          
022800     MOVE IN-IDARTNR                     TO W-IDARTNR-WLARTC01            
022900     PERFORM IMS-GHU-WLARTC11                                             
023000     .                                                                    
023100     EJECT                                                                
023200 D-RESERVERA-OCH-ADDERA-RESKAMP          SECTION.                         
023300                                                                          
023400     MOVE 0                              TO WS-SUBEART-KAMP               
023500                                            WS-SURESS-KAMP                
023600     PERFORM IMS-GU-WDM201                                                
023601     IF SEGMENT-FINNS                                                     
023610        IF KAMP-TISTODAT > WS-DAGENS-DATUM                                
023620        OR KAMP-TISTODAT = +0                                             
023900           PERFORM IMS-GHNP-WDM211                                        
024000                                                                          
024100           IF SEGMENT-FINNS                                               
024200             MOVE IN-IDDC TO WS-IDDC                                      
024300             IF NOT CDC-SE    AND                                         
024400               (CLAG-KDLTK  = 1 )                                         
024500               CONTINUE                                                   
024600             ELSE                                                         
024700               PERFORM DA-BERAKNA-DISP                                    
024800               PERFORM DB-EV-RESERVERA                                    
024900               PERFORM DC-EV-ADDERA-RESKAMP                               
025000             END-IF                                                       
025100           END-IF                                                         
025300        END-IF                                                            
025310     END-IF                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 DA-BERAKNA-DISP                         SECTION.                         
025700                                                                          
025800     COMPUTE WS-KVDISP = CLAG-KVLS    -                                   
025900                         CLAG-KVRESS  -                                   
026000                         CLAG-KVUTRS  -                                   
026100                         CLAG-KVSPANT                                     
026200                                                                          
026300     .                                                                    
026400     EJECT                                                                
026500 DB-EV-RESERVERA                         SECTION.                         
026600                                                                          
026700     MOVE WS-DAGENS-DATUM   TO TMP1-YYMMDD                                
026800     MOVE KART-TIRES        TO TMP2-YYMMDD                                
026900     PERFORM WY2000P1                                                     
027000     IF TMP1-YYMMDD >= TMP2-YYMMDD      AND                               
027100        KART-TIRES        >  0              AND                           
027200        WS-KVDISP         >  0              AND                           
027300        KART-KVRESS-KAMP  =  0              AND                           
027400*** FIX TILLS DESS ATT DET ÄR LÖST MED TOMMA KAMPANJRADER                 
027500        KART-KVBEART-KAMP > 0                                             
027600       PERFORM DBA-UTFOR-RESERVATION                                      
027700     END-IF                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 DBA-UTFOR-RESERVATION                   SECTION.                         
028100                                                                          
028200     MOVE IN-IDARTNR                     TO W-IDARTNR-WLARTM01            
028300     PERFORM IMS-GHU-WLARTM01                                             
028400     SUBTRACT KART-KVBEART-KAMP          FROM                             
028500                                     ART-SUTPO-TOT                        
028600     PERFORM IMS-REPL-WLARTM01                                            
028700                                                                          
028800     ADD KART-KVBEART-KAMP               TO KART-KVRESS-ART               
028900     PERFORM IMS-REPL-WDM211                                              
029000                                                                          
029100     PERFORM DBAA-KONVERTERA-KART-TIRES                                   
029200     MOVE WS-DABEHOV                     TO W-DABEHOV-WLARTM11            
029300     PERFORM IMS-GHNP-WLARTM11                                            
029400     IF SEGMENT-FINNS                                                     
029500       SUBTRACT KART-KVBEART-KAMP          FROM ANT-SUTPO-EJPB            
029600       IF ANT-SUTPO-EJPB = 0 AND ANT-SUTPO-PB = 0                         
029700         PERFORM IMS-DLET-WLARTM11                                        
029800       ELSE                                                               
029900         PERFORM IMS-REPL-WLARTM11                                        
030000       END-IF                                                             
030100     END-IF                                                               
030200                                                                          
030300     ADD KART-KVRESS-ART                 TO CLAG-KVRESS                   
030400     PERFORM IMS-REPL-WLARTC11                                            
030500     .                                                                    
030600     EJECT                                                                
030700 DBAA-KONVERTERA-KART-TIRES              SECTION.                         
030800                                                                          
030900     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
031000     MOVE KART-TIRES              TO DAT-I-TIDATUM                        
031100     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                     
031200                         DAT-O-TIDATUM, DAT-KDSVAR                        
031300     IF DAT-KDSVAR-OK                                                     
031400       MOVE DAT-TISEKEL           TO WS-DABEHOV-SEKEL                     
031500       MOVE DAT-TIAA-VECKA        TO WS-DABEHOV-YEAR                      
031600       MOVE DAT-TIVV              TO WS-DABEHOV-WEEK                      
031700     ELSE                                                                 
031800       MOVE ABENDMSG1             TO FELTEXT                              
031900       CALL ABEND USING RKOD-ABEND                                        
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 DC-EV-ADDERA-RESKAMP                    SECTION.                         
032400                                                                          
032500     IF KART-KVRESS-KAMP > 0   OR                                         
032600        KART-KVRESS-ART  > 0                                              
032700*                                                                         
032800*                              OM DET ÄR FÖRSTA GÅNGEN MAN                
032900*                              FÖRSÖKER RESERVERA MOT EN KAMPANJ          
033000*                              ÄR KVRESS-KAMP NOLL ÄN SÅ LÄNGE.           
033100*                              DÄRFÖR HAR MAN TESTEN PÅ                   
033200*                              KVRESS-ART SOM TIDIGARE HAR                
033300*                              RÄKNATS UPP.                               
033400*                                                                         
033500       ADD KART-KVBEART-KAMP             TO WS-SUBEART-KAMP               
033600       ADD KART-KVRESS-KAMP              TO WS-SURESS-KAMP                
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 E-ADD-RESERVERAT-FOR-KAMPANJ            SECTION.                         
034100                                                                          
034200     PERFORM IMS-GU-WDM201                                                
034300     IF SEGMENT-FINNS                                                     
034400        IF KAMP-TISTODAT > WS-DAGENS-DATUM                                
034410        OR KAMP-TISTODAT = +0                                             
034500           PERFORM EA-BRERAKNA-DISP-FOR-KAMPANJ                           
034600                                                                          
034700           IF WS-KVDISP-KAMP > 0                                          
034800              PERFORM IMS-GHNP-WDM211                                     
034900                                                                          
035000              IF SEGMENT-FINNS AND                                        
035100                 WS-KVDISP-KAMP > 0                                       
035200                MOVE WS-DAGENS-DATUM   TO TMP1-YYMMDD                     
035300                MOVE KART-TIRES        TO TMP2-YYMMDD                     
035400                PERFORM WY2000P1                                          
035500                IF KART-KVBEART-KAMP >  KART-KVRESS-KAMP   AND            
035600                   TMP1-YYMMDD       >= TMP2-YYMMDD        AND            
035700                   KART-TIRES        >  0                                 
035800                  PERFORM EB-RES-HELA-BEST-ELLER-DEL-AV                   
035900                END-IF                                                    
036000                                                                          
036100              END-IF                                                      
036200           END-IF                                                         
036300        END-IF                                                            
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 EA-BRERAKNA-DISP-FOR-KAMPANJ            SECTION.                         
036800                                                                          
036900     COMPUTE WS-KVDISP-KAMP = CLAG-KVLS      -                            
037000                              CLAG-KVRESS    -                            
037100                              CLAG-KVUTRS    -                            
037200                              CLAG-KVSPANT +                              
037300                             (WS-SUBEART-KAMP - WS-SURESS-KAMP)           
037400     .                                                                    
037500     EJECT                                                                
037600 EB-RES-HELA-BEST-ELLER-DEL-AV           SECTION.                         
037700                                                                          
037800     COMPUTE WS-DIFF = KART-KVBEART-KAMP - KART-KVRESS-KAMP               
037900     END-COMPUTE                                                          
038000                                                                          
038100     IF WS-KVDISP-KAMP > WS-DIFF                                          
038200       MOVE KART-KVBEART-KAMP            TO   KART-KVRESS-KAMP            
038300       SUBTRACT WS-DIFF                  FROM WS-KVDISP-KAMP              
038400     ELSE                                                                 
038500       ADD WS-KVDISP-KAMP                TO KART-KVRESS-KAMP              
038600       MOVE 0                            TO WS-KVDISP-KAMP                
038700     END-IF                                                               
038800                                                                          
038900     PERFORM IMS-REPL-WDM211                                              
039000     .                                                                    
039100     EJECT                                                                
039200 Z-FINIT                                 SECTION.                         
039300                                                                          
039400     CLOSE W21902                                                         
039500                                                                          
039600     MOVE 'S'               TO POSTSUM-OPKOD                              
039700     CALL POSTSUM USING POSTSUM-PARM                                      
039800     .                                                                    
039900     EJECT                                                                
040000* --- IMS SECTIONS  ---                                                   
040100 IMS-RESTART  SECTION.                                                    
040200                                                                          
040300     MOVE SPACE TO MSG-IO-AREA                                            
040400     MOVE '  '  TO GODK-STATUS-KODER                                      
040500     CALL CBLTDLI USING XRST MSG-PCB                                      
040600                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
040700                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
040800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040900     PERFORM IMS-STATUS-KONTROLL                                          
041000                                                                          
041100     IF IMS-EJ-OK                                                         
041200       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
041300       CALL FELLOG                                                        
041400     END-IF                                                               
041500     .                                                                    
041600     SKIP2                                                                
041700 IMS-CHECKPOINT SECTION.                                                  
041800                                                                          
041900     MOVE IDPGM        TO MSG-IO-AREA                                     
042000     MOVE '  XD'       TO GODK-STATUS-KODER                               
042100     CALL CBLTDLI USING CHKP MSG-PCB                                      
042200                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
042300                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
042400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042500     PERFORM IMS-STATUS-KONTROLL                                          
042600     IF IMS-EJ-OK                                                         
042700       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
042800       CALL FELLOG                                                        
042900     END-IF                                                               
043000     MOVE ZERO TO CHKP-ANT                                                
043100     .                                                                    
043200     EJECT                                                                
043300 IMS-GU-WDM201                          SECTION.                          
043400                                                                          
043500     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     MOVE '  GE' TO GODK-STATUS-KODER                                     
043800     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM201 SSA1                    
043900     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUS-KONTROLL                                          
044100     .                                                                    
044200     SKIP2                                                                
044300 IMS-GHNP-WDM211                          SECTION.                        
044400                                                                          
044500     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
044600          DELIMITED BY SIZE INTO SSA1                                     
044700     MOVE '  GE' TO GODK-STATUS-KODER                                     
044800     CALL CBLTDLI USING GHNP WDM2-PCB DLI-IO-WDM211 SSA1                  
044900     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUS-KONTROLL                                          
045100     .                                                                    
045200     SKIP2                                                                
045300 IMS-REPL-WDM211                         SECTION.                         
045400                                                                          
045500     MOVE '  '             TO GODK-STATUS-KODER                           
045600     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
045700     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
045800     PERFORM IMS-STATUS-KONTROLL                                          
045900                                                                          
046000     ADD +1                TO CHKP-ANT                                    
046100     .                                                                    
046200     EJECT                                                                
046300 IMS-GHU-WLARTC11   SECTION.                                              
046400                                                                          
046500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WLARTC01-X ')'                
046600          DELIMITED BY SIZE INTO SSA1                                     
046700     MOVE 'WLARTC11 '         TO SSA2                                     
046800     MOVE '  GE' TO GODK-STATUS-KODER                                     
046900     CALL CBLTDLI USING GHU ARTC-PCB CLAG-WDK611  SSA1 SSA2               
047000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUS-KONTROLL                                          
047200     .                                                                    
047300     SKIP2                                                                
047400 IMS-REPL-WLARTC11                       SECTION.                         
047500                                                                          
047600     MOVE '  ' TO GODK-STATUS-KODER                                       
047700     CALL CBLTDLI USING REPL ARTC-PCB CLAG-WDK611                         
047800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047900     PERFORM IMS-STATUS-KONTROLL                                          
048000     ADD +1 TO CHKP-ANT                                                   
048100     .                                                                    
048200     EJECT                                                                
048300 IMS-GHU-WLARTM01                        SECTION.                         
048400                                                                          
048500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-WLARTM01-X ')'                
048600          DELIMITED BY SIZE INTO SSA1                                     
048700     MOVE '  ' TO GODK-STATUS-KODER                                       
048800     CALL CBLTDLI USING GHU ARTM-PCB ART-WDK901 SSA1                      
048900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
049000     PERFORM IMS-STATUS-KONTROLL                                          
049100     .                                                                    
049200     SKIP2                                                                
049300 IMS-GHNP-WLARTM11                       SECTION.                         
049400                                                                          
049500     STRING 'WLARTM11(DABEHOV  =' W-WDK911KY-X ')'                        
049600          DELIMITED BY SIZE INTO SSA1                                     
049700     MOVE '  GE' TO GODK-STATUS-KODER                                     
049800     CALL CBLTDLI USING GHNP ARTM-PCB ANT-WDK911 SSA1                     
049900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUS-KONTROLL                                          
050100     .                                                                    
050200     SKIP2                                                                
050300 IMS-REPL-WLARTM01                       SECTION.                         
050400                                                                          
050500     MOVE '  ' TO GODK-STATUS-KODER                                       
050600     CALL CBLTDLI USING REPL ARTM-PCB ART-WDK901                          
050700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
050800     PERFORM IMS-STATUS-KONTROLL                                          
050900     ADD +1 TO CHKP-ANT                                                   
051000     .                                                                    
051100     SKIP2                                                                
051200 IMS-REPL-WLARTM11                       SECTION.                         
051300                                                                          
051400     MOVE '  ' TO GODK-STATUS-KODER                                       
051500     CALL CBLTDLI USING REPL ARTM-PCB ANT-WDK911                          
051600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
051700     PERFORM IMS-STATUS-KONTROLL                                          
051800     ADD +1 TO CHKP-ANT                                                   
051900     .                                                                    
052000     SKIP2                                                                
052100 IMS-DLET-WLARTM11                       SECTION.                         
052200                                                                          
052300     MOVE '  ' TO GODK-STATUS-KODER                                       
052400     CALL CBLTDLI USING DLET ARTM-PCB ANT-WDK911                          
052500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUS-KONTROLL                                          
052700     ADD +1 TO CHKP-ANT                                                   
052800     .                                                                    
052900     EJECT                                                                
053000 IMS-STATUS-KONTROLL                     SECTION.                         
053100                                                                          
053200     SET STATUS-IX TO 1                                                   
053300     SEARCH GODK-STATUS                                                   
053400       AT END CALL FELLOG                                                 
053500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
053600     END-SEARCH                                                           
053700     .                                                                    
053800     EJECT                                                                
053900*    -COPY WY2000P1                                                       
