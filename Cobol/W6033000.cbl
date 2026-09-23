000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6033000.                                                
000400 AUTHOR.         KUMAR LOVISH.                                            
000500 DATE-WRITTEN.   22/03/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       CARPARTS.LDC.W60330                                      
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        HELD AT CUSTOMS                                                  
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W60330T                                             
001500*        REQUEST:     WZ01REQU                                            
001600*                     W60330I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    WZ01RESP                                            
002000*                     W60330O1                                            
002100                                                                          
002200*    PROGRAME READS   WDR5(6352)                                          
002300*                                                                         
002400*    PROGRAME UPDATE  WDL6                                                
002500*                     WDL9                                                
002600*                     WDK7                                                
002700*                                                                         
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W6033000'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500 77  RADIND                      PIC S9(9)   VALUE +0   COMP-3.           
004600 77  INDX                        PIC S9(9)   VALUE +0   COMP-3.           
004700 77  MAX-INDX                    PIC S9(4)   VALUE +12  COMP SYNC.        
004800 77  WS-MAX-500-RADER            PIC 9(5)    VALUE 500.                   
004900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
005100 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
005200 77  WS-LOGG-AAAAMMDD            PIC 9(8)    VALUE ZERO.                  
005300 77  WS-KVANTMOT                 PIC 9(6)    VALUE ZERO.                  
005310 77  WS-IDKOLLI                  PIC 9(5)    VALUE ZERO.                  
005500 77  WS-IDFAKT                   PIC S9(7)   COMP-3.                      
005510 77  WS-SAP-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
005520 77  W-IDSEKVNR-SAP              PIC S9(3)   VALUE 0   COMP-3.            
005530 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
005540 77  WS-SAP-IDFAKT               PIC 9(7)  VALUE ZERO.                    
005550 77  WS-SAP-X-IDFAKT             PIC X(7)  VALUE ZERO.                    
005560                                                                          
005600 01  WS.                                                                  
005700   02  WS-DAINLEV                PIC 9(16)   VALUE ZERO.                  
005800   02  WS-TIAAAAMMDDTTMMSSTH     PIC 9(16)   VALUE ZERO.                  
005900   02     FILLER                 REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
006000    03    WS-TISEKEL               PIC 9(2).                              
006100    03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                              
006200    03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                              
006300                                                                          
006400 77  YES                         PIC X       VALUE 'J'.                   
006500 77  NOO                         PIC X       VALUE 'N'.                   
006600                                                                          
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 01  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88  INDATA-OK                           VALUE 'J'.                   
007200     88  INDATA-FEL                          VALUE 'N'.                   
007300                                                                          
007400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007500     88  NYCKLAR-OK                          VALUE 'J'.                   
007600     88  NYCKLAR-FEL                         VALUE 'N'.                   
007700                                                                          
007710*01 -COPY W510AVG                                                         
007800     EJECT                                                                
007900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008000 01  GENERAL-SUBPROGRAMS.                                                 
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008410     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
008500     SKIP3                                                                
008600*    --- PARAMETERS TO ABEND                                              
008700                                                                          
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009100     SKIP3                                                                
009200 01  MESSAGE-CODES.                                                       
009300     03  ERR-NOT-NUM             PIC X(3)    VALUE '024'.                 
009310     03  NO-DATA-INPUT           PIC X(3)    VALUE '014'.                 
009400     03  SYSTEM-ERROR            PIC X(3)    VALUE '099'.                 
009500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009600     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '041'.                 
009700     03  QUANTITY-TOO-BIG        PIC X(3)    VALUE '298'.                 
009710     03  MUST-ENTER-EMP-ID       PIC X(3)    VALUE '026'.                 
009800     EJECT                                                                
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010100     SKIP3                                                                
010200*01  -COPY WZ01SUB                                                        
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010500     SKIP3                                                                
010600 01  REQU-AREA.                                                           
010700*    03  -COPY WZ01REQU                                                   
010800*    03  -COPY W60330I1                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011100     SKIP3                                                                
011200 01  RESP-AREA.                                                           
011300*    03  -COPY WZ01RESP                                                   
011400*    03  -COPY W60330O1                                                   
011500     EJECT                                                                
011600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  KEYS-FOR-DLI.                                                        
012100     03  W-IDARTNR-R5.                                                    
012200         05  W-IDARTNR-6352      PIC S9(8)   VALUE ZERO COMP-3.           
012300     03  W-IDARTNR-X.                                                     
012400         05  W-IDARTNR           PIC S9(8)   VALUE ZERO COMP-3.           
012500     03  W-WDD3-KEY.                                                      
012600         05  W-IDARTNR-D3        PIC S9(8)   VALUE ZERO COMP-3.           
012700     03  W-DAINLEV-X.                                                     
012800         05  W-DAINLEV           PIC 9(16).                               
012900     03  W-WDGX6351-X.                                                    
013000         05  W-IDHTYP-6351       PIC X(4)    VALUE '6351'.                
013100         05  W-IDDC-HAC          PIC X(2)    VALUE '86'.                  
013200         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
013300     03  W-IDSKYLT-X.                                                     
013400         05  W-IDSKYLT           PIC X(3)    VALUE 'GB'.                  
013410     03  W-IDDC-X.                                                        
013420         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013500                                                                          
013600     03  WS-IDDC                 PIC X(2)    VALUE '86'.                  
013700     SKIP2                                                                
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FOUND                       VALUE '  '.                  
014100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014300     88  END-OF-DATABASE                     VALUE 'GB'.                  
014400     SKIP2                                                                
014500 01  GOOD-STATUSCODES.                                                    
014600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                        PIC X(64).                               
014900 01  SSA2                        PIC X(64).                               
014910 01  SSA3                        PIC X(64).                               
015000     EJECT                                                                
015100*    --- IMS FUNCTION CODES                                               
015200*01  -COPY W0003                                                          
015300     EJECT                                                                
015400                                                                          
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6352'.                    
015600 01  DLI-IO-WDGX6352.                                                     
015700*    03  -COPY WDGX6352                                                   
015800     EJECT                                                                
015900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
016000 01  DLI-IO-WDD311.                                                       
016100*    03  -COPY WDD311                                                     
016200     EJECT                                                                
016300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
016400 01  DLI-IO-WDL611.                                                       
016500*    03 -COPY WDL611                                                      
016600     EJECT                                                                
016700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
016800 01  DLI-IO-WDK711.                                                       
016900*    03  -COPY WDK711                                                     
017000     EJECT                                                                
017001 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK728'.                      
017002 01  DLI-IO-WDK728.                                                       
017003*    03  -COPY WDK728                                                     
017004     EJECT                                                                
017100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL901'.                      
017200 01  DLI-IO-WDL901.                                                       
017300*    03 -COPY WDL901                                                      
017400     EJECT                                                                
017410 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR801'.              
017420 01  DLI-IO-WDR801.                                                       
017430*    03  WDR801    -COPY WDR801 -PRE EKO-                                 
017440*    07  -COPY W510EKHA  -RED EKO-FIL-WDR801-DATA -PRE EKO-               
017450     EJECT                                                                
017460 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017470 01   DLI-IO-AREA-B601.                                                   
017480*     03  -COPY WDB601                                                    
017490     EJECT                                                                
017500                                                                          
017600 LINKAGE SECTION.                                                         
017700                                                                          
017800*01  -COPY W0009  -PRE MSG-                                               
017900     EJECT                                                                
018000*01  -COPY W0008  -PRE 6352-                                              
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008  -PRE WDD3-                                              
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE WDL6-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE WDL9-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE WDK7-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019410*01  -COPY W0008  -PRE WDR8-                                              
019420     05  FILLER                  PIC X.                                   
019430     EJECT                                                                
019440*01  -COPY W0008  -PRE WDB6-                                              
019450     05  FILLER                  PIC X.                                   
019460     EJECT                                                                
019470*01  -COPY W0008  -PRE 9305-                                              
019480     05  FILLER                  PIC X.                                   
019490     EJECT                                                                
019491*01  -COPY W0008  -PRE AVG-WDB6-                                          
019492     05  FILLER                  PIC X.                                   
019493     EJECT                                                                
019500 PROCEDURE DIVISION  USING MSG-PCB 6352-PCB WDD3-PCB                      
019600                           WDL6-PCB WDL9-PCB WDK7-PCB                     
019610                           WDR8-PCB WDB6-PCB                              
019620                           9305-PCB AVG-WDB6-PCB.                         
019700 MAIN SECTION.                                                            
019800     ENTRY 'DLITCBL' USING MSG-PCB 6352-PCB WDD3-PCB                      
019900                           WDL6-PCB WDL9-PCB WDK7-PCB                     
019910                           WDR8-PCB WDB6-PCB                              
019920                           9305-PCB AVG-WDB6-PCB.                         
020000                                                                          
020100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
020200     IF SUB-KDRC = 0                                                      
020300       PERFORM A-INIT                                                     
020400       PERFORM B-CHECK-KEYS                                               
020500       IF NYCKLAR-OK                                                      
020600           IF REQU-KDPGMACT = 'E'                                         
020700              PERFORM G-CHECK-INPUT                                       
020800              IF INDATA-OK                                                
020900                 PERFORM H-UPDATE                                         
021000              END-IF                                                      
021100           END-IF                                                         
021200           PERFORM F-READ-SHOW-INFO                                       
021300       END-IF                                                             
021400       PERFORM S02-RETURN-RESPONSE                                        
021500     END-IF                                                               
021600                                                                          
021700     MOVE ZERO TO RETURN-CODE                                             
021800     GOBACK                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 A-INIT SECTION.                                                          
022200                                                                          
022300     ACCEPT DAGENS-DATUM FROM DATE                                        
022400     ACCEPT DAGENS-TID   FROM TIME                                        
022500                                                                          
022600     IF REQU-KVRADER NOT NUMERIC                                          
022700        MOVE ZERO TO REQU-KVRADER                                         
022800     END-IF                                                               
022900                                                                          
023000     IF REQU-IDDC NOT = ALL '+' OR SPACE                                  
023100      MOVE REQU-IDDC TO WS-IDDC                                           
023200                        W-IDDC-HAC                                        
023300     END-IF                                                               
023301                                                                          
023302                                                                          
023303     MOVE ALL '+' TO RESP-AREA                                            
023304     MOVE SPACE   TO RESP-AREA                                            
023305     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
023306                     RESP-IDMSG-INFO                                      
023400                     RESP-IDELMT-ERROR                                    
023500     MOVE REQU-KVRADER    TO RESP-KVRADER                                 
023600     MOVE '001'   TO RESP-IDMSGVER                                        
023700     .                                                                    
023800     EJECT                                                                
023900 B-CHECK-KEYS SECTION.                                                    
024000                                                                          
024100     MOVE JA  TO NYCKLAR-SW                                               
024200                                                                          
024300***  KONTROLL AV REQU-KDPGMACT                                            
024400     IF REQU-KDPGMACT = 'S' OR 'E'                                        
024500        CONTINUE                                                          
024600     ELSE                                                                 
024700        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
024800        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
024900        MOVE NEJ TO NYCKLAR-SW                                            
025000     END-IF                                                               
025100                                                                          
025200***  KONTROLL AV IDARTNR-KEY                                              
025300     IF REQU-IDARTNR-KEY NOT = ALL '+'                                    
025400        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
025500        IF REQU-IDARTNR-KEY NUMERIC                                       
025600           MOVE REQU-IDARTNR-KEY TO W-IDARTNR-6352                        
025700        ELSE                                                              
025800           MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                            
025900           MOVE NEJ       TO NYCKLAR-SW                                   
026000        END-IF                                                            
026100     ELSE                                                                 
026200        MOVE ZERO         TO W-IDARTNR-6352                               
026300     END-IF                                                               
026400                                                                          
026500     IF NYCKLAR-FEL                                                       
026600        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
026700           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
026800        ELSE                                                              
026900           MOVE ERR-NOT-NUM  TO RESP-IDMSG-ERROR                          
027000        END-IF                                                            
027100        MOVE ZERO TO RESP-KVRADER                                         
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500                                                                          
027600 F-READ-SHOW-INFO SECTION.                                                
027700                                                                          
027800     PERFORM IMS-GU-WDGX6351                                              
027900                                                                          
028000     MOVE +1                 TO RADIND                                    
028100     MOVE ZERO               TO RESP-KVRADER                              
028200     IF SEGMENT-FOUND                                                     
028300                                                                          
028400       IF W-IDARTNR-6352 > ZERO                                           
028500          PERFORM IMS-GNP-WDGX6352                                        
028600       ELSE                                                               
028700          PERFORM IMS-GNP-WDGX6352-ALL                                    
028800       END-IF                                                             
028900                                                                          
029000       IF SEGMENT-FOUND                                                   
029100                                                                          
029200          PERFORM UNTIL RADIND > WS-MAX-500-RADER                         
029300                   OR SEGMENT-MISSING                                     
029500             MOVE 6352-DAINLEV   TO RESP-DAINLEV(RADIND)                  
029600             MOVE 6352-KVAVIS    TO RESP-KVAVIS(RADIND)                   
029700             MOVE 6352-IDFAKT    TO RESP-IDFAKT(RADIND)                   
029800             MOVE 6352-IDKUNDNR  TO RESP-IDKUNDNR(RADIND)                 
029900             MOVE 6352-IDORDNR7  TO RESP-IDORDNR7(RADIND)                 
030000             MOVE 6352-IDOKOLLI  TO RESP-IDKOLLI(RADIND)                  
030100             MOVE 6352-IDARTNR   TO RESP-IDARTNR(RADIND)                  
030200                                    W-IDARTNR-D3                          
030300             PERFORM IMS-GU-WDD301                                        
030310                                                                          
030400             IF SEGMENT-FOUND                                             
030500               PERFORM IMS-GNP-WDD311                                     
030600               IF SEGMENT-FOUND                                           
030700                  MOVE TEXT-BEART TO RESP-BEART(RADIND)                   
030800               END-IF                                                     
030900             END-IF                                                       
030910                                                                          
031000             ADD +1 TO RADIND                                             
031100                       RESP-KVRADER                                       
031200                                                                          
031300             IF W-IDARTNR-6352 > ZERO                                     
031400               PERFORM IMS-GNP-WDGX6352                                   
031500             ELSE                                                         
031600               PERFORM IMS-GNP-WDGX6352-ALL                               
031700             END-IF                                                       
031800                                                                          
031900          END-PERFORM                                                     
032000       ELSE                                                               
032100          MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERROR                       
032200          MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                      
032300       END-IF                                                             
032400     ELSE                                                                 
032500        MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERROR                         
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 G-CHECK-INPUT SECTION.                                                   
033000                                                                          
033100     IF REQU-KVRADER NUMERIC                                              
033200     AND REQU-KVRADER > ZERO                                              
033300        MOVE REQU-KVRADER TO MAX-INDX                                     
033301     END-IF                                                               
033302                                                                          
033600     MOVE +1   TO RADIND                                                  
033700     MOVE NEJ  TO INDATA-SW                                               
033710                                                                          
033800     PERFORM UNTIL RADIND > MAX-INDX                                      
033900        IF REQU-KVANTMOT(RADIND)  = ALL '+'                               
033901           CONTINUE                                                       
033902        ELSE                                                              
033903             MOVE JA   TO INDATA-SW                                       
033910             MOVE REQU-KVANTMOT(RADIND) TO                                
033920                               RESP-KVANTMOT(RADIND)                      
033930             INSPECT RESP-KVANTMOT(RADIND) REPLACING                      
033940                               LEADING ZERO BY SPACE                      
034000           IF REQU-KVANTMOT(RADIND) NOT NUMERIC                           
034100             MOVE 'KVANTMOT'  TO RESP-IDELMT-ERROR                        
034200             MOVE ERR-NOT-NUM TO RESP-IDMSG-ERROR                         
034300             MOVE ERR-NOT-NUM TO RESP-IDMSG-ERR-LINE(RADIND)              
034400             MOVE NEJ         TO INDATA-SW                                
034500           END-IF                                                         
034600        END-IF                                                            
034700        IF REQU-IDTRACK(RADIND) = ALL '+'                                 
034800         CONTINUE                                                         
034900        ELSE                                                              
035000         MOVE REQU-IDTRACK(RADIND) TO RESP-IDTRACK(RADIND)                
035001        END-IF                                                            
035002        ADD +1 TO RADIND                                                  
035003     END-PERFORM                                                          
035004                                                                          
035005     IF INDATA-FEL                                                        
035006        IF RESP-IDELMT-ERROR = 'KVANTMOT'                                 
035007           MOVE ERR-NOT-NUM  TO RESP-IDMSG-ERROR                          
035008        ELSE                                                              
035009           MOVE NO-DATA-INPUT TO RESP-IDMSG-ERROR                         
035010        END-IF                                                            
035011                                                                          
035012     ELSE                                                                 
035013       IF REQU-IDUSER-003 = ALL '+'                                       
035014         MOVE 'IDANSTNR'      TO RESP-IDELMT-ERROR                        
035015         MOVE MUST-ENTER-EMP-ID TO RESP-IDMSG-ERROR                       
035016         MOVE NEJ       TO INDATA-SW                                      
035017       END-IF                                                             
035018     END-IF                                                               
035019     .                                                                    
035020 H-UPDATE SECTION.                                                        
035100                                                                          
035200     MOVE +1   TO INDX                                                    
035300     PERFORM UNTIL INDX > MAX-INDX                                        
035500       IF REQU-KVANTMOT(INDX) NOT = ALL '+'                               
035600         MOVE REQU-KVANTMOT(INDX) TO WS-KVANTMOT                          
035700         MOVE REQU-DAINLEV(INDX)  TO W-DAINLEV                            
035800         PERFORM IMS-GHU-WDGX6352                                         
035900                                                                          
036000         IF SEGMENT-FOUND                                                 
036200           IF REQU-KVANTMOT(INDX) > 6352-KVAVIS                           
036300             MOVE 'KVANTMOT'        TO RESP-IDELMT-ERROR                  
036400             MOVE QUANTITY-TOO-BIG  TO RESP-IDMSG-ERROR                   
036500             MOVE QUANTITY-TOO-BIG  TO RESP-IDMSG-ERR-LINE(RADIND)        
036600             MOVE NEJ   TO INDATA-SW                                      
036700           ELSE                                                           
036800             MOVE 6352-IDARTNR         TO W-IDARTNR                       
036900             MOVE 6352-DAINLEV-9KOMPL  TO W-DAINLEV                       
037000             MOVE 6352-IDFAKT          TO WS-IDFAKT                       
037110             MOVE 6352-IDOKOLLI        TO WS-IDKOLLI                      
037200                                                                          
037300             PERFORM IMS-GU-WDL611                                        
037500             IF  SEGMENT-FOUND                                            
037700               MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL             
037800               ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                  
037900               ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                  
037910               COMPUTE WS-DAINLEV  = 9999999999999999                     
037920                                    - WS-TIAAAAMMDDTTMMSSTH               
037930               END-COMPUTE                                                
038100               MOVE WS-DAINLEV      TO INL-DAINLEV                        
038200               MOVE WS-KVANTMOT     TO INL-KVANTMOT                       
038300                                       INL-KVTULRET                       
038400               MOVE DAGENS-DATUM    TO INL-TIINLINL                       
038500               MOVE 'N'             TO INL-FLTULLST                       
038510               MOVE REQU-IDUSER-003 TO INL-IDUSER-003                     
038600                                                                          
038700               PERFORM IMS-ISRT-WDL611                                    
038710               PERFORM UNTIL SEGMENT-FOUND                                
038711                 SUBTRACT +1 FROM WS-DAINLEV                              
038720                 MOVE WS-DAINLEV TO INL-DAINLEV                           
038730                 PERFORM IMS-ISRT-WDL611                                  
038740               END-PERFORM                                                
038800             END-IF                                                       
038900           END-IF                                                         
039000         END-IF                                                           
039100                                                                          
039200         IF INDATA-OK                                                     
039300            IF WS-KVANTMOT > ZERO                                         
039310               MOVE WS-IDDC TO W-IDDC                                     
039400               PERFORM IMS-GHU-WDK711                                     
039401**** CALCULATE AVGCOST                                                    
039402               MOVE WS-KVANTMOT      TO AVG-KVANTMOT                      
039410               PERFORM S10-OMRAKN-MEDELPRIS                               
039420               MOVE AVG-PRAVCOST-NEW TO SLAG-PRAVCOST                     
039500               ADD WS-KVANTMOT TO SLAG-KVLS                               
039600               PERFORM IMS-REPL-WDK711                                    
039601**** KVANTMOT IS THE NUMBER OF PARTS RECIEVED FROM CUSTOM ****            
039602               IF REQU-IDTRACK(INDX) NOT = SPACES AND                     
039603                  REQU-IDTRACK(INDX) NOT = ALL '+'                        
039604                MOVE W-DAINLEV     TO TRCK-DAINLEV                        
039605                MOVE WS-KVANTMOT   TO TRCK-KVANTMOT                       
039606                                      TRCK-KVAVIS                         
039607                                      TRCK-KVTRACK-KVAR                   
039608                MOVE REQU-IDTRACK(INDX) TO TRCK-IDTRACK                   
039609                PERFORM IMS-ISRT-WDK728                                   
039610               END-IF                                                     
039611               PERFORM S98-SAP-TRANS-HAC                                  
039620               PERFORM HA-CREATE-WDL901                                   
039700            END-IF                                                        
039803**** THE DIFFERENCE BETWEEN KVANTMOT RECIEVED PARTS FROM CUSTOM           
039804**** AND KVAVIS PARTS HELD AT CUSTOM, COUNTS AS SCRAPPED PARTS            
039805            IF WS-KVANTMOT < 6352-KVAVIS                                  
039806              PERFORM S99-SAP-TRANS-SCRAP                                 
039807            END-IF                                                        
039810            PERFORM IMS-DLET-6352                                         
039820            MOVE SPACE  TO RESP-KVANTMOT(INDX)                            
039830            MOVE SPACE  TO RESP-IDTRACK(INDX)                             
039900         END-IF                                                           
040100       END-IF                                                             
040200       ADD +1 TO INDX                                                     
040300     END-PERFORM                                                          
040500     EJECT                                                                
040600     .                                                                    
040700 HA-CREATE-WDL901 SECTION.                                                
040800                                                                          
040900     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
041000     MOVE 9                         TO LOGG-IDSEKVNR                      
041100     MOVE WS-IDDC                   TO LOGG-IDDC                          
041200     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
041300     MOVE 'R32'                     TO LOGG-IDSUBTYP                      
041400     MOVE 'W6033000'                TO LOGG-IDPGM                         
041500     MOVE 'W603'                    TO LOGG-IDTRANS                       
041600     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
041700     MOVE SPACE                     TO LOGG-REF                           
041800     MOVE WS-IDFAKT                 TO LOGG-IDFAKT                        
041801     MOVE 6352-IDORDNR7             TO LOGG-IDKUNDRF                      
041802     MOVE 6352-IDKUNDNR             TO LOGG-IDKUNDNR                      
041810     MOVE WS-KVANTMOT               TO LOGG-KVART-SALDO                   
042100     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
042300     MOVE SLAG-KVAKS-SDC            TO LOGG-KVAKS                         
042400     MOVE SLAG-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
042500     MOVE SLAG-KVEFRS               TO LOGG-KVEFRS                        
042600     MOVE SLAG-KVLS                 TO LOGG-KVLS                          
042700     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
042800                                                                          
042900     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-LOGG-AAAAMMDD                 
043000     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
043100                                   - WS-LOGG-AAAAMMDD                     
043200     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
043300     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
043400                                   - WS-TTMMSSTH                          
043500                                                                          
043600     PERFORM IMS-ISRT-WDL901                                              
043700     IF SEGMENT-FOUND-EXISTS                                              
043800       PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                             
043900         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
044000         PERFORM IMS-ISRT-WDL901                                          
044100       END-PERFORM                                                        
044200     END-IF                                                               
044300     .                                                                    
044400     EJECT                                                                
044500                                                                          
044600*    -- DISPATCHER SECTIONS                                               
044700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
044800                                                                          
044900     MOVE 'GETARG'               TO SUB-KDFUNC                            
045000     MOVE 'CARPARTS.LDC.PARTATCUSTOMS'       TO SUB-ADDISPABS             
045100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
045200                                                                          
045300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
045400     IF SUB-KDRC > 0                                                      
045500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
045600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
045700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
045800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
045900     END-IF                                                               
046000     .                                                                    
046100     SKIP3                                                                
046200 S02-RETURN-RESPONSE SECTION.                                             
046300                                                                          
046400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
046500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
046700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
046800     IF SUB-KDRC > 0                                                      
046900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
047000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
047100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
047200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600*                                                                         
047601 S10-OMRAKN-MEDELPRIS    SECTION.                                         
047602     MOVE WS-IDDC           TO AVG-IDDC                                   
047603     MOVE SLAG-PRAVCOST     TO AVG-PRAVCOST-OLD                           
047604     COMPUTE AVG-KVLS-OLD = SLAG-KVLS + SLAG-KVEFRS                       
047605     MOVE INL-PRARTNTO      TO AVG-PRARTNTO                               
047606     MOVE ZERO              TO AVG-KDPSLLOC                               
047607                                                                          
047622**** HÄMTA RÄTT FAKTURAMÅNAD FÖR ATT RÄKNA UT RÄTT AVERAGECOST            
047626     MOVE 6352-DAINLEV(3:2) TO AVG-TIAA                                   
047627     MOVE 6352-DAINLEV(5:2) TO AVG-TIMM                                   
047628****                                                                      
047648     MOVE '011'             TO AVG-KDCALL                                 
047649     MOVE ZERO              TO AVG-PRKURS                                 
047650                               AVG-KDPRODSL                               
047670                               AVG-IDFKNGRP                               
047671                                                                          
047672     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
047673                        AVG-WDB6-PCB                                      
047674     IF AVG-KDSVAR = SPACE OR '4'                                         
047675        CONTINUE                                                          
047676     ELSE                                                                 
047677        MOVE 'MARKUP IS'      TO RESP-IDELMT-ERROR                        
047678        MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERROR                         
047679        MOVE KEYS-ARE-MISSING TO RESP-IDMSG-ERR-LINE(RADIND)              
047680        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
047685     END-IF                                                               
047686     .                                                                    
047687     EJECT                                                                
047688                                                                          
047689 S98-SAP-TRANS-HAC SECTION.                                               
047690     MOVE 'W6033000'                  TO EKO-FIL-IDPGM                    
047691     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKO-FIL-TIREGDAT                 
047692     ACCEPT EKO-FIL-TIKLOCK         FROM TIME                             
047693     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
047694     MOVE WS-SAP-AAAAMMDD             TO EKO-EKH-DAVERDAT                 
047695     ADD +1                           TO W-IDSEKVNR-SAP                   
047696*LK  MOVE WS-IDDC                     TO W-IDDC                           
047697     PERFORM IMS-GU-WDB601                                                
047698     MOVE DCS-KDTRADP                 TO EKO-FIL-IDCPYTXT(1:4)            
047699     MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)            
047700     MOVE W-IDSEKVNR-SAP              TO EKO-FIL-IDSEKVNR                 
047701     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
047702     MOVE '127'                       TO EKO-EKH-KDEKSHT                  
047703     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
047704     MOVE SPACE                       TO EKO-EKH-IDDC-SEND                
047705     MOVE WS-IDDC                     TO EKO-EKH-IDDC-REC                 
047706     MOVE ZERO                        TO EKO-EKH-IDDISTR                  
047707     MOVE ZERO                        TO EKO-EKH-IDKUNDNR                 
047708*******************************                                           
047709     MOVE ZERO TO NOLL-RAKNARE                                            
047710     MOVE WS-IDFAKT                   TO WS-SAP-IDFAKT                    
047711     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
047712     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
047713          FOR LEADING ZERO                                                
047714     ADD +1 TO NOLL-RAKNARE                                               
047715     UNSTRING WS-SAP-X-IDFAKT       INTO EKO-EKH-IDVERGL                  
047716          WITH POINTER NOLL-RAKNARE                                       
047717     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
047718     MOVE INL-PRARTNTO                TO EKO-EKH-PRARTNTO                 
047719     MOVE 6352-PRAVCOST               TO EKO-EKH-PRARTSTD                 
047720     MOVE ZERO                        TO EKO-EKH-KDPRODSL                 
047721                                         EKO-EKH-PRARTSJK                 
047722                                         EKO-EKH-PRHEMTAG                 
047723                                         EKO-EKH-PRINK                    
047724                                         EKO-EKH-PRDIRLON                 
047725                                         EKO-EKH-PRDMTRL                  
047726                                         EKO-EKH-PROVRPAL                 
047727                                         EKO-EKH-SUBEL                    
047728     MOVE W-IDARTNR                   TO EKO-EKH-IDARTNR                  
047729     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
047730     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
047731********** EV ÄNDRING FÖR PRKURS                                          
047732     MOVE 1.00                        TO EKO-EKH-PRKURS                   
047733**********                                                                
047734     MOVE WS-KVANTMOT                 TO EKO-EKH-KVANTAL                  
047735     MOVE '6330'                      TO EKO-EKH-IDTRANS                  
047736     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
047737     MOVE ZERO                        TO EKO-EKH-BEVAT                    
047738                                         EKO-EKH-IDANALYS                 
047739                                         EKO-EKH-IDKONTO                  
047740                                         EKO-EKH-KDANMORS                 
047741                                         EKO-EKH-SUVAT                    
047742                                         EKO-EKH-PRLANDCO                 
047743                                         EKO-EKH-DAAVIDAT                 
047744                                         EKO-EKH-IDAVINR                  
047745                                         EKO-EKH-KDAVVTYP                 
047746                                         EKO-EKH-KDRT                     
047747                                         EKO-EKH-KVANTMOT                 
047748                                         EKO-EKH-KVAVIS                   
047749     MOVE SPACE                      TO  EKO-EKH-KDSORT                   
047750                                         EKO-EKH-IDKST                    
047751     MOVE SPACE                      TO  EKO-EKH-IDLEVNR                  
047752     MOVE DCS-KDTRADP                TO  EKO-EKH-KDTRADP                  
047753     MOVE SPACE                      TO  EKO-EKH-FLDCET                   
047754     MOVE SPACE                      TO  EKO-EKH-IDKUNDRF                 
047755     MOVE SPACE                      TO  EKO-EKH-IDFAKT-EXP               
047756                                                                          
047757     PERFORM IMS-ISRT-WDR8                                                
047758                                                                          
047759     IF SEGMENT-FOUND-EXISTS                                              
047760       PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                             
047761          ADD +1 TO EKO-FIL-IDSEKVNR                                      
047762          PERFORM IMS-ISRT-WDR8                                           
047763       END-PERFORM                                                        
047764     END-IF                                                               
047765     .                                                                    
047766     EJECT                                                                
047767                                                                          
047768 S99-SAP-TRANS-SCRAP SECTION.                                             
047769     MOVE 'W6033000'                  TO EKO-FIL-IDPGM                    
047770     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKO-FIL-TIREGDAT                 
047771     ACCEPT EKO-FIL-TIKLOCK         FROM TIME                             
047772     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
047773     MOVE WS-SAP-AAAAMMDD             TO EKO-EKH-DAVERDAT                 
047774     ADD +1                           TO W-IDSEKVNR-SAP                   
047775     MOVE WS-IDDC                     TO W-IDDC                           
047780     PERFORM IMS-GU-WDB601                                                
047790     MOVE DCS-KDTRADP                 TO EKO-FIL-IDCPYTXT(1:4)            
047800     MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)            
047810     MOVE W-IDSEKVNR-SAP              TO EKO-FIL-IDSEKVNR                 
047820     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
047830     MOVE '128'                       TO EKO-EKH-KDEKSHT                  
047831     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
047832     MOVE SPACE                       TO EKO-EKH-IDDC-SEND                
047833     MOVE WS-IDDC                     TO EKO-EKH-IDDC-REC                 
047834     MOVE ZERO                        TO EKO-EKH-IDDISTR                  
047835     MOVE ZERO                        TO EKO-EKH-IDKUNDNR                 
047836*******************************                                           
047837     MOVE ZERO TO NOLL-RAKNARE                                            
047838     MOVE WS-IDFAKT                   TO WS-SAP-IDFAKT                    
047839     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
047840     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
047841          FOR LEADING ZERO                                                
047842     ADD +1 TO NOLL-RAKNARE                                               
047843     UNSTRING WS-SAP-X-IDFAKT       INTO EKO-EKH-IDVERGL                  
047844          WITH POINTER NOLL-RAKNARE                                       
047845     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
047846     MOVE INL-PRARTNTO                TO EKO-EKH-PRARTNTO                 
047847     MOVE 6352-PRAVCOST               TO EKO-EKH-PRARTSTD                 
047848     MOVE ZERO                        TO EKO-EKH-KDPRODSL                 
047849                                         EKO-EKH-PRARTSJK                 
047850                                         EKO-EKH-PRHEMTAG                 
047851                                         EKO-EKH-PRINK                    
047852                                         EKO-EKH-PRDIRLON                 
047853                                         EKO-EKH-PRDMTRL                  
047854                                         EKO-EKH-PROVRPAL                 
047855                                         EKO-EKH-SUBEL                    
047856     MOVE W-IDARTNR                   TO EKO-EKH-IDARTNR                  
047857     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
047858     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
047859********** EV ÄNDRING FÖR PRKURS                                          
047860     MOVE 1.00                        TO EKO-EKH-PRKURS                   
047861**********                                                                
047862     COMPUTE EKO-EKH-KVANTAL = 6352-KVAVIS - WS-KVANTMOT                  
047864     MOVE '6330'                      TO EKO-EKH-IDTRANS                  
047865     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
047866     MOVE ZERO                        TO EKO-EKH-BEVAT                    
047867                                         EKO-EKH-IDANALYS                 
047868                                         EKO-EKH-IDKONTO                  
047869                                         EKO-EKH-KDANMORS                 
047870                                         EKO-EKH-SUVAT                    
047871                                         EKO-EKH-PRLANDCO                 
047872                                         EKO-EKH-DAAVIDAT                 
047873                                         EKO-EKH-IDAVINR                  
047874                                         EKO-EKH-KDAVVTYP                 
047875                                         EKO-EKH-KDRT                     
047876                                         EKO-EKH-KVANTMOT                 
047877                                         EKO-EKH-KVAVIS                   
047878     MOVE SPACE                      TO  EKO-EKH-KDSORT                   
047879                                         EKO-EKH-IDKST                    
047880     MOVE SPACE                      TO  EKO-EKH-IDLEVNR                  
047881     MOVE DCS-KDTRADP                TO  EKO-EKH-KDTRADP                  
047882     MOVE SPACE                      TO  EKO-EKH-FLDCET                   
047883     MOVE SPACE                      TO  EKO-EKH-IDKUNDRF                 
047884     MOVE SPACE                      TO  EKO-EKH-IDFAKT-EXP               
047885                                                                          
047886     PERFORM IMS-ISRT-WDR8                                                
047887                                                                          
047888     IF SEGMENT-FOUND-EXISTS                                              
047889       PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                             
047890          ADD +1 TO EKO-FIL-IDSEKVNR                                      
047891          PERFORM IMS-ISRT-WDR8                                           
047892       END-PERFORM                                                        
047893     END-IF                                                               
047894     .                                                                    
047895     EJECT                                                                
047896                                                                          
047897***  IMS-SECTION ****                                                     
047898*                                                                         
047900 IMS-GU-WDGX6351 SECTION.                                                 
048000                                                                          
048100     STRING 'WDR501  (WDGXKEY  =' W-WDGX6351-X ')'                        
048200          DELIMITED BY SIZE INTO SSA1                                     
048300     MOVE '  ' TO GOOD-STATUSCODES                                        
048400     CALL CBLTDLI USING GU 6352-PCB DLI-IO-WDGX6352 SSA1                  
048500     MOVE 6352-STATUS-CODE TO STATUS-WS                                   
048600     PERFORM IMS-STATUSCHECK                                              
048700     .                                                                    
048800     SKIP3                                                                
048900 IMS-GNP-WDGX6352 SECTION.                                                
049000                                                                          
049100     STRING 'WDGX6352(IDARTNR  =' W-IDARTNR-R5 ')'                        
049200          DELIMITED BY SIZE INTO SSA1                                     
049300     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
049400     CALL CBLTDLI USING GNP 6352-PCB DLI-IO-WDGX6352 SSA1                 
049500     MOVE 6352-STATUS-CODE TO STATUS-WS                                   
049600     PERFORM IMS-STATUSCHECK                                              
049700     .                                                                    
049800     EJECT                                                                
049900 IMS-GNP-WDGX6352-ALL SECTION.                                            
050000                                                                          
050100     MOVE 'WDGX6352 ' TO SSA1                                             
050200     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
050300     CALL CBLTDLI USING GNP 6352-PCB DLI-IO-WDGX6352 SSA1                 
050400     MOVE 6352-STATUS-CODE TO STATUS-WS                                   
050500     PERFORM IMS-STATUSCHECK                                              
050600     .                                                                    
050700     EJECT                                                                
050800 IMS-GHU-WDGX6352 SECTION.                                                
050900                                                                          
051000     STRING 'WDR501  (WDGXKEY  =' W-WDGX6351-X ')'                        
051100          DELIMITED BY SIZE INTO SSA1                                     
051200     STRING 'WDGX6352(DAINLEV  =' W-DAINLEV-X ')'                         
051300          DELIMITED BY SIZE INTO SSA2                                     
051400     MOVE '  ' TO GOOD-STATUSCODES                                        
051500     CALL CBLTDLI USING GHU  6352-PCB DLI-IO-WDGX6352 SSA1 SSA2           
051600     MOVE 6352-STATUS-CODE TO STATUS-WS                                   
051700     PERFORM IMS-STATUSCHECK                                              
051800     .                                                                    
051900     EJECT                                                                
052000 IMS-DLET-6352   SECTION.                                                 
052100                                                                          
052200     MOVE '  ' TO GOOD-STATUSCODES                                        
052300     CALL CBLTDLI USING DLET 6352-PCB DLI-IO-WDGX6352                     
052400     MOVE 6352-STATUS-CODE TO STATUS-WS                                   
052500     PERFORM IMS-STATUSCHECK                                              
052600     .                                                                    
052700     SKIP3                                                                
052800 IMS-GU-WDD301 SECTION.                                                   
052900                                                                          
053000     STRING 'WDD301  (WDD3BSEQ =' W-WDD3-KEY ')'                          
053100          DELIMITED BY SIZE INTO SSA1                                     
053200     MOVE '  GE'              TO GOOD-STATUSCODES                         
053300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1                    
053400     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
053500     PERFORM IMS-STATUSCHECK                                              
053600     .                                                                    
053700                                                                          
053800                                                                          
053900 IMS-GNP-WDD311 SECTION.                                                  
054000                                                                          
054100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
054200          DELIMITED BY SIZE INTO SSA1                                     
054300     MOVE '  GE'              TO GOOD-STATUSCODES                         
054400     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
054500     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
054600     PERFORM IMS-STATUSCHECK                                              
054700     .                                                                    
054800                                                                          
054900 IMS-GU-WDL611   SECTION.                                                 
054910                                                                          
055000     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
055100          DELIMITED BY SIZE INTO SSA1                                     
055200     STRING 'WDL611  (DAINLEV = ' W-DAINLEV-X ')'                         
055300          DELIMITED BY SIZE INTO SSA2                                     
055400     MOVE SPACE  TO GOOD-STATUSCODES                                      
055500     CALL CBLTDLI USING GU  WDL6-PCB DLI-IO-WDL611 SSA1 SSA2              
055600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
055700     PERFORM IMS-STATUSCHECK                                              
055800     .                                                                    
055900     SKIP3                                                                
056000 IMS-ISRT-WDL611 SECTION.                                                 
056100                                                                          
056200     MOVE 'WDL611 ' TO SSA1                                               
056300     MOVE '  II' TO GOOD-STATUSCODES                                      
056400     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL611 SSA1                  
056500     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
056600     PERFORM IMS-STATUSCHECK                                              
056700     .                                                                    
056800     SKIP3                                                                
056900 IMS-GHU-WDK711 SECTION.                                                  
057000                                                                          
057100     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
057200          DELIMITED BY SIZE INTO SSA1                                     
057300     STRING 'WDK711  (IDDC    = ' W-IDDC ')'                              
057400          DELIMITED BY SIZE INTO SSA2                                     
057500     MOVE SPACE  TO GOOD-STATUSCODES                                      
057600     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
057700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
057800     PERFORM IMS-STATUSCHECK                                              
057900     .                                                                    
058000     SKIP3                                                                
058100 IMS-REPL-WDK711 SECTION.                                                 
058200                                                                          
058300     MOVE '  ' TO GOOD-STATUSCODES                                        
058400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
058500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
058600     PERFORM IMS-STATUSCHECK                                              
058700     .                                                                    
058800     SKIP3                                                                
058900 IMS-ISRT-WDK728 SECTION.                                                 
059000                                                                          
059100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
059200          DELIMITED BY SIZE INTO SSA1                                     
059300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
059400          DELIMITED BY SIZE INTO SSA2                                     
059500     MOVE   'WDK728'          TO SSA3                                     
059600     MOVE '  '                TO GOOD-STATUSCODES                         
059700     CALL CBLTDLI USING ISRT WDK7-PCB DLI-IO-WDK728 SSA1 SSA2 SSA3        
059800     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
059900     PERFORM IMS-STATUSCHECK                                              
059901     .                                                                    
059902 IMS-ISRT-WDL901 SECTION.                                                 
059903                                                                          
059904     MOVE 'WDL901 ' TO SSA1                                               
059905     MOVE '  II' TO GOOD-STATUSCODES                                      
059906     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
059907     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
059908     PERFORM IMS-STATUSCHECK                                              
059909     .                                                                    
059910     SKIP3                                                                
059911                                                                          
059912 IMS-ISRT-WDR8   SECTION.                                                 
059913     MOVE 'WDR801   ' TO SSA1                                             
059914     MOVE '  II' TO GOOD-STATUSCODES                                      
059915     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
059916     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
059917     PERFORM IMS-STATUSCHECK                                              
059918     .                                                                    
059919     EJECT                                                                
059920                                                                          
059921 IMS-GU-WDB601 SECTION.                                                   
059922     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
059923          DELIMITED BY SIZE INTO SSA1                                     
059924     MOVE '  ' TO GOOD-STATUSCODES                                        
059925     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
059926     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
059927     PERFORM IMS-STATUSCHECK                                              
059928     .                                                                    
059929     EJECT                                                                
059930                                                                          
059931 IMS-STATUSCHECK SECTION.                                                 
059932                                                                          
060000     SET STATUS-IX TO 1                                                   
060100     SEARCH GOOD-STATUS                                                   
060200       AT END                                                             
060300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
060400         DELIMITED BY SIZE INTO ERROR-TEXT                                
060500         CALL FELLOG                                                      
060600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
060700         CONTINUE                                                         
060800     END-SEARCH                                                           
060900     .                                                                    
