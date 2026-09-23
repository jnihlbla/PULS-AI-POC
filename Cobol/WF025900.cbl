000100 PROCESS DYNAM                                                            
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF025900.                                                
001400 AUTHOR.         HENRIKSSON ANDERS.                                       
001500 DATE-WRITTEN.   02/03/12.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001710*    NAME                                                                 
001720*        CARPARTS.BILLIT.APPROVEDCURRENCIESLOCATE                         
001800*    FUNCTION:                                                            
001900*        LOCATE APPROVED CURRENCIES                                       
002000*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
002100*                                                                         
002210*        THE PROGRAM READS     TABLE T01LSEL                              
002211*        THE PROGRAM READS     TABLE T01CURR                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSACTION: WF0259T                                             
002600*        REQUEST:     WF0259I1                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        RESPONSE:    WF0259O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003400 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
004000 DATA DIVISION.                                                           
004200 FILE SECTION.                                                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WF025900'.            
004700                                                                          
004800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND. ***           
004900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100                                                                          
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
006000                                                                          
006010 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
006020                                                                          
006100 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006200     88  KEYS-OK                             VALUE 'Y'.                   
006300     88  KEYS-WRONG                          VALUE 'N'.                   
006400     EJECT                                                                
006410                                                                          
006500*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007100     SKIP3                                                                
007110                                                                          
007200*    --- PARAMETRARS TO ABEND                                             
007400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007710 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007800     SKIP3                                                                
007810                                                                          
007900 01  MESSAGE-CODES.                                                       
008100     03  ERROR-CODES.                                                     
008200       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
008201       05 IS-INVALID                 PIC X(3)    VALUE '023'.             
008210       05 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.             
008211       05 NOT-FOUND                  PIC X(3)    VALUE '025'.             
008220       05 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.             
008300       05 LINE-NOT-FOUND             PIC X(3)    VALUE '027'.             
008400       05 MORE-LINE-EXIST            PIC X(3)    VALUE '028'.             
008500       05 SYSTEM-ERROR               PIC X(3)    VALUE '099'.             
008600     EJECT                                                                
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008900     SKIP3                                                                
009000 01  -COPY WZ01SUB                                                        
009100     EJECT                                                                
009110                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009300     SKIP3                                                                
009400 01  REQU-AREA.                                                           
009500*    03  -COPY WZ01REQU                                                   
009600*    03  -COPY WF0259I1                                                   
009700     EJECT                                                                
009710                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009900     SKIP3                                                                
010000 01  RESP-AREA.                                                           
010100*    03  -COPY WZ01RESP                                                   
010200*    03  -COPY WF0259O1                                                   
010300     EJECT                                                                
010401                                                                          
010402 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010403       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010404                                                                          
010405 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
010406 01  DB2-WS.                                                              
010407     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
010408         88  CURSOR-OK                       VALUE 000.                   
010409         88  LINES-FOUND                     VALUE 000.                   
010411         88  LINES-MISSING                   VALUE 100.                   
010412         88  RESCORCE-WRONG                  VALUE 904.                   
010413                                                                          
010419     03  GOOD-SQLCODECODES.                                               
010420         05  GOOD-SQLCODE OCCURS 5                                        
010430             INDEXED BY SQLCODE-IX PIC 9(3).                              
010500     EJECT                                                                
010501                                                                          
010502 01  WS-IX                      PIC S9(9)    VALUE ZERO BINARY.           
010503 01  WS-COUNTER-T01CURR         PIC S9(7)    COMP-3 VALUE ZERO.           
010504                                                                          
010505 01  WS-AREA.                                                             
010506     03 WS-KDSTATUS             PIC S9(3)    COMP-3 VALUE ZERO.           
010540     03 WS-IDLEGSEL             PIC X(4)     VALUE SPACE.                 
010550     03 WS-BELEGRAD-1           PIC X(35)    VALUE SPACE.                 
010551     03 WS-KVRADER              PIC Z(4)9(1) VALUE ZERO.                  
010565     03 WS-KDVALISO-LINE        PIC X(3)     VALUE SPACE.                 
010566     03 WS-DASTADAT-LINE        PIC X(8)     VALUE SPACE.                 
010567     03 WS-PRKURS-LINE          PIC S9(6)V9(5) COMP-3 VALUE ZERO.         
010568     03 WS-PRKURS-LINE-2        PIC Z(5)9.9(5) VALUE ZERO.                
010569     03 WS-REVALUTA-LINE        PIC S9(5)    COMP-3 VALUE ZERO.           
010570     03 WS-REVALUTA-LINE-2      PIC Z(4)9    VALUE ZERO.                  
010580     03 WS-DAREGDAT-LINE        PIC X(8)     VALUE SPACE.                 
010593     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
010594     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
010595     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
010801     EJECT                                                                
010802                                                                          
010803 01  WS-DASTADAT-START          PIC X(8)     VALUE SPACE.                 
010804                                                                          
010805 01  FILLER                    PIC X(16)    VALUE 'T01LSEL-AREA'.         
010810*01  -COPY T01LSEL -PRE T01LSEL-                                          
010901     EJECT                                                                
010902                                                                          
010903 01  FILLER                    PIC X(16)    VALUE 'T01CURR-AREA'.         
010904*01  -COPY T01CURR -PRE T01CURR-                                          
010905     EJECT                                                                
010906                                                                          
010910     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
011000     EJECT                                                                
011020     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
011030     EJECT                                                                
011040                                                                          
011100 LINKAGE SECTION.                                                         
011501 PROCEDURE DIVISION.                                                      
011502 MAIN SECTION.                                                            
011600                                                                          
011800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
011900     IF SUB-KDRC = ZERO                                                   
012000       PERFORM A-INIT                                                     
012100       PERFORM B-CHECK-KEYS                                               
012101       IF KEYS-OK                                                         
012110         PERFORM C-CHECK-INDATA                                           
012120       END-IF                                                             
012200       IF KEYS-OK                                                         
012600         PERFORM D-PERFORM-REQUEST                                        
012700       END-IF                                                             
012710       PERFORM E-READ-SHOW-INFO                                           
012800       PERFORM S02-RETURN-RESPONSE                                        
012900     END-IF                                                               
013200                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013710                                                                          
013800 A-INIT SECTION.                                                          
014305                                                                          
014313     MOVE YES                   TO KEYS-SW                                
014314                                                                          
014316     MOVE ALL '+'               TO RESP-AREA                              
014317     MOVE SPACE TO RESP-IDMSG-ERROR                                       
014318     MOVE SPACE TO RESP-IDMSG-INFO                                        
014319     MOVE SPACE TO RESP-IDELMT-ERROR                                      
014320                                                                          
014321     MOVE ZERO                  TO WS-COUNTER-T01CURR                     
014322                                   RESP-KVRADER                           
014323                                                                          
014330     INITIALIZE GOOD-SQLCODECODES                                         
014600     .                                                                    
014700     EJECT                                                                
014710                                                                          
014800 B-CHECK-KEYS SECTION.                                                    
015200     IF REQU-KDPGMACT = 'S'                                               
015210     AND REQU-IDMSGVER NUMERIC                                            
015300       CONTINUE                                                           
015310     ELSE                                                                 
015350       MOVE NOO TO KEYS-SW                                                
015380     END-IF                                                               
015400                                                                          
015410     IF REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                            
015430       MOVE NOO TO KEYS-SW                                                
015440     END-IF                                                               
015450                                                                          
015460     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
015490       MOVE NOO TO KEYS-SW                                                
015500     END-IF                                                               
015600                                                                          
015700     IF KEYS-WRONG                                                        
015710       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015720       IF REQU-KDPGMACT = 'S'                                             
015730         CONTINUE                                                         
015740       ELSE                                                               
015750         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015760         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
015770       END-IF                                                             
015780       IF REQU-IDMSGVER NUMERIC                                           
015790         CONTINUE                                                         
015791       ELSE                                                               
015792         MOVE SYSTEM-ERROR    TO WS-IDMSG-ERROR                           
015793         MOVE 'IDMSGVER'      TO WS-IDELMT-ERROR                          
015794       END-IF                                                             
015795       IF REQU-IDUSER = SPACE OR = ALL '+'                                
015796         MOVE SYSTEM-ERROR    TO WS-IDMSG-ERROR                           
015797         MOVE 'IDUSER'        TO WS-IDELMT-ERROR                          
015798       ELSE                                                               
015799         CONTINUE                                                         
015800       END-IF                                                             
015801     END-IF                                                               
015810     .                                                                    
015900     EJECT                                                                
016000                                                                          
016100 C-CHECK-INDATA SECTION.                                                  
016200     IF REQU-KDPGMACT = 'S'                                               
016212       PERFORM DB2-SELECT-T01LSEL                                         
016213       IF LINES-FOUND                                                     
016214         CONTINUE                                                         
016216       ELSE                                                               
016217         MOVE NOT-FOUND  TO WS-IDMSG-ERROR                                
016218         MOVE 'IDLEGSEL' TO WS-IDELMT-ERROR                               
016219         MOVE NOO TO KEYS-SW                                              
016222       END-IF                                                             
016223     END-IF                                                               
016224     IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                            
016227       PERFORM DB2-OPEN-T01CURR-CRS                                       
016228       PERFORM DB2-FETCH-T01CURR-CRS                                      
016229       IF LINES-FOUND                                                     
016230         MOVE WS-DASTADAT-LINE TO WS-DASTADAT-START                       
016231       ELSE                                                               
016232         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
016235         MOVE NOO TO KEYS-SW                                              
016236       END-IF                                                             
016237       PERFORM DB2-CLOSE-T01CURR-CRS                                      
016238     ELSE                                                                 
016239       PERFORM DB2-OPEN-T01CURR-CRS-2                                     
016240       PERFORM DB2-FETCH-T01CURR-CRS-2                                    
016241       IF LINES-FOUND                                                     
016242         CONTINUE                                                         
016243       ELSE                                                               
016244         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
016247         MOVE NOO TO KEYS-SW                                              
016248       END-IF                                                             
016249       PERFORM DB2-CLOSE-T01CURR-CRS-2                                    
016250     END-IF                                                               
016251     .                                                                    
016252     EJECT                                                                
016253                                                                          
016260 D-PERFORM-REQUEST SECTION.                                               
016300     IF REQU-KDPGMACT = 'S'                                               
016301       IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                          
016302         PERFORM DB2-COUNT-CRS-1                                          
016303       ELSE                                                               
016304         PERFORM DB2-COUNT-CRS-2                                          
016305       END-IF                                                             
016307       IF WS-COUNTER-T01CURR NOT = ZERO                                   
016308         IF WS-COUNTER-T01CURR > WS-MAX-LINES                             
016309            MOVE MORE-LINE-EXIST TO WS-IDMSG-ERROR                        
016310            MOVE NOO TO KEYS-SW                                           
016311         END-IF                                                           
016312       ELSE                                                               
016313         MOVE NOO TO KEYS-SW                                              
016314         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
016315       END-IF                                                             
016316       IF KEYS-OK                                                         
016317         IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                        
016318           PERFORM DB2-OPEN-T01CURR-CRS-3                                 
016319           PERFORM DB2-FETCH-T01CURR-CRS-3                                
016320           MOVE ZERO TO WS-IX                                             
016321           PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES            
016322             PERFORM F-READ-SHOW-INFO-TABELL                              
016323             PERFORM DB2-FETCH-T01CURR-CRS-3                              
016324           END-PERFORM                                                    
016325           PERFORM DB2-CLOSE-T01CURR-CRS-3                                
016326         ELSE                                                             
016327           PERFORM DB2-OPEN-T01CURR-CRS-2                                 
016328           PERFORM DB2-FETCH-T01CURR-CRS-2                                
016329           MOVE ZERO TO WS-IX                                             
016330           PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES            
016331             PERFORM F-READ-SHOW-INFO-TABELL                              
016332             PERFORM DB2-FETCH-T01CURR-CRS-2                              
016333           END-PERFORM                                                    
016334           PERFORM DB2-CLOSE-T01CURR-CRS-2                                
016335         END-IF                                                           
016336       END-IF                                                             
016340     END-IF                                                               
017100     .                                                                    
017200     EJECT                                                                
017300                                                                          
017400 E-READ-SHOW-INFO SECTION.                                                
017500     IF REQU-KDPGMACT = 'S'                                               
017600       MOVE REQU-IDMSGVER            TO RESP-IDMSGVER                     
017700       MOVE WS-IDMSG-INFO            TO RESP-IDMSG-INFO                   
017800       MOVE WS-IDMSG-ERROR           TO RESP-IDMSG-ERROR                  
017810       MOVE WS-IDELMT-ERROR          TO RESP-IDELMT-ERROR                 
017900       MOVE REQU-IDLEGSEL-KEY        TO RESP-IDLEGSEL-KEY                 
017910       MOVE REQU-KDVALISO-KEY        TO RESP-KDVALISO-KEY                 
018000       MOVE WS-BELEGRAD-1            TO RESP-BELEGRAD-1                   
018100       MOVE WS-IX                    TO RESP-KVRADER                      
018500     END-IF                                                               
018693     .                                                                    
018694     EJECT                                                                
018695                                                                          
018696 F-READ-SHOW-INFO-TABELL SECTION.                                         
018697     IF REQU-KDPGMACT = 'S'                                               
018698       ADD 1 TO WS-IX                                                     
018699       MOVE WS-KDVALISO-LINE         TO RESP-KDVALISO-LINE(WS-IX)         
018700       MOVE WS-DASTADAT-LINE         TO RESP-DASTADAT-LINE(WS-IX)         
018701       MOVE WS-PRKURS-LINE           TO WS-PRKURS-LINE-2                  
018702       MOVE WS-PRKURS-LINE-2         TO RESP-PRKURS-LINE(WS-IX)           
018703       MOVE WS-REVALUTA-LINE         TO WS-REVALUTA-LINE-2                
018704       MOVE WS-REVALUTA-LINE-2       TO RESP-REVALUTA-LINE(WS-IX)         
018705       MOVE WS-DAREGDAT-LINE         TO RESP-DAREGDAT-LINE(WS-IX)         
018706     END-IF                                                               
018707     .                                                                    
018710     EJECT                                                                
018800                                                                          
019100*    --- DISPATCHER-SECTION START                                         
019200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019400     MOVE 'GETARG'                               TO SUB-KDFUNC            
019500     MOVE 'CARPARTS.BILLIT.APPROVEDCURRENCIESLOCATE'                      
019510       TO SUB-ADDISPABS                                                   
019600     MOVE LENGTH OF REQU-AREA                    TO SUB-KVDLEN            
019700                                                                          
019800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
019900                                                                          
020000     IF SUB-KDRC > 0                                                      
020100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
020300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020500     END-IF                                                               
020600     .                                                                    
020700     SKIP3                                                                
020710                                                                          
020800 S02-RETURN-RESPONSE SECTION.                                             
021000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021110                                                                          
021120     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
021130                              - ((WS-MAX-LINES - WS-IX)                   
021140                              * LENGTH OF RESP-TABELLRAD)                 
021200                                                                          
021300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021400                                                                          
021500     IF SUB-KDRC > 0                                                      
021600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022000     END-IF                                                               
022100     .                                                                    
022200     EJECT                                                                
022802                                                                          
022803 DB2-SELECT-T01LSEL SECTION.                                              
022804     MOVE 000100  TO GOOD-SQLCODECODES                                    
022805     EXEC SQL                                                             
022806         SELECT  IDLEGSEL, KDSTATUS, BELEGRAD_1                           
022807                                                                          
022808         INTO :WS-IDLEGSEL, :WS-KDSTATUS, :WS-BELEGRAD-1                  
022809                                                                          
022811         FROM    T01LSEL                                                  
022812                                                                          
022814         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
022817         AND     KDSTATUS = 001                                           
022818     END-EXEC                                                             
022819     MOVE SQLCODE TO SQLCODE-WS                                           
022821     PERFORM DB2-STATUS-CHECK                                             
022822     .                                                                    
022823     EJECT                                                                
022824                                                                          
022859 DB2-COUNT-CRS-1 SECTION.                                                 
022860     MOVE 000100  TO GOOD-SQLCODECODES                                    
022861     EXEC SQL                                                             
022862                                                                          
022863           SELECT COUNT(*)                                                
022864                                                                          
022865           INTO  :WS-COUNTER-T01CURR                                      
022866                                                                          
022867           FROM   T01CURR                                                 
022868                                                                          
022869           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022870           AND      DASTADAT = :WS-DASTADAT-START                         
022871                                                                          
022872     END-EXEC                                                             
022873                                                                          
022874     MOVE SQLCODE TO SQLCODE-WS                                           
022875     PERFORM DB2-STATUS-CHECK                                             
022876     .                                                                    
022877     EJECT                                                                
022878                                                                          
022879 DB2-COUNT-CRS-2 SECTION.                                                 
022880     MOVE 000100  TO GOOD-SQLCODECODES                                    
022881     EXEC SQL                                                             
022882                                                                          
022883           SELECT COUNT(*)                                                
022884                                                                          
022885           INTO  :WS-COUNTER-T01CURR                                      
022886                                                                          
022887           FROM   T01CURR                                                 
022888                                                                          
022889           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022890           AND      KDVALISO = :REQU-KDVALISO-KEY                         
022891                                                                          
022892     END-EXEC                                                             
022893     MOVE SQLCODE TO SQLCODE-WS                                           
022894     PERFORM DB2-STATUS-CHECK                                             
022895     .                                                                    
022896     EJECT                                                                
022897                                                                          
022898 DB2-OPEN-T01CURR-CRS SECTION.                                            
022899     MOVE 000100 TO GOOD-SQLCODECODES                                     
022900     EXEC SQL                                                             
022901         DECLARE T01CURR-CRS CURSOR WITH HOLD FOR                         
022902                                                                          
022903           SELECT  IDLEGSEL, KDVALISO,                                    
022904                   DASTADAT, REVALUTA, PRKURS,                            
022905                   DAREGDAT                                               
022906                                                                          
022907           FROM    T01CURR                                                
022908                                                                          
022909           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022910                                                                          
022911           ORDER BY IDLEGSEL                                              
022912                  , KDVALISO                                              
022913                  , DASTADAT DESC                                         
022914     END-EXEC                                                             
022915     MOVE 000100  TO GOOD-SQLCODECODES                                    
022916                                                                          
022917     EXEC SQL                                                             
022918        OPEN T01CURR-CRS                                                  
022919     END-EXEC                                                             
022920     MOVE SQLCODE TO SQLCODE-WS                                           
022921     PERFORM DB2-STATUS-CHECK                                             
022922     .                                                                    
022923     EJECT                                                                
022924                                                                          
022925 DB2-FETCH-T01CURR-CRS SECTION.                                           
022926     MOVE 000100  TO GOOD-SQLCODECODES                                    
022927     EXEC SQL                                                             
022928                                                                          
022929         FETCH T01CURR-CRS                                                
022930                                                                          
022931         INTO :WS-IDLEGSEL,                                               
022932              :WS-KDVALISO-LINE,                                          
022933              :WS-DASTADAT-LINE,                                          
022934              :WS-REVALUTA-LINE,                                          
022935              :WS-PRKURS-LINE,                                            
022936              :WS-DAREGDAT-LINE                                           
022937     END-EXEC                                                             
022938     MOVE SQLCODE TO SQLCODE-WS                                           
022939     PERFORM DB2-STATUS-CHECK                                             
022940     .                                                                    
022941     EJECT                                                                
022942                                                                          
022943 DB2-CLOSE-T01CURR-CRS SECTION.                                           
022944                                                                          
022945     EXEC SQL                                                             
022946        CLOSE T01CURR-CRS                                                 
022947     END-EXEC                                                             
022948     .                                                                    
022949     EJECT                                                                
022950                                                                          
022951 DB2-OPEN-T01CURR-CRS-2 SECTION.                                          
022952     MOVE 000100 TO GOOD-SQLCODECODES                                     
022953     EXEC SQL                                                             
022954         DECLARE T01CURR-CRS-2 CURSOR WITH HOLD FOR                       
022955                                                                          
022956           SELECT  IDLEGSEL, KDVALISO,                                    
022957                   DASTADAT, REVALUTA, PRKURS,                            
022958                   DAREGDAT                                               
022959                                                                          
022960           FROM    T01CURR                                                
022961                                                                          
022962           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022963           AND      KDVALISO = :REQU-KDVALISO-KEY                         
022964                                                                          
022965           ORDER BY IDLEGSEL                                              
022967                  , KDVALISO                                              
022968                  , DASTADAT DESC                                         
022969     END-EXEC                                                             
022970     MOVE 000100  TO GOOD-SQLCODECODES                                    
022971                                                                          
022972     EXEC SQL                                                             
022973        OPEN T01CURR-CRS-2                                                
022974     END-EXEC                                                             
022975     MOVE SQLCODE TO SQLCODE-WS                                           
022976     PERFORM DB2-STATUS-CHECK                                             
022977     .                                                                    
022978     EJECT                                                                
022979                                                                          
022980 DB2-FETCH-T01CURR-CRS-2 SECTION.                                         
022981     MOVE 000100  TO GOOD-SQLCODECODES                                    
022982     EXEC SQL                                                             
022983                                                                          
022984         FETCH T01CURR-CRS-2                                              
022985                                                                          
022986         INTO :WS-IDLEGSEL,                                               
022987              :WS-KDVALISO-LINE,                                          
022988              :WS-DASTADAT-LINE,                                          
022989              :WS-REVALUTA-LINE,                                          
022990              :WS-PRKURS-LINE,                                            
022991              :WS-DAREGDAT-LINE                                           
022992     END-EXEC                                                             
022993     MOVE SQLCODE TO SQLCODE-WS                                           
022994     PERFORM DB2-STATUS-CHECK                                             
022995     .                                                                    
022996     EJECT                                                                
022997                                                                          
022998 DB2-CLOSE-T01CURR-CRS-2 SECTION.                                         
022999                                                                          
023000     EXEC SQL                                                             
023001        CLOSE T01CURR-CRS-2                                               
023002     END-EXEC                                                             
023003     .                                                                    
023004     EJECT                                                                
023005                                                                          
023006 DB2-OPEN-T01CURR-CRS-3 SECTION.                                          
023007     MOVE 000100 TO GOOD-SQLCODECODES                                     
023008     EXEC SQL                                                             
023009         DECLARE T01CURR-CRS-3 CURSOR WITH HOLD FOR                       
023010                                                                          
023011           SELECT  IDLEGSEL, KDVALISO,                                    
023012                   DASTADAT, REVALUTA, PRKURS,                            
023013                   DAREGDAT                                               
023014                                                                          
023015           FROM    T01CURR                                                
023016                                                                          
023017           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
023018           AND      DASTADAT = :WS-DASTADAT-START                         
023019                                                                          
023020           ORDER BY IDLEGSEL                                              
023021                  , KDVALISO                                              
023022                  , DASTADAT DESC                                         
023023     END-EXEC                                                             
023024     MOVE 000100  TO GOOD-SQLCODECODES                                    
023025                                                                          
023026     EXEC SQL                                                             
023027        OPEN T01CURR-CRS-3                                                
023028     END-EXEC                                                             
023029     MOVE SQLCODE TO SQLCODE-WS                                           
023030     PERFORM DB2-STATUS-CHECK                                             
023031     .                                                                    
023032     EJECT                                                                
023033                                                                          
023034 DB2-FETCH-T01CURR-CRS-3 SECTION.                                         
023035     MOVE 000100  TO GOOD-SQLCODECODES                                    
023036     EXEC SQL                                                             
023037                                                                          
023038         FETCH T01CURR-CRS-3                                              
023039                                                                          
023040         INTO :WS-IDLEGSEL,                                               
023041              :WS-KDVALISO-LINE,                                          
023042              :WS-DASTADAT-LINE,                                          
023043              :WS-REVALUTA-LINE,                                          
023044              :WS-PRKURS-LINE,                                            
023045              :WS-DAREGDAT-LINE                                           
023046     END-EXEC                                                             
023047     MOVE SQLCODE TO SQLCODE-WS                                           
023048     PERFORM DB2-STATUS-CHECK                                             
023049     .                                                                    
023050     EJECT                                                                
023051                                                                          
023052 DB2-CLOSE-T01CURR-CRS-3 SECTION.                                         
023053                                                                          
023054     EXEC SQL                                                             
023055        CLOSE T01CURR-CRS-3                                               
023056     END-EXEC                                                             
023057     .                                                                    
023058     EJECT                                                                
023059                                                                          
023060 DB2-STATUS-CHECK     SECTION.                                            
023061     SET SQLCODE-IX TO 1                                                  
023062     SEARCH GOOD-SQLCODE                                                  
023063       AT END                                                             
023064          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
023065          DELIMITED BY SIZE INTO ERROR-TEXT                               
023066          CALL ABEND USING RKOD-ABEND-DB2                                 
023067       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
023070     END-SEARCH                                                           
023100     .                                                                    
