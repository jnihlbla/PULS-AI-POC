000100 PROCESS DYNAM                                                            
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF026000.                                                
001400 AUTHOR.         HENRIKSSON ANDERS.                                       
001500 DATE-WRITTEN.   02/03/12.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001710*    NAME                                                                 
001720*        CARPARTS.BILLIT.APPROVEDVATLOCATE                                
001800*    FUNCTION:                                                            
001900*        LOCATE APPROVED V.A.T                                            
002000*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
002100*                                                                         
002210*        THE PROGRAM READS     TABLE T01LSEL                              
002220*        THE PROGRAM READS     TABLE T01VAT                               
002230*        THE PROGRAM READS     TABLE T01COCO                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: WF0260T                                             
002600*        REQUEST:     WF0260I1                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        RESPONSE:    WF0260O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003400 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
004000 DATA DIVISION.                                                           
004200 FILE SECTION.                                                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WF026000'.            
004700                                                                          
004800*    --- WORKFIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.                
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
007200*    --- PARAMETERS TO ABEND                                              
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
009600*    03  -COPY WF0260I1                                                   
009700     EJECT                                                                
009710                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009900     SKIP3                                                                
010000 01  RESP-AREA.                                                           
010100*    03  -COPY WZ01RESP                                                   
010200*    03  -COPY WF0260O1                                                   
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
010412         88  RESOURCE-WRONG                  VALUE 904.                   
010413                                                                          
010419     03  GOOD-SQLCODECODES.                                               
010420         05  GOOD-SQLCODE OCCURS 5                                        
010430             INDEXED BY SQLCODE-IX PIC 9(3).                              
010500     EJECT                                                                
010501                                                                          
010502 01  WS-IX                      PIC S9(9)    VALUE ZERO BINARY.           
010503 01  WS-COUNTER-T01VAT          PIC S9(7)    COMP-3 VALUE ZERO.           
010504                                                                          
010505 01  WS-AREA.                                                             
010506     03 WS-KDSTATUS             PIC S9(3)    COMP-3 VALUE ZERO.           
010540     03 WS-IDLEGSEL             PIC X(4)     VALUE SPACE.                 
010550     03 WS-BELEGRAD-1           PIC X(35)    VALUE SPACE.                 
010551     03 WS-KVRADER              PIC Z(4)9(1) VALUE ZERO.                  
010560     03 WS-IDLANDX2-LINE        PIC X(2)     VALUE SPACE.                 
010562     03 WS-KDVAT-LINE           PIC X(2)     VALUE SPACE.                 
010563     03 WS-REVAT-LINE           PIC S9(3)V9(2) COMP-3 VALUE ZERO.         
010564     03 WS-REVAT-LINE-2         PIC Z(2)9.9(2) VALUE ZERO.                
010570     03 WS-DAREGDAT-LINE        PIC X(8)     VALUE SPACE.                 
010590     03 WS-DAUPPDAT-LINE        PIC X(8)     VALUE SPACE.                 
010591     03 WS-DADELDAT-LINE        PIC X(8)     VALUE SPACE.                 
010592     03 WS-BEVAT-LINE           PIC X(50)    VALUE SPACE.                 
010593     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
010594     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
010595     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
010596                                                                          
010597     03 COCO-BELAND          PIC X(16)    VALUE SPACE.                    
010801     EJECT                                                                
010802                                                                          
010803 01  FILLER                    PIC X(16)    VALUE 'T01LSEL-AREA'.         
010810*01  -COPY T01LSEL -PRE T01LSEL-                                          
010901     EJECT                                                                
010902                                                                          
010903 01  FILLER                    PIC X(16)    VALUE 'T01VAT-AREA'.          
010904*01  -COPY T01VAT -PRE T01VAT-                                            
010905     EJECT                                                                
010906                                                                          
010907 01  FILLER                    PIC X(16)   VALUE 'T01COCO-AREA'.          
010909*01  -COPY T01COCO -PRE T01COCO-                                          
010910     EJECT                                                                
010911                                                                          
010920     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
011000     EJECT                                                                
011010     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
011011     EJECT                                                                
011020     EXEC SQL INCLUDE T01VAT END-EXEC.                                    
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
014313     MOVE YES                   TO KEYS-SW                                
014314                                                                          
014316     MOVE ALL '+'               TO RESP-AREA                              
014317     MOVE SPACE TO RESP-IDMSG-ERROR                                       
014318     MOVE SPACE TO RESP-IDMSG-INFO                                        
014319     MOVE SPACE TO RESP-IDELMT-ERROR                                      
014320                                                                          
014321     MOVE ZERO                  TO WS-COUNTER-T01VAT                      
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
015360       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015380     END-IF                                                               
015400                                                                          
015410     IF REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                            
015411       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015430       MOVE NOO TO KEYS-SW                                                
015440     END-IF                                                               
015450                                                                          
015460     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
015461       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
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
015792         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015793         MOVE 'IDMSGVER'   TO WS-IDELMT-ERROR                             
015794       END-IF                                                             
015795       IF REQU-IDUSER = SPACE OR = ALL '+'                                
015796         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015797         MOVE 'IDUSER'     TO WS-IDELMT-ERROR                             
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
016224     IF REQU-IDLANDX2-KEY = SPACE OR = ALL '+'                            
016226       PERFORM DB2-OPEN-T01VAT-CRS                                        
016227       PERFORM DB2-FETCH-T01VAT-CRS                                       
016228       IF LINES-FOUND                                                     
016229         CONTINUE                                                         
016230       ELSE                                                               
016231         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
016234         MOVE NOO TO KEYS-SW                                              
016235       END-IF                                                             
016236       PERFORM DB2-CLOSE-T01VAT-CRS                                       
016237     ELSE                                                                 
016238       PERFORM DB2-SELECT-T01COCO-TAB                                     
016239       IF LINES-FOUND OR REQU-IDLANDX2-KEY = 'EU'                         
016241         PERFORM DB2-OPEN-T01VAT-CRS-2                                    
016242         PERFORM DB2-FETCH-T01VAT-CRS-2                                   
016243         IF LINES-FOUND                                                   
016244           CONTINUE                                                       
016245         ELSE                                                             
016246           MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                          
016247           MOVE NOO TO KEYS-SW                                            
016248         END-IF                                                           
016249         PERFORM DB2-CLOSE-T01VAT-CRS-2                                   
016250       ELSE                                                               
016251         MOVE NOT-FOUND   TO WS-IDMSG-ERROR                               
016252         MOVE 'IDLANDX2'  TO WS-IDELMT-ERROR                              
016253         MOVE NOO TO KEYS-SW                                              
016254       END-IF                                                             
016255     END-IF                                                               
016256     .                                                                    
016257     EJECT                                                                
016258                                                                          
016260 D-PERFORM-REQUEST SECTION.                                               
016300     IF REQU-KDPGMACT = 'S'                                               
016301       IF REQU-IDLANDX2-KEY = SPACE OR = ALL '+'                          
016302         PERFORM DB2-COUNT-CRS-1                                          
016303       ELSE                                                               
016304         PERFORM DB2-COUNT-CRS-2                                          
016305       END-IF                                                             
016306                                                                          
016307       IF WS-COUNTER-T01VAT NOT = ZERO                                    
016308         IF WS-COUNTER-T01VAT > WS-MAX-LINES                              
016309            MOVE MORE-LINE-EXIST TO WS-IDMSG-ERROR                        
016310            MOVE NOO TO KEYS-SW                                           
016311         END-IF                                                           
016312       ELSE                                                               
016313         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
016314         MOVE NOO TO KEYS-SW                                              
016315       END-IF                                                             
016316       IF KEYS-OK                                                         
016317         IF REQU-IDLANDX2-KEY = SPACE OR = ALL '+'                        
016318           PERFORM DB2-OPEN-T01VAT-CRS                                    
016319           PERFORM DB2-FETCH-T01VAT-CRS                                   
016320           MOVE ZERO TO WS-IX                                             
016321           PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES            
016322             PERFORM F-READ-SHOW-INFO-TABLE                               
016323             PERFORM DB2-FETCH-T01VAT-CRS                                 
016324           END-PERFORM                                                    
016325           PERFORM DB2-CLOSE-T01VAT-CRS                                   
016326         ELSE                                                             
016327           PERFORM DB2-OPEN-T01VAT-CRS-2                                  
016328           PERFORM DB2-FETCH-T01VAT-CRS-2                                 
016329           MOVE ZERO TO WS-IX                                             
016330           PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES            
016331             PERFORM F-READ-SHOW-INFO-TABLE                               
016332             PERFORM DB2-FETCH-T01VAT-CRS-2                               
016333           END-PERFORM                                                    
016334           PERFORM DB2-CLOSE-T01VAT-CRS-2                                 
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
017910       MOVE REQU-IDLANDX2-KEY        TO RESP-IDLANDX2-KEY                 
018000       MOVE WS-BELEGRAD-1            TO RESP-BELEGRAD-1                   
018100       MOVE WS-IX                    TO RESP-KVRADER                      
018500     END-IF                                                               
018693     .                                                                    
018694     EJECT                                                                
018695                                                                          
018696 F-READ-SHOW-INFO-TABLE SECTION.                                          
018697     IF REQU-KDPGMACT = 'S'                                               
018698       ADD 1 TO WS-IX                                                     
018699       MOVE WS-IDLANDX2-LINE         TO RESP-IDLANDX2-LINE(WS-IX)         
018700       MOVE WS-KDVAT-LINE            TO RESP-KDVAT-LINE(WS-IX)            
018701       MOVE WS-REVAT-LINE            TO WS-REVAT-LINE-2                   
018702       MOVE WS-REVAT-LINE-2          TO RESP-REVAT-LINE(WS-IX)            
018703       MOVE WS-DAREGDAT-LINE         TO RESP-DAREGDAT-LINE(WS-IX)         
018704       MOVE WS-DAUPPDAT-LINE         TO RESP-DAUPPDAT-LINE(WS-IX)         
018705       MOVE WS-DADELDAT-LINE         TO RESP-DADELDAT-LINE(WS-IX)         
018706       MOVE WS-BEVAT-LINE            TO RESP-BEVAT-LINE(WS-IX)            
018707     END-IF                                                               
018708     .                                                                    
018710     EJECT                                                                
018800                                                                          
019100*    --- DISPATCHER-SECTIONS                                              
019200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019400     MOVE 'GETARG'                               TO SUB-KDFUNC            
019500     MOVE 'CARPARTS.BILLIT.APPROVEDVATLOCATE'    TO SUB-ADDISPABS         
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
022813         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
022817         AND     KDSTATUS = 001                                           
022818     END-EXEC                                                             
022819     MOVE SQLCODE TO SQLCODE-WS                                           
022821     PERFORM DB2-STATUS-CHECK                                             
022822     .                                                                    
022823     EJECT                                                                
022824                                                                          
022825*** - CHECK THAT REQUESTED COUNTRY EXISTS                                 
022826 DB2-SELECT-T01COCO-TAB SECTION.                                          
022827                                                                          
022828     MOVE 000100 TO GOOD-SQLCODECODES                                     
022829                                                                          
022830     EXEC SQL                                                             
022831           SELECT  BELAND                                                 
022832                                                                          
022833           INTO   :COCO-BELAND                                            
022834                                                                          
022835           FROM    T01COCO                                                
022836                                                                          
022837           WHERE   IDLANDX3 = :REQU-IDLANDX2-KEY                          
022838     END-EXEC                                                             
022839                                                                          
022840     MOVE SQLCODE TO SQLCODE-WS                                           
022841     PERFORM DB2-STATUS-CHECK                                             
022842     .                                                                    
022843     EJECT                                                                
022850                                                                          
022859 DB2-COUNT-CRS-1 SECTION.                                                 
022860     MOVE 000100  TO GOOD-SQLCODECODES                                    
022861     EXEC SQL                                                             
022862                                                                          
022863           SELECT COUNT(*)                                                
022864                                                                          
022865           INTO  :WS-COUNTER-T01VAT                                       
022866                                                                          
022867           FROM   T01VAT                                                  
022868                                                                          
022869           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022870                                                                          
022871     END-EXEC                                                             
022872                                                                          
022873     MOVE SQLCODE TO SQLCODE-WS                                           
022874     PERFORM DB2-STATUS-CHECK                                             
022875     .                                                                    
022876     EJECT                                                                
022877                                                                          
022878 DB2-COUNT-CRS-2 SECTION.                                                 
022879     MOVE 000100  TO GOOD-SQLCODECODES                                    
022880     EXEC SQL                                                             
022881                                                                          
022882           SELECT COUNT(*)                                                
022883                                                                          
022884           INTO  :WS-COUNTER-T01VAT                                       
022885                                                                          
022886           FROM   T01VAT                                                  
022887                                                                          
022888           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022889           AND      IDLANDX2 = :REQU-IDLANDX2-KEY                         
022890                                                                          
022891     END-EXEC                                                             
022892     MOVE SQLCODE TO SQLCODE-WS                                           
022893     PERFORM DB2-STATUS-CHECK                                             
022894     .                                                                    
022895     EJECT                                                                
022896                                                                          
022897 DB2-OPEN-T01VAT-CRS SECTION.                                             
022898     MOVE 000100 TO GOOD-SQLCODECODES                                     
022899     EXEC SQL                                                             
022900         DECLARE T01VAT-CRS CURSOR WITH HOLD FOR                          
022901                                                                          
022902           SELECT  IDLEGSEL, IDLANDX2,                                    
022903                   KDVAT, REVAT, DAREGDAT,                                
022904                   DAUPPDAT, DADELDAT, BEVAT                              
022905                                                                          
022906           FROM    T01VAT                                                 
022907                                                                          
022908           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022909                                                                          
022910           ORDER BY IDLEGSEL                                              
022911                  , IDLANDX2                                              
022912                  , KDVAT                                                 
022913     END-EXEC                                                             
022914     MOVE 000100  TO GOOD-SQLCODECODES                                    
022915                                                                          
022916     EXEC SQL                                                             
022917        OPEN T01VAT-CRS                                                   
022918     END-EXEC                                                             
022919     MOVE SQLCODE TO SQLCODE-WS                                           
022920     PERFORM DB2-STATUS-CHECK                                             
022921     .                                                                    
022922     EJECT                                                                
022923                                                                          
022924 DB2-FETCH-T01VAT-CRS SECTION.                                            
022925     MOVE 000100  TO GOOD-SQLCODECODES                                    
022926     EXEC SQL                                                             
022927                                                                          
022928         FETCH T01VAT-CRS                                                 
022929                                                                          
022930         INTO :WS-IDLEGSEL,                                               
022931              :WS-IDLANDX2-LINE,                                          
022932              :WS-KDVAT-LINE,                                             
022933              :WS-REVAT-LINE,                                             
022934              :WS-DAREGDAT-LINE,                                          
022935              :WS-DAUPPDAT-LINE,                                          
022936              :WS-DADELDAT-LINE,                                          
022937              :WS-BEVAT-LINE                                              
022938     END-EXEC                                                             
022939     MOVE SQLCODE TO SQLCODE-WS                                           
022940     PERFORM DB2-STATUS-CHECK                                             
022941     .                                                                    
022942     EJECT                                                                
022943                                                                          
022944 DB2-CLOSE-T01VAT-CRS SECTION.                                            
022945                                                                          
022946     EXEC SQL                                                             
022947        CLOSE T01VAT-CRS                                                  
022948     END-EXEC                                                             
022949     .                                                                    
022950     EJECT                                                                
022951                                                                          
022952 DB2-OPEN-T01VAT-CRS-2 SECTION.                                           
022953     MOVE 000100 TO GOOD-SQLCODECODES                                     
022954     EXEC SQL                                                             
022955         DECLARE T01VAT-CRS-2 CURSOR WITH HOLD FOR                        
022956                                                                          
022957           SELECT  IDLEGSEL, IDLANDX2,                                    
022958                   KDVAT, REVAT, DAREGDAT,                                
022959                   DAUPPDAT, DADELDAT, BEVAT                              
022960                                                                          
022961           FROM    T01VAT                                                 
022962                                                                          
022963           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022964           AND      IDLANDX2 = :REQU-IDLANDX2-KEY                         
022965                                                                          
022966           ORDER BY IDLEGSEL                                              
022967                  , IDLANDX2                                              
022968                  , KDVAT                                                 
022969     END-EXEC                                                             
022970     MOVE 000100  TO GOOD-SQLCODECODES                                    
022971                                                                          
022972     EXEC SQL                                                             
022973        OPEN T01VAT-CRS-2                                                 
022974     END-EXEC                                                             
022975     MOVE SQLCODE TO SQLCODE-WS                                           
022976     PERFORM DB2-STATUS-CHECK                                             
022977     .                                                                    
022978     EJECT                                                                
022979                                                                          
022980 DB2-FETCH-T01VAT-CRS-2 SECTION.                                          
022981     MOVE 000100  TO GOOD-SQLCODECODES                                    
022982     EXEC SQL                                                             
022983                                                                          
022984         FETCH T01VAT-CRS-2                                               
022985                                                                          
022986         INTO :WS-IDLEGSEL,                                               
022987              :WS-IDLANDX2-LINE,                                          
022988              :WS-KDVAT-LINE,                                             
022989              :WS-REVAT-LINE,                                             
022990              :WS-DAREGDAT-LINE,                                          
022991              :WS-DAUPPDAT-LINE,                                          
022992              :WS-DADELDAT-LINE,                                          
022993              :WS-BEVAT-LINE                                              
022994     END-EXEC                                                             
022995     MOVE SQLCODE TO SQLCODE-WS                                           
022996     PERFORM DB2-STATUS-CHECK                                             
022997     .                                                                    
022998     EJECT                                                                
022999                                                                          
023000 DB2-CLOSE-T01VAT-CRS-2 SECTION.                                          
023001                                                                          
023002     EXEC SQL                                                             
023003        CLOSE T01VAT-CRS-2                                                
023004     END-EXEC                                                             
023005     .                                                                    
023006     EJECT                                                                
023007 DB2-STATUS-CHECK     SECTION.                                            
023008     SET SQLCODE-IX TO 1                                                  
023009     SEARCH GOOD-SQLCODE                                                  
023010       AT END                                                             
023011          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
023012          DELIMITED BY SIZE INTO ERROR-TEXT                               
023013          CALL ABEND USING RKOD-ABEND-DB2                                 
023014       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
023020     END-SEARCH                                                           
023100     .                                                                    
