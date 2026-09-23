000100 PROCESS DYNAM                                                            
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF025500.                                                
001400 AUTHOR.         HENRIKSSON ANDERS.                                       
001500 DATE-WRITTEN.   02/03/01.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001710*    NAME                                                                 
001720*        CARPARTS.BILLIT.PERIODCALENDARLOCATE                             
001800*    FUNCTION:                                                            
001900*        LOCATE PERIOD CALENDAR                                           
002000*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
002100*                                                                         
002210*        THE PROGRAM READS     TABLE T01LSEL                              
002211*        THE PROGRAM READS     TABLE T01PECA                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: WF0255T                                             
002600*        REQUEST:     WF0255I1                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        RESPONSE:    WF0255O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003400 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
004000 DATA DIVISION.                                                           
004200 FILE SECTION.                                                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WF025500'.            
004700                                                                          
004800*    --- WORK FIELDS FOR ERROR MESSAGE WHEN CALLING ABEND.                
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
008202       05 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.             
008203       05 NOT-FOUND                  PIC X(3)    VALUE '025'.             
008210       05 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.             
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
009600*    03  -COPY WF0255I1                                                   
009700     EJECT                                                                
009710                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009900     SKIP3                                                                
010000 01  RESP-AREA.                                                           
010100*    03  -COPY WZ01RESP                                                   
010200*    03  -COPY WF0255O1                                                   
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
010503 01  WS-COUNTER-T01PECA         PIC S9(7)    COMP-3 VALUE ZERO.           
010504                                                                          
010505 01  WS-AREA.                                                             
010506     03 WS-KDSTATUS             PIC S9(3)    COMP-3 VALUE ZERO.           
010540     03 WS-IDLEGSEL             PIC X(4)     VALUE SPACE.                 
010550     03 WS-BELEGRAD-1           PIC X(35)    VALUE SPACE.                 
010551     03 WS-KVRADER              PIC Z(4)9(1) VALUE ZERO.                  
010560     03 WS-TIRP-LINE            PIC S9(2)    COMP-3 VALUE ZERO.           
010561     03 WS-DASTADAT-LINE        PIC X(8)     VALUE SPACE.                 
010562     03 WS-DASTADAT-LINE-2      PIC 9(8)     VALUE ZERO.                  
010563     03 WS-DAFINDOC-LINE        PIC X(8)     VALUE SPACE.                 
010564     03 WS-DAFINDOC-LINE-2      PIC 9(8)     VALUE ZERO.                  
010565     03 WS-FLPERIOD-LINE        PIC X(1)     VALUE SPACE.                 
010570     03 WS-DAREGDAT-LINE        PIC X(8)     VALUE SPACE.                 
010580     03 WS-DAREGDAT-LINE-2      PIC 9(8)     VALUE ZERO.                  
010590     03 WS-DAUPPDAT-LINE        PIC X(8)     VALUE SPACE.                 
010591     03 WS-DAUPPDAT-LINE-2      PIC 9(8)     VALUE ZERO.                  
010592     03 WS-IDUSER-LINE          PIC X(8)     VALUE SPACE.                 
010593     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
010594     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
010595     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
010801     EJECT                                                                
010802                                                                          
010803 01  FILLER                    PIC X(16)    VALUE 'T01LSEL-AREA'.         
010810*01  -COPY T01LSEL -PRE T01LSEL-                                          
010901     EJECT                                                                
010902                                                                          
010903 01  FILLER                    PIC X(16)    VALUE 'T01PECA-AREA'.         
010904*01  -COPY T01PECA -PRE T01PECA-                                          
010905     EJECT                                                                
010906                                                                          
010910     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
011000     EJECT                                                                
011020     EXEC SQL INCLUDE T01PECA END-EXEC.                                   
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
014318     MOVE SPACE TO RESP-IDMSG-ERROR                                       
014319     MOVE SPACE TO RESP-IDMSG-INFO                                        
014320     MOVE SPACE TO RESP-IDELMT-ERROR                                      
014321                                                                          
014322     MOVE ZERO                  TO WS-COUNTER-T01PECA                     
014323                                   RESP-KVRADER                           
014324                                                                          
014330     INITIALIZE GOOD-SQLCODECODES                                         
014600     .                                                                    
014700     EJECT                                                                
014710                                                                          
014800 B-CHECK-KEYS SECTION.                                                    
015200     IF REQU-KDPGMACT = 'S'                                               
015210     AND REQU-IDMSGVER NUMERIC                                            
015300       CONTINUE                                                           
015310     ELSE                                                                 
015320       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015350       MOVE NOO TO KEYS-SW                                                
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
015510                                                                          
015600     IF KEYS-WRONG                                                        
015700       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015710       IF REQU-KDPGMACT = 'S'                                             
015720         CONTINUE                                                         
015730       ELSE                                                               
015740         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015750         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
015760       END-IF                                                             
015770       IF REQU-IDMSGVER NUMERIC                                           
015780         CONTINUE                                                         
015790       ELSE                                                               
015791         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015792         MOVE 'IDMSGVER'   TO WS-IDELMT-ERROR                             
015793       END-IF                                                             
015794       IF REQU-IDUSER = SPACE OR = ALL '+'                                
015795         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015796         MOVE 'IDUSER'     TO WS-IDELMT-ERROR                             
015797       ELSE                                                               
015798         CONTINUE                                                         
015799       END-IF                                                             
015800     END-IF                                                               
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
016224     .                                                                    
016225     EJECT                                                                
016226                                                                          
016230 D-PERFORM-REQUEST SECTION.                                               
016300     IF REQU-KDPGMACT = 'S'                                               
016301       PERFORM DB2-COUNT-CRS-1                                            
016303       IF WS-COUNTER-T01PECA NOT = ZERO                                   
016304         IF WS-COUNTER-T01PECA > WS-MAX-LINES                             
016305           MOVE MORE-LINE-EXIST TO WS-IDMSG-ERROR                         
016306           MOVE NOO TO KEYS-SW                                            
016307         END-IF                                                           
016308       ELSE                                                               
016309         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
016310         MOVE NOO TO KEYS-SW                                              
016311       END-IF                                                             
016312       IF KEYS-OK                                                         
016313         PERFORM DB2-OPEN-T01PECA-CRS                                     
016314         PERFORM DB2-FETCH-T01PECA-CRS                                    
016315         MOVE ZERO TO WS-IX                                               
016316         PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES              
016317           PERFORM F-READ-SHOW-INFO-TABELL                                
016318           PERFORM DB2-FETCH-T01PECA-CRS                                  
016319         END-PERFORM                                                      
016325       END-IF                                                             
016330     END-IF                                                               
016340     PERFORM DB2-CLOSE-T01PECA-CRS                                        
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
018500     END-IF                                                               
018693     .                                                                    
018694     EJECT                                                                
018695                                                                          
018696 F-READ-SHOW-INFO-TABELL SECTION.                                         
018697     MOVE WS-BELEGRAD-1            TO RESP-BELEGRAD-1                     
018698     MOVE WS-COUNTER-T01PECA       TO RESP-KVRADER                        
018699     MOVE WS-DASTADAT-LINE         TO WS-DASTADAT-LINE-2                  
018700     MOVE WS-DAFINDOC-LINE         TO WS-DAFINDOC-LINE-2                  
018701     MOVE WS-DAREGDAT-LINE         TO WS-DAREGDAT-LINE-2                  
018702     MOVE WS-DAUPPDAT-LINE         TO WS-DAUPPDAT-LINE-2                  
018703                                                                          
018704     IF REQU-KDPGMACT = 'S'                                               
018705       IF WS-FLPERIOD-LINE = 'J'                                          
018706         MOVE 'Y' TO WS-FLPERIOD-LINE                                     
018707       END-IF                                                             
018708       ADD 1 TO WS-IX                                                     
018709       MOVE WS-TIRP-LINE             TO RESP-TIRP-LINE(WS-IX)             
018710       MOVE WS-DASTADAT-LINE-2       TO RESP-DASTADAT-LINE(WS-IX)         
018711       MOVE WS-FLPERIOD-LINE         TO RESP-FLPERIOD-LINE(WS-IX)         
018712       MOVE WS-DAREGDAT-LINE-2       TO RESP-DAREGDAT-LINE(WS-IX)         
018713       MOVE WS-DAFINDOC-LINE-2       TO RESP-DAFINDOC-LINE(WS-IX)         
018714       MOVE WS-DAUPPDAT-LINE-2       TO RESP-DAUPPDAT-LINE(WS-IX)         
018715       MOVE WS-IDUSER-LINE           TO RESP-IDUSER-LINE(WS-IX)           
018716     END-IF                                                               
018717     .                                                                    
018720     EJECT                                                                
018800                                                                          
019100*    --- DISPATCHER-SECTIONS                                              
019200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019400     MOVE 'GETARG'                               TO SUB-KDFUNC            
019500     MOVE 'CARPARTS.BILLIT.PERIODCALENDARLOCATE' TO SUB-ADDISPABS         
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
022825 DB2-COUNT-CRS-1 SECTION.                                                 
022827     MOVE 000100  TO GOOD-SQLCODECODES                                    
022828     EXEC SQL                                                             
022829                                                                          
022830           SELECT COUNT(*)                                                
022831                                                                          
022832           INTO  :WS-COUNTER-T01PECA                                      
022833                                                                          
022834           FROM   T01PECA                                                 
022835                                                                          
022836           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022839                                                                          
022840     END-EXEC                                                             
022843                                                                          
022844     MOVE SQLCODE TO SQLCODE-WS                                           
022845     PERFORM DB2-STATUS-CHECK                                             
022846     .                                                                    
022847     EJECT                                                                
022848                                                                          
022849 DB2-OPEN-T01PECA-CRS SECTION.                                            
022851     MOVE 000100 TO GOOD-SQLCODECODES                                     
022853     EXEC SQL                                                             
022854         DECLARE T01PECA-CRS CURSOR WITH HOLD FOR                         
022855                                                                          
022856           SELECT  DASTADAT,                                              
022857                   TIRP, DAREGDAT, DAUPPDAT, DAFINDOC,                    
022858                   FLPERIOD, IDUSER                                       
022864                                                                          
022865           FROM    T01PECA                                                
022866                                                                          
022867           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
022870                                                                          
022871           ORDER BY IDLEGSEL                                              
022872                  , DASTADAT DESC                                         
022874     END-EXEC                                                             
022875     MOVE 000100  TO GOOD-SQLCODECODES                                    
022876                                                                          
022877     EXEC SQL                                                             
022878        OPEN T01PECA-CRS                                                  
022879     END-EXEC                                                             
022881     MOVE SQLCODE TO SQLCODE-WS                                           
022882     PERFORM DB2-STATUS-CHECK                                             
022883     .                                                                    
022884     EJECT                                                                
022885                                                                          
022886 DB2-FETCH-T01PECA-CRS SECTION.                                           
022888     MOVE 000100  TO GOOD-SQLCODECODES                                    
022890     EXEC SQL                                                             
022891                                                                          
022892         FETCH T01PECA-CRS                                                
022893                                                                          
022894         INTO                                                             
022895              :WS-DASTADAT-LINE,                                          
022896              :WS-TIRP-LINE,                                              
022897              :WS-DAREGDAT-LINE,                                          
022899              :WS-DAUPPDAT-LINE,                                          
022900              :WS-DAFINDOC-LINE,                                          
022901              :WS-FLPERIOD-LINE,                                          
022902              :WS-IDUSER-LINE                                             
022903     END-EXEC                                                             
022904     MOVE SQLCODE TO SQLCODE-WS                                           
022905     PERFORM DB2-STATUS-CHECK                                             
022906     .                                                                    
022907     EJECT                                                                
022908                                                                          
022909 DB2-CLOSE-T01PECA-CRS SECTION.                                           
022910                                                                          
022911     EXEC SQL                                                             
022912        CLOSE T01PECA-CRS                                                 
022913     END-EXEC                                                             
022914     .                                                                    
022915     EJECT                                                                
022916                                                                          
022934 DB2-STATUS-CHECK     SECTION.                                            
022935     SET SQLCODE-IX TO 1                                                  
022936     SEARCH GOOD-SQLCODE                                                  
022937       AT END                                                             
022938          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
022939          DELIMITED BY SIZE INTO ERROR-TEXT                               
022940          CALL ABEND USING RKOD-ABEND-DB2                                 
022941       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
022942     END-SEARCH                                                           
022950     .                                                                    
