000100 PROCESS DYNAM                                                            
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5049300.                                                
001400 AUTHOR.         HAMMARIN BO.                                             
001500 DATE-WRITTEN.   NOV 2003.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001710*    NAME                                                                 
001711*        CARPARTS.PULS.RECEIVEC                                           
001800*    FUNCTION:                                                            
001900*        STARTS BY COMMUNICATION REGISTER VIA WZ01SEND.                   
001910*        READS ALL ROWS IN SENT DATA VIA WZ01RECV.                        
001920*        BUILD UP ROWS IN T01IVW.                                         
002400*                                                                         
002410*        THE PROGRAM UPDATES TABLE T01IVW                                 
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W50493X                                             
002800*        REQUEST:     WF2108I1 CUSTOMS                                    
002900*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
004200 DATA DIVISION.                                                           
004700 WORKING-STORAGE SECTION.                                                 
004800 77  IDPGM                       PIC X(08)  VALUE 'W5049300'.             
004900                                                                          
005000*    --- WORKFIELD FOR ERROR MESSAGES WHEN CALLING ABEND.                 
005100 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
005200 77  KDRC-DISPLAY                PIC Z(5).                                
005300                                                                          
005400 77  YES                         PIC X      VALUE 'J'.                    
005500 77  NOO                         PIC X      VALUE 'N'.                    
005510                                                                          
005520 77  WS-IX                       PIC S9(9)  VALUE +0    COMP SYNC.        
005521 77  RADER-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005522 77  LOPNR-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005530 77  MAX-RADER                   PIC S9(9)  VALUE +50   COMP SYNC.        
005531 77  MAX-LINES                  PIC S9(9)  VALUE +80000 COMP SYNC.        
005540 77  W-ANT                       PIC S9(3)  VALUE ZERO COMP-3.            
005600                                                                          
006300 77  KEYS-SW                     PIC X      VALUE SPACE.                  
006400     88  KEYS-OK                            VALUE 'J'.                    
006500     88  KEYS-ERROR                         VALUE 'N'.                    
006501                                                                          
006502 77  DUPLICATE-SW                PIC X.                                   
006503     88  DUPLICATE-ERROR                    VALUE 'J'.                    
006504     88  DUPLICATE-OK                       VALUE 'N'.                    
006505                                                                          
006600     EJECT                                                                
006700*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
007000     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
007100     03  WZ01RECV                PIC X(8)   VALUE 'WZ01RECV'.             
007200     03  WZ01SEND                PIC X(8)   VALUE 'WZ01SEND'.             
007300     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
007400     SKIP3                                                                
007410                                                                          
007500*    --- PARAMETERS TO ABEND                                              
007700 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
007800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
007810 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
007900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008300     03  ERR-DUPLICATE-LINES     PIC X(3)   VALUE '029'.                  
008800     EJECT                                                                
008801                                                                          
008802*    --- AREAS FOR WORK FIELDS                                            
008803 01  FILLER                      PIC X(16)  VALUE 'WDAT-CONTROL'.         
008804     SKIP3                                                                
008805 01  -COPY WDATAREA                                                       
008806     EJECT                                                                
008807 01  FILLER                      PIC X(16)  VALUE 'WDAT-AREA'.            
008808     SKIP3                                                                
008810                                                                          
008820 01  FILLER                      PIC X(16)  VALUE 'CUS-CONTROL'.          
008830     SKIP3                                                                
008840 01  -COPY W522CUS      -PRE CUS-                                         
008850     EJECT                                                                
008860 01  FILLER                      PIC X(16)  VALUE 'CUS-AREA'.             
008870     SKIP3                                                                
008880                                                                          
008900*    --- AREAS FOR COMMUNICATION                                          
009000 01  FILLER                      PIC X(16)  VALUE 'RECV-CONTROL'.         
009100     SKIP3                                                                
009200 01  -COPY WZ01RECV                                                       
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'RECV-AREA'.            
009500     SKIP3                                                                
009510                                                                          
009600 01  RECV-AREA.                                                           
009700*    03  -COPY WZ01REQU -PRE IN-                                          
009800*    03  -COPY WF2108I1 -PRE MID-WF2108I1-                                
009810     EJECT                                                                
009900                                                                          
009942 01  -COPY WZ01SEND                                                       
009943     EJECT                                                                
009944 01  FILLER                      PIC X(16)  VALUE 'SEND-AREA'.            
009945     SKIP3                                                                
009946                                                                          
009952 01  SEND-AREA.                                                           
009953*    03  -COPY WZ01RESP                                                   
009955     EJECT                                                                
009956                                                                          
009960                                                                          
011002 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
011003       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011004                                                                          
011005 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
011006 01  DB2-WS.                                                              
011007     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
011008         88  CURSOR-OK                      VALUE 000.                    
011009         88  LINES-FOUND                    VALUE 000.                    
011010         88  LINES-MISSING                  VALUE 100.                    
011011         88  DUPLICATE-LINES                VALUE 811.                    
011012         88  RESOURCE-WRONG                 VALUE 904.                    
011020                                                                          
011021     03  GOOD-SQLCODECODES.                                               
011022         05  GOOD-SQLCODE OCCURS 5                                        
011030             INDEXED BY SQLCODE-IX PIC 9(3).                              
011401     EJECT                                                                
011402                                                                          
011403 01  FILLER                      PIC X(16)  VALUE 'WS-AREA'.              
011405 01  WS-AREA.                                                             
011406     03 WS-DATUM                 PIC X(8)   VALUE SPACE.                  
011407     03 WS-KLOCKAN               PIC 9(7)   VALUE ZERO.                   
011411     03 WS-IDDISTR-1             PIC 9(5)   VALUE ZERO.                   
011413     03 WS-IDARTNR-1             PIC 9(9)   VALUE ZERO.                   
011414                                                                          
011415     03 WS-DAREGDAT              PIC X(8)   VALUE SPACE.                  
011416     03 WS-DAEXDAT               PIC 9(8)   VALUE ZERO.                   
011417     03 WS-TIREGTID              PIC S9(6)  VALUE ZERO COMP-3.            
011418     03 WS-TIEXTID               PIC 9(6)   VALUE ZERO.                   
011419     03 WS-IDLOPNR               PIC S9(5)  VALUE ZERO COMP-3.            
011420     03 WS-IDPTYP                PIC X(3)   VALUE SPACE.                  
011421     03 WS-TIRP-1                PIC S9(2)  VALUE ZERO COMP-3.            
011422     03 WS-TIAA-1                PIC S9(2)  VALUE ZERO COMP-3.            
011423     03 WS-TIMM-1                PIC S9(2)  VALUE ZERO COMP-3.            
011424     03 WS-FLKLAR                PIC X(1)   VALUE 'N'.                    
011425                                                                          
011426     03 WS-CUSTOMS-DATA          PIC X(200) VALUE SPACE.                  
011427     03 WS-CUSTOMS-DATA-2        PIC X(200) VALUE SPACE.                  
011460                                                                          
011472 01  FILLER                      PIC X(16)  VALUE 'T01IVW-AREA'.          
011480*01  -COPY T01IVW -PRE T01IVW-                                            
011501     EJECT                                                                
011502                                                                          
011510     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
011600     EJECT                                                                
011610                                                                          
011700 LINKAGE SECTION.                                                         
012101 PROCEDURE DIVISION.                                                      
012102 MAIN SECTION.                                                            
012200                                                                          
013710     PERFORM S01-READ-OPEN                                                
013720     PERFORM S02-READ-MESSAGE                                             
013740     PERFORM A-INIT                                                       
013741     IF RECV-KDRC = ZERO                                                  
013751       PERFORM UNTIL RECV-KDRC > ZERO OR WS-IX > MAX-LINES                
013760         ADD 1 TO LOPNR-IX                                                
013771         PERFORM B-PERFORM-LINES                                          
013780         PERFORM C-UPDATE-T01IVW                                          
013790         PERFORM S02-READ-MESSAGE                                         
013791       END-PERFORM                                                        
013792     END-IF                                                               
013793                                                                          
013801     PERFORM S03-READ-CLOSE                                               
013802* TO MANY LINES INCREASE MAX-LINES OR MAKE A RESTART FUNCTION             
013900     IF WS-IX > MAX-LINES                                                 
014000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014010     END-IF                                                               
014200                                                                          
014400     MOVE ZERO TO RETURN-CODE                                             
014500     GOBACK                                                               
014600     .                                                                    
014700     EJECT                                                                
014710                                                                          
014800 A-INIT SECTION.                                                          
015000     MOVE YES                          TO KEYS-SW                         
015001     MOVE NOO                          TO DUPLICATE-SW                    
015002     MOVE ZERO                         TO RADER-IX                        
015003     MOVE ZERO                         TO LOPNR-IX                        
015004     MOVE ZERO                         TO WS-IX                           
015005                                                                          
015170     INITIALIZE GOOD-SQLCODECODES                                         
015180     .                                                                    
015190     EJECT                                                                
015191                                                                          
015192 B-PERFORM-LINES SECTION.                                                 
015193     IF MID-WF2108I1-IDPTYP = 'CUS'                                       
015194       PERFORM BA-PERFORM-LINES-CUSTOMS                                   
015195     END-IF                                                               
015199     .                                                                    
015200     EJECT                                                                
015201                                                                          
015202 BA-PERFORM-LINES-CUSTOMS SECTION.                                        
015203     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
015204     MOVE MID-WF2108I1-DAFINDOC(3:6) TO DAT-I-TIDATUM                     
015205     CALL WDATKONV USING                                                  
015206          DAT-KDDATFORM                                                   
015207          DAT-I-TIDATUM                                                   
015208          DAT-O-TIDATUM                                                   
015209          DAT-KDSVAR                                                      
015210     MOVE DAT-TIVV                     TO WS-TIRP-1                       
015211                                                                          
015213     MOVE MID-WF2108I1-DAEXDAT(3:2)    TO WS-TIAA-1                       
015224                                                                          
015225     MOVE +0  TO W-ANT                                                    
015226     INSPECT MID-WF2108I1-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS        
015227             BEFORE INITIAL ' '                                           
015228     IF W-ANT = 0                                                         
015229       MOVE +1  TO W-ANT                                                  
015230     END-IF                                                               
015231     MOVE MID-WF2108I1-IDEXCUST-1(1:W-ANT) TO WS-IDDISTR-1                
015232                                                                          
015235     MOVE +0  TO W-ANT                                                    
015236     INSPECT MID-WF2108I1-IDARTNR-FINANCE                                 
015237             TALLYING W-ANT FOR CHARACTERS                                
015238             BEFORE INITIAL ' '                                           
015239     IF W-ANT = 0                                                         
015240       MOVE +1  TO W-ANT                                                  
015241     END-IF                                                               
015242     MOVE MID-WF2108I1-IDARTNR-FINANCE(1:W-ANT) TO WS-IDARTNR-1           
015243                                                                          
015244     MOVE WS-TIAA-1                    TO CUS-TIAA                        
015245     MOVE WS-TIRP-1                    TO CUS-TIRP                        
015250     MOVE MID-WF2108I1-DAEXDAT         TO WS-DAREGDAT                     
015300     MOVE MID-WF2108I1-TIEXTID         TO WS-TIREGTID                     
015305     MOVE MID-WF2108I1-IDPTYP          TO WS-IDPTYP                       
015307     MOVE LOPNR-IX                     TO WS-IDLOPNR                      
015310     MOVE MID-WF2108I1-IDLANDX3-BET    TO CUS-IDLANDX3-BET                
015311     MOVE MID-WF2108I1-IDLANDX3-SEND   TO CUS-IDLANDX3-SEND               
015315     MOVE MID-WF2108I1-IDPARTNR        TO CUS-IDPARTNR                    
015316     MOVE MID-WF2108I1-KDFINDOC        TO CUS-KDFINDOC                    
015317     MOVE MID-WF2108I1-DAFINDOC        TO CUS-DAFINDOC                    
015318     MOVE MID-WF2108I1-IDFINDOC        TO CUS-IDFINDOC                    
015319     MOVE WS-IDDISTR-1                 TO CUS-IDDISTR                     
015322     MOVE WS-IDARTNR-1                 TO CUS-IDARTNR                     
015323     MOVE MID-WF2108I1-BEART           TO CUS-BEART                       
015330     MOVE MID-WF2108I1-KVLEVART        TO CUS-KVLEVART                    
015336                                                                          
015337     MOVE SPACE                        TO WS-CUSTOMS-DATA                 
015338                                          WS-CUSTOMS-DATA-2               
015339     MOVE CUS-W522CUS                  TO WS-CUSTOMS-DATA                 
015340                                                                          
015350     ADD 1                             TO WS-IX                           
015600     .                                                                    
015700     EJECT                                                                
015701                                                                          
017100 C-UPDATE-T01IVW SECTION.                                                 
017101     IF MID-WF2108I1-IDPTYP = 'CUS'                                       
017102       PERFORM DB2-INSERT-T01IVW-CUSTOMS                                  
017103     END-IF                                                               
018100     .                                                                    
018200     EJECT                                                                
018210                                                                          
020100*    --- DISPATCHER-SECTIONS                                              
020200 S01-READ-OPEN SECTION.                                                   
020400     MOVE 'OPEN'                      TO RECV-KDFUNC                      
020500     MOVE 'CARPARTS.PULS.RECEIVEC'    TO RECV-ADDISPABS                   
020600     CALL WZ01RECV USING RECV-CONTROL-AREA                                
020700                         RECV-OPEN-AREA                                   
020800     IF RECV-KDRC > 0                                                     
020900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
021000       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
021100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021300     END-IF                                                               
021400     .                                                                    
021500     SKIP3                                                                
021510                                                                          
021600 S02-READ-MESSAGE SECTION.                                                
021800     MOVE 'GET'                           TO RECV-KDFUNC                  
021900     MOVE LENGTH OF RECV-AREA             TO RECV-KVDLEN                  
022000     CALL WZ01RECV USING RECV-CONTROL-AREA                                
022100                         RECV-KVDLEN                                      
022110                         RECV-AREA                                        
022200     IF RECV-KDRC > 1                                                     
022300       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
022400       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
022500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022700     END-IF                                                               
022800     .                                                                    
022900     SKIP3                                                                
022910                                                                          
023000 S03-READ-CLOSE SECTION.                                                  
023200     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
023300     CALL WZ01RECV USING RECV-CONTROL-AREA                                
023400                                                                          
023500     IF RECV-KDRC > 0                                                     
023600       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
023700       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
023800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
023900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
024210                                                                          
028941 DB2-INSERT-T01IVW-CUSTOMS SECTION.                                       
028942     SKIP2                                                                
028943     MOVE 000811 TO GOOD-SQLCODECODES                                     
028945     EXEC SQL                                                             
028946         INSERT INTO T01IVW                                               
028947         (DAREGDAT,                                                       
028948          TIREGTID,                                                       
028949          IDLOPNR,                                                        
028950          IDPTYP,                                                         
028951          TIRP,                                                           
028953          FLKLAR,                                                         
028955          IV_DATA,                                                        
028956          IV_DATA2)                                                       
029005         VALUES(:WS-DAREGDAT,                                             
029006                :WS-TIREGTID,                                             
029007                :WS-IDLOPNR,                                              
029008                :WS-IDPTYP,                                               
029009                :WS-TIRP-1,                                               
029011                'N',                                                      
029012                :WS-CUSTOMS-DATA,                                         
029013                :WS-CUSTOMS-DATA-2)                                       
029061     END-EXEC                                                             
029062                                                                          
029063     MOVE SQLCODE TO SQLCODE-WS                                           
029065     PERFORM DB2-STATUS-CHECK                                             
029066     .                                                                    
029067     EJECT                                                                
029068                                                                          
029097 DB2-STATUS-CHECK     SECTION.                                            
029098     SET SQLCODE-IX TO 1                                                  
029099     SEARCH GOOD-SQLCODE                                                  
029100       AT END                                                             
029101          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
029102          DELIMITED BY SIZE INTO ERROR-TEXT                               
029103          CALL ABEND USING RKOD-ABEND-DB2                                 
029104       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
029110     END-SEARCH                                                           
029200     .                                                                    
