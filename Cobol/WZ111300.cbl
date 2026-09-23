000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ111300.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   20/05/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PROGRAM FOR RECEIVING FILE FROM MQ                               
001000*                                                                         
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300 CONFIGURATION SECTION.                                                   
001400*SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700*          --- SYMBOLIC PARAMETER TO SOP ORDER                            
001800     SELECT SOPPARM                    ASSIGN TO SOPSYM.                  
001900                                                                          
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200 FD  SOPPARM                                                              
002300     RECORDING       F                                                    
002400     BLOCK CONTAINS  0.                                                   
002500                                                                          
002600 01  SOPPARM-RECORD   PIC X(80).                                          
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(9)    VALUE 'WZ111300'.            
003100 77  YES                         PIC X       VALUE 'J'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300 77  LL                          PIC 9(4)    VALUE ZERO BINARY.           
003400 77  WS-LEN                      PIC X(4)    VALUE SPACE.                 
003500 77  IX-R                        PIC 9       VALUE ZERO.                  
003600 77  IX-S                        PIC 9       VALUE ZERO.                  
003700 77  WS-RECKEY-TOTAL             PIC 9(4)    VALUE ZERO.                  
003800 77  WSTART                      PIC 9(4)    VALUE 1.                     
003900                                                                          
004000*    -- "OUTFILE" CORRESPONDS TO AN FD NAME FOR NORMAL FILES              
004100 01  OUTFILE                     PIC S9(9)   COMP.                        
004200                                                                          
004300 01  W-RECORD-COUNT              PIC S9(9)   COMP-3 VALUE ZERO.           
004400                                                                          
004500*    -- CONTROL PARAMETERS READ VIA PARM                                  
004600 01  WS-ADDISPABS                PIC X(50)   VALUE SPACE.                 
004700 01  WS-FILE-PATTERN             PIC X(44)   VALUE SPACE.                 
004800 01  WS-RECFM                    PIC XX      VALUE SPACE.                 
004900 01  WS-LRECL-NUM                PIC 9(4)    VALUE ZERO BINARY.           
005000                                                                          
005100 01  WS-RECKEY-TABLE.                                                     
005200     03  FILLER OCCURS 5 TIMES.                                           
005300         05  WS-RECKEY-LEN       PIC 9(2)    VALUE ZERO.                  
005400         05  WS-RECKEY-NAME      PIC X(20)   VALUE SPACES.                
005500         05  WS-RECKEY-VALUE     PIC X(60)   VALUE SPACES.                
005600                                                                          
005700 01  WS-SOPSYM-TABLE.                                                     
005800     03  FILLER OCCURS 5 TIMES.                                           
005900         05  WS-SOPSYM-KEY       PIC X(20)   VALUE SPACES.                
006000         05  WS-SOPSYM-NAME      PIC X(20)   VALUE SPACES.                
006100         05  WS-SOPSYM-VALUE     PIC X(60)   VALUE SPACES.                
006200                                                                          
006300 01  SOPPARM-AREA                PIC X(80)   VALUE SPACES.                
006400                                                                          
006500*    -- WORK AREAS FOR CONSTRUCTING THE OUTPUT DSNAME                     
006600 01  W-PART1                     PIC X(40)   VALUE SPACES.                
006700 01  W-PART2                     PIC X(40)   VALUE SPACES.                
006800                                                                          
006900*    -- FILE NAME CONSTRUCTED FROM FILE PATTERN AND FILE MARKER           
007000 01  OUTPUT-FILE-NAME            PIC X(44).                               
007100                                                                          
007200 01  DYN-PARM.                                                            
007300     03  DYN-INIT                PIC X       VALUE 'I'.                   
007400     03  DYN-OPEN                PIC X       VALUE 'O'.                   
007500     03  DYN-WRITE               PIC X       VALUE ' '.                   
007600     03  DYN-CLOSE               PIC X       VALUE 'C'.                   
007700                                                                          
007800 01   -COPY WDYNAREA                                                      
007900                                                                          
008000 01  KDRC-DISPLAY                PIC Z(5).                                
008100                                                                          
008200 01  GENERAL-SUBPROGRAMS.                                                 
008300*                                                                         
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008500     03  WZ11OUTQ                PIC X(8)    VALUE 'WZ11OUTQ'.            
008600     03  WDYNALC                 PIC X(8)    VALUE 'WDYNALC'.             
008700     03  WFILWRT                 PIC X(8)    VALUE 'WFILWRT'.             
008800     03  WDSINFO                 PIC X(8)    VALUE 'WDSINFO'.             
008900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009000                                                                          
009100*    --- PARAMETERS TO ABEND                                              
009200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009500                                                                          
009600*    --- PARAMETERS TO POSTSUM                                            
009700                                                                          
009800 01  -COPY W0005   -PRE  POSTSUM-                                         
009900                                                                          
010000*    --- PARAMETERS TO WDSINFO                                            
010100                                                                          
010200 01  -COPY WDSAREA -PRE  DSINFO-                                          
010300                                                                          
010400 01  ERROR-TEXT.                                                          
010500     03  FILLER                  PIC X(12)   VALUE 'ERROR-TEXT: '.        
010600     03  ERROR-TEXT-STR          PIC X(80)   VALUE SPACE.                 
010700                                                                          
010800 01  DISPLAY-TEXT                PIC X(80)   VALUE SPACE.                 
010900                                                                          
011000 01  SYSIN-AREA                  PIC X(80)   VALUE SPACES.                
011100                                                                          
011200 01  WS-SYSIN-REC.                                                        
011300     03  SYSIN-KEY               PIC X(80).                               
011400     03  SYSIN-VALUE             PIC X(80).                               
011500                                                                          
011600 01  UT-AREA-START               PIC X(16)   VALUE  'UT-AREA'.            
011700 01  UT-RECLEN                   PIC 9(4)    VALUE ZERO.                  
011800 01  UT-AREA                     PIC X(3000) VALUE SPACE.                 
011900                                                                          
012000 01 OUTQ-AREA.                                                            
012100    03  OUTQ-CONTROL-AREA.                                                
012200        05  OUTQ-KDFUNC          PIC X(10).                               
012300        05  OUTQ-KDRC            PIC S9(9) COMP.                          
012400        05  OUTQ-IDCOM           PIC S9(9) COMP.                          
012500    03  OUTQ-OPEN-AREA.                                                   
012600        05  OUTQ-ADDISPABS       PIC X(50).                               
012700    03  OUTQ-PROPERTY-AREA.                                               
012800        05  OUTQ-PROPERTY-NAME   PIC X(100).                              
012900        05  OUTQ-PROPERTY-VALUE  PIC X(100).                              
013000    03  OUTQ-ADDITIONAL-INFO.                                             
013100        05  OUTQ-PHYSICALID      PIC X(100).                              
013200    03  OUTQ-KVDLEN              PIC S9(9) BINARY.                        
013300    03  OUTQ-DATA.                                                        
013400        05 FILLER OCCURS 1 TO 104857600 TIMES                             
013500           DEPENDING ON OUTQ-KVDLEN PIC X.                                
013600                                                                          
013700                                                                          
013800 LINKAGE SECTION.                                                         
013900                                                                          
014000 01  PARM-AREA.                                                           
014100     03  PARM-LENGTH             PIC S9(4) BINARY.                        
014200     03  PARM-RTENV              PIC X(4).                                
014300     03  PARM-DATA               PIC X(96).                               
014400                                                                          
014500 PROCEDURE DIVISION USING PARM-AREA.                                      
014600 MAIN SECTION.                                                            
014700                                                                          
014800     PERFORM A-INIT                                                       
014900                                                                          
015000     PERFORM S04-OUTQ-OPEN                                                
015100     MOVE 'IntegrationId'        TO OUTQ-PROPERTY-NAME                    
015200     PERFORM S04-INQ-PROP                                                 
015300                                                                          
015400     PERFORM                                                              
015500     VARYING IX-R FROM 1 BY 1                                             
015600       UNTIL IX-R > 5 OR WS-RECKEY-LEN (IX-R) = 0                         
015700       MOVE WS-RECKEY-NAME (IX-R)                                         
015800                                 TO OUTQ-PROPERTY-NAME                    
015900       PERFORM S04-INQ-PROP                                               
016000       *> RRR CHK FOR INVALID VALUE                                       
016100       MOVE OUTQ-PROPERTY-VALUE                                           
016200                                 TO WS-RECKEY-VALUE (IX-R)                
016300     END-PERFORM                                                          
016400                                                                          
016500     PERFORM B-PROCESS-SOPPARM                                            
016600                                                                          
016700     PERFORM S01-ALLOC-OPEN-OUTPUT-FILE                                   
016800     PERFORM S02-GET-DSINFO                                               
016900     PERFORM S04-GET-MESSAGE                                              
017000                                                                          
017100     PERFORM UNTIL OUTQ-KDRC > 0                                          
017200       PERFORM B-PROCESS-MESSAGE                                          
017300       PERFORM S01-WRITE-OUTPUT-RECORD                                    
017400       PERFORM S04-GET-MESSAGE                                            
017500     END-PERFORM                                                          
017600                                                                          
017700     PERFORM S01-CLOSE-OUTPUT-FILE                                        
017800     PERFORM S04-OUTQ-CLOSE                                               
017900                                                                          
018000     PERFORM Z-FINIT                                                      
018100                                                                          
018200     MOVE ZERO                   TO RETURN-CODE                           
018300     GOBACK                                                               
018400     .                                                                    
018500 A-INIT SECTION.                                                          
018600                                                                          
018700     OPEN OUTPUT SOPPARM                                                  
018800                                                                          
018900* -- OUTPUT DATA FILE IS OPENED VIA WDYNALC                               
019000                                                                          
019100     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
019200                                                                          
019300     MOVE 1                      TO IX-R                                  
019400                                    IX-S                                  
019500                                                                          
019600     MOVE SPACES                 TO SYSIN-AREA                            
019700     ACCEPT SYSIN-AREA         FROM SYSIN                                 
019800                                                                          
019900     PERFORM                                                              
020000       UNTIL SYSIN-AREA = SPACES                                          
020100D      DISPLAY IDPGM SYSIN-AREA                                           
020200       MOVE ZERO                 TO LL                                    
020300       UNSTRING SYSIN-AREA DELIMITED BY SPACE                             
020400                               INTO SYSIN-KEY                             
020500                                    SYSIN-VALUE COUNT IN LL               
020600       EVALUATE SYSIN-KEY                                                 
020700         WHEN '¤ADDISPABS'                                                
020800           MOVE SYSIN-VALUE      TO WS-ADDISPABS                          
020900                                    OUTQ-ADDISPABS                        
021000         WHEN '¤DSOUT'                                                    
021100           MOVE SYSIN-VALUE      TO WS-FILE-PATTERN                       
021200         WHEN '¤RECFM'                                                    
021300           MOVE SYSIN-VALUE      TO WS-RECFM                              
021400         WHEN '¤LRECL'                                                    
021500           MOVE SYSIN-VALUE      TO WS-LEN                                
021600           IF LL = 0 OR WS-LEN (1:LL) NOT NUMERIC                         
021700             MOVE 'LRECL MISSING OR NOT NUMERIC'                          
021800                                 TO ERROR-TEXT-STR                        
021900             PERFORM S99-ABEND                                            
022000           ELSE                                                           
022100             MOVE WS-LEN (1:LL)  TO WS-LRECL-NUM                          
022200           END-IF                                                         
022300         WHEN '¤RECKEY'                                                   
022400           PERFORM ABA-GET-RECKEY                                         
022500         WHEN '¤SOPSYM'                                                   
022600           PERFORM ABB-GET-SOPSYM                                         
022700         WHEN OTHER                                                       
022800           MOVE 'UNKNOWN PARAMETER IN SYSIN !'                            
022900                                 TO ERROR-TEXT-STR                        
023000           PERFORM S99-ABEND                                              
023100       END-EVALUATE                                                       
023200       MOVE SPACES               TO SYSIN-AREA                            
023300       ACCEPT SYSIN-AREA       FROM SYSIN                                 
023400     END-PERFORM                                                          
023500                                                                          
023600     IF WS-ADDISPABS = SPACES                                             
023700       MOVE 'INVALID/MISSING ADDISPABS !'                                 
023800                                 TO ERROR-TEXT-STR                        
023900       PERFORM S99-ABEND                                                  
024000     END-IF                                                               
024100                                                                          
024200     IF WS-FILE-PATTERN = SPACES                                          
024300       MOVE 'MISSING ¤DSOUT            !'                                 
024400                                 TO ERROR-TEXT-STR                        
024500       PERFORM S99-ABEND                                                  
024600     END-IF                                                               
024700                                                                          
024800     IF WS-RECFM = 'FB' OR 'VB' OR SPACES                                 
024900       IF WS-RECFM = SPACES                                               
025000         MOVE 'VB'               TO WS-RECFM                              
025100       END-IF                                                             
025200     ELSE                                                                 
025300       MOVE 'OUTPUT FILE RECORD FORMAT MUST BE FB OR VB'                  
025400                                 TO ERROR-TEXT-STR                        
025500       PERFORM S99-ABEND                                                  
025600     END-IF                                                               
025700                                                                          
025800     IF WS-LRECL-NUM = ZERO                                               
025900       MOVE 3000                 TO WS-LRECL-NUM                          
026000     END-IF                                                               
026100                                                                          
026200D    DISPLAY IDPGM '************* PARM DATA **************'               
026300D    DISPLAY IDPGM 'ENVIRONMENT        : ' PARM-RTENV                     
026400D    DISPLAY IDPGM 'ADDRESS            : ' WS-ADDISPABS                   
026500D    DISPLAY IDPGM 'FILE PATTERN       : ' WS-FILE-PATTERN                
026600D    DISPLAY IDPGM 'RECFM              : ' WS-RECFM                       
026700D    DISPLAY IDPGM 'LRECL              : ' WS-LRECL-NUM                   
026800D    DISPLAY IDPGM '********** END OF PARM DATA **********'               
026900                                                                          
027000     .                                                                    
027100                                                                          
027200 ABA-GET-RECKEY SECTION.                                                  
027300                                                                          
027400     IF IX-R > 5                                                          
027500       DISPLAY IDPGM 'CAN HANDLE ONLY 5 RECKEY''s'                        
027600       CALL ABEND                                                         
027700     ELSE                                                                 
027800       MOVE SPACES               TO WS-LEN                                
027900       MOVE ZEROES               TO LL                                    
028000       UNSTRING SYSIN-VALUE                                               
028100                       DELIMITED BY ';' OR ALL SPACES                     
028200                               INTO WS-RECKEY-NAME (IX-R)                 
028300                                    WS-LEN COUNT IN LL                    
028400       IF WS-RECKEY-NAME (IX-R) > SPACES                                  
028500         IF LL = 0 OR WS-LEN (1:LL) NOT NUMERIC                           
028600           MOVE 'LENGTH OF RECKEY MISSING'                                
028700                                 TO ERROR-TEXT-STR                        
028800           PERFORM S99-ABEND                                              
028900         ELSE                                                             
029000           MOVE WS-LEN (1:LL)                                             
029100                                 TO WS-RECKEY-LEN (IX-R)                  
029200           ADD WS-RECKEY-LEN (IX-R)                                       
029300                                 TO WS-RECKEY-TOTAL                       
029400           ADD 1                 TO IX-R                                  
029500         END-IF                                                           
029600       ELSE                                                               
029700         MOVE 'RECKEY IS MISSING...'                                      
029800                                 TO ERROR-TEXT-STR                        
029900         PERFORM S99-ABEND                                                
030000       END-IF                                                             
030100     END-IF                                                               
030200     .                                                                    
030300                                                                          
030400 ABB-GET-SOPSYM SECTION.                                                  
030500                                                                          
030600     IF IX-S > 5                                                          
030700       MOVE 'CAN HANDLE ONLY 5 SOPSYM''s'                                 
030800                                 TO ERROR-TEXT-STR                        
030900       PERFORM S99-ABEND                                                  
031000     ELSE                                                                 
031100       MOVE SPACES               TO WS-LEN                                
031200       MOVE ZEROES               TO LL                                    
031300       UNSTRING SYSIN-VALUE                                               
031400                       DELIMITED BY ';' OR ALL SPACES                     
031500                               INTO WS-SOPSYM-NAME (IX-S)                 
031600                                    WS-SOPSYM-KEY  (IX-S)                 
031700       IF WS-SOPSYM-NAME (IX-S) = SPACES                                  
031800         MOVE 'SOPSYM MISSING...'                                         
031900                                 TO ERROR-TEXT-STR                        
032000         PERFORM S99-ABEND                                                
032100       END-IF                                                             
032200       IF WS-SOPSYM-KEY (IX-S) = SPACES                                   
032300         MOVE 'SOP SYMBOL KEY MISSING...'                                 
032400                                 TO ERROR-TEXT-STR                        
032500         PERFORM S99-ABEND                                                
032600       END-IF                                                             
032700       ADD 1                     TO IX-S                                  
032800     END-IF                                                               
032900     .                                                                    
033000                                                                          
033100 B-PROCESS-SOPPARM SECTION.                                               
033200                                                                          
033300     PERFORM                                                              
033400     VARYING IX-S FROM 1 BY 1                                             
033500       UNTIL IX-S > 5 OR WS-SOPSYM-KEY (IX-S) = SPACES                    
033600       MOVE WS-SOPSYM-NAME (IX-S)                                         
033700                                 TO OUTQ-PROPERTY-NAME                    
033800       PERFORM S04-INQ-PROP                                               
033900       *> RRR CHK FOR INVALID VALUE                                       
034000       IF OUTQ-PROPERTY-VALUE = SPACES                                    
034100         MOVE 'SOPSYM PROPERTY IS MISSING A VALUE..'                      
034200                                 TO ERROR-TEXT-STR                        
034300         DISPLAY IDPGM ERROR-TEXT-STR                                     
034400         PERFORM S99-ABEND                                                
034500       ELSE                                                               
034600         MOVE OUTQ-PROPERTY-VALUE                                         
034700                                 TO WS-SOPSYM-VALUE (IX-S)                
034800         MOVE SPACE              TO SOPPARM-AREA                          
034900         STRING ' '                    DELIMITED BY SIZE                  
035000                WS-SOPSYM-KEY (IX-S)   DELIMITED BY SPACE                 
035100                '('                    DELIMITED BY SIZE                  
035200                WS-SOPSYM-VALUE (IX-S) DELIMITED BY SPACE                 
035300                ')'                    DELIMITED BY SIZE                  
035400                               INTO SOPPARM-AREA                          
035500                                                                          
035600         WRITE SOPPARM-RECORD  FROM SOPPARM-AREA                          
035700       END-IF                                                             
035800     END-PERFORM                                                          
035900                                                                          
036000     .                                                                    
036100 B-PROCESS-MESSAGE SECTION.                                               
036200                                                                          
036300D    DISPLAY IDPGM 'RECEIVED MSG LEN = ' OUTQ-KVDLEN                      
036400     IF OUTQ-KVDLEN > 0                                                   
036500       INITIALIZE UT-AREA                                                 
036600       MOVE 1                    TO WSTART                                
036700       PERFORM                                                            
036800       VARYING IX-R FROM 1 BY 1                                           
036900         UNTIL IX-R > 5 OR WS-RECKEY-LEN (IX-R) = 0                       
037000         STRING WS-RECKEY-VALUE (IX-R) (1 : WS-RECKEY-LEN (IX-R))         
037100                       DELIMITED BY SIZE                                  
037200                               INTO UT-AREA                               
037300                       WITH POINTER WSTART                                
037400       END-PERFORM                                                        
037500       STRING OUTQ-DATA (1:OUTQ-KVDLEN)                                   
037600                       DELIMITED BY SIZE                                  
037700                               INTO UT-AREA                               
037800                       WITH POINTER WSTART                                
037900       SUBTRACT 1              FROM WSTART                                
038000                             GIVING UT-RECLEN                             
038100     ELSE                                                                 
038200       MOVE ZERO                 TO UT-RECLEN                             
038300       MOVE SPACES               TO UT-AREA                               
038400     END-IF                                                               
038500     .                                                                    
038600                                                                          
038700 Z-FINIT SECTION.                                                         
038800                                                                          
038900     CLOSE SOPPARM                                                        
039000                                                                          
039100     MOVE 'S'                    TO POSTSUM-OPKOD                         
039200     CALL POSTSUM             USING POSTSUM-PARM                          
039300                                                                          
039400     CONTINUE                                                             
039500     .                                                                    
039600                                                                          
039700 S01-ALLOC-OPEN-OUTPUT-FILE SECTION.                                      
039800                                                                          
039900*--  INITIERA DCB I WFILWRT SÅ PROGRAMMET VET                             
040000*--  VILKEN FIL DEN SKA ÖPPNA OCH SKRIVA SEDAN                            
040100     CALL WFILWRT             USING OUTFILE                               
040200                                    DYN-INIT                              
040300                                                                          
040400     IF DYN-INIT = 'F'                                                    
040500       MOVE 'INIT CALL TO WFILWRT FAILED'                                 
040600                                 TO ERROR-TEXT-STR                        
040700       PERFORM S99-ABEND                                                  
040800     END-IF                                                               
040900                                                                          
041000*--  SET THE FILE NAME                                                    
041100     MOVE SPACES                 TO OUTQ-PROPERTY-NAME                    
041200                                    OUTQ-PROPERTY-VALUE                   
041300     UNSTRING WS-FILE-PATTERN DELIMITED BY '*'                            
041400                               INTO W-PART1                               
041500                                    OUTQ-PROPERTY-NAME                    
041600                                    W-PART2                               
041700     IF OUTQ-PROPERTY-NAME > SPACES                                       
041800       PERFORM S04-INQ-PROP                                               
041900         *> RRR  HANDLE INVALID PROP VALUE                                
042000     END-IF                                                               
042100                                                                          
042200     MOVE SPACE                  TO OUTPUT-FILE-NAME                      
042300     STRING W-PART1             DELIMITED BY SPACE                        
042400            OUTQ-PROPERTY-VALUE DELIMITED BY SPACE                        
042500            W-PART2             DELIMITED BY SPACE                        
042600       INTO OUTPUT-FILE-NAME                                              
042700                                                                          
042800     MOVE FUNCTION UPPER-CASE (OUTPUT-FILE-NAME)                          
042900                                 TO OUTPUT-FILE-NAME                      
043000     DISPLAY IDPGM 'ALLOCATING OUTPUT FILE: ' OUTPUT-FILE-NAME            
043100                                                                          
043200     MOVE 'C'                    TO DYN-FREE                              
043300     MOVE OUTPUT-FILE-NAME       TO DYN-DSNAME                            
043400     MOVE '+1'                   TO DYN-GENMBR                            
043500     MOVE WS-RECFM               TO DYN-RECFM                             
043600     MOVE 'N'                    TO DYN-DISP1                             
043700     MOVE 'C'                    TO DYN-DISP2                             
043800     MOVE 'D'                    TO DYN-DISP3                             
043900     MOVE 'PSEB'                 TO DYN-DATACLASS                         
044000     MOVE 'NOBACKUP'             TO DYN-MGMCLASS                          
044100     MOVE WS-LRECL-NUM           TO DYN-LRECL                             
044200     MOVE +0                     TO DYN-BLKSIZE                           
044300     MOVE 'R'                    TO DYN-RLSE                              
044400                                                                          
044500     CALL WDYNALC             USING OUTFILE                               
044600                                    DYN-AREA                              
044700     IF DYN-KDSVAR-OK                                                     
044800D      DISPLAY IDPGM 'OUTPUT FILE ALLOCATED - OK'                         
044900       CONTINUE                                                           
045000     ELSE                                                                 
045100       MOVE 'OUTPUT FILE COULD NOT BE ALLOCATED'                          
045200                                 TO ERROR-TEXT-STR                        
045300       PERFORM S99-ABEND                                                  
045400     END-IF                                                               
045500                                                                          
045600*    -- ÖPPNA FILEN FÖR SKRIVNING                                         
045700     CALL WFILWRT             USING OUTFILE                               
045800                                    DYN-OPEN                              
045900                                                                          
046000     IF DYN-OPEN = 'F'                                                    
046100       MOVE SPACE                TO ERROR-TEXT-STR                        
046200       STRING 'OUTPUT FILE ' DELIMITED BY SIZE                            
046300              DYN-DSNAME     DELIMITED BY SPACE                           
046400              DYN-GENMBR     DELIMITED BY SIZE                            
046500              ' COULD NOT BE OPENED.' DELIMITED BY SIZE                   
046600                               INTO ERROR-TEXT-STR                        
046700       PERFORM S99-ABEND                                                  
046800     ELSE                                                                 
046900D      DISPLAY IDPGM 'OUTPUT FILE OPENED  OK'                             
047000       CONTINUE                                                           
047100     END-IF                                                               
047200     .                                                                    
047300                                                                          
047400 S01-WRITE-OUTPUT-RECORD SECTION.                                         
047500                                                                          
047600     IF DYN-RECFM = 'FB'                                                  
047700       MOVE WS-LRECL-NUM         TO DYN-LRECL                             
047800     ELSE                                                                 
047900       MOVE UT-RECLEN            TO DYN-LRECL                             
048000     END-IF                                                               
048100                                                                          
048200     CALL WFILWRT             USING OUTFILE                               
048300                                    DYN-WRITE                             
048400                                    DYN-RECFM                             
048500                                    DYN-LRECL                             
048600                                    UT-AREA                               
048700                                                                          
048800     IF DYN-WRITE  = 'F'                                                  
048900       MOVE  'ERROR WHEN WRITING DATA TO OUTPUT FILE'                     
049000                                 TO ERROR-TEXT-STR                        
049100       PERFORM S99-ABEND                                                  
049200     ELSE                                                                 
049300       ADD 1                     TO W-RECORD-COUNT                        
049400       MOVE 'UTDATA'             TO POSTSUM-FDNAMN                        
049500       MOVE 'FILDD1  '           TO POSTSUM-DDNAMN2                       
049600       MOVE SPACE                TO POSTSUM-TRANSTYP                      
049700       CALL POSTSUM           USING POSTSUM-PARM                          
049800     END-IF                                                               
049900     .                                                                    
050000                                                                          
050100 S01-CLOSE-OUTPUT-FILE   SECTION.                                         
050200                                                                          
050300     CALL WFILWRT             USING OUTFILE                               
050400                                    DYN-CLOSE                             
050500     IF DYN-CLOSE = 'F'                                                   
050600       MOVE  'ERROR WHEN CLOSING OUTPUT FILE'                             
050700                                 TO ERROR-TEXT-STR                        
050800       PERFORM S99-ABEND                                                  
050900     ELSE                                                                 
051000       DISPLAY IDPGM W-RECORD-COUNT ' RECS WRITTEN TO OUTPUT FILE'        
051100     END-IF                                                               
051200     .                                                                    
051300                                                                          
051400 S02-GET-DSINFO SECTION.                                                  
051500                                                                          
051600     MOVE DYN-DDNAME             TO DSINFO-DDNAME                         
051700     CALL WDSINFO             USING DSINFO-WDSAREA                        
051800     IF DSINFO-KDSVAR-OK                                                  
051900       MOVE DSINFO-DSNAME        TO OUTQ-PHYSICALID                       
052000     ELSE                                                                 
052100       DISPLAY IDPGM 'DDNAME NOT AVAILABLE'                               
052200       MOVE 'NOT AVAILABLE'      TO OUTQ-PHYSICALID                       
052300     END-IF                                                               
052400     .                                                                    
052500                                                                          
052600 S04-OUTQ-OPEN  SECTION.                                                  
052700                                                                          
052800     MOVE 'OPEN'                 TO OUTQ-KDFUNC                           
052900     MOVE WS-ADDISPABS           TO OUTQ-ADDISPABS                        
053000     MOVE ZERO                   TO OUTQ-KVDLEN                           
053100     CALL WZ11OUTQ            USING OUTQ-AREA                             
053200                                                                          
053300     IF OUTQ-KDRC > 0                                                     
053400       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
053500       STRING 'WZ11OUTQ OPEN ERROR RC = ' KDRC-DISPLAY                    
053600         DELIMITED BY SIZE     INTO ERROR-TEXT                            
053700       PERFORM S99-ABEND                                                  
053800     END-IF                                                               
053900     .                                                                    
054000                                                                          
054100 S04-GET-MESSAGE SECTION.                                                 
054200*    -- GET THE MESSAGE                                                   
054300     MOVE 'GET'                  TO OUTQ-KDFUNC                           
054400     COMPUTE OUTQ-KVDLEN = WS-LRECL-NUM - WS-RECKEY-TOTAL                 
054500     CALL WZ11OUTQ            USING OUTQ-AREA                             
054600                                                                          
054700     IF OUTQ-KDRC > 1                                                     
054800       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
054900       STRING 'WZ11OUTQ GET ERROR RC = ' KDRC-DISPLAY                     
055000         DELIMITED BY SIZE     INTO ERROR-TEXT                            
055100       PERFORM S99-ABEND                                                  
055200     END-IF                                                               
055300                                                                          
055400D    DISPLAY IDPGM 'MQGET   SUCCESSFUL   '                                
055500     .                                                                    
055600                                                                          
055700 S04-INQ-PROP SECTION.                                                    
055800                                                                          
055900     MOVE 'INQPROP'              TO OUTQ-KDFUNC                           
056000     MOVE ZERO                   TO OUTQ-KVDLEN                           
056100     CALL WZ11OUTQ            USING OUTQ-AREA                             
056200                                                                          
056300     IF OUTQ-KDRC > 0                                                     
056400       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
056500       STRING 'WZ11OUTQ INQPROP ERROR RC = ' KDRC-DISPLAY                 
056600         DELIMITED BY SIZE     INTO ERROR-TEXT                            
056700       PERFORM S99-ABEND                                                  
056800     ELSE                                                                 
056900D      DISPLAY IDPGM OUTQ-PROPERTY-NAME '=' OUTQ-PROPERTY-VALUE           
057000       CONTINUE                                                           
057100     END-IF                                                               
057200     .                                                                    
057300 S04-OUTQ-CLOSE  SECTION.                                                 
057400                                                                          
057500     MOVE 'CLOSE'                TO OUTQ-KDFUNC                           
057600     MOVE ZERO                   TO OUTQ-KVDLEN                           
057700     CALL WZ11OUTQ            USING OUTQ-AREA                             
057800                                                                          
057900     IF OUTQ-KDRC > 0                                                     
058000       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
058100       STRING 'WZ11OUTQ CLOSE ERROR RC = ' KDRC-DISPLAY                   
058200         DELIMITED BY SIZE     INTO ERROR-TEXT                            
058300       PERFORM S99-ABEND                                                  
058400     END-IF                                                               
058500                                                                          
058600D    DISPLAY IDPGM 'MQCLOSE SUCCESSFUL   '                                
058700     .                                                                    
058800 S99-ABEND SECTION.                                                       
058900                                                                          
059000     DISPLAY IDPGM ' ' ERROR-TEXT                                         
059100     CALL ABEND               USING RKOD-ABEND-NO-DUMP                    
059200     .                                                                    
059300                                                                          
