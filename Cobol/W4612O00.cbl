000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4612O00.                                                
000300 AUTHOR.         BOHLIN HÅKAN.                                            
000400 DATE-WRITTEN.   24/06/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        UPDATE MX FILES WHICH SHOULD BE SENT TO VIPS                     
000900*        WITH TRACKING ID INFO.                                           
001000*        AFFECTED RECORDTYPE IS RKC (CREDIT TRANSACTIONS                  
001100*        ON LINE LEVEL).                                                  
001200*                                                                         
001300*        THE PROGRAM READS     WDA2                                       
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- MX FILE TO VIPS CREATED IN W46120 PROGRAM                  
002800     SELECT W4612M                     ASSIGN TO W4612OD1.                
002900     SKIP2                                                                
003000*          --- MX FILE TO VIPS ADDED WITH TRACKING ID INFO                
003100     SELECT W46121                     ASSIGN TO W4612OD2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W4612M                                                               
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100 01  INPOST          PIC X(211).                                          
004200     SKIP2                                                                
004300 FD  W46121                                                               
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600 01  UTPOST                       PIC X(80).                              
004700     SKIP2                                                                
004800 01  RID-POST      -COPY W461RIDN     -L.                                 
004900     SKIP2                                                                
005000 01  RIE-POST      -COPY W461RIEN     -L.                                 
005100     SKIP2                                                                
005200 01  RIH-POST      -COPY W461RIHN     -L.                                 
005300     SKIP2                                                                
005400 01  RIO-POST      -COPY W461RIO2     -L.                                 
005500     SKIP2                                                                
005600 01  RKB-POST      -COPY W461RKBN     -L.                                 
005700     SKIP2                                                                
005800 01  RKC-POST      -COPY W461RKCN     -L.                                 
005900     SKIP2                                                                
006000 01  RKD-POST      -COPY W461RKDN     -L.                                 
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W4612O00'.            
006500 77  YES                         PIC X       VALUE 'J'.                   
006600 77  NOO                         PIC X       VALUE 'N'.                   
006700                                                                          
006800 77  W4612M-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W4612M                       VALUE 'J'.                   
007000     EJECT                                                                
007100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES TODAYS-DATE.                                        
007300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007500     03  TODAYS-DATE-DAY         PIC 9(2).                                
007600     EJECT                                                                
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     SKIP2                                                                
008400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900     SKIP2                                                                
009000 01  ERROR-TEXT.                                                          
009100     03  FILLER                  PIC X(11)   VALUE 'ERROR-TEXT:'.         
009200     03  ERROR-TEXT-STR          PIC X(69)   VALUE SPACE.                 
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL POSTSUM                                          
009500*                                                                         
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800 01  IN-AREA-START               PIC X(24)   VALUE                        
009900                                 'IN-AREA-START  '.                       
010000     SKIP2                                                                
010100 01  IN-AREA.                                                             
010200     03  IN-AREA-X.                                                       
010300       05  IN-IDPTYP               PIC X(3).                              
010400       05  FILLER                  PIC X(208).                            
010500*    03  FILLER -COPY W461RKBN  -PRE IN-  -RED   IN-AREA-X                
010600*    03  FILLER -COPY W461RKCN  -PRE IN-  -RED   IN-AREA-X                
010700     EJECT                                                                
010800*    --- AREAS FOR IMS-SECTIONS                                           
010900*                                                                         
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011200 01  KEYS-FOR-DLI.                                                        
011300     03  W-WDA201KY-X.                                                    
011400         05  W-IDDISTR           PIC S9(5)    VALUE ZERO  COMP-3.         
011500         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO  COMP-3.         
011600         05  W-IDRAPPNR          PIC  9(7)    VALUE ZERO.                 
011700     03  W-WDA211KY-X.                                                    
011800         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
011900         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
012000     03  W-IDTRACK-X.                                                     
012100         05  W-IDTRACK           PIC X(25)    VALUE SPACE.                
012200     SKIP2                                                                
012300*    --- STATUS-KOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FOUND                       VALUE '  '.                  
012600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012700     88  SEGMENT-END                         VALUE 'GB'.                  
012800     SKIP2                                                                
012900 01  GOOD-STATUSCODES.                                                    
013000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(128).                              
013300 01  SSA2                        PIC X(128).                              
013400     EJECT                                                                
013500*    --- IMS FUNCTION CODES                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900     EJECT                                                                
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
014100 01  DLI-IO-WDA211.                                                       
014200*    03  -COPY WDA211                                                     
014300     EJECT                                                                
014400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA222'.                      
014500 01  DLI-IO-WDA222.                                                       
014600*    03  -COPY WDA222                                                     
014700     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900                                                                          
015000*01  -COPY W0008  -PRE WDA2-                                              
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING WDA2-PCB.                                      
015400 MAIN SECTION.                                                            
015500     ENTRY 'DLITCBL' USING WDA2-PCB.                                      
015600                                                                          
015700                                                                          
015800     PERFORM A-INIT                                                       
015900                                                                          
016000     PERFORM S01-READ-W4612M                                              
016100     PERFORM UNTIL END-OF-W4612M                                          
016200       EVALUATE IN-IDPTYP                                                 
016300         WHEN 'RID'                                                       
016400           PERFORM S11-WRITE-W46121                                       
016500         WHEN 'RIE'                                                       
016600           PERFORM S11-WRITE-W46121                                       
016700         WHEN 'RIH'                                                       
016800           PERFORM S11-WRITE-W46121                                       
016900         WHEN 'RIO'                                                       
017000           PERFORM S11-WRITE-W46121                                       
017100         WHEN 'RKB'                                                       
017200           MOVE IN-RKB-IDDISTR     TO W-IDDISTR                           
017300           MOVE IN-RKB-IDKUNDNR    TO W-IDKUNDNR                          
017400           MOVE IN-RKB-IDRAPPNR    TO W-IDRAPPNR                          
017500           PERFORM S11-WRITE-W46121                                       
017600         WHEN 'RKC'                                                       
017700           PERFORM B-ADD-IDTRACK-INFO                                     
017800         WHEN 'RKD'                                                       
017900           PERFORM S11-WRITE-W46121                                       
018000         WHEN OTHER                                                       
018100           PERFORM S11-WRITE-W46121                                       
018200       END-EVALUATE                                                       
018300       PERFORM S01-READ-W4612M                                            
018400     END-PERFORM                                                          
018500                                                                          
018600                                                                          
018700     PERFORM Z-FINIT                                                      
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT SECTION.                                                          
019400                                                                          
019500     OPEN INPUT  W4612M                                                   
019600                                                                          
019700     OPEN OUTPUT W46121                                                   
019800                                                                          
019900     ACCEPT TODAYS-DATE  FROM DATE                                        
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100     .                                                                    
020200     EJECT                                                                
020300 B-ADD-IDTRACK-INFO SECTION.                                              
020400                                                                          
020500     MOVE IN-RKC-IDARTNR     TO W-IDARTNR                                 
020600     MOVE IN-RKC-IDRADNR     TO W-IDRADNR                                 
020700     PERFORM IMS-GU-WDA211                                                
020800     IF SEGMENT-FOUND                                                     
020900        PERFORM IMS-GNP-WDA222                                            
021000        IF SEGMENT-FOUND                                                  
021100          PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                    
021200            MOVE TRK-IDTRACK(1:15) TO IN-RKC-IDTRACK                      
021300            MOVE TRK-DADATUM       TO IN-RKC-DADATUM                      
021400            MOVE TRK-KVANTMOT      TO IN-RKC-KVKREANT                     
021500            PERFORM S12-WRITE-W46121                                      
021600            PERFORM IMS-GNP-WDA222                                        
021700          END-PERFORM                                                     
021710        ELSE                                                              
021720          PERFORM S11-WRITE-W46121                                        
021730        END-IF                                                            
021800     ELSE                                                                 
021900        PERFORM S11-WRITE-W46121                                          
022000     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 Z-FINIT SECTION.                                                         
023000     CLOSE W4612M                                                         
023100           W46121                                                         
023200     SKIP2                                                                
023300     MOVE 'S' TO POSTSUM-OPKOD                                            
023400     CALL POSTSUM USING POSTSUM-PARM                                      
023500     .                                                                    
023600     EJECT                                                                
023700 S01-READ-W4612M  SECTION.                                                
023800     READ W4612M INTO IN-AREA                                             
023900     AT END                                                               
024000        MOVE HIGH-VALUE TO IN-AREA                                        
024100        SET END-OF-W4612M TO TRUE                                         
024200                                                                          
024300     NOT AT END                                                           
024400        MOVE 'W4612M' TO POSTSUM-FDNAMN                                   
024500        MOVE 'W4612OD1' TO POSTSUM-DDNAMN2                                
024600        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
024700        CALL POSTSUM USING POSTSUM-PARM                                   
024800     END-READ                                                             
024900     .                                                                    
025000     EJECT                                                                
025100 S11-WRITE-W46121  SECTION.                                               
025200     EVALUATE IN-IDPTYP                                                   
025300       WHEN 'RID'                                                         
025400         WRITE RID-POST     FROM INPOST                                   
025500       WHEN 'RIE'                                                         
025600         WRITE RIE-POST     FROM INPOST                                   
025700       WHEN 'RIH'                                                         
025800         WRITE RIH-POST     FROM INPOST                                   
025900       WHEN 'RIO'                                                         
026000         WRITE RIO-POST     FROM INPOST                                   
026100       WHEN 'RKB'                                                         
026200         WRITE RKB-POST     FROM INPOST                                   
026300       WHEN 'RKC'                                                         
026400         WRITE RKC-POST     FROM INPOST                                   
026500       WHEN 'RKD'                                                         
026600         WRITE RKD-POST     FROM INPOST                                   
026700       WHEN OTHER                                                         
026800         WRITE UTPOST       FROM INPOST                                   
026900     END-EVALUATE                                                         
027000                                                                          
027100     MOVE IN-IDPTYP     TO POSTSUM-TRANSTYP                               
027200     MOVE 'W46121'      TO POSTSUM-FDNAMN                                 
027300     MOVE 'W4612OD2'    TO POSTSUM-DDNAMN2                                
027400     CALL POSTSUM      USING POSTSUM-PARM                                 
027500     .                                                                    
027600     EJECT                                                                
027700 S12-WRITE-W46121  SECTION.                                               
027800     WRITE RKC-POST     FROM IN-AREA-X                                    
027900                                                                          
028000     MOVE IN-IDPTYP     TO POSTSUM-TRANSTYP                               
028100     MOVE 'W46121'      TO POSTSUM-FDNAMN                                 
028200     MOVE 'W4612OD2'    TO POSTSUM-DDNAMN2                                
028300     CALL POSTSUM      USING POSTSUM-PARM                                 
028400     .                                                                    
028500     EJECT                                                                
028600 S99-ABEND SECTION.                                                       
028700                                                                          
028800     SKIP2                                                                
028900     MOVE 'S' TO POSTSUM-OPKOD                                            
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     CALL ABEND USING RKOD-ABEND                                          
029200     .                                                                    
029300     EJECT                                                                
029400* --- IMS SECTIONS  ---                                                   
029500                                                                          
029600     EJECT                                                                
029700 IMS-GU-WDA211 SECTION.                                                   
029800                                                                          
029900     STRING 'WDA201  (IDLEVANM =' W-WDA201KY-X ')'                        
030000          DELIMITED BY SIZE INTO SSA1                                     
030100     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
030200          DELIMITED BY SIZE INTO SSA2                                     
030300     MOVE '  GE' TO GOOD-STATUSCODES                                      
030400     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-WDA211 SSA1 SSA2               
030500     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
030600     PERFORM IMS-STATUSCHECK                                              
030700     .                                                                    
030800     EJECT                                                                
030900 IMS-GNP-WDA222 SECTION.                                                  
031000                                                                          
031100     MOVE 'WDA222' TO SSA1                                                
031200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
031300     CALL CBLTDLI USING GNP WDA2-PCB DLI-IO-WDA222 SSA1                   
031400     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSCHECK                                              
031600     .                                                                    
031700     EJECT                                                                
031800 IMS-STATUSCHECK SECTION.                                                 
031900                                                                          
032000     SET STATUS-IX TO 1                                                   
032100     SEARCH GOOD-STATUS                                                   
032200       AT END                                                             
032300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032400           DELIMITED BY SIZE INTO ERROR-TEXT                              
032500         DISPLAY ERROR-TEXT                                               
032600         CALL FELLOG                                                      
032700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
032800         CONTINUE                                                         
032900     END-SEARCH                                                           
033000     .                                                                    
