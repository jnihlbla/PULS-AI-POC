000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4832100.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   10/03/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        TO EXTRACT INVOICE AND CASE DETAILS FOR PREPARING                
000900*        TRANSPORTATION REPORT                                            
001000*                                                                         
001100*        THE PROGRAM READS     WDE6                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- LAST WEEKS INVOICED CASES                                  
002600     SELECT W48321                     ASSIGN TO W48321D1.                
002700*          --- CROSS DOCK CASES WITH STATUS 9                             
002800     SELECT W48322                     ASSIGN TO W48321D2.                
002900*          --- CROSS DOCK CASES WITH STATUS OTHER THAN 9                  
003000     SELECT W48323                     ASSIGN TO W48321D3.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W48321                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  RECORD -COPY W4832101 -PRE  UT-  -L.                                 
004100     EJECT                                                                
004200 FD  W48322                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  RECORD -COPY W4832102 -PRE  UT1-  -L.                                
004700     EJECT                                                                
004800 FD  W48323                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  RECORD -COPY W4832102 -PRE  UT2-  -L.                                
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W4832100'.            
005700 77  YES                         PIC X       VALUE 'J'.                   
005800 77  NOO                         PIC X       VALUE 'N'.                   
005900 77  IX                          PIC S9(3) COMP-3 VALUE 1.                
       77  TAB                         PIC X       VALUE X'05'.                 
006000     EJECT                                                                
006100 01  WS-FOM                      PIC 9(6)    VALUE ZERO.                  
006200 01  WS-TOM                      PIC 9(6)    VALUE ZERO.                  
006300*01  WS-TIVV                     PIC 9(2)    VALUE ZERO.                  
006400 01  WS-FOM-AAMMDD               PIC 9(6)    VALUE ZERO.                  
006500 01  WS-TOM-AAMMDD               PIC 9(6)    VALUE ZERO.                  
006600*                                                                         
006700 01  WS-TIAAVVD                  PIC 9(5).                                
006800 01  WS-TIAAVVD-GRP REDEFINES WS-TIAAVVD.                                 
006900     05 WS-TIAAVV-GRP.                                                    
007000        07 WS-TIAA-VECKA         PIC 9(2).                                
007100        07 WS-TIVV               PIC 9(2).                                
007200     05 WS-TID                   PIC 9.                                   
007300*                                                                         
007400 01  WS-TISKEPPN                 PIC 9(6)    VALUE ZERO.                  
007400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007500 01  FILLER REDEFINES TODAYS-DATE.                                        
007600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007800     03  TODAYS-DATE-DAY         PIC 9(2).                                
007900     EJECT                                                                
       01 WS-DATE.                                                              
         03 WS-TODAYS-DATE               PIC 9(8)  VALUE ZERO.                  
         03 WS-YESTERDAY-AAAAMMDD        PIC 9(8)  VALUE ZERO.                  
         03 FILLER REDEFINES WS-YESTERDAY-AAAAMMDD.                             
             05 WS-YESTERDAY-AA         PIC 9(2).                               
             05 WS-YESTERDAY-YYMMDD     PIC 9(6).                               
           EJECT                                                                
008000 01  GENERAL-SUBPROGRAMS.                                                 
008100*                                                                         
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008700     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
008700     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
008800     SKIP2                                                                
008900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
009000                                                                          
009100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009400     SKIP2                                                                
009500 01  ERROR-TEXT.                                                          
009600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
009700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009800     EJECT                                                                
      *    -- SUBPROGRAM WZ20DAYS                                               
       01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
      *01 -COPY WZ20DAYS                                                        
                                                                                
           EJECT                                                                
009900*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
010000*01  -COPY WORKAREA                                                       
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL POSTSUM                                          
010300*                                                                         
010400*01  -COPY W0005   -PRE  POSTSUM-                                         
010500     EJECT                                                                
010600*01  -COPY WDATAREA                                                       
010700     EJECT                                                                
010800 01  UT-AREA-START               PIC X(24)   VALUE                        
010900                                 'UT-AREA-START  '.                       
011000     SKIP2                                                                
011100                                                                          
011200*01  AREA -COPY W4832101     -PRE UT-                                     
011300                                                                          
011400 01  UT1-AREA-START               PIC X(24)   VALUE                       
011500                                 'UT1-AREA-START  '.                      
011600     SKIP2                                                                
011700                                                                          
011800*01  AREA -COPY W4832102     -PRE UT1-                                    
011900                                                                          
012000 01  UT2-AREA-START               PIC X(24)   VALUE                       
012100                                 'UT2-AREA-START  '.                      
012200     SKIP2                                                                
012300                                                                          
012400*01  AREA -COPY W4832102     -PRE UT2-                                    
012500                                                                          
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FOUND                       VALUE '  '.                  
012900     88  SEGMENT-MISSING                     VALUE 'GB'.                  
013000     SKIP2                                                                
013100 01  GOOD-STATUSCODES.                                                    
013200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(64).                               
013500 01  SSA2                        PIC X(64).                               
013600     EJECT                                                                
013700*    --- IMS FUNCTION CODES                                               
013800*01  -COPY W0003                                                          
013900     EJECT                                                                
014000*    ---  DLI INPUT-OUTPUT AREA                                           
014100 01  DLI-IO-AREA.                                                         
014200     03  IO-AREA                 PIC X(335) VALUE SPACE.                  
014300     03  WDE601 REDEFINES IO-AREA.                                        
014400*        05  -COPY WDE601                                                 
014500     03  WD6411 REDEFINES IO-AREA.                                        
014600*        05  -COPY WDE611                                                 
014700     03  WDE621 REDEFINES IO-AREA.                                        
014800*        05  -COPY WDE621                                                 
014900*                                                                         
015000 LINKAGE SECTION.                                                         
015100*01  -COPY W0008  -PRE WDE6-                                              
015200     05  FILLER                  PIC X.                                   
015300*                                                                         
015400 PROCEDURE DIVISION  USING WDE6-PCB.                                      
015500 MAIN SECTION.                                                            
015600     ENTRY 'DLITCBL' USING WDE6-PCB.                                      
015700                                                                          
015800     PERFORM A-INIT                                                       
015900                                                                          
016000     PERFORM IMS-GN-WDE6                                                  
016100     PERFORM UNTIL SEGMENT-MISSING                                        
016200       EVALUATE WDE6-SEG-NAME-FB                                          
016300         WHEN 'WDE601'                                                    
016400           MOVE VORD-IDPRODNR          TO UT-IDPRODNR                     
016500           MOVE VORD-IDDISTR           TO UT-IDDISTR                      
016600           MOVE VORD-IDKUNDNR          TO UT-IDKUNDNR                     
016700           MOVE VORD-KDORDKL           TO UT-KDORDKL                      
016800           MOVE VORD-FLDIRLEV          TO UT-FLDIRLEV                     
016900           MOVE VORD-IDDC              TO UT-IDDC                         
017000           MOVE VORD-KDFAKTYP          TO UT-KDFAKTYP                     
017100           MOVE VORD-KDFRAKT           TO UT-KDFRAKT                      
017200           MOVE VORD-TIUTSTID          TO UT-TIUTSTID                     
017300           MOVE VORD-TIUTSKR           TO UT-TIUTSKR                      
017400           MOVE VORD-IDLEVNR           TO UT-IDLEVNR                      
017500           MOVE VORD-KDVIA             TO UT-KDVIA                        
017600         WHEN 'WDE611'                                                    
017700           IF KOLLI-TIFAKT >= WS-FOM-AAMMDD AND                           
017800              KOLLI-TIFAKT <= WS-TOM-AAMMDD                               
017900             MOVE KOLLI-IDKOLLI          TO UT-IDKOLLI                    
018000             MOVE KOLLI-IDTRPTNR         TO UT-IDTRPTNR                   
018100             MOVE KOLLI-DIKOLLIL         TO UT-DIKOLLIL                   
018200             MOVE KOLLI-DIKOLLIB         TO UT-DIKOLLIB                   
018300             MOVE KOLLI-DIKOLLIH         TO UT-DIKOLLIH                   
018400             MOVE KOLLI-IDFAKT           TO UT-IDFAKT                     
018500             MOVE KOLLI-IDLBBET          TO UT-IDLBBET                    
018600             MOVE KOLLI-KVFALRAD         TO UT-KVFALRAD                   
018700             MOVE KOLLI-KDKOLLI          TO UT-KDKOLLI                    
018800             MOVE KOLLI-KVFLAMP-KOLLI    TO UT-KVFLAMP-KOLLI              
018900             MOVE KOLLI-KVORDRAD         TO UT-KVORDRAD                   
019000             MOVE KOLLI-SUORDV-KOLLI     TO UT-SUORDV-KOLLI               
019100             MOVE KOLLI-TIFAKT           TO UT-TIFAKT                     
019200             MOVE KOLLI-TIFAKTID         TO UT-TIFAKTID                   
019300             MOVE KOLLI-TILASTN          TO UT-TILASTN                    
019400             MOVE KOLLI-TILASTID         TO UT-TILASTID                   
019500             MOVE KOLLI-VKORDNTO-KOLLI   TO UT-VKORDNTO-KOLLI             
019600             MOVE KOLLI-KDFARLIG-KOLLI   TO UT-KDFARLIG-KOLLI             
019700             MOVE KOLLI-VKORDBTO-KOLLI   TO UT-VKORDBTO-KOLLI             
019800             MOVE KOLLI-VLORDBTO-KOLLI   TO UT-VLORDBTO-KOLLI             
019900             MOVE KOLLI-IDLASTN          TO UT-IDLASTN                    
020000             MOVE 1 TO IX                                                 
020100             PERFORM UNTIL IX > 10                                        
020200               MOVE KOLLI-IDPSN    (IX)  TO UT-IDPSN    (IX)              
020300               MOVE KOLLI-VKART-FG (IX)  TO UT-VKART-FG (IX)              
020400               MOVE KOLLI-VLFG     (IX)  TO UT-VLFG     (IX)              
020500               ADD +1 TO IX                                               
020600             END-PERFORM                                                  
020700             MOVE KOLLI-SUEQFG           TO UT-SUEQFG                     
020800             MOVE KOLLI-DASUPREF         TO UT-DASUPREF                   
020900             MOVE KOLLI-TISUPTID         TO UT-TISUPTID                   
021000             MOVE KOLLI-IDSHIPM          TO UT-IDSHIPM                    
021100             PERFORM S11-WRITE-W48321                                     
021200           END-IF                                                         
021300         WHEN 'WDE621'                                                    
021400             IF CROSS-KDKOLSTA-CROSS = 9                                  
                    MOVE CROSS-TISKEPPN TO WS-TISKEPPN                          
                    IF WS-YESTERDAY-YYMMDD = WS-TISKEPPN                        
021500               MOVE CROSS-IDDC-CROSS     TO UT1-IDDC-CROSS                
021600               MOVE CROSS-TIRFSDAT       TO UT1-TIRFSDAT                  
021700               MOVE CROSS-IDDISTR        TO UT1-IDDISTR                   
021800               MOVE CROSS-IDKUNDNR       TO UT1-IDKUNDNR                  
021900               MOVE CROSS-IDPRODNR       TO UT1-IDPRODNR                  
022000               MOVE CROSS-IDKOLLI        TO UT1-IDKOLLI                   
022100               MOVE CROSS-IDLEVNR        TO UT1-IDLEVNR                   
022200               MOVE CROSS-IDSUPREF       TO UT1-IDSUPREF                  
022300               MOVE CROSS-IDTRPTNR-CROSS TO UT1-IDTRPTNR-CROSS            
022400               MOVE CROSS-TIRECXDAT      TO UT1-TIRECXDAT                 
022500               MOVE CROSS-TIRECXTID      TO UT1-TIRECXTID                 
022600               MOVE CROSS-IDDC-SEND      TO UT1-IDDC-SEND                 
022700               MOVE CROSS-KDKOLSTA-CROSS TO UT1-KDKOLSTA-CROSS            
022700               MOVE CROSS-TISKEPPN       TO UT1-TISKEPPN                  
022700               MOVE CROSS-IDSHIPM-CROSS  TO UT1-IDSHIPM-CROSS             
022700               MOVE CROSS-IDLBBET-CROSS  TO UT1-IDLBBET-CROSS             
022800               PERFORM S12-WRITE-W48322                                   
                    END-IF                                                      
022900             ELSE                                                         
023000              MOVE CROSS-IDDC-CROSS     TO UT2-IDDC-CROSS                 
023100              MOVE CROSS-TIRFSDAT       TO UT2-TIRFSDAT                   
023200              MOVE CROSS-IDDISTR        TO UT2-IDDISTR                    
023300              MOVE CROSS-IDKUNDNR       TO UT2-IDKUNDNR                   
023400              MOVE CROSS-IDPRODNR       TO UT2-IDPRODNR                   
023500              MOVE CROSS-IDKOLLI        TO UT2-IDKOLLI                    
023600              MOVE CROSS-IDLEVNR        TO UT2-IDLEVNR                    
023700              MOVE CROSS-IDSUPREF       TO UT2-IDSUPREF                   
023800              MOVE CROSS-IDTRPTNR-CROSS TO UT2-IDTRPTNR-CROSS             
023900              MOVE CROSS-TIRECXDAT      TO UT2-TIRECXDAT                  
024000              MOVE CROSS-TIRECXTID      TO UT2-TIRECXTID                  
024100              MOVE CROSS-IDDC-SEND      TO UT2-IDDC-SEND                  
024200              MOVE CROSS-KDKOLSTA-CROSS TO UT2-KDKOLSTA-CROSS             
022700              MOVE CROSS-TISKEPPN       TO UT2-TISKEPPN                   
022700              MOVE CROSS-IDSHIPM-CROSS  TO UT2-IDSHIPM-CROSS              
022700              MOVE CROSS-IDLBBET-CROSS  TO UT2-IDLBBET-CROSS              
024300              PERFORM S13-WRITE-W48323                                    
024400             END-IF                                                       
024500       END-EVALUATE                                                       
024600       PERFORM IMS-GN-WDE6                                                
024700     END-PERFORM                                                          
024800     PERFORM Z-FINIT                                                      
024900                                                                          
025000     MOVE ZERO TO RETURN-CODE                                             
025100     GOBACK                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 A-INIT SECTION.                                                          
025500     OPEN OUTPUT W48321                                                   
025600     OPEN OUTPUT W48322                                                   
025700     OPEN OUTPUT W48323                                                   
025800                                                                          
025900     ACCEPT  TODAYS-DATE FROM DATE                                        
           MOVE TAB  TO UT1-TAB-1 UT1-TAB-2 UT1-TAB-3                           
                        UT1-TAB-4 UT1-TAB-5 UT1-TAB-6                           
                        UT1-TAB-7 UT1-TAB-8 UT1-TAB-9                           
                        UT1-TAB-10 UT1-TAB-11 UT1-TAB-12                        
                        UT1-TAB-13 UT2-TAB-1 UT2-TAB-2 UT2-TAB-3                
                        UT2-TAB-4 UT2-TAB-5 UT2-TAB-6                           
                        UT2-TAB-7 UT2-TAB-8 UT2-TAB-9                           
                        UT2-TAB-10 UT2-TAB-11 UT2-TAB-12                        
                        UT2-TAB-13 UT1-TAB-14 UT2-TAB-14                        
                        UT2-TAB-15 UT1-TAB-15                                   
026000     MOVE ZERO        TO WORK-IDDC                                        
026100     MOVE TODAYS-DATE TO WORK-TIAAMMDD-TOM                                
026200     MOVE 006         TO WORK-KVWORKD                                     
026300     MOVE 003         TO WORK-KDCALL                                      
026400     CALL WORKDAY USING  WORK-KDCALL                                      
026500                         WORK-DATE-AREA                                   
026600                         WORK-KDSVAR                                      
026700     IF WORK-KDSVAR-FEL                                                   
026800       MOVE 'ERROR FROM WORKDAY IN SECTION INIT' TO                       
026900                                   ERROR-TEXT-STR                         
027000       CALL ABEND USING RKOD-ABEND                                        
027100     ELSE                                                                 
027200       MOVE WORK-TIAAMMDD-FOM TO WS-FOM                                   
027300       MOVE WORK-TIAAMMDD-TOM TO WS-TOM                                   
027400     END-IF                                                               
027500                                                                          
027600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
027700     MOVE WS-FOM   TO DAT-I-TIDATUM                                       
027800                                                                          
027900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
028000                     DAT-O-TIDATUM DAT-KDSVAR                             
028100                                                                          
028200     IF DAT-KDSVAR-OK                                                     
028300       MOVE 01             TO WS-TID                                      
028400       MOVE DAT-TIAAVV-GRP TO WS-TIAAVV-GRP                               
028500       MOVE WS-TIAAVVD     TO DAT-I-TIDATUM                               
028600       MOVE 'AAVVD'        TO DAT-KDDATFORM                               
028700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
028800                           DAT-O-TIDATUM DAT-KDSVAR                       
028900       IF DAT-KDSVAR-OK                                                   
029000         MOVE DAT-TIAAMMDD   TO WS-FOM-AAMMDD                             
029100         DISPLAY 'WS-FOM-AAMMDD:-->' WS-FOM-AAMMDD                        
029200         MOVE 07             TO WS-TID                                    
029300         MOVE DAT-TIAAVV-GRP TO WS-TIAAVV-GRP                             
029400         MOVE WS-TIAAVVD     TO DAT-I-TIDATUM                             
029500         MOVE 'AAVVD'        TO DAT-KDDATFORM                             
029600         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
029700                             DAT-O-TIDATUM DAT-KDSVAR                     
029800         IF DAT-KDSVAR-OK                                                 
029900           MOVE DAT-TIAAMMDD TO WS-TOM-AAMMDD                             
030000           DISPLAY 'WS-TOM-AAMMDD:-->' WS-TOM-AAMMDD                      
030100         ELSE                                                             
030200           MOVE 'INVALID DATE FROM WDATKONV' TO ERROR-TEXT-STR            
030300           CALL ABEND USING RKOD-ABEND                                    
030400         END-IF                                                           
030500       ELSE                                                               
030600         MOVE 'INVALID DATE FROM WDATKONV' TO ERROR-TEXT-STR              
030700         CALL ABEND USING RKOD-ABEND                                      
030800       END-IF                                                             
030900     ELSE                                                                 
031000       MOVE 'INVALID DATE FROM WDATKONV' TO ERROR-TEXT-STR                
031100       CALL ABEND USING RKOD-ABEND                                        
031200     END-IF                                                               
           PERFORM AA-GET-YESTERDAY-DATE                                        
031300                                                                          
031400     .                                                                    
031500     EJECT                                                                
       AA-GET-YESTERDAY-DATE SECTION.                                           
                                                                                
           MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-TODAYS-DATE                   
           MOVE WS-TODAYS-DATE TO DAYS-TIDATE1                                  
           MOVE 'YYYYMMDD'       TO DAYS-KDDATFMT1                              
           MOVE 'YYYYMMDD'       TO DAYS-KDDATFMT2                              
           MOVE SPACE            TO DAYS-TIDATE2                                
                                    DAYS-IDCALEND                               
           MOVE -1               TO DAYS-KVDAYS                                 
                                                                                
           CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
                                                                                
           MOVE DAYS-TIDATE2(1:8) TO WS-YESTERDAY-AAAAMMDD                      
                                                                                
           .                                                                    
           EJECT                                                                
031600 Z-FINIT SECTION.                                                         
031700     CLOSE W48321                                                         
031800     MOVE 'S' TO POSTSUM-OPKOD                                            
031900     CALL POSTSUM USING POSTSUM-PARM                                      
032000     .                                                                    
032100     EJECT                                                                
032200 S11-WRITE-W48321 SECTION.                                                
032300     WRITE UT-RECORD FROM UT-AREA                                         
032400     MOVE 'W48321'   TO POSTSUM-FDNAMN                                    
032500     MOVE 'W48321D1' TO POSTSUM-DDNAMN2                                   
032600     CALL POSTSUM USING POSTSUM-PARM                                      
032700     .                                                                    
032800     EJECT                                                                
032900 S12-WRITE-W48322 SECTION.                                                
033000     WRITE UT1-RECORD FROM UT1-AREA                                       
033100     MOVE 'W48322'   TO POSTSUM-FDNAMN                                    
033200     MOVE 'W48322D1' TO POSTSUM-DDNAMN2                                   
033300     CALL POSTSUM USING POSTSUM-PARM                                      
033400     .                                                                    
033500     EJECT                                                                
033600 S13-WRITE-W48323 SECTION.                                                
033700     WRITE UT2-RECORD FROM UT2-AREA                                       
033800     MOVE 'W48323'   TO POSTSUM-FDNAMN                                    
033900     MOVE 'W48323D1' TO POSTSUM-DDNAMN2                                   
034000     CALL POSTSUM USING POSTSUM-PARM                                      
034100     .                                                                    
034200     EJECT                                                                
034300 S99-ABEND SECTION.                                                       
034400     MOVE 'S' TO POSTSUM-OPKOD                                            
034500     CALL POSTSUM USING POSTSUM-PARM                                      
034600     CALL ABEND USING RKOD-ABEND                                          
034700     .                                                                    
034800     EJECT                                                                
034900* --- IMS SECTIONS  ---                                                   
035000 IMS-GN-WDE6   SECTION.                                                   
035100     CALL CBLTDLI USING GN WDE6-PCB DLI-IO-AREA                           
035200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
035300     MOVE '  GAGKGBGA' TO GOOD-STATUSCODES                                
035400     PERFORM IMS-STATUSCHECK                                              
035500     .                                                                    
035600     EJECT                                                                
035700 IMS-STATUSCHECK SECTION.                                                 
035800     SET STATUS-IX TO 1                                                   
035900     SEARCH GOOD-STATUS                                                   
036000       AT END                                                             
036100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036200           DELIMITED BY SIZE INTO ERROR-TEXT                              
036300         DISPLAY ERROR-TEXT                                               
036400         CALL FELLOG                                                      
036500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
036600         CONTINUE                                                         
036700     END-SEARCH                                                           
036800     .                                                                    
