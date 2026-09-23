000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2368200.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   03/10/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PGM LÄSER WDL2 OCH SKAPAR FIL W23682                             
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDL2                                       
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- UTFIL FRÅN PGM W23682                                      
002500     SELECT W23682                     ASSIGN TO W23682D1.                
002600*          --- UTFIL - INFO ÄTTOT LIST                                    
002700     SELECT W23683                     ASSIGN TO W23682D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W23682                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST -COPY W23682 -PRE  UT-  -L.                                     
003800     EJECT                                                                
003900 FD  W23683                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY W61250 -PRE  UT2- -L.                                     
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W2368200'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
005100 77  WDL221-SW                   PIC X       VALUE 'J'.                   
005200     EJECT                                                                
005300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM.                                       
005500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005800     EJECT                                                                
005900                                                                          
006000 01  OM-AAMMDD                   PIC 9(6).                                
006100 01  FILLER REDEFINES OM-AAMMDD.                                          
006200     03  OM-AA                   PIC 9(2).                                
006300     03  OM-MM                   PIC 9(2).                                
006400     03  OM-DD                   PIC 9(2).                                
006500     EJECT                                                                
006600                                                                          
006700 01  DIN-AAAAMMDDTTMMSSTH        PIC 9(16).                               
006800 01  FILLER REDEFINES DIN-AAAAMMDDTTMMSSTH.                               
006900     03  DIN-SEKEL               PIC 9(2).                                
007000     03  DIN-AAMMDDTTMMSSTH      PIC 9(14).                               
007100     03  FILLER REDEFINES DIN-AAMMDDTTMMSSTH.                             
007200         05  DIN-AAMMDD          PIC 9(6).                                
007300         05  FILLER REDEFINES DIN-AAMMDD.                                 
007400             07 DIN-AA           PIC 9(2).                                
007500             07 DIN-MM           PIC 9(2).                                
007600             07 DIN-DD           PIC 9(2).                                
007700         05  DIN-TTMMSSTH        PIC 9(8).                                
007800                                                                          
007900 01  WS-AA                       PIC 9(2)    VALUE ZERO.                  
008000 01  WS-DATUM                    PIC 9(6)    VALUE ZERO.                  
008100 01  WS-DATUM-2MAAN              PIC 9(6)    VALUE ZERO.                  
008200     EJECT                                                                
008300 01  WS-DAINLEV                  PIC 9(16).                               
008400 01  WS-IDLEVNR                  PIC X(5).                                
008500 01  WS-IDARTNR                  PIC S9(9) COMP-3.                        
008600                                                                          
008700 01  WS-IDLOPNRM                 PIC 9(9).                                
008800 01  FILLER REDEFINES WS-IDLOPNRM.                                        
008900     03  WS-0                    PIC 9.                                   
009000     03  WS-VVD                  PIC 9(3).                                
009100     03  WS-LLLL                 PIC 9(4).                                
009200     03  WS-K                    PIC 9.                                   
009300                                                                          
009400 01  W-IDLOPNRM                  PIC 9(7).                                
009500 01  FILLER REDEFINES W-IDLOPNRM.                                         
009600     03  W-VVD                   PIC 9(3).                                
009700     03  W-LLLL                  PIC 9(4).                                
009800                                                                          
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     SKIP2                                                                
010710*01    -COPY WWDCKONS                                                     
010720     EJECT                                                                
010800*    --- PARAMETRAR TILL ABEND                                            
010900                                                                          
011000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011300     SKIP2                                                                
011400 01  FELTEXT.                                                             
011500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL DATKORT                                          
011900*                                                                         
012000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23682'.              
012100     SKIP2                                                                
012200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012300     SKIP2                                                                
012400*01  -COPY WDATKORT                                                       
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL POSTSUM                                          
012700*                                                                         
012800*01  -COPY W0005   -PRE  POSTSUM-                                         
012900     EJECT                                                                
013000*01  -COPY WDATAREA                                                       
013100     EJECT                                                                
013200 01  UT-AREA-START               PIC X(24)   VALUE                        
013300                                 'UT-AREA-START  '.                       
013400     SKIP2                                                                
013500                                                                          
013600*01  AREA -COPY W23682     -PRE UT-                                       
013700     EJECT                                                                
013710 01  UT-AREA-START2          PIC X(24)   VALUE                            
013720                                 'W23683-AREA-START  '.                   
013750*01  AREA -COPY W61250    -PRE UT2-                                       
013760     EJECT                                                                
013800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013900*                                                                         
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014200     SKIP3                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FINNS                       VALUE '  '.                  
014600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014800     SKIP2                                                                
014900 01  GODK-STATUSKODER.                                                    
015000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(64).                               
015300 01  SSA2                        PIC X(64).                               
015400     EJECT                                                                
015500*    --- IMS FUNKTIONSKODER                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
016000 01  DLI-IO-AREA.                                                         
016100     03  IO-AREA    PIC X(150) VALUE SPACE.                               
016200     SKIP2                                                                
016300     03  WDL201 REDEFINES IO-AREA.                                        
016400*        05  -COPY WDL201                                                 
016500     EJECT                                                                
016600     03  WDL211 REDEFINES IO-AREA.                                        
016700*        05  -COPY WDL211                                                 
016800     EJECT                                                                
016900     03  WDL221 REDEFINES IO-AREA.                                        
017000*        05  -COPY WDL221                                                 
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300                                                                          
017400                                                                          
017500*01  -COPY W0008  -PRE WDL2-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800 PROCEDURE DIVISION  USING WDL2-PCB.                                      
017900 MAIN SECTION.                                                            
018000     ENTRY 'DLITCBL' USING WDL2-PCB.                                      
018100                                                                          
018200                                                                          
018300     PERFORM A-INIT                                                       
018400                                                                          
018500     PERFORM IMS-GET-WDL2                                                 
018600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
018700       EVALUATE WDL2-SEG-NAME-FB                                          
018800         WHEN 'WDL201'                                                    
018900            IF SKRIV-SW = JA                                              
019000              PERFORM S01-SKRIV-POST                                      
019100              MOVE NEJ TO SKRIV-SW                                        
019200            END-IF                                                        
019300            MOVE ART-IDARTNR TO WS-IDARTNR                                
019400         WHEN 'WDL211'                                                    
019500            IF SKRIV-SW = JA                                              
019600              PERFORM S01-SKRIV-POST                                      
019700              MOVE NEJ TO SKRIV-SW                                        
019800              MOVE NEJ TO WDL221-SW                                       
019900            END-IF                                                        
020000            MOVE JA TO WDL221-SW                                          
020100            MOVE INL-DAINLEV TO DIN-AAAAMMDDTTMMSSTH                      
020200            COMPUTE WS-DATUM = 999999 - DIN-AAMMDD                        
020300              IF WS-DATUM = DAGENS-DATUM                                  
020400                 MOVE INL-DAINLEV TO WS-DAINLEV                           
020500              ELSE                                                        
020600                 MOVE INL-DAINLEV TO WS-DAINLEV                           
020700                 MOVE NEJ TO WDL221-SW                                    
020800              END-IF                                                      
020900         WHEN 'WDL221'                                                    
021000           IF WDL221-SW = JA OR MOT-TIUPPDAT = DAGENS-DATUM               
021100             IF MOT-IDPTYP = 'R31' OR 'R32'                               
021200                IF MOT-KDRT = 0 OR 6                                      
021300                  MOVE MOT-IDLEVNR TO UT-IDLEVNR                          
021400                  IF MOT-IDPTYP = 'R31'                                   
021500                    MOVE MOT-KVAVIS      TO UT-KVANTMOT                   
021600                    MOVE MOT-IDPTYP      TO UT-IDPTYP                     
021700                  END-IF                                                  
021800                  IF MOT-IDPTYP = 'R32'                                   
021900                    MOVE MOT-KVANTMOT    TO UT-KVANTMOT                   
022000                    MOVE MOT-IDPTYP      TO UT-IDPTYP                     
022100                  END-IF                                                  
022200                  MOVE MOT-KVAVIS   TO UT-KVAVIS                          
022300                  MOVE MOT-TIAVIDAT TO UT-TIAVIDAT                        
022400                  MOVE MOT-IDLOPNRM TO WS-IDLOPNRM                        
022500                  MOVE WS-VVD TO W-VVD                                    
022600                  MOVE WS-LLLL TO W-LLLL                                  
022700                  MOVE W-IDLOPNRM TO UT-IDLOPNRM                          
022800                  MOVE JA TO WDL221-SW                                    
022900                  MOVE JA TO SKRIV-SW                                     
023000***************                                                           
023100                  IF WS-DATUM NOT = DAGENS-DATUM                          
023200                     IF MOT-IDPTYP = 'R32'                                
023300                        IF MOT-KVAVIS = MOT-KVANTMOT                      
023400                           MOVE NEJ TO SKRIV-SW                           
023500                        END-IF                                            
023600                     END-IF                                               
023700                  END-IF                                                  
023800***************                                                           
023900                END-IF                                                    
024000             END-IF                                                       
024100           END-IF                                                         
024110                                                                          
024111           IF MOT-KDRT = 0                                                
024120              MOVE WS-IDARTNR          TO UT2-IDARTNR                     
024130              MOVE WC-CDC-SE           TO UT2-IDDC                        
024140              MOVE +0                  TO UT2-TIBERANK                    
024150              MOVE +0                  TO UT2-TIINLMOT                    
024160              MOVE MOT-TIAVIDAT        TO UT2-TIAVIDAT                    
024170              PERFORM S12-WRITE-W23683                                    
024171           END-IF                                                         
024180                                                                          
024200       END-EVALUATE                                                       
024300       PERFORM IMS-GET-WDL2                                               
024400     END-PERFORM                                                          
024500     IF SKRIV-SW = JA                                                     
024600       PERFORM S01-SKRIV-POST                                             
024700       MOVE NEJ TO SKRIV-SW                                               
024800     END-IF                                                               
024900     PERFORM Z-FINIT                                                      
025000                                                                          
025100     MOVE ZERO TO RETURN-CODE                                             
025200     GOBACK                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 A-INIT SECTION.                                                          
025600                                                                          
025700     OPEN OUTPUT W23682                                                   
025800                 W23683                                                   
025900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
026000     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
026100     MOVE D-MAANAD TO DAGENS-DATUM-MAANAD                                 
026200     MOVE D-DAG    TO DAGENS-DATUM-DAG                                    
026300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
026700 AA-NOLLSTAELL-VARDEN SECTION.                                            
026800     MOVE ZERO   TO UT-KVANTMOT                                           
026900                    UT-TIAVIDAT                                           
027000                    UT-IDLOPNRM                                           
027100                                                                          
027200                                                                          
027300     MOVE SPACE TO  UT-IDLEVNR                                            
027400     .                                                                    
027500     EJECT                                                                
027600     EJECT                                                                
027700 Z-FINIT SECTION.                                                         
027800     CLOSE W23682                                                         
027810           W23683                                                         
027900     SKIP2                                                                
028000     MOVE 'S' TO POSTSUM-OPKOD                                            
028100     CALL POSTSUM USING POSTSUM-PARM                                      
028200     .                                                                    
028300     EJECT                                                                
028400 S01-SKRIV-POST SECTION.                                                  
028500                                                                          
028600     INSPECT UT-IDLEVNR                                                   
028700                  REPLACING ALL SPACE BY ZERO                             
028800     IF UT-IDLEVNR = 'AEFW8' OR 'AD0QT'                                   
028900        PERFORM S11-SKAPA-SKRIV-UTPOST                                    
029000     END-IF                                                               
029100     MOVE NEJ TO SKRIV-SW                                                 
029200     .                                                                    
029300     EJECT                                                                
029400 S11-SKAPA-SKRIV-UTPOST SECTION.                                          
029500                                                                          
029600     IF UT-KVANTMOT > ZERO                                                
029700        MOVE WS-IDARTNR     TO UT-IDARTNR                                 
029800        MOVE WS-DAINLEV     TO UT-DAINLEV                                 
029900        WRITE UT-POST FROM UT-AREA                                        
030000                                                                          
030100        MOVE SPACE     TO POSTSUM-TRANSTYP                                
030200        MOVE 'W23682' TO POSTSUM-FDNAMN                                   
030300        MOVE 'W23682D1' TO POSTSUM-DDNAMN2                                
030400        CALL POSTSUM USING POSTSUM-PARM                                   
030500     END-IF                                                               
030600     PERFORM AA-NOLLSTAELL-VARDEN                                         
030700     .                                                                    
030800     EJECT                                                                
030810 S12-WRITE-W23683       SECTION.                                          
030820                                                                          
030860     WRITE UT2-POST FROM UT2-AREA                                         
030870                                                                          
030880     MOVE SPACE          TO POSTSUM-TRANSTYP                              
030890     MOVE 'W23683'       TO POSTSUM-FDNAMN                                
030891     MOVE 'W23682D2'     TO POSTSUM-DDNAMN2                               
030892     CALL POSTSUM     USING POSTSUM-PARM                                  
030895     .                                                                    
030896     EJECT                                                                
030900 S99-ABEND SECTION.                                                       
031000                                                                          
031100     SKIP2                                                                
031200     MOVE 'S' TO POSTSUM-OPKOD                                            
031300     CALL POSTSUM USING POSTSUM-PARM                                      
031400     CALL ABEND USING RKOD-ABEND                                          
031500     .                                                                    
031600     EJECT                                                                
031700* --- IMS SEKTIONER ---                                                   
031800                                                                          
031900                                                                          
032000 IMS-GET-WDL2   SECTION.                                                  
032100                                                                          
032200     CALL CBLTDLI USING GN WDL2-PCB DLI-IO-AREA                           
032300     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
032400     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
032500     PERFORM IMS-STATUSKONTROLL                                           
032600     .                                                                    
032700     EJECT                                                                
032800 IMS-STATUSKONTROLL SECTION.                                              
032900                                                                          
033000     SET STATUS-IX TO 1                                                   
033100     SEARCH GODK-STATUS                                                   
033200       AT END                                                             
033300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033400           DELIMITED BY SIZE INTO FELTEXT                                 
033500         DISPLAY FELTEXT                                                  
033600         CALL FELLOG                                                      
033700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033800         CONTINUE                                                         
033900     END-SEARCH                                                           
034000     .                                                                    
