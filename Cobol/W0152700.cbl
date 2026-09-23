000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     W0152700.                                                
000600 AUTHOR.         ANDERS HENRIKSSON                                        
000700 DATE-WRITTEN.   JAN 2017.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*   PGM                                                                   
001100*   -SELECTS ROWS FROM YESTERDAYS INVOICING READING                       
001200*      TP0WLOG                                                            
001300*                                                                         
001400*   -CREATES FILE                                                         
001500*                                                                         
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002310*          --- WEB LOG DATA FILE DAY                                      
002320     SELECT W01527                     ASSIGN TO W01527D1.                
002330     EJECT                                                                
002331*          --- WEB LOG DATA FILE TOTAL                                    
002340     SELECT W01527B                    ASSIGN TO W01527D2.                
002350     EJECT                                                                
002360*          --- FIL MED DELETE-DATUM                                       
002370     SELECT W01527C                    ASSIGN TO W01527D3.                
002380     SKIP2                                                                
002390*          --- FIL MED HÖGSTA TIDERNA                                     
002391     SELECT W01527D                    ASSIGN TO W01527D4.                
002392     SKIP2                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002510 FD  W01527                                                               
002520     RECORDING       F                                                    
002530     BLOCK CONTAINS  0.                                                   
002540                                                                          
002550 01  UT-POST.                                                             
002560*    03   -COPY W01526    -L.                                             
002570     EJECT                                                                
002600                                                                          
002610 FD  W01527B                                                              
002620     RECORDING       F                                                    
002630     BLOCK CONTAINS  0.                                                   
002640                                                                          
002650 01  UT2-POST.                                                            
002660*    03   -COPY W01526    -L.                                             
002670     EJECT                                                                
002680                                                                          
002690 FD  W01527C                                                              
002691     RECORDING       F                                                    
002692     BLOCK CONTAINS  0.                                                   
002693                                                                          
002694 01  UT3-POST     PIC X(80).                                              
002695     SKIP3                                                                
002696                                                                          
002697 FD  W01527D                                                              
002698     RECORDING       V                                                    
002699     BLOCK CONTAINS  0.                                                   
002700                                                                          
002701 01  UT4-POST.                                                            
002702*    03   -COPY W01526    -L.                                             
002703     EJECT                                                                
002704                                                                          
002710 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                     PIC X(8)    VALUE 'W0152700'.              
002900                                                                          
003100 01  WS-CURRENT-DATE           PIC X(8)  VALUE SPACE.                     
003200 01  WS-TIREGDAT               PIC 9(6)  VALUE ZERO.                      
003300 01  WS-TIREGTID               PIC 9(6)  VALUE ZERO.                      
003400 01  WS-IDLOPNR                PIC 9(2)  VALUE ZERO.                      
003500 01  WS-KVMILSEC               PIC 9(6)  VALUE ZERO.                      
003600 01  WS-RUNDATUM               PIC X(8)  VALUE SPACE.                     
003610 01  WS3-RUNDATUM              PIC X(8)  VALUE SPACE.                     
003700 01  WS2-RUNDATUM              PIC S9(7) COMP-3 VALUE ZERO.               
003710 01  WS4-RUNDATUM              PIC S9(7) COMP-3 VALUE ZERO.               
003800 77  WS-IX                     PIC S9(9) VALUE +0    COMP-3.              
003900 77  WS-MAX                    PIC S9(9) VALUE +100  COMP-3.              
004000                                                                          
004500     EJECT                                                                
004600                                                                          
004610 01  UT3-AREA-START              PIC X(24)   VALUE                        
004620                                 'UT3-AREA-START  '.                      
004630     SKIP2                                                                
004640 01  FILLER                      PIC X(16) VALUE 'UT3-AREA'.              
004650 01  UT3-AREA.                                                            
004660     03 WS-FILLER1               PIC X(8)  VALUE SPACE.                   
004670     03 WS-DAEXDAT               PIC X(4) VALUE " <  ".                   
004680     03 WS-DELDATUM              PIC X(6)  VALUE SPACE.                   
004690     03 WS-FILLER2               PIC X(62) VALUE " )".                    
004691     EJECT                                                                
004692                                                                          
004700 01  ERRTEXT.                                                             
004800     03  FILLER                PIC X(8)    VALUE 'ERRTEXT'.               
004900     03  ERRTEXT-STR           PIC X(72)   VALUE SPACE.                   
005000 01  KDRC-DISPLAY              PIC Z(5).                                  
005100     EJECT                                                                
005200                                                                          
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400     03  ABEND                 PIC X(8)    VALUE 'ABEND   '.              
005500     03  POSTSUM                 PIC X(8)  VALUE 'POSTSUM'.               
005600     EJECT                                                                
005700                                                                          
005710*01  -COPY W0005   -PRE  POSTSUM-                                         
005720     EJECT                                                                
005730                                                                          
005800 01  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.              
005900     SKIP3                                                                
006000*    -COPY WZ20DAYS                                                       
006100     EJECT                                                                
006200                                                                          
006300*    --- PARAMETRAR TILL ABEND                                            
006400*                                                                         
006500 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
006600 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
006700     EJECT                                                                
006800                                                                          
006900*    --- AREOR FÖR KOMMUNIKATION                                          
007300                                                                          
007400*    --- UTAREA EKONOMIPOST                                               
007500*01  AREA -COPY W01526  -PRE UT-                                          
007501     EJECT                                                                
007502                                                                          
007510*01  AREA2 -COPY W01526  -PRE UT2-                                        
007600     EJECT                                                                
007610                                                                          
007700*01  AREA4 -COPY W01526  -PRE UT4-                                        
007800     EJECT                                                                
008300*                                                                         
008400*        WORK-AREAS FOR DB2-SECTIONS                                      
008500*                                                                         
009300 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
009400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009500*                        **** STATUS-CODE FROM DB2                        
009600                                                                          
009700 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
009800 01  DB2-WS.                                                              
009900   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
010000     88  ROW-FOUND                         VALUE +000.                    
010100     88  ROW-MISSING                       VALUE +100.                    
010200   03  GOOD-SQLCODES.                                                     
010300     05  GOOD-SQLCODE OCCURS 5                                            
010400         INDEXED BY SQLCODE-IX PIC 999.                                   
010500     EJECT                                                                
010501                                                                          
010502 01  FILLER                    PIC X(16)   VALUE 'WLOG-TAB    '.          
010503*01  -COPY TP0WLOG        -PRE WLOG-                                      
010504     EJECT                                                                
010510 01  FILLER                    PIC X(16)   VALUE 'WLOG-AREA'.             
010520       EXEC SQL INCLUDE TP0WLOG END-EXEC.                                 
010530                                                                          
010600                                                                          
010700 LINKAGE SECTION.                                                         
010800*01  -COPY W0009   -PRE MSG-                                              
010900     EJECT                                                                
011000                                                                          
011100 PROCEDURE DIVISION.                                                      
011200 MAIN SECTION.                                                            
011500     PERFORM A-INIT                                                       
011600                                                                          
011700     PERFORM B-EXECUTE                                                    
011710     PERFORM C-SKAPA-DELETE-DATE                                          
011800                                                                          
011900     PERFORM Z-FINISH                                                     
012000     MOVE ZERO TO RETURN-CODE                                             
012100     GOBACK                                                               
012200     .                                                                    
012300     EJECT                                                                
012400                                                                          
012500 A-INIT SECTION.                                                          
012510     DISPLAY 'A-SECTION'                                                  
012600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
012700     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
012800     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
012900     MOVE 10                          TO DAYS-KVDAYS                      
013000     MOVE ' '                         TO DAYS-IDCALEND                    
013100     MOVE SPACE                       TO DAYS-TIDATE1                     
013200     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
013300     CALL WZ20DAYS USING                                                  
013400          DAYS-WZ20DAYS                                                   
013500     IF DAYS-KDRC = ZERO                                                  
013600       MOVE DAYS-TIDATE1              TO WS-RUNDATUM                      
013700       MOVE WS-RUNDATUM(3:6)          TO WS2-RUNDATUM                     
013800     END-IF                                                               
013802                                                                          
013803     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
013804     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
013805     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
013806     MOVE 1                           TO DAYS-KVDAYS                      
013807     MOVE ' '                         TO DAYS-IDCALEND                    
013808     MOVE SPACE                       TO DAYS-TIDATE1                     
013809     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
013810     CALL WZ20DAYS USING                                                  
013811          DAYS-WZ20DAYS                                                   
013812     IF DAYS-KDRC = ZERO                                                  
013813       MOVE DAYS-TIDATE1              TO WS3-RUNDATUM                     
013814       MOVE WS3-RUNDATUM(3:6)         TO WS4-RUNDATUM                     
013815     END-IF                                                               
013816                                                                          
013817     OPEN OUTPUT W01527                                                   
013818     OPEN OUTPUT W01527B                                                  
013819     OPEN OUTPUT W01527C                                                  
013820     OPEN OUTPUT W01527D                                                  
013830     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014100                                                                          
014200 B-EXECUTE SECTION.                                                       
014300     DISPLAY 'B-SECTION'                                                  
015500     MOVE ZERO TO WS-IX                                                   
015600     PERFORM DB2-OPEN-CRS-WLOG                                            
015700     PERFORM DB2-FETCH-CRS-WLOG                                           
015800     PERFORM UNTIL ROW-MISSING                                            
015900       ADD +1 TO WS-IX                                                    
016000       PERFORM BA-BUILD-OUTPUT-WLOG                                       
016100       PERFORM S01-CREATE-FILE                                            
016200       PERFORM DB2-FETCH-CRS-WLOG                                         
016300     END-PERFORM                                                          
016400     PERFORM DB2-CLOSE-CRS-WLOG                                           
016410                                                                          
016500     MOVE ZERO TO WS-IX                                                   
016600     PERFORM DB2-OPEN-CRS-WLOG-TOT                                        
016700     PERFORM DB2-FETCH-CRS-WLOG-TOT                                       
016710     PERFORM UNTIL ROW-MISSING                                            
016720       ADD +1 TO WS-IX                                                    
016730       PERFORM BB-BUILD-OUTPUT-WLOG-TOT                                   
016740       PERFORM S02-CREATE-FILE-TOT                                        
016750       PERFORM DB2-FETCH-CRS-WLOG-TOT                                     
016760     END-PERFORM                                                          
016770     PERFORM DB2-CLOSE-CRS-WLOG-TOT                                       
016780                                                                          
016790     MOVE ZERO TO WS-IX                                                   
016791     PERFORM DB2-OPEN-CRS-WLOG-MAX                                        
016792     PERFORM DB2-FETCH-CRS-WLOG-MAX                                       
016793     PERFORM UNTIL ROW-MISSING OR WS-IX = WS-MAX                          
016794       ADD +1 TO WS-IX                                                    
016795       PERFORM BC-BUILD-OUTPUT-WLOG-MAX                                   
016797       PERFORM S03-CREATE-FILE-MAX                                        
016798       PERFORM DB2-FETCH-CRS-WLOG-MAX                                     
016799     END-PERFORM                                                          
016800     PERFORM DB2-CLOSE-CRS-WLOG-MAX                                       
016810     .                                                                    
016900     EJECT                                                                
017000                                                                          
017100 BA-BUILD-OUTPUT-WLOG          SECTION.                                   
017200     MOVE WLOG-IDDC             TO UT-IDDC                                
017300     MOVE WLOG-TIREGDAT         TO WS-TIREGDAT                            
017400     MOVE WS-TIREGDAT           TO UT-TIREGDAT                            
017500     MOVE WLOG-TIREGTID         TO WS-TIREGTID                            
017600     MOVE WS-TIREGTID           TO UT-TIREGTID                            
017700     MOVE WLOG-IDUSER           TO UT-IDUSER                              
017800     MOVE WLOG-IDLOPNR          TO WS-IDLOPNR                             
017900     MOVE WS-IDLOPNR            TO UT-IDLOPNR                             
018000     MOVE WLOG-KVMILSEC         TO WS-KVMILSEC                            
018100     MOVE WS-KVMILSEC           TO UT-KVMILSEC                            
018200     MOVE WLOG-BEWEBSCR         TO UT-BEWEBSCR                            
018300     MOVE WLOG-BEWEBURL         TO UT-BEWEBURL                            
018400     .                                                                    
018500     EJECT                                                                
018700                                                                          
018710 BB-BUILD-OUTPUT-WLOG-TOT      SECTION.                                   
018720     MOVE WLOG-IDDC             TO UT2-IDDC                               
018730     MOVE WLOG-TIREGDAT         TO WS-TIREGDAT                            
018740     MOVE WS-TIREGDAT           TO UT2-TIREGDAT                           
018750     MOVE WLOG-TIREGTID         TO WS-TIREGTID                            
018760     MOVE WS-TIREGTID           TO UT2-TIREGTID                           
018770     MOVE WLOG-IDUSER           TO UT2-IDUSER                             
018780     MOVE WLOG-IDLOPNR          TO WS-IDLOPNR                             
018790     MOVE WS-IDLOPNR            TO UT2-IDLOPNR                            
018791     MOVE WLOG-KVMILSEC         TO WS-KVMILSEC                            
018792     MOVE WS-KVMILSEC           TO UT2-KVMILSEC                           
018793     MOVE WLOG-BEWEBSCR         TO UT2-BEWEBSCR                           
018794     MOVE WLOG-BEWEBURL         TO UT2-BEWEBURL                           
018795     .                                                                    
018796     EJECT                                                                
018797                                                                          
018798 BC-BUILD-OUTPUT-WLOG-MAX      SECTION.                                   
018799     MOVE WLOG-IDDC             TO UT4-IDDC                               
018800     MOVE WLOG-TIREGDAT         TO WS-TIREGDAT                            
018801     MOVE WS-TIREGDAT           TO UT4-TIREGDAT                           
018802     MOVE WLOG-TIREGTID         TO WS-TIREGTID                            
018803     MOVE WS-TIREGTID           TO UT4-TIREGTID                           
018804     MOVE WLOG-IDUSER           TO UT4-IDUSER                             
018805     MOVE WLOG-IDLOPNR          TO WS-IDLOPNR                             
018806     MOVE WS-IDLOPNR            TO UT4-IDLOPNR                            
018807     MOVE WLOG-KVMILSEC         TO WS-KVMILSEC                            
018808     MOVE WS-KVMILSEC           TO UT4-KVMILSEC                           
018809     MOVE WLOG-BEWEBSCR         TO UT4-BEWEBSCR                           
018810     MOVE WLOG-BEWEBURL         TO UT4-BEWEBURL                           
018811     .                                                                    
018812     EJECT                                                                
018813                                                                          
018814 C-SKAPA-DELETE-DATE SECTION.                                             
018815     DISPLAY 'C-SECTION'                                                  
018816*- TAR FRAM DELETE-DATUM SOM ÄR 6 MÅNADER TIDIGARE ÄN DAGENS DATUM        
018817     MOVE WS-RUNDATUM             TO DAYS-TIDATE2                         
018818     MOVE 'YYYYMMDD'              TO DAYS-KDDATFMT2                       
018820     MOVE 100                     TO DAYS-KVDAYS                          
018830     MOVE SPACE                   TO DAYS-TIDATE1                         
018840     MOVE 'YYYYMMDD'              TO DAYS-KDDATFMT1                       
018850                                                                          
018860     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
018870                                                                          
018880     IF DAYS-KDRC = ZERO                                                  
018890       MOVE DAYS-TIDATE1(3:6)     TO WS-DELDATUM                          
018891     ELSE                                                                 
018892       DISPLAY ' ERROR IN WZ20DAYS ' DAYS-KDRC                            
018893     END-IF                                                               
018894                                                                          
018895     PERFORM S11-SKRIV-W01527C                                            
018896     .                                                                    
018897     EJECT                                                                
018898                                                                          
018900 Z-FINISH SECTION.                                                        
018910     MOVE 'S' TO POSTSUM-OPKOD                                            
018920     CALL POSTSUM USING POSTSUM-PARM                                      
018930     CLOSE W01527                                                         
018940     CLOSE W01527B                                                        
018950     CLOSE W01527C                                                        
018960     CLOSE W01527D                                                        
019000     .                                                                    
019100     EJECT                                                                
024100                                                                          
024101 S01-CREATE-FILE  SECTION.                                                
024102     WRITE UT-POST   FROM UT-AREA                                         
024103                                                                          
024104     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
024105     MOVE 'W01527'   TO POSTSUM-FDNAMN                                    
024106     MOVE 'W01527D1' TO POSTSUM-DDNAMN2                                   
024107     CALL POSTSUM USING POSTSUM-PARM                                      
024108     .                                                                    
024109     EJECT                                                                
024110                                                                          
024120 S02-CREATE-FILE-TOT SECTION.                                             
024130     WRITE UT2-POST   FROM UT2-AREA2                                      
024140                                                                          
024150     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
024160     MOVE 'W01527'   TO POSTSUM-FDNAMN                                    
024170     MOVE 'W01527D2' TO POSTSUM-DDNAMN2                                   
024180     CALL POSTSUM USING POSTSUM-PARM                                      
024190     .                                                                    
024191     EJECT                                                                
024192                                                                          
024193 S03-CREATE-FILE-MAX SECTION.                                             
024194     WRITE UT4-POST   FROM UT4-AREA4                                      
024195                                                                          
024196     MOVE 'UT4-'     TO POSTSUM-TRANSTYP                                  
024197     MOVE 'W01527'   TO POSTSUM-FDNAMN                                    
024198     MOVE 'W01527D4' TO POSTSUM-DDNAMN2                                   
024199     CALL POSTSUM USING POSTSUM-PARM                                      
024200     .                                                                    
024201     EJECT                                                                
024202                                                                          
024203 S11-SKRIV-W01527C SECTION.                                               
024204     WRITE UT3-POST FROM UT3-AREA                                         
024205                                                                          
024206     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
024207     MOVE 'W01527' TO POSTSUM-FDNAMN                                      
024208     MOVE 'W01527D3' TO POSTSUM-DDNAMN2                                   
024209     CALL POSTSUM USING POSTSUM-PARM                                      
024210     .                                                                    
024211     EJECT                                                                
024212                                                                          
024220* --- DB2 SECTIONS  ---                                                   
024300*                                                                         
024400****** WLOG DATA              ********                                    
024500*                                                                         
024630 DB2-OPEN-CRS-WLOG     SECTION.                                           
024700     EXEC SQL DECLARE WLOG-CRS CURSOR FOR                                 
024800     SELECT   IDDC                                                        
024900             ,TIREGDAT                                                    
025000             ,TIREGTID                                                    
025100             ,IDUSER                                                      
025200             ,IDLOPNR                                                     
025300             ,KVMILSEC                                                    
025400             ,BEWEBSCR                                                    
025500             ,BEWEBURL                                                    
025600                                                                          
025700     FROM     TP0WLOG                                                     
025800                                                                          
025900     WHERE    TIREGDAT > :WS2-RUNDATUM                                    
026000                                                                          
026100     ORDER BY TIREGDAT DESC, TIREGTID DESC                                
026200                                                                          
026300     FOR FETCH ONLY                                                       
026400     END-EXEC                                                             
026500                                                                          
026600     MOVE 000100         TO GOOD-SQLCODES                                 
026700     EXEC SQL                                                             
026710       OPEN WLOG-CRS                                                      
026800     END-EXEC                                                             
026900     MOVE SQLCODE        TO SQLCODE-WS                                    
027000     PERFORM DB2-STATUS-CHECK                                             
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 DB2-FETCH-CRS-WLOG     SECTION.                                          
027500     EXEC SQL FETCH WLOG-CRS INTO                                         
027600            :WLOG-IDDC                                                    
027700           ,:WLOG-TIREGDAT                                                
027800           ,:WLOG-TIREGTID                                                
027900           ,:WLOG-IDUSER                                                  
028000           ,:WLOG-IDLOPNR                                                 
028100           ,:WLOG-KVMILSEC                                                
028200           ,:WLOG-BEWEBSCR                                                
028300           ,:WLOG-BEWEBURL                                                
028400     END-EXEC                                                             
028500                                                                          
028600     MOVE 000100         TO GOOD-SQLCODES                                 
028700     MOVE SQLCODE        TO SQLCODE-WS                                    
028800     PERFORM DB2-STATUS-CHECK                                             
028900     .                                                                    
029000     EJECT                                                                
029100                                                                          
029200 DB2-CLOSE-CRS-WLOG     SECTION.                                          
029300     EXEC SQL                                                             
029310       CLOSE WLOG-CRS                                                     
029400     END-EXEC                                                             
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029710 DB2-OPEN-CRS-WLOG-TOT SECTION.                                           
029720     EXEC SQL DECLARE WLOG2-CRS CURSOR FOR                                
029730     SELECT   IDDC                                                        
029740             ,TIREGDAT                                                    
029750             ,TIREGTID                                                    
029760             ,IDUSER                                                      
029770             ,IDLOPNR                                                     
029780             ,KVMILSEC                                                    
029790             ,BEWEBSCR                                                    
029791             ,BEWEBURL                                                    
029792                                                                          
029793     FROM     TP0WLOG                                                     
029794                                                                          
029797     ORDER BY TIREGDAT DESC, TIREGTID DESC                                
029798                                                                          
029799     FOR FETCH ONLY                                                       
029800     END-EXEC                                                             
029801                                                                          
029802     MOVE 000100         TO GOOD-SQLCODES                                 
029803     EXEC SQL OPEN WLOG2-CRS                                              
029804     END-EXEC                                                             
029805     MOVE SQLCODE        TO SQLCODE-WS                                    
029806     PERFORM DB2-STATUS-CHECK                                             
029807     .                                                                    
029808     EJECT                                                                
029809                                                                          
029810 DB2-FETCH-CRS-WLOG-TOT SECTION.                                          
029811     EXEC SQL FETCH WLOG2-CRS INTO                                        
029812            :WLOG-IDDC                                                    
029813           ,:WLOG-TIREGDAT                                                
029814           ,:WLOG-TIREGTID                                                
029815           ,:WLOG-IDUSER                                                  
029816           ,:WLOG-IDLOPNR                                                 
029817           ,:WLOG-KVMILSEC                                                
029818           ,:WLOG-BEWEBSCR                                                
029819           ,:WLOG-BEWEBURL                                                
029820     END-EXEC                                                             
029821                                                                          
029822     MOVE 000100         TO GOOD-SQLCODES                                 
029823     MOVE SQLCODE        TO SQLCODE-WS                                    
029824     PERFORM DB2-STATUS-CHECK                                             
029825     .                                                                    
029826     EJECT                                                                
029827                                                                          
029828 DB2-CLOSE-CRS-WLOG-TOT SECTION.                                          
029829     EXEC SQL CLOSE WLOG2-CRS                                             
029830     END-EXEC                                                             
029831     .                                                                    
029832     EJECT                                                                
029833                                                                          
029834 DB2-OPEN-CRS-WLOG-MAX SECTION.                                           
029835     EXEC SQL DECLARE WLOG4-CRS CURSOR FOR                                
029836     SELECT   IDDC                                                        
029837             ,TIREGDAT                                                    
029838             ,TIREGTID                                                    
029839             ,IDUSER                                                      
029840             ,IDLOPNR                                                     
029841             ,KVMILSEC                                                    
029842             ,BEWEBSCR                                                    
029843             ,BEWEBURL                                                    
029844                                                                          
029845     FROM     TP0WLOG                                                     
029846                                                                          
029847     WHERE    TIREGDAT = :WS4-RUNDATUM                                    
029848                                                                          
029849     ORDER BY KVMILSEC DESC                                               
029850                                                                          
029851     FOR FETCH ONLY                                                       
029852     END-EXEC                                                             
029853                                                                          
029854     MOVE 000100         TO GOOD-SQLCODES                                 
029855     EXEC SQL OPEN WLOG4-CRS                                              
029856     END-EXEC                                                             
029857     MOVE SQLCODE        TO SQLCODE-WS                                    
029858     PERFORM DB2-STATUS-CHECK                                             
029859     .                                                                    
029860     EJECT                                                                
029861                                                                          
029862 DB2-FETCH-CRS-WLOG-MAX SECTION.                                          
029863     EXEC SQL FETCH WLOG4-CRS INTO                                        
029864            :WLOG-IDDC                                                    
029865           ,:WLOG-TIREGDAT                                                
029866           ,:WLOG-TIREGTID                                                
029867           ,:WLOG-IDUSER                                                  
029868           ,:WLOG-IDLOPNR                                                 
029869           ,:WLOG-KVMILSEC                                                
029870           ,:WLOG-BEWEBSCR                                                
029871           ,:WLOG-BEWEBURL                                                
029872     END-EXEC                                                             
029873                                                                          
029874     MOVE 000100         TO GOOD-SQLCODES                                 
029875     MOVE SQLCODE        TO SQLCODE-WS                                    
029876     PERFORM DB2-STATUS-CHECK                                             
029877     .                                                                    
029878     EJECT                                                                
029879                                                                          
029880 DB2-CLOSE-CRS-WLOG-MAX SECTION.                                          
029881     EXEC SQL CLOSE WLOG4-CRS                                             
029882     END-EXEC                                                             
029883     .                                                                    
029884     EJECT                                                                
029885                                                                          
029890 DB2-STATUS-CHECK SECTION.                                                
029900     SET SQLCODE-IX         TO 1                                          
030000     SEARCH GOOD-SQLCODE AT END                                           
030100           CALL ABEND USING RKOD-ABEND-DB2                                
030200        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
030300           CONTINUE                                                       
030400     END-SEARCH                                                           
030500     .                                                                    
