000100*********************************************                             
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6125500.                                                
000400 AUTHOR.         SRINADH NADIMPALLI.                                      
000500 DATE-WRITTEN.   22/07/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        CREATE A ZIPPED CSV FILE OF WDH701/711 EACH DAY SENT TO          
001000*        AZURE BY MQ                                                      
001100*                                                                         
001200*        THE PROGRAM READS     WDH7                                       
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- FILE CONTAINING 711 SEGMENT WITH IDARTNR                   
002600     SELECT W6125N                     ASSIGN TO W61255D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W6125N                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  RECORD -COPY W61255X -PRE UT-  -L.                                   
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W6125500'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400 01  COUNTER                     PIC 9(4) VALUE ZERO.                     
004500 01  INT-DAYS                    PIC S9(6) COMP.                          
004600 01  YESTER-DATE                 PIC 9(8)    VALUE ZERO.                  
004700 01  FILLER REDEFINES YESTER-DATE.                                        
004800     03 YESTER-DATE-CC           PIC 9(2).                                
004900     03 YESTER-DATE-YYMMDD       PIC 9(6).                                
005000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES TODAYS-DATE.                                        
005200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005400     03  TODAYS-DATE-DAY         PIC 9(2).                                
005500     EJECT                                                                
005600 01  SAVE-IDARTNR                PIC 9(8)    VALUE ZERO.                  
005700     EJECT                                                                
005800 01  GENERAL-SUBPROGRAMS.                                                 
005900*                                                                         
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     SKIP2                                                                
006600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006700                                                                          
006800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007100     SKIP2                                                                
007200 01  ERROR-TEXT.                                                          
007300     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007500     EJECT                                                                
007600*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
007700*                                                                         
007800 01  PROGRAM-NAME                PIC X(6)    VALUE 'W61255'.              
007900     SKIP2                                                                
008000 01  DATECARD-ID                 PIC X(6)    VALUE '000000'.              
008100     SKIP2                                                                
008200*01  -COPY WDATKORT                                                       
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700 01  UT-TRANSID.                                                          
008800     03  FILLER                  PIC X(6)  VALUE 'W61255'.                
008900     03  FILLER                  PIC X(8)  VALUE 'W61255D1'.              
009000     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
009100     EJECT                                                                
009200 01  UT-AREA-START               PIC X(24)   VALUE                        
009300                                 'UT-AREA-START  '.                       
009400     SKIP2                                                                
009500                                                                          
009600 01  AREA -COPY W61255X    -PRE UT-                                       
009700     EJECT                                                                
009800*    --- AREAS FOR IMS-SECTIONS                                           
009900*                                                                         
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  KEYS-FOR-DLI.                                                        
010400     03  W-IDARTNR-X.                                                     
010500         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
010600     03  W-WDH711KY-X.                                                    
010700         05  W-WDH711KY          PIC X(7)    VALUE SPACE.                 
010800     SKIP2                                                                
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FOUND                       VALUE '  '.                  
011200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011300     88  SEGMENT-MISSING                     VALUE 'GB'.                  
011400     SKIP2                                                                
011500 01  GOOD-STATUSCODES.                                                    
011600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
011900     SKIP3                                                                
012000*    -COPY WZ20DAYS                                                       
012100     EJECT                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNCTION CODES                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900 01  DLI-IO-AREA.                                                         
013000     03  IO-AREA              PIC X(150).                                 
013100     SKIP3                                                                
013200*    03  FILLER -COPY WDH701             -RED IO-AREA                     
013300     EJECT                                                                
013400*    03  FILLER -COPY WDH711             -RED IO-AREA                     
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700                                                                          
013800                                                                          
013900*01  -COPY W0008  -PRE WDH7-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION  USING WDH7-PCB.                                      
014300 MAIN SECTION.                                                            
014400     ENTRY 'DLITCBL' USING WDH7-PCB.                                      
014500                                                                          
014600                                                                          
014700     PERFORM A-INIT                                                       
014800     PERFORM IMS-GET-WDH7                                                 
014900     PERFORM UNTIL SEGMENT-MISSING                                        
015000      EVALUATE WDH7-SEG-NAME-FB                                           
015100       WHEN 'WDH701  '                                                    
015200         MOVE INVA-IDARTNR TO SAVE-IDARTNR                                
015300       WHEN 'WDH711  '                                                    
015400        IF INVH-DAREGDAT-CLO = YESTER-DATE                                
015500         PERFORM B-MOVE-WDH711                                            
015600         PERFORM S11-WRITE-W6125N                                         
015700        END-IF                                                            
015800       WHEN OTHER                                                         
015900        CONTINUE                                                          
016000      END-EVALUATE                                                        
016100       PERFORM IMS-GET-WDH7                                               
016200                                                                          
016300     END-PERFORM                                                          
016400                                                                          
016500                                                                          
016600     PERFORM Z-FINIT                                                      
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     OPEN OUTPUT W6125N                                                   
017500                                                                          
017600     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
017700     MOVE D-AAR       TO TODAYS-DATE-YEAR                                 
017800     MOVE D-MAANAD    TO TODAYS-DATE-MONTH                                
017900     MOVE D-DAG       TO TODAYS-DATE-DAY                                  
018000     MOVE 20          TO YESTER-DATE-CC                                   
018100     MOVE TODAYS-DATE TO YESTER-DATE-YYMMDD                               
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300                                                                          
019500                                                                          
019600     .                                                                    
019700     EJECT                                                                
019800 B-MOVE-WDH711 SECTION.                                                   
019900     MOVE  SAVE-IDARTNR TO UT-INVA-IDARTNR                                
020000     MOVE  INVH-IDDC TO UT-INVH-IDDC                                      
020100     MOVE  INVH-DAREGDAT-CLO TO UT-INVH-DAREGDAT-CLO                      
020200     MOVE  INVH-FLAUTLSJ TO UT-INVH-FLAUTLSJ                              
020300     MOVE  INVH-IDUSER-CLO TO UT-INVH-IDUSER-CLO                          
020400     MOVE  INVH-KDJUSTYP TO UT-INVH-KDJUSTYP                              
020500     MOVE  INVH-KVJUSTKV TO UT-INVH-KVJUSTKV                              
020600     MOVE  INVH-PRARTSTD TO UT-INVH-PRARTSTD                              
020700     MOVE  INVH-DAREGDAT-CRE TO UT-INVH-DAREGDAT-CRE                      
020800     MOVE  INVH-DAREGDAT-PR1 TO UT-INVH-DAREGDAT-PR1                      
020900     MOVE  INVH-DAREGDAT-PR2 TO UT-INVH-DAREGDAT-PR2                      
021000     MOVE  INVH-DAREGDAT-PR3 TO UT-INVH-DAREGDAT-PR3                      
021100     MOVE  INVH-IDUSER-PR1 TO UT-INVH-IDUSER-PR1                          
021200     MOVE  INVH-IDUSER-PR2 TO UT-INVH-IDUSER-PR2                          
021300     MOVE  INVH-IDUSER-PR3 TO UT-INVH-IDUSER-PR3                          
021400     MOVE  INVH-IDUSER-CRE TO UT-INVH-IDUSER-CRE                          
021500     MOVE  INVH-KVANTAL TO UT-INVH-KVANTAL                                
021600     .                                                                    
021700     EJECT                                                                
021800 Z-FINIT SECTION.                                                         
021900     CLOSE W6125N                                                         
022000     SKIP2                                                                
022100     MOVE 'S' TO POSTSUM-OPKOD                                            
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300     .                                                                    
022400     EJECT                                                                
022500 S11-WRITE-W6125N SECTION.                                                
022600                                                                          
022700     WRITE UT-RECORD FROM UT-AREA                                         
022800                                                                          
022900     MOVE UT-TRANSID      TO POSTSUM-TRANSID                              
023000*    MOVE NO-IDPTYP TO POSTSUM-TRANSTYP                                   
023100*    MOVE 'W6125N' TO POSTSUM-FDNAMN                                      
023200*    MOVE 'W61255D1' TO POSTSUM-DDNAMN1                                   
023300     CALL POSTSUM USING POSTSUM-PARM                                      
023400     .                                                                    
023500     EJECT                                                                
023600 S99-ABEND SECTION.                                                       
023700                                                                          
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     CALL ABEND USING RKOD-ABEND                                          
024200     .                                                                    
024300     EJECT                                                                
024400* --- IMS SECTIONS  ---                                                   
024500                                                                          
024600     EJECT                                                                
024700 IMS-GET-WDH7   SECTION.                                                  
024800                                                                          
024900     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
025000     CALL CBLTDLI USING GN WDH7-PCB DLI-IO-AREA                           
025100     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
025200     PERFORM IMS-STATUSCHECK                                              
025300     .                                                                    
025400     EJECT                                                                
025500 IMS-STATUSCHECK SECTION.                                                 
025600                                                                          
025700     SET STATUS-IX TO 1                                                   
025800     SEARCH GOOD-STATUS                                                   
025900       AT END                                                             
026000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026100           DELIMITED BY SIZE INTO ERROR-TEXT                              
026200         DISPLAY ERROR-TEXT                                               
026300         CALL FELLOG                                                      
026400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
026500         CONTINUE                                                         
026600     END-SEARCH                                                           
026700     .                                                                    
