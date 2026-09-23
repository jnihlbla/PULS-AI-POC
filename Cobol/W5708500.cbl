001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5708500.                                                
001300 AUTHOR.         ARCHANA BHAT.                                            
001400 DATE-WRITTEN.   15/06/29.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        THIS PROGRAM MAKES A LIST OF VALID INBOUNDS FOR PREVIOUS         
001900*        MONTH FOR CHINA/US                                               
002000*                                                                         
002110*        THE PROGRAM READS     WDL6                                       
002200*                                                                         
002300*    ABENDCODES:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- VALID INBOUNDS                                             
003410     SELECT W57085                     ASSIGN TO W57085D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W57085                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004010*01  RECORD -COPY W57085 -PRE  UT-  -L.                                   
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W5708500'.            
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004810 77  WS-TIINLINL                 PIC 9(6)    VALUE ZERO.                  
004900     EJECT                                                                
005000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES TODAYS-DATE.                                        
005200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005400     03  TODAYS-DATE-DAY         PIC 9(2).                                
005500     EJECT                                                                
005510 01  WS-PREV-DATE                PIC 9(4)    VALUE ZERO.                  
005520 01  FILLER REDEFINES WS-PREV-DATE.                                       
005530     03  WS-PREV-YEAR            PIC 9(2).                                
005540     03  WS-PREV-MONTH           PIC 9(2).                                
005550     EJECT                                                                
005560*      --- VALID IDDC CODES                                               
005570*                                                                         
005580*01    -COPY WWDC99                                                       
005590       EJECT                                                              
005600 01  GENERAL-SUBPROGRAMS.                                                 
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  ERROR-TEXT.                                                          
007000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  UT-AREA-START               PIC X(24)   VALUE                        
007403                                 'UT-AREA-START  '.                       
007404     SKIP2                                                                
007405                                                                          
007410*01  AREA -COPY W57085     -PRE UT-                                       
007500     EJECT                                                                
007600*    --- AREAS FOR IMS-SECTIONS                                           
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-FOR-DLI.                                                        
008201     03  W-IDARTNR-X.                                                     
008202         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
008203     03  W-DAINLEV-X.                                                     
008210         05  W-DAINLEV           PIC S9(16)   VALUE ZERO COMP-3.          
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FOUND                       VALUE '  '.                  
008700     88  SEGMENT-MISSING                     VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010002     SKIP3                                                                
010003 01  DLI-IO-AREA.                                                         
010004     03  IO-AREA                 PIC X(1600) VALUE SPACE.                 
010005     SKIP3                                                                
010006     03  WDL601   REDEFINES IO-AREA.                                      
010007*        05  -COPY WDL601                                                 
010008     SKIP3                                                                
010009     03  WDL611   REDEFINES IO-AREA.                                      
010010*        05  -COPY WDL611                                                 
010011     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDL6-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING WDL6-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDL6-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011401     PERFORM IMS-GET-WDL6                                                 
011402     PERFORM UNTIL SEGMENT-MISSING                                        
011403       EVALUATE WDL6-SEG-NAME-FB                                          
011404         WHEN 'WDL601'                                                    
011407           MOVE ART-IDARTNR    TO UT-L6-IDARTNR                           
011408         WHEN 'WDL611'                                                    
011409           PERFORM B-CHECK-TIINLINL                                       
011410       END-EVALUATE                                                       
011411       PERFORM IMS-GET-WDL6                                               
011420     END-PERFORM                                                          
011500     PERFORM Z-FINIT                                                      
011600                                                                          
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012200 A-INIT SECTION.                                                          
012301                                                                          
012310     OPEN OUTPUT W57085                                                   
012400                                                                          
012500     ACCEPT TODAYS-DATE  FROM DATE                                        
012610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012620     IF TODAYS-DATE-MONTH = 01                                            
012630        COMPUTE WS-PREV-YEAR  = TODAYS-DATE-YEAR - 1                      
012640        COMPUTE WS-PREV-MONTH = 12                                        
012650     ELSE                                                                 
012660        COMPUTE WS-PREV-YEAR  = TODAYS-DATE-YEAR                          
012670        COMPUTE WS-PREV-MONTH = TODAYS-DATE-MONTH - 1                     
012680     END-IF                                                               
012800     .                                                                    
012900     EJECT                                                                
012910 B-CHECK-TIINLINL SECTION.                                                
012920                                                                          
012921     MOVE INL-IDDC           TO WS-IDDC                                   
012922     MOVE INL-TIINLINL       TO WS-TIINLINL                               
012923     IF NDC-CN OR NDC-US                                                  
012924        IF INL-TIINLINL > 0                                               
012925        AND WS-TIINLINL(1:4) = WS-PREV-DATE                               
012930            MOVE INL-IDDC          TO UT-L6-IDDC                          
012932            MOVE INL-IDLEVNR       TO UT-L6-IDLEVNR                       
012933            MOVE '20'              TO UT-L6-PERIOD(1:2)                   
012934            MOVE WS-PREV-DATE      TO UT-L6-PERIOD(3:4)                   
012935            PERFORM S11-WRITE-W57085                                      
012937        END-IF                                                            
012938     END-IF                                                               
012939     .                                                                    
012940     EJECT                                                                
013000 Z-FINIT SECTION.                                                         
013110     CLOSE W57085                                                         
013201     SKIP2                                                                
013202     MOVE 'S' TO POSTSUM-OPKOD                                            
013210     CALL POSTSUM USING POSTSUM-PARM                                      
013300     .                                                                    
013501     EJECT                                                                
013502 S11-WRITE-W57085 SECTION.                                                
013503                                                                          
013504     WRITE UT-RECORD FROM UT-AREA                                         
013505                                                                          
013507     MOVE 'W57085' TO POSTSUM-FDNAMN                                      
013508     MOVE 'W57085D1' TO POSTSUM-DDNAMN2                                   
013509     CALL POSTSUM USING POSTSUM-PARM                                      
013510     .                                                                    
013700     EJECT                                                                
013800 S99-ABEND SECTION.                                                       
013900                                                                          
014001     SKIP2                                                                
014002     MOVE 'S' TO POSTSUM-OPKOD                                            
014010     CALL POSTSUM USING POSTSUM-PARM                                      
014100     CALL ABEND USING RKOD-ABEND                                          
014200     .                                                                    
014300     EJECT                                                                
014400* --- IMS SECTIONS  ---                                                   
014500                                                                          
014601                                                                          
014602 IMS-GET-WDL6   SECTION.                                                  
014603                                                                          
014604     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-AREA                           
014605     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
014606     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
014607     PERFORM IMS-STATUSCHECK                                              
014610     .                                                                    
014700     EJECT                                                                
014800 IMS-STATUSCHECK SECTION.                                                 
014900                                                                          
015000     SET STATUS-IX TO 1                                                   
015100     SEARCH GOOD-STATUS                                                   
015200       AT END                                                             
015300         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
015400           DELIMITED BY SIZE INTO ERROR-TEXT                              
015500         DISPLAY ERROR-TEXT                                               
015600         CALL FELLOG                                                      
015700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
015800         CONTINUE                                                         
015900     END-SEARCH                                                           
016000     .                                                                    
