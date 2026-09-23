001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5127200.                                                
001300 AUTHOR.         SARASWATHY S.                                            
001400 DATE-WRITTEN.   19/04/19.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        CREATES GIT FILE WITH ITEMS IN GIT MORE THAN 6 MONTHS            
001900*                                                                         
002010*        THE PROGRAM READS   FILES W51262 AND W01160                      
002020*                            DB    WDB6                                   
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003302*          --- CDC INFO FILE                                              
003303     SELECT W01160                     ASSIGN TO W51272D1.                
003305*          --- GIT RECORDS FROM W51262                                    
003310     SELECT W51262                     ASSIGN TO W51272D2.                
003311*          --- GIT OLDER THAN 6 MONTHS                                    
003320     SELECT W51272                     ASSIGN TO W51272D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003800 FILE SECTION.                                                            
003900 FD  W01160                                                               
003901     RECORDING       F                                                    
003902     BLOCK CONTAINS  0.                                                   
003903                                                                          
003904*01  RECORD -COPY W01160  -PRE  IN1- -L.                                  
003905     EJECT                                                                
003906*                                                                         
003907 FD  W51262                                                               
003908     RECORDING       F                                                    
003909     BLOCK CONTAINS  0.                                                   
003910                                                                          
003911*01  RECORD -COPY W51262  -PRE  IN2- -L.                                  
003912     EJECT                                                                
003913*                                                                         
003914 FD  W51272                                                               
003915     RECORDING       F                                                    
003916     BLOCK CONTAINS  0.                                                   
003917                                                                          
003920*01  RECORD -COPY W51272  -PRE  UT- -L.                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)       VALUE 'W5127200'.         
004601 01  WS-DATUM                    PIC X(8).                                
004610 01  WS-DAINLEV                  PIC 9(16).                               
004700 01  WS-DATE-DIFF                PIC 9(8)       VALUE ZERO.               
004800     EJECT                                                                
005310 01  WS-IDFTG-TABEL.                                                      
005320     03 WS-SAVE-IDFTG OCCURS 500 INDEXED BY WS-IDFTG-IX.                  
005330        05 WS-IDDC               PIC X(2).                                
005340        05 WS-IDFTG              PIC X(2).                                
005350 01  WS-TOT-PRARTSTD             PIC S9(11)V9(2) VALUE +0 COMP-3.         
005400     EJECT                                                                
005402 01  WS-CLAG-IDARTNR             PIC 9(9)       VALUE ZERO.               
005404                                                                          
005405 01  WS-62-ARTNR-IDDC.                                                    
005406     05  WS-62-ARTNR             PIC 9(9)       VALUE ZERO.               
005408                                                                          
005409 77  W01160-EOF-SW               PIC X          VALUE 'N'.                
005410     88  END-OF-W01160                          VALUE 'J'.                
005411*                                                                         
005412 77  W51262-EOF-SW               PIC X          VALUE 'N'.                
005420     88  END-OF-W51262                          VALUE 'J'.                
005430*                                                                         
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)       VALUE 'ABEND'.            
005800     03  CBLTDLI                 PIC X(8)       VALUE 'CBLTDLI '.         
005900     03  FELLOG                  PIC X(8)       VALUE 'FELLOG  '.         
006101     03  POSTSUM                 PIC X(8)       VALUE 'POSTSUM'.          
006110     03  WZ20DAYS                PIC X(8)       VALUE 'WZ20DAYS'.         
006200     SKIP2                                                                
006300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006310 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
006320     SKIP3                                                                
006330*    -COPY WZ20DAYS                                                       
006340     EJECT                                                                
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  ERROR-TEXT.                                                          
007000     03  FILLER                  PIC X(15)   VALUE 'ERROR-TEXT'.          
007100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007210 01  FELTEXT.                                                             
007220     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007230     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
007400*                                                                         
008010*01  -COPY W0005   -PRE  POSTSUM-                                         
008101     EJECT                                                                
008202 01  IN-AREA-START               PIC X(24)   VALUE                        
008203                                 'IN-AREA-START  '.                       
008204*01  AREA -COPY W01160 -PRE IN1-                                          
008205     EJECT                                                                
008206*01  AREA -COPY W51262 -PRE IN2-                                          
008207     EJECT                                                                
008208                                                                          
008209 01  UT-AREA-START               PIC X(24)   VALUE                        
008210                                 'UT-AREA-START  '.                       
008220*01  AREA -COPY W51272 -PRE UT-                                           
008300     EJECT                                                                
008400*    --- AREAS FOR IMS-SECTIONS                                           
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FOUND                       VALUE '  '.                  
009600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009610     88  SEGMENT-END                         VALUE 'GB'.                  
009700     SKIP2                                                                
009800 01  GOOD-STATUSCODES.                                                    
009900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNCTION CODES                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010810 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB6'.                        
010820 01  DLI-IO-WDB601.                                                       
010830*    03  -COPY WDB601                                                     
010840     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011501                                                                          
011504*01  -COPY W0008  -PRE WDB6-                                              
011505     05  FILLER                  PIC X.                                   
011506*                                                                         
011507     EJECT                                                                
011508                                                                          
011701 PROCEDURE DIVISION  USING WDB6-PCB.                                      
011702 MAIN SECTION.                                                            
011710     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
011800                                                                          
012100     PERFORM A-INIT                                                       
012101     PERFORM S01-READ-W01160                                              
012110     PERFORM S02-READ-W51262                                              
012111     PERFORM UNTIL  END-OF-W01160                                         
012112       PERFORM UNTIL END-OF-W51262                                        
012113                 OR WS-62-ARTNR > WS-CLAG-IDARTNR                         
012114                                                                          
012115         IF  IN2-IDARTNR = IN1-CLAG-IDARTNR                               
012117             PERFORM B-SELECTION                                          
012118         END-IF                                                           
012119         PERFORM S02-READ-W51262                                          
012120       END-PERFORM                                                        
012121       PERFORM S01-READ-W01160                                            
012122     END-PERFORM                                                          
012200                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014301                                                                          
014310     OPEN INPUT  W01160                                                   
014311                 W51262                                                   
014312     OPEN OUTPUT W51272                                                   
014313     PERFORM AA-LOAD-IDFTG-TABEL                                          
014314     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
014320     .                                                                    
014330     EJECT                                                                
014340 AA-LOAD-IDFTG-TABEL  SECTION.                                            
014350                                                                          
014360     INITIALIZE WS-IDFTG-TABEL                                            
014370     SET WS-IDFTG-IX TO +1                                                
014380     PERFORM IMS-GN-WDB601                                                
014390     PERFORM UNTIL SEGMENT-END                                            
014391        MOVE DCS-IDDC            TO WS-IDDC (WS-IDFTG-IX)                 
014392        MOVE DCS-IDFTG           TO WS-IDFTG(WS-IDFTG-IX)                 
014393        PERFORM IMS-GN-WDB601                                             
014394        SET WS-IDFTG-IX UP BY +1                                          
014395        IF WS-IDFTG-IX      > 500                                         
014396           MOVE 'IDFTG TABLE FULL'                                        
014397                                 TO FELTEXT                               
014398           CALL FELLOG                                                    
014399        END-IF                                                            
014400     END-PERFORM                                                          
014401     .                                                                    
014402     EJECT                                                                
014403                                                                          
014404 B-SELECTION SECTION.                                                     
014405     PERFORM BA-CHECK-IDFTG                                               
014406     COMPUTE WS-TOT-PRARTSTD ROUNDED =                                    
014407                (IN1-CLAG-PRARTSTD * IN2-KVAVIS)                          
014408     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
014409                                                                          
014410     MOVE WS-DATUM (1:8)         TO DAYS-TIDATE2                          
014411     MOVE 'YYYYMMDD'             TO DAYS-KDDATFMT2                        
014412     MOVE 0                      TO DAYS-KVDAYS                           
014413     COMPUTE WS-DAINLEV = 9999999999999999 - IN2-DAINLEV                  
014414     MOVE WS-DAINLEV (1:8)       TO DAYS-TIDATE1                          
014416     MOVE 'YYYYMMDD'             TO DAYS-KDDATFMT1                        
014420     MOVE ' '                    TO DAYS-IDCALEND                         
014421     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
014422     IF DAYS-KDRC = ZERO                                                  
014423       MOVE DAYS-KVDAYS          TO WS-DATE-DIFF                          
014424     END-IF                                                               
014425     IF WS-DATE-DIFF > 180                                                
014428        MOVE WS-DAINLEV(1:8)      TO UT-DATRADAT                          
014429        MOVE IN2-IDARTNR          TO UT-IDARTNR                           
014430        MOVE IN2-IDDC             TO UT-IDDC                              
014431        MOVE IN2-IDLEVNR          TO UT-IDLEVNR                           
014432        MOVE IN2-IDFAKT           TO UT-IDFAKT                            
014433        MOVE IN2-IDDISTR          TO UT-IDDISTR                           
014434        MOVE IN2-IDKUNDNR         TO UT-IDKUNDNR                          
014435        MOVE IN2-IDORDNR5         TO UT-IDORDNR5                          
014437        MOVE IN2-KDFRAKT          TO UT-KDFRAKT                           
014438        MOVE IN1-CLAG-PRARTSTD    TO UT-PRARTSTD                          
014439        MOVE IN2-KVAVIS           TO UT-KVAVIS                            
014440        MOVE IN2-KDVALISO         TO UT-KDVALISO                          
014441        MOVE WS-TOT-PRARTSTD      TO UT-TOT-PRARTSTD                      
014450        PERFORM S11-WRITE-W51272                                          
014453     END-IF                                                               
014454     .                                                                    
014460     EJECT                                                                
014470 BA-CHECK-IDFTG SECTION.                                                  
014480     SET WS-IDFTG-IX TO  1                                                
014490     SEARCH WS-SAVE-IDFTG                                                 
014500       AT END                                                             
014600         CONTINUE                                                         
014700       WHEN ((WS-IDDC(WS-IDFTG-IX) = IN2-IDDC))                           
014800         MOVE WS-IDFTG(WS-IDFTG-IX) TO UT-IDFTG                           
015210     END-SEARCH                                                           
015220     .                                                                    
015230     EJECT                                                                
015300 Z-FINIT SECTION.                                                         
015410     CLOSE W01160                                                         
015420           W51262                                                         
015430           W51272                                                         
015501     SKIP2                                                                
015502     MOVE 'S' TO POSTSUM-OPKOD                                            
015510     CALL POSTSUM USING POSTSUM-PARM                                      
015600     .                                                                    
015801     EJECT                                                                
015802 S01-READ-W01160  SECTION.                                                
015803     READ W01160        INTO IN1-AREA                                     
015804     AT END                                                               
015805        MOVE HIGH-VALUE   TO IN1-AREA                                     
015806        SET END-OF-W01160 TO TRUE                                         
015807                                                                          
015808     NOT AT END                                                           
015809        MOVE IN1-CLAG-IDARTNR TO WS-CLAG-IDARTNR                          
015811        MOVE 'W01160'    TO POSTSUM-FDNAMN                                
015812        MOVE 'W51272D1'  TO POSTSUM-DDNAMN2                               
015813        CALL POSTSUM  USING POSTSUM-PARM                                  
015814     END-READ                                                             
015815     .                                                                    
015816     EJECT                                                                
015817 S02-READ-W51262  SECTION.                                                
015818     READ W51262        INTO IN2-AREA                                     
015819     AT END                                                               
015820        MOVE HIGH-VALUE   TO IN2-AREA                                     
015821        SET END-OF-W51262 TO TRUE                                         
015822                                                                          
015823     NOT AT END                                                           
015824        MOVE IN2-IDARTNR TO WS-62-ARTNR                                   
015826        MOVE 'W51262'    TO POSTSUM-FDNAMN                                
015827        MOVE 'W51272D2'  TO POSTSUM-DDNAMN2                               
015828        CALL POSTSUM  USING POSTSUM-PARM                                  
015829     END-READ                                                             
015830     .                                                                    
015831     EJECT                                                                
015832 S11-WRITE-W51272 SECTION.                                                
015833                                                                          
015834     WRITE UT-RECORD FROM UT-AREA                                         
015835                                                                          
015836     MOVE SPACES     TO POSTSUM-TRANSTYP                                  
015837     MOVE 'W51272'   TO POSTSUM-FDNAMN                                    
015838     MOVE 'W51272D3' TO POSTSUM-DDNAMN2                                   
015839     CALL POSTSUM    USING POSTSUM-PARM                                   
015840     .                                                                    
016000     EJECT                                                                
016100 S99-ABEND SECTION.                                                       
016200                                                                          
016301     SKIP2                                                                
016302     MOVE 'S' TO POSTSUM-OPKOD                                            
016310     CALL POSTSUM USING POSTSUM-PARM                                      
016400     CALL ABEND USING RKOD-ABEND                                          
016500     .                                                                    
016600     EJECT                                                                
016700* --- IMS SECTIONS  ---                                                   
016800                                                                          
016901     EJECT                                                                
017010 IMS-GN-WDB601    SECTION.                                                
017020     MOVE 'WDB601  '            TO SSA1                                   
017030     MOVE '  GB'                TO GOOD-STATUSCODES                       
017040     CALL CBLTDLI            USING GN WDB6-PCB DLI-IO-WDB601 SSA1         
017050     MOVE WDB6-STATUS-CODE      TO STATUS-WS                              
017060     PERFORM IMS-STATUSCHECK                                              
017070     .                                                                    
017080     EJECT                                                                
017100 IMS-STATUSCHECK SECTION.                                                 
017200                                                                          
017300     SET STATUS-IX TO 1                                                   
017400     SEARCH GOOD-STATUS                                                   
017500       AT END                                                             
017600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
017700           DELIMITED BY SIZE INTO ERROR-TEXT                              
017800         DISPLAY ERROR-TEXT                                               
017900         CALL FELLOG                                                      
018000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
018100         CONTINUE                                                         
018200     END-SEARCH                                                           
018300     .                                                                    
