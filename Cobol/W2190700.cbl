000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2190700.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   16/10/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        FILE TO REMOVE OR REPLACE CAMPAIGN (WDM2)                        
000900*                                                                         
001000*        THE PROGRAM READS     WDM2                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
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
002400*          --- FILE TO DELETE/REPLASE CAMPAIGN - WDM2                     
002500     SELECT W21907                   ASSIGN TO W21907D1.                  
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W21907                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  RECORD -COPY W21907 -PRE  OUT- -L.                                   
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -COPY WY2000W1                                                       
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W2190700'.            
003910 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
003920 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200 77  FL-DELETE-WDM2              PIC X       VALUE 'N'.                   
004210 77  W-KAMP-IDKAMPRF             PIC S9(07) COMP-3 VALUE +0.              
004211 77  W-KAMP-IDDC                 PIC X(02)         VALUE SPACE.           
004212 77  W-KAMP-TISTODAT             PIC S9(07) COMP-3 VALUE +0.              
004300     EJECT                                                                
004400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES TODAYS-DATE.                                        
004600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004800     03  TODAYS-DATE-DAY         PIC 9(2).                                
004900     EJECT                                                                
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100*                                                                         
005110     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
005200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005800     SKIP2                                                                
005812*                            *** PARAMETRAR TILL WDAGKONV '               
005813*01  -COPY WDAGAREA.                                                      
005814     EJECT                                                                
005900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006410     EJECT                                                                
006500 01  ERROR-TEXT.                                                          
006600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006800     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008300 01  OUT-AREA-START              PIC X(24)   VALUE                        
008400                                 'OUT-AREA-START  '.                      
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W21907     -PRE OUT-                                      
008800     EJECT                                                                
008900*    --- AREAS FOR IMS-SECTIONS                                           
009000*                                                                         
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  KEYS-FOR-DLI.                                                        
009500     03  W-IDKAMPRF-X.                                                    
009600         05  W-IDKAMPRF          PIC S9(5)   VALUE ZERO COMP-3.           
009700     03  W-IDARTNR-X.                                                     
009800         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
009801                                                                          
009810     03  W-WDQ4BSEQ-MIN-X.                                                
009820         05  W-WDQ4B-IDARTNR-MIN PIC S9(9)    COMP-3.                     
009830         05  FILLER              PIC  X(19)   VALUE SPACE.                
009840*                                                                         
009850     03  W-WDQ4BSEQ-MAX-X.                                                
009860         05  W-WDQ4B-IDARTNR-MAX PIC S9(9)    COMP-3.                     
009870         05  FILLER              PIC  X(19)   VALUE SPACE.                
009880*                                                                         
009890     03  W-WDA5ASEQ-MIN-X.                                                
009891         05  W-WDA5A-IDARTNR-MIN PIC S9(9) VALUE ZERO COMP-3.             
009892         05  W-WDA5A-IDDC-MIN    PIC X(2)  VALUE SPACE.                   
009893         05  FILLER              PIC X(2)  VALUE SPACE.                   
009894                                                                          
009895     03  W-WDA5ASEQ-MAX-X.                                                
009896         05  W-WDA5A-IDARTNR-MAX PIC S9(9) VALUE ZERO COMP-3.             
009897         05  W-WDA5A-IDDC-MAX    PIC X(2)  VALUE SPACE.                   
009898         05  FILLER              PIC X(2)  VALUE SPACE.                   
009900                                                                          
009901     03  W-WDE4CSEQ-MIN-X.                                                
009902         05  W-WDE4C-IDARTNR-MIN PIC S9(9) VALUE ZERO COMP-3.             
009904                                                                          
009905     03  W-WDE4CSEQ-MAX-X.                                                
009906         05  W-WDE4C-IDARTNR-MAX PIC S9(9) VALUE ZERO COMP-3.             
009909                                                                          
009910     SKIP2                                                                
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FOUND                       VALUE '  '.                  
010300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010600     SKIP2                                                                
010700 01  GOOD-STATUSCODES.                                                    
010800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(128).                              
011100 01  SSA2                        PIC X(128).                              
011200     EJECT                                                                
011300*    --- IMS FUNCTION CODES                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600*    ---  DLI INPUT-OUTPUT AREA                                           
011610 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDM2'.               
011700 01  DLI-IO-WDM2.                                                         
011800     03  IO-AREA-WDM2        PIC X(100).                                  
011900*    03            -COPY WDM201    -RED IO-AREA-WDM2                      
012000     EJECT                                                                
012210*    03            -COPY WDM211    -RED IO-AREA-WDM2                      
012220     EJECT                                                                
012221 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDA501'.             
012230 01  DLI-IO-WDA501.                                                       
012250*    03            -COPY WDA501                                           
012260     EJECT                                                                
012270 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDQ401'.             
012280 01  DLI-IO-WDQ401.                                                       
012290*    03            -COPY WDQ401                                           
012291     EJECT                                                                
012292 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE401'.             
012293 01  DLI-IO-WDE401.                                                       
012294*    03            -COPY WDE401                                           
012295     EJECT                                                                
012296 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE411'.             
012297 01  DLI-IO-WDE411.                                                       
012298*    03            -COPY WDE411 -PRE E411-                                
012299     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012500                                                                          
012600*01  -COPY W0008  -PRE WDM2-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012810*01  -COPY W0008  -PRE WDQ4B-                                             
012820     05  FILLER                  PIC X.                                   
012830     EJECT                                                                
012840*01  -COPY W0008  -PRE WDA5A-                                             
012850     05  FILLER                  PIC X.                                   
012860     EJECT                                                                
012870*01  -COPY W0008  -PRE WDE4C-                                             
012880     05  FILLER                  PIC X.                                   
012890     EJECT                                                                
012900 PROCEDURE DIVISION  USING WDM2-PCB WDQ4B-PCB WDA5A-PCB WDE4C-PCB.        
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING WDM2-PCB WDQ4B-PCB WDA5A-PCB WDE4C-PCB.        
013200                                                                          
013500     PERFORM A-INIT                                                       
013600                                                                          
013700     PERFORM IMS-GN-WDM2                                                  
013800                                                                          
013900     PERFORM UNTIL SEGMENT-SLUT                                           
014000        EVALUATE WDM2-SEG-NAME-FB                                         
014100           WHEN 'WDM201'                                                  
014110             IF FL-DELETE-WDM2 = YES                                      
014111                MOVE 'DEL'             TO OUT-IDPTYP                      
014120                MOVE W-KAMP-IDKAMPRF   TO OUT-IDKAMPRF                    
014130                MOVE W-KAMP-IDDC       TO OUT-IDDC                        
014132                MOVE +0                TO OUT-IDARTNR                     
014140                PERFORM S11-WRITE-W21907                                  
014150             END-IF                                                       
014151             MOVE KAMP-IDKAMPRF        TO W-KAMP-IDKAMPRF                 
014152             MOVE KAMP-IDDC            TO W-KAMP-IDDC                     
014153             MOVE KAMP-TISTODAT        TO W-KAMP-TISTODAT                 
014160                                                                          
014200             PERFORM B-CHECK-DELETE-WDM201                                
014201                                                                          
014600           WHEN 'WDM211'                                                  
014700             PERFORM C-CHECK-ORDER-BACKORDER                              
014800             PERFORM D-UNRESERVE-CAMPAIGN                                 
015600                                                                          
016400        END-EVALUATE                                                      
016500                                                                          
016600        PERFORM IMS-GN-WDM2                                               
016700                                                                          
016800     END-PERFORM                                                          
017300                                                                          
017310     IF FL-DELETE-WDM2 = YES                                              
017311        MOVE 'DEL'                     TO OUT-IDPTYP                      
017320        MOVE W-KAMP-IDKAMPRF           TO OUT-IDKAMPRF                    
017330        MOVE W-KAMP-IDDC               TO OUT-IDDC                        
017331        MOVE +0                        TO OUT-IDARTNR                     
017340        PERFORM S11-WRITE-W21907                                          
017350     END-IF                                                               
017360                                                                          
017400     PERFORM Z-FINIT                                                      
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018010     MOVE 'A-INITT                 ' TO CURRENT-SECTION                   
018100                                                                          
018200     OPEN OUTPUT W21907                                                   
018300                                                                          
018400     ACCEPT TODAYS-DATE  FROM DATE                                        
018500                                                                          
019100     .                                                                    
019200     EJECT                                                                
019210 B-CHECK-DELETE-WDM201 SECTION.                                           
019211     MOVE 'B-CHECK-DELETE-WDM201   ' TO CURRENT-SECTION                   
019214                                                                          
019215     MOVE NOO                        TO FL-DELETE-WDM2                    
019218                                                                          
019220     IF KAMP-TISTODAT > TODAYS-DATE                                       
019222        CONTINUE                                                          
019223     ELSE                                                                 
019224        IF KAMP-TISTADAT > TODAYS-DATE                                    
019226           CONTINUE                                                       
019227        ELSE                                                              
019228           MOVE 001                  TO DAG-KDCALL                        
019229           MOVE 20                   TO DAG-TISEKEL-TOM                   
019230           MOVE TODAYS-DATE          TO DAG-TIAAMMDD-TOM                  
019231           MOVE 20                   TO DAG-TISEKEL-FOM                   
019232           IF KAMP-TISTODAT > +0                                          
019234              MOVE KAMP-TISTODAT     TO DAG-TIAAMMDD-FOM                  
019235           ELSE                                                           
019236              IF KAMP-TISTADAT > +0                                       
019238                 MOVE KAMP-TISTADAT  TO DAG-TIAAMMDD-FOM                  
019239              ELSE                                                        
019241                 MOVE KAMP-TIREGDAT  TO DAG-TIAAMMDD-FOM                  
019243              END-IF                                                      
019244           END-IF                                                         
019245                                                                          
019253           CALL WDAGKONV   USING DAG-KDCALL,                              
019254                                 DAG-DATUM-AREA,                          
019255                                 DAG-KDSVAR                               
019256           IF DAG-KDSVAR = SPACE                                          
019258              IF KAMP-TISTODAT > +0                                       
019259                 IF DAG-KVKALDAG > 365                                    
019260                    MOVE YES         TO FL-DELETE-WDM2                    
019262                 END-IF                                                   
019263              ELSE                                                        
019264                 IF DAG-KVKALDAG > 730                                    
019265                    MOVE YES         TO FL-DELETE-WDM2                    
019267                 END-IF                                                   
019270              END-IF                                                      
019271           ELSE                                                           
019272              MOVE '*** WRONG RETURN CODE FROM WORKDAY'                   
019273                                     TO ERROR-TEXT-STR                    
019274              DISPLAY ERROR-TEXT                                          
019275              CALL ABEND USING RKOD-ABEND-NO-DUMP                         
019276           END-IF                                                         
019277        END-IF                                                            
019279     END-IF                                                               
019306     .                                                                    
019307     EJECT                                                                
019308                                                                          
019309 C-CHECK-ORDER-BACKORDER SECTION.                                         
019310     MOVE 'C-CHECK-ORDER-BACKORDER ' TO CURRENT-SECTION                   
019312                                                                          
019316     IF FL-DELETE-WDM2 = YES                                              
019318        MOVE LOW-VALUE            TO W-WDQ4BSEQ-MIN-X                     
019319        MOVE HIGH-VALUE           TO W-WDQ4BSEQ-MAX-X                     
019320        MOVE KART-IDARTNR         TO W-WDQ4B-IDARTNR-MIN                  
019321                                     W-WDQ4B-IDARTNR-MAX                  
019323        PERFORM IMS-GU-WDQ4BSEQ                                           
019324        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-SLUT OR                  
019325                      FL-DELETE-WDM2 = NOO                                
019329          IF ORAD-IDKAMPRF = W-KAMP-IDKAMPRF                              
019330         AND ORAD-IDDC     = W-KAMP-IDDC                                  
019331             MOVE NOO             TO FL-DELETE-WDM2                       
019333          END-IF                                                          
019334          PERFORM IMS-GN-WDQ4BSEQ                                         
019335        END-PERFORM                                                       
019336                                                                          
019337        IF FL-DELETE-WDM2 = YES                                           
019338           MOVE LOW-VALUE         TO W-WDA5ASEQ-MIN-X                     
019339           MOVE HIGH-VALUE        TO W-WDA5ASEQ-MAX-X                     
019340           MOVE KART-IDARTNR      TO W-WDA5A-IDARTNR-MIN                  
019341                                     W-WDA5A-IDARTNR-MAX                  
019342           MOVE W-KAMP-IDDC       TO W-WDA5A-IDDC-MIN                     
019343                                     W-WDA5A-IDDC-MAX                     
019344           PERFORM IMS-GU-WDA5ASEQ                                        
019345           PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-SLUT OR               
019346                         FL-DELETE-WDM2 = NOO                             
019350              IF RAD-IDKAMPRF = W-KAMP-IDKAMPRF                           
019351                 IF RAD-KDSTARAD NOT = '4'                                
019353                    MOVE NOO TO FL-DELETE-WDM2                            
019355                 END-IF                                                   
019356              END-IF                                                      
019357              PERFORM IMS-GN-WDA5ASEQ                                     
019358           END-PERFORM                                                    
019359        END-IF                                                            
019360                                                                          
019361        IF FL-DELETE-WDM2 = YES                                           
019362           MOVE LOW-VALUE         TO W-WDE4CSEQ-MIN-X                     
019363           MOVE HIGH-VALUE        TO W-WDE4CSEQ-MAX-X                     
019364           MOVE KART-IDARTNR      TO W-WDE4C-IDARTNR-MIN                  
019365                                     W-WDE4C-IDARTNR-MAX                  
019366           PERFORM IMS-GU-WDE4CSEQ                                        
019367           PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-SLUT OR               
019368                         FL-DELETE-WDM2 = NOO                             
019371             IF E411-ORAD-IDKAMPRF = W-KAMP-IDKAMPRF                      
019372                PERFORM IMS-GNP-WDE401                                    
019373                IF SEGMENT-FOUND                                          
019375                   IF KORD-IDDC  = W-KAMP-IDDC                            
019376                      MOVE NOO       TO FL-DELETE-WDM2                    
019378                   END-IF                                                 
019379                END-IF                                                    
019380             END-IF                                                       
019381             PERFORM IMS-GN-WDE4CSEQ                                      
019382           END-PERFORM                                                    
019383        END-IF                                                            
019384                                                                          
019385     END-IF                                                               
019386     .                                                                    
019387     EJECT                                                                
019388 D-UNRESERVE-CAMPAIGN SECTION.                                            
019389     MOVE 'D-UNRESERVE-CAMPAIGN    ' TO CURRENT-SECTION                   
019390                                                                          
019396     IF W-KAMP-TISTODAT = +0                                              
019398     OR W-KAMP-TISTODAT > TODAYS-DATE                                     
019400        CONTINUE                                                          
019401     ELSE                                                                 
019405        IF KART-KVRESS-KAMP > KART-KVBEART-KUND                           
019407           MOVE 'REP'                TO OUT-IDPTYP                        
019408           MOVE W-KAMP-IDKAMPRF      TO OUT-IDKAMPRF                      
019409           MOVE W-KAMP-IDDC          TO OUT-IDDC                          
019410           MOVE KART-IDARTNR         TO OUT-IDARTNR                       
019411           PERFORM S11-WRITE-W21907                                       
019412        END-IF                                                            
019413     END-IF                                                               
019414     .                                                                    
019415     EJECT                                                                
019416 Z-FINIT SECTION.                                                         
019417     MOVE 'Z-FINIT                 ' TO CURRENT-SECTION                   
019418                                                                          
019420     CLOSE W21907                                                         
019500     SKIP2                                                                
019600     MOVE 'S' TO POSTSUM-OPKOD                                            
019700     CALL POSTSUM USING POSTSUM-PARM                                      
019800     .                                                                    
019900     EJECT                                                                
020000 S11-WRITE-W21907 SECTION.                                                
020010     MOVE 'S11-WRITE-W21907      ' TO CURRENT-SECTION                     
020100                                                                          
020200     WRITE OUT-RECORD FROM OUT-AREA                                       
020300                                                                          
020400     MOVE '    '     TO POSTSUM-TRANSTYP                                  
020500     MOVE 'W2190701' TO POSTSUM-FDNAMN                                    
020600     MOVE 'W21907D1' TO POSTSUM-DDNAMN2                                   
020700     CALL POSTSUM USING POSTSUM-PARM                                      
020800     .                                                                    
020900     EJECT                                                                
021000 S99-ABEND SECTION.                                                       
021100                                                                          
021200     SKIP2                                                                
021300     MOVE 'S' TO POSTSUM-OPKOD                                            
021400     CALL POSTSUM USING POSTSUM-PARM                                      
021500     CALL ABEND USING RKOD-ABEND                                          
021600     .                                                                    
021700     EJECT                                                                
021800* --- IMS SECTIONS  ---                                                   
021900                                                                          
022000     EJECT                                                                
022100 IMS-GN-WDM2             SECTION.                                         
022110     MOVE 'IMS-GN-WDM2   ' TO DBS-SECTION                                 
022200                                                                          
022300     MOVE '  GAGKGB'       TO GOOD-STATUSCODES                            
022400     CALL CBLTDLI USING GN WDM2-PCB IO-AREA-WDM2                          
022500     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
022600     PERFORM IMS-STATUSCHECK                                              
022700     .                                                                    
022800                                                                          
022810 IMS-GU-WDQ4BSEQ SECTION.                                                 
022820     MOVE 'IMS-GN-WDQ4BSEQ     ' TO DBS-SECTION                           
022830                                                                          
022831     STRING 'WDQ401  (WDQ4BSEQ>=' W-WDQ4BSEQ-MIN-X                        
022832                    '&WDQ4BSEQ<=' W-WDQ4BSEQ-MAX-X ')'                    
022860          DELIMITED BY SIZE INTO SSA1                                     
022861     MOVE '  GE'            TO GOOD-STATUSCODES                           
022880     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ401 SSA1                   
022890     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
022891     PERFORM IMS-STATUSCHECK                                              
022893     .                                                                    
022894                                                                          
022895 IMS-GN-WDQ4BSEQ SECTION.                                                 
022897     MOVE 'IMS-GN-WDQ4BSEQ     ' TO DBS-SECTION                           
022898                                                                          
022899     STRING 'WDQ401  (WDQ4BSEQ>=' W-WDQ4BSEQ-MIN-X                        
022900                    '&WDQ4BSEQ<=' W-WDQ4BSEQ-MAX-X ')'                    
022901          DELIMITED BY SIZE INTO SSA1                                     
022902     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
022903     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ401 SSA1                   
022904     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
022905     PERFORM IMS-STATUSCHECK                                              
022906     .                                                                    
022907                                                                          
022908 IMS-GU-WDA5ASEQ SECTION.                                                 
022910     MOVE 'IMS-GN-WDA5ASEQ     ' TO DBS-SECTION                           
022911                                                                          
022912     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
022913                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
022914          DELIMITED BY SIZE INTO SSA1                                     
022915     MOVE '  GE'            TO GOOD-STATUSCODES                           
022916     CALL CBLTDLI USING GU WDA5A-PCB DLI-IO-WDA501 SSA1                   
022917     MOVE WDA5A-STATUS-CODE   TO STATUS-WS                                
022918     PERFORM IMS-STATUSCHECK                                              
022919     .                                                                    
022920                                                                          
022921 IMS-GN-WDA5ASEQ SECTION.                                                 
022923     MOVE 'IMS-GN-WDA5ASEQ     ' TO DBS-SECTION                           
022924                                                                          
022925     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
022926                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
022927          DELIMITED BY SIZE INTO SSA1                                     
022928     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
022929     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-WDA501 SSA1                   
022930     MOVE WDA5A-STATUS-CODE   TO STATUS-WS                                
022931     PERFORM IMS-STATUSCHECK                                              
022932     .                                                                    
022933                                                                          
022934 IMS-GU-WDE4CSEQ SECTION.                                                 
022936     MOVE 'IMS-GN-WDE4CSEQ     ' TO DBS-SECTION                           
022937                                                                          
022938     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
022939                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
022940          DELIMITED BY SIZE INTO SSA1                                     
022941     MOVE '  GE'            TO GOOD-STATUSCODES                           
022942     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-WDE411 SSA1                   
022943     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
022944     PERFORM IMS-STATUSCHECK                                              
022945     .                                                                    
022946                                                                          
022947 IMS-GN-WDE4CSEQ SECTION.                                                 
022949     MOVE 'IMS-GN-WDE4CSEQ     ' TO DBS-SECTION                           
022950                                                                          
022951     STRING 'WDE411  (WDE4CSEQ>=' W-WDE4CSEQ-MIN-X                        
022952                    '&WDE4CSEQ<=' W-WDE4CSEQ-MAX-X ')'                    
022953          DELIMITED BY SIZE INTO SSA1                                     
022954     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
022955     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-WDE411 SSA1                   
022956     MOVE WDE4C-STATUS-CODE   TO STATUS-WS                                
022957     PERFORM IMS-STATUSCHECK                                              
022958     .                                                                    
022959                                                                          
022960 IMS-GNP-WDE401 SECTION.                                                  
022961                                                                          
022962     MOVE 'WDE401 '         TO SSA1                                       
022963     MOVE '  '              TO GOOD-STATUSCODES                           
022964     CALL CBLTDLI USING GNP WDE4C-PCB DLI-IO-WDE401 SSA1                  
022965     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
022966     PERFORM IMS-STATUSCHECK                                              
022967     .                                                                    
022968     EJECT                                                                
022970 IMS-STATUSCHECK SECTION.                                                 
023000                                                                          
023100     SET STATUS-IX TO 1                                                   
023200     SEARCH GOOD-STATUS                                                   
023300       AT END                                                             
023400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023500           DELIMITED BY SIZE INTO ERROR-TEXT                              
023600         DISPLAY ERROR-TEXT                                               
023700         CALL FELLOG                                                      
023800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
023900         CONTINUE                                                         
024000     END-SEARCH                                                           
024100     .                                                                    
