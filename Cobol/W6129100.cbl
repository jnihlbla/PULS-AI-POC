001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W6129100.                                                
001300 AUTHOR.         ARCHANA BHAT.                                            
001400 DATE-WRITTEN.   14/03/04.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001710*        SB PROGRAM                                                       
001800*        READS ALL WDJ911 SEGMENTS AND CREATES A FILE TO BE USED          
001900*        BY W6129900                                                      
002000*                                                                         
002110*        THE PROGRAM READS     WDJ9                                       
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
003402*          --- WDJ911 SEGMENT DATA                                        
003410     SELECT W61291                     ASSIGN TO W61291D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W61291                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004010*01  POST -COPY W6129101 -PRE  UT-  -L.                                   
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W6129100'.            
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700 77  WS-TISTODAT                 PIC 9(8)    VALUE ZERO.                  
004701 77  WS-DATE-YEAR                PIC 9(2)    VALUE ZERO.                  
004702 77  WS-SAVE-IDDC                PIC X(2)    VALUE SPACE.                 
004703 77  WS-CDC                      PIC X(2)    VALUE '11'.                  
004710 77  WS-KDLOC-B                  PIC 9(2)    VALUE ZERO.                  
004800 77  WS-KDLOC-P                  PIC 9(2)    VALUE ZERO.                  
004810 77  WS-KDLOC-B-CDC              PIC 9(2)    VALUE ZERO.                  
004830 77  WS-KDLOC-C                  PIC 9(2)    VALUE ZERO.                  
004840 77  WS-KDLOC-H                  PIC 9(2)    VALUE ZERO.                  
004850 77  WS-KDLOC-S                  PIC 9(2)    VALUE ZERO.                  
004860 77  WS-TOT-KDLOC                PIC 9(2)    VALUE ZERO.                  
004900     EJECT                                                                
005000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES TODAYS-DATE.                                        
005200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005400     03  TODAYS-DATE-DAY         PIC 9(2).                                
005500     EJECT                                                                
005510 77  WS-DATE-PASS-SW             PIC X       VALUE 'N'.                   
005520     88  DATE-PASS                           VALUE 'J'.                   
005540                                                                          
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
007410*01  AREA -COPY W6129101     -PRE UT-                                     
007500     EJECT                                                                
007600*    --- AREAS FOR IMS-SECTIONS                                           
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FOUND                       VALUE '  '.                  
008700     88  SEGMENT-CHANGE                      VALUE 'GA'.                  
008710     88  SEGMENT-MISSING                     VALUE 'GB'.                  
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
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ9'.                        
010002 01  DLI-IO-WDJ9.                                                         
010004     03  IO-AREA    PIC X(500).                                           
010005     03  AREA-WDJ901     REDEFINES IO-AREA.                               
010006*      05  -COPY WDJ901                                                   
010007     SKIP3                                                                
010008     03  AREA-WDJ911     REDEFINES IO-AREA.                               
010009*      05  -COPY WDJ911                                                   
010010     SKIP3                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDJ9-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING WDJ9-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDJ9-PCB.                                      
011100                                                                          
011200     PERFORM A-INIT                                                       
011210                                                                          
011300* READS ALL WDJ911 SEGMENTS                                               
011401     PERFORM IMS-GET-WDJ9                                                 
011402     PERFORM UNTIL SEGMENT-MISSING                                        
011403       EVALUATE WDJ9-SEG-NAME-FB                                          
011404         WHEN 'WDJ901'                                                    
011405           MOVE ART-IDARTNR   TO UT-IDARTNR                               
011406         WHEN 'WDJ911'                                                    
011407           IF HIST-DASTODAT > 0                                           
011410              PERFORM B-CHECK-WDJ911                                      
011415           END-IF                                                         
011416       END-EVALUATE                                                       
011417       PERFORM IMS-GET-WDJ9                                               
011418                                                                          
011419* INITIALIZE COUNTERS WHEN PART NUMBER CHANGES                            
011420       IF SEGMENT-CHANGE                                                  
011421          PERFORM S10-INITIALIZE-FIELDS                                   
011422       END-IF                                                             
011430     END-PERFORM                                                          
011500     PERFORM Z-FINIT                                                      
011600                                                                          
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012100 A-INIT SECTION.                                                          
012301                                                                          
012310     OPEN OUTPUT W61291                                                   
012400                                                                          
012500     ACCEPT TODAYS-DATE  FROM DATE                                        
012610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012611* CALCULATE MACHINE YEAR - 6                                              
012620     COMPUTE WS-DATE-YEAR = TODAYS-DATE-YEAR - 6                          
012800     .                                                                    
012900     EJECT                                                                
013000 B-CHECK-WDJ911 SECTION.                                                  
013001                                                                          
013012     IF HIST-IDDC = WS-SAVE-IDDC                                          
013013        CONTINUE                                                          
013014     ELSE                                                                 
013015        MOVE HIST-IDDC  TO WS-SAVE-IDDC                                   
013016                                                                          
013017* INITIALIZE COUNTERS WHEN DC CHANGES                                     
013018        PERFORM S10-INITIALIZE-FIELDS                                     
013023     END-IF                                                               
013024                                                                          
013025     IF HIST-IDDC = WS-CDC                                                
013026* DC 11                                                                   
013027        PERFORM BA-CHECK-KDLOC-CDC                                        
013028     ELSE                                                                 
013029* ALL OTHER DC                                                            
013030        PERFORM BB-CHECK-KDLOC-OTHER                                      
013031     END-IF                                                               
013032                                                                          
013033     IF DATE-PASS                                                         
013034        PERFORM BC-MOVE-FIELDS                                            
013035        PERFORM S21-WRITE-W61291                                          
013036        MOVE NOO        TO WS-DATE-PASS-SW                                
013037     END-IF                                                               
013038     .                                                                    
013039     EJECT                                                                
013080                                                                          
013100 BA-CHECK-KDLOC-CDC SECTION.                                              
013101                                                                          
013102* POSSIBLE KDLOC VALUES FOR DC 11 ARE B,C(OR P),H,S                       
013103     EVALUATE TRUE                                                        
013104       WHEN  HIST-KDLOC = 'B'                                             
013105         ADD +1        TO WS-KDLOC-B-CDC                                  
013106* IF QTY OF KDLOC B > 5, CHECK THE STOP DATE AND WRITE TO OUTPUT          
013107         IF WS-KDLOC-B-CDC > 5                                            
013108            PERFORM S20-CHECK-DASTODAT                                    
013109         END-IF                                                           
013110       WHEN  HIST-KDLOC = 'C' OR HIST-KDLOC = 'P'                         
013112         ADD +1        TO WS-KDLOC-C                                      
013113         ADD +1        TO WS-TOT-KDLOC                                    
013114       WHEN  HIST-KDLOC = 'H'                                             
013115         ADD +1        TO WS-KDLOC-H                                      
013116         ADD +1        TO WS-TOT-KDLOC                                    
013117       WHEN  HIST-KDLOC = 'S'                                             
013118         ADD +1        TO WS-KDLOC-S                                      
013119         ADD +1        TO WS-TOT-KDLOC                                    
013126     END-EVALUATE                                                         
013127                                                                          
013128** IF TOTAL QTY OF KDLOC C,H,S IS GREATER THAN 10 AND                     
013129** INDIVIDUALLY C > 6 OR H > 2 OR S > 2, CHECK THE STOP DATE              
013130** AND WRITE TO OUTPUT                                                    
013131     IF WS-TOT-KDLOC > 10                                                 
013132        IF (WS-KDLOC-C > 6) AND                                           
013133           (HIST-KDLOC = 'C' OR HIST-KDLOC = 'P')                         
013134        OR WS-KDLOC-H > 2 AND HIST-KDLOC = 'H'                            
013135        OR WS-KDLOC-S > 2 AND HIST-KDLOC = 'S'                            
013136           PERFORM S20-CHECK-DASTODAT                                     
013137        END-IF                                                            
013138     END-IF                                                               
013139     .                                                                    
013140     EJECT                                                                
013141                                                                          
013142 BB-CHECK-KDLOC-OTHER SECTION.                                            
013143                                                                          
013144* POSSIBLE KDLOC VALUES FOR OTHER DC ARE B AND P                          
013145     EVALUATE TRUE                                                        
013146       WHEN  HIST-KDLOC = 'B'                                             
013147         ADD +1        TO WS-KDLOC-B                                      
013148* QTY FOR KDLOC B > 5, CHECK THE STOP DATE AND WRITE TO OUTPUT            
013149         IF WS-KDLOC-B > 5                                                
013150            PERFORM S20-CHECK-DASTODAT                                    
013151         END-IF                                                           
013152* QTY FOR KDLOC P > 5, CHECK THE STOP DATE AND WRITE TO OUTPUT            
013153       WHEN  HIST-KDLOC = 'P'                                             
013154         ADD +1        TO WS-KDLOC-P                                      
013155         IF WS-KDLOC-P > 5                                                
013156            PERFORM S20-CHECK-DASTODAT                                    
013157         END-IF                                                           
013158     END-EVALUATE                                                         
013159     .                                                                    
013160     EJECT                                                                
013161                                                                          
013162 BC-MOVE-FIELDS SECTION.                                                  
013163                                                                          
013164     MOVE HIST-IDDC            TO UT-IDDC                                 
013165     MOVE HIST-DASTADAT-9KOMPL TO UT-DASTADAT-9KOMPL                      
013166     MOVE HIST-TISTATID-9KOMPL TO UT-TISTATID-9KOMPL                      
013167     MOVE HIST-ADLAGOMR        TO UT-ADLAGOMR                             
013168     MOVE HIST-ADGANG          TO UT-ADGANG                               
013169     MOVE HIST-ADPLATS         TO UT-ADPLATS                              
013170     .                                                                    
013171     EJECT                                                                
013172 Z-FINIT SECTION.                                                         
013180     CLOSE W61291                                                         
013201     SKIP2                                                                
013202     MOVE 'S' TO POSTSUM-OPKOD                                            
013210     CALL POSTSUM USING POSTSUM-PARM                                      
013300     .                                                                    
013501     EJECT                                                                
013502 S10-INITIALIZE-FIELDS SECTION.                                           
013503     MOVE ZERO       TO WS-KDLOC-B                                        
013504                        WS-KDLOC-P                                        
013505                        WS-KDLOC-B-CDC                                    
013506                        WS-KDLOC-C                                        
013507                        WS-KDLOC-H                                        
013508                        WS-KDLOC-S                                        
013509                        WS-TOT-KDLOC                                      
013510     MOVE NOO        TO WS-DATE-PASS-SW                                   
013512     .                                                                    
013513     EJECT                                                                
013514 S20-CHECK-DASTODAT SECTION.                                              
013515                                                                          
013516* CHECK IF MACHINE YEAR - 6 (WS-DATE-YEAR) IS GREATER THAN                
013517* STOP DATE IN WDJ911. IF YES, RECORD IS TO BE WRITTEN TO OUTPUT          
013518     MOVE HIST-DASTODAT       TO WS-TISTODAT                              
013519     IF WS-DATE-YEAR > WS-TISTODAT (3:2)                                  
013520        MOVE YES              TO WS-DATE-PASS-SW                          
013521     END-IF                                                               
013522     .                                                                    
013523     EJECT                                                                
013524 S21-WRITE-W61291 SECTION.                                                
013525                                                                          
013526     WRITE UT-POST FROM UT-AREA                                           
013527                                                                          
013528     MOVE '    '     TO POSTSUM-TRANSTYP                                  
013529     MOVE 'W61291'   TO POSTSUM-FDNAMN                                    
013530     MOVE 'W61291D1' TO POSTSUM-DDNAMN2                                   
013531     CALL POSTSUM USING POSTSUM-PARM                                      
013540     .                                                                    
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
014602 IMS-GET-WDJ9   SECTION.                                                  
014603                                                                          
014604     CALL CBLTDLI USING GN WDJ9-PCB DLI-IO-WDJ9                           
014605     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
014606     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
014607     PERFORM IMS-STATUSCHECK                                              
014610     .                                                                    
014700     EJECT                                                                
014800 IMS-STATUSCHECK SECTION.                                                 
014900                                                                          
015000     SET STATUS-IX TO 1                                                   
015100     SEARCH GOOD-STATUS                                                   
015200       AT END                                                             
015300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015400           DELIMITED BY SIZE INTO ERROR-TEXT                              
015500         DISPLAY ERROR-TEXT                                               
015600         CALL FELLOG                                                      
015700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
015800         CONTINUE                                                         
015900     END-SEARCH                                                           
016000     .                                                                    
