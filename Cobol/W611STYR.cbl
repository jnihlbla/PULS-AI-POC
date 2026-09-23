000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W611STYR.                                                
000500*AUTHOR.         ROSMARIE CLASON - GUIDE DATAKONSULT AB                   
000600*DATE-WRITTEN.   92/02/25.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*    FUNCTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET LÄSER STYR-REGISTRET FÖR ATT FÅ REDA PÅ               
001200*        VILKEN FÖRBEHANDLIGSGRUPP OCH/ELLER FÖRPACKNINGSGRUPP            
001300*        EN VISS ARTIKEL HAR. PROGRAMMET ÄR ETT GEN. SUBPROGRAM           
001400*        SOM SKA ANROPAS FRÅN DIVERSE PROGRAM.                            
001500*                                                                         
001600*        INDATA FÅS VIA COPYTEXT OCH ANSES VARA RIKTIGA.                  
001700*                                                                         
001800*                                                                         
001900*        THE PROGRAM READS     W6HANB (W6G1)                              
002000*                              COPYTXT: W6GX01                            
002100*                                       W6GX6032                          
002200*                                       W6GX6034                          
002300*                                       W6GX6036                          
002400*                                       W6GX6038                          
002500*        THE PROGRAM READS     W6PLAA (W6G1)                              
002600*                              COPYTXT: W6GX6006                          
002700*    ABENDCODES:                                                          
002800*        U0016 -  . . . .                                                 
002900*        U1000 -  . . . .                                                 
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     EJECT                                                                
003900                                                                          
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     EJECT                                                                
004400                                                                          
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W611STYR'.            
005000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005100 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  ART-IX                      PIC 9(4)    VALUE ZERO.                  
005500     EJECT                                                                
005600                                                                          
005700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES TODAYS-DATE.                                        
005900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006100     03  TODAYS-DATE-DAY         PIC 9(2).                                
006200     EJECT                                                                
006300*      --- VALID IDDC CODES                                               
006400*                                                                         
006500*01    -COPY WWDC99                                                       
006600       EJECT                                                              
006700                                                                          
006800 01  GENERAL-SUBPROGRAM.                                                  
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     SKIP2                                                                
007300     EJECT                                                                
007400                                                                          
007500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007600     SKIP2                                                                
007700 01  ERRTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000     EJECT                                                                
008100                                                                          
008200*    --- AREAS FOR IMS-SECTIONS                                           
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  KEYS-TILL-DLI.                                                       
008700     03  W-W6GXKEY-X.                                                     
008800         05  W-IDHTYP            PIC X(4).                                
008900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
009000     03  W-W6GXKEY-6005-X.                                                
009100         05  FILLER              PIC X(4)    VALUE '6005'.                
009200         05  W-W6GXKEY-6005-IDDC PIC X(2)    VALUE SPACE.                 
009300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
009400     03  W-W6GXKEY-6031-X.                                                
009500         05  FILLER              PIC X(4)    VALUE '6031'.                
009600         05  W-W6GXKEY-6031-IDDC PIC X(2)    VALUE SPACE.                 
009700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
009800     03  W-KDSEGKEY-X.                                                    
009900         05  W-KDSEGKEY          PIC X(01)   VALUE SPACE.                 
010000     03  W-ADINLOMR              PIC X(4)    VALUE SPACE.                 
010100 01  W-IDARTNR-FOM-X.                                                     
010200     03  W-IDARTNR-FOM           PIC S9(9)   VALUE +0 COMP-3.             
010300 01  W-IDARTNR-TOM-X.                                                     
010400     03  W-IDARTNR-TOM           PIC S9(9)   VALUE +0 COMP-3.             
010500 01  W-IDFKNGRP-FOM-X.                                                    
010600     03  W-IDFKNGRP-FOM          PIC S9(5)   VALUE +0 COMP-3.             
010700 01  W-IDFKNGRP-TOM-X.                                                    
010800     03  W-IDFKNGRP-TOM          PIC S9(5)   VALUE +0 COMP-3.             
010900 01  W-IDLEVNR-X.                                                         
011000     03  W-IDLEVNR               PIC X(5)    VALUE SPACE.                 
011100 01  W-BEFT-FOM-X.                                                        
011200     03  W-BEFT-FOM              PIC S9(3)   VALUE +0 COMP-3.             
011300 01  W-BEFT-TOM-X.                                                        
011400     03  W-BEFT-TOM              PIC S9(3)   VALUE +0 COMP-3.             
011500     EJECT                                                                
011600                                                                          
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FOUND                       VALUE '  '.                  
012000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012200     EJECT                                                                
012300                                                                          
012400     SKIP2                                                                
012500 77  TRAEFF-SW                   PIC X(1).                                
012600  88 TRAEFF-YES                              VALUE 'J'.                   
012700  88 TRAEFF-NEJ                              VALUE 'N'.                   
012800     EJECT                                                                
012900                                                                          
013000 01  GOOD-STATUSCODES.                                                    
013100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     EJECT                                                                
013300                                                                          
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800                                                                          
013900*    --- IMS FUNCTION CODES                                               
014000*01  -COPY W0003                                                          
014100     EJECT                                                                
014200                                                                          
014300*    ---  DLI INPUT-OUTPUT AREA                                           
014400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014500     SKIP3                                                                
014600 01  DLI-IO-AREA-01.                                                      
014700     03  IO-AREA-01                 PIC X(30)   VALUE SPACE.              
014800     SKIP3                                                                
014900     03  W6HANB01 REDEFINES IO-AREA-01.                                   
015000*        05  -COPY W6GX01                                                 
015100     EJECT                                                                
015200                                                                          
015300     SKIP3                                                                
015400 01  DLI-IO-AREA-11.                                                      
015500     03  IO-AREA-11                 PIC X(100) VALUE SPACE.               
015600     SKIP3                                                                
015700     03  W6HANB11 REDEFINES IO-AREA-11.                                   
015800*        05  -COPY W6GX6032                                               
015900     03  W6HANB12 REDEFINES IO-AREA-11.                                   
016000*        05  -COPY W6GX6034                                               
016100     03  W6HANB13 REDEFINES IO-AREA-11.                                   
016200*        05  -COPY W6GX6036                                               
016300     03  W6HANB14 REDEFINES IO-AREA-11.                                   
016400*        05  -COPY W6GX6038                                               
016500     EJECT                                                                
016600 01  DLI-IO-AREA-PLAA.                                                    
016700     03  PLAA-IO-AREA               PIC X(250) VALUE SPACE.               
016800     SKIP3                                                                
016900     03  W6PLAA11 REDEFINES PLAA-IO-AREA.                                 
017000*        05  -COPY W6GX6006                                               
017100     EJECT                                                                
017200                                                                          
017300 LINKAGE SECTION.                                                         
017400                                                                          
017500*01  -COPY W611STYR                                                       
017600     EJECT                                                                
017700                                                                          
017800*01  -COPY W0008    -PRE HANB-                                            
017900     05  FILLER                  PIC X.                                   
018000     EJECT                                                                
018100*01  -COPY W0008    -PRE PLAA-                                            
018200     05  FILLER                  PIC X.                                   
018300     EJECT                                                                
018400                                                                          
018500 PROCEDURE DIVISION  USING STYR-W611STYR                                  
018600                           HANB-PCB PLAA-PCB.                             
018700                                                                          
018800     SKIP2                                                                
018900     PERFORM A-INIT                                                       
019000                                                                          
019100     PERFORM B-BEARB                                                      
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700                                                                          
019800*----------------------------------------------------------------*        
019900 A-INIT SECTION.                                                          
020000     SKIP2                                                                
020100     ACCEPT TODAYS-DATE  FROM DATE                                        
020200     MOVE SPACE            TO STYR-UTDATA                                 
020300     .                                                                    
020400     EJECT                                                                
020500                                                                          
020600*----------------------------------------------------------------*        
020700 B-BEARB SECTION.                                                         
020800                                                                          
020900     PERFORM BA-LAES-HANB-ROT                                             
021000     IF STYR-KDSVAR-OK                                                    
021100       IF STYR-IDARTNR  > ZERO OR                                         
021200          STYR-IDFKNGRP > ZERO OR                                         
021300          STYR-IDLEVNR  > SPACE                                           
021400          MOVE NEJ        TO TRAEFF-SW                                    
021500          MOVE STYR-IDARTNR   TO W-IDARTNR-FOM                            
021600                                 W-IDARTNR-TOM                            
021700          MOVE STYR-IDFKNGRP  TO W-IDFKNGRP-FOM                           
021800                                 W-IDFKNGRP-TOM                           
021900          MOVE STYR-IDLEVNR   TO W-IDLEVNR                                
022000          MOVE STYR-BEFT      TO W-BEFT-FOM                               
022100                                 W-BEFT-TOM                               
022200          IF W-IDARTNR-FOM > ZERO                                         
022300             PERFORM BB-SEARCH-ARTNR                                      
022400          END-IF                                                          
022500          IF TRAEFF-NEJ                                                   
022600            IF W-IDFKNGRP-FOM > ZERO                                      
022700               PERFORM BC-SEARCH-FKNGRP                                   
022800            END-IF                                                        
022900            IF TRAEFF-NEJ                                                 
023000              IF W-IDLEVNR > SPACE                                        
023100                 PERFORM BD-SEARCH-LEVNR                                  
023200              END-IF                                                      
023300            END-IF                                                        
023400          END-IF                                                          
023500          IF TRAEFF-NEJ                                                   
023600             MOVE STYR-IDDC  TO WS-IDDC                                   
023700             EVALUATE TRUE                                                
023800               WHEN   CDC-SE                                              
023900                  MOVE 'FB?'   TO STYR-ADINLOMR-FB                        
024000               WHEN   CDC-TR                                              
024100                  MOVE 'INS?'  TO STYR-ADINLOMR-FB                        
024200               WHEN   NDC-US-RU                                           
024300                  MOVE 'U41?'  TO STYR-ADINLOMR-FB                        
024600               WHEN   NDC-US-LA                                           
024700                  MOVE 'U43?'  TO STYR-ADINLOMR-FB                        
024800               WHEN   NDC-US-SE                                           
024900                  MOVE 'U44?'  TO STYR-ADINLOMR-FB                        
025000               WHEN   NDC-US-CH                                           
025100                  MOVE 'U45?'  TO STYR-ADINLOMR-FB                        
025200               WHEN   NDC-US-JA                                           
025300                  MOVE 'U46?'  TO STYR-ADINLOMR-FB                        
025310               WHEN   NDC-US-DA                                           
025320                  MOVE 'U47?'  TO STYR-ADINLOMR-FB                        
025400               WHEN   NDC-CA                                              
025500                  MOVE 'C51?'  TO STYR-ADINLOMR-FB                        
025510               WHEN   NDC-BR                                              
025520                  MOVE 'B52?'  TO STYR-ADINLOMR-FB                        
025530               WHEN   NDC-MX                                              
025540                  MOVE 'M53?'  TO STYR-ADINLOMR-FB                        
025600               WHEN   NDC-JP-6A                                           
025700                  MOVE 'J6A?'  TO STYR-ADINLOMR-FB                        
025710               WHEN   NDC-JP-61                                           
025720                  MOVE 'J61?'  TO STYR-ADINLOMR-FB                        
025800               WHEN   NDC-AU                                              
025900                  MOVE 'A62?'  TO STYR-ADINLOMR-FB                        
025901               WHEN   NDC-TH-63                                           
025902                  MOVE 'T63?'  TO STYR-ADINLOMR-FB                        
025901               WHEN   NDC-TH-93                                           
025902                  MOVE 'T93?'  TO STYR-ADINLOMR-FB                        
025903               WHEN   NDC-TW                                              
025904                  MOVE 'T64?'  TO STYR-ADINLOMR-FB                        
025910               WHEN   NDC-KR                                              
025920                  MOVE 'K65?'  TO STYR-ADINLOMR-FB                        
025921               WHEN   NDC-MY                                              
025922                  MOVE 'M66?'  TO STYR-ADINLOMR-FB                        
025923               WHEN   NDC-ZA                                              
025924                  MOVE 'Z85?'  TO STYR-ADINLOMR-FB                        
025923               WHEN   NDC-TR                                              
025924                  MOVE 'T86?'  TO STYR-ADINLOMR-FB                        
025925               WHEN   NDC-AE                                              
025926                  MOVE 'A87?'  TO STYR-ADINLOMR-FB                        
025927               WHEN   NDC-US-BAT                                          
025928                  MOVE 'U92?'  TO STYR-ADINLOMR-FB                        
026000               WHEN   NDC-IN                                              
026100                  MOVE 'I67?'  TO STYR-ADINLOMR-FB                        
026200               WHEN   NDC-CN-71                                           
026300                  MOVE 'C71?'  TO STYR-ADINLOMR-FB                        
026400               WHEN   NDC-CN-72                                           
026500                  MOVE 'C72?'  TO STYR-ADINLOMR-FB                        
026600               WHEN   NDC-CN-73                                           
026700                  MOVE 'C73?'  TO STYR-ADINLOMR-FB                        
026800               WHEN   NDC-CN-74                                           
026900                  MOVE 'C74?'  TO STYR-ADINLOMR-FB                        
027000             END-EVALUATE                                                 
027100          END-IF                                                          
027200       END-IF                                                             
027300       IF STYR-BEFT > ZERO    AND (STYR-IDLEVNR NOT = '3324 ')            
027400          MOVE STYR-BEFT  TO W-BEFT-FOM                                   
027500                             W-BEFT-TOM                                   
027600          MOVE NEJ        TO TRAEFF-SW                                    
027700          PERFORM BE-SEARCH-BEFT                                          
027800          IF TRAEFF-NEJ                                                   
027900            MOVE SPACE   TO STYR-ADINLOMR-FP                              
028000          ELSE                                                            
028100**          IF STYR-BEFT = 40                                             
028200**            MOVE STYR-ADINLOMR-FP  TO STYR-ADINLOMR-FB                  
028300**          ELSE                                                          
028400              IF STYR-ADINLOMR-FP = 'FB  '                                
028500                IF STYR-ADINLOMR-FB = SPACE                               
028600                  CONTINUE                                                
028700                ELSE                                                      
028800                  MOVE STYR-ADINLOMR-FB TO STYR-ADINLOMR-FP               
028900                  IF (STYR-ADINLOMR-FP(1:3) = '573' OR '583' OR           
029000                       '588' OR '589' OR 'EM1' OR '512' OR 'SVS')         
029100                    MOVE 'P'              TO STYR-ADINLOMR-FP(4:1)        
029200                  END-IF                                                  
029300                END-IF                                                    
029400              ELSE                                                        
029500                IF STYR-ADINLOMR-FB NOT = SPACE                           
029600                  MOVE STYR-ADINLOMR-FB TO W-ADINLOMR                     
029700                  MOVE STYR-IDDC        TO W-W6GXKEY-6005-IDDC            
029800                  PERFORM IMS-GU-6006                                     
029900                  IF 6006-KDINLOMR = 'FBP'                                
030000                    MOVE STYR-ADINLOMR-FB TO STYR-ADINLOMR-FP             
030100                    IF (STYR-ADINLOMR-FP(1:3) = '573' OR '583' OR         
030200                       '588' OR '589' OR 'EM1' OR '512' OR 'SVS')         
030300                      MOVE 'P'          TO STYR-ADINLOMR-FP(4:1)          
030400                    END-IF                                                
030500                  END-IF                                                  
030600                END-IF                                                    
030700              END-IF                                                      
030800**          END-IF                                                        
030900          END-IF                                                          
031000        END-IF                                                            
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400                                                                          
031500*----------------------------------------------------------------*        
031600 BA-LAES-HANB-ROT SECTION.                                                
031700     MOVE STYR-IDDC    TO W-W6GXKEY-6031-IDDC                             
031800     PERFORM IMS-GU-HANB-6031                                             
031900     IF SEGMENT-FOUND                                                     
032000       CONTINUE                                                           
032100     ELSE                                                                 
032200       MOVE 'F' TO STYR-KDSVAR                                            
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600*----------------------------------------------------------------*        
032700 BB-SEARCH-ARTNR SECTION.                                                 
032800                                                                          
032900     PERFORM IMS-GNP-HANB-6032                                            
033000     IF SEGMENT-FOUND                                                     
033100        MOVE YES     TO TRAEFF-SW                                         
033200        MOVE 6032-ADINLOMR TO STYR-ADINLOMR-FB                            
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600                                                                          
033700*----------------------------------------------------------------*        
033800 BC-SEARCH-FKNGRP SECTION.                                                
033900                                                                          
034000     PERFORM IMS-GNP-HANB-6034                                            
034100     IF SEGMENT-FOUND                                                     
034200        MOVE YES     TO TRAEFF-SW                                         
034300        MOVE 6034-ADINLOMR TO STYR-ADINLOMR-FB                            
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700                                                                          
034800*----------------------------------------------------------------*        
034900 BD-SEARCH-LEVNR SECTION.                                                 
035000                                                                          
035100     PERFORM IMS-GNP-HANB-6036                                            
035200     IF SEGMENT-FOUND                                                     
035300        MOVE YES     TO TRAEFF-SW                                         
035400        MOVE 6036-ADINLOMR TO STYR-ADINLOMR-FB                            
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900*----------------------------------------------------------------*        
036000 BE-SEARCH-BEFT SECTION.                                                  
036100                                                                          
036200     PERFORM IMS-GNP-HANB-6038                                            
036300     IF SEGMENT-FOUND                                                     
036400        MOVE YES     TO TRAEFF-SW                                         
036500        MOVE 6038-ADINLOMR TO STYR-ADINLOMR-FP                            
036600     END-IF                                                               
036700     .                                                                    
036800     EJECT                                                                
036900                                                                          
037000*----------------------------------------------------------------*        
037100* --- IMS SECTIONS  ---                                                   
037200*----------------------------------------------------------------*        
037300     SKIP3                                                                
037400 IMS-GU-HANB-6031 SECTION.                                                
037500                                                                          
037600     STRING 'W6HANB01(W6GXKEY  =' W-W6GXKEY-6031-X ')'                    
037700             DELIMITED BY SIZE INTO SSA1                                  
037800     MOVE '  GE'               TO GOOD-STATUSCODES                        
037900     CALL CBLTDLI USING GU     HANB-PCB                                   
038000                               DLI-IO-AREA-01                             
038100                               SSA1                                       
038200     MOVE HANB-STATUS-CODE     TO STATUS-WS                               
038300     PERFORM IMS-STATUSCHECK                                              
038400     .                                                                    
038500     EJECT                                                                
038600 IMS-GNP-HANB-6032 SECTION.                                               
038700                                                                          
038800     STRING 'W6HANB11(IDARTNRF<=' W-IDARTNR-FOM-X                         
038900                   '&IDARTNRT>=' W-IDARTNR-TOM-X ')'                      
039000             DELIMITED BY SIZE INTO SSA1                                  
039100     MOVE '  GE'               TO GOOD-STATUSCODES                        
039200     CALL CBLTDLI USING GNP    HANB-PCB                                   
039300                               IO-AREA-11                                 
039400                               SSA1                                       
039500     MOVE HANB-STATUS-CODE     TO STATUS-WS                               
039600     PERFORM IMS-STATUSCHECK                                              
039700     .                                                                    
039800     EJECT                                                                
039900 IMS-GNP-HANB-6034 SECTION.                                               
040000                                                                          
040100     STRING 'W6HANB12(IDFKNGRF<=' W-IDFKNGRP-FOM-X                        
040200                   '&IDFKNGRT>=' W-IDFKNGRP-TOM-X ')'                     
040300             DELIMITED BY SIZE INTO SSA1                                  
040400     MOVE '  GE'               TO GOOD-STATUSCODES                        
040500     CALL CBLTDLI USING GNP    HANB-PCB                                   
040600                               IO-AREA-11                                 
040700                               SSA1                                       
040800     MOVE HANB-STATUS-CODE     TO STATUS-WS                               
040900     PERFORM IMS-STATUSCHECK                                              
041000     .                                                                    
041100     EJECT                                                                
041200 IMS-GNP-HANB-6036 SECTION.                                               
041300                                                                          
041400     STRING 'W6HANB13(IDLEVNR  =' W-IDLEVNR-X ')'                         
041500             DELIMITED BY SIZE INTO SSA1                                  
041600     MOVE '  GE'               TO GOOD-STATUSCODES                        
041700     CALL CBLTDLI USING GNP    HANB-PCB                                   
041800                               IO-AREA-11                                 
041900                               SSA1                                       
042000     MOVE HANB-STATUS-CODE     TO STATUS-WS                               
042100     PERFORM IMS-STATUSCHECK                                              
042200     .                                                                    
042300     EJECT                                                                
042400 IMS-GNP-HANB-6038 SECTION.                                               
042500                                                                          
042600     STRING 'W6HANB14(BEFTF   <=' W-BEFT-FOM-X                            
042700                   '&BEFTT   >=' W-BEFT-TOM-X ')'                         
042800             DELIMITED BY SIZE INTO SSA1                                  
042900     MOVE '  GE'               TO GOOD-STATUSCODES                        
043000     CALL CBLTDLI USING GNP    HANB-PCB                                   
043100                               IO-AREA-11                                 
043200                               SSA1                                       
043300     MOVE HANB-STATUS-CODE     TO STATUS-WS                               
043400     PERFORM IMS-STATUSCHECK                                              
043500     .                                                                    
043600     EJECT                                                                
043700 IMS-GU-6006 SECTION.                                                     
043800                                                                          
043900     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
044000             DELIMITED BY SIZE INTO SSA1                                  
044100     STRING 'W6PLAA11(ADINLOMR =' W-ADINLOMR ')'                          
044200             DELIMITED BY SIZE INTO SSA2                                  
044300     MOVE '  '               TO GOOD-STATUSCODES                          
044400     CALL CBLTDLI USING GU     PLAA-PCB                                   
044500                               PLAA-IO-AREA                               
044600                               SSA1                                       
044700                               SSA2                                       
044800     MOVE PLAA-STATUS-CODE     TO STATUS-WS                               
044900     PERFORM IMS-STATUSCHECK                                              
045000     .                                                                    
045100     EJECT                                                                
045200 IMS-STATUSCHECK SECTION.                                                 
045300     SKIP2                                                                
045400     SET STATUS-IX TO 1                                                   
045500     SEARCH GOOD-STATUS                                                   
045600       AT END                                                             
045700         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
045800         DELIMITED BY SIZE INTO ERRTEXT                                   
045900         CALL FELLOG                                                      
046000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
046100         CONTINUE                                                         
046200     END-SEARCH                                                           
046300     .                                                                    
