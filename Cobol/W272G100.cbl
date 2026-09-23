000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W272G100.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   16/03/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        REFILL ORDERS                                                    
000900*                                                                         
001000*        THE PROGRAM READS     K611                                       
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
002400*          --- REFILL ORDERS                                              
002500     SELECT W272G101                   ASSIGN TO W272G1D1.                
002510     SELECT W272G102                   ASSIGN TO W272G1D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W272G101                                                             
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  RECORD -COPY W272G1 -PRE  OUT1- -L.                                  
003600     EJECT                                                                
003610 FD  W272G102                                                             
003620     RECORDING       F                                                    
003630     BLOCK CONTAINS  0.                                                   
003640                                                                          
003650*01  RECORD -COPY W27111 -PRE  OUT2- -L.                                  
003660     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W272G100'.            
004000 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004100 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400 77  W-WRITE-W272G1              PIC X       VALUE 'N'.                   
004500 77  W-IDDC-REF-ALL-SPACE        PIC X       VALUE 'N'.                   
004600     EJECT                                                                
004700*    --- VALID IDDC CODES                                                 
004800*01  -COPY WWDC99                                                         
004900     EJECT                                                                
005000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES TODAYS-DATE.                                        
005200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005400     03  TODAYS-DATE-DAY         PIC 9(2).                                
005500     EJECT                                                                
005600 01  GENERAL-SUBPROGRAMS.                                                 
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  ERROR-TEXT.                                                          
007000     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  OUT1-AREA-START              PIC X(24)   VALUE                       
007800                                 'OUT1-AREA-START  '.                     
007900     SKIP2                                                                
008000                                                                          
008100*01  AREA -COPY W272G1     -PRE OUT1-                                     
008200     EJECT                                                                
008210 01  OUT2-AREA-START              PIC X(24)   VALUE                       
008220                                 'OUT2-AREA-START  '.                     
008230     SKIP2                                                                
008240                                                                          
008250*01  AREA -COPY W27111     -PRE OUT2-                                     
008260     EJECT                                                                
008300*    --- AREAS FOR IMS-SECTIONS                                           
008400*                                                                         
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  KEYS-FOR-DLI.                                                        
008900     03  W-WDK6H1KY-MIN-X.                                                
009000       05  W-IDDC-REF-MIN      PIC X(02)     VALUE LOW-VALUE.             
009100       05  FILLER              PIC X(05)     VALUE LOW-VALUE.             
009200     03  W-WDK6H1KY-MAX-X.                                                
009300       05  W-IDDC-REF-MAX      PIC X(02)     VALUE HIGH-VALUE.            
009400       05  FILLER              PIC X(05)     VALUE HIGH-VALUE.            
009500                                                                          
009600     03  W-IDDC-MIN-X.                                                    
009700         05  W-IDDC-MIN          PIC X(2)    VALUE '71'.                  
009800     03  W-IDDC-MAX-X.                                                    
009900         05  W-IDDC-MAX          PIC X(2)    VALUE '79'.                  
010000                                                                          
010100     03  W-IDARTNR-X.                                                     
010200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010300     03  W-IDDC-X.                                                        
010400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010410                                                                          
010500     03  W-IDDC-B6-X.                                                     
010600         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
010610     03  W-IDDC-B616-X.                                                   
010620         05  W-IDDC-B616         PIC X(2)   VALUE SPACE.                  
010700                                                                          
010800     SKIP2                                                                
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FOUND                       VALUE '  '.                  
011200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011400     SKIP2                                                                
011500 01  GOOD-STATUSCODES.                                                    
011600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01  SSA1                        PIC X(64).                               
011900 01  SSA2                        PIC X(64).                               
012000     EJECT                                                                
012100*    --- IMS FUNCTION CODES                                               
012200*01  -COPY W0003                                                          
012300     EJECT                                                                
012400*    ---  DLI INPUT-OUTPUT AREA                                           
012500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6H1'.                      
012600 01  DLI-IO-WDK6H1.                                                       
012700*    03  -COPY WDK6H1                                                     
012800     EJECT                                                                
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013000 01  DLI-IO-WDK601.                                                       
013100*    03  -COPY WDK601                                                     
013200     EJECT                                                                
013210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013220 01  DLI-IO-WDK611.                                                       
013230*    03  -COPY WDK611                                                     
013240     EJECT                                                                
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
013400 01  DLI-IO-WDK701.                                                       
013500*    03  -COPY WDK701                                                     
013600     EJECT                                                                
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
013710 01  DLI-IO-WDK711.                                                       
013720*    03  -COPY WDK711                                                     
013730     EJECT                                                                
013740 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB616'.                      
013750 01  DLI-IO-WDB616.                                                       
013760*    03  -COPY WDB616                                                     
013770     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900                                                                          
014000                                                                          
014100*01  -COPY W0008  -PRE K6H1-                                              
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400*01  -COPY W0008  -PRE WDK6-                                              
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700*01  -COPY W0008  -PRE WDK7-                                              
014800     05  FILLER                  PIC X.                                   
014810*01  -COPY W0008  -PRE WDB6-                                              
014820     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING K6H1-PCB WDK6-PCB WDK7-PCB WDB6-PCB.           
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING K6H1-PCB WDK6-PCB WDK7-PCB WDB6-PCB.           
015300                                                                          
015400     PERFORM A-INIT                                                       
015500                                                                          
015600     PERFORM IMS-GN-WDK6H1                                                
015700     PERFORM UNTIL SEGMENT-MISSING                                        
015800                                                                          
015900        MOVE SEQH-IDARTNR       TO W-IDARTNR                              
015910        MOVE SEQH-IDDC-REF      TO W-IDDC                                 
016000        PERFORM IMS-GU-WDK711                                             
016010        MOVE NOO                TO W-WRITE-W272G1                         
016100        IF SEGMENT-FOUND                                                  
017000          IF SLAG-IDLEVNR = '1441'                                        
017010            PERFORM B-WRITE-W272G1                                        
017600          END-IF                                                          
018600        END-IF                                                            
018700                                                                          
018800        PERFORM IMS-GN-WDK6H1                                             
018900                                                                          
019000     END-PERFORM                                                          
019100                                                                          
019200                                                                          
019300     PERFORM Z-FINIT                                                      
019400                                                                          
019500     MOVE ZERO TO RETURN-CODE                                             
019600     GOBACK                                                               
019700     .                                                                    
019800     EJECT                                                                
019900 A-INIT SECTION.                                                          
020000                                                                          
020100     OPEN OUTPUT W272G101                                                 
020110                 W272G102                                                 
020200                                                                          
020300     ACCEPT TODAYS-DATE  FROM DATE                                        
020400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020500     .                                                                    
020600     EJECT                                                                
020700 B-WRITE-W272G1 SECTION.                                                  
020800     MOVE 'B-WRITE-W272G1          ' TO CURRENT-SECTION                   
020900                                                                          
021000     MOVE '11'                          TO OUT2-IDDC                      
021100                                           W-IDDC-B6                      
021200     MOVE SEQH-IDDC-REF                 TO W-IDDC-B616                    
021300                                           W-IDDC                         
021400     PERFORM IMS-GU-WDB616                                                
021500     IF SEGMENT-FOUND                                                     
021600       MOVE SEQH-IDARTNR                TO W-IDARTNR                      
021700       PERFORM IMS-GU-WDK601                                              
021701       IF SEGMENT-FOUND                                                   
021710         PERFORM IMS-GNP-WDK611                                           
021800         IF SEGMENT-FOUND                                                 
021802           MOVE '11'                      TO OUT1-IDDC                    
021803           MOVE SEQH-IDARTNR              TO OUT1-IDARTNR                 
021804           PERFORM S11A-WRITE-W272G101                                    
021805                                                                          
021820           MOVE SEQH-IDPERSON-BUY         TO OUT2-IDPERSON-BUY            
021821           MOVE 'B'                       TO OUT2-KDREFTYP                
021822           MOVE SEQH-IDARTNR              TO OUT2-IDARTNR                 
021823           MOVE REF-IDDISTR-REFILL        TO OUT2-IDDISTR                 
021824           MOVE ZERO                      TO OUT2-ADLAGOMR-CDC            
021825                                             OUT2-ADGANG-CDC              
021826                                             OUT2-ADPLATS-CDC             
021827           MOVE CLAG-ADLAGOMR             TO OUT2-ADLAGOMR-SDC            
021828           MOVE CLAG-ADGANG               TO OUT2-ADGANG-SDC              
021829           MOVE CLAG-ADPLATS              TO OUT2-ADPLATS-SDC             
021830           MOVE ZERO                      TO OUT2-KVBEART                 
021831           MOVE 'P'                       TO OUT2-KDREFORS                
021832           MOVE ART-IDLEVNR               TO OUT2-IDLEVNR                 
021833           MOVE '75'                      TO OUT2-KDREFTXT                
023200           MOVE ZERO                      TO OUT2-KDFRAKT                 
023300                                             OUT2-KVBEART-CD              
023400                                             OUT2-ADLAGOMR-CD             
023500                                             OUT2-ADGANG-CD               
023600                                             OUT2-ADPLATS-CD              
023700                                             OUT2-IDKUNDNR                
023800           MOVE CLAG-IDDC-REF             TO OUT2-IDDC-REF                
024400           PERFORM S11B-WRITE-W272G102                                    
024401                                                                          
024500         END-IF                                                           
024600       END-IF                                                             
024610     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 Z-FINIT SECTION.                                                         
025000     CLOSE W272G101                                                       
025010           W272G102                                                       
025100                                                                          
025200     MOVE 'S' TO POSTSUM-OPKOD                                            
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400     .                                                                    
025500     EJECT                                                                
025600 S11A-WRITE-W272G101 SECTION.                                             
025700     MOVE 'S11A-WRITE-W272G101      ' TO CURRENT-SECTION                  
025800                                                                          
025900     WRITE OUT1-RECORD FROM OUT1-AREA                                     
026000                                                                          
026100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
026200     MOVE 'W272G1'   TO POSTSUM-FDNAMN                                    
026300     MOVE 'W272G1D1' TO POSTSUM-DDNAMN2                                   
026400     CALL POSTSUM USING POSTSUM-PARM                                      
026500     .                                                                    
026600     EJECT                                                                
026610 S11B-WRITE-W272G102 SECTION.                                             
026620     MOVE 'S11B-WRITE-W272G102      ' TO CURRENT-SECTION                  
026630                                                                          
026640     WRITE OUT2-RECORD FROM OUT2-AREA                                     
026650                                                                          
026660     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
026670     MOVE 'W272G1'   TO POSTSUM-FDNAMN                                    
026680     MOVE 'W272G1D2' TO POSTSUM-DDNAMN2                                   
026690     CALL POSTSUM USING POSTSUM-PARM                                      
026691     .                                                                    
026692     EJECT                                                                
026700 S99-ABEND SECTION.                                                       
026800                                                                          
026900     SKIP2                                                                
027000     MOVE 'S' TO POSTSUM-OPKOD                                            
027100     CALL POSTSUM USING POSTSUM-PARM                                      
027200     CALL ABEND USING RKOD-ABEND                                          
027300     .                                                                    
027400     EJECT                                                                
027500* --- IMS SECTIONS  ---                                                   
027600                                                                          
027700     EJECT                                                                
027800 IMS-GN-WDK6H1 SECTION.                                                   
027900     MOVE 'IMS-GN-WDK6H1    ' TO DBS-SECTION                              
028000                                                                          
028100     STRING 'WDK6H1  (WDK6H1KY=>' W-WDK6H1KY-MIN-X                        
028200                    '&WDK6H1KY<=' W-WDK6H1KY-MAX-X  ')'                   
028300            DELIMITED BY SIZE INTO SSA1                                   
028400     MOVE '  GE'              TO GOOD-STATUSCODES                         
028500     CALL CBLTDLI USING GN K6H1-PCB DLI-IO-WDK6H1 SSA1                    
028600     MOVE K6H1-STATUS-CODE    TO STATUS-WS                                
028700     PERFORM IMS-STATUSCHECK                                              
028800     .                                                                    
028900     EJECT                                                                
029000 IMS-GU-WDK601 SECTION.                                                   
029100     MOVE 'IMS-GU-WDK601    ' TO DBS-SECTION                              
029200                                                                          
029300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
029400          DELIMITED BY SIZE INTO SSA1                                     
029600     MOVE '  '             TO GOOD-STATUSCODES                            
029700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
029800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
029900     PERFORM IMS-STATUSCHECK                                              
030000     .                                                                    
030100                                                                          
030110 IMS-GNP-WDK611 SECTION.                                                  
030120     MOVE 'IMS-GNP-WDK611    ' TO DBS-SECTION                             
030130                                                                          
030160     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
030170     MOVE '  '             TO GOOD-STATUSCODES                            
030180     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
030190     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
030191     PERFORM IMS-STATUSCHECK                                              
030192     .                                                                    
030193                                                                          
030200 IMS-GU-WDK711 SECTION.                                                   
030300     MOVE 'IMS-GU-WDK701    '   TO DBS-SECTION                            
030400                                                                          
030500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
030600          DELIMITED BY SIZE INTO SSA1                                     
030610     STRING 'WDK711  (IDDC     =' W-IDDC-X    ')'                         
030620          DELIMITED BY SIZE INTO SSA2                                     
030700     MOVE '  GE'           TO GOOD-STATUSCODES                            
030800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
030900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
031000     PERFORM IMS-STATUSCHECK                                              
031100     .                                                                    
031200     EJECT                                                                
032410 IMS-GU-WDB616    SECTION.                                                
032420     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
032430          DELIMITED BY SIZE INTO SSA1                                     
032440     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
032450          DELIMITED BY SIZE INTO SSA2                                     
032460     MOVE '  GE'              TO GOOD-STATUSCODES                         
032470     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
032480     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032481     PERFORM IMS-STATUSCHECK                                              
032491     .                                                                    
032492     EJECT                                                                
032493                                                                          
032500 IMS-STATUSCHECK SECTION.                                                 
032600                                                                          
032700     SET STATUS-IX TO 1                                                   
032800     SEARCH GOOD-STATUS                                                   
032900       AT END                                                             
033000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033100           DELIMITED BY SIZE INTO ERROR-TEXT                              
033200         DISPLAY ERROR-TEXT                                               
033300         CALL FELLOG                                                      
033400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033500         CONTINUE                                                         
033600     END-SEARCH                                                           
033700     .                                                                    
