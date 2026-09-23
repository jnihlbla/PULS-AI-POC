000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6118H00.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   09/12/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        FIND SUPERCEEDED PART NUMBERS FROM WDT2 AND WDK6                 
000900*                                                                         
001000*        THE PROGRAM READS     WDT2                                       
001100*        THE PROGRAM READS     WDK6                                       
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
002500*          --- TIAAVV IN WDT2 < TODAY.                                    
002600     SELECT W6118H                     ASSIGN TO W6118HD1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W6118H                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  RECORD -COPY W6118H01 -PRE  W6118H-  -L.                             
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W6118H00'.            
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004210 77  WS-SUPCED                   PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES TODAYS-DATE.                                        
004600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004800     03  TODAYS-DATE-DAY         PIC 9(2).                                
004900     EJECT                                                                
005000*                                                                         
005100 77  W6118H-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W6118H                       VALUE 'Y'.                   
005300                                                                          
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006200     SKIP2                                                                
006300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800*    --- WORKING STOREGE MISSELENEOUS                                     
006900 01  W-TIAAVV.                                                            
007000     03  W-TIAA                      PIC 9(2) VALUE 0.                    
007100     03  W-TIVV                      PIC 9(2) VALUE 0.                    
007200                                                                          
007300 01  W-TIAAVV-NUM-OLD                PIC 9(4) VALUE 0.                    
007310 01  W-TIAAVV-NUM                    PIC 9(4) VALUE 0.                    
007400     SKIP2                                                                
007500 01  ERROR-TEXT.                                                          
007600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
008000*                                                                         
008100 01  PROGRAM-NAME                PIC X(6)    VALUE 'W6118H'.              
008200     SKIP2                                                                
008300 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
008400     SKIP2                                                                
008500*01  -COPY WDATKORT                                                       
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100*01  -COPY WDATAREA                                                       
009200     EJECT                                                                
009300 01  W6118H-AREA-START           PIC X(24)   VALUE                        
009400                                 'W6118H-AREA-START  '.                   
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY W6118H01     -PRE W6118H-                                 
009800     EJECT                                                                
009900*    --- AREAS FOR IMS-SECTIONS                                           
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  KEYS-FOR-DLI.                                                        
010410     03  W-IDDC-X.                                                        
010420         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010430     03  W-BEFT-X.                                                        
010440         05  W-BEFT              PIC X(2)    VALUE SPACE.                 
010700     03  W-KDSEGKEY-X.                                                    
010800         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC S9(9) COMP-3 VALUE ZERO.             
011100     03  W-WDK611KY-X.                                                    
011200         05  W-WDK611KY          PIC X(1)    VALUE '1'.                   
012410     03  W-ADLAGOMR-X.                                                    
012420         05  W-ADLAGOMR            PIC S9(3)    VALUE ZERO COMP-3.        
012421     03  W-TIAAVV1-FOM-X.                                                 
012430         05  W-TIAAVV1-FOM         PIC S9(4)    VALUE 9999 COMP-3.        
012431     03  W-TIAAVV1-FOM-OLD-X.                                             
012440         05  W-TIAAVV1-FOM-OLD     PIC S9(4)    VALUE 9999 COMP-3.        
012450     03  W-TIAAVV2-FOM-X.                                                 
012460         05  W-TIAAVV2-FOM         PIC S9(4)    VALUE 9999 COMP-3.        
012470     03  W-TIAAVV2-FOM-OLD-X.                                             
012480         05  W-TIAAVV2-FOM-OLD     PIC S9(4)    VALUE 9999 COMP-3.        
012490     03  W-TIAAVV3-FOM-X.                                                 
012500         05  W-TIAAVV3-FOM         PIC S9(4)    VALUE 9999 COMP-3.        
012600     03  W-TIAAVV3-FOM-OLD-X.                                             
012700         05  W-TIAAVV3-FOM-OLD     PIC S9(4)    VALUE 9999 COMP-3.        
013700     SKIP2                                                                
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FOUND                       VALUE '  '.                  
014100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014300     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
014400     SKIP2                                                                
014500 01  GOOD-STATUSCODES.                                                    
014600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                        PIC X(64).                               
014900 01  SSA2                        PIC X(64).                               
015000     EJECT                                                                
015100*    --- IMS FUNCTION CODES                                               
015200*01  -COPY W0003                                                          
015300     EJECT                                                                
015400*    ---  DLI INPUT-OUTPUT AREA                                           
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT201'.                      
015600 01  DLI-IO-WDT201.                                                       
015700*    03  -COPY WDT201                                                     
015800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT211'.                      
015900 01  DLI-IO-WDT211.                                                       
016000*    03  -COPY WDT211                                                     
016010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT212'.                      
016020 01  DLI-IO-WDT212.                                                       
016030*    03  -COPY WDT212                                                     
016040 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT213'.                      
016050 01  DLI-IO-WDT213.                                                       
016060*    03  -COPY WDT213                                                     
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016200 01  DLI-IO-WDK601.                                                       
016300*    03  -COPY WDK601                                                     
016310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
016320 01  DLI-IO-WDK611.                                                       
016330*    03  -COPY WDK611                                                     
016400     EJECT                                                                
016500 LINKAGE SECTION.                                                         
016600                                                                          
016700*01  -COPY W0008  -PRE WDT2-                                              
016800     05  FILLER                  PIC X.                                   
016900                                                                          
017000*01  -COPY W0008  -PRE WDK6-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING WDT2-PCB WDK6-PCB.                             
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING WDT2-PCB WDK6-PCB.                             
017600                                                                          
017700     PERFORM A-INIT                                                       
017710     MOVE '11'        TO W-IDDC                                           
017720     PERFORM IMS-GU-WDT201                                                
017800*** WRITE WDT211 RECORDS                                                  
018160     MOVE W-TIAAVV-NUM  TO W-TIAAVV1-FOM                                  
018170     MOVE ZERO          TO W-ADLAGOMR                                     
018200     PERFORM IMS-GNP-WDT211                                               
018300     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
018400        MOVE GLO-ADLAGOMR     TO W-ADLAGOMR                               
018630        MOVE W-TIAAVV-NUM-OLD TO W-TIAAVV1-FOM-OLD                        
018700        PERFORM IMS-GNP-WDT211-OLD                                        
018800        IF SEGMENT-FOUND                                                  
018810           MOVE LOW-VALUES TO W6118H-AREA                                 
018900           MOVE GLO-ADLAGOMR   TO W6118H-ADLAGOMR                         
019000           MOVE GLO-TIAAVV-FOM TO W6118H-TIAAVV-FOM                       
019100           PERFORM S11-WRITE-W6118H                                       
019200        END-IF                                                            
019400        PERFORM IMS-GNP-WDT211-NEXT                                       
019500     END-PERFORM                                                          
019600                                                                          
019601*** WRITE WDT212 RECORDS                                                  
019610     MOVE W-TIAAVV-NUM  TO W-TIAAVV2-FOM                                  
019620     MOVE ZERO          TO W-BEFT                                         
019630     PERFORM IMS-GNP-WDT212                                               
019640     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
019650        MOVE GFT-BEFT         TO W-BEFT                                   
019660        MOVE W-TIAAVV-NUM-OLD TO W-TIAAVV2-FOM-OLD                        
019670        PERFORM IMS-GNP-WDT212-OLD                                        
019680        IF SEGMENT-FOUND                                                  
019681           MOVE LOW-VALUES TO W6118H-AREA                                 
019690           MOVE GFT-BEFT       TO W6118H-BEFT                             
019691           MOVE GFT-TIAAVV-FOM TO W6118H-TIAAVV-FOM                       
019692           PERFORM S11-WRITE-W6118H                                       
019693        END-IF                                                            
019694        PERFORM IMS-GNP-WDT212-NEXT                                       
019695     END-PERFORM                                                          
019696                                                                          
019697*** WRITE WDT213 RECORDS                                                  
019698     MOVE W-TIAAVV-NUM  TO W-TIAAVV3-FOM                                  
019699     MOVE ZERO          TO W-IDARTNR                                      
019700     PERFORM IMS-GNP-WDT213                                               
019701     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
019702        MOVE GART-IDARTNR     TO W-IDARTNR                                
019703        MOVE W-TIAAVV-NUM-OLD TO W-TIAAVV3-FOM-OLD                        
019704        PERFORM IMS-GNP-WDT213-OLD                                        
019705        IF SEGMENT-FOUND                                                  
019706           MOVE LOW-VALUES TO W6118H-AREA                                 
019707           MOVE GART-IDARTNR    TO W6118H-IDARTNR                         
019708           MOVE GART-TIAAVV-FOM TO W6118H-TIAAVV-FOM                      
019709           PERFORM S11-WRITE-W6118H                                       
019710        END-IF                                                            
019711        PERFORM IMS-GNP-WDT213-NEXT                                       
019712     END-PERFORM                                                          
019713                                                                          
019715*** WRITE WDT213 RECORDS - SUPERCEEDED PART NUMBERS                       
019716     MOVE ZERO          TO W-TIAAVV3-FOM                                  
019717     MOVE ZERO          TO W-IDARTNR                                      
019718     PERFORM IMS-GNP-WDT213-IDARTNR                                       
019719     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
019720           MOVE GART-IDARTNR  TO W-IDARTNR                                
019721           MOVE 'N' TO WS-SUPCED                                          
019722           PERFORM IMS-GU-WDK601                                          
019723           IF SEGMENT-FOUND                                               
019724              IF ART-KDSORT  = 'SW' OR                                    
019725                 ART-KDERS-UTG  > 0                                       
019726                 MOVE 'Y' TO WS-SUPCED                                    
019727              ELSE                                                        
019728                 PERFORM IMS-GU-WDK611                                    
019729                 IF SEGMENT-MISSING                                       
019730                    MOVE 'Y' TO WS-SUPCED                                 
019733                 ELSE                                                     
019734                    IF SEGMENT-FOUND                                      
019735                       IF CLAG-KDERS > 20                                 
019773                          MOVE 'Y' TO WS-SUPCED                           
019777                       END-IF                                             
019778                    END-IF                                                
019779                 END-IF                                                   
019780              END-IF                                                      
019781           END-IF                                                         
019782           IF WS-SUPCED = 'Y'                                             
019783              MOVE ZERO          TO W-TIAAVV3-FOM                         
019784              MOVE GART-IDARTNR  TO W-IDARTNR                             
019785              PERFORM B-IDARTNR-SAME                                      
019786           END-IF                                                         
019787                                                                          
019788           MOVE ZERO          TO W-TIAAVV3-FOM                            
019789           MOVE GART-IDARTNR  TO W-IDARTNR                                
019790           PERFORM IMS-GNP-WDT213-IDARTNR-NEXT                            
019791     END-PERFORM                                                          
019792                                                                          
019794     PERFORM Z-FINIT                                                      
019800                                                                          
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT SECTION.                                                          
020400     OPEN OUTPUT W6118H                                                   
020500                                                                          
020600     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
020700     MOVE D-AAR       TO TODAYS-DATE-YEAR                                 
020800     MOVE D-MAANAD    TO TODAYS-DATE-MONTH                                
020900     MOVE D-DAG       TO TODAYS-DATE-DAY                                  
021000                                                                          
021100     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
021200     MOVE  TODAYS-DATE TO DAT-I-TIDATUM                                   
021300                                                                          
021400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
021500                     DAT-O-TIDATUM DAT-KDSVAR                             
021600                                                                          
021700     IF DAT-KDSVAR-OK                                                     
021800       MOVE DAT-TIAA-VECKA     TO W-TIAA                                  
021900       MOVE DAT-TIVV           TO W-TIVV                                  
021910       MOVE W-TIAAVV           TO W-TIAAVV-NUM-OLD                        
022000       ADD 1                   TO W-TIVV                                  
022100       MOVE W-TIAAVV           TO W-TIAAVV-NUM                            
022200     ELSE                                                                 
022300       MOVE 'ERROR IN WDATKONV ' TO ERROR-TEXT                            
022400       DISPLAY ERROR-TEXT                                                 
022500       CALL FELLOG                                                        
022600     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023710 B-IDARTNR-SAME SECTION.                                                  
023740     PERFORM IMS-GNP-WDT213-SUPCED-FIRST                                  
023750     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
023751       MOVE LOW-VALUES TO W6118H-AREA                                     
023752       MOVE GART-IDARTNR    TO W6118H-IDARTNR                             
023753       MOVE GART-TIAAVV-FOM TO W6118H-TIAAVV-FOM                          
023754                               W-TIAAVV3-FOM                              
023755       PERFORM S11-WRITE-W6118H                                           
023780       PERFORM IMS-GNP-WDT213-SUPCED-NEXT                                 
023806     END-PERFORM                                                          
023808     .                                                                    
023809     EJECT                                                                
023810 Z-FINIT SECTION.                                                         
023900     CLOSE W6118H                                                         
024000     SKIP2                                                                
024100     MOVE 'S' TO POSTSUM-OPKOD                                            
024200     CALL POSTSUM USING POSTSUM-PARM                                      
024300     .                                                                    
024400     EJECT                                                                
024500 S11-WRITE-W6118H SECTION.                                                
024600     WRITE W6118H-RECORD FROM W6118H-AREA                                 
024700     MOVE 'W6118H'   TO POSTSUM-FDNAMN                                    
024800     MOVE 'W6118HD1' TO POSTSUM-DDNAMN2                                   
024900     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300 S99-ABEND SECTION.                                                       
025400     SKIP2                                                                
025500     MOVE 'S' TO POSTSUM-OPKOD                                            
025600     CALL POSTSUM USING POSTSUM-PARM                                      
025700     CALL ABEND USING RKOD-ABEND                                          
025800     .                                                                    
025900     EJECT                                                                
026000* --- IMS SECTIONS  ---                                                   
026100                                                                          
026200     EJECT                                                                
026210 IMS-GU-WDT201 SECTION.                                                   
026220     STRING 'WDT201  (IDDC     =' W-IDDC-X ')'                            
026230          DELIMITED BY SIZE INTO SSA1                                     
026240     MOVE '  ' TO GOOD-STATUSCODES                                        
026250     CALL CBLTDLI USING GU WDT2-PCB DLI-IO-WDT201 SSA1                    
026260     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
026270     PERFORM IMS-STATUSCHECK                                              
026280     .                                                                    
026290     EJECT                                                                
026300 IMS-GNP-WDT201 SECTION.                                                  
026400*    STRING 'WDT201  (BEFT     =' W-BEFT-X ')'                            
026500*         DELIMITED BY SIZE INTO SSA1                                     
026600     MOVE 'WDT201' TO SSA1                                                
026700     MOVE '  GE' TO GOOD-STATUSCODES                                      
026800     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT201 SSA1                   
026900     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSCHECK                                              
027100     .                                                                    
027200     EJECT                                                                
027300 IMS-GNP-WDT211 SECTION.                                                  
027400     STRING 'WDT211  *F(ADLAGOMR>=' W-ADLAGOMR-X                          
027500                      '&TIAAVVF1 =' W-TIAAVV1-FOM-X ')'                   
027600          DELIMITED BY SIZE INTO SSA1                                     
027700     MOVE '  GE' TO GOOD-STATUSCODES                                      
027800     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT211 SSA1                   
027900     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028000     PERFORM IMS-STATUSCHECK                                              
028100     .                                                                    
028200     EJECT                                                                
028211 IMS-GNP-WDT211-OLD SECTION.                                              
028212     STRING 'WDT211  *F(ADLAGOMR =' W-ADLAGOMR-X                          
028213                      '&TIAAVVF1<=' W-TIAAVV1-FOM-OLD-X ')'               
028214          DELIMITED BY SIZE INTO SSA1                                     
028250     MOVE '  GE' TO GOOD-STATUSCODES                                      
028260     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT211 SSA1                   
028270     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028280     PERFORM IMS-STATUSCHECK                                              
028290     .                                                                    
028291     EJECT                                                                
028292 IMS-GNP-WDT211-NEXT SECTION.                                             
028293     STRING 'WDT211  *F(ADLAGOMR> ' W-ADLAGOMR-X                          
028294                      '&TIAAVVF1 =' W-TIAAVV1-FOM-X ')'                   
028295          DELIMITED BY SIZE INTO SSA1                                     
028296     MOVE '  GE' TO GOOD-STATUSCODES                                      
028297     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT211 SSA1                   
028298     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028299     PERFORM IMS-STATUSCHECK                                              
028300     .                                                                    
028301     EJECT                                                                
028302 IMS-GNP-WDT212 SECTION.                                                  
028303     STRING 'WDT212  *F(BEFT    >=' W-BEFT-X                              
028304                      '&TIAAVVF2 =' W-TIAAVV2-FOM-X ')'                   
028305          DELIMITED BY SIZE INTO SSA1                                     
028306     MOVE '  GE' TO GOOD-STATUSCODES                                      
028307     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT212 SSA1                   
028308     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028309     PERFORM IMS-STATUSCHECK                                              
028310     .                                                                    
028311     EJECT                                                                
028312 IMS-GNP-WDT212-OLD SECTION.                                              
028313     STRING 'WDT212  *F(BEFT     =' W-BEFT-X                              
028314                      '&TIAAVVF2<=' W-TIAAVV2-FOM-OLD-X ')'               
028315          DELIMITED BY SIZE INTO SSA1                                     
028316     MOVE '  GE' TO GOOD-STATUSCODES                                      
028317     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT212 SSA1                   
028318     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028319     PERFORM IMS-STATUSCHECK                                              
028320     .                                                                    
028321     EJECT                                                                
028322 IMS-GNP-WDT212-NEXT SECTION.                                             
028323     STRING 'WDT212  *F(BEFT    > ' W-BEFT-X                              
028324                      '&TIAAVVF2 =' W-TIAAVV2-FOM-X ')'                   
028325          DELIMITED BY SIZE INTO SSA1                                     
028326     MOVE '  GE' TO GOOD-STATUSCODES                                      
028327     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT212 SSA1                   
028328     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028329     PERFORM IMS-STATUSCHECK                                              
028330     .                                                                    
028331     EJECT                                                                
028332 IMS-GNP-WDT213 SECTION.                                                  
028333     STRING 'WDT213  *F(IDARTNR >=' W-IDARTNR-X                           
028334                      '&TIAAVVF3 =' W-TIAAVV3-FOM-X ')'                   
028335          DELIMITED BY SIZE INTO SSA1                                     
028336     MOVE '  GE' TO GOOD-STATUSCODES                                      
028337     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
028338     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028339     PERFORM IMS-STATUSCHECK                                              
028340     .                                                                    
028341     EJECT                                                                
028342 IMS-GNP-WDT213-IDARTNR SECTION.                                          
028343     STRING 'WDT213  *F(IDARTNR >=' W-IDARTNR-X                           
028344                      '&TIAAVVF3>=' W-TIAAVV3-FOM-X ')'                   
028345          DELIMITED BY SIZE INTO SSA1                                     
028346     MOVE '  GE' TO GOOD-STATUSCODES                                      
028347     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
028348     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028349     PERFORM IMS-STATUSCHECK                                              
028350     .                                                                    
028351     EJECT                                                                
028352 IMS-GNP-WDT213-IDARTNR-NEXT SECTION.                                     
028353     STRING 'WDT213  *F(IDARTNR > ' W-IDARTNR-X                           
028354                      '&TIAAVVF3>=' W-TIAAVV3-FOM-X ')'                   
028355          DELIMITED BY SIZE INTO SSA1                                     
028356     MOVE '  GE' TO GOOD-STATUSCODES                                      
028357     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
028358     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028359     PERFORM IMS-STATUSCHECK                                              
028360     .                                                                    
028361     EJECT                                                                
028362 IMS-GNP-WDT213-OLD SECTION.                                              
028363     STRING 'WDT213  *F(IDARTNR  =' W-IDARTNR-X                           
028364                      '&TIAAVVF3<=' W-TIAAVV3-FOM-OLD-X ')'               
028365          DELIMITED BY SIZE INTO SSA1                                     
028366     MOVE '  GE' TO GOOD-STATUSCODES                                      
028367     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
028368     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028369     PERFORM IMS-STATUSCHECK                                              
028370     .                                                                    
028371     EJECT                                                                
028372 IMS-GNP-WDT213-NEXT SECTION.                                             
028373     STRING 'WDT213  *F(IDARTNR > ' W-IDARTNR-X                           
028374                      '&TIAAVVF3 =' W-TIAAVV3-FOM-X ')'                   
028375          DELIMITED BY SIZE INTO SSA1                                     
028376     MOVE '  GE' TO GOOD-STATUSCODES                                      
028377     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
028378     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028379     PERFORM IMS-STATUSCHECK                                              
028380     .                                                                    
028381     EJECT                                                                
028382 IMS-GNP-WDT213-SUPCED-FIRST SECTION.                                     
028383     STRING 'WDT213  *F(IDARTNR  =' W-IDARTNR-X                           
028384                      '&TIAAVVF3>=' W-TIAAVV3-FOM-X ')'                   
028385          DELIMITED BY SIZE INTO SSA1                                     
028386     MOVE '  GE' TO GOOD-STATUSCODES                                      
028387     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
028388     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028389     PERFORM IMS-STATUSCHECK                                              
028390     .                                                                    
028391     EJECT                                                                
028392 IMS-GNP-WDT213-SUPCED-NEXT SECTION.                                      
028393     STRING 'WDT213  *F(IDARTNR  =' W-IDARTNR-X                           
028394                      '&TIAAVVF3> ' W-TIAAVV3-FOM-X ')'                   
028395          DELIMITED BY SIZE INTO SSA1                                     
028396     MOVE '  GE' TO GOOD-STATUSCODES                                      
028397     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT213 SSA1                   
028398     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
028399     PERFORM IMS-STATUSCHECK                                              
028400     .                                                                    
028401     EJECT                                                                
030300 IMS-GU-WDK601 SECTION.                                                   
030400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
030500          DELIMITED BY SIZE INTO SSA1                                     
030800     MOVE '  GE' TO GOOD-STATUSCODES                                      
030900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
031000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031100     PERFORM IMS-STATUSCHECK                                              
031200     .                                                                    
031300     EJECT                                                                
031310 IMS-GU-WDK611 SECTION.                                                   
031320     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
031330          DELIMITED BY SIZE INTO SSA1                                     
031340     STRING 'WDK611  (KDSEGKEY =' W-WDK611KY-X ')'                        
031350          DELIMITED BY SIZE INTO SSA2                                     
031360     MOVE '  GE' TO GOOD-STATUSCODES                                      
031370     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
031380     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031390     PERFORM IMS-STATUSCHECK                                              
031391     .                                                                    
031392     EJECT                                                                
031400 IMS-STATUSCHECK SECTION.                                                 
031500     SET STATUS-IX TO 1                                                   
031600     SEARCH GOOD-STATUS                                                   
031700       AT END                                                             
031800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031900           DELIMITED BY SIZE INTO ERROR-TEXT                              
032000         DISPLAY ERROR-TEXT                                               
032100         CALL FELLOG                                                      
032200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
032300         CONTINUE                                                         
032400     END-SEARCH                                                           
032500     .                                                                    
