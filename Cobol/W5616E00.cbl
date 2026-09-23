000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5616E00.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   2017-11-24                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        RÄKNAR UT VALUTADIFF                                             
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600     SKIP2                                                                
001700*          --- INFIL1-HÄNDELSEPOSTER                                      
001800     SELECT W5616V                     ASSIGN TO W5616ED1.                
001900     SKIP2                                                                
002000*          --- UTFIL1-KORREKTAPOSTER                                      
002100     SELECT W56164                     ASSIGN TO W5616ED2.                
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W5616V                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY WDR801      -L.                                                
003200     SKIP3                                                                
003300 FD  W56164                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST -COPY WDR801 -PRE  RATT- -L.                                    
003800     SKIP3                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W5616E00'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  W5616V-EOF-SW               PIC X       VALUE 'N'.                   
004700     88  END-OF-W5616V                       VALUE 'J'.                   
004800                                                                          
004900 77  WS-TOT-AMOUNT-DDI           PIC S9(9)V99  COMP-3 VALUE ZERO.         
005000 77  WS-LINE-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
005100 77  WS-DIFF-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
005200 77  WS-PRARTNTO                 PIC S9(7)V99  COMP-3 VALUE ZERO.         
005300 77  WS-PRARTNTO1                PIC S9(7)V99  COMP-3 VALUE ZERO.         
005400 77  WS-SUBEL                    PIC S9(9)V99  COMP-3 VALUE ZERO.         
005500 77  WS-IDVERGL                  PIC X(10)   VALUE '          '.          
005600 77  WS-EKH-IDVERGL              PIC X(10)   VALUE SPACE.                 
005700 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005800 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005900                                                                          
006000 01  WS-KDVALISO                  PIC X(3).                               
006100 01  WS-PRKURS-US                 PIC S9(6)V9(5) COMP-3.                  
006200 01  WS-PRKURS-US3                PIC S9(6)V9(5) COMP-3.                  
006300 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
006400 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
006500 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
006600 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
006700     EJECT                                                                
006800                                                                          
006900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007000 01  FILLER REDEFINES DAGENS-DATUM.                                       
007100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007400 01  W-AAAAMMDD                  PIC 9(8).                                
007500 01  WS-DAGENS-DATUM             PIC 9(8).                                
007600                                                                          
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
008600     SKIP2                                                                
008700 01  FELTEXT.                                                             
008800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL DATKORT                                          
009200*                                                                         
009300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W5616E'.              
009400     SKIP2                                                                
009500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009600     SKIP2                                                                
009700*01  -COPY WDATKORT                                                       
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200     EJECT                                                                
010300*01  -COPY WDATAREA                                                       
010400     EJECT                                                                
010500*01  -COPY W510CURR                                                       
010600     EJECT                                                                
010700                                                                          
010800 01 IN1-AREA-START              PIC X(24)   VALUE                         
010900                                 'IN1-AREA-START  '.                      
011000*01  AREA -COPY WDR801   -PRE IN1-                                        
011100*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR801-DATA               
011200     EJECT                                                                
011300                                                                          
011400 01  RATT-AREA-START             PIC X(24)   VALUE                        
011500                                 'RATT-AREA-START  '.                     
011600*01  AREA -COPY WDR801   -PRE RATT-                                       
011700*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR801-DATA             
011800     EJECT                                                                
011900                                                                          
012000                                                                          
012100 LINKAGE SECTION.                                                         
012200*01  -COPY W0008  -PRE WDG2-                                              
012300     05  FILLER                  PIC X.                                   
012400                                                                          
012500                                                                          
012600 PROCEDURE DIVISION  USING WDG2-PCB.                                      
012700                                                                          
012800 MAIN SECTION.                                                            
012900     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
013000                                                                          
013100     PERFORM A-INIT                                                       
013200     PERFORM S01-LAES-W5616V                                              
013300     PERFORM UNTIL END-OF-W5616V                                          
013400       PERFORM S02-GET-CURRENCY                                           
013500       PERFORM BA-KONTROLLERA-POST                                        
013600       PERFORM S01-LAES-W5616V                                            
013700     END-PERFORM                                                          
013800                                                                          
013900     PERFORM Z-FINIT                                                      
014000                                                                          
014100     MOVE ZERO TO  RETURN-CODE                                            
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500                                                                          
014600 A-INIT SECTION.                                                          
014700     OPEN INPUT  W5616V                                                   
014800     OPEN OUTPUT W56164                                                   
014900     SKIP2                                                                
015000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015100     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
015200                        W-DATE-AAMM(1:2)                                  
015300                        WS-TIAA                                           
015400     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
015500                        W-DATE-AAMM(3:2)                                  
015600                        WS-TIMM                                           
015700     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
015800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
015900                                                                          
016000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016100     .                                                                    
016200     EJECT                                                                
016300                                                                          
016400 BA-KONTROLLERA-POST SECTION.                                             
016500     IF  (IN1-EKH-KDEKHHT = '102'                                         
016600     AND IN1-EKH-KDEKSHT = '130')                                         
016700     OR  (IN1-EKH-KDEKHHT = '102'                                         
016800     AND IN1-EKH-KDEKSHT = '134')                                         
016900     OR  (IN1-EKH-KDEKHHT = '303'                                         
017000     AND IN1-EKH-KDEKSHT = '371')                                         
017100**** NOLLSTÄLLNING AV BERÄKNING AV DDI POST MELLAN EVENT                  
017200       IF IN1-EKH-IDVERGL = WS-EKH-IDVERGL                                
017300         CONTINUE                                                         
017400       ELSE                                                               
017500         MOVE ZERO            TO WS-TOT-AMOUNT-DDI                        
017600                                 WS-LINE-AMOUNT-DDI                       
017700                                 WS-DIFF-AMOUNT-DDI                       
017800                                 WS-SUBEL                                 
017900                                 WS-PRARTNTO                              
018000                                 WS-PRARTNTO1                             
018100         MOVE IN1-EKH-IDVERGL TO WS-EKH-IDVERGL                           
018200       END-IF                                                             
018300* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
018400       IF IN1-EKH-KDEKNIVA = 'DET'                                        
018500         COMPUTE WS-PRARTNTO ROUNDED = IN1-EKH-PRARTNTO *                 
018600                  IN1-EKH-KVANTAL  / WS-PRKURS-US3                        
018700         COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +               
018800                                        WS-PRARTNTO                       
018900         PERFORM BD-SKICKA-RATT-POST                                      
019000                                                                          
019100       ELSE                                                               
019200         IF IN1-EKH-KDEKNIVA = 'SUM'                                      
019300           COMPUTE WS-SUBEL ROUNDED = IN1-EKH-SUBEL  /                    
019400                                           WS-PRKURS-US3                  
019500           PERFORM BD-SKICKA-RATT-POST                                    
019600                                                                          
019700           COMPUTE WS-TOT-AMOUNT-DDI =  WS-SUBEL                          
019800           COMPUTE WS-DIFF-AMOUNT-DDI =  WS-TOT-AMOUNT-DDI -              
019900                                         WS-LINE-AMOUNT-DDI               
020000           IF WS-DIFF-AMOUNT-DDI NOT = ZERO                               
020100             DISPLAY '    '                                               
020200             DISPLAY 'VER=' IN1-EKH-IDVERGL                               
020300             DISPLAY 'TOT=' WS-TOT-AMOUNT-DDI                             
020400             DISPLAY 'LIN=' WS-LINE-AMOUNT-DDI                            
020500             DISPLAY 'DIF=' WS-DIFF-AMOUNT-DDI                            
020600                                                                          
020700             MOVE 'DDI'                TO IN1-EKH-KDEKNIVA                
020800             MOVE WS-DIFF-AMOUNT-DDI   TO IN1-EKH-SUBEL                   
020900             MOVE ZERO                 TO IN1-EKH-SUVAT                   
021000                                                                          
021100               PERFORM BD-SKICKA-RATT-POST                                
021200           END-IF                                                         
021300           MOVE ZERO                 TO WS-TOT-AMOUNT-DDI                 
021400                                        WS-LINE-AMOUNT-DDI                
021500                                        WS-DIFF-AMOUNT-DDI                
021600                                        WS-SUBEL                          
021700                                        WS-PRARTNTO                       
021800                                        WS-PRARTNTO1                      
021900         ELSE                                                             
022000           COMPUTE WS-SUBEL ROUNDED = IN1-EKH-SUBEL  /                    
022100                                           WS-PRKURS-US3                  
022200           END-COMPUTE                                                    
022300           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
022400                                         WS-SUBEL                         
022500           END-COMPUTE                                                    
022600           PERFORM BD-SKICKA-RATT-POST                                    
022700         END-IF                                                           
022800       END-IF                                                             
022900     ELSE                                                                 
023000       IF  (IN1-EKH-KDEKHHT = '103'                                       
023100       AND IN1-EKH-KDEKSHT = '102')                                       
023200       OR  (IN1-EKH-KDEKHHT = '102'                                       
023300       AND IN1-EKH-KDEKSHT = '107')                                       
023400         IF IN1-EKH-IDVERGL = WS-EKH-IDVERGL                              
023500           CONTINUE                                                       
023600         ELSE                                                             
023700           MOVE ZERO            TO WS-TOT-AMOUNT-DDI                      
023800                                   WS-LINE-AMOUNT-DDI                     
023900                                   WS-DIFF-AMOUNT-DDI                     
024000                                   WS-SUBEL                               
024100                                   WS-PRARTNTO                            
024200                                   WS-PRARTNTO1                           
024300           MOVE IN1-EKH-IDVERGL TO WS-EKH-IDVERGL                         
024400         END-IF                                                           
024500         IF IN1-EKH-KDEKNIVA = 'DET'                                      
024600           COMPUTE WS-PRARTNTO ROUNDED =                                  
024700                   IN1-EKH-KVANTAL * IN1-EKH-PRARTSTD *                   
024800                   IN1-EKH-PRKURS                                         
024900           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
025000                                      WS-PRARTNTO                         
025100           PERFORM BD-SKICKA-RATT-POST                                    
025200         ELSE                                                             
025300           IF IN1-EKH-KDEKNIVA = 'ARB'                                    
025400             COMPUTE WS-PRARTNTO1 ROUNDED =                               
025500                     IN1-EKH-SUBEL * IN1-EKH-PRKURS                       
025600             COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +           
025700                                        WS-PRARTNTO1                      
025800             PERFORM BD-SKICKA-RATT-POST                                  
025900           ELSE                                                           
026000* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
026100             IF IN1-EKH-KDEKNIVA = 'SUM'                                  
026200               COMPUTE WS-SUBEL ROUNDED = IN1-EKH-SUBEL *                 
026300                                          IN1-EKH-PRKURS                  
026400               PERFORM BD-SKICKA-RATT-POST                                
026500                                                                          
026600               COMPUTE WS-TOT-AMOUNT-DDI =  WS-SUBEL                      
026700               IF WS-LINE-AMOUNT-DDI < 0                                  
026800                 COMPUTE WS-LINE-AMOUNT-DDI = WS-LINE-AMOUNT-DDI          
026900                                               * -1                       
027000                 COMPUTE WS-DIFF-AMOUNT-DDI = WS-TOT-AMOUNT-DDI -         
027100                                             WS-LINE-AMOUNT-DDI           
027200               ELSE                                                       
027300                 COMPUTE WS-LINE-AMOUNT-DDI = WS-LINE-AMOUNT-DDI          
027400                                               * 1                        
027500                 COMPUTE WS-DIFF-AMOUNT-DDI = WS-TOT-AMOUNT-DDI +         
027600                                             WS-LINE-AMOUNT-DDI           
027700               END-IF                                                     
027800               IF WS-DIFF-AMOUNT-DDI NOT = ZERO                           
027900                 MOVE 'DDI'                TO IN1-EKH-KDEKNIVA            
028000                 MOVE WS-DIFF-AMOUNT-DDI   TO IN1-EKH-SUBEL               
028100                 MOVE ZERO                 TO IN1-EKH-SUVAT               
028200                                                                          
028300                 PERFORM BD-SKICKA-RATT-POST                              
028400               END-IF                                                     
028500               MOVE ZERO                 TO WS-TOT-AMOUNT-DDI             
028600                                            WS-LINE-AMOUNT-DDI            
028700                                            WS-DIFF-AMOUNT-DDI            
028800                                            WS-SUBEL                      
028900                                            WS-PRARTNTO                   
029000                                            WS-PRARTNTO1                  
029100             ELSE                                                         
029200               PERFORM BD-SKICKA-RATT-POST                                
029300             END-IF                                                       
029400           END-IF                                                         
029500         END-IF                                                           
029600       ELSE                                                               
029700           PERFORM BD-SKICKA-RATT-POST                                    
029800       END-IF                                                             
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030300 BD-SKICKA-RATT-POST SECTION.                                             
030400     MOVE IN1-AREA TO RATT-AREA                                           
030500     PERFORM S11-SKRIV-RATT-POST                                          
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900 Z-FINIT SECTION.                                                         
031000     CLOSE W5616V                                                         
031100           W56164                                                         
031200     SKIP2                                                                
031300     MOVE 'S' TO POSTSUM-OPKOD                                            
031400     CALL POSTSUM USING POSTSUM-PARM                                      
031500     .                                                                    
031600     EJECT                                                                
031700                                                                          
031800 S01-LAES-W5616V  SECTION.                                                
031900     READ W5616V INTO IN1-AREA                                            
032000     AT END                                                               
032100        MOVE HIGH-VALUE TO IN1-AREA                                       
032200        SET END-OF-W5616V TO TRUE                                         
032300                                                                          
032400     NOT AT END                                                           
032500        MOVE 'W5616V' TO POSTSUM-FDNAMN                                   
032600        MOVE 'W5616ED1' TO POSTSUM-DDNAMN2                                
032700        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
032800        CALL POSTSUM USING POSTSUM-PARM                                   
032900     END-READ                                                             
033000     .                                                                    
033100     EJECT                                                                
033200                                                                          
033300 S02-GET-CURRENCY SECTION.                                                
033400     IF IN1-FIL-IDPGM = 'W4183300'                                        
033500       IF IN1-EKH-DAAVIDAT > ZERO                                         
033600         MOVE IN1-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                         
033700         MOVE IN1-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                         
033800       ELSE                                                               
033900         MOVE WS-TIAA               TO WS-TIAA-CR                         
034000         MOVE WS-TIMM               TO WS-TIMM-CR                         
034100       END-IF                                                             
034200     ELSE                                                                 
034300       MOVE WS-TIAA                 TO WS-TIAA-CR                         
034400       MOVE WS-TIMM                 TO WS-TIMM-CR                         
034500     END-IF                                                               
034600     MOVE WS-TIAA-CR                TO W-DATE-AAMM(1:2)                   
034700     MOVE WS-TIMM-CR                TO W-DATE-AAMM(3:2)                   
034800     MOVE 'USD'                     TO CURR-KDVALISO-ROW                  
034900     MOVE W-DATE-AAMM               TO CURR-TIAAMM                        
035000     MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV                  
035100     MOVE 'M'                       TO CURR-KDVALTYP                      
035200     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
035300     IF CURR-KDSVAR = ' '                                                 
035400       MOVE CURR-PRKURS-NEW         TO WS-PRKURS-US                       
035500     ELSE                                                                 
035600       MOVE 1                       TO WS-PRKURS-US                       
035700     END-IF                                                               
035800     MOVE WS-PRKURS-US              TO WS-PRKURS-US3                      
035900     .                                                                    
036000     EJECT                                                                
036100                                                                          
036200 S11-SKRIV-RATT-POST SECTION.                                             
036300     WRITE RATT-POST FROM RATT-AREA                                       
036400                                                                          
036500     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
036600     MOVE 'W56164' TO POSTSUM-FDNAMN                                      
036700     MOVE 'W5616ED2' TO POSTSUM-DDNAMN2                                   
036800     CALL POSTSUM USING POSTSUM-PARM                                      
036900     .                                                                    
037000     EJECT                                                                
