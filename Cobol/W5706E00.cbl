000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5706E00.                                                
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
001800     SELECT W5706V                     ASSIGN TO W5706ED1.                
001900     SKIP2                                                                
002000*          --- UTFIL1-KORREKTAPOSTER                                      
002100     SELECT W57064                     ASSIGN TO W5706ED2.                
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W5706V                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY WDR801      -L.                                                
003200     SKIP3                                                                
003300 FD  W57064                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST -COPY WDR801 -PRE  RATT- -L.                                    
003800     SKIP3                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W5706E00'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-SAVE-KDVALISO            PIC X(3)    VALUE SPACES.                
004600                                                                          
004700 77  W5706V-EOF-SW               PIC X       VALUE 'N'.                   
004800     88  END-OF-W5706V                       VALUE 'J'.                   
004900                                                                          
004910 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACES.                
004920     88  WS-IDLANDX2-CN                      VALUE 'CN'.                  
004930                                                                          
005000 77  WS-TOT-AMOUNT-DDI           PIC S9(11)V99  COMP-3 VALUE ZERO.        
005100 77  WS-LINE-AMOUNT-DDI          PIC S9(11)V99  COMP-3 VALUE ZERO.        
005200 77  WS-DIFF-AMOUNT-DDI          PIC S9(11)V99  COMP-3 VALUE ZERO.        
005300 77  WS-SUBEL                    PIC S9(11)V99  COMP-3 VALUE ZERO.        
005400 77  WS-PRARTNTO                 PIC S9(11)V99  COMP-3 VALUE ZERO.        
005410 77  WS-PRARTNTO1                PIC S9(11)V99  COMP-3 VALUE ZERO.        
005500 77  WS-IDVERGL                  PIC X(10)   VALUE '          '.          
005600 77  WS-EKH-IDVERGL              PIC X(10)   VALUE SPACE.                 
005700 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005800 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005900                                                                          
006000 01  W-KDTRADP                    PIC X(4) VALUE SPACES.                  
006100 01  WS-SAVE-KDTRADP              PIC X(4) VALUE SPACES.                  
006200 01  WS-PRKURS-SC                 PIC S9(6)V9(5) COMP-3.                  
006300 01  WS-PRKURS-SC3                PIC S9(6)V9(5) COMP-3.                  
006400 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
006500 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
006600 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
006700 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
006800     EJECT                                                                
006900                                                                          
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500 01  W-AAAAMMDD                  PIC 9(8).                                
007600 01  WS-DAGENS-DATUM             PIC 9(8).                                
007700                                                                          
007800     EJECT                                                                
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
008700     SKIP2                                                                
008800 01  FELTEXT.                                                             
008900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL DATKORT                                          
009300*                                                                         
009400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W5706E'.              
009500     SKIP2                                                                
009600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009700     SKIP2                                                                
009800*01  -COPY WDATKORT                                                       
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100*                                                                         
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400*01  -COPY WDATAREA                                                       
010500     EJECT                                                                
010600*01  -COPY W510CURR                                                       
010700     EJECT                                                                
010800                                                                          
010900 01 IN1-AREA-START              PIC X(24)   VALUE                         
011000                                 'IN1-AREA-START  '.                      
011100*01  AREA -COPY WDR801   -PRE IN1-                                        
011200*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR801-DATA               
011300     EJECT                                                                
011400                                                                          
011500 01  RATT-AREA-START             PIC X(24)   VALUE                        
011600                                 'RATT-AREA-START  '.                     
011700*01  AREA -COPY WDR801   -PRE RATT-                                       
011800*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR801-DATA             
011900     EJECT                                                                
012000                                                                          
012100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012200 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400*    --- STATUS-KOD FRÅN IMS                                              
012500 01  STATUS-WS                    PIC XX.                                 
012600     88  SEGMENT-FINNS                        VALUE '  '.                 
012700     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
012800                                                                          
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100                                                                          
013200 01  SSA1                         PIC X(128).                             
013300 01  SSA2                         PIC X(64).                              
013400     EJECT                                                                
013500                                                                          
013600* ---IMS FUNKTIONSKODER----                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
013900                                                                          
014000*    ---  DLI INPUT-OUTPUT AREA                                           
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014200 01  DLI-IO-WDB601.                                                       
014300*    03  -COPY WDB601                                                     
014400     EJECT                                                                
014500                                                                          
014600 LINKAGE SECTION.                                                         
014700*01  -COPY W0008  -PRE WDG2-                                              
014800     05  FILLER                  PIC X.                                   
014900*01  -COPY W0008  -PRE WDB6-                                              
015000     05  FILLER                  PIC X.                                   
015100                                                                          
015200                                                                          
015300 PROCEDURE DIVISION  USING WDG2-PCB WDB6-PCB.                             
015400                                                                          
015500 MAIN SECTION.                                                            
015600     ENTRY 'DLITCBL' USING WDG2-PCB WDB6-PCB.                             
015700                                                                          
015800     PERFORM A-INIT                                                       
015900     PERFORM S01-LAES-W5706V                                              
016000     PERFORM UNTIL END-OF-W5706V                                          
016100       PERFORM S02-GET-CURRENCY                                           
016200       PERFORM BA-KONTROLLERA-POST                                        
016300       PERFORM S01-LAES-W5706V                                            
016400     END-PERFORM                                                          
016500                                                                          
016600     PERFORM Z-FINIT                                                      
016700                                                                          
016800     MOVE ZERO TO  RETURN-CODE                                            
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
017300 A-INIT SECTION.                                                          
017400     OPEN INPUT  W5706V                                                   
017500     OPEN OUTPUT W57064                                                   
017600     SKIP2                                                                
017700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
017800     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
017900                        W-DATE-AAMM(1:2)                                  
018000                        WS-TIAA                                           
018100     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
018200                        W-DATE-AAMM(3:2)                                  
018300                        WS-TIMM                                           
018400     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
018500     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
018600                                                                          
018700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018800                                                                          
018900     .                                                                    
019000     EJECT                                                                
019100                                                                          
019200 BA-KONTROLLERA-POST SECTION.                                             
019300     IF  (IN1-EKH-KDEKHHT = '102'                                         
019400     AND IN1-EKH-KDEKSHT = '120')                                         
019500     OR  (IN1-EKH-KDEKHHT = '102'                                         
019600     AND IN1-EKH-KDEKSHT = '124')                                         
019700     OR  (IN1-EKH-KDEKHHT = '102'                                         
019800     AND IN1-EKH-KDEKSHT = '125')                                         
019900     OR  (IN1-EKH-KDEKHHT = '102'                                         
020000     AND IN1-EKH-KDEKSHT = '130')                                         
020100     OR  (IN1-EKH-KDEKHHT = '102'                                         
020200     AND IN1-EKH-KDEKSHT = '134')                                         
020300     OR  (IN1-EKH-KDEKHHT = '303'                                         
020400     AND IN1-EKH-KDEKSHT = '361')                                         
020500     OR  (IN1-EKH-KDEKHHT = '303'                                         
020600     AND IN1-EKH-KDEKSHT = '371')                                         
020700     OR  (IN1-EKH-KDEKHHT = '303'                                         
020800     AND IN1-EKH-KDEKSHT = '3XX')                                         
020900     OR  (IN1-EKH-KDEKHHT = '303'                                         
021000     AND IN1-EKH-KDEKSHT = '301')                                         
021100     OR  (IN1-EKH-KDEKHHT = '303'                                         
021200     AND IN1-EKH-KDEKSHT = '307')                                         
021300**** NOLLSTÄLLNING AV BERÄKNING AV DDI POST MELLAN EVENT                  
021400       IF IN1-EKH-IDVERGL = WS-EKH-IDVERGL                                
021500         CONTINUE                                                         
021600       ELSE                                                               
021700         MOVE ZERO            TO WS-TOT-AMOUNT-DDI                        
021800                                 WS-LINE-AMOUNT-DDI                       
021900                                 WS-DIFF-AMOUNT-DDI                       
022000                                 WS-SUBEL                                 
022100                                 WS-PRARTNTO                              
022110                                 WS-PRARTNTO1                             
022200         MOVE IN1-EKH-IDVERGL TO WS-EKH-IDVERGL                           
022300       END-IF                                                             
022400* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
022500       IF IN1-EKH-KDEKNIVA = 'DET'                                        
022600         COMPUTE WS-PRARTNTO ROUNDED = IN1-EKH-PRARTNTO *                 
022700                 IN1-EKH-KVANTAL / WS-PRKURS-SC3                          
022800         COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +               
022900                                    WS-PRARTNTO                           
023000         PERFORM BD-SKICKA-RATT-POST                                      
023100       ELSE                                                               
023200         IF IN1-EKH-KDEKNIVA = 'SUM'                                      
023300           COMPUTE WS-SUBEL ROUNDED = IN1-EKH-SUBEL  /                    
023400                                      WS-PRKURS-SC3                       
023500           PERFORM BD-SKICKA-RATT-POST                                    
023600                                                                          
023700           COMPUTE WS-TOT-AMOUNT-DDI =  WS-SUBEL                          
023800           COMPUTE WS-DIFF-AMOUNT-DDI =  WS-TOT-AMOUNT-DDI -              
023900                                         WS-LINE-AMOUNT-DDI               
024000           IF WS-DIFF-AMOUNT-DDI NOT = ZERO                               
024100                                                                          
024200             MOVE 'DDI'                TO IN1-EKH-KDEKNIVA                
024300             MOVE WS-DIFF-AMOUNT-DDI   TO IN1-EKH-SUBEL                   
024400             MOVE ZERO                 TO IN1-EKH-SUVAT                   
024500                                                                          
024600               PERFORM BD-SKICKA-RATT-POST                                
024700           END-IF                                                         
024800           MOVE ZERO                 TO WS-TOT-AMOUNT-DDI                 
024900                                        WS-LINE-AMOUNT-DDI                
025000                                        WS-DIFF-AMOUNT-DDI                
025100                                        WS-SUBEL                          
025200                                        WS-PRARTNTO                       
025210                                        WS-PRARTNTO1                      
025300         ELSE                                                             
025400           COMPUTE WS-SUBEL ROUNDED = IN1-EKH-SUBEL  /                    
025500                                      WS-PRKURS-SC3                       
025600           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
025700                                      WS-SUBEL                            
025800           PERFORM BD-SKICKA-RATT-POST                                    
025900         END-IF                                                           
026000       END-IF                                                             
026100     ELSE                                                                 
026200       IF  (IN1-EKH-KDEKHHT = '103'                                       
026300       AND IN1-EKH-KDEKSHT = '102')                                       
026310       OR  (IN1-EKH-KDEKHHT = '103'                                       
026320       AND IN1-EKH-KDEKSHT = '106')                                       
026330       OR  (IN1-EKH-KDEKHHT = '103'                                       
026340       AND IN1-EKH-KDEKSHT = '107')                                       
026350       OR  (IN1-EKH-KDEKHHT = '102'                                       
026360       AND IN1-EKH-KDEKSHT = '107')                                       
026400         IF IN1-EKH-IDVERGL = WS-EKH-IDVERGL                              
026500           CONTINUE                                                       
026600         ELSE                                                             
026700           MOVE ZERO            TO WS-TOT-AMOUNT-DDI                      
026800                                   WS-LINE-AMOUNT-DDI                     
026900                                   WS-DIFF-AMOUNT-DDI                     
027000                                   WS-SUBEL                               
027100                                   WS-PRARTNTO                            
027110                                   WS-PRARTNTO1                           
027200           MOVE IN1-EKH-IDVERGL TO WS-EKH-IDVERGL                         
027300         END-IF                                                           
027400         IF IN1-EKH-KDEKNIVA = 'DET'                                      
027410           IF WS-IDLANDX2-CN                                              
027500             COMPUTE WS-PRARTNTO ROUNDED =                                
027600                     IN1-EKH-KVANTAL * IN1-EKH-PRARTSTD *                 
027601                     IN1-EKH-PRKURS                                       
027610           ELSE                                                           
027611             COMPUTE WS-PRARTNTO ROUNDED =                                
027612                     IN1-EKH-KVANTAL * IN1-EKH-PRARTSTD                   
027620           END-IF                                                         
027700           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
027800                                      WS-PRARTNTO                         
027810           PERFORM BD-SKICKA-RATT-POST                                    
027900         ELSE                                                             
027901           IF IN1-EKH-KDEKNIVA = 'ARB'                                    
027910             COMPUTE WS-PRARTNTO1 ROUNDED =                               
027911                     IN1-EKH-SUBEL * IN1-EKH-PRKURS                       
027912             COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +           
027913                                      WS-PRARTNTO1                        
027914             PERFORM BD-SKICKA-RATT-POST                                  
027920           ELSE                                                           
028000* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
028100             IF IN1-EKH-KDEKNIVA = 'SUM'                                  
028200               COMPUTE WS-SUBEL ROUNDED = IN1-EKH-SUBEL *                 
028300                                          IN1-EKH-PRKURS                  
028400               PERFORM BD-SKICKA-RATT-POST                                
028500                                                                          
028600               COMPUTE WS-TOT-AMOUNT-DDI =  WS-SUBEL                      
028610               IF (WS-LINE-AMOUNT-DDI <= 0 AND                            
028611                   WS-TOT-AMOUNT-DDI  <= 0) OR                            
028612                  (WS-LINE-AMOUNT-DDI >= 0 AND                            
028613                   WS-TOT-AMOUNT-DDI  >= 0)                               
028614                 COMPUTE WS-DIFF-AMOUNT-DDI =  WS-TOT-AMOUNT-DDI -        
028615                                               WS-LINE-AMOUNT-DDI         
028616               ELSE                                                       
028617                 COMPUTE WS-DIFF-AMOUNT-DDI =  WS-TOT-AMOUNT-DDI +        
028618                                               WS-LINE-AMOUNT-DDI         
028640               END-IF                                                     
028900               IF WS-DIFF-AMOUNT-DDI NOT = ZERO                           
029000                 MOVE 'DDI'                TO IN1-EKH-KDEKNIVA            
029100                 MOVE WS-DIFF-AMOUNT-DDI   TO IN1-EKH-SUBEL               
029200                 MOVE ZERO                 TO IN1-EKH-SUVAT               
029300                                                                          
029400                 PERFORM BD-SKICKA-RATT-POST                              
029500               END-IF                                                     
029600               MOVE ZERO                 TO WS-TOT-AMOUNT-DDI             
029700                                            WS-LINE-AMOUNT-DDI            
029800                                            WS-DIFF-AMOUNT-DDI            
029900                                            WS-SUBEL                      
030000                                            WS-PRARTNTO                   
030001                                            WS-PRARTNTO1                  
030010             ELSE                                                         
030011               PERFORM BD-SKICKA-RATT-POST                                
030020             END-IF                                                       
030021           END-IF                                                         
030030         END-IF                                                           
030100       ELSE                                                               
030200           PERFORM BD-SKICKA-RATT-POST                                    
030300       END-IF                                                             
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700                                                                          
030800 BD-SKICKA-RATT-POST SECTION.                                             
030900     MOVE IN1-AREA TO RATT-AREA                                           
031000     PERFORM S11-SKRIV-RATT-POST                                          
031100     .                                                                    
031200     EJECT                                                                
031300                                                                          
031400 Z-FINIT SECTION.                                                         
031500     CLOSE W5706V                                                         
031600           W57064                                                         
031700     SKIP2                                                                
031800     MOVE 'S' TO POSTSUM-OPKOD                                            
031900     CALL POSTSUM USING POSTSUM-PARM                                      
032000     .                                                                    
032100     EJECT                                                                
032200                                                                          
032300 S01-LAES-W5706V  SECTION.                                                
032400     READ W5706V INTO IN1-AREA                                            
032500     AT END                                                               
032600        MOVE HIGH-VALUE TO IN1-AREA                                       
032700        SET END-OF-W5706V TO TRUE                                         
032800                                                                          
032900     NOT AT END                                                           
033000        MOVE 'W5706V' TO POSTSUM-FDNAMN                                   
033100        MOVE 'W5706ED1' TO POSTSUM-DDNAMN2                                
033200        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
033300        CALL POSTSUM USING POSTSUM-PARM                                   
033400     END-READ                                                             
033500     .                                                                    
033600     EJECT                                                                
033700                                                                          
033800 S02-GET-CURRENCY SECTION.                                                
033900     IF IN1-FIL-IDPGM = 'W4183300'                                        
034000       IF IN1-EKH-DAAVIDAT > ZERO                                         
034100         MOVE IN1-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                         
034200         MOVE IN1-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                         
034300       ELSE                                                               
034400         MOVE WS-TIAA              TO WS-TIAA-CR                          
034500         MOVE WS-TIMM              TO WS-TIMM-CR                          
034600       END-IF                                                             
034700     ELSE                                                                 
034800       MOVE WS-TIAA                TO WS-TIAA-CR                          
034900       MOVE WS-TIMM                TO WS-TIMM-CR                          
035000     END-IF                                                               
035100     MOVE WS-TIAA-CR               TO W-DATE-AAMM(1:2)                    
035200     MOVE WS-TIMM-CR               TO W-DATE-AAMM(3:2)                    
035300**   IF IN1-EKH-KDTRADP = WS-SAVE-KDTRADP                                 
035400**     CONTINUE                                                           
035500**   ELSE                                                                 
035600     MOVE IN1-EKH-KDTRADP       TO WS-SAVE-KDTRADP                        
035700                                   W-KDTRADP                              
035800     PERFORM IMS-GU-WDB601-TRADP                                          
035810     MOVE DCS-IDLANDX2          TO WS-IDLANDX2                            
035900     MOVE DCS-KDVALISO          TO CURR-KDVALISO-ROW                      
036000     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
036100     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
036200     MOVE 'M'                   TO CURR-KDVALTYP                          
036300     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
036400     IF CURR-KDSVAR = ' '                                                 
036500       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-SC                           
036600     ELSE                                                                 
036700       MOVE 1                   TO WS-PRKURS-SC                           
036800     END-IF                                                               
036900     MOVE WS-PRKURS-SC          TO WS-PRKURS-SC3                          
037000*    END-IF                                                               
037100     .                                                                    
037200     EJECT                                                                
037300                                                                          
037400 S11-SKRIV-RATT-POST SECTION.                                             
037500     WRITE RATT-POST FROM RATT-AREA                                       
037600                                                                          
037700     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
037800     MOVE 'W57064' TO POSTSUM-FDNAMN                                      
037900     MOVE 'W5706ED2' TO POSTSUM-DDNAMN2                                   
038000     CALL POSTSUM USING POSTSUM-PARM                                      
038100     .                                                                    
038200     EJECT                                                                
038300                                                                          
038400* --- IMS SECTIONS ---                                                    
038500                                                                          
038600 IMS-GU-WDB601-TRADP SECTION.                                             
038700     STRING 'WDB601  (KDTRADP  =' W-KDTRADP ')'                           
038800            DELIMITED BY SIZE INTO SSA1                                   
038900     MOVE '  GE'                 TO GODK-STATUSKODER                      
039000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
039100     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
039200     PERFORM IMS-STATUS-CONTROL                                           
039300     .                                                                    
039400     SKIP3                                                                
039500                                                                          
039600 IMS-STATUS-CONTROL SECTION.                                              
039700     SET STATUS-IX TO 1                                                   
039800     SEARCH GODK-STATUS                                                   
039900       AT END                                                             
040000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040100           DELIMITED BY SIZE INTO FELTEXT                                 
040200         DISPLAY FELTEXT                                                  
040300         CALL FELLOG                                                      
040400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040500         CONTINUE                                                         
040600     END-SEARCH                                                           
040700     .                                                                    
