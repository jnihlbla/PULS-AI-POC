000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5131400.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/06/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER FIL OCH WDB6 SKAPAR FIL TILL DAP                           
000900*        ADJUSTMENTS WEEKLY FOLLOW UP FÖR MANAGEMENT                      
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDB6                                       
001200*                                                                         
001300*    ABENDKODER:                                                          
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
002500*          --- INFIL01                                                    
002600     SELECT W51315                     ASSIGN TO W51314D1.                
002700*          --- UTFIL01                                                    
002800     SELECT W513141                    ASSIGN TO W51314D2.                
002900*          --- UTFIL02                                                    
003000     SELECT W513142                    ASSIGN TO W51314D3.                
003100*          --- UTFIL03                                                    
003200     SELECT W513143                    ASSIGN TO W51314D4.                
003300*          --- UTFIL04                                                    
003400     SELECT W513144                    ASSIGN TO W51314D5.                
003500*          --- UTFIL05                                                    
003600     SELECT W513145                    ASSIGN TO W51314D6.                
003700*          --- UTFIL06                                                    
003800     SELECT W513146                    ASSIGN TO W51314D7.                
003900*          --- UTFIL07                                                    
004000     SELECT W513147                    ASSIGN TO W51314D8.                
004100*          --- UTFIL08                                                    
004200     SELECT W513148                    ASSIGN TO W51314D9.                
004300*          --- UTFIL09                                                    
004400     SELECT W513149                    ASSIGN TO W51314DA.                
004500*          --- UTFIL10                                                    
004600     SELECT W51314A                    ASSIGN TO W51314DB.                
004700*          --- UTFIL11                                                    
004800     SELECT W51314B                    ASSIGN TO W51314DC.                
004900*          --- UTFIL12                                                    
005000     SELECT W51314C                    ASSIGN TO W51314DD.                
005010*          --- UTFIL13                                                    
005020     SELECT W51314D                    ASSIGN TO W51314DE.                
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300     SKIP2                                                                
005400 FILE SECTION.                                                            
005500     SKIP3                                                                
005600 FD  W51315                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY W51315       -L.                                               
006100                                                                          
006200 FD  W513141                                                              
006300     RECORDING V                                                          
006400     BLOCK 0 RECORDS.                                                     
006500*01  UT-POST      -COPY W513141  -L.                                      
006600                                                                          
006700 FD  W513142                                                              
006800     RECORDING V                                                          
006900     BLOCK 0 RECORDS.                                                     
007000*01  UT-POST2     -COPY W513141  -L.                                      
007100     SKIP3                                                                
007200                                                                          
007300 FD  W513143                                                              
007400     RECORDING V                                                          
007500     BLOCK 0 RECORDS.                                                     
007600*01  UT-POST3     -COPY W513141  -L.                                      
007700     SKIP3                                                                
007800                                                                          
007900 FD  W513144                                                              
008000     RECORDING V                                                          
008100     BLOCK 0 RECORDS.                                                     
008200*01  UT-POST4     -COPY W513141  -L.                                      
008300                                                                          
008400 FD  W513145                                                              
008500     RECORDING V                                                          
008600     BLOCK 0 RECORDS.                                                     
008700*01  UT-POST5     -COPY W513141  -L.                                      
008800                                                                          
008900 FD  W513146                                                              
009000     RECORDING V                                                          
009100     BLOCK 0 RECORDS.                                                     
009200*01  UT-POST6     -COPY W513141  -L.                                      
009300                                                                          
009400 FD  W513147                                                              
009500     RECORDING V                                                          
009600     BLOCK 0 RECORDS.                                                     
009700*01  UT-POST7     -COPY W513141  -L.                                      
009800     SKIP3                                                                
009900                                                                          
010000 FD  W513148                                                              
010100     RECORDING V                                                          
010200     BLOCK 0 RECORDS.                                                     
010300*01  UT-POST8     -COPY W513141  -L.                                      
010400     SKIP3                                                                
010500                                                                          
010600 FD  W513149                                                              
010700     RECORDING V                                                          
010800     BLOCK 0 RECORDS.                                                     
010900*01  UT-POST9     -COPY W513141  -L.                                      
011000     SKIP3                                                                
011100                                                                          
011200 FD  W51314A                                                              
011300     RECORDING V                                                          
011400     BLOCK 0 RECORDS.                                                     
011500*01  UT-POST10    -COPY W513141  -L.                                      
011600     SKIP3                                                                
011700                                                                          
011800 FD  W51314B                                                              
011900     RECORDING V                                                          
012000     BLOCK 0 RECORDS.                                                     
012100*01  UT-POST11    -COPY W513141  -L.                                      
012200     SKIP3                                                                
012300                                                                          
012400 FD  W51314C                                                              
012500     RECORDING V                                                          
012600     BLOCK 0 RECORDS.                                                     
012700*01  UT-POST12    -COPY W513141  -L.                                      
012710     SKIP3                                                                
012720                                                                          
012730 FD  W51314D                                                              
012740     RECORDING V                                                          
012750     BLOCK 0 RECORDS.                                                     
012760*01  UT-POST13    -COPY W513141  -L.                                      
012800     SKIP3                                                                
012900     EJECT                                                                
013000 WORKING-STORAGE SECTION.                                                 
013100                                                                          
013200 77  IDPGM                       PIC X(8)    VALUE 'W5131400'.            
013300 77  JA                          PIC X       VALUE 'J'.                   
013400 77  YES                         PIC X       VALUE 'Y'.                   
013500 77  NEJ                         PIC X       VALUE 'N'.                   
013600 77  INDX                        PIC S9(9) COMP-3.                        
013700 77  ANTAL-POSTER                PIC S9(9) COMP-3.                        
013800 77  W-ADCITY                    PIC X(20).                               
013900 77  W-IDLANDX2                  PIC X(2).                                
014000 77  WS-SAVE-IDFTG               PIC 9(2) VALUE 0.                        
014100 77  WDCS-KDMFUP                 PIC X(2)    VALUE SPACE.                 
014200                                                                          
014300 77  W51315-EOF-SW               PIC X       VALUE 'N'.                   
014400     88  END-OF-W51315                       VALUE 'J'.                   
014500     EJECT                                                                
014600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014700 01  FILLER REDEFINES DAGENS-DATUM.                                       
014800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015100                                                                          
015200 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
015300 01  FILLER REDEFINES DAGENS-AAVV.                                        
015400     03  DAGENS-DATUM-AA         PIC 9(2).                                
015500     03  DAGENS-DATUM-VV         PIC 9(2).                                
015600     EJECT                                                                
015700 01  TABENTRY-PARM.                                                       
015800     03  STEGLAANGD              PIC S9(9) COMP.                          
015900     03  ANTAL                   PIC S9(9) COMP.                          
016000     03  NYCKELLAANGD            PIC S9(9) COMP.                          
016100 01  FILLER                      PIC X(16) VALUE 'SORT-TABELL'.           
016200 01  SORT-TABELL.                                                         
016300     03  TAB-RAD OCCURS 500.                                              
016400        05  TAB-SORT-BEGREPP1.                                            
016500            07 TAB-IDLANDX2      PIC X(2).                                
016600            07 TAB-IDDC          PIC X(2).                                
016700        05  TAB-ADLAGOMR         PIC X(5).                                
016800        05  TAB-BETEXT           PIC X(5).                                
016900        05  TAB-ADCITY           PIC X(20).                               
017000        05  TAB-SUARTSTD-WEEK    PIC S9(9)V9(2) COMP-3.                   
017100        05  TAB-SUARTSTD-WEEKAVG PIC S9(9)V9(2) COMP-3.                   
017200        05  TAB-SUARTSTD-TOT     PIC S9(9)V9(2) COMP-3.                   
017300        05  TAB-REDIFF           PIC S9(9)V9(2) COMP-3.                   
017400        05  TAB-SUARTSTD-DIFF    PIC S9(9)V9(2) COMP-3.                   
017500        05  TAB-REDIFF-LYEAR     PIC S9(9)V9(2) COMP-3.                   
017600        05  TAB-TIAAVV           PIC 9(4).                                
017700        05  TAB-IDAFPRCD         PIC X(10).                               
017800                                                                          
017900 01  DYNAMISKA-SUBPROGRAM.                                                
018000*                                                                         
018100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
018200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
018600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
018700     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
018800                                                                          
018900     SKIP2                                                                
019000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
019100                                                                          
019200*01  -COPY WDATKORT                                                       
019300     EJECT                                                                
019400*    --- PARAMETRAR TILL WL10WBDC                                         
019500*01  -COPY WL10WBDC                                                       
019600     EJECT                                                                
019700*    --- PARAMETRAR TILL ABEND                                            
019800                                                                          
019900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
020100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
020200     SKIP2                                                                
020300 01  FELTEXT.                                                             
020400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
020500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
020600     EJECT                                                                
020700*    --- PARAMETRAR TILL POSTSUM                                          
020800*                                                                         
020900*01  -COPY W0005   -PRE  POSTSUM-                                         
021000                                                                          
021100*    --- IDFTG VALUES                                                     
021200*                                                                         
021300*01  -COPY WWIDFTG                                                        
021400     EJECT                                                                
021500 01  IN-AREA-START               PIC X(24)   VALUE                        
021600                                 'IN-AREA-START  '.                       
021700     SKIP2                                                                
021800                                                                          
021900*01  AREA -COPY W51315      -PRE IN-                                      
022000     EJECT                                                                
022100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022200*                                                                         
022300     EJECT                                                                
022400 01  UT-AREA-START               PIC X(24)   VALUE                        
022500                                 'UT-AREA-START  '.                       
022600     SKIP2                                                                
022700                                                                          
022800*01  AREA -COPY W513141     -PRE UT-                                      
022900     EJECT                                                                
023000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023100*                                                                         
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023400     SKIP3                                                                
023500 01  NYCKLAR-TILL-DLI.                                                    
023600     03  W-IDDC-X.                                                        
023700         05  W-IDDC              PIC  X(2).                               
023800     SKIP2                                                                
023900*    --- STATUS-KOD FRÅN IMS                                              
024000 01  STATUS-WS                   PIC XX.                                  
024100     88  SEGMENT-FINNS                       VALUE '  '.                  
024200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024400     SKIP2                                                                
024500 01  GODK-STATUSKODER.                                                    
024600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024700     SKIP3                                                                
024800 01  SSA1                        PIC X(64).                               
024900 01  SSA2                        PIC X(64).                               
025000     EJECT                                                                
025100*    --- IMS FUNKTIONSKODER                                               
025200*01  -COPY W0003                                                          
025300     EJECT                                                                
025400*    ---  DLI INPUT-OUTPUT AREA                                           
025500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
025600 01  DLI-IO-WDB601.                                                       
025700*    03  -COPY WDB601                                                     
025800     EJECT                                                                
025900 LINKAGE SECTION.                                                         
026000                                                                          
026100*01  -COPY W0008  -PRE WDB6-                                              
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400 PROCEDURE DIVISION  USING WDB6-PCB.                                      
026500 MAIN SECTION.                                                            
026600     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
026700                                                                          
026800                                                                          
026900     PERFORM A-INIT                                                       
027000                                                                          
027100     PERFORM S01-LAES-W51315                                              
027200     PERFORM UNTIL END-OF-W51315                                          
027300*    LÄS FRAM TILL EN TOT POST                                            
027400       IF IN-ADLAGOMR = 'TOT'                                             
027500         MOVE IN-IDDC TO W-IDDC                                           
027600         PERFORM IMS-GET-WDB601                                           
027700         IF DCS-FLWEBDC = JA OR YES                                       
027800           MOVE DCS-IDLANDX2          TO W-IDLANDX2                       
027900           MOVE DCS-ADGMT-PADR(11:20) TO W-ADCITY                         
028000           PERFORM B-FYLL-TABELL                                          
028100           PERFORM S01-LAES-W51315                                        
028200           IF IN-BETEXT   = 'DOWN'                                        
028300             PERFORM B-FYLL-TABELL                                        
028400             PERFORM S01-LAES-W51315                                      
028500                                                                          
028600             IF IN-BETEXT   = ' NET'                                      
028700               PERFORM B-FYLL-TABELL                                      
028800               PERFORM S01-LAES-W51315                                    
028900             END-IF                                                       
029000           END-IF                                                         
029100         END-IF                                                           
029200       END-IF                                                             
029300       IF END-OF-W51315                                                   
029400         CONTINUE                                                         
029500       ELSE                                                               
029600         PERFORM S01-LAES-W51315                                          
029700       END-IF                                                             
029800     END-PERFORM                                                          
029900     IF INDX > +0                                                         
030000       PERFORM C-SORT-TABELL                                              
030100       PERFORM D-SKAPA-UTPOST                                             
030200     END-IF                                                               
030300     PERFORM Z-FINIT                                                      
030400                                                                          
030500     MOVE ZERO TO RETURN-CODE                                             
030600     GOBACK                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 A-INIT SECTION.                                                          
031000                                                                          
031100     OPEN INPUT  W51315                                                   
031200     OPEN OUTPUT W513141                                                  
031300                 W513142                                                  
031400                 W513143                                                  
031500                 W513144                                                  
031600                 W513145                                                  
031700                 W513146                                                  
031800                 W513147                                                  
031900                 W513148                                                  
032000                 W513149                                                  
032100                 W51314A                                                  
032200                 W51314B                                                  
032300                 W51314C                                                  
032310                 W51314D                                                  
032400                                                                          
032500     ACCEPT DAGENS-DATUM  FROM DATE                                       
032600     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
032700     MOVE D-AAR          TO DAGENS-DATUM-AA                               
032800                                                                          
032900     MOVE D-VECKA        TO DAGENS-DATUM-VV                               
033000                                                                          
033100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033200     MOVE +0 TO INDX                                                      
033300     .                                                                    
033400     EJECT                                                                
033500 B-FYLL-TABELL  SECTION.                                                  
033600     IF IN-ADLAGOMR = 'TOT' OR IN-BETEXT = 'DOWN' OR                      
033700        IN-BETEXT   = ' NET'                                              
033800       ADD +1 TO INDX                                                     
033900                                                                          
034000       MOVE IN-IDAFPRCD          TO TAB-IDAFPRCD          (INDX)          
034100       MOVE IN-IDDC              TO TAB-IDDC              (INDX)          
034200       MOVE DAGENS-AAVV          TO TAB-TIAAVV            (INDX)          
034300       MOVE IN-ADLAGOMR          TO TAB-ADLAGOMR          (INDX)          
034400       MOVE IN-BETEXT            TO TAB-BETEXT            (INDX)          
034500       MOVE IN-SUARTSTD-WEEK     TO TAB-SUARTSTD-WEEK     (INDX)          
034600       MOVE IN-SUARTSTD-WEEKAVG  TO TAB-SUARTSTD-WEEKAVG  (INDX)          
034700       MOVE IN-SUARTSTD-TOT      TO TAB-SUARTSTD-TOT      (INDX)          
034800       MOVE IN-REDIFF            TO TAB-REDIFF            (INDX)          
034900       MOVE IN-SUARTSTD-DIFF     TO TAB-SUARTSTD-DIFF     (INDX)          
035000       MOVE IN-REDIFF-LYEAR      TO TAB-REDIFF-LYEAR      (INDX)          
035100       IF IN-ADLAGOMR = 'TOT'                                             
035200         MOVE W-ADCITY           TO TAB-ADCITY            (INDX)          
035300       ELSE                                                               
035400         MOVE SPACE              TO TAB-ADCITY            (INDX)          
035500       END-IF                                                             
035600       MOVE W-IDLANDX2           TO TAB-IDLANDX2          (INDX)          
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 C-SORT-TABELL SECTION.                                                   
036100       MOVE +84   TO STEGLAANGD                                           
036200       MOVE INDX  TO ANTAL                                                
036300       MOVE +4    TO NYCKELLAANGD                                         
036400                                                                          
036500       CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                    
036600       TAB-SORT-BEGREPP1(1) NYCKELLAANGD                                  
036700     .                                                                    
036800     EJECT                                                                
036900 D-SKAPA-UTPOST SECTION.                                                  
037000     MOVE INDX TO ANTAL-POSTER                                            
037100     MOVE +1 TO INDX                                                      
037200     MOVE TAB-IDDC (INDX)     TO W-IDDC                                   
037300                                                                          
037400     PERFORM IMS-GET-WDB601                                               
037500     IF SEGMENT-FINNS                                                     
037600        MOVE DCS-IDFTG        TO WS-SAVE-IDFTG                            
037700     ELSE                                                                 
037800        MOVE 0                TO WS-SAVE-IDFTG                            
037900     END-IF                                                               
038000                                                                          
038100     PERFORM UNTIL INDX > ANTAL-POSTER                                    
038200       IF TAB-ADLAGOMR(INDX) = 'TOT' AND INDX > +1                        
038300         IF DCS-IDFTG NOT = WS-SAVE-IDFTG                                 
038400           MOVE TAB-IDAFPRCD      (INDX) TO UT-IDAFPRCD                   
038500           IF WS-SAVE-IDFTG = WC-IDFTG-PV                                 
038600              MOVE DCS-IDFTG                TO WS-SAVE-IDFTG              
038700           ELSE                                                           
038800             IF WS-SAVE-IDFTG = WC-IDFTG-IN                               
038900               MOVE DCS-IDFTG               TO WS-SAVE-IDFTG              
039000             ELSE                                                         
039100               IF WS-SAVE-IDFTG = WC-IDFTG-CN                             
039200                  MOVE DCS-IDFTG            TO WS-SAVE-IDFTG              
039300               ELSE                                                       
039400                 IF WS-SAVE-IDFTG = WC-IDFTG-KR                           
039500                    MOVE DCS-IDFTG          TO WS-SAVE-IDFTG              
039600                 ELSE                                                     
039700                   IF WS-SAVE-IDFTG = WC-IDFTG-AE                         
039800                      MOVE DCS-IDFTG        TO WS-SAVE-IDFTG              
039900                   ELSE                                                   
040000                     IF WS-SAVE-IDFTG = WC-IDFTG-TR                       
040100                        MOVE DCS-IDFTG     TO WS-SAVE-IDFTG               
040200                     ELSE                                                 
040300                       IF WS-SAVE-IDFTG = WC-IDFTG-MY                     
040400                          MOVE DCS-IDFTG   TO WS-SAVE-IDFTG               
040500                       ELSE                                               
040600                         IF WS-SAVE-IDFTG = WC-IDFTG-TH                   
040700                            MOVE DCS-IDFTG TO WS-SAVE-IDFTG               
040800                         ELSE                                             
040900                           IF WS-SAVE-IDFTG = WC-IDFTG-TW                 
041000                              MOVE DCS-IDFTG TO WS-SAVE-IDFTG             
041100                           ELSE                                           
041200                             IF WS-SAVE-IDFTG = WC-IDFTG-BR               
041300                                MOVE DCS-IDFTG TO WS-SAVE-IDFTG           
041400                             ELSE                                         
041500                              IF WS-SAVE-IDFTG = WC-IDFTG-MX              
041600                                 MOVE DCS-IDFTG TO WS-SAVE-IDFTG          
041700                              ELSE                                        
041710                               IF WS-SAVE-IDFTG = WC-IDFTG-ZA             
041720                                  MOVE DCS-IDFTG TO WS-SAVE-IDFTG         
041730                               END-IF                                     
041800                              END-IF                                      
041900                             END-IF                                       
042000                           END-IF                                         
042100                         END-IF                                           
042200                       END-IF                                             
042300                     END-IF                                               
042400                   END-IF                                                 
042500                 END-IF                                                   
042600               END-IF                                                     
042700             END-IF                                                       
042800           END-IF                                                         
042900           MOVE ALL '+'                  TO UT-AREA                       
043000         END-IF                                                           
043100       END-IF                                                             
043200                                                                          
043300       MOVE TAB-IDAFPRCD        (INDX) TO UT-IDAFPRCD                     
043400       MOVE TAB-IDDC            (INDX) TO UT-IDDC                         
043500       MOVE TAB-TIAAVV          (INDX) TO UT-TIAAVV                       
043600       MOVE TAB-ADLAGOMR        (INDX) TO UT-ADLAGOMR                     
043700       MOVE TAB-BETEXT          (INDX) TO UT-BETEXT                       
043800       MOVE TAB-SUARTSTD-WEEK   (INDX) TO UT-SUARTSTD-WEEK                
043900       MOVE TAB-SUARTSTD-WEEKAVG(INDX) TO UT-SUARTSTD-WEEKAVG             
044000       MOVE TAB-SUARTSTD-TOT    (INDX) TO UT-SUARTSTD-TOT                 
044100       MOVE TAB-REDIFF          (INDX) TO UT-REDIFF                       
044200       MOVE TAB-SUARTSTD-DIFF   (INDX) TO UT-SUARTSTD-DIFF                
044300       MOVE TAB-REDIFF-LYEAR    (INDX) TO UT-REDIFF-LYEAR                 
044400       MOVE TAB-IDLANDX2        (INDX) TO UT-IDLANDX2                     
044500       MOVE TAB-ADCITY          (INDX) TO UT-ADCITY                       
044600                                                                          
044700       IF TAB-ADLAGOMR(INDX) = 'TOT'                                      
044800         MOVE TAB-ADCITY        (INDX) TO UT-ADCITY                       
044900         MOVE TAB-IDDC          (INDX) TO UT-IDDC                         
045000         MOVE TAB-ADLAGOMR      (INDX) TO UT-ADLAGOMR                     
045100       ELSE                                                               
045200         MOVE SPACE                    TO UT-ADCITY                       
045300                                          UT-IDDC                         
045400                                          UT-ADLAGOMR                     
045500                                          UT-IDLANDX2                     
045600                                                                          
045700       END-IF                                                             
045800                                                                          
045900       MOVE TAB-IDDC(INDX) TO WBDC-IDDC                                   
046000       CALL WL10WBDC USING WBDC-AREA                                      
046100       IF WBDC-FLWEBDC = JA                                               
046200         MOVE WBDC-KDMFUP TO WDCS-KDMFUP                                  
046300       ELSE                                                               
046400         MOVE SPACE       TO WDCS-KDMFUP                                  
046500       END-IF                                                             
046600                                                                          
046700       IF WS-SAVE-IDFTG = WC-IDFTG-PV                                     
046800         IF WDCS-KDMFUP = 'MA'                                            
046900           PERFORM S02-SKRIV-UTPOST                                       
047000         END-IF                                                           
047100         IF WDCS-KDMFUP = 'PF'                                            
047200           PERFORM S04-SKRIV-UTPOST3                                      
047300         END-IF                                                           
047400       END-IF                                                             
047500       IF WS-SAVE-IDFTG = WC-IDFTG-CN                                     
047600         PERFORM S03-SKRIV-UTPOST2                                        
047700       END-IF                                                             
047800       IF WS-SAVE-IDFTG = WC-IDFTG-IN                                     
047900         PERFORM S05-SKRIV-UTPOST4                                        
048000       END-IF                                                             
048100       IF WS-SAVE-IDFTG = WC-IDFTG-KR                                     
048200         PERFORM S06-SKRIV-UTPOST5                                        
048300       END-IF                                                             
048400       IF WS-SAVE-IDFTG = WC-IDFTG-AE                                     
048500         PERFORM S07-SKRIV-UTPOST6                                        
048600       END-IF                                                             
048700       IF WS-SAVE-IDFTG = WC-IDFTG-TR                                     
048800         PERFORM S08-SKRIV-UTPOST7                                        
048900       END-IF                                                             
049000       IF WS-SAVE-IDFTG = WC-IDFTG-MY                                     
049100         PERFORM S09-SKRIV-UTPOST8                                        
049200       END-IF                                                             
049300       IF WS-SAVE-IDFTG = WC-IDFTG-TH                                     
049400         PERFORM S10-SKRIV-UTPOST9                                        
049500       END-IF                                                             
049600       IF WS-SAVE-IDFTG = WC-IDFTG-TW                                     
049700         PERFORM S11-SKRIV-UTPOST10                                       
049800       END-IF                                                             
049900       IF WS-SAVE-IDFTG = WC-IDFTG-BR                                     
050000         PERFORM S12-SKRIV-UTPOST11                                       
050100       END-IF                                                             
050200       IF WS-SAVE-IDFTG = WC-IDFTG-MX                                     
050300         PERFORM S13-SKRIV-UTPOST12                                       
050400       END-IF                                                             
050410       IF WS-SAVE-IDFTG = WC-IDFTG-ZA                                     
050420         PERFORM S14-SKRIV-UTPOST13                                       
050430       END-IF                                                             
050500       ADD +1 TO INDX                                                     
050600       MOVE TAB-IDDC (INDX)            TO W-IDDC                          
050700                                                                          
050800       PERFORM IMS-GET-WDB601                                             
050900       IF SEGMENT-FINNS                                                   
051000          CONTINUE                                                        
051100       ELSE                                                               
051200          MOVE 0                       TO WS-SAVE-IDFTG                   
051300       END-IF                                                             
051400                                                                          
051500     END-PERFORM                                                          
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900 Z-FINIT SECTION.                                                         
052000     CLOSE W51315                                                         
052100     CLOSE W513141                                                        
052200           W513142                                                        
052300           W513143                                                        
052400           W513144                                                        
052500           W513145                                                        
052600           W513146                                                        
052700           W513147                                                        
052800           W513148                                                        
052900           W513149                                                        
053000           W51314A                                                        
053100           W51314B                                                        
053200           W51314C                                                        
053210           W51314D                                                        
053300     SKIP2                                                                
053400     MOVE 'S' TO POSTSUM-OPKOD                                            
053500     CALL POSTSUM USING POSTSUM-PARM                                      
053600     .                                                                    
053700     EJECT                                                                
053800 S01-LAES-W51315  SECTION.                                                
053900     READ W51315 INTO IN-AREA                                             
054000     AT END                                                               
054100        MOVE HIGH-VALUE TO IN-AREA                                        
054200        SET END-OF-W51315 TO TRUE                                         
054300                                                                          
054400     NOT AT END                                                           
054500        MOVE 'W51315' TO POSTSUM-FDNAMN                                   
054600        MOVE 'W51314D1' TO POSTSUM-DDNAMN2                                
054700        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
054800        CALL POSTSUM USING POSTSUM-PARM                                   
054900     END-READ                                                             
055000     .                                                                    
055100     EJECT                                                                
055200                                                                          
055300 S02-SKRIV-UTPOST        SECTION.                                         
055400     WRITE UT-POST FROM UT-AREA                                           
055500                                                                          
055600     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
055700     MOVE 'W51314D2' TO POSTSUM-DDNAMN2                                   
055800     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
055900     CALL POSTSUM USING POSTSUM-PARM                                      
056000     .                                                                    
056100     EJECT                                                                
056200                                                                          
056300 S03-SKRIV-UTPOST2       SECTION.                                         
056400     WRITE UT-POST2 FROM UT-AREA                                          
056500                                                                          
056600     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
056700     MOVE 'W51314D3' TO POSTSUM-DDNAMN2                                   
056800     MOVE 'UT2'      TO POSTSUM-TRANSTYP                                  
056900     CALL POSTSUM USING POSTSUM-PARM                                      
057000     .                                                                    
057100     EJECT                                                                
057200                                                                          
057300 S04-SKRIV-UTPOST3       SECTION.                                         
057400     WRITE UT-POST3 FROM UT-AREA                                          
057500                                                                          
057600     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
057700     MOVE 'W51314D4' TO POSTSUM-DDNAMN2                                   
057800     MOVE 'UT3'      TO POSTSUM-TRANSTYP                                  
057900     CALL POSTSUM USING POSTSUM-PARM                                      
058000     .                                                                    
058100     EJECT                                                                
058200 S05-SKRIV-UTPOST4       SECTION.                                         
058300     WRITE UT-POST4 FROM UT-AREA                                          
058400                                                                          
058500     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
058600     MOVE 'W51314D5' TO POSTSUM-DDNAMN2                                   
058700     MOVE 'UT4'      TO POSTSUM-TRANSTYP                                  
058800     CALL POSTSUM USING POSTSUM-PARM                                      
058900     .                                                                    
059000     EJECT                                                                
059100 S06-SKRIV-UTPOST5       SECTION.                                         
059200     WRITE UT-POST5 FROM UT-AREA                                          
059300                                                                          
059400     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
059500     MOVE 'W51314D6' TO POSTSUM-DDNAMN2                                   
059600     MOVE 'UT5'      TO POSTSUM-TRANSTYP                                  
059700     CALL POSTSUM USING POSTSUM-PARM                                      
059800     .                                                                    
059900     EJECT                                                                
060000 S07-SKRIV-UTPOST6       SECTION.                                         
060100     WRITE UT-POST6 FROM UT-AREA                                          
060200                                                                          
060300     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
060400     MOVE 'W51314D7' TO POSTSUM-DDNAMN2                                   
060500     MOVE 'UT6'      TO POSTSUM-TRANSTYP                                  
060600     CALL POSTSUM USING POSTSUM-PARM                                      
060700     .                                                                    
060800     EJECT                                                                
060900 S08-SKRIV-UTPOST7       SECTION.                                         
061000     WRITE UT-POST7 FROM UT-AREA                                          
061100                                                                          
061200     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
061300     MOVE 'W51314D8' TO POSTSUM-DDNAMN2                                   
061400     MOVE 'UT7'      TO POSTSUM-TRANSTYP                                  
061500     CALL POSTSUM USING POSTSUM-PARM                                      
061600     .                                                                    
061700     EJECT                                                                
061800 S09-SKRIV-UTPOST8       SECTION.                                         
061900     WRITE UT-POST8 FROM UT-AREA                                          
062000                                                                          
062100     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
062200     MOVE 'W51314D9' TO POSTSUM-DDNAMN2                                   
062300     MOVE 'UT8'      TO POSTSUM-TRANSTYP                                  
062400     CALL POSTSUM USING POSTSUM-PARM                                      
062500     .                                                                    
062600     EJECT                                                                
062700 S10-SKRIV-UTPOST9       SECTION.                                         
062800     WRITE UT-POST9 FROM UT-AREA                                          
062900                                                                          
063000     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
063100     MOVE 'W51314DA' TO POSTSUM-DDNAMN2                                   
063200     MOVE 'UT9'      TO POSTSUM-TRANSTYP                                  
063300     CALL POSTSUM USING POSTSUM-PARM                                      
063400     .                                                                    
063500     EJECT                                                                
063600 S11-SKRIV-UTPOST10      SECTION.                                         
063700     WRITE UT-POST10 FROM UT-AREA                                         
063800                                                                          
063900     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
064000     MOVE 'W51314DB' TO POSTSUM-DDNAMN2                                   
064100     MOVE 'UT10'      TO POSTSUM-TRANSTYP                                 
064200     CALL POSTSUM USING POSTSUM-PARM                                      
064300     .                                                                    
064400     EJECT                                                                
064500 S12-SKRIV-UTPOST11      SECTION.                                         
064600     WRITE UT-POST11 FROM UT-AREA                                         
064700                                                                          
064800     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
064900     MOVE 'W51314DC' TO POSTSUM-DDNAMN2                                   
065000     MOVE 'UT11'      TO POSTSUM-TRANSTYP                                 
065100     CALL POSTSUM USING POSTSUM-PARM                                      
065200     .                                                                    
065300     EJECT                                                                
065400 S13-SKRIV-UTPOST12      SECTION.                                         
065500     WRITE UT-POST12 FROM UT-AREA                                         
065600                                                                          
065700     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
065800     MOVE 'W51314DD' TO POSTSUM-DDNAMN2                                   
065900     MOVE 'UT12'      TO POSTSUM-TRANSTYP                                 
066000     CALL POSTSUM USING POSTSUM-PARM                                      
066100     .                                                                    
066110     EJECT                                                                
066120 S14-SKRIV-UTPOST13      SECTION.                                         
066130     WRITE UT-POST13 FROM UT-AREA                                         
066140                                                                          
066150     MOVE 'W51314'   TO POSTSUM-FDNAMN                                    
066160     MOVE 'W51314DE' TO POSTSUM-DDNAMN2                                   
066170     MOVE 'UT13'      TO POSTSUM-TRANSTYP                                 
066180     CALL POSTSUM USING POSTSUM-PARM                                      
066190     .                                                                    
066200     EJECT                                                                
066300 S99-ABEND SECTION.                                                       
066400                                                                          
066500     SKIP2                                                                
066600     MOVE 'S' TO POSTSUM-OPKOD                                            
066700     CALL POSTSUM USING POSTSUM-PARM                                      
066800     CALL ABEND USING RKOD-ABEND                                          
066900     .                                                                    
067000     EJECT                                                                
067100* --- IMS SEKTIONER ---                                                   
067200                                                                          
067300     EJECT                                                                
067400 IMS-GET-WDB601 SECTION.                                                  
067500                                                                          
067600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
067700          DELIMITED BY SIZE INTO SSA1                                     
067800     MOVE '  GE' TO GODK-STATUSKODER                                      
067900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
068000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
068100     PERFORM IMS-STATUSKONTROLL                                           
068200     .                                                                    
068300     EJECT                                                                
068400 IMS-STATUSKONTROLL SECTION.                                              
068500                                                                          
068600     SET STATUS-IX TO 1                                                   
068700     SEARCH GODK-STATUS                                                   
068800       AT END                                                             
068900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
069000           DELIMITED BY SIZE INTO FELTEXT                                 
069100         DISPLAY FELTEXT                                                  
069200         CALL FELLOG                                                      
069300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
069400         CONTINUE                                                         
069500     END-SEARCH                                                           
070000     .                                                                    
