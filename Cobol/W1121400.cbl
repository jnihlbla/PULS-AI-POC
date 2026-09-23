000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1121400.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   91/01/31.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSNING WDJ1.                                                   
001100*        - TOMMA RÖTTER MED REGISTRERINGSDATUM ÄLDRE ÄN 3 MÅN             
001200*          RENSAS.                                                        
001300*        - TOMMA RÖTTER DÄR STRUKTURNR FINNS PÅ ARTREG MED ANNAN          
001400*          SORT ÄN SATS RENSAS.                                           
001500*        - EJ 1002-SATSER STÄDAS.                                         
001600*                                                                         
001700*        PROGRAMMET UPPATERAR WLSATB (WDJ1)                               
001800*                                                                         
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003200*    -COPY WY2000W3                                                       
003300     SKIP3                                                                
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
003700 77  W-CHKP-MAX                  PIC S9(5)   VALUE +800  COMP-3.          
003800 77  CHKP-ID                     PIC X(8)    VALUE 'W1121400'.            
003900 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
004000 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
004100 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
004200 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W1121400'.            
004500 77  WS-SPAR-TIREGDAT            PIC S9(7)   VALUE ZERO COMP-3.           
004600 77  WS-SPAR-IDARTNR             PIC S9(9)   VALUE ZERO COMP-3.           
004700 77  WS-SPAR-IDLEVNR             PIC X(5)    VALUE SPACE.                 
004800     SKIP3                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
005100 01  WS-KOLL-AAVV                PIC 9(4)    VALUE ZERO.                  
005200     SKIP3                                                                
005300 01  WS-AAVVD                    PIC 9(5)    VALUE ZERO.                  
005400 01  FILLER REDEFINES WS-AAVVD.                                           
005500     03  WS-AAVV                 PIC 9(4).                                
005600     03  FILLER                  PIC 9.                                   
005700     SKIP3                                                                
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900*                                                                         
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006300     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
006400     SKIP2                                                                
006500     EJECT                                                                
006600*01  -COPY WDATAREA                                                       
006700     EJECT                                                                
006800* VARIABLER TILL SUBPROGRAM W009VADD                                      
006900 01  W009VADD-AAVV               PIC S9(5)  COMP-3.                       
007000 01  W009VADD-ANTAL              PIC S9(3)  COMP-3.                       
007100     EJECT                                                                
007200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007500     SKIP3                                                                
007600 01  NYCKLAR-TILL-DLI.                                                    
007700     03  W-IDARTNR-X.                                                     
007800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007900     SKIP2                                                                
008000*    --- STATUS-KOD FRÅN IMS                                              
008100 01  STATUS-WS                   PIC XX.                                  
008200     88  SEGMENT-FINNS                       VALUE '  '.                  
008300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008400     88  BASEN-SLUT                          VALUE 'GB'.                  
008500     SKIP2                                                                
008600 01  GODK-STATUSKODER.                                                    
008700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008800     SKIP3                                                                
008900 01  SSA1                        PIC X(64).                               
009000     EJECT                                                                
009100*    --- IMS FUNKTIONSKODER                                               
009200*01  -COPY W0003                                                          
009300     EJECT                                                                
009400*    ---  DLI INPUT-OUTPUT AREA                                           
009500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009600     SKIP3                                                                
009700 01  DLI-IO-AREA.                                                         
009800     03  IO-AREA                 PIC X(250)  VALUE SPACE.                 
009900     SKIP3                                                                
010000     03  WLSATB01 REDEFINES IO-AREA.                                      
010100*        05  -COPY WDJ101                                                 
010200     EJECT                                                                
010300     03  WLSATB11 REDEFINES IO-AREA.                                      
010400*        05  -COPY WDJ111                                                 
010500     EJECT                                                                
010600 01  DLI-IO-AREA-2.                                                       
010700     03  IO-AREA-2               PIC X(110) VALUE SPACE.                  
010800     SKIP3                                                                
010900     03  WLARTC01 REDEFINES IO-AREA-2.                                    
011000*        05  -COPY WDK601                                                 
011100     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300     SKIP3                                                                
011400*01  -COPY W0009      -PRE MSG-                                           
011600     EJECT                                                                
011700*01  -COPY W0008      -PRE WDJ1-                                          
011710     05  FILLER                  PIC X.                                   
011720     EJECT                                                                
011800*01  -COPY W0008      -PRE WDK6-                                          
011900     05  FILLER                  PIC X.                                   
012000     EJECT                                                                
012100*01  -COPY W0008      -PRE WDJ12-                                         
012200     05  FILLER                  PIC X.                                   
012300     EJECT                                                                
012400 PROCEDURE DIVISION  USING MSG-PCB WDJ1-PCB WDK6-PCB WDJ12-PCB.           
012500     ENTRY 'DLITCBL' USING MSG-PCB WDJ1-PCB WDK6-PCB WDJ12-PCB.           
012600                                                                          
012700     SKIP2                                                                
012800     PERFORM A-INIT                                                       
012900                                                                          
013000     PERFORM IMS-GHN-WDJ101                                               
013100                                                                          
013200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
013300        MOVE STR-TIREGDAT TO WS-SPAR-TIREGDAT                             
013400        MOVE STR-IDARTNR  TO WS-SPAR-IDARTNR                              
013500        MOVE STR-IDLEVNR  TO WS-SPAR-IDLEVNR                              
013600        PERFORM IMS-GHNP-WDJ111                                           
013700        IF SEGMENT-FINNS                                                  
013800           IF WS-SPAR-IDLEVNR = '1002 '                                   
013900              CONTINUE                                                    
014000           ELSE                                                           
014100              PERFORM C-STAEDA-STRUKTUREN                                 
014200           END-IF                                                         
014300        ELSE                                                              
014400           PERFORM B-KOLLA-RENSNING                                       
014500        END-IF                                                            
014501                                                                          
014502        PERFORM IMS-GHN-WDJ101                                            
014503                                                                          
014510        IF W-CHKP-RAKNARE           >  W-CHKP-MAX                         
014520           PERFORM IMS-CHECKPOINT                                         
014530           MOVE ZERO                TO W-CHKP-RAKNARE                     
014531           MOVE STR-IDARTNR         TO W-IDARTNR                          
014532           PERFORM IMS-GU-WDJ101                                          
014610        END-IF                                                            
014700     END-PERFORM                                                          
014800                                                                          
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500     SKIP2                                                                
015510     PERFORM IMS-RESTART                                                  
015520                                                                          
015600     ACCEPT DAGENS-DATUM  FROM DATE                                       
015610     MOVE ZERO       TO W-CHKP-RAKNARE                                    
015700     .                                                                    
015800     EJECT                                                                
015900 B-KOLLA-RENSNING SECTION.                                                
016100******************************************************************        
016200*                                                                         
016300* OM RADER SAKNAS OCH TIREGDAT ÄLDRE ÄN 3 MÅNADER ELLER SATSENS           
016400* SORT PÅ ARTC01 SKILD FRÅN SATS RENSAS ROTEN FRÅN WDJ1                   
016500*                                                                         
016600******************************************************************        
016700     MOVE WS-SPAR-TIREGDAT TO DAT-I-TIDATUM                               
016800     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
016900                                                                          
017000     CALL WDATKONV USING DAT-KDDATFORM                                    
017100                         DAT-I-TIDATUM                                    
017200                         DAT-O-TIDATUM                                    
017300                         DAT-KDSVAR                                       
017400                                                                          
017500     IF DAT-KDSVAR-OK                                                     
017600       MOVE DAT-TIAAVVD    TO WS-AAVVD                                    
017700       MOVE WS-AAVV        TO W009VADD-AAVV                               
017800       MOVE +12            TO W009VADD-ANTAL                              
017900       CALL W009VADD USING W009VADD-AAVV                                  
018000                           W009VADD-ANTAL                                 
018100       MOVE W009VADD-AAVV  TO WS-KOLL-AAVV                                
018200                                                                          
018300       MOVE DAGENS-DATUM   TO DAT-I-TIDATUM                               
018400       MOVE 'AAMMDD'       TO DAT-KDDATFORM                               
018500                                                                          
018600       CALL WDATKONV USING DAT-KDDATFORM                                  
018700                           DAT-I-TIDATUM                                  
018800                           DAT-O-TIDATUM                                  
018900                           DAT-KDSVAR                                     
019000                                                                          
019100       MOVE DAT-TIAAVVD    TO WS-AAVVD                                    
019200       MOVE WS-AAVV        TO DAGENS-AAVV                                 
019300                                                                          
019400       MOVE WS-KOLL-AAVV   TO TMP1-YYWW                                   
019500       MOVE DAGENS-AAVV    TO TMP2-YYWW                                   
019600       PERFORM WY2000P3                                                   
019700       IF TMP1-YYWW < TMP2-YYWW                                           
019800          MOVE WS-SPAR-IDARTNR TO W-IDARTNR                               
019900          PERFORM IMS-GHU-WDJ101                                          
019910          IF SEGMENT-FINNS                                                
020000            PERFORM IMS-DLET-WDJ1                                         
020100            DISPLAY WS-SPAR-IDARTNR 'RENSAT REGDAT ÄLDRE 3 MÅN'           
020110          END-IF                                                          
020200       ELSE                                                               
020300          MOVE WS-SPAR-IDARTNR TO W-IDARTNR                               
020400          PERFORM IMS-GU-WDK601                                           
020500          IF SEGMENT-FINNS                                                
020600            IF ART-KDSORT = 'SA' OR 'TM'                                  
020700               CONTINUE                                                   
020800            ELSE                                                          
020900               PERFORM IMS-GHU-WDJ101                                     
020910               IF SEGMENT-FINNS                                           
021000                 PERFORM IMS-DLET-WDJ1                                    
021100                 DISPLAY WS-SPAR-IDARTNR 'RENSAT FEL SORT ARTREG'         
021110               END-IF                                                     
021200            END-IF                                                        
021300          END-IF                                                          
021400        END-IF                                                            
021500     ELSE                                                                 
021600       DISPLAY STR-IDARTNR 'TIREGDAT = FEL'                               
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 C-STAEDA-STRUKTUREN SECTION.                                             
022100     SKIP2                                                                
022200******************************************************************        
022300*                                                                *        
022400* KDISATS KOLLAS I STRUKTURER MED LEVNR EJ 1002. OM TISTADAT/    *        
022500* TISTODAT ÄR PASSERAT VID KDISATS N/U BLANKAS KDISATS.          *        
022600*                                                                *        
022700******************************************************************        
022800     PERFORM UNTIL SEGMENT-SAKNAS                                         
022900                                                                          
023000        IF RAD-KDISATS = 'U' OR 'N'                                       
023100           MOVE RAD-TISTODAT        TO TMP1-YYMMDD                        
023200           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
023300           PERFORM WY2000P1                                               
023400           IF TMP1-YYMMDD <= TMP2-YYMMDD                                  
023500              MOVE SPACE TO RAD-KDISATS                                   
023600              PERFORM IMS-REPL-WDJ1                                       
023700           END-IF                                                         
024800        END-IF                                                            
024900        PERFORM IMS-GHNP-WDJ111                                           
025000                                                                          
025100     END-PERFORM                                                          
025200     .                                                                    
025300     EJECT                                                                
025400* --- IMS SEKTIONER ---                                                   
025500     SKIP3                                                                
025510 IMS-RESTART           SECTION.                                           
025520                                                                          
025530     MOVE SPACE TO MSG-IO-AREA-1                                          
025540     MOVE '  ' TO GODK-STATUSKODER                                        
025550     CALL CBLTDLI USING XRST MSG-PCB                                      
025560                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
025570                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
025580     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025590     PERFORM IMS-STATUSKONTROLL                                           
025591     .                                                                    
025592                                                                          
025593     SKIP3                                                                
025594 IMS-CHECKPOINT        SECTION.                                           
025595                                                                          
025596     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
025597     MOVE '  XD' TO GODK-STATUSKODER                                      
025598     CALL CBLTDLI USING CHKP MSG-PCB                                      
025599                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
025600                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
025601     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025602     PERFORM IMS-STATUSKONTROLL                                           
025603     .                                                                    
025604     EJECT                                                                
025610 IMS-GHN-WDJ101 SECTION.                                                  
025700     MOVE 'WDJ101   ' TO SSA1                                             
025800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
025900     CALL CBLTDLI USING GHN WDJ1-PCB DLI-IO-AREA SSA1                     
026000     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
026100     PERFORM IMS-STATUSKONTROLL                                           
026200     .                                                                    
026300     SKIP3                                                                
026400 IMS-GHNP-WDJ111 SECTION.                                                 
026500     MOVE 'WDJ111   ' TO SSA1                                             
026600     MOVE '  GE' TO GODK-STATUSKODER                                      
026700     CALL CBLTDLI USING GHNP WDJ1-PCB DLI-IO-AREA SSA1                    
026800     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100     SKIP3                                                                
027200 IMS-GU-WDJ101 SECTION.                                                   
027300     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
027400        DELIMITED BY SIZE INTO SSA1                                       
027500     MOVE '  GE' TO GODK-STATUSKODER                                      
027600     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-AREA SSA1                      
027700     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000     EJECT                                                                
028010 IMS-GHU-WDJ101 SECTION.                                                  
028020     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
028030        DELIMITED BY SIZE INTO SSA1                                       
028040     MOVE '  GE' TO GODK-STATUSKODER                                      
028050     CALL CBLTDLI USING GHU WDJ12-PCB DLI-IO-AREA SSA1                    
028060     MOVE WDJ12-STATUS-CODE TO STATUS-WS                                  
028070     PERFORM IMS-STATUSKONTROLL                                           
028080     .                                                                    
028090     EJECT                                                                
028100 IMS-GU-WDK601 SECTION.                                                   
028200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
028300        DELIMITED BY SIZE INTO SSA1                                       
028400     MOVE '  GE' TO GODK-STATUSKODER                                      
028500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-2 SSA1                    
028600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028700     PERFORM IMS-STATUSKONTROLL                                           
028800     .                                                                    
028900     SKIP3                                                                
029000 IMS-REPL-WDJ1 SECTION.                                                   
029100     MOVE '  ' TO GODK-STATUSKODER                                        
029200     CALL CBLTDLI USING REPL WDJ1-PCB DLI-IO-AREA                         
029300     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
029400     PERFORM IMS-STATUSKONTROLL                                           
029410     ADD +1 TO W-CHKP-RAKNARE                                             
029500     .                                                                    
029600     SKIP3                                                                
029700 IMS-DLET-WDJ1 SECTION.                                                   
029800     MOVE '  ' TO GODK-STATUSKODER                                        
029900     CALL CBLTDLI USING DLET WDJ12-PCB DLI-IO-AREA                        
030000     MOVE WDJ12-STATUS-CODE TO STATUS-WS                                  
030100     PERFORM IMS-STATUSKONTROLL                                           
030110     ADD +1 TO W-CHKP-RAKNARE                                             
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-STATUSKONTROLL SECTION.                                              
030500     SKIP2                                                                
030600     SET STATUS-IX TO 1                                                   
030700     SEARCH GODK-STATUS                                                   
030800       AT END CALL FELLOG                                                 
030900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
031000     END-SEARCH                                                           
031100     .                                                                    
031200     EJECT                                                                
031300*    -COPY WY2000P1                                                       
031400     EJECT                                                                
031500*    -COPY WY2000P3                                                       
