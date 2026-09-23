000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2221800.                                            
000300 AUTHOR.             BODIL LINDAHL.                                       
000400 DATE-WRITTEN.       JULI 1990.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION.                                                            
000800*                                                                         
000900*            PROGRAMMET LÄSER TRANSAKTIONS-FIL MED UPPGIFT                
001000*            UPPGIFT OM UPPDATERADE LEVERANSPLANER (2201-POST)            
001100*            FÖR JUSTERADE INGÅENDE ARTIKLAR I SATSEN                     
001200*            BORTTAGES ARTIKELBEHOVET I SATSSTRUKTUREN OCH                
001300*            I ARTIKELREGISTRET.                                          
001400*            OM GIVNA FÖRUTSÄTTNINGAR ÄR UPPFYLLDA SÅ BERÄKNAS            
001500*            ARTIKELBEHOVET PÅ NYTT, OCH INFÖRES I SATS-                  
001600*            STRUKTUR OCH I ARTIKELREGISTRET                              
001700*                                                                         
001800*            -MAJ -92   2204-HÄNDELSER LÄGGS UT PÅ FIL                    
001900*                       I STÄLLET FÖR PÅ WDG3          (PAH)              
002000*                                                                         
002100*            -SEP -96   OMGJORT TILL BMP                                  
002200*                                                                         
002300*    SUBPROGRAM:                                                          
002400*            DATKORT                                                      
002500*            POSTSUM                                                      
002600*            W009VADD    ADD AV VECKOR TILL DATUM                         
002700*            WDATKONV                                                     
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003100 FILE-CONTROL.                                                            
003200                                                                          
003300*                            *** 2201- LEVPLAN UPPDATERAD      ***        
003400*                            *** INFIL                                    
003500     SELECT  W22201     ASSIGN  W22218D1.                                 
003600*                            *** 2204- HÄNDELSER               ***        
003700*                            *** UTFIL                         ***        
003800     SELECT  W22219     ASSIGN  W22218D2.                                 
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W22201                                                               
004400     RECORDING F                                                          
004500     BLOCK 0                                                              
004600     LABEL RECORD STANDARD.                                               
004700*01  -COPY W2222201 -PRE I2201-  -L.                                      
004800     SKIP3                                                                
004900 FD  W22219                                                               
005000     RECORDING F                                                          
005100     BLOCK 0                                                              
005200     LABEL RECORD STANDARD.                                               
005300                                                                          
005400 01  W22219-POST.                                                         
005500*    03  -COPY W2212204   -L.                                             
005600     SKIP3                                                                
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900 77  IDPGM                   PIC X(8)    VALUE 'W2221800'.                
006000     SKIP2                                                                
006100*    -COPY WY2000W1                                                       
006200     SKIP3                                                                
006300*    -COPY WY2000W3                                                       
006400     SKIP3                                                                
006500 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
006600 01  TABIX                   PIC S9(9)   VALUE +0    COMP SYNC.           
006700 01  TABIX2                  PIC S9(9)   VALUE +0    COMP SYNC.           
006800 01  TABIXMAX                PIC S9(9)   VALUE +999  COMP SYNC.           
006900                                                                          
007000*01  -COPY WWDCKONS                                                       
007100                                                                          
007200 01  KONSTANTER.                                                          
007300     03  JA                  PIC X       VALUE 'J'.                       
007400     03  NEJ                 PIC X       VALUE 'N'.                       
007500     03  DELEATE             PIC X       VALUE 'D'.                       
007600                                                                          
007700 01  FELTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000                                                                          
008100 01  CHKP-VAR.                                                            
008200   03  CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
008300   03  CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
008400   03  CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
008500   03  CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
008600   03  CHKP-ANT                  PIC S9(3)   VALUE +0.                    
008700   03  CHKP-MAX                  PIC S9(3)   VALUE +100.                  
008800                                                                          
008900 01  W-ARBETSFALT.                                                        
009000     03  W-DAGENS-DATUM-AAVV PIC 9(4).                                    
009100     03  W-DATUM REDEFINES W-DAGENS-DATUM-AAVV.                           
009200         05  W-DAGENS-DATUM-AA                                            
009300                             PIC 9(2).                                    
009400         05  W-DAGENS-DATUM-VV                                            
009500                             PIC 9(2).                                    
009600                                                                          
009700     03  WS-DAGENS-DATUM-KOLL.                                            
009800         05 WS-AAR           PIC 9(2).                                    
009900         05 WS-MAANAD        PIC 9(2).                                    
010000         05 WS-DAG           PIC 9(2).                                    
010100     03  WS-DAGENS-DATUM-NY REDEFINES WS-DAGENS-DATUM-KOLL                
010200                             PIC 9(6).                                    
010300     03  WS-DAGENS-DATUM     PIC S9(7) COMP-3.                            
010400                                                                          
010500     03  WS-AAVV-KOLL.                                                    
010600         05 WS-AA            PIC 9(2).                                    
010700         05 WS-VV            PIC 9(2).                                    
010800     03  WS-AAVV-NY REDEFINES WS-AAVV-KOLL                                
010900                             PIC 9(4).                                    
011000     03  WS-TIAAVV           PIC 9(4)  VALUE ZERO.                        
011100     03  WS-TISTAVECKA       PIC S9(5) COMP-3    VALUE ZERO.              
011200     03  WS-TISTOVECKA       PIC S9(5) COMP-3    VALUE ZERO.              
011300                                                                          
011400     03  W-BEORDR-VECKA      PIC S9(5)               COMP-3.              
011500     03  W-BEORDR-ANTAL      PIC S9(7)V9(2)          COMP-3.              
011600                                                                          
011700 01  WS-FLIART               PIC X          VALUE 'N'.                    
011800 01  ANTAL                   PIC S9(3)      VALUE ZERO COMP-3.            
011900 01  WS-ANTAL                PIC S9(3)      VALUE ZERO COMP-3.            
012000 01  WS-SPAR-TIBORT          PIC S9(7)      VALUE ZERO COMP-3.            
012100 01  WS-SPAR-TIBEHOV-TOTSATS PIC S9(5)      VALUE ZERO COMP-3.            
012200 01  WS-SUM-KVBEHOV-TOTSATS  PIC S9(7)V9(2) VALUE ZERO COMP-3.            
012300 01  WS-SPAR-TIBEHOV-SATS    PIC S9(5)      VALUE ZERO COMP-3.            
012400 01  WS-SPAR-KVBEHOV-SATS    PIC S9(7)V9(2) VALUE ZERO COMP-3.            
012500 01  WS-SPAR-IDARTNR-SATS    PIC S9(9)      VALUE ZERO COMP-3.            
012600                                                                          
012700****************************************** TABELL FÖR 2204-TRANS          
012800 01  TABELL.                                                              
012900     03  TAB-IDARTNR OCCURS 999    PIC S9(9) COMP-3.                      
013000                                                                          
013100****************************************** ICKE BEORDRADE AVROP           
013200*                                          I LEVPLAN                      
013300 01  AVROPS-TABELL.                                                       
013400     03  AVRTAB-IX           PIC S9(9)   VALUE ZERO  COMP SYNC.           
013500     03  AVRTAB-ANTAL        PIC S9(9)   VALUE ZERO  COMP SYNC.           
013600     03  AVRTAB-MAX          PIC S9(9)   VALUE +750  COMP SYNC.           
013700     03  AVRTAB.                                                          
013800         05  AVRTAB-ELEMENT  OCCURS 750.                                  
013900             07  AVRTAB-TIAVROP-DISP                                      
014000                             PIC S9(5)               COMP-3.              
014100             07  AVRTAB-KVAVROP                                           
014200                             PIC S9(7)               COMP-3.              
014300                                                                          
014400 77  W22201-EOF-SW           PIC X       VALUE 'N'.                       
014500     88  END-OF-W22201                   VALUE 'J'.                       
014600                                                                          
014700 01  SWITCHAR.                                                            
014800     03  SWITCH-TOT-SATSBEHOV-UPPD                                        
014900                             PIC X       VALUE 'N'.                       
015000     03  FINNS-I-TABELL      PIC X       VALUE 'N'.                       
015100     EJECT                                                                
015200 01  DYNAMISKA-SUBPROGRAM.                                                
015300     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
015400     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
015500     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
015600     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
015700     EJECT                                                                
015800 01  NYCKLAR-TILL-DLI.                                                    
015900     03  W-IDARTNR-X.                                                     
016000        05  W-IDARTNR           PIC S9(9) VALUE ZERO    COMP-3.           
016100     03  W-IDARTNR-SATS-X.                                                
016200        05  W-IDARTNR-SATS      PIC S9(9) VALUE ZERO    COMP-3.           
016300     03  W-IDARTNR-ING-X.                                                 
016400        05  W-IDARTNR-ING       PIC S9(9) VALUE ZERO    COMP-3.           
016500     03  W-TIBEHOV-SATS-X.                                                
016600        05  W-TIBEHOV-SATS      PIC S9(5) VALUE ZERO    COMP-3.           
016700     03  W-WDD901KY-X.                                                    
016710        05  W-IDARTNR-D9        PIC S9(9) VALUE ZERO    COMP-3.           
016720        05  W-IDDC-D9           PIC X(2)  VALUE SPACE.                    
016800     03  W-IDLEVNR-X.                                                     
016900        05  W-IDLEVNR           PIC X(5) VALUE '1002 '.                   
017000     03  W-WDD905KY-X.                                                    
017100         05  W-DAAVROP-AVS-X.                                             
017200             07  W-DAAVROP-AVS   PIC  9(6)    VALUE ZERO.                 
017300         05  W-TILEVDAG-X.                                                
017400             07  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.          
017500     03  W-WDJ301KY-MIN-X.                                                
017600        05  W-IDARTNR-SATS-MIN  PIC S9(9) VALUE ZERO    COMP-3.           
017700        05  W-IDARTNR-ING-MIN   PIC S9(9) VALUE ZERO    COMP-3.           
017800        05  W-TIBEHOV-MIN       PIC S9(5) VALUE ZERO    COMP-3.           
017900     03  W-WDJ301KY-MAX-X.                                                
018000        05  W-IDARTNR-SATS-MAX  PIC S9(9) VALUE ZERO    COMP-3.           
018100        05  W-IDARTNR-ING-MAX   PIC S9(9) VALUE ZERO    COMP-3.           
018200        05  W-TIBEHOV-MAX       PIC S9(5) VALUE ZERO    COMP-3.           
018300     03  W-WDJ1CSEQ-X.                                                    
018400        05  W-IDLEVNR-S         PIC X(5) VALUE SPACE.                     
018500        05  W-BELEVART-S        PIC X(30) VALUE SPACE.                    
018600        05  W-IDARTNR-S         PIC S9(9) VALUE ZERO    COMP-3.           
018700                                                                          
018800*                            *** GENERELLA SUBRUTINER                     
018900 01      SUBPROGRAM.                                                      
019000   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
019100   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
019200   03    ABEND           PIC X(8)    VALUE 'ABEND   '.                    
019300     EJECT                                                                
019400*                            *** PARAMETRAR TILL DATKORT                  
019500 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22218'.                  
019600 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
019700*01  -COPY WDATKORT                                                       
019800     EJECT                                                                
019900*                            *** PARAMETRAR TILL POSTSUM                  
020000*01  -COPY W0005       -PRE POSTSUM-.                                     
020100     EJECT                                                                
020200*01  -COPY WDATAREA                                                       
020300     EJECT                                                                
020400*                            *************************************        
020500*                            ** AREA FÖR W22201 - 2201-POST     **        
020600*                            ** LEVERANSPLAN UPPDATERAD         **        
020700*                            *************************************        
020800*01  AREA  -COPY W2222201   -PRE I2201-.                                  
020900     EJECT                                                                
021000*                            *************************************        
021100*                            ** AREA FÖR W22219 - 2204-POST     **        
021200*                            ** 2204-HÄNDELSE SKAPAS FÖR ING.ART**        
021300*                            *************************************        
021400*01  AREA  -COPY W2212204    -PRE 2204-.                                  
021500     EJECT                                                                
021600******************************************************************        
021700*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
021800*                                                                         
021900 01      IMS-WS.                                                          
022000   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
022100                                                                          
022200*                            *** STATUSKOD FRÅN IMS                       
022300   03    STATUS-WS       PIC XX.                                          
022400     88  SEGMENT-FINNS               VALUE '  '.                          
022500     88  SEGMENT-UPPLAGT             VALUE '  '.                          
022600     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
022700     88  BASEN-SLUT                  VALUE 'GB'.                          
022800     88  IMS-EJ-OK                   VALUE 'XD'.                          
022900     SKIP3                                                                
023000   03    SSA1            PIC X(64).                                       
023100   03    SSA2            PIC X(32).                                       
023200   03    SSA3            PIC X(32).                                       
023300     SKIP3                                                                
023400   03    GODK-STATUSKODER.                                                
023500     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
023600     EJECT                                                                
023700*01      -COPY W0003                                                      
023800     EJECT                                                                
023900 01      DLI-IO-AREA     PIC X(900)  VALUE SPACE.                         
024000     SKIP3                                                                
024100*01  WLARTC01 -COPY WDK601               -RED DLI-IO-AREA.                
024200     EJECT                                                                
024300*01  WLARTC11 -COPY WDK611               -RED DLI-IO-AREA.                
024400     EJECT                                                                
024500*01  WLARTC24 -COPY WDK624               -RED DLI-IO-AREA.                
024600     EJECT                                                                
024700*01  WLSATB01 -COPY WDJ101 -PRE SATB-    -RED DLI-IO-AREA.                
024800     EJECT                                                                
024900*01  WLINLB11 -COPY WDD902 -PRE INLB11-  -RED DLI-IO-AREA.                
025000     EJECT                                                                
025100*01  WLINLB23 -COPY WDD905 -PRE INLB23-  -RED DLI-IO-AREA.                
025200     EJECT                                                                
025300*01  WLINLB32 -COPY WDD907 -PRE INLB32-  -RED DLI-IO-AREA.                
025400     EJECT                                                                
025500 01      WDJ1CSEQ REDEFINES DLI-IO-AREA.                                  
025600*   03  WLSATB11   -COPY WDJ111     -PRE SATE-                            
025700*   03  WLSATB01   -COPY WDJ101     -PRE SATE-                            
025800     EJECT                                                                
025900 01      DLI-IO-AREA-2   PIC X(200)  VALUE SPACE.                         
026000*01  WLSATB11 -COPY WDJ111 -PRE SATB-    -RED DLI-IO-AREA-2.              
026100     EJECT                                                                
026200 01      DLI-IO-AREA-3   PIC X(200)  VALUE SPACE.                         
026300*01  WLSATM01 -COPY WDJ301 -PRE SATM-    -RED DLI-IO-AREA-3.              
026400     EJECT                                                                
026500 LINKAGE SECTION.                                                         
026600                                                                          
026700*01  -COPY W0009         -PRE MSG-                                        
026800     EJECT                                                                
026900*01      -COPY W0008     -PRE ARTC-                                       
027000      05 FILLER          PIC X.                                           
027100     EJECT                                                                
027200*01      -COPY W0008     -PRE SATB-                                       
027300      05 FILLER          PIC X.                                           
027400     EJECT                                                                
027500*01      -COPY W0008     -PRE INLB-                                       
027600      05 FILLER          PIC X.                                           
027700     EJECT                                                                
027800*01      -COPY W0008     -PRE SATM1-                                      
027900      05 FILLER          PIC X.                                           
028000     EJECT                                                                
028100*01      -COPY W0008     -PRE SATM2-                                      
028200      05 FILLER          PIC X.                                           
028300     EJECT                                                                
028400*01      -COPY W0008     -PRE SATE-                                       
028500      05 FILLER          PIC X.                                           
028600     EJECT                                                                
028700*01      -COPY W0008     -PRE ARTC2-                                      
028800      05 FILLER          PIC X.                                           
028900     EJECT                                                                
029000 PROCEDURE DIVISION USING  MSG-PCB  ARTC-PCB  SATB-PCB                    
029100                           INLB-PCB SATM1-PCB SATM2-PCB                   
029200                           SATE-PCB ARTC2-PCB.                            
029300 MAIN SECTION.                                                            
029400     ENTRY 'DLITCBL' USING MSG-PCB  ARTC-PCB  SATB-PCB                    
029500                           INLB-PCB SATM1-PCB SATM2-PCB                   
029600                           SATE-PCB ARTC2-PCB.                            
029700                                                                          
029800     PERFORM A-INIT                                                       
029900                                                                          
030000     PERFORM UNTIL END-OF-W22201                                          
030100       PERFORM B-BEHANDLA-2201-TRANS                                      
030200                                                                          
030300       IF CHKP-ANT > CHKP-MAX                                             
030400         PERFORM X-TAG-CHECKPOINT                                         
030500       END-IF                                                             
030600                                                                          
030700       PERFORM S01-LAES-W22201-POST                                       
030800     END-PERFORM                                                          
030900                                                                          
031000     PERFORM D-AVSLUTA                                                    
031100                                                                          
031200     MOVE ZERO TO RETURN-CODE                                             
031300     GOBACK                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 A-INIT SECTION.                                                          
031700******************************************************************        
031800*                                                                *        
031900*    ÖPPNA FILER, LÄS DATUMKORT OCH FÖRSTA POSTER PÅ TRANS-FILER *        
032000*                                                                *        
032100******************************************************************        
032200                                                                          
032300     OPEN INPUT  W22201                                                   
032400     OPEN OUTPUT W22219                                                   
032500                                                                          
032600     PERFORM IMS-RESTART                                                  
032700                                                                          
032800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
032900     MOVE D-AAR              TO W-DAGENS-DATUM-AA                         
033000                                WS-AAR                                    
033100     MOVE D-VECKA            TO W-DAGENS-DATUM-VV                         
033200     MOVE D-MAANAD           TO WS-MAANAD                                 
033300     MOVE D-DAG              TO WS-DAG                                    
033400     MOVE WS-DAGENS-DATUM-NY TO WS-DAGENS-DATUM                           
033500                                                                          
033600     MOVE 'W22218' TO POSTSUM-PROGNAMN                                    
033700                                                                          
033800     PERFORM S01-LAES-W22201-POST                                         
033900     .                                                                    
034000     EJECT                                                                
034100 B-BEHANDLA-2201-TRANS SECTION.                                           
034200******************************************************************        
034300*                                                                *        
034400*    LEVERANSPLAN UPPDATERAD                                     *        
034500*                                                                *        
034600*    UPPDATERING AV SATSBEHOV                                    *        
034700*    ÄNDRAD BEHOVSINFO DELETAS. HELT NY SATSBEHOVSINFO           *        
034800*    SKAPAS OM GIVNA FÖRUTSÄTTNINGAR UPPFYLLDA                   *        
034900*                                                                *        
035000******************************************************************        
035100                                                                          
035200     MOVE I2201-IDARTNR-SATS TO W-IDARTNR-SATS                            
035300                                WS-SPAR-IDARTNR-SATS                      
035400     PERFORM IMS-GET-SATB01                                               
035500                                                                          
035600     IF  SEGMENT-FINNS                                                    
035700                                                                          
035800       MOVE +1 TO TABIX                                                   
035900       PERFORM UNTIL TABIX > TABIXMAX                                     
036000          MOVE ZERO TO TAB-IDARTNR(TABIX)                                 
036100          ADD +1 TO TABIX                                                 
036200       END-PERFORM                                                        
036300       MOVE ZERO TO TABIX                                                 
036400                                                                          
036500       MOVE SATB-STR-TIBORT TO WS-SPAR-TIBORT                             
036600       PERFORM S03-LAES-LEVPLAN-FOR-SATS                                  
036700                                                                          
036800       MOVE LOW-VALUE  TO W-WDJ301KY-MIN-X                                
036900       MOVE HIGH-VALUE TO W-WDJ301KY-MAX-X                                
037000       MOVE W-IDARTNR-SATS TO W-IDARTNR-SATS-MIN                          
037100                              W-IDARTNR-SATS-MAX                          
037200       PERFORM IMS-GU-SATM01                                              
037300                                                                          
037400       PERFORM UNTIL SEGMENT-SAKNAS                                       
037500         MOVE NEJ TO SWITCH-TOT-SATSBEHOV-UPPD                            
037600                                                                          
037700         MOVE SATM-BEH-TIBEHOV      TO WS-SPAR-TIBEHOV-SATS               
037800         MOVE SATM-BEH-KVBEHOV      TO WS-SPAR-KVBEHOV-SATS               
037900         MOVE SATM-BEH-IDARTNR-ING  TO W-IDARTNR-ING                      
038000         PERFORM IMS-GET-ARTC01                                           
038100         IF SEGMENT-FINNS                                                 
038200           PERFORM S04-BORTTAG-SATSBEHOV                                  
038300                                                                          
038400           IF  SWITCH-TOT-SATSBEHOV-UPPD = JA                             
038500               AND I2201-FLAGGA-LPKNTL-ING = JA                           
038600               PERFORM S07-KOLLA-TABELL                                   
038700           END-IF                                                         
038800                                                                          
038900         END-IF                                                           
039000         MOVE LOW-VALUE  TO W-WDJ301KY-MIN-X                              
039100         MOVE HIGH-VALUE TO W-WDJ301KY-MAX-X                              
039200         MOVE W-IDARTNR-SATS TO W-IDARTNR-SATS-MIN                        
039300                                W-IDARTNR-SATS-MAX                        
039400         PERFORM IMS-GN-SATM01                                            
039500       END-PERFORM                                                        
039600                                                                          
039700       PERFORM IMS-GET-SATB01                                             
039800       IF SEGMENT-FINNS                                                   
039900          PERFORM IMS-GET-SATB11                                          
040000          PERFORM UNTIL SEGMENT-SAKNAS                                    
040100             MOVE SATB-RAD-IDARTNR TO W-IDARTNR-ING                       
040200             MOVE SATB-RAD-TISTODAT    TO TMP1-YYMMDD                     
040300             MOVE WS-DAGENS-DATUM      TO TMP2-YYMMDD                     
040400             MOVE SATB-RAD-TISTADAT    TO TMP3-YYMMDD                     
040500             PERFORM WY2000Q1                                             
040600             IF TMP1-YYMMDD <= TMP2-YYMMDD AND                            
040700                TMP3-YYMMDD >= TMP2-YYMMDD                                
040800                PERFORM S09-RENSA-RADEN                                   
040900             ELSE                                                         
041000                PERFORM IMS-GET-ARTC01                                    
041100                IF SEGMENT-FINNS                                          
041200                  IF ART-KDERS-UTG = 0                                    
041300                     IF  AVRTAB-ANTAL > ZERO                              
041400                        AND WS-SPAR-TIBORT  = ZERO                        
041500                        MOVE NEJ TO SWITCH-TOT-SATSBEHOV-UPPD             
041600                        PERFORM S05-SKAPA-SATSBEHOV                       
041700                        IF  SWITCH-TOT-SATSBEHOV-UPPD = JA                
041800                           AND I2201-FLAGGA-LPKNTL-ING = JA               
041900                           PERFORM S07-KOLLA-TABELL                       
042000                        END-IF                                            
042100                     END-IF                                               
042200                   END-IF                                                 
042300                END-IF                                                    
042400             END-IF                                                       
042500             PERFORM IMS-GET-SATB11                                       
042600          END-PERFORM                                                     
042700                                                                          
042800          MOVE +1 TO TABIX                                                
042900          IF TAB-IDARTNR(TABIX) > ZERO                                    
043000              PERFORM S08-UPPDAT-HAENDELSEREG                             
043100          END-IF                                                          
043200       END-IF                                                             
043300                                                                          
043400     ELSE                                                                 
043500          DISPLAY 'IDARTNR-SATS= '                                        
043600                  I2201-IDARTNR-SATS                                      
043700                 ' I 2201 FINNS EJ I SATSREG'                             
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 D-AVSLUTA SECTION.                                                       
044200                                                                          
044300     CLOSE  W22201                                                        
044400            W22219                                                        
044500                                                                          
044600     MOVE 'S' TO POSTSUM-OPKOD                                            
044700     CALL POSTSUM USING POSTSUM-PARM                                      
044800     .                                                                    
044900     EJECT                                                                
045000 S01-LAES-W22201-POST SECTION.                                            
045100                                                                          
045200     READ W22201 INTO I2201-AREA                                          
045300     AT END                                                               
045400        SET END-OF-W22201 TO TRUE                                         
045500     NOT AT END                                                           
045600         MOVE 'W22201'       TO POSTSUM-FDNAMN                            
045700         MOVE 'W22218D1'     TO POSTSUM-DDNAMN2                           
045800         MOVE I2201-IDHTYP   TO POSTSUM-TRANSTYP                          
045900         CALL POSTSUM USING POSTSUM-PARM                                  
046000     END-READ                                                             
046100     .                                                                    
046200     EJECT                                                                
046300 S03-LAES-LEVPLAN-FOR-SATS SECTION.                                       
046400******************************************************************        
046500*                                                                *        
046600*    SATSENS LEVERANSPLAN LÄSES. ALLA ICKE BEORDRADE AVROP       *        
046700*    MED AVROPSKOD=2 FRÅN GÄLLANDE PLAN SPARAS I INTERN-         *        
046800*    TABELL.                                                     *        
046900*                                                                *        
047000******************************************************************        
047100     MOVE ZERO TO AVRTAB-ANTAL                                            
047110     MOVE W-IDARTNR-SATS TO W-IDARTNR-D9                                  
047120     MOVE WC-CDC-SE      TO W-IDDC-D9                                     
047200     MOVE '1002 '        TO W-IDLEVNR                                     
047300     PERFORM IMS-GET-INLB11                                               
047400                                                                          
047500     IF  SEGMENT-FINNS                                                    
047600     AND AVRTAB-ANTAL NOT > AVRTAB-MAX                                    
047700         PERFORM IMS-GET-INLB23                                           
047800                                                                          
047900         PERFORM UNTIL SEGMENT-SAKNAS                                     
048000             IF  INLB23-KDAVROP = 2                                       
048100                MOVE INLB23-DAAVROP-AVS TO W-DAAVROP-AVS                  
048200                MOVE INLB23-TILEVDAG    TO W-TILEVDAG                     
048300* ? W-TILEVDAG ?                                                          
048400                PERFORM IMS-GET-INLB32                                    
048500                                                                          
048600                IF SEGMENT-SAKNAS                                         
048700                   ADD 1 TO AVRTAB-ANTAL                                  
048800                   MOVE INLB23-TIAVRDAT-DISP TO DAT-I-TIDATUM             
048900                   MOVE 'AAMMDD'             TO DAT-KDDATFORM             
049000                   CALL WDATKONV USING          DAT-KDDATFORM             
049100                                                DAT-I-TIDATUM             
049200                                                DAT-O-TIDATUM             
049300                                                DAT-KDSVAR                
049400                   IF DAT-KDSVAR-FEL                                      
049500                     MOVE 'FEL VID ANROP TILL DATKONV 2'                  
049600                          TO FELTEXT-STR                                  
049700                     CALL FELLOG                                          
049800                   ELSE                                                   
049900                     MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                 
050000                     MOVE WS-TIAAVV                                       
050100                          TO AVRTAB-TIAVROP-DISP (AVRTAB-ANTAL)           
050200                   END-IF                                                 
050300                   MOVE INLB23-KVAVROP                                    
050400                        TO AVRTAB-KVAVROP (AVRTAB-ANTAL)                  
050500                END-IF                                                    
050600             END-IF                                                       
050700             PERFORM IMS-GET-INLB23                                       
050800         END-PERFORM                                                      
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 S04-BORTTAG-SATSBEHOV SECTION.                                           
051300******************************************************************        
051400*                                                                *        
051500*    UPPDATERING AV TOTALT SATSBEHOV I ARTIKEL-REG,              *        
051600*    SAMT DELEATE AV SATSBEHOV I SATSSTRUKTUREN FÖR AKTUELL               
051700*    ARTIKEL                                                     *        
051800*                                                                *        
051900******************************************************************        
052000                                                                          
052100     MOVE WS-SPAR-TIBEHOV-SATS TO W-TIBEHOV-SATS                          
052200     PERFORM IMS-GET-ARTC24-KVAL                                          
052300     IF  SEGMENT-FINNS                                                    
052400       SUBTRACT WS-SPAR-KVBEHOV-SATS                                      
052500                       FROM SATS-KVBEHOV-TOTSATS                          
052600       MOVE JA TO SWITCH-TOT-SATSBEHOV-UPPD                               
052700       IF  SATS-KVBEHOV-TOTSATS > ZERO                                    
052800           PERFORM IMS-REPLACE-ARTC                                       
052900           ADD +1              TO CHKP-ANT                                
053000       ELSE                                                               
053100           PERFORM IMS-DELETE-ARTC                                        
053200           ADD +1              TO CHKP-ANT                                
053300       END-IF                                                             
053400     END-IF                                                               
053500                                                                          
053600     MOVE WS-SPAR-TIBEHOV-SATS TO W-TIBEHOV-MIN                           
053700                                  W-TIBEHOV-MAX                           
053800     MOVE W-IDARTNR-ING        TO W-IDARTNR-ING-MIN                       
053900                                  W-IDARTNR-ING-MAX                       
054000     MOVE W-IDARTNR-SATS       TO W-IDARTNR-SATS-MIN                      
054100                                  W-IDARTNR-SATS-MAX                      
054200     PERFORM IMS-GET-SATM01                                               
054300                                                                          
054400     IF  SEGMENT-FINNS                                                    
054500       PERFORM IMS-DELETE-SATM                                            
054600       ADD +1                  TO CHKP-ANT                                
054700     END-IF                                                               
054800                                                                          
054900     .                                                                    
055000     EJECT                                                                
055100 S05-SKAPA-SATSBEHOV SECTION.                                             
055200******************************************************************        
055300*                                                                *        
055400*    NYTT SATSBEHOV SKAPAS MED HJÄLP AV AVROPS-TABELLEN          *        
055500*                                                                *        
055600******************************************************************        
055700                                                                          
055800     MOVE WS-SPAR-IDARTNR-SATS TO W-IDARTNR                               
055900     PERFORM IMS-GET-ARTC11                                               
056000     MOVE CLAG-KVVECKOR-LT TO WS-ANTAL                                    
056100                                                                          
056200     MOVE 1 TO AVRTAB-IX                                                  
056300                                                                          
056400     PERFORM UNTIL AVRTAB-IX > AVRTAB-ANTAL                               
056500       MOVE AVRTAB-TIAVROP-DISP (AVRTAB-IX) TO W-BEORDR-VECKA             
056600       COMPUTE ANTAL = WS-ANTAL * -1                                      
056700       CALL W009VADD USING W-BEORDR-VECKA ANTAL                           
056800       MULTIPLY AVRTAB-KVAVROP (AVRTAB-IX) BY SATB-RAD-REANTPSA           
056900                             GIVING W-BEORDR-ANTAL ROUNDED                
057000                                                                          
057100       MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                            
057200       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
057300       CALL WDATKONV USING DAT-KDDATFORM                                  
057400                           DAT-I-TIDATUM                                  
057500                           DAT-O-TIDATUM                                  
057600                           DAT-KDSVAR                                     
057700       IF DAT-KDSVAR-OK                                                   
057800          MOVE DAT-TIAA-VECKA   TO WS-AA                                  
057900          MOVE DAT-TIVV         TO WS-VV                                  
058000          MOVE WS-AAVV-NY       TO WS-TISTAVECKA                          
058100                                                                          
058200          IF SATB-RAD-TISTODAT = +999999                                  
058300             MOVE WS-TISTAVECKA    TO TMP1-YYWW                           
058400             MOVE W-BEORDR-VECKA   TO TMP2-YYWW                           
058500             PERFORM WY2000P3                                             
058600             IF TMP1-YYWW NOT > TMP2-YYWW                                 
058700                 PERFORM S05A-UPPDATERA-SATSBEHOV                         
058800             END-IF                                                       
058900          ELSE                                                            
059000             MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                      
059100             MOVE 'AAMMDD'          TO DAT-KDDATFORM                      
059200             CALL WDATKONV USING DAT-KDDATFORM                            
059300                                 DAT-I-TIDATUM                            
059400                                 DAT-O-TIDATUM                            
059500                                 DAT-KDSVAR                               
059600             IF DAT-KDSVAR-OK                                             
059700               MOVE DAT-TIAA-VECKA   TO WS-AA                             
059800               MOVE DAT-TIVV         TO WS-VV                             
059900               MOVE WS-AAVV-NY       TO WS-TISTOVECKA                     
060000                                                                          
060100               MOVE WS-TISTAVECKA    TO TMP1-YYWW                         
060200               MOVE W-BEORDR-VECKA   TO TMP2-YYWW                         
060300               MOVE WS-TISTOVECKA    TO TMP3-YYWW                         
060400               PERFORM WY2000Q3                                           
060500                                                                          
060600               IF (    TMP1-YYWW NOT > TMP2-YYWW                          
060700                   AND TMP3-YYWW > TMP2-YYWW )                            
060800                 PERFORM S05A-UPPDATERA-SATSBEHOV                         
060900               END-IF                                                     
061000             END-IF                                                       
061100          END-IF                                                          
061200        END-IF                                                            
061300        ADD 1 TO AVRTAB-IX                                                
061400     END-PERFORM                                                          
061500     .                                                                    
061600     EJECT                                                                
061700 S05A-UPPDATERA-SATSBEHOV SECTION.                                        
061800                                                                          
061900     MOVE WS-SPAR-IDARTNR-SATS TO W-IDARTNR-SATS-MIN                      
062000                                  W-IDARTNR-SATS-MAX                      
062100     MOVE SATB-RAD-IDARTNR     TO W-IDARTNR-ING-MIN                       
062200                                  W-IDARTNR-ING-MAX                       
062300     MOVE W-BEORDR-VECKA       TO W-TIBEHOV-SATS                          
062400                                  W-TIBEHOV-MIN                           
062500                                  W-TIBEHOV-MAX                           
062600                                                                          
062700***                                                                       
062800* 960904 (FRONTEC) TAGIT BORT LÄSNING SAMT REPLACE                        
062900* AV SATM PGA ATT DETTA ALDRIG INTRÄFFAR (ENL. BODIL L.)                  
063000* LÄSNING OCH REPLACE TILLAGT IGEN - INTRÄFFAR NÄR                        
063100* EN ARTIKEL FÖREKOMMER FLERA GÅNGER I SATSEN                             
063200***                                                                       
063300     PERFORM IMS-GET-SATM01                                               
063400     IF SEGMENT-SAKNAS                                                    
063500                                                                          
063600     MOVE WS-SPAR-IDARTNR-SATS TO SATM-BEH-IDARTNR                        
063700     MOVE W-IDARTNR-ING-MIN       TO SATM-BEH-IDARTNR-ING                 
063800     MOVE W-BEORDR-VECKA          TO SATM-BEH-TIBEHOV                     
063900     MOVE W-BEORDR-ANTAL          TO SATM-BEH-KVBEHOV                     
064000     PERFORM IMS-INSERT-SATM                                              
064100     ADD +1                       TO CHKP-ANT                             
064200                                                                          
064300     ELSE                                                                 
064400       ADD W-BEORDR-ANTAL TO SATM-BEH-KVBEHOV                             
064500       PERFORM IMS-REPLACE-SATM                                           
064600       ADD +1 TO CHKP-ANT                                                 
064700     END-IF                                                               
064800                                                                          
064900     IF  SEGMENT-UPPLAGT                                                  
065000       MOVE W-BEORDR-VECKA     TO W-TIBEHOV-SATS                          
065100       PERFORM IMS-GET-ARTC24-KVAL                                        
065200                                                                          
065300       IF SEGMENT-FINNS                                                   
065400          ADD W-BEORDR-ANTAL TO SATS-KVBEHOV-TOTSATS                      
065500          PERFORM IMS-REPLACE-ARTC                                        
065600          ADD +1                    TO CHKP-ANT                           
065700       ELSE                                                               
065800          MOVE W-BEORDR-ANTAL       TO SATS-KVBEHOV-TOTSATS               
065900          MOVE W-BEORDR-VECKA       TO SATS-TIBEHOV-SATS                  
066000          PERFORM IMS-INSERT-ARTC24                                       
066100          ADD +1                    TO CHKP-ANT                           
066200        END-IF                                                            
066300        MOVE JA TO SWITCH-TOT-SATSBEHOV-UPPD                              
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 S07-KOLLA-TABELL SECTION.                                                
066800******************************************************************        
066900*                                                                *        
067000*    KONTROLL OM SATSBEHOV REDAN UPPDATERAT FÖR DEN INGÅENDE     *        
067100*    ARTIKEL                                                     *        
067200*                                                                *        
067300******************************************************************        
067400     MOVE NEJ TO FINNS-I-TABELL                                           
067500     MOVE +1 TO TABIX2                                                    
067600     PERFORM UNTIL TABIX2 > TABIXMAX                                      
067700        IF TAB-IDARTNR(TABIX2) = W-IDARTNR-ING                            
067800           MOVE TABIXMAX TO TABIX2                                        
067900           MOVE JA TO FINNS-I-TABELL                                      
068000        ELSE                                                              
068100           CONTINUE                                                       
068200        END-IF                                                            
068300        ADD +1 TO TABIX2                                                  
068400     END-PERFORM                                                          
068500                                                                          
068600     IF FINNS-I-TABELL = NEJ                                              
068700        ADD +1 TO TABIX                                                   
068800        MOVE W-IDARTNR-ING TO TAB-IDARTNR(TABIX)                          
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 S08-UPPDAT-HAENDELSEREG SECTION.                                         
069300******************************************************************        
069400*                                                                *        
069500*    2204-TRANS SKAPAS FÖR INGAENDE ARTIKEL FRÅN 2201-FILEN      *        
069600*                                                                *        
069700******************************************************************        
069800     MOVE +1 TO TABIX                                                     
069900     PERFORM UNTIL TABIX > TABIXMAX                                       
070000      IF TAB-IDARTNR(TABIX) > 0                                           
070100        MOVE TAB-IDARTNR(TABIX) TO 2204-IDARTNR                           
070200        MOVE 52                 TO 2204-KDLPORS                           
070300        MOVE '2204'             TO 2204-IDHTYP                            
070400        PERFORM S10-SKRIV-W22219                                          
070500       END-IF                                                             
070600       ADD +1 TO TABIX                                                    
070700     END-PERFORM                                                          
070800     .                                                                    
070900     EJECT                                                                
071000 S09-RENSA-RADEN SECTION.                                                 
071100******************************************************************        
071200*                                                                *        
071300*    ARTIKELRADEN HAR ALDRIG VARIT GÄLLANDE OCH DELEATAS         *        
071400*    FLIART PÅ ARTIKELREGISTRET ARTC01 KONTROLLERAS              *        
071500*                                                                *        
071600******************************************************************        
071700     PERFORM IMS-DELETE-SATB                                              
071800     ADD +1              TO CHKP-ANT                                      
071900                                                                          
072000     MOVE W-IDARTNR-ING  TO W-IDARTNR-S                                   
072100     MOVE SPACE          TO W-IDLEVNR-S                                   
072200     MOVE SPACE          TO W-BELEVART-S                                  
072300     MOVE '1002 '        TO W-IDLEVNR                                     
072400     MOVE NEJ            TO WS-FLIART                                     
072500                                                                          
072600     PERFORM IMS-GET-SATE-CSEQ-FIRST                                      
072700     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
072800        IF SATE-STR-TIBORT = ZERO                                         
072900           IF SATE-STR-IDARTNR < 100000000                                
073000              MOVE SATE-RAD-TISTODAT    TO TMP1-YYMMDD                    
073100              MOVE WS-DAGENS-DATUM      TO TMP2-YYMMDD                    
073200              PERFORM WY2000P1                                            
073300              IF TMP1-YYMMDD > TMP2-YYMMDD                                
073400                 MOVE JA TO WS-FLIART                                     
073500              END-IF                                                      
073600           END-IF                                                         
073700        END-IF                                                            
073800        PERFORM IMS-GET-SATE-CSEQ-NEXT                                    
073900     END-PERFORM                                                          
074000                                                                          
074100     PERFORM IMS-GET-ARTC01                                               
074200     IF SEGMENT-FINNS                                                     
074300        IF WS-FLIART NOT = ART-FLIART                                     
074400           MOVE WS-FLIART TO ART-FLIART                                   
074500           PERFORM IMS-REPLACE-ARTC                                       
074600           ADD +1         TO CHKP-ANT                                     
074700        END-IF                                                            
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 S10-SKRIV-W22219 SECTION.                                                
075200                                                                          
075300*****************************************************************         
075400*    SKRIV 2204-TRANSAR TILL W221P022  (HOPPA ÖVER WDG3 !)      *         
075500*****************************************************************         
075600                                                                          
075700     WRITE W22219-POST         FROM  2204-AREA                            
075800     MOVE 'W22219'             TO POSTSUM-FDNAMN                          
075900     MOVE 'W22218D2'           TO POSTSUM-DDNAMN2                         
076000     MOVE '2204'               TO POSTSUM-TRANSTYP                        
076100     CALL POSTSUM USING POSTSUM-PARM                                      
076200     .                                                                    
076300     EJECT                                                                
076400 X-TAG-CHECKPOINT   SECTION.                                              
076500                                                                          
076600     PERFORM IMS-CHECKPOINT                                               
076700     MOVE ZERO TO CHKP-ANT                                                
076800     .                                                                    
076900     EJECT                                                                
077000* IMS SEKTIONER                                                           
077100     SKIP3                                                                
077200 IMS-GET-SATB01 SECTION.                                                  
077300     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
077400            DELIMITED BY SIZE INTO SSA1                                   
077500     MOVE '  GE' TO GODK-STATUSKODER                                      
077600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
077700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
077800     PERFORM IMS-STATUSKONTROLL                                           
077900     .                                                                    
078000     SKIP3                                                                
078100 IMS-GET-SATB11 SECTION.                                                  
078200     MOVE 'WLSATB11 ' TO SSA1                                             
078300     MOVE '  GE' TO GODK-STATUSKODER                                      
078400     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-2 SSA1                  
078500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
078600     PERFORM IMS-STATUSKONTROLL                                           
078700     .                                                                    
078800     EJECT                                                                
078900 IMS-GU-SATM01 SECTION.                                                   
079000     STRING 'WLSATM01(WDJ301KY=>' W-WDJ301KY-MIN-X                        
079100                   '&WDJ301KY=<' W-WDJ301KY-MAX-X ')'                     
079200            DELIMITED BY SIZE INTO SSA1                                   
079300     MOVE '  GE' TO GODK-STATUSKODER                                      
079400     CALL CBLTDLI USING GHU SATM1-PCB DLI-IO-AREA-3 SSA1                  
079500     MOVE SATM1-STATUS-CODE TO STATUS-WS                                  
079600     PERFORM IMS-STATUSKONTROLL                                           
079700     .                                                                    
079800     SKIP3                                                                
079900 IMS-GN-SATM01 SECTION.                                                   
080000     STRING 'WLSATM01(WDJ301KY=>' W-WDJ301KY-MIN-X                        
080100                   '&WDJ301KY=<' W-WDJ301KY-MAX-X ')'                     
080200            DELIMITED BY SIZE INTO SSA1                                   
080300     MOVE '  GE' TO GODK-STATUSKODER                                      
080400     CALL CBLTDLI USING GHN SATM1-PCB DLI-IO-AREA-3 SSA1                  
080500     MOVE SATM1-STATUS-CODE TO STATUS-WS                                  
080600     PERFORM IMS-STATUSKONTROLL                                           
080700     .                                                                    
080800     SKIP3                                                                
080900 IMS-GET-SATM01 SECTION.                                                  
081000     STRING 'WLSATM01(WDJ301KY=>' W-WDJ301KY-MIN-X                        
081100                   '&WDJ301KY=<' W-WDJ301KY-MAX-X ')'                     
081200            DELIMITED BY SIZE INTO SSA1                                   
081300     MOVE '  GE' TO GODK-STATUSKODER                                      
081400     CALL CBLTDLI USING GHU SATM2-PCB DLI-IO-AREA-3 SSA1                  
081500     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     EJECT                                                                
081900 IMS-GET-SATE-CSEQ-FIRST SECTION.                                         
082000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
082100            DELIMITED BY SIZE INTO SSA1                                   
082200     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
082300            DELIMITED BY SIZE INTO SSA2                                   
082400     MOVE '  GE' TO GODK-STATUSKODER                                      
082500     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA SSA1 SSA2                 
082600     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
082700     PERFORM IMS-STATUSKONTROLL                                           
082800     .                                                                    
082900     SKIP3                                                                
083000 IMS-GET-SATE-CSEQ-NEXT SECTION.                                          
083100     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
083200            DELIMITED BY SIZE INTO SSA1                                   
083300     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
083400            DELIMITED BY SIZE INTO SSA2                                   
083500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
083600     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA SSA1 SSA2                 
083700     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
083800     PERFORM IMS-STATUSKONTROLL                                           
083900     .                                                                    
084000     EJECT                                                                
084100 IMS-GET-ARTC01 SECTION.                                                  
084200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ING-X ')'                     
084300            DELIMITED BY SIZE INTO SSA1                                   
084400     MOVE '  GE' TO GODK-STATUSKODER                                      
084500     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
084600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
084700     PERFORM IMS-STATUSKONTROLL                                           
084800     .                                                                    
084900     SKIP3                                                                
085000 IMS-GET-ARTC24-KVAL SECTION.                                             
085100     MOVE 'WLARTC11 ' TO SSA1                                             
085200     STRING 'WLARTC24*F(TIBEHOV  =' W-TIBEHOV-SATS-X ')'                  
085300            DELIMITED BY SIZE INTO SSA2                                   
085400     MOVE '  GE' TO GODK-STATUSKODER                                      
085500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1 SSA2               
085600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
085700     PERFORM IMS-STATUSKONTROLL                                           
085800     .                                                                    
085900     EJECT                                                                
086000 IMS-GET-INLB11 SECTION.                                                  
086100     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
086200            DELIMITED BY SIZE INTO SSA1                                   
086300     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
086400            DELIMITED BY SIZE INTO SSA2                                   
086500     MOVE '  GE' TO GODK-STATUSKODER                                      
086600     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
086700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
086800     PERFORM IMS-STATUSKONTROLL                                           
086900     SKIP3                                                                
087000     .                                                                    
087100 IMS-GET-INLB23 SECTION.                                                  
087200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
087300            DELIMITED BY SIZE INTO SSA1                                   
087400     MOVE 'WLINLB23 ' TO SSA2                                             
087500     MOVE '  GE' TO GODK-STATUSKODER                                      
087600     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
087700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000     SKIP3                                                                
088100 IMS-GET-INLB32 SECTION.                                                  
088200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
088300            DELIMITED BY SIZE INTO SSA1                                   
088400     STRING 'WLINLB23(WDD905KY =' W-WDD905KY-X ')'                        
088500            DELIMITED BY SIZE INTO SSA2                                   
088600     MOVE 'WLINLB32 ' TO SSA3                                             
088700     MOVE '  GE' TO GODK-STATUSKODER                                      
088800     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
088900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
089000     PERFORM IMS-STATUSKONTROLL                                           
089100     EJECT                                                                
089200     .                                                                    
089300 IMS-INSERT-ARTC24 SECTION.                                               
089400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ING-X ')'                     
089500            DELIMITED BY SIZE INTO SSA1                                   
089600     MOVE 'WLARTC11 ' TO SSA2                                             
089700     MOVE 'WLARTC24 ' TO SSA3                                             
089800     MOVE '  II' TO GODK-STATUSKODER                                      
089900     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
090000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
090100     PERFORM IMS-STATUSKONTROLL                                           
090200     .                                                                    
090300     SKIP3                                                                
090400 IMS-INSERT-SATM SECTION.                                                 
090500     MOVE 'WLSATM01 ' TO SSA1                                             
090600     MOVE '  ' TO GODK-STATUSKODER                                        
090700     CALL CBLTDLI USING ISRT SATM2-PCB DLI-IO-AREA-3 SSA1                 
090800     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
090900     PERFORM IMS-STATUSKONTROLL                                           
091000     .                                                                    
091100     EJECT                                                                
091200 IMS-REPLACE-ARTC SECTION.                                                
091300     MOVE '  '   TO GODK-STATUSKODER                                      
091400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
091500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800     SKIP3                                                                
091900 IMS-REPLACE-SATM SECTION.                                                
092000     MOVE '  '   TO GODK-STATUSKODER                                      
092100     CALL CBLTDLI USING REPL SATM2-PCB DLI-IO-AREA-3                      
092200     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
092300     PERFORM IMS-STATUSKONTROLL                                           
092400     .                                                                    
092500     SKIP3                                                                
092600 IMS-DELETE-ARTC SECTION.                                                 
092700     MOVE '  '   TO GODK-STATUSKODER                                      
092800     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA                         
092900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
093000     PERFORM IMS-STATUSKONTROLL                                           
093100     .                                                                    
093200     EJECT                                                                
093300 IMS-DELETE-SATB SECTION.                                                 
093400     MOVE '  '   TO GODK-STATUSKODER                                      
093500     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA-2                       
093600     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     SKIP3                                                                
094000 IMS-DELETE-SATM SECTION.                                                 
094100     MOVE '  '   TO GODK-STATUSKODER                                      
094200     CALL CBLTDLI USING DLET SATM2-PCB DLI-IO-AREA-3                      
094300     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
094400     PERFORM IMS-STATUSKONTROLL                                           
094500     .                                                                    
094600     SKIP3                                                                
094700 IMS-GET-ARTC11 SECTION.                                                  
094800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
094900            DELIMITED BY SIZE INTO SSA1                                   
095000     MOVE 'WLARTC11 ' TO SSA2                                             
095100     MOVE '  ' TO GODK-STATUSKODER                                        
095200     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA SSA1 SSA2                
095300     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
095400     PERFORM IMS-STATUSKONTROLL                                           
095500     .                                                                    
095600     EJECT                                                                
095700 IMS-CHECKPOINT SECTION.                                                  
095800     SKIP2                                                                
095900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
096000     MOVE '  XD' TO GODK-STATUSKODER                                      
096100     CALL CBLTDLI USING CHKP MSG-PCB                                      
096200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
096300                        CHKP-AREA-LENGTH CHKP-AREA                        
096400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096500     PERFORM IMS-STATUSKONTROLL                                           
096600                                                                          
096700     IF IMS-EJ-OK                                                         
096800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
096900       DISPLAY FELTEXT                                                    
097000       CALL FELLOG                                                        
097100     END-IF                                                               
097200     .                                                                    
097300     EJECT                                                                
097400 IMS-RESTART SECTION.                                                     
097500     SKIP2                                                                
097600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
097700     MOVE '  ' TO GODK-STATUSKODER                                        
097800     CALL CBLTDLI USING XRST MSG-PCB                                      
097900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
098000                        CHKP-AREA-LENGTH CHKP-AREA                        
098100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
098200     PERFORM IMS-STATUSKONTROLL                                           
098300     .                                                                    
098400     SKIP3                                                                
098500 IMS-STATUSKONTROLL SECTION.                                              
098600     SET STATUS-IX TO 1                                                   
098700     SEARCH GODK-STATUS AT END CALL FELLOG                                
098800        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                 
098900     END-SEARCH                                                           
099000     .                                                                    
099100     EJECT                                                                
099200*    -COPY WY2000P1                                                       
099300     EJECT                                                                
099400*    -COPY WY2000P3                                                       
099500     EJECT                                                                
099600*    -COPY WY2000Q1                                                       
099700     EJECT                                                                
099800*    -COPY WY2000Q3                                                       
