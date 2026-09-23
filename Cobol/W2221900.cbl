000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2221900.                                            
000300 AUTHOR.             BODIL LINDAHL.                                       
000400 DATE-WRITTEN.       JULI 1990.                                           
000500                                                                          
000600     REMARKS.                                                             
000700*    FUNCTION.                                                            
000800*                                                                         
000900*            PROGRAMMET LÄSER TRANSAKTIONS-FIL MED UPPGIFT OM             
001000*            UPPDATERADE SATSSTRUKTURER (2234-POST)                       
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
002100*            -SEP -96   OMGJORT TILL BMP.                                 
002200*    SUBPROGRAM:                                                          
002300*            DATKORT                                                      
002400*            POSTSUM                                                      
002500*            W009VADD    ADD AV VECKOR TILL DATUM                         
002600*            WDATKONV                                                     
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900 INPUT-OUTPUT SECTION.                                                    
003000 FILE-CONTROL.                                                            
003100                                                                          
003200*                            *** 2234- SATSSTRUKT. UPPDATERAD  ***        
003300*                            *** INFIL                         ***        
003400     SELECT  W22215     ASSIGN  W22219D1.                                 
003500*                            *** 2204- HÄNDELSER               ***        
003600*                            *** UTFIL                         ***        
003700     SELECT  W22220     ASSIGN  W22219D2.                                 
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W22215                                                               
004300     RECORDING F                                                          
004400     BLOCK 0                                                              
004500     LABEL RECORD STANDARD.                                               
004600*01  -COPY W2222203              -L.                                      
004700     SKIP3                                                                
004800 FD  W22220                                                               
004900     RECORDING F                                                          
005000     BLOCK 0                                                              
005100     LABEL RECORD STANDARD.                                               
005200                                                                          
005300 01  W22220-POST.                                                         
005400*    03  -COPY W2212204   -L.                                             
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700     SKIP2                                                                
005800*    -COPY WY2000W1                                                       
005900     SKIP3                                                                
006000*    -COPY WY2000W3                                                       
006100     SKIP3                                                                
006200 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
006300 01  TABIX                   PIC S9(9)   VALUE +0    COMP-3.              
006400 01  TABIX2                  PIC S9(9)   VALUE +0    COMP-3.              
006500 01  TABIXMAX                PIC S9(9)   VALUE +999  COMP-3.              
006600                                                                          
006610*01  -COPY WWDCKONS                                                       
006620                                                                          
006700 01  KONSTANTER.                                                          
006800     03  JA                  PIC X       VALUE 'J'.                       
006900     03  NEJ                 PIC X       VALUE 'N'.                       
007000     03  DELEATE             PIC X       VALUE 'D'.                       
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500                                                                          
007600 01  CHKP-VAR.                                                            
007700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
007800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
007900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
008000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
008100 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
008200 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
008300                                                                          
008400 77  W22215-EOF-SW           PIC X       VALUE 'N'.                       
008500     88  END-OF-W22215                   VALUE 'J'.                       
008600                                                                          
008700 01  W-ARBETSFALT.                                                        
008800     03  W-DAGENS-DATUM-AAVV PIC 9(4).                                    
008900     03  W-DATUM REDEFINES W-DAGENS-DATUM-AAVV.                           
009000         05  W-DAGENS-DATUM-AA                                            
009100                             PIC 9(2).                                    
009200         05  W-DAGENS-DATUM-VV                                            
009300                             PIC 9(2).                                    
009400                                                                          
009500     03  WS-DAGENS-DATUM-KOLL.                                            
009600         05 WS-AAR           PIC 9(2).                                    
009700         05 WS-MAANAD        PIC 9(2).                                    
009800         05 WS-DAG           PIC 9(2).                                    
009900     03  WS-DAGENS-DATUM-NY REDEFINES WS-DAGENS-DATUM-KOLL                
010000                             PIC 9(6).                                    
010100     03  WS-DAGENS-DATUM     PIC S9(7) COMP-3.                            
010200                                                                          
010300     03  WS-AAVV-KOLL.                                                    
010400         05 WS-AA            PIC 9(2).                                    
010500         05 WS-VV            PIC 9(2).                                    
010600     03  WS-AAVV-NY REDEFINES WS-AAVV-KOLL                                
010700                             PIC 9(4).                                    
010800     03  WS-TIAAVV           PIC 9(4)  VALUE ZERO.                        
010900     03  WS-TISTAVECKA       PIC S9(5) COMP-3    VALUE ZERO.              
011000     03  WS-TISTOVECKA       PIC S9(5) COMP-3    VALUE ZERO.              
011100                                                                          
011200     03  W-BEORDR-VECKA      PIC S9(5)               COMP-3.              
011300     03  W-BEORDR-ANTAL      PIC S9(7)V9(2)          COMP-3.              
011400                                                                          
011500 01  WS-FLIART               PIC X          VALUE 'N'.                    
011600 01  ANTAL                   PIC S9(3)      VALUE ZERO COMP-3.            
011700 01  WS-ANTAL                PIC S9(3)      VALUE ZERO COMP-3.            
011800 01  WS-SPAR-TIBORT          PIC S9(7)      VALUE ZERO COMP-3.            
011900 01  WS-SPAR-TIBEHOV-TOTSATS PIC S9(5)      VALUE ZERO COMP-3.            
012000 01  WS-SUM-KVBEHOV-TOTSATS  PIC S9(7)V9(2) VALUE ZERO COMP-3.            
012100 01  WS-SPAR-TIBEHOV-SATS    PIC S9(5)      VALUE ZERO COMP-3.            
012200 01  WS-SPAR-KVBEHOV-SATS    PIC S9(7)V9(2) VALUE ZERO COMP-3.            
012300 01  WS-SPAR-IDARTNR-SATS    PIC S9(9)      VALUE ZERO COMP-3.            
012400                                                                          
012500****************************************** TABELL FÖR 2204-TRANS          
012600 01  TABELL.                                                              
012700     03  TAB-IDARTNR OCCURS 999    PIC S9(9) COMP-3.                      
012800                                                                          
012900****************************************** ICKE BEORDRADE AVROP           
013000*                                          I LEVPLAN                      
013100 01  AVROPS-TABELL.                                                       
013200     03  AVRTAB-IX           PIC S9(9)   VALUE ZERO  COMP SYNC.           
013300     03  AVRTAB-ANTAL        PIC S9(9)   VALUE ZERO  COMP SYNC.           
013400     03  AVRTAB-MAX          PIC S9(9)   VALUE +750  COMP SYNC.           
013500     03  AVRTAB.                                                          
013600         05  AVRTAB-ELEMENT  OCCURS 750.                                  
013700             07  AVRTAB-TIAVROP-DISP                                      
013800                             PIC S9(5)               COMP-3.              
013900             07  AVRTAB-KVAVROP                                           
014000                             PIC S9(7)               COMP-3.              
014100 01  SWITCHAR.                                                            
014200     03  SWITCH-TOT-SATSBEHOV-UPPD                                        
014300                             PIC X       VALUE 'N'.                       
014400     03  FINNS-I-TABELL      PIC X       VALUE 'N'.                       
014500     EJECT                                                                
014600 01  DYNAMISKA-SUBPROGRAM.                                                
014700     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
014800     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
014900     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
015000     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
015100     EJECT                                                                
015200 01  NYCKLAR-TILL-DLI.                                                    
015300     03  W-IDARTNR-X.                                                     
015400        05  W-IDARTNR           PIC S9(9) VALUE ZERO    COMP-3.           
015500     03  W-IDARTNR-SATS-X.                                                
015600        05  W-IDARTNR-SATS      PIC S9(9) VALUE ZERO    COMP-3.           
015700     03  W-IDARTNR-ING-X.                                                 
015800        05  W-IDARTNR-ING       PIC S9(9) VALUE ZERO    COMP-3.           
015900     03  W-TIBEHOV-SATS-X.                                                
016000        05  W-TIBEHOV-SATS      PIC S9(5) VALUE ZERO    COMP-3.           
016010     03  W-WDD901KY-X.                                                    
016020        05  W-IDARTNR-D9        PIC S9(9) VALUE ZERO    COMP-3.           
016030        05  W-IDDC-D9           PIC X(2)  VALUE SPACE.                    
016100     03  W-IDLEVNR-X.                                                     
016200        05  W-IDLEVNR           PIC X(5) VALUE '1002 '.                   
016300     03  W-WDD905KY-X.                                                    
016400         05  W-DAAVROP-AVS-X.                                             
016500             07  W-DAAVROP-AVS   PIC 9(6) VALUE ZERO.                     
016600         05  W-TILEVDAG-X.                                                
016700             07  W-TILEVDAG      PIC S9   VALUE ZERO    COMP-3.           
016800     03  W-WDJ301KY-MIN-X.                                                
016900        05  W-IDARTNR-SATS-MIN  PIC S9(9) VALUE ZERO    COMP-3.           
017000        05  W-IDARTNR-ING-MIN   PIC S9(9) VALUE ZERO    COMP-3.           
017100        05  W-TIBEHOV-MIN       PIC S9(5) VALUE ZERO    COMP-3.           
017200     03  W-WDJ301KY-MAX-X.                                                
017300        05  W-IDARTNR-SATS-MAX  PIC S9(9) VALUE ZERO    COMP-3.           
017400        05  W-IDARTNR-ING-MAX   PIC S9(9) VALUE ZERO    COMP-3.           
017500        05  W-TIBEHOV-MAX       PIC S9(5) VALUE ZERO    COMP-3.           
017600     03  W-WDJ1CSEQ-X.                                                    
017700        05  W-IDLEVNR-S         PIC X(5) VALUE SPACE.                     
017800        05  W-BELEVART-S        PIC X(30) VALUE SPACE.                    
017900        05  W-IDARTNR-S         PIC S9(9) VALUE ZERO    COMP-3.           
018000                                                                          
018100*                            *** GENERELLA SUBRUTINER                     
018200 01      SUBPROGRAM.                                                      
018300   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
018400   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
018500   03    ABEND           PIC X(8)    VALUE 'ABEND   '.                    
018600     EJECT                                                                
018700*                            *** PARAMETRAR TILL DATKORT                  
018800 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22219'.                  
018900 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
019000*01  -COPY WDATKORT                                                       
019100     EJECT                                                                
019200*                            *** PARAMETRAR TILL POSTSUM                  
019300*01  -COPY W0005       -PRE POSTSUM-.                                     
019400     EJECT                                                                
019500*01  -COPY WDATAREA                                                       
019600     EJECT                                                                
019700*                            *************************************        
019800*                            ** AREA FÖR W22215 - 2234-POST     **        
019900*                            ** SATSSTRUKTUR UPPDATERAD         **        
020000*                            *************************************        
020100*01  AREA  -COPY W2222203    -PRE 2234-.                                  
020200     EJECT                                                                
020300*                            *************************************        
020400*                            ** AREA FÖR W22220 - 2204-POST     **        
020500*                            ** 2204-HÄNDELSE SKAPAS FÖR ING.ART**        
020600*                            *************************************        
020700*01  AREA  -COPY W2212204    -PRE 2204-.                                  
020800     EJECT                                                                
020900******************************************************************        
021000*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
021100*                                                                         
021200 01      IMS-WS.                                                          
021300   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
021400                                                                          
021500*                            *** STATUSKOD FRÅN IMS                       
021600   03    STATUS-WS       PIC XX.                                          
021700     88  SEGMENT-FINNS               VALUE '  '.                          
021800     88  SEGMENT-UPPLAGT             VALUE '  '.                          
021900     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
022000     88  BASEN-SLUT                  VALUE 'GB'.                          
022100     88  IMS-EJ-OK                   VALUE 'XD'.                          
022200     SKIP3                                                                
022300   03    SSA1            PIC X(64).                                       
022400   03    SSA2            PIC X(32).                                       
022500   03    SSA3            PIC X(32).                                       
022600     SKIP3                                                                
022700   03    GODK-STATUSKODER.                                                
022800     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
022900     EJECT                                                                
023000*01      -COPY W0003                                                      
023100     EJECT                                                                
023200 01      DLI-IO-AREA     PIC X(900)  VALUE SPACE.                         
023300     SKIP3                                                                
023400*01  WLARTC01 -COPY WDK601               -RED DLI-IO-AREA.                
023500     EJECT                                                                
023600*01  WLARTC11 -COPY WDK611               -RED DLI-IO-AREA.                
023700     EJECT                                                                
023800*01  WLARTC24 -COPY WDK624               -RED DLI-IO-AREA.                
023900     EJECT                                                                
024000*01  WLSATB01 -COPY WDJ101 -PRE SATB-    -RED DLI-IO-AREA.                
024100     EJECT                                                                
024200*01  WLINLB11 -COPY WDD902 -PRE INLB11-  -RED DLI-IO-AREA.                
024300     EJECT                                                                
024400*01  WLINLB23 -COPY WDD905 -PRE INLB23-  -RED DLI-IO-AREA.                
024500     EJECT                                                                
024600*01  WLINLB32 -COPY WDD907 -PRE INLB32-  -RED DLI-IO-AREA.                
024700     EJECT                                                                
024800 01      WDJ1CSEQ REDEFINES DLI-IO-AREA.                                  
024900*   03  WLSATB11   -COPY WDJ111     -PRE SATE-                            
025000*   03  WLSATB01   -COPY WDJ101     -PRE SATE-                            
025100     EJECT                                                                
025200 01      DLI-IO-AREA-2   PIC X(200)  VALUE SPACE.                         
025300*01  WLSATB11 -COPY WDJ111 -PRE SATB-    -RED DLI-IO-AREA-2.              
025400     EJECT                                                                
025500 01      DLI-IO-AREA-3   PIC X(200)  VALUE SPACE.                         
025600*01  WLSATM01 -COPY WDJ301 -PRE SATM-    -RED DLI-IO-AREA-3.              
025700     EJECT                                                                
025800 LINKAGE SECTION.                                                         
025900                                                                          
026000*01      -COPY W0009     -PRE MSG-                                        
026100     EJECT                                                                
026200*01      -COPY W0008     -PRE ARTC-                                       
026300      05 FILLER          PIC X.                                           
026400     EJECT                                                                
026500*01      -COPY W0008     -PRE SATB-                                       
026600      05 FILLER          PIC X.                                           
026700     EJECT                                                                
026800*01      -COPY W0008     -PRE INLB-                                       
026900      05 FILLER          PIC X.                                           
027000     EJECT                                                                
027100*01      -COPY W0008     -PRE SATM1-                                      
027200      05 FILLER          PIC X.                                           
027300     EJECT                                                                
027400*01      -COPY W0008     -PRE SATM2-                                      
027500      05 FILLER          PIC X.                                           
027600     EJECT                                                                
027700*01      -COPY W0008     -PRE SATE-                                       
027800      05 FILLER          PIC X.                                           
027900     EJECT                                                                
028000*01      -COPY W0008     -PRE ARTC2-                                      
028100      05 FILLER          PIC X.                                           
028200     EJECT                                                                
028300 PROCEDURE DIVISION USING   MSG-PCB  ARTC-PCB  SATB-PCB                   
028400                            INLB-PCB SATM1-PCB SATM2-PCB                  
028500                            SATE-PCB ARTC2-PCB.                           
028600     ENTRY 'DLITCBL' USING  MSG-PCB  ARTC-PCB  SATB-PCB                   
028700                            INLB-PCB SATM1-PCB SATM2-PCB                  
028800                            SATE-PCB ARTC2-PCB.                           
028900     SKIP3                                                                
029000     PERFORM A-INIT                                                       
029100                                                                          
029200     PERFORM UNTIL END-OF-W22215                                          
029300       PERFORM B-BEHANDLA-2234-TRANS                                      
029400                                                                          
029500       IF CHKP-ANT > CHKP-MAX                                             
029600         PERFORM X-TAG-CHECKPOINT                                         
029700       END-IF                                                             
029800                                                                          
029900       PERFORM S01-LAES-W22215-POST                                       
030000     END-PERFORM                                                          
030100                                                                          
030200     PERFORM D-AVSLUTA                                                    
030300                                                                          
030400     MOVE ZERO TO RETURN-CODE                                             
030500     GOBACK                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 A-INIT SECTION.                                                          
030900******************************************************************        
031000*                                                                *        
031100*    ÖPPNA FILER, LÄS DATUMKORT OCH FÖRSTA POST PÅ TRANS-FILEN   *        
031200*                                                                *        
031300******************************************************************        
031400                                                                          
031500     OPEN INPUT  W22215                                                   
031600     OPEN OUTPUT W22220                                                   
031700                                                                          
031800     PERFORM IMS-RESTART                                                  
031900                                                                          
032000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
032100     MOVE D-AAR              TO W-DAGENS-DATUM-AA                         
032200                                WS-AAR                                    
032300     MOVE D-VECKA            TO W-DAGENS-DATUM-VV                         
032400     MOVE D-MAANAD           TO WS-MAANAD                                 
032500     MOVE D-DAG              TO WS-DAG                                    
032600     MOVE WS-DAGENS-DATUM-NY TO WS-DAGENS-DATUM                           
032700                                                                          
032800     MOVE 'W22219' TO POSTSUM-PROGNAMN                                    
032900                                                                          
033000     PERFORM S01-LAES-W22215-POST                                         
033100     .                                                                    
033200     EJECT                                                                
033300 B-BEHANDLA-2234-TRANS SECTION.                                           
033400******************************************************************        
033500*                                                                *        
033600*    SATSSTRUKTUR UPPDATERAD (ING ARTIKEL)                       *        
033700*                                                                *        
033800*    UPPDATERING AV SATSBEHOV                                    *        
033900*    ÄNDRAD BEHOVSINFO DELEATAS. HELT NY SATSBEHOVSINFO          *        
034000*    SKAPAS OM GIVNA FÖRUTSÄTTNINGAR UPPFYLLDA                   *        
034100*                                                                *        
034200******************************************************************        
034300                                                                          
034400     IF 2234-KDISATS NOT = DELEATE                                        
034500       MOVE 2234-IDARTNR-SATS TO W-IDARTNR-SATS                           
034600                                 WS-SPAR-IDARTNR-SATS                     
034700       MOVE 2234-IDARTNR-ING TO W-IDARTNR-ING                             
034800                                                                          
034900       PERFORM IMS-GET-SATB01                                             
035000       IF SEGMENT-FINNS                                                   
035100          MOVE SATB-STR-TIBORT TO WS-SPAR-TIBORT                          
035200          PERFORM S03-LAES-LEVPLAN-FOR-SATS                               
035300                                                                          
035400          MOVE LOW-VALUE        TO W-WDJ301KY-MIN-X                       
035500          MOVE HIGH-VALUE       TO W-WDJ301KY-MAX-X                       
035600          MOVE W-IDARTNR-SATS   TO W-IDARTNR-SATS-MIN                     
035700                                   W-IDARTNR-SATS-MAX                     
035800          MOVE 2234-IDARTNR-ING TO W-IDARTNR-ING-MIN                      
035900                                   W-IDARTNR-ING-MAX                      
036000          PERFORM IMS-GU-SATM01                                           
036100                                                                          
036200          PERFORM UNTIL SEGMENT-SAKNAS                                    
036300            MOVE NEJ TO SWITCH-TOT-SATSBEHOV-UPPD                         
036400            MOVE SATM-BEH-TIBEHOV TO WS-SPAR-TIBEHOV-SATS                 
036500            MOVE SATM-BEH-KVBEHOV TO WS-SPAR-KVBEHOV-SATS                 
036600            PERFORM IMS-GET-ARTC01                                        
036700            IF SEGMENT-FINNS                                              
036800              PERFORM S04-BORTTAG-SATSBEHOV                               
036900            END-IF                                                        
037000            MOVE LOW-VALUE        TO W-WDJ301KY-MIN-X                     
037100            MOVE HIGH-VALUE       TO W-WDJ301KY-MAX-X                     
037200            MOVE W-IDARTNR-SATS   TO W-IDARTNR-SATS-MIN                   
037300                                     W-IDARTNR-SATS-MAX                   
037400            MOVE 2234-IDARTNR-ING TO W-IDARTNR-ING-MIN                    
037500                                     W-IDARTNR-ING-MAX                    
037600            PERFORM IMS-GN-SATM01                                         
037700          END-PERFORM                                                     
037800                                                                          
037900          PERFORM IMS-GET-SATB01                                          
038000          PERFORM IMS-GET-SATB11-KVAL                                     
038100          PERFORM UNTIL SEGMENT-SAKNAS                                    
038200             MOVE SATB-RAD-TISTODAT    TO TMP1-YYMMDD                     
038300             MOVE WS-DAGENS-DATUM      TO TMP2-YYMMDD                     
038400             MOVE SATB-RAD-TISTADAT    TO TMP3-YYMMDD                     
038500             PERFORM WY2000Q1                                             
038600             IF TMP1-YYMMDD <= TMP2-YYMMDD AND                            
038700                TMP3-YYMMDD >= TMP2-YYMMDD                                
038800                   PERFORM S09-RENSA-RADEN                                
038900             ELSE                                                         
039000                PERFORM IMS-GET-ARTC01                                    
039100                IF  SEGMENT-FINNS AND ART-KDERS-UTG = ZERO                
039200                    IF  AVRTAB-ANTAL > ZERO                               
039300                      AND WS-SPAR-TIBORT  = ZERO                          
039400                         PERFORM S05-SKAPA-SATSBEHOV                      
039500                    END-IF                                                
039600                END-IF                                                    
039700             END-IF                                                       
039800             PERFORM IMS-GET-SATB11-KVAL                                  
039900          END-PERFORM                                                     
040000                                                                          
040100          IF  SWITCH-TOT-SATSBEHOV-UPPD = JA                              
040200             PERFORM S06-UPPDAT-HAENDELSEREG                              
040300          END-IF                                                          
040400                                                                          
040500         ELSE                                                             
040600               DISPLAY 'IDARTNR-SATS = '                                  
040700              2234-IDARTNR-SATS                                           
040800              ' I 2234 FINNS EJ I SATSREG'                                
040900         END-IF                                                           
041000     ELSE                                                                 
041100         PERFORM BA-DELETE-ARTREG-TOT-SATSBEHOV                           
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 BA-DELETE-ARTREG-TOT-SATSBEHOV SECTION.                                  
041600******************************************************************        
041700*                                                                *        
041800*    ALL BEORDINGSINFO DELEATAS PÅ ARTIKELREGISTRET FÖR          *        
041900*    INGÅENDE ARTIKEL                                            *        
042000*    BEHOVSSEGMENTEN PÅ SATSREGISTRET DELEATAS                   *        
042100******************************************************************        
042200                                                                          
042300     MOVE 2234-IDARTNR-ING TO W-IDARTNR-ING                               
042400     PERFORM IMS-GET-ARTC01                                               
042500     IF SEGMENT-FINNS                                                     
042600         PERFORM IMS-GET-ARTC24                                           
042700         PERFORM UNTIL SEGMENT-SAKNAS                                     
042800            PERFORM IMS-DELETE-ARTC                                       
042900            ADD +1            TO CHKP-ANT                                 
043000            PERFORM IMS-GET-ARTC24                                        
043100         END-PERFORM                                                      
043200     END-IF                                                               
043300                                                                          
043400     MOVE LOW-VALUE           TO W-WDJ301KY-MIN-X                         
043500     MOVE HIGH-VALUE          TO W-WDJ301KY-MAX-X                         
043600     MOVE 2234-IDARTNR-SATS   TO W-IDARTNR-SATS-MIN                       
043700                                 W-IDARTNR-SATS-MAX                       
043800     MOVE 2234-IDARTNR-ING    TO W-IDARTNR-ING-MIN                        
043900                                  W-IDARTNR-ING-MAX                       
044000     PERFORM IMS-GU-SATM01                                                
044100     PERFORM UNTIL SEGMENT-SAKNAS                                         
044200        PERFORM IMS-DELETE-SATM-PCB1                                      
044300        ADD +1                TO CHKP-ANT                                 
044400        PERFORM IMS-GN-SATM01                                             
044500     END-PERFORM                                                          
044600     .                                                                    
044700     EJECT                                                                
044800 D-AVSLUTA SECTION.                                                       
044900                                                                          
045000     CLOSE  W22215                                                        
045100            W22220                                                        
045200                                                                          
045300     MOVE 'S' TO POSTSUM-OPKOD                                            
045400     CALL POSTSUM USING POSTSUM-PARM                                      
045500     .                                                                    
045600     EJECT                                                                
045700 S01-LAES-W22215-POST SECTION.                                            
045800                                                                          
045900     READ W22215 INTO 2234-AREA                                           
046000     AT END                                                               
046100        SET END-OF-W22215 TO TRUE                                         
046200     NOT AT END                                                           
046300         MOVE 'W22215'       TO POSTSUM-FDNAMN                            
046400         MOVE 'W22219D2'     TO POSTSUM-DDNAMN2                           
046500         MOVE 2234-IDHTYP    TO POSTSUM-TRANSTYP                          
046600         CALL POSTSUM USING POSTSUM-PARM                                  
046700     END-READ                                                             
046800     .                                                                    
046900     EJECT                                                                
047000 S03-LAES-LEVPLAN-FOR-SATS SECTION.                                       
047100******************************************************************        
047200*                                                                *        
047300*    SATSENS LEVERANSPLAN LÄSES. ALLA ICKE BEORDRADE AVROP       *        
047400*    MED AVROPSKOD=2 FRÅN GÄLLANDE PLAN SPARAS I INTERN-         *        
047500*    TABELL.                                                     *        
047600*                                                                *        
047700******************************************************************        
047800     MOVE ZERO TO AVRTAB-ANTAL                                            
047900     MOVE W-IDARTNR-SATS  TO W-IDARTNR-D9                                 
047910     MOVE WC-CDC-SE       TO W-IDDC-D9                                    
048000     MOVE '1002 '         TO W-IDLEVNR                                    
048100     PERFORM IMS-GET-INLB11                                               
048200                                                                          
048300     IF  SEGMENT-FINNS                                                    
048400     AND AVRTAB-ANTAL NOT > AVRTAB-MAX                                    
048500         PERFORM IMS-GET-INLB23                                           
048600                                                                          
048700         PERFORM UNTIL SEGMENT-SAKNAS                                     
048800             IF  INLB23-KDAVROP = 2                                       
048900                MOVE INLB23-DAAVROP-AVS TO W-DAAVROP-AVS                  
049000                MOVE INLB23-TILEVDAG    TO W-TILEVDAG                     
049100* ? W-TILEVDAG ?                                                          
049200                PERFORM IMS-GET-INLB32                                    
049300                                                                          
049400                IF SEGMENT-SAKNAS                                         
049500                   ADD 1 TO AVRTAB-ANTAL                                  
049600                   MOVE INLB23-TIAVRDAT-DISP TO DAT-I-TIDATUM             
049700                   MOVE 'AAMMDD'             TO DAT-KDDATFORM             
049800                   CALL WDATKONV USING          DAT-KDDATFORM             
049900                                                DAT-I-TIDATUM             
050000                                                DAT-O-TIDATUM             
050100                                                DAT-KDSVAR                
050200                   IF DAT-KDSVAR-FEL                                      
050300                     MOVE 'FEL VID ANROP TILL DATKONV 2'                  
050400                                             TO FELTEXT-STR               
050500                     CALL FELLOG                                          
050600                   ELSE                                                   
050700                     MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                 
050800                     MOVE WS-TIAAVV                                       
050900                        TO AVRTAB-TIAVROP-DISP (AVRTAB-ANTAL)             
051000                   END-IF                                                 
051100                   MOVE INLB23-KVAVROP                                    
051200                        TO AVRTAB-KVAVROP (AVRTAB-ANTAL)                  
051300                END-IF                                                    
051400             END-IF                                                       
051500             PERFORM IMS-GET-INLB23                                       
051600         END-PERFORM                                                      
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 S04-BORTTAG-SATSBEHOV SECTION.                                           
052100******************************************************************        
052200*                                                                *        
052300*    UPPDATERING AV TOTALT SATSBEHOV I ARTIKEL-REG,              *        
052400*    SAMT DELEATE AV SATSBEHOV I SATSSTRUKTUREN FÖR AKTUELL               
052500*    ARTIKEL                                                     *        
052600*                                                                *        
052700******************************************************************        
052800                                                                          
052900     MOVE WS-SPAR-TIBEHOV-SATS TO W-TIBEHOV-SATS                          
053000     PERFORM IMS-GET-ARTC24-KVAL                                          
053100     IF  SEGMENT-FINNS                                                    
053200       SUBTRACT WS-SPAR-KVBEHOV-SATS                                      
053300                       FROM SATS-KVBEHOV-TOTSATS                          
053400       MOVE JA TO SWITCH-TOT-SATSBEHOV-UPPD                               
053500       IF  SATS-KVBEHOV-TOTSATS > ZERO                                    
053600           PERFORM IMS-REPLACE-ARTC                                       
053700           ADD +1              TO CHKP-ANT                                
053800       ELSE                                                               
053900           PERFORM IMS-DELETE-ARTC                                        
054000           ADD +1              TO CHKP-ANT                                
054100       END-IF                                                             
054200     END-IF                                                               
054300                                                                          
054400     MOVE WS-SPAR-TIBEHOV-SATS TO W-TIBEHOV-MIN                           
054500                                  W-TIBEHOV-MAX                           
054600     MOVE W-IDARTNR-ING        TO W-IDARTNR-ING-MIN                       
054700                                  W-IDARTNR-ING-MAX                       
054800     MOVE W-IDARTNR-SATS       TO W-IDARTNR-SATS-MIN                      
054900                                  W-IDARTNR-SATS-MAX                      
055000     PERFORM IMS-GET-SATM01                                               
055100                                                                          
055200     IF  SEGMENT-FINNS                                                    
055300       PERFORM IMS-DELETE-SATM                                            
055400       ADD +1                  TO CHKP-ANT                                
055500     END-IF                                                               
055600                                                                          
055700     .                                                                    
055800     EJECT                                                                
055900 S05-SKAPA-SATSBEHOV SECTION.                                             
056000******************************************************************        
056100*                                                                *        
056200*    NYTT SATSBEHOV SKAPAS MED HJÄLP AV AVROPS-TABELLEN          *        
056300*                                                                *        
056400******************************************************************        
056500                                                                          
056600     MOVE WS-SPAR-IDARTNR-SATS TO W-IDARTNR                               
056700     PERFORM IMS-GET-ARTC11                                               
056800     MOVE CLAG-KVVECKOR-LT TO WS-ANTAL                                    
056900                                                                          
057000     MOVE 1 TO AVRTAB-IX                                                  
057100                                                                          
057200     PERFORM UNTIL AVRTAB-IX > AVRTAB-ANTAL                               
057300       MOVE AVRTAB-TIAVROP-DISP (AVRTAB-IX) TO W-BEORDR-VECKA             
057400       COMPUTE ANTAL = WS-ANTAL * -1                                      
057500       CALL W009VADD USING W-BEORDR-VECKA ANTAL                           
057600       MULTIPLY AVRTAB-KVAVROP (AVRTAB-IX) BY SATB-RAD-REANTPSA           
057700                             GIVING W-BEORDR-ANTAL ROUNDED                
057800                                                                          
057900       MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                            
058000       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
058100       CALL WDATKONV USING DAT-KDDATFORM                                  
058200                           DAT-I-TIDATUM                                  
058300                           DAT-O-TIDATUM                                  
058400                           DAT-KDSVAR                                     
058500       IF DAT-KDSVAR-OK                                                   
058600          MOVE DAT-TIAA-VECKA   TO WS-AA                                  
058700          MOVE DAT-TIVV         TO WS-VV                                  
058800          MOVE WS-AAVV-NY       TO WS-TISTAVECKA                          
058900                                                                          
059000          IF SATB-RAD-TISTODAT = +999999                                  
059100             MOVE WS-TISTAVECKA    TO TMP1-YYWW                           
059200             MOVE W-BEORDR-VECKA   TO TMP2-YYWW                           
059300             PERFORM WY2000P3                                             
059400             IF TMP1-YYWW NOT > TMP2-YYWW                                 
059500                 PERFORM S05A-UPPDATERA-SATSBEHOV                         
059600             END-IF                                                       
059700          ELSE                                                            
059800             MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                      
059900             MOVE 'AAMMDD'          TO DAT-KDDATFORM                      
060000             CALL WDATKONV USING DAT-KDDATFORM                            
060100                                 DAT-I-TIDATUM                            
060200                                 DAT-O-TIDATUM                            
060300                                 DAT-KDSVAR                               
060400             IF DAT-KDSVAR-OK                                             
060500               MOVE DAT-TIAA-VECKA   TO WS-AA                             
060600               MOVE DAT-TIVV         TO WS-VV                             
060700               MOVE WS-AAVV-NY       TO WS-TISTOVECKA                     
060800                                                                          
060900               MOVE WS-TISTAVECKA    TO TMP1-YYWW                         
061000               MOVE W-BEORDR-VECKA   TO TMP2-YYWW                         
061100               MOVE WS-TISTOVECKA    TO TMP3-YYWW                         
061200               PERFORM WY2000Q3                                           
061300                                                                          
061400               IF (    TMP1-YYWW NOT > TMP2-YYWW                          
061500                   AND TMP3-YYWW > TMP2-YYWW )                            
061600                 PERFORM S05A-UPPDATERA-SATSBEHOV                         
061700               END-IF                                                     
061800             END-IF                                                       
061900          END-IF                                                          
062000        END-IF                                                            
062100        ADD 1 TO AVRTAB-IX                                                
062200     END-PERFORM                                                          
062300     .                                                                    
062400     EJECT                                                                
062500 S05A-UPPDATERA-SATSBEHOV SECTION.                                        
062600                                                                          
062700     MOVE WS-SPAR-IDARTNR-SATS TO W-IDARTNR-SATS-MIN                      
062800                                  W-IDARTNR-SATS-MAX                      
062900     MOVE SATB-RAD-IDARTNR     TO W-IDARTNR-ING-MIN                       
063000                                  W-IDARTNR-ING-MAX                       
063100     MOVE W-BEORDR-VECKA       TO W-TIBEHOV-SATS                          
063200                                  W-TIBEHOV-MIN                           
063300                                  W-TIBEHOV-MAX                           
063400***                                                                       
063500* 960904 (FRONTEC) TAGIT BORT LÄSNING SAMT REPLACE                        
063600* AV SATM PGA ATT DETTA ALDRIG INTRÄFFAR (ENL. BODIL L.)                  
063700* TILLAGT IGEN - KAN INTRÄFFA NÄR EN ARTIKEL FÖREKOMMER                   
063800* FLERA GÅNGER I SATSEN                                                   
063900***                                                                       
064000                                                                          
064100     PERFORM IMS-GET-SATM01                                               
064200     IF SEGMENT-SAKNAS                                                    
064300        MOVE WS-SPAR-IDARTNR-SATS TO SATM-BEH-IDARTNR                     
064400        MOVE W-IDARTNR-ING-MIN       TO SATM-BEH-IDARTNR-ING              
064500        MOVE W-BEORDR-VECKA          TO SATM-BEH-TIBEHOV                  
064600        MOVE W-BEORDR-ANTAL          TO SATM-BEH-KVBEHOV                  
064700        PERFORM IMS-INSERT-SATM                                           
064800        ADD +1                       TO CHKP-ANT                          
064900     ELSE                                                                 
065000        ADD W-BEORDR-ANTAL        TO SATM-BEH-KVBEHOV                     
065100        PERFORM IMS-REPLACE-SATM                                          
065200        ADD +1                       TO CHKP-ANT                          
065300     END-IF                                                               
065400                                                                          
065500     IF  SEGMENT-UPPLAGT                                                  
065600       MOVE W-BEORDR-VECKA        TO W-TIBEHOV-SATS                       
065700       PERFORM IMS-GET-ARTC24-KVAL                                        
065800                                                                          
065900       IF SEGMENT-FINNS                                                   
066000          ADD W-BEORDR-ANTAL TO SATS-KVBEHOV-TOTSATS                      
066100          PERFORM IMS-REPLACE-ARTC                                        
066200          ADD +1                  TO CHKP-ANT                             
066300       ELSE                                                               
066400          MOVE W-BEORDR-ANTAL     TO SATS-KVBEHOV-TOTSATS                 
066500          MOVE W-BEORDR-VECKA     TO SATS-TIBEHOV-SATS                    
066600          PERFORM IMS-INSERT-ARTC24                                       
066700          ADD +1                  TO CHKP-ANT                             
066800        END-IF                                                            
066900        MOVE JA TO SWITCH-TOT-SATSBEHOV-UPPD                              
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 S06-UPPDAT-HAENDELSEREG SECTION.                                         
067400******************************************************************        
067500*                                                                *        
067600*    2204-TRANS SKAPAS FÖR INGAENDE ARTIKEL FRÅN 2234-FILEN      *        
067700*                                                                *        
067800******************************************************************        
067900                                                                          
068000     MOVE SATB-RAD-IDARTNR TO 2204-IDARTNR                                
068100     MOVE 52               TO 2204-KDLPORS                                
068200     MOVE '2204'           TO 2204-IDHTYP                                 
068300     PERFORM S10-SKRIV-W22220                                             
068400                                                                          
068500     MOVE NEJ TO SWITCH-TOT-SATSBEHOV-UPPD                                
068600     .                                                                    
068700     EJECT                                                                
068800 S09-RENSA-RADEN SECTION.                                                 
068900******************************************************************        
069000*                                                                *        
069100*    ARTIKELRADEN HAR ALDRIG VARIT GÄLLANDE OCH DELEATAS         *        
069200*    FLIART PÅ ARTIKELREGISTRET ARTC01 KONTROLLERAS              *        
069300*                                                                *        
069400******************************************************************        
069500     PERFORM IMS-DELETE-SATB                                              
069600     ADD +1              TO CHKP-ANT                                      
069700                                                                          
069800     MOVE W-IDARTNR-ING  TO W-IDARTNR-S                                   
069900     MOVE SPACE          TO W-IDLEVNR-S                                   
070000     MOVE SPACE          TO W-BELEVART-S                                  
070100     MOVE '1002 '        TO W-IDLEVNR                                     
070200     MOVE NEJ            TO WS-FLIART                                     
070300                                                                          
070400     PERFORM IMS-GET-SATE-CSEQ-FIRST                                      
070500     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
070600        IF SATE-STR-TIBORT = ZERO                                         
070700           IF SATE-STR-IDARTNR < 100000000                                
070800              MOVE SATE-RAD-TISTODAT    TO TMP1-YYMMDD                    
070900              MOVE WS-DAGENS-DATUM      TO TMP2-YYMMDD                    
071000              PERFORM WY2000P1                                            
071100              IF TMP1-YYMMDD > TMP2-YYMMDD                                
071200                 MOVE JA TO WS-FLIART                                     
071300              END-IF                                                      
071400           END-IF                                                         
071500        END-IF                                                            
071600        PERFORM IMS-GET-SATE-CSEQ-NEXT                                    
071700     END-PERFORM                                                          
071800                                                                          
071900     PERFORM IMS-GET-ARTC01                                               
072000     IF SEGMENT-FINNS                                                     
072100        IF WS-FLIART NOT = ART-FLIART                                     
072200           MOVE WS-FLIART TO ART-FLIART                                   
072300           PERFORM IMS-REPLACE-ARTC                                       
072400           ADD +1         TO CHKP-ANT                                     
072500        END-IF                                                            
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 S10-SKRIV-W22220 SECTION.                                                
073000                                                                          
073100*****************************************************************         
073200*    SKRIV 2204-TRANSAR TILL W221P022  (HOPPA ÖVER WDG3 !)      *         
073300*****************************************************************         
073400                                                                          
073500     WRITE W22220-POST         FROM  2204-AREA                            
073600     MOVE 'W22220'             TO POSTSUM-FDNAMN                          
073700     MOVE 'W22219D3'           TO POSTSUM-DDNAMN2                         
073800     MOVE '2204'               TO POSTSUM-TRANSTYP                        
073900     CALL POSTSUM USING POSTSUM-PARM                                      
074000     .                                                                    
074100     EJECT                                                                
074200 X-TAG-CHECKPOINT   SECTION.                                              
074300                                                                          
074400     PERFORM IMS-CHECKPOINT                                               
074500     MOVE ZERO TO CHKP-ANT                                                
074600     .                                                                    
074700     EJECT                                                                
074800* IMS SEKTIONER                                                           
074900     SKIP3                                                                
075000 IMS-GET-SATB01 SECTION.                                                  
075100     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
075200            DELIMITED BY SIZE INTO SSA1                                   
075300     MOVE '  GE' TO GODK-STATUSKODER                                      
075400     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
075500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
075600     PERFORM IMS-STATUSKONTROLL                                           
075700     .                                                                    
075800     SKIP3                                                                
075900 IMS-GET-SATB11-KVAL SECTION.                                             
076000     STRING 'WLSATB11(IDARTNR  =' W-IDARTNR-ING-X ')'                     
076100            DELIMITED BY SIZE INTO SSA1                                   
076200     MOVE '  GE' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-2 SSA1                  
076400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-GU-SATM01 SECTION.                                                   
076900     STRING 'WLSATM01(WDJ301KY=>' W-WDJ301KY-MIN-X                        
077000                   '&WDJ301KY=<' W-WDJ301KY-MAX-X ')'                     
077100            DELIMITED BY SIZE INTO SSA1                                   
077200     MOVE '  GE' TO GODK-STATUSKODER                                      
077300     CALL CBLTDLI USING GHU SATM1-PCB DLI-IO-AREA-3 SSA1                  
077400     MOVE SATM1-STATUS-CODE TO STATUS-WS                                  
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     .                                                                    
077700     SKIP3                                                                
077800 IMS-GN-SATM01 SECTION.                                                   
077900     STRING 'WLSATM01(WDJ301KY=>' W-WDJ301KY-MIN-X                        
078000                   '&WDJ301KY=<' W-WDJ301KY-MAX-X ')'                     
078100            DELIMITED BY SIZE INTO SSA1                                   
078200     MOVE '  GE' TO GODK-STATUSKODER                                      
078300     CALL CBLTDLI USING GHN SATM1-PCB DLI-IO-AREA-3 SSA1                  
078400     MOVE SATM1-STATUS-CODE TO STATUS-WS                                  
078500     PERFORM IMS-STATUSKONTROLL                                           
078600     .                                                                    
078700     SKIP3                                                                
078800 IMS-GET-SATM01 SECTION.                                                  
078900     STRING 'WLSATM01(WDJ301KY=>' W-WDJ301KY-MIN-X                        
079000                   '&WDJ301KY=<' W-WDJ301KY-MAX-X ')'                     
079100            DELIMITED BY SIZE INTO SSA1                                   
079200     MOVE '  GE' TO GODK-STATUSKODER                                      
079300     CALL CBLTDLI USING GHU SATM2-PCB DLI-IO-AREA-3 SSA1                  
079400     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
079500     PERFORM IMS-STATUSKONTROLL                                           
079600     .                                                                    
079700     EJECT                                                                
079800 IMS-GET-SATE-CSEQ-FIRST SECTION.                                         
079900     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
080000            DELIMITED BY SIZE INTO SSA1                                   
080100     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
080200            DELIMITED BY SIZE INTO SSA2                                   
080300     MOVE '  GE' TO GODK-STATUSKODER                                      
080400     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA SSA1 SSA2                 
080500     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
080600     PERFORM IMS-STATUSKONTROLL                                           
080700     .                                                                    
080800     SKIP3                                                                
080900 IMS-GET-SATE-CSEQ-NEXT SECTION.                                          
081000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
081100            DELIMITED BY SIZE INTO SSA1                                   
081200     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
081300            DELIMITED BY SIZE INTO SSA2                                   
081400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
081500     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA SSA1 SSA2                 
081600     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
081700     PERFORM IMS-STATUSKONTROLL                                           
081800     .                                                                    
081900     EJECT                                                                
082000 IMS-GET-ARTC01 SECTION.                                                  
082100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ING-X ')'                     
082200            DELIMITED BY SIZE INTO SSA1                                   
082300     MOVE '  GE' TO GODK-STATUSKODER                                      
082400     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
082500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
082600     PERFORM IMS-STATUSKONTROLL                                           
082700     .                                                                    
082800     SKIP3                                                                
082900 IMS-GET-ARTC24 SECTION.                                                  
083000     MOVE 'WLARTC11 ' TO SSA1                                             
083100     MOVE 'WLARTC24 ' TO SSA2                                             
083200     MOVE '  GE' TO GODK-STATUSKODER                                      
083300     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1 SSA2               
083400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
083500     PERFORM IMS-STATUSKONTROLL                                           
083600     .                                                                    
083700     SKIP3                                                                
083800 IMS-GET-ARTC24-KVAL SECTION.                                             
083900     MOVE 'WLARTC11 ' TO SSA1                                             
084000     STRING 'WLARTC24*F(TIBEHOV  =' W-TIBEHOV-SATS-X ')'                  
084100            DELIMITED BY SIZE INTO SSA2                                   
084200     MOVE '  GE' TO GODK-STATUSKODER                                      
084300     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1 SSA2               
084400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
084500     PERFORM IMS-STATUSKONTROLL                                           
084600     .                                                                    
084700     EJECT                                                                
084800 IMS-GET-INLB11 SECTION.                                                  
084900     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
085000            DELIMITED BY SIZE INTO SSA1                                   
085100     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
085200            DELIMITED BY SIZE INTO SSA2                                   
085300     MOVE '  GE' TO GODK-STATUSKODER                                      
085400     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
085500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
085600     PERFORM IMS-STATUSKONTROLL                                           
085700     SKIP3                                                                
085800     .                                                                    
085900 IMS-GET-INLB23 SECTION.                                                  
086000     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
086100            DELIMITED BY SIZE INTO SSA1                                   
086200     MOVE 'WLINLB23 ' TO SSA2                                             
086300     MOVE '  GE' TO GODK-STATUSKODER                                      
086400     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2               
086500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
086600     PERFORM IMS-STATUSKONTROLL                                           
086700     .                                                                    
086800     SKIP3                                                                
086900 IMS-GET-INLB32 SECTION.                                                  
087000     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
087100            DELIMITED BY SIZE INTO SSA1                                   
087200     STRING 'WLINLB23(WDD905KY =' W-WDD905KY-X ')'                        
087300            DELIMITED BY SIZE INTO SSA2                                   
087400     MOVE 'WLINLB32 ' TO SSA3                                             
087500     MOVE '  GE' TO GODK-STATUSKODER                                      
087600     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
087700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     EJECT                                                                
088000     .                                                                    
088100 IMS-INSERT-ARTC24 SECTION.                                               
088200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ING-X ')'                     
088300            DELIMITED BY SIZE INTO SSA1                                   
088400     MOVE 'WLARTC11 ' TO SSA2                                             
088500     MOVE 'WLARTC24 ' TO SSA3                                             
088600     MOVE '  II' TO GODK-STATUSKODER                                      
088700     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
088800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088900     PERFORM IMS-STATUSKONTROLL                                           
089000     .                                                                    
089100     SKIP3                                                                
089200 IMS-INSERT-SATM SECTION.                                                 
089300     MOVE 'WLSATM01 ' TO SSA1                                             
089400     MOVE '  ' TO GODK-STATUSKODER                                        
089500     CALL CBLTDLI USING ISRT SATM2-PCB DLI-IO-AREA-3 SSA1                 
089600     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     .                                                                    
089900     EJECT                                                                
090000 IMS-REPLACE-ARTC SECTION.                                                
090100     MOVE '  '   TO GODK-STATUSKODER                                      
090200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
090300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600     SKIP3                                                                
090700 IMS-REPLACE-SATM SECTION.                                                
090800     MOVE '  '   TO GODK-STATUSKODER                                      
090900     CALL CBLTDLI USING REPL SATM2-PCB DLI-IO-AREA-3                      
091000     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
091100     PERFORM IMS-STATUSKONTROLL                                           
091200     .                                                                    
091300     SKIP3                                                                
091400 IMS-DELETE-ARTC SECTION.                                                 
091500     MOVE '  '   TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA                         
091700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000     EJECT                                                                
092100 IMS-DELETE-SATB SECTION.                                                 
092200     MOVE '  '   TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA-2                       
092400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     SKIP3                                                                
092800 IMS-DELETE-SATM SECTION.                                                 
092900     MOVE '  '   TO GODK-STATUSKODER                                      
093000     CALL CBLTDLI USING DLET SATM2-PCB DLI-IO-AREA-3                      
093100     MOVE SATM2-STATUS-CODE TO STATUS-WS                                  
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     SKIP3                                                                
093500 IMS-DELETE-SATM-PCB1 SECTION.                                            
093600     MOVE '  '   TO GODK-STATUSKODER                                      
093700     CALL CBLTDLI USING DLET SATM1-PCB DLI-IO-AREA-3                      
093800     MOVE SATM1-STATUS-CODE TO STATUS-WS                                  
093900     PERFORM IMS-STATUSKONTROLL                                           
094000     .                                                                    
094100     SKIP3                                                                
094200 IMS-GET-ARTC11 SECTION.                                                  
094300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
094400            DELIMITED BY SIZE INTO SSA1                                   
094500     MOVE 'WLARTC11 ' TO SSA2                                             
094600     MOVE '  ' TO GODK-STATUSKODER                                        
094700     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA SSA1 SSA2                
094800     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
094900     PERFORM IMS-STATUSKONTROLL                                           
095000     .                                                                    
095100     EJECT                                                                
095200 IMS-CHECKPOINT SECTION.                                                  
095300     SKIP2                                                                
095400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
095500     MOVE '  XD' TO GODK-STATUSKODER                                      
095600     CALL CBLTDLI USING CHKP MSG-PCB                                      
095700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
095800                        CHKP-AREA-LENGTH CHKP-AREA                        
095900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096000     PERFORM IMS-STATUSKONTROLL                                           
096100                                                                          
096200     IF IMS-EJ-OK                                                         
096300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
096400       DISPLAY FELTEXT                                                    
096500       CALL FELLOG                                                        
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900 IMS-RESTART SECTION.                                                     
097000     SKIP2                                                                
097100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
097200     MOVE '  ' TO GODK-STATUSKODER                                        
097300     CALL CBLTDLI USING XRST MSG-PCB                                      
097400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
097500                        CHKP-AREA-LENGTH CHKP-AREA                        
097600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097700     PERFORM IMS-STATUSKONTROLL                                           
097800     .                                                                    
097900     SKIP3                                                                
098000 IMS-STATUSKONTROLL SECTION.                                              
098100     SET STATUS-IX TO 1                                                   
098200     SEARCH GODK-STATUS AT END CALL FELLOG                                
098300        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                 
098400     END-SEARCH                                                           
098500     .                                                                    
098600     EJECT                                                                
098700*    -COPY WY2000P1                                                       
098800     EJECT                                                                
098900*    -COPY WY2000P3                                                       
099000     EJECT                                                                
099100*    -COPY WY2000Q1                                                       
099200     EJECT                                                                
099300*    -COPY WY2000Q3                                                       
