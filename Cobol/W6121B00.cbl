000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6121B00.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   FEBRUARI 2007.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER FIL W6121C OCH SKAPAR LISTOR SOM LÄGGS PÅ                  
001000*        D&P (DISTRIBUTION & PRINT). LISTA KAN SES PÅ WEBBEN              
001100*        PÅ FOLLOW UP SIDAN, NO-STOCK.                                    
001200*        PASSIVA ARTIKLAR UTAN LAGERSALDO.                                
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INFILER W612IC                                             
002300     SELECT W6121C                     ASSIGN TO W6121BD1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W6121C                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W6121C      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 01  CHKP-VAR.                                                            
003800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004300     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W6121B00'.            
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
005000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200                                                                          
005300 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
005400 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
005500                                                                          
005600 01  CURR-SECTION                PIC X(30)   VALUE SPACE.                 
005700 01  WS-ANTAL-POSTER             PIC 9(9)    VALUE ZERO.                  
005800                                                                          
005900 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 77  W6121C-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W6121C                       VALUE 'J'.                   
006500                                                                          
006600 77  DAP-OPEN-SW                 PIC X       VALUE 'N'.                   
006700     88  DAP-OPEN                            VALUE 'J'.                   
006800     88  DAP-CLOSED                          VALUE 'N'.                   
006900                                                                          
007000 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
007100     88  POST-FINNS                          VALUE 'J'.                   
007200     88  POST-SAKNAS                         VALUE 'N'.                   
007300                                                                          
007400*    --- ARBETSFÄLT                                                       
007500 01  FILLER                      PIC  X(9)   VALUE 'ARB-FAELT'.           
007600 01  ARBETSFAELT.                                                         
007700     03  INDX                    PIC  9(2)   VALUE ZERO.                  
007800     03  VV                      PIC  9(2)   VALUE ZERO.                  
007900     03  ANT-VV                  PIC  9(2)   VALUE ZERO.                  
008000     03  WS-VECKA                PIC  9(2)   VALUE ZERO.                  
008100     03  WS-TABELL    OCCURS 12.                                          
008200         05 WS-PER               PIC  9(2)   VALUE ZERO.                  
008300         05 WS-FORSTA-V          PIC  9(2)   VALUE ZERO.                  
008400         05 WS-SISTA-V           PIC  9(2)   VALUE ZERO.                  
008500         05 WS-KVOT              PIC S9(7)   VALUE ZERO COMP-3.           
008600                                                                          
008700     03  WS-KVOT-TOT6-MONTHS     PIC S9(9)   VALUE ZERO COMP-3.           
008800     03  DAGENS-AAAAMMDD         PIC  9(8)   VALUE ZERO.                  
008900     03  DAGENS-PER              PIC  9(4)   VALUE ZERO.                  
009000     03  DAG-PER REDEFINES DAGENS-PER.                                    
009100         05 DAGENS-AA            PIC  9(2).                               
009200         05 DAGENS-PP            PIC  9(2).                               
009300     03  WS-TIAAMMDD.                                                     
009400         05 WS-TIAA              PIC  9(2)   VALUE ZERO.                  
009500         05 WS-TIMM              PIC  9(2)   VALUE ZERO.                  
009600         05 WS-TIDD              PIC  9(2)   VALUE ZERO.                  
009700     03  TIAAMMDD REDEFINES WS-TIAAMMDD PIC 9(6).                         
009800     03  WS-TIAAVV.                                                       
009900         05 WS-AAR               PIC  9(2)   VALUE ZERO.                  
010000         05 WS-VV                PIC  9(2)   VALUE ZERO.                  
010100     03  TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                             
010200     03  WS-TIAAPER.                                                      
010300         05 TIAA                 PIC  9(2)   VALUE ZERO.                  
010400         05 PER                  PIC  9(2)   VALUE ZERO.                  
010500     03  TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                           
010600     03  WS-ARHAA.                                                        
010700         05 WS-ARH               PIC  9(2)   VALUE 20.                    
010800         05 WS-AA                PIC  9(2)   VALUE ZERO.                  
010900     03  WS-FOM-TOM.                                                      
011000         05 WS-FOM               PIC  X(2)   VALUE ZERO.                  
011100         05 WS-STRECK            PIC  X(1)   VALUE '-'.                   
011200         05 WS-TOM               PIC  X(2)   VALUE ZERO.                  
011300                                                                          
011400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011500 01  FILLER REDEFINES DAGENS-DATUM.                                       
011600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011900                                                                          
012000 01  DYNAMISKA-SUBPROGRAM.                                                
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
012600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012800     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
012900     EJECT                                                                
013000*---- PARAMETRAR TILL ABEND                                               
013100 01  RETURKODER.                                                          
013200     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
013300     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
013400     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
013500     03  RKOD-ABEND-WITH-DUMP    PIC S9(4) COMP SYNC VALUE +1000.         
013600                                                                          
013700     EJECT                                                                
013800*01  -COPY WDATAREA                                                       
013900     EJECT                                                                
014000*01  -COPY W0005   -PRE  POSTSUM-                                         
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014300*01  -COPY WZ01SUB                                                        
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014600*01  -COPY WZ01SEND                                                       
014700     EJECT                                                                
014800                                                                          
014900 01  FILLER                      PIC X(16) VALUE 'WWDC99-AREA'.           
015000*01  -COPY WWDC99                                                         
015100                                                                          
015200 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
015300*01  -COPY WTRAUTF8                                                       
015400                                                                          
015500 01  IN-AREA-START               PIC X(24)   VALUE 'IN-AREA'.             
015600*01  AREA -COPY W6121C     -PRE IN-                                       
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
015900 01  HDR-AREA.                                                            
016000*    03  -COPY WZ01REQU  -PRE HDR-                                        
016100*    03  -COPY WZ04HDR                                                    
016200 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
016300 01  DOC-AREA.                                                            
016400*    03  -COPY W6121D                                                     
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016700     SKIP3                                                                
016800*    --- STATUS-KOD FRÅN IMS                                              
016900 01  STATUS-WS                   PIC XX.                                  
017000     88  SEGMENT-FINNS                       VALUE '  '.                  
017100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017400     88  IMS-EJ-OK                           VALUE 'XD'.                  
017500     SKIP2                                                                
017600 01  GODK-STATUSKODER.                                                    
017700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017800     SKIP3                                                                
017900 01  SSA1                        PIC X(64).                               
018000 01  SSA2                        PIC X(64).                               
018100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018200 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
018300 01  NYCKLAR-TILL-DLI.                                                    
018400   03   W-IDARTNR-X.                                                      
018500     05 W-IDARTNR            PIC S9(9)  COMP-3  VALUE ZERO.               
018600   03   W-IDDC-X.                                                         
018700     05 W-IDDC               PIC X(2) VALUE SPACE.                        
018800   03   W-IDSKYLT-X.                                                      
018900     05 W-IDSKYLT            PIC X(3) VALUE 'GB'.                         
         03  W-IDDC-B6-X.                                                       
             05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                   
019000     EJECT                                                                
019100*    --- IMS FUNKTIONSKODER                                               
019200*01  -COPY W0003                                                          
019300     EJECT                                                                
019400*    ---  DLI INPUT-OUTPUT AREA                                           
019500     EJECT                                                                
019600 01  DLI-IO-WDD311.                                                       
019700*    03  -COPY WDD311                                                     
019800     EJECT                                                                
019900 01  DLI-IO-WDL711.                                                       
020000*    03  -COPY WDL711                                                     
020100     EJECT                                                                
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
           EJECT                                                                
020200 LINKAGE SECTION.                                                         
020300*01  -COPY W0009   -PRE MSG-                                              
020400 01  DISTRDOC-PCB                PIC X.                                   
020500     EJECT                                                                
020600*01  -COPY W0008  -PRE BENA-                                              
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900*01  -COPY W0008  -PRE WDL7-                                              
021000     05  FILLER                  PIC X.                                   
021100     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
021200 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB BENA-PCB                  
021300                           WDL7-PCB WDB6-PCB.                             
021400 MAIN SECTION.                                                            
021500     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB BENA-PCB                  
021600                           WDL7-PCB WDB6-PCB.                             
021700                                                                          
021800     PERFORM A-INIT                                                       
021900     PERFORM S01-LAS-W6121C                                               
022000     PERFORM UNTIL END-OF-W6121C                                          
022100        MOVE IN-DOC-IDDC TO SPAR-IDDC                                     
022200        PERFORM S90-OPEN-DAP-SEND                                         
022300        MOVE JA TO DAP-OPEN-SW                                            
022400        PERFORM C-SKAPA-HEADER                                            
022500                                                                          
022600        PERFORM UNTIL END-OF-W6121C OR                                    
022700          (SPAR-IDDC NOT = IN-DOC-IDDC)                                   
022800            PERFORM D-RAD-DATA                                            
022900            PERFORM S01-LAS-W6121C                                        
023000        END-PERFORM                                                       
023100                                                                          
023200        PERFORM S90-CLOSE-DAP-SEND                                        
023300        MOVE NEJ TO DAP-OPEN-SW                                           
023400     END-PERFORM                                                          
023500                                                                          
023600     IF DAP-OPEN                                                          
023700       PERFORM S90-CLOSE-DAP-SEND                                         
023800     END-IF                                                               
023900                                                                          
024000     PERFORM Z-FINIT                                                      
024100                                                                          
024200     MOVE ZERO TO RETURN-CODE                                             
024300     GOBACK                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 A-INIT SECTION.                                                          
024700                                                                          
024800     OPEN INPUT W6121C                                                    
024900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025000                                                                          
025100     ACCEPT DAGENS-DATUM FROM DATE                                        
025200     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
025300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
025400                         DAT-O-TIDATUM DAT-KDSVAR                         
025500     IF DAT-KDSVAR-OK                                                     
025600       MOVE DAT-TIAAVV-GRP TO DOC-TIAAVV                                  
025700       MOVE DAT-TIPP   TO DAGENS-PER                                      
025800       MOVE DAT-TIVV   TO WS-VECKA                                        
025900                                                                          
026000     END-IF                                                               
026100                                                                          
026200     PERFORM AA-HAMTA-VV-I-PER                                            
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 AA-HAMTA-VV-I-PER SECTION.                                               
026700                                                                          
026800     MOVE +1             TO INDX                                          
026900     MOVE DAGENS-PER     TO WS-TIAAPER                                    
027000                                                                          
027100     IF TIAA = 00   MOVE    99 TO TIAA                                    
027200     ELSE           SUBTRACT 1 FROM TIAA                                  
027300     END-IF                                                               
027400                                                                          
027500*    --- TA FRAM HUR MÅNGA VECKOR DET VAR FÖREGÅENDE ÅR                   
027600     MOVE TIAA           TO WS-AAR                                        
027700     MOVE 53             TO WS-VV                                         
027800     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
027900     MOVE TIAAVV         TO DAT-I-TIDATUM                                 
028000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
028100                         DAT-O-TIDATUM DAT-KDSVAR                         
028200     IF DAT-KDSVAR-OK                                                     
028300       MOVE 53           TO ANT-VV                                        
028400     ELSE                                                                 
028500       MOVE 52           TO ANT-VV                                        
028600     END-IF                                                               
028700                                                                          
028800*    --- FYLL I VECKONR FÖR PERIODERNA                                    
028900                                                                          
029000     MOVE 'AARP  '       TO DAT-KDDATFORM                                 
029100     MOVE TIAAPER        TO DAT-I-TIDATUM                                 
029200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
029300                         DAT-O-TIDATUM DAT-KDSVAR                         
029400     IF DAT-KDSVAR-OK                                                     
029500       IF PER            = 1                                              
029600         MOVE 1          TO WS-PER(INDX)                                  
029700                            WS-FORSTA-V(INDX)                             
029800       ELSE                                                               
029900         MOVE PER        TO WS-PER(INDX)                                  
030000         MOVE DAT-TIVV   TO WS-FORSTA-V(INDX)                             
030100       END-IF                                                             
030200     ELSE                                                                 
030300       MOVE 'FELAKTIGT DATUM - DATKONV2' TO FELTEXT                       
030400       CALL FELLOG                                                        
030500     END-IF                                                               
030600                                                                          
030700     PERFORM UNTIL INDX       >  12                                       
030800       ADD +1                 TO PER                                      
030900       IF PER                 >  12                                       
031000         ADD +1               TO TIAA                                     
031100         MOVE +1              TO PER                                      
031200       END-IF                                                             
031300                                                                          
031400       MOVE TIAAPER           TO DAT-I-TIDATUM                            
031500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
031600                           DAT-O-TIDATUM DAT-KDSVAR                       
031700       IF DAT-KDSVAR-OK                                                   
031800         IF PER               =  1                                        
031900           MOVE ANT-VV        TO WS-SISTA-V(INDX)                         
032000         ELSE                                                             
032100           COMPUTE WS-SISTA-V(INDX) = DAT-TIVV - 1                        
032200         END-IF                                                           
032300         ADD +1               TO INDX                                     
032400         IF INDX              <= 12                                       
032500           MOVE PER           TO WS-PER(INDX)                             
032600           IF PER             =  1                                        
032700             MOVE +1          TO WS-FORSTA-V(INDX)                        
032800           ELSE                                                           
032900             MOVE DAT-TIVV    TO WS-FORSTA-V(INDX)                        
033000           END-IF                                                         
033100         END-IF                                                           
033200       ELSE                                                               
033300         MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                     
033400         CALL FELLOG                                                      
033500       END-IF                                                             
033600     END-PERFORM                                                          
033700*    --- OM VECKOR I FÖRSTA OCH SISTA PERIODEN ÖVERLAPPAR,                
033800*    --- RÄTTA I FÖRSTA (DVS DEN ÄLDSTA) PERIODEN.                        
033900     IF WS-FORSTA-V(1)        = WS-SISTA-V(12)                            
034000     OR WS-FORSTA-V(1)        = WS-SISTA-V(12) - 1                        
034100       COMPUTE WS-FORSTA-V(1) = WS-SISTA-V(12) + 1                        
034200     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500 C-SKAPA-HEADER SECTION.                                                  
034600                                                                          
034700     MOVE 001             TO HDR-REQU-IDMSGVER                            
034800     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
034900     MOVE 'W6121B'        TO HDR-REQU-IDUSER                              
035000                                                                          
035100     MOVE 'NO-STOCK'      TO HDR-IDOUTTYPE                                
035200     MOVE SPACE           TO HDR-IDOUTREC                                 
035300     MOVE IN-DOC-IDDC     TO HDR-IDOUTREC(1:2)                            
035400     MOVE 'W6121B'        TO HDR-IDOUTREC(3:8)                            
035500                                                                          
035600     MOVE FUNCTION CURRENT-DATE(3:10)                                     
035700                                 TO HDR-IDLIST                            
035800     PERFORM S90-PUT-DAP-HEADER                                           
035900     .                                                                    
036000     EJECT                                                                
036100 D-RAD-DATA SECTION.                                                      
036200                                                                          
036300     IF  IN-DOC-ADLAGOMR = ZERO   AND IN-DOC-ADGANG = ZERO                
036400     AND IN-DOC-ADPLATS = ZERO    AND IN-DOC-ADBUFFOMR = ZERO             
036500     AND IN-DOC-ADBUFFGANG = ZERO AND IN-DOC-ADBUFFPL = ZERO              
036600        CONTINUE                                                          
036700     ELSE                                                                 
036800       MOVE IN-DOC-IDARTNR TO W-IDARTNR                                   
036900       MOVE IN-DOC-IDDC        TO W-IDDC                                  
037000                                                                          
037100*CHECK IF ACTIVE PART                                                     
037200       IF IN-DOC-KDREFSTA = 'A'                                           
037300         PERFORM DA-CHECK-ORDER-HITS                                      
037400*CHECK IF NO ORDER HIT WITHIN 6 MONTHS.                                   
037500                                                                          
037600         IF IN-DOC-KVREFPKT = ZERO                                        
037700         AND WS-KVOT-TOT6-MONTHS = ZERO                                   
037800                                                                          
037900           PERFORM S01-PRINT-PART                                         
038000         END-IF                                                           
038100       ELSE                                                               
038200         PERFORM S01-PRINT-PART                                           
038300       END-IF                                                             
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700                                                                          
038800 DA-CHECK-ORDER-HITS  SECTION.                                            
038900                                                                          
039000*CHECK IF NO ORDER HIT WITHIN 6 MONTHS.                                   
039100*CODE FROM PGM W2034200.                                                  
039200                                                                          
039300     MOVE ZERO            TO WS-KVOT-TOT6-MONTHS                          
039400                                                                          
039500     PERFORM IMS-GU-WDL711                                                
039600     IF SEGMENT-FINNS                                                     
039700                                                                          
039800        MOVE +1                          TO INDX                          
039900        PERFORM UNTIL INDX               >  12                            
040000          MOVE WS-FORSTA-V(INDX)         TO VV                            
040100          PERFORM UNTIL VV               >  WS-SISTA-V(INDX)              
040200            IF INDX > +6                                                  
040300*ACCUMULATE THE LAST 6 MONTHS                                             
040400                ADD DC-KVOT-RULL(VV)     TO WS-KVOT-TOT6-MONTHS           
040500            END-IF                                                        
040600            ADD +1                       TO VV                            
040700          END-PERFORM                                                     
040800          ADD +1                         TO INDX                          
040900        END-PERFORM                                                       
041000                                                                          
041100        MOVE +1                          TO INDX                          
041200        PERFORM UNTIL INDX               >  5                             
041300*INCLUDE CURRENT PERIOD                                                   
041400           ADD DC-KVOT-INNEV(INDX)  TO WS-KVOT-TOT6-MONTHS                
041500           ADD +1                        TO INDX                          
041600        END-PERFORM                                                       
041700                                                                          
041800     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100                                                                          
042200 S01-PRINT-PART          SECTION.                                         
042300                                                                          
042400     MOVE IN-DOC-IDDC          TO WS-IDDC                                 
                                        W-IDDC-B6                               
           PERFORM IMS-GU-WDB601                                                
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
043200     PERFORM IMS-GET-BENA-TEXT                                            
043300     IF SEGMENT-FINNS                                                     
043400        MOVE TEXT-BEART            TO TRAUTF8-TECONV-FROM                 
043500     ELSE                                                                 
043600        MOVE SPACE                 TO TRAUTF8-TECONV-FROM                 
043700        MOVE '278'                 TO TRAUTF8-KDCP                        
043800     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GET-BENA-TEXT                                           
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
043900     MOVE 25                       TO TRAUTF8-KVMAXTL                     
044000     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
044100     MOVE TRAUTF8-TECONV-TO        TO DOC-BEART                           
044200                                                                          
044300     MOVE 'LINE'                   TO DOC-IDAFPRCD                        
044400     MOVE IN-DOC-IDDC              TO DOC-IDDC                            
044500     MOVE IN-DOC-IDARTNR           TO DOC-IDARTNR                         
044600     MOVE IN-DOC-ADLAGOMR          TO DOC-ADLAGOMR                        
044700     MOVE IN-DOC-ADGANG            TO DOC-ADGANG                          
044800     MOVE IN-DOC-ADPLATS           TO DOC-ADPLATS                         
044900     MOVE IN-DOC-ADBUFFOMR         TO DOC-ADBUFFOMR                       
045000     MOVE IN-DOC-ADBUFFGANG        TO DOC-ADBUFFGANG                      
045100     MOVE IN-DOC-ADBUFFPL          TO DOC-ADBUFFPL                        
045200                                                                          
045300     PERFORM S90-PUT-DOC                                                  
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 Z-FINIT SECTION.                                                         
045800                                                                          
045900     CLOSE W6121C                                                         
046000     MOVE 'S' TO POSTSUM-OPKOD                                            
046100     CALL POSTSUM USING POSTSUM-PARM                                      
046200     .                                                                    
046300     EJECT                                                                
046400 X-TAG-CHECKPOINT SECTION.                                                
046500                                                                          
046600     PERFORM IMS-CHECKPOINT                                               
046700     MOVE ZERO TO CHKP-ANT                                                
046800     .                                                                    
046900     EJECT                                                                
047000 S01-LAS-W6121C SECTION.                                                  
047100                                                                          
047200     READ W6121C INTO IN-AREA                                             
047300     AT END                                                               
047400        SET END-OF-W6121C TO TRUE                                         
047500                                                                          
047600     NOT AT END                                                           
047700        MOVE 'W6121C'   TO POSTSUM-FDNAMN                                 
047800        MOVE 'W6121BD1' TO POSTSUM-DDNAMN2                                
047900        MOVE  W-IDDC   TO POSTSUM-TRANSTYP                                
048000        CALL POSTSUM USING POSTSUM-PARM                                   
048100        ADD +1 TO WS-ANTAL-POSTER                                         
048200     END-READ                                                             
048300     .                                                                    
048400     EJECT                                                                
048500* --- IMS SEKTIONER ---                                                   
048600 S90-OPEN-DAP-SEND SECTION.                                               
048700*    DISPLAY 'S90-OPEN-DAP-S'                                             
048800     MOVE 'S90-OPEN-DAP-S' TO CURR-SECTION                                
048900                                                                          
049000     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
049100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
049200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
049300                         SEND-OPEN-AREA                                   
049400     IF SEND-KDRC > ZERO                                                  
049500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
049600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
049700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
049800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
049900     END-IF                                                               
050000     .                                                                    
050100 S90-CLOSE-DAP-SEND SECTION.                                              
050200*    DISPLAY 'S90-CLOSE-DAP-'                                             
050300     MOVE 'S90-CLOSE-DAP-' TO CURR-SECTION                                
050400                                                                          
050500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
050600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
050700                                                                          
050800     IF SEND-KDRC > 0                                                     
050900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
051000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
051100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
051200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
051300     END-IF                                                               
051400     .                                                                    
051500 S90-PUT-DAP-HEADER SECTION.                                              
051600*    DISPLAY'90-PUT-DAP-HE'                                               
051700     MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
051800                                                                          
051900     MOVE 'PUT'                           TO SEND-KDFUNC                  
052000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
052100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
052200                         SEND-KVDLEN                                      
052300                         HDR-AREA                                         
052400     IF SEND-KDRC > ZERO                                                  
052500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
052600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
052700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
052800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
052900     END-IF                                                               
053000     .                                                                    
053100 S90-PUT-DOC SECTION.                                                     
053200*    DISPLAY   'S90-PUT-DOC '                                             
053300     MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
053400                                                                          
053500     MOVE 'PUT'                           TO SEND-KDFUNC                  
053600     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
053700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
053800                         SEND-KVDLEN                                      
053900                         DOC-AREA                                         
054000     IF SEND-KDRC > ZERO                                                  
054100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
054200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
054300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054500     END-IF                                                               
054600     MOVE 'W612C1'   TO POSTSUM-FDNAMN                                    
054700     MOVE 'PUTLINE'  TO POSTSUM-DDNAMN2                                   
054800     MOVE  W-IDDC   TO POSTSUM-TRANSTYP                                   
054900     CALL POSTSUM USING POSTSUM-PARM                                      
055000     .                                                                    
055100 IMS-GET-BENA-TEXT SECTION.                                               
055200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
055300            DELIMITED BY SIZE INTO SSA1                                   
055400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
055500            DELIMITED BY SIZE INTO SSA2                                   
055600     MOVE '  GE' TO GODK-STATUSKODER                                      
055700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
055800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
055900     PERFORM IMS-STATUS-KONTROLL                                          
056000     .                                                                    
056100     SKIP3                                                                
056200 IMS-GU-WDL711     SECTION.                                               
056300     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
056400          DELIMITED BY SIZE INTO SSA1                                     
056500     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
056600          DELIMITED BY SIZE INTO SSA2                                     
056700     MOVE '  GE' TO GODK-STATUSKODER                                      
056800     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
056900     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
057000     PERFORM IMS-STATUS-KONTROLL                                          
057100     .                                                                    
057200     SKIP3                                                                
057300                                                                          
057400 IMS-RESTART SECTION.                                                     
057500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
057600     MOVE '  ' TO GODK-STATUSKODER                                        
057700     CALL CBLTDLI USING XRST MSG-PCB                                      
057800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
057900                        CHKP-AREA-LENGTH CHKP-AREA                        
058000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058100     PERFORM IMS-STATUS-KONTROLL                                          
058200     .                                                                    
058300     SKIP3                                                                
058400 IMS-CHECKPOINT SECTION.                                                  
058500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
058600     MOVE '  XD' TO GODK-STATUSKODER                                      
058700     CALL CBLTDLI USING CHKP MSG-PCB                                      
058800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
058900                        CHKP-AREA-LENGTH CHKP-AREA                        
059000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059100     PERFORM IMS-STATUS-KONTROLL                                          
059200                                                                          
059300     IF IMS-EJ-OK                                                         
059400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
059500       DISPLAY FELTEXT                                                    
059600       CALL FELLOG                                                        
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUS-KONTROLL                                          
           .                                                                    
           SKIP3                                                                
060000 IMS-STATUS-KONTROLL SECTION.                                             
060100     SKIP2                                                                
060200     SET STATUS-IX TO 1                                                   
060300     SEARCH GODK-STATUS                                                   
060400       AT END                                                             
060500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
060600           DELIMITED BY SIZE INTO FELTEXT                                 
060700         DISPLAY FELTEXT                                                  
060800         CALL FELLOG                                                      
060900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
061000         CONTINUE                                                         
061100     END-SEARCH                                                           
061200     .                                                                    
