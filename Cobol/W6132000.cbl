000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6132000.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   08/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        CALCULATION OF POINTS                                            
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDK6                                       
001200*                                                                         
001300*                                                                         
001400*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
001500*    E'TRACKER: 8081720       ORDER STEERING TO VOHF                      
001600*    E'TRACKER: 8069785 CHANGE VALUES IN CC-CALCULATE                     
001610*    E'TRACKER: 10254592 2015-HÖST  DECOMISSION VOHF                      
001620*    JIRA     : 1967     180322  CHANGE CALCULATED REFILLING              
001630*                                QTY-PICK FOR AREA 11                     
001640*                                AND ADD AREA 36                          
001641*    JIRA     : 2564     180611  ADD MAX STOCK TO AREA                    
001642*                                20, 21 AND 22                            
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- DEMAND HISTORY FILE                                        
002500     SELECT INFIL                      ASSIGN TO W61320D1.                
002600     SELECT UTFIL                      ASSIGN TO W61320D2.                
002700     SELECT SORTFIL                    ASSIGN TO W61320D3.                
002800     SELECT INFILS                     ASSIGN TO W61320D4.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  INFIL                                                                
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W02008      -L.                                                
003900     EJECT                                                                
004000 FD  UTFIL                                                                
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  OUTPOST    -COPY W6132001    -L.                                     
004500     EJECT                                                                
004600 SD  SORTFIL.                                                             
004700                                                                          
004800 01  SORT-POSTA.                                                          
004900*03  POST -COPY W61320          -PRE SORT-                                
005000     EJECT                                                                
005100 FD  INFILS                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  -COPY W61320      -L.                                                
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)    VALUE 'W6132000'.            
006000 77  YES                         PIC X       VALUE 'J'.                   
006100 77  NOO                         PIC X       VALUE 'N'.                   
006200     SKIP2                                                                
006300 01  ERRTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600                                                                          
006700 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
006800     88  END-OF-INFIL                        VALUE 'Y'.                   
006900     SKIP2                                                                
007000 77  INFILS-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-INFILS                       VALUE 'Y'.                   
007200     SKIP2                                                                
007300 01  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
007500 01  WS-KVOI-OVR                 PIC S9(9)   VALUE ZERO COMP-3.           
007600 01  WS-KVOI-PLOCK               PIC S9(9)   VALUE ZERO COMP-3.           
007700 01  WS-KVOI-MONTH            PIC S9(9)V9(1) VALUE ZERO COMP-3.           
008000 01  WS-KVREFPKT-PLOCK           PIC S9(9)   VALUE ZERO COMP-3.           
008100 01  WS-KVREFBER-PLOCK           PIC S9(9)   VALUE ZERO COMP-3.           
008200 77  W-NO-CASE                   PIC S9(7)   COMP-3  VALUE ZERO.          
008300 77  W-ANT-REST                  PIC S9(7)   COMP-3  VALUE ZERO.          
008400 77  W-QTY-CASE                  PIC S9(7)   COMP-3  VALUE ZERO.          
008410 77  WS-MAX-Q3                   PIC S9(3)   COMP-3  VALUE ZERO.          
008420 77  WS-ADPLATS                  PIC 9(5)    VALUE ZERO.                  
008500                                                                          
008530 01  WS-NUM-ADPLATS              PIC  9(5)   VALUE ZERO.                  
008600                                                                          
008700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008800 01  FILLER REDEFINES TODAYS-DATE.                                        
008900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
009000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
009100     03  TODAYS-DATE-DAY         PIC 9(2).                                
009200     EJECT                                                                
009300 01  RETURKODER.                                                          
009400   03  RKOD-ABEND                PIC S9(4)  COMP SYNC VALUE ZERO.         
009500   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009600   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
009700     EJECT                                                                
009800 01  CHKP-VAR.                                                            
009900   03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.           
010000   03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                   
010100   03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.           
010200   03 CHKP-AREA                PIC X(32)   VALUE SPACE.                   
010300   03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.             
010400   03 CHKP-MAX                 PIC S9(3)   VALUE +900 COMP-3.             
010500 01  GENERAL-SUBPROGRAMS.                                                 
010600*                                                                         
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL POSTSUM                                          
011400*                                                                         
011500*01  -COPY W0005   -PRE  POSTSUM-                                         
011600     EJECT                                                                
011700*01  -COPY WDATAREA                                                       
011800     EJECT                                                                
011900 01  HIST-AREA-START             PIC X(24)   VALUE                        
012000                                             'HIST-AREA-START'.           
012100*01  AREA -COPY W02008     -PRE HIST-                                     
012200*                                                                         
012300     EJECT                                                                
012400 01  UT-AREA-START               PIC X(24)   VALUE                        
012500                                             'UT-AREA-START'.             
012600*01  POST -COPY W6132001   -PRE UT-                                       
012700*                                                                         
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013000     SKIP3                                                                
013100 01  KEYS-TILL-DLI.                                                       
013500     03  W-IDARTNR-X.                                                     
013600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013700     SKIP2                                                                
013800 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
013900*    --- STATUS-KOD FRÅN IMS                                              
014000 01 SW-BMP                 PIC X.                                         
014100     88  BMP                      VALUE 'J'.                              
014200 01  STATUS-WS                   PIC XX.                                  
014300     88  SEGMENT-FOUND                       VALUE '  '.                  
014400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014600     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
014700     88  IMS-NOT-OK                          VALUE 'XD'.                  
014800     SKIP2                                                                
014900 01  GOOD-STATUSCODES.                                                    
015000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(64).                               
015300 01  SSA2                        PIC X(64).                               
015400     EJECT                                                                
015500 01  VIMSREGT          PIC X(8)     VALUE 'VIMSREGT'.                     
015600*    --- IMS FUNCTION CODES                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000                                                                          
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016600 01  DLI-IO-WDK601.                                                       
016700*    03  -COPY WDK601                                                     
016800     EJECT                                                                
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
017000 01  DLI-IO-WDK611.                                                       
017100*    03  -COPY WDK611                                                     
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017400                                                                          
017500*01  -COPY W0009   -PRE MSG-                                              
017600                                                                          
017700*01  -COPY W0008  -PRE WDK6-                                              
017800     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
018400 MAIN SECTION.                                                            
018500     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
018600                                                                          
018700     SKIP2                                                                
018800     PERFORM A-INIT                                                       
018900                                                                          
019000     SORT SORTFIL ON                                                      
019100        ASCENDING KEY SORT-IDARTNR                                        
019200                                                                          
019300          INPUT PROCEDURE B-SORT-INPUT GIVING INFILS                      
019400                                                                          
019500     IF SORT-RETURN NOT = 0                                               
019600       MOVE SORT-RETURN TO SORT-RETURN-X                                  
019700       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
019800       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
019900       DISPLAY ERRTEXT                                                    
020000       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
020100       PERFORM S99-ABEND                                                  
020200     END-IF                                                               
020300                                                                          
020400     OPEN INPUT INFILS                                                    
020500                                                                          
020600     PERFORM C-CALCULATE-POINTS                                           
020700                                                                          
020800     PERFORM Z-FINIT                                                      
020900                                                                          
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 A-INIT SECTION.                                                          
021500     SKIP2                                                                
021600     MOVE SPACE   TO SW-BMP                                               
021700     CALL VIMSREGT                                                        
021800     IF RETURN-CODE = +8                                                  
021900       MOVE 'J'    TO SW-BMP                                              
022000       PERFORM IMS-RESTART                                                
022100     END-IF                                                               
022200                                                                          
022300     OPEN INPUT INFIL                                                     
022400     OPEN OUTPUT UTFIL                                                    
022500                                                                          
022600     ACCEPT TODAYS-DATE   FROM DATE                                       
022700     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
022800     MOVE TODAYS-DATE     TO DAT-I-TIDATUM                                
022900                                                                          
023000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
023100                     DAT-O-TIDATUM DAT-KDSVAR                             
023200                                                                          
023600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023700     .                                                                    
023800     EJECT                                                                
023900 B-SORT-INPUT    SECTION.                                                 
024000     PERFORM S01-READ-INFIL                                               
024100     PERFORM UNTIL END-OF-INFIL                                           
024200        IF HIST-IDDC = '11'                                               
024300          MOVE HIST-AREA   TO SORT-POST                                   
026200          RELEASE SORT-POSTA                                              
026300                                                                          
026400        END-IF                                                            
026500       PERFORM S01-READ-INFIL                                             
026600     END-PERFORM                                                          
026700     CLOSE INFIL                                                          
026800     .                                                                    
026900     EJECT                                                                
027000 C-CALCULATE-POINTS  SECTION.                                             
027100     PERFORM S02-READ-INFILS                                              
027200     PERFORM CA-NOLLA                                                     
027300     PERFORM UNTIL END-OF-INFILS                                          
027400       PERFORM UNTIL WS-IDARTNR NOT = SORT-IDARTNR OR                     
027500          END-OF-INFILS                                                   
027600         PERFORM CB-ADDA                                                  
027700         PERFORM S02-READ-INFILS                                          
027800       END-PERFORM                                                        
028000       IF CLAG-ADLAGOMR = 10                                              
028100         PERFORM CD-CALCULATE-LAG-L10                                     
028200       ELSE                                                               
028300         IF CLAG-ADLAGOMR = 11                                            
028301           PERFORM CE-CALCULATE-LAG-L11                                   
028302         ELSE                                                             
028306           IF CLAG-ADLAGOMR = 21 OR 22 OR 13                              
028400             PERFORM CF-CALCULATE-LAG-L2                                  
028500           ELSE                                                           
028600** JIRA 1967 - ADD AREA 36 AS SAME LOGIC USED FOR 30 TO 33                
028706             IF CLAG-ADLAGOMR = 30 OR 31 OR 32 OR 33 OR 36                
028800               PERFORM CG-CALCULATE-LAG-L3                                
028900             ELSE                                                         
029000               IF CLAG-ADLAGOMR = 74 OR 72                                
029100                 PERFORM CH-CALCULATE-LAG-L4                              
029200               ELSE                                                       
029210                 IF CLAG-ADLAGOMR = 23                                    
029220                   PERFORM CI-CALCULATE-LAG-L5                            
029230                 ELSE                                                     
029300                   PERFORM CJ-CALCULATE-LAG-L6                            
029310                 END-IF                                                   
029400               END-IF                                                     
029500             END-IF                                                       
029600           END-IF                                                         
029700         END-IF                                                           
029710       END-IF                                                             
029800*                                                                         
029900       IF (CLAG-KVQPACK-3 > 1 AND                                         
030000         CLAG-KVQPACK-3 > CLAG-KVREFBER-PLOCK) AND                        
030100         CLAG-ADLAGOMR NOT = 10                                           
030200          MOVE CLAG-KVQPACK-3 TO CLAG-KVREFBER-PLOCK                      
030201       END-IF                                                             
030202*                                                                         
030203       PERFORM IMS-REPL-WDK611                                            
030204       IF CLAG-ADLAGOMR > ZERO                                            
030205         PERFORM CCA-SKRIV-UTFIL                                          
030206       END-IF                                                             
030300       ADD 1              TO CHKP-ANT                                     
030400       IF BMP AND                                                         
030500          CHKP-ANT > CHKP-MAX                                             
030600          PERFORM X-TAG-CHECKPOINT                                        
030700       END-IF                                                             
030800*                                                                         
030900       PERFORM CA-NOLLA                                                   
031000     END-PERFORM                                                          
031100     .                                                                    
031200     EJECT                                                                
031300 CA-NOLLA  SECTION.                                                       
031400     MOVE ZERO                 TO WS-KVOI-OVR                             
031600                                  WS-KVOI-PLOCK                           
031700     MOVE SORT-IDARTNR         TO WS-IDARTNR                              
031800     MOVE WS-IDARTNR           TO W-IDARTNR                               
031900     PERFORM IMS-GHU-WDK611                                               
032000     .                                                                    
032100     EJECT                                                                
032200 CB-ADDA   SECTION.                                                       
032300                                                                          
032400     IF SORT-IDDISTR = 98 OR 81                                           
032500**SATSORDER                                                               
032600        ADD SORT-KVBEART        TO WS-KVOI-OVR                            
032700     ELSE                                                                 
032800        PERFORM CBA-KOLL-KVANTQ3                                          
033600        ADD W-ANT-REST          TO WS-KVOI-PLOCK                          
033700        ADD W-QTY-CASE          TO WS-KVOI-OVR                            
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 CBA-KOLL-KVANTQ3  SECTION.                                               
034300     MOVE SORT-KVBEART          TO W-ANT-REST                             
034400     MOVE ZERO                  TO W-NO-CASE                              
034500                                   W-QTY-CASE                             
034600     IF (CLAG-KVQPACK-3    > +1   OR                                      
034700         CLAG-KVQPACK-4    > +0)                                          
034800        IF CLAG-KVQPACK-3 > +1                                            
034900             COMPUTE W-NO-CASE =                                          
035000                   SORT-KVBEART / CLAG-KVQPACK-3                          
035100             END-COMPUTE                                                  
035200             COMPUTE W-QTY-CASE =                                         
035300                   W-NO-CASE * CLAG-KVQPACK-3                             
035400             END-COMPUTE                                                  
035500             COMPUTE W-ANT-REST =                                         
035600                   SORT-KVBEART - W-QTY-CASE                              
035700             END-COMPUTE                                                  
035800                                                                          
035900        ELSE                                                              
036000           IF CLAG-KVQPACK-4 > +0                                         
036100             COMPUTE W-NO-CASE =                                          
036200                   SORT-KVBEART / CLAG-KVQPACK-4                          
036300             END-COMPUTE                                                  
036400             COMPUTE W-QTY-CASE =                                         
036500                   W-NO-CASE * CLAG-KVQPACK-4                             
036600             END-COMPUTE                                                  
036700             COMPUTE W-ANT-REST =                                         
036800                   SORT-KVBEART -  W-QTY-CASE                             
036900             END-COMPUTE                                                  
037000           END-IF                                                         
037100        END-IF                                                            
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
048800* CALCULATION FOR ADDRESS 10                                              
048900 CD-CALCULATE-LAG-L10 SECTION.                                            
049000     COMPUTE WS-KVREFPKT-PLOCK ROUNDED = WS-KVOI-PLOCK * 2 / 40           
049100     COMPUTE WS-KVOI-MONTH ROUNDED = WS-KVOI-PLOCK / 2                    
049200                                                                          
049210     IF WS-KVOI-MONTH < 10.1                                              
049220        PERFORM CDA-CALC-FREQ10                                           
050300     ELSE                                                                 
050310        IF WS-KVOI-MONTH < 30.1                                           
050320           PERFORM CDB-CALC-FREQ30                                        
051401        ELSE                                                              
051402           IF WS-KVOI-MONTH < 100.1                                       
051403              PERFORM CDC-CALC-FREQ100                                    
052501           ELSE                                                           
052502              IF WS-KVOI-MONTH < 1000.1                                   
052503                 PERFORM CDD-CALC-FREQ1000                                
053601              ELSE                                                        
053602                 PERFORM CDE-CALC-FREQ1000-1                              
053901              END-IF                                                      
053902           END-IF                                                         
054100        END-IF                                                            
054300     END-IF                                                               
055000*                                                                         
060701                                                                          
060710     IF CLAG-KVMAXPL > ZERO                                               
060711       IF CLAG-ADGANG > 9 AND CLAG-ADGANG < 36                            
060712         COMPUTE WS-KVREFPKT-PLOCK = CLAG-KVMAXPL * 0.2                   
060713         COMPUTE CLAG-KVREFBER-PLOCK = CLAG-KVMAXPL * 0.8                 
060714       ELSE                                                               
060720         COMPUTE CLAG-KVREFBER-PLOCK =                                    
060730                 CLAG-KVMAXPL - CLAG-KVREFPKT-PLOCK                       
060740         IF CLAG-KVREFBER-PLOCK < ZERO                                    
060750            MOVE ZERO TO CLAG-KVREFBER-PLOCK                              
060760         END-IF                                                           
060761         MOVE CLAG-KVREFBER-PLOCK TO WS-KVREFBER-PLOCK                    
060762       END-IF                                                             
060770     ELSE                                                                 
060772        MOVE WS-KVREFBER-PLOCK TO CLAG-KVREFBER-PLOCK                     
060780     END-IF                                                               
060781                                                                          
060790     MOVE WS-KVOI-OVR        TO CLAG-KVOI-OVR                             
060791     MOVE WS-KVOI-PLOCK      TO CLAG-KVOI-PLOCK                           
060792     MOVE WS-KVREFPKT-PLOCK  TO CLAG-KVREFPKT-PLOCK                       
060800     .                                                                    
061100     EJECT                                                                
061101 CDA-CALC-FREQ10 SECTION.                                                 
061102                                                                          
061103     EVALUATE TRUE                                                        
061110       WHEN CLAG-PRARTSTD < 3.01                                          
061120          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061121                           WS-KVOI-PLOCK * 100 / 40                       
061130       WHEN CLAG-PRARTSTD < 30.01                                         
061140          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061141                           WS-KVOI-PLOCK * 55 / 40                        
061150       WHEN CLAG-PRARTSTD < 300.01                                        
061160          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061161                           WS-KVOI-PLOCK * 30 / 40                        
061170       WHEN CLAG-PRARTSTD > 300.01                                        
061190          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061191                           WS-KVOI-PLOCK * 30 / 40                        
061192     END-EVALUATE                                                         
061204     .                                                                    
061205     EJECT                                                                
061206 CDB-CALC-FREQ30 SECTION.                                                 
061207                                                                          
061208     EVALUATE TRUE                                                        
061209       WHEN CLAG-PRARTSTD < 3.01                                          
061210          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061211                           WS-KVOI-PLOCK * 70 / 40                        
061212       WHEN CLAG-PRARTSTD < 30.01                                         
061213          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061214                           WS-KVOI-PLOCK * 35 / 40                        
061215       WHEN CLAG-PRARTSTD < 300.01                                        
061216          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061217                           WS-KVOI-PLOCK * 25 / 40                        
061218       WHEN CLAG-PRARTSTD > 300.01                                        
061219          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061220                           WS-KVOI-PLOCK * 25 / 40                        
061221     END-EVALUATE                                                         
061222     .                                                                    
061223     EJECT                                                                
061224 CDC-CALC-FREQ100 SECTION.                                                
061225                                                                          
061226     EVALUATE TRUE                                                        
061227       WHEN CLAG-PRARTSTD < 3.01                                          
061228          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061229                           WS-KVOI-PLOCK * 35 / 40                        
061230       WHEN CLAG-PRARTSTD < 30.01                                         
061231          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061232                           WS-KVOI-PLOCK * 25 / 40                        
061233       WHEN CLAG-PRARTSTD < 300.01                                        
061234          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061235                           WS-KVOI-PLOCK * 20 / 40                        
061236       WHEN CLAG-PRARTSTD > 300.01                                        
061237          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061238                           WS-KVOI-PLOCK * 20 / 40                        
061239     END-EVALUATE                                                         
061240     .                                                                    
061241     EJECT                                                                
061242 CDD-CALC-FREQ1000 SECTION.                                               
061243                                                                          
061244     EVALUATE TRUE                                                        
061245       WHEN CLAG-PRARTSTD < 3.01                                          
061246          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061247                           WS-KVOI-PLOCK * 30 / 40                        
061248       WHEN CLAG-PRARTSTD < 30.01                                         
061249          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061250                           WS-KVOI-PLOCK * 25 / 40                        
061251       WHEN CLAG-PRARTSTD < 300.01                                        
061252          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061253                           WS-KVOI-PLOCK * 20 / 40                        
061254       WHEN CLAG-PRARTSTD > 300.01                                        
061255          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061256                           WS-KVOI-PLOCK * 10 / 40                        
061257     END-EVALUATE                                                         
061258     .                                                                    
061259     EJECT                                                                
061260 CDE-CALC-FREQ1000-1 SECTION.                                             
061261                                                                          
061262     EVALUATE TRUE                                                        
061263       WHEN CLAG-PRARTSTD < 3.01                                          
061264          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061265                           WS-KVOI-PLOCK * 10 / 40                        
061266       WHEN CLAG-PRARTSTD < 30.01                                         
061267          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061268                           WS-KVOI-PLOCK *  8 / 40                        
061269       WHEN CLAG-PRARTSTD < 300.01                                        
061270          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061271                           WS-KVOI-PLOCK *  5 / 40                        
061272       WHEN CLAG-PRARTSTD > 300.01                                        
061273          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
061274                           WS-KVOI-PLOCK *  5 / 40                        
061275     END-EVALUATE                                                         
061276     .                                                                    
061277     EJECT                                                                
061290* CALCULATION FOR ADDRESS 11                                              
061291 CE-CALCULATE-LAG-L11 SECTION.                                            
061296     COMPUTE WS-KVREFPKT-PLOCK ROUNDED = WS-KVOI-PLOCK * 2 / 40           
061297     COMPUTE WS-KVOI-MONTH ROUNDED = WS-KVOI-PLOCK / 2                    
061298                                                                          
061299     IF CLAG-PRARTSTD < 3.01                                              
061300       IF WS-KVOI-MONTH < 10.1                                            
061301         COMPUTE WS-KVREFBER-PLOCK ROUNDED =                              
061302                         WS-KVOI-PLOCK * 100 / 40                         
061303       ELSE                                                               
061304         IF WS-KVOI-MONTH < 30.1                                          
061305           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
061306                         WS-KVOI-PLOCK * 80 / 40                          
061307         ELSE                                                             
061308           IF WS-KVOI-MONTH  < 100.1                                      
061309             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
061310                         WS-KVOI-PLOCK * 50 / 40                          
061311           ELSE                                                           
061312             IF WS-KVOI-MONTH < 1000.1                                    
061313               COMPUTE WS-KVREFBER-PLOCK ROUNDED =                        
061314                         WS-KVOI-PLOCK * 30 / 40                          
061315             ELSE                                                         
061316               COMPUTE WS-KVREFBER-PLOCK ROUNDED =                        
061317                         WS-KVOI-PLOCK * 10 / 40                          
061318             END-IF                                                       
061319           END-IF                                                         
061320         END-IF                                                           
061321       END-IF                                                             
061322     ELSE                                                                 
061323*                                                                         
061324       IF CLAG-PRARTSTD < 30.01                                           
061325         IF WS-KVOI-MONTH < 10.1                                          
061326           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
061327                              WS-KVOI-PLOCK * 40 / 40                     
061328          ELSE                                                            
061329           IF WS-KVOI-MONTH < 30.1                                        
061330             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
061331                              WS-KVOI-PLOCK * 30 / 40                     
061332           ELSE                                                           
061333             IF WS-KVOI-MONTH < 100.1                                     
061334               COMPUTE WS-KVREFBER-PLOCK ROUNDED =                        
061335                              WS-KVOI-PLOCK * 15 / 40                     
061336             ELSE                                                         
061337               IF WS-KVOI-MONTH < 1000.1                                  
061338                 COMPUTE WS-KVREFBER-PLOCK ROUNDED =                      
061339                              WS-KVOI-PLOCK * 10 / 40                     
061340               ELSE                                                       
061341                 COMPUTE WS-KVREFBER-PLOCK ROUNDED =                      
061342                              WS-KVOI-PLOCK * 5 / 40                      
061343               END-IF                                                     
061344             END-IF                                                       
061345           END-IF                                                         
061346         END-IF                                                           
061347       ELSE                                                               
061348*                                                                         
061349         IF CLAG-PRARTSTD < 300.01                                        
061350           IF WS-KVOI-MONTH < 10.1                                        
061351             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
061352                              WS-KVOI-PLOCK * 20 / 40                     
061353           ELSE                                                           
061354             IF WS-KVOI-MONTH < 30.1                                      
061355               COMPUTE WS-KVREFBER-PLOCK ROUNDED =                        
061356                              WS-KVOI-PLOCK * 15 / 40                     
061357             ELSE                                                         
061358               IF WS-KVOI-MONTH < 100.1                                   
061359                 COMPUTE WS-KVREFBER-PLOCK ROUNDED =                      
061360                              WS-KVOI-PLOCK * 10 / 40                     
061361               ELSE                                                       
061362                 IF WS-KVOI-MONTH < 1000.1                                
061363                   COMPUTE WS-KVREFBER-PLOCK ROUNDED =                    
061364                              WS-KVOI-PLOCK * 7 / 40                      
061365                 ELSE                                                     
061366                   COMPUTE WS-KVREFBER-PLOCK ROUNDED =                    
061367                              WS-KVOI-PLOCK * 4 / 40                      
061368                 END-IF                                                   
061369               END-IF                                                     
061370             END-IF                                                       
061371           END-IF                                                         
061372         ELSE                                                             
061373*          > 300                                                          
061374          IF WS-KVOI-MONTH < 10.1                                         
061375            COMPUTE WS-KVREFBER-PLOCK ROUNDED =                           
061376                              WS-KVOI-PLOCK * 10 / 40                     
061377          ELSE                                                            
061378            IF WS-KVOI-MONTH < 30.1                                       
061379              COMPUTE WS-KVREFBER-PLOCK ROUNDED =                         
061380                              WS-KVOI-PLOCK * 7 / 40                      
061381            ELSE                                                          
061382              IF WS-KVOI-MONTH < 100.1                                    
061383                COMPUTE WS-KVREFBER-PLOCK ROUNDED =                       
061384                              WS-KVOI-PLOCK * 5 / 40                      
061385              ELSE                                                        
061386                IF WS-KVOI-MONTH < 1000.1                                 
061387                  COMPUTE WS-KVREFBER-PLOCK ROUNDED =                     
061388                              WS-KVOI-PLOCK * 3 / 40                      
061389                ELSE                                                      
061390                  COMPUTE WS-KVREFBER-PLOCK ROUNDED =                     
061391                              WS-KVOI-PLOCK * 2.5 / 40                    
061392                END-IF                                                    
061393              END-IF                                                      
061394            END-IF                                                        
061395          END-IF                                                          
061396         END-IF                                                           
061397       END-IF                                                             
061398     END-IF                                                               
061399*                                                                         
061400     IF CLAG-ADGANG > 1 AND CLAG-ADGANG < 8                               
061500       IF CLAG-KVQPACK-3 > 1                                              
061600         COMPUTE WS-KVREFPKT-PLOCK = CLAG-KVMAXPL - CLAG-KVQPACK-3        
061700         IF WS-KVREFPKT-PLOCK < ZERO                                      
061800           MOVE ZERO TO WS-KVREFPKT-PLOCK                                 
061900         END-IF                                                           
062000         MOVE CLAG-KVQPACK-3 TO WS-KVREFBER-PLOCK                         
062100       ELSE                                                               
062200         IF CLAG-KVMAXPL > 0                                              
062300           COMPUTE WS-KVREFPKT-PLOCK = 0.2 * CLAG-KVMAXPL                 
062400           COMPUTE WS-KVREFBER-PLOCK = 0.8 * CLAG-KVMAXPL                 
062500         END-IF                                                           
062600       END-IF                                                             
062700     END-IF                                                               
062701                                                                          
062702     IF CLAG-ADGANG = 2 OR 7                                              
062703       MOVE CLAG-ADPLATS TO WS-ADPLATS                                    
062704       IF (CLAG-ADGANG = 2 AND (CLAG-ADPLATS > 104                        
062705                          AND CLAG-ADPLATS < 136)                         
062706                          AND (WS-ADPLATS (5:1) =                         
062707                              1 OR 3 OR 5 OR 7 OR 9)) OR                  
062709          (CLAG-ADGANG = 7 AND (CLAG-ADPLATS > 390                        
062710                          AND CLAG-ADPLATS < 470)                         
062711                          AND (WS-ADPLATS (5:1) =                         
062712                              1 OR 3 OR 5 OR 7 OR 9))                     
062713         IF CLAG-KVQPACK-3 > 1 AND CLAG-KVMAXPL > 0                       
062714           COMPUTE WS-KVREFPKT-PLOCK = CLAG-KVMAXPL -                     
062715                                       (CLAG-KVQPACK-3 * 2)               
062716           IF WS-KVREFPKT-PLOCK < ZERO                                    
062717             MOVE ZERO TO WS-KVREFPKT-PLOCK                               
062718           END-IF                                                         
062719           COMPUTE WS-KVREFBER-PLOCK = CLAG-KVQPACK-3 * 2                 
062720         END-IF                                                           
062721       END-IF                                                             
062722     END-IF                                                               
062726     MOVE WS-KVOI-OVR        TO CLAG-KVOI-OVR                             
062727     MOVE WS-KVREFPKT-PLOCK  TO CLAG-KVREFPKT-PLOCK                       
062728     MOVE WS-KVREFBER-PLOCK  TO CLAG-KVREFBER-PLOCK                       
062729     .                                                                    
062730     EJECT                                                                
062731* CALCULATION FOR ADDRESS 21-24 + 13                                      
062732 CF-CALCULATE-LAG-L2 SECTION.                                             
062733     COMPUTE WS-KVREFPKT-PLOCK ROUNDED = WS-KVOI-PLOCK * 0.8 / 40         
062734     COMPUTE WS-KVOI-MONTH ROUNDED = WS-KVOI-PLOCK / 2                    
062735     IF ((CLAG-PRARTSTD < 3.01                                            
062736                        AND WS-KVOI-MONTH < 10.1) OR                      
062737         (CLAG-PRARTSTD < 30.01                                           
062740                        AND WS-KVOI-MONTH < 10.1) OR                      
063000         (CLAG-PRARTSTD < 300.01                                          
064000                        AND WS-KVOI-MONTH < 10.1) OR                      
065000         (CLAG-PRARTSTD > 300.01                                          
066000                        AND WS-KVOI-MONTH < 10.1))                        
067000        COMPUTE WS-KVREFBER-PLOCK ROUNDED =                               
068000                         WS-KVOI-PLOCK * 10 / 40                          
069000     ELSE                                                                 
070000       IF ((CLAG-PRARTSTD < 3.01                                          
071000                          AND WS-KVOI-MONTH < 30.1) OR                    
072000           (CLAG-PRARTSTD < 30.01                                         
073000                          AND WS-KVOI-MONTH < 30.1) OR                    
074000           (CLAG-PRARTSTD < 300.01                                        
075000                          AND WS-KVOI-MONTH < 30.1) OR                    
076000           (CLAG-PRARTSTD > 300.01                                        
077000                          AND WS-KVOI-MONTH < 30.1))                      
078000           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
078100                         WS-KVOI-PLOCK * 10 / 40                          
078201       ELSE                                                               
078301         IF ((CLAG-PRARTSTD < 3.01                                        
078401                            AND WS-KVOI-MONTH < 100.1) OR                 
078501             (CLAG-PRARTSTD < 30.01                                       
078601                            AND WS-KVOI-MONTH < 100.1) OR                 
078701             (CLAG-PRARTSTD < 300.01                                      
078801                            AND WS-KVOI-MONTH < 100.1) OR                 
078901             (CLAG-PRARTSTD > 300.01                                      
079001                            AND WS-KVOI-MONTH < 100.1))                   
079101           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
079201                       WS-KVOI-PLOCK *  5 / 40                            
079301         ELSE                                                             
079401           IF ((CLAG-PRARTSTD < 3.01                                      
079501                              AND WS-KVOI-MONTH < 1000.1) OR              
079601               (CLAG-PRARTSTD < 30.01                                     
079701                              AND WS-KVOI-MONTH < 1000.1) OR              
079801               (CLAG-PRARTSTD < 300.01                                    
079901                              AND WS-KVOI-MONTH < 1000.1) OR              
080001               (CLAG-PRARTSTD > 300.01                                    
080101                              AND WS-KVOI-MONTH < 1000.1))                
080201             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
080301                       WS-KVOI-PLOCK *  3 / 40                            
080401           ELSE                                                           
080501             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
080601                       WS-KVOI-PLOCK * 2 / 40                             
080701           END-IF                                                         
080900         END-IF                                                           
081000       END-IF                                                             
081100     END-IF                                                               
081200*                                                                         
081300     IF CLAG-ADLAGOMR = 21 OR                                             
081310        CLAG-ADLAGOMR = 22 AND CLAG-ADGANG > 8 AND                        
081400        CLAG-ADGANG < 39                                                  
081500       IF CLAG-KVQPACK-3 > 1                                              
081600         COMPUTE WS-KVREFPKT-PLOCK = CLAG-KVMAXPL - CLAG-KVQPACK-3        
081700         IF WS-KVREFPKT-PLOCK < ZERO                                      
081800           MOVE ZERO TO WS-KVREFPKT-PLOCK                                 
081900         END-IF                                                           
082000         MOVE CLAG-KVQPACK-3 TO WS-KVREFBER-PLOCK                         
082100       ELSE                                                               
082200         IF CLAG-KVMAXPL > 0                                              
082300           COMPUTE WS-KVREFPKT-PLOCK = 0.2 * CLAG-KVMAXPL                 
082400           COMPUTE WS-KVREFBER-PLOCK = 0.8 * CLAG-KVMAXPL                 
082500         END-IF                                                           
082600       END-IF                                                             
082601     END-IF                                                               
082602*                                                                         
082603                                                                          
082604     MOVE WS-KVOI-OVR        TO CLAG-KVOI-OVR                             
082605     MOVE WS-KVOI-PLOCK      TO CLAG-KVOI-PLOCK                           
082606     MOVE WS-KVREFPKT-PLOCK  TO CLAG-KVREFPKT-PLOCK                       
082607     MOVE WS-KVREFBER-PLOCK  TO CLAG-KVREFBER-PLOCK                       
087995     .                                                                    
087996     EJECT                                                                
087997* CALCULATION FOR ADDRESS 30-33                                           
087998 CG-CALCULATE-LAG-L3 SECTION.                                             
087999     COMPUTE WS-KVREFPKT-PLOCK ROUNDED = WS-KVOI-PLOCK * 2 / 40           
088000     COMPUTE WS-KVOI-MONTH ROUNDED = WS-KVOI-PLOCK / 2                    
088001                                                                          
088002     IF ((CLAG-PRARTSTD < 3.01                                            
088003                        AND WS-KVOI-MONTH < 10.1) OR                      
088004         (CLAG-PRARTSTD < 30.01                                           
088005                        AND WS-KVOI-MONTH < 10.1) OR                      
088006         (CLAG-PRARTSTD < 300.01                                          
088007                        AND WS-KVOI-MONTH < 10.1) OR                      
088008         (CLAG-PRARTSTD > 300.01                                          
088009                        AND WS-KVOI-MONTH < 10.1))                        
088010        COMPUTE WS-KVREFBER-PLOCK ROUNDED =                               
088011                         WS-KVOI-PLOCK * 20 / 40                          
088012     ELSE                                                                 
088013       IF ((CLAG-PRARTSTD < 3.01                                          
088014                          AND WS-KVOI-MONTH < 30.1) OR                    
088015           (CLAG-PRARTSTD < 30.01                                         
088016                          AND WS-KVOI-MONTH < 30.1) OR                    
088017           (CLAG-PRARTSTD < 300.01                                        
088018                          AND WS-KVOI-MONTH < 30.1) OR                    
088019           (CLAG-PRARTSTD > 300.01                                        
088020                          AND WS-KVOI-MONTH < 30.1))                      
088021           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
088022                         WS-KVOI-PLOCK * 15 / 40                          
088023       ELSE                                                               
088024         IF ((CLAG-PRARTSTD < 3.01                                        
088025                            AND WS-KVOI-MONTH < 100.1) OR                 
088026             (CLAG-PRARTSTD < 30.01                                       
088027                            AND WS-KVOI-MONTH < 100.1) OR                 
088028             (CLAG-PRARTSTD < 300.01                                      
088029                            AND WS-KVOI-MONTH < 100.1) OR                 
088030             (CLAG-PRARTSTD > 300.01                                      
088031                            AND WS-KVOI-MONTH < 100.1))                   
088032           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
088033                       WS-KVOI-PLOCK * 10 / 40                            
088034         ELSE                                                             
088035           IF ((CLAG-PRARTSTD < 3.01                                      
088036                              AND WS-KVOI-MONTH < 1000.1) OR              
088037               (CLAG-PRARTSTD < 30.01                                     
088038                              AND WS-KVOI-MONTH < 1000.1) OR              
088039               (CLAG-PRARTSTD < 300.01                                    
088040                              AND WS-KVOI-MONTH < 1000.1) OR              
088041               (CLAG-PRARTSTD > 300.01                                    
088042                              AND WS-KVOI-MONTH < 1000.1))                
088043             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
088044                       WS-KVOI-PLOCK *  7 / 40                            
088045           ELSE                                                           
088046             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
088047                       WS-KVOI-PLOCK * 4 / 40                             
088048           END-IF                                                         
088049         END-IF                                                           
088050       END-IF                                                             
088051     END-IF                                                               
088052*                                                                         
088053     IF CLAG-ADLAGOMR = 31 AND CLAG-ADGANG > 39 AND                       
088054                               CLAG-ADGANG < 42                           
088055       IF CLAG-KVQPACK-3 > 1 AND CLAG-KVMAXPL > 0                         
088056         COMPUTE WS-KVREFPKT-PLOCK = CLAG-KVMAXPL -                       
088057                                     (CLAG-KVQPACK-3 * 2)                 
088058         IF WS-KVREFPKT-PLOCK < ZERO                                      
088059           MOVE ZERO TO WS-KVREFPKT-PLOCK                                 
088060         END-IF                                                           
088061         COMPUTE WS-KVREFBER-PLOCK = CLAG-KVQPACK-3 * 2                   
088062       END-IF                                                             
088063     END-IF                                                               
088064                                                                          
088065     MOVE WS-KVOI-OVR        TO CLAG-KVOI-OVR                             
088066     MOVE WS-KVOI-PLOCK      TO CLAG-KVOI-PLOCK                           
088067     MOVE WS-KVREFPKT-PLOCK  TO CLAG-KVREFPKT-PLOCK                       
088070     MOVE WS-KVREFBER-PLOCK  TO CLAG-KVREFBER-PLOCK                       
088073     .                                                                    
088074     EJECT                                                                
088100* CALCULATION FOR ADDRESS 74+72                                           
088200 CH-CALCULATE-LAG-L4 SECTION.                                             
088300     COMPUTE WS-KVREFPKT-PLOCK ROUNDED = WS-KVOI-PLOCK * 3 / 40           
088400     COMPUTE WS-KVOI-MONTH ROUNDED = WS-KVOI-PLOCK / 2                    
088500                                                                          
088600     IF ((CLAG-PRARTSTD < 3.01   AND WS-KVOI-MONTH < 10.1) OR             
088700         (CLAG-PRARTSTD < 30.01  AND WS-KVOI-MONTH < 10.1) OR             
088900         (CLAG-PRARTSTD < 300.01 AND WS-KVOI-MONTH < 10.1) OR             
089100         (CLAG-PRARTSTD > 300.01 AND WS-KVOI-MONTH < 10.1) OR             
089304                                                                          
089600         (CLAG-PRARTSTD < 3.01   AND WS-KVOI-MONTH < 30.1) OR             
089800         (CLAG-PRARTSTD < 30.01  AND WS-KVOI-MONTH < 30.1) OR             
090000         (CLAG-PRARTSTD < 300.01 AND WS-KVOI-MONTH < 30.1) OR             
090200         (CLAG-PRARTSTD > 300.01 AND WS-KVOI-MONTH < 30.1) OR             
090400                                                                          
090700         (CLAG-PRARTSTD < 3.01   AND WS-KVOI-MONTH < 100.1) OR            
090900         (CLAG-PRARTSTD < 30.01  AND WS-KVOI-MONTH < 100.1) OR            
091100         (CLAG-PRARTSTD < 300.01 AND WS-KVOI-MONTH < 100.1) OR            
091300         (CLAG-PRARTSTD > 300.01 AND WS-KVOI-MONTH < 100.1) OR            
091500                                                                          
091800         (CLAG-PRARTSTD < 3.01   AND WS-KVOI-MONTH < 1000.1) OR           
092000         (CLAG-PRARTSTD < 30.01  AND WS-KVOI-MONTH < 1000.1) OR           
092204         (CLAG-PRARTSTD < 300.01 AND WS-KVOI-MONTH < 1000.1) OR           
092304         (CLAG-PRARTSTD > 300.01 AND WS-KVOI-MONTH < 1000.1) OR           
092404                                                                          
092504         (CLAG-PRARTSTD < 3.01   AND WS-KVOI-MONTH > 1000.1) OR           
092604         (CLAG-PRARTSTD < 30.01  AND WS-KVOI-MONTH > 1000.1) OR           
092704         (CLAG-PRARTSTD < 300.01 AND WS-KVOI-MONTH > 1000.1))             
092904        COMPUTE WS-KVREFBER-PLOCK ROUNDED =                               
093004                         WS-KVOI-PLOCK * 20 / 40                          
093104     ELSE                                                                 
093204       IF CLAG-PRARTSTD > 300.01 AND WS-KVOI-MONTH > 1000                 
093302         COMPUTE WS-KVREFBER-PLOCK ROUNDED =                              
093400                         WS-KVOI-PLOCK * 2.5 / 40                         
093902       END-IF                                                             
094002     END-IF                                                               
094102*                                                                         
094203     MOVE WS-KVOI-OVR        TO CLAG-KVOI-OVR                             
094302     MOVE WS-KVOI-PLOCK      TO CLAG-KVOI-PLOCK                           
094402     MOVE WS-KVREFPKT-PLOCK  TO CLAG-KVREFPKT-PLOCK                       
094502     MOVE WS-KVREFBER-PLOCK  TO CLAG-KVREFBER-PLOCK                       
094602     .                                                                    
094702     EJECT                                                                
094703* CALCULATION FOR ADDRESS 23                                              
094704 CI-CALCULATE-LAG-L5 SECTION.                                             
094705     COMPUTE WS-KVREFPKT-PLOCK ROUNDED = WS-KVOI-PLOCK * 0.8 / 40         
094706     COMPUTE WS-KVOI-MONTH ROUNDED = WS-KVOI-PLOCK / 2                    
094707                                                                          
094708     IF WS-KVOI-MONTH < 10.1                                              
094709        PERFORM CIA-CALC-FREQ10                                           
094710     ELSE                                                                 
094720        IF WS-KVOI-MONTH < 30.1                                           
094730           PERFORM CIB-CALC-FREQ30                                        
094740        ELSE                                                              
094750           IF WS-KVOI-MONTH < 100.1                                       
094760              PERFORM CIC-CALC-FREQ100                                    
094770           ELSE                                                           
094780              IF WS-KVOI-MONTH < 1000.1                                   
094790                 PERFORM CID-CALC-FREQ1000                                
094800              ELSE                                                        
094801                 PERFORM CIE-CALC-FREQ1000-1                              
094802              END-IF                                                      
094803           END-IF                                                         
094804        END-IF                                                            
094805     END-IF                                                               
094806*                                                                         
094807     MOVE WS-KVOI-OVR        TO CLAG-KVOI-OVR                             
094808     MOVE WS-KVOI-PLOCK      TO CLAG-KVOI-PLOCK                           
094809     MOVE WS-KVREFPKT-PLOCK  TO CLAG-KVREFPKT-PLOCK                       
094810     MOVE WS-KVREFBER-PLOCK  TO CLAG-KVREFBER-PLOCK                       
094811     .                                                                    
094812     EJECT                                                                
094813 CIA-CALC-FREQ10 SECTION.                                                 
094814                                                                          
094815     EVALUATE TRUE                                                        
094816       WHEN CLAG-PRARTSTD < 3.01                                          
094817          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094818                           WS-KVOI-PLOCK * 10 / 40                        
094819       WHEN CLAG-PRARTSTD < 30.01                                         
094820          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094821                           WS-KVOI-PLOCK * 10 / 40                        
094822       WHEN CLAG-PRARTSTD < 300.01                                        
094823          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094824                           WS-KVOI-PLOCK * 10 / 40                        
094825       WHEN CLAG-PRARTSTD > 300.01                                        
094826          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094827                           WS-KVOI-PLOCK * 10 / 40                        
094828     END-EVALUATE                                                         
094829     .                                                                    
094830     EJECT                                                                
094831 CIB-CALC-FREQ30 SECTION.                                                 
094832                                                                          
094833     EVALUATE TRUE                                                        
094834       WHEN CLAG-PRARTSTD < 3.01                                          
094835          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094836                           WS-KVOI-PLOCK * 10 / 40                        
094837       WHEN CLAG-PRARTSTD < 30.01                                         
094838          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094839                           WS-KVOI-PLOCK * 10 / 40                        
094840       WHEN CLAG-PRARTSTD < 300.01                                        
094841          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094842                           WS-KVOI-PLOCK * 10 / 40                        
094843       WHEN CLAG-PRARTSTD > 300.01                                        
094844          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094845                           WS-KVOI-PLOCK * 10 / 40                        
094846     END-EVALUATE                                                         
094847     .                                                                    
094848     EJECT                                                                
094849 CIC-CALC-FREQ100 SECTION.                                                
094850                                                                          
094851     EVALUATE TRUE                                                        
094852       WHEN CLAG-PRARTSTD < 3.01                                          
094853          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094854                           WS-KVOI-PLOCK * 5 / 40                         
094855       WHEN CLAG-PRARTSTD < 30.01                                         
094856          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094857                           WS-KVOI-PLOCK * 5 / 40                         
094858       WHEN CLAG-PRARTSTD < 300.01                                        
094859          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094860                           WS-KVOI-PLOCK * 5 / 40                         
094861       WHEN CLAG-PRARTSTD > 300.01                                        
094862          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094863                           WS-KVOI-PLOCK * 5 / 40                         
094864     END-EVALUATE                                                         
094865     .                                                                    
094866     EJECT                                                                
094867 CID-CALC-FREQ1000 SECTION.                                               
094868                                                                          
094869     EVALUATE TRUE                                                        
094870       WHEN CLAG-PRARTSTD < 3.01                                          
094871          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094872                           WS-KVOI-PLOCK * 3 / 40                         
094873       WHEN CLAG-PRARTSTD < 30.01                                         
094874          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094875                           WS-KVOI-PLOCK * 3 / 40                         
094876       WHEN CLAG-PRARTSTD < 300.01                                        
094877          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094878                           WS-KVOI-PLOCK * 3 / 40                         
094879       WHEN CLAG-PRARTSTD > 300.01                                        
094880          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094881                           WS-KVOI-PLOCK * 3 / 40                         
094882     END-EVALUATE                                                         
094883     .                                                                    
094884     EJECT                                                                
094885 CIE-CALC-FREQ1000-1 SECTION.                                             
094886                                                                          
094887     EVALUATE TRUE                                                        
094888       WHEN CLAG-PRARTSTD < 3.01                                          
094889          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094890                           WS-KVOI-PLOCK * 2 / 40                         
094891       WHEN CLAG-PRARTSTD < 30.01                                         
094892          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094893                           WS-KVOI-PLOCK *  2 / 40                        
094894       WHEN CLAG-PRARTSTD < 300.01                                        
094895          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094896                           WS-KVOI-PLOCK *  2 / 40                        
094897       WHEN CLAG-PRARTSTD > 300.01                                        
094898          COMPUTE WS-KVREFBER-PLOCK ROUNDED =                             
094899                           WS-KVOI-PLOCK *  2 / 40                        
094900     END-EVALUATE                                                         
094901     .                                                                    
094902     EJECT                                                                
094903* CALCULATION FOR ALL OTHER ADDRESSES                                     
094904 CJ-CALCULATE-LAG-L6 SECTION.                                             
095002     COMPUTE WS-KVREFPKT-PLOCK ROUNDED = WS-KVOI-PLOCK * 2 / 40           
095102     COMPUTE WS-KVOI-MONTH ROUNDED = WS-KVOI-PLOCK / 2                    
095202                                                                          
095302     IF ((CLAG-PRARTSTD < 3.01                                            
095402                        AND WS-KVOI-MONTH < 10.1) OR                      
095502         (CLAG-PRARTSTD < 30.01                                           
095602                        AND WS-KVOI-MONTH < 10.1) OR                      
095702         (CLAG-PRARTSTD < 300.01                                          
095802                        AND WS-KVOI-MONTH < 10.1) OR                      
095902         (CLAG-PRARTSTD > 300.01                                          
096002                        AND WS-KVOI-MONTH < 10.1))                        
096102        COMPUTE WS-KVREFBER-PLOCK ROUNDED =                               
096202                         WS-KVOI-PLOCK * 20 / 40                          
096302     ELSE                                                                 
096402       IF ((CLAG-PRARTSTD < 3.01                                          
096502                          AND WS-KVOI-MONTH < 30.1) OR                    
096602           (CLAG-PRARTSTD < 30.01                                         
096702                          AND WS-KVOI-MONTH < 30.1) OR                    
096802           (CLAG-PRARTSTD < 300.01                                        
096902                          AND WS-KVOI-MONTH < 30.1) OR                    
097002           (CLAG-PRARTSTD > 300.01                                        
097102                          AND WS-KVOI-MONTH < 30.1))                      
097202           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
097302                         WS-KVOI-PLOCK * 15 / 40                          
097402       ELSE                                                               
097502         IF ((CLAG-PRARTSTD < 3.01                                        
097602                            AND WS-KVOI-MONTH < 100.1) OR                 
097702             (CLAG-PRARTSTD < 30.01                                       
097802                            AND WS-KVOI-MONTH < 100.1) OR                 
097902             (CLAG-PRARTSTD < 300.01                                      
098002                            AND WS-KVOI-MONTH < 100.1) OR                 
098102             (CLAG-PRARTSTD > 300.01                                      
098202                            AND WS-KVOI-MONTH < 100.1))                   
098302           COMPUTE WS-KVREFBER-PLOCK ROUNDED =                            
098402                       WS-KVOI-PLOCK * 10 / 40                            
098502         ELSE                                                             
098602           IF ((CLAG-PRARTSTD < 3.01                                      
098702                              AND WS-KVOI-MONTH < 1000.1) OR              
098802               (CLAG-PRARTSTD < 30.01                                     
098902                              AND WS-KVOI-MONTH < 1000.1) OR              
099002               (CLAG-PRARTSTD < 300.01                                    
099102                              AND WS-KVOI-MONTH < 1000.1) OR              
099202               (CLAG-PRARTSTD > 300.01                                    
099302                              AND WS-KVOI-MONTH < 1000.1))                
099402             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
099502                       WS-KVOI-PLOCK *  7 / 40                            
099602           ELSE                                                           
099702             COMPUTE WS-KVREFBER-PLOCK ROUNDED =                          
099802                       WS-KVOI-PLOCK * 4 / 40                             
099902           END-IF                                                         
100000         END-IF                                                           
100100       END-IF                                                             
100200     END-IF                                                               
100300*                                                                         
100400     IF CLAG-ADLAGOMR = 20                                                
100600       IF CLAG-KVQPACK-3 > 1 AND CLAG-KVMAXPL > 0                         
100700         COMPUTE WS-KVREFPKT-PLOCK = CLAG-KVMAXPL -                       
100710                                     (CLAG-KVQPACK-3 * 2)                 
100800         IF WS-KVREFPKT-PLOCK < ZERO                                      
100900           MOVE ZERO TO WS-KVREFPKT-PLOCK                                 
101000         END-IF                                                           
101010         COMPUTE WS-KVREFBER-PLOCK = CLAG-KVQPACK-3 * 2                   
101200       END-IF                                                             
101701     END-IF                                                               
101717*                                                                         
101718     MOVE WS-KVOI-OVR        TO CLAG-KVOI-OVR                             
101719     MOVE WS-KVOI-PLOCK      TO CLAG-KVOI-PLOCK                           
101720     MOVE WS-KVREFPKT-PLOCK  TO CLAG-KVREFPKT-PLOCK                       
101721     MOVE WS-KVREFBER-PLOCK  TO CLAG-KVREFBER-PLOCK                       
101722                                                                          
101739     .                                                                    
101740     EJECT                                                                
101741 CCA-SKRIV-UTFIL  SECTION.                                                
101742     MOVE WS-IDARTNR         TO UT-IDARTNR                                
101743     MOVE WS-KVOI-OVR        TO UT-KVOI-OVR                               
101744     MOVE WS-KVREFPKT-PLOCK  TO UT-KVREFPKT-PLOCK                         
101750     MOVE WS-KVREFBER-PLOCK  TO UT-KVREFBER-PLOCK                         
101800     WRITE OUTPOST       FROM UT-POST                                     
101900     .                                                                    
102000     EJECT                                                                
102100 X-TAG-CHECKPOINT  SECTION.                                               
102200     PERFORM IMS-CHECKPOINT                                               
102300     MOVE ZERO       TO CHKP-ANT                                          
102400     .                                                                    
102500     EJECT                                                                
102600 Z-FINIT SECTION.                                                         
102700                                                                          
102800     CLOSE INFILS                                                         
102900     CLOSE UTFIL                                                          
103000     SKIP2                                                                
103100     MOVE 'S' TO POSTSUM-OPKOD                                            
103200     CALL POSTSUM USING POSTSUM-PARM                                      
103300     .                                                                    
103400     EJECT                                                                
103500 S01-READ-INFIL   SECTION.                                                
103600     SKIP2                                                                
103700     READ INFIL  INTO HIST-AREA                                           
103800     AT END                                                               
103900*       MOVE HIGH-VALUE TO HIST-IDDISTR                                   
104000        SET END-OF-INFIL  TO TRUE                                         
104100                                                                          
104200     NOT AT END                                                           
104300        MOVE 'WXTRB4'     TO POSTSUM-FDNAMN                               
104400        MOVE 'W61320D1'   TO POSTSUM-DDNAMN2                              
104500        MOVE SPACE        TO POSTSUM-TRANSTYP                             
104600        CALL POSTSUM USING POSTSUM-PARM                                   
104700     END-READ                                                             
104800     .                                                                    
104900     EJECT                                                                
105000 S02-READ-INFILS  SECTION.                                                
105100     SKIP2                                                                
105200     READ INFILS INTO SORT-POST                                           
105300     AT END                                                               
105400*       MOVE HIGH-VALUE TO SORT-IDARTNR                                   
105500        SET END-OF-INFILS TO TRUE                                         
105600                                                                          
105700     NOT AT END                                                           
105800        MOVE 'WSORTA'     TO POSTSUM-FDNAMN                               
105900        MOVE 'W61320D3'   TO POSTSUM-DDNAMN2                              
106000        MOVE SPACE        TO POSTSUM-TRANSTYP                             
106100        CALL POSTSUM USING POSTSUM-PARM                                   
106200     END-READ                                                             
106300     .                                                                    
106400     EJECT                                                                
106500 S99-ABEND SECTION.                                                       
106600     MOVE 'S' TO POSTSUM-OPKOD                                            
106700     CALL POSTSUM USING POSTSUM-PARM                                      
106800     CALL ABEND USING RKOD-ABEND                                          
106900     .                                                                    
107000     EJECT                                                                
107100* --- IMS SECTIONS  ---                                                   
107200 IMS-RESTART SECTION.                                                     
107300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
107400     MOVE '  ' TO GOOD-STATUSCODES                                        
107500     CALL CBLTDLI USING XRST MSG-PCB                                      
107600       CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA                           
107700       CHKP-AREA-LENGTH CHKP-AREA                                         
107800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107900     PERFORM IMS-STATUSCHECK                                              
108000     .                                                                    
108100     SKIP3                                                                
108200 IMS-CHECKPOINT SECTION.                                                  
108300                                                                          
108400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
108500     MOVE '  XD' TO GOOD-STATUSCODES                                      
108600     CALL CBLTDLI USING CHKP MSG-PCB                                      
108700         CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA                         
108800         CHKP-AREA-LENGTH CHKP-AREA                                       
108900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109000     PERFORM IMS-STATUSCHECK                                              
109100                                                                          
109200     IF IMS-NOT-OK                                                        
109300      MOVE 'IMS-KONTROLLREGION EJ ÄNGLIG' TO ERRTEXT                      
109400      DISPLAY ERRTEXT                                                     
109500      CALL FELLOG                                                         
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900                                                                          
111000 IMS-GHU-WDK611 SECTION.                                                  
111100                                                                          
111200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
111300          DELIMITED BY SIZE INTO SSA1                                     
111400     MOVE 'WDK611   '         TO SSA2                                     
111500     MOVE '    '              TO GOOD-STATUSCODES                         
111600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
111700     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
111800     PERFORM IMS-STATUSCHECK                                              
111900     .                                                                    
112000     SKIP2                                                                
112100 IMS-REPL-WDK611 SECTION.                                                 
112200                                                                          
112300     MOVE '    '               TO GOOD-STATUSCODES                        
112400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
112500     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
112600     PERFORM IMS-STATUSCHECK                                              
112700     .                                                                    
112800     SKIP2                                                                
112900 IMS-STATUSCHECK SECTION.                                                 
113000     SKIP2                                                                
113100     SET STATUS-IX TO 1                                                   
113200     SEARCH GOOD-STATUS                                                   
113300       AT END                                                             
113400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
113500           DELIMITED BY SIZE INTO ERRTEXT                                 
113600         DISPLAY ERRTEXT                                                  
113700         CALL FELLOG                                                      
113800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
113900         CONTINUE                                                         
114000     END-SEARCH                                                           
120000     .                                                                    
