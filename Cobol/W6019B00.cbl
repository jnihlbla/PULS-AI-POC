000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6019B00.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/08/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      BAKGRUNDS MPP SOM STARTAS UPP VIA DISPATCHEN, NÄR ARTIKEL-         
001100*      INFORMATION ÄR ÄNDRAD PÅ BILD 6163, 6164, 6304 ELLER 2124.         
001200*      PARTIER MED AKTUELL ARTIKEL OCH FLKLAR = N UPPDATERAS              
001300*      PÅ W6D1.                                                           
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001600*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T19B                                              
002000*        MID:         W6I19B01                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         SVAR TILL DISPATCHEN                                
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6019B00'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X        VALUE 'J'.                  
003800 77  NEJ                         PIC X        VALUE 'N'.                  
003900 01  EMB-IX                      PIC S9(4)    VALUE +0 COMP SYNC.         
004000 01  CD-IX                       PIC  9(1)    VALUE ZERO.                 
004100 01  CD-HELP                     PIC  9(1)    VALUE ZERO.                 
004200                                                                          
004300 77  W-IDTRANS                   PIC X(4)     VALUE SPACE.                
004400     88  EGEN-MID                             VALUE '619B'.               
004500     88  GODK-MID                             VALUE '619B'.               
004600                                                                          
004700 77  W-IDFTG                    PIC S9(3)     VALUE +0 COMP-3.            
004800 77  W-ART-FLFEL                PIC X         VALUE 'N'.                  
004900                                                                          
005000 01  WS-VLARTNTO             PIC 9(8)V9(1).                               
005100 01  WS-VLARTNTO-XX  REDEFINES WS-VLARTNTO.                               
005200     05 WS-VLARTNTO-ALFA     PIC X(9).                                    
005300                                                                          
005400*    --- SWITCHAR                                                         
005500 77  FLFEL-AENDRAD-SW           PIC X(1)      VALUE 'N'.                  
005600     88  FLFEL-AENDRAD                        VALUE 'J'.                  
005700                                                                          
005800 77  FOERSTA-INLA11-SW          PIC X(1)      VALUE 'J'.                  
005900     88  FOERSTA-INLA11                       VALUE 'J'.                  
006000                                                                          
006100 01  MESSAGE-CODES.                                                       
006200     03  INF-UPDATE-DONE         PIC X(3)     VALUE '101'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     EJECT                                                                
006900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007000*                                                                         
007100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007200     SKIP3                                                                
007300*01  MID -COPY W6I19B01                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'WMSGKOM'.             
007600     SKIP3                                                                
007700*01  -COPY WMSGKOM                                                        
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008000     SKIP3                                                                
008100*01  -COPY WMSGAREA                                                       
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008400     SKIP3                                                                
008500*01  -COPY WMFSAREA                                                       
008600     EJECT                                                                
008700*      --- EMBALLAGE-KODER MED ÖVERSÄTTNINGAR                             
008800*01    -COPY W611EMB3                                                     
008900       EJECT                                                              
009000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-W6D101KY-X.                                                    
009600         05  W-D101KY-IDDC       PIC X(2)     VALUE SPACE.                
009700         05  W-D101KY-IDLEVNR    PIC X(5)     VALUE SPACE.                
009800         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
009900         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
010000                                                                          
010100     03  W-W6D1H1KY-MIN-X.                                                
010200         05  W-D1H1KY-IDARTNR-MIN    PIC S9(9) COMP-3 VALUE ZERO.         
010300         05  W-D1H1KY-IDDC-MIN       PIC X(2)  VALUE SPACE.               
010400         05  W-D1H1KY-IDLEVNR-MIN    PIC X(5)  VALUE SPACE.               
010500         05  W-D1H1KY-IDRADNR-INL-MIN PIC S9(5) COMP-3 VALUE ZERO.        
010600         05  W-D1H1KY-IDFS-MIN       PIC X(8)  VALUE SPACE.               
010700         05  W-D1H1KY-TIAVIDAT-MIN   PIC S9(7) COMP-3 VALUE ZERO.         
010800                                                                          
010900     03  W-W6D1H1KY-MAX-X.                                                
011000         05  W-D1H1KY-IDARTNR-MAX    PIC S9(9) COMP-3 VALUE ZERO.         
011100         05  W-D1H1KY-IDDC-MAX       PIC X(2)  VALUE SPACE.               
011200         05  W-D1H1KY-IDLEVNR-MAX    PIC X(5)  VALUE SPACE.               
011300         05  W-D1H1KY-IDRADNR-INL-MAX PIC S9(5) COMP-3 VALUE ZERO.        
011400         05  W-D1H1KY-IDFS-MAX       PIC X(8)  VALUE SPACE.               
011500         05  W-D1H1KY-TIAVIDAT-MAX   PIC S9(7) COMP-3 VALUE ZERO.         
011600                                                                          
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900                                                                          
012000     03  W-IDRADNR-INL-X.                                                 
012100         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
012200                                                                          
012300     03  W-IDLEVNR-X.                                                     
012400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012500                                                                          
012600     03  W-KDSEGKEY-X.                                                    
012700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
012800                                                                          
012900     03  W-KDCLAGER-X.                                                    
013000         05  W-KDCLAGER          PIC S9(1)   VALUE +1 COMP-3.             
013100     SKIP2                                                                
013200*    --- STATUS-KOD FRÅN IMS                                              
013300 01  STATUS-WS                   PIC XX.                                  
013400     88  SEGMENT-FINNS                       VALUE '  '.                  
013500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013700     SKIP2                                                                
013800 01  GODK-STATUSKODER.                                                    
013900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000     SKIP3                                                                
014100 01  SSA1                        PIC X(128).                              
014200 01  SSA2                        PIC X(64).                               
014300     EJECT                                                                
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700*    ---  DLI INPUT-OUTPUT AREA                                           
014800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014900     SKIP3                                                                
015000 01  DLI-IO-AREA.                                                         
015100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
015200     SKIP3                                                                
015300     03  W6INLI01 REDEFINES IO-AREA.                                      
015400*        05  -COPY W6D1H1                                                 
015500     EJECT                                                                
015600     03  W6INLA01 REDEFINES IO-AREA.                                      
015700*        05  -COPY W6D111                                                 
015800     EJECT                                                                
015900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
016000     SKIP3                                                                
016100 01  DLI-IO-AREA2.                                                        
016200     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
016300     SKIP3                                                                
016400     03  WLARTC01 REDEFINES IO-AREA2.                                     
016500*        05  -COPY WDK601    -PRE ARTC-                                   
016600     EJECT                                                                
016700     03  WLARTC11 REDEFINES IO-AREA2.                                     
016800*        05  -COPY WDK611    -PRE ARTC-                                   
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA3'.         
017100     SKIP3                                                                
017200 01  DLI-IO-AREA3.                                                        
017300     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
017400     03  W6INLA01 REDEFINES IO-AREA3.                                     
017500*        05  -COPY W6D101                                                 
017600     EJECT                                                                
017700 LINKAGE SECTION.                                                         
017800                                                                          
017900*01  -COPY W0009   -PRE MSG-                                              
018000     EJECT                                                                
018100*01  -COPY W0009   -PRE DISP-                                             
018200     EJECT                                                                
018300*01  -COPY W0008  -PRE INLI-                                              
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE INLA1-                                             
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE INLA2-                                             
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE ARTC-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB INLI-PCB INLA1-PCB            
019600                                   INLA2-PCB ARTC-PCB.                    
019700     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB INLI-PCB INLA1-PCB            
019800                                   INLA2-PCB ARTC-PCB.                    
019900                                                                          
020000     PERFORM IMS-GET-MSG                                                  
020100     IF SEGMENT-FINNS                                                     
020200         PERFORM IMS-GN-MSG                                               
020300         PERFORM A-INIT                                                   
020400         PERFORM B-UPPDATERA-INLA                                         
020500     END-IF                                                               
020600                                                                          
020700     MOVE INF-UPDATE-DONE        TO MSG-KOM-IDMFSMED                      
020800     PERFORM IMS-ISRT-DISP-MSG                                            
020900                                                                          
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 A-INIT SECTION.                                                          
021500                                                                          
021600     MOVE NEJ                             TO FLFEL-AENDRAD-SW             
021700     MOVE JA                              TO FOERSTA-INLA11-SW            
021800     IF MSG-DUBBLA-TRANSKODER                                             
021900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I19B01                 
022000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
022100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
022200     ELSE                                                                 
022300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I19B01                 
022400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
022500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
022600     END-IF                                                               
022700                                                                          
022800     MOVE LOW-VALUE                       TO MSG-AREA                     
022900     .                                                                    
023000     EJECT                                                                
023100 B-UPPDATERA-INLA    SECTION.                                             
023200                                                                          
023300     MOVE LOW-VALUE            TO W-W6D1H1KY-MIN-X                        
023400     MOVE HIGH-VALUE           TO W-W6D1H1KY-MAX-X                        
023500     MOVE MID-IDARTNR          TO W-D1H1KY-IDARTNR-MIN                    
023600                                  W-D1H1KY-IDARTNR-MAX                    
023700                                  W-IDARTNR                               
023800     MOVE MID-IDDC             TO W-D1H1KY-IDDC-MIN                       
023900                                  W-D1H1KY-IDDC-MAX                       
024000     PERFORM IMS-GU-INLI-INLI01                                           
024100     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
024200         MOVE SEQH-IDDC        TO W-D101KY-IDDC                           
024300         MOVE SEQH-IDLEVNR     TO W-D101KY-IDLEVNR                        
024400                                  W-IDLEVNR                               
024500         MOVE SEQH-IDFS        TO W-D101KY-IDFS                           
024600         MOVE SEQH-TIAVIDAT    TO W-D101KY-TIAVIDAT                       
024700         MOVE SEQH-IDRADNR-INL TO W-IDRADNR-INL                           
024800         PERFORM IMS-GHU-INLA1-INLA11                                     
024900         IF ART-FLKLAR         =  NEJ                                     
025000             PERFORM BA-UPPDATERA-INLA11                                  
025100             IF FLFEL-AENDRAD                                             
025200                 PERFORM BB-BEHANDLA-FLFEL-INLA01                         
025300             END-IF                                                       
025400         END-IF                                                           
025500         PERFORM IMS-GN-INLI-INLI01                                       
025600     END-PERFORM                                                          
025700     .                                                                    
025800     EJECT                                                                
025900 BA-UPPDATERA-INLA11     SECTION.                                         
026000                                                                          
026100     IF (MID-ADLAGOMR NOT = ALL '+')        AND                           
026200        (ART-ADTRDEST(1:2) NOT = 'CD')                                    
026300         MOVE MID-ADLAGOMR     TO ART-ADLAGOMR                            
026400     END-IF                                                               
026500                                                                          
026600     IF (MID-ADGANG NOT = ALL '+')          AND                           
026700        (ART-ADTRDEST(1:2) NOT = 'CD')                                    
026800         MOVE MID-ADGANG       TO ART-ADGANG                              
026900     END-IF                                                               
027000                                                                          
027100     IF (MID-ADPLATS NOT = ALL '+')         AND                           
027200        (ART-ADTRDEST(1:2) NOT = 'CD')                                    
027300         MOVE MID-ADPLATS      TO ART-ADPLATS                             
027400     END-IF                                                               
027500                                                                          
027600     MOVE 1 TO CD-IX                                                      
027700     PERFORM UNTIL CD-IX > 4                                              
027800       IF (MID-ADGANG-CD(CD-IX) NOT = ALL '+')          AND               
027900          (ART-ADTRDEST(1:2) = 'CD')                                      
028000           MOVE ART-ADTRDEST(3:1) TO CD-HELP                              
028100           IF CD-HELP = CD-IX                                             
028200             MOVE MID-ADGANG-CD(CD-IX) TO ART-ADGANG                      
028300           END-IF                                                         
028400       END-IF                                                             
028500                                                                          
028600       IF (MID-ADPLATS-CD(CD-IX) NOT = ALL '+')         AND               
028700          (ART-ADTRDEST(1:2) = 'CD')                                      
028800           MOVE ART-ADTRDEST(3:1) TO CD-HELP                              
028900           IF CD-HELP = CD-IX                                             
029000             MOVE MID-ADPLATS-CD(CD-IX) TO ART-ADPLATS                    
029100           END-IF                                                         
029200       END-IF                                                             
029300       ADD 1 TO CD-IX                                                     
029400     END-PERFORM                                                          
029500                                                                          
029600                                                                          
029700     IF MID-BEFT               NOT = ALL '+'                              
029800         MOVE MID-BEFT         TO ART-BEFT                                
029900     END-IF                                                               
030000                                                                          
030100     IF MID-VKART              NOT = ALL '+'                              
030200         MOVE MID-VKART        TO ART-VKART                               
030300     END-IF                                                               
030400                                                                          
030500     IF MID-VLARTNTO           NOT = ALL '+'                              
030600** INMATNING PÅ BILD 6163 GÖRS 10 GGR STÖRRE ÄN FAKTISKT VÄRDE            
030700** (ANGER EJ DECIMAL) (INMATNING 100 GER VOLYM 10)                        
030710*  FIXEN VERKAR INTE GÄLLA LÄNGRE / GK                                    
030800        MOVE MID-VLARTNTO          TO ART-VLARTNTO                        
030900*       MOVE MID-VLARTNTO          TO WS-VLARTNTO-ALFA                    
031000*       COMPUTE WS-VLARTNTO = WS-VLARTNTO / 10                            
031100*       MOVE WS-VLARTNTO           TO ART-VLARTNTO                        
031110                                                                          
031120*    ...BUT FROM SCREEN 6169 WE NEED THE FIX AGAIN                        
031130        IF MSG-MOD-NAME = 'W6O169N1'                                      
031132           COMPUTE ART-VLARTNTO = ART-VLARTNTO / 10                       
031140        END-IF                                                            
031200     END-IF                                                               
031300                                                                          
031400     IF MID-IDARTNR-EMBQ3      NOT = ALL '+'                              
031500         PERFORM BAA-UPPDATERA-KDLAGEMB                                   
031600     END-IF                                                               
031700                                                                          
031800     IF MID-PRARTSTD           NOT = ALL '+'                              
031900         PERFORM BAB-BEHANDLA-PRIS-AENDRING                               
032000     END-IF                                                               
032100                                                                          
032200     IF MID-KDARTURS           NOT = ALL '+'                              
032300         MOVE MID-KDARTURS     TO ART-KDARTURS                            
032400     END-IF                                                               
032500                                                                          
032600     PERFORM IMS-REPL-INLA1-INLA11                                        
032700     .                                                                    
032800     EJECT                                                                
032900 BAA-UPPDATERA-KDLAGEMB  SECTION.                                         
033000                                                                          
033100     MOVE 1                         TO EMB-IX                             
033200     PERFORM UNTIL EMB-IX           >  TAB-EMBQ3-MAX OR                   
033300          TAB-KOD (EMB-IX)          = MID-IDARTNR-EMBQ3(7:3)              
033400       ADD 1                        TO EMB-IX                             
033500     END-PERFORM                                                          
033600                                                                          
033700     IF EMB-IX                      > TAB-EMBQ3-MAX                       
033800         CONTINUE                                                         
033900      ELSE                                                                
034000         IF TAB-KOD (EMB-IX)        =  MID-IDARTNR-EMBQ3(7:3)             
034100             MOVE TAB-TEXT (EMB-IX) TO ART-KDLAGEMB                       
034200         END-IF                                                           
034300     END-IF                                                               
034400     .                                                                    
034500     EJECT                                                                
034600 BAB-BEHANDLA-PRIS-AENDRING SECTION.                                      
034700                                                                          
034800     MOVE NEJ                  TO FLFEL-AENDRAD-SW                        
034900     MOVE MID-PRARTSTD         TO ART-PRARTSTD                            
035000     IF (ART-FLFEL             =  JA AND                                  
035100         ART-PRARTSTD          =  ZERO)    OR                             
035200        (ART-FLFEL             =  NEJ AND                                 
035300         ART-PRARTSTD          >  ZERO)    OR                             
035400         ART-IDLOPNRM          >  ZERO                                    
035500          CONTINUE                                                        
035600      ELSE                                                                
035700          PERFORM BABA-BEHANDLA-FLFEL-INLA11                              
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 BABA-BEHANDLA-FLFEL-INLA11  SECTION.                                     
036200                                                                          
036300     MOVE NEJ                  TO W-ART-FLFEL                             
036400                                                                          
036500     IF ART-PRARTSTD           =  ZERO                                    
036600         MOVE JA               TO W-ART-FLFEL                             
036700     END-IF                                                               
036800                                                                          
036900     PERFORM IMS-GHU-INLA2-INLA01                                         
037000     IF FOERSTA-INLA11                                                    
037100         PERFORM BABAA-KOLLA-ART-REG                                      
037200         MOVE NEJ              TO FOERSTA-INLA11-SW                       
037300     END-IF                                                               
037400                                                                          
037500     IF INL-IDFTG              =  W-IDFTG                                 
037600         CONTINUE                                                         
037700      ELSE                                                                
037800         MOVE JA               TO W-ART-FLFEL                             
037900     END-IF                                                               
038000                                                                          
038100     IF W-ART-FLFEL            =  ART-FLFEL                               
038200         CONTINUE                                                         
038300      ELSE                                                                
038400         MOVE W-ART-FLFEL      TO ART-FLFEL                               
038500         MOVE JA               TO FLFEL-AENDRAD-SW                        
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 BABAA-KOLLA-ART-REG         SECTION.                                     
039000                                                                          
039100     PERFORM IMS-GU-ARTC-ARTC01                                           
039200     IF SEGMENT-FINNS                                                     
039300         MOVE ARTC-ART-IDFTG  TO W-IDFTG                                  
039400         PERFORM IMS-GNP-ARTC-ARTC11                                      
039500         IF SEGMENT-SAKNAS OR ARTC-CLAG-KDERS > 21                        
039600             MOVE JA           TO W-ART-FLFEL                             
039700         END-IF                                                           
039800      ELSE                                                                
039900         MOVE ZERO             TO W-IDFTG                                 
040000         MOVE JA               TO W-ART-FLFEL                             
040100     END-IF                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 BB-BEHANDLA-FLFEL-INLA01  SECTION.                                       
040500                                                                          
040600     PERFORM IMS-GU-INLA1-INLA11                                          
040700     PERFORM UNTIL SEGMENT-SAKNAS OR ART-FLFEL = JA                       
040800         PERFORM IMS-GNP-INLA1-INLA11                                     
040900     END-PERFORM                                                          
041000                                                                          
041100     IF SEGMENT-FINNS                                                     
041200         IF INL-FLFEL          =  NEJ                                     
041300             MOVE JA           TO INL-FLFEL                               
041400             PERFORM IMS-REPL-INLA2-INLA01                                
041500         END-IF                                                           
041600      ELSE                                                                
041700         IF INL-FLFEL          =  JA                                      
041800             MOVE NEJ          TO INL-FLFEL                               
041900             PERFORM IMS-REPL-INLA2-INLA01                                
042000         END-IF                                                           
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400* --- IMS SEKTIONER ---                                                   
042500     SKIP3                                                                
042600 IMS-GET-MSG SECTION.                                                     
042700                                                                          
042800     MOVE '  QC' TO GODK-STATUSKODER                                      
042900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
043000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300     SKIP3                                                                
043400 IMS-GN-MSG SECTION.                                                      
043500                                                                          
043600     MOVE '    ' TO GODK-STATUSKODER                                      
043700     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
043800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043900     PERFORM IMS-STATUSKONTROLL                                           
044000     .                                                                    
044100     SKIP3                                                                
044200 IMS-ISRT-DISP-MSG SECTION.                                               
044300                                                                          
044400     MOVE    '  '             TO GODK-STATUSKODER                         
044500     CALL    CBLTDLI          USING ISRT DISP-PCB MSG-KOM-WMSGKOM         
044600     MOVE    DISP-STATUS-CODE TO STATUS-WS                                
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     EJECT                                                                
045000 IMS-GU-INLI-INLI01 SECTION.                                              
045100     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
045200                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
045300          DELIMITED BY SIZE INTO SSA1                                     
045400     MOVE '  GE' TO GODK-STATUSKODER                                      
045500     CALL CBLTDLI USING GU INLI-PCB DLI-IO-AREA SSA1                      
045600     MOVE INLI-STATUS-CODE TO STATUS-WS                                   
045700     PERFORM IMS-STATUSKONTROLL                                           
045800     .                                                                    
045900     SKIP3                                                                
046000 IMS-GN-INLI-INLI01 SECTION.                                              
046100     STRING 'W6INLI01(W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
046200                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
046300          DELIMITED BY SIZE INTO SSA1                                     
046400     MOVE '  GE' TO GODK-STATUSKODER                                      
046500     CALL CBLTDLI USING GN INLI-PCB DLI-IO-AREA SSA1                      
046600     MOVE INLI-STATUS-CODE TO STATUS-WS                                   
046700     PERFORM IMS-STATUSKONTROLL                                           
046800     .                                                                    
046900     EJECT                                                                
047000 IMS-GU-INLA1-INLA11 SECTION.                                             
047100     STRING 'W6INLA01*P(W6D101KY =' W-W6D101KY-X ')'                      
047200          DELIMITED BY SIZE INTO SSA1                                     
047300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
047400          DELIMITED BY SIZE INTO SSA2                                     
047500     MOVE '  GE' TO GODK-STATUSKODER                                      
047600     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA SSA1 SSA2                
047700     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
047800     PERFORM IMS-STATUSKONTROLL                                           
047900     .                                                                    
048000     SKIP3                                                                
048100 IMS-GNP-INLA1-INLA11 SECTION.                                            
048200     MOVE 'W6INLA11'       TO SSA1                                        
048300     MOVE '  GE' TO GODK-STATUSKODER                                      
048400     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA SSA1                    
048500     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
048600     PERFORM IMS-STATUSKONTROLL                                           
048700     .                                                                    
048800     SKIP3                                                                
048900 IMS-GHU-INLA1-INLA11 SECTION.                                            
049000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
049100          DELIMITED BY SIZE INTO SSA1                                     
049200     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
049300          DELIMITED BY SIZE INTO SSA2                                     
049400     MOVE '  GE' TO GODK-STATUSKODER                                      
049500     CALL CBLTDLI USING GHU  INLA1-PCB DLI-IO-AREA SSA1 SSA2              
049600     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
049700     PERFORM IMS-STATUSKONTROLL                                           
049800     .                                                                    
049900     SKIP3                                                                
050000 IMS-REPL-INLA1-INLA11 SECTION.                                           
050100                                                                          
050200     MOVE '  ' TO GODK-STATUSKODER                                        
050300     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA                        
050400     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     .                                                                    
050700     EJECT                                                                
050800 IMS-GHU-INLA2-INLA01 SECTION.                                            
050900     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
051000          DELIMITED BY SIZE INTO SSA1                                     
051100     MOVE '  GE' TO GODK-STATUSKODER                                      
051200     CALL CBLTDLI USING GHU INLA2-PCB DLI-IO-AREA3 SSA1                   
051300     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600     SKIP3                                                                
051700 IMS-REPL-INLA2-INLA01 SECTION.                                           
051800                                                                          
051900     MOVE '  ' TO GODK-STATUSKODER                                        
052000     CALL CBLTDLI USING REPL INLA2-PCB DLI-IO-AREA3                       
052100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
052200     PERFORM IMS-STATUSKONTROLL                                           
052300     .                                                                    
052400     EJECT                                                                
052500 IMS-GU-ARTC-ARTC01 SECTION.                                              
052600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
052700          DELIMITED BY SIZE INTO SSA1                                     
052800     MOVE '  GE' TO GODK-STATUSKODER                                      
052900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1                     
053000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     SKIP3                                                                
053400 IMS-GNP-ARTC-ARTC11 SECTION.                                             
053500     MOVE 'WLARTC11 ' TO SSA1                                             
053600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
053700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
053800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053900     PERFORM IMS-STATUSKONTROLL                                           
054000     .                                                                    
054100     EJECT                                                                
054200 IMS-STATUSKONTROLL SECTION.                                              
054300                                                                          
054400     SET STATUS-IX TO 1                                                   
054500     SEARCH GODK-STATUS                                                   
054600       AT END                                                             
054700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
054800         DELIMITED BY SIZE INTO FELTEXT                                   
054900         CALL FELLOG                                                      
055000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055100         CONTINUE                                                         
055200     END-SEARCH                                                           
055300     .                                                                    
