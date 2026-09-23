000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2253400.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   90/12/10.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        MATCHAR TPO 3 URVAL MED SAMTLIGA SEGMENT I WDD2,                 
001100*        KOMPLETTERAR MED INFORMATION FRÅN WDG2. SKRIVER                  
001200*        MATCHNINGARNA PÅ UTFIL.                                          
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLARTG (WDD2)                              
001500*                              WLARTC (WDK6)                              
001600*                              WLXXAP (WDG2)                              
001700*                                                                         
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- FIL MED URVAL (TPO 3)                                      
003200     SELECT W22531                     ASSIGN TO W22534D1.                
003300     SKIP2                                                                
003400*          --- FIL MED MATCHADE URVAL FRÅN WDD2                           
003500     SELECT W22534                     ASSIGN TO W22534D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W22531                                                               
004200     LABEL RECORD    STANDARD                                             
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600*01  -COPY W22531          -L.                                            
004700     SKIP3                                                                
004800 FD  W22534                                                               
004900     LABEL RECORD    STANDARD                                             
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200     SKIP2                                                                
005300*01  POST -COPY W22532     -PRE  UT-  -L.                                 
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005700*    -COPY WY2000W1                                                       
005800     SKIP3                                                                
005900 77  IDPGM                       PIC X(8)  VALUE 'W2253400'.              
006000 77  JA                          PIC X     VALUE 'J'.                     
006100 77  NEJ                         PIC X     VALUE 'N'.                     
006200 77  INDX                        PIC S9(3) VALUE +0   COMP SYNC.          
006300 77  ANT-URVAL                   PIC S9(3) VALUE +0   COMP SYNC.          
006400 77  MAX-INDX                    PIC S9(3) VALUE +100 COMP SYNC.          
006500 77  ART-IX                      PIC S9(3) VALUE +0   COMP SYNC.          
006600 77  SPAR-IX                     PIC S9(3) VALUE +0   COMP SYNC.          
006700                                                                          
006800 77  W22531-EOF-SW               PIC X     VALUE 'N'.                     
006900     88  END-OF-W22531                     VALUE 'J'.                     
007000 77  TRAFF-SW                    PIC X     VALUE 'N'.                     
007100     88  TRAFF                             VALUE 'J'.                     
007200     EJECT                                                                
007300*    --- ARBETSFÄLT                                                       
007400 01  ARBETSFAELT.                                                         
007500     03  WS-TIAAVVD.                                                      
007600       05  WS-TIAAVV              PIC 9(4) VALUE ZERO.                    
007700       05  FILLER                 PIC 9(1) VALUE ZERO.                    
007800     03  URVAL-FINNS              PIC X(1) VALUE 'N'.                     
007900*    --- URVALSTABELL                                                     
008000 01  URVALSTABELL.                                                        
008100     03  URVAL   OCCURS 200.                                              
008200       05  URVALS-ID.                                                     
008300         07  URV-IDUSER           PIC X(8).                               
008400         07  URV-TIREGDAT         PIC 9(7).                               
008500         07  URV-TIREGTID         PIC 9(7).                               
008600       05  URV-IDANSK-FOM         PIC 9(3).                               
008700       05  URV-IDANSK-TOM         PIC 9(3).                               
008800       05  URV-KDSORT1            PIC 9.                                  
008900       05  URV-IDLEVNR            PIC X(5).                               
009000       05  URV-KDPRODSL           PIC 9(3).                               
009100       05  URV-KDBASLM-FOM        PIC X(6).                               
009200       05  URV-KDBASLM-TOM        PIC X(6).                               
009300       05  URV-TITPO-FOM          PIC 9(7).                               
009400       05  URV-TITPO-TOM          PIC 9(7).                               
009500       05  URV-IDARTNR OCCURS 100 PIC 9(9).                               
009600     EJECT                                                                
009700*    --- SPAR-AREA: LÄSER IN FRÅN WDD2, WDK6, WDG2. ANVÄNDS FÖR           
009800*                   ATT JÄMFÖRA MED URVALEN.                              
009900 01  SPAR-AREA.                                                           
010000     03  SPAR-IDARTNR            PIC S9(9).                               
010100     03  SPAR-IDANSK             PIC S9(3).                               
010200     03  SPAR-IDLEVNR            PIC X(5).                                
010300     03  SPAR-KDPRODSL           PIC S9(3).                               
010400     03  SPAR-MARKN-INFO OCCURS 100.                                      
010500       05  SPAR-KDBASLM          PIC X(6).                                
010600       05  SPAR-IDPROJ           PIC X(4).                                
010700       05  SPAR-KVBASLM          PIC S9(7).                               
010800       05  SPAR-TITPO            PIC S9(7).                               
010900     SKIP3                                                                
011000 01  NOLL-SPAR-AREA.                                                      
011100     03  NOLL-SPAR-IDARTNR       PIC S9(9).                               
011200     03  NOLL-SPAR-IDANSK        PIC S9(3).                               
011300     03  NOLL-SPAR-IDLEVNR       PIC X(5).                                
011400     03  NOLL-SPAR-KDPRODSL      PIC S9(3).                               
011500     03  NOLL-SPAR-MARKN-INFO OCCURS 15.                                  
011600       05  NOLL-SPAR-KDBASLM     PIC X(6).                                
011700       05  NOLL-SPAR-IDPROJ      PIC X(4).                                
011800       05  NOLL-SPAR-KVBASLM     PIC S9(7).                               
011900       05  NOLL-SPAR-TITPO       PIC S9(7).                               
012000     EJECT                                                                
012100*                                                                         
012200*01  -COPY WWPRODSL                                                       
012300                                                                          
012400*    --- NOLL-AREA FÖR NOLLNING AV UT-POST                                
012500*01  AREA -COPY W22532   -PRE NOLL-                                       
012600                                                                          
012700 01  DYNAMISKA-SUBPROGRAM.                                                
012800*                                                                         
012900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013400     SKIP2                                                                
013500*    --- PARAMETRAR TILL ABEND                                            
013600                                                                          
013700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL POSTSUM                                          
014100*                                                                         
014200*01  -COPY W0005      -PRE  POSTSUM-                                      
014300     EJECT                                                                
014400*01  -COPY WDATAREA                                                       
014500     EJECT                                                                
014600 01  IN-AREA-START               PIC X(24)   VALUE                        
014700                                 'IN-AREA-START  '.                       
014800     SKIP2                                                                
014900*01  AREA -COPY W22531         -PRE IN-                                   
015000     EJECT                                                                
015100 01  UT-AREA-START               PIC X(24)   VALUE                        
015200                                 'UT-AREA-START  '.                       
015300     SKIP2                                                                
015400*01  AREA -COPY W22532         -PRE UT-                                   
015500     EJECT                                                                
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
016100     03  W-IDARTNR-X.                                                     
016200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016300     03  W-KDBASLM-X.                                                     
016400         05  W-KDBASLM           PIC X(6)    VALUE SPACE.                 
016500     03  W-WDGXKEY-1123-X.                                                
016600         05  W-IDHTYP            PIC X(4)    VALUE '1123'.                
016700         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
016800         05  W-IDPROJ            PIC X(4)    VALUE SPACE.                 
016900         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
017000     03  W-KDSEGKEY-X.                                                    
017100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
017200     03  W-WDGXKEY-1126-X.                                                
017300         05  W-KDBASLM-1126      PIC X(6)    VALUE SPACE.                 
017400         05  FILLLER             PIC X(9)    VALUE LOW-VALUE.             
017500     SKIP2                                                                
017600*    --- STATUS-KOD FRÅN IMS                                              
017700 01  STATUS-WS                   PIC XX.                                  
017800     88  SEGMENT-FINNS                       VALUE '  '.                  
017900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018200     SKIP2                                                                
018300 01  GODK-STATUSKODER.                                                    
018400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018500     SKIP3                                                                
018600 01  SSA1                        PIC X(64).                               
018700 01  SSA2                        PIC X(64).                               
018800     EJECT                                                                
018900*    --- IMS FUNKTIONSKODER                                               
019000*01  -COPY W0003                                                          
019100     EJECT                                                                
019200*    ---  DLI INPUT-OUTPUT AREA                                           
019300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-01'.        
019400     SKIP3                                                                
019500 01  DLI-IO-AREA-01.                                                      
019600     03  IO-AREA-01              PIC X(150)  VALUE SPACE.                 
019700     03  WLARTC01 REDEFINES IO-AREA-01.                                   
019800*        05  -COPY WDK601                                                 
019900     SKIP3                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020100     SKIP3                                                                
020200 01  DLI-IO-AREA.                                                         
020300     03  IO-AREA                 PIC X(550)  VALUE SPACE.                 
020400     SKIP3                                                                
020500     03  WLARTG01 REDEFINES IO-AREA.                                      
020600*        05  -COPY WDD201     -PRE ARTG01-                                
020700     SKIP3                                                                
020800     03  WLARTG11 REDEFINES IO-AREA.                                      
020900*        05  -COPY WDD211     -PRE ARTG11-                                
021000     SKIP3                                                                
021100     03  WLXXAP01 REDEFINES IO-AREA.                                      
021200*        05  -COPY WDGX1123   -PRE XXAP-                                  
021300     SKIP3                                                                
021400     03  WLXXAP11 REDEFINES IO-AREA.                                      
021500*        05  -COPY WDGX1124   -PRE XXAP-                                  
021600     SKIP3                                                                
021700     03  WLXXAP12 REDEFINES IO-AREA.                                      
021800*        05  -COPY WDGX1126   -PRE XXAP-                                  
021900     EJECT                                                                
022000 LINKAGE SECTION.                                                         
022100                                                                          
022200     EJECT                                                                
022300*01  -COPY W0008      -PRE ARTG-                                          
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008      -PRE ARTC-                                          
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008      -PRE XXAP-                                          
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200 PROCEDURE DIVISION  USING ARTG-PCB ARTC-PCB XXAP-PCB.                    
023300     ENTRY 'DLITCBL' USING ARTG-PCB ARTC-PCB XXAP-PCB.                    
023400                                                                          
023500     PERFORM A-INIT                                                       
023600     PERFORM B-LAS-IN-URVAL-TILL-TABELL                                   
023700     IF URVAL-FINNS = JA                                                  
023800       PERFORM IMS-GET-ARTG01                                             
023900       PERFORM UNTIL SEGMENT-SLUT                                         
024000         IF ARTG01-ART-KVBASL > 0                                         
024100           PERFORM C-HAEMTA-IN-INFO                                       
024200           MOVE +1 TO INDX                                                
024300           PERFORM UNTIL INDX > ANT-URVAL                                 
024400             PERFORM D-JFR-INFO-MED-URVAL-SKRIV                           
024500             ADD +1 TO INDX                                               
024600           END-PERFORM                                                    
024700           MOVE NOLL-SPAR-AREA TO SPAR-AREA                               
024800         END-IF                                                           
024900         PERFORM IMS-GET-ARTG01                                           
025000       END-PERFORM                                                        
025100     END-IF                                                               
025200                                                                          
025300     PERFORM Z-FINIT                                                      
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT SECTION.                                                          
026000                                                                          
026100     OPEN INPUT  W22531                                                   
026200                                                                          
026300     OPEN OUTPUT W22534                                                   
026400                                                                          
026500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026600                                                                          
026700     INITIALIZE NOLL-AREA                                                 
026800     INITIALIZE URVALSTABELL                                              
026900     INITIALIZE NOLL-SPAR-AREA                                            
027000                                                                          
027100     MOVE NOLL-AREA      TO UT-AREA                                       
027200     MOVE NOLL-SPAR-AREA TO SPAR-AREA                                     
027300     .                                                                    
027400     EJECT                                                                
027500 B-LAS-IN-URVAL-TILL-TABELL SECTION.                                      
027600                                                                          
027700     PERFORM S01-LAES-W22531                                              
027800     MOVE +1 TO ANT-URVAL                                                 
027900     PERFORM UNTIL END-OF-W22531                                          
028000       MOVE JA TO URVAL-FINNS                                             
028100       MOVE IN-IDUSER       TO URV-IDUSER(ANT-URVAL)                      
028200       MOVE IN-TIREGDAT     TO URV-TIREGDAT(ANT-URVAL)                    
028300       MOVE IN-TIREGTID     TO URV-TIREGTID(ANT-URVAL)                    
028400       MOVE IN-IDANSK-FOM   TO URV-IDANSK-FOM(ANT-URVAL)                  
028500       MOVE IN-IDANSK-TOM   TO URV-IDANSK-TOM(ANT-URVAL)                  
028600       MOVE IN-KDSORT1      TO URV-KDSORT1(ANT-URVAL)                     
028700       MOVE IN-IDLEVNR      TO URV-IDLEVNR(ANT-URVAL)                     
028800       MOVE IN-KDPRODSL     TO URV-KDPRODSL(ANT-URVAL)                    
028900       MOVE IN-KDBASLM-FOM  TO URV-KDBASLM-FOM(ANT-URVAL)                 
029000       MOVE IN-KDBASLM-TOM  TO URV-KDBASLM-TOM(ANT-URVAL)                 
029100       MOVE IN-TITPO-FOM    TO URV-TITPO-FOM(ANT-URVAL)                   
029200       MOVE IN-TITPO-TOM    TO URV-TITPO-TOM(ANT-URVAL)                   
029300       MOVE +1 TO ART-IX                                                  
029400       PERFORM UNTIL ART-IX > MAX-INDX OR                                 
029500                     IN-IDARTNR(ART-IX) = 0                               
029600         MOVE IN-IDARTNR(ART-IX) TO                                       
029700                            URV-IDARTNR(ANT-URVAL, ART-IX)                
029800         ADD +1 TO ART-IX                                                 
029900       END-PERFORM                                                        
030000       ADD +1 TO ANT-URVAL                                                
030100       PERFORM S01-LAES-W22531                                            
030200     END-PERFORM                                                          
030300     COMPUTE ANT-URVAL = ANT-URVAL - 1                                    
030400     .                                                                    
030500     EJECT                                                                
030600 C-HAEMTA-IN-INFO SECTION.                                                
030700                                                                          
030800*** W D D 2                                                               
030900     MOVE ARTG01-ART-IDARTNR TO SPAR-IDARTNR                              
031000                                W-IDARTNR                                 
031100     MOVE ARTG01-ART-IDANSK  TO SPAR-IDANSK                               
031200     PERFORM IMS-GNP-ARTG11                                               
031300     MOVE +1 TO INDX                                                      
031400     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > +100                          
031500       MOVE ARTG11-ART-KDBASLM TO SPAR-KDBASLM(INDX)                      
031600       MOVE ARTG11-ART-IDPROJ  TO SPAR-IDPROJ(INDX)                       
031700       MOVE ARTG11-ART-KVBASLM TO SPAR-KVBASLM(INDX)                      
031800       PERFORM IMS-GNP-ARTG11                                             
031900       ADD +1 TO INDX                                                     
032000     END-PERFORM                                                          
032100     MOVE INDX TO SPAR-IX                                                 
032200*** W D K 6                                                               
032300     PERFORM IMS-GET-ARTC01                                               
032400     MOVE ART-IDLEVNR  TO SPAR-IDLEVNR                                    
032500     MOVE ART-KDPRODSL TO SPAR-KDPRODSL                                   
032600                           W-KDPRODSL                                     
032700*** W D G 2                                                               
032800     MOVE +1 TO INDX                                                      
032900     PERFORM UNTIL INDX > SPAR-IX                                         
033000       MOVE SPAR-IDPROJ(INDX) TO W-IDPROJ                                 
033100       PERFORM IMS-GET-XXAP-1123                                          
033200       IF SEGMENT-FINNS                                                   
033300         PERFORM IMS-GET-XXAP-1124                                        
033400         IF SEGMENT-FINNS                                                 
033500           MOVE XXAP-1124-TIGENORD TO SPAR-TITPO(INDX)                    
033600         ELSE                                                             
033700           MOVE ZERO               TO SPAR-TITPO(INDX)                    
033800         END-IF                                                           
033900         MOVE SPAR-KDBASLM(INDX)   TO W-KDBASLM-1126                      
034000         PERFORM IMS-GET-XXAP-1126                                        
034100         IF SEGMENT-FINNS                                                 
034200           IF XXAP-1126-TIMARKORD > 0                                     
034300             MOVE XXAP-1126-TIMARKORD TO SPAR-TITPO(INDX)                 
034400           END-IF                                                         
034500         END-IF                                                           
034600       ELSE                                                               
034700         MOVE ZERO TO SPAR-TITPO(INDX)                                    
034800       END-IF                                                             
034900       ADD +1 TO INDX                                                     
035000     END-PERFORM                                                          
035100     .                                                                    
035200     EJECT                                                                
035300 D-JFR-INFO-MED-URVAL-SKRIV SECTION.                                      
035400                                                                          
035500     IF URV-IDARTNR(INDX, 1) > 0                                          
035600       MOVE +1 TO ART-IX                                                  
035700       MOVE NEJ TO TRAFF-SW                                               
035800       PERFORM UNTIL ART-IX > MAX-INDX OR                                 
035900                     TRAFF             OR                                 
036000                     URV-IDARTNR(INDX, ART-IX) = ZERO                     
036100         IF URV-IDARTNR(INDX, ART-IX) = SPAR-IDARTNR                      
036200           MOVE JA TO TRAFF-SW                                            
036300         ELSE                                                             
036400           ADD +1 TO ART-IX                                               
036500         END-IF                                                           
036600       END-PERFORM                                                        
036700     ELSE                                                                 
036800       MOVE JA TO TRAFF-SW                                                
036900     END-IF                                                               
037000                                                                          
037100     IF TRAFF                                                             
037200       IF (URV-IDANSK-FOM(INDX) = 0 AND                                   
037300           URV-IDANSK-TOM(INDX) = 0)                                      
037400       OR (SPAR-IDANSK >= URV-IDANSK-FOM(INDX) AND                        
037500                       <= URV-IDANSK-TOM(INDX))                           
037600         IF URV-IDLEVNR(INDX) = SPACE                                     
037700         OR URV-IDLEVNR(INDX) = SPAR-IDLEVNR                              
037800           IF URV-KDPRODSL(INDX) = 0                                      
037900           OR URV-KDPRODSL(INDX) = SPAR-KDPRODSL                          
038000             MOVE +1 TO SPAR-IX                                           
038100             PERFORM UNTIL SPAR-IX > +15 OR                               
038200                           SPAR-KDBASLM(SPAR-IX) = 0                      
038300               IF SPAR-KDBASLM(SPAR-IX) >= URV-KDBASLM-FOM(INDX)          
038400               AND                      <= URV-KDBASLM-TOM(INDX)          
038500                 MOVE SPAR-TITPO(SPAR-IX)   TO TMP1-YYMMDD                
038600                 MOVE URV-TITPO-FOM(INDX)   TO TMP2-YYMMDD                
038700                 MOVE URV-TITPO-TOM(INDX)   TO TMP3-YYMMDD                
038800                 PERFORM WY2000Q1                                         
038900                 IF TMP1-YYMMDD >= TMP2-YYMMDD                            
039000                 AND            <= TMP3-YYMMDD                            
039100                   PERFORM S02-SKRIV-URVAL                                
039200                 END-IF                                                   
039300               END-IF                                                     
039400               ADD +1 TO SPAR-IX                                          
039500             END-PERFORM                                                  
039600           END-IF                                                         
039700         END-IF                                                           
039800       END-IF                                                             
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 Z-FINIT SECTION.                                                         
040300     CLOSE W22531                                                         
040400           W22534                                                         
040500     SKIP2                                                                
040600     MOVE 'S' TO POSTSUM-OPKOD                                            
040700     CALL POSTSUM USING POSTSUM-PARM                                      
040800     .                                                                    
040900     EJECT                                                                
041000 S01-LAES-W22531  SECTION.                                                
041100     SKIP2                                                                
041200     READ W22531 INTO IN-AREA                                             
041300     AT END                                                               
041400        SET END-OF-W22531 TO TRUE                                         
041500                                                                          
041600     NOT AT END                                                           
041700        MOVE 'W22531' TO POSTSUM-FDNAMN                                   
041800        MOVE 'W22534D1' TO POSTSUM-DDNAMN2                                
041900        CALL POSTSUM USING POSTSUM-PARM                                   
042000     END-READ                                                             
042100     .                                                                    
042200     EJECT                                                                
042300 S02-SKRIV-URVAL SECTION.                                                 
042400                                                                          
042500     MOVE URV-IDUSER(INDX)       TO UT-IDUSER                             
042600     MOVE URV-TIREGDAT(INDX)     TO UT-TIREGDAT                           
042700     MOVE URV-TIREGTID(INDX)     TO UT-TIREGTID                           
042800     MOVE ZERO                   TO UT-URV-IDURVNR                        
042900     MOVE URV-IDANSK-FOM(INDX)   TO UT-URV-IDANSK-FOM                     
043000     MOVE URV-IDANSK-TOM(INDX)   TO UT-URV-IDANSK-TOM                     
043100     MOVE URV-KDSORT1(INDX)      TO UT-URV-KDSORT1                        
043200     MOVE URV-IDLEVNR(INDX)      TO UT-URV-IDLEVNR                        
043300     MOVE URV-KDPRODSL(INDX)     TO UT-URV-KDPRODSL                       
043400     MOVE ZERO                   TO UT-URV-IDDISTR-FOM                    
043500                                    UT-URV-IDDISTR-TOM                    
043600     MOVE URV-KDBASLM-FOM(INDX)  TO UT-URV-KDBASLM-FOM                    
043700     MOVE URV-KDBASLM-TOM(INDX)  TO UT-URV-KDBASLM-TOM                    
043800     MOVE +3                     TO UT-URV-KDTPOTYP-FOM                   
043900                                    UT-URV-KDTPOTYP-TOM                   
044000     IF URV-TITPO-FOM(INDX) > 0                                           
044100       MOVE URV-TITPO-FOM(INDX)  TO DAT-I-TIDATUM                         
044200       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
044300       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
044400                           DAT-O-TIDATUM DAT-KDSVAR                       
044500       IF DAT-KDSVAR-OK                                                   
044600         MOVE DAT-TIAAVVD-GRP    TO WS-TIAAVVD                            
044700         MOVE WS-TIAAVV          TO UT-URV-TITPO-FOM                      
044800       ELSE                                                               
044900         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
045000       END-IF                                                             
045100     ELSE                                                                 
045200       MOVE ZERO                 TO UT-URV-TITPO-FOM                      
045300     END-IF                                                               
045400     IF URV-TITPO-TOM(INDX) > 0                                           
045500       MOVE URV-TITPO-TOM(INDX)  TO DAT-I-TIDATUM                         
045600       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
045700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
045800                           DAT-O-TIDATUM DAT-KDSVAR                       
045900       IF DAT-KDSVAR-OK                                                   
046000         MOVE DAT-TIAAVVD        TO WS-TIAAVVD                            
046100         MOVE WS-TIAAVV          TO UT-URV-TITPO-TOM                      
046200       ELSE                                                               
046300         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
046400       END-IF                                                             
046500     ELSE                                                                 
046600       MOVE ZERO                 TO UT-URV-TITPO-TOM                      
046700     END-IF                                                               
046800     MOVE +1 TO ART-IX                                                    
046900     PERFORM UNTIL ART-IX > MAX-INDX OR                                   
047000                   URV-IDARTNR(INDX, ART-IX) = 0                          
047100       MOVE URV-IDARTNR(INDX, ART-IX) TO UT-URV-IDARTNR(ART-IX)           
047200       ADD +1 TO ART-IX                                                   
047300     END-PERFORM                                                          
047400     MOVE SPAR-IDANSK            TO UT-IDANSK                             
047500     MOVE SPAR-IDLEVNR           TO UT-IDLEVNR                            
047600     MOVE SPAR-IDARTNR           TO UT-IDARTNR                            
047700     MOVE +3                     TO UT-KDTPOTYP                           
047800     MOVE SPAR-KVBASLM(SPAR-IX)  TO UT-KVART                              
047900     IF SPAR-TITPO(SPAR-IX) > 0                                           
048000       MOVE SPAR-TITPO(SPAR-IX)  TO DAT-I-TIDATUM                         
048100       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
048200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
048300                           DAT-O-TIDATUM DAT-KDSVAR                       
048400       IF DAT-KDSVAR-OK                                                   
048500         MOVE DAT-TIAAVVD        TO WS-TIAAVVD                            
048600         MOVE WS-TIAAVV          TO UT-TITPO                              
048700       ELSE                                                               
048800         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
048900       END-IF                                                             
049000     ELSE                                                                 
049100       MOVE ZERO                 TO UT-TITPO                              
049200     END-IF                                                               
049300     MOVE ZERO                   TO UT-IDDISTR                            
049400                                    UT-IDORDER                            
049500     MOVE SPAR-KDBASLM(SPAR-IX)  TO UT-KDBASLM                            
049600     MOVE URV-KDPRODSL(INDX)     TO TEST-KDPRODSL                         
049700     IF KDPRODSL-VOLVO-BIMA                                               
049800        PERFORM S11-SKRIV-W22534                                          
049900     END-IF                                                               
050000     MOVE NOLL-AREA TO UT-AREA                                            
050100     .                                                                    
050200     EJECT                                                                
050300 S11-SKRIV-W22534 SECTION.                                                
050400     SKIP2                                                                
050500     WRITE UT-POST FROM UT-AREA                                           
050600                                                                          
050700     MOVE 'W22534' TO POSTSUM-FDNAMN                                      
050800     MOVE 'W22534D2' TO POSTSUM-DDNAMN2                                   
050900     CALL POSTSUM USING POSTSUM-PARM                                      
051000     .                                                                    
051100     EJECT                                                                
051200*S99-ABEND SECTION.                                                       
051300*                                                                         
051400*    CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
051500*    .                                                                    
051600* --- IMS-SEKTIONER ---                                                   
051700     SKIP3                                                                
051800     EJECT                                                                
051900 IMS-GET-ARTG01   SECTION.                                                
052000     MOVE 'WLARTG01 ' TO SSA1                                             
052100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
052200     CALL CBLTDLI USING GN ARTG-PCB DLI-IO-AREA SSA1                      
052300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
052400     PERFORM IMS-STATUSKONTROLL                                           
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-GNP-ARTG11   SECTION.                                                
052800     MOVE 'WLARTG11 ' TO SSA1                                             
052900     MOVE '  GE' TO GODK-STATUSKODER                                      
053000     CALL CBLTDLI USING GNP ARTG-PCB DLI-IO-AREA SSA1                     
053100     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
053200     PERFORM IMS-STATUSKONTROLL                                           
053300     .                                                                    
053400     EJECT                                                                
053500 IMS-GET-ARTC01   SECTION.                                                
053600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
053700          DELIMITED BY SIZE INTO SSA1                                     
053800     MOVE '  ' TO GODK-STATUSKODER                                        
053900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
054000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
054100     PERFORM IMS-STATUSKONTROLL                                           
054200     .                                                                    
054300     EJECT                                                                
054400 IMS-GET-XXAP-1123 SECTION.                                               
054500     STRING 'WLXXAP01(WDGXKEY  =' W-WDGXKEY-1123-X ')'                    
054600          DELIMITED BY SIZE INTO SSA1                                     
054700     MOVE '  GE' TO GODK-STATUSKODER                                      
054800     CALL CBLTDLI USING GU XXAP-PCB DLI-IO-AREA SSA1                      
054900     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
055000     PERFORM IMS-STATUSKONTROLL                                           
055100     .                                                                    
055200     EJECT                                                                
055300 IMS-GET-XXAP-1124 SECTION.                                               
055400     STRING 'WLXXAP11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
055500          DELIMITED BY SIZE INTO SSA1                                     
055600     MOVE '  GE' TO GODK-STATUSKODER                                      
055700     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA SSA1                     
055800     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
055900     PERFORM IMS-STATUSKONTROLL                                           
056000     .                                                                    
056100     EJECT                                                                
056200 IMS-GET-XXAP-1126 SECTION.                                               
056300     STRING 'WLXXAP12(WDGXKEY  =' W-WDGXKEY-1126-X ')'                    
056400          DELIMITED BY SIZE INTO SSA1                                     
056500     MOVE '  GE' TO GODK-STATUSKODER                                      
056600     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA SSA1                     
056700     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUSKONTROLL                                           
056900     .                                                                    
057000     EJECT                                                                
057100 IMS-STATUSKONTROLL SECTION.                                              
057200     SKIP2                                                                
057300     SET STATUS-IX TO 1                                                   
057400     SEARCH GODK-STATUS                                                   
057500       AT END CALL FELLOG                                                 
057600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
057700     END-SEARCH                                                           
057800     .                                                                    
057900     EJECT                                                                
058000*    -COPY WY2000Q1                                                       
