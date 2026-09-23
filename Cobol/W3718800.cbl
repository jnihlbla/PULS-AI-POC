000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3718800.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   APRIL-2000.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
001500*       -PGM SUMMERAR/SKRIVER                                             
001600*        RETUR-TRANSAKTIONER FÖR BYTES-ARTIKLAR (CORE)                    
002100*                                                                         
002200*       -PROGRAMMET LÄSER      WDM6                                       
002300*                   LÄSER      WDM6E                                      
002400*                   LÄSER      WDK6                                       
002700*                                                                         
003700*    ABENDKODER:                                                          
003800*        U0016 -  . . . .                                                 
003900*        U1000 -  . . . .                                                 
004000*                                                                         
004010*    CHANGE LOG:                                                          
004020*                                                                         
004030*    DIGAMBAR/20021004                                                    
004040*    WDM6E INDEX IS CHANGED TO REFER THE IDBYTRAP-9KOMPL INSTEAD          
004050*    OF THE IDBYTRAP. THIS IS TO SHOW THE DETAILS IN DESCENDING           
004060*    ORDER OF THE IDBYTRAP.IDBYTRAP-9KOMPL FIELD IS ADDED IN              
004070*    WDM611                                                               
004100                                                                          
004300 ENVIRONMENT DIVISION.                                                    
004400                                                                          
004500 INPUT-OUTPUT SECTION.                                                    
004600                                                                          
004700 FILE-CONTROL.                                                            
005200*          --- SUMMERADE NYINKOMNA RETURER                                
005300     SELECT W37188                     ASSIGN TO W37188D1.                
006610                                                                          
006700 DATA DIVISION.                                                           
006800                                                                          
006900 FILE SECTION.                                                            
007000                                                                          
007010 FD  W37188                                                               
007020     RECORDING       F                                                    
007030     BLOCK CONTAINS  0.                                                   
007040                                                                          
007050*01  POST -COPY W37188 -PRE  UT-  -L.                                     
007060     EJECT                                                                
007070                                                                          
011500 WORKING-STORAGE SECTION.                                                 
011700*    -- CHECKED BY WY2000                                                 
011800 77  IDPGM                       PIC X(8)    VALUE 'W3718800'.            
011900 77  JA                          PIC X       VALUE 'Y'.                   
012000 77  NEJ                         PIC X       VALUE 'N'.                   
012100 77  WS-DAAAPP                   PIC 9(6)    VALUE 200000.                
012200 77  WS-KVRETUR                  PIC S9(7)   COMP-3 VALUE ZERO.           
012300 77  WS-SPAR-IDARTNR             PIC S9(9)   COMP-3 VALUE ZERO.           
012400 77  W-9KOMPL                    PIC 9(07)   VALUE 9999999.               
013110     EJECT                                                                
013120                                                                          
013121 01  FELTEXT                     PIC X(80).                               
013190     EJECT                                                                
018900                                                                          
019000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
019100 01  FILLER REDEFINES DAGENS-DATUM.                                       
019200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
019300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
019400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
019410 01  WS-DAREGDAT.                                                         
019420     03  FILLER                  PIC X(2).                                
019430     03  WS-TIREGDAT             PIC 9(6).                                
019500                                                                          
020000 01  DAGENS-KLOCKA               PIC 9(8)    VALUE ZERO.                  
020100 01  WS-KLOCKA                   PIC 9(6)    VALUE ZERO.                  
020200     EJECT                                                                
020210                                                                          
020300 01  DYNAMISKA-SUBPROGRAM.                                                
020400*                                                                         
020500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021300                                                                          
021400*    --- PARAMETRAR TILL ABEND                                            
021500                                                                          
021600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021900     EJECT                                                                
022000                                                                          
022110*    --- PARAMETRAR TILL DATKORT                                          
022120*                                                                         
022200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37188'.              
022400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022500                                                                          
022600*01  -COPY WDATKORT                                                       
022700     EJECT                                                                
022720                                                                          
022730 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
022740*01  -COPY WDATAREA                                                       
022750     EJECT                                                                
022760                                                                          
022800*    --- PARAMETRAR TILL POSTSUM                                          
022900*                                                                         
023000*01  -COPY W0005   -PRE  POSTSUM-                                         
023100     EJECT                                                                
023200                                                                          
025200 01  UT-AREA-START               PIC X(24)   VALUE                        
025300                                 'UT-AREA-START  '.                       
025500                                                                          
025600*01  -COPY W37188    -PRE UT-                                             
026200     EJECT                                                                
026210                                                                          
026300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026400*                                                                         
026600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026700                                                                          
026800 01  NYCKLAR-TILL-DLI.                                                    
026810     03  W-WDM6E1KY-MIN-X.                                                
026820         05  W-IDARTNR-M6E-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
026830         05  W-IDDISTR-M6E-MIN   PIC S9(5)   VALUE ZERO COMP-3.           
026840         05  W-IDBYTRAP-M6E-MIN  PIC S9(7)   VALUE ZERO COMP-3.           
026850         05  W-IDBYTRAD-M6E-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
026851     03  W-WDM6E1KY-MAX-X.                                                
026852         05  W-IDARTNR-M6E-MAX   PIC S9(9) VALUE ZERO     COMP-3.         
026853         05  W-IDDISTR-M6E-MAX   PIC S9(5) VALUE +99999   COMP-3.         
026854         05  W-IDBYTRAP-M6E-MAX  PIC S9(7) VALUE +9999999 COMP-3.         
026855         05  W-IDBYTRAD-M6E-MAX  PIC S9(5) VALUE +99999   COMP-3.         
026856     03  W-WDM601KY-X.                                                    
026857         05  W-IDDISTR-M6        PIC S9(5)   VALUE ZERO COMP-3.           
026858         05  W-IDBYTRAP-M6       PIC S9(7)   VALUE ZERO COMP-3.           
026859     03  W-IDBYTRAD-M6-X.                                                 
026860         05  W-IDBYTRAD-M6       PIC S9(5)   VALUE ZERO COMP-3.           
026870     03  W-IDARTNR-K6-X.                                                  
026880         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
026910     EJECT                                                                
030500                                                                          
030600*    --- STATUS-KOD FRÅN IMS                                              
030700 01  STATUS-WS                   PIC XX.                                  
030800     88  SEGMENT-FINNS                       VALUE '  '.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030910     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031000                                                                          
031100 01  GODK-STATUSKODER.                                                    
031200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031300                                                                          
031400 01  SSA1                        PIC X(64).                               
031500 01  SSA2                        PIC X(64).                               
031600 01  SSA3                        PIC X(64).                               
031800     EJECT                                                                
031810                                                                          
031900*    --- IMS FUNKTIONSKODER                                               
032000*01  -COPY W0003                                                          
032100     EJECT                                                                
032110                                                                          
032200*    ---  DLI INPUT-OUTPUT AREA                                           
032210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM601'.                      
032220 01  DLI-IO-WDM601.                                                       
032230*    03  -COPY WDM601                                                     
032240     EJECT                                                                
032250                                                                          
032260 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM611'.                      
032270 01  DLI-IO-WDM611.                                                       
032280*    03  -COPY WDM611                                                     
032290     EJECT                                                                
032291                                                                          
032292 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM6E1'.                      
032293 01  DLI-IO-WDM6E1.                                                       
032294*    03  -COPY WDM6E1                                                     
032295     EJECT                                                                
032296                                                                          
032297 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
032298 01  DLI-IO-WDK601.                                                       
032299*    03  -COPY WDK601                                                     
032300     EJECT                                                                
032400                                                                          
035500 LINKAGE SECTION.                                                         
035610*01  -COPY W0008   -PRE WDM6E-                                            
035620     05  FILLER                  PIC X.                                   
035696     EJECT                                                                
035697                                                                          
035698*01  -COPY W0008   -PRE WDM6-                                             
035699     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035710                                                                          
035800*01  -COPY W0008   -PRE WDK6-                                             
035900     05  FILLER                  PIC X.                                   
036000     EJECT                                                                
036100                                                                          
037310 PROCEDURE DIVISION  USING WDM6E-PCB WDM6-PCB WDK6-PCB.                   
037400 MAIN SECTION.                                                            
037610     ENTRY 'DLITCBL' USING WDM6E-PCB WDM6-PCB WDK6-PCB.                   
037700                                                                          
037800     PERFORM A-INIT                                                       
037900                                                                          
038000     PERFORM B-BEARBETA                                                   
038900                                                                          
039000     PERFORM Z-FINIT                                                      
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039510                                                                          
039600 A-INIT SECTION.                                                          
040000     OPEN OUTPUT W37188                                                   
040500                                                                          
040600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
040700     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
040800     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
040900     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
040920                                                                          
041000     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
041100                                                                          
041900     ACCEPT DAGENS-KLOCKA FROM TIME                                       
042000     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
042100     .                                                                    
042200     EJECT                                                                
042210                                                                          
042300 B-BEARBETA SECTION.                                                      
042302     PERFORM IMS-GN-WDM6E1                                                
042303                                                                          
042304     IF NOT SEGMENT-SLUT                                                  
042305       MOVE ZERO                            TO WS-KVRETUR                 
042306                                               WS-SPAR-IDARTNR            
042309                                                                          
042310       PERFORM UNTIL SEGMENT-SLUT                                         
042311         MOVE SEQE-IDDISTR                  TO W-IDDISTR-M6               
042313         COMPUTE W-IDBYTRAP-M6 = W-9KOMPL - SEQE-IDBYTRAP-9KOMPL          
042314         PERFORM IMS-GU-WDM601                                            
042315         MOVE RAPP-DAREGDAT                 TO WS-DAREGDAT                
042316                                                                          
042317         IF WS-TIREGDAT = DAGENS-DATUM                                    
042318           MOVE SEQE-IDBYTRAD               TO W-IDBYTRAD-M6              
042319           PERFORM IMS-GU-WDM611                                          
042320                                                                          
042321           IF WS-SPAR-IDARTNR NOT = OBJ-IDARTNR-OBJ AND                   
042322              WS-SPAR-IDARTNR NOT = 0                                     
042323                                                                          
042324             MOVE WS-SPAR-IDARTNR           TO UT-IDARTNR                 
042325                                               W-IDARTNR-K6               
042326             PERFORM IMS-GU-WDK601                                        
042327             IF SEGMENT-FINNS                                             
042328               MOVE ART-IDFKNGRP            TO UT-IDFKNGRP                
042329               MOVE WS-KVRETUR              TO UT-KVRETUR                 
042330                                                                          
042331               MOVE 'AAMMDD'                TO DAT-KDDATFORM              
042332               MOVE DAGENS-DATUM            TO DAT-I-TIDATUM              
042333                                                                          
042334               CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM            
042335                                   DAT-O-TIDATUM DAT-KDSVAR               
042336                                                                          
042337               IF DAT-KDSVAR-OK                                           
042338                 MOVE 200000                TO WS-DAAAPP                  
042339                 ADD  DAT-TIAAPP            TO WS-DAAAPP                  
042340                 MOVE WS-DAAAPP             TO UT-DAAAPP                  
042341               ELSE                                                       
042342                 DISPLAY 'FELAKTIGT DATUM'                                
042343                 CALL FELLOG                                              
042344               END-IF                                                     
042345                                                                          
042349               PERFORM S11-SKRIV-W37188                                   
042350             END-IF                                                       
042351                                                                          
042352             MOVE ZERO                      TO WS-KVRETUR                 
042353             MOVE SEQE-IDARTNR              TO WS-SPAR-IDARTNR            
042355           END-IF                                                         
042356                                                                          
042357           IF WS-SPAR-IDARTNR = 0                                         
042358             MOVE SEQE-IDARTNR              TO WS-SPAR-IDARTNR            
042359           END-IF                                                         
042360           ADD OBJ-KVRETUR-URSP             TO WS-KVRETUR                 
042361         END-IF                                                           
042362                                                                          
042363         PERFORM IMS-GN-WDM6E1                                            
042370       END-PERFORM                                                        
042371                                                                          
042372       IF WS-SPAR-IDARTNR NOT = 0                                         
042373         MOVE WS-SPAR-IDARTNR               TO UT-IDARTNR                 
042374                                               W-IDARTNR-K6               
042375         PERFORM IMS-GU-WDK601                                            
042376                                                                          
042377         IF SEGMENT-FINNS                                                 
042378           MOVE ART-IDFKNGRP                TO UT-IDFKNGRP                
042379           MOVE WS-KVRETUR                  TO UT-KVRETUR                 
042380                                                                          
042381           MOVE 'AAMMDD'                    TO DAT-KDDATFORM              
042382           MOVE DAGENS-DATUM                TO DAT-I-TIDATUM              
042383                                                                          
042384           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
042385                               DAT-O-TIDATUM DAT-KDSVAR                   
042386                                                                          
042387           IF DAT-KDSVAR-OK                                               
042388             MOVE 200000                    TO WS-DAAAPP                  
042389             ADD  DAT-TIAAPP                TO WS-DAAAPP                  
042390             MOVE WS-DAAAPP                 TO UT-DAAAPP                  
042391           ELSE                                                           
042392             DISPLAY 'FELAKTIGT DATUM'                                    
042393             CALL FELLOG                                                  
042394           END-IF                                                         
042401                                                                          
042402           PERFORM S11-SKRIV-W37188                                       
042403         END-IF                                                           
042410       END-IF                                                             
042500     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044110                                                                          
445200 Z-FINIT SECTION.                                                         
445300     CLOSE W37188                                                         
445900                                                                          
446000     MOVE 'S' TO POSTSUM-OPKOD                                            
446100     CALL POSTSUM USING POSTSUM-PARM                                      
446200     .                                                                    
446300     EJECT                                                                
446310                                                                          
446320 S11-SKRIV-W37188 SECTION.                                                
446330     WRITE UT-POST   FROM UT-W37188                                       
446340                                                                          
446350     MOVE 'UT-'      TO   POSTSUM-TRANSTYP                                
446360     MOVE 'W37188'   TO   POSTSUM-FDNAMN                                  
446370     MOVE 'W37188D1' TO   POSTSUM-DDNAMN2                                 
446380     CALL POSTSUM USING POSTSUM-PARM                                      
446390     .                                                                    
446400     EJECT                                                                
446500                                                                          
466700* --- IMS SEKTIONER ---                                                   
474000 IMS-GN-WDM6E1 SECTION.                                                   
474001     MOVE 'WDM6E1  '       TO SSA1                                        
474003     MOVE '  GB'           TO GODK-STATUSKODER                            
474004     CALL CBLTDLI USING GN WDM6E-PCB DLI-IO-WDM6E1 SSA1                   
474005     MOVE WDM6E-STATUS-CODE TO STATUS-WS                                  
474006     PERFORM IMS-STATUSKONTROLL                                           
474007     .                                                                    
474008     EJECT                                                                
474009                                                                          
474010 IMS-GU-WDM601 SECTION.                                                   
474011     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
474012     DELIMITED BY SIZE INTO SSA1                                          
474013     MOVE '  '             TO GODK-STATUSKODER                            
474014     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
474015     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
474016     PERFORM IMS-STATUSKONTROLL                                           
474017     .                                                                    
474018                                                                          
474019 IMS-GU-WDM611 SECTION.                                                   
474020     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
474021     DELIMITED BY SIZE INTO SSA1                                          
474022     STRING 'WDM611  (IDBYTRAD =' W-IDBYTRAD-M6-X ')'                     
474023     DELIMITED BY SIZE INTO SSA2                                          
474024     MOVE '  '             TO GODK-STATUSKODER                            
474025     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM611 SSA1 SSA2               
474026     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
474027     PERFORM IMS-STATUSKONTROLL                                           
474028     .                                                                    
474120     EJECT                                                                
474121                                                                          
474122 IMS-GU-WDK601 SECTION.                                                   
474123     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
474124     DELIMITED BY SIZE INTO SSA1                                          
474125     MOVE '  GE'           TO GODK-STATUSKODER                            
474126     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
474127     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
474128     PERFORM IMS-STATUSKONTROLL                                           
474129     .                                                                    
474130     EJECT                                                                
474131                                                                          
474140 IMS-STATUSKONTROLL SECTION.                                              
474200     SET STATUS-IX TO 1                                                   
474300     SEARCH GODK-STATUS                                                   
474400       AT END                                                             
474500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
474600           DELIMITED BY SIZE INTO FELTEXT                                 
474700         DISPLAY FELTEXT                                                  
474800         CALL FELLOG                                                      
474900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
475000         CONTINUE                                                         
475100     END-SEARCH                                                           
475200     .                                                                    
