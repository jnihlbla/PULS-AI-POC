000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6155000.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/04/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WDK7, WDD8 OCH JÄMFÖR MED WDJ8. SAKNAS POST PÅ             
000900*        WDJ8 SKAPAS EN FIL, ARTIKLAR MED EN ICKE DEFINIERAD              
001000*        PLATS.                                                           
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDJ8                                       
001300*        PROGRAMMET LÄSER      WDK7A                                      
001400*        PROGRAMMET LÄSER      WDD8                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- UTFIL MED ARTIKLAR MED ICKE DEFINIERADE PLATSER            
002900     SELECT W61550                     ASSIGN TO W61550D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W61550                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  POST -COPY W61550 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6155000'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  WS-ADLAGOMR-NUM             PIC  9(2).                               
004700 77  WS-ADGANG-NUM               PIC  9(2).                               
004800 77  WS-ADPLATS-NUM              PIC  9(5).                               
004900 77  WS-ANTAL-UT                 PIC  9(3).                               
005000 01  WS-ADPLATS.                                                          
005100     03 WS-BAY                    PIC 9(2) VALUE ZERO.                    
005200     03 WS-ADLEVEL                PIC 9(2) VALUE ZERO.                    
005300     03 WS-ADSEQ                  PIC 9(1) VALUE ZERO.                    
005400                                                                          
005500 01  WS-ADPLATS-MIN.                                                      
005600     03 WS-BAY-MIN                PIC 9(2) VALUE ZERO.                    
005700     03 WS-ADLEVEL-MIN            PIC 9(2) VALUE ZERO.                    
005800     03 WS-ADSEQ-MIN              PIC 9(1) VALUE ZERO.                    
005900                                                                          
006000 01  WS-ADPLATS-MAX.                                                      
006100     03 WS-BAY-MAX                PIC 9(2) VALUE 99.                      
006200     03 WS-ADLEVEL-MAX            PIC 9(2) VALUE 99.                      
006300     03 WS-ADSEQ-MAX              PIC 9(1) VALUE 9.                       
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     SKIP2                                                                
007800*    --- PARAMETRAR TILL ABEND                                            
007900                                                                          
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008300     SKIP2                                                                
008400 01  FELTEXT.                                                             
008500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  UT-AREA-START               PIC X(24)   VALUE                        
009300                                 'UT-AREA-START  '.                       
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY W61550     -PRE UT-                                       
009700     EJECT                                                                
009800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009900*                                                                         
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  NYCKLAR-TILL-DLI.                                                    
010400     03  W-IDDC-X.                                                        
010500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010600     03  W-IDARTNR-X.                                                     
010700         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
010800     03  W-IDDC-B6-X.                                                     
010900         05 W-IDDC-B6            PIC X(2).                                
011000                                                                          
011100     03  W-WDJ8KEY-X.                                                     
011200         05  W-LOC-IDDC          PIC X(2)   VALUE SPACE.                  
011300         05  W-LOC-ADLAGOMR      PIC 9(2)   VALUE ZERO.                   
011400         05  W-LOC-ADGANG        PIC 9(2)   VALUE ZERO.                   
011500         05  W-LOC-ADPLATS.                                               
011600             07 W-LOC-BAY        PIC 9(2) VALUE ZERO.                     
011700             07 W-LOC-ADLEVEL    PIC 9(2) VALUE ZERO.                     
011800             07 W-LOC-ADSEQ      PIC 9(1) VALUE ZERO.                     
011900                                                                          
012000     03  W-WDJ8KEY-MIN-X.                                                 
012100         05  W-LOC-IDDC-MIN       PIC X(2)   VALUE SPACE.                 
012200         05  W-LOC-ADLAGOMR-MIN   PIC 9(2)   VALUE ZERO.                  
012300         05  W-LOC-ADGANG-MIN     PIC 9(2)   VALUE ZERO.                  
012400         05  W-LOC-ADPLATS-MIN.                                           
012500             07 W-LOC-BAY-MIN     PIC 9(2) VALUE ZERO.                    
012600             07 W-LOC-ADLEVEL-MIN PIC 9(2) VALUE ZERO.                    
012700             07 W-LOC-ADSEQ-MIN   PIC 9(1) VALUE ZERO.                    
012800     03  W-WDJ8KEY-MAX-X.                                                 
012900         05  W-LOC-IDDC-MAX       PIC X(2)   VALUE SPACE.                 
013000         05  W-LOC-ADLAGOMR-MAX   PIC 9(2)   VALUE 99.                    
013100         05  W-LOC-ADGANG-MAX     PIC 9(2)   VALUE 99.                    
013200         05  W-LOC-ADPLATS-MAX.                                           
013300             07 W-LOC-BAY-MAX     PIC 9(2) VALUE 99.                      
013400             07 W-LOC-ADLEVEL-MAX PIC 9(2) VALUE 99.                      
013500             07 W-LOC-ADSEQ-MAX   PIC 9(1) VALUE 9.                       
013600                                                                          
013700     03   W-WDD8A1KY-MIN-X.                                               
013800          05 W-IDDC-D8-MIN-X     PIC X(2)           VALUE SPACE.          
013900          05 W-ADBUFFOM-D8-MIN-X PIC S9(3)  COMP-3  VALUE ZERO.           
014000          05 W-ADBUFGAN-D8-MIN-X PIC S9(3)  COMP-3  VALUE ZERO.           
014100          05 W-ADBUFPL-D8-MIN-X  PIC S9(5)  COMP-3  VALUE ZERO.           
014200          05 W-DABUFPAF-D8-MIN-X PIC 9(8)           VALUE ZERO.           
014300          05 W-IDARTNR-D8-MIN-X  PIC S9(9)  COMP-3  VALUE ZERO.           
014400                                                                          
014500     03 W-WDD8A1KY-MAX-X.                                                 
014600        05 W-IDDC-D8-MAX-X     PIC X(2)         VALUE SPACE.              
014700        05 W-ADBUFFOM-D8-MAX-X PIC S9(3) COMP-3 VALUE +999.               
014800        05 W-ADBUFGAN-D8-MAX-X PIC S9(3) COMP-3 VALUE +999.               
014900        05 W-ADBUFPL-D8-MAX-X  PIC S9(5) COMP-3 VALUE +99999.             
015000        05 W-DABUFPAF-D8-MAX-X PIC 9(8)       VALUE 99999999.             
015100        05 W-IDARTNR-D8-MAX-X  PIC S9(9) COMP-3 VALUE +999999999.         
015200                                                                          
015300     03 WDK7A1KY-MIN-X.                                                   
015400        05 W-IDDC-K7-MIN        PIC X(2)         VALUE SPACE.             
015500        05 W-ADART-K7-MIN.                                                
015600          07  W-ADLAGOMR-K7-MIN PIC S9(3)  COMP-3  VALUE ZERO.            
015700          07  W-ADGANG-K7-MIN   PIC S9(3)  COMP-3  VALUE ZERO.            
015800          07  W-ADPLATS-K7-MIN  PIC S9(5)  COMP-3  VALUE ZERO.            
015900        05 W-IDARTNR-K7-MIN     PIC S9(9)  COMP-3  VALUE ZERO.            
016000                                                                          
016100     03 WDK7A1KY-MAX-X.                                                   
016200        05  W-IDDC-K7-MAX       PIC X(2)           VALUE SPACE.           
016300        05  W-ADART-K7-MAX.                                               
016400          07  W-ADLAGOMR-K7-MAX PIC S9(3) COMP-3 VALUE +999.              
016500          07  W-ADGANG-K7-MAX   PIC S9(3) COMP-3 VALUE +999.              
016600          07  W-ADPLATS-K7-MAX  PIC S9(5) COMP-3 VALUE +99999.            
016700        05 W-IDARTNR-K7-MAX     PIC S9(9) COMP-3 VALUE +999999999.        
016800                                                                          
016900     SKIP2                                                                
017000*    --- STATUS-KOD FRÅN IMS                                              
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FINNS                       VALUE '  '.                  
017300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017500     88  BAS-SLUT                            VALUE 'GB'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(128).                              
018100 01  SSA2                        PIC X(128).                              
018200     EJECT                                                                
018300*    --- IMS FUNKTIONSKODER                                               
018400*01  -COPY W0003                                                          
018500     EJECT                                                                
018600*    ---  DLI INPUT-OUTPUT AREA                                           
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ801'.                      
018800 01  DLI-IO-WDJ801.                                                       
018900*    03  -COPY WDJ801                                                     
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
019100 01  DLI-IO-WDK7A1.                                                       
019200*    03  -COPY WDK7A1 -PRE K7-                                            
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD8A1'.                      
019400 01  DLI-IO-WDD8A1.                                                       
019500*    03  -COPY WDD8A1                                                     
019600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019700 01   DLI-IO-WDB601.                                                      
019800*     03  -COPY WDB601                                                    
019900 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
020000 01   DLI-IO-WDK711.                                                      
020100*     03  -COPY WDK711                                                    
020200     EJECT                                                                
020300 LINKAGE SECTION.                                                         
020400                                                                          
020500                                                                          
020600*01  -COPY W0008  -PRE WDJ8-                                              
020700     05  FILLER                  PIC X.                                   
020800                                                                          
020900*01  -COPY W0008  -PRE WDK7A-                                             
021000     05  FILLER                  PIC X.                                   
021100                                                                          
021200*01  -COPY W0008  -PRE WDD8A-                                             
021300     05  FILLER                  PIC X.                                   
021400*01  -COPY W0008  -PRE WDB6-                                              
021500     05  FILLER                  PIC X.                                   
021600*01  -COPY W0008  -PRE WDK7-                                              
021700     05  FILLER                  PIC X.                                   
021800     EJECT                                                                
021900 PROCEDURE DIVISION  USING WDJ8-PCB WDK7A-PCB                             
022000     WDD8A-PCB WDB6-PCB WDK7-PCB.                                         
022100 MAIN SECTION.                                                            
022200     ENTRY 'DLITCBL' USING WDJ8-PCB WDK7A-PCB                             
022300     WDD8A-PCB WDB6-PCB WDK7-PCB                                          
022400                                                                          
022500                                                                          
022600     PERFORM A-INIT                                                       
022700     PERFORM IMS-GN-WDB601                                                
022800     PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                             
022900       IF DCS-FLWEBDC = 'J' OR 'Y'                                        
023000         MOVE DCS-IDDC TO W-IDDC                                          
023100                          W-IDDC-K7-MIN                                   
023200                          W-IDDC-K7-MAX                                   
023300                          W-LOC-IDDC-MIN                                  
023400                          W-LOC-IDDC-MAX                                  
023500*** KOLLA OM DC FINNS PÅ WDJ8                                             
023600         PERFORM IMS-GET-WDJ801-MIN-MAX                                   
023700                                                                          
023800         IF SEGMENT-FINNS                                                 
023900          PERFORM IMS-GU-WDK7A1                                           
024000          IF SEGMENT-FINNS                                                
024100            PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                      
024200              MOVE W-IDDC           TO W-LOC-IDDC                         
024300              MOVE K7-SEQA-IDARTNR  TO W-IDARTNR                          
024400              MOVE K7-SEQA-ADLAGOMR TO W-LOC-ADLAGOMR                     
024500              MOVE K7-SEQA-ADGANG   TO W-LOC-ADGANG                       
024600              MOVE K7-SEQA-ADPLATS  TO WS-ADPLATS-NUM                     
024700              MOVE WS-ADPLATS-NUM   TO WS-ADPLATS                         
024800              MOVE ZERO             TO WS-ADLEVEL                         
024900                                       WS-ADSEQ                           
025000              MOVE WS-ADPLATS       TO W-LOC-ADPLATS                      
025100                                                                          
025200                                                                          
025300              PERFORM IMS-GHU-WDJ801                                      
025400              IF SEGMENT-SAKNAS                                           
025500** * * * *      KOLLA OM ARTIKEL HAR LAGERSALDO                           
025600                PERFORM IMS-GU-WDK711                                     
025700                IF SEGMENT-FINNS                                          
025800                 IF SLAG-ADLAGOMR = 0 AND SLAG-ADGANG = 0                 
025900                    AND SLAG-ADPLATS = 0                                  
026000                  IF SLAG-KVLS NOT = 0                                    
026100                    PERFORM B-SKAPA-UTFIL                                 
026200                  END-IF                                                  
026300                 ELSE                                                     
026400                   PERFORM B-SKAPA-UTFIL                                  
026500                 END-IF                                                   
026600                END-IF                                                    
026700              END-IF                                                      
026800                                                                          
026900              PERFORM IMS-GN-WDK7A1                                       
027000            END-PERFORM                                                   
027100**** BUFFERT PLATSER ***                                                  
027200            MOVE W-IDDC            TO W-IDDC-D8-MIN-X                     
027300                                      W-IDDC-D8-MAX-X                     
027400            PERFORM IMS-GU-WDD801                                         
027500            PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                      
027600              MOVE W-IDDC           TO W-LOC-IDDC                         
027700                                                                          
027800              MOVE SEQA-IDARTNR     TO W-IDARTNR                          
027900              MOVE SEQA-ADBUFFOMR   TO W-LOC-ADLAGOMR                     
028000              MOVE SEQA-ADBUFFGANG  TO W-LOC-ADGANG                       
028100              MOVE SEQA-ADBUFFPL    TO WS-ADPLATS-NUM                     
028200              MOVE WS-ADPLATS-NUM   TO WS-ADPLATS                         
028300              MOVE ZERO             TO WS-ADLEVEL                         
028400                                       WS-ADSEQ                           
028500              MOVE WS-ADPLATS       TO W-LOC-ADPLATS                      
028600              PERFORM IMS-GHU-WDJ801                                      
028700                                                                          
028800              IF SEGMENT-SAKNAS                                           
028900                PERFORM IMS-GU-WDK711                                     
029000                IF SEGMENT-FINNS                                          
029100                  IF SLAG-ADLAGOMR = 0 AND SLAG-ADGANG  = 0               
029200                     AND SLAG-ADPLATS = 0                                 
029300                    IF SLAG-KVLS NOT = 0                                  
029400                      PERFORM BA-SKAPA-UTFIL                              
029500                    END-IF                                                
029600                  ELSE                                                    
029700                    PERFORM BA-SKAPA-UTFIL                                
029800                  END-IF                                                  
029900                END-IF                                                    
030000              END-IF                                                      
030100                                                                          
030200              PERFORM IMS-GN-WDD801                                       
030300            END-PERFORM                                                   
030400          END-IF                                                          
030500         END-IF                                                           
030600       END-IF                                                             
030700       PERFORM IMS-GN-WDB601                                              
030800       MOVE 0 TO WS-ANTAL-UT                                              
030900     END-PERFORM                                                          
031000                                                                          
031100                                                                          
031200     PERFORM Z-FINIT                                                      
031300                                                                          
031400     MOVE ZERO TO RETURN-CODE                                             
031500     GOBACK                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 A-INIT SECTION.                                                          
031900                                                                          
032000     OPEN OUTPUT W61550                                                   
032100                                                                          
032200     ACCEPT DAGENS-DATUM  FROM DATE                                       
032300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
032400     MOVE 0 TO WS-ANTAL-UT                                                
032500     .                                                                    
032600     EJECT                                                                
032700 B-SKAPA-UTFIL SECTION.                                                   
032800     IF WS-ANTAL-UT < 500                                                 
032900       MOVE K7-SEQA-IDDC              TO UT-IDDC                          
033000       MOVE K7-SEQA-IDARTNR           TO UT-IDARTNR                       
033100       MOVE K7-SEQA-ADLAGOMR          TO UT-ADLAGOMR                      
033200       MOVE K7-SEQA-ADGANG            TO UT-ADGANG                        
033300       MOVE K7-SEQA-ADPLATS           TO UT-ADPLATS                       
033600       PERFORM S11-SKRIV-W61550                                           
033700       COMPUTE WS-ANTAL-UT = WS-ANTAL-UT + 1                              
033800     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 BA-SKAPA-UTFIL SECTION.                                                  
034200     IF WS-ANTAL-UT < 500                                                 
034300       MOVE SEQA-IDDC                 TO UT-IDDC                          
034400       MOVE SEQA-IDARTNR              TO UT-IDARTNR                       
034500       MOVE SEQA-ADBUFFOMR            TO UT-ADLAGOMR                      
034600       MOVE SEQA-ADBUFFGANG           TO UT-ADGANG                        
034700       MOVE SEQA-ADBUFFPL             TO UT-ADPLATS                       
035000       PERFORM S11-SKRIV-W61550                                           
035100       COMPUTE WS-ANTAL-UT = WS-ANTAL-UT + 1                              
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 Z-FINIT SECTION.                                                         
035600     CLOSE W61550                                                         
035700     SKIP2                                                                
035800     MOVE 'S' TO POSTSUM-OPKOD                                            
035900     CALL POSTSUM USING POSTSUM-PARM                                      
036000     .                                                                    
036100     EJECT                                                                
036200 S11-SKRIV-W61550 SECTION.                                                
036300                                                                          
036400     WRITE UT-POST FROM UT-AREA                                           
036500                                                                          
036600     MOVE 'UT-'      TO POSTSUM-TRANSTYP                                  
036700     MOVE W-IDDC     TO POSTSUM-FDNAMN                                    
036800     MOVE 'W61550D1' TO POSTSUM-DDNAMN2                                   
036900     CALL POSTSUM USING POSTSUM-PARM                                      
037000     .                                                                    
037100     EJECT                                                                
038000* --- IMS SEKTIONER ---                                                   
038100                                                                          
038200     EJECT                                                                
038300 IMS-GN-WDB601    SECTION.                                                
038400*    STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
038500*         DELIMITED BY SIZE INTO SSA1                                     
038600     MOVE 'WDB601  ' TO SSA1                                              
038700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
038800     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
038900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
039000     PERFORM IMS-STATUSKONTROLL                                           
039100     IF SEGMENT-SAKNAS                                                    
039200         MOVE SPACE TO DCS-KDDC                                           
039300     END-IF                                                               
039400     .                                                                    
039500 IMS-GHU-WDJ801 SECTION.                                                  
039600                                                                          
039700     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
039800          DELIMITED BY SIZE INTO SSA1                                     
039900     MOVE '  GE' TO GODK-STATUSKODER                                      
040000     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
040100     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
040200     PERFORM IMS-STATUSKONTROLL                                           
040300     .                                                                    
040400     SKIP3                                                                
041500 IMS-GET-WDJ801-MIN-MAX SECTION.                                          
041600                                                                          
041700     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
041800                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
041900          DELIMITED BY SIZE INTO SSA1                                     
042000     MOVE '  GE' TO GODK-STATUSKODER                                      
042100     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
042200     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
042300     PERFORM IMS-STATUSKONTROLL                                           
042400     .                                                                    
042500     SKIP3                                                                
043700 IMS-GU-WDD801 SECTION.                                                   
043800     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
043900                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
044000                                                                          
044100            DELIMITED BY SIZE INTO SSA1                                   
044200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
044300     CALL CBLTDLI USING GU WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
044400     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
044500     PERFORM IMS-STATUSKONTROLL                                           
044600     .                                                                    
044700     EJECT                                                                
044800 IMS-GN-WDD801 SECTION.                                                   
044900     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
045000                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
045100                                                                          
045200            DELIMITED BY SIZE INTO SSA1                                   
045300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
045400     CALL CBLTDLI USING GN WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
045500     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     .                                                                    
045800     EJECT                                                                
045900 IMS-GU-WDK7A1 SECTION.                                                   
046000     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
046100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
046200          DELIMITED BY SIZE INTO SSA1                                     
046300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
046400     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
046500     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
046600     PERFORM IMS-STATUSKONTROLL                                           
046700     .                                                                    
046800     SKIP3                                                                
046900 IMS-GN-WDK7A1 SECTION.                                                   
047000     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
047100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
047200          DELIMITED BY SIZE INTO SSA1                                     
047300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
047400     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
047500     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
047600     PERFORM IMS-STATUSKONTROLL                                           
047700     .                                                                    
047800     SKIP3                                                                
047900 IMS-GU-WDK711 SECTION.                                                   
048000                                                                          
048100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
048200     DELIMITED BY SIZE INTO SSA1                                          
048300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
048400     DELIMITED BY SIZE INTO SSA2                                          
048500     MOVE '  ' TO GODK-STATUSKODER                                        
048600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
048700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
048800     PERFORM IMS-STATUSKONTROLL                                           
048900     .                                                                    
049000     SKIP2                                                                
049100 IMS-STATUSKONTROLL SECTION.                                              
049200                                                                          
049300     SET STATUS-IX TO 1                                                   
049400     SEARCH GODK-STATUS                                                   
049500       AT END                                                             
049600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
049700           DELIMITED BY SIZE INTO FELTEXT                                 
049800         DISPLAY FELTEXT                                                  
049900         CALL FELLOG                                                      
050000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050100         CONTINUE                                                         
050200     END-SEARCH                                                           
050300     .                                                                    
