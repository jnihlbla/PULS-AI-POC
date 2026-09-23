000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6155200.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/04/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WDJ8 OCH KONTROLLERAR ATT MAN INTE HAR FÖR MÅNGA           
000900*        ARTIKLAR PÅ EN LAGERPLATS, ISÅFALL SKAPAS EN UTPOST              
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDJ8                                       
001200*        PROGRAMMET LÄSER      WDK7A                                      
001300*        PROGRAMMET LÄSER      WDD8                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- UTFIL MED LAGEROMRÅDEN MED FÖR MÅNGA ARTKLAR PÅ            
002800     SELECT W61552                     ASSIGN TO W61552D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W61552                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W61552 -PRE  UT-  -L.                                     
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W6155200'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-ADLAGOMR-NUM             PIC  9(2).                               
004600 77  WS-ADGANG-NUM               PIC  9(2).                               
004700 77  WS-ADPLATS-NUM              PIC  9(5).                               
004800 77  W-KVPLATS-WDJ8              PIC 9(3)   VALUE 0.                      
004900 77  W-KVPLATS-WDK7              PIC 9(3)   VALUE 0.                      
005000 77  W-KVPLATS-WDD8              PIC 9(3)   VALUE 0.                      
005100 77  W-KVPLATS-TOT               PIC 9(3)   VALUE 0.                      
005200 01  WS-ADPLATS.                                                          
005300     03 WS-BAY                    PIC 9(2) VALUE ZERO.                    
005400     03 WS-ADLEVEL                PIC 9(2) VALUE ZERO.                    
005500     03 WS-ADSEQ                  PIC 9(1) VALUE ZERO.                    
005600                                                                          
005700 01  WS-ADPLATS-MIN.                                                      
005800     03 WS-BAY-MIN                PIC 9(2) VALUE ZERO.                    
005900     03 WS-ADLEVEL-MIN            PIC 9(2) VALUE ZERO.                    
006000     03 WS-ADSEQ-MIN              PIC 9(1) VALUE ZERO.                    
006100                                                                          
006200 01  WS-ADPLATS-MAX.                                                      
006300     03 WS-BAY-MAX                PIC 9(2) VALUE 99.                      
006400     03 WS-ADLEVEL-MAX            PIC 9(2) VALUE 99.                      
006500     03 WS-ADSEQ-MAX              PIC 9(1) VALUE 9.                       
006600     EJECT                                                                
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES DAGENS-DATUM.                                       
006900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007200     EJECT                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007900     SKIP2                                                                
008000*    --- PARAMETRAR TILL ABEND                                            
008100                                                                          
008200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008500     SKIP2                                                                
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL POSTSUM                                          
009100*                                                                         
009200*01  -COPY W0005   -PRE  POSTSUM-                                         
009300     EJECT                                                                
009400 01  UT-AREA-START               PIC X(24)   VALUE                        
009500                                 'UT-AREA-START  '.                       
009600     SKIP2                                                                
009700                                                                          
009800*01  AREA -COPY W61552     -PRE UT-                                       
009900     EJECT                                                                
010000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010100*                                                                         
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010600     03  W-IDDC-X.                                                        
010700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
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
019900     EJECT                                                                
020000 LINKAGE SECTION.                                                         
020100                                                                          
020200                                                                          
020300*01  -COPY W0008  -PRE WDJ8-                                              
020400     05  FILLER                  PIC X.                                   
020500                                                                          
020600*01  -COPY W0008  -PRE WDK7A-                                             
020700     05  FILLER                  PIC X.                                   
020800                                                                          
020900*01  -COPY W0008  -PRE WDD8A-                                             
021000     05  FILLER                  PIC X.                                   
021100*01  -COPY W0008  -PRE WDB6-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400 PROCEDURE DIVISION  USING WDJ8-PCB WDK7A-PCB                             
021500     WDD8A-PCB WDB6-PCB.                                                  
021600 MAIN SECTION.                                                            
021700     ENTRY 'DLITCBL' USING WDJ8-PCB WDK7A-PCB                             
021800     WDD8A-PCB WDB6-PCB.                                                  
021900     PERFORM A-INIT                                                       
022000     PERFORM IMS-GN-WDB601                                                
022100     PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                             
022200     IF (DCS-FLWEBDC = 'J' OR 'Y')                                        
022210        AND (DCS-IDDC NOT = '61' AND '43' AND '41')                       
022300***     IF  DCS-IDDC = '1A'                                               
022400         MOVE DCS-IDDC TO W-IDDC                                          
022500                          W-IDDC-K7-MIN                                   
022600                          W-IDDC-K7-MAX                                   
022700                          W-IDDC-D8-MIN-X                                 
022800                          W-IDDC-D8-MAX-X                                 
022900                          W-LOC-IDDC-MIN                                  
023000                          W-LOC-IDDC-MAX                                  
023100         PERFORM IMS-GET-WDJ801-MIN-MAX                                   
023200         PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                         
023300           MOVE LOC-ADLAGOMR     TO W-ADLAGOMR-K7-MIN                     
023400                                    W-ADLAGOMR-K7-MAX                     
023500           MOVE LOC-ADGANG       TO W-ADGANG-K7-MIN                       
023600                                    W-ADGANG-K7-MAX                       
023700           MOVE LOC-ADPLATS      TO WS-ADPLATS                            
023800                                                                          
023900           MOVE WS-BAY           TO WS-BAY-MIN                            
024000                                    WS-BAY-MAX                            
024100           MOVE WS-ADPLATS-MIN   TO WS-ADPLATS-NUM                        
024200           MOVE WS-ADPLATS-NUM   TO W-ADPLATS-K7-MIN                      
024300                                                                          
024400           MOVE WS-ADPLATS-MAX   TO WS-ADPLATS-NUM                        
024500           MOVE WS-ADPLATS-NUM   TO W-ADPLATS-K7-MAX                      
024600           MOVE LOC-KVPLATS      TO W-KVPLATS-WDJ8                        
024700*** LÄS WDK7 ***                                                          
024800           PERFORM IMS-GU-WDK7A1                                          
024900           PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                       
025000             COMPUTE W-KVPLATS-WDK7 = W-KVPLATS-WDK7 + 1                  
025100             PERFORM IMS-GN-WDK7A1                                        
025200           END-PERFORM                                                    
025300                                                                          
025400           MOVE LOC-ADLAGOMR        TO W-ADBUFFOM-D8-MIN-X                
025500           MOVE LOC-ADGANG          TO W-ADBUFGAN-D8-MIN-X                
025600           MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                     
025700           MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MIN-X                 
025800                                                                          
025900           MOVE LOC-ADLAGOMR        TO W-ADBUFFOM-D8-MAX-X                
026000           MOVE LOC-ADGANG          TO W-ADBUFGAN-D8-MAX-X                
026100           MOVE WS-ADPLATS-MAX      TO WS-ADPLATS-NUM                     
026200           MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MAX-X                 
026300** LÄS WDD8 BUFFERT                                                       
026400                                                                          
026500           PERFORM IMS-GU-WDD801                                          
026600           PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                       
026700             COMPUTE W-KVPLATS-WDD8 = W-KVPLATS-WDD8 + 1                  
026800             PERFORM IMS-GN-WDD801                                        
026900           END-PERFORM                                                    
027000                                                                          
027100           COMPUTE W-KVPLATS-TOT =                                        
027200                   W-KVPLATS-WDK7 + W-KVPLATS-WDD8                        
027300           IF W-KVPLATS-TOT > W-KVPLATS-WDJ8                              
027400             PERFORM B-SKAPA-UTFIL                                        
027500           END-IF                                                         
027600                                                                          
027700           MOVE ZERO TO  W-KVPLATS-WDK7                                   
027800                         W-KVPLATS-WDD8                                   
027900                         W-KVPLATS-WDJ8                                   
028000                         W-KVPLATS-TOT                                    
028100         PERFORM IMS-GN-WDJ801-MIN-MAX                                    
028200         END-PERFORM                                                      
028300**      END-IF                                                            
028400     END-IF                                                               
028500     PERFORM IMS-GN-WDB601                                                
028600     END-PERFORM                                                          
028700                                                                          
028800                                                                          
028900     PERFORM Z-FINIT                                                      
029000                                                                          
029100     MOVE ZERO TO RETURN-CODE                                             
029200     GOBACK                                                               
029300     .                                                                    
029400     EJECT                                                                
029500 A-INIT SECTION.                                                          
029600                                                                          
029700     OPEN OUTPUT W61552                                                   
029800                                                                          
029900     ACCEPT DAGENS-DATUM  FROM DATE                                       
030000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
030100     .                                                                    
030200     EJECT                                                                
030300 B-SKAPA-UTFIL SECTION.                                                   
030400     MOVE W-IDDC                    TO UT-IDDC                            
030500     MOVE LOC-ADLAGOMR              TO UT-ADLAGOMR                        
030600     MOVE LOC-ADGANG                TO UT-ADGANG                          
030700     MOVE LOC-ADPLATS               TO UT-ADPLATS                         
030800     MOVE W-KVPLATS-WDJ8            TO UT-KVPLATS                         
030900     MOVE W-KVPLATS-TOT             TO UT-KVANTART                        
031000     PERFORM S11-SKRIV-W61552                                             
031100     .                                                                    
031200     EJECT                                                                
031300 Z-FINIT SECTION.                                                         
031400     CLOSE W61552                                                         
031500     SKIP2                                                                
031600     MOVE 'S' TO POSTSUM-OPKOD                                            
031700     CALL POSTSUM USING POSTSUM-PARM                                      
031800     .                                                                    
031900     EJECT                                                                
032000 S11-SKRIV-W61552 SECTION.                                                
032100                                                                          
032200     WRITE UT-POST FROM UT-AREA                                           
032300                                                                          
032400     MOVE 'UT-'      TO POSTSUM-TRANSTYP                                  
032500     MOVE W-IDDC     TO POSTSUM-FDNAMN                                    
032600     MOVE 'W61552D1' TO POSTSUM-DDNAMN2                                   
032700     CALL POSTSUM USING POSTSUM-PARM                                      
032800     .                                                                    
032900     EJECT                                                                
033000 S99-ABEND SECTION.                                                       
033100                                                                          
033200     SKIP2                                                                
033300     MOVE 'S' TO POSTSUM-OPKOD                                            
033400     CALL POSTSUM USING POSTSUM-PARM                                      
033500     CALL ABEND USING RKOD-ABEND                                          
033600     .                                                                    
033700     EJECT                                                                
033800* --- IMS SEKTIONER ---                                                   
033900                                                                          
034000     EJECT                                                                
034100 IMS-GN-WDB601    SECTION.                                                
034200*    STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
034300*         DELIMITED BY SIZE INTO SSA1                                     
034400     MOVE 'WDB601  ' TO SSA1                                              
034500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
034600     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
034700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
034800     PERFORM IMS-STATUSKONTROLL                                           
034900     IF SEGMENT-SAKNAS                                                    
035000         MOVE SPACE TO DCS-KDDC                                           
035100     END-IF                                                               
035200     .                                                                    
035300 IMS-GHU-WDJ801 SECTION.                                                  
035400                                                                          
035500     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
035600          DELIMITED BY SIZE INTO SSA1                                     
035700     MOVE '  GE' TO GODK-STATUSKODER                                      
035800     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
035900     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
036000     PERFORM IMS-STATUSKONTROLL                                           
036100     .                                                                    
036200     SKIP3                                                                
036300 IMS-GN-WDJ801 SECTION.                                                   
036400                                                                          
036500     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
036600          DELIMITED BY SIZE INTO SSA1                                     
036700     MOVE '  GE' TO GODK-STATUSKODER                                      
036800     CALL CBLTDLI USING GN  WDJ8-PCB DLI-IO-WDJ801 SSA1                   
036900     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
037000     PERFORM IMS-STATUSKONTROLL                                           
037100     .                                                                    
037200     SKIP3                                                                
037300 IMS-GET-WDJ801-MIN-MAX SECTION.                                          
037400                                                                          
037500     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
037600                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
037700          DELIMITED BY SIZE INTO SSA1                                     
037800     MOVE '  GE' TO GODK-STATUSKODER                                      
037900     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
038000     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
038100     PERFORM IMS-STATUSKONTROLL                                           
038200     .                                                                    
038300     SKIP3                                                                
038400 IMS-GN-WDJ801-MIN-MAX SECTION.                                           
038500                                                                          
038600     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
038700                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
038800          DELIMITED BY SIZE INTO SSA1                                     
038900     MOVE '  GE' TO GODK-STATUSKODER                                      
039000     CALL CBLTDLI USING GN WDJ8-PCB DLI-IO-WDJ801 SSA1                    
039100     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039400     SKIP3                                                                
039500 IMS-GU-WDD801 SECTION.                                                   
039600     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
039700                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
039800                                                                          
039900            DELIMITED BY SIZE INTO SSA1                                   
040000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
040100     CALL CBLTDLI USING GU WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
040200     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
040300     PERFORM IMS-STATUSKONTROLL                                           
040400     .                                                                    
040500     EJECT                                                                
040600 IMS-GN-WDD801 SECTION.                                                   
040700     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
040800                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
040900                                                                          
041000            DELIMITED BY SIZE INTO SSA1                                   
041100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
041200     CALL CBLTDLI USING GN WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
041300     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     .                                                                    
041600     EJECT                                                                
041700 IMS-GU-WDK7A1 SECTION.                                                   
041800     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
041900                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
042000          DELIMITED BY SIZE INTO SSA1                                     
042100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
042200     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
042300     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
042400     PERFORM IMS-STATUSKONTROLL                                           
042500     .                                                                    
042600     SKIP3                                                                
042700 IMS-GN-WDK7A1 SECTION.                                                   
042800     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
042900                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
043000          DELIMITED BY SIZE INTO SSA1                                     
043100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
043200     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
043300     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
043400     PERFORM IMS-STATUSKONTROLL                                           
043500     .                                                                    
043600     SKIP3                                                                
043700 IMS-STATUSKONTROLL SECTION.                                              
043800                                                                          
043900     SET STATUS-IX TO 1                                                   
044000     SEARCH GODK-STATUS                                                   
044100       AT END                                                             
044200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044300           DELIMITED BY SIZE INTO FELTEXT                                 
044400         DISPLAY FELTEXT                                                  
044500         CALL FELLOG                                                      
044600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044700         CONTINUE                                                         
044800     END-SEARCH                                                           
044900     .                                                                    
