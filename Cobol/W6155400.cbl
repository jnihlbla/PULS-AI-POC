000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6155400.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/04/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WDJ8 OCH KONTROLLERAR BELÄGGNING PÅ LAGEROMRÅDEN           
000900*        O PROCENT, EN UTPOST PER LAGEROMR + EN TOTAL POST PER DC         
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
002700*          --- UTFIL MED ARTIKLAR MED ICKE DEFINIERADE PLATSER            
002800     SELECT W61554                     ASSIGN TO W61554D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W61554                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W61554 -PRE  UT-  -L.                                     
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W6155400'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-ADLAGOMR-NUM             PIC  9(2)   VALUE 0.                     
004600 77  WS-ADGANG-NUM               PIC  9(2)   VALUE 0.                     
004700 77  WS-ADPLATS-NUM              PIC  9(5)   VALUE 0.                     
004800 77  WS-SPAR-ADLAGOMR            PIC  9(2)   VALUE 0.                     
004900 77  WS-SPAR-ADGANG              PIC  9(2)   VALUE 0.                     
005000 77  WS-SPAR-IDDC                PIC  X(2)  VALUE SPACE.                  
005100 77  W-KVPLATS-WDJ8              PIC 9(6)   VALUE 0.                      
005200 77  W-KVPLATS-WDJ8-TOT          PIC 9(6)   VALUE 0.                      
005300 77  W-KVPLATS-WDK7              PIC 9(6)   VALUE 0.                      
005400 77  W-KVPLATS-WDD8              PIC 9(6)   VALUE 0.                      
005500 77  W-KVPLATS-TOT               PIC 9(6)   VALUE 0.                      
005600 77  W-KVPLATS-STOT              PIC 9(6)   VALUE 0.                      
005700 77  W-REDCBEL                   PIC 9(4).9(3) VALUE ZERO.                
005800 77  W-RELOBEL                   PIC 9(4).9(3) VALUE ZERO.                
005900 01  WS-ADPLATS.                                                          
006000     03 WS-BAY                    PIC 9(2) VALUE ZERO.                    
006100     03 WS-ADLEVEL                PIC 9(2) VALUE ZERO.                    
006200     03 WS-ADSEQ                  PIC 9(1) VALUE ZERO.                    
006300                                                                          
006400 01  WS-ADPLATS-MIN.                                                      
006500     03 WS-BAY-MIN                PIC 9(2) VALUE ZERO.                    
006600     03 WS-ADLEVEL-MIN            PIC 9(2) VALUE ZERO.                    
006700     03 WS-ADSEQ-MIN              PIC 9(1) VALUE ZERO.                    
006800                                                                          
006900 01  WS-ADPLATS-MAX.                                                      
007000     03 WS-BAY-MAX                PIC 9(2) VALUE 99.                      
007100     03 WS-ADLEVEL-MAX            PIC 9(2) VALUE 99.                      
007200     03 WS-ADSEQ-MAX              PIC 9(1) VALUE 9.                       
007300                                                                          
007400 77  POST-FANNS-SW                PIC X      VALUE 'N'.                   
007500     88  POST-FANNS                          VALUE 'J'.                   
007600     88  POST-SAKNAS                         VALUE 'N'.                   
007700     EJECT                                                                
007800                                                                          
007900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008000 01  FILLER REDEFINES DAGENS-DATUM.                                       
008100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008400     EJECT                                                                
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008600*                                                                         
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     SKIP2                                                                
009200*    --- PARAMETRAR TILL ABEND                                            
009300                                                                          
009400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009700     SKIP2                                                                
009800 01  FELTEXT.                                                             
009900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL POSTSUM                                          
010300*                                                                         
010400*01  -COPY W0005   -PRE  POSTSUM-                                         
010500     EJECT                                                                
010600 01  UT-AREA-START               PIC X(24)   VALUE                        
010700                                 'UT-AREA-START  '.                       
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY W61554     -PRE UT-                                       
011100     EJECT                                                                
011200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  NYCKLAR-TILL-DLI.                                                    
011800     03  W-IDDC-X.                                                        
011900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012000     03  W-IDDC-B6-X.                                                     
012100         05 W-IDDC-B6            PIC X(2).                                
012200                                                                          
012300     03  W-WDJ8KEY-X.                                                     
012400         05  W-LOC-IDDC          PIC X(2)   VALUE SPACE.                  
012500         05  W-LOC-ADLAGOMR      PIC 9(2)   VALUE ZERO.                   
012600         05  W-LOC-ADGANG        PIC 9(2)   VALUE ZERO.                   
012700         05  W-LOC-ADPLATS.                                               
012800             07 W-LOC-BAY        PIC 9(2) VALUE ZERO.                     
012900             07 W-LOC-ADLEVEL    PIC 9(2) VALUE ZERO.                     
013000             07 W-LOC-ADSEQ      PIC 9(1) VALUE ZERO.                     
013100                                                                          
013200     03  W-WDJ8KEY-MIN-X.                                                 
013300         05  W-LOC-IDDC-MIN       PIC X(2)   VALUE SPACE.                 
013400         05  W-LOC-ADLAGOMR-MIN   PIC 9(2)   VALUE ZERO.                  
013500         05  W-LOC-ADGANG-MIN     PIC 9(2)   VALUE ZERO.                  
013600         05  W-LOC-ADPLATS-MIN.                                           
013700             07 W-LOC-BAY-MIN     PIC 9(2) VALUE ZERO.                    
013800             07 W-LOC-ADLEVEL-MIN PIC 9(2) VALUE ZERO.                    
013900             07 W-LOC-ADSEQ-MIN   PIC 9(1) VALUE ZERO.                    
014000     03  W-WDJ8KEY-MAX-X.                                                 
014100         05  W-LOC-IDDC-MAX       PIC X(2)   VALUE SPACE.                 
014200         05  W-LOC-ADLAGOMR-MAX   PIC 9(2)   VALUE 99.                    
014300         05  W-LOC-ADGANG-MAX     PIC 9(2)   VALUE 99.                    
014400         05  W-LOC-ADPLATS-MAX.                                           
014500             07 W-LOC-BAY-MAX     PIC 9(2) VALUE 99.                      
014600             07 W-LOC-ADLEVEL-MAX PIC 9(2) VALUE 99.                      
014700             07 W-LOC-ADSEQ-MAX   PIC 9(1) VALUE 9.                       
014800                                                                          
014900     03   W-WDD8A1KY-MIN-X.                                               
015000          05 W-IDDC-D8-MIN-X     PIC X(2)           VALUE SPACE.          
015100          05 W-ADBUFFOM-D8-MIN-X PIC S9(3)  COMP-3  VALUE ZERO.           
015200          05 W-ADBUFGAN-D8-MIN-X PIC S9(3)  COMP-3  VALUE ZERO.           
015300          05 W-ADBUFPL-D8-MIN-X  PIC S9(5)  COMP-3  VALUE ZERO.           
015400          05 W-DABUFPAF-D8-MIN-X PIC 9(8)           VALUE ZERO.           
015500          05 W-IDARTNR-D8-MIN-X  PIC S9(9)  COMP-3  VALUE ZERO.           
015600                                                                          
015700     03 W-WDD8A1KY-MAX-X.                                                 
015800        05 W-IDDC-D8-MAX-X     PIC X(2)         VALUE SPACE.              
015900        05 W-ADBUFFOM-D8-MAX-X PIC S9(3) COMP-3 VALUE +999.               
016000        05 W-ADBUFGAN-D8-MAX-X PIC S9(3) COMP-3 VALUE +999.               
016100        05 W-ADBUFPL-D8-MAX-X  PIC S9(5) COMP-3 VALUE +99999.             
016200        05 W-DABUFPAF-D8-MAX-X PIC 9(8)       VALUE 99999999.             
016300        05 W-IDARTNR-D8-MAX-X  PIC S9(9) COMP-3 VALUE +999999999.         
016400                                                                          
016500     03 WDK7A1KY-MIN-X.                                                   
016600        05 W-IDDC-K7-MIN        PIC X(2)         VALUE SPACE.             
016700        05 W-ADART-K7-MIN.                                                
016800          07  W-ADLAGOMR-K7-MIN PIC S9(3)  COMP-3  VALUE ZERO.            
016900          07  W-ADGANG-K7-MIN   PIC S9(3)  COMP-3  VALUE ZERO.            
017000          07  W-ADPLATS-K7-MIN  PIC S9(5)  COMP-3  VALUE ZERO.            
017100        05 W-IDARTNR-K7-MIN     PIC S9(9)  COMP-3  VALUE ZERO.            
017200                                                                          
017300     03 WDK7A1KY-MAX-X.                                                   
017400        05  W-IDDC-K7-MAX       PIC X(2)           VALUE SPACE.           
017500        05  W-ADART-K7-MAX.                                               
017600          07  W-ADLAGOMR-K7-MAX PIC S9(3) COMP-3 VALUE +999.              
017700          07  W-ADGANG-K7-MAX   PIC S9(3) COMP-3 VALUE +999.              
017800          07  W-ADPLATS-K7-MAX  PIC S9(5) COMP-3 VALUE +99999.            
017900        05 W-IDARTNR-K7-MAX     PIC S9(9) COMP-3 VALUE +999999999.        
018000                                                                          
018100     SKIP2                                                                
018200*    --- STATUS-KOD FRÅN IMS                                              
018300 01  STATUS-WS                   PIC XX.                                  
018400     88  SEGMENT-FINNS                       VALUE '  '.                  
018500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     88  BAS-SLUT                            VALUE 'GB'.                  
018800     SKIP2                                                                
018900 01  GODK-STATUSKODER.                                                    
019000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019100     SKIP3                                                                
019200 01  SSA1                        PIC X(128).                              
019300 01  SSA2                        PIC X(128).                              
019400     EJECT                                                                
019500*    --- IMS FUNKTIONSKODER                                               
019600*01  -COPY W0003                                                          
019700     EJECT                                                                
019800*    ---  DLI INPUT-OUTPUT AREA                                           
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ801'.                      
020000 01  DLI-IO-WDJ801.                                                       
020100*    03  -COPY WDJ801                                                     
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
020300 01  DLI-IO-WDK7A1.                                                       
020400*    03  -COPY WDK7A1 -PRE K7-                                            
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD8A1'.                      
020600 01  DLI-IO-WDD8A1.                                                       
020700*    03  -COPY WDD8A1                                                     
020800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020900 01   DLI-IO-WDB601.                                                      
021000*     03  -COPY WDB601                                                    
021100     EJECT                                                                
021200 LINKAGE SECTION.                                                         
021300                                                                          
021400                                                                          
021500*01  -COPY W0008  -PRE WDJ8-                                              
021600     05  FILLER                  PIC X.                                   
021700                                                                          
021800*01  -COPY W0008  -PRE WDK7A-                                             
021900     05  FILLER                  PIC X.                                   
022000                                                                          
022100*01  -COPY W0008  -PRE WDD8A-                                             
022200     05  FILLER                  PIC X.                                   
022300*01  -COPY W0008  -PRE WDB6-                                              
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600 PROCEDURE DIVISION  USING WDJ8-PCB WDK7A-PCB                             
022700     WDD8A-PCB WDB6-PCB.                                                  
022800 MAIN SECTION.                                                            
022900     ENTRY 'DLITCBL' USING WDJ8-PCB WDK7A-PCB                             
023000     WDD8A-PCB WDB6-PCB.                                                  
023100     PERFORM A-INIT                                                       
023200                                                                          
023300     MOVE NEJ TO POST-FANNS-SW                                            
023400     PERFORM IMS-GN-WDB601                                                
023500                                                                          
023600     PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                             
023700     IF (DCS-FLWEBDC = 'J' OR 'Y')                                        
023710        AND (DCS-IDDC NOT = '61' AND '43' AND '41')                       
023800       MOVE DCS-IDDC TO W-IDDC                                            
023900                        W-IDDC-K7-MIN                                     
024000                        W-IDDC-K7-MAX                                     
024100                        W-IDDC-D8-MIN-X                                   
024200                        W-IDDC-D8-MAX-X                                   
024300                        W-LOC-IDDC-MIN                                    
024400                        W-LOC-IDDC-MAX                                    
024500       PERFORM IMS-GET-WDJ801-MIN-MAX                                     
024600                                                                          
024700       IF SEGMENT-FINNS                                                   
024800         MOVE LOC-ADLAGOMR TO WS-SPAR-ADLAGOMR                            
024900         MOVE LOC-ADGANG   TO WS-SPAR-ADGANG                              
025000         MOVE DCS-IDDC     TO WS-SPAR-IDDC                                
025100       END-IF                                                             
025200                                                                          
025300       PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                           
025400         MOVE LOC-ADLAGOMR     TO W-ADLAGOMR-K7-MIN                       
025500                                  W-ADLAGOMR-K7-MAX                       
025600         MOVE LOC-ADGANG       TO W-ADGANG-K7-MIN                         
025700                                  W-ADGANG-K7-MAX                         
025800         MOVE LOC-ADPLATS      TO WS-ADPLATS                              
025900                                                                          
026000         MOVE WS-BAY           TO WS-BAY-MIN                              
026100                                  WS-BAY-MAX                              
026200         MOVE WS-ADPLATS-MIN   TO WS-ADPLATS-NUM                          
026300         MOVE WS-ADPLATS-NUM   TO W-ADPLATS-K7-MIN                        
026400                                                                          
026500         MOVE WS-ADPLATS-MAX   TO WS-ADPLATS-NUM                          
026600         MOVE WS-ADPLATS-NUM   TO W-ADPLATS-K7-MAX                        
026700****     MOVE LOC-KVPLATS      TO W-KVPLATS-WDJ8                          
026800* LÄS WDK7 ***                                                            
026900         PERFORM IMS-GU-WDK7A1                                            
027000                                                                          
027100         PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                         
027200           MOVE JA TO POST-FANNS-SW                                       
027300           COMPUTE W-KVPLATS-WDK7 = W-KVPLATS-WDK7 + 1                    
027400           PERFORM IMS-GN-WDK7A1                                          
027500         END-PERFORM                                                      
027600                                                                          
027700         MOVE LOC-ADLAGOMR        TO W-ADBUFFOM-D8-MIN-X                  
027800         MOVE LOC-ADGANG          TO W-ADBUFGAN-D8-MIN-X                  
027900         MOVE WS-ADPLATS-MIN      TO WS-ADPLATS-NUM                       
028000         MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MIN-X                   
028100                                                                          
028200         MOVE LOC-ADLAGOMR        TO W-ADBUFFOM-D8-MAX-X                  
028300         MOVE LOC-ADGANG          TO W-ADBUFGAN-D8-MAX-X                  
028400         MOVE WS-ADPLATS-MAX      TO WS-ADPLATS-NUM                       
028500         MOVE WS-ADPLATS-NUM      TO W-ADBUFPL-D8-MAX-X                   
028600****LÄS WDD8 BUFFERT                                                      
028700                                                                          
028800         PERFORM IMS-GU-WDD801                                            
028900                                                                          
029000         PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                         
029100           MOVE JA TO POST-FANNS-SW                                       
029200           COMPUTE W-KVPLATS-WDD8 = W-KVPLATS-WDD8 + 1                    
029300           PERFORM IMS-GN-WDD801                                          
029400         END-PERFORM                                                      
029500                                                                          
029600*** KOLLA OM POST SKALL SKRIVAS ***                                       
029700         IF WS-SPAR-ADLAGOMR NOT = LOC-ADLAGOMR                           
029800                                                                          
029900           COMPUTE W-KVPLATS-WDJ8-TOT =                                   
030000                   W-KVPLATS-WDJ8 + W-KVPLATS-WDJ8-TOT                    
030100                                                                          
030200           COMPUTE W-KVPLATS-STOT      =                                  
030300                   W-KVPLATS-STOT + W-KVPLATS-TOT                         
030400                                                                          
030500           PERFORM B-SKAPA-UTFIL                                          
030600                                                                          
030700           MOVE DCS-IDDC         TO WS-SPAR-IDDC                          
030800           MOVE LOC-ADLAGOMR     TO WS-SPAR-ADLAGOMR                      
030900                                                                          
031000           COMPUTE W-KVPLATS-TOT =                                        
031100               W-KVPLATS-WDK7 + W-KVPLATS-WDD8                            
031200           MOVE LOC-KVPLATS      TO W-KVPLATS-WDJ8                        
031300         ELSE                                                             
031400           COMPUTE W-KVPLATS-TOT =                                        
031500               W-KVPLATS-WDK7 + W-KVPLATS-WDD8 + W-KVPLATS-TOT            
031600                                                                          
031700           COMPUTE W-KVPLATS-WDJ8 = W-KVPLATS-WDJ8 + LOC-KVPLATS          
031800                                                                          
031900         END-IF                                                           
032000                                                                          
032100         MOVE ZERO TO  W-KVPLATS-WDK7                                     
032200                       W-KVPLATS-WDD8                                     
032300                                                                          
032400                                                                          
032500       PERFORM IMS-GN-WDJ801-MIN-MAX                                      
032600       END-PERFORM                                                        
032700     END-IF                                                               
032800     IF POST-FANNS                                                        
032900       PERFORM BA-SKAPA-SISTA-DC-POST                                     
033000       MOVE NEJ TO POST-FANNS-SW                                          
033100     END-IF                                                               
033200     PERFORM IMS-GN-WDB601                                                
033300                                                                          
033400     END-PERFORM                                                          
033500                                                                          
033600     IF WS-SPAR-IDDC NOT = SPACE                                          
033700       COMPUTE W-KVPLATS-WDJ8-TOT =                                       
033800               W-KVPLATS-WDJ8 + W-KVPLATS-WDJ8-TOT                        
033900                                                                          
034000       COMPUTE W-KVPLATS-STOT      =                                      
034100               W-KVPLATS-STOT + W-KVPLATS-TOT                             
034200               MOVE 'XX' TO DCS-IDDC                                      
034300       PERFORM B-SKAPA-UTFIL                                              
034400     END-IF                                                               
034500     PERFORM Z-FINIT                                                      
034600                                                                          
034700     MOVE ZERO TO RETURN-CODE                                             
034800     GOBACK                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 A-INIT SECTION.                                                          
035200                                                                          
035300     OPEN OUTPUT W61554                                                   
035400                                                                          
035500     ACCEPT DAGENS-DATUM  FROM DATE                                       
035600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035700     .                                                                    
035800     EJECT                                                                
035900 B-SKAPA-UTFIL SECTION.                                                   
036000                                                                          
036100     MOVE WS-SPAR-IDDC              TO UT-IDDC                            
036200     MOVE WS-SPAR-ADLAGOMR          TO UT-ADLAGOMR                        
036300     COMPUTE W-RELOBEL = (W-KVPLATS-TOT / W-KVPLATS-WDJ8)                 
036400                       * 100                                              
036500                                                                          
036600     MOVE W-RELOBEL                 TO UT-RELOBEL                         
036700     MOVE ZERO                      TO UT-REDCBEL                         
036800     MOVE SPACE                     TO UT-POST-TEXT                       
036900                                                                          
037000     PERFORM S11-SKRIV-W61554                                             
037100                                                                          
037200     IF WS-SPAR-IDDC NOT = DCS-IDDC                                       
037300       COMPUTE W-REDCBEL = (W-KVPLATS-STOT / W-KVPLATS-WDJ8-TOT)          
037400                          * 100                                           
037500                                                                          
037600       MOVE W-REDCBEL               TO UT-REDCBEL                         
037700       MOVE ZERO                    TO UT-RELOBEL                         
037800       MOVE 'TOT'                   TO UT-POST-TEXT                       
037900       MOVE ZERO                    TO UT-ADLAGOMR                        
038000       PERFORM S11-SKRIV-W61554                                           
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 BA-SKAPA-SISTA-DC-POST SECTION.                                          
038500                                                                          
038600     MOVE WS-SPAR-IDDC              TO UT-IDDC                            
038700     MOVE WS-SPAR-ADLAGOMR          TO UT-ADLAGOMR                        
038800     COMPUTE W-RELOBEL = (W-KVPLATS-TOT / W-KVPLATS-WDJ8)                 
038900                       * 100                                              
039000                                                                          
039100                                                                          
039200     MOVE W-RELOBEL                 TO UT-RELOBEL                         
039300     MOVE ZERO                      TO UT-REDCBEL                         
039400     MOVE SPACE                     TO UT-POST-TEXT                       
039500     PERFORM S11-SKRIV-W61554                                             
039600                                                                          
039700     COMPUTE W-KVPLATS-WDJ8-TOT =                                         
039800                   W-KVPLATS-WDJ8 + W-KVPLATS-WDJ8-TOT                    
039900                                                                          
040000     COMPUTE W-KVPLATS-STOT      =                                        
040100                   W-KVPLATS-STOT + W-KVPLATS-TOT                         
040200                                                                          
040300     COMPUTE W-REDCBEL = (W-KVPLATS-STOT / W-KVPLATS-WDJ8-TOT)            
040400                          * 100                                           
040500                                                                          
040600     MOVE W-REDCBEL               TO UT-REDCBEL                           
040700     MOVE ZERO                    TO UT-RELOBEL                           
040800     MOVE 'TOT'                   TO UT-POST-TEXT                         
040900     MOVE ZERO                    TO UT-ADLAGOMR                          
041000       PERFORM S11-SKRIV-W61554                                           
041100       MOVE ZERO TO W-REDCBEL                                             
041200                    W-KVPLATS-TOT                                         
041300                    W-KVPLATS-STOT                                        
041400                    W-KVPLATS-WDJ8-TOT                                    
041500                    W-KVPLATS-WDJ8                                        
041600                    W-KVPLATS-WDK7                                        
041700                    W-KVPLATS-WDD8                                        
041800     MOVE SPACE TO WS-SPAR-IDDC                                           
041900     .                                                                    
042000     EJECT                                                                
042100 Z-FINIT SECTION.                                                         
042200     CLOSE W61554                                                         
042300     SKIP2                                                                
042400     MOVE 'S' TO POSTSUM-OPKOD                                            
042500     CALL POSTSUM USING POSTSUM-PARM                                      
042600     .                                                                    
042700     EJECT                                                                
042800 S11-SKRIV-W61554 SECTION.                                                
042900                                                                          
043000     WRITE UT-POST FROM UT-AREA                                           
043100                                                                          
043200     MOVE 'UT-'      TO POSTSUM-TRANSTYP                                  
043300     MOVE W-IDDC     TO POSTSUM-FDNAMN                                    
043400     MOVE 'W61554D1' TO POSTSUM-DDNAMN2                                   
043500     CALL POSTSUM USING POSTSUM-PARM                                      
043600     .                                                                    
043700     EJECT                                                                
043800 S99-ABEND SECTION.                                                       
043900                                                                          
044000     SKIP2                                                                
044100     MOVE 'S' TO POSTSUM-OPKOD                                            
044200     CALL POSTSUM USING POSTSUM-PARM                                      
044300     CALL ABEND USING RKOD-ABEND                                          
044400     .                                                                    
044500     EJECT                                                                
044600* --- IMS SEKTIONER ---                                                   
044700                                                                          
044800     EJECT                                                                
044900 IMS-GN-WDB601    SECTION.                                                
045000*    STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
045100*         DELIMITED BY SIZE INTO SSA1                                     
045200     MOVE 'WDB601  ' TO SSA1                                              
045300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
045400     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
045500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     IF SEGMENT-SAKNAS                                                    
045800         MOVE SPACE TO DCS-KDDC                                           
045900     END-IF                                                               
046000     .                                                                    
046100 IMS-GHU-WDJ801 SECTION.                                                  
046200                                                                          
046300     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
046400          DELIMITED BY SIZE INTO SSA1                                     
046500     MOVE '  GE' TO GODK-STATUSKODER                                      
046600     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
046700     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
046800     PERFORM IMS-STATUSKONTROLL                                           
046900     .                                                                    
047000     SKIP3                                                                
047100 IMS-GN-WDJ801 SECTION.                                                   
047200                                                                          
047300     STRING 'WDJ801  (WDJ801KY =' W-WDJ8KEY-X ')'                         
047400          DELIMITED BY SIZE INTO SSA1                                     
047500     MOVE '  GE' TO GODK-STATUSKODER                                      
047600     CALL CBLTDLI USING GN  WDJ8-PCB DLI-IO-WDJ801 SSA1                   
047700     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
047800     PERFORM IMS-STATUSKONTROLL                                           
047900     .                                                                    
048000     SKIP3                                                                
048100 IMS-GET-WDJ801-MIN-MAX SECTION.                                          
048200                                                                          
048300     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
048400                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
048500          DELIMITED BY SIZE INTO SSA1                                     
048600     MOVE '  GE' TO GODK-STATUSKODER                                      
048700     CALL CBLTDLI USING GHU WDJ8-PCB DLI-IO-WDJ801 SSA1                   
048800     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
048900     PERFORM IMS-STATUSKONTROLL                                           
049000     .                                                                    
049100     SKIP3                                                                
049200 IMS-GN-WDJ801-MIN-MAX SECTION.                                           
049300                                                                          
049400     STRING 'WDJ801  (WDJ801KY>=' W-WDJ8KEY-MIN-X                         
049500                    '&WDJ801KY<=' W-WDJ8KEY-MAX-X ')'                     
049600          DELIMITED BY SIZE INTO SSA1                                     
049700     MOVE '  GE' TO GODK-STATUSKODER                                      
049800     CALL CBLTDLI USING GN WDJ8-PCB DLI-IO-WDJ801 SSA1                    
049900     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUSKONTROLL                                           
050100     .                                                                    
050200     SKIP3                                                                
050300 IMS-GU-WDD801 SECTION.                                                   
050400     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
050500                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
050600                                                                          
050700            DELIMITED BY SIZE INTO SSA1                                   
050800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
050900     CALL CBLTDLI USING GU WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
051000     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     .                                                                    
051300     EJECT                                                                
051400 IMS-GN-WDD801 SECTION.                                                   
051500     STRING 'WDD8A1  (WDD8A1KY=>' W-WDD8A1KY-MIN-X                        
051600                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
051700                                                                          
051800            DELIMITED BY SIZE INTO SSA1                                   
051900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
052000     CALL CBLTDLI USING GN WDD8A-PCB DLI-IO-WDD8A1 SSA1                   
052100     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
052200     PERFORM IMS-STATUSKONTROLL                                           
052300     .                                                                    
052400     EJECT                                                                
052500 IMS-GU-WDK7A1 SECTION.                                                   
052600     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
052700                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
052800          DELIMITED BY SIZE INTO SSA1                                     
052900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
053000     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
053100     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
053200     PERFORM IMS-STATUSKONTROLL                                           
053300     .                                                                    
053400     SKIP3                                                                
053500 IMS-GN-WDK7A1 SECTION.                                                   
053600     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
053700                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
053800          DELIMITED BY SIZE INTO SSA1                                     
053900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
054000     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
054100     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
054200     PERFORM IMS-STATUSKONTROLL                                           
054300     .                                                                    
054400     SKIP3                                                                
054500 IMS-STATUSKONTROLL SECTION.                                              
054600                                                                          
054700     SET STATUS-IX TO 1                                                   
054800     SEARCH GODK-STATUS                                                   
054900       AT END                                                             
055000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055100           DELIMITED BY SIZE INTO FELTEXT                                 
055200         DISPLAY FELTEXT                                                  
055300         CALL FELLOG                                                      
055400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055500         CONTINUE                                                         
055600     END-SEARCH                                                           
055700     .                                                                    
