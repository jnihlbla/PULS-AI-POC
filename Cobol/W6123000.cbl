000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6123000.                                                
000400 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500 DATE-WRITTEN.   96/12/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        RÄKNAR OM KVRADER-PRIO PÅ HÄNDELSTRANS 6302                      
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001200*                                      WDK6                               
001300*                                      WDK9                               
001400*        PROGRAMMET UPPDATERAR WL6301 (WDR5)                              
001500*                                      WDL6                               
001600*                                                                         
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W6123000'.            
002700 01  CHKP-VAR.                                                            
002800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
002900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
003100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
003200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
003300     03 CHKP-MAX                 PIC S9(3)   VALUE +5   COMP-3.           
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- INDEX                                                            
004200 77  PER-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004300                                                                          
004400*    --- GENERELLA ARBETSAREAOR.                                          
004500 01  WS.                                                                  
004600     05  WS-DATUM-X.                                                      
004700         10  WS-DATUM        PIC 9(6).                                    
004800                                                                          
004900     05  WS-FLPRIO           PIC X                VALUE SPACE.            
005000     05  WS-KVPRIOART        PIC S9(7)            COMP-3.                 
005100     05  WS-KVTILLGANG       PIC S9(7)V9(1)       VALUE ZERO.             
005200     05  WS-KVBEHOV          PIC S9(7)V9(1)       VALUE ZERO.             
005300     05  WS-DIFF             PIC S9(7)V9(1)       VALUE ZERO.             
005400     05  WS-KVROS            PIC S9(7)            VALUE ZERO.             
005410     05  WS-KVOKS-BULK       PIC S9(7)            VALUE ZERO.             
005420     05  WS-KVOKS-DAG        PIC S9(7)            VALUE ZERO.             
005430     05  WS-KVOKS-VOR        PIC S9(7)            VALUE ZERO.             
005500     EJECT                                                                
005600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005700 01  GENERELLA-SUBPROGRAM.                                                
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     EJECT                                                                
006100                                                                          
006200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006300*                                                                         
006400     EJECT                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006600     SKIP3                                                                
006700 01  NYCKLAR-TILL-DLI.                                                    
006800     03  W-IDARTNR-X.                                                     
006900         05  W-IDARTNR           PIC S9(9)              COMP-3.           
007000                                                                          
007100     03  W-KDSEGKEY-X.                                                    
007200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
007300                                                                          
007400     03  W-DAINLEV-X.                                                     
007500         05 W-DAINLEV            PIC X(16).                               
007600                                                                          
007700     03  W-IDDC-X.                                                        
007800         05 W-IDDC               PIC X(2).                                
007900                                                                          
008000     03  W-WDL6A1KY-MIN.                                                  
008100         05  W-IDFAKT-MIN-A1      PIC S9(7)             COMP-3.           
008200         05  W-IDKUNDRF-MIN-A1    PIC X(10).                              
008300         05  W-IDKUNDNR-MIN-A1    PIC S9(7)             COMP-3.           
008400         05  W-IDKOLLI-MIN-A1     PIC S9(5)             COMP-3.           
008500         05  FILLER               PIC X(21).                              
008600                                                                          
008700     03  W-WDL6A1KY-MAX.                                                  
008800         05  W-IDFAKT-MAX-A1      PIC S9(7)             COMP-3.           
008900         05  W-IDKUNDRF-MAX-A1    PIC X(10).                              
009000         05  W-IDKUNDNR-MAX-A1    PIC S9(7)             COMP-3.           
009100         05  W-IDKOLLI-MAX-A1     PIC S9(5)             COMP-3.           
009200         05  FILLER               PIC X(21).                              
009300                                                                          
009400     03  W-WDL6A1KY.                                                      
009500         05  W-IDFAKT             PIC S9(7)             COMP-3.           
009600         05  W-IDKUNDRF           PIC X(10).                              
009700         05  W-IDKUNDNR           PIC S9(7)             COMP-3.           
009800         05  W-IDKOLLI            PIC S9(5)             COMP-3.           
009900         05  W-IDARTNR-MIN        PIC S9(9)             COMP-3.           
010000         05  W-DAINLEV-MIN        PIC 9(16).                              
010100                                                                          
010200     03  W-6301KEY-X.                                                     
010300         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
010400         05  W-6301-IDDC        PIC X(2).                                 
010500         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
010600                                                                          
010700     03  W-KEY6302-X.                                                     
010800         05  W-6302-DABERANK    PIC X(8)     VALUE SPACE.                 
010900         05  W-6302-IDFAKT      PIC S9(7)    VALUE ZERO COMP-3.           
011000                                                                          
011100     03  W-IDDC-B6-X.                                                     
011200         05 W-IDDC-B6                  PIC X(2).                          
011300     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
011900                                                   'GB'.                  
012000     88  BASEN-SLUT                          VALUE 'GB'.                  
012100     88  IMS-EJ-OK                           VALUE 'XD'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(128).                              
012700 01  SSA2                        PIC X(128).                              
012800 01  SSA3                        PIC X(128).                              
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTS11'.           
013500     SKIP3                                                                
013600 01  DLI-IO-AREA-ARTS11.                                                  
013700*    03      -COPY WDK711                                                 
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
014100     SKIP3                                                                
014200 01  DLI-IO-AREA-WDK611.                                                  
014300*    03      -COPY WDK611                                                 
014400     EJECT                                                                
014500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDL6A'.            
014600     SKIP3                                                                
014700 01  DLI-IO-AREA-WDL6A.                                                   
014800*    03      -COPY WDL6A1                                                 
014900     EJECT                                                                
015000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDL611-01'.        
015100     SKIP3                                                                
015200 01  DLI-IO-AREA-L611-01.                                                 
015300*    03 DLI-IO-AREA-L611   -COPY WDL611                                   
015400     EJECT                                                                
015500*    03 DLI-IO-AREA-L601   -COPY WDL601                                   
015600     EJECT                                                                
015700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-6301'.             
015800     SKIP3                                                                
015900 01  DLI-IO-AREA-6301.                                                    
016000*    03      -COPY WDGX6301                                               
016100     EJECT                                                                
016200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-6302'.             
016300     SKIP3                                                                
016400 01  DLI-IO-AREA-6302.                                                    
016500*    03      -COPY WDGX6302                                               
016600     EJECT                                                                
016700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016800 01   DLI-IO-AREA-B601.                                                   
016900*     03  -COPY WDB601                                                    
017000     EJECT                                                                
017100 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK901'.              
017200 01   DLI-IO-AREA-WDK901.                                                 
017300*     03  -COPY WDK901                                                    
017400     EJECT                                                                
017500 LINKAGE SECTION.                                                         
017600                                                                          
017700*01  -COPY W0009   -PRE MSG-                                              
017800     EJECT                                                                
017900*01  -COPY W0008  -PRE 6301-                                              
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
018200*01  -COPY W0008  -PRE WDL6-                                              
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE WDL6A-                                             
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800*01  -COPY W0008  -PRE ARTS-                                              
018900     05  FILLER                  PIC X.                                   
019000     EJECT                                                                
019100*01  -COPY W0008  -PRE WDB6-                                              
019200     05  FILLER                  PIC X.                                   
019300     EJECT                                                                
019400*01  -COPY W0008  -PRE WDK6-                                              
019500     05  FILLER                  PIC X.                                   
019600     EJECT                                                                
019700*01  -COPY W0008  -PRE WDK9-                                              
019800     05  FILLER                  PIC X.                                   
019900     EJECT                                                                
020000 PROCEDURE DIVISION  USING MSG-PCB  6301-PCB WDL6-PCB                     
020100                                    WDL6A-PCB ARTS-PCB                    
020200                                    WDB6-PCB WDK6-PCB                     
020300                                    WDK9-PCB.                             
020400     ENTRY 'DLITCBL' USING MSG-PCB  6301-PCB WDL6-PCB                     
020500                                    WDL6A-PCB ARTS-PCB                    
020600                                    WDB6-PCB WDK6-PCB                     
020700                                    WDK9-PCB.                             
020800                                                                          
020900     PERFORM A-INIT                                                       
021000     PERFORM B-UPD-WDGX6302                                               
021100                                                                          
021200     MOVE ZERO TO RETURN-CODE                                             
021300     GOBACK                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 A-INIT SECTION.                                                          
021700                                                                          
021800     PERFORM IMS-RESTART                                                  
021900                                                                          
022000     ACCEPT WS-DATUM         FROM DATE                                    
022100     MOVE WS-DATUM (3:2)     TO PER-IX                                    
022200                                                                          
022300     MOVE LOW-VALUE  TO W-WDL6A1KY-MIN                                    
022400     MOVE HIGH-VALUE TO W-WDL6A1KY-MAX                                    
022500     .                                                                    
022600     EJECT                                                                
022700 B-UPD-WDGX6302 SECTION.                                                  
022800******************************************************************        
022900* RÄKNAR ALLA PRIOART PTYP R30 OCH 310 OCH UPPDATERAR HTR 6301   *        
023000* R30 KONTROLL OM PRIOART OCH UPPDATERAR WDL6                    *        
023100* 310 KONTROLL OCH UPPDATERING OM PRIOART GÖRS BILD 6301 VID REC *        
023200******************************************************************        
023300                                                                          
023400     PERFORM IMS-GN-WDB601                                                
023500     PERFORM UNTIL BASEN-SLUT                                             
023600                                                                          
023700       IF DCS-DDC                                                         
023800          CONTINUE                                                        
023900       ELSE                                                               
024000       MOVE DCS-IDDC         TO W-6301-IDDC                               
024100                                                                          
024200       PERFORM IMS-GU-WL630101                                            
024300                                                                          
024400       IF SEGMENT-FINNS                                                   
024500                                                                          
024600         PERFORM IMS-GHNP-WL630111                                        
024700                                                                          
024800         PERFORM UNTIL SEGMENT-SAKNAS                                     
024900                                                                          
025000           MOVE ZERO         TO WS-KVPRIOART                              
025100           MOVE 6302-IDFAKT  TO W-IDFAKT-MIN-A1                           
025200                                W-IDFAKT-MAX-A1                           
025300           PERFORM IMS-GU-WDL6A1                                          
025400                                                                          
025500           PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                     
025600                                                                          
025700             MOVE SPACE TO WS-FLPRIO                                      
025800             MOVE SEQA-IDARTNR TO W-IDARTNR                               
025900             MOVE SEQA-DAINLEV TO W-DAINLEV                               
026000             PERFORM IMS-GHU-WDL611                                       
026100                                                                          
026200                IF INL-IDPTYP = 'R30' OR                                  
026300                   INL-IDPTYP = '310'                                     
026400                                                                          
026500                   IF DCS-CDC                                             
026600                     PERFORM BA-BEH-CDC                                   
026700                   ELSE                                                   
026800                     PERFORM BB-BEH-XDC                                   
026900                   END-IF                                                 
027000                                                                          
027100                   IF WS-FLPRIO = 'J'                                     
027200                      IF INL-FLPRIO = 'N'                                 
027300                         MOVE 'J' TO INL-FLPRIO                           
027400                         PERFORM IMS-REPL-WDL611                          
027500                         ADD 1 TO CHKP-ANT                                
027600                      END-IF                                              
027700                   ELSE                                                   
027800                      IF INL-FLPRIO = 'J' OR 'Y'                          
027900                         MOVE 'N' TO INL-FLPRIO                           
028000                         PERFORM IMS-REPL-WDL611                          
028100                         ADD 1 TO CHKP-ANT                                
028200                      END-IF                                              
028300                   END-IF                                                 
028400                   IF CHKP-ANT > CHKP-MAX                                 
028500                     PERFORM X-TAG-CHECKPOINT                             
028600* OMPOSITIONERING I WDL6                                                  
028700                     MOVE SEQA-IDFAKT   TO W-IDFAKT                       
028800                     MOVE SEQA-IDKUNDRF TO W-IDKUNDRF                     
028900                     MOVE SEQA-IDKUNDNR TO W-IDKUNDNR                     
029000                     MOVE SEQA-IDKOLLI  TO W-IDKOLLI                      
029100                     MOVE SEQA-IDARTNR  TO W-IDARTNR-MIN                  
029200                     MOVE SEQA-DAINLEV  TO W-DAINLEV-MIN                  
029300                                                                          
029400                     PERFORM IMS-GET-WDL6A1                               
029500* OMPOSITIONERING I WDB6                                                  
029600                     MOVE DCS-IDDC TO W-IDDC-B6                           
029700                     PERFORM IMS-GU-WDB601                                
029800                   END-IF                                                 
029900                END-IF                                                    
030000                                                                          
030100             PERFORM IMS-GN-WDL6A1                                        
030200           END-PERFORM                                                    
030300                                                                          
030400           MOVE WS-KVPRIOART TO 6302-KVRADER-PRIO                         
030500           PERFORM IMS-REPL-WL630111                                      
030600           ADD 1 TO CHKP-ANT                                              
030700           IF CHKP-ANT > CHKP-MAX                                         
030800             PERFORM X-TAG-CHECKPOINT                                     
030900             MOVE DCS-IDDC TO W-IDDC-B6                                   
031000             PERFORM IMS-GU-WDB601                                        
031100           END-IF                                                         
031200                                                                          
031300           PERFORM IMS-GHNP-WL630111                                      
031400         END-PERFORM                                                      
031500       END-IF                                                             
031600       END-IF                                                             
031700     PERFORM IMS-GN-WDB601                                                
031800     END-PERFORM                                                          
031900     .                                                                    
032000     EJECT                                                                
032100 BA-BEH-CDC SECTION.                                                      
032200     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
032300     PERFORM IMS-GU-WDK611                                                
032400     PERFORM IMS-GU-WDK901                                                
032410     IF SEGMENT-FINNS                                                     
032411       MOVE ART-KVOKS-BULK TO WS-KVOKS-BULK                               
032412       MOVE ART-KVOKS-DAG  TO WS-KVOKS-DAG                                
032413       MOVE ART-KVOKS-VOR  TO WS-KVOKS-VOR                                
032420     ELSE                                                                 
032421       MOVE ZERO           TO WS-KVOKS-BULK                               
032422                              WS-KVOKS-DAG                                
032423                              WS-KVOKS-VOR                                
032430     END-IF                                                               
032500                                                                          
032600     IF CLAG-KVROS > 0                                                    
032700        ADD +1   TO WS-KVPRIOART                                          
032800        MOVE 'J' TO WS-FLPRIO                                             
033300     ELSE                                                                 
033400        IF DCS-FLEXCP1-PRIO = JA                                          
033500           COMPUTE WS-KVTILLGANG = CLAG-KVLS -                            
033600             (CLAG-KVRESS + CLAG-KVUTRS + WS-KVOKS-BULK +                 
033700              WS-KVOKS-DAG + WS-KVOKS-VOR +                               
033800              CLAG-KVSPARR-KVAL) + CLAG-KVAKS-CDC                         
033900           END-COMPUTE                                                    
034000           COMPUTE WS-KVBEHOV = CLAG-KVROS +                              
034100             (CLAG-KVPB-PLAN * CLAG-RESEASON-PLAN (PER-IX)                
034200                            * 12 / 52 * 0.4)                              
034300           END-COMPUTE                                                    
034400                                                                          
034500           MOVE ZERO TO WS-DIFF                                           
034600                                                                          
034700           COMPUTE WS-DIFF = WS-KVTILLGANG                                
034800                           - WS-KVBEHOV                                   
034900           END-COMPUTE                                                    
035000           IF WS-DIFF <= ZERO                                             
035100              ADD +1      TO WS-KVPRIOART                                 
035200              MOVE 'J' TO WS-FLPRIO                                       
035300           END-IF                                                         
035400       ELSE                                                               
035500           IF CLAG-ADLAGOMR = ZERO                                        
035600             CONTINUE                                                     
035700           ELSE                                                           
035800             COMPUTE WS-KVTILLGANG = CLAG-KVLS -                          
035900                (CLAG-KVRESS + CLAG-KVUTRS + WS-KVOKS-BULK +              
036000                 WS-KVOKS-DAG + WS-KVOKS-VOR +                            
036100                 CLAG-KVSPARR-KVAL)                                       
036200             END-COMPUTE                                                  
036300             COMPUTE WS-KVBEHOV = CLAG-KVROS +                            
036400             (CLAG-KVPB-PLAN * CLAG-RESEASON-PLAN (PER-IX)                
036500                              * 12 / 52 * 0.4)                            
036600             END-COMPUTE                                                  
036700                                                                          
036800             MOVE ZERO TO WS-DIFF                                         
036900                                                                          
037000             COMPUTE WS-DIFF = WS-KVTILLGANG                              
037100                             - WS-KVBEHOV                                 
037200             END-COMPUTE                                                  
037300             IF WS-DIFF < ZERO                                            
037400                ADD +1    TO WS-KVPRIOART                                 
037500                MOVE 'J' TO WS-FLPRIO                                     
038100             END-IF                                                       
038200           END-IF                                                         
038300        END-IF                                                            
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700 BB-BEH-XDC SECTION.                                                      
038800     MOVE SEQA-IDARTNR TO W-IDARTNR                                       
038900     MOVE INL-IDDC     TO W-IDDC                                          
039000     PERFORM IMS-GU-ARTS11                                                
039100                                                                          
039200     COMPUTE WS-KVROS = SLAG-KVROS-BULK +                                 
039300             SLAG-KVROS-DAG                                               
039400     END-COMPUTE                                                          
039500     IF WS-KVROS > 0                                                      
039600        ADD +1   TO WS-KVPRIOART                                          
039700        MOVE 'J' TO WS-FLPRIO                                             
040200     ELSE                                                                 
040300        IF DCS-FLEXCP1-PRIO = JA                                          
040400           COMPUTE WS-KVTILLGANG = SLAG-KVLS -                            
040500             (SLAG-KVRESS + SLAG-KVOKS-BULK +                             
040600              SLAG-KVOKS-DAG + SLAG-KVUTRS +                              
040700              SLAG-KVSPARR-KVAL) + SLAG-KVAKS-SDC                         
040800           END-COMPUTE                                                    
040900           COMPUTE WS-KVBEHOV = (SLAG-KVROS-BULK +                        
041000              SLAG-KVROS-DAG) +                                           
041100             (SLAG-KVPB-REF * SLAG-RESEASON (PER-IX)                      
041200                            * 12 / 52 * 0.4)                              
041300           END-COMPUTE                                                    
041400                                                                          
041500           MOVE ZERO TO WS-DIFF                                           
041600                                                                          
041700           COMPUTE WS-DIFF = WS-KVTILLGANG                                
041800                           - WS-KVBEHOV                                   
041900           END-COMPUTE                                                    
042000           IF WS-DIFF <= ZERO                                             
042100              ADD +1      TO WS-KVPRIOART                                 
042200              MOVE 'J' TO WS-FLPRIO                                       
042300           END-IF                                                         
042400       ELSE                                                               
042500           IF SLAG-ADLAGOMR = ZERO                                        
042600             CONTINUE                                                     
042700           ELSE                                                           
042800             COMPUTE WS-KVTILLGANG = SLAG-KVLS -                          
042900                (SLAG-KVRESS + SLAG-KVOKS-BULK +                          
043000                 SLAG-KVOKS-DAG + SLAG-KVUTRS +                           
043100                 SLAG-KVSPARR-KVAL)                                       
043200             END-COMPUTE                                                  
043300             COMPUTE WS-KVBEHOV = (SLAG-KVROS-BULK +                      
043400                SLAG-KVROS-DAG) +                                         
043500             (SLAG-KVPB-REF * SLAG-RESEASON (PER-IX)                      
043600                              * 12 / 52 * 0.4)                            
043700             END-COMPUTE                                                  
043800                                                                          
043900             MOVE ZERO TO WS-DIFF                                         
044000                                                                          
044100             COMPUTE WS-DIFF = WS-KVTILLGANG                              
044200                             - WS-KVBEHOV                                 
044300             END-COMPUTE                                                  
044400             IF WS-DIFF < ZERO                                            
044500                ADD +1    TO WS-KVPRIOART                                 
044600                MOVE 'J' TO WS-FLPRIO                                     
045100             END-IF                                                       
045200           END-IF                                                         
045300        END-IF                                                            
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700 X-TAG-CHECKPOINT   SECTION.                                              
045800     SKIP2                                                                
045900     PERFORM IMS-CHECKPOINT                                               
046000     MOVE ZERO TO CHKP-ANT                                                
046100                                                                          
046200* OMPOSITIONERING I WL630111                                              
046300     PERFORM IMS-GU-WL630101                                              
046400                                                                          
046500     MOVE 6302-DABERANK TO W-6302-DABERANK                                
046600     MOVE 6302-IDFAKT   TO W-6302-IDFAKT                                  
046700     PERFORM IMS-GHNP-WL630111-KVAL                                       
046800     .                                                                    
046900     EJECT                                                                
047000* --- IMS SEKTIONER ---                                                   
047100     SKIP3                                                                
047200 IMS-GU-WL630101 SECTION.                                                 
047300                                                                          
047400     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
047500          DELIMITED BY SIZE INTO SSA1                                     
047600     MOVE '  GE' TO GODK-STATUSKODER                                      
047700     CALL CBLTDLI USING GU 6301-PCB DLI-IO-AREA-6301 SSA1                 
047800                                                                          
047900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200     SKIP3                                                                
048300 IMS-GHNP-WL630111-KVAL SECTION.                                          
048400                                                                          
048500     STRING 'WL630111(KEY6302 = ' W-KEY6302-X ')'                         
048600          DELIMITED BY SIZE INTO SSA1                                     
048700     MOVE '    ' TO GODK-STATUSKODER                                      
048800     CALL CBLTDLI USING GHNP 6301-PCB DLI-IO-AREA-6302 SSA1               
048900                                                                          
049000     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     SKIP3                                                                
049400 IMS-GHNP-WL630111 SECTION.                                               
049500                                                                          
049600     MOVE 'WL630111 ' TO SSA1                                             
049700     MOVE '  GE' TO GODK-STATUSKODER                                      
049800     CALL CBLTDLI USING GHNP 6301-PCB DLI-IO-AREA-6302 SSA1               
049900                                                                          
050000     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050300     SKIP3                                                                
050400 IMS-REPL-WL630111 SECTION.                                               
050500                                                                          
050600     MOVE SPACE           TO GODK-STATUSKODER                             
050700     CALL CBLTDLI USING REPL 6301-PCB DLI-IO-AREA-6302                    
050800                                                                          
050900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
051000     PERFORM IMS-STATUSKONTROLL                                           
051100     .                                                                    
051200     EJECT                                                                
051300 IMS-GHU-WDL611   SECTION.                                                
051400                                                                          
051500     STRING 'WDL601  (IDARTNR = ' W-IDARTNR-X ')'                         
051600          DELIMITED BY SIZE INTO SSA1                                     
051700     STRING 'WDL611  (DAINLEV = ' W-DAINLEV-X ')'                         
051800          DELIMITED BY SIZE INTO SSA2                                     
051900     MOVE SPACE  TO GODK-STATUSKODER                                      
052000     CALL CBLTDLI USING GHU WDL6-PCB DLI-IO-AREA-L611 SSA1 SSA2           
052100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
052200     PERFORM IMS-STATUSKONTROLL                                           
052300     .                                                                    
052400     SKIP3                                                                
052500 IMS-REPL-WDL611 SECTION.                                                 
052600                                                                          
052700     MOVE SPACE           TO GODK-STATUSKODER                             
052800     CALL CBLTDLI USING REPL WDL6-PCB DLI-IO-AREA-L611                    
052900                                                                          
053000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     EJECT                                                                
053400 IMS-GU-ARTS11   SECTION.                                                 
053500                                                                          
053600     STRING 'WLARTS01(IDARTNR = ' W-IDARTNR-X ')'                         
053700          DELIMITED BY SIZE INTO SSA1                                     
053800     STRING 'WLARTS11(IDDC    = ' W-IDDC-X ')'                            
053900          DELIMITED BY SIZE INTO SSA2                                     
054000     MOVE SPACE  TO GODK-STATUSKODER                                      
054100     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-AREA-ARTS11 SSA1 SSA2         
054200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
054300     PERFORM IMS-STATUSKONTROLL                                           
054400     .                                                                    
054500     EJECT                                                                
054600 IMS-GU-WDK611   SECTION.                                                 
054700                                                                          
054800     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
054900          DELIMITED BY SIZE INTO SSA1                                     
055000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
055100          DELIMITED BY SIZE INTO SSA2                                     
055200     MOVE SPACE  TO GODK-STATUSKODER                                      
055300     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
055400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
055500     PERFORM IMS-STATUSKONTROLL                                           
055600     .                                                                    
055700     EJECT                                                                
055800 IMS-GU-WDK901   SECTION.                                                 
055900                                                                          
056000     STRING 'WDK901  (IDARTNR = ' W-IDARTNR-X ')'                         
056100          DELIMITED BY SIZE INTO SSA1                                     
056200     MOVE '  GE'  TO GODK-STATUSKODER                                     
056300     CALL CBLTDLI USING GU  WDK9-PCB DLI-IO-AREA-WDK901 SSA1              
056400     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
056500     PERFORM IMS-STATUSKONTROLL                                           
056600     .                                                                    
056700     EJECT                                                                
056800 IMS-GU-WDL6A1 SECTION.                                                   
056900     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN                          
057000                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
057100          DELIMITED BY SIZE INTO SSA1                                     
057200     MOVE '  GE' TO GODK-STATUSKODER                                      
057300     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-AREA-WDL6A SSA1               
057400                                                                          
057500     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
057600     PERFORM IMS-STATUSKONTROLL                                           
057700     .                                                                    
057800     SKIP3                                                                
057900 IMS-GN-WDL6A1 SECTION.                                                   
058000     STRING 'WDL6A1  (WDL6A1KY=>' W-WDL6A1KY-MIN                          
058100                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
058200          DELIMITED BY SIZE INTO SSA1                                     
058300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
058400     CALL CBLTDLI USING GN WDL6A-PCB DLI-IO-AREA-WDL6A SSA1               
058500     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
058600     PERFORM IMS-STATUSKONTROLL                                           
058700     .                                                                    
058800     SKIP3                                                                
058900 IMS-GET-WDL6A1 SECTION.                                                  
059000     STRING 'WDL6A1  (WDL6A1KY= ' W-WDL6A1KY ')'                          
059100          DELIMITED BY SIZE INTO SSA1                                     
059200     MOVE '  GE' TO GODK-STATUSKODER                                      
059300     CALL CBLTDLI USING GU WDL6A-PCB DLI-IO-AREA-WDL6A SSA1               
059400     MOVE WDL6A-STATUS-CODE TO STATUS-WS                                  
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     .                                                                    
059700     EJECT                                                                
059800 IMS-GN-WDB601    SECTION.                                                
059900     MOVE 'WDB601  ' TO SSA1                                              
060000     MOVE '  GB' TO GODK-STATUSKODER                                      
060100     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
060200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     EJECT                                                                
060600 IMS-GU-WDB601    SECTION.                                                
060700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
060800          DELIMITED BY SIZE INTO SSA1                                     
060900     MOVE '  ' TO GODK-STATUSKODER                                        
061000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
061100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
061200     PERFORM IMS-STATUSKONTROLL                                           
061300     .                                                                    
061400 IMS-RESTART SECTION.                                                     
061500     SKIP2                                                                
061600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
061700     MOVE '  ' TO GODK-STATUSKODER                                        
061800     CALL CBLTDLI USING XRST MSG-PCB                                      
061900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
062000                        CHKP-AREA-LENGTH CHKP-AREA                        
062100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062200     PERFORM IMS-STATUSKONTROLL                                           
062300     .                                                                    
062400     EJECT                                                                
062500 IMS-CHECKPOINT SECTION.                                                  
062600     SKIP2                                                                
062700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
062800     MOVE '  XD' TO GODK-STATUSKODER                                      
062900     CALL CBLTDLI USING CHKP MSG-PCB                                      
063000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
063100                        CHKP-AREA-LENGTH CHKP-AREA                        
063200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063300     PERFORM IMS-STATUSKONTROLL                                           
063400                                                                          
063500     IF IMS-EJ-OK                                                         
063600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT                
063700       DISPLAY FELTEXT                                                    
063800       CALL FELLOG                                                        
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 IMS-STATUSKONTROLL SECTION.                                              
064300                                                                          
064400     SET STATUS-IX TO 1                                                   
064500     SEARCH GODK-STATUS                                                   
064600       AT END                                                             
064700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
064800         DELIMITED BY SIZE INTO FELTEXT                                   
064900         CALL FELLOG                                                      
065000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
065100         CONTINUE                                                         
065200     END-SEARCH                                                           
065300     .                                                                    
