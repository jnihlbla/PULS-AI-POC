000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2220600.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   00/03/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR WDK626 MED KVPB-JUST                                  
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDK6                                       
001200*                                                                         
001300*        KAN ÅTERSTARTAS MED HELA INFILEN FRÅN BÖRJAN                     
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- POSTE FÖR UPPDAT WDK626                                    
002300     SELECT W22257                     ASSIGN TO W22206D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W22257                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W22257      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W2220600'.            
004000 01  CHKP-VAR.                                                            
004100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004600     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004810 01  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
004900     SKIP2                                                                
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400 77  W22257-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W22257                       VALUE 'J'.                   
005600     EJECT                                                                
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007300 01  IN-AREA-START               PIC X(24)   VALUE                        
007400                                             'IN-AREA-START'.             
007500     SKIP2                                                                
007600                                                                          
007700*01  AREA -COPY W22257     -PRE IN-                                       
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008300     03  W-IDARTNR-X.                                                     
008400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008500     03  W-KDSEGKEY-X.                                                    
008600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008700     SKIP2                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     88  IMS-EJ-OK                           VALUE 'XD'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010010 01  SSA3                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600                                                                          
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010800 01  DLI-IO-WDK601.                                                       
010900*    03  -COPY WDK601                                                     
011000     EJECT                                                                
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
011200 01  DLI-IO-WDK611.                                                       
011300*    03  -COPY WDK611                                                     
011400     EJECT                                                                
011500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK626'.                      
011600 01  DLI-IO-WDK626.                                                       
011700*    03  -COPY WDK626                                                     
011800                                                                          
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200*01  -COPY W0009   -PRE MSG-                                              
012300                                                                          
012400*01  -COPY W0008  -PRE WDK6-                                              
012500     05  FILLER                  PIC X.                                   
012600     EJECT                                                                
012700 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
012800 MAIN SECTION.                                                            
012900     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
013000                                                                          
013100     SKIP2                                                                
013200     PERFORM A-INIT                                                       
013300     PERFORM S01-LAES-W22257                                              
013400     PERFORM UNTIL END-OF-W22257                                          
013500       IF CHKP-ANT > CHKP-MAX                                             
013600         PERFORM X-TAG-CHECKPOINT                                         
013700       END-IF                                                             
013800       MOVE IN-IDARTNR TO W-IDARTNR                                       
013900       PERFORM IMS-GHU-WDK626                                             
014000       IF SEGMENT-FINNS                                                   
014100          PERFORM B-UPPDATERA-WDK626                                      
014200       ELSE                                                               
014300          PERFORM C-INSERT-WDK626                                         
014400       END-IF                                                             
014500                                                                          
014600       PERFORM S01-LAES-W22257                                            
014700     END-PERFORM                                                          
014800                                                                          
014900                                                                          
015000     PERFORM Z-FINIT                                                      
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015700     SKIP2                                                                
015800                                                                          
015900     PERFORM IMS-RESTART                                                  
016000                                                                          
016100     OPEN INPUT W22257                                                    
016200                                                                          
016300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016400     .                                                                    
016500     EJECT                                                                
016600                                                                          
016700 B-UPPDATERA-WDK626 SECTION.                                              
016800                                                                          
016900     MOVE IN-KVPB-JUST (1)   TO JUST-KVPB-JUST (1)                        
017000     MOVE IN-TIPBJUST  (1)   TO JUST-TIPBJUST  (1)                        
017100     MOVE IN-KVPB-JUST (2)   TO JUST-KVPB-JUST (2)                        
017200     MOVE IN-TIPBJUST  (2)   TO JUST-TIPBJUST  (2)                        
017300                                                                          
017400     PERFORM IMS-REPL-WDK626                                              
017410                                                                          
017420     ADD +1                  TO CHKP-ANT                                  
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800 C-INSERT-WDK626 SECTION.                                                 
017900                                                                          
018000     MOVE IN-KVPB-JUST (1)   TO JUST-KVPB-JUST (1)                        
018100     MOVE IN-TIPBJUST  (1)   TO JUST-TIPBJUST  (1)                        
018200     MOVE IN-KVPB-JUST (2)   TO JUST-KVPB-JUST (2)                        
018300     MOVE IN-TIPBJUST  (2)   TO JUST-TIPBJUST  (2)                        
018400                                                                          
018500     MOVE ZERO               TO JUST-REPBJUST                             
018600                                JUST-TIPBJUST-CENTR                       
018700                                JUST-DAMANSEA                             
018800                                JUST-DASPSEA                              
018900     MOVE +1                 TO IX                                        
019000     PERFORM UNTIL IX > 12                                                
019100        MOVE 1.00            TO JUST-RESEASON (IX)                        
019200        ADD +1               TO IX                                        
019300     END-PERFORM                                                          
019400                                                                          
019500     PERFORM IMS-ISRT-WDK626                                              
019510                                                                          
019520     ADD +1                  TO CHKP-ANT                                  
019600     .                                                                    
019700     EJECT                                                                
019800                                                                          
019900 Z-FINIT SECTION.                                                         
020000                                                                          
020100                                                                          
020200     CLOSE W22257                                                         
020300     SKIP2                                                                
020400     MOVE 'S' TO POSTSUM-OPKOD                                            
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600     .                                                                    
020700     EJECT                                                                
020800 S01-LAES-W22257  SECTION.                                                
020900     SKIP2                                                                
021000     READ W22257 INTO IN-AREA                                             
021100     AT END                                                               
021200        SET END-OF-W22257 TO TRUE                                         
021300                                                                          
021400     NOT AT END                                                           
021500        MOVE 'W22257'   TO POSTSUM-FDNAMN                                 
021600        MOVE 'W22206D1' TO POSTSUM-DDNAMN2                                
021700        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
021800        CALL POSTSUM USING POSTSUM-PARM                                   
021900                                                                          
022100     END-READ                                                             
022200     .                                                                    
022300     EJECT                                                                
022400 X-TAG-CHECKPOINT   SECTION.                                              
022500                                                                          
022600     PERFORM IMS-CHECKPOINT                                               
022700     MOVE ZERO TO CHKP-ANT                                                
022800     .                                                                    
022900     EJECT                                                                
023000* --- IMS SEKTIONER ---                                                   
023100                                                                          
023200 IMS-GET-WDK601 SECTION.                                                  
023300                                                                          
023400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023500          DELIMITED BY SIZE INTO SSA1                                     
023600     MOVE '  GE' TO GODK-STATUSKODER                                      
023700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
023800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023900     PERFORM IMS-STATUSKONTROLL                                           
024000     .                                                                    
024100     SKIP3                                                                
024200 IMS-GET-WDK611 SECTION.                                                  
024300                                                                          
024400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
024500          DELIMITED BY SIZE INTO SSA1                                     
024600     MOVE '  GE' TO GODK-STATUSKODER                                      
024700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
024800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024900     PERFORM IMS-STATUSKONTROLL                                           
025000     .                                                                    
025100     EJECT                                                                
025200 IMS-GHU-WDK626 SECTION.                                                  
025300                                                                          
025400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
025500          DELIMITED BY SIZE INTO SSA1                                     
025600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
025700          DELIMITED BY SIZE INTO SSA2                                     
025800     STRING 'WDK626    '                                                  
025900          DELIMITED BY SIZE INTO SSA3                                     
026000     MOVE '  GE' TO GODK-STATUSKODER                                      
026100     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3         
026200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026300     PERFORM IMS-STATUSKONTROLL                                           
026400     .                                                                    
026500     EJECT                                                                
026600 IMS-ISRT-WDK626 SECTION.                                                 
026700                                                                          
026800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
026900          DELIMITED BY SIZE INTO SSA1                                     
027000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
027100          DELIMITED BY SIZE INTO SSA2                                     
027200     MOVE 'WDK626 '           TO SSA3                                     
027300     MOVE '  II' TO GODK-STATUSKODER                                      
027400     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3        
027500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027600     PERFORM IMS-STATUSKONTROLL                                           
027610                                                                          
027620        MOVE 'WDK626'   TO POSTSUM-FDNAMN                                 
027630        MOVE 'BAS     ' TO POSTSUM-DDNAMN2                                
027640        MOVE 'ISRT'     TO POSTSUM-TRANSTYP                               
027650        CALL POSTSUM USING POSTSUM-PARM                                   
027700     .                                                                    
027800     EJECT                                                                
027900 IMS-REPL-WDK626 SECTION.                                                 
028000                                                                          
028100     MOVE '  ' TO GODK-STATUSKODER                                        
028200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK626                       
028300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028400     PERFORM IMS-STATUSKONTROLL                                           
028410                                                                          
028420        MOVE 'WDK626'   TO POSTSUM-FDNAMN                                 
028430        MOVE 'BAS     ' TO POSTSUM-DDNAMN2                                
028440        MOVE 'REPL'     TO POSTSUM-TRANSTYP                               
028450        CALL POSTSUM USING POSTSUM-PARM                                   
028500     .                                                                    
028600     EJECT                                                                
028700 IMS-RESTART SECTION.                                                     
028800     SKIP2                                                                
028900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029000     MOVE '  ' TO GODK-STATUSKODER                                        
029100     CALL CBLTDLI USING XRST MSG-PCB                                      
029200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029300                        CHKP-AREA-LENGTH CHKP-AREA                        
029400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029500     PERFORM IMS-STATUSKONTROLL                                           
029600     .                                                                    
029700     SKIP3                                                                
029800 IMS-CHECKPOINT SECTION.                                                  
029900     SKIP2                                                                
030000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030100     MOVE '  XD' TO GODK-STATUSKODER                                      
030200     CALL CBLTDLI USING CHKP MSG-PCB                                      
030300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030400                        CHKP-AREA-LENGTH CHKP-AREA                        
030500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030600     PERFORM IMS-STATUSKONTROLL                                           
030700                                                                          
030800     IF IMS-EJ-OK                                                         
030900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
031000       DISPLAY FELTEXT                                                    
031100       CALL FELLOG                                                        
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 IMS-STATUSKONTROLL SECTION.                                              
031600     SKIP2                                                                
031700     SET STATUS-IX TO 1                                                   
031800     SEARCH GODK-STATUS                                                   
031900       AT END                                                             
032000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032100           DELIMITED BY SIZE INTO FELTEXT                                 
032200         DISPLAY FELTEXT                                                  
032300         CALL FELLOG                                                      
032400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032500         CONTINUE                                                         
032600     END-SEARCH                                                           
032700     .                                                                    
