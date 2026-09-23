000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1112100.                                                
000400 AUTHOR.         FRONTEC, GÖTEBORG.                                       
000500 DATE-WRITTEN.   96/08/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR HÄNDELSER 1141/1142, 1157/1158 OCH 2303                   
001100*        FRÅN FILER SOM SKAPATS I W1111800 (W11118)                       
001200*        OCH W1112000 (W11124)                                            
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WLXXAV (WDG2)                              
001500*        PROGRAMMET UPPDATERAR WLXXCW (WDG3)                              
001510*        PROGRAMMET UPPDATERAR WDGX1134 (WDR5)                            
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  OM FELAKTIG POSTTYP PÅ INFILEN                          
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- UPPDATERINGSPOSTER                                         
002900     SELECT W111XX                     ASSIGN TO W11121D1.                
003000     SELECT W111XX-UT                  ASSIGN TO W11121D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W111XX                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W111240      -L.                                               
004100*01  -COPY W111241      -L.                                               
004200*01  -COPY W111242      -L.                                               
004210*01  -COPY W111243      -L.                                               
004300 FD  W111XX-UT                                                            
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  240-POST     -COPY W111240   -PRE W111XX- -L                         
004800*01  241-POST     -COPY W111241   -PRE W111XX- -L                         
004900*01  242-POST     -COPY W111242   -PRE W111XX- -L                         
004910*01  243-POST     -COPY W111243   -PRE W111XX- -L                         
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200     SKIP2                                                                
005300*    -- CHECKED BY WY2000                                                 
005400     SKIP3                                                                
005500 77  IDPGM                       PIC X(8)    VALUE 'W1112100'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800     SKIP2                                                                
005900 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 77  W111XX-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W111XX                       VALUE 'J'.                   
006500                                                                          
006600 01  W-ANT-UPPDAT                PIC 9(4)    VALUE ZERO.                  
006700 01  W-MAX-UPPDAT                PIC 9(3)    VALUE 500.                   
006800     EJECT                                                                
006900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007000 01  FILLER REDEFINES DAGENS-DATUM.                                       
007100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007400     EJECT                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     SKIP3                                                                
008600*    --- PARAMETRAR TILL ABEND                                            
008700*                                                                         
008800 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900     EJECT                                                                
009000 01  W111XX-AREA-START           PIC X(24)   VALUE                        
009100                                             'W111XX-AREA-START'.         
009200     SKIP2                                                                
009300                                                                          
009400 01  W111XX-AREA.                                                         
009500     03  W111XX-IDPTYP           PIC X(3).                                
009600     03  FILLER                  PIC X(17).                               
009700     SKIP2                                                                
009800 01  240-AREA   REDEFINES W111XX-AREA.                                    
009900*    03   -COPY W111240  -PRE 240-                                        
010000     EJECT                                                                
010100 01  241-AREA   REDEFINES W111XX-AREA.                                    
010200*    03   -COPY W111241  -PRE 241-                                        
010300     EJECT                                                                
010400 01  242-AREA   REDEFINES W111XX-AREA.                                    
010500*    03   -COPY W111242  -PRE 242-                                        
010600     EJECT                                                                
010610 01  243-AREA   REDEFINES W111XX-AREA.                                    
010620*    03   -COPY W111243  -PRE 243-                                        
010630     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011000                                                                          
011100     03  W-1141-KEY-X.                                                    
011200       05  W-KDSEGKEY          PIC X(4)      VALUE '1141'.                
011300       05  FILLER              PIC X(26)     VALUE LOW-VALUE.             
011400                                                                          
011500     03  W-1157-KEY-X.                                                    
011600       05  W-WDGXKEY           PIC X(4)      VALUE '1157'.                
011700       05  FILLER              PIC X(26)     VALUE LOW-VALUE.             
011800                                                                          
011900     03  W-2303-KEY-X.                                                    
012000       05  W-WDGXKEY           PIC X(4)      VALUE '2303'.                
012100       05  FILLER              PIC X(26)     VALUE LOW-VALUE.             
012200                                                                          
012210     03  W-1133-KEY-X.                                                    
012220       05  W-WDGXKEY           PIC X(4)      VALUE '1133'.                
012230       05  FILLER              PIC X(26)     VALUE LOW-VALUE.             
012231                                                                          
012232     03  W-IDARTNR-X.                                                     
012233       05  W-IDARTNR           PIC S9(9)     VALUE ZERO COMP-3.           
012240                                                                          
012300     SKIP2                                                                
012400*    --- STATUS-KOD FRÅN IMS                                              
012500 01  STATUS-WS                   PIC XX.                                  
012600     88  SEGMENT-FINNS                       VALUE '  '.                  
012700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013000     88  IMS-EJ-OK                           VALUE 'XD'.                  
013100     SKIP2                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014300     SKIP3                                                                
014400 01  DLI-IO-AREA.                                                         
014500     03  IO-AREA                 PIC X(500)  VALUE SPACE.                 
014600     SKIP3                                                                
014700     03  WLXXAV REDEFINES IO-AREA.                                        
014800*        05  -COPY WDGX1142 -PRE XXAV-                                    
014900     SKIP3                                                                
015000     03  WDGX2304 REDEFINES IO-AREA.                                      
015100*        GAMLA XXBT11                                                     
015200*        05  -COPY WDGX2304                                               
015300     SKIP3                                                                
015400     03  WLXXCW REDEFINES IO-AREA.                                        
015500*        05  -COPY WDGX1158 -PRE XXCW-                                    
015600     SKIP3                                                                
015610     03  WDGX1134 REDEFINES IO-AREA.                                      
015620*        05  -COPY WDGX1134 -PRE 1133-                                    
015630     EJECT                                                                
015700 LINKAGE SECTION.                                                         
015800                                                                          
015900*01  -COPY W0009   -PRE MSG-                                              
016000     EJECT                                                                
016100*01  -COPY W0008  -PRE XXAV-                                              
016200     05  FILLER                  PIC X.                                   
016300     EJECT                                                                
016400*01  -COPY W0008  -PRE XXCW-                                              
016500     05  FILLER                  PIC X.                                   
016600     EJECT                                                                
016700*01  -COPY W0008  -PRE 2303-                                              
016800     05  FILLER                  PIC X.                                   
016900     EJECT                                                                
016910*01  -COPY W0008  -PRE WDR5-                                              
016920     05  FILLER                  PIC X.                                   
016930     EJECT                                                                
017000 PROCEDURE DIVISION  USING MSG-PCB XXAV-PCB XXCW-PCB 2303-PCB             
017010                           WDR5-PCB.                                      
017100 MAIN SECTION.                                                            
017200     ENTRY 'DLITCBL' USING MSG-PCB XXAV-PCB XXCW-PCB 2303-PCB             
017210                           WDR5-PCB.                                      
017300                                                                          
017400     SKIP2                                                                
017500     PERFORM A-INIT                                                       
017600     PERFORM S01-LAES-W111XX                                              
017700                                                                          
017800     PERFORM UNTIL (   END-OF-W111XX                                      
017900                    OR W-ANT-UPPDAT > W-MAX-UPPDAT)                       
018000       EVALUATE W111XX-IDPTYP                                             
018100       WHEN '240'                                                         
018200         PERFORM B-SKAPA-WDG3-1158                                        
018300       WHEN '241'                                                         
018400         PERFORM C-SKAPA-WDG2-1142                                        
018500       WHEN '242'                                                         
018600         PERFORM D-SKAPA-WDG3-2303                                        
018610       WHEN '243'                                                         
018620         PERFORM F-BEHANDLA-WDR5                                          
018700       WHEN OTHER                                                         
018800         DISPLAY 'W11121 - OTILLÅTEN POSTTYP ' W111XX-IDPTYP              
018900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
019000       END-EVALUATE                                                       
019100                                                                          
019200       ADD +1 TO W-ANT-UPPDAT                                             
019300                                                                          
019400       PERFORM S01-LAES-W111XX                                            
019500     END-PERFORM                                                          
019600                                                                          
019700     PERFORM E-SKRIV-OBEHANDLADE                                          
019800                                                                          
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 A-INIT SECTION.                                                          
020600     SKIP2                                                                
020700                                                                          
020800     OPEN INPUT  W111XX                                                   
020900     OPEN OUTPUT W111XX-UT                                                
021000                                                                          
021100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021200     .                                                                    
021300     EJECT                                                                
021400 B-SKAPA-WDG3-1158 SECTION.                                               
021500***                                                                       
021600* SKAPAR SEGMENT PÅ WDG3 (WLXXCW) HTYP 1157/1158                          
021700***                                                                       
021800                                                                          
021900     MOVE 240-1158-IDARTNR  TO XXCW-1158-IDARTNR                          
022000     MOVE 240-1158-KDERS    TO XXCW-1158-KDERS                            
022100     MOVE 240-1158-TIREGDAT TO XXCW-1158-TIREGDAT                         
022200                                                                          
022300     PERFORM IMS-ISRT-XXCW                                                
022400     .                                                                    
022500     EJECT                                                                
022600 C-SKAPA-WDG2-1142 SECTION.                                               
022700***                                                                       
022800* SKAPAR SEGMENT PÅ WDG2 (WLXXAV) HTYP 1141/1142                          
022900***                                                                       
023000                                                                          
023100     MOVE 241-1142-IDARTNR  TO XXAV-1142-IDARTNR                          
023200     MOVE 241-1142-KDSEGKEY TO XXAV-1142-KDSEGKEY                         
023300     MOVE 241-1142-KDSVAR   TO XXAV-1142-KDSVAR                           
023400                                                                          
023500     PERFORM IMS-ISRT-XXAV                                                
023600     .                                                                    
023700     EJECT                                                                
023800 D-SKAPA-WDG3-2303 SECTION.                                               
023900***                                                                       
024000* SKAPAR SEGMENT PÅ WDG3 (WDGX2304) HTYP 2303                             
024100***                                                                       
024200                                                                          
024300     MOVE 242-2303-IDARTNR-SATS  TO 2304-IDARTNR-SATS                     
024400     MOVE 242-2303-IDARTNR-ING   TO 2304-IDARTNR-ING                      
024500     MOVE 242-2303-KDERS-NEW     TO 2304-KDERS-NEW                        
024600     MOVE 242-2303-KDERS-OLD     TO 2304-KDERS-OLD                        
024700     MOVE 242-2303-TIERSDAT-PREL TO 2304-TIERSDAT-PREL                    
024800                                                                          
024900     PERFORM IMS-ISRT-2304                                                
025000     .                                                                    
025100     EJECT                                                                
025200 E-SKRIV-OBEHANDLADE SECTION.                                             
025300***                                                                       
025400* SKRIVER DE POSTER SOM INTE HAR BEHANDLATS                               
025500* PÅ EN NY GENERATION AV FILEN W111XX                                     
025600***                                                                       
025700                                                                          
025800     PERFORM UNTIL (END-OF-W111XX)                                        
025900                                                                          
026000       PERFORM S11-SKRIV-W111XX                                           
026100       PERFORM S01-LAES-W111XX                                            
026200     END-PERFORM                                                          
026300     .                                                                    
026400     EJECT                                                                
026410 F-BEHANDLA-WDR5 SECTION.                                                 
026420***                                                                       
026430* BEHANDLAR SEGMENT PÅ WDR5 (WDGX1134) HTYP 1133                          
026440***                                                                       
026450                                                                          
026451     IF 243-1134-KDUPPD = 'I'                                             
026460       MOVE 243-1134-IDARTNR TO 1133-1134-IDARTNR                         
026493       PERFORM IMS-ISRT-1134                                              
026494     ELSE                                                                 
026495       IF 243-1134-KDUPPD = 'D'                                           
026496          MOVE 243-1134-IDARTNR TO W-IDARTNR                              
026497          PERFORM IMS-GET-WDR501                                          
026498          PERFORM IMS-GET-WDGX1134                                        
026500          IF SEGMENT-FINNS                                                
026501             PERFORM IMS-DLET-1134                                        
026503          END-IF                                                          
026504       END-IF                                                             
026505     END-IF                                                               
026506     .                                                                    
026507     EJECT                                                                
026510 Z-FINIT SECTION.                                                         
026600                                                                          
026700                                                                          
026800     CLOSE W111XX                                                         
026900           W111XX-UT                                                      
027000                                                                          
027100     SKIP2                                                                
027200     MOVE 'S' TO POSTSUM-OPKOD                                            
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
027500     EJECT                                                                
027600 S01-LAES-W111XX  SECTION.                                                
027700     SKIP2                                                                
027800     READ W111XX INTO W111XX-AREA                                         
027900     AT END                                                               
028000        SET END-OF-W111XX TO TRUE                                         
028100                                                                          
028200     NOT AT END                                                           
028300        MOVE 'W111XX'      TO POSTSUM-FDNAMN                              
028400        MOVE 'W11121D1'    TO POSTSUM-DDNAMN2                             
028500        MOVE W111XX-IDPTYP TO POSTSUM-TRANSTYP                            
028600        CALL POSTSUM USING POSTSUM-PARM                                   
028700     END-READ                                                             
028800     .                                                                    
028900     EJECT                                                                
029000 S11-SKRIV-W111XX SECTION.                                                
029100     SKIP2                                                                
029200     IF      W111XX-IDPTYP = '240'                                        
029300       WRITE W111XX-240-POST   FROM 240-AREA                              
029400     ELSE IF W111XX-IDPTYP = '241'                                        
029500       WRITE W111XX-241-POST   FROM 241-AREA                              
029600     ELSE IF W111XX-IDPTYP = '242'                                        
029700       WRITE W111XX-242-POST   FROM 242-AREA                              
029810     ELSE IF W111XX-IDPTYP = '243'                                        
029820       WRITE W111XX-243-POST   FROM 243-AREA                              
029830     ELSE                                                                 
029900       DISPLAY 'W11121 - FEL POSTTYP PÅ W111XX-UT ' W111XX-IDPTYP         
030000       CALL FELLOG                                                        
030100     END-IF END-IF END-IF END-IF                                          
030200                                                                          
030300     MOVE 'W111XX'      TO POSTSUM-FDNAMN                                 
030400     MOVE 'W11121D2'    TO POSTSUM-DDNAMN2                                
030500     MOVE W111XX-IDPTYP TO POSTSUM-TRANSTYP                               
030600     CALL POSTSUM USING POSTSUM-PARM                                      
030700     .                                                                    
030800     EJECT                                                                
030900* --- IMS SEKTIONER ---                                                   
031000     SKIP3                                                                
031100     EJECT                                                                
031200 IMS-ISRT-XXAV SECTION.                                                   
031300                                                                          
031400     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
031500              DELIMITED BY SIZE INTO SSA1                                 
031600     MOVE 'WLXXAV11 ' TO SSA2                                             
031700     MOVE '  II' TO GODK-STATUSKODER                                      
031800     CALL CBLTDLI USING ISRT XXAV-PCB DLI-IO-AREA SSA1 SSA2               
031900     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
032000     DISPLAY 'XXAV' XXAV-STATUS-CODE                                      
032100     PERFORM IMS-STATUSKONTROLL                                           
032200     .                                                                    
032300     EJECT                                                                
032400 IMS-ISRT-2304 SECTION.                                                   
032500                                                                          
032600     STRING 'WDG301  (WDG3KEY  =' W-2303-KEY-X ')'                        
032700     DELIMITED BY SIZE INTO SSA1                                          
032800     MOVE 'WDGX2304*L'  TO SSA2                                           
032900     MOVE '  ' TO GODK-STATUSKODER                                        
033000     CALL CBLTDLI USING ISRT 2303-PCB DLI-IO-AREA SSA1 SSA2               
033100     MOVE 2303-STATUS-CODE  TO STATUS-WS                                  
033200     PERFORM IMS-STATUSKONTROLL                                           
033300     .                                                                    
033400     EJECT                                                                
033500 IMS-ISRT-XXCW SECTION.                                                   
033600*                                                                         
033700     STRING 'WLXXCW01(WDG3KEY  =' W-1157-KEY-X ')'                        
033800     DELIMITED BY SIZE INTO SSA1                                          
033900     MOVE 'WLXXCW11 ' TO SSA2                                             
034000     MOVE '  II' TO GODK-STATUSKODER                                      
034100     CALL CBLTDLI USING ISRT XXCW-PCB DLI-IO-AREA SSA1 SSA2               
034200     MOVE XXCW-STATUS-CODE  TO STATUS-WS                                  
034300     DISPLAY 'XXCW' XXCW-STATUS-CODE                                      
034400     PERFORM IMS-STATUSKONTROLL                                           
034500     .                                                                    
034600     EJECT                                                                
034610 IMS-ISRT-1134 SECTION.                                                   
034620                                                                          
034630     STRING 'WDR501  (WDGXKEY  =' W-1133-KEY-X ')'                        
034640              DELIMITED BY SIZE INTO SSA1                                 
034650     MOVE 'WDGX1134 ' TO SSA2                                             
034660     MOVE '  II' TO GODK-STATUSKODER                                      
034670     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA SSA1 SSA2               
034680     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
034690     DISPLAY 'WDR5' XXAV-STATUS-CODE                                      
034691     PERFORM IMS-STATUSKONTROLL                                           
034692     .                                                                    
034693     SKIP2                                                                
034694 IMS-GET-WDR501 SECTION.                                                  
034695                                                                          
034696     STRING 'WDR501  (WDGXKEY  =' W-1133-KEY-X ')'                        
034697           DELIMITED BY SIZE INTO SSA1                                    
034698     MOVE '  ' TO GODK-STATUSKODER                                        
034699     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-AREA SSA1                      
034700     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
034701     PERFORM IMS-STATUSKONTROLL                                           
034702     .                                                                    
034703     SKIP2                                                                
034704 IMS-GET-WDGX1134 SECTION.                                                
034705                                                                          
034706     STRING 'WDGX1134(IDARTNR  =' W-IDARTNR-X ')'                         
034707             DELIMITED BY SIZE INTO SSA1                                  
034708     MOVE '  GE' TO GODK-STATUSKODER                                      
034709     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-AREA SSA1                    
034710     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
034711     PERFORM IMS-STATUSKONTROLL                                           
034712     .                                                                    
034713     EJECT                                                                
034714 IMS-DLET-1134 SECTION.                                                   
034715                                                                          
034716     MOVE '  ' TO GODK-STATUSKODER                                        
034717     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-AREA                         
034718     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
034720     PERFORM IMS-STATUSKONTROLL                                           
034721     .                                                                    
034722     EJECT                                                                
034730 IMS-STATUSKONTROLL SECTION.                                              
034800     SKIP2                                                                
034900     SET STATUS-IX TO 1                                                   
035000     SEARCH GODK-STATUS                                                   
035100       AT END                                                             
035200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035300           DELIMITED BY SIZE INTO FELTEXT                                 
035400         DISPLAY FELTEXT                                                  
035500         CALL FELLOG                                                      
035600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035700         CONTINUE                                                         
035800     END-SEARCH                                                           
035900     .                                                                    
