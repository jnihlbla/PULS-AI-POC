000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4791300.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   91/05/22.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSNING AV                                                      
001100*                    ORDERBEKR  WDQ1                                      
001200*                    ORDERHUV   WDQ2                                      
001300*                    ORDERDEL   WDQ3                                      
001400*                                                                         
001500*        PROGRAMMET UPPATERAR WDQ1                                        
001600*        PROGRAMMET UPPATERAR WDQ2                                        
001700*        PROGRAMMET UPPATERAR WDQ3                                        
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*                                                                         
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 INPUT-OUTPUT          SECTION.                                           
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900*          --- RENSNINGSPOSTER FRÅN KONTROLLPGM                           
003000     SELECT W47921                     ASSIGN TO W47913D1.                
003100     SELECT W47913                     ASSIGN TO W47913D2.                
003200                                                                          
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500                                                                          
003600 FILE                  SECTION.                                           
003700                                                                          
003800 FD  W47921                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W479021       -L.                                              
004300                                                                          
004400                                                                          
004500 FD  W47913                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004910*01  OBKR-POST -COPY WDQ101   -L.                                         
005000                                                                          
005100     EJECT                                                                
005200 WORKING-STORAGE       SECTION.                                           
005300                                                                          
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600 77  IDPGM                       PIC X(8)    VALUE 'W4791300'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
006000 77  W-CHKP-MAX                  PIC S9(5)   VALUE +2000 COMP-3.          
006100 77  CHKP-ID                     PIC X(8)    VALUE 'W4791300'.            
006200 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
006300 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
006400 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
006500 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
006600                                                                          
006700 77  W47921-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W47921                       VALUE 'J'.                   
006900     EJECT                                                                
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700*                                                                         
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200                                                                          
008300*    --- PARAMETRAR TILL ABEND                                            
008400                                                                          
008500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008700                                                                          
008800 01  FELTEXT.                                                             
008900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009000     03  FELTEXT-STR             PIC X(20)   VALUE SPACE.                 
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600 01  IN-AREA-START               PIC X(24)   VALUE                        
009700                                 'IN-AREA-START  '.                       
009800                                                                          
009900                                                                          
010000*01  AREA -COPY W479021      -PRE IN-                                     
010100     EJECT                                                                
010200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600                                                                          
010700 01  NYCKLAR-TILL-DLI.                                                    
010800     03  W-WDQ211KY-X.                                                    
010900         05 W-IDDC-X.                                                     
011000             07 W-IDDC           PIC  X(2).                               
011100         05  W-IDLEVNR           PIC  X(5).                               
011200     03  W-IDORDER-X.                                                     
011300         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
011400     03  W-WDQ101KY-MIN.                                                  
011500         05  Q1-IDORDER-MIN      PIC S9(7)    COMP-3.                     
011600         05  Q1-IDARTNR-MIN      PIC S9(9)    COMP-3.                     
011700         05  Q1-IDLOPNR-MIN      PIC S9(3)    COMP-3.                     
011800         05  Q1-IDSEKVNR-MIN     PIC S9(3)    COMP-3.                     
011900         05  Q1-IDDC-MIN         PIC  X(2).                               
012000         05  Q1-KDORDBEK-MIN     PIC 9(2).                                
012100     03  W-WDQ101KY-MAX.                                                  
012200         05  Q1-IDORDER-MAX      PIC S9(7)    COMP-3.                     
012300         05  Q1-IDARTNR-MAX      PIC S9(9)    COMP-3.                     
012400         05  Q1-IDLOPNR-MAX      PIC S9(3)    COMP-3.                     
012500         05  Q1-IDSEKVNR-MAX     PIC S9(3)    COMP-3.                     
012600         05  Q1-IDDC-MAX         PIC  X(2).                               
012700         05  Q1-KDORDBEK-MAX     PIC 9(2).                                
012800     03  W-WDQ301KY-X.                                                    
012900         05  Q3-IDORDER          PIC S9(7)    COMP-3.                     
013000         05  Q3-IDDC             PIC  X(2).                               
013100         05  Q3-IDPRODNR         PIC S9(7)    COMP-3.                     
013200         05  Q3-IDPLKLST         PIC S9(3)    COMP-3.                     
013300                                                                          
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013800     88  BASEN-SLUT                          VALUE 'GB'.                  
013900                                                                          
014000 01  GODK-STATUSKODER.                                                    
014100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200                                                                          
014300 01  SSA1                        PIC X(80).                               
014400     EJECT                                                                
014500*    --- IMS FUNKTIONSKODER                                               
014600*01  -COPY W0003                                                          
014700     EJECT                                                                
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015000                                                                          
015100 01  DLI-IO-AREA.                                                         
015200     03  IO-AREA                 PIC X(4000) VALUE SPACE.                 
015300                                                                          
015400     03  WDQ101 REDEFINES IO-AREA.                                        
015500*        05  -COPY WDQ101                                                 
015600                                                                          
015700     03  WDQ201 REDEFINES IO-AREA.                                        
015800*        05  -COPY WDQ201                                                 
015900                                                                          
016000     03  WDQ211 REDEFINES IO-AREA.                                        
016100*        05  -COPY WDQ211                                                 
016200                                                                          
016300     03  WDQ212 REDEFINES IO-AREA.                                        
016400*        05  -COPY WDQ212                                                 
016500                                                                          
016600     03  WDQ301 REDEFINES IO-AREA.                                        
016700*        05  -COPY WDQ301                                                 
016800     EJECT                                                                
016900 LINKAGE               SECTION.                                           
017000*01  -COPY W0009  -PRE MSG-                                               
017100     EJECT                                                                
017200*01  -COPY W0008  -PRE WDQ3-                                              
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008  -PRE WDQ2-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800*01  -COPY W0008  -PRE WDQ1-                                              
017900     05  FILLER                  PIC X.                                   
018000     EJECT                                                                
018100 PROCEDURE DIVISION  USING MSG-PCB WDQ3-PCB WDQ2-PCB WDQ1-PCB.            
018200     ENTRY 'DLITCBL' USING MSG-PCB WDQ3-PCB WDQ2-PCB WDQ1-PCB.            
018300                                                                          
018400     PERFORM A-INIT                                                       
018500     PERFORM IMS-RESTART                                                  
018600                                                                          
018700     PERFORM S01-LAES-W47921                                              
018800     PERFORM UNTIL END-OF-W47921                                          
018900       EVALUATE IN-IDPTYP                                                 
019000         WHEN '010'                                                       
019100              PERFORM B-RENSA-UTAN-OBEK                                   
019200         WHEN '014'                                                       
019300              PERFORM D-RENSA-STATUS-E                                    
019400         WHEN '017'                                                       
019500              PERFORM C-RENSA-OBEK                                        
019600         WHEN 'XTR'                                                       
019700              PERFORM E-RENSA-ENBART-OBEK                                 
019800         WHEN OTHER                                                       
019900              MOVE 'FEL POSTTYP'              TO FELTEXT-STR              
020000              DISPLAY FELTEXT IN-IDPTYP                                   
020100              CALL ABEND USING RKOD-ABEND-UTAN-DUMP                       
020200       END-EVALUATE                                                       
020400       IF W-CHKP-RAKNARE                       >  W-CHKP-MAX              
020500          PERFORM IMS-CHECKPOINT                                          
020600          MOVE ZERO                           TO W-CHKP-RAKNARE           
020700       END-IF                                                             
020800       PERFORM S01-LAES-W47921                                            
020900     END-PERFORM                                                          
021000                                                                          
021100     PERFORM Z-FINIT                                                      
021200                                                                          
021300     MOVE ZERO                                TO RETURN-CODE              
021400     GOBACK                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 A-INIT                SECTION.                                           
021800                                                                          
021900     OPEN INPUT  W47921                                                   
022000     OPEN OUTPUT W47913                                                   
022100                                                                          
022300     MOVE IDPGM                               TO POSTSUM-PROGNAMN         
022400     MOVE ZERO                                TO W-CHKP-RAKNARE           
022500     .                                                                    
022600     EJECT                                                                
022700 B-RENSA-UTAN-OBEK     SECTION.                                           
022800                                                                          
022900     PERFORM BA-DLET-ORDERDELAR                                           
023000                                                                          
023100     PERFORM BB-DLET-DLEV-ARBTAB                                          
023200     .                                                                    
023300     EJECT                                                                
023400 BA-DLET-ORDERDELAR    SECTION.                                           
023500                                                                          
023600     MOVE IN-IDORDER              TO Q3-IDORDER                           
023700     MOVE IN-IDDC                 TO Q3-IDDC                              
023800     MOVE IN-IDPRODNR             TO Q3-IDPRODNR                          
023900     MOVE IN-IDPLKLST             TO Q3-IDPLKLST                          
024000     PERFORM IMS-GHU-WDQ301                                               
024100     IF SEGMENT-FINNS                                                     
024200        PERFORM IMS-DLET-WDQ3                                             
024210        ADD +1                   TO W-CHKP-RAKNARE                        
024300     END-IF                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 BB-DLET-DLEV-ARBTAB        SECTION.                                      
024700                                                                          
024800     MOVE IN-IDORDER                          TO W-IDORDER                
024900     PERFORM IMS-GHU-WDQ201                                               
025000     IF SEGMENT-FINNS                                                     
025100        MOVE IN-IDDC                          TO W-IDDC                   
025200        IF IN-IDLEVNR NOT = SPACE                                         
025300          MOVE IN-IDLEVNR                     TO W-IDLEVNR                
025400          PERFORM IMS-GHNP-WDQ211                                         
025500          IF SEGMENT-FINNS                                                
025600             PERFORM IMS-DLET-WDQ2                                        
025610             ADD +1                   TO W-CHKP-RAKNARE                   
025700          END-IF                                                          
025800        ELSE                                                              
025900          PERFORM IMS-GHNP-WDQ212                                         
026000          IF SEGMENT-FINNS                                                
026100             PERFORM IMS-DLET-WDQ2                                        
026110             ADD +1                   TO W-CHKP-RAKNARE                   
026200          END-IF                                                          
026300        END-IF                                                            
026400                                                                          
026500        PERFORM IMS-GNP-WDQ211-FIRST                                      
026600        IF SEGMENT-SAKNAS                                                 
026700          PERFORM IMS-GNP-WDQ212                                          
026800          IF SEGMENT-SAKNAS                                               
026900             PERFORM IMS-GHU-WDQ201                                       
027000             MOVE JA                          TO OHUV-FLBORT              
027100             PERFORM IMS-REPL-WDQ2                                        
027110             ADD +1                   TO W-CHKP-RAKNARE                   
027200          END-IF                                                          
027300        END-IF                                                            
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 C-RENSA-OBEK          SECTION.                                           
027800                                                                          
027900     MOVE IN-IDORDER                          TO W-IDORDER                
028000     PERFORM IMS-GHU-WDQ201                                               
028100     IF SEGMENT-FINNS                                                     
028200        PERFORM IMS-DLET-WDQ2                                             
028210        ADD +1                   TO W-CHKP-RAKNARE                        
028300     END-IF                                                               
028400     PERFORM CA-DLET-WDQ1                                                 
028500     .                                                                    
028600     EJECT                                                                
028700 CA-DLET-WDQ1          SECTION.                                           
028800                                                                          
028900     MOVE LOW-VALUE                           TO W-WDQ101KY-MIN           
029000     MOVE HIGH-VALUE                          TO W-WDQ101KY-MAX           
029100     MOVE IN-IDORDER                          TO Q1-IDORDER-MIN           
029200                                                 Q1-IDORDER-MAX           
029300     PERFORM IMS-GHU-WDQ101                                               
029400     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
029410       PERFORM S02-SKRIV-W47913                                           
029500       PERFORM IMS-DLET-WDQ1                                              
029510       ADD +1                   TO W-CHKP-RAKNARE                         
029600       PERFORM IMS-GHN-WDQ101                                             
029700     END-PERFORM                                                          
029800     .                                                                    
029900     EJECT                                                                
030000 D-RENSA-STATUS-E      SECTION.                                           
030100                                                                          
030200     MOVE IN-IDORDER                          TO W-IDORDER                
030300     PERFORM IMS-GHU-WDQ201                                               
030400     IF SEGMENT-FINNS                                                     
030500        PERFORM IMS-DLET-WDQ2                                             
030510        ADD +1                   TO W-CHKP-RAKNARE                        
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 E-RENSA-ENBART-OBEK   SECTION.                                           
031000                                                                          
031100     MOVE LOW-VALUE                           TO W-WDQ101KY-MIN           
031200     MOVE HIGH-VALUE                          TO W-WDQ101KY-MAX           
031300     MOVE IN-IDORDER                          TO Q1-IDORDER-MIN           
031400                                                 Q1-IDORDER-MAX           
031500     PERFORM IMS-GHU-WDQ101                                               
031600     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
031700       IF OBKR-KDORDBEK =  98 OR 85                                       
031710         PERFORM S02-SKRIV-W47913                                         
031800         PERFORM IMS-DLET-WDQ1                                            
031810         ADD +1                   TO W-CHKP-RAKNARE                       
031900       END-IF                                                             
032000       PERFORM IMS-GHN-WDQ101                                             
032100     END-PERFORM                                                          
032200     .                                                                    
032300     EJECT                                                                
032400 Z-FINIT               SECTION.                                           
032500                                                                          
032600     CLOSE W47921                                                         
032610           W47913                                                         
032700                                                                          
032800     MOVE 'S'                                 TO POSTSUM-OPKOD            
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     .                                                                    
033100     EJECT                                                                
033200 S01-LAES-W47921       SECTION.                                           
033300                                                                          
033400     READ W47921                INTO IN-AREA                              
033500     AT END                                                               
033600        MOVE HIGH-VALUE           TO IN-AREA                              
033700        SET END-OF-W47921         TO TRUE                                 
033800                                                                          
033900     NOT AT END                                                           
034000        MOVE 'W47921'             TO POSTSUM-FDNAMN                       
034100        MOVE 'W47913D1'           TO POSTSUM-DDNAMN2                      
034200        MOVE IN-IDPTYP            TO POSTSUM-TRANSTYP                     
034300        CALL POSTSUM USING POSTSUM-PARM                                   
034400     END-READ                                                             
034500     .                                                                    
034510                                                                          
034520 S02-SKRIV-W47913 SECTION.                                                
034530                                                                          
034540     WRITE OBKR-POST FROM OBKR-WDQ101                                     
034550                                                                          
034560     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
034570     MOVE 'W47913'   TO POSTSUM-FDNAMN                                    
034580     MOVE 'W47913D2' TO POSTSUM-DDNAMN2                                   
034590     CALL POSTSUM USING POSTSUM-PARM                                      
034591     .                                                                    
034592                                                                          
034600     EJECT                                                                
034700* --- IMS SEKTIONER ---                                                   
034800                                                                          
034900 IMS-RESTART           SECTION.                                           
035000                                                                          
035100     MOVE SPACE TO MSG-IO-AREA-1                                          
035200     MOVE '  ' TO GODK-STATUSKODER                                        
035300     CALL CBLTDLI USING XRST MSG-PCB                                      
035400                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
035500                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
035600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900                                                                          
036000 IMS-CHECKPOINT        SECTION.                                           
036100                                                                          
036200     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
036300     MOVE '  XD' TO GODK-STATUSKODER                                      
036400     CALL CBLTDLI USING CHKP MSG-PCB                                      
036500                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
036600                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
036700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     .                                                                    
037000     EJECT                                                                
037100 IMS-GHU-WDQ101         SECTION.                                          
037200                                                                          
037300     STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-MIN                          
037400                    '&WDQ101KY<=' W-WDQ101KY-MAX ')'                      
037500          DELIMITED BY SIZE     INTO SSA1                                 
037600     MOVE '  GE'                  TO GODK-STATUSKODER                     
037700     CALL CBLTDLI USING GHU WDQ1-PCB DLI-IO-AREA SSA1                     
037800     MOVE WDQ1-STATUS-CODE        TO STATUS-WS                            
037900     PERFORM IMS-STATUSKONTROLL                                           
038000     .                                                                    
038100                                                                          
038200 IMS-GHN-WDQ101         SECTION.                                          
038300                                                                          
038400     STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-MIN                          
038500                    '&WDQ101KY<=' W-WDQ101KY-MAX ')'                      
038600          DELIMITED BY SIZE     INTO SSA1                                 
038700     MOVE '  GEGB'                TO GODK-STATUSKODER                     
038800     CALL CBLTDLI USING GHN WDQ1-PCB DLI-IO-AREA SSA1                     
038900     MOVE WDQ1-STATUS-CODE        TO STATUS-WS                            
039000     PERFORM IMS-STATUSKONTROLL                                           
039100     .                                                                    
039200                                                                          
039300 IMS-DLET-WDQ1         SECTION.                                           
039400                                                                          
039500     MOVE '  '                    TO GODK-STATUSKODER                     
039600     CALL CBLTDLI USING DLET WDQ1-PCB DLI-IO-AREA                         
039700     MOVE WDQ1-STATUS-CODE        TO STATUS-WS                            
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040000     EJECT                                                                
040100 IMS-GHU-WDQ201        SECTION.                                           
040200                                                                          
040300     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
040400          DELIMITED BY SIZE     INTO SSA1                                 
040500     MOVE '  GE'                  TO GODK-STATUSKODER                     
040600     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA SSA1                     
040700     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
040800     PERFORM IMS-STATUSKONTROLL                                           
040900     .                                                                    
041000                                                                          
041100 IMS-GHNP-WDQ211       SECTION.                                           
041200                                                                          
041300     STRING 'WDQ211  (WDQ211KY =' W-WDQ211KY-X ')'                        
041400          DELIMITED BY SIZE     INTO SSA1                                 
041500     MOVE '  GE'                  TO GODK-STATUSKODER                     
041600     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-AREA SSA1                    
041700     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     .                                                                    
042000                                                                          
042100 IMS-GNP-WDQ211-FIRST SECTION.                                            
042200                                                                          
042300     MOVE 'WDQ211  *F'            TO SSA1                                 
042400     MOVE '  GE'                  TO GODK-STATUSKODER                     
042500     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA SSA1                     
042600     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
042700     PERFORM IMS-STATUSKONTROLL                                           
042800     .                                                                    
042900                                                                          
043000 IMS-GHNP-WDQ212       SECTION.                                           
043100                                                                          
043200     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
043300          DELIMITED BY SIZE     INTO SSA1                                 
043400     MOVE '  GE'                  TO GODK-STATUSKODER                     
043500     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-AREA SSA1                    
043600     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
043700     PERFORM IMS-STATUSKONTROLL                                           
043800     .                                                                    
043900                                                                          
044000 IMS-GNP-WDQ212       SECTION.                                            
044100                                                                          
044200     MOVE 'WDQ212'                TO SSA1                                 
044300     MOVE '  GE'                  TO GODK-STATUSKODER                     
044400     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA SSA1                     
044500     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     .                                                                    
044800                                                                          
044900 IMS-REPL-WDQ2         SECTION.                                           
045000                                                                          
045100     MOVE '  '                    TO GODK-STATUSKODER                     
045200     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA                         
045300     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600                                                                          
045700 IMS-DLET-WDQ2         SECTION.                                           
045800                                                                          
045900     MOVE '  '                    TO GODK-STATUSKODER                     
046000     CALL CBLTDLI USING DLET WDQ2-PCB DLI-IO-AREA                         
046100     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-GHU-WDQ301        SECTION.                                           
046600                                                                          
046700     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
046800          DELIMITED BY SIZE     INTO SSA1                                 
046900     MOVE '  GE'                  TO GODK-STATUSKODER                     
047000     CALL CBLTDLI USING GHU WDQ3-PCB DLI-IO-AREA SSA1                     
047100     MOVE WDQ3-STATUS-CODE        TO STATUS-WS                            
047200     PERFORM IMS-STATUSKONTROLL                                           
047300     .                                                                    
047400                                                                          
047500 IMS-DLET-WDQ3         SECTION.                                           
047600                                                                          
047700     MOVE '  '                    TO GODK-STATUSKODER                     
047800     CALL CBLTDLI USING DLET WDQ3-PCB DLI-IO-AREA                         
047900     MOVE WDQ3-STATUS-CODE        TO STATUS-WS                            
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200     EJECT                                                                
048300 IMS-STATUSKONTROLL    SECTION.                                           
048400                                                                          
048500     SET STATUS-IX                TO 1                                    
048600     SEARCH GODK-STATUS                                                   
048700       AT END                                                             
048800         MOVE ' OGILTIG STATUSKOD: ' TO FELTEXT-STR                       
048900         DISPLAY FELTEXT STATUS-WS                                        
049000         CALL FELLOG                                                      
049100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
049200     END-SEARCH                                                           
049300     .                                                                    
