000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4268100.                                                
000400*AUTHOR.         ANNELIE ENGLUND.                                         
000500*DATE-WRITTEN.   93/10/27.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER WDL2 - PARTIER SOM SKULLE HA KUNNAT BLI                
001100*        UTTAGNA TILL KONTROLL - PÅ FIL W020.AAVV.WINLHIST                
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLINLE (WDL2)  SB                          
001400*                              W6LEVA (W6F1)  DLI                         
001500*                              WLARTC (WDK6)  DLI                         
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- FIL MED WDL2-INFO                                          
003000     SELECT W42681                     ASSIGN TO W42681D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W42681                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000*01  POST -COPY W4268101 -PRE  UT-  -L.                                   
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W4268100'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  SPAR-AAVV                   PIC 9(4)    VALUE ZERO.                  
005000 77  W-AAVV                      PIC 9(4)    VALUE ZERO.                  
005100                                                                          
005200 77  ARTNR-SW                    PIC X       VALUE 'J'.                   
005300     88  ARTNR-OK                            VALUE 'J'.                   
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100 01  TIREGDATUM                  PIC 9(16)   VALUE ZERO.                  
006200 01  FILLER REDEFINES TIREGDATUM.                                         
006300     03  TIREGDAT-SEKEL          PIC 9(2).                                
006400     03  TIREGDAT-AAR            PIC 9(2).                                
006500     03  TIREGDAT-MAANAD         PIC 9(2).                                
006600     03  TIREGDAT-DAG            PIC 9(2).                                
006700     03  TIREGDAT-REST           PIC 9(8).                                
006800     SKIP2                                                                
006900 01  INLDATUM                    PIC 9(6)   VALUE ZERO.                   
007000 01  FILLER REDEFINES INLDATUM.                                           
007100     03  INLDATUM-AAR            PIC 9(2).                                
007200     03  INLDATUM-MAANAD         PIC 9(2).                                
007300     03  INLDATUM-DAG            PIC 9(2).                                
007400     EJECT                                                                
007500 01  TEST-IDARTNR                PIC 9(9) COMP-3.                         
007600                                                                          
007700*01  FILLER -COPY WWBYT03 -RED TEST-IDARTNR                               
007800                                                                          
007900*01  -COPY WWPRODSL                                                       
008000                                                                          
008100     EJECT                                                                
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008900     SKIP2                                                                
009000*    --- PARAMETRAR TILL ABEND                                            
009100                                                                          
009200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009400     SKIP2                                                                
009500 01  FELTEXT.                                                             
009600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200     EJECT                                                                
010300*01  -COPY WDATAREA                                                       
010400     EJECT                                                                
010500 01  UT-AREA-START               PIC X(24)   VALUE                        
010600                                 'UT-AREA-START  '.                       
010700     SKIP2                                                                
010800                                                                          
010900*01  AREA -COPY W4268101     -PRE UT-                                     
011000     EJECT                                                                
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200*                                                                         
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011900     03  W-IDLEVNR-X.                                                     
012000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012100     SKIP2                                                                
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012700     88  SEGMENT-TYP-SLUT                    VALUE 'GA'.                  
012800     SKIP2                                                                
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013400     EJECT                                                                
013500*    --- IMS FUNKTIONSKODER                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA1'.         
014000     SKIP3                                                                
014100 01  DLI-IO-AREA1.                                                        
014200     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
014300     SKIP3                                                                
014400     03  WLINLE11 REDEFINES IO-AREA1.                                     
014500*        05  -COPY WDL201  -PRE INLE-                                     
014600                                                                          
014700     03  WLINLE11 REDEFINES IO-AREA1.                                     
014800*        05  -COPY WDL211  -PRE INLE-                                     
014900                                                                          
015000     03  WLINLE21 REDEFINES IO-AREA1.                                     
015100*        05  -COPY WDL221  -PRE INLE-                                     
015200     EJECT                                                                
015300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA2'.          
015400     SKIP3                                                                
015500 01  DLI-IO-AREA2.                                                        
015600     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
015700     SKIP3                                                                
015800     03  W6LEVA01 REDEFINES IO-AREA2.                                     
015900*        05  -COPY W6F101  -PRE LEVA-                                     
016000     EJECT                                                                
016100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA3'.          
016200     SKIP3                                                                
016300 01  DLI-IO-AREA3.                                                        
016400     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
016500     SKIP3                                                                
016600     03  WLARTC01 REDEFINES IO-AREA3.                                     
016700*        05  -COPY WDK601                                                 
016800     EJECT                                                                
016900 LINKAGE SECTION.                                                         
017000                                                                          
017100     EJECT                                                                
017200*01  -COPY W0008  -PRE INLE-                                              
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008  -PRE LEVA-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800*01  -COPY W0008  -PRE ARTC-                                              
017900     05  FILLER                  PIC X.                                   
018000     EJECT                                                                
018100 PROCEDURE DIVISION  USING INLE-PCB LEVA-PCB ARTC-PCB.                    
018200     ENTRY 'DLITCBL' USING INLE-PCB LEVA-PCB ARTC-PCB.                    
018300                                                                          
018400     PERFORM A-INIT                                                       
018500     PERFORM IMS-GET-INLE-WDL2                                            
018600     PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                         
018700       EVALUATE INLE-SEG-NAME-FB                                          
018800       WHEN 'WDL201'                                                      
018900         MOVE INLE-ART-IDARTNR TO W-IDARTNR                               
019000                                  TEST-IDARTNR                            
019100       WHEN 'WDL211'                                                      
019200         COMPUTE TIREGDATUM = 9999999999999999 - INLE-INL-DAINLEV         
019300         MOVE TIREGDAT-AAR    TO INLDATUM-AAR                             
019400         MOVE TIREGDAT-MAANAD TO INLDATUM-MAANAD                          
019500         MOVE TIREGDAT-DAG    TO INLDATUM-DAG                             
019600       WHEN 'WDL221'                                                      
019700         MOVE INLDATUM TO DAT-I-TIDATUM                                   
019800         IF INLDATUM-AAR = DAGENS-DATUM-AAR                               
019900           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
020000                                                                          
020100           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
020200                           DAT-O-TIDATUM DAT-KDSVAR                       
020300                                                                          
020400           IF DAT-KDSVAR-OK                                               
020500             MOVE DAT-TIAAVV-GRP TO W-AAVV                                
020600           ELSE                                                           
020700             PERFORM S99-ABEND                                            
020800           END-IF                                                         
020900                                                                          
021000           IF W-AAVV = SPAR-AAVV                                          
021100             IF BYT03-OBJEKT                                              
021200             OR TEST-IDARTNR = 8401236                                    
021300                MOVE NEJ         TO ARTNR-SW                              
021400             ELSE                                                         
021500               PERFORM IMS-GU-ARTC-WDK601                                 
021600               MOVE ART-KDPRODSL  TO TEST-KDPRODSL                        
021700               IF KDPRODSL-BIMA                                           
021800                 MOVE NEJ         TO ARTNR-SW                             
021900               ELSE                                                       
022000                 MOVE JA TO ARTNR-SW                                      
022100               END-IF                                                     
022200             END-IF                                                       
022300             IF ARTNR-OK                                                  
022400               MOVE INLE-MOT-IDLEVNR TO W-IDLEVNR                         
022500               PERFORM IMS-GU-LEVA-W6F101                                 
022600               IF SEGMENT-FINNS                                           
022700                 IF LEVA-LEV-FLSKPLOT = JA                                
022800                   IF INLE-MOT-KDRT = +0 OR +1 OR +2 OR +4 OR             
022900                                      +5 OR +9 OR +10                     
023000                     PERFORM B-FLYTTA-WDL221-TILL-W4268101                
023100                     PERFORM S11-SKRIV-W42681                             
023200                   END-IF                                                 
023300                 END-IF                                                   
023400               ELSE                                                       
023500                 IF INLE-MOT-KDRT = +0 OR +1 OR +2 OR +4 OR               
023600                                    +5 OR +9 OR +10                       
023700                   PERFORM B-FLYTTA-WDL221-TILL-W4268101                  
023800                   PERFORM S11-SKRIV-W42681                               
023900                 END-IF                                                   
024000               END-IF                                                     
024100             END-IF                                                       
024200           END-IF                                                         
024300         END-IF                                                           
024400       END-EVALUATE                                                       
024500       PERFORM IMS-GET-INLE-WDL2                                          
024600     END-PERFORM                                                          
024700                                                                          
024800                                                                          
024900     PERFORM Z-FINIT                                                      
025000                                                                          
025100     MOVE ZERO TO RETURN-CODE                                             
025200     GOBACK                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 A-INIT SECTION.                                                          
025600                                                                          
025700     OPEN OUTPUT W42681                                                   
025800     SKIP2                                                                
025900     ACCEPT DAGENS-DATUM  FROM DATE                                       
026000*    MOVE 931022 TO DAGENS-DATUM                                          
026100     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
026200     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
026300                                                                          
026400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
026500                     DAT-O-TIDATUM DAT-KDSVAR                             
026600                                                                          
026700     IF DAT-KDSVAR-OK                                                     
026800       MOVE DAT-TIAAVV-GRP TO SPAR-AAVV                                   
026900     ELSE                                                                 
027000       PERFORM S99-ABEND                                                  
027100     END-IF                                                               
027200                                                                          
027300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027400     .                                                                    
027500     EJECT                                                                
027600 B-FLYTTA-WDL221-TILL-W4268101 SECTION.                                   
027700                                                                          
027800     MOVE INLE-MOT-IDPTYP       TO UT-IDPTYP                              
027900     MOVE INLE-MOT-IDLOPNRM     TO UT-IDLOPNRM                            
028000     MOVE INLE-MOT-IDAVINR      TO UT-IDAVINR                             
028100     MOVE INLE-MOT-IDKONTO      TO UT-IDKONTO                             
028200     MOVE INLE-MOT-IDLEVNR      TO UT-IDLEVNR                             
028300     MOVE INLE-MOT-ADLAGOMR     TO UT-ADLAGOMR                            
028400     MOVE INLE-MOT-ADGANG       TO UT-ADGANG                              
028500     MOVE INLE-MOT-ADPLATS      TO UT-ADPLATS                             
028600     MOVE '1'                   TO UT-KDCLAGER                            
028700     MOVE INLE-MOT-KDRT         TO UT-KDRT                                
028800     MOVE INLE-MOT-KDAVVANT     TO UT-KDAVVANT                            
028900     MOVE INLE-MOT-KDAVVKV      TO UT-KDAVVKV                             
029000     MOVE INLE-MOT-KVANTMOT     TO UT-KVANTMOT                            
029100     MOVE INLE-MOT-KVAVIS       TO UT-KVAVIS                              
029200     MOVE INLE-MOT-KVFORDEL     TO UT-KVFORDEL                            
029300     MOVE INLE-MOT-KVRETUR      TO UT-KVRETUR                             
029400     MOVE INLE-MOT-KVFORV       TO UT-KVFORV                              
029500     MOVE INLE-MOT-TIAVIDAT     TO UT-TIAVIDAT                            
029600     MOVE INLE-MOT-TIUPPDAT     TO UT-TIUPPDAT                            
029700     .                                                                    
029800     EJECT                                                                
029900 Z-FINIT SECTION.                                                         
030000     CLOSE W42681                                                         
030100     SKIP2                                                                
030200     MOVE 'S' TO POSTSUM-OPKOD                                            
030300     CALL POSTSUM USING POSTSUM-PARM                                      
030400     .                                                                    
030500     EJECT                                                                
030600 S11-SKRIV-W42681 SECTION.                                                
030700     SKIP2                                                                
030800     WRITE UT-POST FROM UT-AREA                                           
030900                                                                          
031000     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
031100     MOVE 'W42681' TO POSTSUM-FDNAMN                                      
031200     MOVE 'W42681D1' TO POSTSUM-DDNAMN2                                   
031300     CALL POSTSUM USING POSTSUM-PARM                                      
031400     .                                                                    
031500     EJECT                                                                
031600 S99-ABEND SECTION.                                                       
031700     SKIP2                                                                
031800     SKIP2                                                                
031900     MOVE 'S' TO POSTSUM-OPKOD                                            
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
032200     .                                                                    
032300     EJECT                                                                
032400* --- IMS SEKTIONER ---                                                   
032500     SKIP3                                                                
032600     EJECT                                                                
032700 IMS-GET-INLE-WDL2    SECTION.                                            
032800     SKIP2                                                                
032900     CALL CBLTDLI USING GN INLE-PCB DLI-IO-AREA1                          
033000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
033100     MOVE '  GAGKGBGA' TO GODK-STATUSKODER                                
033200     PERFORM IMS-STATUSKONTROLL                                           
033300     .                                                                    
033400     EJECT                                                                
033500 IMS-GU-LEVA-W6F101 SECTION.                                              
033600     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
033700          DELIMITED BY SIZE INTO SSA1                                     
033800     MOVE '  GE' TO GODK-STATUSKODER                                      
033900     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA2 SSA1                     
034000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300     EJECT                                                                
034400 IMS-GU-ARTC-WDK601 SECTION.                                              
034500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
034600           DELIMITED BY SIZE INTO SSA1                                    
034700     MOVE '  ' TO GODK-STATUSKODER                                        
034800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1                     
034900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
035000     PERFORM IMS-STATUSKONTROLL                                           
035100     .                                                                    
035200     SKIP3                                                                
035300 IMS-STATUSKONTROLL SECTION.                                              
035400     SKIP2                                                                
035500     SET STATUS-IX TO 1                                                   
035600     SEARCH GODK-STATUS                                                   
035700       AT END                                                             
035800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
035900         DISPLAY FELTEXT                                                  
036000         CALL FELLOG                                                      
036100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036200         CONTINUE                                                         
036300     END-SEARCH                                                           
036400     .                                                                    
