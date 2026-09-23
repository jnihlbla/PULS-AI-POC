000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000301 PROGRAM-ID.     W2212A00.                                                
000401*AUTHOR.         GÖRAN KJELLSON.                                          
000501*DATE-WRITTEN.   14/03/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*                                                                         
001203*        PARAMETER FILE FROM SCREEN 2149                                  
001303*                                                                         
001304*        VID INSERT/DELETE PÅ BILD 2149 SKALL ALLA BERÖRDA                
001305*        (GÄLLANDE) LEVERANSPLANER LOGGAS FÖR ATT SPECAS OM I             
001306*        KOMMANDE VECKOBATCH.UNDANTAG ÄR PLANER SOM SPÄRRATS MED          
001307*        MANUELL LEVERANSPLANESPÄRR.KDLPORS = 22.                         
001308*                                                                         
001309*                                                                         
001403*        PROGRAMMET LÄSER      WDD9 MED SB                                
001503*        PROGRAMMET LÄSER      WDK6                                       
001603*                                                                         
001800*                                                                         
001900                                                                          
002001                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002700*                                                                         
002800                                                                          
002903*          --- PARAMETER FILE FROM SCREEN 2149                            
003003     SELECT W2212A01                   ASSIGN TO W2212AD1.                
003103                                                                          
003203*          --- UT-FIL ARTIKLAR MED BLOCKADE AVROP (KDAVROP = 2)           
003303     SELECT W2212A                     ASSIGN TO W2212AD2.                
003401                                                                          
003501                                                                          
003601 DATA DIVISION.                                                           
003701 FILE SECTION.                                                            
003801                                                                          
003901                                                                          
004003 FD  W2212A01                                                             
004103     RECORDING       F                                                    
004203     BLOCK CONTAINS  0.                                                   
004303                                                                          
004403 01  IN-PARM           PIC X(80).                                         
004503                                                                          
004603                                                                          
004701 FD  W2212A                                                               
004801     RECORDING       F                                                    
004901     BLOCK CONTAINS  0.                                                   
005001                                                                          
005101*01  POST -COPY W2242204 -PRE UT-    -L.                                  
005201                                                                          
005301                                                                          
005401 WORKING-STORAGE SECTION.                                                 
005501                                                                          
005601 77  IDPGM                       PIC X(8)    VALUE 'W2212A00'.            
005701 77  JA                          PIC X       VALUE 'J'.                   
005801 77  NEJ                         PIC X       VALUE 'N'.                   
005901 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
006001 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
006101                                                                          
006203 77  PARM-EOF-SW                 PIC X       VALUE 'N'.                   
006303     88  END-OF-PARM                         VALUE 'J'.                   
006401                                                                          
007801                                                                          
007901                                                                          
008001*      --- VALID IDDC CODES                                               
008101*                                                                         
008201*01    -COPY WWDCKONS                                                     
008301                                                                          
008401                                                                          
008501                                                                          
008601 01  W-DAGENS-AAVV-X.                                                     
008602     03 W-DAGENS-AAVV            PIC 9(4)    VALUE ZERO.                  
009301                                                                          
009401 01  DYNAMISKA-SUBPROGRAM.                                                
009501*                                                                         
009601     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009701     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009801     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009901     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010002     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010201                                                                          
010202*    --- PARAMETRAR TILL WDATKONV                                         
010203*01      -COPY WDATAREA.                                                  
010301                                                                          
010401*    --- PARAMETRAR TILL ABEND                                            
010501                                                                          
010601 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010701 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010801                                                                          
010901 01  FELTEXT.                                                             
011001     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011101     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011201                                                                          
011301                                                                          
011401*    --- PARAMETRAR TILL DATKORT                                          
011501                                                                          
011601 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2212A'.              
011801 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011901                                                                          
012001*01  -COPY WDATKORT                                                       
012101                                                                          
012201                                                                          
012301*    --- PARAMETRAR TILL POSTSUM                                          
012401                                                                          
012501*01  -COPY W0005   -PRE  POSTSUM-                                         
012601                                                                          
012701                                                                          
012803 01  IN-PARM-START               PIC X(24)   VALUE                        
012903                                 'IN-PARM-START  '.                       
013003 01  PARM-AREA.                                                           
013103     05  IN-PARM-IDLEVNR-SHIP    PIC X(05) VALUE SPACE.                   
013304     05  IN-PARM-DAAVROP-FOM     PIC 9(06) VALUE ZERO.                    
013504     05  IN-PARM-DAAVROP-TOM     PIC 9(06) VALUE ZERO.                    
013703     05  IN-PARM-IDANSK-FOM      PIC 9(03) VALUE ZERO.                    
013903     05  IN-PARM-IDANSK-TOM      PIC 9(03) VALUE ZERO.                    
014703                                                                          
014803                                                                          
014903*01  AREA -COPY W2242204   -PRE UT-                                       
015003                                                                          
015103                                                                          
015203*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015303                                                                          
015403 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015503                                                                          
015603 01  NYCKLAR-TILL-DLI.                                                    
015703     03  W-KDAVROP-X.                                                     
015803         05  W-KDAVROP           PIC S9      VALUE +2   COMP-3.           
015903                                                                          
016003     03  W-WDD905KY-MIN-X.                                                
016103         05  W-DAAVROP-AVS-MIN   PIC 9(6)    VALUE ZERO.                  
016203         05  W-TILEVDAG-MIN      PIC S9      VALUE ZERO COMP-3.           
016303                                                                          
016403     03  W-WDD905KY-MAX-X.                                                
016503         05  W-DAAVROP-AVS-MAX   PIC 9(6)    VALUE ZERO.                  
016603         05  W-TILEVDAG-MAX      PIC S9      VALUE ZERO COMP-3.           
016703                                                                          
016803     03  W-IDARTNR-X.                                                     
016903         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017003                                                                          
017103                                                                          
017203                                                                          
017303*    --- STATUS-KOD FRÅN IMS                                              
017403 01  STATUS-WS                   PIC XX.                                  
017503     88  SEGMENT-FINNS                       VALUE '  '.                  
017603     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017604     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017703                                                                          
017803 01  GODK-STATUSKODER.                                                    
017903     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018003                                                                          
018103 01  ALL-SSA.                                                             
018203     03 SSA1                     PIC X(64).                               
018303     03 SSA2                     PIC X(64).                               
018403                                                                          
018503                                                                          
018603*    --- IMS FUNKTIONSKODER                                               
018703*01  -COPY W0003                                                          
018803                                                                          
018903*    ---  DLI INPUT-OUTPUT AREA                                           
019003                                                                          
019103                                                                          
019203 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDD905'.         
019303                                                                          
019403 01  DLI-IO-WDD905.                                                       
019503*    05  -COPY WDD905                                                     
019603                                                                          
019703                                                                          
020201 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
020301 01  DLI-IO-WDK601.                                                       
020401*    03  -COPY WDK601                                                     
020501                                                                          
020601 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
020701 01  DLI-IO-WDK611.                                                       
020801*    03  -COPY WDK611                                                     
020901                                                                          
021001                                                                          
021101                                                                          
021201 LINKAGE SECTION.                                                         
021301                                                                          
021401     EJECT                                                                
021501*01  -COPY W0008  -PRE WDD9-                                              
021601     05  WDD9-KEY-FB-IDARTNR       PIC S9(9) COMP-3.                      
021701     05  WDD9-KEY-FB-IDDC          PIC  X(2).                             
021801     05  WDD9-KEY-FB-IDLEVNR       PIC  X(5).                             
021901                                                                          
022001                                                                          
022101*01  -COPY W0008  -PRE WDK6-                                              
022201     05  FILLER                  PIC X.                                   
022301                                                                          
022401                                                                          
022501 PROCEDURE DIVISION  USING WDD9-PCB WDK6-PCB.                             
022601     ENTRY 'DLITCBL' USING WDD9-PCB WDK6-PCB.                             
022701                                                                          
022801     PERFORM A-INIT                                                       
023103                                                                          
023203     PERFORM S01-READ-W2212A01                                            
023303                                                                          
023304     PERFORM UNTIL END-OF-PARM                                            
023305                OR IN-PARM-IDLEVNR-SHIP = SPACE                           
023306        MOVE IN-PARM-DAAVROP-FOM TO W-DAAVROP-AVS-MIN                     
023307        MOVE IN-PARM-DAAVROP-TOM TO W-DAAVROP-AVS-MAX                     
023603        PERFORM IMS-GN-WDD905                                             
023701        PERFORM UNTIL SEGMENT-SLUT                                        
024001                                                                          
024101           IF WDD9-KEY-FB-IDDC = WC-CDC-SE                                
024104              IF WDD9-KEY-FB-IDARTNR NOT = W-IDARTNR                      
024201                 MOVE WDD9-KEY-FB-IDARTNR TO W-IDARTNR                    
024301                 PERFORM IMS-GU-WDK601                                    
024401                 IF SEGMENT-FINNS AND                                     
024501                    ART-IDLEVNR = WDD9-KEY-FB-IDLEVNR                     
024601                                                                          
024703                    PERFORM B-BEHANDLA-AVROP                              
024801                 END-IF                                                   
024802              END-IF                                                      
024901           END-IF                                                         
025001                                                                          
025203           PERFORM IMS-GN-WDD905                                          
025301        END-PERFORM                                                       
025302                                                                          
025303        PERFORM S01-READ-W2212A01                                         
025304     END-PERFORM                                                          
025401                                                                          
025501     PERFORM Z-FINIT                                                      
025601                                                                          
025701     MOVE ZERO TO RETURN-CODE                                             
025801     GOBACK                                                               
025901     .                                                                    
026001                                                                          
026101                                                                          
026201                                                                          
026301 A-INIT SECTION.                                                          
026401     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
026501                                                                          
026603     OPEN INPUT  W2212A01                                                 
026703                                                                          
026704     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
026705     CALL WDATKONV USING DAT-KDDATFORM                                    
026706                         DAT-I-TIDATUM                                    
026707                         DAT-O-TIDATUM                                    
026708                         DAT-KDSVAR                                       
026709     MOVE DAT-TIAAVV-GRP      TO W-DAGENS-AAVV-X                          
026901                                                                          
027103     MOVE ZERO                TO W-TILEVDAG-MIN                           
027303     MOVE 7                   TO W-TILEVDAG-MAX                           
027401                                                                          
027501     OPEN OUTPUT W2212A                                                   
027601     .                                                                    
027701                                                                          
027801 B-BEHANDLA-AVROP   SECTION.                                              
027901     MOVE 'B-BEHANDLA-AVROP' TO CURRENT-SECTION                           
028001                                                                          
028101     PERFORM IMS-GNP-WDK611                                               
028201     IF SEGMENT-FINNS                                                     
028303        IF  CLAG-IDLEVNR-SHIP = IN-PARM-IDLEVNR-SHIP                      
028403        AND   (CLAG-IDANSK >= IN-PARM-IDANSK-FOM                          
028503          AND  CLAG-IDANSK <= IN-PARM-IDANSK-TOM)                         
028601                                                                          
028701           IF  CLAG-KDLPSP = 3                                            
028801           AND CLAG-TILPSP > W-DAGENS-AAVV                                
028901               CONTINUE                                                   
029001           ELSE                                                           
029101               MOVE '2203'       TO UT-IDHTYP                             
029201               MOVE WC-CDC-SE    TO UT-IDDC                               
029301               MOVE 22           TO UT-KDLPORS                            
029401               MOVE ART-IDARTNR  TO UT-IDARTNR                            
029501               PERFORM S12-SKRIV-W2212A                                   
029601           END-IF                                                         
029701        END-IF                                                            
029801     END-IF                                                               
029901     .                                                                    
030001                                                                          
030101                                                                          
030201                                                                          
030301 Z-FINIT SECTION.                                                         
030401     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
030501                                                                          
030603     CLOSE W2212A01 W2212A                                                
030701                                                                          
030801     MOVE 'S' TO POSTSUM-OPKOD                                            
030901     CALL POSTSUM USING POSTSUM-PARM                                      
031001     .                                                                    
031101                                                                          
031201                                                                          
031305 S01-READ-W2212A01  SECTION.                                              
031405                                                                          
031505     READ W2212A01 INTO PARM-AREA                                         
031604     AT END                                                               
031704        MOVE HIGH-VALUE TO PARM-AREA                                      
031804        SET END-OF-PARM TO TRUE                                           
031904                                                                          
032004     NOT AT END                                                           
032104        MOVE 'W2212A01' TO POSTSUM-FDNAMN                                 
032204        MOVE 'W2212AD1' TO POSTSUM-DDNAMN2                                
032304        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
032404        CALL POSTSUM USING POSTSUM-PARM                                   
032504     END-READ                                                             
032604     .                                                                    
032704                                                                          
032804                                                                          
032901 S12-SKRIV-W2212A SECTION.                                                
033001                                                                          
033101     WRITE UT-POST FROM UT-AREA                                           
033201                                                                          
033301     MOVE 'W2212A'   TO POSTSUM-FDNAMN                                    
033404     MOVE 'W2212AD2' TO POSTSUM-DDNAMN2                                   
033501     CALL POSTSUM USING POSTSUM-PARM                                      
033601     .                                                                    
033701                                                                          
033801                                                                          
033901* --- IMS SEKTIONER ---                                                   
034001                                                                          
034103 IMS-GN-WDD905 SECTION.                                                   
034203     MOVE 'IMS-GN-WDD905   ' TO CURRENT-IMS-SECTION                       
034301                                                                          
034401     STRING 'WDD905  (WDD905KY>=' W-WDD905KY-MIN-X                        
034501                    '&WDD905KY<=' W-WDD905KY-MAX-X                        
034601                    '&KDAVROP  =' W-KDAVROP-X ')'                         
034701          DELIMITED BY SIZE INTO SSA1                                     
034801     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-WDD905 SSA1                    
034901     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
035001     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
035101     PERFORM IMS-STATUSKONTROLL                                           
035201     .                                                                    
035301                                                                          
035401                                                                          
035501 IMS-GU-WDK601 SECTION.                                                   
035502     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
035601                                                                          
035701     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
035801          DELIMITED BY SIZE INTO SSA1                                     
035901     MOVE '  GE' TO GODK-STATUSKODER                                      
036001     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
036101     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
036201     PERFORM IMS-STATUSKONTROLL                                           
036301     .                                                                    
036401                                                                          
036501                                                                          
036601 IMS-GNP-WDK611 SECTION.                                                  
036602     MOVE 'IMS-GNP-WDK611  ' TO CURRENT-IMS-SECTION                       
036701                                                                          
036801     MOVE 'WDK611 '           TO SSA1                                     
036901     MOVE '  GE' TO GODK-STATUSKODER                                      
037001     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
037101     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
037201     PERFORM IMS-STATUSKONTROLL                                           
037301     .                                                                    
037401     EJECT                                                                
037501                                                                          
037601 IMS-STATUSKONTROLL SECTION.                                              
037701                                                                          
037801     SET STATUS-IX TO 1                                                   
037901     SEARCH GODK-STATUS                                                   
038001       AT END                                                             
038101         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
038201           DELIMITED BY SIZE INTO FELTEXT-STR                             
038301         DISPLAY FELTEXT                                                  
038401         CALL FELLOG                                                      
038501       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
038601         CONTINUE                                                         
038701     END-SEARCH                                                           
039001     .                                                                    
