001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W1151400.                                                
001400*AUTHOR.         BODIL LINDAHL.                                           
001500*DATE-WRITTEN.   92/12/16.                                                
001600                                                                          
001700*    REMARKS.                                                             
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        PROGRAMMET LÄSER FIL MED SPÄRRADE ORDERRADER BASLAGER.           
002100*        TPO RÄKNAS OM FÖR ARTIKEL.                                       
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WLARTM (WDK9)                              
002400*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003601     SKIP2                                                                
003602*          --- SPÄRRADE ORDERRADER BASLAGER                               
003610     SELECT INFIL                      ASSIGN TO W11514D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  INFIL                                                                
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205     SKIP2                                                                
004210*01  -COPY W414007      -L.                                               
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W1151400'.            
004700 01  CHKP-VAR.                                                            
004800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005300 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005600                                                                          
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101                                                                          
006102 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
006110     88  END-OF-INFIL                        VALUE 'J'.                   
006400                                                                          
006500 01  WS-AAAAVV.                                                           
006700     03  WS-SEKEL                  PIC 9(2).                              
006710     03  WS-AA                     PIC 9(2).                              
006800     03  WS-VV                     PIC 9(2).                              
006900 01  WS-DABEHOV REDEFINES WS-AAAAVV  PIC 9(6).                            
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007501     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007510     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007601     EJECT                                                                
007602*    --- PARAMETRAR TILL POSTSUM                                          
007603*                                                                         
007610*01  -COPY W0005   -PRE  POSTSUM-                                         
007801     EJECT                                                                
007810*01  -COPY WDATAREA                                                       
007901     EJECT                                                                
007902 01  IN-AREA-START               PIC X(24)   VALUE                        
007903                                             'IN-AREA-START'.             
007904     SKIP2                                                                
007905                                                                          
007910*01  AREA -COPY W414007     -PRE IN-                                      
008000*                                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008501     03  W-IDARTNR-X.                                                     
008502         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008503     03  W-DABEHOV-X.                                                     
008520         05  W-DABEHOV           PIC  9(6)   VALUE ZERO.                  
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010700     SKIP3                                                                
010800 01  DLI-IO-AREA.                                                         
010900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011001     SKIP3                                                                
011002     03  WLARTM01 REDEFINES IO-AREA.                                      
011003*        05  -COPY WDK901                                                 
011004     EJECT                                                                
011005     03  WLARTM11 REDEFINES IO-AREA.                                      
011006*        05  -COPY WDK911                                                 
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009   -PRE MSG-                                              
012401     EJECT                                                                
012402*01  -COPY W0008   -PRE ARTM-                                             
012410     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012801 PROCEDURE DIVISION  USING MSG-PCB ARTM-PCB.                              
012810     ENTRY 'DLITCBL' USING MSG-PCB ARTM-PCB.                              
012900                                                                          
013200     PERFORM A-INIT                                                       
013310     PERFORM S01-LAES-INFIL                                               
013400     PERFORM UNTIL END-OF-INFIL                                           
013401                                                                          
013440        PERFORM B-RAKNA-OM-TPO                                            
013450                                                                          
013500        IF CHKP-ANT > CHKP-MAX                                            
013600           PERFORM X-TAG-CHECKPOINT                                       
013700        END-IF                                                            
013800                                                                          
014410        PERFORM S01-LAES-INFIL                                            
014500     END-PERFORM                                                          
014700                                                                          
014800     PERFORM Z-FINIT                                                      
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500                                                                          
015700     PERFORM IMS-RESTART                                                  
015901     MOVE ZERO TO CHKP-ANT                                                
015902                                                                          
015910     OPEN INPUT INFIL                                                     
016610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     .                                                                    
017100     EJECT                                                                
017110 B-RAKNA-OM-TPO SECTION.                                                  
017120                                                                          
017121     MOVE IN-BASL-IDARTNR TO W-IDARTNR                                    
017130     PERFORM IMS-GET-ARTM01                                               
017140     IF SEGMENT-FINNS                                                     
017141                                                                          
017142        PERFORM BA-WDATKONV                                               
017143        IF DAT-KDSVAR-OK                                                  
017150           SUBTRACT IN-BASL-KVBEART-Q                                     
017151                 FROM ART-SUTPO-TOT                                       
017160           IF ART-SUTPO-TOT < ZERO                                        
017170              MOVE ZERO TO ART-SUTPO-TOT                                  
017180           END-IF                                                         
017190                                                                          
017191           PERFORM IMS-REPL-ARTM                                          
017192           ADD +1 TO CHKP-ANT                                             
017193                                                                          
017195           MOVE WS-DABEHOV TO W-DABEHOV                                   
017196           PERFORM IMS-GET-ARTM11                                         
017197           IF SEGMENT-FINNS                                               
017198                                                                          
017199             SUBTRACT IN-BASL-KVBEART-Q FROM ANT-SUTPO-EJPB               
017200             IF ANT-SUTPO-EJPB < ZERO                                     
017201                MOVE ZERO TO ANT-SUTPO-EJPB                               
017202             END-IF                                                       
017203             IF (ANT-SUTPO-EJPB = ZERO) AND                               
017204                (ANT-SUTPO-PB = ZERO)                                     
017205                 PERFORM IMS-DLET-ARTM                                    
017207             ELSE                                                         
017208                 PERFORM IMS-REPL-ARTM                                    
017210             END-IF                                                       
017211             ADD +1 TO CHKP-ANT                                           
017212                                                                          
017213           END-IF                                                         
017214        END-IF                                                            
017215     END-IF                                                               
017216     .                                                                    
017217     EJECT                                                                
018202 BA-WDATKONV SECTION.                                                     
018204                                                                          
018205     MOVE IN-BASL-TITPO TO DAT-I-TIDATUM                                  
018209     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
018210                                                                          
018211     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
018212                         DAT-O-TIDATUM DAT-KDSVAR                         
018213                                                                          
018214     IF DAT-KDSVAR-OK                                                     
018215        MOVE DAT-TIAA-VECKA TO WS-AA                                      
018216        MOVE DAT-TIVV       TO WS-VV                                      
018217        MOVE DAT-TISEKEL    TO WS-SEKEL                                   
018220     END-IF                                                               
018223     .                                                                    
018224     EJECT                                                                
018225 X-TAG-CHECKPOINT   SECTION.                                              
018226                                                                          
018227     PERFORM IMS-CHECKPOINT                                               
018228     MOVE ZERO TO CHKP-ANT                                                
018229     .                                                                    
018230     EJECT                                                                
018231 Z-FINIT SECTION.                                                         
018232                                                                          
018233     CLOSE INFIL                                                          
018234                                                                          
018235     MOVE 'S' TO POSTSUM-OPKOD                                            
018236     CALL POSTSUM USING POSTSUM-PARM                                      
018237     .                                                                    
018238     EJECT                                                                
018239 S01-LAES-INFIL SECTION.                                                  
018240                                                                          
018241     READ INFIL INTO IN-AREA                                              
018242     AT END                                                               
018243        SET END-OF-INFIL TO TRUE                                          
018244                                                                          
018245     NOT AT END                                                           
018246        MOVE 'INFIL'    TO POSTSUM-FDNAMN                                 
018247        MOVE 'W11514D1' TO POSTSUM-DDNAMN2                                
018248        MOVE SPACE      TO POSTSUM-TRANSTYP                               
018249        CALL POSTSUM USING POSTSUM-PARM                                   
018250     END-READ                                                             
018260     .                                                                    
018500     EJECT                                                                
019800* --- IMS SEKTIONER ---                                                   
019900     SKIP3                                                                
020002 IMS-GET-ARTM01 SECTION.                                                  
020003     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
020004          DELIMITED BY SIZE INTO SSA1                                     
020005     MOVE '  GE' TO GODK-STATUSKODER                                      
020006     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA SSA1                     
020007     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
020008     PERFORM IMS-STATUSKONTROLL                                           
020009     .                                                                    
020010     SKIP3                                                                
020011 IMS-GET-ARTM11 SECTION.                                                  
020012     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
020013          DELIMITED BY SIZE INTO SSA1                                     
020014     MOVE '  GE' TO GODK-STATUSKODER                                      
020015     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-AREA SSA1                    
020016     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
020017     PERFORM IMS-STATUSKONTROLL                                           
020018     .                                                                    
020019     EJECT                                                                
020020 IMS-REPL-ARTM SECTION.                                                   
020022     MOVE '  ' TO GODK-STATUSKODER                                        
020023     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA                         
020024     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
020025     PERFORM IMS-STATUSKONTROLL                                           
020026     .                                                                    
020027     SKIP3                                                                
020028 IMS-DLET-ARTM SECTION.                                                   
020030     MOVE '  ' TO GODK-STATUSKODER                                        
020031     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-AREA                         
020032     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
020033     PERFORM IMS-STATUSKONTROLL                                           
020040     .                                                                    
020100     EJECT                                                                
020200 IMS-RESTART SECTION.                                                     
020300     SKIP2                                                                
020400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020500     MOVE '  ' TO GODK-STATUSKODER                                        
020600     CALL CBLTDLI USING XRST MSG-PCB                                      
020700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020800                        CHKP-AREA-LENGTH CHKP-AREA                        
020900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021000     PERFORM IMS-STATUSKONTROLL                                           
021100     .                                                                    
021200     SKIP3                                                                
021300 IMS-CHECKPOINT SECTION.                                                  
021400     SKIP2                                                                
021500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021600     MOVE '  XD' TO GODK-STATUSKODER                                      
021700     CALL CBLTDLI USING CHKP MSG-PCB                                      
021800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021900                        CHKP-AREA-LENGTH CHKP-AREA                        
022000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022100     PERFORM IMS-STATUSKONTROLL                                           
022200                                                                          
022300     IF IMS-EJ-OK                                                         
022400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022500       DISPLAY FELTEXT                                                    
022600       CALL FELLOG                                                        
022700     END-IF                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 IMS-STATUSKONTROLL SECTION.                                              
023100     SKIP2                                                                
023200     SET STATUS-IX TO 1                                                   
023300     SEARCH GODK-STATUS                                                   
023400       AT END                                                             
023500         STRING 'FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                 
023510            DELIMITED BY SIZE INTO FELTEXT-STR                            
023600         DISPLAY FELTEXT                                                  
023700         CALL FELLOG                                                      
023800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023900         CONTINUE                                                         
024000     END-SEARCH                                                           
024100     .                                                                    
