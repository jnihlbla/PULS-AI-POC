000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W612TIME.                                                
000400 AUTHOR.         JOHAN LINDKVIST.                                         
000500 DATE-WRITTEN.   97/06/03.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SUBMODUL SOM ANVÄNDS TILL DAGLIG/VECKO LIST FRAMSTÄLLNING        
001100*        (W6126000, W6127600 & W6128600).                                 
001200*        RÄKNAR FRAM TIDEN I ARBETSDAGAR (EN DECIMAL) MELLAN TVÅ          
001300*        TI-MOTTAGEN OCH TI-INLAGD                                        
001400*                                                                         
001500* ÄNDRING:   97-11-27   JOHAN L                                           
001510*        FÖR ATT UNDVIKA ABEND I PERIODKÖRNING I ARBKONV                  
001530*        Y2K ÄNDRING                                                      
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
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003310*    -COPY WY2000W1                                                       
003320     SKIP3                                                                
003400 01  FILLER.                                                              
004200     03  NDC-TIDER.                                                       
004300       05  NDC-STARTTID          PIC 9(4)    COMP-3.                      
004400       05  NDC-SLUTTID           PIC 9(4)    COMP-3.                      
004500       05  CHAR-STARTTID         PIC X(4)  JUST RIGHT.                    
004600       05  CHAR-SLUTTID          PIC X(4)  JUST RIGHT.                    
004700       05  KONV-NDC-STARTTID     PIC 9(4)    COMP-3.                      
004800       05  KONV-NDC-SLUTTID      PIC 9(4)    COMP-3.                      
004900                                                                          
005000     03  INL-MOT-TIDER.                                                   
005100       05  TEMP-MOTTID           PIC 9(4)    COMP-3.                      
005200       05  TEMP-INLTID           PIC 9(4)    COMP-3.                      
005300       05  KONV-TIINLMTI         PIC 9(4)    COMP-3.                      
005400       05  KONV-TIINLITI         PIC 9(4)    COMP-3.                      
005500                                                                          
005600 01  FILLER.                                                              
005700     03  CHAR-TID                PIC X(4)  JUST RIGHT.                    
005800     03  KONV-TID                PIC 9(4)    COMP-3.                      
005900                                                                          
006000 01  FILLER.                                                              
006100     03 TIDS-DIFF-DAYS           PIC S9(5)  COMP-3.                       
006200     03 TIDS-DIFF-MIN            PIC S9(5)  COMP-3.                       
006400                                                                          
006500 01 FILLER.                                                               
006600     03 KD-DIFF-ARB-DAG          PIC 9(3)   VALUE 001.                    
006700     03 KD-NEXT-ARB-DAG          PIC 9(3)   VALUE 002.                    
006800     03 KD-LAST-ARB-DAG          PIC 9(3)   VALUE 003.                    
006900                                                                          
007000*    -- CHECKED BY WY2000                                                 
007100 77  IDPGM                       PIC X(8)    VALUE 'W612TIME'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007310 77  TIME-KVDAGDEC-W             PIC S9(4)V9(3)      COMP-3.              
007400     EJECT                                                                
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000     EJECT                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008210     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
008410     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     SKIP2                                                                
008600*    --- PARAMETRAR TILL ABEND                                            
008700                                                                          
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009100     SKIP2                                                                
009200 01  FELTEXT.                                                             
009300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009500     EJECT                                                                
009600*01  -COPY WORKAREA                                                       
009700     EJECT                                                                
009701*                                                                         
009702 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009703     SKIP3                                                                
009704 01  NYCKLAR-TILL-DLI.                                                    
009705                                                                          
009706     03  W-IDDC-B6-X.                                                     
009707         05 W-IDDC-B6                  PIC X(2).                          
009708                                                                          
009709     SKIP2                                                                
009710*    --- STATUS-KOD FRÅN IMS                                              
009711 01  STATUS-WS                   PIC XX.                                  
009712     88  SEGMENT-FINNS                       VALUE '  '.                  
009713     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009714     SKIP2                                                                
009715 01  GODK-STATUSKODER.                                                    
009716     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009717     SKIP3                                                                
009718 01  SSA1                        PIC X(64).                               
009720     EJECT                                                                
009721*    --- IMS FUNKTIONSKODER                                               
009722*01  -COPY W0003                                                          
009723     EJECT                                                                
009724*    ---  DLI INPUT-OUTPUT AREA                                           
009725 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
009726 01   DLI-IO-AREA-B601.                                                   
009727*     03  -COPY WDB601.                                                   
009728                                                                          
009740 LINKAGE SECTION.                                                         
009741*01 -COPY W612TID    -PRE TIME-                                           
009742                                                                          
009743*01  -COPY W0008     -PRE WDB6-                                           
009744     05  FILLER                  PIC X.                                   
009745     EJECT                                                                
009750                                                                          
009800 PROCEDURE DIVISION USING TIME-W612TID WDB6-PCB.                          
009900 MAIN SECTION.                                                            
010000                                                                          
010100                                                                          
010200     PERFORM B-GET-NDC-TIDER                                              
010300     PERFORM C-CHECK-STARTTID                                             
010400     PERFORM D-CHECK-SLUTTID                                              
010500     PERFORM E-CHECK-KLOCKSLAG-START                                      
010600     PERFORM F-CHECK-KLOCKSLAG-SLUT                                       
010700     PERFORM G-KONVERTERA-TID                                             
010800     PERFORM H-BERAK-SKILLNAD-I-TID                                       
010900     MOVE ZERO TO RETURN-CODE                                             
010910     MOVE JA   TO TIME-KDSVAR                                             
011000     GOBACK                                                               
011100     .                                                                    
011200 B-GET-NDC-TIDER SECTION.                                                 
011300                                                                          
011310     MOVE TIME-IDDC      TO W-IDDC-B6                                     
011320                            WORK-IDDC                                     
011400     PERFORM IMS-GU-WDB601                                                
011500     MOVE DCS-TIHHMM-START TO NDC-STARTTID                                
011501                              CHAR-STARTTID                               
011510     MOVE DCS-TIHHMM-READY TO NDC-SLUTTID                                 
011520                              CHAR-SLUTTID                                
013400     .                                                                    
013500     EJECT                                                                
013600 C-CHECK-STARTTID SECTION.                                                
013700                                                                          
013800     MOVE TIME-TIINLMOT     TO WORK-TIAAMMDD-FOM                          
013900     MOVE TIME-TIINLMOT     TO WORK-TIAAMMDD-TOM                          
014000     MOVE KD-DIFF-ARB-DAG   TO WORK-KDCALL                                
014100                                                                          
014200     CALL WORKDAY USING WORK-KDCALL                                       
014300                WORK-DATE-AREA WORK-KDSVAR                                
014400     IF WORK-KDSVAR-OK                                                    
014500       IF WORK-KVWORKD  > 0                                               
014600*        ARBETSDAG                                                        
014700         MOVE TIME-TIINLMTI TO TEMP-MOTTID                                
014800         CONTINUE                                                         
014900       ELSE                                                               
015000*        HELGDAG / ARBETSFRI DAG                                          
015100         MOVE TIME-TIINLMOT     TO WORK-TIAAMMDD-FOM                      
015200         MOVE 1                 TO WORK-KVWORKD                           
015300         MOVE KD-NEXT-ARB-DAG   TO WORK-KDCALL                            
015400                                                                          
015500         CALL WORKDAY USING WORK-KDCALL                                   
015600                    WORK-DATE-AREA WORK-KDSVAR                            
015700         IF WORK-KDSVAR-OK                                                
015800           MOVE WORK-TIAAMMDD-NEXT-WORKDAY     TO TIME-TIINLMOT           
015900           MOVE NDC-STARTTID  TO TEMP-MOTTID                              
016000         ELSE                                                             
016100           MOVE 'FEL I WORKDAY C1' TO FELTEXT-STR                         
016200           DISPLAY FELTEXT                                                
016300           MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                        
016400           PERFORM S99-ABEND                                              
016500         END-IF                                                           
016600       END-IF                                                             
016700     ELSE                                                                 
016800       MOVE 'FEL I WORKDAY C2' TO FELTEXT-STR                             
016900       DISPLAY FELTEXT                                                    
017000       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
017100       PERFORM S99-ABEND                                                  
017200     END-IF                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 D-CHECK-SLUTTID SECTION.                                                 
017600                                                                          
017700     MOVE TIME-TIINLINL TO WORK-TIAAMMDD-FOM                              
017800     MOVE TIME-TIINLINL TO WORK-TIAAMMDD-TOM                              
017900     MOVE KD-DIFF-ARB-DAG   TO WORK-KDCALL                                
018000                                                                          
018100     CALL WORKDAY USING WORK-KDCALL                                       
018200               WORK-DATE-AREA WORK-KDSVAR                                 
018300     IF WORK-KDSVAR-OK                                                    
018400       IF WORK-KVWORKD > 0                                                
018500*        ARBETSDAG                                                        
018600         MOVE TIME-TIINLITI  TO TEMP-INLTID                               
018700       ELSE                                                               
018800*        HELGDAG / ARBETSFRI DAG                                          
018900         MOVE TIME-TIINLINL     TO WORK-TIAAMMDD-TOM                      
019000         MOVE 1                 TO WORK-KVWORKD                           
019100         MOVE KD-LAST-ARB-DAG   TO WORK-KDCALL                            
019200                                                                          
019300         CALL WORKDAY USING WORK-KDCALL                                   
019400                    WORK-DATE-AREA WORK-KDSVAR                            
019500         IF WORK-KDSVAR-OK                                                
019600           MOVE WORK-TIAAMMDD-FOM  TO TIME-TIINLINL                       
019700           MOVE NDC-SLUTTID        TO TEMP-INLTID                         
019800         ELSE                                                             
019900           MOVE 'FEL I WORKDAY D1' TO FELTEXT-STR                         
020000           DISPLAY FELTEXT                                                
020100           MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                        
020200           PERFORM S99-ABEND                                              
020300         END-IF                                                           
020400       END-IF                                                             
020500     ELSE                                                                 
020600       MOVE 'FEL I WORKDAY D2' TO FELTEXT-STR                             
020700       DISPLAY FELTEXT                                                    
020800       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
020900       PERFORM S99-ABEND                                                  
021000     END-IF                                                               
021100                                                                          
021200     .                                                                    
021300     EJECT                                                                
021400 E-CHECK-KLOCKSLAG-START SECTION.                                         
021500                                                                          
021600     EVALUATE TRUE                                                        
021700       WHEN TEMP-MOTTID < NDC-STARTTID                                    
021800         MOVE NDC-STARTTID TO TEMP-MOTTID                                 
021900       WHEN TEMP-MOTTID > NDC-SLUTTID                                     
022000         MOVE NDC-SLUTTID TO TEMP-MOTTID                                  
022300     END-EVALUATE                                                         
022400     IF TEMP-MOTTID > 1100 AND TEMP-MOTTID <= 1142                        
022500       MOVE 1100 TO TEMP-MOTTID                                           
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 F-CHECK-KLOCKSLAG-SLUT SECTION.                                          
023000     MOVE TIME-TIINLITI  TO  TEMP-INLTID                                  
023100     EVALUATE TRUE                                                        
023200       WHEN TEMP-INLTID > NDC-SLUTTID                                     
023300         MOVE NDC-SLUTTID TO TEMP-INLTID                                  
023400       WHEN TEMP-INLTID < NDC-STARTTID                                    
023500         MOVE NDC-SLUTTID TO TEMP-INLTID                                  
023600                                                                          
023700         MOVE TIME-TIINLINL     TO WORK-TIAAMMDD-TOM                      
023800         MOVE 1                 TO WORK-KVWORKD                           
023900         MOVE KD-LAST-ARB-DAG   TO WORK-KDCALL                            
024000                                                                          
024100         CALL WORKDAY USING WORK-KDCALL                                   
024200                    WORK-DATE-AREA WORK-KDSVAR                            
024300         IF WORK-KDSVAR-OK                                                
024400           MOVE WORK-TIAAMMDD-FOM TO TIME-TIINLINL                        
024500         ELSE                                                             
024600           MOVE 'FEL I WORKDAY F' TO FELTEXT-STR                          
024700           DISPLAY FELTEXT                                                
024800           MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                        
024900           PERFORM S99-ABEND                                              
025000         END-IF                                                           
025200     END-EVALUATE                                                         
025300     IF TEMP-INLTID > 1100 AND TEMP-INLTID <= 1142                        
025400       MOVE 1100 TO TEMP-INLTID                                           
025500     END-IF                                                               
025600                                                                          
025700     .                                                                    
025800     EJECT                                                                
025900 G-KONVERTERA-TID SECTION.                                                
026000                                                                          
026100     MOVE CHAR-STARTTID  TO CHAR-TID                                      
026200     PERFORM S11-KONVERTERA-TILL-MIN                                      
026300     MOVE KONV-TID       TO KONV-NDC-STARTTID                             
026400                                                                          
026500     MOVE CHAR-SLUTTID   TO CHAR-TID                                      
026600     PERFORM S11-KONVERTERA-TILL-MIN                                      
026700     SUBTRACT 42 FROM KONV-TID                                            
026800     MOVE KONV-TID       TO KONV-NDC-SLUTTID                              
026900                                                                          
027000     MOVE TIME-TIINLMTI  TO CHAR-TID                                      
027100     INSPECT CHAR-TID REPLACING ALL SPACE BY ZERO                         
027200     PERFORM S11-KONVERTERA-TILL-MIN                                      
027300     IF KONV-TID > 660                                                    
027400       SUBTRACT 42 FROM KONV-TID                                          
027500     END-IF                                                               
027600     MOVE KONV-TID       TO KONV-TIINLMTI                                 
027700                                                                          
027800     MOVE TIME-TIINLITI  TO CHAR-TID                                      
027900     INSPECT CHAR-TID REPLACING ALL SPACE BY ZERO                         
028000     PERFORM S11-KONVERTERA-TILL-MIN                                      
028100     IF KONV-TID > 660                                                    
028200       SUBTRACT 42 FROM KONV-TID                                          
028300     END-IF                                                               
028400     MOVE KONV-TID       TO KONV-TIINLITI                                 
028500                                                                          
028600     .                                                                    
028700     EJECT                                                                
028800 H-BERAK-SKILLNAD-I-TID SECTION.                                          
028900                                                                          
029000     MOVE TIME-TIINLMOT TO WORK-TIAAMMDD-FOM                              
029100     MOVE TIME-TIINLINL TO WORK-TIAAMMDD-TOM                              
029200     MOVE KD-DIFF-ARB-DAG   TO WORK-KDCALL                                
029210* Y2K ÄNDRING                                                             
029220     MOVE WORK-TIAAMMDD-FOM   TO TMP1-YYMMDD                              
029230     MOVE WORK-TIAAMMDD-TOM   TO TMP2-YYMMDD                              
029240     PERFORM WY2000P1                                                     
029250     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
029311*      BÅDE MOTTAGEN OCH INLAGD PÅ SAMMA HELGDAG !                        
029320       MOVE ZERO TO TIDS-DIFF-DAYS                                        
029330     ELSE                                                                 
029400       CALL WORKDAY USING WORK-KDCALL                                     
029500                  WORK-DATE-AREA WORK-KDSVAR                              
029600       IF WORK-KDSVAR-OK                                                  
029700         COMPUTE TIDS-DIFF-DAYS = WORK-KVWORKD - 1                        
029800       ELSE                                                               
029900         MOVE 'FEL I WORKDAY H' TO FELTEXT-STR                            
030000         DISPLAY FELTEXT                                                  
030100         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                          
030200         PERFORM S99-ABEND                                                
030300       END-IF                                                             
030310     END-IF                                                               
030400                                                                          
030500     EVALUATE TRUE                                                        
030600     WHEN KONV-TIINLMTI < KONV-TIINLITI                                   
030700       COMPUTE TIDS-DIFF-MIN = KONV-TIINLITI - KONV-TIINLMTI              
030800       COMPUTE TIME-KVDAGDEC ROUNDED = TIDS-DIFF-DAYS +                   
030900          (TIDS-DIFF-MIN / (KONV-NDC-SLUTTID - KONV-NDC-STARTTID))        
030901       IF TIME-KVDAGDEC = 0.0                                             
030903       COMPUTE TIME-KVDAGDEC-W ROUNDED = TIDS-DIFF-DAYS +                 
030904          (TIDS-DIFF-MIN / (KONV-NDC-SLUTTID - KONV-NDC-STARTTID))        
030905          IF TIME-KVDAGDEC-W > 0.000                                      
030906             MOVE 0.1 TO TIME-KVDAGDEC                                    
030908          END-IF                                                          
030909       END-IF                                                             
030910                                                                          
031000     WHEN KONV-TIINLMTI > KONV-TIINLITI                                   
031100       SUBTRACT 1 FROM TIDS-DIFF-DAYS                                     
031200       COMPUTE TIDS-DIFF-MIN = (KONV-NDC-SLUTTID - KONV-TIINLMTI)         
031300                          + (KONV-TIINLITI - KONV-NDC-STARTTID)           
031400       COMPUTE TIME-KVDAGDEC ROUNDED = TIDS-DIFF-DAYS +                   
031500          (TIDS-DIFF-MIN / (KONV-NDC-SLUTTID - KONV-NDC-STARTTID))        
031600     WHEN KONV-TIINLMTI = KONV-TIINLITI                                   
031700       MOVE TIDS-DIFF-DAYS TO TIME-KVDAGDEC                               
031800     END-EVALUATE                                                         
031900     .                                                                    
032000     EJECT                                                                
032100 S11-KONVERTERA-TILL-MIN SECTION.                                         
032200                                                                          
032300     MOVE ZERO TO KONV-TID                                                
032400     COMPUTE KONV-TID = FUNCTION NUMVAL(CHAR-TID(1:1)) * 600              
032500     COMPUTE KONV-TID =                                                   
032600             (FUNCTION NUMVAL(CHAR-TID(2:1)) * 60) + KONV-TID             
032700     COMPUTE KONV-TID =                                                   
032800             (FUNCTION NUMVAL(CHAR-TID(3:1)) * 10) + KONV-TID             
032900     COMPUTE KONV-TID =                                                   
033000             (FUNCTION NUMVAL(CHAR-TID(4:1)) * 1)  + KONV-TID             
033100     MOVE SPACE TO CHAR-TID                                               
033200     .                                                                    
033300     EJECT                                                                
033400 S99-ABEND SECTION.                                                       
033500                                                                          
033600     CALL ABEND USING RKOD-ABEND                                          
033700     .                                                                    
033800     EJECT                                                                
033900*    -COPY WY2000P1                                                       
034000 IMS-GU-WDB601   SECTION.                                                 
034100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
034200          DELIMITED BY SIZE INTO SSA1                                     
034300     MOVE '  GE' TO GODK-STATUSKODER                                      
034400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
034500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
034600     PERFORM IMS-STATUSKONTROLL                                           
034700     IF SEGMENT-SAKNAS                                                    
034800        MOVE SPACE TO DCS-KDDC                                            
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 IMS-STATUSKONTROLL SECTION.                                              
035300     SKIP2                                                                
035400     SET STATUS-IX TO 1                                                   
035500     SEARCH GODK-STATUS                                                   
035600       AT END                                                             
035700         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
035800         DISPLAY FELTEXT                                                  
035900         CALL FELLOG                                                      
036000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036100         CONTINUE                                                         
036200     END-SEARCH                                                           
036300     .                                                                    
