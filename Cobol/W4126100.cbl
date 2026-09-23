000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4126100.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   91/05/30.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN BMP SOM SKALL KÖRAS VARJE NATT                  
001100*        OCH HAR FÖLJANDE FUNKTIONER.                                     
001200*        1. PROGRAMMET RENSAR FÖRFALLNA/BORTTAGSMARKERADE                 
001300*        PROFORMOR PÅ WDQ1, WDE8 OCH WDE9.                                
001400*        2. PROGRAMMET SKAPAR EN FIL W41261001 SOM INNEHÅLLER             
001500*        PROFORMOR SOM FÖRFALLER INOM FYRA VECKOR.                        
001600*        FILEN SKALL SKRIVAS UT VARJE MÅNDAG AV E+ - PGM W4126200         
001700*                                                                         
001800*        PROGRAMMET LÄSER/DLET  WLPROC (WDE8)                             
001900*        PROGRAMMET LÄSER/DLET  WLPROD (WDE9)                             
002000*        PROGRAMMET UPPATERAR   WLARTM (WDK9)                             
002100*        PROGRAMMET LÄSER/DLET  WLORQM (WDQ1)                             
002200*        PROGRAMMET LÄSER       WLORQI (WDQ2)                             
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U1000 -  MED-DUMP                                                
002510*        U0016 -  UTAN-DUMP                                               
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*--   INFIL STARTFIL VID ABEND                                            
003600     SELECT W41261-I                   ASSIGN TO W41261D1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W41261-I                                                             
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600 01  FILLER               PIC X(998).                                     
004610 01  POST -COPY W41261   -PRE  IN- -L.                                    
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
004901*    -COPY WY2000W1                                                       
004902     SKIP3                                                                
005000 77  IDPGM                       PIC X(8)    VALUE 'W4126100'.            
005100 77  INDX                        PIC S9(3)   VALUE +0 COMP-3.             
005200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005210 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005300 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
005400 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005500 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005600 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005700 01  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005800 01  CHKP-MAX                    PIC S9(3)   VALUE +98.                   
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  END-OF-W41261-I             PIC X(1)    VALUE 'N'.                   
006110 77  MAX-GSAM                    PIC S9(4)   COMP.                        
006200     SKIP2                                                                
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600                                                                          
006700 01  POST-SW                     PIC X(1)    VALUE 'J'.                   
006800     88  POSTER-FINNS                        VALUE 'J'.                   
006900                                                                          
007000 01  ORDER-SW                    PIC X(1)    VALUE 'N'.                   
007100     88  ORDER-FINNS                         VALUE 'J'.                   
007200                                                                          
007300 77  W-W41261-KVPOST-IN          PIC S9(7)   VALUE +0 COMP-3.             
007400 77  W-W41261-KVPOST-UT          PIC S9(7)   VALUE +0 COMP-3.             
007500     EJECT                                                                
007600                                                                          
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     SKIP2                                                                
008300                                                                          
008400 01  DAGENS-DATUM-PLUS-4-VECKOR  PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES DAGENS-DATUM-PLUS-4-VECKOR.                         
008600     03  MAX-FORF-DATUM-AAR      PIC 9(2).                                
008700     03  MAX-FORF-DATUM-MAANAD   PIC 9(2).                                
008800     03  MAX-FORF-DATUM-DAG      PIC 9(2).                                
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009500     EJECT                                                                
009600                                                                          
009601 01  GSAM-POST.                                                           
009610     03  GSAM-LRECL      PIC S9(4) COMP.                                  
009700*    03  POST -COPY W41261   -PRE  WSUT-  -L.                             
009800     EJECT                                                                
009900*--- PARAMETRAR TILL POSTSUM                                              
010000     SKIP2                                                                
010100*01  -COPY W0005        -PRE POSTSUM-                                     
010200     EJECT                                                                
010300                                                                          
010400 01  GSAM-SKRIV-AREA     PIC X(24)   VALUE   'GSAM-SKRIV-AREA'.           
010500                                                                          
010600*01  AREA -COPY W41261  -PRE GSAM-                                        
010700*                                                                         
010800     EJECT                                                                
010900 01  RESTRT-AREA-START   PIC X(24)   VALUE 'RESTRT-AREA-START'.           
011000                                                                          
011100*01  AREA -COPY W41261       -PRE RESTRT-                                 
011200*                                                                         
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700     03  W-WDE801KY-X.                                                    
011800         05  W-IDDISTR-WDE8      PIC S9(5)   VALUE ZERO COMP-3.           
011900         05  W-IDKUNDNR-WDE8     PIC S9(7)   VALUE ZERO COMP-3.           
012000         05  W-IDKUNDRF-WDE8     PIC  X(10)  VALUE SPACE.                 
012100                                                                          
012200     03  W-WDE901KY-MIN-X.                                                
012300         05  W-IDORDER-MIN       PIC S9(7)   VALUE ZERO COMP-3.           
012500         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
012600         05  W-IDLOPNR-MIN       PIC S9(3)   VALUE ZERO COMP-3.           
012700                                                                          
012800     03  W-WDE901KY-MAX-X.                                                
012900         05  W-IDORDER-MAX       PIC S9(7)   VALUE ZERO COMP-3.           
013100         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
013200         05  W-IDLOPNR-MAX       PIC S9(3)   VALUE ZERO COMP-3.           
013300                                                                          
013400                                                                          
013500     03  W-WDQ101KY-MIN-X.                                                
013600         05  W-IDORDER-MIN-Q1    PIC S9(7)   VALUE ZERO COMP-3.           
013610         05  FILLER              PIC X(13)   VALUE LOW-VALUE.             
014200                                                                          
014300     03  W-WDQ101KY-MAX-X.                                                
014400         05  W-IDORDER-MAX-Q1    PIC S9(7)   VALUE ZERO COMP-3.           
014410         05  FILLER              PIC X(13)   VALUE HIGH-VALUE.            
015000                                                                          
015100     03  W-IDARTNR-X.                                                     
015200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015300                                                                          
015400     03  W-IDGMTREF-X.                                                    
015500         05  W-IDDISTR-WDQ2C     PIC S9(5)   VALUE ZERO COMP-3.           
015600         05  W-IDKUNDNR-WDQ2C    PIC S9(7)   VALUE ZERO COMP-3.           
015700         05  W-IDKUNDRF-WDQ2C    PIC  X(10)  VALUE SPACE.                 
015800                                                                          
015900     SKIP2                                                                
016000                                                                          
016100     03  W-IDHTYP-X.                                                      
016200         05  W-IDHTYP            PIC  X(4)   VALUE SPACE.                 
016300         05  NYCKEL-VALFRI       PIC  X(26).                              
016400                                                                          
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017100     88  IMS-EJ-OK                           VALUE 'XD'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(80).                               
017700 01  SSA2                        PIC X(64).                               
017800     EJECT                                                                
017900*    --- IMS FUNKTIONSKODER                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300 01  FILLER                    PIC X(16)   VALUE 'PROC-IO-AREA'.          
018400 01  PROC-IO-AREA.                                                        
018800*    03  -COPY WDE801                                                     
018900     EJECT                                                                
019000                                                                          
019100 01  FILLER                    PIC X(16)   VALUE 'PROD-IO-AREA'.          
019200 01  PROD-IO-AREA.                                                        
019600*    03  -COPY WDE901                                                     
019610     EJECT                                                                
019800                                                                          
019900 01  FILLER                    PIC X(16)   VALUE 'ORQM-IO-AREA'.          
020000 01  ORQM-IO-AREA.                                                        
020400*    03  -COPY WDQ101                                                     
020500     EJECT                                                                
020600                                                                          
020700 01  FILLER                    PIC X(16)   VALUE 'ARTM-IO-AREA'.          
020900 01  ARTM-IO-AREA.                                                        
021300*    03  -COPY WDK901                                                     
021310     EJECT                                                                
021320 01  FILLER                    PIC X(16)   VALUE 'ORQI-IO-AREA'.          
021330 01  ORQI-IO-AREA.                                                        
021600*    03  -COPY WDQ201                                                     
021610     EJECT                                                                
021700*    ---  DLI INPUT-OUTPUT AREA                                           
021800 01  FILLER                 PIC X(16)   VALUE 'XXLQ-IO-AREA'.             
022000 01  XXLQ-IO-AREA.                                                        
022300*    03  -COPY WDGX4562                                                   
022400     EJECT                                                                
022500 LINKAGE SECTION.                                                         
022600                                                                          
022700*01  -COPY W0009   -PRE MSG-                                              
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE PROC-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE PROD-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE ARTM-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE ORQI-                                              
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100*01  -COPY W0008  -PRE XXLQ-                                              
024200         05  FILLER              PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008  -PRE ORQM-                                              
024500         05  FILLER              PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE GSAM-                                              
024800         05  FILLER              PIC X.                                   
024900     EJECT                                                                
025000 PROCEDURE DIVISION  USING MSG-PCB PROC-PCB PROD-PCB                      
025100     ARTM-PCB ORQI-PCB XXLQ-PCB ORQM-PCB GSAM-PCB.                        
025200                                                                          
025300     ENTRY 'DLITCBL' USING MSG-PCB PROC-PCB PROD-PCB                      
025400     ARTM-PCB ORQI-PCB XXLQ-PCB ORQM-PCB GSAM-PCB.                        
025500                                                                          
025610     PERFORM A-INIT                                                       
025700     IF POSTER-FINNS                                                      
025800       MOVE DAGENS-DATUM     TO DAGENS-DATUM-PLUS-4-VECKOR                
025900       ADD 1                 TO MAX-FORF-DATUM-MAANAD                     
025920       IF MAX-FORF-DATUM-MAANAD > 12                                      
026100          SUBTRACT 12        FROM MAX-FORF-DATUM-MAANAD                   
026200          ADD 1              TO MAX-FORF-DATUM-AAR                        
026300       END-IF                                                             
026400       PERFORM UNTIL SEGMENT-SLUT                                         
026401         MOVE PHUV-TIFORDAT   TO TMP1-YYMMDD                              
026402         MOVE DAGENS-DATUM    TO TMP2-YYMMDD                              
026410         PERFORM WY2000P1                                                 
026500         IF (PHUV-FLBORT = JA)         OR                                 
026600           (TMP1-YYMMDD   < TMP2-YYMMDD  AND                              
026700            PHUV-TIORDDAT = ZERO)                                         
026800            PERFORM C-TA-BORT-EJ-RELEASADE-PROF                           
026900         ELSE                                                             
027000            IF TMP1-YYMMDD   < TMP2-YYMMDD  AND                           
027100               PHUV-TIORDDAT > ZERO                                       
027200               PERFORM D-TA-BORT-RELEASADE-PROF                           
027300            ELSE                                                          
027400               IF TMP1-YYMMDD   > TMP2-YYMMDD                             
027410                  MOVE PHUV-TIFORDAT              TO TMP1-YYMMDD          
027420                  MOVE DAGENS-DATUM-PLUS-4-VECKOR TO TMP2-YYMMDD          
027430                  PERFORM WY2000P1                                        
027500                  IF TMP1-YYMMDD   < TMP2-YYMMDD AND                      
027600                     PHUV-TIORDDAT = ZERO        AND                      
027700                     PHUV-KDPROTYP NOT = 'F'                              
027810                     PERFORM E-SKRIV-UT-SIGNALPOST                        
027900                  END-IF                                                  
027910               END-IF                                                     
028000            END-IF                                                        
028100         END-IF                                                           
028200         PERFORM IMS-GHN-PROC-WDE8-OKVAL                                  
028300       END-PERFORM                                                        
028410     END-IF                                                               
028500                                                                          
028610     IF W-W41261-KVPOST-UT = +0                                           
028611        PERFORM F-SKRIV-UT-DUMMYPOST                                      
028630     END-IF                                                               
028631                                                                          
028640     PERFORM Z-FINIT                                                      
028700                                                                          
028800     MOVE ZERO TO RETURN-CODE                                             
028900     GOBACK                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 A-INIT SECTION.                                                          
029300     MOVE 'A-INIT SECTION'       TO FELTEXT-STR                           
029400     SKIP2                                                                
029500                                                                          
029910     ACCEPT DAGENS-DATUM       FROM DATE                                  
030000                                                                          
030100     MOVE +108      TO MAX-GSAM                                           
030101***** OBS  GSAM-LÄNGDEN SKALL VARA POSTLÄNGDEN PLUS 2 BYTES (VB)**        
030102                                                                          
030110     MOVE '4561'    TO W-IDHTYP                                           
030200     MOVE LOW-VALUE TO NYCKEL-VALFRI                                      
030300                       W-WDE801KY-X                                       
030400                       W-WDQ101KY-MIN-X                                   
030500                       W-WDE901KY-MIN-X                                   
030600                       W-IDARTNR-X                                        
030700                       W-IDGMTREF-X                                       
030800     MOVE HIGH-VALUE TO W-WDE901KY-MAX-X                                  
030900                        W-WDQ101KY-MAX-X                                  
031000     PERFORM IMS-LAES-ATERSTART-4561                                      
031100                                                                          
031200     IF SEGMENT-FINNS                                                     
031300        IF 4562-KVPOST > ZERO                                             
031310          PERFORM IMS-RESTART                                             
031320          PERFORM IMS-OPEN-GSAM-W41261                                    
031400          PERFORM AA-LAES-FRAM-FILER                                      
031500          PERFORM AB-INIT-LAES-BAS                                        
031600        ELSE                                                              
031700          PERFORM IMS-GET-PROC-WDE8-OKVAL                                 
031800          IF SEGMENT-FINNS                                                
031900             CONTINUE                                                     
032000          ELSE                                                            
032100             MOVE NEJ TO POST-SW                                          
032200             MOVE 'POSTER SAKNAS PÅ WDE8' TO FELTEXT-STR                  
032210             DISPLAY 'POSTER SAKNAS PÅ WDE8'                              
032300          END-IF                                                          
032400        END-IF                                                            
032500     ELSE                                                                 
032600       PERFORM IMS-GET-PROC-WDE8-OKVAL                                    
032700       IF SEGMENT-FINNS                                                   
032800          CONTINUE                                                        
032900       ELSE                                                               
033000          MOVE NEJ TO POST-SW                                             
033100          MOVE 'POSTER SAKNAS PÅ WDE8' TO FELTEXT-STR                     
033110          DISPLAY 'POSTER SAKNAS PÅ WDE8'                                 
033200       END-IF                                                             
033300     END-IF                                                               
033301                                                                          
033330     PERFORM IMS-RESTART                                                  
033340     PERFORM IMS-OPEN-GSAM-W41261                                         
033400     .                                                                    
033500     EJECT                                                                
033600 AA-LAES-FRAM-FILER  SECTION.                                             
033700     MOVE 'AA-LAES-FRAM-FILER SECTION' TO FELTEXT-STR                     
033800                                                                          
033900     MOVE NEJ TO END-OF-W41261-I                                          
034000     MOVE +0 TO W-W41261-KVPOST-UT                                        
034100     OPEN INPUT W41261-I                                                  
034200     PERFORM S01-LAES-W41261-I                                            
034300     MOVE 4562-KVPOST TO W-W41261-KVPOST-IN                               
034400                                                                          
034500     PERFORM UNTIL W-W41261-KVPOST-UT = W-W41261-KVPOST-IN OR             
034600             END-OF-W41261-I = JA                                         
034610       MOVE MAX-GSAM    TO GSAM-LRECL                                     
034700       MOVE RESTRT-AREA TO WSUT-POST                                      
034800       PERFORM IMS-ISRT-GSAM-W41261                                       
034900       PERFORM S01-LAES-W41261-I                                          
035000     END-PERFORM                                                          
035100                                                                          
035200     CLOSE W41261-I                                                       
035300                                                                          
035400     IF W-W41261-KVPOST-UT NOT = W-W41261-KVPOST-IN                       
035500     DISPLAY 'FEL VID ÅTERSTART. POSTER SAKNAS PÅ IN-FIL '                
035600       CALL FELLOG                                                        
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 AB-INIT-LAES-BAS SECTION.                                                
036100     MOVE 'AB-INIT-LAES-BAS SECTION.' TO FELTEXT-STR                      
036200                                                                          
036300     MOVE 4562-IDDISTR              TO W-IDDISTR-WDE8                     
036400     MOVE 4562-IDKUNDNR             TO W-IDKUNDNR-WDE8                    
036500     MOVE 4562-IDKUNDRF             TO W-IDKUNDRF-WDE8                    
036600                                                                          
036700     MOVE 4562-IDORDER              TO W-IDORDER-MIN                      
036900     MOVE 4562-IDARTNR              TO W-IDARTNR-MIN                      
037000     MOVE 4562-IDLOPNR              TO W-IDLOPNR-MIN                      
037100                                                                          
037200     PERFORM IMS-GET-PROC-WDE8                                            
037300     PERFORM IMS-GET-PROD-WDE9                                            
037400                                                                          
037500     MOVE LOW-VALUE                 TO W-WDE901KY-MIN-X                   
037600     MOVE 4562-IDORDER              TO W-IDORDER-MIN                      
037700                                                                          
037800     .                                                                    
037900     EJECT                                                                
038000 C-TA-BORT-EJ-RELEASADE-PROF SECTION.                                     
038100     MOVE ' C-TA-BORT-EJ-RELEASADE-PROF' TO FELTEXT-STR                   
038200                                                                          
038300     MOVE PHUV-IDORDER  TO W-IDORDER-MIN                                  
038400                           W-IDORDER-MAX                                  
038500                           W-IDORDER-MIN-Q1                               
038600                           W-IDORDER-MAX-Q1                               
038700     PERFORM IMS-GET-PROD-WDE9                                            
038800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
038900        IF PHUV-KDPROTYP NOT = 'F'                                        
039000           PERFORM CA-UPPDATERA-WDK9-SALDO                                
039100        END-IF                                                            
039200        IF CHKP-ANT > CHKP-MAX                                            
039300          MOVE ZERO TO OBKR-IDORDER                                       
039400          PERFORM X-TAG-CHECKPOINT                                        
039500          PERFORM S02-POSITIONERA-BASERNA                                 
039600          MOVE +0 TO CHKP-ANT                                             
039700        END-IF                                                            
039800        PERFORM IMS-DLET-PROD-WDE9                                        
039900        ADD +1  TO CHKP-ANT                                               
040000        PERFORM IMS-GHN-PROD-WDE9                                         
040100     END-PERFORM                                                          
040200     PERFORM IMS-GHU-ORQM-WDQ1                                            
040300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
040400        IF PHUV-KDPROTYP NOT = 'F'                                        
040500           PERFORM CB-UPPDATERA-WDK9-SALDO                                
040600        END-IF                                                            
040700        IF CHKP-ANT > CHKP-MAX                                            
040800          MOVE ZERO TO PRAD-IDORDER                                       
040900          PERFORM X-TAG-CHECKPOINT                                        
041000          PERFORM S02-POSITIONERA-BASERNA                                 
041100          MOVE +0 TO CHKP-ANT                                             
041200        END-IF                                                            
041300        PERFORM IMS-DLET-ORQM-WDQ1                                        
041400        ADD +1  TO CHKP-ANT                                               
041500        PERFORM IMS-GHN-ORQM-WDQ1                                         
041600     END-PERFORM                                                          
041700     DISPLAY 'DIST ' PHUV-IDDISTR ' KUND ' PHUV-IDKUNDNR ' ONR '          
041800       PHUV-IDKUNDRF ' FLB ' PHUV-FLBORT ' RELDAT ' PHUV-TIORDDAT         
041900       ' TYP ' PHUV-KDPROTYP ' FÖRFDAT ' PHUV-TIFORDAT                    
042000     PERFORM IMS-DLET-PROC-WDE8                                           
042100                                                                          
042200     .                                                                    
042300     EJECT                                                                
042400 CA-UPPDATERA-WDK9-SALDO SECTION.                                         
042500      MOVE ' CA-UPPDATERA-WDK9-SALDO SECTION.' TO FELTEXT-STR             
042600                                                                          
042700     MOVE PRAD-IDARTNR TO W-IDARTNR                                       
042800     PERFORM IMS-GHU-ARTM-WDK9                                            
042900     IF SEGMENT-FINNS                                                     
043000        COMPUTE ART-KVOFFERT =                                            
043100                ART-KVOFFERT - PRAD-KVBEART-Q                             
043200        PERFORM IMS-REPL-ARTM-WDK9                                        
043300        ADD +1  TO CHKP-ANT                                               
043400     ELSE                                                                 
043500*       ARTIKELN SAKNAS PÅ ARTIKELREGISTER WDK9                           
043600        MOVE 'ARTIKELN SAKNAS PÅ ARTIKELREGISTRET WDK9'                   
043700                                  TO FELTEXT                              
043800        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 CB-UPPDATERA-WDK9-SALDO SECTION.                                         
044300      MOVE ' CB-UPPDATERA-WDK9-SALDO SECTION.' TO FELTEXT-STR             
044400                                                                          
044500     IF OBKR-FLOBOK = NEJ AND OBKR-IDSYSTEM = 'PROF' AND                  
044600        OBKR-KVPREAVB > 0                                                 
044700        IF OBKR-IDARTNR-TILLK > 0                                         
044800           MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                           
044900        ELSE                                                              
045000           MOVE OBKR-IDARTNR       TO W-IDARTNR                           
045100        END-IF                                                            
045200        PERFORM IMS-GHU-ARTM-WDK9                                         
045300        IF SEGMENT-FINNS                                                  
045400           COMPUTE ART-KVOFFERT =                                         
045500                   ART-KVOFFERT - OBKR-KVPREAVB                           
045600           PERFORM IMS-REPL-ARTM-WDK9                                     
045700           ADD +1  TO CHKP-ANT                                            
045800        ELSE                                                              
045900*    ARTIKELN SAKNAS PÅ ARTIKELREGISTER WDK9                              
046000           MOVE 'ARTIKELN SAKNAS PÅ ARTIKELREGISTRET WDK9'                
046100                                     TO FELTEXT                           
046200           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
046300        END-IF                                                            
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 D-TA-BORT-RELEASADE-PROF SECTION.                                        
046800     MOVE ' D-TA-BORT-RELEASADE-PROF' TO FELTEXT-STR                      
046900                                                                          
047000     MOVE PHUV-IDDISTR                TO W-IDDISTR-WDQ2C                  
047100     MOVE PHUV-IDKUNDNR               TO W-IDKUNDNR-WDQ2C                 
047200     MOVE +1                          TO INDX                             
047300     MOVE NEJ                         TO ORDER-SW                         
047400     PERFORM UNTIL INDX > +10                             OR              
047500                   PHUV-IDKUNDRF-ING(INDX )= '0000000   ' OR              
047600                   ORDER-FINNS                                            
047700        MOVE PHUV-IDKUNDRF-ING(INDX) TO W-IDKUNDRF-WDQ2C                  
047800        PERFORM IMS-GET-ORQI-WDQ2                                         
047900        IF SEGMENT-FINNS                                                  
048000           MOVE JA TO ORDER-SW                                            
048100        END-IF                                                            
048200        ADD +1                        TO INDX                             
048300     END-PERFORM                                                          
048400                                                                          
048500     IF ORDER-FINNS                                                       
048600        CONTINUE                                                          
048700     ELSE                                                                 
048800        MOVE PHUV-IDORDER TO W-IDORDER-MIN                                
048900                             W-IDORDER-MAX                                
049000                             W-IDORDER-MIN-Q1                             
049100                             W-IDORDER-MAX-Q1                             
049200        PERFORM IMS-GET-PROD-WDE9                                         
049300        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
049400           IF CHKP-ANT > CHKP-MAX                                         
049500             MOVE ZERO TO OBKR-IDORDER                                    
049600             PERFORM X-TAG-CHECKPOINT                                     
049700             PERFORM S02-POSITIONERA-BASERNA                              
049800             MOVE +0 TO CHKP-ANT                                          
049900           END-IF                                                         
050000           PERFORM IMS-DLET-PROD-WDE9                                     
050100           ADD +1 TO CHKP-ANT                                             
050200           PERFORM IMS-GHN-PROD-WDE9                                      
050300        END-PERFORM                                                       
050400        PERFORM IMS-GHU-ORQM-WDQ1                                         
050500        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
050600           IF CHKP-ANT > CHKP-MAX                                         
050700             MOVE ZERO TO PRAD-IDORDER                                    
050800             PERFORM X-TAG-CHECKPOINT                                     
050900             PERFORM S02-POSITIONERA-BASERNA                              
051000             MOVE +0 TO CHKP-ANT                                          
051100           END-IF                                                         
051200           PERFORM IMS-DLET-ORQM-WDQ1                                     
051300           ADD +1  TO CHKP-ANT                                            
051400           PERFORM IMS-GHN-ORQM-WDQ1                                      
051500        END-PERFORM                                                       
051600     DISPLAY 'DIST ' PHUV-IDDISTR ' KUND ' PHUV-IDKUNDNR ' ONR '          
051700       PHUV-IDKUNDRF ' FLB ' PHUV-FLBORT ' RELDAT ' PHUV-TIORDDAT         
051800       ' TYP ' PHUV-KDPROTYP ' FÖRFDAT ' PHUV-TIFORDAT                    
051900        PERFORM IMS-DLET-PROC-WDE8                                        
052000     END-IF                                                               
052100                                                                          
052200     .                                                                    
052300     EJECT                                                                
052400 E-SKRIV-UT-SIGNALPOST SECTION.                                           
052500     MOVE ' E-SKRIV-UT-SIGNALPOST' TO FELTEXT-STR                         
052600                                                                          
052700     MOVE PHUV-IDDISTR               TO GSAM-IDDISTR                      
052800     MOVE PHUV-IDKUNDNR              TO GSAM-IDKUNDNR                     
052900     MOVE PHUV-IDKUNDRF              TO GSAM-IDKUNDRF                     
053100     MOVE PHUV-BEBETRAD-1            TO GSAM-BEBETRAD-1                   
053200     MOVE PHUV-BEBETRAD-2            TO GSAM-BEBETRAD-2                   
053300     MOVE PHUV-IDUSER                TO GSAM-IDUSER                       
053400     MOVE PHUV-TIFORDAT              TO GSAM-TIFORDAT                     
053500     MOVE PHUV-SUORDV                TO GSAM-SUORDV                       
053600                                                                          
053610     MOVE MAX-GSAM                   TO GSAM-LRECL                        
053700     MOVE GSAM-AREA                  TO WSUT-POST                         
053800     PERFORM IMS-ISRT-GSAM-W41261                                         
053900     ADD +1 TO W-W41261-KVPOST-UT                                         
054000                                                                          
054100     IF CHKP-ANT > CHKP-MAX                                               
054200       MOVE +0 TO PRAD-IDORDER                                            
054300                  OBKR-IDORDER                                            
054400       PERFORM X-TAG-CHECKPOINT                                           
054500       PERFORM S02-POSITIONERA-BASERNA                                    
054600       MOVE +0 TO CHKP-ANT                                                
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
054910 F-SKRIV-UT-DUMMYPOST SECTION.                                            
054920     MOVE ' F-SKRIV-UT-DUMMYPOST'    TO FELTEXT-STR                       
054930                                                                          
054940     MOVE 99999                      TO GSAM-IDDISTR                      
054950     MOVE 9999999                    TO GSAM-IDKUNDNR                     
054960     MOVE 'XXXXXXXXXX'               TO GSAM-IDKUNDRF                     
054970     MOVE 'INGA PROFORMOR UTSKRIVNA' TO GSAM-BEBETRAD-1                   
054980     MOVE SPACE                      TO GSAM-BEBETRAD-1                   
054990     MOVE 'XXXXXXXX'                 TO GSAM-IDUSER                       
054991     MOVE 0                          TO GSAM-TIFORDAT                     
054992                                        GSAM-SUORDV                       
054993                                                                          
054994     MOVE MAX-GSAM                   TO GSAM-LRECL                        
054995     MOVE GSAM-AREA                  TO WSUT-POST                         
054996     PERFORM IMS-ISRT-GSAM-W41261                                         
054997     ADD +1 TO W-W41261-KVPOST-UT                                         
054998                                                                          
054999     IF CHKP-ANT > CHKP-MAX                                               
055000       MOVE +0 TO PRAD-IDORDER                                            
055001                  OBKR-IDORDER                                            
055002       PERFORM X-TAG-CHECKPOINT                                           
055003       PERFORM S02-POSITIONERA-BASERNA                                    
055004       MOVE +0 TO CHKP-ANT                                                
055005     END-IF                                                               
055006     .                                                                    
055007     EJECT                                                                
055010 Z-FINIT SECTION.                                                         
055100     MOVE 'Z-FINIT SECTION.'          TO FELTEXT-STR                      
055200                                                                          
055310     MOVE '4561' TO W-IDHTYP                                              
055400     MOVE LOW-VALUE TO NYCKEL-VALFRI                                      
055500     PERFORM IMS-LAES-ATERSTART-4561                                      
055600                                                                          
055700     IF SEGMENT-FINNS                                                     
055800        PERFORM IMS-DLET-ATERSTART-4562                                   
055900     END-IF                                                               
056000                                                                          
056100     PERFORM IMS-CLOSE-GSAM-W41261                                        
056200     .                                                                    
056300     EJECT                                                                
056400 S01-LAES-W41261-I  SECTION.                                              
056500     MOVE ' S01-LAES-W41261-I SECTION.' TO FELTEXT-STR                    
056600     SKIP2                                                                
056700     READ W41261-I INTO RESTRT-AREA                                       
056800     AT END                                                               
056900        MOVE HIGH-VALUE TO RESTRT-AREA                                    
057000        MOVE JA         TO END-OF-W41261-I                                
057100                                                                          
057200     NOT AT END                                                           
057300        MOVE 'W41261-I' TO POSTSUM-FDNAMN                                 
057400        MOVE 'W41261D1' TO POSTSUM-DDNAMN2                                
057500        MOVE 'XRST'     TO POSTSUM-TRANSTYP                               
057600        CALL POSTSUM USING POSTSUM-PARM                                   
057700                                                                          
057800        ADD +1 TO W-W41261-KVPOST-UT                                      
057900     END-READ                                                             
058000     .                                                                    
058100     EJECT                                                                
058200 S02-POSITIONERA-BASERNA SECTION.                                         
058300     MOVE ' S02-POSITIONERA-BASERNA ' TO FELTEXT-STR                      
058400                                                                          
058500     MOVE 4562-IDDISTR                TO W-IDDISTR-WDE8                   
058600     MOVE 4562-IDKUNDNR               TO W-IDKUNDNR-WDE8                  
058700     MOVE 4562-IDKUNDRF               TO W-IDKUNDRF-WDE8                  
058800                                                                          
058900     IF 4562-IDORDER > +0                                                 
059000        MOVE 4562-IDORDER             TO W-IDORDER-MIN                    
059200        MOVE 4562-IDARTNR             TO W-IDARTNR-MIN                    
059300        MOVE 4562-IDLOPNR             TO W-IDLOPNR-MIN                    
059400     END-IF                                                               
059500     MOVE 4562-KVPOST                 TO W-W41261-KVPOST-UT               
059600                                                                          
059700     PERFORM IMS-GET-PROC-WDE8                                            
059800     IF SEGMENT-FINNS                                                     
059900        IF 4562-IDORDER > +0                                              
060000           PERFORM IMS-GET-PROD-WDE9                                      
060100           IF SEGMENT-FINNS                                               
060200              CONTINUE                                                    
060300           ELSE                                                           
060400              PERFORM IMS-GHU-ORQM-WDQ1                                   
060500              IF SEGMENT-FINNS                                            
060600                 CONTINUE                                                 
060700              ELSE                                                        
060800                 MOVE 'FEL I POSITIONERINGEN EFTER CHECKP WDE9' TO        
060900                       FELTEXT-STR                                        
061000                 CALL ABEND USING RKOD-ABEND-MED-DUMP                     
061100              END-IF                                                      
061200           END-IF                                                         
061300        END-IF                                                            
061400     ELSE                                                                 
061500        MOVE 'FEL I POSITIONERINGEN EFTER CHECKP WDE8' TO                 
061600              FELTEXT-STR                                                 
061700        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
061800     END-IF                                                               
061900                                                                          
062000     MOVE LOW-VALUE                   TO W-WDE901KY-MIN-X                 
062100     MOVE 4562-IDORDER                TO W-IDORDER-MIN                    
062200                                         W-IDORDER-MAX                    
062300     .                                                                    
062400     EJECT                                                                
062500 X-TAG-CHECKPOINT   SECTION.                                              
062600     MOVE ' X-TAG-CHECKPOINT SECTION.' TO FELTEXT-STR                     
062700                                                                          
062800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
062900     MOVE '4561' TO  W-IDHTYP                                             
063000     MOVE LOW-VALUE  TO NYCKEL-VALFRI                                     
063100     PERFORM IMS-LAES-ATERSTART-4561                                      
063200                                                                          
063300*--- NYCKLAR TILL WDE801                                                  
063400     MOVE PHUV-IDDISTR               TO 4562-IDDISTR                      
063500     MOVE PHUV-IDKUNDNR              TO 4562-IDKUNDNR                     
063600     MOVE PHUV-IDKUNDRF              TO 4562-IDKUNDRF                     
063700                                                                          
063800     MOVE +0                         TO 4562-IDORDER                      
064000                                        4562-IDARTNR                      
064100                                        4562-IDLOPNR                      
064200*--- NYCKLAR TILL WDE901                                                  
064300     IF PRAD-IDORDER > +0                                                 
064400        MOVE PRAD-IDORDER            TO 4562-IDORDER                      
064600        MOVE PRAD-IDARTNR            TO 4562-IDARTNR                      
064700        MOVE PRAD-IDLOPNR            TO 4562-IDLOPNR                      
064800     END-IF                                                               
064900     IF OBKR-IDORDER > +0                                                 
065000        MOVE OBKR-IDORDER            TO 4562-IDORDER                      
065200        MOVE OBKR-IDARTNR            TO 4562-IDARTNR                      
065300        MOVE OBKR-IDLOPNR            TO 4562-IDLOPNR                      
065400     END-IF                                                               
065500     MOVE W-W41261-KVPOST-UT         TO 4562-KVPOST                       
065600     ACCEPT  4562-TIUPPDAT           FROM DATE                            
065700     ACCEPT  4562-TIUPPTID           FROM TIME                            
065800                                                                          
065900     IF SEGMENT-FINNS                                                     
066000        PERFORM IMS-REPL-ATERSTART-4561                                   
066100     ELSE                                                                 
066200        MOVE '1' TO 4562-KDSEGKEY                                         
066300        PERFORM IMS-ISRT-ATERSTART-4561                                   
066400     END-IF                                                               
066500                                                                          
066600     PERFORM IMS-CHECKPOINT                                               
066700*--- LÄS OM DATABAS OM DET BEHÖVS                                         
066800     PERFORM IMS-LAES-ATERSTART-4561                                      
066900     .                                                                    
067000     EJECT                                                                
067100* --- IMS SEKTIONER ---                                                   
067200     SKIP3                                                                
067300     EJECT                                                                
067400 IMS-OPEN-GSAM-W41261   SECTION.                                          
067500                                                                          
067600     MOVE 'OUT' TO WSUT-POST                                              
067700     CALL CBLTDLI USING OPEN-GSAM GSAM-PCB  GSAM-POST                     
067800     .                                                                    
067900     EJECT                                                                
068000 IMS-CLOSE-GSAM-W41261   SECTION.                                         
068100                                                                          
068200     CALL CBLTDLI USING CLSE-GSAM GSAM-PCB GSAM-POST                      
068300     .                                                                    
068400     EJECT                                                                
068500 IMS-ISRT-GSAM-W41261   SECTION.                                          
068600                                                                          
068700     MOVE '  ' TO GODK-STATUSKODER                                        
068800     CALL CBLTDLI USING ISRT GSAM-PCB GSAM-POST                           
068900     MOVE GSAM-STATUS-CODE TO STATUS-WS                                   
069000     PERFORM IMS-STATUSKONTROLL                                           
069100                                                                          
069200     MOVE 'XRST'    TO POSTSUM-TRANSTYP                                   
069300     MOVE 'W41261 ' TO POSTSUM-FDNAMN                                     
069400     MOVE 'W41261D1' TO POSTSUM-DDNAMN2                                   
069500     CALL POSTSUM USING POSTSUM-PARM                                      
069600     .                                                                    
069700     EJECT                                                                
069800 IMS-LAES-ATERSTART-4561  SECTION.                                        
069900                                                                          
070000     STRING 'WLXXLQ01(WDGXKEY  =' W-IDHTYP-X ')'                          
070100            DELIMITED BY SIZE INTO SSA1                                   
070200     MOVE 'WLXXLQ11(KDSEGKEY =1)' TO SSA2                                 
070300     MOVE '  GE' TO GODK-STATUSKODER                                      
070400     CALL CBLTDLI USING GHU XXLQ-PCB XXLQ-IO-AREA SSA1 SSA2               
070500     MOVE XXLQ-STATUS-CODE TO STATUS-WS                                   
070600     PERFORM IMS-STATUSKONTROLL                                           
070700     .                                                                    
070800     EJECT                                                                
070900 IMS-REPL-ATERSTART-4561  SECTION.                                        
071000                                                                          
071100     MOVE '  ' TO GODK-STATUSKODER                                        
071200     CALL CBLTDLI USING REPL XXLQ-PCB XXLQ-IO-AREA                        
071300     MOVE XXLQ-STATUS-CODE TO STATUS-WS                                   
071400     PERFORM IMS-STATUSKONTROLL                                           
071500     .                                                                    
071600     SKIP2                                                                
071700 IMS-DLET-ATERSTART-4562  SECTION.                                        
071800                                                                          
071900     MOVE '  ' TO GODK-STATUSKODER                                        
072000     CALL CBLTDLI USING DLET XXLQ-PCB XXLQ-IO-AREA                        
072100     MOVE XXLQ-STATUS-CODE TO STATUS-WS                                   
072200     PERFORM IMS-STATUSKONTROLL                                           
072300     .                                                                    
072400     SKIP2                                                                
072500 IMS-ISRT-ATERSTART-4561  SECTION.                                        
072600                                                                          
072700     STRING 'WLXXLQ01(WDGXKEY  =' W-IDHTYP-X ')'                          
072800            DELIMITED BY SIZE INTO SSA1                                   
072900     MOVE 'WLXXLQ11 '           TO SSA2                                   
073000     MOVE '  ' TO GODK-STATUSKODER                                        
073100     CALL CBLTDLI USING ISRT XXLQ-PCB XXLQ-IO-AREA SSA1 SSA2              
073200     MOVE XXLQ-STATUS-CODE TO STATUS-WS                                   
073300     PERFORM IMS-STATUSKONTROLL                                           
073400     .                                                                    
073500     EJECT                                                                
073600 IMS-GET-PROC-WDE8-OKVAL SECTION.                                         
073700     MOVE 'WLPROC01 ' TO SSA1                                             
073800     MOVE '  GE' TO GODK-STATUSKODER                                      
073900     CALL CBLTDLI USING GHU PROC-PCB PROC-IO-AREA SSA1                    
074000     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
074100     PERFORM IMS-STATUSKONTROLL                                           
074200     .                                                                    
074300     SKIP2                                                                
074400 IMS-GET-PROC-WDE8 SECTION.                                               
074500     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
074600          DELIMITED BY SIZE INTO SSA1                                     
074700     MOVE '  GE' TO GODK-STATUSKODER                                      
074800     CALL CBLTDLI USING GHU PROC-PCB PROC-IO-AREA SSA1                    
074900     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
075000     PERFORM IMS-STATUSKONTROLL                                           
075100     .                                                                    
075200     SKIP2                                                                
075300 IMS-GHN-PROC-WDE8-OKVAL SECTION.                                         
075400     MOVE 'WLPROC01 ' TO SSA1                                             
075500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
075600     CALL CBLTDLI USING GHN PROC-PCB PROC-IO-AREA SSA1                    
075700     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
075800     PERFORM IMS-STATUSKONTROLL                                           
075900     .                                                                    
076000     SKIP2                                                                
076100 IMS-DLET-PROC-WDE8 SECTION.                                              
076200     MOVE '  ' TO GODK-STATUSKODER                                        
076300     CALL CBLTDLI USING DLET PROC-PCB PROC-IO-AREA                        
076400     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-GET-PROD-WDE9 SECTION.                                               
076900     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
077000                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
077100          DELIMITED BY SIZE INTO SSA1                                     
077200     MOVE '  GE' TO GODK-STATUSKODER                                      
077300     CALL CBLTDLI USING GHU PROD-PCB PROD-IO-AREA SSA1                    
077400     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     .                                                                    
077700     SKIP2                                                                
077800 IMS-GHN-PROD-WDE9 SECTION.                                               
077900     STRING 'WLPROD01(WDE901KY>=' W-WDE901KY-MIN-X                        
078000                    '&WDE901KY<=' W-WDE901KY-MAX-X ')'                    
078100          DELIMITED BY SIZE INTO SSA1                                     
078200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
078300     CALL CBLTDLI USING GHN PROD-PCB PROD-IO-AREA SSA1                    
078400     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
078500     PERFORM IMS-STATUSKONTROLL                                           
078600     .                                                                    
078700     SKIP2                                                                
078800 IMS-DLET-PROD-WDE9 SECTION.                                              
078900                                                                          
079000     MOVE '  ' TO GODK-STATUSKODER                                        
079100     CALL CBLTDLI USING DLET PROD-PCB PROD-IO-AREA                        
079200     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
079300     PERFORM IMS-STATUSKONTROLL                                           
079400     .                                                                    
079500     EJECT                                                                
079600 IMS-GHU-ORQM-WDQ1 SECTION.                                               
079700     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
079800                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
079900          DELIMITED BY SIZE INTO SSA1                                     
080000     MOVE '  GE' TO GODK-STATUSKODER                                      
080100     CALL CBLTDLI USING GHU ORQM-PCB ORQM-IO-AREA SSA1                    
080200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
080300     PERFORM IMS-STATUSKONTROLL                                           
080400     .                                                                    
080500     SKIP2                                                                
080600 IMS-GHN-ORQM-WDQ1 SECTION.                                               
080700     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
080800                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
080900          DELIMITED BY SIZE INTO SSA1                                     
081000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
081100     CALL CBLTDLI USING GHN ORQM-PCB ORQM-IO-AREA SSA1                    
081200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
081300     PERFORM IMS-STATUSKONTROLL                                           
081400     .                                                                    
081500     SKIP2                                                                
081600 IMS-DLET-ORQM-WDQ1 SECTION.                                              
081700                                                                          
081800     MOVE '  ' TO GODK-STATUSKODER                                        
081900     CALL CBLTDLI USING DLET ORQM-PCB ORQM-IO-AREA                        
082000     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
082100     PERFORM IMS-STATUSKONTROLL                                           
082200     .                                                                    
082300     EJECT                                                                
082400 IMS-GHU-ARTM-WDK9 SECTION.                                               
082500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
082600          DELIMITED BY SIZE INTO SSA1                                     
082700     MOVE '  GE' TO GODK-STATUSKODER                                      
082800     CALL CBLTDLI USING GHU ARTM-PCB ARTM-IO-AREA SSA1                    
082900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
083000     PERFORM IMS-STATUSKONTROLL                                           
083100     .                                                                    
083200     SKIP3                                                                
083300 IMS-REPL-ARTM-WDK9 SECTION.                                              
083400     MOVE '  ' TO GODK-STATUSKODER                                        
083500     CALL CBLTDLI USING REPL ARTM-PCB ARTM-IO-AREA                        
083600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
083700     PERFORM IMS-STATUSKONTROLL                                           
083800     .                                                                    
083900     EJECT                                                                
084000 IMS-GET-ORQI-WDQ2 SECTION.                                               
084100     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
084200          DELIMITED BY SIZE INTO SSA1                                     
084300     MOVE '  GE' TO GODK-STATUSKODER                                      
084400     CALL CBLTDLI USING GU ORQI-PCB ORQI-IO-AREA SSA1                     
084500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     .                                                                    
084800     EJECT                                                                
084900 IMS-RESTART SECTION.                                                     
085000     SKIP2                                                                
085100     MOVE SPACE TO MSG-IO-AREA                                            
085200     MOVE '  ' TO GODK-STATUSKODER                                        
085300     CALL CBLTDLI USING XRST MSG-PCB                                      
085400                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
085500                        CHKP-AREA-LENGTH CHKP-AREA                        
085600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085700     PERFORM IMS-STATUSKONTROLL                                           
085800     .                                                                    
085900     EJECT                                                                
086000 IMS-CHECKPOINT SECTION.                                                  
086100     SKIP2                                                                
086200     MOVE SPACE TO MSG-IO-AREA                                            
086300     MOVE '  XD' TO GODK-STATUSKODER                                      
086400     CALL CBLTDLI USING CHKP MSG-PCB                                      
086500                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
086600                        CHKP-AREA-LENGTH CHKP-AREA                        
086700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086800     PERFORM IMS-STATUSKONTROLL                                           
086900                                                                          
087000     IF IMS-EJ-OK                                                         
087100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
087200       DISPLAY FELTEXT                                                    
087300       CALL FELLOG                                                        
087400     END-IF                                                               
087500     .                                                                    
087600     EJECT                                                                
087700 IMS-STATUSKONTROLL SECTION.                                              
087800     SKIP2                                                                
087900     SET STATUS-IX TO 1                                                   
088000     SEARCH GODK-STATUS                                                   
088100       AT END                                                             
088200         MOVE 'FEL STATUS-KOD FRÅN IMS' TO FELTEXT-STR                    
088300         DISPLAY FELTEXT                                                  
088400         CALL FELLOG                                                      
088500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
088600     END-SEARCH                                                           
088700     .                                                                    
088710     EJECT                                                                
088900*    -COPY WY2000P1                                                       
