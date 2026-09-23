000100 IDENTIFICATION DIVISION.                                                 
000200                                                                          
000300 PROGRAM-ID.    W2314000.                                                 
000400                                                                          
000500 AUTHOR.        THOMAS FALLENIUS.                                         
000600                                                                          
000700 DATE-WRITTEN.  MAJ 1979.                                                 
000800                                                                          
000900     REMARKS.                                                             
001000*            PROGRAMMET LÄSER RESTORDERFILEN W44061 SOM ÄR                
001100*            SORTERARAD PER ARTNR OCH SKAPAR EN UTPOST PER ARTIKEL        
001200*                                                                         
001300* 2012-01-03 E'TRACKER: 10143271 CHINA WAREHOUSE PROJECT-1                
001400*                                                                         
001500                                                                          
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000     SKIP2                                                                
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300     SELECT W44061     ASSIGN TO UT-S-W23140D1.                           
002400     SELECT W23141     ASSIGN TO UT-S-W23140D2.                           
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP2                                                                
003000 FD  W44061                                                               
003100     LABEL RECORD STANDARD                                                
003200     RECORDING F                                                          
003300     BLOCK CONTAINS 0.                                                    
003400                                                                          
003500*01          -COPY W44060      -PRE W44061-   -L.                         
003600     SKIP2                                                                
003700 FD  W23141                                                               
003800     LABEL RECORD STANDARD                                                
003900     RECORDING F                                                          
004000     BLOCK CONTAINS 0.                                                    
004100     SKIP2                                                                
004200*01  FILE    -COPY W231217     -PRE W23141-    -L.                        
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004600*    -COPY WY2000W3                                                       
004700     SKIP3                                                                
004800*    -COPY WY2000W2                                                       
004900     SKIP3                                                                
005000 77  JA                 PIC X       VALUE 'J'.                            
005100 77  NEJ                PIC X       VALUE 'N'.                            
005200     SKIP2                                                                
005300 01  END-OF-FILE-SWITCHAR.                                                
005400     03  W44061-EOF     PIC X       VALUE 'N'.                            
005500     SKIP3                                                                
005600*      --- VALID IDDC CODES                                               
005700*                                                                         
005800*01    -COPY WWDC99                                                       
005900       EJECT                                                              
006000 01  GENERELLA-SUBPROGRAM.                                                
006100     03  DATKORT        PIC X(8)   VALUE 'DATKORT'.                       
006200     03  WDATKONV       PIC X(8)   VALUE 'WDATKONV'.                      
006300     03  POSTSUM        PIC X(8)   VALUE 'POSTSUM'.                       
006400     03  W009VADD       PIC X(8)   VALUE 'W009VADD'.                      
006500     EJECT                                                                
006600*                      ARBETS-AREOR FÖR INFIL W44061.                     
006700     SKIP2                                                                
006800 01  SAMMA-IDARTNR      PIC 9(9)   COMP-3.                                
006900     SKIP3                                                                
007000*01  POST   -PRE I61-      -COPY W44060                                   
007100     EJECT                                                                
007200*                      ARBETS-AREOR FÖR UTFIL W23141.                     
007300     SKIP2                                                                
007400 01  FILLER             PIC X(16)  VALUE 'UTAREA-W23141'.                 
007500     SKIP3                                                                
007600*01  POST   -PRE U41217-   -COPY W231217.                                 
007700     EJECT                                                                
007800 01  WS-TIRODAT              PIC S9(5).                                   
007900 01  FILLER   REDEFINES  WS-TIRODAT.                                      
008000     03  TIRODAT-A1          PIC 9.                                       
008100     03  TIRODAT-A2          PIC 9.                                       
008200     03  TIRODAT-VV          PIC 99.                                      
008300     03  TIRODAT-D           PIC 9.                                       
008400 01  FILLER   REDEFINES  WS-TIRODAT.                                      
008500     03  TIRODAT-AA          PIC 99.                                      
008600     03  FILLER              PIC 999.                                     
008700 01  FILLER   REDEFINES  WS-TIRODAT.                                      
008800     03  TIRODAT-AAVV        PIC 9999.                                    
008900     03  FILLER              PIC 9.                                       
009000     SKIP2                                                                
009100 01  WS-DATUM                PIC S9(5).                                   
009200 01  FILLER   REDEFINES  WS-DATUM.                                        
009300     03  DATUM-AA            PIC 99.                                      
009400     03  DATUM-VV            PIC 99.                                      
009500     03  DATUM-D             PIC 9.                                       
009600 01  FILLER   REDEFINES  WS-DATUM.                                        
009700     03  DATUM-AAVV          PIC 9999.                                    
009800     03  FILLER              PIC 9.                                       
009900 01  FILLER   REDEFINES  WS-DATUM.                                        
010000     03  DATUM-A1            PIC 9.                                       
010100     03  FILLER              PIC 9999.                                    
010200     SKIP2                                                                
010300 01  WS-DATUM-KORR           PIC S9(5)               COMP-3.              
010400 01  WS-PLUS4-KORR           PIC S9(3)   VALUE +4    COMP-3.              
010500     EJECT                                                                
010600 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W23140'.                  
010700 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
010800*01  -COPY WDATKORT                                                       
010900     EJECT                                                                
011000*01  -COPY WDATAREA                                                       
011100     EJECT                                                                
011200*01  -COPY W0005       -PRE POSTSUM-                                      
011300     EJECT                                                                
011400 PROCEDURE DIVISION.                                                      
011500     SKIP2                                                                
011600 STYR SECTION.                                                            
011700     SKIP2                                                                
011800     PERFORM A-INIT                                                       
011900     SKIP2                                                                
012000     PERFORM S01-LAS-W44061                                               
012100     PERFORM UNTIL W44061-EOF = 'J'                                       
012200         MOVE I61-RAD-IDARTNR TO SAMMA-IDARTNR                            
012300         PERFORM UNTIL I61-RAD-IDARTNR NOT = SAMMA-IDARTNR OR             
012400                 W44061-EOF = 'J'                                         
012500             MOVE I61-RAD-IDDC        TO WS-IDDC                          
012600             IF I61-RAD-KDSTARAD = 2                                      
012700             AND GOOD-DC                                                  
012800             AND NOT NDC                                                  
013000                 PERFORM B-SKAPA-UTDATA                                   
013100             END-IF                                                       
013200             PERFORM S01-LAS-W44061                                       
013300         END-PERFORM                                                      
013400         IF U41217-IDARTNR NUMERIC                                        
013500             PERFORM S02-SKRIV-W23141                                     
013600         END-IF                                                           
013700     END-PERFORM                                                          
013800     SKIP2                                                                
013900     PERFORM C-FINIT                                                      
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK.                                                              
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400     SKIP2                                                                
014500     OPEN INPUT W44061                                                    
014600     SKIP2                                                                
014700     OPEN OUTPUT W23141                                                   
014800     SKIP2                                                                
014900     MOVE LOW-VALUE TO U41217-POST                                        
015000     MOVE 99999 TO U41217-TIRODAT (1)                                     
015100                   U41217-TIRODAT (2)                                     
015200     MOVE ZERO TO U41217-KVRORAD-0-4 (1)                                  
015300                  U41217-KVRORAD-5-8 (1)                                  
015400                  U41217-KVRORAD-9 (1)                                    
015500                  U41217-KVRORAD-0-4 (2)                                  
015600                  U41217-KVRORAD-5-8 (2)                                  
015700                  U41217-KVRORAD-9 (2)                                    
015800     SKIP2                                                                
015900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016000     SKIP2                                                                
016100     MOVE D-AAR TO DATUM-AA                                               
016200     MOVE D-VECKA TO DATUM-VV                                             
016300     MOVE D-DAGNR TO DATUM-D                                              
016400     SKIP2                                                                
016500     MOVE 'W23140' TO POSTSUM-PROGNAMN                                    
016600     MOVE 'W23140D2' TO POSTSUM-DDNAMN2                                   
016700     MOVE 'W23141' TO POSTSUM-FDNAMN.                                     
016800     EJECT                                                                
016900 B-SKAPA-UTDATA SECTION.                                                  
017000     SKIP2                                                                
017100     MOVE '217'         TO U41217-IDPTYP                                  
017200     MOVE I61-RAD-IDARTNR TO U41217-IDARTNR                               
017300     SKIP2                                                                
017400     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
017500     MOVE I61-RAD-DARODAT (3:6) TO DAT-I-TIDATUM                          
017600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
017700                        DAT-O-TIDATUM DAT-KDSVAR                          
017800     MOVE DAT-TIAAVVD   TO WS-TIRODAT                                     
017900     SKIP2                                                                
018000     MOVE DATUM-A1 TO TIRODAT-A1                                          
018100     MOVE WS-TIRODAT   TO TMP1-YYWWD                                      
018200     MOVE WS-DATUM     TO TMP2-YYWWD                                      
018300     PERFORM WY2000P2                                                     
018400     IF TMP1-YYWWD > TMP2-YYWWD                                           
018500         IF TIRODAT-AA < 10                                               
018600             ADD 90 TO TIRODAT-AA                                         
018700         ELSE                                                             
018800             SUBTRACT 10 FROM TIRODAT-AA                                  
018900         END-IF                                                           
019000     END-IF                                                               
019100     SKIP2                                                                
019200     MOVE WS-TIRODAT         TO TMP1-YYWWD                                
019300     MOVE U41217-TIRODAT (1) TO TMP2-YYWWD                                
019400     PERFORM WY2000P2                                                     
019500     IF TMP1-YYWWD < TMP2-YYWWD                                           
019600         MOVE WS-TIRODAT TO U41217-TIRODAT (1)                            
019700     END-IF                                                               
019800     SKIP2                                                                
019900     MOVE TIRODAT-AAVV TO WS-DATUM-KORR                                   
020000     CALL W009VADD USING WS-DATUM-KORR WS-PLUS4-KORR                      
020100     MOVE DATUM-AAVV      TO TMP1-YYWW                                    
020200     MOVE WS-DATUM-KORR   TO TMP2-YYWW                                    
020300     PERFORM WY2000P3                                                     
020400     IF TMP1-YYWW <= TMP2-YYWW                                            
020500         ADD +1 TO U41217-KVRORAD-0-4 (1)                                 
020600     ELSE                                                                 
020700         CALL W009VADD USING WS-DATUM-KORR WS-PLUS4-KORR                  
020800         MOVE DATUM-AAVV      TO TMP1-YYWW                                
020900         MOVE WS-DATUM-KORR   TO TMP2-YYWW                                
021000         PERFORM WY2000P3                                                 
021100         IF TMP1-YYWW <= TMP2-YYWW                                        
021200             ADD +1 TO U41217-KVRORAD-5-8 (1)                             
021300         ELSE                                                             
021400             ADD +1 TO U41217-KVRORAD-9 (1)                               
021500         END-IF                                                           
021600     END-IF.                                                              
021700     EJECT                                                                
021800 C-FINIT SECTION.                                                         
021900     SKIP2                                                                
022000     CLOSE W44061                                                         
022100     SKIP2                                                                
022200     CLOSE W23141                                                         
022300     SKIP2                                                                
022400     MOVE 'S' TO POSTSUM-OPKOD                                            
022500     CALL POSTSUM USING POSTSUM-PARM.                                     
022600     EJECT                                                                
022700 S01-LAS-W44061 SECTION.                                                  
022800     SKIP2                                                                
022900     READ W44061 INTO I61-POST                                            
023000         AT END                                                           
023100             MOVE JA TO W44061-EOF                                        
023200     END-READ                                                             
023300     SKIP2                                                                
023400     IF W44061-EOF = 'N'                                                  
023500         MOVE 'R57' TO POSTSUM-TRANSTYP                                   
023600         CALL POSTSUM USING POSTSUM-PARM                                  
023700     END-IF.                                                              
023800     EJECT                                                                
023900 S02-SKRIV-W23141 SECTION.                                                
024000     SKIP2                                                                
024100     IF U41217-TIRODAT (1) = 99999                                        
024200         MOVE ZERO TO U41217-TIRODAT (1)                                  
024300     END-IF                                                               
024400     SKIP2                                                                
024500     IF U41217-TIRODAT (2) = 99999                                        
024600         MOVE ZERO TO U41217-TIRODAT (2)                                  
024700     END-IF                                                               
024800     SKIP2                                                                
024900     WRITE W23141-FILE FROM U41217-POST                                   
025000     SKIP2                                                                
025100     MOVE LOW-VALUE TO U41217-POST                                        
025200     MOVE 99999 TO U41217-TIRODAT (1)                                     
025300                   U41217-TIRODAT (2)                                     
025400     MOVE ZERO TO U41217-KVRORAD-0-4 (1)                                  
025500                  U41217-KVRORAD-5-8 (1)                                  
025600                  U41217-KVRORAD-9 (1)                                    
025700                  U41217-KVRORAD-0-4 (2)                                  
025800                  U41217-KVRORAD-5-8 (2)                                  
025900                  U41217-KVRORAD-9 (2).                                   
026000     EJECT                                                                
026100     EJECT                                                                
026200*    -COPY WY2000P2                                                       
026300     EJECT                                                                
026400*    -COPY WY2000P3                                                       
