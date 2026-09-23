000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W22504.                                              
000300 AUTHOR.             MONICA.                                              
000400 DATE-WRITTEN.       NOVEMBER 1994.                                       
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*                                                                         
000900*    FUNKTION                                                             
001100*        PROGRAMMET LÄSER FILEN W44061, SEKV RO-REG                       
001200*        SKRIVER RESTORDERPOSTER (57-OR) PÅ W22505                        
001300*                                                                         
001400*    OBS!   SORT I BÖRJAN AV PROCEDUREN, GER SORTORDNING                  
001500*           IDARTNR IDDC KDSTARAD DARODAT                                 
001510*    OBS!   SKALL ÄNDRAS VID LAYOUTÄNDRING PÅ WDA5                        
001600*                                                                         
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*                            *** REST-DIV-ORDFIL                          
002600*                            *** INPUT                                    
002700     SELECT W44061           ASSIGN W22504D1.                             
002800*                                                                         
002900*                            *** RESTORDERREG                             
003000*                            *** OUTPUT                                   
003100     SELECT W22505           ASSIGN W22504D2.                             
003200*                                                                         
003300*                            *** KALENDER                                 
003400*                            *** INPUT                                    
003500     SELECT W22101           ASSIGN W22504D3.                             
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 FILE SECTION.                                                            
003900     SKIP2                                                                
004000 FD  W44061                                                               
004100     RECORDING F                                                          
004200     BLOCK 0                                                              
004300     LABEL RECORD STANDARD.                                               
004400*01  -COPY W44060     -L.                                                 
004600     SKIP2                                                                
004700 FD  W22505                                                               
004800     RECORDING F                                                          
004900     BLOCK 0                                                              
005000     LABEL RECORD STANDARD.                                               
005100*01  POST  -COPY W225P231   -PRE U01NIV2- -L                              
005300     SKIP2                                                                
005400     EJECT                                                                
005500 FD  W22101                                                               
005600     RECORDING F                                                          
005700     BLOCK 0                                                              
005800     LABEL RECORD STANDARD.                                               
005900*01  -COPY W221W001 -L.                                                   
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300     SKIP2                                                                
006301                                                                          
006302*    -COPY WY2000W9                                                       
006303     SKIP3                                                                
006400 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16   COMP SYNC.           
006500     SKIP2                                                                
006600 01  KONSTANTER.                                                          
006700     03  JA                  PIC X       VALUE 'J'.                       
006800     03  NEJ                 PIC X       VALUE 'N'.                       
006900     SKIP2                                                                
007000 01  EOF-SWITCHAR.                                                        
007100     03  W44061-EOF          PIC X       VALUE 'N'.                       
007200     03  EOF-KAL             PIC X       VALUE 'N'.                       
007300 01  SW.                                                                  
007400     03  SW-TRAFF-KALENDER   PIC X       VALUE 'N'.                       
007500         88  TRAFF-I-KALENDER            VALUE 'J'.                       
007600     SKIP3                                                                
007700 01  W-AAVV.                                                              
007800     03  W-AA                PIC 9(2)  VALUE ZERO.                        
007900     03  W-VV                PIC 9(2)  VALUE ZERO.                        
008000 01  W-AAVV-9  REDEFINES W-AAVV PIC 9(4).                                 
008100     SKIP3                                                                
008200 01  W-AAR.                                                               
008300     03  W-AAR-1             PIC 9.                                       
008400     03  FILLER              PIC X.                                       
008500 01  W-AAR-N REDEFINES W-AAR PIC 9(2).                                    
008600 01  W-ROAAVV.                                                            
008700     03  W-ROAA.                                                          
008800         05  W-ROAA-1        PIC 9.                                       
008900         05  FILLER          PIC X.                                       
009000     03  W-ROAA-N REDEFINES W-ROAA PIC 9(2).                              
009100     03  W-ROVV              PIC 9(2).                                    
009200     03  FILLER              PIC X.                                       
009300 01  W-ROAAVV-N REDEFINES W-ROAAVV PIC 9(5).                              
009400 01  TIAAVVD                 PIC 9(5)    COMP-3.                          
009500     SKIP3                                                                
009600 01  OLD-ID.                                                              
009700     03  OLD-ARTNR-DC.                                                    
009800         05  OLD-ARTNR       PIC S9(9).                                   
009900         05  OLD-DC          PIC  X(2).                                   
010000     03  OLD-VECK59          PIC  9(4).                                   
010100 01  NEW-ID.                                                              
010200     03  NEW-ARTNR-DC.                                                    
010300         05  NEW-ARTNR       PIC S9(9).                                   
010400         05  NEW-DC          PIC  X(2).                                   
010500     03  NEW-VECK59          PIC  9(4).                                   
010600 01  IDKUNDRF-X.                                                          
010700     03  IDKUNDRF-9          PIC 9(5).                                    
010800     03  FILLER              PIC X(5).                                    
010900     EJECT                                                                
010910*      --- VALID IDDC CODES                                               
010920*                                                                         
010930*01    -COPY WWDC99                                                       
010940       EJECT                                                              
011000 01  DYNAMISKA-SUBPROGRAM.                                                
011100     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
011200     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
011300     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
011400     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
011500     SKIP2                                                                
011600*                            *************************************        
011700*                            *** PARAMETRAR TILL DATKORT       ***        
011800*                            *************************************        
011900 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22504'.                  
012000 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
012100*01  -COPY WDATKORT                                                       
012300*                            *************************************        
012400*                            *** TESTDISTRIKT                  ***        
012500*                            *************************************        
012600 01  FILLER                  PIC X(16)   VALUE 'TESTDISTRIKT'.            
012700 01  TEST-IDDISTR            PIC 9(5)    COMP-3.                          
012800*01  WWDIST19   -COPY WWDIST19  -RED TEST-IDDISTR.                        
013000     EJECT                                                                
013100*01  WWDIST20   -COPY WWDIST20  -RED TEST-IDDISTR.                        
013300     EJECT                                                                
013400*                            *************************************        
013500*                            *** PARAMETRAR TILL WDATKONV      ***        
013600*                            *************************************        
013700*01  -COPY WDATAREA                                                       
013900     EJECT                                                                
014000*                            *************************************        
014100*                            *** PARAMTERAR TILL POSTSUM       ***        
014200*                            *************************************        
014300*01  -COPY W0005      -PRE POSTSUM-                                       
014500     EJECT                                                                
014600*                            *************************************        
014700*                            *** AREA FÖR INFIL                ***        
014800*                            *************************************        
014900*01  AREA -COPY W44060     -PRE I01-                                      
015100     EJECT                                                                
015200*                            *************************************        
015300*                            *** AREA FÖR UTPOST               ***        
015400*                            *************************************        
015500*01  AREA -COPY  W225P231   -PRE U01NIV2-                                 
015700     EJECT                                                                
015800*                             ***********************************         
015900*                             *** TRANSAR KALENDER            ***         
016000*                             ***********************************         
016100*01  AREA -COPY W221W001 -PRE I01KAL-.                                    
016300     EJECT                                                                
016400 01  WKALAR.                                                              
016500     03  FILLER               PIC X.                                      
016600     03  WKALAR-2             PIC 9.                                      
016700 01  WKALAR-N REDEFINES WKALAR PIC 99.                                    
016800*                             ***********************************         
016900*                             *** KALENDERTRANSAR, FÖR PERIOD ***         
017000*                             ***********************************         
017100 01  KALTAB.                                                              
017200     03  INGANG OCCURS 10 ASCENDING KEY IS KAL-TIAAR KAL-TIVECKNR         
017300                          INDEXED BY KAL-IDEX.                            
017400*       05  -COPY W221W001 -PRE KAL-.                                     
017600     EJECT                                                                
017700 PROCEDURE DIVISION.                                                      
017800     SKIP2                                                                
017900     PERFORM A-INITIERA                                                   
018000     SKIP1                                                                
018100     PERFORM B-LAS-W44061                                                 
018200     PERFORM UNTIL W44061-EOF = JA                                        
018300         MOVE NEW-ARTNR      TO U01NIV2-IDARTNR                           
018400         MOVE NEW-ID         TO OLD-ID                                    
018500         PERFORM UNTIL W44061-EOF = JA OR                                 
018600                 OLD-ARTNR NOT = NEW-ARTNR                                
018700             PERFORM D-INIT-57-POST                                       
018800             MOVE NEW-DC TO OLD-DC                                        
018900            PERFORM UNTIL W44061-EOF = JA OR                              
019000                OLD-ARTNR-DC NOT = NEW-ARTNR-DC                           
019001                MOVE I01-RAD-IDDC    TO WS-IDDC                           
019002                IF CDC-SE                                                 
019100                   EVALUATE I01-RAD-KDSTARAD                              
019200                      WHEN '2'                                            
019300                               PERFORM E-SUM-57-POST                      
019400                               PERFORM B-LAS-W44061                       
019500                      WHEN OTHER                                          
019600                               PERFORM B-LAS-W44061                       
019700                   END-EVALUATE                                           
019710                ELSE                                                      
019720                   PERFORM B-LAS-W44061                                   
019730                END-IF                                                    
019800            END-PERFORM                                                   
019900            IF  U01NIV2-KVRORAD-TOT-CDC > 0                               
020000                PERFORM H-SKRIV-57-POST                                   
020100            END-IF                                                        
020200        END-PERFORM                                                       
020300     END-PERFORM                                                          
020400     SKIP1                                                                
020500     PERFORM Z-AVSLUTA                                                    
020600                                                                          
020700     MOVE ZERO           TO RETURN-CODE                                   
020800                                                                          
020900     GOBACK.                                                              
021000     EJECT                                                                
021100 A-INITIERA SECTION.                                                      
021200******************************************************************        
021300*    ÖPPNAR FILER  LÄS DATUMKORT                                 *        
021400******************************************************************        
021500     SKIP2                                                                
021600     OPEN INPUT  W44061 W22101                                            
021700     OPEN OUTPUT W22505                                                   
021800     SKIP1                                                                
021900     CALL DATKORT  USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT              
022000         MOVE D-AAR           TO W-AAR-N                                  
022100     SKIP2                                                                
022200*-------------------------------------- LÄS KALENDER                      
022300     SET KAL-IDEX            TO 1                                         
022400     MOVE ALL '9'            TO KALTAB                                    
022500     READ W22101 INTO I01KAL-AREA                                         
022600          AT END MOVE JA     TO EOF-KAL                                   
022700     END-READ                                                             
022800     PERFORM UNTIL EOF-KAL = JA                                           
022900         MOVE I01KAL-TIAAR   TO WKALAR-N                                  
023000         IF  I01KAL-TIPER = K-PERIOD                                      
023100         AND WKALAR-2     = K-AAR                                         
023200             MOVE I01KAL-AREA TO INGANG (KAL-IDEX)                        
023300             SET KAL-IDEX UP BY 1                                         
023400         END-IF                                                           
023500         READ W22101 INTO I01KAL-AREA                                     
023600             AT END MOVE JA TO EOF-KAL                                    
023700         END-READ                                                         
023800     END-PERFORM                                                          
023900     CLOSE W22101.                                                        
024000     EJECT                                                                
024100 B-LAS-W44061 SECTION.                                                    
024200******************************************************************        
024300*    LÄSER W44061 OCH ÖKAR POSTRÄKNAREN                          *        
024400******************************************************************        
024500     SKIP2                                                                
024600     READ W44061 INTO I01-AREA                                            
024700         AT END MOVE JA  TO W44061-EOF                                    
024800     END-READ                                                             
024900     IF  W44061-EOF = NEJ                                                 
025000         MOVE 'W44061'       TO POSTSUM-FDNAMN                            
025100         MOVE 'W22504D1'     TO POSTSUM-DDNAMN2                           
025200         MOVE I01-RAD-KDSTARAD   TO POSTSUM-TRANSTYP                      
025300         CALL POSTSUM USING POSTSUM-PARM                                  
025400         MOVE I01-RAD-IDARTNR  TO NEW-ARTNR                               
025500         MOVE I01-RAD-IDDC     TO NEW-DC                                  
027200         IF I01-RAD-KDSTARAD = '2'                                        
027300            IF I01-RAD-DARODAT NOT ZERO                                   
027400               MOVE 'AAMMDD'         TO DAT-KDDATFORM                     
027500               MOVE I01-RAD-DARODAT (3:6) TO DAT-I-TIDATUM                
027600               CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM            
027700                                  DAT-O-TIDATUM DAT-KDSVAR                
027800               IF DAT-KDSVAR-FEL                                          
027900                  DISPLAY 'FEL I DATUMKONVERTERINGEN'                     
028000                  CALL ABEND USING RKOD-ABEND-UTAN-DUMP                   
028100               END-IF                                                     
028200               MOVE DAT-TIAA-VECKA   TO W-AA                              
028300               MOVE DAT-TIVV         TO W-VV                              
028400               MOVE W-AAVV-9         TO NEW-VECK59                        
028500               MOVE DAT-TIAAVVD      TO TIAAVVD                           
028600            END-IF                                                        
028700         END-IF                                                           
028800         MOVE I01-RAD-IDKUNDRF TO IDKUNDRF-X                              
028900     END-IF                                                               
028910     .                                                                    
029000     EJECT                                                                
029100 D-INIT-57-POST SECTION.                                                  
029200******************************************************************        
029300*    INITIERAR DETALJPOST 57 (RESTORDERPOST)                     *        
029400******************************************************************        
029500     SKIP2                                                                
029600     MOVE '231'              TO U01NIV2-IDPTYP                            
029610     MOVE NEW-ARTNR          TO U01NIV2-IDARTNR                           
029700     MOVE 1                  TO U01NIV2-KDCLPOST                          
029800     MOVE ZERO               TO U01NIV2-KVROS-CDC-1-2                     
029900     MOVE ZERO               TO U01NIV2-KVROS-CDC-3-4                     
030200     MOVE ZERO               TO U01NIV2-TIRODAT-ORDER-CDC                 
030300     MOVE ZERO               TO U01NIV2-KVRORAD-KVAR-V1-CDC               
030400     MOVE ZERO               TO U01NIV2-KVRORAD-KVAR-IV-CDC               
030500     MOVE ZERO               TO U01NIV2-KVRORAD-KVAR-P-CDC                
030501     MOVE ZERO               TO U01NIV2-KVRORAD-TOT-CDC                   
030510     .                                                                    
030600     EJECT                                                                
030700 E-SUM-57-POST SECTION.                                                   
030800******************************************************************        
030900*    ADDERAR BELOPP UR INTRANS57 TILL UTPOST 57 NIVÅ2            *        
031000******************************************************************        
031100     SKIP1                                                                
031200     EVALUATE I01-RAD-KDORDKL                                             
031300        WHEN 1                                                            
031400               ADD I01-RAD-KVART   TO U01NIV2-KVROS-CDC-1-2               
031500        WHEN 2                                                            
031600               ADD I01-RAD-KVART   TO U01NIV2-KVROS-CDC-1-2               
031700        WHEN 3                                                            
031800               ADD I01-RAD-KVART   TO U01NIV2-KVROS-CDC-3-4               
031900        WHEN 4                                                            
032000               ADD I01-RAD-KVART   TO U01NIV2-KVROS-CDC-3-4               
032100        WHEN OTHER                                                        
032200               ADD I01-RAD-KVART   TO U01NIV2-KVROS-CDC-3-4               
032300     END-EVALUATE                                                         
032400     ADD 1                   TO U01NIV2-KVRORAD-TOT-CDC                   
032500     MOVE W-AAVV-9        TO U01NIV2-TIRODAT-ORDER-CDC                    
032600*-------------------------------------- SÖK KALENDER                      
032700     MOVE I01-RAD-IDDISTR TO TEST-IDDISTR                                 
032800     IF I01-RAD-KDORDKL > 4 AND                                           
032900       (DIST19-SATS OR                                                    
033200        DIST20-EMBALLAGE)                                                 
033300         CONTINUE                                                         
033400     ELSE                                                                 
033500         MOVE TIAAVVD         TO W-ROAAVV-N                               
033600         MOVE W-AAR-1         TO W-ROAA-1                                 
033601         MOVE W-ROAA-N   TO TMP1-YY                                       
033602         MOVE D-AAR      TO TMP2-YY                                       
033610         PERFORM WY2000P9                                                 
033700         IF  TMP1-YY > TMP2-YY                                            
033710           IF W-ROAA-1 = 00                                               
033720             MOVE 99          TO W-ROAA-1                                 
033730           ELSE                                                           
033800             SUBTRACT +1      FROM W-ROAA-1                               
033810           END-IF                                                         
033900         END-IF                                                           
034000         MOVE JA              TO SW-TRAFF-KALENDER                        
034100         SEARCH ALL INGANG                                                
034200             AT END MOVE NEJ TO SW-TRAFF-KALENDER                         
034300             WHEN KAL-TIVECKNR (KAL-IDEX) = W-ROVV                        
034400             AND  KAL-TIAAR    (KAL-IDEX) = W-ROAA-N                      
034500                  CONTINUE                                                
034600         END-SEARCH                                                       
034700         IF  TRAFF-I-KALENDER                                             
034800             ADD +1            TO U01NIV2-KVRORAD-KVAR-P-CDC              
034900             IF  KAL-TIVECKNR-PERIOD (KAL-IDEX) = 1                       
035000                 ADD +1        TO U01NIV2-KVRORAD-KVAR-V1-CDC             
035100             END-IF                                                       
035200             IF  KAL-TIVECKNR-PERIOD (KAL-IDEX) = K-STATVECKA             
035300                 ADD +1        TO U01NIV2-KVRORAD-KVAR-IV-CDC             
035400             END-IF                                                       
035500         END-IF                                                           
035600     END-IF                                                               
035610     .                                                                    
035700     EJECT                                                                
035800 H-SKRIV-57-POST SECTION.                                                 
035900******************************************************************        
036000*    SKRIVER SUMMAPOST (57-SUMMA)                                *        
036100******************************************************************        
036200     SKIP2                                                                
036300     WRITE U01NIV2-POST FROM U01NIV2-AREA                                 
036400     MOVE '231'              TO POSTSUM-TRANSTYP                          
036500     MOVE 'W22504'           TO POSTSUM-FDNAMN                            
036600     MOVE 'W22504D2'         TO POSTSUM-DDNAMN2                           
036700     CALL POSTSUM USING POSTSUM-PARM                                      
036710     .                                                                    
036800     EJECT                                                                
036900 Z-AVSLUTA SECTION.                                                       
037000******************************************************************        
037100*    STÄNGER FILER  SKRIVER  POSTSUMMOR                          *        
037200******************************************************************        
037300     SKIP2                                                                
037400     CLOSE W44061 W22505                                                  
037500     SKIP1                                                                
037600     MOVE 'S'                TO POSTSUM-OPKOD                             
037700     CALL POSTSUM USING POSTSUM-PARM                                      
037710     .                                                                    
037800     EJECT                                                                
037810     EJECT                                                                
037900*    -COPY WY2000P9                                                       
