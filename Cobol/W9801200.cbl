000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W9801200.                                            
000300 AUTHOR.             KJELL ANDRE.                                         
000400     DATE-WRITTEN.       26 AUGUSTI 1985.                                 
000500                                                                          
000600     REMARKS.                                                             
000700*      PROGRAMMET UPPDATERAR SOP-REGISTRET MED KATALOG-ORD                
000800*      FÖR ETT INTERVALL AV DATUM.                                        
000900*      DATUM-INTERVALLET HÄMTAS FRÅN EXEC-PARM.                           
001000*      KATALOGORDEN HÄMTAS FRÅN EN SEKVENTIELL DÄR VARJE POST             
001100*      INNEHÅLLER ETT DATUM FÖLJT AV TILLHÖRANDE KATALOG-ORD.             
001200*      FILEN ÄR SORTERAD I STIGANDE DATUM-ORDNING, MEN MULTIPLA           
001300*      POSTER MED SAMMA DATUM KAN FÖREKOMMA.                              
001400*                                                                         
001500*      NORMALA KATALOG-ORD LÄGGS UPP UNDER DATUM.CAT.                     
001600*      KATALOGORD SOM BÖRJAR MED "NOT-" ELLER "NOT." LÄGGS UPP            
001700*      UNDER DATUM.NCAT.                                                  
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*--- INFIL                                                                
002600     SELECT DATUMFIL            ASSIGN TO W98012D1.                       
002700                                                                          
002800*    W980WSPC ANVÄNDER DDNAMNET: SOPDD1                                   
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  DATUMFIL                                                             
003500     LABEL RECORD STANDARD                                                
003600     RECORDING F                                                          
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900 01  DATUMFIL-BUFFER         PIC X(250).                                  
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004201*    -COPY WY2000W1                                                       
004210     SKIP3                                                                
004300 01  KONSTANTER.                                                          
004400     03  JA                  PIC X       VALUE 'J'.                       
004500     03  NEJ                 PIC X       VALUE 'N'.                       
004600                                                                          
004700 01  EOF-SWITCHAR.                                                        
004800     03  EOF-DATUMFIL        PIC X       VALUE 'N'.                       
004900                                                                          
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100     03  W980WSPC            PIC X(8)    VALUE 'W980WSPC'.                
005200     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
005300                                                                          
005400 01  DIVERSE-INDEX-OCH-PEKARE.                                            
005500     03  PTR                 PIC S9(9)              COMP SYNC.            
005600                                                                          
005700 01  RETURKODER.                                                          
005800     03  RKOD                    PIC S9(4)   COMP VALUE +0.               
005900     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.              
006000     03  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.            
006100                                                                          
006200 01  SPAR-DATUM                  PIC 9(6).                                
006300                                                                          
006400 01  W-ORD.                                                               
006500     03  W-ORD-POS-1-4           PIC X(4).                                
006600     03  W-ORD-SLUTET            PIC X(100).                              
006700     EJECT                                                                
006800*------- RAD FRÅN DATUM-FILEN                                             
006900 01  DATUMFIL-POST.                                                       
007000     03  DATUMFIL-DATUM          PIC 9(6).                                
007100     03  DATUMFIL-ORD            PIC X(244).                              
007200                                                                          
007300 01  PTR-MAX                     PIC S9(9) COMP SYNC VALUE +244.          
007400     EJECT                                                                
007500*------- PARAMETERAR TILL W980WSPC (HANTERING AV SOP-REGISTRET)           
007600                                                                          
007700 01  WSPACE-DDNAMN-PARM.                                                  
007800     03  FILLER              PIC S9(4)  COMP VALUE +8.                    
007900     03  WSPACE-DDNAMN       PIC X(8)   VALUE 'SOPDD1  '.                 
008000                                                                          
008100 01  FUNKTIONSKODER.                                                      
008200     03  FOPEN               PIC X(4)    VALUE 'OPEN'.                    
008300     03  FCLSE               PIC X(4)    VALUE 'CLSE'.                    
008400     03  FSAVE               PIC X(4)    VALUE 'SAVE'.                    
008500     03  FTEST               PIC X(4)    VALUE 'TEST'.                    
008600     03  FGETF               PIC X(4)    VALUE 'GETF'.                    
008700     03  FGETN               PIC X(4)    VALUE 'GETN'.                    
008800     03  FADD                PIC X(4)    VALUE 'ADD '.                    
008900     03  FSET                PIC X(4)    VALUE 'SET '.                    
009000     03  FCLR                PIC X(4)    VALUE 'CLR '.                    
009100     03  FDELK               PIC X(4)    VALUE 'DELK'.                    
009200                                                                          
009300 01  WSRKOD                  PIC X.                                       
009400                                                                          
009500 01  ATTR-PARM.                                                           
009600     03   PROCESS-LENGD      PIC S9(4)   COMP.                            
009700     03   ATTR-VERDE         PIC X(20).                                   
009800                                                                          
009900 01  ORD-PARM.                                                            
010000     03   ORD-LENGD          PIC S9(4)   COMP.                            
010100     03   ORD-VERDE          PIC X(100).                                  
010200                                                                          
010300 01  DATUM-PARM.                                                          
010400     03   DATUM-LENGD        PIC S9(4)   COMP  VALUE +6.                  
010500     03   DATUM-VERDE        PIC S9(7)   COMP-3.                          
010600                                                                          
010700 01  KALENDER-PARM.                                                       
010800     03   FILLER             PIC S9(4)   COMP  VALUE +7.                  
010900     03   FILLER             PIC X(5)    VALUE '*CAL*'.                   
011000                                                                          
011100 01  AKTUELLT-DATUM-PARM.                                                 
011200     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
011300     03   FILLER             PIC X(4)    VALUE 'CDAT'.                    
011400                                                                          
011500 01  DATUM-LIST-PARM.                                                     
011600     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
011700     03   FILLER             PIC X(4)    VALUE 'DATL'.                    
011800                                                                          
011900 01  CATALOG-PARM.                                                        
012000     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
012100     03   FILLER             PIC X(3)    VALUE 'CAT'.                     
012200                                                                          
012300 01  NOT-CATALOG-PARM.                                                    
012400     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
012500     03   FILLER             PIC X(4)    VALUE 'NCAT'.                    
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800     SKIP2                                                                
012900 01  EXEC-PARM.                                                           
013000     03  LAENGD                    PIC S9(4)   COMP.                      
013100     03  EXEC-START-DATUM          PIC 9(6).                              
013200     03  EXEC-STOPP-DATUM          PIC 9(6).                              
013300     EJECT                                                                
013400 PROCEDURE DIVISION USING EXEC-PARM.                                      
013500                                                                          
013600     PERFORM A-INIT                                                       
013700     PERFORM B-RENSA-KALENDER                                             
013800                                                                          
013900     PERFORM S01-LAES-DATUMFIL                                            
013902     MOVE DATUMFIL-DATUM   TO TMP1-YYMMDD                                 
013903     MOVE EXEC-START-DATUM TO TMP2-YYMMDD                                 
013904     PERFORM WY2000P1                                                     
014000     PERFORM UNTIL EOF-DATUMFIL = JA                                      
014100        OR TMP1-YYMMDD NOT < TMP2-YYMMDD                                  
014200        PERFORM S01-LAES-DATUMFIL                                         
014210       MOVE DATUMFIL-DATUM   TO TMP1-YYMMDD                               
014220       MOVE EXEC-START-DATUM TO TMP2-YYMMDD                               
014230       PERFORM WY2000P1                                                   
014300     END-PERFORM                                                          
014301                                                                          
014305     MOVE DATUMFIL-DATUM   TO TMP1-YYMMDD                                 
014306     MOVE EXEC-STOPP-DATUM TO TMP2-YYMMDD                                 
014307     PERFORM WY2000P1                                                     
014400     PERFORM UNTIL EOF-DATUMFIL = JA OR                                   
014500        TMP1-YYMMDD > TMP2-YYMMDD                                         
014600        MOVE DATUMFIL-DATUM TO SPAR-DATUM                                 
014700        PERFORM C-INIT-DATUM                                              
014800        PERFORM UNTIL EOF-DATUMFIL = JA OR                                
014900                SPAR-DATUM NOT = DATUMFIL-DATUM                           
015000          PERFORM D-LAEGG-UPP-ORD                                         
015100          PERFORM S01-LAES-DATUMFIL                                       
015200        END-PERFORM                                                       
015210       MOVE DATUMFIL-DATUM   TO TMP1-YYMMDD                               
015220       MOVE EXEC-STOPP-DATUM TO TMP2-YYMMDD                               
015230       PERFORM WY2000P1                                                   
015300     END-PERFORM                                                          
015400     PERFORM E-KOLLA-AKTUELLT-DATUM                                       
015500     PERFORM Z-SLUT                                                       
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK.                                                              
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000     SKIP2                                                                
016100*------------- KONTROLLERA DATUM I EXEC-PARM                              
016200     IF EXEC-START-DATUM NOT NUMERIC                                      
016300         OR EXEC-STOPP-DATUM NOT NUMERIC                                  
016400       DISPLAY 'SOP200S  INVALID START OR STOP DATE IN EXEC-PARM'         
016500             UPON CONSOLE                                                 
016600       DISPLAY '         START DATE: ' EXEC-START-DATUM                   
016700               '  STOP DATE: ' EXEC-STOPP-DATUM                           
016800             UPON CONSOLE                                                 
016900       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017000     END-IF                                                               
017100                                                                          
017200*------------- ÖPPNA SOPREGISTRET                                         
017300     CALL W980WSPC USING FOPEN WSRKOD WSPACE-DDNAMN-PARM                  
017400     IF WSRKOD NOT = SPACE                                                
017500       DISPLAY 'SOP005S  CAN NOT OPEN DDNAME ' WSPACE-DDNAMN              
017600             UPON CONSOLE                                                 
017700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017800     END-IF                                                               
017900                                                                          
018000     OPEN INPUT DATUMFIL                                                  
018100     .                                                                    
018200     EJECT                                                                
018300 B-RENSA-KALENDER      SECTION.                                           
018400     SKIP2                                                                
018500*------ RENSA KATALOG-ORD                                                 
018600     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
018700          DATUM-LIST-PARM DATUM-PARM                                      
018800                                                                          
018900     PERFORM UNTIL WSRKOD NOT = SPACE                                     
019000       CALL W980WSPC USING FDELK WSRKOD DATUM-PARM                        
019100               CATALOG-PARM                                               
019200       CALL W980WSPC USING FDELK WSRKOD DATUM-PARM                        
019300               NOT-CATALOG-PARM                                           
019400                                                                          
019500       CALL W980WSPC USING FGETN WSRKOD KALENDER-PARM                     
019600          DATUM-LIST-PARM DATUM-PARM                                      
019700     END-PERFORM                                                          
019800                                                                          
019900*------ RENSA LISTAN ÖVER TILLÅTNA DATUM                                  
020000     CALL W980WSPC USING FCLR WSRKOD KALENDER-PARM                        
020100               DATUM-LIST-PARM                                            
020200     .                                                                    
020300     EJECT                                                                
020400 C-INIT-DATUM          SECTION.                                           
020500     SKIP2                                                                
020600     MOVE DATUMFIL-DATUM TO DATUM-VERDE                                   
020700                                                                          
020800*------ LÄGG UPP DATUMET I LISTAN ÖVER TILLÅTNA DATUM                     
020900     CALL W980WSPC USING FADD WSRKOD KALENDER-PARM                        
021000                 DATUM-LIST-PARM  DATUM-PARM                              
021100     IF WSRKOD NOT = SPACE                                                
021200       DISPLAY 'SOP026S  SECTION C.  W980WSPC RETURN CODE: '              
021300               WSRKOD UPON CONSOLE                                        
021400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
021500     END-IF                                                               
021600                                                                          
021700     DISPLAY 'SOP201I  DATE ' DATUMFIL-DATUM ' ADDED TO CALENDAR'         
021800     .                                                                    
021900     EJECT                                                                
022000 D-LAEGG-UPP-ORD       SECTION.                                           
022100     SKIP2                                                                
022200     MOVE 1 TO PTR                                                        
022300     INSPECT DATUMFIL-ORD TALLYING PTR FOR LEADING SPACE                  
022400                                                                          
022500     PERFORM UNTIL PTR > PTR-MAX                                          
022600       UNSTRING DATUMFIL-ORD DELIMITED BY ALL SPACE                       
022700              INTO W-ORD COUNT IN ORD-LENGD                               
022800              WITH POINTER PTR                                            
022900       IF W-ORD-POS-1-4 = 'NOT-' OR 'NOT.'                                
023000          MOVE W-ORD-SLUTET TO ORD-VERDE                                  
023100          SUBTRACT 4 FROM ORD-LENGD                                       
023200          MOVE NOT-CATALOG-PARM TO ATTR-PARM                              
023300       ELSE                                                               
023400          MOVE W-ORD TO ORD-VERDE                                         
023500          MOVE CATALOG-PARM TO ATTR-PARM                                  
023600       END-IF                                                             
023700       ADD 2 TO ORD-LENGD                                                 
023800                                                                          
023900       CALL W980WSPC USING FADD WSRKOD DATUM-PARM                         
024000                   ATTR-PARM ORD-PARM                                     
024100       IF WSRKOD NOT = SPACE                                              
024200         DISPLAY 'SOP026S  SECTION D.  W980WSPC RETURN CODE: '            
024300               WSRKOD UPON CONSOLE                                        
024400         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
024500       END-IF                                                             
024600       IF ATTR-PARM = CATALOG-PARM                                        
024700         DISPLAY '           ' ORD-VERDE                                  
024800       ELSE                                                               
024900         DISPLAY '           NOT: ' ORD-VERDE                             
025000       END-IF                                                             
025100     END-PERFORM                                                          
025200     .                                                                    
025300     EJECT                                                                
025400 E-KOLLA-AKTUELLT-DATUM   SECTION.                                        
025500     SKIP2                                                                
025600*----- OM AKTUELLT DATUM ÄR SATT I SOP-REGISTRET, MÅSTE                   
025700*      DET INGÅ I DET NYA DATUM-INTERVALLET                               
025800                                                                          
025900     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
026000                 AKTUELLT-DATUM-PARM  DATUM-PARM                          
026100     IF WSRKOD = SPACE                                                    
026200       CALL W980WSPC USING FTEST WSRKOD KALENDER-PARM                     
026300                 DATUM-LIST-PARM  DATUM-PARM                              
026400       IF WSRKOD NOT = SPACE                                              
026500        MOVE DATUM-VERDE TO SPAR-DATUM                                    
026600        DISPLAY 'SOP202S  CURRENT DATE IN THE SOP DATA BASE, '            
026700                SPAR-DATUM ', NOT FOUND IN THE NEW CALENDAR.'             
026800        UPON CONSOLE                                                      
026900        DISPLAY '         THE CALENDAR WILL NOT BE UPDATED.'              
027000                ' ALL CHANGES ARE CANCELLED.'                             
027100        UPON CONSOLE                                                      
027200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
027300       END-IF                                                             
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 Z-SLUT SECTION.                                                          
027800     SKIP2                                                                
027900     CALL W980WSPC USING FSAVE WSRKOD                                     
028000     IF WSRKOD NOT = SPACE                                                
028100       DISPLAY 'SOP026S  SECTION Z.  W980WSPC RETURN CODE: '              
028200               WSRKOD UPON CONSOLE                                        
028300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
028400     ELSE                                                                 
028500       CALL W980WSPC USING FCLSE WSRKOD                                   
028600       IF WSRKOD NOT = SPACE                                              
028700         DISPLAY 'SOP028S  CAN NOT CLOSE DDNAME ' WSPACE-DDNAMN           
028800         UPON CONSOLE                                                     
028900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
029000       END-IF                                                             
029100     END-IF                                                               
029200                                                                          
029300     CLOSE DATUMFIL                                                       
029400     .                                                                    
029500     EJECT                                                                
029600 S01-LAES-DATUMFIL   SECTION.                                             
029700     SKIP2                                                                
029800     READ DATUMFIL INTO DATUMFIL-POST                                     
029900         AT END MOVE JA TO EOF-DATUMFIL                                   
030000     END-READ                                                             
030100     .                                                                    
030110     EJECT                                                                
030200*    -COPY WY2000P1                                                       
