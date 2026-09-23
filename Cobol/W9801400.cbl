000100CBL TRUNC(BIN)                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W9801400.                                            
000400 AUTHOR.             KJELL ANDRE.                                         
000500     DATE-WRITTEN.       20 AUGUSTI 1985 PÅ FÖRMIDDAGEN.                  
000600                                                                          
000700     REMARKS.                                                             
000800*      PROGRAMMET BYTER AKTUELLT DATUM I SOP-REGISTRET.                   
000900*      NYTT DATUM HÄMTAS FRÅN EXEC-PARM ELLER FRÅN MASKINEN               
001000*      OM EXEC-PARM = 000000.                                             
001100*      EVENTUELLA AKTIVERINGS- OCH PASSIVERINGS-KÖER UNDERSÖKS            
001200*      OCH RENSAS (FRÅN DET GAMLA TILL DET NYA DATUMET), OCH              
001300*      ALLA PROCESSER SOM KÖATS, AKTIVERAS RESP. PASSIVERAS.              
001400*      DETTA PROGRAM KAN KÖRAS MOT ETT HELT TOMT SOP-REGISTER             
001500*      FÖR ATT SÄTTA AKTUELLT DATUM. FÖR ATT KUNNA BYTA DATUM             
001600*      DÄREMOT, MÅSTE EN KALENDER VARA DEFINIERAT FÖR ATT MAN SKA         
001700*      KUNNA HITTA VILKA DATUM SOM LIGGER MELLAN DET GAMLA OCH DET        
001800*      NYA DATUMET.                                                       
001900*      TEMPORÄRA SYMBOLVÄRDEN MED AKTIVERINGS-DATUM ÄLDRE ÄN 1            
002000*      VECKA RENSAS BORT.                                                 
002100*                                                                         
002200*      W98020 ANVÄNDS FÖR AKTIVERING/PASSIVERING. W980WSPC FÖR ATT        
002300*      LÄSA/UPPDATERA DATUM OCH KÖER. OPEN OCH CLOSE AV                   
002400*      SOP-REGISTRET GÖRS VIA W98020                                      
002500*                                                                         
002600*      TILLS VIDARE SKER INGEN NYUPPLÄGGNING AV PASSIVERINGERAR           
002700*      TILL DET NYA DATUMET.                                              
002800*                                                                         
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400*    W98020 ANVÄNDER DDNAMNEN:   SOPDD1                                   
003500*                                PDSDD1                                   
003600*                                PDSDD2                                   
003700*    W980WSPC ANVÄNDER DDNAMNET: SOPDD1                                   
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -COPY WY2000W1                                                       
004400     SKIP3                                                                
004500 01  KONSTANTER.                                                          
004600     03  JA                      PIC X       VALUE 'J'.                   
004700     03  NEJ                     PIC X       VALUE 'N'.                   
004710     03  ANTAL-DAGAR-EJ-RENSA    PIC S9(1)   COMP-3 VALUE +4.             
004800                                                                          
004900 01  BESTAELL-OM                 PIC X       VALUE 'N'.                   
005000                                                                          
005100 01  PROCESS-STATUS.                                                      
005200     03  PASSIVE-STATUS          PIC X       VALUE 'P'.                   
005300     03  WAITING-STATUS          PIC X       VALUE 'W'.                   
005400     03  STARTED-STATUS          PIC X       VALUE 'S'.                   
005500     03  ENDED-STATUS            PIC X       VALUE 'E'.                   
005600                                                                          
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005900     03  W980WSPC                PIC X(8)    VALUE 'W980WSPC'.            
006000     03  W9802000                PIC X(8)    VALUE 'W9802000'.            
006100                                                                          
006200 01  DIVERSE-INDEX-OCH-PEKARE.                                            
006300     03  AIX                     PIC S9(9)   COMP SYNC.                   
006400     03  PIX                     PIC S9(9)   COMP SYNC.                   
006500     03  OIX                     PIC S9(9)   COMP SYNC.                   
006600     03  PTR                     PIC S9(9)   COMP SYNC.                   
006700     03  SYMB-PTR                PIC S9(9)   COMP SYNC.                   
006800                                                                          
006900 01  RETURKODER.                                                          
007000     03  RKOD                    PIC S9(4)   COMP VALUE +0.               
007100     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.              
007200     03  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.            
007300                                                                          
007400 01  GAMMALT-DATUM               PIC S9(7)   COMP-3.                      
007500 01  NYTT-DATUM                  PIC S9(7)   COMP-3.                      
007600 01  DISPLAY-DATUM               PIC 9(6).                                
007700                                                                          
007800 01  W-FOM-DATUM-TAL             PIC 9(9)    COMP-3.                      
007900 01  W-TOM-DATUM-TAL             PIC 9(9)    COMP-3.                      
008000 01  W-FULLT-DATUM               PIC 9(9)    COMP-3.                      
008100 01  W-KORT-DATUM                PIC 9(6).                                
008200                                                                          
008300 01  SYMBOLER                    PIC X(1000) VALUE SPACE.                 
008400 01  SYMBOL                      PIC X(20)  VALUE SPACE.                  
008500 01  FILL                        PIC X(10)  VALUE SPACE.                  
008600                                                                          
008700 01  SPAR-RETCODE                PIC S9(4)   COMP.                        
008800 01  SPAR-MSGCODE                PIC S9(3)   COMP-3.                      
008900     EJECT                                                                
009000 01  TAB-FULL             PIC S9(9) COMP SYNC VALUE +500.                 
009100                                                                          
009200 01  FILLER               PIC X(16) VALUE 'AKT TABELL      '.             
009300 01  AIX-MAX              PIC S9(9) COMP SYNC VALUE ZERO.                 
009400 01  AKT-TABELL.                                                          
009500     03  FILLER  OCCURS 500.                                              
009600         05  A-IDPROCESS             PIC X(10).                           
009700                                                                          
009800 01  FILLER               PIC X(16) VALUE 'OM-AKT TABELL   '.             
009900 01  OIX-MAX              PIC S9(9) COMP SYNC VALUE ZERO.                 
010000 01  OM-AKT-TABELL.                                                       
010100     03  FILLER  OCCURS 500.                                              
010200         05  O-A-IDPROCESS           PIC X(10).                           
010300         05  O-A-DATUM               PIC S9(7)  COMP-3.                   
010400                                                                          
010500  01  FILLER               PIC X(16) VALUE 'PASS TABELL     '.            
010600  01  PIX-MAX              PIC S9(9) COMP SYNC VALUE ZERO.                
010700  01  PASS-TABELL.                                                        
010800      03  FILLER  OCCURS 500.                                             
010900          05  P-IDPROCESS             PIC X(10).                          
011000                                                                          
011100 01  FILLER               PIC X(16) VALUE 'AKTSYMB TABELL  '.             
011200 01  AKTSYMB-TABELL.                                                      
011300     03  FILLER  OCCURS 500.                                              
011400         05  A-SYMBOLER              PIC X(1000).                         
011500                                                                          
011600 01  FILLER               PIC X(16) VALUE 'OMAKTSYMB TABELL'.             
011700 01  OM-AKTSYMB-TABELL.                                                   
011800     03  FILLER  OCCURS 500.                                              
011900         05  O-A-SYMBOLER            PIC X(1000).                         
012000     EJECT                                                                
012100*------- PARAMETERAR TILL W980WSPC (HANTERING AV SOP-REGISTRET)           
012200                                                                          
012300 01  FUNKTIONSKODER.                                                      
012400     03  FOPEN               PIC X(4)    VALUE 'OPEN'.                    
012500     03  FCLSE               PIC X(4)    VALUE 'CLSE'.                    
012600     03  FSAVE               PIC X(4)    VALUE 'SAVE'.                    
012700     03  FGETF               PIC X(4)    VALUE 'GETF'.                    
012800     03  FGETN               PIC X(4)    VALUE 'GETN'.                    
012900     03  FTEST               PIC X(4)    VALUE 'TEST'.                    
013000     03  FSET                PIC X(4)    VALUE 'SET '.                    
013100     03  FDELK               PIC X(4)    VALUE 'DELK'.                    
013200     03  FDEL                PIC X(4)    VALUE 'DEL '.                    
013300                                                                          
013400 01  WSRKOD                  PIC X.                                       
013500                                                                          
013600 01  PROCESS-PARM.                                                        
013700     03   PROCESS-LENGD      PIC S9(4)   COMP.                            
013800     03   PROCESS-VERDE      PIC X(10).                                   
013900                                                                          
014000 01  PAR-P-PARM.                                                          
014100     03   PAR-P-LENGD        PIC S9(4)   COMP.                            
014200     03   PAR-P-VERDE        PIC X(10).                                   
014300                                                                          
014400 01  ORD-PARM.                                                            
014500     03   ORD-LENGD          PIC S9(4)   COMP.                            
014600     03   ORD-VERDE          PIC X(100).                                  
014700                                                                          
014800 01  DATUM-PARM.                                                          
014900     03   DATUM-LENGD        PIC S9(4)   COMP  VALUE +6.                  
015000     03   DATUM-VERDE        PIC S9(7)   COMP-3.                          
015100                                                                          
015200 01  DAT-ANR-PARM.                                                        
015300     03   DAT-ANR-LENGD      PIC S9(4)   COMP  VALUE +8.                  
015400     03   AKT-DATUM          PIC S9(7)   COMP-3.                          
015500     03   ANR                PIC S9(4)   COMP.                            
015600                                                                          
015700 01  ANR-PARM.                                                            
015800     03   ANR-LENGD          PIC S9(4)   COMP  VALUE +4.                  
015900     03   ANR-VERDE          PIC S9(4)   COMP.                            
016000                                                                          
016100 01  DATA-PARM.                                                           
016200     03   DATA-LENGD         PIC S9(4)   COMP  VALUE +0.                  
016300     03   DATA-VERDE         PIC X(120)  VALUE SPACE.                     
016400                                                                          
016500 01  VALUE-PARM.                                                          
016600     03   VALUE-LENGD        PIC S9(4)   COMP  VALUE +0.                  
016700     03   SYMB-VERDE         PIC X(90)   VALUE SPACE.                     
016800                                                                          
016900 01  KALENDER-PARM.                                                       
017000     03   FILLER             PIC S9(4)   COMP  VALUE +7.                  
017100     03   FILLER             PIC X(5)    VALUE '*CAL*'.                   
017200                                                                          
017300 01  SOP-PARM.                                                            
017400     03   FILLER             PIC S9(4)   COMP  VALUE +7.                  
017500     03   FILLER             PIC X(5)    VALUE '*SOP*'.                   
017600                                                                          
017700 01  AKT-NR-PARM.                                                         
017800     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
017900     03   FILLER             PIC X(5)    VALUE '*ANR'.                    
018000                                                                          
018100 01  AKTUELLT-DATUM-PARM.                                                 
018200     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
018300     03   FILLER             PIC X(4)    VALUE 'CDAT'.                    
018400                                                                          
018500 01  AQ-PARM.                                                             
018600     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
018700     03   FILLER             PIC X(2)    VALUE 'AQ'.                      
018800                                                                          
018900 01  DQ-PARM.                                                             
019000     03   FILLER             PIC S9(4)   COMP  VALUE +4.                  
019100     03   FILLER             PIC X(2)    VALUE 'DQ'.                      
019200                                                                          
019300 01  DATUM-LIST-PARM.                                                     
019400     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
019500     03   FILLER             PIC X(4)    VALUE 'DATL'.                    
019600     EJECT                                                                
019700                                                                          
019800 01  SYMB-PARM.                                                           
019900     03   FILLER             PIC S9(4)   COMP  VALUE +6.                  
020000     03   FILLER             PIC X(4)    VALUE 'SYMB'.                    
020100                                                                          
020200 01  PARENT-PARM.                                                         
020300     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
020400     03   FILLER             PIC X(4)    VALUE 'PAR'.                     
020500                                                                          
020600 01  INFO-NAMN-PARM.                                                      
020700     03   FILLER             PIC S9(4)   COMP  VALUE +5.                  
020800     03   FILLER             PIC X(4)    VALUE 'INF'.                     
020900     EJECT                                                                
021000*-------------  PARAMETRAR TILL W98020                                    
021100 01  FILLER               PIC X(16) VALUE 'SOP20-AREA      '.             
021200*    -COPY W98020                                                         
021300     EJECT                                                                
021400*-------------  DATA-PARATMETER TILL ATTRIBUT INF                         
021500 01  INFO-VAERDE-PARM.                                                    
021600*    03  -COPY W980INF                                                    
021700     EJECT                                                                
021800 LINKAGE SECTION.                                                         
021900     SKIP2                                                                
022000 01  EXEC-PARM.                                                           
022100     03  LAENGD                    PIC S9(4)   COMP.                      
022200     03  EXEC-NYTT-DATUM           PIC 9(6).                              
022300     EJECT                                                                
022400 PROCEDURE DIVISION USING EXEC-PARM.                                      
022500     SKIP2                                                                
022600     PERFORM A-INIT                                                       
022700     IF GAMMALT-DATUM > 0                                                 
022800       PERFORM B-SPARA-KOER                                               
022900     END-IF                                                               
023000     IF GAMMALT-DATUM > 0                                                 
023100       PERFORM C-BESTAELL-OM                                              
023200     END-IF                                                               
023300     PERFORM D-BYT-DATUM                                                  
023400     IF GAMMALT-DATUM > 0                                                 
023500       PERFORM E-AKTIVERA-PASSIVERA                                       
023600     END-IF                                                               
023700     IF GAMMALT-DATUM > 0                                                 
023800       PERFORM F-RENSA-GAMLA-SYMBOLER                                     
023900     END-IF                                                               
024000     PERFORM Z-SLUT                                                       
024100     MOVE RKOD TO RETURN-CODE                                             
024200     GOBACK.                                                              
024300     EJECT                                                                
024400 A-INIT SECTION.                                                          
024500     SKIP2                                                                
024600     MOVE JA TO SOP20-FLBATCH                                             
024700*------------- FASTSTÄLL NYTT AKTUELLT DATUM                              
024800     IF EXEC-NYTT-DATUM NOT NUMERIC                                       
024900       DISPLAY 'SOP250S  INVALID DATE IN EXEC-PARM: '                     
025000                EXEC-NYTT-DATUM                                           
025100       UPON CONSOLE                                                       
025200       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
025300     ELSE IF EXEC-NYTT-DATUM = ZERO                                       
025400       ACCEPT NYTT-DATUM FROM DATE                                        
025420     ELSE                                                                 
025600       MOVE EXEC-NYTT-DATUM TO NYTT-DATUM                                 
025800     END-IF                                                               
025810     END-IF                                                               
025900                                                                          
026000*------------- ÖPPNA SOPREGISTRET                                         
026100     MOVE SPACE TO SOP20-IDDDPREFIX                                       
026200     MOVE '0' TO SOP20-KDSOPFUNK                                          
026300     CALL W9802000 USING SOP20-W98020                                     
026400     IF SOP20-KDRET NOT = ZERO                                            
026500       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
026600     END-IF                                                               
026700                                                                          
026800*------------- SPARA FÖREGÅENDE AKTUELLT-DATUM.                           
026900     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
027000          AKTUELLT-DATUM-PARM  DATUM-PARM                                 
027100     IF WSRKOD = SPACE                                                    
027200       MOVE DATUM-VERDE TO GAMMALT-DATUM DISPLAY-DATUM                    
027300       DISPLAY 'SOP252I  PREVIOUS CURRENT DATE WAS '                      
027400               DISPLAY-DATUM '.'                                          
027410       IF EXEC-NYTT-DATUM = 999999                                        
027420*       -- ANVÄND FÖREGÅENDE AKT DATUM PÅ NYTT                            
027430*       -- OCH RENSA SYMB PARAMETER ÄVEN FÖR DETTA DATUM                  
027440         MOVE GAMMALT-DATUM TO NYTT-DATUM                                 
027450         MOVE ZERO          TO ANTAL-DAGAR-EJ-RENSA                       
027460       END-IF                                                             
027500     ELSE                                                                 
027600       MOVE ZERO TO GAMMALT-DATUM                                         
027700       DISPLAY 'SOP251W  PREVIOUS CURRENT DATE NOT FOUND.'                
027800       MOVE 4 TO RKOD                                                     
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 B-SPARA-KOER          SECTION.                                           
028300     SKIP2                                                                
028400     MOVE NYTT-DATUM TO DATUM-VERDE                                       
028500     CALL W980WSPC USING FTEST WSRKOD KALENDER-PARM                       
028600           DATUM-LIST-PARM DATUM-PARM                                     
028700     IF WSRKOD NOT = SPACE                                                
028800       DISPLAY 'SOP256S  NEW CURRENT DATE NOT FOUND IN CALENDAR.'         
028900             UPON CONSOLE                                                 
029000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
029100     END-IF                                                               
029200                                                                          
029300     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
029400           DATUM-LIST-PARM DATUM-PARM                                     
029500     IF WSRKOD NOT = SPACE                                                
029600       DISPLAY 'SOP255S  NO CALENDAR FOUND IN SOP DATA BASE.'             
029700             UPON CONSOLE                                                 
029800       DISPLAY '         A CALENDAR MUST EXIST BEFORE CHANGING '          
029900               'CURRENT DATE.'                                            
030000             UPON CONSOLE                                                 
030100       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
030200     END-IF                                                               
030300                                                                          
030400     MOVE 0 TO AIX PIX OIX                                                
030500                                                                          
030600*--  SÖK I KALENDERN EFTER GAMLA DATUMET                                  
030700     MOVE DATUM-VERDE   TO TMP1-YYMMDD                                    
030800     MOVE GAMMALT-DATUM TO TMP2-YYMMDD                                    
030900     PERFORM WY2000P1                                                     
031000     PERFORM UNTIL WSRKOD NOT = SPACE OR                                  
031100               TMP1-YYMMDD >= TMP2-YYMMDD                                 
031200       CALL W980WSPC USING FGETN WSRKOD KALENDER-PARM                     
031300              DATUM-LIST-PARM DATUM-PARM                                  
031400       MOVE DATUM-VERDE   TO TMP1-YYMMDD                                  
031500       MOVE GAMMALT-DATUM TO TMP2-YYMMDD                                  
031600       PERFORM WY2000P1                                                   
031700     END-PERFORM                                                          
031800                                                                          
031900*--  SPARA ALLA KÖER FRAM T.O.M DET NYA DATUMET                           
032000     MOVE DATUM-VERDE   TO TMP1-YYMMDD                                    
032100     MOVE NYTT-DATUM    TO TMP2-YYMMDD                                    
032200     PERFORM WY2000P1                                                     
032300     PERFORM UNTIL WSRKOD NOT = SPACE OR                                  
032400                  TMP1-YYMMDD > TMP2-YYMMDD                               
032500       MOVE DATUM-VERDE TO DISPLAY-DATUM                                  
032600       PERFORM BA-SPARA-AQ                                                
032700       PERFORM BB-SPARA-DQ                                                
032800       CALL W980WSPC USING FGETN WSRKOD KALENDER-PARM                     
032900             DATUM-LIST-PARM DATUM-PARM                                   
033000       MOVE DATUM-VERDE   TO TMP1-YYMMDD                                  
033100       MOVE NYTT-DATUM    TO TMP2-YYMMDD                                  
033200       PERFORM WY2000P1                                                   
033300     END-PERFORM                                                          
033400                                                                          
033500     IF AIX-MAX = TAB-FULL OR PIX-MAX = TAB-FULL OR                       
033600        OIX-MAX = TAB-FULL                                                
033700        DISPLAY 'SOP257S  QUEUE TABLE OVERFLOW.'                          
033800              UPON CONSOLE                                                
033900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
034000     END-IF                                                               
034100                                                                          
034200     MOVE PIX TO PIX-MAX                                                  
034300     MOVE AIX TO AIX-MAX                                                  
034400     MOVE OIX TO OIX-MAX                                                  
034500     .                                                                    
034600     EJECT                                                                
034700 BA-SPARA-AQ    SECTION.                                                  
034800     SKIP2                                                                
034900     MOVE SPACE TO PROCESS-VERDE                                          
035000     CALL W980WSPC USING FGETF WSRKOD DATUM-PARM AQ-PARM                  
035100                   PROCESS-PARM                                           
035200     IF WSRKOD = SPACE                                                    
035300       DISPLAY 'SOP253I  PROCESS(ES) QUEUED FOR ACTIVATION ON '           
035400               DISPLAY-DATUM ':'                                          
035500       PERFORM UNTIL WSRKOD NOT = SPACE OR AIX NOT < TAB-FULL             
035600                OR OIX NOT < TAB-FULL                                     
035700         MOVE ZERO TO AKT-DATUM                                           
035800         MOVE ZERO TO ANR                                                 
035900         CALL W980WSPC USING FGETF WSRKOD PROCESS-PARM AQ-PARM            
036000                    DAT-ANR-PARM                                          
036100         PERFORM UNTIL WSRKOD NOT = SPACE OR                              
036200                      DATUM-VERDE = AKT-DATUM                             
036300           MOVE ZERO TO AKT-DATUM                                         
036400           MOVE ZERO TO ANR                                               
036500           CALL W980WSPC USING FGETN WSRKOD PROCESS-PARM AQ-PARM          
036600                    DAT-ANR-PARM                                          
036700         END-PERFORM                                                      
036800         CALL W980WSPC USING FDEL WSRKOD PROCESS-PARM                     
036900                       AQ-PARM  DAT-ANR-PARM                              
037000                                                                          
037100*        -- KOLLA OM PROCESSEN ÄR AKTIV.                                  
037200*        -- BESTÄLL I SÅ FALL OM TILL URSPRUNGS-DATUMET SÅ ATT            
037300*        -- ALLA MULTIPLA BESTÄLLNINGAR KÖRS FÄRDIGT.                     
037400         MOVE NEJ TO BESTAELL-OM                                          
037500         CALL W980WSPC USING FGETF WSRKOD PROCESS-PARM                    
037600                       INFO-NAMN-PARM INFO-VAERDE-PARM                    
037700         IF WSRKOD = SPACE                                                
037800           IF (KDPROCSTAT = WAITING-STATUS OR                             
037900                 STARTED-STATUS) AND TIAPDAT = DATUM-VERDE                
038000             MOVE JA TO BESTAELL-OM                                       
038100           END-IF                                                         
038200         END-IF                                                           
038300         DISPLAY '           ' PROCESS-VERDE                              
038400         IF BESTAELL-OM = NEJ                                             
038500           ADD 1 TO AIX                                                   
038600           MOVE PROCESS-VERDE TO A-IDPROCESS (AIX)                        
038700         ELSE                                                             
038800           ADD 1 TO OIX                                                   
038900           MOVE PROCESS-VERDE TO O-A-IDPROCESS (OIX)                      
039000           MOVE DATUM-VERDE   TO O-A-DATUM (OIX)                          
039100         END-IF                                                           
039200         IF ANR NOT = 0                                                   
039300           PERFORM BAA-HAEMTA-SYMBOLER                                    
039400           IF BESTAELL-OM = NEJ                                           
039500             MOVE SYMBOLER TO A-SYMBOLER (AIX)                            
039600           ELSE                                                           
039700             MOVE SYMBOLER TO O-A-SYMBOLER (OIX)                          
039800           END-IF                                                         
039900         ELSE                                                             
040000           MOVE SPACE TO A-SYMBOLER (AIX)                                 
040100                         O-A-SYMBOLER (OIX)                               
040200         END-IF                                                           
040300         MOVE SPACE TO PROCESS-VERDE                                      
040400         CALL W980WSPC USING FGETN WSRKOD DATUM-PARM AQ-PARM              
040500                   PROCESS-PARM                                           
040600       END-PERFORM                                                        
040700                                                                          
040800     ELSE                                                                 
040900       DISPLAY 'SOP254I  NO PROCESSES QUEUED FOR '                        
041000               'ACTIVATION ON ' DISPLAY-DATUM '.'                         
041100     END-IF                                                               
041200     CALL W980WSPC USING FDELK WSRKOD DATUM-PARM AQ-PARM                  
041300     .                                                                    
041400     EJECT                                                                
041500 BAA-HAEMTA-SYMBOLER   SECTION.                                           
041600     SKIP2                                                                
041700     MOVE ANR TO ANR-VERDE                                                
041800     MOVE SPACE TO SYMBOLER DATA-VERDE                                    
041900     CALL W980WSPC USING FGETF WSRKOD ANR-PARM SYMB-PARM                  
042000             DATA-PARM                                                    
042100       MOVE +1 TO SYMB-PTR                                                
042200     PERFORM UNTIL WSRKOD NOT = SPACE                                     
042300       MOVE SPACE TO SYMBOL                                               
042400       UNSTRING DATA-VERDE DELIMITED BY '&'                               
042500           INTO FILL SYMBOL                                               
042600       STRING SYMBOL DELIMITED BY SPACE                                   
042700              '(' DELIMITED BY SIZE                                       
042800              INTO SYMBOLER WITH POINTER SYMB-PTR                         
042900       MOVE ALL '?' TO SYMB-VERDE                                         
043000       CALL W980WSPC USING FGETF WSRKOD ANR-PARM DATA-PARM                
043100               VALUE-PARM                                                 
043200       IF WSRKOD = SPACE                                                  
043300         STRING SYMB-VERDE DELIMITED BY '????????'                        
043400                ') ' DELIMITED BY SIZE                                    
043500                INTO SYMBOLER WITH POINTER SYMB-PTR                       
043600         MOVE SPACE TO DATA-VERDE                                         
043700         CALL W980WSPC USING FGETN WSRKOD ANR-PARM SYMB-PARM              
043800                 DATA-PARM                                                
043900       ELSE                                                               
044000        DISPLAY 'SOP260S - VALUE NOT FOUND FOR SYMBOL ' DATA-VERDE        
044100         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
044200       END-IF                                                             
044300     END-PERFORM                                                          
044400     .                                                                    
044500     EJECT                                                                
044600 BB-SPARA-DQ    SECTION.                                                  
044700     SKIP2                                                                
044800     MOVE SPACE TO PROCESS-VERDE                                          
044900     CALL W980WSPC USING FGETF WSRKOD DATUM-PARM DQ-PARM                  
045000                   PROCESS-PARM                                           
045100     IF WSRKOD = SPACE                                                    
045200       DISPLAY 'SOP253I  PROCESS(ES) QUEUED FOR PASSIVATION ON '          
045300             DISPLAY-DATUM ':'                                            
045400       PERFORM UNTIL WSRKOD NOT = SPACE OR PIX NOT < TAB-FULL             
045500         CALL W980WSPC USING FDEL WSRKOD PROCESS-PARM                     
045600                       DQ-PARM  DATUM-PARM                                
045700         DISPLAY '           ' PROCESS-VERDE                              
045800         IF DATUM-VERDE = NYTT-DATUM                                      
045900           ADD 1 TO PIX                                                   
046000           MOVE PROCESS-VERDE TO P-IDPROCESS (PIX)                        
046100         END-IF                                                           
046200         MOVE SPACE TO PROCESS-VERDE                                      
046300         CALL W980WSPC USING FGETN WSRKOD DATUM-PARM DQ-PARM              
046400                   PROCESS-PARM                                           
046500       END-PERFORM                                                        
046600                                                                          
046700     ELSE                                                                 
046800       DISPLAY 'SOP254I  NO PROCESSES QUEUED FOR '                        
046900               'PASSIVATION ON ' DISPLAY-DATUM '.'                        
047000     END-IF                                                               
047100     CALL W980WSPC USING FDELK WSRKOD DATUM-PARM DQ-PARM                  
047200     .                                                                    
047300     EJECT                                                                
047400 C-BESTAELL-OM         SECTION.                                           
047500     SKIP2                                                                
047600     MOVE ZERO TO SPAR-RETCODE                                            
047700                                                                          
047800     MOVE 'O' TO SOP20-KDSOPFUNK                                          
047900     MOVE 1 TO OIX                                                        
048000     PERFORM UNTIL OIX  > OIX-MAX                                         
048100        MOVE O-A-IDPROCESS (OIX) TO SOP20-IDPROCESS                       
048200        MOVE O-A-SYMBOLER  (OIX) TO SOP20-TESYMBV                         
048300        MOVE O-A-DATUM     (OIX) TO SOP20-TIAPDAT                         
048400        CALL W9802000 USING SOP20-W98020                                  
048500        IF SOP20-KDRET > SPAR-RETCODE                                     
048600          MOVE SOP20-KDRET TO SPAR-RETCODE                                
048700          MOVE SOP20-KDMEDD TO SPAR-MSGCODE                               
048800        END-IF                                                            
048900        ADD 1 TO OIX                                                      
049000     END-PERFORM                                                          
049100                                                                          
049200     IF SPAR-RETCODE > 4                                                  
049300        DISPLAY 'SOP259E  ERROR OCURRED DURING REACTIVATING.'             
049400                       UPON CONSOLE                                       
049500        DISPLAY '         SEE MESSAGE SOP' SPAR-MSGCODE                   
049600              UPON CONSOLE                                                
049700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
049800     END-IF                                                               
049900     .                                                                    
050000     EJECT                                                                
050100 D-BYT-DATUM           SECTION.                                           
050200     SKIP2                                                                
050300     MOVE NYTT-DATUM TO DATUM-VERDE DISPLAY-DATUM                         
050400     CALL W980WSPC USING FSET WSRKOD KALENDER-PARM                        
050500           AKTUELLT-DATUM-PARM DATUM-PARM                                 
050600     IF WSRKOD NOT = SPACE                                                
050700       DISPLAY 'SOP026S  SECTION C. W980WSPC RETURN CODE = '              
050800                WSRKOD                                                    
050900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
051000     ELSE                                                                 
051100       DISPLAY 'SOP258I  NEW CURRENT DATE IS ' DISPLAY-DATUM '.'          
051200       CALL W980WSPC USING FSAVE WSRKOD                                   
051300     END-IF                                                               
051400     .                                                                    
051500     EJECT                                                                
051600 E-AKTIVERA-PASSIVERA  SECTION.                                           
051700     SKIP2                                                                
051800     MOVE NYTT-DATUM TO SOP20-TIAPDAT                                     
051900     MOVE ZERO TO SPAR-RETCODE                                            
052000                                                                          
052100     MOVE 'O' TO SOP20-KDSOPFUNK                                          
052200     MOVE 1 TO AIX                                                        
052300     PERFORM UNTIL AIX  > AIX-MAX                                         
052400        MOVE A-IDPROCESS (AIX) TO SOP20-IDPROCESS                         
052500        MOVE A-SYMBOLER  (AIX) TO SOP20-TESYMBV                           
052600        CALL W9802000 USING SOP20-W98020                                  
052700        IF SOP20-KDRET > SPAR-RETCODE                                     
052800          MOVE SOP20-KDRET TO SPAR-RETCODE                                
052900          MOVE SOP20-KDMEDD TO SPAR-MSGCODE                               
053000        END-IF                                                            
053100        ADD 1 TO AIX                                                      
053200     END-PERFORM                                                          
053300                                                                          
053400     MOVE 'C' TO SOP20-KDSOPFUNK                                          
053500     MOVE 1 TO PIX                                                        
053600     PERFORM UNTIL PIX  > PIX-MAX                                         
053700        MOVE P-IDPROCESS (PIX) TO SOP20-IDPROCESS                         
053800        MOVE SPACE TO SOP20-TESYMBV                                       
053900        CALL W9802000 USING SOP20-W98020                                  
054000        IF SOP20-KDRET > SPAR-RETCODE                                     
054100          MOVE SOP20-KDRET TO SPAR-RETCODE                                
054200          MOVE SOP20-KDMEDD TO SPAR-MSGCODE                               
054300        END-IF                                                            
054400        ADD 1 TO PIX                                                      
054500     END-PERFORM                                                          
054600                                                                          
054700     IF SPAR-RETCODE > 4                                                  
054800        DISPLAY 'SOP259E  ERROR OCURRED DURING ACTIVATION/'               
054900             'PASSIVATION.' UPON CONSOLE                                  
055000        DISPLAY '         SEE MESSAGE SOP' SPAR-MSGCODE                   
055100              UPON CONSOLE                                                
055200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 F-RENSA-GAMLA-SYMBOLER   SECTION.                                        
055700     SKIP2                                                                
055800     MOVE DISPLAY-DATUM TO W-KORT-DATUM                                   
055900     PERFORM FA-BERAEKNA-RENS-TOM                                         
056000                                                                          
056100     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
056200          DATUM-LIST-PARM DATUM-PARM                                      
056300     MOVE DATUM-VERDE TO DISPLAY-DATUM                                    
056400     MOVE DISPLAY-DATUM TO W-KORT-DATUM                                   
056500     PERFORM FD-BERAEKNA-RENS-FOM                                         
056600                                                                          
056700     PERFORM UNTIL W-FOM-DATUM-TAL > W-TOM-DATUM-TAL                      
056800       MOVE W-KORT-DATUM TO DISPLAY-DATUM                                 
056900       MOVE DISPLAY-DATUM TO DATUM-VERDE                                  
057000       CALL W980WSPC USING FGETF WSRKOD DATUM-PARM                        
057100                    AKT-NR-PARM ANR-PARM                                  
057200                                                                          
057300       PERFORM UNTIL WSRKOD NOT = SPACE                                   
057400         PERFORM FB-RENSA-SYMBOLER                                        
057500         CALL W980WSPC USING FGETN WSRKOD DATUM-PARM                      
057600               AKT-NR-PARM ANR-PARM                                       
057700       END-PERFORM                                                        
057800                                                                          
057900       CALL W980WSPC USING FDELK WSRKOD DATUM-PARM                        
058000                    AKT-NR-PARM                                           
058100       IF WSRKOD = SPACE                                                  
058200         DISPLAY 'SOP261I  SYMBOL VALUES FOR DATE: ' DISPLAY-DATUM        
058300                              ' CLEARED'                                  
058400       END-IF                                                             
058500       PERFORM FC-BERAEKNA-NAESTA-DATUM                                   
058600     END-PERFORM                                                          
058700     CALL W980WSPC USING FSAVE WSRKOD                                     
058800     .                                                                    
058900     EJECT                                                                
059000 FA-BERAEKNA-RENS-TOM     SECTION.                                        
059100     SKIP2                                                                
059200     IF W-KORT-DATUM < 500000                                             
059300       COMPUTE W-FULLT-DATUM = W-KORT-DATUM + 20000000                    
059400     ELSE                                                                 
059500       COMPUTE W-FULLT-DATUM = W-KORT-DATUM + 19000000                    
059600     END-IF                                                               
059700     COMPUTE W-TOM-DATUM-TAL =                                            
059800         FUNCTION INTEGER-OF-DATE (W-FULLT-DATUM)                         
059900                  - ANTAL-DAGAR-EJ-RENSA                                  
060000     .                                                                    
060100     EJECT                                                                
060200 FB-RENSA-SYMBOLER   SECTION.                                             
060300     SKIP2                                                                
060400     MOVE SPACE TO DATA-VERDE                                             
060500     CALL W980WSPC USING FGETF WSRKOD ANR-PARM SYMB-PARM                  
060600                  DATA-PARM                                               
060700                                                                          
060800     PERFORM UNTIL WSRKOD NOT = SPACE                                     
060900       CALL W980WSPC USING FDELK WSRKOD ANR-PARM DATA-PARM                
061000       MOVE SPACE TO DATA-VERDE                                           
061100       CALL W980WSPC USING FGETN WSRKOD ANR-PARM SYMB-PARM                
061200                  DATA-PARM                                               
061300     END-PERFORM                                                          
061400                                                                          
061500     CALL W980WSPC USING FDELK WSRKOD ANR-PARM SYMB-PARM                  
061600     .                                                                    
061700     EJECT                                                                
061800 FC-BERAEKNA-NAESTA-DATUM  SECTION.                                       
061900     SKIP2                                                                
062000     ADD 1 TO W-FOM-DATUM-TAL                                             
062100     COMPUTE W-FULLT-DATUM =                                              
062200         FUNCTION DATE-OF-INTEGER (W-FOM-DATUM-TAL)                       
062300     MOVE W-FULLT-DATUM TO W-KORT-DATUM                                   
062400     .                                                                    
062500     EJECT                                                                
062600 FD-BERAEKNA-RENS-FOM      SECTION.                                       
062700     SKIP2                                                                
062800     IF W-KORT-DATUM < 500000                                             
062900       COMPUTE W-FULLT-DATUM = W-KORT-DATUM + 20000000                    
063000     ELSE                                                                 
063100       COMPUTE W-FULLT-DATUM = W-KORT-DATUM + 19000000                    
063200     END-IF                                                               
063300     COMPUTE W-FOM-DATUM-TAL =                                            
063400         FUNCTION INTEGER-OF-DATE (W-FULLT-DATUM)                         
063500     .                                                                    
063600     EJECT                                                                
063700 Z-SLUT SECTION.                                                          
063800     SKIP2                                                                
063900     MOVE '9' TO SOP20-KDSOPFUNK                                          
064000     CALL W9802000 USING SOP20-W98020                                     
064100     IF SOP20-KDRET NOT = 0                                               
064200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
064300     END-IF                                                               
064400     .                                                                    
064500     EJECT                                                                
064600*    -COPY WY2000P1                                                       
