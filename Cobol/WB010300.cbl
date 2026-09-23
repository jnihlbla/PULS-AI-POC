000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WB010300.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   04/02/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN:       WB0103                                                   
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        LÄSER TRANS WB0103T OCH STARTAR BATCH BMP                        
001200*                                                                         
001300*        PROGRAMMET LÄSER      TABELL TB1ACCE                             
001400*        PROGRAMMET LÄSER      WDK611                                     
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: WB0103T från                                        
001800*        REQUEST:     WB0103I1 "carparts.acce.createxlsfile*              
001900*                 med WZ01REQU                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        RESPONSE:    WB0103O1 "carparts.acce.createxlsfile"              
002300*                 med WZ01RESP                                            
002400*                                                                         
002500*        REQUEST:     WZ01SOP  "CARPARTS.PULS.SOP"                        
002600*                 med WZ01SEND                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WB010300'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  FILLER                      PIC X(16) VALUE 'DB2-SEKTION ='.         
004600 77  DB2-SEKTION                 PIC X(80) VALUE SPACE.                   
004700                                                                          
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  POS                         PIC S9(4)   VALUE ZERO BINARY.           
005300                                                                          
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005800                                                                          
005900                                                                          
006000 77  SW-WDK611                   PIC X       VALUE 'N'.                   
006100     88  WDK611                              VALUE 'J'.                   
006200                                                                          
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     SKIP3                                                                
007300*    --- PARAMETRAR TILL ABEND                                            
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007900     SKIP3                                                                
008000 01  MESSAGE-CODES.                                                       
008100     03  ERR-INVALID-KEY-FLD-0   PIC X(3)    VALUE '022'.                 
008200     03  ERR-0-MUST-BE-NUMERIC   PIC X(3)    VALUE '024'.                 
008300     03  ERR-0-NOT-FOUND         PIC X(3)    VALUE '025'.                 
008400     03  ERR-KEYS-NOT-ENTERED    PIC X(3)    VALUE '026'.                 
008500     03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
008600     03  ERR-SYSTEM-ERROR        PIC X(3)    VALUE '099'.                 
008700     03  ERR-WRONG-DATE          PIC X(3)    VALUE '102'.                 
008800     03  ERR-LINES-OVERFLOW      PIC X(3)    VALUE '107'.                 
008900     03  ERR-BAD-COMBINATION     PIC X(3)    VALUE '109'.                 
009000     03  INF-FILE-CREATED        PIC X(3)    VALUE '111'.                 
009100     EJECT                                                                
009200                                                                          
009300*    --- SELECTION DATA FROM JSP PAGE                                     
009400                                                                          
009500 01  WS-AAVVD-AREA.                                                       
009600     03  WS-TIAOINF-1.                                                    
009700         05  WS-TIAOINF-SEK      PIC 9(2).                                
009800         05  WS-TIAOINF-AAVV     PIC 9(4).                                
009900         05  WS-TIAOINF-D        PIC 9.                                   
010000                                                                          
010100     03  WS-TIAOINF-2  REDEFINES WS-TIAOINF-1.                            
010200         05  WS-TIAOINF-AAAAVV   PIC 9(6).                                
010300         05  FILLER              PIC 9(1).                                
010400                                                                          
010500     03  WS-TIAOINF-3  REDEFINES WS-TIAOINF-1.                            
010600         05  FILLER              PIC 9(2).                                
010700         05  WS-TIAOINF-AAVVD    PIC 9(5).                                
010800                                                                          
010900 77  W-TIAOINF-FOM               PIC S9(7) COMP-3 VALUE +0200101.         
011000 77  W-TIAOINF-TOM               PIC S9(7) COMP-3 VALUE +0202052.         
011100 77  WS-TIAOINF-X                PIC 9(6)  VALUE ZERO.                    
011200 77  W-IDUPPDSU                  PIC X(8)  VALUE SPACE.                   
011300 77  W-IDUPPDKU                  PIC X(8)  VALUE SPACE.                   
011400*77  W-IDPROJUP                  PIC X(8)  VALUE SPACE.                   
011500 01  WS-KVRADER-TOTAL            PIC S9(5) VALUE ZERO COMP-3.             
011600 01  WS-KVRADER-X                PIC ZZZZ9.                               
011700                                                                          
011800 01  WS-ADDISPABS                PIC X(50)                                
011900     VALUE 'CARPARTS.ACCE.CREATEXLSFILE'.                                 
012000                                                                          
012100 01  WS-FILDEL-IDUSER         PIC X(8)  VALUE SPACE.                      
012200 01  WS-FILDEL-AAMMDD         PIC 9(6)  VALUE ZERO.                       
012300 01  WS-FILDEL-HHMMSS         PIC 9(6)  VALUE ZERO.                       
012400 01  WS-FILDEL-SUFFIX         PIC X(3)  VALUE 'xls'.                      
012500     EJECT                                                                
012600                                                                          
012700 01  WS-CALLDB2-AREA.                                                     
012800     03 CALL-TABELL.                                                      
012900        05 CALL-TIAOINF          PIC X     VALUE 'N'.                     
013000        05 CALL-IDUPPDKU         PIC X     VALUE 'N'.                     
013100        05 CALL-IDUPPDSU         PIC X     VALUE 'N'.                     
013200                                                                          
013300     03 WS-CALL-TYPE-DB2 REDEFINES CALL-TABELL PIC XXX.                   
013400*                     -- på SU                                            
013500        88 CALL-TYPE-1                      VALUE 'NNJ'.                  
013600*                     -- på KU                                            
013700        88 CALL-TYPE-2                      VALUE 'NJN'.                  
013800*                     -- på KU+SU                                         
013900        88 CALL-TYPE-3                      VALUE 'NJJ'.                  
014000*                     -- på ÄO                                            
014100        88 CALL-TYPE-4                      VALUE 'JNN'.                  
014200*                     -- på ÄO+SU                                         
014300        88 CALL-TYPE-5                      VALUE 'JNJ'.                  
014400*                     -- på ÄO+KU                                         
014500        88 CALL-TYPE-6                      VALUE 'JJN'.                  
014600*                     -- på ÄO+KU+SU                                      
014700        88 CALL-TYPE-7                      VALUE 'JJJ'.                  
014800     EJECT                                                                
014900                                                                          
015000*01  -COPY WDATAREA                                                       
015100     EJECT                                                                
015200*                                                                         
015300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015400     SKIP3                                                                
015500*01  -COPY WZ01SUB                                                        
015600     EJECT                                                                
015700*      ******     DATA SOM SKICKAS TILL SOP i wz01sop                     
015800 01  PARM-TESYMBV.                                                        
015900*      ---------- PARM-1 filnamn                                          
016000     03 FILLER             PIC X(7)  VALUE 'XLSFIL('.                     
016100     03 PARM-FILNAMN-XLS   PIC X(25) VALUE SPACE.                         
016200     03 FILLER             PIC XX    VALUE ')'.                           
016300*      ---------- PARM-2 urval fr.o.m.                                    
016400     03 FILLER             PIC X(12) VALUE 'TIAOINF-FOM('.                
016500     03 PARM-TIAOINF-FOM   PIC 9(6).                                      
016600     03 FILLER             PIC XX    VALUE ') '.                          
016700*      ---------- PARM-3 urval t.o.m.                                     
016800     03 FILLER             PIC X(12) VALUE 'TIAOINF-TOM('.                
016900     03 PARM-TIAOINF-TOM   PIC 9(6).                                      
017000     03 FILLER             PIC XX    VALUE ') '.                          
017100*      ---------- PARM-4                                                  
017200     03 FILLER             PIC X(7)  VALUE 'IDUSER('.                     
017300     03 PARM-IDUSER        PIC X(8).                                      
017400     03 FILLER             PIC XX    VALUE ') '.                          
017500*      ---------- PARM-5                                                  
017600     03 FILLER             PIC X(3)  VALUE 'SU('.                         
017700     03 PARM-IDUPPDSU      PIC X(8).                                      
017800     03 FILLER             PIC XX    VALUE ') '.                          
017900*      ---------- PARM-6                                                  
018000     03 FILLER             PIC X(3)  VALUE 'KU('.                         
018100     03 PARM-IDUPPDKU      PIC X(8).                                      
018200     03 FILLER             PIC XX    VALUE ') '.                          
018300*      ---------- PARM-7                                                  
018400     03 FILLER             PIC X(5)  VALUE 'CALL('.                       
018500     03 PARM-CALL-TYPE     PIC X(3).                                      
018600     03 FILLER             PIC XX    VALUE ') '.                          
018700                                                                          
018800*    03 FILLER             PIC X(9)  VALUE 'IDARTNR('.                    
018900*    03 PARM-IDARTNR       PIC 9(8).                                      
019000*    03 FILLER             PIC XX    VALUE ') '.                          
019100     EJECT                                                                
019200                                                                          
019300 01  FILLER                    PIC X(16) VALUE 'MID-REQU-AREA'.           
019400     SKIP3                                                                
019500 01  MID-REQU-AREA.                                                       
019600*    03  -COPY WZ01REQU                                                   
019700*    03  -COPY WB0103I1                                                   
019800     EJECT                                                                
019900                                                                          
020000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
020100     SKIP3                                                                
020200 01  RESP-AREA.                                                           
020300*    03  -COPY WZ01RESP                                                   
020400*    03  -COPY WB0103O1                                                   
020500     EJECT                                                                
020600                                                                          
020700 01  FILLER                    PIC X(16)   VALUE 'SOP-REQU-AREA'.         
020800*01  SOP-REQU-AREA.                                                       
020900*- - - - - - - - - - - -   - - - PARAMETRAR TILL SOP                      
021000 01  W-PROG-TO-PROG-SW.                                                   
021100*    03  -COPY WMSGSOP                                                    
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021400     SKIP3                                                                
021500 01  NYCKLAR-TILL-DLI.                                                    
021600     03  W-IDARTNR-X.                                                     
021700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021800     03  W-KDSEGKEY-X.                                                    
021900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
022000     EJECT                                                                
022100                                                                          
022200*    --- STATUS-KOD FRÅN IMS                                              
022300 01  FILLER              PIC X(16) VALUE 'IMS-STATUS----->'.              
022400 01  STATUS-WS           PIC XX.                                          
022500     88 SEGMENT-OK      VALUE  IS '  '.                                   
022600     88 SEGMENT-SAKNAS  VALUE  IS 'GE'.                                   
022700     88 PURG-STATUS-FEL VALUES ARE 'AD' 'AL' 'AP' 'AT' 'AZ'               
022800                                   'A3' 'A5' 'QF' 'QH' 'X2'.              
022900     SKIP2                                                                
023000 01  GODK-STATUSKODER.                                                    
023100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023200     SKIP3                                                                
023300 01  SSA1                        PIC X(64).                               
023400 01  SSA2                        PIC X(64).                               
023500     EJECT                                                                
023600*    --- IMS FUNKTIONSKODER                                               
023700*01  -COPY W0003                                                          
023800     EJECT                                                                
023900*    ---  DLI INPUT-OUTPUT AREA                                           
024000                                                                          
024100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
024200 01  DLI-IO-WDK611.                                                       
024300*    03  -COPY WDK611  -PRE WDK6-                                         
024400     EJECT                                                                
024500                                                                          
024600                                                                          
024700                                                                          
024800                                                                          
024900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
025000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025100                                                                          
025200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
025300 01  DB2-WS.                                                              
025400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
025500         88  CURSOR-OK                       VALUE 000.                   
025600         88  RADER-FINNS                     VALUE 000.                   
025700         88  RADER-SAKNAS                    VALUE 100.                   
025800         88  ATKOMST-FEL                     VALUE 904.                   
025900     03  GODK-SQLCODEKODER.                                               
026000         05  GODK-SQLCODE OCCURS 5                                        
026100             INDEXED BY SQLCODE-IX PIC 9(3).                              
026200     EJECT                                                                
026300                                                                          
026400 01  FILLER                      PIC X(16)  VALUE 'TB1ACCE-AREA'.         
026500                                                                          
026600*01  -COPY TB1ACCE -PRE TB1ACCE-                                          
026700     EJECT                                                                
026800                                                                          
026900     EXEC SQL INCLUDE TB1ACCE END-EXEC.                                   
027000     EJECT                                                                
027100                                                                          
027200 LINKAGE SECTION.                                                         
027300*01  -COPY W0009   -PRE MSG-                                              
027400     SKIP2                                                                
027500*01  -COPY W0009   -PRE ALT-                                              
027600     SKIP2                                                                
027700*01  -COPY W0008  -PRE WDK6-                                              
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000                                                                          
028100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDK6-PCB.                      
028200 MAIN SECTION.                                                            
028300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDK6-PCB.                      
028400                                                                          
028500     PERFORM S01-HAEMTA-ANROPSDATA                                        
028600     IF SUB-KDRC = 0                                                      
028700       PERFORM A-INIT                                                     
028800       PERFORM B-KOLLA-NYCKLAR                                            
028900       IF NYCKLAR-OK                                                      
029000         PERFORM C-KOLLA-URVAL                                            
029100         IF WS-KVRADER-TOTAL > ZERO                                       
029200         AND WS-KVRADER-TOTAL <= +65536                                   
029300           PERFORM F-ORDER-BMP-SOP                                        
029400         END-IF                                                           
029500       END-IF                                                             
029600       PERFORM S02-RETURNERA-SVAR                                         
029700     END-IF                                                               
029800                                                                          
029900     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 A-INIT SECTION.                                                          
030400     SKIP2                                                                
030500     INITIALIZE GODK-SQLCODEKODER                                         
030600     MOVE ALL '+' TO RESP-AREA                                            
030700     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
030800                     RESP-IDMSG-INFO                                      
030900                     RESP-IDELMT-ERROR                                    
031000     MOVE '001'   TO RESP-IDMSGVER                                        
031100     .                                                                    
031200     EJECT                                                                
031300 B-KOLLA-NYCKLAR SECTION.                                                 
031400                                                                          
031500     MOVE JA  TO NYCKLAR-SW                                               
031600     MOVE 'NNN' TO CALL-TABELL                                            
031700                                                                          
031800*    MOVE SPACE TO W-IDPROJUP                                             
031810     MOVE SPACE TO PARM-IDUPPDSU                                          
031820     MOVE SPACE TO PARM-IDUPPDKU                                          
031900                                                                          
032000     IF REQU-TIAOINF-AAVV-FOM-KEY NOT = ALL '+' AND SPACE                 
032300*************************   CHECK TIAOINF-FOM ***********                 
032400       IF REQU-TIAOINF-AAVV-FOM-KEY NUMERIC                               
032500                                                                          
032600         MOVE REQU-TIAOINF-AAVV-FOM-KEY TO WS-TIAOINF-AAVV                
032700         MOVE 1                         TO WS-TIAOINF-D                   
032800         MOVE WS-TIAOINF-AAVVD          TO DAT-I-TIDATUM                  
032900                                                                          
033000                                                                          
033100         MOVE 'AAVVD' TO DAT-KDDATFORM                                    
033200         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
033300                             DAT-O-TIDATUM DAT-KDSVAR                     
033400         IF DAT-KDSVAR-OK                                                 
033500           MOVE DAT-TISEKEL        TO WS-TIAOINF-SEK                      
033600           MOVE WS-TIAOINF-AAAAVV  TO W-TIAOINF-FOM                       
033700           MOVE JA                 TO CALL-TIAOINF                        
033800*           --- kolla IDPROJUP                                            
033900*  ÄT 07:7  IDPROJUP SKA INTE LÄNGRE ANVÄNDAS                             
034000*                           FÖR BEGRÄNSNING                               
034100*          IF REQU-IDPROJUP-KEY NOT = SPACE AND ALL '+'                   
034200*            -- REMOVE ANY ERRONEUS LEADING SPACE                         
034300*            MOVE +1 TO POS                                               
034400                                                                          
034500*            INSPECT REQU-IDPROJUP-KEY TALLYING POS FOR                   
034600*            LEADING ' '                                                  
034700                                                                          
034800*            UNSTRING REQU-IDPROJUP-KEY INTO W-IDPROJUP                   
034900*            WITH POINTER POS                                             
035000*          END-IF                                                         
035100         ELSE                                                             
035200           MOVE NEJ TO NYCKLAR-SW                                         
035300           MOVE ERR-WRONG-DATE     TO RESP-IDMSG-ERROR                    
035400           MOVE 'TIAOINF-AAVV-FOM' TO RESP-IDELMT-ERROR                   
035500         END-IF                                                           
035600       ELSE                                                               
035700         MOVE NEJ TO NYCKLAR-SW                                           
035800         MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
035900         MOVE 'TIAOINF-AAVV-FOM'    TO RESP-IDELMT-ERROR                  
036000       END-IF                                                             
036100                                                                          
036200*************** CHECK RELATIVE VALUE IN TIAOINF-TOM ***********           
036300       IF NYCKLAR-OK                                                      
036400         INSPECT REQU-TIAOINF-AAVV-TOM-KEY                                
036500                                      REPLACING ALL SPACE BY ZERO         
036600         IF REQU-TIAOINF-AAVV-TOM-KEY = ALL '+' OR SPACE                  
036700*            --- USE THE SAME WEEK AS IN FOM-KEY                          
036800             MOVE WS-TIAOINF-AAAAVV TO W-TIAOINF-TOM                      
036900             MOVE WS-TIAOINF-AAAAVV (3:)                                  
037000                                    TO RESP-TIAOINF-AAVV-TOM-KEY          
037100                                       REQU-TIAOINF-AAVV-TOM-KEY          
037200         ELSE                                                             
037300           IF REQU-TIAOINF-AAVV-TOM-KEY NUMERIC                           
037400             IF REQU-TIAOINF-AAVV-TOM-KEY                                 
037500              < REQU-TIAOINF-AAVV-FOM-KEY                                 
037600                 MOVE WS-TIAOINF-AAAAVV (3:)                              
037700                                    TO RESP-TIAOINF-AAVV-TOM-KEY          
037800                                       REQU-TIAOINF-AAVV-TOM-KEY          
037900             END-IF                                                       
038000           END-IF                                                         
038100         END-IF                                                           
038200                                                                          
038300         IF REQU-TIAOINF-AAVV-TOM-KEY NOT = ALL '+' AND SPACE             
038400           IF REQU-TIAOINF-AAVV-TOM-KEY NUMERIC                           
038500             MOVE REQU-TIAOINF-AAVV-TOM-KEY TO WS-TIAOINF-AAVV            
038600             MOVE 5              TO WS-TIAOINF-D                          
038700             MOVE WS-TIAOINF-AAVVD TO DAT-I-TIDATUM                       
038800                                                                          
038900                                                                          
039000             MOVE 'AAVVD' TO DAT-KDDATFORM                                
039100             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
039200                                 DAT-O-TIDATUM DAT-KDSVAR                 
039300                                                                          
039400             IF DAT-KDSVAR-OK                                             
039500               MOVE DAT-TISEKEL    TO WS-TIAOINF-SEK                      
039600               MOVE WS-TIAOINF-AAAAVV TO W-TIAOINF-TOM                    
039700             ELSE                                                         
039800               MOVE NEJ TO NYCKLAR-SW                                     
039900               MOVE ERR-WRONG-DATE   TO RESP-IDMSG-ERROR                  
040000               MOVE 'TIAOINF-AAVV-TOM' TO RESP-IDELMT-ERROR               
040100             END-IF                                                       
040200           ELSE                                                           
040300             MOVE NEJ TO NYCKLAR-SW                                       
040400             MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR               
040500             MOVE 'TIAOINF-AAVV-TOM'  TO RESP-IDELMT-ERROR                
040600           END-IF                                                         
041100         END-IF                                                           
041200       END-IF                                                             
041210     END-IF                                                               
041300* SU + KU                                                                 
041310     IF (REQU-IDUPPDSU-KEY  NOT = ALL '+' AND SPACE)                      
041700       INSPECT REQU-IDUPPDSU-KEY REPLACING LEADING SPACE BY ZERO          
041800       IF REQU-IDUPPDSU-KEY NUMERIC                                       
041900         MOVE REQU-IDUPPDSU-KEY TO W-IDUPPDSU                             
042000         MOVE JA                TO CALL-IDUPPDSU                          
042100       ELSE                                                               
042101         MOVE NEJ TO NYCKLAR-SW                                           
042102         MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
042103         MOVE 'SU     '        TO RESP-IDELMT-ERROR                       
042200       END-IF                                                             
042210     END-IF                                                               
042211     IF (REQU-IDUPPDKU-KEY  NOT = ALL '+' AND SPACE)                      
042220       INSPECT REQU-IDUPPDKU-KEY REPLACING LEADING SPACE BY ZERO          
042230       IF REQU-IDUPPDKU-KEY NUMERIC                                       
042240         MOVE REQU-IDUPPDKU-KEY TO W-IDUPPDKU                             
042250         MOVE JA                TO CALL-IDUPPDKU                          
042260       ELSE                                                               
042270         MOVE NEJ TO NYCKLAR-SW                                           
042280         MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
042290         MOVE 'KU     '        TO RESP-IDELMT-ERROR                       
042291       END-IF                                                             
042292     END-IF                                                               
042400                                                                          
043600     EVALUATE TRUE                                                        
043700       WHEN CALL-TYPE-1  CONTINUE                                         
043800       WHEN CALL-TYPE-2  CONTINUE                                         
043900       WHEN CALL-TYPE-3  CONTINUE                                         
044000       WHEN CALL-TYPE-4  CONTINUE                                         
044100       WHEN CALL-TYPE-5  CONTINUE                                         
044200       WHEN CALL-TYPE-6  CONTINUE                                         
044300       WHEN CALL-TYPE-7  CONTINUE                                         
044400       WHEN OTHER        MOVE NEJ TO NYCKLAR-SW                           
044500             MOVE ERR-KEYS-NOT-ENTERED TO RESP-IDMSG-ERROR                
044600             MOVE       'KEY'         TO RESP-IDELMT-ERROR                
044700     END-EVALUATE                                                         
044900     .                                                                    
045000     EJECT                                                                
045100                                                                          
045200 C-KOLLA-URVAL    SECTION.                                                
045300     SKIP2                                                                
045400     MOVE CALL-TABELL  TO PARM-CALL-TYPE                                  
045500                                                                          
045600     EVALUATE TRUE                                                        
045700       WHEN CALL-TYPE-1  PERFORM DB2-GET-TOTAL-LINES-1                    
045800       WHEN CALL-TYPE-2  PERFORM DB2-GET-TOTAL-LINES-2                    
045900       WHEN CALL-TYPE-3  PERFORM DB2-GET-TOTAL-LINES-3                    
046000       WHEN CALL-TYPE-4  PERFORM DB2-GET-TOTAL-LINES-4                    
046100       WHEN CALL-TYPE-5  PERFORM DB2-GET-TOTAL-LINES-5                    
046200       WHEN CALL-TYPE-6  PERFORM DB2-GET-TOTAL-LINES-6                    
046300       WHEN CALL-TYPE-7  PERFORM DB2-GET-TOTAL-LINES-7                    
046400     END-EVALUATE                                                         
046500                                                                          
046800     IF RADER-FINNS                                                       
046801       CONTINUE                                                           
046810*      -- Denna kod används inte efter ÄT 07:7                            
046820*                                                                         
046900*      PERFORM DB2-DCL-OPN-TB1ACCE-CRS                                    
048000*      IF CURSOR-OK                                                       
048100*        PERFORM DB2-FETCH-TB1ACCE-CRS                                    
048300*        PERFORM UNTIL RADER-SAKNAS                                       
048310*          -- ÄT 07:7  IDPROJUP ska inte längre användas                  
048320*          --                   för begränsning                           
048400*          PERFORM CA-KOLLA-WDK6                                          
048500*          IF ( W-IDPROJUP = SPACE )                                      
048600*          OR ( WDK611 AND                                                
048700*             ( WDK6-CLAG-IDPROJUP = W-IDPROJUP ) )                       
048800*             CONTINUE                                                    
048900*          ELSE                                                           
049000*            SUBTRACT 1 FROM WS-KVRADER-TOTAL                             
049100*            -- Denna artikel är utanför urvalet!                         
049200*          END-IF                                                         
049300*          PERFORM DB2-FETCH-TB1ACCE-CRS                                  
049400*        END-PERFORM                                                      
049500*      ELSE                                                               
049600*        MOVE ERR-SYSTEM-ERROR    TO RESP-IDMSG-ERROR                     
049700*        MOVE 'TB1ACCE    '     TO RESP-IDELMT-ERROR                      
049800*      END-IF                                                             
049900     ELSE                                                                 
050000       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
050100       MOVE 'KEY        '       TO RESP-IDELMT-ERROR                      
050200     END-IF                                                               
050300*                                                                         
050400     IF WS-KVRADER-TOTAL = ZERO                                           
050500       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
050600       MOVE 'TB1ACCE   '       TO RESP-IDELMT-ERROR                       
050700     ELSE                                                                 
050800       IF WS-KVRADER-TOTAL > +65536                                       
050900         MOVE ERR-LINES-OVERFLOW  TO RESP-IDMSG-ERROR                     
051000         MOVE 'KEY        '       TO RESP-IDELMT-ERROR                    
051100       END-IF                                                             
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051500                                                                          
051600*CA-KOLLA-WDK6    SECTION.                                                
051700*    SKIP2                                                                
051800*    MOVE TB1ACCE-IDARTNR TO W-IDARTNR                                    
051900*                                                                         
052000*    MOVE Nej TO SW-WDK611                                                
052100*                                                                         
052200*                                                                         
052300*    PERFORM DLI-GU-WDK611                                                
052400*    IF SEGMENT-OK                                                        
052500*      SET WDK611 TO TRUE                                                 
052600*    END-IF                                                               
052700*    .                                                                    
052800                                                                          
052900     EJECT                                                                
053000 F-ORDER-BMP-SOP  SECTION.                                                
053100     SKIP2                                                                
053200     PERFORM FA-FYLL-PARM-TESYMBV                                         
053300                                                                          
053400     MOVE 'B103'           TO MSGSOP-IDTRANS                              
053500     MOVE '2'              TO MSGSOP-KDMFSFOR                             
053600     MOVE 'WB11S1BLK'      TO MSGSOP-IDPROCESS                            
053700     MOVE 'O'              TO MSGSOP-KDSOPFUNK                            
053800     MOVE SPACE            TO MSGSOP-TESYMBV                              
053900     PERFORM IMS-INSERT-ALT-MSG                                           
054000                                                                          
054100     MOVE 'B103 '          TO MSGSOP-IDTRANS                              
054200     MOVE '2'              TO MSGSOP-KDMFSFOR                             
054300     MOVE 'WB11S1  '       TO MSGSOP-IDPROCESS                            
054400     MOVE 'O'              TO MSGSOP-KDSOPFUNK                            
054500     MOVE PARM-TESYMBV     TO MSGSOP-TESYMBV                              
054600                                                                          
054700     PERFORM IMS-INSERT-ALT-MSG                                           
054800                                                                          
054900     IF SEGMENT-OK                                                        
055000       MOVE INF-FILE-CREATED   TO RESP-IDMSG-INFO                         
055100                                                                          
055200       MOVE WS-KVRADER-TOTAL                                              
055300         TO WS-KVRADER-X  MOVE WS-KVRADER-X                               
055400                               TO RESP-KVRADER                            
055500       MOVE PARM-FILNAMN-XLS   TO RESP-IDDSN                              
055600     ELSE                                                                 
055700       IF PURG-STATUS-FEL                                                 
055800         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
055900         PERFORM S02-RETURNERA-SVAR                                       
056000         STRING ' FEL STATUS FRÅN IMS: =' STATUS-WS                       
056100         '. SSA1=' SSA1(1:19) '. SSA2=' SSA2(1:19)                        
056200         DELIMITED BY SIZE INTO FELTEXT                                   
056300         CALL FELLOG                                                      
056400       END-IF                                                             
056500     END-IF                                                               
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 FA-FYLL-PARM-TESYMBV  SECTION.                                           
057000     SKIP2                                                                
057100     MOVE REQU-IDUSER TO WS-FILDEL-IDUSER                                 
057200                              PARM-IDUSER                                 
057300                                                                          
057400     IF WS-FILDEL-IDUSER = SPACE                                          
057500       MOVE 'x' TO WS-FILDEL-IDUSER                                       
057600     END-IF                                                               
057700                                                                          
057800     MOVE FUNCTION CURRENT-DATE(3:6)                                      
057900                          TO WS-FILDEL-AAMMDD                             
058000     MOVE FUNCTION CURRENT-DATE(9:6)                                      
058100                          TO WS-FILDEL-HHMMSS                             
058200                                                                          
058300     MOVE W-TIAOINF-FOM    TO WS-TIAOINF-X                                
058400     MOVE WS-TIAOINF-X     TO PARM-TIAOINF-FOM                            
058500                                                                          
058600     MOVE W-TIAOINF-TOM    TO WS-TIAOINF-X                                
058700     MOVE WS-TIAOINF-X     TO PARM-TIAOINF-TOM                            
058701                                                                          
058710     MOVE W-IDUPPDKU       TO PARM-IDUPPDKU                               
058720     MOVE W-IDUPPDSU       TO PARM-IDUPPDSU                               
058800                                                                          
058900*    MOVE W-IDPROJUP       TO PARM-IDPROJUP                               
059000                                                                          
059100     MOVE SPACE TO PARM-FILNAMN-XLS                                       
059200     STRING  WS-FILDEL-IDUSER         DELIMITED BY SPACE                  
059300             '.'                      DELIMITED BY SIZE                   
059400             WS-FILDEL-AAMMDD         DELIMITED BY SIZE                   
059500             '.'                      DELIMITED BY SIZE                   
059600             WS-FILDEL-HHMMSS         DELIMITED BY SIZE                   
059700             '.'                      DELIMITED BY SIZE                   
059800             WS-FILDEL-SUFFIX         DELIMITED BY SIZE                   
059900             INTO      PARM-FILNAMN-XLS                                   
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300*    --- DISPATCHER-SEKTIONER                                             
060400 S01-HAEMTA-ANROPSDATA SECTION.                                           
060500                                                                          
060600     MOVE 'GETARG'                TO SUB-KDFUNC                           
060700     MOVE WS-ADDISPABS            TO SUB-ADDISPABS                        
060800     MOVE LENGTH OF MID-REQU-AREA TO SUB-KVDLEN                           
060900                                                                          
061000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN MID-REQU-AREA         
061100                                                                          
061200     IF SUB-KDRC > 0                                                      
061300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
061400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
061500       DELIMITED BY SIZE INTO FELTEXT                                     
061600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
061700     END-IF                                                               
061800     .                                                                    
061900     SKIP3                                                                
062000 S02-RETURNERA-SVAR SECTION.                                              
062100                                                                          
062200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
062300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
062400                                                                          
062500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
062600                                                                          
062700     IF SUB-KDRC > 0                                                      
062800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
062900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
063000       DELIMITED BY SIZE INTO FELTEXT                                     
063100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 IMS-INSERT-ALT-MSG SECTION.                                              
063600                                                                          
063700     MOVE SPACE TO GODK-STATUSKODER                                       
063800                                                                          
063900     CALL CBLTDLI USING PURG   ALT-PCB   W-PROG-TO-PROG-SW                
064000                                                                          
064100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
064200                                                                          
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500     EJECT                                                                
064600*                                                                         
064700*DLI-GU-WDK611 SECTION.                                                   
064800*                                                                         
064900*    STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
065000*         DELIMITED BY SIZE INTO SSA1                                     
065100*    STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
065200*         DELIMITED BY SIZE INTO SSA2                                     
065300*    MOVE '  GE' TO GODK-STATUSKODER                                      
065400*    CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
065500*    MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
065600*    PERFORM IMS-STATUSKONTROLL                                           
065700*    .                                                                    
065800                                                                          
065900     EJECT                                                                
066000 DB2-GET-TOTAL-LINES-1    SECTION.                                        
066100     EXEC SQL                                                             
066200         SELECT COUNT(*)                                                  
066300         INTO :WS-KVRADER-TOTAL                                           
066400         FROM TB1ACCE                                                     
066500         WHERE ( IDUPPDSU = :W-IDUPPDSU )                                 
066600     END-EXEC                                                             
066700     MOVE 000100  TO GODK-SQLCODEKODER                                    
066800     MOVE SQLCODE TO SQLCODE-WS                                           
066900     PERFORM DB2-STATUS-KONTROLL                                          
067000     .                                                                    
067100     EJECT                                                                
067200                                                                          
067300 DB2-GET-TOTAL-LINES-2    SECTION.                                        
067400     EXEC SQL                                                             
067500         SELECT COUNT(*)                                                  
067600         INTO :WS-KVRADER-TOTAL                                           
067700         FROM TB1ACCE                                                     
067800         WHERE ( IDUPPDKU = :W-IDUPPDKU )                                 
067900     END-EXEC                                                             
068000     MOVE 000100  TO GODK-SQLCODEKODER                                    
068100     MOVE SQLCODE TO SQLCODE-WS                                           
068200     PERFORM DB2-STATUS-KONTROLL                                          
068300     .                                                                    
068400     EJECT                                                                
068500                                                                          
068600 DB2-GET-TOTAL-LINES-3    SECTION.                                        
068700     EXEC SQL                                                             
068800         SELECT COUNT(*)                                                  
068900         INTO :WS-KVRADER-TOTAL                                           
069000         FROM TB1ACCE                                                     
069100         WHERE ( IDUPPDKU = :W-IDUPPDKU                                   
069200           AND   IDUPPDSU = :W-IDUPPDSU )                                 
069300     END-EXEC                                                             
069400     MOVE 000100  TO GODK-SQLCODEKODER                                    
069500     MOVE SQLCODE TO SQLCODE-WS                                           
069600     PERFORM DB2-STATUS-KONTROLL                                          
069700     .                                                                    
069800     EJECT                                                                
069900                                                                          
070000 DB2-GET-TOTAL-LINES-4    SECTION.                                        
070100     EXEC SQL                                                             
070200         SELECT COUNT(*)                                                  
070300         INTO :WS-KVRADER-TOTAL                                           
070400         FROM TB1ACCE                                                     
070500         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
070600           AND   TIAOINF <= :W-TIAOINF-TOM  )                             
070700     END-EXEC                                                             
070800     MOVE 000100  TO GODK-SQLCODEKODER                                    
070900     MOVE SQLCODE TO SQLCODE-WS                                           
071000     PERFORM DB2-STATUS-KONTROLL                                          
071100     .                                                                    
071200     EJECT                                                                
071300                                                                          
071400 DB2-GET-TOTAL-LINES-5    SECTION.                                        
071500     EXEC SQL                                                             
071600         SELECT COUNT(*)                                                  
071700         INTO :WS-KVRADER-TOTAL                                           
071800         FROM TB1ACCE                                                     
071900         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
072000           AND   TIAOINF <= :W-TIAOINF-TOM                                
072100           AND  IDUPPDSU  = :W-IDUPPDSU    )                              
072200     END-EXEC                                                             
072300     MOVE 000100  TO GODK-SQLCODEKODER                                    
072400     MOVE SQLCODE TO SQLCODE-WS                                           
072500     PERFORM DB2-STATUS-KONTROLL                                          
072600     .                                                                    
072700     EJECT                                                                
072800                                                                          
072900 DB2-GET-TOTAL-LINES-6    SECTION.                                        
073000     EXEC SQL                                                             
073100         SELECT COUNT(*)                                                  
073200         INTO :WS-KVRADER-TOTAL                                           
073300         FROM TB1ACCE                                                     
073400         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
073500           AND   TIAOINF <= :W-TIAOINF-TOM                                
073600           AND  IDUPPDKU  = :W-IDUPPDKU    )                              
073700     END-EXEC                                                             
073800     MOVE 000100  TO GODK-SQLCODEKODER                                    
073900     MOVE SQLCODE TO SQLCODE-WS                                           
074000     PERFORM DB2-STATUS-KONTROLL                                          
074100     .                                                                    
074200     EJECT                                                                
074300*                                                                         
074400 DB2-GET-TOTAL-LINES-7    SECTION.                                        
074500     EXEC SQL                                                             
074600         SELECT COUNT(*)                                                  
074700         INTO :WS-KVRADER-TOTAL                                           
074800         FROM TB1ACCE                                                     
074900         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
075000           AND   TIAOINF <= :W-TIAOINF-TOM                                
075100           AND  IDUPPDKU  = :W-IDUPPDKU                                   
075200           AND  IDUPPDSU  = :W-IDUPPDSU    )                              
075300     END-EXEC                                                             
075400     MOVE 000100  TO GODK-SQLCODEKODER                                    
075500     MOVE SQLCODE TO SQLCODE-WS                                           
075600     PERFORM DB2-STATUS-KONTROLL                                          
075700     .                                                                    
075800     EJECT                                                                
075900*                                                                         
076000*DB2-DCL-OPN-TB1ACCE-CRS  SECTION.                                        
076100*    SKIP2                                                                
076200*    MOVE 'DB2-DCL-OPN-TB1ACCE-CRS      ' TO DB2-SEKTION                  
076300*    EXEC SQL                                                             
076400*         DECLARE TB1ACCE-CRS CURSOR FOR                                  
076500*         SELECT                                                          
076600*            IDARTNR                                                      
076700*           ,BEART                                                        
076800*           ,IDUPPDSU                                                     
076900*           ,IDUPPDKU                                                     
077000*           ,IDAOT                                                        
077100*           ,TIAOINF                                                      
077200*         FROM   TB1ACCE                                                  
077300*         WHERE ( TIAOINF >= :W-TIAOINF-FOM                               
077400*           AND  TIAOINF <= :W-TIAOINF-TOM )                              
077500*         ORDER BY IDARTNR                                                
077600*         FOR FETCH ONLY                                                  
077700*    END-EXEC                                                             
077800*    MOVE 000100  TO GODK-SQLCODEKODER                                    
077900*    EXEC SQL                                                             
078000*       OPEN TB1ACCE-CRS                                                  
078100*    END-EXEC                                                             
078200*    MOVE SQLCODE TO SQLCODE-WS                                           
078300*    PERFORM DB2-STATUS-KONTROLL                                          
078400*    .                                                                    
078500     EJECT                                                                
078600*                                                                         
078700*DB2-FETCH-TB1ACCE-CRS  SECTION.                                          
078800*    SKIP2                                                                
078900*    MOVE 'DB2-FETCH-TB1ACCE-CRS        ' TO DB2-SEKTION                  
079000*    MOVE 000100  TO GODK-SQLCODEKODER                                    
079100*    EXEC SQL                                                             
079200*        FETCH TB1ACCE-CRS                                                
079300*        INTO                                                             
079400*            :TB1ACCE-IDARTNR                                             
079500*           ,:TB1ACCE-BEART                                               
079600*           ,:TB1ACCE-IDUPPDSU                                            
079700*           ,:TB1ACCE-IDUPPDKU                                            
079800*           ,:TB1ACCE-IDAOT                                               
079900*           ,:TB1ACCE-TIAOINF                                             
080000*    END-EXEC                                                             
080100*    MOVE SQLCODE TO SQLCODE-WS                                           
080200*    PERFORM DB2-STATUS-KONTROLL                                          
080300*    .                                                                    
080400     EJECT                                                                
080500 DB2-STATUS-KONTROLL  SECTION.                                            
080600                                                                          
080700     SET SQLCODE-IX TO 1                                                  
080800     SEARCH GODK-SQLCODE                                                  
080900       AT END                                                             
081000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
081100          DELIMITED BY SIZE INTO FELTEXT                                  
081200          CALL ABEND USING RKOD-ABEND-DB2                                 
081300       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
081400     END-SEARCH                                                           
081500     .                                                                    
081600 IMS-STATUSKONTROLL SECTION.                                              
081700                                                                          
081800     SET STATUS-IX TO 1                                                   
081900     SEARCH GODK-STATUS                                                   
082000       AT END                                                             
082100         STRING ' FEL STATUS FRÅN IMS: =' STATUS-WS                       
082200         '. SSA1=' SSA1(1:19) '. SSA2=' SSA2(1:19)                        
082300         DELIMITED BY SIZE INTO FELTEXT                                   
082400         CALL FELLOG                                                      
082500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
082600     END-SEARCH                                                           
082700     .                                                                    
