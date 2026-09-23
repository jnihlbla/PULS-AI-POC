000100*PROCESS DYNAM                                                            
000200*                                                                         
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     WF201300.                                                
000500 AUTHOR.         LUNDH BERNT.                                             
000600 DATE-WRITTEN.   02/04/24.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        CREATES V.A.T. DATA FILE FROM INVOICING DB2-TABLES.              
001200*                                                                         
001300*        PGM READS                                                        
001400*        - DB2-TABLE T01PROC                                              
001500*        - DB2-TABLE T01DHEA                                              
001600*        - DB2-TABLE T01DLIN                                              
001700*        - DB2-TABLE T01DOTY                                              
001800*        - DB2-TABLE T01INRE                                              
001900*        - DB2-TABLE T01FCUS                                              
002000*        - DB2-TABLE T01CURR                                              
002100*        - DB2-TABLE T01LSEL                                              
002200*                                                                         
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- V.A.T. DATA FILE                                           
003100     SELECT WF2013                     ASSIGN TO WF2013D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  WF2013                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST-WF2013 -COPY WF2013   -L.                                       
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004600 77  ERROR-TEXT                   PIC X(80)  VALUE SPACE.                 
004700                                                                          
004800* CONSTANTS.                                                              
004900 77  IDPGM                       PIC X(8)    VALUE 'WF201300'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  WS-CURRENT-VERSION          PIC S9(3)   VALUE +001 COMP-3.           
005300 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
005400 77  WS-IDSYSTEM                 PIC X(4)    VALUE 'WF02'.                
005500 77  WS-IDLEGSEL-CRS             PIC X(4).                                
005600 77  WS-DAGENS-DATUM             PIC X(8)    VALUE '00000000'.            
005700 77  WS-DAGENS-DATUM-NUM         PIC 9(8).                                
005800 77  WS-DASTADAT-KEY             PIC X(8)    VALUE SPACE.                 
005810 77  WS-DASTADAT-CREDIT          PIC X(8)    VALUE SPACE.                 
005820 77  WS2-DASTADAT-CREDIT         PIC X(8)    VALUE SPACE.                 
005900 77  WS-SPAR-IDFINDOC            PIC S9(9)   COMP-3 VALUE 0.              
006000                                                                          
006100* WORKING-FIELDS.                                                         
006200 01  WS-MISC-MULTIFETCH.                                                  
006300     03 WS-DATUM                  PIC X(8)   VALUE SPACE.                 
006400     03 WS-KLOCKAN                PIC 9(10)  VALUE ZERO.                  
006500     03 WS-MX                     PIC S9(3)  COMP-3.                      
006600     03 WS-MULTIFETCH             PIC S9(3)  COMP-3.                      
006700     03 WS-IDLEGSEL               PIC X(4).                               
006800     03 WS-KDVALISO               PIC X(4).                               
006900     03 WS-IDDISTR                PIC X(4).                               
007000                                                                          
007100 01  WS-MISC-TABLES.                                                      
007200     03 WS-TAB-DAFINDOC   OCCURS 100     PIC X(8).                        
007300     03 WS-TAB-IDEXCUST-3 OCCURS 100     PIC X(15).                       
007400     03 WS-TAB-FCUS-KDVALISO OCCURS 100  PIC X(3).                        
007410     03 WS-TAB-IDLEVNR    OCCURS 100     PIC X(5).                        
007500                                                                          
007600 01  GENERAL-SUBPROGRAMS.                                                 
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007800                                                                          
007900*    --- PARAMETERS TO ABEND                                              
008000                                                                          
008100                                                                          
008200                                                                          
008300 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
008400*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
008500                                                                          
008600 01  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 01  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 01  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
009000     EJECT                                                                
009100                                                                          
009200*    --- WORK-AREAS FOR OUTPUT-FILE                                       
009300 01  OUTPUT-AREA-T               PIC X(24)   VALUE                        
009400                                 'OUTPUT-TAB   '.                         
009500*01  -COPY WF2013T                                                        
009600     EJECT                                                                
009700 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
009800                                 'OUTPUT-AREA  '.                         
009900*01  -COPY WF2013  -PRE WS-                                               
010000     EJECT                                                                
010100                                                                          
010200*    --- WORK-AREAS FOR DB2-SECTIONS                                      
010300 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
010400*01  -COPY T01PROC    -PRE PROC-                                          
010500                                                                          
010600 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
010700*01  -COPY T01DHEA    -PRE DHEA-                                          
010800                                                                          
010900 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
011000*01  -COPY T01DLIN    -PRE DLIN-                                          
011100                                                                          
011200 01   FILLER                      PIC X(16)  VALUE 'DOTY-TAB  '.          
011300*01  -COPY T01DOTY    -PRE DOTY-                                          
011400                                                                          
011500 01  FILLER                       PIC X(16)  VALUE 'INRE-TAB   '.         
011600*01  -COPY T01INRE    -PRE INRE-                                          
011700                                                                          
011800 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
011900*01  -COPY T01FCUS    -PRE FCUS-                                          
012000                                                                          
012100 01  FILLER                       PIC X(16)  VALUE 'CURR-TAB   '.         
012200*01  -COPY T01CURR    -PRE CURR-                                          
012300                                                                          
012400 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
012500*01  -COPY T01LSEL    -PRE LSEL-                                          
012600     EJECT                                                                
012700                                                                          
012800 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
012900       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
013000                                                                          
013100 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
013200       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
013300                                                                          
013400 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
013500       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
013600                                                                          
013700 01  FILLER                       PIC X(16)  VALUE 'CUST-AREA'.           
013800       EXEC SQL INCLUDE T01DOTY  END-EXEC.                                
013900                                                                          
014000 01  FILLER                       PIC X(16)  VALUE 'INRE-AREA'.           
014100       EXEC SQL INCLUDE T01INRE  END-EXEC.                                
014200                                                                          
014300 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA'.           
014400       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
014500                                                                          
014600 01  FILLER                       PIC X(16)  VALUE 'CURR-AREA'.           
014700       EXEC SQL INCLUDE T01CURR  END-EXEC.                                
014800                                                                          
014900 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
015000       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
015100     EJECT                                                                
015200                                                                          
015300 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
015400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
015500*                        **** STATUS-CODE FROM DB2                        
015600                                                                          
015700 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
015800 01  DB2-WS.                                                              
015900   03  SQLCODE-WS                 PIC 9(3)   VALUE ZERO.                  
016000     88  LINES-FOUND                         VALUE 000.                   
016100     88  LINES-MISSING                       VALUE 100.                   
016200     88  RESOURCE-WRONG                      VALUE 904.                   
016300   03  GOOD-SQLCODES.                                                     
016400     05  GOOD-SQLCODE OCCURS 5                                            
016500         INDEXED BY SQLCODE-IX    PIC 999.                                
016600     EJECT                                                                
016700                                                                          
016800 PROCEDURE DIVISION.                                                      
016900 MAIN SECTION.                                                            
017000     PERFORM A-INIT                                                       
017100                                                                          
017200     PERFORM DB2-OPEN-CRS-LSEL                                            
017300     PERFORM DB2-FETCH-CRS-LSEL                                           
017400     PERFORM UNTIL LINES-MISSING                                          
017500       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                
017600       MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM            
017700       PERFORM DB2-SELECT-T01PROC                                         
017800       IF PROC-KDBEH = 'P'                                                
017900         SUBTRACT 1 FROM WS-DAGENS-DATUM-NUM                              
018000         MOVE WS-DAGENS-DATUM-NUM       TO WS-DAGENS-DATUM                
018100       END-IF                                                             
018200                                                                          
018300       PERFORM DB2-DCL-OPN-CRS1                                           
018400       PERFORM DB2-FETCH-CRS1                                             
018500       IF SQLERRD(3) > 0                                                  
018600         MOVE 000     TO SQLCODE-WS                                       
018700       END-IF                                                             
018800                                                                          
018900       PERFORM UNTIL LINES-MISSING                                        
019000         MOVE SQLERRD(3)                  TO WS-MULTIFETCH                
019100         MOVE ZERO                        TO WS-MX                        
019200         PERFORM UNTIL WS-MX = WS-MULTIFETCH                              
019300           ADD +1                         TO WS-MX                        
019301**** INT2 SHOULD NOT CREATE VAT REPORT                                    
019302**** INT2 SHOULD CREATE VAT REPORT                                        
019310*          IF VAT-KDFINDOC(WS-MX) = 'INT2'                                
019320*            CONTINUE                                                     
019330*          ELSE                                                           
019400             IF WS-SPAR-IDFINDOC NOT = VAT-IDFINDOC (WS-MX)               
019500               MOVE VAT-IDFINDOC  (WS-MX) TO WS-SPAR-IDFINDOC             
019600               MOVE VAT-IDLEGSEL  (WS-MX) TO WS-IDLEGSEL                  
019700               PERFORM S11-WRITE-WF20X3                                   
019800             END-IF                                                       
019810*          END-IF                                                         
019900         END-PERFORM                                                      
020000         IF WS-MULTIFETCH = 100                                           
020100           PERFORM DB2-FETCH-CRS1                                         
020200           IF SQLERRD(3) > 0                                              
020300             MOVE 000                       TO SQLCODE-WS                 
020400           END-IF                                                         
020500         ELSE                                                             
020600           MOVE 100     TO SQLCODE-WS                                     
020700         END-IF                                                           
020800       END-PERFORM                                                        
020900       PERFORM DB2-CLOSE-CRS1                                             
021000                                                                          
021100       PERFORM DB2-FETCH-CRS-LSEL                                         
021200     END-PERFORM                                                          
021300     PERFORM DB2-CLOSE-CRS-LSEL                                           
021400                                                                          
021500     PERFORM Z-FINIT                                                      
021600     MOVE ZERO TO RETURN-CODE                                             
021700     GOBACK                                                               
021800     .                                                                    
021900                                                                          
022000 A-INIT SECTION.                                                          
022100     OPEN OUTPUT WF2013                                                   
022200                                                                          
022300     INITIALIZE VAT-WF2013T                                               
022400                                                                          
022500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
022600     MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM              
022700     .                                                                    
022800                                                                          
022900 Z-FINIT SECTION.                                                         
023000     CLOSE WF2013                                                         
023100     .                                                                    
023200                                                                          
023300 S11-WRITE-WF20X3 SECTION.                                                
023400     MOVE PROC-DAEXDAT              TO WS-VAT-DAEXDAT                     
023500     MOVE PROC-TIEXTID              TO WS-VAT-TIEXTID                     
023600     MOVE WS-TAB-DAFINDOC   (WS-MX) TO WS-VAT-DAFINDOC                    
023700     MOVE VAT-IDLEGSEL      (WS-MX) TO WS-VAT-IDLEGSEL                    
023800     MOVE VAT-IDVAT-LEG     (WS-MX) TO WS-VAT-IDVAT-LEG                   
023810     IF VAT-KDFINDOC(WS-MX) = 'INT2'                                      
023830       MOVE 'SE556074308901'        TO WS-VAT-IDVAT-RESP                  
023840     ELSE                                                                 
023900       MOVE VAT-IDVAT-RESP  (WS-MX) TO WS-VAT-IDVAT-RESP                  
023910     END-IF                                                               
024000     MOVE VAT-IDVAT-AGENT   (WS-MX) TO WS-VAT-IDVAT-AGENT                 
024100     MOVE VAT-IDVAT-BET     (WS-MX) TO WS-VAT-IDVAT-BET                   
024200     MOVE VAT-IDLANDX3-SEND (WS-MX) TO WS-VAT-IDLANDX3-SEND               
024300     MOVE VAT-IDLANDX3-BET  (WS-MX) TO WS-VAT-IDLANDX3-BET                
024400     MOVE VAT-KDFINDOC      (WS-MX) TO WS-VAT-KDFINDOC                    
024500     MOVE VAT-IDFINDOC      (WS-MX) TO WS-VAT-IDFINDOC                    
024600     MOVE VAT-IDPARTNR      (WS-MX) TO WS-VAT-IDPARTNR                    
024700     MOVE VAT-IDPARTNR      (WS-MX) TO WS-VAT-IDPARTNR                    
024800     MOVE VAT-IDEXCUST-1    (WS-MX) TO WS-VAT-IDEXCUST-1                  
024900     MOVE VAT-IDEXCUST-2    (WS-MX) TO WS-VAT-IDEXCUST-2                  
025000     MOVE VAT-KDVALISO      (WS-MX) TO WS-VAT-KDVALISO                    
025100     MOVE VAT-PRKURS        (WS-MX) TO WS-VAT-PRKURS                      
025200     MOVE VAT-SUNTO-TOT     (WS-MX) TO WS-VAT-SUNTO-TOT                   
025300     MOVE VAT-SUVAT-BILLIT-TOT (WS-MX) TO WS-VAT-SUVAT-BILLIT-TOT         
025400     MOVE SPACE                     TO WS-VAT-IDEXCUST-3                  
025500                                                                          
025600*    WHEN BASE-CURRENCY NOT THE SAME AS CUSTOMERS CURRENCY                
025700     IF VAT-KDVALISO (WS-MX) NOT = WS-TAB-FCUS-KDVALISO (WS-MX)           
025800       MOVE WS-TAB-FCUS-KDVALISO (WS-MX) TO WS-KDVALISO                   
025810       IF VAT-KDFINDOC(WS-MX) = 'CR'                                      
025820         IF WS-TAB-IDLEVNR(WS-MX) = '0000'                                
025830         OR WS-TAB-IDLEVNR(WS-MX) = '    '                                
025900           PERFORM DB2-SELECT-MAX-T01CURR                                 
026000           PERFORM DB2-SELECT-T01CURR                                     
026010         ELSE                                                             
026011           MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                           
026012     MOVE WS-TAB-IDLEVNR(WS-MX)(1:4) TO WS-DASTADAT-CREDIT(3:4)           
026013           MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                           
026014           PERFORM DB2-SELECT-MAX-T01CURR-CREDIT                          
026015           PERFORM DB2-SELECT-T01CURR-CREDIT                              
026020         END-IF                                                           
026030       ELSE                                                               
026031         PERFORM DB2-SELECT-MAX-T01CURR                                   
026032         PERFORM DB2-SELECT-T01CURR                                       
026040       END-IF                                                             
026100       COMPUTE WS-VAT-SUNTO-TOT ROUNDED =                                 
026200              (WS-VAT-SUNTO-TOT * CURR-REVALUTA) /                        
026300               CURR-PRKURS                                                
026400       END-COMPUTE                                                        
026500       COMPUTE WS-VAT-SUVAT-BILLIT-TOT ROUNDED =                          
026600              (WS-VAT-SUVAT-BILLIT-TOT * CURR-REVALUTA) /                 
026700               CURR-PRKURS                                                
026800       END-COMPUTE                                                        
026900       COMPUTE WS-VAT-PRKURS ROUNDED =                                    
027000               CURR-PRKURS / CURR-REVALUTA                                
027100       END-COMPUTE                                                        
027200       MOVE WS-KDVALISO             TO WS-VAT-KDVALISO                    
027300     END-IF                                                               
027400                                                                          
027500     IF VAT-IDLEGSEL (WS-MX) = 'VCCS'                                     
027600       IF WS-VAT-IDEXCUST-1(1:4) NUMERIC                                  
027610**** THE REFILL BOUNCE FLOW SHOULD NOT CREATE INTRASTAT                   
027620         MOVE WS-VAT-IDEXCUST-1(1:4) TO TEST-IDDISTR                      
027630         IF DIST35-NONVCC-NONVCC-REFILL                                   
027631         OR DIST35-NONVCC-NONVCC-TRANSFER                                 
027640           CONTINUE                                                       
027650         ELSE                                                             
027651           WRITE POST-WF2013    FROM WS-VAT-WF2013                        
027660         END-IF                                                           
027670       ELSE                                                               
027700         WRITE POST-WF2013    FROM WS-VAT-WF2013                          
027800       END-IF                                                             
027810     END-IF                                                               
027900     .                                                                    
028000                                                                          
028100*  --- DB2 SECTIONS  ---                                                  
028200*                                                                         
028300 DB2-SELECT-T01PROC SECTION.                                              
028400     MOVE 000100 TO GOOD-SQLCODES                                         
028500                                                                          
028600     EXEC SQL                                                             
028700           SELECT  DAEXDAT                                                
028800                ,  TIEXTID                                                
028900                ,  KDBEH                                                  
029000                ,  IDLEGSEL                                               
029100                                                                          
029200           INTO   :PROC-DAEXDAT                                           
029300                , :PROC-TIEXTID                                           
029400                , :PROC-KDBEH                                             
029500                , :PROC-IDLEGSEL                                          
029600                                                                          
029700           FROM    T01PROC                                                
029800                                                                          
029900           WHERE   IDSYSTEM = :WS-IDSYSTEM     AND                        
030000                   IDLEGSEL = :WS-IDLEGSEL-CRS                            
030100     END-EXEC                                                             
030200                                                                          
030300     MOVE SQLCODE TO SQLCODE-WS                                           
030400     PERFORM DB2-STATUS-CHECK                                             
030500     .                                                                    
030600                                                                          
030700 DB2-DCL-OPN-CRS1 SECTION.                                                
030800     MOVE 000100 TO GOOD-SQLCODES                                         
030900                                                                          
031000     EXEC SQL                                                             
031100        DECLARE CRS1 CURSOR WITH ROWSET POSITIONING FOR                   
031200        SELECT                                                            
031300               A.IDLEGSEL                                                 
031400             , A.IDLANDX3_SEND                                            
031500             , A.IDLANDX3_BET                                             
031600             , A.KDVALISO                                                 
031700             , A.PRKURS                                                   
031800             , A.IDVAT_LEG                                                
031900             , A.IDVAT_RESP                                               
032000             , A.IDVAT_AGENT                                              
032100             , A.IDVAT_BET                                                
032200             , A.IDPARTNR                                                 
032300             , A.KDFINDOC                                                 
032400             , A.DAFINDOC                                                 
032500             , A.IDFINDOC                                                 
032510             , A.IDLEVNR                                                  
032600             , A.SUNTO_TOT                                                
032700             , A.SUVAT_BILLIT_TOT                                         
032800             , D.KDVALISO                                                 
032900             , E.IDEXCUST_1                                               
033000             , E.IDEXCUST_2                                               
033100             , E.IDEXCUST_3                                               
033200                                                                          
033300        FROM   T01DHEA A                                                  
033400             , T01DLIN E                                                  
033500             , T01DOTY B                                                  
033600             , T01INRE C                                                  
033700             , T01FCUS D                                                  
033800                                                                          
033900        WHERE  A.IDLEGSEL      = :PROC-IDLEGSEL                           
034000        AND    A.DAEXDAT       = :PROC-DAEXDAT                            
034100        AND    A.TIEXTID       = :PROC-TIEXTID                            
034200        AND    A.IDLEGSEL        = E.IDLEGSEL                             
034300        AND    A.DAEXDAT         = E.DAEXDAT                              
034400        AND    A.TIEXTID         = E.TIEXTID                              
034500        AND    A.KDVALISO        = E.KDVALISO                             
034600        AND    A.IDLANDX3_SEND   = E.IDLANDX3_SEND                        
034700        AND    A.IDLEVNR         = E.IDLEVNR                              
034800        AND    A.IDPARTNR        = E.IDPARTNR                             
034900        AND    A.KDFINDOC        = E.KDFINDOC                             
035000        AND    A.FLSOFT          = E.FLSOFT                               
035100        AND    A.FLFREE          = E.FLFREE                               
035200        AND    A.FLPRIV          = E.FLPRIV                               
035300        AND    A.IDBREAK_1       = E.IDBREAK_1                            
035400        AND    A.IDBREAK_2       = E.IDBREAK_2                            
035500        AND    A.FLFREE        = :NEJ                                     
035510        AND    (A.FLSOFT       = :NEJ                                     
035520             OR A.FLSOFT       = :JA)                                     
035600        AND    B.IDLEGSEL      = A.IDLEGSEL                               
035700        AND    B.KDFINDOC      = A.KDFINDOC                               
035800        AND    B.KDSTATUS      = :WS-CURRENT-VERSION                      
035900        AND    B.FLVATREP      = :JA                                      
036000        AND    B.DADELDAT      = :WS-ACTIVE                               
036100        AND    C.IDLEGSEL      = A.IDLEGSEL                               
036200        AND    C.IDLANDX3_SEND = A.IDLANDX3_SEND                          
036300        AND    C.IDLANDX3_REC  = A.IDLANDX3_BET                           
036400        AND    C.KDSTATUS      = :WS-CURRENT-VERSION                      
036500        AND    C.FLVATREP      = :JA                                      
036600        AND    C.DADELDAT      = :WS-ACTIVE                               
036700        AND    D.IDLEGSEL      = A.IDLEGSEL                               
036800        AND    D.IDPARTNR      = A.IDPARTNR                               
036900        AND    D.KDSTATUS      = :WS-CURRENT-VERSION                      
037000        AND    D.DADELDAT      = :WS-ACTIVE                               
037100                                                                          
037200        ORDER BY A.IDLEGSEL                                               
037300               , A.DAEXDAT                                                
037400               , A.TIEXTID                                                
037500               , A.KDVALISO                                               
037600               , A.IDLANDX3_SEND                                          
037700               , A.IDLEVNR                                                
037800               , A.IDPARTNR                                               
037900               , A.KDFINDOC                                               
038000               , A.FLSOFT                                                 
038100               , A.FLFREE                                                 
038200               , A.FLPRIV                                                 
038300               , A.IDBREAK_1                                              
038400               , A.IDBREAK_2                                              
038500     END-EXEC                                                             
038600                                                                          
038700     EXEC SQL                                                             
038800        OPEN CRS1                                                         
038900     END-EXEC                                                             
039000                                                                          
039100     MOVE SQLCODE TO SQLCODE-WS                                           
039200     PERFORM DB2-STATUS-CHECK                                             
039300     .                                                                    
039400                                                                          
039500 DB2-FETCH-CRS1 SECTION.                                                  
039600     MOVE 000100         TO GOOD-SQLCODES                                 
039700                                                                          
039800     EXEC SQL                                                             
039900       FETCH NEXT ROWSET FROM CRS1 FOR 100 ROWS                           
040000       INTO  :VAT-IDLEGSEL                                                
040100           , :VAT-IDLANDX3-SEND                                           
040200           , :VAT-IDLANDX3-BET                                            
040300           , :VAT-KDVALISO                                                
040400           , :VAT-PRKURS                                                  
040500           , :VAT-IDVAT-LEG                                               
040600           , :VAT-IDVAT-RESP                                              
040700           , :VAT-IDVAT-AGENT                                             
040800           , :VAT-IDVAT-BET                                               
040900           , :VAT-IDPARTNR                                                
041000           , :VAT-KDFINDOC                                                
041100           , :WS-TAB-DAFINDOC                                             
041200           , :VAT-IDFINDOC                                                
041210           , :WS-TAB-IDLEVNR                                              
041300           , :VAT-SUNTO-TOT                                               
041400           , :VAT-SUVAT-BILLIT-TOT                                        
041500           , :WS-TAB-FCUS-KDVALISO                                        
041600           , :VAT-IDEXCUST-1                                              
041700           , :VAT-IDEXCUST-2                                              
041800           , :WS-TAB-IDEXCUST-3                                           
041900     END-EXEC                                                             
042000                                                                          
042100     MOVE SQLCODE TO SQLCODE-WS                                           
042200     PERFORM DB2-STATUS-CHECK                                             
042300     .                                                                    
042400                                                                          
042500 DB2-CLOSE-CRS1 SECTION.                                                  
042600     EXEC SQL                                                             
042700        CLOSE CRS1                                                        
042800     END-EXEC                                                             
042900     .                                                                    
043000     EJECT                                                                
043100                                                                          
043200 DB2-OPEN-CRS-LSEL SECTION.                                               
043300     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
043400     SELECT   T01LSEL.IDLEGSEL                                            
043500                                                                          
043600     FROM     T01LSEL                                                     
043700                                                                          
043800     WHERE    KDSTATUS = 1                                                
043900     END-EXEC                                                             
044000                                                                          
044100     EXEC SQL OPEN T01LSEL-CRS                                            
044200     END-EXEC                                                             
044300                                                                          
044400     MOVE 000            TO GOOD-SQLCODES                                 
044500     MOVE SQLCODE        TO SQLCODE-WS                                    
044600     PERFORM DB2-STATUS-CHECK                                             
044700     .                                                                    
044800                                                                          
044900 DB2-FETCH-CRS-LSEL SECTION.                                              
045000     EXEC SQL FETCH T01LSEL-CRS INTO                                      
045100            :WS-IDLEGSEL-CRS                                              
045200     END-EXEC                                                             
045300                                                                          
045400     MOVE 000100         TO GOOD-SQLCODES                                 
045500     MOVE SQLCODE        TO SQLCODE-WS                                    
045600     PERFORM DB2-STATUS-CHECK                                             
045700     .                                                                    
045800                                                                          
045900 DB2-CLOSE-CRS-LSEL SECTION.                                              
046000     EXEC SQL CLOSE T01LSEL-CRS                                           
046100     END-EXEC                                                             
046200     .                                                                    
046300     EJECT                                                                
046400                                                                          
046500 DB2-SELECT-MAX-T01CURR SECTION.                                          
046600     MOVE 000            TO GOOD-SQLCODES                                 
046700                                                                          
046800     EXEC SQL                                                             
046900     SELECT   MAX(T01CURR.DASTADAT)                                       
047000                                                                          
047100     INTO     :WS-DASTADAT-KEY                                            
047200                                                                          
047300     FROM     T01CURR                                                     
047400                                                                          
047500     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL                             
047600       AND   (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
047700        OR    T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
047800     END-EXEC                                                             
047900                                                                          
048000     MOVE SQLCODE        TO SQLCODE-WS                                    
048100     PERFORM DB2-STATUS-CHECK                                             
048200     .                                                                    
048300                                                                          
048400 DB2-SELECT-T01CURR SECTION.                                              
048500     MOVE 000            TO GOOD-SQLCODES                                 
048600                                                                          
048700     EXEC SQL                                                             
048800     SELECT   PRKURS                                                      
048900             ,REVALUTA                                                    
049000                                                                          
049100     INTO     :CURR-PRKURS                                                
049200             ,:CURR-REVALUTA                                              
049300                                                                          
049400     FROM     T01CURR                                                     
049500                                                                          
049600     WHERE    IDLEGSEL = :WS-IDLEGSEL                                     
049700       AND    KDVALISO = :WS-KDVALISO                                     
049800       AND    DASTADAT = :WS-DASTADAT-KEY                                 
049900     END-EXEC                                                             
050000                                                                          
050100     MOVE SQLCODE        TO SQLCODE-WS                                    
050200     PERFORM DB2-STATUS-CHECK                                             
050300     .                                                                    
050400                                                                          
050410 DB2-SELECT-MAX-T01CURR-CREDIT SECTION.                                   
050420     MOVE 000            TO GOOD-SQLCODES                                 
050430                                                                          
050440     EXEC SQL                                                             
050450     SELECT   MAX(T01CURR.DASTADAT)                                       
050460                                                                          
050470     INTO     :WS2-DASTADAT-CREDIT                                        
050480                                                                          
050490     FROM     T01CURR                                                     
050491                                                                          
050492     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL                             
050493       AND   (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
050494        OR    T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
050495     END-EXEC                                                             
050496                                                                          
050497     MOVE SQLCODE        TO SQLCODE-WS                                    
050498     PERFORM DB2-STATUS-CHECK                                             
050499     .                                                                    
050500                                                                          
050501 DB2-SELECT-T01CURR-CREDIT SECTION.                                       
050502     MOVE 000            TO GOOD-SQLCODES                                 
050503                                                                          
050504     EXEC SQL                                                             
050505     SELECT   PRKURS                                                      
050506             ,REVALUTA                                                    
050507                                                                          
050508     INTO     :CURR-PRKURS                                                
050509             ,:CURR-REVALUTA                                              
050510                                                                          
050511     FROM     T01CURR                                                     
050512                                                                          
050513     WHERE    IDLEGSEL = :WS-IDLEGSEL                                     
050514       AND    KDVALISO = :WS-KDVALISO                                     
050515       AND    DASTADAT = :WS2-DASTADAT-CREDIT                             
050516     END-EXEC                                                             
050517                                                                          
050518     MOVE SQLCODE        TO SQLCODE-WS                                    
050519     PERFORM DB2-STATUS-CHECK                                             
050520     .                                                                    
050521                                                                          
050530 DB2-STATUS-CHECK  SECTION.                                               
050600     SET SQLCODE-IX TO 1                                                  
050700     SEARCH GOOD-SQLCODE                                                  
050800       AT END                                                             
050900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
051000          DELIMITED BY SIZE INTO ERROR-TEXT                               
051100          CALL ABEND USING RKOD-ABEND-DB2                                 
051200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
051300          CONTINUE                                                        
051400     END-SEARCH                                                           
051500     .                                                                    
