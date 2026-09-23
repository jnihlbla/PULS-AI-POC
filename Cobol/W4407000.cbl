000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.                 W4407000.                                    
000400 AUTHOR.                     STEFANO GIOBBI.                              
000500     DATE-WRITTEN.           JUN 1991.                                    
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*    LÄSER ORDERKÖN (WDQ1) MED SB.                                        
001200*    SALDOINFORMATION LISTAS PÅ FIL.                                      
001300*                                                                         
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900                                                                          
002000     SELECT W44070           ASSIGN TO      W44070D1.                     
002100     SELECT W44068           ASSIGN TO      W44070D2.                     
002200     SELECT W44075           ASSIGN TO      W44070D3.                     
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600     SKIP2                                                                
002700 FD  W44070                                                               
002800     LABEL RECORD STANDARD                                                
002900     RECORDING F                                                          
003000     BLOCK CONTAINS 0.                                                    
003100*01  POST -COPY W440068 -PRE W44070-  -L.                                 
003200     EJECT                                                                
003300 FD  W44068                                                               
003400     LABEL RECORD STANDARD                                                
003500     RECORDING F                                                          
003600     BLOCK CONTAINS 0.                                                    
003700*01  POST -COPY W440068 -PRE W44068-  -L.                                 
003800     EJECT                                                                
003900 FD  W44075                                                               
004000     LABEL RECORD STANDARD                                                
004100     RECORDING F                                                          
004200     BLOCK CONTAINS 0.                                                    
004300*01  POST -COPY W440702 -PRE W44075-  -L.                                 
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004700*    -COPY WY2000W1                                                       
004800     SKIP3                                                                
004900*    ---- GENERELLA KONSTANTER                                            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  FEL                         PIC X       VALUE 'F'.                   
005300     EJECT                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700     EJECT                                                                
005800 01  FELSECTION.                                                          
005900     03  FILLER                  PIC X(8)    VALUE 'FELSECT'.             
006000     03  FELSECT-STR             PIC X(72)   VALUE SPACE.                 
006100     EJECT                                                                
006200 01  SPAR-AREA-START             PIC X(24)   VALUE                        
006300                                             'SPAR-AREA START'.           
006400 01  SPAR-AREA.                                                           
006500     03  SPAR-TIAAVVD            PIC  9(5)           VALUE  0.            
006600     03  FILLER REDEFINES SPAR-TIAAVVD.                                   
006700       05  SPAR-TIAAVVD1-4       PIC  9(4).                               
006800       05  SPAR-TIAAVVD-DAG      PIC  9(1).                               
006900     03  SPAR-TIAAMMDD-FOM       PIC  9(6)           VALUE  0.            
007000     03  SPAR-TIAAMMDD-TOM       PIC  9(6)           VALUE  0.            
007100     SKIP2                                                                
007200 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
007300 01  FILLER REDEFINES TEST-IDDISTR.                                       
007400*    03 -COPY WWDIST35                                                    
007500     EJECT                                                                
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
007800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
007900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008100   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
008200   03  WDAGKONV                  PIC X(8)    VALUE 'WDAGKONV'.            
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL ABEND                                            
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008900     EJECT                                                                
009000*    ---- PARAMETRAR TILL POSTSUM                                         
009100                                                                          
009200*01  -COPY W0005      -PRE POSTSUM-.                                      
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL WDATKONV                                         
009500 01  WDATKONV-START              PIC X(24)   VALUE                        
009600                                 'WDATKONV START '.                       
009700*                                                                         
009800*01  -COPY WDATAREA                                                       
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL WDAGKONV                                         
010100 01  WDAGKONV-START              PIC X(24)   VALUE                        
010200                                 'WDAGKONV START '.                       
010300*                                                                         
010400*01  -COPY WDAGAREA                                                       
010500     EJECT                                                                
010600*    ---- UTAREA FÖR W44070-POST                                          
010700                                                                          
010800 01  FILLER                      PIC X(16)   VALUE                        
010900                                             'W44070 POST'.               
011000     SKIP3                                                                
011100*01  AREA -COPY W440068    -PRE UT70-.                                    
011200     EJECT                                                                
011300                                                                          
011400*    ---- UTAREA FÖR W44068-POST                                          
011500                                                                          
011600 01  FILLER                      PIC X(16)   VALUE                        
011700                                             'W44068 POST'.               
011800     SKIP3                                                                
011900*01  AREA -COPY W440068    -PRE UT68-.                                    
012000     EJECT                                                                
012100*    ---- UTAREA FÖR W44075-POST                                          
012200                                                                          
012300 01  FILLER                      PIC X(16)   VALUE                        
012400                                             'W44075 POST'.               
012500     SKIP3                                                                
012600*01  AREA -COPY W440702    -PRE UT75-.                                    
012700     EJECT                                                                
012800 01 DB2-LASNING.                                                          
012900     03 FILLER                   PIC X(16)   VALUE                        
013000                                             'WS-DB2-SEKTION'.            
013100     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
013200                                                                          
013300     EJECT                                                                
013400 01 NYCKLAR-TP4TRAN.                                                      
013500     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
013800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013900                                                                          
014000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
014100 01  DB2-WS.                                                              
014200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
014300         88  CURSOR-OK                       VALUE 000.                   
014400         88  RADER-FINNS                     VALUE 000.                   
014500         88  RADER-SAKNAS                    VALUE 100.                   
014600         88  ATKOMST-FEL                     VALUE 904.                   
014700     03  GODK-SQLCODEKODER.                                               
014800         05  GODK-SQLCODE OCCURS 5                                        
014900             INDEXED BY SQLCODE-IX PIC 9(3).                              
015000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
015100     EJECT                                                                
015200*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
015300                                                                          
015400 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
015500                                                                          
015600*    ---- STATUSKOD FRÅN IMS                                              
015700                                                                          
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FINNS                      VALUE '  '.                   
016000     88  SEGMENT-SLUT                       VALUE 'GB'.                   
016100                                                                          
016200 01  GODK-STATUSKODER.                                                    
016300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
016400                                                                          
016500 01  SSA1                        PIC X(40).                               
016600     EJECT                                                                
016700*01      -COPY W0003.                                                     
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)  VALUE                         
017000                                            'DLI-IO-AREA'.                
017100 01  DLI-IO-AREA.                                                         
017200*                                                                         
017300*  03  WLORQM01 -COPY WDQ101                                              
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
017600                                                                          
017700*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
017800     EJECT                                                                
017900     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
018000     EJECT                                                                
018100 LINKAGE SECTION.                                                         
018200     SKIP2                                                                
018300*    -COPY W0008 -PRE WDQ1-.                                              
018400    05  FILLER                   PIC XX.                                  
018500     EJECT                                                                
018600 PROCEDURE DIVISION  USING WDQ1-PCB.                                      
018700     ENTRY 'DLITCBL' USING WDQ1-PCB.                                      
018800                                                                          
018900 STYR SECTION.                                                            
019000                                                                          
019100     PERFORM A-INIT                                                       
019200     PERFORM IMS-GET-WDQ1                                                 
019300     PERFORM UNTIL SEGMENT-SLUT                                           
019400       PERFORM B-SKAPA-W44070                                             
019500       PERFORM C-SKAPA-W44068                                             
019600       PERFORM D-SKAPA-W44075                                             
019700       PERFORM IMS-GET-WDQ1                                               
019800     END-PERFORM                                                          
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100     MOVE    ZERO TO RETURN-CODE                                          
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 A-INIT SECTION.                                                          
020600                                                                          
020700     OPEN OUTPUT W44070                                                   
020800                 W44068                                                   
020900                 W44075                                                   
021000     MOVE 'W4407000'           TO    POSTSUM-PROGNAMN                     
021100     .                                                                    
021200     EJECT                                                                
021300 B-SKAPA-W44070 SECTION.                                                  
021400                                                                          
021500     IF OBKR-FLOBOK   = NEJ AND                                           
021600        OBKR-KDORDBEK = 10 AND OBKR-TIRODAT > 0                           
021700                                                                          
021800       MOVE  OBKR-IDARTNR        TO    UT70-IDARTNR                       
021900       MOVE  OBKR-IDDC           TO    UT70-IDDC                          
022000       MOVE  OBKR-KDORDKL        TO    UT70-KDORDKL                       
022100       MOVE  OBKR-KVBEART-Q      TO    UT70-KVBEART-Q                     
022200                                                                          
022300       MOVE ZERO                 TO    UT70-KVPREAVB                      
022400                                       UT70-KVPRERO                       
022500       WRITE W44070-POST         FROM  UT70-AREA                          
022600                                                                          
022700       MOVE 'W44070D1'           TO    POSTSUM-DDNAMN2                    
022800       MOVE 'W44070  '           TO    POSTSUM-FDNAMN                     
022900       MOVE 'BIPA'               TO    POSTSUM-TRANSTYP                   
023000       CALL  POSTSUM             USING POSTSUM-PARM                       
023100                                                                          
023200     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 C-SKAPA-W44068 SECTION.                                                  
023600                                                                          
023700     IF OBKR-FLOBOK   = NEJ AND OBKR-IDSYSTEM NOT = 'PROF'                
023800         EVALUATE TRUE                                                    
023900             WHEN  OBKR-KDORDBEK = 10                                     
024000                  PERFORM CA-REDIGERA                                     
024100                                                                          
024200             WHEN (OBKR-KDORDBEK = 15  OR                                 
024300                                   16  OR                                 
024400                                   43  OR                                 
024500                                   44  OR                                 
024600                                   92  OR 99)                             
024700                                       AND                                
024800                  (OBKR-KVPREAVB > ZERO OR OBKR-KVPRERO > ZERO)           
024900                  PERFORM CA-REDIGERA                                     
025000                                                                          
025100             WHEN (OBKR-KDORDBEK = 41)                                    
025200                                       AND                                
025300                  (OBKR-IDARTNR-TILLK > +0)                               
025400                                       AND                                
025500                  (OBKR-KVPREAVB > ZERO OR OBKR-KVPRERO > ZERO)           
025600                                                                          
025700                  PERFORM CA-REDIGERA                                     
025800         END-EVALUATE                                                     
025900     END-IF                                                               
026000     .                                                                    
026100 CA-REDIGERA SECTION.                                                     
026200                                                                          
026300     IF OBKR-IDARTNR-TILLK > +0                                           
026400         MOVE OBKR-IDARTNR-TILLK   TO    UT68-IDARTNR                     
026500     ELSE                                                                 
026600         MOVE  OBKR-IDARTNR        TO    UT68-IDARTNR                     
026700     END-IF                                                               
026800     MOVE  OBKR-IDDC               TO    UT68-IDDC                        
026900     MOVE  OBKR-KDORDKL            TO    UT68-KDORDKL                     
027000     IF OBKR-KDORDBEK = 92                                                
027100        COMPUTE UT68-KVBEART-Q = OBKR-KVPREAVB + OBKR-KVPRERO             
027200     ELSE                                                                 
027300        MOVE  OBKR-KVBEART-Q       TO    UT68-KVBEART-Q                   
027400     END-IF                                                               
027500                                                                          
027600     IF OBKR-IDKAMPRF > +0                                                
027700         MOVE +0                   TO    UT68-KVBEART-Q                   
027800     END-IF                                                               
027900                                                                          
028000     IF OBKR-KDORDBEK = 10                                                
028100       IF OBKR-TIRODAT = ZERO                                             
028200         MOVE ZERO                 TO    UT68-KVPREAVB                    
028300                                         UT68-KVPRERO                     
028400         PERFORM CB-SKRIV                                                 
028500       END-IF                                                             
028600     ELSE                                                                 
028700       IF OBKR-IDLEVNR NOT = SPACE                                        
028800         MOVE ZERO                 TO    UT68-KVPREAVB                    
028900       ELSE                                                               
029000         MOVE OBKR-KVPREAVB        TO    UT68-KVPREAVB                    
029100       END-IF                                                             
029200                                                                          
029300       MOVE  OBKR-KVPRERO          TO    UT68-KVPRERO                     
029400                                                                          
029500       PERFORM CB-SKRIV                                                   
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 CB-SKRIV SECTION.                                                        
030000                                                                          
030100         WRITE W44068-POST     FROM  UT68-AREA                            
030200                                                                          
030300         MOVE 'W44070D2'       TO    POSTSUM-DDNAMN2                      
030400         MOVE 'W44068  '       TO    POSTSUM-FDNAMN                       
030500         MOVE 'OBKR'           TO    POSTSUM-TRANSTYP                     
030600         CALL  POSTSUM         USING POSTSUM-PARM                         
030700     .                                                                    
030800     EJECT                                                                
030900 D-SKAPA-W44075 SECTION.                                                  
031000                                                                          
031100     PERFORM DA-BERAKNA-INNEVARANDE-AAVVD                                 
031200     PERFORM DB-BERAKNA-DESS-STARTDATUM                                   
031300     PERFORM DC-BERAKNA-TOM-DATUM                                         
031400     PERFORM DD-BERAKNA-FOM-DATUM                                         
031500     PERFORM DE-REDIGERA-REFILLPOST                                       
031600     .                                                                    
031700     EJECT                                                                
031800 DA-BERAKNA-INNEVARANDE-AAVVD SECTION.                                    
031900                                                                          
032000     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
032100                                                                          
032200     CALL WDATKONV USING                                                  
032300          DAT-KDDATFORM,                                                  
032400          DAT-I-TIDATUM,                                                  
032500          DAT-O-TIDATUM,                                                  
032600          DAT-KDSVAR                                                      
032700                                                                          
032800     IF DAT-KDSVAR = FEL                                                  
032900       MOVE    'KDSVAR = F FRÅN WDATKONV'     TO FELTEXT-STR              
033000       MOVE    'DA-BERAKNA-INNEVARANDE-AAVVD' TO FELSECT-STR              
033100       MOVE     RKOD-ABEND-UTAN-DUMP          TO RKOD-ABEND               
033200       PERFORM  S99-ABEND                                                 
033300     END-IF                                                               
033400                                                                          
033500     MOVE DAT-TIAAVVD TO SPAR-TIAAVVD                                     
033600     .                                                                    
033700     EJECT                                                                
033800 DB-BERAKNA-DESS-STARTDATUM SECTION.                                      
033900                                                                          
034000     MOVE 1 TO SPAR-TIAAVVD-DAG                                           
034100                                                                          
034200     MOVE 'AAVVD '     TO DAT-KDDATFORM                                   
034300     MOVE SPAR-TIAAVVD TO DAT-I-TIDATUM                                   
034400                                                                          
034500     CALL WDATKONV USING                                                  
034600          DAT-KDDATFORM,                                                  
034700          DAT-I-TIDATUM,                                                  
034800          DAT-O-TIDATUM,                                                  
034900          DAT-KDSVAR                                                      
035000                                                                          
035100     IF DAT-KDSVAR = FEL                                                  
035200       MOVE    'KDSVAR = F FRÅN WDATKONV'     TO FELTEXT-STR              
035300       MOVE    'DB-BERAKNA-DESS-STARTDATUM'   TO FELSECT-STR              
035400       MOVE     RKOD-ABEND-UTAN-DUMP          TO RKOD-ABEND               
035500       PERFORM  S99-ABEND                                                 
035600     END-IF                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 DC-BERAKNA-TOM-DATUM SECTION.                                            
036000                                                                          
036100     MOVE 003          TO DAG-KDCALL                                      
036200*     KDCALL 003: COMPUTE  OM =  OM -  AYS                                
036300*                         F     T     D                                   
036400     MOVE DAT-TISEKEL  TO DAG-TISEKEL-FOM                                 
036500     MOVE DAT-TIAAMMDD TO DAG-TIAAMMDD-TOM                                
036600     MOVE 2            TO DAG-KVKALDAG                                    
036700                                                                          
036800     CALL WDAGKONV USING                                                  
036900          DAG-KDCALL,                                                     
037000          DAG-DATUM-AREA,                                                 
037100          DAG-KDSVAR                                                      
037200                                                                          
037300     IF DAG-KDSVAR = FEL                                                  
037400       MOVE    'KDSVAR = F FRÅN WDAGKONV'     TO FELTEXT-STR              
037500       MOVE    'DC-BERAKNA-TOM-DATUM'         TO FELSECT-STR              
037600       MOVE     RKOD-ABEND-UTAN-DUMP          TO RKOD-ABEND               
037700       PERFORM  S99-ABEND                                                 
037800     END-IF                                                               
037900                                                                          
038000     MOVE DAG-TIAAMMDD-FOM TO SPAR-TIAAMMDD-TOM                           
038100     .                                                                    
038200     EJECT                                                                
038300 DD-BERAKNA-FOM-DATUM SECTION.                                            
038400                                                                          
038500     MOVE 003          TO DAG-KDCALL                                      
038600*     KDCALL 003: COMPUTE  OM =  OM -  AYS                                
038700*                         F     T     D                                   
038800                                                                          
038900     MOVE DAT-TISEKEL  TO DAG-TISEKEL-FOM                                 
039000     MOVE DAT-TIAAMMDD TO DAG-TIAAMMDD-TOM                                
039100     MOVE 15           TO DAG-KVKALDAG                                    
039200                                                                          
039300     CALL WDAGKONV USING                                                  
039400          DAG-KDCALL,                                                     
039500          DAG-DATUM-AREA,                                                 
039600          DAG-KDSVAR                                                      
039700                                                                          
039800     IF DAG-KDSVAR = FEL                                                  
039900       MOVE    'KDSVAR = F FRÅN WDAGKONV'     TO FELTEXT-STR              
040000       MOVE    'DD-BERAKNA-FOM-DATUM'         TO FELSECT-STR              
040100       MOVE     RKOD-ABEND-UTAN-DUMP          TO RKOD-ABEND               
040200       PERFORM  S99-ABEND                                                 
040300     END-IF                                                               
040400                                                                          
040500     MOVE DAG-TIAAMMDD-FOM TO SPAR-TIAAMMDD-FOM                           
040600     .                                                                    
040700     EJECT                                                                
040800 DE-REDIGERA-REFILLPOST SECTION.                                          
040900                                                                          
041000******************************************************************        
041100*                                                                         
041200*  KOLLA OM TRANSFER (GER SVARET DB2-RADER-FINNS)                         
041300*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
041400*                                                                         
041500******************************************************************        
041600                                                                          
041700     MOVE OBKR-IDDISTR       TO W-TP4TRAN-IDDISTR                         
041800                                                                          
041900     PERFORM DB2-SELECT-TP4TRAN                                           
042000                                                                          
042100     MOVE OBKR-IDDISTR TO TEST-IDDISTR                                    
042200                                                                          
042300     IF DIST35-REFILL             OR                                      
042400        DIST35-NONVCC-REFILL      OR                                      
042500        DIST35-NONVCC-VCC-TRANSFER OR                                     
042600        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
042700        DIST35-REFILL-INOM-NDC    OR                                      
042800        DIST35-REFILL-NA-JAP      OR                                      
042900        DIST35-NA-TRANSFER        OR                                      
043000        DIST35-NA-NDC-RETURNS     OR                                      
043100        DIST35-PACIFIC-TRANSFER   OR                                      
043200        DIST35-REFILL-INOM-JP     OR                                      
043300        DIST35-CN-TRANSFER        OR                                      
043400        RADER-FINNS                                                       
043500       MOVE OBKR-TIREGDAT     TO TMP1-YYMMDD                              
043600       MOVE SPAR-TIAAMMDD-FOM TO TMP2-YYMMDD                              
043700       MOVE SPAR-TIAAMMDD-TOM TO TMP3-YYMMDD                              
043800       PERFORM WY2000P1                                                   
043900       IF TMP1-YYMMDD >= TMP2-YYMMDD AND                                  
044000          TMP1-YYMMDD <= TMP3-YYMMDD                                      
044100         MOVE    OBKR-IDGMTREF     TO UT75-IDGMTREF                       
044200         MOVE    OBKR-IDARTNR      TO UT75-IDARTNR                        
044300         MOVE    OBKR-KDORDBEK     TO UT75-KDORDBEK                       
044400         MOVE    OBKR-KVBEART      TO UT75-KVBEART                        
044500         MOVE    OBKR-KVBEART-Q    TO UT75-KVBEART-Q                      
044600         MOVE    OBKR-KVANNANT     TO UT75-KVANNANT                       
044700         MOVE    SPAR-TIAAMMDD-FOM TO UT75-TIORDREG-STA                   
044800         MOVE    SPAR-TIAAMMDD-TOM TO UT75-TIORDREG-STO                   
044900         PERFORM S11-SKRIV-W44075                                         
045000       END-IF                                                             
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 Z-FINIT SECTION.                                                         
045500                                                                          
045600     CLOSE W44070                                                         
045700           W44068                                                         
045800           W44075                                                         
045900     MOVE  'S'     TO    POSTSUM-OPKOD                                    
046000     CALL  POSTSUM USING POSTSUM-PARM                                     
046100     .                                                                    
046200     EJECT                                                                
046300*                                                                         
046400 S11-SKRIV-W44075 SECTION.                                                
046500                                                                          
046600     WRITE W44075-POST FROM UT75-AREA                                     
046700                                                                          
046800     MOVE 'REFI'     TO    POSTSUM-TRANSTYP                               
046900     MOVE 'W44075'   TO    POSTSUM-FDNAMN                                 
047000     MOVE 'W44070D2' TO    POSTSUM-DDNAMN2                                
047100     CALL  POSTSUM   USING POSTSUM-PARM                                   
047200     .                                                                    
047300     EJECT                                                                
047400*                                                                         
047500 IMS-GET-WDQ1 SECTION.                                                    
047600                                                                          
047700     MOVE    '  GAGKGB'       TO    GODK-STATUSKODER                      
047800     CALL    CBLTDLI          USING GN WDQ1-PCB DLI-IO-AREA               
047900     MOVE    WDQ1-STATUS-CODE TO    STATUS-WS                             
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200     SKIP3                                                                
048300     EJECT                                                                
048400 DB2-SELECT-TP4TRAN     SECTION.                                          
048500     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
048600                                                                          
048700     MOVE 000100 TO GODK-SQLCODEKODER                                     
048800                                                                          
048900     EXEC SQL                                                             
049000           SELECT  DISTINCT                                               
049100                   IDDC_REC                                               
049200                                                                          
049300           INTO   :TP4TRAN-IDDC-REC                                       
049400                                                                          
049500           FROM    TP4TRAN                                                
049600                                                                          
049700           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
049800     END-EXEC                                                             
049900                                                                          
050000     MOVE SQLCODE TO SQLCODE-WS                                           
050100     PERFORM DB2-STATUSKONTROLL                                           
050200     .                                                                    
050300     EJECT                                                                
050400 IMS-STATUSKONTROLL SECTION.                                              
050500                                                                          
050600     SET    STATUS-IX TO 1                                                
050700     SEARCH GODK-STATUS                                                   
050800       AT END CALL FELLOG                                                 
050900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
051000         CONTINUE                                                         
051100     END-SEARCH                                                           
051200     .                                                                    
051300     EJECT                                                                
051400 DB2-STATUSKONTROLL  SECTION.                                             
051500                                                                          
051600     SET SQLCODE-IX TO 1                                                  
051700     SEARCH GODK-SQLCODE                                                  
051800       AT END                                                             
051900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
052000          DELIMITED BY SIZE INTO FELTEXT                                  
052100          CALL ABEND USING RKOD-ABEND-DB2                                 
052200       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
052300     END-SEARCH                                                           
052400     .                                                                    
052500 S99-ABEND SECTION.                                                       
052600                                                                          
052700     MOVE 'S'     TO    POSTSUM-OPKOD                                     
052800     CALL POSTSUM USING POSTSUM-PARM                                      
052900     CALL ABEND   USING RKOD-ABEND                                        
053000     .                                                                    
053100     EJECT                                                                
053200*    -COPY WY2000P1                                                       
