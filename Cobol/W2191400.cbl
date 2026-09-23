000100 PROCESS DYNAM                                                            
000020*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000030*                                                                         
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2191400.                                                
000300 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000400 DATE-WRITTEN.   APRIL 2003.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                LADDAR/UPPDATERAR KAMPANJTABELL (TP1KAMP)                
001000*                DATAT KOMMER FRÅN QW90                                   
001100*                EXEMPEL SE WF1002                                        
001500*                                                                         
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002810     SELECT W21914IN                   ASSIGN TO W21914D1.                
004000     SKIP2                                                                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004410     SKIP3                                                                
004430                                                                          
004440 FD  W21914IN                                                             
004450     RECORDING F                                                          
004470     BLOCK CONTAINS  0.                                                   
004480                                                                          
004481                                                                          
004482*01  POST -COPY W21912    -PRE  IN-  -L.                                  
004491                                                                          
004493                                                                          
009100*                                                                         
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400*    -- CHECKED BY WY2000                                                 
009500 77  IDPGM                       PIC X(8)    VALUE 'W2191400'.            
010300 77  DC-IX                       PIC S9(4)   VALUE +0  COMP SYNC.         
010310 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
010400 77  MAX-TAB-IX                  PIC S9(4)   VALUE +10 COMP SYNC.         
010500 77  JA                          PIC X       VALUE 'J'.                   
010600 77  NEJ                         PIC X       VALUE 'N'.                   
010700     SKIP2                                                                
010800 01  WS.                                                                  
010801     03  WS-AKTUELL-AAVV         PIC 9(4)    VALUE ZERO.                  
010802     03  WS-KTRL-AAVV            PIC 9(4)    VALUE ZERO.                  
010803     03  WS-IDDC                 PIC X(2)    VALUE SPACE.                 
010804     03  WS-VLORDBTO-KOLLI       PIC 9(7)V9(3)                            
010805                                             VALUE ZERO.                  
010843     03  WS-SPARA-IDKAMP         PIC X(7)    VALUE SPACE.                 
010844     03  WS-SPARA-TISTADAT       PIC 9(6)    VALUE ZERO.                  
010845     03  WS-SPARA-TISTODAT       PIC 9(6)    VALUE ZERO.                  
010846     03  WS-KVKAMP-CARS          PIC 9(7)    VALUE ZERO.                  
010847     03  WS-ANTAL-INDATA         PIC 9(9)    VALUE ZERO.                  
010848     03  WS-ANTAL-INSERT         PIC 9(9)    VALUE ZERO.                  
010849     03  WS-ANTAL-UPDATE         PIC 9(9)    VALUE ZERO.                  
010850     03  WS-ANTAL-DELETE         PIC 9(9)    VALUE ZERO.                  
010851     03 FILLER                   PIC X(16)   VALUE                        
010852                                             'WS-DB2-SEKTION'.            
010860     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
012700     EJECT                                                                
014100 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
014110     EJECT                                                                
014120*    --- PARAMETRAR TILL POSTSUM                                          
014130*                                                                         
014140*01  -COPY W0005   -PRE  POSTSUM-                                         
014200                                                                          
014300 01  FILLER          PIC X(16)   VALUE 'SPAR-FAELT START'.                
015800                                                                          
015900 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
016000 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
016010*                                                                         
016020 01   ABEND-TX.                                                           
016030  02  ABEND-RUBRIK.                                                       
016040   03 FILLER              PIC X(16)   VALUE '--------->     *'.           
016050   03 FILLER              PIC X(16)   VALUE '*** A B E N D **'.           
016060   03 FILLER              PIC X(16)   VALUE '**     <--------'.           
016070*                                                                         
016080  02  ABEND-RAD1.                                                         
016090   03 FILLER              PIC X(120)  VALUE ' '.                          
016091*                                                                         
016092  02  ABEND-RAD2.                                                         
016093   03 FILLER              PIC X(120)  VALUE ' '.                          
016094*                                                                         
016095 01      PRM.                                                             
016096  02     PARM-MODE           PIC X(4)    VALUE ' '.                       
016097  02     PARM-REST           PIC X(50)   VALUE ' '.                       
016100                                                                          
016200 01  WS-ARBETSAREA.                                                       
016300*    DATE + TIME  FÖR SKAPANDE AV INLEVERANSNUMMER                        
016400     03  WS-TIAAMMDDTTMMSSTH     PIC 9(14)  VALUE ZERO.                   
016500     03  FILLER REDEFINES WS-TIAAMMDDTTMMSSTH.                            
016600         05  WS-TIAAMMDD-DATE    PIC 9(6).                                
016700         05  WS-TTMMSSTH-TIME    PIC 9(8).                                
016800     03  WS-IDINLEV              PIC S9(15) VALUE ZERO COMP-3.            
016810     03  WS-IDFAKT               PIC  9(7) VALUE ZERO.                    
016900                                                                          
018000     EJECT                                                                
018100 01  FELTEXT.                                                             
018200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018400                                                                          
018500 77  W21914-EOF-SW               PIC X       VALUE 'N'.                   
018600     88  END-OF-W21914                       VALUE 'J'.                   
018900     EJECT                                                                
019000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
019100 01  FILLER REDEFINES DAGENS-DATUM.                                       
019200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
019300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
019400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
019500     SKIP3                                                                
019600 01  WS-AAMMDD                   PIC 9(6).                                
019700 01  FILLER REDEFINES WS-AAMMDD.                                          
019800     03  WS-AA               PIC 9(2).                                    
019900     03  FILLER              PIC 9(4).                                    
019901                                                                          
019910     EJECT                                                                
019920                                                                          
019930*      --- VALID IDDC CODES                                               
019940*                                                                         
019950*01    -COPY WWDC99                                                       
019960       EJECT                                                              
020000                                                                          
020100 01  DYNAMISKA-SUBPROGRAM.                                                
020500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
020600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020700     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
020800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG '.             
020900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020910     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
021000     EJECT                                                                
021011 01  FILLER                      PIC X(16) VALUE 'WDATAREA    '.          
021012*    ---PARAMETRAR TILL DATKONV                                           
021013*01  -COPY WDATAREA                                                       
021014     EJECT                                                                
021015****************************************** PARAM. W009VADD                
021016 01  W009VADDW.                                                           
021017     03  W009VADDW-AAVV      PIC S9(5)               COMP-3.              
021018     03  W009VADDW-ANTAL     PIC S9(3)               COMP-3.              
021019     EJECT                                                                
021020*    --- PARAMETRAR TILL WORKDAY                                          
021030*                                                                         
021040*01  -COPY WORKAREA                                                       
021050     EJECT                                                                
021100 01  IN-AREA-START               PIC X(24)   VALUE                        
021200                                             'IN-AREA-START'.             
021300     SKIP2                                                                
021400*01  AREA -COPY W21912     -PRE IN-                                       
022800                                                                          
022900     EJECT                                                                
023201*                                                                         
023202 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
023203       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
023204                                                                          
023205 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
023206 01  DB2-WS.                                                              
023207     03  SQLCODE-WS              PIC  9(3)   VALUE ZERO.                  
023208         88  CURSOR-OK                       VALUE  000.                  
023209         88  ROW-FOUND                       VALUE  000.                  
023210         88  ROW-NOTFOUND                    VALUE  100.                  
023211         88  ROW-DUPLICATE                   VALUE  803.                  
023212         88  ROW-SEVERAL                     VALUE  811.                  
023213         88  RESOURCE-WRONG                  VALUE  904.                  
023214     03  GOOD-SQLCODECODES.                                               
023215         05  GOOD-SQLCODE OCCURS 5                                        
023216             INDEXED BY SQLCODE-IX PIC 9(3).                              
023217*                                                                         
023218*                                                                         
023219*    ---  DB2 HOST-COPYTEXTER                                             
023220     EJECT                                                                
023221 01  FILLER                      PIC X(16)   VALUE 'TP1KAMP-AREA'.        
023222*01  AREA -COPY TP1KAMP  -PRE TP1KAMP-                                    
023226     EJECT                                                                
023227 01  FILLER                      PIC X(16)   VALUE 'TP1KAMP-DCL '.        
023228       EXEC SQL INCLUDE TP1KAMP  END-EXEC.                                
023232                                                                          
023239     EJECT                                                                
023240 PROCEDURE DIVISION.                                                      
023241 MAIN SECTION.                                                            
023242     ENTRY 'DLITCBL'.                                                     
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023910     PERFORM S01-LAES-W21914IN                                            
024100                                                                          
024200     PERFORM UNTIL END-OF-W21914                                          
024300       ADD 1                 TO WS-ANTAL-INDATA                           
024410                                                                          
024411       IF IN-KDSTATUS-KAMP = 'N'                                          
024412         MOVE IN-IDKAMP      TO TP1KAMP-IDKAMP                            
024413         MOVE IN-KVKAMP-CARS TO TP1KAMP-KVKAMP-CARS                       
024414         MOVE IN-TISTADAT-KAMP                                            
024417                             TO TP1KAMP-TISTADAT-KAMP                     
024418         MOVE IN-TISTODAT-KAMP                                            
024419                             TO TP1KAMP-TISTODAT-KAMP                     
024420         MOVE 1              TO TP1KAMP-RERESPRT                          
024421         MOVE ZERO           TO TP1KAMP-IDKAMP-GRP                        
024422                                TP1KAMP-IDLOPNR-KAMP                      
024423         MOVE 'W'            TO TP1KAMP-KDKAMP                            
024424         MOVE SPACE          TO TP1KAMP-BEKAMNOT                          
024426         PERFORM DB2-INSERT-TP1KAMP                                       
024427         IF CURSOR-OK                                                     
024428           ADD 1             TO WS-ANTAL-INSERT                           
024429         ELSE                                                             
024430           PERFORM DB2-UPDATE-TP1KAMP                                     
024431           ADD 1             TO WS-ANTAL-UPDATE                           
024433         END-IF                                                           
024434       ELSE                                                               
024435         IF IN-KDSTATUS-KAMP = 'C'                                        
024436           MOVE IN-KVKAMP-CARS                                            
024437                             TO TP1KAMP-KVKAMP-CARS                       
024438           MOVE IN-TISTADAT-KAMP                                          
024439                             TO TP1KAMP-TISTADAT-KAMP                     
024440           MOVE IN-TISTODAT-KAMP                                          
024441                             TO TP1KAMP-TISTODAT-KAMP                     
024442           PERFORM DB2-UPDATE-TP1KAMP                                     
024443           ADD 1             TO WS-ANTAL-UPDATE                           
024444         ELSE                                                             
024445*                                                                         
024446*          PERFORM DB2-DELETE-TP1KAMP                                     
024447           ADD 1             TO WS-ANTAL-DELETE                           
024450         END-IF                                                           
024460       END-IF                                                             
024540                                                                          
024900       PERFORM S01-LAES-W21914IN                                          
026092                                                                          
026100     END-PERFORM                                                          
026102                                                                          
030100     PERFORM Z-FINIT                                                      
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800     SKIP2                                                                
030900                                                                          
031000     OPEN INPUT  W21914IN                                                 
031300     ACCEPT DAGENS-DATUM  FROM DATE                                       
031400                                                                          
031500     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
031510     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
031520                                                                          
031530     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
031540                     DAT-O-TIDATUM DAT-KDSVAR                             
031550                                                                          
031560     IF DAT-KDSVAR-OK                                                     
031570                                                                          
031580       MOVE DAT-TIAAVV-GRP   TO WS-AKTUELL-AAVV                           
031590       MOVE WS-AKTUELL-AAVV  TO W009VADDW-AAVV                            
031591       MOVE -1               TO W009VADDW-ANTAL                           
031592       CALL W009VADD USING W009VADDW-AAVV W009VADDW-ANTAL                 
031593       MOVE W009VADDW-AAVV   TO WS-KTRL-AAVV                              
031594                                                                          
031595     ELSE                                                                 
031596         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
031597         DELIMITED BY SIZE INTO FELTEXT                                   
031598         CALL FELLOG                                                      
031599     END-IF                                                               
031600     INITIALIZE GOOD-SQLCODECODES                                         
031601     .                                                                    
031602     EJECT                                                                
031800                                                                          
031900 Z-FINIT SECTION.                                                         
032000                                                                          
032200     DISPLAY 'ANTAL INDATA : ' WS-ANTAL-INDATA                            
032201     DISPLAY 'ANTAL INSERT : ' WS-ANTAL-INSERT                            
032210     DISPLAY 'ANTAL UPDATE : ' WS-ANTAL-UPDATE                            
032220     DISPLAY 'ANTAL DELETE : ' WS-ANTAL-DELETE                            
032300     CLOSE W21914IN                                                       
032600     .                                                                    
032700     EJECT                                                                
032800 S01-LAES-W21914IN SECTION.                                               
032900     SKIP2                                                                
032901     READ W21914IN           INTO IN-AREA                                 
032902     AT END                                                               
032903        MOVE JA TO W21914-EOF-SW                                          
032904                                                                          
032905     END-READ                                                             
033500     .                                                                    
033600     EJECT                                                                
033700                                                                          
035100                                                                          
035800 DB2-INSERT-TP1KAMP SECTION.                                              
035810     MOVE 'DB2-INSERT-TP1KAMP   ' TO  WS-DB2-SEKTION                      
035820     SKIP2                                                                
035830     MOVE 000803   TO GOOD-SQLCODECODES                                   
035900                                                                          
036100     SKIP2                                                                
036200     EXEC SQL                                                             
036300       INSERT INTO TP1KAMP                                                
036400         (                                                                
036500          IDKAMP                                                          
036600         ,KVKAMP_CARS                                                     
036700         ,TISTADAT_KAMP                                                   
036710         ,TISTODAT_KAMP                                                   
036800         ,RERESPRT                                                        
036900         ,KDKAMP                                                          
037000         ,BEKAMNOT                                                        
037100         ,IDKAMP_GRP                                                      
037200         ,IDLOPNR_KAMP                                                    
039600         )                                                                
039700       VALUES                                                             
039800         (                                                                
039900          :TP1KAMP-IDKAMP                                                 
040000         ,:TP1KAMP-KVKAMP-CARS                                            
040100         ,:TP1KAMP-TISTADAT-KAMP                                          
040110         ,:TP1KAMP-TISTODAT-KAMP                                          
040200         ,:TP1KAMP-RERESPRT                                               
040300         ,:TP1KAMP-KDKAMP                                                 
040400         ,:TP1KAMP-BEKAMNOT                                               
040500         ,:TP1KAMP-IDKAMP-GRP                                             
040600         ,:TP1KAMP-IDLOPNR-KAMP                                           
043000         )                                                                
043100     END-EXEC                                                             
043200                                                                          
043500     MOVE SQLCODE TO SQLCODE-WS                                           
043800     PERFORM DB2-STATUS-CHECK                                             
044300     .                                                                    
044310     EJECT                                                                
044400                                                                          
044500                                                                          
044600 DB2-UPDATE-TP1KAMP SECTION.                                              
044700                                                                          
044800     MOVE 'DB2-UPDATE-TP1KAMP   ' TO  WS-DB2-SEKTION                      
044810     SKIP2                                                                
044830     MOVE 000   TO GOOD-SQLCODECODES                                      
044900     SKIP2                                                                
045000     EXEC SQL                                                             
045100       UPDATE TP1KAMP                                                     
045200       SET                                                                
045220          KVKAMP_CARS    = :TP1KAMP-KVKAMP-CARS                           
045230         ,TISTADAT_KAMP  = :TP1KAMP-TISTADAT-KAMP                         
045231         ,TISTODAT_KAMP  = :TP1KAMP-TISTODAT-KAMP                         
047100       WHERE                                                              
047200                IDKAMP         = :IN-IDKAMP                               
047800     END-EXEC                                                             
047900                                                                          
048100     MOVE SQLCODE TO SQLCODE-WS                                           
048400     PERFORM DB2-STATUS-CHECK                                             
048900     .                                                                    
048910     EJECT                                                                
049000                                                                          
049001     EJECT                                                                
049002 DB2-DELETE-TP1KAMP SECTION.                                              
049003     MOVE 'DB2-DELETE-TP1KAMP   ' TO  WS-DB2-SEKTION                      
049004                                                                          
049006     MOVE 000   TO GOOD-SQLCODECODES                                      
049007                                                                          
049008     EXEC SQL                                                             
049009         DELETE FROM TP1KAMP                                              
049010                                                                          
049011         WHERE   IDKAMP    = :IN-IDKAMP                                   
049012     END-EXEC                                                             
049013                                                                          
049014     MOVE SQLCODE TO SQLCODE-WS                                           
049015     PERFORM DB2-STATUS-CHECK                                             
049016     .                                                                    
049017     EJECT                                                                
049018                                                                          
049020                                                                          
049030 DB2-STATUS-CHECK  SECTION.                                               
049040                                                                          
049070     SET SQLCODE-IX TO 1                                                  
049080     SEARCH GOOD-SQLCODE                                                  
049091       AT END CALL FELLOG                                                 
049095       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
049096     END-SEARCH                                                           
049097     .                                                                    
049100                                                                          
