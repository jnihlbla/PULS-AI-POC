000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2191000.                                                
000300 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000400 DATE-WRITTEN.   MARS 2003.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                PROGRAMMET SUMMERAR DATA PER KAMPANJ                     
001000*                FRÅN QW90                                                
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
002810     SELECT W21910IN                   ASSIGN TO W21910D1.                
002900*                                                                         
003000     SELECT W21910UT                   ASSIGN TO W21910D2.                
004000     SKIP2                                                                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004410     SKIP3                                                                
004430                                                                          
004440 FD  W21910IN                                                             
004450     RECORDING F                                                          
004470     BLOCK CONTAINS  0.                                                   
004480                                                                          
004481                                                                          
004482*01  POST -COPY W21910QW  -PRE  IN-  -L.                                  
004491                                                                          
004492     SKIP3                                                                
004493                                                                          
004494 FD  W21910UT                                                             
004495     RECORDING F                                                          
004496     BLOCK CONTAINS  0.                                                   
004500                                                                          
004501*01  POST -COPY W21910   -PRE  UT-  -L.                                   
004600                                                                          
009100*                                                                         
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400*    -- CHECKED BY WY2000                                                 
009500 77  IDPGM                       PIC X(8)    VALUE 'W2191000'.            
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
010847     03  WS-ANTAL-UTFIL          PIC 9(9)    VALUE ZERO.                  
012700     EJECT                                                                
014100 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
014200                                                                          
014300 01  FILLER          PIC X(16)   VALUE 'SPAR-FAELT START'.                
015800                                                                          
015900 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
016000 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
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
018500 77  W21910-EOF-SW               PIC X       VALUE 'N'.                   
018600     88  END-OF-W21910                       VALUE 'J'.                   
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
021400*01  AREA -COPY W21910QW   -PRE IN-                                       
022800                                                                          
022900     EJECT                                                                
023000 01  UT-AREA-START              PIC X(24)   VALUE                         
023100                                 'UT-AREA-START  '.                       
023110*01  AREA -COPY W21910     -PRE UT-                                       
023200     SKIP2                                                                
023210                                                                          
023310                                                                          
023400     EJECT                                                                
023500 PROCEDURE DIVISION.                                                      
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023910     PERFORM S01-LAES-W21910IN                                            
024100                                                                          
024200     PERFORM UNTIL END-OF-W21910                                          
024410                                                                          
024411       PERFORM B-NOLLSTALL                                                
024412       MOVE IN-IDKAMP        TO WS-SPARA-IDKAMP                           
024413       MOVE IN-TISTADAT-KAMP TO WS-SPARA-TISTADAT                         
024414       MOVE IN-TISTODAT-KAMP TO WS-SPARA-TISTODAT                         
024415       PERFORM UNTIL END-OF-W21910                                        
024416       OR IN-IDKAMP NOT = WS-SPARA-IDKAMP                                 
024417                                                                          
024430         PERFORM C-BEHANDLA                                               
024540                                                                          
024900         PERFORM S01-LAES-W21910IN                                        
026092                                                                          
026100       END-PERFORM                                                        
026102                                                                          
026129                                                                          
026130                                                                          
026131       MOVE 'Q99'            TO UT-IDPTYP                                 
026132       MOVE WS-SPARA-IDKAMP  TO UT-IDKAMP                                 
026133       MOVE WS-SPARA-TISTADAT                                             
026134                             TO UT-TISTADAT-KAMP                          
026135       MOVE WS-SPARA-TISTODAT                                             
026136                             TO UT-TISTODAT-KAMP                          
026137       MOVE WS-KVKAMP-CARS   TO UT-KVKAMP-CARS                            
026150       PERFORM S02-SKRIV-UTFIL                                            
027000                                                                          
027199     END-PERFORM                                                          
027200                                                                          
027300     IF WS-ANTAL-UTFIL = ZERO                                             
027400         STRING ' OMKÖRNING ?   KTRL VECKA !  SE A-INIT   '               
027500         DELIMITED BY SIZE INTO FELTEXT-STR                               
027600         CALL FELLOG                                                      
027700     END-IF                                                               
030000                                                                          
030100     PERFORM Z-FINIT                                                      
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800     SKIP2                                                                
030900                                                                          
031000     OPEN INPUT  W21910IN                                                 
031200     OPEN OUTPUT W21910UT                                                 
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
031600                                                                          
031601*                                                                         
031602* VID OMKÖRNING VECKAN EFTER      START                                   
031603*                                                                         
031604*    SUBTRACT 1              FROM WS-AKTUELL-AAVV                         
031605* VID OMKÖRNING VECKAN EFTER      SLUT                                    
031606     .                                                                    
031607     EJECT                                                                
031608 B-NOLLSTALL SECTION.                                                     
031609     SKIP2                                                                
031610                                                                          
031620     MOVE ZERO               TO WS-KVKAMP-CARS                            
031628     .                                                                    
031629     EJECT                                                                
031630 C-BEHANDLA SECTION.                                                      
031631                                                                          
031632     ADD IN-KVKAMP-CARS      TO WS-KVKAMP-CARS                            
031670     .                                                                    
031700     EJECT                                                                
031800                                                                          
031900 Z-FINIT SECTION.                                                         
032000                                                                          
032100                                                                          
032300     CLOSE W21910IN                                                       
032400           W21910UT                                                       
032600     .                                                                    
032700     EJECT                                                                
032800 S01-LAES-W21910IN SECTION.                                               
032900     SKIP2                                                                
032901     READ W21910IN           INTO IN-AREA                                 
032902     AT END                                                               
032903        MOVE JA TO W21910-EOF-SW                                          
032904                                                                          
032905     END-READ                                                             
032906                                                                          
032907     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
032908     MOVE IN-TICREATE        TO DAT-I-TIDATUM                             
032909                                                                          
032910     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
032920                     DAT-O-TIDATUM DAT-KDSVAR                             
032930                                                                          
032940     IF DAT-KDSVAR-OK                                                     
032960       MOVE DAT-TIAAVV-GRP   TO WS-KTRL-AAVV                              
033200     ELSE                                                                 
033210       MOVE ZERO             TO WS-KTRL-AAVV                              
033420     END-IF                                                               
033421                                                                          
033430     PERFORM UNTIL END-OF-W21910                                          
033431     OR WS-AKTUELL-AAVV = WS-KTRL-AAVV                                    
033432                                                                          
033440       READ W21910IN         INTO IN-AREA                                 
033450       AT END                                                             
033460          MOVE JA TO W21910-EOF-SW                                        
033470                                                                          
033480       END-READ                                                           
033490                                                                          
033491       MOVE 'AAMMDD'         TO DAT-KDDATFORM                             
033492       MOVE IN-TICREATE      TO DAT-I-TIDATUM                             
033493                                                                          
033494       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
033495                       DAT-O-TIDATUM DAT-KDSVAR                           
033496                                                                          
033497       IF DAT-KDSVAR-OK                                                   
033498         MOVE DAT-TIAAVV-GRP TO WS-KTRL-AAVV                              
033499       ELSE                                                               
033500         MOVE ZERO           TO WS-KTRL-AAVV                              
033501       END-IF                                                             
033502     END-PERFORM                                                          
033510     .                                                                    
033600     EJECT                                                                
034600 S02-SKRIV-UTFIL SECTION.                                                 
034700     SKIP2                                                                
035100     WRITE UT-POST           FROM UT-AREA                                 
035200     ADD 1                   TO WS-ANTAL-UTFIL                            
035600     .                                                                    
