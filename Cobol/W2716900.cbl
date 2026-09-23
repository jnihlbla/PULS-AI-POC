000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2716900.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   FEB 1998.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                PROGRAMMET LÄSER RESTORDER OCH SKRIVER                   
001000*                BARA FÖREGÅENDE VECKAS RESTORDER PÅ UTFILEN              
001100*                FÖREGÅENDE VECKA = AKTUELL KÖRNINGSVECKA                 
001400*                EFTERSOM DEN KÖRS PÅ SÖNDAGAR                            
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
002810     SELECT W27169IN                   ASSIGN TO W27169D1.                
002900*                                                                         
003000     SELECT W27169UT                   ASSIGN TO W27169D2.                
004000     SKIP2                                                                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004410     SKIP3                                                                
004430                                                                          
004440 FD  W27169IN                                                             
004441     RECORDING       F                                                    
004442     BLOCK CONTAINS  0.                                                   
004480                                                                          
004492*01  POST  -COPY W27162    -PRE IN-    -L.                                
004493                                                                          
004494     SKIP3                                                                
004495                                                                          
004496 FD  W27169UT                                                             
004497     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004501                                                                          
004502*01  POST  -COPY W27162    -PRE UT-    -L.                                
004600                                                                          
009100*                                                                         
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400*    -- CHECKED BY WY2000                                                 
009500 77  IDPGM                       PIC X(8)    VALUE 'W2716900'.            
009600 01  CHKP-VAR.                                                            
009700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
009800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
009900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
010000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
010100 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
010200 03  CHKP-MAX                    PIC S9(3)   VALUE +10.                   
010300 77  DC-IX                       PIC S9(4)   VALUE +0  COMP SYNC.         
010400 77  MAX-TAB-IX                  PIC S9(4)   VALUE +10 COMP SYNC.         
010500 77  JA                          PIC X       VALUE 'J'.                   
010600 77  NEJ                         PIC X       VALUE 'N'.                   
010700     SKIP2                                                                
010800 01  WS.                                                                  
010801     03  WS-IDDC                 PIC X(2)    VALUE SPACE.                 
010802     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
010803     03  WS-ANTAL-ORDRAD         PIC 9(7)    VALUE ZERO.                  
010804     03  WS-ANTAL-KRIT-ORDRAD    PIC 9(7)    VALUE ZERO.                  
010805     03  WS-ANTAL-QTY            PIC 9(7)    VALUE ZERO.                  
010806     03  WS-ANTAL-KRIT-QTY       PIC 9(7)    VALUE ZERO.                  
010807     03  WS-KVBEART              PIC 9(7)    VALUE ZERO.                  
010809     03  WS-PRAVCOST             PIC S9(07)V9(2)                          
010810                                             VALUE ZERO.                  
010811     03  WS-KVPB-REF             PIC S9(06)V9(1)                          
010812                                             VALUE ZERO.                  
010813     03  WS-KVLS                 PIC S9(07)  VALUE ZERO.                  
010814     03  WS-KVRESS               PIC S9(07)  VALUE ZERO.                  
010815     03  WS-DARODAT              PIC S9(8)   VALUE ZERO.                  
010816                                                                          
010817     03  WS-CURRENT-DATE.                                                 
010818         05  WS-DAGENS-TIAAAA    PIC 9(4)    VALUE ZERO.                  
010819         05  FILLER              PIC 9(4)    VALUE ZERO.                  
010820         05  FILLER              PIC 9(6)    VALUE ZERO.                  
010821                                                                          
010822     03  FILLER REDEFINES WS-CURRENT-DATE.                                
010823*-----   INKLUSIVE SEKEL                                                  
010824         05  WS-DAGENS-DATUM     PIC 9(8).                                
010825         05  WS-DAGENS-TID.                                               
010826             07 WS-DAGENS-TIMME  PIC 9(2).                                
010827             07 WS-DAGENS-MINUT  PIC 9(2).                                
010828             07 WS-DAGENS-SEKUND PIC 9(2).                                
010829     03  WS-AKTUELL-VECKA        PIC 9(4)    VALUE ZERO.                  
010830                                                                          
014100 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
014200                                                                          
014300 01  FILLER          PIC X(16)   VALUE 'SPAR-FAELT START'.                
014400 01  SPAR-ANTAL-KOLLI            PIC S9(7)   VALUE ZERO  COMP-3.          
014500 01  SPAR-ANTAL-RADER            PIC S9(7)   VALUE ZERO  COMP-3.          
014600 01  SPAR-IDKUNDNR               PIC S9(7)   VALUE ZERO  COMP-3.          
014700 01  SPAR-IDLBBET                PIC X(12)   VALUE SPACE.                 
014800 01  SPAR-IDFAKT                 PIC S9(7)   VALUE ZERO  COMP-3.          
014900 01  SPAR-IDKOLLI                PIC S9(5)   VALUE ZERO  COMP-3.          
015000 01  SPAR-KDKOLLI                PIC X(8)    VALUE SPACE.                 
015100 01  SPAR-IDKUNDRF               PIC X(10)   VALUE SPACE.                 
015200 01  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO  COMP-3.          
015300 01  SPAR-KDFRAKT                PIC S9(3)   VALUE ZERO  COMP-3.          
015400 01  SPAR-KDVALISO               PIC X(3)    VALUE SPACE.                 
015500 01  SPAR-TIFAKT                 PIC S9(7)   VALUE ZERO  COMP-3.          
015600 01  SPAR-TIBERANK               PIC S9(7)   VALUE ZERO  COMP-3.          
015700 01  SPAR-PRKURS              PIC S9(6)V9(1) VALUE ZERO  COMP-3.          
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
018500 77  W27169-EOF-SW               PIC X       VALUE 'N'.                   
018600     88  END-OF-W27169                       VALUE 'J'.                   
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
020000     EJECT                                                                
020100 01  DYNAMISKA-SUBPROGRAM.                                                
020200*                                                                         
020400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
020600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020920     EJECT                                                                
020930*    --- PARAMETRAR TILL WDATKONV                                         
020940*                                                                         
020950*01  -COPY WDATAREA                                                       
021000     EJECT                                                                
021100 01  IN-AREA-START               PIC X(24)   VALUE                        
021200                                             'IN-AREA-START'.             
021300     SKIP2                                                                
021400*01  AREA -COPY W27162     -PRE IN-                                       
022800                                                                          
022900     EJECT                                                                
023000 01  UT-AREA-START              PIC X(24)   VALUE                         
023100                                 'UT-AREA-START  '.                       
023200     SKIP2                                                                
023201*01  AREA -COPY W27162     -PRE UT-                                       
023210                                                                          
023310                                                                          
023400     EJECT                                                                
023500 PROCEDURE DIVISION.                                                      
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023910     PERFORM S01-LAES-W27169IN                                            
024100                                                                          
024200     PERFORM UNTIL END-OF-W27169                                          
024300                                                                          
024310       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
024320       MOVE IN-DARODAT        TO WS-DARODAT                               
024330       MOVE WS-DARODAT (3:6)  TO DAT-I-TIDATUM                            
024340                                                                          
024350       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
024360                           DAT-O-TIDATUM DAT-KDSVAR                       
024370                                                                          
024380       IF DAT-KDSVAR-FEL                                                  
024392         MOVE 'FELAKTIGT DATUM - DATKONV2'                                
024393                              TO FELTEXT-STR                              
024394         CALL FELLOG                                                      
024395       END-IF                                                             
024396                                                                          
024397       IF DAT-TIAAVV-GRP = WS-AKTUELL-VECKA                               
025800         PERFORM S02-SKRIV-UTFIL                                          
025802       END-IF                                                             
025803                                                                          
025900       PERFORM S01-LAES-W27169IN                                          
026092                                                                          
026100     END-PERFORM                                                          
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
031000     OPEN INPUT  W27169IN                                                 
031200     OPEN OUTPUT W27169UT                                                 
031210                                                                          
031220     MOVE FUNCTION CURRENT-DATE                                           
031230                              TO WS-CURRENT-DATE                          
031300                                                                          
031400     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
031500     MOVE WS-CURRENT-DATE (3:6)                                           
031501                              TO DAT-I-TIDATUM                            
031510                                                                          
031520     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
031530                         DAT-O-TIDATUM DAT-KDSVAR                         
031540                                                                          
031550     IF DAT-KDSVAR-OK                                                     
031560       MOVE DAT-TIAAVV-GRP   TO WS-AKTUELL-VECKA                          
031561     ELSE                                                                 
031562       MOVE 'FELAKTIGT DATUM - DATKONV1'                                  
031563                             TO FELTEXT-STR                               
031564       CALL FELLOG                                                        
031570     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800                                                                          
031900 Z-FINIT SECTION.                                                         
032000                                                                          
032100                                                                          
032300     CLOSE W27169IN                                                       
032400           W27169UT                                                       
032600     .                                                                    
032700     EJECT                                                                
032800 S01-LAES-W27169IN SECTION.                                               
032900     SKIP2                                                                
033000     READ W27169IN           INTO IN-AREA                                 
033100     AT END                                                               
033200        MOVE JA TO W27169-EOF-SW                                          
033300                                                                          
033400     END-READ                                                             
033500     .                                                                    
033600     EJECT                                                                
034600 S02-SKRIV-UTFIL SECTION.                                                 
034700     SKIP2                                                                
035000     WRITE UT-POST                    FROM IN-AREA                        
035600     .                                                                    
