000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF026100.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   02/03/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*    NAME:                                                                
000620*        CARPARTS.BILLIT.APPRPARTGRLOCATE                                 
000700*    FUNCTION:                                                            
000800*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
000810*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
000900*                                                                         
001000*        THE PROGRAM READS   TABLE T01LSEL                                
001100*        THE PROGRAM READS   TABLE T01CUGR                                
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: WF0261T                                             
001500*        REQUEST:     WZ01REQU                                            
001600*                     WF0261I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    WZ01RESP                                            
002000*                     WF0261O1                                            
002100                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002500 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)  VALUE 'WF026100'.             
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003110 77  KDRC-DISPLAY                PIC Z(5).                                
003200                                                                          
003210*    --- CONSTANTS                                                        
003300 77  YES                         PIC X      VALUE 'Y'.                    
003400 77  NOO                         PIC X      VALUE 'N'.                    
003500                                                                          
003800 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
003900 77  WS-CURRENT                  PIC S9(3)  VALUE +001    COMP-3.         
004000 77  WS-COMING                   PIC S9(3)  VALUE +002    COMP-3.         
004100 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004200 77  WS-ADRESS                   PIC X(50)                                
004210                    VALUE 'CARPARTS.BILLIT.APPRPARTGRLOCATE'.             
004300 77  WS-PERCENTAGE               PIC X      VALUE '%'.                    
004400 77  WS-KDPARTGR                 PIC X(4)   VALUE SPACE.                  
004410                                                                          
004600 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004700     88  KEYS-OK                            VALUE 'Y'.                    
004800     88  KEYS-WRONG                         VALUE 'N'.                    
004900                                                                          
005310*    --- WORK FIELDS                                                      
005400 01  WS-IX-MOD                   PIC S9(4)  VALUE ZERO    BINARY.         
005401 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005500 01  WS-COUNTER-T01CUGR          PIC S9(7)  VALUE ZERO    COMP-3.         
005500 01  WS-COUNTER-T03CUGR          PIC S9(7)  VALUE ZERO    COMP-3.         
005510 01  WS-BELEGRAD-1               PIC X(35)  VALUE SPACE.                  
005520 01  WS-IDLEGSEL                 PIC X(4)   VALUE SPACE.                  
005523 01  WS-KDPARTGR-2               PIC X(2)   VALUE SPACE.                  
005524 01  WS-KDPARTGR-3               PIC X(3)   VALUE SPACE.                  
005525 01  WS-KDPARTGR-4               PIC X(4)   VALUE SPACE.                  
005526 01  WS-KDPARTGR-5               PIC X(5)   VALUE SPACE.                  
005527 01  WS-KDPARTGR-6               PIC X(6)   VALUE SPACE.                  
005528 01  WS-KDPARTGR-7               PIC X(7)   VALUE SPACE.                  
005529 01  WS-KDPARTGR-8               PIC X(8)   VALUE SPACE.                  
005530 01  WS-KDPARTGR-9               PIC X(9)   VALUE SPACE.                  
005532 01  WS-KDPARTGR-10              PIC X(10)  VALUE SPACE.                  
005533 01  WS-KDPARTGR-11              PIC X(11)  VALUE SPACE.                  
005534 01  WS-KDPARTGR-12              PIC X(12)  VALUE SPACE.                  
005535 01  WS-KDPARTGR-13              PIC X(13)  VALUE SPACE.                  
005536 01  WS-KDPARTGR-14              PIC X(14)  VALUE SPACE.                  
005537 01  WS-KDPARTGR-15              PIC X(15)  VALUE SPACE.                  
005538 01  WS-KDPARTGR-16              PIC X(16)  VALUE SPACE.                  
005542                                                                          
005543 01  WS-KDPARTGR-EDIT.                                                    
005550     03 WS-KDPARTGR-OCC16 OCCURS 16 PIC X.                                
005551                                                                          
007710*    --- MAPPING FIELDS                                                   
007711 01  MAP-KDPARTTY-LINE           PIC X(3)   VALUE SPACE.                  
007712 01  MAP-KDPARTGR-LINE           PIC X(15)  VALUE SPACE.                  
007713 01  MAP-KDSTATUS-LINE           PIC S9(3)  VALUE ZERO    COMP-3.         
007714 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
007715 01  MAP-DAUPPDAT-LINE           PIC X(8)   VALUE SPACE.                  
007716 01  MAP-DADELDAT-LINE           PIC X(8)   VALUE SPACE.                  
007717 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
007720                                                                          
008200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008300 01  GENERAL-SUBPROGRAMS.                                                 
008410     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
008500     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
008600                                                                          
008610*    --- PARAMETERS TO ABEND                                              
008620 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008630 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008640 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008650 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
008670                                                                          
008700 01  MESSAGE-CODES.                                                       
008800     03  ERROR-CODES.                                                     
008801         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
008810         05  IS-INVALID              PIC X(3)   VALUE '023'.              
008811         05  MUST-BE-NUMERIC         PIC X(3)   VALUE '024'.              
008812         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
008813         05  MUST-BE-ENTERED         PIC X(3)   VALUE '026'.              
008814         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
008820         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
008830         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
009310     EJECT                                                                
009400                                                                          
009410*01  -COPY WZ01SUB                                                        
009411     EJECT                                                                
009500*                                                                         
009600 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
009800 01  REQU-AREA.                                                           
009900*    03 -COPY WZ01REQU                                                    
010000*    03 -COPY WF0261I1                                                    
010010     EJECT                                                                
010100                                                                          
010200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
010400 01  RESP-AREA.                                                           
010500*    03 -COPY WZ01RESP                                                    
010600*    03 -COPY WF0261O1                                                    
010610     EJECT                                                                
010700                                                                          
010800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
010900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011000                                                                          
011100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
011200 01  DB2-WS.                                                              
011300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
011400         88  CURSOR-OK                      VALUE 000.                    
011500         88  LINES-FOUND                    VALUE 000.                    
011600         88  LINES-MISSING                  VALUE 100.                    
011700         88  RESOURCE-WRONG                 VALUE 904.                    
011710                                                                          
011800     03  GOOD-SQLCODECODES.                                               
011900         05  GOOD-SQLCODE OCCURS 5                                        
012000             INDEXED BY SQLCODE-IX PIC 9(3).                              
012100                                                                          
012200 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
012400*01  -COPY T01LSEL -PRE T01LSEL-                                          
012500     EJECT                                                                
012501                                                                          
012510 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
012600*01  -COPY T01CUGR -PRE T01CUGR-                                          
012700     EJECT                                                                
012800                                                                          
012900     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
013000     EJECT                                                                
013200     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
013300     EJECT                                                                
013310                                                                          
013400 LINKAGE SECTION.                                                         
013700 PROCEDURE DIVISION.                                                      
013800 MAIN SECTION.                                                            
014000                                                                          
014100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014200     IF SUB-KDRC = ZERO                                                   
014300       PERFORM A-INIT                                                     
014400       PERFORM B-CHECK-KEYS                                               
014500       IF KEYS-OK                                                         
014600         PERFORM F-READ-SHOW-INFO                                         
014700       END-IF                                                             
014800       PERFORM S02-RETURN-RESPONSE                                        
014910     END-IF                                                               
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500                                                                          
015700 A-INIT SECTION.                                                          
015900     INITIALIZE GOOD-SQLCODECODES                                         
016001     MOVE ALL '+' TO RESP-AREA                                            
016002     MOVE SPACE TO RESP-IDMSG-ERROR                                       
016003     MOVE SPACE TO RESP-IDMSG-INFO                                        
016004     MOVE SPACE TO RESP-IDELMT-ERROR                                      
016005     MOVE ZERO                  TO WS-COUNTER-T01CUGR                     
016005                                   WS-COUNTER-T03CUGR                     
016006                                   RESP-KVRADER                           
016500     .                                                                    
016600     EJECT                                                                
016700                                                                          
016800*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
016900 B-CHECK-KEYS SECTION.                                                    
017100     MOVE YES TO KEYS-SW                                                  
017152     IF REQU-KDPGMACT = WS-SEARCH                                         
017153     AND REQU-IDMSGVER NUMERIC                                            
017154     AND REQU-IDLEGSEL-KEY > SPACE                                        
017500*      IF  (REQU-KDPARTTY-KEY = SPACE OR = ALL '+')                       
017710*        MOVE NOO TO KEYS-SW                                              
017900*      ELSE                                                               
017910*        IF REQU-KDPARTTY-KEY NOT = SPACE OR NOT = ALL '+'                
017920*          CONTINUE                                                       
017930*        ELSE                                                             
017940*          MOVE NOO TO KEYS-SW                                            
017950*        END-IF                                                           
018100*      END-IF                                                             
018100       CONTINUE                                                           
018200     ELSE                                                                 
018300       MOVE NOO TO KEYS-SW                                                
018400     END-IF                                                               
018410                                                                          
018420     IF REQU-KDPGMACT = 'S'                                               
018430       CONTINUE                                                           
018440     ELSE                                                                 
018441       MOVE NOO TO KEYS-SW                                                
018470     END-IF                                                               
018480     IF REQU-IDMSGVER NUMERIC                                             
018490       CONTINUE                                                           
018491     ELSE                                                                 
018492       MOVE NOO TO KEYS-SW                                                
018495     END-IF                                                               
018496     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
018497       MOVE NOO TO KEYS-SW                                                
018500     ELSE                                                                 
018501       CONTINUE                                                           
018502     END-IF                                                               
018510                                                                          
018540     IF KEYS-WRONG                                                        
018550       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
018560       IF REQU-KDPGMACT = 'S'                                             
018570         CONTINUE                                                         
018580       ELSE                                                               
018590         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018600         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
018700       END-IF                                                             
018800       IF REQU-IDMSGVER NUMERIC                                           
018810         CONTINUE                                                         
018820       ELSE                                                               
018830         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018840         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
018850       END-IF                                                             
018860       IF REQU-IDUSER = SPACE OR = ALL '+'                                
018870         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018880         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
018890       ELSE                                                               
018891         CONTINUE                                                         
018892       END-IF                                                             
018893     END-IF                                                               
018894     IF KEYS-OK                                                           
018895       PERFORM DB2-SELECT-T01LSEL-TAB                                     
018896       IF LINES-FOUND                                                     
018897         CONTINUE                                                         
018898       ELSE                                                               
018899         MOVE NOT-FOUND    TO RESP-IDMSG-ERROR                            
018900         MOVE 'IDLEGSEL'   TO RESP-IDELMT-ERROR                           
018901         MOVE NOO TO KEYS-SW                                              
018902       END-IF                                                             
018903     END-IF                                                               
018904     .                                                                    
018910     EJECT                                                                
019000                                                                          
019200*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
019300 F-READ-SHOW-INFO SECTION.                                                
019500     MOVE REQU-IDLEGSEL-KEY TO RESP-IDLEGSEL-KEY                          
019600     MOVE REQU-KDPARTTY-KEY TO RESP-KDPARTTY-KEY                          
019700     MOVE REQU-KDPARTGR-KEY TO RESP-KDPARTGR-KEY                          
019810     MOVE REQU-IDMSGVER     TO RESP-IDMSGVER                              
019820     MOVE WS-BELEGRAD-1     TO RESP-BELEGRAD-1                            
019900                                                                          
020000     PERFORM FA-READ-BASICDATA                                            
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400*** - CHECK WHICH REQUESTED KEY                                           
020500 FA-READ-BASICDATA SECTION.                                               
020610     MOVE ZERO TO WS-COUNTER-T01CUGR                                      
020610                  WS-COUNTER-T03CUGR                                      
020700*    IF REQU-KDPARTTY-KEY NOT = SPACE OR NOT = ALL '+'                    
020700     IF REQU-KDPARTTY-KEY = SPACE OR ALL '+'                              
020801         PERFORM FAC-HANDLE-KDPARTTY-KEY                                  
           ELSE                                                                 
020800       IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                          
020801         PERFORM FAA-HANDLE-KDPARTTY-KEY                                  
020802       ELSE                                                               
020803         PERFORM FAB-HANDLE-KDPARTGR-KEY                                  
020820       END-IF                                                             
020910     END-IF                                                               
020920                                                                          
022600     MOVE WS-IX              TO RESP-KVRADER                              
023800     .                                                                    
023900     EJECT                                                                
024000                                                                          
024010*** - HANDLE KEY KDPARTTY                                                 
024030 FAA-HANDLE-KDPARTTY-KEY SECTION.                                         
024041     PERFORM DB2-COUNT-CRS-1                                              
024046     IF WS-COUNTER-T01CUGR = ZERO                                         
024051       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
024052     ELSE                                                                 
024053       IF WS-COUNTER-T01CUGR > WS-MAX-LINES                               
024054         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
024055       ELSE                                                               
024056         MOVE WS-COUNTER-T01CUGR TO RESP-KVRADER                          
024057       END-IF                                                             
024059     END-IF                                                               
024060                                                                          
024061     IF RESP-IDMSG-ERROR = SPACE                                          
024062       PERFORM DB2-DCL-OPN-T01CUGR-CRS-1                                  
024063       PERFORM DB2-FETCH-T01CUGR-CRS-1                                    
024064       MOVE ZERO TO WS-IX                                                 
024065                                                                          
024066       PERFORM UNTIL LINES-MISSING                                        
024067         PERFORM S03-MOVE-TO-RESPOND                                      
024068         PERFORM DB2-FETCH-T01CUGR-CRS-1                                  
024069       END-PERFORM                                                        
024070                                                                          
024071       PERFORM DB2-CLOSE-T01CUGR-CRS-1                                    
024072     END-IF                                                               
024073     .                                                                    
024074     EJECT                                                                
024075                                                                          
024076*** - HANDLE KEY KDPARTGR                                                 
024077 FAB-HANDLE-KDPARTGR-KEY SECTION.                                         
024078     PERFORM FABA-EDIT-KDPARTGR-KEY                                       
024079     PERFORM DB2-COUNT-CRS-2                                              
024121     IF WS-COUNTER-T01CUGR = ZERO                                         
024123       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
024127     ELSE                                                                 
024128       IF WS-COUNTER-T01CUGR > WS-MAX-LINES                               
024129         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
024130       ELSE                                                               
024131         MOVE WS-COUNTER-T01CUGR TO RESP-KVRADER                          
024132       END-IF                                                             
024133     END-IF                                                               
024134                                                                          
024135     IF RESP-IDMSG-ERROR = SPACE                                          
024136       PERFORM DB2-DCL-OPN-T01CUGR-CRS-2                                  
024137       PERFORM DB2-FETCH-T01CUGR-CRS-2                                    
024138       MOVE ZERO TO WS-IX                                                 
024139                                                                          
024140       PERFORM UNTIL LINES-MISSING                                        
024141         PERFORM S03-MOVE-TO-RESPOND                                      
024142         PERFORM DB2-FETCH-T01CUGR-CRS-2                                  
024143       END-PERFORM                                                        
024144                                                                          
024145       PERFORM DB2-CLOSE-T01CUGR-CRS-2                                    
024146     END-IF                                                               
024147     .                                                                    
024148     EJECT                                                                
024149                                                                          
024010*** - HANDLE KEY KDPARTTY WHEN SPACES                                     
024030 FAC-HANDLE-KDPARTTY-KEY SECTION.                                         
024041     PERFORM DB2-COUNT-CRS-3                                              
024046     IF WS-COUNTER-T03CUGR = ZERO                                         
024051       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
024052     ELSE                                                                 
024053       IF WS-COUNTER-T03CUGR > WS-MAX-LINES                               
024054         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
024055       ELSE                                                               
024056         MOVE WS-COUNTER-T03CUGR TO RESP-KVRADER                          
024057       END-IF                                                             
024059     END-IF                                                               
024060                                                                          
024061     IF RESP-IDMSG-ERROR = SPACE                                          
024062       PERFORM DB2-DCL-OPN-T01CUGR-CRS-3                                  
024063       PERFORM DB2-FETCH-T01CUGR-CRS-3                                    
024064       MOVE ZERO TO WS-IX                                                 
024065                                                                          
024066       PERFORM UNTIL LINES-MISSING                                        
024067         PERFORM S03-MOVE-TO-RESPOND                                      
024068         PERFORM DB2-FETCH-T01CUGR-CRS-3                                  
024069       END-PERFORM                                                        
024070                                                                          
024071       PERFORM DB2-CLOSE-T01CUGR-CRS-3                                    
024072     END-IF                                                               
024073     .                                                                    
024074     EJECT                                                                
024075                                                                          
024150*** - CHECK NUMBERS OF ENTERED POSITION IN KDPARTGR-KEY AND               
024151***   MOVE '%' AFTER ENTERED POSITION.                                    
024152 FABA-EDIT-KDPARTGR-KEY SECTION.                                          
024153     MOVE SPACE TO WS-KDPARTGR-2                                          
024154                   WS-KDPARTGR-3                                          
024155                   WS-KDPARTGR-4                                          
024156                   WS-KDPARTGR-5                                          
024157                   WS-KDPARTGR-6                                          
024158                   WS-KDPARTGR-7                                          
024159                   WS-KDPARTGR-8                                          
024160                   WS-KDPARTGR-9                                          
024161                   WS-KDPARTGR-10                                         
024162                   WS-KDPARTGR-11                                         
024163                   WS-KDPARTGR-12                                         
024164                   WS-KDPARTGR-13                                         
024165                   WS-KDPARTGR-14                                         
024166                   WS-KDPARTGR-15                                         
024167                   WS-KDPARTGR-16                                         
024168                   WS-KDPARTGR-EDIT                                       
024169     MOVE REQU-KDPARTGR-KEY TO WS-KDPARTGR-EDIT                           
024170     MOVE +15 TO WS-IX-MOD                                                
024171                                                                          
024172     PERFORM UNTIL WS-KDPARTGR-OCC16(WS-IX-MOD) > SPACE                   
024173     OR WS-IX-MOD < +1                                                    
024174        SUBTRACT 1 FROM WS-IX-MOD                                         
024175     END-PERFORM                                                          
024176                                                                          
024177     ADD 1 TO WS-IX-MOD                                                   
024178     MOVE WS-PERCENTAGE TO WS-KDPARTGR-OCC16(WS-IX-MOD)                   
024179                                                                          
024180     IF WS-IX-MOD = +2                                                    
024181       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-2                             
024182     END-IF                                                               
024183                                                                          
024184     IF WS-IX-MOD = +3                                                    
024185       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-3                             
024186     END-IF                                                               
024187                                                                          
024188     IF WS-IX-MOD = +4                                                    
024189       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-4                             
024190     END-IF                                                               
024191                                                                          
024192     IF WS-IX-MOD = +5                                                    
024193       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-5                             
024194     END-IF                                                               
024195                                                                          
024196     IF WS-IX-MOD = +6                                                    
024197       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-6                             
024198     END-IF                                                               
024199                                                                          
024200     IF WS-IX-MOD = +7                                                    
024201       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-7                             
024202     END-IF                                                               
024203                                                                          
024204     IF WS-IX-MOD = +8                                                    
024205       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-8                             
024206     END-IF                                                               
024207                                                                          
024208     IF WS-IX-MOD = +9                                                    
024209       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-9                             
024210     END-IF                                                               
024211                                                                          
024212     IF WS-IX-MOD = +10                                                   
024213       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-10                            
024214     END-IF                                                               
024215                                                                          
024216     IF WS-IX-MOD = +11                                                   
024217       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-11                            
024218     END-IF                                                               
024219                                                                          
024220     IF WS-IX-MOD = +12                                                   
024221       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-12                            
024222     END-IF                                                               
024223                                                                          
024224     IF WS-IX-MOD = +13                                                   
024225       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-13                            
024226     END-IF                                                               
024227                                                                          
024228     IF WS-IX-MOD = +14                                                   
024229       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-14                            
024230     END-IF                                                               
024231                                                                          
024232     IF WS-IX-MOD = +15                                                   
024233       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-15                            
024234     END-IF                                                               
024235                                                                          
024236     IF WS-IX-MOD = +16                                                   
024237       MOVE WS-KDPARTGR-EDIT TO WS-KDPARTGR-16                            
024238     END-IF                                                               
024239     .                                                                    
024240     EJECT                                                                
029300                                                                          
029400*   --- DISPATCHER SECTION START                                          
029500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
029700     MOVE 'GETARG'             TO SUB-KDFUNC                              
029800     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
029910     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
030000                                                                          
030100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
030110                                                                          
030120     IF SUB-KDRC > 0                                                      
030130       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
030140       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
030150       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030160       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030170     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030310                                                                          
030400 S02-RETURN-RESPONSE SECTION.                                             
030600     MOVE 'RETURN'             TO SUB-KDFUNC                              
030610                                                                          
030680     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
030690                              - ((WS-MAX-LINES - WS-IX)                   
030700                              * LENGTH OF RESP-TABELLRAD)                 
030810                                                                          
030900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
030910                                                                          
030920     IF SUB-KDRC > 0                                                      
030930       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
030940       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
030950       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030960       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030970     END-IF                                                               
031000     .                                                                    
031010     EJECT                                                                
031100                                                                          
031101*   --- MOVE TO OUTPUT SECTION START                                      
031110*** - MOVE DATA TO RESPOND WHEN CURRENT LINE,                             
031120***   WHEN COMING LINE MODIFY CURRENT LINE.                               
031130 S03-MOVE-TO-RESPOND SECTION.                                             
031150     IF MAP-KDSTATUS-LINE = WS-CURRENT                                    
031160       ADD 1 TO WS-IX                                                     
031170       MOVE MAP-KDPARTTY-LINE     TO RESP-KDPARTTY-LINE(WS-IX)            
031171       MOVE MAP-KDPARTGR-LINE     TO RESP-KDPARTGR-LINE(WS-IX)            
031180       MOVE MAP-DAREGDAT-LINE     TO RESP-DAREGDAT-LINE(WS-IX)            
031190       MOVE MAP-DAUPPDAT-LINE     TO RESP-DAUPPDAT-LINE(WS-IX)            
031191       MOVE MAP-DADELDAT-LINE     TO RESP-DADELDAT-LINE(WS-IX)            
031192       MOVE MAP-IDUSER-LINE       TO RESP-IDUSER-LINE(WS-IX)              
031195       MOVE NOO                   TO RESP-FLCOMING-LINE(WS-IX)            
031196     ELSE                                                                 
031197       IF  MAP-KDSTATUS-LINE = WS-COMING                                  
031198       AND MAP-KDPARTTY-LINE =  RESP-KDPARTTY-LINE(WS-IX)                 
031199         MOVE YES TO RESP-FLCOMING-LINE(WS-IX)                            
031201       END-IF                                                             
031203     END-IF                                                               
031206     .                                                                    
031207     EJECT                                                                
031220                                                                          
031300*   --- DB2 SECTIONS                                                      
031500*** - CHECK THAT THE REQUESTED LEGAL SELLER EXIST                         
031600 DB2-SELECT-T01LSEL-TAB SECTION.                                          
031900     MOVE 000100 TO GOOD-SQLCODECODES                                     
032000                                                                          
032100     EXEC SQL                                                             
032211           SELECT  IDLEGSEL                                               
032212                 , BELEGRAD_1                                             
032220                                                                          
032290           INTO   :WS-IDLEGSEL                                            
032291                , :WS-BELEGRAD-1                                          
032300                                                                          
032400           FROM    T01LSEL                                                
032500                                                                          
032700           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
032800               AND KDSTATUS = :WS-CURRENT                                 
032900     END-EXEC                                                             
033231                                                                          
033240     MOVE SQLCODE TO SQLCODE-WS                                           
033250     PERFORM DB2-STATUS-CHECK                                             
033900     .                                                                    
034000     EJECT                                                                
034100                                                                          
034200* * * * * * * * * *   - CURSOR-1 -   * * * * * * * * * * * * * * *        
034220 DB2-COUNT-CRS-1 SECTION.                                                 
034240     EXEC SQL                                                             
034250                                                                          
034260           SELECT COUNT(*)                                                
034270                                                                          
034280           INTO  :WS-COUNTER-T01CUGR                                      
034290                                                                          
034291           FROM   T01CUGR                                                 
034292                                                                          
034296           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
034297                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
034298                AND DADELDAT = '00000000'                                 
034299                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
034300                                                                          
034301     END-EXEC                                                             
034302                                                                          
034303     MOVE 000100  TO GOOD-SQLCODECODES                                    
034304                                                                          
034305     MOVE SQLCODE TO SQLCODE-WS                                           
034306     PERFORM DB2-STATUS-CHECK                                             
034307     .                                                                    
034308     EJECT                                                                
034309                                                                          
034310 DB2-DCL-OPN-T01CUGR-CRS-1 SECTION.                                       
034600     MOVE 000100 TO GOOD-SQLCODECODES                                     
034700                                                                          
034800     EXEC SQL                                                             
034900         DECLARE T01CUGR-CRS-1 CURSOR WITH HOLD FOR                       
035000                                                                          
035200           SELECT  KDPARTTY                                               
035210                 , KDPARTGR                                               
035300                 , KDSTATUS                                               
035400                 , DAREGDAT                                               
035500                 , DAUPPDAT                                               
035600                 , DADELDAT                                               
035700                 , IDUSER                                                 
036000                                                                          
036100           FROM    T01CUGR                                                
036200                                                                          
036300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
036400                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
036410                AND DADELDAT = '00000000'                                 
036500                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
036600                                                                          
036800           ORDER BY IDLEGSEL                                              
036900                  , KDPARTTY                                              
036910                  , KDPARTGR                                              
037000                  , KDSTATUS                                              
037100     END-EXEC                                                             
037200                                                                          
037300     MOVE 000100  TO GOOD-SQLCODECODES                                    
037400                                                                          
037500     EXEC SQL                                                             
037600        OPEN T01CUGR-CRS-1                                                
037700     END-EXEC                                                             
037800                                                                          
037810     MOVE SQLCODE TO SQLCODE-WS                                           
037820     PERFORM DB2-STATUS-CHECK                                             
038500     .                                                                    
038501     EJECT                                                                
038510                                                                          
038530 DB2-FETCH-T01CUGR-CRS-1 SECTION.                                         
038550     MOVE 000100  TO GOOD-SQLCODECODES                                    
038560                                                                          
038570     EXEC SQL                                                             
038580                                                                          
038590         FETCH T01CUGR-CRS-1                                              
038591                                                                          
038592         INTO :MAP-KDPARTTY-LINE                                          
038593            , :MAP-KDPARTGR-LINE                                          
038594            , :MAP-KDSTATUS-LINE                                          
038595            , :MAP-DAREGDAT-LINE                                          
038596            , :MAP-DAUPPDAT-LINE                                          
038597            , :MAP-DADELDAT-LINE                                          
038598            , :MAP-IDUSER-LINE                                            
038600     END-EXEC                                                             
038601                                                                          
038609     MOVE SQLCODE TO SQLCODE-WS                                           
038610     PERFORM DB2-STATUS-CHECK                                             
038614     .                                                                    
038615     EJECT                                                                
038616                                                                          
038617 DB2-CLOSE-T01CUGR-CRS-1 SECTION.                                         
038619     EXEC SQL                                                             
038620        CLOSE T01CUGR-CRS-1                                               
038621     END-EXEC                                                             
038622     .                                                                    
038623     EJECT                                                                
038624                                                                          
038625* * * * * * * * * *   - CURSOR-2 -   * * * * * * * * * * * * * * *        
038800 DB2-COUNT-CRS-2 SECTION.                                                 
039000     EXEC SQL                                                             
039100                                                                          
039200          SELECT COUNT(*)                                                 
039300                                                                          
039400          INTO  :WS-COUNTER-T01CUGR                                       
039500                                                                          
039600          FROM   T01CUGR                                                  
039700                                                                          
039800          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
039810             AND KDPARTTY = :REQU-KDPARTTY-KEY                            
039820             AND DADELDAT = '00000000'                                    
039900             AND KDSTATUS = :WS-CURRENT                                   
040000             AND (KDPARTGR LIKE :WS-KDPARTGR-2                            
040010             OR   KDPARTGR LIKE :WS-KDPARTGR-3                            
040020             OR   KDPARTGR LIKE :WS-KDPARTGR-4                            
040030             OR   KDPARTGR LIKE :WS-KDPARTGR-5                            
040040             OR   KDPARTGR LIKE :WS-KDPARTGR-6                            
040050             OR   KDPARTGR LIKE :WS-KDPARTGR-7                            
040060             OR   KDPARTGR LIKE :WS-KDPARTGR-8                            
040070             OR   KDPARTGR LIKE :WS-KDPARTGR-9                            
040080             OR   KDPARTGR LIKE :WS-KDPARTGR-10                           
040081             OR   KDPARTGR LIKE :WS-KDPARTGR-11                           
040082             OR   KDPARTGR LIKE :WS-KDPARTGR-12                           
040083             OR   KDPARTGR LIKE :WS-KDPARTGR-13                           
040084             OR   KDPARTGR LIKE :WS-KDPARTGR-14                           
040085             OR   KDPARTGR LIKE :WS-KDPARTGR-15                           
040090             OR   KDPARTGR LIKE :WS-KDPARTGR-16)                          
040100                                                                          
040200     END-EXEC                                                             
040300                                                                          
040400     MOVE 000100  TO GOOD-SQLCODECODES                                    
040500                                                                          
040510     MOVE SQLCODE TO SQLCODE-WS                                           
040520     PERFORM DB2-STATUS-CHECK                                             
042000     .                                                                    
042010     EJECT                                                                
042100                                                                          
042560 DB2-DCL-OPN-T01CUGR-CRS-2 SECTION.                                       
042700     MOVE 000100 TO GOOD-SQLCODECODES                                     
042800                                                                          
042900     EXEC SQL                                                             
043000         DECLARE T01CUGR-CRS-2 CURSOR WITH HOLD FOR                       
043100                                                                          
043300          SELECT   KDPARTTY                                               
043310                 , KDPARTGR                                               
043400                 , KDSTATUS                                               
043500                 , DAREGDAT                                               
043600                 , DAUPPDAT                                               
043700                 , DADELDAT                                               
043800                 , IDUSER                                                 
044100                                                                          
044200          FROM     T01CUGR                                                
044300                                                                          
044400          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
044500             AND   KDPARTTY = :REQU-KDPARTTY-KEY                          
044510             AND   DADELDAT = '00000000'                                  
044520             AND (KDPARTGR LIKE :WS-KDPARTGR-2                            
044530             OR   KDPARTGR LIKE :WS-KDPARTGR-3                            
044540             OR   KDPARTGR LIKE :WS-KDPARTGR-4                            
044550             OR   KDPARTGR LIKE :WS-KDPARTGR-5                            
044560             OR   KDPARTGR LIKE :WS-KDPARTGR-6                            
044570             OR   KDPARTGR LIKE :WS-KDPARTGR-7                            
044580             OR   KDPARTGR LIKE :WS-KDPARTGR-8                            
044590             OR   KDPARTGR LIKE :WS-KDPARTGR-9                            
044591             OR   KDPARTGR LIKE :WS-KDPARTGR-10                           
044592             OR   KDPARTGR LIKE :WS-KDPARTGR-11                           
044593             OR   KDPARTGR LIKE :WS-KDPARTGR-12                           
044594             OR   KDPARTGR LIKE :WS-KDPARTGR-13                           
044595             OR   KDPARTGR LIKE :WS-KDPARTGR-14                           
044596             OR   KDPARTGR LIKE :WS-KDPARTGR-15                           
044597             OR   KDPARTGR LIKE :WS-KDPARTGR-16)                          
044600                                                                          
044800          ORDER BY IDLEGSEL                                               
044900                 , KDPARTTY                                               
044910                 , KDPARTGR                                               
045000                 , KDSTATUS                                               
045100     END-EXEC                                                             
045200                                                                          
045300     MOVE 000100  TO GOOD-SQLCODECODES                                    
045400                                                                          
045500     EXEC SQL                                                             
045600        OPEN T01CUGR-CRS-2                                                
045700     END-EXEC                                                             
045800                                                                          
045810     MOVE SQLCODE TO SQLCODE-WS                                           
045820     PERFORM DB2-STATUS-CHECK                                             
046500     .                                                                    
046600     EJECT                                                                
046610                                                                          
046620 DB2-FETCH-T01CUGR-CRS-2 SECTION.                                         
046640     MOVE 000100  TO GOOD-SQLCODECODES                                    
046650                                                                          
046660     EXEC SQL                                                             
046670                                                                          
046680         FETCH T01CUGR-CRS-2                                              
046690                                                                          
046691         INTO :MAP-KDPARTTY-LINE                                          
046692            , :MAP-KDPARTGR-LINE                                          
046693            , :MAP-KDSTATUS-LINE                                          
046694            , :MAP-DAREGDAT-LINE                                          
046695            , :MAP-DAUPPDAT-LINE                                          
046696            , :MAP-DADELDAT-LINE                                          
046697            , :MAP-IDUSER-LINE                                            
046700     END-EXEC                                                             
046701                                                                          
046708     MOVE SQLCODE TO SQLCODE-WS                                           
046709     PERFORM DB2-STATUS-CHECK                                             
046713     .                                                                    
046714     EJECT                                                                
046715                                                                          
046716 DB2-CLOSE-T01CUGR-CRS-2 SECTION.                                         
046718     EXEC SQL                                                             
046719        CLOSE T01CUGR-CRS-2                                               
046720     END-EXEC                                                             
046721     .                                                                    
046722     EJECT                                                                
058900                                                                          
034200* * * * * * * * * *   - CURSOR-3 -   * * * * * * * * * * * * * * *        
034220 DB2-COUNT-CRS-3 SECTION.                                                 
034240     EXEC SQL                                                             
034250                                                                          
034260           SELECT COUNT(*)                                                
034270                                                                          
034280           INTO  :WS-COUNTER-T03CUGR                                      
034290                                                                          
034291           FROM   T01CUGR                                                 
034292                                                                          
034296           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
034298                AND DADELDAT = '00000000'                                 
034299                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
034300                                                                          
034301     END-EXEC                                                             
034302                                                                          
034303     MOVE 000100  TO GOOD-SQLCODECODES                                    
034304                                                                          
034305     MOVE SQLCODE TO SQLCODE-WS                                           
034306     PERFORM DB2-STATUS-CHECK                                             
034307     .                                                                    
034308     EJECT                                                                
034309                                                                          
034310 DB2-DCL-OPN-T01CUGR-CRS-3 SECTION.                                       
034600     MOVE 000100 TO GOOD-SQLCODECODES                                     
034700                                                                          
034800     EXEC SQL                                                             
034900         DECLARE T01CUGR-CRS-3 CURSOR WITH HOLD FOR                       
035000                                                                          
035200           SELECT  KDPARTTY                                               
035210                 , KDPARTGR                                               
035300                 , KDSTATUS                                               
035400                 , DAREGDAT                                               
035500                 , DAUPPDAT                                               
035600                 , DADELDAT                                               
035700                 , IDUSER                                                 
036000                                                                          
036100           FROM    T01CUGR                                                
036200                                                                          
036300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
036410                AND DADELDAT = '00000000'                                 
036500                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
036600                                                                          
036800           ORDER BY IDLEGSEL                                              
036900                  , KDPARTTY                                              
036910                  , KDPARTGR                                              
037000                  , KDSTATUS                                              
037100     END-EXEC                                                             
037200                                                                          
037300     MOVE 000100  TO GOOD-SQLCODECODES                                    
037400                                                                          
037500     EXEC SQL                                                             
037600        OPEN T01CUGR-CRS-3                                                
037700     END-EXEC                                                             
037800                                                                          
037810     MOVE SQLCODE TO SQLCODE-WS                                           
037820     PERFORM DB2-STATUS-CHECK                                             
038500     .                                                                    
038501     EJECT                                                                
038510                                                                          
038530 DB2-FETCH-T01CUGR-CRS-3 SECTION.                                         
038550     MOVE 000100  TO GOOD-SQLCODECODES                                    
038560                                                                          
038570     EXEC SQL                                                             
038580                                                                          
038590         FETCH T01CUGR-CRS-3                                              
038591                                                                          
038592         INTO :MAP-KDPARTTY-LINE                                          
038593            , :MAP-KDPARTGR-LINE                                          
038594            , :MAP-KDSTATUS-LINE                                          
038595            , :MAP-DAREGDAT-LINE                                          
038596            , :MAP-DAUPPDAT-LINE                                          
038597            , :MAP-DADELDAT-LINE                                          
038598            , :MAP-IDUSER-LINE                                            
038600     END-EXEC                                                             
038601                                                                          
038609     MOVE SQLCODE TO SQLCODE-WS                                           
038610     PERFORM DB2-STATUS-CHECK                                             
038614     .                                                                    
038615     EJECT                                                                
038616                                                                          
038617 DB2-CLOSE-T01CUGR-CRS-3 SECTION.                                         
038619     EXEC SQL                                                             
038620        CLOSE T01CUGR-CRS-3                                               
038621     END-EXEC                                                             
038622     .                                                                    
038623     EJECT                                                                
038624                                                                          
058910 DB2-STATUS-CHECK  SECTION.                                               
059100     SET SQLCODE-IX TO 1                                                  
059200     SEARCH GOOD-SQLCODE                                                  
059300       AT END                                                             
059310          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
059320          DELIMITED BY SIZE INTO ERROR-TEXT                               
059340          CALL ABEND USING RKOD-ABEND-DB2                                 
059400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
059410          CONTINUE                                                        
059500     END-SEARCH                                                           
059600     .                                                                    
