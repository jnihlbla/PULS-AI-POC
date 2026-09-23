000100 PROCESS DYNAM                                                            
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF025300.                                                
001400 AUTHOR.         HENRIKSSON ANDERS.                                       
001500 DATE-WRITTEN.   02/02/25.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001710*    NAME                                                                 
001720*        CARPARTS.BILLIT.SYSTEMMAINTENANCE                                
001800*    FUNCTION:                                                            
001900*        SYSTEM MAINTENANCE                                               
002000*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
002100*                                                                         
002210*        THE PROGRAM UPDATES TABLE T01SYST                                
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: WF0253T                                             
002600*        REQUEST:     WF0253I1                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        RESPONSE:    WF0253O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003400 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
004000 DATA DIVISION.                                                           
004200 FILE SECTION.                                                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WF025300'.            
004700                                                                          
004800*    --- WORK FIELDS FOR ERROR MESSAGE WHEN CALLING ABEND.                
004900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100                                                                          
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006200     88  KEYS-OK                             VALUE 'Y'.                   
006300     88  KEYS-WRONG                          VALUE 'N'.                   
006400     EJECT                                                                
006410                                                                          
006500*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007100     SKIP3                                                                
007200*    --- PARAMETRAR TILL ABEND                                            
007300                                                                          
007400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007710 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007800     SKIP3                                                                
007810                                                                          
007900 01  MESSAGE-CODES.                                                       
008100     03  ERROR-CODES.                                                     
008110       05 NOTHING-HAS-BEEN-UPDATED   PIC X(3)    VALUE '004'.             
008200       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
008201       05 IS-INVALID                 PIC X(3)    VALUE '023'.             
008202       05 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.             
008203       05 NOT-FOUND                  PIC X(3)    VALUE '025'.             
008210       05 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.             
008300       05 LINE-NOT-FOUND             PIC X(3)    VALUE '027'.             
008310       05 SYSTEM-ERROR               PIC X(3)    VALUE '099'.             
008400     03  INFO-CODES.                                                      
008500       05 OK-UPDATE-DONE             PIC X(3)    VALUE '001'.             
008600     EJECT                                                                
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008900     SKIP3                                                                
009000 01  -COPY WZ01SUB                                                        
009100     EJECT                                                                
009110                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009300     SKIP3                                                                
009400 01  REQU-AREA.                                                           
009500*    03  -COPY WZ01REQU                                                   
009600*    03  -COPY WF0253I1                                                   
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009900     SKIP3                                                                
010000 01  RESP-AREA.                                                           
010100*    03  -COPY WZ01RESP                                                   
010200*    03  -COPY WF0253O1                                                   
010300     EJECT                                                                
010401     EJECT                                                                
010402 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010403       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010404                                                                          
010405 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
010406 01  DB2-WS.                                                              
010407     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
010408         88  CURSOR-OK                       VALUE 000.                   
010409         88  LINES-FOUND                     VALUE 000.                   
010411         88  LINES-MISSING                   VALUE 100.                   
010412         88  RESOURCE-WRONG                  VALUE 904.                   
010413                                                                          
010414     03  T01SYST-WS              PIC 9(3)    VALUE ZERO.                  
010416         88  UPDATE-OK                       VALUE 000.                   
010417         88  UPDATE-NOO                      VALUE 100.                   
010418                                                                          
010419     03  GOOD-SQLCODECODES.                                               
010420         05  GOOD-SQLCODE OCCURS 5                                        
010430             INDEXED BY SQLCODE-IX PIC 9(3).                              
010500     EJECT                                                                
010510     03 WS-IDMSGVER             PIC 9(3)     VALUE ZERO.                  
010520     03 WS-KDPGMACT             PIC X(1)     VALUE SPACE.                 
010530     03 WS-IDUSER               PIC X(8)     VALUE SPACE.                 
010540     03 WS-IDSYSTEM             PIC X(4)     VALUE SPACE.                 
010550     03 WS-KVMINUT              PIC Z(4)9(1) VALUE ZERO.                  
010560     03 WS-KVDAGAR              PIC Z9(1)    VALUE ZERO.                  
010570     03 WS-FLKLAR               PIC X(1)     VALUE SPACE.                 
010580     03 WS-DAREGDAT             PIC X(8)     VALUE ZERO.                  
010590     03 WS-DAUPPDAT             PIC X(8)     VALUE ZERO.                  
010591     03 WS-DAUPPDAT-2           PIC X(8)     VALUE ZERO.                  
010592     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
010593     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
010594     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
010595                                                                          
010596     03 SYST-KVMINUT            PIC S9(5)    COMP-3 VALUE ZERO.           
010597     03 SYST-KVDAGAR            PIC S9(3)    COMP-3 VALUE ZERO.           
010600                                                                          
010801     EJECT                                                                
010802                                                                          
010803 01  FILLER                    PIC X(16)    VALUE 'T01SYST-AREA'.         
010810*01  -COPY T01SYST -PRE T01SYST-                                          
010901     EJECT                                                                
010902                                                                          
010910     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
011000     EJECT                                                                
011010                                                                          
011100 LINKAGE SECTION.                                                         
011501 PROCEDURE DIVISION.                                                      
011502 MAIN SECTION.                                                            
011600                                                                          
011800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
011900     IF SUB-KDRC = ZERO                                                   
012000       PERFORM A-INIT                                                     
012100       PERFORM B-CHECK-KEYS                                               
012101       IF KEYS-OK                                                         
012110         PERFORM C-CHECK-INDATA                                           
012120       END-IF                                                             
012200       IF KEYS-OK                                                         
012600         PERFORM D-PERFORM-REQUEST                                        
012700       END-IF                                                             
012710       PERFORM E-READ-SHOW-INFO                                           
012800       PERFORM S02-RETURN-RESPOND                                         
012900     END-IF                                                               
013200                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013710                                                                          
013800 A-INIT SECTION.                                                          
014312     MOVE YES TO KEYS-SW                                                  
014313                                                                          
014314     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAUPPDAT                      
014315     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAUPPDAT-2                    
014316                                                                          
014317     MOVE ALL '+' TO RESP-AREA                                            
014319     MOVE SPACE TO RESP-IDMSG-ERROR                                       
014320     MOVE SPACE TO RESP-IDMSG-INFO                                        
014321     MOVE SPACE TO RESP-IDELMT-ERROR                                      
014322                                                                          
014330     INITIALIZE GOOD-SQLCODECODES                                         
014600     .                                                                    
014700     EJECT                                                                
014710                                                                          
014800 B-CHECK-KEYS SECTION.                                                    
015200     IF REQU-KDPGMACT = 'S'                                               
015210     AND REQU-IDMSGVER NUMERIC                                            
015300       CONTINUE                                                           
015310     ELSE                                                                 
015320       IF REQU-KDPGMACT = 'U'                                             
015321       AND REQU-IDMSGVER NUMERIC                                          
015330         CONTINUE                                                         
015340       ELSE                                                               
015341         MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                             
015350         MOVE NOO TO KEYS-SW                                              
015370       END-IF                                                             
015380     END-IF                                                               
015400                                                                          
015410     IF REQU-IDSYSTEM-KEY = SPACE OR = ALL '+'                            
015422       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015430       MOVE NOO TO KEYS-SW                                                
015440     END-IF                                                               
015450                                                                          
015460     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
015461       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015490       MOVE NOO TO KEYS-SW                                                
015500     END-IF                                                               
015510                                                                          
015600     IF REQU-KVMINUT = SPACE OR = ALL '+'                                 
015610       MOVE ZERO TO WS-KVMINUT                                            
015620       MOVE WS-KVMINUT            TO SYST-KVMINUT                         
015730     END-IF                                                               
015731                                                                          
015740     IF REQU-KVDAGAR = SPACE OR = ALL '+'                                 
015750       MOVE ZERO TO WS-KVDAGAR                                            
015751       MOVE WS-KVDAGAR            TO SYST-KVDAGAR                         
015760     END-IF                                                               
015770                                                                          
015780     IF KEYS-WRONG                                                        
015781       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015782       IF REQU-KDPGMACT = 'S' OR = 'U'                                    
015783         CONTINUE                                                         
015784       ELSE                                                               
015785         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015786         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
015787       END-IF                                                             
015790       IF REQU-IDMSGVER NUMERIC                                           
015791         CONTINUE                                                         
015792       ELSE                                                               
015793         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015794         MOVE 'IDMSGVER'   TO WS-IDELMT-ERROR                             
015795       END-IF                                                             
015796       IF REQU-IDUSER = SPACE OR = ALL '+'                                
015797         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015798         MOVE 'IDUSER'     TO WS-IDELMT-ERROR                             
015799       ELSE                                                               
015800         CONTINUE                                                         
015801       END-IF                                                             
015802     END-IF                                                               
015810     .                                                                    
015900     EJECT                                                                
016000                                                                          
016100 C-CHECK-INDATA SECTION.                                                  
016200     IF REQU-KDPGMACT = 'U'                                               
016201       IF REQU-KVMINUT NOT NUMERIC                                        
016202         MOVE IS-INVALID TO WS-IDMSG-ERROR                                
016203         MOVE 'KVMINUT'  TO WS-IDELMT-ERROR                               
016204         MOVE NOO TO KEYS-SW                                              
016205       ELSE                                                               
016206         IF REQU-KVMINUT < 1                                              
016207           MOVE IS-INVALID TO WS-IDMSG-ERROR                              
016208           MOVE 'KVMINUT'  TO WS-IDELMT-ERROR                             
016209           MOVE NOO TO KEYS-SW                                            
016210         END-IF                                                           
016211         IF REQU-KVMINUT > 60                                             
016212           MOVE IS-INVALID TO WS-IDMSG-ERROR                              
016213           MOVE 'KVMINUT'  TO WS-IDELMT-ERROR                             
016214           MOVE NOO TO KEYS-SW                                            
016215         END-IF                                                           
016216       END-IF                                                             
016217       IF REQU-KVDAGAR NOT NUMERIC                                        
016218         MOVE IS-INVALID TO WS-IDMSG-ERROR                                
016219         MOVE 'KVDAGAR'  TO WS-IDELMT-ERROR                               
016220         MOVE NOO TO KEYS-SW                                              
016221       ELSE                                                               
016222         IF REQU-KVDAGAR < 1                                              
016223           MOVE IS-INVALID TO WS-IDMSG-ERROR                              
016224           MOVE 'KVDAGAR'  TO WS-IDELMT-ERROR                             
016225           MOVE NOO TO KEYS-SW                                            
016226         END-IF                                                           
016227         IF REQU-KVDAGAR > 5                                              
016228           MOVE IS-INVALID TO WS-IDMSG-ERROR                              
016229           MOVE 'KVDAGAR'  TO WS-IDELMT-ERROR                             
016230           MOVE NOO TO KEYS-SW                                            
016231         END-IF                                                           
016232       END-IF                                                             
016233     END-IF                                                               
016234     .                                                                    
016235     EJECT                                                                
016236                                                                          
016240 D-PERFORM-REQUEST SECTION.                                               
016300     IF REQU-KDPGMACT = 'S'                                               
016310       PERFORM DB2-SELECT-T01SYST                                         
016311       IF LINES-FOUND                                                     
016312         CONTINUE                                                         
016313       ELSE                                                               
016314         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
016315         MOVE NOT-FOUND    TO WS-IDMSG-ERROR                              
016316         MOVE NOO          TO KEYS-SW                                     
016317       END-IF                                                             
016320     END-IF                                                               
016330     IF REQU-KDPGMACT = 'U'                                               
016331       PERFORM DB2-SELECT-T01SYST                                         
016332       IF LINES-FOUND                                                     
016333         MOVE REQU-KVMINUT TO SYST-KVMINUT                                
016334         MOVE REQU-KVDAGAR TO SYST-KVDAGAR                                
016335         MOVE REQU-IDUSER  TO WS-IDUSER                                   
016336         MOVE WS-DAUPPDAT-2 TO WS-DAUPPDAT                                
016340         PERFORM DB2-UPDATE-T01SYST                                       
016341         IF UPDATE-OK                                                     
016342           MOVE OK-UPDATE-DONE TO WS-IDMSG-INFO                           
016343         ELSE                                                             
016344           MOVE NOTHING-HAS-BEEN-UPDATED TO WS-IDMSG-ERROR                
016345           MOVE NOO TO KEYS-SW                                            
016346         END-IF                                                           
016347       ELSE                                                               
016348         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
016350         MOVE NOT-FOUND    TO WS-IDMSG-ERROR                              
016351         MOVE NOO          TO KEYS-SW                                     
016352       END-IF                                                             
016360     END-IF                                                               
017100     .                                                                    
017200     EJECT                                                                
017300                                                                          
017400 E-READ-SHOW-INFO SECTION.                                                
017401     IF KEYS-WRONG                                                        
017402       MOVE REQU-IDMSGVER            TO RESP-IDMSGVER                     
017403       MOVE WS-IDMSG-INFO            TO RESP-IDMSG-INFO                   
017404       MOVE WS-IDMSG-ERROR           TO RESP-IDMSG-ERROR                  
017405       MOVE WS-IDELMT-ERROR          TO RESP-IDELMT-ERROR                 
017406       MOVE REQU-IDSYSTEM-KEY        TO RESP-IDSYSTEM-KEY                 
017407       MOVE REQU-FLKLAR              TO RESP-FLKLAR                       
017408       MOVE REQU-KVMINUT             TO RESP-KVMINUT                      
017409       MOVE REQU-KVDAGAR             TO RESP-KVDAGAR                      
017410       MOVE REQU-DAREGDAT            TO RESP-DAREGDAT                     
017411       MOVE REQU-DAUPPDAT            TO RESP-DAUPPDAT                     
017412       MOVE REQU-IDUSER              TO RESP-IDUSER                       
017413     ELSE                                                                 
017414       MOVE SYST-KVMINUT               TO WS-KVMINUT                      
017420       MOVE SYST-KVDAGAR               TO WS-KVDAGAR                      
017500       IF REQU-KDPGMACT = 'S' OR = 'U'                                    
017510         IF WS-FLKLAR > SPACE                                             
017520           MOVE 'Y' TO WS-FLKLAR                                          
017530         END-IF                                                           
017600         MOVE REQU-IDMSGVER            TO RESP-IDMSGVER                   
017700         MOVE WS-IDMSG-INFO            TO RESP-IDMSG-INFO                 
017800         MOVE WS-IDMSG-ERROR           TO RESP-IDMSG-ERROR                
017810         MOVE WS-IDELMT-ERROR          TO RESP-IDELMT-ERROR               
017900         MOVE WS-IDSYSTEM              TO RESP-IDSYSTEM-KEY               
018000         MOVE WS-KVMINUT               TO RESP-KVMINUT                    
018100         MOVE WS-KVDAGAR               TO RESP-KVDAGAR                    
018110         MOVE WS-IDUSER                TO RESP-IDUSER                     
018200         MOVE WS-FLKLAR                TO RESP-FLKLAR                     
018300         MOVE WS-DAREGDAT              TO RESP-DAREGDAT                   
018400         MOVE WS-DAUPPDAT              TO RESP-DAUPPDAT                   
018410       END-IF                                                             
018500     END-IF                                                               
018693     .                                                                    
018694     EJECT                                                                
018700                                                                          
019100*    --- DISPATCHER-SECTIONS                                              
019200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019400     MOVE 'GETARG'                            TO SUB-KDFUNC               
019500     MOVE 'CARPARTS.BILLIT.SYSTEMMAINTENANCE' TO SUB-ADDISPABS            
019600     MOVE LENGTH OF REQU-AREA                 TO SUB-KVDLEN               
019700                                                                          
019800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
019900                                                                          
020000     IF SUB-KDRC > 0                                                      
020100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
020300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020500     END-IF                                                               
020600     .                                                                    
020700     SKIP3                                                                
020710                                                                          
020800 S02-RETURN-RESPOND SECTION.                                              
021000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
021200                                                                          
021300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021400                                                                          
021500     IF SUB-KDRC > 0                                                      
021600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022000     END-IF                                                               
022100     .                                                                    
022200     EJECT                                                                
022802                                                                          
022825 DB2-SELECT-T01SYST SECTION.                                              
022826     MOVE 000100  TO GOOD-SQLCODECODES                                    
022827     EXEC SQL                                                             
022828         SELECT  IDSYSTEM,                                                
022829                 KVMINUT,                                                 
022830                 KVDAGAR,                                                 
022831                 KDBEH,                                                   
022832                 DAREGDAT,                                                
022833                 DAUPPDAT,                                                
022834                 IDUSER                                                   
022835                                                                          
022836         INTO :WS-IDSYSTEM,                                               
022837              :SYST-KVMINUT,                                              
022838              :SYST-KVDAGAR,                                              
022839              :WS-FLKLAR,                                                 
022840              :WS-DAREGDAT,                                               
022841              :WS-DAUPPDAT,                                               
022842              :WS-IDUSER                                                  
022843                                                                          
022844         FROM    T01SYST                                                  
022845                                                                          
022846         WHERE   IDSYSTEM = 'WF02'                                        
022847                                                                          
022848     END-EXEC                                                             
022849     MOVE SQLCODE TO SQLCODE-WS                                           
022850     PERFORM DB2-STATUS-CHECK                                             
022851     .                                                                    
022852     EJECT                                                                
022853                                                                          
022854 DB2-UPDATE-T01SYST SECTION.                                              
022855     MOVE 000     TO GOOD-SQLCODECODES                                    
022856     EXEC SQL                                                             
022857         UPDATE T01SYST                                                   
022858         SET KVMINUT  = :SYST-KVMINUT,                                    
022859             KVDAGAR  = :SYST-KVDAGAR,                                    
022860             DAUPPDAT = :WS-DAUPPDAT,                                     
022861             IDUSER   = :REQU-IDUSER                                      
022862         WHERE   IDSYSTEM = 'WF02'                                        
022863     END-EXEC                                                             
022864                                                                          
022865     MOVE SQLCODE TO SQLCODE-WS                                           
022866                     T01SYST-WS                                           
022867     PERFORM DB2-STATUS-CHECK                                             
022870     .                                                                    
022901     EJECT                                                                
022902                                                                          
022903 DB2-STATUS-CHECK     SECTION.                                            
022905     SET SQLCODE-IX TO 1                                                  
022906     SEARCH GOOD-SQLCODE                                                  
022907       AT END                                                             
022908          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
022909          DELIMITED BY SIZE INTO ERROR-TEXT                               
022910          CALL ABEND USING RKOD-ABEND-DB2                                 
022911       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
022912     END-SEARCH                                                           
022920     .                                                                    
