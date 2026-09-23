000100 PROCESS DYNAM                                                            
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF027900.                                                
001400 AUTHOR.         HAMMARIN BO.                                             
001500 DATE-WRITTEN.   JANUARY 2004.                                            
001600 DATE-COMPILED.                                                           
001700                                                                          
001710*    NAME                                                                 
001720*        CARPARTS.BILLIT.APPROVEDVATMAINTENANCE                           
001800*    FUNCTION:                                                            
001900*        MAINTENANCE VAT.                                                 
002000*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
002100*                                                                         
002201*        THE PROGRAM READS     TABLE T01LSEL                              
002210*        THE PROGRAM UPDATES   TABLE T01VAT                               
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: WF0279U                                             
002600*        REQUEST:     WF0279I1                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        RESPONSE:    WF0279O1                                            
003000                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003400 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
004000 DATA DIVISION.                                                           
004200 FILE SECTION.                                                            
004300     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WF027900'.            
004700                                                                          
004800*    --- WORK FIELD FOR ERROR MESSAGE WHEN CALLING ABEND.                 
004900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100                                                                          
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400                                                                          
006100 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006200     88  KEYS-OK                             VALUE 'Y'.                   
006300     88  KEYS-FEL                            VALUE 'N'.                   
006400     EJECT                                                                
006410                                                                          
006500*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007110                                                                          
007200*    --- PARAMETERS TO ABEND                                              
007400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007710 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007810                                                                          
007900 01  MESSAGE-CODES.                                                       
008000     03  ERROR-CODES.                                                     
008001       05 NOTHING-HAS-BEEN-UPDATED   PIC X(3)    VALUE '004'.             
008002       05 NOTHING-HAS-BEEN-INSERTED  PIC X(3)    VALUE '005'.             
008003       05 NOTHING-HAS-BEEN-DELETED   PIC X(3)    VALUE '006'.             
008004       05 UPDATE-NOT-ALLOWED         PIC X(3)    VALUE '007'.             
008005       05 INSERT-NOT-ALLOWED         PIC X(3)    VALUE '008'.             
008006       05 DELETE-NOT-ALLOWED         PIC X(3)    VALUE '009'.             
008010       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
008011       05 IS-INVALID                 PIC X(3)    VALUE '023'.             
008012       05 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.             
008013       05 NOT-FOUND                  PIC X(3)    VALUE '025'.             
008020       05 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.             
008060       05 LINE-NOT-FOUND             PIC X(3)    VALUE '027'.             
008061       05 ALREADY-EXISTS             PIC X(3)    VALUE '030'.             
008070       05 SYSTEM-ERROR               PIC X(3)    VALUE '099'.             
008093     03  INFO-KODER.                                                      
008094       05 UPDATE-DONE                PIC X(3)    VALUE '001'.             
008095       05 INSERT-DONE                PIC X(3)    VALUE '002'.             
008096       05 DELETE-DONE                PIC X(3)    VALUE '003'.             
008097     EJECT                                                                
008098*01  -COPY WDECAREA                                                       
008099 01  FILLER                      PIC X(16)   VALUE 'DECEDIT    '.         
008100     EJECT                                                                
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008900                                                                          
009000 01  -COPY WZ01SUB                                                        
009100     EJECT                                                                
009110                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009300                                                                          
009400 01  REQU-AREA.                                                           
009500*    03  -COPY WZ01REQU                                                   
009600*    03  -COPY WF0279I1                                                   
009700     EJECT                                                                
009710                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009900                                                                          
010000 01  RESP-AREA.                                                           
010100*    03  -COPY WZ01RESP                                                   
010200*    03  -COPY WF0279O1                                                   
010300     EJECT                                                                
010411                                                                          
010412 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010413       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010414                                                                          
010415 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
010416 01  DB2-WS.                                                              
010417     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
010418         88  CURSOR-OK                       VALUE 000.                   
010419         88  LINES-FOUND                     VALUE 000.                   
010420         88  LINES-MISSING                   VALUE 100.                   
010421         88  RESOURCE-WRONG                  VALUE 904.                   
010422                                                                          
010423     03  GOOD-SQLCODECODES.                                               
010424         05  GOOD-SQLCODE OCCURS 5                                        
010430             INDEXED BY SQLCODE-IX PIC 9(3).                              
010500     EJECT                                                                
010801                                                                          
010802 01  WS-AREA.                                                             
010821     03 WS-REVAT-RED            PIC Z(2)9.9(2).                           
010822     03 WS-REVAT-NUM            PIC S9(3)V9(2) COMP-3.                    
010823     03 WS-REVAT-DEC            PIC 9(3)V9(2).                            
010824     03 WS-REVAT-HELTAL         PIC 9(3).                                 
010825     03 WS-ACTIVE               PIC X(8)     VALUE '00000000'.            
010826     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
010827     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
010828     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
010829     03 WS-DATUM                PIC X(8)     VALUE SPACE.                 
010830     EJECT                                                                
010837                                                                          
010838 01  FILLER                  PIC X(16)    VALUE 'T01LSEL-AREA'.           
010839*01  -COPY T01LSEL -PRE T01LSEL-                                          
010840     EJECT                                                                
010841                                                                          
010842 01  FILLER                  PIC X(16)    VALUE 'T01VAT-AREA'.            
010850*01  -COPY T01VAT  -PRE T01VAT-                                           
010901     EJECT                                                                
010902                                                                          
010903     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
010904     EJECT                                                                
010905                                                                          
010910     EXEC SQL INCLUDE T01VAT  END-EXEC.                                   
011000     EJECT                                                                
011010                                                                          
011100 LINKAGE SECTION.                                                         
011501 PROCEDURE DIVISION.                                                      
011502 MAIN SECTION.                                                            
011800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
011900     IF SUB-KDRC = ZERO                                                   
012000       PERFORM A-INIT                                                     
012100       PERFORM B-CHECK-KEYS                                               
012110       IF KEYS-OK                                                         
012200         PERFORM C-CHECK-INDATA                                           
012300       END-IF                                                             
012600       IF KEYS-OK                                                         
012700         PERFORM D-PERFORM-REQUEST                                        
012701         IF KEYS-OK                                                       
012710           PERFORM E-BUILD-DATA-RESPONSE                                  
012810         END-IF                                                           
013000       END-IF                                                             
013010       PERFORM F-BUILD-HEADER-RESPONSE                                    
013100       PERFORM S02-RETURN-RESPONSE                                        
013200     END-IF                                                               
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013710                                                                          
013800 A-INIT SECTION.                                                          
014304     MOVE YES                         TO KEYS-SW                          
014305                                                                          
014306     MOVE ALL ' '                     TO RESP-AREA                        
014311     MOVE SPACE TO RESP-IDMSG-ERROR                                       
014312     MOVE SPACE TO RESP-IDMSG-INFO                                        
014313     MOVE SPACE TO RESP-IDELMT-ERROR                                      
014314                                                                          
014315     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
014316                                                                          
014320     INITIALIZE GOOD-SQLCODECODES                                         
014600     .                                                                    
014700     EJECT                                                                
014710                                                                          
014800 B-CHECK-KEYS SECTION.                                                    
015000     IF REQU-KDPGMACT = 'S'                                               
015010     AND REQU-IDMSGVER NUMERIC                                            
015100       CONTINUE                                                           
015110     ELSE                                                                 
015120       IF REQU-KDPGMACT = 'I'                                             
015121       AND REQU-IDMSGVER NUMERIC                                          
015130         CONTINUE                                                         
015140       ELSE                                                               
015150         IF REQU-KDPGMACT = 'U'                                           
015151         AND REQU-IDMSGVER NUMERIC                                        
015160           CONTINUE                                                       
015170         ELSE                                                             
015180           IF REQU-KDPGMACT = 'D'                                         
015181           AND REQU-IDMSGVER NUMERIC                                      
015190             CONTINUE                                                     
015200           ELSE                                                           
015300             MOVE NOO TO KEYS-SW                                          
015400             MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                         
015410           END-IF                                                         
015420         END-IF                                                           
015430       END-IF                                                             
015500     END-IF                                                               
015600                                                                          
015700     IF REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                            
015701       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015730       MOVE NOO TO KEYS-SW                                                
015740     END-IF                                                               
015750                                                                          
015751     IF REQU-IDLANDX2-KEY = SPACE OR = ALL '+'                            
015752       MOVE NOO TO KEYS-SW                                                
015753       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015754     END-IF                                                               
015755                                                                          
015756     IF REQU-KDVAT-KEY = SPACE OR = ALL '+'                               
015757       MOVE NOO TO KEYS-SW                                                
015758       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015759     END-IF                                                               
015760                                                                          
015761     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
015762       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015790       MOVE NOO TO KEYS-SW                                                
015791     END-IF                                                               
015792                                                                          
015793     IF KEYS-FEL                                                          
015794       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
015795       IF REQU-KDPGMACT = 'S' OR = 'U' OR = 'I' OR = 'D'                  
015796         CONTINUE                                                         
015797       ELSE                                                               
015798         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015799         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
015800       END-IF                                                             
015801       IF REQU-IDMSGVER NUMERIC                                           
015802         CONTINUE                                                         
015803       ELSE                                                               
015804         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015805         MOVE 'IDMSGVER'   TO WS-IDELMT-ERROR                             
015806       END-IF                                                             
015807       IF REQU-IDUSER = SPACE OR = ALL '+'                                
015808         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
015809         MOVE 'IDUSER'     TO WS-IDELMT-ERROR                             
015810       ELSE                                                               
015811         CONTINUE                                                         
015812       END-IF                                                             
015813     END-IF                                                               
015820     .                                                                    
015900     EJECT                                                                
016000                                                                          
016100 C-CHECK-INDATA SECTION.                                                  
016203     PERFORM DB2-SELECT-T01LSEL                                           
016205     IF LINES-FOUND                                                       
016206       CONTINUE                                                           
016207       IF T01LSEL-FLVATUPD = 'N'                                          
016208         MOVE NOO          TO KEYS-SW                                     
016209         MOVE IS-INVALID   TO WS-IDMSG-ERROR                              
016210         MOVE 'IDLEGSEL'   TO WS-IDELMT-ERROR                             
016211       END-IF                                                             
016213     ELSE                                                                 
016214       MOVE NOO            TO KEYS-SW                                     
016215       MOVE NOT-FOUND      TO WS-IDMSG-ERROR                              
016216       MOVE 'IDLEGSEL'     TO WS-IDELMT-ERROR                             
016217     END-IF                                                               
016218                                                                          
016219     IF KEYS-OK                                                           
016220       IF REQU-KDPGMACT = 'U' OR 'I'                                      
016221         IF REQU-BEVAT = SPACE OR ALL '+'                                 
016222           MOVE MUST-BE-ENTERED       TO WS-IDMSG-ERROR                   
016223           MOVE 'BEVAT'               TO WS-IDELMT-ERROR                  
016224         END-IF                                                           
016226         IF KEYS-OK                                                       
016227           IF REQU-REVAT = SPACE OR ALL '+'                               
016228             MOVE MUST-BE-ENTERED     TO WS-IDMSG-ERROR                   
016229             MOVE 'REVAT'             TO WS-IDELMT-ERROR                  
016230           ELSE                                                           
016231             MOVE REQU-REVAT          TO DEC-IDFRIDATA                    
016232             MOVE  3                  TO DEC-KVHELTAL                     
016233             MOVE  2                  TO DEC-KVDECIMAL                    
016234             CALL WDECEDIT               USING DEC-WDECAREA               
016235             IF DEC-KDSVAR-OK                                             
016236               MOVE DEC-IDEDITDATA    TO WS-REVAT-DEC                     
016237               IF WS-REVAT-DEC NOT NUMERIC                                
016238                 MOVE MUST-BE-NUMERIC TO WS-IDMSG-ERROR                   
016239                 MOVE 'REVAT'         TO RESP-IDELMT-ERROR                
016240                 MOVE NOO             TO KEYS-SW                          
016243               ELSE                                                       
016244                 MOVE WS-REVAT-DEC    TO WS-REVAT-HELTAL                  
016245                 IF WS-REVAT-HELTAL > 100                                 
016248                   MOVE IS-INVALID    TO WS-IDMSG-ERROR                   
016249                   MOVE 'REVAT'       TO RESP-IDELMT-ERROR                
016250                   MOVE NOO           TO KEYS-SW                          
016251                 ELSE                                                     
016252                   MOVE WS-REVAT-DEC  TO WS-REVAT-NUM                     
016253                 END-IF                                                   
016254               END-IF                                                     
016255             ELSE                                                         
016256               MOVE IS-INVALID        TO WS-IDMSG-ERROR                   
016257               MOVE 'REVAT'           TO RESP-IDELMT-ERROR                
016258               MOVE NOO               TO KEYS-SW                          
016259             END-IF                                                       
016260           END-IF                                                         
016270         END-IF                                                           
017030       END-IF                                                             
017147     END-IF                                                               
017150     .                                                                    
017200     EJECT                                                                
017210                                                                          
017300 D-PERFORM-REQUEST SECTION.                                               
017310     IF REQU-KDPGMACT = 'S'                                               
017320       PERFORM DB2-SELECT-T01VAT                                          
017330       IF LINES-FOUND                                                     
017340         CONTINUE                                                         
017350       ELSE                                                               
017352         MOVE NOT-FOUND      TO WS-IDMSG-ERROR                            
017353         MOVE 'VAT CODE'     TO WS-IDELMT-ERROR                           
017355         MOVE NOO TO KEYS-SW                                              
017356       END-IF                                                             
017360     END-IF                                                               
017361                                                                          
017362     IF REQU-KDPGMACT = 'U'                                               
017363       PERFORM DB2-SELECT-T01VAT                                          
017364       IF LINES-FOUND AND                                                 
017365         T01VAT-DADELDAT = WS-ACTIVE                                      
017366         PERFORM DB2-UPDATE-T01VAT                                        
017367         PERFORM DB2-SELECT-T01VAT                                        
017368         MOVE UPDATE-DONE          TO WS-IDMSG-INFO                       
017369       ELSE                                                               
017370         IF LINES-FOUND AND                                               
017371            T01VAT-DADELDAT NOT = WS-ACTIVE                               
017372           MOVE UPDATE-NOT-ALLOWED TO WS-IDMSG-ERROR                      
017373           MOVE 'VAT CODE'         TO WS-IDELMT-ERROR                     
017374           MOVE NOO                TO KEYS-SW                             
017375         ELSE                                                             
017376           IF LINES-MISSING                                               
017377             MOVE NOT-FOUND        TO WS-IDMSG-ERROR                      
017378             MOVE 'VAT CODE'       TO WS-IDELMT-ERROR                     
017379             MOVE NOO              TO KEYS-SW                             
017380           END-IF                                                         
017381         END-IF                                                           
017382       END-IF                                                             
017383     END-IF                                                               
017384                                                                          
017385     IF REQU-KDPGMACT = 'I'                                               
017386       PERFORM DB2-SELECT-T01VAT                                          
017387       IF LINES-FOUND AND                                                 
017388          T01VAT-DADELDAT = WS-ACTIVE                                     
017389         MOVE ALREADY-EXISTS     TO WS-IDMSG-ERROR                        
017390         MOVE 'VAT CODE'         TO WS-IDELMT-ERROR                       
017391         MOVE NOO                TO KEYS-SW                               
017392       ELSE                                                               
017393         IF LINES-FOUND AND                                               
017394            T01VAT-DADELDAT NOT = WS-ACTIVE                               
017395           PERFORM DB2-DELETE-T01VAT                                      
017396           PERFORM DB2-INSERT-T01VAT                                      
017397           PERFORM DB2-SELECT-T01VAT                                      
017398           MOVE INSERT-DONE      TO WS-IDMSG-INFO                         
017399         ELSE                                                             
017400           IF LINES-MISSING                                               
017401             PERFORM DB2-INSERT-T01VAT                                    
017402             PERFORM DB2-SELECT-T01VAT                                    
017403             MOVE INSERT-DONE    TO WS-IDMSG-INFO                         
017404           END-IF                                                         
017405         END-IF                                                           
017406       END-IF                                                             
017407     END-IF                                                               
017408                                                                          
017409     IF REQU-KDPGMACT = 'D'                                               
017410       PERFORM DB2-SELECT-T01VAT                                          
017411       IF LINES-FOUND AND                                                 
017412         T01VAT-DADELDAT = WS-ACTIVE                                      
017414         PERFORM DB2-DELETE-T01VAT                                        
017415         PERFORM DB2-SELECT-T01VAT                                        
017416         MOVE DELETE-DONE          TO WS-IDMSG-INFO                       
017417       ELSE                                                               
017418         IF LINES-FOUND AND                                               
017419           T01VAT-DADELDAT NOT = WS-ACTIVE                                
017420           MOVE DELETE-NOT-ALLOWED TO WS-IDMSG-ERROR                      
017421           MOVE 'VAT CODE'         TO WS-IDELMT-ERROR                     
017422           MOVE NOO                TO KEYS-SW                             
017423         ELSE                                                             
017424           IF LINES-MISSING                                               
017425             MOVE NOT-FOUND        TO WS-IDMSG-ERROR                      
017426             MOVE 'VAT CODE'       TO WS-IDELMT-ERROR                     
017427             MOVE NOO              TO KEYS-SW                             
017428           END-IF                                                         
017429         END-IF                                                           
017430       END-IF                                                             
017440     END-IF                                                               
017600     .                                                                    
018200     EJECT                                                                
018300                                                                          
018310 E-BUILD-DATA-RESPONSE SECTION.                                           
018399     MOVE T01VAT-BEVAT            TO RESP-BEVAT                           
018400     MOVE T01VAT-REVAT            TO WS-REVAT-RED                         
018401     MOVE WS-REVAT-RED            TO RESP-REVAT                           
018402     MOVE T01VAT-DAREGDAT         TO RESP-DAREGDAT                        
018403     MOVE T01VAT-DAUPPDAT         TO RESP-DAUPPDAT                        
018405     MOVE T01VAT-DADELDAT         TO RESP-DADELDAT                        
018406     MOVE T01VAT-IDUSER           TO RESP-IDUSER                          
018408     .                                                                    
018409     EJECT                                                                
018410                                                                          
018420 F-BUILD-HEADER-RESPONSE SECTION.                                         
018492     MOVE REQU-IDMSGVER             TO RESP-IDMSGVER                      
018493     MOVE WS-IDMSG-INFO             TO RESP-IDMSG-INFO                    
018494     MOVE WS-IDMSG-ERROR            TO RESP-IDMSG-ERROR                   
018495     MOVE WS-IDELMT-ERROR           TO RESP-IDELMT-ERROR                  
018497     MOVE REQU-IDLEGSEL-KEY         TO RESP-IDLEGSEL-KEY                  
018498     MOVE T01LSEL-BELEGRAD-1        TO RESP-BELEGRAD-1                    
018499     MOVE REQU-IDLANDX2-KEY         TO RESP-IDLANDX2-KEY                  
018500     MOVE REQU-KDVAT-KEY            TO RESP-KDVAT-KEY                     
018510     .                                                                    
018600     EJECT                                                                
019070                                                                          
019110*    --- DISPATCHER-SECTIONS                                              
019200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019400     MOVE 'GETARG'                   TO SUB-KDFUNC                        
019500     MOVE 'CARPARTS.BILLIT.APPROVEDVATMAINTENANCE'                        
019510       TO SUB-ADDISPABS                                                   
019600     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
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
020710                                                                          
020800 S02-RETURN-RESPONSE SECTION.                                             
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
022801                                                                          
022830 DB2-SELECT-T01LSEL SECTION.                                              
022831     MOVE 000100  TO GOOD-SQLCODECODES                                    
022832                                                                          
022833     EXEC SQL                                                             
022834         SELECT  BELEGRAD_1                                               
022835                ,FLVATUPD                                                 
022836                                                                          
022837         INTO    :T01LSEL-BELEGRAD-1                                      
022838                ,:T01LSEL-FLVATUPD                                        
022839                                                                          
022840         FROM    T01LSEL                                                  
022841                                                                          
022842         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
022843         AND     KDSTATUS = 001                                           
022844     END-EXEC                                                             
022845     MOVE SQLCODE TO SQLCODE-WS                                           
022846     PERFORM DB2-STATUS-CHECK                                             
022847     .                                                                    
022848     EJECT                                                                
022849                                                                          
022850 DB2-SELECT-T01VAT  SECTION.                                              
022851     MOVE 000100  TO GOOD-SQLCODECODES                                    
022852                                                                          
022853     EXEC SQL                                                             
022854         SELECT  IDLEGSEL                                                 
022855                ,IDLANDX2                                                 
022856                ,KDVAT                                                    
022857                ,REVAT                                                    
022858                ,BEVAT                                                    
022859                ,DAREGDAT                                                 
022860                ,DAUPPDAT                                                 
022861                ,DADELDAT                                                 
022862                ,IDUSER                                                   
022863                                                                          
022864         INTO   :T01VAT-IDLEGSEL                                          
022865               ,:T01VAT-IDLANDX2                                          
022866               ,:T01VAT-KDVAT                                             
022867               ,:T01VAT-REVAT                                             
022868               ,:T01VAT-BEVAT                                             
022869               ,:T01VAT-DAREGDAT                                          
022870               ,:T01VAT-DAUPPDAT                                          
022871               ,:T01VAT-DADELDAT                                          
022872               ,:T01VAT-IDUSER                                            
022874                                                                          
022875         FROM    T01VAT                                                   
022876                                                                          
022877         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY AND                        
022878                 IDLANDX2 = :REQU-IDLANDX2-KEY AND                        
022879                 KDVAT    = :REQU-KDVAT-KEY                               
022881     END-EXEC                                                             
022882                                                                          
022883     MOVE SQLCODE TO SQLCODE-WS                                           
022884     PERFORM DB2-STATUS-CHECK                                             
022885     .                                                                    
022886     EJECT                                                                
022888                                                                          
023097 DB2-UPDATE-T01VAT  SECTION.                                              
023098     MOVE 000    TO GOOD-SQLCODECODES                                     
023099     EXEC SQL                                                             
023100         UPDATE T01VAT                                                    
023101                                                                          
023102         SET    REVAT    = :WS-REVAT-NUM                                  
023103               ,BEVAT    = :REQU-BEVAT                                    
023104               ,DAUPPDAT = :WS-DATUM                                      
023105               ,IDUSER   = :REQU-IDUSER                                   
023106                                                                          
023107         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY AND                         
023108                IDLANDX2 = :REQU-IDLANDX2-KEY AND                         
023109                KDVAT    = :REQU-KDVAT-KEY                                
023110     END-EXEC                                                             
023111                                                                          
023112     MOVE SQLCODE TO SQLCODE-WS                                           
023113     PERFORM DB2-STATUS-CHECK                                             
023114     .                                                                    
023115     EJECT                                                                
023136                                                                          
023137 DB2-INSERT-T01VAT  SECTION.                                              
023138     MOVE 000   TO GOOD-SQLCODECODES                                      
023139                                                                          
023140     EXEC SQL                                                             
023141         INSERT                                                           
023142                                                                          
023143         INTO     T01VAT                                                  
023144                                                                          
023145                 (IDLEGSEL,                                               
023146                  IDLANDX2,                                               
023147                  KDVAT,                                                  
023148                  REVAT,                                                  
023149                  BEVAT,                                                  
023150                  DAREGDAT,                                               
023151                  DAUPPDAT,                                               
023152                  DADELDAT,                                               
023153                  IDUSER)                                                 
023154                                                                          
023155         VALUES (:REQU-IDLEGSEL-KEY                                       
023156                ,:REQU-IDLANDX2-KEY                                       
023157                ,:REQU-KDVAT-KEY                                          
023158                ,:WS-REVAT-NUM                                            
023159                ,:REQU-BEVAT                                              
023160                ,:WS-DATUM                                                
023161                ,'00000000'                                               
023162                ,'00000000'                                               
023163                ,:REQU-IDUSER)                                            
023164     END-EXEC                                                             
023165                                                                          
023166     MOVE SQLCODE TO SQLCODE-WS                                           
023167     PERFORM DB2-STATUS-CHECK                                             
023168     .                                                                    
023169     EJECT                                                                
023170                                                                          
023180 DB2-DELETE-T01VAT  SECTION.                                              
023190     MOVE 000    TO GOOD-SQLCODECODES                                     
023191                                                                          
023200     EXEC SQL                                                             
023210         DELETE                                                           
023211                                                                          
023212         FROM   T01VAT                                                    
023213                                                                          
023214         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY AND                         
023215                IDLANDX2 = :REQU-IDLANDX2-KEY AND                         
023216                KDVAT    = :REQU-KDVAT-KEY                                
023218     END-EXEC                                                             
023219                                                                          
023220     MOVE SQLCODE TO SQLCODE-WS                                           
023221     PERFORM DB2-STATUS-CHECK                                             
023222     .                                                                    
023223     EJECT                                                                
023224                                                                          
023225 DB2-STATUS-CHECK     SECTION.                                            
023226     SET SQLCODE-IX TO 1                                                  
023227     SEARCH GOOD-SQLCODE                                                  
023228       AT END                                                             
023229          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
023230          DELIMITED BY SIZE INTO ERROR-TEXT                               
023231          CALL ABEND USING RKOD-ABEND-DB2                                 
023232       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
023240     END-SEARCH                                                           
023300     .                                                                    
