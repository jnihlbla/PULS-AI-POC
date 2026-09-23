000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2037300.                                                
000300 AUTHOR.         CHESTER COUCH.                                           
000400 DATE-WRITTEN.   21/10/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM TRIGGERS SOP ROUTINE W271S2 TO LIST AND/OR          
000900*        REMOVE REFILL PROPOSALS FROM DATABASE WDE3 FOR THE               
001000*        GIVEN DC.                                                        
001100*                                                                         
001200*        THE PROGRAM READS     WDE3                                       
001300*        THE PROGRAM READS     WDP3                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W2T373                                              
001700*        MID:         W2I37301                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W2O373N1                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W2037300'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  YES                         PIC X       VALUE 'J'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003700                                                                          
003800                                                                          
003900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004000     88  KEYS-OK                             VALUE 'J'.                   
004100     88  KEYS-WRONG                          VALUE 'N'.                   
004200                                                                          
004300 77  INPUT-SW                    PIC X       VALUE 'J'.                   
004400     88  INPUT-OK                            VALUE 'J'.                   
004500     88  INPUT-WRONG                         VALUE 'N'.                   
004600                                                                          
004610 77  DATA-FOUND-SW               PIC X       VALUE 'J'.                   
004620     88  DATA-FOUND                          VALUE 'J'.                   
004630     88  DATA-MISSING                        VALUE 'N'.                   
004640                                                                          
004700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004800     88  OWN-MID                             VALUE '2373'.                
004900     88  GOOD-MID                            VALUE '2371' '2372'          
005000                                                   '2373' '2374'          
005100                                                   '2375' '2376'          
005200                                                   '2377' '2378'          
005300                                                   '2379'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500     EJECT                                                                
005600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005700 01  GENERAL-SUBPROGRAMS.                                                 
005800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     EJECT                                                                
006300*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006400*01 -COPY WMEDAREA                                                        
006500     SKIP3                                                                
006600 01  MESSAGE-CODES.                                                       
006700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006800     EJECT                                                                
006900 01  ERR-INF-MESSAGES.                                                    
007000     03  INF-JOB-STARTED         PIC X(21)                                
007100             VALUE 'JOB STARTED, MAIL TO '.                               
007110     03  INF-UPD-STARTED         PIC X(24)                                
007120             VALUE 'UPDATE STARTED, MAIL TO '.                            
007200     03  INF-F11-TO-LIST        PIC X(40)                                 
007310             VALUE 'PRESS F11 TO LIST P PROPOSALS'.                       
007400     03  ERR-VALID-F11-OPTIONS   PIC X(40)                                
007500             VALUE 'VALID OPTION IS P   '.                                
007501     03  ERR-VALID-F23-OPTIONS   PIC X(40)                                
007502             VALUE 'VALID OPTION IS O   '.                                
007503     03  ERR-MAILID-NOT-FOUND    PIC X(40)                                
007504             VALUE 'MAIL ID NOT FOUND   '.                                
007510     03  ERR-MAIL-MISSING        PIC X(40)                                
007520             VALUE 'EMAIL NOT UPDATED ON THIS MAIL ID'.                   
007800     03  ERR-SEND-DC-REQUIRED    PIC X(40)                                
007900             VALUE 'SENDING DC REQUIRED'.                                 
007910     03  ERR-NO-PROPOSALS        PIC X(40)                                
007920             VALUE 'NO PROPOSALS FOR DC AND OPTION COMBO'.                
008110     03  ERR-PRESS-F23           PIC X(40)                                
008120             VALUE 'PRESS F23 TO LIST & REMOVE O PROPOSALS'.              
008200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008300*                                                                         
008400*    --- DATA SOM SKICKAS TILL SOP                                        
008500 01  PARM-TESYMBV.                                                        
008600     03  FILLER                  PIC X(3)    VALUE 'DC('.                 
008700     03  PARM-IDDC               PIC X(2).                                
008800     03  FILLER                  PIC X(1)    VALUE ')'.                   
008900     03  FILLER                  PIC X(7)    VALUE 'OPTION('.             
009000     03  PARM-KDREFORS           PIC X(1).                                
009100     03  FILLER                  PIC X(1)    VALUE ')'.                   
009200     03  FILLER                  PIC X(6)    VALUE 'EMAIL('.              
009300     03  PARM-IDMAIL             PIC X(60).                               
009400     03  FILLER                  PIC X(1)    VALUE ')'.                   
009500     EJECT                                                                
009600                                                                          
009700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009800     SKIP3                                                                
009900*01 -COPY WMSGINIT                                                        
010000     EJECT                                                                
010100*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010200*                                                                         
010300 01  SAVE-AREA.                                                           
010400     03  SAVE-IDTRANS           PIC X(4)    VALUE '2373'.                 
010500     EJECT                                                                
010600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010700*                                                                         
010800 01  W-PROG-TO-PROG-SW.                                                   
010900*03 -COPY WMSGSOP                                                         
011000  SKIP3                                                                   
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200     SKIP3                                                                
011300*01  MID -COPY W2I37301                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W2O37301                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300     SKIP3                                                                
012400*01  -COPY WMFSAREA                                                       
012500     EJECT                                                                
012600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  KEYS-FOR-DLI.                                                        
013010     03 W-WDE301KY-MIN-X.                                                 
013020         05  W-IDDC-301-MIN      PIC X(2)  VALUE SPACE.                   
013030         05  W-IDPERSON-BUY-301-MIN                                       
013040                                 PIC S9(3) VALUE ZERO COMP-3.             
013050         05  W-KDREFTYP-301-MIN  PIC X     VALUE SPACE.                   
013060         05  W-IDARTNR-301-MIN   PIC S9(9) VALUE ZERO COMP-3.             
013070         05  W-IDDISTR-301-MIN   PIC S9(5) VALUE ZERO COMP-3.             
013080                                                                          
013090     03 W-WDE301KY-MAX-X.                                                 
013091         05  W-IDDC-301-MAX      PIC X(2)  VALUE HIGH-VALUE.              
013092         05  W-IDPERSON-BUY-301-MAX                                       
013093                                 PIC S9(3) VALUE +999 COMP-3.             
013094         05  W-KDREFTYP-301-MAX  PIC X     VALUE HIGH-VALUE.              
013095         05  W-IDARTNR-301-MAX   PIC S9(9)                                
013096                                         VALUE +999999999 COMP-3.         
013097         05  W-IDDISTR-301-MAX   PIC S9(5) VALUE +99999 COMP-3.           
013300     03  W-KDARBTYP-X.                                                    
013400         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
013410     03  W-IDPERSON-X.                                                    
013420         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
013500     SKIP2                                                                
013600*    --- STATUS CODES FROM IMS                                            
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FOUND                       VALUE '  '.                  
013900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014100     SKIP2                                                                
014200 01  GOOD-STATUSCODES.                                                    
014300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014400     SKIP3                                                                
014500 01  SSA1                        PIC X(64).                               
014600 01  SSA2                        PIC X(64).                               
014700     EJECT                                                                
014800*    --- IMS FUNCTION CODES                                               
014900*01  -COPY W0003                                                          
015000     EJECT                                                                
015100*    ---  DLI INPUT-OUTPUT AREA                                           
015200                                                                          
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE301'.                      
015400 01  DLI-IO-WDE301.                                                       
015500*    03  -COPY WDE301                                                     
015600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
015700 01  DLI-IO-WDP311.                                                       
015800*    03  -COPY WDP311                                                     
015900     EJECT                                                                
016000 LINKAGE SECTION.                                                         
016100*01  -COPY W0009   -PRE MSG-                                              
016200*01  -COPY W0009   -PRE ALT-                                              
016300*01  -COPY W0008  -PRE WDP7-                                              
016400     05  FILLER                  PIC X.                                   
016500                                                                          
016600*01  -COPY W0008  -PRE WDE3-                                              
016700     05  FILLER                  PIC X.                                   
016800                                                                          
016900*01  -COPY W0008  -PRE WDP3-                                              
017000     05  FILLER                  PIC X.                                   
017100     EJECT                                                                
017200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
017210                           WDP7-PCB WDE3-PCB WDP3-PCB.                    
017300 MAIN SECTION.                                                            
017400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
017410                           WDP7-PCB WDE3-PCB WDP3-PCB.                    
017500                                                                          
017600     PERFORM IMS-GET-MSG                                                  
017700     IF SEGMENT-FOUND                                                     
017800       PERFORM A-INIT                                                     
017900       PERFORM B-CHECK-KEYS                                               
018000       IF KEYS-OK                                                         
018100         IF OWN-MID                                                       
018200           IF MFS-UPDATE                                                  
018210           OR MFS-UPD-V                                                   
018300             PERFORM C-CHECK-INPUT                                        
018400             IF INPUT-OK                                                  
018500               PERFORM D-START-JOB                                        
018510               IF MFS-UPD-V                                               
018600                 STRING INF-UPD-STARTED                                   
018700                        PARM-IDMAIL                                       
018800                    DELIMITED BY SIZE    INTO MOD-TEMFSINF                
018801               ELSE                                                       
018802                 STRING INF-JOB-STARTED                                   
018803                        PARM-IDMAIL                                       
018804                    DELIMITED BY SIZE    INTO MOD-TEMFSINF                
018805               END-IF                                                     
018810               PERFORM MFS-ERASE-FIELD-OUT                                
019200             END-IF                                                       
019300           ELSE                                                           
019400             PERFORM MFS-DONT-TOUCH-FIELD-OUT                             
019500             PERFORM MFS-READ-IN-AGAIN                                    
019600             MOVE INF-F11-TO-LIST        TO MOD-TEMFSINF                  
019700           END-IF                                                         
019800         ELSE                                                             
019900           PERFORM MFS-ERASE-FIELD-OUT                                    
020000         END-IF                                                           
020100       END-IF                                                             
020200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O37301 + 4                      
020300       PERFORM IMS-INSERT-MSG                                             
020400     END-IF                                                               
020500                                                                          
020600     MOVE ZERO TO RETURN-CODE                                             
020700     GOBACK                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 A-INIT SECTION.                                                          
021100                                                                          
021200     IF MSG-DOUBLE-TRANSACTIONS                                           
021300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I37301                 
021400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021600     ELSE                                                                 
021700       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I37301                  
021800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022000     END-IF                                                               
022100                                                                          
022200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022500                                                                          
022600     MOVE LOW-VALUE TO MSG-AREA                                           
022700     MOVE 'W2O373N1' TO MFS-IDMOD                                         
022800     MOVE '2373' TO MOD-IDTRANS                                           
022900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
023000                                                                          
023100     IF OWN-MID OR HELP-MID                                               
023200       CONTINUE                                                           
023300     ELSE                                                                 
023400       MOVE SPACE TO MFS-KDTRTYP                                          
023500       MOVE '7' TO MFS-IDPFK                                              
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 B-CHECK-KEYS SECTION.                                                    
024000                                                                          
024100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024200     MOVE '001'             TO MSGI-KDCALL                                
024300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024500     MOVE '2373'            TO MSGI-IDTRANS                               
024900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
025000     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
025100                                                                          
025200*    - LANGUAGE TO BE USED BY MEDKONV                                     
025300     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
025400                                                                          
025500     MOVE YES TO KEYS-SW                                                  
025600                                                                          
025700                                                                          
025710     IF OWN-MID                                                           
025800*      -- CHECK OF KDARBTYP                                               
026600       IF MID-KDARBTYP NOT = ALL '+'                                      
026700         MOVE MID-KDARBTYP          TO W-KDARBTYP                         
026710                                      MOD-KDARBTYP                        
026800       ELSE                                                               
026900         MOVE NOO                   TO KEYS-SW                            
026901         MOVE MFS-ERASE-FIELD       TO MOD-KDARBTYP                       
026910         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDARBTYP-ATTR                  
027000       END-IF                                                             
027001                                                                          
027010*      -- CHECK OF IDPERSON                                               
027030       INSPECT MID-IDPERSON REPLACING LEADING SPACE BY ZERO               
027040       IF  MID-IDPERSON NOT = ALL '+'                                     
027041       AND MID-IDPERSON NUMERIC                                           
027042       AND MID-IDPERSON NOT = ZERO                                        
027050         MOVE MID-IDPERSON          TO W-IDPERSON                         
027060                                       MOD-IDPERSON                       
027061         INSPECT MOD-IDPERSON REPLACING LEADING ZERO BY SPACE             
027070       ELSE                                                               
027071         IF MID-IDPERSON  NOT = ALL '+'                                   
027072         AND MID-IDPERSON NOT = ZERO                                      
027073           MOVE MID-IDPERSON        TO MOD-IDPERSON                       
027074         END-IF                                                           
027075         IF MID-IDPERSON  = ALL '+'                                       
027076           MOVE MFS-ERASE-FIELD     TO MOD-IDPERSON                       
027077         END-IF                                                           
027078         MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDPERSON-ATTR                   
027080         MOVE NOO                  TO KEYS-SW                             
027092       END-IF                                                             
027093                                                                          
027094       IF KEYS-WRONG                                                      
027095         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
027096         CALL WMEDKONV USING MED-WMEDAREA                                 
027097         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
027098         MOVE MFS-ERASE-FIELD TO MOD-IDDC                                 
027099                                 MOD-KDREFORS                             
027100       END-IF                                                             
027101     ELSE                                                                 
027102       MOVE MFS-ERASE-FIELD       TO MOD-KDARBTYP                         
027103                                     MOD-IDPERSON                         
027110     END-IF                                                               
027120     .                                                                    
028600     EJECT                                                                
028700 C-CHECK-INPUT SECTION.                                                   
028800**                                                                        
028833     PERFORM IMS-GU-WDP311                                                
028834     IF SEGMENT-FOUND                                                     
028836       IF PERS-IDMAIL > SPACE                                             
028837         MOVE PERS-IDMAIL            TO PARM-IDMAIL                       
028838       ELSE                                                               
028839         MOVE ERR-MAIL-MISSING       TO MOD-TEMFSINF                      
028840         MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDARBTYP-ATTR                 
028841         MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDPERSON-ATTR                 
028842         MOVE NOO                    TO INPUT-SW                          
028843       END-IF                                                             
028850     ELSE                                                                 
028851       MOVE ERR-MAILID-NOT-FOUND   TO MOD-TEMFSINF                        
028852       MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDARBTYP-ATTR                   
028853       MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDPERSON-ATTR                   
028855       MOVE NOO                    TO INPUT-SW                            
028860     END-IF                                                               
028861                                                                          
028862     IF  MID-IDDC NOT = ALL '+'                                           
028863     AND MID-IDDC     > SPACE                                             
028870       MOVE MID-IDDC               TO W-IDDC-301-MIN                      
028871                                      W-IDDC-301-MAX                      
028872                                      PARM-IDDC                           
028880                                      MOD-IDDC                            
028881       IF (MID-KDREFORS = 'P' AND MFS-UPDATE)                             
028882       OR (MID-KDREFORS = 'O' AND MFS-UPD-V)                              
028883         MOVE MID-KDREFORS   TO PARM-KDREFORS                             
028884                                MOD-KDREFORS                              
028885         MOVE NOO            TO DATA-FOUND-SW                             
028886         PERFORM IMS-GN-WDE301                                            
028887         PERFORM UNTIL DATA-FOUND                                         
028888                    OR SEGMENT-MISSING                                    
028889           IF (REF-KDREFTYP = 'A' OR 'B' OR 'C')                          
028890           AND REF-KDREFORS = MID-KDREFORS                                
028891             MOVE YES            TO DATA-FOUND-SW                         
028892           END-IF                                                         
028894           PERFORM IMS-GN-WDE301                                          
028895         END-PERFORM                                                      
028896         IF DATA-MISSING                                                  
028899           IF INPUT-OK                                                    
028900             MOVE ERR-NO-PROPOSALS     TO MOD-TEMFSINF                    
028901           END-IF                                                         
028902           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDDC-ATTR                   
028903                                          MOD-KDREFORS-ATTR               
028904           MOVE NOO                    TO INPUT-SW                        
028905         END-IF                                                           
028906       ELSE                                                               
028907         IF INPUT-OK                                                      
028908           EVALUATE TRUE                                                  
028909           WHEN MID-KDREFORS = 'O' AND MFS-UPDATE                         
028910             MOVE ERR-PRESS-F23         TO MOD-TEMFSINF                   
028911           WHEN MFS-UPD-V                                                 
028912             MOVE ERR-VALID-F23-OPTIONS TO MOD-TEMFSINF                   
028913           WHEN OTHER                                                     
028914             MOVE ERR-VALID-F11-OPTIONS TO MOD-TEMFSINF                   
028915           END-EVALUATE                                                   
028916         END-IF                                                           
028917         MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDREFORS-ATTR                 
028918         IF  MID-KDREFORS NOT = ALL '+'                                   
028919           MOVE MID-KDREFORS           TO MOD-KDREFORS                    
028920         END-IF                                                           
028921         MOVE NOO                    TO INPUT-SW                          
028922       END-IF                                                             
028923     ELSE                                                                 
028924       IF INPUT-OK                                                        
028925         MOVE ERR-SEND-DC-REQUIRED TO MOD-TEMFSINF                        
028926       END-IF                                                             
028927       MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-IDDC-ATTR                       
028928       IF  MID-KDREFORS NOT = ALL '+'                                     
028929         MOVE MID-KDREFORS           TO MOD-KDREFORS                      
028930       END-IF                                                             
028931       MOVE NOO                    TO INPUT-SW                            
028932     END-IF                                                               
028940     .                                                                    
029000     EJECT                                                                
029100 D-START-JOB SECTION.                                                     
029200     MOVE '2373'       TO MSGSOP-IDTRANS                                  
029300     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
029400     MOVE 'W271S2    ' TO MSGSOP-IDPROCESS                                
029500     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
029600     MOVE PARM-TESYMBV TO MSGSOP-TESYMBV                                  
029700     PERFORM IMS-INSERT-ALT-MSG                                           
029800     .                                                                    
029900     EJECT                                                                
030000 MFS-ERASE-FIELD-OUT SECTION.                                             
030100                                                                          
030200*    --- ALLA UTDATA-FÄLT                                                 
030300     MOVE MFS-ERASE-FIELD TO MOD-KDARBTYP                                 
030310                             MOD-IDPERSON                                 
030400                             MOD-IDDC                                     
030500                             MOD-KDREFORS                                 
030600     .                                                                    
030700     SKIP3                                                                
031500 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
031600                                                                          
031700*    --- ALLA UTDATA-FÄLT                                                 
031800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDARBTYP                          
031810                                    MOD-IDPERSON                          
031900                                    MOD-IDDC                              
032000                                    MOD-KDREFORS                          
032100     .                                                                    
032200     SKIP3                                                                
033700 MFS-READ-IN-AGAIN SECTION.                                               
033800                                                                          
033900*    --- ALL INDATA-FIELDS                                                
034000     MOVE MFS-ADD-READ-FIELD TO MOD-KDARBTYP-ATTR                         
034010                                MOD-IDPERSON-ATTR                         
034100                                MOD-IDDC-ATTR                             
034200                                MOD-KDREFORS-ATTR                         
034300     .                                                                    
034400     EJECT                                                                
034500* --- IMS SECTIONS ---                                                    
034600     SKIP3                                                                
034700 IMS-GET-MSG SECTION.                                                     
034800                                                                          
034900     MOVE '  QC' TO GOOD-STATUSCODES                                      
035000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
035100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035200     PERFORM IMS-STATUSCHECK                                              
035300     .                                                                    
035400     SKIP3                                                                
035500 IMS-INSERT-MSG SECTION.                                                  
035600                                                                          
036000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
036100     MOVE SPACE TO GOOD-STATUSCODES                                       
036200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
036300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036400     PERFORM IMS-STATUSCHECK                                              
036500     .                                                                    
036600     EJECT                                                                
036700 IMS-INSERT-ALT-MSG SECTION.                                              
036800                                                                          
036900     MOVE SPACE TO GOOD-STATUSCODES                                       
037000     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
037100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
037200     PERFORM IMS-STATUSCHECK                                              
037300     .                                                                    
037400     EJECT                                                                
037500 IMS-GN-WDE301 SECTION.                                                   
037600                                                                          
037610     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
037620                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
037800          DELIMITED BY SIZE INTO SSA1                                     
037900     MOVE '  GE' TO GOOD-STATUSCODES                                      
038000     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-WDE301 SSA1                    
038100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSCHECK                                              
038300     .                                                                    
038400     EJECT                                                                
038500 IMS-GU-WDP311 SECTION.                                                   
038600                                                                          
038700     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
038800          DELIMITED BY SIZE INTO SSA1                                     
038810     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
038820          DELIMITED BY SIZE INTO SSA2                                     
038900     MOVE '  GE' TO GOOD-STATUSCODES                                      
039000     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
039100     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSCHECK                                              
039300     .                                                                    
039400     EJECT                                                                
039500 IMS-STATUSCHECK SECTION.                                                 
039600                                                                          
039700     SET STATUS-IX TO 1                                                   
039800     SEARCH GOOD-STATUS                                                   
039900       AT END                                                             
040000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
040100         DELIMITED BY SIZE INTO ERROR-TEXT                                
040200         CALL FELLOG                                                      
040300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
040400         CONTINUE                                                         
040500     END-SEARCH                                                           
040600     .                                                                    
