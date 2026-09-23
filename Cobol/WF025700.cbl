000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF025700.                                                
000300 AUTHOR.         LUNDH BERNT.                                             
000400 DATE-WRITTEN.   02/03/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*    NAME:                                                                
000620*        CARPARTS.BILLIT.PAYTERMLOCATE                                    
000700*    FUNCTION:                                                            
000800*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
000810*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
000900*                                                                         
001000*        THE PROGRAM READS   TABLE T01LSEL                                
001100*        THE PROGRAM READS   TABLE T01PATE                                
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: WF0257T                                             
001500*        REQUEST:     WZ01REQU                                            
001600*                     WF0257I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    WZ01RESP                                            
002000*                     WF0257O1                                            
002100                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)  VALUE 'WF025700'.             
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003110 77  KDRC-DISPLAY                PIC Z(5).                                
003200                                                                          
003210*    --- CONSTANTS                                                        
003300 77  YES                         PIC X      VALUE 'Y'.                    
003400 77  NOO                         PIC X      VALUE 'N'.                    
003500                                                                          
003900 77  WS-CURRENT                  PIC S9(3)  VALUE +001    COMP-3.         
004100 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004200 77  WS-ADRESS                   PIC X(50)                                
004210                            VALUE 'CARPARTS.BILLIT.PAYTERMLOCATE'.        
004410                                                                          
004600 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004700     88  KEYS-OK                            VALUE 'Y'.                    
004800     88  KEYS-WRONG                         VALUE 'N'.                    
004900                                                                          
005000 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
005200     88  ACT-CODE-VALID                      VALUE 'S'.                   
005210     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
005220                                                                          
005310*    --- WORK FIELDS                                                      
005401 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005500 01  WS-COUNTER-T01PATE          PIC S9(7)  VALUE ZERO    COMP-3.         
005521 01  WS-BEBETVIL                 PIC X(30)  VALUE SPACE.                  
005530 01  WS-WHICH-KEY                PIC 9      VALUE ZERO.                   
005551                                                                          
007710*    --- MAPPING FIELDS                                                   
007711 01  MAP-IDSPRAK-LINE            PIC X(2)   VALUE SPACE.                  
007712 01  MAP-KDBETALV-LINE           PIC X(4)   VALUE SPACE.                  
007713 01  MAP-BEBETVIL-LINE           PIC X(30)  VALUE SPACE.                  
007714 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
007715 01  MAP-DAUPPDAT-LINE           PIC X(8)   VALUE SPACE.                  
007716 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
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
008802         05  ERR-INVALID-FIELD       PIC X(3)   VALUE '023'.              
008803         05  ERR-MUST-BE-NUMERIC     PIC X(3)   VALUE '024'.              
008804         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
008805         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
008810         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
009100         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
009310     EJECT                                                                
009400                                                                          
009410*01  -COPY WZ01SUB                                                        
009411     EJECT                                                                
009500*                                                                         
009600 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
009800 01  REQU-AREA.                                                           
009900*    03 -COPY WZ01REQU                                                    
010000*    03 -COPY WF0257I1                                                    
010010     EJECT                                                                
010100                                                                          
010200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
010400 01  RESP-AREA.                                                           
010500*    03 -COPY WZ01RESP                                                    
010600*    03 -COPY WF0257O1                                                    
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
011800     03  GOOD-SQLCODECODES.                                               
011900         05  GOOD-SQLCODE OCCURS 5                                        
012000             INDEXED BY SQLCODE-IX PIC 9(3).                              
012100                                                                          
012200 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
012300                                                                          
012400*01  -COPY T01LSEL -PRE T01LSEL-                                          
012500     EJECT                                                                
012510 01  FILLER                      PIC X(16)   VALUE 'T01PATE-AREA'.        
012520                                                                          
012600*01  -COPY T01PATE -PRE T01PATE-                                          
012700     EJECT                                                                
012900       EXEC SQL INCLUDE T01LSEL END-EXEC.                                 
013000     EJECT                                                                
013200       EXEC SQL INCLUDE T01PATE END-EXEC.                                 
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013600                                                                          
013700 PROCEDURE DIVISION.                                                      
013800 MAIN SECTION.                                                            
014000                                                                          
014100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014200     IF SUB-KDRC = ZERO                                                   
014300       PERFORM A-INIT                                                     
014400       PERFORM B-CHECK-KEYS                                               
014401       IF KEYS-OK                                                         
014410         PERFORM BA-CHECK-KEY-RELATION                                    
014420       END-IF                                                             
014430       IF KEYS-OK                                                         
014600         PERFORM F-READ-SHOW-INFO                                         
014700       END-IF                                                             
014800       PERFORM S02-RETURN-RESPONSE                                        
014910     END-IF                                                               
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015500                                                                          
015700 A-INIT SECTION.                                                          
015800                                                                          
015900     INITIALIZE GOOD-SQLCODECODES                                         
016001     MOVE ALL '+' TO RESP-AREA                                            
016003     MOVE SPACE TO RESP-IDMSG-ERROR                                       
016004     MOVE SPACE TO RESP-IDMSG-INFO                                        
016005     MOVE SPACE TO RESP-IDELMT-ERROR                                      
016006     MOVE ZERO TO RESP-KVRADER                                            
016007     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
016500     .                                                                    
016800*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
016900 B-CHECK-KEYS SECTION.                                                    
017000                                                                          
017100     MOVE YES TO KEYS-SW                                                  
017151                                                                          
017152     IF REQU-IDMSGVER NUMERIC                                             
017153       IF ACT-CODE-SEARCH                                                 
017154       AND REQU-IDLEGSEL-KEY > SPACE                                      
017155       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
017500         IF (REQU-IDSPRAK-KEY  = SPACE OR = ALL '+')                      
017600         AND (REQU-KDBETALV-KEY = SPACE OR = ALL '+')                     
017700         AND (REQU-FLPREL-KEY   = SPACE OR = ALL '+')                     
017710*          MOVE NOO TO KEYS-SW                                            
018100         CONTINUE                                                         
018100         END-IF                                                           
018200       ELSE                                                               
018300         MOVE NOO TO KEYS-SW                                              
018400       END-IF                                                             
018410     ELSE                                                                 
018420       MOVE NOO TO KEYS-SW                                                
018430     END-IF                                                               
018440                                                                          
018450     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
018460       MOVE NOO TO KEYS-SW                                                
018470     END-IF                                                               
018500                                                                          
018503     IF KEYS-WRONG                                                        
018504       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
018505       IF REQU-IDMSGVER NUMERIC                                           
018506         CONTINUE                                                         
018507       ELSE                                                               
018508         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018509         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
018510       END-IF                                                             
018511       IF ACT-CODE-SEARCH                                                 
018512         CONTINUE                                                         
018513       ELSE                                                               
018514         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018515         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
018516       END-IF                                                             
018517       IF REQU-IDUSER = SPACE OR = ALL '+'                                
018518         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018519         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
018522       END-IF                                                             
018523     END-IF                                                               
018524                                                                          
018530     IF KEYS-OK                                                           
018540       PERFORM DB2-SELECT-T01LSEL-TAB                                     
018550       IF LINES-FOUND                                                     
018560         CONTINUE                                                         
018570       ELSE                                                               
018571         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
018572         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
018580         MOVE NOO TO KEYS-SW                                              
018590       END-IF                                                             
018600     END-IF                                                               
018900     .                                                                    
019010*** - CHECK RELATION BETWEEN REQUSTED KEYS                                
019020 BA-CHECK-KEY-RELATION SECTION.                                           
019030                                                                          
019040     MOVE ZERO TO WS-WHICH-KEY                                            
019042                                                                          
019043     IF REQU-IDSPRAK-KEY > SPACE AND NOT = ALL '+'                        
019044       IF  (REQU-KDBETALV-KEY = SPACE OR = ALL '+')                       
019045       AND (REQU-FLPREL-KEY = SPACE OR = ALL '+')                         
019046         MOVE 1 TO WS-WHICH-KEY                                           
019047       ELSE                                                               
019048         IF  (REQU-KDBETALV-KEY > SPACE AND NOT = ALL '+')                
019049         AND (REQU-FLPREL-KEY = SPACE OR = ALL '+')                       
019050           MOVE 2 TO WS-WHICH-KEY                                         
019054         ELSE                                                             
019056           IF  (REQU-KDBETALV-KEY = SPACE OR = ALL '+')                   
019057           AND (REQU-FLPREL-KEY > SPACE AND NOT = ALL '+')                
019058             MOVE 3 TO WS-WHICH-KEY                                       
019059           ELSE                                                           
019060             IF  (REQU-KDBETALV-KEY > SPACE AND NOT = ALL '+')            
019061             AND (REQU-FLPREL-KEY > SPACE AND NOT = ALL '+')              
019062               MOVE 4 TO WS-WHICH-KEY                                     
019063             END-IF                                                       
019064           END-IF                                                         
019065         END-IF                                                           
019066       END-IF                                                             
019067     ELSE                                                                 
019068       IF (REQU-KDBETALV-KEY > SPACE AND NOT = ALL '+')                   
019069         IF (REQU-FLPREL-KEY = SPACE OR = ALL '+')                        
019070           MOVE 5 TO WS-WHICH-KEY                                         
019072         ELSE                                                             
019073           MOVE 6 TO WS-WHICH-KEY                                         
019074         END-IF                                                           
019075       ELSE                                                               
               IF (REQU-FLPREL-KEY > SPACE AND NOT = ALL '+')                   
019076           MOVE 7 TO WS-WHICH-KEY                                         
               ELSE                                                             
                 MOVE 8 TO WS-WHICH-KEY                                         
019082       END-IF                                                             
019090     END-IF                                                               
019091     .                                                                    
019200*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
019300 F-READ-SHOW-INFO SECTION.                                                
019400                                                                          
019500     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
019600     MOVE REQU-IDSPRAK-KEY   TO RESP-IDSPRAK-KEY                          
019700     MOVE REQU-KDBETALV-KEY  TO RESP-KDBETALV-KEY                         
019800     MOVE REQU-FLPREL-KEY    TO RESP-FLPREL-KEY                           
019810     MOVE REQU-IDMSGVER      TO RESP-IDMSGVER                             
019820     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
019900                                                                          
020000     PERFORM FA-READ-BASICDATA                                            
020100     .                                                                    
020400*** - CHECK WHICH REQUESTED KEY                                           
020500 FA-READ-BASICDATA SECTION.                                               
020600                                                                          
020610     MOVE ZERO TO WS-COUNTER-T01PATE                                      
020620                                                                          
020700     EVALUATE WS-WHICH-KEY                                                
020710        WHEN 1                                                            
020800           PERFORM FAA-IDSPRAK                                            
020804        WHEN 2                                                            
020810           PERFORM FAB-IDSPRAK-KDBETALV                                   
020820        WHEN 3                                                            
020830           PERFORM FAC-IDSPRAK-FLPREL                                     
020840        WHEN 4                                                            
020850           PERFORM FAD-IDSPRAK-KDBETALV-FLPREL                            
020860        WHEN 5                                                            
020870           PERFORM FAE-KDBETALV                                           
020880        WHEN 6                                                            
020890           PERFORM FAF-KDBETALV-FLPREL                                    
020891        WHEN 7                                                            
020892           PERFORM FAG-FLPREL                                             
020891        WHEN 8                                                            
020892           PERFORM FAH-BLNKSRCH                                           
020910     END-EVALUATE                                                         
022500                                                                          
022600     MOVE WS-IX TO RESP-KVRADER                                           
023800     .                                                                    
024010*** - HANDLE KEY IDSPRAK                                                  
024030 FAA-IDSPRAK SECTION.                                                     
024040                                                                          
024075     PERFORM DB2-COUNT-CRS-1                                              
024120                                                                          
024121     IF WS-COUNTER-T01PATE = ZERO                                         
024123        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
024127     ELSE                                                                 
024128        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
024129           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
024130        END-IF                                                            
024131     END-IF                                                               
024132                                                                          
024133     IF RESP-IDMSG-ERROR = SPACE                                          
024134        PERFORM DB2-DCL-OPN-T01PATE-CRS-1                                 
024135        PERFORM DB2-FETCH-T01PATE-CRS-1                                   
024136        MOVE ZERO TO WS-IX                                                
024137                                                                          
024138        PERFORM UNTIL LINES-MISSING                                       
024139           PERFORM S03-MOVE-TO-RESPOND                                    
024140           PERFORM DB2-FETCH-T01PATE-CRS-1                                
024141        END-PERFORM                                                       
024142                                                                          
024143        PERFORM DB2-CLOSE-T01PATE-CRS-1                                   
024144     END-IF                                                               
024145     .                                                                    
024146*** - HANDLE KEY IDSPRAK/KDBETALV                                         
024147 FAB-IDSPRAK-KDBETALV SECTION.                                            
024148                                                                          
024149     PERFORM DB2-COUNT-CRS-2                                              
024150                                                                          
024160     IF WS-COUNTER-T01PATE = ZERO                                         
024170        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
024180     ELSE                                                                 
024190        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
024200           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
024300        END-IF                                                            
024400     END-IF                                                               
024500                                                                          
024600     IF RESP-IDMSG-ERROR = SPACE                                          
024700        PERFORM DB2-DCL-OPN-T01PATE-CRS-2                                 
024800        PERFORM DB2-FETCH-T01PATE-CRS-2                                   
024900        MOVE ZERO TO WS-IX                                                
025000                                                                          
025100        PERFORM UNTIL LINES-MISSING                                       
025200           PERFORM S03-MOVE-TO-RESPOND                                    
025300           PERFORM DB2-FETCH-T01PATE-CRS-2                                
025400        END-PERFORM                                                       
025500                                                                          
025600        PERFORM DB2-CLOSE-T01PATE-CRS-2                                   
025700     END-IF                                                               
025800     .                                                                    
025900*** - HANDLE KEY IDSPRAK/FLPREL                                           
026000 FAC-IDSPRAK-FLPREL SECTION.                                              
026100                                                                          
026200     PERFORM DB2-COUNT-CRS-3                                              
026300                                                                          
026400     IF WS-COUNTER-T01PATE = ZERO                                         
026500        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
026600     ELSE                                                                 
026700        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
026800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
026900        END-IF                                                            
027000     END-IF                                                               
027100                                                                          
027200     IF RESP-IDMSG-ERROR = SPACE                                          
027300        PERFORM DB2-DCL-OPN-T01PATE-CRS-3                                 
027400        PERFORM DB2-FETCH-T01PATE-CRS-3                                   
027500        MOVE ZERO TO WS-IX                                                
027600                                                                          
027700        PERFORM UNTIL LINES-MISSING                                       
027800           PERFORM S03-MOVE-TO-RESPOND                                    
027900           PERFORM DB2-FETCH-T01PATE-CRS-3                                
028000        END-PERFORM                                                       
028100                                                                          
028200        PERFORM DB2-CLOSE-T01PATE-CRS-3                                   
028300     END-IF                                                               
028400     .                                                                    
028500*** - HANDLE KEY IDSPRAK/KDBETALV/FLPREL                                  
028600 FAD-IDSPRAK-KDBETALV-FLPREL SECTION.                                     
028700                                                                          
028800     PERFORM DB2-COUNT-CRS-4                                              
028900                                                                          
029000     IF WS-COUNTER-T01PATE = ZERO                                         
029100        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
029200     ELSE                                                                 
029300        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
029310           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
029320        END-IF                                                            
029330     END-IF                                                               
029340                                                                          
029350     IF RESP-IDMSG-ERROR = SPACE                                          
029360        PERFORM DB2-DCL-OPN-T01PATE-CRS-4                                 
029370        PERFORM DB2-FETCH-T01PATE-CRS-4                                   
029380        MOVE ZERO TO WS-IX                                                
029390                                                                          
029391        PERFORM UNTIL LINES-MISSING                                       
029392           PERFORM S03-MOVE-TO-RESPOND                                    
029393           PERFORM DB2-FETCH-T01PATE-CRS-4                                
029394        END-PERFORM                                                       
029395                                                                          
029396        PERFORM DB2-CLOSE-T01PATE-CRS-4                                   
029397     END-IF                                                               
029398     .                                                                    
029399*** - HANDLE KEY KDBETALV                                                 
029400 FAE-KDBETALV SECTION.                                                    
029401                                                                          
029402     PERFORM DB2-COUNT-CRS-5                                              
029403                                                                          
029404     IF WS-COUNTER-T01PATE = ZERO                                         
029405        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
029406     ELSE                                                                 
029407        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
029408           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
029409        END-IF                                                            
029410     END-IF                                                               
029411                                                                          
029412     IF RESP-IDMSG-ERROR = SPACE                                          
029413        PERFORM DB2-DCL-OPN-T01PATE-CRS-5                                 
029414        PERFORM DB2-FETCH-T01PATE-CRS-5                                   
029415        MOVE ZERO TO WS-IX                                                
029416                                                                          
029417        PERFORM UNTIL LINES-MISSING                                       
029418           PERFORM S03-MOVE-TO-RESPOND                                    
029419           PERFORM DB2-FETCH-T01PATE-CRS-5                                
029420        END-PERFORM                                                       
029421                                                                          
029422        PERFORM DB2-CLOSE-T01PATE-CRS-5                                   
029423     END-IF                                                               
029424     .                                                                    
029425*** - HANDLE KEY KDBETALV/FLPREL                                          
029426 FAF-KDBETALV-FLPREL SECTION.                                             
029427                                                                          
029428     PERFORM DB2-COUNT-CRS-6                                              
029429                                                                          
029430     IF WS-COUNTER-T01PATE = ZERO                                         
029431        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
029432     ELSE                                                                 
029433        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
029434           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
029435        END-IF                                                            
029436     END-IF                                                               
029437                                                                          
029438     IF RESP-IDMSG-ERROR = SPACE                                          
029439        PERFORM DB2-DCL-OPN-T01PATE-CRS-6                                 
029440        PERFORM DB2-FETCH-T01PATE-CRS-6                                   
029441        MOVE ZERO TO WS-IX                                                
029442                                                                          
029443        PERFORM UNTIL LINES-MISSING                                       
029444           PERFORM S03-MOVE-TO-RESPOND                                    
029445           PERFORM DB2-FETCH-T01PATE-CRS-6                                
029446        END-PERFORM                                                       
029447                                                                          
029448        PERFORM DB2-CLOSE-T01PATE-CRS-6                                   
029449     END-IF                                                               
029450     .                                                                    
029451*** - HANDLE KEY FLPREL                                                   
029452 FAG-FLPREL SECTION.                                                      
029453                                                                          
029454     PERFORM DB2-COUNT-CRS-7                                              
029455                                                                          
029456     IF WS-COUNTER-T01PATE = ZERO                                         
029457        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
029458     ELSE                                                                 
029459        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
029460           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
029461        END-IF                                                            
029462     END-IF                                                               
029463                                                                          
029464     IF RESP-IDMSG-ERROR = SPACE                                          
029465        PERFORM DB2-DCL-OPN-T01PATE-CRS-7                                 
029466        PERFORM DB2-FETCH-T01PATE-CRS-7                                   
029467        MOVE ZERO TO WS-IX                                                
029468                                                                          
029469        PERFORM UNTIL LINES-MISSING                                       
029470           PERFORM S03-MOVE-TO-RESPOND                                    
029471           PERFORM DB2-FETCH-T01PATE-CRS-7                                
029472        END-PERFORM                                                       
029473                                                                          
029474        PERFORM DB2-CLOSE-T01PATE-CRS-7                                   
029475     END-IF                                                               
029476     .                                                                    
029451*** - HANDLE WHEN KEYS ARE SPACES                                         
029452 FAH-BLNKSRCH SECTION.                                                    
029453                                                                          
029454     PERFORM DB2-COUNT-CRS-8                                              
029455                                                                          
029456     IF WS-COUNTER-T01PATE = ZERO                                         
029457        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
029458     ELSE                                                                 
029459        IF WS-COUNTER-T01PATE > WS-MAX-LINES                              
029460           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
029461        END-IF                                                            
029462     END-IF                                                               
029463                                                                          
029464     IF RESP-IDMSG-ERROR = SPACE                                          
029465        PERFORM DB2-DCL-OPN-T01PATE-CRS-8                                 
029466        PERFORM DB2-FETCH-T01PATE-CRS-8                                   
029467        MOVE ZERO TO WS-IX                                                
029468                                                                          
029469        PERFORM UNTIL LINES-MISSING                                       
029470           PERFORM S03-MOVE-TO-RESPOND                                    
029471           PERFORM DB2-FETCH-T01PATE-CRS-8                                
029472        END-PERFORM                                                       
029473                                                                          
029474        PERFORM DB2-CLOSE-T01PATE-CRS-8                                   
029475     END-IF                                                               
029476     .                                                                    
029480*   --- DISPATCHER SECTION START                                          
029500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
029600                                                                          
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
030400 S02-RETURN-RESPONSE SECTION.                                             
030500                                                                          
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
031100                                                                          
031101*   --- MOVE TO OUTPUT SECTION START                                      
031130 S03-MOVE-TO-RESPOND SECTION.                                             
031140                                                                          
031160     ADD 1 TO WS-IX                                                       
031161                                                                          
031170     MOVE MAP-IDSPRAK-LINE  TO RESP-IDSPRAK-LINE(WS-IX)                   
031180     MOVE MAP-KDBETALV-LINE TO RESP-KDBETALV-LINE(WS-IX)                  
031190     MOVE MAP-BEBETVIL-LINE TO RESP-BEBETVIL-LINE(WS-IX)                  
031191     MOVE MAP-DAREGDAT-LINE TO RESP-DAREGDAT-LINE(WS-IX)                  
031192     MOVE MAP-DAUPPDAT-LINE TO RESP-DAUPPDAT-LINE(WS-IX)                  
031193     MOVE MAP-IDUSER-LINE   TO RESP-IDUSER-LINE(WS-IX)                    
031206     .                                                                    
031300*   --- DB2 SECTIONS                                                      
031400                                                                          
031500*** - CHECK THAT THE REQUESTED LEGAL SELLER EXIST                         
031600 DB2-SELECT-T01LSEL-TAB SECTION.                                          
031800                                                                          
031900     MOVE 000100 TO GOOD-SQLCODECODES                                     
032000                                                                          
032100     EXEC SQL                                                             
032211           SELECT  BELEGRAD_1                                             
032220                                                                          
032290           INTO   :T01LSEL-BELEGRAD-1                                     
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
038625* * * * * * * * * *   - CURSOR-1 -   * * * * * * * * * * * * * * *        
038800 DB2-COUNT-CRS-1 SECTION.                                                 
038900                                                                          
038910     MOVE 000100  TO GOOD-SQLCODECODES                                    
038920                                                                          
039000     EXEC SQL                                                             
039200          SELECT COUNT(*)                                                 
039300                                                                          
039400          INTO  :WS-COUNTER-T01PATE                                       
039500                                                                          
039600          FROM   T01PATE                                                  
039700                                                                          
039800          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
039900            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
040200     END-EXEC                                                             
040500                                                                          
040510     MOVE SQLCODE TO SQLCODE-WS                                           
040520     PERFORM DB2-STATUS-CHECK                                             
042000     .                                                                    
042560 DB2-DCL-OPN-T01PATE-CRS-1 SECTION.                                       
042600                                                                          
042700     MOVE 000100 TO GOOD-SQLCODECODES                                     
042800                                                                          
042900     EXEC SQL                                                             
043000         DECLARE T01PATE-CRS-1 CURSOR WITH HOLD FOR                       
043100                                                                          
043300          SELECT IDSPRAK                                                  
043400               , KDBETALV                                                 
043500               , BEBETVIL                                                 
043600               , DAREGDAT                                                 
043700               , DAUPPDAT                                                 
043800               , IDUSER                                                   
044100                                                                          
044200          FROM T01PATE                                                    
044300                                                                          
044400          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
044520            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
044600                                                                          
044800          ORDER BY IDSPRAK                                                
044900                 , KDBETALV                                               
045100     END-EXEC                                                             
045200                                                                          
045300     MOVE 000100  TO GOOD-SQLCODECODES                                    
045400                                                                          
045500     EXEC SQL                                                             
045600        OPEN T01PATE-CRS-1                                                
045700     END-EXEC                                                             
045800                                                                          
045810     MOVE SQLCODE TO SQLCODE-WS                                           
045820     PERFORM DB2-STATUS-CHECK                                             
046500     .                                                                    
046620 DB2-FETCH-T01PATE-CRS-1 SECTION.                                         
046630                                                                          
046640     MOVE 000100  TO GOOD-SQLCODECODES                                    
046650                                                                          
046660     EXEC SQL                                                             
046680         FETCH T01PATE-CRS-1                                              
046690                                                                          
046691         INTO :MAP-IDSPRAK-LINE                                           
046692            , :MAP-KDBETALV-LINE                                          
046693            , :MAP-BEBETVIL-LINE                                          
046694            , :MAP-DAREGDAT-LINE                                          
046695            , :MAP-DAUPPDAT-LINE                                          
046696            , :MAP-IDUSER-LINE                                            
046699     END-EXEC                                                             
046700                                                                          
046708     MOVE SQLCODE TO SQLCODE-WS                                           
046709     PERFORM DB2-STATUS-CHECK                                             
046713     .                                                                    
046715 DB2-CLOSE-T01PATE-CRS-1 SECTION.                                         
046716                                                                          
046717     EXEC SQL                                                             
046718        CLOSE T01PATE-CRS-1                                               
046719     END-EXEC                                                             
046720     .                                                                    
046730* * * * * * * * * *   - CURSOR-2 -   * * * * * * * * * * * * * * *        
046740 DB2-COUNT-CRS-2 SECTION.                                                 
046750                                                                          
046760     MOVE 000100  TO GOOD-SQLCODECODES                                    
046770                                                                          
046780     EXEC SQL                                                             
046790          SELECT COUNT(*)                                                 
046800                                                                          
046900          INTO  :WS-COUNTER-T01PATE                                       
047000                                                                          
047100          FROM   T01PATE                                                  
047200                                                                          
047300          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
047400            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
047410            AND  KDBETALV = :REQU-KDBETALV-KEY                            
047500     END-EXEC                                                             
047600                                                                          
047700     MOVE SQLCODE TO SQLCODE-WS                                           
047800     PERFORM DB2-STATUS-CHECK                                             
047900     .                                                                    
048000 DB2-DCL-OPN-T01PATE-CRS-2 SECTION.                                       
048100                                                                          
048200     MOVE 000100 TO GOOD-SQLCODECODES                                     
048300                                                                          
048400     EXEC SQL                                                             
048500         DECLARE T01PATE-CRS-2 CURSOR WITH HOLD FOR                       
048600                                                                          
048700          SELECT IDSPRAK                                                  
048800               , KDBETALV                                                 
048900               , BEBETVIL                                                 
049000               , DAREGDAT                                                 
049100               , DAUPPDAT                                                 
049200               , IDUSER                                                   
049300                                                                          
049400          FROM T01PATE                                                    
049500                                                                          
049600          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
049700            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
049710            AND  KDBETALV = :REQU-KDBETALV-KEY                            
049800                                                                          
049900          ORDER BY IDSPRAK                                                
049910                 , KDBETALV                                               
050000     END-EXEC                                                             
050100                                                                          
050200     MOVE 000100  TO GOOD-SQLCODECODES                                    
050300                                                                          
050400     EXEC SQL                                                             
050500        OPEN T01PATE-CRS-2                                                
050600     END-EXEC                                                             
050700                                                                          
050800     MOVE SQLCODE TO SQLCODE-WS                                           
050900     PERFORM DB2-STATUS-CHECK                                             
051000     .                                                                    
051100 DB2-FETCH-T01PATE-CRS-2 SECTION.                                         
051200                                                                          
051300     MOVE 000100  TO GOOD-SQLCODECODES                                    
051400                                                                          
051500     EXEC SQL                                                             
051600         FETCH T01PATE-CRS-2                                              
051700                                                                          
051800         INTO :MAP-IDSPRAK-LINE                                           
051900            , :MAP-KDBETALV-LINE                                          
052000            , :MAP-BEBETVIL-LINE                                          
052100            , :MAP-DAREGDAT-LINE                                          
052200            , :MAP-DAUPPDAT-LINE                                          
052300            , :MAP-IDUSER-LINE                                            
052400     END-EXEC                                                             
052500                                                                          
052600     MOVE SQLCODE TO SQLCODE-WS                                           
052700     PERFORM DB2-STATUS-CHECK                                             
052800     .                                                                    
052900 DB2-CLOSE-T01PATE-CRS-2 SECTION.                                         
053000                                                                          
053100     EXEC SQL                                                             
053200        CLOSE T01PATE-CRS-2                                               
053300     END-EXEC                                                             
053400     .                                                                    
053500* * * * * * * * * *   - CURSOR-3 -   * * * * * * * * * * * * * * *        
053600 DB2-COUNT-CRS-3 SECTION.                                                 
053700                                                                          
053800     MOVE 000100  TO GOOD-SQLCODECODES                                    
053900                                                                          
054000     EXEC SQL                                                             
054100          SELECT COUNT(*)                                                 
054200                                                                          
054300          INTO  :WS-COUNTER-T01PATE                                       
054400                                                                          
054500          FROM   T01PATE                                                  
054600                                                                          
054700          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
054800            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
054900            AND  BEBETVIL = :WS-BEBETVIL                                  
055000     END-EXEC                                                             
055100                                                                          
055200     MOVE SQLCODE TO SQLCODE-WS                                           
055300     PERFORM DB2-STATUS-CHECK                                             
055400     .                                                                    
055500 DB2-DCL-OPN-T01PATE-CRS-3 SECTION.                                       
055600                                                                          
055700     MOVE 000100 TO GOOD-SQLCODECODES                                     
055800                                                                          
055900     EXEC SQL                                                             
056000         DECLARE T01PATE-CRS-3 CURSOR WITH HOLD FOR                       
056100                                                                          
056200          SELECT IDSPRAK                                                  
056300               , KDBETALV                                                 
056400               , BEBETVIL                                                 
056500               , DAREGDAT                                                 
056600               , DAUPPDAT                                                 
056700               , IDUSER                                                   
056800                                                                          
056900          FROM T01PATE                                                    
057000                                                                          
057100          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
057200            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
057300            AND  BEBETVIL = :WS-BEBETVIL                                  
057400                                                                          
057500          ORDER BY IDSPRAK                                                
057600                 , KDBETALV                                               
057700     END-EXEC                                                             
057800                                                                          
057900     MOVE 000100  TO GOOD-SQLCODECODES                                    
058000                                                                          
058100     EXEC SQL                                                             
058200        OPEN T01PATE-CRS-3                                                
058300     END-EXEC                                                             
058400                                                                          
058500     MOVE SQLCODE TO SQLCODE-WS                                           
058600     PERFORM DB2-STATUS-CHECK                                             
058700     .                                                                    
058800 DB2-FETCH-T01PATE-CRS-3 SECTION.                                         
058810                                                                          
058820     MOVE 000100  TO GOOD-SQLCODECODES                                    
058830                                                                          
058840     EXEC SQL                                                             
058850         FETCH T01PATE-CRS-3                                              
058860                                                                          
058870         INTO :MAP-IDSPRAK-LINE                                           
058880            , :MAP-KDBETALV-LINE                                          
058890            , :MAP-BEBETVIL-LINE                                          
058891            , :MAP-DAREGDAT-LINE                                          
058892            , :MAP-DAUPPDAT-LINE                                          
058893            , :MAP-IDUSER-LINE                                            
058894     END-EXEC                                                             
058895                                                                          
058896     MOVE SQLCODE TO SQLCODE-WS                                           
058897     PERFORM DB2-STATUS-CHECK                                             
058898     .                                                                    
058899 DB2-CLOSE-T01PATE-CRS-3 SECTION.                                         
058900                                                                          
058901     EXEC SQL                                                             
058902        CLOSE T01PATE-CRS-3                                               
058903     END-EXEC                                                             
058904     .                                                                    
058905* * * * * * * * * *   - CURSOR-4 -   * * * * * * * * * * * * * * *        
058906 DB2-COUNT-CRS-4 SECTION.                                                 
058907                                                                          
058908     MOVE 000100  TO GOOD-SQLCODECODES                                    
058909                                                                          
058910     EXEC SQL                                                             
058911          SELECT COUNT(*)                                                 
058912                                                                          
058913          INTO  :WS-COUNTER-T01PATE                                       
058914                                                                          
058915          FROM   T01PATE                                                  
058916                                                                          
058917          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
058918            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
058919            AND  KDBETALV = :REQU-KDBETALV-KEY                            
058920            AND  BEBETVIL = :WS-BEBETVIL                                  
058921     END-EXEC                                                             
058922                                                                          
058923     MOVE SQLCODE TO SQLCODE-WS                                           
058924     PERFORM DB2-STATUS-CHECK                                             
058925     .                                                                    
058926 DB2-DCL-OPN-T01PATE-CRS-4 SECTION.                                       
058927                                                                          
058928     MOVE 000100 TO GOOD-SQLCODECODES                                     
058929                                                                          
058930     EXEC SQL                                                             
058931         DECLARE T01PATE-CRS-4 CURSOR WITH HOLD FOR                       
058932                                                                          
058933          SELECT IDSPRAK                                                  
058934               , KDBETALV                                                 
058935               , BEBETVIL                                                 
058936               , DAREGDAT                                                 
058937               , DAUPPDAT                                                 
058938               , IDUSER                                                   
058939                                                                          
058940          FROM T01PATE                                                    
058941                                                                          
058942          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
058943            AND  IDSPRAK  = :REQU-IDSPRAK-KEY                             
058944            AND  KDBETALV = :REQU-KDBETALV-KEY                            
058945            AND  BEBETVIL = :WS-BEBETVIL                                  
058946                                                                          
058947          ORDER BY IDSPRAK                                                
058948                 , KDBETALV                                               
058949     END-EXEC                                                             
058950                                                                          
058951     MOVE 000100  TO GOOD-SQLCODECODES                                    
058952                                                                          
058953     EXEC SQL                                                             
058954        OPEN T01PATE-CRS-4                                                
058955     END-EXEC                                                             
058956                                                                          
058957     MOVE SQLCODE TO SQLCODE-WS                                           
058958     PERFORM DB2-STATUS-CHECK                                             
058959     .                                                                    
058960 DB2-FETCH-T01PATE-CRS-4 SECTION.                                         
058961                                                                          
058962     MOVE 000100  TO GOOD-SQLCODECODES                                    
058963                                                                          
058964     EXEC SQL                                                             
058965         FETCH T01PATE-CRS-4                                              
058966                                                                          
058967         INTO :MAP-IDSPRAK-LINE                                           
058968            , :MAP-KDBETALV-LINE                                          
058969            , :MAP-BEBETVIL-LINE                                          
058970            , :MAP-DAREGDAT-LINE                                          
058971            , :MAP-DAUPPDAT-LINE                                          
058972            , :MAP-IDUSER-LINE                                            
058973     END-EXEC                                                             
058974                                                                          
058975     MOVE SQLCODE TO SQLCODE-WS                                           
058976     PERFORM DB2-STATUS-CHECK                                             
058977     .                                                                    
058978 DB2-CLOSE-T01PATE-CRS-4 SECTION.                                         
058979                                                                          
058980     EXEC SQL                                                             
058981        CLOSE T01PATE-CRS-4                                               
058982     END-EXEC                                                             
058983     .                                                                    
058984* * * * * * * * * *   - CURSOR-5 -   * * * * * * * * * * * * * * *        
058985 DB2-COUNT-CRS-5 SECTION.                                                 
058986                                                                          
058987     MOVE 000100  TO GOOD-SQLCODECODES                                    
058988                                                                          
058989     EXEC SQL                                                             
058990          SELECT COUNT(*)                                                 
058991                                                                          
058992          INTO  :WS-COUNTER-T01PATE                                       
058993                                                                          
058994          FROM   T01PATE                                                  
058995                                                                          
058996          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
058998            AND  KDBETALV = :REQU-KDBETALV-KEY                            
059000     END-EXEC                                                             
059001                                                                          
059002     MOVE SQLCODE TO SQLCODE-WS                                           
059003     PERFORM DB2-STATUS-CHECK                                             
059004     .                                                                    
059005 DB2-DCL-OPN-T01PATE-CRS-5 SECTION.                                       
059006                                                                          
059007     MOVE 000100 TO GOOD-SQLCODECODES                                     
059008                                                                          
059009     EXEC SQL                                                             
059010         DECLARE T01PATE-CRS-5 CURSOR WITH HOLD FOR                       
059011                                                                          
059012          SELECT IDSPRAK                                                  
059013               , KDBETALV                                                 
059014               , BEBETVIL                                                 
059015               , DAREGDAT                                                 
059016               , DAUPPDAT                                                 
059017               , IDUSER                                                   
059018                                                                          
059019          FROM T01PATE                                                    
059020                                                                          
059021          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059023            AND  KDBETALV = :REQU-KDBETALV-KEY                            
059025                                                                          
059026          ORDER BY IDSPRAK                                                
059027                 , KDBETALV                                               
059028     END-EXEC                                                             
059029                                                                          
059030     MOVE 000100  TO GOOD-SQLCODECODES                                    
059031                                                                          
059032     EXEC SQL                                                             
059033        OPEN T01PATE-CRS-5                                                
059034     END-EXEC                                                             
059035                                                                          
059036     MOVE SQLCODE TO SQLCODE-WS                                           
059037     PERFORM DB2-STATUS-CHECK                                             
059038     .                                                                    
059039 DB2-FETCH-T01PATE-CRS-5 SECTION.                                         
059040                                                                          
059041     MOVE 000100  TO GOOD-SQLCODECODES                                    
059042                                                                          
059043     EXEC SQL                                                             
059044         FETCH T01PATE-CRS-5                                              
059045                                                                          
059046         INTO :MAP-IDSPRAK-LINE                                           
059047            , :MAP-KDBETALV-LINE                                          
059048            , :MAP-BEBETVIL-LINE                                          
059049            , :MAP-DAREGDAT-LINE                                          
059050            , :MAP-DAUPPDAT-LINE                                          
059051            , :MAP-IDUSER-LINE                                            
059052     END-EXEC                                                             
059053                                                                          
059054     MOVE SQLCODE TO SQLCODE-WS                                           
059055     PERFORM DB2-STATUS-CHECK                                             
059056     .                                                                    
059057 DB2-CLOSE-T01PATE-CRS-5 SECTION.                                         
059058                                                                          
059059     EXEC SQL                                                             
059060        CLOSE T01PATE-CRS-5                                               
059061     END-EXEC                                                             
059062     .                                                                    
059063* * * * * * * * * *   - CURSOR-6 -   * * * * * * * * * * * * * * *        
059064 DB2-COUNT-CRS-6 SECTION.                                                 
059065                                                                          
059066     MOVE 000100  TO GOOD-SQLCODECODES                                    
059067                                                                          
059068     EXEC SQL                                                             
059069          SELECT COUNT(*)                                                 
059070                                                                          
059071          INTO  :WS-COUNTER-T01PATE                                       
059072                                                                          
059073          FROM   T01PATE                                                  
059074                                                                          
059075          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059076            AND  KDBETALV = :REQU-KDBETALV-KEY                            
059077            AND  BEBETVIL = :WS-BEBETVIL                                  
059078     END-EXEC                                                             
059079                                                                          
059080     MOVE SQLCODE TO SQLCODE-WS                                           
059081     PERFORM DB2-STATUS-CHECK                                             
059082     .                                                                    
059083 DB2-DCL-OPN-T01PATE-CRS-6 SECTION.                                       
059084                                                                          
059085     MOVE 000100 TO GOOD-SQLCODECODES                                     
059086                                                                          
059087     EXEC SQL                                                             
059088         DECLARE T01PATE-CRS-6 CURSOR WITH HOLD FOR                       
059089                                                                          
059090          SELECT IDSPRAK                                                  
059091               , KDBETALV                                                 
059092               , BEBETVIL                                                 
059093               , DAREGDAT                                                 
059094               , DAUPPDAT                                                 
059095               , IDUSER                                                   
059096                                                                          
059097          FROM T01PATE                                                    
059098                                                                          
059099          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059100            AND  KDBETALV = :REQU-KDBETALV-KEY                            
059101            AND  BEBETVIL = :WS-BEBETVIL                                  
059102                                                                          
059103          ORDER BY IDSPRAK                                                
059104                 , KDBETALV                                               
059105     END-EXEC                                                             
059106                                                                          
059107     MOVE 000100  TO GOOD-SQLCODECODES                                    
059108                                                                          
059109     EXEC SQL                                                             
059110        OPEN T01PATE-CRS-6                                                
059111     END-EXEC                                                             
059112                                                                          
059113     MOVE SQLCODE TO SQLCODE-WS                                           
059114     PERFORM DB2-STATUS-CHECK                                             
059115     .                                                                    
059116 DB2-FETCH-T01PATE-CRS-6 SECTION.                                         
059117                                                                          
059118     MOVE 000100  TO GOOD-SQLCODECODES                                    
059119                                                                          
059120     EXEC SQL                                                             
059121         FETCH T01PATE-CRS-6                                              
059122                                                                          
059123         INTO :MAP-IDSPRAK-LINE                                           
059124            , :MAP-KDBETALV-LINE                                          
059125            , :MAP-BEBETVIL-LINE                                          
059126            , :MAP-DAREGDAT-LINE                                          
059127            , :MAP-DAUPPDAT-LINE                                          
059128            , :MAP-IDUSER-LINE                                            
059129     END-EXEC                                                             
059130                                                                          
059131     MOVE SQLCODE TO SQLCODE-WS                                           
059132     PERFORM DB2-STATUS-CHECK                                             
059133     .                                                                    
059134 DB2-CLOSE-T01PATE-CRS-6 SECTION.                                         
059135                                                                          
059136     EXEC SQL                                                             
059137        CLOSE T01PATE-CRS-6                                               
059138     END-EXEC                                                             
059139     .                                                                    
059140* * * * * * * * * *   - CURSOR-7 -   * * * * * * * * * * * * * * *        
059141 DB2-COUNT-CRS-7 SECTION.                                                 
059142                                                                          
059143     MOVE 000100  TO GOOD-SQLCODECODES                                    
059144                                                                          
059145     EXEC SQL                                                             
059146          SELECT COUNT(*)                                                 
059147                                                                          
059148          INTO  :WS-COUNTER-T01PATE                                       
059149                                                                          
059150          FROM   T01PATE                                                  
059151                                                                          
059152          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059154            AND  BEBETVIL = :WS-BEBETVIL                                  
059155     END-EXEC                                                             
059156                                                                          
059157     MOVE SQLCODE TO SQLCODE-WS                                           
059158     PERFORM DB2-STATUS-CHECK                                             
059159     .                                                                    
059160 DB2-DCL-OPN-T01PATE-CRS-7 SECTION.                                       
059161                                                                          
059162     MOVE 000100 TO GOOD-SQLCODECODES                                     
059163                                                                          
059164     EXEC SQL                                                             
059165         DECLARE T01PATE-CRS-7 CURSOR WITH HOLD FOR                       
059166                                                                          
059167          SELECT IDSPRAK                                                  
059168               , KDBETALV                                                 
059169               , BEBETVIL                                                 
059170               , DAREGDAT                                                 
059171               , DAUPPDAT                                                 
059172               , IDUSER                                                   
059173                                                                          
059174          FROM T01PATE                                                    
059175                                                                          
059176          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059178            AND  BEBETVIL = :WS-BEBETVIL                                  
059179                                                                          
059180          ORDER BY IDSPRAK                                                
059181                 , KDBETALV                                               
059182     END-EXEC                                                             
059183                                                                          
059184     MOVE 000100  TO GOOD-SQLCODECODES                                    
059185                                                                          
059186     EXEC SQL                                                             
059187        OPEN T01PATE-CRS-7                                                
059188     END-EXEC                                                             
059189                                                                          
059190     MOVE SQLCODE TO SQLCODE-WS                                           
059191     PERFORM DB2-STATUS-CHECK                                             
059192     .                                                                    
059193 DB2-FETCH-T01PATE-CRS-7 SECTION.                                         
059194                                                                          
059195     MOVE 000100  TO GOOD-SQLCODECODES                                    
059196                                                                          
059197     EXEC SQL                                                             
059198         FETCH T01PATE-CRS-7                                              
059199                                                                          
059200         INTO :MAP-IDSPRAK-LINE                                           
059201            , :MAP-KDBETALV-LINE                                          
059202            , :MAP-BEBETVIL-LINE                                          
059203            , :MAP-DAREGDAT-LINE                                          
059204            , :MAP-DAUPPDAT-LINE                                          
059205            , :MAP-IDUSER-LINE                                            
059206     END-EXEC                                                             
059207                                                                          
059208     MOVE SQLCODE TO SQLCODE-WS                                           
059209     PERFORM DB2-STATUS-CHECK                                             
059210     .                                                                    
059211 DB2-CLOSE-T01PATE-CRS-7 SECTION.                                         
059212                                                                          
059213     EXEC SQL                                                             
059214        CLOSE T01PATE-CRS-7                                               
059215     END-EXEC                                                             
059216     .                                                                    
059140* * * * * * * * * *   - CURSOR-8 -   * * * * * * * * * * * * * * *        
059141 DB2-COUNT-CRS-8 SECTION.                                                 
059142                                                                          
059143     MOVE 000100  TO GOOD-SQLCODECODES                                    
059144                                                                          
059145     EXEC SQL                                                             
059146          SELECT COUNT(*)                                                 
059147                                                                          
059148          INTO  :WS-COUNTER-T01PATE                                       
059149                                                                          
059150          FROM   T01PATE                                                  
059151                                                                          
059152          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059155     END-EXEC                                                             
059156                                                                          
059157     MOVE SQLCODE TO SQLCODE-WS                                           
059158     PERFORM DB2-STATUS-CHECK                                             
059159     .                                                                    
059160 DB2-DCL-OPN-T01PATE-CRS-8 SECTION.                                       
059161                                                                          
059162     MOVE 000100 TO GOOD-SQLCODECODES                                     
059163                                                                          
059164     EXEC SQL                                                             
059165         DECLARE T01PATE-CRS-8 CURSOR WITH HOLD FOR                       
059166                                                                          
059167          SELECT IDSPRAK                                                  
059168               , KDBETALV                                                 
059169               , BEBETVIL                                                 
059170               , DAREGDAT                                                 
059171               , DAUPPDAT                                                 
059172               , IDUSER                                                   
059173                                                                          
059174          FROM T01PATE                                                    
059175                                                                          
059176          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059179                                                                          
059180          ORDER BY IDSPRAK                                                
059181                 , KDBETALV                                               
059182     END-EXEC                                                             
059183                                                                          
059184     MOVE 000100  TO GOOD-SQLCODECODES                                    
059185                                                                          
059186     EXEC SQL                                                             
059187        OPEN T01PATE-CRS-8                                                
059188     END-EXEC                                                             
059189                                                                          
059190     MOVE SQLCODE TO SQLCODE-WS                                           
059191     PERFORM DB2-STATUS-CHECK                                             
059192     .                                                                    
059193 DB2-FETCH-T01PATE-CRS-8 SECTION.                                         
059194                                                                          
059195     MOVE 000100  TO GOOD-SQLCODECODES                                    
059196                                                                          
059197     EXEC SQL                                                             
059198         FETCH T01PATE-CRS-8                                              
059199                                                                          
059200         INTO :MAP-IDSPRAK-LINE                                           
059201            , :MAP-KDBETALV-LINE                                          
059202            , :MAP-BEBETVIL-LINE                                          
059203            , :MAP-DAREGDAT-LINE                                          
059204            , :MAP-DAUPPDAT-LINE                                          
059205            , :MAP-IDUSER-LINE                                            
059206     END-EXEC                                                             
059207                                                                          
059208     MOVE SQLCODE TO SQLCODE-WS                                           
059209     PERFORM DB2-STATUS-CHECK                                             
059210     .                                                                    
059211 DB2-CLOSE-T01PATE-CRS-8 SECTION.                                         
059212                                                                          
059213     EXEC SQL                                                             
059214        CLOSE T01PATE-CRS-8                                               
059215     END-EXEC                                                             
059216     .                                                                    
059217 DB2-STATUS-CHECK  SECTION.                                               
059218                                                                          
059219     SET SQLCODE-IX TO 1                                                  
059220     SEARCH GOOD-SQLCODE                                                  
059300       AT END                                                             
059310          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
059320          DELIMITED BY SIZE INTO ERROR-TEXT                               
059340          CALL ABEND USING RKOD-ABEND-DB2                                 
059400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
059410          CONTINUE                                                        
059500     END-SEARCH                                                           
059600     .                                                                    
