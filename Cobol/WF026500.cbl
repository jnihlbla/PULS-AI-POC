000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF026500.                                                
000300 AUTHOR.         LUNDH BERNT.                                             
000400 DATE-WRITTEN.   02/03/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610*    NAME:                                                                
000620*        CARPARTS.BILLIT.SENDCOUNTRYLOCATE                                
000700*    FUNCTION:                                                            
000800*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
000810*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
000900*                                                                         
001000*        THE PROGRAM READS   TABLE T01LSEL                                
001100*        THE PROGRAM READS   TABLE T01SECO                                
001110*        THE PROGRAM READS   TABLE T01COCO                                
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: WF0265T                                             
001410*                                                                         
001500*        REQUEST:     WZ01REQU                                            
001600*                     WF0265I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    WZ01RESP                                            
002000*                     WF0265O1                                            
002100                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)  VALUE 'WF026500'.             
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
004010 77  WS-ACTIVE                   PIC X(8)   VALUE '00000000'.             
004100 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004200 77  WS-ADRESS                   PIC X(50)                                
004210                        VALUE 'CARPARTS.BILLIT.SENDCOUNTRYLOCATE'.        
004410                                                                          
004600 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004700     88  KEYS-OK                            VALUE 'Y'.                    
004800     88  KEYS-WRONG                         VALUE 'N'.                    
004900                                                                          
005310*    --- WORK FIELDS                                                      
005401 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005500 01  WS-COUNTER-T01SECO          PIC S9(7)  VALUE ZERO    COMP-3.         
005510 01  WS-IDLANDX3                 PIC  X(3)  VALUE SPACE.                  
005551                                                                          
007710*    --- MAPPING FIELDS                                                   
007711 01  MAP-KDSTATUS-LINE           PIC S9(3)  VALUE ZERO    COMP-3.         
007712 01  MAP-IDLANDX3-LINE           PIC X(3)   VALUE SPACE.                  
007713 01  MAP-BELAND-LINE             PIC X(35)  VALUE SPACE.                  
007714 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
007715 01  MAP-DAUPPDAT-LINE           PIC X(8)   VALUE SPACE.                  
007717 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
007730                                                                          
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
008810         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
009000         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
009100         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
009400                                                                          
009410*01  -COPY WZ01SUB                                                        
009411     EJECT                                                                
009500*                                                                         
009600 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
009800 01  REQU-AREA.                                                           
009900*    03 -COPY WZ01REQU                                                    
010000*    03 -COPY WF0265I1                                                    
010010     EJECT                                                                
010100                                                                          
010200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
010400 01  RESP-AREA.                                                           
010500*    03 -COPY WZ01RESP                                                    
010600*    03 -COPY WF0265O1                                                    
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
012400*01  -COPY T01LSEL -PRE T01LSEL-                                          
012500     EJECT                                                                
012501                                                                          
012510 01  FILLER                      PIC X(16)   VALUE 'T01SECO-AREA'.        
012600*01  -COPY T01SECO -PRE T01SECO-                                          
012700     EJECT                                                                
012710                                                                          
012800 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
012810*01  -COPY T01COCO -PRE T01COCO-                                          
012820     EJECT                                                                
012821                                                                          
012900     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
013000     EJECT                                                                
013200     EXEC SQL INCLUDE T01SECO END-EXEC.                                   
013300     EJECT                                                                
013310     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
013320     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013600                                                                          
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
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015700 A-INIT SECTION.                                                          
015800                                                                          
015900     INITIALIZE GOOD-SQLCODECODES                                         
016001     MOVE ALL '+' TO RESP-AREA                                            
016002     MOVE SPACE TO RESP-IDMSG-ERROR                                       
016003     MOVE SPACE TO RESP-IDMSG-INFO                                        
016004     MOVE SPACE TO RESP-IDELMT-ERROR                                      
016005     MOVE ZERO TO RESP-KVRADER                                            
016500     .                                                                    
016800*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
016900 B-CHECK-KEYS SECTION.                                                    
017000                                                                          
017100     MOVE YES TO KEYS-SW                                                  
017151                                                                          
017152     IF REQU-IDMSGVER NUMERIC                                             
017153       IF REQU-KDPGMACT = WS-SEARCH                                       
017154       AND REQU-IDLEGSEL-KEY > SPACE                                      
017155       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
017156         CONTINUE                                                         
018200       ELSE                                                               
018300         MOVE NOO TO KEYS-SW                                              
018400       END-IF                                                             
018410     ELSE                                                                 
018420       MOVE NOO TO KEYS-SW                                                
018430     END-IF                                                               
018440                                                                          
018450     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
018460       MOVE NOO TO KEYS-SW                                                
018490     END-IF                                                               
018500                                                                          
018503     IF KEYS-WRONG                                                        
018504       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
018505       IF REQU-IDMSGVER NUMERIC                                           
018506         CONTINUE                                                         
018507       ELSE                                                               
018508         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018509         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
018510       END-IF                                                             
018511       IF REQU-KDPGMACT = WS-SEARCH                                       
018512         CONTINUE                                                         
018513       ELSE                                                               
018514         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018515         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
018520       END-IF                                                             
018530       IF REQU-IDUSER = SPACE OR = ALL '+'                                
018531         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
018532         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
018535       END-IF                                                             
018540     END-IF                                                               
018541     IF KEYS-OK                                                           
018550       IF REQU-IDLANDX3-KEY > SPACE                                       
018560       AND REQU-IDLANDX3-KEY NOT = ALL '+'                                
018561         MOVE REQU-IDLANDX3-KEY TO WS-IDLANDX3                            
018570         PERFORM DB2-SELECT-T01COCO-TAB                                   
018580         IF LINES-FOUND                                                   
018590           CONTINUE                                                       
018600         ELSE                                                             
018601           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
018602           MOVE 'IDLANDX3'      TO RESP-IDELMT-ERROR                      
018603           MOVE NOO TO KEYS-SW                                            
018610         END-IF                                                           
018700       END-IF                                                             
018800     END-IF                                                               
018801     IF KEYS-OK                                                           
018810       PERFORM DB2-SELECT-T01LSEL-TAB                                     
018820       IF LINES-FOUND                                                     
018830         CONTINUE                                                         
018840       ELSE                                                               
018841         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
018842         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
018850         MOVE NOO        TO KEYS-SW                                       
018860       END-IF                                                             
018870     END-IF                                                               
018900     .                                                                    
019200*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
019300 F-READ-SHOW-INFO SECTION.                                                
019400                                                                          
019500     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
019600     MOVE REQU-IDLANDX3-KEY  TO RESP-IDLANDX3-KEY                         
019810     MOVE REQU-IDMSGVER      TO RESP-IDMSGVER                             
019820     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
019900                                                                          
020000     PERFORM FA-READ-BASICDATA                                            
020100     .                                                                    
020400*** - CHECK WHICH REQUESTED KEY                                           
020500 FA-READ-BASICDATA SECTION.                                               
020600                                                                          
020610     MOVE ZERO TO WS-COUNTER-T01SECO                                      
020620                                                                          
020700     IF  REQU-IDLANDX3-KEY > SPACE                                        
020710     AND REQU-IDLANDX3-KEY NOT = ALL '+'                                  
020810       PERFORM FAA-SEARCH-IDLANDX3                                        
020910     ELSE                                                                 
020912       PERFORM FAB-SEARCH-ALL-IDLANDX3                                    
020913     END-IF                                                               
020920                                                                          
022600     MOVE WS-IX              TO RESP-KVRADER                              
023800     .                                                                    
024010*** - HANDLE REQUESTED SENDING COUNTRY                                    
024030 FAA-SEARCH-IDLANDX3 SECTION.                                             
024040                                                                          
024041     PERFORM DB2-COUNT-CRS-1                                              
024045                                                                          
024046     IF WS-COUNTER-T01SECO = ZERO                                         
024051       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
024052     ELSE                                                                 
024053       IF WS-COUNTER-T01SECO > WS-MAX-LINES                               
024054         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
024055       END-IF                                                             
024056     END-IF                                                               
024057                                                                          
024058     IF RESP-IDMSG-ERROR = SPACE                                          
024059        PERFORM DB2-DCL-OPN-T01SECO-CRS-1                                 
024060        PERFORM DB2-FETCH-T01SECO-CRS-1                                   
024061        MOVE ZERO TO WS-IX                                                
024062                                                                          
024063        PERFORM UNTIL LINES-MISSING                                       
024064           PERFORM S03-MOVE-TO-RESPOND                                    
024065           PERFORM DB2-FETCH-T01SECO-CRS-1                                
024066        END-PERFORM                                                       
024067                                                                          
024068        PERFORM DB2-CLOSE-T01SECO-CRS-1                                   
024069     END-IF                                                               
024070     .                                                                    
024071*** - HANDLE ALL SENDING COUNTRYS                                         
024072 FAB-SEARCH-ALL-IDLANDX3 SECTION.                                         
024073                                                                          
024075     PERFORM DB2-COUNT-CRS-2                                              
024120                                                                          
024121     IF WS-COUNTER-T01SECO = ZERO                                         
024123        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
024127     ELSE                                                                 
024128        IF WS-COUNTER-T01SECO > WS-MAX-LINES                              
024129           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
024130        END-IF                                                            
024131     END-IF                                                               
024132                                                                          
024133     IF RESP-IDMSG-ERROR = SPACE                                          
024134        PERFORM DB2-DCL-OPN-T01SECO-CRS-2                                 
024135        PERFORM DB2-FETCH-T01SECO-CRS-2                                   
024136        MOVE ZERO TO WS-IX                                                
024137                                                                          
024138        PERFORM UNTIL LINES-MISSING                                       
024139           PERFORM S03-MOVE-TO-RESPOND                                    
024140           PERFORM DB2-FETCH-T01SECO-CRS-2                                
024141        END-PERFORM                                                       
024142                                                                          
024143        PERFORM DB2-CLOSE-T01SECO-CRS-2                                   
024144     END-IF                                                               
024145     .                                                                    
029400*   --- DISPATCHER SECTION START                                          
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
031110*** - MOVE TO RESPOND AND COUNT NUMBER OF HITS WHEN CURRENT LINE.         
031120***   WHEN COMING LINE MODIFY CURRENT LINE.                               
031130 S03-MOVE-TO-RESPOND SECTION.                                             
031140                                                                          
031150     IF MAP-KDSTATUS-LINE = WS-CURRENT                                    
031160       ADD 1 TO WS-IX                                                     
031170       MOVE MAP-IDLANDX3-LINE     TO RESP-IDLANDX3-LINE(WS-IX)            
031171                                     WS-IDLANDX3                          
031172       PERFORM DB2-SELECT-T01COCO-TAB                                     
031180       MOVE MAP-BELAND-LINE       TO RESP-BELAND-LINE(WS-IX)              
031190       MOVE MAP-DAREGDAT-LINE     TO RESP-DAREGDAT-LINE(WS-IX)            
031191       MOVE MAP-DAUPPDAT-LINE     TO RESP-DAUPPDAT-LINE(WS-IX)            
031193       MOVE MAP-IDUSER-LINE       TO RESP-IDUSER-LINE(WS-IX)              
031195       MOVE NOO                   TO RESP-FLCOMING-LINE(WS-IX)            
031199     ELSE                                                                 
031200       IF  MAP-KDSTATUS-LINE = WS-COMING                                  
031201       AND MAP-IDLANDX3-LINE = RESP-IDLANDX3-LINE(WS-IX)                  
031202         MOVE YES TO RESP-FLCOMING-LINE(WS-IX)                            
031203       END-IF                                                             
031204     END-IF                                                               
031206     .                                                                    
031300*   --- DB2 SECTIONS                                                      
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
034000 DB2-SELECT-T01COCO-TAB SECTION.                                          
034100                                                                          
034110     MOVE 000100 TO GOOD-SQLCODECODES                                     
034120                                                                          
034130     EXEC SQL                                                             
034140           SELECT  BELAND                                                 
034150                                                                          
034160           INTO   :MAP-BELAND-LINE                                        
034170                                                                          
034180           FROM    T01COCO                                                
034190                                                                          
034191           WHERE   IDLANDX3 = :WS-IDLANDX3                                
034193     END-EXEC                                                             
034194                                                                          
034195     MOVE SQLCODE TO SQLCODE-WS                                           
034196     PERFORM DB2-STATUS-CHECK                                             
034197     .                                                                    
034200* * * * * * * * * *   - CURSOR-1 -   * * * * * * * * * * * * * * *        
034220 DB2-COUNT-CRS-1 SECTION.                                                 
034230                                                                          
034240     EXEC SQL                                                             
034260           SELECT COUNT(*)                                                
034270                                                                          
034280           INTO  :WS-COUNTER-T01SECO                                      
034290                                                                          
034291           FROM   T01SECO                                                 
034292                                                                          
034296           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
034297                AND IDLANDX3 = :REQU-IDLANDX3-KEY                         
034298                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
034299                AND DADELDAT = : WS-ACTIVE                                
034300     END-EXEC                                                             
034301                                                                          
034302     MOVE 000100  TO GOOD-SQLCODECODES                                    
034303                                                                          
034304     MOVE SQLCODE TO SQLCODE-WS                                           
034305     PERFORM DB2-STATUS-CHECK                                             
034306     .                                                                    
034310 DB2-DCL-OPN-T01SECO-CRS-1 SECTION.                                       
034400                                                                          
034600     MOVE 000100 TO GOOD-SQLCODECODES                                     
034700                                                                          
034800     EXEC SQL                                                             
034900         DECLARE T01SECO-CRS-1 CURSOR WITH HOLD FOR                       
035000                                                                          
035200           SELECT  KDSTATUS                                               
035300                 , IDLANDX3                                               
035500                 , DAREGDAT                                               
035600                 , DAUPPDAT                                               
035800                 , IDUSER                                                 
036000                                                                          
036100           FROM    T01SECO                                                
036200                                                                          
036300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
036400                AND IDLANDX3 = :REQU-IDLANDX3-KEY                         
036510                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
036520                AND DADELDAT = :WS-ACTIVE                                 
036600                                                                          
036800           ORDER BY IDLEGSEL                                              
036900                  , IDLANDX3                                              
037000                  , KDSTATUS                                              
037100     END-EXEC                                                             
037200                                                                          
037300     MOVE 000100  TO GOOD-SQLCODECODES                                    
037400                                                                          
037500     EXEC SQL                                                             
037600        OPEN T01SECO-CRS-1                                                
037700     END-EXEC                                                             
037800                                                                          
037810     MOVE SQLCODE TO SQLCODE-WS                                           
037820     PERFORM DB2-STATUS-CHECK                                             
038500     .                                                                    
038530 DB2-FETCH-T01SECO-CRS-1 SECTION.                                         
038540                                                                          
038550     MOVE 000100  TO GOOD-SQLCODECODES                                    
038560                                                                          
038570     EXEC SQL                                                             
038590         FETCH T01SECO-CRS-1                                              
038591                                                                          
038592         INTO :MAP-KDSTATUS-LINE                                          
038593            , :MAP-IDLANDX3-LINE                                          
038595            , :MAP-DAREGDAT-LINE                                          
038596            , :MAP-DAUPPDAT-LINE                                          
038598            , :MAP-IDUSER-LINE                                            
038602     END-EXEC                                                             
038603                                                                          
038609     MOVE SQLCODE TO SQLCODE-WS                                           
038610     PERFORM DB2-STATUS-CHECK                                             
038614     .                                                                    
038617 DB2-CLOSE-T01SECO-CRS-1 SECTION.                                         
038618                                                                          
038619     EXEC SQL                                                             
038620        CLOSE T01SECO-CRS-1                                               
038621     END-EXEC                                                             
038622     .                                                                    
038625* * * * * * * * * *   - CURSOR-2 -   * * * * * * * * * * * * * * *        
038800 DB2-COUNT-CRS-2 SECTION.                                                 
038900                                                                          
039000     EXEC SQL                                                             
039200          SELECT COUNT(*)                                                 
039300                                                                          
039400          INTO  :WS-COUNTER-T01SECO                                       
039500                                                                          
039600          FROM   T01SECO                                                  
039700                                                                          
039800          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
039900             AND DADELDAT = :WS-ACTIVE                                    
040200     END-EXEC                                                             
040300                                                                          
040400     MOVE 000100  TO GOOD-SQLCODECODES                                    
040500                                                                          
040510     MOVE SQLCODE TO SQLCODE-WS                                           
040520     PERFORM DB2-STATUS-CHECK                                             
042000     .                                                                    
042560 DB2-DCL-OPN-T01SECO-CRS-2 SECTION.                                       
042600                                                                          
042700     MOVE 000100 TO GOOD-SQLCODECODES                                     
042800                                                                          
042900     EXEC SQL                                                             
043000         DECLARE T01SECO-CRS-2 CURSOR WITH HOLD FOR                       
043100                                                                          
043300          SELECT  KDSTATUS                                                
043400                , IDLANDX3                                                
043600                , DAREGDAT                                                
043700                , DAUPPDAT                                                
043900                , IDUSER                                                  
044100                                                                          
044200          FROM    T01SECO                                                 
044300                                                                          
044400          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
044520             AND  DADELDAT = :WS-ACTIVE                                   
044600                                                                          
044800          ORDER BY IDLEGSEL                                               
044900                 , IDLANDX3                                               
045000                 , KDSTATUS                                               
045100     END-EXEC                                                             
045200                                                                          
045300     MOVE 000100  TO GOOD-SQLCODECODES                                    
045400                                                                          
045500     EXEC SQL                                                             
045600        OPEN T01SECO-CRS-2                                                
045700     END-EXEC                                                             
045800                                                                          
045810     MOVE SQLCODE TO SQLCODE-WS                                           
045820     PERFORM DB2-STATUS-CHECK                                             
046500     .                                                                    
046620 DB2-FETCH-T01SECO-CRS-2 SECTION.                                         
046630                                                                          
046640     MOVE 000100  TO GOOD-SQLCODECODES                                    
046650                                                                          
046660     EXEC SQL                                                             
046680         FETCH T01SECO-CRS-2                                              
046690                                                                          
046691         INTO :MAP-KDSTATUS-LINE                                          
046692            , :MAP-IDLANDX3-LINE                                          
046694            , :MAP-DAREGDAT-LINE                                          
046695            , :MAP-DAUPPDAT-LINE                                          
046697            , :MAP-IDUSER-LINE                                            
046699     END-EXEC                                                             
046700                                                                          
046708     MOVE SQLCODE TO SQLCODE-WS                                           
046709     PERFORM DB2-STATUS-CHECK                                             
046713     .                                                                    
046715 DB2-CLOSE-T01SECO-CRS-2 SECTION.                                         
046716                                                                          
046717     EXEC SQL                                                             
046718        CLOSE T01SECO-CRS-2                                               
046719     END-EXEC                                                             
046720     .                                                                    
058900 DB2-STATUS-CHECK  SECTION.                                               
059000                                                                          
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
