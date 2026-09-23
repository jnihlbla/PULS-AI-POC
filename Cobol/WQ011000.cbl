000100 PROCESS DYNAM                                                            
000200*COMPOPT DYNBIND=YES                                                      
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     WQ011000.                                                
000500 AUTHOR.         HENRIKSSON ANDERS.                                       
000600 DATE-WRITTEN.   2010-11-10.                                              
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    NAME                                                                 
001000*        CARPARTS.TRPUP.KEYVALUES                                         
001100*    FUNCTION:                                                            
001200*        LOCATE KEY VALUES FOR TRANSPORT FOLLOW-UP                        
001300*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001400*                                                                         
001500*        THE PROGRAM READS     TABLE TQ1TRPUP                             
001600*        THE PROGRAM READS     TABLE TQ1TRPPS                             
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: WQ0110T                                             
002000*        REQUEST:     WQ0110I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    WQ0110O1                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'WQ011000'.            
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGE WHEN CALLING ABEND.                
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100                                                                          
004200 77  WS-MAX-LINES                PIC S9(3)   VALUE +15     COMP-3.        
004300 77  WS-MAX-LINES2               PIC S9(3)   VALUE +15     COMP-3.        
004400 77  WS-COUNTER                  PIC S9(9)   VALUE ZERO    COMP-3.        
004500 77  W-ANT                       PIC S9(3)   VALUE ZERO    COMP-3.        
004600                                                                          
004700 01  WS-LEN-1                    PIC S9(9)   BINARY.                      
004800 01  WS-LEN-2                    PIC S9(9)   BINARY.                      
004900                                                                          
005000 01  WS-LEN-A                    PIC S9(9)   BINARY.                      
005100 01  WS-LEN-C                    PIC S9(9)   BINARY.                      
005200 01  WS-A                        PIC X       VALUE X'7D'.                 
005300                                                                          
005400 01  WS-RESULT                   PIC X(15)   VALUE SPACE.                 
005500 01  WS-RESULT-NUM               PIC S9(15)  VALUE ZERO COMP-3.           
005600 01  WS-RESULT-NUM-3             PIC 9(15)   VALUE ZERO.                  
005700                                                                          
005800 01  WLEN                        PIC S9(9)   BINARY.                      
005900 01  WSEGSTART                   PIC S9(9)   BINARY.                      
006000 01  WSEGSTOPP                   PIC S9(9)   BINARY.                      
006100 01  WSEGLEN-X.                                                           
006200     03 WSEGLEN                  PIC S9(5)   BINARY.                      
006300                                                                          
006400 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006500     88  KEYS-OK                             VALUE 'Y'.                   
006600     88  KEYS-NO                             VALUE 'N'.                   
006700                                                                          
006800 77  ITEM-CHECK                 PIC X(16)   VALUE SPACE.                  
006900     88  ITEM-EXIST                       VALUE 'IDDC'                    
007000                                                'IDLEVNR'                 
007100                                                'IDLANDX2'                
007200                                                'IDDISTR'                 
007300                                                'IDKUNDNR'                
007400                                                'KDORDKL'                 
007500                                                'KDFRAKT'                 
007600                                                'IDKOLLI'                 
007700                                                'IDPSN'                   
007800                                                'KDFARLIG_KOLLI'          
007900                                                'FLDIRLEV'                
008000                                                'FLLDCKND'.               
008100     EJECT                                                                
008200 77  ITEM-CHECK-2               PIC X(16)   VALUE SPACE.                  
008300     88 ITEM-ALFA                         VALUE 'IDDC'                    
008400                                                'IDLEVNR'                 
008500                                                'IDLANDX2'                
008600                                                'FLDIRLEV'                
008700                                                'FLLDCKND'                
008800                                                'IDPSN'.                  
008900     88 ITEM-NUM                          VALUE 'KDORDKL'                 
009000                                                'IDKUNDNR'                
009100                                                'IDDISTR'                 
009200                                                'KDFRAKT'                 
009300                                                'IDKOLLI'                 
009400                                               'KDFARLIG_KOLLI'.          
009500     88 ITEM-DATE6                        VALUE 'TILASTN'.                
009600     EJECT                                                                
009700 77  ITEM-CHECK-3               PIC X(16)   VALUE SPACE.                  
009800     88 ITEM-PUP                          VALUE 'IDDC'                    
009900                                                'IDLEVNR'                 
010000                                                'IDLANDX2'                
010100                                                'FLDIRLEV'                
010200                                                'FLLDCKND'                
010300                                                'IDKUNDNR'                
010400                                                'IDDISTR'                 
010500                                                'KDFRAKT'                 
010600                                                'IDKOLLI'                 
010700                                                'KDFARLIG_KOLLI'          
010800                                                'KDORDKL'                 
010900                                                'TILASTN'.                
011000     88 ITEM-PPS                          VALUE 'IDPSN'.                  
011100     EJECT                                                                
011200                                                                          
011300*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
011400 01  GENERAL-SUBPROGRAMS.                                                 
011500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
011700     03  WQ0XVL2S                PIC X(8)    VALUE 'WQ0XVL2S'.            
011800     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
011900     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
012000     SKIP3                                                                
012100                                                                          
012200*    --- PARAMETERS TO ABEND                                              
012300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
012700     SKIP3                                                                
012800                                                                          
012900 01  MESSAGE-CODES.                                                       
013000     03  ERROR-CODES.                                                     
013100       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
013200       05 SYSTEM-ERROR               PIC X(3)    VALUE '099'.             
013300       05 INVALID-ITEM-NAME          PIC X(3)    VALUE '101'.             
013400       05 VALUE-OVERFLOW             PIC X(3)    VALUE '102'.             
013500       05 ERROR-FROM-WQ0XVL2S        PIC X(3)    VALUE '103'.             
013600                                                                          
013700                                                                          
013800*    --- PARAMETERS TO VIMSID                                             
013900 01  VIMSID-PARM.                                                         
014000   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
014100   03  FILLER                    PIC X(4)    VALUE SPACE.                 
014200     EJECT                                                                
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014500     SKIP3                                                                
014600 01  -COPY WZ01SUB                                                        
014700     EJECT                                                                
014800                                                                          
014900 01  -COPY WQ0XVL2S                                                       
015000     EJECT                                                                
015100                                                                          
015200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015300     SKIP3                                                                
015400 01  REQU-AREA.                                                           
015500*    03  -COPY WZ01REQU                                                   
015600*    03  -COPY WQ0110I1                                                   
015700     EJECT                                                                
015800                                                                          
015900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
016000     SKIP3                                                                
016100 01  RESP-AREA.                                                           
016200*    03  -COPY WZ01RESP                                                   
016300*    03  -COPY WQ0110O1                                                   
016400     EJECT                                                                
016500                                                                          
016600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
016700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
016800                                                                          
016900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
017000 01  DB2-WS.                                                              
017100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
017200         88  CURSOR-OK                       VALUE 000.                   
017300         88  LINES-FOUND                     VALUE 000.                   
017400         88  LINES-MISSING                   VALUE 100.                   
017500         88  RESOURCE-WRONG                  VALUE 904.                   
017600                                                                          
017700     03  GOOD-SQLCODECODES.                                               
017800         05  GOOD-SQLCODE OCCURS 5                                        
017900             INDEXED BY SQLCODE-IX PIC 9(3).                              
018000     EJECT                                                                
018100                                                                          
018200 01  WS-IX                      PIC S9(9)    VALUE ZERO BINARY.           
018300 01  WS-IX2                     PIC S9(9)    VALUE ZERO BINARY.           
018400 01  WS-IXU                     PIC S9(9)    VALUE ZERO BINARY.           
018500 01  WS-IXW                     PIC S9(9)    VALUE ZERO BINARY.           
018600 01  WS-IXW2                    PIC S9(9)    VALUE ZERO BINARY.           
018700                                                                          
018800 01  WS-AREA.                                                             
018900     03 WS-KDSTATUS             PIC S9(3)    COMP-3 VALUE ZERO.           
019000     03 WS-KVRADER              PIC Z(4)9(1) VALUE ZERO.                  
019100     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
019200     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
019300     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
019400     EJECT                                                                
019500                                                                          
019600 01  STMTBUFF.                                                            
019700     49 STMTBUFF-L          PIC S9(4)     COMP VALUE ZERO.                
019800     49 STMTBUFF-D          PIC X(15500).                                 
019900     EJECT                                                                
020000                                                                          
020100 01  WS-TECORREL          PIC X(1)     VALUE SPACE.                       
020200                                                                          
020300 01  SQL-QUESTION-FIELDS.                                                 
020400     03 SELECT-FIELDS1.                                                   
020500       05 FILLER         PIC X(16)  VALUE 'SELECT DISTINCT('.             
020600     03 SELECT-FIELDS2.                                                   
020700       05 FILLER         PIC X(18)  VALUE SPACE.                          
020800     03 SELECT-FIELDS3.                                                   
020900       05 FILLER         PIC X(2)   VALUE ') '.                           
021000     03 SELECT-FIELDS4.                                                   
021100       05 FILLER         PIC X(5)   VALUE 'FROM '.                        
021200       05 SELECT-OWNER   PIC X(5)   VALUE SPACE.                          
021300       05 FILLER         PIC X(12)  VALUE '.TQ1TRPUP A '.                 
021400       05 SELECT-TABLE   PIC X(25)  VALUE SPACE.                          
021500     03 SELECT-FIELDS5.                                                   
021600       05 FILLER         PIC X(15100) VALUE SPACE.                        
021700     03 SELECT-FIELDS6.                                                   
021800       05 SELECT-JOIN    PIC X(200) VALUE SPACE.                          
021900                                                                          
022000     03 COUNT-FIELDS1.                                                    
022100       05 FILLER         PIC X(16)  VALUE "SELECT COUNT(*) ".             
022200     03 COUNT-FIELDS2.                                                    
022300       05 FILLER         PIC X(5)   VALUE 'FROM '.                        
022400       05 COUNT-OWNER    PIC X(5)   VALUE SPACE.                          
022500       05 FILLER         PIC X(12)  VALUE '.TQ1TRPUP A '.                 
022600       05 COUNT-TABLE    PIC X(25)  VALUE SPACE.                          
022700     03 COUNT-FIELDS3.                                                    
022800       05 FILLER         PIC X(15100) VALUE SPACE.                        
022900     03 COUNT-FIELDS4.                                                    
023000       05 COUNT-JOIN     PIC X(200) VALUE SPACE.                          
023100                                                                          
023200     03 SELECT-FIELDS-U1.                                                 
023300       05 FILLER         PIC X(15100) VALUE SPACE.                        
023400     03 SELECT-TABLES-U1.                                                 
023500       05 FILLER         PIC X(25) VALUE SPACE.                           
023600                                                                          
023700     03 SELECT-FIELDS-U2.                                                 
023800       05 FILLER         PIC X(15100) VALUE SPACE.                        
023900     03 SELECT-TABLES-U2.                                                 
024000       05 FILLER         PIC X(25) VALUE SPACE.                           
024100                                                                          
024200     03 SELECT-FIELDS-U3.                                                 
024300       05 FILLER         PIC X(15100) VALUE SPACE.                        
024400     03 SELECT-TABLES-U3.                                                 
024500       05 FILLER         PIC X(25) VALUE SPACE.                           
024600                                                                          
024700     03 SELECT-FIELDS-U4.                                                 
024800       05 FILLER         PIC X(15100) VALUE SPACE.                        
024900     03 SELECT-TABLES-U4.                                                 
025000       05 FILLER         PIC X(25) VALUE SPACE.                           
025100                                                                          
025200     03 SELECT-FIELDS-U5.                                                 
025300       05 FILLER         PIC X(15100) VALUE SPACE.                        
025400     03 SELECT-TABLES-U5.                                                 
025500       05 FILLER         PIC X(25) VALUE SPACE.                           
025600                                                                          
025700     03 SELECT-FIELDS-U6.                                                 
025800       05 FILLER         PIC X(15100) VALUE SPACE.                        
025900     03 SELECT-TABLES-U6.                                                 
026000       05 FILLER         PIC X(25) VALUE SPACE.                           
026100                                                                          
026200     03 SELECT-FIELDS-U7.                                                 
026300       05 FILLER         PIC X(15100) VALUE SPACE.                        
026400     03 SELECT-TABLES-U7.                                                 
026500       05 FILLER         PIC X(25) VALUE SPACE.                           
026600                                                                          
026700     03 SELECT-FIELDS-U8.                                                 
026800       05 FILLER         PIC X(15100) VALUE SPACE.                        
026900     03 SELECT-TABLES-U8.                                                 
027000       05 FILLER         PIC X(25) VALUE SPACE.                           
027100                                                                          
027200     03 SELECT-FIELDS-U9.                                                 
027300       05 FILLER         PIC X(15100) VALUE SPACE.                        
027400     03 SELECT-TABLES-U9.                                                 
027500       05 FILLER         PIC X(25) VALUE SPACE.                           
027600                                                                          
027700     03 SELECT-FIELDS-U10.                                                
027800       05 FILLER         PIC X(15100) VALUE SPACE.                        
027900     03 SELECT-TABLES-U10.                                                
028000       05 FILLER         PIC X(25) VALUE SPACE.                           
028100                                                                          
028200     03 SELECT-FIELDS-U11.                                                
028300       05 FILLER         PIC X(15100) VALUE SPACE.                        
028400     03 SELECT-TABLES-U11.                                                
028500       05 FILLER         PIC X(25) VALUE SPACE.                           
028600                                                                          
028700     03 SELECT-FIELDS-U12.                                                
028800       05 FILLER         PIC X(15100) VALUE SPACE.                        
028900     03 SELECT-TABLES-U12.                                                
029000       05 FILLER         PIC X(25) VALUE SPACE.                           
029100                                                                          
029200     03 SELECT-FIELDS-U13.                                                
029300       05 FILLER         PIC X(15100) VALUE SPACE.                        
029400     03 SELECT-TABLES-U13.                                                
029500       05 FILLER         PIC X(25) VALUE SPACE.                           
029600                                                                          
029700     03 SELECT-FIELDS-U14.                                                
029800       05 FILLER         PIC X(15100) VALUE SPACE.                        
029900     03 SELECT-TABLES-U14.                                                
030000       05 FILLER         PIC X(25) VALUE SPACE.                           
030100                                                                          
030200     03 SELECT-FIELDS-U15.                                                
030300       05 FILLER         PIC X(15100) VALUE SPACE.                        
030400     03 SELECT-TABLES-U15.                                                
030500       05 FILLER         PIC X(25) VALUE SPACE.                           
030600                                                                          
030700     03 COUNT-TABLES-U1.                                                  
030800       05 FILLER         PIC X(25)    VALUE SPACE.                        
030900                                                                          
031000                                                                          
031100 01  FILLER                    PIC X(16)    VALUE 'TQ1TRPUP-AREA'.        
031200*01  -COPY TQ1TRPUP -PRE TQ1TRPUP-                                        
031300     EJECT                                                                
031400                                                                          
031500 01  FILLER                    PIC X(16)    VALUE 'TQ1TRPPS-AREA'.        
031600*01  -COPY TQ1TRPPS -PRE TQ1TRPPS-                                        
031700     EJECT                                                                
031800                                                                          
031900     EXEC SQL INCLUDE TQ1TRPUP END-EXEC.                                  
032000     EJECT                                                                
032100     EXEC SQL INCLUDE TQ1TRPPS END-EXEC.                                  
032200     EJECT                                                                
032300                                                                          
032400 LINKAGE SECTION.                                                         
032500 PROCEDURE DIVISION.                                                      
032600 MAIN SECTION.                                                            
032700                                                                          
032800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
032900     IF SUB-KDRC = ZERO                                                   
033000       PERFORM A-INIT                                                     
033100       PERFORM B-CHECK-KEYS                                               
033200       IF REQU-KDPGMACT = 'C' OR 'O'                                      
033300*        -- C[HECK] AND O[PEN] HAVE IDENTICAL FUNCTIONALITY,              
033400*        -- BUT OPEN INVOLVES ONLY ONE SINGLE FIELD                       
033500*        -- WHILE CHECK OFTEN CHECKS SEVERAL FIELDS                       
033600         IF KEYS-OK                                                       
033700           PERFORM C-CHECK-INDATA                                         
033800         END-IF                                                           
033900         IF KEYS-OK                                                       
034000           PERFORM C-CREATE-WHERE-CLAUSE                                  
034100         END-IF                                                           
034200         IF KEYS-OK                                                       
034300           PERFORM D-PERFORM-REQUEST                                      
034400         END-IF                                                           
034500       ELSE                                                               
034600         IF KEYS-OK                                                       
034700           IF REQU-KDPGMACT = 'I'                                         
034800             PERFORM E-SELECT-INIT                                        
034900           END-IF                                                         
035000         END-IF                                                           
035100       END-IF                                                             
035200       PERFORM F-READ-SHOW-INFO                                           
035300       PERFORM S02-RETURN-RESPONSE                                        
035400     END-IF                                                               
035500                                                                          
035600                                                                          
035700     MOVE ZERO TO RETURN-CODE                                             
035800     GOBACK                                                               
035900     .                                                                    
036000     EJECT                                                                
036100                                                                          
036200 A-INIT SECTION.                                                          
036300     MOVE YES                   TO KEYS-SW                                
036400     MOVE SPACE                 TO RESP-AREA                              
036500     MOVE SPACE                 TO RESP-IDMSG-ERROR                       
036600     MOVE SPACE                 TO RESP-IDMSG-INFO                        
036700     MOVE SPACE                 TO RESP-IDELMT-ERROR                      
036800     MOVE SPACE                 TO COUNT-JOIN                             
036900                                                                          
037000     MOVE ZERO                  TO RESP-KVCOUNT                           
037100     MOVE ZERO                  TO WS-COUNTER                             
037200                                                                          
037300     INITIALIZE GOOD-SQLCODECODES                                         
037400                                                                          
037500                                                                          
037600*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
037700     CALL VIMSID USING VIMSID-PARM                                        
037800     IF IMSID4 = 'IMG0' OR 'IMB0'                                         
037900       MOVE ' WDB2' TO SELECT-OWNER COUNT-OWNER                           
038000     ELSE                                                                 
038010       IF IMSID4 = 'IMD1' OR 'IMD0'                                       
038100         MOVE 'WDB2X' TO SELECT-OWNER COUNT-OWNER                         
038110       END-IF                                                             
038120       IF IMSID4 = 'IMY0'                                                 
038130         MOVE 'WDB2I' TO SELECT-OWNER COUNT-OWNER                         
038140       END-IF                                                             
038150       IF IMSID4 = 'IMP1' OR 'IMP0'                                       
038160         MOVE 'WDB2D' TO SELECT-OWNER COUNT-OWNER                         
038170       END-IF                                                             
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600 B-CHECK-KEYS SECTION.                                                    
038700     IF REQU-KDPGMACT = 'C'                                               
038800     OR REQU-KDPGMACT = 'O'                                               
038900     OR REQU-KDPGMACT = 'I'                                               
039000       CONTINUE                                                           
039100     ELSE                                                                 
039200       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
039300       MOVE 'KDPGMACT'    TO WS-IDELMT-ERROR                              
039400       MOVE NOO TO KEYS-SW                                                
039500     END-IF                                                               
039600                                                                          
039700     IF REQU-IDMSGVER NUMERIC                                             
039800       CONTINUE                                                           
039900     ELSE                                                                 
040000       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
040100       MOVE 'IDMSGVER'    TO WS-IDELMT-ERROR                              
040200       MOVE NOO TO KEYS-SW                                                
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600                                                                          
040700 C-CHECK-INDATA SECTION.                                                  
040800**** CHECK VALID TABLE ITEM                                               
040900     MOVE +1 TO WS-IX                                                     
041000     PERFORM UNTIL WS-IX > WS-MAX-LINES                                   
041100     OR REQU-IDELMT(WS-IX) = SPACE                                        
041200     OR KEYS-NO                                                           
041300       MOVE REQU-IDELMT(WS-IX) TO ITEM-CHECK                              
041400       IF NOT ITEM-EXIST                                                  
041500         MOVE INVALID-ITEM-NAME  TO WS-IDMSG-ERROR                        
041600         MOVE REQU-IDELMT(WS-IX) TO WS-IDELMT-ERROR                       
041700         MOVE NOO TO KEYS-SW                                              
041800       END-IF                                                             
041900       ADD +1 TO WS-IX                                                    
042000     END-PERFORM                                                          
042100     .                                                                    
042200     EJECT                                                                
042300                                                                          
042400 C-CREATE-WHERE-CLAUSE SECTION.                                           
042500**** GET WHERE CLAUSE                                                     
042600     MOVE +1 TO WS-IX                                                     
042700     MOVE +1 TO WS-LEN-C                                                  
042800     PERFORM UNTIL WS-IX > WS-MAX-LINES                                   
042900     OR REQU-IDELMT(WS-IX) = SPACE                                        
043000     OR KEYS-NO                                                           
043100       MOVE +1 TO WS-IXU                                                  
043200       MOVE +0 TO WS-IXW                                                  
043300       MOVE +0 TO WS-IXW2                                                 
043400       MOVE +1 TO WS-LEN-1                                                
043500       MOVE +1 TO WS-LEN-2                                                
043600       PERFORM UNTIL WS-IXU > WS-MAX-LINES                                
043700       OR REQU-IDELMT-SEARCH(WS-IXU) = SPACE                              
043800         IF REQU-TEELMTVAL-SEARCH(WS-IXU) > SPACE                         
043900           IF REQU-IDELMT-SEARCH(WS-IXU) NOT = REQU-IDELMT(WS-IX)         
044000           OR WS-IX = 1                                                   
044100             ADD +1 TO WS-IXW2                                            
044200             PERFORM CD-VALUE-LIST-TO-SQL                                 
044300             IF VL2S-KDSVAR = SPACE                                       
044400               IF REQU-IDELMT-SEARCH(WS-IXU)                              
044500               NOT = REQU-IDELMT(WS-IX)                                   
044600*                -- INCLUDE SEARCH VALUES ONLY FOR OTHER FIELDS           
044700                 ADD +1 TO WS-IXW                                         
044800                 PERFORM CA-PERFORM-WHERE-UNIQUE                          
044900               END-IF                                                     
045000               IF WS-IX = +1                                              
045100*                -- CREATE WHERE FOR COUNT ONLY ONCE                      
045200                 PERFORM CB-PERFORM-WHERE-COUNT                           
045300               END-IF                                                     
045400             END-IF                                                       
045500           END-IF                                                         
045600         END-IF                                                           
045700         ADD  +1 TO WS-IXU                                                
045800         MOVE +1 TO WS-LEN-1                                              
045900       END-PERFORM                                                        
046000       MOVE REQU-IDELMT(WS-IX) TO ITEM-CHECK-3                            
046100       IF ITEM-PUP                                                        
046200         CONTINUE                                                         
046300       ELSE                                                               
046400**** CHECK IF TABLES HAS TO BE ADDED FOR REQU-IDELMT                      
046500         PERFORM CC-PERFORM-TABLES                                        
046600       END-IF                                                             
046700       ADD  +1 TO WS-IX                                                   
046800       MOVE +1 TO WS-LEN-2                                                
046900     END-PERFORM                                                          
047000     .                                                                    
047100     EJECT                                                                
047200                                                                          
047300 CA-PERFORM-WHERE-UNIQUE SECTION.                                         
047400     MOVE SPACE TO SELECT-FIELDS5                                         
047500     IF WS-IXW = +1                                                       
047600       MOVE 'WHERE '        TO SELECT-FIELDS5(WS-LEN-1:6)                 
047700       ADD +6               TO WS-LEN-1                                   
047800       MOVE VL2S-WHERE-COND TO SELECT-FIELDS5(WS-LEN-1:1000)              
047900       ADD +1000            TO WS-LEN-1                                   
048000     END-IF                                                               
048100     IF WS-IXW > +1                                                       
048200       MOVE ' AND '         TO SELECT-FIELDS5(WS-LEN-1:5)                 
048300       ADD +5               TO WS-LEN-1                                   
048400       MOVE VL2S-WHERE-COND TO SELECT-FIELDS5(WS-LEN-1:1000)              
048500       ADD +1000            TO WS-LEN-1                                   
048600     END-IF                                                               
048700**** MOVE TO RIGHT WHERE SATS                                             
048800     IF WS-IX = 1                                                         
048900       MOVE SELECT-FIELDS5(1:1000) TO                                     
049000                                 SELECT-FIELDS-U1(WS-LEN-2:1000)          
049100     END-IF                                                               
049200     IF WS-IX = 2                                                         
049300       MOVE SELECT-FIELDS5(1:1000) TO                                     
049400                                 SELECT-FIELDS-U2(WS-LEN-2:1000)          
049500     END-IF                                                               
049600     IF WS-IX = 3                                                         
049700       MOVE SELECT-FIELDS5(1:1000) TO                                     
049800                                 SELECT-FIELDS-U3(WS-LEN-2:1000)          
049900     END-IF                                                               
050000     IF WS-IX = 4                                                         
050100       MOVE SELECT-FIELDS5(1:1000) TO                                     
050200                                 SELECT-FIELDS-U4(WS-LEN-2:1000)          
050300     END-IF                                                               
050400     IF WS-IX = 5                                                         
050500       MOVE SELECT-FIELDS5(1:1000) TO                                     
050600                                 SELECT-FIELDS-U5(WS-LEN-2:1000)          
050700     END-IF                                                               
050800     IF WS-IX = 6                                                         
050900       MOVE SELECT-FIELDS5(1:1000) TO                                     
051000                                 SELECT-FIELDS-U6(WS-LEN-2:1000)          
051100     END-IF                                                               
051200     IF WS-IX = 7                                                         
051300       MOVE SELECT-FIELDS5(1:1000) TO                                     
051400                                 SELECT-FIELDS-U7(WS-LEN-2:1000)          
051500     END-IF                                                               
051600     IF WS-IX = 8                                                         
051700       MOVE SELECT-FIELDS5(1:1000) TO                                     
051800                                 SELECT-FIELDS-U8(WS-LEN-2:1000)          
051900     END-IF                                                               
052000     IF WS-IX = 9                                                         
052100       MOVE SELECT-FIELDS5(1:1000) TO                                     
052200                                 SELECT-FIELDS-U9(WS-LEN-2:1000)          
052300     END-IF                                                               
052400     IF WS-IX = 10                                                        
052500       MOVE SELECT-FIELDS5(1:1000) TO                                     
052600                                SELECT-FIELDS-U10(WS-LEN-2:1000)          
052700     END-IF                                                               
052800     IF WS-IX = 11                                                        
052900       MOVE SELECT-FIELDS5(1:1000) TO                                     
053000                                SELECT-FIELDS-U11(WS-LEN-2:1000)          
053100     END-IF                                                               
053200     IF WS-IX = 12                                                        
053300       MOVE SELECT-FIELDS5(1:1000) TO                                     
053400                                SELECT-FIELDS-U12(WS-LEN-2:1000)          
053500     END-IF                                                               
053600     IF WS-IX = 13                                                        
053700       MOVE SELECT-FIELDS5(1:1000) TO                                     
053800                                SELECT-FIELDS-U13(WS-LEN-2:1000)          
053900     END-IF                                                               
054000     IF WS-IX = 14                                                        
054100       MOVE SELECT-FIELDS5(1:1000) TO                                     
054200                                SELECT-FIELDS-U14(WS-LEN-2:1000)          
054300     END-IF                                                               
054400     IF WS-IX = 15                                                        
054500       MOVE SELECT-FIELDS5(1:1000) TO                                     
054600                                SELECT-FIELDS-U15(WS-LEN-2:1000)          
054700     END-IF                                                               
054800**** CHECK IF TABLES HAS TO BE ADDED FOR REQU-TEELMTVAL-SEARCH            
054900     PERFORM CC-PERFORM-TABLES                                            
055000     ADD +1000 TO WS-LEN-2                                                
055100     .                                                                    
055200     EJECT                                                                
055300                                                                          
055400 CB-PERFORM-WHERE-COUNT SECTION.                                          
055500     IF WS-IXW2 = +1                                                      
055600       MOVE 'WHERE '        TO COUNT-FIELDS3(WS-LEN-C:6)                  
055700       ADD +6               TO WS-LEN-C                                   
055800       MOVE VL2S-WHERE-COND TO COUNT-FIELDS3(WS-LEN-C:1000)               
055900       ADD +1000            TO WS-LEN-C                                   
056000     END-IF                                                               
056100     IF WS-IXW2 > +1                                                      
056200       MOVE ' AND '         TO COUNT-FIELDS3(WS-LEN-C:5)                  
056300       ADD +5               TO WS-LEN-C                                   
056400       MOVE VL2S-WHERE-COND TO COUNT-FIELDS3(WS-LEN-C:1000)               
056500       ADD +1000            TO WS-LEN-C                                   
056600     END-IF                                                               
056700     IF ITEM-PPS                                                          
056800       MOVE ', '            TO COUNT-TABLES-U1(1:2)                       
056900       MOVE SELECT-OWNER    TO COUNT-TABLES-U1(3:5)                       
057000       MOVE '.TQ1TRPPS B '   TO COUNT-TABLES-U1(8:12)                     
057100       MOVE 'AND A.TILASTN  = B.TILASTN    ' TO COUNT-JOIN(1:30)          
057200       MOVE 'AND A.IDPRODNR = B.IDPRODNR   ' TO COUNT-JOIN(31:30)         
057300       MOVE 'AND A.IDKOLLI  = B.IDKOLLI    ' TO COUNT-JOIN(61:30)         
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700                                                                          
057800 CC-PERFORM-TABLES SECTION.                                               
057900     IF WS-IX = 1                                                         
058000       IF ITEM-PPS                                                        
058100         MOVE ', '          TO SELECT-TABLES-U1(1:2)                      
058200         MOVE SELECT-OWNER  TO SELECT-TABLES-U1(3:5)                      
058300         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U1(8:12)                    
058400       END-IF                                                             
058500     END-IF                                                               
058600     IF WS-IX = 2                                                         
058700       IF ITEM-PPS                                                        
058800         MOVE ', '          TO SELECT-TABLES-U2(1:2)                      
058900         MOVE SELECT-OWNER  TO SELECT-TABLES-U2(3:5)                      
059000         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U2(8:12)                    
059100       END-IF                                                             
059200     END-IF                                                               
059300     IF WS-IX = 3                                                         
059400       IF ITEM-PPS                                                        
059500         MOVE ', '          TO SELECT-TABLES-U3(1:2)                      
059600         MOVE SELECT-OWNER  TO SELECT-TABLES-U3(3:5)                      
059700         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U3(8:12)                    
059800       END-IF                                                             
059900     END-IF                                                               
060000     IF WS-IX = 4                                                         
060100       IF ITEM-PPS                                                        
060200         MOVE ', '          TO SELECT-TABLES-U4(1:2)                      
060300         MOVE SELECT-OWNER  TO SELECT-TABLES-U4(3:5)                      
060400         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U4(8:12)                    
060500       END-IF                                                             
060600     END-IF                                                               
060700     IF WS-IX = 5                                                         
060800       IF ITEM-PPS                                                        
060900         MOVE ', '          TO SELECT-TABLES-U5(1:2)                      
061000         MOVE SELECT-OWNER  TO SELECT-TABLES-U5(3:5)                      
061100         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U5(8:12)                    
061200       END-IF                                                             
061300     END-IF                                                               
061400     IF WS-IX = 6                                                         
061500       IF ITEM-PPS                                                        
061600         MOVE ', '          TO SELECT-TABLES-U6(1:2)                      
061700         MOVE SELECT-OWNER  TO SELECT-TABLES-U6(3:5)                      
061800         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U6(8:12)                    
061900       END-IF                                                             
062000     END-IF                                                               
062100     IF WS-IX = 7                                                         
062200       IF ITEM-PPS                                                        
062300         MOVE ', '          TO SELECT-TABLES-U7(1:2)                      
062400         MOVE SELECT-OWNER  TO SELECT-TABLES-U7(3:5)                      
062500         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U7(8:12)                    
062600       END-IF                                                             
062700     END-IF                                                               
062800     IF WS-IX = 8                                                         
062900       IF ITEM-PPS                                                        
063000         MOVE ', '          TO SELECT-TABLES-U8(1:2)                      
063100         MOVE SELECT-OWNER  TO SELECT-TABLES-U8(3:5)                      
063200         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U8(8:12)                    
063300       END-IF                                                             
063400     END-IF                                                               
063500     IF WS-IX = 9                                                         
063600       IF ITEM-PPS                                                        
063700         MOVE ', '          TO SELECT-TABLES-U9(1:2)                      
063800         MOVE SELECT-OWNER  TO SELECT-TABLES-U9(3:5)                      
063900         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U9(8:12)                    
064000       END-IF                                                             
064100     END-IF                                                               
064200     IF WS-IX = 10                                                        
064300       IF ITEM-PPS                                                        
064400         MOVE ', '          TO SELECT-TABLES-U10(1:2)                     
064500         MOVE SELECT-OWNER  TO SELECT-TABLES-U10(3:5)                     
064600         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U10(8:12)                   
064700       END-IF                                                             
064800     END-IF                                                               
064900     IF WS-IX = 11                                                        
065000       IF ITEM-PPS                                                        
065100         MOVE ', '          TO SELECT-TABLES-U11(1:2)                     
065200         MOVE SELECT-OWNER  TO SELECT-TABLES-U11(3:5)                     
065300         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U11(8:12)                   
065400       END-IF                                                             
065500     END-IF                                                               
065600     IF WS-IX = 12                                                        
065700       IF ITEM-PPS                                                        
065800         MOVE ', '          TO SELECT-TABLES-U12(1:2)                     
065900         MOVE SELECT-OWNER  TO SELECT-TABLES-U12(3:5)                     
066000         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U12(8:12)                   
066100       END-IF                                                             
066200     END-IF                                                               
066300     IF WS-IX = 13                                                        
066400       IF ITEM-PPS                                                        
066500         MOVE ', '          TO SELECT-TABLES-U13(1:2)                     
066600         MOVE SELECT-OWNER  TO SELECT-TABLES-U13(3:5)                     
066700         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U13(8:12)                   
066800       END-IF                                                             
066900     END-IF                                                               
067000     IF WS-IX = 14                                                        
067100       IF ITEM-PPS                                                        
067200         MOVE ', '          TO SELECT-TABLES-U14(1:2)                     
067300         MOVE SELECT-OWNER  TO SELECT-TABLES-U14(3:5)                     
067400         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U14(8:12)                   
067500       END-IF                                                             
067600     END-IF                                                               
067700     IF WS-IX = 15                                                        
067800       IF ITEM-PPS                                                        
067900         MOVE ', '          TO SELECT-TABLES-U15(1:2)                     
068000         MOVE SELECT-OWNER  TO SELECT-TABLES-U15(3:5)                     
068100         MOVE '.TQ1TRPPS B ' TO SELECT-TABLES-U15(8:12)                   
068200       END-IF                                                             
068300     END-IF                                                               
068400     .                                                                    
068500     EJECT                                                                
068600                                                                          
068700 CD-VALUE-LIST-TO-SQL     SECTION.                                        
068800     MOVE REQU-IDELMT-SEARCH(WS-IXU)    TO VL2S-IDELMT                    
068900                                           ITEM-CHECK-2                   
069000                                           ITEM-CHECK-3                   
069100     MOVE REQU-TEELMTVAL-SEARCH(WS-IXU) TO VL2S-TEELMTVAL                 
069200     IF ITEM-PUP                                                          
069300       MOVE 'A' TO VL2S-TECORREL                                          
069400     END-IF                                                               
069500     IF ITEM-PPS                                                          
069600       MOVE 'B' TO VL2S-TECORREL                                          
069700     END-IF                                                               
069800     IF ITEM-ALFA                                                         
069900       MOVE 'AN'  TO VL2S-KDFORMAT                                        
070000     END-IF                                                               
070100     IF ITEM-NUM                                                          
070200       MOVE 'NU'  TO VL2S-KDFORMAT                                        
070300     END-IF                                                               
070400     IF ITEM-DATE6                                                        
070500       MOVE 'D6'  TO VL2S-KDFORMAT                                        
070600     END-IF                                                               
070700     CALL WQ0XVL2S USING VL2S-WQ0XVL2S                                    
070800     IF VL2S-KDSVAR NOT = SPACE                                           
070900       MOVE NOO TO KEYS-SW                                                
071000       MOVE ERROR-FROM-WQ0XVL2S        TO WS-IDMSG-ERROR                  
071100       MOVE REQU-IDELMT-SEARCH(WS-IXU) TO WS-IDELMT-ERROR                 
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500                                                                          
071600 D-PERFORM-REQUEST SECTION.                                               
071700     MOVE +1                   TO WS-IX                                   
071800     PERFORM UNTIL WS-IX > WS-MAX-LINES                                   
071900     OR REQU-IDELMT(WS-IX) = SPACE                                        
072000                                                                          
072100       MOVE SPACE              TO SELECT-JOIN                             
072200       MOVE REQU-IDELMT(WS-IX) TO ITEM-CHECK-3                            
072300       IF ITEM-PUP                                                        
072400         MOVE 'A' TO WS-TECORREL                                          
072500       END-IF                                                             
072600       IF ITEM-PPS                                                        
072700         MOVE 'B' TO WS-TECORREL                                          
072800         PERFORM S03-ADD-JOIN                                             
072900       END-IF                                                             
073000       STRING WS-TECORREL        DELIMITED BY SIZE                        
073100              '.'                DELIMITED BY SIZE                        
073200              REQU-IDELMT(WS-IX) DELIMITED BY SIZE                        
073300       INTO SELECT-FIELDS2                                                
073400                                                                          
073500       MOVE SPACE              TO STMTBUFF-D                              
073600**** USE RIGHT WHERE SATS                                                 
073700       IF WS-IX = 1                                                       
073800         MOVE SELECT-FIELDS-U1  TO SELECT-FIELDS5                         
073900         MOVE SELECT-TABLES-U1  TO SELECT-TABLE                           
074000       END-IF                                                             
074100       IF WS-IX = 2                                                       
074200         MOVE SELECT-FIELDS-U2  TO SELECT-FIELDS5                         
074300         MOVE SELECT-TABLES-U2  TO SELECT-TABLE                           
074400       END-IF                                                             
074500       IF WS-IX = 3                                                       
074600         MOVE SELECT-FIELDS-U3  TO SELECT-FIELDS5                         
074700         MOVE SELECT-TABLES-U3  TO SELECT-TABLE                           
074800       END-IF                                                             
074900       IF WS-IX = 4                                                       
075000         MOVE SELECT-FIELDS-U4  TO SELECT-FIELDS5                         
075100         MOVE SELECT-TABLES-U4  TO SELECT-TABLE                           
075200       END-IF                                                             
075300       IF WS-IX = 5                                                       
075400         MOVE SELECT-FIELDS-U5  TO SELECT-FIELDS5                         
075500         MOVE SELECT-TABLES-U5  TO SELECT-TABLE                           
075600       END-IF                                                             
075700       IF WS-IX = 6                                                       
075800         MOVE SELECT-FIELDS-U6  TO SELECT-FIELDS5                         
075900         MOVE SELECT-TABLES-U6  TO SELECT-TABLE                           
076000       END-IF                                                             
076100       IF WS-IX = 7                                                       
076200         MOVE SELECT-FIELDS-U7  TO SELECT-FIELDS5                         
076300         MOVE SELECT-TABLES-U7  TO SELECT-TABLE                           
076400       END-IF                                                             
076500       IF WS-IX = 8                                                       
076600         MOVE SELECT-FIELDS-U8  TO SELECT-FIELDS5                         
076700         MOVE SELECT-TABLES-U8  TO SELECT-TABLE                           
076800       END-IF                                                             
076900       IF WS-IX = 9                                                       
077000         MOVE SELECT-FIELDS-U9  TO SELECT-FIELDS5                         
077100         MOVE SELECT-TABLES-U9  TO SELECT-TABLE                           
077200       END-IF                                                             
077300       IF WS-IX = 10                                                      
077400         MOVE SELECT-FIELDS-U10 TO SELECT-FIELDS5                         
077500         MOVE SELECT-TABLES-U10 TO SELECT-TABLE                           
077600       END-IF                                                             
077700       IF WS-IX = 11                                                      
077800         MOVE SELECT-FIELDS-U11 TO SELECT-FIELDS5                         
077900         MOVE SELECT-TABLES-U11 TO SELECT-TABLE                           
078000       END-IF                                                             
078100       IF WS-IX = 12                                                      
078200         MOVE SELECT-FIELDS-U12 TO SELECT-FIELDS5                         
078300         MOVE SELECT-TABLES-U12 TO SELECT-TABLE                           
078400       END-IF                                                             
078500       IF WS-IX = 13                                                      
078600         MOVE SELECT-FIELDS-U13 TO SELECT-FIELDS5                         
078700         MOVE SELECT-TABLES-U13 TO SELECT-TABLE                           
078800       END-IF                                                             
078900       IF WS-IX = 14                                                      
079000         MOVE SELECT-FIELDS-U14 TO SELECT-FIELDS5                         
079100         MOVE SELECT-TABLES-U14 TO SELECT-TABLE                           
079200       END-IF                                                             
079300       IF WS-IX = 15                                                      
079400         MOVE SELECT-FIELDS-U15 TO SELECT-FIELDS5                         
079500         MOVE SELECT-TABLES-U15 TO SELECT-TABLE                           
079600       END-IF                                                             
079700       IF SELECT-FIELDS5 > SPACE                                          
079800         IF SELECT-TABLE > SPACE                                          
079900           PERFORM S03-ADD-JOIN                                           
080000         END-IF                                                           
080100       ELSE                                                               
080200         IF SELECT-TABLE > SPACE                                          
080300           PERFORM S03-ADD-JOIN-WHERE                                     
080400         END-IF                                                           
080500       END-IF                                                             
080600                                                                          
080700       MOVE +1                 TO WS-LEN-A                                
080800       STRING SELECT-FIELDS1 DELIMITED BY SIZE                            
080900              SELECT-FIELDS2 DELIMITED BY SIZE                            
081000              SELECT-FIELDS3 DELIMITED BY SIZE                            
081100              SELECT-FIELDS4 DELIMITED BY SIZE                            
081200              SELECT-FIELDS5 DELIMITED BY SIZE                            
081300              SELECT-FIELDS6 DELIMITED BY SIZE                            
081400       INTO STMTBUFF-D                                                    
081500       WITH POINTER WS-LEN-A                                              
081600       MOVE WS-LEN-A           TO STMTBUFF-L                              
081700                                                                          
081800       PERFORM DB2-OPEN-TQ1TRPUP                                          
081900                                                                          
082000       MOVE REQU-IDELMT(WS-IX) TO RESP-IDELMT(WS-IX)                      
082100                                  ITEM-CHECK-2                            
082200       MOVE SPACE              TO RESP-TEELMTVAL(WS-IX)                   
082300       MOVE 1                           TO WSEGSTART                      
082400       MOVE LENGTH OF RESP-TEELMTVAL(1) TO WSEGSTOPP                      
082500       SUBTRACT LENGTH OF WS-RESULT  FROM  WSEGSTOPP                      
082600                                                                          
082700       IF ITEM-ALFA                                                       
082800         PERFORM DB2-FETCH-TQ1TRPUP                                       
082900       ELSE                                                               
083000         PERFORM DB2-FETCH-TQ1TRPUP-NUM                                   
083100       END-IF                                                             
083200                                                                          
083300       PERFORM UNTIL LINES-MISSING OR WSEGSTART > WSEGSTOPP               
083400         PERFORM DA-READ-SHOW-INFO-TABELL                                 
083500         IF ITEM-ALFA                                                     
083600           PERFORM DB2-FETCH-TQ1TRPUP                                     
083700         ELSE                                                             
083800           PERFORM DB2-FETCH-TQ1TRPUP-NUM                                 
083900         END-IF                                                           
084000       END-PERFORM                                                        
084100                                                                          
084200**** IF RESULT HAS OVERFLOW                                               
084300       IF WSEGSTART > WSEGSTOPP                                           
084400         MOVE VALUE-OVERFLOW     TO WS-IDMSG-ERROR                        
084500         MOVE REQU-IDELMT(WS-IX) TO WS-IDELMT-ERROR                       
084600       END-IF                                                             
084700                                                                          
084800       PERFORM DB2-CLOSE-TQ1TRPUP                                         
084900                                                                          
085000       ADD 1 TO WS-IX                                                     
085100     END-PERFORM                                                          
085200                                                                          
085300     MOVE COUNT-TABLES-U1  TO COUNT-TABLE                                 
085400**** GET COUNT OF HITS IF QUESTION IS ASKED                               
085500     MOVE SPACE          TO STMTBUFF-D                                    
085600     MOVE +1 TO WS-LEN-A                                                  
085700     STRING COUNT-FIELDS1  DELIMITED BY SIZE                              
085800            COUNT-FIELDS2  DELIMITED BY SIZE                              
085900            COUNT-FIELDS3  DELIMITED BY SIZE                              
086000            COUNT-FIELDS4  DELIMITED BY SIZE                              
086100     INTO STMTBUFF-D                                                      
086200     WITH POINTER WS-LEN-A                                                
086300     MOVE WS-LEN-A       TO STMTBUFF-L                                    
086400                                                                          
086500     PERFORM DB2-COUNT-OPEN                                               
086600     PERFORM DB2-COUNT-FETCH                                              
086700     PERFORM DB2-COUNT-CLOSE                                              
086800                                                                          
086900     MOVE WS-COUNTER     TO RESP-KVCOUNT                                  
087000     .                                                                    
087100     EJECT                                                                
087200                                                                          
087300 DA-READ-SHOW-INFO-TABELL SECTION.                                        
087400     IF ITEM-NUM                                                          
087500       MOVE WS-RESULT-NUM   TO WS-RESULT-NUM-3                            
087601**** MAKE SURE THAT RESULT ZERO IS SHOWN                                  
087610       IF WS-RESULT-NUM-3 = ZERO                                          
087620         MOVE '0' TO WS-RESULT                                            
087630       ELSE                                                               
087640         MOVE ZERO TO TALLY                                               
087700         INSPECT WS-RESULT-NUM-3 TALLYING TALLY FOR LEADING ZEROES        
087800         MOVE WS-RESULT-NUM-3(TALLY + 1: 15 - TALLY) TO WS-RESULT         
088210       END-IF                                                             
088300     END-IF                                                               
088400                                                                          
088500     STRING                                                               
088600     WS-RESULT DELIMITED BY SPACE                                         
088700     ', '      DELIMITED BY SIZE                                          
088800     INTO RESP-TEELMTVAL(WS-IX)                                           
088900     WITH POINTER WSEGSTART                                               
089000     .                                                                    
089100     EJECT                                                                
089200                                                                          
089300 E-SELECT-INIT SECTION.                                                   
089400     PERFORM DB2-SELECT-COUNT                                             
089500     MOVE WS-COUNTER     TO RESP-KVCOUNT                                  
089600                                                                          
089700     PERFORM DB2-SELECT-FIRST-DATE                                        
089800**** MOVE RESULT                                                          
089900     MOVE WS-RESULT-NUM   TO WS-RESULT-NUM-3                              
089910     IF WS-RESULT-NUM-3 = ZERO                                            
089920       MOVE '0' TO WS-RESULT                                              
089930     ELSE                                                                 
090000       MOVE ZERO TO TALLY                                                 
090100       INSPECT WS-RESULT-NUM-3 TALLYING TALLY FOR LEADING ZEROES          
090200       MOVE WS-RESULT-NUM-3(TALLY + 1: 15 - TALLY) TO WS-RESULT           
090500     END-IF                                                               
090600                                                                          
090700     MOVE WS-RESULT(1:6) TO RESP-TEELMTVAL(1)(1:6)                        
090800                                                                          
090900     PERFORM DB2-SELECT-LAST-DATE                                         
091000                                                                          
091100     MOVE WS-RESULT-NUM   TO WS-RESULT-NUM-3                              
091110     IF WS-RESULT-NUM-3 = ZERO                                            
091120       MOVE '0' TO WS-RESULT                                              
091130     ELSE                                                                 
091200       MOVE ZERO TO TALLY                                                 
091300       INSPECT WS-RESULT-NUM-3 TALLYING TALLY FOR LEADING ZEROES          
091400       MOVE WS-RESULT-NUM-3(TALLY + 1: 15 - TALLY) TO WS-RESULT           
091700     END-IF                                                               
091800                                                                          
091900     MOVE '-'            TO RESP-TEELMTVAL(1)(7:1)                        
092000     MOVE WS-RESULT(1:6) TO RESP-TEELMTVAL(1)(8:6)                        
092100     .                                                                    
092200     EJECT                                                                
092300                                                                          
092400 F-READ-SHOW-INFO SECTION.                                                
092500     MOVE 001                      TO RESP-IDMSGVER                       
092600     MOVE WS-IDMSG-INFO            TO RESP-IDMSG-INFO                     
092700     MOVE WS-IDMSG-ERROR           TO RESP-IDMSG-ERROR                    
092800     MOVE WS-IDELMT-ERROR          TO RESP-IDELMT-ERROR                   
092900     .                                                                    
093000     EJECT                                                                
093100                                                                          
093200*    --- DISPATCHER-SECTIONS                                              
093300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
093400     MOVE 'GETARG'                               TO SUB-KDFUNC            
093500     MOVE 'CARPARTS.TRPUP.KEYVALUES'             TO SUB-ADDISPABS         
093600     MOVE LENGTH OF REQU-AREA                    TO SUB-KVDLEN            
093700                                                                          
093800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
093900                                                                          
094000     IF SUB-KDRC > 0                                                      
094100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
094200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
094300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
094400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
094500     END-IF                                                               
094600     .                                                                    
094700     SKIP3                                                                
094800                                                                          
094900 S02-RETURN-RESPONSE SECTION.                                             
095000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
095100                                                                          
095200     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
095300                                                                          
095400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
095500                                                                          
095600     IF SUB-KDRC > 0                                                      
095700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
095800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
095900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
096000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
096100     END-IF                                                               
096200     .                                                                    
096300     EJECT                                                                
096400                                                                          
096500 S03-ADD-JOIN SECTION.                                                    
096600     MOVE 'AND A.TILASTN  = B.TILASTN    ' TO SELECT-JOIN(1:30)           
096700     MOVE 'AND A.IDPRODNR = B.IDPRODNR   ' TO SELECT-JOIN(31:30)          
096800     MOVE 'AND A.IDKOLLI  = B.IDKOLLI    ' TO SELECT-JOIN(61:30)          
096900     .                                                                    
097000     EJECT                                                                
097100                                                                          
097200 S03-ADD-JOIN-WHERE SECTION.                                              
097300     MOVE 'WHERE A.TILASTN  = B.TILASTN  ' TO SELECT-JOIN(1:30)           
097400     MOVE 'AND A.IDPRODNR = B.IDPRODNR   ' TO SELECT-JOIN(31:30)          
097500     MOVE 'AND A.IDKOLLI  = B.IDKOLLI    ' TO SELECT-JOIN(61:30)          
097600     .                                                                    
097700     EJECT                                                                
097800                                                                          
097900 DB2-OPEN-TQ1TRPUP    SECTION.                                            
098000     MOVE 000100 TO GOOD-SQLCODECODES                                     
098100     EXEC SQL                                                             
098200         DECLARE STMT STATEMENT                                           
098300     END-EXEC                                                             
098400     MOVE SQLCODE TO SQLCODE-WS                                           
098500     PERFORM DB2-STATUS-CHECK                                             
098600                                                                          
098700     MOVE 000100  TO GOOD-SQLCODECODES                                    
098800     EXEC SQL                                                             
098900         DECLARE TQ1TRPUP-CRS CURSOR FOR STMT                             
099000     END-EXEC                                                             
099100     MOVE SQLCODE TO SQLCODE-WS                                           
099200     PERFORM DB2-STATUS-CHECK                                             
099300                                                                          
099400     MOVE 000100  TO GOOD-SQLCODECODES                                    
099500     EXEC SQL                                                             
099600         PREPARE STMT FROM :STMTBUFF                                      
099700     END-EXEC                                                             
099800     MOVE SQLCODE TO SQLCODE-WS                                           
099900     PERFORM DB2-STATUS-CHECK                                             
100000                                                                          
100100     MOVE 000100  TO GOOD-SQLCODECODES                                    
100200     EXEC SQL                                                             
100300         OPEN TQ1TRPUP-CRS                                                
100400     END-EXEC                                                             
100500     MOVE SQLCODE TO SQLCODE-WS                                           
100600     PERFORM DB2-STATUS-CHECK                                             
100700     .                                                                    
100800     EJECT                                                                
100900                                                                          
101000 DB2-FETCH-TQ1TRPUP    SECTION.                                           
101100     MOVE 000100  TO GOOD-SQLCODECODES                                    
101200     EXEC SQL                                                             
101300         FETCH TQ1TRPUP-CRS                                               
101400         INTO  :WS-RESULT                                                 
101500     END-EXEC                                                             
101600     MOVE SQLCODE TO SQLCODE-WS                                           
101700     PERFORM DB2-STATUS-CHECK                                             
101800     .                                                                    
101900     EJECT                                                                
102000                                                                          
102100 DB2-FETCH-TQ1TRPUP-NUM SECTION.                                          
102200     MOVE 000100  TO GOOD-SQLCODECODES                                    
102300     EXEC SQL                                                             
102400         FETCH TQ1TRPUP-CRS                                               
102500         INTO  :WS-RESULT-NUM                                             
102600     END-EXEC                                                             
102700     MOVE SQLCODE TO SQLCODE-WS                                           
102800     PERFORM DB2-STATUS-CHECK                                             
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200 DB2-CLOSE-TQ1TRPUP    SECTION.                                           
103300     EXEC SQL                                                             
103400        CLOSE TQ1TRPUP-CRS                                                
103500     END-EXEC                                                             
103600     .                                                                    
103700     EJECT                                                                
103800                                                                          
103900 DB2-COUNT-OPEN  SECTION.                                                 
104000     MOVE 000100 TO GOOD-SQLCODECODES                                     
104100     EXEC SQL                                                             
104200         DECLARE STMT2 STATEMENT                                          
104300     END-EXEC                                                             
104400     MOVE SQLCODE TO SQLCODE-WS                                           
104500     PERFORM DB2-STATUS-CHECK                                             
104600                                                                          
104700     MOVE 000100  TO GOOD-SQLCODECODES                                    
104800     EXEC SQL                                                             
104900         DECLARE TQ1TRPUP-CNT CURSOR FOR STMT2                            
105000     END-EXEC                                                             
105100     MOVE SQLCODE TO SQLCODE-WS                                           
105200     PERFORM DB2-STATUS-CHECK                                             
105300                                                                          
105400     MOVE 000100  TO GOOD-SQLCODECODES                                    
105500     EXEC SQL                                                             
105600         PREPARE STMT2 FROM :STMTBUFF                                     
105700     END-EXEC                                                             
105800     MOVE SQLCODE TO SQLCODE-WS                                           
105900     PERFORM DB2-STATUS-CHECK                                             
106000                                                                          
106100     MOVE 000100  TO GOOD-SQLCODECODES                                    
106200     EXEC SQL                                                             
106300         OPEN TQ1TRPUP-CNT                                                
106400     END-EXEC                                                             
106500     MOVE SQLCODE TO SQLCODE-WS                                           
106600     PERFORM DB2-STATUS-CHECK                                             
106700     .                                                                    
106800     EJECT                                                                
106900                                                                          
107000 DB2-COUNT-FETCH SECTION.                                                 
107100     MOVE 000100  TO GOOD-SQLCODECODES                                    
107200     EXEC SQL                                                             
107300         FETCH TQ1TRPUP-CNT                                               
107400         INTO  :WS-COUNTER                                                
107500     END-EXEC                                                             
107600     MOVE SQLCODE TO SQLCODE-WS                                           
107700     PERFORM DB2-STATUS-CHECK                                             
107800     .                                                                    
107900     EJECT                                                                
108000                                                                          
108100 DB2-COUNT-CLOSE SECTION.                                                 
108200     MOVE 000100  TO GOOD-SQLCODECODES                                    
108300     EXEC SQL                                                             
108400        CLOSE TQ1TRPUP-CNT                                                
108500     END-EXEC                                                             
108600     MOVE SQLCODE TO SQLCODE-WS                                           
108700     PERFORM DB2-STATUS-CHECK                                             
108800     .                                                                    
108900     EJECT                                                                
109000                                                                          
109100 DB2-SELECT-COUNT      SECTION.                                           
109200     MOVE 000100  TO GOOD-SQLCODECODES                                    
109300     EXEC SQL                                                             
109400       SELECT COUNT(*)                                                    
109500       INTO   :WS-COUNTER                                                 
109600       FROM   TQ1TRPUP A                                                  
109700     END-EXEC                                                             
109800     MOVE SQLCODE TO SQLCODE-WS                                           
109900     PERFORM DB2-STATUS-CHECK                                             
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300 DB2-SELECT-FIRST-DATE SECTION.                                           
110400     MOVE 000100  TO GOOD-SQLCODECODES                                    
110500     EXEC SQL                                                             
110600       SELECT DISTINCT(A.TILASTN)                                         
110700       INTO   :WS-RESULT-NUM                                              
110800       FROM   TQ1TRPUP A                                                  
110900       ORDER BY A.TILASTN                                                 
111000       FETCH FIRST 1 ROW ONLY                                             
111100     END-EXEC                                                             
111200     MOVE SQLCODE TO SQLCODE-WS                                           
111300     PERFORM DB2-STATUS-CHECK                                             
111400     .                                                                    
111500     EJECT                                                                
111600                                                                          
111700 DB2-SELECT-LAST-DATE SECTION.                                            
111800     MOVE 000100  TO GOOD-SQLCODECODES                                    
111900     EXEC SQL                                                             
112000       SELECT DISTINCT(A.TILASTN)                                         
112100       INTO   :WS-RESULT-NUM                                              
112200       FROM   TQ1TRPUP A                                                  
112300       ORDER BY A.TILASTN DESC                                            
112400       FETCH FIRST 1 ROW ONLY                                             
112500     END-EXEC                                                             
112600     MOVE SQLCODE TO SQLCODE-WS                                           
112700     PERFORM DB2-STATUS-CHECK                                             
112800     .                                                                    
112900     EJECT                                                                
113000                                                                          
113100 DB2-STATUS-CHECK     SECTION.                                            
113200     SET SQLCODE-IX TO 1                                                  
113300     SEARCH GOOD-SQLCODE                                                  
113400       AT END                                                             
113500          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
113600          DELIMITED BY SIZE INTO ERROR-TEXT                               
113700          CALL ABEND USING RKOD-ABEND-DB2                                 
113800       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
113900     END-SEARCH                                                           
114000     .                                                                    
