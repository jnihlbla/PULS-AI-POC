000100 PROCESS DYNAM                                                            
000200*COMPOPT DYNBIND=YES                                                      
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     WQ011100.                                                
000500 AUTHOR.         HENRIKSSON ANDERS.                                       
000600 DATE-WRITTEN.   2010-12-27.                                              
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    NAME                                                                 
001000*        CARPARTS.TRPUP.REPORT                                            
001100*    FUNCTION:                                                            
001200*        PRODUCE REPORTS OR EXTRACT FROM TRANSPORT FOLLOW-UP              
001300*        ANSWER VIA SUBPROGRAM WZ01SUB TO WEB.                            
001400*        ANSWER VIA SUBPROGRAM WZ01SEND TO D&P                            
001500*                                                                         
001600*        THE PROGRAM READS     TABLE TQ1TRPUP                             
001700*        THE PROGRAM READS     TABLE TQ1TRPPS                             
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: WQ0111T                                             
002100*        REQUEST:     WQ0111I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    WQ0111O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'WQ011100'.            
003500                                                                          
003600*    --- WORK FIELDS FOR ERROR MESSAGE WHEN CALLING ABEND.                
003700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003800 77  KDRC-DISPLAY                PIC Z(5).                                
003900                                                                          
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  WS-MAX-LINES                PIC S9(3)   VALUE +15     COMP-3.        
004400 77  WS-MAX-KVRADER              PIC S9(3)   VALUE +49     COMP-3.        
004500 77  WS-MAX-SEARCHCRIT           PIC S9(3)   VALUE +10     COMP-3.        
004600 77  WS-MAX-PUT                  PIC S9(5)   VALUE +20000  COMP-3.        
004700 77  WS-COUNTER                  PIC S9(9)   VALUE ZERO    COMP-3.        
004800 77  W-ANT                       PIC S9(3)   VALUE ZERO    COMP-3.        
004900                                                                          
005000 01  WS-LEN-1                    PIC S9(9)   BINARY.                      
005100 01  WS-LEN-2                    PIC S9(9)   BINARY.                      
005200 01  WS-KVDLEN                   PIC S9(9)   BINARY.                      
005300                                                                          
005400 01  WS-LEN-A                    PIC S9(9)   BINARY.                      
005500 01  WS-LEN-S                    PIC S9(9)   BINARY.                      
005600 01  WS-LEN-GB                   PIC S9(9)   BINARY.                      
005700 01  WS-LEN-OB                   PIC S9(9)   BINARY.                      
005800 01  WS-A                        PIC X       VALUE X'7D'.                 
005900                                                                          
006000 01  WS-RESULT                   PIC X(15)   VALUE SPACE.                 
006100 01  WS-RESULT-NUM               PIC S9(15)  VALUE ZERO COMP-3.           
006200 01  WS-RESULT-NUM-3             PIC 9(15)   VALUE ZERO.                  
006300                                                                          
006400 01  WLEN                        PIC S9(9)   BINARY.                      
006500 01  WSEGSTART                   PIC S9(9)   BINARY.                      
006600 01  WSEGSTOPP                   PIC S9(9)   BINARY.                      
006700 01  WSEGSTOPP-DOP               PIC S9(9)   BINARY.                      
006800 01  WSEGLEN-X.                                                           
006900     03 WSEGLEN                  PIC S9(5)   BINARY.                      
007000                                                                          
007100 01  WS-SQLD-TABELL.                                                      
007200       03  WS-SQLDATA OCCURS 30 TIMES PIC X(30).                          
007300       03  WS-SQLIND  OCCURS 30 TIMES PIC X(9).                           
007400                                                                          
007500 77  KEYS-SW                     PIC X       VALUE SPACE.                 
007600     88  KEYS-OK                             VALUE 'Y'.                   
007700     88  KEYS-NO                             VALUE 'N'.                   
007800                                                                          
007900 77  ITEM-CHECK-SEARCH          PIC X(16)   VALUE SPACE.                  
008000     88  ITEM-EXIST-SEARCH                  VALUE 'IDDC'                  
008100                                                  'IDLEVNR'               
008200                                                  'IDLANDX2'              
008300                                                  'IDDISTR'               
008400                                                  'IDKUNDNR'              
008500                                                  'KDORDKL'               
008600                                                  'KDFRAKT'               
008700                                                  'IDKOLLI'               
008800                                                  'IDPSN'                 
008900                                                  'KDFARLIG_KOLLI'        
009000                                                  'FLDIRLEV'              
009100                                                  'TILASTN'               
009200                                                  'FLLDCKND'.             
009300     EJECT                                                                
009400 77  ITEM-CHECK-2               PIC X(16)   VALUE SPACE.                  
009500     88 ITEM-ALFA                         VALUE 'IDDC'                    
009600                                                'IDLEVNR'                 
009700                                                'IDLANDX2'                
009800                                                'FLDIRLEV'                
009900                                                'FLLDCKND'                
010000                                                'IDPSN'.                  
010100     88 ITEM-NUM                          VALUE 'KDORDKL'                 
010200                                                'IDKUNDNR'                
010300                                                'IDDISTR'                 
010400                                                'KDFRAKT'                 
010500                                                'IDKOLLI'                 
010600                                               'KDFARLIG_KOLLI'.          
010700     88 ITEM-DATE6                        VALUE 'TILASTN'.                
010800     EJECT                                                                
010900 77  ITEM-CHECK-3               PIC X(16)   VALUE SPACE.                  
011000     88 ITEM-PUP                            VALUE 'IDDC'                  
011100                                                  'IDLEVNR'               
011200                                                  'IDLANDX2'              
011300                                                  'FLDIRLEV'              
011400                                                  'FLLDCKND'              
011500                                                  'IDKUNDNR'              
011600                                                  'IDDISTR'               
011700                                                  'KDFRAKT'               
011800                                                  'IDKOLLI'               
011900                                                  'KDFARLIG_KOLLI'        
012000                                                  'KDORDKL'               
012100                                                  'TILASTN'.              
012200     88 ITEM-PPS                            VALUE 'IDPSN'                 
012300                                                  'VKART_FG'              
012400                                                  'SUM(VKART_FG)'         
012500                                                  'SUM(VLFG)'             
012600                                                  'VLFG'.                 
012700     88 ITEM-CORR                           VALUE 'TILASTN'               
012800                                                  'IDKOLLI'               
012900                                                  'IDPRODNR'.             
013000     EJECT                                                                
013100                                                                          
013200*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
013300 01  GENERAL-SUBPROGRAMS.                                                 
013400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
013600     03  WQ0XVL2S                PIC X(8)    VALUE 'WQ0XVL2S'.            
013700     03  WQ0XVSEL                PIC X(8)    VALUE 'WQ0XVSEL'.            
013800     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
013900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
014000     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
014100     SKIP3                                                                
014200                                                                          
014300*    --- PARAMETERS TO ABEND                                              
014400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
014800     SKIP3                                                                
014900                                                                          
015000 01  MESSAGE-CODES.                                                       
015100     03  ERROR-CODES.                                                     
015200       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
015300       05 INVALID-ITEM-NAME          PIC X(3)    VALUE '101'.             
015400       05 VALUE-OVERFLOW             PIC X(3)    VALUE '102'.             
015500       05 ERROR-FROM-WQ0XVL2S        PIC X(3)    VALUE '103'.             
015600       05 MAILADRESS-MISSING         PIC X(3)    VALUE '104'.             
015700       05 DATA-EXCEED-LIMIT          PIC X(3)    VALUE '105'.             
015800                                                                          
015900     03  INFO-CODES.                                                      
016000       05 INFO-MORE-DATA-EXIST       PIC X(3)    VALUE '120'.             
016100       05 DATA-SENT-TO-DOP           PIC X(3)    VALUE '121'.             
016200                                                                          
016300 01  DATA-EXCEED-LIMIT-TEXT         PIC X(100)   VALUE                    
016400     '!REPORT;DATA;TRUNCATED!;ONLY;THE;FIRST;20 000;LINES;HAVE;BEE        
016500-    'N;CREATED;'.                                                        
016600                                                                          
016700                                                                          
016800*    --- PARAMETERS TO VIMSID                                             
016900 01  VIMSID-PARM.                                                         
017000   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
017100   03  FILLER                    PIC X(4)    VALUE SPACE.                 
017200     EJECT                                                                
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
017500     SKIP3                                                                
017600 01  -COPY WZ01SUB                                                        
017700     EJECT                                                                
017800                                                                          
017900 01  -COPY WQ0XVL2S                                                       
018000     EJECT                                                                
018100                                                                          
018200 01  -COPY WQ0XVSEL                                                       
018300     EJECT                                                                
018400                                                                          
018500 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
018600 01  -COPY WZ01SEND                                                       
018700     EJECT                                                                
018800                                                                          
018900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
019000     SKIP3                                                                
019100 01  REQU-AREA.                                                           
019200*    03  -COPY WZ01REQU                                                   
019300*    03  -COPY WQ0111I1                                                   
019400     EJECT                                                                
019500                                                                          
019600*    --- UT-AREOR                                                         
019700 01  OUTPUT-AREA                 PIC X(24)   VALUE 'OUTPUT-AREA'.         
019800                                                                          
019900 01  RESP-AREA.                                                           
020000*    03  -COPY WZ01RESP                                                   
020100*    03  -COPY WQ0111O1                                                   
020200     EJECT                                                                
020300                                                                          
020400 01  HDR-AREA.                                                            
020500*    03  -COPY WZ01REQU -PRE DOP-                                         
020600*    03  -COPY WZ04HDR                                                    
020700                                                                          
020800 01  DOP-LINE-AREA.                                                       
020900     03  DOP-LINE-DATA           PIC X(3000) VALUE SPACE.                 
021000     EJECT                                                                
021100                                                                          
021200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
021300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
021400                                                                          
021500 01  FILLER                      PIC X(16)   VALUE 'SQLDA-AREA'.          
021600       EXEC SQL INCLUDE SQLDA END-EXEC.                                   
021700                                                                          
021800 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
021900 01  DB2-WS.                                                              
022000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
022100         88  CURSOR-OK                       VALUE 000.                   
022200         88  LINES-FOUND                     VALUE 000.                   
022300         88  LINES-MISSING                   VALUE 100.                   
022400         88  RESOURCE-WRONG                  VALUE 904.                   
022500                                                                          
022600     03  GOOD-SQLCODECODES.                                               
022700         05  GOOD-SQLCODE OCCURS 5                                        
022800             INDEXED BY SQLCODE-IX PIC 9(3).                              
022900     EJECT                                                                
023000                                                                          
023100 01  WS-IX                      PIC S9(9)    VALUE ZERO BINARY.           
023200 01  WS-IX5                     PIC S9(9)    VALUE ZERO BINARY.           
023300 01  WS-IX8                     PIC S9(9)    VALUE ZERO BINARY.           
023400 01  WS-IXP                     PIC S9(9)    VALUE ZERO BINARY.           
023500 01  WS-IXU                     PIC S9(9)    VALUE ZERO BINARY.           
023600 01  WS-IXW                     PIC S9(9)    VALUE ZERO BINARY.           
023700 01  WS-SQL-IX                  PIC S9(9)    VALUE ZERO BINARY.           
023800 01  WS-TECORREL                PIC X(1)     VALUE SPACE.                 
023900                                                                          
024000 01  WS-AREA.                                                             
024100     03 WS-KDSTATUS             PIC S9(3)    COMP-3 VALUE ZERO.           
024200     03 WS-KVRADER              PIC Z(4)9(1) VALUE ZERO.                  
024300     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
024400     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
024500     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
024600     EJECT                                                                
024700                                                                          
024800 01  STMTBUFF.                                                            
024900     49 STMTBUFF-L          PIC S9(4)     COMP VALUE ZERO.                
025000     49 STMTBUFF-D          PIC X(17200).                                 
025100     EJECT                                                                
025200                                                                          
025300 01  SQL-QUESTION-FIELDS.                                                 
025400     03 SELECT-FIELDS1.                                                   
025500       05 FILLER         PIC X(1100) VALUE SPACE.                         
025600     03 SELECT-FIELDS4.                                                   
025700       05 FILLER         PIC X(5)   VALUE 'FROM '.                        
025800       05 SELECT-OWNER   PIC X(5)   VALUE SPACE.                          
025900       05 FILLER         PIC X(12)  VALUE '.TQ1TRPUP A '.                 
026000     03 SELECT-FIELDS5.                                                   
026100       05 FILLER         PIC X(15100) VALUE SPACE.                        
026200     03 SELECT-FIELDS6.                                                   
026300       05 FILLER         PIC X(300)   VALUE SPACE.                        
026400     03 SELECT-FIELDS7.                                                   
026500       05 FILLER         PIC X(300)   VALUE SPACE.                        
026600     03 SELECT-FIELDS8.                                                   
026700       05 FILLER         PIC X(30)    VALUE                               
026800                                  'FETCH FIRST 51 ROWS ONLY'.             
026900     03 SELECT-FIELDS9.                                                   
027000       05 FILLER         PIC X(30)    VALUE                               
027100                                  'FETCH FIRST 20001 ROWS ONLY'.          
027200     03 SELECT-FIELDS10.                                                  
027300       05 SELECT-JOIN    PIC X(200)   VALUE SPACE.                        
027400     03 SELECT-FIELDS11.                                                  
027500       05 FILLER         PIC X(25)  VALUE SPACE.                          
027600                                                                          
027700 01  FILLER                    PIC X(16)    VALUE 'TQ1TRPUP-AREA'.        
027800*01  -COPY TQ1TRPUP -PRE TQ1TRPUP-                                        
027900     EJECT                                                                
028000                                                                          
028100 01  FILLER                    PIC X(16)    VALUE 'TQ1TRPPS-AREA'.        
028200*01  -COPY TQ1TRPPS -PRE TQ1TRPPS-                                        
028300     EJECT                                                                
028400                                                                          
028500     EXEC SQL INCLUDE TQ1TRPUP END-EXEC.                                  
028600     EJECT                                                                
028700     EXEC SQL INCLUDE TQ1TRPPS END-EXEC.                                  
028800     EJECT                                                                
028900                                                                          
029000 LINKAGE SECTION.                                                         
029100 PROCEDURE DIVISION.                                                      
029200 MAIN SECTION.                                                            
029300                                                                          
029400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
029500     IF SUB-KDRC = ZERO                                                   
029600       PERFORM A-INIT                                                     
029700       PERFORM B-CHECK-KEYS                                               
029800       IF KEYS-OK                                                         
029900         PERFORM C-CHECK-INDATA-SEARCH                                    
030000       END-IF                                                             
030100       IF KEYS-OK                                                         
030200         PERFORM C-CREATE-SELECT-CLAUSE                                   
030300       END-IF                                                             
030400       IF KEYS-OK                                                         
030500         PERFORM C-CREATE-WHERE-CLAUSE                                    
030600       END-IF                                                             
030700       IF KEYS-OK                                                         
030800         PERFORM D-PERFORM-REQUEST                                        
030900       END-IF                                                             
031000       PERFORM E-READ-SHOW-INFO                                           
031100       IF REQU-KDQOUT = 'W' AND KEYS-OK                                   
031200         CONTINUE                                                         
031300       ELSE                                                               
031400         PERFORM S02-RETURN-RESPONSE                                      
031500       END-IF                                                             
031600     END-IF                                                               
031700                                                                          
031800     MOVE ZERO TO RETURN-CODE                                             
031900     GOBACK                                                               
032000     .                                                                    
032100     EJECT                                                                
032200                                                                          
032300 A-INIT SECTION.                                                          
032400     MOVE YES     TO KEYS-SW                                              
032500     MOVE SPACE   TO RESP-AREA                                            
032600     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
032700     MOVE SPACE   TO RESP-IDMSG-INFO                                      
032800     MOVE SPACE   TO RESP-IDELMT-ERROR                                    
032900     MOVE ZERO    TO RESP-KVRADER                                         
033000     MOVE 30      TO SQLN                                                 
033100                                                                          
033200     MOVE ZERO    TO WS-COUNTER                                           
033300                                                                          
033400     INITIALIZE GOOD-SQLCODECODES                                         
033500                                                                          
033600*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
033700     CALL VIMSID USING VIMSID-PARM                                        
033800     IF IMSID4 = 'IMG0' OR 'IMB0'                                         
033900       MOVE ' WDB2' TO SELECT-OWNER SELECT-OWNER                          
034000     END-IF                                                               
034010     IF IMSID4 = 'IMD1' OR 'IMD0'                                         
034100       MOVE 'WDB2X' TO SELECT-OWNER SELECT-OWNER                          
034200     END-IF                                                               
034210     IF IMSID4 = 'IMY0'                                                   
034220       MOVE 'WDB2I' TO SELECT-OWNER SELECT-OWNER                          
034230     END-IF                                                               
034240     IF IMSID4 = 'IMP1' OR 'IMP0'                                         
034250       MOVE 'WDB2D' TO SELECT-OWNER SELECT-OWNER                          
034260     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500                                                                          
034600 B-CHECK-KEYS SECTION.                                                    
034700     IF REQU-KDPGMACT = 'E'                                               
034800       CONTINUE                                                           
034900     ELSE                                                                 
035000       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
035100       MOVE 'KDPGMACT'    TO WS-IDELMT-ERROR                              
035200       MOVE NOO TO KEYS-SW                                                
035300     END-IF                                                               
035400                                                                          
035500     IF REQU-IDMSGVER NUMERIC                                             
035600       CONTINUE                                                           
035700     ELSE                                                                 
035800       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
035900       MOVE 'IDMSGVER'    TO WS-IDELMT-ERROR                              
036000       MOVE NOO TO KEYS-SW                                                
036100     END-IF                                                               
036200                                                                          
036300     IF REQU-KDQOUT = 'M'                                                 
036400       IF REQU-IDMAIL > SPACE AND NOT = ALL '+'                           
036500         CONTINUE                                                         
036600       ELSE                                                               
036700         MOVE MAILADRESS-MISSING TO WS-IDMSG-ERROR                        
036800         MOVE 'IDMAIL'           TO WS-IDELMT-ERROR                       
036900         MOVE NOO TO KEYS-SW                                              
037000       END-IF                                                             
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500 C-CHECK-INDATA-SEARCH SECTION.                                           
037600**** CHECK VALID TABLE ITEM                                               
037700     MOVE +1 TO WS-IX                                                     
037800     PERFORM UNTIL WS-IX > WS-MAX-LINES                                   
037900       IF REQU-IDELMT-SEARCH(WS-IX) > SPACE                               
038000         MOVE REQU-IDELMT-SEARCH(WS-IX) TO ITEM-CHECK-SEARCH              
038100         IF ITEM-EXIST-SEARCH                                             
038200           ADD +1 TO WS-IX                                                
038300         ELSE                                                             
038400           MOVE INVALID-ITEM-NAME  TO WS-IDMSG-ERROR                      
038500           MOVE REQU-IDELMT-SEARCH(WS-IX) TO WS-IDELMT-ERROR              
038600           MOVE NOO TO KEYS-SW                                            
038700           ADD +15 TO WS-IX                                               
038800         END-IF                                                           
038900       ELSE                                                               
039000         ADD +15 TO WS-IX                                                 
039100       END-IF                                                             
039200     END-PERFORM                                                          
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 C-CREATE-SELECT-CLAUSE SECTION.                                          
039700     MOVE +1 TO WS-LEN-S                                                  
039800     MOVE +1 TO WS-LEN-GB                                                 
039900     MOVE +1 TO WS-LEN-OB                                                 
040000     MOVE +1 TO WS-IX                                                     
040100     MOVE SPACE TO SELECT-FIELDS1                                         
040200     MOVE SPACE TO SELECT-FIELDS6                                         
040300     MOVE SPACE TO SELECT-FIELDS7                                         
040400     MOVE SPACE TO SELECT-JOIN                                            
040500     IF REQU-KDQOUT = 'S'                                                 
040600       MOVE SPACE TO SELECT-FIELDS9                                       
040700     ELSE                                                                 
040800       MOVE SPACE TO SELECT-FIELDS8                                       
040900     END-IF                                                               
041000                                                                          
041100     PERFORM UNTIL WS-IX > WS-MAX-LINES                                   
041200       IF REQU-IDELMT-OB(WS-IX) > SPACE                                   
041300         IF WS-LEN-S = 1                                                  
041400           MOVE 'SELECT '         TO SELECT-FIELDS1(WS-LEN-S:7)           
041500           ADD +7                 TO WS-LEN-S                             
041600           MOVE 'ORDER BY '       TO SELECT-FIELDS6(WS-LEN-OB:9)          
041700           ADD +9                 TO WS-LEN-OB                            
041800           IF REQU-KDREPLVL = 'S'                                         
041900             MOVE 'GROUP BY '       TO SELECT-FIELDS7(WS-LEN-GB:9)        
042000             ADD +9                 TO WS-LEN-GB                          
042100           END-IF                                                         
042200         ELSE                                                             
042300           MOVE ', '                TO SELECT-FIELDS1(WS-LEN-S:2)         
042400           ADD +2                   TO WS-LEN-S                           
042500           MOVE ', '                TO SELECT-FIELDS6(WS-LEN-OB:2)        
042600           ADD +2                   TO WS-LEN-OB                          
042700           IF REQU-KDREPLVL = 'S'                                         
042800             MOVE ', '              TO SELECT-FIELDS7(WS-LEN-GB:2)        
042900             ADD +2                 TO WS-LEN-GB                          
043000           END-IF                                                         
043100         END-IF                                                           
043200                                                                          
043300         MOVE REQU-IDELMT-OB(WS-IX) TO ITEM-CHECK-3                       
043400         IF ITEM-PPS                                                      
043500           MOVE 'B' TO WS-TECORREL                                        
043600           PERFORM S03-ADD-JOIN                                           
043700         ELSE                                                             
043800           MOVE 'A' TO WS-TECORREL                                        
043900         END-IF                                                           
044000                                                                          
044100         MOVE WS-TECORREL           TO                                    
044200                        SELECT-FIELDS1(WS-LEN-S:1)                        
044300         ADD +1                     TO WS-LEN-S                           
044400         MOVE '.'                   TO                                    
044500                        SELECT-FIELDS1(WS-LEN-S:1)                        
044600         ADD +1                     TO WS-LEN-S                           
044700         MOVE REQU-IDELMT-OB(WS-IX) TO                                    
044800                        SELECT-FIELDS1(WS-LEN-S:15)                       
044900         ADD +15                    TO WS-LEN-S                           
045000                                                                          
045100         MOVE WS-TECORREL           TO                                    
045200                        SELECT-FIELDS6(WS-LEN-OB:1)                       
045300         ADD +1                     TO WS-LEN-OB                          
045400         MOVE '.'                   TO                                    
045500                        SELECT-FIELDS6(WS-LEN-OB:1)                       
045600         ADD +1                     TO WS-LEN-OB                          
045700         MOVE REQU-IDELMT-OB(WS-IX) TO                                    
045800                        SELECT-FIELDS6(WS-LEN-OB:15)                      
045900         ADD +15                    TO WS-LEN-OB                          
046000                                                                          
046100         IF REQU-KDREPLVL = 'S'                                           
046200           MOVE WS-TECORREL           TO                                  
046300                          SELECT-FIELDS7(WS-LEN-GB:1)                     
046400           ADD +1                     TO WS-LEN-GB                        
046500           MOVE '.'                   TO                                  
046600                          SELECT-FIELDS7(WS-LEN-GB:1)                     
046700           ADD +1                     TO WS-LEN-GB                        
046800           MOVE REQU-IDELMT-OB(WS-IX) TO                                  
046900                          SELECT-FIELDS7(WS-LEN-GB:15)                    
047000           ADD +15                    TO WS-LEN-GB                        
047100         END-IF                                                           
047200       ELSE                                                               
047300         CONTINUE                                                         
047400       END-IF                                                             
047500       ADD +1 TO WS-IX                                                    
047600     END-PERFORM                                                          
047700                                                                          
047800     MOVE +1 TO WS-IX                                                     
047900                                                                          
048000     PERFORM UNTIL WS-IX > WS-MAX-LINES                                   
048100       IF REQU-TESELCOL(WS-IX) > SPACE                                    
048200         IF WS-LEN-S = 1                                                  
048300           MOVE 'SELECT '          TO SELECT-FIELDS1(WS-LEN-S:7)          
048400           ADD +7                  TO WS-LEN-S                            
048500         ELSE                                                             
048600           MOVE ', '                TO SELECT-FIELDS1(WS-LEN-S:2)         
048700           ADD +2                   TO WS-LEN-S                           
048800         END-IF                                                           
048900                                                                          
049000         MOVE REQU-TESELCOL(WS-IX)  TO ITEM-CHECK-3                       
049100         IF ITEM-PPS                                                      
049200           MOVE 'B' TO WS-TECORREL                                        
049300           PERFORM S03-ADD-JOIN                                           
049400         ELSE                                                             
049500           MOVE 'A' TO WS-TECORREL                                        
049600         END-IF                                                           
049700                                                                          
049800         IF ITEM-CORR                                                     
049900           MOVE WS-TECORREL         TO                                    
050000                          SELECT-FIELDS1(WS-LEN-S:1)                      
050100           ADD +1                   TO WS-LEN-S                           
050200           MOVE '.'                 TO                                    
050300                          SELECT-FIELDS1(WS-LEN-S:1)                      
050400           ADD +1                   TO WS-LEN-S                           
050500         END-IF                                                           
050600         MOVE REQU-TESELCOL(WS-IX)  TO                                    
050700                        SELECT-FIELDS1(WS-LEN-S:50)                       
050800         ADD +50                    TO WS-LEN-S                           
050900       ELSE                                                               
051000         CONTINUE                                                         
051100       END-IF                                                             
051200       ADD +1 TO WS-IX                                                    
051300     END-PERFORM                                                          
051400     .                                                                    
051500     EJECT                                                                
051600                                                                          
051700 C-CREATE-WHERE-CLAUSE SECTION.                                           
051800**** GET WHERE CLAUSE                                                     
051900     MOVE SPACE TO SELECT-FIELDS5                                         
052000     MOVE +1 TO WS-IXU                                                    
052100     MOVE +0 TO WS-IXW                                                    
052200     MOVE +1 TO WS-LEN-1                                                  
052300     MOVE +1 TO WS-LEN-2                                                  
052400     PERFORM UNTIL WS-IXU > WS-MAX-LINES                                  
052500     OR REQU-IDELMT-SEARCH(WS-IXU) = SPACE                                
052601     OR KEYS-NO                                                           
052700       IF REQU-TEELMTVAL-SEARCH(WS-IXU) > SPACE                           
052800         MOVE REQU-IDELMT-SEARCH(WS-IXU)    TO VL2S-IDELMT                
052900         MOVE REQU-TEELMTVAL-SEARCH(WS-IXU) TO VL2S-TEELMTVAL             
053000         MOVE REQU-IDELMT-SEARCH(WS-IXU)    TO ITEM-CHECK-3               
053100                                               ITEM-CHECK-2               
053200         IF ITEM-PPS                                                      
053300           MOVE 'B' TO VL2S-TECORREL                                      
053400           PERFORM S03-ADD-JOIN                                           
053500         ELSE                                                             
053600           MOVE 'A' TO VL2S-TECORREL                                      
053700         END-IF                                                           
053801         IF ITEM-ALFA                                                     
053901           MOVE 'AN'  TO VL2S-KDFORMAT                                    
054001         END-IF                                                           
054101         IF ITEM-NUM                                                      
054201           MOVE 'NU'  TO VL2S-KDFORMAT                                    
054301         END-IF                                                           
054401         IF ITEM-DATE6                                                    
054501           MOVE 'D6'  TO VL2S-KDFORMAT                                    
054601         END-IF                                                           
054700         CALL WQ0XVL2S USING VL2S-WQ0XVL2S                                
054800         IF VL2S-KDSVAR = SPACE                                           
054900           ADD +1 TO WS-IXW                                               
055000           PERFORM CA-PERFORM-WHERE-UNIQUE                                
055100         ELSE                                                             
055200           MOVE NOO TO KEYS-SW                                            
055300           MOVE ERROR-FROM-WQ0XVL2S TO WS-IDMSG-ERROR                     
055400           MOVE REQU-IDELMT-SEARCH(WS-IXU) TO WS-IDELMT-ERROR             
055500         END-IF                                                           
055600       END-IF                                                             
055700       ADD  +1 TO WS-IXU                                                  
055800     END-PERFORM                                                          
055900     .                                                                    
056000     EJECT                                                                
056100                                                                          
056200 CA-PERFORM-WHERE-UNIQUE SECTION.                                         
056300     IF WS-IXW = +1                                                       
056400       MOVE 'WHERE '        TO SELECT-FIELDS5(WS-LEN-1:6)                 
056500       ADD +6               TO WS-LEN-1                                   
056600       MOVE VL2S-WHERE-COND TO SELECT-FIELDS5(WS-LEN-1:1000)              
056700       ADD +1000            TO WS-LEN-1                                   
056800     ELSE                                                                 
056900       MOVE ' AND '         TO SELECT-FIELDS5(WS-LEN-1:5)                 
057000       ADD +5               TO WS-LEN-1                                   
057100       MOVE VL2S-WHERE-COND TO SELECT-FIELDS5(WS-LEN-1:1000)              
057200       ADD +1000            TO WS-LEN-1                                   
057300     END-IF                                                               
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700 D-PERFORM-REQUEST SECTION.                                               
057800     MOVE SPACE             TO STMTBUFF-D                                 
057900                                                                          
058000     IF  SELECT-FIELDS5 = SPACE                                           
058100     AND SELECT-FIELDS10 > SPACE                                          
058200       MOVE SPACE TO SELECT-JOIN                                          
058300       PERFORM S03-ADD-JOIN-WHERE                                         
058400     END-IF                                                               
058500                                                                          
058600     MOVE +1                TO WS-LEN-A                                   
058700     STRING SELECT-FIELDS1  DELIMITED BY SIZE                             
058800            SELECT-FIELDS4  DELIMITED BY SIZE                             
058900            SELECT-FIELDS11 DELIMITED BY SIZE                             
059000            SELECT-FIELDS5  DELIMITED BY SIZE                             
059100            SELECT-FIELDS10 DELIMITED BY SIZE                             
059200            SELECT-FIELDS7  DELIMITED BY SIZE                             
059300            SELECT-FIELDS6  DELIMITED BY SIZE                             
059400            SELECT-FIELDS8  DELIMITED BY SIZE                             
059500            SELECT-FIELDS9  DELIMITED BY SIZE                             
059600     INTO STMTBUFF-D                                                      
059700     WITH POINTER WS-LEN-A                                                
059800     MOVE WS-LEN-A             TO STMTBUFF-L                              
059900                                                                          
060000     PERFORM DB2-OPEN-TQ1TRPUP                                            
060100                                                                          
060200     MOVE +1 TO WS-SQL-IX                                                 
060300     MOVE +0 TO WS-IX8                                                    
060400     MOVE +1 TO WS-IXP                                                    
060500     MOVE 1                           TO WSEGSTART                        
060600     MOVE LENGTH OF RESP-TESSVDATA(1) TO WSEGSTOPP                        
060700     SUBTRACT LENGTH OF VSEL-TECOLVAL FROM WSEGSTOPP                      
060800     MOVE LENGTH OF DOP-LINE-DATA     TO WSEGSTOPP-DOP                    
060900     SUBTRACT LENGTH OF VSEL-TECOLVAL FROM WSEGSTOPP-DOP                  
061000                                                                          
061100     PERFORM UNTIL WS-SQL-IX > 30                                         
061200      SET SQLDATA(WS-SQL-IX) TO ADDRESS OF WS-SQLDATA(WS-SQL-IX)          
061300      SET SQLIND(WS-SQL-IX)  TO ADDRESS OF WS-SQLIND(WS-SQL-IX)           
061400      ADD +1 TO WS-SQL-IX                                                 
061500     END-PERFORM                                                          
061600                                                                          
061700     PERFORM DB2-FETCH-TQ1TRPUP                                           
061800                                                                          
061900     IF REQU-KDQOUT = 'M'                                                 
062000     OR REQU-KDQOUT = 'W'                                                 
062100       PERFORM S90-SEND-OPEN                                              
062200       PERFORM S90-PUT-HEADER                                             
062300       PERFORM DC-COPY-HEADINGS-TO-DOP                                    
062400     END-IF                                                               
062500                                                                          
062600     PERFORM UNTIL LINES-MISSING                                          
062700     OR WS-IX8 > WS-MAX-KVRADER                                           
062800     OR WS-IXP > WS-MAX-PUT                                               
062900       IF REQU-KDQOUT = 'R'                                               
063000         ADD  +1 TO WS-IX8                                                
063100         IF WS-IX8 > WS-MAX-KVRADER                                       
063200           MOVE INFO-MORE-DATA-EXIST TO WS-IDMSG-INFO                     
063300         ELSE                                                             
063400           PERFORM DA-READ-SHOW-INFO-TABELL                               
063500           MOVE WS-IX8 TO RESP-KVRADER                                    
063600         END-IF                                                           
063700       END-IF                                                             
063800       IF REQU-KDQOUT = 'M'                                               
063900       OR REQU-KDQOUT = 'W'                                               
064000         PERFORM DB-READ-INFO-TABELL-TO-DOP                               
064100         ADD  +1 TO WS-IXP                                                
064200       END-IF                                                             
064300       PERFORM DB2-FETCH-TQ1TRPUP                                         
064400     END-PERFORM                                                          
064500                                                                          
064600     PERFORM DB2-CLOSE-TQ1TRPUP                                           
064700                                                                          
064800     IF REQU-KDQOUT = 'M'                                                 
064900     OR REQU-KDQOUT = 'W'                                                 
065000       MOVE ZERO TO RESP-KVRADER                                          
065100       MOVE DATA-SENT-TO-DOP TO WS-IDMSG-INFO                             
065200       IF WS-IXP > WS-MAX-PUT                                             
065300         MOVE DATA-EXCEED-LIMIT     TO WS-IDMSG-ERROR                     
065400         MOVE DATA-EXCEED-LIMIT-TEXT  TO DOP-LINE-DATA                    
065500         MOVE LENGTH OF DATA-EXCEED-LIMIT-TEXT TO WS-KVDLEN               
065600         PERFORM S90-PUT-LINE                                             
065700       END-IF                                                             
065800                                                                          
065900       PERFORM S90-SEND-CLOSE                                             
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300                                                                          
066400 DA-READ-SHOW-INFO-TABELL SECTION.                                        
066500     MOVE +1 TO WS-IX5                                                    
066600     MOVE 1  TO WSEGSTART                                                 
066700     MOVE SPACE  TO RESP-TESSVDATA(WS-IX8)                                
066800     PERFORM UNTIL WS-IX5 > SQLD                                          
066900       MOVE SQLTYPE(WS-IX5) TO VSEL-SQLTYPE                               
067000       MOVE SQLLEN(WS-IX5)  TO VSEL-SQLLEN                                
067100       SET  VSEL-SQLDATA    TO SQLDATA(WS-IX5)                            
067200       SET  VSEL-SQLIND     TO SQLIND(WS-IX5)                             
067300                                                                          
067400       CALL WQ0XVSEL USING VSEL-WQ0XVSEL                                  
067500                                                                          
067600       IF WSEGSTART < WSEGSTOPP                                           
067700         STRING VSEL-TECOLVAL DELIMITED BY '  '                           
067800                '; '          DELIMITED BY SIZE                           
067900         INTO RESP-TESSVDATA(WS-IX8)                                      
068000         WITH POINTER WSEGSTART                                           
068100         ADD +1 TO WS-IX5                                                 
068200       ELSE                                                               
068300         MOVE VALUE-OVERFLOW     TO WS-IDMSG-INFO                         
068400       END-IF                                                             
068500     END-PERFORM                                                          
068600     .                                                                    
068700     EJECT                                                                
068800                                                                          
068900 DB-READ-INFO-TABELL-TO-DOP SECTION.                                      
069000     MOVE +1 TO WS-IX5                                                    
069100     MOVE 1 TO WSEGSTART                                                  
069200     MOVE SPACE TO DOP-LINE-DATA                                          
069300     PERFORM UNTIL WS-IX5 > SQLD                                          
069400       MOVE SQLTYPE(WS-IX5) TO VSEL-SQLTYPE                               
069500       MOVE SQLLEN(WS-IX5)  TO VSEL-SQLLEN                                
069600       SET  VSEL-SQLDATA    TO SQLDATA(WS-IX5)                            
069700       SET  VSEL-SQLIND     TO SQLIND(WS-IX5)                             
069800                                                                          
069900       CALL WQ0XVSEL USING VSEL-WQ0XVSEL                                  
070000                                                                          
070100       IF WSEGSTART < WSEGSTOPP-DOP                                       
070200         STRING VSEL-TECOLVAL DELIMITED BY '  '                           
070300                '; '          DELIMITED BY SIZE                           
070400         INTO DOP-LINE-DATA                                               
070500         WITH POINTER WSEGSTART                                           
070600         ADD +1 TO WS-IX5                                                 
070700       ELSE                                                               
070800         MOVE VALUE-OVERFLOW     TO WS-IDMSG-INFO                         
070900       END-IF                                                             
071000     END-PERFORM                                                          
071100     MOVE WSEGSTART TO WS-KVDLEN                                          
071200                                                                          
071300     PERFORM S90-PUT-LINE                                                 
071400     .                                                                    
071500     EJECT                                                                
071600                                                                          
071700 DC-COPY-HEADINGS-TO-DOP SECTION.                                         
071800     MOVE 1 TO WS-IX5                                                     
071900     PERFORM UNTIL WS-IX5 > WS-MAX-SEARCHCRIT                             
072000     OR REQU-TEELMTVAL-SCRIT(WS-IX5)(1:10) = SPACE                        
072100                                                                          
072200       MOVE 0 TO WSEGSTART                                                
072300       INSPECT REQU-TEELMTVAL-SCRIT(WS-IX5)                               
072400         TALLYING WSEGSTART FOR CHARACTERS BEFORE '      '                
072500       MOVE REQU-TEELMTVAL-SCRIT(WS-IX5)(1:WSEGSTART)                     
072600         TO DOP-LINE-DATA                                                 
072700       MOVE WSEGSTART TO WS-KVDLEN                                        
072800       PERFORM S90-PUT-LINE                                               
072900                                                                          
073000       ADD 1 TO WS-IX5                                                    
073100     END-PERFORM                                                          
073200                                                                          
073300     MOVE REQU-TESSVDATA-HEADERS TO DOP-LINE-DATA                         
073400     MOVE LENGTH OF REQU-TESSVDATA-HEADERS TO WS-KVDLEN                   
073500     PERFORM S90-PUT-LINE                                                 
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900 E-READ-SHOW-INFO SECTION.                                                
074000     MOVE REQU-IDMSGVER            TO RESP-IDMSGVER                       
074100     MOVE WS-IDMSG-INFO            TO RESP-IDMSG-INFO                     
074200     MOVE WS-IDMSG-ERROR           TO RESP-IDMSG-ERROR                    
074300     MOVE WS-IDELMT-ERROR          TO RESP-IDELMT-ERROR                   
074400     .                                                                    
074500     EJECT                                                                
074600                                                                          
074700*    --- DISPATCHER-SECTIONS                                              
074800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
074900     MOVE 'GETARG'                 TO SUB-KDFUNC                          
075000     MOVE 'CARPARTS.TRPUP.REPORT'  TO SUB-ADDISPABS                       
075100     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
075200                                                                          
075300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
075400                                                                          
075500     IF SUB-KDRC > 0                                                      
075600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
075700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
075800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
075900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
076000     END-IF                                                               
076100     .                                                                    
076200     SKIP3                                                                
076300                                                                          
076400 S02-RETURN-RESPONSE SECTION.                                             
076500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
076600                                                                          
076700     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
076800                                                                          
076900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
077000                                                                          
077100     IF SUB-KDRC > 0                                                      
077200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
077300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
077400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
077500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
077600     END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900                                                                          
078000 S03-ADD-JOIN SECTION.                                                    
078100     MOVE 'AND A.TILASTN  = B.TILASTN    ' TO SELECT-JOIN(1:30)           
078200     MOVE 'AND A.IDPRODNR = B.IDPRODNR   ' TO SELECT-JOIN(31:30)          
078300     MOVE 'AND A.IDKOLLI  = B.IDKOLLI    ' TO SELECT-JOIN(61:30)          
078400                                                                          
078500     MOVE ', '           TO SELECT-FIELDS11(1:2)                          
078600     MOVE SELECT-OWNER   TO SELECT-FIELDS11(3:5)                          
078700     MOVE '.TQ1TRPPS B ' TO SELECT-FIELDS11(8:12)                         
078800     .                                                                    
078900     EJECT                                                                
079000                                                                          
079100 S03-ADD-JOIN-WHERE SECTION.                                              
079200     MOVE 'WHERE A.TILASTN  = B.TILASTN  ' TO SELECT-JOIN(1:30)           
079300     MOVE 'AND A.IDPRODNR = B.IDPRODNR   ' TO SELECT-JOIN(31:30)          
079400     MOVE 'AND A.IDKOLLI  = B.IDKOLLI    ' TO SELECT-JOIN(61:30)          
079500                                                                          
079600     MOVE ', '           TO SELECT-FIELDS11(1:2)                          
079700     MOVE SELECT-OWNER   TO SELECT-FIELDS11(3:5)                          
079800     MOVE '.TQ1TRPPS B ' TO SELECT-FIELDS11(8:12)                         
079900     .                                                                    
080000     EJECT                                                                
080100                                                                          
080200 S90-SEND-OPEN SECTION.                                                   
080300     MOVE 'CARPARTS.DAP.DISTRDOC'    TO SEND-ADDISPABS                    
080400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
080500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
080600                         SEND-OPEN-AREA                                   
080700     IF SEND-KDRC > ZERO                                                  
080800       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
080900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
081000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
081100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
081200     END-IF                                                               
081300     .                                                                    
081400     EJECT                                                                
081500                                                                          
081600 S90-PUT-HEADER SECTION.                                                  
081700     MOVE 001                        TO DOP-REQU-IDMSGVER                 
081800     MOVE 'R'                        TO DOP-REQU-KDPGMACT                 
081900     MOVE IDPGM                      TO DOP-REQU-IDUSER                   
082000       MOVE SPACE                    TO HDR-IDOUTREC                      
082100                                                                          
082200     IF REQU-KDQOUT = 'M'                                                 
082300       MOVE 'TRPFOLLOWUPM'           TO HDR-IDOUTTYPE                     
082400       MOVE REQU-IDMAIL(1:30)        TO HDR-IDOUTREC                      
082500     END-IF                                                               
082600                                                                          
082700     IF REQU-KDQOUT = 'W'                                                 
082800       MOVE 'TRPFOLLOWUPW'           TO HDR-IDOUTTYPE                     
082900       MOVE REQU-IDUSER              TO HDR-IDOUTREC                      
083000     END-IF                                                               
083100                                                                          
083200     MOVE FUNCTION CURRENT-DATE(3:10) TO HDR-IDLIST                       
083300     MOVE 'PUT'                      TO SEND-KDFUNC                       
083400     MOVE LENGTH OF HDR-AREA         TO SEND-KVDLEN                       
083500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
083600                         SEND-KVDLEN                                      
083700                         HDR-AREA                                         
083800     IF SEND-KDRC > ZERO                                                  
083900       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
084000       STRING 'WZ01SEND PUT ERROR RC='  KDRC-DISPLAY                      
084100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
084200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
084300     END-IF                                                               
084400     .                                                                    
084500     EJECT                                                                
084600                                                                          
084700 S90-PUT-LINE SECTION.                                                    
084800     MOVE 'PUT'                      TO SEND-KDFUNC                       
084900     MOVE WS-KVDLEN                  TO SEND-KVDLEN                       
085000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
085100                         SEND-KVDLEN                                      
085200                         DOP-LINE-AREA                                    
085300     IF SEND-KDRC > ZERO                                                  
085400       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
085500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
085600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
085700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
085800     END-IF                                                               
085900     .                                                                    
086000     EJECT                                                                
086100                                                                          
086200 S90-SEND-CLOSE SECTION.                                                  
086300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
086400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
086500     .                                                                    
086600     EJECT                                                                
086700                                                                          
086800 DB2-OPEN-TQ1TRPUP    SECTION.                                            
086900     MOVE 000100 TO GOOD-SQLCODECODES                                     
087000     EXEC SQL                                                             
087100         DECLARE STMT STATEMENT                                           
087200     END-EXEC                                                             
087300     MOVE SQLCODE TO SQLCODE-WS                                           
087400     PERFORM DB2-STATUS-CHECK                                             
087500                                                                          
087600     MOVE 000100  TO GOOD-SQLCODECODES                                    
087700     EXEC SQL                                                             
087800         PREPARE STMT FROM :STMTBUFF                                      
087900     END-EXEC                                                             
088000     MOVE SQLCODE TO SQLCODE-WS                                           
088100     PERFORM DB2-STATUS-CHECK                                             
088200                                                                          
088300     MOVE 000100  TO GOOD-SQLCODECODES                                    
088400     EXEC SQL                                                             
088500         DESCRIBE STMT INTO :SQLDA                                        
088600     END-EXEC                                                             
088700     MOVE SQLCODE TO SQLCODE-WS                                           
088800     PERFORM DB2-STATUS-CHECK                                             
088900                                                                          
089000     MOVE 000100  TO GOOD-SQLCODECODES                                    
089100     EXEC SQL                                                             
089200         DECLARE TQ1TRPUP-CRS CURSOR FOR STMT                             
089300     END-EXEC                                                             
089400     MOVE SQLCODE TO SQLCODE-WS                                           
089500     PERFORM DB2-STATUS-CHECK                                             
089600                                                                          
089700     MOVE 000100  TO GOOD-SQLCODECODES                                    
089800     EXEC SQL                                                             
089900         OPEN TQ1TRPUP-CRS                                                
090000     END-EXEC                                                             
090100     MOVE SQLCODE TO SQLCODE-WS                                           
090200     PERFORM DB2-STATUS-CHECK                                             
090300     .                                                                    
090400     EJECT                                                                
090500                                                                          
090600 DB2-FETCH-TQ1TRPUP    SECTION.                                           
090700     MOVE 000100  TO GOOD-SQLCODECODES                                    
090800     EXEC SQL                                                             
090900         FETCH TQ1TRPUP-CRS USING DESCRIPTOR :SQLDA                       
091000     END-EXEC                                                             
091100     MOVE SQLCODE TO SQLCODE-WS                                           
091200     PERFORM DB2-STATUS-CHECK                                             
091300     .                                                                    
091400     EJECT                                                                
091500                                                                          
091600 DB2-CLOSE-TQ1TRPUP    SECTION.                                           
091700     EXEC SQL                                                             
091800        CLOSE TQ1TRPUP-CRS                                                
091900     END-EXEC                                                             
092000     .                                                                    
092100     EJECT                                                                
092200                                                                          
092300 DB2-STATUS-CHECK     SECTION.                                            
092400     SET SQLCODE-IX TO 1                                                  
092500     SEARCH GOOD-SQLCODE                                                  
092600       AT END                                                             
092700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
092800          DELIMITED BY SIZE INTO ERROR-TEXT                               
092900          CALL ABEND USING RKOD-ABEND-DB2                                 
093000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
093100     END-SEARCH                                                           
094000     .                                                                    
