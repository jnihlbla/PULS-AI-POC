000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3039100.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   02/01/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BACKGRUNDS MPP FÖR ATT SKICKA IVÄG PRISFRÅGOR TILL WZ1.          
000900*        UPPDATERAR WDC7 MED INFO OM IVÄGSKICKNING.                       
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDC7                                       
001200*        TRANSACTIONER SKICKAS MED HJÄLP AV WZ01 M0ODULEN                 
001300*                                                                         
001400*    ÄNDRING: 041207 E'TRACKER NO. 1574010                                
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W30391X                                             
001800*        MID:         W30391I1 (VIA WZ01)                                 
001900*                                                                         
002000*    UTDATA.                                                              
002100*        SÄNDNING VIA WZ01                                                
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W3039100'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  W-FIRST-TIME                PIC 9       VALUE ZERO.                  
003700                                                                          
003800                                                                          
003900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004000     88  NYCKLAR-OK                          VALUE 'J'.                   
004100     88  NYCKLAR-FEL                         VALUE 'N'.                   
004200     EJECT                                                                
004300 77  SINGLE-STATUS-WS            PIC X       VALUE 'N'.                   
004400     88  SINGLE-POST                         VALUE 'J'.                   
004500     88  BUNT-POST                           VALUE 'N'.                   
004600     EJECT                                                                
004700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
004800 01  GENERELLA-SUBPROGRAM.                                                
004900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005200     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
005300     EJECT                                                                
005400 01  MESSAGE-CODES.                                                       
005500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005600     EJECT                                                                
005700*    --- AREOR FÖR KOMMUNIKATION                                          
005800 01  FILLER                      PIC X(16)   VALUE 'RECEIVE-AREA'.        
005900*01  -COPY WZ01RECV                                                       
006000 01  RECV-DATA.                                                           
006100*03  -COPY  WZ01REQU  -PRE MID-                                           
006200*03  -COPY  W30391I1                                                      
006300     EJECT                                                                
006400 01  FILLER                      PIC X(16)   VALUE 'SENDING-AREA'.        
006500*01  -COPY WZ01SEND                                                       
006600 01  SEND-DATA.                                                           
006700*03  -COPY  WZ01REQU -PRE MOD-                                            
006800*03  -COPY W30391O1                                                       
006900     EJECT                                                                
007000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007300     SKIP3                                                                
007400 01  NYCKLAR-TILL-DLI.                                                    
007500     03  WDR401-X.                                                        
007600         05  FILLER              PIC X(4)    VALUE '3101'.                
007700         05  W-IDDISTR1          PIC 9(4)    VALUE ZERO.                  
007800         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
007900     03  W-WDC701KY-X.                                                    
008000         05  W-IDDISTR           PIC 9(4)     VALUE ZERO.                 
008100         05  W-IDKUNDNR          PIC 9(7)     VALUE ZERO.                 
008200         05  W-IDBUNDLE          PIC X(15)    VALUE SPACE.                
008300     03  W-WDC711KY-X.                                                    
008400         05  W-IDPRQUES          PIC 9(7)     VALUE ZERO.                 
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     SKIP2                                                                
009200 01  GODK-STATUSKODER.                                                    
009300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009400     SKIP3                                                                
009500 01  SSA1                        PIC X(64).                               
009600 01  SSA2                        PIC X(64).                               
009700     EJECT                                                                
009800*    --- IMS FUNKTIONSKODER                                               
009900*01  -COPY W0003                                                          
010000     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010200                                                                          
010300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
010400 01  DLI-IO-WDC701.                                                       
010500*    03  -COPY WDC701                                                     
010600     EJECT                                                                
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
010800 01  DLI-IO-WDC711.                                                       
010900*    03  -COPY WDC711                                                     
011000     EJECT                                                                
011100 01  FILLER         PIC X(16) VALUE 'DLI-WDR4FEL'.                        
011200 01  DLI-IO-WDR401.                                                       
011300*    03  -COPY WDGX3101                                                   
011400 01  DLI-IO-WDGX3102.                                                     
011500*    03  -COPY WDGX3102                                                   
011600 01  DLI-IO-WDGX3104.                                                     
011700*    03  -COPY WDGX3104                                                   
011800 01  DLI-IO-WDGX3106.                                                     
011900*    03  -COPY WDGX3106                                                   
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*    -- ANVÄNDS EJ I DETTA PROGRAMMET                                     
012400 01  IO-PCB                      PIC X.                                   
012500                                                                          
012600*01  -COPY W0008  -PRE WDC7-                                              
012700     05  FILLER                  PIC X.                                   
012800*01  -COPY W0008  -PRE WDR4-                                              
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100 PROCEDURE DIVISION  USING IO-PCB WDC7-PCB WDR4-PCB.                      
013200 MAIN SECTION.                                                            
013300     ENTRY 'DLITCBL' USING IO-PCB WDC7-PCB WDR4-PCB.                      
013400                                                                          
013500     PERFORM S01-RECV-OPEN                                                
013600     PERFORM S02-RECV-MESSAGE                                             
013700     IF RECV-KDRC = ZERO                                                  
013800       PERFORM B-BEHANDLA-RADER                                           
013900       PERFORM IMS-GU-WDC701                                              
014000       PERFORM UNTIL SEGMENT-SAKNAS OR SINGLE-POST                        
014100         PERFORM E-UPPDATERA                                              
014200         IF SEGMENT-FINNS                                                 
014300           IF (PRQ-IDDISTR = 0778 AND LPRQ-ADDISPABS = SPACE) OR          
014310              (PRQ-IDDISTR = 8857 AND LPRQ-ADDISPABS = SPACE) OR          
014400              (PRQ-IDDISTR = 8859 AND LPRQ-ADDISPABS = SPACE)             
014500             CONTINUE                                                     
014600           ELSE                                                           
014700             IF W-FIRST-TIME = 0                                          
014800               MOVE 1 TO W-FIRST-TIME                                     
014900               PERFORM S04-SEND-OPEN                                      
015000             END-IF                                                       
015100                                                                          
015200             PERFORM S05-SEND-MESSAGE                                     
015300           END-IF                                                         
015400         END-IF                                                           
015500       END-PERFORM                                                        
015600                                                                          
015700       IF W-FIRST-TIME = 1                                                
015800         PERFORM S06-SEND-CLOSE                                           
015900       END-IF                                                             
016000     END-IF                                                               
016100     PERFORM S03-RECV-CLOSE                                               
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 B-BEHANDLA-RADER SECTION.                                                
016800                                                                          
016900     MOVE 'J' TO NYCKLAR-SW                                               
017000     MOVE MID-IDDISTR      TO   W-IDDISTR W-IDDISTR1                      
017100     MOVE MID-IDKUNDNR     TO   W-IDKUNDNR                                
017200     MOVE MID-IDBUNDLE     TO   W-IDBUNDLE                                
017300     MOVE MID-IDPRQUES     TO   W-IDPRQUES                                
017400     PERFORM IMS-GU-WDGX3102                                              
017500     MOVE 3102-ADDISPABS-ASYNC TO SEND-ADDISPABS                          
017600                                                                          
017700     MOVE 0                TO W-FIRST-TIME                                
017800     .                                                                    
017900     EJECT                                                                
018000 C-UPPDATERA SECTION.                                                     
018100     CONTINUE                                                             
018200     .                                                                    
018300     EJECT                                                                
018400 D-BEHANDLA-RADER SECTION.                                                
018500     CONTINUE                                                             
018600     .                                                                    
018700     EJECT                                                                
018800 E-UPPDATERA SECTION.                                                     
018900                                                                          
019000     EVALUATE MID-IDPRQUES                                                
019100     WHEN ZERO                                                            
019200       PERFORM IMS-GHNP-WDC711                                            
019300       PERFORM UNTIL LPRQ-DADATTID-SEND = 0 OR SEGMENT-SAKNAS             
019400         PERFORM IMS-GHNP-WDC711                                          
019500       END-PERFORM                                                        
019600                                                                          
019700*****      PUT DATA                                                       
019800       IF SEGMENT-FINNS                                                   
019900         MOVE MID-REQU-WZ01REQU    TO  MOD-REQU-WZ01REQU                  
020000         MOVE PRQ-IDDISTR     TO  MOD-IDDISTR                             
020100         MOVE PRQ-IDKUNDNR    TO  MOD-IDKUNDNR                            
020200         MOVE PRQ-IDBUNDLE    TO  MOD-IDBUNDLE                            
020300         MOVE LPRQ-IDPRQUES   TO  MOD-IDPRQUES                            
020400         MOVE LPRQ-IDARTNR    TO  MOD-IDARTNR                             
020500         MOVE LPRQ-KDORDKL    TO  MOD-KDORDKL                             
020600         MOVE LPRQ-KVBEART    TO  MOD-KVBEART                             
020700***EVENTUELLT UPPDATERA SEND TIDPUNKTEN******************                 
020800         IF (PRQ-IDDISTR = 0778 AND LPRQ-ADDISPABS = SPACE) OR            
020810            (PRQ-IDDISTR = 8857 AND LPRQ-ADDISPABS = SPACE) OR            
020820            (PRQ-IDDISTR = 8859 AND LPRQ-ADDISPABS = SPACE)               
020900           CONTINUE                                                       
021000         ELSE                                                             
021100           MOVE FUNCTION CURRENT-DATE(1:14) TO                            
021200                                        LPRQ-DADATTID-SEND                
021300         END-IF                                                           
021400                                                                          
021500         MOVE 'M' TO LPRQ-KDORDTYP                                        
021600         PERFORM IMS-REPL-WDC711                                          
021700       END-IF                                                             
021800     WHEN OTHER                                                           
021900       PERFORM IMS-GHU-WDC711                                             
022000*****      PUT DATA                                                       
022100       IF SEGMENT-FINNS                                                   
022200         MOVE MID-REQU-WZ01REQU    TO  MOD-REQU-WZ01REQU                  
022300         MOVE PRQ-IDDISTR     TO  MOD-IDDISTR                             
022400         MOVE PRQ-IDKUNDNR    TO  MOD-IDKUNDNR                            
022500         MOVE PRQ-IDBUNDLE    TO  MOD-IDBUNDLE                            
022600         MOVE LPRQ-IDPRQUES   TO  MOD-IDPRQUES                            
022700         MOVE LPRQ-IDARTNR    TO  MOD-IDARTNR                             
022800         MOVE LPRQ-KDORDKL    TO  MOD-KDORDKL                             
022900         MOVE LPRQ-KVBEART    TO  MOD-KVBEART                             
023000***UPPDATERA SEND TIDPUNKTEN*                                             
023100         MOVE FUNCTION CURRENT-DATE(1:14) TO                              
023200                                       LPRQ-DADATTID-SEND                 
023300         MOVE 'S' TO LPRQ-KDORDTYP                                        
023400         PERFORM IMS-REPL-WDC711                                          
023500         MOVE 'J' TO SINGLE-STATUS-WS                                     
023600       END-IF                                                             
023700                                                                          
023800     END-EVALUATE                                                         
023900     .                                                                    
024000     EJECT                                                                
024100 S01-RECV-OPEN SECTION.                                                   
024200                                                                          
024300     MOVE 'OPEN' TO RECV-KDFUNC                                           
024400     MOVE 'CARPARTS.PULS.PRQRY' TO RECV-ADDISPABS                         
024500                                                                          
024600     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
024700                                   RECV-OPEN-AREA                         
024800**FELHANTERING...                                                         
024900     IF RECV-KDRC > 0                                                     
025000      STRING 'UNSUCCESSFUL OPEN'                                          
025100      DELIMITED BY SIZE INTO FELTEXT                                      
025200      DISPLAY FELTEXT                                                     
025300      CALL FELLOG                                                         
025400     END-IF                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 S02-RECV-MESSAGE SECTION.                                                
025800                                                                          
025900     MOVE 'GET' TO RECV-KDFUNC                                            
026000     MOVE LENGTH OF RECV-DATA TO RECV-KVDLEN                              
026100     CALL WZ01RECV USING RECV-CONTROL-AREA                                
026200                         RECV-KVDLEN                                      
026300                         RECV-DATA                                        
026400**FELHANTERING...                                                         
026500     IF RECV-KDRC > 1                                                     
026600       STRING 'UNSUCCESSFUL RECEIVE'                                      
026700       DELIMITED BY SIZE INTO FELTEXT                                     
026800       DISPLAY FELTEXT                                                    
026900       CALL FELLOG                                                        
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 S03-RECV-CLOSE SECTION.                                                  
027400                                                                          
027500     MOVE 'CLOSE' TO RECV-KDFUNC                                          
027600     CALL WZ01RECV USING RECV-CONTROL-AREA                                
027700     .                                                                    
027800     EJECT                                                                
027900 S04-SEND-OPEN SECTION.                                                   
028000                                                                          
028100     MOVE 'OPEN' TO SEND-KDFUNC                                           
028200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
028300                         SEND-OPEN-AREA                                   
028400**FELHANTERING...                                                         
028500     IF SEND-KDRC  > 0                                                    
028600        STRING 'FEL VID ÖPPNING AV SÄNDNING'  W-IDDISTR                   
028700        DELIMITED BY SIZE INTO FELTEXT                                    
028800        DISPLAY FELTEXT                                                   
028900        CALL FELLOG                                                       
029000     END-IF                                                               
029100     .                                                                    
029200     EJECT                                                                
029300 S05-SEND-MESSAGE SECTION.                                                
029400                                                                          
029500     MOVE 'PUT' TO SEND-KDFUNC                                            
029600     MOVE LENGTH OF SEND-DATA TO SEND-KVDLEN                              
029700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029800                                   SEND-KVDLEN                            
029900                                   SEND-DATA                              
030000**FELHANTERING...                                                         
030100     IF SEND-KDRC  > 0                                                    
030200        STRING 'FEL VID SÄNDNING AV RAD'  W-IDDISTR                       
030300        DELIMITED BY SIZE INTO FELTEXT                                    
030400        DISPLAY FELTEXT                                                   
030500        CALL FELLOG                                                       
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 S06-SEND-CLOSE SECTION.                                                  
031000                                                                          
031100       MOVE 'CLOSE' TO SEND-KDFUNC                                        
031200       CALL WZ01SEND USING SEND-CONTROL-AREA                              
031300     .                                                                    
031400     EJECT                                                                
031500* --- IMS SEKTIONER ---                                                   
031600     SKIP3                                                                
031700 IMS-GU-WDC701 SECTION.                                                   
031800                                                                          
031900     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
032000          DELIMITED BY SIZE INTO SSA1                                     
032100     MOVE '  GE' TO GODK-STATUSKODER                                      
032200     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-WDC701 SSA1                    
032300     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032600     SKIP3                                                                
032700 IMS-GHU-WDC711 SECTION.                                                  
032800                                                                          
032900     STRING 'WDC711  (IDPRQUES =' W-WDC711KY-X ')'                        
033000          DELIMITED BY SIZE INTO SSA1                                     
033100     MOVE '  GE' TO GODK-STATUSKODER                                      
033200     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC711 SSA1                   
033300     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
033400     PERFORM IMS-STATUSKONTROLL                                           
033500     .                                                                    
033600     SKIP3                                                                
033700 IMS-GHNP-WDC711 SECTION.                                                 
033800                                                                          
033900     MOVE 'WDC711  ' TO SSA1                                              
034000     MOVE '  GE' TO GODK-STATUSKODER                                      
034100     CALL CBLTDLI USING GHNP WDC7-PCB DLI-IO-WDC711 SSA1                  
034200     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
034300     PERFORM IMS-STATUSKONTROLL                                           
034400     .                                                                    
034500     SKIP3                                                                
034600 IMS-REPL-WDC711 SECTION.                                                 
034700                                                                          
034800     MOVE '  ' TO GODK-STATUSKODER                                        
034900     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
035000     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
035100     PERFORM IMS-STATUSKONTROLL                                           
035200     .                                                                    
035300     SKIP3                                                                
035400 IMS-GU-WDGX3102 SECTION.                                                 
035500                                                                          
035600     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
035700          DELIMITED BY SIZE INTO SSA1                                     
035800     MOVE   'WDGX3102' TO SSA2                                            
035900     MOVE '  ' TO GODK-STATUSKODER                                        
036000     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-WDGX3102                      
036100                                     SSA1 SSA2                            
036200     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
036300     PERFORM IMS-STATUSKONTROLL                                           
036400     .                                                                    
036500     SKIP3                                                                
036600 IMS-STATUSKONTROLL SECTION.                                              
036700                                                                          
036800     SET STATUS-IX TO 1                                                   
036900     SEARCH GODK-STATUS                                                   
037000       AT END                                                             
037100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037200         DELIMITED BY SIZE INTO FELTEXT                                   
037300         CALL FELLOG                                                      
037400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037500         CONTINUE                                                         
037600     END-SEARCH                                                           
037700     .                                                                    
