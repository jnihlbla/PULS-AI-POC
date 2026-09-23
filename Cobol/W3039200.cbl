000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3039200.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   02/01/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BACKGRUNDS MPP FÖR ATT TA EMOT     PRISSVAR  FRÅN  WZ1.          
000900*        UPPDATERAR WDC7 MED PRISER .OCH  SÄNDER MEDDELANDE TILL          
001000*        DET URSPRUNGLIGA FRÅGANDE PROGRAM.                               
001100*        OM ETT FELKOD ERHÅLLS, SKICKAS ETT MEDDELANDE TILL W33594        
001200*        SOM SKA MEDDELA PRISSÄTTARE ATT PRISSAKNAS I VIPS.               
001300*        PROGRAMMET UPPDATERAR WDC7                                       
001400*        TRANSACTIONER TAS EMOT MED HJÄLP AV WZ01 M0ODULEN                
001500*    INDATA.                                                              
001600*        TRANSAKTION: W30392X                                             
001700*        MID:         W30392I1 (FRÅN VIPS VIA WZ01)                       
001800*                                                                         
001900*    UTDATA.                                                              
002000*        SÄNDNING VIA WZ01                                                
002100         MOD:         W30392O1 (VIA WZ01) TILL FLERA OLIKA MOTTAG.        
002200                      W40798I1 (MOD-MID)                                  
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W3039200'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700                                                                          
003800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003900     88  NYCKLAR-OK                          VALUE 'J'.                   
004000     88  NYCKLAR-FEL                         VALUE 'N'.                   
004100     EJECT                                                                
004200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
004300 01  GENERELLA-SUBPROGRAM.                                                
004400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
004700     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
004800     EJECT                                                                
004900 01  MESSAGE-CODES.                                                       
005000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005100     EJECT                                                                
005200 01  COUNTERS.                                                            
005300     03  TALLY-OK                PIC 9(4)    VALUE ZERO.                  
005400     03  TALLY-FEL               PIC 9(4)    VALUE ZERO.                  
005500     03  TALLY-TRANS             PIC 9(4)    VALUE ZERO.                  
005600     03  TALLY-TRANS-2           PIC 9(4)    VALUE ZERO.                  
005700*    --- AREOR FÖR KOMMUNIKATION                                          
005800*    --- AREOR FÖR KOMMUNIKATION                                          
005900 01  FILLER                      PIC X(16)   VALUE 'RECEIVE-AREA'.        
006000*01  -COPY WZ01RECV                                                       
006100     SKIP3                                                                
006200 01  RECV-DATA.                                                           
006300*03  -COPY WZ01RESP  -PRE MID-                                            
006400*03  -COPY  W30392I1                                                      
006500     EJECT                                                                
006600 01  FILLER                      PIC X(16)   VALUE 'SENDING-AREA'.        
006700*01  -COPY WZ01SEND                                                       
006800 01  SEND-DATA.                                                           
006900*03  -COPY WZ01RESP  -PRE MOD-                                            
007000 03  MOD-MID-DATA    PIC X(32).                                           
007100*03  -COPY W40798I1  -PRE MOD- -RED MOD-MID-DATA                          
007200*03  -COPY W40290I1  -PRE MOD4290- -RED MOD-MID-DATA                      
007300*03  -COPY W40291I1  -PRE MOD4291- -RED MOD-MID-DATA                      
007400 01   KDRC-DISPLAY               PIC X(9).                                
007500*****SEND DATA FÖR EJ PRISSATTA ARTIKLAR TILL W30393PGMET                 
007600 01  FILLER                      PIC X(16)   VALUE 'SENDING-AREB'.        
007700*01  -COPY WZ01SEND -PRE FEL-                                             
007800 01  SEND-DATA-FEL.                                                       
007900*03  -COPY WZ01RESP  -PRE MOD2-                                           
008000*03  -COPY W30393I1 -PRE  MOD2-                                           
008100                                                                          
008200     EJECT                                                                
008300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008600     SKIP3                                                                
008700 01  NYCKLAR-TILL-DLI.                                                    
008800     03  W-WDC701KY-X.                                                    
008900         05  W-IDDISTR           PIC 9(4)     VALUE ZERO.                 
009000         05  W-IDKUNDNR          PIC 9(7)     VALUE ZERO.                 
009100         05  W-IDBUNDLE          PIC X(15)    VALUE SPACE.                
009200     03  W-WDC711KY-X.                                                    
009300         05  W-IDPRQUES          PIC 9(7)     VALUE ZERO.                 
009400     03  W-PRFEL1KY-X.                                                    
009500         05  W1-IDDISTR          PIC 9(4)     VALUE ZERO.                 
009600         05  W1-IDARTNR          PIC 9(9)     VALUE ZERO.                 
009700     SKIP2                                                                
009800*    --- STATUS-KOD FRÅN IMS                                              
009900 01  STATUS-WS                   PIC XX.                                  
010000     88  SEGMENT-FINNS                       VALUE '  '.                  
010100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400                                                                          
011500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
011600 01  DLI-IO-WDC701.                                                       
011700*    03  -COPY WDC701                                                     
011800     EJECT                                                                
011900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
012000 01  DLI-IO-WDC711.                                                       
012100*    03  -COPY WDC711                                                     
012200     EJECT                                                                
012300 01  FILLER         PIC X(16) VALUE 'DLI-IO-PRFEL1'.                      
012400 01  DLI-IO-PRFEL1.                                                       
012500*    03  -COPY WDC201                                                     
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800*    -- ANVÄNDS EJ I DETTA PROGRAMMET                                     
012900 01  IO-PCB                      PIC X.                                   
013000*01  -COPY W0009  -PRE CRE-                                               
013100     EJECT                                                                
013200*01  -COPY W0009  -PRE PRO-                                               
013300     EJECT                                                                
013400*01  -COPY W0009  -PRE LTD-                                               
013500     EJECT                                                                
013600*01  -COPY W0009  -PRE PRMAIL-                                            
013700     EJECT                                                                
013800*01  -COPY W0008  -PRE WDC7-                                              
013900     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100 PROCEDURE DIVISION  USING IO-PCB CRE-PCB  PRO-PCB                        
014200                               LTD-PCB PRMAIL-PCB WDC7-PCB.               
014300 MAIN SECTION.                                                            
014400     ENTRY 'DLITCBL' USING IO-PCB CRE-PCB  PRO-PCB                        
014500                                  LTD-PCB PRMAIL-PCB WDC7-PCB.            
014600                                                                          
014700     PERFORM S01-RECV-OPEN                                                
014800     PERFORM S02-RECV-MESSAGE                                             
014900     PERFORM UNTIL RECV-KDRC > ZERO                                       
015000     PERFORM A-INIT                                                       
015100       PERFORM C-UPPDATERA                                                
015200       PERFORM S02-RECV-MESSAGE                                           
015300     END-PERFORM                                                          
015400     IF TALLY-OK > 0                                                      
015500*FIX FÖR MAX ANT OPEN PUT CLOSE TRANS < 13                                
015600      COMPUTE TALLY-TRANS = TALLY-TRANS + 1                               
015700      IF TALLY-TRANS < 13                                                 
015800       PERFORM S04-SEND-OPEN                                              
015900       PERFORM S05-SEND-MESSAGE                                           
016000       PERFORM S06-SEND-CLOSE                                             
016100      END-IF                                                              
016200     END-IF                                                               
016300     PERFORM S03-RECV-CLOSE                                               
016400                                                                          
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     .                                                                    
016800     EJECT                                                                
016900                                                                          
017000 A-INIT SECTION.                                                          
017100***** HÄMTA  NYCKELINFO FRÅN MIDEN                                        
017200     MOVE MID-IDDISTR  TO  W-IDDISTR      MOD-MID-IDDISTR                 
017300     MOVE MID-IDKUNDNR TO  W-IDKUNDNR     MOD-MID-IDKUNDNR                
017400     MOVE MID-IDBUNDLE TO  W-IDBUNDLE     MOD-MID-IDBUNDLE                
017500     INSPECT MID-IDPRQUES REPLACING LEADING SPACE BY ZEROS                
017600     MOVE MID-IDPRQUES TO  W-IDPRQUES                                     
017700     .                                                                    
017800     EJECT                                                                
017900                                                                          
018000 C-UPPDATERA SECTION.                                                     
018100                                                                          
018200     PERFORM IMS-GHU-WDC711                                               
018300**** FIX IN TEST TO STOP ABEND WHEN REPLY FROM VIPS CANNOT MATCH          
018400     IF SEGMENT-FINNS                                                     
018500**** END OF FIX  TO STOP ABEND WHEN REPLY FROM VIPS CANNOT MATCH          
018600**** POST ON WDC7.                                                        
018700****  CHECK IF SINGLE QUERY*****************                              
018800     IF LPRQ-KDORDTYP = 'S'                                               
018900       MOVE MID-IDPRQUES     TO  MOD-MID-IDPRQUES                         
019000     ELSE                                                                 
019100       MOVE ZERO TO              MOD-MID-IDPRQUES                         
019200     END-IF                                                               
019300****   PREPARE UPDATE OF DATABASE**********************                   
019400****   MED RECV TIDPUNKTEN******************                              
019500     MOVE FUNCTION CURRENT-DATE(1:14) TO LPRQ-DADATTID-SVAR               
019600***CHECK RETURN CODE *****FELRUTINEN********                              
019700*        P =  PRICE MISSING, BUT PART IN VIPS                             
019800*        V =  PART MISSING IN VIPS                                        
019900*        A =  PRICING DONE OK                                             
020000********************************************                              
020100     IF MID-KDFEL = ZERO                                                  
020200              MOVE 'A'                TO  LPRQ-KDPRSTA                    
020300              MOVE LPRQ-DADATTID-SVAR TO  LPRQ-DADATTID-OK                
020400              MOVE MID-PRARTBTO-LOC   TO  LPRQ-PRARTBTO-LOC               
020500              MOVE MID-PRARTNTO-LOC   TO  LPRQ-PRARTNTO-LOC               
020600              MOVE MID-KDRAB          TO  LPRQ-KDRAB                      
020700              MOVE MID-REARTRAB       TO  LPRQ-REARTRAB                   
020800              MOVE MID-BEART-VIPS     TO  LPRQ-BEART-VIPS                 
020900*FIX FÖR ATT TA HAND OM GROSS<NET PRISER*                                 
021000              IF LPRQ-PRARTNTO-LOC > LPRQ-PRARTBTO-LOC                    
021100               MOVE LPRQ-PRARTNTO-LOC TO LPRQ-PRARTBTO-LOC                
021200               MOVE ZERO TO LPRQ-REARTRAB                                 
021300              END-IF                                                      
021400*END-FIX                                                                  
021500              MOVE 1 TO TALLY-OK                                          
021600              IF LPRQ-ADDISPABS = SPACE                                   
021700               MOVE ZERO TO TALLY-OK                                      
021800              END-IF                                                      
021900     ELSE                                                                 
022000              MOVE 1 TO TALLY-FEL                                         
022100              MOVE MID-KDFEL          TO MOD2-MID-KDFEL                   
022200              MOVE MID-IDDISTR        TO MOD2-MID-IDDISTR                 
022300              MOVE MID-IDKUNDNR       TO MOD2-MID-IDKUNDNR                
022400              MOVE MID-IDARTNR        TO MOD2-MID-IDARTNR                 
022500              PERFORM CA-PRISFEL                                          
022600     END-IF                                                               
022700     MOVE MID-KDFEL                   TO  LPRQ-KDFEL                      
022800     IF MID-KDFEL > 0                                                     
022900       MOVE 'P'                       TO  LPRQ-KDPRSTA                    
023000       MOVE MID-BEART-VIPS            TO  LPRQ-BEART-VIPS                 
023100     END-IF                                                               
023200     IF MID-KDFEL > 1                                                     
023300       MOVE 'V'                         TO  LPRQ-KDPRSTA                  
023400     END-IF                                                               
023500     MOVE MID-KDVAT                     TO  LPRQ-KDVAT                    
023600     MOVE MID-KDVALISO                  TO  LPRQ-KDVALISO                 
023700     MOVE MID-KDFEL                     TO  LPRQ-KDFEL                    
023800     PERFORM IMS-REPL-WDC711                                              
023900****FIX END-IF START                                                      
024000     END-IF                                                               
024100****FIX END-IF END                                                        
024200     .                                                                    
024300     EJECT                                                                
024400 CA-PRISFEL SECTION.                                                      
024500     COMPUTE TALLY-TRANS-2 = TALLY-TRANS-2 + 1                            
024510     IF TALLY-TRANS-2 < 100                                               
024600       PERFORM S07-SEND-OPEN                                              
024700       PERFORM S08-SEND-MESSAGE                                           
024800       PERFORM S09-SEND-CLOSE                                             
024810     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 S01-RECV-OPEN SECTION.                                                   
025200     MOVE 'OPEN' TO RECV-KDFUNC                                           
025300     MOVE 'CARPARTS.PULS.PRANSW' TO RECV-ADDISPABS                        
025400                                                                          
025500     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
025600                                   RECV-OPEN-AREA                         
025700********              ...FELHANTERING...                                  
025800     IF RECV-KDRC > 0                                                     
025900      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
026000      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                      
026100      DELIMITED BY SIZE INTO FELTEXT                                      
026200      CALL FELLOG                                                         
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 S02-RECV-MESSAGE SECTION.                                                
026700                                                                          
026800     MOVE 'GET' TO RECV-KDFUNC                                            
026900     MOVE LENGTH OF RECV-DATA TO RECV-KVDLEN                              
027000     CALL WZ01RECV USING RECV-CONTROL-AREA                                
027100                         RECV-KVDLEN                                      
027200                         RECV-DATA                                        
027300**FELHANTERING...                                                         
027400     IF RECV-KDRC > 1                                                     
027500       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
027600       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
027700       DELIMITED BY SIZE INTO FELTEXT                                     
027800       CALL FELLOG                                                        
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 S03-RECV-CLOSE SECTION.                                                  
028300                                                                          
028400     MOVE 'CLOSE' TO RECV-KDFUNC                                          
028500     CALL WZ01RECV USING RECV-CONTROL-AREA                                
028600**FELHANTERING...                                                         
028700     IF RECV-KDRC > 0                                                     
028800      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
028900      STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                     
029000       DELIMITED BY SIZE INTO FELTEXT                                     
029100       CALL FELLOG                                                        
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
029500 S04-SEND-OPEN SECTION.                                                   
029600     MOVE 'OPEN' TO SEND-KDFUNC                                           
029700     MOVE LPRQ-ADDISPABS TO SEND-ADDISPABS                                
029800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029900                         SEND-OPEN-AREA                                   
030000******** OM FEL                                                           
030100     IF SEND-KDRC  > 0                                                    
030200        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
030300        STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                    
030400        DELIMITED BY SIZE INTO FELTEXT                                    
030500        CALL FELLOG                                                       
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 S05-SEND-MESSAGE SECTION.                                                
031000                                                                          
031100     MOVE 'PUT' TO SEND-KDFUNC                                            
031200     MOVE LENGTH OF SEND-DATA TO SEND-KVDLEN                              
031300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031400                         SEND-KVDLEN                                      
031500                         SEND-DATA                                        
031600******** OM FEL                                                           
031700     IF SEND-KDRC  > 0                                                    
031800        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
031900        STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                    
032000        DELIMITED BY SIZE INTO FELTEXT                                    
032100        CALL FELLOG                                                       
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 S06-SEND-CLOSE SECTION.                                                  
032600                                                                          
032700       MOVE 'CLOSE' TO SEND-KDFUNC                                        
032800       CALL WZ01SEND USING SEND-CONTROL-AREA                              
032900******** OM FEL                                                           
033000     IF SEND-KDRC  > 0                                                    
033100        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
033200        STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                   
033300        DELIMITED BY SIZE INTO FELTEXT                                    
033400        CALL FELLOG                                                       
033500     END-IF                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 S07-SEND-OPEN SECTION.                                                   
033900     MOVE 'OPEN' TO FEL-SEND-KDFUNC                                       
034000     MOVE 'CARPARTS.PULS.PRMAIL' TO FEL-SEND-ADDISPABS                    
034100     CALL WZ01SEND USING FEL-SEND-CONTROL-AREA                            
034200                         FEL-SEND-OPEN-AREA                               
034300******** OM FEL                                                           
034400     IF FEL-SEND-KDRC  > 0                                                
034500        MOVE FEL-SEND-KDRC TO KDRC-DISPLAY                                
034600        STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                    
034700        DELIMITED BY SIZE INTO FELTEXT                                    
034800        CALL FELLOG                                                       
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 S08-SEND-MESSAGE SECTION.                                                
035300                                                                          
035400     MOVE 'PUT' TO FEL-SEND-KDFUNC                                        
035500     MOVE LENGTH OF SEND-DATA-FEL TO FEL-SEND-KVDLEN                      
035600     CALL WZ01SEND USING FEL-SEND-CONTROL-AREA                            
035700                         FEL-SEND-KVDLEN                                  
035800                             SEND-DATA-FEL                                
035900******** OM FEL                                                           
036000     IF FEL-SEND-KDRC  > 0                                                
036100        MOVE FEL-SEND-KDRC TO KDRC-DISPLAY                                
036200        STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                    
036300        DELIMITED BY SIZE INTO FELTEXT                                    
036400        CALL FELLOG                                                       
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800 S09-SEND-CLOSE SECTION.                                                  
036900                                                                          
037000       MOVE 'CLOSE' TO FEL-SEND-KDFUNC                                    
037100       CALL WZ01SEND USING FEL-SEND-CONTROL-AREA                          
037200******** OM FEL                                                           
037300     IF FEL-SEND-KDRC  > 0                                                
037400        MOVE FEL-SEND-KDRC TO KDRC-DISPLAY                                
037500        STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                   
037600        DELIMITED BY SIZE INTO FELTEXT                                    
037700        CALL FELLOG                                                       
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100* --- IMS SEKTIONER ---                                                   
038200     SKIP3                                                                
038300 IMS-GHU-WDC711 SECTION.                                                  
038400                                                                          
038500     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
038600          DELIMITED BY SIZE INTO SSA1                                     
038700     STRING 'WDC711  (IDPRQUES =' W-WDC711KY-X ')'                        
038800          DELIMITED BY SIZE INTO SSA2                                     
038900     MOVE '  GE' TO GODK-STATUSKODER                                      
039000     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC711 SSA1 SSA2              
039100     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039400     SKIP3                                                                
039500 IMS-REPL-WDC711 SECTION.                                                 
039600                                                                          
039700     MOVE '  ' TO GODK-STATUSKODER                                        
039800     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
039900     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
040000     PERFORM IMS-STATUSKONTROLL                                           
040100     .                                                                    
040200     SKIP3                                                                
040300 IMS-STATUSKONTROLL SECTION.                                              
040400                                                                          
040500     SET STATUS-IX TO 1                                                   
040600     SEARCH GODK-STATUS                                                   
040700       AT END                                                             
040800         PERFORM S03-RECV-CLOSE                                           
040900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041000         DELIMITED BY SIZE INTO FELTEXT                                   
041100         CALL FELLOG                                                      
041200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041300         CONTINUE                                                         
041400     END-SEARCH                                                           
041500     .                                                                    
