000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3359D00.                                                
000300 AUTHOR.         ELEONOR ÖSTRÖM                                           
000400 DATE-WRITTEN.   07/11/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BMP FÖR ATT SÄNDA ALLA OMFRÅGOR FÖR PRISFRÅGOR.                  
001100*        FRÅGORNA BUNTAS IHOP FÖR ATT VIPS INTE KLARAR AV ATT TA          
001200*        EMOT SÅ STORA MÄNGDER FRÅGOR SOM DET BLIR VID OMFRÅGOR.          
001400*                                                                         
001500*    E'TRACKER ID: 5978507                                                
001600*                                                                         
001700*    UTDATA.                                                              
001800*        SÄNDNING VIA WZ01  TILL VIPS                                     
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500*     --- INFIL FRÅN W3359E                                               
002600     SELECT W3359E               ASSIGN TO W3359DD1.                      
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 FILE SECTION.                                                            
003100 FD  W3359E                                                               
003200     RECORDING F                                                          
003300     BLOCK CONTAINS 0.                                                    
003400                                                                          
003500*01  POST  -COPY W3359C01   -PRE IN-   -L.                                
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W3359D00'.            
003900*                                                                         
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  ANTAL-SEND                  PIC S9(4)  COMP-3 VALUE ZERO.            
004610 77  MAX-ANTAL-SEND              PIC S9(4)  COMP-3 VALUE +0400.           
004700 77  W-FIRST-TIME                PIC 9       VALUE ZERO.                  
004800                                                                          
004900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005000     88  NYCKLAR-OK                          VALUE 'J'.                   
005100     88  NYCKLAR-FEL                         VALUE 'N'.                   
005200                                                                          
005300 77  W3359E-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W3359E                       VALUE 'J'.                   
005500                                                                          
005600                                                                          
005700 01  CHKP-VAR.                                                            
005800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006300     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006400     SKIP2                                                                
006500                                                                          
006600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006700 01  GENERELLA-SUBPROGRAM.                                                
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007100     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007300     EJECT                                                                
007400 01  MESSAGE-CODES.                                                       
007500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100*    --- AREOR FÖR KOMMUNIKATION                                          
008200 01  FILLER                      PIC X(16)   VALUE 'SENDING-AREA'.        
008300*01  -COPY WZ01SEND                                                       
008400 01  SEND-DATA.                                                           
008500*03  -COPY WZ01REQU -PRE MID-                                             
008600*03  -COPY W30391O1                                                       
008700 01   KDRC-DISPLAY               PIC X(4).                                
008800     EJECT                                                                
008900*01  FILLER    -COPY W3359C01   -PRE IN-                                  
009000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500                                                                          
009600     03  W-WDC701-X.                                                      
009700         05  W-IDDISTR           PIC 9(4)    VALUE ZERO.                  
009800         05  W-IDKUNDNR          PIC 9(7)    VALUE ZERO.                  
009900         05  W-IDBUNDLE          PIC X(15)   VALUE SPACE.                 
010000                                                                          
010100     03  W-WDC711-X.                                                      
010200       05  W-IDPRQUES            PIC 9(7)    VALUE ZERO.                  
010300                                                                          
010400   03  W-IDBUNDLE-X.                                                      
010500     05  W-PRQ-IDBUNDLE          PIC X(15) VALUE SPACE.                   
010600     05  W-PRQ-IDORDNR7-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
010700       07  W-PRQ-IDORDNR7        PIC 9(7).                                
010800       07  FILLER                PIC X(8).                                
010900     05  W-PRQ-IDRAPPNR-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
011000       07  W-PRQ-IDRAPPNR        PIC 9(7).                                
011100       07  FILLER                PIC X(8).                                
011200                                                                          
011300     03  WDR401-X.                                                        
011400         05  FILLER              PIC X(4)    VALUE '3101'.                
011500         05  W-IDDISTR1          PIC 9(4)    VALUE ZERO.                  
011600         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012400     88  IMS-EJ-OK                           VALUE 'XD'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(128).                              
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
013800 01  DLI-IO-WDC701.                                                       
013900*    03  -COPY WDC701                                                     
014000     EJECT                                                                
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
014200 01  DLI-IO-WDC711.                                                       
014300*    03  -COPY WDC711                                                     
014400     EJECT                                                                
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3102'.                    
014600 01  DLI-IO-WDGX3102.                                                     
014700*    03  -COPY WDGX3102                                                   
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000*01  -COPY W0009  -PRE MSG-                                               
015100*01  -COPY W0008  -PRE WDC7-                                              
015200     05  FILLER                  PIC X.                                   
015300*01  -COPY W0008  -PRE WDR4-                                              
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING MSG-PCB WDC7-PCB WDR4-PCB.                     
015700 MAIN SECTION.                                                            
015800     ENTRY 'DLITCBL' USING MSG-PCB WDC7-PCB WDR4-PCB.                     
015900                                                                          
016000     PERFORM A-INIT                                                       
016100     PERFORM C-CHECKBASE                                                  
016200     PERFORM Z-FINIT                                                      
016300                                                                          
016400     MOVE ZERO TO RETURN-CODE                                             
016500     GOBACK                                                               
016600     .                                                                    
016700     EJECT                                                                
016800                                                                          
016900 A-INIT SECTION.                                                          
017000                                                                          
017100     OPEN INPUT W3359E                                                    
017200     PERFORM IMS-RESTART                                                  
017300                                                                          
017400     MOVE +0                          TO CHKP-ANT                         
017500                                                                          
017600     MOVE +0                          TO ANTAL-SEND                       
017700     MOVE 0                           TO W-FIRST-TIME                     
017800                                                                          
017900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018000     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 C-CHECKBASE SECTION.                                                     
018400                                                                          
018500     PERFORM S01-LAES-W3359E                                              
018600     MOVE IN-IDDISTR    TO W-IDDISTR1                                     
018700     IF NOT END-OF-W3359E                                                 
018800       PERFORM IMS-GU-WDGX3102                                            
018900       MOVE 3102-ADDISPABS-ASYNC TO SEND-ADDISPABS                        
019000     END-IF                                                               
019100                                                                          
019200     PERFORM UNTIL END-OF-W3359E                                          
019300                                                                          
019400       MOVE IN-IDDISTR    TO   W-IDDISTR                                  
019500       MOVE IN-IDKUNDNR   TO   W-IDKUNDNR                                 
019600       MOVE IN-IDBUNDLE   TO   W-IDBUNDLE                                 
019700       PERFORM IMS-GU-WDC701                                              
019800       MOVE PRQ-IDDISTR   TO   MOD-IDDISTR                                
019900       MOVE PRQ-IDKUNDNR  TO   MOD-IDKUNDNR                               
020000       MOVE PRQ-IDBUNDLE  TO   MOD-IDBUNDLE                               
020100                                                                          
020200       IF SEGMENT-FINNS                                                   
020300         IF IN-IDDISTR NOT = W-IDDISTR1                                   
020400                                                                          
020500           IF W-FIRST-TIME = +1                                           
020600             PERFORM S06-SEND-CLOSE                                       
020700             MOVE 0    TO W-FIRST-TIME                                    
020800             MOVE ZERO TO ANTAL-SEND                                      
020900             PERFORM X-TAG-CHECKPOINT                                     
021000             ADD +1       TO CHKP-ANT                                     
021100           END-IF                                                         
021200                                                                          
021300           MOVE IN-IDDISTR    TO W-IDDISTR1                               
021400           PERFORM IMS-GU-WDGX3102                                        
021500           MOVE 3102-ADDISPABS-ASYNC TO SEND-ADDISPABS                    
021600         END-IF                                                           
021700                                                                          
021800         PERFORM UNTIL END-OF-W3359E OR                                   
021900              MOD-IDDISTR  NOT = IN-IDDISTR OR                            
022000              MOD-IDKUNDNR NOT = IN-IDKUNDNR OR                           
022100              MOD-IDBUNDLE NOT = IN-IDBUNDLE                              
022200           MOVE IN-IDPRQUES   TO   W-IDPRQUES                             
022300           PERFORM IMS-GHNP-WDC711                                        
022400                                                                          
022500           IF SEGMENT-FINNS                                               
022600             IF W-FIRST-TIME = 0                                          
022700               MOVE 1 TO W-FIRST-TIME                                     
022800               PERFORM S04-SEND-OPEN                                      
022900             END-IF                                                       
023000                                                                          
023100             MOVE LPRQ-IDPRQUES TO   MOD-IDPRQUES                         
023200             MOVE LPRQ-IDARTNR  TO   MOD-IDARTNR                          
023300             MOVE LPRQ-KDORDKL  TO   MOD-KDORDKL                          
023400             MOVE LPRQ-KVBEART  TO   MOD-KVBEART                          
023500                                                                          
023600             PERFORM S05-SEND-PUT                                         
023700                                                                          
023800             ADD +1             TO ANTAL-SEND                             
023900                                                                          
024700            IF ANTAL-SEND > MAX-ANTAL-SEND                                
024710            DISPLAY 'ANTAL-SEND = ' ANTAL-SEND                            
024800              PERFORM S06-SEND-CLOSE                                      
024900              MOVE 0    TO W-FIRST-TIME                                   
025000              MOVE ZERO TO ANTAL-SEND                                     
025100              PERFORM X-TAG-CHECKPOINT                                    
025200              ADD +1       TO CHKP-ANT                                    
025300            END-IF                                                        
025400          END-IF                                                          
025500                                                                          
025600          PERFORM S01-LAES-W3359E                                         
025700                                                                          
025800         END-PERFORM                                                      
025900       ELSE                                                               
026000         PERFORM S01-LAES-W3359E                                          
026100       END-IF                                                             
026200                                                                          
026300     END-PERFORM                                                          
026400                                                                          
026500     IF W-FIRST-TIME = 1                                                  
026510            DISPLAY 'ANTAL-SEND = ' ANTAL-SEND                            
026600       PERFORM S06-SEND-CLOSE                                             
026700     END-IF                                                               
026800                                                                          
026900     .                                                                    
027000     EJECT                                                                
027100 Z-FINIT SECTION.                                                         
027200                                                                          
027300     CLOSE W3359E                                                         
027400                                                                          
027500     MOVE 'S' TO POSTSUM-OPKOD                                            
027600     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
027700     .                                                                    
027800     EJECT                                                                
027900 S01-LAES-W3359E  SECTION.                                                
028000                                                                          
028100     READ W3359E INTO IN-AREA                                             
028200     AT END                                                               
028300        SET END-OF-W3359E TO TRUE                                         
028400     NOT AT END                                                           
028500        MOVE 'W3359E'       TO POSTSUM-FDNAMN                             
028600        MOVE 'W3359DD1'     TO POSTSUM-DDNAMN2                            
028700        MOVE SPACE          TO POSTSUM-TRANSTYP                           
028800        CALL POSTSUM USING POSTSUM-PARM                                   
028900     END-READ                                                             
029000     .                                                                    
029100     EJECT                                                                
029200 S04-SEND-OPEN SECTION.                                                   
029300     MOVE 'OPEN' TO SEND-KDFUNC                                           
029400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029500                         SEND-OPEN-AREA                                   
029600******** OM FEL                                                           
029700     IF SEND-KDRC  > 0                                                    
029800        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
029900        STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                    
030000        DELIMITED BY SIZE INTO FELTEXT                                    
030100        DISPLAY FELTEXT                                                   
030200        CALL FELLOG                                                       
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 S05-SEND-PUT SECTION.                                                    
030700                                                                          
030800     MOVE 'PUT' TO SEND-KDFUNC                                            
030900     MOVE LENGTH OF SEND-DATA TO SEND-KVDLEN                              
031000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031100                         SEND-KVDLEN                                      
031200                         SEND-DATA                                        
031300******** OM FEL                                                           
031400     IF SEND-KDRC  > 0                                                    
031500        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
031600        STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                    
031700        DELIMITED BY SIZE INTO FELTEXT                                    
031800        DISPLAY FELTEXT                                                   
031900        CALL FELLOG                                                       
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 S06-SEND-CLOSE SECTION.                                                  
032400                                                                          
032500       MOVE 'CLOSE' TO SEND-KDFUNC                                        
032600       CALL WZ01SEND USING SEND-CONTROL-AREA                              
032700******** OM FEL                                                           
032800     IF SEND-KDRC  > 0                                                    
032900        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
033000        STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                   
033100        DELIMITED BY SIZE INTO FELTEXT                                    
033200        DISPLAY FELTEXT                                                   
033300        CALL FELLOG                                                       
033400     END-IF                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 X-TAG-CHECKPOINT   SECTION.                                              
033800                                                                          
033900* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
034000* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
034100     PERFORM IMS-CHECKPOINT                                               
034200     MOVE ZERO TO CHKP-ANT                                                
034300* --- LÄS OM DATABAS OM DET BEHÖVS                                        
034400                                                                          
034500     MOVE PRQ-IDDISTR       TO W-IDDISTR                                  
034600     MOVE PRQ-IDKUNDNR      TO W-IDKUNDNR                                 
034700     MOVE PRQ-IDBUNDLE      TO W-PRQ-IDBUNDLE                             
034800                                                                          
034900     PERFORM IMS-GU-WDC701                                                
035000     .                                                                    
035100     EJECT                                                                
035200* --- IMS SEKTIONER ---                                                   
035300     SKIP3                                                                
035400 IMS-RESTART SECTION.                                                     
035500     SKIP2                                                                
035600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035700     MOVE '  ' TO GODK-STATUSKODER                                        
035800     CALL CBLTDLI USING XRST MSG-PCB                                      
035900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036000                        CHKP-AREA-LENGTH CHKP-AREA                        
036100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036200     PERFORM IMS-STATUSKONTROLL                                           
036300     .                                                                    
036400     SKIP3                                                                
036500 IMS-CHECKPOINT SECTION.                                                  
036600     SKIP2                                                                
036700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036800     MOVE '  XD' TO GODK-STATUSKODER                                      
036900     CALL CBLTDLI USING CHKP MSG-PCB                                      
037000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037100                        CHKP-AREA-LENGTH CHKP-AREA                        
037200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037300     PERFORM IMS-STATUSKONTROLL                                           
037400                                                                          
037500     IF IMS-EJ-OK                                                         
037600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT                
037700       DISPLAY FELTEXT                                                    
037800       CALL FELLOG                                                        
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 IMS-GU-WDC701 SECTION.                                                   
038300                                                                          
038400     STRING 'WDC701  (WDC701KY =' W-WDC701-X ')'                          
038500          DELIMITED BY SIZE INTO SSA1                                     
038600     MOVE '  GE'         TO GODK-STATUSKODER                              
038700     CALL CBLTDLI USING GU WDC7-PCB DLI-IO-WDC701 SSA1                    
038800     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
038900     PERFORM IMS-STATUSKONTROLL                                           
039000     .                                                                    
039100     SKIP3                                                                
039200 IMS-GHNP-WDC711 SECTION.                                                 
039300                                                                          
039400     STRING 'WDC711  (IDPRQUES =' W-WDC711-X ')'                          
039500          DELIMITED BY SIZE INTO SSA1                                     
039600     MOVE '  GE'         TO GODK-STATUSKODER                              
039700     CALL CBLTDLI USING GHNP WDC7-PCB DLI-IO-WDC711 SSA1                  
039800     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100     SKIP3                                                                
040200 IMS-REPL-WDC711 SECTION.                                                 
040300                                                                          
040400     MOVE '  ' TO GODK-STATUSKODER                                        
040500     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
040600     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
040700     PERFORM IMS-STATUSKONTROLL                                           
040800     .                                                                    
040900     SKIP3                                                                
041000 IMS-GU-WDGX3102 SECTION.                                                 
041100                                                                          
041200     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
041300          DELIMITED BY SIZE INTO SSA1                                     
041400     MOVE   'WDGX3102' TO SSA2                                            
041500     MOVE '  ' TO GODK-STATUSKODER                                        
041600     CALL CBLTDLI USING GU WDR4-PCB DLI-IO-WDGX3102                       
041700                                     SSA1 SSA2                            
041800     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     SKIP3                                                                
042200 IMS-STATUSKONTROLL SECTION.                                              
042300                                                                          
042400     SET STATUS-IX TO 1                                                   
042500     SEARCH GODK-STATUS                                                   
042600       AT END                                                             
042700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042800         DELIMITED BY SIZE INTO FELTEXT                                   
042900         CALL FELLOG                                                      
043000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043100         CONTINUE                                                         
043200     END-SEARCH                                                           
043300     .                                                                    
