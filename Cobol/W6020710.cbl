000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6020710.                                                
000300 AUTHOR.         TOMMIE JIVARP / RAHUL REDDY.                             
000400 DATE-WRITTEN.   98/01/26 / JUNE 2012.                                    
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER OCH UPPDATERAR W6KVAE                           
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR W6KVAE (W6H7)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        REQU:        W60207I1                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        RESP:        W60207O1                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6020710'.            
002600                                                                          
002700*    -- INDEX FÖR BILDENS FRITEXTRADER                                    
002800 77  INDX                        PIC S9(4)   VALUE +0                     
002900                                                   COMP SYNC.             
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 01  ALL-SPACE.                                                           
003700     03 FILLER                   PIC X(80)   VALUE SPACE.                 
003800 01  ALL-PLUS.                                                            
003900     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
004000                                                                          
004100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004200                                                                          
004300 77  WS-IDKR                     PIC X(5)    VALUE SPACE.                 
004400                                                                          
004500 77  LAES-VISA-INFO-SW           PIC X       VALUE 'J'.                   
004600     88  LAES-VISA-OK                        VALUE 'J'.                   
004700     88  LAES-VISA-EJ-OK                     VALUE 'N'.                   
004800                                                                          
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700     EJECT                                                                
005800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005900 01  GENERELLA-SUBPROGRAM.                                                
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     EJECT                                                                
006300 01  MESSAGE-CODES.                                                       
006400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
006500     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
006600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
006700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
006800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
006900     03  ERR-REPORT-MISSING      PIC X(3)    VALUE '364'.                 
007000     EJECT                                                                
007100                                                                          
007200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
007300     SKIP3                                                                
007400*01  -COPY WMFSAREA                                                       
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008200     03  W-IDKR-X.                                                        
008300         05  W-IDKR              PIC 9(5)    VALUE ZERO.                  
008400     03  W-KDSEGKEY-X.                                                    
008500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     SKIP2                                                                
009300 01  GODK-STATUSKODER.                                                    
009400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009500     SKIP3                                                                
009600 01  SSA1                        PIC X(64).                               
009700 01  SSA2                        PIC X(64).                               
009800     EJECT                                                                
009900*    --- IMS FUNKTIONSKODER                                               
010000*01  -COPY W0003                                                          
010100     EJECT                                                                
010200*    ---  DLI INPUT-OUTPUT AREA                                           
010300                                                                          
010400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-W6KVAE01'.        
010500 01  DLI-IO-W6KVAE01.                                                     
010600*    03  -COPY W6H701  -PRE KVAE-                                         
010700     EJECT                                                                
010800 01  FILLER                     PIC X(16) VALUE 'DLI-IO-W6KVAE14'.        
010900 01  DLI-IO-W6KVAE14.                                                     
011000*    03  -COPY W6H714  -PRE KVAE-                                         
011100     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300 01  REQU-AREA.                                                           
011400*    03 -COPY WZ01REQU                                                    
011500*    03 -COPY W60207I1                                                    
011600     EJECT                                                                
011700 01  RESP-AREA.                                                           
011800*    03 -COPY WZ01RESP                                                    
011900*    03 -COPY W60207O1                                                    
012000     EJECT                                                                
012100 01  MAX-KVRADER                 PIC S9(4) COMP.                          
012200*01  -COPY W0008  -PRE KVAE-                                              
012300     05  FILLER                  PIC X.                                   
012400     EJECT                                                                
012500 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
012600                           KVAE-PCB.                                      
012700 MAIN SECTION.                                                            
012800     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER                
012900                           KVAE-PCB.                                      
013000     PERFORM A-INIT                                                       
013100     PERFORM B-KOLLA-NYCKLAR                                              
013200     IF NYCKLAR-OK                                                        
013300       IF REQU-UPDATE                                                     
013400         PERFORM G-KOLLA-INPUT                                            
013500         IF INDATA-OK                                                     
013600           PERFORM H-UPPDATERA                                            
013700         END-IF                                                           
013800       ELSE                                                               
013900         IF REQU-FIRST                                                    
014000           PERFORM C-FOERSTA-SIDA                                         
014100         ELSE                                                             
014200           PERFORM E-SAMMA-SIDA                                           
014300         END-IF                                                           
014400       END-IF                                                             
014500       IF LAES-VISA-OK                                                    
014600         PERFORM F-LAES-VISA-INFO                                         
014700       END-IF                                                             
014800     END-IF                                                               
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200                                                                          
015300 A-INIT SECTION.                                                          
015400                                                                          
015500     MOVE ALL '+'                TO RESP-W60207O1                         
015600     PERFORM MFS-FORM-ATTR                                                
015700     MOVE 001                    TO RESP-IDMSGVER                         
015800     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
015900                                    RESP-IDMSG-INFO                       
016000                                    RESP-IDELMT-ERROR                     
016100     .                                                                    
016200     EJECT                                                                
016300                                                                          
016400 B-KOLLA-NYCKLAR SECTION.                                                 
016500                                                                          
016600     MOVE JA                     TO NYCKLAR-SW                            
016700                                                                          
016800     MOVE REQU-IDKR-KEY          TO WS-IDKR                               
016900                                                                          
017000     IF WS-IDKR NUMERIC                                                   
017100       MOVE WS-IDKR              TO W-IDKR                                
017200     ELSE                                                                 
017300       MOVE NEJ                  TO NYCKLAR-SW                            
017400     END-IF                                                               
017500                                                                          
017600     IF NYCKLAR-FEL                                                       
017700       MOVE ERR-REPORT-MISSING   TO RESP-IDMSG-ERROR                      
017800       PERFORM MFS-RENSA-FAELT-IN                                         
017900     END-IF                                                               
018000     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 C-FOERSTA-SIDA SECTION.                                                  
018400                                                                          
018500     CONTINUE                                                             
018600     .                                                                    
018700     EJECT                                                                
018800                                                                          
018900 E-SAMMA-SIDA SECTION.                                                    
019000                                                                          
019100       IF REQU-INPUT = ALL '+'                                            
019200         PERFORM MFS-RENSA-FAELT-IN                                       
019300       ELSE                                                               
019400         MOVE NEJ                TO LAES-VISA-INFO-SW                     
019500         MOVE INF-PRESS-PF11     TO RESP-IDMSG-INFO                       
019600         PERFORM EA-MID-INDATA-TILL-MOD                                   
019700       END-IF                                                             
019800     .                                                                    
019900     EJECT                                                                
020000                                                                          
020100 EA-MID-INDATA-TILL-MOD SECTION.                                          
020200                                                                          
020300     IF REQU-FLAGGA-DEL-UPD NOT = ALL '+'                                 
020400       MOVE ALL-PLUS             TO RESP-FLAGGA-DEL-UPD                   
020500       MOVE MFS-ADD-LAES-IN-FAELT                                         
020600                                 TO RESP-FLAGGA-DEL-UPD-ATTR              
020700     ELSE                                                                 
020800       MOVE ALL-SPACE            TO RESP-FLAGGA-DEL-UPD                   
020900     END-IF                                                               
021000                                                                          
021100     MOVE +1                     TO INDX                                  
021200     PERFORM                                                              
021300       UNTIL INDX > MAX-KVRADER                                           
021400       MOVE ALL-PLUS             TO RESP-TEKRFEL-LINE(INDX)               
021500       MOVE MFS-ADD-LAES-IN-FAELT                                         
021600                                 TO RESP-TEKRFEL-LINE-ATTR(INDX)          
021700       ADD +1                    TO INDX                                  
021800     END-PERFORM                                                          
021900     .                                                                    
022000     EJECT                                                                
022100                                                                          
022200 F-LAES-VISA-INFO SECTION.                                                
022300                                                                          
022400     PERFORM IMS-GU-KVAE-IDKR                                             
022500     IF SEGMENT-SAKNAS                                                    
022600       MOVE ERR-REPORT-MISSING   TO RESP-IDMSG-ERROR                      
022700       PERFORM MFS-RENSA-FAELT-IN                                         
022800     ELSE                                                                 
022900       IF KVAE-KR-FLANNULL = JA                                           
023000         PERFORM MFS-STAENG-INMATNINGSFAELT                               
023100       ELSE                                                               
023200         IF KVAE-KR-KDKRSTA > 1                                           
023300           IF KVAE-KR-FLKRGODK = JA                                       
023400             PERFORM MFS-STAENG-INMATNINGSFAELT                           
023500           END-IF                                                         
023600         END-IF                                                           
023700       END-IF                                                             
023800       MOVE 1                    TO INDX                                  
023900       PERFORM IMS-GHNP-KVAE-TEXT                                         
024000       PERFORM                                                            
024100         UNTIL INDX > MAX-KVRADER                                         
024200         IF SEGMENT-FINNS                                                 
024300           MOVE KVAE-TEXT-TEKRFEL(INDX)                                   
024400                                 TO RESP-TEKRFEL-LINE(INDX)               
024500         ELSE                                                             
024600           MOVE ALL-SPACE        TO RESP-TEKRFEL-LINE(INDX)               
024700                                    RESP-FLAGGA-DEL-UPD                   
024800         END-IF                                                           
024900         ADD +1                  TO INDX                                  
025000       END-PERFORM                                                        
025100     END-IF                                                               
025200     .                                                                    
025300     EJECT                                                                
025400                                                                          
025500 G-KOLLA-INPUT SECTION.                                                   
025600                                                                          
025700     MOVE JA                     TO INDATA-SW                             
025800     PERFORM IMS-GU-KVAE-IDKR                                             
025900     IF SEGMENT-SAKNAS                                                    
026000       MOVE NEJ                  TO INDATA-SW                             
026100     ELSE                                                                 
026200       IF REQU-INPUT = ALL '+'                                            
026300         MOVE ERR-PF11-AND-NO-DATA                                        
026400                                 TO RESP-IDMSG-ERROR                      
026500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
026600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
026700         MOVE NEJ                TO INDATA-SW                             
026800       ELSE                                                               
026900         IF REQU-FLAGGA-DEL-UPD = ALL '+' OR 'J' OR                       
027000                                  'Y' OR 'N' OR SPACE                     
027100           MOVE MFS-ALFA-FAELT-RAETT                                      
027200                                 TO RESP-FLAGGA-DEL-UPD-ATTR              
027300         ELSE                                                             
027400           MOVE MFS-ALFA-FAELT-FEL                                        
027500                                 TO RESP-FLAGGA-DEL-UPD-ATTR              
027600           MOVE NEJ              TO INDATA-SW                             
027700         END-IF                                                           
027800         MOVE +1 TO INDX                                                  
027900         PERFORM                                                          
028000           UNTIL INDX > MAX-KVRADER                                       
028100           MOVE MFS-ALFA-FAELT-RAETT                                      
028200                                 TO RESP-TEKRFEL-LINE-ATTR(INDX)          
028300           ADD +1                TO INDX                                  
028400         END-PERFORM                                                      
028500       END-IF                                                             
028600     END-IF                                                               
028700     IF INDATA-FEL                                                        
028800       IF SEGMENT-SAKNAS                                                  
028900         MOVE ERR-REPORT-MISSING TO RESP-IDMSG-ERROR                      
029000       ELSE                                                               
029100         MOVE ERR-CORR-HILITE-FLDS                                        
029200                                 TO RESP-IDMSG-ERROR                      
029300         MOVE NEJ                TO LAES-VISA-INFO-SW                     
029400       END-IF                                                             
029500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
029600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000                                                                          
030100 H-UPPDATERA SECTION.                                                     
030200                                                                          
030300     PERFORM IMS-GU-KVAE-IDKR                                             
030400     IF SEGMENT-FINNS                                                     
030500       PERFORM IMS-GHNP-KVAE-TEXT                                         
030600       IF SEGMENT-FINNS                                                   
030700         IF REQU-FLAGGA-DEL-UPD = 'Y' OR 'J'                              
030800           PERFORM IMS-DLET-KVAE-TEXT                                     
030900         ELSE                                                             
031000           MOVE +1               TO INDX                                  
031100           PERFORM                                                        
031200             UNTIL INDX > MAX-KVRADER                                     
031300             IF REQU-TEKRFEL-LINE(INDX) NOT = ALL '+'                     
031400               MOVE REQU-TEKRFEL-LINE(INDX)                               
031500                                 TO KVAE-TEXT-TEKRFEL(INDX)               
031600                                    RESP-TEKRFEL-LINE(INDX)               
031700             ELSE                                                         
031800               MOVE SPACE        TO KVAE-TEXT-TEKRFEL(INDX)               
031900               MOVE ALL-PLUS     TO RESP-TEKRFEL-LINE(INDX)               
032000             END-IF                                                       
032100             ADD +1              TO INDX                                  
032200           END-PERFORM                                                    
032300           PERFORM IMS-REPL-KVAE-TEXT                                     
032400         END-IF                                                           
032500       ELSE                                                               
032600         MOVE +1                 TO INDX                                  
032700         PERFORM                                                          
032800           UNTIL INDX > MAX-KVRADER                                       
032900           IF REQU-TEKRFEL-LINE(INDX) NOT = ALL '+'                       
033000             MOVE REQU-TEKRFEL-LINE(INDX)                                 
033100                                 TO KVAE-TEXT-TEKRFEL(INDX)               
033200                                    RESP-TEKRFEL-LINE(INDX)               
033300           ELSE                                                           
033400             MOVE ALL-SPACE      TO RESP-TEKRFEL-LINE(INDX)               
033500             MOVE SPACE          TO KVAE-TEXT-TEKRFEL(INDX)               
033600           END-IF                                                         
033700           ADD +1                TO INDX                                  
033800         END-PERFORM                                                      
033900         MOVE '1'                TO KVAE-TEXT-KDSEGKEY                    
034000         PERFORM IMS-ISRT-KVAE-TEXT                                       
034100       END-IF                                                             
034200     END-IF                                                               
034300     MOVE JA                     TO LAES-VISA-INFO-SW                     
034400     MOVE INF-UPDATE-DONE        TO RESP-IDMSG-INFO                       
034500     PERFORM MFS-FORM-ATTR                                                
034600     PERFORM MFS-ROER-EJ-FAELT-IN                                         
034700     PERFORM MFS-ROER-EJ-FAELT-UT                                         
034800     .                                                                    
034900     EJECT                                                                
035000 MFS-RENSA-FAELT-IN SECTION.                                              
035100                                                                          
035200     MOVE +1                     TO INDX                                  
035300     PERFORM                                                              
035400       UNTIL INDX > MAX-KVRADER                                           
035500       MOVE ALL-SPACE            TO RESP-TEKRFEL-LINE(INDX)               
035600       ADD +1                    TO INDX                                  
035700     END-PERFORM                                                          
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
036100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
036200                                                                          
036300     MOVE ALL-PLUS               TO RESP-FLAGGA-DEL-UPD                   
036400                                                                          
036500     MOVE +1 TO INDX                                                      
036600     PERFORM                                                              
036700       UNTIL INDX > MAX-KVRADER                                           
036800       MOVE ALL-PLUS             TO RESP-TEKRFEL-LINE(INDX)               
036900       ADD +1                    TO INDX                                  
037000     END-PERFORM                                                          
037100     .                                                                    
037200     SKIP3                                                                
037300                                                                          
037400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
037500                                                                          
037600     MOVE +1                     TO INDX                                  
037700     PERFORM                                                              
037800       UNTIL INDX > MAX-KVRADER                                           
037900       MOVE ALL-PLUS             TO RESP-TEKRFEL-LINE(INDX)               
038000       ADD +1                    TO INDX                                  
038100     END-PERFORM                                                          
038200     .                                                                    
038300     EJECT                                                                
038400                                                                          
038500 MFS-FORM-ATTR SECTION.                                                   
038600                                                                          
038700*    --- ALLA INDATA-FÄLT                                                 
038800     MOVE MFS-FORMATETS-ATTR     TO RESP-FLAGGA-DEL-UPD-ATTR              
038900     MOVE +1                     TO INDX                                  
039000     PERFORM                                                              
039100       UNTIL INDX > MAX-KVRADER                                           
039200       MOVE MFS-FORMATETS-ATTR   TO RESP-TEKRFEL-LINE-ATTR (INDX)         
039300       ADD +1                    TO INDX                                  
039400     END-PERFORM                                                          
039500     .                                                                    
039600     SKIP2                                                                
039700                                                                          
039800 MFS-STAENG-INMATNINGSFAELT SECTION.                                      
039900                                                                          
040000     MOVE +1                     TO INDX                                  
040100     MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLAGGA-DEL-UPD-ATTR              
040200     PERFORM                                                              
040300       UNTIL INDX > MAX-KVRADER                                           
040400       MOVE MFS-STAENG-FAELT-NOMOD                                        
040500                                 TO RESP-TEKRFEL-LINE-ATTR(INDX)          
040600       ADD +1                    TO INDX                                  
040700     END-PERFORM                                                          
040800     .                                                                    
040900     SKIP3                                                                
041000                                                                          
041100* --- IMS SEKTIONER ---                                                   
041200     SKIP3                                                                
041300                                                                          
041400 IMS-GU-KVAE-IDKR SECTION.                                                
041500                                                                          
041600     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
041700       DELIMITED BY SIZE INTO SSA1                                        
041800     MOVE '  GE'                 TO GODK-STATUSKODER                      
041900     CALL CBLTDLI             USING GU KVAE-PCB                           
042000                                    DLI-IO-W6KVAE01 SSA1                  
042100     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     EJECT                                                                
042500                                                                          
042600 IMS-GHNP-KVAE-TEXT SECTION.                                              
042700                                                                          
042800     STRING 'W6KVAE14(KDSEGKEY =' W-KDSEGKEY-X ')'                        
042900       DELIMITED BY SIZE INTO SSA1                                        
043000     MOVE '  GE'                 TO GODK-STATUSKODER                      
043100     CALL CBLTDLI             USING GHNP KVAE-PCB                         
043200                                    DLI-IO-W6KVAE14 SSA1                  
043300     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
043400     PERFORM IMS-STATUSKONTROLL                                           
043500     .                                                                    
043600     SKIP3                                                                
043700                                                                          
043800 IMS-ISRT-KVAE-TEXT SECTION.                                              
043900                                                                          
044000     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
044100       DELIMITED BY SIZE INTO SSA1                                        
044200     MOVE 'W6KVAE14 '            TO SSA2                                  
044300     MOVE '  II'                 TO GODK-STATUSKODER                      
044400     CALL CBLTDLI             USING ISRT KVAE-PCB                         
044500                                    DLI-IO-W6KVAE14 SSA1 SSA2             
044600     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     SKIP3                                                                
045000                                                                          
045100 IMS-REPL-KVAE-TEXT SECTION.                                              
045200                                                                          
045300     MOVE '  '                   TO GODK-STATUSKODER                      
045400     CALL CBLTDLI             USING REPL KVAE-PCB DLI-IO-W6KVAE14         
045500     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     .                                                                    
045800     SKIP3                                                                
045900                                                                          
046000 IMS-DLET-KVAE-TEXT SECTION.                                              
046100                                                                          
046200     MOVE '  '                   TO GODK-STATUSKODER                      
046300     CALL CBLTDLI             USING DLET KVAE-PCB DLI-IO-W6KVAE14         
046400     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     .                                                                    
046700     EJECT                                                                
046800                                                                          
046900 IMS-STATUSKONTROLL SECTION.                                              
047000                                                                          
047100     SET STATUS-IX               TO 1                                     
047200     SEARCH GODK-STATUS                                                   
047300       AT END                                                             
047400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047500           DELIMITED BY SIZE INTO FELTEXT                                 
047600         CALL FELLOG                                                      
047700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047800         CONTINUE                                                         
047900     END-SEARCH                                                           
048000     .                                                                    
