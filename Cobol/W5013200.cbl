000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5013200.                                                
000300 AUTHOR.         LASSE C. / MARKUS ASPFJÄLL                               
000400 DATE-WRITTEN.   SEPT. 1995./ FEB 2002                                    
000500                                                                          
000600*    FUNKTION:                                                            
000700*        FRÅGA PÅ OCH UPPDATERING AV LANDINGCOST PÅ BETALAR-              
000800*        REGISTRET WDB1.                                                  
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W5T132                                              
001200*                     W5T132U                                             
001300*        MID:         W5I13201                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W5O13201                                            
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W5013200'.            
002700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 77  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
003100                                                                          
003200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003300     88  INDATA-OK                           VALUE 'J'.                   
003400     88  INDATA-FEL                          VALUE 'N'.                   
003500                                                                          
003600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003700     88  NYCKLAR-OK                          VALUE 'J'.                   
003800     88  NYCKLAR-FEL                         VALUE 'N'.                   
003900                                                                          
004000 77  NYA-NYCKLAR-SW              PIC X       VALUE 'N'.                   
004100     88  NYA-NYCKLAR                         VALUE 'J'.                   
004200                                                                          
004300 77  ALLT-SW                     PIC X       VALUE 'N'.                   
004400     88  ALLT-OK                             VALUE 'J'.                   
004500                                                                          
004600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004700     88  EGEN-MID                            VALUE '5132'.                
004800     88  GODK-MID                            VALUE '5131' '5132'          
004900                                                   '5133' '5134'          
005000                                                   '5135' '5136'          
005100                                                   '5137' '5138'          
005200                                                   '5139'.                
005300     88 HELP-MID                             VALUE '0551'.                
005400                                                                          
005500 77  WS-DATUM                    PIC 9(6).                                
005600 77  WS-KLOCK                    PIC 9(8).                                
005700 01  W-RELANDCO                  PIC 9(3).9(2)   VALUE ZERO.              
005800 01  W-TISTADAT                  PIC 9(6)        VALUE ZERO.              
005900 01  W-TISTODAT                  PIC 9(6)        VALUE ZERO.              
006000 01  W-DAGENS-DATUM              PIC 9(6)        VALUE ZERO.              
006100                                                                          
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA'.            
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*   -COPY WMEDAREA                                                        
007500                                                                          
007600 01  MESSAGE-CODES.                                                       
007700     03  ERR-CORR-HILITE-FLDS     PIC X(3)    VALUE '001'.                
007800     03  INF-PRESS-PF11           PIC X(3)    VALUE '003'.                
007900     03  INF-FIRST-PAGE           PIC X(3)    VALUE '006'.                
008000     03  ERR-UPPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                
008100     03  ERR-PF11-AND-NO-DATA     PIC X(3)    VALUE '011'.                
008200     03  INF-CUSTOMER-MISSING     PIC X(3)    VALUE '063'.                
008300     03  INF-UPDATE-DONE          PIC X(3)    VALUE '101'.                
008400     03  INF-LAST-PAGE-ALREADY    PIC X(3)    VALUE '115'.                
008500     03  ERR-TERMS-PAYMENT-MISING PIC X(3)    VALUE '147'.                
008600     03  ERR-CURRENCYCODE-MISING  PIC X(3)    VALUE '148'.                
008700     03  ERR-NAME-ADDRESS-MISING  PIC X(3)    VALUE '149'.                
008800     03  ERR-CUST-DATA-MISING     PIC X(3)    VALUE '150'.                
008900     03  ERR-TYP-CURRENCY-MISING  PIC X(3)    VALUE '151'.                
009000     03  ERR-MOMS-REGNR-MISING    PIC X(3)    VALUE '223'.                
009100     03  ERR-WRONG-KEY            PIC X(3)    VALUE '401'.                
009200     03  INF-NEW-KEY-PF11         PIC X(3)    VALUE '788'.                
009300     03  ERR-NOT-NUMERIC          PIC X(3)    VALUE '020'.                
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
009600*01  -COPY WDATAREA                                                       
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
009900*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
010000*   -COPY WDECAREA                                                        
010100                                                                          
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010400     SKIP3                                                                
010500*01 -COPY WMSGINIT                                                        
010600     EJECT                                                                
010700 01  SPAR-AREA.                                                           
010800     03  SPAR-IDTRANS           PIC X(4)    VALUE '5132'.                 
010900     03  SPAR-IDPARTNR          PIC X(9).                                 
011000     EJECT                                                                
011100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011400                                                                          
011500*01  -COPY W5I13201                                                       
011600     EJECT                                                                
011700 01  FILLER                    PIC X(16)  VALUE 'MSG/MOD-AREA'.           
011800                                                                          
011900*01  -COPY WMSGAREA                                                       
012000     EJECT                                                                
012100   03  MOD REDEFINES MSG-AREA.                                            
012200*    05  -COPY W5O13201                                                   
012300     EJECT                                                                
012400 01  FILLER                    PIC X(16)   VALUE 'MFS-AREA'.              
012500*01  -COPY WMFSAREA                                                       
012600     EJECT                                                                
012700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800                                                                          
012900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013000                                                                          
013100 01  NYCKLAR-TILL-DLI.                                                    
013200     03  W-WDB101KY-X.                                                    
013300         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
013400         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
013500     EJECT                                                                
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FINNS                       VALUE '  '.                  
013900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014000                                                                          
014100 01  GODK-STATUSKODER.                                                    
014200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014300                                                                          
014400 01  SSA1                        PIC X(64).                               
014500     EJECT                                                                
014600*    --- IMS FUNKTIONSKODER                                               
014700*01  -COPY W0003                                                          
014800     EJECT                                                                
014900*    ---  DLI INPUT-OUTPUT AREA                                           
015000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015100                                                                          
015200 01  DLI-IO-AREA.                                                         
015300*  03  -COPY WDB101                                                       
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600*01  -COPY W0009      -PRE MSG-                                           
015700*01  -COPY W0008      -PRE WDP7-                                          
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000*01  -COPY W0008      -PRE WDB1-                                          
016100     05  FILLER                  PIC X(9).                                
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB1-PCB.                     
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB1-PCB.                     
016600                                                                          
016700     PERFORM IMS-GET-MSG                                                  
016800     IF SEGMENT-FINNS                                                     
016900       PERFORM A-INIT                                                     
017000       PERFORM B-KOLLA-NYCKLAR                                            
017100                                                                          
017200       IF NYCKLAR-OK                                                      
017300         IF MFS-UPDATE                                                    
017400           PERFORM G-KOLLA-INPUT                                          
017500           IF INDATA-OK                                                   
017600             PERFORM H-UPPDATERA-KUNDREG                                  
017700           END-IF                                                         
017800         END-IF                                                           
017900         PERFORM F-VISA-INFO                                              
018000       END-IF                                                             
018100                                                                          
018200       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O13201 + 4                      
018300       PERFORM IMS-INSERT-MSG                                             
018400     END-IF                                                               
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000 A-INIT SECTION.                                                          
019100                                                                          
019200     IF MSG-DUBBLA-TRANSKODER                                             
019300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I13201                 
019400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
019500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019600     ELSE                                                                 
019700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I13201                  
019800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
019900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020000     END-IF                                                               
020100                                                                          
020200     MOVE MSG-KDTRTYP           TO MFS-KDTRTYP                            
020300     MOVE MSG-IDPFK             TO MFS-IDPFK                              
020400     MOVE MFS-IDTRANS           TO W-IDTRANS                              
020500                                                                          
020600     MOVE LOW-VALUE             TO MSG-AREA                               
020700     MOVE 'W5O13201'            TO MFS-IDMOD                              
020800     MOVE '5132'                TO MOD-IDTRANS                            
020900     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
021000                                                                          
021100                                                                          
021200                                                                          
021300     IF EGEN-MID OR HELP-MID                                              
021400       CONTINUE                                                           
021500     ELSE                                                                 
021600       MOVE SPACE TO MFS-KDTRTYP                                          
021700       MOVE '7' TO MFS-IDPFK                                              
021800     END-IF                                                               
021900                                                                          
022000     IF ENGLISH-TEXT                                                      
022100       MOVE 'GB '               TO MED-IDSKYLT                            
022200     ELSE                                                                 
022300       MOVE 'S  '               TO MED-IDSKYLT                            
022400     END-IF                                                               
022500                                                                          
022600     MOVE 'IDAG  '               TO  DAT-KDDATFORM                        
022700     CALL WDATKONV USING             DAT-KDDATFORM                        
022800                                     DAT-I-TIDATUM                        
022900                                     DAT-O-TIDATUM                        
023000                                     DAT-KDSVAR                           
023100     MOVE DAT-TIAAMMDD           TO  W-DAGENS-DATUM                       
023200                                                                          
023300     .                                                                    
023400     EJECT                                                                
023500 B-KOLLA-NYCKLAR SECTION.                                                 
023600                                                                          
023700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023800     MOVE '001'             TO MSGI-KDCALL                                
023900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024100     MOVE '5132'            TO MSGI-IDTRANS                               
024200     IF GODK-MID                                                          
024300         MOVE MID-IDPARTNR-IN     TO SPAR-IDPARTNR                        
024400     END-IF                                                               
024500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
024600                                                                          
024700     MOVE MSGI-IDFTG      TO MOD-IDFTG-UT                                 
024800                             W-IDFTG                                      
024900                                                                          
025000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
025100                                                                          
025200*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
025300     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
025400                                                                          
025500                                                                          
025600     MOVE JA                    TO NYCKLAR-SW                             
025700                                                                          
025800     MOVE MFS-RENSA-FAELT       TO MOD-IDPARTNR-IN                        
025900                                                                          
026000     IF MID-IDPARTNR-IN = ALL '+'                                         
026100       MOVE SPAR-IDPARTNR       TO WS-IDPARTNR                            
026200     ELSE                                                                 
026300       IF EGEN-MID AND MID-IDPARTNR-IN NOT = ALL '+'                      
026400         MOVE MID-IDPARTNR-IN   TO WS-IDPARTNR                            
026500                                   SPAR-IDPARTNR                          
026600       END-IF                                                             
026700     END-IF                                                               
026800                                                                          
026900     IF MID-IDPARTNR-IN      NOT = ALL '+'                                
027000        MOVE JA                 TO NYA-NYCKLAR-SW                         
027100     END-IF                                                               
027200                                                                          
027300     IF NYA-NYCKLAR                                                       
027400       MOVE SPACE               TO MFS-KDTRTYP                            
027500     END-IF                                                               
027600                                                                          
027700     IF WS-IDPARTNR              = SPACE                                  
027800        MOVE NEJ                TO NYCKLAR-SW                             
027900     ELSE                                                                 
028000        MOVE WS-IDPARTNR        TO W-IDPARTNR                             
028100     END-IF                                                               
028200                                                                          
028300     IF GODK-MID OR NYCKLAR-OK                                            
028400       MOVE WS-IDPARTNR         TO MOD-IDPARTNR-UT                        
028500     ELSE                                                                 
028600       MOVE MFS-RENSA-FAELT     TO MOD-IDPARTNR-UT                        
028700     END-IF                                                               
028800                                                                          
028900     IF NYCKLAR-FEL                                                       
029000       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
029100       CALL WMEDKONV USING MED-WMEDAREA                                   
029200       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
029300       PERFORM MFS-RENSA-FAELT-IN                                         
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 F-VISA-INFO SECTION.                                                     
029800                                                                          
029900     PERFORM IMS-GET-WDB101                                               
030000                                                                          
030100     IF SEGMENT-SAKNAS                                                    
030200       MOVE INF-CUSTOMER-MISSING TO MED-IDMFSINF                          
030300       CALL WMEDKONV USING MED-WMEDAREA                                   
030400       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
030500       PERFORM MFS-RENSA-FAELT-IN                                         
030600     ELSE                                                                 
030700       MOVE BET-IDPARTNR        TO MOD-IDPARTNR-UT                        
030800                                                                          
030900       MOVE BET-BEBETRAD-1      TO MOD-BEBETRAD-1                         
031000       MOVE BET-BEBETRAD-2      TO MOD-BEBETRAD-2                         
031100       MOVE BET-ADBETRAD-1      TO MOD-ADBETRAD-1                         
031200       MOVE BET-ADBETRAD-2      TO MOD-ADBETRAD-2                         
031300       MOVE BET-BELAND-SVE      TO MOD-BELAND-SVE                         
031400       MOVE BET-RELANDCO        TO MOD-RELANDCO-OLD                       
031500*      MOVE BET-TISTADAT-KUND   TO MOD-TISTADAT-KUND-OLD                  
031600*      MOVE BET-TISTODAT-KUND   TO MOD-TISTODAT-KUND-OLD                  
031700       MOVE MFS-RENSA-FAELT     TO MOD-RELANDCO-NEW                       
031800*      MOVE MFS-RENSA-FAELT     TO MOD-TISTADAT-KUND-NEW                  
031900*      MOVE MFS-RENSA-FAELT     TO MOD-TISTODAT-KUND-NEW                  
032000                                                                          
032100       MOVE '002'             TO MSGI-KDCALL                              
032200       MOVE '5132'            TO SPAR-IDTRANS                             
032300       MOVE SPAR-AREA         TO MSGI-SPAR-AREA                           
032400       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 G-KOLLA-INPUT SECTION.                                                   
032900                                                                          
033000     MOVE JA  TO INDATA-SW                                                
033100     IF MID-RELANDCO-NEW = ALL '+'                                        
033200*       MID-TISTADAT-KUND-NEW = ALL '+' AND                               
033300*       MID-TISTODAT-KUND-NEW = ALL '+'                                   
033400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
033500       CALL WMEDKONV USING MED-WMEDAREA                                   
033600       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
033700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
033800       MOVE NEJ                 TO INDATA-SW                              
033900     ELSE                                                                 
034000       IF MID-RELANDCO-NEW NOT = ALL '+'                                  
034100         PERFORM GA-KOLLA-LANDINGCOST                                     
034200       END-IF                                                             
034300*      IF MID-TISTADAT-KUND-NEW NOT = ALL '+'                             
034400*        PERFORM GB-KOLLA-STARTDATUM                                      
034500*      END-IF                                                             
034600*      IF MID-TISTODAT-KUND-NEW NOT = ALL '+'                             
034700*        PERFORM GC-KOLLA-STOPPDATUM                                      
034800*      END-IF                                                             
034900                                                                          
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 GA-KOLLA-LANDINGCOST SECTION.                                            
035400                                                                          
035500     PERFORM IMS-GHU-WDB101                                               
035600     IF SEGMENT-FINNS                                                     
035700       MOVE MID-RELANDCO-NEW    TO DEC-IDFRIDATA                          
035800       MOVE 3                   TO DEC-KVHELTAL                           
035900       MOVE 2                   TO DEC-KVDECIMAL                          
036000       CALL WDECEDIT USING DEC-WDECAREA                                   
036100       IF DEC-KDSVAR-OK                                                   
036200         MOVE MFS-NUM-FAELT-RAETT      TO                                 
036300                                   MOD-RELANDCO-NEW-ATTR                  
036400         MOVE DEC-IDEDITDATA           TO W-RELANDCO                      
036500       ELSE                                                               
036600         MOVE NEJ                      TO INDATA-SW                       
036700         MOVE MFS-NUM-FAELT-FEL        TO MOD-RELANDCO-NEW-ATTR           
036800         MOVE MFS-ROER-EJ-FAELT        TO MOD-RELANDCO-NEW                
036900         MOVE ERR-NOT-NUMERIC          TO MED-IDMFSFEL                    
037000         CALL WMEDKONV USING MED-WMEDAREA                                 
037100         MOVE MED-MFSFEL               TO MOD-TEMFSFEL                    
037200       END-IF                                                             
037300     ELSE                                                                 
037400       MOVE INF-CUSTOMER-MISSING TO MED-IDMFSINF                          
037500       CALL WMEDKONV USING MED-WMEDAREA                                   
037600       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
037700       PERFORM MFS-RENSA-FAELT-IN                                         
037800       MOVE NEJ                 TO INDATA-SW                              
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200*                                                                         
038300*GB-KOLLA-STARTDATUM SECTION.                                             
038400*    IF MID-TISTADAT-KUND-NEW = ZERO                                      
038500*      MOVE MFS-NUM-FAELT-RAETT TO                                        
038600*                                   MOD-TISTADAT-KUND-NEW-ATTR            
038700*    ELSE                                                                 
038800*      MOVE 'AAMMDD'               TO  DAT-KDDATFORM                      
038900*      MOVE MID-TISTADAT-KUND-NEW  TO  DAT-I-TIDATUM                      
039000*      CALL WDATKONV USING             DAT-KDDATFORM                      
039100*                                      DAT-I-TIDATUM                      
039200*                                      DAT-O-TIDATUM                      
039300*                                      DAT-KDSVAR                         
039400*                                                                         
039500*      IF DAT-KDSVAR-FEL                                                  
039600*        MOVE NEJ               TO INDATA-SW                              
039700*        MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-KUND-NEW-ATTR             
039800*        MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-KUND-NEW                  
039900*        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                        
040000*        CALL WMEDKONV USING MED-WMEDAREA                                 
040100*        MOVE MED-MFSINF           TO MOD-TEMFSINF                        
040200*        PERFORM MFS-RENSA-FAELT-IN                                       
040300*      ELSE                                                               
040400*         MOVE MFS-NUM-FAELT-RAETT TO                                     
040500*                                   MOD-TISTADAT-KUND-NEW-ATTR            
040600*                                                                         
040700*      END-IF                                                             
040800*    END-IF                                                               
040900*                                                                         
041000*    .                                                                    
041100*    EJECT                                                                
041200*                                                                         
041300*GC-KOLLA-STOPPDATUM SECTION.                                             
041400*    IF MID-TISTODAT-KUND-NEW = ZERO                                      
041500*          MOVE MFS-NUM-FAELT-RAETT TO                                    
041600*                                    MOD-TISTODAT-KUND-NEW-ATTR           
041700*    ELSE                                                                 
041800*      MOVE 'AAMMDD'               TO  DAT-KDDATFORM                      
041900*      MOVE MID-TISTODAT-KUND-NEW  TO  DAT-I-TIDATUM                      
042000*      CALL WDATKONV USING             DAT-KDDATFORM                      
042100*                                      DAT-I-TIDATUM                      
042200*                                      DAT-O-TIDATUM                      
042300*                                      DAT-KDSVAR                         
042400*                                                                         
042500*      IF DAT-KDSVAR-FEL                                                  
042600*        MOVE NEJ               TO INDATA-SW                              
042700*        MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-KUND-NEW-ATTR             
042800*        MOVE MFS-ROER-EJ-FAELT TO MOD-TISTODAT-KUND-NEW                  
042900*        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                        
043000*        CALL WMEDKONV USING MED-WMEDAREA                                 
043100*        MOVE MED-MFSINF           TO MOD-TEMFSINF                        
043200*        PERFORM MFS-RENSA-FAELT-IN                                       
043300*      ELSE                                                               
043400*          MOVE MFS-NUM-FAELT-RAETT TO                                    
043500*                                    MOD-TISTODAT-KUND-NEW-ATTR           
043600*                                                                         
043700*      END-IF                                                             
043800*    END-IF                                                               
043900*    .                                                                    
044000*    EJECT                                                                
044100 H-UPPDATERA-KUNDREG SECTION.                                             
044200                                                                          
044300     PERFORM IMS-GHU-WDB101                                               
044400     IF SEGMENT-FINNS                                                     
044500       IF MID-RELANDCO-NEW     NOT = ALL '+'                              
044600         MOVE W-RELANDCO          TO BET-RELANDCO                         
044700         MOVE MFS-RENSA-FAELT     TO MOD-RELANDCO-NEW                     
044800       END-IF                                                             
044900*      IF MID-TISTADAT-KUND-NEW NOT = ALL '+'                             
045000*        MOVE MID-TISTADAT-KUND-NEW TO BET-TISTADAT-KUND                  
045100*        MOVE MFS-RENSA-FAELT     TO MOD-TISTADAT-KUND-NEW                
045200*      END-IF                                                             
045300*      IF MID-TISTODAT-KUND-NEW NOT = ALL '+'                             
045400*        MOVE MID-TISTODAT-KUND-NEW TO BET-TISTODAT-KUND                  
045500*        MOVE MFS-RENSA-FAELT     TO MOD-TISTODAT-KUND-NEW                
045600*      END-IF                                                             
045700                                                                          
045800       PERFORM IMS-REPL-WDB101                                            
045900                                                                          
046000       MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                         
046100       CALL WMEDKONV USING MED-WMEDAREA                                   
046200       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 MFS-RENSA-FAELT-IN SECTION.                                              
046700                                                                          
046800     MOVE MFS-RENSA-FAELT       TO MOD-BEBETRAD-1                         
046900                                   MOD-BEBETRAD-2                         
047000                                   MOD-ADBETRAD-1                         
047100                                   MOD-ADBETRAD-2                         
047200                                   MOD-RELANDCO-OLD                       
047300                                   MOD-RELANDCO-NEW                       
047400*                                  MOD-TISTADAT-KUND-OLD                  
047500*                                  MOD-TISTADAT-KUND-NEW                  
047600*                                  MOD-TISTODAT-KUND-OLD                  
047700*                                  MOD-TISTODAT-KUND-NEW                  
047800     .                                                                    
047900     EJECT                                                                
048000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
048100                                                                          
048200                                                                          
048300     MOVE MFS-ROER-EJ-FAELT     TO MOD-BEBETRAD-1                         
048400                                   MOD-BEBETRAD-2                         
048500                                   MOD-ADBETRAD-1                         
048600                                   MOD-ADBETRAD-2                         
048700                                   MOD-RELANDCO-OLD                       
048800                                   MOD-RELANDCO-NEW                       
048900*                                  MOD-TISTADAT-KUND-OLD                  
049000*                                  MOD-TISTADAT-KUND-NEW                  
049100*                                  MOD-TISTODAT-KUND-OLD                  
049200*                                  MOD-TISTODAT-KUND-NEW                  
049300     .                                                                    
049400     EJECT                                                                
049500* --- IMS SEKTIONER ---                                                   
049600                                                                          
049700 IMS-GET-MSG SECTION.                                                     
049800                                                                          
049900     MOVE '  QC' TO GODK-STATUSKODER                                      
050000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
050100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400                                                                          
050500     EJECT                                                                
050600 IMS-INSERT-MSG SECTION.                                                  
050700                                                                          
050800     IF ENGLISH-TEXT                                                      
050900       MOVE '0' TO MFS-KDHUVOMR                                           
051000     END-IF                                                               
051100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
051200     MOVE SPACE TO GODK-STATUSKODER                                       
051300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
051400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051500     PERFORM IMS-STATUSKONTROLL                                           
051600     .                                                                    
051700                                                                          
051800 IMS-GET-WDB101 SECTION.                                                  
051900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
052000          DELIMITED BY SIZE INTO SSA1                                     
052100     MOVE '  GE' TO GODK-STATUSKODER                                      
052200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA SSA1                      
052300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
052400     PERFORM IMS-STATUSKONTROLL                                           
052500     .                                                                    
052600                                                                          
052700 IMS-GHU-WDB101 SECTION.                                                  
052800     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
052900          DELIMITED BY SIZE INTO SSA1                                     
053000     MOVE '  GE' TO GODK-STATUSKODER                                      
053100     CALL CBLTDLI USING GHU WDB1-PCB DLI-IO-AREA SSA1                     
053200     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
053300     PERFORM IMS-STATUSKONTROLL                                           
053400     .                                                                    
053500                                                                          
053600 IMS-REPL-WDB101 SECTION.                                                 
053700                                                                          
053800     MOVE '  ' TO GODK-STATUSKODER                                        
053900     CALL CBLTDLI USING REPL WDB1-PCB DLI-IO-AREA                         
054000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
054100     PERFORM IMS-STATUSKONTROLL                                           
054200     .                                                                    
054300     EJECT                                                                
054400 IMS-STATUSKONTROLL SECTION.                                              
054500                                                                          
054600     SET STATUS-IX TO 1                                                   
054700     SEARCH GODK-STATUS                                                   
054800       AT END                                                             
054900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055000         DELIMITED BY SIZE INTO FELTEXT                                   
055100         CALL FELLOG                                                      
055200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055300         CONTINUE                                                         
055400     END-SEARCH                                                           
055500     .                                                                    
