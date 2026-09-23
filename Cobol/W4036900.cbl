000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036900.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   90/09/03.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT FRÅGE-/ÄNDRINGS-PROGRAM MOT ARBETS-            
001100*        TIDSTABELLEN (WL4437).                                           
001200*        I PROGRAMMET FINNS MÖJLIGHET ATT:                                
001300*        - SÖKA MED IDPRC ELLER BETRPDST.                                 
001400*        - ÄNDRA/UPPDATERA DAGLIGA ARBETSTIDER.                           
001500*                                                                         
001600*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001700*        PROGRAMMET UPPDATERAR WL4437 (WDR1)                              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T369                                              
002100*        MID:         W4I36901                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O36901                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003010     SKIP3                                                                
003011*    -- CHECKED BY WY2000                                                 
003020     SKIP3                                                                
003100 77  IDPGM                       PIC X(08)   VALUE 'W4036900'.            
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  WS-STAPAC                   PIC 9(4).                                
003500 77  WS-STOPAC                   PIC 9(4).                                
003600 77  WS-STAADM                   PIC 9(4).                                
003700 77  WS-STOADM                   PIC 9(4).                                
003800 77  WS-STALAST                  PIC 9(4).                                
003900 77  WS-STOLAST                  PIC 9(4).                                
004100 77  WS-IDPRC                    PIC X(04)   VALUE SPACE.                 
004300 77  WS-IDTIDZON                 PIC X(2).                                
004400                                                                          
004500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004600 77  INDX                        PIC S9(2)  VALUE +00   COMP SYNC.        
004700 77  MAX-INDX                    PIC S9(2)  VALUE +12   COMP SYNC.        
004800                                                                          
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +625 COMP SYNC.         
005010*                                                                         
005011 01  WS-DCUSER.                                                           
005012     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
005013     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
005014     03 FILLER                   PIC X(1)   VALUE SPACE.                  
005015*                                                                         
005016*      --- VALID IDDC CODES                                               
005017*                                                                         
005018*01    -COPY WWDC99                                                       
005019       EJECT                                                              
005200 01  WS-NUMEDIT                  PIC 99V99.                               
005300 01  FILLER      REDEFINES   WS-NUMEDIT.                                  
005400     03   WS-NUMFYRA             PIC 9(4).                                
005500                                                                          
005600 01  WS-DATUM-EDIT               PIC 9(7).                                
005700 01  FILLER      REDEFINES   WS-DATUM-EDIT.                               
005800     03   FILLER                 PIC 9.                                   
005900     03   WS-DATUM               PIC 9(6).                                
006000                                                                          
006100 01  WS-KONTROLLTID              PIC 99V99.                               
006200 01  WS-TID  REDEFINES   WS-KONTROLLTID.                                  
006300     03 WS-TIMMA                 PIC 99.                                  
006400        88 TIMMA-OK                     VALUE 00 THRU 23.                 
006500     03 WS-MINUT                 PIC 99.                                  
006600        88 MINUT-OK                     VALUE 00 THRU 59.                 
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
007300     88  UPDATE-OK                           VALUE 'J'.                   
007400                                                                          
007500 77  MESSAGE-SW                  PIC X       VALUE 'J'.                   
007600     88  KEYS-ARE-MISSING                    VALUE 'J'.                   
007700                                                                          
007800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007900     88  NYCKLAR-OK                          VALUE 'J'.                   
008000     88  NYCKLAR-FEL                         VALUE 'N'.                   
008100                                                                          
008200 77  NYA-NYCKLAR-SW              PIC X       VALUE 'J'.                   
008300     88  NYA-NYCKLAR                         VALUE 'J'.                   
008400                                                                          
008500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008600     88  ALLT-OK                             VALUE 'J'.                   
008700                                                                          
008800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008900     88  EGEN-MID                            VALUE '4369'.                
009000     88  GODK-MID           VALUE '4364' '4365' '4368' '4369'.            
009100     EJECT                                                                
009200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009300 01  GENERELLA-SUBPROGRAM.                                                
009400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009500     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010100*   -COPY WDECAREA                                                        
010200     EJECT                                                                
010300*   -COPY WMEDAREA                                                        
010400     EJECT                                                                
010530*                   ****    PARAMETRAR TILL W005INIT                      
010600*01  -COPY WMSGINIT                                                       
010700     EJECT                                                                
010701*01  -COPY WMSGINIT -PRE DC-                                              
010702     EJECT                                                                
010730     EJECT                                                                
010800 01  MESSAGE-CODES.                                                       
010900     03  ERR-UPDATE-FORBIDDEN         PIC X(3)    VALUE '007'.            
011000     03  ERR-KEYS-ARE-MISSING         PIC X(3)    VALUE '005'.            
011100     03  ERR-WRONG-KEY                PIC X(3)    VALUE '401'.            
011200     03  ERR-HIGHLITED-FIELDS-WRONG   PIC X(3)    VALUE '409'.            
011300     03  INF-PRESS-PF11               PIC X(3)    VALUE '003'.            
011400     03  INF-FIRST-PAGE               PIC X(3)    VALUE '006'.            
011500     03  INF-UPDATE-DONE              PIC X(3)    VALUE '101'.            
011600     03  INF-MORE-INFO-EXISTS         PIC X(3)    VALUE '105'.            
011700     03  INF-PF11-AND-NO-DATA         PIC X(3)    VALUE '414'.            
011800     EJECT                                                                
011900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012000*                                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012200     SKIP3                                                                
012300*01  MID -COPY W4I36901                                                   
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012600     SKIP3                                                                
012700*01  -COPY WMSGAREA                                                       
012800     EJECT                                                                
012900     03  MOD REDEFINES MSG-AREA.                                          
013000*      05  -COPY W4O36901                                                 
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013300     SKIP3                                                                
013400*01  -COPY WMFSAREA                                                       
013500     EJECT                                                                
013600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013900     SKIP3                                                                
014000 01  NYCKLAR-TILL-DLI.                                                    
014100     03  W-WDGXKEY-4437-X.                                                
014200         05  W-IDHTYP            PIC X(04)    VALUE '4437'.               
014300         05  W-IDDC              PIC X(2)     VALUE '00'.                 
014400         05  W-IDPRC             PIC X(04)    VALUE SPACE.                
014500         05  W-LOWVALUE          PIC X(20)    VALUE LOW-VALUE.            
014600     03  W-WDGXKEY-4438-X.                                                
014700         05  W-DADATUM           PIC 9(08)   VALUE 00000101.              
014900     03  W-WDGXKEY-4438-MAX-X.                                            
015000         05  W-DADATUM-MAX       PIC 9(08)   VALUE 99991231.              
015200*    --- STATUS-KOD FRÅN IMS                                              
015300 01  STATUS-WS                   PIC XX.                                  
015400     88  SEGMENT-FINNS                       VALUE '  '.                  
015500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016900     SKIP2                                                                
017000 01  DLI-IO-AREA.                                                         
017100     03  IO-AREA                 PIC X(30)   VALUE SPACE.                 
017200     SKIP2                                                                
017300*    03  WL443701  -COPY WDGX4437       -RED IO-AREA.                     
017400     EJECT                                                                
017500     SKIP2                                                                
017600*    03  WL443711  -COPY WDGX4438       -RED IO-AREA.                     
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900                                                                          
018000*01  -COPY W0009      -PRE MSG-                                           
018100     EJECT                                                                
018200*01  -COPY W0008     -PRE USEA-                                           
018300     05  FILLER              PIC X.                                       
018400     EJECT                                                                
018500*01  -COPY W0008      -PRE 4437-                                          
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
018900                                   4437-PCB.                              
019000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
019100                                   4437-PCB.                              
019200 STYR SECTION.                                                            
019300                                                                          
019400     PERFORM IMS-GET-MSG                                                  
019500     IF SEGMENT-FINNS                                                     
019600       PERFORM A-INIT                                                     
019700       PERFORM B-NYCKEL-KONTROLL                                          
019800       IF NYCKLAR-OK                                                      
019900         IF NYA-NYCKLAR                                                   
020000           MOVE JA TO ALLT-SW                                             
020100         ELSE                                                             
020200           IF MFS-UPDATE                                                  
020300             PERFORM C-INPUT-CONTROL                                      
020400             IF INDATA-OK                                                 
020500               PERFORM D-UPDATE                                           
020600             END-IF                                                       
020700           ELSE                                                           
020800             PERFORM E-MFS-TRANSTYPE-CONTROL                              
020900           END-IF                                                         
021000         END-IF                                                           
021100         IF ALLT-OK                                                       
021200           PERFORM F-READ-SHOW-INFO                                       
021300         END-IF                                                           
021400       END-IF                                                             
021500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
021600       PERFORM IMS-INSERT-MSG                                             
021700     END-IF                                                               
021800                                                                          
021900     MOVE ZERO TO RETURN-CODE                                             
022000     GOBACK                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 A-INIT SECTION.                                                          
022400                                                                          
022500     IF MSG-DUBBLA-TRANSKODER                                             
022600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36901                 
022700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022900     ELSE                                                                 
023000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36901                  
023100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023300     END-IF                                                               
023400                                                                          
023500     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
023600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
023700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
023800                                                                          
023900     MOVE LOW-VALUE TO MSG-AREA                                           
024000     MOVE 'W4O369N1' TO MFS-IDMOD                                         
024100     MOVE '4369' TO MOD-IDTRANS                                           
024200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024300                                                                          
024400     IF NOT EGEN-MID                                                      
024500       MOVE SPACE TO MFS-KDTRTYP                                          
024600       MOVE '7' TO MFS-IDPFK                                              
024700       MOVE JA                      TO NYA-NYCKLAR-SW                     
024800     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 B-NYCKEL-KONTROLL SECTION.                                               
026000                                                                          
026100                                                                          
026200       MOVE ALL '+'           TO MSGI-WMSGINIT                            
026300       MOVE '013'             TO MSGI-KDCALL                              
026400       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
026500       MOVE '4369'            TO MSGI-IDTRANS                             
026600       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
026700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
026710                                                                          
026720       IF MSGI-IDLAND-SPR = 'GB'                                          
026730         MOVE +2 TO SPRAK-IX                                              
026740         MOVE 'GB ' TO MED-IDSKYLT                                        
026750       ELSE                                                               
026760         MOVE +1 TO SPRAK-IX                                              
026770         MOVE 'S  ' TO MED-IDSKYLT                                        
026780       END-IF                                                             
026800                                                                          
026810       MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                              
026900       MOVE JA                TO NYCKLAR-SW ALLT-SW                       
027000                                 NYA-NYCKLAR-SW                           
027100       MOVE MFS-RENSA-FAELT TO MOD-IDPRC-IN MOD-DATUM-IN                  
027200                               MOD-IDDC-IN                                
027300                                                                          
027400       IF MID-IDPRC-IN = ALL '+'                                          
027500         MOVE MID-IDPRC-UT TO WS-IDPRC                                    
027600         MOVE NEJ                     TO NYA-NYCKLAR-SW                   
027700       ELSE                                                               
027800         MOVE MID-IDPRC-IN TO WS-IDPRC                                    
027900         MOVE '7'                     TO MFS-IDPFK                        
028000         MOVE SPACE                   TO MFS-KDTRTYP                      
028100       END-IF                                                             
028200                                                                          
028300       IF WS-IDPRC (1:3) NUMERIC                                          
028400          IF WS-IDPRC (1:3) > ZERO                                        
028500             IF WS-IDPRC (4:1) NOT = SPACE                                
028600                INSPECT WS-IDPRC REPLACING LEADING ZERO BY SPACE          
028700             ELSE                                                         
028800                MOVE NEJ              TO NYCKLAR-SW                       
028900             END-IF                                                       
029000          ELSE                                                            
029100             MOVE NEJ                 TO NYCKLAR-SW                       
029200          END-IF                                                          
029300       ELSE                                                               
029400          MOVE NEJ                    TO NYCKLAR-SW                       
029500       END-IF                                                             
029600                                                                          
029700       MOVE MSGI-IDDC               TO WS-IDDC                            
029800                                                                          
029900       IF WS-IDDC IS > SPACE                                              
030000         CONTINUE                                                         
030100       ELSE                                                               
030200         MOVE NEJ                     TO NYCKLAR-SW                       
030300       END-IF                                                             
030400                                                                          
030500       IF NYCKLAR-OK                                                      
030600         MOVE WS-IDDC                 TO W-IDDC                           
030700       END-IF                                                             
030800                                                                          
030900       IF MID-DATUM-IN = ALL '+'                                          
031000         MOVE MID-DATUM-UT            TO WS-DATUM                         
031100         MOVE NEJ                     TO NYA-NYCKLAR-SW                   
031200       ELSE                                                               
031300         MOVE MID-DATUM-IN            TO WS-DATUM                         
031400         MOVE '7'                     TO MFS-IDPFK                        
031500         MOVE SPACE                   TO MFS-KDTRTYP                      
031600       END-IF                                                             
031700       INSPECT WS-DATUM REPLACING ALL SPACE BY ZERO                       
031800                                                                          
031900       IF WS-DATUM = ZERO                                                 
031910         MOVE ALL '+'           TO DC-MSGI-WMSGINIT                       
031920         MOVE '013'             TO DC-MSGI-KDCALL                         
031930         MOVE WS-IDDC           TO WS-DCUSER-IDDC                         
031940         MOVE WS-DCUSER         TO DC-MSGI-IDUSER                         
031950         MOVE '4369'            TO DC-MSGI-IDTRANS                        
031960         MOVE MSG-LTERM-NAME    TO DC-MSGI-IDLTERM-USER                   
031970         CALL W005INIT USING DC-MSGI-WMSGINIT USEA-PCB                    
031980         MOVE DC-MSGI-TILOKDAT  TO WS-DATUM                               
032100       ELSE                                                               
032200         IF WS-DATUM NUMERIC AND WS-DATUM > ZERO                          
032600           MOVE WS-DATUM              TO W-DADATUM                        
032610           IF WS-DATUM < 500000                                           
032620             MOVE 20                  TO W-DADATUM (1:2)                  
032630           ELSE                                                           
032640             IF WS-DATUM < 999999                                         
032650               MOVE 19                TO W-DADATUM (1:2)                  
032660             ELSE                                                         
032670               MOVE 99999999          TO W-DADATUM                        
032680             END-IF                                                       
032690           END-IF                                                         
032700         ELSE                                                             
032800           IF NOT EGEN-MID                                                
032900             ACCEPT WS-DATUM FROM DATE                                    
033000           ELSE                                                           
033100             MOVE NEJ                 TO NYCKLAR-SW                       
033200           END-IF                                                         
033300         END-IF                                                           
033400       END-IF                                                             
033500                                                                          
033600       IF WS-IDPRC = ALL '+' OR  WS-DATUM = ALL '+'                       
033700         MOVE MFS-RENSA-FAELT         TO MOD-IDPRC-UT                     
033800                                         MOD-DATUM-UT                     
033900                                         MOD-IDDC-UT                      
034000       END-IF                                                             
034100                                                                          
034200     IF GODK-MID                                                          
034300       MOVE WS-IDPRC                  TO MOD-IDPRC-UT W-IDPRC             
034400       MOVE WS-DATUM                  TO W-DADATUM                        
034600                                         MOD-DATUM-UT                     
034610       IF WS-DATUM NOT = ZERO                                             
034620         IF WS-DATUM < 500000                                             
034630           MOVE 20                    TO W-DADATUM (1:2)                  
034640         ELSE                                                             
034650           IF WS-DATUM < 999999                                           
034660             MOVE 19                  TO W-DADATUM (1:2)                  
034670           ELSE                                                           
034680             MOVE 99999999            TO W-DADATUM                        
034690           END-IF                                                         
034691         END-IF                                                           
034692       END-IF                                                             
034700                                                                          
034800       MOVE WS-IDDC                   TO MOD-IDDC-UT                      
035000     ELSE                                                                 
035100       MOVE MFS-RENSA-FAELT           TO MOD-IDPRC-UT                     
035200                                         MOD-DATUM-UT                     
035300                                         MOD-IDDC-UT                      
035400     END-IF                                                               
035500                                                                          
035600     IF NYCKLAR-FEL                                                       
035700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
035800       PERFORM S01-ERR-RUTINE                                             
035900       PERFORM MFS-RENSA-FAELT-IN                                         
036000       PERFORM MFS-RENSA-FAELT-UT                                         
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 C-INPUT-CONTROL SECTION.                                                 
036500                                                                          
036600     MOVE JA  TO INDATA-SW                                                
036700     IF MID-RAD = ALL '+'                                                 
036800       MOVE INF-PF11-AND-NO-DATA TO MED-IDMFSINF                          
036900       PERFORM S02-INF-RUTINE                                             
037000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
037100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
037200       MOVE NEJ TO INDATA-SW ALLT-SW                                      
037300     ELSE                                                                 
037400       IF MID-DATUM = ALL '+'                                             
037500        MOVE NEJ                      TO INDATA-SW                        
037600       ELSE                                                               
037700         MOVE MID-DATUM               TO W-DADATUM                        
037701         IF MID-DATUM NOT = ZERO                                          
037702           IF MID-DATUM < 500000                                          
037703             MOVE 20                  TO W-DADATUM (1:2)                  
037704           ELSE                                                           
037705             IF MID-DATUM < 999999                                        
037706               MOVE 19                TO W-DADATUM (1:2)                  
037707             ELSE                                                         
037708               MOVE 99999999          TO W-DADATUM                        
037709             END-IF                                                       
037710           END-IF                                                         
037711         END-IF                                                           
037720                                                                          
037800         PERFORM IMS-GU-WL443711                                          
037900         IF SEGMENT-FINNS                                                 
038000           PERFORM CA-CONTROL-STAPAC                                      
038100           PERFORM CB-CONTROL-STOPAC                                      
038200           PERFORM CC-CONTROL-STAADM                                      
038300           PERFORM CD-CONTROL-STOADM                                      
038400           PERFORM CE-CONTROL-STALAST                                     
038500           PERFORM CF-CONTROL-STOLAST                                     
038600                                                                          
038700           IF INDATA-FEL                                                  
038800             MOVE MFS-NUM-FAELT-RAETT TO MOD-DATUM-ATTR                   
038900             MOVE NEJ TO ALLT-SW                                          
039000             MOVE ERR-HIGHLITED-FIELDS-WRONG TO MED-IDMFSFEL              
039100             PERFORM S01-ERR-RUTINE                                       
039200             PERFORM MFS-ROER-EJ-FAELT-IN                                 
039300             PERFORM MFS-ROER-EJ-FAELT-UT                                 
039400           END-IF                                                         
039500         ELSE                                                             
039600           MOVE NEJ TO INDATA-SW ALLT-SW                                  
039700           MOVE ERR-UPDATE-FORBIDDEN TO MED-IDMFSFEL                      
039800           CALL WMEDKONV USING MED-WMEDAREA                               
039900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
040000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
040100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
040200         END-IF                                                           
040300       END-IF                                                             
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 CA-CONTROL-STAPAC SECTION.                                               
040800                                                                          
040900     IF MID-STAPAC-IN = ALL '+'                                           
041000       CONTINUE                                                           
041100     ELSE                                                                 
041200         MOVE MID-STAPAC-IN           TO DEC-IDFRIDATA                    
041300         PERFORM S03-FORMATERING-MED-RDECDATA                             
041400         IF DEC-KDSVAR-OK                                                 
041500           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID                   
041600           IF TIMMA-OK AND MINUT-OK                                       
041700             MOVE MFS-NUM-FAELT-RAETT TO MOD-STAPAC-IN-ATTR               
041800             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
041900             MOVE WS-NUMFYRA          TO WS-STAPAC                        
042000           ELSE                                                           
042100             MOVE MFS-NUM-FAELT-FEL   TO MOD-STAPAC-IN-ATTR               
042200             MOVE NEJ                 TO INDATA-SW                        
042300           END-IF                                                         
042400         ELSE                                                             
042500           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAPAC-IN-ATTR               
042600           MOVE NEJ                   TO INDATA-SW                        
042700         END-IF                                                           
042710                                                                          
042800     END-IF                                                               
042900     .                                                                    
043000 CB-CONTROL-STOPAC SECTION.                                               
043100                                                                          
043200     IF MID-STOPAC-IN = ALL '+'                                           
043300       CONTINUE                                                           
043400     ELSE                                                                 
043500         MOVE MID-STOPAC-IN           TO DEC-IDFRIDATA                    
043600         PERFORM S03-FORMATERING-MED-RDECDATA                             
043700         IF DEC-KDSVAR-OK                                                 
043800           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID                   
043900           IF TIMMA-OK AND MINUT-OK                                       
044000             MOVE MFS-NUM-FAELT-RAETT TO MOD-STOPAC-IN-ATTR               
044100             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
044200             MOVE WS-NUMFYRA          TO WS-STOPAC                        
044300           ELSE                                                           
044400             MOVE MFS-NUM-FAELT-FEL   TO MOD-STOPAC-IN-ATTR               
044500             MOVE NEJ                 TO INDATA-SW                        
044600           END-IF                                                         
044700         ELSE                                                             
044800           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOPAC-IN-ATTR               
044900           MOVE NEJ                   TO INDATA-SW                        
045000         END-IF                                                           
045001                                                                          
045100     END-IF                                                               
045200     .                                                                    
045300 CC-CONTROL-STAADM SECTION.                                               
045400                                                                          
045500     IF MID-STAADM-IN = ALL '+'                                           
045600       CONTINUE                                                           
045700     ELSE                                                                 
045800         MOVE MID-STAADM-IN           TO DEC-IDFRIDATA                    
045900         PERFORM S03-FORMATERING-MED-RDECDATA                             
046000         IF DEC-KDSVAR-OK                                                 
046100           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID                   
046200           IF TIMMA-OK AND MINUT-OK                                       
046300             MOVE MFS-NUM-FAELT-RAETT TO MOD-STAADM-IN-ATTR               
046400             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
046500             MOVE WS-NUMFYRA          TO WS-STAADM                        
046600           ELSE                                                           
046700             MOVE MFS-NUM-FAELT-FEL   TO MOD-STAADM-IN-ATTR               
046800             MOVE NEJ                 TO INDATA-SW                        
046900           END-IF                                                         
047000         ELSE                                                             
047100           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAADM-IN-ATTR               
047200           MOVE NEJ                   TO INDATA-SW                        
047300         END-IF                                                           
047310                                                                          
047400     END-IF                                                               
047500     .                                                                    
047600 CD-CONTROL-STOADM SECTION.                                               
047700                                                                          
047800     IF MID-STOADM-IN = ALL '+'                                           
047900       CONTINUE                                                           
048000     ELSE                                                                 
048100         MOVE MID-STOADM-IN           TO DEC-IDFRIDATA                    
048200         PERFORM S03-FORMATERING-MED-RDECDATA                             
048300         IF DEC-KDSVAR-OK                                                 
048400           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID                   
048500           IF TIMMA-OK AND MINUT-OK                                       
048600             MOVE MFS-NUM-FAELT-RAETT TO MOD-STOADM-IN-ATTR               
048700             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
048800             MOVE WS-NUMFYRA          TO WS-STOADM                        
048900           ELSE                                                           
049000             MOVE MFS-NUM-FAELT-FEL   TO MOD-STOADM-IN-ATTR               
049100             MOVE NEJ                 TO INDATA-SW                        
049200           END-IF                                                         
049300         ELSE                                                             
049400           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOADM-IN-ATTR               
049500           MOVE NEJ                   TO INDATA-SW                        
049600         END-IF                                                           
049610                                                                          
049700     END-IF                                                               
049800     .                                                                    
049900 CE-CONTROL-STALAST SECTION.                                              
050000                                                                          
050100     IF MID-STALAST-IN = ALL '+'                                          
050200       CONTINUE                                                           
050300     ELSE                                                                 
050400         MOVE MID-STALAST-IN          TO DEC-IDFRIDATA                    
050500         PERFORM S03-FORMATERING-MED-RDECDATA                             
050600         IF DEC-KDSVAR-OK                                                 
050700           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID                   
050800           IF TIMMA-OK AND MINUT-OK                                       
050900            MOVE MFS-NUM-FAELT-RAETT TO MOD-STALAST-IN-ATTR               
051000             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
051100             MOVE WS-NUMFYRA          TO WS-STALAST                       
051200           ELSE                                                           
051300             MOVE MFS-NUM-FAELT-FEL   TO MOD-STALAST-IN-ATTR              
051400             MOVE NEJ                 TO INDATA-SW                        
051500           END-IF                                                         
051600         ELSE                                                             
051700           MOVE MFS-NUM-FAELT-FEL     TO MOD-STALAST-IN-ATTR              
051800           MOVE NEJ                   TO INDATA-SW                        
051900         END-IF                                                           
051910                                                                          
052000     END-IF                                                               
052100     .                                                                    
052200 CF-CONTROL-STOLAST SECTION.                                              
052300                                                                          
052400     IF MID-STOLAST-IN = ALL '+'                                          
052500       CONTINUE                                                           
052600     ELSE                                                                 
052700         MOVE MID-STOLAST-IN            TO DEC-IDFRIDATA                  
052800         PERFORM S03-FORMATERING-MED-RDECDATA                             
052900         IF DEC-KDSVAR-OK                                                 
053000           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID                   
053100           IF TIMMA-OK AND MINUT-OK                                       
053200            MOVE MFS-NUM-FAELT-RAETT TO MOD-STOLAST-IN-ATTR               
053300             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
053400             MOVE WS-NUMFYRA          TO WS-STOLAST                       
053500           ELSE                                                           
053600             MOVE MFS-NUM-FAELT-FEL   TO MOD-STOLAST-IN-ATTR              
053700             MOVE NEJ                 TO INDATA-SW                        
053800           END-IF                                                         
053900         ELSE                                                             
054000           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOLAST-IN-ATTR              
054100           MOVE NEJ                   TO INDATA-SW                        
054200         END-IF                                                           
054210                                                                          
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600 D-UPDATE SECTION.                                                        
054700                                                                          
054800       PERFORM IMS-GHU-WL443711                                           
054900       PERFORM DA-CONTROL-OF-WS-FIELDS                                    
055000       IF INDATA-OK                                                       
055100         PERFORM DB-MOVE-WS-FIELDS-TO-IO-AREA                             
055200         PERFORM IMS-REPL-WL443711                                        
055300         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
055400         PERFORM S02-INF-RUTINE                                           
055500         PERFORM MFS-FORM-ATTR                                            
055600         PERFORM MFS-RENSA-FAELT-IN                                       
055700       ELSE                                                               
055800         MOVE NEJ TO ALLT-SW                                              
055900         MOVE ERR-HIGHLITED-FIELDS-WRONG TO MED-IDMFSFEL                  
056000         PERFORM S01-ERR-RUTINE                                           
056100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
056200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
056300       END-IF                                                             
056400     .                                                                    
056500     EJECT                                                                
056600 DA-CONTROL-OF-WS-FIELDS SECTION.                                         
056700                                                                          
056800     IF MID-STAPAC-IN = ALL '+' AND MID-STOPAC-IN = ALL '+'               
056900       CONTINUE                                                           
057000     ELSE                                                                 
057100       IF MID-STAPAC-IN = ALL '+'                                         
057200         MOVE 4438-TISTAMIN-PAC       TO WS-STAPAC                        
057300       END-IF                                                             
057400       IF MID-STOPAC-IN = ALL '+'                                         
057500         MOVE 4438-TISTOMIN-PAC       TO WS-STOPAC                        
057600       END-IF                                                             
057700     END-IF                                                               
057800                                                                          
057900     IF  WS-STOPAC  IS GREATER THAN  WS-STAPAC                            
058000       CONTINUE                                                           
058100     ELSE                                                                 
058200       IF (WS-STOPAC < +0.1) AND (WS-STAPAC < + 0.1)                      
058300         CONTINUE                                                         
058400       ELSE                                                               
058500         IF (MID-STAPAC-IN NOT = ALL '+') AND                             
058600            (MID-STOPAC-IN NOT = ALL '+')                                 
058700           MOVE NEJ                   TO INDATA-SW                        
058800           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAPAC-IN-ATTR               
058900                                         MOD-STOPAC-IN-ATTR               
059000         ELSE                                                             
059100           IF MID-STAPAC-IN = ALL '+'                                     
059200             MOVE NEJ                 TO INDATA-SW                        
059300             MOVE MFS-NUM-FAELT-FEL   TO MOD-STOPAC-IN-ATTR               
059400           END-IF                                                         
059500           IF MID-STOPAC-IN = ALL '+'                                     
059600             MOVE NEJ                 TO INDATA-SW                        
059700             MOVE MFS-NUM-FAELT-FEL   TO MOD-STAPAC-IN-ATTR               
059800           END-IF                                                         
059900         END-IF                                                           
060000       END-IF                                                             
060100     END-IF                                                               
060200                                                                          
060300     IF MID-STAADM-IN = ALL '+' AND MID-STOADM-IN = ALL '+'               
060400       CONTINUE                                                           
060500     ELSE                                                                 
060600       IF MID-STAADM-IN = ALL '+'                                         
060700         MOVE 4438-TISTAMIN-ADM       TO WS-STAADM                        
060800       END-IF                                                             
060900       IF MID-STOADM-IN = ALL '+'                                         
061000         MOVE 4438-TISTOMIN-ADM       TO WS-STOADM                        
061100       END-IF                                                             
061200     END-IF                                                               
061300                                                                          
061400     IF WS-STOADM > WS-STAADM                                             
061500       CONTINUE                                                           
061600     ELSE                                                                 
061700       IF WS-STOADM < +0.1 AND                                            
061800          WS-STAADM < +0.1                                                
061900         CONTINUE                                                         
062000       ELSE                                                               
062100         IF (MID-STAADM-IN NOT = ALL '+') AND                             
062200            (MID-STOADM-IN NOT = ALL '+')                                 
062300           MOVE NEJ                   TO INDATA-SW                        
062400           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAADM-IN-ATTR               
062500                                         MOD-STOADM-IN-ATTR               
062600         ELSE                                                             
062700           IF MID-STAADM-IN = ALL '+'                                     
062800             MOVE NEJ               TO INDATA-SW                          
062900             MOVE MFS-NUM-FAELT-FEL TO MOD-STOADM-IN-ATTR                 
063000           END-IF                                                         
063100           IF MID-STOADM-IN = ALL '+'                                     
063200             MOVE NEJ               TO INDATA-SW                          
063300             MOVE MFS-NUM-FAELT-FEL TO MOD-STAADM-IN-ATTR                 
063400           END-IF                                                         
063500         END-IF                                                           
063600       END-IF                                                             
063700     END-IF                                                               
063800                                                                          
063900     IF MID-STALAST-IN = ALL '+' AND MID-STOLAST-IN = ALL '+'             
064000       CONTINUE                                                           
064100     ELSE                                                                 
064200       IF MID-STALAST-IN = ALL '+'                                        
064300         MOVE 4438-TISTAMIN-LAST       TO WS-STALAST                      
064400       END-IF                                                             
064500       IF MID-STOLAST-IN = ALL '+'                                        
064600         MOVE 4438-TISTOMIN-LAST       TO WS-STOLAST                      
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     IF WS-STOLAST > WS-STALAST                                           
065100       CONTINUE                                                           
065200     ELSE                                                                 
065300       IF WS-STOLAST < +0.1 AND                                           
065400          WS-STALAST < +0.1                                               
065500         CONTINUE                                                         
065600       ELSE                                                               
065700         IF (MID-STALAST-IN NOT = ALL '+') AND                            
065800            (MID-STOLAST-IN NOT = ALL '+')                                
065900           MOVE NEJ                   TO INDATA-SW                        
066000           MOVE MFS-NUM-FAELT-FEL     TO MOD-STALAST-IN-ATTR              
066100                                         MOD-STOLAST-IN-ATTR              
066200         ELSE                                                             
066300           IF MID-STALAST-IN = ALL '+'                                    
066400             MOVE NEJ               TO INDATA-SW                          
066500             MOVE MFS-NUM-FAELT-FEL TO MOD-STOLAST-IN-ATTR                
066600           END-IF                                                         
066700           IF MID-STOLAST-IN = ALL '+'                                    
066800             MOVE NEJ               TO INDATA-SW                          
066900             MOVE MFS-NUM-FAELT-FEL TO MOD-STALAST-IN-ATTR                
067000           END-IF                                                         
067100         END-IF                                                           
067200       END-IF                                                             
067300     END-IF                                                               
067400     .                                                                    
067500     EJECT                                                                
067600 DB-MOVE-WS-FIELDS-TO-IO-AREA SECTION.                                    
067700                                                                          
067800     IF MID-STAPAC-IN NOT = ALL '+'                                       
067900       MOVE WS-STAPAC                TO MOD-STAPAC-IN                     
068000                                        4438-TISTAMIN-PAC                 
068100     END-IF                                                               
068200     IF MID-STOPAC-IN NOT = ALL '+'                                       
068300       MOVE WS-STOPAC                TO MOD-STOPAC-IN                     
068400                                        4438-TISTOMIN-PAC                 
068500     END-IF                                                               
068600     IF MID-STAADM-IN NOT = ALL '+'                                       
068700       MOVE WS-STAADM                 TO 4438-TISTAMIN-ADM                
068800                                        MOD-STAADM-IN                     
068900     END-IF                                                               
069000     IF MID-STOADM-IN NOT = ALL '+'                                       
069100       MOVE WS-STOADM                TO MOD-STOADM-IN                     
069200                                        4438-TISTOMIN-ADM                 
069300     END-IF                                                               
069400     IF MID-STALAST-IN NOT = ALL '+'                                      
069500       MOVE WS-STALAST               TO MOD-STALAST-IN                    
069600                                        4438-TISTAMIN-LAST                
069700     END-IF                                                               
069800     IF MID-STOLAST-IN NOT = ALL '+'                                      
069900       MOVE WS-STOLAST               TO MOD-STOLAST-IN                    
070000                                        4438-TISTOMIN-LAST                
070100     END-IF                                                               
070200       .                                                                  
070300 E-MFS-TRANSTYPE-CONTROL SECTION.                                         
070400     IF EGEN-MID                                                          
070500       IF MID-RAD = ALL '+'                                               
070600         IF MFS-FIRST                                                     
070700           PERFORM EA-FIRST-SIDE                                          
070800         ELSE                                                             
070900           IF MFS-NEXT                                                    
071000             PERFORM EB-NEXT-SIDE                                         
071100           ELSE                                                           
071200             PERFORM EC-ENTER-SIDE                                        
071300           END-IF                                                         
071400         END-IF                                                           
071500       ELSE                                                               
071600         MOVE NEJ TO ALLT-SW                                              
071700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
071800         PERFORM S02-INF-RUTINE                                           
071900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
072000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
072100         PERFORM MFS-LAS-IN-IGEN                                          
072200       END-IF                                                             
072300     ELSE                                                                 
072400       PERFORM EA-FIRST-SIDE                                              
072500     END-IF                                                               
072600     .                                                                    
072700 EA-FIRST-SIDE SECTION.                                                   
072800                                                                          
072900     MOVE WS-DATUM                    TO W-DADATUM                        
072910     IF WS-DATUM NOT = ZERO                                               
072920       IF WS-DATUM < 500000                                               
072930         MOVE 20                      TO W-DADATUM (1:2)                  
072940       ELSE                                                               
072950         IF WS-DATUM < 999999                                             
072960           MOVE 19                    TO W-DADATUM (1:2)                  
072970         ELSE                                                             
072980           MOVE 99999999              TO W-DADATUM                        
072990         END-IF                                                           
072991       END-IF                                                             
072992     END-IF                                                               
072993                                                                          
073000     MOVE JA                          TO ALLT-SW                          
073100     .                                                                    
073200     EJECT                                                                
073300 EB-NEXT-SIDE SECTION.                                                    
073400                                                                          
073500     MOVE MID-DATUM-NEXT              TO W-DADATUM                        
073510     IF MID-DATUM-NEXT NOT = ZERO                                         
073520       IF MID-DATUM-NEXT < 500000                                         
073530         MOVE 20                      TO W-DADATUM (1:2)                  
073540       ELSE                                                               
073550         IF MID-DATUM-NEXT < 999999                                       
073560           MOVE 19                    TO W-DADATUM (1:2)                  
073570         ELSE                                                             
073580           MOVE 99999999              TO W-DADATUM                        
073590         END-IF                                                           
073591       END-IF                                                             
073592     END-IF                                                               
073600     MOVE JA                          TO ALLT-SW                          
073700     .                                                                    
073800 EC-ENTER-SIDE SECTION.                                                   
073900                                                                          
074000     MOVE MID-DATUM-ENTER             TO W-DADATUM                        
074010     IF MID-DATUM-ENTER NOT = ZERO                                        
074020       IF MID-DATUM-ENTER < 500000                                        
074030         MOVE 20                      TO W-DADATUM (1:2)                  
074040       ELSE                                                               
074050         IF MID-DATUM-ENTER < 999999                                      
074060           MOVE 19                    TO W-DADATUM (1:2)                  
074070         ELSE                                                             
074080           MOVE 99999999              TO W-DADATUM                        
074090         END-IF                                                           
074091       END-IF                                                             
074092     END-IF                                                               
074100       MOVE JA                        TO ALLT-SW                          
074200     .                                                                    
074300     EJECT                                                                
074400 F-READ-SHOW-INFO SECTION.                                                
074500                                                                          
074600     PERFORM IMS-GU-WL443701                                              
074700                                                                          
074800     IF SEGMENT-SAKNAS                                                    
074900        MOVE ERR-KEYS-ARE-MISSING     TO MED-IDMFSFEL                     
075000        PERFORM S01-ERR-RUTINE                                            
075100        PERFORM MFS-RENSA-FAELT-UT                                        
075200     ELSE                                                                 
075300       MOVE +1 TO INDX                                                    
075400         PERFORM IMS-GNP-F-WL443711                                       
075500       IF SEGMENT-FINNS                                                   
075600         MOVE 4438-DADATUM (3:6)      TO MOD-DATUM-ENTER                  
075700         MOVE NEJ                     TO MESSAGE-SW                       
075800       ELSE                                                               
075900         MOVE ZERO                    TO MOD-DATUM-ENTER                  
076000       END-IF                                                             
076100                                                                          
076200       PERFORM UNTIL INDX > MAX-INDX                                      
076300         IF SEGMENT-FINNS                                                 
076400           PERFORM FA-MOVE-RADDATA-TO-MOD                                 
076500           PERFORM IMS-GNP-MIN-MAX-WL443711                               
076600         ELSE                                                             
076700           PERFORM FB-MOVE-RENSA-FAELT-TO-MOD                             
076800         END-IF                                                           
076900         ADD 1 TO INDX                                                    
077000       END-PERFORM                                                        
077100                                                                          
077200       IF SEGMENT-FINNS                                                   
077300         MOVE 4438-DADATUM (3:6)      TO MOD-DATUM-NEXT                   
077400         IF MFS-IDPFK = 7                                                 
077500           MOVE INF-FIRST-PAGE        TO MED-IDMFSFEL                     
077600           PERFORM S01-ERR-RUTINE                                         
077700         ELSE                                                             
077800           MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                     
077900           PERFORM S02-INF-RUTINE                                         
078000         END-IF                                                           
078100       ELSE                                                               
078200         MOVE MOD-DATUM-ENTER         TO MOD-DATUM-NEXT                   
078300         IF KEYS-ARE-MISSING                                              
078400           MOVE ERR-KEYS-ARE-MISSING  TO MED-IDMFSFEL                     
078500           PERFORM S01-ERR-RUTINE                                         
078600         END-IF                                                           
078700       END-IF                                                             
078800                                                                          
078900     END-IF                                                               
079000     PERFORM MFS-RENSA-FAELT-IN                                           
079100     .                                                                    
079200     EJECT                                                                
079300 FA-MOVE-RADDATA-TO-MOD SECTION.                                          
079400                                                                          
079900     MOVE 4438-DADATUM (3:6)          TO MOD-DATUM-RAD(INDX)              
080000                                                                          
080100     MOVE 4438-TISTAMIN-PAC           TO WS-NUMFYRA                       
080300     MOVE WS-NUMEDIT                  TO MOD-STAPAC-RAD(INDX)             
080400*                                                                         
080500     MOVE 4438-TISTOMIN-PAC           TO WS-NUMFYRA                       
080700     MOVE WS-NUMEDIT                  TO MOD-STOPAC-RAD(INDX)             
080800*                                                                         
080900     MOVE 4438-TISTAMIN-ADM           TO WS-NUMFYRA                       
081100     MOVE WS-NUMEDIT                  TO MOD-STAADM-RAD(INDX)             
081200*                                                                         
081300     MOVE 4438-TISTOMIN-ADM           TO WS-NUMFYRA                       
081500     MOVE WS-NUMEDIT                  TO MOD-STOADM-RAD(INDX)             
081600*                                                                         
081700     MOVE 4438-TISTAMIN-LAST          TO WS-NUMFYRA                       
081900     MOVE WS-NUMEDIT                  TO MOD-STALAST-RAD(INDX)            
082000*                                                                         
082100     MOVE 4438-TISTOMIN-LAST          TO WS-NUMFYRA                       
082300     MOVE WS-NUMEDIT                  TO MOD-STOLAST-RAD(INDX)            
082400     .                                                                    
082500     EJECT                                                                
082600 FB-MOVE-RENSA-FAELT-TO-MOD SECTION.                                      
082700     MOVE MFS-RENSA-FAELT             TO MOD-DATUM-RAD(INDX)              
082800                                         MOD-STAPAC-RAD(INDX)             
082900                                         MOD-STOPAC-RAD(INDX)             
083000                                         MOD-STAADM-RAD(INDX)             
083100                                         MOD-STOADM-RAD(INDX)             
083200                                         MOD-STALAST-RAD(INDX)            
083300                                         MOD-STOLAST-RAD(INDX)            
083400     .                                                                    
083500     EJECT                                                                
083600 S01-ERR-RUTINE SECTION.                                                  
083700     CALL WMEDKONV USING MED-WMEDAREA                                     
083800     MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                     
083900     .                                                                    
084000 S02-INF-RUTINE SECTION.                                                  
084100     CALL WMEDKONV USING MED-WMEDAREA                                     
084200     MOVE MED-MFSINF                  TO MOD-TEMFSINF                     
084300     .                                                                    
084400 S03-FORMATERING-MED-RDECDATA SECTION.                                    
084500     MOVE +2                          TO DEC-KVHELTAL                     
084600     MOVE +2                          TO DEC-KVDECIMAL                    
084700     CALL WDECEDIT USING DEC-WDECAREA                                     
084800     .                                                                    
086070     SKIP2                                                                
086800 MFS-RENSA-FAELT-UT SECTION.                                              
086900                                                                          
087000*    --- ALLA UTDATA-FÄLT                                                 
087100     MOVE +1                          TO INDX                             
087200     PERFORM UNTIL INDX > MAX-INDX                                        
087300       MOVE MFS-RENSA-FAELT           TO MOD-DATUM-RAD(INDX)              
087400                                         MOD-STAPAC-RAD(INDX)             
087500                                         MOD-STOPAC-RAD(INDX)             
087600                                         MOD-STAADM-RAD(INDX)             
087700                                         MOD-STOADM-RAD(INDX)             
087800                                         MOD-STALAST-RAD(INDX)            
087900                                         MOD-STOLAST-RAD(INDX)            
088000     ADD +1                           TO INDX                             
088100     END-PERFORM                                                          
088200     .                                                                    
088300 MFS-RENSA-FAELT-IN SECTION.                                              
088400                                                                          
088500*    --- ALLA INDATA-FÄLT                                                 
088600       MOVE MFS-RENSA-FAELT           TO MOD-DATUM                        
088700                                         MOD-STAPAC-IN                    
088800                                         MOD-STOPAC-IN                    
088900                                         MOD-STAADM-IN                    
089000                                         MOD-STOADM-IN                    
089100                                         MOD-STALAST-IN                   
089200                                         MOD-STOLAST-IN                   
089300     .                                                                    
089400     EJECT                                                                
089500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
089600                                                                          
089700*    --- ALLA UTDATA-FÄLT                                                 
089800     MOVE MFS-ROER-EJ-FAELT           TO MOD-DATUM-UT                     
089900                                         MOD-IDPRC-UT                     
090000                                         MOD-IDDC-UT                      
090100                                         MOD-DATUM-NEXT                   
090200                                         MOD-DATUM-ENTER                  
090300     MOVE +1                          TO INDX                             
090400     PERFORM UNTIL INDX > MAX-INDX                                        
090500       MOVE MFS-ROER-EJ-FAELT         TO MOD-DATUM-RAD(INDX)              
090600                                         MOD-STAPAC-RAD(INDX)             
090700                                         MOD-STOPAC-RAD(INDX)             
090800                                         MOD-STAADM-RAD(INDX)             
090900                                         MOD-STOADM-RAD(INDX)             
091000                                         MOD-STALAST-RAD(INDX)            
091100                                         MOD-STOLAST-RAD(INDX)            
091200     ADD +1                           TO INDX                             
091300     END-PERFORM                                                          
091400     .                                                                    
091500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
091600       MOVE MFS-ROER-EJ-FAELT         TO MOD-DATUM                        
091700                                         MOD-STAPAC-IN                    
091800                                         MOD-STOPAC-IN                    
091900                                         MOD-STAADM-IN                    
092000                                         MOD-STOADM-IN                    
092100                                         MOD-STALAST-IN                   
092200                                         MOD-STOLAST-IN                   
092300     .                                                                    
092400 MFS-FORM-ATTR SECTION.                                                   
092500                                                                          
092600*    --- ALLA INDATA-FÄLT                                                 
092700     MOVE MFS-FORMATETS-ATTR TO MOD-DATUM                                 
092800                                MOD-STAPAC-IN                             
092900                                MOD-STOPAC-IN                             
093000                                MOD-STAADM-IN                             
093100                                MOD-STOADM-IN                             
093200                                MOD-STALAST-IN                            
093300                                MOD-STOLAST-IN                            
093400     .                                                                    
093500     SKIP2                                                                
093600 MFS-LAS-IN-IGEN SECTION.                                                 
093700                                                                          
093800*    --- ALLA INDATA-FÄLT                                                 
093900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DATUM-ATTR                         
094000                                MOD-STAPAC-IN-ATTR                        
094100                                MOD-STOPAC-IN-ATTR                        
094200                                MOD-STAADM-IN-ATTR                        
094300                                MOD-STOADM-IN-ATTR                        
094400                                MOD-STALAST-IN-ATTR                       
094500                                MOD-STOLAST-IN-ATTR                       
094600     .                                                                    
094700     EJECT                                                                
094800* --- IMS SEKTIONER ---                                                   
094900     SKIP2                                                                
095000 IMS-GET-MSG SECTION.                                                     
095100                                                                          
095200     MOVE '  QC' TO GODK-STATUSKODER                                      
095300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
095400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     .                                                                    
095700     SKIP3                                                                
095800 IMS-INSERT-MSG SECTION.                                                  
095900                                                                          
095910     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
096100       MOVE '0' TO MFS-KDHUVOMR                                           
096200     END-IF                                                               
096300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
096400     MOVE SPACE TO GODK-STATUSKODER                                       
096500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
096600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     .                                                                    
096900     EJECT                                                                
097000 IMS-GHU-WL443711 SECTION.                                                
097100                                                                          
097200     STRING 'WL443701(WDGXKEY  =' W-WDGXKEY-4437-X ')'                    
097300          DELIMITED BY SIZE INTO SSA1                                     
097400     STRING 'WL443711(DADATUM  =' W-WDGXKEY-4438-X ')'                    
097500          DELIMITED BY SIZE INTO SSA2                                     
097600     MOVE '  GE' TO GODK-STATUSKODER                                      
097700     CALL CBLTDLI USING GHU 4437-PCB DLI-IO-AREA SSA1 SSA2                
097800     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100 IMS-GU-WL443701 SECTION.                                                 
098200                                                                          
098300     STRING 'WL443701(WDGXKEY  =' W-WDGXKEY-4437-X ')'                    
098400          DELIMITED BY SIZE INTO SSA1                                     
098500     MOVE '  GE' TO GODK-STATUSKODER                                      
098600     CALL CBLTDLI USING GU 4437-PCB DLI-IO-AREA SSA1                      
098700     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
098800     PERFORM IMS-STATUSKONTROLL                                           
098900     .                                                                    
099000 IMS-GU-WL443711 SECTION.                                                 
099100                                                                          
099200     STRING 'WL443701(WDGXKEY  =' W-WDGXKEY-4437-X ')'                    
099300          DELIMITED BY SIZE INTO SSA1                                     
099400     STRING 'WL443711(DADATUM  =' W-WDGXKEY-4438-X ')'                    
099500          DELIMITED BY SIZE INTO SSA2                                     
099600     MOVE '  GE' TO GODK-STATUSKODER                                      
099700     CALL CBLTDLI USING GU 4437-PCB DLI-IO-AREA SSA1 SSA2                 
099800     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
099900     PERFORM IMS-STATUSKONTROLL                                           
100000     .                                                                    
100100 IMS-GNP-F-WL443711 SECTION.                                              
100200                                                                          
100300     STRING 'WL443711*F(DADATUM  =' W-WDGXKEY-4438-X ')'                  
100400          DELIMITED BY SIZE INTO SSA1                                     
100500     MOVE '  GE' TO GODK-STATUSKODER                                      
100600     CALL CBLTDLI USING GNP 4437-PCB DLI-IO-AREA SSA1                     
100700     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
100800     PERFORM IMS-STATUSKONTROLL                                           
100900     .                                                                    
101000 IMS-GNP-MIN-MAX-WL443711 SECTION.                                        
101100                                                                          
101200     STRING 'WL443711(DADATUM >=' W-WDGXKEY-4438-X                        
101300                    '&DADATUM <=' W-WDGXKEY-4438-MAX-X ')'                
101400          DELIMITED BY SIZE INTO SSA1                                     
101500     MOVE '  GE' TO GODK-STATUSKODER                                      
101600     CALL CBLTDLI USING GNP 4437-PCB DLI-IO-AREA SSA1                     
101700     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
101800     PERFORM IMS-STATUSKONTROLL                                           
101900     .                                                                    
102000 IMS-REPL-WL443711 SECTION.                                               
102100                                                                          
102200     MOVE '  ' TO GODK-STATUSKODER                                        
102300     CALL CBLTDLI USING REPL 4437-PCB DLI-IO-AREA                         
102400     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     EJECT                                                                
102800 IMS-STATUSKONTROLL SECTION.                                              
102900                                                                          
103000     SET STATUS-IX TO 1                                                   
103100     SEARCH GODK-STATUS                                                   
103200       AT END CALL FELLOG                                                 
103300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
103400     END-SEARCH                                                           
103500     .                                                                    
103510     EJECT                                                                
103511     EJECT                                                                
