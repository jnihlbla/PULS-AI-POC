000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070400.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   JUNI 1984.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   TP-UPPDATERINGSPROGRAM. LÄGGER UPP NYA JOB.              
000800                                                                          
000900*    INDATA.                                                              
001000*        TRANSAKTION: W0T704                                              
001100*        MID:         W0I70401                                            
001200*                     N0I70401                                            
001300*    UTDATA.                                                              
001400*        MOD:         W0O70401                                            
001500*                     N0O70401                                            
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002400 77  IDPGM                       PIC X(8)    VALUE 'W0070400'.            
002500 77  JA                          PIC X(1)    VALUE 'J'.                   
002600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
002700 77  OK                          PIC X(1)    VALUE 'O'.                   
002800 77  FEL                         PIC X(1)    VALUE 'F'.                   
002900 77  SECURITY-TEST               PIC X(1)    VALUE 'F'.                   
003000 77  LINECT                      PIC X(1)    VALUE 'N'.                   
003100 77  BORT                        PIC X(1)    VALUE 'B'.                   
003200 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +979 COMP SYNC.        
003300 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77  IXA                         PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003700     88  INDATA-OK                           VALUE 'J'.                   
003800     88  INDATA-FEL                          VALUE 'N'.                   
003900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004000     88  NYCKLAR-OK                          VALUE 'J'.                   
004100     88  NYCKLAR-FEL                         VALUE 'N'.                   
004200 77  MALL-SW                     PIC X       VALUE 'J'.                   
004300     88  VISA-MALL                           VALUE 'J'.                   
004400     88  VISA-EJ-MALL                        VALUE 'N'.                   
004500                                                                          
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005000                                                                          
005100 01  W-IDTRANS                   PIC X(4)    VALUE  SPACE.                
005200     88  EGEN-MID                            VALUE '0704'.                
005300     88  GODK-MID                            VALUE '0704'.                
005400                                                                          
005500 01  SW-NY-NYCKEL                PIC X(1).                                
005600     88  NY-NYCKEL                           VALUE 'J'.                   
005700                                                                          
005800 01  SPAR-AREOR.                                                          
005900     03  SPAR-IDRUTIN            PIC X(8).                                
006000     03  SPAR-IDJOB              PIC X(8).                                
006100     EJECT                                                                
006200 01  IDJCLRAD-TABELL.                                                     
006300     03  FILLER OCCURS 20.                                                
006400         05  IDJCLRAD-IFYLLD     PIC S9(5).                               
006500     SKIP3                                                                
006600 01  TEJCL-TABELL.                                                        
006700     03  FILLER OCCURS 8.                                                 
006800         05  TEJCL-IFYLLD        PIC S9(1).                               
006900     SKIP3                                                                
007000 01  TEST-IDJOB.                                                          
007100     03  TEST-IDJOB-POS-1        PIC X(1).                                
007200     03  FILLER                  PIC X(7).                                
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007500*    -COPY WMEDAREA                                                       
007600     SKIP3                                                                
007700 01  MESSAGES-CODES.                                                      
007800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008400     03  ERR-ROUTINE-MISSING     PIC X(3)    VALUE '135'.                 
008500     03  ERR-JOB-MISSING         PIC X(3)    VALUE '136'.                 
008600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008700     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
008800     EJECT                                                                
008900*01  AREA -COPY W007W001 -PRE MALL-.                                      
009000     EJECT                                                                
009100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009200*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009400     SKIP2                                                                
009500*01  MID -COPY W0I70401                                                   
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
009800*01  -COPY WMSGAREA                                                       
009900     EJECT                                                                
010000     03  MOD REDEFINES MSG-AREA.                                          
010100*      05  -COPY W0O70401                                                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400*01  -COPY WMFSAREA.                                                      
010500     EJECT                                                                
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700     SKIP2                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-WDP101KY-6001-X.                                               
011200         05  FILLER              PIC X(4)    VALUE '6001'.                
011300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011400     SKIP2                                                                
011500     03  W-WDP101KY-X.                                                    
011600         05  W-IDHTYP            PIC X(4).                                
011700         05  W-IDRUTIN           PIC X(8).                                
011800         05  W-IDJOB             PIC X(8).                                
011900         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
012000     SKIP2                                                                
012100     03  W-IDUSER                PIC X(8).                                
012200     SKIP2                                                                
012300     03  W-IDJOB1                PIC X(8).                                
012400     SKIP2                                                                
012500     03  W-IDJCLRAD-X.                                                    
012600         05  W-IDJCLRAD          PIC S9(5)   COMP-3.                      
012700     EJECT                                                                
012800*    --- STATUS-KOD FRÅN IMS                                              
012900 01  STATUS-WS                   PIC X(2).                                
013000     88  SEGMENT-FINNS                       VALUE '  '.                  
013100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013200     SKIP3                                                                
013300 01  GODK-STATUSKODER.                                                    
013400     03  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
013500     SKIP3                                                                
013600 01  SSA1                        PIC X(64).                               
013700 01  SSA2                        PIC X(64).                               
013800     EJECT                                                                
013900*    --- IMS FUNKTIONSKODER                                               
014000*01  -COPY W0003                                                          
014100     EJECT                                                                
014200*    --- DLI INPUT-OUTPUT AREA                                            
014300 01  DLI-IO-AREA.                                                         
014400     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
014500     SKIP3                                                                
014600     03  WLJCLC01 REDEFINES IO-AREA.                                      
014700*      05  -COPY WDP101   -PRE JCLC-                                      
014800     EJECT                                                                
014900     03  WLJCLC11 REDEFINES IO-AREA.                                      
015000*      05  -COPY WDP111   -PRE JCLC-                                      
015100     EJECT                                                                
015200     03  WLJCLC12 REDEFINES IO-AREA.                                      
015300*      05  -COPY WDP113   -PRE JCLC-                                      
015400     EJECT                                                                
015500     03  WLJCLD12 REDEFINES IO-AREA.                                      
015600*      05  -COPY WDP114   -PRE JCLD-                                      
015700     EJECT                                                                
015800 LINKAGE SECTION.                                                         
015900                                                                          
016000*01  -COPY W0009     -PRE MSG-                                            
016100     EJECT                                                                
016200*01  -COPY W0008     -PRE JCLA-                                           
016300         05  FILLER              PIC X.                                   
016400                                                                          
016500*01  -COPY W0008     -PRE JCLC-                                           
016600         05  FILLER              PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008     -PRE JCLD-                                           
016900         05  FILLER              PIC X.                                   
017000     EJECT                                                                
017100 PROCEDURE DIVISION USING MSG-PCB JCLA-PCB JCLC-PCB JCLD-PCB.             
017200 MAIN SECTION.                                                            
017300     ENTRY 'DLITCBL' USING MSG-PCB JCLA-PCB JCLC-PCB JCLD-PCB.            
017400                                                                          
017500     PERFORM IMS-GET-MSG                                                  
017600     IF SEGMENT-FINNS                                                     
017700       PERFORM A-INIT-SPARA-INPUT                                         
017800       PERFORM B-TESTA-SECURITY                                           
017900       IF SECURITY-TEST = OK                                              
018000         IF NY-NYCKEL OR MFS-FIRST                                        
018100           PERFORM C-VISA-JOB                                             
018200         ELSE                                                             
018300           PERFORM D-KOLLA-INPUT                                          
018400           IF MFS-NEXT                                                    
018500             PERFORM G-PFK8-BLADDRA                                       
018600           ELSE                                                           
018700             IF INDATA-OK                                                 
018800               IF MFS-UPDATE                                              
018900                 PERFORM E-UPPDATERA-JOB                                  
019000               ELSE                                                       
019100                 IF MID-IDJCLRAD-SKIP-IN NOT = ALL '+'                    
019200                   PERFORM F-VISA-MERA-JCL                                
019300                 END-IF                                                   
019400               END-IF                                                     
019500             ELSE                                                         
019600               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
019700               CALL WMEDKONV USING MED-WMEDAREA                           
019800               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
019900             END-IF                                                       
020000           END-IF                                                         
020100         END-IF                                                           
020200       END-IF                                                             
020300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
020400       PERFORM IMS-INSERT-MSG                                             
020500     END-IF                                                               
020600                                                                          
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 A-INIT-SPARA-INPUT SECTION.                                              
021200     SKIP2                                                                
021300     IF MSG-DUBBLA-TRANSKODER                                             
021400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70401                 
021500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021700     ELSE                                                                 
021800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I70401                  
021900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
022000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022100     END-IF                                                               
022200                                                                          
022300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
022400     MOVE MSG-IDPFK            TO MFS-IDPFK                               
022500     MOVE MFS-IDTRANS          TO W-IDTRANS                               
022600                                                                          
022700     MOVE LOW-VALUE  TO MOD-W0O70401                                      
022800     MOVE 'W0O70401' TO MFS-IDMOD                                         
022900     MOVE '0704'     TO MOD-IDTRANS                                       
023000                                                                          
023100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
023200                             MOD-TEMFSINF                                 
023300                             MOD-IDRUTIN-IN                               
023400                             MOD-IDJOB-IN                                 
023500                                                                          
023600     IF NOT EGEN-MID                                                      
023700       MOVE SPACE TO MFS-KDTRTYP                                          
023800       MOVE '7'   TO MFS-IDPFK                                            
023900       MOVE JA TO SW-NY-NYCKEL                                            
024000     END-IF                                                               
024100                                                                          
024200     IF ENGLISH-TEXT                                                      
024300       MOVE +2    TO SPRAK-IX                                             
024400       MOVE 'GB ' TO MED-IDSKYLT                                          
024500     ELSE                                                                 
024600       MOVE +1    TO SPRAK-IX                                             
024700       MOVE 'S  ' TO MED-IDSKYLT                                          
024800     END-IF                                                               
024900                                                                          
025000                                                                          
025100     MOVE MFS-ROER-EJ-FAELT TO MOD-BEJOB                                  
025200                               MOD-IDOWNER                                
025300                               MOD-IDJOB                                  
025400                               MOD-KDDEBINFO                              
025500                               MOD-KDJROOM                                
025600                               MOD-BEPGMNAMN                              
025700                               MOD-KDMSGCLASS                             
025800                               MOD-KDMSGLEVEL                             
025900                               MOD-KDJCLASS                               
026000                               MOD-IDNOTIFY                               
026100                               MOD-KVJTIME                                
026200                               MOD-KVJLINES                               
026300                               MOD-KDJFORMS                               
026400                               MOD-KVJCOUNT                               
026500                               MOD-KDROUTEX                               
026600                               MOD-KDROUTEP                               
026700                               MOD-IDPROCDD                               
026800                               MOD-KDOUTPUT                               
026900                               MOD-KDTRSTAT                               
027000     SET MOD-IX-LINE TO +1                                                
027100     PERFORM UNTIL MOD-IX-LINE > 8                                        
027200         MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)             
027300                                   MOD-TEJCL    (MOD-IX-LINE)             
027400         SET MOD-IX-LINE UP BY +1                                         
027500     END-PERFORM                                                          
027600                                                                          
027700     IF NOT EGEN-MID                                                      
027800       MOVE JA    TO SW-NY-NYCKEL                                         
027900       MOVE '7'   TO MFS-IDPFK                                            
028000       MOVE SPACE TO MFS-KDTRTYP                                          
028100     END-IF                                                               
028200                                                                          
028300     IF MID-IDRUTIN-IN = ALL '+'                                          
028400       MOVE MID-IDRUTIN-UT TO SPAR-IDRUTIN MOD-IDRUTIN-UT                 
028500     ELSE                                                                 
028600       MOVE MID-IDRUTIN-IN TO SPAR-IDRUTIN MOD-IDRUTIN-UT                 
028700       MOVE '7'   TO MFS-IDPFK                                            
028800       MOVE SPACE TO MFS-KDTRTYP                                          
028900     END-IF                                                               
029000                                                                          
029100     IF MID-IDJOB-IN = ALL '+'                                            
029200       MOVE MID-IDJOB-UT TO SPAR-IDJOB MOD-IDJOB-UT                       
029300     ELSE                                                                 
029400       MOVE MID-IDJOB-IN TO SPAR-IDJOB MOD-IDJOB-UT                       
029500       MOVE '7'   TO MFS-IDPFK                                            
029600       MOVE SPACE TO MFS-KDTRTYP                                          
029700     END-IF                                                               
029800                                                                          
029900     MOVE JA TO INDATA-SW                                                 
030000     .                                                                    
030100     EJECT                                                                
030200 B-TESTA-SECURITY SECTION.                                                
030300     SKIP2                                                                
030400     MOVE '6011'            TO W-IDHTYP                                   
030500     MOVE SPAR-IDRUTIN      TO W-IDRUTIN                                  
030600     MOVE LOW-VALUE         TO W-IDJOB                                    
030700     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
030800                                                                          
030900     PERFORM IMS-GET-6011-KNTL                                            
031000     IF SEGMENT-FINNS                                                     
031100       MOVE OK               TO SECURITY-TEST                             
031200     ELSE                                                                 
031300       PERFORM IMS-GET-6001-KNTL                                          
031400       IF SEGMENT-FINNS                                                   
031500         MOVE OK           TO SECURITY-TEST                               
031600       ELSE                                                               
031700         MOVE FEL          TO SECURITY-TEST                               
031800         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
031900         CALL WMEDKONV USING MED-WMEDAREA                                 
032000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
032100       END-IF                                                             
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 C-VISA-JOB SECTION.                                                      
032600     SKIP2                                                                
032700     MOVE '6011'       TO W-IDHTYP                                        
032800     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
032900     MOVE LOW-VALUE    TO W-IDJOB                                         
033000     MOVE SPAR-IDJOB   TO W-IDJOB1                                        
033100     PERFORM IMS-GET-6011-JOB                                             
033200                                                                          
033300     IF SEGMENT-FINNS                                                     
033400       PERFORM CA-VISA-JOBB-SOM-FINNS                                     
033500     ELSE                                                                 
033600       IF JCLC-SEG-LEVEL = ZERO                                           
033700         PERFORM CB-RENSA-ALLT                                            
033800         MOVE ERR-ROUTINE-MISSING TO MED-IDMFSFEL                         
033900         CALL WMEDKONV USING MED-WMEDAREA                                 
034000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
034100       ELSE                                                               
034200         PERFORM CC-VISA-RUTIN-MALL                                       
034300       END-IF                                                             
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 CA-VISA-JOBB-SOM-FINNS SECTION.                                          
034800     SKIP2                                                                
034900     MOVE NEJ TO MALL-SW                                                  
035000     MOVE JCLC-JOB-KDTRSTAT    TO MOD-KDTRSTAT                            
035100     MOVE MFS-FORMATETS-ATTR   TO MOD-KDTRSTAT-IN-ATTR                    
035200     MOVE MFS-RENSA-FAELT      TO MOD-KDTRSTAT-IN                         
035300     MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEJOB-ATTR                          
035400     MOVE JCLC-JOB-BEJOB       TO MOD-BEJOB                               
035500                                                                          
035600     MOVE '6021'       TO W-IDHTYP                                        
035700     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
035800     MOVE SPAR-IDJOB   TO W-IDJOB                                         
035900     PERFORM IMS-GET-6021-ROT                                             
036000     PERFORM IMS-GET-6021-KNTL                                            
036100     MOVE JCLC-KNTL-IDOWNER TO MOD-IDOWNER                                
036200                                                                          
036300     PERFORM IMS-GET-6021-JCL-OKVAL                                       
036400     SET MOD-IX-LINE MID-IX-LINE TO 1                                     
036500     PERFORM UNTIL JCLD-JCL-IDJCLRAD > 999 OR                             
036600                         MOD-IX-LINE > 8                                  
036700       IF SEGMENT-FINNS                                                   
036800         PERFORM S01-REDIGERA-MOD                                         
036900         PERFORM IMS-GET-6021-JCL-OKVAL                                   
037000       ELSE                                                               
037100         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
037200                                 MOD-TEJCL    (MOD-IX-LINE)               
037300         SET MOD-IX-LINE UP BY 1                                          
037400       END-IF                                                             
037500     END-PERFORM                                                          
037600                                                                          
037700     IF SEGMENT-FINNS                                                     
037800       MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD-NEXT                        
037900       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
038000       CALL WMEDKONV USING MED-WMEDAREA                                   
038100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
038200     END-IF                                                               
038300                                                                          
038400     .                                                                    
038500     EJECT                                                                
038600 CB-RENSA-ALLT SECTION.                                                   
038700     SKIP2                                                                
038800     MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD-SKIP-IN                         
038900                             MOD-IDJCLRAD-SKIP-UT                         
039000                             MOD-BEJOB                                    
039100                             MOD-IDOWNER                                  
039200                             MOD-IDJOB                                    
039300                             MOD-KDDEBINFO                                
039400                             MOD-KDJROOM                                  
039500                             MOD-BEPGMNAMN                                
039600                             MOD-KDMSGCLASS                               
039700                             MOD-KDMSGLEVEL                               
039800                             MOD-KDJCLASS                                 
039900                             MOD-IDNOTIFY                                 
040000                             MOD-KVJTIME                                  
040100                             MOD-KVJLINES                                 
040200                             MOD-KDJFORMS                                 
040300                             MOD-KVJCOUNT                                 
040400                             MOD-KDROUTEX                                 
040500                             MOD-KDROUTEP                                 
040600                             MOD-IDPROCDD                                 
040700                             MOD-KDOUTPUT                                 
040800                             MOD-KDTRSTAT-IN                              
040900                             MOD-KDTRSTAT                                 
041000                                                                          
041100     MOVE MFS-FORMATETS-ATTR TO MOD-IDJCLRAD-SKIP-ATTR                    
041200                                MOD-BEJOB-ATTR                            
041300                                MOD-IDOWNER-ATTR                          
041400                                MOD-IDJOB-ATTR                            
041500                                MOD-KDTRSTAT-IN-ATTR                      
041600                                MOD-KDJROOM-ATTR                          
041700                                MOD-BEPGMNAMN-ATTR                        
041800                                MOD-KDMSGCLASS-ATTR                       
041900                                MOD-KDMSGLEVEL-ATTR                       
042000                                MOD-KDJCLASS-ATTR                         
042100                                MOD-IDNOTIFY-ATTR                         
042200                                MOD-KVJTIME-ATTR                          
042300                                MOD-KVJLINES-ATTR                         
042400                                MOD-KDJFORMS-ATTR                         
042500                                MOD-KVJCOUNT-ATTR                         
042600                                MOD-KDROUTEX-ATTR                         
042700                                MOD-KDROUTEP-ATTR                         
042800                                MOD-IDPROCDD-ATTR                         
042900                                MOD-KDOUTPUT-ATTR                         
043000                                                                          
043100     SET MOD-IX-LINE TO +1                                                
043200     PERFORM UNTIL MOD-IX-LINE > +8                                       
043300        MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)                
043400                                MOD-TEJCL    (MOD-IX-LINE)                
043500        MOVE MFS-FORMATETS-ATTR TO MOD-IDJCLRAD-ATTR (MOD-IX-LINE)        
043600                                   MOD-TEJCL-ATTR    (MOD-IX-LINE)        
043700        SET MOD-IX-LINE UP BY +1                                          
043800     END-PERFORM                                                          
043900     .                                                                    
044000     EJECT                                                                
044100 CC-VISA-RUTIN-MALL SECTION.                                              
044200     SKIP2                                                                
044300     MOVE JA TO MALL-SW                                                   
044400     MOVE MFS-RENSA-FAELT    TO MOD-KDTRSTAT                              
044500     MOVE MFS-RENSA-FAELT    TO MOD-KDTRSTAT-IN                           
044600     MOVE MFS-FORMATETS-ATTR TO MOD-KDTRSTAT-IN-ATTR                      
044700                                                                          
044800     MOVE MFS-RENSA-FAELT      TO MOD-BEJOB                               
044900     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDOWNER-ATTR                        
045000     MOVE MSG-SIGNON-USERID    TO MOD-IDOWNER                             
045100                                                                          
045200     MOVE '6011'       TO W-IDHTYP                                        
045300     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
045400     MOVE LOW-VALUE    TO W-IDJOB                                         
045500     PERFORM IMS-GET-6011-ROT                                             
045600     PERFORM IMS-GET-6011-JCL                                             
045700                                                                          
045800     SET MOD-IX-LINE MID-IX-LINE TO 1                                     
045900     PERFORM UNTIL JCLD-JCL-IDJCLRAD > 999 OR                             
046000                         MOD-IX-LINE > 8                                  
046100       IF SEGMENT-FINNS                                                   
046200         PERFORM S01-REDIGERA-MOD                                         
046300         PERFORM IMS-GET-6011-JCL                                         
046400       ELSE                                                               
046500         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
046600                                 MOD-TEJCL  (MOD-IX-LINE)                 
046700         SET MOD-IX-LINE UP BY 1                                          
046800       END-IF                                                             
046900     END-PERFORM                                                          
047000                                                                          
047100     MOVE ERR-JOB-MISSING TO MED-IDMFSFEL                                 
047200     CALL WMEDKONV USING MED-WMEDAREA                                     
047300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
047400     .                                                                    
047500     EJECT                                                                
047600 D-KOLLA-INPUT SECTION.                                                   
047700     SKIP2                                                                
047800     MOVE 1 TO IX                                                         
047900     PERFORM UNTIL IX > 16                                                
048000       MOVE ZERO TO IDJCLRAD-IFYLLD (IX)                                  
048100       ADD +1 TO IX                                                       
048200     END-PERFORM                                                          
048300                                                                          
048400     PERFORM DA-KOLLA-JOB                                                 
048500     PERFORM DB-RAD10                                                     
048600     PERFORM DC-RAD20                                                     
048700     PERFORM DD-RAD30                                                     
048800     PERFORM DE-RAD40                                                     
048900     PERFORM DK-RAD47                                                     
049000     PERFORM DF-RAD50                                                     
049100     PERFORM DG-RAD60                                                     
049200     PERFORM DH-RAD70                                                     
049300     PERFORM DJ-RAD80                                                     
049400     PERFORM DI-RESTEN-AV-RADER                                           
049500     .                                                                    
049600     EJECT                                                                
049700 DA-KOLLA-JOB SECTION.                                                    
049800     SKIP2                                                                
049900     IF MID-BEJOB = ALL '+'                                               
050000       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEJOB-ATTR                          
050100       MOVE NEJ TO INDATA-SW                                              
050200     ELSE                                                                 
050300       IF MID-BEJOB = SPACE                                               
050400         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEJOB-ATTR                        
050500         MOVE NEJ TO INDATA-SW                                            
050600       ELSE                                                               
050700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEJOB-ATTR                      
050800       END-IF                                                             
050900     END-IF                                                               
051000                                                                          
051100     IF MID-IDOWNER NOT = ALL '+'                                         
051200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDOWNER-ATTR                      
051300     END-IF                                                               
051400                                                                          
051500     IF MID-KDTRSTAT NOT = ALL '+'                                        
051600       IF MID-KDTRSTAT NOT NUMERIC                                        
051700         MOVE MFS-NUM-FAELT-FEL TO MOD-KDTRSTAT-IN-ATTR                   
051800         MOVE NEJ TO INDATA-SW                                            
051900       ELSE                                                               
052000         IF MID-KDTRSTAT = +1 OR +2                                       
052100           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTRSTAT-IN-ATTR               
052200         ELSE                                                             
052300           MOVE MFS-NUM-FAELT-FEL TO MOD-KDTRSTAT-IN-ATTR                 
052400           MOVE NEJ TO INDATA-SW                                          
052500         END-IF                                                           
052600       END-IF                                                             
052700     END-IF                                                               
052800                                                                          
052900     IF INDATA-FEL                                                        
053000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDTRSTAT-IN                          
053100                                 MOD-KDTRSTAT                             
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 DB-RAD10 SECTION.                                                        
053600     SKIP2                                                                
053700     MOVE 1 TO IX                                                         
053800     IF MID-IDJOB = ALL '+'                                               
053900       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDJOB-ATTR                          
054000       MOVE NEJ TO INDATA-SW                                              
054100     ELSE                                                                 
054200       MOVE MID-IDJOB TO TEST-IDJOB                                       
054300       IF TEST-IDJOB-POS-1 ALPHABETIC AND                                 
054400          TEST-IDJOB-POS-1 NOT = SPACE                                    
054500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDJOB-ATTR                      
054600         MOVE 10                   TO IDJCLRAD-IFYLLD (IX)                
054700       ELSE                                                               
054800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDJOB-ATTR                      
054900         MOVE NEJ TO INDATA-SW                                            
055000       END-IF                                                             
055100     END-IF                                                               
055200                                                                          
055300     IF MID-KDJROOM = ALL '+'                                             
055400       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJROOM-ATTR                        
055500       MOVE NEJ TO INDATA-SW                                              
055600     ELSE                                                                 
055700       INSPECT MID-KDJROOM TALLYING TALLY FOR ALL SPACE                   
055800       IF TALLY > 0                                                       
055900         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJROOM-ATTR                      
056000         MOVE NEJ TO INDATA-SW                                            
056100       ELSE                                                               
056200         IF MID-KDJFORMS = '1800'                                         
056300           IF MID-KDJROOM = 'WBC2' OR 'W100' OR 'W200' OR 'W300'          
056400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJROOM-ATTR                
056500             MOVE 10                   TO IDJCLRAD-IFYLLD (IX)            
056600           ELSE                                                           
056700             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJROOM-ATTR                  
056800             MOVE NEJ                TO INDATA-SW                         
056900           END-IF                                                         
057000         ELSE                                                             
057100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJROOM-ATTR                  
057200           MOVE 10                   TO IDJCLRAD-IFYLLD (IX)              
057300         END-IF                                                           
057400       END-IF                                                             
057500     END-IF                                                               
057600                                                                          
057700     IF MID-BEPGMNAMN = ALL '+'                                           
057800       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEPGMNAMN-ATTR                      
057900       MOVE NEJ                TO INDATA-SW                               
058000     ELSE                                                                 
058100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEPGMNAMN-ATTR                    
058200       MOVE 10                   TO IDJCLRAD-IFYLLD (IX)                  
058300     END-IF                                                               
058400                                                                          
058500     IF IDJCLRAD-IFYLLD (IX) > 0                                          
058600       ADD +1 TO IX                                                       
058700     END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000 DC-RAD20 SECTION.                                                        
059100     SKIP2                                                                
059200     IF MID-KDMSGCLASS = ALL '+'                                          
059300       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGCLASS-ATTR                     
059400       MOVE NEJ                TO INDATA-SW                               
059500     ELSE                                                                 
059600       IF MID-KDMSGCLASS = 'A' OR 'H'                                     
059700         IF MID-KDJFORMS = '1800'                                         
059800           IF MID-KDMSGCLASS = 'A'                                        
059900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGCLASS-ATTR             
060000             MOVE 20                   TO IDJCLRAD-IFYLLD (IX)            
060100           ELSE                                                           
060200             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGCLASS-ATTR               
060300             MOVE NEJ                TO INDATA-SW                         
060400           END-IF                                                         
060500         ELSE                                                             
060600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGCLASS-ATTR               
060700           MOVE 20                   TO IDJCLRAD-IFYLLD (IX)              
060800         END-IF                                                           
060900       ELSE                                                               
061000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGCLASS-ATTR                   
061100         MOVE NEJ                TO INDATA-SW                             
061200      END-IF                                                              
061300     END-IF                                                               
061400                                                                          
061500     IF MID-KDMSGLEVEL = ALL '+'                                          
061600       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGLEVEL-ATTR                     
061700       MOVE NEJ                TO INDATA-SW                               
061800     ELSE                                                                 
061900       IF MID-KDMSGLEVEL = '0,0' OR '1,0' OR '2,0' OR                     
062000                           '0,1' OR '1,1' OR '2,1'                        
062100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGLEVEL-ATTR                 
062200         MOVE 20                   TO IDJCLRAD-IFYLLD (IX)                
062300       ELSE                                                               
062400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGLEVEL-ATTR                   
062500         MOVE NEJ                TO INDATA-SW                             
062600       END-IF                                                             
062700     END-IF                                                               
062800                                                                          
062900     IF IDJCLRAD-IFYLLD (IX) > 0                                          
063000       ADD +1 TO IX                                                       
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 DD-RAD30 SECTION.                                                        
063500     SKIP2                                                                
063600     IF MID-KDJCLASS = ALL '+'                                            
063700       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJCLASS-ATTR                       
063800       MOVE NEJ                TO INDATA-SW                               
063900     ELSE                                                                 
064000       IF MID-KDJCLASS = 'N' OR 'T' OR 'V' OR '1' OR                      
064100                         'E' OR 'K' OR 'L' OR 'M'                         
064200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJCLASS-ATTR                   
064300         MOVE 30                   TO IDJCLRAD-IFYLLD (IX)                
064400       ELSE                                                               
064500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJCLASS-ATTR                     
064600         MOVE NEJ                TO INDATA-SW                             
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     IF MID-IDNOTIFY NOT = ALL '+'                                        
065100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDNOTIFY-ATTR                     
065200       MOVE 30                   TO IDJCLRAD-IFYLLD (IX)                  
065300     ELSE                                                                 
065400       MOVE MFS-FORMATETS-ATTR   TO MOD-IDNOTIFY-ATTR                     
065500     END-IF                                                               
065600                                                                          
065700     IF IDJCLRAD-IFYLLD (IX) > 0                                          
065800       ADD +1 TO IX                                                       
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 DE-RAD40 SECTION.                                                        
066300     SKIP2                                                                
066400     IF MID-KVJTIME = ALL '+'                                             
066500       MOVE MFS-NUM-FAELT-FEL TO MOD-KVJTIME-ATTR                         
066600       MOVE NEJ               TO INDATA-SW                                
066700     ELSE                                                                 
066800       IF MID-KVJTIME NOT NUMERIC                                         
066900           MOVE MFS-NUM-FAELT-FEL TO MOD-KVJTIME-ATTR                     
067000           MOVE NEJ               TO INDATA-SW                            
067100       ELSE                                                               
067200         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVJTIME-ATTR                     
067300         MOVE 40                  TO IDJCLRAD-IFYLLD (IX)                 
067400       END-IF                                                             
067500     END-IF                                                               
067600                                                                          
067700     IF MID-KVJLINES = ALL '+'                                            
067800       MOVE MFS-NUM-FAELT-FEL TO MOD-KVJLINES-ATTR                        
067900       MOVE NEJ               TO INDATA-SW                                
068000     ELSE                                                                 
068100       IF MID-KVJLINES NOT NUMERIC                                        
068200         MOVE MFS-NUM-FAELT-FEL TO MOD-KVJLINES-ATTR                      
068300         MOVE NEJ               TO INDATA-SW                              
068400       ELSE                                                               
068500         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVJLINES-ATTR                    
068600         MOVE 40                  TO IDJCLRAD-IFYLLD (IX)                 
068700       END-IF                                                             
068800     END-IF                                                               
068900                                                                          
069000     IF MID-KDJFORMS = ALL '+'                                            
069100       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJFORMS-ATTR                       
069200       MOVE NEJ                TO INDATA-SW                               
069300     ELSE                                                                 
069400       IF MID-KDJFORMS = 'STD' OR MID-KDJFORMS NUMERIC                    
069500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJFORMS-ATTR                   
069600         MOVE 40                   TO IDJCLRAD-IFYLLD (IX)                
069700       ELSE                                                               
069800         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJFORMS-ATTR                     
069900         MOVE NEJ                TO INDATA-SW                             
070000       END-IF                                                             
070100     END-IF                                                               
070200                                                                          
070300     IF IDJCLRAD-IFYLLD (IX) > 0                                          
070400       ADD +1 TO IX                                                       
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800 DF-RAD50 SECTION.                                                        
070900     SKIP2                                                                
071000     IF MID-KDROUTEX = ALL '+'                                            
071100       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEX-ATTR                       
071200       MOVE NEJ                TO INDATA-SW                               
071300     ELSE                                                                 
071400       IF MID-KDROUTEX = SPACE                                            
071500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEX-ATTR                     
071600         MOVE NEJ                TO INDATA-SW                             
071700       ELSE                                                               
071800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEX-ATTR                   
071900         MOVE 50                   TO IDJCLRAD-IFYLLD (IX)                
072000       END-IF                                                             
072100     END-IF                                                               
072200                                                                          
072300     IF IDJCLRAD-IFYLLD (IX) > 0                                          
072400       ADD +1 TO IX                                                       
072500     END-IF                                                               
072600     .                                                                    
072700     EJECT                                                                
072800 DG-RAD60 SECTION.                                                        
072900     SKIP2                                                                
073000     IF MID-KDROUTEP = ALL '+'                                            
073100       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEP-ATTR                       
073200       MOVE NEJ                TO INDATA-SW                               
073300     ELSE                                                                 
073400       IF MID-KDROUTEP = SPACE                                            
073500          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEP-ATTR                    
073600          MOVE NEJ                TO INDATA-SW                            
073700       ELSE                                                               
073800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEP-ATTR                   
073900         MOVE 60                   TO IDJCLRAD-IFYLLD (IX)                
074000       END-IF                                                             
074100     END-IF                                                               
074200                                                                          
074300     IF IDJCLRAD-IFYLLD (IX) > 0                                          
074400       ADD +1 TO IX                                                       
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 DH-RAD70 SECTION.                                                        
074900     SKIP2                                                                
075000     IF MID-KDOUTPUT = ALL '+'                                            
075100       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDOUTPUT-ATTR                       
075200       MOVE NEJ                TO INDATA-SW                               
075300     ELSE                                                                 
075400       IF MID-KDOUTPUT = SPACE                                            
075500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDOUTPUT-ATTR                     
075600         MOVE NEJ                TO INDATA-SW                             
075700       ELSE                                                               
075800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDOUTPUT-ATTR                   
075900         MOVE 70                   TO IDJCLRAD-IFYLLD (IX)                
076000       END-IF                                                             
076100     END-IF                                                               
076200                                                                          
076300     IF IDJCLRAD-IFYLLD (IX) > 0                                          
076400       ADD +1 TO IX                                                       
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800 DI-RESTEN-AV-RADER SECTION.                                              
076900     SKIP2                                                                
077000     MOVE 1 TO IXA                                                        
077100     SET MID-IX-LINE MOD-IX-LINE TO +1                                    
077200     PERFORM UNTIL MID-IX-LINE > 8                                        
077300       IF MID-TEJCL (MID-IX-LINE) NOT = ALL '+'                           
077400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEJCL-ATTR (MOD-IX-LINE)        
077500         IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC AND                        
077600            MID-IDJCLRAD (MID-IX-LINE) > 99 AND < 1000                    
077700           INSPECT MID-TEJCL (MID-IX-LINE) REPLACING                      
077800                   ALL ' JOB ' BY '*FEL*'                                 
077900           MOVE MID-IDJCLRAD (MID-IX-LINE) TO IDJCLRAD-IFYLLD (IX)        
078000           SET TEJCL-IFYLLD (IXA)          TO MID-IX-LINE                 
078100           ADD 1 TO IX IXA                                                
078200         ELSE                                                             
078300           MOVE NEJ TO INDATA-SW                                          
078400           MOVE MFS-NUM-FAELT-FEL TO                                      
078500                MOD-IDJCLRAD-ATTR (MOD-IX-LINE)                           
078600         END-IF                                                           
078700       ELSE                                                               
078800         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
078900                                 MOD-TEJCL    (MOD-IX-LINE)               
079000         MOVE MFS-FORMATETS-ATTR TO MOD-IDJCLRAD-ATTR(MOD-IX-LINE)        
079100                                    MOD-TEJCL-ATTR   (MOD-IX-LINE)        
079200         END-IF                                                           
079300         SET MID-IX-LINE MOD-IX-LINE UP BY 1                              
079400     END-PERFORM                                                          
079500     .                                                                    
079600     EJECT                                                                
079700 DJ-RAD80 SECTION.                                                        
079800     SKIP2                                                                
079900     IF MID-IDPROCDD = ALL '+' OR SPACE                                   
080000       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROCDD-ATTR                       
080100       MOVE NEJ                TO INDATA-SW                               
080200     ELSE                                                                 
080300       IF MID-IDPROCDD = 'PROD' OR 'TEST'                                 
080400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROCDD-ATTR                   
080500         MOVE 80                   TO IDJCLRAD-IFYLLD (IX)                
080600       ELSE                                                               
080700         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROCDD-ATTR                     
080800         MOVE NEJ                TO INDATA-SW                             
080900       END-IF                                                             
081000     END-IF                                                               
081100                                                                          
081200     IF IDJCLRAD-IFYLLD (IX) > 0                                          
081300       ADD +1 TO IX                                                       
081400     END-IF                                                               
081500     .                                                                    
081600     EJECT                                                                
081700 DK-RAD47 SECTION.                                                        
081800     SKIP2                                                                
081900     IF MID-KVJCOUNT                   =  ALL '+'                         
082000       MOVE MFS-RENSA-FAELT          TO MOD-KVJCOUNT                      
082100       MOVE NEJ                      TO LINECT                            
082200       MOVE 47                       TO IDJCLRAD-IFYLLD (IX)              
082300     ELSE                                                                 
082400       IF MID-KVJCOUNT                =  SPACE                            
082500         MOVE MFS-RENSA-FAELT         TO MOD-KVJCOUNT                     
082600         MOVE 47                      TO IDJCLRAD-IFYLLD (IX)             
082700         MOVE BORT                    TO LINECT                           
082800       ELSE                                                               
082900         IF MID-KVJCOUNT NUMERIC                                          
083000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVJCOUNT-ATTR                 
083100           MOVE 47                   TO IDJCLRAD-IFYLLD (IX)              
083200           MOVE JA                   TO LINECT                            
083300         ELSE                                                             
083400           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVJCOUNT-ATTR                 
083500           MOVE NEJ                  TO INDATA-SW                         
083600                                        LINECT                            
083700         END-IF                                                           
083800       END-IF                                                             
083900     END-IF                                                               
084000                                                                          
084100     IF IDJCLRAD-IFYLLD (IX) > 0                                          
084200       ADD +1 TO IX                                                       
084300     END-IF                                                               
084400     .                                                                    
084500     EJECT                                                                
084600 E-UPPDATERA-JOB SECTION.                                                 
084700     SKIP2                                                                
084800     MOVE '6011'       TO W-IDHTYP                                        
084900     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
085000     MOVE LOW-VALUE    TO W-IDJOB                                         
085100     MOVE SPAR-IDJOB TO W-IDJOB1                                          
085200     PERFORM IMS-GET-6011-JOB                                             
085300     IF SEGMENT-FINNS                                                     
085400       PERFORM EA-AENDRA-JOB                                              
085500       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
085600       CALL WMEDKONV USING MED-WMEDAREA                                   
085700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
085800     ELSE                                                                 
085900       IF JCLC-SEG-LEVEL = ZERO                                           
086000         MOVE ERR-ROUTINE-MISSING TO MED-IDMFSFEL                         
086100         CALL WMEDKONV USING MED-WMEDAREA                                 
086200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
086300       ELSE                                                               
086400         PERFORM EB-NYTT-JOB                                              
086500         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
086600         CALL WMEDKONV USING MED-WMEDAREA                                 
086700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
086800       END-IF                                                             
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 EA-AENDRA-JOB SECTION.                                                   
087300     SKIP2                                                                
087400     ACCEPT JCLC-JOB-TIUPPDAT FROM DATE                                   
087500     ACCEPT JCLC-JOB-TIUPPTID FROM TIME                                   
087600                                                                          
087700     IF MID-KDTRSTAT = ALL '+'                                            
087800       CONTINUE                                                           
087900     ELSE                                                                 
088000       MOVE MID-KDTRSTAT       TO MOD-KDTRSTAT                            
088100                                  JCLC-JOB-KDTRSTAT                       
088200     END-IF                                                               
088300     MOVE MFS-RENSA-FAELT    TO MOD-KDTRSTAT-IN                           
088400     MOVE MFS-FORMATETS-ATTR TO MOD-KDTRSTAT-IN-ATTR                      
088500                                                                          
088600     MOVE MID-BEJOB       TO JCLC-JOB-BEJOB MOD-BEJOB                     
088700     PERFORM IMS-REPLACE-6011-JOB                                         
088800                                                                          
088900     MOVE '6021'       TO W-IDHTYP                                        
089000     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
089100     MOVE SPAR-IDJOB   TO W-IDJOB                                         
089200     PERFORM IMS-GET-6021-ROT                                             
089300                                                                          
089400     MOVE +1 TO IX IXA                                                    
089500     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
089600     PERFORM UNTIL IX > 16 OR IDJCLRAD-IFYLLD (IX) NOT > 0                
089700       MOVE IDJCLRAD-IFYLLD (IX) TO W-IDJCLRAD                            
089800       PERFORM IMS-GET-6021-JCL-KVAL                                      
089900       PERFORM EAA-FYLL-I-TEJCL                                           
090000       IF SEGMENT-FINNS                                                   
090100         IF W-IDJCLRAD > 99 AND MID-TEJCL (MID-IX-LINE) = SPACE           
090200           PERFORM IMS-DELETE                                             
090300         ELSE                                                             
090400           IF W-IDJCLRAD = 47 AND                                         
090500              (LINECT     = BORT OR NEJ)                                  
090600             PERFORM IMS-DELETE                                           
090700           ELSE                                                           
090800             PERFORM IMS-REPLACE-6021-JCL                                 
090900           END-IF                                                         
091000         END-IF                                                           
091100       ELSE                                                               
091200         IF W-IDJCLRAD = 47 AND                                           
091300           (LINECT     = BORT OR NEJ)                                     
091400           CONTINUE                                                       
091500         ELSE                                                             
091600           MOVE IDJCLRAD-IFYLLD (IX) TO JCLD-JCL-IDJCLRAD                 
091700           PERFORM IMS-INSERT-6021-JCL                                    
091800         END-IF                                                           
091900       END-IF                                                             
092000       ADD +1 TO IX                                                       
092100     END-PERFORM                                                          
092200                                                                          
092300     PERFORM IMS-GET-6021-ROT                                             
092400     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
092500     IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC                                
092600       MOVE MID-IDJCLRAD (MID-IX-LINE) TO W-IDJCLRAD                      
092700     ELSE                                                                 
092800       MOVE 100 TO W-IDJCLRAD                                             
092900     END-IF                                                               
093000     PERFORM IMS-GET-6021-JCL-KVAL-STORRE                                 
093100     PERFORM UNTIL MOD-IX-LINE > 8                                        
093200       IF SEGMENT-FINNS                                                   
093300         IF JCLD-JCL-IDJCLRAD > 99 AND < 1000                             
093400           MOVE JCLD-JCL-IDJCLRAD TO                                      
093500                MOD-IDJCLRAD (MOD-IX-LINE)                                
093600           MOVE JCLD-JCL-TEJCL TO MOD-TEJCL (MOD-IX-LINE)                 
093700           SET MOD-IX-LINE UP BY 1                                        
093800         END-IF                                                           
093900         PERFORM IMS-GET-6021-JCL-KVAL-STORRE                             
094000       ELSE                                                               
094100         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
094200                                 MOD-TEJCL    (MOD-IX-LINE)               
094300         SET MOD-IX-LINE UP BY 1                                          
094400       END-IF                                                             
094500     END-PERFORM                                                          
094600     IF SEGMENT-FINNS                                                     
094700       MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD-NEXT                        
094800     END-IF                                                               
094900     .                                                                    
095000     EJECT                                                                
095100 EAA-FYLL-I-TEJCL SECTION.                                                
095200     SKIP2                                                                
095300     EVALUATE W-IDJCLRAD                                                  
095400         WHEN 10                                                          
095500             MOVE MID-IDJOB     TO MALL-IDJOB MOD-IDJOB                   
095600             MOVE MID-KDDEBINFO TO MALL-KDDEBINFO                         
095700             MOVE MID-KDJROOM   TO MALL-KDJROOM MOD-KDJROOM               
095800             MOVE MID-BEPGMNAMN TO MALL-BEPGMNAMN MOD-BEPGMNAMN           
095900             MOVE MALL-RAD10    TO JCLD-JCL-TEJCL                         
096000                                                                          
096100         WHEN 20                                                          
096200             MOVE MID-KDMSGCLASS TO MALL-KDMSGCLASS MOD-KDMSGCLASS        
096300             MOVE MID-KDMSGLEVEL TO MALL-KDMSGLEVEL MOD-KDMSGLEVEL        
096400             MOVE MALL-RAD20     TO JCLD-JCL-TEJCL                        
096500                                                                          
096600         WHEN 30                                                          
096700             MOVE MID-KDJCLASS TO MALL-KDJCLASS                           
096800                                  MOD-KDJCLASS                            
096900             IF MID-IDNOTIFY NOT = ALL '+'                                
097000                 MOVE MID-IDNOTIFY TO MALL-IDNOTIFY MOD-IDNOTIFY          
097100             ELSE                                                         
097200                 MOVE SPACE TO MALL-IDNOTIFY                              
097300                 MOVE MFS-RENSA-FAELT TO MOD-IDNOTIFY                     
097400             END-IF                                                       
097500             MOVE MALL-RAD30 TO JCLD-JCL-TEJCL                            
097600                                                                          
097700         WHEN 40                                                          
097800             MOVE MID-KVJTIME  TO MALL-KVJTIME MOD-KVJTIME                
097900             MOVE MID-KVJLINES TO MALL-KVJLINES MOD-KVJLINES              
098000             MOVE MID-KDJFORMS TO MALL-KDJFORMS MOD-KDJFORMS              
098100             MOVE MALL-RAD40   TO JCLD-JCL-TEJCL                          
098200                                                                          
098300         WHEN 47                                                          
098400             IF LINECT = JA                                               
098500                MOVE MID-KVJCOUNT TO MALL-KVJCOUNT MOD-KVJCOUNT           
098600                MOVE MALL-RAD47   TO JCLD-JCL-TEJCL                       
098700             END-IF                                                       
098800                                                                          
098900         WHEN 50                                                          
099000             MOVE MID-KDROUTEX TO MALL-KDROUTEX MOD-KDROUTEX              
099100             MOVE MALL-RAD50   TO JCLD-JCL-TEJCL                          
099200                                                                          
099300         WHEN 60                                                          
099400             MOVE MID-KDROUTEP TO MALL-KDROUTEP MOD-KDROUTEP              
099500             MOVE MALL-RAD60   TO JCLD-JCL-TEJCL                          
099600                                                                          
099700         WHEN 70                                                          
099800             MOVE MID-KDOUTPUT TO MALL-KDOUTPUT MOD-KDOUTPUT              
099900             MOVE MALL-RAD70   TO JCLD-JCL-TEJCL                          
100000                                                                          
100100         WHEN 80                                                          
100200             MOVE MID-IDPROCDD TO MALL-IDPROCDD MOD-IDPROCDD              
100300             MOVE MALL-RAD80   TO JCLD-JCL-TEJCL                          
100400                                                                          
100500         WHEN OTHER                                                       
100600             SET MID-IX-LINE MOD-IX-LINE  TO TEJCL-IFYLLD (IXA)           
100700             MOVE MID-TEJCL (MID-IX-LINE) TO                              
100800                                          JCLD-JCL-TEJCL                  
100900                                          MOD-TEJCL (MOD-IX-LINE)         
101000             MOVE MID-IDJCLRAD (MID-IX-LINE) TO                           
101100                                      MOD-IDJCLRAD (MOD-IX-LINE)          
101200             ADD 1 TO IXA                                                 
101300*            CALL FELLOG                                                  
101400     END-EVALUATE                                                         
101500     .                                                                    
101600     EJECT                                                                
101700 EB-NYTT-JOB SECTION.                                                     
101800     SKIP2                                                                
101900     MOVE '6011'       TO W-IDHTYP                                        
102000     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
102100     MOVE LOW-VALUE    TO W-IDJOB                                         
102200     MOVE SPAR-IDJOB   TO JCLC-JOB-IDJOB                                  
102300     ACCEPT JCLC-JOB-TIREGDAT FROM DATE                                   
102400     MOVE ZERO         TO JCLC-JOB-TIUPPDAT                               
102500                          JCLC-JOB-TIUPPTID                               
102600                                                                          
102700     IF MID-KDTRSTAT = ALL '+'                                            
102800       MOVE +1 TO JCLC-JOB-KDTRSTAT                                       
102900                       MOD-KDTRSTAT                                       
103000     ELSE                                                                 
103100       MOVE MID-KDTRSTAT  TO JCLC-JOB-KDTRSTAT                            
103200                                  MOD-KDTRSTAT                            
103300     END-IF                                                               
103400     MOVE MFS-RENSA-FAELT    TO MOD-KDTRSTAT-IN                           
103500     MOVE MFS-FORMATETS-ATTR TO MOD-KDTRSTAT-IN-ATTR                      
103600                                                                          
103700     MOVE MID-BEJOB TO JCLC-JOB-BEJOB                                     
103800                            MOD-BEJOB                                     
103900     PERFORM IMS-INSERT-6011-JOB                                          
104000                                                                          
104100     MOVE '6021'       TO JCLC-ROT-IDHTYP W-IDHTYP                        
104200     MOVE SPAR-IDRUTIN TO JCLC-ROT-IDRUTIN W-IDRUTIN                      
104300     MOVE SPAR-IDJOB   TO JCLC-ROT-IDJOB W-IDJOB                          
104400     MOVE LOW-VALUE    TO JCLC-ROT-LOWVALUE                               
104500     PERFORM IMS-INSERT-6021-ROT                                          
104600                                                                          
104700     IF MID-IDOWNER = ALL '+'                                             
104800       MOVE MSG-SIGNON-USERID TO JCLC-KNTL-IDOWNER                        
104900                                 JCLC-KNTL-IDUSER                         
105000                                 MOD-IDOWNER                              
105100     ELSE                                                                 
105200       MOVE MID-IDOWNER TO JCLC-KNTL-IDOWNER                              
105300                           JCLC-KNTL-IDUSER                               
105400                           MOD-IDOWNER                                    
105500     END-IF                                                               
105600     ACCEPT JCLC-KNTL-TIREGDAT FROM DATE                                  
105700     PERFORM IMS-INSERT-6021-KNTL                                         
105800                                                                          
105900     MOVE +1 TO IX IXA                                                    
106000     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
106100     PERFORM UNTIL IX > 16 OR IDJCLRAD-IFYLLD (IX) NOT > 0                
106200       MOVE IDJCLRAD-IFYLLD (IX) TO JCLD-JCL-IDJCLRAD                     
106300                                    W-IDJCLRAD                            
106400       PERFORM EBA-FYLL-I-TEJCL                                           
106500       IF W-IDJCLRAD = 47 AND                                             
106600          LINECT     = NEJ                                                
106700         MOVE MFS-RENSA-FAELT TO MOD-KVJCOUNT                             
106800       ELSE                                                               
106900         PERFORM IMS-INSERT-6021-JCL                                      
107000       END-IF                                                             
107100       ADD +1 TO IX                                                       
107200     END-PERFORM                                                          
107300     .                                                                    
107400     EJECT                                                                
107500 EBA-FYLL-I-TEJCL SECTION.                                                
107600     SKIP2                                                                
107700     EVALUATE W-IDJCLRAD                                                  
107800         WHEN 10                                                          
107900             MOVE MID-IDJOB      TO MALL-IDJOB MOD-IDJOB                  
108000             MOVE MID-KDDEBINFO  TO MALL-KDDEBINFO                        
108100             MOVE MID-KDJROOM    TO MALL-KDJROOM MOD-KDJROOM              
108200             MOVE MID-BEPGMNAMN  TO MALL-BEPGMNAMN MOD-BEPGMNAMN          
108300             MOVE MALL-RAD10     TO JCLD-JCL-TEJCL                        
108400                                                                          
108500         WHEN 20                                                          
108600             MOVE MID-KDMSGCLASS TO MALL-KDMSGCLASS MOD-KDMSGCLASS        
108700             MOVE MID-KDMSGLEVEL TO MALL-KDMSGLEVEL MOD-KDMSGLEVEL        
108800             MOVE MALL-RAD20     TO JCLD-JCL-TEJCL                        
108900                                                                          
109000         WHEN 30                                                          
109100             MOVE MID-KDJCLASS TO MALL-KDJCLASS                           
109200                                  MOD-KDJCLASS                            
109300             IF MID-IDNOTIFY NOT = ALL '+'                                
109400               MOVE MID-IDNOTIFY TO MALL-IDNOTIFY                         
109500                                    MOD-IDNOTIFY                          
109600             ELSE                                                         
109700               MOVE SPACE           TO MALL-IDNOTIFY                      
109800               MOVE MFS-RENSA-FAELT TO MOD-IDNOTIFY                       
109900             END-IF                                                       
110000             MOVE MALL-RAD30 TO JCLD-JCL-TEJCL                            
110100                                                                          
110200         WHEN 40                                                          
110300             MOVE MID-KVJTIME  TO MALL-KVJTIME MOD-KVJTIME                
110400             MOVE MID-KVJLINES TO MALL-KVJLINES MOD-KVJLINES              
110500             MOVE MID-KDJFORMS TO MALL-KDJFORMS MOD-KDJFORMS              
110600             MOVE MALL-RAD40   TO JCLD-JCL-TEJCL                          
110700                                                                          
110800         WHEN 47                                                          
110900             MOVE MID-KVJCOUNT TO MALL-KVJCOUNT MOD-KVJCOUNT              
111000             MOVE MALL-RAD47   TO JCLD-JCL-TEJCL                          
111100                                                                          
111200         WHEN 50                                                          
111300             MOVE MID-KDROUTEX TO MALL-KDROUTEX MOD-KDROUTEX              
111400             MOVE MALL-RAD50   TO JCLD-JCL-TEJCL                          
111500                                                                          
111600         WHEN 60                                                          
111700             MOVE MID-KDROUTEP TO MALL-KDROUTEP MOD-KDROUTEP              
111800             MOVE MALL-RAD60   TO JCLD-JCL-TEJCL                          
111900                                                                          
112000         WHEN 70                                                          
112100             MOVE MID-KDOUTPUT TO MALL-KDOUTPUT MOD-KDOUTPUT              
112200             MOVE MALL-RAD70   TO JCLD-JCL-TEJCL                          
112300                                                                          
112400         WHEN 80                                                          
112500             MOVE MID-IDPROCDD TO MALL-IDPROCDD MOD-IDPROCDD              
112600             MOVE MALL-RAD80   TO JCLD-JCL-TEJCL                          
112700                                                                          
112800         WHEN OTHER                                                       
112900             SET MID-IX-LINE MOD-IX-LINE TO TEJCL-IFYLLD (IXA)            
113000             IF MID-TEJCL (MID-IX-LINE) = ALL '+'                         
113100               MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)         
113200                                        MOD-TEJCL   (MOD-IX-LINE)         
113300             ELSE                                                         
113400               MOVE MID-TEJCL (MID-IX-LINE) TO JCLD-JCL-TEJCL             
113500                    MOD-TEJCL (MOD-IX-LINE)                               
113600               MOVE MID-IDJCLRAD (MID-IX-LINE) TO                         
113700                    MOD-IDJCLRAD (MOD-IX-LINE)                            
113800             END-IF                                                       
113900             ADD 1 TO IXA                                                 
114000                                                                          
114100     END-EVALUATE                                                         
114200     .                                                                    
114300     EJECT                                                                
114400 F-VISA-MERA-JCL SECTION.                                                 
114500     SKIP2                                                                
114600     IF MID-IDJCLRAD-SKIP-IN NUMERIC AND                                  
114700             MID-IDJCLRAD-SKIP-IN > 99 AND < 1000                         
114800         MOVE MID-IDJCLRAD-SKIP-IN TO MOD-IDJCLRAD-SKIP-UT                
114900                                      W-IDJCLRAD                          
115000         MOVE '6021'       TO W-IDHTYP                                    
115100         MOVE SPAR-IDRUTIN TO W-IDRUTIN                                   
115200         MOVE SPAR-IDJOB   TO W-IDJOB                                     
115300         PERFORM IMS-GET-6021-ROT                                         
115400         PERFORM IMS-GET-6021-JCL-KVAL-STORRE                             
115500         SET MOD-IX-LINE TO 1                                             
115600         PERFORM UNTIL MOD-IX-LINE > 8 OR JCLD-JCL-IDJCLRAD > 999         
115700           IF SEGMENT-FINNS                                               
115800             MOVE JCLD-JCL-IDJCLRAD TO                                    
115900                       MOD-IDJCLRAD (MOD-IX-LINE)                         
116000             MOVE JCLD-JCL-TEJCL TO MOD-TEJCL (MOD-IX-LINE)               
116100             PERFORM IMS-GET-6021-JCL-KVAL-STORRE                         
116200           ELSE                                                           
116300             MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)           
116400                                     MOD-TEJCL (MOD-IX-LINE)              
116500             MOVE MFS-FORMATETS-ATTR TO                                   
116600                      MOD-TEJCL-ATTR (MOD-IX-LINE)                        
116700           END-IF                                                         
116800           SET MOD-IX-LINE UP BY 1                                        
116900         END-PERFORM                                                      
117000         IF SEGMENT-FINNS                                                 
117100           MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD-NEXT                    
117200           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
117300           CALL WMEDKONV USING MED-WMEDAREA                               
117400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
117500         END-IF                                                           
117600     ELSE                                                                 
117700       MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD-SKIP-IN                     
117800       MOVE MFS-NUM-FAELT-FEL TO MOD-IDJCLRAD-SKIP-ATTR                   
117900       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
118000       CALL WMEDKONV USING MED-WMEDAREA                                   
118100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
118200     END-IF.                                                              
118300     EJECT                                                                
118400 G-PFK8-BLADDRA SECTION.                                                  
118500                                                                          
118600     IF MID-IDJCLRAD-NEXT > 99                                            
118700       MOVE MID-IDJCLRAD-NEXT TO W-IDJCLRAD                               
118800     ELSE                                                                 
118900       MOVE 100               TO W-IDJCLRAD                               
119000     END-IF                                                               
119100     MOVE '6021'       TO W-IDHTYP                                        
119200     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
119300     MOVE SPAR-IDJOB   TO W-IDJOB                                         
119400     PERFORM IMS-GET-6021-ROT                                             
119500     PERFORM IMS-GET-6021-JCL-KVAL-STORRE                                 
119600     SET MOD-IX-LINE TO 1                                                 
119700     PERFORM UNTIL MOD-IX-LINE > 8                                        
119800       IF SEGMENT-FINNS                                                   
119900         MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD (MOD-IX-LINE)             
120000         MOVE JCLD-JCL-TEJCL TO MOD-TEJCL (MOD-IX-LINE)                   
120100         PERFORM IMS-GET-6021-JCL-KVAL-STORRE                             
120200       ELSE                                                               
120300         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
120400                                 MOD-TEJCL (MOD-IX-LINE)                  
120500         MOVE MFS-FORMATETS-ATTR TO                                       
120600                  MOD-TEJCL-ATTR (MOD-IX-LINE)                            
120700       END-IF                                                             
120800       SET MOD-IX-LINE UP BY 1                                            
120900     END-PERFORM                                                          
121000     IF SEGMENT-FINNS                                                     
121100       MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD-NEXT                        
121200       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
121300       CALL WMEDKONV USING MED-WMEDAREA                                   
121400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
121500     ELSE                                                                 
121600       MOVE 100          TO MOD-IDJCLRAD-NEXT                             
121700     END-IF                                                               
121800     .                                                                    
121900     EJECT                                                                
122000 S01-REDIGERA-MOD SECTION.                                                
122100     SKIP2                                                                
122200     EVALUATE JCLD-JCL-IDJCLRAD                                           
122300         WHEN 10                                                          
122400           MOVE JCLD-JCL-TEJCL       TO MALL-RAD10                        
122500           MOVE MALL-IDJOB           TO MOD-IDJOB                         
122600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDJOB-ATTR                    
122700           MOVE MALL-KDDEBINFO       TO MOD-KDDEBINFO                     
122800           MOVE MALL-KDJROOM         TO MOD-KDJROOM                       
122900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJROOM-ATTR                  
123000           MOVE MALL-BEPGMNAMN       TO MOD-BEPGMNAMN                     
123100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEPGMNAMN-ATTR                
123200                                                                          
123300         WHEN 20                                                          
123400           MOVE JCLD-JCL-TEJCL       TO MALL-RAD20                        
123500           MOVE MALL-KDMSGCLASS      TO MOD-KDMSGCLASS                    
123600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGCLASS-ATTR               
123700           MOVE MALL-KDMSGLEVEL      TO MOD-KDMSGLEVEL                    
123800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGLEVEL-ATTR               
123900                                                                          
124000         WHEN 30                                                          
124100           IF VISA-MALL                                                   
124200             MOVE JCLD-JCL-TEJCL     TO MALL-RAD30                        
124300             MOVE MALL-KDJCLASS      TO MOD-KDJCLASS                      
124400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJCLASS-ATTR               
124500             MOVE MFS-RENSA-FAELT    TO MOD-IDNOTIFY                      
124600             MOVE MFS-FORMATETS-ATTR TO MOD-IDNOTIFY-ATTR                 
124700           ELSE                                                           
124800             MOVE JCLD-JCL-TEJCL     TO MALL-RAD30                        
124900             MOVE MALL-KDJCLASS      TO MOD-KDJCLASS                      
125000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJCLASS-ATTR               
125100             MOVE MALL-IDNOTIFY      TO MOD-IDNOTIFY                      
125200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDNOTIFY-ATTR               
125300           END-IF                                                         
125400                                                                          
125500         WHEN 40                                                          
125600           MOVE JCLD-JCL-TEJCL       TO MALL-RAD40                        
125700           MOVE MALL-KVJTIME         TO MOD-KVJTIME                       
125800           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVJTIME-ATTR                  
125900           MOVE MALL-KVJLINES        TO MOD-KVJLINES                      
126000           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVJLINES-ATTR                 
126100           MOVE MALL-KDJFORMS        TO MOD-KDJFORMS                      
126200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJFORMS-ATTR                 
126300                                                                          
126400         WHEN 47                                                          
126500           MOVE JCLD-JCL-TEJCL       TO MALL-RAD47                        
126600           MOVE MALL-KVJCOUNT        TO MOD-KVJCOUNT                      
126700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVJCOUNT-ATTR                 
126800                                                                          
126900         WHEN 50                                                          
127000           MOVE JCLD-JCL-TEJCL       TO MALL-RAD50                        
127100           MOVE MALL-KDROUTEX        TO MOD-KDROUTEX                      
127200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEX-ATTR                 
127300                                                                          
127400         WHEN 60                                                          
127500           MOVE JCLD-JCL-TEJCL       TO MALL-RAD60                        
127600           MOVE MALL-KDROUTEP        TO MOD-KDROUTEP                      
127700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEP-ATTR                 
127800                                                                          
127900         WHEN 70                                                          
128000           MOVE JCLD-JCL-TEJCL       TO MALL-RAD70                        
128100           MOVE MALL-KDOUTPUT        TO MOD-KDOUTPUT                      
128200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDOUTPUT-ATTR                 
128300                                                                          
128400         WHEN 80                                                          
128500           MOVE JCLD-JCL-TEJCL       TO MALL-RAD80                        
128600           MOVE MALL-IDPROCDD        TO MOD-IDPROCDD                      
128700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROCDD-ATTR                 
128800                                                                          
128900         WHEN OTHER                                                       
129000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEJCL-ATTR (MOD-IX-LINE)        
129100           MOVE JCLD-JCL-TEJCL     TO MOD-TEJCL      (MOD-IX-LINE)        
129200           MOVE JCLD-JCL-IDJCLRAD  TO MOD-IDJCLRAD   (MOD-IX-LINE)        
129300           SET MOD-IX-LINE MID-IX-LINE UP BY 1                            
129400                                                                          
129500     END-EVALUATE                                                         
129600     .                                                                    
129700     EJECT                                                                
129800* IMS SEKTIONER                                                           
129900     SKIP3                                                                
130000 IMS-GET-MSG SECTION.                                                     
130100                                                                          
130200     MOVE '  QC' TO GODK-STATUSKODER                                      
130300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
130400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130500     PERFORM IMS-STATUSKONTROLL                                           
130600     .                                                                    
130700     SKIP3                                                                
130800 IMS-INSERT-MSG SECTION.                                                  
130900                                                                          
131000     IF ENGLISH-TEXT                                                      
131100       MOVE 'N' TO MFS-KDHUVOMR                                           
131200     END-IF                                                               
131300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
131400     MOVE SPACE TO GODK-STATUSKODER                                       
131500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
131600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131700     PERFORM IMS-STATUSKONTROLL                                           
131800     .                                                                    
131900     EJECT                                                                
132000 IMS-GET-6001-KNTL SECTION.                                               
132100                                                                          
132200     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6001-X ')'                   
132300            DELIMITED BY SIZE INTO SSA1                                   
132400     STRING 'WLJCLA11(IDUSER   =' W-IDUSER ')'                            
132500            DELIMITED BY SIZE INTO SSA2                                   
132600     MOVE '  GE' TO GODK-STATUSKODER                                      
132700     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
132800     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
132900     PERFORM IMS-STATUSKONTROLL                                           
133000     .                                                                    
133100     EJECT                                                                
133200 IMS-GET-6011-ROT SECTION.                                                
133300                                                                          
133400     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
133500            DELIMITED BY SIZE INTO SSA1                                   
133600     MOVE '  GE' TO GODK-STATUSKODER                                      
133700     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1                      
133800     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
133900     PERFORM IMS-STATUSKONTROLL                                           
134000     .                                                                    
134100     SKIP3                                                                
134200 IMS-GET-6011-KNTL SECTION.                                               
134300                                                                          
134400     STRING 'WLJCLC01(WDP101KY =' W-WDP101KY-X ')'                        
134500            DELIMITED BY SIZE INTO SSA1                                   
134600     STRING 'WLJCLC11(IDUSER   =' W-IDUSER ')'                            
134700            DELIMITED BY SIZE INTO SSA2                                   
134800     MOVE '  GE' TO GODK-STATUSKODER                                      
134900     CALL CBLTDLI USING GU JCLC-PCB DLI-IO-AREA SSA1 SSA2                 
135000     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
135100     PERFORM IMS-STATUSKONTROLL                                           
135200     .                                                                    
135300     SKIP3                                                                
135400 IMS-GET-6011-JOB SECTION.                                                
135500                                                                          
135600     STRING 'WLJCLC01(WDP101KY =' W-WDP101KY-X ')'                        
135700            DELIMITED BY SIZE INTO SSA1                                   
135800     STRING 'WLJCLC12(IDJOB    =' W-IDJOB1 ')'                            
135900            DELIMITED BY SIZE INTO SSA2                                   
136000     MOVE '  GE' TO GODK-STATUSKODER                                      
136100     CALL CBLTDLI USING GHU JCLC-PCB DLI-IO-AREA SSA1 SSA2                
136200     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     .                                                                    
136500     SKIP3                                                                
136600 IMS-GET-6011-JCL SECTION.                                                
136700                                                                          
136800     MOVE 'WLJCLD12 ' TO SSA1                                             
136900     MOVE '  GE' TO GODK-STATUSKODER                                      
137000     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
137100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
137200     PERFORM IMS-STATUSKONTROLL                                           
137300     .                                                                    
137400     EJECT                                                                
137500 IMS-GET-6021-ROT SECTION.                                                
137600                                                                          
137700     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
137800            DELIMITED BY SIZE INTO SSA1                                   
137900     MOVE '  GE' TO GODK-STATUSKODER                                      
138000     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1                      
138100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400     SKIP3                                                                
138500 IMS-GET-6021-KNTL SECTION.                                               
138600                                                                          
138700     MOVE 'WLJCLD11 ' TO SSA1                                             
138800     MOVE '  ' TO GODK-STATUSKODER                                        
138900     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
139000     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
139100     PERFORM IMS-STATUSKONTROLL                                           
139200     .                                                                    
139300     EJECT                                                                
139400 IMS-GET-6021-JCL-KVAL SECTION.                                           
139500                                                                          
139600     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
139700            DELIMITED BY SIZE INTO SSA1                                   
139800     STRING 'WLJCLD12(IDJCLRAD =' W-IDJCLRAD-X ')'                        
139900            DELIMITED BY SIZE INTO SSA2                                   
140000     MOVE '  GE' TO GODK-STATUSKODER                                      
140100     CALL CBLTDLI USING GHU    JCLD-PCB DLI-IO-AREA SSA1 SSA2             
140200     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
140300     PERFORM IMS-STATUSKONTROLL                                           
140400     .                                                                    
140500     SKIP3                                                                
140600 IMS-GET-6021-JCL-KVAL-STORRE SECTION.                                    
140700                                                                          
140800     STRING 'WLJCLD12(IDJCLRAD>=' W-IDJCLRAD-X ')'                        
140900            DELIMITED BY SIZE INTO SSA1                                   
141000     MOVE '  GE' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
141200     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     SKIP3                                                                
141600 IMS-GET-6021-JCL-OKVAL SECTION.                                          
141700                                                                          
141800     MOVE 'WLJCLD12 ' TO SSA1                                             
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
142100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     EJECT                                                                
142500 IMS-INSERT-6011-JOB SECTION.                                             
142600                                                                          
142700     STRING 'WLJCLC01(WDP101KY =' W-WDP101KY-X ')'                        
142800            DELIMITED BY SIZE INTO SSA1                                   
142900     MOVE 'WLJCLC12 ' TO SSA2                                             
143000     MOVE '  ' TO GODK-STATUSKODER                                        
143100     CALL CBLTDLI USING ISRT JCLC-PCB DLI-IO-AREA SSA1 SSA2               
143200     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
143300     PERFORM IMS-STATUSKONTROLL                                           
143400     .                                                                    
143500     EJECT                                                                
143600 IMS-INSERT-6021-ROT SECTION.                                             
143700                                                                          
143800     MOVE 'WLJCLD01 ' TO SSA1                                             
143900     MOVE '  ' TO GODK-STATUSKODER                                        
144000     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1                    
144100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
144200     PERFORM IMS-STATUSKONTROLL                                           
144300     .                                                                    
144400     SKIP3                                                                
144500 IMS-INSERT-6021-KNTL SECTION.                                            
144600                                                                          
144700     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
144800            DELIMITED BY SIZE INTO SSA1                                   
144900     MOVE 'WLJCLD11 ' TO SSA2                                             
145000     MOVE '  ' TO GODK-STATUSKODER                                        
145100     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1 SSA2               
145200     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
145300     PERFORM IMS-STATUSKONTROLL                                           
145400     .                                                                    
145500     SKIP3                                                                
145600 IMS-INSERT-6021-JCL SECTION.                                             
145700                                                                          
145800     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
145900            DELIMITED BY SIZE INTO SSA1                                   
146000     MOVE 'WLJCLD12 ' TO SSA2                                             
146100     MOVE '  ' TO GODK-STATUSKODER                                        
146200     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1 SSA2               
146300     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
146400     PERFORM IMS-STATUSKONTROLL                                           
146500     .                                                                    
146600     EJECT                                                                
146700 IMS-REPLACE-6011-JOB SECTION.                                            
146800                                                                          
146900     MOVE '  ' TO GODK-STATUSKODER                                        
147000     CALL CBLTDLI USING REPL JCLC-PCB DLI-IO-AREA                         
147100     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
147200     PERFORM IMS-STATUSKONTROLL                                           
147300     .                                                                    
147400     SKIP3                                                                
147500 IMS-REPLACE-6021-JCL SECTION.                                            
147600                                                                          
147700     MOVE '  ' TO GODK-STATUSKODER                                        
147800     CALL CBLTDLI USING REPL JCLD-PCB DLI-IO-AREA                         
147900     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
148000     PERFORM IMS-STATUSKONTROLL                                           
148100     .                                                                    
148200     SKIP3                                                                
148300 IMS-DELETE SECTION.                                                      
148400                                                                          
148500     MOVE '  ' TO GODK-STATUSKODER                                        
148600     CALL CBLTDLI USING DLET JCLD-PCB DLI-IO-AREA                         
148700     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     .                                                                    
149000     EJECT                                                                
149100 IMS-STATUSKONTROLL SECTION.                                              
149200     SET STATUS-IX TO 1                                                   
149300     SEARCH GODK-STATUS                                                   
149400       AT END                                                             
149500         CALL FELLOG                                                      
149600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
149700         CONTINUE                                                         
149800     END-SEARCH.                                                          
