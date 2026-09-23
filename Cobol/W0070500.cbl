000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070500.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   AUGUSTI 1984                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   LÄGGER TILL, ÄNDRAR SAMT TAR BORT                        
000800*                DATAKORT ALT SUBMITTAR AKTUELLT JOBB.                    
000900*    SKIP2                                                                
001000*    INDATA.                                                              
001100*        TRANSAKTION: W0T705                                              
001200*        MID:         W0I70501                                            
001300*                     N0I70501                                            
001400*    UTDATA.                                                              
001500*        MOD:         W0O70501                                            
001600*                     N0O70501                                            
001700*                     W0O70901      SUBMITTA                              
001800*                     N0O70901      SUBMITTA                              
001900*    SUBPROGRAM.                                                          
002000*        FELLOG                                                           
002100*        WMEDKONV                                                         
002200*    SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(8)   VALUE 'W0070500'.             
003100 77  JA                          PIC X(1)    VALUE 'J'.                   
003200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003300 77  OK                          PIC X(1)    VALUE 'O'.                   
003400 77  FEL                         PIC X(1)    VALUE 'F'.                   
003500 77  SECURITY-TEST               PIC X(1)    VALUE 'F'.                   
003600 77  SUBMIT                      PIC X(1)    VALUE 'N'.                   
003700 77  ALT-ISRT                    PIC X(1)    VALUE 'N'.                   
003800 77  NYCKLAR-RETT                PIC X(1)    VALUE 'J'.                   
003900 77  TAB-IX                      PIC S9(9)   COMP SYNC.                   
004000 77  TAB-IX-MAX                  PIC S9(3)   COMP-3.                      
004100 77  MAX-RADER                   PIC S9(3)   VALUE +15 COMP-3.            
004200 77  MIN-MOD-LAENGD              PIC S9(4)   VALUE +49 COMP SYNC.         
004300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1257 COMP SYNC.        
004400 77  SPAR-IDJCLRAD               PIC S9(5)   VALUE ZERO COMP-3.           
004500 77  SPRAK-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
004600     EJECT                                                                
004700 01  W-IDTRANS                   PIC X(4).                                
004800     88  EGEN-BILD                           VALUE '0705'.                
004900     SKIP2                                                                
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005400                                                                          
005500 01  FILLER.                                                              
005600     03  FILLER OCCURS 14.                                                
005700         05  TAB-LINE            PIC S9(3)   COMP-3.                      
005800     EJECT                                                                
005900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006000*    -COPY WMEDAREA                                                       
006100     EJECT                                                                
006200 01  MESSAGES-CODES.                                                      
006300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006500     03  ERR-RTN-JOB-MISSING     PIC X(3)    VALUE '164'.                 
006600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
006700     EJECT                                                                
006800*    --- AREAOR FÖR MFS OCH SKÄRMHANTERING                                
006900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007000     SKIP2                                                                
007100*01  MID -COPY W0I70501                                                   
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
007400     SKIP2                                                                
007500*01  -COPY WMSGAREA                                                       
007600     EJECT                                                                
007700     03  MOD REDEFINES MSG-AREA.                                          
007800*      05  -COPY W0O70501                                                 
007900     EJECT                                                                
008000*01  MOD -COPY W0I70901 -PRE MOD-.                                        
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008300     SKIP2                                                                
008400*01  -COPY WMFSAREA                                                       
008500     EJECT                                                                
008600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008700     SKIP2                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03  W-WDP101KY-6001-X.                                               
009200         05  FILLER              PIC X(4)    VALUE '6001'.                
009300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
009400     SKIP2                                                                
009500     03  W-WDP101KY-6011-X.                                               
009600         05  FILLER              PIC X(4)    VALUE '6011'.                
009700         05  W-IDRUTIN-6011      PIC X(8).                                
009800         05  LOWVALUE            PIC X(18)   VALUE LOW-VALUE.             
009900     SKIP2                                                                
010000     03  W-WDP101KY-6021-X.                                               
010100         05  W-IDHTYP            PIC X(4)    VALUE '6021'.                
010200         05  W-IDRUTIN-6021      PIC X(8).                                
010300         05  W-IDJOB-6021        PIC X(8).                                
010400         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
010500     SKIP2                                                                
010600     03  W-IDJOB-X.                                                       
010700         05  W-IDJOB             PIC X(8).                                
010800     SKIP2                                                                
010900     03  W-IDUSER-X.                                                      
011000         05  W-IDUSER            PIC X(8).                                
011100     SKIP2                                                                
011200     03  W-IDJCLRAD-X.                                                    
011300         05  W-IDJCLRAD          PIC S9(5)   COMP-3.                      
011400     EJECT                                                                
011500*    --- STATUS-KOD FRÅN IMS                                              
011600     03  STATUS-WS               PIC X(2).                                
011700         88  SEGMENT-FINNS                   VALUE '  '.                  
011800         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
011900     SKIP3                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    --- DLI INPUT-OUTPUT AREA                                            
013000 01  DLI-IO-AREA.                                                         
013100     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
013200     SKIP3                                                                
013300     03  WLJCLD01 REDEFINES IO-AREA.                                      
013400*      05  -COPY WDP101  -PRE JCLD-                                       
013500     EJECT                                                                
013600     03  WLJCLD11 REDEFINES IO-AREA.                                      
013700*      05  -COPY WDP111  -PRE JCLD-                                       
013800     EJECT                                                                
013900     03  WLJCLC12 REDEFINES IO-AREA.                                      
014000*      05  -COPY WDP113  -PRE JCLC-                                       
014100     EJECT                                                                
014200     03  WLJCLD12 REDEFINES IO-AREA.                                      
014300*      05  -COPY WDP114  -PRE JCLD-                                       
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600     SKIP2                                                                
014700*01  -COPY W0009     -PRE MSG-                                            
014800     EJECT                                                                
014900*01  -COPY W0009     -PRE ALT-                                            
015000     EJECT                                                                
015100*01  -COPY W0008     -PRE JCLA-                                           
015200         05  FILLER              PIC X.                                   
015300     EJECT                                                                
015400*01  -COPY W0008     -PRE JCLC-                                           
015500         05  FILLER              PIC X.                                   
015600     EJECT                                                                
015700*01  -COPY W0008     -PRE JCLD-                                           
015800         05  FILLER              PIC X.                                   
015900     EJECT                                                                
016000 PROCEDURE DIVISION USING MSG-PCB ALT-PCB JCLA-PCB JCLC-PCB               
016100                                          JCLD-PCB.                       
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB JCLA-PCB JCLC-PCB              
016400                                           JCLD-PCB.                      
016500                                                                          
016600     PERFORM IMS-GET-MSG                                                  
016700     IF SEGMENT-FINNS                                                     
016800       PERFORM A-INIT-SPARA-INPUT                                         
016900       IF NYCKLAR-RETT = JA                                               
017000         PERFORM IMS-GET-6021-ROT                                         
017100         IF SEGMENT-FINNS                                                 
017200           PERFORM B-TESTA-SECURITY                                       
017300           IF SECURITY-TEST = OK                                          
017400             IF MFS-UPDATE                                                
017500               PERFORM C-UPPDATERA-DATA                                   
017600             END-IF                                                       
017700                                                                          
017800             IF SUBMIT = JA                                               
017900               PERFORM D-SPARKA-IGANG-W0T709                              
018000               PERFORM IMS-INSERT-ALT-MSG                                 
018100               MOVE JA TO ALT-ISRT                                        
018200             ELSE                                                         
018300               IF NYCKLAR-RETT = JA                                       
018400                 PERFORM E-VISA-DATA                                      
018500               END-IF                                                     
018600             END-IF                                                       
018700           END-IF                                                         
018800         ELSE                                                             
018900           MOVE ERR-RTN-JOB-MISSING TO MED-IDMFSFEL                       
019000           CALL WMEDKONV USING MED-WMEDAREA                               
019100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
019200         END-IF                                                           
019300       END-IF                                                             
019400                                                                          
019500       IF ALT-ISRT = NEJ                                                  
019600         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
019700         PERFORM IMS-INSERT-MSG                                           
019800       END-IF                                                             
019900     END-IF                                                               
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK.                                                              
020300     EJECT                                                                
020400 A-INIT-SPARA-INPUT SECTION.                                              
020500     SKIP2                                                                
020600     IF MSG-DUBBLA-TRANSKODER                                             
020700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70501                 
020800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021000     ELSE                                                                 
021100       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W0I70501                   
021200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021400     END-IF                                                               
021500                                                                          
021600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
021700     MOVE MSG-IDPFK            TO MFS-IDPFK                               
021800     MOVE MFS-IDTRANS          TO W-IDTRANS                               
021900                                                                          
022000     MOVE LOW-VALUE  TO MOD-W0O70501                                      
022100     MOVE 'W0O70501' TO MFS-IDMOD                                         
022200     MOVE '0705'     TO MOD-IDTRANS                                       
022300                                                                          
022400     IF MID-IDRUTIN-IN = ALL '+'                                          
022500       MOVE MID-IDRUTIN-UT TO W-IDRUTIN-6011 W-IDRUTIN-6021               
022600                            MOD-IDRUTIN-UT                                
022700     ELSE                                                                 
022800       MOVE MID-IDRUTIN-IN TO W-IDRUTIN-6011 W-IDRUTIN-6021               
022900                            MOD-IDRUTIN-UT                                
023000     END-IF                                                               
023100                                                                          
023200     IF MID-IDJOB-IN = ALL '+'                                            
023300       MOVE MID-IDJOB-UT TO W-IDJOB-6021 W-IDJOB MOD-IDJOB-UT             
023400     ELSE                                                                 
023500       MOVE MID-IDJOB-IN TO W-IDJOB-6021 W-IDJOB MOD-IDJOB-UT             
023600     END-IF                                                               
023700                                                                          
023800     MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-IN                               
023900                             MOD-IDJOB-IN                                 
024000                             MOD-FLKLAR                                   
024100                             MOD-TEMFSFEL                                 
024200                             MOD-TEMFSINF                                 
024300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD-IN                            
024400                                                                          
024500     SET MOD-IX-LINE TO +1                                                
024600     PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1                            
024700       MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
024800                                 MOD-TEJCL (MOD-IX-LINE)                  
024900       SET MOD-IX-LINE UP BY +1                                           
025000     END-PERFORM                                                          
025100                                                                          
025200     IF MFS-IDTRANS = '0705' AND MFS-IDPFK NOT = '7'                      
025300       IF MID-IDJCLRAD-IN NUMERIC                                         
025400         IF MID-IDJCLRAD-IN > 999                                         
025500           MOVE MID-IDJCLRAD-IN TO W-IDJCLRAD SPAR-IDJCLRAD               
025600         ELSE                                                             
025700           MOVE +1000 TO W-IDJCLRAD SPAR-IDJCLRAD                         
025800         END-IF                                                           
025900       ELSE                                                               
026000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDJCLRAD-IN-ATTR                   
026100         MOVE NEJ        TO NYCKLAR-RETT                                  
026200         MOVE ERR-CORR-HILITE-FLDS TO MOD-TEMFSFEL                        
026300         CALL WMEDKONV USING MED-WMEDAREA                                 
026400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
026500       END-IF                                                             
026600     ELSE                                                                 
026700       MOVE ' '   TO MFS-KDTRTYP                                          
026800       MOVE +1000 TO W-IDJCLRAD SPAR-IDJCLRAD                             
026900     END-IF                                                               
027000                                                                          
027100     MOVE NEJ TO SUBMIT ALT-ISRT                                          
027200     IF MID-FLKLAR = JA AND MFS-UPDATE                                    
027300       MOVE JA TO SUBMIT                                                  
027400     END-IF                                                               
027500                                                                          
027600     IF ENGLISH-TEXT                                                      
027700       MOVE +2    TO SPRAK-IX                                             
027800       MOVE 'GB ' TO MED-IDSKYLT                                          
027900     ELSE                                                                 
028000       MOVE +1    TO SPRAK-IX                                             
028100       MOVE 'S  ' TO MED-IDSKYLT                                          
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 B-TESTA-SECURITY SECTION.                                                
028600     SKIP2                                                                
028700     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
028800     PERFORM IMS-GET-6021-KNTL                                            
028900     IF SEGMENT-FINNS                                                     
029000       MOVE OK TO SECURITY-TEST                                           
029100     ELSE                                                                 
029200       PERFORM IMS-GET-6011-KNTL                                          
029300       IF SEGMENT-FINNS                                                   
029400         MOVE OK TO SECURITY-TEST                                         
029500       ELSE                                                               
029600         PERFORM IMS-GET-6001-KNTL                                        
029700         IF SEGMENT-FINNS                                                 
029800           MOVE OK TO SECURITY-TEST                                       
029900         ELSE                                                             
030000           MOVE FEL TO SECURITY-TEST                                      
030100           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
030200           CALL WMEDKONV USING MED-WMEDAREA                               
030300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
030400         END-IF                                                           
030500       END-IF                                                             
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 C-UPPDATERA-DATA SECTION.                                                
031000     SKIP2                                                                
031100     SET MID-IX-LINE MOD-IX-LINE TO +1                                    
031200     IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC                                
031300       MOVE MID-IDJCLRAD (MID-IX-LINE) TO SPAR-IDJCLRAD                   
031400     END-IF                                                               
031500     PERFORM CA-KOLLA-INPUT                                               
031600     IF NYCKLAR-RETT = JA                                                 
031700       MOVE +1 TO TAB-IX                                                  
031800       PERFORM UNTIL TAB-IX > TAB-IX-MAX - 1                              
031900         SET MID-IX-LINE TO TAB-LINE (TAB-IX)                             
032000         MOVE MID-IDJCLRAD (MID-IX-LINE) TO W-IDJCLRAD                    
032100         PERFORM IMS-GET-HOLD-6021-JCL                                    
032200         IF SEGMENT-FINNS                                                 
032300           IF MID-TEJCL (MID-IX-LINE) = SPACE                             
032400             PERFORM IMS-DELETE-6021                                      
032500           ELSE                                                           
032600             MOVE MID-TEJCL (MID-IX-LINE) TO JCLD-JCL-TEJCL               
032700             PERFORM IMS-REPLACE-6021                                     
032800           END-IF                                                         
032900         ELSE                                                             
033000           MOVE MID-IDJCLRAD (MID-IX-LINE) TO JCLD-JCL-IDJCLRAD           
033100           MOVE MID-TEJCL (MID-IX-LINE)    TO JCLD-JCL-TEJCL              
033200           PERFORM IMS-INSERT-6021                                        
033300         END-IF                                                           
033400         ADD +1 TO TAB-IX                                                 
033500       END-PERFORM                                                        
033600     END-IF                                                               
033700                                                                          
033800     PERFORM IMS-GET-6011-JOB                                             
033900     IF JCLC-JOB-KDTRSTAT = 3 OR 4                                        
034000       COMPUTE JCLC-JOB-KDTRSTAT = JCLC-JOB-KDTRSTAT - 2                  
034100       PERFORM IMS-REPLACE-6011                                           
034200     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500 CA-KOLLA-INPUT SECTION.                                                  
034600     SKIP2                                                                
034700     MOVE +1 TO TAB-IX                                                    
034800     PERFORM UNTIL MID-IX-LINE > MAX-RADER - 1                            
034900       IF MID-TEJCL (MID-IX-LINE) NOT = ALL '+'                           
035000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEJCL-ATTR (MOD-IX-LINE)        
035100         IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC AND                        
035200            MID-IDJCLRAD (MID-IX-LINE) > 999                              
035300           INSPECT MID-TEJCL (MID-IX-LINE) REPLACING                      
035400           ALL ' JOB ' BY '*FEL*'                                         
035500           SET TAB-LINE (TAB-IX) TO MID-IX-LINE                           
035600         ELSE                                                             
035700           MOVE MFS-NUM-FAELT-FEL TO                                      
035800                MOD-IDJCLRAD-ATTR (MOD-IX-LINE)                           
035900           MOVE NEJ TO NYCKLAR-RETT                                       
036000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
036100           CALL WMEDKONV USING MED-WMEDAREA                               
036200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
036300         END-IF                                                           
036400         ADD +1 TO TAB-IX                                                 
036500       END-IF                                                             
036600       SET MID-IX-LINE MOD-IX-LINE UP BY +1                               
036700     END-PERFORM                                                          
036800     MOVE TAB-IX TO TAB-IX-MAX                                            
036900     .                                                                    
037000     EJECT                                                                
037100 D-SPARKA-IGANG-W0T709 SECTION.                                           
037200     SKIP2                                                                
037300     MOVE W-IDRUTIN-6011 TO MOD-MID-IDRUTIN-IN                            
037400     MOVE W-IDJOB        TO MOD-MID-IDJOB-IN                              
037500                                                                          
037600     MOVE MIN-MOD-LAENGD   TO MSG-KVLL                                    
037700     MOVE 'W0T709U '       TO MSG-KDTRANS-1                               
037800     MOVE '0705'           TO MSG-IDTRANS-1                               
037900     MOVE SPRAK-IX         TO MSG-KDMFSFOR-1                              
038000     MOVE MOD-MID-W0I70901 TO MSG-INDATA-MINUS-1-TRANSKOD                 
038100     .                                                                    
038200     EJECT                                                                
038300 E-VISA-DATA SECTION.                                                     
038400     SKIP2                                                                
038500     PERFORM IMS-GET-6021-ROT                                             
038600     MOVE SPAR-IDJCLRAD TO W-IDJCLRAD                                     
038700     SET MOD-IX-LINE TO +1                                                
038800     PERFORM IMS-GET-6021-JCL                                             
038900     PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1 OR SEGMENT-SAKNAS          
039000       MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD (MOD-IX-LINE)               
039100       MOVE JCLD-JCL-TEJCL    TO MOD-TEJCL (MOD-IX-LINE)                  
039200       PERFORM IMS-GET-6021-JCL                                           
039300       SET MOD-IX-LINE UP BY +1                                           
039400     END-PERFORM                                                          
039500     IF MOD-IX-LINE = MAX-RADER AND SEGMENT-FINNS                         
039600       MOVE JCLD-JCL-IDJCLRAD    TO MOD-IDJCLRAD-IN                       
039700       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
039800       CALL WMEDKONV USING MED-WMEDAREA                                   
039900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
040000     END-IF                                                               
040100     PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1                            
040200       MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)                 
040300                               MOD-TEJCL (MOD-IX-LINE)                    
040400                               MOD-IDJCLRAD-IN                            
040500       SET MOD-IX-LINE UP BY +1                                           
040600     END-PERFORM.                                                         
040700     EJECT                                                                
040800* IMS SEKTIONER                                                           
040900     SKIP3                                                                
041000 IMS-GET-MSG SECTION.                                                     
041100     SKIP2                                                                
041200     MOVE '  QC' TO GODK-STATUSKODER                                      
041300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
041400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041500     PERFORM IMS-STATUSKONTROLL                                           
041600     SKIP3                                                                
041700     .                                                                    
041800 IMS-INSERT-MSG SECTION.                                                  
041900     SKIP2                                                                
042000     IF ENGLISH-TEXT                                                      
042100       MOVE 'N' TO MFS-KDHUVOMR                                           
042200     END-IF                                                               
042300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
042400     MOVE SPACE TO GODK-STATUSKODER                                       
042500     CALL CBLTDLI USING ISRT MSG-PCB                                      
042600                          MSG-IO-AREA MFS-IDMOD                           
042700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042800     PERFORM IMS-STATUSKONTROLL                                           
042900     SKIP3                                                                
043000     .                                                                    
043100 IMS-INSERT-ALT-MSG SECTION.                                              
043200     SKIP2                                                                
043300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
043400     MOVE SPACE TO GODK-STATUSKODER                                       
043500     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
043600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
043700     PERFORM IMS-STATUSKONTROLL                                           
043800     .                                                                    
043900     EJECT                                                                
044000 IMS-GET-6001-KNTL SECTION.                                               
044100                                                                          
044200     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6001-X ')'                   
044300            DELIMITED BY SIZE INTO SSA1                                   
044400     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
044500            DELIMITED BY SIZE INTO SSA2                                   
044600     MOVE '  GE' TO GODK-STATUSKODER                                      
044700     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
044800     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
044900     PERFORM IMS-STATUSKONTROLL                                           
045000     SKIP2                                                                
045100     .                                                                    
045200 IMS-GET-6011-KNTL SECTION.                                               
045300                                                                          
045400     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6011-X ')'                   
045500            DELIMITED BY SIZE INTO SSA1                                   
045600     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
045700            DELIMITED BY SIZE INTO SSA2                                   
045800     MOVE '  GE' TO GODK-STATUSKODER                                      
045900     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
046000     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
046100     PERFORM IMS-STATUSKONTROLL                                           
046200     SKIP2                                                                
046300     .                                                                    
046400 IMS-GET-6011-JOB SECTION.                                                
046500                                                                          
046600     STRING 'WLJCLC01(WDP101KY =' W-WDP101KY-6011-X ')'                   
046700            DELIMITED BY SIZE INTO SSA1                                   
046800     STRING 'WLJCLC12(IDJOB    =' W-IDJOB-X ')'                           
046900            DELIMITED BY SIZE INTO SSA2                                   
047000     MOVE '  ' TO GODK-STATUSKODER                                        
047100     CALL CBLTDLI USING GHU JCLC-PCB DLI-IO-AREA SSA1 SSA2                
047200     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
047300     PERFORM IMS-STATUSKONTROLL                                           
047400     .                                                                    
047500     EJECT                                                                
047600 IMS-GET-6021-ROT SECTION.                                                
047700     SKIP2                                                                
047800     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
047900            DELIMITED BY SIZE INTO SSA1                                   
048000     MOVE '  GE' TO GODK-STATUSKODER                                      
048100     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1                      
048200     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
048300     PERFORM IMS-STATUSKONTROLL                                           
048400     SKIP3                                                                
048500     .                                                                    
048600 IMS-GET-6021-KNTL SECTION.                                               
048700     SKIP2                                                                
048800     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6021-X ')'                   
048900            DELIMITED BY SIZE INTO SSA1                                   
049000     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
049100            DELIMITED BY SIZE INTO SSA2                                   
049200     MOVE '  GE' TO GODK-STATUSKODER                                      
049300     CALL CBLTDLI USING GU    JCLA-PCB DLI-IO-AREA SSA1 SSA2              
049400     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
049500     PERFORM IMS-STATUSKONTROLL                                           
049600     .                                                                    
049700     EJECT                                                                
049800 IMS-GET-6021-JCL SECTION.                                                
049900     SKIP2                                                                
050000     STRING 'WLJCLD12(IDJCLRAD>=' W-IDJCLRAD-X ')'                        
050100            DELIMITED BY SIZE INTO SSA1                                   
050200     MOVE '  GE' TO GODK-STATUSKODER                                      
050300     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
050400     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     SKIP3                                                                
050700     .                                                                    
050800 IMS-GET-HOLD-6021-JCL SECTION.                                           
050900     SKIP2                                                                
051000     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
051100            DELIMITED BY SIZE INTO SSA1                                   
051200     STRING 'WLJCLD12(IDJCLRAD =' W-IDJCLRAD-X ')'                        
051300            DELIMITED BY SIZE INTO SSA2                                   
051400     MOVE '  GE' TO GODK-STATUSKODER                                      
051500     CALL CBLTDLI USING GHU JCLD-PCB DLI-IO-AREA SSA1 SSA2                
051600     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
051700     PERFORM IMS-STATUSKONTROLL                                           
051800     .                                                                    
051900     EJECT                                                                
052000 IMS-REPLACE-6011 SECTION.                                                
052100     SKIP2                                                                
052200     MOVE '  ' TO GODK-STATUSKODER                                        
052300     CALL CBLTDLI USING REPL JCLC-PCB DLI-IO-AREA                         
052400     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     SKIP3                                                                
052700     .                                                                    
052800 IMS-INSERT-6021 SECTION.                                                 
052900     SKIP2                                                                
053000     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
053100            DELIMITED BY SIZE INTO SSA1                                   
053200     MOVE 'WLJCLD12 ' TO SSA2                                             
053300     MOVE '  ' TO GODK-STATUSKODER                                        
053400     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1 SSA2               
053500     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
053600     PERFORM IMS-STATUSKONTROLL                                           
053700     SKIP3                                                                
053800     .                                                                    
053900 IMS-REPLACE-6021 SECTION.                                                
054000     SKIP2                                                                
054100     MOVE '  ' TO GODK-STATUSKODER                                        
054200     CALL CBLTDLI USING REPL JCLD-PCB DLI-IO-AREA                         
054300     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     SKIP3                                                                
054600     .                                                                    
054700 IMS-DELETE-6021 SECTION.                                                 
054800     SKIP2                                                                
054900     MOVE '  ' TO GODK-STATUSKODER                                        
055000     CALL CBLTDLI USING DLET JCLD-PCB DLI-IO-AREA                         
055100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
055200     PERFORM IMS-STATUSKONTROLL                                           
055300     .                                                                    
055400     EJECT                                                                
055500 IMS-STATUSKONTROLL SECTION.                                              
055600     SET STATUS-IX TO 1                                                   
055700     SEARCH GODK-STATUS AT END CALL FELLOG                                
055800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
055900     END-SEARCH.                                                          
