000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070700.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   JANUARI 1984                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   LÄGGER UPP JCL ENLIGT INPUT PÅ WDP1.                     
000800*    SKIP2                                                                
000900*    INDATA.                                                              
001000*        TRANSAKTION: W0T707                                              
001100*        MID:         W0I70701                                            
001200*    UTDATA.                                                              
001300*        MOD:         W0O70701                                            
001400*    SUBPROGRAM.                                                          
001500*        FELLOG                                                           
001600*        WMEDKONV                                                         
001700*    SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP3                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(8)    VALUE 'W0070700'.            
002600 77  JA                          PIC X(1)    VALUE 'J'.                   
002700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
002800 77  OK                          PIC X(1)    VALUE 'O'.                   
002900 77  FEL                         PIC X(1)    VALUE 'F'.                   
003000 77  SECURITY-TEST               PIC X(1)    VALUE 'O'.                   
003100 77  NYCKLAR-RETT                PIC X(1)    VALUE 'J'.                   
003200 77  NYCKLAR-NYA                 PIC X(1)    VALUE 'J'.                   
003300 77  TAB-IX                      PIC S9(9)   COMP SYNC.                   
003400 77  TAB-IX-MAX                  PIC S9(3)   COMP-3.                      
003500 77  MAX-RADER                   PIC S9(3)   VALUE +15 COMP-3.            
003600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1256 COMP SYNC.        
003700 77  SPAR-IDJCLRAD               PIC S9(5)   VALUE ZERO COMP-3.           
003800 77  SPRAK-IX                    PIC S9(9)   VALUE ZERO COMP SYNC.        
003900     SKIP2                                                                
004000 01  W-IDTRANS                   PIC X(4).                                
004100     88  EGEN-BILD                           VALUE '0707'.                
004200     EJECT                                                                
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
004700                                                                          
004800 01  FILLER.                                                              
004900     03  FILLER OCCURS 14.                                                
005000         05  TAB-LINE            PIC S9(3)   COMP-3.                      
005100     SKIP3                                                                
005200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
005300*    -COPY WMEDAREA                                                       
005400     EJECT                                                                
005500 01  MESSAGES-CODES.                                                      
005600     03  ERR-NOT-NUMERIC             PIC X(3)      VALUE '020'.           
005700     03  INF-UPDATE-DONE             PIC X(3)      VALUE '101'.           
005800     03  INF-MORE-INFO-EXISTS        PIC X(3)      VALUE '105'.           
005900     03  ERR-RTN-JOB-MISSING         PIC X(3)      VALUE '164'.           
006000     03  ERR-NOT-AUTHORIZED          PIC X(3)      VALUE '405'.           
006100     EJECT                                                                
006200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
006300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA  '.          
006400     SKIP2                                                                
006500*01  MID -COPY W0I70701                                                   
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
006800*01  -COPY WMSGAREA                                                       
006900     EJECT                                                                
007000     03  MOD REDEFINES MSG-AREA.                                          
007100*      05  -COPY W0O70701                                                 
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
007400*01  -COPY WMFSAREA                                                       
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700     SKIP2                                                                
007800 01  FILLER                      PIC X(16)   VALUE ' IMS-WS '.            
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-WDP101KY-6001-X.                                               
008200         05  FILLER              PIC X(4)    VALUE '6001'.                
008300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008400     SKIP2                                                                
008500     03  W-WDP101KY-6011-X.                                               
008600         05  FILLER              PIC X(4)    VALUE '6011'.                
008700         05  W-IDRUTIN-KNTL      PIC X(8).                                
008800         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
008900     SKIP2                                                                
009000     03  W-WDP101KY-X.                                                    
009100         05  W-IDHTYP            PIC X(4).                                
009200         05  W-IDRUTIN           PIC X(8).                                
009300         05  W-IDJOB             PIC X(8).                                
009400         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
009500     SKIP2                                                                
009600     03  W-IDUSER-X.                                                      
009700         05  W-IDUSER            PIC X(8).                                
009800     SKIP2                                                                
009900     03  W-IDJCLRAD-X.                                                    
010000         05  W-IDJCLRAD          PIC S9(5)   COMP-3.                      
010100     EJECT                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300     03  STATUS-WS               PIC X(2).                                
010400         88  SEGMENT-FINNS                   VALUE '  '.                  
010500         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
010600     SKIP3                                                                
010700     03  GODK-STATUSKODER.                                                
010800         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100 01  SSA2                        PIC X(64).                               
011200     EJECT                                                                
011300*                            IMS FUNKTIONSKODER                           
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600*                            DLI INPUT-OUTPUT AREA                        
011700 01  DLI-IO-AREA.                                                         
011800     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
011900     SKIP3                                                                
012000     03  WLJCLD01 REDEFINES IO-AREA.                                      
012100*      05  -COPY WDP101  -PRE JCLD-                                       
012200     EJECT                                                                
012300     03  WLJCLA11 REDEFINES IO-AREA.                                      
012400*      05  -COPY WDP111  -PRE JCLA-                                       
012500     EJECT                                                                
012600     03  WLJCLD12 REDEFINES IO-AREA.                                      
012700*      05  -COPY WDP114  -PRE JCLD-                                       
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000     SKIP2                                                                
013100*01  -COPY W0009     -PRE MSG-                                            
013200     EJECT                                                                
013300*01  -COPY W0008     -PRE JCLA-                                           
013400         05  FILLER              PIC X.                                   
013500     EJECT                                                                
013600*01  -COPY W0008     -PRE JCLD-                                           
013700         05  FILLER              PIC X.                                   
013800     EJECT                                                                
013900 PROCEDURE DIVISION USING MSG-PCB JCLA-PCB JCLD-PCB.                      
014000 MAIN SECTION.                                                            
014100     ENTRY 'DLITCBL' USING MSG-PCB JCLA-PCB JCLD-PCB.                     
014200                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT-SPARA-INPUT                                         
014600       IF NYCKLAR-RETT = JA                                               
014700         PERFORM IMS-GET-ROT                                              
014800         IF SEGMENT-FINNS                                                 
014900           PERFORM B-TESTA-SECURITY                                       
015000           IF SECURITY-TEST = OK                                          
015100             IF MFS-UPDATE                                                
015200               PERFORM C-UPPDATERA-JCL                                    
015300             END-IF                                                       
015400             IF NYCKLAR-RETT = JA                                         
015500               PERFORM D-VISA-JCL                                         
015600             END-IF                                                       
015700           END-IF                                                         
015800         ELSE                                                             
015900           MOVE ERR-RTN-JOB-MISSING TO MED-IDMFSFEL                       
016000           CALL WMEDKONV USING MED-WMEDAREA                               
016100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
016200         END-IF                                                           
016300       END-IF                                                             
016400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
016500       PERFORM IMS-INSERT-MSG                                             
016600     END-IF                                                               
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK.                                                              
017000     EJECT                                                                
017100 A-INIT-SPARA-INPUT SECTION.                                              
017200     SKIP2                                                                
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70701                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I70701                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
018400     MOVE MSG-IDPFK            TO MFS-IDPFK                               
018500     MOVE MFS-IDTRANS          TO W-IDTRANS                               
018600                                                                          
018700     MOVE LOW-VALUE TO MOD-W0O70701                                       
018800     MOVE 'W0O70701' TO MFS-IDMOD                                         
018900     MOVE '0707' TO MOD-IDTRANS                                           
019000                                                                          
019100     MOVE JA TO NYCKLAR-NYA                                               
019200     IF MID-IDRUTIN-IN = ALL '+'                                          
019300       IF MID-IDRUTIN-UT = ALL '+' OR SPACE                               
019400         MOVE LOW-VALUE TO W-IDRUTIN W-IDRUTIN-KNTL                       
019500         MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-UT                           
019600       ELSE                                                               
019700         MOVE MID-IDRUTIN-UT TO W-IDRUTIN MOD-IDRUTIN-UT                  
019800                                W-IDRUTIN-KNTL                            
019900       END-IF                                                             
020000     ELSE                                                                 
020100       MOVE MID-IDRUTIN-IN TO W-IDRUTIN MOD-IDRUTIN-UT                    
020200                              W-IDRUTIN-KNTL                              
020300     END-IF                                                               
020400                                                                          
020500     IF MID-IDJOB-IN = ALL '+'                                            
020600       IF MID-IDJOB-UT = ALL '+' OR SPACE                                 
020700         MOVE LOW-VALUE TO W-IDJOB                                        
020800         MOVE MFS-RENSA-FAELT TO MOD-IDJOB-UT                             
020900       ELSE                                                               
021000         MOVE MID-IDJOB-UT TO W-IDJOB MOD-IDJOB-UT                        
021100       END-IF                                                             
021200     ELSE                                                                 
021300       MOVE MID-IDJOB-IN TO W-IDJOB MOD-IDJOB-UT                          
021400     END-IF                                                               
021500                                                                          
021600     IF MID-IDRUTIN-IN = ALL '+' AND                                      
021700        MID-IDJOB-IN   = ALL '+'                                          
021800       MOVE NEJ TO NYCKLAR-NYA                                            
021900     END-IF                                                               
022000                                                                          
022100     IF W-IDJOB = LOW-VALUE                                               
022200       MOVE '6011' TO W-IDHTYP                                            
022300     ELSE                                                                 
022400       MOVE '6021' TO W-IDHTYP                                            
022500     END-IF                                                               
022600                                                                          
022700     IF NYCKLAR-NYA = JA                                                  
022800       MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD-IN                            
022900     ELSE                                                                 
023000       MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD-IN                          
023100     END-IF                                                               
023200                                                                          
023300     MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-IN                               
023400                             MOD-IDJOB-IN                                 
023500                             MOD-TEMFSFEL                                 
023600                             MOD-TEMFSINF                                 
023700     SET MOD-IX-LINE TO +1                                                
023800     PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1                            
023900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
024000                                 MOD-TEJCL (MOD-IX-LINE)                  
024100       SET MOD-IX-LINE UP BY +1                                           
024200     END-PERFORM                                                          
024300                                                                          
024400     IF MFS-IDTRANS NOT = '0707' OR MFS-IDPFK = '7'                       
024500       MOVE ' '  TO MFS-KDTRTYP                                           
024600       MOVE ZERO TO W-IDJCLRAD SPAR-IDJCLRAD                              
024700       MOVE JA   TO NYCKLAR-NYA                                           
024800     ELSE                                                                 
024900       IF MID-IDJCLRAD-IN NUMERIC AND MFS-IDPFK = '8'                     
025000         MOVE MID-IDJCLRAD-IN TO W-IDJCLRAD SPAR-IDJCLRAD                 
025100       ELSE                                                               
025200         IF MFS-IDPFK NOT = SPACE                                         
025300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDJCLRAD-IN-ATTR                 
025400           MOVE NEJ               TO NYCKLAR-RETT                         
025500           MOVE ERR-NOT-NUMERIC TO MED-IDMFSFEL                           
025600           CALL WMEDKONV USING MED-WMEDAREA                               
025700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
025800         END-IF                                                           
025900       END-IF                                                             
026000     END-IF                                                               
026100                                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 B-TESTA-SECURITY SECTION.                                                
026500     SKIP2                                                                
026600     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
026700     IF MFS-UPDATE                                                        
026800       PERFORM IMS-GET-6001-KNTL                                          
026900       IF SEGMENT-FINNS                                                   
027000         MOVE OK TO SECURITY-TEST                                         
027100       ELSE                                                               
027200         MOVE FEL TO SECURITY-TEST                                        
027300         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
027400         CALL WMEDKONV USING MED-WMEDAREA                                 
027500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
027600       END-IF                                                             
027700     ELSE                                                                 
027800       PERFORM IMS-GET-6011-KNTL                                          
027900       IF SEGMENT-FINNS                                                   
028000         MOVE OK TO SECURITY-TEST                                         
028100       ELSE                                                               
028200         PERFORM IMS-GET-6001-KNTL                                        
028300         IF SEGMENT-FINNS                                                 
028400           MOVE OK TO SECURITY-TEST                                       
028500         ELSE                                                             
028600           MOVE FEL TO SECURITY-TEST                                      
028700           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
028800           CALL WMEDKONV USING MED-WMEDAREA                               
028900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
029000         END-IF                                                           
029100       END-IF                                                             
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
029500 C-UPPDATERA-JCL SECTION.                                                 
029600     SKIP2                                                                
029700     SET MID-IX-LINE MOD-IX-LINE TO +1                                    
029800     IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC                                
029900       MOVE MID-IDJCLRAD (MID-IX-LINE) TO SPAR-IDJCLRAD                   
030000     END-IF                                                               
030100     PERFORM CA-KOLLA-INPUT                                               
030200     IF NYCKLAR-RETT = JA                                                 
030300       MOVE +1 TO TAB-IX                                                  
030400       PERFORM UNTIL TAB-IX > TAB-IX-MAX - 1                              
030500         SET MID-IX-LINE                 TO TAB-LINE (TAB-IX)             
030600         MOVE MID-IDJCLRAD (MID-IX-LINE) TO W-IDJCLRAD                    
030700         PERFORM IMS-GET-HOLD-JCL                                         
030800         IF SEGMENT-FINNS                                                 
030900           IF MID-TEJCL (MID-IX-LINE) = SPACE                             
031000             PERFORM IMS-DELETE                                           
031100           ELSE                                                           
031200             MOVE MID-TEJCL (MID-IX-LINE) TO JCLD-JCL-TEJCL               
031300             PERFORM IMS-REPLACE                                          
031400           END-IF                                                         
031500         ELSE                                                             
031600           MOVE MID-IDJCLRAD (MID-IX-LINE) TO JCLD-JCL-IDJCLRAD           
031700           MOVE MID-TEJCL (MID-IX-LINE)    TO JCLD-JCL-TEJCL              
031800           PERFORM IMS-INSERT-RAD                                         
031900         END-IF                                                           
032000         ADD +1 TO TAB-IX                                                 
032100       END-PERFORM                                                        
032200       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
032300       CALL WMEDKONV USING MED-WMEDAREA                                   
032400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 CA-KOLLA-INPUT SECTION.                                                  
032900     SKIP2                                                                
033000     MOVE +1 TO TAB-IX                                                    
033100     PERFORM UNTIL MID-IX-LINE > MAX-RADER - 1                            
033200       IF MID-TEJCL (MID-IX-LINE) NOT = ALL '+'                           
033300         MOVE MFS-ALFA-FAELT-RAETT TO                                     
033400              MOD-TEJCL-ATTR (MOD-IX-LINE)                                
033500         IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC                            
033600           SET TAB-LINE (TAB-IX) TO MID-IX-LINE                           
033700         ELSE                                                             
033800           MOVE MFS-NUM-FAELT-FEL TO                                      
033900                MOD-IDJCLRAD-ATTR (MOD-IX-LINE)                           
034000           MOVE NEJ TO NYCKLAR-RETT                                       
034100           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
034200           CALL WMEDKONV USING MED-WMEDAREA                               
034300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
034400         END-IF                                                           
034500         ADD +1 TO TAB-IX                                                 
034600       END-IF                                                             
034700       SET MID-IX-LINE MOD-IX-LINE UP BY +1                               
034800     END-PERFORM                                                          
034900     MOVE TAB-IX TO TAB-IX-MAX                                            
035000     .                                                                    
035100     EJECT                                                                
035200 D-VISA-JCL SECTION.                                                      
035300     SKIP2                                                                
035400     IF NYCKLAR-NYA = JA OR MFS-IDPFK = '7' OR '8' OR MFS-UPDATE          
035500       PERFORM IMS-GET-ROT                                                
035600       MOVE SPAR-IDJCLRAD TO W-IDJCLRAD                                   
035700       SET MOD-IX-LINE TO +1                                              
035800       PERFORM IMS-GET-JCL                                                
035900       PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1 OR SEGMENT-SAKNAS        
036000         MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD (MOD-IX-LINE)             
036100         MOVE JCLD-JCL-TEJCL    TO MOD-TEJCL (MOD-IX-LINE)                
036200         PERFORM IMS-GET-JCL                                              
036300         SET MOD-IX-LINE UP BY +1                                         
036400       END-PERFORM                                                        
036500       IF MOD-IX-LINE = MAX-RADER AND SEGMENT-FINNS                       
036600         MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD-IN                        
036700         IF NOT MFS-UPDATE                                                
036800           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
036900           CALL WMEDKONV USING MED-WMEDAREA                               
037000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
037100         END-IF                                                           
037200       END-IF                                                             
037300       PERFORM UNTIL MOD-IX-LINE > MAX-RADER - 1                          
037400         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
037500                                 MOD-TEJCL (MOD-IX-LINE)                  
037600                                 MOD-IDJCLRAD-IN                          
037700         SET MOD-IX-LINE UP BY +1                                         
037800       END-PERFORM                                                        
037900     END-IF.                                                              
038000     EJECT                                                                
038100* IMS SEKTIONER                                                           
038200     SKIP3                                                                
038300 IMS-GET-MSG SECTION.                                                     
038400     SKIP2                                                                
038500     MOVE '  QC' TO GODK-STATUSKODER                                      
038600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
038700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038800     PERFORM IMS-STATUSKONTROLL                                           
038900     SKIP3                                                                
039000     .                                                                    
039100 IMS-INSERT-MSG SECTION.                                                  
039200     SKIP2                                                                
039300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
039400     MOVE SPACE TO GODK-STATUSKODER                                       
039500     CALL CBLTDLI USING ISRT MSG-PCB                                      
039600                          MSG-IO-AREA MFS-IDMOD                           
039700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040000     EJECT                                                                
040100 IMS-GET-6001-KNTL SECTION.                                               
040200     SKIP2                                                                
040300     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6001-X ')'                   
040400            DELIMITED BY SIZE INTO SSA1                                   
040500     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
040600            DELIMITED BY SIZE INTO SSA2                                   
040700     MOVE '  GE' TO GODK-STATUSKODER                                      
040800     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
040900     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
041000     PERFORM IMS-STATUSKONTROLL                                           
041100     SKIP2                                                                
041200     .                                                                    
041300 IMS-GET-6011-KNTL SECTION.                                               
041400     SKIP2                                                                
041500     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6011-X ')'                   
041600            DELIMITED BY SIZE INTO SSA1                                   
041700     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
041800            DELIMITED BY SIZE INTO SSA2                                   
041900     MOVE '  GE' TO GODK-STATUSKODER                                      
042000     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
042100     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     EJECT                                                                
042500 IMS-GET-ROT SECTION.                                                     
042600     SKIP2                                                                
042700     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
042800            DELIMITED BY SIZE INTO SSA1                                   
042900     MOVE '  GE' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1                      
043100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     SKIP3                                                                
043400     .                                                                    
043500 IMS-GET-JCL SECTION.                                                     
043600     SKIP2                                                                
043700     STRING 'WLJCLD12(IDJCLRAD>=' W-IDJCLRAD-X ')'                        
043800            DELIMITED BY SIZE INTO SSA1                                   
043900     MOVE '  GE' TO GODK-STATUSKODER                                      
044000     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
044100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
044200     PERFORM IMS-STATUSKONTROLL                                           
044300     SKIP3                                                                
044400     .                                                                    
044500 IMS-GET-HOLD-JCL SECTION.                                                
044600     SKIP2                                                                
044700     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
044800            DELIMITED BY SIZE INTO SSA1                                   
044900     STRING 'WLJCLD12(IDJCLRAD =' W-IDJCLRAD-X ')'                        
045000            DELIMITED BY SIZE INTO SSA2                                   
045100     MOVE '  GE' TO GODK-STATUSKODER                                      
045200     CALL CBLTDLI USING GHU JCLD-PCB DLI-IO-AREA SSA1 SSA2                
045300     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     EJECT                                                                
045700 IMS-INSERT-RAD SECTION.                                                  
045800     SKIP2                                                                
045900     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
046000            DELIMITED BY SIZE INTO SSA1                                   
046100     MOVE 'WLJCLD12 ' TO SSA2                                             
046200     MOVE '  ' TO GODK-STATUSKODER                                        
046300     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1 SSA2               
046400     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     SKIP3                                                                
046700     .                                                                    
046800 IMS-REPLACE SECTION.                                                     
046900     SKIP2                                                                
047000     MOVE '  ' TO GODK-STATUSKODER                                        
047100     CALL CBLTDLI USING REPL JCLD-PCB DLI-IO-AREA                         
047200     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
047300     PERFORM IMS-STATUSKONTROLL                                           
047400     SKIP3                                                                
047500     .                                                                    
047600 IMS-DELETE SECTION.                                                      
047700     SKIP2                                                                
047800     MOVE '  ' TO GODK-STATUSKODER                                        
047900     CALL CBLTDLI USING DLET JCLD-PCB DLI-IO-AREA                         
048000     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
048100     PERFORM IMS-STATUSKONTROLL                                           
048200     .                                                                    
048300     EJECT                                                                
048400 IMS-STATUSKONTROLL SECTION.                                              
048500     SET STATUS-IX TO 1                                                   
048600     SEARCH GODK-STATUS AT END CALL FELLOG                                
048700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
048800     END-SEARCH.                                                          
