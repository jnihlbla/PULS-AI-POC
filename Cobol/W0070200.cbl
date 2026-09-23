000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070200.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   MAJ 1984.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   LÄGGER UPP NYA RUTINER                                   
000800*                INKLUSIVE DEFAULT-JOBBKORT TILL DESSA.                   
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W0T702                                              
001200*        MID:         W0I70201                                            
001300*    UTDATA.                                                              
001400*        MOD:         W0O70201     OM INDATA FEL                          
001500                                                                          
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -- CHECKED BY WY2000                                                 
002400 77  IDPGM                       PIC X(8)    VALUE 'W0070200'.            
002500 77  JA                          PIC X(1)    VALUE 'J'.                   
002600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
002700 77  INDATA-RETT                 PIC X(1)    VALUE 'J'.                   
002800 77  OK                          PIC X(1)    VALUE 'O'.                   
002900 77  FEL                         PIC X(1)    VALUE 'F'.                   
003000 77  SECURITY-TEST               PIC X(1)    VALUE 'F'.                   
003100 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +641 COMP SYNC.        
003200 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
003300 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77  IXA                         PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77  DELETE-SW                   PIC X       VALUE 'N'.                   
003600     88 DELETE-OK                            VALUE 'J'.                   
003700                                                                          
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
004200                                                                          
004300 01  W-IDTRANS                   PIC X(4).                                
004400     88  EGEN-BILD                           VALUE '0702'.                
004500     SKIP3                                                                
004600 01  SW-NY-NYCKEL                PIC X(1).                                
004700     88  NY-NYCKEL                           VALUE 'J'.                   
004800     SKIP3                                                                
004900 01  SPAR-IDRUTIN                PIC X(8).                                
005000     EJECT                                                                
005100 01  IDJCLRAD-TABELL.                                                     
005200     03  FILLER OCCURS 12.                                                
005300         05  IDJCLRAD-IFYLLD     PIC S9(5).                               
005400     SKIP3                                                                
005500 01  TEJCL-TABELL.                                                        
005600     03  FILLER OCCURS 4.                                                 
005700         05  TEJCL-IFYLLD        PIC S9(1).                               
005800     SKIP3                                                                
005900 01  TEST-IDJOB.                                                          
006000     03  TEST-IDJOB-POS-1        PIC X(1).                                
006100     03  FILLER                  PIC X(7).                                
006200     EJECT                                                                
006300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006400     SKIP2                                                                
006500*    -COPY WMEDAREA                                                       
006600     EJECT                                                                
006700 01  MESSAGES-CODES.                                                      
006800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007200     03  INF-PRESS-PF1-FOR-INFO  PIC X(3)    VALUE '113'.                 
007300     03  ERR-ROUTINE-MISSING     PIC X(3)    VALUE '135'.                 
007400     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
007500     EJECT                                                                
007600*01  AREA -COPY W007W001 -PRE MALL-.                                      
007700     EJECT                                                                
007800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008000     SKIP2                                                                
008100*01  MID -COPY W0I70201                                                   
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
008400     SKIP2                                                                
008500*01  -COPY WMSGAREA                                                       
008600     EJECT                                                                
008700     03  MOD REDEFINES MSG-AREA.                                          
008800*      05  -COPY W0O70201                                                 
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009100     SKIP2                                                                
009200*01  -COPY WMFSAREA                                                       
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500     SKIP2                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-WDP101KY-X.                                                    
010000         05  W-IDHTYP            PIC X(4).                                
010100         05  W-IDRUTIN           PIC X(8).                                
010200         05  W-IDJOB             PIC X(8).                                
010300         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
010400     SKIP2                                                                
010500     03  W-IDUSER                PIC X(8).                                
010600     SKIP2                                                                
010700     03  W-IDRUTIN1-X.                                                    
010800         05  W-IDRUTIN1          PIC X(8).                                
010900     SKIP2                                                                
011000     03  W-IDJCLRAD-X.                                                    
011100         05  W-IDJCLRAD          PIC S9(5)   COMP-3.                      
011200     EJECT                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400     03  STATUS-WS               PIC X(2).                                
011500         88  SEGMENT-FINNS                   VALUE '  '.                  
011600         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
011700         88  SEGMENT-SLUT                    VALUE 'GB'.                  
011800     SKIP3                                                                
011900     03  GODK-STATUSKODER.                                                
012000         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    --- DLI INPUT-OUTPUT AREA                                            
012900 01  DLI-IO-AREA.                                                         
013000     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
013100     SKIP3                                                                
013200     03  WLJCLB01 REDEFINES IO-AREA.                                      
013300*      05  -COPY WDP101  -PRE JCLB-                                       
013400     EJECT                                                                
013500     03  WLJCLB11 REDEFINES IO-AREA.                                      
013600*      05  -COPY WDP111  -PRE JCLB-                                       
013700     EJECT                                                                
013800     03  WLJCLB12 REDEFINES IO-AREA.                                      
013900*      05  -COPY WDP112  -PRE JCLB-                                       
014000     EJECT                                                                
014100     03  WLJCLD12 REDEFINES IO-AREA.                                      
014200*      05  -COPY WDP114  -PRE JCLD-                                       
014300     EJECT                                                                
014400 LINKAGE SECTION.                                                         
014500                                                                          
014600*01  -COPY W0009     -PRE MSG-                                            
014700     EJECT                                                                
014800*01  -COPY W0008     -PRE JCLB-                                           
014900         05  FILLER              PIC X.                                   
015000                                                                          
015100*01  -COPY W0008     -PRE JCLD-                                           
015200         05  FILLER              PIC X.                                   
015300     EJECT                                                                
015400 PROCEDURE DIVISION USING MSG-PCB JCLB-PCB JCLD-PCB.                      
015500 MAIN SECTION.                                                            
015600     ENTRY 'DLITCBL' USING MSG-PCB JCLB-PCB JCLD-PCB.                     
015700                                                                          
015800     PERFORM IMS-GET-MSG                                                  
015900     IF SEGMENT-FINNS                                                     
016000       PERFORM A-INIT-SPARA-INPUT                                         
016100       PERFORM B-TESTA-SECURITY                                           
016200       IF SECURITY-TEST = OK                                              
016300         IF NY-NYCKEL OR MFS-IDPFK = '7'                                  
016400           PERFORM C-VISA-RUTIN                                           
016500         ELSE                                                             
016600           PERFORM D-KOLLA-INPUT                                          
016700           IF MFS-IDPFK = '8'                                             
016800             PERFORM G-PFK8-BLADDRA                                       
016900           ELSE                                                           
017000             IF INDATA-RETT = JA                                          
017100               IF MFS-UPDATE                                              
017200                  IF DELETE-OK                                            
017300                     PERFORM H-DELETE-RUTIN                               
017400                  ELSE                                                    
017500                     PERFORM E-UPPDATERA-RUTIN                            
017600                  END-IF                                                  
017700               ELSE                                                       
017800                 IF MID-IDJCLRAD-SKIP-IN NOT = ALL '+'                    
017900                   PERFORM F-VISA-MERA-JCL                                
018000                 ELSE                                                     
018100                   MOVE INF-PRESS-PF11 TO MED-IDMFSINF                    
018200                   CALL WMEDKONV USING MED-WMEDAREA                       
018300                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
018400                 END-IF                                                   
018500               END-IF                                                     
018600             ELSE                                                         
018700               MOVE ERR-ROUTINE-MISSING TO MED-IDMFSFEL                   
018800               CALL WMEDKONV USING MED-WMEDAREA                           
018900               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
019000             END-IF                                                       
019100           END-IF                                                         
019200         END-IF                                                           
019300       END-IF                                                             
019400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
019500       PERFORM IMS-INSERT-MSG                                             
019600     END-IF                                                               
019700                                                                          
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 A-INIT-SPARA-INPUT SECTION.                                              
020300                                                                          
020400     IF MSG-DUBBLA-TRANSKODER                                             
020500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70201                 
020600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020800     ELSE                                                                 
020900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I70201                  
021000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021200     END-IF                                                               
021300                                                                          
021400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
021500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
021600     MOVE MFS-IDTRANS          TO W-IDTRANS                               
021700                                                                          
021800     MOVE LOW-VALUE  TO MOD-W0O70201                                      
021900     MOVE 'W0O70201' TO MFS-IDMOD                                         
022000     MOVE '0702'     TO MOD-IDTRANS                                       
022100                                                                          
022200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
022300                             MOD-TEMFSINF                                 
022400                             MOD-IDRUTIN-IN                               
022500                             MOD-IDJCLRAD-SKIP-IN                         
022600     MOVE MFS-ROER-EJ-FAELT TO MOD-BERUTIN                                
022700                               MOD-IDOWNER                                
022800                               MOD-IDJOB                                  
022900                               MOD-IDJCLRAD-SPAR                          
023000                               MOD-KDDEBINFO                              
023100                               MOD-KDJROOM                                
023200                               MOD-BEPGMNAMN                              
023300                               MOD-KDMSGCLASS                             
023400                               MOD-KDMSGLEVEL                             
023500                               MOD-KDJCLASS                               
023600                               MOD-KVJTIME                                
023700                               MOD-KVJLINES                               
023800                               MOD-KDJFORMS                               
023900                               MOD-KDROUTEX                               
024000                               MOD-KDROUTEP                               
024100                               MOD-IDPROCDD                               
024200                               MOD-KDOUTPUT                               
024300     SET MOD-IX-LINE TO +1                                                
024400     PERFORM UNTIL MOD-IX-LINE > 4                                        
024500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
024600                                 MOD-TEJCL (MOD-IX-LINE)                  
024700       SET MOD-IX-LINE UP BY +1                                           
024800     END-PERFORM                                                          
024900                                                                          
025000     IF MID-IDRUTIN-IN = ALL '+'                                          
025100       MOVE NEJ TO SW-NY-NYCKEL                                           
025200       IF MID-IDRUTIN-UT = ALL '+'                                        
025300         MOVE SPACE TO SPAR-IDRUTIN                                       
025400         MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-UT                           
025500       ELSE                                                               
025600         MOVE MID-IDRUTIN-UT TO SPAR-IDRUTIN MOD-IDRUTIN-UT               
025700       END-IF                                                             
025800     ELSE                                                                 
025900       MOVE MID-IDRUTIN-IN TO SPAR-IDRUTIN MOD-IDRUTIN-UT                 
026000       MOVE JA             TO SW-NY-NYCKEL                                
026100       MOVE SPACE          TO MFS-KDTRTYP                                 
026200     END-IF                                                               
026300                                                                          
026400     IF MID-IDJOB-IN = ALL '+'                                            
026500       IF MID-IDJOB-UT = ALL '+'                                          
026600         MOVE MFS-RENSA-FAELT TO MOD-IDJOB-UT                             
026700       ELSE                                                               
026800         MOVE MID-IDJOB-UT TO MOD-IDJOB-UT                                
026900       END-IF                                                             
027000     ELSE                                                                 
027100       MOVE MID-IDJOB-IN TO MOD-IDJOB-UT                                  
027200     END-IF                                                               
027300                                                                          
027400     IF NOT EGEN-BILD                                                     
027500       MOVE JA    TO SW-NY-NYCKEL                                         
027600       MOVE SPACE TO MFS-KDTRTYP                                          
027700     END-IF                                                               
027800                                                                          
027900     MOVE JA TO INDATA-RETT                                               
028000     .                                                                    
028100     EJECT                                                                
028200 B-TESTA-SECURITY SECTION.                                                
028300                                                                          
028400     MOVE FEL TO SECURITY-TEST                                            
028500                                                                          
028600     MOVE '6001'            TO W-IDHTYP                                   
028700     MOVE LOW-VALUE         TO W-IDRUTIN W-IDJOB                          
028800     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
028900     PERFORM IMS-GET-6001-KNTL                                            
029000     IF SEGMENT-FINNS                                                     
029100       MOVE OK TO SECURITY-TEST                                           
029200     ELSE                                                                 
029300       MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                            
029400       CALL WMEDKONV USING MED-WMEDAREA                                   
029500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 C-VISA-RUTIN SECTION.                                                    
030000                                                                          
030100     MOVE '6001'       TO W-IDHTYP                                        
030200     MOVE LOW-VALUE    TO W-IDJOB W-IDRUTIN                               
030300     MOVE SPAR-IDRUTIN TO W-IDRUTIN1                                      
030400     PERFORM IMS-GET-6001-RTN                                             
030500                                                                          
030600     IF SEGMENT-FINNS                                                     
030700       PERFORM CA-VISA-RUTIN-SOM-FINNS                                    
030800     ELSE                                                                 
030900       PERFORM CB-VISA-MALL                                               
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300 CA-VISA-RUTIN-SOM-FINNS SECTION.                                         
031400                                                                          
031500     MOVE MFS-ALFA-FAELT-RAETT TO MOD-BERUTIN-ATTR                        
031600     MOVE JCLB-RTN-BERUTIN     TO MOD-BERUTIN                             
031700     MOVE '6011'               TO W-IDHTYP                                
031800     MOVE SPAR-IDRUTIN         TO W-IDRUTIN                               
031900     PERFORM IMS-GET-6011-ROT                                             
032000     PERFORM IMS-GET-6011-KNTL                                            
032100     MOVE JCLB-KNTL-IDOWNER    TO MOD-IDOWNER                             
032200     PERFORM IMS-GET-6011-JCL-OKVAL                                       
032300     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
032400*    DOWHILE SEGMENT-FINNS AND JCLD-JCL-IDJCLRAD < 1000 AND               
032500*                              MID-IX-LINE < 5                            
032600     PERFORM UNTIL SEGMENT-SAKNAS OR JCLD-JCL-IDJCLRAD > 999  OR          
032700                               MID-IX-LINE > 4                            
032800       PERFORM CAA-REDIGERA-MOD                                           
032900       PERFORM IMS-GET-6011-JCL-OKVAL                                     
033000     END-PERFORM                                                          
033100                                                                          
033200     IF SEGMENT-FINNS                                                     
033300       MOVE JCLD-JCL-IDJCLRAD     TO MOD-IDJCLRAD-SPAR                    
033400       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
033500       CALL WMEDKONV USING MED-WMEDAREA                                   
033600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
033700     END-IF                                                               
033800                                                                          
033900     PERFORM UNTIL MOD-IX-LINE > 4                                        
034000       MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)                 
034100                               MOD-TEJCL (MOD-IX-LINE)                    
034200       SET MOD-IX-LINE UP BY 1                                            
034300     END-PERFORM                                                          
034400     .                                                                    
034500     EJECT                                                                
034600 CAA-REDIGERA-MOD SECTION.                                                
034700                                                                          
034800     EVALUATE JCLD-JCL-IDJCLRAD                                           
034900         WHEN 10                                                          
035000           MOVE JCLD-JCL-TEJCL       TO MALL-RAD10                        
035100           MOVE MALL-IDJOB           TO MOD-IDJOB                         
035200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDJOB-ATTR                    
035300           MOVE MALL-KDDEBINFO       TO MOD-KDDEBINFO                     
035400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDEBINFO-ATTR                
035500           MOVE MALL-KDJROOM         TO MOD-KDJROOM                       
035600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJROOM-ATTR                  
035700           MOVE MALL-BEPGMNAMN       TO MOD-BEPGMNAMN                     
035800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEPGMNAMN-ATTR                
035900                                                                          
036000         WHEN 20                                                          
036100           MOVE JCLD-JCL-TEJCL       TO MALL-RAD20                        
036200           MOVE MALL-KDMSGCLASS      TO MOD-KDMSGCLASS                    
036300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGCLASS-ATTR               
036400           MOVE MALL-KDMSGLEVEL      TO MOD-KDMSGLEVEL                    
036500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGLEVEL-ATTR               
036600                                                                          
036700         WHEN 30                                                          
036800           MOVE JCLD-JCL-TEJCL       TO MALL-RAD30                        
036900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJCLASS-ATTR                 
037000           MOVE MALL-KDJCLASS        TO MOD-KDJCLASS                      
037100                                                                          
037200         WHEN 40                                                          
037300           MOVE JCLD-JCL-TEJCL      TO MALL-RAD40                         
037400           MOVE MALL-KVJTIME        TO MOD-KVJTIME                        
037500           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVJTIME-ATTR                   
037600           MOVE MALL-KVJLINES       TO MOD-KVJLINES                       
037700           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVJLINES-ATTR                  
037800           MOVE MALL-KDJFORMS       TO MOD-KDJFORMS                       
037900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJFORMS-ATTR                 
038000                                                                          
038100         WHEN 50                                                          
038200           MOVE JCLD-JCL-TEJCL       TO MALL-RAD50                        
038300           MOVE MALL-KDROUTEX        TO MOD-KDROUTEX                      
038400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEX-ATTR                 
038500                                                                          
038600         WHEN 60                                                          
038700           MOVE JCLD-JCL-TEJCL       TO MALL-RAD60                        
038800           MOVE MALL-KDROUTEP        TO MOD-KDROUTEP                      
038900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEP-ATTR                 
039000                                                                          
039100         WHEN 70                                                          
039200           MOVE JCLD-JCL-TEJCL       TO MALL-RAD70                        
039300           MOVE MALL-KDOUTPUT        TO MOD-KDOUTPUT                      
039400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDOUTPUT-ATTR                 
039500                                                                          
039600         WHEN 80                                                          
039700           MOVE JCLD-JCL-TEJCL       TO MALL-RAD80                        
039800           MOVE MALL-IDPROCDD        TO MOD-IDPROCDD                      
039900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROCDD-ATTR                 
040000                                                                          
040100         WHEN OTHER                                                       
040200           IF JCLD-JCL-IDJCLRAD > 99                                      
040300               MOVE MFS-ALFA-FAELT-RAETT TO                               
040400                               MOD-TEJCL-ATTR (MOD-IX-LINE)               
040500               MOVE JCLD-JCL-TEJCL TO MOD-TEJCL (MOD-IX-LINE)             
040600               MOVE JCLD-JCL-IDJCLRAD TO                                  
040700                         MOD-IDJCLRAD (MOD-IX-LINE)                       
040800                                                                          
040900               SET MOD-IX-LINE MID-IX-LINE UP BY 1                        
041000           END-IF                                                         
041100     END-EVALUATE                                                         
041200     .                                                                    
041300     EJECT                                                                
041400 CB-VISA-MALL SECTION.                                                    
041500                                                                          
041600     MOVE MFS-RENSA-FAELT      TO MOD-BERUTIN                             
041700     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDOWNER-ATTR                        
041800     MOVE MSG-SIGNON-USERID    TO MOD-IDOWNER                             
041900     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDJOB-ATTR                          
042000     MOVE MFS-RENSA-FAELT      TO MOD-IDJOB                               
042100     MOVE MFS-RENSA-FAELT      TO MOD-KDDEBINFO                           
042200     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJROOM-ATTR                        
042300     MOVE 'PVV2'               TO MOD-KDJROOM                             
042400     MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEPGMNAMN-ATTR                      
042500     MOVE MFS-RENSA-FAELT      TO MOD-BEPGMNAMN                           
042600     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGCLASS-ATTR                     
042700     MOVE 'A'                  TO MOD-KDMSGCLASS                          
042800     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGLEVEL-ATTR                     
042900     MOVE '1,1'                TO MOD-KDMSGLEVEL                          
043000     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJCLASS-ATTR                       
043100     MOVE 'N'                  TO MOD-KDJCLASS                            
043200     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVJTIME-ATTR                        
043300     MOVE 1                    TO MOD-KVJTIME                             
043400     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVJLINES-ATTR                       
043500     MOVE 9                    TO MOD-KVJLINES                            
043600     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJFORMS-ATTR                       
043700     MOVE 'STD'                TO MOD-KDJFORMS                            
043800     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEX-ATTR                       
043900     MOVE 'LOCAL '             TO MOD-KDROUTEX                            
044000     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEP-ATTR                       
044100     MOVE 'NJOV1'              TO MOD-KDROUTEP                            
044200     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROCDD-ATTR                       
044300     MOVE 'PROD'               TO MOD-IDPROCDD                            
044400     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDOUTPUT-ATTR                       
044500     MOVE 'CSYS FORMS=STD'     TO MOD-KDOUTPUT                            
044600                                                                          
044700     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
044800     PERFORM UNTIL  MOD-IX-LINE > 4                                       
044900       MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)                 
045000                               MOD-TEJCL (MOD-IX-LINE)                    
045100       SET MID-IX-LINE MOD-IX-LINE UP BY 1                                
045200     END-PERFORM                                                          
045300     MOVE ERR-ROUTINE-MISSING TO MED-IDMFSFEL                             
045400     CALL WMEDKONV USING MED-WMEDAREA                                     
045500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
045600     .                                                                    
045700     EJECT                                                                
045800 D-KOLLA-INPUT SECTION.                                                   
045900                                                                          
046000     MOVE 1 TO IX                                                         
046100     PERFORM UNTIL IX > 12                                                
046200       MOVE ZERO TO IDJCLRAD-IFYLLD (IX)                                  
046300       ADD +1 TO IX                                                       
046400     END-PERFORM                                                          
046500                                                                          
046600     PERFORM DA-KOLLA-RUTIN                                               
046700     PERFORM DB-RAD10                                                     
046800     PERFORM DC-RAD20                                                     
046900     PERFORM DD-RAD30                                                     
047000     PERFORM DE-RAD40                                                     
047100     PERFORM DF-RAD50                                                     
047200     PERFORM DG-RAD60                                                     
047300     PERFORM DH-RAD70                                                     
047400     PERFORM DJ-RAD80                                                     
047500     PERFORM DI-RESTEN-AV-RADER                                           
047600     .                                                                    
047700     EJECT                                                                
047800 DA-KOLLA-RUTIN SECTION.                                                  
047900                                                                          
048000     IF MID-BERUTIN = ALL '+'                                             
048100       MOVE MFS-ALFA-FAELT-FEL TO MOD-BERUTIN-ATTR                        
048200       MOVE NEJ TO INDATA-RETT                                            
048300     ELSE                                                                 
048400       IF MID-BERUTIN = SPACE                                             
048500         MOVE MFS-ALFA-FAELT-FEL TO MOD-BERUTIN-ATTR                      
048600         MOVE NEJ TO INDATA-RETT                                          
048700       ELSE                                                               
048800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BERUTIN-ATTR                    
048900       END-IF                                                             
049000     END-IF                                                               
049100                                                                          
049200     IF MID-IDOWNER NOT = ALL '+'                                         
049300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDOWNER-ATTR                      
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 DB-RAD10 SECTION.                                                        
049800                                                                          
049900     MOVE 1 TO IX                                                         
050000     IF MID-IDJOB = ALL '+'                                               
050100       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDJOB-ATTR                          
050200       MOVE NEJ TO INDATA-RETT                                            
050300     ELSE                                                                 
050400       IF MID-IDJOB NOT = SPACE                                           
050500         MOVE MID-IDJOB TO TEST-IDJOB                                     
050600         IF TEST-IDJOB-POS-1 ALPHABETIC AND                               
050700            TEST-IDJOB-POS-1 NOT = SPACE                                  
050800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDJOB-ATTR                    
050900           MOVE 10 TO IDJCLRAD-IFYLLD (IX)                                
051000         ELSE                                                             
051100           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDJOB-ATTR                      
051200           MOVE NEJ TO INDATA-RETT                                        
051300         END-IF                                                           
051400       ELSE                                                               
051500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDJOB-ATTR                      
051600         MOVE 10 TO IDJCLRAD-IFYLLD (IX)                                  
051700       END-IF                                                             
051800     END-IF                                                               
051900                                                                          
052000     IF MID-KDDEBINFO = ALL '+'                                           
052100       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDEBINFO-ATTR                      
052200       MOVE NEJ TO INDATA-RETT                                            
052300     ELSE                                                                 
052400       INSPECT MID-KDDEBINFO TALLYING TALLY FOR ALL SPACE                 
052500       IF TALLY > 0                                                       
052600         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDEBINFO-ATTR                    
052700         MOVE NEJ TO INDATA-RETT                                          
052800       ELSE                                                               
052900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDEBINFO-ATTR                  
053000         MOVE 10 TO IDJCLRAD-IFYLLD (IX)                                  
053100       END-IF                                                             
053200     END-IF                                                               
053300                                                                          
053400     IF MID-KDJROOM = ALL '+'                                             
053500       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJROOM-ATTR                        
053600       MOVE NEJ TO INDATA-RETT                                            
053700     ELSE                                                                 
053800       INSPECT MID-KDJROOM TALLYING TALLY FOR ALL SPACE                   
053900       IF TALLY > 0                                                       
054000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJROOM-ATTR                      
054100         MOVE NEJ TO INDATA-RETT                                          
054200       ELSE                                                               
054300         IF MID-KDJROOM = 'BORT'                                          
054400            MOVE JA TO DELETE-SW                                          
054500         END-IF                                                           
054600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJROOM-ATTR                    
054700         MOVE 10 TO IDJCLRAD-IFYLLD (IX)                                  
054800       END-IF                                                             
054900     END-IF                                                               
055000                                                                          
055100     IF MID-BEPGMNAMN = ALL '+'                                           
055200       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEPGMNAMN-ATTR                      
055300       MOVE NEJ TO INDATA-RETT                                            
055400     ELSE                                                                 
055500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEPGMNAMN-ATTR                    
055600       MOVE 10 TO IDJCLRAD-IFYLLD (IX)                                    
055700     END-IF                                                               
055800                                                                          
055900     IF IDJCLRAD-IFYLLD (IX) > 0                                          
056000       ADD +1 TO IX                                                       
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400 DC-RAD20 SECTION.                                                        
056500                                                                          
056600     IF MID-KDMSGCLASS = ALL '+'                                          
056700       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGCLASS-ATTR                     
056800       MOVE NEJ TO INDATA-RETT                                            
056900     ELSE                                                                 
057000       IF MID-KDMSGCLASS = SPACE                                          
057100         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGCLASS-ATTR                   
057200         MOVE NEJ TO INDATA-RETT                                          
057300       ELSE                                                               
057400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGCLASS-ATTR                 
057500         MOVE 20 TO IDJCLRAD-IFYLLD (IX)                                  
057600       END-IF                                                             
057700     END-IF                                                               
057800                                                                          
057900     IF MID-KDMSGLEVEL = ALL '+'                                          
058000       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGLEVEL-ATTR                     
058100       MOVE NEJ TO INDATA-RETT                                            
058200     ELSE                                                                 
058300       IF MID-KDMSGLEVEL = SPACE                                          
058400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDMSGLEVEL-ATTR                   
058500         MOVE NEJ TO INDATA-RETT                                          
058600       ELSE                                                               
058700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDMSGLEVEL-ATTR                 
058800         MOVE 20 TO IDJCLRAD-IFYLLD (IX)                                  
058900       END-IF                                                             
059000     END-IF                                                               
059100                                                                          
059200     IF IDJCLRAD-IFYLLD (IX) > 0                                          
059300       ADD +1 TO IX                                                       
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 DD-RAD30 SECTION.                                                        
059800                                                                          
059900     IF MID-KDJCLASS = ALL '+'                                            
060000       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJCLASS-ATTR                       
060100       MOVE NEJ TO INDATA-RETT                                            
060200     ELSE                                                                 
060300       IF MID-KDJCLASS = SPACE                                            
060400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJCLASS-ATTR                     
060500         MOVE NEJ TO INDATA-RETT                                          
060600       ELSE                                                               
060700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJCLASS-ATTR                   
060800         MOVE 30 TO IDJCLRAD-IFYLLD (IX)                                  
060900       END-IF                                                             
061000     END-IF                                                               
061100                                                                          
061200     IF IDJCLRAD-IFYLLD (IX) > 0                                          
061300       ADD +1 TO IX                                                       
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 DE-RAD40 SECTION.                                                        
061800                                                                          
061900     IF MID-KVJTIME NOT NUMERIC                                           
062000       MOVE MFS-NUM-FAELT-FEL TO MOD-KVJTIME-ATTR                         
062100       MOVE NEJ TO INDATA-RETT                                            
062200     ELSE                                                                 
062300       MOVE MFS-NUM-FAELT-RAETT TO MOD-KVJTIME-ATTR                       
062400       MOVE 40 TO IDJCLRAD-IFYLLD (IX)                                    
062500     END-IF                                                               
062600                                                                          
062700     IF MID-KVJLINES NOT NUMERIC                                          
062800       MOVE MFS-NUM-FAELT-FEL TO MOD-KVJLINES-ATTR                        
062900       MOVE NEJ TO INDATA-RETT                                            
063000     ELSE                                                                 
063100       MOVE MFS-NUM-FAELT-RAETT TO MOD-KVJLINES-ATTR                      
063200       MOVE 40 TO IDJCLRAD-IFYLLD (IX)                                    
063300     END-IF                                                               
063400                                                                          
063500     IF MID-KDJFORMS = ALL '+'                                            
063600       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJFORMS-ATTR                       
063700       MOVE NEJ TO INDATA-RETT                                            
063800     ELSE                                                                 
063900       IF MID-KDJFORMS = 'STD' OR MID-KDJFORMS NUMERIC                    
064000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDJFORMS-ATTR                   
064100         MOVE 40 TO IDJCLRAD-IFYLLD (IX)                                  
064200       ELSE                                                               
064300         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDJFORMS-ATTR                     
064400         MOVE NEJ TO INDATA-RETT                                          
064500       END-IF                                                             
064600     END-IF                                                               
064700                                                                          
064800     IF IDJCLRAD-IFYLLD (IX) > 0                                          
064900       ADD +1 TO IX                                                       
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 DF-RAD50 SECTION.                                                        
065400                                                                          
065500     IF MID-KDROUTEX = ALL '+'                                            
065600       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEX-ATTR                       
065700       MOVE NEJ TO INDATA-RETT                                            
065800     ELSE                                                                 
065900       IF MID-KDROUTEX = SPACE                                            
066000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEX-ATTR                     
066100         MOVE NEJ TO INDATA-RETT                                          
066200       ELSE                                                               
066300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEX-ATTR                   
066400         MOVE 50 TO IDJCLRAD-IFYLLD (IX)                                  
066500       END-IF                                                             
066600     END-IF                                                               
066700                                                                          
066800     IF IDJCLRAD-IFYLLD (IX) > 0                                          
066900       ADD +1 TO IX                                                       
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 DG-RAD60 SECTION.                                                        
067400                                                                          
067500     IF MID-KDROUTEP = ALL '+'                                            
067600       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEP-ATTR                       
067700       MOVE NEJ TO INDATA-RETT                                            
067800     ELSE                                                                 
067900       IF MID-KDROUTEP = SPACE                                            
068000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDROUTEP-ATTR                     
068100         MOVE NEJ TO INDATA-RETT                                          
068200       ELSE                                                               
068300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDROUTEP-ATTR                   
068400         MOVE 60 TO IDJCLRAD-IFYLLD (IX)                                  
068500       END-IF                                                             
068600     END-IF                                                               
068700                                                                          
068800     IF IDJCLRAD-IFYLLD (IX) > 0                                          
068900       ADD +1 TO IX                                                       
069000     END-IF                                                               
069100     .                                                                    
069200     EJECT                                                                
069300 DH-RAD70 SECTION.                                                        
069400                                                                          
069500     IF MID-KDOUTPUT = ALL '+'                                            
069600       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDOUTPUT-ATTR                       
069700       MOVE NEJ TO INDATA-RETT                                            
069800     ELSE                                                                 
069900       IF MID-KDOUTPUT = SPACE                                            
070000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDOUTPUT-ATTR                     
070100         MOVE NEJ TO INDATA-RETT                                          
070200       ELSE                                                               
070300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDOUTPUT-ATTR                   
070400         MOVE 70 TO IDJCLRAD-IFYLLD (IX)                                  
070500       END-IF                                                             
070600     END-IF                                                               
070700                                                                          
070800     IF IDJCLRAD-IFYLLD (IX) > 0                                          
070900       ADD +1 TO IX                                                       
071000     END-IF                                                               
071100     .                                                                    
071200     EJECT                                                                
071300 DI-RESTEN-AV-RADER SECTION.                                              
071400                                                                          
071500     MOVE 1 TO IXA                                                        
071600     SET MID-IX-LINE MOD-IX-LINE TO +1                                    
071700     PERFORM UNTIL MID-IX-LINE > 4                                        
071800       IF MID-TEJCL (MID-IX-LINE) NOT = ALL '+'                           
071900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEJCL-ATTR (MOD-IX-LINE)        
072000         IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC AND                        
072100            MID-IDJCLRAD (MID-IX-LINE) > 99 AND < 1000 OR > 9000          
072200           INSPECT MID-TEJCL (MID-IX-LINE) REPLACING                      
072300                          ALL ' JOB ' BY '*FEL*'                          
072400           MOVE MID-IDJCLRAD (MID-IX-LINE) TO IDJCLRAD-IFYLLD (IX)        
072500           SET TEJCL-IFYLLD (IXA)          TO MID-IX-LINE                 
072600           ADD 1 TO IX IXA                                                
072700         ELSE                                                             
072800           MOVE NEJ TO INDATA-RETT                                        
072900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDJCLRAD-ATTR (MOD-IX-LINE)        
073000         END-IF                                                           
073100       ELSE                                                               
073200         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
073300                                    MOD-TEJCL (MOD-IX-LINE)               
073400        MOVE MFS-FORMATETS-ATTR TO MOD-IDJCLRAD-ATTR (MOD-IX-LINE)        
073500                                      MOD-TEJCL-ATTR (MOD-IX-LINE)        
073600       END-IF                                                             
073700       SET MID-IX-LINE MOD-IX-LINE UP BY 1                                
073800     END-PERFORM                                                          
073900     .                                                                    
074000     EJECT                                                                
074100 DJ-RAD80 SECTION.                                                        
074200                                                                          
074300     IF MID-IDPROCDD = ALL '+'                                            
074400       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROCDD-ATTR                       
074500       MOVE NEJ TO INDATA-RETT                                            
074600     ELSE                                                                 
074700       IF MID-IDPROCDD = SPACE                                            
074800         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROCDD-ATTR                     
074900         MOVE NEJ TO INDATA-RETT                                          
075000       ELSE                                                               
075100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROCDD-ATTR                   
075200         MOVE 80 TO IDJCLRAD-IFYLLD (IX)                                  
075300       END-IF                                                             
075400     END-IF                                                               
075500                                                                          
075600     IF IDJCLRAD-IFYLLD (IX) > 0                                          
075700       ADD +1 TO IX                                                       
075800     END-IF                                                               
075900     .                                                                    
076000     EJECT                                                                
076100 E-UPPDATERA-RUTIN SECTION.                                               
076200                                                                          
076300     MOVE SPAR-IDRUTIN TO W-IDRUTIN1                                      
076400     PERFORM IMS-GET-6001-RTN                                             
076500     IF SEGMENT-FINNS                                                     
076600       PERFORM EA-AENDRA-RUTIN                                            
076700     ELSE                                                                 
076800       PERFORM EB-NY-RUTIN                                                
076900     END-IF                                                               
077000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
077100     CALL WMEDKONV USING MED-WMEDAREA                                     
077200     MOVE MED-MFSINF TO MOD-TEMFSINF.                                     
077300     EJECT                                                                
077400 EA-AENDRA-RUTIN SECTION.                                                 
077500                                                                          
077600     ACCEPT JCLB-RTN-TIUPPDAT FROM DATE                                   
077700     MOVE MID-BERUTIN TO JCLB-RTN-BERUTIN MOD-BERUTIN                     
077800     PERFORM IMS-REPLACE-6001-RTN                                         
077900                                                                          
078000     MOVE '6011'       TO W-IDHTYP                                        
078100     MOVE SPAR-IDRUTIN TO W-IDRUTIN                                       
078200     MOVE LOW-VALUE    TO W-IDJOB                                         
078300     PERFORM IMS-GET-6011-ROT                                             
078400                                                                          
078500     MOVE +1 TO IX IXA                                                    
078600     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
078700     PERFORM UNTIL IX > 12 OR IDJCLRAD-IFYLLD (IX) NOT > 0                
078800       MOVE IDJCLRAD-IFYLLD (IX) TO W-IDJCLRAD                            
078900       PERFORM IMS-GET-6011-JCL-KVAL                                      
079000       PERFORM EAA-FYLL-I-TEJCL                                           
079100       IF SEGMENT-FINNS                                                   
079200         IF W-IDJCLRAD > 99 AND MID-TEJCL (MID-IX-LINE) = SPACE           
079300           PERFORM IMS-DELETE                                             
079400         ELSE                                                             
079500           PERFORM IMS-REPLACE-6011-JCL                                   
079600         END-IF                                                           
079700       ELSE                                                               
079800         MOVE IDJCLRAD-IFYLLD (IX) TO JCLD-JCL-IDJCLRAD                   
079900         PERFORM IMS-INSERT-6011-JCL                                      
080000       END-IF                                                             
080100       ADD +1 TO IX                                                       
080200     END-PERFORM                                                          
080300                                                                          
080400     PERFORM IMS-GET-6011-ROT                                             
080500     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
080600     IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC                                
080700       MOVE MID-IDJCLRAD (MID-IX-LINE) TO W-IDJCLRAD                      
080800     ELSE                                                                 
080900       MOVE 100 TO W-IDJCLRAD                                             
081000     END-IF                                                               
081100     PERFORM UNTIL MOD-IX-LINE > 4                                        
081200       PERFORM IMS-GET-6011-JCL-KVAL-STORRE                               
081300       IF SEGMENT-FINNS                                                   
081400         IF JCLD-JCL-IDJCLRAD > 99                                        
081500           MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD (MOD-IX-LINE)           
081600           MOVE JCLD-JCL-TEJCL    TO MOD-TEJCL (MOD-IX-LINE)              
081700           SET MOD-IX-LINE UP BY 1                                        
081800         END-IF                                                           
081900       ELSE                                                               
082000         MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)               
082100                                 MOD-TEJCL (MOD-IX-LINE)                  
082200         SET MOD-IX-LINE UP BY 1                                          
082300       END-IF                                                             
082400     END-PERFORM.                                                         
082500     EJECT                                                                
082600 EAA-FYLL-I-TEJCL SECTION.                                                
082700                                                                          
082800     EVALUATE W-IDJCLRAD                                                  
082900         WHEN 10                                                          
083000             MOVE MID-IDJOB     TO MALL-IDJOB MOD-IDJOB                   
083100             MOVE MID-KDDEBINFO TO MALL-KDDEBINFO MOD-KDDEBINFO           
083200             MOVE MID-KDJROOM   TO MALL-KDJROOM MOD-KDJROOM               
083300             MOVE MID-BEPGMNAMN TO MALL-BEPGMNAMN MOD-BEPGMNAMN           
083400             MOVE MALL-RAD10    TO JCLD-JCL-TEJCL                         
083500                                                                          
083600         WHEN 20                                                          
083700             MOVE MID-KDMSGCLASS TO MALL-KDMSGCLASS MOD-KDMSGCLASS        
083800             MOVE MID-KDMSGLEVEL TO MALL-KDMSGLEVEL MOD-KDMSGLEVEL        
083900             MOVE MALL-RAD20     TO JCLD-JCL-TEJCL                        
084000                                                                          
084100         WHEN 30                                                          
084200             MOVE MID-KDJCLASS TO MALL-KDJCLASS MOD-KDJCLASS              
084300             MOVE SPACE        TO MALL-IDNOTIFY                           
084400             MOVE MALL-RAD30   TO JCLD-JCL-TEJCL                          
084500                                                                          
084600         WHEN 40                                                          
084700             MOVE MID-KVJTIME  TO MALL-KVJTIME MOD-KVJTIME                
084800             MOVE MID-KVJLINES TO MALL-KVJLINES MOD-KVJLINES              
084900             MOVE MID-KDJFORMS TO MALL-KDJFORMS MOD-KDJFORMS              
085000             MOVE MALL-RAD40   TO JCLD-JCL-TEJCL                          
085100                                                                          
085200         WHEN 50                                                          
085300             MOVE MID-KDROUTEX TO MALL-KDROUTEX MOD-KDROUTEX              
085400             MOVE MALL-RAD50   TO JCLD-JCL-TEJCL                          
085500                                                                          
085600         WHEN 60                                                          
085700             MOVE MID-KDROUTEP TO MALL-KDROUTEP MOD-KDROUTEP              
085800             MOVE MALL-RAD60   TO JCLD-JCL-TEJCL                          
085900                                                                          
086000         WHEN 70                                                          
086100             MOVE MID-KDOUTPUT TO MALL-KDOUTPUT MOD-KDOUTPUT              
086200             MOVE MALL-RAD70   TO JCLD-JCL-TEJCL                          
086300                                                                          
086400         WHEN 80                                                          
086500             MOVE MID-IDPROCDD TO MALL-IDPROCDD MOD-IDPROCDD              
086600             MOVE MALL-RAD80   TO JCLD-JCL-TEJCL                          
086700                                                                          
086800         WHEN OTHER                                                       
086900             SET MID-IX-LINE MOD-IX-LINE  TO TEJCL-IFYLLD (IXA)           
087000             MOVE MID-TEJCL (MID-IX-LINE) TO JCLD-JCL-TEJCL               
087100             ADD 1 TO IXA                                                 
087200                                                                          
087300     END-EVALUATE.                                                        
087400     EJECT                                                                
087500 EB-NY-RUTIN SECTION.                                                     
087600                                                                          
087700     MOVE '6001'       TO W-IDHTYP                                        
087800     MOVE LOW-VALUE    TO W-IDRUTIN W-IDJOB                               
087900     MOVE SPAR-IDRUTIN TO JCLB-RTN-IDRUTIN                                
088000     ACCEPT JCLB-RTN-TIREGDAT FROM DATE                                   
088100     MOVE ZERO        TO JCLB-RTN-TIUPPDAT                                
088200     MOVE MID-BERUTIN TO JCLB-RTN-BERUTIN MOD-BERUTIN                     
088300     PERFORM IMS-INSERT-6001-RTN                                          
088400                                                                          
088500     MOVE '6011'       TO JCLB-ROT-IDHTYP W-IDHTYP                        
088600     MOVE SPAR-IDRUTIN TO JCLB-ROT-IDRUTIN W-IDRUTIN                      
088700     MOVE LOW-VALUE    TO JCLB-ROT-IDJOB JCLB-ROT-LOWVALUE W-IDJOB        
088800     PERFORM IMS-INSERT-6011-ROT                                          
088900                                                                          
089000     IF MID-IDOWNER = ALL '+'                                             
089100       MOVE MSG-SIGNON-USERID TO JCLB-KNTL-IDOWNER                        
089200                                 JCLB-KNTL-IDUSER                         
089300                                 MOD-IDOWNER                              
089400     ELSE                                                                 
089500       MOVE MID-IDOWNER TO JCLB-KNTL-IDOWNER                              
089600                           JCLB-KNTL-IDUSER MOD-IDOWNER                   
089700     END-IF                                                               
089800     ACCEPT JCLB-KNTL-TIREGDAT FROM DATE                                  
089900     PERFORM IMS-INSERT-6011-KNTL                                         
090000                                                                          
090100     MOVE +1 TO IX IXA                                                    
090200     SET MID-IX-LINE MOD-IX-LINE TO 1                                     
090300     PERFORM UNTIL IX > 12                                                
090400       IF IDJCLRAD-IFYLLD (IX) > 0                                        
090500         MOVE IDJCLRAD-IFYLLD (IX) TO JCLD-JCL-IDJCLRAD W-IDJCLRAD        
090600         PERFORM EBA-FYLL-I-TEJCL                                         
090700         PERFORM IMS-INSERT-6011-JCL                                      
090800       END-IF                                                             
090900       ADD +1 TO IX                                                       
091000     END-PERFORM.                                                         
091100     EJECT                                                                
091200 EBA-FYLL-I-TEJCL SECTION.                                                
091300                                                                          
091400     EVALUATE W-IDJCLRAD                                                  
091500         WHEN 10                                                          
091600             MOVE MID-IDJOB     TO MALL-IDJOB MOD-IDJOB                   
091700             MOVE MID-KDDEBINFO TO MALL-KDDEBINFO MOD-KDDEBINFO           
091800             MOVE MID-KDJROOM   TO MALL-KDJROOM MOD-KDJROOM               
091900             MOVE MID-BEPGMNAMN TO MALL-BEPGMNAMN MOD-BEPGMNAMN           
092000             MOVE MALL-RAD10    TO JCLD-JCL-TEJCL                         
092100                                                                          
092200         WHEN 20                                                          
092300             MOVE MID-KDMSGCLASS TO MALL-KDMSGCLASS MOD-KDMSGCLASS        
092400             MOVE MID-KDMSGLEVEL TO MALL-KDMSGLEVEL MOD-KDMSGLEVEL        
092500             MOVE MALL-RAD20     TO JCLD-JCL-TEJCL                        
092600                                                                          
092700         WHEN 30                                                          
092800             MOVE MID-KDJCLASS TO MALL-KDJCLASS MOD-KDJCLASS              
092900             MOVE SPACE        TO MALL-IDNOTIFY                           
093000             MOVE MALL-RAD30   TO JCLD-JCL-TEJCL                          
093100                                                                          
093200         WHEN 40                                                          
093300             MOVE MID-KVJTIME  TO MALL-KVJTIME MOD-KVJTIME                
093400             MOVE MID-KVJLINES TO MALL-KVJLINES MOD-KVJLINES              
093500             MOVE MID-KDJFORMS TO MALL-KDJFORMS MOD-KDJFORMS              
093600             MOVE MALL-RAD40   TO JCLD-JCL-TEJCL                          
093700                                                                          
093800         WHEN 50                                                          
093900             MOVE MID-KDROUTEX TO MALL-KDROUTEX MOD-KDROUTEX              
094000             MOVE MALL-RAD50   TO JCLD-JCL-TEJCL                          
094100                                                                          
094200         WHEN 60                                                          
094300             MOVE MID-KDROUTEP TO MALL-KDROUTEP MOD-KDROUTEP              
094400             MOVE MALL-RAD60   TO JCLD-JCL-TEJCL                          
094500                                                                          
094600         WHEN 70                                                          
094700             MOVE MID-KDOUTPUT TO MALL-KDOUTPUT MOD-KDOUTPUT              
094800             MOVE MALL-RAD70   TO JCLD-JCL-TEJCL                          
094900                                                                          
095000         WHEN 80                                                          
095100             MOVE MID-IDPROCDD TO MALL-IDPROCDD MOD-IDPROCDD              
095200             MOVE MALL-RAD80   TO JCLD-JCL-TEJCL                          
095300                                                                          
095400         WHEN OTHER                                                       
095500             SET MID-IX-LINE MOD-IX-LINE TO TEJCL-IFYLLD (IXA)            
095600             IF MID-TEJCL (MID-IX-LINE) = ALL '+'                         
095700               MOVE MFS-RENSA-FAELT TO MOD-TEJCL (MOD-IX-LINE)            
095800                                      MOD-IDJCLRAD (MOD-IX-LINE)          
095900             ELSE                                                         
096000               MOVE MID-TEJCL (MID-IX-LINE) TO JCLD-JCL-TEJCL             
096100                    MOD-TEJCL (MOD-IX-LINE)                               
096200               MOVE MID-IDJCLRAD (MID-IX-LINE) TO                         
096300                    MOD-IDJCLRAD (MOD-IX-LINE)                            
096400             END-IF                                                       
096500             ADD 1 TO IXA                                                 
096600                                                                          
096700     END-EVALUATE.                                                        
096800     EJECT                                                                
096900 F-VISA-MERA-JCL SECTION.                                                 
097000                                                                          
097100     IF MID-IDJCLRAD-SKIP-IN NUMERIC AND                                  
097200        MID-IDJCLRAD-SKIP-IN > 99 AND < 1000 OR > 9000                    
097300       MOVE MID-IDJCLRAD-SKIP-IN TO MOD-IDJCLRAD-SKIP-UT                  
097400                                      W-IDJCLRAD                          
097500       MOVE '6011'       TO W-IDHTYP                                      
097600       MOVE SPAR-IDRUTIN TO W-IDRUTIN                                     
097700       PERFORM IMS-GET-6011-ROT                                           
097800       PERFORM IMS-GET-6011-JCL-KVAL-STORRE                               
097900       SET MOD-IX-LINE TO 1                                               
098000       PERFORM UNTIL MOD-IX-LINE > 4                                      
098100         IF SEGMENT-FINNS                                                 
098200           IF JCLD-JCL-IDJCLRAD > 99                                      
098300             MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD (MOD-IX-LINE)         
098400             MOVE JCLD-JCL-TEJCL    TO MOD-TEJCL (MOD-IX-LINE)            
098500             MOVE MFS-ALFA-FAELT-RAETT TO                                 
098600                       MOD-TEJCL-ATTR (MOD-IX-LINE)                       
098700           END-IF                                                         
098800           PERFORM IMS-GET-6011-JCL-KVAL-STORRE                           
098900         ELSE                                                             
099000           MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)             
099100                                   MOD-TEJCL (MOD-IX-LINE)                
099200           MOVE MFS-FORMATETS-ATTR TO                                     
099300                    MOD-TEJCL-ATTR (MOD-IX-LINE)                          
099400         END-IF                                                           
099500         SET MOD-IX-LINE UP BY 1                                          
099600       END-PERFORM                                                        
099700       IF SEGMENT-FINNS                                                   
099800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
099900         CALL WMEDKONV USING MED-WMEDAREA                                 
100000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
100100         MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD-SPAR                      
100200       END-IF                                                             
100300     ELSE                                                                 
100400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDJCLRAD-SKIP-IN                     
100500       MOVE MFS-NUM-FAELT-FEL TO MOD-IDJCLRAD-SKIP-ATTR                   
100600       MOVE ERR-ROUTINE-MISSING TO MED-IDMFSFEL                           
100700       CALL WMEDKONV USING MED-WMEDAREA                                   
100800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
100900     END-IF.                                                              
101000     EJECT                                                                
101100 G-PFK8-BLADDRA SECTION.                                                  
101200                                                                          
101300     IF MID-IDJCLRAD-SPAR > 99 AND < 1000 OR > 9000                       
101400       MOVE MID-IDJCLRAD-SPAR TO W-IDJCLRAD                               
101500       MOVE '6011'       TO W-IDHTYP                                      
101600       MOVE SPAR-IDRUTIN TO W-IDRUTIN                                     
101700       PERFORM IMS-GET-6011-ROT                                           
101800       PERFORM IMS-GET-6011-JCL-KVAL-STORRE                               
101900       SET MOD-IX-LINE TO 1                                               
102000       PERFORM UNTIL MOD-IX-LINE > 4                                      
102100         IF SEGMENT-FINNS                                                 
102200           IF JCLD-JCL-IDJCLRAD > 99 AND < 1000 OR > 9000                 
102300             MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD (MOD-IX-LINE)         
102400             MOVE JCLD-JCL-TEJCL    TO MOD-TEJCL (MOD-IX-LINE)            
102500             MOVE MFS-ALFA-FAELT-RAETT TO                                 
102600                        MOD-TEJCL-ATTR (MOD-IX-LINE)                      
102700            END-IF                                                        
102800            PERFORM IMS-GET-6011-JCL-KVAL-STORRE                          
102900         ELSE                                                             
103000           MOVE MFS-RENSA-FAELT TO MOD-IDJCLRAD (MOD-IX-LINE)             
103100                                   MOD-TEJCL (MOD-IX-LINE)                
103200           MOVE MFS-FORMATETS-ATTR TO                                     
103300                    MOD-TEJCL-ATTR (MOD-IX-LINE)                          
103400         END-IF                                                           
103500         SET MOD-IX-LINE UP BY 1                                          
103600       END-PERFORM                                                        
103700       IF SEGMENT-FINNS                                                   
103800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
103900         CALL WMEDKONV USING MED-WMEDAREA                                 
104000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
104100         MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD-SPAR                      
104200       END-IF                                                             
104300     ELSE                                                                 
104400       MOVE INF-PRESS-PF1-FOR-INFO TO MED-IDMFSINF                        
104500       CALL WMEDKONV USING MED-WMEDAREA                                   
104600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
104700     END-IF.                                                              
104800     EJECT                                                                
104900 H-DELETE-RUTIN SECTION.                                                  
105000                                                                          
105100     MOVE '6001'                 TO W-IDHTYP                              
105200     MOVE LOW-VALUE              TO W-IDJOB W-IDRUTIN                     
105300     MOVE SPAR-IDRUTIN           TO W-IDRUTIN1                            
105400     PERFORM IMS-GHU-6001-RTN                                             
105500     IF SEGMENT-FINNS                                                     
105600        PERFORM IMS-DELETE-RTN                                            
105700                                                                          
105800        MOVE '6011'              TO W-IDHTYP                              
105900        MOVE SPAR-IDRUTIN        TO W-IDRUTIN                             
106000        PERFORM IMS-GHU-6011-RTN                                          
106100        IF SEGMENT-FINNS                                                  
106200           PERFORM IMS-DELETE-RTN                                         
106300        END-IF                                                            
106400                                                                          
106500        MOVE '6021'              TO W-IDHTYP                              
106600        MOVE SPAR-IDRUTIN        TO W-IDRUTIN                             
106700*       PERFORM IMS-GHN-6021-RTN                                          
106800*       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
106900*          PERFORM IMS-DELETE-RTN                                         
107000*          PERFORM IMS-GHN-6021-RTN                                       
107100*       END-PERFORM                                                       
107200        MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                          
107300        CALL WMEDKONV USING MED-WMEDAREA                                  
107400        MOVE MED-MFSINF          TO MOD-TEMFSINF                          
107500        MOVE MFS-RENSA-FAELT TO MOD-BERUTIN                               
107600                                MOD-IDOWNER                               
107700                                MOD-IDJOB                                 
107800                                MOD-IDJCLRAD-SPAR                         
107900                                MOD-KDDEBINFO                             
108000                                MOD-KDJROOM                               
108100                                MOD-BEPGMNAMN                             
108200                                MOD-KDMSGCLASS                            
108300                                MOD-KDMSGLEVEL                            
108400                                MOD-KDJCLASS                              
108500                                MOD-KVJTIME                               
108600                                MOD-KVJLINES                              
108700                                MOD-KDJFORMS                              
108800                                MOD-KDROUTEX                              
108900                                MOD-KDROUTEP                              
109000                                MOD-IDPROCDD                              
109100                                MOD-KDOUTPUT                              
109200     ELSE                                                                 
109300        MOVE ERR-ROUTINE-MISSING TO MED-IDMFSFEL                          
109400        CALL WMEDKONV USING MED-WMEDAREA                                  
109500        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900* IMS SEKTIONER                                                           
110000                                                                          
110100 IMS-GET-MSG SECTION.                                                     
110200                                                                          
110300     MOVE '  QC' TO GODK-STATUSKODER                                      
110400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
110500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110600     PERFORM IMS-STATUSKONTROLL                                           
110700     .                                                                    
110800     SKIP3                                                                
110900 IMS-INSERT-MSG SECTION.                                                  
111000                                                                          
111100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
111200     MOVE SPACE TO GODK-STATUSKODER                                       
111300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
111400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111500     PERFORM IMS-STATUSKONTROLL                                           
111600     .                                                                    
111700     EJECT                                                                
111800 IMS-GET-6001-KNTL SECTION.                                               
111900                                                                          
112000     STRING 'WLJCLB01(WDP101KY =' W-WDP101KY-X ')'                        
112100            DELIMITED BY SIZE INTO SSA1                                   
112200     STRING 'WLJCLB11(IDUSER   =' W-IDUSER ')'                            
112300            DELIMITED BY SIZE INTO SSA2                                   
112400     MOVE '  GE' TO GODK-STATUSKODER                                      
112500     CALL CBLTDLI USING GU JCLB-PCB DLI-IO-AREA SSA1 SSA2                 
112600     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
112700     PERFORM IMS-STATUSKONTROLL                                           
112800     .                                                                    
112900     SKIP3                                                                
113000 IMS-GET-6001-RTN SECTION.                                                
113100                                                                          
113200     STRING 'WLJCLB01(WDP101KY =' W-WDP101KY-X ')'                        
113300            DELIMITED BY SIZE INTO SSA1                                   
113400     STRING 'WLJCLB12(IDRUTIN  =' W-IDRUTIN1-X ')'                        
113500            DELIMITED BY SIZE INTO SSA2                                   
113600     MOVE '  GE' TO GODK-STATUSKODER                                      
113700     CALL CBLTDLI USING GHU JCLB-PCB DLI-IO-AREA SSA1 SSA2                
113800     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
113900     PERFORM IMS-STATUSKONTROLL                                           
114000     .                                                                    
114100     EJECT                                                                
114200 IMS-GET-6011-ROT SECTION.                                                
114300                                                                          
114400     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
114500            DELIMITED BY SIZE INTO SSA1                                   
114600     MOVE '  GE' TO GODK-STATUSKODER                                      
114700     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1                      
114800     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
114900     PERFORM IMS-STATUSKONTROLL                                           
115000     .                                                                    
115100     SKIP3                                                                
115200 IMS-GET-6011-KNTL SECTION.                                               
115300                                                                          
115400     MOVE 'WLJCLD11 ' TO SSA1                                             
115500     MOVE '  GE' TO GODK-STATUSKODER                                      
115600     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
115700     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000     SKIP3                                                                
116100 IMS-GET-6011-JCL-KVAL SECTION.                                           
116200                                                                          
116300     STRING 'WLJCLD12(IDJCLRAD =' W-IDJCLRAD-X ')'                        
116400            DELIMITED BY SIZE INTO SSA1                                   
116500     MOVE '  GE' TO GODK-STATUSKODER                                      
116600     CALL CBLTDLI USING GHNP JCLD-PCB DLI-IO-AREA SSA1                    
116700     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
116800     PERFORM IMS-STATUSKONTROLL                                           
116900     .                                                                    
117000     EJECT                                                                
117100 IMS-GET-6011-JCL-KVAL-STORRE SECTION.                                    
117200                                                                          
117300     STRING 'WLJCLD12(IDJCLRAD>=' W-IDJCLRAD-X ')'                        
117400            DELIMITED BY SIZE INTO SSA1                                   
117500     MOVE '  GE' TO GODK-STATUSKODER                                      
117600     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
117700     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
117800     PERFORM IMS-STATUSKONTROLL                                           
117900     .                                                                    
118000     SKIP3                                                                
118100 IMS-GET-6011-JCL-OKVAL SECTION.                                          
118200                                                                          
118300     MOVE 'WLJCLD12 ' TO SSA1                                             
118400     MOVE '  GE' TO GODK-STATUSKODER                                      
118500     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
118600     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
118700     PERFORM IMS-STATUSKONTROLL                                           
118800     .                                                                    
118900     EJECT                                                                
119000 IMS-INSERT-6001-RTN SECTION.                                             
119100                                                                          
119200     STRING 'WLJCLB01(WDP101KY =' W-WDP101KY-X ')'                        
119300            DELIMITED BY SIZE INTO SSA1                                   
119400     MOVE 'WLJCLB12 ' TO SSA2                                             
119500     MOVE '  ' TO GODK-STATUSKODER                                        
119600     CALL CBLTDLI USING ISRT JCLB-PCB DLI-IO-AREA SSA1 SSA2               
119700     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
119800     PERFORM IMS-STATUSKONTROLL                                           
119900     .                                                                    
120000     EJECT                                                                
120100 IMS-INSERT-6011-ROT SECTION.                                             
120200                                                                          
120300     MOVE 'WLJCLD01 ' TO SSA1                                             
120400     MOVE '  ' TO GODK-STATUSKODER                                        
120500     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1                    
120600     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
120700     PERFORM IMS-STATUSKONTROLL                                           
120800     .                                                                    
120900     SKIP3                                                                
121000 IMS-INSERT-6011-KNTL SECTION.                                            
121100                                                                          
121200     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
121300            DELIMITED BY SIZE INTO SSA1                                   
121400     MOVE 'WLJCLD11 ' TO SSA2                                             
121500     MOVE '  ' TO GODK-STATUSKODER                                        
121600     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1 SSA2               
121700     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
121800     PERFORM IMS-STATUSKONTROLL                                           
121900     .                                                                    
122000     SKIP3                                                                
122100 IMS-INSERT-6011-JCL SECTION.                                             
122200                                                                          
122300     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-X ')'                        
122400            DELIMITED BY SIZE INTO SSA1                                   
122500     MOVE 'WLJCLD12 ' TO SSA2                                             
122600     MOVE '  ' TO GODK-STATUSKODER                                        
122700     CALL CBLTDLI USING ISRT JCLD-PCB DLI-IO-AREA SSA1 SSA2               
122800     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
122900     PERFORM IMS-STATUSKONTROLL                                           
123000     .                                                                    
123100     EJECT                                                                
123200 IMS-REPLACE-6001-RTN SECTION.                                            
123300                                                                          
123400     MOVE '  ' TO GODK-STATUSKODER                                        
123500     CALL CBLTDLI USING REPL JCLB-PCB DLI-IO-AREA                         
123600     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     .                                                                    
123900     SKIP3                                                                
124000 IMS-REPLACE-6011-JCL SECTION.                                            
124100                                                                          
124200     MOVE '  ' TO GODK-STATUSKODER                                        
124300     CALL CBLTDLI USING REPL JCLD-PCB DLI-IO-AREA                         
124400     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
124500     PERFORM IMS-STATUSKONTROLL                                           
124600     .                                                                    
124700     SKIP3                                                                
124800 IMS-DELETE SECTION.                                                      
124900                                                                          
125000     MOVE '  ' TO GODK-STATUSKODER                                        
125100     CALL CBLTDLI USING DLET JCLD-PCB DLI-IO-AREA                         
125200     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
125300     PERFORM IMS-STATUSKONTROLL                                           
125400     .                                                                    
125500     EJECT                                                                
125600 IMS-GHU-6001-RTN SECTION.                                                
125700                                                                          
125800     STRING 'WLJCLB01(WDP101KY =' W-WDP101KY-X ')'                        
125900            DELIMITED BY SIZE INTO SSA1                                   
126000     STRING 'WLJCLB12(IDRUTIN  =' W-IDRUTIN1-X ')'                        
126100            DELIMITED BY SIZE INTO SSA2                                   
126200     MOVE '  GE' TO GODK-STATUSKODER                                      
126300     CALL CBLTDLI USING GHU JCLB-PCB DLI-IO-AREA SSA1 SSA2                
126400     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
126500     PERFORM IMS-STATUSKONTROLL                                           
126600     .                                                                    
126700     SKIP3                                                                
126800 IMS-GHU-6011-RTN SECTION.                                                
126900                                                                          
127000     STRING 'WLJCLB01(WDP101KY =' W-WDP101KY-X ')'                        
127100            DELIMITED BY SIZE INTO SSA1                                   
127200     MOVE '  GE' TO GODK-STATUSKODER                                      
127300     CALL CBLTDLI USING GHU JCLB-PCB DLI-IO-AREA SSA1                     
127400     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
127500     PERFORM IMS-STATUSKONTROLL                                           
127600     .                                                                    
127700     SKIP3                                                                
127800 IMS-GHN-6021-RTN SECTION.                                                
127900                                                                          
128000     STRING 'WLJCLB01(IDHTYP   =' W-IDHTYP                                
128100                    '&IDRUTIN  =' W-IDRUTIN ')'                           
128200            DELIMITED BY SIZE INTO SSA1                                   
128300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
128400     CALL CBLTDLI USING GHN JCLB-PCB DLI-IO-AREA SSA1                     
128500     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
128600     PERFORM IMS-STATUSKONTROLL                                           
128700     .                                                                    
128800     SKIP3                                                                
128900 IMS-DELETE-RTN SECTION.                                                  
129000                                                                          
129100     MOVE '  ' TO GODK-STATUSKODER                                        
129200     CALL CBLTDLI USING DLET JCLB-PCB DLI-IO-AREA                         
129300     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
129400     PERFORM IMS-STATUSKONTROLL                                           
129500     .                                                                    
129600     EJECT                                                                
129700 IMS-STATUSKONTROLL SECTION.                                              
129800     SET STATUS-IX TO 1                                                   
129900     SEARCH GODK-STATUS                                                   
130000       AT END                                                             
130100         CALL FELLOG                                                      
130200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
130300         CONTINUE                                                         
130400     END-SEARCH                                                           
130500     .                                                                    
