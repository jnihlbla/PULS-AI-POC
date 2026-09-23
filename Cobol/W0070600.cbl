000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070600.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   APRIL 1984.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   VISAR, TAR BORT                                          
000800*                INDATA FRÅN DATASEGMENTET SAMT SUBMITTAR                 
000900*                AKTUELLT JOBB.                                           
001000*    SKIP2                                                                
001100*    INDATA.                                                              
001200*        TRANSAKTION: W0T706                                              
001300*        MID:         W0I70601                                            
001400*    UTDATA.                                                              
001500*        MOD:         W0O70601      VISA                                  
001600*                     W0O70901      SUBMITTA                              
001700*    SUBPROGRAM.                                                          
001800*        FELLOG                                                           
001900*        WMEDKONV                                                         
002000*    SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP3                                                                
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  ID-PGM                      PIC X(8)    VALUE 'W0070600'.            
002900 77  JA                          PIC X(1)    VALUE 'J'.                   
003000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003100 77  OK                          PIC X(1)    VALUE 'O'.                   
003200 77  FEL                         PIC X(1)    VALUE 'F'.                   
003300 77  SECURITY-TEST               PIC X(1)    VALUE 'F'.                   
003400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77  MAX-RADER                   PIC S9(3)   VALUE +16  COMP-3.           
003600 77  MIN-MOD-LAENGD              PIC S9(4)   VALUE +49  COMP SYNC.        
003700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1290 COMP SYNC.        
003800 77  SUBMIT                      PIC X(1)    VALUE 'N'.                   
003900 77  ALT-ISRT                    PIC X(1)    VALUE 'N'.                   
004000 77  SPAR-IDJCLRAD               PIC S9(5)   COMP-3.                      
004100                                                                          
004200 01  DYNAMISKA-SUBPROGRAM.                                                
004300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
004600                                                                          
004700 01  W-IDTRANS                   PIC X(4).                                
004800     88  EGEN-BILD                           VALUE '0706'.                
004900     EJECT                                                                
005000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
005100*    -COPY WMEDAREA                                                       
005200     SKIP3                                                                
005300 01  MESSAGES-CODES.                                                      
005400     03  INF-MORE-INFO-EXISTS   PIC X(3)      VALUE '105'.                
005500     03  ERR-RTN-JOB-MISSING    PIC X(3)      VALUE '164'.                
005600     03  ERR-NOT-AUTHORIZED     PIC X(3)      VALUE '405'.                
005700     EJECT                                                                
005800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
005900*                                                                         
006000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
006100     SKIP2                                                                
006200*01  MID -COPY W0I70601                                                   
006300     EJECT                                                                
006400 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
006500     SKIP2                                                                
006600*01  -COPY WMSGAREA                                                       
006700     EJECT                                                                
006800     03  MOD REDEFINES MSG-AREA.                                          
006900*      05  -COPY W0O70601                                                 
007000     EJECT                                                                
007100*01  MOD -COPY W0I70901 -PRE MOD-.                                        
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
007400     SKIP2                                                                
007500*01  -COPY WMFSAREA                                                       
007600     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800     SKIP2                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP2                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008200     03  W-WDP101KY-6001-X.                                               
008300         05  FILLER              PIC X(4)    VALUE '6001'.                
008400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008500     SKIP2                                                                
008600     03  W-WDP101KY-6011-X.                                               
008700         05  FILLER              PIC X(4)    VALUE '6011'.                
008800         05  W-IDRUTIN-6011      PIC X(8).                                
008900         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
009000     SKIP2                                                                
009100     03  W-WDP101KY-6021-X.                                               
009200         05  FILLER              PIC X(4)    VALUE '6021'.                
009300         05  W-IDRUTIN-6021      PIC X(8).                                
009400         05  W-IDJOB             PIC X(8).                                
009500         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
009600     SKIP2                                                                
009700     03  W-IDJCLRAD-X.                                                    
009800         05  W-IDJCLRAD          PIC S9(5)   COMP-3.                      
009900     SKIP2                                                                
010000     03  W-IDUSER-X.                                                      
010100         05  W-IDUSER            PIC X(8).                                
010200     EJECT                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS               PIC X(2).                                    
010500     88  SEGMENT-FINNS                   VALUE '  '.                      
010600     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
010700     SKIP3                                                                
010800 01  GODK-STATUSKODER.                                                    
010900     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300     EJECT                                                                
011400*    --- IMS FUNKTIONSKODER                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    --- DLI INPUT-OUTPUT AREA                                            
011800 01  DLI-IO-AREA.                                                         
011900     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
012000     SKIP3                                                                
012100     03  WLJCLD01  REDEFINES IO-AREA.                                     
012200*      05  -COPY WDP101  -PRE JCLD-                                       
012300     EJECT                                                                
012400     03  WLJCLD11  REDEFINES IO-AREA.                                     
012500*      05  -COPY WDP111  -PRE JCLD-                                       
012600     EJECT                                                                
012700     03  WLJCLC12  REDEFINES IO-AREA.                                     
012800*      05  -COPY WDP113  -PRE JCLC-                                       
012900     EJECT                                                                
013000     03  WLJCLD12  REDEFINES IO-AREA.                                     
013100*      05  -COPY WDP114  -PRE JCLD-                                       
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400     SKIP2                                                                
013500*01  -COPY W0009     -PRE MSG-                                            
013600     EJECT                                                                
013700*01  -COPY W0009     -PRE ALT-                                            
013800     EJECT                                                                
013900*01  -COPY W0008     -PRE JCLA-                                           
014000         05  FILLER              PIC X.                                   
014100     EJECT                                                                
014200*01  -COPY W0008     -PRE JCLC-                                           
014300         05  FILLER              PIC X.                                   
014400     EJECT                                                                
014500*01  -COPY W0008     -PRE JCLD-                                           
014600         05  FILLER              PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION USING MSG-PCB ALT-PCB JCLA-PCB JCLC-PCB               
014900                                          JCLD-PCB.                       
015000 MAIN SECTION.                                                            
015100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB JCLA-PCB JCLC-PCB              
015200                                           JCLD-PCB.                      
015300     PERFORM IMS-GET-MSG                                                  
015400     IF SEGMENT-FINNS                                                     
015500       PERFORM A-INIT-SPARA-INPUT                                         
015600       PERFORM IMS-GET-6021-ROT                                           
015700       IF SEGMENT-FINNS                                                   
015800         PERFORM B-TESTA-SECURITY                                         
015900         IF SECURITY-TEST = OK                                            
016000           IF MFS-UPDATE  AND SUBMIT = NEJ                                
016100             PERFORM C-UPPDATERA-DATA                                     
016200           END-IF                                                         
016300           IF SUBMIT = JA                                                 
016400             PERFORM D-SPARKA-IGANG-W0T709                                
016500             PERFORM IMS-INSERT-ALT-MSG                                   
016600             MOVE JA TO ALT-ISRT                                          
016700           ELSE                                                           
016800             PERFORM E-VISA-DATA                                          
016900           END-IF                                                         
017000         END-IF                                                           
017100       ELSE                                                               
017200         MOVE ERR-RTN-JOB-MISSING TO MED-IDMFSFEL                         
017300         CALL WMEDKONV USING MED-WMEDAREA                                 
017400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
017500       END-IF                                                             
017600       IF ALT-ISRT = NEJ                                                  
017700         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
017800         PERFORM IMS-INSERT-MSG                                           
017900       END-IF                                                             
018000     END-IF                                                               
018100                                                                          
018200     MOVE ZERO TO RETURN-CODE                                             
018300     GOBACK.                                                              
018400     EJECT                                                                
018500 A-INIT-SPARA-INPUT SECTION.                                              
018600     SKIP2                                                                
018700     IF MSG-DUBBLA-TRANSKODER                                             
018800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70601                 
018900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019100     ELSE                                                                 
019200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I70601                  
019300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019500     END-IF                                                               
019600                                                                          
019700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
019800     MOVE MSG-IDPFK            TO MFS-IDPFK                               
019900     MOVE MFS-IDTRANS          TO W-IDTRANS                               
020000                                                                          
020100     MOVE LOW-VALUE  TO MOD-W0O70601                                      
020200     MOVE 'W0O70601' TO MFS-IDMOD                                         
020300     MOVE '0706'     TO MOD-IDTRANS                                       
020400                                                                          
020500     IF MID-IDRUTIN-IN = ALL '+'                                          
020600       MOVE MID-IDRUTIN-UT TO W-IDRUTIN-6011                              
020700                              W-IDRUTIN-6021 MOD-IDRUTIN-UT               
020800     ELSE                                                                 
020900       MOVE MID-IDRUTIN-IN TO W-IDRUTIN-6011                              
021000                              W-IDRUTIN-6021 MOD-IDRUTIN-UT               
021100     END-IF                                                               
021200                                                                          
021300     IF MID-IDJOB-IN = ALL '+'                                            
021400       MOVE MID-IDJOB-UT TO W-IDJOB MOD-IDJOB-UT                          
021500     ELSE                                                                 
021600       MOVE MID-IDJOB-IN TO W-IDJOB MOD-IDJOB-UT                          
021700     END-IF                                                               
021800                                                                          
021900     IF MID-IDJCLRAD-SKIP NUMERIC AND EGEN-BILD AND                       
022000       MFS-IDPFK NOT = '7'                                                
022100       IF MFS-UPDATE                                                      
022200         SET MID-IX-LINE TO +1                                            
022300         MOVE MID-IDJCLRAD (MID-IX-LINE) TO                               
022400                           SPAR-IDJCLRAD W-IDJCLRAD                       
022500       ELSE                                                               
022600         MOVE MID-IDJCLRAD-SKIP TO W-IDJCLRAD SPAR-IDJCLRAD               
022700       END-IF                                                             
022800     ELSE                                                                 
022900       MOVE +1000 TO W-IDJCLRAD SPAR-IDJCLRAD                             
023000     END-IF                                                               
023100                                                                          
023200     MOVE NEJ TO SUBMIT ALT-ISRT                                          
023300     IF MID-FLKLAR = JA AND MFS-UPDATE                                    
023400       MOVE JA TO SUBMIT                                                  
023500     END-IF                                                               
023600                                                                          
023700                                                                          
023800     IF ENGLISH-TEXT                                                      
023900       MOVE +2    TO SPRAK-IX                                             
024000       MOVE 'GB ' TO MED-IDSKYLT                                          
024100       MOVE 'N'   TO MFS-KDHUVOMR                                         
024200     ELSE                                                                 
024300       MOVE +1    TO SPRAK-IX                                             
024400       MOVE 'S  ' TO MED-IDSKYLT                                          
024500     END-IF                                                               
024600                                                                          
024700     MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-IN                               
024800                             MOD-IDJOB-IN                                 
024900                             MOD-FLKLAR                                   
025000                             MOD-TEMFSFEL                                 
025100                             MOD-TEMFSINF                                 
025200     .                                                                    
025300     EJECT                                                                
025400 B-TESTA-SECURITY SECTION.                                                
025500     SKIP2                                                                
025600     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
025700     PERFORM IMS-GET-6021-KNTL                                            
025800     IF SEGMENT-FINNS                                                     
025900       MOVE OK TO SECURITY-TEST                                           
026000     ELSE                                                                 
026100       PERFORM IMS-GET-6011-KNTL                                          
026200       IF SEGMENT-FINNS                                                   
026300         MOVE OK TO SECURITY-TEST                                         
026400       ELSE                                                               
026500         PERFORM IMS-GET-6001-KNTL                                        
026600         IF SEGMENT-FINNS                                                 
026700           MOVE OK TO SECURITY-TEST                                       
026800         ELSE                                                             
026900           MOVE FEL TO SECURITY-TEST                                      
027000           MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                        
027100           CALL WMEDKONV USING MED-WMEDAREA                               
027200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
027300         END-IF                                                           
027400       END-IF                                                             
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 C-UPPDATERA-DATA SECTION.                                                
027900     SKIP2                                                                
028000     SET MID-IX-LINE TO +1                                                
028100     PERFORM IMS-GET-HOLD-6021-DATA                                       
028200     PERFORM UNTIL MID-IX-LINE > MAX-RADER - 1 OR SEGMENT-SAKNAS          
028300       OR MID-IDJCLRAD (MID-IX-LINE) NOT NUMERIC                          
028400       IF MID-KDCMD (MID-IX-LINE) = 'D'                                   
028500         PERFORM IMS-DELETE                                               
028600         MOVE NEJ TO SUBMIT                                               
028700       END-IF                                                             
028800       SET MID-IX-LINE UP BY +1                                           
028900       IF MID-IDJCLRAD (MID-IX-LINE) NUMERIC                              
029000         MOVE MID-IDJCLRAD (MID-IX-LINE) TO W-IDJCLRAD                    
029100         PERFORM IMS-GET-HOLD-6021-DATA                                   
029200       END-IF                                                             
029300     END-PERFORM                                                          
029400                                                                          
029500     PERFORM IMS-GET-6011-JOB                                             
029600     IF JCLC-JOB-KDTRSTAT = 3 OR 4                                        
029700       COMPUTE JCLC-JOB-KDTRSTAT = JCLC-JOB-KDTRSTAT - 2                  
029800       PERFORM IMS-REPLACE-6011                                           
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 D-SPARKA-IGANG-W0T709 SECTION.                                           
030300     SKIP2                                                                
030400     MOVE W-IDRUTIN-6011 TO MOD-MID-IDRUTIN-IN                            
030500     MOVE W-IDJOB        TO MOD-MID-IDJOB-IN                              
030600                                                                          
030700     MOVE MIN-MOD-LAENGD TO MSG-KVLL                                      
030800     MOVE 'W0T709U '     TO MSG-KDTRANS-1                                 
030900     MOVE '0706'         TO MSG-IDTRANS-1                                 
031000     MOVE SPRAK-IX       TO MSG-KDMFSFOR-1                                
031100     MOVE MOD-MID-W0I70901 TO MSG-INDATA-MINUS-1-TRANSKOD                 
031200     .                                                                    
031300     EJECT                                                                
031400 E-VISA-DATA SECTION.                                                     
031500     SKIP2                                                                
031600     MOVE SPAR-IDJCLRAD TO W-IDJCLRAD                                     
031700     PERFORM IMS-GET-6021-ROT                                             
031800     PERFORM IMS-GET-6021-DATA                                            
031900     SET MOD-IX-LINE TO +1                                                
032000     PERFORM UNTIL SEGMENT-SAKNAS OR MOD-IX-LINE > MAX-RADER - 1          
032100       MOVE JCLD-JCL-IDJCLRAD TO MOD-IDJCLRAD (MOD-IX-LINE)               
032200       MOVE SPACE             TO MOD-KDCMD (MOD-IX-LINE)                  
032300       MOVE JCLD-JCL-TEJCL    TO MOD-TEJCL (MOD-IX-LINE)                  
032400       PERFORM IMS-GET-6021-DATA                                          
032500       SET MOD-IX-LINE UP BY +1                                           
032600     END-PERFORM                                                          
032700     IF SEGMENT-FINNS AND MOD-IX-LINE = MAX-RADER                         
032800       MOVE JCLD-JCL-IDJCLRAD    TO MOD-IDJCLRAD-SKIP                     
032900       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
033000       CALL WMEDKONV USING MED-WMEDAREA                                   
033100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
033200     END-IF.                                                              
033300     EJECT                                                                
033400* IMS SEKTIONER                                                           
033500     SKIP3                                                                
033600 IMS-GET-MSG SECTION.                                                     
033700     SKIP2                                                                
033800     MOVE '  QC' TO GODK-STATUSKODER                                      
033900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
034000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     SKIP3                                                                
034300     .                                                                    
034400 IMS-INSERT-MSG SECTION.                                                  
034500     SKIP2                                                                
034600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
034700     MOVE SPACE TO GODK-STATUSKODER                                       
034800     CALL CBLTDLI USING ISRT MSG-PCB                                      
034900                          MSG-IO-AREA MFS-IDMOD                           
035000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035100     PERFORM IMS-STATUSKONTROLL                                           
035200     SKIP3                                                                
035300     .                                                                    
035400 IMS-INSERT-ALT-MSG SECTION.                                              
035500     SKIP2                                                                
035600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
035700     MOVE SPACE TO GODK-STATUSKODER                                       
035800     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
035900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
036000     .                                                                    
036100     EJECT                                                                
036200 IMS-GET-6001-KNTL SECTION.                                               
036300                                                                          
036400     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6001-X ')'                   
036500            DELIMITED BY SIZE INTO SSA1                                   
036600     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
036700            DELIMITED BY SIZE INTO SSA2                                   
036800     MOVE '  GE' TO GODK-STATUSKODER                                      
036900     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
037000     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
037100     PERFORM IMS-STATUSKONTROLL.                                          
037200                                                                          
037300 IMS-GET-6011-KNTL SECTION.                                               
037400                                                                          
037500     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6011-X ')'                   
037600            DELIMITED BY SIZE INTO SSA1                                   
037700     STRING 'WLJCLA11(IDUSER   =' W-IDUSER-X ')'                          
037800            DELIMITED BY SIZE INTO SSA2                                   
037900     MOVE '  GE' TO GODK-STATUSKODER                                      
038000     CALL CBLTDLI USING GU JCLA-PCB DLI-IO-AREA SSA1 SSA2                 
038100     MOVE JCLA-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSKONTROLL.                                          
038300                                                                          
038400 IMS-GET-6011-JOB SECTION.                                                
038500                                                                          
038600     STRING 'WLJCLC01(WDP101KY =' W-WDP101KY-6011-X ')'                   
038700            DELIMITED BY SIZE INTO SSA1                                   
038800     STRING 'WLJCLC12(IDJOB    =' W-IDJOB ')'                             
038900            DELIMITED BY SIZE INTO SSA2                                   
039000     MOVE '  ' TO GODK-STATUSKODER                                        
039100     CALL CBLTDLI USING GHU JCLC-PCB DLI-IO-AREA SSA1 SSA2                
039200     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
039300     PERFORM IMS-STATUSKONTROLL.                                          
039400     EJECT                                                                
039500 IMS-GET-6021-ROT SECTION.                                                
039600     SKIP2                                                                
039700     STRING 'WLJCLD01(WDP101KY =' W-WDP101KY-6021-X ')'                   
039800            DELIMITED BY SIZE INTO SSA1                                   
039900     MOVE '  GE' TO GODK-STATUSKODER                                      
040000     CALL CBLTDLI USING GU JCLD-PCB DLI-IO-AREA SSA1                      
040100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
040200     PERFORM IMS-STATUSKONTROLL                                           
040300     SKIP3                                                                
040400     .                                                                    
040500 IMS-GET-6021-KNTL SECTION.                                               
040600     SKIP2                                                                
040700     STRING 'WLJCLD11(IDUSER   =' W-IDUSER-X ')'                          
040800            DELIMITED BY SIZE INTO SSA1                                   
040900     MOVE '  GE' TO GODK-STATUSKODER                                      
041000     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
041100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
041200     PERFORM IMS-STATUSKONTROLL                                           
041300     SKIP3                                                                
041400     .                                                                    
041500 IMS-GET-HOLD-6021-DATA SECTION.                                          
041600     SKIP2                                                                
041700     STRING 'WLJCLD12(IDJCLRAD>=' W-IDJCLRAD-X ')'                        
041800            DELIMITED BY SIZE INTO SSA1                                   
041900     MOVE '  GE' TO GODK-STATUSKODER                                      
042000     CALL CBLTDLI USING GHNP JCLD-PCB DLI-IO-AREA SSA1                    
042100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     EJECT                                                                
042500 IMS-GET-6021-DATA SECTION.                                               
042600     SKIP2                                                                
042700     STRING 'WLJCLD12(IDJCLRAD>=' W-IDJCLRAD-X ')'                        
042800            DELIMITED BY SIZE INTO SSA1                                   
042900     MOVE '  GE' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GNP JCLD-PCB DLI-IO-AREA SSA1                     
043100     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     SKIP3                                                                
043400     .                                                                    
043500 IMS-DELETE SECTION.                                                      
043600     SKIP2                                                                
043700     MOVE '  ' TO GODK-STATUSKODER                                        
043800     CALL CBLTDLI USING DLET JCLD-PCB DLI-IO-AREA                         
043900     MOVE JCLD-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     SKIP3                                                                
044200     .                                                                    
044300 IMS-REPLACE-6011 SECTION.                                                
044400     SKIP2                                                                
044500     MOVE '  ' TO GODK-STATUSKODER                                        
044600     CALL CBLTDLI USING REPL JCLC-PCB DLI-IO-AREA                         
044700     MOVE JCLC-STATUS-CODE TO STATUS-WS                                   
044800     PERFORM IMS-STATUSKONTROLL                                           
044900     .                                                                    
045000     EJECT                                                                
045100 IMS-STATUSKONTROLL SECTION.                                              
045200     SET STATUS-IX TO 1                                                   
045300     SEARCH GODK-STATUS AT END CALL FELLOG                                
045400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
045500     END-SEARCH.                                                          
