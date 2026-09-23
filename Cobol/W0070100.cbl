000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070100.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   FEBRUARI 1984.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   HANTERAR RS IMS-SUBMIT-PAKETETS SECURITY.                
000800*                                                                         
000900*    INDATA.                                                              
001000*        TRANSAKTION: W0T701                                              
001100*        MID:         W0I70101                                            
001200*    UTDATA.                                                              
001300*        MOD:         W0O70101                                            
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
002500 77  IDPGM                       PIC X(8)    VALUE 'W0070100'.            
002600 77  JA                          PIC X(1)    VALUE 'J'.                   
002700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
002800 77  BACKA                       PIC X(1)    VALUE 'N'.                   
002900 77  OK                          PIC X(1)    VALUE 'O'.                   
003000 77  FEL                         PIC X(1)    VALUE 'F'.                   
003100 77  SECURITY-TEST               PIC X(1)    VALUE 'F'.                   
003200 77  MAX-RADER                   PIC S9(3)   VALUE +25  COMP-3.           
003300 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +681 COMP SYNC.        
003400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003500                                                                          
003600 01  DYNAMISKA-SUBPROGRAM.                                                
003700     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
003800     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
003900     03 WMEDKONV                 PIC X(8)    VALUE 'WMEDKONV'.            
004000                                                                          
004100 01  W-IDTRANS                   PIC X(4).                                
004200     88  EGEN-BILD                           VALUE '0701'.                
004300     88  GODK-BILD                           VALUE '0702' '0703'          
004400                                                   '0704' '0705'          
004500                                                   '0706' '0707'          
004600                                                   '0708' '0709'.         
004700     EJECT                                                                
004800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
004900*    -COPY WMEDAREA                                                       
005000     EJECT                                                                
005100 01  MESSAGES-CODES.                                                      
005200     03  ERR-CORR-HILITE-FLDS    PIC X(3)      VALUE '001'.               
005300     03  INF-PRESS-PF11          PIC X(3)      VALUE '003'.               
005400     03  INF-UPDATE-DONE         PIC X(3)      VALUE '101'.               
005500     03  INF-MORE-INFO-EXISTS    PIC X(3)      VALUE '105'.               
005600     03  ERR-RTN-JOB-MISSING     PIC X(3)      VALUE '164'.               
005700     03  ERR-NOT-AUTHORIZED      PIC X(3)      VALUE '405'.               
005800     EJECT                                                                
005900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
006000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
006100     SKIP2                                                                
006200*01  MID -COPY W0I70101                                                   
006300     EJECT                                                                
006400 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
006500     SKIP2                                                                
006600*01  -COPY WMSGAREA                                                       
006700     EJECT                                                                
006800     03  MOD REDEFINES MSG-AREA.                                          
006900*      05  -COPY W0O70101                                                 
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
007200     SKIP2                                                                
007300*01  -COPY WMFSAREA                                                       
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600     SKIP2                                                                
007700 01  FILLER                    PIC X(16)   VALUE ' IMS-WS '.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008000     03  W-WDP101KY-X.                                                    
008100         05  IDHTYP              PIC X(4).                                
008200         05  W-IDRUTIN           PIC X(8).                                
008300         05  W-IDJOB             PIC X(8).                                
008400         05  LOWVALUE            PIC X(10)   VALUE LOW-VALUE.             
008500                                                                          
008600     03  W-WDP101KY-6001-X.                                               
008700         05  FILLER              PIC X(4)    VALUE '6001'.                
008800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008900                                                                          
009000     03  W-WDP101KY-6011-X.                                               
009100         05  FILLER              PIC X(4)    VALUE '6011'.                
009200         05  W-IDRUTIN-6011      PIC X(8).                                
009300         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
009400                                                                          
009500     03  W-WDP111KY-X.                                                    
009600         05  W-IDOWNER           PIC X(8).                                
009700         05  W-IDUSER            PIC X(8).                                
009800                                                                          
009900     03  W-IDOWNER1              PIC X(8).                                
010000     03  W-IDUSER1               PIC X(8).                                
010100     EJECT                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS               PIC X(2).                                    
010400     88  SEGMENT-FINNS                   VALUE '  '.                      
010500     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
010600     88  INSERT-OK                       VALUE '  '.                      
010700     SKIP3                                                                
010800 01  GODK-STATUSKODER.                                                    
010900     03  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
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
012100     03  WLJCLA01  REDEFINES IO-AREA.                                     
012200*      05  -COPY WDP101  -PRE JCLA-                                       
012300     EJECT                                                                
012400     03  WLJCLA11  REDEFINES IO-AREA.                                     
012500*      05  -COPY WDP111  -PRE JCLA-                                       
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900*01  -COPY W0009     -PRE MSG-                                            
013000     EJECT                                                                
013100*01  -COPY W0008     -PRE JCLA1-                                          
013200         07  FILLER              PIC X.                                   
013300                                                                          
013400*01  -COPY W0008     -PRE JCLA2-                                          
013500         07  FILLER              PIC X.                                   
013600     EJECT                                                                
013700 PROCEDURE DIVISION USING MSG-PCB JCLA1-PCB JCLA2-PCB.                    
013800 MAIN SECTION.                                                            
013900     ENTRY 'DLITCBL' USING MSG-PCB JCLA1-PCB JCLA2-PCB.                   
014000     PERFORM IMS-GET-MSG                                                  
014100     IF SEGMENT-FINNS                                                     
014200                                                                          
014300       PERFORM A-INIT-SPARA-INPUT                                         
014400       PERFORM IMS-GET-ROT                                                
014500       IF SEGMENT-FINNS                                                   
014600         IF MFS-UPDATE                                                    
014700           PERFORM B-TESTA-SECURITY                                       
014800           IF SECURITY-TEST = OK                                          
014900             PERFORM C-UPPDATERA-USER                                     
015000           END-IF                                                         
015100         END-IF                                                           
015200         PERFORM D-VISA-USER                                              
015300       ELSE                                                               
015400         MOVE ERR-RTN-JOB-MISSING TO MED-IDMFSFEL                         
015500         CALL WMEDKONV USING MED-WMEDAREA                                 
015600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
015700       END-IF                                                             
015800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
015900       PERFORM IMS-INSERT-MSG                                             
016000     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK.                                                              
016400     EJECT                                                                
016500 A-INIT-SPARA-INPUT SECTION.                                              
016600     SKIP2                                                                
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70101                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I70101                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
017800     MOVE MSG-IDPFK            TO MFS-IDPFK                               
017900     MOVE MFS-IDTRANS          TO W-IDTRANS                               
018000                                                                          
018100     MOVE LOW-VALUE  TO MOD-W0O70101                                      
018200     MOVE 'W0O70101' TO MFS-IDMOD                                         
018300     MOVE '0701'     TO MOD-IDTRANS                                       
018400                                                                          
018500     SET MOD-IX-LINE TO +1                                                
018600                                                                          
018700     IF MID-IDRUTIN-IN = ALL '+'                                          
018800       MOVE MID-IDRUTIN-UT TO W-IDRUTIN MOD-IDRUTIN-UT                    
018900                              W-IDRUTIN-6011                              
019000     ELSE                                                                 
019100       MOVE MID-IDRUTIN-IN TO W-IDRUTIN MOD-IDRUTIN-UT                    
019200                              W-IDRUTIN-6011                              
019300     END-IF                                                               
019400                                                                          
019500     IF MID-IDJOB-IN = ALL '+'                                            
019600       MOVE MID-IDJOB-UT TO W-IDJOB MOD-IDJOB-UT                          
019700     ELSE                                                                 
019800       MOVE MID-IDJOB-IN TO W-IDJOB MOD-IDJOB-UT                          
019900     END-IF                                                               
020000                                                                          
020100     IF EGEN-BILD AND MFS-IDPFK NOT = '7'                                 
020200       MOVE MID-IDOWNER-SKIP TO W-IDOWNER                                 
020300       MOVE MID-IDUSER-SKIP TO W-IDUSER                                   
020400     ELSE                                                                 
020500       MOVE SPACE TO W-IDOWNER W-IDUSER                                   
020600     END-IF                                                               
020700                                                                          
020800     IF W-IDRUTIN = SPACE                                                 
020900       MOVE '6001' TO IDHTYP                                              
021000       MOVE LOW-VALUE TO W-IDRUTIN W-IDJOB                                
021100     ELSE                                                                 
021200       IF W-IDJOB = SPACE                                                 
021300         MOVE '6011' TO IDHTYP                                            
021400         MOVE LOW-VALUE TO W-IDJOB                                        
021500       ELSE                                                               
021600         MOVE '6021' TO IDHTYP                                            
021700       END-IF                                                             
021800     END-IF                                                               
021900                                                                          
022000     MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-IN                               
022100                             MOD-IDJOB-IN                                 
022200                             MOD-TEMFSFEL                                 
022300                             MOD-TEMFSINF                                 
022400                             MOD-IDOWNER                                  
022500                             MOD-IDUSER                                   
022600                                                                          
022700                                                                          
022800     IF ENGLISH-TEXT                                                      
022900       MOVE +2    TO SPRAK-IX                                             
023000       MOVE 'GB ' TO MED-IDSKYLT                                          
023100     ELSE                                                                 
023200       MOVE +1    TO SPRAK-IX                                             
023300       MOVE 'S  ' TO MED-IDSKYLT                                          
023400     END-IF.                                                              
023500     EJECT                                                                
023600 B-TESTA-SECURITY SECTION.                                                
023700     SKIP2                                                                
023800     MOVE FEL TO SECURITY-TEST                                            
023900                                                                          
024000     MOVE MSG-SIGNON-USERID TO W-IDOWNER1 W-IDUSER1                       
024100     PERFORM IMS-GET-KNTL-IDOWNER                                         
024200                                                                          
024300     IF SEGMENT-FINNS                                                     
024400       MOVE OK TO SECURITY-TEST                                           
024500     ELSE                                                                 
024600       IF IDHTYP = '6021'                                                 
024700         PERFORM IMS-GET-6011-KNTL                                        
024800         IF SEGMENT-FINNS                                                 
024900           MOVE OK TO SECURITY-TEST                                       
025000         ELSE                                                             
025100           PERFORM IMS-GET-6001-KNTL                                      
025200           IF SEGMENT-FINNS                                               
025300             MOVE OK TO SECURITY-TEST                                     
025400           ELSE                                                           
025500             MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                      
025600             CALL WMEDKONV USING MED-WMEDAREA                             
025700             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
025800           END-IF                                                         
025900         END-IF                                                           
026000       ELSE                                                               
026100         IF IDHTYP = '6011'                                               
026200           PERFORM IMS-GET-6001-KNTL                                      
026300           IF SEGMENT-FINNS                                               
026400             MOVE OK TO SECURITY-TEST                                     
026500           ELSE                                                           
026600             MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                      
026700             CALL WMEDKONV USING MED-WMEDAREA                             
026800             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
026900           END-IF                                                         
027000         ELSE                                                             
027100           IF IDHTYP = '6001'                                             
027200             MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                      
027300             CALL WMEDKONV USING MED-WMEDAREA                             
027400             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
027500           END-IF                                                         
027600         END-IF                                                           
027700       END-IF                                                             
027800     END-IF.                                                              
027900     EJECT                                                                
028000 C-UPPDATERA-USER SECTION.                                                
028100     SKIP2                                                                
028200     IF MID-IDUSER = ALL '+' OR SPACE                                     
028300       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDUSER-ATTR                       
028400       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
028500       CALL WMEDKONV USING MED-WMEDAREA                                   
028600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
028700       IF MID-IDOWNER NOT = ALL '+'                                       
028800         MOVE MFS-ROER-EJ-FAELT    TO MOD-IDOWNER                         
028900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDOWNER-ATTR                    
029000       END-IF                                                             
029100     ELSE                                                                 
029200       IF MID-IDOWNER = SPACE                                             
029300         MOVE MFS-ROER-EJ-FAELT    TO MOD-IDUSER                          
029400         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDOWNER-ATTR                    
029500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
029600         CALL WMEDKONV USING MED-WMEDAREA                                 
029700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
029800       ELSE                                                               
029900         IF MID-IDOWNER  = ALL '+'                                        
030000           MOVE MSG-SIGNON-USERID TO JCLA-KNTL-IDOWNER W-IDOWNER          
030100         ELSE                                                             
030200           MOVE MID-IDOWNER TO JCLA-KNTL-IDOWNER W-IDOWNER                
030300         END-IF                                                           
030400         ACCEPT JCLA-KNTL-TIREGDAT FROM DATE                              
030500         MOVE MID-IDUSER TO JCLA-KNTL-IDUSER W-IDUSER                     
030600         PERFORM IMS-INSERT-KNTL                                          
030700         IF INSERT-OK                                                     
030800           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
030900           CALL WMEDKONV USING MED-WMEDAREA                               
031000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
031100           MOVE JCLA-KNTL-IDOWNER  TO MOD-IDOWNER-UT(MOD-IX-LINE)         
031200           MOVE JCLA-KNTL-IDUSER   TO MOD-IDUSER-UT(MOD-IX-LINE)          
031300           MOVE JCLA-KNTL-TIREGDAT TO MOD-TIREGDAT (MOD-IX-LINE)          
031400           SET MOD-IX-LINE TO +2                                          
031500         ELSE                                                             
031600           PERFORM IMS-GHU-KNTL                                           
031700           PERFORM IMS-DELETE                                             
031800           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
031900           CALL WMEDKONV USING MED-WMEDAREA                               
032000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
032100         END-IF                                                           
032200       END-IF                                                             
032300     END-IF.                                                              
032400     EJECT                                                                
032500 D-VISA-USER SECTION.                                                     
032600     SKIP2                                                                
032700     IF NOT MFS-UPDATE  AND EGEN-BILD                                     
032800       IF MID-IDOWNER NOT = ALL '+'                                       
032900         MOVE MFS-ROER-EJ-FAELT    TO MOD-IDOWNER                         
033000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDOWNER-ATTR                    
033100         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
033200         CALL WMEDKONV USING MED-WMEDAREA                                 
033300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
033400       END-IF                                                             
033500       IF MID-IDUSER NOT = ALL '+'                                        
033600         MOVE MFS-ROER-EJ-FAELT    TO MOD-IDUSER                          
033700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-ATTR                     
033800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
033900         CALL WMEDKONV USING MED-WMEDAREA                                 
034000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
034100       END-IF                                                             
034200       PERFORM IMS-GET-ROT                                                
034300     END-IF                                                               
034400                                                                          
034500     IF  GODK-BILD                                                        
034600       PERFORM IMS-GET-ROT                                                
034700     END-IF                                                               
034800                                                                          
034900     PERFORM IMS-GNP-KNTL                                                 
035000     PERFORM UNTIL MOD-IX-LINE = MAX-RADER OR SEGMENT-SAKNAS              
035100       MOVE JCLA-KNTL-IDOWNER  TO MOD-IDOWNER-UT (MOD-IX-LINE)            
035200       MOVE JCLA-KNTL-IDUSER   TO MOD-IDUSER-UT (MOD-IX-LINE)             
035300       MOVE JCLA-KNTL-TIREGDAT TO MOD-TIREGDAT (MOD-IX-LINE)              
035400       SET MOD-IX-LINE UP BY +1                                           
035500       PERFORM IMS-GNP-KNTL                                               
035600     END-PERFORM                                                          
035700     IF MOD-IX-LINE = MAX-RADER AND SEGMENT-FINNS                         
035800       MOVE JCLA-KNTL-IDOWNER TO MOD-IDOWNER-SKIP                         
035900       MOVE JCLA-KNTL-IDUSER  TO MOD-IDUSER-SKIP                          
036000       IF NOT MFS-UPDATE                                                  
036100         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                        
036200         CALL WMEDKONV USING MED-WMEDAREA                                 
036300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
036400       END-IF                                                             
036500     END-IF.                                                              
036600     EJECT                                                                
036700* IMS SEKTIONER                                                           
036800     SKIP3                                                                
036900 IMS-GET-MSG SECTION.                                                     
037000     SKIP2                                                                
037100     MOVE '  QC' TO GODK-STATUSKODER                                      
037200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
037300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037400     PERFORM IMS-STATUSKONTROLL                                           
037500     SKIP3                                                                
037600     .                                                                    
037700 IMS-INSERT-MSG SECTION.                                                  
037800     SKIP2                                                                
037900     IF ENGLISH-TEXT                                                      
038000       MOVE 'N' TO MFS-KDHUVOMR                                           
038100     END-IF                                                               
038200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
038300     MOVE SPACE TO GODK-STATUSKODER                                       
038400     CALL CBLTDLI USING ISRT MSG-PCB                                      
038500                        MSG-IO-AREA MFS-IDMOD                             
038600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038700     PERFORM IMS-STATUSKONTROLL                                           
038800     .                                                                    
038900     EJECT                                                                
039000 IMS-GET-6001-KNTL SECTION.                                               
039100     SKIP2                                                                
039200     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6001-X ')'                   
039300            DELIMITED BY SIZE INTO SSA1                                   
039400     STRING 'WLJCLA11(IDUSER   =' W-IDUSER1 ')'                           
039500            DELIMITED BY SIZE INTO SSA2                                   
039600     MOVE '  GE' TO GODK-STATUSKODER                                      
039700     CALL CBLTDLI USING GU JCLA2-PCB DLI-IO-AREA SSA1 SSA2                
039800     MOVE JCLA2-STATUS-CODE TO STATUS-WS                                  
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100 IMS-GET-6011-KNTL SECTION.                                               
040200     SKIP2                                                                
040300     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-6011-X ')'                   
040400            DELIMITED BY SIZE INTO SSA1                                   
040500     STRING 'WLJCLA11(IDOWNER  =' W-IDOWNER1 ')'                          
040600            DELIMITED BY SIZE INTO SSA2                                   
040700     MOVE '  GE' TO GODK-STATUSKODER                                      
040800     CALL CBLTDLI USING GU JCLA2-PCB DLI-IO-AREA SSA1 SSA2                
040900     MOVE JCLA2-STATUS-CODE TO STATUS-WS                                  
041000     PERFORM IMS-STATUSKONTROLL                                           
041100     .                                                                    
041200     EJECT                                                                
041300 IMS-GET-ROT SECTION.                                                     
041400     SKIP2                                                                
041500     STRING 'WLJCLA01(WDP101KY =' W-WDP101KY-X ')'                        
041600            DELIMITED BY SIZE INTO SSA1                                   
041700     MOVE '  GE' TO GODK-STATUSKODER                                      
041800     CALL CBLTDLI USING GU JCLA1-PCB DLI-IO-AREA SSA1                     
041900     MOVE JCLA1-STATUS-CODE TO STATUS-WS                                  
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     EJECT                                                                
042300 IMS-GET-KNTL-IDOWNER SECTION.                                            
042400     SKIP2                                                                
042500     STRING 'WLJCLA11(IDOWNER  =' W-IDOWNER1 ')'                          
042600            DELIMITED BY SIZE INTO SSA1                                   
042700     MOVE '  GE' TO GODK-STATUSKODER                                      
042800     CALL CBLTDLI USING GNP JCLA1-PCB DLI-IO-AREA SSA1                    
042900     MOVE JCLA1-STATUS-CODE TO STATUS-WS                                  
043000     PERFORM IMS-STATUSKONTROLL                                           
043100     SKIP3                                                                
043200     .                                                                    
043300 IMS-GNP-KNTL SECTION.                                                    
043400     SKIP2                                                                
043500     STRING 'WLJCLA11(WDP111KY>=' W-WDP111KY-X ')'                        
043600            DELIMITED BY SIZE INTO SSA1                                   
043700     MOVE '  GE' TO GODK-STATUSKODER                                      
043800     CALL CBLTDLI USING GNP JCLA1-PCB DLI-IO-AREA SSA1                    
043900     MOVE JCLA1-STATUS-CODE TO STATUS-WS                                  
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200     EJECT                                                                
044300 IMS-GHU-KNTL SECTION.                                                    
044400     SKIP2                                                                
044500     STRING 'WLJCLA01*P(WDP101KY =' W-WDP101KY-X ')'                      
044600            DELIMITED BY SIZE INTO SSA1                                   
044700     STRING 'WLJCLA11(WDP111KY =' W-WDP111KY-X ')'                        
044800            DELIMITED BY SIZE INTO SSA2                                   
044900     MOVE '  ' TO GODK-STATUSKODER                                        
045000     CALL CBLTDLI USING GHU JCLA1-PCB DLI-IO-AREA SSA1 SSA2               
045100     MOVE JCLA1-STATUS-CODE TO STATUS-WS                                  
045200     PERFORM IMS-STATUSKONTROLL                                           
045300     SKIP3                                                                
045400     .                                                                    
045500 IMS-INSERT-KNTL SECTION.                                                 
045600     SKIP2                                                                
045700     STRING 'WLJCLA01*P(WDP101KY =' W-WDP101KY-X ')'                      
045800            DELIMITED BY SIZE INTO SSA1                                   
045900     MOVE 'WLJCLA11 ' TO SSA2                                             
046000     MOVE '  II' TO GODK-STATUSKODER                                      
046100     CALL CBLTDLI USING ISRT JCLA1-PCB DLI-IO-AREA SSA1 SSA2              
046200     MOVE JCLA1-STATUS-CODE TO STATUS-WS                                  
046300     PERFORM IMS-STATUSKONTROLL                                           
046400     SKIP3                                                                
046500     .                                                                    
046600 IMS-DELETE SECTION.                                                      
046700     SKIP2                                                                
046800     MOVE '  ' TO GODK-STATUSKODER                                        
046900     CALL CBLTDLI USING DLET JCLA1-PCB DLI-IO-AREA                        
047000     MOVE JCLA1-STATUS-CODE TO STATUS-WS                                  
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300     EJECT                                                                
047400 IMS-STATUSKONTROLL SECTION.                                              
047500     SET STATUS-IX TO 1                                                   
047600     SEARCH GODK-STATUS AT END CALL FELLOG                                
047700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
047800     END-SEARCH.                                                          
