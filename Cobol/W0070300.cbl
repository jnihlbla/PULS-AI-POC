000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0070300.                                                
000300 AUTHOR.         MATS VINNEFORS.                                          
000400 DATE-WRITTEN.   AUGUSTI 1984.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   VISAR ALLA RUTINER MED                                   
000800*                VISS INFO OM RESPEKTIVE.                                 
000900*    SKIP2                                                                
001000*    INDATA.                                                              
001100*        TRANSAKTION: W0T703                                              
001200*        MID:         W0I70301                                            
001300*    UTDATA.                                                              
001400*        MOD:         W0O70301                                            
001500*    SUBPROGRAM.                                                          
001600*        FELLOG                                                           
001700*        WMEDKONV                                                         
001800*    SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP3                                                                
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(8)    VALUE 'W0070300'.            
002700 77  JA                          PIC X(1)    VALUE 'J'.                   
002800 77  NEJ                         PIC X(1)    VALUE 'N'.                   
002900 77  OK                          PIC X(1)    VALUE 'O'.                   
003000 77  FEL                         PIC X(1)    VALUE 'F'.                   
003100 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003200 77  MAX-RADER                   PIC S9(3)   VALUE +14  COMP-3.           
003300 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +722 COMP SYNC.        
003400                                                                          
003500 01  DYNAMISKA-SUBPROGRAM.                                                
003600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
003900                                                                          
004000 01  W-IDTRANS                   PIC X(4).                                
004100     88  EGEN-BILD                           VALUE '0703'.                
004200     SKIP3                                                                
004300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
004400*    -COPY WMEDAREA                                                       
004500     SKIP3                                                                
004600 01  MESSAGES-CODES.                                                      
004700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
004800     EJECT                                                                
004900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
005000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA  '.          
005100     SKIP2                                                                
005200*01  MID -COPY W0I70301                                                   
005300     EJECT                                                                
005400 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
005500*01  -COPY WMSGAREA                                                       
005600     EJECT                                                                
005700     03  MOD REDEFINES MSG-AREA.                                          
005800*      05  -COPY W0O70301                                                 
005900     EJECT                                                                
006000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA  '.          
006100*01  -COPY WMFSAREA.                                                      
006200     EJECT                                                                
006300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006400     SKIP2                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006600     SKIP3                                                                
006700 01  NYCKLAR-TILL-DLI.                                                    
006800     03  W-WDP101KY-X.                                                    
006900         05  IDHTYP              PIC X(4)    VALUE '6001'.                
007000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007100     SKIP2                                                                
007200     03  W-IDRUTIN-X.                                                     
007300         05  W-IDRUTIN           PIC X(8).                                
007400     SKIP2                                                                
007500     03  W-IDUSER-X.                                                      
007600         05  W-IDUSER            PIC X(8).                                
007700     EJECT                                                                
007800*    --- STATUS-KOD FRÅN IMS                                              
007900 01  STATUS-WS                   PIC X(2).                                
008000     88  SEGMENT-FINNS                       VALUE '  '.                  
008100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008200     SKIP3                                                                
008300 01  GODK-STATUSKODER.                                                    
008400     03  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
008500     SKIP3                                                                
008600 01  SSA1                        PIC X(64).                               
008700 01  SSA2                        PIC X(64).                               
008800     EJECT                                                                
008900*    --- IMS FUNKTIONSKODER                                               
009000*01  -COPY W0003                                                          
009100     EJECT                                                                
009200*    --- DLI INPUT-OUTPUT AREA                                            
009300 01  DLI-IO-AREA.                                                         
009400     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
009500     SKIP3                                                                
009600     03  WLJCLB01  REDEFINES IO-AREA.                                     
009700*      05  -COPY WDP101  -PRE JCLB-                                       
009800     EJECT                                                                
009900     03  WLJCLB11  REDEFINES IO-AREA.                                     
010000*      05  -COPY WDP111  -PRE JCLB-                                       
010100     EJECT                                                                
010200     03  WLJCLB12  REDEFINES IO-AREA.                                     
010300*      05  -COPY WDP112  -PRE JCLB-                                       
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600     SKIP2                                                                
010700*01  -COPY W0009     -PRE MSG-                                            
010800     EJECT                                                                
010900*01  -COPY W0008     -PRE JCLB-                                           
011000         05  FILLER              PIC X.                                   
011100     EJECT                                                                
011200 PROCEDURE DIVISION USING MSG-PCB JCLB-PCB.                               
011300 MAIN SECTION.                                                            
011400     ENTRY 'DLITCBL' USING MSG-PCB JCLB-PCB.                              
011500     PERFORM IMS-GET-MSG                                                  
011600     IF SEGMENT-FINNS                                                     
011700       PERFORM A-INIT-SPARA-INPUT                                         
011800       PERFORM IMS-GET-6001-ROT                                           
011900       PERFORM B-VISA-RUTINER                                             
012000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
012100       PERFORM IMS-INSERT-MSG                                             
012200     END-IF                                                               
012300                                                                          
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK.                                                              
012600     EJECT                                                                
012700 A-INIT-SPARA-INPUT SECTION.                                              
012800     SKIP2                                                                
012900     IF MSG-DUBBLA-TRANSKODER                                             
013000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I70301                 
013100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
013200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
013300     ELSE                                                                 
013400       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W0I70301                   
013500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
013600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
013700     END-IF                                                               
013800                                                                          
013900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
014000     MOVE MSG-IDPFK            TO MFS-IDPFK                               
014100     MOVE MFS-IDTRANS          TO W-IDTRANS                               
014200                                                                          
014300     MOVE LOW-VALUE  TO MOD-W0O70301                                      
014400     MOVE 'W0O70301' TO MFS-IDMOD                                         
014500     MOVE '0703'     TO MOD-IDTRANS                                       
014600                                                                          
014700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
014800                             MOD-TEMFSINF                                 
014900                             MOD-IDRUTIN-IN                               
015000                             MOD-IDJOB-IN                                 
015100                                                                          
015200     IF ENGLISH-TEXT                                                      
015300       MOVE +2    TO SPRAK-IX                                             
015400       MOVE 'GB ' TO MED-IDSKYLT                                          
015500     ELSE                                                                 
015600       MOVE +1    TO SPRAK-IX                                             
015700       MOVE 'S  ' TO MED-IDSKYLT                                          
015800     END-IF                                                               
015900                                                                          
016000     IF MID-IDRUTIN-IN = ALL '+'                                          
016100       IF MID-IDRUTIN-UT = ALL '+'                                        
016200         MOVE MID-IDRUTIN-SKIP TO W-IDRUTIN                               
016300         MOVE MFS-RENSA-FAELT TO MOD-IDRUTIN-UT                           
016400       ELSE                                                               
016500         MOVE MID-IDRUTIN-UT TO MOD-IDRUTIN-UT                            
016600         IF MID-IDRUTIN-SKIP = SPACE                                      
016700           MOVE MID-IDRUTIN-UT TO W-IDRUTIN                               
016800         ELSE                                                             
016900           MOVE MID-IDRUTIN-SKIP TO W-IDRUTIN                             
017000         END-IF                                                           
017100       END-IF                                                             
017200     ELSE                                                                 
017300       MOVE MID-IDRUTIN-IN TO W-IDRUTIN MOD-IDRUTIN-UT                    
017400     END-IF                                                               
017500                                                                          
017600     IF MFS-IDPFK = '7'                                                   
017700       MOVE SPACE TO W-IDRUTIN                                            
017800     END-IF                                                               
017900                                                                          
018000     IF MID-IDJOB-IN = ALL '+'                                            
018100       IF MID-IDJOB-UT = ALL '+'                                          
018200         MOVE MFS-RENSA-FAELT TO MOD-IDJOB-UT                             
018300       ELSE                                                               
018400         MOVE MID-IDJOB-UT TO MOD-IDJOB-UT                                
018500       END-IF                                                             
018600     ELSE                                                                 
018700       MOVE MID-IDJOB-IN TO MOD-IDJOB-UT                                  
018800     END-IF                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 B-VISA-RUTINER SECTION.                                                  
019200     SKIP2                                                                
019300     PERFORM IMS-GET-6001-RTN                                             
019400     SET MOD-IX-LINE TO +1                                                
019500     PERFORM UNTIL SEGMENT-SAKNAS OR MOD-IX-LINE = MAX-RADER              
019600       MOVE JCLB-RTN-IDRUTIN  TO MOD-IDRUTIN (MOD-IX-LINE)                
019700       MOVE JCLB-RTN-TIREGDAT TO MOD-TIREGDAT(MOD-IX-LINE)                
019800       MOVE JCLB-RTN-TIUPPDAT TO MOD-TIUPPDAT(MOD-IX-LINE)                
019900       MOVE JCLB-RTN-BERUTIN  TO MOD-BERUTIN (MOD-IX-LINE)                
020000       SET MOD-IX-LINE UP BY +1                                           
020100       PERFORM IMS-GET-6001-RTN                                           
020200     END-PERFORM                                                          
020300                                                                          
020400     IF SEGMENT-FINNS AND MOD-IX-LINE = MAX-RADER                         
020500       MOVE JCLB-RTN-IDRUTIN TO MOD-IDRUTIN-SKIP                          
020600       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
020700       CALL WMEDKONV USING MED-WMEDAREA                                   
020800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
020900     END-IF.                                                              
021000     EJECT                                                                
021100* IMS SEKTIONER                                                           
021200     SKIP3                                                                
021300 IMS-GET-MSG SECTION.                                                     
021400     SKIP2                                                                
021500     MOVE '  QC' TO GODK-STATUSKODER                                      
021600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
021700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021800     PERFORM IMS-STATUSKONTROLL                                           
021900     SKIP3                                                                
022000     .                                                                    
022100 IMS-INSERT-MSG SECTION.                                                  
022200     SKIP2                                                                
022300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
022400     MOVE SPACE TO GODK-STATUSKODER                                       
022500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
022600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     .                                                                    
022900     EJECT                                                                
023000 IMS-GET-6001-ROT SECTION.                                                
023100     SKIP2                                                                
023200     STRING 'WLJCLB01(WDP101KY =' W-WDP101KY-X ')'                        
023300            DELIMITED BY SIZE INTO SSA1                                   
023400     MOVE '  ' TO GODK-STATUSKODER                                        
023500     CALL CBLTDLI USING GU JCLB-PCB DLI-IO-AREA SSA1                      
023600     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
023700     PERFORM IMS-STATUSKONTROLL                                           
023800     SKIP3                                                                
023900     .                                                                    
024000 IMS-GET-6001-RTN SECTION.                                                
024100     SKIP2                                                                
024200     STRING 'WLJCLB12(IDRUTIN >=' W-IDRUTIN-X ')'                         
024300            DELIMITED BY SIZE INTO SSA1                                   
024400     MOVE '  GE' TO GODK-STATUSKODER                                      
024500     CALL CBLTDLI USING GNP JCLB-PCB DLI-IO-AREA SSA1                     
024600     MOVE JCLB-STATUS-CODE TO STATUS-WS                                   
024700     PERFORM IMS-STATUSKONTROLL                                           
024800     .                                                                    
024900     EJECT                                                                
025000 IMS-STATUSKONTROLL SECTION.                                              
025100     SET STATUS-IX TO 1                                                   
025200     SEARCH GODK-STATUS AT END CALL FELLOG                                
025300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
025400     END-SEARCH.                                                          
