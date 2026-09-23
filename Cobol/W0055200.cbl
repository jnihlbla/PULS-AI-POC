000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0055200.                                                
000300 AUTHOR.         THOMAS NILSSON.                                          
000400 DATE-WRITTEN.   JULI  89.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        HELP FÖR IMS PROGRAM. VISAR DOKUMENTATION                        
001000*        FÖR IMS-BILDER, ALLMÄN SYSTEM INFO. LÄSER SEK-INDEX              
001100*    INDATA.                                                              
001200*        TRANSAKTION: W0T552                                              
001300*        MID:         W0I55201                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W0O55201                                            
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77    IDPGM                     PIC X(8)    VALUE 'W0055200'.            
002700 77    JA                        PIC X       VALUE 'J'.                   
002800 77    NEJ                       PIC X       VALUE 'N'.                   
002900 77    IDDOKTYP-W                PIC X(8)    VALUE SPACE.                 
003000 77    IDDOK-W                   PIC X(8)    VALUE SPACE.                 
003100 77    INDX                      PIC S9(4)   VALUE ZERO COMP SYNC.        
003200 77    RADER                     PIC S9(4)   VALUE ZERO COMP SYNC.        
003300 77    MAX-RADER                 PIC S9(4)   VALUE +14  COMP SYNC.        
003400                                                                          
003500 01    IDTRANS                   PIC X(4).                                
003600     88  EGEN-TRANS                          VALUE '0551'.                
003700     88  GODK-TRANS                          VALUE '0551' '0552'          
003800                                                   '0553' '0555'.         
003900                                                                          
004000 01  DYNAMISKA-SUBPROGRAM.                                                
004100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004300                                                                          
004400     EJECT                                                                
004500 01    MEDDELANDE.                                                        
004600     03  MED-1.                                                           
004700         05 FILLER               PIC X(32)   VALUE                        
004800             'TRYCK PF8 FÖR FLER SIDOR        '.                          
004900         05 FILLER               PIC X(32)   VALUE                        
005000             'PRESS PF8 FOR MORE PAGES        '.                          
005100     03  FILLER REDEFINES MED-1.                                          
005200         05 MED1                 PIC X(32)   OCCURS 2.                    
005300                                                                          
005400     03  MED-2.                                                           
005500         05 FILLER               PIC X(32)   VALUE                        
005600             'DOKUMENTATIONEN SLUT            '.                          
005700         05 FILLER               PIC X(32)   VALUE                        
005800             'END OF DOKUMENT                 '.                          
005900     03  FILLER REDEFINES MED-2.                                          
006000         05 MED2                 PIC X(32)   OCCURS 2.                    
006100                                                                          
006200     03  FEL-2.                                                           
006300         05 FILLER               PIC X(32)   VALUE                        
006400             'INFORMATION SAKNAS              '.                          
006500         05 FILLER               PIC X(32)   VALUE                        
006600             'INFORMATION MISSING             '.                          
006700     03  FILLER REDEFINES FEL-2.                                          
006800         05 FEL2                 PIC X(32)   OCCURS 2.                    
006900                                                                          
007000     EJECT                                                                
007100******************************************************************        
007200*                                                                         
007300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
007400*                                                                         
007500 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
007600 01    FILLER                    PIC X(16)   VALUE 'MID   '.              
007700                                                                          
007800*01    MID -COPY W0I55201                                                 
007900                                                                          
008000     EJECT                                                                
008100 01    FILLER                    PIC X(16)   VALUE 'WMSGAREA'.            
008200*01    -COPY WMSGAREA                                                     
008300                                                                          
008400     EJECT                                                                
008500*  03    MOD -COPY W0O55201           -RED MSG-AREA.                      
008600                                                                          
008700     EJECT                                                                
008800 01    FILLER                    PIC X(16)   VALUE 'WMFSAREA'.            
008900*01    -COPY WMFSAREA                                                     
009000                                                                          
009100     EJECT                                                                
009200******************************************************************        
009300*                                                                         
009400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*                                                                         
009600 01    IMS-WS.                                                            
009700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
009800                                                                          
009900 01    NYCKLAR-TILL-DLI.                                                  
010000   03  W-WDP5A1KY-MIN.                                                    
010100     05    W-IDSKYLT-MIN         PIC X(3)    VALUE LOW-VALUE.             
010200     05    W-IDDOKTYP-MIN        PIC X(8)    VALUE LOW-VALUE.             
010300     05    W-IDDOK-MIN           PIC X(8)    VALUE LOW-VALUE.             
010400                                                                          
010500   03  W-WDP5A1KY-MAX.                                                    
010600     05    W-IDSKYLT-MAX         PIC X(3)    VALUE HIGH-VALUE.            
010700     05    W-IDDOKTYP-MAX        PIC X(8)    VALUE HIGH-VALUE.            
010800     05    W-IDDOK-MAX           PIC X(8)    VALUE HIGH-VALUE.            
010900                                                                          
011000*                        **** STATUS-KOD FRÅN IMS                         
011100   03    STATUS-WS               PIC XX.                                  
011200     88    SEGMENT-FINNS                     VALUE '  '.                  
011300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
011400                                                                          
011500   03    GODK-STATUSKODER.                                                
011600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
011700                                                                          
011800 01    SSA1                      PIC X(96).                               
011900                                                                          
012000     EJECT                                                                
012100*                            IMS FUNKTIONSKODER                           
012200*01    -COPY W0003                                                        
012300                                                                          
012400     EJECT                                                                
012500*                            DLI INPUT-OUTPUT AREA                        
012600 03    FILLER                PIC X(16)   VALUE 'DLI-IO-P5A1'.             
012700 01    DLI-IO-P5A1.                                                       
012800*  03  -COPY WDP5A1                                                       
012900                                                                          
013000     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013200*01    -COPY W0009     -PRE MSG-                                          
013300*01    -COPY W0008     -PRE WDP5A-                                        
013400     05  FILLER                  PIC X.                                   
013500                                                                          
013600     EJECT                                                                
013700 PROCEDURE DIVISION USING MSG-PCB WDP5A-PCB.                              
013800 MAIN SECTION.                                                            
013900     ENTRY 'DLITCBL' USING MSG-PCB WDP5A-PCB.                             
014000                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014300       PERFORM A-INIT-SPARA-INPUT                                         
014400                                                                          
014500       IF MFS-IDPFK = '7'                                                 
014600         MOVE IDDOKTYP-W      TO W-IDDOKTYP-MIN                           
014700         MOVE IDDOK-W         TO W-IDDOK-MIN                              
014800       ELSE                                                               
014900         IF MFS-IDPFK = '8'                                               
015000           MOVE MID-IDDOKTYP-SPAR TO W-IDDOKTYP-MIN                       
015100           MOVE MID-IDDOK-SPAR    TO W-IDDOK-MIN                          
015200         ELSE                                                             
015300           MOVE MID-IDDOKTYP-ENTER TO W-IDDOKTYP-MIN                      
015400           MOVE MID-IDDOK-ENTER TO W-IDDOK-MIN                            
015500         END-IF                                                           
015600       END-IF                                                             
015700       PERFORM B-LAS-SIDOR                                                
015800       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55201 + 4                      
015900       PERFORM IMS-INSERT-MSG                                             
016000     END-IF                                                               
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400                                                                          
016500     EJECT                                                                
016600 A-INIT-SPARA-INPUT SECTION.                                              
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I55201                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I55201                 
017300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
017400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
017500     END-IF                                                               
017600     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
017700     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
017800     MOVE MFS-IDTRANS                     TO IDTRANS                      
017900                                                                          
018000     MOVE LOW-VALUE                       TO MSG-AREA                     
018100     MOVE 'W0O55201'                      TO MFS-IDMOD                    
018200     MOVE '0552'                          TO MOD-IDTRANS                  
018300                                                                          
018400     IF SWEDISH-TEXT                                                      
018500       MOVE +1                            TO INDX                         
018600     ELSE                                                                 
018700       MOVE +2                            TO INDX                         
018800     END-IF                                                               
018900     MOVE 'S  '                           TO W-IDSKYLT-MIN                
019000                                             W-IDSKYLT-MAX                
019100                                                                          
019200     MOVE MFS-RENSA-FAELT                 TO MOD-IDDOKTYP-IN              
019300                                             MOD-IDDOK-IN                 
019400                                             MOD-TEMFSFEL                 
019500                                             MOD-TEMFSINF                 
019600                                                                          
019700     EJECT                                                                
019800                                                                          
019900     IF MID-IDDOKTYP-IN =  ALL '+'                                        
020000       MOVE MID-IDDOKTYP-UT               TO IDDOKTYP-W                   
020100     ELSE                                                                 
020200       MOVE MID-IDDOKTYP-IN               TO IDDOKTYP-W                   
020300       MOVE '7'                           TO MFS-IDPFK                    
020400     END-IF                                                               
020500     IF MID-IDDOK-IN =  ALL '+'                                           
020600       MOVE MID-IDDOK-UT                  TO IDDOK-W                      
020700     ELSE                                                                 
020800       MOVE MID-IDDOK-IN                  TO IDDOK-W                      
020900       MOVE '7'                           TO MFS-IDPFK                    
021000     END-IF                                                               
021100                                                                          
021200     IF GODK-TRANS                                                        
021300       MOVE IDDOKTYP-W                    TO MOD-IDDOKTYP-UT              
021400       MOVE IDDOK-W                       TO MOD-IDDOK-UT                 
021500     ELSE                                                                 
021600       MOVE MFS-RENSA-FAELT               TO MOD-IDDOKTYP-UT              
021700                                             MOD-IDDOK-UT                 
021800     END-IF                                                               
021900     .                                                                    
022000                                                                          
022100     EJECT                                                                
022200 B-LAS-SIDOR SECTION.                                                     
022300                                                                          
022400     MOVE +1                    TO RADER                                  
022500     PERFORM IMS-GN-INFO                                                  
022600     IF SEGMENT-FINNS                                                     
022700       MOVE SEQA-IDDOKTYP       TO MOD-IDDOKTYP-ENTER                     
022800                                   MOD-IDDOKTYP(RADER)                    
022900       MOVE SEQA-IDDOK          TO MOD-IDDOK-ENTER                        
023000                                   MOD-IDDOK(RADER)                       
023100       MOVE SEQA-TIREGDAT       TO MOD-TIREGDAT(RADER)                    
023200       MOVE SEQA-TIREGTID       TO MOD-TIREGTID(RADER)                    
023300       MOVE SEQA-IDUSER         TO MOD-IDUSER(RADER)                      
023400       MOVE SEQA-BEDOK          TO MOD-BEDOK(RADER)                       
023500       ADD +1                   TO RADER                                  
023600       PERFORM IMS-GN-INFO                                                
023700       PERFORM UNTIL RADER > MAX-RADER                                    
023800         IF SEGMENT-FINNS                                                 
023900           MOVE SEQA-IDDOKTYP   TO MOD-IDDOKTYP(RADER)                    
024000           MOVE SEQA-IDDOK      TO MOD-IDDOK(RADER)                       
024100           MOVE SEQA-TIREGDAT   TO MOD-TIREGDAT(RADER)                    
024200           MOVE SEQA-TIREGTID   TO MOD-TIREGTID(RADER)                    
024300           MOVE SEQA-IDUSER     TO MOD-IDUSER(RADER)                      
024400           MOVE SEQA-BEDOK      TO MOD-BEDOK(RADER)                       
024500           PERFORM IMS-GN-INFO                                            
024600         ELSE                                                             
024700           MOVE MFS-RENSA-FAELT TO MOD-IDDOK(RADER)                       
024800                                   MOD-IDDOKTYP(RADER)                    
024900                                   MOD-TIREGDAT(RADER)                    
025000                                   MOD-TIREGTID(RADER)                    
025100                                   MOD-IDUSER(RADER)                      
025200                                   MOD-BEDOK(RADER)                       
025300         END-IF                                                           
025400         ADD +1                 TO RADER                                  
025500       END-PERFORM                                                        
025600       IF SEGMENT-FINNS                                                   
025700         MOVE MED1(INDX)        TO MOD-TEMFSINF                           
025800         MOVE SEQA-IDDOKTYP     TO MOD-IDDOKTYP-SPAR                      
025900         MOVE SEQA-IDDOK        TO MOD-IDDOK-SPAR                         
026000       ELSE                                                               
026100         MOVE MED2(INDX)        TO MOD-TEMFSINF                           
026200       END-IF                                                             
026300     ELSE                                                                 
026400       MOVE FEL2(INDX)          TO MOD-TEMFSFEL                           
026500     END-IF                                                               
026600     .                                                                    
026700                                                                          
026800     EJECT                                                                
026900* IMS SEKTIONER                                                           
027000                                                                          
027100 IMS-GET-MSG SECTION.                                                     
027200                                                                          
027300     MOVE '  QC' TO GODK-STATUSKODER                                      
027400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
027500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027600     PERFORM IMS-STATUSKONTROLL                                           
027700     .                                                                    
027800                                                                          
027900                                                                          
028000 IMS-INSERT-MSG SECTION.                                                  
028100                                                                          
028200     IF ENGLISH-TEXT                                                      
028300       MOVE 'N' TO MFS-KDHUVOMR                                           
028400     END-IF                                                               
028500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
028600     MOVE SPACE TO GODK-STATUSKODER                                       
028700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
028800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028900     PERFORM IMS-STATUSKONTROLL                                           
029000     .                                                                    
029100                                                                          
029200     EJECT                                                                
029300 IMS-GN-INFO SECTION.                                                     
029400                                                                          
029500     STRING 'WDP5A1  (WDP5A1KY>=' W-WDP5A1KY-MIN                          
029600                    '&WDP5A1KY<=' W-WDP5A1KY-MAX ')'                      
029700            DELIMITED BY SIZE INTO SSA1                                   
029800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
029900     CALL CBLTDLI USING GN  WDP5A-PCB DLI-IO-P5A1 SSA1                    
030000     MOVE WDP5A-STATUS-CODE TO STATUS-WS                                  
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300                                                                          
030400                                                                          
030500 IMS-STATUSKONTROLL SECTION.                                              
030600                                                                          
030700     SET STATUS-IX TO 1                                                   
030800     SEARCH GODK-STATUS                                                   
030900       AT END                                                             
031000         CALL FELLOG                                                      
031100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
031200         CONTINUE                                                         
031300     END-SEARCH                                                           
031400     .                                                                    
