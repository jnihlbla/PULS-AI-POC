000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0062200.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   91/02/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DISPATCH-SYSTEMET.                                               
000900*        VISAR VÄNTANDE TRANS-GRUPPER FRÅN SEKUNDÄR-INDEX.                
001000*        VALD RAD SKICKAS TILL W00621.                                    
001100*                                                                         
001200*        PROGRAMMET LÄSER     WLKOMB (WDP8A)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T622                                              
001600*        MID:         W0I62201                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W0O62201                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W0062200'.            
003000 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
003100 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  WS-ISRT-SW                  PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003700 77  MAX-INDX                    PIC S9(9)   VALUE +14  COMP SYNC.        
003800                                                                          
003900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004000     88  NYCKLAR-OK                          VALUE 'J'.                   
004100     88  NYCKLAR-FEL                         VALUE 'N'.                   
004200                                                                          
004300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004400     88  ALLT-OK                             VALUE 'J'.                   
004500                                                                          
004600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004700     88  EGEN-MID                            VALUE '0622'.                
004800     88  GODK-MID                            VALUE '0621' '0622'.         
004900                                                                          
005000 01  WS-STYR.                                                             
005100   03  WS-NOD                    PIC X(1)    VALUE ' '.                   
005200   03  WS-JOB                    PIC X(1)    VALUE ' '.                   
005300   03  WS-STA                    PIC X(1)    VALUE ' '.                   
005400     EJECT                                                                
005500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005600 01  GENERELLA-SUBPROGRAM.                                                
005700   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
005800   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
005900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006100     EJECT                                                                
006200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006300*   -COPY WMSGINIT                                                        
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006600*   -COPY WMEDAREA                                                        
006700     SKIP3                                                                
006800 01  MESSAGE-CODES.                                                       
006900   03  INF-FIRST-PAGE            PIC X(3)    VALUE '006'.                 
007000   03  INF-MORE-INFO-EXISTS      PIC X(3)    VALUE '105'.                 
007010   03  INF-LAST-PAGE             PIC X(3)    VALUE '106'.                 
007100   03  ERR-WRONG-KEY             PIC X(3)    VALUE '401'.                 
007200   03  ERR-EMPTY                 PIC X(3)    VALUE '010'.                 
007300     EJECT                                                                
007400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007500*                                                                         
007600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007700                                                                          
007800*01  MID -COPY W0I62201                                                   
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008100                                                                          
008200*01  -COPY WMSGAREA                                                       
008300     EJECT                                                                
008400     05  MOD-MID REDEFINES MSG-MID-OUT.                                   
008500*      07  -COPY W0I62101 -PRE MOD-                                       
008600     EJECT                                                                
008700   03  MOD REDEFINES MSG-AREA.                                            
008800*    05  -COPY W0O62201                                                   
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009100*01  -COPY WMFSAREA                                                       
009200     EJECT                                                                
009300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009400                                                                          
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600                                                                          
009700 01  SAVE-AREA.                                                           
009800   03  SAVE-IDTRANS              PIC X(4)    VALUE SPACE.                 
009900   03  PGNO                      PIC 9(2).                                
010000   03  SAVE-WDP8A1KY-ENTER OCCURS 20                                      
010010                                 PIC X(25)   VALUE SPACE.                 
010100   03  SAVE-WDP8A1KY-NEXT        PIC X(25)   VALUE SPACE.                 
010200                                                                          
010300 01  NYCKLAR-TILL-DLI.                                                    
010400   03  W-WDP8A1KY-MIN.                                                    
010500     05  W-TIREGDAT-MIN          PIC S9(7)   VALUE ZERO COMP-3.           
010600     05  W-TIKLOCK-MIN           PIC S9(9)   VALUE ZERO COMP-3.           
010700     05  W-IDSNDNOD-MIN          PIC X(8)    VALUE LOW-VALUE.             
010800     05  W-IDSNDJOB-MIN          PIC X(8)    VALUE LOW-VALUE.             
010900   03  W-TIREGDAT                PIC S9(7)   VALUE ZERO COMP-3.           
011000   03  W-TIKLOCK                 PIC S9(9)   VALUE ZERO COMP-3.           
011100   03  W-IDSNDNOD                PIC X(8)    VALUE SPACE.                 
011200   03  W-IDSNDJOB                PIC X(8)    VALUE SPACE.                 
011300   03  W-KDKOMSTA                PIC X(1)    VALUE SPACE.                 
011400   03  W-WDP8A1KY-SPAR.                                                   
011500     05  W-TIREGDAT-SPAR         PIC S9(7)   VALUE ZERO COMP-3.           
011600     05  W-TIKLOCK-SPAR          PIC S9(9)   VALUE ZERO COMP-3.           
011700     05  W-IDSNDNOD-SPAR         PIC X(8)    VALUE SPACE.                 
011800     05  W-IDSNDJOB-SPAR         PIC X(8)    VALUE SPACE.                 
011900                                                                          
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500                                                                          
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800                                                                          
012900 01  SSA1                        PIC X(96).                               
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER                      PIC X(16)                                
013600                                     VALUE 'DLI-IO-KOMBA1'.               
013700                                                                          
013800 01  DLI-IO-KOMBA1.                                                       
013900*  05  -COPY WDP8A1 -PRE KOMB-                                            
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200*01  -COPY W0009      -PRE MSG-                                           
014300                                                                          
014400*01  -COPY W0009      -PRE ALT-                                           
014500     EJECT                                                                
014600*01  -COPY W0008      -PRE USEA-                                          
014700     05  FILLER                  PIC X.                                   
014800                                                                          
014900*01  -COPY W0008      -PRE KOMB-                                          
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB KOMB-PCB.             
015300 MAIN SECTION.                                                            
015400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB KOMB-PCB.             
015500                                                                          
015600     PERFORM IMS-GET-MSG                                                  
015700     IF SEGMENT-FINNS                                                     
015800       PERFORM A-INIT                                                     
015900       PERFORM B-KOLLA-NYCKLAR                                            
016000       IF NYCKLAR-OK                                                      
016100         IF MFS-SPLIT                                                     
016200           PERFORM G-TILL-0621                                            
016300         ELSE                                                             
016400           IF MFS-FIRST                                                   
016500             PERFORM C-FOERSTA-SIDA                                       
016600           ELSE                                                           
016700             IF MFS-NEXT                                                  
016800               PERFORM D-NAESTA-SIDA                                      
016900             ELSE                                                         
017000               IF MFS-PREVIOUS                                            
017100                 PERFORM I-PREV-SIDA                                      
017200               ELSE                                                       
017300                 PERFORM E-SAMMA-SIDA                                     
017400               END-IF                                                     
017500             END-IF                                                       
017600           END-IF                                                         
017700           IF ALLT-OK                                                     
017800             PERFORM F-LAES-VISA-INFO                                     
017900           END-IF                                                         
018000         END-IF                                                           
018100       END-IF                                                             
018200       IF MFS-SPLIT                                                       
018300         CONTINUE                                                         
018400       ELSE                                                               
018500         COMPUTE MSG-KVLL = LENGTH OF MOD-W0O62201 + 4                    
018600         PERFORM IMS-INSERT-MSG                                           
018700       END-IF                                                             
018800     END-IF                                                               
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500                                                                          
019600     MOVE WHEN-COMPILED TO W-COMPILED                                     
019700                                                                          
019800     IF MSG-DUBBLA-TRANSKODER                                             
019900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I62201                 
020000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
020100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020200     ELSE                                                                 
020300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I62201                  
020400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
020500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020600     END-IF                                                               
020700                                                                          
020800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
021000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021100                                                                          
021200     MOVE LOW-VALUE TO MSG-AREA                                           
021300     MOVE 'W0O62201' TO MFS-IDMOD                                         
021400     MOVE '0622' TO MOD-IDTRANS                                           
021500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021600     MOVE SPACE TO MED-WMEDAREA                                           
021700                                                                          
021800     IF EGEN-MID                                                          
021900         AND MID-IDSNDNOD-IN = ALL '+'                                    
022000         AND MID-IDSNDJOB-IN = ALL '+'                                    
022100         AND MID-TIREGDAT-IN = ALL '+'                                    
022200         AND MID-TIKLOCK-IN  = ALL '+'                                    
022300         AND MID-KDKOMSTA-IN = ALL '+'                                    
022400       CONTINUE                                                           
022500     ELSE                                                                 
022600       MOVE SPACE TO MFS-KDTRTYP                                          
022700       MOVE '7' TO MFS-IDPFK                                              
022800     END-IF                                                               
022900                                                                          
023000     .                                                                    
023100     EJECT                                                                
023200 B-KOLLA-NYCKLAR SECTION.                                                 
023300                                                                          
023400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
023500     MOVE '001' TO MSGI-KDCALL                                            
023600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023800     MOVE '0622'            TO MSGI-IDTRANS                               
023900     IF EGEN-MID                                                          
024000       MOVE MID-IDSNDNOD-IN TO MSGI-IDSNDNOD                              
024100       MOVE MID-IDSNDJOB-IN TO MSGI-IDSNDJOB                              
024200       MOVE MID-TIREGDAT-IN TO MSGI-TIREGDAT                              
024300       MOVE MID-TIKLOCK-IN  TO MSGI-TIKLOCK                               
024400       MOVE MID-KDKOMSTA-IN TO MSGI-KDKOMSTA                              
024500     END-IF                                                               
024600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024700     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
024800     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
024900                                                                          
025000     MOVE JA TO NYCKLAR-SW                                                
025100                                                                          
025200     MOVE MFS-RENSA-FAELT TO MOD-IDSNDNOD-IN                              
025300     MOVE MSGI-IDSNDNOD TO W-IDSNDNOD                                     
025400                                                                          
025500     MOVE MFS-RENSA-FAELT TO MOD-IDSNDJOB-IN                              
025600     MOVE MSGI-IDSNDJOB TO W-IDSNDJOB                                     
025700                                                                          
025800     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-IN                              
025900     INSPECT MSGI-TIREGDAT REPLACING LEADING SPACE BY ZERO                
026000     IF MSGI-TIREGDAT NUMERIC                                             
026100       MOVE MSGI-TIREGDAT TO W-TIREGDAT                                   
026200     ELSE                                                                 
026300       MOVE NEJ TO NYCKLAR-SW                                             
026400     END-IF                                                               
026500                                                                          
026600     MOVE MFS-RENSA-FAELT TO MOD-TIKLOCK-IN                               
026700     INSPECT MSGI-TIKLOCK REPLACING LEADING SPACE BY ZERO                 
026800     IF MSGI-TIKLOCK NUMERIC                                              
026900       MOVE MSGI-TIKLOCK TO W-TIKLOCK                                     
027000     ELSE                                                                 
027100       MOVE NEJ TO NYCKLAR-SW                                             
027200     END-IF                                                               
027300                                                                          
027400     MOVE MFS-RENSA-FAELT TO MOD-KDKOMSTA-IN                              
027500     MOVE MSGI-KDKOMSTA TO W-KDKOMSTA                                     
027600                                                                          
027700     IF GODK-MID OR NYCKLAR-OK                                            
027800       MOVE MSGI-IDSNDNOD TO MOD-IDSNDNOD-UT                              
027900       MOVE MSGI-IDSNDJOB TO MOD-IDSNDJOB-UT                              
028000       MOVE MSGI-TIREGDAT TO MOD-TIREGDAT-UT                              
028100       MOVE MSGI-TIKLOCK TO MOD-TIKLOCK-UT                                
028200       MOVE MSGI-KDKOMSTA TO MOD-KDKOMSTA-UT                              
028300       IF MSGI-IDSNDNOD = SPACE                                           
028400         MOVE SPACE TO WS-NOD                                             
028500       ELSE                                                               
028600         MOVE JA TO WS-NOD                                                
028700       END-IF                                                             
028800       IF MSGI-IDSNDJOB = SPACE                                           
028900         MOVE SPACE TO WS-JOB                                             
029000       ELSE                                                               
029100         MOVE JA TO WS-JOB                                                
029200       END-IF                                                             
029300       IF MSGI-KDKOMSTA = SPACE                                           
029400         MOVE SPACE TO WS-STA                                             
029500       ELSE                                                               
029600         MOVE JA TO WS-STA                                                
029700       END-IF                                                             
029800     ELSE                                                                 
029900       MOVE MFS-RENSA-FAELT TO MOD-IDSNDNOD-UT                            
030000                               MOD-IDSNDJOB-UT                            
030100                               MOD-TIREGDAT-UT                            
030200                               MOD-TIKLOCK-UT                             
030300                               MOD-KDKOMSTA-UT                            
030400     END-IF                                                               
030500                                                                          
030600     IF NYCKLAR-FEL                                                       
030700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
030800       CALL WMEDKONV USING MED-WMEDAREA                                   
030900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
031000       PERFORM MFS-RENSA-FAELT-UT                                         
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 C-FOERSTA-SIDA SECTION.                                                  
031500                                                                          
031510     MOVE 1 TO PGNO                                                       
031600     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
031700     CALL WMEDKONV USING MED-WMEDAREA                                     
031800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
031900                                                                          
032000     MOVE W-TIREGDAT TO W-TIREGDAT-MIN                                    
032100     MOVE W-TIKLOCK  TO W-TIKLOCK-MIN                                     
032200     MOVE W-IDSNDNOD TO W-IDSNDNOD-MIN                                    
032300     MOVE W-IDSNDJOB TO W-IDSNDJOB-MIN                                    
032400     MOVE JA TO ALLT-SW                                                   
032500     .                                                                    
032600     EJECT                                                                
032700 D-NAESTA-SIDA SECTION.                                                   
032800                                                                          
032900     IF SAVE-IDTRANS = '0622'                                             
033000       MOVE SAVE-WDP8A1KY-NEXT TO W-WDP8A1KY-MIN                          
033001       IF SAVE-WDP8A1KY-ENTER(PGNO) NOT = SAVE-WDP8A1KY-NEXT              
033002         IF PGNO = 20                                                     
033003           PERFORM VARYING PGNO FROM 1 BY 1                               
033004           UNTIL PGNO = 20                                                
033005             MOVE SAVE-WDP8A1KY-ENTER(PGNO + 1 )                          
033006                               TO SAVE-WDP8A1KY-ENTER(PGNO)               
033007           END-PERFORM                                                    
033008         ELSE                                                             
033009           ADD 1 TO PGNO                                                  
033010         END-IF                                                           
033020       END-IF                                                             
033100     ELSE                                                                 
033200       MOVE W-IDSNDNOD TO W-IDSNDNOD-MIN                                  
033300       MOVE W-IDSNDJOB TO W-IDSNDJOB-MIN                                  
033400       MOVE W-TIREGDAT TO W-TIREGDAT-MIN                                  
033500       MOVE W-TIKLOCK  TO W-TIKLOCK-MIN                                   
033510       MOVE 1 TO PGNO                                                     
033600     END-IF                                                               
033700     MOVE JA TO ALLT-SW                                                   
033800     .                                                                    
033900     EJECT                                                                
034000 I-PREV-SIDA SECTION.                                                     
034100                                                                          
034200     IF SAVE-IDTRANS = '0622'                                             
034300       IF PGNO > 1                                                        
034310         SUBTRACT 1 FROM PGNO                                             
034400         MOVE SAVE-WDP8A1KY-ENTER(PGNO) TO W-WDP8A1KY-MIN                 
034500       ELSE                                                               
034600         MOVE SAVE-WDP8A1KY-ENTER(1)      TO W-WDP8A1KY-MIN               
034620         MOVE INF-FIRST-PAGE TO MED-IDMFSINF                              
034630         CALL WMEDKONV USING MED-WMEDAREA                                 
034640         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
034700       END-IF                                                             
034800     ELSE                                                                 
034900       MOVE W-IDSNDNOD TO W-IDSNDNOD-MIN                                  
035000       MOVE W-IDSNDJOB TO W-IDSNDJOB-MIN                                  
035100       MOVE W-TIREGDAT TO W-TIREGDAT-MIN                                  
035200       MOVE W-TIKLOCK  TO W-TIKLOCK-MIN                                   
035300     END-IF                                                               
035400     MOVE JA TO ALLT-SW                                                   
035500     .                                                                    
035600     EJECT                                                                
035700 E-SAMMA-SIDA SECTION.                                                    
035800                                                                          
035900     IF SAVE-IDTRANS = '0622'                                             
036100       MOVE SAVE-WDP8A1KY-ENTER(PGNO) TO W-WDP8A1KY-MIN                   
036200     ELSE                                                                 
036210       MOVE 1 TO PGNO                                                     
036300       MOVE W-IDSNDNOD TO W-IDSNDNOD-MIN                                  
036400       MOVE W-IDSNDJOB TO W-IDSNDJOB-MIN                                  
036500       MOVE W-TIREGDAT TO W-TIREGDAT-MIN                                  
036600       MOVE W-TIKLOCK  TO W-TIKLOCK-MIN                                   
036700     END-IF                                                               
036800     MOVE JA TO ALLT-SW                                                   
036900     .                                                                    
037000     EJECT                                                                
037100 F-LAES-VISA-INFO SECTION.                                                
037200                                                                          
037300     MOVE +1 TO INDX                                                      
037400     PERFORM FA-LAES-DATA                                                 
037500     IF SEGMENT-FINNS                                                     
037600       MOVE KOMB-SEQA-TIREGDAT TO W-TIREGDAT-SPAR                         
037700       MOVE KOMB-SEQA-TIKLOCK  TO W-TIKLOCK-SPAR                          
037800       MOVE KOMB-SEQA-IDSNDNOD TO W-IDSNDNOD-SPAR                         
037900       MOVE KOMB-SEQA-IDSNDJOB TO W-IDSNDJOB-SPAR                         
038000       MOVE W-WDP8A1KY-SPAR TO SAVE-WDP8A1KY-ENTER(PGNO)                  
038100     ELSE                                                                 
038200       MOVE ERR-EMPTY TO MED-IDMFSFEL                                     
038300       CALL WMEDKONV USING MED-WMEDAREA                                   
038400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
038500       PERFORM MFS-RENSA-FAELT-UT                                         
038600     END-IF                                                               
038700                                                                          
038800     PERFORM UNTIL INDX > MAX-INDX                                        
038900       IF SEGMENT-FINNS                                                   
039000         MOVE SPACE              TO MOD-KDSVAR (INDX)                     
039100         MOVE KOMB-SEQA-IDSNDNOD TO MOD-IDSNDNOD (INDX)                   
039200         MOVE KOMB-SEQA-IDSNDJOB TO MOD-IDSNDJOB (INDX)                   
039300         MOVE KOMB-SEQA-TIREGDAT TO MOD-TIREGDAT (INDX)                   
039400         MOVE KOMB-SEQA-TIKLOCK  TO MOD-TIKLOCK (INDX)                    
039500         MOVE KOMB-SEQA-IDCPYTXT TO MOD-IDCPYTXT(INDX)                    
039600         MOVE KOMB-SEQA-IDLTERM  TO MOD-IDLTERM (INDX)                    
039700         MOVE KOMB-SEQA-IDUSER   TO MOD-IDUSER (INDX)                     
039800         MOVE KOMB-SEQA-IDMFSMED TO MOD-IDMFSMED (INDX)                   
039900         MOVE KOMB-SEQA-KDKOMSTA TO MOD-KDKOMSTA (INDX)                   
040000         MOVE KOMB-SEQA-KDKOMBEH TO MOD-KDKOMBEH (INDX)                   
040100         PERFORM FA-LAES-DATA                                             
040200       ELSE                                                               
040300         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
040400       END-IF                                                             
040500       ADD 1 TO INDX                                                      
040600     END-PERFORM                                                          
040700                                                                          
040800     IF SEGMENT-FINNS                                                     
040900       MOVE KOMB-SEQA-TIREGDAT TO W-TIREGDAT-SPAR                         
041000       MOVE KOMB-SEQA-TIKLOCK  TO W-TIKLOCK-SPAR                          
041100       MOVE KOMB-SEQA-IDSNDNOD TO W-IDSNDNOD-SPAR                         
041200       MOVE KOMB-SEQA-IDSNDJOB TO W-IDSNDJOB-SPAR                         
041300       MOVE W-WDP8A1KY-SPAR    TO SAVE-WDP8A1KY-NEXT                      
041400       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
041401     ELSE                                                                 
041410       MOVE INF-LAST-PAGE        TO MED-IDMFSINF                          
041420     END-IF                                                               
041500     CALL WMEDKONV USING MED-WMEDAREA                                     
041600     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
041700     MOVE '002' TO MSGI-KDCALL                                            
041800     MOVE '0622' TO SAVE-IDTRANS                                          
041900     MOVE SAVE-AREA TO MSGI-SPAR-AREA                                     
042000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042200     .                                                                    
042300     EJECT                                                                
042400 FA-LAES-DATA SECTION.                                                    
042500                                                                          
042600     EVALUATE WS-STYR                                                     
042700       WHEN '   ' PERFORM IMS-GET-KOMB                                    
042800       WHEN 'J  ' PERFORM IMS-GET-KOMB-NOD                                
042900       WHEN ' J ' PERFORM IMS-GET-KOMB-JOB                                
043000       WHEN '  J' PERFORM IMS-GET-KOMB-STA                                
043100       WHEN 'JJ ' PERFORM IMS-GET-KOMB-NOD-JOB                            
043200       WHEN 'J J' PERFORM IMS-GET-KOMB-NOD-STA                            
043300       WHEN ' JJ' PERFORM IMS-GET-KOMB-JOB-STA                            
043400       WHEN 'JJJ' PERFORM IMS-GET-KOMB-NOD-JOB-STA                        
043500     END-EVALUATE                                                         
043600     .                                                                    
043700     EJECT                                                                
043800 G-TILL-0621 SECTION.                                                     
043900                                                                          
044000     MOVE LOW-VALUE    TO MSG-KDZ1 MSG-KDZ2                               
044100     MOVE '0622'       TO MSG-IDTRANS-1                                   
044200     MOVE '1'          TO MSG-KDMFSFOR-1                                  
044300     MOVE '++++++++++' TO MOD-MID-TRANSDATA                               
044400     MOVE '0001'       TO MOD-MID-IDRADNR-IN                              
044500                          MOD-MID-IDRADNR-UT                              
044600     COMPUTE MSG-KVLL = LENGTH OF MOD-MID-W0I62101 + 17                   
044700     MOVE +1 TO INDX                                                      
044800     MOVE NEJ TO WS-ISRT-SW                                               
044900     PERFORM UNTIL INDX > MAX-INDX                                        
045000       IF MID-KDSVAR (INDX) = 'R' OR 'S' OR 'X'                           
045100         MOVE MID-IDSNDNOD (INDX) TO MOD-MID-IDSNDNOD                     
045200         MOVE MID-IDSNDJOB (INDX) TO MOD-MID-IDSNDJOB                     
045300         MOVE MID-TIREGDAT (INDX) TO MOD-MID-TIREGDAT                     
045400         MOVE MID-TIKLOCK  (INDX) TO MOD-MID-TIKLOCK                      
045500         IF MID-KDSVAR (INDX) = 'R'                                       
045600           MOVE 'W0T621X ' TO MSG-KDTRANS-1                               
045700           MOVE 'STA' TO MOD-MID-KDCMDVAL                                 
045800           PERFORM IMS-INSERT-ALT-MSG                                     
045900         ELSE                                                             
046000           IF WS-ISRT-SW = NEJ                                            
046100             MOVE 'W0T621 7' TO MSG-KDTRANS-1                             
046200             MOVE '+++' TO MOD-MID-KDCMDVAL                               
046300             MOVE JA TO WS-ISRT-SW                                        
046400             PERFORM IMS-INSERT-ALT-MSG                                   
046500            END-IF                                                        
046600         END-IF                                                           
046700       END-IF                                                             
046800       ADD +1 TO INDX                                                     
046900     END-PERFORM                                                          
047000     .                                                                    
047100     EJECT                                                                
047200 MFS-RENSA-FAELT-UT SECTION.                                              
047300                                                                          
047400     MOVE +1 TO INDX                                                      
047500     PERFORM UNTIL INDX > MAX-INDX                                        
047600       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
047700       ADD 1 TO INDX                                                      
047800     END-PERFORM                                                          
047900     .                                                                    
048000     SKIP2                                                                
048100 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
048200                                                                          
048300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
048400     MOVE MFS-RENSA-FAELT TO MOD-KDSVAR   (INDX)                          
048500                             MOD-IDSNDNOD (INDX)                          
048600                             MOD-IDSNDJOB (INDX)                          
048700                             MOD-TIREGDAT (INDX)                          
048800                             MOD-TIKLOCK  (INDX)                          
048900                             MOD-IDCPYTXT (INDX)                          
049000                             MOD-IDLTERM  (INDX)                          
049100                             MOD-IDUSER   (INDX)                          
049200                             MOD-IDMFSMED (INDX)                          
049300                             MOD-KDKOMSTA (INDX)                          
049400                             MOD-KDKOMBEH (INDX)                          
049500     .                                                                    
049600     EJECT                                                                
049700* --- IMS SEKTIONER ---                                                   
049800     SKIP3                                                                
049900 IMS-GET-MSG SECTION.                                                     
050000                                                                          
050100     MOVE '  QC' TO GODK-STATUSKODER                                      
050200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
050300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     .                                                                    
050600     SKIP3                                                                
050700 IMS-INSERT-ALT-MSG SECTION.                                              
050800                                                                          
050900     MOVE SPACE TO GODK-STATUSKODER                                       
051000     CALL CBLTDLI USING PURG ALT-PCB MSG-IO-AREA                          
051100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     SKIP3                                                                
051500 IMS-INSERT-MSG SECTION.                                                  
051600                                                                          
051700     IF MSGI-IDLAND-SPR = 'GB'                                            
051800*      MOVE 'N' TO MFS-KDHUVOMR                                           
051900       MOVE '0' TO MFS-KDHUVOMR                                           
052000     END-IF                                                               
052100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
052200     MOVE SPACE TO GODK-STATUSKODER                                       
052300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
052400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     .                                                                    
052700     EJECT                                                                
052800 IMS-GET-KOMB SECTION.                                                    
052900     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN ')'                      
053000          DELIMITED BY SIZE INTO SSA1                                     
053100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
053200     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
053300     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
053400     PERFORM IMS-STATUSKONTROLL                                           
053500     .                                                                    
053600     SKIP3                                                                
053700 IMS-GET-KOMB-NOD SECTION.                                                
053800     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN                          
053900                   '&IDSNDNOD =' W-IDSNDNOD ')'                           
054000          DELIMITED BY SIZE INTO SSA1                                     
054100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
054200     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
054300     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     .                                                                    
054600     SKIP3                                                                
054700 IMS-GET-KOMB-JOB SECTION.                                                
054800     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN                          
054900                   '&IDSNDJOB =' W-IDSNDJOB ')'                           
055000          DELIMITED BY SIZE INTO SSA1                                     
055100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
055200     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
055300     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     EJECT                                                                
055700 IMS-GET-KOMB-STA SECTION.                                                
055800     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN                          
055900                   '&KDKOMSTA =' W-KDKOMSTA ')'                           
056000          DELIMITED BY SIZE INTO SSA1                                     
056100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
056200     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
056300     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     SKIP3                                                                
056700 IMS-GET-KOMB-NOD-JOB SECTION.                                            
056800     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN                          
056900                   '&IDSNDNOD =' W-IDSNDNOD                               
057000                   '&IDSNDJOB =' W-IDSNDJOB ')'                           
057100          DELIMITED BY SIZE INTO SSA1                                     
057200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
057300     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
057400     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     .                                                                    
057700     SKIP3                                                                
057800 IMS-GET-KOMB-NOD-STA SECTION.                                            
057900     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN                          
058000                   '&IDSNDNOD =' W-IDSNDNOD                               
058100                   '&KDKOMSTA =' W-KDKOMSTA ')'                           
058200          DELIMITED BY SIZE INTO SSA1                                     
058300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
058400     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
058500     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
058600     PERFORM IMS-STATUSKONTROLL                                           
058700     .                                                                    
058800     EJECT                                                                
058900 IMS-GET-KOMB-JOB-STA SECTION.                                            
059000     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN                          
059100                   '&IDSNDJOB =' W-IDSNDJOB                               
059200                   '&KDKOMSTA =' W-KDKOMSTA ')'                           
059300          DELIMITED BY SIZE INTO SSA1                                     
059400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
059500     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
059600     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     SKIP3                                                                
060000 IMS-GET-KOMB-NOD-JOB-STA SECTION.                                        
060100     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-MIN                          
060200                   '&IDSNDNOD =' W-IDSNDNOD                               
060300                   '&IDSNDJOB =' W-IDSNDJOB                               
060400                   '&KDKOMSTA =' W-KDKOMSTA ')'                           
060500          DELIMITED BY SIZE INTO SSA1                                     
060600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
060700     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMBA1 SSA1                    
060800     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
060900     PERFORM IMS-STATUSKONTROLL                                           
061000     .                                                                    
061100     EJECT                                                                
061200 IMS-STATUSKONTROLL SECTION.                                              
061300                                                                          
061400     SET STATUS-IX TO 1                                                   
061500     SEARCH GODK-STATUS                                                   
061600       AT END                                                             
061700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
061800         DELIMITED BY SIZE INTO FELTEXT                                   
061900         CALL FELLOG                                                      
062000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062100         CONTINUE                                                         
062200     END-SEARCH                                                           
062300     .                                                                    
