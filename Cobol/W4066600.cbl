000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4066600.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   94/10/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER ALLA PÅBÖRJADE LASTBÄRARE PÅ HÄNDELSREG.                   
001000*                                                                         
001100*        PROGRAMMET LÄSER      WL4495 (WDR4)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T666                                              
001500*        MID:         W4I66601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O66601                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002401                                                                          
002410*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4066600'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003500 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
003700*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
003800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
003900                                                                          
004000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004100 77  WS-IDTRPTNR                 PIC X(3)    VALUE SPACE.                 
004200 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
004300 77  WS-FLFARLIG                 PIC X(1)    VALUE SPACE.                 
004400 77  WS-IDDC-IN                  PIC X(2)    VALUE SPACE.                 
004500                                                                          
004600 77  W-IDTRPTNR                  PIC S9(3)   COMP-3 VALUE ZERO.           
004700 77  W-IDTRPTNR-NEXT             PIC S9(3)   COMP-3 VALUE ZERO.           
004800 77  W-IDTRPTNR-ENTER            PIC S9(3)   COMP-3 VALUE ZERO.           
004900 77  W-IDDC                      PIC  X(2)          VALUE SPACE.          
005000 77  W-IDLBBET                   PIC  X(12)         VALUE SPACE.          
005100                                                                          
005200 77  SW-WL449501-LAEST           PIC X       VALUE 'N'.                   
005300     88  WL449501-LAEST                      VALUE 'J'.                   
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  EGEN-MID                            VALUE '4666'.                
006100     88  GODK-MID                            VALUE '4663' '4664'          
006200                                                   '4666'.                
006210     88  HELP-MID                            VALUE '0551'.                
006220     EJECT                                                                
006230*      --- VALID IDDC CODES                                               
006240*                                                                         
006250*01    -COPY WWDC99                                                       
006260       EJECT                                                              
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     EJECT                                                                
007000*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
007100*                                                                         
007200 01  FILLER                     PIC X(16)   VALUE 'W005INIT '.            
007300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007400*01 -COPY WMSGINIT                                                        
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007800     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
007910     03  ERR-NOTHING             PIC X(3)    VALUE '005'.                 
008000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008300     EJECT                                                                
008400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008700     SKIP3                                                                
008800*01  MID -COPY W4I66601                                                   
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009100     SKIP3                                                                
009200*01  -COPY WMSGAREA                                                       
009300     EJECT                                                                
009400     03  MOD REDEFINES MSG-AREA.                                          
009500*      05  -COPY W4O66601 -PRE MOD-                                       
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009800     SKIP3                                                                
009900*01  -COPY WMFSAREA                                                       
010000     EJECT                                                                
010100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010200*                                                                         
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010410 01  NYCKLAR-TILL-DLI.                                                    
010420     03  W-IDHTYP-MIN-X.                                                  
010430         05  W-IDHTYP-MIN        PIC X(4)    VALUE '4495'.                
010440     03  W-IDHTYP-MAX-X.                                                  
010450         05  W-IDHTYP-MAX        PIC X(4)    VALUE '4495'.                
010460     SKIP2                                                                
010500     SKIP3                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012100     SKIP2                                                                
012200 01  GODK-STATUSKODER.                                                    
012300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     SKIP3                                                                
012500 01  SSA1                        PIC X(96).                               
012600     EJECT                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013200     SKIP3                                                                
013300 01  DLI-IO-AREA.                                                         
013400     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013500     SKIP3                                                                
013600     03  WL449501 REDEFINES IO-AREA.                                      
013700*        05  -COPY WDGX4495                                               
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100*01  -COPY W0009   -PRE MSG-                                              
014200     EJECT                                                                
014300*01  -COPY W0008  -PRE USEA-                                              
014400     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014600*01  -COPY W0008  -PRE 4495-                                              
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 4495-PCB.                     
015000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 4495-PCB.                     
015100                                                                          
015200     PERFORM IMS-GET-MSG                                                  
015300     IF SEGMENT-FINNS                                                     
015400       PERFORM A-INIT                                                     
015500       PERFORM B-KOLLA-NYCKLAR                                            
015600       IF NYCKLAR-OK                                                      
015700           IF MFS-FIRST                                                   
015800             PERFORM C-FOERSTA-SIDA                                       
015900           ELSE                                                           
016000             IF MFS-NEXT                                                  
016100               PERFORM D-NAESTA-SIDA                                      
016200             ELSE                                                         
016300               PERFORM E-SAMMA-SIDA                                       
016400             END-IF                                                       
016500           END-IF                                                         
016600         PERFORM F-LAES-VISA-INFO                                         
016700       END-IF                                                             
016800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
016900       PERFORM IMS-INSERT-MSG                                             
017000     END-IF                                                               
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     MOVE NEJ                TO SW-WL449501-LAEST                         
017900                                                                          
018000     IF MSG-DUBBLA-TRANSKODER                                             
018100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66601                 
018200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018400     ELSE                                                                 
018500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66601                  
018600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018800     END-IF                                                               
018900                                                                          
019000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
019100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019300                                                                          
019400     MOVE LOW-VALUE   TO MSG-AREA                                         
019500     MOVE 'W4O666N1'  TO MFS-IDMOD                                        
019600     MOVE '4666' TO MOD-IDTRANS                                           
019700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019800                                                                          
019900     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O66601 + 4                  
020000                                                                          
020100     IF EGEN-MID OR HELP-MID                                              
020200       CONTINUE                                                           
020300     ELSE                                                                 
020400       MOVE SPACE TO MFS-KDTRTYP                                          
020500       MOVE '7' TO MFS-IDPFK                                              
020600     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 B-KOLLA-NYCKLAR SECTION.                                                 
021800                                                                          
021900     MOVE JA TO NYCKLAR-SW                                                
022000                                                                          
022100*    -- KONTROLL AV IDTRPTNR                                              
022200     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
022300                             MOD-IDDC-IN                                  
022400                                                                          
022500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022600     MOVE '001'             TO MSGI-KDCALL                                
022700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022710     MOVE '4666'            TO MSGI-IDTRANS                               
022720     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022810                                                                          
022840     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
022900                                                                          
023000     IF MID-IDTRPTNR-IN     =  ALL '+'                                    
023100       MOVE MID-IDTRPTNR-UT TO WS-IDTRPTNR                                
023200       INSPECT WS-IDTRPTNR REPLACING LEADING SPACE BY ZERO                
023300     ELSE                                                                 
023400       MOVE MID-IDTRPTNR-IN TO WS-IDTRPTNR                                
023500       MOVE '7'             TO MFS-IDPFK                                  
023600       MOVE SPACE           TO MFS-KDTRTYP                                
023700     END-IF                                                               
023800                                                                          
023900                                                                          
024000     IF WS-IDTRPTNR NUMERIC AND WS-IDTRPTNR > ZERO                        
024100       MOVE WS-IDTRPTNR     TO W-IDTRPTNR                                 
024200     ELSE                                                                 
024300       MOVE ZERO            TO W-IDTRPTNR                                 
024400     END-IF                                                               
024500                                                                          
024810     MOVE MSGI-IDDC         TO WS-IDDC                                    
024830     IF CDC-SE                                                            
024840        IF MID-IDDC-IN NOT = ALL '+'                                      
024850          MOVE MID-IDDC-IN      TO WS-IDDC                                
024860          IF DDC-SE                                                       
024870             MOVE MID-IDDC-IN   TO WS-IDDC-IN                             
024881             MOVE '7'             TO MFS-IDPFK                            
024882             MOVE SPACE           TO MFS-KDTRTYP                          
024890          ELSE                                                            
024891             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
024892          END-IF                                                          
024893        ELSE                                                              
024894          MOVE MID-IDDC-UT      TO WS-IDDC                                
024895          IF DDC-SE                                                       
024896             MOVE MID-IDDC-UT   TO WS-IDDC-IN                             
024897          ELSE                                                            
024898             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
024899          END-IF                                                          
024900        END-IF                                                            
024901     ELSE                                                                 
024902        MOVE MSGI-IDDC          TO WS-IDDC-IN                             
024903     END-IF                                                               
024904                                                                          
024905                                                                          
024910                                                                          
025000     MOVE MID-IDLBBET-UT    TO WS-IDLBBET                                 
025100     MOVE MID-FLFARLIG-UT   TO WS-FLFARLIG                                
025200                                                                          
025300     IF GODK-MID                                                          
025400       MOVE WS-IDLBBET       TO MOD-IDLBBET-UT                            
025500       MOVE WS-FLFARLIG      TO MOD-FLFARLIG-UT                           
025600       MOVE WS-IDTRPTNR      TO MOD-IDTRPTNR-UT                           
025610       MOVE WS-IDDC-IN       TO MOD-IDDC-UT                               
025700       INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE            
025800     ELSE                                                                 
025900       MOVE MFS-RENSA-FAELT  TO MOD-IDTRPTNR-UT                           
026000                                MOD-FLFARLIG-UT                           
026100                                MOD-IDTRPTNR-UT                           
026200       MOVE WS-IDDC-IN       TO MOD-IDDC-UT                               
026300     END-IF                                                               
026400                                                                          
026500     IF NYCKLAR-FEL                                                       
026600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
026700       CALL WMEDKONV USING MED-WMEDAREA                                   
026800       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
026900       PERFORM MFS-RENSA-FAELT-UT                                         
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 C-FOERSTA-SIDA SECTION.                                                  
027400                                                                          
027500     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
027600     CALL WMEDKONV USING MED-WMEDAREA                                     
027700     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
027800                                                                          
027900     .                                                                    
028000     EJECT                                                                
028100 D-NAESTA-SIDA SECTION.                                                   
028200                                                                          
028300     IF MID-IDTRPTNR-NEXT  NUMERIC AND                                    
028400        MID-IDTRPTNR-NEXT  > ZERO                                         
028500        MOVE MID-IDTRPTNR-NEXT        TO W-IDTRPTNR-NEXT                  
028600        PERFORM IMS-GU-WL449501                                           
028610        IF SEGMENT-FINNS AND                                              
028630           (4495-IDDC     = WS-IDDC-IN       AND                          
028640            4495-IDTRPTNR = W-IDTRPTNR-NEXT  AND                          
028650            4495-IDLBBET  = MID-IDLBBET-NEXT)                             
028660           MOVE JA                    TO SW-WL449501-LAEST                
028670        ELSE                                                              
028700          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                 
028701               WL449501-LAEST                                             
028710            PERFORM IMS-GN-WL449501                                       
028720            IF SEGMENT-FINNS                                              
028800              IF (4495-IDDC     = WS-IDDC-IN       AND                    
028900                  4495-IDTRPTNR = W-IDTRPTNR-NEXT  AND                    
028910                  4495-IDLBBET  = MID-IDLBBET-NEXT)                       
028920                MOVE JA               TO SW-WL449501-LAEST                
029100              END-IF                                                      
029110            END-IF                                                        
029200          END-PERFORM                                                     
029300        END-IF                                                            
029310     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 E-SAMMA-SIDA SECTION.                                                    
029700                                                                          
029800     IF EGEN-MID OR HELP-MID                                              
029900       IF MID-IDTRPTNR-ENTER NUMERIC                                      
030000          MOVE MID-IDTRPTNR-ENTER TO W-IDTRPTNR-ENTER                     
030100          PERFORM IMS-GU-WL449501                                         
030200          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                 
030300                       (4495-IDDC     = WS-IDDC-IN       AND              
030310                        4495-IDTRPTNR = W-IDTRPTNR-ENTER AND              
030400                        4495-IDLBBET = MID-IDLBBET-ENTER)                 
030500             PERFORM IMS-GN-WL449501                                      
030600             MOVE JA                TO SW-WL449501-LAEST                  
030700          END-PERFORM                                                     
030800       ELSE                                                               
030900         MOVE ZERO               TO W-IDTRPTNR                            
031000       END-IF                                                             
031100       MOVE MID-IDLBBET-ENTER    TO W-IDLBBET                             
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 F-LAES-VISA-INFO SECTION.                                                
031600                                                                          
031700     IF NOT WL449501-LAEST                                                
031800        PERFORM IMS-GU-WL449501                                           
031900        IF W-IDTRPTNR    > ZERO                                           
032000           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                
032100                       (4495-IDDC     = WS-IDDC-IN  AND                   
032110                        4495-IDTRPTNR = W-IDTRPTNR)                       
032200              PERFORM IMS-GN-WL449501                                     
032300           END-PERFORM                                                    
032400        END-IF                                                            
032500     END-IF                                                               
032700     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
032710        MOVE ERR-NOTHING    TO MED-IDMFSFEL                               
032800        CALL WMEDKONV USING MED-WMEDAREA                                  
032910        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
033000     ELSE                                                                 
033100       MOVE +1 TO INDX                                                    
033200       IF SEGMENT-FINNS                                                   
033300         MOVE 4495-IDTRPTNR    TO MOD-IDTRPTNR-ENTER                      
033400         MOVE 4495-IDLBBET     TO MOD-IDLBBET-ENTER                       
033500       ELSE                                                               
033600         MOVE ZERO             TO MOD-IDTRPTNR-ENTER                      
033700         MOVE SPACE            TO MOD-IDLBBET-ENTER                       
033800       END-IF                                                             
033900                                                                          
034000       PERFORM UNTIL INDX > MAX-INDX                                      
034100         IF SEGMENT-FINNS                                                 
034200           IF (W-IDTRPTNR         =  ZERO AND                             
034300               WS-IDDC-IN          =  4495-IDDC)    OR                    
034301              (WS-IDDC-IN          =  4495-IDDC     AND                   
034310               W-IDTRPTNR          =  4495-IDTRPTNR)                      
034400              MOVE 4495-IDTRPTNR  TO MOD-IDTRPTNR (INDX)                  
034500              MOVE 4495-IDLBBET   TO MOD-IDLBBET (INDX)                   
034600              ADD 1               TO INDX                                 
034700           END-IF                                                         
034800           PERFORM IMS-GN-WL449501                                        
034900         ELSE                                                             
035000           MOVE MFS-RENSA-FAELT   TO MOD-IDTRPTNR (INDX)                  
035100                                     MOD-IDLBBET (INDX)                   
035200           ADD 1                  TO INDX                                 
035300         END-IF                                                           
035400       END-PERFORM                                                        
035500                                                                          
035600       IF SEGMENT-FINNS                                                   
035601         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
035602           WS-IDDC-IN = 4495-IDDC                                         
035603           PERFORM IMS-GN-WL449501                                        
035604         END-PERFORM                                                      
035610         IF SEGMENT-FINNS                                                 
035700           MOVE 4495-IDTRPTNR        TO MOD-IDTRPTNR-NEXT                 
035800           MOVE 4495-IDLBBET         TO MOD-IDLBBET-NEXT                  
035900           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
036000           CALL WMEDKONV USING MED-WMEDAREA                               
036100           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
036101         ELSE                                                             
036102           MOVE ZERO                 TO MOD-IDTRPTNR-NEXT                 
036103           MOVE SPACE                TO MOD-IDLBBET-NEXT                  
036110         END-IF                                                           
036200       ELSE                                                               
036300         MOVE ZERO                 TO MOD-IDTRPTNR-NEXT                   
036400         MOVE SPACE                TO MOD-IDLBBET-NEXT                    
036500       END-IF                                                             
036600                                                                          
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 MFS-RENSA-FAELT-UT SECTION.                                              
037100                                                                          
037200*    --- ALLA UTDATA-FÄLT                                                 
037300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
037400     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-ENTER                           
037500                             MOD-IDTRPTNR-NEXT                            
037600                             MOD-IDLBBET-ENTER                            
037700                             MOD-IDLBBET-NEXT                             
037800                                                                          
037900     MOVE +1              TO INDX                                         
038000     PERFORM  UNTIL INDX  > MAX-INDX                                      
038100       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
038200       ADD +1             TO INDX                                         
038300     END-PERFORM                                                          
038400     .                                                                    
038500     SKIP3                                                                
038600 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
038700                                                                          
038800*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
038900     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR (INDX)                          
039000                             MOD-IDLBBET  (INDX)                          
039100     .                                                                    
039200     EJECT                                                                
039300* --- IMS SEKTIONER ---                                                   
039400     SKIP3                                                                
039500 IMS-GET-MSG SECTION.                                                     
039600                                                                          
039700     MOVE '  QC' TO GODK-STATUSKODER                                      
039800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040000     PERFORM IMS-STATUSKONTROLL                                           
040100     .                                                                    
040200     SKIP3                                                                
040300 IMS-INSERT-MSG SECTION.                                                  
040400                                                                          
040500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
040600       MOVE '0' TO MFS-KDHUVOMR                                           
040700     END-IF                                                               
040800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040900     MOVE SPACE TO GODK-STATUSKODER                                       
041000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041200     PERFORM IMS-STATUSKONTROLL                                           
041300     .                                                                    
041400     EJECT                                                                
041500 IMS-GU-WL449501  SECTION.                                                
041600     STRING 'WL449501(IDHTYP  >=' W-IDHTYP-MIN-X                          
041700                    '&IDHTYP  <=' W-IDHTYP-MAX-X ')'                      
041800                      DELIMITED BY SIZE INTO SSA1                         
041900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
042000     CALL CBLTDLI USING GU 4495-PCB IO-AREA SSA1                          
042100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     SKIP2                                                                
042500 IMS-GN-WL449501  SECTION.                                                
042600     STRING 'WL449501(IDHTYP  >=' W-IDHTYP-MIN-X                          
042700                    '&IDHTYP  <=' W-IDHTYP-MAX-X ')'                      
042800                      DELIMITED BY SIZE INTO SSA1                         
042900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
043000     CALL CBLTDLI USING GN 4495-PCB IO-AREA SSA1                          
043100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043400     SKIP2                                                                
043900 IMS-STATUSKONTROLL SECTION.                                              
044000                                                                          
044100     SET STATUS-IX TO 1                                                   
044200     SEARCH GODK-STATUS                                                   
044300       AT END                                                             
044400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044500         DELIMITED BY SIZE INTO FELTEXT                                   
044600         CALL FELLOG                                                      
044700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044800         CONTINUE                                                         
044900     END-SEARCH                                                           
045000     .                                                                    
