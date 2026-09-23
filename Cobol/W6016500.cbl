000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6016500.                                                
000300 AUTHOR.         SUSANNE ENEGARD.                                         
000400 DATE-WRITTEN.   OKTOBER 1986.                                            
000500     REMARKS.                                                             
000600*    FUNKTION.   TP-PROGRAM FÖR FRÅGA PÅ HISTORIKREGISTRET                
000700*                WDL2 WDL6                                                
000800*                INLEVERANSINFORMATION.                                   
000900*    INDATA.                                                              
001000*        TRANSAKTION: W6T165                                              
001100*        MID:         W6I16501                                            
001200*    UTDATA.                                                              
001300*        MOD:         W6O16501                                            
001400*    SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP3                                                                
001700 DATA DIVISION.                                                           
001800     EJECT                                                                
001900 WORKING-STORAGE SECTION.                                                 
002000*    -COPY WY2000W1                                                       
002100     SKIP3                                                                
002200 77   PROGRAM-NAMN           VALUE 'W6016500'                             
002300                               PIC X(8).                                  
002400 77  JA                        PIC X(1)    VALUE 'J'.                     
002500 77  NEJ                       PIC X(1)    VALUE 'N'.                     
002600 77  DEL-RAPP-FINNS            PIC X(1)    VALUE 'N'.                     
002700 77  FOERSTA-GAANG             PIC X(1)    VALUE 'N'.                     
002800 77  FORTS-FINNS               PIC X(1)    VALUE 'N'.                     
002900 77  SPRAAK-IX                 PIC S9(9)   VALUE ZERO  COMP SYNC.         
003000 77  INDX                      PIC S9(9)   VALUE ZERO  COMP SYNC.         
003100 77  MAX-MOD-LAENGD            PIC S9(4)   VALUE +1228 COMP SYNC.         
003200 77  MAX-ANT-RADER             PIC S9(9)   VALUE +14   COMP SYNC.         
003300 01  WS-INLE-DAINLEV           PIC 9(16)   VALUE ZERO.                    
003400 01  WS-WDL6-DAINLEV           PIC 9(16)   VALUE ZERO.                    
003500 01  W-DEL-KVRAPP              PIC S9(7)   VALUE ZERO  COMP-3.            
003600 01  W-C2-FORDEL               PIC S9(7)   VALUE ZERO  COMP-3.            
003700 01  WS-TIUPPDAT               PIC S9(7)   VALUE ZERO  COMP-3.            
003800 01  WS-KVAVIS                 PIC S9(7)   VALUE ZERO  COMP-3.            
003900 01  WS-DEL-TIAAVVD            PIC S9(7)   VALUE ZERO  COMP-3.            
004000 01  WS-IDAVINR                PIC Z(6)9   VALUE ZERO.                    
004100 01  X                         PIC 9       VALUE ZERO.                    
004200 01  AX                        PIC 9       VALUE ZERO.                    
004300 01  BX                        PIC 9       VALUE ZERO.                    
004400                                                                          
004500 77  WS-INLE-ART-SW            PIC X       VALUE 'J'.                     
004600     88 WS-INLE-ART-FOUND                  VALUE 'J'.                     
004700     88 WS-INLE-ART-SAKNAS                 VALUE 'N'.                     
004800 77  WS-INLE-INL-SW            PIC X       VALUE 'J'.                     
004900     88 WS-INLE-INL-FOUND                  VALUE 'J'.                     
005000     88 WS-INLE-INL-SAKNAS                 VALUE 'N'.                     
005100 77  WS-WDL6-ART-SW            PIC X       VALUE 'J'.                     
005200     88 WS-WDL6-ART-FOUND                  VALUE 'J'.                     
005300     88 WS-WDL6-ART-SAKNAS                 VALUE 'N'.                     
005400 77  WS-WDL6-INL-SW            PIC X       VALUE 'J'.                     
005500     88 WS-WDL6-INL-FOUND                  VALUE 'J'.                     
005600     88 WS-WDL6-INL-SAKNAS                 VALUE 'N'.                     
005700                                                                          
005800*      --- VALID IDDC CODES                                               
005900*                                                                         
006000*01    -COPY WWDC99                                                       
006100       EJECT                                                              
006200 01  SPLIT-DAINLEV             PIC 9(16).                                 
006300 01  FILLER    REDEFINES SPLIT-DAINLEV.                                   
006400     03  SPLIT-TISEKEL         PIC 9(2).                                  
006500     03  SPLIT-TIAAMMDD        PIC 9(6).                                  
006600     03  FILLER                PIC 9(8).                                  
006700                                                                          
006800 01  IDARTNR-WS                PIC X(9).                                  
006900 01  FILLER    REDEFINES IDARTNR-WS.                                      
007000     03  KEY-IDARTNR           PIC 9(9).                                  
007100 01  IDDC-WS                   PIC X(2).                                  
007200 01  FILLER    REDEFINES IDDC-WS.                                         
007300     03  KEY-IDDC              PIC X(2).                                  
007400     EJECT                                                                
007500*- - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                      
007600 01  FILLER                    PIC X(16)  VALUE 'NYCKLAR-T-DLI'.          
007700 01  NYCKLAR-TILL-DLI.                                                    
007800     03  W-IDARTNR-X.                                                     
007900         05  W-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.            
008000     03  W-DAINLEV-X.                                                     
008100         05  W-DAINLEV         PIC 9(16)  VALUE ZERO.                     
008200     03  W-TIREGDAT-X.                                                    
008300         05  W-TIREGDAT        PIC S9(6)   VALUE ZERO  COMP-3.            
008400     03  W-IDLOPNRM-X.                                                    
008500         05  W-IDLOPNRM        PIC S9(9)   VALUE ZERO  COMP-3.            
008600     EJECT                                                                
008700*- - - - - - - - - - - - - - - - -  MEDDELANDEN                           
008800 01  FILLER                    PIC X(16)  VALUE 'MEDDELANDEN'.            
008900 01  MEDDELANDEN.                                                         
009000     03  FILLER-1.                                                        
009100         05 FILLER              PIC X(40)                                 
009200             VALUE '    FELAKTIG NYCKEL                     '.            
009300         05 FILLER              PIC X(40)                                 
009400             VALUE '    WRONG KEYS                          '.            
009500     03  FILLER REDEFINES FILLER-1.                                       
009600         05 FEL-1   OCCURS 2    PIC X(40).                                
009700     03  FILLER-2.                                                        
009800         05 FILLER              PIC X(40)                                 
009900             VALUE '    ARTIKELN FINNS EJ PÅ HISTORIKREG.   '.            
010000         05 FILLER              PIC X(40)                                 
010100             VALUE '    PART NO MISSING ON HISTORICAL FILE  '.            
010200     03  FILLER REDEFINES FILLER-2.                                       
010300         05 FEL-2   OCCURS 2    PIC X(40).                                
010400     SKIP3                                                                
010500     03  FILLER-11.                                                       
010600         05 FILLER              PIC X(61)                                 
010700             VALUE 'FLER TRANSAKTIONER FINNS PÅ AKTUELLT ARTNR'.          
010800         05 FILLER              PIC X(61)                                 
010900             VALUE 'MORE TRANSACTIONS EXIST P.T.O.          '.            
011000     03  FILLER REDEFINES FILLER-11.                                      
011100         05 MED-1   OCCURS 2    PIC X(61).                                
011200     EJECT                                                                
011300*- - - - - - - - - - - - - - - - -  DYNAMISKA SUB-PROGRAM                 
011400 01  FILLER                    PIC X(16)  VALUE 'DYN-SUB-PGM'.            
011500 01  DYN-SUB-PGM.                                                         
011600     03  WDATKONV              PIC X(8)   VALUE 'WDATKONV'.               
011700     03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.              
011800     03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.              
011900     03  W005INIT              PIC X(8)    VALUE 'W005INIT'.              
012000     SKIP2                                                                
012100 01  FILLER                    PIC X(16)  VALUE 'WDATAREA'.               
012200*01  -COPY WDATAREA                                                       
012300     EJECT                                                                
012400*                    ****   PARAMETRAR TILL W005INIT                      
012500*01  -COPY WMSGINIT                                                       
012600     EJECT                                                                
012700*                    ****   VALID IDDC CODES                              
012800 01  FILLER                    PIC X(16)  VALUE 'IDDC CODES'.             
012900*01  -COPY WWDCKONS                                                       
013000     EJECT                                                                
013100*- - - - - - - - - - - - - - - - -  MID-AREA                              
013200 01  FILLER                    PIC X(16)  VALUE 'MID-AREA   '.            
013300*    -COPY W6I16501.                                                      
013400     EJECT                                                                
013500*- - - - - - - - - - - - - - - - -  MSG-AREA                              
013600 01  FILLER                    PIC X(16)  VALUE 'MSG-AREA   '.            
013700*    -COPY WMSGAREA                                                       
013800     EJECT                                                                
013900*    03 POST -COPY W6O16501  -RED MSG-AREA.                               
014000     EJECT                                                                
014100*- - - - - - - - - - - - - - - - -  MFS-AREA                              
014200 01  FILLER                    PIC X(16)  VALUE 'MFS-AREA   '.            
014300*    -COPY WMFSAREA                                                       
014400     EJECT                                                                
014500*- - - - - - - - - - - - - - - - -  IMS-WS                                
014600 01  FILLER                    PIC X(16)  VALUE 'IMS-WS     '.            
014700 01      IMS-WS.                                                          
014800*                        **** STATUS-KOD FRÅN IMS                         
014900     03  STATUS-WS             PIC XX.                                    
015000         88  SEGMENT-FINNS       VALUE '  '.                              
015100         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
015200         88  BASEN-SLUT          VALUE 'GB'.                              
015300     SKIP3                                                                
015400     03  GODK-STATUSKODER.                                                
015500         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
015600     SKIP3                                                                
015700 01  SSA1                      PIC X(64).                                 
015800 01  SSA2                      PIC X(64).                                 
015900 01  SSA3                      PIC X(64).                                 
016000     EJECT                                                                
016100*                            IMS FUNKTIONSKODER                           
016200*01      -COPY W0003                                                      
016300     EJECT                                                                
016400*- - - - - - - - - - - - - - - - -  DLI-IO-AREA                           
016500 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-AREA'.            
016600 01  DLI-IO-AREA-2             PIC X(150)  VALUE SPACE.                   
016700     SKIP3                                                                
016800*01  WLINLE01 -COPY WDL201 -RED DLI-IO-AREA-2.                            
016900     EJECT                                                                
017000*01  WLINLE11 -COPY WDL211 -RED DLI-IO-AREA-2.                            
017100     EJECT                                                                
017200*01  WLINLE21 -COPY WDL221 -RED DLI-IO-AREA-2.                            
017300     EJECT                                                                
017400*01  WLINLE22 -COPY WDL222 -RED DLI-IO-AREA-2.                            
017500     EJECT                                                                
017600*01  WLINLE23 -COPY WDL223 -RED DLI-IO-AREA-2.                            
017700     EJECT                                                                
017800*01  WLINLE31 -COPY WDL231 -RED DLI-IO-AREA-2.                            
017900     EJECT                                                                
018000 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDL601'.                  
018100 01  DLI-IO-WDL601.                                                       
018200*    03  -COPY WDL601  -PRE WDL6-                                         
018300 01  FILLER             PIC X(16) VALUE 'DLI-IO-WDL611'.                  
018400 01  DLI-IO-WDL611.                                                       
018500*    03  -COPY WDL611  -PRE WDL6-                                         
018600     EJECT                                                                
018700 LINKAGE SECTION.                                                         
018800*01  -COPY W0009     -PRE MSG-                                            
018900     SKIP2                                                                
019000*    -COPY W0008     -PRE USEA-                                           
019100         05  FILLER       PIC X(1).                                       
019200     SKIP2                                                                
019300*    -COPY W0008     -PRE INLE-                                           
019400         05  FILLER       PIC X(1).                                       
019500     SKIP2                                                                
019600*    -COPY W0008     -PRE WDL6-                                           
019700         05  FILLER       PIC X(1).                                       
019800     EJECT                                                                
019900 PROCEDURE DIVISION USING MSG-PCB USEA-PCB INLE-PCB WDL6-PCB.             
020000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB INLE-PCB WDL6-PCB.            
020100*                                                                         
020200 STYR SECTION.                                                            
020300     PERFORM IMS-GET-MSG                                                  
020400     IF SEGMENT-FINNS                                                     
020500        PERFORM A-INIT-SPARA-INPUT                                        
020600        IF IDARTNR-WS NOT NUMERIC                                         
020700           MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                         
020800           PERFORM X-RENSA-BILD                                           
020900        ELSE                                                              
021000           MOVE KEY-IDARTNR TO W-IDARTNR                                  
021100           IF W-IDARTNR = ZERO                                            
021200              MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSFEL                      
021300              PERFORM X-RENSA-BILD                                        
021400           ELSE                                                           
021500              PERFORM B-REDIGERA-RAD                                      
021600           END-IF                                                         
021700        END-IF                                                            
021800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
021900        PERFORM IMS-INSERT-MSG                                            
022000     END-IF                                                               
022100                                                                          
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT-SPARA-INPUT SECTION.                                              
022700     IF MSG-DUBBLA-TRANSKODER                                             
022800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I16501                 
022900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023100     ELSE                                                                 
023200       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W6I16501                   
023300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023500     END-IF                                                               
023600                                                                          
023700     IF MFS-IDTRANS = '6165'                                              
023800        MOVE MSG-IDPFK TO MFS-IDPFK                                       
023900     ELSE                                                                 
024000        MOVE SPACE TO MFS-IDPFK                                           
024100     END-IF                                                               
024200                                                                          
024300     MOVE ALL '+' TO MSGI-WMSGINIT                                        
024400     MOVE '001'             TO MSGI-KDCALL                                
024500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024600     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
024700     MOVE '6165'                 TO MSGI-IDTRANS                          
024800                                                                          
024900     IF MFS-IDTRANS = '6165'                                              
025000     OR (MID-IDARTNR-IN NUMERIC                                           
025100     AND MID-IDARTNR-IN > ZERO)                                           
025200         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
025300     END-IF                                                               
025400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025500     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
025600     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
025700                                                                          
025800     IF MID-IDARTNR-IN = ALL '+'                                          
025900       CONTINUE                                                           
026000     ELSE                                                                 
026100       MOVE SPACE TO MFS-IDPFK                                            
026200     END-IF                                                               
026300                                                                          
026400     MOVE MSGI-IDDC TO WS-IDDC                                            
026500                                                                          
026600     IF MFS-IDTRANS = '6165'                                              
026700       IF MID-IDDC-IN = ALL '+'                                           
026800         MOVE MID-IDDC-UT TO WS-IDDC                                      
026900       ELSE                                                               
027000         MOVE MID-IDDC-IN   TO WS-IDDC                                    
027100         MOVE SPACE TO MFS-IDPFK                                          
027200       END-IF                                                             
027300     ELSE                                                                 
027400       MOVE MSGI-IDDC TO WS-IDDC                                          
027500     END-IF                                                               
027600                                                                          
027700     MOVE WS-IDDC     TO IDDC-WS                                          
027800*                        KEY-IDDC                                         
027900                                                                          
028000     MOVE LOW-VALUE TO MSG-AREA                                           
028100     MOVE 'W6O165N1' TO MFS-IDMOD                                         
028200     MOVE '6165' TO MOD-IDTRANS                                           
028300                                                                          
028400     IF MSGI-IDLAND-SPR = 'GB'                                            
028500       MOVE 2 TO SPRAAK-IX                                                
028600     ELSE                                                                 
028700       MOVE 1 TO SPRAAK-IX                                                
028800     END-IF                                                               
028900                                                                          
029000     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
029100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
029200     MOVE WS-IDDC TO MOD-IDDC-UT                                          
029300                                                                          
029400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
029500                             MOD-IDDC-IN                                  
029600                             MOD-TEMFSFEL                                 
029700                             MOD-TEMFSINF                                 
029800     .                                                                    
029900     EJECT                                                                
030000 B-REDIGERA-RAD SECTION.                                                  
030100     SKIP2                                                                
030200     MOVE +1                     TO INDX                                  
030300     MOVE NEJ                    TO DEL-RAPP-FINNS                        
030400     MOVE JA                     TO WS-WDL6-ART-SW                        
030500                                    WS-INLE-ART-SW                        
030600                                    WS-WDL6-INL-SW                        
030700                                    WS-INLE-INL-SW                        
030800     MOVE ZERO                   TO W-DAINLEV                             
030900     MOVE ZERO                   TO MOD-TIREGDAT-NEXT                     
031000                                                                          
031100     IF MFS-IDPFK = '8'                                                   
031200       IF MID-IDINLEV-NEXT > ZERO                                         
031300         MOVE MID-IDINLEV-NEXT   TO W-DAINLEV                             
031400         IF MID-IDINLEV-NEXT (1:1) = 0                                    
031500           MOVE 8                TO W-DAINLEV (1:1)                       
031600         ELSE                                                             
031700           MOVE 7                TO W-DAINLEV (1:1)                       
031800         END-IF                                                           
031900         MOVE MID-TIREGDAT-NEXT  TO W-TIREGDAT                            
032000       END-IF                                                             
032100     ELSE                                                                 
032200       MOVE ZERO                 TO W-TIREGDAT                            
032300     END-IF                                                               
032400                                                                          
032500     PERFORM IMS-GET-ART                                                  
032600     IF SEGMENT-SAKNAS                                                    
032700       MOVE NEJ                  TO WS-INLE-ART-SW                        
032800     END-IF                                                               
032900                                                                          
033000     PERFORM IMS-GET-WDL6-ART                                             
033100     IF SEGMENT-SAKNAS                                                    
033200       MOVE NEJ                  TO WS-WDL6-ART-SW                        
033300     END-IF                                                               
033400                                                                          
033500     IF WS-INLE-ART-FOUND OR WS-WDL6-ART-FOUND                            
033600       IF WS-INLE-ART-FOUND                                               
033700         PERFORM IMS-GET-INLEV                                            
033800         IF SEGMENT-SAKNAS                                                
033900           MOVE NEJ              TO WS-INLE-INL-SW                        
034000           MOVE ZERO             TO INL-DAINLEV                           
034100         END-IF                                                           
034200       ELSE                                                               
034300         MOVE NEJ                TO WS-INLE-INL-SW                        
034400         MOVE ZERO               TO INL-DAINLEV                           
034500       END-IF                                                             
034600                                                                          
034700       IF WS-WDL6-ART-FOUND AND KEY-IDDC = WC-CDC-SE                      
034800         PERFORM IMS-GET-WDL6-INL                                         
034900         IF SEGMENT-SAKNAS                                                
035000           MOVE NEJ              TO WS-WDL6-INL-SW                        
035100           MOVE ZERO             TO WDL6-INL-DAINLEV                      
035200         END-IF                                                           
035300       ELSE                                                               
035400         MOVE NEJ                TO WS-WDL6-INL-SW                        
035500                                    WS-WDL6-ART-SW                        
035600         MOVE ZERO               TO WDL6-INL-DAINLEV                      
035700       END-IF                                                             
035800       PERFORM                                                            
035900         UNTIL (WS-INLE-INL-SAKNAS AND WS-WDL6-INL-SAKNAS) OR             
036000               (INDX > MAX-ANT-RADER)                                     
036100         EVALUATE TRUE                                                    
036200           WHEN WS-INLE-INL-FOUND AND                                     
036300                WS-WDL6-INL-SAKNAS                                        
036400             PERFORM BA-PROCESS-INLE                                      
036500           WHEN WS-INLE-INL-SAKNAS AND                                    
036600                WS-WDL6-INL-FOUND                                         
036700             PERFORM BB-PROCESS-WDL6                                      
036800           WHEN WS-INLE-INL-FOUND AND                                     
036900                WS-WDL6-INL-FOUND                                         
037000             IF INL-DAINLEV < WDL6-INL-DAINLEV                            
037100               PERFORM BA-PROCESS-INLE                                    
037200             ELSE                                                         
037300               PERFORM BB-PROCESS-WDL6                                    
037400             END-IF                                                       
037500         END-EVALUATE                                                     
037600       END-PERFORM                                                        
037700                                                                          
037800       IF DEL-RAPP-FINNS = JA                                             
037900         CONTINUE                                                         
038000       ELSE                                                               
038100         IF WS-INLE-INL-SAKNAS AND WS-WDL6-INL-SAKNAS                     
038200           MOVE ZERO             TO MOD-IDINLEV-NEXT                      
038300           PERFORM D-BLANKA-RADER                                         
038400         ELSE                                                             
038500           PERFORM C-KOLLA-FORTS                                          
038600         END-IF                                                           
038700       END-IF                                                             
038800     ELSE                                                                 
038900       MOVE FEL-2 (SPRAAK-IX)    TO MOD-TEMFSFEL                          
039000       PERFORM X-RENSA-BILD                                               
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 BA-PROCESS-INLE SECTION.                                                 
039500     SKIP2                                                                
039600     MOVE INL-DAINLEV            TO W-DAINLEV                             
039700                                    SPLIT-DAINLEV                         
039800     PERFORM IMS-GET-31-32-310                                            
039900     IF SEGMENT-FINNS                                                     
040000       IF MOT-IDDC = KEY-IDDC                                             
040100         IF MOT-IDPTYP = 'R32'                                            
040200           PERFORM BAA-REDIGERA-R32                                       
040300           PERFORM S02-LAS-AK                                             
040400           ADD +1                TO INDX                                  
040500         ELSE                                                             
040600           PERFORM BAB-REDIGERA-R30-R31-310                               
040700         END-IF                                                           
040800       END-IF                                                             
040900     ELSE                                                                 
041000       PERFORM IMS-GET-33-34                                              
041100       IF SEGMENT-FINNS                                                   
041200         IF DIR-IDDC = KEY-IDDC                                           
041300           PERFORM BAC-REDIGERA-R33-R34                                   
041400           PERFORM S02-LAS-AK                                             
041500           ADD +1                TO INDX                                  
041600         END-IF                                                           
041700       ELSE                                                               
041800         PERFORM IMS-GET-40                                               
041900         IF SEGMENT-FINNS                                                 
042000           IF RET-IDDC = KEY-IDDC                                         
042100             PERFORM BAD-REDIGERA-R40                                     
042200             PERFORM S02-LAS-AK                                           
042300             ADD +1              TO INDX                                  
042400           END-IF                                                         
042500         END-IF                                                           
042600       END-IF                                                             
042700     END-IF                                                               
042800     IF DEL-RAPP-FINNS = JA                                               
042900       CONTINUE                                                           
043000     ELSE                                                                 
043100       PERFORM IMS-GET-INLEV                                              
043200       IF SEGMENT-SAKNAS                                                  
043300         MOVE NEJ                TO WS-INLE-INL-SW                        
043400       END-IF                                                             
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 BAA-REDIGERA-R32 SECTION.                                                
043900     SKIP2                                                                
044000     MOVE SPACE             TO MOD-INFO-RAD (INDX)                        
044100                                                                          
044200     MOVE MOT-IDPTYP        TO MOD-IDPTYP(INDX)                           
044300     MOVE MOT-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
044400                               W-IDLOPNRM                                 
044500                                                                          
044600     MOVE MOT-TIUPPDAT      TO WS-TIUPPDAT                                
044700     PERFORM Y-KONV-AAVVD                                                 
044800     IF DAT-KDSVAR-OK                                                     
044900        MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                          
045000     ELSE                                                                 
045100        MOVE ZERO           TO MOD-TIAAVVD(INDX)                          
045200     END-IF                                                               
045300                                                                          
045400     MOVE MOT-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
045500     MOVE MOT-KDRT          TO MOD-KDRT(INDX)                             
045600     MOVE MOT-ADLAGOMR      TO MOD-ADLAGOMR(INDX)                         
045700     MOVE MOT-ADGANG        TO MOD-ADGANG(INDX)                           
045800     MOVE MOT-ADPLATS       TO MOD-ADPLATS(INDX)                          
033300     IF MOT-IDFS > SPACE                                                  
033400        MOVE MOT-IDFS    TO MOD-IDFS(INDX)                                
033600     ELSE                                                                 
033700        MOVE MOT-IDAVINR TO MOD-IDAVINR(INDX)                             
033900     END-IF                                                               
046000     MOVE MOT-KVANTMOT      TO MOD-KVAVIS(INDX)                           
046100     MOVE MOT-KVFORDEL      TO MOD-KVFORDEL(INDX)                         
046200     MOVE MOT-KVRETUR       TO MOD-KVRETUR(INDX)                          
046300     MOVE MOT-KDAVVANT      TO MOD-KDAVVANT(INDX)                         
046400     .                                                                    
046500     EJECT                                                                
046600 BAB-REDIGERA-R30-R31-310 SECTION.                                        
046700     SKIP2                                                                
046800     MOVE JA   TO FOERSTA-GAANG                                           
046900     MOVE ZERO TO W-C2-FORDEL                                             
047000                                                                          
047100     MOVE SPACE             TO MOD-INFO-RAD (INDX)                        
047200                                                                          
047300     MOVE MOT-IDPTYP        TO MOD-IDPTYP(INDX)                           
047400     MOVE MOT-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
047500                               W-IDLOPNRM                                 
047600                                                                          
047700     PERFORM S01-KONV-IDINLEV                                             
047800     IF DAT-KDSVAR-OK                                                     
047900        MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                          
048000                               WS-DEL-TIAAVVD                             
048100     ELSE                                                                 
048200        MOVE ZERO           TO MOD-TIAAVVD(INDX)                          
048300                               WS-DEL-TIAAVVD                             
048400     END-IF                                                               
048500                                                                          
048600     MOVE MOT-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
048700     MOVE MOT-KDRT          TO MOD-KDRT(INDX)                             
048800     MOVE MOT-ADLAGOMR      TO MOD-ADLAGOMR(INDX)                         
048900     MOVE MOT-ADGANG        TO MOD-ADGANG(INDX)                           
049000     MOVE MOT-ADPLATS       TO MOD-ADPLATS(INDX)                          
033300     IF MOT-IDFS > SPACE                                                  
033400        MOVE MOT-IDFS    TO MOD-IDFS(INDX)                                
033600     ELSE                                                                 
033700        MOVE MOT-IDAVINR TO MOD-IDAVINR(INDX)                             
033900     END-IF                                                               
049200     MOVE MOT-KVAVIS        TO MOD-KVAVIS(INDX)                           
049300     MOVE MOT-KVFORDEL      TO MOD-KVFORDEL(INDX)                         
049400     MOVE MOT-KVRETUR       TO MOD-KVRETUR(INDX)                          
049500     MOVE MOT-KDAVVANT      TO MOD-KDAVVANT(INDX)                         
049600                                                                          
049700     PERFORM S02-LAS-AK                                                   
049800                                                                          
049900     ADD +1 TO INDX                                                       
050000                                                                          
050100     PERFORM BABA-LAES-P32                                                
050200                                                                          
050300     .                                                                    
050400     EJECT                                                                
050500 BABA-LAES-P32 SECTION.                                                   
050600     SKIP2                                                                
050700     IF W-TIREGDAT > ZERO                                                 
050800        PERFORM IMS-GET-DEL                                               
050900        IF SEGMENT-FINNS                                                  
051000          MOVE DEL-TIREGDAT   TO TMP1-YYMMDD                              
051100          MOVE W-TIREGDAT     TO TMP2-YYMMDD                              
051200          PERFORM WY2000P1                                                
051300        END-IF                                                            
051400        PERFORM UNTIL SEGMENT-SAKNAS OR TMP1-YYMMDD > TMP2-YYMMDD         
051500           PERFORM IMS-GET-DEL                                            
051600           IF SEGMENT-FINNS                                               
051700             MOVE DEL-TIREGDAT   TO TMP1-YYMMDD                           
051800             MOVE W-TIREGDAT     TO TMP2-YYMMDD                           
051900             PERFORM WY2000P1                                             
052000           END-IF                                                         
052100        END-PERFORM                                                       
052200     ELSE                                                                 
052300        PERFORM IMS-GET-DEL                                               
052400     END-IF                                                               
052500     MOVE ZERO TO WS-TIUPPDAT                                             
052600                                                                          
052700     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-ANT-RADER                 
052800        IF DEL-IDDC = KEY-IDDC                                            
052900           MOVE SPACE          TO MOD-INFO-RAD (INDX)                     
053000                                                                          
053100           IF FOERSTA-GAANG = JA                                          
053200              MOVE 'P32'       TO MOD-IDPTYP (INDX)                       
053300              MOVE NEJ TO FOERSTA-GAANG                                   
053400           END-IF                                                         
053500                                                                          
053600           IF DEL-TIREGDAT NOT = WS-TIUPPDAT                              
053700             MOVE DEL-KVRAPP     TO MOD-KVAVIS(INDX)                      
053800                                    WS-KVAVIS                             
053900             MOVE DEL-TIREGDAT   TO WS-TIUPPDAT                           
054000             PERFORM Y-KONV-AAVVD                                         
054100             IF DAT-KDSVAR-OK                                             
054200                MOVE DAT-TIAAVVD TO MOD-TIAAVVD(INDX)                     
054300             ELSE                                                         
054400                MOVE ZERO        TO MOD-TIAAVVD(INDX)                     
054500             END-IF                                                       
054600           ELSE                                                           
054700             ADD DEL-KVRAPP      TO WS-KVAVIS                             
054800             SUBTRACT 1 FROM INDX                                         
054900             MOVE WS-KVAVIS      TO MOD-KVAVIS(INDX)                      
055000           END-IF                                                         
055100           ADD +1 TO INDX                                                 
055200        END-IF                                                            
055300        PERFORM IMS-GET-DEL                                               
055400     END-PERFORM                                                          
055500                                                                          
055600     IF SEGMENT-FINNS                                                     
055700        IF DEL-IDDC = KEY-IDDC                                            
055800           MOVE JA TO DEL-RAPP-FINNS                                      
055900           MOVE W-DAINLEV (2:15) TO MOD-IDINLEV-NEXT                      
056000           MOVE DEL-TIREGDAT   TO MOD-TIREGDAT-NEXT                       
056100        ELSE                                                              
056200           PERFORM BABAA-LAS-NEXT                                         
056300        END-IF                                                            
056400     ELSE                                                                 
056500        MOVE NEJ TO DEL-RAPP-FINNS                                        
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 BABAA-LAS-NEXT SECTION.                                                  
057000     SKIP2                                                                
057100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
057200        (DEL-IDDC = KEY-IDDC)                                             
057300        PERFORM IMS-GET-DEL                                               
057400     END-PERFORM                                                          
057500                                                                          
057600     IF SEGMENT-FINNS                                                     
057700        MOVE JA           TO DEL-RAPP-FINNS                               
057800        MOVE W-DAINLEV (2:15)  TO MOD-IDINLEV-NEXT                        
057900        MOVE DEL-TIREGDAT TO MOD-TIREGDAT-NEXT                            
058000     ELSE                                                                 
058100        MOVE NEJ TO DEL-RAPP-FINNS                                        
058200     END-IF                                                               
058300                                                                          
058400     .                                                                    
058500     EJECT                                                                
058600 BAC-REDIGERA-R33-R34 SECTION.                                            
058700     SKIP2                                                                
058800     MOVE SPACE             TO MOD-INFO-RAD (INDX)                        
058900                                                                          
059000     MOVE DIR-IDPTYP        TO MOD-IDPTYP(INDX)                           
059100     MOVE DIR-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
059200                               W-IDLOPNRM                                 
059300     MOVE DIR-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
059400     MOVE DIR-KDRT          TO MOD-KDRT(INDX)                             
059500     MOVE DIR-IDAVINR       TO MOD-IDAVINR(INDX)                          
059600     MOVE DIR-KVAVIS        TO MOD-KVAVIS(INDX)                           
059700                                                                          
059800     PERFORM S01-KONV-IDINLEV                                             
059900     IF DAT-KDSVAR-OK                                                     
060000        MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                          
060100                               WS-DEL-TIAAVVD                             
060200     ELSE                                                                 
060300        MOVE ZERO           TO MOD-TIAAVVD(INDX)                          
060400                               WS-DEL-TIAAVVD                             
060500     END-IF                                                               
060600                                                                          
060700     .                                                                    
060800     EJECT                                                                
060900 BAD-REDIGERA-R40 SECTION.                                                
061000     SKIP2                                                                
061100     MOVE SPACE             TO MOD-INFO-RAD (INDX)                        
061200                                                                          
061300     MOVE RET-IDPTYP        TO MOD-IDPTYP(INDX)                           
061400     MOVE RET-IDLOPNRM      TO MOD-IDLOPNRM(INDX)                         
061500                               W-IDLOPNRM                                 
061600     MOVE RET-IDLEVNR       TO MOD-IDLEVNR(INDX)                          
061700     MOVE RET-IDORDNR       TO MOD-IDAVINR(INDX)                          
061800     MOVE RET-KVRETUR       TO MOD-KVRETUR(INDX)                          
061900                                                                          
062000     PERFORM S01-KONV-IDINLEV                                             
062100     IF DAT-KDSVAR-OK                                                     
062200        MOVE DAT-TIAAVVD    TO MOD-TIAAVVD(INDX)                          
062300                               WS-DEL-TIAAVVD                             
062400     ELSE                                                                 
062500        MOVE ZERO           TO MOD-TIAAVVD(INDX)                          
062600                               WS-DEL-TIAAVVD                             
062700     END-IF                                                               
062800                                                                          
062900     .                                                                    
063000     EJECT                                                                
063100 BB-PROCESS-WDL6 SECTION.                                                 
063200     SKIP2                                                                
063300     MOVE WDL6-INL-DAINLEV       TO SPLIT-DAINLEV                         
063400                                                                          
063500     IF KEY-IDDC = WDL6-INL-IDDC                                          
063600       PERFORM BBA-WRITE-WDL6-DATA                                        
063700     END-IF                                                               
063800                                                                          
063900     PERFORM IMS-GET-WDL6-INL                                             
064000     IF SEGMENT-SAKNAS                                                    
064100       MOVE NEJ                  TO WS-WDL6-INL-SW                        
064200     END-IF                                                               
064300     .                                                                    
064400     EJECT                                                                
064500 BBA-WRITE-WDL6-DATA SECTION.                                             
064600     SKIP2                                                                
064700     MOVE SPACE                  TO MOD-INFO-RAD    (INDX)                
064800     MOVE WDL6-INL-IDPTYP        TO MOD-IDPTYP      (INDX)                
064900     MOVE WDL6-INL-IDLEVNR       TO MOD-IDLEVNR     (INDX)                
065000     MOVE WDL6-INL-KDRT          TO MOD-KDRT        (INDX)                
065100     MOVE WDL6-INL-ADLAGOMR      TO MOD-ADLAGOMR    (INDX)                
065200     MOVE WDL6-INL-ADGANG        TO MOD-ADGANG      (INDX)                
065300     MOVE WDL6-INL-ADPLATS       TO MOD-ADPLATS     (INDX)                
065400                                                                          
065500     IF WDL6-INL-IDPTYP = 'R30' OR '310'                                  
065600       MOVE WDL6-INL-IDFAKT      TO MOD-IDLOPNRM    (INDX)                
065700       MOVE WDL6-INL-KVAVIS      TO MOD-KVAVIS      (INDX)                
065800     ELSE                                                                 
065900       IF WDL6-INL-IDPTYP = 'R31'                                         
066000         MOVE WDL6-INL-IDLOPNRM  TO MOD-IDLOPNRM    (INDX)                
066100         MOVE WDL6-INL-KVAVIS    TO MOD-KVAVIS      (INDX)                
066200       ELSE                                                               
066300         IF WDL6-INL-IDPTYP = 'R32'                                       
066400           MOVE WDL6-INL-KVANTMOT                                         
066500                                 TO MOD-KVAVIS      (INDX)                
066600           IF WDL6-INL-IDLOPNRM = 0                                       
066700             MOVE WDL6-INL-IDFAKT                                         
066800                                 TO MOD-IDLOPNRM    (INDX)                
066900           ELSE                                                           
067000             MOVE WDL6-INL-IDLOPNRM                                       
067100                                 TO MOD-IDLOPNRM    (INDX)                
067200           END-IF                                                         
067300                                                                          
067400           IF WDL6-INL-FLMAKUL = 'J' OR 'Y'                               
067500             MOVE '2'            TO MOD-KDAVVANT    (INDX)                
067600           ELSE                                                           
067700             IF WDL6-INL-KVANTMOT = WDL6-INL-KVAVIS OR                    
067800               (WDL6-INL-KVANTMOT = WDL6-INL-KVAVIS * -1)                 
067900               MOVE '0'          TO MOD-KDAVVANT    (INDX)                
068000             ELSE                                                         
068100               MOVE '1'          TO MOD-KDAVVANT    (INDX)                
068200             END-IF                                                       
068300           END-IF                                                         
068400                                                                          
068500         ELSE                                                             
068600           MOVE WDL6-INL-KVAVIS  TO MOD-KVAVIS      (INDX)                
068700           MOVE WDL6-INL-IDLOPNRM                                         
068800                                 TO MOD-IDLOPNRM    (INDX)                
068900         END-IF                                                           
069000       END-IF                                                             
069100     END-IF                                                               
069200     PERFORM BBAA-FLYTTA-IDAVINR                                          
069300     MOVE WS-IDAVINR             TO MOD-IDAVINR     (INDX)                
069400                                                                          
069500     MOVE WDL6-INL-TIINLINL      TO WS-TIUPPDAT                           
069600     PERFORM Y-KONV-AAVVD                                                 
069700     IF DAT-KDSVAR-OK                                                     
069800       MOVE DAT-TIAAVVD          TO MOD-TIAAVVD     (INDX)                
069900     ELSE                                                                 
070000       MOVE ZERO                 TO MOD-TIAAVVD     (INDX)                
070100     END-IF                                                               
070200     MOVE WDL6-INL-KVART-SKROT   TO MOD-KVRETUR     (INDX)                
070300     MOVE ZERO                   TO MOD-KVFORDEL    (INDX)                
070400                                                                          
070500     ADD +1                      TO INDX                                  
070600     .                                                                    
070700     EJECT                                                                
070800 BBAA-FLYTTA-IDAVINR SECTION.                                             
070900     SKIP2                                                                
071000     MOVE +7                     TO AX                                    
071100     MOVE +7                     TO BX                                    
071200     MOVE +1                     TO X                                     
071300     MOVE ZERO                   TO WS-IDAVINR                            
071400     PERFORM                                                              
071500       UNTIL X > 7                                                        
071600       IF WDL6-INL-IDKUNDRF(AX:1) >= 0                                    
071700         MOVE WDL6-INL-IDKUNDRF(AX:1)                                     
071800                                 TO WS-IDAVINR(BX:1)                      
071900         SUBTRACT 1            FROM AX                                    
072000         SUBTRACT 1            FROM BX                                    
072100         ADD 1                   TO X                                     
072200       ELSE                                                               
072300         SUBTRACT 1            FROM AX                                    
072400         ADD 1                   TO X                                     
072500       END-IF                                                             
072600     END-PERFORM                                                          
072700     .                                                                    
072800     EJECT                                                                
072900                                                                          
073000 C-KOLLA-FORTS SECTION.                                                   
073100     SKIP2                                                                
073200     MOVE NEJ TO FORTS-FINNS                                              
073300                                                                          
073400     IF WS-INLE-INL-FOUND                                                 
073500       PERFORM                                                            
073600         UNTIL WS-INLE-INL-SAKNAS OR                                      
073700               FORTS-FINNS = JA                                           
073800         MOVE INL-DAINLEV        TO W-DAINLEV                             
073900                                    WS-INLE-DAINLEV                       
074000         PERFORM IMS-GET-31-32-310                                        
074100         IF SEGMENT-FINNS                                                 
074200           IF MOT-IDDC = KEY-IDDC                                         
074300             MOVE JA             TO FORTS-FINNS                           
074400           END-IF                                                         
074500         ELSE                                                             
074600           PERFORM IMS-GET-33-34                                          
074700           IF SEGMENT-FINNS                                               
074800             IF DIR-IDDC = KEY-IDDC                                       
074900               MOVE JA           TO FORTS-FINNS                           
075000             END-IF                                                       
075100           ELSE                                                           
075200             PERFORM IMS-GET-40                                           
075300             IF SEGMENT-FINNS                                             
075400               IF RET-IDDC = KEY-IDDC                                     
075500                 MOVE JA         TO FORTS-FINNS                           
075600               END-IF                                                     
075700             END-IF                                                       
075800           END-IF                                                         
075900         END-IF                                                           
076000         IF FORTS-FINNS = JA                                              
076100           CONTINUE                                                       
076200         ELSE                                                             
076300           PERFORM IMS-GET-INLEV                                          
076400           IF SEGMENT-SAKNAS                                              
076500             MOVE NEJ            TO WS-INLE-INL-SW                        
076600           END-IF                                                         
076700         END-IF                                                           
076800       END-PERFORM                                                        
076900     END-IF                                                               
077000                                                                          
077100     MOVE NEJ TO FORTS-FINNS                                              
077200                                                                          
077300     IF WS-WDL6-INL-FOUND                                                 
077400       PERFORM                                                            
077500         UNTIL WS-WDL6-INL-SAKNAS OR                                      
077600               FORTS-FINNS = JA                                           
077700         MOVE WDL6-INL-DAINLEV   TO W-DAINLEV                             
077800                                    WS-WDL6-DAINLEV                       
077900         IF KEY-IDDC = WDL6-INL-IDDC                                      
078000           MOVE JA               TO FORTS-FINNS                           
078100         END-IF                                                           
078200         IF FORTS-FINNS = JA                                              
078300           CONTINUE                                                       
078400         ELSE                                                             
078500           PERFORM IMS-GET-WDL6-INL                                       
078600           IF SEGMENT-SAKNAS                                              
078700             MOVE NEJ            TO WS-WDL6-INL-SW                        
078800           END-IF                                                         
078900         END-IF                                                           
079000       END-PERFORM                                                        
079100     END-IF                                                               
079200                                                                          
079300     EVALUATE TRUE                                                        
079400       WHEN WS-INLE-DAINLEV > ZERO AND                                    
079500            WS-WDL6-DAINLEV > ZERO                                        
079600         IF WS-INLE-DAINLEV < WS-WDL6-DAINLEV                             
079700           MOVE WS-INLE-DAINLEV (2:15)                                    
079800                                 TO MOD-IDINLEV-NEXT                      
079900         ELSE                                                             
080000           MOVE WS-WDL6-DAINLEV (2:15)                                    
080100                                 TO MOD-IDINLEV-NEXT                      
080200         END-IF                                                           
080300         MOVE MED-1 (SPRAAK-IX)  TO MOD-TEMFSINF                          
080400       WHEN WS-INLE-DAINLEV = ZERO AND                                    
080500            WS-WDL6-DAINLEV > ZERO                                        
080600         MOVE WS-WDL6-DAINLEV (2:15)                                      
080700                                 TO MOD-IDINLEV-NEXT                      
080800         MOVE MED-1 (SPRAAK-IX)  TO MOD-TEMFSINF                          
080900       WHEN WS-INLE-DAINLEV > ZERO AND                                    
081000            WS-WDL6-DAINLEV = ZERO                                        
081100         MOVE WS-INLE-DAINLEV (2:15)                                      
081200                                 TO MOD-IDINLEV-NEXT                      
081300         MOVE MED-1 (SPRAAK-IX)  TO MOD-TEMFSINF                          
081400       WHEN OTHER                                                         
081500         MOVE ZERO               TO MOD-IDINLEV-NEXT                      
081600     END-EVALUATE                                                         
081700     .                                                                    
081800     EJECT                                                                
081900 D-BLANKA-RADER SECTION.                                                  
082000     PERFORM UNTIL INDX > MAX-ANT-RADER                                   
082100        MOVE MFS-RENSA-FAELT TO MOD-INFO-RAD (INDX)                       
082200                                                                          
082300        ADD +1 TO INDX                                                    
082400     END-PERFORM                                                          
082500     .                                                                    
082600     EJECT                                                                
082700 X-RENSA-BILD SECTION.                                                    
082800     SKIP2                                                                
082900     MOVE ZERO               TO MOD-IDINLEV-NEXT                          
083000                                MOD-TIREGDAT-NEXT                         
083100     MOVE +1 TO INDX                                                      
083200     PERFORM UNTIL INDX > MAX-ANT-RADER                                   
083300        MOVE MFS-RENSA-FAELT TO MOD-INFO-RAD (INDX)                       
083400                                                                          
083500        ADD +1 TO INDX                                                    
083600     END-PERFORM                                                          
083700     .                                                                    
083800     EJECT                                                                
083900 Y-KONV-AAVVD SECTION.                                                    
084000     SKIP2                                                                
084100     MOVE WS-TIUPPDAT        TO DAT-I-TIDATUM                             
084200     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
084300                                                                          
084400     CALL WDATKONV USING DAT-KDDATFORM                                    
084500                         DAT-I-TIDATUM                                    
084600                         DAT-O-TIDATUM                                    
084700                         DAT-KDSVAR                                       
084800     .                                                                    
084900     EJECT                                                                
085000 S01-KONV-IDINLEV SECTION.                                                
085100     SKIP2                                                                
085200     COMPUTE DAT-I-TIDATUM = 9999999 - SPLIT-TIAAMMDD                     
085300                                                                          
085400     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
085500                                                                          
085600     CALL WDATKONV USING DAT-KDDATFORM                                    
085700                         DAT-I-TIDATUM                                    
085800                         DAT-O-TIDATUM                                    
085900                         DAT-KDSVAR                                       
086000     .                                                                    
086100     EJECT                                                                
086200 S02-LAS-AK SECTION.                                                      
086300     SKIP2                                                                
086400     MOVE MFS-RENSA-FAELT TO MOD-KDAKSTAT(INDX)                           
086500     EJECT                                                                
086600*    I M S - SEKTIONER *                                                  
086700*                                                                         
086800     .                                                                    
086900 IMS-GET-MSG SECTION.                                                     
087000     MOVE '  QC' TO GODK-STATUSKODER                                      
087100     CALL CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                           
087200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087300     PERFORM IMS-STATUSKONTROLL                                           
087400     SKIP3                                                                
087500     .                                                                    
087600 IMS-INSERT-MSG SECTION.                                                  
087700                                                                          
087800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
087900       MOVE '0' TO MFS-KDHUVOMR                                           
088000     END-IF                                                               
088100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
088200     MOVE SPACE TO GODK-STATUSKODER                                       
088300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
088400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088500     PERFORM IMS-STATUSKONTROLL                                           
088600     .                                                                    
088700     EJECT                                                                
088800 IMS-GET-ART SECTION.                                                     
088900                                                                          
089000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
089100             DELIMITED BY SIZE INTO SSA1                                  
089200     MOVE '  GE' TO GODK-STATUSKODER                                      
089300     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA-2 SSA1                    
089400     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
089500     PERFORM IMS-STATUSKONTROLL                                           
089600     SKIP3                                                                
089700     .                                                                    
089800 IMS-GET-INLEV SECTION.                                                   
089900                                                                          
090000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
090100             DELIMITED BY SIZE INTO SSA1                                  
090200     STRING 'WLINLE11(DAINLEV =>' W-DAINLEV-X ')'                         
090300             DELIMITED BY SIZE INTO SSA2                                  
090400     MOVE '  GE' TO GODK-STATUSKODER                                      
090500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA-2 SSA1 SSA2              
090600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     .                                                                    
090900     EJECT                                                                
091000 IMS-GET-31-32-310 SECTION.                                               
091100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
091200             DELIMITED BY SIZE INTO SSA1                                  
091300     MOVE 'WLINLE21' TO SSA2                                              
091400     MOVE '  GE' TO GODK-STATUSKODER                                      
091500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA-2 SSA1 SSA2              
091600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
091700     PERFORM IMS-STATUSKONTROLL                                           
091800     SKIP3                                                                
091900     .                                                                    
092000 IMS-GET-33-34 SECTION.                                                   
092100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
092200             DELIMITED BY SIZE INTO SSA1                                  
092300     MOVE 'WLINLE22' TO SSA2                                              
092400     MOVE '  GE' TO GODK-STATUSKODER                                      
092500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA-2 SSA1 SSA2              
092600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
092700     PERFORM IMS-STATUSKONTROLL                                           
092800     SKIP3                                                                
092900     .                                                                    
093000 IMS-GET-40 SECTION.                                                      
093100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
093200             DELIMITED BY SIZE INTO SSA1                                  
093300     MOVE 'WLINLE23' TO SSA2                                              
093400     MOVE '  GE' TO GODK-STATUSKODER                                      
093500     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA-2 SSA1 SSA2              
093600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     EJECT                                                                
094000 IMS-GET-DEL SECTION.                                                     
094100     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
094200             DELIMITED BY SIZE INTO SSA1                                  
094300     MOVE 'WLINLE21' TO SSA2                                              
094400     MOVE 'WLINLE31' TO SSA3                                              
094500     MOVE '  GE' TO GODK-STATUSKODER                                      
094600     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA-2                        
094700          SSA1 SSA2 SSA3                                                  
094800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
094900     PERFORM IMS-STATUSKONTROLL                                           
095000     .                                                                    
095100     EJECT                                                                
095200 IMS-GET-WDL6-ART SECTION.                                                
095300                                                                          
095400     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
095500          DELIMITED BY SIZE INTO SSA1                                     
095600     MOVE '  GE'   TO GODK-STATUSKODER                                    
095700     CALL CBLTDLI  USING GU WDL6-PCB DLI-IO-WDL601 SSA1                   
095800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100     EJECT                                                                
096200 IMS-GET-WDL6-INL SECTION.                                                
096300                                                                          
096400     STRING 'WDL611  (DAINLEV =>' W-DAINLEV-X ')'                         
096500          DELIMITED BY SIZE INTO SSA1                                     
096600     MOVE '  GE'           TO GODK-STATUSKODER                            
096700     CALL CBLTDLI          USING GNP WDL6-PCB DLI-IO-WDL611 SSA1          
096800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     EJECT                                                                
097200 IMS-STATUSKONTROLL SECTION.                                              
097300     SET STATUS-IX TO 1                                                   
097400     SEARCH GODK-STATUS AT END CALL FELLOG                                
097500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
097600     END-SEARCH                                                           
097700     CONTINUE                                                             
097800     .                                                                    
097900     EJECT                                                                
098000*    -COPY WY2000P1                                                       
