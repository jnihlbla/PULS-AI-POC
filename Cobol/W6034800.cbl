000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6034800.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   97/07/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOPIERAR SEKTION OCH SKRIVER UPP TILL EN GÅNG AV                 
000900*        LIKADANA SEKTIONER. ANVÄNDS I LOCATION MANAGEMENT                
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLLOCA (WDJ8)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W6T348                                              
001500*        MID:         W6I34801                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W6O34801                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
002700                                                                          
002800 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
002900 77  WS-ADGANG                   PIC 9(2)   VALUE ZERO.                   
003000 77  WS-ADSEC11                  PIC 9(3)   VALUE ZERO.                   
003100                                                                          
003200 77  WS-ADLAGOMR-UPD             PIC 9(2)   VALUE ZERO.                   
003300 77  WS-ADGANG-UPD               PIC 9(2)   VALUE ZERO.                   
003400 77  WS-ADSEC11-FOM-UPD          PIC 9(3)   VALUE ZERO.                   
003500 77  WS-ADSEC11-TOM-UPD          PIC 9(3)   VALUE ZERO.                   
003600                                                                          
003700 77  WS-SECTION                  PIC 9(4)   VALUE ZERO COMP-3.            
003800 77  WS-SECTION-STEP             PIC 9      VALUE 1    COMP-3.            
003900 77  IX                          PIC 9(3)   VALUE 1    COMP-3.            
004000                                                                          
004100 77  ARE-THE-TABLE-FILLED        PIC X      VALUE 'N'.                    
004200     88  THE-TABLE-ARE-FILLED               VALUE 'J'.                    
004300     88  KEEP-UP-FILLING-THE-TABLE          VALUE 'N'.                    
004400                                                                          
004500 77  ARE-ALL-PLACES-UPDATED      PIC X      VALUE 'N'.                    
004600     88  ALL-PLACES-UPDATED                 VALUE 'J'.                    
004700     88  KEEP-ON-INSERTING                  VALUE 'N'.                    
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(08)   VALUE 'W6034800'.            
005100                                                                          
005200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005400                                                                          
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006500     88  NYCKLAR-OK                          VALUE 'J'.                   
006600     88  NYCKLAR-FEL                         VALUE 'N'.                   
006700                                                                          
006800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006900     88  EGEN-MID                            VALUE '6348'.                
007000     88  GODK-MID                            VALUE '6348'.                
007100     88  HELP-MID                            VALUE '0551'.                
007200                                                                          
007300 77  ALLA                        PIC X       VALUE 'A'.                   
007400 77  ODD                         PIC X       VALUE 'O'.                   
007500 77  EVEN                        PIC X       VALUE 'E'.                   
007600                                                                          
007700 01  TEMP-ADPLATS.                                                        
007800     03 TEMP-ADSEC11             PIC 9(3).                                
007900     03 TEMP-ADLEVEL11           PIC 9(1).                                
008000     03 TEMP-ADSEQ               PIC 9(1).                                
008100 01  MED1-ADPLATS.                                                        
008200     03 MED1-ADSEC11             PIC 9(3).                                
008300     03 MED1-ADLEVEL11           PIC 9(1).                                
008400     03 MED1-ADSEQ               PIC 9(1).                                
008500                                                                          
008600 01  TEMP-TABELL.                                                         
008700     03 ELEMENT OCCURS 150 TIMES.                                         
008800*05  -COPY WDJ801 -PRE TEMP-                                              
008900                                                                          
009000*      --- VALID IDDC CODES                                               
009100*                                                                         
009200*01    -COPY WWDC99                                                       
009300       EJECT                                                              
009400                                                                          
009500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009600 01  GENERELLA-SUBPROGRAM.                                                
009700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010300*01 -COPY WMEDAREA                                                        
010400     SKIP3                                                                
010500 01  MESSAGE-CODES.                                                       
010600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011100     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
011200     03  NO-ZEROES-ALLOWED       PIC X(3)    VALUE '724'.                 
011300     03  NO-GOOD-PLACE-FOUND     PIC X(3)    VALUE '729'.                 
011400                                                                          
011500     03 MED-1.                                                            
011600       05  FILLER                  PIC X(23)   VALUE                      
011700           'UPDATED UNTIL LOCATION '.                                     
011800       05  MED-IDDC                PIC X(02).                             
011900       05  FILLER                  PIC X(02)   VALUE SPACE.               
012000       05  MED-ADLAGOMR            PIC 9(02).                             
012100       05  FILLER                  PIC X(01)   VALUE SPACE.               
012200       05  MED-ADGANG              PIC 9(02).                             
012300       05  FILLER                  PIC X(01)   VALUE SPACE.               
012400       05  MED-ADSEC11             PIC 9(02).                             
012500       05  MED-ADLEVEL11           PIC 9(01).                             
012600       05  MED-ADSEQ               PIC 9(01).                             
012700       05  FILLER                  PIC X(01)   VALUE '.'.                 
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013200     SKIP3                                                                
013300*01 -COPY WMSGINIT                                                        
013400     EJECT                                                                
013500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013800     SKIP3                                                                
013900*01  MID -COPY W6I34801                                                   
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014200     SKIP3                                                                
014300*01  -COPY WMSGAREA                                                       
014400     EJECT                                                                
014500     03  MOD REDEFINES MSG-AREA.                                          
014600*      05  -COPY W6O34801                                                 
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014900     SKIP3                                                                
015000*01  -COPY WMFSAREA                                                       
015100     EJECT                                                                
015200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015500     SKIP3                                                                
015600 01  NYCKLAR-TILL-DLI.                                                    
015700     03  W-WDJ801KY-X.                                                    
015800         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
015900         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
016000         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
016100         05  W-LOC-ADPLATS.                                               
016200             07 W-LOC-ADSEC11    PIC 9(3).                                
016300             07 W-LOC-ADLEVEL11  PIC 9(1).                                
016400             07 W-LOC-ADSEQ      PIC 9(1).                                
016500     SKIP2                                                                
016600*    --- STATUS-KOD FRÅN IMS                                              
016700 01  STATUS-WS                   PIC XX.                                  
016800     88  SEGMENT-FINNS                       VALUE '  '.                  
016900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(64).                               
017700 01  SSA2                        PIC X(64).                               
017800     EJECT                                                                
017900*    --- IMS FUNKTIONSKODER                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300                                                                          
018400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOCA01'.                    
018500 01  DLI-IO-WLLOCA01.                                                     
018600*    03  -COPY WDJ801  -PRE LOCA-                                         
018700     EJECT                                                                
018800 LINKAGE SECTION.                                                         
018900*01  -COPY W0009   -PRE MSG-                                              
019000*01  -COPY W0008   -PRE USEA-                                             
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008  -PRE LOCA-                                              
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB LOCA-PCB.                     
019700 MAIN SECTION.                                                            
019800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB LOCA-PCB.                     
019900                                                                          
020000     PERFORM IMS-GET-MSG                                                  
020100     IF SEGMENT-FINNS                                                     
020200       PERFORM A-INIT                                                     
020300       PERFORM B-KOLLA-NYCKLAR                                            
020400       IF NYCKLAR-OK                                                      
020500         IF MFS-UPDATE                                                    
020600           PERFORM G-KOLLA-INPUT                                          
020700           IF INDATA-OK                                                   
020800             PERFORM H-UPPDATERA                                          
020900           END-IF                                                         
021000         ELSE                                                             
021100           IF MFS-FIRST                                                   
021200             PERFORM C-FOERSTA-SIDA                                       
021300           ELSE                                                           
021400             PERFORM E-SAMMA-SIDA                                         
021500           END-IF                                                         
021600         END-IF                                                           
021700       END-IF                                                             
021800       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34801 + 4                      
021900       PERFORM IMS-INSERT-MSG                                             
022000     END-IF                                                               
022100                                                                          
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT SECTION.                                                          
022700                                                                          
022800     IF MSG-DUBBLA-TRANSKODER                                             
022900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I34801                 
023000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
023100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
023200     ELSE                                                                 
023300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I34801                 
023400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
023500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
023600     END-IF                                                               
023700                                                                          
023800     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
023900     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
024000     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
024100                                                                          
024200     MOVE LOW-VALUE                       TO MSG-AREA                     
024300     MOVE 'W6O348N1'                      TO MFS-IDMOD                    
024400     MOVE '6348'                          TO MOD-IDTRANS                  
024500     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
024600                                             MOD-TEMFSINF                 
024700     IF EGEN-MID OR HELP-MID                                              
024800       CONTINUE                                                           
024900     ELSE                                                                 
025000       MOVE SPACE                         TO MFS-KDTRTYP                  
025100       MOVE '7'                           TO MFS-IDPFK                    
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 B-KOLLA-NYCKLAR SECTION.                                                 
025600                                                                          
025700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025800     MOVE '001'             TO MSGI-KDCALL                                
025900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026100     MOVE '6348'            TO MSGI-IDTRANS                               
026200                                                                          
026300     CALL W005INIT          USING MSGI-WMSGINIT USEA-PCB                  
026400                                                                          
026500     MOVE MSGI-IDDC         TO WS-IDDC                                    
026600                              MOD-IDDC                                    
026700     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
026800                                                                          
026900     MOVE JA                TO NYCKLAR-SW                                 
027000                                                                          
027100*                                                                         
027200*    -- CONTROL  ON WAREHOUSE                                             
027300*                                                                         
027400     IF CDC                                                               
027500       CONTINUE                                                           
027600     ELSE                                                                 
027700       MOVE NEJ TO NYCKLAR-SW                                             
027800     END-IF                                                               
027900*                                                                         
028000*    -- CONTROL  AREA                                                     
028100*                                                                         
028200     MOVE MFS-RENSA-FAELT           TO MOD-ADLAGOMR-IN                    
028300     IF MID-ADLAGOMR-IN = ALL '+'                                         
028400       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
028500       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
028600     ELSE                                                                 
028700       IF MID-ADLAGOMR-IN NUMERIC                                         
028800          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
028900       ELSE                                                               
029000          MOVE NEJ             TO NYCKLAR-SW                              
029100       END-IF                                                             
029200     END-IF                                                               
029300*                                                                         
029400*    -- CONTROL  AISLE FROM                                               
029500*                                                                         
029600     MOVE MFS-RENSA-FAELT      TO MOD-ADGANG-IN                           
029700     IF MID-ADGANG-IN = ALL '+'                                           
029800       INSPECT MID-ADGANG-UT REPLACING LEADING SPACE BY ZERO              
029900       MOVE MID-ADGANG-UT      TO WS-ADGANG                               
030000     ELSE                                                                 
030100       IF MID-ADGANG-IN NUMERIC                                           
030200          MOVE MID-ADGANG-IN   TO WS-ADGANG                               
030300       ELSE                                                               
030400          MOVE NEJ             TO NYCKLAR-SW                              
030500       END-IF                                                             
030600     END-IF                                                               
030700*                                                                         
030800*    -- CONTROL  SECTION                                                  
030900*                                                                         
031000     MOVE MFS-RENSA-FAELT      TO MOD-ADSEC11-IN                          
031100     IF MID-ADSEC11-IN = ALL '+'                                          
031200       INSPECT MID-ADSEC11-UT REPLACING LEADING SPACE BY ZERO             
031300       MOVE MID-ADSEC11-UT     TO WS-ADSEC11                              
031400     ELSE                                                                 
031500       IF MID-ADSEC11-IN NUMERIC                                          
031600          MOVE MID-ADSEC11-IN  TO WS-ADSEC11                              
031700       ELSE                                                               
031800          MOVE NEJ             TO NYCKLAR-SW                              
031900       END-IF                                                             
032000     END-IF                                                               
032100                                                                          
032200*                                                                         
032300*   KONTROLL FÖR ATT HITTA OM EN ANGIVEN                                  
032400*   SEKTION FINNS ÖVERHUVUDTAGET PÅ BASEN                                 
032500*                                                                         
032600     IF NYCKLAR-OK                                                        
032700       MOVE WS-ADLAGOMR  TO  MOD-ADLAGOMR-UT                              
032800       MOVE WS-ADGANG    TO  MOD-ADGANG-UT                                
032900       MOVE WS-ADSEC11   TO  MOD-ADSEC11-UT                               
033000                                                                          
033100       PERFORM MFS-RENSA-FAELT-IN                                         
033200                                                                          
033300       MOVE WS-IDDC      TO  W-LOC-IDDC                                   
033400       MOVE WS-ADLAGOMR  TO  W-LOC-ADLAGOMR                               
033500       MOVE WS-ADGANG    TO  W-LOC-ADGANG                                 
033600       MOVE WS-ADSEC11   TO  W-LOC-ADSEC11                                
033700       MOVE 0            TO  W-LOC-ADLEVEL11                              
033800       MOVE 0            TO  W-LOC-ADSEQ                                  
033900                                                                          
034000       PERFORM IMS-GET-LOCA-LOC                                           
034100                                                                          
034200       IF SEGMENT-FINNS                                                   
034300          MOVE LOCA-LOC-ADPLATS TO TEMP-ADPLATS                           
034400          IF LOCA-LOC-ADLAGOMR = WS-ADLAGOMR AND                          
034500             LOCA-LOC-ADGANG   = WS-ADGANG   AND                          
034600             TEMP-ADSEC11      = WS-ADSEC11                               
034700             CONTINUE                                                     
034800          ELSE                                                            
034900             MOVE NEJ                 TO NYCKLAR-SW                       
035000             MOVE NO-GOOD-PLACE-FOUND TO MED-IDMFSINF                     
035100          END-IF                                                          
035200       END-IF                                                             
035300     END-IF                                                               
035400     IF NYCKLAR-FEL                                                       
035500       MOVE ERR-WRONG-KEY TO    MED-IDMFSFEL                              
035600       CALL WMEDKONV      USING MED-WMEDAREA                              
035700       MOVE MED-MFSFEL    TO    MOD-TEMFSFEL                              
035800       MOVE MED-MFSINF    TO    MOD-TEMFSINF                              
035900       PERFORM MFS-RENSA-FAELT-IN                                         
036000       PERFORM MFS-RENSA-FAELT-UT                                         
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 C-FOERSTA-SIDA SECTION.                                                  
036500                                                                          
036600     PERFORM MFS-RENSA-FAELT-IN                                           
036700     PERFORM MFS-RENSA-FAELT-UT                                           
036800     .                                                                    
036900     EJECT                                                                
037000 E-SAMMA-SIDA SECTION.                                                    
037100                                                                          
037200     IF EGEN-MID OR HELP-MID                                              
037300       MOVE INF-PRESS-PF11        TO MED-IDMFSINF                         
037400       CALL WMEDKONV              USING MED-WMEDAREA                      
037500       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
037600       PERFORM EA-MID-INDATA-TILL-MOD                                     
037700     ELSE                                                                 
037800       PERFORM MFS-RENSA-FAELT-IN                                         
037900       PERFORM MFS-RENSA-FAELT-UT                                         
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 EA-MID-INDATA-TILL-MOD SECTION.                                          
038400                                                                          
038500*    IF MID-ADLAGOMR-IN             NOT = ALL '+'                         
038600*       MOVE MID-ADLAGOMR-IN        TO MOD-ADLAGOMR-IN                    
038700*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADLAGOMR-IN-ATTR               
038800*    ELSE                                                                 
038900*       MOVE MFS-RENSA-FAELT        TO MOD-ADLAGOMR-IN                    
039000*    END-IF                                                               
039100*                                                                         
039200*    IF MID-ADGANG-IN               NOT = ALL '+'                         
039300*       MOVE MID-ADGANG-IN          TO MOD-ADGANG-IN                      
039400*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADGANG-IN-ATTR                 
039500*    ELSE                                                                 
039600*       MOVE MFS-RENSA-FAELT        TO MOD-ADGANG-IN                      
039700*    END-IF                                                               
039800*                                                                         
039900*    IF MID-ADSEC11-IN              NOT = ALL '+'                         
040000*       MOVE MID-ADSEC11-IN         TO MOD-ADSEC11-IN                     
040100*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADSEC11-IN-ATTR                
040200*    ELSE                                                                 
040300*       MOVE MFS-RENSA-FAELT        TO MOD-ADSEC11-IN                     
040400*    END-IF                                                               
040500                                                                          
040600     IF MID-ADLAGOMR NOT = ALL '+' AND MID-ADLAGOMR NUMERIC               
040700        MOVE MID-ADLAGOMR           TO MOD-ADLAGOMR-TO-IN                 
040800        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADLAGOMR-TO-ATTR               
040900     ELSE                                                                 
041000        MOVE MFS-RENSA-FAELT        TO MOD-ADLAGOMR-TO-IN                 
041100     END-IF                                                               
041200                                                                          
041300     IF MID-ADGANG NOT = ALL '+' AND MID-ADGANG NUMERIC                   
041400        MOVE MID-ADGANG             TO MOD-ADGANG-TO-IN                   
041500        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADGANG-TO-ATTR                 
041600     ELSE                                                                 
041700        MOVE MFS-RENSA-FAELT        TO MOD-ADGANG-TO-IN                   
041800     END-IF                                                               
041900                                                                          
042000     IF MID-ADSEC11-FOM NOT = ALL '+' AND MID-ADSEC11-FOM NUMERIC         
042100        MOVE MID-ADSEC11-FOM        TO MOD-ADSEC11-FOM-IN                 
042200        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADSEC11-FOM-ATTR               
042300     ELSE                                                                 
042400        MOVE MFS-RENSA-FAELT        TO MOD-ADSEC11-FOM-IN                 
042500     END-IF                                                               
042600                                                                          
042700     IF MID-ADSEC11-TOM NOT = ALL '+' AND MID-ADSEC11-TOM NUMERIC         
042800        MOVE MID-ADSEC11-TOM        TO MOD-ADSEC11-TOM-IN                 
042900        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADSEC11-TOM-ATTR               
043000     ELSE                                                                 
043100        MOVE MFS-RENSA-FAELT        TO MOD-ADSEC11-TOM-IN                 
043200     END-IF                                                               
043300                                                                          
043400     IF MID-KDAOE = ALLA OR ODD OR EVEN                                   
043500        MOVE MID-KDAOE              TO MOD-KDAOE-IN                       
043600        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDAOE-ATTR                     
043700     ELSE                                                                 
043800        MOVE MFS-RENSA-FAELT        TO MOD-KDAOE-IN                       
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 G-KOLLA-INPUT SECTION.                                                   
044300                                                                          
044400     MOVE JA  TO INDATA-SW                                                
044500     IF MID-ADLAGOMR  = ALL '+'  AND                                      
044600        MID-ADGANG    = ALL '+'  AND                                      
044700        MID-ADSEC11-FOM = ALL '+' AND                                     
044800        MID-ADSEC11-TOM = ALL '+' AND                                     
044900        MID-KDAOE     = ALL '+'                                           
045000                                                                          
045100       MOVE ERR-PF11-AND-NO-DATA TO    MED-IDMFSFEL                       
045200       CALL WMEDKONV             USING MED-WMEDAREA                       
045300       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
045400       MOVE NEJ                  TO INDATA-SW                             
045500     ELSE                                                                 
045600                                                                          
045700       IF MID-ADLAGOMR = ALL '+' OR MID-ADLAGOMR NOT NUMERIC              
045800         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADLAGOMR-TO-ATTR               
045900         MOVE NEJ                   TO INDATA-SW                          
046000       ELSE                                                               
046100*        IF MID-ADLAGOMR = ZERO                                           
046200*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADLAGOMR-TO-ATTR               
046300*          MOVE NEJ                 TO INDATA-SW                          
046400*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
046500*        ELSE                                                             
046600           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADLAGOMR-TO-ATTR               
046700*        END-IF                                                           
046800       END-IF                                                             
046900                                                                          
047000       IF MID-ADGANG = ALL '+' OR MID-ADGANG NOT NUMERIC                  
047100         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADGANG-TO-ATTR                 
047200         MOVE NEJ                   TO INDATA-SW                          
047300       ELSE                                                               
047400*        IF MID-ADGANG = ZERO                                             
047500*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADGANG-TO-ATTR                 
047600*          MOVE NEJ                 TO INDATA-SW                          
047700*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
047800*        ELSE                                                             
047900           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADGANG-TO-ATTR                 
048000*        END-IF                                                           
048100       END-IF                                                             
048200                                                                          
048300       IF MID-ADSEC11-FOM = ALL '+' OR MID-ADSEC11-FOM NOT NUMERIC        
048400         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADSEC11-FOM-ATTR               
048500         MOVE NEJ                   TO INDATA-SW                          
048600       ELSE                                                               
048700*        IF MID-ADSEC11-FOM = ZERO                                        
048800*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-FOM-ATTR               
048900*          MOVE NEJ                 TO INDATA-SW                          
049000*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
049100*        ELSE                                                             
049200           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADSEC11-FOM-ATTR               
049300*        END-IF                                                           
049400       END-IF                                                             
049500                                                                          
049600       IF MID-ADSEC11-TOM = ALL '+' OR MID-ADSEC11-TOM NOT NUMERIC        
049700         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADSEC11-TOM-ATTR               
049800         MOVE NEJ                   TO INDATA-SW                          
049900       ELSE                                                               
050000*        IF MID-ADSEC11-TOM = ZERO                                        
050100*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-TOM-ATTR               
050200*          MOVE NEJ                 TO INDATA-SW                          
050300*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
050400*        ELSE                                                             
050500           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADSEC11-TOM-ATTR               
050600*        END-IF                                                           
050700       END-IF                                                             
050800                                                                          
050900       IF MID-KDAOE = ALLA OR ODD OR EVEN                                 
051000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAOE-ATTR                      
051100       ELSE                                                               
051200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDAOE-ATTR                      
051300         MOVE NEJ                  TO INDATA-SW                           
051400       END-IF                                                             
051500                                                                          
051600       IF INDATA-OK                                                       
051700         IF MID-ADSEC11-FOM > MID-ADSEC11-TOM                             
051800           MOVE WRONG-INTERVAL-INFO TO MED-IDMFSINF                       
051900           MOVE NEJ                 TO INDATA-SW                          
052000           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-TOM-ATTR               
052100           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-FOM-ATTR               
052200         ELSE                                                             
052300*  KONTROLL FÖR ATT KOLLA A/O/E GENTEMOT ANGIVNA INTERVALLER              
052400           EVALUATE MID-KDAOE                                             
052500           WHEN ALLA                                                      
052600             MOVE 1                     TO WS-SECTION-STEP                
052700           WHEN EVEN                                                      
052800*  FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT        
052900             IF FUNCTION MOD (MID-ADSEC11-FOM 2) = 0 AND                  
053000                FUNCTION MOD (MID-ADSEC11-TOM 2) = 0                      
053100                MOVE 2                  TO WS-SECTION-STEP                
053200             ELSE                                                         
053300               MOVE WRONG-INTERVAL-INFO TO MED-IDMFSINF                   
053400               MOVE NEJ                 TO INDATA-SW                      
053500               MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDAOE-ATTR                 
053600               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-FOM-ATTR           
053700               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-TOM-ATTR           
053800             END-IF                                                       
053900           WHEN ODD                                                       
054000*  FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT        
054100             IF FUNCTION MOD (MID-ADSEC11-FOM 2) = 1 AND                  
054200                FUNCTION MOD (MID-ADSEC11-TOM 2) = 1                      
054300                MOVE 2 TO WS-SECTION-STEP                                 
054400             ELSE                                                         
054500               MOVE WRONG-INTERVAL-INFO TO MED-IDMFSINF                   
054600               MOVE NEJ                 TO INDATA-SW                      
054700               MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDAOE-ATTR                 
054800               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-FOM-ATTR           
054900               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC11-TOM-ATTR           
055000             END-IF                                                       
055100           END-EVALUATE                                                   
055200         END-IF                                                           
055300       END-IF                                                             
055400                                                                          
055500         MOVE MFS-ADD-SET-CURSOR   TO MOD-ADLAGOMR-TO-ATTR                
055600       IF INDATA-FEL                                                      
055700         MOVE ERR-CORR-HILITE-FLDS TO    MED-IDMFSFEL                     
055800         CALL WMEDKONV             USING MED-WMEDAREA                     
055900         MOVE MED-MFSFEL           TO    MOD-TEMFSFEL                     
056000         MOVE MED-MFSINF           TO    MOD-TEMFSINF                     
056100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
056200       END-IF                                                             
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600                                                                          
056700 H-UPPDATERA SECTION.                                                     
056800     PERFORM HA-INIT                                                      
056900     PERFORM HB-UPDATE                                                    
057000     PERFORM HC-CLOSE                                                     
057100     .                                                                    
057200     EJECT                                                                
057300 HA-INIT SECTION.                                                         
057400     MOVE MID-ADLAGOMR  TO  WS-ADLAGOMR-UPD   MOD-ADLAGOMR-TO-UT          
057500     MOVE MID-ADGANG    TO  WS-ADGANG-UPD     MOD-ADGANG-TO-UT            
057600     MOVE MID-ADSEC11-FOM TO WS-ADSEC11-FOM-UPD MOD-ADSEC11-FOM-UT        
057700                            WS-SECTION                                    
057800     MOVE MID-ADSEC11-TOM TO WS-ADSEC11-TOM-UPD MOD-ADSEC11-TOM-UT        
057900     MOVE MID-KDAOE     TO                    MOD-KDAOE-UT                
058000     .                                                                    
058100     EJECT                                                                
058200 HB-UPDATE SECTION.                                                       
058300* FYLLER TABELLEN AV INFORMATION OM HELA SEKTIONEN, PLATS FÖR             
058400* PLATS, VARJE INDEX MOTSVARAR EN LAGERPLATS.                             
058500     INITIALIZE TEMP-TABELL                                               
058600     MOVE NEJ             TO ARE-THE-TABLE-FILLED                         
058700     MOVE 1               TO IX                                           
058800     MOVE LOCA-LOC-WDJ801 TO TEMP-LOC-WDJ801(IX)                          
058900     ADD 1                TO IX                                           
059000     PERFORM UNTIL THE-TABLE-ARE-FILLED                                   
059100       PERFORM IMS-GET-NEXT-LOC                                           
059200       IF SEGMENT-FINNS                                                   
059300         MOVE LOCA-LOC-ADPLATS  TO TEMP-ADPLATS                           
059400         IF TEMP-ADSEC11      = WS-ADSEC11          AND                   
059500            LOCA-LOC-ADGANG   = WS-ADGANG           AND                   
059600            LOCA-LOC-ADLAGOMR = WS-ADLAGOMR                               
059700                                                                          
059800           MOVE LOCA-LOC-WDJ801 TO TEMP-LOC-WDJ801(IX)                    
059900           ADD 1                TO IX                                     
060000         ELSE                                                             
060100           MOVE JA              TO ARE-THE-TABLE-FILLED                   
060200         END-IF                                                           
060300       ELSE                                                               
060400         MOVE JA                TO ARE-THE-TABLE-FILLED                   
060500       END-IF                                                             
060600     END-PERFORM                                                          
060700                                                                          
060800* SKAPAR DE NYA SEKTIONERNA                                               
060900*                                                                         
061000     PERFORM UNTIL WS-SECTION > WS-ADSEC11-TOM-UPD    OR                  
061100                   SEGMENT-FINNS-REDAN                                    
061200     MOVE 1   TO IX                                                       
061300     MOVE NEJ TO ARE-ALL-PLACES-UPDATED                                   
061400       PERFORM UNTIL ALL-PLACES-UPDATED OR SEGMENT-FINNS-REDAN            
061500         MOVE TEMP-LOC-WDJ801(IX)  TO LOCA-LOC-WDJ801                     
061600         MOVE WS-ADLAGOMR-UPD      TO LOCA-LOC-ADLAGOMR                   
061700         MOVE WS-ADGANG-UPD        TO LOCA-LOC-ADGANG                     
061800         MOVE TEMP-LOC-ADPLATS(IX) TO TEMP-ADPLATS                        
061900         MOVE WS-SECTION           TO TEMP-ADSEC11                        
062000         MOVE TEMP-ADPLATS         TO LOCA-LOC-ADPLATS                    
062100         MOVE ZERO                 TO LOCA-LOC-KVPLATS                    
062200                                                                          
062300         PERFORM IMS-ISRT-LOCA-LOC                                        
062400         IF SEGMENT-FINNS                                                 
062500           ADD 1           TO IX                                          
062600           IF TEMP-LOC-ADLAGOMR(IX) > 0                                   
062700             CONTINUE                                                     
062800           ELSE                                                           
062900             MOVE JA       TO ARE-ALL-PLACES-UPDATED                      
063000           END-IF                                                         
063100         ELSE                                                             
063200           MOVE JA         TO ARE-ALL-PLACES-UPDATED                      
063300         END-IF                                                           
063400       END-PERFORM                                                        
063500       ADD WS-SECTION-STEP TO WS-SECTION                                  
063600     END-PERFORM                                                          
063700     .                                                                    
063800     EJECT                                                                
063900 HC-CLOSE SECTION.                                                        
064000     IF SEGMENT-FINNS-REDAN                                               
064100        MOVE LOCA-LOC-IDDC     TO MED-IDDC                                
064200        MOVE WS-ADLAGOMR-UPD   TO MED-ADLAGOMR                            
064300        MOVE WS-ADGANG-UPD     TO MED-ADGANG                              
064400*       MOVE TEMP-ADSEC11      TO MED-ADSEC11                             
064500*       MOVE TEMP-ADLEVEL11    TO MED-ADLEVEL11                           
064600*       MOVE TEMP-ADSEQ        TO MED-ADSEQ                               
064700        MOVE LOCA-LOC-ADPLATS  TO MED1-ADPLATS                            
064800        MOVE MED1-ADSEC11      TO MED-ADSEC11                             
064900        MOVE MED1-ADLEVEL11    TO MED-ADLEVEL11                           
065000        MOVE MED1-ADSEQ        TO MED-ADSEQ                               
065100                                                                          
065200        MOVE MED-1             TO MOD-TEMFSFEL                            
065300     END-IF                                                               
065400                                                                          
065500     MOVE INF-UPDATE-DONE      TO    MED-IDMFSINF                         
065600     CALL WMEDKONV             USING MED-WMEDAREA                         
065700     MOVE MED-MFSINF           TO    MOD-TEMFSINF                         
065800     .                                                                    
065900     EJECT                                                                
066000 MFS-RENSA-FAELT-IN SECTION.                                              
066100                                                                          
066200     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-IN                              
066300                                MOD-ADGANG-IN                             
066400                                MOD-ADSEC11-IN                            
066500     .                                                                    
066600     EJECT                                                                
066700 MFS-RENSA-FAELT-UT SECTION.                                              
066800                                                                          
066900     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-TO-UT                           
067000                                MOD-ADLAGOMR-TO-UT                        
067100                                MOD-ADGANG-TO-UT                          
067200                                MOD-ADSEC11-FOM-UT                        
067300                                MOD-ADSEC11-TOM-UT                        
067400                                MOD-KDAOE-UT                              
067500                                MOD-ADLAGOMR-UT                           
067600                                MOD-ADGANG-UT                             
067700                                MOD-ADSEC11-UT                            
067800     .                                                                    
067900     EJECT                                                                
068000 MFS-ROER-EJ-FAELT-IN SECTION.                                            
068100                                                                          
068200*    --- ALLA INDATA-FÄLT                                                 
068300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC                                   
068400                                MOD-ADLAGOMR-IN                           
068500                                MOD-ADGANG-IN                             
068600                                MOD-ADSEC11-IN                            
068700                                MOD-ADLAGOMR-TO-IN                        
068800                                MOD-ADGANG-TO-IN                          
068900                                MOD-ADSEC11-FOM-IN                        
069000                                MOD-ADSEC11-TOM-IN                        
069100                                MOD-KDAOE-IN                              
069200     .                                                                    
069300     EJECT                                                                
069400* --- IMS SEKTIONER ---                                                   
069500     SKIP3                                                                
069600 IMS-GET-MSG SECTION.                                                     
069700                                                                          
069800     MOVE '  QC' TO GODK-STATUSKODER                                      
069900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
070000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070100     PERFORM IMS-STATUSKONTROLL                                           
070200     .                                                                    
070300     SKIP3                                                                
070400 IMS-INSERT-MSG SECTION.                                                  
070500                                                                          
070600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070700     MOVE SPACE TO GODK-STATUSKODER                                       
070800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     EJECT                                                                
071300 IMS-GET-LOCA-LOC SECTION.                                                
071400                                                                          
071500     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ801KY-X ')'                        
071600          DELIMITED BY SIZE INTO SSA1                                     
071700     MOVE '  GE' TO GODK-STATUSKODER                                      
071800     CALL CBLTDLI USING GU LOCA-PCB DLI-IO-WLLOCA01 SSA1                  
071900     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     SKIP3                                                                
072300 IMS-GET-NEXT-LOC SECTION.                                                
072400                                                                          
072500     MOVE 'WLLOCA01 ' TO SSA1                                             
072600     MOVE '  GB' TO GODK-STATUSKODER                                      
072700     CALL CBLTDLI USING GN LOCA-PCB DLI-IO-WLLOCA01 SSA1                  
072800     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
072900     PERFORM IMS-STATUSKONTROLL                                           
073000     .                                                                    
073100     SKIP3                                                                
073200 IMS-ISRT-LOCA-LOC SECTION.                                               
073300                                                                          
073400     MOVE 'WLLOCA01 ' TO SSA1                                             
073500     MOVE '  II' TO GODK-STATUSKODER                                      
073600     CALL CBLTDLI USING ISRT LOCA-PCB DLI-IO-WLLOCA01 SSA1                
073700     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
073800     PERFORM IMS-STATUSKONTROLL                                           
073900     .                                                                    
074000     EJECT                                                                
074100 IMS-STATUSKONTROLL SECTION.                                              
074200                                                                          
074300     SET STATUS-IX TO 1                                                   
074400     SEARCH GODK-STATUS                                                   
074500       AT END                                                             
074600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
074700         DELIMITED BY SIZE INTO FELTEXT                                   
074800         CALL FELLOG                                                      
074900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075000         CONTINUE                                                         
075100     END-SEARCH                                                           
075200     .                                                                    
