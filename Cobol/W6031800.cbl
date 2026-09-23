000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6031800.                                                
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
001400*        TRANSAKTION: W6T318                                              
001500*        MID:         W6I31801                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W6O31801                                            
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
003000 77  WS-ADSEC                    PIC 9(2)   VALUE ZERO.                   
003100                                                                          
003200 77  WS-ADLAGOMR-UPD             PIC 9(2)   VALUE ZERO.                   
003300 77  WS-ADGANG-UPD               PIC 9(2)   VALUE ZERO.                   
003400 77  WS-ADSEC-FOM-UPD            PIC 9(2)   VALUE ZERO.                   
003500 77  WS-ADSEC-TOM-UPD            PIC 9(2)   VALUE ZERO.                   
003600                                                                          
003700 77  WS-SECTION                  PIC 9(2)   VALUE ZERO COMP-3.            
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
005000 77  IDPGM                       PIC X(08)   VALUE 'W6031800'.            
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
006900     88  EGEN-MID                            VALUE '6318'.                
007000     88  GODK-MID                            VALUE '6318'.                
007100     88  HELP-MID                            VALUE '0551'.                
007200                                                                          
007300 77  ALLA                        PIC X       VALUE 'A'.                   
007400 77  ODD                         PIC X       VALUE 'O'.                   
007500 77  EVEN                        PIC X       VALUE 'E'.                   
007600                                                                          
007700 01  TEMP-ADPLATS.                                                        
007800     03 TEMP-ADSEC               PIC 9(2).                                
007900     03 TEMP-ADLEVEL             PIC 9(2).                                
008000     03 TEMP-ADSEQ               PIC 9(1).                                
008100 01  MED1-ADPLATS.                                                        
008200     03 MED1-ADSEC               PIC 9(2).                                
008300     03 MED1-ADLEVEL             PIC 9(2).                                
008400     03 MED1-ADSEQ               PIC 9(1).                                
008500                                                                          
008600 01  TEMP-TABELL.                                                         
008700     03 ELEMENT OCCURS 150 TIMES.                                         
008800*05  -COPY WDJ801 -PRE TEMP-                                              
008900                                                                          
009000       EJECT                                                              
009100                                                                          
009200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009300 01  GENERELLA-SUBPROGRAM.                                                
009400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010000*01 -COPY WMEDAREA                                                        
010100     SKIP3                                                                
010200 01  MESSAGE-CODES.                                                       
010300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010800     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
010900     03  NO-ZEROES-ALLOWED       PIC X(3)    VALUE '724'.                 
011000     03  NO-GOOD-PLACE-FOUND     PIC X(3)    VALUE '729'.                 
011100                                                                          
011200     03 MED-1.                                                            
011300       05  FILLER                  PIC X(23)   VALUE                      
011400           'UPDATED UNTIL LOCATION '.                                     
011500       05  MED-IDDC                PIC X(02).                             
011600       05  FILLER                  PIC X(02)   VALUE SPACE.               
011700       05  MED-ADLAGOMR            PIC 9(02).                             
011800       05  FILLER                  PIC X(01)   VALUE SPACE.               
011900       05  MED-ADGANG              PIC 9(02).                             
012000       05  FILLER                  PIC X(01)   VALUE SPACE.               
012100       05  MED-ADSEC               PIC 9(02).                             
012200       05  MED-ADLEVEL             PIC 9(02).                             
012300       05  MED-ADSEQ               PIC 9(01).                             
012400       05  FILLER                  PIC X(01)   VALUE '.'.                 
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012900     SKIP3                                                                
013000*01 -COPY WMSGINIT                                                        
013100     EJECT                                                                
013200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013300*                                                                         
013400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013500     SKIP3                                                                
013600*01  MID -COPY W6I31801                                                   
013700     EJECT                                                                
013800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013900     SKIP3                                                                
014000*01  -COPY WMSGAREA                                                       
014100     EJECT                                                                
014200     03  MOD REDEFINES MSG-AREA.                                          
014300*      05  -COPY W6O31801                                                 
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014600     SKIP3                                                                
014700*01  -COPY WMFSAREA                                                       
014800     EJECT                                                                
014900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015000*                                                                         
015100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015200     SKIP3                                                                
015300 01  NYCKLAR-TILL-DLI.                                                    
015400     03  W-WDJ801KY-X.                                                    
015500         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
015600         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
015700         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
015800         05  W-LOC-ADPLATS.                                               
015900             07 W-LOC-ADSEC      PIC 9(2).                                
016000             07 W-LOC-ADLEVEL    PIC 9(2).                                
016100             07 W-LOC-ADSEQ      PIC 9(1).                                
016200                                                                          
016300     03  W-IDDC-B6-X.                                                     
016400         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
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
018800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018900 01   DLI-IO-AREA-B601.                                                   
019000*     03  -COPY WDB601                                                    
019100     EJECT                                                                
019200 LINKAGE SECTION.                                                         
019300*01  -COPY W0009   -PRE MSG-                                              
019400*01  -COPY W0008   -PRE USEA-                                             
019500     05  FILLER                  PIC X.                                   
019600     EJECT                                                                
019700*01  -COPY W0008  -PRE LOCA-                                              
019800     05  FILLER                  PIC X.                                   
019900     EJECT                                                                
020000*01  -COPY W0008      -PRE WDB6-                                          
020100     05  FILLER                  PIC X.                                   
020200     EJECT                                                                
020300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB LOCA-PCB                      
020400                           WDB6-PCB.                                      
020500 MAIN SECTION.                                                            
020600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB LOCA-PCB                      
020700                           WDB6-PCB.                                      
020800                                                                          
020900     PERFORM IMS-GET-MSG                                                  
021000     IF SEGMENT-FINNS                                                     
021100       PERFORM A-INIT                                                     
021200       PERFORM B-KOLLA-NYCKLAR                                            
021300       IF NYCKLAR-OK                                                      
021400         IF MFS-UPDATE                                                    
021500           PERFORM G-KOLLA-INPUT                                          
021600           IF INDATA-OK                                                   
021700             PERFORM H-UPPDATERA                                          
021800           END-IF                                                         
021900         ELSE                                                             
022000           IF MFS-FIRST                                                   
022100             PERFORM C-FOERSTA-SIDA                                       
022200           ELSE                                                           
022300             PERFORM E-SAMMA-SIDA                                         
022400           END-IF                                                         
022500         END-IF                                                           
022600       END-IF                                                             
022700       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31801 + 4                      
022800       PERFORM IMS-INSERT-MSG                                             
022900     END-IF                                                               
023000                                                                          
023100     MOVE ZERO TO RETURN-CODE                                             
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600                                                                          
023700     IF MSG-DUBBLA-TRANSKODER                                             
023800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I31801                 
023900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
024000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
024100     ELSE                                                                 
024200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I31801                 
024300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
024400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
024500     END-IF                                                               
024600                                                                          
024700     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
024800     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
024900     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
025000                                                                          
025100     MOVE LOW-VALUE                       TO MSG-AREA                     
025200     MOVE 'W6O318N1'                      TO MFS-IDMOD                    
025300     MOVE '6318'                          TO MOD-IDTRANS                  
025400     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
025500                                             MOD-TEMFSINF                 
025600     IF EGEN-MID OR HELP-MID                                              
025700       CONTINUE                                                           
025800     ELSE                                                                 
025900       MOVE SPACE                         TO MFS-KDTRTYP                  
026000       MOVE '7'                           TO MFS-IDPFK                    
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 B-KOLLA-NYCKLAR SECTION.                                                 
026500                                                                          
026600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026700     MOVE '001'             TO MSGI-KDCALL                                
026800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027000     MOVE '6318'            TO MSGI-IDTRANS                               
027100                                                                          
027200     CALL W005INIT          USING MSGI-WMSGINIT USEA-PCB                  
027300                                                                          
027400     MOVE MSGI-IDDC         TO MOD-IDDC                                   
027500     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
027600                                                                          
027700     MOVE JA                TO NYCKLAR-SW                                 
027800                                                                          
027900*                                                                         
028000*    -- CONTROL  ON WAREHOUSE                                             
028100*                                                                         
028200     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
028300     PERFORM IMS-GU-WDB601                                                
028400     IF SEGMENT-FINNS                                                     
028500     AND (DCS-SDC                                                         
028600     OR   DCS-NDC-NA                                                      
028700     OR   DCS-NDC-PF                                                      
028800     OR   DCS-NDC-OTHERS)                                                 
028900       CONTINUE                                                           
029000     ELSE                                                                 
029100       MOVE NEJ TO NYCKLAR-SW                                             
029200     END-IF                                                               
029300*                                                                         
029400*    -- CONTROL  AREA                                                     
029500*                                                                         
029600     MOVE MFS-RENSA-FAELT           TO MOD-ADLAGOMR-IN                    
029700     IF MID-ADLAGOMR-IN = ALL '+'                                         
029800       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
029900       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
030000     ELSE                                                                 
030100       IF MID-ADLAGOMR-IN NUMERIC                                         
030200          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
030300       ELSE                                                               
030400          MOVE NEJ             TO NYCKLAR-SW                              
030500       END-IF                                                             
030600     END-IF                                                               
030700*                                                                         
030800*    -- CONTROL  AISLE FROM                                               
030900*                                                                         
031000     MOVE MFS-RENSA-FAELT      TO MOD-ADGANG-IN                           
031100     IF MID-ADGANG-IN = ALL '+'                                           
031200       INSPECT MID-ADGANG-UT REPLACING LEADING SPACE BY ZERO              
031300       MOVE MID-ADGANG-UT      TO WS-ADGANG                               
031400     ELSE                                                                 
031500       IF MID-ADGANG-IN NUMERIC                                           
031600          MOVE MID-ADGANG-IN   TO WS-ADGANG                               
031700       ELSE                                                               
031800          MOVE NEJ             TO NYCKLAR-SW                              
031900       END-IF                                                             
032000     END-IF                                                               
032100*                                                                         
032200*    -- CONTROL  SECTION                                                  
032300*                                                                         
032400     MOVE MFS-RENSA-FAELT      TO MOD-ADSEC-IN                            
032500     IF MID-ADSEC-IN = ALL '+'                                            
032600       INSPECT MID-ADSEC-UT REPLACING LEADING SPACE BY ZERO               
032700       MOVE MID-ADSEC-UT       TO WS-ADSEC                                
032800     ELSE                                                                 
032900       IF MID-ADSEC-IN NUMERIC                                            
033000          MOVE MID-ADSEC-IN    TO WS-ADSEC                                
033100       ELSE                                                               
033200          MOVE NEJ             TO NYCKLAR-SW                              
033300       END-IF                                                             
033400     END-IF                                                               
033500                                                                          
033600*                                                                         
033700*   KONTROLL FÖR ATT HITTA OM EN ANGIVEN                                  
033800*   SEKTION FINNS ÖVERHUVUDTAGET PÅ BASEN                                 
033900*                                                                         
034000     IF NYCKLAR-OK                                                        
034100       MOVE WS-ADLAGOMR  TO  MOD-ADLAGOMR-UT                              
034200       MOVE WS-ADGANG    TO  MOD-ADGANG-UT                                
034300       MOVE WS-ADSEC     TO  MOD-ADSEC-UT                                 
034400                                                                          
034500       PERFORM MFS-RENSA-FAELT-IN                                         
034600                                                                          
034700       MOVE W-IDDC-B6    TO  W-LOC-IDDC                                   
034800       MOVE WS-ADLAGOMR  TO  W-LOC-ADLAGOMR                               
034900       MOVE WS-ADGANG    TO  W-LOC-ADGANG                                 
035000       MOVE WS-ADSEC     TO  W-LOC-ADSEC                                  
035100       MOVE 0            TO  W-LOC-ADLEVEL                                
035200       MOVE 0            TO  W-LOC-ADSEQ                                  
035300                                                                          
035400       PERFORM IMS-GET-LOCA-LOC                                           
035500                                                                          
035600       IF SEGMENT-FINNS                                                   
035700          MOVE LOCA-LOC-ADPLATS TO TEMP-ADPLATS                           
035800          IF LOCA-LOC-ADLAGOMR = WS-ADLAGOMR AND                          
035900             LOCA-LOC-ADGANG   = WS-ADGANG   AND                          
036000             TEMP-ADSEC        = WS-ADSEC                                 
036100             CONTINUE                                                     
036200          ELSE                                                            
036300             MOVE NEJ                 TO NYCKLAR-SW                       
036400             MOVE NO-GOOD-PLACE-FOUND TO MED-IDMFSINF                     
036500          END-IF                                                          
036600       END-IF                                                             
036700     END-IF                                                               
036800     IF NYCKLAR-FEL                                                       
036900       MOVE ERR-WRONG-KEY TO    MED-IDMFSFEL                              
037000       CALL WMEDKONV      USING MED-WMEDAREA                              
037100       MOVE MED-MFSFEL    TO    MOD-TEMFSFEL                              
037200       MOVE MED-MFSINF    TO    MOD-TEMFSINF                              
037300       PERFORM MFS-RENSA-FAELT-IN                                         
037400       PERFORM MFS-RENSA-FAELT-UT                                         
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800 C-FOERSTA-SIDA SECTION.                                                  
037900                                                                          
038000     PERFORM MFS-RENSA-FAELT-IN                                           
038100     PERFORM MFS-RENSA-FAELT-UT                                           
038200     .                                                                    
038300     EJECT                                                                
038400 E-SAMMA-SIDA SECTION.                                                    
038500                                                                          
038600     IF EGEN-MID OR HELP-MID                                              
038700       MOVE INF-PRESS-PF11        TO MED-IDMFSINF                         
038800       CALL WMEDKONV              USING MED-WMEDAREA                      
038900       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
039000       PERFORM EA-MID-INDATA-TILL-MOD                                     
039100     ELSE                                                                 
039200       PERFORM MFS-RENSA-FAELT-IN                                         
039300       PERFORM MFS-RENSA-FAELT-UT                                         
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 EA-MID-INDATA-TILL-MOD SECTION.                                          
039800                                                                          
039900*    IF MID-ADLAGOMR-IN             NOT = ALL '+'                         
040000*       MOVE MID-ADLAGOMR-IN        TO MOD-ADLAGOMR-IN                    
040100*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADLAGOMR-IN-ATTR               
040200*    ELSE                                                                 
040300*       MOVE MFS-RENSA-FAELT        TO MOD-ADLAGOMR-IN                    
040400*    END-IF                                                               
040500*                                                                         
040600*    IF MID-ADGANG-IN               NOT = ALL '+'                         
040700*       MOVE MID-ADGANG-IN          TO MOD-ADGANG-IN                      
040800*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADGANG-IN-ATTR                 
040900*    ELSE                                                                 
041000*       MOVE MFS-RENSA-FAELT        TO MOD-ADGANG-IN                      
041100*    END-IF                                                               
041200*                                                                         
041300*    IF MID-ADSEC-IN                NOT = ALL '+'                         
041400*       MOVE MID-ADSEC-IN           TO MOD-ADSEC-IN                       
041500*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADSEC-IN-ATTR                  
041600*    ELSE                                                                 
041700*       MOVE MFS-RENSA-FAELT        TO MOD-ADSEC-IN                       
041800*    END-IF                                                               
041900                                                                          
042000     IF MID-ADLAGOMR NOT = ALL '+' AND MID-ADLAGOMR NUMERIC               
042100        MOVE MID-ADLAGOMR           TO MOD-ADLAGOMR-TO-IN                 
042200        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADLAGOMR-TO-ATTR               
042300     ELSE                                                                 
042400        MOVE MFS-RENSA-FAELT        TO MOD-ADLAGOMR-TO-IN                 
042500     END-IF                                                               
042600                                                                          
042700     IF MID-ADGANG NOT = ALL '+' AND MID-ADGANG NUMERIC                   
042800        MOVE MID-ADGANG             TO MOD-ADGANG-TO-IN                   
042900        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADGANG-TO-ATTR                 
043000     ELSE                                                                 
043100        MOVE MFS-RENSA-FAELT        TO MOD-ADGANG-TO-IN                   
043200     END-IF                                                               
043300                                                                          
043400     IF MID-ADSEC-FOM NOT = ALL '+' AND MID-ADSEC-FOM NUMERIC             
043500        MOVE MID-ADSEC-FOM          TO MOD-ADSEC-FOM-IN                   
043600        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADSEC-FOM-ATTR                 
043700     ELSE                                                                 
043800        MOVE MFS-RENSA-FAELT        TO MOD-ADSEC-FOM-IN                   
043900     END-IF                                                               
044000                                                                          
044100     IF MID-ADSEC-TOM NOT = ALL '+' AND MID-ADSEC-TOM NUMERIC             
044200        MOVE MID-ADSEC-TOM          TO MOD-ADSEC-TOM-IN                   
044300        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADSEC-TOM-ATTR                 
044400     ELSE                                                                 
044500        MOVE MFS-RENSA-FAELT        TO MOD-ADSEC-TOM-IN                   
044600     END-IF                                                               
044700                                                                          
044800     IF MID-KDAOE = ALLA OR ODD OR EVEN                                   
044900        MOVE MID-KDAOE              TO MOD-KDAOE-IN                       
045000        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDAOE-ATTR                     
045100     ELSE                                                                 
045200        MOVE MFS-RENSA-FAELT        TO MOD-KDAOE-IN                       
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 G-KOLLA-INPUT SECTION.                                                   
045700                                                                          
045800     MOVE JA  TO INDATA-SW                                                
045900     IF MID-ADLAGOMR  = ALL '+'  AND                                      
046000        MID-ADGANG    = ALL '+'  AND                                      
046100        MID-ADSEC-FOM = ALL '+'  AND                                      
046200        MID-ADSEC-TOM = ALL '+'  AND                                      
046300        MID-KDAOE     = ALL '+'                                           
046400                                                                          
046500       MOVE ERR-PF11-AND-NO-DATA TO    MED-IDMFSFEL                       
046600       CALL WMEDKONV             USING MED-WMEDAREA                       
046700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
046800       MOVE NEJ                  TO INDATA-SW                             
046900     ELSE                                                                 
047000                                                                          
047100       IF MID-ADLAGOMR = ALL '+' OR MID-ADLAGOMR NOT NUMERIC              
047200         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADLAGOMR-TO-ATTR               
047300         MOVE NEJ                   TO INDATA-SW                          
047400       ELSE                                                               
047500*        IF MID-ADLAGOMR = ZERO                                           
047600*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADLAGOMR-TO-ATTR               
047700*          MOVE NEJ                 TO INDATA-SW                          
047800*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
047900*        ELSE                                                             
048000           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADLAGOMR-TO-ATTR               
048100*        END-IF                                                           
048200       END-IF                                                             
048300                                                                          
048400       IF MID-ADGANG = ALL '+' OR MID-ADGANG NOT NUMERIC                  
048500         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADGANG-TO-ATTR                 
048600         MOVE NEJ                   TO INDATA-SW                          
048700       ELSE                                                               
048800*        IF MID-ADGANG = ZERO                                             
048900*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADGANG-TO-ATTR                 
049000*          MOVE NEJ                 TO INDATA-SW                          
049100*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
049200*        ELSE                                                             
049300           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADGANG-TO-ATTR                 
049400*        END-IF                                                           
049500       END-IF                                                             
049600                                                                          
049700       IF MID-ADSEC-FOM = ALL '+' OR MID-ADSEC-FOM NOT NUMERIC            
049800         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADSEC-FOM-ATTR                 
049900         MOVE NEJ                   TO INDATA-SW                          
050000       ELSE                                                               
050100*        IF MID-ADSEC-FOM = ZERO                                          
050200*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-FOM-ATTR                 
050300*          MOVE NEJ                 TO INDATA-SW                          
050400*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
050500*        ELSE                                                             
050600           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADSEC-FOM-ATTR                 
050700*        END-IF                                                           
050800       END-IF                                                             
050900                                                                          
051000       IF MID-ADSEC-TOM  = ALL '+' OR MID-ADSEC-TOM NOT NUMERIC           
051100         MOVE MFS-NUM-FAELT-FEL     TO MOD-ADSEC-TOM-ATTR                 
051200         MOVE NEJ                   TO INDATA-SW                          
051300       ELSE                                                               
051400*        IF MID-ADSEC-TOM = ZERO                                          
051500*          MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-TOM-ATTR                 
051600*          MOVE NEJ                 TO INDATA-SW                          
051700*          MOVE NO-ZEROES-ALLOWED   TO MED-IDMFSINF                       
051800*        ELSE                                                             
051900           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADSEC-TOM-ATTR                 
052000*        END-IF                                                           
052100       END-IF                                                             
052200                                                                          
052300       IF MID-KDAOE = ALLA OR ODD OR EVEN                                 
052400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAOE-ATTR                      
052500       ELSE                                                               
052600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDAOE-ATTR                      
052700         MOVE NEJ                  TO INDATA-SW                           
052800       END-IF                                                             
052900                                                                          
053000       IF INDATA-OK                                                       
053100         IF MID-ADSEC-FOM  >  MID-ADSEC-TOM                               
053200           MOVE WRONG-INTERVAL-INFO TO MED-IDMFSINF                       
053300           MOVE NEJ                 TO INDATA-SW                          
053400           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-TOM-ATTR                 
053500           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-FOM-ATTR                 
053600         ELSE                                                             
053700*  KONTROLL FÖR ATT KOLLA A/O/E GENTEMOT ANGIVNA INTERVALLER              
053800           EVALUATE MID-KDAOE                                             
053900           WHEN ALLA                                                      
054000             MOVE 1                     TO WS-SECTION-STEP                
054100           WHEN EVEN                                                      
054200*  FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT        
054300             IF FUNCTION MOD (MID-ADSEC-FOM 2) = 0 AND                    
054400                FUNCTION MOD (MID-ADSEC-TOM 2) = 0                        
054500                MOVE 2                  TO WS-SECTION-STEP                
054600             ELSE                                                         
054700               MOVE WRONG-INTERVAL-INFO TO MED-IDMFSINF                   
054800               MOVE NEJ                 TO INDATA-SW                      
054900               MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDAOE-ATTR                 
055000               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-FOM-ATTR             
055100               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-TOM-ATTR             
055200             END-IF                                                       
055300           WHEN ODD                                                       
055400*  FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT        
055500             IF FUNCTION MOD (MID-ADSEC-FOM 2) = 1 AND                    
055600                FUNCTION MOD (MID-ADSEC-TOM 2) = 1                        
055700                MOVE 2 TO WS-SECTION-STEP                                 
055800             ELSE                                                         
055900               MOVE WRONG-INTERVAL-INFO TO MED-IDMFSINF                   
056000               MOVE NEJ                 TO INDATA-SW                      
056100               MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDAOE-ATTR                 
056200               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-FOM-ATTR             
056300               MOVE MFS-NUM-FAELT-FEL   TO MOD-ADSEC-TOM-ATTR             
056400             END-IF                                                       
056500           END-EVALUATE                                                   
056600         END-IF                                                           
056700       END-IF                                                             
056800                                                                          
056900         MOVE MFS-ADD-SET-CURSOR   TO MOD-ADLAGOMR-TO-ATTR                
057000       IF INDATA-FEL                                                      
057100         MOVE ERR-CORR-HILITE-FLDS TO    MED-IDMFSFEL                     
057200         CALL WMEDKONV             USING MED-WMEDAREA                     
057300         MOVE MED-MFSFEL           TO    MOD-TEMFSFEL                     
057400         MOVE MED-MFSINF           TO    MOD-TEMFSINF                     
057500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
057600       END-IF                                                             
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000                                                                          
058100 H-UPPDATERA SECTION.                                                     
058200     PERFORM HA-INIT                                                      
058300     PERFORM HB-UPDATE                                                    
058400     PERFORM HC-CLOSE                                                     
058500     .                                                                    
058600     EJECT                                                                
058700 HA-INIT SECTION.                                                         
058800     MOVE MID-ADLAGOMR  TO  WS-ADLAGOMR-UPD   MOD-ADLAGOMR-TO-UT          
058900     MOVE MID-ADGANG    TO  WS-ADGANG-UPD     MOD-ADGANG-TO-UT            
059000     MOVE MID-ADSEC-FOM TO  WS-ADSEC-FOM-UPD  MOD-ADSEC-FOM-UT            
059100                            WS-SECTION                                    
059200     MOVE MID-ADSEC-TOM TO  WS-ADSEC-TOM-UPD  MOD-ADSEC-TOM-UT            
059300     MOVE MID-KDAOE     TO                    MOD-KDAOE-UT                
059400     .                                                                    
059500     EJECT                                                                
059600 HB-UPDATE SECTION.                                                       
059700* FYLLER TABELLEN AV INFORMATION OM HELA SEKTIONEN, PLATS FÖR             
059800* PLATS, VARJE INDEX MOTSVARAR EN LAGERPLATS.                             
059900     INITIALIZE TEMP-TABELL                                               
060000     MOVE NEJ             TO ARE-THE-TABLE-FILLED                         
060100     MOVE 1               TO IX                                           
060200     MOVE LOCA-LOC-WDJ801 TO TEMP-LOC-WDJ801(IX)                          
060300     ADD 1                TO IX                                           
060400     PERFORM UNTIL THE-TABLE-ARE-FILLED                                   
060500       PERFORM IMS-GET-NEXT-LOC                                           
060600       IF SEGMENT-FINNS                                                   
060700         MOVE LOCA-LOC-ADPLATS  TO TEMP-ADPLATS                           
060800         IF TEMP-ADSEC        = WS-ADSEC            AND                   
060900            LOCA-LOC-ADGANG   = WS-ADGANG           AND                   
061000            LOCA-LOC-ADLAGOMR = WS-ADLAGOMR                               
061100                                                                          
061200           MOVE LOCA-LOC-WDJ801 TO TEMP-LOC-WDJ801(IX)                    
061300           ADD 1                TO IX                                     
061400         ELSE                                                             
061500           MOVE JA              TO ARE-THE-TABLE-FILLED                   
061600         END-IF                                                           
061700       ELSE                                                               
061800         MOVE JA                TO ARE-THE-TABLE-FILLED                   
061900       END-IF                                                             
062000     END-PERFORM                                                          
062100                                                                          
062200* SKAPAR DE NYA SEKTIONERNA                                               
062300*                                                                         
062400     PERFORM UNTIL WS-SECTION > WS-ADSEC-TOM-UPD      OR                  
062500                   SEGMENT-FINNS-REDAN                                    
062600     MOVE 1   TO IX                                                       
062700     MOVE NEJ TO ARE-ALL-PLACES-UPDATED                                   
062800       PERFORM UNTIL ALL-PLACES-UPDATED OR SEGMENT-FINNS-REDAN            
062900         MOVE TEMP-LOC-WDJ801(IX)  TO LOCA-LOC-WDJ801                     
063000         MOVE WS-ADLAGOMR-UPD      TO LOCA-LOC-ADLAGOMR                   
063100         MOVE WS-ADGANG-UPD        TO LOCA-LOC-ADGANG                     
063200         MOVE TEMP-LOC-ADPLATS(IX) TO TEMP-ADPLATS                        
063300         MOVE WS-SECTION           TO TEMP-ADSEC                          
063400         MOVE TEMP-ADPLATS         TO LOCA-LOC-ADPLATS                    
063500                                                                          
063600         PERFORM IMS-ISRT-LOCA-LOC                                        
063700         IF SEGMENT-FINNS                                                 
063800           ADD 1           TO IX                                          
063900           IF TEMP-LOC-ADLAGOMR(IX) > 0                                   
064000             CONTINUE                                                     
064100           ELSE                                                           
064200             MOVE JA       TO ARE-ALL-PLACES-UPDATED                      
064300           END-IF                                                         
064400         ELSE                                                             
064500           MOVE JA         TO ARE-ALL-PLACES-UPDATED                      
064600         END-IF                                                           
064700       END-PERFORM                                                        
064800       ADD WS-SECTION-STEP TO WS-SECTION                                  
064900     END-PERFORM                                                          
065000     .                                                                    
065100     EJECT                                                                
065200 HC-CLOSE SECTION.                                                        
065300     IF SEGMENT-FINNS-REDAN                                               
065400        MOVE LOCA-LOC-IDDC     TO MED-IDDC                                
065500        MOVE WS-ADLAGOMR-UPD   TO MED-ADLAGOMR                            
065600        MOVE WS-ADGANG-UPD     TO MED-ADGANG                              
065700*       MOVE TEMP-ADSEC        TO MED-ADSEC                               
065800*       MOVE TEMP-ADLEVEL      TO MED-ADLEVEL                             
065900*       MOVE TEMP-ADSEQ        TO MED-ADSEQ                               
066000        MOVE LOCA-LOC-ADPLATS  TO MED1-ADPLATS                            
066100        MOVE MED1-ADSEC        TO MED-ADSEC                               
066200        MOVE MED1-ADLEVEL      TO MED-ADLEVEL                             
066300        MOVE MED1-ADSEQ        TO MED-ADSEQ                               
066400                                                                          
066500        MOVE MED-1             TO MOD-TEMFSFEL                            
066600     END-IF                                                               
066700                                                                          
066800     MOVE INF-UPDATE-DONE      TO    MED-IDMFSINF                         
066900     CALL WMEDKONV             USING MED-WMEDAREA                         
067000     MOVE MED-MFSINF           TO    MOD-TEMFSINF                         
067100     .                                                                    
067200     EJECT                                                                
067300 MFS-RENSA-FAELT-IN SECTION.                                              
067400                                                                          
067500     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-IN                              
067600                                MOD-ADGANG-IN                             
067700                                MOD-ADSEC-IN                              
067800     .                                                                    
067900     EJECT                                                                
068000 MFS-RENSA-FAELT-UT SECTION.                                              
068100                                                                          
068200     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-TO-UT                           
068300                                MOD-ADLAGOMR-TO-UT                        
068400                                MOD-ADGANG-TO-UT                          
068500                                MOD-ADSEC-FOM-UT                          
068600                                MOD-ADSEC-TOM-UT                          
068700                                MOD-KDAOE-UT                              
068800                                MOD-ADLAGOMR-UT                           
068900                                MOD-ADGANG-UT                             
069000                                MOD-ADSEC-UT                              
069100     .                                                                    
069200     EJECT                                                                
069300 MFS-ROER-EJ-FAELT-IN SECTION.                                            
069400                                                                          
069500*    --- ALLA INDATA-FÄLT                                                 
069600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC                                   
069700                                MOD-ADLAGOMR-IN                           
069800                                MOD-ADGANG-IN                             
069900                                MOD-ADSEC-IN                              
070000                                MOD-ADLAGOMR-TO-IN                        
070100                                MOD-ADGANG-TO-IN                          
070200                                MOD-ADSEC-FOM-IN                          
070300                                MOD-ADSEC-TOM-IN                          
070400                                MOD-KDAOE-IN                              
070500     .                                                                    
070600     EJECT                                                                
070700* --- IMS SEKTIONER ---                                                   
070800     SKIP3                                                                
070900 IMS-GET-MSG SECTION.                                                     
071000                                                                          
071100     MOVE '  QC' TO GODK-STATUSKODER                                      
071200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
071300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071400     PERFORM IMS-STATUSKONTROLL                                           
071500     .                                                                    
071600     SKIP3                                                                
071700 IMS-INSERT-MSG SECTION.                                                  
071800                                                                          
071900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
072000     MOVE SPACE TO GODK-STATUSKODER                                       
072100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
072200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     EJECT                                                                
072600 IMS-GET-LOCA-LOC SECTION.                                                
072700                                                                          
072800     STRING 'WLLOCA01(WDJ801KY=>' W-WDJ801KY-X ')'                        
072900          DELIMITED BY SIZE INTO SSA1                                     
073000     MOVE '  GE' TO GODK-STATUSKODER                                      
073100     CALL CBLTDLI USING GU LOCA-PCB DLI-IO-WLLOCA01 SSA1                  
073200     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
073300     PERFORM IMS-STATUSKONTROLL                                           
073400     .                                                                    
073500     SKIP3                                                                
073600 IMS-GET-NEXT-LOC SECTION.                                                
073700                                                                          
073800     MOVE 'WLLOCA01 ' TO SSA1                                             
073900     MOVE '  GB' TO GODK-STATUSKODER                                      
074000     CALL CBLTDLI USING GN LOCA-PCB DLI-IO-WLLOCA01 SSA1                  
074100     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
074200     PERFORM IMS-STATUSKONTROLL                                           
074300     .                                                                    
074400     SKIP3                                                                
074500 IMS-ISRT-LOCA-LOC SECTION.                                               
074600                                                                          
074700     MOVE 'WLLOCA01 ' TO SSA1                                             
074800     MOVE '  II' TO GODK-STATUSKODER                                      
074900     CALL CBLTDLI USING ISRT LOCA-PCB DLI-IO-WLLOCA01 SSA1                
075000     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
075100     PERFORM IMS-STATUSKONTROLL                                           
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500 IMS-GU-WDB601    SECTION.                                                
075600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
075700          DELIMITED BY SIZE INTO SSA1                                     
075800     MOVE '  GE' TO GODK-STATUSKODER                                      
075900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
076000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
076100     PERFORM IMS-STATUSKONTROLL                                           
076200     .                                                                    
076300     EJECT                                                                
076400 IMS-STATUSKONTROLL SECTION.                                              
076500                                                                          
076600     SET STATUS-IX TO 1                                                   
076700     SEARCH GODK-STATUS                                                   
076800       AT END                                                             
076900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
077000         DELIMITED BY SIZE INTO FELTEXT                                   
077100         CALL FELLOG                                                      
077200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
077300         CONTINUE                                                         
077400     END-SEARCH                                                           
077500     .                                                                    
