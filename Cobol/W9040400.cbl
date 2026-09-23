000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                W9040400.                                     
000300*AUTHOR.                    IDK, GÖTEBORG.                                
000400*DATE-COMPILED.                                                           
000500*DATE-WRITTEN.              MAJ  -79.                                     
000600*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000700*                ERSÄTTNINGAR. ANGIVEN ARTIKEL 'ERSÄTTER'                 
000800*    INDATA.                                                              
000900*        TRANSAKTION: W90404T                                             
001000*        MID:         W90404I1                                            
001100*    UTDATA.                                                              
001200*        MOD:         W90404O1                                            
001300*    SUBPROGRAM.                                                          
001400*        FELLOG                                                           
001500*                                                                         
001600*   ÄNDRINGAR:                                                            
001700*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
001800*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
001900*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002000*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002100*                                                                         
002200*        MARS-04   KOPIERAT PGM:ET FRÅN W1010300 OCH SEDAN                
002300*                  MODIFIERAT DET FÖR SPIE2 GENOM ATT TA                  
002400*                  BORT ONÖDIG INFORMATION ATT SKICKA TILL SPIE2.         
002500*                  //JOHAN NIHLBLAD                                       
002600*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM               PIC X(8)    VALUE 'W9040400'.                    
003600     SKIP3                                                                
003700 77      SPIND           PIC S9(9)   VALUE +0    COMP SYNC.               
003800 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
003900     SKIP1                                                                
004000 77      MAX-MOD-LENGD   PIC S9(4)   VALUE +1182 COMP SYNC.               
004100 77      MAX-ANT-BILD-RADER                                               
004200                         PIC S9(9)   VALUE +13   COMP SYNC.               
004300 77      WS-IDLEVNR-8    PIC  X(8)   VALUE SPACE.                         
004400                                                                          
004500 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
004600     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
004700     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
004800                                                                          
004900                                                                          
005000     SKIP3                                                                
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005600                                                                          
005700 01      W-IDARTNR-X.                                                     
005800   03    W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
005900 01      W-ERSATT-IDARTNR-X.                                              
006000   03    W-ERSATT-IDARTNR        PIC S9(9)  VALUE ZERO  COMP-3.           
006100 01      W-IDSKYLT-X.                                                     
006200   03    W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
006300     SKIP3                                                                
006400 01  W.                                                                   
006500     03  IX                  PIC S9(9)               COMP SYNC.           
006600     EJECT                                                                
006700 01      MEDDELANDEN.                                                     
006800   03    W-TEXT-KOMMENTAR-1.                                              
006900     05  FILLER     PIC X(14)   VALUE                                     
007000                                'EJ ENSAM TILL.'.                         
007100     05  FILLER     PIC X(14)   VALUE                                     
007200                                'MORE ADD PART.'.                         
007300   03   FILLER REDEFINES W-TEXT-KOMMENTAR-1.                              
007400     05  TEXT-KOMMENTAR-1   PIC X(14) OCCURS 2.                           
007500                                                                          
007600   03    W-TEXT-KOMMENTAR-2.                                              
007700     05  FILLER          PIC X(27)   VALUE                                
007800                         'FLER ERSATTA ARTIKLAR FINNS'.                   
007900     05  FILLER          PIC X(27)   VALUE                                
008000                         '   SEE MORE REPLACED PARTS '.                   
008100   03   FILLER REDEFINES W-TEXT-KOMMENTAR-2.                              
008200     05  TEXT-KOMMENTAR-2  PIC X(27) OCCURS 2.                            
008300     SKIP3                                                                
008400 01      FELMEDDELANDE.                                                   
008500   03    W-FEL-1.                                                         
008600     05  FILLER          PIC X(26)   VALUE                                
008700                                   'ARTIKELNUMMER EJ NUMERISKT'.          
008800     05  FILLER          PIC X(26)   VALUE                                
008900                                   'PARTNUMBER NOT NUMERIC    '.          
009000   03   FILLER REDEFINES W-FEL-1.                                         
009100     05  FEL-1           PIC X(26) OCCURS 2.                              
009200                                                                          
009300   03    W-FEL-2.                                                         
009400     05  FILLER          PIC X(35)   VALUE                                
009500                                     'ARTIKEL ERSÄTTER EJ'.               
009600     05  FILLER          PIC X(35)   VALUE                                
009700                                     'NOT A SUPERSEDING PART'.            
009800   03   FILLER REDEFINES W-FEL-2.                                         
009900     05  FEL-2           PIC X(35) OCCURS 2.                              
010000                                                                          
010100   03    W-FEL-3.                                                         
010200     05  FILLER          PIC X(15)   VALUE 'ARTIKELN ERSATT'.             
010300     05  FILLER          PIC X(15)   VALUE 'PART SUPERSEDED'.             
010400   03   FILLER REDEFINES W-FEL-3.                                         
010500     05  FEL-3           PIC X(15) OCCURS 2.                              
010600                                                                          
010700   03    W-FEL-4.                                                         
010800     05  FILLER          PIC X(17)   VALUE 'ARTIKELN UTGÅNGEN'.           
010900     05  FILLER          PIC X(17)   VALUE 'PART IS DELETED  '.           
011000   03   FILLER REDEFINES W-FEL-4.                                         
011100     05  FEL-4           PIC X(17) OCCURS 2.                              
011200                                                                          
011300   03    W-FEL-5.                                                         
011400     05  FILLER          PIC X(18)   VALUE 'ERSÄTTANDE ARTIKEL'.          
011500     05  FILLER          PIC X(18)   VALUE 'SUPERSEDING PART  '.          
011600   03   FILLER REDEFINES W-FEL-5.                                         
011700     05  FEL-5           PIC X(18) OCCURS 2.                              
011800                                                                          
011900   03    W-FEL-6.                                                         
012000     05  FILLER        PIC X(19)   VALUE 'OBEHÖRIG ANVÄNDARE '.           
012100     05  FILLER        PIC X(19)   VALUE 'USER NOT AUTHORIZED'.           
012200   03   FILLER REDEFINES W-FEL-6.                                         
012300     05  FEL-6         PIC X(19) OCCURS 2.                                
012400                                                                          
012500   03    W-FEL-7.                                                         
012600     05  FILLER PIC X(40) VALUE 'SÖKT ARTIKELNR EJ REGISTRERAT'.          
012700     05  FILLER PIC X(40) VALUE 'KEY PART NO. NOT REGISTERED'.            
012800   03   FILLER REDEFINES W-FEL-7.                                         
012900     05  FEL-7         PIC X(40) OCCURS 2.                                
013000     EJECT                                                                
013100*                        ****    PARAMETRAR TILL W005INIT                 
013200*        -COPY WMSGINIT                                                   
013300     SKIP3                                                                
013400*                        ****    TP-AREOR                                 
013500 01      TP-WS.                                                           
013600   03    FILLER          PIC X(16)   VALUE '   TP-AREOR    '.             
013700     SKIP3                                                                
013800*01      MID -COPY W90404I1 -PRE MID-.                                    
013900     SKIP3                                                                
014000*01      -COPY WMSGAREA                                                   
014100     SKIP3                                                                
014200*  03    MOD -COPY W90404O1 -PRE MOD- -RED MSG-AREA.                      
014300     EJECT                                                                
014400*01  -COPY WMFSAREA.                                                      
014500     EJECT                                                                
014600******************************************************************        
014700*****                                                                     
014800*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014900*****                                                                     
015000 01  IMS-WS.                                                              
015100   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
015200     SKIP3                                                                
015300*****                    **** STATUS-KOD FRÅN IMS                         
015400   03    STATUS-WS       PIC XX.                                          
015500         88  SEGMENT-FINNS       VALUE '  '.                              
015600         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
015700     SKIP3                                                                
015800   03    GODK-STATUSKODER.                                                
015900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01      SSA1            PIC X(64).                                       
016200 01      SSA2            PIC X(64).                                       
016300     EJECT                                                                
016400*                            IMS FUNKTIONSKODER                           
016500*01      -COPY W0003                                                      
016600     EJECT                                                                
016700*                            DLI INPUT-OUTPUT AREA                        
016800 01  DLI-IO-AREA.                                                         
016900     03  IO-AREA         PIC X(900)  VALUE SPACE.                         
017000     SKIP3                                                                
017100*    03  WLARTC01 -COPY WDK601               -RED IO-AREA.                
017200     EJECT                                                                
017300*    03  WLARTC11 -COPY WDK611               -RED IO-AREA.                
017400     EJECT                                                                
017500*    03  WLBENA11 -COPY WDD311 -PRE BENA-    -RED IO-AREA.                
017600     EJECT                                                                
017700     03  FILLER REDEFINES IO-AREA.                                        
017800*        05  WLERSA11 -COPY WDD702 -PRE TILLK-.                           
017900     EJECT                                                                
018000*        05  WLERSA01 -COPY WDD701 -PRE ERSATT-.                          
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300*01  -COPY W0009     -PRE MSG-                                            
018400     SKIP3                                                                
018500*01  -COPY W0008     -PRE USEA-                                           
018600         05  FILLER           PIC X.                                      
018700     SKIP3                                                                
018800*01  -COPY W0008     -PRE WLARTC-                                         
018900         05  FILLER           PIC X.                                      
019000     SKIP3                                                                
019100*01  -COPY W0008     -PRE WLERSA-                                         
019200         05  FILLER           PIC X.                                      
019300     EJECT                                                                
019400*01  -COPY W0008     -PRE WLBENA-                                         
019500         05  FILLER           PIC X.                                      
019600     EJECT                                                                
019700 PROCEDURE DIVISION USING MSG-PCB USEA-PCB   WLARTC-PCB                   
019800                                  WLERSA-PCB WLBENA-PCB.                  
019900                                                                          
020000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB   WLARTC-PCB                  
020100                                   WLERSA-PCB WLBENA-PCB.                 
020200     SKIP1                                                                
020300     PERFORM IMS-GET-MSG                                                  
020400     SKIP1                                                                
020500     IF SEGMENT-FINNS                                                     
020600       PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                                
020700                                                                          
020800       IF IDARTNR-WS NUMERIC                                              
020900                                                                          
021000         PERFORM S1-SECURITY-CHECK-PARTNO-IDLEV                           
021100         IF PASSED-SECURITY-CHECK                                         
021200                                                                          
021300           PERFORM B-RED-BILD                                             
021400         END-IF                                                           
021500       ELSE                                                               
021600         MOVE FEL-1 (SPIND) TO MOD-MESSAGE                                
021700       END-IF                                                             
021800     END-IF                                                               
021900     SKIP1                                                                
022000     PERFORM IMS-INSERT-MSG                                               
022100     SKIP1                                                                
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     CONTINUE.                                                            
022500     EJECT                                                                
022600 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
022700******************************************************************        
022800*                                                                *        
022900*                                                                *        
023000*                                                                *        
023100******************************************************************        
023200     SKIP1                                                                
023300     IF MSG-DUBBLA-TRANSKODER                                             
023400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90404I1-CTX             
023700     ELSE                                                                 
023800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90404I1-CTX              
024100     END-IF                                                               
024200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
024300     MOVE '001'             TO MSGI-KDCALL                                
024400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024500                               MSGI-IDLTERM-USER                          
024600     MOVE '9404'            TO MSGI-IDTRANS                               
024700     IF MFS-IDTRANS = '9404'                                              
024800     OR (MID-IDARTNR-IN NUMERIC                                           
024900     AND MID-IDARTNR-IN > ZERO)                                           
025000         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
025100     END-IF                                                               
025200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025300     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
025400     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
025500*    MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
025600*    INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
025700     SKIP1                                                                
025800     MOVE LOW-VALUE TO MOD-AREA-OUTPUT                                    
025900     MOVE 'W90404O1' TO MFS-IDMOD                                         
026000     MOVE '9404' TO MOD-IDTRANS                                           
026100     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
026200                                                                          
026300     IF MSGI-IDLAND-SPR = SPACE                                           
026400       IF MSGI-IDSPRAK = SPACE                                            
026500         IF SWEDISH-TEXT                                                  
026600           MOVE +1 TO SPIND                                               
026700         ELSE                                                             
026800           MOVE +2 TO SPIND                                               
026900         END-IF                                                           
027000       ELSE                                                               
027100         IF MSGI-IDSPRAK = 'SV'                                           
027200           MOVE +1 TO SPIND                                               
027300         ELSE                                                             
027400           MOVE +2 TO SPIND                                               
027500         END-IF                                                           
027600       END-IF                                                             
027700     ELSE                                                                 
027800       IF MSGI-IDLAND-SPR = 'SE'                                          
027900         MOVE +1 TO SPIND                                                 
028000       ELSE                                                               
028100         MOVE +2 TO SPIND                                                 
028200       END-IF                                                             
028300     END-IF                                                               
028400                                                                          
028500     MOVE MFS-RENSA-FAELT TO                                              
028600*                            MOD-IDARTNR-IN                               
028700                             MOD-BLAEDRING-IDARTNR                        
028800                             MOD-MESSAGE                                  
028900                             MOD-MESSAGE-BOTTOM                           
029000     MOVE 1 TO IX                                                         
029100     PERFORM UNTIL                                                        
029200      ( IX > MAX-ANT-BILD-RADER )                                         
029300       MOVE MFS-RENSA-FAELT TO MOD-RADER (IX)                             
029400       ADD 1 TO IX                                                        
029500     END-PERFORM                                                          
029600     SKIP1                                                                
029700     IF  MFS-IDTRANS     = '9404'                                         
029800       MOVE MID-BLAEDRING-IDARTNR TO W-ERSATT-IDARTNR                     
029900     ELSE                                                                 
030000       MOVE ZERO TO W-ERSATT-IDARTNR                                      
030100     END-IF                                                               
030200     SUBTRACT 1 FROM W-ERSATT-IDARTNR                                     
030300     CONTINUE.                                                            
030400     EJECT                                                                
030500 B-RED-BILD      SECTION.                                                 
030600******************************************************************        
030700*                                                                *        
030800*    REDIGERING AV BILD MED DATA FRÅN WDD7 WDK6 WDD3             *        
030900*                                                                *        
031000******************************************************************        
031100     SKIP1                                                                
031200     MOVE IDARTNR-WS TO W-IDARTNR                                         
031300     PERFORM IMS-GET-ERSATT-INFO                                          
031400     IF SEGMENT-FINNS                                                     
031500       MOVE 1 TO IX                                                       
031600       SKIP1                                                              
031700       PERFORM UNTIL                                                      
031800        NOT ( SEGMENT-FINNS AND IX NOT > MAX-ANT-BILD-RADER )             
031900         IF ERSATT-IDARTNR > W-ERSATT-IDARTNR                             
032000           MOVE ERSATT-DIERS-ERS TO MOD-DIERS-ERS (IX)                    
032100           MOVE ERSATT-IDARTNR TO MOD-IDARTNR    (IX)                     
032200                                   W-ERSATT-IDARTNR                       
032300           IF ERSATT-KVKORT > 1                                           
032400             MOVE TEXT-KOMMENTAR-1 (SPIND) TO                             
032500                                MOD-TEXT-KOMMENTAR (IX)                   
032600           ELSE                                                           
032700             MOVE SPACE TO MOD-TEXT-KOMMENTAR (IX)                        
032800           END-IF                                                         
032900           MOVE TILLK-DIERS-TILLK TO MOD-DIERS-TILLK (IX)                 
033000                                                                          
033100           PERFORM IMS-GET-ARTC01-ERSATT                                  
033200           IF SEGMENT-FINNS                                               
033300             PERFORM BA-UPPDAT-BILD-FRAN-WDK6                             
033400                                                                          
033500             IF SWEDISH-TEXT                                              
033600               MOVE 'S  ' TO W-IDSKYLT                                    
033700             ELSE                                                         
033800               MOVE 'GB ' TO W-IDSKYLT                                    
033900             END-IF                                                       
034000             PERFORM IMS-GET-BENA11                                       
034100             MOVE BENA-TEXT-BEART TO MOD-BEART (IX)                       
034101           ELSE                                                           
034102             MOVE SPACE TO MOD-BEART (IX)                                 
034103             MOVE ZERO  TO MOD-KDERS-C1 (IX)                              
034110           END-IF                                                         
034200           ADD 1 TO IX                                                    
034300         END-IF                                                           
034400         PERFORM IMS-GET-ERSATT-INFO                                      
034500       END-PERFORM                                                        
034600       IF IX > MAX-ANT-BILD-RADER AND SEGMENT-FINNS                       
034700         MOVE ERSATT-IDARTNR TO MOD-BLAEDRING-IDARTNR                     
034800         MOVE TEXT-KOMMENTAR-2 (SPIND) TO MOD-MESSAGE-BOTTOM              
034900       END-IF                                                             
035000     ELSE                                                                 
035100       MOVE FEL-2 (SPIND) TO MOD-MESSAGE                                  
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 BA-UPPDAT-BILD-FRAN-WDK6 SECTION.                                        
035600******************************************************************        
035700*                                                                *        
035800*    REDIGERING AV BILD MED DATA FRÅN WDK6                       *        
035900*                                                                *        
036000******************************************************************        
036100                                                                          
036200     MOVE ART-KDERS-UTG TO MOD-KDERS-C1 (IX)                              
036300                                                                          
036400     PERFORM IMS-GET-ARTC11                                               
036500     IF  SEGMENT-FINNS                                                    
036600       MOVE CLAG-KDERS TO MOD-KDERS-C1 (IX)                               
036700     END-IF                                                               
036800     CONTINUE.                                                            
036900     EJECT                                                                
037000                                                                          
037100 S1-SECURITY-CHECK-PARTNO-IDLEV SECTION.                                  
037200     SKIP2                                                                
037300*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
037400     MOVE IDARTNR-WS  TO W-IDARTNR                                        
037500     PERFORM IMS-GET-ARTC01-ERSATTANDE                                    
037600     IF  SEGMENT-FINNS                                                    
037700       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
037800       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
037900       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
038000*        --- BEHÖRIG USER                                                 
038100         SET PASSED-SECURITY-CHECK TO TRUE                                
038200       ELSE                                                               
038300*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
038400         MOVE FEL-6 (SPIND) TO MOD-MESSAGE                                
038500         SET BLOCKED-SECURITY-CHECK TO TRUE                               
038600       END-IF                                                             
038700     ELSE                                                                 
038800*      --- ERSÄTTANDE ARTIKEL SAKNAS PÅ WDK6                              
038900         MOVE FEL-7 (SPIND) TO MOD-MESSAGE                                
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400* IMS SEKTIONER                                                           
039500     SKIP3                                                                
039600 IMS-GET-MSG SECTION.                                                     
039700     SKIP2                                                                
039800     MOVE '  QC' TO GODK-STATUSKODER                                      
039900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
040000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040100     PERFORM IMS-STATUSKONTROLL                                           
040200     CONTINUE.                                                            
040300     SKIP3                                                                
040400 IMS-INSERT-MSG SECTION.                                                  
040500     SKIP2                                                                
040600*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
040700*      MOVE '0' TO MFS-KDHUVOMR                                           
040800*    END-IF                                                               
040900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
041000     MOVE SPACE TO GODK-STATUSKODER                                       
041100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041300     PERFORM IMS-STATUSKONTROLL                                           
041400     CONTINUE.                                                            
041500     EJECT                                                                
041600 IMS-GET-ARTC01-ERSATTANDE SECTION.                                       
041700     SKIP2                                                                
041800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
041900            DELIMITED BY SIZE INTO SSA1                                   
042000     MOVE '  GE' TO GODK-STATUSKODER                                      
042100     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA SSA1                    
042200     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
042300     PERFORM IMS-STATUSKONTROLL                                           
042400     CONTINUE.                                                            
042500     SKIP3                                                                
042600 IMS-GET-ARTC01-ERSATT SECTION.                                           
042700     SKIP2                                                                
042800     STRING 'WLARTC01(IDARTNR  =' W-ERSATT-IDARTNR-X ')'                  
042900            DELIMITED BY SIZE INTO SSA1                                   
043000     MOVE '  GE' TO GODK-STATUSKODER                                      
043100     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA SSA1                    
043200     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     CONTINUE.                                                            
043500     SKIP3                                                                
043600 IMS-GET-ARTC11 SECTION.                                                  
043700     SKIP2                                                                
043800     MOVE 'WLARTC11 '  TO SSA1                                            
043900     MOVE '  GE' TO GODK-STATUSKODER                                      
044000     CALL CBLTDLI USING GNP WLARTC-PCB DLI-IO-AREA SSA1                   
044100     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
044200     PERFORM IMS-STATUSKONTROLL                                           
044300     CONTINUE.                                                            
044400     EJECT                                                                
044500 IMS-GET-ERSATT-INFO SECTION.                                             
044600     SKIP2                                                                
044700     STRING 'WLERSA11*D(WDD7ASEQ =' W-IDARTNR-X ')'                       
044800            DELIMITED BY SIZE INTO SSA1                                   
044900     MOVE 'WLERSA01 ' TO SSA2                                             
045000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
045100     CALL CBLTDLI USING GN WLERSA-PCB DLI-IO-AREA SSA1 SSA2               
045200     MOVE WLERSA-STATUS-CODE TO STATUS-WS                                 
045300     PERFORM IMS-STATUSKONTROLL                                           
045400     CONTINUE.                                                            
045500     SKIP3                                                                
045600 IMS-GET-BENA11 SECTION.                                                  
045700     SKIP2                                                                
045800     STRING 'WLBENA01(WDD3BSEQ =' W-ERSATT-IDARTNR-X ')'                  
045900            DELIMITED BY SIZE INTO SSA1                                   
046000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
046100            DELIMITED BY SIZE INTO SSA2                                   
046200     MOVE '  ' TO GODK-STATUSKODER                                        
046300     CALL CBLTDLI USING GU WLBENA-PCB DLI-IO-AREA SSA1 SSA2               
046400     MOVE WLBENA-STATUS-CODE TO STATUS-WS                                 
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     CONTINUE.                                                            
046700     EJECT                                                                
046800 IMS-STATUSKONTROLL SECTION.                                              
046900     SET STATUS-IX TO 1                                                   
047000     SEARCH GODK-STATUS                                                   
047100       AT END                                                             
047200         CALL FELLOG                                                      
047300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
047400     END-SEARCH                                                           
047500     CONTINUE                                                             
047600            CONTINUE.                                                     
047700 IMS-STATUS-KONTROLL-EXIT. EXIT.                                          
047800     CONTINUE.                                                            
