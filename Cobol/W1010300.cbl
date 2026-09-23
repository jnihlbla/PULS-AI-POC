000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                W1010300.                                     
000300*AUTHOR.                    IDK, GÖTEBORG.                                
000400*DATE-COMPILED.                                                           
000500*DATE-WRITTEN.              MAJ  -79.                                     
000600*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000700*                ERSÄTTNINGAR. ANGIVEN ARTIKEL 'ERSÄTTER'                 
000800*    INDATA.                                                              
000900*        TRANSAKTION: W1T103                                              
001000*        MID:         W1I10301                                            
001100*    UTDATA.                                                              
001200*        MOD:         W1O10301                                            
001300*    SUBPROGRAM.                                                          
001400*        FELLOG                                                           
001500*                                                                         
001600*   ÄNDRINGAR:                                                            
001700*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
001800*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
001900*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002000*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002100*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM               PIC X(8)    VALUE 'W1010300'.                    
003100     SKIP3                                                                
003200 77      SPIND           PIC S9(9)   VALUE +0    COMP SYNC.               
003300 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
003400     SKIP1                                                                
003500 77      MAX-MOD-LENGD   PIC S9(4)   VALUE +1182 COMP SYNC.               
003600 77      MAX-ANT-BILD-RADER                                               
003700                         PIC S9(9)   VALUE +13   COMP SYNC.               
003800 77      WS-IDLEVNR-8    PIC  X(8)   VALUE SPACE.                         
003900                                                                          
004000 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
004100     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
004200     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
004300                                                                          
004400                                                                          
004500     SKIP3                                                                
004600                                                                          
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005100                                                                          
005200 01      W-IDARTNR-X.                                                     
005300   03    W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
005400 01      W-ERSATT-IDARTNR-X.                                              
005500   03    W-ERSATT-IDARTNR        PIC S9(9)  VALUE ZERO  COMP-3.           
005600 01      W-IDSKYLT-X.                                                     
005700   03    W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
005800     SKIP3                                                                
005900 01  W.                                                                   
006000     03  IX                  PIC S9(9)               COMP SYNC.           
006100     EJECT                                                                
006200 01      MEDDELANDEN.                                                     
006300   03    W-TEXT-KOMMENTAR-1.                                              
006400     05  FILLER     PIC X(14)   VALUE                                     
006500                                'EJ ENSAM TILL.'.                         
006600     05  FILLER     PIC X(14)   VALUE                                     
006700                                'MORE ADD PART.'.                         
006800   03   FILLER REDEFINES W-TEXT-KOMMENTAR-1.                              
006900     05  TEXT-KOMMENTAR-1   PIC X(14) OCCURS 2.                           
007000                                                                          
007100   03    W-TEXT-KOMMENTAR-2.                                              
007200     05  FILLER          PIC X(27)   VALUE                                
007300                         'FLER ERSATTA ARTIKLAR FINNS'.                   
007400     05  FILLER          PIC X(27)   VALUE                                
007500                         '   SEE MORE REPLACED PARTS '.                   
007600   03   FILLER REDEFINES W-TEXT-KOMMENTAR-2.                              
007700     05  TEXT-KOMMENTAR-2  PIC X(27) OCCURS 2.                            
007800     SKIP3                                                                
007900 01      FELMEDDELANDE.                                                   
008000   03    W-FEL-1.                                                         
008100     05  FILLER          PIC X(26)   VALUE                                
008200                                   'ARTIKELNUMMER EJ NUMERISKT'.          
008300     05  FILLER          PIC X(26)   VALUE                                
008400                                   'PARTNUMBER NOT NUMERIC    '.          
008500   03   FILLER REDEFINES W-FEL-1.                                         
008600     05  FEL-1           PIC X(26) OCCURS 2.                              
008700                                                                          
008800   03    W-FEL-2.                                                         
008900     05  FILLER          PIC X(35)   VALUE                                
009000                                     'ARTIKEL ERSÄTTER EJ'.               
009100     05  FILLER          PIC X(35)   VALUE                                
009200                                     'NOT A SUPERSEDING PART'.            
009300   03   FILLER REDEFINES W-FEL-2.                                         
009400     05  FEL-2           PIC X(35) OCCURS 2.                              
009500                                                                          
009600   03    W-FEL-3.                                                         
009700     05  FILLER          PIC X(15)   VALUE 'ARTIKELN ERSATT'.             
009800     05  FILLER          PIC X(15)   VALUE 'PART SUPERSEDED'.             
009900   03   FILLER REDEFINES W-FEL-3.                                         
010000     05  FEL-3           PIC X(15) OCCURS 2.                              
010100                                                                          
010200   03    W-FEL-4.                                                         
010300     05  FILLER          PIC X(17)   VALUE 'ARTIKELN UTGÅNGEN'.           
010400     05  FILLER          PIC X(17)   VALUE 'PART IS DELETED  '.           
010500   03   FILLER REDEFINES W-FEL-4.                                         
010600     05  FEL-4           PIC X(17) OCCURS 2.                              
010700                                                                          
010800   03    W-FEL-5.                                                         
010900     05  FILLER          PIC X(18)   VALUE 'ERSÄTTANDE ARTIKEL'.          
011000     05  FILLER          PIC X(18)   VALUE 'SUPERSEDING PART  '.          
011100   03   FILLER REDEFINES W-FEL-5.                                         
011200     05  FEL-5           PIC X(18) OCCURS 2.                              
011300                                                                          
011400   03    W-FEL-6.                                                         
011500     05  FILLER        PIC X(19)   VALUE 'OBEHÖRIG ANVÄNDARE '.           
011600     05  FILLER        PIC X(19)   VALUE 'USER NOT AUTHORIZED'.           
011700   03   FILLER REDEFINES W-FEL-6.                                         
011800     05  FEL-6         PIC X(19) OCCURS 2.                                
011900                                                                          
012000   03    W-FEL-7.                                                         
012100     05  FILLER PIC X(40) VALUE 'SÖKT ARTIKELNR EJ REGISTRERAT'.          
012200     05  FILLER PIC X(40) VALUE 'KEY PART NO. NOT REGISTERED'.            
012300   03   FILLER REDEFINES W-FEL-7.                                         
012400     05  FEL-7         PIC X(40) OCCURS 2.                                
012500     EJECT                                                                
012600*                        ****    PARAMETRAR TILL W005INIT                 
012700*        -COPY WMSGINIT                                                   
012800     SKIP3                                                                
012900*                        ****    TP-AREOR                                 
013000 01      TP-WS.                                                           
013100   03    FILLER          PIC X(16)   VALUE '   TP-AREOR    '.             
013200     SKIP3                                                                
013300*01      MID -COPY W1I10301 -PRE MID-.                                    
013400     SKIP3                                                                
013500*01      -COPY WMSGAREA                                                   
013600     SKIP3                                                                
013700*  03    MOD -COPY W1O10301 -PRE MOD- -RED MSG-AREA.                      
013800     EJECT                                                                
013900*01  -COPY WMFSAREA.                                                      
014000     EJECT                                                                
014100******************************************************************        
014200*****                                                                     
014300*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014400*****                                                                     
014500 01  IMS-WS.                                                              
014600   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
014700     SKIP3                                                                
014800*****                    **** STATUS-KOD FRÅN IMS                         
014900   03    STATUS-WS       PIC XX.                                          
015000         88  SEGMENT-FINNS       VALUE '  '.                              
015100         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
015200     SKIP3                                                                
015300   03    GODK-STATUSKODER.                                                
015400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015500     SKIP3                                                                
015600 01      SSA1            PIC X(64).                                       
015700 01      SSA2            PIC X(64).                                       
015800     EJECT                                                                
015900*                            IMS FUNKTIONSKODER                           
016000*01      -COPY W0003                                                      
016100     EJECT                                                                
016200*                            DLI INPUT-OUTPUT AREA                        
016300 01  DLI-IO-AREA.                                                         
016400     03  IO-AREA         PIC X(928)  VALUE SPACE.                         
016500     SKIP3                                                                
016600*    03  WLARTC01 -COPY WDK601               -RED IO-AREA.                
016700     EJECT                                                                
016800*    03  WLARTC11 -COPY WDK611               -RED IO-AREA.                
016900     EJECT                                                                
017000*    03  WLBENA11 -COPY WDD311 -PRE BENA-    -RED IO-AREA.                
017100     EJECT                                                                
017200     03  FILLER REDEFINES IO-AREA.                                        
017300*        05  WLERSA11 -COPY WDD702 -PRE TILLK-.                           
017400     EJECT                                                                
017500*        05  WLERSA01 -COPY WDD701 -PRE ERSATT-.                          
017600     EJECT                                                                
017700 LINKAGE SECTION.                                                         
017800*01  -COPY W0009     -PRE MSG-                                            
017900     SKIP3                                                                
018000*01  -COPY W0008     -PRE USEA-                                           
018100         05  FILLER           PIC X.                                      
018200     SKIP3                                                                
018300*01  -COPY W0008     -PRE WLARTC-                                         
018400         05  FILLER           PIC X.                                      
018500     SKIP3                                                                
018600*01  -COPY W0008     -PRE WLERSA-                                         
018700         05  FILLER           PIC X.                                      
018800     EJECT                                                                
018900*01  -COPY W0008     -PRE WLBENA-                                         
019000         05  FILLER           PIC X.                                      
019100     EJECT                                                                
019200 PROCEDURE DIVISION USING MSG-PCB USEA-PCB   WLARTC-PCB                   
019300                                  WLERSA-PCB WLBENA-PCB.                  
019400                                                                          
019500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB   WLARTC-PCB                  
019600                                   WLERSA-PCB WLBENA-PCB.                 
019700     SKIP1                                                                
019800     PERFORM IMS-GET-MSG                                                  
019900     SKIP1                                                                
020000     IF SEGMENT-FINNS                                                     
020100       PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                                
020200                                                                          
020300       IF IDARTNR-WS NUMERIC                                              
020400                                                                          
020500         PERFORM S1-SECURITY-CHECK-PARTNO-IDLEV                           
020600         IF PASSED-SECURITY-CHECK                                         
020700                                                                          
020800           PERFORM B-RED-BILD                                             
020900         END-IF                                                           
021000       ELSE                                                               
021100         MOVE FEL-1 (SPIND) TO MOD-MESSAGE                                
021200       END-IF                                                             
021300     END-IF                                                               
021400     SKIP1                                                                
021500     PERFORM IMS-INSERT-MSG                                               
021600     SKIP1                                                                
021700     MOVE ZERO TO RETURN-CODE                                             
021800     GOBACK                                                               
021900     CONTINUE.                                                            
022000     EJECT                                                                
022100 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
022200******************************************************************        
022300*                                                                *        
022400*                                                                *        
022500*                                                                *        
022600******************************************************************        
022700     SKIP1                                                                
022800     IF MSG-DUBBLA-TRANSKODER                                             
022900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I10301                 
023200     ELSE                                                                 
023300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I10301                  
023600     END-IF                                                               
023700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
023800     MOVE '001'             TO MSGI-KDCALL                                
023900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024100     MOVE '1103'            TO MSGI-IDTRANS                               
024200     IF MFS-IDTRANS = '1103'                                              
024300     OR (MID-IDARTNR-IN NUMERIC                                           
024400     AND MID-IDARTNR-IN > ZERO)                                           
024500         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
024600     END-IF                                                               
024700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024800     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
024900     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
025000     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
025100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
025200     SKIP1                                                                
025300     MOVE LOW-VALUE TO MOD-AREA-OUTPUT                                    
025400     MOVE 'W1O103N1' TO MFS-IDMOD                                         
025500     MOVE '1103' TO MOD-IDTRANS                                           
025600     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
025700                                                                          
025800     IF MSGI-IDLAND-SPR = SPACE                                           
025900       IF MSGI-IDSPRAK = SPACE                                            
026000         IF SWEDISH-TEXT                                                  
026100           MOVE +1 TO SPIND                                               
026200         ELSE                                                             
026300           MOVE +2 TO SPIND                                               
026400         END-IF                                                           
026500       ELSE                                                               
026600         IF MSGI-IDSPRAK = 'SV'                                           
026700           MOVE +1 TO SPIND                                               
026800         ELSE                                                             
026900           MOVE +2 TO SPIND                                               
027000         END-IF                                                           
027100       END-IF                                                             
027200     ELSE                                                                 
027300       IF MSGI-IDLAND-SPR = 'SE'                                          
027400         MOVE +1 TO SPIND                                                 
027500       ELSE                                                               
027600         MOVE +2 TO SPIND                                                 
027700       END-IF                                                             
027800     END-IF                                                               
027900                                                                          
028000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
028100                             MOD-BLAEDRING-IDARTNR                        
028200                             MOD-MESSAGE                                  
028300                             MOD-MESSAGE-BOTTOM                           
028400     MOVE 1 TO IX                                                         
028500     PERFORM UNTIL                                                        
028600      ( IX > MAX-ANT-BILD-RADER )                                         
028700       MOVE MFS-RENSA-FAELT TO MOD-RADER (IX)                             
028800       ADD 1 TO IX                                                        
028900     END-PERFORM                                                          
029000     SKIP1                                                                
029100     IF  MFS-IDTRANS     = '1103'                                         
029200       MOVE MID-BLAEDRING-IDARTNR TO W-ERSATT-IDARTNR                     
029300     ELSE                                                                 
029400       MOVE ZERO TO W-ERSATT-IDARTNR                                      
029500     END-IF                                                               
029600     SUBTRACT 1 FROM W-ERSATT-IDARTNR                                     
029700     CONTINUE.                                                            
029800     EJECT                                                                
029900 B-RED-BILD      SECTION.                                                 
030000******************************************************************        
030100*                                                                *        
030200*    REDIGERING AV BILD MED DATA FRÅN WDD7 WDK6 WDD3             *        
030300*                                                                *        
030400******************************************************************        
030500     SKIP1                                                                
030600     MOVE IDARTNR-WS TO W-IDARTNR                                         
030700     PERFORM IMS-GET-ERSATT-INFO                                          
030800     IF SEGMENT-FINNS                                                     
030900       MOVE 1 TO IX                                                       
031000       SKIP1                                                              
031100       PERFORM UNTIL                                                      
031200        NOT ( SEGMENT-FINNS AND IX NOT > MAX-ANT-BILD-RADER )             
031300         IF ERSATT-IDARTNR > W-ERSATT-IDARTNR                             
031400           MOVE ERSATT-DIERS-ERS TO MOD-DIERS-ERS (IX)                    
031500           MOVE ERSATT-IDARTNR TO MOD-IDARTNR    (IX)                     
031600                                   W-ERSATT-IDARTNR                       
031700           IF ERSATT-KVKORT > 1                                           
031800             MOVE TEXT-KOMMENTAR-1 (SPIND) TO                             
031900                                MOD-TEXT-KOMMENTAR (IX)                   
032000           ELSE                                                           
032100             MOVE SPACE TO MOD-TEXT-KOMMENTAR (IX)                        
032200           END-IF                                                         
032300           MOVE TILLK-DIERS-TILLK TO MOD-DIERS-TILLK (IX)                 
032400                                                                          
032500           PERFORM IMS-GET-ARTC01-ERSATT                                  
                 IF SEGMENT-FINNS                                               
032600             PERFORM BA-UPPDAT-BILD-FRAN-WDK6                             
032700                                                                          
032800             IF SWEDISH-TEXT                                              
032900               MOVE 'S  ' TO W-IDSKYLT                                    
033000             ELSE                                                         
033100               MOVE 'GB ' TO W-IDSKYLT                                    
033200             END-IF                                                       
033300             PERFORM IMS-GET-BENA11                                       
033400             MOVE BENA-TEXT-BEART TO MOD-BEART (IX)                       
                 END-IF                                                         
033500           ADD 1 TO IX                                                    
033600         END-IF                                                           
033700         PERFORM IMS-GET-ERSATT-INFO                                      
033800       END-PERFORM                                                        
033900       IF IX > MAX-ANT-BILD-RADER AND SEGMENT-FINNS                       
034000         MOVE ERSATT-IDARTNR TO MOD-BLAEDRING-IDARTNR                     
034100         MOVE TEXT-KOMMENTAR-2 (SPIND) TO MOD-MESSAGE-BOTTOM              
034200       END-IF                                                             
034300     ELSE                                                                 
034400       MOVE FEL-2 (SPIND) TO MOD-MESSAGE                                  
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 BA-UPPDAT-BILD-FRAN-WDK6 SECTION.                                        
034900******************************************************************        
035000*                                                                *        
035100*    REDIGERING AV BILD MED DATA FRÅN WDK6                       *        
035200*                                                                *        
035300******************************************************************        
035400                                                                          
035500     MOVE ART-KDERS-UTG TO MOD-KDERS-C1 (IX)                              
035600                                                                          
035700     PERFORM IMS-GET-ARTC11                                               
035800     IF  SEGMENT-FINNS                                                    
035900       MOVE CLAG-KDERS TO MOD-KDERS-C1 (IX)                               
036000     END-IF                                                               
036100     CONTINUE.                                                            
036200     EJECT                                                                
036300                                                                          
036400 S1-SECURITY-CHECK-PARTNO-IDLEV SECTION.                                  
036500     SKIP2                                                                
036600*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
036700     MOVE IDARTNR-WS  TO W-IDARTNR                                        
036800     PERFORM IMS-GET-ARTC01-ERSATTANDE                                    
036900     IF  SEGMENT-FINNS                                                    
037000       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
037100       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
037200       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
037300*        --- BEHÖRIG USER                                                 
037400         SET PASSED-SECURITY-CHECK TO TRUE                                
037500       ELSE                                                               
037600*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
037700         MOVE FEL-6 (SPIND) TO MOD-MESSAGE                                
037800         SET BLOCKED-SECURITY-CHECK TO TRUE                               
037900       END-IF                                                             
038000     ELSE                                                                 
038100*      --- ERSÄTTANDE ARTIKEL SAKNAS PÅ WDK6                              
038200         MOVE FEL-7 (SPIND) TO MOD-MESSAGE                                
038300     END-IF                                                               
038400     .                                                                    
038500     EJECT                                                                
038600                                                                          
038700* IMS SEKTIONER                                                           
038800     SKIP3                                                                
038900 IMS-GET-MSG SECTION.                                                     
039000     SKIP2                                                                
039100     MOVE '  QC' TO GODK-STATUSKODER                                      
039200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     CONTINUE.                                                            
039600     SKIP3                                                                
039700 IMS-INSERT-MSG SECTION.                                                  
039800     SKIP2                                                                
039900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
040000       MOVE '0' TO MFS-KDHUVOMR                                           
040100     END-IF                                                               
040200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040300     MOVE SPACE TO GODK-STATUSKODER                                       
040400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
040500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     CONTINUE.                                                            
040800     EJECT                                                                
040900 IMS-GET-ARTC01-ERSATTANDE SECTION.                                       
041000     SKIP2                                                                
041100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
041200            DELIMITED BY SIZE INTO SSA1                                   
041300     MOVE '  GE' TO GODK-STATUSKODER                                      
041400     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA SSA1                    
041500     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
041600     PERFORM IMS-STATUSKONTROLL                                           
041700     CONTINUE.                                                            
041800     SKIP3                                                                
041900 IMS-GET-ARTC01-ERSATT SECTION.                                           
042000     SKIP2                                                                
042100     STRING 'WLARTC01(IDARTNR  =' W-ERSATT-IDARTNR-X ')'                  
042200            DELIMITED BY SIZE INTO SSA1                                   
042300     MOVE '  GE' TO GODK-STATUSKODER                                      
042400     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA SSA1                    
042500     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
042600     PERFORM IMS-STATUSKONTROLL                                           
042700     CONTINUE.                                                            
042800     SKIP3                                                                
042900 IMS-GET-ARTC11 SECTION.                                                  
043000     SKIP2                                                                
043100     MOVE 'WLARTC11 '  TO SSA1                                            
043200     MOVE '  GE' TO GODK-STATUSKODER                                      
043300     CALL CBLTDLI USING GNP WLARTC-PCB DLI-IO-AREA SSA1                   
043400     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     CONTINUE.                                                            
043700     EJECT                                                                
043800 IMS-GET-ERSATT-INFO SECTION.                                             
043900     SKIP2                                                                
044000     STRING 'WLERSA11*D(WDD7ASEQ =' W-IDARTNR-X ')'                       
044100            DELIMITED BY SIZE INTO SSA1                                   
044200     MOVE 'WLERSA01 ' TO SSA2                                             
044300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
044400     CALL CBLTDLI USING GN WLERSA-PCB DLI-IO-AREA SSA1 SSA2               
044500     MOVE WLERSA-STATUS-CODE TO STATUS-WS                                 
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     CONTINUE.                                                            
044800     SKIP3                                                                
044900 IMS-GET-BENA11 SECTION.                                                  
045000     SKIP2                                                                
045100     STRING 'WLBENA01(WDD3BSEQ =' W-ERSATT-IDARTNR-X ')'                  
045200            DELIMITED BY SIZE INTO SSA1                                   
045300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
045400            DELIMITED BY SIZE INTO SSA2                                   
045500     MOVE '  ' TO GODK-STATUSKODER                                        
045600     CALL CBLTDLI USING GU WLBENA-PCB DLI-IO-AREA SSA1 SSA2               
045700     MOVE WLBENA-STATUS-CODE TO STATUS-WS                                 
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     CONTINUE.                                                            
046000     EJECT                                                                
046100 IMS-STATUSKONTROLL SECTION.                                              
046200     SET STATUS-IX TO 1                                                   
046300     SEARCH GODK-STATUS                                                   
046400       AT END                                                             
046500         CALL FELLOG                                                      
046600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
046700     END-SEARCH                                                           
046800     CONTINUE                                                             
046900            CONTINUE.                                                     
047000 IMS-STATUS-KONTROLL-EXIT. EXIT.                                          
047100     CONTINUE.                                                            
