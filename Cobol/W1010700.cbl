000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1010700.                                                
000300 AUTHOR.         KARL JOHAN HANSSON.                                      
000400 DATE-WRITTEN.   FEBR  82.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION.                                                            
000900*        CROSS-INDEX.  ( LEVERANTÖRENS ID, MOT IDARTNR )                  
001000                                                                          
001100                                                                          
001200                                                                          
001300                                                                          
001400*    INDATA.                                                              
001500*        TRANSAKTION: W1T107                                              
001600*        MID:         W1I10701                                            
001700                                                                          
001800*    UTDATA.                                                              
001900*        MOD:         W1O10701                                            
002000*                                                                         
002100*   ÄNDRINGAR:                                                            
002200*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002300*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002400*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002500*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002600*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W1010700'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  IDLEVNR-WS                  PIC X(5)    VALUE SPACE.                 
003900 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
004000 77  BELEV-WS                    PIC X(30).                               
004100 77  IDARTNR-SPAR                PIC S9(9)              COMP-3.           
004200 77  KDFTAG-SPAR                 PIC S9(1)              COMP-3.           
004300 77  SPAR-STATUS-WS              PIC X(2).                                
004400 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  SPIND                       PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  MAX-LINE                    PIC S9(9)   VALUE +14  COMP SYNC.        
004700 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +876 COMP SYNC.        
004800     SKIP2                                                                
004900                                                                          
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
005200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500                                                                          
005600 01  STARTFLAGGA.                                                         
005700     03  FL-BORJA-SIDA1          PIC X       VALUE 'N'.                   
005800         88  BORJA-SIDA1                     VALUE 'J'.                   
005900     EJECT                                                                
006000 01  NYCKLAR-TILL-DLI.                                                    
006100   03  W-WDF5ASEQ-X.                                                      
006200     05  W-IDLEVNR-X.                                                     
006300       07  W-IDLEVNR             PIC X(5)   VALUE LOW-VALUE.              
006400     05  W-IDLEVART              PIC X(30)   VALUE LOW-VALUE.             
006500                                                                          
006600   03  W-WDF5ASEQ-MAX-X.                                                  
006700     05  W-IDLEVNR-MAX           PIC X(5)    VALUE HIGH-VALUE.            
006800     05  FILLER                  PIC X(30)   VALUE HIGH-VALUE.            
006900                                                                          
007000   03  W-IDARTNR-X.                                                       
007100     05  W-IDARTNR               PIC S9(9)   COMP-3 VALUE ZERO.           
007200     SKIP3                                                                
007300 01  MEDDELANDE.                                                          
007400   03  W-FEL-1.                                                           
007500     05  FILLER                  PIC X(40)   VALUE                        
007600             'LEVERANTÖR-ID FELAKTIGT INMATAT'.                           
007700     05  FILLER                  PIC X(40)   VALUE                        
007800             'SUPPLIER ID IS NOT ENTERED CORRECTLY'.                      
007900   03   FILLER REDEFINES W-FEL-1.                                         
008000     05  FEL-1                   PIC X(40) OCCURS 2.                      
008100                                                                          
008200   03  W-FEL-2.                                                           
008300     05  FILLER        PIC X(21)   VALUE 'OBEHÖRIG ANVÄNDARE '.           
008400     05  FILLER        PIC X(21)   VALUE 'USER NOT AUTHORIZED'.           
008500   03   FILLER REDEFINES W-FEL-2.                                         
008600     05  FEL-2         PIC X(21) OCCURS 2.                                
008700                                                                          
008800   03  W-MED-1.                                                           
008900     05  FILLER                  PIC X(40)   VALUE                        
009000             'LEVERANTÖR MED DENNA IDENTITET FINNS EJ'.                   
009100     05  FILLER                  PIC X(40)   VALUE                        
009200             '      NO SUPPLIER WITH THIS IDENTITY  '.                    
009300   03   FILLER REDEFINES W-MED-1.                                         
009400     05  MED-1                   PIC X(40) OCCURS 2.                      
009500                                                                          
009600   03  W-MED-2.                                                           
009700     05  FILLER                  PIC X(32)   VALUE                        
009800             'TRYCK ENTER FÖR MER INFORMATION '.                          
009900     05  FILLER                  PIC X(32)   VALUE                        
010000             'FOR MORE INFORMATION PRESS ENTER'.                          
010100   03   FILLER REDEFINES W-MED-2.                                         
010200     05 MED-2                    PIC X(32) OCCURS 2.                      
010300     EJECT                                                                
010400******************************************************************        
010500*                                                                         
010600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
010900     SKIP3                                                                
011000*                        ****    PARAMETRAR TILL W005INIT                 
011100*01      -COPY WMSGINIT                                                   
011200     SKIP3                                                                
011300                                                                          
011400*01  MID -COPY W1I10701 -PRE MID-.                                        
011500     EJECT                                                                
011600                                                                          
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900                                                                          
012000*  03  MOD -COPY W1O10701 -PRE MOD- -RED MSG-AREA.                        
012100     EJECT                                                                
012200                                                                          
012300*01  -COPY WMFSAREA                                                       
012400     EJECT                                                                
012500                                                                          
012600******************************************************************        
012700*                                                                         
012800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012900*                                                                         
013000 01  IMS-WS.                                                              
013100   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
013200     SKIP3                                                                
013300*                        **** STATUS-KOD FRÅN IMS                         
013400   03  STATUS-WS                 PIC XX.                                  
013500     88  SEGMENT-FINNS                       VALUE '  '.                  
013600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013700     SKIP3                                                                
013800   03  GODK-STATUSKODER.                                                  
013900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000     SKIP3                                                                
014100 01    SSA1                      PIC X(128).                              
014200 01    SSA2                      PIC X(64).                               
014300     EJECT                                                                
014400*                            IMS FUNKTIONSKODER                           
014500*01    -COPY W0003                                                        
014600     EJECT                                                                
014700*                            DLI INPUT-OUTPUT AREA                        
014800 01  DLI-IO-AREA.                                                         
014900   03  IO-AREA                   PIC X(200)  VALUE SPACE.                 
015000     SKIP3                                                                
015100*  03  WDF501   -COPY WDF501 -RED IO-AREA.                                
015200     EJECT                                                                
015300*  03  WDF502   -COPY WDF502 -RED IO-AREA.                                
015400     EJECT                                                                
015500*                            DLI INPUT-OUTPUT AREA-2                      
015600 01  DLI-IO-AREA-2.                                                       
015700   03  IO-AREA-2                 PIC X(928)  VALUE SPACE.                 
015800     SKIP3                                                                
015900*  03  WLARTC01 -COPY WDK601               -RED IO-AREA-2.                
016000*  03  WLARTC11 -COPY WDK611               -RED IO-AREA-2.                
016100     EJECT                                                                
016200 LINKAGE SECTION.                                                         
016300*01  -COPY W0009     -PRE MSG-                                            
016400     EJECT                                                                
016500*01  -COPY W0008     -PRE USEA-                                           
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008     -PRE WDF5-                                           
016900     05  WDF5-IDLEVNR            PIC X(5).                                
017000     05  WDF5-IDLEVART           PIC X(30).                               
017100     EJECT                                                                
017200*01  -COPY W0008     -PRE ARTC-                                           
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDF5-PCB ARTC-PCB.             
017600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDF5-PCB ARTC-PCB.            
017700                                                                          
017800     PERFORM IMS-GET-MSG                                                  
017900     IF SEGMENT-FINNS                                                     
018000       PERFORM A-INIT-SPARA-INPUT                                         
018100                                                                          
018200       MOVE IDLEVNR-WS           TO WS-IDLEVNR-8                          
018300       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
018400       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
018500*         --- BEHÖRIG USER                                                
018600          IF IDLEVNR-WS(1:1) = ' ' OR '+' OR '0'                          
018700             MOVE FEL-1 (SPIND) TO MOD-MESSAGE-RAD1                       
018800          ELSE                                                            
018900             IF BORJA-SIDA1                                               
019000                PERFORM B-LAS-FOERSTA-SIDA-RAD1                           
019100                IF XLEV-BELEVART NOT = BELEV-WS AND SEGMENT-FINNS         
019200                   PERFORM D-LAS-RESTERANDE-RADER                         
019300                END-IF                                                    
019400             ELSE                                                         
019500                PERFORM C-LAS-NESTA-SIDA-RAD1                             
019600                PERFORM D-LAS-RESTERANDE-RADER                            
019700             END-IF                                                       
019800          END-IF                                                          
019900       ELSE                                                               
020000*         --- EJ BEHÖRIG USER                                             
020100          PERFORM UNTIL MOD-IX-LINE  >  MAX-LINE                          
020200             MOVE MFS-RENSA-FAELT TO MOD-BELEV (MOD-IX-LINE)              
020300             SET MOD-IX-LINE UP BY +1                                     
020400          END-PERFORM                                                     
020500          MOVE FEL-2 (SPIND) TO MOD-MESSAGE-RAD1                          
020600       END-IF                                                             
020700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
020800       PERFORM IMS-INSERT-MSG                                             
020900     END-IF                                                               
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     CONTINUE.                                                            
021300     EJECT                                                                
021400 A-INIT-SPARA-INPUT SECTION.                                              
021500                                                                          
021600     MOVE NEJ TO FL-BORJA-SIDA1                                           
021700     IF MSG-DUBBLA-TRANSKODER                                             
021800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I10701                 
021900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022100       MOVE JA TO FL-BORJA-SIDA1                                          
022200     ELSE                                                                 
022300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I10701                  
022400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
022500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022600     END-IF                                                               
022700                                                                          
022800* WMSGINIT -  USER-BASEN                                                  
022900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
023000     MOVE '001'             TO MSGI-KDCALL                                
023100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023300     MOVE '1107'            TO MSGI-IDTRANS                               
023400                                                                          
023500     IF ( MID-IDLEVNR-IN NOT = ALL '+' AND SPACE )                        
023600         MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                              
023700         MOVE JA TO FL-BORJA-SIDA1                                        
023800     END-IF                                                               
023900                                                                          
024000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024100                                                                          
024200     MOVE MSGI-IDLEVNR TO IDLEVNR-WS                                      
024300*                                                                         
024400                                                                          
024500     PERFORM AA-INIT-BELEV                                                
024600                                                                          
024700     MOVE LOW-VALUE       TO MSG-AREA                                     
024800     MOVE 'W1O107N1'      TO MFS-IDMOD                                    
024900     MOVE '1107'          TO MOD-TRANS-NUMMER                             
025000                                                                          
025100     MOVE IDLEVNR-WS      TO MOD-IDLEVNR-UT                               
025200                                                                          
025300     MOVE BELEV-WS        TO MOD-BELEV-UT                                 
025400                                                                          
025500     IF MSGI-IDLAND-SPR = SPACE                                           
025600       IF MSGI-IDSPRAK = SPACE                                            
025700         IF SWEDISH-TEXT                                                  
025800           MOVE +1 TO SPIND                                               
025900         ELSE                                                             
026000           MOVE +2 TO SPIND                                               
026100         END-IF                                                           
026200       ELSE                                                               
026300         IF MSGI-IDSPRAK = 'SV'                                           
026400           MOVE +1 TO SPIND                                               
026500         ELSE                                                             
026600           MOVE +2 TO SPIND                                               
026700         END-IF                                                           
026800       END-IF                                                             
026900     ELSE                                                                 
027000       IF MSGI-IDLAND-SPR = 'SE'                                          
027100         MOVE +1 TO SPIND                                                 
027200       ELSE                                                               
027300         MOVE +2 TO SPIND                                                 
027400       END-IF                                                             
027500     END-IF                                                               
027600                                                                          
027700     MOVE MFS-RENSA-FAELT TO MOD-MESSAGE-RAD1                             
027800                             MOD-IDLEVNR-IN                               
027900                             MOD-BELEV-IN                                 
028000                             MOD-BELEV-SPAR                               
028100                             MOD-IDARTNR-IN                               
028200                             MOD-IDARTNR-UT                               
028300                             MOD-MESSAGE-23                               
028400     CONTINUE.                                                            
028500     EJECT                                                                
028600 AA-INIT-BELEV SECTION.                                                   
028700                                                                          
028800     IF MFS-IDTRANS  NOT = '1107'                                         
028900       MOVE JA TO FL-BORJA-SIDA1                                          
029000     END-IF                                                               
029100                                                                          
029200     IF MID-IDLEVNR-IN = ALL '+'                                          
029300       IF MID-BELEV-IN = ALL '+'                                          
029400         MOVE MID-BELEV-UT TO BELEV-WS                                    
029500       ELSE                                                               
029600         MOVE MID-BELEV-IN TO BELEV-WS                                    
029700         MOVE JA TO FL-BORJA-SIDA1                                        
029800       END-IF                                                             
029900     ELSE                                                                 
030000       IF MID-BELEV-IN = ALL '+'                                          
030100         MOVE LOW-VALUE TO BELEV-WS                                       
030200       ELSE                                                               
030300         MOVE MID-BELEV-IN TO BELEV-WS                                    
030400       END-IF                                                             
030500     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 B-LAS-FOERSTA-SIDA-RAD1 SECTION.                                         
030900                                                                          
031000     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
031100                        W-IDLEVNR-MAX                                     
031200     CALL W009REDU USING BELEV-WS W-IDLEVART                              
031300     PERFORM IMS-GET-LEV-FIRST                                            
031400     IF SEGMENT-FINNS                                                     
031500       SET MOD-IX-LINE TO +1                                              
031600        MOVE XART-KDFTAG       TO KDFTAG-SPAR                             
031700        MOVE XART-IDARTNR      TO IDARTNR-SPAR                            
031800       PERFORM IMS-GET-BENAEMN                                            
031900       IF XLEV-BELEVART = BELEV-WS                                        
032000          MOVE IDARTNR-SPAR   TO MOD-IDARTNR-UT                           
032100          MOVE MFS-ADD-LYS-UPP-FAELT                                      
032200                              TO MOD-BELEV-ATTR (MOD-IX-LINE)             
032300       END-IF                                                             
032400       MOVE XLEV-BELEVART      TO MOD-BELEV (MOD-IX-LINE)                 
032500       MOVE KDFTAG-SPAR        TO MOD-KDFTAG (MOD-IX-LINE)                
032600       MOVE XLEV-FLTLVM        TO MOD-FLTLVM (MOD-IX-LINE)                
032700       MOVE XLEV-IDBENR        TO MOD-IDBENR (MOD-IX-LINE)                
032800       MOVE IDARTNR-SPAR       TO MOD-IDARTNR (MOD-IX-LINE)               
032900                                                                          
033000       MOVE STATUS-WS TO SPAR-STATUS-WS                                   
033100       MOVE IDARTNR-SPAR TO W-IDARTNR                                     
033200       PERFORM IMS-GET-ARTC01                                             
033300       IF SEGMENT-FINNS                                                   
033400         PERFORM IMS-GET-ARTC11                                           
033500         IF SEGMENT-FINNS                                                 
033600           MOVE CLAG-FLLSRDEL TO MOD-FLLSRDEL(MOD-IX-LINE)                
033700         ELSE                                                             
033800           MOVE MFS-RENSA-FAELT TO MOD-FLLSRDEL(MOD-IX-LINE)              
033900         END-IF                                                           
034000       ELSE                                                               
034100         MOVE NEJ TO MOD-FLLSRDEL(MOD-IX-LINE)                            
034200       END-IF                                                             
034300       MOVE SPAR-STATUS-WS TO STATUS-WS                                   
034400       SET MOD-IX-LINE UP BY +1                                           
034500     ELSE                                                                 
034600       MOVE MED-1 (SPIND) TO MOD-MESSAGE-RAD1                             
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 C-LAS-NESTA-SIDA-RAD1 SECTION.                                           
035100                                                                          
035200     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
035300                        W-IDLEVNR-MAX                                     
035400     MOVE MID-BELEV-SPAR TO W-IDLEVART                                    
035500     PERFORM IMS-GET-LEV-FIRST                                            
035600     IF SEGMENT-FINNS                                                     
035700        MOVE XART-KDFTAG       TO KDFTAG-SPAR                             
035800        MOVE XART-IDARTNR      TO IDARTNR-SPAR                            
035900       PERFORM IMS-GET-BENAEMN                                            
036000     END-IF                                                               
036100     SET MOD-IX-LINE TO +1                                                
036200     .                                                                    
036300     EJECT                                                                
036400 D-LAS-RESTERANDE-RADER SECTION.                                          
036500                                                                          
036600     PERFORM UNTIL MOD-IX-LINE > MAX-LINE                                 
036700       IF SEGMENT-FINNS                                                   
036800         PERFORM IMS-GET-BENAEMN                                          
036900         IF SEGMENT-FINNS                                                 
037000           IF XLEV-BELEVART = BELEV-WS                                    
037100              MOVE IDARTNR-SPAR   TO MOD-IDARTNR-UT                       
037200              MOVE MFS-ADD-LYS-UPP-FAELT                                  
037300                                  TO MOD-BELEV-ATTR(MOD-IX-LINE)          
037400           END-IF                                                         
037500                                                                          
037600           MOVE XLEV-BELEVART      TO MOD-BELEV (MOD-IX-LINE)             
037700           MOVE KDFTAG-SPAR        TO MOD-KDFTAG (MOD-IX-LINE)            
037800           MOVE XLEV-FLTLVM        TO MOD-FLTLVM (MOD-IX-LINE)            
037900           MOVE XLEV-IDBENR        TO MOD-IDBENR (MOD-IX-LINE)            
038000           MOVE IDARTNR-SPAR       TO MOD-IDARTNR (MOD-IX-LINE)           
038100                                                                          
038200           MOVE STATUS-WS TO SPAR-STATUS-WS                               
038300           MOVE IDARTNR-SPAR TO W-IDARTNR                                 
038400           PERFORM IMS-GET-ARTC01                                         
038500           IF SEGMENT-FINNS                                               
038600             PERFORM IMS-GET-ARTC11                                       
038700             IF SEGMENT-FINNS                                             
038800               MOVE CLAG-FLLSRDEL TO MOD-FLLSRDEL(MOD-IX-LINE)            
038900             ELSE                                                         
039000               MOVE MFS-RENSA-FAELT TO MOD-FLLSRDEL(MOD-IX-LINE)          
039100             END-IF                                                       
039200           ELSE                                                           
039300             MOVE NEJ TO MOD-FLLSRDEL(MOD-IX-LINE)                        
039400           END-IF                                                         
039500           MOVE SPAR-STATUS-WS TO STATUS-WS                               
039600           SET MOD-IX-LINE UP BY +1                                       
039700         ELSE                                                             
039800           PERFORM IMS-GET-LEV-NEXT                                       
039900           PERFORM UNTIL SEGMENT-SAKNAS                                   
040000                      OR (XART-IDARTNR NOT = IDARTNR-SPAR )               
040100             PERFORM IMS-GET-LEV-NEXT                                     
040200           END-PERFORM                                                    
040300           IF SEGMENT-FINNS                                               
040400              MOVE XART-IDARTNR      TO IDARTNR-SPAR                      
040500              MOVE XART-KDFTAG       TO KDFTAG-SPAR                       
040600           END-IF                                                         
040700         END-IF                                                           
040800       ELSE                                                               
040900         MOVE MFS-RENSA-FAELT TO MOD-BELEV (MOD-IX-LINE)                  
041000         SET MOD-IX-LINE UP BY +1                                         
041100       END-IF                                                             
041200     END-PERFORM                                                          
041300     IF SEGMENT-FINNS                                                     
041400       MOVE MED-2 (SPIND) TO MOD-MESSAGE-23                               
041500       MOVE WDF5-IDLEVART TO MOD-BELEV-SPAR                               
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900* IMS SEKTIONER                                                           
042000     SKIP3                                                                
042100 IMS-GET-MSG SECTION.                                                     
042200     MOVE '  QC' TO GODK-STATUSKODER                                      
042300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
042400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     .                                                                    
042700     SKIP3                                                                
042800 IMS-INSERT-MSG SECTION.                                                  
042900     IF SWEDISH-TEXT                                                      
043000       MOVE '0' TO MFS-KDHUVOMR                                           
043100     END-IF                                                               
043200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
043300     MOVE SPACE TO GODK-STATUSKODER                                       
043400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
043500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     SKIP3                                                                
043900     EJECT                                                                
044000 IMS-GET-LEV-FIRST SECTION.                                               
044100     STRING 'WDF501  (WDF5ASEQ=>' W-WDF5ASEQ-X                            
044200            '&WDF5ASEQ=<' W-WDF5ASEQ-MAX-X ')'                            
044300            DELIMITED BY SIZE INTO SSA1                                   
044400     MOVE '  GE' TO GODK-STATUSKODER                                      
044500     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
044600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     SKIP3                                                                
045000 IMS-GET-LEV-NEXT SECTION.                                                
045100     STRING 'WDF501  (WDF5ASEQ=<' W-WDF5ASEQ-MAX-X ')'                    
045200            DELIMITED BY SIZE INTO SSA1                                   
045300     MOVE '  GE' TO GODK-STATUSKODER                                      
045400     CALL CBLTDLI USING GN WDF5-PCB DLI-IO-AREA SSA1                      
045500     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     .                                                                    
045800     SKIP3                                                                
045900 IMS-GET-BENAEMN SECTION.                                                 
046000     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
046100            DELIMITED BY SIZE INTO SSA1                                   
046200     MOVE '  GE' TO GODK-STATUSKODER                                      
046300     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
046400     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     .                                                                    
046700     EJECT                                                                
046800 IMS-GET-ARTC01 SECTION.                                                  
046900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
047000            DELIMITED BY SIZE INTO SSA1                                   
047100     MOVE '  GE' TO GODK-STATUSKODER                                      
047200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
047300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047400     PERFORM IMS-STATUSKONTROLL                                           
047500     .                                                                    
047600     SKIP3                                                                
047700 IMS-GET-ARTC11 SECTION.                                                  
047800     MOVE 'WLARTC11 ' TO SSA1                                             
047900     MOVE '  GE' TO GODK-STATUSKODER                                      
048000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-2 SSA1                   
048100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
048200     PERFORM IMS-STATUSKONTROLL                                           
048300     .                                                                    
048400     EJECT                                                                
048500 IMS-STATUSKONTROLL SECTION.                                              
048600     SET STATUS-IX TO 1                                                   
048700     SEARCH GODK-STATUS                                                   
048800       AT END                                                             
048900         CALL FELLOG                                                      
049000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
049100     END-SEARCH                                                           
049200     .                                                                    
