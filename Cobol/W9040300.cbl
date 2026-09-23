000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W9040300.                                                 
000300 AUTHOR.        IDK, GÖTEBORG.                                            
000400 DATE-WRITTEN.  MAJ  -79.                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000800*                ERSÄTTNINGAR. ANGIVEN ARTIKEL 'ERSÄTTES AV'              
000900*    INDATA.                                                              
001000*        TRANSAKTION: W90403T                                             
001100*        MID:         W90403I1                                            
001200*    UTDATA.                                                              
001300*        MOD:         W90403O1                                            
001400*    SUBPROGRAM.                                                          
001500*        FELLOG                                                           
001600*                                                                         
001700*   ÄNDRINGAR:                                                            
001800*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
001900*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002000*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002100*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002200*                                                                         
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP3                                                                
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77      IDPGM           PIC X(8)    VALUE 'W9040300'.                    
003200 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
003300 77      NUM-FAELT       PIC 9(3)    VALUE ZERO.                          
003400                                                                          
003500 77      JA              PIC X       VALUE 'J'.                           
003600 77      NEJ             PIC X       VALUE 'N'.                           
003700 77      STRECK          PIC X       VALUE '-'.                           
003800                                                                          
003900 77      MAX-ANT-BILD-RADER                                               
004000                         PIC S9(9)   VALUE +10   COMP SYNC.               
004100 77      MAX-ANT-BILD-KOLUMNER                                            
004200                         PIC S9(9)   VALUE +3    COMP SYNC.               
004300 77      W-KDERS         PIC S9(3)               COMP-3.                  
004400 77      WS-IDLEVNR-8    PIC  X(8)   VALUE SPACE.                         
004500                                                                          
004600 77  SPIND                PIC S9(9)   VALUE +0    COMP SYNC.              
004700                                                                          
004800                                                                          
004900 01    DYNAMISKA-SUBPROGRAM.                                              
005000   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
005100   03    W009KSIF        PIC X(8)    VALUE 'W009KSIF'.                    
005200   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
005300   03    W005INIT        PIC X(8)    VALUE 'W005INIT'.                    
005400     SKIP2                                                                
005500 01  W009KSIF-PARM.                                                       
005600     03  KS-IDARTNR      PIC 9(9).                                        
005700     03  KS-LGD          PIC 9       VALUE 9.                             
005800     03  KS-SIFF         PIC 9.                                           
005900     SKIP2                                                                
006000 01      W-IDARTNR-X.                                                     
006100   03    W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
006200     SKIP2                                                                
006300 01      SWITCHAR.                                                        
006400   03    SW-NYCKLAR-OK   PIC X       VALUE 'N'.                           
006500     SKIP2                                                                
006600 01  W.                                                                   
006700     03  IX                  PIC S9(9)               COMP SYNC.           
006800     03  IY                  PIC S9(9)               COMP SYNC.           
006900     03  IY-PL1              PIC S9(9)               COMP SYNC.           
007000     03  IZ                  PIC S9(9)               COMP SYNC.           
007100     SKIP2                                                                
007200     03  W-BLAEDRING-ANT     PIC S9(3)               COMP-3.              
007300     EJECT                                                                
007400 01      MEDDELANDEN.                                                     
007500   03    W-TEXT-KOMMENTAR-1.                                              
007600     05  FILLER     PIC X(32) VALUE                                       
007700                          'FLER TILLKOMMANDE ARTIKLAR FINNS'.             
007800     05  FILLER     PIC X(32) VALUE                                       
007900                          '  SEE MORE SUPERSEDING PARTS    '.             
008000   03    FILLER REDEFINES W-TEXT-KOMMENTAR-1.                             
008100     05  TEXT-KOMMENTAR-1      PIC X(32) OCCURS 2.                        
008200     SKIP3                                                                
008300 01  MEDDELANDE.                                                          
008400   03    W-FEL-1.                                                         
008500     05  FILLER          PIC X(26)   VALUE                                
008600                      'ARTIKELNUMMER EJ NUMERISKT'.                       
008700     05  FILLER          PIC X(26)   VALUE                                
008800                      'PARTNUMBER NOT NUMERIC    '.                       
008900   03   FILLER REDEFINES W-FEL-1.                                         
009000     05  FEL-1           PIC X(26) OCCURS 2.                              
009100                                                                          
009200   03    W-FEL-2.                                                         
009300     05  FILLER          PIC X(35)   VALUE                                
009400                                  'ARTIKEL ERSÄTTES EJ'.                  
009500     05  FILLER          PIC X(35)   VALUE                                
009600                                  'PART-NO NOT SUPERSEDED'.               
009700   03   FILLER REDEFINES W-FEL-2.                                         
009800     05  FEL-2           PIC X(35) OCCURS 2.                              
009900                                                                          
010000   03    W-FEL-2A.                                                        
010100     05  FILLER          PIC X(35)   VALUE                                
010200                                  'ARTIKEL SAKNAS'.                       
010300     05  FILLER          PIC X(35)   VALUE                                
010400                                  'PART-NO MISSING'.                      
010500   03   FILLER REDEFINES W-FEL-2A.                                        
010600     05  FEL-2A           PIC X(35) OCCURS 2.                             
010700                                                                          
010800   03    W-FEL-3.                                                         
010900     05  FILLER          PIC X(15)   VALUE 'ARTIKELN ERSATT'.             
011000     05  FILLER          PIC X(15)   VALUE 'PART SUPERSEDED'.             
011100   03   FILLER REDEFINES W-FEL-3.                                         
011200     05  FEL-3           PIC X(15) OCCURS 2.                              
011300                                                                          
011400   03    W-FEL-4.                                                         
011500     05  FILLER          PIC X(23)   VALUE                                
011600                                 'ARTIKELN UTGÅR         '.               
011700     05  FILLER          PIC X(23)   VALUE                                
011800                                 'PART-NO WILL BE DELETED'.               
011900   03   FILLER REDEFINES W-FEL-4.                                         
012000     05  FEL-4           PIC X(23) OCCURS 2.                              
012100                                                                          
012200   03    W-FEL-5.                                                         
012300     05  FILLER        PIC X(19)   VALUE 'ARTIKELN HAR UTGÅTT'.           
012400     05  FILLER        PIC X(19)   VALUE 'PART-NO IS DELETED '.           
012500   03   FILLER REDEFINES W-FEL-5.                                         
012600     05  FEL-5         PIC X(19) OCCURS 2.                                
012700                                                                          
012800   03    W-FEL-6.                                                         
012900     05  FILLER        PIC X(19)   VALUE 'OBEHÖRIG ANVÄNDARE '.           
013000     05  FILLER        PIC X(19)   VALUE 'USER NOT AUTHORIZED'.           
013100   03   FILLER REDEFINES W-FEL-6.                                         
013200     05  FEL-6         PIC X(19) OCCURS 2.                                
013300     EJECT                                                                
013400*                        ****    PARAMETRAR TILL W005INIT                 
013500*01      -COPY WMSGINIT                                                   
013600     SKIP3                                                                
013700*                        ****    TP-AREOR                                 
013800 01      TP-WS.                                                           
013900   03    FILLER          PIC X(16)   VALUE '   TP-AREOR    '.             
014000     SKIP3                                                                
014100*01      MID -COPY W90403I1 -PRE MID-.                                    
014200     SKIP3                                                                
014300*01      -COPY WMSGAREA                                                   
014400     SKIP3                                                                
014500*  03    MOD -COPY W90403O1 -PRE MOD- -RED MSG-AREA.                      
014600     EJECT                                                                
014700*01  -COPY WMFSAREA.                                                      
014800     EJECT                                                                
014900******************************************************************        
015000*****                                                                     
015100*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015200*****                                                                     
015300 01  IMS-WS.                                                              
015400   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
015500     SKIP3                                                                
015600*****                    **** STATUS-KOD FRÅN IMS                         
015700   03    STATUS-WS       PIC XX.                                          
015800         88  SEGMENT-FINNS       VALUE '  '.                              
015900         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
016000     SKIP3                                                                
016100   03    GODK-STATUSKODER.                                                
016200     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
016300     SKIP3                                                                
016400 01      SSA1            PIC X(32).                                       
016500 01      SSA2            PIC X(32).                                       
016600 01      SSA3            PIC X(32).                                       
016700     EJECT                                                                
016800*                            IMS FUNKTIONSKODER                           
016900*01      -COPY W0003                                                      
017000     EJECT                                                                
017100*                            DLI INPUT-OUTPUT AREA                        
017200 01      FILLER.                                                          
017300 03      DLI-IO-AREA     PIC X(850)  VALUE SPACE.                         
017400     SKIP3                                                                
017500*03      WLARTC01 -COPY WDK601  -RED DLI-IO-AREA.                         
017600     EJECT                                                                
017700*03      WLARTC11 -COPY WDK611  -RED DLI-IO-AREA.                         
017800     EJECT                                                                
017900*03      WLERSA01 -COPY WDD701  -PRE ERSATT-  -RED DLI-IO-AREA.           
018000     EJECT                                                                
018100*03      WLERSA11 -COPY WDD702  -PRE TILLK-   -RED DLI-IO-AREA.           
018200     EJECT                                                                
018300*03      WLERSA11 -COPY WDD704  -PRE ERSINF-  -RED DLI-IO-AREA.           
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600*01  -COPY W0009     -PRE MSG-                                            
018700     EJECT                                                                
018800*01  -COPY W0008     -PRE USEA-                                           
018900         05  FILLER           PIC X.                                      
019000     EJECT                                                                
019100*01  -COPY W0008     -PRE ARTC-                                           
019200         05  FILLER           PIC X.                                      
019300     EJECT                                                                
019400*01  -COPY W0008     -PRE ERSA-                                           
019500         05  FILLER           PIC X.                                      
019600     EJECT                                                                
019700 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
019800                                  ARTC-PCB ERSA-PCB.                      
019900 MAIN SECTION.                                                            
020000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
020100                                   ARTC-PCB ERSA-PCB.                     
020200                                                                          
020300     PERFORM IMS-GET-MSG                                                  
020400                                                                          
020500     IF  SEGMENT-FINNS                                                    
020600       PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                                
020700                                                                          
020800       IF  SW-NYCKLAR-OK = JA                                             
020900                                                                          
021000           MOVE IDARTNR-WS  TO W-IDARTNR                                  
021100           PERFORM IMS-GET-ARTC01                                         
021200           IF SEGMENT-FINNS                                               
021300              MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                   
021400              IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                   
021500              OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE             
021600*             OR MSGI-KDARBTYP-SEC (1:3) = 'ESC'                          
021700*             OR MSGI-KDARBTYP-SEC (1:3) = 'VOR'                          
021800*             OR MSGI-KDARBTYP-SEC (1:3) = 'LOC'                          
021900                                                                          
022000                 PERFORM IMS-GET-ERSATT-ART-SEG                           
022100                                                                          
022200                 IF SEGMENT-FINNS                                         
022300                    PERFORM C-RED-BILD-FRAN-WDD7                          
022400                 END-IF                                                   
022500                 PERFORM B-RED-BILD-FRAN-WDK6                             
022600              ELSE                                                        
022700*                --- EJ BEHÖRIG USER                                      
022800                 MOVE FEL-6 (SPIND) TO MOD-MESSAGE                        
022900              END-IF                                                      
023000           ELSE                                                           
023100              MOVE FEL-2A (SPIND) TO MOD-MESSAGE                          
023200*             MOVE MFS-RENSA-FAELT TO                                     
023210*                                     MOD-TIERSDAT                        
023300*                                     MOD-KDERS-C1                        
023400*                                     MOD-KDERS-C2                        
023500           END-IF                                                         
023600       ELSE                                                               
023700         MOVE FEL-1 (SPIND) TO MOD-MESSAGE                                
023800       END-IF                                                             
023900       PERFORM IMS-INSERT-MSG                                             
024000     END-IF                                                               
024100     MOVE ZERO TO RETURN-CODE                                             
024200     GOBACK                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
024600                                                                          
024700     MOVE JA TO SW-NYCKLAR-OK                                             
024800     IF MSG-DUBBLA-TRANSKODER                                             
024900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
025000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90403I1-CTX             
025200       MOVE ZERO TO MID-BLAEDRING-ANT                                     
025300     ELSE                                                                 
025400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
025500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90403I1-CTX              
025700     END-IF                                                               
025800     IF MID-IDARTNR-IN NOT = ALL '+'                                      
025900       MOVE ZERO TO MID-BLAEDRING-ANT                                     
026000     END-IF                                                               
026100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
026200     MOVE '001'             TO MSGI-KDCALL                                
026300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026400                               MSGI-IDLTERM-USER                          
026500     MOVE '9403'            TO MSGI-IDTRANS                               
026600     IF MFS-IDTRANS = '9403'                                              
026700     OR (MID-IDARTNR-IN NUMERIC                                           
026800     AND MID-IDARTNR-IN > ZERO)                                           
026900         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
027000     END-IF                                                               
027100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027200     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
027300     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
027400     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
027500     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
027600                                                                          
027700     IF MSGI-IDLAND-SPR = SPACE                                           
027800       IF MSGI-IDSPRAK = SPACE                                            
027900         IF SWEDISH-TEXT                                                  
028000           MOVE +1 TO SPIND                                               
028100         ELSE                                                             
028200           MOVE +2 TO SPIND                                               
028300         END-IF                                                           
028400       ELSE                                                               
028500         IF MSGI-IDSPRAK = 'SV'                                           
028600           MOVE +1 TO SPIND                                               
028700         ELSE                                                             
028800           MOVE +2 TO SPIND                                               
028900         END-IF                                                           
029000       END-IF                                                             
029100     ELSE                                                                 
029200       IF MSGI-IDLAND-SPR = 'SE'                                          
029300         MOVE +1 TO SPIND                                                 
029400       ELSE                                                               
029500         MOVE +2 TO SPIND                                                 
029600       END-IF                                                             
029700     END-IF                                                               
029800                                                                          
029900     MOVE LOW-VALUE TO MOD-AREA-OUTPUT                                    
030000     MOVE 'W90403O1' TO MFS-IDMOD                                         
030100     MOVE '9403' TO MOD-TRANS-NUMMER                                      
030200     MOVE LENGTH OF MOD-W90403O1  TO MSG-KVLL                             
030300     ADD  +4                      TO MSG-KVLL                             
030400     MOVE MFS-RENSA-FAELT TO                                              
030410                             MOD-IDARTNR-IN                               
030500                             MOD-MESSAGE                                  
030600                             MOD-MESSAGE-BOTTOM                           
030700     MOVE 1 TO IX                                                         
030800     PERFORM UNTIL                                                        
030900      ( IX > MAX-ANT-BILD-RADER )                                         
031000       MOVE MFS-RENSA-FAELT TO MOD-RADER (IX)                             
031100           ADD 1 TO IX                                                    
031200     END-PERFORM                                                          
031300                                                                          
031400     IF  MID-BLAEDRING-ANT NOT NUMERIC                                    
031500     OR  MFS-IDTRANS NOT = '9403'                                         
031600       MOVE ZERO TO MID-BLAEDRING-ANT                                     
031700     END-IF                                                               
031800                                                                          
031900     IF  IDARTNR-WS NOT NUMERIC                                           
032000       MOVE FEL-1 (SPIND) TO MOD-MESSAGE                                  
032100       MOVE NEJ TO SW-NYCKLAR-OK                                          
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 B-RED-BILD-FRAN-WDK6 SECTION.                                            
032600                                                                          
032700*    MOVE MFS-RENSA-FAELT TO MOD-KDERS-C2                                 
032800                                                                          
032900     PERFORM IMS-GET-ARTC01                                               
033000     IF SEGMENT-FINNS                                                     
033100*      MOVE ART-TIERSDAT TO MOD-TIERSDAT                                  
033200*      MOVE ART-KDERS-UTG TO MOD-KDERS-C1                                 
033300       MOVE ART-KDERS-UTG TO       W-KDERS                                
033400*      MOVE STRECK       TO MOD-STRECK                                    
033500*      MOVE ART-REKSIFFR TO MOD-REKSIFFR                                  
033600                                                                          
033700       PERFORM IMS-GET-ARTC11                                             
033800       IF  SEGMENT-FINNS                                                  
033900*        MOVE CLAG-KDERS TO MOD-KDERS-C1                                  
034000         MOVE CLAG-KDERS TO      W-KDERS                                  
034100       END-IF                                                             
034200                                                                          
034300       IF W-KDERS = ZERO                                                  
034400         MOVE FEL-2 (SPIND) TO MOD-MESSAGE                                
034500       ELSE                                                               
034600         IF W-KDERS = +09 OR +19                                          
034700           MOVE FEL-4 (SPIND) TO MOD-MESSAGE                              
034800         ELSE                                                             
034900           IF W-KDERS = +29 OR +52                                        
035000             MOVE FEL-5 (SPIND) TO MOD-MESSAGE                            
035100           ELSE                                                           
035200             MOVE FEL-3 (SPIND) TO MOD-MESSAGE                            
035300           END-IF                                                         
035400         END-IF                                                           
035500       END-IF                                                             
035600     ELSE                                                                 
035700*      --- REDAN KOLLAT I STYR-SEKTIONEN                                  
035800       CONTINUE                                                           
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 C-RED-BILD-FRAN-WDD7 SECTION.                                            
036300                                                                          
036400     MOVE ERSATT-DIERS-ERS TO MOD-DIERS-ERS                               
036500*    IF ERSATT-TEARTNOT = SPACE                                           
036600*      MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT                               
036700*    ELSE                                                                 
036800*      MOVE ERSATT-TEARTNOT TO MOD-TEARTNOT                               
036900*    END-IF                                                               
037000                                                                          
037100     PERFORM IMS-GET-TILLK-SEG                                            
037200     MOVE MID-BLAEDRING-ANT TO W-BLAEDRING-ANT                            
037300     MOVE 1 TO IX                                                         
037400               IY                                                         
037500               IZ                                                         
037600     MOVE 2 TO IY-PL1                                                     
037700     PERFORM UNTIL                                                        
037800      NOT ( SEGMENT-FINNS AND IY NOT > MAX-ANT-BILD-KOLUMNER )            
037900                                                                          
038000       IF IZ > W-BLAEDRING-ANT                                            
038100         IF  TILLK-FLTEXT = NEJ                                           
038200           MOVE TILLK-IDARTNR-TILLK                                       
038300                                 TO MOD-IDARTNR-TILLK (IX, IY)            
038400                                    KS-IDARTNR                            
038500           MOVE TILLK-DIERS-TILLK TO MOD-DIERS-TILLK (IX, IY)             
038600           CALL W009KSIF USING KS-IDARTNR KS-LGD KS-SIFF                  
038700*          MOVE STRECK           TO MOD-STRECK-TILLK (IX, IY)             
038800*          MOVE KS-SIFF          TO MOD-REKSIFFR-TILLK (IX, IY)           
038900         ELSE                                                             
039000           MOVE TILLK-BEERS TO MOD-KOLUMNER (IX, IY)                      
039100         END-IF                                                           
039200         IF  IY < MAX-ANT-BILD-KOLUMNER                                   
039300           MOVE MFS-RENSA-FAELT TO MOD-KOLUMNER (IX, IY-PL1)              
039400         END-IF                                                           
039500         ADD 1 TO IX                                                      
039600         IF  IX > MAX-ANT-BILD-RADER                                      
039700           MOVE 1 TO IX                                                   
039800           ADD 1 TO IY                                                    
039900                            IY-PL1                                        
040000         END-IF                                                           
040100       END-IF                                                             
040200       ADD 1 TO IZ                                                        
040300       PERFORM IMS-GET-TILLK-SEG                                          
040400     END-PERFORM                                                          
040500                                                                          
040600     IF IY > MAX-ANT-BILD-KOLUMNER                                        
040700       MOVE TEXT-KOMMENTAR-1 (SPIND) TO MOD-MESSAGE-BOTTOM                
040800       COMPUTE IZ = IZ - 1                                                
040900       MOVE IZ TO NUM-FAELT                                               
041000       MOVE NUM-FAELT TO MOD-BLAEDRING-ANT                                
041100     ELSE                                                                 
041200       MOVE ZERO TO MOD-BLAEDRING-ANT                                     
041300     END-IF                                                               
041400                                                                          
041500*    MOVE MFS-RENSA-FAELT TO MOD-KDSTATUS-C2                              
041600*                            MOD-TIERSDAT-PREL-C2                         
041700                                                                          
041800*    PERFORM IMS-GET-ERSINF-SEG                                           
041900*    IF  SEGMENT-FINNS                                                    
042000*      MOVE ERSINF-KDSTATUS-C1 TO MOD-KDSTATUS-C1                         
042100*      MOVE ERSINF-TIERSDAT-REG TO MOD-TIERSDAT-REG                       
042200*      MOVE ERSINF-TIERSDAT-PREL-C1 TO MOD-TIERSDAT-PREL-C1               
042300*    ELSE                                                                 
042400*      MOVE MFS-RENSA-FAELT TO MOD-KDSTATUS-C1                            
042500*                              MOD-TIERSDAT-REG                           
042600*                              MOD-TIERSDAT-PREL-C1                       
042700*    END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000* IMS SEKTIONER                                                           
043100     SKIP3                                                                
043200 IMS-GET-MSG SECTION.                                                     
043300     MOVE '  QC' TO GODK-STATUSKODER                                      
043400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
043500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     SKIP3                                                                
043900 IMS-INSERT-MSG SECTION.                                                  
044000*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
044100*      MOVE '0' TO MFS-KDHUVOMR                                           
044200*    END-IF                                                               
044300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
044400     MOVE SPACE TO GODK-STATUSKODER                                       
044500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
044600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     EJECT                                                                
045000 IMS-GET-ARTC01 SECTION.                                                  
045100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
045200            DELIMITED BY SIZE INTO SSA1                                   
045300     MOVE '  GE' TO GODK-STATUSKODER                                      
045400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
045500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     .                                                                    
045800     SKIP3                                                                
045900 IMS-GET-ARTC11 SECTION.                                                  
046000     MOVE 'WLARTC11 '  TO SSA1                                            
046100     MOVE '  GE' TO GODK-STATUSKODER                                      
046200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
046300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
046400     PERFORM IMS-STATUSKONTROLL                                           
046500     .                                                                    
046600     EJECT                                                                
046700 IMS-GET-ERSATT-ART-SEG SECTION.                                          
046800     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
046900            DELIMITED BY SIZE INTO SSA1                                   
047000     MOVE '  GE' TO GODK-STATUSKODER                                      
047100     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
047200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
047300     PERFORM IMS-STATUSKONTROLL                                           
047400     .                                                                    
047500     SKIP3                                                                
047600 IMS-GET-TILLK-SEG SECTION.                                               
047700     MOVE 'WLERSA11' TO SSA1                                              
047800     MOVE '  GE' TO GODK-STATUSKODER                                      
047900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
048000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
048100     PERFORM IMS-STATUSKONTROLL                                           
048200     .                                                                    
048300     SKIP3                                                                
048400 IMS-GET-ERSINF-SEG SECTION.                                              
048500     MOVE 'WLERSA13' TO SSA1                                              
048600     MOVE '  GE' TO GODK-STATUSKODER                                      
048700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
048800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
048900     PERFORM IMS-STATUSKONTROLL                                           
049000     .                                                                    
049100     EJECT                                                                
049200 IMS-STATUSKONTROLL SECTION.                                              
049300     SET STATUS-IX TO 1                                                   
049400     SEARCH GODK-STATUS                                                   
049500       AT END                                                             
049600         CALL FELLOG                                                      
049700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
049800       CONTINUE                                                           
049900     END-SEARCH                                                           
050000     .                                                                    
