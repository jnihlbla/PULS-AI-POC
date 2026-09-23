000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0105      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W6030500.                                                
000900 AUTHOR.         MÅNS SAMUELSSON/ TOMMIE JIVARP                           
001000 DATE-WRITTEN.   95/12/12.     /  TILLÄGG 98/03/18.                       
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        UPPDATERAR BUFFERTSALDO                                          
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001700*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001800*        PROGRAMMET UPPDATERAR WLARTD (WDD8)                              
001900*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002000*        PROGRAMMET UPPDATERAR WLLOCB (WDJ9)                              
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6T305                                              
002400*        MID:         W6I30501                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W6O30501                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -- CHECKED BY WY2000                                                 
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W6030500'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  RAD-IX                      PIC S9(9)   VALUE +0 COMP SYNC.          
004500 77  MAX-IX                      PIC S9(9)   VALUE +7 COMP SYNC.          
004600                                                                          
004700*    --- ARBETSFÄLT FÖR UPPDATERING AV PLATSREGISTRET (WDJ9)              
004800 77      LOGG-DATUM         PIC S9(8)             VALUE ZERO.             
004900 77      LOGG-TID           PIC S9(7)             VALUE ZERO.             
005000 77      BUFFER-LOCATION    PIC X                 VALUE 'B'.              
005100                                                                          
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005500     88  INDATA-OK                           VALUE 'J'.                   
005600     88  INDATA-FEL                          VALUE 'N'.                   
005700                                                                          
005800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005900     88  NYCKLAR-OK                          VALUE 'J'.                   
006000     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100                                                                          
006200 77  UPD-FRAN-NYCKELRAD-SW       PIC X       VALUE 'N'.                   
006300     88  UPD-FRAN-NYCKELRAD                  VALUE 'J'.                   
006400                                                                          
006500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006600     88  EGEN-MID                            VALUE '6305'.                
006700     88  HELP-MID                            VALUE '0551'.                
006800     EJECT                                                                
006900 01  WS-KVBUFF                   PIC X(7).                                
007000 01  WS-ADBUFFOMR                PIC X(2).                                
007100 01  WS-ADBUFFGANG               PIC X(2).                                
007200 01  WS-ADBUFFPL                 PIC X(5).                                
007300 01  WS-ANTAL                    PIC 9(7).                                
007400                                                                          
007500 01  WS-SUBUFF-F                 PIC S9(7)   VALUE +0.                    
007600 01  WS-SUKOLLI-F                PIC S9(5)   VALUE +0.                    
007700                                                                          
007800 01  W-MINKEY-WDD811KY.                                                   
007900     03  W-MINKEY-IDTRANS          PIC X(4)    VALUE '6305'.              
008000     03  W-MINKEY-ADBUFFOMR-ENTER  PIC S9(3)   VALUE +0 COMP-3.           
008100     03  W-MINKEY-DABUFPAF-ENTER   PIC  9(8)   VALUE ZERO.                
008200     03  W-MINKEY-ADBUFFGANG-ENTER PIC S9(3)   VALUE +0 COMP-3.           
008300     03  W-MINKEY-ADBUFFPL-ENTER   PIC S9(5)   VALUE +0 COMP-3.           
008400     03  W-MINKEY-ADBUFFOMR-NEXT   PIC S9(3)   VALUE +0 COMP-3.           
008500     03  W-MINKEY-DABUFPAF-NEXT    PIC  9(8)   VALUE ZERO.                
008600     03  W-MINKEY-ADBUFFGANG-NEXT  PIC S9(3)   VALUE +0 COMP-3.           
008700     03  W-MINKEY-ADBUFFPL-NEXT    PIC S9(5)   VALUE +0 COMP-3.           
008800                                                                          
008900*      --- VALID IDDC CODES                                               
009000*                                                                         
009100*01    -COPY WWDC99                                                       
009200       EJECT                                                              
009300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009400 01  GENERELLA-SUBPROGRAM.                                                
009500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010100*01 -COPY WMEDAREA                                                        
010200     SKIP3                                                                
010300 01  MESSAGE-CODES.                                                       
010400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010500     03  ERR-CONFLICT-FLDS       PIC X(3)    VALUE '002'.                 
010600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010700     03  ERR-MISSING-IN-REG      PIC X(3)    VALUE '010'.                 
010800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010900     03  ERR-MISSING-IN-ARTREG   PIC X(3)    VALUE '017'.                 
011000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011200     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
011300     03  ERR-ALREADY-EXIST       PIC X(3)    VALUE '245'.                 
011400     03  ERR-MISSING-IN-BUFF-REG PIC X(3)    VALUE '316'.                 
011500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012000     SKIP3                                                                
012100*01 -COPY WMSGINIT                                                        
012200     SKIP3                                                                
012300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012600     SKIP3                                                                
012700*01  MID -COPY W6I30501                                                   
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013000     SKIP3                                                                
013100*01  -COPY WMSGAREA                                                       
013200     EJECT                                                                
013300     03  MOD REDEFINES MSG-AREA.                                          
013400*      05  -COPY W6O30501                                                 
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013700     SKIP3                                                                
013800*01  -COPY WMFSAREA                                                       
013900     EJECT                                                                
014000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014100*                                                                         
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014400     SKIP3                                                                
014500 01  NYCKLAR-TILL-DLI.                                                    
014600     03  W-IDARTNR-X.                                                     
014700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014800     03  W-KDSEGKEY-X.                                                    
014900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
015000     03  W-WDD811KY-X.                                                    
015100         05  W-IDDC-X.                                                    
015200           07  W-IDDC            PIC X(2)    VALUE SPACE.                 
015300         05  W-ADBUFFOMR         PIC S9(3)   VALUE ZERO COMP-3.           
015400         05  W-DABUFPAF          PIC  9(8)   VALUE ZERO.                  
015500         05  W-ADBUFFGANG        PIC S9(3)   VALUE ZERO COMP-3.           
015600         05  W-ADBUFFPL          PIC S9(5)   VALUE ZERO COMP-3.           
015700     03  W-WDD811KY-MIN-X.                                                
015800         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
015900         05  W-ADBUFFOMR-MIN     PIC S9(3)   VALUE ZERO COMP-3.           
016000         05  W-DABUFPAF-MIN      PIC  9(8)   VALUE ZERO.                  
016100         05  W-ADBUFFGANG-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
016200         05  W-ADBUFFPL-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
016300     03  W-WDD811KY-MAX-X.                                                
016400         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
016500         05  W-ADBUFFOMR-MAX     PIC S9(3)   VALUE +999 COMP-3.           
016600         05  W-ADBUFPAF-MAX      PIC  9(8)   VALUE  99999999.             
016700         05  W-ADBUFFGANG-MAX    PIC S9(3)   VALUE +999 COMP-3.           
016800         05  W-ADBUFFPL-MAX      PIC S9(5)   VALUE +99999 COMP-3.         
017400     03  W-WDJ911KY-X.                                                    
017500         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
017600         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
017700         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
017800         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO COMP-3.           
017900         05  W-ADGANG            PIC 9(2)    VALUE ZERO COMP-3.           
018000         05  W-ADPLATS           PIC 9(5)    VALUE ZERO COMP-3.           
018100     03  W-WDD8A1KY-MIN-X.                                                
018110         05  W-IDDC-A1KY-MIN      PIC X(2)  VALUE SPACE.                  
018120         05  W-ADBUFFOM-A1KY-MIN  PIC S9(3) VALUE +000 COMP-3.            
018130         05  W-ADBUFGAN-A1KY-MIN  PIC S9(3) VALUE +000 COMP-3.            
018140         05  W-ADBUFPL-A1KY-MIN   PIC S9(5) VALUE +00000 COMP-3.          
018150         05  FILLER               PIC X(13) VALUE LOW-VALUE.              
018160     03  W-WDD8A1KY-MAX-X.                                                
018170         05  W-IDDC-A1KY-MAX      PIC X(2)  VALUE SPACE.                  
018180         05  W-ADBUFFOM-A1KY-MAX  PIC S9(3) VALUE +000 COMP-3.            
018190         05  W-ADBUFGAN-A1KY-MAX  PIC S9(3) VALUE +000 COMP-3.            
018191         05  W-ADBUFPL-A1KY-MAX   PIC S9(5) VALUE +00000 COMP-3.          
018192         05  FILLER               PIC X(13) VALUE HIGH-VALUE.             
020800     03  W-IDSKYLT-X.                                                     
020900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
021000     SKIP2                                                                
021100*    --- STATUS-KOD FRÅN IMS                                              
021200 01  STATUS-WS                   PIC XX.                                  
021300     88  SEGMENT-FINNS                       VALUE '  '.                  
021400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021600     SKIP2                                                                
021700 01  GODK-STATUSKODER.                                                    
021800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021900     SKIP3                                                                
022000 01  SSA1                        PIC X(128).                              
022100 01  SSA2                        PIC X(96).                               
022200     EJECT                                                                
022300*    --- IMS FUNKTIONSKODER                                               
022400*01  -COPY W0003                                                          
022500     EJECT                                                                
022600*    ---  DLI INPUT-OUTPUT AREA                                           
022700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022800     SKIP3                                                                
022900 01  DLI-IO-AREA.                                                         
023000     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
023100     SKIP3                                                                
023200     03  WLARTC01 REDEFINES IO-AREA.                                      
023300*        05  -COPY WDK601 -PRE ARTC-                                      
023400     EJECT                                                                
023500     03  WLARTC11 REDEFINES IO-AREA.                                      
023600*        05  -COPY WDK611 -PRE ARTC-                                      
023700     EJECT                                                                
023800     03  WLARTD01 REDEFINES IO-AREA.                                      
023900*        05  -COPY WDD801 -PRE ARTD-                                      
024000     EJECT                                                                
024100     03  WLARTD11 REDEFINES IO-AREA.                                      
024200*        05  -COPY WDD811 -PRE ARTD-                                      
024300     EJECT                                                                
024400     03  WLBENA11 REDEFINES IO-AREA.                                      
024500*        05  -COPY WDD311 -PRE BENA-                                      
024600     EJECT                                                                
024700     03  WLARTS11 REDEFINES IO-AREA.                                      
024800*        05  -COPY WDK711 -PRE ARTS-                                      
024900     EJECT                                                                
025000     03  WLLOCB01 REDEFINES IO-AREA.                                      
025100*        05  -COPY WDJ901 -PRE LOCB-                                      
025200     SKIP3                                                                
025300     03  WLLOCB11 REDEFINES IO-AREA.                                      
025400*        05  -COPY WDJ911 -PRE LOCB-                                      
026500     EJECT                                                                
026600*    ---  DLI INPUT-OUTPUT AREA3                                          
026700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
026800     SKIP3                                                                
026900 01  DLI-IO-AREA3.                                                        
027000     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
027100     SKIP3                                                                
027200     03  WDA8A1-X REDEFINES IO-AREA3.                                     
027300*        05  -COPY WDD8A1                                                 
027400     SKIP3                                                                
027500 LINKAGE SECTION.                                                         
027600                                                                          
027700*01  -COPY W0009   -PRE MSG-                                              
027800     EJECT                                                                
027900*01  -COPY W0008   -PRE USEA-                                             
028000     05  FILLER                  PIC X.                                   
028100     EJECT                                                                
028200*01  -COPY W0008  -PRE ARTC-                                              
028300     05  FILLER                  PIC X.                                   
028400     EJECT                                                                
028500*01  -COPY W0008  -PRE ARTD-                                              
028600     05  FILLER                  PIC X.                                   
028700     EJECT                                                                
028800*01  -COPY W0008  -PRE BENA-                                              
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029400*01  -COPY W0008  -PRE ARTS-                                              
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008  -PRE LOCB-                                              
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008  -PRE WDD8A-                                             
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB ARTD-PCB             
030400     BENA-PCB ARTS-PCB LOCB-PCB WDD8A-PCB.                                
030500 MAIN SECTION.                                                            
030600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB ARTD-PCB             
030700     BENA-PCB ARTS-PCB LOCB-PCB WDD8A-PCB.                                
030800                                                                          
030900     PERFORM IMS-GET-MSG                                                  
031000     IF SEGMENT-FINNS                                                     
031100       PERFORM A-INIT                                                     
031200       PERFORM B-KOLLA-NYCKLAR                                            
031300       IF NYCKLAR-OK                                                      
031400         IF MFS-UPDATE                                                    
031500           PERFORM G-KOLLA-INPUT                                          
031600           IF INDATA-OK                                                   
031700             PERFORM H-UPPDATERA                                          
031800           END-IF                                                         
031900         ELSE                                                             
032000           IF MFS-FIRST                                                   
032100             PERFORM C-FOERSTA-SIDA                                       
032200           ELSE                                                           
032300             IF MFS-NEXT                                                  
032400               PERFORM D-NAESTA-SIDA                                      
032500             ELSE                                                         
032600               PERFORM E-SAMMA-SIDA                                       
032700             END-IF                                                       
032800           END-IF                                                         
032900         END-IF                                                           
033000         IF INDATA-OK                                                     
033100           PERFORM F-LAES-VISA-INFO                                       
033200         END-IF                                                           
033300       END-IF                                                             
033400       PERFORM IMS-INSERT-MSG                                             
033500     END-IF                                                               
033600                                                                          
033700     MOVE ZERO TO RETURN-CODE                                             
033800     GOBACK                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 A-INIT SECTION.                                                          
034200                                                                          
034300     IF MSG-DUBBLA-TRANSKODER                                             
034400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30501                 
034500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
034600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
034700     ELSE                                                                 
034800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I30501                  
034900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
035000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035100     END-IF                                                               
035200                                                                          
035300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
035400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
035500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
035600                                                                          
035700     MOVE LOW-VALUE TO MSG-AREA                                           
035800     MOVE 'W6O305N1' TO MFS-IDMOD                                         
035900     MOVE '6305' TO MOD-IDTRANS                                           
036000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
036100                                                                          
036200     COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30501 + 4                        
036300                                                                          
036400     IF EGEN-MID OR HELP-MID                                              
036500       CONTINUE                                                           
036600     ELSE                                                                 
036700       MOVE SPACE TO MFS-KDTRTYP                                          
036800       MOVE '7' TO MFS-IDPFK                                              
036900     END-IF                                                               
037000     MOVE SPACE    TO MED-IDMFSINF                                        
037100     .                                                                    
037200     EJECT                                                                
037300 B-KOLLA-NYCKLAR SECTION.                                                 
037400                                                                          
037500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037600     MOVE '001'             TO MSGI-KDCALL                                
037700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037900     MOVE '6305'            TO MSGI-IDTRANS                               
038000     IF EGEN-MID                                                          
038100         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
038200     ELSE                                                                 
038300         MOVE ALL '+'            TO MID-KVBUFF-IN                         
038400                                    MID-ADBUFFOMR-IN                      
038500                                    MID-ADBUFFGANG-IN                     
038600                                    MID-ADBUFFPL-IN                       
038700         MOVE SPACE              TO MID-KVBUFF-UT                         
038800                                    MID-ADBUFFOMR-UT                      
038900                                    MID-ADBUFFGANG-UT                     
039000                                    MID-ADBUFFPL-UT                       
039100     END-IF                                                               
039200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039300                                                                          
039400     IF MSGI-IDLAND-SPR = 'SE'                                            
039500       MOVE 'S  '           TO W-IDSKYLT                                  
039600                               MED-IDSKYLT                                
039700     ELSE                                                                 
039800       MOVE 'GB '           TO W-IDSKYLT                                  
039900                               MED-IDSKYLT                                
040000     END-IF                                                               
040100     MOVE JA TO NYCKLAR-SW                                                
040200                                                                          
040300                                                                          
040400*    -- KONTROLL AV IDARTNR                                               
040500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
040600                                                                          
040700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
040800       MOVE '7'         TO MFS-IDPFK                                      
040900       MOVE SPACE       TO MFS-KDTRTYP                                    
041000     END-IF                                                               
041100     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
041200     IF MSGI-IDARTNR NUMERIC                                              
041300       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
041400     ELSE                                                                 
041500       MOVE NEJ TO NYCKLAR-SW                                             
041600     END-IF                                                               
041700                                                                          
041800*    -- KONTROLL AV IDDC                                                  
041900     MOVE MSGI-IDDC       TO MOD-IDDC                                     
042000                             W-IDDC                                       
042100                             W-IDDC-MIN                                   
042200                             W-IDDC-MAX                                   
042400                             W-IDDC-A1KY-MAX                              
042500                             W-IDDC-A1KY-MIN                              
042600                                                                          
042700*    -- KONTROLL AV KVBUFF                                                
042800     MOVE MFS-RENSA-FAELT TO MOD-KVBUFF-IN                                
042900                                                                          
043000     IF MID-KVBUFF-IN NOT = ALL '+'                                       
043100       MOVE '7'         TO MFS-IDPFK                                      
043200       MOVE SPACE       TO MFS-KDTRTYP                                    
043300       MOVE MID-KVBUFF-IN   TO WS-KVBUFF                                  
043400     ELSE                                                                 
043500       IF MID-IDARTNR-IN NOT = ALL '+'                                    
043600         MOVE ZERO            TO WS-KVBUFF                                
043700       ELSE                                                               
043800         MOVE MID-KVBUFF-UT   TO WS-KVBUFF                                
043900       END-IF                                                             
044000     END-IF                                                               
044100     INSPECT WS-KVBUFF REPLACING LEADING SPACE BY ZERO                    
044200     IF WS-KVBUFF NUMERIC                                                 
044300       CONTINUE                                                           
044400     ELSE                                                                 
044500       MOVE NEJ TO NYCKLAR-SW                                             
044600     END-IF                                                               
044700                                                                          
044800*    -- KONTROLL AV ADBUFFOMR                                             
044900     MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR-IN                             
045000                                                                          
045100     IF MID-ADBUFFOMR-IN NOT = ALL '+'                                    
045200       MOVE '7'         TO MFS-IDPFK                                      
045300       MOVE SPACE       TO MFS-KDTRTYP                                    
045400       MOVE MID-ADBUFFOMR-IN   TO WS-ADBUFFOMR                            
045500     ELSE                                                                 
045600       IF MID-IDARTNR-IN NOT = ALL '+'                                    
045700         MOVE ZERO               TO WS-ADBUFFOMR                          
045800       ELSE                                                               
045900         MOVE MID-ADBUFFOMR-UT   TO WS-ADBUFFOMR                          
046000       END-IF                                                             
046100     END-IF                                                               
046200     INSPECT WS-ADBUFFOMR REPLACING LEADING SPACE BY ZERO                 
046300     IF WS-ADBUFFOMR NUMERIC                                              
046400       MOVE WS-ADBUFFOMR TO W-ADBUFFOMR                                   
046500                            W-ADBUFFOMR-MIN                               
046600     ELSE                                                                 
046700       MOVE NEJ TO NYCKLAR-SW                                             
046800     END-IF                                                               
046900                                                                          
047000*    -- KONTROLL AV ADBUFFGANG                                            
047100     MOVE MFS-RENSA-FAELT TO MOD-ADBUFFGANG-IN                            
047200                                                                          
047300     IF MID-ADBUFFGANG-IN NOT = ALL '+'                                   
047400       MOVE '7'         TO MFS-IDPFK                                      
047500       MOVE SPACE       TO MFS-KDTRTYP                                    
047600       MOVE MID-ADBUFFGANG-IN TO WS-ADBUFFGANG                            
047700     ELSE                                                                 
047800       IF MID-IDARTNR-IN NOT = ALL '+'                                    
047900         MOVE ZERO              TO WS-ADBUFFGANG                          
048000       ELSE                                                               
048100         MOVE MID-ADBUFFGANG-UT TO WS-ADBUFFGANG                          
048200       END-IF                                                             
048300     END-IF                                                               
048400     INSPECT WS-ADBUFFGANG REPLACING LEADING SPACE BY ZERO                
048500     IF WS-ADBUFFGANG NUMERIC                                             
048600       MOVE WS-ADBUFFGANG TO W-ADBUFFGANG                                 
048700                             W-ADBUFFGANG-MIN                             
048800     ELSE                                                                 
048900       MOVE NEJ TO NYCKLAR-SW                                             
049000     END-IF                                                               
049100                                                                          
049200*    -- KONTROLL AV ADBUFFPL                                              
049300     MOVE MFS-RENSA-FAELT TO MOD-ADBUFFPL-IN                              
049400                                                                          
049500     IF MID-ADBUFFPL-IN NOT = ALL '+'                                     
049600       MOVE '7'         TO MFS-IDPFK                                      
049700       MOVE SPACE       TO MFS-KDTRTYP                                    
049800       MOVE MID-ADBUFFPL-IN TO WS-ADBUFFPL                                
049900     ELSE                                                                 
050000       IF MID-IDARTNR-IN NOT = ALL '+'                                    
050100         MOVE ZERO            TO WS-ADBUFFPL                              
050200       ELSE                                                               
050300         MOVE MID-ADBUFFPL-UT TO WS-ADBUFFPL                              
050400       END-IF                                                             
050500     END-IF                                                               
050600     INSPECT WS-ADBUFFPL REPLACING LEADING SPACE BY ZERO                  
050700     IF WS-ADBUFFPL NUMERIC                                               
050800       MOVE WS-ADBUFFPL TO W-ADBUFFPL                                     
050900                           W-ADBUFFPL-MIN                                 
051000     ELSE                                                                 
051100       MOVE NEJ TO NYCKLAR-SW                                             
051200     END-IF                                                               
051300                                                                          
051400     IF EGEN-MID OR NYCKLAR-OK                                            
051500       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
051600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
051700       MOVE WS-KVBUFF           TO MOD-KVBUFF-UT                          
051800       INSPECT MOD-KVBUFF-UT REPLACING LEADING ZERO BY SPACE              
051900       MOVE WS-ADBUFFOMR        TO MOD-ADBUFFOMR-UT                       
052000       INSPECT MOD-ADBUFFOMR-UT REPLACING LEADING ZERO BY SPACE           
052100       MOVE WS-ADBUFFGANG       TO MOD-ADBUFFGANG-UT                      
052200       INSPECT MOD-ADBUFFGANG-UT REPLACING LEADING ZERO BY SPACE          
052300       MOVE WS-ADBUFFPL         TO MOD-ADBUFFPL-UT                        
052400       INSPECT MOD-ADBUFFPL-UT REPLACING LEADING ZERO BY SPACE            
052500     ELSE                                                                 
052600       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
052700                               MOD-KVBUFF-UT                              
052800                               MOD-ADBUFFOMR-UT                           
052900                               MOD-ADBUFFGANG-UT                          
053000                               MOD-ADBUFFPL-UT                            
053100     END-IF                                                               
053200                                                                          
053300     IF NYCKLAR-FEL                                                       
053400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
053500       CALL WMEDKONV USING MED-WMEDAREA                                   
053600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
053700       PERFORM MFS-RENSA-FAELT-IN                                         
053800       PERFORM MFS-RENSA-FAELT-UT                                         
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200 C-FOERSTA-SIDA SECTION.                                                  
054300                                                                          
054400     PERFORM MFS-RENSA-FAELT-IN                                           
054500     IF MID-ADBUFFOMR-IN  = ALL '+' AND                                   
054600        MID-ADBUFFGANG-IN = ALL '+' AND                                   
054700        MID-ADBUFFPL-IN   = ALL '+'                                       
054800        MOVE ZERO     TO W-ADBUFFOMR-MIN                                  
054900                         W-DABUFPAF-MIN                                   
055000                         W-ADBUFFGANG-MIN                                 
055100                         W-ADBUFFPL-MIN                                   
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500 D-NAESTA-SIDA SECTION.                                                   
055600                                                                          
055700     PERFORM MFS-RENSA-FAELT-IN                                           
055800     IF EGEN-MID OR HELP-MID                                              
055900       MOVE MSGI-SPAR-AREA         TO W-MINKEY-WDD811KY                   
056000       IF W-MINKEY-IDTRANS = '6305'   AND                                 
056100          W-MINKEY-ADBUFFOMR-NEXT  NUMERIC AND                            
056200          W-MINKEY-DABUFPAF-NEXT   NUMERIC AND                            
056300          W-MINKEY-ADBUFFGANG-NEXT NUMERIC AND                            
056400          W-MINKEY-ADBUFFPL-NEXT   NUMERIC                                
056500         MOVE W-MINKEY-ADBUFFOMR-NEXT     TO W-ADBUFFOMR-MIN              
056600         MOVE W-MINKEY-DABUFPAF-NEXT      TO W-DABUFPAF-MIN               
056700         MOVE W-MINKEY-ADBUFFGANG-NEXT    TO W-ADBUFFGANG-MIN             
056800         MOVE W-MINKEY-ADBUFFPL-NEXT      TO W-ADBUFFPL-MIN               
056900       ELSE                                                               
057000         MOVE ZERO                   TO W-ADBUFFOMR-MIN                   
057100                                        W-DABUFPAF-MIN                    
057200                                        W-ADBUFFGANG-MIN                  
057300                                        W-ADBUFFPL-MIN                    
057400       END-IF                                                             
057500     ELSE                                                                 
057600       MOVE ZERO                   TO W-ADBUFFOMR-MIN                     
057700                                      W-DABUFPAF-MIN                      
057800                                      W-ADBUFFGANG-MIN                    
057900                                      W-ADBUFFPL-MIN                      
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300 E-SAMMA-SIDA SECTION.                                                    
058400     IF EGEN-MID OR HELP-MID                                              
058500       MOVE MSGI-SPAR-AREA         TO W-MINKEY-WDD811KY                   
058600       IF W-MINKEY-IDTRANS = '6305' AND                                   
058700          W-MINKEY-ADBUFFOMR-ENTER  NUMERIC AND                           
058800          W-MINKEY-DABUFPAF-ENTER   NUMERIC AND                           
058900          W-MINKEY-ADBUFFGANG-ENTER NUMERIC AND                           
059000          W-MINKEY-ADBUFFPL-ENTER   NUMERIC                               
059100         MOVE W-MINKEY-ADBUFFOMR-ENTER     TO W-ADBUFFOMR-MIN             
059200         MOVE W-MINKEY-DABUFPAF-ENTER      TO W-DABUFPAF-MIN              
059300         MOVE W-MINKEY-ADBUFFGANG-ENTER    TO W-ADBUFFGANG-MIN            
059400         MOVE W-MINKEY-ADBUFFPL-ENTER      TO W-ADBUFFPL-MIN              
059500       ELSE                                                               
059600         MOVE ZERO                   TO W-ADBUFFOMR-MIN                   
059700                                        W-DABUFPAF-MIN                    
059800                                        W-ADBUFFGANG-MIN                  
059900                                        W-ADBUFFPL-MIN                    
060000       END-IF                                                             
060100       IF MID-INPUT-RAD = ALL '+' AND                                     
060200          MID-KDCMDVAL (1) = ALL '+' AND                                  
060300          MID-KDCMDVAL (2) = ALL '+' AND                                  
060400          MID-KDCMDVAL (3) = ALL '+' AND                                  
060500          MID-KDCMDVAL (4) = ALL '+' AND                                  
060600          MID-KDCMDVAL (5) = ALL '+' AND                                  
060700          MID-KDCMDVAL (6) = ALL '+' AND                                  
060800          MID-KDCMDVAL (7) = ALL '+' AND                                  
060900          MID-INPUT-SALDO (1) = ALL '+' AND                               
061000          MID-INPUT-SALDO (2) = ALL '+' AND                               
061100          MID-INPUT-SALDO (3) = ALL '+' AND                               
061200          MID-INPUT-SALDO (4) = ALL '+' AND                               
061300          MID-INPUT-SALDO (5) = ALL '+' AND                               
061400          MID-INPUT-SALDO (6) = ALL '+' AND                               
061500          MID-INPUT-SALDO (7) = ALL '+'                                   
061600         PERFORM MFS-RENSA-FAELT-IN                                       
061700       ELSE                                                               
061800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
061900         CALL WMEDKONV USING MED-WMEDAREA                                 
062000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
062100         PERFORM EA-MID-INDATA-TILL-MOD                                   
062200       END-IF                                                             
062300     ELSE                                                                 
062400       PERFORM MFS-RENSA-FAELT-IN                                         
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 EA-MID-INDATA-TILL-MOD SECTION.                                          
062900                                                                          
063000     MOVE +1 TO RAD-IX                                                    
063100                                                                          
063200     PERFORM UNTIL RAD-IX > MAX-IX                                        
063300        IF MID-KDCMDVAL (RAD-IX)       = ALL '+'                          
063400           MOVE MFS-RENSA-FAELT         TO MOD-KDCMDVAL (RAD-IX)          
063500        ELSE                                                              
063600           MOVE MID-KDCMDVAL (RAD-IX)   TO MOD-KDCMDVAL (RAD-IX)          
063700           MOVE MFS-ADD-LAES-IN-FAELT   TO                                
063800                                   MOD-KDCMDVAL-ATTR (RAD-IX)             
063900        END-IF                                                            
064000        IF MID-ADBUFFOMR (RAD-IX)   = ALL '+'                             
064100           MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFOMR (RAD-IX)         
064200        ELSE                                                              
064300           MOVE MID-ADBUFFOMR (RAD-IX) TO MOD-ADBUFFOMR (RAD-IX)          
064400        END-IF                                                            
064500        IF MID-ADBUFFGANG (RAD-IX) = ALL '+'                              
064600           MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFGANG (RAD-IX)        
064700        ELSE                                                              
064800           MOVE MID-ADBUFFGANG (RAD-IX) TO MOD-ADBUFFGANG (RAD-IX)        
064900        END-IF                                                            
065000        IF MID-ADBUFFPL  (RAD-IX) = ALL '+'                               
065100           MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFPL  (RAD-IX)         
065200        ELSE                                                              
065300           MOVE MID-ADBUFFPL  (RAD-IX)  TO MOD-ADBUFFPL  (RAD-IX)         
065400        END-IF                                                            
065500        IF MID-DABUFPAF  (RAD-IX) = ALL '+'                               
065600           MOVE MFS-RENSA-FAELT         TO MOD-DABUFPAF  (RAD-IX)         
065700        ELSE                                                              
065800           MOVE MID-DABUFPAF  (RAD-IX)  TO MOD-DABUFPAF  (RAD-IX)         
065900        END-IF                                                            
066000        IF MID-KVBUFF-F-IN  (RAD-IX) = ALL '+'                            
066100           MOVE MFS-RENSA-FAELT         TO MOD-KVBUFF-F-IN(RAD-IX)        
066200        ELSE                                                              
066300           MOVE MID-KVBUFF-F-IN(RAD-IX) TO MOD-KVBUFF-F-IN(RAD-IX)        
066400           MOVE MFS-ADD-LAES-IN-FAELT   TO                                
066500                                   MOD-KVBUFF-F-IN-ATTR (RAD-IX)          
066600        END-IF                                                            
066700        IF MID-KVKOLLI-F-IN (RAD-IX) = ALL '+'                            
066800           MOVE MFS-RENSA-FAELT    TO MOD-KVKOLLI-F-IN  (RAD-IX)          
066900        ELSE                                                              
067000           MOVE MID-KVKOLLI-F-IN (RAD-IX)  TO                             
067100                                    MOD-KVKOLLI-F-IN  (RAD-IX)            
067200           MOVE MFS-ADD-LAES-IN-FAELT   TO                                
067300                                    MOD-KVKOLLI-F-IN-ATTR(RAD-IX)         
067400        END-IF                                                            
067500        ADD +1     TO RAD-IX                                              
067600                                                                          
067700     END-PERFORM                                                          
067800                                                                          
067900     IF MID-ADBUFFOMR-UPD     = ALL '+'                                   
068000        MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFOMR-UPD                 
068100     ELSE                                                                 
068200        MOVE MID-ADBUFFOMR-UPD       TO MOD-ADBUFFOMR-UPD                 
068300        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-ADBUFFOMR-UPD-ATTR            
068400     END-IF                                                               
068500     IF MID-ADBUFFGANG-UPD   = ALL '+'                                    
068600        MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFGANG-UPD                
068700     ELSE                                                                 
068800        MOVE MID-ADBUFFGANG-UPD      TO MOD-ADBUFFGANG-UPD                
068900        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-ADBUFFGANG-UPD-ATTR           
069000     END-IF                                                               
069100     IF MID-ADBUFFPL-UPD    = ALL '+'                                     
069200        MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFPL-UPD                  
069300     ELSE                                                                 
069400        MOVE MID-ADBUFFPL-UPD        TO MOD-ADBUFFPL-UPD                  
069500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-ADBUFFPL-UPD-ATTR             
069600     END-IF                                                               
069700     .                                                                    
069800     EJECT                                                                
069900 F-LAES-VISA-INFO SECTION.                                                
070000                                                                          
070100     PERFORM FA-LAES-GRUNDDATA                                            
070200                                                                          
070300     IF SEGMENT-SAKNAS                                                    
070400        MOVE ERR-MISSING-IN-ARTREG  TO MED-IDMFSFEL                       
070500        CALL WMEDKONV USING MED-WMEDAREA                                  
070600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
070700        PERFORM MFS-RENSA-FAELT-UT                                        
070800     ELSE                                                                 
070900        PERFORM IMS-GU-ARTD01                                             
071000        IF SEGMENT-SAKNAS                                                 
071100          MOVE ERR-MISSING-IN-BUFF-REG TO MED-IDMFSFEL                    
071200          CALL WMEDKONV USING MED-WMEDAREA                                
071300          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
071400          PERFORM MFS-RENSA-FAELT-UT-BUFFERT                              
071500        ELSE                                                              
071600                                                                          
071700           MOVE +1      TO RAD-IX                                         
071800           PERFORM IMS-GNP-ARTD11                                         
071900           IF SEGMENT-FINNS                                               
072000              MOVE ARTD-SALDO-ADBUFFOMR   TO                              
072100                                   W-MINKEY-ADBUFFOMR-ENTER               
072200              MOVE ARTD-SALDO-ADBUFFGANG  TO                              
072300                                   W-MINKEY-ADBUFFGANG-ENTER              
072400              MOVE ARTD-SALDO-ADBUFFPL    TO                              
072500                                   W-MINKEY-ADBUFFPL-ENTER                
072600              MOVE ARTD-SALDO-DABUFPAF    TO                              
072700                                   W-MINKEY-DABUFPAF-ENTER                
072800              MOVE '6305'                 TO W-MINKEY-IDTRANS             
072900*CURSOR TILL ARTNR ?                                                      
073000*             MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDCMDVAL-ATTR (1)          
073100           ELSE                                                           
073200              MOVE ZERO  TO        W-MINKEY-ADBUFFOMR-ENTER               
073300                                   W-MINKEY-ADBUFFGANG-ENTER              
073400                                   W-MINKEY-ADBUFFPL-ENTER                
073500                                   W-MINKEY-DABUFPAF-ENTER                
073600           END-IF                                                         
073700                                                                          
073800           PERFORM UNTIL SEGMENT-SAKNAS OR RAD-IX > MAX-IX                
073900                                                                          
074000              PERFORM FB-RADINFO                                          
074100              PERFORM FC-SUMMA-RAD                                        
074200              ADD +1      TO RAD-IX                                       
074300              PERFORM IMS-GNP-ARTD11                                      
074400                                                                          
074500           END-PERFORM                                                    
074600                                                                          
074700           IF SEGMENT-FINNS                                               
074800              MOVE ARTD-SALDO-ADBUFFOMR   TO                              
074900                                   W-MINKEY-ADBUFFOMR-NEXT                
075000              MOVE ARTD-SALDO-ADBUFFGANG  TO                              
075100                                   W-MINKEY-ADBUFFGANG-NEXT               
075200              MOVE ARTD-SALDO-ADBUFFPL    TO                              
075300                                   W-MINKEY-ADBUFFPL-NEXT                 
075400              MOVE ARTD-SALDO-DABUFPAF    TO                              
075500                                   W-MINKEY-DABUFPAF-NEXT                 
075600                                                                          
075700              IF MED-IDMFSINF = SPACE                                     
075800                MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF               
075900                CALL WMEDKONV USING MED-WMEDAREA                          
076000                MOVE MED-TEMFSINF           TO MOD-TEMFSINF               
076100              END-IF                                                      
076200                                                                          
076300              PERFORM UNTIL SEGMENT-SAKNAS                                
076400                PERFORM FC-SUMMA-RAD                                      
076500                PERFORM IMS-GNP-ARTD11                                    
076600              END-PERFORM                                                 
076700           ELSE                                                           
076800              MOVE ZERO       TO W-MINKEY-ADBUFFOMR-NEXT                  
076900                                 W-MINKEY-DABUFPAF-NEXT                   
077000                                 W-MINKEY-ADBUFFGANG-NEXT                 
077100                                 W-MINKEY-ADBUFFPL-NEXT                   
077200              IF MED-IDMFSINF = SPACE                                     
077300                MOVE INF-LAST-PAGE          TO MED-IDMFSINF               
077400                CALL WMEDKONV USING MED-WMEDAREA                          
077500                MOVE MED-TEMFSINF           TO MOD-TEMFSINF               
077600              END-IF                                                      
077700           END-IF                                                         
077800                                                                          
077900           MOVE W-MINKEY-WDD811KY    TO MSGI-SPAR-AREA                    
078000           MOVE '002'                TO MSGI-KDCALL                       
078100           MOVE '6305'               TO MSGI-IDTRANS                      
078200           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
078300           IF MFS-UPDATE                                                  
078400             PERFORM IMS-GU-ARTD01                                        
078500             MOVE ZERO              TO W-ADBUFFOMR-MIN                    
078600                                       W-ADBUFFGANG-MIN                   
078700                                       W-ADBUFFPL-MIN                     
078800             PERFORM IMS-GNP-ARTD11                                       
078900             MOVE ZERO              TO WS-SUBUFF-F                        
079000                                       WS-SUKOLLI-F                       
079100                                                                          
079200             PERFORM UNTIL SEGMENT-SAKNAS                                 
079300               PERFORM FC-SUMMA-RAD                                       
079400               PERFORM IMS-GNP-ARTD11                                     
079500             END-PERFORM                                                  
079600                                                                          
079700             MOVE WS-SUBUFF-F       TO MOD-SUBUFF-F                       
079800             MOVE WS-SUKOLLI-F      TO MOD-SUKOLLI-F                      
079900           ELSE                                                           
080000                                                                          
080100             IF MFS-NEXT OR MFS-ENTER                                     
080200               MOVE MFS-ROER-EJ-FAELT TO MOD-SUBUFF-F                     
080300                                         MOD-SUKOLLI-F                    
080400             ELSE                                                         
080500               MOVE WS-SUBUFF-F       TO MOD-SUBUFF-F                     
080600               MOVE WS-SUKOLLI-F      TO MOD-SUKOLLI-F                    
080700             END-IF                                                       
080800           END-IF                                                         
080900        END-IF                                                            
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300 FA-LAES-GRUNDDATA SECTION.                                               
081400                                                                          
081500     PERFORM IMS-GU-ARTS11                                                
081600     IF SEGMENT-FINNS                                                     
081700        MOVE ARTS-SLAG-ADLAGOMR     TO MOD-ADLAGOMR                       
081800        MOVE ARTS-SLAG-ADGANG       TO MOD-ADGANG                         
081900        MOVE ARTS-SLAG-ADPLATS      TO MOD-ADPLATS                        
082000        MOVE ARTS-SLAG-KVLS         TO MOD-KVLS                           
082100        PERFORM IMS-GU-BENA11                                             
082200        IF SEGMENT-FINNS                                                  
082300          MOVE BENA-TEXT-BEART      TO MOD-BEART                          
082400        ELSE                                                              
082500          MOVE MFS-RENSA-FAELT      TO MOD-BEART                          
082600        END-IF                                                            
082700     END-IF                                                               
082800                                                                          
082900     .                                                                    
083000     EJECT                                                                
083100 FB-RADINFO        SECTION.                                               
083200                                                                          
083300     MOVE ARTD-SALDO-ADBUFFOMR   TO MOD-ADBUFFOMR (RAD-IX)                
083400     MOVE ARTD-SALDO-ADBUFFGANG  TO MOD-ADBUFFGANG (RAD-IX)               
083500     MOVE ARTD-SALDO-ADBUFFPL    TO MOD-ADBUFFPL  (RAD-IX)                
083501     MOVE ARTD-SALDO-DABUFPAF    TO MOD-DABUFPAF  (RAD-IX)                
083700     MOVE ARTD-SALDO-KVBUFF-F    TO MOD-KVBUFF-F  (RAD-IX)                
083800     MOVE ARTD-SALDO-KVKOLLI-F   TO MOD-KVKOLLI-F (RAD-IX)                
083900                                                                          
084000     .                                                                    
084100     EJECT                                                                
084200 FC-SUMMA-RAD SECTION.                                                    
084300                                                                          
084400     ADD  ARTD-SALDO-KVBUFF-F    TO WS-SUBUFF-F                           
084500     ADD  ARTD-SALDO-KVKOLLI-F   TO WS-SUKOLLI-F                          
084600     .                                                                    
084700     EJECT                                                                
084800 G-KOLLA-INPUT SECTION.                                                   
084900                                                                          
085000     MOVE JA  TO INDATA-SW                                                
085100     IF MID-KDCMDVAL (1) = ALL '+' AND                                    
085200        MID-KDCMDVAL (2) = ALL '+' AND                                    
085300        MID-KDCMDVAL (3) = ALL '+' AND                                    
085400        MID-KDCMDVAL (4) = ALL '+' AND                                    
085500        MID-KDCMDVAL (5) = ALL '+' AND                                    
085600        MID-KDCMDVAL (6) = ALL '+' AND                                    
085700        MID-KDCMDVAL (7) = ALL '+' AND                                    
085800        MID-INPUT-SALDO (1) = ALL '+' AND                                 
085900        MID-INPUT-SALDO (2) = ALL '+' AND                                 
086000        MID-INPUT-SALDO (3) = ALL '+' AND                                 
086100        MID-INPUT-SALDO (4) = ALL '+' AND                                 
086200        MID-INPUT-SALDO (5) = ALL '+' AND                                 
086300        MID-INPUT-SALDO (6) = ALL '+' AND                                 
086400        MID-INPUT-SALDO (7) = ALL '+' AND                                 
086500        MID-INPUT-RAD   = ALL '+'     AND                                 
086600       (WS-KVBUFF           = ZERO    OR                                  
086700        WS-ADBUFFOMR        = ZERO    OR                                  
086800        WS-ADBUFFPL         = ZERO)                                       
086900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
087000       CALL WMEDKONV USING MED-WMEDAREA                                   
087100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
087200       PERFORM MFS-RENSA-FAELT-IN                                         
087300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
087400       MOVE NEJ TO INDATA-SW                                              
087500     ELSE                                                                 
087600                                                                          
087700       MOVE SPACE TO MED-IDMFSFEL                                         
087800                                                                          
087900       PERFORM GA-KOLLA-RADER                                             
088000       IF MID-INPUT-RAD NOT = ALL '+'                                     
088100         PERFORM GB-KOLLA-INPUT-RAD                                       
088200         IF INDATA-OK                                                     
088300           PERFORM GC-KOLLA-KONFLIKT                                      
088400         END-IF                                                           
088500       END-IF                                                             
088600                                                                          
088700       IF INDATA-FEL                                                      
088800         IF MED-IDMFSFEL = SPACE                                          
088900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
089000         END-IF                                                           
089100         CALL WMEDKONV USING MED-WMEDAREA                                 
089200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
089300         PERFORM MFS-ROER-EJ-FAELT-UT                                     
089400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
089500       ELSE                                                               
089600         PERFORM GD-KOLL-MOT-DB                                           
089700         IF INDATA-FEL                                                    
089800           IF MED-IDMFSFEL = SPACE                                        
089900             MOVE ERR-MISSING-IN-REG   TO MED-IDMFSFEL                    
090000           END-IF                                                         
090100           CALL WMEDKONV USING MED-WMEDAREA                               
090200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
090300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
090400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
090500         END-IF                                                           
090600       END-IF                                                             
090700     END-IF                                                               
090800     .                                                                    
090900     EJECT                                                                
091000 GA-KOLLA-RADER      SECTION.                                             
091100                                                                          
091200     MOVE +1      TO RAD-IX                                               
091300     PERFORM UNTIL RAD-IX > MAX-IX                                        
091400        IF MID-KDCMDVAL (RAD-IX) = ALL '+'                                
091500          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR(RAD-IX)          
091600        ELSE                                                              
091700          IF MID-KDCMDVAL(RAD-IX) = 'OUT' OR 'IN ' OR 'D  '               
091800                                 OR 'O  ' OR 'I  '                        
091900            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR(RAD-IX)        
092000            IF MID-INPUT-SALDO (RAD-IX) = ALL '+'                         
092100              IF MID-KDCMDVAL(RAD-IX) = 'D  '                             
092200                CONTINUE                                                  
092300              ELSE                                                        
092400                MOVE MFS-NUM-FAELT-FEL   TO                               
092500                            MOD-KVBUFF-F-IN-ATTR(RAD-IX)                  
092600                            MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                 
092700                MOVE NEJ TO INDATA-SW                                     
092800              END-IF                                                      
092900            END-IF                                                        
093000          ELSE                                                            
093100            MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(RAD-IX)        
093200            MOVE NEJ TO INDATA-SW                                         
093300          END-IF                                                          
093400        END-IF                                                            
093500        PERFORM GAA-KOLLA-ANTAL                                           
093600        ADD +1 TO RAD-IX                                                  
093700     END-PERFORM                                                          
093800                                                                          
093900     .                                                                    
094000     EJECT                                                                
094100 GAA-KOLLA-ANTAL     SECTION.                                             
094200                                                                          
094300     IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                            
094400       IF MID-KDCMDVAL (RAD-IX) = ALL '+' OR                              
094500          MID-KDCMDVAL (RAD-IX) = 'D  '                                   
094600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(RAD-IX)           
094700         MOVE NEJ TO INDATA-SW                                            
094800       END-IF                                                             
094900       IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                          
095000         IF MID-KVBUFF-F-IN (RAD-IX) NUMERIC                              
095100           MOVE MFS-NUM-FAELT-RAETT TO                                    
095200                         MOD-KVBUFF-F-IN-ATTR(RAD-IX)                     
095300         ELSE                                                             
095400           MOVE MFS-NUM-FAELT-FEL   TO                                    
095500                         MOD-KVBUFF-F-IN-ATTR(RAD-IX)                     
095600           MOVE NEJ TO INDATA-SW                                          
095700         END-IF                                                           
095800       ELSE                                                               
095900         MOVE MFS-NUM-FAELT-RAETT TO                                      
096000                       MOD-KVBUFF-F-IN-ATTR(RAD-IX)                       
096100       END-IF                                                             
096200                                                                          
096300       IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'                         
096400         IF MID-KVKOLLI-F-IN (RAD-IX) NUMERIC                             
096500           MOVE MFS-NUM-FAELT-RAETT TO                                    
096600                         MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                    
096700         ELSE                                                             
096800           MOVE MFS-NUM-FAELT-FEL   TO                                    
096900                         MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                    
097000           MOVE NEJ TO INDATA-SW                                          
097100         END-IF                                                           
097200       ELSE                                                               
097300         MOVE MFS-NUM-FAELT-RAETT TO                                      
097400                       MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                      
097500       END-IF                                                             
097600                                                                          
097700       IF MID-KDCMDVAL (RAD-IX) = 'D  ' OR '+++'                          
097800         MOVE MFS-ALFA-FAELT-FEL   TO                                     
097900                       MOD-KDCMDVAL-ATTR(RAD-IX)                          
098000         MOVE MFS-NUM-FAELT-FEL   TO                                      
098100                       MOD-KVBUFF-F-IN-ATTR(RAD-IX)                       
098200                       MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                      
098300         MOVE NEJ TO INDATA-SW                                            
098400         MOVE ERR-CONFLICT-FLDS TO MED-IDMFSFEL                           
098500       END-IF                                                             
098600                                                                          
098700     ELSE                                                                 
098800       IF MID-KDCMDVAL (RAD-IX) = 'IN ' OR 'OUT'                          
098900                               OR 'I  ' OR 'O  '                          
099000         IF WS-KVBUFF NUMERIC                                             
099100           CONTINUE                                                       
099200         ELSE                                                             
099300           MOVE MFS-ALFA-FAELT-FEL   TO                                   
099400                         MOD-KDCMDVAL-ATTR(RAD-IX)                        
099500           MOVE NEJ TO INDATA-SW                                          
099600         END-IF                                                           
099700       END-IF                                                             
099800     END-IF                                                               
099900     .                                                                    
100000     EJECT                                                                
100100 GB-KOLLA-INPUT-RAD  SECTION.                                             
100200                                                                          
100300     IF MID-ADBUFFOMR-UPD = ALL '+'                                       
100400       IF MID-ADBUFFGANG-UPD = ALL '+' AND                                
100500          MID-ADBUFFPL-UPD = ALL '+'                                      
100600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFOMR-UPD-ATTR              
100700       ELSE                                                               
100800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR              
100900                                      MOD-ADBUFFGANG-UPD-ATTR             
101000                                      MOD-ADBUFFPL-UPD-ATTR               
101100         MOVE NEJ TO INDATA-SW                                            
101200       END-IF                                                             
101300     ELSE                                                                 
101400       IF MID-ADBUFFOMR-UPD NUMERIC AND                                   
101500          MID-ADBUFFOMR-UPD > ZERO                                        
101600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFOMR-UPD-ATTR              
101700       ELSE                                                               
101800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR              
101900         MOVE NEJ TO INDATA-SW                                            
102000       END-IF                                                             
102100     END-IF                                                               
102200                                                                          
102300     IF MID-ADBUFFGANG-UPD = ALL '+'                                      
102400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFGANG-UPD-ATTR             
102500     ELSE                                                                 
102600       IF MID-ADBUFFGANG-UPD NUMERIC                                      
102700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFGANG-UPD-ATTR             
102800       ELSE                                                               
102900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFGANG-UPD-ATTR             
103000         MOVE NEJ TO INDATA-SW                                            
103100       END-IF                                                             
103200     END-IF                                                               
103300                                                                          
103400     IF MID-ADBUFFPL-UPD = ALL '+'                                        
103500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFPL-UPD-ATTR               
103600     ELSE                                                                 
103700       IF MID-ADBUFFPL-UPD NUMERIC                                        
103800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFPL-UPD-ATTR               
103900       ELSE                                                               
104000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFPL-UPD-ATTR               
104100         MOVE NEJ TO INDATA-SW                                            
104200       END-IF                                                             
104300     END-IF                                                               
104400                                                                          
104500     IF MID-KVBUFF-F-UPD = ALL '+'                                        
104600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBUFF-F-UPD-ATTR               
104700     ELSE                                                                 
104800       IF MID-KVBUFF-F-UPD NUMERIC                                        
104900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBUFF-F-UPD-ATTR               
105000       ELSE                                                               
105100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVBUFF-F-UPD-ATTR               
105200         MOVE NEJ TO INDATA-SW                                            
105300       END-IF                                                             
105400     END-IF                                                               
105500                                                                          
105600     IF MID-KVKOLLI-F-UPD = ALL '+'                                       
105700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOLLI-F-UPD-ATTR              
105800     ELSE                                                                 
105900       IF MID-KVKOLLI-F-UPD NUMERIC                                       
106000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOLLI-F-UPD-ATTR              
106100       ELSE                                                               
106200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVKOLLI-F-UPD-ATTR              
106300         MOVE NEJ TO INDATA-SW                                            
106400       END-IF                                                             
106500     END-IF                                                               
106600                                                                          
106700     IF MID-ADBUFFOMR-UPD = ALL    '+'                                    
106800       IF MID-KVBUFF-F-UPD = ALL   '+' AND                                
106900          MID-KVKOLLI-F-UPD = ALL  '+'                                    
107000         CONTINUE                                                         
107100       ELSE                                                               
107200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR              
107300                                      MOD-ADBUFFGANG-UPD-ATTR             
107400                                      MOD-ADBUFFPL-UPD-ATTR               
107500         MOVE NEJ TO INDATA-SW                                            
107600       END-IF                                                             
107700     END-IF                                                               
107800     .                                                                    
107900     EJECT                                                                
108000 GC-KOLLA-KONFLIKT SECTION.                                               
108100                                                                          
108200     MOVE +1      TO RAD-IX                                               
108300     PERFORM UNTIL RAD-IX > MAX-IX                                        
108400        IF MID-KDCMDVAL (RAD-IX) = ALL '+'                                
108500          CONTINUE                                                        
108600        ELSE                                                              
108700          MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL                       
108800          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(RAD-IX)          
108900                                       MOD-ADBUFFOMR-UPD-ATTR             
109000                                       MOD-ADBUFFGANG-UPD-ATTR            
109100                                       MOD-ADBUFFPL-UPD-ATTR              
109200                                       MOD-KVBUFF-F-UPD-ATTR              
109300                                       MOD-KVKOLLI-F-UPD-ATTR             
109400          MOVE NEJ TO INDATA-SW                                           
109500        END-IF                                                            
109600        ADD +1 TO RAD-IX                                                  
109700     END-PERFORM                                                          
109800     .                                                                    
109900     EJECT                                                                
110000 GD-KOLL-MOT-DB      SECTION.                                             
110100                                                                          
110200     PERFORM IMS-GU-ARTC11                                                
110300     IF SEGMENT-SAKNAS                                                    
110400        MOVE NEJ TO INDATA-SW                                             
110500        MOVE ERR-MISSING-IN-ARTREG  TO MED-IDMFSFEL                       
110600     ELSE                                                                 
110700       PERFORM IMS-GU-ARTS11                                              
110800       IF SEGMENT-SAKNAS                                                  
110900          MOVE NEJ TO INDATA-SW                                           
111000          MOVE ERR-MISSING-IN-ARTREG  TO MED-IDMFSFEL                     
111100       END-IF                                                             
111200     END-IF                                                               
111300                                                                          
111400     PERFORM GDA-UPD-FRAN-NYCKELRAD                                       
111500                                                                          
111600     MOVE +1      TO RAD-IX                                               
111700     PERFORM UNTIL RAD-IX > MAX-IX                                        
111800        IF MID-KDCMDVAL (RAD-IX) = ALL '+'                                
111900          CONTINUE                                                        
112000        ELSE                                                              
112100          INSPECT MID-ADBUFFOMR (RAD-IX)                                  
112200                                   REPLACING LEADING SPACE BY ZERO        
112300          MOVE MID-ADBUFFOMR (RAD-IX) TO W-ADBUFFOMR                      
112400          INSPECT MID-ADBUFFGANG (RAD-IX)                                 
112500                                 REPLACING LEADING SPACE BY ZERO          
112600          MOVE MID-ADBUFFGANG (RAD-IX)   TO W-ADBUFFGANG                  
112700          INSPECT MID-ADBUFFPL (RAD-IX)                                   
112800                                REPLACING LEADING SPACE BY ZERO           
112900          MOVE MID-ADBUFFPL (RAD-IX)    TO W-ADBUFFPL                     
113000          INSPECT MID-DABUFPAF (RAD-IX)                                   
113100                                REPLACING LEADING SPACE BY ZERO           
113200          MOVE MID-DABUFPAF (RAD-IX)    TO W-DABUFPAF                     
113300                                                                          
113400          PERFORM IMS-GU-ARTD11                                           
113500          IF SEGMENT-SAKNAS                                               
113600            MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(RAD-IX)        
113700            MOVE NEJ TO INDATA-SW                                         
113800          ELSE                                                            
113900            IF MID-KDCMDVAL (RAD-IX) = 'OUT' OR 'O  '                     
114000              IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                   
114100                IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                 
114200                   MOVE MID-KVBUFF-F-IN (RAD-IX) TO WS-ANTAL              
114300                   IF WS-ANTAL        > ARTD-SALDO-KVBUFF-F               
114400                     MOVE MFS-ALFA-FAELT-FEL TO                           
114500                                     MOD-KDCMDVAL-ATTR(RAD-IX)            
114600                                     MOD-KVBUFF-F-IN-ATTR(RAD-IX)         
114700                     MOVE NEJ TO INDATA-SW                                
114800                     MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL            
114900                   END-IF                                                 
115000                END-IF                                                    
115100                IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'                
115200                   MOVE MID-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL             
115300                   IF WS-ANTAL         > ARTD-SALDO-KVKOLLI-F             
115400                     MOVE MFS-ALFA-FAELT-FEL TO                           
115500                                   MOD-KDCMDVAL-ATTR(RAD-IX)              
115600                                   MOD-KVKOLLI-F-IN-ATTR(RAD-IX)          
115700                     MOVE NEJ TO INDATA-SW                                
115800                     MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL            
115900                   END-IF                                                 
116000                END-IF                                                    
116100              ELSE                                                        
116200                MOVE WS-KVBUFF TO WS-ANTAL                                
116300                IF WS-ANTAL  > ARTD-SALDO-KVBUFF-F                        
116400                  MOVE MFS-ALFA-FAELT-FEL TO                              
116500                                MOD-KDCMDVAL-ATTR(RAD-IX)                 
116600                  MOVE NEJ TO INDATA-SW                                   
116700                  MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL               
116800                END-IF                                                    
116900              END-IF                                                      
117000                                                                          
117100            END-IF                                                        
117200          END-IF                                                          
117300        END-IF                                                            
117400                                                                          
117500        ADD +1 TO RAD-IX                                                  
117600     END-PERFORM                                                          
117700                                                                          
117800     IF MID-INPUT-RAD NOT = ALL '+'                                       
117900       MOVE MID-ADBUFFOMR-UPD TO W-ADBUFFOMR                              
118000       IF MID-ADBUFFGANG-UPD NOT = ALL '+'                                
118100         MOVE MID-ADBUFFGANG-UPD TO W-ADBUFFGANG                          
118200       ELSE                                                               
118300         MOVE ZERO               TO W-ADBUFFGANG                          
118400       END-IF                                                             
118500       IF MID-ADBUFFPL-UPD NOT = ALL '+'                                  
118600         MOVE MID-ADBUFFPL-UPD TO W-ADBUFFPL                              
118700       ELSE                                                               
118800         MOVE ZERO             TO W-ADBUFFPL                              
118900       END-IF                                                             
119000                                                                          
119100       PERFORM IMS-GU-ARTD11                                              
119200       IF SEGMENT-FINNS                                                   
119300         MOVE MFS-NUM-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR               
119400                                     MOD-ADBUFFGANG-UPD-ATTR              
119500                                     MOD-ADBUFFPL-UPD-ATTR                
119600         MOVE NEJ TO INDATA-SW                                            
119700         MOVE ERR-ALREADY-EXIST   TO MED-IDMFSFEL                         
119800       ELSE                                                               
119900        MOVE W-IDDC      TO WS-IDDC                                       
120000        IF NDC-US OR NDC-AU                                               
120100         CONTINUE                                                         
120200        ELSE                                                              
120300         IF W-ADBUFFOMR > 19 AND                                          
120400            W-ADBUFFOMR < 30                                              
120600            MOVE W-ADBUFFOMR     TO W-ADBUFFOM-A1KY-MIN                   
120700                                    W-ADBUFFOM-A1KY-MAX                   
120900            MOVE W-ADBUFFGANG    TO W-ADBUFGAN-A1KY-MIN                   
121000                                    W-ADBUFGAN-A1KY-MAX                   
121200            MOVE W-ADBUFFPL      TO W-ADBUFPL-A1KY-MIN                    
121300                                    W-ADBUFPL-A1KY-MAX                    
121400            PERFORM IMS-GU-WDD8A1                                         
121500            IF SEGMENT-FINNS                                              
121510               MOVE 'Part no in same buffer loc.  '                       
121520                      TO MOD-MOD-LEDTEXT                                  
121530               MOVE SEQA-IDARTNR TO MOD-IDARTNR                           
121540                                                                          
121600              MOVE MFS-NUM-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR          
121700                                          MOD-ADBUFFGANG-UPD-ATTR         
121800                                          MOD-ADBUFFPL-UPD-ATTR           
121900              MOVE NEJ TO INDATA-SW                                       
122000              MOVE ERR-ALREADY-EXIST   TO MED-IDMFSFEL                    
122800            END-IF                                                        
122900          END-IF                                                          
122900         END-IF                                                           
123000       END-IF                                                             
123100     END-IF                                                               
123200                                                                          
123300     IF UPD-FRAN-NYCKELRAD                                                
123400       MOVE WS-ADBUFFOMR      TO W-ADBUFFOMR                              
123500       MOVE WS-ADBUFFGANG     TO W-ADBUFFGANG                             
123600       MOVE WS-ADBUFFPL       TO W-ADBUFFPL                               
123700       PERFORM IMS-GU-ARTD11                                              
123800       IF SEGMENT-FINNS                                                   
123900         MOVE NEJ TO INDATA-SW                                            
124000         MOVE ERR-ALREADY-EXIST   TO MED-IDMFSFEL                         
124100       ELSE                                                               
124200        MOVE W-IDDC  TO WS-IDDC                                           
124300        IF NDC-US                                                         
124400         CONTINUE                                                         
124500        ELSE                                                              
124600         IF W-ADBUFFOMR > 19 AND                                          
124700            W-ADBUFFOMR < 30                                              
124900            MOVE W-ADBUFFOMR     TO W-ADBUFFOM-A1KY-MIN                   
125000                                    W-ADBUFFOM-A1KY-MAX                   
125200            MOVE W-ADBUFFGANG    TO W-ADBUFGAN-A1KY-MIN                   
125300                                    W-ADBUFGAN-A1KY-MAX                   
125500            MOVE W-ADBUFFPL      TO W-ADBUFPL-A1KY-MIN                    
125600                                    W-ADBUFPL-A1KY-MAX                    
125700            PERFORM IMS-GU-WDD8A1                                         
125800            IF SEGMENT-FINNS                                              
125810               MOVE 'Part no in same buffer loc.  '                       
125820                      TO MOD-MOD-LEDTEXT                                  
125830               MOVE SEQA-IDARTNR TO MOD-IDARTNR                           
125840                                                                          
125900               MOVE NEJ TO INDATA-SW                                      
126000               MOVE ERR-ALREADY-EXIST   TO MED-IDMFSFEL                   
126700            END-IF                                                        
126800         END-IF                                                           
126900        END-IF                                                            
127000       END-IF                                                             
127100     END-IF                                                               
127200                                                                          
127300     .                                                                    
127400     EJECT                                                                
127500 GDA-UPD-FRAN-NYCKELRAD SECTION.                                          
127600                                                                          
127700     IF MID-KDCMDVAL (1) = ALL '+' AND                                    
127800        MID-KDCMDVAL (2) = ALL '+' AND                                    
127900        MID-KDCMDVAL (3) = ALL '+' AND                                    
128000        MID-KDCMDVAL (4) = ALL '+' AND                                    
128100        MID-KDCMDVAL (5) = ALL '+' AND                                    
128200        MID-KDCMDVAL (6) = ALL '+' AND                                    
128300        MID-KDCMDVAL (7) = ALL '+' AND                                    
128400        MID-INPUT-SALDO (1) = ALL '+' AND                                 
128500        MID-INPUT-SALDO (2) = ALL '+' AND                                 
128600        MID-INPUT-SALDO (3) = ALL '+' AND                                 
128700        MID-INPUT-SALDO (4) = ALL '+' AND                                 
128800        MID-INPUT-SALDO (5) = ALL '+' AND                                 
128900        MID-INPUT-SALDO (6) = ALL '+' AND                                 
129000        MID-INPUT-SALDO (7) = ALL '+' AND                                 
129100        MID-INPUT-RAD   = ALL '+'                                         
129200       MOVE JA TO UPD-FRAN-NYCKELRAD-SW                                   
129300     END-IF                                                               
129400     .                                                                    
129500     EJECT                                                                
129600 H-UPPDATERA SECTION.                                                     
129700                                                                          
129800     PERFORM IMS-GU-ARTD01                                                
129900     IF SEGMENT-SAKNAS                                                    
130000       MOVE W-IDARTNR      TO ARTD-ART-IDARTNR                            
130100       PERFORM IMS-ISRT-ARTD01                                            
130200     END-IF                                                               
130300     IF MID-INPUT-RAD = ALL '+'                                           
130400       MOVE +1   TO RAD-IX                                                
130500       PERFORM UNTIL RAD-IX > MAX-IX                                      
130600         IF MID-KDCMDVAL (RAD-IX) = ALL '+'                               
130700           CONTINUE                                                       
130800         ELSE                                                             
130900           INSPECT MID-ADBUFFOMR (RAD-IX)                                 
131000                                 REPLACING LEADING SPACE BY ZERO          
131100           MOVE MID-ADBUFFOMR (RAD-IX) TO W-ADBUFFOMR                     
131200                                          W-ADBUFFOMR-MIN                 
131300           INSPECT MID-ADBUFFGANG (RAD-IX)                                
131400                                  REPLACING LEADING SPACE BY ZERO         
131500           MOVE MID-ADBUFFGANG (RAD-IX) TO W-ADBUFFGANG                   
131600                                           W-ADBUFFGANG-MIN               
131700           INSPECT MID-ADBUFFPL   (RAD-IX)                                
131800                                  REPLACING LEADING SPACE BY ZERO         
131900           MOVE MID-ADBUFFPL   (RAD-IX) TO W-ADBUFFPL                     
132000                                           W-ADBUFFPL-MIN                 
132100           INSPECT MID-DABUFPAF   (RAD-IX)                                
132200                                  REPLACING LEADING SPACE BY ZERO         
132300           MOVE MID-DABUFPAF   (RAD-IX) TO W-DABUFPAF                     
132400                                           W-DABUFPAF-MIN                 
132500                                                                          
132600           PERFORM IMS-GHU-ARTD11                                         
132700           IF MID-KDCMDVAL (RAD-IX) = 'D  '                               
132800             PERFORM IMS-DLET-ARTD                                        
132900*            SLÄCKNING AV BUFFERTPLATS (WDJ9)                             
133000             PERFORM HB-STANG-BUFFERTPLATS-WDJ9                           
133100           ELSE                                                           
133200             IF MID-KDCMDVAL (RAD-IX) = 'IN ' OR 'I  '                    
133300               IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                  
133400                 IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                
133500                   MOVE MID-KVBUFF-F-IN (RAD-IX) TO WS-ANTAL              
133600                   ADD WS-ANTAL            TO ARTD-SALDO-KVBUFF-F         
133700                 END-IF                                                   
133800                 IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'               
133900                   MOVE MID-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL             
134000                   ADD WS-ANTAL          TO ARTD-SALDO-KVKOLLI-F          
134100                 END-IF                                                   
134200               ELSE                                                       
134300                 MOVE WS-KVBUFF        TO WS-ANTAL                        
134400                 ADD WS-ANTAL          TO ARTD-SALDO-KVBUFF-F             
134500               END-IF                                                     
134600               PERFORM IMS-REPL-ARTD                                      
134700             ELSE                                                         
134800               IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                  
134900                 IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                
135000                   MOVE MID-KVBUFF-F-IN (RAD-IX)  TO WS-ANTAL             
135100                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVBUFF-F             
135200                 END-IF                                                   
135300                 IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'               
135400                   MOVE MID-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL             
135500                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVKOLLI-F            
135600                 END-IF                                                   
135700               ELSE                                                       
135800                 MOVE WS-KVBUFF    TO WS-ANTAL                            
135900                 SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVBUFF-F               
136000               END-IF                                                     
136100                                                                          
136200               IF (ARTD-SALDO-ADBUFFOMR > 19  AND                         
136300                   ARTD-SALDO-ADBUFFOMR < 30) AND                         
136400                  ARTD-SALDO-KVBUFF-F  = ZERO  AND                        
136500                  ARTD-SALDO-KVBUFF-OF = ZERO  AND                        
136600                  ARTD-SALDO-KVKOLLI-F = ZERO  AND                        
136700                  ARTD-SALDO-KVKOLLI-OF = ZERO                            
136800                  MOVE W-IDDC      TO WS-IDDC                             
136900                  IF NDC-US                                               
137000                      PERFORM IMS-REPL-ARTD                               
137100                  ELSE                                                    
137200                      PERFORM IMS-DLET-ARTD                               
137300                      PERFORM HB-STANG-BUFFERTPLATS-WDJ9                  
137400                  END-IF                                                  
137500               ELSE                                                       
137600                 PERFORM IMS-REPL-ARTD                                    
137700               END-IF                                                     
137800                                                                          
137900             END-IF                                                       
138000           END-IF                                                         
138100         END-IF                                                           
138200         ADD +1 TO RAD-IX                                                 
138300       END-PERFORM                                                        
138400                                                                          
138500     ELSE                                                                 
138600       MOVE MID-ADBUFFOMR-UPD TO ARTD-SALDO-ADBUFFOMR                     
138700       IF MID-ADBUFFGANG-UPD NOT = ALL '+'                                
138800         MOVE MID-ADBUFFGANG-UPD TO ARTD-SALDO-ADBUFFGANG                 
138900       ELSE                                                               
139000         MOVE ZERO               TO ARTD-SALDO-ADBUFFGANG                 
139100       END-IF                                                             
139200       IF MID-ADBUFFPL-UPD NOT = ALL '+'                                  
139300         MOVE MID-ADBUFFPL-UPD TO ARTD-SALDO-ADBUFFPL                     
139400       ELSE                                                               
139500         MOVE ZERO             TO ARTD-SALDO-ADBUFFPL                     
139600       END-IF                                                             
139700       IF MID-KVBUFF-F-UPD NOT = ALL '+'                                  
139800         MOVE MID-KVBUFF-F-UPD TO ARTD-SALDO-KVBUFF-F                     
139900       ELSE                                                               
140000         MOVE ZERO             TO ARTD-SALDO-KVBUFF-F                     
140100       END-IF                                                             
140200                                                                          
140300                                                                          
140400       IF MID-KVKOLLI-F-UPD NOT = ALL '+'                                 
140500         MOVE MID-KVKOLLI-F-UPD TO ARTD-SALDO-KVKOLLI-F                   
140600       ELSE                                                               
140700         MOVE ZERO              TO ARTD-SALDO-KVKOLLI-F                   
140800       END-IF                                                             
140900                                                                          
141000       MOVE ZERO                TO ARTD-SALDO-KVKOLLI-OF                  
141100                                   ARTD-SALDO-KDPAF                       
141200                                   ARTD-SALDO-KDBRIST                     
141300                                   ARTD-SALDO-KVBUFF-OF                   
141400                                                                          
141500       MOVE MSGI-IDDC TO ARTD-SALDO-IDDC                                  
141600       IF ARTD-SALDO-ADBUFFOMR > 19 AND                                   
141700          ARTD-SALDO-ADBUFFOMR < 30                                       
141800           MOVE ARTD-SALDO-IDDC  TO WS-IDDC                               
141900           IF NDC-US                                                      
142000               MOVE ZERO                  TO ARTD-SALDO-DABUFPAF          
142100           ELSE                                                           
142200           MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF        
142300           END-IF                                                         
142400       ELSE                                                               
142500         MOVE ZERO                        TO ARTD-SALDO-DABUFPAF          
142600       END-IF                                                             
142700       PERFORM IMS-ISRT-ARTD11                                            
142800       PERFORM HA-UPPDATERA-WDJ9                                          
142900     END-IF                                                               
143000                                                                          
143100     IF UPD-FRAN-NYCKELRAD                                                
143200       MOVE WS-ADBUFFOMR    TO ARTD-SALDO-ADBUFFOMR                       
143300       MOVE WS-ADBUFFGANG   TO ARTD-SALDO-ADBUFFGANG                      
143400       MOVE WS-ADBUFFPL     TO ARTD-SALDO-ADBUFFPL                        
143500       MOVE WS-KVBUFF       TO ARTD-SALDO-KVBUFF-F                        
143600       IF WS-ADBUFFOMR > 19 AND                                           
143700          WS-ADBUFFOMR < 30                                               
143800        MOVE W-IDDC     TO WS-IDDC                                        
143900        IF NDC-US                                                         
144000         MOVE ZERO          TO ARTD-SALDO-KVKOLLI-F                       
144100                               ARTD-SALDO-DABUFPAF                        
144200        ELSE                                                              
144300         MOVE +1            TO ARTD-SALDO-KVKOLLI-F                       
144400         MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF          
144500        END-IF                                                            
144600       ELSE                                                               
144700         MOVE ZERO          TO ARTD-SALDO-KVKOLLI-F                       
144800                               ARTD-SALDO-DABUFPAF                        
144900       END-IF                                                             
145000       MOVE ZERO            TO ARTD-SALDO-KVBUFF-OF                       
145100                               ARTD-SALDO-KVKOLLI-OF                      
145200                               ARTD-SALDO-KDPAF                           
145300                               ARTD-SALDO-KDBRIST                         
145400       MOVE MSGI-IDDC       TO ARTD-SALDO-IDDC                            
145500       PERFORM IMS-ISRT-ARTD11                                            
145600       PERFORM HA-UPPDATERA-WDJ9                                          
145700     END-IF                                                               
145800                                                                          
145900     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
146000     CALL WMEDKONV USING MED-WMEDAREA                                     
146100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
146200     PERFORM MFS-FORM-ATTR                                                
146300     PERFORM MFS-RENSA-FAELT-IN                                           
146400     .                                                                    
146500     EJECT                                                                
146600                                                                          
146700 HA-UPPDATERA-WDJ9 SECTION.                                               
146800                                                                          
146900     PERFORM IMS-GU-LOCB01                                                
147000     IF SEGMENT-SAKNAS                                                    
147100       MOVE W-IDARTNR TO LOCB-ART-IDARTNR                                 
147200       PERFORM IMS-ISRT-LOCB01                                            
147300       PERFORM IMS-GU-LOCB01                                              
147400     END-IF                                                               
147500     IF SEGMENT-FINNS                                                     
147600       MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM                     
147700       MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                       
147800       COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                     
147900                                                     LOGG-DATUM           
148000       COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID              
148100       MOVE MSGI-IDDC            TO LOCB-HIST-IDDC                        
148200       IF UPD-FRAN-NYCKELRAD                                              
148300         MOVE WS-ADBUFFOMR       TO LOCB-HIST-ADLAGOMR                    
148400         IF WS-ADBUFFGANG NOT = ALL '+'                                   
148500           MOVE WS-ADBUFFGANG      TO LOCB-HIST-ADGANG                    
148600         ELSE                                                             
148700           MOVE ZERO               TO LOCB-HIST-ADGANG                    
148800         END-IF                                                           
148900         IF WS-ADBUFFPL NOT = ALL '+'                                     
149000           MOVE WS-ADBUFFPL        TO LOCB-HIST-ADPLATS                   
149100         ELSE                                                             
149200           MOVE ZERO               TO LOCB-HIST-ADPLATS                   
149300         END-IF                                                           
149400       ELSE                                                               
149500         MOVE MID-ADBUFFOMR-UPD  TO LOCB-HIST-ADLAGOMR                    
149600         IF MID-ADBUFFGANG-UPD NOT = ALL '+'                              
149700           MOVE MID-ADBUFFGANG-UPD TO LOCB-HIST-ADGANG                    
149800         ELSE                                                             
149900           MOVE ZERO               TO LOCB-HIST-ADGANG                    
150000         END-IF                                                           
150100         IF MID-ADBUFFPL-UPD NOT = ALL '+'                                
150200           MOVE MID-ADBUFFPL-UPD   TO LOCB-HIST-ADPLATS                   
150300         ELSE                                                             
150400           MOVE ZERO               TO LOCB-HIST-ADPLATS                   
150500         END-IF                                                           
150600       END-IF                                                             
150700       MOVE BUFFER-LOCATION      TO LOCB-HIST-KDLOC                       
150800       MOVE MSGI-IDUSER          TO LOCB-HIST-IDUSER                      
150900       MOVE SPACE                TO LOCB-HIST-IDUSER-STO                  
151000       MOVE ZERO                 TO LOCB-HIST-DASTODAT                    
151100                                                                          
151200       PERFORM IMS-ISRT-LOCB11                                            
151300     END-IF                                                               
151400     .                                                                    
151500     EJECT                                                                
151600                                                                          
151700 HB-STANG-BUFFERTPLATS-WDJ9 SECTION.                                      
151800                                                                          
151900     PERFORM IMS-GU-LOCB01                                                
152000     IF SEGMENT-SAKNAS                                                    
152100       CONTINUE                                                           
152200     ELSE                                                                 
152300       PERFORM IMS-GHNP-LOCB11                                            
152400       IF SEGMENT-FINNS                                                   
152500         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
152600           (MID-ADBUFFOMR (RAD-IX)  = LOCB-HIST-ADLAGOMR)                 
152700                                                     AND                  
152800           (MID-ADBUFFGANG (RAD-IX) = LOCB-HIST-ADGANG)                   
152900                                                     AND                  
153000           (MID-ADBUFFPL (RAD-IX)   = LOCB-HIST-ADPLATS)                  
153100                                                     AND                  
153200           (LOCB-HIST-KDLOC = 'B')                                        
153300           PERFORM IMS-GHNP-LOCB11                                        
153400         END-PERFORM                                                      
153500         IF SEGMENT-FINNS AND                                             
153600           (MID-ADBUFFOMR (RAD-IX)  = LOCB-HIST-ADLAGOMR)                 
153700                                                     AND                  
153800           (MID-ADBUFFGANG (RAD-IX) = LOCB-HIST-ADGANG)                   
153900                                                     AND                  
154000           (MID-ADBUFFPL (RAD-IX)   = LOCB-HIST-ADPLATS)                  
154100                                                     AND                  
154200           (LOCB-HIST-KDLOC = 'B')                                        
154300           MOVE FUNCTION CURRENT-DATE(1:8) TO                             
154400                                    LOCB-HIST-DASTODAT                    
154500           MOVE MSGI-IDUSER TO LOCB-HIST-IDUSER-STO                       
154600           PERFORM IMS-REPL-LOCB11                                        
154700         END-IF                                                           
154800       END-IF                                                             
154900     END-IF                                                               
155000     .                                                                    
155100                                                                          
155200 MFS-RENSA-FAELT-UT SECTION.                                              
155300                                                                          
155400*    --- ALLA UTDATA-FÄLT                                                 
155500     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR                                 
155600                             MOD-ADGANG                                   
155700                             MOD-ADPLATS                                  
155800                             MOD-BEART                                    
155900                             MOD-KVLS                                     
156000                             MOD-SUBUFF-F                                 
156100                             MOD-SUKOLLI-F                                
156200                             MOD-IDARTNR                                  
156300                             MOD-MOD-LEDTEXT                              
156400     MOVE +1 TO RAD-IX                                                    
156500     PERFORM UNTIL RAD-IX > MAX-IX                                        
156600       MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR (RAD-IX)                     
156700                               MOD-ADBUFFGANG (RAD-IX)                    
156800                               MOD-ADBUFFPL (RAD-IX)                      
156910                               MOD-DABUFPAF (RAD-IX)                      
157000                               MOD-KVBUFF-F (RAD-IX)                      
157100                               MOD-KVKOLLI-F (RAD-IX)                     
157200       ADD +1 TO RAD-IX                                                   
157300     END-PERFORM                                                          
157400     .                                                                    
157500     SKIP3                                                                
157600 MFS-RENSA-FAELT-UT-BUFFERT SECTION.                                      
157700                                                                          
157800*    --- ALLA UTDATA-FÄLT UTOM ARTIKEL UPPGIFTER                          
157900                                                                          
158000     MOVE MFS-RENSA-FAELT TO MOD-SUBUFF-F                                 
158100                             MOD-SUKOLLI-F                                
158200                             MOD-IDARTNR                                  
158300                             MOD-MOD-LEDTEXT                              
158400     MOVE +1 TO RAD-IX                                                    
158500     PERFORM UNTIL RAD-IX > MAX-IX                                        
158600       MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR (RAD-IX)                     
158700                               MOD-ADBUFFGANG (RAD-IX)                    
158800                               MOD-ADBUFFPL (RAD-IX)                      
158910                               MOD-DABUFPAF (RAD-IX)                      
159000                               MOD-KVBUFF-F (RAD-IX)                      
159100                               MOD-KVKOLLI-F (RAD-IX)                     
159200       ADD +1 TO RAD-IX                                                   
159300     END-PERFORM                                                          
159400     .                                                                    
159500     SKIP3                                                                
159600 MFS-RENSA-FAELT-IN SECTION.                                              
159700                                                                          
159800*    --- ALLA INDATA-FÄLT                                                 
159900     MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR-UPD                            
160000                             MOD-ADBUFFGANG-UPD                           
160100                             MOD-ADBUFFPL-UPD                             
160200                             MOD-KVBUFF-F-UPD                             
160300                             MOD-KVKOLLI-F-UPD                            
160400     MOVE +1 TO RAD-IX                                                    
160500     PERFORM UNTIL RAD-IX > MAX-IX                                        
160600       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL (RAD-IX)                      
160700                               MOD-KVBUFF-F-IN (RAD-IX)                   
160800                               MOD-KVKOLLI-F-IN (RAD-IX)                  
160900       ADD +1 TO RAD-IX                                                   
161000     END-PERFORM                                                          
161100     .                                                                    
161200     EJECT                                                                
161300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
161400                                                                          
161500*    --- ALLA UTDATA-FÄLT                                                 
161600     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR                               
161700                               MOD-ADGANG                                 
161800                               MOD-ADPLATS                                
161900                               MOD-BEART                                  
162000                               MOD-KVLS                                   
162100                               MOD-SUBUFF-F                               
162200                               MOD-SUKOLLI-F                              
162300     MOVE +1 TO RAD-IX                                                    
162400     PERFORM UNTIL RAD-IX > MAX-IX                                        
162500       MOVE MFS-ROER-EJ-FAELT TO MOD-ADBUFFOMR (RAD-IX)                   
162600                                 MOD-ADBUFFGANG (RAD-IX)                  
162700                                 MOD-ADBUFFPL (RAD-IX)                    
162810                                 MOD-DABUFPAF (RAD-IX)                    
162900                                 MOD-KVBUFF-F (RAD-IX)                    
163000                                 MOD-KVKOLLI-F (RAD-IX)                   
163100       ADD +1 TO RAD-IX                                                   
163200     END-PERFORM                                                          
163300     .                                                                    
163400     SKIP3                                                                
163500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
163600                                                                          
163700*    --- ALLA INDATA-FÄLT                                                 
163800     MOVE MFS-ROER-EJ-FAELT TO MOD-ADBUFFOMR-UPD                          
163900                               MOD-ADBUFFGANG-UPD                         
164000                               MOD-ADBUFFPL-UPD                           
164100                               MOD-KVBUFF-F-UPD                           
164200                               MOD-KVKOLLI-F-UPD                          
164300     MOVE +1 TO RAD-IX                                                    
164400     PERFORM UNTIL RAD-IX > MAX-IX                                        
164500       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL (RAD-IX)                    
164600                                 MOD-KVBUFF-F-IN (RAD-IX)                 
164700                                 MOD-KVKOLLI-F-IN (RAD-IX)                
164800       ADD +1 TO RAD-IX                                                   
164900     END-PERFORM                                                          
165000     .                                                                    
165100     EJECT                                                                
165200 MFS-FORM-ATTR SECTION.                                                   
165300                                                                          
165400*    --- ALLA INDATA-FÄLT                                                 
165500     MOVE MFS-FORMATETS-ATTR TO MOD-ADBUFFOMR-UPD-ATTR                    
165600                                MOD-ADBUFFGANG-UPD-ATTR                   
165700                                MOD-ADBUFFPL-UPD-ATTR                     
165800                                MOD-KVBUFF-F-UPD-ATTR                     
165900                                MOD-KVKOLLI-F-UPD-ATTR                    
166000     MOVE +1 TO RAD-IX                                                    
166100     PERFORM UNTIL RAD-IX > MAX-IX                                        
166200       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR (RAD-IX)              
166300                                  MOD-KVBUFF-F-IN-ATTR (RAD-IX)           
166400                                  MOD-KVKOLLI-F-IN-ATTR (RAD-IX)          
166500       ADD +1 TO RAD-IX                                                   
166600     END-PERFORM                                                          
166700     .                                                                    
166800     SKIP2                                                                
166900* --- IMS SEKTIONER ---                                                   
167000     SKIP3                                                                
167100 IMS-GET-MSG SECTION.                                                     
167200                                                                          
167300     MOVE '  QC' TO GODK-STATUSKODER                                      
167400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
167500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
167600     PERFORM IMS-STATUSKONTROLL                                           
167700     .                                                                    
167800     SKIP3                                                                
167900 IMS-INSERT-MSG SECTION.                                                  
168000                                                                          
168100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
168200     MOVE SPACE TO GODK-STATUSKODER                                       
168300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
168400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
168500     PERFORM IMS-STATUSKONTROLL                                           
168600     .                                                                    
168700     EJECT                                                                
168800 IMS-GU-ARTC11   SECTION.                                                 
168900                                                                          
169000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
169100          DELIMITED BY SIZE INTO SSA1                                     
169200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
169300          DELIMITED BY SIZE INTO SSA2                                     
169400     MOVE '  GE' TO GODK-STATUSKODER                                      
169500     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA SSA1 SSA2                
169600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
169700     PERFORM IMS-STATUSKONTROLL                                           
169800     .                                                                    
169900     EJECT                                                                
170000 IMS-GU-ARTS11   SECTION.                                                 
170100                                                                          
170200     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
170300          DELIMITED BY SIZE INTO SSA1                                     
170400     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
170500          DELIMITED BY SIZE INTO SSA2                                     
170600     MOVE '  GE' TO GODK-STATUSKODER                                      
170700     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-AREA SSA1 SSA2                
170800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
170900     PERFORM IMS-STATUSKONTROLL                                           
171000     .                                                                    
171100     EJECT                                                                
172200 IMS-GU-ARTD01    SECTION.                                                
172300                                                                          
172400     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
172500          DELIMITED BY SIZE INTO SSA1                                     
172600     MOVE '  GE' TO GODK-STATUSKODER                                      
172700     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1                     
172800     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
172900     PERFORM IMS-STATUSKONTROLL                                           
173000     .                                                                    
173100     SKIP3                                                                
173200 IMS-GNP-ARTD11     SECTION.                                              
173300                                                                          
173400     STRING 'WLARTD11(WDD811KY=>' W-WDD811KY-MIN-X                        
173500                    '&WDD811KY=<' W-WDD811KY-MAX-X ')'                    
173600          DELIMITED BY SIZE INTO SSA1                                     
173700     MOVE '  GE' TO GODK-STATUSKODER                                      
173800     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
173900     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
174000     PERFORM IMS-STATUSKONTROLL                                           
174100     .                                                                    
174200     SKIP3                                                                
174300 IMS-GU-ARTD11     SECTION.                                               
174400                                                                          
174500     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
174600          DELIMITED BY SIZE INTO SSA1                                     
174700     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
174800          DELIMITED BY SIZE INTO SSA2                                     
174900     MOVE '  GE' TO GODK-STATUSKODER                                      
175000     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA SSA1 SSA2                 
175100     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
175200     PERFORM IMS-STATUSKONTROLL                                           
175300     .                                                                    
175400     SKIP3                                                                
175500 IMS-GHU-ARTD11     SECTION.                                              
175600                                                                          
175700     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
175800          DELIMITED BY SIZE INTO SSA1                                     
175900     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
176000          DELIMITED BY SIZE INTO SSA2                                     
176100     MOVE '  ' TO GODK-STATUSKODER                                        
176200     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1 SSA2                
176300     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
176400     PERFORM IMS-STATUSKONTROLL                                           
176500     .                                                                    
176600     SKIP3                                                                
176700 IMS-ISRT-ARTD01 SECTION.                                                 
176800                                                                          
176900     MOVE 'WLARTD01 ' TO SSA1                                             
177000     MOVE '  II' TO GODK-STATUSKODER                                      
177100     CALL CBLTDLI USING ISRT ARTD-PCB DLI-IO-AREA SSA1                    
177200     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
177300     PERFORM IMS-STATUSKONTROLL                                           
177400     .                                                                    
177500     SKIP3                                                                
177600 IMS-ISRT-ARTD11 SECTION.                                                 
177700                                                                          
177800     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
177900          DELIMITED BY SIZE INTO SSA1                                     
178000     MOVE 'WLARTD11 ' TO SSA2                                             
178100     MOVE '  II' TO GODK-STATUSKODER                                      
178200     CALL CBLTDLI USING ISRT ARTD-PCB DLI-IO-AREA SSA1 SSA2               
178300     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
178400     PERFORM IMS-STATUSKONTROLL                                           
178500     .                                                                    
178600     SKIP3                                                                
178700 IMS-REPL-ARTD SECTION.                                                   
178800                                                                          
178900     MOVE '  ' TO GODK-STATUSKODER                                        
179000     CALL CBLTDLI USING REPL ARTD-PCB DLI-IO-AREA                         
179100     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
179200     PERFORM IMS-STATUSKONTROLL                                           
179300     .                                                                    
179400     EJECT                                                                
179500 IMS-DLET-ARTD SECTION.                                                   
179600                                                                          
179700     MOVE '  ' TO GODK-STATUSKODER                                        
179800     CALL CBLTDLI USING DLET ARTD-PCB DLI-IO-AREA                         
179900     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
180000     PERFORM IMS-STATUSKONTROLL                                           
180100     .                                                                    
180200     EJECT                                                                
180300 IMS-GU-WDD8A1 SECTION.                                                   
180400     STRING 'WDD8A1  (WDD8A1KY >' W-WDD8A1KY-MIN-X                        
181000                    '&WDD8A1KY <' W-WDD8A1KY-MAX-X ')'                    
181700            DELIMITED BY SIZE INTO SSA1                                   
181800     MOVE '  GE' TO GODK-STATUSKODER                                      
181900     CALL CBLTDLI USING GU WDD8A-PCB DLI-IO-AREA3 SSA1                    
182000     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
182100     PERFORM IMS-STATUSKONTROLL                                           
182200     .                                                                    
182300     EJECT                                                                
182400 IMS-GU-BENA11    SECTION.                                                
182500                                                                          
182600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
182700          DELIMITED BY SIZE INTO SSA1                                     
182800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
182900          DELIMITED BY SIZE INTO SSA2                                     
183000     MOVE '  GE' TO GODK-STATUSKODER                                      
183100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
183200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
183300     PERFORM IMS-STATUSKONTROLL                                           
183400     .                                                                    
183500     EJECT                                                                
183600                                                                          
183700 IMS-GU-LOCB01 SECTION.                                                   
183800                                                                          
183900     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
184000          DELIMITED BY SIZE INTO SSA1                                     
184100     MOVE '  GE' TO GODK-STATUSKODER                                      
184200     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-AREA SSA1                      
184300     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
184400     PERFORM IMS-STATUSKONTROLL                                           
184500     .                                                                    
184600     EJECT                                                                
184700                                                                          
184800 IMS-ISRT-LOCB01 SECTION.                                                 
184900                                                                          
185000     MOVE 'WLLOCB01 ' TO SSA1                                             
185100     MOVE '  ' TO GODK-STATUSKODER                                        
185200     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA SSA1                    
185300     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
185400     PERFORM IMS-STATUSKONTROLL                                           
185500     .                                                                    
185600     SKIP3                                                                
185700                                                                          
185800 IMS-GHNP-LOCB11 SECTION.                                                 
185900                                                                          
186000     STRING 'WLLOCB11(IDDC     =' W-IDDC-X ')'                            
186100             DELIMITED BY SIZE INTO SSA1                                  
186200     MOVE '  GE' TO GODK-STATUSKODER                                      
186300     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-AREA SSA1                    
186400     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
186500     PERFORM IMS-STATUSKONTROLL                                           
186600                                                                          
186700     EJECT                                                                
186800     .                                                                    
186900                                                                          
187000 IMS-REPL-LOCB11 SECTION.                                                 
187100                                                                          
187200     MOVE '  ' TO GODK-STATUSKODER                                        
187300     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-AREA                         
187400     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
187500     PERFORM IMS-STATUSKONTROLL                                           
187600     .                                                                    
187700     EJECT                                                                
187800     SKIP3                                                                
187900                                                                          
188000 IMS-ISRT-LOCB11 SECTION.                                                 
188100                                                                          
188200     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
188300          DELIMITED BY SIZE INTO SSA1                                     
188400     MOVE 'WLLOCB11 ' TO SSA2                                             
188500     MOVE '  II' TO GODK-STATUSKODER                                      
188600     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA SSA1 SSA2               
188700     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
188800     PERFORM IMS-STATUSKONTROLL                                           
188900     .                                                                    
189000     SKIP3                                                                
189100                                                                          
189200 IMS-STATUSKONTROLL SECTION.                                              
189300                                                                          
189400     SET STATUS-IX TO 1                                                   
189500     SEARCH GODK-STATUS                                                   
189600       AT END                                                             
189700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
189800         DELIMITED BY SIZE INTO FELTEXT                                   
189900         CALL FELLOG                                                      
190000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
190100         CONTINUE                                                         
190200     END-SEARCH                                                           
190300     .                                                                    
