000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4048300.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   95/04/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        BILD FÖR ATT UPPDATERA CASH ON DELIVERY FLAGGA OCH TID           
001000*        FÖR EN VISS KUND PÅ KUNDREGISTRET.                               
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLGMTA (WDB2)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T483                                              
001600*        MID:         W4I48301                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O48301                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002501*    -- CHECKED BY WY2000                                                 
002510     SKIP3                                                                
002600 77  IDPGM                       PIC X(08)   VALUE 'W4048300'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003110 77  YES                         PIC X       VALUE 'Y'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003201 01  TEST-IDDISTR                PIC 9(5)      COMP-3.                    
003202 01     FILLER REDEFINES TEST-IDDISTR.                                    
003210*  03   -COPY WWDIST51.                                                   
003300                                                                          
003400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003500                                                                          
003600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003700     88  INDATA-OK                           VALUE 'J'.                   
003800     88  INDATA-FEL                          VALUE 'N'.                   
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004500     88  EGEN-MID                            VALUE '4483'.                
004600     88  GODK-MID                            VALUE '4483'.                
004700     88  HELP-MID                            VALUE '0551'.                
004800     EJECT                                                                
004900*    --- DIVERSE ARBETSFÄLT                                               
005000 01  ARB-FAELT.                                                           
005100     05  W-DATUM                 PIC 9(6)    VALUE ZERO.                  
005200     05  W-TID                   PIC 9(8)    VALUE ZERO.                  
005400     05  W-TIAAMMDD              PIC 9(6)    VALUE ZERO.                  
005410     05  W-TIAAMMDD-X REDEFINES W-TIAAMMDD                                
005420                                 PIC X(6).                                
005500     05  W-TIHHMM                PIC 9(6).                                
005600     05  W-TIHHMM-X REDEFINES W-TIHHMM                                    
005700                                 PIC X(6).                                
005900                                                                          
006000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006100 01  GENERELLA-SUBPROGRAM.                                                
006200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006900*01 -COPY WMSGINIT                                                        
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007800     03  ERR-KUND-SAKNAS         PIC X(3)    VALUE '040'.                 
007900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     EJECT                                                                
008700     SKIP3                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W4I48301                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W4O48301                                                 
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200     SKIP3                                                                
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011210     SKIP2                                                                
011220     03  W-IDGMT-X.                                                       
011230         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
011240         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
011600     SKIP2                                                                
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700 01  SSA2                        PIC X(64).                               
012800     EJECT                                                                
012900*    --- IMS FUNKTIONSKODER                                               
013000*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013400     SKIP3                                                                
013500 01  DLI-IO-AREA.                                                         
013800     03  WLGMTA01.                                                        
013900*        05  -COPY WDB201                                                 
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200                                                                          
014300*01  -COPY W0009   -PRE MSG-                                              
014400*01  -COPY W0008   -PRE USEA-                                             
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700*01  -COPY W0008  -PRE GMTA-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTA-PCB.                     
015100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTA-PCB.                     
015200                                                                          
015300     PERFORM IMS-GET-MSG                                                  
015400     IF SEGMENT-FINNS                                                     
015500       PERFORM A-INIT                                                     
015600       PERFORM B-KOLLA-NYCKLAR                                            
015700       IF NYCKLAR-OK                                                      
015800         IF MFS-UPDATE                                                    
015900           PERFORM G-KOLLA-INPUT                                          
016000           IF INDATA-OK                                                   
016100             PERFORM H-UPPDATERA                                          
016200           END-IF                                                         
016300         ELSE                                                             
016400           IF MFS-FIRST                                                   
016500             PERFORM C-FOERSTA-SIDA                                       
016600           ELSE                                                           
016700             PERFORM E-SAMMA-SIDA                                         
016800           END-IF                                                         
016900         END-IF                                                           
017000         PERFORM F-LAES-VISA-INFO                                         
017100       END-IF                                                             
017200       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O48301 + 4                      
017300       PERFORM IMS-INSERT-MSG                                             
017400     END-IF                                                               
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100                                                                          
018200     IF MSG-DUBBLA-TRANSKODER                                             
018300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I48301                 
018400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
018500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
018600     ELSE                                                                 
018700       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I48301                 
018800       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
018900       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
019000     END-IF                                                               
019100                                                                          
019200     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
019300     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
019400     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
019500                                                                          
019600     MOVE LOW-VALUE       TO MSG-AREA                                     
019700     MOVE 'W4O483N1'      TO MFS-IDMOD                                    
019800     MOVE '4483'          TO MOD-IDTRANS                                  
019900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
020000                                                                          
020100                                                                          
020200     IF EGEN-MID OR HELP-MID                                              
020300       CONTINUE                                                           
020400     ELSE                                                                 
020500       MOVE SPACE TO MFS-KDTRTYP                                          
020600       MOVE '7' TO MFS-IDPFK                                              
020700     END-IF                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 B-KOLLA-NYCKLAR SECTION.                                                 
021100                                                                          
021200     MOVE ALL '+'                TO MSGI-WMSGINIT                         
021300     MOVE '001'                  TO MSGI-KDCALL                           
021400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
021410     MOVE '4483'                 TO MSGI-IDTRANS                          
021420     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
021500     IF GODK-MID                                                          
021600         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
021700         MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                         
021800     END-IF                                                               
021900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021910     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
022000                                                                          
022100     MOVE JA                     TO NYCKLAR-SW                            
022200                                                                          
022300     PERFORM BA-KOLLA-IDDISTR                                             
022400     PERFORM BB-KOLLA-IDKUNDNR                                            
022500                                                                          
022600     IF GODK-MID OR NYCKLAR-OK                                            
022700       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
022800       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
022810       INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
022820       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
022900     ELSE                                                                 
023000       MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                        
023100                                    MOD-IDKUNDNR-UT                       
023200     END-IF                                                               
023300                                                                          
023400     IF NYCKLAR-FEL                                                       
023500       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
023600       CALL WMEDKONV USING MED-WMEDAREA                                   
023700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
023800       PERFORM MFS-RENSA-FAELT-IN                                         
023900       PERFORM MFS-RENSA-FAELT-UT                                         
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 BA-KOLLA-IDDISTR SECTION.                                                
024400                                                                          
024500*    -- KONTROLL AV IDDISTR                                               
024600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
024700                                                                          
024800     IF MID-IDDISTR-IN    NOT = ALL '+'                                   
024900       MOVE '7'           TO MFS-IDPFK                                    
025000       MOVE SPACE         TO MFS-KDTRTYP                                  
025100     END-IF                                                               
025200                                                                          
025300     IF MSGI-IDDISTR      NUMERIC                                         
025400        MOVE MSGI-IDDISTR TO W-IDDISTR                                    
025500     ELSE                                                                 
025600        MOVE NEJ          TO NYCKLAR-SW                                   
025700     END-IF                                                               
025800                                                                          
025900     .                                                                    
026000     EJECT                                                                
026100 BB-KOLLA-IDKUNDNR SECTION.                                               
026200                                                                          
026300*    -- KONTROLL AV IDKUDNR                                               
026400     MOVE MFS-RENSA-FAELT  TO MOD-IDKUNDNR-IN                             
026500                                                                          
026600     IF MID-IDKUNDNR-IN    NOT = ALL '+'                                  
026700       MOVE '7'            TO MFS-IDPFK                                   
026800       MOVE SPACE          TO MFS-KDTRTYP                                 
026900     END-IF                                                               
027000                                                                          
027100     IF MSGI-IDKUNDNR      NUMERIC                                        
027200        MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                  
027300     ELSE                                                                 
027400        MOVE NEJ           TO NYCKLAR-SW                                  
027500     END-IF                                                               
027600                                                                          
027700     .                                                                    
027800     EJECT                                                                
027900 C-FOERSTA-SIDA SECTION.                                                  
028000                                                                          
028100     PERFORM MFS-RENSA-FAELT-IN                                           
028200     .                                                                    
028300     EJECT                                                                
028400 E-SAMMA-SIDA SECTION.                                                    
028500                                                                          
028600     IF EGEN-MID OR HELP-MID                                              
028700       IF MID-FLAGGA-AENDRA       = ALL '+'                               
028800         PERFORM MFS-RENSA-FAELT-IN                                       
028900       ELSE                                                               
029000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
029100         CALL WMEDKONV USING MED-WMEDAREA                                 
029200         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
029300         PERFORM EA-MID-INDATA-TILL-MOD                                   
029400       END-IF                                                             
029500     ELSE                                                                 
029600       PERFORM MFS-RENSA-FAELT-IN                                         
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 EA-MID-INDATA-TILL-MOD SECTION.                                          
030100                                                                          
030200     IF MID-FLAGGA-AENDRA          NOT = ALL '+'                          
030300        MOVE MID-FLAGGA-AENDRA     TO MOD-FLAGGA-AENDRA                   
030400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-AENDRA-ATTR              
030500     ELSE                                                                 
030600        MOVE MFS-RENSA-FAELT       TO MOD-FLAGGA-AENDRA                   
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 F-LAES-VISA-INFO SECTION.                                                
031100                                                                          
031200     PERFORM IMS-GHU-WDB201                                               
031300                                                                          
031400     IF SEGMENT-SAKNAS                                                    
031500        MOVE ERR-KUND-SAKNAS    TO MED-IDMFSFEL                           
031600        CALL WMEDKONV USING MED-WMEDAREA                                  
031700        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
031800        PERFORM MFS-RENSA-FAELT-UT                                        
031900     ELSE                                                                 
032000        PERFORM FA-REDIGERA-MOD                                           
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 FA-REDIGERA-MOD   SECTION.                                               
032500                                                                          
032600     IF GMT-FLCOD            = JA                                         
032700        PERFORM FAA-KUND-AER-COD                                          
032800     ELSE                                                                 
033000        PERFORM FAB-KUND-AER-EJ-COD                                       
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 FAA-KUND-AER-COD   SECTION.                                              
033600                                                                          
033700     MOVE 'YES'                 TO MOD-FLCOD1                             
033800     MOVE GMT-TISTADAT-COD      TO W-TIAAMMDD                             
033900     IF W-TIAAMMDD (1:2)        <  50                                     
034000        MOVE '20'               TO MOD-TIAAAAMMDDSTA1 (1:2)               
034100     ELSE                                                                 
034200        MOVE '19'               TO MOD-TIAAAAMMDDSTA1 (1:2)               
034300     END-IF                                                               
034310     MOVE GMT-TISTATID-COD      TO W-TIHHMM                               
034320     MOVE W-TIAAMMDD-X          TO MSGI-TILOKDAT                          
034330     MOVE W-TIHHMM-X(1:4)       TO MSGI-TILOKTID                          
034340     MOVE '011'                 TO MSGI-KDCALL                            
034350     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
034360                                   MSGI-IDLTERM-USER                      
034370     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034380     MOVE MSGI-TILOKDAT         TO W-TIAAMMDD-X                           
034390     MOVE MSGI-TILOKTID         TO W-TIHHMM-X(1:4)                        
034391                                                                          
034400     MOVE W-TIAAMMDD            TO MOD-TIAAAAMMDDSTA1 (3:6)               
034600     MOVE W-TIHHMM(1:2)         TO MOD-TIHHMMSTA1 (1:2)                   
034700     MOVE ':'                   TO MOD-TIHHMMSTA1 (3:1)                   
034800     MOVE W-TIHHMM(3:2)         TO MOD-TIHHMMSTA1 (4:2)                   
034900     MOVE MFS-RENSA-FAELT       TO MOD-FLCOD2                             
035000                                   MOD-TIAAAAMMDDSTA2                     
035100                                   MOD-TIAAAAMMDDSTO                      
035200                                   MOD-TIHHMMSTA2                         
035300                                   MOD-TIHHMMSTO                          
035400     .                                                                    
035500     EJECT                                                                
035600 FAB-KUND-AER-EJ-COD   SECTION.                                           
035700                                                                          
035800     MOVE 'NO '                 TO MOD-FLCOD1                             
035900     MOVE MFS-RENSA-FAELT       TO MOD-TIAAAAMMDDSTA1                     
036000                                   MOD-TIHHMMSTA1                         
036100     MOVE GMT-TISTADAT-COD      TO W-TIAAMMDD                             
036200     IF W-TIAAMMDD              > ZERO                                    
036300        MOVE 'YES'              TO MOD-FLCOD2                             
036400        IF W-TIAAMMDD (1:2)     <  95                                     
036500           MOVE '20'            TO MOD-TIAAAAMMDDSTA2 (1:2)               
036600        ELSE                                                              
036700           MOVE '19'            TO MOD-TIAAAAMMDDSTA2 (1:2)               
036800        END-IF                                                            
036801        MOVE GMT-TISTATID-COD   TO W-TIHHMM                               
036810        MOVE W-TIAAMMDD-X       TO MSGI-TILOKDAT                          
036820        MOVE W-TIHHMM-X(1:4)    TO MSGI-TILOKTID                          
036830        MOVE '011'              TO MSGI-KDCALL                            
036840        MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                            
036850                                   MSGI-IDLTERM-USER                      
036860        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
036870        MOVE MSGI-TILOKDAT      TO W-TIAAMMDD-X                           
036880        MOVE MSGI-TILOKTID      TO W-TIHHMM-X(1:4)                        
036900        MOVE W-TIAAMMDD         TO MOD-TIAAAAMMDDSTA2 (3:6)               
037100        MOVE W-TIHHMM(1:2)      TO MOD-TIHHMMSTA2 (1:2)                   
037200        MOVE ':'                TO MOD-TIHHMMSTA2 (3:1)                   
037300        MOVE W-TIHHMM(3:2)      TO MOD-TIHHMMSTA2 (4:2)                   
037400        MOVE GMT-TISTODAT-COD   TO W-TIAAMMDD                             
037500        IF W-TIAAMMDD (1:2)     <  95                                     
037600           MOVE '20'            TO MOD-TIAAAAMMDDSTO  (1:2)               
037700        ELSE                                                              
037800           MOVE '19'            TO MOD-TIAAAAMMDDSTO  (1:2)               
037900        END-IF                                                            
037910        MOVE GMT-TISTOTID-COD   TO W-TIHHMM                               
037920        MOVE W-TIAAMMDD-X       TO MSGI-TILOKDAT                          
037930        MOVE W-TIHHMM-X(1:4)    TO MSGI-TILOKTID                          
037940        MOVE '011'              TO MSGI-KDCALL                            
037950        MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                            
037960                                   MSGI-IDLTERM-USER                      
037970        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
037980        MOVE MSGI-TILOKDAT      TO W-TIAAMMDD-X                           
037990        MOVE MSGI-TILOKTID      TO W-TIHHMM-X(1:4)                        
038000        MOVE W-TIAAMMDD         TO MOD-TIAAAAMMDDSTO  (3:6)               
038200        MOVE W-TIHHMM(1:2)      TO MOD-TIHHMMSTO  (1:2)                   
038300        MOVE ':'                TO MOD-TIHHMMSTO  (3:1)                   
038400        MOVE W-TIHHMM(3:2)      TO MOD-TIHHMMSTO  (4:2)                   
038500     ELSE                                                                 
038600        MOVE 'NO '              TO MOD-FLCOD2                             
038700        MOVE MFS-RENSA-FAELT    TO MOD-TIAAAAMMDDSTA2                     
038800                                   MOD-TIAAAAMMDDSTO                      
038900                                   MOD-TIHHMMSTA2                         
039000                                   MOD-TIHHMMSTO                          
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 G-KOLLA-INPUT SECTION.                                                   
039500                                                                          
039600     MOVE JA                     TO INDATA-SW                             
039700     IF MID-FLAGGA-AENDRA        =  ALL '+'                               
039800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
039900       CALL WMEDKONV USING MED-WMEDAREA                                   
040000       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
040100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
040200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
040300       MOVE NEJ                  TO INDATA-SW                             
040400     ELSE                                                                 
040500       IF MID-FLAGGA-AENDRA        = JA OR YES                            
040600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-AENDRA-ATTR              
040700       ELSE                                                               
040800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAGGA-AENDRA-ATTR              
040900         MOVE NEJ                  TO INDATA-SW                           
041000       END-IF                                                             
041100                                                                          
041101       MOVE W-IDDISTR              TO TEST-IDDISTR                        
041110       IF DIST51-COD                                                      
041120          CONTINUE                                                        
041130       ELSE                                                               
041140         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAGGA-AENDRA-ATTR              
041150         MOVE NEJ                  TO INDATA-SW                           
041160       END-IF                                                             
041170                                                                          
041200       IF INDATA-FEL                                                      
041300         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
041400         CALL WMEDKONV USING MED-WMEDAREA                                 
041500         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
041600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
041700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
041800       END-IF                                                             
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 H-UPPDATERA SECTION.                                                     
042300                                                                          
042310     ACCEPT W-DATUM          FROM DATE                                    
042320     ACCEPT W-TID            FROM TIME                                    
042330                                                                          
042400     PERFORM IMS-GHU-WDB201                                               
042500     IF SEGMENT-FINNS                                                     
042600       IF GMT-FLCOD       = NEJ OR SPACE                                  
042700          PERFORM HA-FLAGGA-PAA-KUND                                      
042800       ELSE                                                               
042900          IF GMT-FLCOD    = JA                                            
043000             PERFORM HB-FLAGGA-AV-KUND                                    
043100          END-IF                                                          
043200       END-IF                                                             
043300       PERFORM IMS-REPL-WDB201                                            
043400                                                                          
043500       MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                              
043600       CALL WMEDKONV USING MED-WMEDAREA                                   
043700       MOVE MED-MFSINF       TO MOD-TEMFSINF                              
043800       PERFORM MFS-FORM-ATTR                                              
043900       PERFORM MFS-RENSA-FAELT-IN                                         
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300                                                                          
044310 HA-FLAGGA-PAA-KUND SECTION.                                              
044320                                                                          
044330     MOVE JA                 TO GMT-FLCOD                                 
044340     MOVE W-DATUM            TO GMT-TISTADAT-COD                          
044350     MOVE W-TID(1:6)         TO GMT-TISTATID-COD                          
044360                                                                          
044370     .                                                                    
044380     EJECT                                                                
044400 HB-FLAGGA-AV-KUND SECTION.                                               
044500                                                                          
044510     MOVE NEJ                TO GMT-FLCOD                                 
044511     MOVE W-DATUM            TO GMT-TISTODAT-COD                          
044512     MOVE W-TID(1:6)         TO GMT-TISTOTID-COD                          
044520                                                                          
044600     .                                                                    
044700     EJECT                                                                
045200 MFS-RENSA-FAELT-UT SECTION.                                              
045300                                                                          
045500     MOVE MFS-RENSA-FAELT       TO MOD-FLCOD1                             
045510                                   MOD-TIAAAAMMDDSTA1                     
045520                                   MOD-TIHHMMSTA1                         
045600                                   MOD-FLCOD2                             
045610                                   MOD-TIAAAAMMDDSTA2                     
045620                                   MOD-TIAAAAMMDDSTO                      
045630                                   MOD-TIHHMMSTA2                         
045640                                   MOD-TIHHMMSTO                          
045700     .                                                                    
045800     SKIP3                                                                
045900 MFS-RENSA-FAELT-IN SECTION.                                              
046000                                                                          
046200     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-AENDRA                            
046400     .                                                                    
046500     EJECT                                                                
046600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
046700                                                                          
046910     MOVE MFS-ROER-EJ-FAELT     TO MOD-FLCOD1                             
046920                                   MOD-TIAAAAMMDDSTA1                     
046930                                   MOD-TIHHMMSTA1                         
046940                                   MOD-FLCOD2                             
046950                                   MOD-TIAAAAMMDDSTA2                     
046960                                   MOD-TIAAAAMMDDSTO                      
046970                                   MOD-TIHHMMSTA2                         
046980                                   MOD-TIHHMMSTO                          
047100     .                                                                    
047200     SKIP3                                                                
047300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
047400                                                                          
047600     MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-AENDRA                          
047800     .                                                                    
047900     EJECT                                                                
048000 MFS-FORM-ATTR SECTION.                                                   
048100                                                                          
048300     MOVE MFS-FORMATETS-ATTR TO MOD-FLAGGA-AENDRA-ATTR                    
048500     .                                                                    
048600     EJECT                                                                
049400* --- IMS SEKTIONER ---                                                   
049500     SKIP3                                                                
049600 IMS-GET-MSG SECTION.                                                     
049700                                                                          
049800     MOVE '  QC' TO GODK-STATUSKODER                                      
049900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
050000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050300     SKIP3                                                                
050400 IMS-INSERT-MSG SECTION.                                                  
050500                                                                          
050600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
050700       MOVE 'N' TO MFS-KDHUVOMR                                           
050800     END-IF                                                               
050900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
051000     MOVE SPACE TO GODK-STATUSKODER                                       
051100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
051200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051300     PERFORM IMS-STATUSKONTROLL                                           
051400     .                                                                    
051500     EJECT                                                                
052510 IMS-GHU-WDB201 SECTION.                                                  
052520                                                                          
052530     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
052540          DELIMITED BY SIZE INTO SSA1                                     
052550     MOVE '  GE' TO GODK-STATUSKODER                                      
052560     CALL CBLTDLI USING GHU GMTA-PCB DLI-IO-AREA SSA1                     
052570     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
052580     PERFORM IMS-STATUSKONTROLL                                           
052590     .                                                                    
052591     SKIP3                                                                
052600 IMS-REPL-WDB201 SECTION.                                                 
052700                                                                          
052800     MOVE '  ' TO GODK-STATUSKODER                                        
052900     CALL CBLTDLI USING REPL GMTA-PCB DLI-IO-AREA                         
053000     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     EJECT                                                                
053400 IMS-STATUSKONTROLL SECTION.                                              
053500                                                                          
053600     SET STATUS-IX TO 1                                                   
053700     SEARCH GODK-STATUS                                                   
053800       AT END                                                             
053900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
054000         DELIMITED BY SIZE INTO FELTEXT                                   
054100         CALL FELLOG                                                      
054200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054300         CONTINUE                                                         
054400     END-SEARCH                                                           
054500     .                                                                    
054510     EJECT                                                                
