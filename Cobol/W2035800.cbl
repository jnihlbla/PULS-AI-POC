000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2035800.                                                
000400 AUTHOR.         KENT JEBSEN.                                             
000500 DATE-WRITTEN.   96/12/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR ORDERKVANT OCH BERÄKNAT ANKOMSTDATUM                  
001000*        PÅ USA LOKALA ARTIKLAR                                           
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
001300*                              WLARTS (WDK7)                              
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T358                                              
001800*        MID:         W2I35801                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O35801                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900                                                                          
003000*    -COPY WY2000W1                                                       
003100     SKIP3                                                                
003200 77  IDPGM                       PIC X(08)   VALUE 'W2035800'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  WS-TIBERANK                 PIC S9(6)   COMP-3.                      
004000 77  WS-KVBEART                  PIC S9(7)   COMP-3.                      
004100 77  WS-KVBEART-IN               PIC S9(7)   COMP-3.                      
004200 77  WS-INMATAT                  PIC X.                                   
004300 77  WS-RAD-STAENG               PIC 9(3).                                
004400 01  WS-EJ-TRAEFF.                                                        
004500   03 WS-EJTR-TIREGDAT-X         PIC 9(7)    VALUE ZERO.                  
004600   03 WS-EJTR-TIREGTID-X         PIC 9(7)    VALUE ZERO.                  
004700 77  DAGENS-DATUM                PIC 9(6).                                
004800 77  DATUM-PASSERAT              PIC X(40)                                
004900     VALUE 'DATE HAS ALREADY EXPIRED-UPD NOT ALLOWED'.                    
005000                                                                          
005100 01  WS-MSGI-SPAR-AREA.                                                   
005200     03  WS-MSGI-PGM             PIC X(6)   VALUE SPACE.                  
005300     03  WS-MSGI-RADNR-ENTER     PIC 9(3)   VALUE ZERO.                   
005400     03  WS-MSGI-IDARTNR         PIC X(9)   VALUE SPACE.                  
005500     03  WS-MSGI-RADNR-NEXT      PIC 9(3)   VALUE ZERO.                   
005600     03  FILLER                  PIC X(179) VALUE SPACE.                  
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
006900     88  EGEN-MID                            VALUE '2358'.                
007000     88  GODK-MID                            VALUE '2352'                 
007100                                                   '2353' '2354'          
007200                                                   '2355' '2356'          
007300                                                   '2357' '2358'          
007400                                                   '2359'.                
007500     88  HELP-MID                            VALUE '0551'.                
007600     EJECT                                                                
007700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007800 01  GENERELLA-SUBPROGRAM.                                                
007900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008600*01 -COPY WMEDAREA                                                        
008700     SKIP3                                                                
008800 01  MESSAGE-CODES.                                                       
008900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009200     03  PARTNO-MISSING          PIC X(3)    VALUE '017'.                 
009300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009700     03  UPD-NOT-ALLOWED         PIC X(3)    VALUE '777'.                 
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010200     SKIP3                                                                
010300*01 -COPY WMSGINIT                                                        
010310 01  FILLER                      PIC X(16)   VALUE 'DC CODES'.            
010320     SKIP3                                                                
010330*01 -COPY WWDC99                                                          
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
010600 01  TABENTRY-PARM.                                                       
010700     03  STEGLANGD               PIC S9(9) COMP   VALUE 67.               
010800     03  ANTAL                   PIC S9(9) COMP.                          
010900     03  NYCKELLANGD             PIC S9(9) COMP   VALUE 10.               
011000                                                                          
011100 01  IX-RAD                      PIC S9(3) COMP-3 VALUE 1.                
011200 01  MAX-IX-RAD                  PIC S9(3) COMP-3 VALUE +13.              
011300 01  IX-RAD-TAB                  PIC S9(3) COMP-3 VALUE 1.                
011400                                                                          
011500 01  TAB-MAX                     PIC S9(9) COMP   VALUE 100.              
011600     EJECT                                                                
011700*    --- TABELL SOM SORTERAS AV WINTSOR                                   
011800 01  TABELL.                                                              
011900     03  TAB-POST  OCCURS 130.                                            
012000       04  TAB-RAD.                                                       
012100         05  TAB-IDDC             PIC X(2).                               
012200         05  TAB-IDLEVNR          PIC X(5).                               
012300         05  TAB-TIREGDAT         PIC 9(6).                               
012400         05  TAB-IDKUNDRF         PIC X(10).                              
012500         05  TAB-KVBEART-UT       PIC Z(6).                               
012600         05  TAB-KVBEART-IN-ATTR  PIC X(2).                               
012700         05  TAB-KVBEART-IN       PIC Z(6).                               
012800         05  TAB-TIBERANK-UT      PIC 9(6).                               
012900         05  TAB-TIBERANK-IN-ATTR PIC X(2).                               
013000         05  TAB-TIBERANK-IN      PIC 9(6).                               
013100         05  TAB-TIREGTID         PIC 9(6).                               
013200       04  TAB-SORT.                                                      
013300         05  TAB-IDDC-SORT        PIC X(2).                               
013400         05  TAB-TISEKEL-SORT     PIC Z(2).                               
013500         05  TAB-TIBERANK-SORT    PIC 9(6).                               
013600     EJECT                                                                
013700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014000     SKIP3                                                                
014100*01  MID -COPY W2I35801                                                   
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014400     SKIP3                                                                
014500*01  -COPY WMSGAREA                                                       
014600     EJECT                                                                
014700     03  MOD REDEFINES MSG-AREA.                                          
014800*      05  -COPY W2O35801                                                 
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015100     SKIP3                                                                
015200*01  -COPY WMFSAREA                                                       
015300     EJECT                                                                
015400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015500*                                                                         
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015800     SKIP3                                                                
015900 01  NYCKLAR-TILL-DLI.                                                    
016000*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
016100     03  W-IDARTNR-X.                                                     
016200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016300     03  W-WDL612KY-X.                                                    
016400         05  W-DAREGDAT-X        PIC 9(8)    VALUE ZERO.                  
016500         05  W-TIREGTID-X        PIC S9(7)   VALUE ZERO COMP-3.           
016600     03  W-IDDC-X.                                                        
016700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016800     03  W-IDSKYLT-X.                                                     
016900         05  W-IDSKYLT           PIC X(3)    VALUE 'USA'.                 
017000     03  W-IDDC-B6-X.                                                     
017100         05 W-IDDC-B6                  PIC X(2).                          
017200     SKIP2                                                                
017300*    --- STATUS-KOD FRÅN IMS                                              
017400 01  STATUS-WS                   PIC XX.                                  
017500     88  SEGMENT-FINNS                       VALUE '  '.                  
017600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017800     SKIP2                                                                
017900 01  GODK-STATUSKODER.                                                    
018000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100     SKIP3                                                                
018200 01  SSA1                        PIC X(64).                               
018300 01  SSA2                        PIC X(64).                               
018400     EJECT                                                                
018500*    --- IMS FUNKTIONSKODER                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800*    ---  DLI INPUT-OUTPUT AREA                                           
018900                                                                          
019000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC01'.                    
019100 01  DLI-IO-WLINLC01.                                                     
019200*    03  -COPY WDL601  -PRE INLC-                                         
019300     EJECT                                                                
019400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC12'.                    
019500 01  DLI-IO-WLINLC12.                                                     
019600*    03  -COPY WDL612  -PRE INLC-                                         
019700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS01'.                    
019800 01  DLI-IO-WLARTS01.                                                     
019900*    03  -COPY WDK701  -PRE ARTS-                                         
020000     EJECT                                                                
020100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
020200 01  DLI-IO-WLARTS11.                                                     
020300*    03  -COPY WDK711  -PRE ARTS-                                         
020400     EJECT                                                                
020500 01  DLI-IO-AREA2.                                                        
020600     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
020700     EJECT                                                                
020800     03  WLBENA11 REDEFINES IO-AREA2.                                     
020900*        05  -COPY WDD311  -PRE BENA-                                     
021000                                                                          
021100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021200 01   DLI-IO-AREA-B601.                                                   
021300*     03  -COPY WDB601                                                    
021400                                                                          
021500     EJECT                                                                
021600 LINKAGE SECTION.                                                         
021700*01  -COPY W0009   -PRE MSG-                                              
021800*01  -COPY W0008   -PRE USEA-                                             
021900     05  FILLER                  PIC X.                                   
022000     EJECT                                                                
022100*01  -COPY W0008  -PRE INLC-                                              
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008  -PRE ARTS-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE BENA-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000*01  -COPY W0008  -PRE WDB6-                                              
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB INLC-PCB ARTS-PCB             
023400                                            BENA-PCB WDB6-PCB.            
023500 MAIN SECTION.                                                            
023600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB INLC-PCB ARTS-PCB             
023700                                            BENA-PCB WDB6-PCB.            
023800                                                                          
023900     PERFORM IMS-GET-MSG                                                  
024000     IF SEGMENT-FINNS                                                     
024100       PERFORM A-INIT                                                     
024200       PERFORM B-KOLLA-NYCKLAR                                            
024300       IF NYCKLAR-OK                                                      
024400         IF MFS-UPDATE                                                    
024500           PERFORM G-KOLLA-INPUT                                          
024600           IF INDATA-OK                                                   
024700             PERFORM H-UPPDATERA                                          
024800           END-IF                                                         
024900         ELSE                                                             
025000           PERFORM C-LAES-RADINFO                                         
025100           PERFORM D-SORTERA-RAD                                          
025200           PERFORM F-VISA-INFO                                            
025300           IF MFS-ENTER                                                   
025400             PERFORM E-SPARA-INMAT                                        
025500           END-IF                                                         
025600         END-IF                                                           
025700       END-IF                                                             
025800       PERFORM I-STAENG-FAELT                                             
025900       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35801 + 4                      
026000       PERFORM IMS-INSERT-MSG                                             
026100     END-IF                                                               
026200                                                                          
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 A-INIT SECTION.                                                          
026800                                                                          
026900     IF MSG-DUBBLA-TRANSKODER                                             
027000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35801                 
027100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027300     ELSE                                                                 
027400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35801                  
027500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027700     END-IF                                                               
027800                                                                          
027900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028200                                                                          
028300     MOVE LOW-VALUE TO MSG-AREA                                           
028400     MOVE 'W2O358N1' TO MFS-IDMOD                                         
028500     MOVE '2358' TO  MOD-IDTRANS                                          
028600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028700                                                                          
028800     IF EGEN-MID OR HELP-MID                                              
028900       CONTINUE                                                           
029000     ELSE                                                                 
029100       MOVE SPACE TO MFS-KDTRTYP                                          
029200       MOVE '7' TO MFS-IDPFK                                              
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 B-KOLLA-NYCKLAR SECTION.                                                 
029700                                                                          
029800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029900     MOVE '001'             TO MSGI-KDCALL                                
030000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030200     MOVE '2358'            TO MSGI-IDTRANS                               
030300     IF EGEN-MID                                                          
030400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
030500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
030600     ELSE                                                                 
030700       IF  MID-IDARTNR-IN NUMERIC                                         
030800       AND MID-IDARTNR-IN > ZERO                                          
030900         MOVE MID-IDARTNR-IN                                              
031000                            TO MSGI-IDARTNR                               
031100       END-IF                                                             
031200     END-IF                                                               
031300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031400                                                                          
031500     MOVE MSGI-SPAR-AREA    TO WS-MSGI-SPAR-AREA                          
031600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
031700                                                                          
031800     MOVE JA TO NYCKLAR-SW                                                
031900                                                                          
032000*    -- KONTROLL AV IDARTNR OCH IDDC                                      
032100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
032200                                                                          
032300     IF MID-IDARTNR-IN  NOT = ALL '+'                                     
032400       MOVE '7'         TO MFS-IDPFK                                      
032500       MOVE SPACE       TO MFS-KDTRTYP                                    
032600     END-IF                                                               
032700     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
032800     IF MSGI-IDARTNR NUMERIC                                              
032900       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
033000                            WS-MSGI-IDARTNR                               
033100     ELSE                                                                 
033200       MOVE NEJ TO NYCKLAR-SW                                             
033300     END-IF                                                               
033400                                                                          
033500*    -- KONTROLL AV IDDC                                                  
033600     MOVE MFS-RENSA-FAELT             TO MOD-IDDC-IN                      
033700                                                                          
033800     IF MID-IDDC-IN NOT = ALL '+'                                         
033900       MOVE '7'              TO MFS-IDPFK                                 
034000       MOVE SPACE            TO MFS-KDTRTYP                               
034100     END-IF                                                               
034200                                                                          
034300     MOVE MSGI-IDDC-KEY     TO W-IDDC-B6                                  
034400                               W-IDDC                                     
034410                               WS-IDDC                                    
034500     PERFORM IMS-GU-WDB601                                                
034600     IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC                            
034700       MOVE NEJ             TO NYCKLAR-SW                                 
034800     END-IF                                                               
034801                                                                          
034802     IF NDC-CN OR NDC-US                                                  
034810       PERFORM IMS-GET-ARTS-SLAG                                          
034820       IF ARTS-SLAG-IDDC-REF = SPACES                                     
034830          MOVE NEJ          TO NYCKLAR-SW                                 
034840       END-IF                                                             
034850     END-IF                                                               
034900                                                                          
035000     MOVE WS-MSGI-IDARTNR  TO MOD-IDARTNR-UT                              
035100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
035200     MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                                 
035300                                                                          
035400     IF NYCKLAR-FEL                                                       
035500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
035600       CALL WMEDKONV USING MED-WMEDAREA                                   
035700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
035800       PERFORM MFS-RENSA-FAELT-IN                                         
035900       PERFORM MFS-RENSA-FAELT-UT                                         
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 C-LAES-RADINFO SECTION.                                                  
036400                                                                          
036500     PERFORM CA-LAES-GRUNDDATA                                            
036600                                                                          
036700     IF SEGMENT-SAKNAS                                                    
036800       MOVE PARTNO-MISSING TO MED-IDMFSFEL                                
036900       CALL WMEDKONV USING MED-WMEDAREA                                   
037000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
037100       PERFORM MFS-RENSA-FAELT-UT                                         
037200     ELSE                                                                 
037300                                                                          
037400       MOVE +1 TO IX-RAD                                                  
037500       PERFORM CB-LAES-RADDATA                                            
037600                                                                          
037700       PERFORM UNTIL SEGMENT-SAKNAS OR IX-RAD > 130                       
037800         IF DCS-IDDC = INLC-ORD-IDDC                                      
037900           IF INLC-ORD-IDLOPNRM = +0                                      
038000             MOVE INLC-ORD-IDDC      TO TAB-IDDC          (IX-RAD)        
038100                                        TAB-IDDC-SORT     (IX-RAD)        
038200             MOVE INLC-ORD-IDLEVNR   TO TAB-IDLEVNR       (IX-RAD)        
038300             MOVE INLC-ORD-DAREGDAT (3:6) TO TAB-TIREGDAT (IX-RAD)        
038400             MOVE INLC-ORD-IDKUNDRF  TO TAB-IDKUNDRF      (IX-RAD)        
038500             MOVE INLC-ORD-KVBEART   TO TAB-KVBEART-UT    (IX-RAD)        
038600             MOVE MFS-FORMATETS-ATTR TO                                   
038700                                      TAB-KVBEART-IN-ATTR (IX-RAD)        
038800             MOVE INLC-ORD-TIBERANK  TO                                   
038900                                      TAB-TIBERANK-UT     (IX-RAD)        
039000                                      TAB-TIBERANK-SORT   (IX-RAD)        
039100             MOVE MFS-FORMATETS-ATTR TO                                   
039200                                      TAB-TIBERANK-IN-ATTR(IX-RAD)        
039300                                                                          
039400             MOVE INLC-ORD-TIREGTID  TO TAB-TIREGTID      (IX-RAD)        
039500                                                                          
039600             IF INLC-ORD-TIBERANK < 900000                                
039700               MOVE  20              TO TAB-TISEKEL-SORT  (IX-RAD)        
039800             ELSE                                                         
039900               MOVE  19              TO TAB-TISEKEL-SORT  (IX-RAD)        
040000             END-IF                                                       
040100                                                                          
040200             ADD +1 TO IX-RAD                                             
040300           END-IF                                                         
040400         END-IF                                                           
040500         PERFORM CB-LAES-RADDATA                                          
040600       END-PERFORM                                                        
040700     END-IF                                                               
040800                                                                          
040900*** SÄTTER 999999 TILL DOLT FÄLT FÖR ATT KUNNA STÄNGA INMATNINGS-         
041000*** FÄLTEN PÅ TOMMA RADER. VÄRDE 99 TILL SORT-FÄLT FÖR ATT LÄGGA          
041100*** DESSA RADER SIST PÅ SKÄRMEN.                                          
041300     MOVE MAX-IX-RAD TO WS-RAD-STAENG                                     
041420     IF WS-MSGI-PGM = 'W20358'                                            
041500        IF MFS-NEXT                                                       
041600           COMPUTE WS-RAD-STAENG = WS-MSGI-RADNR-NEXT + 12                
041700        END-IF                                                            
041800        IF MFS-ENTER                                                      
041900           COMPUTE WS-RAD-STAENG = WS-MSGI-RADNR-ENTER + 12               
042000        END-IF                                                            
042010     END-IF                                                               
042100                                                                          
042200     PERFORM UNTIL IX-RAD > WS-RAD-STAENG                                 
042300                                                                          
042400       MOVE 999999               TO TAB-TIREGTID        (IX-RAD)          
042500       MOVE '99'                 TO TAB-IDDC-SORT       (IX-RAD)          
042600       MOVE  99                  TO TAB-TISEKEL-SORT    (IX-RAD)          
042700       ADD +1                    TO IX-RAD                                
042800     END-PERFORM                                                          
042900     .                                                                    
043000     EJECT                                                                
043100 CA-LAES-GRUNDDATA SECTION.                                               
043200                                                                          
043300     PERFORM IMS-GET-INLC-ART                                             
043400     .                                                                    
043500     EJECT                                                                
043600 CB-LAES-RADDATA SECTION.                                                 
043700                                                                          
043800     PERFORM IMS-GET-INLC-ORD                                             
043900     .                                                                    
044000     EJECT                                                                
044100 D-SORTERA-RAD SECTION.                                                   
044200                                                                          
044300     SUBTRACT 1   FROM IX-RAD                                             
044400     MOVE IX-RAD  TO ANTAL                                                
044500                                                                          
044600     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
044700                  TAB-SORT (1) NYCKELLANGD                                
044800     .                                                                    
044900     EJECT                                                                
045000 E-SPARA-INMAT SECTION.                                                   
045100                                                                          
045200     IF EGEN-MID OR HELP-MID                                              
045300       MOVE INF-PRESS-PF11        TO MED-IDMFSINF                         
045400       CALL WMEDKONV              USING MED-WMEDAREA                      
045500       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
045600       PERFORM EA-MID-INDATA-TILL-MOD                                     
045700     ELSE                                                                 
045800       PERFORM MFS-RENSA-FAELT-IN                                         
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 EA-MID-INDATA-TILL-MOD SECTION.                                          
046300                                                                          
046400     MOVE +1 TO IX-RAD                                                    
046500     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
046600       IF MID-KVBEART-IN(IX-RAD)     NOT = ALL '+'                        
046700         MOVE MID-KVBEART-IN(IX-RAD) TO MOD-KVBEART-IN(IX-RAD)            
046800         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVBEART-IN-ATTR(IX-RAD)        
046900       ELSE                                                               
047000         MOVE MFS-RENSA-FAELT        TO MOD-KVBEART-IN(IX-RAD)            
047100       END-IF                                                             
047200       IF MID-TIBERANK-IN(IX-RAD)     NOT = ALL '+'                       
047300         MOVE MID-TIBERANK-IN(IX-RAD) TO MOD-TIBERANK-IN(IX-RAD)          
047400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIBERANK-IN-ATTR(IX-RAD)        
047500        ELSE                                                              
047600          MOVE MFS-RENSA-FAELT        TO MOD-TIBERANK-IN(IX-RAD)          
047700        END-IF                                                            
047800        ADD 1 TO IX-RAD                                                   
047900      END-PERFORM                                                         
048000     .                                                                    
048100     EJECT                                                                
048200 F-VISA-INFO SECTION.                                                     
048300                                                                          
048400     PERFORM IMS-GU-WLBENA11                                              
048500                                                                          
048600     IF SEGMENT-FINNS                                                     
048700       MOVE BENA-TEXT-BEART TO MOD-BEART                                  
048800     ELSE                                                                 
048900       MOVE SPACE           TO MOD-BEART                                  
049000     END-IF                                                               
049100                                                                          
049200***  HÄR LÄSES ETT STARTVÄRDE FÖR IX-RAD FRÅN USER-BASEN                  
049300***  SÅ ATT VID BLÄDDRING, START SKER MED RÄTT RAD                        
049400     IF MFS-NEXT AND WS-MSGI-PGM = 'W20358'                               
049500        MOVE WS-MSGI-RADNR-NEXT  TO IX-RAD-TAB                            
049600     END-IF                                                               
049700     IF MFS-FIRST                                                         
049800        MOVE +1                  TO IX-RAD-TAB                            
049900     END-IF                                                               
050000     IF MFS-ENTER                                                         
050100        MOVE WS-MSGI-RADNR-ENTER TO IX-RAD-TAB                            
050200     END-IF                                                               
050300                                                                          
050400     MOVE +1 TO IX-RAD                                                    
050500     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
050600        MOVE TAB-RAD (IX-RAD-TAB) TO MOD-INFO-RAD   (IX-RAD)              
050700        MOVE MFS-RENSA-FAELT      TO MOD-KVBEART-IN (IX-RAD)              
050800                                     MOD-TIBERANK-IN(IX-RAD)              
050900                                                                          
051000        ADD 1 TO IX-RAD IX-RAD-TAB                                        
051100     END-PERFORM                                                          
051200                                                                          
051300***  OM FLER RADER FINNS, SÅ SPARAS I USERBASEN NÄSTA RADNR               
051400***  FRÅN TABELLEN                                                        
051500     MOVE 'W20358'               TO WS-MSGI-PGM                           
051600     COMPUTE WS-MSGI-RADNR-ENTER = IX-RAD-TAB - MAX-IX-RAD                
051700     END-COMPUTE                                                          
051800                                                                          
051900     ADD 1 TO ANTAL                                                       
052000     IF IX-RAD-TAB       < ANTAL                                          
052100        MOVE IX-RAD-TAB            TO WS-MSGI-RADNR-NEXT                  
052200     ELSE                                                                 
052300        MOVE WS-MSGI-RADNR-ENTER   TO WS-MSGI-RADNR-NEXT                  
052400     END-IF                                                               
052500                                                                          
052600     MOVE '002'             TO MSGI-KDCALL                                
052700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
052800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
052900     MOVE '2358'            TO MSGI-IDTRANS                               
053000     MOVE WS-MSGI-SPAR-AREA TO MSGI-SPAR-AREA                             
053100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
053200     .                                                                    
053300     EJECT                                                                
053400 G-KOLLA-INPUT SECTION.                                                   
053500                                                                          
053600     MOVE JA  TO INDATA-SW                                                
053700     MOVE +1 TO IX-RAD                                                    
053800********  KOLLAR OM NGT ÄR INMATAT                                        
053900     MOVE 'N' TO WS-INMATAT                                               
054000     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
054100       IF MID-KVBEART-IN(IX-RAD) NOT = ALL '+' OR                         
054200          MID-TIBERANK-IN(IX-RAD) NOT = ALL '+'                           
054300         MOVE 'J' TO WS-INMATAT                                           
054400       END-IF                                                             
054500       ADD 1 TO IX-RAD                                                    
054600     END-PERFORM                                                          
054700                                                                          
054800     IF WS-INMATAT = 'N'                                                  
054900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
055000       CALL WMEDKONV USING MED-WMEDAREA                                   
055100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
055200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
055300       MOVE +1 TO IX-RAD                                                  
055400       PERFORM UNTIL IX-RAD > MAX-IX-RAD                                  
055500         MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                              
055600                                   MOD-IDDC        (IX-RAD)               
055700                                   MOD-IDLEVNR     (IX-RAD)               
055800                                   MOD-TIREGDAT    (IX-RAD)               
055900                                   MOD-IDKUNDRF    (IX-RAD)               
056000                                   MOD-KVBEART-UT  (IX-RAD)               
056100                                   MOD-TIBERANK-UT (IX-RAD)               
056200                                   MOD-TIREGTID    (IX-RAD)               
056300         ADD +1 TO IX-RAD                                                 
056400       END-PERFORM                                                        
056500       MOVE NEJ TO INDATA-SW                                              
056600     ELSE                                                                 
056700       PERFORM GA-KOLLA-NUM                                               
056800       IF INDATA-OK                                                       
056900         PERFORM GB-KOLLA-DATUM                                           
057000       END-IF                                                             
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400 GA-KOLLA-NUM SECTION.                                                    
057500                                                                          
057600     MOVE +1 TO IX-RAD                                                    
057700     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
057800       IF MID-KVBEART-IN(IX-RAD) NOT = ALL '+'                            
057900         INSPECT MID-KVBEART-IN(IX-RAD)                                   
058000                            REPLACING LEADING SPACE BY ZERO               
058100         IF MID-KVBEART-IN(IX-RAD) NOT NUMERIC                            
058200           MOVE MFS-NUM-FAELT-FEL TO                                      
058300                               MOD-KVBEART-IN-ATTR(IX-RAD)                
058400           MOVE NEJ TO INDATA-SW                                          
058500         ELSE                                                             
058600           MOVE MFS-NUM-FAELT-RAETT TO                                    
058700                               MOD-KVBEART-IN-ATTR(IX-RAD)                
058800         END-IF                                                           
058900       ELSE                                                               
059000         MOVE MFS-NUM-FAELT-RAETT TO                                      
059100                               MOD-KVBEART-IN-ATTR(IX-RAD)                
059200       END-IF                                                             
059300                                                                          
059400       IF MID-TIBERANK-IN(IX-RAD) NOT = ALL '+'                           
059500         INSPECT MID-TIBERANK-IN(IX-RAD)                                  
059600                            REPLACING LEADING SPACE BY ZERO               
059700         IF MID-TIBERANK-IN(IX-RAD) NOT NUMERIC                           
059800           MOVE MFS-NUM-FAELT-FEL TO                                      
059900                             MOD-TIBERANK-IN-ATTR(IX-RAD)                 
060000           MOVE NEJ TO INDATA-SW                                          
060100         ELSE                                                             
060200           MOVE MFS-NUM-FAELT-RAETT TO                                    
060300                             MOD-TIBERANK-IN-ATTR(IX-RAD)                 
060400         END-IF                                                           
060500       ELSE                                                               
060600         MOVE MFS-NUM-FAELT-RAETT TO                                      
060700                             MOD-TIBERANK-IN-ATTR(IX-RAD)                 
060800       END-IF                                                             
060900       ADD 1 TO IX-RAD                                                    
061000     END-PERFORM                                                          
061100                                                                          
061200     IF INDATA-FEL                                                        
061300       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
061400       CALL WMEDKONV USING MED-WMEDAREA                                   
061500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
061600       MOVE +1 TO IX-RAD                                                  
061700       PERFORM UNTIL IX-RAD > MAX-IX-RAD                                  
061800         MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                              
061900                                   MOD-IDDC        (IX-RAD)               
062000                                   MOD-IDLEVNR     (IX-RAD)               
062100                                   MOD-TIREGDAT    (IX-RAD)               
062200                                   MOD-IDKUNDRF    (IX-RAD)               
062300                                   MOD-KVBEART-UT  (IX-RAD)               
062400                                   MOD-TIBERANK-UT (IX-RAD)               
062500                                   MOD-TIREGTID    (IX-RAD)               
062600         ADD +1 TO IX-RAD                                                 
062700       END-PERFORM                                                        
062800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200 GB-KOLLA-DATUM SECTION.                                                  
063300                                                                          
063400     ACCEPT DAGENS-DATUM FROM DATE                                        
063500                                                                          
063600     MOVE +1 TO IX-RAD                                                    
063700     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
063800       IF MID-TIBERANK-IN(IX-RAD) NOT = ALL '+'                           
063900         MOVE MID-TIBERANK-IN(IX-RAD)     TO WS-TIBERANK                  
064000** DATUM KAN INTE BLI MINDRE ÄN 000101                                    
064100         IF WS-TIBERANK  > 000100                                         
064200           MOVE MID-TIBERANK-IN(IX-RAD)   TO TMP1-YYMMDD                  
064300           MOVE DAGENS-DATUM              TO TMP2-YYMMDD                  
064400           PERFORM WY2000P1                                               
064500           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
064600             MOVE MFS-NUM-FAELT-FEL TO                                    
064700                             MOD-TIBERANK-IN-ATTR(IX-RAD)                 
064800             MOVE NEJ  TO INDATA-SW                                       
064900           END-IF                                                         
065000         ELSE                                                             
065100           MOVE MFS-NUM-FAELT-FEL TO                                      
065200                             MOD-TIBERANK-IN-ATTR(IX-RAD)                 
065300           MOVE NEJ  TO INDATA-SW                                         
065400         END-IF                                                           
065500       END-IF                                                             
065600       ADD 1 TO IX-RAD                                                    
065700     END-PERFORM                                                          
065800                                                                          
065900     IF INDATA-FEL                                                        
066000       MOVE DATUM-PASSERAT TO MOD-TEMFSFEL                                
066100       MOVE +1 TO IX-RAD                                                  
066200       PERFORM UNTIL IX-RAD > MAX-IX-RAD                                  
066300         MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                              
066400                                   MOD-IDDC        (IX-RAD)               
066500                                   MOD-IDLEVNR     (IX-RAD)               
066600                                   MOD-TIREGDAT    (IX-RAD)               
066700                                   MOD-IDKUNDRF    (IX-RAD)               
066800                                   MOD-KVBEART-UT  (IX-RAD)               
066900                                   MOD-TIBERANK-UT (IX-RAD)               
067000                                   MOD-TIREGTID    (IX-RAD)               
067100         ADD +1 TO IX-RAD                                                 
067200       END-PERFORM                                                        
067300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
067400     END-IF                                                               
067500     .                                                                    
067600     EJECT                                                                
067700 H-UPPDATERA SECTION.                                                     
067800                                                                          
067900     MOVE +1 TO IX-RAD                                                    
068000     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
068100       IF MID-KVBEART-IN(IX-RAD) NOT = ALL '+' OR                         
068200                          MID-TIBERANK-IN(IX-RAD) NOT = ALL '+'           
068300         MOVE MID-TIREGDAT(IX-RAD) TO W-DAREGDAT-X                        
068400         IF MID-TIREGDAT(IX-RAD) NOT = ZERO                               
068500           IF MID-TIREGDAT(IX-RAD) < 500000                               
068600             MOVE 20               TO W-DAREGDAT-X (1:2)                  
068700           ELSE                                                           
068800             IF MID-TIREGDAT(IX-RAD) < 999999                             
068900               MOVE 19             TO W-DAREGDAT-X (1:2)                  
069000             ELSE                                                         
069100               MOVE 99999999       TO W-DAREGDAT-X                        
069200             END-IF                                                       
069300           END-IF                                                         
069400         END-IF                                                           
069500         MOVE MID-TIREGTID(IX-RAD) TO W-TIREGTID-X                        
069600         PERFORM IMS-GHU-INLC-ORD                                         
069700         IF SEGMENT-FINNS                                                 
069800           MOVE SPACE        TO WS-EJ-TRAEFF                              
069900           IF MID-KVBEART-IN(IX-RAD) NOT = ALL '+'                        
070000             MOVE MID-KVBEART-IN(IX-RAD) TO WS-KVBEART                    
070100                                        WS-KVBEART-IN                     
070200             COMPUTE WS-KVBEART =                                         
070300                       WS-KVBEART - INLC-ORD-KVBEART                      
070400             MOVE MID-IDDC(IX-RAD) TO W-IDDC                              
070500             PERFORM IMS-GET-ARTS-SLAG                                    
070600             IF SEGMENT-FINNS                                             
070700               COMPUTE WS-KVBEART =                                       
070800                           ARTS-SLAG-KVBEART + WS-KVBEART                 
070900               MOVE WS-KVBEART TO ARTS-SLAG-KVBEART                       
071000               PERFORM IMS-REPL-ARTS-SLAG                                 
071100               MOVE MID-KVBEART-IN(IX-RAD) TO INLC-ORD-KVBEART            
071200                                         MOD-KVBEART-UT(IX-RAD)           
071300             END-IF                                                       
071400           ELSE                                                           
071500             MOVE MFS-ROER-EJ-FAELT TO MOD-KVBEART-IN(IX-RAD)             
071600           END-IF                                                         
071700                                                                          
071800           IF MID-TIBERANK-IN(IX-RAD) NOT = ALL '+'                       
071900             MOVE MID-TIBERANK-IN(IX-RAD) TO INLC-ORD-TIBERANK            
072000                                        MOD-TIBERANK-UT(IX-RAD)           
072100           ELSE                                                           
072200             MOVE MFS-ROER-EJ-FAELT TO MOD-TIBERANK-IN(IX-RAD)            
072300           END-IF                                                         
072400                                                                          
072500           IF MID-KVBEART-IN(IX-RAD) NOT = ALL '+'                        
072600             IF WS-KVBEART-IN = 0                                         
072700               PERFORM IMS-DLET-INLC-ORD                                  
072800             ELSE                                                         
072900               PERFORM IMS-REPL-INLC-ORD                                  
073000             END-IF                                                       
073100           ELSE                                                           
073200             PERFORM IMS-REPL-INLC-ORD                                    
073300           END-IF                                                         
073400         ELSE                                                             
073500*                                                                         
073600*  DETTA SKA EJ KUNNA FÖREKOMMA                                           
073700*                                                                         
073800           MOVE MID-TIREGDAT(IX-RAD) TO WS-EJTR-TIREGDAT-X                
073900           MOVE MID-TIREGTID(IX-RAD) TO WS-EJTR-TIREGTID-X                
074000         END-IF                                                           
074100       END-IF                                                             
074200       ADD 1 TO IX-RAD                                                    
074300     END-PERFORM                                                          
074400                                                                          
074500     IF WS-EJ-TRAEFF = SPACE                                              
074600       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
074700       CALL WMEDKONV USING MED-WMEDAREA                                   
074800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
074900     ELSE                                                                 
075000*                                                                         
075100*  DETTA SKA EJ KUNNA FÖREKOMMA                                           
075200*                                                                         
075300       MOVE 'NO UPDATING DONE, CONTACT ANDERS LARSSON'                    
075400                         TO MOD-TEMFSINF                                  
075500     END-IF                                                               
075600     PERFORM MFS-RENSA-FAELT-IN                                           
075700     MOVE +1 TO IX-RAD                                                    
075800     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
075900       MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                
076000                                 MOD-IDDC        (IX-RAD)                 
076100                                 MOD-IDLEVNR     (IX-RAD)                 
076200                                 MOD-TIREGDAT    (IX-RAD)                 
076300                                 MOD-IDKUNDRF    (IX-RAD)                 
076400                                 MOD-TIREGTID    (IX-RAD)                 
076500       IF MID-KVBEART-IN (IX-RAD) = ALL '+'                               
076600         MOVE MFS-ROER-EJ-FAELT TO MOD-KVBEART-UT(IX-RAD)                 
076700       END-IF                                                             
076800       IF MID-TIBERANK-IN (IX-RAD) = ALL '+'                              
076900         MOVE MFS-ROER-EJ-FAELT TO MOD-TIBERANK-UT(IX-RAD)                
077000       END-IF                                                             
077100       ADD +1 TO IX-RAD                                                   
077200     END-PERFORM                                                          
077300     .                                                                    
077400     EJECT                                                                
077500 I-STAENG-FAELT SECTION.                                                  
077600     MOVE +1 TO IX-RAD                                                    
077700     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
077800        IF MOD-TIREGTID(IX-RAD) = 999999                                  
077900          MOVE MFS-STAENG-FAELT   TO MOD-KVBEART-IN-ATTR (IX-RAD)         
078000                                     MOD-TIBERANK-IN-ATTR(IX-RAD)         
078100        END-IF                                                            
078200        ADD +1 TO IX-RAD                                                  
078300     END-PERFORM                                                          
078400     .                                                                    
078500     EJECT                                                                
078600 MFS-RENSA-FAELT-UT SECTION.                                              
078700                                                                          
078800     MOVE 0 TO W-WDL612KY-X                                               
078900                                                                          
079000     MOVE +1 TO IX-RAD                                                    
079100     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
079200       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
079300       ADD +1 TO IX-RAD                                                   
079400     END-PERFORM                                                          
079500     .                                                                    
079600     SKIP3                                                                
079700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
079800                                                                          
079900     MOVE MFS-RENSA-FAELT   TO MOD-BEART                                  
080000                               MOD-IDDC       (IX-RAD)                    
080100                               MOD-IDLEVNR    (IX-RAD)                    
080200                               MOD-TIREGDAT   (IX-RAD)                    
080300                               MOD-IDKUNDRF   (IX-RAD)                    
080400                               MOD-KVBEART-UT (IX-RAD)                    
080500                               MOD-TIBERANK-UT(IX-RAD)                    
080600                               MOD-TIREGTID   (IX-RAD)                    
080700     .                                                                    
080800     SKIP3                                                                
080900 MFS-RENSA-FAELT-IN SECTION.                                              
081000                                                                          
081100*    --- ALLA INDATA-FÄLT                                                 
081200     MOVE +1 TO IX-RAD                                                    
081300     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
081400       MOVE MFS-RENSA-FAELT TO MOD-KVBEART-IN(IX-RAD)                     
081500                               MOD-TIBERANK-IN(IX-RAD)                    
081600       ADD +1 TO IX-RAD                                                   
081700     END-PERFORM                                                          
081800     .                                                                    
081900     EJECT                                                                
082000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
082100                                                                          
082200*    --- ALLA INDATA-FÄLT                                                 
082300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                           
082400                                 MOD-IDDC-IN                              
082500     MOVE +1 TO IX-RAD                                                    
082600     PERFORM UNTIL IX-RAD > MAX-IX-RAD                                    
082700       MOVE MFS-ROER-EJ-FAELT TO MOD-KVBEART-IN(IX-RAD)                   
082800                                 MOD-TIBERANK-IN(IX-RAD)                  
082900       ADD +1 TO IX-RAD                                                   
083000     END-PERFORM                                                          
083100     .                                                                    
083200     EJECT                                                                
083300* --- IMS SEKTIONER ---                                                   
083400     SKIP3                                                                
083500 IMS-GET-MSG SECTION.                                                     
083600                                                                          
083700     MOVE '  QC' TO GODK-STATUSKODER                                      
083800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
083900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084000     PERFORM IMS-STATUSKONTROLL                                           
084100     .                                                                    
084200     SKIP3                                                                
084300 IMS-INSERT-MSG SECTION.                                                  
084400                                                                          
084500     IF MSGI-IDLAND-SPR = 'GB'                                            
084600       MOVE 'N' TO MFS-KDHUVOMR                                           
084700     END-IF                                                               
084800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
084900     MOVE SPACE TO GODK-STATUSKODER                                       
085000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
085100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085200     PERFORM IMS-STATUSKONTROLL                                           
085300     .                                                                    
085400     EJECT                                                                
085500 IMS-GET-INLC-ART SECTION.                                                
085600                                                                          
085700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
085800          DELIMITED BY SIZE INTO SSA1                                     
085900     MOVE '  GE' TO GODK-STATUSKODER                                      
086000     CALL CBLTDLI USING GU INLC-PCB DLI-IO-WLINLC01 SSA1                  
086100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
086200     PERFORM IMS-STATUSKONTROLL                                           
086300     .                                                                    
086400     EJECT                                                                
086500 IMS-GET-INLC-ORD SECTION.                                                
086600                                                                          
086700     STRING 'WLINLC12(WDL612KY=>' W-WDL612KY-X ')'                        
086800          DELIMITED BY SIZE INTO SSA1                                     
086900     MOVE '  GE' TO GODK-STATUSKODER                                      
087000     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-WLINLC12 SSA1                
087100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
087200     PERFORM IMS-STATUSKONTROLL                                           
087300     .                                                                    
087400     SKIP3                                                                
087500 IMS-GHU-INLC-ORD SECTION.                                                
087600                                                                          
087700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
087800          DELIMITED BY SIZE INTO SSA1                                     
087900     STRING 'WLINLC12(WDL612KY =' W-WDL612KY-X ')'                        
088000          DELIMITED BY SIZE INTO SSA2                                     
088100     MOVE '  GE' TO GODK-STATUSKODER                                      
088200     CALL CBLTDLI USING GHU INLC-PCB DLI-IO-WLINLC12 SSA1 SSA2            
088300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
088400     PERFORM IMS-STATUSKONTROLL                                           
088500     .                                                                    
088600     SKIP3                                                                
088700 IMS-REPL-INLC-ORD SECTION.                                               
088800                                                                          
088900     MOVE '  ' TO GODK-STATUSKODER                                        
089000     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-WLINLC12                     
089100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
089200     PERFORM IMS-STATUSKONTROLL                                           
089300     .                                                                    
089400     EJECT                                                                
089500 IMS-DLET-INLC-ORD SECTION.                                               
089600                                                                          
089700     MOVE '  ' TO GODK-STATUSKODER                                        
089800     CALL CBLTDLI USING DLET INLC-PCB DLI-IO-WLINLC12                     
089900     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
090000     PERFORM IMS-STATUSKONTROLL                                           
090100     .                                                                    
090200     EJECT                                                                
090300 IMS-GET-ARTS-SLAG SECTION.                                               
090400                                                                          
090500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
090600          DELIMITED BY SIZE INTO SSA1                                     
090700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
090800          DELIMITED BY SIZE INTO SSA2                                     
090900     MOVE '  GE' TO GODK-STATUSKODER                                      
091000     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
091100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
091200     PERFORM IMS-STATUSKONTROLL                                           
091300     .                                                                    
091400     SKIP3                                                                
091500 IMS-REPL-ARTS-SLAG SECTION.                                              
091600                                                                          
091700     MOVE '  ' TO GODK-STATUSKODER                                        
091800     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WLARTS11                     
091900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     EJECT                                                                
092300 IMS-GU-WLBENA11 SECTION.                                                 
092400                                                                          
092500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
092600          DELIMITED BY SIZE INTO SSA1                                     
092700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
092800          DELIMITED BY SIZE INTO SSA2                                     
092900     MOVE '  GE' TO GODK-STATUSKODER                                      
093000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
093100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-GU-WDB601    SECTION.                                                
093600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
093700          DELIMITED BY SIZE INTO SSA1                                     
093800     MOVE '  GE' TO GODK-STATUSKODER                                      
093900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
094000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
094100     PERFORM IMS-STATUSKONTROLL                                           
094200     IF SEGMENT-SAKNAS                                                    
094300         MOVE SPACE TO DCS-KDDC                                           
094400     END-IF                                                               
094500     .                                                                    
094600 IMS-STATUSKONTROLL SECTION.                                              
094700                                                                          
094800     SET STATUS-IX TO 1                                                   
094900     SEARCH GODK-STATUS                                                   
095000       AT END                                                             
095100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
095200         DELIMITED BY SIZE INTO FELTEXT                                   
095300         CALL FELLOG                                                      
095400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
095500         CONTINUE                                                         
095600     END-SEARCH                                                           
095700     .                                                                    
095800     EJECT                                                                
095900*    -COPY WY2000P1                                                       
