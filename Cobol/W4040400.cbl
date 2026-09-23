000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4040400.                                                
000300 AUTHOR.         GÖRAN KJELLSON    GUIDE                                  
000400 DATE-WRITTEN.   AUGUSTI   2006                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HANTERING AV DC-STYRREGISTER WDB6                                
000900*                                                                         
000910*        THE PROGRAM READS     WDB6 WDK7B WDG3                            
000920*                                                                         
001000*        PROGRAMMET UPPDATERAR WDB6, WDR6 WDG3                            
001100*                                                                         
001110*        E-TRACKER 10243132, CHINA PART EXPORT PROJECT                    
001120*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T404                                              
001400*        MID:         W4I40401                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O40401                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4040400'.            
002600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002700                                                                          
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  YES                         PIC X       VALUE 'Y'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100                                                                          
003100 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
003100 77  MAX-INDX                    PIC S9(4)   VALUE +7  COMP SYNC.         
003100                                                                          
003200*01  -COPY WWDCKONS.                                                      
003210*01  -COPY WWDC99.                                                        
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
004500     88  EGEN-MID                            VALUE '4404'.                
004600     88  GODK-MID                            VALUE '4402' '4403'          
004700                                                   '0551'.                
004800     88  HELP-MID                            VALUE '0551'.                
004900     EJECT                                                                
005000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005100 01  GENERELLA-SUBPROGRAM.                                                
005200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
005700     EJECT                                                                
005800                                                                          
005900*   -COPY WDECAREA.                                                       
006000                                                                          
006100     EJECT                                                                
006200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006300*01 -COPY WMEDAREA                                                        
006400     SKIP3                                                                
006500 01  MESSAGE-CODES.                                                       
006600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006800     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
006900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007000     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
007100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007500*                                                                         
007600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007700     SKIP3                                                                
007800*01 -COPY WMSGINIT                                                        
007900     EJECT                                                                
008000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008100*                                                                         
008200 01  SPAR-AREA.                                                           
008300     03  SPAR-IDTRANS           PIC X(4)    VALUE '4404'.                 
008400     03  SPAR-IDDC-REF          PIC X(2)    VALUE SPACE.                  
008410     03  WS-SPAR-DC             PIC X(2)    VALUE SPACE.                  
008420     03  WS-SPAR-REF-DC         PIC X(2)    VALUE SPACE.                  
008500     EJECT                                                                
008600 01  WS-AREA.                                                             
008700     03  WS-PERCENT             PIC 9(3)V9(2)                             
008800                                            VALUE ZERO.                   
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W4I40401                                                   
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W4O40401                                                 
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300     SKIP3                                                                
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-IDDC-X.                                                        
011200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011300     03  W-IDDC-REF-X.                                                    
011400         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
011500     SKIP2                                                                
011510     03  W-WDK7B1KY-MIN-X.                                                
011520         05  W-IDDC-REF-B1-MIN   PIC X(2)    VALUE SPACE.                 
011530         05  W-IDDC-B1-MIN       PIC X(2)    VALUE SPACE.                 
011540         05  W-IDARTNR-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
011550     03  W-WDK7B1KY-MAX-X.                                                
011560         05  W-IDDC-REF-B1-MAX   PIC X(2)    VALUE SPACE.                 
011570         05  W-IDDC-B1-MAX       PIC X(2)    VALUE SPACE.                 
011580         05  W-IDARTNR-B1-MAX    PIC S9(9)                                
011590                                         VALUE +999999999 COMP-3.         
011591     03  W-WDG301-2203-X.                                                 
011592         05  W-IDHTYP            PIC X(04)   VALUE '2203'.                
011593         05  W-IDDC-2203         PIC X(02)   VALUE SPACE.                 
011594         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
011595                                                                          
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012100     SKIP2                                                                
012200 01  GODK-STATUSKODER.                                                    
012300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     SKIP3                                                                
012500 01  SSA1                        PIC X(64).                               
012600 01  SSA2                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNKTIONSKODER                                               
012900*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200                                                                          
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
013400 01  DLI-IO-WDB601.                                                       
013500*    03  -COPY WDB601                                                     
013600                                                                          
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
013800 01  DLI-IO-WDB616.                                                       
013900*    03  -COPY WDB616                                                     
014000     EJECT                                                                
014100                                                                          
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR601'.                      
014700 01  DLI-IO-WDR601.                                                       
014800*    03  -COPY WDR601                                                     
014900*    05  LOGG  -COPY W414DCSA          -RED FIL-WDR601-DATA.              
015000     EJECT                                                                
015010 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK7B1'.         
015020 01  DLI-IO-WDK7B1.                                                       
015030*    03  -COPY WDK7B1                                                     
015040                                                                          
015050 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDG301'.         
015060 01  DLI-IO-WDG301.                                                       
015070*    03  -COPY WDG301                                                     
015080                                                                          
015100 LINKAGE SECTION.                                                         
015200*01  -COPY W0009   -PRE MSG-                                              
015300*01  -COPY W0008   -PRE WDP7-                                             
015400     05  FILLER                  PIC X.                                   
015500                                                                          
015600*01  -COPY W0008  -PRE WDB6-                                              
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008  -PRE WDR6-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016110*01  -COPY W0008  -PRE WDK7B-                                             
016120     05  FILLER                  PIC X.                                   
016121*01  -COPY W0008  -PRE WDG3-                                              
016122     05  FILLER                  PIC X.                                   
016130     EJECT                                                                
016200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB WDR6-PCB             
016210                           WDK7B-PCB WDG3-PCB.                            
016300 MAIN SECTION.                                                            
016400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB WDR6-PCB             
016410                           WDK7B-PCB WDG3-PCB.                            
016500                                                                          
016600     PERFORM IMS-GET-MSG                                                  
016700     IF SEGMENT-FINNS                                                     
016800       PERFORM A-INIT                                                     
016900       PERFORM B-KOLLA-NYCKLAR                                            
017000       IF NYCKLAR-OK                                                      
017100         IF MFS-UPDATE                                                    
017200           PERFORM G-KOLLA-INPUT                                          
017300           IF INDATA-OK                                                   
017400             PERFORM H-UPPDATERA                                          
017500           END-IF                                                         
017600         ELSE                                                             
017700           IF MFS-FIRST                                                   
017800             PERFORM C-FOERSTA-SIDA                                       
017900           ELSE                                                           
018000             PERFORM E-SAMMA-SIDA                                         
018100           END-IF                                                         
018200         END-IF                                                           
018300         PERFORM F-LAES-VISA-INFO                                         
018400       END-IF                                                             
018500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40401 + 4                      
018600       PERFORM IMS-INSERT-MSG                                             
018700     END-IF                                                               
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT SECTION.                                                          
019400                                                                          
019500     IF MSG-DUBBLA-TRANSKODER                                             
019600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I40401                 
019700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019900     ELSE                                                                 
020000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I40401                  
020100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020300     END-IF                                                               
020400                                                                          
020500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020800                                                                          
020900     MOVE LOW-VALUE TO MSG-AREA                                           
021000     MOVE 'W4O404N1' TO MFS-IDMOD                                         
021100     MOVE '4404' TO MOD-IDTRANS                                           
021200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021210                                                                          
021220     MOVE SPACE           TO MED-IDMFSFEL                                 
021300                                                                          
021400     IF EGEN-MID OR HELP-MID                                              
021500       CONTINUE                                                           
021600     ELSE                                                                 
021700       MOVE SPACE TO MFS-KDTRTYP                                          
021800       MOVE '7' TO MFS-IDPFK                                              
021900     END-IF                                                               
022000     MOVE IDPGM                TO  FIL-IDPGM                              
022100     ACCEPT FIL-TIREGDAT       FROM  DATE                                 
022200     ACCEPT FIL-TIKLOCK        FROM  TIME                                 
022300     MOVE ZERO                 TO  FIL-IDSEKVNR                           
022400     MOVE 'W414'               TO  FIL-CT-IDSYSTEM                        
022500     MOVE 'DCS'                TO  FIL-CT-IDPTYP                          
022600     MOVE 'A'                  TO  FIL-CT-IDVTYP                          
022700     .                                                                    
022800     EJECT                                                                
022900 B-KOLLA-NYCKLAR SECTION.                                                 
023000                                                                          
023100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023200     MOVE '001'             TO MSGI-KDCALL                                
023300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023500     MOVE '4404'            TO MSGI-IDTRANS                               
023510     IF EGEN-MID OR GODK-MID                                              
023600        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
023601        MOVE MID-IDDC-REF-IN TO MSGI-IDDC-SEND                            
023610     END-IF                                                               
023700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
023900                                                                          
024000*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
024100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
024200                                                                          
024300     MOVE JA TO NYCKLAR-SW                                                
024400                                                                          
024500                                                                          
024600*    -- KONTROLL AV IDDC                                                  
024700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
024800                                                                          
024900     IF MID-IDDC-IN NOT = ALL '+'                                         
025000       MOVE '7'         TO MFS-IDPFK                                      
025100       MOVE SPACE       TO MFS-KDTRTYP                                    
025200     END-IF                                                               
025300     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
025400                           MOD-IDDC-UT                                    
025500                           DCSL-IDDC                                      
025510                           WS-SPAR-DC                                     
025600     PERFORM IMS-GU-WDB601                                                
025700                                                                          
025800     IF SEGMENT-SAKNAS                                                    
025900       MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                                
026000       CALL WMEDKONV USING MED-WMEDAREA                                   
026100       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
026200       PERFORM MFS-RENSA-FAELT-UT                                         
026300       MOVE NEJ            TO NYCKLAR-SW                                  
026400     ELSE                                                                 
026820       MOVE MFS-RENSA-FAELT TO MOD-IDDC-REF-IN                            
026830                                                                          
026840       IF MID-IDDC-REF-IN NOT = ALL '+'                                   
026850         MOVE MID-IDDC-REF-IN                                             
026860                          TO SPAR-IDDC-REF                                
026870         MOVE '7'       TO MFS-IDPFK                                      
026880       ELSE                                                               
026881          MOVE MSGI-IDDC-SEND TO MID-IDDC-REF-IN                          
026882                                   W-IDDC-REF                             
026892          PERFORM IMS-GU-WDB616                                           
026893          IF SEGMENT-FINNS                                                
026894             MOVE REF-IDDC-REF TO SPAR-IDDC-REF                           
026895          ELSE                                                            
026896             IF MFS-UPDATE                                                
026897                CONTINUE                                                  
026898             ELSE                                                         
026899                PERFORM IMS-GU-WDB601                                     
026900                PERFORM IMS-GNP-WDB616                                    
026901                IF SEGMENT-FINNS                                          
026902                   MOVE REF-IDDC-REF TO SPAR-IDDC-REF                     
026903                ELSE                                                      
026905                   MOVE WC-CDC-SE    TO SPAR-IDDC-REF                     
026906                END-IF                                                    
026907             END-IF                                                       
026908          END-IF                                                          
026910       END-IF                                                             
026911       MOVE SPAR-IDDC-REF TO W-IDDC                                       
026912                             W-IDDC-REF                                   
026913                             MOD-IDDC-REF-UT                              
026914                             WS-SPAR-REF-DC                               
026915       PERFORM IMS-GU-WDB601                                              
026916                                                                          
026917       IF SEGMENT-SAKNAS                                                  
026918         MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                              
026919         CALL WMEDKONV USING MED-WMEDAREA                                 
026920         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
026921         PERFORM MFS-RENSA-FAELT-UT                                       
026922         MOVE NEJ            TO NYCKLAR-SW                                
026923       ELSE                                                               
026925         IF MID-IDDC-REF-IN = ALL '+' AND MFS-FIRST                       
026926            MOVE MSGI-IDDC-KEY TO W-IDDC                                  
026927            PERFORM IMS-GU-WDB616                                         
026928            IF SEGMENT-SAKNAS                                             
026929               MOVE WC-CDC-SE TO MOD-IDDC-REF-UT                          
026930                                  SPAR-IDDC-REF                           
026931            END-IF                                                        
026932         END-IF                                                           
026941       END-IF                                                             
026950     END-IF                                                               
027000                                                                          
031990     MOVE MSGI-IDUSER   TO DCSL-IDUSER                                    
032000     MOVE MSGI-BEANST   TO DCSL-BEANST                                    
032100     MOVE 'OLD VALUE: ' TO DCSL-BETEXT-OLD                                
032200     MOVE 'NEW VALUE: ' TO DCSL-BETEXT-NEW                                
032300                                                                          
032400     MOVE '002'             TO MSGI-KDCALL                                
032500     MOVE '4404'            TO MSGI-IDTRANS                               
032600     MOVE SPAR-AREA         TO MSGI-SPAR-AREA                             
032700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
032800     .                                                                    
032900     EJECT                                                                
033000 C-FOERSTA-SIDA SECTION.                                                  
033100                                                                          
033200     PERFORM MFS-RENSA-FAELT-IN                                           
033300     .                                                                    
033400     EJECT                                                                
033500 E-SAMMA-SIDA SECTION.                                                    
033600                                                                          
033700     IF EGEN-MID OR HELP-MID                                              
033800       IF  MID-INPUT    = ALL '+'                                         
033810       AND MID-FLTABORT = ALL '+'                                         
033900         PERFORM MFS-RENSA-FAELT-IN                                       
034000       ELSE                                                               
034100         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
034200         CALL WMEDKONV USING MED-WMEDAREA                                 
034300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
034400         PERFORM EA-MID-INDATA-TILL-MOD                                   
034500       END-IF                                                             
034600     ELSE                                                                 
034700       PERFORM MFS-RENSA-FAELT-IN                                         
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 EA-MID-INDATA-TILL-MOD SECTION.                                          
035200                                                                          
035300     IF MID-FLTABORT = ALL '+'                                            
035400       MOVE MFS-RENSA-FAELT       TO MOD-FLTABORT                         
035500     ELSE                                                                 
035600       MOVE MID-FLTABORT          TO MOD-FLTABORT                         
035700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTABORT-ATTR                    
035800     END-IF                                                               
035810                                                                          
035820     IF MID-KDFRAKT-BPS-IN = ALL '+'                                      
035830       MOVE MFS-RENSA-FAELT       TO MOD-KDFRAKT-BPS-IN                   
035840     ELSE                                                                 
035850       MOVE MID-KDFRAKT-BPS-IN TO MOD-KDFRAKT-BPS-IN                      
035860       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFRAKT-BPS-IN-ATTR              
035870     END-IF                                                               
035900                                                                          
036000     IF MID-KDFRAKT-SBPS-IN = ALL '+'                                     
036100       MOVE MFS-RENSA-FAELT       TO MOD-KDFRAKT-SBPS-IN                  
036200     ELSE                                                                 
036300       MOVE MID-KDFRAKT-SBPS-IN TO MOD-KDFRAKT-SBPS-IN                    
036400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFRAKT-SBPS-IN-ATTR             
036500     END-IF                                                               
036600                                                                          
036700     IF MID-IDDISTR-REFILL-IN = ALL '+'                                   
036800       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-REFILL-IN                
036900     ELSE                                                                 
037000       MOVE MID-IDDISTR-REFILL-IN TO MOD-IDDISTR-REFILL-IN                
037100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-REFILL-IN-ATTR           
037200     END-IF                                                               
037300                                                                          
037400     IF MID-IDDISTR-RETUR-IN = ALL '+'                                    
037500       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-RETUR-IN                 
037600     ELSE                                                                 
037700       MOVE MID-IDDISTR-RETUR-IN TO MOD-IDDISTR-RETUR-IN                  
037800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-RETUR-IN-ATTR            
037900     END-IF                                                               
038000                                                                          
038100     IF MID-IDDISTR-QRETUR-IN = ALL '+'                                   
038200       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-QRETUR-IN                
038300     ELSE                                                                 
038400       MOVE MID-IDDISTR-QRETUR-IN TO MOD-IDDISTR-QRETUR-IN                
038500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-QRETUR-IN-ATTR           
038600     END-IF                                                               
038700                                                                          
038800     IF MID-IDDISTR-SKROT-IN = ALL '+'                                    
038900       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-SKROT-IN                 
039000     ELSE                                                                 
039100       MOVE MID-IDDISTR-SKROT-IN  TO MOD-IDDISTR-SKROT-IN                 
039200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-SKROT-IN-ATTR            
039300     END-IF                                                               
039400                                                                          
039500     IF MID-IDDISTR-QSKROT-IN = ALL '+'                                   
039600       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-QSKROT-IN                
039700     ELSE                                                                 
039800       MOVE MID-IDDISTR-QSKROT-IN TO MOD-IDDISTR-QSKROT-IN                
039900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-QSKROT-IN-ATTR           
040000     END-IF                                                               
040100                                                                          
040200     IF MID-IDDISTR-RSKROT-IN = ALL '+'                                   
040300       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-RSKROT-IN                
040400     ELSE                                                                 
040500       MOVE MID-IDDISTR-RSKROT-IN TO MOD-IDDISTR-RSKROT-IN                
040600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-RSKROT-IN-ATTR           
040700     END-IF                                                               
040800                                                                          
040900     IF MID-IDDISTR-MIX-IN = ALL '+'                                      
041000       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-MIX-IN                   
041100     ELSE                                                                 
041200       MOVE MID-IDDISTR-MIX-IN    TO MOD-IDDISTR-MIX-IN                   
041300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-MIX-IN-ATTR              
041400     END-IF                                                               
041500                                                                          
041600     IF MID-IDDISTR-JUST-IN = ALL '+'                                     
041700       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-JUST-IN                  
041800     ELSE                                                                 
041900       MOVE MID-IDDISTR-JUST-IN   TO MOD-IDDISTR-JUST-IN                  
042000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-JUST-IN-ATTR             
042100     END-IF                                                               
042200                                                                          
042300     IF MID-IDKUNDNR-SORD-IN = ALL '+'                                    
042400       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-SORD-IN                 
042500     ELSE                                                                 
042600       MOVE MID-IDKUNDNR-SORD-IN TO MOD-IDKUNDNR-SORD-IN                  
042700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-SORD-IN-ATTR            
042800     END-IF                                                               
042900                                                                          
043000     IF MID-IDKUNDNR-RETUR-IN = ALL '+'                                   
043100       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-RETUR-IN                
043200     ELSE                                                                 
043300       MOVE MID-IDKUNDNR-RETUR-IN TO MOD-IDKUNDNR-RETUR-IN                
043400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-RETUR-IN-ATTR           
043500     END-IF                                                               
043600                                                                          
043700     IF MID-IDKUNDNR-QRETUR-IN = ALL '+'                                  
043800       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-QRETUR-IN               
043900     ELSE                                                                 
044000       MOVE MID-IDKUNDNR-QRETUR-IN TO MOD-IDKUNDNR-QRETUR-IN              
044100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-QRETUR-IN-ATTR          
044200     END-IF                                                               
044300                                                                          
044400     IF MID-IDKUNDNR-SKROT-IN = ALL '+'                                   
044500       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-SKROT-IN                
044600     ELSE                                                                 
044700       MOVE MID-IDKUNDNR-SKROT-IN TO MOD-IDKUNDNR-SKROT-IN                
044800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-SKROT-IN-ATTR           
044900     END-IF                                                               
045000                                                                          
045100     IF MID-IDKUNDNR-QSKROT-IN = ALL '+'                                  
045200       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-QSKROT-IN               
045300     ELSE                                                                 
045400       MOVE MID-IDKUNDNR-QSKROT-IN TO MOD-IDKUNDNR-QSKROT-IN              
045500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-QSKROT-IN-ATTR          
045600     END-IF                                                               
045700                                                                          
045800     IF MID-IDKUNDNR-RSKROT-IN = ALL '+'                                  
045900       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-RSKROT-IN               
046000     ELSE                                                                 
046100       MOVE MID-IDKUNDNR-RSKROT-IN TO MOD-IDKUNDNR-RSKROT-IN              
046200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-RSKROT-IN-ATTR          
046300     END-IF                                                               
046400                                                                          
046500     IF MID-IDKUNDNR-MIX-IN = ALL '+'                                     
046600       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-MIX-IN                  
046700     ELSE                                                                 
046800       MOVE MID-IDKUNDNR-MIX-IN   TO MOD-IDKUNDNR-MIX-IN                  
046900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-MIX-IN-ATTR             
047000     END-IF                                                               
047100                                                                          
047200     IF MID-IDKUNDNR-JUST-IN = ALL '+'                                    
047300       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-JUST-IN                 
047400     ELSE                                                                 
047500       MOVE MID-IDKUNDNR-JUST-IN   TO MOD-IDKUNDNR-JUST-IN                
047600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-JUST-IN-ATTR            
047700     END-IF                                                               
047800                                                                          
047900     IF MID-IDKUNDNR-BPS-IN = ALL '+'                                     
048000       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-BPS-IN                  
048100     ELSE                                                                 
048200       MOVE MID-IDKUNDNR-BPS-IN TO MOD-IDKUNDNR-BPS-IN                    
048300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-BPS-IN-ATTR             
048400     END-IF                                                               
048500                                                                          
048600     IF MID-IDKUNDNR-TRETUR-IN = ALL '+'                                  
048700       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-TRETUR-IN               
048800     ELSE                                                                 
048900       MOVE MID-IDKUNDNR-TRETUR-IN TO MOD-IDKUNDNR-TRETUR-IN              
049000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-TRETUR-IN-ATTR          
049100     END-IF                                                               
049200                                                                          
049300     IF MID-IDKUNDNR-SQRET-IN = ALL '+'                                   
049400       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-SQRET-IN                
049500     ELSE                                                                 
049600       MOVE MID-IDKUNDNR-SQRET-IN TO MOD-IDKUNDNR-SQRET-IN                
049700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-SQRET-IN-ATTR           
049800     END-IF                                                               
049900                                                                          
050000     IF MID-IDKUNDNR-SBPS-IN = ALL '+'                                    
050100       MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-SBPS-IN                 
050200     ELSE                                                                 
050300       MOVE MID-IDKUNDNR-SBPS-IN  TO MOD-IDKUNDNR-SBPS-IN                 
050400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-SBPS-IN-ATTR            
050500     END-IF                                                               
050600                                                                          
050700     IF MID-IDKUNDNR-SRETUR-IN = ALL '+'                                  
050800       MOVE MFS-RENSA-FAELT        TO MOD-IDKUNDNR-SRETUR-IN              
050900     ELSE                                                                 
051000       MOVE MID-IDKUNDNR-SRETUR-IN TO MOD-IDKUNDNR-SRETUR-IN              
051100       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKUNDNR-SRETUR-IN-ATTR         
051200     END-IF                                                               
051300                                                                          
053500     IF MID-IDPERSON-REM-IN = ALL '+'                                     
053600       MOVE MFS-RENSA-FAELT       TO MOD-IDPERSON-REM-IN                  
053700     ELSE                                                                 
053800       MOVE MID-IDPERSON-REM-IN   TO MOD-IDPERSON-REM-IN                  
053900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-REM-IN-ATTR             
054000     END-IF                                                               
054100                                                                          
054200     IF MID-IDKST-JUST-IN = ALL '+'                                       
054300       MOVE MFS-RENSA-FAELT       TO MOD-IDKST-JUST-IN                    
054400     ELSE                                                                 
054500       MOVE MID-IDKST-JUST-IN     TO MOD-IDKST-JUST-IN                    
054600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKST-JUST-IN-ATTR               
054700     END-IF                                                               
054800                                                                          
054900     IF MID-IDKONTO-JUST-IN = ALL '+'                                     
055000       MOVE MFS-RENSA-FAELT       TO MOD-IDKONTO-JUST-IN                  
055100     ELSE                                                                 
055200       MOVE MID-IDKONTO-JUST-IN   TO MOD-IDKONTO-JUST-IN                  
055300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKONTO-JUST-IN-ATTR             
055400     END-IF                                                               
055500                                                                          
055600     IF MID-IDANALYS-JUST-IN = ALL '+'                                    
055700       MOVE MFS-RENSA-FAELT        TO MOD-IDANALYS-JUST-IN                
055800     ELSE                                                                 
055900       MOVE MID-IDANALYS-JUST-IN   TO MOD-IDANALYS-JUST-IN                
056000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDANALYS-JUST-IN-ATTR           
056100     END-IF                                                               
056200                                                                          
056210     IF MID-KDRT-JUST-IN = ALL '+'                                        
056220       MOVE MFS-RENSA-FAELT        TO MOD-KDRT-JUST-IN                    
056230     ELSE                                                                 
056240       MOVE MID-KDRT-JUST-IN       TO MOD-KDRT-JUST-IN                    
056250       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDRT-JUST-IN-ATTR               
056260     END-IF                                                               
056270                                                                          
056300     IF MID-IDKST-SKROT-IN = ALL '+'                                      
056400       MOVE MFS-RENSA-FAELT       TO MOD-IDKST-SKROT-IN                   
056500     ELSE                                                                 
056600       MOVE MID-IDKST-SKROT-IN    TO MOD-IDKST-SKROT-IN                   
056700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKST-SKROT-IN-ATTR              
056800     END-IF                                                               
056900                                                                          
057000     IF MID-IDKONTO-SKROT-IN = ALL '+'                                    
057100       MOVE MFS-RENSA-FAELT       TO MOD-IDKONTO-SKROT-IN                 
057200     ELSE                                                                 
057300       MOVE MID-IDKONTO-SKROT-IN  TO MOD-IDKONTO-SKROT-IN                 
057400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKONTO-SKROT-IN-ATTR            
057500     END-IF                                                               
057600                                                                          
057700     IF MID-IDANALYS-SKROT-IN = ALL '+'                                   
057800       MOVE MFS-RENSA-FAELT        TO MOD-IDANALYS-SKROT-IN               
057900     ELSE                                                                 
058000       MOVE MID-IDANALYS-SKROT-IN  TO MOD-IDANALYS-SKROT-IN               
058100       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDANALYS-SKROT-IN-ATTR          
058200     END-IF                                                               
058300                                                                          
058310     IF MID-KDRT-SKROT-IN = ALL '+'                                       
058320       MOVE MFS-RENSA-FAELT        TO MOD-KDRT-SKROT-IN                   
058330     ELSE                                                                 
058340       MOVE MID-KDRT-SKROT-IN      TO MOD-KDRT-SKROT-IN                   
058350       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDRT-SKROT-IN-ATTR              
058360     END-IF                                                               
058370                                                                          
058400     IF MID-IDKST-MIX-IN = ALL '+'                                        
058500       MOVE MFS-RENSA-FAELT       TO MOD-IDKST-MIX-IN                     
058600     ELSE                                                                 
058700       MOVE MID-IDKST-MIX-IN      TO MOD-IDKST-MIX-IN                     
058800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKST-MIX-IN-ATTR                
058900     END-IF                                                               
059000                                                                          
059100     IF MID-IDKONTO-MIX-IN = ALL '+'                                      
059200       MOVE MFS-RENSA-FAELT       TO MOD-IDKONTO-MIX-IN                   
059300     ELSE                                                                 
059400       MOVE MID-IDKONTO-MIX-IN    TO MOD-IDKONTO-MIX-IN                   
059500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKONTO-MIX-IN-ATTR              
059600     END-IF                                                               
059700                                                                          
059800     IF MID-IDANALYS-MIX-IN = ALL '+'                                     
059900       MOVE MFS-RENSA-FAELT        TO MOD-IDANALYS-MIX-IN                 
060000     ELSE                                                                 
060100       MOVE MID-IDANALYS-MIX-IN    TO MOD-IDANALYS-MIX-IN                 
060200       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDANALYS-MIX-IN-ATTR            
060300     END-IF                                                               
060310                                                                          
060320     IF MID-KDRT-MIX-IN = ALL '+'                                         
060330       MOVE MFS-RENSA-FAELT        TO MOD-KDRT-MIX-IN                     
060340     ELSE                                                                 
060350       MOVE MID-KDRT-MIX-IN        TO MOD-KDRT-MIX-IN                     
060360       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDRT-MIX-IN-ATTR                
060370     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600 F-LAES-VISA-INFO SECTION.                                                
060700                                                                          
060800     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
060900     PERFORM IMS-GU-WDB601                                                
061000                                                                          
061100     IF SEGMENT-SAKNAS                                                    
061200       MOVE ERR-DC-MISSING TO MED-IDMFSFEL                                
061300       CALL WMEDKONV USING MED-WMEDAREA                                   
061400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
061500       PERFORM MFS-RENSA-FAELT-UT                                         
061600     ELSE                                                                 
061700       MOVE DCS-KDFRAKT-BPS       TO MOD-KDFRAKT-BPS                      
061800       MOVE DCS-KDFRAKT-SBPS      TO MOD-KDFRAKT-SBPS                     
061900       MOVE DCS-IDPERSON-REM      TO MOD-IDPERSON-REM                     
062000       MOVE DCS-IDKST-JUST        TO MOD-IDKST-JUST                       
062100       MOVE DCS-IDKONTO-JUST      TO MOD-IDKONTO-JUST                     
062200       MOVE DCS-IDANALYS-JUST     TO MOD-IDANALYS-JUST                    
062210       MOVE DCS-KDRT-JUST         TO MOD-KDRT-JUST                        
062300       MOVE DCS-IDKST-SKROT       TO MOD-IDKST-SKROT                      
062400       MOVE DCS-IDKONTO-SKROT     TO MOD-IDKONTO-SKROT                    
062500       MOVE DCS-IDANALYS-SKROT    TO MOD-IDANALYS-SKROT                   
062510       MOVE DCS-KDRT-SKROT        TO MOD-KDRT-SKROT                       
062600       MOVE DCS-IDKST-MIX         TO MOD-IDKST-MIX                        
062700       MOVE DCS-IDKONTO-MIX       TO MOD-IDKONTO-MIX                      
062800       MOVE DCS-IDANALYS-MIX      TO MOD-IDANALYS-MIX                     
062810       MOVE DCS-KDRT-MIX          TO MOD-KDRT-MIX                         
062900       MOVE DCS-IDDISTR-SKROT     TO MOD-IDDISTR-SKROT                    
063000       MOVE DCS-IDDISTR-QSKROT    TO MOD-IDDISTR-QSKROT                   
063100       MOVE DCS-IDDISTR-RSKROT    TO MOD-IDDISTR-RSKROT                   
063200       MOVE DCS-IDDISTR-MIX       TO MOD-IDDISTR-MIX                      
063300       MOVE DCS-IDDISTR-JUST      TO MOD-IDDISTR-JUST                     
063400       MOVE DCS-IDKUNDNR-SKROT    TO MOD-IDKUNDNR-SKROT                   
063500       MOVE DCS-IDKUNDNR-QSKROT   TO MOD-IDKUNDNR-QSKROT                  
063600       MOVE DCS-IDKUNDNR-RSKROT   TO MOD-IDKUNDNR-RSKROT                  
063700       MOVE DCS-IDKUNDNR-MIX      TO MOD-IDKUNDNR-MIX                     
063800       MOVE DCS-IDKUNDNR-JUST     TO MOD-IDKUNDNR-JUST                    
063900       IF W-IDDC-REF = WC-CDC-SE                                          
064000          PERFORM FA-VISA-CDC-INFO                                        
064100       ELSE                                                               
064200          PERFORM FB-VISA-REF-INFO                                        
064300       END-IF                                                             
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700 FA-VISA-CDC-INFO SECTION.                                                
064800                                                                          
064900       MOVE DCS-IDDISTR-REFILL    TO MOD-IDDISTR-REFILL                   
065000       MOVE DCS-IDDISTR-RETUR     TO MOD-IDDISTR-RETUR                    
065100       MOVE DCS-IDDISTR-QRETUR    TO MOD-IDDISTR-QRETUR                   
065200       MOVE DCS-IDKUNDNR-BPS      TO MOD-IDKUNDNR-BPS                     
065300       MOVE DCS-IDKUNDNR-SBPS     TO MOD-IDKUNDNR-SBPS                    
065400       MOVE DCS-IDKUNDNR-RETUR    TO MOD-IDKUNDNR-RETUR                   
065500       MOVE DCS-IDKUNDNR-QRETUR   TO MOD-IDKUNDNR-QRETUR                  
065600       MOVE DCS-IDKUNDNR-SRETUR   TO MOD-IDKUNDNR-SRETUR                  
065700       MOVE DCS-IDKUNDNR-TRETUR   TO MOD-IDKUNDNR-TRETUR                  
065800       MOVE DCS-IDKUNDNR-SORD     TO MOD-IDKUNDNR-SORD                    
065900       MOVE DCS-IDKUNDNR-SQRET    TO MOD-IDKUNDNR-SQRET                   
066000     .                                                                    
066100     EJECT                                                                
066200 FB-VISA-REF-INFO SECTION.                                                
066300                                                                          
066400     PERFORM IMS-GU-WDB616                                                
066500     IF SEGMENT-FINNS                                                     
066600        MOVE REF-IDDISTR-REFILL  TO MOD-IDDISTR-REFILL                    
066700        MOVE REF-IDDISTR-RETUR   TO MOD-IDDISTR-RETUR                     
066800        MOVE REF-IDDISTR-QRETUR  TO MOD-IDDISTR-QRETUR                    
066900        MOVE REF-IDKUNDNR-BPS    TO MOD-IDKUNDNR-BPS                      
067000        MOVE REF-IDKUNDNR-SBPS   TO MOD-IDKUNDNR-SBPS                     
067100        MOVE REF-IDKUNDNR-RETUR  TO MOD-IDKUNDNR-RETUR                    
067200        MOVE REF-IDKUNDNR-QRETUR TO MOD-IDKUNDNR-QRETUR                   
067300        MOVE REF-IDKUNDNR-SRETUR TO MOD-IDKUNDNR-SRETUR                   
067400        MOVE REF-IDKUNDNR-TRETUR TO MOD-IDKUNDNR-TRETUR                   
067500        MOVE REF-IDKUNDNR-SORD   TO MOD-IDKUNDNR-SORD                     
067600        MOVE REF-IDKUNDNR-SQRET  TO MOD-IDKUNDNR-SQRET                    
067700     ELSE                                                                 
067800        MOVE ZERO                TO MOD-IDDISTR-REFILL                    
067900                                    MOD-IDDISTR-RETUR                     
068000                                    MOD-IDDISTR-QRETUR                    
068100                                    MOD-IDKUNDNR-BPS                      
068200                                    MOD-IDKUNDNR-SBPS                     
068300                                    MOD-IDKUNDNR-RETUR                    
068400                                    MOD-IDKUNDNR-QRETUR                   
068500                                    MOD-IDKUNDNR-SRETUR                   
068600                                    MOD-IDKUNDNR-TRETUR                   
068700                                    MOD-IDKUNDNR-SORD                     
068800                                    MOD-IDKUNDNR-SQRET                    
068900     END-IF                                                               
070500     .                                                                    
070600     EJECT                                                                
070700 G-KOLLA-INPUT SECTION.                                                   
070800                                                                          
070900     MOVE JA  TO INDATA-SW                                                
071010     IF  MID-INPUT    = ALL '+'                                           
071020     AND MID-FLTABORT = ALL '+'                                           
071100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
071200       CALL WMEDKONV USING MED-WMEDAREA                                   
071300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
071400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
071500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
071600       MOVE NEJ TO INDATA-SW                                              
071700     ELSE                                                                 
071800       PERFORM IMS-GU-WDB601                                              
071900       IF SEGMENT-FINNS                                                   
072000         PERFORM GA-KOLLA-ANDR-SEGM                                       
072100       ELSE                                                               
072200         MOVE ERR-DC-MISSING  TO MED-IDMFSFEL                             
072300         MOVE NEJ TO INDATA-SW                                            
072400       END-IF                                                             
072500                                                                          
072600                                                                          
072700       IF INDATA-FEL                                                      
072800         IF MED-IDMFSFEL = SPACE                                          
072900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
073000         END-IF                                                           
073100         CALL WMEDKONV USING MED-WMEDAREA                                 
073200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
073300         PERFORM MFS-ROER-EJ-FAELT-UT                                     
073400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
073500       END-IF                                                             
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 GA-KOLLA-ANDR-SEGM SECTION.                                              
073910                                                                          
073920     IF MID-FLTABORT       NOT = ALL '+'                                  
073934       IF MID-FLTABORT   = 'J' OR 'Y'                                     
073935          IF MID-FLTABORT   = 'Y'                                         
073936             MOVE 'J'              TO MID-FLTABORT                        
073937          END-IF                                                          
073938          IF MID-FLTABORT   = 'J'                                         
073939             MOVE WS-SPAR-DC       TO W-IDDC                              
073940             MOVE WS-SPAR-REF-DC   TO W-IDDC-REF                          
073941             PERFORM IMS-GU-WDB616                                        
073942             IF SEGMENT-FINNS                                             
073945                MOVE WS-SPAR-DC       TO W-IDDC-B1-MIN                    
073946                                         W-IDDC-B1-MAX                    
073947                MOVE WS-SPAR-REF-DC   TO W-IDDC-REF-B1-MIN                
073948                                         W-IDDC-REF-B1-MAX                
073949                PERFORM IMS-GN-WDK7B                                      
073950                IF SEGMENT-SAKNAS                                         
073951                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTABORT-ATTR         
073952                ELSE                                                      
073953                   MOVE ERR-UPD-NOT-ALLOWED  TO MED-IDMFSFEL              
073954                   MOVE MID-FLTABORT         TO MOD-FLTABORT              
073957                   MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTABORT-ATTR         
073958                   MOVE NEJ TO INDATA-SW                                  
073959                END-IF                                                    
073960             ELSE                                                         
073961                MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                  
073962                MOVE MID-FLTABORT        TO MOD-FLTABORT                  
073963                MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLTABORT-ATTR             
073964                MOVE NEJ TO INDATA-SW                                     
073965             END-IF                                                       
073966          END-IF                                                          
073968       ELSE                                                               
073969          MOVE MID-FLTABORT        TO MOD-FLTABORT                        
073970          MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLTABORT-ATTR                   
073980          MOVE NEJ TO INDATA-SW                                           
073990       END-IF                                                             
073991     ELSE                                                                 
073992        IF WS-SPAR-DC = WS-SPAR-REF-DC                                    
073998           MOVE ERR-UPD-NOT-ALLOWED  TO MED-IDMFSFEL                      
074001           MOVE NEJ TO INDATA-SW                                          
074002        END-IF                                                            
074003     END-IF                                                               
074004                                                                          
074005     IF MID-KDFRAKT-BPS-IN NOT = ALL '+'                                  
074006       IF MID-KDFRAKT-BPS-IN NUMERIC AND                                  
074007          MID-KDFRAKT-BPS-IN > ZERO                                       
074008         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-BPS-IN-ATTR              
074009       ELSE                                                               
074010         MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-BPS-IN-ATTR                
074011         MOVE NEJ TO INDATA-SW                                            
074012       END-IF                                                             
074020     END-IF                                                               
075000                                                                          
075100     IF MID-KDFRAKT-SBPS-IN NOT = ALL '+'                                 
075200       IF MID-KDFRAKT-SBPS-IN NUMERIC AND                                 
075300          MID-KDFRAKT-SBPS-IN > ZERO                                      
075400         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-SBPS-IN-ATTR             
075500       ELSE                                                               
075600         MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-SBPS-IN-ATTR               
075700         MOVE NEJ TO INDATA-SW                                            
075800       END-IF                                                             
075900     END-IF                                                               
076000                                                                          
076100     IF MID-IDDISTR-REFILL-IN NOT = ALL '+'                               
076200       IF MID-IDDISTR-REFILL-IN NUMERIC AND                               
076300          MID-IDDISTR-REFILL-IN > ZERO                                    
076400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-REFILL-IN-ATTR           
076500       ELSE                                                               
076600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-REFILL-IN-ATTR             
076700         MOVE NEJ TO INDATA-SW                                            
076800       END-IF                                                             
076900     END-IF                                                               
077000                                                                          
077100     IF MID-IDDISTR-RETUR-IN NOT = ALL '+'                                
077200       IF MID-IDDISTR-RETUR-IN NUMERIC AND                                
077300          MID-IDDISTR-RETUR-IN > ZERO                                     
077400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-RETUR-IN-ATTR            
077500       ELSE                                                               
077600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-RETUR-IN-ATTR              
077700         MOVE NEJ TO INDATA-SW                                            
077800       END-IF                                                             
077900     END-IF                                                               
078000                                                                          
078100     IF MID-IDDISTR-QRETUR-IN NOT = ALL '+'                               
078200       IF MID-IDDISTR-QRETUR-IN NUMERIC AND                               
078300          MID-IDDISTR-QRETUR-IN > ZERO                                    
078400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-QRETUR-IN-ATTR           
078500       ELSE                                                               
078600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-QRETUR-IN-ATTR             
078700         MOVE NEJ TO INDATA-SW                                            
078800       END-IF                                                             
078900     END-IF                                                               
079000                                                                          
079100     IF MID-IDDISTR-SKROT-IN NOT = ALL '+'                                
079200       IF MID-IDDISTR-SKROT-IN NUMERIC AND                                
079300          MID-IDDISTR-SKROT-IN > ZERO                                     
079400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-SKROT-IN-ATTR            
079500       ELSE                                                               
079600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-SKROT-IN-ATTR              
079700         MOVE NEJ TO INDATA-SW                                            
079800       END-IF                                                             
079900     END-IF                                                               
080000                                                                          
080100     IF MID-IDDISTR-QSKROT-IN NOT = ALL '+'                               
080200       IF MID-IDDISTR-QSKROT-IN NUMERIC AND                               
080300          MID-IDDISTR-QSKROT-IN > ZERO                                    
080400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-QSKROT-IN-ATTR           
080500       ELSE                                                               
080600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-QSKROT-IN-ATTR             
080700         MOVE NEJ TO INDATA-SW                                            
080800       END-IF                                                             
080900     END-IF                                                               
081000                                                                          
081100     IF MID-IDDISTR-RSKROT-IN NOT = ALL '+'                               
081200       IF MID-IDDISTR-RSKROT-IN NUMERIC AND                               
081300          MID-IDDISTR-RSKROT-IN > ZERO                                    
081400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-RSKROT-IN-ATTR           
081500       ELSE                                                               
081600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-RSKROT-IN-ATTR             
081700         MOVE NEJ TO INDATA-SW                                            
081800       END-IF                                                             
081900     END-IF                                                               
082000                                                                          
082100     IF MID-IDDISTR-MIX-IN NOT = ALL '+'                                  
082200       IF MID-IDDISTR-MIX-IN NUMERIC AND                                  
082300          MID-IDDISTR-MIX-IN > ZERO                                       
082400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-MIX-IN-ATTR              
082500       ELSE                                                               
082600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-MIX-IN-ATTR                
082700         MOVE NEJ TO INDATA-SW                                            
082800       END-IF                                                             
082900     END-IF                                                               
083000                                                                          
083100     IF MID-IDDISTR-JUST-IN NOT = ALL '+'                                 
083200       IF MID-IDDISTR-JUST-IN NUMERIC AND                                 
083300          MID-IDDISTR-JUST-IN > ZERO                                      
083400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-JUST-IN-ATTR             
083500       ELSE                                                               
083600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-JUST-IN-ATTR               
083700         MOVE NEJ TO INDATA-SW                                            
083800       END-IF                                                             
083900     END-IF                                                               
084000                                                                          
084100     IF MID-IDKUNDNR-SORD-IN NOT = ALL '+'                                
084200       IF MID-IDKUNDNR-SORD-IN NUMERIC AND                                
084300          MID-IDKUNDNR-SORD-IN > ZERO                                     
084400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-SORD-IN-ATTR            
084500       ELSE                                                               
084600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-SORD-IN-ATTR              
084700         MOVE NEJ TO INDATA-SW                                            
084800       END-IF                                                             
084900     END-IF                                                               
085000                                                                          
085100     IF MID-IDKUNDNR-RETUR-IN NOT = ALL '+'                               
085200       IF MID-IDKUNDNR-RETUR-IN NUMERIC AND                               
085300          MID-IDKUNDNR-RETUR-IN > ZERO                                    
085400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-RETUR-IN-ATTR           
085500       ELSE                                                               
085600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-RETUR-IN-ATTR             
085700         MOVE NEJ TO INDATA-SW                                            
085800       END-IF                                                             
085900     END-IF                                                               
086000                                                                          
086100     IF MID-IDKUNDNR-QRETUR-IN NOT = ALL '+'                              
086200       IF MID-IDKUNDNR-QRETUR-IN NUMERIC AND                              
086300          MID-IDKUNDNR-QRETUR-IN > ZERO                                   
086400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-QRETUR-IN-ATTR          
086500       ELSE                                                               
086600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-QRETUR-IN-ATTR            
086700         MOVE NEJ TO INDATA-SW                                            
086800       END-IF                                                             
086900     END-IF                                                               
087000                                                                          
087100     IF MID-IDKUNDNR-SKROT-IN NOT = ALL '+'                               
087200       IF MID-IDKUNDNR-SKROT-IN NUMERIC AND                               
087300          MID-IDKUNDNR-SKROT-IN > ZERO                                    
087400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-SKROT-IN-ATTR           
087500       ELSE                                                               
087600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-SKROT-IN-ATTR             
087700         MOVE NEJ TO INDATA-SW                                            
087800       END-IF                                                             
087900     END-IF                                                               
088000                                                                          
088100     IF MID-IDKUNDNR-QSKROT-IN NOT = ALL '+'                              
088200       IF MID-IDKUNDNR-QSKROT-IN NUMERIC AND                              
088300          MID-IDKUNDNR-QSKROT-IN > ZERO                                   
088400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-QSKROT-IN-ATTR          
088500       ELSE                                                               
088600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-QSKROT-IN-ATTR            
088700         MOVE NEJ TO INDATA-SW                                            
088800       END-IF                                                             
088900     END-IF                                                               
089000                                                                          
089100     IF MID-IDKUNDNR-RSKROT-IN NOT = ALL '+'                              
089200       IF MID-IDKUNDNR-RSKROT-IN NUMERIC AND                              
089300          MID-IDKUNDNR-RSKROT-IN > ZERO                                   
089400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-RSKROT-IN-ATTR          
089500       ELSE                                                               
089600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-RSKROT-IN-ATTR            
089700         MOVE NEJ TO INDATA-SW                                            
089800       END-IF                                                             
089900     END-IF                                                               
090000                                                                          
090100     IF MID-IDKUNDNR-MIX-IN NOT = ALL '+'                                 
090200       IF MID-IDKUNDNR-MIX-IN NUMERIC AND                                 
090300          MID-IDKUNDNR-MIX-IN > ZERO                                      
090400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-MIX-IN-ATTR             
090500       ELSE                                                               
090600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-MIX-IN-ATTR               
090700         MOVE NEJ TO INDATA-SW                                            
090800       END-IF                                                             
090900     END-IF                                                               
091000                                                                          
091100     IF MID-IDKUNDNR-JUST-IN NOT = ALL '+'                                
091200       IF MID-IDKUNDNR-JUST-IN NUMERIC AND                                
091300          MID-IDKUNDNR-JUST-IN > ZERO                                     
091400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-JUST-IN-ATTR            
091500       ELSE                                                               
091600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-JUST-IN-ATTR              
091700         MOVE NEJ TO INDATA-SW                                            
091800       END-IF                                                             
091900     END-IF                                                               
092000                                                                          
092100     IF MID-IDKUNDNR-BPS-IN NOT = ALL '+'                                 
092200       IF MID-IDKUNDNR-BPS-IN NUMERIC AND                                 
092300          MID-IDKUNDNR-BPS-IN > ZERO                                      
092400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-BPS-IN-ATTR             
092500       ELSE                                                               
092600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-BPS-IN-ATTR               
092700         MOVE NEJ TO INDATA-SW                                            
092800       END-IF                                                             
092900     END-IF                                                               
093000                                                                          
093100     IF MID-IDKUNDNR-TRETUR-IN NOT = ALL '+'                              
093200       IF MID-IDKUNDNR-TRETUR-IN NUMERIC AND                              
093300          MID-IDKUNDNR-TRETUR-IN > ZERO                                   
093400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TRETUR-IN-ATTR          
093500       ELSE                                                               
093600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-TRETUR-IN-ATTR            
093700         MOVE NEJ TO INDATA-SW                                            
093800       END-IF                                                             
093900     END-IF                                                               
094000                                                                          
094100     IF MID-IDKUNDNR-SQRET-IN NOT = ALL '+'                               
094200       IF MID-IDKUNDNR-SQRET-IN NUMERIC AND                               
094300          MID-IDKUNDNR-SQRET-IN > ZERO                                    
094400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-SQRET-IN-ATTR           
094500       ELSE                                                               
094600         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-SQRET-IN-ATTR           
094700         MOVE NEJ TO INDATA-SW                                            
094800       END-IF                                                             
094900     END-IF                                                               
095000                                                                          
095100     IF MID-IDKUNDNR-SBPS-IN NOT = ALL '+'                                
095200       IF MID-IDKUNDNR-SBPS-IN NUMERIC AND                                
095300          MID-IDKUNDNR-SBPS-IN > ZERO                                     
095400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-SBPS-IN-ATTR            
095500       ELSE                                                               
095600         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-SBPS-IN-ATTR            
095700         MOVE NEJ TO INDATA-SW                                            
095800       END-IF                                                             
095900     END-IF                                                               
096000                                                                          
096100     IF MID-IDKUNDNR-SRETUR-IN NOT = ALL '+'                              
096200       IF MID-IDKUNDNR-SRETUR-IN NUMERIC AND                              
096300          MID-IDKUNDNR-SRETUR-IN > ZERO                                   
096400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-SRETUR-IN-ATTR          
096500       ELSE                                                               
096600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-SRETUR-IN-ATTR            
096700         MOVE NEJ TO INDATA-SW                                            
096800       END-IF                                                             
096900     END-IF                                                               
097000                                                                          
097100     IF MID-IDPERSON-REM-IN NOT = ALL '+'                                 
097200       IF MID-IDPERSON-REM-IN NUMERIC AND                                 
097300          MID-IDPERSON-REM-IN > ZERO                                      
097400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPERSON-REM-IN-ATTR             
097500       ELSE                                                               
097600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-REM-IN-ATTR               
097700         MOVE NEJ TO INDATA-SW                                            
097800       END-IF                                                             
097900     END-IF                                                               
098000                                                                          
098100     IF MID-IDKST-JUST-IN NOT = ALL '+'                                   
098300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKST-JUST-IN-ATTR               
098800     END-IF                                                               
098900                                                                          
099000     IF MID-IDKONTO-JUST-IN NOT = ALL '+'                                 
099100       IF MID-IDKONTO-JUST-IN NUMERIC                                     
099200         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-JUST-IN-ATTR             
099300       ELSE                                                               
099400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-JUST-IN-ATTR               
099500         MOVE NEJ TO INDATA-SW                                            
099600       END-IF                                                             
099700     END-IF                                                               
099800                                                                          
099900     IF MID-IDANALYS-JUST-IN NOT = ALL '+'                                
100000        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANALYS-JUST-IN-ATTR             
100100     END-IF                                                               
100200                                                                          
100210     IF MID-KDRT-JUST-IN NOT = ALL '+'                                    
100211       IF MID-KDRT-JUST-IN NUMERIC                                        
100220         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDRT-JUST-IN-ATTR                
100221       ELSE                                                               
100222         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDRT-JUST-IN-ATTR                
100223         MOVE NEJ TO INDATA-SW                                            
100224       END-IF                                                             
100230     END-IF                                                               
100240                                                                          
100300     IF MID-IDKST-SKROT-IN NOT = ALL '+'                                  
100500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKST-SKROT-IN-ATTR              
101000     END-IF                                                               
101100                                                                          
101200     IF MID-IDKONTO-SKROT-IN NOT = ALL '+'                                
101300       IF MID-IDKONTO-SKROT-IN NUMERIC                                    
101400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-SKROT-IN-ATTR            
101500       ELSE                                                               
101600         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-SKROT-IN-ATTR            
101700         MOVE NEJ TO INDATA-SW                                            
101800       END-IF                                                             
101900     END-IF                                                               
102000                                                                          
102100     IF MID-IDANALYS-SKROT-IN NOT = ALL '+'                               
102200        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANALYS-SKROT-IN-ATTR            
102300     END-IF                                                               
102400                                                                          
102410     IF MID-KDRT-SKROT-IN NOT = ALL '+'                                   
102411       IF MID-KDRT-SKROT-IN NUMERIC                                       
102420         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDRT-SKROT-IN-ATTR               
102421       ELSE                                                               
102422         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDRT-SKROT-IN-ATTR               
102423         MOVE NEJ TO INDATA-SW                                            
102424       END-IF                                                             
102430     END-IF                                                               
102440                                                                          
102500     IF MID-IDKST-MIX-IN NOT = ALL '+'                                    
102700         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKST-MIX-IN-ATTR                
103200     END-IF                                                               
103300                                                                          
103400     IF MID-IDKONTO-MIX-IN NOT = ALL '+'                                  
103500       IF MID-IDKONTO-MIX-IN NUMERIC                                      
103600         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-MIX-IN-ATTR              
103700       ELSE                                                               
103800         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-MIX-IN-ATTR              
103900         MOVE NEJ TO INDATA-SW                                            
104000       END-IF                                                             
104100     END-IF                                                               
104200                                                                          
104300     IF MID-IDANALYS-MIX-IN NOT = ALL '+'                                 
104400        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANALYS-MIX-IN-ATTR              
104500     END-IF                                                               
104600                                                                          
104610     IF MID-KDRT-MIX-IN NOT = ALL '+'                                     
104611       IF MID-KDRT-MIX-IN NUMERIC                                         
104620         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDRT-MIX-IN-ATTR                 
104621       ELSE                                                               
104622         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDRT-MIX-IN-ATTR                 
104623         MOVE NEJ TO INDATA-SW                                            
104624       END-IF                                                             
104630     END-IF                                                               
109200     .                                                                    
109300     EJECT                                                                
109400 H-UPPDATERA SECTION.                                                     
109500                                                                          
109600     MOVE MSGI-IDDC-KEY    TO W-IDDC                                      
109610*TO ADD 2203 ENTRY IN WDG3 DB ,WHEN SETTING UP A NEW REFILL               
109620*FLOW TO CDC                                                              
109623     IF MSGI-IDDC-KEY  = WC-CDC-SE                                        
109624        MOVE MSGI-IDDC-SEND      TO W-IDDC-2203                           
109625        PERFORM IMS-GU-WDG301                                             
109626        IF SEGMENT-SAKNAS                                                 
109627           MOVE W-WDG301-2203-X  TO WDG3KEY                               
109629           PERFORM IMS-ISRT-WDG301                                        
109630        END-IF                                                            
109631     END-IF                                                               
109640*                                                                         
109700     PERFORM IMS-GHU-WDB601                                               
109800                                                                          
109900     IF MID-FLTABORT NOT = ALL '+'                                        
110000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTABORT-ATTR                    
110100       MOVE 'REMOVE WDB616'   TO DCSL-BETEXT-ITEM                         
110200       MOVE 'N'               TO DCSL-BETEXT-OLDDATA                      
110300       MOVE MID-FLTABORT      TO DCSL-BETEXT-NEWDATA                      
110500       ADD +1                 TO FIL-IDSEKVNR                             
110600       PERFORM IMS-ISRT-WDR601                                            
110700     END-IF                                                               
110800                                                                          
110810     IF MID-KDFRAKT-BPS-IN NOT = ALL '+'                                  
110820       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFRAKT-BPS-IN-ATTR              
110830       MOVE 'KDFRAKT-BPS' TO DCSL-BETEXT-ITEM                             
110840       MOVE DCS-KDFRAKT-BPS    TO DCSL-BETEXT-OLDDATA                     
110850       MOVE MID-KDFRAKT-BPS-IN TO DCS-KDFRAKT-BPS                         
110860                                  DCSL-BETEXT-NEWDATA                     
110870       ADD +1                 TO FIL-IDSEKVNR                             
110880       PERFORM IMS-ISRT-WDR601                                            
110890     END-IF                                                               
110891                                                                          
110900     IF MID-KDFRAKT-SBPS-IN NOT = ALL '+'                                 
111000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFRAKT-SBPS-IN-ATTR             
111100       MOVE 'KDFRAKT-SBPS' TO DCSL-BETEXT-ITEM                            
111200       MOVE DCS-KDFRAKT-SBPS    TO DCSL-BETEXT-OLDDATA                    
111300       MOVE MID-KDFRAKT-SBPS-IN TO DCS-KDFRAKT-SBPS                       
111400                                   DCSL-BETEXT-NEWDATA                    
111500       ADD +1                 TO FIL-IDSEKVNR                             
111600       PERFORM IMS-ISRT-WDR601                                            
111700     END-IF                                                               
111800                                                                          
111900     IF MID-IDDISTR-REFILL-IN NOT = ALL '+'                               
112000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-REFILL-IN-ATTR           
112100       MOVE 'IDDISTR-REFILL' TO DCSL-BETEXT-ITEM                          
112200       MOVE DCS-IDDISTR-REFILL    TO DCSL-BETEXT-OLDDATA                  
112300       IF SPAR-IDDC-REF = WC-CDC-SE                                       
112400          MOVE MID-IDDISTR-REFILL-IN TO DCS-IDDISTR-REFILL                
112500       END-IF                                                             
112600       MOVE MID-IDDISTR-REFILL-IN TO DCSL-BETEXT-NEWDATA                  
112700       ADD +1                 TO FIL-IDSEKVNR                             
112800       PERFORM IMS-ISRT-WDR601                                            
112900     END-IF                                                               
113000                                                                          
113100     IF MID-IDDISTR-RETUR-IN NOT = ALL '+'                                
113200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-RETUR-IN-ATTR            
113300       MOVE 'IDDISTR-RETUR' TO DCSL-BETEXT-ITEM                           
113400       MOVE DCS-IDDISTR-RETUR    TO DCSL-BETEXT-OLDDATA                   
113500       IF SPAR-IDDC-REF = WC-CDC-SE                                       
113600          MOVE MID-IDDISTR-RETUR-IN TO DCS-IDDISTR-RETUR                  
113700       END-IF                                                             
113800       MOVE MID-IDDISTR-RETUR-IN TO DCSL-BETEXT-NEWDATA                   
113900       ADD +1                 TO FIL-IDSEKVNR                             
114000       PERFORM IMS-ISRT-WDR601                                            
114100     END-IF                                                               
114200                                                                          
114300     IF MID-IDDISTR-QRETUR-IN NOT = ALL '+'                               
114400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-QRETUR-IN-ATTR           
114500       MOVE 'IDDISTR-QRETUR' TO DCSL-BETEXT-ITEM                          
114600       MOVE DCS-IDDISTR-QRETUR    TO DCSL-BETEXT-OLDDATA                  
114700       IF SPAR-IDDC-REF = WC-CDC-SE                                       
114800          MOVE MID-IDDISTR-QRETUR-IN TO DCS-IDDISTR-QRETUR                
114900       END-IF                                                             
115000       MOVE MID-IDDISTR-QRETUR-IN    TO DCSL-BETEXT-NEWDATA               
115100       ADD +1                 TO FIL-IDSEKVNR                             
115200       PERFORM IMS-ISRT-WDR601                                            
115300     END-IF                                                               
115400                                                                          
115500     IF MID-IDDISTR-SKROT-IN NOT = ALL '+'                                
115600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-SKROT-IN-ATTR            
115700       MOVE 'IDDISTR-SKROT'      TO DCSL-BETEXT-ITEM                      
115800       MOVE DCS-IDDISTR-SKROT    TO DCSL-BETEXT-OLDDATA                   
115900       MOVE MID-IDDISTR-SKROT-IN TO DCS-IDDISTR-SKROT                     
116000                                 DCSL-BETEXT-NEWDATA                      
116100       ADD +1                 TO FIL-IDSEKVNR                             
116200       PERFORM IMS-ISRT-WDR601                                            
116300     END-IF                                                               
116400                                                                          
116500     IF MID-IDDISTR-QSKROT-IN NOT = ALL '+'                               
116600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-QSKROT-IN-ATTR           
116700       MOVE 'IDDISTR-QSKROT'  TO DCSL-BETEXT-ITEM                         
116800       MOVE DCS-IDDISTR-QSKROT    TO DCSL-BETEXT-OLDDATA                  
116900       MOVE MID-IDDISTR-QSKROT-IN TO DCS-IDDISTR-QSKROT                   
117000                                 DCSL-BETEXT-NEWDATA                      
117100       ADD +1                 TO FIL-IDSEKVNR                             
117200       PERFORM IMS-ISRT-WDR601                                            
117300     END-IF                                                               
117400                                                                          
117500     IF MID-IDDISTR-RSKROT-IN NOT = ALL '+'                               
117600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-RSKROT-IN-ATTR           
117700       MOVE 'IDDISTR-RSKROT'  TO DCSL-BETEXT-ITEM                         
117800       MOVE DCS-IDDISTR-RSKROT    TO DCSL-BETEXT-OLDDATA                  
117900       MOVE MID-IDDISTR-RSKROT-IN TO DCS-IDDISTR-RSKROT                   
118000                                 DCSL-BETEXT-NEWDATA                      
118100       ADD +1                 TO FIL-IDSEKVNR                             
118200       PERFORM IMS-ISRT-WDR601                                            
118300     END-IF                                                               
118400                                                                          
118500     IF MID-IDDISTR-MIX-IN NOT = ALL '+'                                  
118600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-MIX-IN-ATTR              
118700       MOVE 'IDDISTR-MIX'      TO DCSL-BETEXT-ITEM                        
118800       MOVE DCS-IDDISTR-MIX    TO DCSL-BETEXT-OLDDATA                     
118900       MOVE MID-IDDISTR-MIX-IN TO DCS-IDDISTR-MIX                         
119000                                  DCSL-BETEXT-NEWDATA                     
119100       ADD +1                  TO FIL-IDSEKVNR                            
119200       PERFORM IMS-ISRT-WDR601                                            
119300     END-IF                                                               
119400                                                                          
119500     IF MID-IDDISTR-JUST-IN NOT = ALL '+'                                 
119600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-JUST-IN-ATTR             
119700       MOVE 'IDDISTR-JUST'      TO DCSL-BETEXT-ITEM                       
119800       MOVE DCS-IDDISTR-JUST    TO DCSL-BETEXT-OLDDATA                    
119900       MOVE MID-IDDISTR-JUST-IN TO DCS-IDDISTR-JUST                       
120000                                 DCSL-BETEXT-NEWDATA                      
120100       ADD +1                 TO FIL-IDSEKVNR                             
120200       PERFORM IMS-ISRT-WDR601                                            
120300     END-IF                                                               
120400                                                                          
120500     IF MID-IDKUNDNR-SORD-IN NOT = ALL '+'                                
120600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-SORD-IN-ATTR            
120700       MOVE 'IDKUNDNR-SORD' TO DCSL-BETEXT-ITEM                           
120800       MOVE DCS-IDKUNDNR-SORD    TO DCSL-BETEXT-OLDDATA                   
120900       IF SPAR-IDDC-REF = WC-CDC-SE                                       
121000          MOVE MID-IDKUNDNR-SORD-IN TO DCS-IDKUNDNR-SORD                  
121100       END-IF                                                             
121200       MOVE MID-IDKUNDNR-SORD-IN    TO DCSL-BETEXT-NEWDATA                
121300       ADD +1                 TO FIL-IDSEKVNR                             
121400       PERFORM IMS-ISRT-WDR601                                            
121500     END-IF                                                               
121600                                                                          
121700     IF MID-IDKUNDNR-RETUR-IN NOT = ALL '+'                               
121800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-RETUR-IN-ATTR           
121900       MOVE 'IDKUNDNR-RETUR' TO DCSL-BETEXT-ITEM                          
122000       MOVE DCS-IDKUNDNR-RETUR    TO DCSL-BETEXT-OLDDATA                  
122100       IF SPAR-IDDC-REF = WC-CDC-SE                                       
122200          MOVE MID-IDKUNDNR-RETUR-IN TO DCS-IDKUNDNR-RETUR                
122300       END-IF                                                             
122400       MOVE MID-IDKUNDNR-RETUR-IN    TO DCSL-BETEXT-NEWDATA               
122500       ADD +1                 TO FIL-IDSEKVNR                             
122600       PERFORM IMS-ISRT-WDR601                                            
122700     END-IF                                                               
122800                                                                          
122900     IF MID-IDKUNDNR-QRETUR-IN NOT = ALL '+'                              
123000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-QRETUR-IN-ATTR          
123100       MOVE 'IDKUNDNR-QRETUR' TO DCSL-BETEXT-ITEM                         
123200       MOVE DCS-IDKUNDNR-QRETUR    TO DCSL-BETEXT-OLDDATA                 
123300       IF SPAR-IDDC-REF = WC-CDC-SE                                       
123400          MOVE MID-IDKUNDNR-QRETUR-IN TO DCS-IDKUNDNR-QRETUR              
123500       END-IF                                                             
123600       MOVE MID-IDKUNDNR-QRETUR-IN    TO DCSL-BETEXT-NEWDATA              
123700       ADD +1                 TO FIL-IDSEKVNR                             
123800       PERFORM IMS-ISRT-WDR601                                            
123900     END-IF                                                               
124000                                                                          
124100     IF MID-IDKUNDNR-SKROT-IN NOT = ALL '+'                               
124200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-SKROT-IN-ATTR           
124300       MOVE 'IDKUNDNR-SKROT'      TO DCSL-BETEXT-ITEM                     
124400       MOVE DCS-IDKUNDNR-SKROT    TO DCSL-BETEXT-OLDDATA                  
124500       MOVE MID-IDKUNDNR-SKROT-IN TO DCS-IDKUNDNR-SKROT                   
124600                                     DCSL-BETEXT-NEWDATA                  
124700       ADD +1                 TO FIL-IDSEKVNR                             
124800       PERFORM IMS-ISRT-WDR601                                            
124900     END-IF                                                               
125000                                                                          
125100     IF MID-IDKUNDNR-QSKROT-IN NOT = ALL '+'                              
125200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-QSKROT-IN-ATTR          
125300       MOVE 'IDKUNDNR-QSKROT' TO DCSL-BETEXT-ITEM                         
125400       MOVE DCS-IDKUNDNR-QSKROT    TO DCSL-BETEXT-OLDDATA                 
125500       MOVE MID-IDKUNDNR-QSKROT-IN TO DCS-IDKUNDNR-QSKROT                 
125600                                 DCSL-BETEXT-NEWDATA                      
125700       ADD +1                 TO FIL-IDSEKVNR                             
125800       PERFORM IMS-ISRT-WDR601                                            
125900     END-IF                                                               
126000                                                                          
126100     IF MID-IDKUNDNR-RSKROT-IN NOT = ALL '+'                              
126200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-RSKROT-IN-ATTR          
126300       MOVE 'IDKUNDNR-RSKROT' TO DCSL-BETEXT-ITEM                         
126400       MOVE DCS-IDKUNDNR-RSKROT    TO DCSL-BETEXT-OLDDATA                 
126500       MOVE MID-IDKUNDNR-RSKROT-IN TO DCS-IDKUNDNR-RSKROT                 
126600                                 DCSL-BETEXT-NEWDATA                      
126700       ADD +1                 TO FIL-IDSEKVNR                             
126800       PERFORM IMS-ISRT-WDR601                                            
126900     END-IF                                                               
127000                                                                          
127100     IF MID-IDKUNDNR-MIX-IN NOT = ALL '+'                                 
127200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-MIX-IN-ATTR             
127300       MOVE 'IDKUNDNR-MIX'        TO DCSL-BETEXT-ITEM                     
127400       MOVE DCS-IDKUNDNR-MIX      TO DCSL-BETEXT-OLDDATA                  
127500       MOVE MID-IDKUNDNR-MIX-IN   TO DCS-IDKUNDNR-MIX                     
127600                                     DCSL-BETEXT-NEWDATA                  
127700       ADD +1                     TO FIL-IDSEKVNR                         
127800       PERFORM IMS-ISRT-WDR601                                            
127900     END-IF                                                               
128000                                                                          
128100     IF MID-IDKUNDNR-JUST-IN NOT = ALL '+'                                
128200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-JUST-IN-ATTR            
128300       MOVE 'IDKUNDNR-JUST'      TO DCSL-BETEXT-ITEM                      
128400       MOVE DCS-IDKUNDNR-JUST    TO DCSL-BETEXT-OLDDATA                   
128500       MOVE MID-IDKUNDNR-JUST-IN TO DCS-IDKUNDNR-JUST                     
128600                                    DCSL-BETEXT-NEWDATA                   
128700       ADD +1                    TO FIL-IDSEKVNR                          
128800       PERFORM IMS-ISRT-WDR601                                            
128900     END-IF                                                               
129000                                                                          
129100     IF MID-IDKUNDNR-BPS-IN NOT = ALL '+'                                 
129200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-BPS-IN-ATTR             
129300       MOVE 'IDKUNDNR-BPS' TO DCSL-BETEXT-ITEM                            
129400       MOVE DCS-IDKUNDNR-BPS    TO DCSL-BETEXT-OLDDATA                    
129500       IF SPAR-IDDC-REF = WC-CDC-SE                                       
129600          MOVE MID-IDKUNDNR-BPS-IN TO DCS-IDKUNDNR-BPS                    
129700       END-IF                                                             
129800       MOVE MID-IDKUNDNR-BPS-IN    TO DCSL-BETEXT-NEWDATA                 
129900       ADD +1                 TO FIL-IDSEKVNR                             
130000       PERFORM IMS-ISRT-WDR601                                            
130100     END-IF                                                               
130200                                                                          
130300     IF MID-IDKUNDNR-TRETUR-IN NOT = ALL '+'                              
130400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-TRETUR-IN-ATTR          
130500       MOVE 'IDKUNDNR-TRETUR' TO DCSL-BETEXT-ITEM                         
130600       MOVE DCS-IDKUNDNR-TRETUR    TO DCSL-BETEXT-OLDDATA                 
130700       IF SPAR-IDDC-REF = WC-CDC-SE                                       
130800          MOVE MID-IDKUNDNR-TRETUR-IN TO DCS-IDKUNDNR-TRETUR              
130900       END-IF                                                             
131000       MOVE MID-IDKUNDNR-TRETUR-IN    TO DCSL-BETEXT-NEWDATA              
131100       ADD +1                 TO FIL-IDSEKVNR                             
131200       PERFORM IMS-ISRT-WDR601                                            
131300     END-IF                                                               
131400                                                                          
131500     IF MID-IDKUNDNR-SQRET-IN NOT = ALL '+'                               
131600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-SQRET-IN-ATTR           
131700       MOVE 'IDKUNDNR-SQRET' TO DCSL-BETEXT-ITEM                          
131800       MOVE DCS-IDKUNDNR-SQRET    TO DCSL-BETEXT-OLDDATA                  
131900       MOVE MID-IDKUNDNR-SQRET-IN TO DCS-IDKUNDNR-SQRET                   
132000                                 DCSL-BETEXT-NEWDATA                      
132100       ADD +1                 TO FIL-IDSEKVNR                             
132200       PERFORM IMS-ISRT-WDR601                                            
132300     END-IF                                                               
132400                                                                          
132500     IF MID-IDKUNDNR-SBPS-IN NOT = ALL '+'                                
132600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-SBPS-IN-ATTR            
132700       MOVE 'IDKUNDNR-SBPS' TO DCSL-BETEXT-ITEM                           
132800       MOVE DCS-IDKUNDNR-SBPS    TO DCSL-BETEXT-OLDDATA                   
132900       IF SPAR-IDDC-REF = WC-CDC-SE                                       
133000          MOVE MID-IDKUNDNR-SBPS-IN TO DCS-IDKUNDNR-SBPS                  
133100       END-IF                                                             
133200       MOVE MID-IDKUNDNR-SBPS-IN    TO DCSL-BETEXT-NEWDATA                
133300       ADD +1                 TO FIL-IDSEKVNR                             
133400       PERFORM IMS-ISRT-WDR601                                            
133500     END-IF                                                               
133600                                                                          
133700     IF MID-IDKUNDNR-SRETUR-IN NOT = ALL '+'                              
133800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-SRETUR-IN-ATTR          
133900       MOVE 'IDKUNDNR-SRETUR' TO DCSL-BETEXT-ITEM                         
134000       MOVE DCS-IDKUNDNR-SRETUR    TO DCSL-BETEXT-OLDDATA                 
134100       IF SPAR-IDDC-REF = WC-CDC-SE                                       
134200          MOVE MID-IDKUNDNR-SRETUR-IN TO DCS-IDKUNDNR-SRETUR              
134300       END-IF                                                             
134400       MOVE MID-IDKUNDNR-SRETUR-IN    TO DCSL-BETEXT-NEWDATA              
134500       ADD +1                 TO FIL-IDSEKVNR                             
134600       PERFORM IMS-ISRT-WDR601                                            
134700     END-IF                                                               
134800                                                                          
134900     IF MID-IDPERSON-REM-IN NOT = ALL '+'                                 
135000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPERSON-REM-IN-ATTR             
135100       MOVE 'IDPERSON-REM'      TO DCSL-BETEXT-ITEM                       
135200       MOVE DCS-IDPERSON-REM    TO DCSL-BETEXT-OLDDATA                    
135300       MOVE MID-IDPERSON-REM-IN TO DCS-IDPERSON-REM                       
135400                                   DCSL-BETEXT-NEWDATA                    
135500       ADD +1                   TO FIL-IDSEKVNR                           
135600       PERFORM IMS-ISRT-WDR601                                            
135700     END-IF                                                               
135800                                                                          
135900     IF MID-IDKST-JUST-IN NOT = ALL '+'                                   
136000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKST-JUST-IN-ATTR               
136100       MOVE 'IDKST-JUST'        TO DCSL-BETEXT-ITEM                       
136200       MOVE DCS-IDKST-JUST      TO DCSL-BETEXT-OLDDATA                    
136300       MOVE MID-IDKST-JUST-IN   TO DCS-IDKST-JUST                         
136400                                   DCSL-BETEXT-NEWDATA                    
136500       ADD +1                   TO FIL-IDSEKVNR                           
136600       PERFORM IMS-ISRT-WDR601                                            
136700     END-IF                                                               
136800                                                                          
136900     IF MID-IDKONTO-JUST-IN NOT = ALL '+'                                 
137000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKONTO-JUST-IN-ATTR             
137100       MOVE 'IDKONTO-JUST'      TO DCSL-BETEXT-ITEM                       
137200       MOVE DCS-IDKONTO-JUST    TO DCSL-BETEXT-OLDDATA                    
137300       MOVE MID-IDKONTO-JUST-IN TO DCS-IDKONTO-JUST                       
137400                                   DCSL-BETEXT-NEWDATA                    
137500       ADD +1                   TO FIL-IDSEKVNR                           
137600       PERFORM IMS-ISRT-WDR601                                            
137700     END-IF                                                               
137800                                                                          
137900     IF MID-IDANALYS-JUST-IN NOT = ALL '+'                                
138000       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-IDANALYS-JUST-IN-ATTR           
138100       MOVE 'IDANALYS-JUST'      TO DCSL-BETEXT-ITEM                      
138200       MOVE DCS-IDANALYS-JUST    TO DCSL-BETEXT-OLDDATA                   
138300       MOVE MID-IDANALYS-JUST-IN TO DCS-IDANALYS-JUST                     
138400                                    DCSL-BETEXT-NEWDATA                   
138500       ADD +1                    TO FIL-IDSEKVNR                          
138600       PERFORM IMS-ISRT-WDR601                                            
138700     END-IF                                                               
138800                                                                          
138810     IF MID-KDRT-JUST-IN NOT = ALL '+'                                    
138820       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KDRT-JUST-IN-ATTR               
138830       MOVE 'KDRT-JUST'          TO DCSL-BETEXT-ITEM                      
138840       MOVE DCS-KDRT-JUST        TO DCSL-BETEXT-OLDDATA                   
138850       MOVE MID-KDRT-JUST-IN     TO DCS-KDRT-JUST                         
138860                                    DCSL-BETEXT-NEWDATA                   
138870       ADD +1                    TO FIL-IDSEKVNR                          
138880       PERFORM IMS-ISRT-WDR601                                            
138890     END-IF                                                               
138891                                                                          
138900     IF MID-IDKST-SKROT-IN NOT = ALL '+'                                  
139000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKST-SKROT-IN-ATTR              
139100       MOVE 'IDKST-SKROT'       TO DCSL-BETEXT-ITEM                       
139200       MOVE DCS-IDKST-SKROT     TO DCSL-BETEXT-OLDDATA                    
139300       MOVE MID-IDKST-SKROT-IN  TO DCS-IDKST-SKROT                        
139400                                   DCSL-BETEXT-NEWDATA                    
139500       ADD +1                   TO FIL-IDSEKVNR                           
139600       PERFORM IMS-ISRT-WDR601                                            
139700     END-IF                                                               
139800                                                                          
139900     IF MID-IDKONTO-SKROT-IN NOT = ALL '+'                                
140000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKONTO-SKROT-IN-ATTR            
140100       MOVE 'IDKONTO-SKROT'       TO DCSL-BETEXT-ITEM                     
140200       MOVE DCS-IDKONTO-SKROT     TO DCSL-BETEXT-OLDDATA                  
140300       MOVE MID-IDKONTO-SKROT-IN  TO DCS-IDKONTO-SKROT                    
140400                                     DCSL-BETEXT-NEWDATA                  
140500       ADD +1                     TO FIL-IDSEKVNR                         
140600       PERFORM IMS-ISRT-WDR601                                            
140700     END-IF                                                               
140800                                                                          
140900     IF MID-IDANALYS-SKROT-IN NOT = ALL '+'                               
141000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANALYS-SKROT-IN-ATTR           
141100       MOVE 'IDANALYS-SKROT'      TO DCSL-BETEXT-ITEM                     
141200       MOVE DCS-IDANALYS-SKROT    TO DCSL-BETEXT-OLDDATA                  
141300       MOVE MID-IDANALYS-SKROT-IN TO DCS-IDANALYS-SKROT                   
141400                                     DCSL-BETEXT-NEWDATA                  
141500       ADD +1                     TO FIL-IDSEKVNR                         
141600       PERFORM IMS-ISRT-WDR601                                            
141700     END-IF                                                               
141800                                                                          
141810     IF MID-KDRT-SKROT-IN NOT = ALL '+'                                   
141820       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDRT-SKROT-IN-ATTR               
141830       MOVE 'KDRT-SKRPT'          TO DCSL-BETEXT-ITEM                     
141840       MOVE DCS-KDRT-SKROT        TO DCSL-BETEXT-OLDDATA                  
141850       MOVE MID-KDRT-SKROT-IN     TO DCS-KDRT-SKROT                       
141860                                     DCSL-BETEXT-NEWDATA                  
141870       ADD +1                     TO FIL-IDSEKVNR                         
141880       PERFORM IMS-ISRT-WDR601                                            
141890     END-IF                                                               
141891                                                                          
141900     IF MID-IDKST-MIX-IN NOT = ALL '+'                                    
142000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKST-MIX-IN-ATTR                
142100       MOVE 'IDKST-MIX'         TO DCSL-BETEXT-ITEM                       
142200       MOVE DCS-IDKST-MIX       TO DCSL-BETEXT-OLDDATA                    
142300       MOVE MID-IDKST-MIX-IN    TO DCS-IDKST-MIX                          
142400                                   DCSL-BETEXT-NEWDATA                    
142500       ADD +1                   TO FIL-IDSEKVNR                           
142600       PERFORM IMS-ISRT-WDR601                                            
142700     END-IF                                                               
142800                                                                          
142900     IF MID-IDKONTO-MIX-IN NOT = ALL '+'                                  
143000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKONTO-MIX-IN-ATTR              
143100       MOVE 'IDKONTO-MIX'         TO DCSL-BETEXT-ITEM                     
143200       MOVE DCS-IDKONTO-MIX       TO DCSL-BETEXT-OLDDATA                  
143300       MOVE MID-IDKONTO-MIX-IN    TO DCS-IDKONTO-MIX                      
143400                                     DCSL-BETEXT-NEWDATA                  
143500       ADD +1                     TO FIL-IDSEKVNR                         
143600       PERFORM IMS-ISRT-WDR601                                            
143700     END-IF                                                               
143800                                                                          
143900     IF MID-IDANALYS-MIX-IN NOT = ALL '+'                                 
144000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANALYS-MIX-IN-ATTR             
144100       MOVE 'IDANALYS-MIX'        TO DCSL-BETEXT-ITEM                     
144200       MOVE DCS-IDANALYS-MIX      TO DCSL-BETEXT-OLDDATA                  
144300       MOVE MID-IDANALYS-MIX-IN   TO DCS-IDANALYS-MIX                     
144400                                     DCSL-BETEXT-NEWDATA                  
144500       ADD +1                     TO FIL-IDSEKVNR                         
144600       PERFORM IMS-ISRT-WDR601                                            
144700     END-IF                                                               
144800                                                                          
144810     IF MID-KDRT-MIX-IN NOT = ALL '+'                                     
144820       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDRT-MIX-IN-ATTR                 
144830       MOVE 'KDRT-MIX'            TO DCSL-BETEXT-ITEM                     
144840       MOVE DCS-KDRT-MIX          TO DCSL-BETEXT-OLDDATA                  
144850       MOVE MID-KDRT-MIX-IN       TO DCS-KDRT-MIX                         
144860                                     DCSL-BETEXT-NEWDATA                  
144870       ADD +1                     TO FIL-IDSEKVNR                         
144880       PERFORM IMS-ISRT-WDR601                                            
144890     END-IF                                                               
144891                                                                          
144900     PERFORM IMS-REPL-WDB601                                              
145000*    IF DCS-NDC-CN                                                        
145100     IF W-IDDC-REF NOT = WC-CDC-SE                                        
145110     OR MID-FLTABORT = 'J'                                                
145200        PERFORM HA-UPPDATERA-REFILLINFO                                   
145300     END-IF                                                               
145400                                                                          
145500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
145600     CALL WMEDKONV USING MED-WMEDAREA                                     
145700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
145800     PERFORM MFS-FORM-ATTR                                                
145900     PERFORM MFS-RENSA-FAELT-IN                                           
146000     .                                                                    
146100     EJECT                                                                
146200 HA-UPPDATERA-REFILLINFO  SECTION.                                        
146300                                                                          
146400     MOVE SPAR-IDDC-REF TO W-IDDC-REF                                     
146500     PERFORM IMS-GHU-WDB616                                               
146600     IF SEGMENT-SAKNAS                                                    
146700        MOVE SPAR-IDDC-REF TO REF-IDDC-REF                                
146800        MOVE ZERO          TO REF-IDDISTR-REFILL                          
146900                              REF-IDDISTR-RETUR                           
147000                              REF-IDDISTR-QRETUR                          
147100                              REF-IDKUNDNR-BPS                            
147200                              REF-IDKUNDNR-SBPS                           
147300                              REF-IDKUNDNR-RETUR                          
147400                              REF-IDKUNDNR-QRETUR                         
147500                              REF-IDKUNDNR-SRETUR                         
147600                              REF-IDKUNDNR-TRETUR                         
147700                              REF-IDKUNDNR-SORD                           
147800                              REF-IDKUNDNR-SQRET                          
147900        MOVE 1     TO REF-KVDLTID-TOT                                     
148000                      REF-KVDLTID-BOATPAC                                 
148100                      REF-KVDLTID-AIRREQ                                  
148200        MOVE ZERO  TO REF-KVDLTID-BOATTRP                                 
148300                      REF-KVDLTID-BOAT2DC                                 
148400                      REF-KVDLTID-BOATINS                                 
148500                      REF-KVDLTID-AIRETA                                  
148600                      REF-KVDLTID-AIRPAC                                  
148700                      REF-KVDLTID-AIRTRP                                  
148800                      REF-KVDLTID-AIRINS                                  
148900                      REF-KVDLTID-CUST                                    
149000                      REF-KVDLTID-CUSTWAIT                                
149100                      REF-KVDLTID-CUST2DC                                 
149110                      REF-KVDLTID-BUFF                                    
149120                      REF-TIREFBAT                                        
149130                      REF-REAIRCO                                         
149130        MOVE 1.00  TO REF-RESSFAC                                         
149130        MOVE 10000 TO REF-PRFRAKT                                         
149200        MOVE SPACE TO REF-BETEXT                                          
149200        PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX            
149200           IF INDX = 7                                                    
149200             MOVE 'Y'              TO REF-FLREFDAY (INDX)                 
149200                                      REF-FLREFBLK (INDX)                 
149200                                      REF-KDREFDG  (INDX)                 
149200           ELSE                                                           
149200             MOVE 'N'              TO REF-FLREFDAY (INDX)                 
149200                                      REF-FLREFBLK (INDX)                 
149200             MOVE SPACE            TO REF-KDREFDG  (INDX)                 
149200           END-IF                                                         
149200        END-PERFORM                                                       
149300        PERFORM IMS-ISRT-WDB616                                           
149400        PERFORM IMS-GHU-WDB616                                            
149500     END-IF                                                               
149600                                                                          
149602     IF MID-FLTABORT = 'J'                                                
149603        PERFORM IMS-DLET-WDB616                                           
149604     ELSE                                                                 
149610        IF (MID-IDDISTR-REFILL-IN  NOT = ALL '+' OR                       
149620            MID-IDDISTR-RETUR-IN   NOT = ALL '+' OR                       
149630            MID-IDDISTR-QRETUR-IN  NOT = ALL '+' OR                       
149640            MID-IDKUNDNR-SORD-IN   NOT = ALL '+' OR                       
149650            MID-IDKUNDNR-RETUR-IN  NOT = ALL '+' OR                       
149660            MID-IDKUNDNR-QRETUR-IN NOT = ALL '+' OR                       
149670            MID-IDKUNDNR-BPS-IN    NOT = ALL '+' OR                       
149680            MID-IDKUNDNR-TRETUR-IN NOT = ALL '+' OR                       
149690            MID-IDKUNDNR-SQRET-IN  NOT = ALL '+' OR                       
149691            MID-IDKUNDNR-SBPS-IN   NOT = ALL '+' OR                       
149692            MID-IDKUNDNR-SRETUR-IN NOT = ALL '+')                         
149700                                                                          
149800            IF MID-IDDISTR-REFILL-IN NOT = ALL '+'                        
149900               MOVE MID-IDDISTR-REFILL-IN  TO REF-IDDISTR-REFILL          
150000            END-IF                                                        
150100                                                                          
150200            IF MID-IDDISTR-RETUR-IN NOT = ALL '+'                         
150300               MOVE MID-IDDISTR-RETUR-IN   TO REF-IDDISTR-RETUR           
150400            END-IF                                                        
150500                                                                          
150600            IF MID-IDDISTR-QRETUR-IN NOT = ALL '+'                        
150700               MOVE MID-IDDISTR-QRETUR-IN  TO REF-IDDISTR-QRETUR          
150800            END-IF                                                        
150900                                                                          
151000            IF MID-IDKUNDNR-SORD-IN NOT = ALL '+'                         
151100               MOVE MID-IDKUNDNR-SORD-IN   TO REF-IDKUNDNR-SORD           
151200            END-IF                                                        
151300                                                                          
151400            IF MID-IDKUNDNR-RETUR-IN NOT = ALL '+'                        
151500               MOVE MID-IDKUNDNR-RETUR-IN  TO REF-IDKUNDNR-RETUR          
151600            END-IF                                                        
151700                                                                          
151800            IF MID-IDKUNDNR-QRETUR-IN NOT = ALL '+'                       
151900               MOVE MID-IDKUNDNR-QRETUR-IN TO REF-IDKUNDNR-QRETUR         
152000            END-IF                                                        
152100                                                                          
152200            IF MID-IDKUNDNR-BPS-IN NOT = ALL '+'                          
152300               MOVE MID-IDKUNDNR-BPS-IN    TO REF-IDKUNDNR-BPS            
152400            END-IF                                                        
152500                                                                          
152600            IF MID-IDKUNDNR-TRETUR-IN NOT = ALL '+'                       
152700               MOVE MID-IDKUNDNR-TRETUR-IN TO REF-IDKUNDNR-TRETUR         
152800            END-IF                                                        
152900                                                                          
153000            IF MID-IDKUNDNR-SQRET-IN NOT = ALL '+'                        
153100               MOVE MID-IDKUNDNR-SQRET-IN  TO REF-IDKUNDNR-SQRET          
153200            END-IF                                                        
153300                                                                          
153400            IF MID-IDKUNDNR-SBPS-IN NOT = ALL '+'                         
153500               MOVE MID-IDKUNDNR-SBPS-IN   TO REF-IDKUNDNR-SBPS           
153600            END-IF                                                        
153700                                                                          
153800            IF MID-IDKUNDNR-SRETUR-IN NOT = ALL '+'                       
153900               MOVE MID-IDKUNDNR-SRETUR-IN TO REF-IDKUNDNR-SRETUR         
154000            END-IF                                                        
154100            PERFORM IMS-REPL-WDB616                                       
154200        END-IF                                                            
154300     END-IF                                                               
154400     .                                                                    
154500     EJECT                                                                
157600 MFS-RENSA-FAELT-UT SECTION.                                              
157700                                                                          
157800*    --- ALLA UTDATA-FÄLT                                                 
157810     MOVE MFS-RENSA-FAELT TO MOD-FLTABORT                                 
157900                             MOD-KDFRAKT-BPS                              
158000                             MOD-KDFRAKT-SBPS                             
158100                             MOD-IDDISTR-REFILL                           
158200                             MOD-IDDISTR-RETUR                            
158300                             MOD-IDDISTR-QRETUR                           
158400                             MOD-IDDISTR-SKROT                            
158500                             MOD-IDDISTR-QSKROT                           
158600                             MOD-IDDISTR-RSKROT                           
158700                             MOD-IDDISTR-MIX                              
158800                             MOD-IDDISTR-JUST                             
158900                             MOD-IDKUNDNR-SORD                            
159000                             MOD-IDKUNDNR-RETUR                           
159100                             MOD-IDKUNDNR-QRETUR                          
159200                             MOD-IDKUNDNR-SKROT                           
159300                             MOD-IDKUNDNR-QSKROT                          
159400                             MOD-IDKUNDNR-RSKROT                          
159500                             MOD-IDKUNDNR-MIX                             
159600                             MOD-IDKUNDNR-JUST                            
159700                             MOD-IDKUNDNR-BPS                             
159800                             MOD-IDKUNDNR-TRETUR                          
159900                             MOD-IDKUNDNR-SQRET                           
160000                             MOD-IDKUNDNR-SBPS                            
160100                             MOD-IDKUNDNR-SRETUR                          
160200                             MOD-IDPERSON-REM                             
160300                             MOD-IDKST-JUST                               
160400                             MOD-IDKONTO-JUST                             
160500                             MOD-IDANALYS-JUST                            
160510                             MOD-KDRT-JUST                                
160600                             MOD-IDKST-SKROT                              
160700                             MOD-IDKONTO-SKROT                            
160800                             MOD-IDANALYS-SKROT                           
160810                             MOD-KDRT-SKROT                               
160900                             MOD-IDKST-MIX                                
161000                             MOD-IDKONTO-MIX                              
161100                             MOD-IDANALYS-MIX                             
161110                             MOD-KDRT-MIX                                 
161200     .                                                                    
161300     EJECT                                                                
161400 MFS-RENSA-FAELT-IN SECTION.                                              
161500                                                                          
161600*    --- ALLA INDATA-FÄLT                                                 
161700     MOVE MFS-RENSA-FAELT TO MOD-FLTABORT                                 
161710                             MOD-KDFRAKT-BPS-IN                           
161800                             MOD-KDFRAKT-SBPS-IN                          
161900                             MOD-IDDISTR-REFILL-IN                        
162000                             MOD-IDDISTR-RETUR-IN                         
162100                             MOD-IDDISTR-QRETUR-IN                        
162200                             MOD-IDDISTR-SKROT-IN                         
162300                             MOD-IDDISTR-QSKROT-IN                        
162400                             MOD-IDDISTR-RSKROT-IN                        
162500                             MOD-IDDISTR-MIX-IN                           
162600                             MOD-IDDISTR-JUST-IN                          
162700                             MOD-IDKUNDNR-SORD-IN                         
162800                             MOD-IDKUNDNR-RETUR-IN                        
162900                             MOD-IDKUNDNR-QRETUR-IN                       
163000                             MOD-IDKUNDNR-SKROT-IN                        
163100                             MOD-IDKUNDNR-QSKROT-IN                       
163200                             MOD-IDKUNDNR-RSKROT-IN                       
163300                             MOD-IDKUNDNR-MIX-IN                          
163400                             MOD-IDKUNDNR-JUST-IN                         
163500                             MOD-IDKUNDNR-BPS-IN                          
163600                             MOD-IDKUNDNR-TRETUR-IN                       
163700                             MOD-IDKUNDNR-SQRET-IN                        
163800                             MOD-IDKUNDNR-SBPS-IN                         
163900                             MOD-IDKUNDNR-SRETUR-IN                       
164000                             MOD-IDPERSON-REM-IN                          
164100                             MOD-IDKST-JUST-IN                            
164200                             MOD-IDKONTO-JUST-IN                          
164300                             MOD-IDANALYS-JUST-IN                         
164310                             MOD-KDRT-JUST-IN                             
164400                             MOD-IDKST-SKROT-IN                           
164500                             MOD-IDKONTO-SKROT-IN                         
164600                             MOD-IDANALYS-SKROT-IN                        
164610                             MOD-KDRT-SKROT-IN                            
164700                             MOD-IDKST-MIX-IN                             
164800                             MOD-IDKONTO-MIX-IN                           
164900                             MOD-IDANALYS-MIX-IN                          
164910                             MOD-KDRT-MIX-IN                              
165300     .                                                                    
165400     EJECT                                                                
165500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
165600                                                                          
165700*    --- ALLA UTDATA-FÄLT                                                 
165800     MOVE MFS-ROER-EJ-FAELT TO MOD-FLTABORT                               
165810                               MOD-KDFRAKT-BPS                            
165900                               MOD-KDFRAKT-SBPS                           
166000                               MOD-IDDISTR-REFILL                         
166100                               MOD-IDDISTR-RETUR                          
166200                               MOD-IDDISTR-QRETUR                         
166300                               MOD-IDDISTR-SKROT                          
166400                               MOD-IDDISTR-QSKROT                         
166500                               MOD-IDDISTR-RSKROT                         
166600                               MOD-IDDISTR-MIX                            
166700                               MOD-IDDISTR-JUST                           
166800                               MOD-IDKUNDNR-SORD                          
166900                               MOD-IDKUNDNR-RETUR                         
167000                               MOD-IDKUNDNR-QRETUR                        
167100                               MOD-IDKUNDNR-SKROT                         
167200                               MOD-IDKUNDNR-QSKROT                        
167300                               MOD-IDKUNDNR-RSKROT                        
167400                               MOD-IDKUNDNR-MIX                           
167500                               MOD-IDKUNDNR-JUST                          
167600                               MOD-IDKUNDNR-BPS                           
167700                               MOD-IDKUNDNR-TRETUR                        
167800                               MOD-IDKUNDNR-SQRET                         
167900                               MOD-IDKUNDNR-SBPS                          
168000                               MOD-IDKUNDNR-SRETUR                        
168100                               MOD-IDPERSON-REM                           
168200                               MOD-IDKST-JUST                             
168300                               MOD-IDKONTO-JUST                           
168400                               MOD-IDANALYS-JUST                          
168410                               MOD-KDRT-JUST                              
168500                               MOD-IDKST-SKROT                            
168600                               MOD-IDKONTO-SKROT                          
168700                               MOD-IDANALYS-SKROT                         
168710                               MOD-KDRT-SKROT                             
168800                               MOD-IDKST-MIX                              
168900                               MOD-IDKONTO-MIX                            
169000                               MOD-IDANALYS-MIX                           
169010                               MOD-KDRT-MIX                               
169400     .                                                                    
169500     EJECT                                                                
169600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
169700                                                                          
169800*    --- ALLA INDATA-FÄLT                                                 
169810     MOVE MFS-ROER-EJ-FAELT TO MOD-FLTABORT                               
170000                               MOD-KDFRAKT-BPS-IN                         
170010                               MOD-KDFRAKT-SBPS-IN                        
170100                               MOD-IDDISTR-REFILL-IN                      
170200                               MOD-IDDISTR-RETUR-IN                       
170300                               MOD-IDDISTR-QRETUR-IN                      
170400                               MOD-IDDISTR-SKROT-IN                       
170500                               MOD-IDDISTR-QSKROT-IN                      
170600                               MOD-IDDISTR-RSKROT-IN                      
170700                               MOD-IDDISTR-MIX-IN                         
170800                               MOD-IDDISTR-JUST-IN                        
170900                               MOD-IDKUNDNR-SORD-IN                       
171000                               MOD-IDKUNDNR-RETUR-IN                      
171100                               MOD-IDKUNDNR-QRETUR-IN                     
171200                               MOD-IDKUNDNR-SKROT-IN                      
171300                               MOD-IDKUNDNR-QSKROT-IN                     
171400                               MOD-IDKUNDNR-RSKROT-IN                     
171500                               MOD-IDKUNDNR-MIX-IN                        
171600                               MOD-IDKUNDNR-JUST-IN                       
171700                               MOD-IDKUNDNR-BPS-IN                        
171800                               MOD-IDKUNDNR-TRETUR-IN                     
171900                               MOD-IDKUNDNR-SQRET-IN                      
172000                               MOD-IDKUNDNR-SBPS-IN                       
172100                               MOD-IDKUNDNR-SRETUR-IN                     
172200                               MOD-IDPERSON-REM-IN                        
172300                               MOD-IDKST-JUST-IN                          
172400                               MOD-IDKONTO-JUST-IN                        
172500                               MOD-IDANALYS-JUST-IN                       
172510                               MOD-KDRT-JUST-IN                           
172600                               MOD-IDKST-SKROT-IN                         
172700                               MOD-IDKONTO-SKROT-IN                       
172800                               MOD-IDANALYS-SKROT-IN                      
172810                               MOD-KDRT-SKROT-IN                          
172900                               MOD-IDKST-MIX-IN                           
173000                               MOD-IDKONTO-MIX-IN                         
173100                               MOD-IDANALYS-MIX-IN                        
173110                               MOD-KDRT-MIX-IN                            
173500     .                                                                    
173600     EJECT                                                                
173700 MFS-FORM-ATTR SECTION.                                                   
173800                                                                          
173900*    --- ALLA INDATA-FÄLT                                                 
174000     MOVE MFS-FORMATETS-ATTR TO MOD-FLTABORT-ATTR                         
174100                                MOD-KDFRAKT-BPS-IN-ATTR                   
174110                                MOD-KDFRAKT-SBPS-IN-ATTR                  
174200                                MOD-IDDISTR-REFILL-IN-ATTR                
174300                                MOD-IDDISTR-RETUR-IN-ATTR                 
174400                                MOD-IDDISTR-QRETUR-IN-ATTR                
174500                                MOD-IDDISTR-SKROT-IN-ATTR                 
174600                                MOD-IDDISTR-QSKROT-IN-ATTR                
174700                                MOD-IDDISTR-RSKROT-IN-ATTR                
174800                                MOD-IDDISTR-MIX-IN-ATTR                   
174900                                MOD-IDDISTR-JUST-IN-ATTR                  
175000                                MOD-IDKUNDNR-SORD-IN-ATTR                 
175100                                MOD-IDKUNDNR-RETUR-IN-ATTR                
175200                                MOD-IDKUNDNR-QRETUR-IN-ATTR               
175300                                MOD-IDKUNDNR-SKROT-IN-ATTR                
175400                                MOD-IDKUNDNR-QSKROT-IN-ATTR               
175500                                MOD-IDKUNDNR-RSKROT-IN-ATTR               
175600                                MOD-IDKUNDNR-MIX-IN-ATTR                  
175700                                MOD-IDKUNDNR-JUST-IN-ATTR                 
175800                                MOD-IDKUNDNR-BPS-IN-ATTR                  
175900                                MOD-IDKUNDNR-TRETUR-IN-ATTR               
176000                                MOD-IDKUNDNR-SQRET-IN-ATTR                
176100                                MOD-IDKUNDNR-SBPS-IN-ATTR                 
176200                                MOD-IDKUNDNR-SRETUR-IN-ATTR               
176300                                MOD-IDPERSON-REM-IN-ATTR                  
176400                                MOD-IDKST-JUST-IN-ATTR                    
176500                                MOD-IDKONTO-JUST-IN-ATTR                  
176600                                MOD-IDANALYS-JUST-IN-ATTR                 
176610                                MOD-KDRT-JUST-IN-ATTR                     
176700                                MOD-IDKST-SKROT-IN-ATTR                   
176800                                MOD-IDKONTO-SKROT-IN-ATTR                 
176900                                MOD-IDANALYS-SKROT-IN-ATTR                
176910                                MOD-KDRT-SKROT-IN-ATTR                    
177000                                MOD-IDKST-MIX-IN-ATTR                     
177100                                MOD-IDKONTO-MIX-IN-ATTR                   
177200                                MOD-IDANALYS-MIX-IN-ATTR                  
177210                                MOD-KDRT-MIX-IN-ATTR                      
177600     .                                                                    
177700     EJECT                                                                
177800* --- IMS SEKTIONER ---                                                   
177900     SKIP3                                                                
178000 IMS-GET-MSG SECTION.                                                     
178100                                                                          
178200     MOVE '  QC' TO GODK-STATUSKODER                                      
178300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
178400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
178500     PERFORM IMS-STATUSKONTROLL                                           
178600     .                                                                    
178700     SKIP3                                                                
178800 IMS-INSERT-MSG SECTION.                                                  
178900                                                                          
179000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
179100     MOVE SPACE TO GODK-STATUSKODER                                       
179200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
179300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
179400     PERFORM IMS-STATUSKONTROLL                                           
179500     .                                                                    
179600     EJECT                                                                
179700 IMS-GU-WDB601 SECTION.                                                   
179800                                                                          
179900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
180000          DELIMITED BY SIZE INTO SSA1                                     
180100     MOVE '  GE' TO GODK-STATUSKODER                                      
180200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
180300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
180400     PERFORM IMS-STATUSKONTROLL                                           
180500     .                                                                    
180600     SKIP3                                                                
180700 IMS-GHU-WDB601 SECTION.                                                  
180800                                                                          
180900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
181000          DELIMITED BY SIZE INTO SSA1                                     
181100     MOVE '  ' TO GODK-STATUSKODER                                        
181200     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
181300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
181400     PERFORM IMS-STATUSKONTROLL                                           
181500     .                                                                    
181600     SKIP3                                                                
181700 IMS-REPL-WDB601 SECTION.                                                 
181800                                                                          
181900     MOVE '  ' TO GODK-STATUSKODER                                        
182000     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
182100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
182200     PERFORM IMS-STATUSKONTROLL                                           
182300     .                                                                    
182400     EJECT                                                                
182500 IMS-GU-WDB616 SECTION.                                                   
182600                                                                          
182700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
182800          DELIMITED BY SIZE INTO SSA1                                     
182900     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
183000          DELIMITED BY SIZE INTO SSA2                                     
183100     MOVE '  GE' TO GODK-STATUSKODER                                      
183200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
183300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
183400     PERFORM IMS-STATUSKONTROLL                                           
183500     .                                                                    
183600     SKIP3                                                                
183700 IMS-GHU-WDB616 SECTION.                                                  
183800                                                                          
183900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
184000          DELIMITED BY SIZE INTO SSA1                                     
184100     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
184200          DELIMITED BY SIZE INTO SSA2                                     
184300     MOVE '  GE' TO GODK-STATUSKODER                                      
184400     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2              
184500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
184600     PERFORM IMS-STATUSKONTROLL                                           
184700     .                                                                    
184800     SKIP3                                                                
184900 IMS-GNP-WDB616 SECTION.                                                  
185000                                                                          
185100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
185200          DELIMITED BY SIZE INTO SSA1                                     
185300     MOVE   'WDB616 '         TO SSA2                                     
185400     MOVE '  GE' TO GODK-STATUSKODER                                      
185500     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB616 SSA1 SSA2              
185600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
185700     PERFORM IMS-STATUSKONTROLL                                           
185800     .                                                                    
185900     SKIP3                                                                
186000 IMS-ISRT-WDB616 SECTION.                                                 
186100                                                                          
186200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
186300          DELIMITED BY SIZE INTO SSA1                                     
186400     MOVE 'WDB616   '         TO SSA2                                     
186500     MOVE '  ' TO GODK-STATUSKODER                                        
186600     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB616 SSA1 SSA2             
186700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
186800     PERFORM IMS-STATUSKONTROLL                                           
186900     .                                                                    
187000     EJECT                                                                
187100 IMS-REPL-WDB616 SECTION.                                                 
187200                                                                          
187300     MOVE '  ' TO GODK-STATUSKODER                                        
187400     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB616                       
187500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
187600     PERFORM IMS-STATUSKONTROLL                                           
187700     .                                                                    
187800     EJECT                                                                
187900 IMS-DLET-WDB616 SECTION.                                                 
188000                                                                          
188100     MOVE '  ' TO GODK-STATUSKODER                                        
188200     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB616                       
188300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
188400     PERFORM IMS-STATUSKONTROLL                                           
188500     .                                                                    
188600     EJECT                                                                
192100 IMS-ISRT-WDR601 SECTION.                                                 
192200                                                                          
192300     MOVE 'WDR601 ' TO SSA1                                               
192400     MOVE '    ' TO GODK-STATUSKODER                                      
192500     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
192600     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
192700     PERFORM IMS-STATUSKONTROLL                                           
192800     .                                                                    
192900     SKIP3                                                                
192910 IMS-GN-WDK7B SECTION.                                                    
192920                                                                          
192930     STRING 'WDK7B1  (WDK7B1KY>=' W-WDK7B1KY-MIN-X                        
192940                    '&WDK7B1KY<=' W-WDK7B1KY-MAX-X ')'                    
192950             DELIMITED BY SIZE INTO SSA1                                  
192961     MOVE '  GE' TO GODK-STATUSKODER                                      
192970     CALL CBLTDLI USING GN WDK7B-PCB DLI-IO-WDK7B1 SSA1                   
192980     MOVE WDK7B-STATUS-CODE      TO STATUS-WS                             
192981     PERFORM IMS-STATUSKONTROLL                                           
192991     .                                                                    
192992     SKIP3                                                                
192993 IMS-GU-WDG301 SECTION.                                                   
192995                                                                          
192996     STRING 'WDG301  (WDG3KEY  =' W-WDG301-2203-X  ')'                    
192998            DELIMITED BY SIZE INTO SSA1                                   
192999     MOVE '  GE' TO GODK-STATUSKODER                                      
193000     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-WDG301 SSA1                    
193001     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
193002     PERFORM IMS-STATUSKONTROLL                                           
193003     .                                                                    
193004 IMS-ISRT-WDG301 SECTION.                                                 
193005                                                                          
193006     MOVE 'WDG301 '        TO SSA1                                        
193007     MOVE '  II'           TO GODK-STATUSKODER                            
193008     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDG301 SSA1                  
193009     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
193010     PERFORM IMS-STATUSKONTROLL                                           
193011     .                                                                    
193012     EJECT                                                                
193020 IMS-STATUSKONTROLL SECTION.                                              
193100                                                                          
193200     SET STATUS-IX TO 1                                                   
193300     SEARCH GODK-STATUS                                                   
193400       AT END                                                             
193500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
193600         DELIMITED BY SIZE INTO FELTEXT                                   
193700         CALL FELLOG                                                      
193800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
193900         CONTINUE                                                         
194000     END-SEARCH                                                           
194100     .                                                                    
