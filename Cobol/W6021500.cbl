000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6021500.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   99/12/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DC:NAS STOCK-CHECK-KOMMENTARER.                                  
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR W6D2                                       
001100*        PROGRAMMET LÄSER      WDK7                                       
001200*        PROGRAMMET LÄSER      WDB6                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W6T215                                              
001600*        MID:         W6I21501                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W6O21501                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6021500'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
003800 77  DCIX                        PIC 9(2)    VALUE ZERO.                  
003900 77  DCMAX                       PIC 9(2)    VALUE 13.                    
004000 77  SPRAK-IX                    PIC 9       VALUE ZERO.                  
004100 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
004200 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
004300 01  WS-IMS                      PIC X(80).                               
004400 01  WS-SECTION                  PIC X(80).                               
004500 01  W1-DAREGDAT                 PIC 9(8).                                
004600 01  WS-IDDC-IN                  PIC XX      VALUE SPACE.                 
004700 01  WS-IDDC-6332                PIC XX      VALUE SPACE.                 
004800 01  WS-IDDC-6334                PIC XX      VALUE SPACE.                 
004900 01  WS-IDDC-WDGX                PIC XX      VALUE SPACE.                 
005000 01  WS-SPAR-IDDC                PIC XX      VALUE SPACE.                 
005100 01  WS-IDDC-NEXT                PIC XX      VALUE SPACE.                 
005200 01  WS-IDDC-RAD                 PIC XX      VALUE SPACE.                 
005300 01  WS-SHOW-ALL                 PIC X       VALUE SPACE.                 
005400 01  WS-FL-DC-INFO               PIC X       VALUE SPACE.                 
005500 01  WS-FORSTA-POST              PIC X       VALUE SPACE.                 
005600 01  WS-LEVEL-POST               PIC X       VALUE SPACE.                 
005700 01  WS-FLFIN                    PIC X       VALUE SPACE.                 
005800 01  WS-KVANTAL                  PIC S9(9) VALUE ZERO COMP-3.             
005900 01  WS-KVAVV-KVAL               PIC S9(9) VALUE ZERO COMP-3.             
006000 01  WS-KVART-SKROT              PIC S9(9) VALUE ZERO COMP-3.             
006100 01  WS-KVART-RET                PIC S9(9) VALUE ZERO COMP-3.             
006200 01  WS-KVART-KJUST              PIC S9(9) VALUE ZERO COMP-3.             
006300 01  WS-KVLS                     PIC S9(9) VALUE ZERO COMP-3.             
006400 01  WS-KVSPARR-KVAL             PIC S9(9) VALUE ZERO COMP-3.             
006500 01  WS-KDLEVSP                  PIC 9(2)  VALUE ZERO COMP-3.             
006600 01  WS-TISTADAT                 PIC 9(6)  VALUE ZERO.                    
006700 01  WS-TISTODAT                 PIC 9(6)  VALUE ZERO.                    
006800 01  WS-BEINIT                   PIC X(3)  VALUE SPACE.                   
006900 01  WS-DAGENS-TID               PIC S9(9) VALUE ZERO COMP-3.             
007000 01  WS-DAGENS-DATUM             PIC  9(9) VALUE ZERO.                    
007800                                                                          
007900*      --- VALID IDDC CODES                                               
008000*                                                                         
008100*01    -COPY WWDC99                                                       
008110*01    -COPY WWDCKONS                                                     
008200       EJECT                                                              
008300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008400     88  INDATA-OK                           VALUE 'J'.                   
008500     88  INDATA-FEL                          VALUE 'N'.                   
008600                                                                          
008700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008800     88  NYCKLAR-OK                          VALUE 'J'.                   
008900     88  NYCKLAR-FEL                         VALUE 'N'.                   
009000                                                                          
009100 77  NYTT-ARTNR-SW               PIC X       VALUE 'N'.                   
009200     88  NYTT-ARTNR                          VALUE 'J'.                   
009300     88  GAMMALT-ARTNR                       VALUE 'N'.                   
009400                                                                          
009500 77  POST-LEVEL-SW               PIC X       VALUE 'N'.                   
009600     88  POST-LEVEL-3                        VALUE 'J'.                   
009700     88  POST-LEVEL-2                        VALUE 'N'.                   
009800                                                                          
009900 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
010000     88  POST-FINNS                          VALUE 'J'.                   
010100     88  POST-SAKNAS                         VALUE 'N'.                   
010200                                                                          
010300 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
010400     88  UPDATE-OK                           VALUE 'J'.                   
010500     88  UPDATE-NOT-OK                       VALUE 'N'.                   
010600                                                                          
010700 77  TRYCK-PF11-SW               PIC X       VALUE 'N'.                   
010800     88  TRYCK-PF11                          VALUE 'J'.                   
010900                                                                          
011000 77  SLAGER-SW                   PIC X       VALUE 'N'.                   
011100     88  SLAGER-FINNS                        VALUE 'J'.                   
011200     88  SLAGER-SAKNAS                       VALUE 'N'.                   
011300                                                                          
011400 77  NDC-SW                      PIC X       VALUE 'N'.                   
011500     88  NDC-JA                              VALUE 'J'.                   
011600     88  NDC-NEJ                             VALUE 'N'.                   
011700                                                                          
011800 77  NDC-TYP-SW                  PIC XX      VALUE 'NN'.                  
011900     88  NDC-NA                              VALUE 'NA'.                  
012000     88  NDC-PF                              VALUE 'PF'.                  
012010     88  NDC-OTHERS                          VALUE 'NX'.                  
012100                                                                          
012200 77  VISA-NDC-SW                 PIC X       VALUE 'N'.                   
012300     88  VISA-NDC                            VALUE 'J'.                   
012400     88  VISA-INTE-NDC                       VALUE 'N'.                   
012500                                                                          
012600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012700     88  EGEN-MID                            VALUE '6215'.                
012800     88  GODK-MID                            VALUE '6211' '6212'          
012900                                                   '6213' '6214'          
013000                                                   '6215' '6216'          
013100                                                   '6217' '6218'          
013200                                                   '6219'.                
013300     88  HELP-MID                            VALUE '0551'.                
013400     EJECT                                                                
013500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013600 01  GENERELLA-SUBPROGRAM.                                                
013700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014300*01 -COPY WMEDAREA                                                        
014400     SKIP3                                                                
014500 01  MESSAGE-CODES.                                                       
014600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015100     03  ERR-MISSING-KEY         PIC X(3)    VALUE '005'.                 
015200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
015400     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015900     SKIP3                                                                
016000*01 -COPY WMSGINIT                                                        
016100     EJECT                                                                
016200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016300*                                                                         
016400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016500     SKIP3                                                                
016600*01  MID -COPY W6I21501                                                   
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016900     SKIP3                                                                
017000*01  -COPY WMSGAREA                                                       
017100     EJECT                                                                
017200     03  MOD REDEFINES MSG-AREA.                                          
017300*      05  -COPY W6O21501                                                 
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017600     SKIP3                                                                
017700*01  -COPY WMFSAREA                                                       
017800     EJECT                                                                
017900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018000*                                                                         
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018300     SKIP3                                                                
018400 01  NYCKLAR-TILL-DLI.                                                    
018500     03  W-IDARTNR-X.                                                     
018600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018700     03  W-IDDC-X.                                                        
018800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
018900     03  W-IDDC-K7-X.                                                     
019000         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
019100     03  W-W6D211KY-X.                                                    
019200         05  W-DAREGDAT-9KOMPL   PIC 9(08)   VALUE ZERO.                  
019300         05  W-TIKLOCK-9KOMPL    PIC S9(09)  VALUE ZERO COMP-3.           
019400     03  W-IDKVAINF-X.                                                    
019500         05  W-IDKVAINF          PIC  9(02)  VALUE ZERO.                  
019600     03  W-IDKVAINF-MIN-X.                                                
019700         05  W-IDKVAINF-MIN      PIC  9(02)  VALUE ZERO.                  
019800     03  W-IDKVAINF-MAX-X.                                                
019900         05  W-IDKVAINF-MAX      PIC  9(02)  VALUE ZERO.                  
020000     03  W-IDKVAINF-2.                                                    
020100         05  W-IDKVAINF2         PIC  9(02)  VALUE ZERO.                  
020200     03  W-KDSEGKEY-X.                                                    
020300         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
020400                                                                          
020500     03  W-IDDC-B6-X.                                                     
020600         05 W-IDDC-B6            PIC X(2).                                
020700                                                                          
020800     03  W-WDGXKEY-6331-X.                                                
020900         05  W-IDHTYP            PIC X(4)    VALUE '6331'.                
021000         05  W-IDDC-6331         PIC X(2)    VALUE '11'.                  
021100         05  W-6331-LOW          PIC X(24)   VALUE LOW-VALUE.             
021200                                                                          
021300     03  W-WDGXKEY-6332-X.                                                
021400         05  W-IDDC-6332         PIC X(2)    VALUE SPACE.                 
021500                                                                          
021600     03  W-WDGXKEY-6334-X.                                                
021700         05  W-IDDC-6334         PIC X(2)    VALUE SPACE.                 
021800                                                                          
021900*    --- STATUS-KOD FRÅN IMS                                              
022000 01  STATUS-WS                   PIC XX.                                  
022100     88  SEGMENT-FINNS                       VALUE '  '.                  
022200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022400     88  ROOT-BYTE                           VALUE 'GA'.                  
022500     SKIP2                                                                
022600 01  GODK-STATUSKODER.                                                    
022700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022800     SKIP3                                                                
022900 01  SSA1                        PIC X(128).                              
023000 01  SSA2                        PIC X(128).                              
023100 01  SSA3                        PIC X(128).                              
023200     EJECT                                                                
023300*    --- IMS FUNKTIONSKODER                                               
023400*01  -COPY W0003                                                          
023500     EJECT                                                                
023600*    ---  DLI INPUT-OUTPUT AREA                                           
023700                                                                          
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D2'.                        
023900 01  DLI-IO-W6D2-AREA.                                                    
024000     03  DLI-IO-W6D2 PIC X(1154).                                         
024100     03  IO-W6D201 REDEFINES DLI-IO-W6D2.                                 
024200*          05  -COPY W6D201                                               
024300     EJECT                                                                
024400     03  IO-W6D211 REDEFINES DLI-IO-W6D2.                                 
024500*          05  -COPY W6D211                                               
024600     EJECT                                                                
024700 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D221'.                      
024800 01  DLI-IO-W6D221-AREA.                                                  
024900     03  DLI-IO-W6D221 PIC X(471).                                        
025000     03  IO-W6D221 REDEFINES DLI-IO-W6D221.                               
025100*          05  -COPY W6D221                                               
025200     EJECT                                                                
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7'.                        
025400 01  DLI-IO-WDK7-AREA.                                                    
025500     03  DLI-IO-WDK7 PIC X(300).                                          
025600     03  IO-WDK701 REDEFINES DLI-IO-WDK7.                                 
025700*          05  -COPY WDK701                                               
025800     EJECT                                                                
025900     03  IO-WDK711 REDEFINES DLI-IO-WDK7.                                 
026000*          05  -COPY WDK711                                               
026100     EJECT                                                                
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
026300 01  DLI-IO-WDK6-AREA.                                                    
026400     03  DLI-IO-WDK6 PIC X(900).                                          
026500     03  IO-WDK601 REDEFINES DLI-IO-WDK6.                                 
026600*          05  -COPY WDK601                                               
026700     EJECT                                                                
026800     03  IO-WDK611 REDEFINES DLI-IO-WDK6.                                 
026900*          05  -COPY WDK611                                               
027000                                                                          
027100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
027200 01   DLI-IO-AREA-B601.                                                   
027300*     03  -COPY WDB601                                                    
027400                                                                          
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6331'.                     
027600 01  DLI-IO-WDGX6331.                                                     
027700*    03  -COPY WDGX6331.                                                  
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6332'.                     
027900 01  DLI-IO-WDGX6332.                                                     
028000*    03  -COPY WDGX6332.                                                  
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6334'.                     
028200 01  DLI-IO-WDGX6334.                                                     
028300*    03  -COPY WDGX6334.                                                  
028400     EJECT                                                                
028500 LINKAGE SECTION.                                                         
028600*01  -COPY W0009   -PRE MSG-                                              
028700*01  -COPY W0008   -PRE USEA-                                             
028800     05  FILLER                  PIC X.                                   
028900                                                                          
029000*01  -COPY W0008   -PRE W6D2A-                                            
029100     05  FILLER                  PIC X.                                   
029200                                                                          
029300*01  -COPY W0008   -PRE W6D2B-                                            
029400     05  FILLER                  PIC X.                                   
029500                                                                          
029600*01  -COPY W0008   -PRE WDK7-                                             
029700     05  FILLER                  PIC X.                                   
029800*01  -COPY W0008   -PRE WDK6-                                             
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100*01  -COPY W0008   -PRE WDB6-                                             
030200     05  FILLER                  PIC X.                                   
030300*01  -COPY W0008   -PRE WDR2-                                             
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008   -PRE WDR2ALT-                                          
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB W6D2A-PCB W6D2B-PCB           
031000                                   WDK7-PCB WDK6-PCB  WDB6-PCB            
031100                                   WDR2-PCB WDR2ALT-PCB.                  
031200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB W6D2A-PCB W6D2B-PCB           
031300                                   WDK7-PCB WDK6-PCB  WDB6-PCB            
031400                                   WDR2-PCB WDR2ALT-PCB.                  
031500                                                                          
031600     PERFORM IMS-GET-MSG                                                  
031700     IF SEGMENT-FINNS                                                     
031800       PERFORM A-INIT                                                     
031900       PERFORM B-KOLLA-NYCKLAR                                            
032000       IF NYCKLAR-OK                                                      
032100         IF MFS-UPDATE                                                    
032200           PERFORM G-KOLLA-INPUT                                          
032300           IF INDATA-OK                                                   
032400             PERFORM H-UPPDATERA                                          
032500             MOVE MSGI-IDDC TO W-IDDC                                     
032600           END-IF                                                         
032700         ELSE                                                             
032800           IF MFS-FIRST                                                   
032900             PERFORM C-FOERSTA-SIDA                                       
033000           ELSE                                                           
033100             IF MFS-NEXT                                                  
033200               PERFORM D-NAESTA-SIDA                                      
033300             ELSE                                                         
033400               PERFORM E-SAMMA-SIDA                                       
033500             END-IF                                                       
033600           END-IF                                                         
033700         END-IF                                                           
033800         IF INDATA-OK                                                     
033900***        IF MFS-NEXT                                                    
034000***          PERFORM FD-LAES-VISA-NEXT                                    
034100****       ELSE                                                           
034200             PERFORM F-LAES-VISA-INFO                                     
034300****       END-IF                                                         
034400         END-IF                                                           
034500       END-IF                                                             
034600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O21501 + 4                      
034700       PERFORM IMS-INSERT-MSG                                             
034800     END-IF                                                               
034900                                                                          
035000     MOVE ZERO TO RETURN-CODE                                             
035100     GOBACK                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 A-INIT SECTION.                                                          
035500     IF MSG-DUBBLA-TRANSKODER                                             
035600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I21501                 
035700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
035800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035900     ELSE                                                                 
036000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I21501                  
036100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
036200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036300     END-IF                                                               
036400                                                                          
036500     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
036600     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
036700     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
036800                                                                          
036900     MOVE LOW-VALUE        TO MSG-AREA                                    
037000     MOVE 'W6O215N1'       TO MFS-IDMOD                                   
037100     MOVE '6215'           TO MOD-IDTRANS                                 
037200     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
037300                                                                          
037400     IF EGEN-MID OR HELP-MID                                              
037500       CONTINUE                                                           
037600     ELSE                                                                 
037700       MOVE SPACE TO MFS-KDTRTYP                                          
037800       MOVE '7'   TO MFS-IDPFK                                            
037900     END-IF                                                               
038000     ACCEPT WS-DAGENS-TID   FROM TIME                                     
038100     ACCEPT WS-DAGENS-DATUM FROM DATE                                     
038200                                                                          
038300     .                                                                    
038400     EJECT                                                                
038500 B-KOLLA-NYCKLAR SECTION.                                                 
038600                                                                          
038700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
038800     MOVE '001'             TO MSGI-KDCALL                                
038900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039100     MOVE '6215'            TO MSGI-IDTRANS                               
039200     IF GODK-MID                                                          
039300       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
039400     END-IF                                                               
039500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039600                                                                          
039700     MOVE JA TO NYCKLAR-SW                                                
039800                                                                          
039900     IF MSGI-IDLAND-SPR = 'GB'                                            
040000       MOVE +2 TO SPRAK-IX                                                
040100       MOVE 'GB ' TO MED-IDSKYLT                                          
040200     ELSE                                                                 
040300       MOVE +1 TO SPRAK-IX                                                
040400       MOVE 'S  ' TO MED-IDSKYLT                                          
040500     END-IF                                                               
040600                                                                          
040700*    -- KONTROLL AV IDARTNR                                               
040800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
040900                                                                          
041000     IF MID-IDARTNR-IN NOT = ALL '+'                                      
041100       MOVE '7'         TO MFS-IDPFK                                      
041200       MOVE SPACE       TO MFS-KDTRTYP                                    
041300       MOVE JA          TO NYTT-ARTNR-SW                                  
041400     END-IF                                                               
041500     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
041600     IF MSGI-IDARTNR NUMERIC                                              
041700       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
041800       MOVE W-IDARTNR    TO MOD-IDARTNR-UT                                
041900     ELSE                                                                 
042000       MOVE NEJ TO NYCKLAR-SW                                             
042100     END-IF                                                               
042200                                                                          
042300*    -- KONTROLL AV IDKVAINF                                              
042400                                                                          
042500     MOVE MFS-RENSA-FAELT     TO MOD-IDKVAINF-IN                          
042600                                                                          
042700     IF GODK-MID                                                          
042800       IF MID-IDKVAINF-IN = ALL '+'                                       
042900         IF NYTT-ARTNR OR MFS-FIRST OR MFS-NEXT                           
043000           MOVE ZERO            TO WS-IDKVAINF                            
043100         ELSE                                                             
043200           MOVE MID-IDKVAINF-UT TO WS-IDKVAINF                            
043300         END-IF                                                           
043400       ELSE                                                               
043500         MOVE MID-IDKVAINF-IN   TO WS-IDKVAINF                            
043600       END-IF                                                             
043700     ELSE                                                                 
043800       MOVE ZERO                TO WS-IDKVAINF                            
043900     END-IF                                                               
044000     INSPECT WS-IDKVAINF REPLACING LEADING SPACE BY ZERO                  
044100                                                                          
044200*    -- KONTROLL AV TIREGDAT                                              
044300     MOVE MFS-RENSA-FAELT     TO MOD-TIREGDAT-IN                          
044400                                                                          
044500     IF GODK-MID                                                          
044600       IF MID-TIREGDAT-IN = ALL '+'                                       
044700         IF NYTT-ARTNR                                                    
044800           MOVE ZERO            TO WS-TIREGDAT                            
044900         ELSE                                                             
045000           IF MID-IDKVAINF-IN = ALL '+'                                   
045100             MOVE MID-TIREGDAT-UT TO WS-TIREGDAT                          
045200           ELSE                                                           
045300             MOVE ZERO            TO WS-TIREGDAT                          
045400           END-IF                                                         
045500         END-IF                                                           
045600       ELSE                                                               
045700         MOVE MID-TIREGDAT-IN   TO WS-TIREGDAT                            
045800       END-IF                                                             
045900     ELSE                                                                 
046000       MOVE ZERO                TO WS-TIREGDAT                            
046100     END-IF                                                               
046200     INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO                  
046300     IF WS-TIREGDAT NUMERIC                                               
046400       CONTINUE                                                           
046500     ELSE                                                                 
046600       MOVE NEJ TO NYCKLAR-SW                                             
046700     END-IF                                                               
046800                                                                          
046900*    -- KONTROLL AV IDDC-IN                                               
047000     IF GODK-MID                                                          
047100     MOVE MFS-RENSA-FAELT     TO MOD-IDDC-IN                              
047200       IF MID-IDDC-IN NOT = ALL '+'                                       
047300         MOVE '7'         TO MFS-IDPFK                                    
047400         MOVE SPACE       TO MFS-KDTRTYP                                  
047500           MOVE MID-IDDC-IN     TO WS-IDDC                                
047600                                   WS-IDDC-IN                             
047700                                   MOD-IDDC-UT                            
047800                                   W-IDDC                                 
047900                                   WS-SPAR-IDDC                           
048000       ELSE                                                               
048100         IF MID-IDDC-UT NOT = SPACE                                       
048200           MOVE MID-IDDC-UT     TO WS-IDDC                                
048300                                   WS-IDDC-IN                             
048400                                   MOD-IDDC-UT                            
048500                                   W-IDDC                                 
048600                                   WS-SPAR-IDDC                           
048700         ELSE                                                             
048800           MOVE MSGI-IDDC       TO WS-IDDC                                
048900                                   WS-IDDC-IN                             
049000                                   MOD-IDDC-UT                            
049100                                   W-IDDC                                 
049200                                   WS-SPAR-IDDC                           
049300         END-IF                                                           
049400       END-IF                                                             
049500       MOVE WS-SPAR-IDDC TO W-IDDC-B6                                     
049600       PERFORM IMS-GU-WDB601                                              
049700       IF SEGMENT-SAKNAS                                                  
049800         MOVE NEJ TO NYCKLAR-SW                                           
049900       ELSE                                                               
050000         IF DCS-NDC-NA                                                    
050100           MOVE JA   TO NDC-SW                                            
050200           MOVE 'NA' TO NDC-TYP-SW                                        
050300         END-IF                                                           
050400         IF DCS-NDC-PF                                                    
050500           MOVE JA   TO NDC-SW                                            
050600           MOVE 'PF' TO NDC-TYP-SW                                        
050700         END-IF                                                           
050710         IF DCS-NDC-OTHERS                                                
050720           MOVE JA   TO NDC-SW                                            
050730           MOVE 'NX' TO NDC-TYP-SW                                        
050740         END-IF                                                           
050800       END-IF                                                             
050900     ELSE                                                                 
051000       MOVE MSGI-IDDC       TO WS-IDDC                                    
051100                               WS-IDDC-IN                                 
051200                               MOD-IDDC-UT                                
051300                               W-IDDC                                     
051400                               WS-SPAR-IDDC                               
051500       MOVE WS-SPAR-IDDC TO W-IDDC-B6                                     
051600       PERFORM IMS-GU-WDB601                                              
051700       IF SEGMENT-SAKNAS                                                  
051800         MOVE NEJ TO NYCKLAR-SW                                           
051900       END-IF                                                             
052000                                                                          
052100     END-IF                                                               
052200                                                                          
052300*    -- KONTROLL AV SHOW-ALL                                              
052400     IF GODK-MID                                                          
052500       MOVE MFS-RENSA-FAELT       TO MOD-SHOW-ALL-IN                      
052600       IF MID-SHOW-ALL-IN NOT = ALL '+'                                   
052700         IF MID-SHOW-ALL-IN = 'N' OR 'J' OR 'Y'                           
052800           MOVE '7'         TO MFS-IDPFK                                  
052900           MOVE SPACE       TO MFS-KDTRTYP                                
053000           IF MID-SHOW-ALL-IN = 'J' OR 'Y'                                
053100             MOVE 'Y'             TO WS-SHOW-ALL                          
053200           ELSE                                                           
053300             MOVE MID-SHOW-ALL-IN TO WS-SHOW-ALL                          
053400           END-IF                                                         
053500           MOVE MID-SHOW-ALL-IN   TO MOD-SHOW-ALL-UT                      
053600         ELSE                                                             
053700           MOVE NEJ               TO NYCKLAR-SW                           
053800         END-IF                                                           
053900       ELSE                                                               
054000         IF MID-IDDC-IN NOT = ALL '+'                                     
054100           IF DCS-CDC                                                     
054200             MOVE 'N'             TO WS-SHOW-ALL                          
054300                                     MOD-SHOW-ALL-UT                      
054400           ELSE                                                           
054500             MOVE 'Y'             TO WS-SHOW-ALL                          
054600                                     MOD-SHOW-ALL-UT                      
054700           END-IF                                                         
054800         ELSE                                                             
054900           IF MID-SHOW-ALL-UT = 'Y' OR 'J' OR 'N'                         
055000             IF MID-SHOW-ALL-UT = 'J' OR 'Y'                              
055100               MOVE 'Y'             TO WS-SHOW-ALL                        
055200             ELSE                                                         
055300               MOVE MID-SHOW-ALL-UT TO WS-SHOW-ALL                        
055400             END-IF                                                       
055500             MOVE MID-SHOW-ALL-UT   TO MOD-SHOW-ALL-UT                    
055600                                                                          
055700           ELSE                                                           
055800             IF DCS-CDC                                                   
055900               MOVE 'N'             TO WS-SHOW-ALL                        
056000                                       MOD-SHOW-ALL-UT                    
056100             ELSE                                                         
056200               MOVE 'Y'             TO WS-SHOW-ALL                        
056300                                       MOD-SHOW-ALL-UT                    
056310             END-IF                                                       
056400           END-IF                                                         
056500         END-IF                                                           
056600       END-IF                                                             
056700     ELSE                                                                 
056800       IF DCS-CDC                                                         
056900         MOVE 'N'             TO WS-SHOW-ALL                              
057000                                 MOD-SHOW-ALL-UT                          
057100       ELSE                                                               
057200         MOVE 'Y'             TO WS-SHOW-ALL                              
057300                                 MOD-SHOW-ALL-UT                          
057400       END-IF                                                             
057500     END-IF                                                               
057600                                                                          
057700     IF GODK-MID OR NYCKLAR-OK                                            
057800*      MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                
057900       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
058000       MOVE WS-IDKVAINF  TO W-IDKVAINF                                    
058100                            MOD-IDKVAINF-UT                               
058200       INSPECT MOD-IDKVAINF-UT REPLACING LEADING ZERO BY SPACE            
058300       IF NYCKLAR-OK                                                      
058400         MOVE WS-TIREGDAT  TO W1-DAREGDAT                                 
058500                              MOD-TIREGDAT-UT                             
058600         IF WS-TIREGDAT NOT = ZERO                                        
058700           IF WS-TIREGDAT < 500000                                        
058800             MOVE 20       TO W1-DAREGDAT (1:2)                           
058900           ELSE                                                           
059000             IF WS-TIREGDAT < 999999                                      
059100               MOVE 19     TO W1-DAREGDAT (1:2)                           
059200             ELSE                                                         
059300               MOVE 99999999 TO W1-DAREGDAT                               
059400             END-IF                                                       
059500           END-IF                                                         
059600         END-IF                                                           
059700         COMPUTE W-DAREGDAT-9KOMPL = 99999999 - W1-DAREGDAT               
059800       END-IF                                                             
059900     ELSE                                                                 
060000       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
060100     END-IF                                                               
060200                                                                          
060300     IF NYCKLAR-FEL                                                       
060400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
060500       CALL WMEDKONV   USING MED-WMEDAREA                                 
060600       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
060700       PERFORM MFS-RENSA-FAELT-IN                                         
060800       PERFORM MFS-RENSA-FAELT-UT                                         
060900     END-IF                                                               
061000     .                                                                    
061100     EJECT                                                                
061200 C-FOERSTA-SIDA SECTION.                                                  
061300                                                                          
061400     MOVE INF-FIRST-PAGE           TO MED-IDMFSINF                        
061500     CALL WMEDKONV              USING MED-WMEDAREA                        
061600     MOVE MED-MFSINF               TO MOD-TEMFSINF                        
061700                                                                          
061800*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
061900     MOVE ZERO                     TO MOD-TIREGDAT-9KOMPL-ENTER           
062000     MOVE ZERO                     TO MOD-TIKLOCK-9KOMPL-ENTER            
062100     MOVE ZERO                     TO MOD-TIREGDAT-9KOMPL-NEXT            
062200     MOVE ZERO                     TO MOD-TIKLOCK-9KOMPL-NEXT             
062300     MOVE WS-IDDC-IN               TO MOD-LEVEL2-IDDC-NEXT                
062400     PERFORM MFS-RENSA-FAELT-IN                                           
062500                                                                          
062600     IF MID-TIREGDAT-IN = ALL '+'                                         
062700       MOVE 0                         TO W-DAREGDAT-9KOMPL (1:1)          
062710       IF MID-TIKLOCK-9KOMPL-ENTER NOT = ALL '+'                          
062800          MOVE MID-TIKLOCK-9KOMPL-ENTER  TO W-TIKLOCK-9KOMPL              
062810       ELSE                                                               
062820          MOVE ZERO                      TO W-TIKLOCK-9KOMPL              
062830       END-IF                                                             
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200 D-NAESTA-SIDA SECTION.                                                   
063300     IF MID-LEVEL2-IDDC-NEXT NOT = SPACE AND                              
063400        MID-LEVEL3-IDDC-NEXT  = SPACE                                     
063500       MOVE MID-TIREGDAT-9KOMPL-NEXT  TO W-DAREGDAT-9KOMPL                
063600       IF MID-TIREGDAT-9KOMPL-NEXT  (1:1) = 0                             
063700         MOVE 80                     TO W-DAREGDAT-9KOMPL (1:2)           
063800       ELSE                                                               
063900         MOVE 79                     TO W-DAREGDAT-9KOMPL (1:2)           
064000       END-IF                                                             
064100       MOVE MID-TIKLOCK-9KOMPL-NEXT   TO W-TIKLOCK-9KOMPL                 
064200       MOVE MID-LEVEL2-IDDC-NEXT      TO W-IDDC                           
064300                                         W-IDDC-6332                      
064400       PERFORM MFS-RENSA-FAELT-IN                                         
064500     ELSE                                                                 
064600       IF MID-LEVEL2-IDDC-NEXT NOT = SPACE AND                            
064700          MID-LEVEL3-IDDC-NEXT NOT = SPACE                                
064800                                                                          
064900         MOVE MID-TIREGDAT-9KOMPL-ENTER TO W-DAREGDAT-9KOMPL              
065000         IF MID-TIREGDAT-9KOMPL-ENTER (1:1) = 0                           
065100           MOVE 80                     TO W-DAREGDAT-9KOMPL (1:2)         
065200         ELSE                                                             
065300           MOVE 79                     TO W-DAREGDAT-9KOMPL (1:2)         
065400         END-IF                                                           
065500         MOVE MID-TIKLOCK-9KOMPL-ENTER TO W-TIKLOCK-9KOMPL                
065600         MOVE MID-LEVEL2-IDDC-NEXT     TO W-IDDC-6332                     
065700                                                                          
065800         MOVE MID-LEVEL3-IDDC-NEXT     TO W-IDDC-6334                     
065900                                                                          
066000         PERFORM MFS-RENSA-FAELT-IN                                       
066100       END-IF                                                             
066200     END-IF                                                               
066300     .                                                                    
066400     EJECT                                                                
066500 E-SAMMA-SIDA SECTION.                                                    
066600                                                                          
066700     IF EGEN-MID OR HELP-MID                                              
066800       IF MID-INPUT = ALL '+'                                             
066900         PERFORM MFS-RENSA-FAELT-IN                                       
067000       ELSE                                                               
067100         MOVE JA             TO TRYCK-PF11-SW                             
067200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
067300         CALL WMEDKONV    USING MED-WMEDAREA                              
067400         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
067500         PERFORM EA-MID-INDATA-TILL-MOD                                   
067600       END-IF                                                             
067700     ELSE                                                                 
067800       PERFORM MFS-RENSA-FAELT-IN                                         
067900     END-IF                                                               
068000                                                                          
068100     IF MID-IDKVAINF-IN NOT = ALL '+'                                     
068200        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
068300        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
068400        MOVE SPACE                 TO MOD-TIREGDAT-UT                     
068500     END-IF                                                               
068600                                                                          
068700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
068800        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
068900        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
069000     END-IF                                                               
069100     IF MID-IDDC-IN NOT = ALL '+'                                         
069200        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
069300        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
069400     END-IF                                                               
069500     IF MID-SHOW-ALL-IN NOT = ALL '+'                                     
069600        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
069700        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
069800     END-IF                                                               
069900                                                                          
070000     IF MID-TIREGDAT-IN = ALL '+'                                         
070100        MOVE MID-TIREGDAT-9KOMPL-ENTER TO W-DAREGDAT-9KOMPL               
070200        IF MID-TIREGDAT-9KOMPL-ENTER  (1:1) = 0                           
070300          MOVE 80                      TO W-DAREGDAT-9KOMPL(1:2)          
070400        ELSE                                                              
070500          MOVE 79                      TO W-DAREGDAT-9KOMPL(1:2)          
070600        END-IF                                                            
070700        MOVE MID-TIKLOCK-9KOMPL-ENTER  TO W-TIKLOCK-9KOMPL                
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 EA-MID-INDATA-TILL-MOD SECTION.                                          
071200                                                                          
071300     IF MID-IDDC NOT = ALL '+'                                            
071400        MOVE MID-IDDC              TO MOD-IDDC-UPPD                       
071500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-UPPD-ATTR                  
071600     ELSE                                                                 
071700        MOVE MFS-RENSA-FAELT       TO MOD-IDDC-UPPD                       
071800     END-IF                                                               
071900                                                                          
072000     IF MID-TISTADAT NOT = ALL '+'                                        
072100        MOVE MID-TISTADAT          TO MOD-TISTADAT-UPPD                   
072200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTADAT-UPPD-ATTR              
072300     ELSE                                                                 
072400        MOVE MFS-RENSA-FAELT       TO MOD-TISTADAT-UPPD                   
072500     END-IF                                                               
072600                                                                          
072700     IF MID-TISTODAT NOT = ALL '+'                                        
072800        MOVE MID-TISTODAT          TO MOD-TISTODAT-UPPD                   
072900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTODAT-UPPD-ATTR              
073000     ELSE                                                                 
073100        MOVE MFS-RENSA-FAELT       TO MOD-TISTODAT-UPPD                   
073200     END-IF                                                               
073300                                                                          
073400     IF MID-KVANTAL NOT = ALL '+'                                         
073500        MOVE MID-KVANTAL           TO MOD-KVANTAL-UPPD                    
073600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-UPPD-ATTR               
073700     ELSE                                                                 
073800        MOVE MFS-RENSA-FAELT       TO MOD-KVANTAL-UPPD                    
073900     END-IF                                                               
074000                                                                          
074100     IF MID-KVAVV-KVAL NOT = ALL '+'                                      
074200        MOVE MID-KVAVV-KVAL        TO MOD-KVAVV-KVAL-UPPD                 
074300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAVV-KVAL-UPPD-ATTR            
074400     ELSE                                                                 
074500        MOVE MFS-RENSA-FAELT       TO MOD-KVAVV-KVAL-UPPD                 
074600     END-IF                                                               
074700                                                                          
074800     IF MID-KVART-SKROT NOT = ALL '+'                                     
074900        MOVE MID-KVART-SKROT       TO MOD-KVART-SKROT-UPPD                
075000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-SKROT-UPPD-ATTR           
075100     ELSE                                                                 
075200        MOVE MFS-RENSA-FAELT       TO MOD-KVART-SKROT-UPPD                
075300     END-IF                                                               
075400                                                                          
075500     IF MID-KVART-RET NOT = ALL '+'                                       
075600        MOVE MID-KVART-RET         TO MOD-KVART-RET-UPPD                  
075700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-RET-UPPD-ATTR             
075800     ELSE                                                                 
075900        MOVE MFS-RENSA-FAELT       TO MOD-KVART-RET-UPPD                  
076000     END-IF                                                               
076100                                                                          
076200     IF MID-KVART-KJUST NOT = ALL '+'                                     
076300        MOVE MID-KVART-KJUST       TO MOD-KVART-KJUST-UPPD                
076400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-KJUST-UPPD-ATTR           
076500     ELSE                                                                 
076600        MOVE MFS-RENSA-FAELT       TO MOD-KVART-KJUST-UPPD                
076700     END-IF                                                               
076800                                                                          
076900     IF MID-BEINIT NOT = ALL '+'                                          
077000        MOVE MID-BEINIT            TO MOD-BEINIT-UPPD                     
077100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEINIT-UPPD-ATTR                
077200     ELSE                                                                 
077300        MOVE MFS-RENSA-FAELT       TO MOD-BEINIT-UPPD                     
077400     END-IF                                                               
077500                                                                          
077600     .                                                                    
077700     EJECT                                                                
077800 F-LAES-VISA-INFO SECTION.                                                
077900                                                                          
078000     PERFORM IMS-GU-W6D201                                                
078100                                                                          
078200     IF SEGMENT-FINNS                                                     
078300       MOVE 'Y'                  TO WS-FORSTA-POST                        
078400       MOVE 'N'                  TO WS-LEVEL-POST                         
078500       MOVE 'N'                  TO WS-FLFIN                              
078600       MOVE LOW-VALUE            TO W-IDKVAINF-MIN-X                      
078700       MOVE HIGH-VALUE           TO W-IDKVAINF-MAX-X                      
078800       IF WS-IDKVAINF > ZERO                                              
078900          MOVE W-IDKVAINF        TO W-IDKVAINF-MIN                        
079000                                    W-IDKVAINF-MAX                        
079100          MOVE ZERO              TO W-DAREGDAT-9KOMPL                     
079200       END-IF                                                             
079300                                                                          
079400       PERFORM FA-LAES-W6D211                                             
079500     ELSE                                                                 
079600       MOVE ERR-MISSING-KEY TO MED-IDMFSFEL                               
079700       CALL WMEDKONV     USING MED-WMEDAREA                               
079800       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
079900       PERFORM MFS-RENSA-FAELT-UT                                         
080000     END-IF                                                               
080100     .                                                                    
080200     EJECT                                                                
080300 FA-LAES-W6D211 SECTION.                                                  
080400                                                                          
080500     PERFORM IMS-GNP-W6D211                                               
080600                                                                          
080700     IF SEGMENT-FINNS                                                     
080800       IF INFO-IDKVAINF = 99                                              
080900         PERFORM IMS-GNP-W6D211                                           
081000       END-IF                                                             
081100     END-IF                                                               
081200                                                                          
081300     IF SEGMENT-FINNS                                                     
081400       MOVE INFO-DAREGDAT-9KOMPL      TO W-DAREGDAT-9KOMPL                
081500       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO MOD-TIREGDAT-9KOMPL-ENTER        
081600                                         MOD-TIREGDAT-9KOMPL-NEXT         
081700       MOVE INFO-TIKLOCK-9KOMPL       TO W-TIKLOCK-9KOMPL                 
081800                                         MOD-TIKLOCK-9KOMPL-ENTER         
081900                                         MOD-TIKLOCK-9KOMPL-NEXT          
082000                                                                          
082100       COMPUTE W1-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL              
082200       MOVE W1-DAREGDAT (3:6)   TO MOD-TIREGDAT-UT                        
082300       MOVE INFO-IDKVAINF       TO MOD-IDKVAINF-UT                        
082400       MOVE SPACE               TO MOD-LEVEL2-IDDC-NEXT                   
082500       MOVE SPACE               TO MOD-LEVEL3-IDDC-NEXT                   
082600                                                                          
082700       IF (MFS-NEXT AND MID-LEVEL3-IDDC-NEXT NOT = SPACE)                 
082800*******   (MFS-NEXT AND MID-LEVEL2-IDDC-NEXT NOT = SPACE)                 
082900         IF WS-SHOW-ALL = 'Y' OR 'J'                                      
083000           PERFORM FD-LAES-VISA-NEXT                                      
083100         ELSE                                                             
083200           PERFORM FB-LAES-DCDATA                                         
083300         END-IF                                                           
083400       ELSE                                                               
083500         PERFORM FB-LAES-DCDATA                                           
083600       END-IF                                                             
083700     ELSE                                                                 
083800       MOVE ERR-MISSING-KEY TO MED-IDMFSFEL                               
083900       CALL WMEDKONV     USING MED-WMEDAREA                               
084000       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
084100       PERFORM MFS-RENSA-FAELT-UT                                         
084200     END-IF                                                               
084300                                                                          
084400***  PERFORM FC-KOLLA-INTERN-NOT                                          
084500                                                                          
084600     MOVE LOW-VALUE               TO W-IDKVAINF-MIN-X                     
084700     PERFORM IMS-GNP-W6D211                                               
084800                                                                          
084900                                                                          
085000     IF SEGMENT-FINNS                                                     
085100       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO MOD-TIREGDAT-9KOMPL-NEXT         
085200       MOVE INFO-TIKLOCK-9KOMPL       TO MOD-TIKLOCK-9KOMPL-NEXT          
085300       IF MFS-UPDATE OR TRYCK-PF11-SW = JA                                
085400         CONTINUE                                                         
085500       ELSE                                                               
085600         MOVE INF-MORE-INFO-EXISTS      TO MED-IDMFSINF                   
085700         CALL WMEDKONV               USING MED-WMEDAREA                   
085800         MOVE MED-MFSINF                TO MOD-TEMFSINF                   
085900       END-IF                                                             
086000     END-IF                                                               
086100     .                                                                    
086200     EJECT                                                                
086300 FB-LAES-DCDATA SECTION.                                                  
086400     MOVE +1 TO DCIX                                                      
086500     MOVE WC-CDC-SE  TO W-IDDC                                            
086600*  VISA ALLTID DC CDC INFO PÅ RAD 1                                       
086700     PERFORM IMS-GU-WDK611                                                
086800     PERFORM IMS-GET-WDGX6331                                             
086900     MOVE W-IDDC            TO MOD-IDDC(DCIX)                             
087000     MOVE CLAG-KVLS         TO MOD-KVLS(DCIX)                             
087100     MOVE CLAG-KDLEVSP      TO MOD-KDLEVSP(DCIX)                          
087200     MOVE CLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL(DCIX)                     
087300     PERFORM IMS-GU-W6D221                                                
087400       IF SEGMENT-FINNS                                                   
087500         MOVE DCQ-TISTADAT          TO MOD-TISTADAT(DCIX)                 
087600         MOVE DCQ-TISTODAT          TO MOD-TISTODAT(DCIX)                 
087700         MOVE DCQ-KVANTAL           TO MOD-KVANTAL(DCIX)                  
087800         MOVE DCQ-KVAVV-KVAL        TO MOD-KVAVV-KVAL(DCIX)               
087900         MOVE DCQ-KVART-SKROT       TO MOD-KVART-SKROT(DCIX)              
088000         MOVE DCQ-KVART-RET         TO MOD-KVART-RET(DCIX)                
088100         MOVE DCQ-KVART-KJUST       TO MOD-KVART-KJUST(DCIX)              
088200         MOVE DCQ-BEINIT            TO MOD-BEINIT(DCIX)                   
088300       ELSE                                                               
088400         MOVE MFS-RENSA-FAELT  TO MOD-TISTADAT(DCIX)                      
088500                                  MOD-TISTODAT(DCIX)                      
088600                                  MOD-KVANTAL(DCIX)                       
088700                                  MOD-KVAVV-KVAL(DCIX)                    
088800                                  MOD-KVART-SKROT(DCIX)                   
088900                                  MOD-KVART-RET(DCIX)                     
089000                                  MOD-KVART-KJUST(DCIX)                   
089100                                  MOD-BEINIT(DCIX)                        
089200                                                                          
089300       END-IF                                                             
089400     PERFORM FC-KOLLA-INTERN-NOT                                          
089500     ADD +1 TO DCIX                                                       
089600     IF  CDC-SE                                                           
089700       IF MFS-NEXT AND MID-LEVEL3-IDDC-NEXT NOT = SPACE                   
089800         PERFORM FBAE-LAS-FRAM                                            
089900         MOVE 6332-IDDC TO W-IDDC                                         
090000                           WS-IDDC-RAD                                    
090100                           MOD-LEVEL2-IDDC-NEXT                           
090200       ELSE                                                               
090300         PERFORM IMS-GNP-WDGX6331                                         
090400         MOVE 6332-IDDC TO W-IDDC                                         
090500                           WS-IDDC-RAD                                    
090600                           MOD-LEVEL2-IDDC-NEXT                           
090700       END-IF                                                             
090800     ELSE                                                                 
090900       PERFORM FBA-LAES-DC-TRAD                                           
091000       MOVE 6332-IDDC TO W-IDDC                                           
091100                         MOD-LEVEL2-IDDC-NEXT                             
091200       PERFORM IMS-GU-WDK711                                              
091300       IF SEGMENT-FINNS                                                   
091400         MOVE JA  TO SLAGER-SW                                            
091500         IF WS-SHOW-ALL = 'Y'                                             
091600           MOVE W-IDDC            TO MOD-IDDC(DCIX)                       
091700           MOVE SLAG-KVLS         TO MOD-KVLS(DCIX)                       
091800           MOVE SLAG-KDLEVSP      TO MOD-KDLEVSP(DCIX)                    
091900           MOVE SLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL(DCIX)               
092000         ELSE                                                             
092100           MOVE W-IDDC            TO WS-IDDC-RAD                          
092200           PERFORM FBAA-SPARA-SLAG                                        
092300         END-IF                                                           
092400       ELSE                                                               
092500         MOVE NEJ TO SLAGER-SW                                            
092600         MOVE W-IDDC            TO MOD-IDDC(DCIX)                         
092700         MOVE W-IDDC            TO WS-IDDC-RAD                            
092800         MOVE MFS-RENSA-FAELT   TO MOD-KVLS(DCIX)                         
092900                                   MOD-KDLEVSP(DCIX)                      
093000                                   MOD-KVSPARR-KVAL(DCIX)                 
093100       END-IF                                                             
093200       PERFORM IMS-GU-W6D221                                              
093300       IF SEGMENT-FINNS                                                   
093400         IF SLAGER-FINNS AND DCQ-TISTODAT >  0                            
093500           MOVE 'Y'                   TO WS-FLFIN                         
093600         END-IF                                                           
093700         IF SLAGER-SAKNAS                                                 
093800           MOVE 'N'                   TO WS-FLFIN                         
093900         END-IF                                                           
094000       ELSE                                                               
094100         IF SLAGER-FINNS                                                  
094200           MOVE 'N'                   TO WS-FLFIN                         
094300         END-IF                                                           
094400       END-IF                                                             
094500       IF SEGMENT-FINNS                                                   
094600         IF WS-SHOW-ALL = 'Y'                                             
094700           MOVE DCQ-TISTADAT          TO MOD-TISTADAT(DCIX)               
094800           MOVE DCQ-TISTODAT          TO MOD-TISTODAT(DCIX)               
094900           MOVE DCQ-KVANTAL           TO MOD-KVANTAL(DCIX)                
095000           MOVE DCQ-KVAVV-KVAL        TO MOD-KVAVV-KVAL(DCIX)             
095100           MOVE DCQ-KVART-SKROT       TO MOD-KVART-SKROT(DCIX)            
095200           MOVE DCQ-KVART-RET         TO MOD-KVART-RET(DCIX)              
095300           MOVE DCQ-KVART-KJUST       TO MOD-KVART-KJUST(DCIX)            
095400           MOVE DCQ-BEINIT            TO MOD-BEINIT(DCIX)                 
095500           MOVE MFS-RENSA-FAELT       TO MOD-FLFIN (DCIX)                 
095600           MOVE MFS-RENSA-FAELT       TO MOD-FLLEV (DCIX)                 
095700         ELSE                                                             
095800           IF WS-LEVEL-POST = 'N'                                         
095900             MOVE DCQ-TISTADAT          TO WS-TISTADAT                    
096000             MOVE DCQ-TISTODAT          TO WS-TISTODAT                    
096100             MOVE DCQ-BEINIT            TO WS-BEINIT                      
096200           END-IF                                                         
096300           PERFORM FBAB-SPARA-W6D221                                      
096400         END-IF                                                           
096500       ELSE                                                               
096600         MOVE MFS-RENSA-FAELT  TO MOD-TISTADAT(DCIX)                      
096700                                  MOD-TISTODAT(DCIX)                      
096800                                  MOD-KVANTAL(DCIX)                       
096900                                  MOD-KVAVV-KVAL(DCIX)                    
097000                                  MOD-KVART-SKROT(DCIX)                   
097100                                  MOD-KVART-RET(DCIX)                     
097200                                  MOD-KVART-KJUST(DCIX)                   
097300                                  MOD-BEINIT(DCIX)                        
097400                                                                          
097500                                                                          
097600       END-IF                                                             
097700       PERFORM FC-KOLLA-INTERN-NOT                                        
097800       IF WS-SHOW-ALL = 'Y'                                               
097900         ADD +1 TO DCIX                                                   
098000       END-IF                                                             
098100       IF POST-LEVEL-3                                                    
098200         MOVE 6334-IDDC TO W-IDDC-6334                                    
098300                           W-IDDC                                         
098400         MOVE 'FÖRSTA UNIK LÄSNING '  TO WS-SECTION                       
098500         PERFORM IMS-GET-WDGX6334-UNIK                                    
098600       ELSE                                                               
098700         IF VISA-NDC                                                      
098800*** HÄMTA NÄSTA LEVEL2 NDC                                                
098900           PERFORM FBAD-NASTA-NDC                                         
099000                                                                          
099100         ELSE                                                             
099200*** HÄMTA FÖRSTA LEVEL 3 UNDER LEVEL 2                                    
099300           MOVE 'FÖRSTA GNP-WDGX6334 '  TO WS-SECTION                     
099400           PERFORM IMS-GNP-WDGX6334                                       
099500           IF SEGMENT-FINNS                                               
099600             MOVE 6334-IDDC TO W-IDDC                                     
099700           END-IF                                                         
099800         END-IF                                                           
099900       END-IF                                                             
100000     END-IF                                                               
100100*** LOOP **********                                                       
100200     PERFORM UNTIL DCIX > DCMAX OR SEGMENT-SAKNAS                         
100300       PERFORM IMS-GU-WDK711                                              
100400       IF SEGMENT-FINNS                                                   
100500         MOVE JA  TO SLAGER-SW                                            
100600       ELSE                                                               
100700         MOVE NEJ TO SLAGER-SW                                            
100800       END-IF                                                             
100900       IF SEGMENT-FINNS                                                   
101000         IF WS-SHOW-ALL = 'Y'                                             
101100           MOVE W-IDDC            TO MOD-IDDC(DCIX)                       
101200           MOVE SLAG-KVLS         TO MOD-KVLS(DCIX)                       
101300           MOVE SLAG-KDLEVSP      TO MOD-KDLEVSP(DCIX)                    
101400           MOVE SLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL(DCIX)               
101500         ELSE                                                             
101600           PERFORM FBAA-SPARA-SLAG                                        
101700         END-IF                                                           
101800*        LÄS MED DEN HÄMTADE DC TRÄD INFO                                 
101900         PERFORM IMS-GU-W6D221                                            
102000         IF SEGMENT-FINNS                                                 
102100           IF SLAGER-FINNS AND DCQ-TISTODAT >  0                          
102200             MOVE 'Y'                   TO WS-FLFIN                       
102300           END-IF                                                         
102400         ELSE                                                             
102500           IF SLAGER-FINNS                                                
102600             MOVE 'N'                   TO WS-FLFIN                       
102700           END-IF                                                         
102800         END-IF                                                           
102900         IF SEGMENT-FINNS                                                 
103000           IF WS-SHOW-ALL = 'Y'                                           
103100             MOVE DCQ-TISTADAT          TO MOD-TISTADAT(DCIX)             
103200             MOVE DCQ-TISTODAT          TO MOD-TISTODAT(DCIX)             
103300             MOVE DCQ-KVANTAL           TO MOD-KVANTAL(DCIX)              
103400             MOVE DCQ-KVAVV-KVAL        TO MOD-KVAVV-KVAL(DCIX)           
103500             MOVE DCQ-KVART-SKROT       TO MOD-KVART-SKROT(DCIX)          
103600             MOVE DCQ-KVART-RET         TO MOD-KVART-RET(DCIX)            
103700             MOVE DCQ-KVART-KJUST       TO MOD-KVART-KJUST(DCIX)          
103800             MOVE DCQ-BEINIT            TO MOD-BEINIT(DCIX)               
103900           ELSE                                                           
104000             IF WS-LEVEL-POST = 'N'                                       
104100               MOVE DCQ-TISTADAT          TO WS-TISTADAT                  
104200               MOVE DCQ-TISTODAT          TO WS-TISTODAT                  
104300               MOVE DCQ-BEINIT            TO WS-BEINIT                    
104400             END-IF                                                       
104500             PERFORM FBAB-SPARA-W6D221                                    
104600           END-IF                                                         
104700         ELSE                                                             
104800           IF WS-SHOW-ALL = 'Y'                                           
104900             MOVE MFS-RENSA-FAELT  TO MOD-TISTADAT(DCIX)                  
105000                                      MOD-TISTODAT(DCIX)                  
105100                                      MOD-KVANTAL(DCIX)                   
105200                                      MOD-KVAVV-KVAL(DCIX)                
105300                                      MOD-KVART-SKROT(DCIX)               
105400                                      MOD-KVART-RET(DCIX)                 
105500                                      MOD-KVART-KJUST(DCIX)               
105600                                      MOD-BEINIT(DCIX)                    
105700                                      MOD-FLLEV (DCIX)                    
105800                                      MOD-FLFIN (DCIX)                    
105900           END-IF                                                         
106000         END-IF                                                           
106100       ELSE                                                               
106200         MOVE W-IDDC             TO MOD-IDDC(DCIX)                        
106300         MOVE MFS-RENSA-FAELT    TO MOD-KVLS(DCIX)                        
106400                                    MOD-KDLEVSP(DCIX)                     
106500                                    MOD-KVSPARR-KVAL(DCIX)                
106600                                    MOD-TISTADAT(DCIX)                    
106700                                    MOD-TISTODAT(DCIX)                    
106800                                    MOD-KVANTAL(DCIX)                     
106900                                    MOD-KVAVV-KVAL(DCIX)                  
107000                                    MOD-KVART-SKROT(DCIX)                 
107100                                    MOD-KVART-RET(DCIX)                   
107200                                    MOD-KVART-KJUST(DCIX)                 
107300                                    MOD-BEINIT(DCIX)                      
107400                                    MOD-FL-DC-INFO(DCIX)                  
107500                                    MOD-FLLEV     (DCIX)                  
107600                                    MOD-FLFIN     (DCIX)                  
107700       END-IF                                                             
107800                                                                          
107900* LÄS NOTERING TEXT                                                       
108000                                                                          
108100       PERFORM FC-KOLLA-INTERN-NOT                                        
108200                                                                          
108300* LÄS NÄSTA DC TRÄD                                                       
108400       IF CDC-SE                                                          
108500*       HÄMTA FÖRSTA DC TRÄD EFTER DC 11                                  
108600        PERFORM IMS-GNP-WDGX6331                                          
108700        IF ROOT-BYTE                                                      
108800          MOVE '  ' TO STATUS-WS                                          
108900        END-IF                                                            
109000        IF SEGMENT-FINNS                                                  
109100          MOVE 6332-IDDC TO W-IDDC                                        
109200                                                                          
109300          IF WDR2-SEG-NAME-FB  = 'WDGX6332'                               
109400            IF WS-SHOW-ALL = 'N'                                          
109500              PERFORM FBAC-FLYTTA-TILL-MOD                                
109600              MOVE W-IDDC  TO WS-IDDC-RAD                                 
109700            END-IF                                                        
109800            MOVE 6332-IDDC TO MOD-LEVEL2-IDDC-NEXT                        
109900            MOVE SPACE     TO MOD-LEVEL3-IDDC-NEXT                        
110000          ELSE                                                            
110100            MOVE 6332-IDDC TO MOD-LEVEL3-IDDC-NEXT                        
110200          END-IF                                                          
110300        END-IF                                                            
110400       ELSE                                                               
110500         IF POST-LEVEL-3                                                  
110600           MOVE 'GE' TO STATUS-WS                                         
110700           MOVE 'ANDRA  UNIK LÄSNING '  TO WS-SECTION                     
110800         ELSE                                                             
110900           IF VISA-NDC                                                    
111000             PERFORM FBAD-NASTA-NDC                                       
111100           ELSE                                                           
111200             MOVE 'ANDRA WDGX6334      '  TO WS-SECTION                   
111300             PERFORM IMS-GNP-WDGX6334                                     
111400           END-IF                                                         
111500         END-IF                                                           
111600         IF SEGMENT-FINNS                                                 
111700           IF VISA-NDC                                                    
111800             MOVE 6332-IDDC TO W-IDDC                                     
111900           ELSE                                                           
112000             MOVE 6334-IDDC TO W-IDDC                                     
112100                               MOD-LEVEL3-IDDC-NEXT                       
112200           END-IF                                                         
112300         ELSE                                                             
112400           IF WS-SHOW-ALL = 'N'                                           
112500             PERFORM FBAC-FLYTTA-TILL-MOD                                 
112600           END-IF                                                         
112700         END-IF                                                           
112800       END-IF                                                             
112900       IF SEGMENT-FINNS AND WS-SHOW-ALL = 'Y'                             
113000         ADD +1 TO DCIX                                                   
113100       END-IF                                                             
113200                                                                          
113300     END-PERFORM                                                          
113400*** SLUT PÅ LOOP **********                                               
113500                                                                          
113600     IF SEGMENT-SAKNAS                                                    
113700       MOVE SPACE        TO MOD-LEVEL3-IDDC-NEXT                          
113800***    MOVE WS-SPAR-IDDC TO MOD-LEVEL2-IDDC-NEXT                          
113900     ELSE                                                                 
114000       MOVE 6332-IDDC TO MOD-LEVEL3-IDDC-NEXT                             
114100     END-IF                                                               
114200     IF SEGMENT-SAKNAS AND DCIX = 2                                       
114300       IF WS-SHOW-ALL = 'N'                                               
114400         PERFORM FBAC-FLYTTA-TILL-MOD                                     
114500       END-IF                                                             
114600     ELSE                                                                 
114700       IF WS-SHOW-ALL = 'N' AND SEGMENT-SAKNAS AND CDC-SE                 
114800         PERFORM FBAC-FLYTTA-TILL-MOD                                     
114900       END-IF                                                             
115000     END-IF                                                               
115100     .                                                                    
115200     EJECT                                                                
115300 FBA-LAES-DC-TRAD SECTION.                                                
115400     MOVE ' FBA-LAES-DC ' TO WS-SECTION                                   
115500     MOVE 'N' TO POST-LEVEL-SW                                            
115600     MOVE NEJ TO VISA-NDC-SW                                              
115700     MOVE ' IMS-GET-WDGX6331       ' TO WS-SECTION                        
115800     PERFORM IMS-GET-WDGX6331                                             
115900     IF SEGMENT-FINNS                                                     
116000       MOVE ' IMS-GHU-WDGX6332       ' TO WS-SECTION                      
116100       MOVE WS-SPAR-IDDC TO W-IDDC-6332                                   
116200       PERFORM IMS-GHU-WDGX6332                                           
116300       IF SEGMENT-SAKNAS                                                  
116400        MOVE ' IMS-GET-WDGX6331       ' TO WS-SECTION                     
116500        PERFORM IMS-GET-WDGX6331                                          
116600***     DC SAKNAS PÅ LEVEL 2                                              
116700        MOVE ' IMS-GNP-WDGX6332       ' TO WS-SECTION                     
116800        PERFORM IMS-GNP-WDGX6332                                          
116900        PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                        
117000****      IF SEGMENT-FINNS                                                
117100            MOVE 6332-IDDC TO W-IDDC-6332                                 
117200                              W-IDDC                                      
117300            MOVE WS-SPAR-IDDC TO W-IDDC-6334                              
117400            MOVE ' IMS-GNP-WDGX6334-UNIK  ' TO WS-SECTION                 
117500            PERFORM IMS-GET-WDGX6334-UNIK                                 
117600            IF SEGMENT-FINNS                                              
117700              PERFORM UNTIL SEGMENT-SAKNAS OR                             
117800                            ROOT-BYTE OR POST-FINNS                       
117900                IF WS-SPAR-IDDC = 6334-IDDC                               
118000                  MOVE JA TO POST-FINNS-SW                                
118100                             POST-LEVEL-SW                                
118200                ELSE                                                      
118300                  MOVE ' IMS-GNP-WDGX6334 LOOP  ' TO WS-SECTION           
118400                END-IF                                                    
118500              END-PERFORM                                                 
118600            ELSE                                                          
118700              MOVE ' IMS-GNP-WDGX6332 LOOP  ' TO WS-SECTION               
118800              PERFORM IMS-GNP-WDGX6332                                    
118900            END-IF                                                        
119000        END-PERFORM                                                       
119100       ELSE                                                               
119200         IF NDC-JA                                                        
119300           IF MSGI-IDUSER =  6332-IDUSER(1) OR                            
119400              MSGI-IDUSER =  6332-IDUSER(2) OR                            
119500              MSGI-IDUSER =  6332-IDUSER(3) OR                            
119600              MSGI-IDUSER =  6332-IDUSER(4)                               
119700             MOVE JA TO VISA-NDC-SW                                       
119800**** LÄS FRAM TILL RÄTT NDC FÖR ATT KUNA GÖRA GNP ***                     
119900             PERFORM IMS-GET-WDGX6331                                     
120000             PERFORM IMS-GNP-WDGX6332                                     
120100             PERFORM UNTIL SEGMENT-SAKNAS OR                              
120200                           6332-IDDC = WS-SPAR-IDDC                       
120300               PERFORM IMS-GNP-WDGX6332                                   
120400             END-PERFORM                                                  
120500           END-IF                                                         
120600         END-IF                                                           
120700       END-IF                                                             
120800     ELSE                                                                 
120900       MOVE 'SEG WDGX6331 SAKNAS' TO FELTEXT                              
121000       CALL FELLOG                                                        
121100     END-IF                                                               
121200     .                                                                    
121300     EJECT                                                                
121400 FBAA-SPARA-SLAG SECTION.                                                 
121500     COMPUTE WS-KVLS            = WS-KVLS         + SLAG-KVLS             
121600     COMPUTE WS-KDLEVSP         = WS-KDLEVSP      + SLAG-KDLEVSP          
121700     COMPUTE WS-KVSPARR-KVAL    =                                         
121800                          WS-KVSPARR-KVAL + SLAG-KVSPARR-KVAL             
121900     MOVE SLAG-KDLEVSP          TO    WS-KDLEVSP                          
122000     IF WS-FORSTA-POST = 'N'                                              
122100**** ANDRA POSTEN,  DÅ FINNS DET LEVEL 3 POSTER                           
122200       MOVE 'Y' TO WS-LEVEL-POST                                          
122300     END-IF                                                               
122400     IF WS-FORSTA-POST = 'Y'                                              
122500***  FÖRSTA POSTEN                                                        
122600       MOVE 'N' TO WS-FORSTA-POST                                         
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000 FBAB-SPARA-W6D221 SECTION.                                               
123100     COMPUTE WS-KVANTAL       = WS-KVANTAL      +  DCQ-KVANTAL            
123200     COMPUTE WS-KVAVV-KVAL    = WS-KVAVV-KVAL   +  DCQ-KVAVV-KVAL         
123300     COMPUTE WS-KVART-SKROT   = WS-KVART-SKROT  +  DCQ-KVART-SKROT        
123400     COMPUTE WS-KVART-RET     = WS-KVART-RET    +  DCQ-KVART-RET          
123500     COMPUTE WS-KVART-KJUST   = WS-KVART-KJUST  +  DCQ-KVART-KJUST        
123600     MOVE NEJ                   TO SLAGER-SW                              
123700     .                                                                    
123800     EJECT                                                                
123900 FBAC-FLYTTA-TILL-MOD SECTION.                                            
124000     IF WS-LEVEL-POST = 'Y'                                               
124100       MOVE 'Y'                 TO MOD-FLLEV(DCIX)                        
124200     ELSE                                                                 
124300       MOVE 'N'                 TO MOD-FLLEV(DCIX)                        
124400     END-IF                                                               
124500     MOVE WS-IDDC-RAD           TO MOD-IDDC(DCIX)                         
124600     MOVE WS-KVLS               TO MOD-KVLS(DCIX)                         
124700     MOVE WS-KDLEVSP            TO MOD-KDLEVSP(DCIX)                      
124800     MOVE WS-KVSPARR-KVAL       TO MOD-KVSPARR-KVAL(DCIX)                 
124900                                                                          
125000     IF WS-TISTADAT = ZERO                                                
125100       MOVE MFS-RENSA-FAELT     TO MOD-TISTADAT(DCIX)                     
125200     ELSE                                                                 
125300       MOVE WS-TISTADAT         TO MOD-TISTADAT(DCIX)                     
125400     END-IF                                                               
125500                                                                          
125600     IF WS-TISTODAT = ZERO                                                
125700       MOVE MFS-RENSA-FAELT     TO MOD-TISTODAT(DCIX)                     
125800     ELSE                                                                 
125900       MOVE WS-TISTODAT         TO MOD-TISTODAT(DCIX)                     
126000     END-IF                                                               
126100     IF WS-KVANTAL = ZERO                                                 
126200       MOVE MFS-RENSA-FAELT     TO MOD-KVANTAL(DCIX)                      
126300     ELSE                                                                 
126400       MOVE WS-KVANTAL          TO MOD-KVANTAL(DCIX)                      
126500     END-IF                                                               
126600     IF WS-KVAVV-KVAL = ZERO                                              
126700       MOVE MFS-RENSA-FAELT     TO MOD-KVAVV-KVAL(DCIX)                   
126800     ELSE                                                                 
126900       MOVE WS-KVAVV-KVAL       TO MOD-KVAVV-KVAL(DCIX)                   
127000     END-IF                                                               
127100     IF WS-KVART-SKROT = ZERO                                             
127200       MOVE MFS-RENSA-FAELT     TO MOD-KVART-SKROT(DCIX)                  
127300     ELSE                                                                 
127400       MOVE WS-KVART-SKROT      TO MOD-KVART-SKROT(DCIX)                  
127500     END-IF                                                               
127600     IF WS-KVART-RET = ZERO                                               
127700       MOVE MFS-RENSA-FAELT     TO MOD-KVART-RET(DCIX)                    
127800     ELSE                                                                 
127900       MOVE WS-KVART-RET        TO MOD-KVART-RET(DCIX)                    
128000     END-IF                                                               
128100     IF WS-KVART-KJUST ZERO                                               
128200       MOVE MFS-RENSA-FAELT     TO MOD-KVART-KJUST(DCIX)                  
128300     ELSE                                                                 
128400       MOVE WS-KVART-KJUST      TO MOD-KVART-KJUST(DCIX)                  
128500     END-IF                                                               
128600     MOVE WS-BEINIT             TO MOD-BEINIT(DCIX)                       
128700     MOVE WS-FL-DC-INFO         TO MOD-FL-DC-INFO(DCIX)                   
128800     MOVE WS-FLFIN              TO MOD-FLFIN     (DCIX)                   
128900                                                                          
129000     ADD +1 TO DCIX                                                       
129100     INITIALIZE  WS-KVLS                                                  
129200     INITIALIZE  WS-KDLEVSP                                               
129300     INITIALIZE  WS-KVSPARR-KVAL                                          
129400     INITIALIZE  WS-TISTADAT                                              
129500     INITIALIZE  WS-TISTODAT                                              
129600     INITIALIZE  WS-KVANTAL                                               
129700     INITIALIZE  WS-KVAVV-KVAL                                            
129800     INITIALIZE  WS-KVART-SKROT                                           
129900     INITIALIZE  WS-KVART-RET                                             
130000     INITIALIZE  WS-KVART-KJUST                                           
130100     INITIALIZE  WS-BEINIT                                                
130200     INITIALIZE  WS-FL-DC-INFO                                            
130300     MOVE 'Y' TO WS-FORSTA-POST                                           
130400     MOVE 'N' TO WS-FLFIN                                                 
130500     MOVE 'N' TO WS-LEVEL-POST                                            
130600     .                                                                    
130700     EJECT                                                                
130800 FBAD-NASTA-NDC SECTION.                                                  
130900     PERFORM IMS-GNP-WDGX6332                                             
131000     IF SEGMENT-FINNS                                                     
131100       MOVE 6332-IDDC TO W-IDDC-B6                                        
131200                                                                          
131300       PERFORM IMS-GU-WDB601                                              
131400       IF DCS-KDDC = NDC-TYP-SW                                           
131500         MOVE 6332-IDDC TO W-IDDC                                         
131600       END-IF                                                             
131700       PERFORM UNTIL SEGMENT-SAKNAS OR DCS-KDDC = NDC-TYP-SW              
131800         PERFORM IMS-GNP-WDGX6332                                         
131900         IF SEGMENT-FINNS                                                 
132000           MOVE 6332-IDDC TO W-IDDC-B6                                    
132100           PERFORM IMS-GU-WDB601                                          
132200           IF DCS-KDDC = NDC-TYP-SW                                       
132300             MOVE 6332-IDDC TO W-IDDC                                     
132400           END-IF                                                         
132500         END-IF                                                           
132600       END-PERFORM                                                        
132700     END-IF                                                               
132800     .                                                                    
132900     EJECT                                                                
133000 FBAE-LAS-FRAM SECTION.                                                   
133100     IF MID-LEVEL2-IDDC-NEXT  NOT = SPACE                                 
133200       MOVE MID-LEVEL2-IDDC-NEXT TO W-IDDC-6332                           
133300                                    W-IDDC                                
133400                                    MOD-LEVEL2-IDDC-NEXT                  
133500     END-IF                                                               
133600                                                                          
133700     IF MID-LEVEL3-IDDC-NEXT NOT = SPACE                                  
133800       MOVE MID-LEVEL3-IDDC-NEXT TO W-IDDC                                
133900                                    W-IDDC-6334                           
134000     END-IF                                                               
134100     PERFORM IMS-GET-WDGX6331                                             
134200     PERFORM UNTIL SEGMENT-SAKNAS OR 6332-IDDC = W-IDDC                   
134300       PERFORM IMS-GNP-WDGX6331                                           
134400     END-PERFORM                                                          
134500     .                                                                    
134600     EJECT                                                                
134700 FC-KOLLA-INTERN-NOT SECTION.                                             
134800                                                                          
134900     MOVE 99 TO W-IDKVAINF2                                               
135000     MOVE +1 TO INDX                                                      
135100     PERFORM IMS-GU-W6D211-B                                              
135200     IF SEGMENT-FINNS                                                     
135300       PERFORM IMS-GNP-W6D221                                             
135400       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > DCMAX                       
135500         IF DCQ-IDDC = W-IDDC                                             
135600           MOVE 'X' TO MOD-FL-DC-INFO(DCIX)                               
135700                       WS-FL-DC-INFO                                      
135800           ADD +13  TO INDX                                               
135900         ELSE                                                             
136000           ADD +1   TO INDX                                               
136100           PERFORM IMS-GNP-W6D221                                         
136200         END-IF                                                           
136300       END-PERFORM                                                        
136400     END-IF                                                               
136500     .                                                                    
136600     EJECT                                                                
136700 FD-LAES-VISA-NEXT  SECTION.                                              
136800     MOVE +1 TO DCIX                                                      
136900*    LÄGG ALLTID UT CDC RADEN NÄR NY FULLSIDA VISAS                       
137000     MOVE WC-CDC-SE  TO W-IDDC                                            
137100*    VISA ALLTID DC CDC INFO PÅ RAD 1                                     
137200       PERFORM IMS-GU-WDK611                                              
137300       PERFORM IMS-GET-WDGX6331                                           
137400       MOVE W-IDDC            TO MOD-IDDC(DCIX)                           
137500       MOVE CLAG-KVLS         TO MOD-KVLS(DCIX)                           
137600       MOVE CLAG-KDLEVSP      TO MOD-KDLEVSP(DCIX)                        
137700       MOVE CLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL(DCIX)                   
137800       PERFORM IMS-GU-W6D221                                              
137900         IF SEGMENT-FINNS                                                 
138000           MOVE DCQ-TISTADAT          TO MOD-TISTADAT(DCIX)               
138100           MOVE DCQ-TISTODAT          TO MOD-TISTODAT(DCIX)               
138200           MOVE DCQ-KVANTAL           TO MOD-KVANTAL(DCIX)                
138300           MOVE DCQ-KVAVV-KVAL        TO MOD-KVAVV-KVAL(DCIX)             
138400           MOVE DCQ-KVART-SKROT       TO MOD-KVART-SKROT(DCIX)            
138500           MOVE DCQ-KVART-RET         TO MOD-KVART-RET(DCIX)              
138600           MOVE DCQ-KVART-KJUST       TO MOD-KVART-KJUST(DCIX)            
138700           MOVE DCQ-BEINIT            TO MOD-BEINIT(DCIX)                 
138800         ELSE                                                             
138900           MOVE MFS-RENSA-FAELT  TO MOD-TISTADAT(DCIX)                    
139000                                    MOD-TISTODAT(DCIX)                    
139100                                    MOD-KVANTAL(DCIX)                     
139200                                    MOD-KVAVV-KVAL(DCIX)                  
139300                                    MOD-KVART-SKROT(DCIX)                 
139400                                    MOD-KVART-RET(DCIX)                   
139500                                    MOD-KVART-KJUST(DCIX)                 
139600                                    MOD-BEINIT(DCIX)                      
139700                                                                          
139800         END-IF                                                           
139900       PERFORM FC-KOLLA-INTERN-NOT                                        
140000                                                                          
140100     IF MID-LEVEL2-IDDC-NEXT  NOT = SPACE                                 
140200       MOVE MID-LEVEL2-IDDC-NEXT TO W-IDDC-6332                           
140300                                    W-IDDC                                
140400                                    MOD-LEVEL2-IDDC-NEXT                  
140500     END-IF                                                               
140600                                                                          
140700     IF MID-LEVEL3-IDDC-NEXT NOT = SPACE                                  
140800       MOVE MID-LEVEL3-IDDC-NEXT TO W-IDDC                                
140900                                    W-IDDC-6334                           
141000     END-IF                                                               
141100                                                                          
141200     PERFORM IMS-GET-WDGX6331                                             
141300*    'LÄS FRAM TILL DC ' W-IDDC                                           
141400     PERFORM UNTIL SEGMENT-SAKNAS OR 6332-IDDC = W-IDDC                   
141500       PERFORM IMS-GNP-WDGX6331                                           
141600     END-PERFORM                                                          
141700                                                                          
141800     IF SEGMENT-SAKNAS                                                    
141900       CALL FELLOG                                                        
142000     END-IF                                                               
142100                                                                          
142200     MOVE 'IMS-GU-WDK711        ' TO WS-SECTION                           
142300     PERFORM UNTIL SEGMENT-SAKNAS OR DCIX >= DCMAX                        
142400     PERFORM IMS-GU-WDK711                                                
142500     ADD +1 TO DCIX                                                       
142600     MOVE W-IDDC              TO MOD-IDDC(DCIX)                           
142700     IF SEGMENT-FINNS                                                     
142800       MOVE SLAG-KVLS         TO MOD-KVLS(DCIX)                           
142900       MOVE SLAG-KDLEVSP      TO MOD-KDLEVSP(DCIX)                        
143000       MOVE SLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL(DCIX)                   
143100                                                                          
143200       MOVE 'IMS-GU-W6D221        ' TO WS-SECTION                         
143300       PERFORM IMS-GU-W6D221                                              
143400       IF SEGMENT-FINNS                                                   
143500         MOVE DCQ-TISTADAT          TO MOD-TISTADAT(DCIX)                 
143600         MOVE DCQ-TISTODAT          TO MOD-TISTODAT(DCIX)                 
143700         MOVE DCQ-KVANTAL           TO MOD-KVANTAL(DCIX)                  
143800         MOVE DCQ-KVAVV-KVAL        TO MOD-KVAVV-KVAL(DCIX)               
143900         MOVE DCQ-KVART-SKROT       TO MOD-KVART-SKROT(DCIX)              
144000         MOVE DCQ-KVART-RET         TO MOD-KVART-RET(DCIX)                
144100         MOVE DCQ-KVART-KJUST       TO MOD-KVART-KJUST(DCIX)              
144200         MOVE DCQ-BEINIT            TO MOD-BEINIT(DCIX)                   
144300         PERFORM FC-KOLLA-INTERN-NOT                                      
144400       ELSE                                                               
144500         MOVE MFS-RENSA-FAELT  TO MOD-TISTADAT(DCIX)                      
144600                                  MOD-TISTODAT(DCIX)                      
144700                                  MOD-KVANTAL(DCIX)                       
144800                                  MOD-KVAVV-KVAL(DCIX)                    
144900                                  MOD-KVART-SKROT(DCIX)                   
145000                                  MOD-KVART-RET(DCIX)                     
145100                                  MOD-KVART-KJUST(DCIX)                   
145200                                  MOD-BEINIT(DCIX)                        
145300                                                                          
145400       END-IF                                                             
145500     ELSE                                                                 
145600       MOVE MFS-RENSA-FAELT   TO MOD-KVLS(DCIX)                           
145700                                 MOD-KDLEVSP(DCIX)                        
145800                                 MOD-KVSPARR-KVAL(DCIX)                   
145900       MOVE MFS-RENSA-FAELT   TO MOD-TISTADAT(DCIX)                       
146000                                 MOD-TISTODAT(DCIX)                       
146100                                 MOD-KVANTAL(DCIX)                        
146200                                 MOD-KVAVV-KVAL(DCIX)                     
146300                                 MOD-KVART-SKROT(DCIX)                    
146400                                 MOD-KVART-RET(DCIX)                      
146500                                 MOD-KVART-KJUST(DCIX)                    
146600                                 MOD-BEINIT(DCIX)                         
146700     END-IF                                                               
146800*    LÄS NY DC TRÄD                                                       
146900     MOVE 'IMS-GNP-WDGX6331     ' TO WS-SECTION                           
147000     PERFORM IMS-GNP-WDGX6331                                             
147100     IF ROOT-BYTE                                                         
147200       IF CDC-SE                                                          
147300         MOVE 6332-IDDC TO W-IDDC                                         
147400       ELSE                                                               
147500         MOVE 'GE' TO STATUS-WS                                           
147600         MOVE SPACE TO MOD-LEVEL3-IDDC-NEXT                               
147700       END-IF                                                             
147800     ELSE                                                                 
147900       IF SEGMENT-FINNS                                                   
148000         MOVE 6332-IDDC TO W-IDDC                                         
148100       END-IF                                                             
148200     END-IF                                                               
148300     END-PERFORM                                                          
148400**** SLUT PÅ LOOP ****                                                    
148500                                                                          
148600     IF SEGMENT-FINNS                                                     
148700       MOVE WS-SPAR-IDDC TO  MOD-LEVEL2-IDDC-NEXT                         
148800       MOVE W-IDDC       TO  MOD-LEVEL3-IDDC-NEXT                         
148900     ELSE                                                                 
149000       MOVE SPACE        TO  MOD-LEVEL3-IDDC-NEXT                         
149100     END-IF                                                               
149200     .                                                                    
149300     EJECT                                                                
149400 G-KOLLA-INPUT SECTION.                                                   
149500                                                                          
149600     MOVE JA  TO INDATA-SW                                                
149700                                                                          
149800     IF MID-INPUT = ALL '+'                                               
149900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
150000       CALL WMEDKONV USING MED-WMEDAREA                                   
150100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
150200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
150300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
150400       MOVE NEJ TO INDATA-SW                                              
150500     ELSE                                                                 
150600                                                                          
150700       IF MID-IDDC = ALL '+'                                              
150800         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UPPD-ATTR                    
150900         MOVE NEJ TO INDATA-SW                                            
151000       ELSE                                                               
151100         MOVE MID-IDDC TO W-IDDC                                          
151200         IF DCS-IDDC NOT = MID-IDDC                                       
151300            MOVE MID-IDDC TO W-IDDC-B6                                    
151400            PERFORM IMS-GU-WDB601                                         
151500         END-IF                                                           
151600                                                                          
151700         IF DCS-KDDC = SPACE OR DCS-DDC                                   
151800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UPPD-ATTR                  
151900           MOVE NEJ TO INDATA-SW                                          
152000         ELSE                                                             
152100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-UPPD-ATTR                
152200         END-IF                                                           
152300       END-IF                                                             
152400                                                                          
152500       IF INDATA-OK                                                       
152600         IF DCS-CDC                                                       
152700           PERFORM IMS-GU-WDK611                                          
152800         ELSE                                                             
152900           PERFORM IMS-GU-WDK711                                          
153000         END-IF                                                           
153100                                                                          
153200         IF SEGMENT-SAKNAS                                                
153300           MOVE NEJ                       TO INDATA-SW                    
153400         ELSE                                                             
153500           PERFORM IMS-GU-W6D211                                          
153600           IF SEGMENT-SAKNAS                                              
153700             MOVE NEJ TO INDATA-SW                                        
153800           END-IF                                                         
153900                                                                          
154000           IF MID-BEINIT NOT = ALL '+'                                    
154100             IF MID-BEINIT = SPACE                                        
154200               MOVE MFS-ALFA-FAELT-FEL TO MOD-BEINIT-UPPD-ATTR            
154300               MOVE NEJ TO INDATA-SW                                      
154400             ELSE                                                         
154500               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEINIT-UPPD-ATTR          
154600             END-IF                                                       
154700           END-IF                                                         
154800                                                                          
154900           IF MID-TISTADAT NOT = ALL '+'                                  
155000             IF MID-TISTADAT NUMERIC                                      
155100               MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-UPPD-ATTR         
155200             ELSE                                                         
155300               MOVE MFS-NUM-FAELT-FEL   TO MOD-TISTADAT-UPPD-ATTR         
155400               MOVE NEJ TO INDATA-SW                                      
155500             END-IF                                                       
155600           END-IF                                                         
155700                                                                          
155800           IF MID-TISTODAT NOT = ALL '+'                                  
155900             IF MID-TISTODAT NUMERIC                                      
156000               MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTODAT-UPPD-ATTR         
156100             ELSE                                                         
156200               MOVE MFS-NUM-FAELT-FEL   TO MOD-TISTODAT-UPPD-ATTR         
156300               MOVE NEJ TO INDATA-SW                                      
156400             END-IF                                                       
156500           END-IF                                                         
156600                                                                          
156700           IF MID-KVANTAL NOT = ALL '+'                                   
156800             IF MID-KVANTAL NUMERIC                                       
156900               MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-UPPD-ATTR          
157000             ELSE                                                         
157100               MOVE MFS-NUM-FAELT-FEL   TO MOD-KVANTAL-UPPD-ATTR          
157200               MOVE NEJ TO INDATA-SW                                      
157300             END-IF                                                       
157400           END-IF                                                         
157500                                                                          
157600           IF MID-KVAVV-KVAL NOT = ALL '+'                                
157700             IF MID-KVAVV-KVAL NUMERIC                                    
157800               MOVE MFS-NUM-FAELT-RAETT TO                                
157900                                         MOD-KVAVV-KVAL-UPPD-ATTR         
158000             ELSE                                                         
158100               MOVE MFS-NUM-FAELT-FEL  TO MOD-KVAVV-KVAL-UPPD-ATTR        
158200               MOVE NEJ TO INDATA-SW                                      
158300             END-IF                                                       
158400           END-IF                                                         
158500                                                                          
158600           IF MID-KVART-SKROT NOT = ALL '+'                               
158700             IF MID-KVART-SKROT NUMERIC                                   
158800              MOVE MFS-NUM-FAELT-RAETT TO                                 
158900                                        MOD-KVART-SKROT-UPPD-ATTR         
159000             ELSE                                                         
159100              MOVE MFS-NUM-FAELT-FEL  TO MOD-KVART-SKROT-UPPD-ATTR        
159200              MOVE NEJ TO INDATA-SW                                       
159300             END-IF                                                       
159400           END-IF                                                         
159500                                                                          
159600           IF MID-KVART-RET NOT = ALL '+'                                 
159700             IF MID-KVART-RET NUMERIC                                     
159800               MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-RET-UPPD-ATTR        
159900             ELSE                                                         
160000               MOVE MFS-NUM-FAELT-FEL   TO MOD-KVART-RET-UPPD-ATTR        
160100               MOVE NEJ TO INDATA-SW                                      
160200             END-IF                                                       
160300           END-IF                                                         
160400                                                                          
160500           IF MID-KVART-KJUST NOT = ALL '+'                               
160600             IF MID-KVART-KJUST NUMERIC                                   
160700              MOVE MFS-NUM-FAELT-RAETT TO                                 
160800                                         MOD-KVART-KJUST-UPPD-ATTR        
160900             ELSE                                                         
161000              MOVE MFS-NUM-FAELT-FEL  TO MOD-KVART-KJUST-UPPD-ATTR        
161100              MOVE NEJ TO INDATA-SW                                       
161200             END-IF                                                       
161300           END-IF                                                         
161400** KOLLA OM DC FÅR GÖRA UPPDATERING, LÄS DC TRÄD                          
161500           IF INDATA-OK                                                   
161600             IF (WS-SPAR-IDDC NOT = MID-IDDC) AND                         
161700                (MSGI-IDDC    NOT = MID-IDDC) AND                         
161800                (MSGI-IDDC    NOT = WC-CDC-SE)                            
161900               MOVE MID-IDDC TO W-IDDC-6332                               
162000               PERFORM GB-KOLLA-DCTRAD                                    
162100             END-IF                                                       
162200           END-IF                                                         
162300* SLUT PÅ DC TRÄD                                                         
162400         END-IF                                                           
162500       END-IF                                                             
162600                                                                          
162700       IF INDATA-FEL                                                      
162800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
162900         CALL WMEDKONV USING MED-WMEDAREA                                 
163000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
163100         PERFORM GA-MID-INDATA-TILL-MOD                                   
163200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
163300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
163400       ELSE                                                               
163500         PERFORM IMS-GU-W6D221-2                                          
163600         IF SEGMENT-FINNS                                                 
163700           CONTINUE                                                       
163800         ELSE                                                             
163900           IF DCS-IDDC NOT = MSGI-IDDC                                    
164000              MOVE MSGI-IDDC TO W-IDDC-B6                                 
164100              PERFORM IMS-GU-WDB601                                       
164200           END-IF                                                         
164300           IF DCS-CDC OR UPDATE-OK                                        
164400             CONTINUE                                                     
164500           ELSE                                                           
164600             MOVE NEJ TO INDATA-SW                                        
164700             MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
164800             CALL WMEDKONV USING MED-WMEDAREA                             
164900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
165000             PERFORM MFS-ROER-EJ-FAELT-UT                                 
165100             PERFORM MFS-ROER-EJ-FAELT-IN                                 
165200           END-IF                                                         
165300         END-IF                                                           
165400       END-IF                                                             
165500     END-IF                                                               
165600     .                                                                    
165700     EJECT                                                                
165800 GA-MID-INDATA-TILL-MOD SECTION.                                          
165900                                                                          
166000     IF MID-IDDC NOT = ALL '+'                                            
166100        MOVE MID-IDDC              TO MOD-IDDC-UPPD                       
166200     END-IF                                                               
166300                                                                          
166400     IF MID-TISTADAT NOT = ALL '+'                                        
166500        MOVE MID-TISTADAT          TO MOD-TISTADAT-UPPD                   
166600     END-IF                                                               
166700                                                                          
166800     IF MID-TISTODAT NOT = ALL '+'                                        
166900        MOVE MID-TISTODAT          TO MOD-TISTODAT-UPPD                   
167000     END-IF                                                               
167100                                                                          
167200     IF MID-KVANTAL NOT = ALL '+'                                         
167300        MOVE MID-KVANTAL           TO MOD-KVANTAL-UPPD                    
167400     END-IF                                                               
167500                                                                          
167600     IF MID-KVAVV-KVAL NOT = ALL '+'                                      
167700        MOVE MID-KVAVV-KVAL        TO MOD-KVAVV-KVAL-UPPD                 
167800     END-IF                                                               
167900                                                                          
168000     IF MID-KVART-SKROT NOT = ALL '+'                                     
168100        MOVE MID-KVART-SKROT       TO MOD-KVART-SKROT-UPPD                
168200     END-IF                                                               
168300                                                                          
168400     IF MID-KVART-RET NOT = ALL '+'                                       
168500        MOVE MID-KVART-RET         TO MOD-KVART-RET-UPPD                  
168600     END-IF                                                               
168700                                                                          
168800     IF MID-KVART-KJUST NOT = ALL '+'                                     
168900        MOVE MID-KVART-KJUST       TO MOD-KVART-KJUST-UPPD                
169000     END-IF                                                               
169100                                                                          
169200     IF MID-BEINIT NOT = ALL '+'                                          
169300        MOVE MID-BEINIT            TO MOD-BEINIT-UPPD                     
169400     END-IF                                                               
169500                                                                          
169600     .                                                                    
169700     EJECT                                                                
169800 GB-KOLLA-DCTRAD SECTION.                                                 
169900* KOLLA OM MSGI IDDC = LEVEL 2 = FÅR UPPDATERA LEVLE 3                    
170000     MOVE MSGI-IDDC TO W-IDDC-6332                                        
170100     PERFORM IMS-GHU-WDGX6332                                             
170200     IF SEGMENT-FINNS                                                     
170300* KOLLA OM IDDC-IN FINNS I RÄTT DC BEN                                    
170400       MOVE MID-IDDC TO W-IDDC-6334                                       
170500       PERFORM IMS-GET-WDGX6334-UNIK                                      
170600       IF SEGMENT-SAKNAS                                                  
170700         MOVE NEJ TO INDATA-SW                                            
170800         MOVE NEJ TO UPDATE-SW                                            
170900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPPD-ATTR                  
171000       ELSE                                                               
171100         MOVE JA  TO UPDATE-SW                                            
171200       END-IF                                                             
171300     ELSE                                                                 
171400* MSGI IDDC = LEVEL 3                                                     
171500* HITTA RÄTT LEVEL 2                                                      
171600       PERFORM IMS-GET-WDGX6331                                           
171700       PERFORM IMS-GNP-WDGX6332                                           
171800       MOVE MSGI-IDDC TO W-IDDC-6334                                      
171900       PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                         
172000         MOVE 6332-IDDC TO W-IDDC-6332                                    
172100         PERFORM IMS-GET-WDGX6334-UNIK                                    
172200         IF SEGMENT-FINNS                                                 
172300           MOVE JA  TO POST-FINNS-SW                                      
172400           MOVE JA  TO UPDATE-SW                                          
172500         ELSE                                                             
172600           PERFORM IMS-GNP-WDGX6332                                       
172700         END-IF                                                           
172800       END-PERFORM                                                        
172900       IF POST-SAKNAS                                                     
173000         MOVE NEJ TO INDATA-SW                                            
173100         MOVE NEJ TO UPDATE-SW                                            
173200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPPD-ATTR                  
173300       END-IF                                                             
173400     END-IF                                                               
173500     IF INDATA-OK AND POST-FINNS                                          
173600* KOLLA OM MSGI-IDUSER FINNS PÅ LEVEL2                                    
173700       MOVE 6332-IDDC TO W-IDDC-6332                                      
173800       PERFORM IMS-GHU-WDGX6332                                           
173900       IF SEGMENT-FINNS                                                   
174000        IF 6332-IDUSER(1) NOT = SPACE                                     
174100          IF MSGI-IDUSER NOT = 6332-IDUSER(1)                             
174200            MOVE NEJ TO INDATA-SW                                         
174300          ELSE                                                            
174400            MOVE JA  TO INDATA-SW                                         
174500          END-IF                                                          
174600        END-IF                                                            
174700        IF INDATA-FEL                                                     
174800          IF 6332-IDUSER(2) NOT = SPACE                                   
174900            IF MSGI-IDUSER = 6332-IDUSER(2)                               
175000              MOVE JA  TO INDATA-SW                                       
175100            END-IF                                                        
175200          END-IF                                                          
175300        END-IF                                                            
175400        IF INDATA-FEL                                                     
175500          IF 6332-IDUSER(3) NOT = SPACE                                   
175600            IF MSGI-IDUSER  = 6332-IDUSER(3)                              
175700              MOVE JA  TO INDATA-SW                                       
175800            END-IF                                                        
175900          END-IF                                                          
176000        END-IF                                                            
176100        IF INDATA-FEL                                                     
176200          IF 6332-IDUSER(4) NOT = SPACE                                   
176300            IF MSGI-IDUSER  = 6332-IDUSER(4)                              
176400              MOVE JA  TO INDATA-SW                                       
176500            END-IF                                                        
176600          END-IF                                                          
176700        END-IF                                                            
176800       ELSE                                                               
176900         MOVE NEJ TO INDATA-SW                                            
177000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPPD-ATTR                  
177100       END-IF                                                             
177200     END-IF                                                               
177300     IF INDATA-FEL                                                        
177400       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPPD-ATTR                    
177500     END-IF                                                               
177600     .                                                                    
177700     EJECT                                                                
177800 H-UPPDATERA SECTION.                                                     
177900                                                                          
178000     PERFORM IMS-GHU-W6D221                                               
178100                                                                          
178200     IF SEGMENT-FINNS                                                     
178300                                                                          
178400       MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                     
178500       MOVE INFO-DAREGDAT-9KOMPL (2:7)                                    
178600                                 TO MOD-TIREGDAT-9KOMPL-ENTER             
178700       MOVE INFO-TIKLOCK-9KOMPL  TO MOD-TIKLOCK-9KOMPL-ENTER              
178800                                    W-TIKLOCK-9KOMPL                      
178900       IF MID-TISTADAT NOT = ALL '+'                                      
179000         MOVE MID-TISTADAT TO DCQ-TISTADAT                                
179100         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-UPPD-ATTR             
179200       ELSE                                                               
179300         MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-UPPD                      
179400       END-IF                                                             
179500                                                                          
179600       IF MID-TISTODAT NOT = ALL '+'                                      
179700         MOVE MID-TISTODAT TO DCQ-TISTODAT                                
179800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTODAT-UPPD-ATTR             
179900       ELSE                                                               
180000         MOVE MFS-ROER-EJ-FAELT TO MOD-TISTODAT-UPPD                      
180100       END-IF                                                             
180200                                                                          
180300       IF MID-KVANTAL NOT = ALL '+'                                       
180400         MOVE MID-KVANTAL TO DCQ-KVANTAL                                  
180500         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVANTAL-UPPD-ATTR              
180600       ELSE                                                               
180700         MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTAL-UPPD                       
180800       END-IF                                                             
180900                                                                          
181000       IF MID-KVAVV-KVAL NOT = ALL '+'                                    
181100         MOVE MID-KVAVV-KVAL TO DCQ-KVAVV-KVAL                            
181200         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVAVV-KVAL-UPPD-ATTR           
181300       ELSE                                                               
181400         MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVV-KVAL-UPPD                    
181500       END-IF                                                             
181600                                                                          
181700       IF MID-KVART-SKROT NOT = ALL '+'                                   
181800         MOVE MID-KVART-SKROT TO DCQ-KVART-SKROT                          
181900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-SKROT-UPPD-ATTR          
182000       ELSE                                                               
182100         MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-SKROT-UPPD                   
182200       END-IF                                                             
182300                                                                          
182400       IF MID-KVART-RET NOT = ALL '+'                                     
182500         MOVE MID-KVART-RET TO DCQ-KVART-RET                              
182600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-RET-UPPD-ATTR            
182700       ELSE                                                               
182800         MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-RET-UPPD                     
182900       END-IF                                                             
183000                                                                          
183100       IF MID-KVART-KJUST NOT = ALL '+'                                   
183200         MOVE MID-KVART-KJUST TO DCQ-KVART-KJUST                          
183300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-KJUST-UPPD-ATTR          
183400       ELSE                                                               
183500         MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-KJUST-UPPD                   
183600       END-IF                                                             
183700                                                                          
183800       IF MID-BEINIT NOT = ALL '+'                                        
183900         MOVE MID-BEINIT TO DCQ-BEINIT                                    
184000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEINIT-UPPD-ATTR               
184100       ELSE                                                               
184200         MOVE MFS-ROER-EJ-FAELT TO MOD-BEINIT-UPPD                        
184300       END-IF                                                             
184400                                                                          
184500       PERFORM IMS-REPL-W6D221                                            
184600       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
184700       CALL WMEDKONV USING MED-WMEDAREA                                   
184800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
184900       PERFORM MFS-FORM-ATTR                                              
185000       PERFORM MFS-RENSA-FAELT-IN                                         
185100     ELSE                                                                 
185200       MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                            
185300       CALL WMEDKONV USING MED-WMEDAREA                                   
185400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
185500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
185600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
185700     END-IF                                                               
185800                                                                          
185900     .                                                                    
186000     EJECT                                                                
186100 MFS-RENSA-FAELT-UT SECTION.                                              
186200                                                                          
186300*    --- ALLA UTDATA-FÄLT                                                 
186400     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-9KOMPL-ENTER                    
186500                             MOD-TIKLOCK-9KOMPL-ENTER                     
186600                             MOD-TIREGDAT-9KOMPL-NEXT                     
186700                             MOD-TIKLOCK-9KOMPL-NEXT                      
186800     MOVE +1 TO DCIX                                                      
186900     PERFORM UNTIL DCIX > DCMAX                                           
187000       MOVE MFS-RENSA-FAELT TO  MOD-IDDC(DCIX)                            
187100                                MOD-TISTADAT(DCIX)                        
187200                                MOD-TISTODAT(DCIX)                        
187300                                MOD-KVANTAL(DCIX)                         
187400                                MOD-KVAVV-KVAL(DCIX)                      
187500                                MOD-KVART-SKROT(DCIX)                     
187600                                MOD-KVART-RET(DCIX)                       
187700                                MOD-KVART-KJUST(DCIX)                     
187800                                MOD-BEINIT(DCIX)                          
187900                                MOD-KVLS(DCIX)                            
188000                                MOD-KDLEVSP(DCIX)                         
188100                                MOD-FLLEV  (DCIX)                         
188200                                MOD-FLFIN  (DCIX)                         
188300                                MOD-KVSPARR-KVAL(DCIX)                    
188400                                MOD-FL-DC-INFO(DCIX)                      
188500       ADD +1 TO DCIX                                                     
188600     END-PERFORM                                                          
188700     .                                                                    
188800     SKIP3                                                                
188900 MFS-RENSA-FAELT-IN SECTION.                                              
189000                                                                          
189100*    --- ALLA INDATA-FÄLT                                                 
189200     MOVE MFS-RENSA-FAELT TO MOD-IDDC-UPPD                                
189300                             MOD-TISTADAT-UPPD                            
189400                             MOD-TISTODAT-UPPD                            
189500                             MOD-KVANTAL-UPPD                             
189600                             MOD-KVAVV-KVAL-UPPD                          
189700                             MOD-KVART-SKROT-UPPD                         
189800                             MOD-KVART-RET-UPPD                           
189900                             MOD-KVART-KJUST-UPPD                         
190000                             MOD-BEINIT-UPPD                              
190100     .                                                                    
190200     EJECT                                                                
190300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
190400                                                                          
190500*    --- ALLA UTDATA-FÄLT                                                 
190600     MOVE MFS-ROER-EJ-FAELT TO MOD-TIREGDAT-9KOMPL-ENTER                  
190700                               MOD-TIKLOCK-9KOMPL-ENTER                   
190800                               MOD-TIREGDAT-9KOMPL-NEXT                   
190900                               MOD-TIKLOCK-9KOMPL-NEXT                    
191000     MOVE +1 TO DCIX                                                      
191100     PERFORM UNTIL DCIX > DCMAX                                           
191200       MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDC(DCIX)                          
191300                                  MOD-TISTADAT(DCIX)                      
191400                                  MOD-TISTODAT(DCIX)                      
191500                                  MOD-KVANTAL(DCIX)                       
191600                                  MOD-KVAVV-KVAL(DCIX)                    
191700                                  MOD-KVART-SKROT(DCIX)                   
191800                                  MOD-KVART-RET(DCIX)                     
191900                                  MOD-KVART-KJUST(DCIX)                   
192000                                  MOD-BEINIT(DCIX)                        
192100                                  MOD-KVLS(DCIX)                          
192200                                  MOD-KDLEVSP(DCIX)                       
192300                                  MOD-FLLEV  (DCIX)                       
192400                                  MOD-FLFIN  (DCIX)                       
192500                                  MOD-KVSPARR-KVAL(DCIX)                  
192600                                  MOD-FL-DC-INFO(DCIX)                    
192700        ADD +1 TO DCIX                                                    
192800     END-PERFORM                                                          
192900     .                                                                    
193000     SKIP3                                                                
193100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
193200                                                                          
193300*    --- ALLA INDATA-FÄLT                                                 
193400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-UPPD                              
193500                               MOD-TISTADAT-UPPD                          
193600                               MOD-TISTODAT-UPPD                          
193700                               MOD-KVANTAL-UPPD                           
193800                               MOD-KVAVV-KVAL-UPPD                        
193900                               MOD-KVART-SKROT-UPPD                       
194000                               MOD-KVART-RET-UPPD                         
194100                               MOD-KVART-KJUST-UPPD                       
194200                               MOD-BEINIT-UPPD                            
194300     .                                                                    
194400     EJECT                                                                
194500 MFS-FORM-ATTR SECTION.                                                   
194600                                                                          
194700*    --- ALLA INDATA-FÄLT                                                 
194800     MOVE MFS-FORMATETS-ATTR TO MOD-IDDC-UPPD-ATTR                        
194900                                MOD-TISTADAT-UPPD-ATTR                    
195000                                MOD-TISTODAT-UPPD-ATTR                    
195100                                MOD-KVANTAL-UPPD-ATTR                     
195200                                MOD-KVAVV-KVAL-UPPD-ATTR                  
195300                                MOD-KVART-SKROT-UPPD-ATTR                 
195400                                MOD-KVART-RET-UPPD-ATTR                   
195500                                MOD-KVART-KJUST-UPPD-ATTR                 
195600                                MOD-BEINIT-UPPD-ATTR                      
195700     .                                                                    
195800     SKIP2                                                                
195900* --- IMS SEKTIONER ---                                                   
196000     SKIP3                                                                
196100 IMS-GET-MSG SECTION.                                                     
196200                                                                          
196300     MOVE '  QC' TO GODK-STATUSKODER                                      
196400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
196500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
196600     PERFORM IMS-STATUSKONTROLL                                           
196700     .                                                                    
196800     SKIP3                                                                
196900 IMS-INSERT-MSG SECTION.                                                  
197000                                                                          
197100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
197200     MOVE SPACE TO GODK-STATUSKODER                                       
197300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
197400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
197500     PERFORM IMS-STATUSKONTROLL                                           
197600     .                                                                    
197700     EJECT                                                                
197800 IMS-GU-W6D201 SECTION.                                                   
197900                                                                          
198000     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
198100          DELIMITED BY SIZE INTO SSA1                                     
198200     MOVE '  GE' TO GODK-STATUSKODER                                      
198300     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D2 SSA1                     
198400     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
198500     PERFORM IMS-STATUSKONTROLL                                           
198600     .                                                                    
198700     EJECT                                                                
198800 IMS-GNP-W6D211 SECTION.                                                  
198900                                                                          
199000     STRING 'W6D211  (W6D211KY=>' W-W6D211KY-X                            
199100                    '&IDKVAINF=>' W-IDKVAINF-MIN-X                        
199200                    '&IDKVAINF=<' W-IDKVAINF-MAX-X ')'                    
199300          DELIMITED BY SIZE INTO SSA1                                     
199400     MOVE '  GE' TO GODK-STATUSKODER                                      
199500     CALL CBLTDLI USING GNP W6D2A-PCB DLI-IO-W6D2 SSA1                    
199600     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
199700     PERFORM IMS-STATUSKONTROLL                                           
199800     .                                                                    
199900     SKIP3                                                                
200000 IMS-GU-W6D211 SECTION.                                                   
200100     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
200200          DELIMITED BY SIZE INTO SSA1                                     
200300     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
200400          DELIMITED BY SIZE INTO SSA2                                     
200500     MOVE '  GE' TO GODK-STATUSKODER                                      
200600     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D2 SSA1 SSA2                
200700     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
200800     PERFORM IMS-STATUSKONTROLL                                           
200900     .                                                                    
201000     SKIP3                                                                
201100 IMS-GU-W6D211-B SECTION.                                                 
201200     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
201300          DELIMITED BY SIZE INTO SSA1                                     
201400     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-2 ')'                        
201500          DELIMITED BY SIZE INTO SSA2                                     
201600     MOVE '  GE' TO GODK-STATUSKODER                                      
201700     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D2 SSA1 SSA2                
201800     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
201900     PERFORM IMS-STATUSKONTROLL                                           
202000     .                                                                    
202100     SKIP3                                                                
202200 IMS-GU-W6D221 SECTION.                                                   
202300                                                                          
202400     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
202500          DELIMITED BY SIZE INTO SSA1                                     
202600     STRING 'W6D211  (W6D211KY =' W-W6D211KY-X ')'                        
202700          DELIMITED BY SIZE INTO SSA2                                     
202800     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
202900          DELIMITED BY SIZE INTO SSA3                                     
203000     MOVE '  GE' TO GODK-STATUSKODER                                      
203100     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
203200     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
203300     PERFORM IMS-STATUSKONTROLL                                           
203400     .                                                                    
203500     SKIP3                                                                
203600 IMS-GU-W6D221-2 SECTION.                                                 
203700                                                                          
203800     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
203900          DELIMITED BY SIZE INTO SSA1                                     
204000     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
204100          DELIMITED BY SIZE INTO SSA2                                     
204200     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
204300          DELIMITED BY SIZE INTO SSA3                                     
204400     MOVE '  GE' TO GODK-STATUSKODER                                      
204500     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
204600     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
204700     PERFORM IMS-STATUSKONTROLL                                           
204800     .                                                                    
204900     SKIP3                                                                
205000 IMS-GHU-W6D221 SECTION.                                                  
205100                                                                          
205200     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
205300          DELIMITED BY SIZE INTO SSA1                                     
205400     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
205500          DELIMITED BY SIZE INTO SSA2                                     
205600     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
205700          DELIMITED BY SIZE INTO SSA3                                     
205800     MOVE '  GE' TO GODK-STATUSKODER                                      
205900     CALL CBLTDLI USING GHU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3        
206000     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
206100     PERFORM IMS-STATUSKONTROLL                                           
206200     .                                                                    
206300     SKIP3                                                                
206400 IMS-GNP-W6D221 SECTION.                                                  
206500                                                                          
206600     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
206700          DELIMITED BY SIZE INTO SSA1                                     
206800     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-2 ')'                        
206900          DELIMITED BY SIZE INTO SSA2                                     
207000     MOVE 'W6D221 ' TO SSA3                                               
207100     MOVE '  GE' TO GODK-STATUSKODER                                      
207200     CALL CBLTDLI USING GNP W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3        
207300     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
207400     PERFORM IMS-STATUSKONTROLL                                           
207500     .                                                                    
207600     SKIP3                                                                
207700 IMS-REPL-W6D221 SECTION.                                                 
207800                                                                          
207900     MOVE '  ' TO GODK-STATUSKODER                                        
208000     CALL CBLTDLI USING REPL W6D2B-PCB DLI-IO-W6D221                      
208100     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     SKIP3                                                                
208500 IMS-GU-WDK711 SECTION.                                                   
208600                                                                          
208700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
208800          DELIMITED BY SIZE INTO SSA1                                     
208900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
209000          DELIMITED BY SIZE INTO SSA2                                     
209100     MOVE '  GE' TO GODK-STATUSKODER                                      
209200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK7 SSA1 SSA2                 
209300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
209400     PERFORM IMS-STATUSKONTROLL                                           
209500     .                                                                    
209600     EJECT                                                                
209700 IMS-GU-WDK611 SECTION.                                                   
209800                                                                          
209900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
210000          DELIMITED BY SIZE INTO SSA1                                     
210100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
210200          DELIMITED BY SIZE INTO SSA2                                     
210300     MOVE '  GE' TO GODK-STATUSKODER                                      
210400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK6 SSA1 SSA2                 
210500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     .                                                                    
210800     EJECT                                                                
210900 IMS-GU-WDB601    SECTION.                                                
211000     MOVE 'IMS-GU-WDB601           ' TO WS-IMS                            
211100                                                                          
211200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
211300          DELIMITED BY SIZE INTO SSA1                                     
211400     MOVE '  GE' TO GODK-STATUSKODER                                      
211500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
211600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
211700     PERFORM IMS-STATUSKONTROLL                                           
211800     IF SEGMENT-SAKNAS                                                    
211900         MOVE SPACE TO DCS-KDDC                                           
212000     END-IF                                                               
212100     .                                                                    
212200 IMS-GET-WDGX6331 SECTION.                                                
212300     MOVE 'IMS-GET-WDGX6331        ' TO WS-IMS                            
212400                                                                          
212500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
212600          DELIMITED BY SIZE INTO SSA1                                     
212700     MOVE '  GE' TO GODK-STATUSKODER                                      
212800     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX6331 SSA1                  
212900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
213000     PERFORM IMS-STATUSKONTROLL                                           
213100     .                                                                    
213200     EJECT                                                                
213300 IMS-GNP-WDGX6331 SECTION.                                                
213400     MOVE 'IMS-GNP-WDGX6331        ' TO WS-IMS                            
213500                                                                          
213600**** STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
213700*****     DELIMITED BY SIZE INTO SSA1                                     
213800     MOVE '  GEGA' TO GODK-STATUSKODER                                    
213900     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332                      
214000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
214100     PERFORM IMS-STATUSKONTROLL                                           
214200     .                                                                    
214300     EJECT                                                                
214400 IMS-GHU-WDGX6332 SECTION.                                                
214500     MOVE 'IMS-GU-WDGX6332         ' TO WS-IMS                            
214600                                                                          
214700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
214800          DELIMITED BY SIZE INTO SSA1                                     
214900     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
215000          DELIMITED BY SIZE INTO SSA2                                     
215100     MOVE '  GE' TO GODK-STATUSKODER                                      
215200     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
215300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     EJECT                                                                
215700 IMS-GNP-WDGX6332 SECTION.                                                
215800     MOVE 'IMS-GNP-WDGX6332 SECTION' TO WS-IMS                            
215900                                                                          
216000     MOVE 'WDGX6332   ' TO SSA1                                           
216100     MOVE '  GE' TO GODK-STATUSKODER                                      
216200     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332 SSA1                 
216300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     EJECT                                                                
216700 IMS-GET-WDGX6334-UNIK SECTION.                                           
216800     MOVE 'IMS-GET-WDGX6334-UNIK   ' TO WS-IMS                            
216900                                                                          
217000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
217100          DELIMITED BY SIZE INTO SSA1                                     
217200                                                                          
217300     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
217400          DELIMITED BY SIZE INTO SSA2                                     
217500                                                                          
217600     STRING 'WDGX6334(IDDC     =' W-WDGXKEY-6334-X ')'                    
217700          DELIMITED BY SIZE INTO SSA3                                     
217800                                                                          
217900     MOVE '  GE' TO GODK-STATUSKODER                                      
218000     CALL CBLTDLI USING GU WDR2ALT-PCB                                    
218100     DLI-IO-WDGX6334 SSA1 SSA2 SSA3                                       
218200                                                                          
218300     MOVE WDR2ALT-STATUS-CODE TO STATUS-WS                                
218400     PERFORM IMS-STATUSKONTROLL                                           
218500     .                                                                    
218600     EJECT                                                                
218700 IMS-GET-WDGX6334 SECTION.                                                
218800     MOVE 'IMS-GET-WDGX6334        ' TO WS-IMS                            
218900                                                                          
219000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
219100          DELIMITED BY SIZE INTO SSA1                                     
219200                                                                          
219300     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
219400          DELIMITED BY SIZE INTO SSA2                                     
219500                                                                          
219600     MOVE 'WDGX6334 ' TO SSA3                                             
219700     MOVE '  GE' TO GODK-STATUSKODER                                      
219800     CALL CBLTDLI USING GU  WDR2-PCB DLI-IO-WDGX6334                      
219900                              SSA1 SSA2 SSA3                              
220000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300     EJECT                                                                
220400 IMS-GNP-WDGX6334-UNIK SECTION.                                           
220500     MOVE 'IMS-GNP-WDGX6334-UNIK   ' TO WS-IMS                            
220600                                                                          
220700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
220800          DELIMITED BY SIZE INTO SSA1                                     
220900     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
221000          DELIMITED BY SIZE INTO SSA2                                     
221100                                                                          
221200     MOVE 'WDGX6334 ' TO SSA3                                             
221300     MOVE '  GE' TO GODK-STATUSKODER                                      
221400     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6334                      
221500                              SSA1 SSA2 SSA3                              
221600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
221700     PERFORM IMS-STATUSKONTROLL                                           
221800     .                                                                    
221900     EJECT                                                                
222000 IMS-GNP-WDGX6334      SECTION.                                           
222100     MOVE 'IMS-GNP-WDGX6334        ' TO WS-IMS                            
222200                                                                          
222300     MOVE 'WDGX6334 ' TO SSA1                                             
222400     MOVE '  GE' TO GODK-STATUSKODER                                      
222500     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6334 SSA1                 
222600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
222700     PERFORM IMS-STATUSKONTROLL                                           
222800     .                                                                    
222900     EJECT                                                                
223000 IMS-STATUSKONTROLL SECTION.                                              
223100                                                                          
223200     SET STATUS-IX TO 1                                                   
223300     SEARCH GODK-STATUS                                                   
223400       AT END                                                             
223500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
223600         DELIMITED BY SIZE INTO FELTEXT                                   
223700         CALL FELLOG                                                      
223800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
223900         CONTINUE                                                         
224000     END-SEARCH                                                           
224100     .                                                                    
