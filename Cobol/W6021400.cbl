000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6021400.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   99/11/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV ARTIKELHISTORIK KVALITET                          
000900*        FÖR SDC/NDC.                                                     
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR W6D2                                       
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDB6                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W6T214                                              
001700*        MID:         W6I21401                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W6O21401                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W6021400'.            
003100                                                                          
003200 01  DAGENS-DATUM                PIC 9(6) VALUE ZERO.                     
003300 01  DAGENS-TID                  PIC 9(8) VALUE ZERO.                     
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003700                                                                          
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
004200 77  SPRAK-IX                    PIC 9       VALUE ZERO.                  
004300 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
004400 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
004500 01  W1-DAREGDAT                 PIC 9(08).                               
004600 77  W1-TIKLOCK                  PIC 9(09).                               
004700 77  FL-INTERN                   PIC X       VALUE 'N'.                   
004800 01  WS-6332-IDDC                PIC XX      VALUE SPACE.                 
004900 01  WS-IMS                      PIC X(80)   VALUE SPACE.                 
005000                                                                          
005100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200                                                                          
005300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005400     88  INDATA-OK                           VALUE 'J'.                   
005500     88  INDATA-FEL                          VALUE 'N'.                   
005600                                                                          
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
006000                                                                          
006100 77  NYTT-ARTNR-SW               PIC X       VALUE 'N'.                   
006200     88  NYTT-ARTNR                          VALUE 'J'.                   
006300     88  GAMMALT-ARTNR                       VALUE 'N'.                   
006400                                                                          
006500 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
006600     88  POST-FINNS                          VALUE 'J'.                   
006700     88  POST-SAKNAS                         VALUE 'N'.                   
006800                                                                          
006900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007000     88  EGEN-MID                            VALUE '6214'.                
007100     88  GODK-MID                            VALUE '6211' '6212'          
007200                                                   '6213' '6214'          
007300                                                   '6215' '6216'          
007400                                                   '6217' '6218'          
007500                                                   '6219'.                
007600     88  HELP-MID                            VALUE '0551'.                
007700 77  FL-LAES                     PIC X       VALUE 'J'.                   
007800     EJECT                                                                
007900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008000 01  GENERELLA-SUBPROGRAM.                                                
008100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008700*01 -COPY WMEDAREA                                                        
008800     SKIP3                                                                
008900 01  MESSAGE-CODES.                                                       
009000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009500     03  ERR-MISSING-KEY         PIC X(3)    VALUE '005'.                 
009600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009800     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010300     SKIP3                                                                
010400*01 -COPY WMSGINIT                                                        
010500     EJECT                                                                
010600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010900     SKIP3                                                                
011000*01  MID -COPY W6I21401                                                   
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011300     SKIP3                                                                
011400*01  -COPY WMSGAREA                                                       
011500     EJECT                                                                
011600     03  MOD REDEFINES MSG-AREA.                                          
011700*      05  -COPY W6O21401                                                 
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012000     SKIP3                                                                
012100*01  -COPY WMFSAREA                                                       
012200     EJECT                                                                
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800 01  NYCKLAR-TILL-DLI.                                                    
012900     03  W-IDARTNR-X.                                                     
013000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013100     03  W-IDDC-X.                                                        
013200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013300     03  W-W6D211KY-X.                                                    
013400         05  W-DAREGDAT-9KOMPL   PIC 9(08)   VALUE ZERO.                  
013500         05  W-TIKLOCK-9KOMPL    PIC S9(09)  VALUE ZERO COMP-3.           
013600     03  W-IDKVAINF-X.                                                    
013700         05  W-IDKVAINF          PIC  9(02)  VALUE ZERO.                  
013800     03  W-IDKVAINF-MIN-X.                                                
013900         05  W-IDKVAINF-MIN      PIC  9(02)  VALUE ZERO.                  
014000     03  W-IDKVAINF-MAX-X.                                                
014100         05  W-IDKVAINF-MAX      PIC  9(02)  VALUE ZERO.                  
014200     03  W-KDARBTYP-X.                                                    
014300         05  W-KDARBTYP          PIC X(8)    VALUE 'QUAL    '.            
014400     03  W-IDPERSON-X.                                                    
014500         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
014600                                                                          
014700     03  W-IDDC-B6-X.                                                     
014800         05 W-IDDC-B6            PIC X(2).                                
014900                                                                          
015000     03  W-WDGXKEY-6331-X.                                                
015100         05  W-IDHTYP            PIC X(4)    VALUE '6331'.                
015200         05  W-IDDC-6331         PIC X(2)    VALUE '11'.                  
015300         05  W-6331-LOW          PIC X(24)   VALUE LOW-VALUE.             
015400                                                                          
015500     03  W-WDGXKEY-6332-X.                                                
015600         05  W-IDDC-6332         PIC X(2)    VALUE SPACE.                 
015700                                                                          
015800     03  W-WDGXKEY-6334-X.                                                
015900         05  W-IDDC-6334         PIC X(2)    VALUE SPACE.                 
016000                                                                          
016100     SKIP2                                                                
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FINNS                       VALUE '  '.                  
016500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017300 01  SSA3                        PIC X(64).                               
017400     EJECT                                                                
017500*    --- IMS FUNKTIONSKODER                                               
017600*01  -COPY W0003                                                          
017700     EJECT                                                                
017800*    ---  DLI INPUT-OUTPUT AREA                                           
017900                                                                          
018000 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D2'.                        
018100 01  DLI-IO-W6D2-AREA.                                                    
018200     03  DLI-IO-W6D2 PIC X(1200).                                         
018300     03  IO-W6D201 REDEFINES DLI-IO-W6D2.                                 
018400*        05  -COPY W6D201  -PRE KVAH-                                     
018500     EJECT                                                                
018600     03  IO-W6D211 REDEFINES DLI-IO-W6D2.                                 
018700*          05  -COPY W6D211                                               
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D221'.                      
019000 01  DLI-IO-W6D221.                                                       
019100*          05  -COPY W6D221                                               
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
019400 01  DLI-IO-WDK6-AREA.                                                    
019500     03  DLI-IO-WDK6 PIC X(900).                                          
019600     03  IO-WDK601 REDEFINES DLI-IO-WDK6.                                 
019700*          05  -COPY WDK601                                               
019800     EJECT                                                                
019900     03  IO-WDK611 REDEFINES DLI-IO-WDK6.                                 
020000*          05  -COPY WDK611                                               
020100     EJECT                                                                
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7'.                        
020300 01  DLI-IO-WDK7-AREA.                                                    
020400     03  DLI-IO-WDK7 PIC X(300).                                          
020500     03  IO-WDK701 REDEFINES DLI-IO-WDK7.                                 
020600*          05  -COPY WDK701                                               
020700     EJECT                                                                
020800     03  IO-WDK711 REDEFINES DLI-IO-WDK7.                                 
020900*          05  -COPY WDK711                                               
021000     EJECT                                                                
021100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-P311'.          
021200 01  DLI-IO-P311.                                                         
021300*    03  -COPY WDP311                                                     
021400                                                                          
021500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021600 01   DLI-IO-AREA-B601.                                                   
021700*     03  -COPY WDB601                                                    
021800     EJECT                                                                
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6331'.                     
022000 01  DLI-IO-WDGX6331.                                                     
022100*    03  -COPY WDGX6331.                                                  
022200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6332'.                     
022300 01  DLI-IO-WDGX6332.                                                     
022400*    03  -COPY WDGX6332.                                                  
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6334'.                     
022600 01  DLI-IO-WDGX6334.                                                     
022700*    03  -COPY WDGX6334.                                                  
022800     EJECT                                                                
022900 LINKAGE SECTION.                                                         
023000*01  -COPY W0009   -PRE MSG-                                              
023100*01  -COPY W0008   -PRE USEA-                                             
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400*01  -COPY W0008   -PRE W6D2A-                                            
023500     05  FILLER                  PIC X.                                   
023600                                                                          
023700*01  -COPY W0008   -PRE W6D2B-                                            
023800     05  FILLER                  PIC X.                                   
023900                                                                          
024000*01  -COPY W0008   -PRE WDK6-                                             
024100     05  FILLER                  PIC X.                                   
024200     EJECT                                                                
024300*01  -COPY W0008   -PRE WDK7-                                             
024400     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600*01  -COPY W0008   -PRE WDP3-                                             
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008   -PRE WDB6-                                             
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200*01  -COPY W0008   -PRE WDR2-                                             
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500*01  -COPY W0008   -PRE WDR2ALT-                                          
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB W6D2A-PCB W6D2B-PCB           
025900                                    WDK6-PCB WDK7-PCB WDP3-PCB            
026000                                    WDB6-PCB WDR2-PCB WDR2ALT-PCB.        
026100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB W6D2A-PCB W6D2B-PCB           
026200                                    WDK6-PCB WDK7-PCB WDP3-PCB            
026300                                    WDB6-PCB WDR2-PCB WDR2ALT-PCB.        
026400                                                                          
026500     PERFORM IMS-GET-MSG                                                  
026600     IF SEGMENT-FINNS                                                     
026700       PERFORM A-INIT                                                     
026800       PERFORM B-KOLLA-NYCKLAR                                            
026900       IF NYCKLAR-OK                                                      
027000         IF MFS-UPDATE                                                    
027100           PERFORM G-KOLLA-INPUT                                          
027200           IF INDATA-OK                                                   
027300             PERFORM H-UPPDATERA                                          
027400           END-IF                                                         
027500         ELSE                                                             
027510           IF MID-TIKLOCK-9KOMPL-ENTER NOT NUMERIC                        
027520             MOVE ZERO TO MID-TIKLOCK-9KOMPL-ENTER                        
027530           END-IF                                                         
027540           IF MID-TIKLOCK-9KOMPL-NEXT NOT NUMERIC                         
027550             MOVE ZERO TO MID-TIKLOCK-9KOMPL-NEXT                         
027560           END-IF                                                         
027600           IF MFS-FIRST                                                   
027700             PERFORM C-FOERSTA-SIDA                                       
027800           ELSE                                                           
027900             IF MFS-NEXT                                                  
028000               PERFORM D-NAESTA-SIDA                                      
028100             ELSE                                                         
028200               PERFORM E-SAMMA-SIDA                                       
028300             END-IF                                                       
028400           END-IF                                                         
028500         END-IF                                                           
028600         IF INDATA-OK                                                     
028700           PERFORM F-LAES-VISA-INFO                                       
028800         END-IF                                                           
028900       END-IF                                                             
029000       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O21401 + 4                      
029100       PERFORM IMS-INSERT-MSG                                             
029200     END-IF                                                               
029300                                                                          
029400     MOVE ZERO TO RETURN-CODE                                             
029500     GOBACK                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 A-INIT SECTION.                                                          
029900                                                                          
030000     IF MSG-DUBBLA-TRANSKODER                                             
030100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I21401                 
030200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030400     ELSE                                                                 
030500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I21401                  
030600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
030700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030800     END-IF                                                               
030900                                                                          
031000     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
031100     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
031200     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
031300                                                                          
031400     MOVE LOW-VALUE        TO MSG-AREA                                    
031500     MOVE 'W6O214N1'       TO MFS-IDMOD                                   
031600     MOVE '6214'           TO MOD-IDTRANS                                 
031700     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
031800                                                                          
031900     IF EGEN-MID OR HELP-MID                                              
032000       CONTINUE                                                           
032100     ELSE                                                                 
032200       MOVE SPACE TO MFS-KDTRTYP                                          
032300       MOVE '7'   TO MFS-IDPFK                                            
032400     END-IF                                                               
032500                                                                          
032600     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
032700     ACCEPT DAGENS-TID   FROM TIME                                        
032800     .                                                                    
032900     EJECT                                                                
033000 B-KOLLA-NYCKLAR SECTION.                                                 
033100                                                                          
033200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033300     MOVE '001'             TO MSGI-KDCALL                                
033400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033600     MOVE '6214'            TO MSGI-IDTRANS                               
033700     IF GODK-MID                                                          
033800       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
033900     END-IF                                                               
034000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034100                                                                          
034200     MOVE JA TO NYCKLAR-SW                                                
034300                                                                          
034400     IF MSGI-IDLAND-SPR = 'GB'                                            
034500       MOVE +2 TO SPRAK-IX                                                
034600       MOVE 'GB ' TO MED-IDSKYLT                                          
034700     ELSE                                                                 
034800       MOVE +1 TO SPRAK-IX                                                
034900       MOVE 'S  ' TO MED-IDSKYLT                                          
035000     END-IF                                                               
035100                                                                          
035200*    -- KONTROLL AV IDARTNR                                               
035300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
035400                                                                          
035500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
035600       MOVE '7'         TO MFS-IDPFK                                      
035700       MOVE SPACE       TO MFS-KDTRTYP                                    
035800       MOVE JA          TO NYTT-ARTNR-SW                                  
035900     END-IF                                                               
036000     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
036100     IF MSGI-IDARTNR NUMERIC                                              
036200       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
036300     ELSE                                                                 
036400       MOVE NEJ TO NYCKLAR-SW                                             
036500     END-IF                                                               
036600                                                                          
036700*    -  KONTROLL AV IDKVAINF                                              
036800                                                                          
036900     MOVE MFS-RENSA-FAELT     TO MOD-IDKVAINF-IN                          
037000                                                                          
037100     IF GODK-MID                                                          
037200       IF MID-IDKVAINF-IN = ALL '+'                                       
037300         IF NYTT-ARTNR OR MFS-FIRST OR MFS-NEXT                           
037400           MOVE ZERO            TO WS-IDKVAINF                            
037500         ELSE                                                             
037600           MOVE MID-IDKVAINF-UT TO WS-IDKVAINF                            
037700         END-IF                                                           
037800       ELSE                                                               
037900         MOVE MID-IDKVAINF-IN   TO WS-IDKVAINF                            
038000       END-IF                                                             
038100       IF MID-IDKVAINF-UPPD = '99'                                        
038200         MOVE '99' TO WS-IDKVAINF                                         
038300       END-IF                                                             
038400     ELSE                                                                 
038500       MOVE ZERO                TO WS-IDKVAINF                            
038600     END-IF                                                               
038700     INSPECT WS-IDKVAINF REPLACING LEADING SPACE BY ZERO                  
038800                                                                          
038900*    -- KONTROLL AV TIREGDAT                                              
039000     MOVE MFS-RENSA-FAELT     TO MOD-TIREGDAT-IN                          
039100                                                                          
039200     IF GODK-MID                                                          
039300       IF MID-TIREGDAT-IN = ALL '+'                                       
039400         IF NYTT-ARTNR                                                    
039500           MOVE ZERO            TO WS-TIREGDAT                            
039600         ELSE                                                             
039700           IF MID-IDKVAINF-IN = ALL '+'                                   
039800             MOVE MID-TIREGDAT-UT TO WS-TIREGDAT                          
039900           ELSE                                                           
040000             MOVE ZERO            TO WS-TIREGDAT                          
040100           END-IF                                                         
040200         END-IF                                                           
040300       ELSE                                                               
040400         MOVE MID-TIREGDAT-IN   TO WS-TIREGDAT                            
040500       END-IF                                                             
040600     ELSE                                                                 
040700       MOVE ZERO                TO WS-TIREGDAT                            
040800     END-IF                                                               
040900     INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO                  
041000     IF WS-TIREGDAT NUMERIC                                               
041100       CONTINUE                                                           
041200     ELSE                                                                 
041300       MOVE NEJ TO NYCKLAR-SW                                             
041400     END-IF                                                               
041500                                                                          
041600*    -- KONTROLL AV IDDC                                                  
041700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
041800                                                                          
041900     IF EGEN-MID                                                          
042000       IF MID-IDDC-IN NOT = ALL '+'                                       
042100         MOVE '7'         TO MFS-IDPFK                                    
042200         MOVE SPACE       TO MFS-KDTRTYP                                  
042300         MOVE MID-IDDC-IN TO W-IDDC-B6                                    
042400       ELSE                                                               
042500         MOVE MID-IDDC-UT TO W-IDDC-B6                                    
042600       END-IF                                                             
042700     ELSE                                                                 
042800       MOVE MSGI-IDDC TO W-IDDC-B6                                        
042900     END-IF                                                               
043000     PERFORM IMS-GU-WDB601                                                
043100                                                                          
043200     IF DCS-KDDC = SPACE OR DCS-DDC                                       
043300       MOVE NEJ TO NYCKLAR-SW                                             
043400     END-IF                                                               
043500                                                                          
043600     IF GODK-MID OR NYCKLAR-OK                                            
043700       MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                
043800       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
043900       MOVE DCS-IDDC     TO MOD-IDDC-UT                                   
044000                            W-IDDC                                        
044100       MOVE WS-IDKVAINF  TO W-IDKVAINF                                    
044200                            MOD-IDKVAINF-UT                               
044300       INSPECT MOD-IDKVAINF-UT REPLACING LEADING ZERO BY SPACE            
044400       IF NYCKLAR-OK                                                      
044500         MOVE WS-TIREGDAT  TO W1-DAREGDAT                                 
044600                              W-DAREGDAT-9KOMPL                           
044700                              MOD-TIREGDAT-UT                             
044800         IF WS-TIREGDAT NOT = ZERO                                        
044900           IF WS-TIREGDAT < 500000                                        
045000             MOVE 20       TO W1-DAREGDAT (1:2)                           
045100           ELSE                                                           
045200             IF WS-TIREGDAT < 999999                                      
045300               MOVE 19     TO W1-DAREGDAT (1:2)                           
045400             ELSE                                                         
045500               MOVE 99999999 TO W1-DAREGDAT                               
045600             END-IF                                                       
045700           END-IF                                                         
045800           COMPUTE W-DAREGDAT-9KOMPL = 99999999 - W1-DAREGDAT             
045900         END-IF                                                           
046000       END-IF                                                             
046100     ELSE                                                                 
046200       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
046300       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
046400     END-IF                                                               
046500                                                                          
046600     IF NYCKLAR-FEL                                                       
046700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
046800       CALL WMEDKONV   USING MED-WMEDAREA                                 
046900       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
047000       PERFORM MFS-RENSA-FAELT-IN                                         
047100       PERFORM MFS-RENSA-FAELT-UT                                         
047200     END-IF                                                               
047300     .                                                                    
047400     EJECT                                                                
047500 C-FOERSTA-SIDA SECTION.                                                  
047600                                                                          
047700     MOVE INF-FIRST-PAGE           TO MED-IDMFSINF                        
047800     CALL WMEDKONV              USING MED-WMEDAREA                        
047900     MOVE MED-MFSINF               TO MOD-TEMFSINF                        
048000                                                                          
048100*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
048200     MOVE ZERO                     TO MOD-TIREGDAT-9KOMPL-ENTER           
048300     MOVE ZERO                     TO MOD-TIKLOCK-9KOMPL-ENTER            
048400     MOVE ZERO                     TO MOD-TIREGDAT-9KOMPL-NEXT            
048500     MOVE ZERO                     TO MOD-TIKLOCK-9KOMPL-NEXT             
048600                                                                          
048700     PERFORM MFS-RENSA-FAELT-IN                                           
048800                                                                          
048900     IF MID-TIREGDAT-IN = ALL '+'                                         
049000       MOVE 0                         TO W-DAREGDAT-9KOMPL (1:1)          
049100       MOVE MID-TIKLOCK-9KOMPL-ENTER  TO W-TIKLOCK-9KOMPL                 
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 D-NAESTA-SIDA SECTION.                                                   
049600                                                                          
049700     MOVE MID-TIREGDAT-9KOMPL-NEXT TO W-DAREGDAT-9KOMPL                   
049800     IF MID-TIREGDAT-9KOMPL-NEXT (1:1) = 0                                
049900       MOVE 80                     TO W-DAREGDAT-9KOMPL (1:2)             
050000     ELSE                                                                 
050100       MOVE 79                     TO W-DAREGDAT-9KOMPL (1:2)             
050200     END-IF                                                               
050300     MOVE MID-TIKLOCK-9KOMPL-NEXT  TO W-TIKLOCK-9KOMPL                    
050400                                                                          
050500     PERFORM MFS-RENSA-FAELT-IN                                           
050600     .                                                                    
050700     EJECT                                                                
050800 E-SAMMA-SIDA SECTION.                                                    
050900                                                                          
051000     IF EGEN-MID OR HELP-MID                                              
051100       IF MID-INPUT = ALL '+'                                             
051200         PERFORM MFS-RENSA-FAELT-IN                                       
051300       ELSE                                                               
051400         MOVE NEJ TO FL-LAES                                              
051500         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
051600         CALL WMEDKONV    USING MED-WMEDAREA                              
051700         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
051800         PERFORM EA-MID-INDATA-TILL-MOD                                   
051900       END-IF                                                             
052000     ELSE                                                                 
052100       PERFORM MFS-RENSA-FAELT-IN                                         
052200     END-IF                                                               
052300                                                                          
052400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
052500        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
052600        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
052700     END-IF                                                               
052800                                                                          
052900     IF MID-IDKVAINF-IN NOT = ALL '+'                                     
053000        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
053100        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
053200        MOVE SPACE                 TO MOD-TIREGDAT-UT                     
053300     END-IF                                                               
053400                                                                          
053500     IF MID-TIREGDAT-IN = ALL '+'                                         
053600        MOVE MID-TIREGDAT-9KOMPL-ENTER TO W-DAREGDAT-9KOMPL               
053700        IF MID-TIREGDAT-9KOMPL-ENTER  (1:1) = 0                           
053800          MOVE 80                      TO W-DAREGDAT-9KOMPL (1:2)         
053900        ELSE                                                              
054000          MOVE 79                      TO W-DAREGDAT-9KOMPL (1:2)         
054100        END-IF                                                            
054200        MOVE MID-TIKLOCK-9KOMPL-ENTER  TO W-TIKLOCK-9KOMPL                
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600 EA-MID-INDATA-TILL-MOD SECTION.                                          
054700                                                                          
054800     IF MID-IDKVAINF-UPPD NOT = ALL '+'                                   
054900        MOVE MID-IDKVAINF-UPPD     TO MOD-IDKVAINF-UPPD                   
055000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKVAINF-UPPD-ATTR              
055100     ELSE                                                                 
055200        MOVE MFS-RENSA-FAELT       TO MOD-IDKVAINF-UPPD                   
055300     END-IF                                                               
055400                                                                          
055500     IF MID-TISTADAT-IN NOT = ALL '+'                                     
055600        MOVE MID-TISTADAT-IN       TO MOD-TISTADAT-IN                     
055700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTADAT-IN-ATTR                
055800     ELSE                                                                 
055900        MOVE MFS-RENSA-FAELT       TO MOD-TISTADAT-IN                     
056000     END-IF                                                               
056100                                                                          
056200     IF MID-TISTODAT-IN NOT = ALL '+'                                     
056300        MOVE MID-TISTODAT-IN       TO MOD-TISTODAT-IN                     
056400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTODAT-IN-ATTR                
056500     ELSE                                                                 
056600        MOVE MFS-RENSA-FAELT       TO MOD-TISTODAT-IN                     
056700     END-IF                                                               
056800                                                                          
056900     IF MID-KVANTAL-IN NOT = ALL '+'                                      
057000        MOVE MID-KVANTAL-IN        TO MOD-KVANTAL-IN                      
057100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-IN-ATTR                 
057200     ELSE                                                                 
057300        MOVE MFS-RENSA-FAELT       TO MOD-KVANTAL-IN                      
057400     END-IF                                                               
057500                                                                          
057600     IF MID-KVAVV-KVAL-IN NOT = ALL '+'                                   
057700        MOVE MID-KVAVV-KVAL-IN     TO MOD-KVAVV-KVAL-IN                   
057800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAVV-KVAL-IN-ATTR              
057900     ELSE                                                                 
058000        MOVE MFS-RENSA-FAELT       TO MOD-KVAVV-KVAL-IN                   
058100     END-IF                                                               
058200                                                                          
058300     IF MID-KVART-SKROT-IN NOT = ALL '+'                                  
058400        MOVE MID-KVART-SKROT-IN    TO MOD-KVART-SKROT-IN                  
058500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-SKROT-IN-ATTR             
058600     ELSE                                                                 
058700        MOVE MFS-RENSA-FAELT       TO MOD-KVART-SKROT-IN                  
058800     END-IF                                                               
058900                                                                          
059000     IF MID-KVART-RET-IN NOT = ALL '+'                                    
059100        MOVE MID-KVART-RET-IN      TO MOD-KVART-RET-IN                    
059200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-RET-IN-ATTR               
059300     ELSE                                                                 
059400        MOVE MFS-RENSA-FAELT       TO MOD-KVART-RET-IN                    
059500     END-IF                                                               
059600                                                                          
059700     IF MID-KVART-KJUST-IN NOT = ALL '+'                                  
059800        MOVE MID-KVART-KJUST-IN    TO MOD-KVART-KJUST-IN                  
059900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-KJUST-IN-ATTR             
060000     ELSE                                                                 
060100        MOVE MFS-RENSA-FAELT       TO MOD-KVART-KJUST-IN                  
060200     END-IF                                                               
060300                                                                          
060400     IF MID-BEINIT NOT = ALL '+'                                          
060500        MOVE MID-BEINIT            TO MOD-BEINIT                          
060600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEINIT-ATTR                     
060700     ELSE                                                                 
060800        MOVE MFS-RENSA-FAELT       TO MOD-BEINIT                          
060900     END-IF                                                               
061000                                                                          
061100     IF MID-IDNAMN NOT = ALL '+'                                          
061200        MOVE MID-IDNAMN            TO MOD-IDNAMN-DC-QUAL                  
061300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDNAMN-DC-QUAL-ATTR             
061400     ELSE                                                                 
061500        MOVE MFS-RENSA-FAELT       TO MOD-IDNAMN-DC-QUAL                  
061600     END-IF                                                               
061700                                                                          
061800     IF MID-TEKVAINF-DC-RAD1 NOT = ALL '+'                                
061900       MOVE MID-TEKVAINF-DC-RAD1  TO MOD-TEKVAINF-DC-RAD1                 
062000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-DC-RAD1-ATTR            
062100     ELSE                                                                 
062200       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-DC-RAD1                 
062300     END-IF                                                               
062400                                                                          
062500     IF MID-TEKVAINF-DC-RAD2 NOT = ALL '+'                                
062600       MOVE MID-TEKVAINF-DC-RAD2  TO MOD-TEKVAINF-DC-RAD2                 
062700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-DC-RAD2-ATTR            
062800     ELSE                                                                 
062900       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-DC-RAD2                 
063000     END-IF                                                               
063100                                                                          
063200     IF MID-TEKVAINF-DC-RAD3 NOT = ALL '+'                                
063300       MOVE MID-TEKVAINF-DC-RAD3  TO MOD-TEKVAINF-DC-RAD3                 
063400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-DC-RAD3-ATTR            
063500     ELSE                                                                 
063600       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-DC-RAD3                 
063700     END-IF                                                               
063800                                                                          
063900     IF MID-TEKVAINF-DC-RAD4 NOT = ALL '+'                                
064000       MOVE MID-TEKVAINF-DC-RAD4  TO MOD-TEKVAINF-DC-RAD4                 
064100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-DC-RAD4-ATTR            
064200     ELSE                                                                 
064300       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-DC-RAD4                 
064400     END-IF                                                               
064500                                                                          
064600     IF MID-FLTABORT NOT = ALL '+'                                        
064700       MOVE MID-FLTABORT          TO MOD-FLTABORT                         
064800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTABORT-ATTR                    
064900     ELSE                                                                 
065000       MOVE MFS-RENSA-FAELT       TO MOD-FLTABORT                         
065100     END-IF                                                               
065200     .                                                                    
065300     EJECT                                                                
065400 F-LAES-VISA-INFO SECTION.                                                
065500                                                                          
065600     PERFORM IMS-GU-WDK611                                                
065700                                                                          
065800     IF SEGMENT-FINNS                                                     
065900       IF DCS-CDC                                                         
066000         CONTINUE                                                         
066100       ELSE                                                               
066200         PERFORM IMS-GU-WDK711                                            
066300       END-IF                                                             
066400     END-IF                                                               
066500                                                                          
066600     IF SEGMENT-FINNS                                                     
066700       PERFORM IMS-GU-W6D201                                              
066800                                                                          
066900       IF SEGMENT-FINNS                                                   
067000         MOVE LOW-VALUE            TO W-IDKVAINF-MIN-X                    
067100         MOVE HIGH-VALUE           TO W-IDKVAINF-MAX-X                    
067200         IF WS-IDKVAINF > ZERO                                            
067300            MOVE W-IDKVAINF        TO W-IDKVAINF-MIN                      
067400                                      W-IDKVAINF-MAX                      
067500            MOVE ZERO              TO W-DAREGDAT-9KOMPL                   
067600         END-IF                                                           
067700                                                                          
067800         PERFORM FA-LAES-GRUNDDATA                                        
067900       ELSE                                                               
068000         MOVE ERR-MISSING-KEY TO MED-IDMFSFEL                             
068100         CALL WMEDKONV     USING MED-WMEDAREA                             
068200         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
068300         PERFORM MFS-RENSA-FAELT-UT                                       
068400       END-IF                                                             
068500     ELSE                                                                 
068600       MOVE ERR-MISSING-KEY TO MED-IDMFSFEL                               
068700       CALL WMEDKONV     USING MED-WMEDAREA                               
068800       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
068900       PERFORM MFS-RENSA-FAELT-UT                                         
069000     END-IF                                                               
069100     .                                                                    
069200     EJECT                                                                
069300 FA-LAES-GRUNDDATA SECTION.                                               
069400                                                                          
069500     PERFORM IMS-GNP-W6D211                                               
069600                                                                          
069700     IF SEGMENT-FINNS                                                     
069800                                                                          
069900       MOVE INFO-DAREGDAT-9KOMPL      TO W-DAREGDAT-9KOMPL                
070000       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO MOD-TIREGDAT-9KOMPL-ENTER        
070100       MOVE INFO-TIKLOCK-9KOMPL       TO W-TIKLOCK-9KOMPL                 
070200                                         MOD-TIKLOCK-9KOMPL-ENTER         
070300                                                                          
070400       COMPUTE W1-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL              
070500*      MOVE W1-DAREGDAT (3:6)  TO MOD-TIREGDAT                            
070600*                                 MOD-TIREGDAT-UT                         
070700       MOVE INFO-TIREGDAT      TO MOD-TIREGDAT                            
070800       MOVE MOD-TIREGDAT       TO MOD-TIREGDAT-UT                         
070900       MOVE INFO-KDKVAINF      TO MOD-KDKVAINF                            
071000       MOVE INFO-KDPERSON      TO W-IDPERSON                              
071100       PERFORM IMS-GU-WDP311                                              
071200       IF SEGMENT-FINNS                                                   
071300         MOVE PERS-IDNAMN(1:26) TO MOD-IDNAMN                             
071400         MOVE PERS-IDMAIL       TO MOD-IDMAIL                             
071500       ELSE                                                               
071600         MOVE 'QUAL '           TO MOD-IDNAMN(1:5)                        
071700         MOVE INFO-KDPERSON     TO MOD-IDNAMN(6:3)                        
071800       END-IF                                                             
071900                                                                          
072000       MOVE +1 TO INDX                                                    
072100       PERFORM UNTIL INDX > 7                                             
072200         MOVE INFO-TEKVAINF-EXT(INDX) TO MOD-TEKVAINF(INDX)               
072300         ADD +1 TO INDX                                                   
072400       END-PERFORM                                                        
072500                                                                          
072600       MOVE INFO-IDKVAINF             TO MOD-IDKVAINF-UPPD                
072700                                         MOD-IDKVAINF-UT                  
072800                                                                          
072900       MOVE INFO-TIKLAR-LEV           TO MOD-TIKLAR-LEV                   
073000       MOVE INFO-FLSTOCH              TO MOD-FLSTOCH                      
073100                                                                          
073200       PERFORM FB-LAES-DCDATA                                             
073300                                                                          
073400       MOVE LOW-VALUE                 TO W-IDKVAINF-MIN-X                 
073500       PERFORM IMS-GNP-W6D211                                             
073600                                                                          
073700     ELSE                                                                 
073800       MOVE MFS-RENSA-FAELT         TO MOD-IDKVAINF-UPPD                  
073900                                       MOD-TIREGDAT                       
074000                                       MOD-KDKVAINF                       
074100                                       MOD-IDNAMN                         
074200                                       MOD-IDMAIL                         
074300                                       MOD-TIKLAR-LEV                     
074400                                       MOD-FLSTOCH                        
074500                                       MOD-IDKVAINF-UT                    
074600       MOVE +1 TO INDX                                                    
074700       PERFORM UNTIL INDX > 7                                             
074800         MOVE MFS-RENSA-FAELT TO MOD-TEKVAINF(INDX)                       
074900         ADD +1 TO INDX                                                   
075000       END-PERFORM                                                        
075100       MOVE ERR-MISSING-KEY TO MED-IDMFSFEL                               
075200       CALL WMEDKONV     USING MED-WMEDAREA                               
075300       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
075400       PERFORM MFS-RENSA-FAELT-UT                                         
075500     END-IF                                                               
075600                                                                          
075610     IF INFO-DAREGDAT-9KOMPL(2:7) NUMERIC                                 
075700       MOVE INFO-DAREGDAT-9KOMPL(2:7) TO MOD-TIREGDAT-9KOMPL-NEXT         
075710     ELSE                                                                 
075711       MOVE ZERO                      TO MOD-TIREGDAT-9KOMPL-NEXT         
075720     END-IF                                                               
075730     IF INFO-TIKLOCK-9KOMPL NUMERIC                                       
075800       MOVE INFO-TIKLOCK-9KOMPL       TO MOD-TIKLOCK-9KOMPL-NEXT          
075810     ELSE                                                                 
075811       MOVE ZERO                      TO MOD-TIKLOCK-9KOMPL-NEXT          
075820     END-IF                                                               
075900     IF SEGMENT-FINNS                                                     
076000       IF MFS-UPDATE OR FL-LAES = NEJ                                     
076100         CONTINUE                                                         
076200       ELSE                                                               
076300         MOVE INF-MORE-INFO-EXISTS      TO MED-IDMFSINF                   
076400         CALL WMEDKONV               USING MED-WMEDAREA                   
076500         MOVE MED-MFSINF                TO MOD-TEMFSINF                   
076600       END-IF                                                             
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 FB-LAES-DCDATA SECTION.                                                  
077100                                                                          
077200     PERFORM IMS-GU-W6D221                                                
077300     IF SEGMENT-FINNS                                                     
077400       MOVE DCQ-TISTADAT          TO MOD-TISTADAT-UT                      
077500       MOVE DCQ-TISTODAT          TO MOD-TISTODAT-UT                      
077600       MOVE DCQ-KVANTAL           TO MOD-KVANTAL-UT                       
077700       MOVE DCQ-KVAVV-KVAL        TO MOD-KVAVV-KVAL-UT                    
077800       MOVE DCQ-KVART-SKROT       TO MOD-KVART-SKROT-UT                   
077900       MOVE DCQ-KVART-RET         TO MOD-KVART-RET-UT                     
078000       MOVE DCQ-KVART-KJUST       TO MOD-KVART-KJUST-UT                   
078100       IF EGEN-MID AND FL-LAES = NEJ                                      
078200         IF MID-BEINIT = ALL '+'                                          
078300           MOVE DCQ-BEINIT            TO MOD-BEINIT                       
078400         END-IF                                                           
078500         IF MID-IDNAMN = ALL '+'                                          
078600           MOVE DCQ-IDNAMN            TO MOD-IDNAMN-DC-QUAL               
078700         END-IF                                                           
078800         IF MID-TEKVAINF-DC-RAD1 = ALL '+'                                
078900           MOVE DCQ-TEKVAINF(1)(1:63) TO MOD-TEKVAINF-DC-RAD1             
079000         END-IF                                                           
079100         IF MID-TEKVAINF-DC-RAD2 = ALL '+'                                
079200           MOVE DCQ-TEKVAINF(2)       TO MOD-TEKVAINF-DC-RAD2             
079300         END-IF                                                           
079400         IF MID-TEKVAINF-DC-RAD3 = ALL '+'                                
079500           MOVE DCQ-TEKVAINF(3)       TO MOD-TEKVAINF-DC-RAD3             
079600         END-IF                                                           
079700         IF MID-TEKVAINF-DC-RAD4 = ALL '+'                                
079800           MOVE DCQ-TEKVAINF(4)       TO MOD-TEKVAINF-DC-RAD4             
079900         END-IF                                                           
080000       ELSE                                                               
080100         MOVE DCQ-BEINIT            TO MOD-BEINIT                         
080200         MOVE DCQ-IDNAMN            TO MOD-IDNAMN-DC-QUAL                 
080300         MOVE DCQ-TEKVAINF(1)(1:63) TO MOD-TEKVAINF-DC-RAD1               
080400         MOVE DCQ-TEKVAINF(2)       TO MOD-TEKVAINF-DC-RAD2               
080500         MOVE DCQ-TEKVAINF(3)       TO MOD-TEKVAINF-DC-RAD3               
080600         MOVE DCQ-TEKVAINF(4)       TO MOD-TEKVAINF-DC-RAD4               
080700       END-IF                                                             
080800     ELSE                                                                 
080900       IF EGEN-MID AND FL-LAES = NEJ                                      
081000         CONTINUE                                                         
081100       ELSE                                                               
081200         IF INFO-IDKVAINF = 99                                            
081300           PERFORM FA-LAES-GRUNDDATA                                      
081400         ELSE                                                             
081500           MOVE MFS-RENSA-FAELT     TO MOD-TISTADAT-UT                    
081600                                       MOD-TISTODAT-UT                    
081700                                       MOD-KVANTAL-UT                     
081800                                       MOD-KVAVV-KVAL-UT                  
081900                                       MOD-KVART-SKROT-UT                 
082000                                       MOD-KVART-RET-UT                   
082100                                       MOD-KVART-KJUST-UT                 
082200                                       MOD-BEINIT                         
082300                                       MOD-IDNAMN-DC-QUAL                 
082400                                       MOD-TEKVAINF-DC-RAD1               
082500                                       MOD-TEKVAINF-DC-RAD2               
082600                                       MOD-TEKVAINF-DC-RAD3               
082700                                       MOD-TEKVAINF-DC-RAD4               
082800                                       MOD-FLTABORT                       
082900           PERFORM MFS-RENSA-FAELT-IN                                     
083000         END-IF                                                           
083100       END-IF                                                             
083200     END-IF                                                               
083300     .                                                                    
083400     EJECT                                                                
083500 G-KOLLA-INPUT SECTION.                                                   
083600                                                                          
083700     MOVE JA  TO INDATA-SW                                                
083800     IF MID-INPUT = ALL '+'                                               
083900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
084000       CALL WMEDKONV USING MED-WMEDAREA                                   
084100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
084200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
084300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
084400       MOVE NEJ TO INDATA-SW                                              
084500     ELSE                                                                 
084600       PERFORM IMS-GU-WDK611                                              
084700       IF SEGMENT-FINNS                                                   
084800         IF DCS-CDC                                                       
084900           CONTINUE                                                       
085000         ELSE                                                             
085100           PERFORM IMS-GU-WDK711                                          
085200         END-IF                                                           
085300       END-IF                                                             
085400                                                                          
085500       IF SEGMENT-SAKNAS                                                  
085600         MOVE NEJ                       TO INDATA-SW                      
085700       ELSE                                                               
085800         PERFORM IMS-GU-W6D201                                            
085900         IF SEGMENT-FINNS                                                 
086000           PERFORM IMS-GU-W6D211                                          
086100           IF SEGMENT-SAKNAS                                              
086200             IF MID-IDKVAINF-UPPD = '99'                                  
086300               MOVE JA     TO FL-INTERN                                   
086400             ELSE                                                         
086500               MOVE NEJ    TO INDATA-SW                                   
086600             END-IF                                                       
086700           END-IF                                                         
086800         ELSE                                                             
086900           IF MID-IDKVAINF-UPPD = '99'                                    
087000              MOVE W-IDARTNR   TO KVAH-ART-IDARTNR                        
087100              MOVE SPACE       TO KVAH-ART-ADKVAULG                       
087200              MOVE '0000'      TO KVAH-ART-KDKVAKTL                       
087300              PERFORM IMS-ISRT-W6D201                                     
087400              MOVE JA     TO FL-INTERN                                    
087500           ELSE                                                           
087600              MOVE NEJ    TO INDATA-SW                                    
087700           END-IF                                                         
087800         END-IF                                                           
087900                                                                          
088000         IF MID-BEINIT NOT = ALL '+'                                      
088100           IF MID-BEINIT = SPACE                                          
088200             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEINIT-ATTR                   
088300             MOVE NEJ TO INDATA-SW                                        
088400           END-IF                                                         
088500         END-IF                                                           
088600                                                                          
088700         IF MID-TISTADAT-IN NOT = ALL '+'                                 
088800           IF MID-TISTADAT-IN NUMERIC                                     
088900             MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-IN-ATTR             
089000           ELSE                                                           
089100             MOVE MFS-NUM-FAELT-FEL   TO MOD-TISTADAT-IN-ATTR             
089200             MOVE NEJ TO INDATA-SW                                        
089300           END-IF                                                         
089400         END-IF                                                           
089500                                                                          
089600         IF MID-TISTODAT-IN NOT = ALL '+'                                 
089700           IF MID-TISTODAT-IN NUMERIC                                     
089800             MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTODAT-IN-ATTR             
089900           ELSE                                                           
090000             MOVE MFS-NUM-FAELT-FEL   TO MOD-TISTODAT-IN-ATTR             
090100             MOVE NEJ TO INDATA-SW                                        
090200           END-IF                                                         
090300         END-IF                                                           
090400                                                                          
090500         IF MID-KVANTAL-IN NOT = ALL '+'                                  
090600           IF MID-KVANTAL-IN NUMERIC                                      
090700             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-IN-ATTR              
090800           ELSE                                                           
090900             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVANTAL-IN-ATTR              
091000             MOVE NEJ TO INDATA-SW                                        
091100           END-IF                                                         
091200         END-IF                                                           
091300                                                                          
091400         IF MID-KVAVV-KVAL-IN NOT = ALL '+'                               
091500           IF MID-KVAVV-KVAL-IN NUMERIC                                   
091600             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVAVV-KVAL-IN-ATTR           
091700           ELSE                                                           
091800             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVAVV-KVAL-IN-ATTR           
091900             MOVE NEJ TO INDATA-SW                                        
092000           END-IF                                                         
092100         END-IF                                                           
092200                                                                          
092300         IF MID-KVART-SKROT-IN NOT = ALL '+'                              
092400           IF MID-KVART-SKROT-IN NUMERIC                                  
092500             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-SKROT-IN-ATTR          
092600           ELSE                                                           
092700             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVART-SKROT-IN-ATTR          
092800             MOVE NEJ TO INDATA-SW                                        
092900           END-IF                                                         
093000         END-IF                                                           
093100                                                                          
093200         IF MID-KVART-RET-IN NOT = ALL '+'                                
093300           IF MID-KVART-RET-IN NUMERIC                                    
093400             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-RET-IN-ATTR            
093500           ELSE                                                           
093600             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVART-RET-IN-ATTR            
093700             MOVE NEJ TO INDATA-SW                                        
093800           END-IF                                                         
093900         END-IF                                                           
094000                                                                          
094100         IF MID-KVART-KJUST-IN NOT = ALL '+'                              
094200           IF MID-KVART-KJUST-IN NUMERIC                                  
094300             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-KJUST-IN-ATTR          
094400           ELSE                                                           
094500             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVART-KJUST-IN-ATTR          
094600             MOVE NEJ TO INDATA-SW                                        
094700           END-IF                                                         
094800         END-IF                                                           
094900                                                                          
095000         IF MID-FLTABORT NOT = ALL '+'                                    
095100           IF MID-FLTABORT = JA OR YES                                    
095200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTABORT-ATTR               
095300           ELSE                                                           
095400             MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTABORT-ATTR               
095500             MOVE NEJ                  TO INDATA-SW                       
095600           END-IF                                                         
095700         END-IF                                                           
095800       END-IF                                                             
095900                                                                          
096000       IF INDATA-FEL                                                      
096100         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
096200         CALL WMEDKONV USING MED-WMEDAREA                                 
096300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
096400         PERFORM GA-MID-INDATA-TILL-MOD                                   
096500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
096600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
096700       ELSE                                                               
096800         IF DCS-IDDC  NOT = MSGI-IDDC                                     
096900           IF DCS-NDC-NA  AND                                             
097000              (MSGI-IDDC(1:1) = '4' OR '5' OR '9')                        
097100              CONTINUE                                                    
097200           ELSE                                                           
097300             PERFORM GB-KOLLA-DC-TRAD                                     
097400             IF INDATA-FEL                                                
097500               MOVE NEJ TO INDATA-SW                                      
097600               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
097700               CALL WMEDKONV USING MED-WMEDAREA                           
097800               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
097900*** LÄGG EFTER ÄNDRINGSSTOPP UPP FELTEXT I MEDKONV                        
098000               IF MSGI-IDLAND-SPR = 'GB'                                  
098100                 MOVE 'UPDATING OTHER DC:S ARE NOT ALLOWED'               
098200                                                 TO MOD-TEMFSFEL          
098300               ELSE                                                       
098400                 MOVE 'DU FÅR EJ UPPDATERA PÅ ANNAT DC'                   
098500                                                 TO MOD-TEMFSFEL          
098600               END-IF                                                     
098700               PERFORM MFS-ROER-EJ-FAELT-UT                               
098800             END-IF                                                       
098900           END-IF                                                         
099000         END-IF                                                           
099100       END-IF                                                             
099200       IF INDATA-OK                                                       
099300         PERFORM IMS-GU-W6D221-2                                          
099400         IF SEGMENT-FINNS                                                 
099500           IF DCQ-FLSLUT = JA                                             
099600             MOVE NEJ TO INDATA-SW                                        
099700             MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
099800             CALL WMEDKONV USING MED-WMEDAREA                             
099900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
100000             PERFORM MFS-ROER-EJ-FAELT-IN                                 
100100             PERFORM MFS-ROER-EJ-FAELT-UT                                 
100200           END-IF                                                         
100300                                                                          
100400         ELSE                                                             
100500           IF MID-BEINIT = ALL '+'                                        
100600             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEINIT-ATTR                   
100700             MOVE NEJ TO INDATA-SW                                        
100800             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
100900             CALL WMEDKONV USING MED-WMEDAREA                             
101000             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
101100             PERFORM GA-MID-INDATA-TILL-MOD                               
101200             PERFORM MFS-ROER-EJ-FAELT-IN                                 
101300             PERFORM MFS-ROER-EJ-FAELT-UT                                 
101400           ELSE                                                           
101500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEINIT-ATTR                 
101600           END-IF                                                         
101610         END-IF                                                           
101700         IF WS-IDKVAINF = '99'                                            
101800           CONTINUE                                                       
101900         ELSE                                                             
102000           IF MID-FLTABORT = JA OR YES                                    
102100             MOVE NEJ TO INDATA-SW                                        
102200             MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
102300             CALL WMEDKONV USING MED-WMEDAREA                             
102400             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
102500             PERFORM MFS-ROER-EJ-FAELT-IN                                 
102600             PERFORM MFS-ROER-EJ-FAELT-UT                                 
102700           END-IF                                                         
102800         END-IF                                                           
103000       END-IF                                                             
103100     END-IF                                                               
103200     .                                                                    
103300     EJECT                                                                
103400 GA-MID-INDATA-TILL-MOD SECTION.                                          
103500                                                                          
103600     IF MID-TISTADAT-IN NOT = ALL '+'                                     
103700        MOVE MID-TISTADAT-IN       TO MOD-TISTADAT-IN                     
103800     END-IF                                                               
103900                                                                          
104000     IF MID-TISTODAT-IN NOT = ALL '+'                                     
104100        MOVE MID-TISTODAT-IN       TO MOD-TISTODAT-IN                     
104200     END-IF                                                               
104300                                                                          
104400     IF MID-KVANTAL-IN NOT = ALL '+'                                      
104500        MOVE MID-KVANTAL-IN        TO MOD-KVANTAL-IN                      
104600     END-IF                                                               
104700                                                                          
104800     IF MID-KVAVV-KVAL-IN NOT = ALL '+'                                   
104900        MOVE MID-KVAVV-KVAL-IN     TO MOD-KVAVV-KVAL-IN                   
105000     END-IF                                                               
105100                                                                          
105200     IF MID-KVART-SKROT-IN NOT = ALL '+'                                  
105300        MOVE MID-KVART-SKROT-IN    TO MOD-KVART-SKROT-IN                  
105400     END-IF                                                               
105500                                                                          
105600     IF MID-KVART-RET-IN NOT = ALL '+'                                    
105700        MOVE MID-KVART-RET-IN      TO MOD-KVART-RET-IN                    
105800     END-IF                                                               
105900                                                                          
106000     IF MID-KVART-KJUST-IN NOT = ALL '+'                                  
106100        MOVE MID-KVART-KJUST-IN    TO MOD-KVART-KJUST-IN                  
106200     END-IF                                                               
106300                                                                          
106400     IF MID-BEINIT NOT = ALL '+'                                          
106500        MOVE MID-BEINIT            TO MOD-BEINIT                          
106600     END-IF                                                               
106700                                                                          
106800     IF MID-IDNAMN NOT = ALL '+'                                          
106900        MOVE MID-IDNAMN            TO MOD-IDNAMN-DC-QUAL                  
107000        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDNAMN-DC-QUAL-ATTR             
107100     END-IF                                                               
107200                                                                          
107300     IF MID-TEKVAINF-DC-RAD1 NOT = ALL '+'                                
107400       MOVE MID-TEKVAINF-DC-RAD1  TO MOD-TEKVAINF-DC-RAD1                 
107500       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-DC-RAD1-ATTR            
107600     END-IF                                                               
107700                                                                          
107800     IF MID-TEKVAINF-DC-RAD2 NOT = ALL '+'                                
107900       MOVE MID-TEKVAINF-DC-RAD2  TO MOD-TEKVAINF-DC-RAD2                 
108000       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-DC-RAD2-ATTR            
108100     END-IF                                                               
108200                                                                          
108300     IF MID-TEKVAINF-DC-RAD3 NOT = ALL '+'                                
108400       MOVE MID-TEKVAINF-DC-RAD3  TO MOD-TEKVAINF-DC-RAD3                 
108500       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-DC-RAD3-ATTR            
108600     END-IF                                                               
108700                                                                          
108800     IF MID-TEKVAINF-DC-RAD4 NOT = ALL '+'                                
108900       MOVE MID-TEKVAINF-DC-RAD4  TO MOD-TEKVAINF-DC-RAD4                 
109000       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-DC-RAD4-ATTR            
109100     END-IF                                                               
109200                                                                          
109300     IF MID-FLTABORT NOT = ALL '+'                                        
109400       MOVE MID-FLTABORT          TO MOD-FLTABORT                         
109500     END-IF                                                               
109600     .                                                                    
109700     EJECT                                                                
109800 GB-KOLLA-DC-TRAD SECTION.                                                
109900*** KOLLA OM MSGI IDUSER = LEVEL2                                         
110000     MOVE NEJ TO INDATA-SW                                                
110100     MOVE NEJ TO POST-FINNS-SW                                            
110200     MOVE MSGI-IDDC   TO W-IDDC-6332                                      
110300     PERFORM IMS-GHU-WDGX6332                                             
110400     IF SEGMENT-FINNS                                                     
110500       MOVE JA                TO INDATA-SW                                
110600       MOVE DCS-IDDC          TO W-IDDC-6334                              
110700       PERFORM IMS-GET-WDGX6334-UNIK                                      
110800       IF SEGMENT-SAKNAS                                                  
110900         IF MSGI-IDDC NOT = DCS-IDDC                                      
111000           MOVE DCS-IDDC    TO W-IDDC-6332                                
111100           PERFORM IMS-GHU-WDGX6332                                       
111200           IF SEGMENT-FINNS                                               
111300             IF 6332-IDUSER(1) NOT = SPACE                                
111400               IF MSGI-IDUSER NOT = 6332-IDUSER(1)                        
111500                 MOVE NEJ TO INDATA-SW                                    
111600               ELSE                                                       
111700                 MOVE JA  TO INDATA-SW                                    
111800               END-IF                                                     
111900             ELSE                                                         
112000               MOVE NEJ TO INDATA-SW                                      
112100             END-IF                                                       
112200             IF INDATA-FEL                                                
112300               IF 6332-IDUSER(2) NOT = SPACE                              
112400                 IF MSGI-IDUSER = 6332-IDUSER(2)                          
112500                   MOVE JA  TO INDATA-SW                                  
112600                 END-IF                                                   
112700               END-IF                                                     
112800             END-IF                                                       
112900             IF INDATA-FEL                                                
113000               IF 6332-IDUSER(3) NOT = SPACE                              
113100                 IF MSGI-IDUSER = 6332-IDUSER(2)                          
113200                   MOVE JA  TO INDATA-SW                                  
113300                 END-IF                                                   
113400               END-IF                                                     
113500             END-IF                                                       
113600             IF INDATA-FEL                                                
113700               IF 6332-IDUSER(4) NOT = SPACE                              
113800                 IF MSGI-IDUSER = 6332-IDUSER(2)                          
113900                   MOVE JA  TO INDATA-SW                                  
114000                 END-IF                                                   
114100               END-IF                                                     
114200             END-IF                                                       
114300           END-IF                                                         
114400         END-IF                                                           
114500       END-IF                                                             
114600     ELSE                                                                 
114700       MOVE NEJ               TO INDATA-SW                                
114800***    MSGI-IDUSER NOT LEVEL2                                             
114900***    LETA UPP INMATADES FÖRÄLDER                                        
115000                                                                          
115100       PERFORM IMS-GET-WDGX6331                                           
115200       PERFORM IMS-GNP-WDGX6332                                           
115300       MOVE DCS-IDDC          TO W-IDDC-6334                              
115400       PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                         
115500         MOVE 6332-IDDC       TO W-IDDC-6332                              
115600         PERFORM IMS-GET-WDGX6334-UNIK                                    
115700         IF SEGMENT-FINNS                                                 
115800           MOVE 6332-IDDC     TO WS-6332-IDDC                             
115900           MOVE JA            TO POST-FINNS-SW                            
116000           MOVE JA            TO INDATA-SW                                
116100         ELSE                                                             
116200           PERFORM IMS-GNP-WDGX6332                                       
116300         END-IF                                                           
116400       END-PERFORM                                                        
116500                                                                          
116600       IF POST-SAKNAS                                                     
116700         MOVE NEJ             TO INDATA-SW                                
116800       ELSE                                                               
116900*** KOLLA OM OM USER FÅR ÄNDRA PÅ INMATAT IDDC                            
117000         MOVE WS-6332-IDDC TO W-IDDC-6332                                 
117100         PERFORM IMS-GHU-WDGX6332                                         
117200         IF SEGMENT-FINNS                                                 
117300           IF 6332-IDUSER(1) NOT = SPACE                                  
117400             IF MSGI-IDUSER NOT = 6332-IDUSER(1)                          
117500               MOVE NEJ TO INDATA-SW                                      
117600             ELSE                                                         
117700               MOVE JA  TO INDATA-SW                                      
117800             END-IF                                                       
117900           END-IF                                                         
118000           IF INDATA-FEL                                                  
118100             IF 6332-IDUSER(2) NOT = SPACE                                
118200               IF MSGI-IDUSER = 6332-IDUSER(2)                            
118300                 MOVE JA  TO INDATA-SW                                    
118400               END-IF                                                     
118500             END-IF                                                       
118600           END-IF                                                         
118700           IF INDATA-FEL                                                  
118800             IF 6332-IDUSER(3) NOT = SPACE                                
118900               IF MSGI-IDUSER = 6332-IDUSER(2)                            
119000                 MOVE JA  TO INDATA-SW                                    
119100               END-IF                                                     
119200             END-IF                                                       
119300           END-IF                                                         
119400           IF INDATA-FEL                                                  
119500             IF 6332-IDUSER(4) NOT = SPACE                                
119600               IF MSGI-IDUSER = 6332-IDUSER(2)                            
119700                 MOVE JA  TO INDATA-SW                                    
119800               END-IF                                                     
119900             END-IF                                                       
120000           END-IF                                                         
120100         ELSE                                                             
120200           MOVE NEJ TO INDATA-SW                                          
120300         END-IF                                                           
120400       END-IF                                                             
120500     END-IF                                                               
120600     .                                                                    
120700     EJECT                                                                
120800 H-UPPDATERA SECTION.                                                     
120900                                                                          
121000     IF FL-INTERN = JA                                                    
121100       MOVE DAGENS-DATUM TO INFO-TIREGDAT                                 
121200       MOVE 79000000  TO INFO-DAREGDAT-9KOMPL                             
121300       MOVE 999999999 TO INFO-TIKLOCK-9KOMPL                              
121400                                                                          
121500       MOVE MID-IDKVAINF-UPPD        TO INFO-IDKVAINF                     
121600       MOVE ZERO  TO INFO-TIKLAR-QUAL                                     
121700                     INFO-TIKLAR-LEV                                      
121800                     INFO-KDPERSON                                        
121900       MOVE SPACE TO INFO-KDKVAINF                                        
122000                     INFO-TEKVAINP                                        
122100                     INFO-FLQPA                                           
122200                     INFO-FLSTOCH                                         
122300                     INFO-TEKVAINF-INT(1)                                 
122400                     INFO-TEKVAINF-INT(2)                                 
122500                     INFO-TEKVAINF-INT(3)                                 
122600                     INFO-TEKVAINF-INT(4)                                 
122700                     INFO-TEKVAINF-INT(5)                                 
122800                     INFO-TEKVAINF-INT(6)                                 
122900                     INFO-TEKVAINF-INT(7)                                 
123000                     INFO-TEKVAINF-EXT(1)                                 
123100                     INFO-TEKVAINF-EXT(2)                                 
123200                     INFO-TEKVAINF-EXT(3)                                 
123300                     INFO-TEKVAINF-EXT(4)                                 
123400                     INFO-TEKVAINF-EXT(5)                                 
123500                     INFO-TEKVAINF-EXT(6)                                 
123600                     INFO-TEKVAINF-EXT(7)                                 
123700       PERFORM IMS-ISRT-W6D211                                            
123800     END-IF                                                               
123900                                                                          
124000     PERFORM IMS-GHU-W6D221                                               
124100                                                                          
124200     IF SEGMENT-FINNS                                                     
124300       MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                     
124400       MOVE INFO-DAREGDAT-9KOMPL (2:7)                                    
124500                                 TO MOD-TIREGDAT-9KOMPL-ENTER             
124600       MOVE INFO-TIKLOCK-9KOMPL  TO MOD-TIKLOCK-9KOMPL-ENTER              
124700                                    W-TIKLOCK-9KOMPL                      
124800                                                                          
124900       IF MID-FLTABORT = JA OR YES                                        
125000         PERFORM IMS-DLET-W6D221                                          
125800       ELSE                                                               
125900         MOVE DCS-IDDC   TO DCQ-IDDC                                      
126000                                                                          
126100         IF MID-BEINIT NOT = ALL '+'                                      
126200           MOVE MID-BEINIT TO DCQ-BEINIT                                  
126300         END-IF                                                           
126400                                                                          
126500         IF MID-TISTADAT-IN NOT = ALL '+'                                 
126600           MOVE MID-TISTADAT-IN TO DCQ-TISTADAT                           
126700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-IN-ATTR             
126800         ELSE                                                             
126900           MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-IN                      
127000         END-IF                                                           
127100                                                                          
127200         IF MID-TISTODAT-IN NOT = ALL '+'                                 
127300           MOVE MID-TISTODAT-IN TO DCQ-TISTODAT                           
127400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTODAT-IN-ATTR             
127500         ELSE                                                             
127600           MOVE MFS-ROER-EJ-FAELT TO MOD-TISTODAT-IN                      
127700         END-IF                                                           
127800                                                                          
127900         IF MID-KVANTAL-IN NOT = ALL '+'                                  
128000           MOVE MID-KVANTAL-IN TO DCQ-KVANTAL                             
128100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVANTAL-IN-ATTR              
128200         ELSE                                                             
128300           MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTAL-IN                       
128400         END-IF                                                           
128500                                                                          
128600         IF MID-KVAVV-KVAL-IN NOT = ALL '+'                               
128700           MOVE MID-KVAVV-KVAL-IN TO DCQ-KVAVV-KVAL                       
128800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVAVV-KVAL-IN-ATTR           
128900         ELSE                                                             
129000           MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVV-KVAL-IN                    
129100         END-IF                                                           
129200                                                                          
129300         IF MID-KVART-SKROT-IN NOT = ALL '+'                              
129400           MOVE MID-KVART-SKROT-IN TO DCQ-KVART-SKROT                     
129500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-SKROT-IN-ATTR          
129600         ELSE                                                             
129700           MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-SKROT-IN                   
129800         END-IF                                                           
129900                                                                          
130000         IF MID-KVART-RET-IN NOT = ALL '+'                                
130100           MOVE MID-KVART-RET-IN TO DCQ-KVART-RET                         
130200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-RET-IN-ATTR            
130300         ELSE                                                             
130400           MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-RET-IN                     
130500         END-IF                                                           
130600                                                                          
130700         IF MID-KVART-KJUST-IN NOT = ALL '+'                              
130800           MOVE MID-KVART-KJUST-IN TO DCQ-KVART-KJUST                     
130900           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-KJUST-IN-ATTR          
131000         ELSE                                                             
131100           MOVE MFS-ROER-EJ-FAELT TO MOD-KVART-KJUST-IN                   
131200         END-IF                                                           
131300                                                                          
131400         IF MID-IDNAMN NOT = ALL '+'                                      
131500           MOVE MID-IDNAMN TO DCQ-IDNAMN                                  
131600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDNAMN-DC-QUAL-ATTR          
131700         ELSE                                                             
131800           MOVE MFS-ROER-EJ-FAELT TO MOD-IDNAMN-DC-QUAL                   
131900         END-IF                                                           
132000                                                                          
132100         IF MID-TEKVAINF-DC-RAD1 NOT = ALL '+'                            
132200           MOVE MID-TEKVAINF-DC-RAD1  TO DCQ-TEKVAINF(1)(1:63)            
132300           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD1-ATTR        
132400         ELSE                                                             
132500           MOVE MFS-ROER-EJ-FAELT TO MOD-TEKVAINF-DC-RAD1                 
132600         END-IF                                                           
132700                                                                          
132800         IF MID-TEKVAINF-DC-RAD2 NOT = ALL '+'                            
132900           MOVE MID-TEKVAINF-DC-RAD2  TO DCQ-TEKVAINF(2)                  
133000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD2-ATTR        
133100         ELSE                                                             
133200           MOVE MFS-ROER-EJ-FAELT TO MOD-TEKVAINF-DC-RAD2                 
133300         END-IF                                                           
133400                                                                          
133500         IF MID-TEKVAINF-DC-RAD3 NOT = ALL '+'                            
133600           MOVE MID-TEKVAINF-DC-RAD3  TO DCQ-TEKVAINF(3)                  
133700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD3-ATTR        
133800         ELSE                                                             
133900           MOVE MFS-ROER-EJ-FAELT TO MOD-TEKVAINF-DC-RAD3                 
134000         END-IF                                                           
134100                                                                          
134200         IF MID-TEKVAINF-DC-RAD4 NOT = ALL '+'                            
134300           MOVE MID-TEKVAINF-DC-RAD4  TO DCQ-TEKVAINF(4)                  
134400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD4-ATTR        
134500         ELSE                                                             
134600           MOVE MFS-ROER-EJ-FAELT TO MOD-TEKVAINF-DC-RAD4                 
134700         END-IF                                                           
134800                                                                          
134900         PERFORM IMS-REPL-W6D221                                          
135000       END-IF                                                             
135100     ELSE                                                                 
135200       PERFORM IMS-GHU-W6D211                                             
135300       IF SEGMENT-FINNS                                                   
135400         MOVE DCS-IDDC   TO DCQ-IDDC                                      
135500         MOVE MID-BEINIT TO DCQ-BEINIT                                    
135600         MOVE NEJ        TO DCQ-FLSLUT                                    
135700                                                                          
135800         IF MID-TISTADAT-IN NOT = ALL '+'                                 
135900           MOVE MID-TISTADAT-IN TO DCQ-TISTADAT                           
136000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-IN-ATTR             
136100         ELSE                                                             
136200           MOVE ZERO TO DCQ-TISTADAT                                      
136300         END-IF                                                           
136400                                                                          
136500         IF MID-TISTODAT-IN NOT = ALL '+'                                 
136600           MOVE MID-TISTODAT-IN TO DCQ-TISTODAT                           
136700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTODAT-IN-ATTR             
136800         ELSE                                                             
136900           MOVE ZERO TO DCQ-TISTODAT                                      
137000         END-IF                                                           
137100                                                                          
137200         IF MID-KVANTAL-IN NOT = ALL '+'                                  
137300           MOVE MID-KVANTAL-IN TO DCQ-KVANTAL                             
137400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVANTAL-IN-ATTR              
137500         ELSE                                                             
137600           MOVE ZERO TO DCQ-KVANTAL                                       
137700         END-IF                                                           
137800                                                                          
137900         IF MID-KVAVV-KVAL-IN NOT = ALL '+'                               
138000           MOVE MID-KVAVV-KVAL-IN TO DCQ-KVAVV-KVAL                       
138100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVAVV-KVAL-IN-ATTR           
138200         ELSE                                                             
138300           MOVE ZERO TO DCQ-KVAVV-KVAL                                    
138400         END-IF                                                           
138500                                                                          
138600         IF MID-KVART-SKROT-IN NOT = ALL '+'                              
138700           MOVE MID-KVART-SKROT-IN TO DCQ-KVART-SKROT                     
138800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-SKROT-IN-ATTR          
138900         ELSE                                                             
139000           MOVE ZERO TO DCQ-KVART-SKROT                                   
139100         END-IF                                                           
139200                                                                          
139300         IF MID-KVART-RET-IN NOT = ALL '+'                                
139400           MOVE MID-KVART-RET-IN TO DCQ-KVART-RET                         
139500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-RET-IN-ATTR            
139600         ELSE                                                             
139700           MOVE ZERO TO DCQ-KVART-RET                                     
139800         END-IF                                                           
139900                                                                          
140000         IF MID-KVART-KJUST-IN NOT = ALL '+'                              
140100           MOVE MID-KVART-KJUST-IN TO DCQ-KVART-KJUST                     
140200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVART-KJUST-IN-ATTR          
140300         ELSE                                                             
140400           MOVE ZERO TO DCQ-KVART-KJUST                                   
140500         END-IF                                                           
140600                                                                          
140700         IF MID-IDNAMN NOT = ALL '+'                                      
140800           MOVE MID-IDNAMN TO DCQ-IDNAMN                                  
140900           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDNAMN-DC-QUAL-ATTR          
141000         ELSE                                                             
141100           MOVE SPACE TO DCQ-IDNAMN                                       
141200         END-IF                                                           
141300                                                                          
141400         MOVE SPACE TO DCQ-TEKVAINF(1)                                    
141500         IF MID-TEKVAINF-DC-RAD1 NOT = ALL '+'                            
141600           MOVE MID-TEKVAINF-DC-RAD1  TO DCQ-TEKVAINF(1)(1:63)            
141700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD1-ATTR        
141800         END-IF                                                           
141900                                                                          
142000         IF MID-TEKVAINF-DC-RAD2 NOT = ALL '+'                            
142100           MOVE MID-TEKVAINF-DC-RAD2  TO DCQ-TEKVAINF(2)                  
142200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD2-ATTR        
142300         ELSE                                                             
142400           MOVE SPACE TO DCQ-TEKVAINF(2)                                  
142500         END-IF                                                           
142600                                                                          
142700         IF MID-TEKVAINF-DC-RAD3 NOT = ALL '+'                            
142800           MOVE MID-TEKVAINF-DC-RAD3  TO DCQ-TEKVAINF(3)                  
142900           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD3-ATTR        
143000         ELSE                                                             
143100           MOVE SPACE TO DCQ-TEKVAINF(3)                                  
143200         END-IF                                                           
143300                                                                          
143400         IF MID-TEKVAINF-DC-RAD4 NOT = ALL '+'                            
143500           MOVE MID-TEKVAINF-DC-RAD4  TO DCQ-TEKVAINF(4)                  
143600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEKVAINF-DC-RAD4-ATTR        
143700         ELSE                                                             
143800           MOVE SPACE TO DCQ-TEKVAINF(4)                                  
143900         END-IF                                                           
144000                                                                          
144100         MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                   
144200         MOVE INFO-TIKLOCK-9KOMPL  TO W-TIKLOCK-9KOMPL                    
144300         PERFORM IMS-ISRT-W6D221                                          
144400       END-IF                                                             
144500     END-IF                                                               
144600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
144700     CALL WMEDKONV USING MED-WMEDAREA                                     
144800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
144900     PERFORM MFS-FORM-ATTR                                                
145000     PERFORM MFS-RENSA-FAELT-IN                                           
145100     .                                                                    
145200     EJECT                                                                
145300 MFS-RENSA-FAELT-UT SECTION.                                              
145400                                                                          
145500*    --- ALLA UTDATA-FÄLT                                                 
145600     MOVE MFS-RENSA-FAELT TO MOD-IDKVAINF-UPPD                            
145700                             MOD-TIREGDAT                                 
145800                             MOD-IDNAMN                                   
145900                             MOD-IDMAIL                                   
146000                             MOD-KDKVAINF                                 
146100                             MOD-TIKLAR-LEV                               
146200                             MOD-FLSTOCH                                  
146300                             MOD-TIREGDAT-9KOMPL-ENTER                    
146400                             MOD-TIKLOCK-9KOMPL-ENTER                     
146500                             MOD-TIREGDAT-9KOMPL-NEXT                     
146600                             MOD-TIKLOCK-9KOMPL-NEXT                      
146700                             MOD-TISTADAT-UT                              
146800                             MOD-TISTODAT-UT                              
146900                             MOD-KVANTAL-UT                               
147000                             MOD-KVAVV-KVAL-UT                            
147100                             MOD-KVART-SKROT-UT                           
147200                             MOD-KVART-RET-UT                             
147300                             MOD-KVART-KJUST-UT                           
147400                             MOD-BEINIT                                   
147500                             MOD-IDNAMN-DC-QUAL                           
147600                             MOD-TEKVAINF-DC-RAD1                         
147700                             MOD-TEKVAINF-DC-RAD2                         
147800                             MOD-TEKVAINF-DC-RAD3                         
147900                             MOD-TEKVAINF-DC-RAD4                         
148000                             MOD-FLTABORT                                 
148100     .                                                                    
148200     SKIP3                                                                
148300 MFS-RENSA-FAELT-IN SECTION.                                              
148400                                                                          
148500*    --- ALLA INDATA-FÄLT                                                 
148600     MOVE MFS-RENSA-FAELT TO MOD-TISTADAT-IN                              
148700                             MOD-TISTODAT-IN                              
148800                             MOD-KVANTAL-IN                               
148900                             MOD-KVAVV-KVAL-IN                            
149000                             MOD-KVART-SKROT-IN                           
149100                             MOD-KVART-RET-IN                             
149200                             MOD-KVART-KJUST-IN                           
149300                             MOD-BEINIT                                   
149400                             MOD-IDNAMN-DC-QUAL                           
149500                             MOD-TEKVAINF-DC-RAD1                         
149600                             MOD-TEKVAINF-DC-RAD2                         
149700                             MOD-TEKVAINF-DC-RAD3                         
149800                             MOD-TEKVAINF-DC-RAD4                         
149900                             MOD-FLTABORT                                 
150000     .                                                                    
150100     EJECT                                                                
150200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
150300                                                                          
150400*    --- ALLA UTDATA-FÄLT                                                 
150500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKVAINF-UPPD                          
150600                               MOD-TIREGDAT                               
150700                               MOD-IDNAMN                                 
150800                               MOD-IDMAIL                                 
150900                               MOD-KDKVAINF                               
151000                               MOD-TIKLAR-LEV                             
151100                               MOD-FLSTOCH                                
151200                               MOD-TIREGDAT-9KOMPL-ENTER                  
151300                               MOD-TIKLOCK-9KOMPL-ENTER                   
151400                               MOD-TIREGDAT-9KOMPL-NEXT                   
151500                               MOD-TIKLOCK-9KOMPL-NEXT                    
151600                               MOD-TISTADAT-UT                            
151700                               MOD-TISTODAT-UT                            
151800                               MOD-KVANTAL-UT                             
151900                               MOD-KVAVV-KVAL-UT                          
152000                               MOD-KVART-SKROT-UT                         
152100                               MOD-KVART-RET-UT                           
152200                               MOD-KVART-KJUST-UT                         
152300                               MOD-BEINIT                                 
152400                               MOD-IDNAMN-DC-QUAL                         
152500                               MOD-TEKVAINF-DC-RAD1                       
152600                               MOD-TEKVAINF-DC-RAD2                       
152700                               MOD-TEKVAINF-DC-RAD3                       
152800                               MOD-TEKVAINF-DC-RAD4                       
152900                               MOD-FLTABORT                               
153000     .                                                                    
153100     SKIP3                                                                
153200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
153300                                                                          
153400*    --- ALLA INDATA-FÄLT                                                 
153500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKVAINF-UPPD                          
153600                               MOD-TISTADAT-IN                            
153700                               MOD-TISTODAT-IN                            
153800                               MOD-KVANTAL-IN                             
153900                               MOD-KVAVV-KVAL-IN                          
154000                               MOD-KVART-SKROT-IN                         
154100                               MOD-KVART-RET-IN                           
154200                               MOD-KVART-KJUST-IN                         
154300                               MOD-BEINIT                                 
154400                               MOD-IDNAMN-DC-QUAL                         
154500                               MOD-TEKVAINF-DC-RAD1                       
154600                               MOD-TEKVAINF-DC-RAD2                       
154700                               MOD-TEKVAINF-DC-RAD3                       
154800                               MOD-TEKVAINF-DC-RAD4                       
154900                               MOD-FLTABORT                               
155000     .                                                                    
155100     EJECT                                                                
155200 MFS-FORM-ATTR SECTION.                                                   
155300                                                                          
155400*    --- ALLA INDATA-FÄLT                                                 
155500     MOVE MFS-FORMATETS-ATTR TO MOD-TISTADAT-IN-ATTR                      
155600                                MOD-TISTODAT-IN-ATTR                      
155700                                MOD-KVANTAL-IN-ATTR                       
155800                                MOD-KVAVV-KVAL-IN-ATTR                    
155900                                MOD-KVART-SKROT-IN-ATTR                   
156000                                MOD-KVART-RET-IN-ATTR                     
156100                                MOD-KVART-KJUST-IN-ATTR                   
156200                                MOD-BEINIT-ATTR                           
156300                                MOD-IDNAMN-DC-QUAL-ATTR                   
156400                                MOD-TEKVAINF-DC-RAD1-ATTR                 
156500                                MOD-TEKVAINF-DC-RAD2-ATTR                 
156600                                MOD-TEKVAINF-DC-RAD3-ATTR                 
156700                                MOD-TEKVAINF-DC-RAD4-ATTR                 
156800                                MOD-FLTABORT-ATTR                         
156900     .                                                                    
157000     SKIP2                                                                
157100* --- IMS SEKTIONER ---                                                   
157200     SKIP3                                                                
157300 IMS-GET-MSG SECTION.                                                     
157400                                                                          
157500     MOVE '  QC' TO GODK-STATUSKODER                                      
157600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
157700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000     SKIP3                                                                
158100 IMS-INSERT-MSG SECTION.                                                  
158200                                                                          
158300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
158400     MOVE SPACE TO GODK-STATUSKODER                                       
158500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
158600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
158700     PERFORM IMS-STATUSKONTROLL                                           
158800     .                                                                    
158900     EJECT                                                                
159000 IMS-GU-W6D201 SECTION.                                                   
159100                                                                          
159200     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
159300          DELIMITED BY SIZE INTO SSA1                                     
159400     MOVE '  GE' TO GODK-STATUSKODER                                      
159500     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D2 SSA1                     
159600     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
159700     PERFORM IMS-STATUSKONTROLL                                           
159800     .                                                                    
159900     EJECT                                                                
160000 IMS-ISRT-W6D201 SECTION.                                                 
160100                                                                          
160200     MOVE 'W6D201   ' TO SSA1                                             
160300     MOVE '  ' TO GODK-STATUSKODER                                        
160400     CALL CBLTDLI USING ISRT W6D2A-PCB DLI-IO-W6D2                        
160500                                                    SSA1                  
160600     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900     SKIP3                                                                
161000 IMS-GNP-W6D211 SECTION.                                                  
161100                                                                          
161200     STRING 'W6D211  (W6D211KY=>' W-W6D211KY-X                            
161300                    '&IDKVAINF=>' W-IDKVAINF-MIN-X                        
161400                    '&IDKVAINF=<' W-IDKVAINF-MAX-X ')'                    
161500          DELIMITED BY SIZE INTO SSA1                                     
161600     MOVE '  GE' TO GODK-STATUSKODER                                      
161700     CALL CBLTDLI USING GNP W6D2A-PCB DLI-IO-W6D2 SSA1                    
161800     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
161900     PERFORM IMS-STATUSKONTROLL                                           
162000     .                                                                    
162100     SKIP3                                                                
162200 IMS-GU-W6D211 SECTION.                                                   
162300     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
162400          DELIMITED BY SIZE INTO SSA1                                     
162500     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
162600          DELIMITED BY SIZE INTO SSA2                                     
162700     MOVE '  GE' TO GODK-STATUSKODER                                      
162800     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D2 SSA1 SSA2                
162900     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
163000     PERFORM IMS-STATUSKONTROLL                                           
163100     .                                                                    
163200     SKIP3                                                                
163300 IMS-GHU-W6D211 SECTION.                                                  
163400     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
163500          DELIMITED BY SIZE INTO SSA1                                     
163600     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
163700          DELIMITED BY SIZE INTO SSA2                                     
163800     MOVE '  GE' TO GODK-STATUSKODER                                      
163900     CALL CBLTDLI USING GHU W6D2A-PCB DLI-IO-W6D2 SSA1 SSA2               
164000     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
164100     PERFORM IMS-STATUSKONTROLL                                           
164200     .                                                                    
164300     SKIP3                                                                
164400 IMS-ISRT-W6D211 SECTION.                                                 
164500                                                                          
164600     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
164700          DELIMITED BY SIZE INTO SSA1                                     
164800     MOVE 'W6D211 ' TO SSA2                                               
164900     MOVE '  II' TO GODK-STATUSKODER                                      
165000     CALL CBLTDLI USING ISRT W6D2A-PCB DLI-IO-W6D2                        
165100                                                    SSA1 SSA2             
165200     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
165300     PERFORM IMS-STATUSKONTROLL                                           
165400     .                                                                    
165500     SKIP3                                                                
166400 IMS-GU-W6D221 SECTION.                                                   
166500                                                                          
166600     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
166700          DELIMITED BY SIZE INTO SSA1                                     
166800     STRING 'W6D211  (W6D211KY =' W-W6D211KY-X ')'                        
166900          DELIMITED BY SIZE INTO SSA2                                     
167000     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
167100          DELIMITED BY SIZE INTO SSA3                                     
167200     MOVE '  GE' TO GODK-STATUSKODER                                      
167300     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
167400     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
167500     PERFORM IMS-STATUSKONTROLL                                           
167600     .                                                                    
167700     SKIP3                                                                
167800 IMS-GU-W6D221-2 SECTION.                                                 
167900                                                                          
168000     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
168100          DELIMITED BY SIZE INTO SSA1                                     
168200     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
168300          DELIMITED BY SIZE INTO SSA2                                     
168400     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
168500          DELIMITED BY SIZE INTO SSA3                                     
168600     MOVE '  GE' TO GODK-STATUSKODER                                      
168700     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
168800     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
168900     PERFORM IMS-STATUSKONTROLL                                           
169000     .                                                                    
169100     SKIP3                                                                
169200 IMS-GET-W6D221-FIRST SECTION.                                            
169300                                                                          
169400     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
169500          DELIMITED BY SIZE INTO SSA1                                     
169600     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
169700          DELIMITED BY SIZE INTO SSA2                                     
169800     MOVE 'W6D221  *F'        TO SSA3                                     
169900     MOVE '  GE' TO GODK-STATUSKODER                                      
170000     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
170100     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
170200     PERFORM IMS-STATUSKONTROLL                                           
170300     .                                                                    
170400     SKIP3                                                                
170500 IMS-GHU-W6D221 SECTION.                                                  
170600                                                                          
170700     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
170800          DELIMITED BY SIZE INTO SSA1                                     
170900     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
171000          DELIMITED BY SIZE INTO SSA2                                     
171100     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
171200          DELIMITED BY SIZE INTO SSA3                                     
171300     MOVE '  GE' TO GODK-STATUSKODER                                      
171400     CALL CBLTDLI USING GHU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3        
171500     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
171600     PERFORM IMS-STATUSKONTROLL                                           
171700     .                                                                    
171800     SKIP3                                                                
171900 IMS-ISRT-W6D221 SECTION.                                                 
172000                                                                          
172100     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
172200          DELIMITED BY SIZE INTO SSA1                                     
172300     STRING 'W6D211  (W6D211KY =' W-W6D211KY-X ')'                        
172400          DELIMITED BY SIZE INTO SSA2                                     
172500     MOVE 'W6D221 ' TO SSA3                                               
172600     MOVE '  II' TO GODK-STATUSKODER                                      
172700     CALL CBLTDLI USING ISRT W6D2B-PCB DLI-IO-W6D221                      
172800                                                    SSA1 SSA2 SSA3        
172900     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
173000     PERFORM IMS-STATUSKONTROLL                                           
173100     .                                                                    
173200     SKIP3                                                                
173300 IMS-REPL-W6D221 SECTION.                                                 
173400                                                                          
173500     MOVE '  ' TO GODK-STATUSKODER                                        
173600     CALL CBLTDLI USING REPL W6D2B-PCB DLI-IO-W6D221                      
173700     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
173800     PERFORM IMS-STATUSKONTROLL                                           
173900     .                                                                    
174000     SKIP3                                                                
174100 IMS-DLET-W6D221 SECTION.                                                 
174200                                                                          
174300     MOVE '  ' TO GODK-STATUSKODER                                        
174400     CALL CBLTDLI USING DLET W6D2B-PCB DLI-IO-W6D221                      
174500     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
174600     PERFORM IMS-STATUSKONTROLL                                           
174700     .                                                                    
174800     EJECT                                                                
174900 IMS-GU-WDK611 SECTION.                                                   
175000                                                                          
175100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
175200          DELIMITED BY SIZE INTO SSA1                                     
175300     MOVE 'WDK611   ' TO SSA2                                             
175400     MOVE '  GE' TO GODK-STATUSKODER                                      
175500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK6 SSA1 SSA2                 
175600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900     EJECT                                                                
176000 IMS-GU-WDK711 SECTION.                                                   
176100                                                                          
176200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
176300          DELIMITED BY SIZE INTO SSA1                                     
176400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
176500          DELIMITED BY SIZE INTO SSA2                                     
176600     MOVE '  GE' TO GODK-STATUSKODER                                      
176700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK7 SSA1 SSA2                 
176800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
176900     PERFORM IMS-STATUSKONTROLL                                           
177000     .                                                                    
177100     EJECT                                                                
177200 IMS-GU-WDP311 SECTION.                                                   
177300     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
177400          DELIMITED BY SIZE INTO SSA1                                     
177500     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
177600          DELIMITED BY SIZE INTO SSA2                                     
177700     MOVE '  GE' TO GODK-STATUSKODER                                      
177800     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
177900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
178000     PERFORM IMS-STATUSKONTROLL                                           
178100     .                                                                    
178200     EJECT                                                                
178300 IMS-GU-WDB601    SECTION.                                                
178400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
178500          DELIMITED BY SIZE INTO SSA1                                     
178600     MOVE '  GE' TO GODK-STATUSKODER                                      
178700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
178800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
178900     PERFORM IMS-STATUSKONTROLL                                           
179000     IF SEGMENT-SAKNAS                                                    
179100         MOVE SPACE TO DCS-KDDC                                           
179200     END-IF                                                               
179300     .                                                                    
179400 IMS-GET-WDGX6331 SECTION.                                                
179500     MOVE 'IMS-GET-WDGX6331        ' TO WS-IMS                            
179600                                                                          
179700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
179800          DELIMITED BY SIZE INTO SSA1                                     
179900     MOVE '  GE' TO GODK-STATUSKODER                                      
180000     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX6331 SSA1                  
180100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
180200     PERFORM IMS-STATUSKONTROLL                                           
180300     .                                                                    
180400     EJECT                                                                
180500 IMS-GHU-WDGX6332 SECTION.                                                
180600     MOVE 'IMS-GU-WDGX6332         ' TO WS-IMS                            
180700                                                                          
180800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
180900          DELIMITED BY SIZE INTO SSA1                                     
181000     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
181100          DELIMITED BY SIZE INTO SSA2                                     
181200     MOVE '  GE' TO GODK-STATUSKODER                                      
181300     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
181400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
181500     PERFORM IMS-STATUSKONTROLL                                           
181600     .                                                                    
181700     EJECT                                                                
181800 IMS-GNP-WDGX6332 SECTION.                                                
181900     MOVE 'IMS-GNP-WDGX6332 SECTION' TO WS-IMS                            
182000                                                                          
182100     MOVE 'WDGX6332   ' TO SSA1                                           
182200     MOVE '  GE' TO GODK-STATUSKODER                                      
182300     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332 SSA1                 
182400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
182500     PERFORM IMS-STATUSKONTROLL                                           
182600     .                                                                    
182700     EJECT                                                                
182800 IMS-GET-WDGX6334-UNIK SECTION.                                           
182900     MOVE 'IMS-GET-WDGX6334-UNIK   ' TO WS-IMS                            
183000                                                                          
183100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
183200          DELIMITED BY SIZE INTO SSA1                                     
183300                                                                          
183400     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
183500          DELIMITED BY SIZE INTO SSA2                                     
183600                                                                          
183700     STRING 'WDGX6334(IDDC     =' W-WDGXKEY-6334-X ')'                    
183800          DELIMITED BY SIZE INTO SSA3                                     
183900                                                                          
184000     MOVE '  GE' TO GODK-STATUSKODER                                      
184100     CALL CBLTDLI USING GU WDR2ALT-PCB                                    
184200     DLI-IO-WDGX6334 SSA1 SSA2 SSA3                                       
184300                                                                          
184400     MOVE WDR2ALT-STATUS-CODE TO STATUS-WS                                
184500     PERFORM IMS-STATUSKONTROLL                                           
184600     .                                                                    
184700     EJECT                                                                
184800 IMS-STATUSKONTROLL SECTION.                                              
184900                                                                          
185000     SET STATUS-IX TO 1                                                   
185100     SEARCH GODK-STATUS                                                   
185200       AT END                                                             
185300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
185400         DELIMITED BY SIZE INTO FELTEXT                                   
185500         CALL FELLOG                                                      
185600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
185700         CONTINUE                                                         
185800     END-SEARCH                                                           
185900     .                                                                    
