000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033300.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/10/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMETS HUVUDFUNKTIONER ÄR FÖLJANDE:                         
000900*        -FRÅGA PÅ LEVERANTÖR OCH GRUPP OCH PÅ SÅ SÄTT FÅ UT              
001000*         VILKA ARTIKLAR SOM TILLHÖR VILKEN GRUPP                         
001100*         GÅR ÄVEN ATT FYLLA I ARTIKELNR, PÅ SÅ VIS KAN MAN               
001200*         SE OM ARTIKELN FINNS UPPLAGD I GRUPPEN                          
001300*                                                                         
001400*        -LÄGGA UPP NYA ARTIKLAR I GRUPP (N)                              
001500*        -ÄNDRA STARTDATUM FÖR BEFINTLIG ARTIKEL (E)                      
001600*        -TA BORT GAMLA ARTIKLAR I GRUPP (D)                              
001700*                                                                         
001800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001900*        PROGRAMMET UPPDATERAR WLLEVF (WDF2)                              
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W2T333 W2T333U W2T333X                              
002300*        MID:         W2I33301                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W2O33301                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W2033300'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300*    --- GENERELLA ARBETSFÄLT                                             
004400 01  DAGENS-DATUM                PIC 9(8).                                
004500                                                                          
004600 01 WS-DATUM.                                                             
004700    03 WS-AAR                    PIC 9(2).                                
004800    03 FILLER                    PIC 9(4).                                
004900                                                                          
005000 01  WS-DASTADAT.                                                         
005100     03  WS-DASTADAT-AAR         PIC 9(2).                                
005200     03  WS-DASTADAT-TI          PIC 9(6).                                
005300                                                                          
005400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-INDX                    PIC S9(4)  VALUE +36   COMP SYNC.        
005700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005800                                                                          
005900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006000     88  ALLT-OK                             VALUE 'J'.                   
006100                                                                          
006200 77  ARTNR-SW                    PIC X       VALUE 'J'.                   
006300     88  ARTNR-OK                            VALUE 'J'.                   
006400     88  ARTNR-FEL                           VALUE 'N'.                   
006500                                                                          
006600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006700     88  INDATA-OK                           VALUE 'J'.                   
006800     88  INDATA-FEL                          VALUE 'N'.                   
006900                                                                          
007000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007100     88  NYCKLAR-OK                          VALUE 'J'.                   
007200     88  NYCKLAR-FEL                         VALUE 'N'.                   
007300                                                                          
007400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007500     88  EGEN-MID                            VALUE '2333'.                
007600     88  GODK-MID                            VALUE '2331' '2332'          
007700                                                   '2333' '2334'          
007800                                                   '2335' '2336'          
007900                                                   '2337' '2338'          
008000                                                   '2339'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200     EJECT                                                                
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009200*01 -COPY WMEDAREA                                                        
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
009500*01 -COPY WDATAREA                                                        
009600     SKIP3                                                                
009700 01  MESSAGE-CODES.                                                       
009800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010500     03  GROUP-MISSING           PIC X(3)    VALUE '275'.                 
010600     03  PART-EXISTS             PIC X(3)    VALUE '278'.                 
010700     03  PART-MISSING            PIC X(3)    VALUE '279'.                 
010800     03  PART-MISSING-WDK6       PIC X(3)    VALUE '017'.                 
010810     03  AVTAL-MISSING-WDK6      PIC X(3)    VALUE '999'.                 
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011300     SKIP3                                                                
011400*01 -COPY WMSGINIT                                                        
011500     EJECT                                                                
011600*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
011700*                                                                         
011800 01  SPAR-AREA.                                                           
011900     03  SPAR-IDTRANS              PIC X(4)    VALUE '2333'.              
012000     03  SPAR-IDARTNR-NEXT         PIC 9(9)    VALUE ZERO.                
012100     EJECT                                                                
012200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012500     SKIP3                                                                
012600*01  MID -COPY W2I33301                                                   
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012900     SKIP3                                                                
013000*01  -COPY WMSGAREA                                                       
013100     EJECT                                                                
013200     03  MOD REDEFINES MSG-AREA.                                          
013300*      05  -COPY W2O33301                                                 
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013600     SKIP3                                                                
013700*01  -COPY WMFSAREA                                                       
013800     EJECT                                                                
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300     EJECT                                                                
014400 01  IMS-WS-4.                                                            
014500     03  FILLER              PIC X(16)  VALUE 'MSG-KOM-AREA'.             
014600*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
014700*01  -COPY WMSGKOM                                                        
014800     SKIP3                                                                
014900 01  NYCKLAR-TILL-DLI.                                                    
015000*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
015100     03  W-IDARTNR-WDK6-X.                                                
015200         05  W-IDARTNR-WDK6      PIC S9(9)   VALUE ZERO COMP-3.           
015210     03 W-IDLEVNSH-X.                                                     
015220         05 W-IDLEVNSH       PIC  X(5).                                   
015300                                                                          
015400     03  W-WDF201KY-X.                                                    
015500         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
015600         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
015700                                                                          
015800     03  W-IDARTNR-X.                                                     
015900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016000     SKIP2                                                                
016100*    --- STATUS-KOD FRÅN IMS                                              
016200 01  STATUS-WS                   PIC XX.                                  
016300     88  SEGMENT-FINNS                       VALUE '  '.                  
016400     88  ARTIKEL-FINNS                       VALUE 'NI'.                  
016500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017210 01  SSA3                        PIC X(64).                               
017300     EJECT                                                                
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800                                                                          
017900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
018000 01  DLI-IO-WLARTC01.                                                     
018100*    03  -COPY WDK601 -PRE WDK6                                           
018200 01  FILLER         PIC X(24)    VALUE 'DLI-IO-K623'.                     
018210 01  DLI-IO-K623.                                                         
018220*  03  -COPY WDK623                                                       
018300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVF01'.                    
018400 01  DLI-IO-WLLEVF01.                                                     
018500*    03  -COPY WDF201                                                     
018600     EJECT                                                                
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVF12'.                    
018800 01  DLI-IO-WLLEVF12.                                                     
018900*    03  -COPY WDF212                                                     
019000     EJECT                                                                
019100 LINKAGE SECTION.                                                         
019200*01  -COPY W0009   -PRE MSG-                                              
019300     EJECT                                                                
019400*01  -COPY W0009   -PRE MSGKOM-                                           
019500     EJECT                                                                
019600*01  -COPY W0008   -PRE USEA-                                             
019700     05  FILLER                  PIC X.                                   
019800                                                                          
019900*01  -COPY W0008  -PRE ARTC-                                              
020000     05  FILLER                  PIC X.                                   
020100                                                                          
020200*01  -COPY W0008  -PRE LEVF-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500 PROCEDURE DIVISION  USING MSG-PCB MSGKOM-PCB USEA-PCB                    
020600                           ARTC-PCB LEVF-PCB.                             
020700 MAIN SECTION.                                                            
020800     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB USEA-PCB                    
020900                           ARTC-PCB LEVF-PCB.                             
021000                                                                          
021100     PERFORM IMS-GET-MSG                                                  
021200     IF SEGMENT-FINNS                                                     
021300       PERFORM IMS-GET-WMSGKOM-MSG                                        
021400       PERFORM A-INIT                                                     
021500       PERFORM B-KOLLA-NYCKLAR                                            
021600       IF NYCKLAR-OK                                                      
021700         IF MFS-UPDATE OR MFS-UPD-X                                       
021800           PERFORM G-KOLLA-INPUT                                          
021900           IF INDATA-OK                                                   
022000             PERFORM H-UPPDATERA                                          
022100           END-IF                                                         
022200         ELSE                                                             
022300           IF MFS-FIRST                                                   
022400             PERFORM C-FOERSTA-SIDA                                       
022500           ELSE                                                           
022600             IF MFS-NEXT                                                  
022700               PERFORM D-NAESTA-SIDA                                      
022800             ELSE                                                         
022900               PERFORM E-SAMMA-SIDA                                       
023000             END-IF                                                       
023100           END-IF                                                         
023200         END-IF                                                           
023300         IF ALLT-OK AND NOT MFS-UPD-X                                     
023400           PERFORM F-LAES-VISA-INFO                                       
023500         END-IF                                                           
023600       END-IF                                                             
023700       IF MFS-UPD-X                                                       
023800         COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM + 17            
023900         PERFORM IMS-INSERT-WMSGKOM-MSG                                   
024000       ELSE                                                               
024100         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33301 + 4                    
024200         PERFORM IMS-INSERT-MSG                                           
024300       END-IF                                                             
024400     END-IF                                                               
024500                                                                          
024600     MOVE ZERO TO RETURN-CODE                                             
024700     GOBACK                                                               
024800     .                                                                    
024900     EJECT                                                                
025000 A-INIT SECTION.                                                          
025100                                                                          
025200     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
025300                                                                          
025400     IF MSG-DUBBLA-TRANSKODER                                             
025500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33301                 
025600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025800     ELSE                                                                 
025900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I33301                  
026000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026200     END-IF                                                               
026300                                                                          
026400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026700                                                                          
026800     MOVE LOW-VALUE TO MSG-AREA                                           
026900     MOVE 'W2O333N1' TO MFS-IDMOD                                         
027000     MOVE '2333' TO MOD-IDTRANS                                           
027100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027200                                                                          
027300     IF EGEN-MID OR HELP-MID                                              
027400       CONTINUE                                                           
027500     ELSE                                                                 
027600       MOVE SPACE TO MFS-KDTRTYP                                          
027700       MOVE '7' TO MFS-IDPFK                                              
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 B-KOLLA-NYCKLAR SECTION.                                                 
028200                                                                          
028300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028400     MOVE '001'             TO MSGI-KDCALL                                
028500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
028600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
028700     MOVE '2333'            TO MSGI-IDTRANS                               
028800                                                                          
028900     IF EGEN-MID                                                          
029000       MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                              
029100       MOVE MID-IDDIRGRP-IN  TO MSGI-IDDIRGRP                             
029200       IF MID-IDARTNR-IN NOT = ALL '+'                                    
029300         MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                           
029400       ELSE                                                               
029500         MOVE MID-IDARTNR-UT    TO MID-IDARTNR-IN                         
029600                                   MSGI-IDARTNR                           
029700       END-IF                                                             
029800     END-IF                                                               
029900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030000     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
030100                                                                          
030200     IF MSGI-IDLAND-SPR = 'GB'                                            
030300       MOVE 'GB' TO MED-IDSKYLT                                           
030400     ELSE                                                                 
030500       MOVE 'S' TO MED-IDSKYLT                                            
030600     END-IF                                                               
030700                                                                          
030800     MOVE JA TO ALLT-SW                                                   
030900     MOVE JA TO NYCKLAR-SW                                                
031000     MOVE NEJ TO ARTNR-SW                                                 
031100     MOVE SPACE TO MED-IDMFSFEL                                           
031200                   MED-IDMFSINF                                           
031300                   MSG-KOM-IDMFSMED                                       
031400                                                                          
031500*    -- KONTROLL AV IDLEVNR                                               
031600     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
031700                                                                          
031800     IF MSGI-IDLEVNR NOT = SPACES                                         
032000       MOVE MSGI-IDLEVNR    TO W-IDLEVNR                                  
032400     ELSE                                                                 
032500       MOVE NEJ             TO NYCKLAR-SW                                 
032600     END-IF                                                               
032700                                                                          
032800*    -- KONTROLL AV IDDIRGRP                                              
032900     MOVE MFS-RENSA-FAELT TO MOD-IDDIRGRP-IN                              
033000                                                                          
033100     MOVE MSGI-IDDIRGRP   TO W-IDDIRGRP                                   
033200                                                                          
033300*    -- KONTROLL AV IDARTNR                                               
033400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
033500                                                                          
033600     IF MID-IDARTNR-IN NOT = ALL '+'                                      
033700       INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO             
033800       IF MID-IDARTNR-IN NUMERIC                                          
033900         MOVE MID-IDARTNR-IN  TO W-IDARTNR                                
034000         IF MID-IDARTNR-IN NOT = ZERO                                     
034100           MOVE JA TO ARTNR-SW                                            
034200         END-IF                                                           
034300       ELSE                                                               
034400         MOVE NEJ           TO NYCKLAR-SW                                 
034500       END-IF                                                             
034600       INSPECT MID-IDARTNR-IN REPLACING LEADING ZERO BY SPACE             
034700       MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                           
034800     ELSE                                                                 
034900       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
035000     END-IF                                                               
035100                                                                          
035200     IF GODK-MID OR NYCKLAR-OK                                            
035300       MOVE MSGI-IDLEVNR    TO MOD-IDLEVNR-UT                             
035400       MOVE MSGI-IDDIRGRP   TO MOD-IDDIRGRP-UT                            
035500     ELSE                                                                 
035600       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                             
035700                               MOD-IDDIRGRP-UT                            
035800     END-IF                                                               
035900                                                                          
036000     IF NYCKLAR-FEL                                                       
036100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
036200                             MSG-KOM-IDMFSMED                             
036300       CALL WMEDKONV USING MED-WMEDAREA                                   
036400       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
036500       PERFORM MFS-RENSA-FAELT-IN                                         
036600       PERFORM MFS-RENSA-FAELT-UT                                         
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 C-FOERSTA-SIDA SECTION.                                                  
037100                                                                          
037200     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
037300                            MSG-KOM-IDMFSMED                              
037400     CALL WMEDKONV USING MED-WMEDAREA                                     
037500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
037600                                                                          
037700     PERFORM MFS-RENSA-FAELT-IN                                           
037800     .                                                                    
037900     EJECT                                                                
038000 D-NAESTA-SIDA SECTION.                                                   
038100     IF SPAR-IDTRANS = '2333'                                             
038200       MOVE SPAR-IDARTNR-NEXT  TO W-IDARTNR                               
038300     ELSE                                                                 
038400       PERFORM MFS-RENSA-FAELT-IN                                         
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800 E-SAMMA-SIDA SECTION.                                                    
038900                                                                          
039000     IF SPAR-IDTRANS = '2333' OR '0551'                                   
039100       IF MID-IDARTNR-E    = ALL '+'                                      
039200        AND MID-CMD        = ALL '+'                                      
039300         PERFORM MFS-RENSA-FAELT-IN                                       
039400       ELSE                                                               
039500         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
039600                                MSG-KOM-IDMFSMED                          
039700         CALL WMEDKONV USING MED-WMEDAREA                                 
039800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
039900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
040000         PERFORM MFS-LAES-IN-IGEN                                         
040100       END-IF                                                             
040200     ELSE                                                                 
040300       PERFORM MFS-RENSA-FAELT-IN                                         
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 F-LAES-VISA-INFO SECTION.                                                
040800                                                                          
040900     IF MFS-NEXT                                                          
041000       PERFORM IMS-GU-ART                                                 
041100     ELSE                                                                 
041200       PERFORM IMS-GU-LEV                                                 
041300     END-IF                                                               
041400                                                                          
041500     IF SEGMENT-SAKNAS                                                    
041600       MOVE GROUP-MISSING TO MED-IDMFSFEL                                 
041700                             MSG-KOM-IDMFSMED                             
041800       CALL WMEDKONV USING MED-WMEDAREA                                   
041900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
042000       PERFORM MFS-RENSA-FAELT-UT                                         
042100     ELSE                                                                 
042200       MOVE LEV-DASTADAT(3:6) TO MOD-TISTADAT-GRP                         
042300       IF (ARTNR-FEL AND MFS-ENTER) OR MFS-UPDATE                         
042400       OR (ARTNR-FEL AND MFS-FIRST)                                       
042500         PERFORM IMS-GN-ART                                               
042600       ELSE                                                               
042700         PERFORM IMS-GET-ART                                              
042800       END-IF                                                             
042900                                                                          
043000       IF SEGMENT-SAKNAS                                                  
043100         MOVE PART-MISSING TO MED-IDMFSFEL                                
043200                              MSG-KOM-IDMFSMED                            
043300         CALL WMEDKONV USING MED-WMEDAREA                                 
043400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
043500         PERFORM MFS-RENSA-FAELT-UT                                       
043600       ELSE                                                               
043700         MOVE +1 TO INDX                                                  
043800         PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                  
043900           IF SEGMENT-FINNS                                               
044000             IF ART-DASTADAT > DAGENS-DATUM                               
044100               MOVE ART-DASTADAT(3:6) TO MOD-TISTADAT (INDX)              
044200             END-IF                                                       
044300             MOVE ART-IDARTNR       TO MOD-IDARTNR  (INDX)                
044400           ELSE                                                           
044500             PERFORM MFS-RENSA-RAD-FAELT-UT                               
044600           END-IF                                                         
044700           ADD +1 TO INDX                                                 
044800           PERFORM IMS-GN-ART                                             
044900         END-PERFORM                                                      
045000       END-IF                                                             
045100                                                                          
045200       IF SEGMENT-FINNS                                                   
045300         MOVE ART-IDARTNR          TO SPAR-IDARTNR-NEXT                   
045400         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
045500                                      MSG-KOM-IDMFSMED                    
045600         CALL WMEDKONV USING MED-WMEDAREA                                 
045700         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
045800       END-IF                                                             
045900     END-IF                                                               
046000                                                                          
046100     MOVE '002'                TO MSGI-KDCALL                             
046200     MOVE '2333'               TO SPAR-IDTRANS                            
046300     MOVE SPAR-AREA            TO MSGI-SPAR-AREA                          
046400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046500     .                                                                    
046600     EJECT                                                                
046700 G-KOLLA-INPUT SECTION.                                                   
046800                                                                          
046900     MOVE JA  TO INDATA-SW                                                
047000     IF MID-CMD = ALL '+'                                                 
047100      AND MID-IDARTNR-E = ALL '+'                                         
047200      AND MID-TISTADAT-E = ALL '+'                                        
047300       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
047400                                    MSG-KOM-IDMFSMED                      
047500       CALL WMEDKONV USING MED-WMEDAREA                                   
047600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
047700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
047800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
047900       MOVE NEJ TO INDATA-SW                                              
048000     ELSE                                                                 
048100                                                                          
048110       IF MID-IDARTNR-E NOT = ALL '+'                                     
048120          INSPECT MID-IDARTNR-E REPLACING LEADING SPACE BY ZERO           
048130       END-IF                                                             
048140                                                                          
048200       IF (MID-CMD = 'N' OR MID-CMD = 'E' OR MID-CMD = 'D')               
048210       AND MID-IDARTNR-E NUMERIC                                          
048300          MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR                       
048400       ELSE                                                               
048500          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
048600                                       MSG-KOM-IDMFSMED                   
048601          IF MID-IDARTNR-E NOT NUMERIC                                    
048610             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDARTNR-E-ATTR              
048620          ELSE                                                            
048700             MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR                    
048710          END-IF                                                          
048720          PERFORM MFS-ROER-EJ-FAELT-IN                                    
048730          PERFORM MFS-ROER-EJ-FAELT-UT                                    
048800          MOVE NEJ TO INDATA-SW                                           
048810          MOVE NEJ TO ALLT-SW                                             
048900       END-IF                                                             
049000                                                                          
049100*---DATUM LAGRAS 8-STÄLLIGT PÅ BAS, DÄRFÖR DENNA KOD                      
049200       IF MID-TISTADAT-E = ALL '+'                                        
049300*---OM MAN EJ MATAT IN DATUM TAS DAGENS DATUM                             
049400         MOVE DAGENS-DATUM TO WS-DASTADAT                                 
049500         MOVE WS-DASTADAT-TI TO MID-TISTADAT-E                            
049600       ELSE                                                               
049700         MOVE MID-TISTADAT-E TO WS-DATUM                                  
049800                                WS-DASTADAT-TI                            
049900         IF WS-AAR < 50                                                   
050000           MOVE 20 TO WS-DASTADAT-AAR                                     
050100         ELSE                                                             
050200           MOVE 19 TO WS-DASTADAT-AAR                                     
050300         END-IF                                                           
050400       END-IF                                                             
050500*---SLUT DATUMKOD                                                         
050600                                                                          
050700*---VALIDERING AV DATUM                                                   
050800       MOVE 'AAMMDD'               TO DAT-KDDATFORM                       
050900       MOVE MID-TISTADAT-E         TO DAT-I-TIDATUM                       
051000                                                                          
051100       CALL WDATKONV USING         DAT-KDDATFORM                          
051200                                   DAT-I-TIDATUM                          
051300                                   DAT-O-TIDATUM                          
051400                                   DAT-KDSVAR                             
051500       IF DAT-KDSVAR-FEL                                                  
051600         MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                      
051700                                        MSG-KOM-IDMFSMED                  
051800         MOVE MFS-NUM-FAELT-FEL      TO MOD-TISTADAT-E-ATTR               
051900         MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDARTNR-E-ATTR                
052000         MOVE NEJ                    TO INDATA-SW                         
052100         MOVE NEJ                    TO ALLT-SW                           
052200       END-IF                                                             
052300                                                                          
052400       IF ALLT-OK                                                         
052500         IF MID-CMD = 'N'                                                 
052600*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
052700           IF WS-DASTADAT >= DAGENS-DATUM                                 
052800             MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-E-ATTR              
052900           ELSE                                                           
053000             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
053100                                          MSG-KOM-IDMFSMED                
053200             MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-E-ATTR             
053300             MOVE NEJ                  TO INDATA-SW                       
053400           END-IF                                                         
053500                                                                          
053600*---KOLLA SÅ ATT ARTIKEL FINNS PÅ ARTIKELBASEN                            
053700           MOVE MID-IDARTNR-E TO W-IDARTNR-WDK6                           
053800           PERFORM IMS-GU-WDK6-ART                                        
053900           IF SEGMENT-SAKNAS                                              
054000             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-E-ATTR              
054100             MOVE NEJ                  TO INDATA-SW                       
054200             MOVE PART-MISSING-WDK6    TO MED-IDMFSFEL                    
054300                                          MSG-KOM-IDMFSMED                
054400           ELSE                                                           
054500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-E-ATTR               
054600           END-IF                                                         
054700                                                                          
054800*---KOLLA SÅ ATT SEGMENT INTE REDAN FINNS PÅ BAS                          
054900           MOVE MID-IDARTNR-E  TO W-IDARTNR                               
055000           PERFORM IMS-GHU-ART                                            
055100           IF SEGMENT-FINNS                                               
055200             MOVE MFS-NUM-FAELT-FEL      TO MOD-IDARTNR-E-ATTR            
055300             MOVE NEJ                    TO INDATA-SW                     
055400             MOVE PART-EXISTS            TO MED-IDMFSFEL                  
055500                                            MSG-KOM-IDMFSMED              
055600           ELSE                                                           
055700             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-E-ATTR               
055800           END-IF                                                         
055801                                                                          
055810*---KOLLA ATT AVTAL FINNS PÅ WDK6                                         
055811           IF W-IDLEVNR = 'BQ8VA'                                         
055812*---UNDANTAG: INGEN KONTROLL FÖR CLASSIC (BQ8VA)                          
055813             MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDARTNR-E-ATTR            
055814           ELSE                                                           
055815              MOVE W-IDLEVNR             TO W-IDLEVNSH                    
055820              PERFORM IMS-GU-WDK6-AVTAL                                   
055830              IF SEGMENT-SAKNAS                                           
055840                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-E-ATTR            
055850                MOVE NEJ                 TO INDATA-SW                     
055860                MOVE AVTAL-MISSING-WDK6  TO MED-IDMFSFEL                  
055870                                            MSG-KOM-IDMFSMED              
055880              ELSE                                                        
055890                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-E-ATTR            
055891              END-IF                                                      
055892           END-IF                                                         
055900         END-IF                                                           
056000                                                                          
056100         IF MID-CMD = 'E'                                                 
056200*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
056300           IF WS-DASTADAT >= DAGENS-DATUM                                 
056400             MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-E-ATTR              
056500           ELSE                                                           
056600             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
056700                                          MSG-KOM-IDMFSMED                
056800             MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-E-ATTR             
056900             MOVE NEJ                  TO INDATA-SW                       
057000           END-IF                                                         
057100                                                                          
057200*---KOLLA SÅ ATT SEGMENT FINNS PÅ BAS                                     
057300           MOVE MID-IDARTNR-E  TO W-IDARTNR                               
057400           PERFORM IMS-GHU-ART                                            
057500           IF SEGMENT-SAKNAS                                              
057600             MOVE MFS-NUM-FAELT-FEL       TO MOD-IDARTNR-E-ATTR           
057700             MOVE NEJ                     TO INDATA-SW                    
057800             MOVE PART-MISSING            TO MED-IDMFSFEL                 
057900                                             MSG-KOM-IDMFSMED             
058000           ELSE                                                           
058100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-E-ATTR               
058200           END-IF                                                         
058300         END-IF                                                           
058400                                                                          
058500         IF MID-CMD = 'D'                                                 
058600*---KOLLA SÅ ATT SEGMENT FINNS PÅ BAS                                     
058700           MOVE MID-IDARTNR-E  TO W-IDARTNR                               
058800           PERFORM IMS-GHU-ART                                            
058900           IF SEGMENT-SAKNAS                                              
059000             MOVE MFS-NUM-FAELT-FEL       TO MOD-IDARTNR-E-ATTR           
059100             MOVE NEJ                     TO INDATA-SW                    
059200             MOVE PART-MISSING            TO MED-IDMFSFEL                 
059300                                             MSG-KOM-IDMFSMED             
059400           ELSE                                                           
059500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-E-ATTR               
059600           END-IF                                                         
059700         END-IF                                                           
059800       END-IF                                                             
059900                                                                          
060000       IF INDATA-FEL                                                      
060010         IF MED-IDMFSFEL = AVTAL-MISSING-WDK6                             
060020            MOVE 'AGREEMENT MISSING' TO MOD-TEMFSFEL                      
060030         ELSE                                                             
060100            CALL WMEDKONV USING MED-WMEDAREA                              
060200            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
060210         END-IF                                                           
060300         PERFORM MFS-ROER-EJ-FAELT-UT                                     
060400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
060500         MOVE NEJ TO ALLT-SW                                              
060600       END-IF                                                             
060700     END-IF                                                               
060800     .                                                                    
060900     EJECT                                                                
061000 H-UPPDATERA SECTION.                                                     
061100                                                                          
061200     MOVE MID-IDARTNR-E  TO W-IDARTNR                                     
061300     PERFORM IMS-GHU-ART                                                  
061400                                                                          
061500     IF SEGMENT-FINNS                                                     
061600       IF MID-CMD = 'D'                                                   
061700         PERFORM IMS-DLET-ART                                             
061800       ELSE                                                               
061900         IF MID-CMD = 'E'                                                 
062000           MOVE WS-DASTADAT TO ART-DASTADAT                               
062100           PERFORM IMS-REPL-ART                                           
062200         END-IF                                                           
062300       END-IF                                                             
062400     ELSE                                                                 
062500       MOVE MID-IDARTNR-E  TO ART-IDARTNR                                 
062600       MOVE WS-DASTADAT    TO ART-DASTADAT                                
062610       MOVE -99            TO ART-KVLS-DLEV                               
062611       MOVE ZERO           TO ART-TIINLMOT                                
062612                              ART-TIREGDAT                                
062620*      -99 BETYDER ATT SALDOT INTE UPPDATERAS AV LEVERANTÖR               
062621*      -99 BETYDER ATT SALDOT INTE UPPDATERAS AV LEVERANTÖR               
062622*      DET BETYDER ALLTSÅ INTE ATT VI HAR ETT NEGATIVT SALDO :-)          
062630*      FÖR LEVERANTÖRER SOM SKICKAR SALDOUPPGIFTER ÄR VÄRDET >= 0         
062640*      VID NYUPPLÄGG AV ARTIKLAR PÅ LEVERANTÖRER SOM REDOVISAR            
062650*      SALDO, KOMMER ARTIKELN SÅLEDES HA VÄRDET -99 TILL FÖRSTA           
062660*      UPPDATERINGEN AV SALDOT                                            
062670                                                                          
062700       PERFORM IMS-ISRT-ART                                               
062800       IF ARTIKEL-FINNS                                                   
062900         MOVE NEJ          TO INDATA-SW                                   
063000         MOVE NEJ          TO ALLT-SW                                     
063100         MOVE PART-EXISTS  TO MED-IDMFSFEL                                
063200                              MSG-KOM-IDMFSMED                            
063300         CALL WMEDKONV USING MED-WMEDAREA                                 
063400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
063500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
063600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
063700       END-IF                                                             
063800     END-IF                                                               
063900                                                                          
064000     IF INDATA-OK                                                         
064100       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
064200                               MSG-KOM-IDMFSMED                           
064300       CALL WMEDKONV USING MED-WMEDAREA                                   
064400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
064500       PERFORM MFS-FORM-ATTR                                              
064600       PERFORM MFS-RENSA-FAELT-IN                                         
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 MFS-RENSA-FAELT-UT SECTION.                                              
065100                                                                          
065200*    --- ALLA UTDATA-FÄLT                                                 
065300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
065400     MOVE MFS-RENSA-FAELT TO MOD-CMD                                      
065500                             MOD-IDARTNR-E                                
065600                             MOD-TISTADAT-E                               
065700                             MOD-TISTADAT-GRP                             
065800     MOVE 1 TO INDX                                                       
065900     PERFORM UNTIL INDX > MAX-INDX                                        
066000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
066100       ADD +1 TO INDX                                                     
066200     END-PERFORM                                                          
066300     .                                                                    
066400     SKIP3                                                                
066500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
066600                                                                          
066700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
066800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                           
066900                             MOD-TISTADAT (INDX)                          
067000     .                                                                    
067100     SKIP3                                                                
067200 MFS-RENSA-FAELT-IN SECTION.                                              
067300                                                                          
067400*    --- ALLA INDATA-FÄLT                                                 
067500     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
067600                             MOD-IDDIRGRP-IN                              
067700                             MOD-IDARTNR-IN                               
067800                             MOD-CMD                                      
067900                             MOD-IDARTNR-E                                
068000                             MOD-TISTADAT-E                               
068100     .                                                                    
068200     EJECT                                                                
068300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
068400                                                                          
068500*    --- ALLA UTDATA-FÄLT                                                 
068600*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
068700     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD                                    
068800                               MOD-IDARTNR-E                              
068900                               MOD-TISTADAT-E                             
069000                               MOD-TISTADAT-GRP                           
069100     MOVE +1 TO INDX                                                      
069200     PERFORM UNTIL INDX > MAX-INDX                                        
069300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
069400       ADD +1 TO INDX                                                     
069500     END-PERFORM                                                          
069600     SKIP2                                                                
069700     .                                                                    
069800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
069900                                                                          
070000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
070100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR (INDX)                         
070200                               MOD-TISTADAT (INDX)                        
070300     .                                                                    
070400     SKIP3                                                                
070500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
070600                                                                          
070700*    --- ALLA INDATA-FÄLT                                                 
070800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                             
070900                               MOD-IDDIRGRP-IN                            
071000                               MOD-IDARTNR-IN                             
071100                               MOD-CMD                                    
071200                               MOD-IDARTNR-E                              
071300                               MOD-TISTADAT-E                             
071400     .                                                                    
071500     EJECT                                                                
071600 MFS-FORM-ATTR SECTION.                                                   
071700                                                                          
071800*    --- ALLA INDATA-FÄLT                                                 
071900     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR                              
072000                                MOD-IDARTNR-E-ATTR                        
072100                                MOD-TISTADAT-E-ATTR                       
072200     .                                                                    
072300     SKIP2                                                                
072400 MFS-LAES-IN-IGEN SECTION.                                                
072500                                                                          
072600*    --- ALLA INDATA-FÄLT                                                 
072700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR                           
072800                                   MOD-IDARTNR-E-ATTR                     
072900                                   MOD-TISTADAT-E-ATTR                    
073000     .                                                                    
073100     EJECT                                                                
073200* --- IMS SEKTIONER ---                                                   
073300     SKIP3                                                                
073400 IMS-GET-MSG SECTION.                                                     
073500                                                                          
073600     MOVE '  QC' TO GODK-STATUSKODER                                      
073700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
073800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073900     PERFORM IMS-STATUSKONTROLL                                           
074000     .                                                                    
074100     SKIP3                                                                
074200 IMS-INSERT-MSG SECTION.                                                  
074300                                                                          
074400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
074500     MOVE SPACE TO GODK-STATUSKODER                                       
074600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
074700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074800     PERFORM IMS-STATUSKONTROLL                                           
074900     .                                                                    
075000     EJECT                                                                
075100 IMS-GET-WMSGKOM-MSG SECTION.                                             
075200                                                                          
075300     MOVE '  QD'   TO GODK-STATUSKODER                                    
075400     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
075500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075600     PERFORM IMS-STATUSKONTROLL                                           
075700     .                                                                    
075800     EJECT                                                                
075900                                                                          
076000 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
076100                                                                          
076200     MOVE '  '  TO GODK-STATUSKODER                                       
076300     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
076400     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-GU-WDK6-ART SECTION.                                                 
076900                                                                          
077000     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-WDK6-X ')'                    
077100          DELIMITED BY SIZE INTO SSA1                                     
077200     MOVE '  GE' TO GODK-STATUSKODER                                      
077300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
077400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     .                                                                    
077700     EJECT                                                                
077710 IMS-GU-WDK6-AVTAL SECTION.                                               
077720                                                                          
077740     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
077750            DELIMITED BY SIZE INTO SSA1                                   
077760     MOVE 'WLARTC11'            TO SSA2                                   
077780     STRING 'WLARTC23(IDLEVNSH =' W-IDLEVNSH-X ')'                        
077781            DELIMITED BY SIZE INTO SSA3                                   
077790     MOVE '  GE'                TO GODK-STATUSKODER                       
077791     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-K623 SSA1 SSA2 SSA3            
077792     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
077793     PERFORM IMS-STATUSKONTROLL                                           
077794     .                                                                    
077800 IMS-GU-LEV SECTION.                                                      
077900                                                                          
078000     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
078100          DELIMITED BY SIZE INTO SSA1                                     
078200     MOVE '  GE' TO GODK-STATUSKODER                                      
078300     CALL CBLTDLI USING GU LEVF-PCB DLI-IO-WLLEVF01 SSA1                  
078400     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
078500     PERFORM IMS-STATUSKONTROLL                                           
078600     .                                                                    
078700     EJECT                                                                
078800 IMS-GHU-ART SECTION.                                                     
078900                                                                          
079000     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
079100          DELIMITED BY SIZE INTO SSA1                                     
079200     STRING 'WLLEVF12(IDARTNR = ' W-IDARTNR-X ')'                         
079300          DELIMITED BY SIZE INTO SSA2                                     
079400     MOVE '  GE' TO GODK-STATUSKODER                                      
079500     CALL CBLTDLI USING GHU LEVF-PCB DLI-IO-WLLEVF12 SSA1 SSA2            
079600     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
079700     PERFORM IMS-STATUSKONTROLL                                           
079800     .                                                                    
079900     SKIP3                                                                
080000 IMS-GU-ART SECTION.                                                      
080100                                                                          
080200     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
080300          DELIMITED BY SIZE INTO SSA1                                     
080400     STRING 'WLLEVF12(IDARTNR >=' W-IDARTNR-X ')'                         
080500          DELIMITED BY SIZE INTO SSA2                                     
080600     MOVE '  GE' TO GODK-STATUSKODER                                      
080700     CALL CBLTDLI USING GU LEVF-PCB DLI-IO-WLLEVF12 SSA1 SSA2             
080800     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
080900     PERFORM IMS-STATUSKONTROLL                                           
081000     .                                                                    
081100     SKIP3                                                                
081200 IMS-GET-ART SECTION.                                                     
081300                                                                          
081400     STRING 'WLLEVF12(IDARTNR >=' W-IDARTNR-X ')'                         
081500          DELIMITED BY SIZE INTO SSA1                                     
081600     MOVE '  GE' TO GODK-STATUSKODER                                      
081700     CALL CBLTDLI USING GU LEVF-PCB DLI-IO-WLLEVF12 SSA1                  
081800     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
081900     PERFORM IMS-STATUSKONTROLL                                           
082000     .                                                                    
082100     SKIP3                                                                
082200 IMS-GN-ART SECTION.                                                      
082300                                                                          
082400     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
082500          DELIMITED BY SIZE INTO SSA1                                     
082600     MOVE 'WLLEVF12 ' TO SSA2                                             
082700     MOVE '  GE' TO GODK-STATUSKODER                                      
082800     CALL CBLTDLI USING GN LEVF-PCB DLI-IO-WLLEVF12 SSA1 SSA2             
082900     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
083000     PERFORM IMS-STATUSKONTROLL                                           
083100     .                                                                    
083200     SKIP3                                                                
083300 IMS-ISRT-ART SECTION.                                                    
083400                                                                          
083500     MOVE 'WLLEVF12 ' TO SSA1                                             
083600     MOVE '  IINI' TO GODK-STATUSKODER                                    
083700     CALL CBLTDLI USING ISRT LEVF-PCB DLI-IO-WLLEVF12 SSA1                
083800     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
083900     PERFORM IMS-STATUSKONTROLL                                           
084000     .                                                                    
084100     SKIP3                                                                
084200 IMS-REPL-ART SECTION.                                                    
084300                                                                          
084400     MOVE '  ' TO GODK-STATUSKODER                                        
084500     CALL CBLTDLI USING REPL LEVF-PCB DLI-IO-WLLEVF12                     
084600     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
084700     PERFORM IMS-STATUSKONTROLL                                           
084800     .                                                                    
084900     SKIP3                                                                
085000 IMS-DLET-ART SECTION.                                                    
085100                                                                          
085200     MOVE '  ' TO GODK-STATUSKODER                                        
085300     CALL CBLTDLI USING DLET LEVF-PCB DLI-IO-WLLEVF12                     
085400     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
085500     PERFORM IMS-STATUSKONTROLL                                           
085600     .                                                                    
085700     EJECT                                                                
085800 IMS-STATUSKONTROLL SECTION.                                              
085900                                                                          
086000     SET STATUS-IX TO 1                                                   
086100     SEARCH GODK-STATUS                                                   
086200       AT END                                                             
086300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
086400         DELIMITED BY SIZE INTO FELTEXT                                   
086500         CALL FELLOG                                                      
086600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
086700         CONTINUE                                                         
086800     END-SEARCH                                                           
086900     .                                                                    
