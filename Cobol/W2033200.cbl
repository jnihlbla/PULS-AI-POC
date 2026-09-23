000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033200.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/10/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMETS HUVUDFUNKTIONER ÄR FÖLJANDE:                         
000900*        -FRÅGA PÅ LEVERANTÖRSGRUPPER, MATA IN LEVERANTÖRSNR              
001000*        OCH FÅ UT INFORMATION OM VILKA GRUPPER SOM FINNS FÖR             
001100*        DENNE LEVERANTÖR                                                 
001200*        -LÄGGA UPP NYA LEVERANTÖRSGRUPPER (N)                            
001300*        -EDITERA BEFINTLIGA LEVERANTÖRSGRUPPER (E)                       
001400*        -TA BORT GAMLA LEVERANTÖRSGRUPPER (D)                            
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLLEVF (WDF2)                              
001700*                                                                         
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W2T332                                              
002100*        MID:         W2I33201                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W2O33201                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W2033200'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- GENERELLA ARBETSFÄLT                                             
004200 01  DAGENS-DATUM                PIC 9(8).                                
004300                                                                          
004400 01 WS-DATUM.                                                             
004500    03 WS-AAR                    PIC 9(2).                                
004600    03 FILLER                    PIC 9(4).                                
004700                                                                          
004800 01  WS-DASTADAT.                                                         
004900     03  WS-DASTADAT-AAR         PIC 9(2).                                
005000     03  WS-DASTADAT-TI          PIC 9(6).                                
005100                                                                          
005200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  MAX-INDX                    PIC S9(4)  VALUE +36   COMP SYNC.        
005500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600                                                                          
005700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005800     88  ALLT-OK                             VALUE 'J'.                   
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
006900     88  EGEN-MID                            VALUE '2332'.                
007000     88  GODK-MID                            VALUE '2331' '2332'          
007100                                                   '2333' '2334'          
007200                                                   '2335' '2336'          
007300                                                   '2337' '2338'          
007400                                                   '2339'.                
007500     88  HELP-MID                            VALUE '0551'.                
007600     EJECT                                                                
007700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007800 01  GENERELLA-SUBPROGRAM.                                                
007900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008600*01 -COPY WMEDAREA                                                        
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
008900*   -COPY WDATAREA                                                        
009000     EJECT                                                                
009100     SKIP3                                                                
009200 01  MESSAGE-CODES.                                                       
009300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010000     03  SUPPLIER-MISSING        PIC X(3)    VALUE '273'.                 
010100     03  GROUP-EXISTS            PIC X(3)    VALUE '274'.                 
010200     03  GROUP-MISSING           PIC X(3)    VALUE '275'.                 
010300     03  PARTS-IN-GROUP          PIC X(3)    VALUE '276'.                 
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010800     SKIP3                                                                
010900*01 -COPY WMSGINIT                                                        
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
011200     SKIP3                                                                
011300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
011400*                                                                         
011500 01  SPAR-AREA.                                                           
011600     03  SPAR-IDTRANS              PIC X(4)    VALUE '2332'.              
011700     03  SPAR-IDDIRGRP-ENTER       PIC X(10)   VALUE ZERO.                
011800     03  SPAR-IDDIRGRP-NEXT        PIC X(10)   VALUE ZERO.                
011900     EJECT                                                                
012000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012300     SKIP3                                                                
012400*01  MID -COPY W2I33201                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012700     SKIP3                                                                
012800*01  -COPY WMSGAREA                                                       
012900     EJECT                                                                
013000     03  MOD REDEFINES MSG-AREA.                                          
013100*      05  -COPY W2O33201                                                 
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013400     SKIP3                                                                
013500*01  -COPY WMFSAREA                                                       
013600     EJECT                                                                
013700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013800*                                                                         
013900     EJECT                                                                
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100     SKIP3                                                                
014200 01  NYCKLAR-TILL-DLI.                                                    
014300*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
014400                                                                          
014500     03  W-IDDIRGRP-X.                                                    
014600         05  W-IDDIRGRP-BSEQ     PIC X(10)   VALUE SPACE.                 
014700                                                                          
014800     03  W-IDLEVNR-WDF1-X.                                                
014900         05  W-IDLEVNR-WDF1      PIC  X(5)   VALUE SPACE.                 
015000                                                                          
015100     03  W-WDF2BSEQ-X.                                                    
015200         05  W-IDLEVNR-BSEQ      PIC  X(5)   VALUE SPACE.                 
015300                                                                          
015400     03  W-WDF2BSEQ-MIN-X.                                                
015500         05  W-IDLEVNR-MIN       PIC  X(5)   VALUE SPACE.                 
015600                                                                          
015700     03  W-WDF2BSEQ-MAX-X.                                                
015800         05  W-IDLEVNR-MAX       PIC  X(5)   VALUE SPACE.                 
015900                                                                          
016000     03  W-WDF201KY-X.                                                    
016100         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
016200         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
016300                                                                          
016400     SKIP2                                                                
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000     88  BAS-SLUT                            VALUE 'GB'.                  
017100     SKIP2                                                                
017200 01  GODK-STATUSKODER.                                                    
017300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017400     SKIP3                                                                
017500 01  SSA1                        PIC X(64).                               
017600 01  SSA2                        PIC X(64).                               
017700     EJECT                                                                
017800*    --- IMS FUNKTIONSKODER                                               
017900*01  -COPY W0003                                                          
018000     EJECT                                                                
018100*    ---  DLI INPUT-OUTPUT AREA                                           
018200                                                                          
018300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVF01'.                    
018400 01  DLI-IO-WLLEVF01.                                                     
018500*    03  -COPY WDF201                                                     
018600     EJECT                                                                
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVF12'.                    
018800 01  DLI-IO-WLLEVF12.                                                     
018900*    03  -COPY WDF212                                                     
019000     EJECT                                                                
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA01'.                    
019200 01  DLI-IO-WLLEVA01.                                                     
019300*    03  -COPY WDF101 -PRE WDF1-                                          
019400     EJECT                                                                
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA14'.                    
019600 01  DLI-IO-WLLEVA14.                                                     
019700*    03  -COPY WDF106 -PRE WDF1-                                          
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000*01  -COPY W0009   -PRE MSG-                                              
020100*01  -COPY W0008   -PRE USEA-                                             
020200     05  FILLER                  PIC X.                                   
020300                                                                          
020400*01  -COPY W0008  -PRE LEVF-                                              
020500     05  FILLER                  PIC X.                                   
020600                                                                          
020700*01  -COPY W0008  -PRE LEVFB-                                             
020800     05  FILLER                  PIC X.                                   
020900                                                                          
021000*01  -COPY W0008  -PRE LEVA-                                              
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB LEVF-PCB                      
021400                           LEVFB-PCB LEVA-PCB.                            
021500 MAIN SECTION.                                                            
021600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB LEVF-PCB                      
021700                           LEVFB-PCB LEVA-PCB.                            
021800                                                                          
021900     PERFORM IMS-GET-MSG                                                  
022000     IF SEGMENT-FINNS                                                     
022100       PERFORM A-INIT                                                     
022200       PERFORM B-KOLLA-NYCKLAR                                            
022300       IF NYCKLAR-OK                                                      
022400         IF MFS-UPDATE                                                    
022500           PERFORM G-KOLLA-INPUT                                          
022600           IF INDATA-OK                                                   
022700             PERFORM H-UPPDATERA                                          
022800           END-IF                                                         
022900         ELSE                                                             
023000           IF MFS-FIRST                                                   
023100             PERFORM C-FOERSTA-SIDA                                       
023200           ELSE                                                           
023300             IF MFS-NEXT                                                  
023400               PERFORM D-NAESTA-SIDA                                      
023500             ELSE                                                         
023600               PERFORM E-SAMMA-SIDA                                       
023700             END-IF                                                       
023800           END-IF                                                         
023900         END-IF                                                           
024000         IF ALLT-OK                                                       
024100           PERFORM F-LAES-VISA-INFO                                       
024200         END-IF                                                           
024300       END-IF                                                             
024400       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33201 + 4                      
024500       PERFORM IMS-INSERT-MSG                                             
024600     END-IF                                                               
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
025500                                                                          
025600     IF MSG-DUBBLA-TRANSKODER                                             
025700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33201                 
025800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026000     ELSE                                                                 
026100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I33201                  
026200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026400     END-IF                                                               
026500                                                                          
026600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026900                                                                          
027000     MOVE LOW-VALUE TO MSG-AREA                                           
027100     MOVE 'W2O332N1' TO MFS-IDMOD                                         
027200     MOVE '2332' TO MOD-IDTRANS                                           
027300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027400                                                                          
027500                                                                          
027600     IF EGEN-MID OR HELP-MID                                              
027700       CONTINUE                                                           
027800     ELSE                                                                 
027900       MOVE SPACE TO MFS-KDTRTYP                                          
028000       MOVE '7' TO MFS-IDPFK                                              
028100     END-IF                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 B-KOLLA-NYCKLAR SECTION.                                                 
028500                                                                          
028600     MOVE LOW-VALUE         TO W-WDF2BSEQ-MIN-X                           
028700     MOVE HIGH-VALUE        TO W-WDF2BSEQ-MAX-X                           
028800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028900     MOVE '001'             TO MSGI-KDCALL                                
029000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029200     MOVE '2332'            TO MSGI-IDTRANS                               
029300                                                                          
029400     IF EGEN-MID                                                          
029500       MOVE MID-IDLEVNR-IN  TO MSGI-IDLEVNR                               
029600     END-IF                                                               
029700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029800     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
029900                                                                          
030000     IF MSGI-IDLAND-SPR = 'GB'                                            
030100       MOVE 'GB' TO MED-IDSKYLT                                           
030200     ELSE                                                                 
030300       MOVE 'S' TO MED-IDSKYLT                                            
030400     END-IF                                                               
030500                                                                          
030600     MOVE JA TO ALLT-SW                                                   
030700     MOVE JA TO NYCKLAR-SW                                                
030800     MOVE SPACE TO MED-IDMFSFEL                                           
030900     MOVE SPACE TO MED-IDMFSINF                                           
031000                                                                          
031100*    -- KONTROLL AV IDLEVNR                                               
031200     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
031300                                                                          
031400     IF MSGI-IDLEVNR NOT = SPACE                                          
031600        MOVE MSGI-IDLEVNR  TO W-IDLEVNR                                   
031700                              W-IDLEVNR-MIN                               
031800                              W-IDLEVNR-MAX                               
031900                              W-IDLEVNR-BSEQ                              
032000                              W-IDLEVNR-WDF1                              
032400     ELSE                                                                 
032500        MOVE NEJ           TO NYCKLAR-SW                                  
032600     END-IF                                                               
032700                                                                          
032800     IF GODK-MID OR NYCKLAR-OK                                            
032900       MOVE MSGI-IDLEVNR    TO MOD-IDLEVNR-UT                             
033000     ELSE                                                                 
033100       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                             
033200     END-IF                                                               
033300                                                                          
033400     IF NYCKLAR-FEL                                                       
033500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
033600       CALL WMEDKONV USING MED-WMEDAREA                                   
033700       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
033800       PERFORM MFS-RENSA-FAELT-IN                                         
033900       PERFORM MFS-RENSA-FAELT-UT                                         
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 C-FOERSTA-SIDA SECTION.                                                  
034400                                                                          
034500     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
034600     CALL WMEDKONV USING MED-WMEDAREA                                     
034700     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
034800                                                                          
034900     PERFORM MFS-RENSA-FAELT-IN                                           
035000     .                                                                    
035100     EJECT                                                                
035200 D-NAESTA-SIDA SECTION.                                                   
035300                                                                          
035400     IF SPAR-IDTRANS = '2332'                                             
035500       MOVE SPAR-IDDIRGRP-NEXT TO W-IDDIRGRP-BSEQ                         
035600     ELSE                                                                 
035700       PERFORM MFS-RENSA-FAELT-IN                                         
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 E-SAMMA-SIDA SECTION.                                                    
036200                                                                          
036300     IF SPAR-IDTRANS = '2332' OR '0551'                                   
036400       MOVE SPAR-IDDIRGRP-ENTER TO W-IDDIRGRP                             
036500       IF MID-IDDIRGRP-E   = ALL '+'                                      
036600        OR MID-CMD        = ALL '+'                                       
036700         PERFORM MFS-RENSA-FAELT-IN                                       
036800       ELSE                                                               
036900         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
037000         CALL WMEDKONV USING MED-WMEDAREA                                 
037100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
037200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
037300         PERFORM MFS-LAES-IN-IGEN                                         
037400       END-IF                                                             
037500     ELSE                                                                 
037600       PERFORM MFS-RENSA-FAELT-IN                                         
037700     END-IF                                                               
037800                                                                          
037900     .                                                                    
038000     EJECT                                                                
038100 F-LAES-VISA-INFO SECTION.                                                
038200                                                                          
038300     PERFORM IMS-GU-WDF1-LEV                                              
038400                                                                          
038500     IF SEGMENT-SAKNAS                                                    
038600       MOVE SUPPLIER-MISSING TO MED-IDMFSFEL                              
038700       CALL WMEDKONV USING MED-WMEDAREA                                   
038800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
038900       PERFORM MFS-RENSA-FAELT-UT                                         
039000     ELSE                                                                 
039100       PERFORM IMS-GN-WDF1-ADR                                            
039200       IF SEGMENT-FINNS                                                   
039300         MOVE WDF1-ADR-BELEV TO MOD-BELEV                                 
039400       END-IF                                                             
039500       PERFORM FA-LAES-VISA-WDF2                                          
039600     END-IF                                                               
039700                                                                          
039800     MOVE '002'               TO MSGI-KDCALL                              
039900     MOVE '2332'              TO SPAR-IDTRANS                             
040000     MOVE SPAR-AREA           TO MSGI-SPAR-AREA                           
040100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040200     .                                                                    
040300     EJECT                                                                
040400 FA-LAES-VISA-WDF2 SECTION.                                               
040500                                                                          
040600     IF MFS-NEXT                                                          
040700       PERFORM IMS-GU-LEV-NEXT                                            
040800     ELSE                                                                 
040900       PERFORM IMS-GU-LEV                                                 
041000     END-IF                                                               
041100                                                                          
041200     MOVE LEV-IDDIRGRP TO SPAR-IDDIRGRP-ENTER                             
041300     MOVE +1 TO INDX                                                      
041400     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                      
041500     OR BAS-SLUT                                                          
041600       IF SEGMENT-FINNS                                                   
041700         MOVE LEV-IDDIRGRP      TO MOD-IDDIRGRP (INDX)                    
041800         MOVE LEV-DASTADAT(3:6) TO MOD-TISTADAT (INDX)                    
041900       ELSE                                                               
042000         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
042100       END-IF                                                             
042200       ADD +1 TO INDX                                                     
042300       PERFORM IMS-GN-LEV                                                 
042400     END-PERFORM                                                          
042500                                                                          
042600     IF SEGMENT-FINNS                                                     
042700       MOVE LEV-IDDIRGRP         TO SPAR-IDDIRGRP-NEXT                    
042800       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
042900       CALL WMEDKONV USING MED-WMEDAREA                                   
043000       MOVE MED-TEMFSINF         TO MOD-TEMFSINF                          
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 G-KOLLA-INPUT SECTION.                                                   
043500                                                                          
043600     MOVE JA  TO INDATA-SW                                                
043700     MOVE JA  TO ALLT-SW                                                  
043800     IF MID-CMD = ALL '+'                                                 
043900      AND MID-IDDIRGRP-E = ALL '+'                                        
044000      AND MID-TISTADAT-E = ALL '+'                                        
044100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
044200       CALL WMEDKONV USING MED-WMEDAREA                                   
044300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
044500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
044600       MOVE NEJ TO INDATA-SW                                              
044700     ELSE                                                                 
044800                                                                          
044900       IF MID-CMD = 'N' OR MID-CMD = 'E' OR MID-CMD = 'D'                 
045000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR                       
045100       ELSE                                                               
045200          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
045300          MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR                       
045400          MOVE NEJ                  TO INDATA-SW                          
045500          MOVE NEJ                  TO ALLT-SW                            
045600       END-IF                                                             
045700                                                                          
045800       IF MID-IDDIRGRP-E(1:1) = SPACE                                     
045900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
046000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDIRGRP-E-ATTR                 
046100         MOVE NEJ                  TO INDATA-SW                           
046200         MOVE NEJ                  TO ALLT-SW                             
046300       ELSE                                                               
046400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDIRGRP-E-ATTR                 
046500       END-IF                                                             
046600                                                                          
046700*---DATUM LAGRAS 8-STÄLLIGT PÅ BAS, DÄRFÖR DENNA KOD                      
046800       IF MID-TISTADAT-E = ALL '+'                                        
046900*---OM MAN EJ MATAT IN DATUM TAS DAGENS DATUM                             
047000         MOVE DAGENS-DATUM   TO WS-DASTADAT                               
047100         MOVE WS-DASTADAT-TI TO MID-TISTADAT-E                            
047200       ELSE                                                               
047300         MOVE MID-TISTADAT-E TO WS-DATUM                                  
047400                                WS-DASTADAT-TI                            
047500         IF WS-AAR < 50                                                   
047600           MOVE 20 TO WS-DASTADAT-AAR                                     
047700         ELSE                                                             
047800           MOVE 19 TO WS-DASTADAT-AAR                                     
047900         END-IF                                                           
048000       END-IF                                                             
048100*---SLUT DATUMKOD                                                         
048200                                                                          
048300*---VALIDERING AV DATUM                                                   
048400       MOVE 'AAMMDD'               TO DAT-KDDATFORM                       
048500       MOVE MID-TISTADAT-E         TO DAT-I-TIDATUM                       
048600                                                                          
048700       CALL WDATKONV USING         DAT-KDDATFORM                          
048800                                   DAT-I-TIDATUM                          
048900                                   DAT-O-TIDATUM                          
049000                                   DAT-KDSVAR                             
049100       IF DAT-KDSVAR-FEL                                                  
049200         MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                      
049300         MOVE MFS-NUM-FAELT-FEL      TO MOD-TISTADAT-E-ATTR               
049400         MOVE NEJ                    TO INDATA-SW                         
049500         MOVE NEJ                    TO ALLT-SW                           
049600       END-IF                                                             
049700                                                                          
049800*---KOLLA SÅ ATT LEVERANTÖR FINNS PÅ LEVERANTÖRSREGISTRET                 
049900       PERFORM IMS-GU-WDF1-LEV                                            
050000       IF SEGMENT-SAKNAS                                                  
050100         MOVE NEJ                   TO INDATA-SW                          
050200         MOVE NEJ                   TO ALLT-SW                            
050300         MOVE SUPPLIER-MISSING      TO MED-IDMFSFEL                       
050400       END-IF                                                             
050500                                                                          
050600*---OM LEVERANTÖR SAKNAS PÅ LEVERANTÖRSREGISTRET SÄTTER MAN               
050700*---NEJ TILL ALLT-OK FÖR ATT SLIPPA GÖRA ONÖDIGA DATABASLÄSNINGAR         
050800       IF ALLT-OK                                                         
050900         IF MID-CMD = 'N'                                                 
051000*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
051100           IF WS-DASTADAT >= DAGENS-DATUM                                 
051200             MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-E-ATTR              
051300           ELSE                                                           
051400             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
051500             MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-E-ATTR             
051600             MOVE NEJ                  TO INDATA-SW                       
051700           END-IF                                                         
051800                                                                          
051900*---KOLLA SÅ ATT INTE SEGMENT REDAN FINNS PÅ BAS                          
052000           MOVE MID-IDDIRGRP-E TO W-IDDIRGRP                              
052100           PERFORM IMS-GHU-LEV                                            
052200           IF SEGMENT-FINNS                                               
052300             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDIRGRP-E-ATTR             
052400             MOVE NEJ                  TO INDATA-SW                       
052500             MOVE GROUP-EXISTS         TO MED-IDMFSFEL                    
052600           ELSE                                                           
052700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDIRGRP-E-ATTR             
052800           END-IF                                                         
052900         END-IF                                                           
053000                                                                          
053100         IF MID-CMD = 'E'                                                 
053200*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
053300           IF WS-DASTADAT >= DAGENS-DATUM                                 
053400             MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISTADAT-E-ATTR             
053500           ELSE                                                           
053600             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
053700             MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-E-ATTR             
053800             MOVE NEJ                  TO INDATA-SW                       
053900           END-IF                                                         
054000                                                                          
054100*---KOLLA SÅ SEGMENT FINNS PÅ BAS                                         
054200           MOVE MID-IDDIRGRP-E TO W-IDDIRGRP                              
054300           PERFORM IMS-GHU-LEV                                            
054400           IF SEGMENT-SAKNAS                                              
054500             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDIRGRP-E-ATTR             
054600             MOVE NEJ                  TO INDATA-SW                       
054700             MOVE  GROUP-MISSING       TO MED-IDMFSFEL                    
054800           ELSE                                                           
054900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDIRGRP-E-ATTR             
055000           END-IF                                                         
055100         END-IF                                                           
055200                                                                          
055300         IF MID-CMD = 'D'                                                 
055400*---KOLLA SÅ SEGMENT FINNS PÅ BAS                                         
055500           MOVE MID-IDDIRGRP-E TO W-IDDIRGRP                              
055600           PERFORM IMS-GHU-LEV                                            
055700           IF SEGMENT-SAKNAS                                              
055800             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDIRGRP-E-ATTR             
055900             MOVE NEJ                  TO INDATA-SW                       
056000             MOVE GROUP-MISSING        TO MED-IDMFSFEL                    
056100           ELSE                                                           
056200             PERFORM IMS-GN-ART                                           
056300             IF SEGMENT-FINNS                                             
056400               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDIRGRP-E-ATTR             
056500               MOVE NEJ                TO INDATA-SW                       
056600               MOVE PARTS-IN-GROUP     TO MED-IDMFSFEL                    
056700             ELSE                                                         
056800               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDIRGRP-E-ATTR           
056900             END-IF                                                       
057000           END-IF                                                         
057100         END-IF                                                           
057200       END-IF                                                             
057300                                                                          
057400       IF INDATA-FEL                                                      
057500         CALL WMEDKONV USING MED-WMEDAREA                                 
057600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
057700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
057800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
057900         MOVE NEJ TO ALLT-SW                                              
058000       END-IF                                                             
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 H-UPPDATERA SECTION.                                                     
058500                                                                          
058600     MOVE MID-IDDIRGRP-E TO W-IDDIRGRP                                    
058700     PERFORM IMS-GHU-LEV                                                  
058800                                                                          
058900     IF SEGMENT-FINNS                                                     
059000       IF MID-CMD = 'D'                                                   
059100         PERFORM IMS-DLET-LEV                                             
059200       ELSE                                                               
059300         IF MID-CMD = 'E'                                                 
059400           MOVE WS-DASTADAT TO LEV-DASTADAT                               
059500           PERFORM IMS-REPL-LEV                                           
059600         END-IF                                                           
059700       END-IF                                                             
059800     ELSE                                                                 
059900       MOVE MSGI-IDLEVNR   TO LEV-IDLEVNR                                 
060000       MOVE MID-IDDIRGRP-E TO LEV-IDDIRGRP                                
060100       MOVE WS-DASTADAT    TO LEV-DASTADAT                                
060300       PERFORM IMS-ISRT-LEV                                               
060400     END-IF                                                               
060500                                                                          
060600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
060700     CALL WMEDKONV USING MED-WMEDAREA                                     
060800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
060900     PERFORM MFS-FORM-ATTR                                                
061000     PERFORM MFS-RENSA-FAELT-IN                                           
061100     .                                                                    
061200     EJECT                                                                
061300 MFS-RENSA-FAELT-UT SECTION.                                              
061400                                                                          
061500*    --- ALLA UTDATA-FÄLT                                                 
061600*    --- INKL. BLÄDDRINGSNYCKLAR                                          
061700     MOVE MFS-RENSA-FAELT TO MOD-CMD                                      
061800                             MOD-IDDIRGRP-E                               
061900                             MOD-TISTADAT-E                               
062000                             MOD-BELEV                                    
062100     MOVE +1 TO INDX                                                      
062200     PERFORM UNTIL INDX > MAX-INDX                                        
062300       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
062400       ADD +1 TO INDX                                                     
062500     END-PERFORM                                                          
062600     .                                                                    
062700     SKIP3                                                                
062800 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
062900                                                                          
063000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
063100     MOVE MFS-RENSA-FAELT TO MOD-IDDIRGRP (INDX)                          
063200                             MOD-TISTADAT (INDX)                          
063300     .                                                                    
063400     SKIP3                                                                
063500 MFS-RENSA-FAELT-IN SECTION.                                              
063600                                                                          
063700*    --- ALLA INDATA-FÄLT                                                 
063800     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
063900                             MOD-CMD                                      
064000                             MOD-IDDIRGRP-E                               
064100                             MOD-TISTADAT-E                               
064200     .                                                                    
064300     EJECT                                                                
064400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
064500                                                                          
064600*    --- ALLA UTDATA-FÄLT                                                 
064700*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
064800     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD                                    
064900                               MOD-IDDIRGRP-E                             
065000                               MOD-TISTADAT-E                             
065100     MOVE +1 TO INDX                                                      
065200     PERFORM UNTIL INDX > MAX-INDX                                        
065300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
065400       ADD +1 TO INDX                                                     
065500     END-PERFORM                                                          
065600     SKIP2                                                                
065700     .                                                                    
065800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
065900                                                                          
066000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
066100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDIRGRP (INDX)                        
066200                               MOD-TISTADAT (INDX)                        
066300     .                                                                    
066400     SKIP3                                                                
066500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
066600                                                                          
066700*    --- ALLA INDATA-FÄLT                                                 
066800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                             
066900                               MOD-CMD                                    
067000                               MOD-IDDIRGRP-E                             
067100                               MOD-TISTADAT-E                             
067200     .                                                                    
067300     EJECT                                                                
067400 MFS-FORM-ATTR SECTION.                                                   
067500                                                                          
067600*    --- ALLA INDATA-FÄLT                                                 
067700     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR                              
067800                                MOD-IDDIRGRP-E-ATTR                       
067900                                MOD-TISTADAT-E-ATTR                       
068000     .                                                                    
068100     SKIP2                                                                
068200 MFS-LAES-IN-IGEN SECTION.                                                
068300                                                                          
068400*    --- ALLA INDATA-FÄLT                                                 
068500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR                           
068600                                   MOD-IDDIRGRP-E-ATTR                    
068700                                   MOD-TISTADAT-E-ATTR                    
068800     .                                                                    
068900     EJECT                                                                
069000* --- IMS SEKTIONER ---                                                   
069100     SKIP3                                                                
069200 IMS-GET-MSG SECTION.                                                     
069300                                                                          
069400     MOVE '  QC' TO GODK-STATUSKODER                                      
069500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
069600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     .                                                                    
069900     SKIP3                                                                
070000 IMS-INSERT-MSG SECTION.                                                  
070100                                                                          
070200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070300     MOVE SPACE TO GODK-STATUSKODER                                       
070400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070600     PERFORM IMS-STATUSKONTROLL                                           
070700     .                                                                    
070800     EJECT                                                                
070900 IMS-GU-WDF1-LEV SECTION.                                                 
071000                                                                          
071100     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-WDF1-X ')'                    
071200          DELIMITED BY SIZE INTO SSA1                                     
071300     MOVE '  GE' TO GODK-STATUSKODER                                      
071400     CALL CBLTDLI USING GHU LEVA-PCB DLI-IO-WLLEVA01 SSA1                 
071500     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800     SKIP3                                                                
071900 IMS-GN-WDF1-ADR SECTION.                                                 
072000                                                                          
072100     MOVE 'WLLEVA14 ' TO SSA1                                             
072200     MOVE '  GE' TO GODK-STATUSKODER                                      
072300     CALL CBLTDLI USING GN LEVA-PCB DLI-IO-WLLEVA14 SSA1                  
072400     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
072500     PERFORM IMS-STATUSKONTROLL                                           
072600     .                                                                    
072700     SKIP3                                                                
072800 IMS-GU-LEV SECTION.                                                      
072900                                                                          
073000     STRING 'WLLEVF01(WDF2BSEQ>=' W-WDF2BSEQ-MIN-X                        
073100                    '&WDF2BSEQ<=' W-WDF2BSEQ-MAX-X ')'                    
073200          DELIMITED BY SIZE INTO SSA1                                     
073300     MOVE '  GE' TO GODK-STATUSKODER                                      
073400     CALL CBLTDLI USING GU LEVFB-PCB DLI-IO-WLLEVF01 SSA1                 
073500     MOVE LEVFB-STATUS-CODE TO STATUS-WS                                  
073600     PERFORM IMS-STATUSKONTROLL                                           
073700     .                                                                    
073800     SKIP3                                                                
073900 IMS-GU-LEV-NEXT SECTION.                                                 
074000                                                                          
074100     STRING 'WLLEVF01(WDF2BSEQ =' W-WDF2BSEQ-X ')'                        
074200                    '&IDDIRGRP =' W-IDDIRGRP-X ')'                        
074300          DELIMITED BY SIZE INTO SSA1                                     
074400     MOVE '  GE' TO GODK-STATUSKODER                                      
074500     CALL CBLTDLI USING GU LEVFB-PCB DLI-IO-WLLEVF01 SSA1                 
074600     MOVE LEVFB-STATUS-CODE TO STATUS-WS                                  
074700     PERFORM IMS-STATUSKONTROLL                                           
074800     .                                                                    
074900     SKIP3                                                                
075000 IMS-GHU-LEV SECTION.                                                     
075100                                                                          
075200     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
075300          DELIMITED BY SIZE INTO SSA1                                     
075400     MOVE '  GE' TO GODK-STATUSKODER                                      
075500     CALL CBLTDLI USING GHU LEVF-PCB DLI-IO-WLLEVF01 SSA1                 
075600     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     SKIP3                                                                
076000 IMS-GN-LEV SECTION.                                                      
076100                                                                          
076200     STRING 'WLLEVF01(WDF2BSEQ>=' W-WDF2BSEQ-MIN-X                        
076300                    '&WDF2BSEQ<=' W-WDF2BSEQ-MAX-X ')'                    
076400          DELIMITED BY SIZE INTO SSA1                                     
076500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
076600     CALL CBLTDLI USING GN LEVFB-PCB DLI-IO-WLLEVF01 SSA1                 
076700     MOVE LEVFB-STATUS-CODE TO STATUS-WS                                  
076800     PERFORM IMS-STATUSKONTROLL                                           
076900     .                                                                    
077000     SKIP3                                                                
077100 IMS-ISRT-LEV SECTION.                                                    
077200                                                                          
077300     MOVE 'WLLEVF01 ' TO SSA1                                             
077400     MOVE '  II' TO GODK-STATUSKODER                                      
077500     CALL CBLTDLI USING ISRT LEVF-PCB DLI-IO-WLLEVF01 SSA1                
077600     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
077700     PERFORM IMS-STATUSKONTROLL                                           
077800     .                                                                    
077900     SKIP3                                                                
078000 IMS-REPL-LEV SECTION.                                                    
078100                                                                          
078200     MOVE '  ' TO GODK-STATUSKODER                                        
078300     CALL CBLTDLI USING REPL LEVF-PCB DLI-IO-WLLEVF01                     
078400     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
078500     PERFORM IMS-STATUSKONTROLL                                           
078600     .                                                                    
078700     SKIP3                                                                
078800 IMS-DLET-LEV SECTION.                                                    
078900                                                                          
079000     MOVE '  ' TO GODK-STATUSKODER                                        
079100     CALL CBLTDLI USING DLET LEVF-PCB DLI-IO-WLLEVF01                     
079200     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
079300     PERFORM IMS-STATUSKONTROLL                                           
079400     .                                                                    
079500     EJECT                                                                
079600 IMS-GN-ART SECTION.                                                      
079700                                                                          
079800     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
079900          DELIMITED BY SIZE INTO SSA1                                     
080000     MOVE 'WLLEVF12 ' TO SSA2                                             
080100     MOVE '  GE' TO GODK-STATUSKODER                                      
080200     CALL CBLTDLI USING GN LEVF-PCB DLI-IO-WLLEVF12 SSA1 SSA2             
080300     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
080400     PERFORM IMS-STATUSKONTROLL                                           
080500     .                                                                    
080600     SKIP3                                                                
080700 IMS-STATUSKONTROLL SECTION.                                              
080800                                                                          
080900     SET STATUS-IX TO 1                                                   
081000     SEARCH GODK-STATUS                                                   
081100       AT END                                                             
081200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
081300         DELIMITED BY SIZE INTO FELTEXT                                   
081400         CALL FELLOG                                                      
081500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
081600         CONTINUE                                                         
081700     END-SEARCH                                                           
081800     .                                                                    
