000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4075400.                                                
000300 AUTHOR.         LENA BROMANDER.                                          
000400 DATE-WRITTEN.   17/06/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
001100                                                                          
001200                                                                          
001300*    FUNKTION:                                                            
001400*        PROGRAMMETS HUVUDFUNKTIONER ÄR FÖLJANDE:                         
001500*        -VISA DE LEVERANSKODER PER IDDC SOM SKALL GÅ                     
001600*        AUTOMATISKT FÖR UTREDNING TILL REM QUEUE                         
001700*        ISTÄLLET FÖR ATT GÅ VIA ADM QUEUE                                
001800*        ALLA IDDC VISAS ALLTID                                           
001900*                                                                         
002000*        -LÄGGA UPP NYA IDDC OCH DESS KDANMORS                            
002100*        -EDITERA BEFINTLIGA IDDC                                         
002200*        -TA BORT GAMLA IDDC                                              
002300*                                                                         
002400*        PROGRAMMET UPPDATERAR WDR2  HÄNDELSEBAS                          
002500*                        LÄSER WDR2                                       
002600*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W4T754                                              
003000*                     W4T754U                                             
003100*        MID:         W4I75401                                            
003200*                                                                         
003300*    UTDATA.                                                              
003400*        MOD:         W4O75401                                            
003500                                                                          
003600                                                                          
003700 ENVIRONMENT DIVISION.                                                    
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'W4075400'.            
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005000 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005100                                                                          
005200*    --- GENERELLA ARBETSFÄLT                                             
005300 01  DAGENS-DATUM                PIC 9(6).                                
005400 01  DAGENS-KLOCKAN              PIC 9(8).                                
005500                                                                          
005600 01  WS-TISTADAT                 PIC 9(6)    VALUE ZERO.                  
005700                                                                          
005800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
006100 77  INDX-ANM                    PIC S9(4)  VALUE +0    COMP SYNC.        
006200 77  MAX-INDX-ANM                PIC S9(4)  VALUE +19   COMP SYNC.        
006300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006400                                                                          
006500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006600     88  ALLT-OK                             VALUE 'J'.                   
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  KDANMORS-IFYLLD-SW          PIC X       VALUE 'N'.                   
007300     88  KDANMORS-IFYLLD                     VALUE 'J'.                   
007400                                                                          
007500 77  W-KDANMORS                  PIC X(2)    VALUE SPACE.                 
007600     88  KDANMORS-OK                         VALUE '00' THRU '99'.        
007700                                                                          
007800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007900     88  NYCKLAR-OK                          VALUE 'J'.                   
008000     88  NYCKLAR-FEL                         VALUE 'N'.                   
008100                                                                          
008200 77  RAD-VALD-SW                 PIC X       VALUE 'N'.                   
008300     88  RAD-VALD                            VALUE 'J'.                   
008400     88  RAD-EJ-VALD                         VALUE 'N'.                   
008500                                                                          
008600 77  BYTE-SW                     PIC X       VALUE 'N'.                   
008700     88  BYTE-OK                             VALUE 'J'.                   
008800                                                                          
008900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009000     88  EGEN-MID                            VALUE '4754'.                
009100     88  GODK-MID                            VALUE '4751' '4752'          
009200                                                   '4753' '4754'          
009300                                                   '4755' '4756'          
009400                                                   '4757' '4758'          
009500                                                   '4759'.                
009600     88  HELP-MID                            VALUE '0551'.                
009700                                                                          
009800 01  WS-KDANMORS                 PIC XX      VALUE  SPACE.                
009900                                                                          
010000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010100 01  GENERELLA-SUBPROGRAM.                                                
010200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800                                                                          
012800                                                                          
012900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013000*01 -COPY WMEDAREA                                                        
013100                                                                          
013200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013300*   -COPY WDATAREA                                                        
013400                                                                          
013500                                                                          
013600 01  MESSAGE-CODES.                                                       
013700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014400     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
014500     03  ERR-INVALID-IDDC        PIC X(3)    VALUE '440'.                 
014600                                                                          
014700 01  MESSAGE-TEXT-A01.                                                    
014800     03  INF-MATRIX-EMPTY        PIC X(40)   VALUE                        
014900        'MATRIX EMPTY'.                                                   
015000     03  INF-RULE-MISSING        PIC X(40)   VALUE                        
015100        'RULE MISSING'.                                                   
015200     03  INF-RULE-EXISTS         PIC X(40)   VALUE                        
015300        'RULE EXISTS'.                                                    
015400     03  INF-DISCR-MISSING       PIC X(40)   VALUE                        
015500        'DISCREPANCY CODE NOT GIVEN'.                                     
015600                                                                          
015700                                                                          
015800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016100                                                                          
016200*01 -COPY WMSGINIT                                                        
016800                                                                          
016900 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
017000                                                                          
017100*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
017200*                                                                         
017300 01  SPAR-AREA.                                                           
017400     03  SPAR-IDTRANS              PIC X(4)    VALUE '4754'.              
017500     03  SPAR-IDDC-ENTER           PIC X(2)    VALUE ZERO.                
017600     03  SPAR-IDDC-NEXT            PIC X(2)    VALUE ZERO.                
017700                                                                          
017800                                                                          
017900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018000*                                                                         
018100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018200*01  MID -COPY W4I75401                                                   
018300                                                                          
018400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018500                                                                          
018600*01  -COPY WMSGAREA                                                       
018700     03  MOD REDEFINES MSG-AREA.                                          
018800*      05  -COPY W4O75401                                                 
018900                                                                          
019000                                                                          
019100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019200                                                                          
019300*01  -COPY WMFSAREA                                                       
019400                                                                          
019500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019600*                                                                         
019700                                                                          
019800                                                                          
019900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020000                                                                          
020100 01  NYCKLAR-TILL-DLI.                                                    
020200*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
020300                                                                          
020720     03 W-IDDC-B6-X.                                                      
020730       05 W-IDDC-B6              PIC X(2).                                
020800                                                                          
020900     03  W-WDGXKEY-4129-X.                                                
021000         05  W-IDHTYP-4129       PIC  X(4)   VALUE '4129'.                
021100         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
021200                                                                          
021300     03  W-WDGXKEY-4130-X.                                                
021400         05  W-4130-IDDC         PIC X(2)    VALUE SPACE.                 
021410                                                                          
021420     03  W-WDGXKEY-4132-X.                                                
021430         05  W-4132-KDANMORS     PIC X(2)    VALUE SPACE.                 
021500                                                                          
021600                                                                          
021700*    --- STATUS-KOD FRÅN IMS                                              
021800 01  STATUS-WS                   PIC XX.                                  
021900     88  SEGMENT-FINNS                       VALUE '  '.                  
022000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022200     88  BAS-SLUT                            VALUE 'GB'.                  
022300                                                                          
022400 01  GODK-STATUSKODER.                                                    
022500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022600                                                                          
022700 01  ALL-SSA.                                                             
022800     03 SSA1                     PIC X(128).                              
022900     03 SSA2                     PIC X(128).                              
022910     03 SSA3                     PIC X(128).                              
023000                                                                          
023100                                                                          
023200*    --- IMS FUNKTIONSKODER                                               
023300*01  -COPY W0003                                                          
023400     EJECT                                                                
023500*    ---  DLI INPUT-OUTPUT AREA                                           
023600                                                                          
023700                                                                          
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR201'.                      
023900 01  DLI-IO-WDR201.                                                       
024000*    03  -COPY WDGX01                                                     
024100                                                                          
024200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4130'.                    
024300 01  DLI-IO-WDGX4130.                                                     
024400 *    03  -COPY WDGX4130.                                                 
024410                                                                          
024420 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4132'.                    
024430 01  DLI-IO-WDGX4132.                                                     
024440 *    03  -COPY WDGX4132.                                                 
024500                                                                          
024520 01  FILLER         PIC X(16)   VALUE 'WDB601 AREA'.                      
024530 01   DLI-IO-AREA-B6.                                                     
024540*     03  -COPY WDB601                                                    
024600                                                                          
024700 LINKAGE SECTION.                                                         
024800*01  -COPY W0009   -PRE MSG-                                              
024900*01  -COPY W0008   -PRE USEA-                                             
025000     05  FILLER                  PIC X.                                   
025100                                                                          
025200*01  -COPY W0008   -PRE 4129-                                             
025300     05  FILLER                  PIC X.                                   
025310     EJECT                                                                
025320*01  -COPY W0008   -PRE WDB6-                                             
025330        05 FILLER                PIC X.                                   
025400                                                                          
025500                                                                          
025600                                                                          
025700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
025800                                   4129-PCB                               
025810                                   WDB6-PCB.                              
025900                                                                          
026000 MAIN SECTION.                                                            
026100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
026200                                   4129-PCB                               
026210                                   WDB6-PCB.                              
026300                                                                          
026400                                                                          
026500     PERFORM IMS-GET-MSG                                                  
026600     IF SEGMENT-FINNS                                                     
026700        PERFORM A-INIT                                                    
027000        PERFORM B-KOLLA-NYCKLAR                                           
027100        IF NYCKLAR-OK                                                     
027200           IF MFS-UPDATE                                                  
027300             PERFORM G-KOLLA-INPUT                                        
027400                                                                          
027500              IF INDATA-OK                                                
027600                 PERFORM H-UPPDATERA                                      
027700              END-IF                                                      
027800           ELSE                                                           
027900              IF MFS-FIRST                                                
028000                 PERFORM C-FOERSTA-SIDA                                   
028100              ELSE                                                        
028200                 IF MFS-NEXT                                              
028300                    PERFORM D-NAESTA-SIDA                                 
028400                 ELSE                                                     
028500                    PERFORM E-SAMMA-SIDA                                  
028600                 END-IF                                                   
028700              END-IF                                                      
028800           END-IF                                                         
028900                                                                          
029000           IF ALLT-OK                                                     
029100              PERFORM F-LAES-VISA-WDXX                                    
029200           END-IF                                                         
029300        END-IF                                                            
029400        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O75401 + 4                     
029500        PERFORM IMS-INSERT-MSG                                            
029600     END-IF                                                               
029700                                                                          
029800     MOVE ZERO TO RETURN-CODE                                             
029900     GOBACK                                                               
030000     .                                                                    
030100                                                                          
030200                                                                          
030300 A-INIT SECTION.                                                          
030400     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
030500                                                                          
030600     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
030700     MOVE FUNCTION CURRENT-DATE(9:8) TO DAGENS-KLOCKAN                    
030800                                                                          
030900     IF MSG-DUBBLA-TRANSKODER                                             
031000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I75401                 
031100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031300     ELSE                                                                 
031400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I75401                  
031500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031700     END-IF                                                               
031800                                                                          
031900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032200                                                                          
032300     MOVE LOW-VALUE TO MSG-AREA                                           
032400     MOVE 'W4O754N1' TO MFS-IDMOD                                         
032500     MOVE '4754' TO MOD-IDTRANS                                           
032600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
032700                                                                          
032800                                                                          
032900     IF EGEN-MID OR HELP-MID                                              
033000       CONTINUE                                                           
033100     ELSE                                                                 
033200       MOVE SPACE TO MFS-KDTRTYP                                          
033300       MOVE '7' TO MFS-IDPFK                                              
033400     END-IF                                                               
033500     .                                                                    
033600                                                                          
033700                                                                          
033800 B-KOLLA-NYCKLAR SECTION.                                                 
033900     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
034000                                                                          
034100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034200     MOVE '001'             TO MSGI-KDCALL                                
034300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
034400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034500     MOVE '4754'            TO MSGI-IDTRANS                               
034600                                                                          
035400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035500     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
035600                                                                          
035700     IF MSGI-IDLAND-SPR = 'GB'                                            
035800       MOVE 'GB' TO MED-IDSKYLT                                           
035900     ELSE                                                                 
036000       MOVE 'S' TO MED-IDSKYLT                                            
036100     END-IF                                                               
036200                                                                          
036300     MOVE JA TO ALLT-SW                                                   
036400     MOVE JA TO NYCKLAR-SW                                                
036500                                                                          
038400                                                                          
038500     IF NYCKLAR-FEL                                                       
038600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
038700       CALL WMEDKONV USING MED-WMEDAREA                                   
038800       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
038900       PERFORM MFS-RENSA-FAELT-IN                                         
039000       PERFORM MFS-RENSA-FAELT-UT                                         
039100     END-IF                                                               
039200     .                                                                    
039300                                                                          
039400                                                                          
039500 C-FOERSTA-SIDA SECTION.                                                  
039600     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
039700                                                                          
039800     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
039900     CALL WMEDKONV USING MED-WMEDAREA                                     
040000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
040100                                                                          
040200     PERFORM MFS-RENSA-FAELT-IN                                           
040300     .                                                                    
040400                                                                          
040500                                                                          
040600 D-NAESTA-SIDA SECTION.                                                   
040700     MOVE 'D-NAESTA-SIDA   ' TO CURRENT-SECTION                           
040800                                                                          
040820                                                                          
040900     IF SPAR-IDTRANS = '4754'                                             
041000       MOVE SPAR-IDDC-NEXT TO W-IDDC-B6                                   
041010                              W-4130-IDDC                                 
041100     ELSE                                                                 
041200       PERFORM MFS-RENSA-FAELT-IN                                         
041300     END-IF                                                               
041301                                                                          
041400     .                                                                    
041500                                                                          
041600                                                                          
041700 E-SAMMA-SIDA SECTION.                                                    
041800     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
041900                                                                          
042000     MOVE JA  TO INDATA-SW                                                
042100     MOVE NEJ TO RAD-VALD-SW                                              
042200                                                                          
042300     MOVE 1 TO INDX                                                       
042400     PERFORM UNTIL INDX > MAX-INDX                                        
042500        IF MID-KDCMD (INDX) NOT = '+' AND                                 
042600           MID-KDCMD (INDX) NOT = ' '                                     
042700           IF MID-KDCMD (INDX) NOT = 'C' OR                               
042800              RAD-VALD                                                    
042900              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)          
043000              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
043100              CALL WMEDKONV USING MED-WMEDAREA                            
043200              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
043300              MOVE NEJ TO INDATA-SW                                       
043400              PERFORM MFS-ROER-EJ-FAELT-UT                                
043500              PERFORM MFS-ROER-EJ-FAELT-IN                                
043600              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
043700              PERFORM MFS-LAES-IN-IGEN-E                                  
043800           ELSE                                                           
043900              PERFORM EA-FLYTTA-VALD-RAD                                  
044000              MOVE JA  TO RAD-VALD-SW                                     
044100           END-IF                                                         
044200        END-IF                                                            
044300        ADD 1 TO INDX                                                     
044400     END-PERFORM                                                          
044500                                                                          
044600     IF INDATA-OK AND                                                     
044700        SPAR-IDTRANS = '4754' OR '0551'                                   
044800                                                                          
044901       MOVE SPAR-IDDC-ENTER TO W-4130-IDDC                                
044910                                                                          
045100       IF MID-KDCMD-E      = ALL '+' OR                                   
045110         MID-KDCMD-E       = SPACE                                        
045210         PERFORM MFS-RENSA-FAELT-RAD-E-IN                                 
045300       ELSE                                                               
045400         IF RAD-VALD                                                      
045500            PERFORM MFS-LAES-IN-IGEN-E                                    
045600            MOVE 1 TO INDX                                                
045700            PERFORM UNTIL INDX > MAX-INDX                                 
045800               MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR (INDX)             
045900               ADD +1 TO INDX                                             
046000            END-PERFORM                                                   
046300            CONTINUE                                                      
046400         ELSE                                                             
046500            PERFORM MFS-ROER-EJ-FAELT-UT                                  
046600            PERFORM MFS-ROER-EJ-FAELT-IN                                  
046700            PERFORM MFS-ROER-EJ-FAELT-IN-E                                
046800            PERFORM MFS-LAES-IN-IGEN-E                                    
046900         END-IF                                                           
047000         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
047100         CALL WMEDKONV USING MED-WMEDAREA                                 
047200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
047300       END-IF                                                             
047400     END-IF                                                               
047500     .                                                                    
047600                                                                          
047700                                                                          
047800 EA-FLYTTA-VALD-RAD SECTION.                                              
047900     MOVE 'EA-FLYTTA-VALD  ' TO CURRENT-SECTION                           
048000                                                                          
048100     MOVE MFS-RENSA-FAELT     TO MOD-KDCMD (INDX)                         
048200     MOVE MID-IDDC (INDX)     TO W-IDDC-B6                                
048300                                 W-4130-IDDC                              
048400                                                                          
048500     PERFORM IMS-GU-WDGX4130                                              
048600     MOVE 'C'            TO MOD-KDCMD-E                                   
048700                            MID-KDCMD-E                                   
048800     MOVE 4130-IDDC      TO MOD-IDDC-E                                    
048900                            MID-IDDC-E                                    
048910                            W-4130-IDDC                                   
049000     MOVE +1             TO INDX-ANM                                      
049100                                                                          
049110     PERFORM IMS-GNP-WDGX4132                                             
049120                                                                          
049200     PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT OR                          
049210       INDX-ANM > MAX-INDX-ANM                                            
049400       MOVE 4132-KDANMORS TO  MOD-KDANMORS-E (INDX-ANM)                   
049600       ADD +1               TO INDX-ANM                                   
049610       PERFORM IMS-GNP-WDGX4132                                           
049700     END-PERFORM                                                          
049800                                                                          
049820     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
049830       MOVE MFS-RENSA-FAELT  TO MOD-KDANMORS-E (INDX-ANM)                 
049850       ADD +1                TO INDX-ANM                                  
049870     END-PERFORM                                                          
049880                                                                          
050200     .                                                                    
050300                                                                          
050400                                                                          
050500 F-LAES-VISA-WDXX SECTION.                                                
050600     MOVE 'F-LAES-VISA-WDXX' TO CURRENT-SECTION                           
050700                                                                          
050800     PERFORM IMS-GU-WDGX4129                                              
050900     PERFORM IMS-GNP-WDGX4130                                             
051000                                                                          
051100     IF SEGMENT-SAKNAS                                                    
051200        MOVE INF-MATRIX-EMPTY TO MOD-TEMFSFEL                             
051300        PERFORM MFS-RENSA-FAELT-UT                                        
051400     END-IF                                                               
051500                                                                          
051510                                                                          
051600     MOVE 4130-IDDC     TO SPAR-IDDC-ENTER                                
051700     MOVE +1 TO INDX                                                      
051800     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                      
051900     OR BAS-SLUT                                                          
052000       IF SEGMENT-FINNS                                                   
052100         IF INDATA-FEL AND MID-KDCMD (INDX) NOT = '+'                     
052200           MOVE MID-KDCMD (INDX) TO MOD-KDCMD (INDX)                      
052300         END-IF                                                           
052400         MOVE 4130-IDDC         TO MOD-IDDC      (INDX)                   
052410                                   W-4130-IDDC                            
052500         MOVE 4130-IDUSER       TO MOD-IDUSER    (INDX)                   
052600         MOVE 4130-TIUPPDAT     TO MOD-TIUPPDAT  (INDX)                   
052700                                                                          
052800         PERFORM FA-LAES-VISA-KDANMORS                                    
052900       ELSE                                                               
053000         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
053100         MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR (INDX)                   
053200       END-IF                                                             
053300       ADD +1 TO INDX                                                     
053400                                                                          
053500       PERFORM IMS-GNP-WDGX4130                                           
053600                                                                          
053700     END-PERFORM                                                          
053800     PERFORM UNTIL INDX > MAX-INDX                                        
053900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
054000       MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR (INDX)                     
054100       ADD +1 TO INDX                                                     
054200     END-PERFORM                                                          
054400                                                                          
054500     IF SEGMENT-FINNS                                                     
054600       MOVE 4130-IDDC            TO SPAR-IDDC-NEXT                        
054700       MOVE '002'     TO MSGI-KDCALL                                      
054800       MOVE '4754'    TO SPAR-IDTRANS                                     
054900       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
055000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
055100       IF NOT MFS-UPDATE                                                  
055200         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
055300         CALL WMEDKONV USING MED-WMEDAREA                                 
055400         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
055500       END-IF                                                             
055600     END-IF                                                               
055700                                                                          
055800     .                                                                    
055900                                                                          
056000                                                                          
056100 FA-LAES-VISA-KDANMORS SECTION.                                           
056200     MOVE 'FA-LAES-VISA-KDANMORS' TO CURRENT-SECTION                      
056300                                                                          
056400     PERFORM IMS-GNP-WDGX4132                                             
056500                                                                          
056600     MOVE +1             TO INDX-ANM                                      
056700     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM OR                             
056800       SEGMENT-SAKNAS OR BAS-SLUT                                         
056900       IF SEGMENT-FINNS                                                   
057000         MOVE 4132-KDANMORS TO                                            
057100                         MOD-KDANMORS (INDX, INDX-ANM)                    
057200       ELSE                                                               
057300         MOVE MFS-RENSA-FAELT TO                                          
057400                         MOD-KDANMORS (INDX, INDX-ANM)                    
057500       END-IF                                                             
057600       ADD +1               TO INDX-ANM                                   
057700                                                                          
057800       PERFORM IMS-GNP-WDGX4132                                           
057900                                                                          
058000     END-PERFORM                                                          
058100     .                                                                    
058200                                                                          
058300                                                                          
058400 G-KOLLA-INPUT SECTION.                                                   
058500     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
058700                                                                          
058800     MOVE JA  TO INDATA-SW                                                
058900     MOVE JA  TO ALLT-SW                                                  
059000     MOVE NEJ TO RAD-VALD-SW                                              
059100                                                                          
059200     MOVE 1 TO INDX                                                       
059300     PERFORM UNTIL INDX > MAX-INDX                                        
059600       IF MID-KDCMD(INDX) NOT = '+' AND                                   
059700          MID-KDCMD(INDX) NOT = ' '                                       
059800         IF MID-KDCMD(INDX) = 'D'                                         
059900                                                                          
060000*   ---  BORTTAG AV FLERA RADER SAMTIDIGT SKA VARA OK                     
060400           MOVE JA                      TO RAD-VALD-SW                    
060500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)             
060600           PERFORM GA-KOLLA-BORTTAG                                       
061200         END-IF                                                           
061300       END-IF                                                             
061400       ADD 1 TO INDX                                                      
061500     END-PERFORM                                                          
061600                                                                          
061700     IF INDATA-OK                                                         
061800       IF (MID-KDCMD-E = ALL '+' OR MID-KDCMD-E = SPACE)                  
061900        AND (MID-IDDC-E  = ALL '+' OR SPACE)                              
062100                                                                          
062200         IF RAD-EJ-VALD                                                   
062400           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
062500           CALL WMEDKONV USING            MED-WMEDAREA                    
062600           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
062700           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
062800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
062900           MOVE NEJ                    TO INDATA-SW                       
063000         END-IF                                                           
063100       ELSE                                                               
063200                                                                          
063400         IF MID-KDCMD-E = 'N' OR MID-KDCMD-E = 'C'                        
063600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-E-ATTR                  
063700         ELSE                                                             
063900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
064000           PERFORM GC-FELHANTERING                                        
064100           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-E-ATTR               
064200           MOVE NEJ                     TO ALLT-SW                        
064300         END-IF                                                           
064400                                                                          
064500*---KOLLA IDDC                                                            
064600         IF MID-IDDC-E = SPACE OR MID-IDDC-E = ALL '+'                    
064800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
064900           PERFORM GC-FELHANTERING                                        
065000           MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-E-ATTR                
065100           MOVE NEJ                     TO ALLT-SW                        
065200         ELSE                                                             
065500           MOVE MID-IDDC-E           TO W-IDDC-B6                         
065510           PERFORM IMS-GU-WDB6                                            
065520           IF SEGMENT-FINNS                                               
065800             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-E-ATTR                 
065900           ELSE                                                           
066100             MOVE ERR-INVALID-IDDC     TO MED-IDMFSFEL                    
066200             PERFORM GC-FELHANTERING                                      
066300             MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDDC-E-ATTR                
066400             MOVE NEJ                   TO ALLT-SW                        
066500           END-IF                                                         
066600         END-IF                                                           
066700                                                                          
066800*---KOLLA ALLA KDANMORS                                                   
066900         PERFORM GB-KOLLA-KDANMORS                                        
067000                                                                          
067100*---KOLLA SÅ ATT INTE SEGMENT REDAN FINNS PÅ BAS OM NY                    
067200         IF INDATA-OK                                                     
067400           IF MID-KDCMD-E = 'N'                                           
067600             MOVE MID-IDDC-E TO W-IDDC-B6                                 
067700                                W-4130-IDDC                               
067800                                                                          
067900             PERFORM IMS-GU-WDGX4130                                      
068200                                                                          
068300             IF SEGMENT-FINNS                                             
068400               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-E-ATTR               
068500               MOVE NEJ                  TO INDATA-SW                     
068600               MOVE NEJ                  TO ALLT-SW                       
068700               MOVE INF-RULE-EXISTS      TO MOD-TEMFSFEL                  
068800               PERFORM MFS-ROER-EJ-FAELT-UT                               
068900               PERFORM MFS-ROER-EJ-FAELT-IN                               
069000               PERFORM MFS-ROER-EJ-FAELT-IN-E                             
069100             ELSE                                                         
069200               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-E-ATTR               
069300             END-IF                                                       
069400           ELSE                                                           
069500                                                                          
069600*---KOLLA SÅ SEGMENT FINNS PÅ BAS VID ÄNDRING                             
069700             MOVE MID-IDDC-E TO W-IDDC-B6                                 
069800                                W-4130-IDDC                               
069900                                                                          
070000             PERFORM IMS-GU-WDGX4130                                      
070300                                                                          
070400             IF SEGMENT-SAKNAS                                            
070500               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-E-ATTR               
070600               MOVE NEJ                  TO INDATA-SW                     
070700               MOVE NEJ                  TO ALLT-SW                       
070800               MOVE INF-RULE-MISSING     TO MOD-TEMFSFEL                  
070900               PERFORM MFS-ROER-EJ-FAELT-UT                               
071000               PERFORM MFS-ROER-EJ-FAELT-IN                               
071100               PERFORM MFS-ROER-EJ-FAELT-IN-E                             
071200             ELSE                                                         
071300               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-E-ATTR               
071400             END-IF                                                       
071500           END-IF                                                         
071600                                                                          
071700         END-IF                                                           
071800                                                                          
071900         IF INDATA-FEL                                                    
072000           IF MOD-TEMFSFEL (1:2) = MFS-RENSA-FAELT                        
072100              CALL WMEDKONV USING MED-WMEDAREA                            
072200              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
072300           END-IF                                                         
072400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
072500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
072600           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
072700           MOVE NEJ         TO ALLT-SW                                    
072800         END-IF                                                           
072900       END-IF                                                             
073000     END-IF                                                               
073100     .                                                                    
073200                                                                          
073300                                                                          
073400                                                                          
073500 GA-KOLLA-BORTTAG      SECTION.                                           
073600     MOVE 'GA-KOLLA-BORTTAG '  TO CURRENT-SECTION                         
073700                                                                          
073800*---KOLLA SÅ SEGMENT FINNS PÅ BAS VID BORTTAG                             
073900     MOVE MID-IDDC (INDX)          TO W-IDDC-B6                           
074000                              W-4130-IDDC                                 
074100                                                                          
074200     PERFORM IMS-GU-WDGX4130                                              
074500                                                                          
074600     IF SEGMENT-SAKNAS                                                    
074700       MOVE MFS-ALFA-FAELT-FEL           TO MOD-KDCMD-ATTR (INDX)         
074800       MOVE NEJ                          TO INDATA-SW                     
074900       MOVE INF-RULE-MISSING             TO MOD-TEMFSFEL                  
075000     ELSE                                                                 
075100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)                 
075200     END-IF                                                               
075300     .                                                                    
075400                                                                          
075500                                                                          
075600                                                                          
075700 GB-KOLLA-KDANMORS     SECTION.                                           
075800     MOVE 'GB-KOLLA-KDANMORS'  TO CURRENT-SECTION                         
075900                                                                          
076000     MOVE NEJ            TO KDANMORS-IFYLLD-SW                            
076100     MOVE +1             TO INDX-ANM                                      
076200                                                                          
076300     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
076400       IF MID-KDANMORS-E (INDX-ANM) = SPACE OR                            
076500         MID-KDANMORS-E (INDX-ANM) = ALL '+'                              
076600         CONTINUE                                                         
076700       ELSE                                                               
076800         MOVE JA             TO KDANMORS-IFYLLD-SW                        
076900         IF MID-KDANMORS-E  (INDX-ANM) NOT NUMERIC                        
077000           MOVE MFS-ALFA-FAELT-FEL                                        
077100                TO MOD-KDANMORS-E-ATTR (INDX-ANM)                         
077200           MOVE NEJ                      TO INDATA-SW                     
077300           MOVE ERR-INVALID-VALUE        TO MED-IDMFSFEL                  
077400           MOVE MFS-ALFA-FAELT-FEL                                        
077500                             TO MOD-KDANMORS-E-ATTR (INDX-ANM)            
077600           MOVE NEJ                  TO INDATA-SW                         
077700           MOVE NEJ                  TO ALLT-SW                           
077800                                                                          
077900         ELSE                                                             
078000           MOVE MID-KDANMORS-E (INDX-ANM) TO W-KDANMORS                   
078100           IF KDANMORS-OK                                                 
078200             MOVE MFS-ALFA-FAELT-RAETT                                    
078300                                 TO MOD-KDANMORS-E-ATTR (INDX-ANM)        
078400           ELSE                                                           
078500             MOVE MFS-ALFA-FAELT-FEL                                      
078600                  TO MOD-KDANMORS-E-ATTR (INDX-ANM)                       
078700             MOVE NEJ                    TO INDATA-SW                     
078800             MOVE ERR-INVALID-VALUE      TO MED-IDMFSFEL                  
078900             MOVE MFS-ALFA-FAELT-FEL                                      
079000                               TO MOD-KDANMORS-E-ATTR (INDX-ANM)          
079100             MOVE NEJ                TO INDATA-SW                         
079200             MOVE NEJ                TO ALLT-SW                           
079300                                                                          
079400           END-IF                                                         
079500         END-IF                                                           
079600       END-IF                                                             
079700       ADD +1               TO INDX-ANM                                   
079800     END-PERFORM                                                          
079900                                                                          
080000*--- MINST EN LEV.ANM KOD KRÄVS FÖR EN REGEL                              
080100     IF KDANMORS-IFYLLD                                                   
080200       CONTINUE                                                           
080300     ELSE                                                                 
080400       MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDANMORS-E-ATTR (1)         
080500       MOVE NEJ                        TO INDATA-SW                       
080600       MOVE INF-DISCR-MISSING          TO MOD-TEMFSFEL                    
080700     END-IF                                                               
080800     .                                                                    
080900                                                                          
081000                                                                          
081100 GC-FELHANTERING SECTION.                                                 
081200     MOVE 'GC-FELHANTERING ' TO CURRENT-SECTION                           
081300                                                                          
081400     CALL WMEDKONV USING MED-WMEDAREA                                     
081500     MOVE MED-MFSFEL                    TO MOD-TEMFSFEL                   
081600     PERFORM MFS-ROER-EJ-FAELT-UT                                         
081700     PERFORM MFS-ROER-EJ-FAELT-IN                                         
081800     PERFORM MFS-ROER-EJ-FAELT-IN-E                                       
081900     MOVE NEJ TO INDATA-SW                                                
082000                                                                          
082100                                                                          
082200     .                                                                    
082300                                                                          
082400                                                                          
082500 H-UPPDATERA SECTION.                                                     
082600     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
082700                                                                          
082800     MOVE 1 TO INDX                                                       
082900     PERFORM UNTIL INDX > MAX-INDX                                        
083000        IF MID-KDCMD (INDX) = 'D'                                         
083100           MOVE MID-IDDC (INDX) TO W-IDDC-B6                              
083200                                   W-4130-IDDC                            
083300           PERFORM IMS-GHU-WDGX4130                                       
083400           IF SEGMENT-FINNS                                               
083500              PERFORM IMS-DLET-WDGX4130                                   
083600              CONTINUE                                                    
083700           END-IF                                                         
083800        END-IF                                                            
083900        ADD 1 TO INDX                                                     
084000     END-PERFORM                                                          
084100                                                                          
084200     IF MID-KDCMD-E = 'C'                                                 
084300        MOVE MID-IDDC-E     TO W-IDDC-B6                                  
084400                               W-4130-IDDC                                
084500        PERFORM IMS-GHU-WDGX4130                                          
084510        IF SEGMENT-FINNS                                                  
084520          PERFORM IMS-DLET-WDGX4130                                       
084530        END-IF                                                            
084540                                                                          
084620        MOVE MID-IDDC-E     TO 4130-IDDC                                  
084700        MOVE DAGENS-DATUM   TO 4130-TIUPPDAT                              
084800        MOVE MSGI-IDUSER    TO 4130-IDUSER                                
084801        PERFORM IMS-ISRT-WDGX4130                                         
084802                                                                          
084810        PERFORM HA-INSERTA-KDANMORS                                       
084900                                                                          
085100     ELSE                                                                 
085200        IF MID-KDCMD-E = 'N'                                              
085300           MOVE MID-IDDC-E   TO W-IDDC-B6                                 
085400                                4130-IDDC                                 
085500           MOVE DAGENS-DATUM TO 4130-TIUPPDAT                             
085600           MOVE MSGI-IDUSER  TO 4130-IDUSER                               
085900           PERFORM IMS-ISRT-WDGX4130                                      
085910                                                                          
085920           PERFORM HA-INSERTA-KDANMORS                                    
086000        END-IF                                                            
086100     END-IF                                                               
086200                                                                          
086300     MOVE INF-UPDATE-DONE    TO MED-IDMFSINF                              
086400     CALL WMEDKONV USING        MED-WMEDAREA                              
086500     MOVE MED-MFSINF         TO MOD-TEMFSINF                              
086600                                                                          
086700     PERFORM MFS-FORM-ATTR                                                
086800     PERFORM MFS-RENSA-FAELT-IN                                           
086900     .                                                                    
087000                                                                          
087100                                                                          
087200 HA-INSERTA-KDANMORS SECTION.                                             
087300*--  LÄGG UPP ALLA LEV.ANM KODER                                          
087400                                                                          
087600     MOVE 1 TO INDX-ANM                                                   
087700                                                                          
087800     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
087900       IF MID-KDANMORS-E (INDX-ANM) = SPACE OR                            
087910          MID-KDANMORS-E (INDX-ANM) = ALL '+'                             
087920         CONTINUE                                                         
087930       ELSE                                                               
088000         MOVE MID-KDANMORS-E (INDX-ANM)  TO 4132-KDANMORS                 
088100         PERFORM IMS-ISRT-WDGX4132                                        
088500       END-IF                                                             
088600       ADD 1 TO INDX-ANM                                                  
088700     END-PERFORM                                                          
088800                                                                          
090600     .                                                                    
090700                                                                          
092720                                                                          
092730 MFS-RENSA-FAELT-UT SECTION.                                              
092740                                                                          
092750*    --- ALLA UTDATA-FÄLT                                                 
092760*    --- INKL. BLÄDDRINGSNYCKLAR                                          
092770     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-E                                  
092780                             MOD-IDDC-E                                   
092790                                                                          
092791     MOVE +1 TO INDX-ANM                                                  
092792     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
092793       MOVE MFS-RENSA-FAELT TO MOD-KDANMORS-E (INDX-ANM)                  
092794       ADD +1 TO INDX-ANM                                                 
092795     END-PERFORM                                                          
092796                                                                          
092797     MOVE +1 TO INDX                                                      
092798     PERFORM UNTIL INDX > MAX-INDX                                        
092799       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
092800       ADD +1 TO INDX                                                     
092801     END-PERFORM                                                          
092802     .                                                                    
092810                                                                          
092900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
093000                                                                          
093100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
093200     MOVE MFS-RENSA-FAELT TO MOD-KDCMD     (INDX)                         
093300                             MOD-IDDC      (INDX)                         
093400                             MOD-TIUPPDAT  (INDX)                         
093500                             MOD-IDUSER    (INDX)                         
093600                                                                          
093700     MOVE +1 TO INDX-ANM                                                  
093800     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
093900       MOVE MFS-RENSA-FAELT TO MOD-KDANMORS (INDX, INDX-ANM)              
094000       ADD +1 TO INDX-ANM                                                 
094100     END-PERFORM                                                          
094200                                                                          
094300     .                                                                    
094310                                                                          
094320                                                                          
094330 MFS-RENSA-FAELT-RAD-E-IN SECTION.                                        
094340                                                                          
094350*    --- INDATA-FÄLT PÅ INMATNINGSRAD                                     
094360                                                                          
094370     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-E                                  
094380                             MOD-IDDC-E                                   
094390                                                                          
094391     MOVE +1 TO INDX-ANM                                                  
094392     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
094393       MOVE MFS-RENSA-FAELT TO MOD-KDANMORS-E (INDX-ANM)                  
094394       ADD +1 TO INDX-ANM                                                 
094395     END-PERFORM                                                          
094396                                                                          
094397     .                                                                    
094398                                                                          
094400                                                                          
094500 MFS-RENSA-FAELT-IN SECTION.                                              
094600                                                                          
094700*    --- ALLA INDATA-FÄLT                                                 
094900     MOVE +1 TO INDX                                                      
095000     PERFORM UNTIL INDX > MAX-INDX                                        
095100       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
095200       ADD +1 TO INDX                                                     
095300     END-PERFORM                                                          
095400     .                                                                    
095500                                                                          
095600 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
095700                                                                          
095800     MOVE MFS-RENSA-FAELT TO MOD-KDCMD     (INDX)                         
095900                             MOD-IDDC      (INDX)                         
096000                             MOD-TIUPPDAT  (INDX)                         
096100                             MOD-IDUSER    (INDX)                         
096200                                                                          
096300     MOVE +1 TO INDX-ANM                                                  
096400     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
096500       MOVE MFS-RENSA-FAELT TO MOD-KDANMORS (INDX, INDX-ANM)              
096600       ADD +1 TO INDX-ANM                                                 
096700     END-PERFORM                                                          
096800     .                                                                    
096900                                                                          
097000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
097100                                                                          
097200*    --- ALLA UTDATA-FÄLT                                                 
097300*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
097400     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-E                                
097500                                                                          
097600     MOVE +1 TO INDX-ANM                                                  
097700     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
097800       MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-E (INDX-ANM)                
097900       ADD +1 TO INDX-ANM                                                 
098000     END-PERFORM                                                          
098100                                                                          
098200     MOVE +1 TO INDX                                                      
098300     PERFORM UNTIL INDX > MAX-INDX                                        
098400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
098500       ADD +1 TO INDX                                                     
098600     END-PERFORM                                                          
098700     .                                                                    
098800                                                                          
098900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
099000                                                                          
099100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
099200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD     (INDX)                       
099300                               MOD-IDDC      (INDX)                       
099400                               MOD-TIUPPDAT  (INDX)                       
099500                               MOD-IDUSER    (INDX)                       
099600     MOVE +1 TO INDX-ANM                                                  
099700     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
099800       MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS (INDX, INDX-ANM)            
099900       ADD +1 TO INDX-ANM                                                 
100000     END-PERFORM                                                          
100100     .                                                                    
100200                                                                          
100300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
100400                                                                          
100500*    --- ALLA INDATA-FÄLT                                                 
100700     MOVE 1 TO INDX                                                       
100800     PERFORM UNTIL INDX > MAX-INDX                                        
100900        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD    (INDX)                     
101000                                  MOD-IDDC     (INDX)                     
101100        ADD 1 TO INDX                                                     
101200     END-PERFORM                                                          
101300     .                                                                    
101400                                                                          
101500 MFS-ROER-EJ-FAELT-IN-E SECTION.                                          
101600                                                                          
101700*    --- ALLA INDATA-FÄLT                                                 
101800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-E                                
101900                               MOD-IDDC-E                                 
102000                                                                          
102100     MOVE +1 TO INDX-ANM                                                  
102200     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
102300       MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS-E (INDX-ANM)                
102400       ADD +1 TO INDX-ANM                                                 
102500     END-PERFORM                                                          
102600     .                                                                    
102700                                                                          
102800 MFS-FORM-ATTR SECTION.                                                   
102900                                                                          
103000*    --- ALLA INDATA-FÄLT                                                 
103100     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-E-ATTR                          
103200                                MOD-IDDC-E-ATTR                           
103300                                                                          
103400     MOVE +1 TO INDX-ANM                                                  
103500     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
103600       MOVE MFS-FORMATETS-ATTR      TO MOD-KDANMORS-E (INDX-ANM)          
103700       ADD +1 TO INDX-ANM                                                 
103800     END-PERFORM                                                          
103900                                                                          
104000     MOVE 1 TO INDX                                                       
104100     PERFORM UNTIL INDX > MAX-INDX                                        
104200        MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR    (INDX)               
104300        ADD 1 TO INDX                                                     
104400     END-PERFORM                                                          
104500     .                                                                    
104600                                                                          
104700 MFS-LAES-IN-IGEN-E SECTION.                                              
104800                                                                          
104900*    --- ALLA INDATA-FÄLT                                                 
105000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-E-ATTR                       
105100                                   MOD-IDDC-E-ATTR                        
105200                                                                          
105300     MOVE +1 TO INDX-ANM                                                  
105400                                                                          
105500     PERFORM UNTIL INDX-ANM > MAX-INDX-ANM                                
105600       MOVE MFS-ADD-LAES-IN-FAELT                                         
105700            TO MOD-KDANMORS-E-ATTR (INDX-ANM)                             
105800       ADD +1 TO INDX-ANM                                                 
105900     END-PERFORM                                                          
106010     MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-KDANMORS-E-ATTR (1)                
106100     .                                                                    
106200                                                                          
106300* --- IMS SEKTIONER ---                                                   
106400                                                                          
106500 IMS-GET-MSG SECTION.                                                     
106600     MOVE 'IMS-GET-MSG     ' TO CURRENT-IMS-SECTION                       
106700                                                                          
106800     MOVE '  QC' TO GODK-STATUSKODER                                      
106900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
107000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107100     PERFORM IMS-STATUSKONTROLL                                           
107200     .                                                                    
107300                                                                          
107400 IMS-INSERT-MSG SECTION.                                                  
107500     MOVE 'IMS-INSERT-MSG  ' TO CURRENT-IMS-SECTION                       
107600                                                                          
109000                                                                          
109100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
109200     MOVE SPACE TO GODK-STATUSKODER                                       
109300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
109400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109500     PERFORM IMS-STATUSKONTROLL                                           
109600     .                                                                    
109700                                                                          
109800 IMS-GU-WDGX4129 SECTION.                                                 
109900                                                                          
109920     MOVE SPACE TO SSA1                                                   
109930                                                                          
110000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
110100            DELIMITED BY SIZE INTO SSA1                                   
110200     MOVE '  ' TO GODK-STATUSKODER                                        
110300     CALL CBLTDLI USING GU 4129-PCB DLI-IO-WDR201 SSA1                    
110400     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
110500     PERFORM IMS-STATUSKONTROLL                                           
110600     .                                                                    
110700                                                                          
110800 IMS-GNP-WDGX4130 SECTION.                                                
110900     MOVE 'IMS-GNP-WDGX4130 '    TO CURRENT-IMS-SECTION                   
111000                                                                          
111010     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
111020            DELIMITED BY SIZE INTO SSA1                                   
111030     STRING 'WDGX4130(IDDC    >=' W-WDGXKEY-4130-X ')'                    
111040            DELIMITED BY SIZE INTO SSA2                                   
111210     MOVE '  GE'                 TO GODK-STATUSKODER                      
111300                                                                          
111400     CALL CBLTDLI USING GNP 4129-PCB 4130-WDGX4130 SSA1 SSA2              
111500     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
111600     PERFORM IMS-STATUSKONTROLL                                           
111700     .                                                                    
111800                                                                          
111900 IMS-GU-WDGX4130       SECTION.                                           
112000     MOVE 'IMS-GU-WDGX4130'    TO CURRENT-IMS-SECTION                     
112100                                                                          
112200     MOVE SPACE TO SSA1                                                   
112300                   SSA2                                                   
112400                                                                          
112500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
112600         DELIMITED BY SIZE INTO SSA1                                      
112700     STRING 'WDGX4130(IDDC     =' W-WDGXKEY-4130-X ')'                    
112800         DELIMITED BY SIZE INTO SSA2                                      
112900                                                                          
113000     MOVE '  GE' TO GODK-STATUSKODER                                      
113100     CALL CBLTDLI USING GU 4129-PCB 4130-WDGX4130 SSA1 SSA2               
113200     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
113300                                                                          
113400     PERFORM IMS-STATUSKONTROLL                                           
113500     .                                                                    
113600     SKIP3                                                                
113610 IMS-GHU-WDGX4130       SECTION.                                          
113620     MOVE 'IMS-GHU-WDGX4130'    TO CURRENT-IMS-SECTION                    
113630                                                                          
113640     MOVE SPACE TO SSA1                                                   
113650                   SSA2                                                   
113660                                                                          
113670     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
113680         DELIMITED BY SIZE INTO SSA1                                      
113690     STRING 'WDGX4130(IDDC     =' W-WDGXKEY-4130-X ')'                    
113691         DELIMITED BY SIZE INTO SSA2                                      
113692                                                                          
113693     MOVE '  GE' TO GODK-STATUSKODER                                      
113694     CALL CBLTDLI USING GHU 4129-PCB 4130-WDGX4130 SSA1 SSA2              
113695     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
113696                                                                          
113697     PERFORM IMS-STATUSKONTROLL                                           
113698     .                                                                    
113699     SKIP3                                                                
113700                                                                          
114700                                                                          
114800 IMS-DLET-WDGX4130        SECTION.                                        
114900     MOVE 'IMS-DLET-WDGX4130  '    TO CURRENT-IMS-SECTION                 
115000                                                                          
115100     MOVE '    ' TO GODK-STATUSKODER                                      
115200     CALL CBLTDLI USING DLET 4129-PCB 4130-WDGX4130                       
115300     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
115400                                                                          
115500     PERFORM IMS-STATUSKONTROLL                                           
115600     .                                                                    
115700                                                                          
115800 IMS-ISRT-WDGX4130 SECTION.                                               
115900                                                                          
116000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
116100            DELIMITED BY SIZE INTO SSA1                                   
116200     MOVE 'WDGX4130 ' TO SSA2                                             
116300     MOVE '  II' TO GODK-STATUSKODER                                      
116400     CALL CBLTDLI USING ISRT 4129-PCB DLI-IO-WDGX4130 SSA1 SSA2           
116500     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
116600     PERFORM IMS-STATUSKONTROLL                                           
116700     .                                                                    
116701                                                                          
116710 IMS-GNP-WDGX4132 SECTION.                                                
116720     MOVE 'IMS-GNP-WDGX4132 '    TO CURRENT-IMS-SECTION                   
116730                                                                          
116731     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
116732            DELIMITED BY SIZE INTO SSA1                                   
116733     STRING 'WDGX4130(IDDC     =' W-WDGXKEY-4130-X ')'                    
116734         DELIMITED BY SIZE INTO SSA2                                      
116750     MOVE   'WDGX4132 '          TO SSA3                                  
116751     MOVE '  GE'                 TO GODK-STATUSKODER                      
116760                                                                          
116770     CALL CBLTDLI USING GNP 4129-PCB DLI-IO-WDGX4132                      
116771                                     SSA1 SSA2 SSA3                       
116780     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
116790     PERFORM IMS-STATUSKONTROLL                                           
116791     .                                                                    
116792                                                                          
116800                                                                          
116810 IMS-ISRT-WDGX4132 SECTION.                                               
116820                                                                          
116830     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-4129-X ')'                    
116840            DELIMITED BY SIZE INTO SSA1                                   
116841     STRING 'WDGX4130(IDDC     =' W-WDGXKEY-4130-X ')'                    
116842            DELIMITED BY SIZE INTO SSA2                                   
116850     MOVE 'WDGX4132 ' TO SSA3                                             
116860     MOVE '  II' TO GODK-STATUSKODER                                      
116870     CALL CBLTDLI USING ISRT 4129-PCB DLI-IO-WDGX4132                     
116871                                      SSA1 SSA2 SSA3                      
116880     MOVE 4129-STATUS-CODE TO STATUS-WS                                   
116890     PERFORM IMS-STATUSKONTROLL                                           
116891     .                                                                    
116892                                                                          
116893 IMS-GU-WDB6      SECTION.                                                
116894     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
116895          DELIMITED BY SIZE INTO SSA1                                     
116896     MOVE '  GE' TO GODK-STATUSKODER                                      
116897     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B6 SSA1                   
116898     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
116899     PERFORM IMS-STATUSKONTROLL                                           
116900     .                                                                    
116901                                                                          
116910 IMS-STATUSKONTROLL SECTION.                                              
117000                                                                          
117100     SET STATUS-IX TO 1                                                   
117200     SEARCH GODK-STATUS                                                   
117300       AT END                                                             
117400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
117500         DELIMITED BY SIZE INTO FELTEXT                                   
117600         CALL FELLOG                                                      
117700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
117800         CONTINUE                                                         
117900     END-SEARCH                                                           
118000     .                                                                    
