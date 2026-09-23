000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033600.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL                                          
000400 DATE-WRITTEN.   99/02/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMETS HUVUDFUNKTIONER ÄR FÖLJANDE:                         
000900*        -FRÅGA PÅ SPÄRRGRUPPER, MATA IN SPÄRRGRUPPER                     
001000*        OCH FÅ UT INFORMATION OM VILKA GRUPPER SOM FINNS                 
001100*                                                                         
001200*        -LÄGGA UPP NYA SPÄRRGRUPPER (N)                                  
001300*        -EDITERA BEFINTLIGA SPÄRRGRUPPER (C)                             
001400*        -TA BORT GAMLA SPÄRRGRUPPER (D)                                  
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WDF8                                       
001610*                        LÄSER WDF8B                                      
001700*                                                                         
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W2T336                                              
002100*                     W2T336U                                             
002200*        MID:         W2I33601                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W2O33601                                            
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W2033600'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004010 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004020 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004100                                                                          
004200*    --- GENERELLA ARBETSFÄLT                                             
004500 01  DAGENS-DATUM                PIC 9(6).                                
005100                                                                          
005200 01  WS-TISTADAT                 PIC 9(6)    VALUE ZERO.                  
005500                                                                          
005600 01  WS-KDMARKBLK                PIC 9(3)    VALUE ZERO.                  
005700                                                                          
005800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
006100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006200                                                                          
006300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006400     88  ALLT-OK                             VALUE 'J'.                   
006500                                                                          
006600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006700     88  INDATA-OK                           VALUE 'J'.                   
006800     88  INDATA-FEL                          VALUE 'N'.                   
006900                                                                          
007000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007100     88  NYCKLAR-OK                          VALUE 'J'.                   
007200     88  NYCKLAR-FEL                         VALUE 'N'.                   
007210                                                                          
007220 77  RAD-VALD-SW                 PIC X       VALUE 'N'.                   
007230     88  RAD-VALD                            VALUE 'J'.                   
007240     88  RAD-EJ-VALD                         VALUE 'N'.                   
007300                                                                          
007310 77  BYTE-SW                     PIC X       VALUE 'N'.                   
007320     88  BYTE-OK                             VALUE 'J'.                   
007340                                                                          
007400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007500     88  EGEN-MID                            VALUE '2336'.                
007600     88  GODK-MID                            VALUE '2331' '2332'          
007700                                                   '2333' '2334'          
007800                                                   '2335' '2336'          
007900                                                   '2337' '2338'          
008000                                                   '2339'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200                                                                          
008210                                                                          
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000                                                                          
009100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009200*01 -COPY WMEDAREA                                                        
009300                                                                          
009400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
009500*   -COPY WDATAREA                                                        
009600                                                                          
009700                                                                          
009800 01  MESSAGE-CODES.                                                       
009900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010600     03  SUPPLIER-MISSING        PIC X(3)    VALUE '273'.                 
010700     03  GROUP-EXISTS            PIC X(3)    VALUE '274'.                 
010800     03  GROUP-MISSING           PIC X(3)    VALUE '275'.                 
010900     03  PARTS-IN-GROUP          PIC X(3)    VALUE '276'.                 
010910                                                                          
010920 01  MESSAGE-TEXT-A01.                                                    
010930     03  AUTO-PARTS              PIC X(40)   VALUE                        
010940        'NON AUTO PARTS STILL IN THE GROUP 2337'.                         
010950 01  MESSAGE-TEXT-A02.                                                    
010960     03  AUTO-RULES              PIC X(40)   VALUE                        
010970        'AUTO RULES STILL IN THE GROUP 2337'.                             
011000                                                                          
011010                                                                          
011100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011400                                                                          
011500*01 -COPY WMSGINIT                                                        
011600                                                                          
011610                                                                          
011700 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
011800                                                                          
011900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
012000*                                                                         
012100 01  SPAR-AREA.                                                           
012200     03  SPAR-IDTRANS              PIC X(4)    VALUE '2336'.              
012300     03  SPAR-IDSPRGRP-ENTER       PIC X(10)   VALUE ZERO.                
012400     03  SPAR-IDSPRGRP-NEXT        PIC X(10)   VALUE ZERO.                
012410     03  SPAR-TISTADAT             PIC 9(06)   VALUE ZERO.                
012500                                                                          
012510                                                                          
012600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013000*01  MID -COPY W2I33601                                                   
013100                                                                          
013200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013300                                                                          
013400*01  -COPY WMSGAREA                                                       
013600     03  MOD REDEFINES MSG-AREA.                                          
013700*      05  -COPY W2O33601                                                 
013800                                                                          
013810                                                                          
013900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014000                                                                          
014100*01  -COPY WMFSAREA                                                       
014200                                                                          
014300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014400*                                                                         
014500                                                                          
014510                                                                          
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700                                                                          
014800 01  NYCKLAR-TILL-DLI.                                                    
014900*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
015000                                                                          
015100                                                                          
015200     03  W-IDSPRGRP-X.                                                    
015400         05  W-IDSPRGRP          PIC X(10)   VALUE SPACE.                 
016800                                                                          
017500                                                                          
017600*    --- STATUS-KOD FRÅN IMS                                              
017700 01  STATUS-WS                   PIC XX.                                  
017800     88  SEGMENT-FINNS                       VALUE '  '.                  
017900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018100     88  BAS-SLUT                            VALUE 'GB'.                  
018200                                                                          
018300 01  GODK-STATUSKODER.                                                    
018400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018510                                                                          
018600 01  ALL-SSA.                                                             
018610     03 SSA1                     PIC X(128).                              
018700     03 SSA2                     PIC X(128).                              
018710                                                                          
018800                                                                          
018900*    --- IMS FUNKTIONSKODER                                               
019000*01  -COPY W0003                                                          
019100     EJECT                                                                
019200*    ---  DLI INPUT-OUTPUT AREA                                           
019300                                                                          
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF801'.                      
019500 01  DLI-IO-WDF801.                                                       
019600*    03  -COPY WDF801                                                     
019700                                                                          
019701 01  FILLER         PIC X(16) VALUE 'DLI-IO-DUMMY '.                      
019702 01  DLI-IO-DUMMY.                                                        
019703*    03  -COPY WDF801  -L                                                 
019704                                                                          
019710                                                                          
020200 LINKAGE SECTION.                                                         
020300*01  -COPY W0009   -PRE MSG-                                              
020400*01  -COPY W0008   -PRE USEA-                                             
020500     05  FILLER                  PIC X.                                   
020600                                                                          
020700*01  -COPY W0008   -PRE WDF8-                                             
020800     05  FILLER                  PIC X.                                   
020900                                                                          
021000*01  -COPY W0008   -PRE WDF8B-                                            
021010     05  FILLER                  PIC X.                                   
021100                                                                          
021200                                                                          
021210                                                                          
021300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDF8-PCB WDF8B-PCB.           
021400                                                                          
021500 MAIN SECTION.                                                            
021600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDF8-PCB WDF8B-PCB.           
021700                                                                          
021800                                                                          
021900     PERFORM IMS-GET-MSG                                                  
022000     IF SEGMENT-FINNS                                                     
022100        PERFORM A-INIT                                                    
022200        PERFORM B-KOLLA-NYCKLAR                                           
022300        IF NYCKLAR-OK                                                     
022400           IF MFS-UPDATE                                                  
022500             PERFORM G-KOLLA-INPUT                                        
022510                                                                          
022600              IF INDATA-OK                                                
022700                 PERFORM H-UPPDATERA                                      
022800              END-IF                                                      
022900           ELSE                                                           
023000              IF MFS-FIRST                                                
023100                 PERFORM C-FOERSTA-SIDA                                   
023200              ELSE                                                        
023300                 IF MFS-NEXT                                              
023400                    PERFORM D-NAESTA-SIDA                                 
023500                 ELSE                                                     
023600                    PERFORM E-SAMMA-SIDA                                  
023700                 END-IF                                                   
023800              END-IF                                                      
023900           END-IF                                                         
023910                                                                          
024000           IF ALLT-OK                                                     
024100              PERFORM F-LAES-VISA-WDF8                                    
024200           END-IF                                                         
024300        END-IF                                                            
024400        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33601 + 4                     
024500        PERFORM IMS-INSERT-MSG                                            
024600     END-IF                                                               
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100                                                                          
025110                                                                          
025200 A-INIT SECTION.                                                          
025210     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
025300                                                                          
025400     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
025600                                                                          
025700     IF MSG-DUBBLA-TRANSKODER                                             
025800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33601                 
025900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
026000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026100     ELSE                                                                 
026200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I33601                  
026300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026500     END-IF                                                               
026600                                                                          
026700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027000                                                                          
027100     MOVE LOW-VALUE TO MSG-AREA                                           
027200     MOVE 'W2O336N1' TO MFS-IDMOD                                         
027300     MOVE '2336' TO MOD-IDTRANS                                           
027400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027500                                                                          
027600                                                                          
027700     IF EGEN-MID OR HELP-MID                                              
027800       CONTINUE                                                           
027900     ELSE                                                                 
028000       MOVE SPACE TO MFS-KDTRTYP                                          
028100       MOVE '7' TO MFS-IDPFK                                              
028200     END-IF                                                               
028300     .                                                                    
028400                                                                          
028410                                                                          
028500 B-KOLLA-NYCKLAR SECTION.                                                 
028510     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
028600                                                                          
028700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028800     MOVE '001'             TO MSGI-KDCALL                                
028900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029100     MOVE '2336'            TO MSGI-IDTRANS                               
029200                                                                          
029300     IF EGEN-MID                                                          
029400       IF MID-IDSPRGRP-IN NOT = ALL '+'                                   
029410          MOVE MID-IDSPRGRP-IN TO MSGI-IDDIRGRP                           
029420       ELSE                                                               
029430          MOVE MID-IDSPRGRP-UT TO MSGI-IDDIRGRP                           
029440       END-IF                                                             
029500     END-IF                                                               
029600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029700     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
029800                                                                          
029900     IF MSGI-IDLAND-SPR = 'GB'                                            
030000       MOVE 'GB' TO MED-IDSKYLT                                           
030100     ELSE                                                                 
030200       MOVE 'S' TO MED-IDSKYLT                                            
030300     END-IF                                                               
030400                                                                          
030500     MOVE JA TO ALLT-SW                                                   
030600     MOVE JA TO NYCKLAR-SW                                                
030700                                                                          
030800*    -- KONTROLL AV IDSPRGRP                                              
030900     MOVE MFS-RENSA-FAELT TO MOD-IDSPRGRP-IN                              
031000                                                                          
031200     IF MSGI-IDDIRGRP  NOT = ALL '+'                                      
031300       MOVE MSGI-IDDIRGRP   TO W-IDSPRGRP                                 
031600     ELSE                                                                 
031700       MOVE NEJ             TO NYCKLAR-SW                                 
031800     END-IF                                                               
031900                                                                          
032000     IF GODK-MID OR NYCKLAR-OK                                            
032100       MOVE MSGI-IDDIRGRP    TO MOD-IDSPRGRP-UT                           
032200     ELSE                                                                 
032300       MOVE MFS-RENSA-FAELT TO MOD-IDSPRGRP-UT                            
032400     END-IF                                                               
032500     IF MID-IDSPRGRP-IN NOT = ALL '+'                                     
032600       MOVE SPACE TO MFS-KDTRTYP                                          
032700       MOVE '7' TO MFS-IDPFK                                              
032800     END-IF                                                               
032900                                                                          
033000     IF NYCKLAR-FEL                                                       
033100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
033200       CALL WMEDKONV USING MED-WMEDAREA                                   
033300       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
033400       PERFORM MFS-RENSA-FAELT-IN                                         
033500       PERFORM MFS-RENSA-FAELT-UT                                         
033600     END-IF                                                               
033700     .                                                                    
033800                                                                          
033810                                                                          
033900 C-FOERSTA-SIDA SECTION.                                                  
033910     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
034000                                                                          
034100     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
034200     CALL WMEDKONV USING MED-WMEDAREA                                     
034300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
034400                                                                          
034500     PERFORM MFS-RENSA-FAELT-IN                                           
034600     .                                                                    
034700                                                                          
034710                                                                          
034800 D-NAESTA-SIDA SECTION.                                                   
034810     MOVE 'D-NAESTA-SIDA   ' TO CURRENT-SECTION                           
034900                                                                          
035000     IF SPAR-IDTRANS = '2336'                                             
035100       MOVE SPAR-IDSPRGRP-NEXT TO W-IDSPRGRP                              
035200     ELSE                                                                 
035300       PERFORM MFS-RENSA-FAELT-IN                                         
035400     END-IF                                                               
035500     .                                                                    
035600                                                                          
035610                                                                          
035700 E-SAMMA-SIDA SECTION.                                                    
035710     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
035720                                                                          
035721     MOVE JA  TO INDATA-SW                                                
035722     MOVE NEJ TO RAD-VALD-SW                                              
035723                                                                          
035730     MOVE 1 TO INDX                                                       
035740     PERFORM UNTIL INDX > MAX-INDX                                        
035750        IF MID-KDCMD (INDX) NOT = '+' AND                                 
035751           MID-KDCMD (INDX) NOT = ' '                                     
035760           IF MID-KDCMD (INDX) NOT = 'C' OR                               
035761              RAD-VALD                                                    
035762              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)          
035770              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
035780              CALL WMEDKONV USING MED-WMEDAREA                            
035790              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
035796              MOVE NEJ TO INDATA-SW                                       
035797              PERFORM MFS-ROER-EJ-FAELT-UT                                
035798              PERFORM MFS-ROER-EJ-FAELT-IN                                
035799              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
035800              PERFORM MFS-LAES-IN-IGEN-E                                  
035801           ELSE                                                           
035802              PERFORM EA-FLYTTA-VALD-RAD                                  
035803              MOVE JA  TO RAD-VALD-SW                                     
035804           END-IF                                                         
035805        END-IF                                                            
035806        ADD 1 TO INDX                                                     
035807     END-PERFORM                                                          
035810                                                                          
035900     IF INDATA-OK AND                                                     
035910        SPAR-IDTRANS = '2336' OR '0551'                                   
035930                                                                          
036000       MOVE SPAR-IDSPRGRP-ENTER TO W-IDSPRGRP                             
036100       IF MID-IDSPRGRP-E   = ALL '+'                                      
036200        OR MID-CMD-E      = ALL '+'                                       
036300         PERFORM MFS-RENSA-FAELT-IN                                       
036400       ELSE                                                               
036402         IF RAD-VALD                                                      
036403            PERFORM MFS-LAES-IN-IGEN-E                                    
036404            MOVE 1 TO INDX                                                
036405            PERFORM UNTIL INDX > MAX-INDX                                 
036407               MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR (INDX)             
036408               ADD +1 TO INDX                                             
036409            END-PERFORM                                                   
036410            MOVE MFS-STAENG-FAELT  TO MOD-CMD-E-ATTR                      
036420                                      MOD-IDSPRGRP-E-ATTR                 
036710            CONTINUE                                                      
036800         ELSE                                                             
036930            PERFORM MFS-ROER-EJ-FAELT-UT                                  
036940            PERFORM MFS-ROER-EJ-FAELT-IN                                  
036950            PERFORM MFS-ROER-EJ-FAELT-IN-E                                
036951            PERFORM MFS-LAES-IN-IGEN-E                                    
036952         END-IF                                                           
036953         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
036954         CALL WMEDKONV USING MED-WMEDAREA                                 
036955         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
037010       END-IF                                                             
037300     END-IF                                                               
037500     .                                                                    
037600                                                                          
037610                                                                          
037700 EA-FLYTTA-VALD-RAD SECTION.                                              
037701     MOVE 'EA-FLYTTA-VALD  ' TO CURRENT-SECTION                           
037702                                                                          
037703     MOVE MFS-RENSA-FAELT     TO MOD-KDCMD (INDX)                         
037704     MOVE MID-IDSPRGRP (INDX) TO W-IDSPRGRP                               
037706                                                                          
037707     PERFORM IMS-GHU-WDF801                                               
037708     MOVE 'C'            TO MOD-CMD-E                                     
037709                            MID-CMD-E                                     
037713     MOVE GSPR-IDSPRGRP  TO MOD-IDSPRGRP-E                                
037714                            MID-IDSPRGRP-E                                
037715     MOVE GSPR-FLAUTUPD  TO MOD-FLAUTUPD-E                                
037716                            MID-FLAUTUPD-E                                
037717     MOVE GSPR-TISTADAT  TO MOD-TISTADAT-E                                
037718                            MID-TISTADAT-E                                
037719                            SPAR-TISTADAT                                 
037720     MOVE GSPR-KDMARKBLK TO MOD-KDMARKBLK-E                               
037721                            MID-KDMARKBLK-E                               
037722     MOVE GSPR-TENOTE    TO MOD-TENOTE-E                                  
037723                            MID-TENOTE-E                                  
037724     MOVE GSPR-TENOTE-60 TO MOD-TENOTE-60-E                               
037725                            MID-TENOTE-60-E                               
037726     MOVE MFS-STAENG-FAELT  TO MOD-CMD-E-ATTR                             
037727                               MOD-IDSPRGRP-E-ATTR                        
037728                                                                          
037729     .                                                                    
037730                                                                          
037740                                                                          
037779 F-LAES-VISA-WDF8 SECTION.                                                
037780     MOVE 'F-LAES-VISA-WDF8' TO CURRENT-SECTION                           
037790                                                                          
038100     PERFORM IMS-GU-WDF8B1-START                                          
038200     IF SEGMENT-SAKNAS                                                    
038300        MOVE GROUP-MISSING TO MED-IDMFSFEL                                
038400        CALL WMEDKONV USING   MED-WMEDAREA                                
038500        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
038600        PERFORM MFS-RENSA-FAELT-UT                                        
038700     ELSE                                                                 
038800        CONTINUE                                                          
038900     END-IF                                                               
039100                                                                          
039200     MOVE GSPR-IDSPRGRP TO SPAR-IDSPRGRP-ENTER                            
039300     MOVE +1 TO INDX                                                      
039400     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                      
039500     OR BAS-SLUT                                                          
039600       IF SEGMENT-FINNS                                                   
039700         IF INDATA-FEL AND MID-KDCMD (INDX) NOT = '+'                     
039710          MOVE MID-KDCMD (INDX) TO MOD-KDCMD (INDX)                       
039720         END-IF                                                           
039800         MOVE GSPR-IDSPRGRP     TO MOD-IDSPRGRP  (INDX)                   
039810         MOVE GSPR-FLAUTUPD     TO MOD-FLAUTUPD  (INDX)                   
039900         MOVE GSPR-KDMARKBLK    TO MOD-KDMARKBLK (INDX)                   
039910         MOVE GSPR-TENOTE       TO MOD-TENOTE    (INDX)                   
039911*        MOVE GSPR-IDUSER       TO MOD-IDUSER-CR (INDX)                   
039912         STRING '(' GSPR-IDUSER ')' DELIMITED BY SPACE                    
039913                              INTO MOD-IDUSER-CR (INDX)                   
039920         MOVE GSPR-TENOTE-60    TO MOD-TENOTE-60 (INDX)                   
040000                                                                          
040400         MOVE GSPR-TISTADAT      TO WS-TISTADAT                           
040500         MOVE WS-TISTADAT        TO MOD-TISTADAT (INDX)                   
040510         IF GSPR-TISTADAT > DAGENS-DATUM                                  
040520            MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-ATTR (INDX)        
040530         END-IF                                                           
040600       ELSE                                                               
040700         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
040710         MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR (INDX)                   
040800       END-IF                                                             
040900       ADD +1 TO INDX                                                     
041000       PERFORM IMS-GN-WDF8B1-NEXT                                         
041100     END-PERFORM                                                          
041110     PERFORM UNTIL INDX > MAX-INDX                                        
041197         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
041198         MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR (INDX)                   
041200       ADD +1 TO INDX                                                     
041202     END-PERFORM                                                          
041210                                                                          
041300     IF SEGMENT-FINNS                                                     
041400       MOVE GSPR-IDSPRGRP        TO SPAR-IDSPRGRP-NEXT                    
041410       MOVE '002'     TO MSGI-KDCALL                                      
041420       MOVE '2336'    TO SPAR-IDTRANS                                     
041440       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
041450       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
041460       IF NOT MFS-UPDATE                                                  
041500         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
041600         CALL WMEDKONV USING MED-WMEDAREA                                 
041700         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
041710       END-IF                                                             
041800     END-IF                                                               
041900     .                                                                    
042000                                                                          
042010                                                                          
042100 G-KOLLA-INPUT SECTION.                                                   
042110     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
042200                                                                          
042300     MOVE JA  TO INDATA-SW                                                
042400     MOVE JA  TO ALLT-SW                                                  
042401     MOVE NEJ TO RAD-VALD-SW                                              
042410                                                                          
042420     MOVE 1 TO INDX                                                       
042430     PERFORM UNTIL INDX > MAX-INDX                                        
042431        IF MID-KDCMD(INDX) NOT = '+' AND ' '                              
042432           IF MID-KDCMD(INDX) = 'D' AND                                   
042433              RAD-EJ-VALD                                                 
042434              MOVE JA                   TO RAD-VALD-SW                    
042435              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)          
042436              PERFORM GA-KOLLA-BORTTAG                                    
042445           ELSE                                                           
042448              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)          
042449              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
042450              CALL WMEDKONV USING MED-WMEDAREA                            
042451              MOVE MED-MFSFEL           TO MOD-TEMFSFEL                   
042454              PERFORM MFS-ROER-EJ-FAELT-UT                                
042455              PERFORM MFS-ROER-EJ-FAELT-IN                                
042456              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
042458              MOVE NEJ TO INDATA-SW                                       
042460           END-IF                                                         
042461        END-IF                                                            
042462        ADD 1 TO INDX                                                     
042463     END-PERFORM                                                          
042481                                                                          
042490     IF INDATA-OK                                                         
042500       IF (MID-CMD-E = ALL '+' OR MID-CMD-E = SPACE)                      
042600        AND (MID-IDSPRGRP-E  = ALL '+' OR SPACE)                          
042700        AND (MID-TISTADAT-E  = ALL '+' OR SPACE)                          
042800        AND (MID-KDMARKBLK-E = ALL '+' OR SPACE)                          
042861                                                                          
042870          IF RAD-EJ-VALD                                                  
042900             MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                    
043000             CALL WMEDKONV USING          MED-WMEDAREA                    
043100             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
043200             PERFORM MFS-ROER-EJ-FAELT-IN-E                               
043300             PERFORM MFS-ROER-EJ-FAELT-UT                                 
043400             MOVE NEJ                  TO INDATA-SW                       
043410          END-IF                                                          
043510       ELSE                                                               
043600                                                                          
043700         IF MID-CMD-E = 'N' OR MID-CMD-E OR 'C'                           
043800            MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-E-ATTR                   
043900         ELSE                                                             
044000            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
044100            MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-E-ATTR                   
044200            MOVE NEJ                  TO INDATA-SW                        
044300            MOVE NEJ                  TO ALLT-SW                          
044400         END-IF                                                           
045310                                                                          
045320         IF MID-IDSPRGRP-E(1:1) = SPACE                                   
045330           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
045340           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSPRGRP-E-ATTR               
045350           MOVE NEJ                  TO INDATA-SW                         
045360           MOVE NEJ                  TO ALLT-SW                           
045370         ELSE                                                             
045380           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSPRGRP-E-ATTR               
045390         END-IF                                                           
045400                                                                          
045510*---DATUM LAGRAS 8-STÄLLIGT PÅ BAS, DÄRFÖR DENNA KOD                      
045600         IF MID-TISTADAT-E = ALL '+'                                      
045700*---OM MAN EJ MATAT IN DATUM TAS DAGENS DATUM                             
045900           MOVE DAGENS-DATUM    TO WS-TISTADAT                            
046000         ELSE                                                             
046001           MOVE MID-TISTADAT-E  TO WS-TISTADAT                            
046010         END-IF                                                           
046800                                                                          
046900*---SLUT DATUMKOD                                                         
047000                                                                          
047100*---VALIDERING AV DATUM                                                   
047110         IF WS-TISTADAT NOT = SPAR-TISTADAT                               
047200            MOVE 'AAMMDD'            TO DAT-KDDATFORM                     
047300            MOVE WS-TISTADAT         TO DAT-I-TIDATUM                     
047400                                                                          
047500            CALL WDATKONV USING      DAT-KDDATFORM                        
047600                                     DAT-I-TIDATUM                        
047700                                     DAT-O-TIDATUM                        
047800                                     DAT-KDSVAR                           
047900            IF DAT-KDSVAR-FEL                                             
048000              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
048100              MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-E-ATTR            
048200              MOVE NEJ                  TO INDATA-SW                      
048300              MOVE NEJ                  TO ALLT-SW                        
048410            END-IF                                                        
048420         END-IF                                                           
048500                                                                          
048600         IF MID-KDMARKBLK-E = ALL '+'                                     
048700           MOVE ZERO           TO WS-KDMARKBLK                            
048800         ELSE                                                             
048900           INSPECT MID-KDMARKBLK-E REPLACING LEADING SPACE BY ZERO        
049000           IF MID-KDMARKBLK-E = ZERO                                      
049100           OR (MFS-UPDATE                                                 
049101           AND MID-KDMARKBLK-E = 51)                                      
049110           OR (MFS-UPDATE                                                 
049200           AND MID-KDMARKBLK-E = 67)                                      
049300             MOVE MID-KDMARKBLK-E                                         
049400                             TO WS-KDMARKBLK                              
049500           ELSE                                                           
049600             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
049700             MOVE MFS-NUM-FAELT-FEL    TO MOD-KDMARKBLK-E-ATTR            
049800             MOVE NEJ                  TO INDATA-SW                       
049900             MOVE NEJ                  TO ALLT-SW                         
050000           END-IF                                                         
050110         END-IF                                                           
050200                                                                          
050300         IF ALLT-OK                                                       
050400           IF MID-CMD-E = 'N'                                             
050500*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
050600             IF WS-TISTADAT >= DAGENS-DATUM                               
050700               MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTADAT-E-ATTR            
050800             ELSE                                                         
050900               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
051000               MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-E-ATTR           
051100               MOVE NEJ                  TO INDATA-SW                     
051200             END-IF                                                       
051300                                                                          
051400*---KOLLA SÅ ATT INTE SEGMENT REDAN FINNS PÅ BAS                          
051500             MOVE MID-IDSPRGRP-E TO W-IDSPRGRP                            
051600             PERFORM IMS-GHU-WDF801                                       
051700             IF SEGMENT-FINNS                                             
051800               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSPRGRP-E-ATTR           
051900               MOVE NEJ                  TO INDATA-SW                     
052000               MOVE GROUP-EXISTS         TO MED-IDMFSFEL                  
052100             ELSE                                                         
052200               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSPRGRP-E-ATTR           
052300             END-IF                                                       
052400           END-IF                                                         
052500                                                                          
052600           IF MID-CMD-E = 'C'                                             
052700*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
052800             IF WS-TISTADAT >= DAGENS-DATUM                               
052810             OR WS-TISTADAT  = SPAR-TISTADAT                              
052900               MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISTADAT-E-ATTR           
053000             ELSE                                                         
053100               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
053200               MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-E-ATTR           
053300               MOVE NEJ                  TO INDATA-SW                     
053400             END-IF                                                       
053500                                                                          
053600*---KOLLA SÅ SEGMENT FINNS PÅ BAS                                         
053700             MOVE MID-IDSPRGRP-E TO W-IDSPRGRP                            
053800             PERFORM IMS-GHU-WDF801                                       
053900             IF SEGMENT-SAKNAS                                            
054000               MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSPRGRP-E-ATTR           
054100               MOVE NEJ                  TO INDATA-SW                     
054200               MOVE  GROUP-MISSING       TO MED-IDMFSFEL                  
054300             ELSE                                                         
054400               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSPRGRP-E-ATTR           
054500             END-IF                                                       
054600           END-IF                                                         
054700                                                                          
056700         END-IF                                                           
056710                                                                          
056720         IF MID-FLAUTUPD-E = 'J' OR MID-FLAUTUPD-E = 'Y'                  
056730         OR MID-FLAUTUPD-E = 'N'                                          
056731*        DISPLAY '2336 ----- 01 GB-KOLLA-BYTE'                            
056740           PERFORM GB-KOLLA-BYTE                                          
056750           IF BYTE-OK                                                     
056751*        DISPLAY '2336 ----- 01 BYTE-OK'                                  
056760              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAUTUPD-E-ATTR            
056770           ELSE                                                           
056780*             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
056781*        DISPLAY '2336 ----- 01 BYTE-FEL'                                 
056790              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAUTUPD-E-ATTR              
056791              MOVE NEJ               TO INDATA-SW                         
056792              MOVE NEJ               TO ALLT-SW                           
056793           END-IF                                                         
056794         ELSE                                                             
056795           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
056796           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAUTUPD-E-ATTR               
056797           MOVE NEJ                  TO INDATA-SW                         
056798           MOVE NEJ                  TO ALLT-SW                           
056799         END-IF                                                           
056800                                                                          
056900         IF INDATA-FEL                                                    
056910           IF MED-IDMFSFEL (1:1) NOT = 'A'                                
057000              CALL WMEDKONV USING MED-WMEDAREA                            
057100              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
057110           END-IF                                                         
057200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
057300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
057310           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
057400           MOVE NEJ         TO ALLT-SW                                    
057500         END-IF                                                           
057600       END-IF                                                             
057610     END-IF                                                               
057700     .                                                                    
057801                                                                          
057810                                                                          
057900 GA-KOLLA-BORTTAG SECTION.                                                
057910     MOVE 'GA-KOLLA-BORTTAG' TO CURRENT-SECTION                           
057920                                                                          
057921*---KOLLA SÅ INGA UNDERLIGGANDE SEGMENT FINNS PÅ GRUPPEN                  
057922     MOVE MID-IDSPRGRP (INDX)   TO W-IDSPRGRP                             
057925     PERFORM IMS-GHU-WDF801                                               
057926                                                                          
057928                                                                          
057931     PERFORM IMS-GNP-WDF8                                                 
057932     IF SEGMENT-FINNS                                                     
057934        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)                  
057936        MOVE NEJ                TO INDATA-SW                              
057939        MOVE PARTS-IN-GROUP     TO MED-IDMFSFEL                           
057940        CALL WMEDKONV USING MED-WMEDAREA                                  
057941        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
057942     ELSE                                                                 
057943        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)                
057944     END-IF                                                               
057948     .                                                                    
057949                                                                          
057950                                                                          
057951 GB-KOLLA-BYTE    SECTION.                                                
057952     MOVE 'GB-KOLLA-BYTE   ' TO CURRENT-SECTION                           
057953                                                                          
057954     MOVE JA TO BYTE-SW                                                   
057955     MOVE MID-IDSPRGRP-E     TO W-IDSPRGRP                                
057956     PERFORM IMS-GHU-WDF801                                               
057957                                                                          
057958     IF SEGMENT-FINNS                                                     
057959        IF GSPR-FLAUTUPD NOT = MID-FLAUTUPD-E                             
057960           IF GSPR-FLAUTUPD = NEJ                                         
057961*---KOLLA SÅ INGA ARTIKLAS FINNS PÅ GRUPPEN                               
057962              PERFORM IMS-GNP-WDF812                                      
057963              IF SEGMENT-FINNS                                            
057964*        DISPLAY '2336 ----- 01 NEJ 01  '                                 
057965                 MOVE NEJ TO BYTE-SW                                      
057966                 MOVE 'A01'          TO MED-IDMFSFEL                      
057967                 MOVE  AUTO-PARTS    TO MOD-TEMFSFEL                      
057969              END-IF                                                      
057970           ELSE                                                           
057971*---KOLLA SÅ INGA REGLER FINNS PÅ GRUPPEN                                 
057972              PERFORM IMS-GNP-WDF813                                      
057973              IF SEGMENT-SAKNAS                                           
057974                 PERFORM IMS-GNP-WDF814                                   
057975                 IF SEGMENT-SAKNAS                                        
057976                    PERFORM IMS-GNP-WDF815                                
057977                 END-IF                                                   
057978              END-IF                                                      
057979              IF SEGMENT-FINNS                                            
057980*        DISPLAY '2336 ----- 01 NEJ 02  '                                 
057981                 MOVE NEJ TO BYTE-SW                                      
057982                 MOVE 'A02'          TO MED-IDMFSFEL                      
057983                 MOVE  AUTO-RULES    TO MOD-TEMFSFEL                      
057987              END-IF                                                      
057988           END-IF                                                         
057989        END-IF                                                            
057990     END-IF                                                               
057994     .                                                                    
057995                                                                          
057996                                                                          
057997 H-UPPDATERA SECTION.                                                     
057998     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
058000                                                                          
058010     MOVE 1 TO INDX                                                       
058020     PERFORM UNTIL INDX > MAX-INDX                                        
058030        IF MID-KDCMD (INDX) = 'D'                                         
058040           MOVE MID-IDSPRGRP (INDX) TO W-IDSPRGRP                         
058060           PERFORM IMS-GHU-WDF801                                         
058070           IF SEGMENT-FINNS                                               
058080              PERFORM IMS-DLET-WDF801                                     
058090           END-IF                                                         
058091        END-IF                                                            
058092        ADD 1 TO INDX                                                     
058093     END-PERFORM                                                          
058094                                                                          
058900     IF MID-CMD-E = 'C'                                                   
058910        MOVE MID-IDSPRGRP-E TO W-IDSPRGRP                                 
058930        PERFORM IMS-GHU-WDF801                                            
058940                                                                          
058950        IF MID-FLAUTUPD-E = 'J' OR MID-FLAUTUPD-E = 'Y'                   
058960           MOVE JA              TO GSPR-FLAUTUPD                          
058970        ELSE                                                              
058980           MOVE NEJ             TO GSPR-FLAUTUPD                          
058990        END-IF                                                            
058991        IF WS-TISTADAT NOT = SPAR-TISTADAT                                
059000           MOVE WS-TISTADAT     TO GSPR-TISTADAT                          
059010        END-IF                                                            
059100        MOVE WS-KDMARKBLK       TO GSPR-KDMARKBLK                         
059110        IF MID-TENOTE-E NOT = ALL '+'                                     
059120           MOVE MID-TENOTE-E    TO GSPR-TENOTE                            
059130        END-IF                                                            
059140        IF MID-TENOTE-60-E NOT = ALL '+'                                  
059150           MOVE MID-TENOTE-60-E TO GSPR-TENOTE-60                         
059160        END-IF                                                            
059200        PERFORM IMS-REPL-WDF801                                           
059210     ELSE                                                                 
059220        IF MID-CMD-E = 'N'                                                
059230           MOVE MID-IDSPRGRP-E     TO GSPR-IDSPRGRP                       
059231           IF MID-FLAUTUPD-E = 'J' OR MID-FLAUTUPD-E = 'Y'                
059232              MOVE JA           TO GSPR-FLAUTUPD                          
059233           ELSE                                                           
059234              MOVE NEJ          TO GSPR-FLAUTUPD                          
059235           END-IF                                                         
059240           MOVE WS-TISTADAT        TO GSPR-TISTADAT                       
059250           MOVE WS-KDMARKBLK       TO GSPR-KDMARKBLK                      
059251           IF MID-TENOTE-E NOT = ALL '+'                                  
059252              MOVE MID-TENOTE-E    TO GSPR-TENOTE                         
059253           END-IF                                                         
059254           IF MID-TENOTE-60-E NOT = ALL '+'                               
059255              MOVE MID-TENOTE-60-E TO GSPR-TENOTE-60                      
059256           END-IF                                                         
059257           MOVE MSGI-IDUSER        TO GSPR-IDUSER                         
059258                                                                          
059260           PERFORM IMS-ISRT-WDF801                                        
060100        END-IF                                                            
060110     END-IF                                                               
060200                                                                          
060300     MOVE INF-UPDATE-DONE    TO MED-IDMFSINF                              
060400     CALL WMEDKONV USING        MED-WMEDAREA                              
060500     MOVE MED-MFSINF         TO MOD-TEMFSINF                              
060510                                                                          
060600     PERFORM MFS-FORM-ATTR                                                
060700     PERFORM MFS-RENSA-FAELT-IN                                           
060800     .                                                                    
060900                                                                          
060910                                                                          
061000 MFS-RENSA-FAELT-UT SECTION.                                              
061100                                                                          
061200*    --- ALLA UTDATA-FÄLT                                                 
061300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
061400     MOVE MFS-RENSA-FAELT TO MOD-CMD-E                                    
061500                             MOD-IDSPRGRP-E                               
061600                             MOD-FLAUTUPD-E                               
061610                             MOD-TISTADAT-E                               
061700                             MOD-KDMARKBLK-E                              
061710                             MOD-TENOTE-E                                 
061720                             MOD-TENOTE-60-E                              
061800     MOVE +1 TO INDX                                                      
061900     PERFORM UNTIL INDX > MAX-INDX                                        
062000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
062100       ADD +1 TO INDX                                                     
062200     END-PERFORM                                                          
062300     .                                                                    
062400                                                                          
062500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
062600                                                                          
062700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
062800     MOVE MFS-RENSA-FAELT TO MOD-KDCMD     (INDX)                         
062810                             MOD-IDSPRGRP  (INDX)                         
062900                             MOD-FLAUTUPD  (INDX)                         
062910                             MOD-TISTADAT  (INDX)                         
063000                             MOD-KDMARKBLK (INDX)                         
063010                             MOD-TENOTE    (INDX)                         
063011                             MOD-IDUSER-CR (INDX)                         
063020                             MOD-TENOTE-60 (INDX)                         
063100     .                                                                    
063200                                                                          
063300 MFS-RENSA-FAELT-IN SECTION.                                              
063400                                                                          
063500*    --- ALLA INDATA-FÄLT                                                 
063600     MOVE MFS-RENSA-FAELT TO MOD-IDSPRGRP-IN                              
064010     MOVE +1 TO INDX                                                      
064020     PERFORM UNTIL INDX > MAX-INDX                                        
064030       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
064040       ADD +1 TO INDX                                                     
064050     END-PERFORM                                                          
064100     .                                                                    
064200                                                                          
064210 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
064220                                                                          
064240     MOVE MFS-RENSA-FAELT TO MOD-KDCMD     (INDX)                         
064250                             MOD-IDSPRGRP  (INDX)                         
064260                             MOD-FLAUTUPD  (INDX)                         
064270                             MOD-TISTADAT  (INDX)                         
064280                             MOD-KDMARKBLK (INDX)                         
064290                             MOD-TENOTE    (INDX)                         
064291                             MOD-IDUSER-CR (INDX)                         
064292                             MOD-TENOTE-60 (INDX)                         
064293     .                                                                    
064294                                                                          
064300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
064400                                                                          
064500*    --- ALLA UTDATA-FÄLT                                                 
064600*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
064700     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-E                                  
064800                               MOD-IDSPRGRP-E                             
064900                               MOD-FLAUTUPD-E                             
064910                               MOD-TISTADAT-E                             
065000                               MOD-KDMARKBLK-E                            
065010                               MOD-TENOTE-E                               
065020                               MOD-TENOTE-60-E                            
065100     MOVE +1 TO INDX                                                      
065200     PERFORM UNTIL INDX > MAX-INDX                                        
065300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
065400       ADD +1 TO INDX                                                     
065500     END-PERFORM                                                          
065700     .                                                                    
065710                                                                          
065800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
065900                                                                          
066000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
066100     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD     (INDX)                       
066110                               MOD-IDSPRGRP  (INDX)                       
066200                               MOD-FLAUTUPD  (INDX)                       
066210                               MOD-TISTADAT  (INDX)                       
066300                               MOD-KDMARKBLK (INDX)                       
066310                               MOD-TENOTE    (INDX)                       
066311                               MOD-IDUSER-CR (INDX)                       
066320                               MOD-TENOTE-60 (INDX)                       
066400     .                                                                    
066500                                                                          
066600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
066700                                                                          
066800*    --- ALLA INDATA-FÄLT                                                 
066900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDSPRGRP-IN                            
067310     MOVE 1 TO INDX                                                       
067320     PERFORM UNTIL INDX > MAX-INDX                                        
067330        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD    (INDX)                     
067340                                  MOD-IDSPRGRP (INDX)                     
067350        ADD 1 TO INDX                                                     
067360     END-PERFORM                                                          
067400     .                                                                    
067500                                                                          
067510 MFS-ROER-EJ-FAELT-IN-E SECTION.                                          
067520                                                                          
067530*    --- ALLA INDATA-FÄLT                                                 
067550     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-E                                  
067560                               MOD-IDSPRGRP-E                             
067570                               MOD-FLAUTUPD-E                             
067580                               MOD-TISTADAT-E                             
067590                               MOD-KDMARKBLK-E                            
067591                               MOD-TENOTE-E                               
067592                               MOD-TENOTE-60-E                            
067599     .                                                                    
067600                                                                          
067610 MFS-FORM-ATTR SECTION.                                                   
067700                                                                          
067800*    --- ALLA INDATA-FÄLT                                                 
067900     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-E-ATTR                            
068000                                MOD-IDSPRGRP-E-ATTR                       
068100                                MOD-FLAUTUPD-E-ATTR                       
068110                                MOD-TISTADAT-E-ATTR                       
068200                                MOD-KDMARKBLK-E-ATTR                      
068210                                MOD-TENOTE-E-ATTR                         
068220                                MOD-TENOTE-60-E-ATTR                      
068230     MOVE 1 TO INDX                                                       
068240     PERFORM UNTIL INDX > MAX-INDX                                        
068250        MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR    (INDX)               
068260                                   MOD-IDSPRGRP-ATTR (INDX)               
068270        ADD 1 TO INDX                                                     
068280     END-PERFORM                                                          
068300     .                                                                    
068400                                                                          
069310 MFS-LAES-IN-IGEN-E SECTION.                                              
069320                                                                          
069330*    --- ALLA INDATA-FÄLT                                                 
069340     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-E-ATTR                         
069350                                   MOD-IDSPRGRP-E-ATTR                    
069360                                   MOD-FLAUTUPD-E-ATTR                    
069370                                   MOD-TISTADAT-E-ATTR                    
069380                                   MOD-KDMARKBLK-E-ATTR                   
069390                                   MOD-TENOTE-E-ATTR                      
069391                                   MOD-TENOTE-60-E-ATTR                   
069393     MOVE MFS-ADD-SAETT-CURSOR  TO MOD-CMD-E-ATTR                         
069398     .                                                                    
069399                                                                          
069400* --- IMS SEKTIONER ---                                                   
069500                                                                          
069600 IMS-GET-MSG SECTION.                                                     
069610     MOVE 'IMS-GET-MSG     ' TO CURRENT-IMS-SECTION                       
069700                                                                          
069800     MOVE '  QC' TO GODK-STATUSKODER                                      
069900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
070000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070100     PERFORM IMS-STATUSKONTROLL                                           
070200     .                                                                    
070300                                                                          
070400 IMS-INSERT-MSG SECTION.                                                  
070410     MOVE 'IMS-INSERT-MSG  ' TO CURRENT-IMS-SECTION                       
070500                                                                          
070600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070700     MOVE SPACE TO GODK-STATUSKODER                                       
070800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200                                                                          
071210                                                                          
072400 IMS-GHU-WDF801      SECTION.                                             
072410     MOVE 'IMS-GHU-WDF801  '  TO CURRENT-IMS-SECTION                      
072420                                                                          
072430     MOVE SPACE               TO ALL-SSA                                  
072500                                                                          
072600     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
072700                                                                          
072800          DELIMITED BY SIZE INTO SSA1                                     
072900     MOVE '  GEGB'            TO GODK-STATUSKODER                         
073000     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF801 SSA1                   
073100     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
073200     PERFORM IMS-STATUSKONTROLL                                           
073300     .                                                                    
073400                                                                          
073410                                                                          
073500 IMS-GU-WDF8B1-START  SECTION.                                            
073510     MOVE 'GU-WDF801-START '  TO CURRENT-IMS-SECTION                      
073520                                                                          
073530     MOVE SPACE               TO ALL-SSA                                  
073600                                                                          
073700     STRING 'WDF801  (WDF8BSEQ>=' W-IDSPRGRP-X ')'                        
073900          DELIMITED BY SIZE INTO SSA1                                     
074000     MOVE '  GE'              TO GODK-STATUSKODER                         
074100     CALL CBLTDLI USING GU WDF8B-PCB DLI-IO-WDF801 SSA1                   
074200     MOVE WDF8B-STATUS-CODE   TO STATUS-WS                                
074300     PERFORM IMS-STATUSKONTROLL                                           
074400     .                                                                    
074500                                                                          
074510                                                                          
074600 IMS-GN-WDF8B1-NEXT SECTION.                                              
074610     MOVE 'GN-WDF801-NEXT  '  TO CURRENT-IMS-SECTION                      
074620                                                                          
074630     MOVE SPACE               TO ALL-SSA                                  
074700                                                                          
074800     STRING 'WDF801  (WDF8BSEQ>=' W-IDSPRGRP-X ')'                        
075000          DELIMITED BY SIZE INTO SSA1                                     
075100     MOVE '  GEGB'            TO GODK-STATUSKODER                         
075200     CALL CBLTDLI USING GN WDF8B-PCB DLI-IO-WDF801 SSA1                   
075300     MOVE WDF8B-STATUS-CODE   TO STATUS-WS                                
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600                                                                          
075610                                                                          
075700 IMS-ISRT-WDF801 SECTION.                                                 
075710     MOVE 'IMS-ISRT-WDF801 '  TO CURRENT-IMS-SECTION                      
075720                                                                          
075730     MOVE SPACE               TO ALL-SSA                                  
075800                                                                          
075900     MOVE 'WDF801   '         TO SSA1                                     
076000     MOVE '  II'              TO GODK-STATUSKODER                         
076100     CALL CBLTDLI USING ISRT WDF8-PCB DLI-IO-WDF801 SSA1                  
076200     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
076300     PERFORM IMS-STATUSKONTROLL                                           
076400     .                                                                    
076500                                                                          
076510                                                                          
076600 IMS-REPL-WDF801 SECTION.                                                 
076610     MOVE 'IMS-REPL-WDF801 '  TO CURRENT-IMS-SECTION                      
076620                                                                          
076630     MOVE SPACE               TO ALL-SSA                                  
076700                                                                          
076800     MOVE '  '                TO GODK-STATUSKODER                         
076900     CALL CBLTDLI USING REPL WDF8-PCB DLI-IO-WDF801                       
077000     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
077100     PERFORM IMS-STATUSKONTROLL                                           
077200     .                                                                    
077300                                                                          
077310                                                                          
077400 IMS-DLET-WDF801 SECTION.                                                 
077410     MOVE 'IMS-DLET-WDF801 '  TO CURRENT-IMS-SECTION                      
077420                                                                          
077430     MOVE SPACE               TO ALL-SSA                                  
077500                                                                          
077600     MOVE '  '                TO GODK-STATUSKODER                         
077700     CALL CBLTDLI USING DLET WDF8-PCB DLI-IO-WDF801                       
077800     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100                                                                          
078110                                                                          
078200 IMS-GNP-WDF8 SECTION.                                                    
078210     MOVE 'IMS-GNP-WDF8    '  TO CURRENT-IMS-SECTION                      
078220                                                                          
078230     MOVE SPACE               TO ALL-SSA                                  
078300                                                                          
078700     MOVE '  GE' TO GODK-STATUSKODER                                      
078800     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-DUMMY                         
078900     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
079000     PERFORM IMS-STATUSKONTROLL                                           
079100     .                                                                    
079200                                                                          
079210                                                                          
079220 IMS-GNP-WDF812 SECTION.                                                  
079230     MOVE 'IMS-GNP-WDF812  '  TO CURRENT-IMS-SECTION                      
079240                                                                          
079250     MOVE SPACE               TO ALL-SSA                                  
079251     MOVE 'WDF812 '           TO SSA1                                     
079260                                                                          
079270     MOVE '  GE' TO GODK-STATUSKODER                                      
079280     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-DUMMY SSA1                    
079290     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
079291     PERFORM IMS-STATUSKONTROLL                                           
079292     .                                                                    
079293                                                                          
079294                                                                          
079295 IMS-GNP-WDF813 SECTION.                                                  
079296     MOVE 'IMS-GNP-WDF813  '  TO CURRENT-IMS-SECTION                      
079297                                                                          
079298     MOVE SPACE               TO ALL-SSA                                  
079299     MOVE 'WDF813 '           TO SSA1                                     
079300                                                                          
079301     MOVE '  GE' TO GODK-STATUSKODER                                      
079302     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-DUMMY SSA1                    
079303     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
079304     PERFORM IMS-STATUSKONTROLL                                           
079305     .                                                                    
079306                                                                          
079307                                                                          
079308 IMS-GNP-WDF814 SECTION.                                                  
079309     MOVE 'IMS-GNP-WDF814  '  TO CURRENT-IMS-SECTION                      
079310                                                                          
079311     MOVE SPACE               TO ALL-SSA                                  
079312     MOVE 'WDF814 '           TO SSA1                                     
079313                                                                          
079314     MOVE '  GE' TO GODK-STATUSKODER                                      
079315     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-DUMMY SSA1                    
079316     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
079317     PERFORM IMS-STATUSKONTROLL                                           
079318     .                                                                    
079319                                                                          
079320                                                                          
079321 IMS-GNP-WDF815 SECTION.                                                  
079322     MOVE 'IMS-GNP-WDF815  '  TO CURRENT-IMS-SECTION                      
079323                                                                          
079324     MOVE SPACE               TO ALL-SSA                                  
079325     MOVE 'WDF815 '           TO SSA1                                     
079326                                                                          
079327     MOVE '  GE' TO GODK-STATUSKODER                                      
079328     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-DUMMY SSA1                    
079329     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
079330     PERFORM IMS-STATUSKONTROLL                                           
079331     .                                                                    
079332                                                                          
079333                                                                          
079340 IMS-STATUSKONTROLL SECTION.                                              
079400                                                                          
079500     SET STATUS-IX TO 1                                                   
079600     SEARCH GODK-STATUS                                                   
079700       AT END                                                             
079800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
079900         DELIMITED BY SIZE INTO FELTEXT                                   
080000         CALL FELLOG                                                      
080100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
080200         CONTINUE                                                         
080300     END-SEARCH                                                           
080400     .                                                                    
