000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0080500.                                                
000400 AUTHOR.         PER BERGH.                                               
000500 DATE-WRITTEN.   90/05/09.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        PROGRAMMET ÄR EN UPPDATERINGS- OCH FRÅGE-MPP                     
001200*        PROGRAMMET UPPDATERAR WLXXKU (WDR1)                              
001300*                                                                         
001400*        -FÖRRÅDSDATATEXTER                                               
001500*                                                                         
001600*        -ÄNDRING TILLÄGG OCH BORTTAG AV ROT OCH SEGMENT                  
001700*         ÄR MÖJLIGT                                                      
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W0T805                                              
002200*        MID:         W0I80501                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W0O80501                                            
002600                                                                          
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W0080500'.            
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700                                                                          
003800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +211  COMP SYNC.        
004000                                                                          
004100 77  UPDATE-SW-S                 PIC X      VALUE 'J'.                    
004200 77  UPDATE-SW-GB                PIC X      VALUE 'J'.                    
004300 77  UPDATE-SW                   PIC X      VALUE 'J'.                    
004400     88   UPDATE-OK                         VALUE 'J'.                    
004500                                                                          
004600 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  MAXIX                       PIC S9(9)  VALUE +2    COMP SYNC.        
004800                                                                          
004900 EJECT                                                                    
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100                                                                          
005200                                                                          
005300 77  WS-4535-KDFDKRAV            PIC X(03).                               
005400                                                                          
005500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005600     88  INDATA-OK                           VALUE 'J'.                   
005700     88  INDATA-FEL                          VALUE 'N'.                   
005800                                                                          
005900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006000     88  NYCKLAR-OK                          VALUE 'J'.                   
006100     88  NYCKLAR-FEL                         VALUE 'N'.                   
006200                                                                          
006300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006400     88  ALLT-OK                             VALUE 'J'.                   
006500                                                                          
006600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  EGEN-MID                            VALUE '0805'.                
006800     88  GODK-MID                            VALUE '0805'.                
007300     SKIP2                                                                
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008100*   -COPY WMEDAREA                                                        
008300     SKIP3                                                                
008400 01  MESSAGE-CODES.                                                       
008500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008700     03  INF-NOT-ON-BASE         PIC X(3)    VALUE '010'.                 
008900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009200     03  ERR-NO-CHANGE           PIC X(3)    VALUE '789'.                 
009300     EJECT                                                                
009400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W0I80501   -PRE MID-                                       
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200     SKIP3                                                                
010300*01  -COPY WMSGAREA                                                       
010500     EJECT                                                                
010600*    03  MOD -COPY W0O80501   -PRE MOD-  -RED MSG-AREA.                   
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011000     SKIP3                                                                
011100*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-4535-IDHTYP-X.                                                 
012000         05  W-4535-IDHTYP       PIC X(04)    VALUE '4535'.               
012100         05  W-4535-KDFDKRAV     PIC S9(3)    COMP-3.                     
012200         05  W-4535-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
012300     03  W-4536-IDSKYLT-X.                                                
012400         05  W-4536-IDSKYLT      PIC X(03).                               
012500         05  W-4536-LOW-VALUE    PIC X(02)    VALUE LOW-VALUE.            
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FINNS                       VALUE '  '.                  
012900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013100     SKIP2                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014100     EJECT                                                                
014200*    ---  DLI INPUT-OUTPUT AREA                                           
014300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014400     SKIP3                                                                
014500 01  DLI-IO-AREA.                                                         
014600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
014700     SKIP3                                                                
014800     03  WLXXKU01 REDEFINES IO-AREA.                                      
014900*        05  -COPY WDGX4535                                               
015100     EJECT                                                                
015200     03  WLXXKU11 REDEFINES IO-AREA.                                      
015300*        05  -COPY WDGX4536                                               
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700                                                                          
015800*01  -COPY W0009      -PRE MSG-                                           
016000     EJECT                                                                
016100*01  -COPY W0008      -PRE XXKU-                                          
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500 PROCEDURE DIVISION  USING MSG-PCB XXKU-PCB.                              
016600     ENTRY 'DLITCBL' USING MSG-PCB XXKU-PCB.                              
016700                                                                          
016800     PERFORM IMS-GET-MSG                                                  
016900     IF SEGMENT-FINNS                                                     
017000        PERFORM A-INIT                                                    
017100        PERFORM B-KOLLA-NYCKLAR                                           
017200           IF NYCKLAR-OK                                                  
017300              IF MFS-UPDATE                                               
017400                 PERFORM D-UPPDATERA                                      
017500              END-IF                                                      
017600              PERFORM C-LAES-VISA-INFO                                    
017700           END-IF                                                         
017800           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
017900           PERFORM IMS-INSERT-MSG                                         
018000     END-IF                                                               
018100                                                                          
018200     MOVE ZERO TO RETURN-CODE                                             
018300     GOBACK                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 A-INIT SECTION.                                                          
018700                                                                          
018800     IF MSG-DUBBLA-TRANSKODER                                             
018900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I80501                
019000        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
019100        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
019200     ELSE                                                                 
019300        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I80501                 
019400        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
019500        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
019600     END-IF                                                               
019700                                                                          
019800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
019900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
020000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020100                                                                          
020200     MOVE LOW-VALUE TO MSG-AREA                                           
020300     MOVE 'W0O80501' TO MFS-IDMOD                                         
020400     MOVE '0805' TO MOD-IDTRANS                                           
020500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
020600                                                                          
020700     IF NOT EGEN-MID                                                      
020800        MOVE SPACE TO MFS-KDTRTYP                                         
020900        MOVE '7' TO MFS-IDPFK                                             
021000     END-IF                                                               
021100                                                                          
021200     IF ENGLISH-TEXT                                                      
021300        MOVE +2 TO SPRAK-IX                                               
021400        MOVE 'GB ' TO MED-IDSKYLT                                         
021500     ELSE                                                                 
021600        MOVE +1 TO SPRAK-IX                                               
021700        MOVE 'S  ' TO MED-IDSKYLT                                         
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 B-KOLLA-NYCKLAR SECTION.                                                 
022200                                                                          
022300     MOVE JA TO NYCKLAR-SW                                                
022400                                                                          
022500*    -- KONTROLL AV KDFDKRAV                                              
022600     MOVE MFS-RENSA-FAELT TO MOD-KDFDKRAV-IN                              
022700                                                                          
022800     IF MID-KDFDKRAV-IN = ALL '+'                                         
022900        MOVE MID-KDFDKRAV-UT TO WS-4535-KDFDKRAV                          
023000        INSPECT WS-4535-KDFDKRAV REPLACING LEADING SPACE BY ZERO          
023100     ELSE                                                                 
023200        MOVE MID-KDFDKRAV-IN TO WS-4535-KDFDKRAV                          
023300        MOVE '7'         TO MFS-IDPFK                                     
023400        MOVE SPACE       TO MFS-KDTRTYP                                   
023500     END-IF                                                               
023600                                                                          
023700     IF WS-4535-KDFDKRAV NUMERIC AND                                      
023800        WS-4535-KDFDKRAV > ZERO                                           
023900        MOVE WS-4535-KDFDKRAV TO W-4535-KDFDKRAV                          
024000     ELSE                                                                 
024100        MOVE NEJ TO NYCKLAR-SW                                            
024200     END-IF                                                               
024300                                                                          
024400     IF GODK-MID OR NYCKLAR-OK                                            
024500        MOVE W-4535-KDFDKRAV TO MOD-KDFDKRAV-UT                           
024600        INSPECT MOD-KDFDKRAV-UT REPLACING LEADING ZERO BY SPACE           
024700     ELSE                                                                 
024800        MOVE MFS-RENSA-FAELT TO MOD-KDFDKRAV-UT                           
024900     END-IF                                                               
025000                                                                          
025100     MOVE MID-IDSKYLT-S  TO MOD-IDSKYLT-S                                 
025200     MOVE MID-IDSKYLT-GB TO MOD-IDSKYLT-GB                                
025300                                                                          
025400     IF NYCKLAR-FEL                                                       
025500        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
025600        CALL WMEDKONV USING MED-WMEDAREA                                  
025700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
025800        PERFORM MFS-RENSA-FAELT-UT                                        
025900     END-IF                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 C-LAES-VISA-INFO SECTION.                                                
026300                                                                          
026400                                                                          
026500     IF MID-BEFDKRAV-S  NOT = ALL '+' OR                                  
026600        MID-KDEMBAL-S   NOT = ALL '+' OR                                  
026700        MID-BEFDKRAV-GB NOT = ALL '+' OR                                  
026800        MID-KDEMBAL-GB  NOT = ALL '+'                                     
026900        IF NOT MFS-UPDATE                                                 
027000           PERFORM CB-INF-PRESS-PF11                                      
027100        END-IF                                                            
027200     ELSE                                                                 
027300                                                                          
027400       PERFORM IMS-GET-XXKU-KU01                                          
027500                                                                          
027600       IF SEGMENT-SAKNAS                                                  
027700          PERFORM CA-INF-NOT-ON-BASE                                      
027800       ELSE                                                               
027900          MOVE +1 TO IX                                                   
028000          PERFORM IMS-GNP-XXKU-KU11                                       
028100                                                                          
028200          PERFORM UNTIL IX > MAXIX                                        
028300             IF SEGMENT-FINNS                                             
028400                IF 4536-IDSKYLT = 'S  '                                   
028500                   MOVE 4536-IDSKYLT  TO MOD-IDSKYLT-S                    
028600                   MOVE 4536-BEFDKRAV TO MOD-BEFDKRAV-S                   
028700                   MOVE 4536-KDEMBAL  TO MOD-KDEMBAL-S                    
028800                ELSE                                                      
028900                  IF 4536-IDSKYLT = 'GB '                                 
029000                     MOVE 4536-IDSKYLT  TO MOD-IDSKYLT-GB                 
029100                     MOVE 4536-BEFDKRAV TO MOD-BEFDKRAV-GB                
029200                     MOVE 4536-KDEMBAL  TO MOD-KDEMBAL-GB                 
029300                  END-IF                                                  
029400                END-IF                                                    
029500                PERFORM IMS-GNP-XXKU-KU11                                 
029600                ADD +1 TO IX                                              
029700             ELSE                                                         
029800                ADD +2 TO IX                                              
029900             END-IF                                                       
030000          END-PERFORM                                                     
030100       END-IF                                                             
030200     END-IF                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 CA-INF-NOT-ON-BASE SECTION.                                              
030600     MOVE INF-NOT-ON-BASE TO MED-IDMFSINF                                 
030700     CALL WMEDKONV USING MED-WMEDAREA                                     
030800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
030900     PERFORM MFS-RENSA-FAELT-UT                                           
031000     .                                                                    
031100     SKIP2                                                                
031200 CB-INF-PRESS-PF11 SECTION.                                               
031300     MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
031400     CALL WMEDKONV USING MED-WMEDAREA                                     
031500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
031600     PERFORM MFS-ROR-EJ-FAELT-IN-S                                        
031700     PERFORM MFS-ROR-EJ-FAELT-IN-GB                                       
031800     IF MID-KDEMBAL-S   NOT = ALL '+'                                     
031900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDEMBAL-S-ATTR                  
032000     END-IF                                                               
032100     IF MID-BEFDKRAV-S  NOT = ALL '+'                                     
032200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEFDKRAV-S-ATTR                 
032300     END-IF                                                               
032400     IF MID-KDEMBAL-GB  NOT = ALL '+'                                     
032500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDEMBAL-GB-ATTR                 
032600     END-IF                                                               
032700     IF MID-BEFDKRAV-GB NOT = ALL '+'                                     
032800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEFDKRAV-GB-ATTR                
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 D-UPPDATERA SECTION.                                                     
033300                                                                          
033400     MOVE JA TO UPDATE-SW                                                 
033500                UPDATE-SW-S                                               
033600                UPDATE-SW-GB                                              
033700                                                                          
033800     IF MID-BEFDKRAV-S  = ALL '+' AND                                     
033900        MID-KDEMBAL-S   = ALL '+' AND                                     
034000        MID-BEFDKRAV-GB = ALL '+' AND                                     
034100        MID-KDEMBAL-GB  = ALL '+'                                         
034200        PERFORM DF-ERR-NO-CHANGE                                          
034300     ELSE                                                                 
034400                                                                          
034500       PERFORM IMS-GHU-XXKU-KU01                                          
034600       IF SEGMENT-FINNS                                                   
034700                                                                          
034800          IF MID-IDSKYLT-GB = SPACE                                       
034900             IF MID-BEFDKRAV-GB NOT = ALL '+' AND                         
035000                MID-BEFDKRAV-GB NOT = SPACE                               
035100                PERFORM DA-GB-TILLAEGG                                    
035200             ELSE                                                         
035300                IF MID-KDEMBAL-GB NOT = '+'                               
035400                   MOVE NEJ TO UPDATE-SW-GB                               
035500                END-IF                                                    
035600             END-IF                                                       
035700          END-IF                                                          
035800                                                                          
035900          IF MID-IDSKYLT-S = SPACE                                        
036000             IF MID-BEFDKRAV-S NOT = ALL '+' AND                          
036100                MID-BEFDKRAV-S NOT = SPACE                                
036200                PERFORM DA-S-TILLAEGG                                     
036300             ELSE                                                         
036400                IF MID-KDEMBAL-S NOT = '+'                                
036500                   MOVE NEJ TO UPDATE-SW-S                                
036600                END-IF                                                    
036700             END-IF                                                       
036800          END-IF                                                          
036900                                                                          
037000          IF MID-IDSKYLT-GB NOT = SPACE                                   
037100                                                                          
037200             MOVE MID-IDSKYLT-GB TO W-4536-IDSKYLT                        
037300             PERFORM IMS-GHU-XXKU-KU11                                    
037400             IF SEGMENT-FINNS                                             
037500                                                                          
037600                 IF MID-BEFDKRAV-GB = ALL '+' AND                         
037700                    MID-KDEMBAL-GB  = ALL '+'                             
037800                    PERFORM DE-GB-SAMMA-TEXT                              
037900                 ELSE                                                     
038000                    IF MID-BEFDKRAV-GB = SPACE                            
038100                       PERFORM DC-TA-BORT                                 
038200                       MOVE SPACE TO MOD-KDEMBAL-GB                       
038300                                     MOD-IDSKYLT-GB                       
038400                       MOVE MFS-ADD-LYS-UPP-FAELT                         
038500                            TO MOD-BEFDKRAV-GB-ATTR                       
038600                               MOD-KDEMBAL-GB-ATTR                        
038700                    ELSE                                                  
038800                       PERFORM DB-GB-AENDRA                               
038900                    END-IF                                                
039000                 END-IF                                                   
039100                                                                          
039200             END-IF                                                       
039300          END-IF                                                          
039400                                                                          
039500          IF MID-IDSKYLT-S NOT = SPACE                                    
039600             MOVE MID-IDSKYLT-S  TO W-4536-IDSKYLT                        
039700             PERFORM IMS-GHU-XXKU-KU11                                    
039800             IF SEGMENT-FINNS                                             
039900                                                                          
040000                 IF MID-BEFDKRAV-S = ALL '+' AND                          
040100                    MID-KDEMBAL-S  = ALL '+'                              
040200                    PERFORM DE-S-SAMMA-TEXT                               
040300                 ELSE                                                     
040400                    IF MID-BEFDKRAV-S = SPACE                             
040500                       PERFORM DC-TA-BORT                                 
040600                       MOVE SPACE TO MOD-KDEMBAL-S                        
040700                                     MOD-IDSKYLT-S                        
040800                       MOVE MFS-ADD-LYS-UPP-FAELT                         
040900                            TO MOD-BEFDKRAV-S-ATTR                        
041000                               MOD-KDEMBAL-S-ATTR                         
041100                    ELSE                                                  
041200                       PERFORM DB-S-AENDRA                                
041300                    END-IF                                                
041400                 END-IF                                                   
041500                                                                          
041600             END-IF                                                       
041700          END-IF                                                          
041800                                                                          
041900       ELSE                                                               
042000         MOVE W-4535-IDHTYP-X TO WLXXKU01                                 
042100         PERFORM IMS-ISRT-XXKU-KU01                                       
042200                                                                          
042300           IF MID-BEFDKRAV-GB NOT = ALL '+' AND                           
042400              MID-BEFDKRAV-GB NOT = SPACE                                 
042500              PERFORM DA-GB-TILLAEGG                                      
042600           ELSE                                                           
042700              MOVE NEJ TO UPDATE-SW-GB                                    
042800           END-IF                                                         
042900           IF MID-BEFDKRAV-S NOT = ALL '+' AND                            
043000              MID-BEFDKRAV-S NOT = SPACE                                  
043100              PERFORM DA-S-TILLAEGG                                       
043200           ELSE                                                           
043300              MOVE NEJ TO UPDATE-SW-S                                     
043400           END-IF                                                         
043500       END-IF                                                             
043600                                                                          
043700       PERFORM IMS-GET-XXKU-KU01                                          
043800       PERFORM IMS-GNP-XXKU-KU11                                          
043900       IF SEGMENT-SAKNAS                                                  
044000          PERFORM DD-TA-BORT-ROT                                          
044100       END-IF                                                             
044200                                                                          
044300       IF UPDATE-SW-S = NEJ AND UPDATE-SW-GB = NEJ                        
044400          MOVE NEJ TO UPDATE-SW                                           
044500       END-IF                                                             
044600                                                                          
044700       IF UPDATE-OK                                                       
044800          PERFORM DG-INF-UPDATE-DONE                                      
044900       END-IF                                                             
045000     END-IF                                                               
045100     .                                                                    
045200     EJECT                                                                
045300 DA-S-TILLAEGG SECTION.                                                   
045400     MOVE 'S  '           TO 4536-IDSKYLT                                 
045500     MOVE LOW-VALUE       TO 4536-LOW-VALUE                               
045600     MOVE MID-BEFDKRAV-S  TO 4536-BEFDKRAV                                
045700     IF MID-KDEMBAL-S = '+'                                               
045800        MOVE SPACE        TO 4536-KDEMBAL                                 
045900     ELSE                                                                 
046000       MOVE MID-KDEMBAL-S TO 4536-KDEMBAL                                 
046100     END-IF                                                               
046200     PERFORM IMS-ISRT-XXKU-KU11                                           
046300     MOVE 'S  '           TO MOD-IDSKYLT-S                                
046400     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEFDKRAV-S-ATTR                    
046500                                   MOD-KDEMBAL-S-ATTR                     
046600     .                                                                    
046700     SKIP2                                                                
046800 DA-GB-TILLAEGG SECTION.                                                  
046900     MOVE 'GB '           TO 4536-IDSKYLT                                 
047000     MOVE LOW-VALUE       TO 4536-LOW-VALUE                               
047100     MOVE MID-BEFDKRAV-GB TO 4536-BEFDKRAV                                
047200     IF MID-KDEMBAL-GB = '+'                                              
047300        MOVE SPACE        TO 4536-KDEMBAL                                 
047400     ELSE                                                                 
047500      MOVE MID-KDEMBAL-GB TO 4536-KDEMBAL                                 
047600     END-IF                                                               
047700     PERFORM IMS-ISRT-XXKU-KU11                                           
047800     MOVE 'GB '           TO MOD-IDSKYLT-GB                               
047900     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEFDKRAV-GB-ATTR                   
048000                                   MOD-KDEMBAL-GB-ATTR                    
048100     .                                                                    
048200     SKIP2                                                                
048300 DB-S-AENDRA SECTION.                                                     
048400     IF MID-BEFDKRAV-S = ALL '+'                                          
048500        CONTINUE                                                          
048600     ELSE                                                                 
048700        MOVE MID-BEFDKRAV-S TO 4536-BEFDKRAV                              
048800     END-IF                                                               
048900     IF MID-KDEMBAL-S = '+'                                               
049000        CONTINUE                                                          
049100     ELSE                                                                 
049200       MOVE MID-KDEMBAL-S   TO 4536-KDEMBAL                               
049300     END-IF                                                               
049400     MOVE 'S  '             TO MOD-IDSKYLT-S                              
049500     PERFORM IMS-REPL-XXKU                                                
049600     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEFDKRAV-S-ATTR                    
049700                                   MOD-KDEMBAL-S-ATTR                     
049800      .                                                                   
049900      SKIP2                                                               
050000 DB-GB-AENDRA SECTION.                                                    
050100     IF MID-BEFDKRAV-GB = ALL '+'                                         
050200        CONTINUE                                                          
050300     ELSE                                                                 
050400        MOVE MID-BEFDKRAV-GB TO 4536-BEFDKRAV                             
050500     END-IF                                                               
050600     IF MID-KDEMBAL-GB = '+'                                              
050700        CONTINUE                                                          
050800     ELSE                                                                 
050900      MOVE MID-KDEMBAL-GB TO 4536-KDEMBAL                                 
051000     END-IF                                                               
051100     MOVE 'GB '           TO MOD-IDSKYLT-GB                               
051200     PERFORM IMS-REPL-XXKU                                                
051300     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEFDKRAV-GB-ATTR                   
051400                                   MOD-KDEMBAL-GB-ATTR                    
051500      .                                                                   
051600      EJECT                                                               
051700 DC-TA-BORT SECTION.                                                      
051800      PERFORM IMS-DLET-XXKU-KU11                                          
051900      .                                                                   
052000      SKIP2                                                               
052100 DD-TA-BORT-ROT SECTION.                                                  
052200      PERFORM IMS-GHU-XXKU-KU01                                           
052300      PERFORM IMS-DLET-XXKU-KU01                                          
052400      MOVE SPACE TO MOD-KDFDKRAV-UT                                       
052500      .                                                                   
052600      SKIP3                                                               
052700 DE-S-SAMMA-TEXT SECTION.                                                 
052800      PERFORM MFS-ROR-EJ-FAELT-IN-S                                       
052900      MOVE 'S  ' TO MOD-IDSKYLT-S                                         
053000      MOVE NEJ TO UPDATE-SW-S                                             
053100      .                                                                   
053200      SKIP2                                                               
053300 DE-GB-SAMMA-TEXT SECTION.                                                
053400      PERFORM MFS-ROR-EJ-FAELT-IN-GB                                      
053500      MOVE 'GB ' TO MOD-IDSKYLT-GB                                        
053600      MOVE NEJ   TO UPDATE-SW-GB                                          
053700      .                                                                   
053800      EJECT                                                               
053900 DF-ERR-NO-CHANGE SECTION.                                                
054000      IF MID-IDSKYLT-S NOT = SPACE                                        
054100         MOVE 'S  ' TO MOD-IDSKYLT-S                                      
054200      END-IF                                                              
054300      IF MID-IDSKYLT-GB NOT = SPACE                                       
054400         MOVE 'GB ' TO MOD-IDSKYLT-GB                                     
054500      END-IF                                                              
054600      MOVE ERR-NO-CHANGE TO MED-IDMFSFEL                                  
054700      CALL WMEDKONV USING MED-WMEDAREA                                    
054800      MOVE MED-MFSFEL TO MOD-TEMFSFEL                                     
054900      PERFORM MFS-ROR-EJ-FAELT-IN-S                                       
055000      PERFORM MFS-ROR-EJ-FAELT-IN-GB                                      
055100      .                                                                   
055200      SKIP2                                                               
055300 DG-INF-UPDATE-DONE SECTION.                                              
055400      MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                                
055500      CALL WMEDKONV USING MED-WMEDAREA                                    
055600      MOVE MED-MFSFEL TO MOD-TEMFSFEL                                     
055700      PERFORM MFS-ROR-EJ-FAELT-IN-S                                       
055800      PERFORM MFS-ROR-EJ-FAELT-IN-GB                                      
055900      .                                                                   
056000      EJECT                                                               
056100 MFS-RENSA-FAELT-UT SECTION.                                              
056200                                                                          
056300*    --- ALLA UTDATA-FÄLT                                                 
056400     MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-S                                
056500                             MOD-IDSKYLT-GB                               
056600                             MOD-BEFDKRAV-S                               
056700                             MOD-KDEMBAL-S                                
056800                             MOD-BEFDKRAV-GB                              
056900                             MOD-KDEMBAL-GB                               
057000     .                                                                    
057100     SKIP2                                                                
057200 MFS-ROR-EJ-FAELT-IN-S  SECTION.                                          
057300                                                                          
057400*    --- ALLA INDATA-FÄLT S                                               
057500     MOVE MFS-ROER-EJ-FAELT TO MOD-BEFDKRAV-S                             
057600                               MOD-KDEMBAL-S                              
057700     .                                                                    
057800     SKIP2                                                                
057900 MFS-ROR-EJ-FAELT-IN-GB SECTION.                                          
058000                                                                          
058100*    --- ALLA INDATA-FÄLT GB                                              
058200     MOVE MFS-ROER-EJ-FAELT TO MOD-BEFDKRAV-GB                            
058300                               MOD-KDEMBAL-GB                             
058400     .                                                                    
058500     EJECT                                                                
058600* --- IMS SEKTIONER ---                                                   
058700     SKIP3                                                                
058800 IMS-GET-MSG SECTION.                                                     
058900                                                                          
059000     MOVE '  QC' TO GODK-STATUSKODER                                      
059100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
059200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     SKIP3                                                                
059600 IMS-INSERT-MSG SECTION.                                                  
059700                                                                          
059800     IF ENGLISH-TEXT                                                      
059900       MOVE 'N' TO MFS-KDHUVOMR                                           
060000     END-IF                                                               
060100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
060200     MOVE SPACE TO GODK-STATUSKODER                                       
060300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
060400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060500     PERFORM IMS-STATUSKONTROLL                                           
060600     .                                                                    
060700     EJECT                                                                
060800 IMS-GET-XXKU-KU01 SECTION.                                               
060900                                                                          
061000     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
061100          DELIMITED BY SIZE INTO SSA1                                     
061200     MOVE '  GE' TO GODK-STATUSKODER                                      
061300     CALL CBLTDLI USING GU XXKU-PCB DLI-IO-AREA SSA1                      
061400     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
061500     PERFORM IMS-STATUSKONTROLL                                           
061600     .                                                                    
061700     SKIP2                                                                
061800 IMS-GHU-XXKU-KU01 SECTION.                                               
061900                                                                          
062000     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
062100          DELIMITED BY SIZE INTO SSA1                                     
062200     MOVE '  GE' TO GODK-STATUSKODER                                      
062300     CALL CBLTDLI USING GHU XXKU-PCB DLI-IO-AREA SSA1                     
062400     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     .                                                                    
062700     SKIP2                                                                
062800 IMS-ISRT-XXKU-KU01 SECTION.                                              
062900                                                                          
063000     MOVE 'WLXXKU01 ' TO SSA1                                             
063100     MOVE '  II' TO GODK-STATUSKODER                                      
063200     CALL CBLTDLI USING ISRT XXKU-PCB DLI-IO-AREA SSA1                    
063300     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063600     EJECT                                                                
063700 IMS-GHU-XXKU-KU11 SECTION.                                               
063800                                                                          
063900     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
064000          DELIMITED BY SIZE INTO SSA1                                     
064100     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
064200          DELIMITED BY SIZE INTO SSA2                                     
064300     MOVE '  GE' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING GHU XXKU-PCB DLI-IO-AREA SSA1 SSA2                
064500     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     SKIP2                                                                
064900 IMS-GNP-XXKU-KU11 SECTION.                                               
065000                                                                          
065100     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
065200          DELIMITED BY SIZE INTO SSA1                                     
065300     MOVE 'WLXXKU11 ' TO SSA2                                             
065400     MOVE '  GE' TO GODK-STATUSKODER                                      
065500     CALL CBLTDLI USING GNP XXKU-PCB DLI-IO-AREA SSA1 SSA2                
065600     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
065700     PERFORM IMS-STATUSKONTROLL                                           
065800     .                                                                    
065900     SKIP2                                                                
066000 IMS-ISRT-XXKU-KU11 SECTION.                                              
066100                                                                          
066200     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
066300          DELIMITED BY SIZE INTO SSA1                                     
066400     MOVE 'WLXXKU11 ' TO SSA2                                             
066500     MOVE '  II' TO GODK-STATUSKODER                                      
066600     CALL CBLTDLI USING ISRT XXKU-PCB DLI-IO-AREA SSA1 SSA2               
066700     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
066800     PERFORM IMS-STATUSKONTROLL                                           
066900     .                                                                    
067000     EJECT                                                                
067100 IMS-REPL-XXKU SECTION.                                                   
067200                                                                          
067300     MOVE '  ' TO GODK-STATUSKODER                                        
067400     CALL CBLTDLI USING REPL XXKU-PCB DLI-IO-AREA                         
067500     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     .                                                                    
067800     SKIP2                                                                
067900 IMS-DLET-XXKU-KU01 SECTION.                                              
068000                                                                          
068100     MOVE '  ' TO GODK-STATUSKODER                                        
068200     CALL CBLTDLI USING DLET XXKU-PCB DLI-IO-AREA                         
068300     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600     SKIP2                                                                
068700 IMS-DLET-XXKU-KU11 SECTION.                                              
068800                                                                          
068900     MOVE '  ' TO GODK-STATUSKODER                                        
069000     CALL CBLTDLI USING DLET XXKU-PCB DLI-IO-AREA                         
069100     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
069200     PERFORM IMS-STATUSKONTROLL                                           
069300     .                                                                    
069400     EJECT                                                                
069500 IMS-STATUSKONTROLL SECTION.                                              
069600                                                                          
069700     SET STATUS-IX TO 1                                                   
069800     SEARCH GODK-STATUS                                                   
069900       AT END CALL FELLOG                                                 
070000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
070100     END-SEARCH                                                           
070200     .                                                                    
