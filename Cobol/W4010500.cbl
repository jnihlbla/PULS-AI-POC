000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4010500.                                                
000300 AUTHOR.         KENETH GOUDE.                                            
000400 DATE-WRITTEN.   FEB 1980.                                                
000500 DATE-COMPILED.                                                           
000600*    FUNCTION.                                                            
000700*         TP-FRÅGE-PROGRAM WAREHOUSE C2 GRUNDBILD.                        
000800*                                                                         
000900*    INDATA.                                                              
001000*        TRANSAKTION: W4T105                                              
001100*        MID:         W4I10501                                            
001200*    UTDATA.                                                              
001300*        MOD:         W4O10501                                            
001400*    SUBPROGRAM.                                                          
001500*        FELLOG                                                           
001600*    SKIP2                                                                
001700*                                                                         
001800*   ÄNDRINGAR:                                                            
001900*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002000*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002100*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002200*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002300*                                                                         
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDARTNR-WS          PIC X(9).                                        
003200 77  MAX-MOD-LENGD       PIC S9(4)   COMP SYNC VALUE +507.                
003300 77  CL-INDEX            PIC S9(9)   COMP SYNC.                           
003400 77  IX                  PIC S9(9)   COMP SYNC.                           
003500 77  SPARAD-KDERS        PIC S9(3).                                       
003600 77  WS-IDTRANS                  PIC X(4).                                
003700     88  WS-GODKAEND-BILD      VALUE '4101' '4102' '4103' '4104'          
003800                                     '4105' '4106' '4107' '4108'.         
003900     SKIP2                                                                
004000 01  KONSTANTER.                                                          
004100     03  JA              PIC X       VALUE 'J'.                           
004200     03  NEJ             PIC X       VALUE 'N'.                           
004300     03  EJ-TEXT         PIC X       VALUE 'N'.                           
004400     EJECT                                                                
004500 01  NYCKEL-SW           PIC X.                                           
004600     88  NYCKLAR-OK                  VALUE 'J'.                           
004700     SKIP2                                                                
004800 01  ARTIKEL-SW          PIC X.                                           
004900     88  NY-ARTIKEL                  VALUE 'J'.                           
005000     SKIP2                                                                
005100 01  KVERS-SW            PIC X.                                           
005200     88  FLER-ERSETTNINGAR           VALUE 'J'.                           
005300     SKIP2                                                                
005310 01  DAGENS-DATUM             PIC 9(6)    VALUE ZERO.                     
005400                                                                          
005500 77  WS-IDLEVNR-8        PIC X(8)               VALUE SPACE.              
005600                                                                          
005700 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
005800     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
005900     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
006000                                                                          
006100 01  GENERELLA-SUBPROGAM.                                                 
006200     03  CBLTDLI           PIC X(8)    VALUE 'CBLTDLI '.                  
006300     03  FELLOG            PIC X(8)    VALUE 'FELLOG  '.                  
006400     03  W005INIT          PIC X(8)    VALUE 'W005INIT'.                  
006500     03  WMEDKONV          PIC X(8)    VALUE 'WMEDKONV'.                  
006600     EJECT                                                                
006700                                                                          
006800 01  NYCKLAR-TILL-DLI.                                                    
006900     03  W-IDARTNR-X.                                                     
007000         05  W-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.                   
007100     03  W-IDSKYLT-X.                                                     
007200         05  W-IDSKYLT   PIC X(3).                                        
007300     EJECT                                                                
007400                                                                          
007500 01  MEDDELANDEN.                                                         
007600     03  FEL-1           PIC X(35)                                        
007700         VALUE 'THIS PARTNO. IS NOT IN THE DATABASE'.                     
007800     03  FEL-2           PIC X(26)                                        
007900         VALUE 'PART NUMBER IS NOT NUMERIC'.                              
008000     03  FEL-3           PIC X(23)                                        
008100         VALUE 'THIS PARTNO. IS DELETED'.                                 
008200     03  FEL-4           PIC X(26)                                        
008300         VALUE 'THIS PARTNO. IS SUPERSEDED'.                              
008400     03  FLER-KVERS      PIC X(17)                                        
008500         VALUE 'MORE ITEMS EXISTS'.                                       
008600     03  RESDEL-J-TEXTER.                                                 
008700         05  FILLER      PIC X(3)    VALUE ' JA'.                         
008800         05  FILLER      PIC X(3)    VALUE 'YES'.                         
008900     03  FILLER REDEFINES RESDEL-J-TEXTER.                                
009000         05  JA-TEXT     PIC X(3) OCCURS 2.                               
009100     03  RESDEL-N-TEXTER.                                                 
009200         05  FILLER      PIC X(3)    VALUE 'NEJ'.                         
009300         05  FILLER      PIC X(3)    VALUE ' NO'.                         
009400     03  FILLER REDEFINES RESDEL-N-TEXTER.                                
009500         05  NEJ-TEXT    PIC X(3) OCCURS 2.                               
009600     EJECT                                                                
009700 01  MESSAGE-CODES.                                                       
009800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009900     03  CONFLICT                PIC X(3)    VALUE '002'.                 
010000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010100     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
010200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010300     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
010400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010500     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
010600     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
010700     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
010800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010900     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
011000     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
011100     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
011200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011300     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011600*01      -COPY WMEDAREA                                                   
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL W005INIT                                         
011900*01      -COPY WMSGINIT                                                   
012000     EJECT                                                                
012100*                        ****    TP-AREOR                                 
012200 01  FILLER              PIC X(16)   VALUE '    TP-AREAOR   '.            
012300     SKIP2                                                                
012400*01      MID -COPY W4I10501 -PRE MID-.                                    
012500     EJECT                                                                
012600*01      -COPY WMSGAREA.                                                  
012700     EJECT                                                                
012800*  03    MOD -COPY W4O10501 -PRE MOD- -RED MSG-AREA.                      
012900     EJECT                                                                
013000*01      -COPY WMFSAREA.                                                  
013100     EJECT                                                                
013200******************************************************************        
013300*****                                                                     
013400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*****                                                                     
013600 01  IMS-WS.                                                              
013700     03  FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
013800     SKIP2                                                                
013900*****                    **** STATUS-KOD FRÅN IMS                         
014000     03  STATUS-WS       PIC XX.                                          
014100         88  SEGMENT-FINNS           VALUE '  '.                          
014200         88  SEGMENT-SAKNAS          VALUE 'GE'.                          
014300     SKIP2                                                                
014400     03  GODK-STATUSKODER.                                                
014500         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
014600     SKIP2                                                                
014700 01  SSA1                PIC X(32).                                       
014800     EJECT                                                                
014900*                        *** IMS FUNKTIONSKODER ***                       
015000*01          -COPY   W0003                                                
015100     EJECT                                                                
015200*            *** DLI INPUT-OUTPUT AREA ***                                
015300 01  FILLER.                                                              
015400 03  DLI-IO-AREA         PIC X(150)  VALUE SPACE.                         
015500     EJECT                                                                
015600*03  WLBENA11     -COPY WDD311 -PRE BENA11- -RED DLI-IO-AREA.             
015700     EJECT                                                                
015800*03  WLERSA01     -COPY WDD701 -PRE ERSA01- -RED DLI-IO-AREA.             
015900     EJECT                                                                
016000*03  WLERSA11     -COPY WDD702 -PRE ERSA11- -RED DLI-IO-AREA.             
016100     EJECT                                                                
016200 01  DLI-IO-AREA-K601.                                                    
016300*03  WLARTC01     -COPY WDK601                                            
016400     EJECT                                                                
016500 01  DLI-IO-AREA-K611.                                                    
016600*03  WLARTC11     -COPY WDK611                                            
016700     EJECT                                                                
016800 LINKAGE SECTION.                                                         
016900*01               -COPY W0009  -PRE MSG-                                  
017000     EJECT                                                                
017100*01               -COPY W0008  -PRE USEA-.                                
017200         05  FILLER      PIC X.                                           
017300     EJECT                                                                
017400*01               -COPY W0008  -PRE BENA-.                                
017500         05  FILLER      PIC X.                                           
017600     EJECT                                                                
017700*01               -COPY W0008  -PRE ERSA-.                                
017800         05  FILLER      PIC X.                                           
017900     EJECT                                                                
018000*01               -COPY W0008  -PRE ARTC-.                                
018100         05  FILLER      PIC X.                                           
018200     EJECT                                                                
018300 PROCEDURE DIVISION USING  MSG-PCB USEA-PCB                               
018400                                   BENA-PCB ERSA-PCB ARTC-PCB.            
018500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
018600                                   BENA-PCB ERSA-PCB ARTC-PCB.            
018700                                                                          
018800*****************************************                                 
018900**   WLBENA  BENÄMNINGSREGISTER  WDD3  **                                 
019000**   WLERSA  ERSÄTTNINGSREGISTER WDD7  **                                 
019100**   WLARTC  ARTIKELREGISTER     WDK6  **                                 
019200*****************************************                                 
019300                                                                          
019400     EJECT                                                                
019500     PERFORM IMS-GET-MSG                                                  
019600     IF SEGMENT-FINNS                                                     
019700       PERFORM A-KOLLA-NYCKLAR                                            
019800       IF NYCKLAR-OK                                                      
019900         PERFORM S1-SECURITY-CHECK-PARTNO                                 
020000         IF PASSED-SECURITY-CHECK                                         
020100                                                                          
020200           PERFORM IMS-GET-WLARTC-ARTIKEL                                 
020300           IF SEGMENT-FINNS                                               
020400             IF NY-ARTIKEL                                                
020500               PERFORM B-REDIGERA-WLARTC-INFO                             
020600               PERFORM C-REDIGERA-WLBENA-INFO                             
020700               PERFORM D-REDIGERA-WLERSA-INFO                             
020800             ELSE                                                         
020900               PERFORM E-SPARA-GRUNDBILD                                  
021000               IF FLER-ERSETTNINGAR                                       
021100                 MOVE +11 TO SPARAD-KDERS                                 
021200                 PERFORM D-REDIGERA-WLERSA-INFO                           
021300               ELSE                                                       
021400                 PERFORM F-SPARA-GAMLA-ERS                                
021500               END-IF                                                     
021600             END-IF                                                       
021700           ELSE                                                           
021800             MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                          
021900             CALL WMEDKONV USING MED-WMEDAREA                             
022000             MOVE MED-TEMFSFEL TO MOD-MESSAGE-RAD1                        
022100           END-IF                                                         
022200         ELSE                                                             
022300*          --- USER NOT AUTHORIZED (ERROR MSG IN S1- SECTION)             
022400           CONTINUE                                                       
022500         END-IF                                                           
022600       ELSE                                                               
022700          IF NOT WS-GODKAEND-BILD                                         
022800             PERFORM G-RENSA-NYCKLAR                                      
022900          END-IF                                                          
023000       END-IF                                                             
023100       PERFORM IMS-INSERT-MSG                                             
023200     END-IF                                                               
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK                                                               
023500     .                                                                    
023600     EJECT                                                                
023700 A-KOLLA-NYCKLAR   SECTION.                                               
023800     SKIP2                                                                
023900     MOVE JA TO NYCKEL-SW                                                 
024000     MOVE NEJ TO ARTIKEL-SW KVERS-SW                                      
024100     SKIP2                                                                
024200     IF MSG-DUBBLA-TRANSKODER                                             
024300         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I10501               
024400         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
024500         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR CL-INDEX                     
024600         MOVE JA TO ARTIKEL-SW                                            
024700     ELSE                                                                 
024800         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I10501                
024900         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
025000         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR CL-INDEX                     
025100     END-IF                                                               
025200                                                                          
025300     MOVE MFS-IDTRANS        TO WS-IDTRANS                                
025400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
025500     MOVE '001'             TO MSGI-KDCALL                                
025600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025800     MOVE '4105'            TO MSGI-IDTRANS                               
025900                                                                          
026000     IF MFS-IDTRANS = '4105'                                              
026100       IF MID-IDARTNR-IN = ALL '+'                                        
026200         CONTINUE                                                         
026300       ELSE                                                               
026400         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
026500         MOVE ZERO TO MID-KVERS-SKIP                                      
026600       END-IF                                                             
026700     ELSE                                                                 
026800       MOVE ZERO TO MID-KVERS-SKIP                                        
026900       IF MID-IDARTNR-IN NUMERIC                                          
027000       AND MID-IDARTNR-IN > ZERO                                          
027100         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
027200       END-IF                                                             
027300     END-IF                                                               
027400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027500     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
027600     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
027700                                                                          
027800     IF IDARTNR-WS NUMERIC                                                
027900     AND IDARTNR-WS > ZERO                                                
028000       MOVE JA TO ARTIKEL-SW                                              
028100     END-IF                                                               
028200                                                                          
028300     IF MID-KVERS-SKIP NOT NUMERIC OR MID-KVERS-SKIP = SPACE              
028400       MOVE ZERO TO MID-KVERS-SKIP                                        
028500     END-IF                                                               
028600                                                                          
028700     IF MID-KVERS-SKIP NOT = ZERO                                         
028800       MOVE JA TO KVERS-SW                                                
028900     ELSE                                                                 
029000       MOVE JA TO ARTIKEL-SW                                              
029100     END-IF                                                               
029200                                                                          
029300     MOVE LOW-VALUE TO MOD-W4O10501                                       
029400     MOVE 'W4O105N1' TO MFS-IDMOD                                         
029500                                                                          
029600     IF ENGLISH-TEXT                                                      
029700       MOVE 'N' TO MFS-KDHUVOMR                                           
029800     END-IF                                                               
029900                                                                          
030000     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
030100                                                                          
030200     IF  MSGI-IDLAND-SPR = 'GB'                                           
030300         MOVE 2              TO CL-INDEX                                  
030400     ELSE                                                                 
030500         MOVE 1              TO CL-INDEX                                  
030600     END-IF                                                               
030700                                                                          
030800     MOVE '4105' TO MOD-IDTRANS                                           
030900     MOVE '-' TO MOD-STRECK                                               
031000     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
031100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
031200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
031300                                                                          
031400     IF IDARTNR-WS NOT NUMERIC                                            
031500         MOVE NEJ TO NYCKEL-SW                                            
031600         MOVE FEL-2 TO MOD-MESSAGE-RAD1                                   
031700     ELSE                                                                 
031800         MOVE IDARTNR-WS TO W-IDARTNR                                     
031900     END-IF                                                               
032000                                                                          
032100     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
032101                                                                          
032110     ACCEPT DAGENS-DATUM    FROM DATE                                     
032200     .                                                                    
032300     EJECT                                                                
032400 B-REDIGERA-WLARTC-INFO   SECTION.                                        
032500     SKIP2                                                                
032600*                            *** ARTIKELINFORMATION                       
032700     MOVE ART-REKSIFFR  TO MOD-REKSIFFR                                   
032800     MOVE ART-KDERS-UTG TO MOD-KDERS                                      
032900                           SPARAD-KDERS                                   
033000     MOVE ART-TIERSDAT  TO MOD-TIERSDAT                                   
033100     MOVE ART-IDLEVNR   TO MOD-IDLEVNR                                    
033200     MOVE ART-IDFKNGRP  TO MOD-IDFKNGRP                                   
033300     MOVE ART-KDSORT    TO MOD-KDSORT                                     
033400                                                                          
033500     IF ART-KDERS-UTG > ZERO                                              
033600       CONTINUE                                                           
033700     ELSE                                                                 
033800       PERFORM IMS-GET-WLARTC-CDC                                         
033900                                                                          
034000       MOVE CLAG-IDANSK TO MOD-IDBERED                                    
034100       MOVE CLAG-IDANSK TO MOD-IDANSK                                     
034200       MOVE CLAG-KDVVKL TO MOD-KDVVKL                                     
034300       IF CLAG-FLLSRDEL = 'J'                                             
034400           MOVE JA-TEXT (CL-INDEX) TO MOD-FLLSRDEL                        
034500       ELSE                                                               
034600           MOVE NEJ-TEXT (CL-INDEX) TO MOD-FLLSRDEL                       
034700       END-IF                                                             
034800       MOVE CLAG-KDGK   TO MOD-KDGK                                       
034900       MOVE CLAG-KDLEVSP TO MOD-KDLEVSP                                   
035000       MOVE CLAG-KDLTK  TO MOD-KDLTK                                      
035100       MOVE CLAG-KDARTURS TO MOD-KDARTURS                                 
035110       PERFORM S20-HAMTA-FLPCOO                                           
035300       MOVE CLAG-KDVSOP TO MOD-KDVSOP                                     
035400       MOVE CLAG-VKART  TO MOD-VKART                                      
035500       MOVE CLAG-VLARTNTO TO MOD-VLARTNTO                                 
035600       MOVE ZERO        TO MOD-KDSPARR                                    
035700       PERFORM ARTC-REDIGERA-WLARTC-KDSPARR                               
035800       MOVE CLAG-KDERS  TO MOD-KDERS                                      
035900                             SPARAD-KDERS                                 
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 ARTC-REDIGERA-WLARTC-KDSPARR SECTION.                                    
036400     SKIP2                                                                
036500*                            *** SPÄRRKOD                                 
036600     IF CLAG-KDUART = 'M'                                                 
036700       MOVE 6 TO MOD-KDSPARR                                              
036800     ELSE                                                                 
036900       IF CLAG-KDUART = 'S'                                               
037000         MOVE 4 TO MOD-KDSPARR                                            
037100       ELSE                                                               
037200         IF CLAG-FLLSRDEL = 'N'                                           
037300           MOVE 3 TO MOD-KDSPARR                                          
037400         ELSE                                                             
037500           IF CLAG-KDLEVSP = 20                                           
037600             MOVE 2 TO MOD-KDSPARR                                        
037700           ELSE                                                           
037800             IF SWEDISH-TEXT                                              
037900               IF CLAG-KDLEVSP = 21                                       
038000                 MOVE 2 TO MOD-KDSPARR                                    
038100               END-IF                                                     
038200             ELSE                                                         
038300               IF ENGLISH-TEXT                                            
038400                 IF CLAG-KDLEVSP = 22                                     
038500                   MOVE 2 TO MOD-KDSPARR                                  
038600                 ELSE                                                     
038700                   IF (CLAG-KDLTK = +1 OR +4) OR                          
038800                     CLAG-FLLTKSP = 'J'                                   
038900                     MOVE 1 TO MOD-KDSPARR                                
039000                   END-IF                                                 
039100                 END-IF                                                   
039200               END-IF                                                     
039300             END-IF                                                       
039400           END-IF                                                         
039500         END-IF                                                           
039600       END-IF                                                             
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 C-REDIGERA-WLBENA-INFO SECTION.                                          
040100     PERFORM IMS-BENA01-LASGU-ROTSEG                                      
040200     MOVE 'GB ' TO        W-IDSKYLT                                       
040300     PERFORM IMS-BENA11-LASGNP-TEXTSEG                                    
040400     IF SEGMENT-FINNS                                                     
040500         MOVE BENA11-TEXT-BEART TO MOD-BEART-ENG                          
040600     ELSE                                                                 
040700         MOVE SPACE TO MOD-BEART-ENG                                      
040800     END-IF                                                               
040900     MOVE 'S  ' TO        W-IDSKYLT                                       
041000     PERFORM IMS-BENA11-LASGNP-TEXTSEG                                    
041100     IF SEGMENT-FINNS                                                     
041200         MOVE BENA11-TEXT-BEART TO MOD-BEART-SVE                          
041300     ELSE                                                                 
041400         MOVE SPACE TO MOD-BEART-SVE                                      
041500     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 D-REDIGERA-WLERSA-INFO   SECTION.                                        
041900     SKIP2                                                                
042000*                            *** ERSÄTTNINGAR                             
042100     IF SPARAD-KDERS > 10                                                 
042200       IF SPARAD-KDERS = 29                                               
042300         MOVE FEL-3 TO MOD-MESSAGE-RAD1                                   
042400       ELSE                                                               
042500         MOVE FEL-4 TO MOD-MESSAGE-RAD1                                   
042600         MOVE MID-KVERS-SKIP TO IX                                        
042700         PERFORM IMS-GET-WLERSA-ARTIKEL                                   
042800         IF SEGMENT-FINNS                                                 
042900           MOVE ERSA01-DIERS-ERS TO MOD-DIERS-ERS                         
043000           PERFORM IMS-GET-WLERSA-ERSETTNING                              
043100           PERFORM UNTIL SEGMENT-SAKNAS OR IX NOT > 0                     
043200             SUBTRACT +1 FROM IX                                          
043300             PERFORM IMS-GET-WLERSA-ERSETTNING                            
043400           END-PERFORM                                                    
043500           IF SEGMENT-FINNS                                               
043600             MOVE +1 TO IX                                                
043700             PERFORM UNTIL SEGMENT-SAKNAS OR IX NOT < +13                 
043800               IF ERSA11-FLTEXT = EJ-TEXT                                 
043900                 MOVE ERSA11-IDARTNR-TILLK TO                             
044000                             MOD-IDARTNR-TILLK (IX)                       
044100                 MOVE ERSA11-DIERS-TILLK TO MOD-DIERS-TILLK (IX)          
044200               ELSE                                                       
044300                 MOVE ERSA11-BEERS TO MOD-BEERS (IX)                      
044400               END-IF                                                     
044500               ADD +1 TO IX                                               
044600               ADD +1 TO MID-KVERS-SKIP                                   
044700               PERFORM IMS-GET-WLERSA-ERSETTNING                          
044800             END-PERFORM                                                  
044900             IF IX = +13                                                  
045000               MOVE FLER-KVERS TO MOD-MESSAGE-RAD23                       
045100               SUBTRACT +1 FROM MID-KVERS-SKIP                            
045200               MOVE MID-KVERS-SKIP TO MOD-KVERS-SKIP                      
045300             ELSE                                                         
045400               MOVE ZERO TO MOD-KVERS-SKIP                                
045500             END-IF                                                       
045600           END-IF                                                         
045700         END-IF                                                           
045800       END-IF                                                             
045900     ELSE                                                                 
046000       MOVE ZERO TO MOD-KVERS-SKIP                                        
046100     END-IF                                                               
046200     .                                                                    
046300     EJECT                                                                
046400 E-SPARA-GRUNDBILD  SECTION.                                              
046500     SKIP2                                                                
046600     MOVE MFS-ROER-EJ-FAELT TO MOD-MESSAGE-RAD1                           
046700                               MOD-IDARTNR-UT                             
046800                               MOD-TIERSDAT                               
046900                               MOD-KDERS                                  
047000                               MOD-IDBERED                                
047100                               MOD-IDANSK                                 
047200                               MOD-IDLEVNR                                
047300                               MOD-KDVVKL                                 
047400                               MOD-KDSPARR                                
047500                               MOD-KDGK                                   
047600                               MOD-KDLTK                                  
047700                               MOD-FLLSRDEL                               
047800                               MOD-KDLEVSP                                
047900                               MOD-BEART-SVE                              
048000                               MOD-BEART-ENG                              
048100                               MOD-VLARTNTO                               
048200                               MOD-VKART                                  
048300                               MOD-KDSORT                                 
048400                               MOD-IDFKNGRP                               
048500                               MOD-KDVSOP                                 
048600                               MOD-KDARTURS                               
048610                               MOD-FLPCOO                                 
048700     .                                                                    
048800     EJECT                                                                
048900 F-SPARA-GAMLA-ERS  SECTION.                                              
049000     SKIP2                                                                
049100     MOVE MFS-ROER-EJ-FAELT TO MOD-MESSAGE-RAD23                          
049200     MOVE +1 TO IX                                                        
049300     PERFORM UNTIL IX NOT < +13                                           
049400       MOVE MFS-ROER-EJ-FAELT TO MOD-BEERS (IX)                           
049500       ADD +1 TO IX                                                       
049600     END-PERFORM                                                          
049700     .                                                                    
049800     EJECT                                                                
049900 G-RENSA-NYCKLAR SECTION.                                                 
050000     MOVE MFS-RENSA-FAELT           TO  MOD-IDARTNR-UT                    
050100     .                                                                    
050200     EJECT                                                                
050300                                                                          
050400 S1-SECURITY-CHECK-PARTNO SECTION.                                        
050500     SKIP2                                                                
050600*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
050700     PERFORM IMS-GET-WLARTC-ARTIKEL                                       
050800     IF  SEGMENT-FINNS                                                    
050900       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
051000       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
051100       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
051200*        --- USER AUTHORIZED                                              
051300         SET PASSED-SECURITY-CHECK TO TRUE                                
051400       ELSE                                                               
051500*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
051600         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
051700         CALL WMEDKONV USING MED-WMEDAREA                                 
051800         MOVE MED-TEMFSFEL TO MOD-MESSAGE-RAD1                            
051900                                                                          
052000         SET BLOCKED-SECURITY-CHECK TO TRUE                               
052100       END-IF                                                             
052200     ELSE                                                                 
052300         MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                              
052400         CALL WMEDKONV USING MED-WMEDAREA                                 
052500         MOVE MED-TEMFSFEL TO MOD-MESSAGE-RAD1                            
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
052910 S20-HAMTA-FLPCOO          SECTION.                                       
052920     IF CLAG-KDPCOO > ' '                                                 
052930       IF DAGENS-DATUM > CLAG-TIGILTIG-PCOO                               
052940         MOVE JA  TO MOD-FLPCOO                                           
052950       ELSE                                                               
052960         MOVE NEJ TO MOD-FLPCOO                                           
052970       END-IF                                                             
052980     ELSE                                                                 
052990       MOVE JA    TO MOD-FLPCOO                                           
052991     END-IF                                                               
052992     IF MOD-FLPCOO = 'J'                                                  
052993       MOVE '*' TO MOD-FLPCOO                                             
052994     ELSE                                                                 
052995       MOVE ' ' TO MOD-FLPCOO                                             
052996     END-IF                                                               
052997     .                                                                    
052998     EJECT                                                                
052999                                                                          
053000 IMS-GET-MSG SECTION.                                                     
053100     MOVE '  QC' TO GODK-STATUSKODER                                      
053200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
053300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053400     PERFORM IMS-STATUS-KONTROLL                                          
053500     SKIP2                                                                
053600     .                                                                    
053700 IMS-INSERT-MSG SECTION.                                                  
053800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
053900     MOVE SPACE TO GODK-STATUSKODER                                       
054000     CALL CBLTDLI USING ISRT MSG-PCB                                      
054100                          MSG-IO-AREA MFS-IDMOD                           
054200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
054300     PERFORM IMS-STATUS-KONTROLL                                          
054400     .                                                                    
054500     EJECT                                                                
054600 IMS-BENA01-LASGU-ROTSEG SECTION.                                         
054700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
054800             DELIMITED BY SIZE INTO SSA1                                  
054900     MOVE '  ' TO GODK-STATUSKODER                                        
055000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
055100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
055200     PERFORM IMS-STATUS-KONTROLL                                          
055300     SKIP2                                                                
055400     .                                                                    
055500 IMS-BENA11-LASGNP-TEXTSEG SECTION.                                       
055600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
055700             DELIMITED BY SIZE INTO SSA1                                  
055800     MOVE '  GE' TO GODK-STATUSKODER                                      
055900     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
056000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
056100     PERFORM IMS-STATUS-KONTROLL                                          
056200     .                                                                    
056300     EJECT                                                                
056400 IMS-GET-WLARTC-ARTIKEL  SECTION.                                         
056500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
056600            DELIMITED BY SIZE INTO SSA1                                   
056700     MOVE '  GE' TO GODK-STATUSKODER                                      
056800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-K601 SSA1                 
056900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057000     PERFORM IMS-STATUS-KONTROLL                                          
057100     SKIP2                                                                
057200     .                                                                    
057300 IMS-GET-WLARTC-CDC  SECTION.                                             
057400     MOVE 'WLARTC11 ' TO SSA1                                             
057500     MOVE '  GE' TO GODK-STATUSKODER                                      
057600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-K611 SSA1                
057700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057800     PERFORM IMS-STATUS-KONTROLL                                          
057900     EJECT                                                                
058000     .                                                                    
058100 IMS-GET-WLERSA-ARTIKEL     SECTION.                                      
058200     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
058300            DELIMITED BY SIZE INTO SSA1                                   
058400     MOVE '  GE' TO GODK-STATUSKODER                                      
058500     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
058600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
058700     PERFORM IMS-STATUS-KONTROLL                                          
058800     SKIP3                                                                
058900     .                                                                    
059000 IMS-GET-WLERSA-ERSETTNING  SECTION.                                      
059100     MOVE 'WLERSA11 ' TO SSA1                                             
059200     MOVE '  GE' TO GODK-STATUSKODER                                      
059300     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
059400     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUS-KONTROLL                                          
059600     SKIP2                                                                
059700     .                                                                    
059800 IMS-STATUS-KONTROLL SECTION.                                             
059900     SET STATUS-IX TO 1                                                   
060000     SEARCH GODK-STATUS                                                   
060100       AT END                                                             
060200         CALL FELLOG                                                      
060300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060400         CONTINUE                                                         
060500     END-SEARCH                                                           
060600     .                                                                    
