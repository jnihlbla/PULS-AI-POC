000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1023100.                                                
000400 AUTHOR.         PAUL WACHSBERGER.                                        
000500 DATE-WRITTEN.   APRIL 1990.                                              
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION.                                                            
001100*    PROGRAMMET                                                           
001200*        SKALL VISA SAMTLIGA NYA ELLER ÄNDRADE SATSER                     
001300*                                                                         
001400*        GER MÖJLIGHET ATT VÄLJA ETT SATSNUMMER OCH MED                   
001500*        PF-TANGENT GÅ TILL BILD 'VISA-STRUKTURRAD'                       
001600*                                                                         
001700*        FRÅGEPROGRAMM W1023100 LÄSER FÖLJANDE MPP:ER:                    
001800*                                                                         
001900*                              WLSATC01 (WDJ1)                            
002000*                              WLSATB01 (WDJ1)                            
002200*                              WLBENA01 (WDD3)                            
002300*                              WLARTC01 (WDK6)                            
002400*                              WLARTC11 (WDK6)                            
002500*                                                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W1T231                                              
002900*        MID:         W1I23101                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W1O23101                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003801                                                                          
003810*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W1023100'.            
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004700                                                                          
004800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005000                                                                          
005100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X.                                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  SKICKA-TRANS-SW             PIC X       VALUE 'J'.                   
005800     88  SKICKA-TRANS                        VALUE 'J'.                   
005900     88  SKICKA-EJ-TRANS                     VALUE 'N'.                   
006000                                                                          
006100 77  ALLT-SW                     PIC X.                                   
006200     88  ALLT-OK                             VALUE 'J'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  EGEN-MID                            VALUE '1231'.                
006600     88  GODK-MID                            VALUE '1231' '1232'          
006700                                                   '1233' '1234'.         
006800 77  WS-LEV                      PIC X(1).                                
006900 77  WS-KDPRODSL                 PIC 9(2).                                
007000 77  WS-SELECT                   PIC X.                                   
007100 77  W-IDANSK                    PIC S9(3).                               
007200 77  WS-SATB01-LEV               PIC S9(5)   COMP-3.                      
007300 77  W-TIREGDAT                  PIC S9(7).                               
007400 77  W-TIUPPDAT                  PIC S9(7).                               
007500 77  W-SATSSTATUS                PIC X(1).                                
007600 77  WS-SATSSTATUS               PIC X(1).                                
007700                                                                          
007800                                                                          
007900                                                                          
008000     EJECT                                                                
008100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008200 01  GENERELLA-SUBPROGRAM.                                                
008300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008800*   -COPY WMEDAREA                                                        
009000     SKIP3                                                                
009100 01  MESSAGE-CODES.                                                       
009200     03  INF-PRESS-PF9           PIC X(3)    VALUE '004'.                 
009300     03  MSG-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
009400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009700     EJECT                                                                
009800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'WWLAND-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WWLAND03                                                       
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010600     SKIP3                                                                
010700*01  MID -COPY W1I23101                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011100     SKIP3                                                                
011200*01  -COPY WMSGAREA                                                       
011400     EJECT                                                                
011500*    03  MOD -COPY W1O23101   -RED MSG-AREA.                              
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
012000 01  W-PROG-TO-PROG-SW.                                                   
012100     03    M-SW-LL               PIC S9(4)  VALUE +131 COMP SYNC.         
012200     03    M-SW-Z1-Z2            PIC X(02)  VALUE LOW-VALUE.              
012300     03    M-SW-KDTRANS          PIC X(08)  VALUE 'W1T213  '.             
012400     03    M-SW-IDTRANS          PIC X(04)  VALUE '1231'.                 
012500     03    M-SW-KDMFSTYP         PIC X(01)  VALUE '1'.                    
012600*03  -COPY W1I21301         -PRE 1213-                                    
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013000     SKIP3                                                                
013100*01  -COPY WMFSAREA                                                       
013300     EJECT                                                                
013400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*                                                                         
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP3                                                                
013800 01  NYCKLAR-TILL-DLI.                                                    
013900     03  W-WDJ1A1KY-X.                                                    
014000         05  W-WDJ1A1KY          PIC S9(09)   VALUE ZERO COMP-3.          
014100     03  W-WDJ111KY-X.                                                    
014200         05  W-WDJ111KY          PIC X(04)    VALUE SPACE.                
014300     03  W-IDARTNR-X.                                                     
014400         05  W-IDARTNR           PIC S9(09)   VALUE ZERO COMP-3.          
014500     03  W-IDARTNR-XX.                                                    
014600         05  W-IDARTNR1          PIC S9(09)   VALUE ZERO COMP-3.          
014700     03  W-IDSKYLT-X.                                                     
014800         05  WS-IDSKYLT           PIC X(03)    VALUE SPACE.               
014900     03  W-KDSEGKEY-X.                                                    
015000         05  W-KDSEGKEY          PIC X(01)    VALUE '1'.                  
015100*    --- STATUS-KOD FRÅN IMS                                              
015200 01  STATUS-WS                   PIC XX.                                  
015300     88  SEGMENT-FINNS                       VALUE '  '.                  
015400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015600     88  BASEN-SLUT                          VALUE 'GB'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016700     EJECT                                                                
016800*    ---  DLI INPUT-OUTPUT AREA                                           
016900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017000     SKIP3                                                                
017100 01  DLI-IO-AREA.                                                         
017200     03  IO-AREA                 PIC X(928)  VALUE SPACE.                 
017300     SKIP3                                                                
017400     03  WLSATC01 REDEFINES IO-AREA.                                      
017500*        05  -COPY WDJ1A1     -PRE SATC01-                                
017700     SKIP3                                                                
017800     03  WLSATB01 REDEFINES IO-AREA.                                      
017900*        05  -COPY WDJ101     -PRE SATB01-                                
018100     SKIP3                                                                
018600     03  WLBENA01 REDEFINES IO-AREA.                                      
018700*        05  -COPY WDD311     -PRE BENA01-                                
018900     EJECT                                                                
019000     03  WLARTC01 REDEFINES IO-AREA.                                      
019100*        05  -COPY WDK601                                                 
019300     EJECT                                                                
019400     03  WLARTC11 REDEFINES IO-AREA.                                      
019500*        05  -COPY WDK611                                                 
019700     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900                                                                          
020000*01  -COPY W0009      -PRE MSG-                                           
020200     EJECT                                                                
020300*01  -COPY W0009      -PRE ALT-                                           
020500     EJECT                                                                
020600*01  -COPY W0008      -PRE SATC-                                          
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000*01  -COPY W0008      -PRE SATB-                                          
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021800*01  -COPY W0008      -PRE BENA-                                          
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008      -PRE ARTC-                                          
022400     05  FILLER                  PIC X.                                   
022500                                                                          
022600     EJECT                                                                
022700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB SATC-PCB SATB-PCB              
022800                           BENA-PCB ARTC-PCB.                             
022900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB SATC-PCB SATB-PCB              
023000                           BENA-PCB ARTC-PCB.                             
023100                                                                          
023200     PERFORM IMS-GET-MSG                                                  
023300     IF SEGMENT-FINNS                                                     
023400       PERFORM A-INIT                                                     
023500       PERFORM B-KOLLA-NYCKLAR-PFTRYCK                                    
023600       IF NYCKLAR-OK                                                      
023700          IF MFS-IDPFK = '7'                                              
023800             CONTINUE                                                     
023900          ELSE                                                            
024000             PERFORM F-KOLLA-SELECT                                       
024100          END-IF                                                          
024200          IF ALLT-OK                                                      
024300             IF MFS-IDPFK = '9'                                           
024400                CONTINUE                                                  
024500             ELSE                                                         
024600                PERFORM G-LAES-VISA-INFO                                  
024700             END-IF                                                       
024800          ELSE                                                            
024900             PERFORM MFS-ROER-EJ-FAELT-UT                                 
025000             PERFORM MFS-ADD-LAES-IN-FAELT-IGEN                           
025100             MOVE 'TRYCK PF9 VID SELECT' TO MOD-TEMFSFEL                  
025200          END-IF                                                          
025300       ELSE                                                               
025400         MOVE '401' TO MED-IDMFSFEL                                       
025500         CALL WMEDKONV USING MED-WMEDAREA                                 
025600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
025700         PERFORM MFS-FORM-ATTR                                            
025800       END-IF                                                             
025900     ELSE                                                                 
026000         PERFORM MFS-RENSA-FAELT-UT                                       
026100         PERFORM MFS-FORM-ATTR                                            
026200     END-IF                                                               
026300     IF NOT MFS-IDPFK = '9'                                               
026400         MOVE LENGTH OF MOD-W1O23101 TO MSG-KVLL                          
026410         ADD            +4           TO MSG-KVLL                          
026500         PERFORM IMS-INSERT-MSG                                           
026600     END-IF                                                               
026700     MOVE ZERO TO RETURN-CODE                                             
026800     GOBACK                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 A-INIT SECTION.                                                          
027200                                                                          
027300     IF MSG-DUBBLA-TRANSKODER                                             
027400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I23101                 
027500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
027600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
027700       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
027800       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
027900     ELSE                                                                 
028000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I23101                 
028100       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
028200       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
028300       MOVE SPACE                         TO MFS-IDPFK                    
028400                                             MFS-KDTRTYP                  
028500     END-IF                                                               
028600     MOVE LOW-VALUE                       TO MSG-AREA                     
028700     MOVE 'W1O231N1'                      TO MFS-IDMOD                    
028800     MOVE '1231'                          TO MOD-IDTRANS                  
028900     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
029000     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
029100                                             MOD-TEMFSINF                 
029200                                             MOD-LEV-IN                   
029300                                             MOD-KDPRODSL-IN              
029400                                             MOD-IDSKYLT-IN               
029500     MOVE JA                              TO ALLT-SW                      
029600     MOVE NEJ                             TO SKICKA-TRANS-SW              
029700     IF NOT EGEN-MID                                                      
029800       MOVE SPACE TO MFS-KDTRTYP                                          
029900       MOVE '7' TO MFS-IDPFK                                              
030000       MOVE SPACE                         TO MID-LEV-UT                   
030100       MOVE ZERO                          TO MID-KDPRODSL-UT              
030200       MOVE SPACE                         TO MID-IDSKYLT-UT               
030300     END-IF                                                               
030400                                                                          
030500     IF ENGLISH-TEXT                                                      
030600       MOVE +2                            TO SPRAK-IX                     
030700       MOVE 'GB '                         TO MED-IDSKYLT                  
030800     ELSE                                                                 
030900       MOVE +1                            TO SPRAK-IX                     
031000       MOVE 'S  '                         TO MED-IDSKYLT                  
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 B-KOLLA-NYCKLAR-PFTRYCK SECTION.                                         
031500     MOVE JA                            TO NYCKLAR-SW                     
031600* KONTROLL AV LEV,PRODSL OCH SPRÅK                                        
031700                                                                          
031800     PERFORM BA-KOLLA-LEV                                                 
031900                                                                          
032000     PERFORM BB-KOLLA-PRODSL                                              
032100                                                                          
032200     PERFORM BC-KOLLA-SPRAK                                               
032300                                                                          
032400     IF MFS-IDPFK = '7'                                                   
032500        MOVE INF-FIRST-PAGE             TO MED-IDMFSFEL                   
032600        CALL WMEDKONV USING MED-WMEDAREA                                  
032700        MOVE MED-MFSFEL                 TO MOD-TEMFSFEL                   
032800        MOVE ZERO                       TO W-IDARTNR1                     
032900     ELSE                                                                 
033000        IF MFS-IDPFK = '8'                                                
033100           MOVE MID-IDARTNR-STR-NEXT    TO W-IDARTNR1                     
033200        ELSE                                                              
033300           MOVE MID-IDARTNR-STR-ENTER   TO W-IDARTNR1                     
033400        END-IF                                                            
033500     END-IF                                                               
033600                                                                          
033700     .                                                                    
033800     EJECT                                                                
033900 BA-KOLLA-LEV SECTION.                                                    
034000                                                                          
034100     IF MID-LEV-IN = ALL '+'                                              
034200        MOVE MID-LEV-UT                TO WS-LEV                          
034300     ELSE                                                                 
034400        MOVE MID-LEV-IN                TO WS-LEV                          
034500        MOVE '7'                       TO MFS-IDPFK                       
034600        MOVE SPACE                     TO MFS-KDTRTYP                     
034700     END-IF                                                               
034701                                                                          
034702     IF NOT EGEN-MID                                                      
034703       MOVE 'I' TO WS-LEV                                                 
034704     END-IF                                                               
034710                                                                          
034800     IF WS-LEV    = 'E' OR 'I' OR ' '                                     
034900        CONTINUE                                                          
035000     ELSE                                                                 
035100        MOVE 'N'                       TO NYCKLAR-SW                      
035200     END-IF                                                               
035300     MOVE WS-LEV                       TO MOD-LEV-UT                      
035400     .                                                                    
035500     EJECT                                                                
035600 BB-KOLLA-PRODSL SECTION.                                                 
035700                                                                          
035800     IF MID-KDPRODSL-IN = ALL '+'                                         
035900             MOVE MID-KDPRODSL-UT      TO WS-KDPRODSL                     
036000       INSPECT WS-KDPRODSL  REPLACING LEADING                             
036100                                           SPACE BY ZERO                  
036200     ELSE                                                                 
036300             MOVE '7'                  TO MFS-IDPFK                       
036400             MOVE MID-KDPRODSL-IN      TO WS-KDPRODSL                     
036500             MOVE SPACE                TO MFS-KDTRTYP                     
036600     END-IF                                                               
036700     IF  WS-KDPRODSL NUMERIC                                              
036800            CONTINUE                                                      
036900     ELSE                                                                 
037000        MOVE 'N'                   TO NYCKLAR-SW                          
037100     END-IF                                                               
037200     MOVE WS-KDPRODSL              TO MOD-KDPRODSL-UT                     
037300     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO  BY SPACE             
037400                                                                          
037500     .                                                                    
037600     EJECT                                                                
037700 BC-KOLLA-SPRAK SECTION.                                                  
037800                                                                          
037900     IF MID-IDSKYLT-IN = ALL '+'                                          
038000             MOVE MID-IDSKYLT-UT       TO WS-IDSKYLT                      
038100     ELSE                                                                 
038200             MOVE MID-IDSKYLT-IN       TO WS-IDSKYLT                      
038300             MOVE '7'                  TO MFS-IDPFK                       
038400             MOVE SPACE                TO MFS-KDTRTYP                     
038500     END-IF                                                               
038600     IF WS-IDSKYLT  = SPACE                                               
038700        MOVE 'S  '                     TO WS-IDSKYLT                      
038800     END-IF                                                               
038900     MOVE WS-IDSKYLT                   TO MOD-IDSKYLT-UT                  
039000         SET WWLAND03-IX TO +1                                            
039100         SEARCH WWLAND03-IDSKYLT-RAD                                      
039200           AT END                                                         
039300             MOVE 'N'             TO NYCKLAR-SW                           
039400             MOVE '401'           TO MOD-TEMFSFEL                         
039500           WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT                
039600              CONTINUE                                                    
039700         END-SEARCH                                                       
039800     .                                                                    
039900     EJECT                                                                
040000 F-KOLLA-SELECT SECTION.                                                  
040100                                                                          
040200     MOVE NEJ                      TO SKICKA-TRANS-SW                     
040300     MOVE +1                       TO INDX                                
040400     PERFORM UNTIL INDX > 14 OR SKICKA-TRANS                              
040500                                                                          
040600         IF MID-SELECT (INDX)  NOT = ALL '+'                              
040700            MOVE JA                TO SKICKA-TRANS-SW                     
040800            IF MFS-IDPFK = '9'                                            
040900                PERFORM FA-BEHANDL-MID-OCH-SKICKA                         
041000            ELSE                                                          
041100                MOVE NEJ           TO ALLT-SW                             
041200            END-IF                                                        
041300         ELSE                                                             
041400             ADD  +1               TO INDX                                
041500         END-IF                                                           
041600     END-PERFORM                                                          
041700     IF MFS-IDPFK = '9' AND SKICKA-TRANS-SW = NEJ                         
041800         MOVE SPACE                TO MFS-IDPFK                           
041900     END-IF                                                               
042000                                                                          
042100     .                                                                    
042200     EJECT                                                                
042300 FA-BEHANDL-MID-OCH-SKICKA SECTION.                                       
042400*           PF9 + SELECT                                                  
042500                                                                          
042600                                                                          
042700       INSPECT MID-IDARTNR-STR (INDX) REPLACING LEADING                   
042800                                           SPACE BY ZERO                  
042900       MOVE LOW-VALUE               TO 1213-MID-W1I21301                  
043000       MOVE MID-IDARTNR-STR (INDX)  TO 1213-MID-IDARTNR-IN                
043100       MOVE WS-IDSKYLT              TO 1213-MID-IDSKYLT-IN                
043200       MOVE ZERO                    TO 1213-MID-IDRADNR-IN                
043300       PERFORM IMS-ISRT-ALT-MSG                                           
043400     .                                                                    
043500     EJECT                                                                
043600 G-LAES-VISA-INFO SECTION.                                                
043700                                                                          
043800     MOVE +1                           TO INDX                            
043900     PERFORM IMS-GN-SATC-STR                                              
044000     PERFORM UNTIL INDX > 14                                              
044100        IF SEGMENT-FINNS                                                  
044200           IF SATC01-SEQA-IDARTNR < 100000000                             
044300              PERFORM   GA-LAES-GRUNDDATA                                 
044400           END-IF                                                         
044500           PERFORM IMS-GN-SATC-STR                                        
044600        ELSE                                                              
044700           IF INDX = 1                                                    
044800              MOVE '005'                TO MED-IDMFSFEL                   
044900              CALL WMEDKONV USING MED-WMEDAREA                            
045000              MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                   
045100           END-IF                                                         
045200           PERFORM S01-RENSA-RAD                                          
045300        END-IF                                                            
045400     END-PERFORM                                                          
045500     IF SEGMENT-FINNS                                                     
045600         MOVE SATC01-SEQA-IDARTNR      TO                                 
045700                                     MOD-IDARTNR-STR-NEXT                 
045800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
045900         CALL WMEDKONV USING MED-WMEDAREA                                 
046000         MOVE MED-TEMFSFEL             TO MOD-TEMFSINF                    
046100     ELSE                                                                 
046200         MOVE ZERO                     TO                                 
046300                                     MOD-IDARTNR-STR-NEXT                 
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 GA-LAES-GRUNDDATA SECTION.                                               
046800                                                                          
046900     MOVE SATC01-SEQA-IDARTNR        TO W-IDARTNR                         
047000     PERFORM IMS-GU-SATB-ART                                              
047100     IF SEGMENT-FINNS                                                     
047200       IF  WS-LEV = 'I'   AND  SATB01-STR-IDLEVNR     = '1002 '           
047300         OR  (WS-LEV = 'E' AND SATB01-STR-IDLEVNR NOT = '1002 ')          
047400         OR  (WS-LEV = ' ')                                               
047500         PERFORM   FAA-TIREG-TIUPPD                                       
047610         PERFORM IMS-GU-ARTC                                              
047700         IF SEGMENT-FINNS                                                 
047800           IF  ART-KDPRODSL     = WS-KDPRODSL                             
047900             OR  WS-KDPRODSL = ZERO                                       
048000             IF  INDX = +1                                                
048100               MOVE ART-IDARTNR       TO MOD-IDARTNR-STR-ENTER            
048200             END-IF                                                       
048300             MOVE ART-IDARTNR         TO MOD-IDARTNR-STR (INDX)           
048400             MOVE MFS-RENSA-FAELT     TO MOD-TIFINLV     (INDX)           
048500                                         MOD-IDANSK      (INDX)           
048600             MOVE ART-IDFKNGRP        TO MOD-IDFKNGRP    (INDX)           
048700             MOVE ART-KDPRODSL        TO MOD-KDPRODSLA   (INDX)           
048710             MOVE ART-TIFINLV         TO MOD-TIFINLV     (INDX)           
048720             MOVE ART-IDLEVNR         TO MOD-IDLEVNR     (INDX)           
048730             MOVE WS-SATSSTATUS       TO MOD-SATSSTATUS  (INDX)           
048740             PERFORM IMS-GNP-ARTC11                                       
048750             IF SEGMENT-FINNS                                             
048760                MOVE CLAG-IDANSK     TO W-IDANSK                          
048770                                        MOD-IDANSK (INDX)                 
048791             END-IF                                                       
048792             PERFORM IMS-GU-BENA                                          
048793             IF SEGMENT-FINNS                                             
048794                MOVE BENA01-TEXT-BEART    TO MOD-BEART      (INDX)        
048795             END-IF                                                       
048796             ADD 1                      TO INDX                           
050400           END-IF                                                         
050500*        ELSE                                                             
050600*         MOVE MFS-RENSA-FAELT           TO MOD-SATSSTATUS  (INDX)        
050700         END-IF                                                           
050800       END-IF                                                             
050900     END-IF                                                               
051000     .                                                                    
051100 FAA-TIREG-TIUPPD SECTION.                                                
051200                                                                          
051300     IF   SATB01-STR-TIREGDAT = SATB01-STR-TIUPPDAT                       
051400         MOVE 'N'             TO   WS-SATSSTATUS                          
051500     ELSE                                                                 
051600         MOVE 'C'             TO   WS-SATSSTATUS                          
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 MFS-RENSA-FAELT-UT SECTION.                                              
052100                                                                          
052200*    --- ALLA UTDATA-FÄLT                                                 
052300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
052400     MOVE +1                  TO INDX                                     
052500     PERFORM UNTIL INDX > +14                                             
052600         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-STR (INDX)                   
052700                                 MOD-BEART (INDX)                         
052800                                 MOD-TIFINLV (INDX)                       
052900                                 MOD-IDANSK (INDX)                        
053000                                 MOD-IDLEVNR (INDX)                       
053100                                 MOD-KDPRODSLA (INDX)                     
053200                                 MOD-IDFKNGRP (INDX)                      
053300                                 MOD-SATSSTATUS (INDX)                    
053400         ADD +1               TO INDX                                     
053500     END-PERFORM                                                          
053600     .                                                                    
053700     SKIP2                                                                
053800                                                                          
053900 MFS-ROER-EJ-FAELT-UT SECTION.                                            
054000                                                                          
054100*    --- ALLA UTDATA-FÄLT                                                 
054200     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDARTNR-STR-ENTER                    
054300                                 MOD-IDARTNR-STR-NEXT                     
054400     MOVE +1                  TO INDX                                     
054500     PERFORM UNTIL INDX > +14                                             
054600       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-STR (INDX)                   
054700                                 MOD-SELECT (INDX)                        
054800                                 MOD-BEART (INDX)                         
054900                                 MOD-TIFINLV (INDX)                       
055000                                 MOD-IDANSK (INDX)                        
055100                                 MOD-IDLEVNR (INDX)                       
055200                                 MOD-KDPRODSLA (INDX)                     
055300                                 MOD-IDFKNGRP (INDX)                      
055400                                 MOD-SATSSTATUS (INDX)                    
055500         ADD +1               TO INDX                                     
055600     END-PERFORM                                                          
055700     .                                                                    
055800     SKIP2                                                                
055900 MFS-ADD-LAES-IN-FAELT-IGEN SECTION.                                      
056000                                                                          
056100*    --- ALLA UTDATA-FÄLT                                                 
056200     MOVE +1                  TO INDX                                     
056300     PERFORM UNTIL INDX > +14                                             
056400       MOVE MFS-ADD-LAES-IN-FAELT TO                                      
056500                                 MOD-SELECT-ATTR      (INDX)              
056600         ADD +1               TO INDX                                     
056700     END-PERFORM                                                          
056800     .                                                                    
056900     SKIP2                                                                
057000 MFS-FORM-ATTR SECTION.                                                   
057100                                                                          
057200     MOVE +1                  TO INDX                                     
057300     PERFORM UNTIL INDX > +14                                             
057400       MOVE MFS-FORMATETS-ATTR    TO                                      
057500                                 MOD-SELECT-ATTR      (INDX)              
057600         ADD +1               TO INDX                                     
057700     END-PERFORM                                                          
057800     .                                                                    
057900     SKIP3                                                                
058000 S01-RENSA-RAD   SECTION.                                                 
058100                                                                          
058200     PERFORM UNTIL INDX > +14                                             
058300         MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-STR (INDX)               
058400                                     MOD-SELECT      (INDX)               
058500                                     MOD-BEART       (INDX)               
058600                                     MOD-TIFINLV     (INDX)               
058700                                     MOD-IDANSK      (INDX)               
058800                                     MOD-IDLEVNR     (INDX)               
058900                                     MOD-KDPRODSLA   (INDX)               
059000                                     MOD-IDFKNGRP    (INDX)               
059100         ADD 1                    TO INDX                                 
059200     END-PERFORM                                                          
059300     .                                                                    
059400 IMS-GET-MSG SECTION.                                                     
059500                                                                          
059600     MOVE '  QC' TO GODK-STATUSKODER                                      
059700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
059800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059900     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
060100 IMS-ISRT-ALT-MSG SECTION.                                                
060200                                                                          
060300     MOVE SPACE TO GODK-STATUSKODER                                       
060400     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
060500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     SKIP3                                                                
060900 IMS-INSERT-MSG SECTION.                                                  
061000                                                                          
061100     IF NOT ENGLISH-TEXT                                                  
061200       MOVE '0' TO MFS-KDHUVOMR                                           
061300     END-IF                                                               
061400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
061500     MOVE SPACE TO GODK-STATUSKODER                                       
061600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
061700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000     EJECT                                                                
062100 IMS-GN-SATC-STR SECTION.                                                 
062200                                                                          
062300     STRING 'WLSATC01(WDJ1A1KY=>' W-IDARTNR-XX ')'                        
062400          DELIMITED BY SIZE INTO SSA1                                     
062500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
062600     CALL CBLTDLI USING GN SATC-PCB DLI-IO-AREA SSA1                      
062700     MOVE SATC-STATUS-CODE TO STATUS-WS                                   
062800     PERFORM IMS-STATUSKONTROLL                                           
062900                                                                          
063000     .                                                                    
063100     EJECT                                                                
063200 IMS-GU-SATB-ART SECTION.                                                 
063300                                                                          
063400     STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-X ')'                         
063500          DELIMITED BY SIZE INTO SSA1                                     
063600     MOVE '  GE' TO GODK-STATUSKODER                                      
063700     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
063800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     EJECT                                                                
065300 IMS-GU-ARTC SECTION.                                                     
065400                                                                          
065500     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
065600          DELIMITED BY SIZE INTO SSA1                                     
065700     MOVE '  GE' TO GODK-STATUSKODER                                      
065800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
065900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     EJECT                                                                
066300 IMS-GNP-ARTC11 SECTION.                                                  
066400                                                                          
066500     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
066600          DELIMITED BY SIZE INTO SSA1                                     
066700     STRING 'WLARTC11(KDSEGKEY= ' W-KDSEGKEY-X ')'                        
066800          DELIMITED BY SIZE INTO SSA2                                     
066900     MOVE '  GE' TO GODK-STATUSKODER                                      
067000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
067100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
067200     PERFORM IMS-STATUSKONTROLL                                           
067300     .                                                                    
067400     EJECT                                                                
067500 IMS-GU-BENA  SECTION.                                                    
067600                                                                          
067700     STRING 'WLBENA01(WDD3BSEQ= ' W-IDARTNR-X ')'                         
067800          DELIMITED BY SIZE INTO SSA1                                     
067900     STRING 'WLBENA11(IDSKYLT = ' W-IDSKYLT-X ')'                         
068000          DELIMITED BY SIZE INTO SSA2                                     
068100     MOVE '  GE' TO GODK-STATUSKODER                                      
068200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
068300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600     EJECT                                                                
068700 IMS-STATUSKONTROLL SECTION.                                              
068800                                                                          
068900     SET STATUS-IX TO 1                                                   
069000     SEARCH GODK-STATUS                                                   
069100       AT END CALL FELLOG                                                 
069200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
069300     END-SEARCH                                                           
069400     .                                                                    
