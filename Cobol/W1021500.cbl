000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1021500.                                                
000400 AUTHOR.         PAUL WACHSBERGER.                                        
000500 DATE-WRITTEN.   JULI  1990.                                              
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    PROGRAMMET                                                           
001100*        SKALL VISA INFORMATION OM SATS OCH FÖRPACKNING                   
001200*                                                                         
001300*        GER MÖJLIGHET ATT REGISTRERA OCH UPPDATERA                       
001400*        FÖRPACKNINGSNUMMER.                                              
001500*                                                                         
001600*        BYTA FRÅN EXTERN- TILL INTERNFÖRPACKNING.                        
001700*                                                                         
001800*        PROGRAMMET LÄSER FÖLJANDE BASER  :                               
001900*                                                                         
002000*                              WLSATB01 (WDJ101)                          
002100*                              WLSATB11 (WDJ111)                          
002200*                              WLBENA01 (WDD301)                          
002300*                                       (WDD311)                          
002400*                              WLARTC01 (WDK601)                          
002500*                              WLARTC11 (WDK611)                          
002600*                                                                         
002700*                                                                         
002800*        ÄT SPLIT 930404 BL                                               
002900*          - KONTROLL KDPRODSL ÄNDRAD                                     
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSAKTIONER: W1T215                                            
003300*                       W1T215U                                           
003400*                  MID: W1I21501                                          
003500*                                                                         
003600*    UTDATA.                                                              
003700*                  MOD: W1O21501                                          
003800                                                                          
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400*    -COPY WY2000W3                                                       
004500     SKIP3                                                                
004600*    -COPY WY2000W1                                                       
004700     SKIP3                                                                
004800 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W1021500'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  WS-FLIART                   PIC X       VALUE 'N'.                   
005200 77  WS-FORTSRAD                 PIC X       VALUE 'N'.                   
005300 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
005400 77  SPRAK-IX                    PIC S9(9)  VALUE +0 COMP SYNC.           
005500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-INDX                    PIC S9(4)  VALUE +3    COMP SYNC.        
005700 77  SPAR-KVPB-SEP-C1            PIC S9(6)V9(1) VALUE ZERO COMP-3.        
005800 77  SPAR-KVPB-SEP-C2            PIC S9(6)V9(1) VALUE ZERO COMP-3.        
005900 77  WS-KVBEHOV-SATS             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006000 77  WS-REANTPSA                 PIC S9(2)V9(3) VALUE ZERO COMP-3.        
006100 77  WS-IDARTNR-ING              PIC 9(9).                                
006200 77  WS-IDLEVNR                  PIC X(5).                                
006300     88  1002-SATS                           VALUE '1002 '.               
006400                                                                          
006500 77  INDATA-SW                   PIC X.                                   
006600     88  INDATA-OK                           VALUE 'J'.                   
006700     88  INDATA-FEL                          VALUE 'N'.                   
006800                                                                          
006900 77  SW-IFYLLT                   PIC X.                                   
007000     88  INDATA-IFYLLT                       VALUE 'J'.                   
007100     88  INDATA-EJ-IFYLLT                    VALUE 'N'.                   
007200                                                                          
007300 77  NYCKLAR-SW                  PIC X.                                   
007400     88  NYCKLAR-OK                          VALUE 'J'.                   
007500     88  NYCKLAR-FEL                         VALUE 'N'.                   
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4).                                
007800     88  EGEN-MID                            VALUE '1215'.                
007900     88  GODK-MID                            VALUE '1215' '1211'          
008000                                                   '1212' '1213'          
008100                                                   '1214'.                
008200 01  WS-DATUM-X.                                                          
008300     03  WS-AAR                  PIC 9(2).                                
008400     03  WS-VECKA                PIC 9(2).                                
008500 01  WS-TIAAVV REDEFINES WS-DATUM-X PIC 9(4).                             
008600     SKIP2                                                                
008700 01  WS-DAGENS-X.                                                         
008800     03  DAGENS-AAR                  PIC 9(2).                            
008900     03  DAGENS-VECKA                PIC 9(2).                            
009000 01  DAGENS-AAVV REDEFINES WS-DAGENS-X  PIC 9(4).                         
009100     SKIP2                                                                
009200 77  DAGENS-TIAAMMDD             PIC 9(7) VALUE ZERO.                     
009300 77  WS-TIAAMMDD                 PIC 9(7) VALUE ZERO.                     
009400 77  WS-TISTADAT                 PIC 9(7) VALUE ZERO.                     
009500 77  WS-IDARTNR-STR              PIC X(9).                                
009600 77  WS-IDRADNR                  PIC 9(5).                                
009700                                                                          
009800*01  -COPY WWPRODSL                                                       
009900     EJECT                                                                
010000 01  WS-FAELT.                                                            
010100     03  REANTPSA-IDEDIT         PIC S9(11)V9(4) OCCURS 3.                
010200     EJECT                                                                
010300 01  MEDDELANDEN.                                                         
010400     03  FEL-1.                                                           
010500        05  FILLER                  PIC X(40)   VALUE                     
010600           'EXTERNFÖRPACKNINGSMATERIAL'.                                  
010700        05  FILLER                  PIC X(40)   VALUE                     
010800           'EXTERNAL PACKING'.                                            
010900     03  FILLER REDEFINES  FEL-1.                                         
011000        05  FEL1 OCCURS 2           PIC X(40).                            
011100                                                                          
011200     03  FEL-2.                                                           
011300        05  FILLER                  PIC X(40)   VALUE                     
011400           'FÖRPACKNINGSMATERIAL SAKNAS'.                                 
011500        05  FILLER                  PIC X(40)   VALUE                     
011600           'NO PACKAGE REGISTRED'.                                        
011700     03  FILLER REDEFINES  FEL-2.                                         
011800        05  FEL2 OCCURS 2           PIC X(40).                            
011900     EJECT                                                                
012000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012100 01  GENERELLA-SUBPROGRAM.                                                
012200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
012700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013000*01 -COPY WDATAREA                                                        
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013300*01 -COPY WMEDAREA                                                        
013400*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
013500*01 -COPY WDECAREA                                                        
013600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013700*01 -COPY WMSGINIT                                                        
013800     SKIP3                                                                
013900 01  MESSAGE-CODES.                                                       
014000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014200     03  UPPDATE-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
014300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014600     03  INF-UPPDATE-DONE        PIC X(3)    VALUE '404'.                 
014700     EJECT                                                                
014800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015000     SKIP3                                                                
015100*01  MID -COPY W1I21501                                                   
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015400     SKIP3                                                                
015500*01  -COPY WMSGAREA                                                       
015600     EJECT                                                                
015700*    03  MOD -COPY W1O21501   -RED MSG-AREA.                              
015800                                                                          
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
016100 01  W-PROG-TO-PROG-SW.                                                   
016200     03    M-SW-LL               PIC S9(4)  VALUE +187 COMP SYNC.         
016300     03    M-SW-Z1-Z2            PIC X(2)   VALUE LOW-VALUE.              
016400     03    M-SW-KDTRANS          PIC X(8)   VALUE 'W1T231  '.             
016500     03    M-SW-IDTRANS          PIC X(4)   VALUE '1231'.                 
016600     03    M-SW-KDMFSTYP         PIC X(1)   VALUE '1'.                    
016700*03  -COPY W1I23101         -PRE 1231-                                    
016800     EJECT                                                                
016900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017000*                                                                         
017100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017200     SKIP3                                                                
017300 01  NYCKLAR-TILL-DLI.                                                    
017400     03  W-2233-KEY-X.                                                    
017500         05 FILLER           PIC X(4)         VALUE '2233'.               
017600         05 FILLER           PIC X(26)        VALUE LOW-VALUE.            
017700     03  W-KDSEGKEY-X.                                                    
017800         05  FILLER              PIC X(1)     VALUE '1'.                  
017900     03  W-IDARTNR-STR-X.                                                 
018000         05  W-IDARTNR-STR       PIC S9(9)    VALUE ZERO COMP-3.          
018100     03  W-IDARTNR-X.                                                     
018200         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
018300     03  W-KDSTRRAD-X.                                                    
018400         05  FILLER              PIC X(1)     VALUE '9'.                  
018500     03  W-MAX-X.                                                         
018600         05  FILLER              PIC X(1)     VALUE HIGH-VALUE.           
018700         05  FILLER              PIC S9(5) VALUE +99999  COMP-3.          
018800     03  W-WDJ111KY-X.                                                    
018900         05  FILLER              PIC X(1)     VALUE '9'.                  
019000         05  W-IDRADNR           PIC S9(5)    VALUE ZERO COMP-3.          
019100     03  W-WDJ111KY-2-X.                                                  
019200         05  FILLER              PIC X(1)     VALUE '9'.                  
019300         05  W-IDRADNR-2         PIC S9(5)    VALUE ZERO COMP-3.          
019400     03  W-IDSKYLT-X.                                                     
019500         05  W-IDSKYLT           PIC X(3)     VALUE SPACE.                
019600     03  W-KDCLAGER-2X.                                                   
019700         05  W-KDCLAGER          PIC S9(1)    VALUE +1   COMP-3.          
019800     03  W-KDCLAGER-X.                                                    
019900         05  FILLER              PIC S9(1)    VALUE +1   COMP-3.          
020000     03  W-TIBEHOV-X.                                                     
020100         05  W-TIBEHOV           PIC S9(5)    VALUE ZERO COMP-3.          
020200     03  W-IDSTRNOT-X.                                                    
020300         05  FILLER              PIC X(1)     VALUE '1'.                  
020400*    --- STATUS-KOD FRÅN IMS                                              
020500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020600     SKIP3                                                                
020700*01  -COPY WMFSAREA                                                       
020800     EJECT                                                                
020900 01  STATUS-WS                   PIC XX.                                  
021000     88  SEGMENT-FINNS                       VALUE '  '.                  
021100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021200     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
021300                                                   'GB'.                  
021400     EJECT                                                                
021500 01  GODK-STATUSKODER.                                                    
021600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021700     SKIP3                                                                
021800 01  SSA1                        PIC X(64).                               
021900 01  SSA2                        PIC X(64).                               
022000 01  SSA3                        PIC X(64).                               
022100     EJECT                                                                
022200*    --- IMS FUNKTIONSKODER                                               
022300*01  -COPY W0003                                                          
022400     EJECT                                                                
022500*    ---  DLI INPUT-OUTPUT AREA                                           
022600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022700     SKIP3                                                                
022800 01  DLI-IO-AREA.                                                         
022900     03  IO-AREA                 PIC X(928)  VALUE SPACE.                 
023000     SKIP3                                                                
023100*    03  WLSATB01 -COPY WDJ101 -PRE SATB01-  -RED IO-AREA.                
023200     SKIP3                                                                
023300*    03  WLSATB11 -COPY WDJ111 -PRE SATB11-  -RED IO-AREA.                
023400     SKIP3                                                                
023500*    03  WLSATB22 -COPY WDJ122 -PRE SATB22-   -RED IO-AREA.               
023600     SKIP3                                                                
023700*    03  WLBENA01 -COPY WDD311 -PRE BENA01-   -RED IO-AREA.               
023800     EJECT                                                                
023900*    03  WLARTC01 -COPY WDK601                -RED IO-AREA.               
024000     EJECT                                                                
024100*    03  WLARTC11 -COPY WDK611                -RED IO-AREA.               
024200     EJECT                                                                
024300*    03  WLXXBY11 -COPY WDGX2234              -RED IO-AREA.               
024400     EJECT                                                                
024500 LINKAGE SECTION.                                                         
024600                                                                          
024700*01  -COPY W0009      -PRE MSG-                                           
024800     EJECT                                                                
024900*01  -COPY W0009      -PRE ALT-                                           
025000     EJECT                                                                
025100*01  -COPY W0008      -PRE USEA-                                          
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400*01  -COPY W0008      -PRE SATB-                                          
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008      -PRE BENA-                                          
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008      -PRE ARTC-                                          
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008      -PRE SATB2-                                         
026400     05  FILLER                  PIC X.                                   
026500                                                                          
026600*01  -COPY W0008      -PRE XXBY-                                          
026700     05  FILLER                  PIC X.                                   
026800                                                                          
026900     EJECT                                                                
027000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB SATB-PCB              
027100                           BENA-PCB ARTC-PCB SATB2-PCB XXBY-PCB.          
027200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB SATB-PCB              
027300                           BENA-PCB ARTC-PCB SATB2-PCB XXBY-PCB.          
027400     SKIP3                                                                
027500     PERFORM IMS-GET-MSG                                                  
027600     IF SEGMENT-FINNS                                                     
027700       PERFORM A-INIT                                                     
027800       PERFORM B-KOLLA-NYCKLEL                                            
027900         IF NYCKLAR-OK                                                    
028000           PERFORM C-KLL-PFTRCK-TLLDL-IMSNCKLR                            
028100           PERFORM IMS-GU-SATB01                                          
028200           IF SEGMENT-FINNS                                               
028300             MOVE 'J'                  TO INDATA-SW                       
028400             PERFORM D-FLTT-RTNF-EV-KL-FLEXFORP                           
028500             IF MFS-UPDATE                                                
028600               PERFORM E-KOLLA-INPUT                                      
028700               IF INDATA-OK                                               
028800                 PERFORM F-UPPDATERA                                      
028900                 PERFORM G-LAES-VISA-INFO                                 
029000               ELSE                                                       
029100                 CALL WMEDKONV USING MED-WMEDAREA                         
029200                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
029300                 PERFORM S14-MFS-ROER-EJ-FAELT                            
029400               END-IF                                                     
029500             ELSE                                                         
029600               IF MFS-SPLIT                                               
029700                 PERFORM H-KOLLA-PF9                                      
029800               ELSE                                                       
029900                 MOVE NEJ         TO SW-IFYLLT                            
030000                 IF MFS-IDPFK NOT = '7'                                   
030100                    PERFORM S06-KOLLA-OM-ALL-PLUS                         
030200                 END-IF                                                   
030300                 IF  INDATA-EJ-IFYLLT                                     
030400                    PERFORM G-LAES-VISA-INFO                              
030500                 ELSE                                                     
030600                    MOVE '003'         TO MED-IDMFSFEL                    
030700                    CALL WMEDKONV USING MED-WMEDAREA                      
030800                    MOVE MED-MFSFEL TO MOD-TEMFSFEL                       
030900                    PERFORM S12-ADD-LAES-IN-FAELT                         
031000                    PERFORM S14-MFS-ROER-EJ-FAELT                         
031100                 END-IF                                                   
031200               END-IF                                                     
031300             END-IF                                                       
031400           ELSE                                                           
031500              MOVE '017'         TO MED-IDMFSFEL                          
031600              CALL WMEDKONV USING MED-WMEDAREA                            
031700              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
031800              PERFORM S13-MFS-FORMATETS-ATTR                              
031900              PERFORM S11-RENSA-RADER                                     
032000           END-IF                                                         
032100         ELSE                                                             
032200           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
032300           CALL WMEDKONV USING MED-WMEDAREA                               
032400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
032500           PERFORM S13-MFS-FORMATETS-ATTR                                 
032600         END-IF                                                           
032700     ELSE                                                                 
032800         PERFORM S10-RENSA-ALLT                                           
032900         PERFORM S13-MFS-FORMATETS-ATTR                                   
033000     END-IF                                                               
033100     IF MFS-IDPFK NOT = '9'                                               
033200         MOVE LENGTH OF MOD-W1O21501 TO MSG-KVLL                          
033300         ADD  +4                     TO MSG-KVLL                          
033400         PERFORM IMS-INSERT-MSG                                           
033500     END-IF                                                               
033600     MOVE ZERO TO RETURN-CODE                                             
033700     GOBACK                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 A-INIT SECTION.                                                          
034100     SKIP2                                                                
034200     IF MSG-DUBBLA-TRANSKODER                                             
034300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I21501                 
034400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
034500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
034600       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
034700       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
034800     ELSE                                                                 
034900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I21501                 
035000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
035100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
035200       MOVE SPACE                         TO MFS-KDTRTYP                  
035300                                             MFS-IDPFK                    
035400     END-IF                                                               
035500     MOVE LOW-VALUE                       TO MSG-AREA                     
035600     MOVE 'W1O215N1'                      TO MFS-IDMOD                    
035700     MOVE '1215'                          TO MOD-IDTRANS                  
035800     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
035900     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
036000                                             MOD-TEMFSINF                 
036100                                           MOD-IDARTNR-STR-IN             
036200     IF NOT EGEN-MID                                                      
036300       MOVE SPACE TO MFS-KDTRTYP                                          
036400       MOVE '7' TO MFS-IDPFK                                              
036500     END-IF                                                               
036600                                                                          
036700     ACCEPT DAGENS-TIAAMMDD FROM DATE                                     
036800     PERFORM S15-FIXA-DAGENS-AAVV                                         
036900     .                                                                    
037000     EJECT                                                                
037100 B-KOLLA-NYCKLEL         SECTION.                                         
037200     SKIP2                                                                
037300     MOVE JA                            TO NYCKLAR-SW                     
037400                                                                          
037500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037600     MOVE '001'             TO MSGI-KDCALL                                
037700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037900     MOVE '1215'            TO MSGI-IDTRANS                               
038000                                                                          
038100     IF MFS-IDTRANS = '1215'                                              
038200     OR (MID-IDARTNR-STR-IN NUMERIC                                       
038300     AND MID-IDARTNR-STR-IN > ZERO)                                       
038400        MOVE MID-IDARTNR-STR-IN TO MSGI-IDARTNR                           
038500     END-IF                                                               
038600                                                                          
038700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038800                                                                          
038900     MOVE MSGI-IDARTNR   TO WS-IDARTNR-STR                                
039000     INSPECT WS-IDARTNR-STR REPLACING LEADING SPACE BY ZERO               
039100                                                                          
039200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
039300       MOVE 'S  ' TO MED-IDSKYLT                                          
039400                     W-IDSKYLT                                            
039500       MOVE +1    TO SPRAK-IX                                             
039600     ELSE                                                                 
039700       MOVE 'GB ' TO MED-IDSKYLT                                          
039800                     W-IDSKYLT                                            
039900       MOVE +2    TO SPRAK-IX                                             
040000     END-IF                                                               
040100                                                                          
040200     IF MID-IDARTNR-STR-IN = ALL '+'                                      
040300        CONTINUE                                                          
040400     ELSE                                                                 
040500        MOVE '7'                        TO MFS-IDPFK                      
040600     END-IF                                                               
040700     IF  WS-IDARTNR-STR  NUMERIC                                          
040800     AND WS-IDARTNR-STR > ZERO                                            
040900     AND WS-IDARTNR-STR < 100000000                                       
041000        MOVE WS-IDARTNR-STR             TO W-IDARTNR-STR                  
041100     ELSE                                                                 
041200        MOVE 'N'                        TO NYCKLAR-SW                     
041300     END-IF                                                               
041400     MOVE WS-IDARTNR-STR                TO MOD-IDARTNR-STR-UT             
041500     INSPECT MOD-IDARTNR-STR-UT REPLACING LEADING ZERO BY SPACE           
041600     .                                                                    
041700     EJECT                                                                
041800 C-KLL-PFTRCK-TLLDL-IMSNCKLR SECTION.                                     
041900     SKIP2                                                                
042000     IF MID-IDRADNR-ENTER NOT NUMERIC                                     
042100     OR MID-IDRADNR-NEXT  NOT NUMERIC                                     
042200        MOVE '7'                      TO MFS-IDPFK                        
042300     END-IF                                                               
042400     IF MFS-IDPFK = '7'                                                   
042500        MOVE SPACE                    TO MFS-KDTRTYP                      
042600        MOVE INF-FIRST-PAGE           TO MED-IDMFSFEL                     
042700        CALL WMEDKONV USING MED-WMEDAREA                                  
042800        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
042900        MOVE ZERO                     TO W-IDRADNR                        
043000     ELSE                                                                 
043100        IF MFS-IDPFK = '8'                                                
043200           MOVE MID-IDRADNR-NEXT      TO W-IDRADNR                        
043300        ELSE                                                              
043400           MOVE MID-IDRADNR-ENTER     TO W-IDRADNR                        
043500        END-IF                                                            
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900 D-FLTT-RTNF-EV-KL-FLEXFORP SECTION.                                      
044000     SKIP2                                                                
044100     MOVE SPACE                       TO WS-IDLEVNR                       
044200     MOVE SATB01-STR-BEART-SVE        TO MOD-BEART                        
044300     MOVE SATB01-STR-IDLEVNR          TO MOD-IDLEVNR                      
044400                                         WS-IDLEVNR                       
044500     MOVE SATB01-STR-FLEXFORP         TO MOD-FLEXFORP-UT                  
044600     MOVE SATB01-STR-IDARTNR          TO W-IDARTNR                        
044700     IF SATB01-STR-FLEXFORP = 'J'                                         
044800        MOVE FEL1(SPRAK-IX)           TO MOD-TEMFSFEL                     
044900     ELSE                                                                 
045000        IF SATB01-STR-FLEXFORP = 'N'                                      
045100           MOVE FEL2(SPRAK-IX)        TO MOD-TEMFSFEL                     
045200        END-IF                                                            
045300     END-IF                                                               
045400     IF MFS-UPDATE                                                        
045500        PERFORM DA-KOLLA-FLEXFORP                                         
045600     END-IF                                                               
045700     PERFORM IMS-GU-BENA                                                  
045800     IF SEGMENT-FINNS                                                     
045900        MOVE BENA01-TEXT-BEART        TO MOD-BEART                        
046000        IF NOT 1002-SATS                                                  
046100           PERFORM IMS-GU-ARTC01                                          
046200           MOVE ART-IDLEVNR        TO MOD-IDLEVNR                         
046300        END-IF                                                            
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 DA-KOLLA-FLEXFORP   SECTION.                                             
046800     SKIP2                                                                
046900     IF  MID-FLEXFORP-IN  NOT = ALL '+'                                   
047000        IF  SATB01-STR-FLEXFORP  = SPACE                                  
047100           IF  MID-FLEXFORP-IN    = JA OR 'Y'                             
047200           AND MID-IDARTNR-IN (1) = ALL '+'                               
047300           AND MID-IDARTNR-IN (2) = ALL '+'                               
047400           AND MID-IDARTNR-IN (3) = ALL '+'                               
047500              MOVE ZERO                     TO W-IDRADNR                  
047600              PERFORM IMS-GNP-SATB11                                      
047700              MOVE MID-IDRADNR-ENTER        TO W-IDRADNR                  
047800              IF SEGMENT-FINNS                                            
047900                 MOVE 'N'                   TO INDATA-SW                  
048000                 MOVE MFS-ALFA-FAELT-FEL    TO                            
048100                                           MOD-FLEXFORP-IN-ATTR           
048200              ELSE                                                        
048300                 MOVE MFS-ALFA-FAELT-RAETT  TO                            
048400                                           MOD-FLEXFORP-IN-ATTR           
048500              END-IF                                                      
048600           ELSE                                                           
048700              IF  MID-FLEXFORP-IN    = 'N'                                
048800                 MOVE MFS-ALFA-FAELT-RAETT  TO                            
048900                                           MOD-FLEXFORP-IN-ATTR           
049000              ELSE                                                        
049100                 MOVE 'N'                   TO INDATA-SW                  
049200                 MOVE MFS-ALFA-FAELT-FEL    TO                            
049300                                           MOD-FLEXFORP-IN-ATTR           
049400              END-IF                                                      
049500           END-IF                                                         
049600        ELSE                                                              
049700           IF  SATB01-STR-FLEXFORP  = JA                                  
049800           AND  MID-FLEXFORP-IN     = NEJ                                 
049900              MOVE MFS-ALFA-FAELT-RAETT     TO                            
050000                                           MOD-FLEXFORP-IN-ATTR           
050100           ELSE                                                           
050200              IF  SATB01-STR-FLEXFORP  = NEJ                              
050300              AND  MID-FLEXFORP-IN     = NEJ                              
050400                 MOVE MFS-ALFA-FAELT-RAETT  TO                            
050500                                           MOD-FLEXFORP-IN-ATTR           
050600              ELSE                                                        
050700                 MOVE 'N'                   TO INDATA-SW                  
050800                 MOVE MFS-ALFA-FAELT-FEL    TO                            
050900                                           MOD-FLEXFORP-IN-ATTR           
051000              END-IF                                                      
051100           END-IF                                                         
051200        END-IF                                                            
051300     ELSE                                                                 
051400        IF  SATB01-STR-FLEXFORP  = SPACE                                  
051500                 MOVE 'N'                   TO INDATA-SW                  
051600                 MOVE MFS-ALFA-FAELT-FEL    TO                            
051700                                           MOD-FLEXFORP-IN-ATTR           
051800        END-IF                                                            
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 E-KOLLA-INPUT  SECTION.                                                  
052300     SKIP2                                                                
052400     MOVE NEJ                            TO SW-IFYLLT                     
052500     PERFORM S06-KOLLA-OM-ALL-PLUS                                        
052600     IF  INDATA-EJ-IFYLLT                                                 
052700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
052800        MOVE 'N'                         TO INDATA-SW                     
052900     ELSE                                                                 
053000        IF MID-KLAR NOT = ALL '+'                                         
053100           IF MID-KLAR = 'Y' OR 'J' OR 'N'                                
053200              MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KLAR-ATTR                 
053300           ELSE                                                           
053400              MOVE NEJ                   TO INDATA-SW                     
053500              MOVE MFS-ALFA-FAELT-FEL    TO MOD-KLAR-ATTR                 
053600           END-IF                                                         
053700        ELSE                                                              
053800           MOVE NEJ                      TO INDATA-SW                     
053900           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KLAR-ATTR                 
054000        END-IF                                                            
054100        MOVE +1                          TO INDX                          
054200        PERFORM UNTIL INDX > +3                                           
054300           IF  MID-INDATA       (INDX) NOT = ALL '+'                      
054400              INSPECT MID-IDRADNR (INDX) REPLACING LEADING                
054500                                             SPACE BY ZERO                
054600             IF MID-BORT (INDX) NOT   = ALL '+'                           
054700                PERFORM EA-KOLLA-BORTTAG                                  
054800             ELSE                                                         
054900                IF  MID-IDRADNR (INDX) = ZERO                             
055000                   PERFORM EB-KOLLA-NYUPPLAEGG                            
055100                ELSE                                                      
055200                   PERFORM EC-KOLLA-AENDRING                              
055300                END-IF                                                    
055400             END-IF                                                       
055500           END-IF                                                         
055600           ADD +1                        TO INDX                          
055700        END-PERFORM                                                       
055800        IF  INDATA-SW    = NEJ                                            
055900           MOVE '001'                    TO MED-IDMFSFEL                  
056000        END-IF                                                            
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400 EA-KOLLA-BORTTAG    SECTION.                                             
056500     SKIP2                                                                
056600     IF  MID-BORT (INDX)  = 'J' OR 'Y'                                    
056700        MOVE MID-IDRADNR (INDX)           TO W-IDRADNR-2                  
056800        IF W-IDRADNR-2 > ZERO                                             
056900           MOVE MFS-ALFA-FAELT-RAETT      TO MOD-BORT-ATTR(INDX)          
057000        ELSE                                                              
057100           MOVE NEJ                       TO INDATA-SW                    
057200           MOVE MFS-ALFA-FAELT-FEL        TO MOD-BORT-ATTR(INDX)          
057300        END-IF                                                            
057400     ELSE                                                                 
057500        MOVE NEJ                          TO INDATA-SW                    
057600        MOVE MFS-ALFA-FAELT-FEL           TO MOD-BORT-ATTR(INDX)          
057700     END-IF                                                               
057800     IF MID-FLEXFORP-IN NOT =  ALL '+'                                    
057900        MOVE NEJ                          TO INDATA-SW                    
058000        MOVE MFS-ALFA-FAELT-FEL           TO                              
058100                                     MOD-FLEXFORP-IN-ATTR                 
058200     END-IF                                                               
058300     IF  MID-IDARTNR-IN  (INDX) NOT = ALL '+'                             
058400        MOVE NEJ                          TO INDATA-SW                    
058500        MOVE MFS-NUM-FAELT-FEL            TO                              
058600                                      MOD-IDARTNR-IN-ATTR (INDX)          
058700     END-IF                                                               
058800     IF MID-REANTPSA-IN (INDX) NOT = ALL '+'                              
058900        MOVE NEJ                          TO INDATA-SW                    
059000        MOVE MFS-NUM-FAELT-FEL            TO                              
059100                                     MOD-REANTPSA-IN-ATTR (INDX)          
059200     END-IF                                                               
059300     IF MID-TISTADAT-IN (INDX) NOT = ALL '+'                              
059400        MOVE NEJ                          TO INDATA-SW                    
059500        MOVE MFS-NUM-FAELT-FEL            TO                              
059600                                     MOD-TISTADAT-IN-ATTR (INDX)          
059700     END-IF                                                               
059800     IF MID-TISTODAT-IN (INDX) NOT = ALL '+'                              
059900        MOVE NEJ                          TO INDATA-SW                    
060000        MOVE MFS-NUM-FAELT-FEL            TO                              
060100                                     MOD-TISTODAT-IN-ATTR (INDX)          
060200     END-IF                                                               
060300     IF  MID-TESTRNOT-IN (INDX) NOT = ALL '+'                             
060400        MOVE NEJ                          TO INDATA-SW                    
060500        MOVE MFS-ALFA-FAELT-FEL           TO                              
060600                                     MOD-TESTRNOT-ATTR (INDX)             
060700     END-IF                                                               
060800     IF INDATA-OK                                                         
060900****    MOVE MID-IDRADNR (INDX)           TO W-IDRADNR-2                  
061000        PERFORM IMS-GU-SATB11                                             
061100        IF SATB11-RAD-TIREGDAT NOT = DAGENS-TIAAMMDD                      
061200           MOVE NEJ                       TO INDATA-SW                    
061300           MOVE MFS-ALFA-FAELT-FEL        TO MOD-BORT-ATTR(INDX)          
061400        END-IF                                                            
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 EB-KOLLA-NYUPPLAEGG SECTION.                                             
061900     SKIP2                                                                
062000     IF  MID-IDARTNR-IN (INDX) = ALL '+'                                  
062100        MOVE NEJ                        TO INDATA-SW                      
062200        MOVE MFS-NUM-FAELT-FEL          TO                                
062300                                 MOD-IDARTNR-IN-ATTR(INDX)                
062400     ELSE                                                                 
062500        PERFORM S01-KOLLA-IDARTNR                                         
062600     END-IF                                                               
062700     IF MID-REANTPSA-IN(INDX) = ALL '+'                                   
062800        MOVE NEJ                        TO INDATA-SW                      
062900        MOVE MFS-NUM-FAELT-FEL          TO                                
063000                                 MOD-REANTPSA-IN-ATTR(INDX)               
063100     ELSE                                                                 
063200        PERFORM S02-KOLLA-REANTPSA                                        
063300     END-IF                                                               
063400     IF MID-TISTADAT-IN(INDX) = ALL '+'                                   
063500          MOVE NEJ                      TO INDATA-SW                      
063600          MOVE MFS-NUM-FAELT-FEL        TO                                
063700                                   MOD-TISTADAT-IN-ATTR(INDX)             
063800     ELSE                                                                 
063900          PERFORM S03-KOLLA-TISTADAT                                      
064000     END-IF                                                               
064100     IF  MID-TISTODAT-IN(INDX) NOT = ALL '+'                              
064200        PERFORM S04-KOLLA-TISTODAT                                        
064300     END-IF                                                               
064400     IF  MID-TESTRNOT-IN (INDX) NOT = ALL '+'                             
064500        MOVE MFS-ALFA-FAELT-RAETT         TO                              
064600                                     MOD-TESTRNOT-ATTR (INDX)             
064700     END-IF                                                               
064800     IF INDATA-OK                                                         
064900        PERFORM S05-TESTA-IDARTNR-MOT-ARTREG                              
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 EC-KOLLA-AENDRING   SECTION.                                             
065400     SKIP2                                                                
065500     IF  MID-IDARTNR-IN (INDX) NOT = ALL '+'                              
065600        MOVE NEJ                        TO INDATA-SW                      
065700        MOVE MFS-NUM-FAELT-FEL          TO                                
065800                                   MOD-IDARTNR-IN-ATTR (INDX)             
065900     END-IF                                                               
066000     IF MID-REANTPSA-IN(INDX) NOT = ALL '+'                               
066100        MOVE NEJ                        TO INDATA-SW                      
066200        MOVE MFS-NUM-FAELT-FEL          TO                                
066300                                   MOD-REANTPSA-IN-ATTR (INDX)            
066400     END-IF                                                               
066500                                                                          
066600     IF  MID-TISTADAT-IN(INDX) NOT = ALL '+'                              
066700        MOVE NEJ                        TO INDATA-SW                      
066800        MOVE MFS-NUM-FAELT-FEL          TO                                
066900                                   MOD-TISTADAT-IN-ATTR (INDX)            
067000     END-IF                                                               
067100                                                                          
067200     IF  MID-TISTODAT-IN(INDX) NOT = ALL '+'                              
067300        PERFORM S04-KOLLA-TISTODAT                                        
067400     END-IF                                                               
067500     IF  MID-TESTRNOT-IN (INDX) NOT = ALL '+'                             
067600        MOVE MFS-ALFA-FAELT-RAETT         TO                              
067700                                     MOD-TESTRNOT-ATTR (INDX)             
067800     END-IF                                                               
067900     .                                                                    
068000     EJECT                                                                
068100 F-UPPDATERA SECTION.                                                     
068200     SKIP2                                                                
068300     IF MID-IDARTNR-IN (1) NOT = ALL '+'                                  
068400     OR MID-IDARTNR-IN (2) NOT = ALL '+'                                  
068500     OR MID-IDARTNR-IN (3) NOT = ALL '+'                                  
068600        PERFORM IMS-GU-SATB11-LAST                                        
068700        IF SEGMENT-FINNS                                                  
068800           MOVE SATB11-RAD-IDRADNR       TO WS-IDRADNR                    
068900        ELSE                                                              
069000           MOVE ZERO                     TO WS-IDRADNR                    
069100        END-IF                                                            
069200     END-IF                                                               
069300     PERFORM IMS-GHU-SATB01                                               
069400     MOVE DAGENS-TIAAMMDD                TO SATB01-STR-TIUPPDAT           
069500     IF MID-KLAR = JA OR 'Y'                                              
069600        MOVE NEJ                         TO SATB01-STR-FLFORPQ            
069700     END-IF                                                               
069800     IF MID-FLEXFORP-IN NOT =  ALL '+'                                    
069900        IF MID-FLEXFORP-IN =  JA  OR 'Y'                                  
070000           MOVE NEJ                      TO SATB01-STR-FLFORPQ            
070100        END-IF                                                            
070200        MOVE MID-FLEXFORP-IN             TO SATB01-STR-FLEXFORP           
070300                                            MOD-FLEXFORP-UT               
070400        IF MID-FLEXFORP-IN = 'Y'                                          
070500           MOVE JA                       TO SATB01-STR-FLEXFORP           
070600        END-IF                                                            
070700        MOVE MFS-FORMATETS-ATTR          TO MOD-FLEXFORP-IN-ATTR          
070800        MOVE MFS-RENSA-FAELT             TO MOD-FLEXFORP-IN               
070900     END-IF                                                               
071000     PERFORM IMS-REPL-SATB                                                
071100     MOVE +1                             TO INDX                          
071200     PERFORM UNTIL INDX > +3                                              
071300        IF  MID-INDATA       (INDX) NOT = ALL '+'                         
071400           IF MID-BORT (INDX) NOT = ALL '+'                               
071500                MOVE MID-IDRADNR(INDX)      TO W-IDRADNR                  
071600                PERFORM IMS-GHU-SATB11                                    
071700                IF SEGMENT-FINNS                                          
071800                   PERFORM IMS-DLET-SATB                                  
071900                   IF 1002-SATS                                           
072000                      PERFORM FC-TA-BORT-2234-TRANS                       
072100                   END-IF                                                 
072200                END-IF                                                    
072300           ELSE                                                           
072400              IF  MID-IDARTNR-IN(INDX) NOT = ALL '+'                      
072500              AND MID-IDRADNR (INDX)       = ZERO                         
072600                 PERFORM FB-INSERTA                                       
072700              ELSE                                                        
072800                 PERFORM FA-REPLACA                                       
072900              END-IF                                                      
073000           END-IF                                                         
073100        END-IF                                                            
073200        ADD                            +1 TO INDX                         
073300     END-PERFORM                                                          
073400     MOVE MID-IDRADNR-ENTER               TO W-IDRADNR                    
073500     PERFORM IMS-GU-SATB01                                                
073600     MOVE INF-UPPDATE-DONE                TO MED-IDMFSFEL                 
073700     CALL WMEDKONV USING MED-WMEDAREA                                     
073800     MOVE MED-MFSFEL                      TO MOD-TEMFSINF                 
073900     .                                                                    
074000     EJECT                                                                
074100 FA-REPLACA   SECTION.                                                    
074200     SKIP2                                                                
074300     MOVE MID-IDRADNR(INDX)                 TO W-IDRADNR                  
074400                                               W-IDRADNR-2                
074500     PERFORM IMS-GHU-SATB11                                               
074600     IF SEGMENT-FINNS                                                     
074700           MOVE 'U'                         TO                            
074800                                          SATB11-RAD-KDISATS              
074900           MOVE SATB11-RAD-REANTPSA         TO WS-REANTPSA                
075000           MOVE SATB11-RAD-IDARTNR          TO WS-IDARTNR-ING             
075100           MOVE SATB11-RAD-TISTADAT         TO WS-TISTADAT                
075200           IF MID-TISTODAT-IN(INDX) NOT = ALL '+'                         
075300              IF MID-TISTODAT-IN(INDX) NOT = 9999                         
075400                  MOVE MID-TISTODAT-IN(INDX) TO DAT-I-TIDATUM             
075500                  MOVE 'AAVV  '             TO DAT-KDDATFORM              
075600                  PERFORM S99-CALL-WDATKONV                               
075700                  IF DAT-KDSVAR-OK                                        
075800                     MOVE DAT-TIAA-VECKA    TO WS-AAR                     
075900                     MOVE DAT-TIVV          TO WS-VECKA                   
076000                     MOVE DAT-TIAAMMDD      TO WS-TIAAMMDD                
076100                     IF DAGENS-AAVV = WS-TIAAVV                           
076200                        MOVE DAGENS-TIAAMMDD                              
076300                                       TO SATB11-RAD-TISTODAT             
076400                     ELSE                                                 
076500                        MOVE DAT-TIAAMMDD                                 
076600                                       TO SATB11-RAD-TISTODAT             
076700                     END-IF                                               
076800                  ELSE                                                    
076900                     MOVE ZERO              TO                            
077000                                            SATB11-RAD-TISTODAT           
077100                  END-IF                                                  
077200              ELSE                                                        
077300                  MOVE +999999              TO                            
077400                                            SATB11-RAD-TISTODAT           
077500              END-IF                                                      
077600              PERFORM IMS-REPL-SATB                                       
077700           END-IF                                                         
077800           IF MID-TESTRNOT-IN (INDX) NOT = ALL '+'                        
077900              PERFORM IMS-GHNP-SATB22                                     
078000              IF SEGMENT-FINNS                                            
078100                 MOVE MID-TESTRNOT-IN (INDX) TO                           
078200                                    SATB22-NOT-TESTRNOT (1)               
078300                 PERFORM IMS-REPL-SATB                                    
078400              ELSE                                                        
078500                 MOVE '1'                TO SATB22-NOT-IDSTRNOT           
078600                 MOVE MID-TESTRNOT-IN (INDX) TO                           
078700                                         SATB22-NOT-TESTRNOT (1)          
078800                 MOVE SPACE               TO                              
078900                                         SATB22-NOT-TESTRNOT (2)          
079000                 PERFORM IMS-ISRT-SATB22                                  
079100              END-IF                                                      
079200           END-IF                                                         
079300           IF 1002-SATS                                                   
079400              PERFORM FAB-SKAPA-2234-TRANS-U                              
079500           END-IF                                                         
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900 FAB-SKAPA-2234-TRANS-U  SECTION.                                         
080000     SKIP2                                                                
080100     PERFORM S16-BERAKNA-KVPB                                             
080200     COMPUTE 2234-KVPB-SEP-TOT =                                          
080300     SPAR-KVPB-SEP-C1 + SPAR-KVPB-SEP-C2                                  
080400     MOVE ZERO                          TO 2234-REANTPSA-NY               
080500     MOVE WS-REANTPSA                   TO 2234-REANTPSA-GAMMAL           
080600     MOVE 'U'                           TO 2234-KDISATS                   
080700     MOVE WS-TISTADAT                   TO DAT-I-TIDATUM                  
080800     MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
080900     PERFORM S99-CALL-WDATKONV                                            
081000     IF DAT-KDSVAR-OK                                                     
081100        MOVE DAT-TIAA-VECKA             TO WS-AAR                         
081200        MOVE DAT-TIVV                   TO WS-VECKA                       
081300        MOVE WS-TIAAVV                  TO 2234-TIBEHDAT                  
081400     ELSE                                                                 
081500        MOVE ZERO                       TO 2234-TIBEHDAT                  
081600     END-IF                                                               
081700     MOVE W-IDARTNR-STR                 TO 2234-IDARTNR-SATS              
081800     MOVE WS-IDARTNR-ING                TO 2234-IDARTNR-ING               
081900     PERFORM IMS-ISRT-XXBY                                                
082000     .                                                                    
082100     EJECT                                                                
082200 FB-INSERTA  SECTION.                                                     
082300     SKIP2                                                                
082400     IF 1002-SATS                                                         
082500        PERFORM FBA-SKAPA-2234-TRANS-N                                    
082600        IF WS-FLIART = NEJ                                                
082700           MOVE MID-IDARTNR-IN(INDX)                                      
082800                                   TO W-IDARTNR                           
082900           PERFORM IMS-GHU-ARTC01                                         
083000           MOVE JA                 TO ART-FLIART                          
083100           PERFORM  IMS-REPL-ARTC                                         
083200        END-IF                                                            
083300     END-IF                                                               
083400     INITIALIZE SATB11-RAD-WDJ111                                         
083500     ADD +10                       TO WS-IDRADNR                          
083600     MOVE WS-IDRADNR               TO SATB11-RAD-IDRADNR                  
083700     MOVE '9'                      TO SATB11-RAD-KDSTRRAD                 
083800     MOVE MID-IDARTNR-IN(INDX)     TO SATB11-RAD-IDARTNR                  
083900     MOVE 'N'                      TO SATB11-RAD-KDISATS                  
084000     MOVE REANTPSA-IDEDIT(INDX)    TO SATB11-RAD-REANTPSA                 
084100     MOVE DAGENS-TIAAMMDD          TO SATB11-RAD-TIREGDAT                 
084200     PERFORM S03-KOLLA-TISTADAT                                           
084300     IF DAT-KDSVAR-OK                                                     
084400        IF DAGENS-AAVV = WS-TIAAVV                                        
084500           MOVE DAGENS-TIAAMMDD    TO SATB11-RAD-TISTADAT                 
084600        ELSE                                                              
084700           MOVE WS-TIAAMMDD        TO SATB11-RAD-TISTADAT                 
084800        END-IF                                                            
084900     ELSE                                                                 
085000        MOVE ZERO                  TO SATB11-RAD-TISTADAT                 
085100     END-IF                                                               
085200     IF MID-TISTODAT-IN(INDX) NOT = ALL '+'                               
085300        PERFORM S04-KOLLA-TISTODAT                                        
085400        IF DAT-KDSVAR-OK                                                  
085500           IF DAGENS-AAVV = WS-TIAAVV                                     
085600              MOVE DAGENS-TIAAMMDD TO SATB11-RAD-TISTODAT                 
085700           ELSE                                                           
085800              MOVE WS-TIAAMMDD     TO SATB11-RAD-TISTODAT                 
085900           END-IF                                                         
086000        ELSE                                                              
086100           IF MID-TISTODAT-IN(INDX) = 9999                                
086200              MOVE +999999         TO SATB11-RAD-TISTODAT                 
086300           ELSE                                                           
086400              MOVE ZERO            TO SATB11-RAD-TISTODAT                 
086500           END-IF                                                         
086600        END-IF                                                            
086700     ELSE                                                                 
086800        MOVE +999999               TO SATB11-RAD-TISTODAT                 
086900     END-IF                                                               
087000     PERFORM IMS-ISRT-SATB11                                              
087100     IF MID-TESTRNOT-IN (INDX) NOT = ALL '+'                              
087200        MOVE '1'                      TO SATB22-NOT-IDSTRNOT              
087300        MOVE MID-TESTRNOT-IN (INDX)   TO SATB22-NOT-TESTRNOT (1)          
087400        MOVE SPACE                    TO SATB22-NOT-TESTRNOT (2)          
087500        PERFORM IMS-ISRT-SATB22                                           
087600     END-IF                                                               
087700     .                                                                    
087800     EJECT                                                                
087900 FBA-SKAPA-2234-TRANS-N SECTION.                                          
088000     SKIP2                                                                
088100     PERFORM S16-BERAKNA-KVPB                                             
088200     MOVE ZERO                          TO 2234-KVPB-SEP-TOT              
088300     MOVE REANTPSA-IDEDIT(INDX)         TO 2234-REANTPSA-NY               
088400     MOVE ZERO                          TO 2234-REANTPSA-GAMMAL           
088500     MOVE 'N'                           TO 2234-KDISATS                   
088600     MOVE MID-TISTADAT-IN(INDX)         TO 2234-TIBEHDAT                  
088700     MOVE W-IDARTNR-STR                 TO 2234-IDARTNR-SATS              
088800     MOVE MID-IDARTNR-IN(INDX)          TO 2234-IDARTNR-ING               
088900     PERFORM IMS-ISRT-XXBY                                                
089000     .                                                                    
089100     EJECT                                                                
089200 FC-TA-BORT-2234-TRANS   SECTION.                                         
089300     SKIP2                                                                
089400     MOVE SATB11-RAD-REANTPSA          TO WS-REANTPSA                     
089500     MOVE SATB11-RAD-IDARTNR           TO WS-IDARTNR-ING                  
089600     MOVE SATB11-RAD-TISTADAT          TO DAT-I-TIDATUM                   
089700     MOVE 'AAMMDD'                     TO DAT-KDDATFORM                   
089800     PERFORM S99-CALL-WDATKONV                                            
089900     IF DAT-KDSVAR-OK                                                     
090000        MOVE DAT-TIAA-VECKA            TO WS-AAR                          
090100        MOVE DAT-TIVV                  TO WS-VECKA                        
090200        MOVE NEJ                       TO SW-TRAEFF                       
090300        PERFORM IMS-GU-XXBY01                                             
090400        PERFORM IMS-GHNP-XXBY11                                           
090500        PERFORM UNTIL SW-TRAEFF = JA                                      
090600        OR SEGMENT-SAKNAS                                                 
090700           IF  2234-REANTPSA-NY       = WS-REANTPSA                       
090800           AND 2234-REANTPSA-GAMMAL   = ZERO                              
090900           AND 2234-KDISATS           = NEJ                               
091000           AND 2234-TIBEHDAT          = WS-TIAAVV                         
091100           AND 2234-IDARTNR-SATS      = W-IDARTNR-STR                     
091200           AND 2234-IDARTNR-ING       = WS-IDARTNR-ING                    
091300              MOVE JA                  TO SW-TRAEFF                       
091400              PERFORM IMS-DLET-XXBY                                       
091500           ELSE                                                           
091600              PERFORM IMS-GHNP-XXBY11                                     
091700           END-IF                                                         
091800        END-PERFORM                                                       
091900     END-IF                                                               
092000     .                                                                    
092100     EJECT                                                                
092200 G-LAES-VISA-INFO SECTION.                                                
092300     SKIP2                                                                
092400     IF MFS-IDPFK = '8'                                                   
092500     AND MID-IDRADNR-NEXT = ZERO                                          
092600        MOVE MFS-ROER-EJ-FAELT           TO MOD-IDRADNR-ENTER             
092700                                            MOD-IDRADNR-NEXT              
092800        PERFORM S11-RENSA-RADER                                           
092900        PERFORM S13-MFS-FORMATETS-ATTR                                    
093000        IF MOD-FLEXFORP-UT = NEJ                                          
093100           MOVE SPACE                    TO MOD-TEMFSFEL                  
093200        END-IF                                                            
093300     ELSE                                                                 
093400        MOVE +1                          TO INDX                          
093500        PERFORM IMS-GNP-SATB11                                            
093600        IF SEGMENT-FINNS                                                  
093700          IF MOD-FLEXFORP-UT = NEJ                                        
093800              MOVE SPACE                 TO MOD-TEMFSFEL                  
093900           END-IF                                                         
094000          MOVE SATB11-RAD-IDRADNR        TO MOD-IDRADNR-ENTER             
094100        ELSE                                                              
094200          MOVE ZERO TO MOD-IDRADNR-ENTER                                  
094300        END-IF                                                            
094400        PERFORM UNTIL INDX > 3                                            
094500          IF SEGMENT-FINNS                                                
094600             MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                    
094700             MOVE DAGENS-TIAAMMDD       TO TMP2-YYMMDD                    
094800             PERFORM WY2000P1                                             
094900             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
095000               MOVE SATB11-RAD-IDRADNR TO MOD-IDRADNR(INDX)               
095100                                          W-IDRADNR-2                     
095200               INSPECT MOD-IDRADNR(INDX) REPLACING LEADING ZERO           
095300                                              BY SPACE                    
095400               MOVE SATB11-RAD-IDARTNR TO MOD-IDARTNR-UT(INDX)            
095500                                               W-IDARTNR                  
095600               MOVE SATB11-RAD-REANTPSA  TO MOD-REANTPSA-UT(INDX)         
095700               MOVE SATB11-RAD-TISTADAT  TO DAT-I-TIDATUM                 
095800               MOVE 'AAMMDD'             TO DAT-KDDATFORM                 
095900               PERFORM S99-CALL-WDATKONV                                  
096000               IF DAT-KDSVAR-OK                                           
096100                  MOVE DAT-TIAA-VECKA    TO WS-AAR                        
096200                  MOVE DAT-TIVV          TO WS-VECKA                      
096300                  MOVE WS-TIAAVV         TO MOD-TISTADAT-UT(INDX)         
096400               ELSE                                                       
096500                  MOVE MFS-RENSA-FAELT   TO MOD-TISTADAT-UT(INDX)         
096600               END-IF                                                     
096700               IF   SATB11-RAD-TISTODAT  = 999999                         
096800                  MOVE SATB11-RAD-TISTODAT TO                             
096900                                           MOD-TISTODAT-UT(INDX)          
097000               ELSE                                                       
097100                  MOVE SATB11-RAD-TISTODAT TO DAT-I-TIDATUM               
097200                  MOVE 'AAMMDD'            TO DAT-KDDATFORM               
097300                  PERFORM S99-CALL-WDATKONV                               
097400                  IF DAT-KDSVAR-OK                                        
097500                     MOVE DAT-TIAA-VECKA   TO WS-AAR                      
097600                     MOVE DAT-TIVV         TO WS-VECKA                    
097700                     MOVE WS-TIAAVV        TO                             
097800                                           MOD-TISTODAT-UT(INDX)          
097900                  ELSE                                                    
098000                     MOVE MFS-RENSA-FAELT  TO                             
098100                                           MOD-TISTODAT-UT(INDX)          
098200                  END-IF                                                  
098300               END-IF                                                     
098400               PERFORM IMS-GU-BENA                                        
098500               IF SEGMENT-FINNS                                           
098600                 MOVE BENA01-TEXT-BEART    TO                             
098700                                           MOD-BEART-SVE-UT(INDX)         
098800               ELSE                                                       
098900                 MOVE MFS-RENSA-FAELT      TO                             
099000                                           MOD-BEART-SVE-UT(INDX)         
099100               END-IF                                                     
099200               MOVE MFS-RENSA-FAELT TO MOD-BORT (INDX)                    
099300                                       MOD-IDARTNR-IN (INDX)              
099400                                       MOD-REANTPSA-IN (INDX)             
099500                                       MOD-TISTADAT-IN (INDX)             
099600                                       MOD-TISTODAT-IN (INDX)             
099700                                       MOD-TESTRNOT (INDX)                
099800               MOVE MFS-FORMATETS-ATTR TO                                 
099900                                      MOD-BORT-ATTR        (INDX)         
100000                                      MOD-IDARTNR-IN-ATTR  (INDX)         
100100                                      MOD-REANTPSA-IN-ATTR (INDX)         
100200                                      MOD-TISTADAT-IN-ATTR (INDX)         
100300                                      MOD-TISTODAT-IN-ATTR (INDX)         
100400                                      MOD-TESTRNOT-ATTR    (INDX)         
100500               PERFORM IMS-GU-SATB11                                      
100600               PERFORM IMS-GNP-SATB22                                     
100700               IF SEGMENT-FINNS                                           
100800                 MOVE SATB22-NOT-TESTRNOT(1) TO MOD-TESTRNOT              
100900                                                (INDX)                    
101000               ELSE                                                       
101100                 MOVE MFS-RENSA-FAELT        TO MOD-TESTRNOT              
101200                                                (INDX)                    
101300               END-IF                                                     
101400               ADD +1 TO INDX                                             
101500            END-IF                                                        
101600                                                                          
101700            PERFORM IMS-GNP-SATB11                                        
101800                                                                          
101900          ELSE                                                            
102000                                                                          
102100            MOVE MFS-RENSA-FAELT         TO                               
102200                                     MOD-IDRADNR     (INDX)               
102300                                     MOD-IDARTNR-UT  (INDX)               
102400                                     MOD-BEART-SVE-UT(INDX)               
102500                                     MOD-REANTPSA-UT (INDX)               
102600                                     MOD-TISTADAT-UT (INDX)               
102700                                     MOD-TISTODAT-UT (INDX)               
102800                                     MOD-BORT        (INDX)               
102900                                     MOD-IDARTNR-IN  (INDX)               
103000                                     MOD-REANTPSA-IN (INDX)               
103100                                     MOD-TISTADAT-IN (INDX)               
103200                                     MOD-TISTODAT-IN (INDX)               
103300                                     MOD-TESTRNOT    (INDX)               
103400            MOVE MFS-FORMATETS-ATTR      TO                               
103500                                MOD-BORT-ATTR        (INDX)               
103600                                MOD-IDARTNR-IN-ATTR  (INDX)               
103700                                MOD-REANTPSA-IN-ATTR (INDX)               
103800                                MOD-TISTADAT-IN-ATTR (INDX)               
103900                                MOD-TISTODAT-IN-ATTR (INDX)               
104000                                MOD-TESTRNOT-ATTR    (INDX)               
104100          ADD +1 TO INDX                                                  
104200          END-IF                                                          
104300        END-PERFORM                                                       
104400                                                                          
104500        IF SEGMENT-FINNS                                                  
104600           MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                      
104700           MOVE DAGENS-TIAAMMDD       TO TMP2-YYMMDD                      
104800           PERFORM WY2000P1                                               
104900           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
105000              MOVE SATB11-RAD-IDRADNR    TO MOD-IDRADNR-NEXT              
105100              IF NOT MFS-UPDATE                                           
105200                 MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSFEL               
105300                 CALL WMEDKONV USING MED-WMEDAREA                         
105400                 MOVE MED-MFSFEL            TO MOD-TEMFSINF               
105500              END-IF                                                      
105600           ELSE                                                           
105700              MOVE NEJ TO WS-FORTSRAD                                     
105800              PERFORM IMS-GNP-SATB11                                      
105900                                                                          
106000              PERFORM UNTIL SEGMENT-SAKNAS OR WS-FORTSRAD = JA            
106100                 MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                
106200                 MOVE DAGENS-TIAAMMDD       TO TMP2-YYMMDD                
106300                 PERFORM WY2000P1                                         
106400                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
106500                    MOVE SATB11-RAD-IDRADNR TO MOD-IDRADNR-NEXT           
106600                    MOVE JA TO  WS-FORTSRAD                               
106700                    IF NOT MFS-UPDATE                                     
106800                       MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSFEL         
106900                       CALL WMEDKONV USING MED-WMEDAREA                   
107000                       MOVE MED-MFSFEL            TO MOD-TEMFSINF         
107100                    END-IF                                                
107200                 ELSE                                                     
107300                    PERFORM IMS-GNP-SATB11                                
107400                 END-IF                                                   
107500              END-PERFORM                                                 
107600              IF SEGMENT-SAKNAS AND WS-FORTSRAD = NEJ                     
107700                 MOVE ZERO TO MOD-IDRADNR-NEXT                            
107800              END-IF                                                      
107900           END-IF                                                         
108000        ELSE                                                              
108100           MOVE ZERO                     TO MOD-IDRADNR-NEXT              
108200        END-IF                                                            
108300     END-IF                                                               
108400     .                                                                    
108500     EJECT                                                                
108600 H-KOLLA-PF9 SECTION.                                                     
108700     MOVE SPACE                       TO 1231-MID-LEV-IN                  
108800     MOVE ZERO                        TO 1231-MID-KDPRODSL-IN             
108900     MOVE W-IDSKYLT                   TO 1231-MID-IDSKYLT-IN              
109000     PERFORM IMS-ISRT-ALT-MSG                                             
109100     .                                                                    
109200     EJECT                                                                
109300 S01-KOLLA-IDARTNR SECTION.                                               
109400     SKIP2                                                                
109500     IF  MID-IDARTNR-IN(INDX) NUMERIC                                     
109600     AND MID-IDARTNR-IN(INDX) NOT  = WS-IDARTNR-STR                       
109700        MOVE MFS-NUM-FAELT-RAETT        TO                                
109800                                 MOD-IDARTNR-IN-ATTR(INDX)                
109900     ELSE                                                                 
110000        MOVE NEJ                        TO INDATA-SW                      
110100        MOVE MFS-NUM-FAELT-FEL          TO                                
110200                                 MOD-IDARTNR-IN-ATTR(INDX)                
110300     END-IF                                                               
110400     .                                                                    
110500     EJECT                                                                
110600 S02-KOLLA-REANTPSA   SECTION.                                            
110700     SKIP2                                                                
110800     MOVE MID-REANTPSA-IN(INDX)      TO DEC-IDFRIDATA                     
110900     MOVE +2                         TO DEC-KVHELTAL                      
111000     MOVE +3                         TO DEC-KVDECIMAL                     
111100     CALL WDECEDIT USING DEC-WDECAREA                                     
111200     IF DEC-KDSVAR-FEL OR DEC-IDEDITDATA <= ZERO                          
111300        MOVE NEJ                     TO INDATA-SW                         
111400        MOVE MFS-NUM-FAELT-FEL       TO                                   
111500                              MOD-REANTPSA-IN-ATTR(INDX)                  
111600     ELSE                                                                 
111700        MOVE MFS-NUM-FAELT-RAETT     TO                                   
111800                              MOD-REANTPSA-IN-ATTR(INDX)                  
111900        MOVE DEC-IDEDITDATA TO REANTPSA-IDEDIT(INDX)                      
112000     END-IF                                                               
112100     .                                                                    
112200     EJECT                                                                
112300 S03-KOLLA-TISTADAT   SECTION.                                            
112400     SKIP2                                                                
112500     MOVE 'AAVV  '                     TO DAT-KDDATFORM                   
112600     MOVE MID-TISTADAT-IN(INDX)        TO DAT-I-TIDATUM                   
112700     PERFORM S99-CALL-WDATKONV                                            
112800     IF DAT-KDSVAR-OK                                                     
112900        MOVE DAT-TIAA-VECKA            TO WS-AAR                          
113000        MOVE DAT-TIVV                  TO WS-VECKA                        
113100        MOVE DAT-TIAAMMDD              TO WS-TIAAMMDD                     
113200        MOVE DAGENS-AAVV   TO TMP1-YYWW                                   
113300        MOVE WS-TIAAVV     TO TMP2-YYWW                                   
113400        PERFORM WY2000P3                                                  
113500        IF TMP1-YYWW <= TMP2-YYWW                                         
113600        AND WS-TIAAVV           < 9953                                    
113700           MOVE MFS-NUM-FAELT-RAETT   TO                                  
113800                                 MOD-TISTADAT-IN-ATTR(INDX)               
113900        ELSE                                                              
114000           MOVE NEJ                   TO INDATA-SW                        
114100           MOVE MFS-NUM-FAELT-FEL     TO                                  
114200                                 MOD-TISTADAT-IN-ATTR(INDX)               
114300        END-IF                                                            
114400     ELSE                                                                 
114500        MOVE NEJ                      TO INDATA-SW                        
114600        MOVE MFS-NUM-FAELT-FEL        TO                                  
114700                                 MOD-TISTADAT-IN-ATTR(INDX)               
114800     END-IF                                                               
114900     .                                                                    
115000     EJECT                                                                
115100 S04-KOLLA-TISTODAT   SECTION.                                            
115200     SKIP2                                                                
115300     MOVE 'AAVV  '                     TO DAT-KDDATFORM                   
115400     MOVE MID-TISTODAT-IN(INDX)        TO DAT-I-TIDATUM                   
115500     PERFORM S99-CALL-WDATKONV                                            
115600     IF DAT-KDSVAR-OK                                                     
115700        MOVE DAT-TIAA-VECKA            TO WS-AAR                          
115800        MOVE DAT-TIVV                  TO WS-VECKA                        
115900        MOVE DAT-TIAAMMDD              TO WS-TIAAMMDD                     
116000        MOVE DAGENS-AAVV   TO TMP1-YYWW                                   
116100        MOVE WS-TIAAVV     TO TMP2-YYWW                                   
116200        PERFORM WY2000P3                                                  
116300        IF TMP1-YYWW <= TMP2-YYWW                                         
116400           MOVE MFS-NUM-FAELT-RAETT   TO                                  
116500                                 MOD-TISTODAT-IN-ATTR(INDX)               
116600        ELSE                                                              
116700           MOVE NEJ                   TO INDATA-SW                        
116800           MOVE MFS-NUM-FAELT-FEL     TO                                  
116900                                 MOD-TISTODAT-IN-ATTR(INDX)               
117000        END-IF                                                            
117100     ELSE                                                                 
117200        IF MID-TISTODAT-IN(INDX) = 9999                                   
117300           MOVE MFS-NUM-FAELT-RAETT   TO                                  
117400                                 MOD-TISTODAT-IN-ATTR(INDX)               
117500        ELSE                                                              
117600           MOVE NEJ                   TO INDATA-SW                        
117700           MOVE MFS-NUM-FAELT-FEL     TO                                  
117800                                    MOD-TISTODAT-IN-ATTR(INDX)            
117900        END-IF                                                            
118000     END-IF                                                               
118100     .                                                                    
118200     EJECT                                                                
118300 S05-TESTA-IDARTNR-MOT-ARTREG   SECTION.                                  
118400     SKIP2                                                                
118500     MOVE MID-IDARTNR-IN (INDX)      TO W-IDARTNR                         
118600     PERFORM IMS-GU-ARTC01                                                
118700     IF SEGMENT-FINNS                                                     
118800        MOVE ART-KDPRODSL            TO TEST-KDPRODSL                     
118900        IF KDPRODSL-VOLVO-EMB                                             
119000           IF  ART-KDERS-UTG > ZERO                                       
119100              MOVE NEJ               TO INDATA-SW                         
119200              MOVE MFS-NUM-FAELT-FEL TO                                   
119300                               MOD-IDARTNR-IN-ATTR(INDX)                  
119400           ELSE                                                           
119500              PERFORM IMS-GU-ARTC11                                       
119600              IF SEGMENT-FINNS                                            
119700                 IF  CLAG-KDERS > ZERO                                    
119800                    MOVE NEJ            TO INDATA-SW                      
119900                    MOVE MFS-NUM-FAELT-FEL   TO                           
120000                                  MOD-IDARTNR-IN-ATTR(INDX)               
120100                 END-IF                                                   
120200              END-IF                                                      
120300           END-IF                                                         
120400        ELSE                                                              
120500           MOVE NEJ                  TO INDATA-SW                         
120600           MOVE MFS-NUM-FAELT-FEL    TO                                   
120700                               MOD-IDARTNR-IN-ATTR(INDX)                  
120800        END-IF                                                            
120900     ELSE                                                                 
121000        MOVE NEJ                     TO INDATA-SW                         
121100        MOVE MFS-NUM-FAELT-FEL       TO                                   
121200                               MOD-IDARTNR-IN-ATTR(INDX)                  
121300     END-IF                                                               
121400     .                                                                    
121500     EJECT                                                                
121600 S06-KOLLA-OM-ALL-PLUS    SECTION.                                        
121700     SKIP2                                                                
121800     IF  MID-INDATA       (1)  NOT = ALL '+'                              
121900     OR  MID-INDATA       (2)  NOT = ALL '+'                              
122000     OR  MID-INDATA       (3)  NOT = ALL '+'                              
122100     OR  MID-FLEXFORP-IN       NOT = ALL '+'                              
122200     OR  MID-KLAR              NOT = ALL '+'                              
122300        MOVE JA                           TO SW-IFYLLT                    
122400     END-IF                                                               
122500     .                                                                    
122600     EJECT                                                                
122700 S10-RENSA-ALLT         SECTION.                                          
122800     SKIP2                                                                
122900     MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-STR-UT                       
123000                                 MOD-BEART                                
123100                                 MOD-IDLEVNR                              
123200                                 MOD-FLEXFORP-IN                          
123300                                 MOD-KLAR                                 
123400     PERFORM S11-RENSA-RADER                                              
123500     .                                                                    
123600     EJECT                                                                
123700 S11-RENSA-RADER        SECTION.                                          
123800     SKIP2                                                                
123900     MOVE +1                  TO INDX                                     
124000     PERFORM UNTIL INDX > +3                                              
124100         MOVE MFS-RENSA-FAELT TO MOD-IDRADNR     (INDX)                   
124200                                 MOD-IDARTNR-UT  (INDX)                   
124300                                 MOD-BEART-SVE-UT(INDX)                   
124400                                 MOD-REANTPSA-UT (INDX)                   
124500                                 MOD-TISTADAT-UT (INDX)                   
124600                                 MOD-TISTODAT-UT (INDX)                   
124700                                 MOD-BORT        (INDX)                   
124800                                 MOD-IDARTNR-IN  (INDX)                   
124900                                 MOD-REANTPSA-IN (INDX)                   
125000                                 MOD-TISTADAT-IN (INDX)                   
125100                                 MOD-TISTODAT-IN (INDX)                   
125200                                 MOD-TESTRNOT    (INDX)                   
125300         ADD +1               TO INDX                                     
125400     END-PERFORM                                                          
125500     .                                                                    
125600     EJECT                                                                
125700 S12-ADD-LAES-IN-FAELT      SECTION.                                      
125800     SKIP2                                                                
125900     MOVE MFS-ADD-LAES-IN-FAELT  TO                                       
126000                                 MOD-FLEXFORP-IN-ATTR                     
126100                                 MOD-KLAR-ATTR                            
126200     MOVE +1                  TO INDX                                     
126300     PERFORM UNTIL INDX > +3                                              
126400        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
126500                                    MOD-BORT-ATTR        (INDX)           
126600                                    MOD-IDARTNR-IN-ATTR  (INDX)           
126700                                    MOD-REANTPSA-IN-ATTR (INDX)           
126800                                    MOD-TISTADAT-IN-ATTR (INDX)           
126900                                    MOD-TISTODAT-IN-ATTR (INDX)           
127000                                    MOD-TESTRNOT-ATTR    (INDX)           
127100         ADD +1               TO INDX                                     
127200     END-PERFORM                                                          
127300     .                                                                    
127400     EJECT                                                                
127500 S13-MFS-FORMATETS-ATTR SECTION.                                          
127600     SKIP2                                                                
127700     MOVE MFS-FORMATETS-ATTR     TO                                       
127800                                 MOD-FLEXFORP-IN-ATTR                     
127900                                 MOD-KLAR-ATTR                            
128000     MOVE +1                 TO INDX                                      
128100     PERFORM UNTIL INDX > +3                                              
128200       MOVE MFS-FORMATETS-ATTR TO                                         
128300                                    MOD-BORT-ATTR        (INDX)           
128400                                    MOD-IDARTNR-IN-ATTR  (INDX)           
128500                                    MOD-REANTPSA-IN-ATTR (INDX)           
128600                                    MOD-TISTADAT-IN-ATTR (INDX)           
128700                                    MOD-TISTODAT-IN-ATTR (INDX)           
128800                                    MOD-TESTRNOT-ATTR    (INDX)           
128900         ADD +1                TO INDX                                    
129000     END-PERFORM                                                          
129100     .                                                                    
129200     EJECT                                                                
129300 S14-MFS-ROER-EJ-FAELT SECTION.                                           
129400                                                                          
129500*    --- ALLA UTDATA-FÄLT                                                 
129600     MOVE MFS-ROER-EJ-FAELT   TO MOD-KDSTRRAD-ENTER                       
129700                                 MOD-IDRADNR-ENTER                        
129800                                 MOD-KDSTRRAD-NEXT                        
129900                                 MOD-IDRADNR-NEXT                         
130000                                 MOD-BEART                                
130100                                 MOD-IDLEVNR                              
130200                                 MOD-FLEXFORP-UT                          
130300                                 MOD-FLEXFORP-IN                          
130400                                 MOD-KLAR                                 
130500     MOVE +1                  TO INDX                                     
130600     PERFORM UNTIL INDX > +3                                              
130700       MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR     (INDX)                   
130800                                 MOD-IDARTNR-UT  (INDX)                   
130900                                 MOD-BEART-SVE-UT(INDX)                   
131000                                 MOD-REANTPSA-UT (INDX)                   
131100                                 MOD-TISTADAT-UT (INDX)                   
131200                                 MOD-TISTODAT-UT (INDX)                   
131300                                 MOD-BORT        (INDX)                   
131400                                 MOD-IDARTNR-IN  (INDX)                   
131500                                 MOD-REANTPSA-IN (INDX)                   
131600                                 MOD-TISTADAT-IN (INDX)                   
131700                                 MOD-TISTODAT-IN (INDX)                   
131800                                 MOD-TESTRNOT    (INDX)                   
131900         ADD +1               TO INDX                                     
132000     END-PERFORM                                                          
132100     .                                                                    
132200     EJECT                                                                
132300 S15-FIXA-DAGENS-AAVV SECTION.                                            
132400     SKIP2                                                                
132500     MOVE 'AAMMDD'                     TO DAT-KDDATFORM                   
132600     ACCEPT  DAT-I-TIDATUM  FROM DATE                                     
132700     PERFORM S99-CALL-WDATKONV                                            
132800     IF DAT-KDSVAR-OK                                                     
132900        MOVE DAT-TIAA-VECKA            TO DAGENS-AAR                      
133000        MOVE DAT-TIVV                  TO DAGENS-VECKA                    
133100     ELSE                                                                 
133200        MOVE ZERO                      TO DAGENS-AAR                      
133300                                          DAGENS-VECKA                    
133400     END-IF                                                               
133500     .                                                                    
133600     EJECT                                                                
133700 S16-BERAKNA-KVPB   SECTION.                                              
133800     SKIP2                                                                
133900     MOVE W-IDARTNR-STR                 TO W-IDARTNR                      
134000     PERFORM IMS-GU-ARTC01                                                
134100     IF SEGMENT-FINNS                                                     
134200        MOVE ART-FLIART                 TO WS-FLIART                      
134300        MOVE ZERO                       TO SPAR-KVPB-SEP-C1               
134400                                           SPAR-KVPB-SEP-C2               
134500     END-IF                                                               
134600     .                                                                    
134700     EJECT                                                                
134800 S99-CALL-WDATKONV   SECTION.                                             
134900     SKIP2                                                                
135000     CALL WDATKONV    USING DAT-KDDATFORM                                 
135100                            DAT-I-TIDATUM                                 
135200                            DAT-O-TIDATUM                                 
135300                            DAT-KDSVAR                                    
135400     .                                                                    
135500     EJECT                                                                
135600 IMS-GET-MSG SECTION.                                                     
135700                                                                          
135800     MOVE '  QC' TO GODK-STATUSKODER                                      
135900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
136000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     EJECT                                                                
136400 IMS-ISRT-ALT-MSG SECTION.                                                
136500                                                                          
136600     MOVE SPACE TO GODK-STATUSKODER                                       
136700     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
136800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
136900     PERFORM IMS-STATUSKONTROLL                                           
137000     .                                                                    
137100     EJECT                                                                
137200 IMS-INSERT-MSG SECTION.                                                  
137300                                                                          
137400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
137500       MOVE '0' TO MFS-KDHUVOMR                                           
137600     END-IF                                                               
137700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
137800     MOVE SPACE TO GODK-STATUSKODER                                       
137900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
138000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138100     PERFORM IMS-STATUSKONTROLL                                           
138200     .                                                                    
138300     EJECT                                                                
138400 IMS-GU-ARTC01 SECTION.                                                   
138500     SKIP2                                                                
138600     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
138700          DELIMITED BY SIZE INTO SSA1                                     
138800     MOVE '  GE' TO GODK-STATUSKODER                                      
138900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
139000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
139100     PERFORM IMS-STATUSKONTROLL                                           
139200     .                                                                    
139300     EJECT                                                                
139400 IMS-GHU-ARTC01 SECTION.                                                  
139500     SKIP2                                                                
139600     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
139700          DELIMITED BY SIZE INTO SSA1                                     
139800     MOVE '  ' TO GODK-STATUSKODER                                        
139900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
140000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
140100     PERFORM IMS-STATUSKONTROLL                                           
140200     .                                                                    
140300     EJECT                                                                
140400 IMS-GU-ARTC11  SECTION.                                                  
140500                                                                          
140600     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
140700          DELIMITED BY SIZE INTO SSA1                                     
140800     MOVE 'WLARTC11 ' TO SSA2                                             
140900     MOVE '  GE' TO GODK-STATUSKODER                                      
141000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
141100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
141200     PERFORM IMS-STATUSKONTROLL                                           
141300     .                                                                    
141400     EJECT                                                                
141500 IMS-REPL-ARTC SECTION.                                                   
141600                                                                          
141700     MOVE '  ' TO GODK-STATUSKODER                                        
141800     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
141900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
142000     PERFORM IMS-STATUSKONTROLL                                           
142100     .                                                                    
142200     EJECT                                                                
142300 IMS-GU-BENA  SECTION.                                                    
142400                                                                          
142500     STRING 'WLBENA01(WDD3BSEQ= ' W-IDARTNR-X ')'                         
142600          DELIMITED BY SIZE INTO SSA1                                     
142700     STRING 'WLBENA11(IDSKYLT = ' W-IDSKYLT-X ')'                         
142800          DELIMITED BY SIZE INTO SSA2                                     
142900     MOVE '  GE' TO GODK-STATUSKODER                                      
143000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
143100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
143200     PERFORM IMS-STATUSKONTROLL                                           
143300     .                                                                    
143400     EJECT                                                                
143500 IMS-GU-SATB01 SECTION.                                                   
143600                                                                          
143700     STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-STR-X ')'                     
143800          DELIMITED BY SIZE INTO SSA1                                     
143900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
144000     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
144100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
144200     PERFORM IMS-STATUSKONTROLL                                           
144300     .                                                                    
144400     EJECT                                                                
144500 IMS-GHU-SATB01 SECTION.                                                  
144600                                                                          
144700     STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-STR-X ')'                     
144800          DELIMITED BY SIZE INTO SSA1                                     
144900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
145000     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
145100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400     EJECT                                                                
145500 IMS-GU-SATB11 SECTION.                                                   
145600                                                                          
145700     STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-STR-X ')'                     
145800          DELIMITED BY SIZE INTO SSA1                                     
145900     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-2-X ')'                      
146000          DELIMITED BY SIZE INTO SSA2                                     
146100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
146200     CALL CBLTDLI USING GU SATB2-PCB DLI-IO-AREA SSA1 SSA2                
146300     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
146400     PERFORM IMS-STATUSKONTROLL                                           
146500     .                                                                    
146600     EJECT                                                                
146700*IMS-GU-SATB11-LAST SECTION.                                              
146800*                                                                         
146900*    STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-STR-X ')'                     
147000*         DELIMITED BY SIZE INTO SSA1                                     
147100*    STRING 'WLSATB11*L(KDSTRRAD= ' W-KDSTRRAD-X ')'                      
147200*         DELIMITED BY SIZE INTO SSA2                                     
147300*    MOVE '  GE' TO GODK-STATUSKODER                                      
147400*    CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1 SSA2                 
147500*    MOVE SATB-STATUS-CODE TO STATUS-WS                                   
147600*    PERFORM IMS-STATUSKONTROLL                                           
147700*    .                                                                    
147800*    EJECT                                                                
147900 IMS-GU-SATB11-LAST SECTION.                                              
148000                                                                          
148100     STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-STR-X ')'                     
148200          DELIMITED BY SIZE INTO SSA1                                     
148300     STRING 'WLSATB11*L(WDJ111KY< ' W-MAX-X ')'                           
148400          DELIMITED BY SIZE INTO SSA2                                     
148500     MOVE '  GE' TO GODK-STATUSKODER                                      
148600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1 SSA2                 
148700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     .                                                                    
149000     EJECT                                                                
149100 IMS-GHU-SATB11 SECTION.                                                  
149200                                                                          
149300     STRING 'WLSATB11(WDJ111KY= ' W-WDJ111KY-X ')'                        
149400          DELIMITED BY SIZE INTO SSA1                                     
149500     MOVE '  GE' TO GODK-STATUSKODER                                      
149600     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
149700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
149800     PERFORM IMS-STATUSKONTROLL                                           
149900     .                                                                    
150000     EJECT                                                                
150100 IMS-GNP-SATB11 SECTION.                                                  
150200     SKIP2                                                                
150300     STRING 'WLSATB11(WDJ111KY>=' W-WDJ111KY-X                            
150400                    '&KDSTRRAD =' W-KDSTRRAD-X ')'                        
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     MOVE '  GE' TO GODK-STATUSKODER                                      
150700     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
150800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
150900     PERFORM IMS-STATUSKONTROLL                                           
151000     .                                                                    
151100     SKIP2                                                                
151200 IMS-ISRT-SATB11 SECTION.                                                 
151300     SKIP2                                                                
151400     MOVE 'WLSATB11 ' TO SSA1                                             
151500     MOVE '  ' TO GODK-STATUSKODER                                        
151600     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1                    
151700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     .                                                                    
152000     EJECT                                                                
152100 IMS-GNP-SATB22 SECTION.                                                  
152200     STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-STR-X ')'                     
152300          DELIMITED BY SIZE INTO SSA1                                     
152400     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-2-X ')'                      
152500          DELIMITED BY SIZE INTO SSA2                                     
152600     STRING 'WLSATB22(IDSTRNOT= ' W-IDSTRNOT-X ')'                        
152700          DELIMITED BY SIZE INTO SSA3                                     
152800     MOVE '  GE' TO GODK-STATUSKODER                                      
152900     CALL CBLTDLI USING GNP SATB2-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
153000     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     EJECT                                                                
153400 IMS-GHNP-SATB22 SECTION.                                                 
153500     SKIP2                                                                
153600     STRING 'WLSATB01(IDARTNR = ' W-IDARTNR-STR-X ')'                     
153700          DELIMITED BY SIZE INTO SSA1                                     
153800     STRING 'WLSATB11(WDJ111KY= ' W-WDJ111KY-X ')'                        
153900          DELIMITED BY SIZE INTO SSA2                                     
154000     STRING 'WLSATB22(IDSTRNOT= ' W-IDSTRNOT-X ')'                        
154100          DELIMITED BY SIZE INTO SSA3                                     
154200     MOVE '  GE' TO GODK-STATUSKODER                                      
154300     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
154400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
154500     PERFORM IMS-STATUSKONTROLL                                           
154600     .                                                                    
154700     EJECT                                                                
154800 IMS-ISRT-SATB22 SECTION.                                                 
154900     SKIP2                                                                
155000     MOVE 'WLSATB22 ' TO SSA1                                             
155100     MOVE '  ' TO GODK-STATUSKODER                                        
155200     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1                    
155300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
155400     PERFORM IMS-STATUSKONTROLL                                           
155500     .                                                                    
155600     EJECT                                                                
155700 IMS-DLET-SATB SECTION.                                                   
155800                                                                          
155900     MOVE '  ' TO GODK-STATUSKODER                                        
156000     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
156100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     EJECT                                                                
156500 IMS-REPL-SATB   SECTION.                                                 
156600                                                                          
156700     MOVE '  ' TO GODK-STATUSKODER                                        
156800     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
156900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
157000     PERFORM IMS-STATUSKONTROLL                                           
157100     .                                                                    
157200     SKIP2                                                                
157300 IMS-GU-XXBY01 SECTION.                                                   
157400     SKIP2                                                                
157500     STRING 'WLXXBY01(WDG3KEY  =' W-2233-KEY-X ')'                        
157600          DELIMITED BY SIZE INTO SSA1                                     
157700     CALL CBLTDLI USING GU XXBY-PCB DLI-IO-AREA SSA1                      
157800     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
157900     PERFORM IMS-STATUSKONTROLL                                           
158000     .                                                                    
158100     EJECT                                                                
158200 IMS-GHNP-XXBY11 SECTION.                                                 
158300     SKIP2                                                                
158400     MOVE 'WLXXBY11 ' TO SSA1                                             
158500     MOVE '  GE' TO GODK-STATUSKODER                                      
158600     CALL CBLTDLI USING GHNP XXBY-PCB DLI-IO-AREA SSA1                    
158700     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
158800     PERFORM IMS-STATUSKONTROLL                                           
158900     .                                                                    
159000     SKIP2                                                                
159100 IMS-ISRT-XXBY SECTION.                                                   
159200     SKIP2                                                                
159300     STRING 'WLXXBY01(WDG3KEY  =' W-2233-KEY-X ')'                        
159400          DELIMITED BY SIZE INTO SSA1                                     
159500     MOVE 'WLXXBY11 ' TO SSA2                                             
159600     MOVE '   ' TO GODK-STATUSKODER                                       
159700     CALL CBLTDLI USING ISRT XXBY-PCB DLI-IO-AREA SSA1 SSA2               
159800     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
159900     PERFORM IMS-STATUSKONTROLL                                           
160000     .                                                                    
160100     SKIP2                                                                
160200 IMS-DLET-XXBY SECTION.                                                   
160300                                                                          
160400     MOVE '  ' TO GODK-STATUSKODER                                        
160500     CALL CBLTDLI USING DLET XXBY-PCB DLI-IO-AREA                         
160600     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900     EJECT                                                                
161000 IMS-STATUSKONTROLL SECTION.                                              
161100                                                                          
161200     SET STATUS-IX TO 1                                                   
161300     SEARCH GODK-STATUS                                                   
161400       AT END CALL FELLOG                                                 
161500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
161600     END-SEARCH                                                           
161700     .                                                                    
161800     EJECT                                                                
161900*    -COPY WY2000P1                                                       
162000     EJECT                                                                
162100*    -COPY WY2000P3                                                       
