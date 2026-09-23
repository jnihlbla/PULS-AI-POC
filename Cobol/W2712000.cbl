000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2712000.                                                
000400*AUTHOR.         ANNELIE ENGLUND.                                         
000500*DATE-WRITTEN.   94/12/19.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        UPPDATERAR FÖRÄNDRAD PROGNOS, BESTÄLLNINGSPUNKT,                 
001100*        BESTÄLLNINGSKVANTITET OCH ÖVERLAGERPUNKT                         
001200*                                                                         
001300*        SKAPAR EN FIL FÖR UPPFÖLJNING (W27120)                           
001400*                                                                         
001500*        THE PROGRAM UPDATES   WDK7                                       
001600*                                                                         
001700*    ABENDCODES:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- FIL MED AKTIVA ARTIKLAR                                    
003000     SELECT W27121                     ASSIGN TO W27120D1.                
003100     SKIP2                                                                
003200*          --- FIL MED SAMTLIGA ARTIKLAR FÖR UPPFÖLJNING                  
003300     SELECT W27120                     ASSIGN TO W27120D2.                
003400     EJECT                                                                
003500*          --- FIL MED SAMTLIGA ARTIKLAR FÖR UPPFÖLJNING                  
003600     SELECT W2712F                     ASSIGN TO W27120D3.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W27121                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W27121      -L.                                                
004700     SKIP3                                                                
004800 FD  W27120                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  POST -COPY W27120 -PRE UT-     -L.                                   
005300     EJECT                                                                
005400 FD  W2712F                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800*01  POST -COPY W2712F -PRE UT2-     -L.                                  
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100     SKIP2                                                                
006200*    -COPY WY2000W1                                                       
006300     SKIP3                                                                
006400 77  IDPGM                       PIC X(8)    VALUE 'W2712000'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  DEFINITIV                   PIC S9      VALUE +1  COMP-3.            
006800                                                                          
006900 01  ARBETSFALT.                                                          
007000     03 IX                       PIC 9(2)    VALUE ZERO.                  
007100     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007200     03  WS-PRIS-K6              PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007300                                                                          
007400 01  ERRTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700                                                                          
007800 77  W27121-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W27121                       VALUE 'Y'.                   
008000     EJECT                                                                
008100*                                                                         
008200 01  SW-DC-VALIDATE              PIC X       VALUE 'N'.                   
008300     88 DC-VALIDATE-JA                       VALUE 'J'.                   
008400     88 DC-VALIDATE-NEJ                      VALUE 'N'.                   
008500*                                                                         
008600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008700 01  FILLER REDEFINES TODAYS-DATE.                                        
008800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
009000     03  TODAYS-DATE-DAY         PIC 9(2).                                
009100*                                                                         
009200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700     EJECT                                                                
009800*      --- VALID IDDC CODES                                               
009900*                                                                         
010000*01    -COPY WWDC99                                                       
010100       EJECT                                                              
010200*01    -COPY WWDCKONS                                                     
010300       EJECT                                                              
010400*01    -COPY WWLNDKON                                                     
010500       EJECT                                                              
010600 01  GENERAL-SUBPROGRAM.                                                  
010700*                                                                         
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011200     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
011300     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
011400     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL POSTSUM                                          
011700*                                                                         
011800*01  -COPY W0005   -PRE  POSTSUM-                                         
011900     EJECT                                                                
012000*    --- PARAMETRAR TILL DATKORT                                          
012100*                                                                         
012200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27120'.              
012300     SKIP2                                                                
012400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012500     SKIP2                                                                
012600*01  -COPY WDATKORT                                                       
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL W271REFL                                         
012900*                                                                         
013000*01  -COPY W271REFL                                                       
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL W271UTUP                                         
013300*                                                                         
013400*01  -COPY W271UTUP                                                       
013500*    --- PARAMETRAR TILL W271UTIL                                         
013600*                                                                         
013700*01  -COPY W271UTIL                                                       
013800     EJECT                                                                
013900 01  IN-AREA-START               PIC X(24)   VALUE                        
014000                                             'IN-AREA-START'.             
014100     SKIP2                                                                
014200                                                                          
014300*01  AREA -COPY W27121     -PRE IN-                                       
014400*                                                                         
014500     EJECT                                                                
014600 01  UT-AREA-START               PIC X(24)   VALUE                        
014700                                             'UT-AREA-START'.             
014800     SKIP2                                                                
014900                                                                          
015000*01  AREA -COPY W27120     -PRE UT-                                       
015100*                                                                         
015200     EJECT                                                                
015300 01  UT2-AREA-START               PIC X(24)   VALUE                       
015400                                             'UT2-AREA-START'.            
015500     SKIP2                                                                
015600                                                                          
015700*01  AREA -COPY W2712F     -PRE UT2-                                      
015800*                                                                         
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016100     SKIP3                                                                
016200 01  KEYS-TILL-DLI.                                                       
016300     03  W-IDARTNR-X.                                                     
016400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016500     03  W-IDDC-X.                                                        
016600         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
016700     03  W-IDLAND-X.                                                      
016800         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
016900     03  W-IDLEVNR-X.                                                     
017000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017100     03  W-KDSEGKEY-X.                                                    
017200         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
017300     SKIP2                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FOUND                       VALUE '  '.                  
017700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017900     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
018000     88  IMS-NOT-OK                          VALUE 'XD'.                  
018100     SKIP2                                                                
018200 01  GOOD-STATUSCODES.                                                    
018300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(64).                               
018600 01  SSA2                        PIC X(64).                               
018700     EJECT                                                                
018800*    --- IMS FUNCTION CODES                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
019300 01  DLI-IO-WDK701.                                                       
019400*    03  -COPY WDK701                                                     
019500     EJECT                                                                
019600                                                                          
019700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
019800 01  DLI-IO-WDK711.                                                       
019900*    03  -COPY WDK711                                                     
020000     EJECT                                                                
020100                                                                          
020200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
020300 01  DLI-IO-WDK712.                                                       
020400*    03  -COPY WDK712                                                     
020500     EJECT                                                                
020600                                                                          
020700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF116'.                      
020800 01  DLI-IO-WDF116.                                                       
020900*    03  -COPY WDF116                                                     
021000     EJECT                                                                
021100                                                                          
021200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
021300 01  DLI-IO-WDK601.                                                       
021400*    03  -COPY WDK601                                                     
021500     EJECT                                                                
021600                                                                          
021700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
021800 01  DLI-IO-WDK611.                                                       
021900*    03  -COPY WDK611                                                     
022000     EJECT                                                                
022100                                                                          
022200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK621'.                      
022300 01  DLI-IO-WDK621.                                                       
022400*    03  -COPY WDK621                                                     
022500     EJECT                                                                
022600                                                                          
022700                                                                          
022800 LINKAGE SECTION.                                                         
022900                                                                          
023000*01  -COPY W0009   -PRE MSG-                                              
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE WDK7-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE WDK6-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE WDF1-                                              
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100*01  -COPY W0008  -PRE REFL-2501-                                         
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008      -PRE WDB6-                                          
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE REFL-WDK7-                                         
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000 01  REFL1-UTIL-WDK6-PCB         PIC X.                                   
025100 01  REFL1-UTIL-WDK7-PCB         PIC X.                                   
025200 01  REFL1-UTIL-WDB6-PCB         PIC X.                                   
025300*****W271UTUP**********                                                   
025400 01  UTUP1-WDK7-PCB              PIC X.                                   
025500 01  UTUP1-WDB6-PCB              PIC X.                                   
025600 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
025700 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
025800 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
025900     EJECT                                                                
026000 PROCEDURE DIVISION  USING                                                
026100                           MSG-PCB  WDK7-PCB WDK6-PCB                     
026200                           WDF1-PCB REFL-2501-PCB                         
026300                           WDB6-PCB REFL-WDK7-PCB                         
026400                           REFL1-UTIL-WDK6-PCB                            
026500                           REFL1-UTIL-WDK7-PCB                            
026600                           REFL1-UTIL-WDB6-PCB                            
026700                           UTUP1-WDK7-PCB                                 
026800                           UTUP1-WDB6-PCB                                 
026900                           UTUP1-UTIL-WDK6-PCB                            
027000                           UTUP1-UTIL-WDK7-PCB                            
027100                           UTUP1-UTIL-WDB6-PCB.                           
027200     ENTRY 'DLITCBL' USING                                                
027300                           MSG-PCB  WDK7-PCB WDK6-PCB                     
027400                           WDF1-PCB REFL-2501-PCB                         
027500                           WDB6-PCB REFL-WDK7-PCB                         
027600                           REFL1-UTIL-WDK6-PCB                            
027700                           REFL1-UTIL-WDK7-PCB                            
027800                           REFL1-UTIL-WDB6-PCB                            
027900                           UTUP1-WDK7-PCB                                 
028000                           UTUP1-WDB6-PCB                                 
028100                           UTUP1-UTIL-WDK6-PCB                            
028200                           UTUP1-UTIL-WDK7-PCB                            
028300                           UTUP1-UTIL-WDB6-PCB.                           
028400                                                                          
028500     PERFORM A-INIT                                                       
028600     PERFORM S01-READ-W27121                                              
028700                                                                          
028800     PERFORM UNTIL END-OF-W27121                                          
028900                                                                          
029000                                                                          
029100       INITIALIZE REFL-W271REFL                                           
029200       MOVE ZERO           TO  WS-PRIS                                    
029300                               WS-PRIS-K6                                 
029400       MOVE NEJ            TO  SW-DC-VALIDATE                             
029500                                                                          
029600       MOVE IN-IDDC        TO WS-IDDC                                     
029700                              REFL-IDDC                                   
029800                              W-IDDC                                      
029900       MOVE IN-IDREFTAB    TO REFL-IDREFTAB                               
030000       MOVE IN-FLWILSON    TO REFL-FLWILSON                               
030100       MOVE IN-IDLEVNR-DC  TO REFL-IN-IDLEVNR-DC                          
030200                                                                          
030300       MOVE IN-IDARTNR     TO W-IDARTNR                                   
030400                              REFL-IDARTNR                                
030500       PERFORM IMS-GU-WDK601                                              
030600       PERFORM IMS-GNP-WDK611                                             
030700       IF SEGMENT-FOUND                                                   
030800         IF NDC-CN OR NDC-NA                                              
030900           IF NDC-CN                                                      
031000             MOVE WC-LAND-CN TO W-IDLAND                                  
031100           END-IF                                                         
031200           IF NDC-US                                                      
031300             MOVE WC-LAND-US TO W-IDLAND                                  
031400           END-IF                                                         
031500           IF NDC-CA                                                      
031600             MOVE WC-LAND-CA TO W-IDLAND                                  
031700           END-IF                                                         
031800           SET DC-VALIDATE-JA TO TRUE                                     
031900         ELSE                                                             
032000           MOVE CLAG-PRARTSTD TO REFL-PRARTBES                            
032100           PERFORM IMS-GNP-WDK621                                         
032200           PERFORM UNTIL SEGMENT-MISSING                                  
032300                      OR (PRL-IDLEVNR = ART-IDLEVNR                       
032400                      AND PRL-KDSTATUS-PR = DEFINITIV)                    
032500              PERFORM IMS-GNP-WDK621                                      
032600           END-PERFORM                                                    
032700                                                                          
032800           IF SEGMENT-MISSING                                             
032900              MOVE CLAG-PRARTSTD TO WS-PRIS-K6                            
033000           ELSE                                                           
033100              MOVE PRL-PRARTBES-PR TO WS-PRIS-K6                          
033200           END-IF                                                         
033300         END-IF                                                           
033400                                                                          
033500*TO PLACE IMS POINTER AT A CLOSER POSITION                                
033600         PERFORM IMS-GU-K701                                              
033700*                                                                         
033800         PERFORM IMS-GNP-K711                                             
033900                                                                          
034000         MOVE SLAG-FLFLYG TO REFL-FLFLYG                                  
034100         MOVE IN-IDDC-REF TO REFL-IDDC-REF                                
034200                                                                          
034300         IF DC-VALIDATE-JA                                                
034400           PERFORM IMS-GNP-WDK712                                         
034500           MOVE LART-PRMATRL TO REFL-PRARTBES                             
034600                                WS-PRIS                                   
034700         END-IF                                                           
034800                                                                          
034900         MOVE 1 TO IX                                                     
035000         PERFORM UNTIL IX > 12                                            
035100            MOVE IN-RESEASON(IX) TO REFL-RESEASON(IX)                     
035200            ADD 1 TO IX                                                   
035300         END-PERFORM                                                      
035400                                                                          
035500         IF NDC                                                           
035600            MOVE ZERO                  TO UTUP-LEADTIME                   
035700*           IF IN-IDLEVNR-DC = '1441 ' OR 'BP2TW' OR 'AL3DK' OR           
035800*                              'AD7UX'                                    
035900            IF IN-IDDC-REF NOT = SPACE                                    
036000               CONTINUE                                                   
036100            ELSE                                                          
036200                                                                          
036300               IF SLAG-KVDAGAR-MANLT > ZERO                               
036400                 MOVE SLAG-KVDAGAR-MANLT                                  
036500                               TO REFL-NDC-KVDAGAR-TBT-DC                 
036600               ELSE                                                       
036700                 MOVE IN-IDLEVNR-DC TO W-IDLEVNR                          
036800                 PERFORM IMS-GU-WDF116                                    
036900                 IF SEGMENT-FOUND                                         
037000                    MOVE NDC-KVDAGAR-TBT TO                               
037100                                REFL-NDC-KVDAGAR-TBT-DC                   
037200                 ELSE                                                     
037300                    MOVE 1 TO REFL-NDC-KVDAGAR-TBT-DC                     
037400                 END-IF                                                   
037500               END-IF                                                     
037600            END-IF                                                        
037700         END-IF                                                           
037800         MOVE IN-TIREFPAF  TO TMP1-YYMMDD                                 
037900         MOVE DAGENS-DATUM TO TMP2-YYMMDD                                 
038000         PERFORM WY2000P1                                                 
038100         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
038200*                                                                         
038300*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
038400*                                                                         
038500           MOVE IN-KVREFBER TO REFL-IN-KVREFBER                           
038600         ELSE                                                             
038700           MOVE +0        TO REFL-IN-KVREFBER                             
038800         END-IF                                                           
038900                                                                          
039000         MOVE IN-TIREFPKT  TO TMP1-YYMMDD                                 
039100         MOVE DAGENS-DATUM TO TMP2-YYMMDD                                 
039200         PERFORM WY2000P1                                                 
039300         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
039400*                                                                         
039500*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
039600*                                                                         
039700           MOVE IN-KVREFPKT TO REFL-IN-KVREFPKT                           
039800         ELSE                                                             
039900           MOVE +0        TO REFL-IN-KVREFPKT                             
040000         END-IF                                                           
040100                                                                          
040200         INITIALIZE UTUP-W271UTUP                                         
040300         MOVE 004               TO UTUP-KDCALL                            
040400         MOVE W-IDARTNR         TO UTUP-IDARTNR                           
040500         MOVE W-IDDC            TO UTUP-IDDC                              
040600         MOVE SLAG-IDDC-REF     TO UTUP-IDDC-REF                          
040700         MOVE REFL-NDC-KVDAGAR-TBT-DC                                     
040800                                TO UTUP-LEADTIME                          
040900                                                                          
041000         CALL W271UTUP USING UTUP-W271UTUP                                
041100                             UTUP1-WDK7-PCB                               
041200                             UTUP1-WDB6-PCB                               
041300                             UTUP1-UTIL-WDK6-PCB                          
041400                             UTUP1-UTIL-WDK7-PCB                          
041500                             UTUP1-UTIL-WDB6-PCB                          
041600         IF UTUP-KDSVAR-OK                                                
041700            MOVE UTUP-LEADTID-BEHOV  TO REFL-IN-LEADTID-BEHOV             
041800         ELSE                                                             
041900            DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                          
042000            CALL FELLOG                                                   
042100         END-IF                                                           
042200                                                                          
042300         CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                  
042400                             WDB6-PCB REFL-WDK7-PCB                       
042500                             REFL1-UTIL-WDK6-PCB                          
042600                             REFL1-UTIL-WDK7-PCB                          
042700                             REFL1-UTIL-WDB6-PCB                          
042800                                                                          
042900         IF IN-KVREFOVL = REFL-KVREFOVL AND                               
043000            IN-KVREFPKT = REFL-KVREFPKT AND                               
043100            IN-KVREFBER = REFL-KVREFBER                                   
043200           CONTINUE                                                       
043300         ELSE                                                             
043400           PERFORM B-BEHANDLA-UPDPOST                                     
043500         END-IF                                                           
043600                                                                          
043700         PERFORM C-BEHANDLA-UTPOST                                        
043800                                                                          
043900       END-IF                                                             
044000       PERFORM S01-READ-W27121                                            
044100     END-PERFORM                                                          
044200                                                                          
044300     PERFORM Z-FINIT                                                      
044400                                                                          
044500     MOVE ZERO TO RETURN-CODE                                             
044600     GOBACK                                                               
044700     .                                                                    
044800     EJECT                                                                
044900 A-INIT SECTION.                                                          
045000                                                                          
045100     OPEN INPUT W27121                                                    
045200                                                                          
045300     OPEN OUTPUT W27120                                                   
045400                 W2712F                                                   
045500                                                                          
045600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
045700     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
045800     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
045900     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
046000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
046100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
046200                                                                          
046300     ACCEPT TODAYS-DATE FROM DATE                                         
046400     .                                                                    
046500     EJECT                                                                
046600 B-BEHANDLA-UPDPOST SECTION.                                              
046700                                                                          
046800     MOVE IN-IDARTNR TO W-IDARTNR                                         
046900                        UT2-IDARTNR                                       
047000     MOVE IN-IDDC    TO W-IDDC                                            
047100                        UT2-IDDC                                          
047200                                                                          
047300     MOVE NEJ        TO UT2-FLREFPAF                                      
047400                        UT2-FLREFPKT                                      
047500                                                                          
047600     MOVE SLAG-KVREFBER TO UT2-KVREFBER                                   
047700     MOVE SLAG-KVREFPKT TO UT2-KVREFPKT                                   
047800                                                                          
047900     MOVE REFL-KVREFOVL TO UT2-KVREFOVL                                   
048000                                                                          
048100     IF SLAG-TIREFPAF > ZERO                                              
048200       MOVE TODAYS-DATE            TO TMP1-YYMMDD                         
048300       MOVE SLAG-TIREFPAF          TO TMP2-YYMMDD                         
048400       PERFORM WY2000P1                                                   
048500       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
048600         CONTINUE                                                         
048700       ELSE                                                               
048800****JA TO FLREFPAF MEANS TO SET TIREFPAF TO ZERO IN FOLLOWING PGM         
048900         MOVE JA              TO UT2-FLREFPAF                             
049000         MOVE REFL-KVREFBER TO UT2-KVREFBER                               
049100       END-IF                                                             
049200     ELSE                                                                 
049300       MOVE REFL-KVREFBER TO UT2-KVREFBER                                 
049400     END-IF                                                               
049500                                                                          
049600     IF SLAG-TIREFPKT > ZERO                                              
049700       MOVE TODAYS-DATE            TO TMP1-YYMMDD                         
049800       MOVE SLAG-TIREFPKT          TO TMP2-YYMMDD                         
049900       PERFORM WY2000P1                                                   
050000       IF TMP1-YYMMDD <= TMP2-YYMMDD                                      
050100         CONTINUE                                                         
050200       ELSE                                                               
050300****JA TO FLREFPKT MEANS TO SET TIREFPKT TO ZERO IN FOLLOWING PGM         
050400         MOVE JA              TO UT2-FLREFPKT                             
050500         MOVE REFL-KVREFPKT TO UT2-KVREFPKT                               
050600       END-IF                                                             
050700     ELSE                                                                 
050800       MOVE REFL-KVREFPKT TO UT2-KVREFPKT                                 
050900     END-IF                                                               
051000                                                                          
051100     PERFORM S12-SKRIV-W2712F                                             
051200                                                                          
051300     .                                                                    
051400     EJECT                                                                
051500 C-BEHANDLA-UTPOST SECTION.                                               
051600                                                                          
051700     MOVE IN-IDARTNR  TO UT-IDARTNR                                       
051800     MOVE IN-IDDC     TO UT-IDDC                                          
051900     MOVE IN-KDPRODSL TO UT-KDPRODSL                                      
052000     MOVE IN-KDREFSTA TO UT-KDREFSTA                                      
052100     MOVE REFL-KLASS  TO UT-KLASS                                         
052200     MOVE IN-IDREFTAB TO UT-IDREFTAB                                      
052300     MOVE IN-FLWILSON TO UT-FLWILSON                                      
052400     MOVE WS-PRIS-K6  TO UT-PRARTBES                                      
052500     MOVE IN-PRARTSTD TO UT-PRARTSTD                                      
052600     MOVE WS-PRIS     TO UT-PRMATRL                                       
052700                                                                          
052800     PERFORM S11-SKRIV-W27120                                             
052900     .                                                                    
053000     EJECT                                                                
053100 Z-FINIT SECTION.                                                         
053200                                                                          
053300     CLOSE W27120                                                         
053400           W27121                                                         
053500     SKIP2                                                                
053600     MOVE 'S' TO POSTSUM-OPKOD                                            
053700     CALL POSTSUM USING POSTSUM-PARM                                      
053800     .                                                                    
053900     EJECT                                                                
054000 S01-READ-W27121  SECTION.                                                
054100     SKIP2                                                                
054200     READ W27121 INTO IN-AREA                                             
054300     AT END                                                               
054400        SET END-OF-W27121 TO TRUE                                         
054500                                                                          
054600     NOT AT END                                                           
054700        MOVE 'W27121' TO POSTSUM-FDNAMN                                   
054800        MOVE 'W27120D1' TO POSTSUM-DDNAMN2                                
054900        CALL POSTSUM USING POSTSUM-PARM                                   
055000     END-READ                                                             
055100     .                                                                    
055200     EJECT                                                                
055300 S11-SKRIV-W27120 SECTION.                                                
055400     SKIP2                                                                
055500     WRITE UT-POST FROM UT-AREA                                           
055600                                                                          
055700     MOVE UT-IDARTNR TO POSTSUM-TRANSTYP                                  
055800     MOVE 'W27120 '  TO POSTSUM-FDNAMN                                    
055900     MOVE 'W27120D2' TO POSTSUM-DDNAMN2                                   
056000     CALL POSTSUM    USING POSTSUM-PARM                                   
056100     .                                                                    
056200     EJECT                                                                
056300 S12-SKRIV-W2712F SECTION.                                                
056400     SKIP2                                                                
056500     WRITE UT2-POST FROM UT2-AREA                                         
056600                                                                          
056700     MOVE UT2-IDARTNR TO POSTSUM-TRANSTYP                                 
056800     MOVE 'W2712F '  TO POSTSUM-FDNAMN                                    
056900     MOVE 'W27120D3' TO POSTSUM-DDNAMN2                                   
057000     CALL POSTSUM    USING POSTSUM-PARM                                   
057100     .                                                                    
057200     EJECT                                                                
057300* --- IMS SECTIONS  ---                                                   
057400     EJECT                                                                
057500 IMS-GU-K701 SECTION.                                                     
057600                                                                          
057700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
057800          DELIMITED BY SIZE INTO SSA1                                     
057900     MOVE '  ' TO GOOD-STATUSCODES                                        
058000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
058100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
058200     PERFORM IMS-STATUSCHECK                                              
058300     .                                                                    
058400     EJECT                                                                
058500 IMS-GNP-K711 SECTION.                                                    
058600                                                                          
058700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
058800          DELIMITED BY SIZE INTO SSA1                                     
058900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
059000          DELIMITED BY SIZE INTO SSA2                                     
059100     MOVE '  ' TO GOOD-STATUSCODES                                        
059200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
059300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
059400     PERFORM IMS-STATUSCHECK                                              
059500     .                                                                    
059600     EJECT                                                                
059700 IMS-GNP-WDK712   SECTION.                                                
059800                                                                          
059900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
060000          DELIMITED BY SIZE INTO SSA1                                     
060100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
060200          DELIMITED BY SIZE INTO SSA2                                     
060300     MOVE '  ' TO GOOD-STATUSCODES                                        
060400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK712                        
060500          SSA1 SSA2                                                       
060600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
060700     PERFORM IMS-STATUSCHECK                                              
060800     .                                                                    
060900     EJECT                                                                
061000                                                                          
061100 IMS-GU-WDK601 SECTION.                                                   
061200                                                                          
061300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
061400          DELIMITED BY SIZE INTO SSA1                                     
061500     MOVE '  GE' TO GOOD-STATUSCODES                                      
061600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
061700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUSCHECK                                              
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200 IMS-GNP-WDK611 SECTION.                                                  
062300     MOVE 'WDK611 '        TO SSA1                                        
062400     MOVE '  GE'           TO GOOD-STATUSCODES                            
062500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
062600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
062700     PERFORM IMS-STATUSCHECK                                              
062800     SKIP3                                                                
062900     .                                                                    
063000     EJECT                                                                
063100 IMS-GNP-WDK621 SECTION.                                                  
063200     MOVE 'WDK621 '        TO SSA1                                        
063300     MOVE '  GE'           TO GOOD-STATUSCODES                            
063400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
063500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
063600     PERFORM IMS-STATUSCHECK                                              
063700     SKIP3                                                                
063800     .                                                                    
063900     EJECT                                                                
064000 IMS-GU-WDF116 SECTION.                                                   
064100                                                                          
064200     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
064300          DELIMITED BY SIZE INTO SSA1                                     
064400     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
064500          DELIMITED BY SIZE INTO SSA2                                     
064600     MOVE '  GE' TO GOOD-STATUSCODES                                      
064700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
064800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
064900     PERFORM IMS-STATUSCHECK                                              
065000     .                                                                    
065100     EJECT                                                                
065200 IMS-STATUSCHECK SECTION.                                                 
065300     SKIP2                                                                
065400     SET STATUS-IX TO 1                                                   
065500     SEARCH GOOD-STATUS                                                   
065600       AT END                                                             
065700         MOVE 'WRONG CODE' TO ERRTEXT-STR                                 
065800         DISPLAY ERRTEXT                                                  
065900         CALL FELLOG                                                      
066000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
066100         CONTINUE                                                         
066200     END-SEARCH                                                           
066300     .                                                                    
066400     EJECT                                                                
066500*    -COPY WY2000P1                                                       
