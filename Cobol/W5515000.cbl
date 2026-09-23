000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5515000.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   94/05/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        MATCHAR FILER BEARBETAR DESSA OCH SKAPAR UTFIL.                  
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600*        ÄNDRAD FÖR ETRACKER NO 1567775, INSTALLERAD 2004-11-23           
001700*        ÄNDRAD FÖR ETRACKER NO 1572811, INSTALLERAD 2004-11-25           
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- FIL FRÅN WDK6                                              
002700     SELECT W55142                     ASSIGN TO W55150D1.                
002800     SKIP2                                                                
002900*          --- FIL FRÅN WDD9                                              
003000     SELECT W55143                     ASSIGN TO W55150D2.                
003100     SKIP2                                                                
003200*          --- KOMPLETTERAD FIL MED BEHOV.                                
003300     SELECT W55150                     ASSIGN TO W55150D3.                
003400     SKIP2                                                                
003500*          --- PREL FIL MED BEHOV.                                        
003600     SELECT W55151                     ASSIGN TO W55150D4.                
003700     SKIP2                                                                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W55142                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700*01  -COPY W55142      -L.                                                
004800     SKIP3                                                                
004900 FD  W55143                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200     SKIP2                                                                
005300*01  -COPY W55143      -L.                                                
005400     SKIP3                                                                
005500 FD  W55150                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800     SKIP2                                                                
005900*01  POST -COPY W55150 -PRE  UT-  -L.                                     
006000     SKIP3                                                                
006100 FD  W55151                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400     SKIP2                                                                
006500*01  POST -COPY W55150 -PRE  UT1-  -L.                                    
006600     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900*    -COPY WY2000W1                                                       
007000     SKIP3                                                                
007100 77  IDPGM                       PIC X(8)    VALUE 'W5515000'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007400 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
007500 77  SPAR-STATUS                 PIC X(5)    VALUE SPACE.                 
007600 77  SPAR-IDLEVNR-PR             PIC X(5)    VALUE SPACE.                 
007700 77  SPAR-TIPRLIST               PIC S9(7)   VALUE ZERO COMP-3.           
007800 77  SPAR-PRARTBEL-PR            PIC S9(8)V9(5) VALUE ZERO COMP-3.        
007900 77  SPAR-KDVALISO               PIC X(3)    VALUE SPACE.                 
008000                                                                          
008100 01  W-TIAAVV                    PIC 9(4)    VALUE ZERO.                  
008200                                                                          
008300 77  BEHANDLING-SW               PIC X       VALUE 'N'.                   
008400     88  BEHANDLING                          VALUE 'J'.                   
008500                                                                          
008600 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
008700     88  TRAEFF                              VALUE 'J'.                   
008800     88  RADINFO-FINNS                       VALUE 'J'.                   
008900                                                                          
009000 77  W55142-EOF-SW               PIC X       VALUE 'N'.                   
009100     88  END-OF-W55142                       VALUE 'J'.                   
009200                                                                          
009300 77  W55143-EOF-SW               PIC X       VALUE 'N'.                   
009400     88  END-OF-W55143                       VALUE 'J'.                   
009500                                                                          
009600 01  BERAKNINGS-FAELT.                                                    
009700     03  W-SUM-LAGER             PIC S9(11)  VALUE ZERO COMP-3.           
009800     03  W-TIPRLIST              PIC S9(7)   VALUE ZERO COMP-3.           
009900*      --- VALID IDDC CODES                                               
010000*                                                                         
010100*01    -COPY WWDCKONS                                                     
010200*01    -COPY WWPRODSL                                                     
010300       EJECT                                                              
010400                                                                          
010500 01  JMF-DATUM                   PIC 9(6)     VALUE ZERO.                 
010600 01  JAMFOR-DATUM                PIC S9(7)    VALUE ZERO COMP-3.          
010700 01  W-TIAA                      PIC 9(2)     VALUE ZERO.                 
010800 01  W55142-KEY.                                                          
010900     03  W55142-KEY-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
011000     03  W55142-KEY-IDLEVNR      PIC X(9)    VALUE SPACE.                 
011100                                                                          
011200 01  W55143-KEY.                                                          
011300     03  W55143-KEY-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
011400     03  W55143-KEY-IDLEVNR      PIC X(9)    VALUE SPACE.                 
011500                                                                          
011600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011700 01  FILLER REDEFINES DAGENS-DATUM.                                       
011800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012100     SKIP2                                                                
012200 01  DYNAMISKA-SUBPROGRAM.                                                
012300*                                                                         
012400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
012900     03  W22222                  PIC X(8)    VALUE 'W22222'.              
013000     SKIP2                                                                
013100*    --- PARAMETRAR TILL ABEND                                            
013200                                                                          
013300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013500     SKIP2                                                                
013600 01  FELTEXT.                                                             
013700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL POSTSUM                                          
014100*                                                                         
014200*01  -COPY W0005   -PRE  POSTSUM-                                         
014300     EJECT                                                                
014400 01  FILLER                      PIC X(24)   VALUE 'BEHOVSMODUL '.        
014500*01  -COPY W222L222  -PRE L-.                                             
014600     EJECT                                                                
014700                                                                          
014800 01  DATUMKORT-ID                PIC X(24)   VALUE 'WDATUM-START'.        
014900*01  -COPY WDATKORT                                                       
015000     EJECT                                                                
015100                                                                          
015200 01  IN-AREA-START               PIC X(24)   VALUE                        
015300                                 'IN-AREA-START  '.                       
015400     SKIP2                                                                
015500                                                                          
015600*01  AREA -COPY W55142     -PRE IN-                                       
015700     EJECT                                                                
015800 01  IN2-AREA-START              PIC X(24)   VALUE                        
015900                                 'IN2-AREA-START  '.                      
016000     SKIP2                                                                
016100                                                                          
016200*01  AREA -COPY W55143     -PRE IN2-                                      
016300     EJECT                                                                
016400 01  UT-AREA-START               PIC X(24)   VALUE                        
016500                                 'UT-AREA-START  '.                       
016600     SKIP2                                                                
016700                                                                          
016800*01  AREA -COPY W55150     -PRE UT-                                       
016900     EJECT                                                                
017000 01  UT-AREA-START               PIC X(24)   VALUE                        
017100                                 'PREL-UTAREA    '.                       
017200                                                                          
017300*01  AREA -COPY W55150     -PRE PREL-.                                    
017400     EJECT                                                                
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600*                                                                         
017700     SKIP2                                                                
017800 01   FILLER                      PIC X(16)  VALUE 'IMS-WS'.              
017900     SKIP3                                                                
018000 01  NYCKLAR-TILL-DLI.                                                    
018100     03  W-KDARBTYP-X.                                                    
018200         05  W-KDARBTYP          PIC X(8)    VALUE 'INK     '.            
018300     03  W-IDPERSON-X.                                                    
018400         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
018500*    --- STATUS-KOD FRÅN IMS                                              
018600 01  STATUS-WS                   PIC XX.                                  
018700     88  SEGMENT-FINNS                       VALUE '  '.                  
018800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019000     SKIP2                                                                
019100 01  GODK-STATUSKODER.                                                    
019200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019300     SKIP3                                                                
019400 01  SSA1                        PIC X(128).                              
019500 01  SSA2                        PIC X(128).                              
019600     EJECT                                                                
019700*    --- IMS FUNKTIONSKODER                                               
019800*01  -COPY W0003                                                          
019900     EJECT                                                                
020000                                                                          
020100*    ---  DLI INPUT-OUTPUT AREA                                           
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
020300 01  DLI-IO-WDP301.                                                       
020400*    03  -COPY WDP301                                                     
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
020600 01  DLI-IO-WDP311.                                                       
020700*    03  -COPY WDP311                                                     
020800     EJECT                                                                
020900 LINKAGE SECTION.                                                         
021000*01  -COPY W0008  -PRE WDP3-                                              
021100     05   FILLER                 PIC X.                                   
021200                                                                          
021300 01  WDK6-PCB                    PIC X.                                   
021400 01  WDK7-PCB                    PIC X.                                   
021500 01  ARTM-PCB                    PIC X.                                   
021600 01  REF2-2501-PCB               PIC X.                                   
021700 01  REF2-WDB6-PCB               PIC X.                                   
021800 01  REF2-WDK7-PCB               PIC X.                                   
021900 01  WDB6-PCB                    PIC X.                                   
022000 01  WDD7-PCB                    PIC X.                                   
022100 01  WDK7E-PCB                   PIC X.                                   
022200 01  W222-UTIL-WDK6-PCB          PIC X.                                   
022300 01  W222-UTIL-WDK7-PCB          PIC X.                                   
022400 01  W222-UTIL-WDB6-PCB          PIC X.                                   
022500 01  W222-UTUP-WDK7-PCB          PIC X.                                   
022600 01  W222-UTUP-WDB6-PCB          PIC X.                                   
022700 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
022800 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
022900 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
023000     EJECT                                                                
023100 PROCEDURE DIVISION  USING WDP3-PCB WDK6-PCB WDK7-PCB ARTM-PCB            
023200                           REF2-2501-PCB                                  
023300                           REF2-WDB6-PCB                                  
023400                           REF2-WDK7-PCB                                  
023500                           WDB6-PCB                                       
023600                           WDD7-PCB WDK7E-PCB                             
023700                           W222-UTIL-WDK6-PCB                             
023800                           W222-UTIL-WDK7-PCB                             
023900                           W222-UTIL-WDB6-PCB                             
024000                           W222-UTUP-WDK7-PCB                             
024100                           W222-UTUP-WDB6-PCB                             
024200                           W222-UTUP-UTIL-WDK6-PCB                        
024300                           W222-UTUP-UTIL-WDK7-PCB                        
024400                           W222-UTUP-UTIL-WDB6-PCB                        
024500                           .                                              
024600     ENTRY 'DLITCBL' USING WDP3-PCB WDK6-PCB WDK7-PCB ARTM-PCB            
024700                           REF2-2501-PCB                                  
024800                           REF2-WDB6-PCB                                  
024900                           REF2-WDK7-PCB                                  
025000                           WDB6-PCB                                       
025100                           WDD7-PCB WDK7E-PCB                             
025200                           W222-UTIL-WDK6-PCB                             
025300                           W222-UTIL-WDK7-PCB                             
025400                           W222-UTIL-WDB6-PCB                             
025500                           W222-UTUP-WDK7-PCB                             
025600                           W222-UTUP-WDB6-PCB                             
025700                           W222-UTUP-UTIL-WDK6-PCB                        
025800                           W222-UTUP-UTIL-WDK7-PCB                        
025900                           W222-UTUP-UTIL-WDB6-PCB                        
026000                           .                                              
026100     SKIP2                                                                
026200     PERFORM A-INIT                                                       
026300                                                                          
026400     PERFORM S01-LAES-W55142                                              
026500     PERFORM S02-LAES-W55143                                              
026600     PERFORM UNTIL END-OF-W55142                                          
026700       PERFORM UNTIL END-OF-W55143 OR                                     
026800         W55143-KEY >= W55142-KEY                                         
026900           PERFORM S02-LAES-W55143                                        
027000       END-PERFORM                                                        
027100       PERFORM B-TEST-BEHANDLA-POST                                       
027200       PERFORM AA-NOLLSTAELL-SPARFAELT                                    
027300       PERFORM S01-LAES-W55142                                            
027400     END-PERFORM                                                          
027500                                                                          
027600     PERFORM Z-FINIT                                                      
027700                                                                          
027800     MOVE ZERO TO RETURN-CODE                                             
027900     GOBACK                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 A-INIT SECTION.                                                          
028300                                                                          
028400     OPEN INPUT  W55142                                                   
028500                 W55143                                                   
028600                                                                          
028700     OPEN OUTPUT W55150                                                   
028800                 W55151                                                   
028900     SKIP2                                                                
029000     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
029100                                                                          
029200     MOVE D-AAR    TO W-TIAAVV(1:2)                                       
029300     MOVE D-VECKA  TO W-TIAAVV(3:2)                                       
029400                                                                          
029500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029600     .                                                                    
029700     EJECT                                                                
029800 AA-NOLLSTAELL-SPARFAELT SECTION.                                         
029900     SKIP2                                                                
030000     MOVE ZERO   TO SPAR-TIPRLIST                                         
030100                    SPAR-PRARTBEL-PR                                      
030200     MOVE SPACE  TO SPAR-KDVALISO                                         
030300                    SPAR-STATUS                                           
030400                    SPAR-IDLEVNR-PR                                       
030500     .                                                                    
030600     EJECT                                                                
030700 B-TEST-BEHANDLA-POST SECTION.                                            
030800     SKIP2                                                                
030900     MOVE NEJ TO BEHANDLING-SW                                            
031000     MOVE IN-KDPRODSL TO TEST-KDPRODSL                                    
031100                                                                          
031200     COMPUTE W-SUM-LAGER ROUNDED =                                        
031300             IN-KVLS + IN-KVAKS + IN-KVEFRS                               
031400                                                                          
031500     IF IN-FLIART = 'J'                                                   
031600         MOVE JA        TO BEHANDLING-SW                                  
031700     ELSE                                                                 
031800       IF W55142-KEY = W55143-KEY                                         
031900         IF IN-KDERS < +10                                                
032000           MOVE JA      TO BEHANDLING-SW                                  
032100         ELSE                                                             
032200           MOVE NEJ     TO BEHANDLING-SW                                  
032300         END-IF                                                           
032400       ELSE                                                               
032500         IF IN-KDERS = ZERO                                               
032600           MOVE JA      TO BEHANDLING-SW                                  
032700         ELSE                                                             
032800           IF W-SUM-LAGER > ZERO                                          
032900             MOVE JA    TO BEHANDLING-SW                                  
033000           ELSE                                                           
033100*            IF KDPRODSL-LYNK                                             
033200*              MOVE JA  TO BEHANDLING-SW                                  
033300*            ELSE                                                         
033400               MOVE NEJ TO BEHANDLING-SW                                  
033500*            END-IF                                                       
033600           END-IF                                                         
033700         END-IF                                                           
033800       END-IF                                                             
033900     END-IF                                                               
034000                                                                          
034100     IF BEHANDLING                                                        
034200       PERFORM C-LAES-BEHOVSMODUL                                         
034300       PERFORM D-HAEMTA-PRISRAD                                           
034400       PERFORM E-FLYTTA-RADINFO                                           
034500       PERFORM F-READ-WDP3                                                
034600       PERFORM S11-SKRIV-W55150                                           
034700       PERFORM S12-SKRIV-W55151                                           
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 C-LAES-BEHOVSMODUL SECTION.                                              
035200     SKIP2                                                                
035300     MOVE IN-IDARTNR     TO L-IDARTNR                                     
035400     MOVE SPACE          TO L-IDDC                                        
035500     MOVE W-TIAAVV       TO L-TIAAVV-AKTUELL                              
035600                            L-TIBEHOV-START                               
035700     MOVE D-DAGNR        TO L-TID-AKTUELL                                 
035800     MOVE +52            TO L-KVVECKOR-BEHOV                              
035900     MOVE '17'           TO L-KDBEHOV                                     
036000     MOVE JA             TO L-FLINKLDIRLEV                                
036100                                                                          
036200     CALL W22222 USING L-W222L222 WDK6-PCB WDK7-PCB ARTM-PCB              
036300                       REF2-2501-PCB                                      
036400                       REF2-WDB6-PCB                                      
036500                       REF2-WDK7-PCB                                      
036600                       WDB6-PCB WDD7-PCB WDK7E-PCB                        
036700                       W222-UTIL-WDK6-PCB                                 
036800                       W222-UTIL-WDK7-PCB                                 
036900                       W222-UTIL-WDB6-PCB                                 
037000                       W222-UTUP-WDK7-PCB                                 
037100                       W222-UTUP-WDB6-PCB                                 
037200                       W222-UTUP-UTIL-WDK6-PCB                            
037300                       W222-UTUP-UTIL-WDK7-PCB                            
037400                       W222-UTUP-UTIL-WDB6-PCB                            
037500     .                                                                    
037600     EJECT                                                                
037700 D-HAEMTA-PRISRAD SECTION.                                                
037800                                                                          
037900     MOVE NEJ TO TRAEFF-SW                                                
038000     MOVE ZERO TO INDX                                                    
038100     PERFORM UNTIL TRAEFF OR INDX = 5                                     
038200       ADD +1 TO INDX                                                     
038300       IF  IN-IDLEVNR-PR (INDX) NOT = SPACE                               
038400       AND IN-TIPRLIST (INDX) NOT = ZERO                                  
038500**** CHANGE FOR STD PRICE PROCESS 2022, LATEST PRICEROW SHOULD BE         
038600**** COUNTED IF IT EQUAL MAIL SUPPLIER                                    
038700         IF  IN-IDLEVNR-PR (INDX) = IN-IDLEVNR                            
038800           MOVE JA   TO TRAEFF-SW                                         
038900         END-IF                                                           
039000**** END CHANGE                                                           
039100         IF IN-KDSTATUS-PR (INDX) = ZERO                                  
039200           MOVE 'PREL'        TO SPAR-STATUS                              
039300           PERFORM DA-SKAPA-SKRIV-PRELPOST                                
039400         ELSE                                                             
039500           IF IN-SUINLEV-PR (INDX) = ZERO                                 
039600             MOVE 'GODK'      TO SPAR-STATUS                              
039700           ELSE                                                           
039800             IF IN-SUINLEV-PR (INDX) > ZERO                               
039900               MOVE 'INLEV'   TO SPAR-STATUS                              
040000             END-IF                                                       
040100           END-IF                                                         
040200         END-IF                                                           
040300                                                                          
040400         MOVE IN-TIPRLIST(INDX) TO W-TIPRLIST                             
040500         MOVE D-AAR             TO W-TIAA                                 
040600         ADD 1                  TO W-TIAA                                 
040700         MOVE W-TIAA            TO JMF-DATUM(1:2)                         
040800         MOVE 0101              TO JMF-DATUM(3:4)                         
040900         MOVE JMF-DATUM         TO JAMFOR-DATUM                           
041000*****    1 JANUARI NÄSTA ÅR *********************                         
041100                                                                          
041200         IF W-SUM-LAGER > L-KVBEHOV-SUMMA                                 
041300           IF IN-SUINLEV-PR(INDX) > ZERO                                  
041400             MOVE JA   TO TRAEFF-SW                                       
041500           END-IF                                                         
041600         ELSE                                                             
041700           MOVE W-TIPRLIST     TO TMP1-YYMMDD                             
041800           MOVE JAMFOR-DATUM   TO TMP2-YYMMDD                             
041900           PERFORM WY2000P1                                               
042000           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
042100             IF IN-KDSTATUS-PR(INDX) = 1                                  
042200               MOVE JA TO TRAEFF-SW                                       
042300             END-IF                                                       
042400           END-IF                                                         
042500         END-IF                                                           
042600       ELSE                                                               
042700         MOVE NEJ      TO TRAEFF-SW                                       
042800         MOVE 5        TO INDX                                            
042900       END-IF                                                             
043000     END-PERFORM                                                          
043100                                                                          
043200     IF TRAEFF                                                            
043300       MOVE IN-TIPRLIST (INDX)       TO SPAR-TIPRLIST                     
043400       MOVE IN-IDLEVNR-PR (INDX)     TO SPAR-IDLEVNR-PR                   
043500       MOVE IN-PRARTBEL-PR (INDX)    TO SPAR-PRARTBEL-PR                  
043600       MOVE IN-KDVALISO (INDX)       TO SPAR-KDVALISO                     
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 DA-SKAPA-SKRIV-PRELPOST SECTION.                                         
044100                                                                          
044200     MOVE IN-IDARTNR             TO PREL-IDARTNR                          
044300     MOVE IN-IDFKNGRP            TO PREL-IDFKNGRP                         
044400     MOVE IN-IDANSK              TO PREL-IDANSK                           
044500     MOVE IN-IDINK               TO PREL-IDINK                            
044600     MOVE IN-KDVTH               TO PREL-KDVTH                            
044700     MOVE SPACE                  TO PREL-IDPRANSV                         
044800     MOVE NEJ                    TO PREL-KDPRBEH                          
044900     MOVE IN-REDIRLEV            TO PREL-REDIRLEV                         
045000     MOVE IN-KDHF                TO PREL-KDHF                             
045100     MOVE IN-KDPRODSL            TO PREL-KDPRODSL                         
045200     MOVE IN-FLIART              TO PREL-FLIART                           
045300     MOVE NEJ                    TO PREL-FLAPC                            
045400     MOVE NEJ                    TO PREL-FLPRFIL                          
045500     MOVE IN-IDLEVNR             TO PREL-IDLEVNR-HUV                      
045600     MOVE W-SUM-LAGER            TO PREL-KVDISP-SPIS                      
045700     MOVE L-KVBEHOV-SUMMA        TO PREL-KVBEHOVAR                        
045800     MOVE IN-PRARTBES            TO PREL-PRARTBES                         
045900     MOVE IN-PRARTSJK            TO PREL-PRARTSJK                         
046000     MOVE IN-TIPRLIST    (INDX)  TO PREL-TIPRLIST                         
046100     MOVE IN-IDLEVNR-PR  (INDX)  TO PREL-IDLEVNR                          
046200     MOVE IN-PRARTBEL-PR (INDX)  TO PREL-PRARTBEL-PR                      
046300     MOVE IN-KDVALISO    (INDX)  TO PREL-KDVALISO                         
046400     MOVE 'PREL'                 TO PREL-KDSTASPIS                        
046500     MOVE ZERO                   TO PREL-PRKURS                           
046600     IF IN-RETULF = 1.0000                                                
046700       MOVE IN-RETULF            TO PREL-RETULF                           
046800     ELSE                                                                 
046900       MOVE ZERO                 TO PREL-RETULF                           
047000     END-IF                                                               
047100     MOVE ZERO                   TO PREL-TITULF                           
047200     MOVE IN-PRINK               TO PREL-PRINK-AKT                        
047300     MOVE IN-PRDIRLON            TO PREL-PRDIRLON-AKT                     
047400     MOVE IN-PRDMTRL             TO PREL-PRDMTRL-AKT                      
047500     MOVE IN-PROVRPAL            TO PREL-PROVRPAL-AKT                     
047600     MOVE ZERO                   TO PREL-PRINK-KOM                        
047700                                    PREL-PRDIRLON-KOM                     
047800                                    PREL-PRDMTRL-KOM                      
047900                                    PREL-PROVRPAL-KOM                     
048000     MOVE IN-INLEV-TIPRLIST      TO PREL-INLEV-TIPRLIST                   
048100     MOVE IN-INLEV-IDLEVNR-PR    TO PREL-INLEV-IDLEVNR-PR                 
048200     MOVE IN-INLEV-PRARTBEL-PR   TO PREL-INLEV-PRARTBEL-PR                
048300     MOVE IN-GODK-TIPRLIST       TO PREL-GODK-TIPRLIST                    
048400     MOVE IN-GODK-IDLEVNR-PR     TO PREL-GODK-IDLEVNR-PR                  
048500     MOVE IN-GODK-PRARTBEL-PR    TO PREL-GODK-PRARTBEL-PR                 
048600     MOVE IN-LOCAL-TIPRLIST      TO PREL-LOCAL-TIPRLIST                   
048700     MOVE IN-LOCAL-IDLEVNR-PR    TO PREL-LOCAL-IDLEVNR-PR                 
048800     MOVE IN-LOCAL-PRARTBEL-PR   TO PREL-LOCAL-PRARTBEL-PR                
048900     .                                                                    
049000     EJECT                                                                
049100 E-FLYTTA-RADINFO SECTION.                                                
049200     SKIP2                                                                
049300     MOVE IN-IDARTNR             TO UT-IDARTNR                            
049400     MOVE IN-IDFKNGRP            TO UT-IDFKNGRP                           
049500     MOVE IN-IDANSK              TO UT-IDANSK                             
049600     MOVE IN-IDINK               TO UT-IDINK                              
049700     MOVE IN-KDVTH               TO UT-KDVTH                              
049800     MOVE 'VCCS'                 TO UT-IDPRANSV                           
049900     MOVE NEJ                    TO UT-KDPRBEH                            
050000     MOVE IN-REDIRLEV            TO UT-REDIRLEV                           
050100     MOVE IN-KDHF                TO UT-KDHF                               
050200     MOVE IN-KDPRODSL            TO UT-KDPRODSL                           
050300     MOVE IN-FLIART              TO UT-FLIART                             
050400     MOVE NEJ                    TO UT-FLAPC                              
050500     MOVE NEJ                    TO UT-FLPRFIL                            
050600     MOVE IN-IDLEVNR             TO UT-IDLEVNR-HUV                        
050700     MOVE W-SUM-LAGER            TO UT-KVDISP-SPIS                        
050800     MOVE L-KVBEHOV-SUMMA        TO UT-KVBEHOVAR                          
050900     MOVE IN-PRARTBES            TO UT-PRARTBES                           
051000     MOVE IN-PRARTSJK            TO UT-PRARTSJK                           
051100                                                                          
051200     IF RADINFO-FINNS                                                     
051300       MOVE SPAR-PRARTBEL-PR     TO UT-PRARTBEL-PR                        
051400       MOVE SPAR-TIPRLIST        TO UT-TIPRLIST                           
051500       MOVE SPAR-KDVALISO        TO UT-KDVALISO                           
051600       MOVE SPAR-STATUS          TO UT-KDSTASPIS                          
051700       MOVE SPAR-IDLEVNR-PR      TO UT-IDLEVNR                            
051800     ELSE                                                                 
051900       MOVE ZERO                 TO UT-PRARTBEL-PR                        
052000                                    UT-TIPRLIST                           
052100       MOVE 'SEK'                TO UT-KDVALISO                           
052200       MOVE SPACE                TO UT-KDSTASPIS                          
052300       MOVE IN-IDLEVNR           TO UT-IDLEVNR                            
052400     END-IF                                                               
052500                                                                          
052600     MOVE ZERO                   TO UT-PRKURS                             
052700                                                                          
052800     IF IN-RETULF = 1.0000                                                
052900       MOVE IN-RETULF            TO UT-RETULF                             
053000     ELSE                                                                 
053100       MOVE ZERO                 TO UT-RETULF                             
053200     END-IF                                                               
053300                                                                          
053400     MOVE ZERO                   TO UT-TITULF                             
053500     MOVE IN-PRINK               TO UT-PRINK-AKT                          
053600     MOVE IN-PRDIRLON            TO UT-PRDIRLON-AKT                       
053700     MOVE IN-PRDMTRL             TO UT-PRDMTRL-AKT                        
053800     MOVE IN-PROVRPAL            TO UT-PROVRPAL-AKT                       
053900     MOVE ZERO                   TO UT-PRINK-KOM                          
054000                                    UT-PRDIRLON-KOM                       
054100                                    UT-PRDMTRL-KOM                        
054200                                    UT-PROVRPAL-KOM                       
054300     MOVE IN-INLEV-TIPRLIST      TO UT-INLEV-TIPRLIST                     
054400     MOVE IN-INLEV-IDLEVNR-PR    TO UT-INLEV-IDLEVNR-PR                   
054500     MOVE IN-INLEV-PRARTBEL-PR   TO UT-INLEV-PRARTBEL-PR                  
054600     MOVE IN-GODK-TIPRLIST       TO UT-GODK-TIPRLIST                      
054700     MOVE IN-GODK-IDLEVNR-PR     TO UT-GODK-IDLEVNR-PR                    
054800     MOVE IN-GODK-PRARTBEL-PR    TO UT-GODK-PRARTBEL-PR                   
054900     MOVE IN-LOCAL-TIPRLIST      TO UT-LOCAL-TIPRLIST                     
055000     MOVE IN-LOCAL-IDLEVNR-PR    TO UT-LOCAL-IDLEVNR-PR                   
055100     MOVE IN-LOCAL-PRARTBEL-PR   TO UT-LOCAL-PRARTBEL-PR                  
055200     .                                                                    
055300     EJECT                                                                
055400 F-READ-WDP3 SECTION.                                                     
055500     IF IN-IDINK NOT = SPACE                                              
055600       IF IN-IDINK (1:3) NUMERIC                                          
055700          MOVE IN-IDINK (1:3)      TO W-IDPERSON                          
055800       ELSE                                                               
055900          IF IN-IDINK (2:3) NUMERIC                                       
056000             MOVE IN-IDINK (2:3) TO W-IDPERSON                            
056100          ELSE                                                            
056200             MOVE ZERO             TO W-IDPERSON                          
056300          END-IF                                                          
056400       END-IF                                                             
056500     END-IF                                                               
056600     PERFORM IMS-GU-WDP311                                                
056700     IF SEGMENT-FINNS                                                     
056800       MOVE PERS-IDNAMN   TO UT-IDNAMN                                    
056900                             PREL-IDNAMN                                  
057000       MOVE PERS-IDMAIL   TO UT-IDMAIL                                    
057100                             PREL-IDMAIL                                  
057200     ELSE                                                                 
057300       MOVE SPACE         TO UT-IDNAMN                                    
057400                             UT-IDMAIL                                    
057500                             PREL-IDNAMN                                  
057600                             PREL-IDMAIL                                  
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000                                                                          
058100 Z-FINIT SECTION.                                                         
058200     CLOSE W55143                                                         
058300           W55142                                                         
058400           W55150                                                         
058500           W55151                                                         
058600     SKIP2                                                                
058700     MOVE 'S' TO POSTSUM-OPKOD                                            
058800     CALL POSTSUM USING POSTSUM-PARM                                      
058900     .                                                                    
059000     EJECT                                                                
059100 S01-LAES-W55142  SECTION.                                                
059200     SKIP2                                                                
059300     READ W55142 INTO IN-AREA                                             
059400     AT END                                                               
059500        MOVE 999999999  TO IN-IDARTNR                                     
059600        SET END-OF-W55142 TO TRUE                                         
059700                                                                          
059800     NOT AT END                                                           
059900        MOVE IN-IDARTNR TO W55142-KEY-IDARTNR                             
060000        MOVE IN-IDLEVNR TO W55142-KEY-IDLEVNR                             
060100        MOVE 'W55142' TO POSTSUM-FDNAMN                                   
060200        MOVE 'W55150D1' TO POSTSUM-DDNAMN2                                
060300        MOVE 'WDK6'    TO POSTSUM-TRANSTYP                                
060400        CALL POSTSUM USING POSTSUM-PARM                                   
060500     END-READ                                                             
060600     .                                                                    
060700     EJECT                                                                
060800 S02-LAES-W55143  SECTION.                                                
060900     SKIP2                                                                
061000     READ W55143 INTO IN2-AREA                                            
061100     AT END                                                               
061200        MOVE 999999999  TO IN2-IDARTNR                                    
061300        SET END-OF-W55143 TO TRUE                                         
061400                                                                          
061500     NOT AT END                                                           
061600        MOVE IN2-IDARTNR TO W55143-KEY-IDARTNR                            
061700        MOVE IN2-IDLEVNR TO W55143-KEY-IDLEVNR                            
061800        MOVE 'W55143' TO POSTSUM-FDNAMN                                   
061900        MOVE 'W55150D2' TO POSTSUM-DDNAMN2                                
062000        MOVE 'WDD9'     TO POSTSUM-TRANSTYP                               
062100        CALL POSTSUM USING POSTSUM-PARM                                   
062200     END-READ                                                             
062300     .                                                                    
062400     EJECT                                                                
062500 S11-SKRIV-W55150 SECTION.                                                
062600     SKIP2                                                                
062700     WRITE UT-POST FROM UT-AREA                                           
062800                                                                          
062900     MOVE 'MAST'        TO POSTSUM-TRANSTYP                               
063000     MOVE 'W55150'      TO POSTSUM-FDNAMN                                 
063100     MOVE 'W55150D3'    TO POSTSUM-DDNAMN2                                
063200     CALL POSTSUM USING POSTSUM-PARM                                      
063300     .                                                                    
063400     EJECT                                                                
063500 S12-SKRIV-W55151 SECTION.                                                
063600     SKIP2                                                                
063700     WRITE UT1-POST FROM PREL-AREA                                        
063800                                                                          
063900     MOVE 'PREL'        TO POSTSUM-TRANSTYP                               
064000     MOVE 'W55151'      TO POSTSUM-FDNAMN                                 
064100     MOVE 'W55150D4'    TO POSTSUM-DDNAMN2                                
064200     CALL POSTSUM USING POSTSUM-PARM                                      
064300     .                                                                    
064400     EJECT                                                                
064500* --- IMS SEKTIONER ---                                                   
064600 IMS-GU-WDP311 SECTION.                                                   
064700     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
064800          DELIMITED BY SIZE INTO SSA1                                     
064900     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
065000          DELIMITED BY SIZE INTO SSA2                                     
065100     MOVE '  GE' TO GODK-STATUSKODER                                      
065200     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
065300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600     EJECT                                                                
065700 IMS-STATUSKONTROLL SECTION.                                              
065800                                                                          
065900     SET STATUS-IX TO 1                                                   
066000     SEARCH GODK-STATUS                                                   
066100       AT END                                                             
066200         MOVE 'EJ GODK.STATUS' TO FELTEXT-STR                             
066300         DISPLAY FELTEXT                                                  
066400         CALL FELLOG                                                      
066500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066600         CONTINUE                                                         
066700     END-SEARCH                                                           
066800     .                                                                    
066900     EJECT                                                                
067000*    -COPY WY2000P1                                                       
