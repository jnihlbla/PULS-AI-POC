000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3303500.                                                
000300 AUTHOR.         RONNY STENHOLM.                                          
000400 DATE-WRITTEN.   93/12/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPA ARTIKELSTATISTIK TILL MARKNADSBOLAG                        
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLBETC (WDB1)                              
001100*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- SEKUNDÄRREGISTRET ARTIKELSTATISTIK                         
002600     SELECT W33015                     ASSIGN TO W33035D1.                
002700     SKIP2                                                                
002800*          --- INNEHÅLLER SELEKTIONSARGUMENTEN                            
002900*          --- FRÅN SAMTLIGA MB SOM ÖNSKAR INFO                           
003000     SELECT W33036                     ASSIGN TO W33035D2.                
003100     SKIP2                                                                
003200*          --- OSORTERAD FIL TILL MARKNADSBOLAGEN                         
003300*          --- DELAS I SENARE STEG                                        
003400     SELECT W33038                     ASSIGN TO W33035D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W33015                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300     SKIP2                                                                
004400*01  -COPY W33014      -L.                                                
004500     SKIP3                                                                
004600 FD  W33036                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900     SKIP2                                                                
005000*01  -COPY W33036      -L.                                                
005100     SKIP3                                                                
005200 FD  W33038                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600*01  POST -COPY W33038 -PRE  UT-  -L.                                     
005700                                                                          
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200     SKIP3                                                                
006300 77  IDPGM                       PIC X(8)    VALUE 'W3303500'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  DISTRIKT-PROMR-FINNS        PIC X       VALUE 'J'.                   
006700 77  TAB-IX                      PIC S9(9)   COMP SYNC.                   
006800 77  TRANS-IX                    PIC S9(9)   COMP SYNC.                   
006900 77  GODK-IX                     PIC S9(9)   COMP SYNC.                   
007000 77  MAX-TAB-IX                  PIC S9(9)   COMP SYNC                    
007100                                             VALUE +1500.                 
007200 77  SPAR-IDARTNR                PIC S9(9) COMP-3 VALUE ZERO.             
007300                                                                          
007400 77  GODKAEND-IDPROMR-SW         PIC X       VALUE 'N'.                   
007500     88 GODKAEND-IDPROMR                     VALUE 'J'.                   
007600 77  W33015-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W33015                       VALUE 'J'.                   
007800                                                                          
007900 77  W33036-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-W33036                       VALUE 'J'.                   
008100     EJECT                                                                
008200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008300 01  FILLER REDEFINES DAGENS-DATUM.                                       
008400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008700     EJECT                                                                
008800                                                                          
008900 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
009000 01  WS-SEKTION              PIC X(30)   VALUE SPACE.                     
009100 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
009200 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
009300 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
009400 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
009500                                                                          
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010800                                                                          
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL ABEND                                            
011100                                                                          
011200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011400     SKIP2                                                                
011500 01  FELTEXT.                                                             
011600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011800     EJECT                                                                
011900*    --- PARAMETRAR TILL DATKORT                                          
012000*                                                                         
012100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W33035'.              
012200     SKIP2                                                                
012300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012400     SKIP2                                                                
012500*01  -COPY WDATKORT                                                       
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL POSTSUM                                          
012800*                                                                         
012900*01  -COPY W0005   -PRE  POSTSUM-                                         
013000     EJECT                                                                
013100*01  -COPY WDATAREA                                                       
013200     EJECT                                                                
013300 01  TABELL.                                                              
013400     03  TAB-RAD OCCURS 1500.                                             
013500        05  TAB-AREA.                                                     
013600          07 TAB-IDMARKBO             PIC X.                              
013700          07 TAB-IDPROMR              PIC X(3).                           
013800          07 TAB-SUARTFSG-DO-RAAR     PIC S9(9)V9(2)      COMP-3.         
013900          07 TAB-SULEVANT-DO-RAAR     PIC S9(9)           COMP-3.         
014000          07 TAB-SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.         
014100          07 TAB-SULEVANT-RAAR        PIC S9(9)           COMP-3.         
014200                                                                          
014300     EJECT                                                                
014400 01  TRANS-TAB-START             PIC X(24)   VALUE                        
014500                                 'TRANS-TAB-START  '.                     
014600*    --- NEDANSTÅENDE TABELL ANVÄNDS FÖR ATT SPARA INFO                   
014700*    --- OM VILKET PRISOMRÅDE ETT DISTRIKT HAR.                           
014800*    --- IDDISTR ANVÄNDS OM INDEX.                                        
014900*    --- INITIALT ÄR ALLA ELEMENT = SPACE.                                
015000 01  TRANS-TAB.                                                           
015100     03  TRANS-TAB-RAD OCCURS 9999.                                       
015200        05  TRANS-TAB-IDPROMR        PIC X(3).                            
015300                                                                          
015400     EJECT                                                                
015500 01  STAT-AREA-START             PIC X(24)   VALUE                        
015600                                 'STAT-AREA-START  '.                     
015700     SKIP2                                                                
015800                                                                          
015900*01  AREA -COPY W33014     -PRE STAT-                                     
016000     EJECT                                                                
016100 01  MB-AREA-START               PIC X(24)   VALUE                        
016200                                 'MB-AREA-START  '.                       
016300     SKIP2                                                                
016400                                                                          
016500*01  AREA -COPY W33036     -PRE MB-                                       
016600     EJECT                                                                
016700 01  UT-AREA-START              PIC X(24)   VALUE                         
016800                                 'UT-AREA-START  '.                       
016900     SKIP2                                                                
017000                                                                          
017100*01  AREA -COPY W33038     -PRE UT-                                       
017200     EJECT                                                                
017300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017400*                                                                         
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017700     SKIP3                                                                
017800 01  NYCKLAR-TILL-DLI.                                                    
017900     03  W-WDB101KY-X.                                                    
018000         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
018100         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
018200                                                                          
018300     03  W-IDGMT-MIN-X.                                                   
018400         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
018500         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
018600                                                                          
018700     03  W-IDGMT-MAX-X.                                                   
018800         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
018900         05  W-IDKUNDNR-MAX      PIC S9(7)                                
019000                                 VALUE +9999999 COMP-3.                   
019100     03  W-IDGMT-X.                                                       
019200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
019300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
019400     SKIP2                                                                
019500*    --- STATUS-KOD FRÅN IMS                                              
019600 01  STATUS-WS                   PIC XX.                                  
019700     88  SEGMENT-FINNS                       VALUE '  '.                  
019800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020000     SKIP2                                                                
020100 01  GODK-STATUSKODER.                                                    
020200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020300     SKIP3                                                                
020400 01  SSA1                        PIC X(64).                               
020500 01  SSA2                        PIC X(64).                               
020600     EJECT                                                                
020700*    --- IMS FUNKTIONSKODER                                               
020800*01  -COPY W0003                                                          
020900     EJECT                                                                
021000*    ---  DLI INPUT-OUTPUT AREA                                           
021100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021200                                                                          
021300 01  DLI-IO-AREA.                                                         
021400*  03  -COPY WDB101  -PRE WDB1-                                           
021500     EJECT                                                                
021600 01  DLI-IO-AREA2.                                                        
021700*    03  WLGMTA01  -COPY WDB201 -PRE GMTA-                                
021800     SKIP3                                                                
021900 LINKAGE SECTION.                                                         
022000*01  -COPY W0008  -PRE WDB1-                                              
022100     05  FILLER                  PIC X.                                   
022200                                                                          
022300*01  -COPY W0008  -PRE GMTA-                                              
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600 PROCEDURE DIVISION  USING WDB1-PCB GMTA-PCB.                             
022700 MAIN SECTION.                                                            
022800     ENTRY 'DLITCBL' USING WDB1-PCB GMTA-PCB.                             
022900                                                                          
023000     PERFORM A-INIT                                                       
023100     PERFORM B-BYGG-URVALSTABELL                                          
023200     PERFORM S01-LAES-W33015                                              
023300     MOVE STAT-IDARTNR TO SPAR-IDARTNR                                    
023400     PERFORM UNTIL END-OF-W33015                                          
023500       PERFORM UNTIL STAT-IDARTNR NOT = SPAR-IDARTNR OR                   
023600                     END-OF-W33015                                        
023700         PERFORM C-HAEMTA-IDPROMR                                         
023800         IF  DISTRIKT-PROMR-FINNS = JA                                    
023900           PERFORM D-KOLLA-OM-GILTIG-IDPROMR                              
024000*          IF GODKAEND-IDPROMR                                            
024100*            PERFORM E-FLYTTA-TILL-TABELL                                 
024200*          END-IF                                                         
024300         END-IF                                                           
024400         PERFORM S01-LAES-W33015                                          
024500       END-PERFORM                                                        
024600       PERFORM F-SKRIV-UTFIL                                              
024700       MOVE STAT-IDARTNR TO SPAR-IDARTNR                                  
024800     END-PERFORM                                                          
024900                                                                          
025000                                                                          
025100     PERFORM Z-FINIT                                                      
025200                                                                          
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 A-INIT SECTION.                                                          
025800     MOVE 'A-INIT'     TO WS-SEKTION                                      
025900*    DISPLAY WS-SEKTION                                                   
026000                                                                          
026100     INITIALIZE TABELL                                                    
026200                                                                          
026300     OPEN INPUT  W33015                                                   
026400                 W33036                                                   
026500                                                                          
026600     OPEN OUTPUT W33038                                                   
026700     SKIP2                                                                
026800*    CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
026900*    MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
027000*    MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
027100*    MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
027200*    MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027300                                                                          
027400*    --- NOLLSTÄLL TRANS-TAB (KOPPLING DISTRIKT-PRISOMRÅDE)               
027500     MOVE 1 TO TRANS-IX                                                   
027600     PERFORM UNTIL TRANS-IX > 9999                                        
027700       MOVE SPACE TO TRANS-TAB-IDPROMR (TRANS-IX)                         
027800       ADD 1 TO TRANS-IX                                                  
027900     END-PERFORM                                                          
028000     .                                                                    
028100     EJECT                                                                
028200 B-BYGG-URVALSTABELL SECTION.                                             
028300     MOVE 'B-BYGG-URVALSTABELL'     TO WS-SEKTION                         
028400*    DISPLAY WS-SEKTION                                                   
028500     SKIP2                                                                
028600     PERFORM S02-LAES-W33036                                              
028700     MOVE +1 TO TAB-IX                                                    
028800     MOVE +0 TO MAX-TAB-IX                                                
028900     PERFORM UNTIL  END-OF-W33036   OR TAB-IX > +1500                     
029000       MOVE MB-IDPROMR          TO TAB-IDPROMR  (TAB-IX)                  
029100       MOVE MB-BEST-IDMARKBO    TO TAB-IDMARKBO (TAB-IX)                  
029200       PERFORM S02-LAES-W33036                                            
029300       ADD +1                        TO TAB-IX                            
029400                                        MAX-TAB-IX                        
029500     END-PERFORM                                                          
029600     IF TAB-IX > 1500                                                     
029700       PERFORM S99-ABEND                                                  
029800     END-IF                                                               
029900     IF TAB-IX = 1                                                        
030000*TOMMA BESTÄLLNINGSFILER : NU SLIPPER MAN LÄSA SEK-REG TILL SLUT.         
030100        SET END-OF-W33015 TO TRUE                                         
030200     END-IF                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 C-HAEMTA-IDPROMR SECTION.                                                
030600     MOVE 'C-HAEMTA-IDPROMR'     TO WS-SEKTION                            
030700*    DISPLAY WS-SEKTION                                                   
030800     SKIP2                                                                
030900*    -- SÖK FÖRST I TRANS-TAB                                             
031000     MOVE STAT-IDDISTR TO TRANS-IX                                        
031100     IF TRANS-TAB-IDPROMR (TRANS-IX) = SPACE                              
031200                                                                          
031300*      -- PRISOMRÅDE SAKNAS. LÄS KUNDREGISTRET                            
031400                                                                          
031600       MOVE STAT-IDDISTR                 TO W-IDDISTR-MIN                 
031700                                            W-IDDISTR-MAX                 
031800       PERFORM IMS-GET-WLGMTA01                                           
031900       IF SEGMENT-FINNS                                                   
032000          MOVE GMTA-GMT-IDPARTNR    TO W-WDB1-IDPARTNR                    
032100          MOVE GMTA-GMT-IDFTG       TO W-WDB1-IDFTG                       
032200                                                                          
032300******  WDB1 LÄSES FÖR ATT HÄMTA PRISOMRÅDE OCH     **********            
032400******  MARKNADSBOLAG. DESSA BEHÖVS FÖR ATT VETA    **********            
032500******  OM DETTA DISTRIKT HÖR TILL SÖKT PRISOMRÅDE  **********            
032600                                                                          
032700          PERFORM IMS-GU-WDB101                                           
032800          IF SEGMENT-FINNS                                                
032900             MOVE JA TO DISTRIKT-PROMR-FINNS                              
033000             MOVE WDB1-BET-IDPROMR TO TRANS-TAB-IDPROMR (TRANS-IX)        
033100          ELSE                                                            
033200             MOVE NEJ TO DISTRIKT-PROMR-FINNS                             
033300          END-IF                                                          
033400**************************************************************            
033500       ELSE                                                               
033600         MOVE NEJ TO DISTRIKT-PROMR-FINNS                                 
033700       END-IF                                                             
033800     ELSE                                                                 
033900       MOVE JA TO DISTRIKT-PROMR-FINNS                                    
034000     END-IF                                                               
034100     SKIP2                                                                
034200     .                                                                    
034300     EJECT                                                                
034400 D-KOLLA-OM-GILTIG-IDPROMR SECTION.                                       
034500     MOVE 'D-KOLLA-OM-GILTIG-IDPROMR'  TO WS-SEKTION                      
034600*    DISPLAY WS-SEKTION                                                   
034700     SKIP2                                                                
034800     MOVE +1 TO TAB-IX                                                    
034900*    MOVE NEJ TO GODKAEND-IDPROMR-SW                                      
035000     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
035100       IF  TRANS-TAB-IDPROMR (TRANS-IX) =  TAB-IDPROMR(TAB-IX)            
035200       AND TRANS-TAB-IDPROMR (TRANS-IX) NOT = SPACE                       
035300          ADD STAT-SUARTFSG-DO-RAAR  TO                                   
035400                       TAB-SUARTFSG-DO-RAAR(TAB-IX)                       
035500          ADD STAT-SULEVANT-DO-RAAR  TO                                   
035600                       TAB-SULEVANT-DO-RAAR(TAB-IX)                       
035700          ADD STAT-SUARTFSG-RAAR     TO                                   
035800                       TAB-SUARTFSG-RAAR (TAB-IX)                         
035900          ADD STAT-SULEVANT-RAAR     TO                                   
036000                       TAB-SULEVANT-RAAR (TAB-IX)                         
036100*        MOVE JA TO GODKAEND-IDPROMR-SW                                   
036200*        MOVE TAB-IX TO GODK-IX                                           
036300*        ADD MAX-TAB-IX TO TAB-IX                                         
036400       END-IF                                                             
036500       ADD +1 TO TAB-IX                                                   
036600     END-PERFORM                                                          
036700     .                                                                    
036800     EJECT                                                                
036900*E-FLYTTA-TILL-TABELL SECTION.                                            
037000*    MOVE 'E-FLYTTA-TILL-TABELL'   TO WS-SEKTION                          
037100*    DISPLAY WS-SEKTION                                                   
037200*    SKIP2                                                                
037300*    ADD STAT-SUARTFSG-DO-RAAR  TO TAB-SUARTFSG-DO-RAAR(GODK-IX)          
037400*    ADD STAT-SULEVANT-DO-RAAR  TO TAB-SULEVANT-DO-RAAR(GODK-IX)          
037500*    ADD STAT-SUARTFSG-RAAR     TO TAB-SUARTFSG-RAAR (GODK-IX)            
037600*    ADD STAT-SULEVANT-RAAR     TO TAB-SULEVANT-RAAR (GODK-IX)            
037700*    .                                                                    
037800*    EJECT                                                                
037900 F-SKRIV-UTFIL SECTION.                                                   
038000     MOVE 'F-SKRIV-UTFIL'          TO WS-SEKTION                          
038100*    DISPLAY WS-SEKTION                                                   
038200*****MOVE 'A'                      TO UT-IDVTYP                           
038300     MOVE SPAR-IDARTNR             TO UT-IDARTNR                          
038400     MOVE +1 TO TAB-IX                                                    
038500     PERFORM UNTIL TAB-IX > MAX-TAB-IX OR                                 
038600             TAB-IDPROMR(TAB-IX) = SPACE                                  
038700       IF TAB-SUARTFSG-DO-RAAR(TAB-IX) > ZERO OR                          
038800            TAB-SULEVANT-DO-RAAR(TAB-IX) > ZERO OR                        
038900            TAB-SUARTFSG-RAAR   (TAB-IX) > ZERO OR                        
039000            TAB-SULEVANT-RAAR   (TAB-IX) > ZERO                           
039100         MOVE TAB-IDPROMR(TAB-IX)          TO UT-IDPROMR                  
039200         MOVE TAB-IDMARKBO(TAB-IX)         TO UT-BEST-IDMARKBO            
039300         MOVE TAB-SUARTFSG-DO-RAAR(TAB-IX) TO UT-SUARTFSG-DO-RAAR         
039400         MOVE TAB-SULEVANT-DO-RAAR(TAB-IX) TO UT-SULEVANT-DO-RAAR         
039500         MOVE TAB-SUARTFSG-RAAR   (TAB-IX) TO UT-SUARTFSG-RAAR            
039600         MOVE TAB-SULEVANT-RAAR   (TAB-IX) TO UT-SULEVANT-RAAR            
039700         PERFORM S11-SKRIV-W33038                                         
039800         MOVE ZERO        TO TAB-SUARTFSG-DO-RAAR(TAB-IX)                 
039900         MOVE ZERO        TO TAB-SULEVANT-DO-RAAR(TAB-IX)                 
040000         MOVE ZERO        TO TAB-SUARTFSG-RAAR   (TAB-IX)                 
040100         MOVE ZERO        TO TAB-SULEVANT-RAAR   (TAB-IX)                 
040200       END-IF                                                             
040300       ADD +1 TO TAB-IX                                                   
040400     END-PERFORM                                                          
040500     .                                                                    
040600     EJECT                                                                
040700 Z-FINIT SECTION.                                                         
040800     MOVE 'Z-FINIT'            TO WS-SEKTION                              
040900*    DISPLAY WS-SEKTION                                                   
041000     CLOSE W33015                                                         
041100           W33036                                                         
041200           W33038                                                         
041300     SKIP2                                                                
041400     MOVE 'S' TO POSTSUM-OPKOD                                            
041500     CALL POSTSUM USING POSTSUM-PARM                                      
041600     .                                                                    
041700     EJECT                                                                
041800 S01-LAES-W33015  SECTION.                                                
041900     MOVE 'S01-LAES-W33015'    TO WS-FIL-SEKTION                          
042000*    DISPLAY WS-FIL-SEKTION                                               
042100     SKIP2                                                                
042200     READ W33015 INTO STAT-AREA                                           
042300     AT END                                                               
042400        SET END-OF-W33015 TO TRUE                                         
042500                                                                          
042600     NOT AT END                                                           
042700        MOVE 'W33015'   TO POSTSUM-FDNAMN                                 
042800        MOVE 'W33035D1' TO POSTSUM-DDNAMN2                                
042900        MOVE 'STAT'     TO POSTSUM-TRANSTYP                               
043000        CALL POSTSUM USING POSTSUM-PARM                                   
043100     END-READ                                                             
043200     .                                                                    
043300     EJECT                                                                
043400 S02-LAES-W33036  SECTION.                                                
043500     MOVE 'S02-LAES-W33036'    TO WS-FIL-SEKTION                          
043600*    DISPLAY WS-FIL-SEKTION                                               
043700     SKIP2                                                                
043800     READ W33036 INTO MB-AREA                                             
043900     AT END                                                               
044000        SET END-OF-W33036 TO TRUE                                         
044100                                                                          
044200     NOT AT END                                                           
044300        MOVE 'W33036'   TO POSTSUM-FDNAMN                                 
044400        MOVE 'W33035D2' TO POSTSUM-DDNAMN2                                
044500        MOVE 'MB'       TO POSTSUM-TRANSTYP                               
044600        CALL POSTSUM USING POSTSUM-PARM                                   
044700     END-READ                                                             
044800     .                                                                    
044900     EJECT                                                                
045000 S11-SKRIV-W33038 SECTION.                                                
045100     MOVE 'S11-SKRIV-W33038'    TO WS-FIL-SEKTION                         
045200*    DISPLAY WS-FIL-SEKTION                                               
045300     SKIP2                                                                
045400     WRITE UT-POST FROM UT-AREA                                           
045500                                                                          
045600     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
045700     MOVE 'W33038'   TO POSTSUM-FDNAMN                                    
045800     MOVE 'W33035D3' TO POSTSUM-DDNAMN2                                   
045900     CALL POSTSUM USING POSTSUM-PARM                                      
046000     .                                                                    
046100     EJECT                                                                
046200 S99-ABEND SECTION.                                                       
046300                                                                          
046400     MOVE 'S' TO POSTSUM-OPKOD                                            
046500     CALL POSTSUM USING POSTSUM-PARM                                      
046600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
046700     .                                                                    
046800     EJECT                                                                
046900* --- IMS SEKTIONER ---                                                   
047000                                                                          
047100 IMS-GU-WDB101 SECTION.                                                   
047200     MOVE 'IMS-GU-WDB101'    TO WS-IMS-SEKTION                            
047300*    DISPLAY WS-IMS-SEKTION                                               
047400     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
047500          DELIMITED BY SIZE INTO SSA1                                     
047600     MOVE '  GE' TO GODK-STATUSKODER                                      
047700     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA SSA1                      
047800     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
047900     PERFORM IMS-STATUSKONTROLL                                           
048000     .                                                                    
048100     EJECT                                                                
048200 IMS-GET-WLGMTA01  SECTION.                                               
048300     MOVE 'IMS-GET-WLGMTA01'    TO WS-IMS-SEKTION                         
048400*    DISPLAY WS-IMS-SEKTION                                               
048500                                                                          
048600     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
048700                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
048800            DELIMITED BY SIZE INTO SSA1                                   
048900     MOVE '  GE' TO GODK-STATUSKODER                                      
049000     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA2 SSA1                     
049100     MOVE GMTA-STATUS-CODE  TO STATUS-WS                                  
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     SKIP2                                                                
049500 IMS-GU-WLGMTA01 SECTION.                                                 
049600     MOVE 'IMS-GU-WLGMTA01'    TO WS-IMS-SEKTION                          
049700     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
049800          DELIMITED BY SIZE INTO SSA1                                     
049900     MOVE '  GE'           TO GODK-STATUSKODER                            
050000     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA2 SSA1                     
050100     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400     EJECT                                                                
050500                                                                          
050600 IMS-STATUSKONTROLL SECTION.                                              
050700     SKIP2                                                                
050800     SET STATUS-IX TO 1                                                   
050900     SEARCH GODK-STATUS                                                   
051000       AT END                                                             
051100         MOVE 'IMS ABEND   ' TO FELTEXT-STR                               
051200         DISPLAY FELTEXT                                                  
051300         CALL FELLOG                                                      
051400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
051500         CONTINUE                                                         
051600     END-SEARCH                                                           
051700     .                                                                    
