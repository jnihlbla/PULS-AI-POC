000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3713900.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   95/05/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        - HÄMTAR PRIS OCH OMRÄKNAR TILL LOKAL VALUTA.                    
001000*        - SKICKAR INTRASTAT DATA FÖR "BILLIT" MARKNADER TILL W522        
001100*          GENOM ATT ANROPA WZ01SEND (CARPARTS.PULS.RECEIVEI)             
001200*                                                                         
001300*        - PROGRAMMET LÄSER MÅNADSKURSER WDG2                             
001400*                           ARTIKELREG   WDK6                             
001500*                           KUNDREG      WDB2                             
001600*                           BEN.REG      WDD3                             
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700                                                                          
002800*          --- VALD TULLINFO                                              
002900     SELECT W37139                     ASSIGN TO W37139D1.                
003000                                                                          
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400 FD  W37139                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700*01  -COPY W37138      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W3713900'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  IX                          PIC S9(3) COMP SYNC VALUE ZERO.          
004500 77  TAB-MAX                     PIC S9(3) COMP SYNC VALUE +200.          
004600 77  GEN-IDSTATNR                PIC S9(9) COMP-3 VALUE 87089997.         
004700 77  WS-HELTAL-9                 PIC S9(9).                               
004800 77  SPAR-FAKT-PRKURS            PIC S9(6)V9(5) COMP-3.                   
004900 77  SPAR-FAKT-SUORDV-FAKT       PIC S9(9)V9(2) COMP-3.                   
005000 77  SPAR-FAKT-SUORDV-LOC        PIC S9(9)V9(2) COMP-3.                   
005100                                                                          
005200 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005300 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005400                                                                          
005500 77  WS-PRKURS-GBP               PIC S9(6)V9(5) COMP-3.                   
005600 77  WS-PRKURS-EUR               PIC S9(6)V9(5) COMP-3.                   
005700                                                                          
005800 77  W37139-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W37139                       VALUE 'J'.                   
006000     EJECT                                                                
006100 01  WS-DADATUM.                                                          
006200     03  WS-DATUM-SEKEL          PIC 9(2)    VALUE 20.                    
006300     03  WS-DATUM-AAMMDD         PIC 9(6).                                
006400                                                                          
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100                                                                          
007200 01  WS-ID                        PIC 9(12)  VALUE ZERO.                  
007300 01  FILLER REDEFINES WS-ID.                                              
007400     03  WS-IDDISTR               PIC 9(5).                               
007500     03  WS-IDBYTRAP              PIC 9(7).                               
007600                                                                          
007700 01  SPAR-ID                      PIC 9(12)  VALUE ZERO.                  
007800 01  FILLER REDEFINES SPAR-ID.                                            
007900     03  SPAR-IDDISTR             PIC 9(5).                               
008000     03  SPAR-IDBYTRAP            PIC 9(7).                               
008100*TABELL-AREA                                                              
008200 01  TABELLAREA                  PIC X(24)   VALUE                        
008300                                 'TABELLEN       '.                       
008400 01  TABELL-RAPP.                                                         
008500   03  TABELLEN       OCCURS 200.                                         
008600     05  -COPY W4758TU    -PRE TAB-                                       
008700     EJECT                                                                
008800                                                                          
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000*                                                                         
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
009600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009700     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
009800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009900     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
010000                                                                          
010100*    --- PARAMETRAR TILL ABEND                                            
010200                                                                          
010300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010400                                                                          
010500 01  FELTEXT.                                                             
010600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010800     EJECT                                                                
010900                                                                          
011000*    --- PARAMETRAR TILL POSTSUM                                          
011100*                                                                         
011200*01  -COPY W0005   -PRE  POSTSUM-                                         
011300     EJECT                                                                
011400                                                                          
011500*    --- PARAMETRAR TILL W510CURR                                         
011600*                                                                         
011700*01  -COPY W510CURR                                                       
011800     EJECT                                                                
011900                                                                          
012000*    --- PARAMETRAR TILL W460DS1                                          
012100*                                                                         
012200*01  -COPY W460DIS1                                                       
012300     EJECT                                                                
012400                                                                          
012500*01  -COPY W460LISO                                                       
012600     EJECT                                                                
012700                                                                          
012800*    --- PARAMETRAR TILL DATKORT                                          
012900*                                                                         
013000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37139'.              
013100                                                                          
013200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013300                                                                          
013400*01  -COPY WDATKORT                                                       
013500     EJECT                                                                
013600                                                                          
013700*    --- PARAMETRAR TILL W009CIA                                          
013800*01  -COPY W009CIA                                                        
013900     EJECT                                                                
014000                                                                          
014100*    --- AREOR FÖR KOMMUNIKATION                                          
014200 01  WS-ADRESS                   PIC X(50)                                
014300                                 VALUE 'CARPARTS.PULS.RECEIVEI'.          
014400 01  ERRTEXT.                                                             
014500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
014600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
014700 01  KDRC-DISPLAY                PIC Z(5).                                
014800                                                                          
014900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
015000*01  -COPY WZ01SEND                                                       
015100     EJECT                                                                
015200                                                                          
015300 01  TULL-AREA-START             PIC X(24)   VALUE                        
015400                                 'TULL-AREA-START  '.                     
015500                                                                          
015600 01  INTUT-AREA-START            PIC X(24)   VALUE                        
015700                                 'INTUT-AREA-START'.                      
015800 01  INTUT-AREA.                                                          
015900*    03  FILLER -COPY WZ01REQU  -PRE INTUT-                               
016000*    03  FILLER -COPY WF2102I1  -PRE INTUT-                               
016100     EJECT                                                                
016200                                                                          
016300 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
016400*01  FILLER  -COPY WWDIST01   -RED TEST-IDDISTR.                          
016500     EJECT                                                                
016600                                                                          
016700*01  AREA -COPY W37138     -PRE TULL-                                     
016800     EJECT                                                                
016900                                                                          
017000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300                                                                          
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-IDARTNR-X.                                                     
017600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017700     03  W-IDSKYLT-X.                                                     
017800         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
017900     03  W-IDGMT-X.                                                       
018000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
018100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
018200     EJECT                                                                
018300                                                                          
018400*    --- STATUS-KOD FRÅN IMS                                              
018500 01  STATUS-WS                   PIC XX.                                  
018600     88  SEGMENT-FINNS                       VALUE '  '.                  
018700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018900                                                                          
019000 01  GODK-STATUSKODER.                                                    
019100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019200                                                                          
019300 01  SSA1                        PIC X(64).                               
019400 01  SSA2                        PIC X(64).                               
019500     EJECT                                                                
019600                                                                          
019700*    --- IMS FUNKTIONSKODER                                               
019800*01  -COPY W0003                                                          
019900     EJECT                                                                
020000                                                                          
020100*    ---  DLI INPUT-OUTPUT AREA                                           
020200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020300                                                                          
020400 01  DLI-IO-AREA.                                                         
020500     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
020600                                                                          
020700     03  WLARTC01 REDEFINES IO-AREA.                                      
020800*        05  -COPY WDK601  -PRE ARTC-                                     
020900                                                                          
021000     03  WLARTC11 REDEFINES IO-AREA.                                      
021100*        05  -COPY WDK611  -PRE ARTC-                                     
021200     EJECT                                                                
021300                                                                          
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
021500 01  DLI-IO-WDB201.                                                       
021600*    03 WDB201   -COPY WDB201                                             
021700     EJECT                                                                
021800                                                                          
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
022000 01  DLI-IO-WDD311.                                                       
022100*    03 WDD311   -COPY WDD311                                             
022200     EJECT                                                                
022300                                                                          
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009  -PRE MSG-                                               
022700                                                                          
022800*01  -COPY W0009  -PRE RECEIVEI-                                          
022900                                                                          
023000*01  -COPY W0008  -PRE WDG2-                                              
023100     05  FILLER                  PIC X.                                   
023200                                                                          
023300*01  -COPY W0008  -PRE ARTC-                                              
023400     05  FILLER                  PIC X.                                   
023500                                                                          
023600*01  -COPY W0008  -PRE WDB2-                                              
023700     05  FILLER                  PIC X.                                   
023800                                                                          
023900*01  -COPY W0008  -PRE WDD3-                                              
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200                                                                          
024300 PROCEDURE DIVISION  USING                                                
024400     MSG-PCB                                                              
024500     RECEIVEI-PCB                                                         
024600     WDG2-PCB                                                             
024700     ARTC-PCB                                                             
024800     WDB2-PCB                                                             
024900     WDD3-PCB.                                                            
025000 MAIN SECTION.                                                            
025100     ENTRY 'DLITCBL' USING                                                
025200     MSG-PCB                                                              
025300     RECEIVEI-PCB                                                         
025400     WDG2-PCB                                                             
025500     ARTC-PCB                                                             
025600     WDB2-PCB                                                             
025700     WDD3-PCB.                                                            
025800                                                                          
025900     PERFORM A-INIT                                                       
026000     PERFORM G-HAEMTA-VALUTAKURSER                                        
026100     PERFORM S01-LAES-W37139                                              
026200     PERFORM UNTIL END-OF-W37139                                          
026300       MOVE ZERO TO SPAR-FAKT-PRKURS                                      
026400       MOVE ZERO TO SPAR-FAKT-SUORDV-FAKT                                 
026500       MOVE ZERO TO SPAR-FAKT-SUORDV-LOC                                  
026600       MOVE WS-ID TO SPAR-ID                                              
026700       INITIALIZE TABELL-RAPP                                             
026800       MOVE +1 TO IX                                                      
026900       PERFORM UNTIL WS-ID NOT = SPAR-ID                                  
027000                                                                          
027100         PERFORM B-HAEMTA-WDK6-INFO                                       
027200         PERFORM C-BEHANDLA-VALUTA-RAD                                    
027300         PERFORM D-FLYTTA-FAELT-TILL-TABELL                               
027400                                                                          
027500         PERFORM S01-LAES-W37139                                          
027600         ADD +1 TO IX                                                     
027700       END-PERFORM                                                        
027800       PERFORM E-BEHANDLA-VALUTA-TOTSUM                                   
027900       PERFORM F-SKRIV-TABELL-TILL-FIL                                    
028000     END-PERFORM                                                          
028100                                                                          
028200                                                                          
028300     PERFORM Z-FINIT                                                      
028400                                                                          
028500     MOVE ZERO TO RETURN-CODE                                             
028600     GOBACK                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 A-INIT SECTION.                                                          
029000                                                                          
029100     OPEN INPUT  W37139                                                   
029200                                                                          
029300     PERFORM S90-SEND-OPEN                                                
029400                                                                          
029500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
029600     MOVE D-AAR           TO DAGENS-DATUM-AAR                             
029700                             W-DATE-AAMM(1:2)                             
029800     MOVE D-MAANAD        TO DAGENS-DATUM-MAANAD                          
029900                             W-DATE-AAMM(3:2)                             
030000     MOVE D-DAG           TO DAGENS-DATUM-DAG                             
030100                                                                          
030200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
030300     .                                                                    
030400     EJECT                                                                
030500 B-HAEMTA-WDK6-INFO SECTION.                                              
030600     SKIP2                                                                
030700     MOVE TULL-IDARTNR-OBJ TO W-IDARTNR                                   
030800                              TAB-IDARTNR(IX)                             
030900     PERFORM IMS-GET-ARTC-ART                                             
031000     IF ARTC-CLAG-IDSTATNR(3)     = ZERO                                  
031100       MOVE GEN-IDSTATNR          TO TAB-IDSTATNR(IX)                     
031200     ELSE                                                                 
031300       MOVE ARTC-CLAG-IDSTATNR(3) TO TAB-IDSTATNR(IX)                     
031400     END-IF                                                               
031500     MOVE ARTC-CLAG-KDARTURS    TO TAB-KDARTURS(IX)                       
031600     MOVE ARTC-CLAG-KDSRA       TO TAB-KDSRA   (IX)                       
031700     MOVE TULL-KVRETUR-GODK     TO TAB-KVLEVART(IX)                       
031800     COMPUTE TAB-SUFAKT(IX) ROUNDED =                                     
031900          ARTC-CLAG-PRARTSTD * TULL-KVRETUR-GODK                          
032000     COMPUTE TAB-VKLEV(IX) ROUNDED =                                      
032100       (ARTC-CLAG-VKART * TULL-KVRETUR-GODK) / 1000                       
032200     .                                                                    
032300     EJECT                                                                
032400 C-BEHANDLA-VALUTA-RAD SECTION.                                           
032500                                                                          
032600     MOVE TULL-IDDISTR       TO TEST-IDDISTR                              
032700                                DIS1-IDDISTR                              
032800     IF  DIST01-FRANCE-TULL                                               
032900         MOVE 'FR'               TO TAB-IDLANDX2 (IX)                     
033000         MOVE WS-PRKURS-EUR      TO SPAR-FAKT-PRKURS                      
033100     ELSE                                                                 
033200       IF  DIST01-ITALIEN-TULL                                            
033300           MOVE 'IT'             TO TAB-IDLANDX2 (IX)                     
033400           MOVE WS-PRKURS-EUR    TO SPAR-FAKT-PRKURS                      
033500       ELSE                                                               
033600         IF  DIST01-ENGLAND-TULL                                          
033700           CALL W460DIS1 USING DIS1-W460DIS1                              
033800                                                                          
033900           IF DIS1-IDLANDX2 = ISO-IRLAND                                  
034000             MOVE 'IE'           TO TAB-IDLANDX2 (IX)                     
034100             MOVE WS-PRKURS-EUR  TO SPAR-FAKT-PRKURS                      
034200           ELSE                                                           
034300             MOVE 'GB'           TO TAB-IDLANDX2 (IX)                     
034400             MOVE WS-PRKURS-GBP  TO SPAR-FAKT-PRKURS                      
034500           END-IF                                                         
034600         ELSE                                                             
034700            IF  DIST01-SPANIEN-TULL                                       
034800                MOVE 'ES'    TO TAB-IDLANDX2 (IX)                         
034900                MOVE WS-PRKURS-EUR  TO SPAR-FAKT-PRKURS                   
035000            ELSE                                                          
035100              IF  DIST01-AUSTRIA-TULL                                     
035200                  MOVE 'AT'    TO TAB-IDLANDX2 (IX)                       
035300                  MOVE WS-PRKURS-EUR  TO SPAR-FAKT-PRKURS                 
035400              ELSE                                                        
035500**** OM DISTRIKTET SAKNAS GÅR DET EJ ATT BESTÄMMA PRKURS ***              
035600**** DET FÅR INTE ATT STÄMMA AV TULL-LISTOR FÖR BYTES    ***              
035700                CALL ABEND                                                
035800                                                                          
035900              END-IF                                                      
036000            END-IF                                                        
036100         END-IF                                                           
036200       END-IF                                                             
036300     END-IF                                                               
036400     COMPUTE TAB-SUFAKT-LOC(IX) ROUNDED = TAB-SUFAKT(IX)   /              
036500                                        SPAR-FAKT-PRKURS                  
036600                                                                          
036700     IF TAB-KDSRA(IX) = +1 OR +2                                          
036800        MOVE TAB-SUFAKT(IX)      TO TAB-SUEEC(IX)                         
036900        MOVE TAB-SUFAKT-LOC(IX)  TO TAB-SUEEC-LOC(IX)                     
037000     ELSE                                                                 
037100        MOVE ZERO                TO TAB-SUEEC(IX)                         
037200                                    TAB-SUEEC-LOC(IX)                     
037300     END-IF                                                               
037400                                                                          
037500     IF TAB-KDSRA(IX) = +3 OR +4                                          
037600        MOVE TAB-SUFAKT(IX)      TO TAB-SUEFTA(IX)                        
037700        MOVE TAB-SUFAKT-LOC(IX)  TO TAB-SUEFTA-LOC(IX)                    
037800     ELSE                                                                 
037900        MOVE ZERO                TO TAB-SUEFTA (IX)                       
038000                                    TAB-SUEFTA-LOC (IX)                   
038100     END-IF                                                               
038200                                                                          
038300     IF TAB-KDSRA(IX) = +0 OR                                             
038400        TAB-KDSRA(IX) > +4                                                
038500        MOVE TAB-SUFAKT(IX)      TO TAB-SUOEVR(IX)                        
038600        MOVE TAB-SUFAKT-LOC(IX)  TO TAB-SUOEVR-LOC(IX)                    
038700     ELSE                                                                 
038800       MOVE ZERO                 TO TAB-SUOEVR(IX)                        
038900                                    TAB-SUOEVR-LOC(IX)                    
039000     END-IF                                                               
039100                                                                          
039200     .                                                                    
039300     EJECT                                                                
039400 D-FLYTTA-FAELT-TILL-TABELL SECTION.                                      
039500     MOVE TULL-IDDISTR       TO DIS1-IDDISTR                              
039600     CALL W460DIS1 USING DIS1-W460DIS1                                    
039700                                                                          
039800     IF DIS1-IDLANDX2 = ISO-IRLAND                                        
039900       MOVE 'CO2'            TO TAB-IDPTYP   (IX)                         
040000     ELSE                                                                 
040100       MOVE 'COR'            TO TAB-IDPTYP   (IX)                         
040200     END-IF                                                               
040300                                                                          
040400* OBS OBS HÄR GÖRS RAPPNR "OM" TILL FAKTURANUMMER.                        
040500     MOVE TULL-IDDC          TO TAB-IDDC     (IX)                         
040600     MOVE TULL-IDBYTRAP      TO TAB-IDFAKT   (IX)                         
040700     MOVE TULL-IDDISTR       TO TAB-IDDISTR  (IX)                         
040800     MOVE TULL-IDKUNDNR      TO TAB-IDKUNDNR (IX)                         
040900     MOVE ZERO               TO TAB-IDPRODNR (IX)                         
041000     MOVE SPACE              TO TAB-KDFAKTYP (IX)                         
041100     MOVE SPACE              TO TAB-KDFAKTYP (IX)                         
041200     MOVE TULL-TIREGDAT-GODK TO TAB-TIFAKT   (IX)                         
041300     SKIP2                                                                
041400     .                                                                    
041500     EJECT                                                                
041600 E-BEHANDLA-VALUTA-TOTSUM SECTION.                                        
041700     SKIP2                                                                
041800*    BERÄKNA TOTAL SUMMAN                                                 
041900     MOVE +1 TO IX                                                        
042000     PERFORM UNTIL IX > TAB-MAX OR TAB-IDPTYP(IX) = SPACE                 
042100       ADD TAB-SUFAKT(IX)  TO  SPAR-FAKT-SUORDV-FAKT                      
042200       ADD +1 TO IX                                                       
042300     END-PERFORM                                                          
042400*    BERÄKNA KURS                                                         
042500     COMPUTE SPAR-FAKT-SUORDV-LOC ROUNDED =                               
042600                                     SPAR-FAKT-SUORDV-FAKT /              
042700                                     SPAR-FAKT-PRKURS                     
042800     MOVE +1 TO IX                                                        
042900*    LÄGG IN TOTSUMMA SEK OCH LOKALVALUTA                                 
043000     PERFORM UNTIL IX > TAB-MAX OR TAB-IDPTYP(IX) = SPACE                 
043100       MOVE SPAR-FAKT-SUORDV-FAKT TO TAB-SUORDV-FAKT(IX)                  
043200       MOVE SPAR-FAKT-SUORDV-LOC  TO TAB-SUORDV-FAKT-LOC(IX)              
043300       ADD +1 TO IX                                                       
043400     END-PERFORM                                                          
043500     .                                                                    
043600     EJECT                                                                
043700                                                                          
043800 F-SKRIV-TABELL-TILL-FIL SECTION.                                         
043900                                                                          
044000     MOVE +1 TO IX                                                        
044100     MOVE TAB-IDDISTR(IX)                   TO TEST-IDDISTR               
044200                                                                          
044300     IF DIST01-FRANCE-TULL   OR                                           
044400        DIST01-ITALIEN-TULL  OR                                           
044500        DIST01-ENGLAND-TULL  OR                                           
044600        DIST01-SPANIEN-TULL  OR                                           
044700        DIST01-AUSTRIA-TULL                                               
044800       PERFORM UNTIL IX > TAB-MAX OR TAB-IDPTYP(IX) = SPACE               
044900         PERFORM FA-NY-INTRASTAT                                          
045000         PERFORM S91-SEND-PUT                                             
045100         ADD +1 TO IX                                                     
045200       END-PERFORM                                                        
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 FA-NY-INTRASTAT SECTION.                                                 
045800     MOVE 1                   TO INTUT-REQU-IDMSGVER                      
045900     MOVE SPACE               TO INTUT-REQU-KDPGMACT                      
046000     MOVE 'W3713900'          TO INTUT-REQU-IDUSER                        
046100                                                                          
046200     MOVE DAGENS-DATUM        TO WS-DATUM-AAMMDD                          
046300     MOVE WS-DADATUM          TO INTUT-DAEXDAT                            
046400                                                                          
046500     MOVE 989898              TO INTUT-TIEXTID                            
046600     MOVE 'INT'               TO INTUT-IDPTYP                             
046700                                                                          
046800     MOVE TAB-IDLANDX2(IX)    TO INTUT-IDLANDX3-SEND                      
046900                                                                          
047000     IF TAB-IDDC(IX) = 91                                                 
047100       MOVE 'NL'              TO INTUT-IDLANDX3-REC                       
047200       MOVE 'NL800213609B07'  TO INTUT-IDVAT-BET                          
047300     ELSE                                                                 
047400       MOVE 'SE'              TO INTUT-IDLANDX3-REC                       
047410       MOVE 'SE556074308901'  TO INTUT-IDVAT-BET                          
047500     END-IF                                                               
047600                                                                          
047700     MOVE SPACE               TO INTUT-IDLANDX3-BET                       
047800                                                                          
047900     IF TAB-IDLANDX2(IX) = 'IE' OR 'IT' OR 'ES' OR 'AT'                   
048000       MOVE 'EUR'             TO INTUT-KDVALISO                           
048100       MOVE WS-PRKURS-EUR     TO INTUT-PRKURS                             
048200     ELSE                                                                 
048300       MOVE 'GBP'             TO INTUT-KDVALISO                           
048400       MOVE WS-PRKURS-GBP     TO INTUT-PRKURS                             
048500     END-IF                                                               
048600                                                                          
048700     MOVE TAB-IDDISTR(IX)     TO W-IDDISTR                                
048800     MOVE TAB-IDKUNDNR(IX)    TO W-IDKUNDNR                               
048900     PERFORM IMS-GET-WDB201                                               
049000     MOVE GMT-IDPARTNR        TO INTUT-IDPARTNR                           
049100                                                                          
049200     MOVE 'EXC'               TO INTUT-KDFINDOC                           
049300                                                                          
049400     MOVE TAB-TIFAKT(IX)      TO WS-DATUM-AAMMDD                          
049500     MOVE WS-DADATUM          TO INTUT-DAFINDOC                           
049600                                                                          
049700     MOVE TAB-IDFAKT(IX)      TO INTUT-IDFINDOC                           
049800                                                                          
049900     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
050000     MOVE TAB-IDDISTR(IX)     TO CIA-IDARTBET-IN                          
050100     CALL W009CIA USING          CIA-W009CIA                              
050200     MOVE CIA-IDARTBET-UT     TO INTUT-IDEXCUST-1                         
050300                                                                          
050400     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
050500     MOVE TAB-IDKUNDNR(IX)    TO CIA-IDARTBET-IN                          
050600     CALL W009CIA USING          CIA-W009CIA                              
050700     MOVE CIA-IDARTBET-UT     TO INTUT-IDEXCUST-2                         
050800                                                                          
050900     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
051000     MOVE TAB-IDARTNR(IX)     TO CIA-IDARTBET-IN                          
051100     CALL W009CIA USING          CIA-W009CIA                              
051200     MOVE CIA-IDARTBET-UT     TO INTUT-IDARTNR-FINANCE                    
051300                                                                          
051400     IF TAB-IDLANDX2(IX)       = 'AT'                                     
051500       MOVE 'D '              TO W-IDSKYLT                                
051600     ELSE                                                                 
051700       IF TAB-IDLANDX2(IX)     = 'GB' OR 'IE'                             
051800         MOVE 'GB'            TO W-IDSKYLT                                
051900       ELSE                                                               
052000         IF TAB-IDLANDX2(IX)   = 'IT'                                     
052100           MOVE 'I '          TO W-IDSKYLT                                
052200         ELSE                                                             
052300           IF TAB-IDLANDX2(IX) = 'ES'                                     
052400             MOVE 'E'         TO W-IDSKYLT                                
052500           END-IF                                                         
052600         END-IF                                                           
052700       END-IF                                                             
052800     END-IF                                                               
052900     PERFORM IMS-GET-WDD311-BSEQ                                          
053000     MOVE TEXT-BEART          TO INTUT-BEART                              
053100                                                                          
053200     MOVE TAB-IDSTATNR(IX)    TO INTUT-IDSTATNR                           
053300     MOVE TAB-VKLEV(IX)       TO INTUT-VKORDNTO                           
053300     MOVE ZERO                TO INTUT-VKORDNTO-3DEC                      
053400     MOVE TAB-KVLEVART(IX)    TO INTUT-KVLEVART                           
053500     MOVE TAB-KDARTURS(IX)    TO INTUT-KDARTURS                           
053600     MOVE ZERO                TO INTUT-KDFRAKT                            
053700     MOVE SPACE               TO INTUT-BELEVVIL                           
053800     MOVE TAB-SUFAKT-LOC(IX)  TO INTUT-SUNTO                              
053900     .                                                                    
054000     EJECT                                                                
054100                                                                          
054200 G-HAEMTA-VALUTAKURSER SECTION.                                           
054300                                                                          
054400     MOVE W-DATE-AAMM        TO CURR-TIAAMM                               
054500     MOVE WS-KDVALISO-HUV    TO CURR-KDVALISO-HUV                         
054600     MOVE 'M'                TO CURR-KDVALTYP                             
054700                                                                          
054800     MOVE 'EUR'              TO CURR-KDVALISO-ROW                         
054900     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
055000     IF CURR-KDSVAR = ' '                                                 
055100       MOVE CURR-PRKURS-NEW  TO WS-PRKURS-EUR                             
055200       DISPLAY '*** EUR*** ' CURR-PRKURS-NEW                              
055300     END-IF                                                               
055400                                                                          
055500     MOVE 'GBP'              TO CURR-KDVALISO-ROW                         
055600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
055700     IF CURR-KDSVAR = ' '                                                 
055800       MOVE CURR-PRKURS-NEW  TO WS-PRKURS-GBP                             
055900       DISPLAY '*** GBP*** ' CURR-PRKURS-NEW                              
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 Z-FINIT SECTION.                                                         
056400     CLOSE W37139                                                         
056500                                                                          
056600     PERFORM S99-SEND-CLOSE                                               
056700                                                                          
056800     MOVE 'S' TO POSTSUM-OPKOD                                            
056900     CALL POSTSUM USING POSTSUM-PARM                                      
057000     .                                                                    
057100     EJECT                                                                
057200 S01-LAES-W37139  SECTION.                                                
057300     READ W37139 INTO TULL-AREA                                           
057400     AT END                                                               
057500        MOVE +99999   TO WS-IDDISTR                                       
057600        MOVE +9999999 TO WS-IDBYTRAP                                      
057700        SET END-OF-W37139 TO TRUE                                         
057800                                                                          
057900     NOT AT END                                                           
058000        MOVE TULL-IDDISTR  TO  WS-IDDISTR                                 
058100        MOVE TULL-IDBYTRAP TO  WS-IDBYTRAP                                
058200        MOVE 'W37139' TO POSTSUM-FDNAMN                                   
058300        MOVE 'W37139D1' TO POSTSUM-DDNAMN2                                
058400        MOVE 'TULL' TO POSTSUM-TRANSTYP                                   
058500        CALL POSTSUM USING POSTSUM-PARM                                   
058600     END-READ                                                             
058700     .                                                                    
058800     EJECT                                                                
058900                                                                          
059000 S90-SEND-OPEN SECTION.                                                   
059100     MOVE WS-ADRESS                       TO SEND-ADDISPABS               
059200     MOVE 'OPEN'                          TO SEND-KDFUNC                  
059300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
059400                         SEND-OPEN-AREA                                   
059500     IF SEND-KDRC > ZERO                                                  
059600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
059700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
059800       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
059900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060000     END-IF                                                               
060100     .                                                                    
060200                                                                          
060300 S91-SEND-PUT SECTION.                                                    
060400     MOVE 'PUT'                           TO SEND-KDFUNC                  
060500     MOVE LENGTH OF INTUT-AREA            TO SEND-KVDLEN                  
060600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
060700                         SEND-KVDLEN                                      
060800                         INTUT-AREA                                       
060900     IF SEND-KDRC > ZERO                                                  
061000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
061100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
061200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
061300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
061400     END-IF                                                               
061500     .                                                                    
061600                                                                          
061700 S99-SEND-CLOSE SECTION.                                                  
061800     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
061900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
062000     .                                                                    
062100     EJECT                                                                
062200                                                                          
062300* --- IMS SEKTIONER ---                                                   
062400                                                                          
062500 IMS-GET-ARTC-ART SECTION.                                                
062600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
062700          DELIMITED BY SIZE INTO SSA1                                     
062800     MOVE 'WLARTC11 ' TO SSA2                                             
062900     MOVE '  ' TO GODK-STATUSKODER                                        
063000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
063100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400                                                                          
063500 IMS-GET-WDB201 SECTION.                                                  
063600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
063700          DELIMITED BY SIZE INTO SSA1                                     
063800     MOVE '  GE'           TO GODK-STATUSKODER                            
063900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
064000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
064100     PERFORM IMS-STATUSKONTROLL                                           
064200     .                                                                    
064300                                                                          
064400 IMS-GET-WDD311-BSEQ SECTION.                                             
064500     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
064600          DELIMITED BY SIZE INTO SSA1                                     
064700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
064800          DELIMITED BY SIZE INTO SSA2                                     
064900     MOVE '  GE'           TO GODK-STATUSKODER                            
065000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
065100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400                                                                          
065500 IMS-STATUSKONTROLL SECTION.                                              
065600     SET STATUS-IX TO 1                                                   
065700     SEARCH GODK-STATUS                                                   
065800       AT END                                                             
065900         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
066000         CALL FELLOG                                                      
066100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066200         CONTINUE                                                         
066300     END-SEARCH                                                           
066400     .                                                                    
