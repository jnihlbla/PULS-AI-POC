000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5104800.                                                
000400*AUTHOR.         THOMAS LARSSON.                                          
000500*DATE-WRITTEN.   95/06/02.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER FILER MED TULL- OCH MOMSINFO FRÅN W510.                    
001100*        HÄMTAR PRKURS FRÅN VALUTABASEN OCH RÄKNAR FRAM PRIS I            
001200*        LOKAL VALUTA.                                                    
001300*      - SKICKAR INTRASTAT DATA FÖR "BILLIT" MARKNADER TILL W522          
001400*        GENOM ATT ANROPA WZ01SEND (CARPARTS.PULS.RECEIVEI)               
001500*      - SKICKAR MOMS DATA FÖR "BILLIT" MARKNADER TILL W522               
001600*        GENOM ATT ANROPA WZ01SEND (CARPARTS.PULS.RECEIVEV)               
001700*                                                                         
001800*        PROGRAMMET LÄSER MÅNADSKURS   WDG2(THRU W510CURR)                
001900*                         KUNDREGISTER WDB2                               
002000*                         BEN.REGISTER WDD3                               
002100*                                                                         
002200*    ABENDKODER:                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- FILER MED TULLINFO.                                        
003100     SELECT W51044                     ASSIGN TO W51048D1.                
003200     SKIP2                                                                
003300*          --- FILER MED MOMSINFO.                                        
003400     SELECT W51045                     ASSIGN TO W51048D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W51044                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W4758TU      -L.                                               
004500     SKIP3                                                                
004600 FD  W51045                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W475896      -L.                                               
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W5104800'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005800 77  WS-HELTAL                   PIC S9(9)   VALUE ZERO COMP-3.           
005900                                                                          
006000 77  WS-PRKURS-GBP               PIC S9(6)V9(5) VALUE ZERO COMP-3.        
006100 77  WS-PRKURS-EUR               PIC S9(6)V9(5) VALUE ZERO COMP-3.        
006110 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006120 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
006200     SKIP2                                                                
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600                                                                          
006700 77  W51045-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W51045                       VALUE 'J'.                   
006900                                                                          
007000 77  W51044-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W51044                       VALUE 'J'.                   
007200                                                                          
007300     EJECT                                                                
007400 01  WS-DATUM.                                                            
007500     03  WS-DATUM-SEKEL          PIC 9(2)    VALUE 20.                    
007600     03  WS-DATUM-AAMMDD         PIC 9(6).                                
007700                                                                          
007800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007900 01  FILLER REDEFINES DAGENS-DATUM.                                       
008000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008300     EJECT                                                                
008400                                                                          
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
009100     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
009200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009210     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
009300     EJECT                                                                
009400                                                                          
009500*    --- PARAMETRAR TILL ABEND                                            
009600                                                                          
009700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009800                                                                          
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100*                                                                         
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400                                                                          
010500*    --- PARAMETRAR TILL DATKORT                                          
010600*                                                                         
010700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51048'.              
010800                                                                          
010900 01  DATUMKORT-ID                PIC X(6)    VALUE '000000'.              
011000                                                                          
011100*01  -COPY WDATKORT                                                       
011200     EJECT                                                                
011300                                                                          
011400*    --- PARAMETRAR TILL W009CIA                                          
011500*01  -COPY W009CIA                                                        
011600     EJECT                                                                
011700                                                                          
011710*    --- PARAMETRAR TILL W510CURR                                         
011720*01  -COPY W510CURR                                                       
011730     EJECT                                                                
011800*    --- AREOR FÖR KOMMUNIKATION                                          
011900 01  WS-ADRESS-I                 PIC X(50)                                
012000                                 VALUE 'CARPARTS.PULS.RECEIVEI'.          
012100 01  WS-ADRESS-V                 PIC X(50)                                
012200                                 VALUE 'CARPARTS.PULS.RECEIVEV'.          
012300                                                                          
012400 01  ERRTEXT.                                                             
012500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
012600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
012700 01  KDRC-DISPLAY                PIC Z(5).                                
012800                                                                          
012900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
013000*01  -COPY WZ01SEND                                                       
013100     EJECT                                                                
013200                                                                          
013300 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
013400                                                                          
013500     EJECT                                                                
013600 01  INTUT-AREA-START            PIC X(24)   VALUE                        
013700                                 'INTUT-AREA-START'.                      
013800 01  INTUT-AREA.                                                          
013900*    03  FILLER -COPY WZ01REQU  -PRE INTUT-                               
014000*    03  FILLER -COPY WF2102I1  -PRE INTUT-                               
014100                                                                          
014200     EJECT                                                                
014300 01  VATUT-AREA-START            PIC X(24)   VALUE                        
014400                                 'VATUT-AREA-START'.                      
014500 01  VATUT-AREA.                                                          
014600*    03  FILLER -COPY WZ01REQU  -PRE VATUT-                               
014700*    03  FILLER -COPY WF2103I1  -PRE VATUT-                               
014800                                                                          
014900     EJECT                                                                
015000 01  IN-AREA-START               PIC X(24)   VALUE                        
015100                                             'IN-AREA-START'.             
015200*01  AREA -COPY W4758TU         -PRE IN-                                  
015300     EJECT                                                                
015400 01  IN1-AREA-START               PIC X(24)   VALUE                       
015500                                             'IN1-AREA-START'.            
015600*01  AREA -COPY W475896         -PRE IN1-                                 
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
017000     03  W-WDGXKEY-X.                                                     
017100         05  W-WDGXKEY           PIC X(30)   VALUE SPACE.                 
017200     03  W-KDSEGKEY-X.                                                    
017300         05  W-KDSEGKEY          PIC X(30)   VALUE SPACE.                 
017400     03  W-IDARTNR-X.                                                     
017500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017600     03  W-IDSKYLT-X.                                                     
017700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
017800     03  W-IDGMT-X.                                                       
017900         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
018000         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
018100     SKIP3                                                                
018200*    --- STATUS-KOD FRÅN IMS                                              
018300 01  STATUS-WS                   PIC XX.                                  
018400     88  SEGMENT-FINNS                       VALUE '  '.                  
018500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018800     88  IMS-EJ-OK                           VALUE 'XD'.                  
018900     SKIP2                                                                
019000 01  GODK-STATUSKODER.                                                    
019100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019200     SKIP3                                                                
019300 01  SSA1                        PIC X(64).                               
019400 01  SSA2                        PIC X(64).                               
019500     EJECT                                                                
019600*    --- IMS FUNKTIONSKODER                                               
019700*01  -COPY W0003                                                          
019800     EJECT                                                                
019900*    ---  DLI INPUT-OUTPUT AREA                                           
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
020100 01  DLI-IO-WDB201.                                                       
020200*    03 WDB201   -COPY WDB201                                             
020300     EJECT                                                                
020400                                                                          
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
020600 01  DLI-IO-WDD311.                                                       
020700*    03 WDD311   -COPY WDD311                                             
020800     EJECT                                                                
020900                                                                          
021000 LINKAGE SECTION.                                                         
021100                                                                          
021200*01  -COPY W0009  -PRE MSG-                                               
021300                                                                          
021400*01  -COPY W0009  -PRE RECEIVEI-                                          
021500                                                                          
021600*01  -COPY W0009  -PRE RECEIVEV-                                          
021700                                                                          
021800*01  -COPY W0008  -PRE WDG2-                                              
021900     05  FILLER                  PIC X.                                   
022000                                                                          
022100*01  -COPY W0008  -PRE WDB2-                                              
022200     05  FILLER                  PIC X.                                   
022300                                                                          
022400*01  -COPY W0008  -PRE WDD3-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700                                                                          
022800 PROCEDURE DIVISION  USING MSG-PCB                                        
022900                           RECEIVEI-PCB                                   
023000                           RECEIVEV-PCB                                   
023100                           WDG2-PCB                                       
023200                           WDB2-PCB                                       
023300                           WDD3-PCB.                                      
023400     ENTRY 'DLITCBL' USING MSG-PCB                                        
023500                           RECEIVEI-PCB                                   
023600                           RECEIVEV-PCB                                   
023700                           WDG2-PCB                                       
023800                           WDB2-PCB                                       
023900                           WDD3-PCB.                                      
024000                                                                          
024100     PERFORM A-INIT                                                       
024200     PERFORM B-HAEMTA-VALUTAKURSER                                        
024300     MOVE ZERO       TO WZ04-SEND-IDCOM                                   
024400     PERFORM S80-OPEN-INTRA                                               
024500     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
024600                                                                          
024700     PERFORM S01-LAES-W51044                                              
024800     PERFORM UNTIL END-OF-W51044                                          
024900       PERFORM C-BEHANDLA-FIL-TULLINFO                                    
025000       PERFORM S01-LAES-W51044                                            
025100     END-PERFORM                                                          
025200                                                                          
025300     IF WZ04-SEND-IDCOM > ZERO                                            
025400       PERFORM S82-CLOSE-INTRA                                            
025500     END-IF                                                               
025600                                                                          
025700                                                                          
025800     MOVE ZERO       TO WZ04-SEND-IDCOM                                   
025900     PERFORM S90-OPEN-VAT                                                 
026000     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
026100                                                                          
026200     PERFORM S02-LAES-W51045                                              
026300     PERFORM UNTIL END-OF-W51045                                          
026400       PERFORM D-BEHANDLA-MOMS                                            
026500       PERFORM S02-LAES-W51045                                            
026600     END-PERFORM                                                          
026700                                                                          
026800     IF WZ04-SEND-IDCOM > ZERO                                            
026900       PERFORM S92-CLOSE-VAT                                              
027000     END-IF                                                               
027100                                                                          
027200     PERFORM Z-FINIT                                                      
027300                                                                          
027400     MOVE ZERO TO RETURN-CODE                                             
027500     GOBACK                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 A-INIT SECTION.                                                          
027900                                                                          
028000     OPEN INPUT W51045                                                    
028100                W51044                                                    
028200                                                                          
028300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028400                                                                          
028500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
028600     MOVE D-AAR           TO DAGENS-DATUM-AAR                             
028700     MOVE D-MAANAD        TO DAGENS-DATUM-MAANAD                          
028800     MOVE D-DAG           TO DAGENS-DATUM-DAG                             
028900     .                                                                    
029000     EJECT                                                                
029100 B-HAEMTA-VALUTAKURSER SECTION.                                           
029200                                                                          
029210     MOVE DAGENS-DATUM-AAR    TO W-DATE-AAMM(1:2)                         
029220     MOVE DAGENS-DATUM-MAANAD TO W-DATE-AAMM(3:2)                         
029300     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
029310     MOVE WS-KDVALISO-HUV     TO CURR-KDVALISO-HUV                        
029320     MOVE 'M'                 TO CURR-KDVALTYP                            
029330                                                                          
029400     MOVE 'EUR'               TO CURR-KDVALISO-ROW                        
029600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
029700     IF CURR-KDSVAR = ' '                                                 
029800       MOVE CURR-PRKURS-NEW   TO WS-PRKURS-EUR                            
029900     END-IF                                                               
030000     DISPLAY ' PRKURS EUR ' WS-PRKURS-EUR                                 
030100                                                                          
030200     MOVE 'GBP'    TO CURR-KDVALISO-ROW                                   
030300     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
030400     IF CURR-KDSVAR = ' '                                                 
030500       MOVE CURR-PRKURS-NEW   TO WS-PRKURS-GBP                            
030600     END-IF                                                               
030700     DISPLAY ' PRKURS GBP ' WS-PRKURS-GBP                                 
030800     .                                                                    
030900     EJECT                                                                
031000 C-BEHANDLA-FIL-TULLINFO  SECTION.                                        
031100                                                                          
031200     MOVE IN-IDDISTR TO TEST-IDDISTR                                      
031300     IF IN-IDPTYP = 'GB1' OR 'GB2' OR 'ES1' OR 'ES2' OR                   
031400                    'IT1' OR 'IT2' OR 'AT1' OR 'AT2'                      
031500       PERFORM CA-NY-INTRASTAT                                            
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 CA-NY-INTRASTAT  SECTION.                                                
032000                                                                          
032100     MOVE 1                   TO INTUT-REQU-IDMSGVER                      
032200     MOVE SPACE               TO INTUT-REQU-KDPGMACT                      
032300     MOVE 'W5104800'          TO INTUT-REQU-IDUSER                        
032400                                                                          
032500     MOVE DAGENS-DATUM        TO WS-DATUM-AAMMDD                          
032600     MOVE WS-DATUM            TO INTUT-DAEXDAT                            
032700                                                                          
032800     MOVE 767676              TO INTUT-TIEXTID                            
032900     MOVE 'INT'               TO INTUT-IDPTYP                             
033000     IF IN-IDPTYP = 'GB1'                                                 
033100       MOVE 'SE'              TO INTUT-IDLANDX3-SEND                      
033200       MOVE 'GB'              TO INTUT-IDLANDX3-REC                       
033300       MOVE 'GBP'             TO INTUT-KDVALISO                           
033400       MOVE WS-PRKURS-GBP     TO INTUT-PRKURS                             
033410       MOVE 'GB669040720'     TO INTUT-IDVAT-BET                          
033500     ELSE                                                                 
033600     IF IN-IDPTYP = 'GB2'                                                 
033700       MOVE 'GB'              TO INTUT-IDLANDX3-SEND                      
033800       MOVE 'SE'              TO INTUT-IDLANDX3-REC                       
033900       MOVE 'GBP'             TO INTUT-KDVALISO                           
034000       MOVE WS-PRKURS-GBP     TO INTUT-PRKURS                             
034010       MOVE 'SE556074308901'  TO INTUT-IDVAT-BET                          
034100     ELSE                                                                 
034200     IF IN-IDPTYP = 'ES1'                                                 
034300       MOVE 'SE'              TO INTUT-IDLANDX3-SEND                      
034400       MOVE 'ES'              TO INTUT-IDLANDX3-REC                       
034500       MOVE 'EUR'             TO INTUT-KDVALISO                           
034600       MOVE WS-PRKURS-EUR     TO INTUT-PRKURS                             
034610       MOVE 'ESA0301409I'     TO INTUT-IDVAT-BET                          
034700     ELSE                                                                 
034800     IF IN-IDPTYP = 'ES2'                                                 
034900       MOVE 'ES'              TO INTUT-IDLANDX3-SEND                      
035000       MOVE 'SE'              TO INTUT-IDLANDX3-REC                       
035100       MOVE 'EUR'             TO INTUT-KDVALISO                           
035200       MOVE WS-PRKURS-EUR     TO INTUT-PRKURS                             
035210       MOVE 'SE556074308901'  TO INTUT-IDVAT-BET                          
035300     ELSE                                                                 
035400     IF IN-IDPTYP = 'IT1'                                                 
035500       MOVE 'SE'              TO INTUT-IDLANDX3-SEND                      
035600       MOVE 'IT'              TO INTUT-IDLANDX3-REC                       
035700       MOVE 'EUR'             TO INTUT-KDVALISO                           
035800       MOVE WS-PRKURS-EUR     TO INTUT-PRKURS                             
035810       MOVE 'IT04265320376'   TO INTUT-IDVAT-BET                          
035900     ELSE                                                                 
036000     IF IN-IDPTYP = 'IT2'                                                 
036100       MOVE 'IT'              TO INTUT-IDLANDX3-SEND                      
036200       MOVE 'SE'              TO INTUT-IDLANDX3-REC                       
036300       MOVE 'EUR'             TO INTUT-KDVALISO                           
036400       MOVE WS-PRKURS-EUR     TO INTUT-PRKURS                             
036410       MOVE 'SE556074308901'  TO INTUT-IDVAT-BET                          
036500     ELSE                                                                 
036600     IF IN-IDPTYP = 'AT1'                                                 
036700       MOVE 'SE'              TO INTUT-IDLANDX3-SEND                      
036800       MOVE 'AT'              TO INTUT-IDLANDX3-REC                       
036900       MOVE 'EUR'             TO INTUT-KDVALISO                           
037000       MOVE WS-PRKURS-EUR     TO INTUT-PRKURS                             
037010       MOVE 'ATU46478306'     TO INTUT-IDVAT-BET                          
037100     ELSE                                                                 
037200     IF IN-IDPTYP = 'AT2'                                                 
037300       MOVE 'AT'              TO INTUT-IDLANDX3-SEND                      
037400       MOVE 'SE'              TO INTUT-IDLANDX3-REC                       
037500       MOVE 'EUR'             TO INTUT-KDVALISO                           
037600       MOVE WS-PRKURS-EUR     TO INTUT-PRKURS                             
037610       MOVE 'SE556074308901'  TO INTUT-IDVAT-BET                          
037700     END-IF                                                               
037800     END-IF                                                               
037900     END-IF                                                               
038000     END-IF                                                               
038100     END-IF                                                               
038200     END-IF                                                               
038300     END-IF                                                               
038400     END-IF                                                               
038500                                                                          
038600     MOVE SPACE               TO INTUT-IDLANDX3-BET                       
038700                                                                          
038800     MOVE IN-IDDISTR          TO W-IDDISTR                                
038900     MOVE IN-IDKUNDNR         TO W-IDKUNDNR                               
039000     PERFORM IMS-GET-WDB201                                               
039100     IF SEGMENT-FINNS                                                     
039200       MOVE GMT-IDPARTNR      TO INTUT-IDPARTNR                           
039300     ELSE                                                                 
039400       MOVE 'NOT FOUND'       TO INTUT-IDPARTNR                           
039500     END-IF                                                               
039600                                                                          
039700     MOVE 'ECO'               TO INTUT-KDFINDOC                           
039800                                                                          
039900     MOVE IN-TIFAKT           TO WS-DATUM-AAMMDD                          
040000     MOVE WS-DATUM            TO INTUT-DAFINDOC                           
040100                                                                          
040200     MOVE IN-IDFAKT           TO INTUT-IDFINDOC                           
040300                                                                          
040400     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
040500     MOVE IN-IDDISTR          TO CIA-IDARTBET-IN                          
040600     CALL W009CIA USING          CIA-W009CIA                              
040700     MOVE CIA-IDARTBET-UT     TO INTUT-IDEXCUST-1                         
040800                                                                          
040900     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
041000     MOVE IN-IDKUNDNR         TO CIA-IDARTBET-IN                          
041100     CALL W009CIA USING          CIA-W009CIA                              
041200     MOVE CIA-IDARTBET-UT     TO INTUT-IDEXCUST-2                         
041300                                                                          
041400     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
041500     MOVE IN-IDARTNR          TO CIA-IDARTBET-IN                          
041600                                 W-IDARTNR                                
041700     CALL W009CIA USING          CIA-W009CIA                              
041800     MOVE CIA-IDARTBET-UT     TO INTUT-IDARTNR-FINANCE                    
041900                                                                          
042000     IF IN-IDLANDX2            = 'AT'                                     
042100       MOVE 'D '              TO W-IDSKYLT                                
042200     ELSE                                                                 
042300       IF IN-IDLANDX2          = 'GB' OR 'IE'                             
042400         MOVE 'GB'            TO W-IDSKYLT                                
042500       ELSE                                                               
042600         IF IN-IDLANDX2        = 'IT'                                     
042700           MOVE 'I '          TO W-IDSKYLT                                
042800         ELSE                                                             
042900           IF IN-IDLANDX2      = 'ES'                                     
043000             MOVE 'E'         TO W-IDSKYLT                                
043100           END-IF                                                         
043200         END-IF                                                           
043300       END-IF                                                             
043400     END-IF                                                               
043500     PERFORM IMS-GET-WDD311-BSEQ                                          
043600     IF SEGMENT-FINNS                                                     
043700       MOVE TEXT-BEART        TO INTUT-BEART                              
043800     ELSE                                                                 
043900       MOVE SPACE             TO INTUT-BEART                              
044000     END-IF                                                               
044100                                                                          
044200     MOVE IN-IDSTATNR         TO INTUT-IDSTATNR                           
044300     MOVE IN-VKLEV            TO INTUT-VKORDNTO                           
044300     MOVE ZERO                TO INTUT-VKORDNTO-3DEC                      
044400     MOVE IN-KVLEVART         TO INTUT-KVLEVART                           
044500     MOVE IN-KDARTURS         TO INTUT-KDARTURS                           
044600     MOVE ZERO                TO INTUT-KDFRAKT                            
044700     MOVE SPACE               TO INTUT-BELEVVIL                           
044800     MOVE IN-SUFAKT-LOC       TO INTUT-SUNTO                              
044900                                                                          
045000     PERFORM S81-PUT-INTRA                                                
045100     .                                                                    
045200     EJECT                                                                
045300 D-BEHANDLA-MOMS SECTION.                                                 
045400                                                                          
045500     MOVE IN1-IDDISTR TO TEST-IDDISTR                                     
045600     IF IN1-IDPTYP = 'GBR' OR 'ESP' OR 'ITA' OR 'AUT' OR 'HOL'            
045700       PERFORM DA-NY-VAT                                                  
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 DA-NY-VAT SECTION.                                                       
046200                                                                          
046300     MOVE 1                   TO VATUT-REQU-IDMSGVER                      
046400     MOVE SPACE               TO VATUT-REQU-KDPGMACT                      
046500     MOVE 'W5104800'          TO VATUT-REQU-IDUSER                        
046600                                                                          
046700     MOVE DAGENS-DATUM        TO WS-DATUM-AAMMDD                          
046800     MOVE WS-DATUM            TO VATUT-DAEXDAT                            
046900                                                                          
047000     MOVE 767676              TO VATUT-TIEXTID                            
047100     MOVE 'VAT'               TO VATUT-IDPTYP                             
047200                                                                          
047300     IF IN1-IDPTYP = 'GBR'                                                
047400       MOVE 'GB'              TO VATUT-IDLANDX3-SEND                      
047500       MOVE WS-PRKURS-GBP     TO VATUT-PRKURS                             
047600     ELSE                                                                 
047700     IF IN1-IDPTYP = 'ESP'                                                
047800       MOVE 'ES'              TO VATUT-IDLANDX3-SEND                      
047900       MOVE WS-PRKURS-EUR     TO VATUT-PRKURS                             
048000     ELSE                                                                 
048100     IF IN1-IDPTYP = 'ITA'                                                
048200       MOVE 'IT'              TO VATUT-IDLANDX3-SEND                      
048300       MOVE WS-PRKURS-EUR     TO VATUT-PRKURS                             
048400     ELSE                                                                 
048500     IF IN1-IDPTYP = 'AUT'                                                
048600       MOVE 'AT'              TO VATUT-IDLANDX3-SEND                      
048700       MOVE WS-PRKURS-EUR     TO VATUT-PRKURS                             
048800     ELSE                                                                 
048900     IF IN1-IDPTYP = 'HOL'                                                
049000       MOVE 'NL'              TO VATUT-IDLANDX3-SEND                      
049100       MOVE WS-PRKURS-EUR     TO VATUT-PRKURS                             
049200     END-IF                                                               
049300     END-IF                                                               
049400     END-IF                                                               
049500     END-IF                                                               
049600     END-IF                                                               
049700                                                                          
049800     MOVE SPACE               TO VATUT-IDLANDX3-BET                       
049900     MOVE IN1-KDVALISO        TO VATUT-KDVALISO                           
050000                                                                          
050100     MOVE IN1-IDDISTR         TO W-IDDISTR                                
050200     MOVE IN1-IDKUNDNR        TO W-IDKUNDNR                               
050300     PERFORM IMS-GET-WDB201                                               
050400     IF SEGMENT-FINNS                                                     
050500       MOVE GMT-IDPARTNR      TO VATUT-IDPARTNR                           
050600     ELSE                                                                 
050700       MOVE 'NOT FOUND'       TO VATUT-IDPARTNR                           
050800     END-IF                                                               
050900                                                                          
051000     MOVE 'ECO'               TO VATUT-KDFINDOC                           
051100                                                                          
051200     MOVE IN1-TIFAKT          TO WS-DATUM-AAMMDD                          
051300     MOVE WS-DATUM            TO VATUT-DAFINDOC                           
051400                                                                          
051500     MOVE IN1-IDFAKT           TO VATUT-IDFINDOC                          
051600                                                                          
051700     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
051800     MOVE IN1-IDDISTR         TO CIA-IDARTBET-IN                          
051900     CALL W009CIA USING          CIA-W009CIA                              
052000     MOVE CIA-IDARTBET-UT     TO VATUT-IDEXCUST-1                         
052100                                                                          
052200     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
052300     MOVE IN1-IDKUNDNR        TO CIA-IDARTBET-IN                          
052400     CALL W009CIA USING          CIA-W009CIA                              
052500     MOVE CIA-IDARTBET-UT     TO VATUT-IDEXCUST-2                         
052600                                                                          
052700     MOVE IN1-SUFKTBEL-VAT-LOC TO VATUT-SUNTO-TOT                         
052800     MOVE IN1-SUVAT-FAKT-LOC  TO VATUT-SUVAT-BILLIT-TOT                   
052900                                                                          
053000     MOVE SPACE               TO VATUT-IDVAT-LEG                          
053100                                 VATUT-IDVAT-RESP                         
053200                                 VATUT-IDVAT-BET                          
053300                                                                          
053400     PERFORM S91-PUT-VAT                                                  
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800 Z-FINIT SECTION.                                                         
053900                                                                          
054000     CLOSE W51044                                                         
054100           W51045                                                         
054200                                                                          
054300     MOVE 'S' TO POSTSUM-OPKOD                                            
054400     CALL POSTSUM USING POSTSUM-PARM                                      
054500     .                                                                    
054600     EJECT                                                                
054700 S01-LAES-W51044  SECTION.                                                
054800                                                                          
054900     READ W51044 INTO IN-AREA                                             
055000     AT END                                                               
055100        MOVE 'J' TO W51044-EOF-SW                                         
055200     NOT AT END                                                           
055300        MOVE 'W51044'   TO POSTSUM-FDNAMN                                 
055400        MOVE 'W51048D1' TO POSTSUM-DDNAMN2                                
055500        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
055600        CALL POSTSUM USING POSTSUM-PARM                                   
055700     END-READ                                                             
055800     .                                                                    
055900     EJECT                                                                
056000 S02-LAES-W51045  SECTION.                                                
056100                                                                          
056200     READ W51045 INTO IN1-AREA                                            
056300     AT END                                                               
056400        MOVE 'J' TO W51045-EOF-SW                                         
056500     NOT AT END                                                           
056600        MOVE 'W51045'   TO POSTSUM-FDNAMN                                 
056700        MOVE 'W51048D2' TO POSTSUM-DDNAMN2                                
056800        MOVE IN1-IDPTYP TO POSTSUM-TRANSTYP                               
056900        CALL POSTSUM USING POSTSUM-PARM                                   
057000     END-READ                                                             
057100     .                                                                    
057200     EJECT                                                                
057300 S80-OPEN-INTRA SECTION.                                                  
057400                                                                          
057500     MOVE WS-ADRESS-I                     TO SEND-ADDISPABS               
057600     MOVE 'OPEN'                          TO SEND-KDFUNC                  
057700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
057800                         SEND-OPEN-AREA                                   
057900     IF SEND-KDRC > ZERO                                                  
058000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
058100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
058200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
058300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058400     END-IF                                                               
058500     .                                                                    
058600                                                                          
058700 S81-PUT-INTRA SECTION.                                                   
058800                                                                          
058900     MOVE 'PUT'                           TO SEND-KDFUNC                  
059000     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
059100     MOVE LENGTH OF INTUT-AREA            TO SEND-KVDLEN                  
059200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
059300                         SEND-KVDLEN                                      
059400                         INTUT-AREA                                       
059500     IF SEND-KDRC > ZERO                                                  
059600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
059700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
059800       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
059900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060000     ELSE                                                                 
060100       MOVE 'W51044 '  TO POSTSUM-FDNAMN                                  
060200       MOVE 'SEND TUL' TO POSTSUM-DDNAMN2                                 
060300       MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                                
060400       CALL POSTSUM USING POSTSUM-PARM                                    
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800 S82-CLOSE-INTRA SECTION.                                                 
060900                                                                          
061000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
061100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
061200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
061300     .                                                                    
061400 S90-OPEN-VAT SECTION.                                                    
061500                                                                          
061600     MOVE WS-ADRESS-V                     TO SEND-ADDISPABS               
061700     MOVE 'OPEN'                          TO SEND-KDFUNC                  
061800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
061900                         SEND-OPEN-AREA                                   
062000     IF SEND-KDRC > ZERO                                                  
062100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
062200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
062300       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
062400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 S91-PUT-VAT SECTION.                                                     
062900                                                                          
063000     MOVE 'PUT'                           TO SEND-KDFUNC                  
063100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
063200     MOVE LENGTH OF VATUT-AREA            TO SEND-KVDLEN                  
063300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
063400                         SEND-KVDLEN                                      
063500                         VATUT-AREA                                       
063600     IF SEND-KDRC > ZERO                                                  
063700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
063800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
063900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
064000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
064100     ELSE                                                                 
064200       MOVE 'W51045 '  TO POSTSUM-FDNAMN                                  
064300       MOVE 'SEND MOM' TO POSTSUM-DDNAMN2                                 
064400       MOVE IN1-IDPTYP TO POSTSUM-TRANSTYP                                
064500       CALL POSTSUM USING POSTSUM-PARM                                    
064600     END-IF                                                               
064700     .                                                                    
064800                                                                          
064900 S92-CLOSE-VAT SECTION.                                                   
065000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
065100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
065200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
065300     .                                                                    
065400     EJECT                                                                
065500* --- IMS SEKTIONER ---                                                   
065600     SKIP3                                                                
065700 IMS-GET-WDB201 SECTION.                                                  
065800     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
065900          DELIMITED BY SIZE INTO SSA1                                     
066000     MOVE '  GE'           TO GODK-STATUSKODER                            
066100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
066200     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
066300     PERFORM IMS-STATUSKONTROLL                                           
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700 IMS-GET-WDD311-BSEQ SECTION.                                             
066800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
066900          DELIMITED BY SIZE INTO SSA1                                     
067000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
067100          DELIMITED BY SIZE INTO SSA2                                     
067200     MOVE '  GE'           TO GODK-STATUSKODER                            
067300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
067400     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
067500     PERFORM IMS-STATUSKONTROLL                                           
067600     .                                                                    
067700     EJECT                                                                
067800                                                                          
067900 IMS-STATUSKONTROLL SECTION.                                              
068000     SKIP2                                                                
068100     SET STATUS-IX TO 1                                                   
068200     SEARCH GODK-STATUS                                                   
068300       AT END                                                             
068400         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
068500         DISPLAY FELTEXT                                                  
068600         CALL FELLOG                                                      
068700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
068800         CONTINUE                                                         
068900     END-SEARCH                                                           
069000     .                                                                    
