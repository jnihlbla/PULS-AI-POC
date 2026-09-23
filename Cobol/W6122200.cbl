000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6122200.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   97/01/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        TÖMNING HTR 6305 LEVERANSNMÄRKNING NDC                           
001100*                                                                         
001200*        PROGRAMMET LÄSER HTR 6305                                        
001300*                   SKAPAR FIL W61222 LEV.ANM. CDC                        
001400*                              W61223 LEV.ANM. TRANSFER NDC               
001500*                              W61224 FÖR BORTTAG HTR 6305                
001600*                                                                         
001700*    ÄNDRINGAR:                                                           
001800*    2012-01  E'TRACKER: 10143271 CHINA WAREHOUSE PROJECT-1               
001900*                                                                         
001910*    2016-04  E'TRACKER: 10243132 CHINA EXPORT PROJECT 2015               
001911*                        SKAPA RAPPORT W41841-001 FÖR AVVIKELSER          
001912*                        PÅ REFILL TILL CDC FRÅN KINA.EJ LEV.ANM.         
001920*                                                                         
001930*    2018  -  JIRA PULS-976 GLOBAL EXPORT                                 
001940*                        SKAPA RAPPORT W41841-001 FÖR AVVIKELSER          
001950*                        PÅ REFILL TILL CDC FRÅN USA.EJ LEV.ANM.          
001960*                        SKAPA BÅDE RAPPORT W41841-001 OCH LEV.ANM        
001970*                        FÖR REFILL FRÅN KINA TO USA/USA TO KINA.         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- FIL TILL LEV-ANM CDC (RAPPORT W41841 REF. CN-CDC)          
002810*          --- (RAPPORT W41841 REF. USA-CDC, USA-CN, CN-USA)              
002820*          --- FIL TILL LEV.ANM USA-TO-KINA / KINA-TO-USA REFILL          
002830*          --- OBS! UT-FILEN HETER W61221 I PROCEDUREN.                   
002900     SELECT W61222                     ASSIGN TO W61222D1.                
003000     SKIP2                                                                
003100*          --- LISTFIL LEV.ANM NDC-NA                                     
003200     SELECT W61223                     ASSIGN TO W61222D2.                
003300     SKIP2                                                                
004300*          --- BORTTAG HTR 6305                                           
004400     SELECT W61224                     ASSIGN TO W61222D3.                
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP3                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W61222                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300*01  POST -COPY W418REFC -PRE  UT-  -L.                                   
005400                                                                          
005500 FD  W61223                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800*01  POST -COPY W61223  -PRE  NDC-NA- -L.                                 
005900                                                                          
007500 FD  W61224                                                               
007600     RECORDING       F                                                    
007700     BLOCK CONTAINS  0.                                                   
007800*01  POST -COPY W61224  -PRE  BORT-   -L.                                 
007900     EJECT                                                                
008000 WORKING-STORAGE SECTION.                                                 
008100     SKIP2                                                                
008200                                                                          
008300*    -- CHECKED BY WY2000                                                 
008400 77  IDPGM                       PIC X(8)    VALUE 'W6122200'.            
008500 77  JA                          PIC X       VALUE 'J'.                   
008600 77  NEJ                         PIC X       VALUE 'N'.                   
008700 77  WS-IDRADNR                  PIC S9(5)   VALUE ZERO COMP-3.           
008800 77  WS-KVLEVANM                 PIC 9(7)    VALUE ZERO.                  
009010                                                                          
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400                                                                          
009500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009600 01  FILLER REDEFINES DAGENS-DATUM.                                       
009700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010000     EJECT                                                                
010100*      --- VALID IDDC CODES                                               
010200*                                                                         
010300*01    -COPY WWDC99                                                       
010400       EJECT                                                              
010410*01    -COPY WWDC99 -PRE REC-                                             
010420       EJECT                                                              
010500*01    -COPY WWIDFTG                                                      
010600       EJECT                                                              
010700                                                                          
010800 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
010900*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
010901     EJECT                                                                
010910*01  FILLER  -COPY WWDIST35 -RED TEST-IDDISTR.                            
011000                                                                          
011100       EJECT                                                              
011200 01  DYNAMISKA-SUBPROGRAM.                                                
011300*                                                                         
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL POSTSUM                                          
011900*                                                                         
012000*01  -COPY W0005   -PRE  POSTSUM-                                         
012100     EJECT                                                                
012200 01  UT-AREA-START               PIC X(24)   VALUE                        
012300                                             'UT-AREA-START'.             
012400*01  AREA -COPY W418REFC    -PRE UT-                                      
012500     EJECT                                                                
012600 01  LIST-AREA-START             PIC X(24)   VALUE                        
012700                                             'LIST-AREA-START'.           
012800*01  AREA -COPY W61223      -PRE LIST-                                    
012900     EJECT                                                                
013000 01  BORT-AREA-START             PIC X(24)   VALUE                        
013100                                             'BORT-AREA-START'.           
013200*01  AREA -COPY W61224      -PRE BORT-                                    
013300     EJECT                                                                
013400 01  NYCKLAR-TILL-DLI.                                                    
013500     03  W-6305KEY-X.                                                     
013600         05  W-6305-IDHTYP       PIC X(4)     VALUE '6305'.               
013700         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
013800     03  W-FLKLAR-X.                                                      
013900         05  W-FLKLAR            PIC X        VALUE 'J'.                  
014000     03  W-IDFAKT-X.                                                      
014100         05  W-IDFAKT            PIC S9(7)    VALUE ZERO COMP-3.          
014200     SKIP2                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FINNS                       VALUE '  '.                  
014600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014800     88  IMS-EJ-OK                           VALUE 'XD'.                  
014900     SKIP2                                                                
015000 01  GODK-STATUSKODER.                                                    
015100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015200     SKIP3                                                                
015300 01  SSA1                        PIC X(64).                               
015400 01  SSA2                        PIC X(64).                               
015500     EJECT                                                                
015600*    --- IMS FUNKTIONSKODER                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000                                                                          
016510 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA-1'.                      
016520     SKIP2                                                                
016530 01  DLI-IO-AREA-1.                                                       
016540*    03  WL630511   -COPY WDGX6306                                        
016600     EJECT                                                                
016610 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA-2'.                      
016620     SKIP2                                                                
016630 01  DLI-IO-AREA-2.                                                       
016640*    03  WL630521   -COPY WDGX6308                                        
016650     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300                                                                          
017400*01  -COPY W0008  -PRE 6305-                                              
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017700*01  -COPY W0008  -PRE 6305-2-                                            
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING 6305-PCB 6305-2-PCB.                           
018100 MAIN SECTION.                                                            
018200     ENTRY 'DLITCBL' USING 6305-PCB 6305-2-PCB.                           
018300                                                                          
018400     PERFORM A-INIT                                                       
018500                                                                          
018600     PERFORM IMS-GET-WL630501                                             
018700     IF SEGMENT-FINNS                                                     
018800                                                                          
018900        PERFORM IMS-GET-WL630511                                          
019000        PERFORM UNTIL SEGMENT-SAKNAS                                      
019100           PERFORM B-SKAPA-UTPOST-FAKT                                    
019200           MOVE 6306-IDFAKT TO W-IDFAKT                                   
019300           PERFORM IMS-GU-WL630511                                        
019400                                                                          
019500           PERFORM IMS-GET-WL630521                                       
019600           PERFORM UNTIL SEGMENT-SAKNAS                                   
019700              PERFORM C-SKAPA-SKRIV-UTPOST                                
019800              PERFORM IMS-GET-WL630521                                    
019900           END-PERFORM                                                    
020000                                                                          
020100           PERFORM S17-SKRIV-W61224                                       
020200           PERFORM IMS-GET-WL630511                                       
020300        END-PERFORM                                                       
020400     END-IF                                                               
020500                                                                          
020600     PERFORM Z-FINIT                                                      
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300                                                                          
021400     OPEN OUTPUT W61222                                                   
021500                 W61223                                                   
021900                 W61224                                                   
022000                                                                          
022100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022200     MOVE ZERO  TO WS-IDRADNR                                             
022300                                                                          
022400     PERFORM S01-NOLLSTALL-UT                                             
022500     PERFORM S02-NOLLSTALL-LIST                                           
022600     .                                                                    
022700     EJECT                                                                
022800 B-SKAPA-UTPOST-FAKT SECTION.                                             
022900                                                                          
023000     MOVE 6306-IDFAKT       TO UT-IDFAKT                                  
023100                               UT-IDRAPPNR                                
023200                               LIST-IDFAKT                                
023300                               BORT-IDFAKT                                
023400     MOVE 6306-TIFAKT       TO UT-TIFAKT                                  
023500                               LIST-TIFAKT                                
023600     MOVE 6306-IDDC-SEND    TO LIST-IDDC-SEND                             
023700     MOVE 6306-IDDC-REC     TO LIST-IDDC-REC                              
023710                               UT-IDDC-REC                                
023800     .                                                                    
023900     EJECT                                                                
024000 C-SKAPA-SKRIV-UTPOST SECTION.                                            
024100                                                                          
024200     MOVE 6306-IDDC-SEND   TO WS-IDDC                                     
024300     MOVE 6306-IDDC-REC    TO REC-WS-IDDC                                 
024400                                                                          
024500     EVALUATE TRUE                                                        
024600     WHEN CDC-SE OR GOOD-DDC                                              
024700          PERFORM CA-FIXA-W61222                                          
024800          PERFORM S11-SKRIV-W61222                                        
024900     WHEN NDC-CN OR NDC-JP OR NDC-KR OR NDC-AU OR NDC-TH OR NDC-MY        
024910          PERFORM CA-FIXA-W61222                                          
024920          PERFORM S11-SKRIV-W61222                                        
025000     WHEN NDC-NA                                                          
025010          IF REC-NDC-NA                                                   
025100            PERFORM CB-FIXA-W61223                                        
025200            PERFORM S12-SKRIV-W61223                                      
025300          ELSE                                                            
025310            PERFORM CA-FIXA-W61222                                        
025320            PERFORM S11-SKRIV-W61222                                      
025500          END-IF                                                          
025600     WHEN OTHER                                                           
025700          CONTINUE                                                        
025800                                                                          
026200     END-EVALUATE                                                         
026300     .                                                                    
026400     EJECT                                                                
026500 CA-FIXA-W61222 SECTION.                                                  
026600                                                                          
026700     MOVE 'REF'             TO UT-IDPTYP                                  
026800     MOVE 6308-IDDISTR      TO UT-IDDISTR                                 
026900                               TEST-IDDISTR                               
027000*    MOVE 6308-IDKUNDNR     TO UT-IDKUNDNR ÄNDRAT 970417                  
027100     MOVE 999               TO UT-IDKUNDNR                                
027110                                                                          
027111*--- FÖR ATT VETA SÄNDANDE DC VID EXPORT CN/US..FAKT.1                    
027112*--- SOM SKALL VISAS PÅ RAPPORT W41841-001                                
027122     MOVE 6306-IDDC-LEV     TO UT-IDDC-LEV                                
027130                                                                          
027200     MOVE 6308-IDARTNR      TO UT-IDARTNR                                 
027300     MOVE 6308-IDKOLLI      TO UT-IDKOLLI                                 
027400     MOVE 6308-IDKUNDRF     TO UT-IDKUNDRF                                
027500     MOVE 6308-KDANMORS     TO UT-KDANMORS                                
027600     MOVE 6308-KDFRAKT      TO UT-KDFRAKT                                 
027700     MOVE 6308-KVLEVANM     TO WS-KVLEVANM                                
027800     MOVE WS-KVLEVANM       TO UT-KVLEVANM                                
027900                                                                          
028000*** ÄNDRAT 041004 PGA CENTRAL PRICING FÖR NA.                             
028100     IF DIST79-DEALER-PRICE                                               
028200       MOVE 6308-PRARTBTO-LOC   TO UT-PRARTBTO-LOC                        
028300       MOVE ZERO                TO UT-PRARTBTO                            
028400       MOVE 6308-KDVALISO       TO UT-KDVALISO                            
028500     ELSE                                                                 
028501*** GLOBAL EXPORT.DISTR 9111/9211 I PRAVCOST LOC-VALUTA                   
028502*** STUDS-DISTRIKT FAKT. 2 I SEK. RÄTT FRÅN WDR5                          
028510       IF DIST35-NONVCC-REFILL OR                                         
028510          DIST35-NONVCC-NONVCC-TRANSFER OR                                
028510          DIST35-NONVCC-VCC-TRANSFER                                      
028511         MOVE 6308-PRARTBTO   TO UT-PRARTBTO                              
028512         MOVE ZERO            TO UT-PRARTBTO-LOC                          
028513         MOVE 6308-KDVALISO   TO UT-KDVALISO                              
028560       ELSE                                                               
028600         MOVE 6308-PRARTBTO   TO UT-PRARTBTO                              
028700         MOVE ZERO            TO UT-PRARTBTO-LOC                          
028800         MOVE 'SEK'           TO UT-KDVALISO                              
028900       END-IF                                                             
028910     END-IF                                                               
029000                                                                          
029100     MOVE 6308-TILEVANM     TO UT-TILEVANM                                
029200     IF 6308-KDANMORS = '43'                                              
029300        MOVE 2              TO UT-KDEMBLEV                                
029400     ELSE                                                                 
029500        MOVE ZERO           TO UT-KDEMBLEV                                
029600     END-IF                                                               
029700     MOVE 'J'               TO UT-FLAUTKRE                                
029800     MOVE 'R'               TO UT-KDFAKTYP                                
029900     MOVE WC-IDFTG-PV       TO UT-IDFTG                                   
030000     MOVE WS-IDDC           TO UT-IDDC-SEND                               
030200     IF GOOD-DDC                                                          
030300        MOVE 'J'            TO UT-FLDIRLEV                                
030400     ELSE                                                                 
030500        MOVE 'N'            TO UT-FLDIRLEV                                
030600     END-IF                                                               
030700     ADD +1                 TO UT-IDRADNR                                 
030800     MOVE ZERO              TO UT-IDLOPNRM                                
030900     .                                                                    
031000     EJECT                                                                
031100 CB-FIXA-W61223 SECTION.                                                  
031200                                                                          
031300     MOVE 6308-IDDISTR      TO LIST-IDDISTR                               
031400     MOVE 6308-IDKUNDNR     TO LIST-IDKUNDNR                              
031500     MOVE 6308-IDKUNDRF-GRP TO LIST-IDKUNDRF-GRP                          
031600     MOVE 6308-IDKOLLI      TO LIST-IDKOLLI                               
031700     MOVE 6308-IDARTNR      TO LIST-IDARTNR                               
031800     MOVE 6308-KVLEVANM     TO WS-KVLEVANM                                
031900     MOVE WS-KVLEVANM       TO LIST-KVLEVANM                              
032000     MOVE 6308-KDANMORS     TO LIST-KDANMORS                              
032100     .                                                                    
032200     EJECT                                                                
032300 Z-FINIT SECTION.                                                         
032400                                                                          
032500     CLOSE W61222                                                         
032600           W61223                                                         
033000           W61224                                                         
033100                                                                          
033200     MOVE 'S' TO POSTSUM-OPKOD                                            
033300     CALL POSTSUM USING POSTSUM-PARM                                      
033400     .                                                                    
033500     EJECT                                                                
033600 S01-NOLLSTALL-UT SECTION.                                                
033700                                                                          
033800     MOVE SPACE     TO UT-AREA                                            
033900     MOVE ZERO      TO UT-IDDISTR                                         
034000                       UT-IDKUNDNR                                        
034100                       UT-IDRAPPNR                                        
034200                       UT-IDARTNR                                         
034300                       UT-IDRADNR                                         
034500                       UT-IDFAKT                                          
034510                       UT-IDFTG                                           
034600                       UT-IDKOLLI                                         
034610                       UT-IDLOPNRM                                        
034700                       UT-KDEMBLEV                                        
034800                       UT-KDFRAKT                                         
034900                       UT-KVLEVANM                                        
035000                       UT-PRARTBTO                                        
035100                       UT-TIFAKT                                          
035200                       UT-TILEVANM                                        
035210                       UT-PRARTBTO-LOC                                    
035300     .                                                                    
035400     EJECT                                                                
035500 S02-NOLLSTALL-LIST SECTION.                                              
035600                                                                          
035700     MOVE SPACE     TO LIST-AREA                                          
035800     MOVE ZERO      TO LIST-IDDISTR                                       
035900                       LIST-IDKUNDNR                                      
036000                       LIST-IDFAKT                                        
036100                       LIST-IDARTNR                                       
036200                       LIST-IDKOLLI                                       
036300                       LIST-KVLEVANM                                      
036400                       LIST-TIFAKT                                        
036500     .                                                                    
036600     EJECT                                                                
036700 S11-SKRIV-W61222 SECTION.                                                
036800                                                                          
036900     WRITE UT-POST FROM UT-AREA                                           
037000                                                                          
037100     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
037200     MOVE 'W61222 '  TO POSTSUM-FDNAMN                                    
037300     MOVE 'W61222D1' TO POSTSUM-DDNAMN2                                   
037400     CALL POSTSUM USING POSTSUM-PARM                                      
037500     .                                                                    
037600     SKIP3                                                                
037700 S12-SKRIV-W61223 SECTION.                                                
037800                                                                          
037900     WRITE NDC-NA-POST FROM LIST-AREA                                     
038000                                                                          
038100     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
038200     MOVE 'W61223 '   TO POSTSUM-FDNAMN                                   
038300     MOVE 'W61222D2'  TO POSTSUM-DDNAMN2                                  
038400     CALL POSTSUM USING POSTSUM-PARM                                      
038500     .                                                                    
038600     EJECT                                                                
041700 S17-SKRIV-W61224 SECTION.                                                
041800                                                                          
041900     WRITE BORT-POST FROM BORT-AREA                                       
042000                                                                          
042100     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
042200     MOVE 'W61224 '   TO POSTSUM-FDNAMN                                   
042300     MOVE 'W61222D7'  TO POSTSUM-DDNAMN2                                  
042400     CALL POSTSUM USING POSTSUM-PARM                                      
042500     .                                                                    
042600     EJECT                                                                
042700* --- IMS SEKTIONER ---                                                   
042800                                                                          
042900 IMS-GET-WL630501 SECTION.                                                
043000                                                                          
043100     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
043200          DELIMITED BY SIZE INTO SSA1                                     
043300     MOVE '  GE' TO GODK-STATUSKODER                                      
043400     CALL CBLTDLI USING GU 6305-PCB DLI-IO-AREA-1 SSA1                    
043500     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     SKIP3                                                                
043900 IMS-GET-WL630511 SECTION.                                                
044000                                                                          
044100     STRING 'WL630511(FLKLAR   =' W-FLKLAR-X ')'                          
044200          DELIMITED BY SIZE INTO SSA1                                     
044300     MOVE '  GE' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GNP 6305-PCB DLI-IO-AREA-1 SSA1                   
044500     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     .                                                                    
044800     EJECT                                                                
044900 IMS-GU-WL630511 SECTION.                                                 
045000                                                                          
045100     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
045200          DELIMITED BY SIZE INTO SSA1                                     
045300     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X ')'                          
045400          DELIMITED BY SIZE INTO SSA2                                     
045500     MOVE '  GE' TO GODK-STATUSKODER                                      
045600     CALL CBLTDLI USING GU 6305-2-PCB DLI-IO-AREA-1 SSA1 SSA2             
045700     MOVE 6305-2-STATUS-CODE TO STATUS-WS                                 
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     .                                                                    
046000     SKIP3                                                                
046100 IMS-GET-WL630521 SECTION.                                                
046200                                                                          
046300     MOVE 'WL630521 ' TO SSA1                                             
046400     MOVE '  GE' TO GODK-STATUSKODER                                      
046500     CALL CBLTDLI USING GNP 6305-2-PCB DLI-IO-AREA-2 SSA1                 
046600     MOVE 6305-2-STATUS-CODE TO STATUS-WS                                 
046700     PERFORM IMS-STATUSKONTROLL                                           
046800     .                                                                    
046900     EJECT                                                                
047000 IMS-STATUSKONTROLL SECTION.                                              
047100     SKIP2                                                                
047200     SET STATUS-IX TO 1                                                   
047300     SEARCH GODK-STATUS                                                   
047400       AT END                                                             
047500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047600           DELIMITED BY SIZE INTO FELTEXT                                 
047700         DISPLAY FELTEXT                                                  
047800         CALL FELLOG                                                      
047900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
048000         CONTINUE                                                         
048100     END-SEARCH                                                           
048200     .                                                                    
