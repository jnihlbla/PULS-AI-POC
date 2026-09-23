000100                                                                          
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W4633500.                                                
000500 AUTHOR.         BO SVENSSON.                                             
000600 DATE-WRITTEN.   98/04/17.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        TAR EMOT FAKTURA- ELLER ORDERPOSTER FRÅN PIE SOM                 
001400*        KOMMER FRÅN VCOM-ÖVERFÖRING.                                     
001500*                                                                         
001600*        SKAPAR POSTER På W46335-FIL SOM SKALL TILL AUTOMATISK            
001700*        PACKNING.                                                        
001800*        ELLER                                                            
001900*        SKAPAR POSTER På W46333-FIL SOM SKALL TILL ORDER.                
002000*        ELLER                                                            
002100*        SKAPAR POSTER PÅ W46337-FIL SOM SKAPAR LISTA.                    
002200*                                                                         
002300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     SKIP2                                                                
003700*          --- FIL FRÅN PIE                                               
003800     SELECT W46334                     ASSIGN TO W46335D1.                
003900     SKIP2                                                                
004000*          --- PIE-FIL TILL AUTOMATPACKNING                               
004100     SELECT W46335                     ASSIGN TO W46335D2.                
004200     SKIP2                                                                
004300*          --- PIE-FIL TILL ORDER                                         
004400     SELECT W46333                     ASSIGN TO W46335D3.                
004500     SKIP2                                                                
004600*          --- PIE-FIL TILL LISTA                                         
004700     SELECT W46337                     ASSIGN TO W46335D4.                
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300     SKIP3                                                                
005400 FILE SECTION.                                                            
005500     SKIP3                                                                
005600 FD  W46334                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*    -COPY W463VG4       -L.                                              
006100                                                                          
006200     SKIP3                                                                
006300 FD  W46335                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700*01  POST -COPY W46335 -PRE  UTF-  -L.                                    
006800     SKIP3                                                                
006900 FD  W46333                                                               
007000     RECORDING       V                                                    
007100     BLOCK CONTAINS  0.                                                   
007200                                                                          
007300*01  POST -COPY W46333  -PRE  UTO-  -L.                                   
007400     SKIP3                                                                
007500 FD  W46337                                                               
007600     RECORDING       F                                                    
007700     BLOCK CONTAINS  0.                                                   
007800                                                                          
007900*01  POST -COPY W46337 -PRE  LIST-  -L.                                   
008500     EJECT                                                                
008600 WORKING-STORAGE SECTION.                                                 
008700                                                                          
008800                                                                          
008900*    -- CHECKED BY WY2000                                                 
009000 77  IDPGM                       PIC X(8)    VALUE 'W4633500'.            
009100 77  JA                          PIC X       VALUE 'J'.                   
009200 77  NEJ                         PIC X       VALUE 'N'.                   
009300 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
009600     SKIP2                                                                
009700                                                                          
009800 77  W46334-EOF-SW               PIC X       VALUE 'N'.                   
009900     88  END-OF-W46334                       VALUE 'J'.                   
010000     EJECT                                                                
010100                                                                          
010200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010300 01  FILLER REDEFINES DAGENS-DATUM.                                       
010400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010700                                                                          
010800 01  WS-DADAT-X.                                                          
010900     03  WS-SEKEL                PIC 9(2).                                
011000     03  WS-DAT                  PIC 9(6).                                
011100 01  WS-DADAT-N REDEFINES WS-DADAT-X                                      
011200                                 PIC 9(8).                                
011300                                                                          
011400 01  TEST-IDDISTR                PIC 9(5) COMP-3.                         
011500*01  FILLER -COPY WWDIST47 -RED TEST-IDDISTR.                             
011600     EJECT                                                                
011700 01  DYNAMISKA-SUBPROGRAM.                                                
011800*                                                                         
011900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012300     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
012400     SKIP2                                                                
012500*    --- PARAMETRAR TILL ABEND                                            
012600                                                                          
012700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013000     SKIP2                                                                
013100 01  FELTEXT.                                                             
013200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013400     EJECT                                                                
013500 01  FILLER                     PIC X(16)   VALUE 'CIA-AREA'.             
013600*                                                                         
013700*   -COPY W009CIA                                                         
013800*                                                                         
013900     EJECT                                                                
014000 01  FILLER                     PIC X(10)   VALUE 'WDATAREA'.             
014100*01 -COPY WDATAREA                                                        
014200     EJECT                                                                
014300 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
014400*01 -COPY WDECAREA                                                        
014500     EJECT                                                                
014600*    --- PARAMETRAR TILL POSTSUM                                          
014700*                                                                         
014800*01  -COPY W0005   -PRE  POSTSUM-                                         
014900     EJECT                                                                
015000 01  IN-AREA-START               PIC X(24)   VALUE                        
015100                                 'IN-AREA-START  '.                       
015200     SKIP2                                                                
015300*01  AREA -COPY W463VG4   -PRE IN-                                        
015400     EJECT                                                                
015500 01  UTF-AREA-START               PIC X(24)   VALUE                       
015600                                 'UTF-AREA-START '.                       
015700     SKIP2                                                                
015800                                                                          
015900*01  AREA -COPY W46335     -PRE UTF-                                      
016000     EJECT                                                                
016100 01  UTO-AREA-START               PIC X(24)   VALUE                       
016200                                 'UTO-AREA-START '.                       
016300     SKIP2                                                                
016400                                                                          
016500*01  AREA -COPY W46333     -PRE UTO-                                      
016600     SKIP3                                                                
016700                                                                          
016800 01  LIST-AREA-START              PIC X(24)   VALUE                       
016900                                 'LIST-AREA-START '.                      
017000     SKIP2                                                                
017100                                                                          
017200*01  AREA -COPY W46337     -PRE LIST-                                     
017300                                                                          
019100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019200*                                                                         
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019500     SKIP3                                                                
019600 01  NYCKLAR-TILL-DLI.                                                    
019700     03  W-IDARTNR-X.                                                     
019800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019900                                                                          
020000     03  W-IDSKYLT-X.                                                     
020100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
020200                                                                          
020300     SKIP2                                                                
020400*    --- STATUS-KOD FRÅN IMS                                              
020500 01  STATUS-WS                   PIC XX.                                  
020600     88  SEGMENT-FINNS                       VALUE '  '.                  
020700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020900     SKIP2                                                                
021000 01  GODK-STATUSKODER.                                                    
021100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021200     SKIP3                                                                
021300 01  SSA1                        PIC X(64).                               
021400 01  SSA2                        PIC X(64).                               
021500     EJECT                                                                
021600*    --- IMS FUNKTIONSKODER                                               
021700*01  -COPY W0003                                                          
021800     EJECT                                                                
021900*    ---  DLI INPUT-OUTPUT AREA                                           
022000                                                                          
022100 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WLBENA11'.                   
022200 01  DLI-IO-BENA11.                                                       
022300*    03  -COPY WDD311                                                     
022400                                                                          
022500     EJECT                                                                
022600 LINKAGE SECTION.                                                         
022700                                                                          
022800*01  -COPY W0008  -PRE BENA-                                              
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100     EJECT                                                                
023200 PROCEDURE DIVISION  USING BENA-PCB.                                      
023300 MAIN SECTION.                                                            
023400     ENTRY 'DLITCBL' USING BENA-PCB.                                      
023500     SKIP2                                                                
023600                                                                          
023700     PERFORM A-INIT                                                       
023800     PERFORM S01-LAES-W46334                                              
023900     PERFORM UNTIL END-OF-W46334                                          
024100       EVALUATE IN-KDSOFT                                                 
024200         WHEN '1'                                                         
024300           PERFORM B-FAKTURATRANS                                         
024400         WHEN '2'                                                         
024500           PERFORM C-ORDERTRANS                                           
024600       END-EVALUATE                                                       
024700       PERFORM S01-LAES-W46334                                            
024800     END-PERFORM                                                          
026400                                                                          
026500     PERFORM Z-FINIT                                                      
026600                                                                          
026700     MOVE ZERO TO RETURN-CODE                                             
026800     GOBACK                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 A-INIT SECTION.                                                          
027200                                                                          
027300     OPEN INPUT  W46334                                                   
027400                                                                          
027500     OPEN OUTPUT W46335 W46333 W46337                                     
027600     SKIP2                                                                
027700     ACCEPT DAGENS-DATUM  FROM DATE                                       
027800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027900     MOVE SPACE TO UTF-AREA                                               
028000                   UTO-AREA                                               
028100                   LIST-AREA                                              
028200     .                                                                    
028300     EJECT                                                                
028400 B-FAKTURATRANS SECTION.                                                  
028500                                                                          
028600     MOVE SPACE           TO UTF-AREA                                     
028700                                                                          
028800     MOVE IN-IDPRODNR TO UTF-IDPRODNR                                     
028900                         UTO-IDPRODNR                                     
029000     MOVE IN-IDDISTR  TO UTF-IDDISTR                                      
029100                         UTO-IDDISTR                                      
029200     MOVE IN-IDKUNDNR TO UTF-IDKUNDNR                                     
029300                         UTO-IDKUNDNR                                     
029400     MOVE IN-IDORDNR7 TO UTF-IDORDNR7                                     
029500                         UTO-IDORDNR7                                     
029600     MOVE IN-IDRADNR  TO UTF-IDRADNR                                      
029700                         UTO-IDRADNR                                      
029800     MOVE IN-KVLEVART TO UTF-KVLEVART                                     
029900                         UTO-KVLEVART                                     
030000     MOVE IN-IDARTPRE TO UTF-IDARTPRE                                     
030100                         UTO-IDARTPRE                                     
030200     MOVE IN-IDARTBET TO UTF-IDARTBET                                     
030300                         UTO-IDARTBET                                     
030400     MOVE IN-IDKLIENT TO UTF-IDKLIENT                                     
030500                         UTO-IDKLIENT                                     
030600     MOVE IN-IDARBREF TO UTF-IDARBREF                                     
030700                         UTO-IDARBREF                                     
030800     MOVE IN-IDBIL    TO UTF-IDBIL                                        
030900                         UTO-IDBIL                                        
031000     MOVE IN-IDVIN    TO UTF-IDVIN                                        
031100                         UTO-IDVIN                                        
031200                                                                          
031300     PERFORM S11-SKRIV-W46335                                             
031400                                                                          
031500     MOVE IN-IDPTYP   TO UTO-IDPTYP                                       
031600     MOVE IN-KDSOFT   TO UTO-KDSOFT                                       
031700     MOVE IN-IDPIERAD TO UTO-IDPIERAD                                     
031800     MOVE NEJ         TO UTO-FLSERV                                       
031900                                                                          
032000     PERFORM S12-SKRIV-W46333                                             
032100     .                                                                    
032200 C-ORDERTRANS SECTION.                                                    
032300                                                                          
032400*SOFT DIST 0 + KUND 0 HÄR TILLS PIE SPÄRRAR DEM (INTERN PIE D+K)          
032500                                                                          
032600     MOVE IN-IDARTPRE       TO CIA-IDARTPRE-IN                            
032700     MOVE IN-IDARTBET       TO CIA-IDARTBET-IN                            
032800     CALL W009CIA USING CIA-W009CIA                                       
032900     IF CIA-KDSVAR = 'F'                                                  
033000        MOVE 'RAD FEL' TO FELTEXT-STR                                     
033100        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
033200     ELSE                                                                 
033300        MOVE CIA-IDARTNR        TO WS-IDARTNR                             
033400        MOVE CIA-IDARTN8        TO LIST-IDARTNR                           
033500                                                                          
033600     END-IF                                                               
033700                                                                          
033800     MOVE IN-IDDISTR      TO TEST-IDDISTR                                 
033900     IF DIST47-INTERNA OR                                                 
034000       (IN-IDDISTR = ZERO AND IN-IDKUNDNR = ZERO)                         
034100                                                                          
034200       MOVE SPACE         TO LIST-AREA                                    
034300                                                                          
034400       MOVE WS-IDARTNR       TO W-IDARTNR                                 
034500                                LIST-IDARTNR                              
034600       MOVE 'S  '            TO W-IDSKYLT                                 
034700       PERFORM IMS-GU-BENA-BENA11                                         
034800       IF SEGMENT-FINNS                                                   
034900         MOVE TEXT-BEART  TO LIST-BEART                                   
035000       END-IF                                                             
035100                                                                          
035200       MOVE IN-IDDISTR    TO LIST-IDDISTR                                 
035300       MOVE IN-IDKUNDNR   TO LIST-IDKUNDNR                                
035400       MOVE IN-KVLEVART   TO LIST-KVLEVART                                
035500       MOVE IN-IDKLIENT   TO LIST-IDKLIENT                                
035600                                                                          
035700       PERFORM S13-SKRIV-W46337                                           
035800                                                                          
035900       IF DIST47-INTERNA                                                  
036000*      HÄR SKRIVS 33-FILEN BARA FÖR ATT SKAPA ETT KVITTO                  
036100*      FÖR INTERNORDER                                                    
036200*      DISTRIKT OCH KUND BEHÖVS BARA FÖR SORTERING I W412P003             
036300                                                                          
036400          MOVE SPACE       TO UTO-AREA                                    
036500          MOVE IN-IDPTYP   TO UTO-IDPTYP                                  
036600          MOVE 'I'         TO UTO-KDSOFT                                  
036700          MOVE IN-IDDISTR  TO UTO-IDDISTR                                 
036710          MOVE IN-IDKUNDNR TO UTO-IDKUNDNR                                
036711          MOVE IN-IDORDNR7 TO UTO-IDORDNR7                                
036712          MOVE IN-IDPRODNR TO UTO-IDPRODNR                                
036713          MOVE IN-KVLEVART TO UTO-KVLEVART                                
036714          MOVE IN-IDRADNR  TO UTO-IDRADNR                                 
036720          MOVE IN-IDPIERAD TO UTO-IDPIERAD                                
036800                                                                          
036900          PERFORM S12-SKRIV-W46333                                        
037000       END-IF                                                             
037300     ELSE                                                                 
037400       MOVE SPACE         TO UTO-AREA                                     
037500                                                                          
037600       MOVE IN-IDPTYP     TO UTO-IDPTYP                                   
037700       MOVE IN-KDSOFT     TO UTO-KDSOFT                                   
037800       MOVE IN-IDDISTR    TO UTO-IDDISTR                                  
037900       MOVE IN-IDKUNDNR   TO UTO-IDKUNDNR                                 
038000       MOVE IN-IDORDNR7   TO UTO-IDORDNR7                                 
038100       MOVE IN-IDPRODNR   TO UTO-IDPRODNR                                 
038200       MOVE IN-IDARBREF   TO UTO-IDARBREF                                 
038300       MOVE IN-IDRADNR    TO UTO-IDRADNR                                  
038400       MOVE IN-IDARTPRE   TO UTO-IDARTPRE                                 
038500       MOVE IN-IDARTBET   TO UTO-IDARTBET                                 
038600       MOVE IN-KVLEVART   TO UTO-KVLEVART                                 
038700       MOVE IN-IDKLIENT   TO UTO-IDKLIENT                                 
038800       MOVE IN-IDBIL      TO UTO-IDBIL                                    
038900       MOVE IN-IDVIN      TO UTO-IDVIN                                    
039000       MOVE IN-IDPIERAD   TO UTO-IDPIERAD                                 
039100       MOVE NEJ           TO UTO-FLSERV                                   
039200                                                                          
039300       PERFORM S12-SKRIV-W46333                                           
039400     END-IF                                                               
039500     .                                                                    
039600                                                                          
039700     EJECT                                                                
039800 Z-FINIT SECTION.                                                         
039900     CLOSE W46334                                                         
040000           W46335                                                         
040100           W46333                                                         
040200           W46337                                                         
040400     SKIP2                                                                
040500     MOVE 'S' TO POSTSUM-OPKOD                                            
040600     CALL POSTSUM USING POSTSUM-PARM                                      
040700     .                                                                    
040800     EJECT                                                                
040900 S01-LAES-W46334  SECTION.                                                
041000                                                                          
041100     READ W46334 INTO IN-AREA                                             
041200     AT END                                                               
041300        MOVE HIGH-VALUE   TO IN-AREA                                      
041400        SET END-OF-W46334 TO TRUE                                         
041500                                                                          
041600     NOT AT END                                                           
041700        MOVE 'W46334'   TO POSTSUM-FDNAMN                                 
041800        MOVE 'W46335D1' TO POSTSUM-DDNAMN2                                
041900        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
042000        CALL POSTSUM USING POSTSUM-PARM                                   
042100     END-READ                                                             
042200     .                                                                    
042300     EJECT                                                                
042400 S11-SKRIV-W46335 SECTION.                                                
042500                                                                          
042600     WRITE UTF-POST FROM UTF-AREA                                         
042700                                                                          
042800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
042900     MOVE 'W46335'   TO POSTSUM-FDNAMN                                    
043000     MOVE 'W46335D2' TO POSTSUM-DDNAMN2                                   
043100     CALL POSTSUM USING POSTSUM-PARM                                      
043200     .                                                                    
043300     EJECT                                                                
043400 S12-SKRIV-W46333 SECTION.                                                
043500                                                                          
043600     WRITE UTO-POST FROM UTO-AREA                                         
043700                                                                          
043800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
043900     MOVE 'W46333'   TO POSTSUM-FDNAMN                                    
044000     MOVE 'W46335D3' TO POSTSUM-DDNAMN2                                   
044100     CALL POSTSUM USING POSTSUM-PARM                                      
044200     .                                                                    
044300     EJECT                                                                
044400 S13-SKRIV-W46337 SECTION.                                                
044500                                                                          
044600     WRITE LIST-POST FROM LIST-AREA                                       
044700                                                                          
044800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
044900     MOVE 'W46337'   TO POSTSUM-FDNAMN                                    
045000     MOVE 'W46335D4' TO POSTSUM-DDNAMN2                                   
045100     CALL POSTSUM USING POSTSUM-PARM                                      
045200     .                                                                    
045300     EJECT                                                                
045400* --- IMS SEKTIONER ---                                                   
045500     SKIP3                                                                
045600     EJECT                                                                
045700 IMS-GU-BENA-BENA11 SECTION.                                              
045800                                                                          
045900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
046000          DELIMITED BY SIZE INTO SSA1                                     
046100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
046200          DELIMITED BY SIZE INTO SSA2                                     
046300     MOVE '  GE' TO GODK-STATUSKODER                                      
046400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
046500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
046600     PERFORM IMS-STATUSKONTROLL                                           
046700     .                                                                    
046800     EJECT                                                                
046900 IMS-STATUSKONTROLL SECTION.                                              
047000                                                                          
047100     SET STATUS-IX TO 1                                                   
047200     SEARCH GODK-STATUS                                                   
047300       AT END                                                             
047400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047500           DELIMITED BY SIZE INTO FELTEXT                                 
047600         DISPLAY FELTEXT                                                  
047700         CALL FELLOG                                                      
047800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047900         CONTINUE                                                         
048000     END-SEARCH                                                           
049000     .                                                                    
