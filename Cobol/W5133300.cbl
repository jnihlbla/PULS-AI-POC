000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5133300.                                    
000400*AUTHOR.                     KARL JOHAN HANSSON.                          
000500*DATE-WRITTEN.               MAJ  1999.                                   
000600*REMARKS.                                                                 
000700*      PROGRAMMET SKAPAR LISTOR TILL SDC EUROPA                           
000800*      INNEHÅLLANDE JUSTERINGSRAPPORT PER ANSKAFFARE.                     
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
001500     SELECT  W51333          ASSIGN UT-S-W51333D1.                        
001600                                                                          
001700* - - - - - - - - - - - - - - - - - - - - - - - - LISTOR.                 
001800     SELECT  LISTA9-21       ASSIGN UT-S-W51333D2.                        
001900     SELECT  LISTA9-23       ASSIGN UT-S-W51333D3.                        
002000     SELECT  LISTA9-24       ASSIGN UT-S-W51333D4.                        
002100     SELECT  LISTA9-25       ASSIGN UT-S-W51333D5.                        
002200     SELECT  LISTA9-26       ASSIGN UT-S-W51333D6.                        
002300     SELECT  LISTA9-91       ASSIGN UT-S-W51333DB.                        
002400* - - - - - - - - - - - - - - - - - - - - - - - - UTFILER                 
002500     SELECT  UTFIL           ASSIGN UT-S-W51333DA.                        
002600* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
002700     SELECT  SORTER          ASSIGN UT-S-W51333DS.                        
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W51333                                                               
003300     RECORDING F                                                          
003400     BLOCK 0 RECORDS.                                                     
003500                                                                          
003600*01  IN-POSTER -COPY W51335    -L.                                        
003700     EJECT                                                                
003800 FD  LISTA9-21                                                            
003900     RECORDING F                                                          
004000     BLOCK 0 RECORDS.                                                     
004100                                                                          
004200 01  LISTA-9-21          PIC X(110).                                      
004300     EJECT                                                                
004400 FD  LISTA9-23                                                            
004500     RECORDING F                                                          
004600     BLOCK 0 RECORDS.                                                     
004700                                                                          
004800 01  LISTA-9-23          PIC X(110).                                      
004900     EJECT                                                                
005000 FD  LISTA9-24                                                            
005100     RECORDING F                                                          
005200     BLOCK 0 RECORDS.                                                     
005300                                                                          
005400 01  LISTA-9-24          PIC X(110).                                      
005500     EJECT                                                                
005600 FD  LISTA9-25                                                            
005700     RECORDING F                                                          
005800     BLOCK 0 RECORDS.                                                     
005900                                                                          
006000 01  LISTA-9-25          PIC X(110).                                      
006100     EJECT                                                                
006200 FD  LISTA9-26                                                            
006300     RECORDING F                                                          
006400     BLOCK 0 RECORDS.                                                     
006500                                                                          
006600 01  LISTA-9-26          PIC X(110).                                      
006700     EJECT                                                                
006800 FD  UTFIL                                                                
006900     RECORDING F                                                          
007000     BLOCK 0 RECORDS.                                                     
007100                                                                          
007200 01  UTFILEN             PIC X(121).                                      
007300     EJECT                                                                
007400 FD  LISTA9-91                                                            
007500     RECORDING F                                                          
007600     BLOCK 0 RECORDS.                                                     
007700                                                                          
007800 01  LISTA-9-91          PIC X(110).                                      
007900     EJECT                                                                
008000 SD  SORTER.                                                              
008100                                                                          
008200*01  POST     -COPY W51335    -PRE SORT-.                                 
008300     EJECT                                                                
008400 WORKING-STORAGE SECTION.                                                 
008500                                                                          
008600 77  IDPGM                         PIC X(8) VALUE 'W5133300'.             
008700 77  JA                            PIC X    VALUE 'J'.                    
008800 77  NEJ                           PIC X    VALUE 'N'.                    
008900                                                                          
009000 77  WS-DAP                  PIC X       VALUE 'N'.                       
009100   88  WS-DAP-OPEN                       VALUE 'J'.                       
009200   88  WS-DAP-CLOSE                      VALUE 'N'.                       
009300                                                                          
009400 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
009500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
009600 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
009700 77  KDRC-DISPLAY                PIC Z(5).                                
009800                                                                          
009810 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
009820 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
009830                                                                          
009900 01  W-SUB-PROG.                                                          
010000     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
010100     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
010200     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
010300     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
010500     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
010600     03  WZ01SEND            PIC X(8)    VALUE 'WZ01SEND'.                
010610     03  WTRAUTF8            PIC X(8)    VALUE 'WTRAUTF8'.                
010700                                                                          
010710 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010720 01  FILLER REDEFINES DAGENS-DATUM.                                       
010730     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010740     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010750     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010760     EJECT                                                                
010800*    --- PARAMETERS TO ABEND                                              
010900                                                                          
011000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011300                                                                          
011400  01  WS-YYMMDDHHMM.                                                      
011500      03 WS-YYMMDD                PIC  9(6).                              
011600      03 WS-TIME                  PIC  9(4).                              
011700                                                                          
011800  01  WS-HHMMSSTH.                                                        
011900      03 WS-HHMM                  PIC  9(4).                              
012000      03 WS-SSTH                  PIC  9(4).                              
012100                                                                          
012200                                                                          
012400 01  FILLER                      PIC X(16) VALUE 'SEND-CONTROL'.          
012500*01  -COPY WZ01SEND                                                       
012600     SKIP3                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
012900     EJECT                                                                
013000                                                                          
013100 01  ARBETSFAELT.                                                         
013200     03  RADMAX          PIC S9(3)   VALUE +42  COMP-3.                   
013300     03  EOF             PIC X       VALUE 'N'.                           
013400                                                                          
013500 01  SUBPROGRAM.                                                          
013600     03  WDATUM              PIC X(6)        VALUE 'WDATUM'.              
013700     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM'.             
013800     EJECT                                                                
013900*   --- VALID IDDC CODES                                                  
014000*                                                                         
014100*01  -COPY WWDC99                                                         
014200*                                                                         
014300 01  FILLER.                                                              
014400*    03  -COPY WDATKORT.                                                  
014500     EJECT                                                                
014600 01  FILLER.                                                              
014700*    03  -COPY W0005  -PRE POSTSUM-.                                      
014710                                                                          
014720 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
014730*01  -COPY WTRAUTF8                                                       
014740                                                                          
014800     EJECT                                                                
014900 01  W-AREA.                                                              
015000*    03  AREA   -COPY W51335   -PRE W9-                                   
015100     EJECT                                                                
015200****************************************************************          
015300*         HÄR FÖLJER RADER FÖR LISTA3 'ARTIKLAR FÖR VILKA      *          
015400*         UTREDNINGSSALDO UPPDATERATS'                         *          
015500****************************************************************          
015600 01  FILLER               PIC X(16)       VALUE ALL 'U'.                  
015700                                                                          
015800 01  BLANKRAD           PIC X        VALUE SPACE.                         
015900                                                                          
016000 01  JR-HJELP-AREA.                                                       
016100     03  JR-SIDOR         PIC S9(5)        VALUE +0  COMP-3.              
016200     03  JR-RADER         PIC S9(5)        VALUE +99 COMP-3.              
016300     03  OLD-IDDC         PIC X(2)         VALUE SPACE.                   
016400     03  WS-IDDC-WEB      PIC X(2)         VALUE SPACE.                   
016500     SKIP2                                                                
016600 01  JR-RUBRIK1.                                                          
016700     03  FILLER           PIC X(2)           VALUE SPACE.                 
016800     03  FILLER           PIC X(39)  VALUE                                
016900               'VOLVO CAR CORPORATION, CUSTOMER SERVICE'.                 
017000     03  FILLER           PIC X(10)  VALUE '  W51333-9'.                  
017100     03  JR-RUB1-IDDC     PIC XX             VALUE SPACE.                 
017200     03  FILLER           PIC X(19)                                       
017300         VALUE ' ADJUSTMENT REPORT.'.                                     
017400     03  FILLER           PIC X(11)          VALUE SPACE.                 
017500     03  FILLER           PIC X(3)           VALUE 'DC '.                 
017600     03  JR-RUB1-IDDC2    PIC X(3).                                       
017700     03  JR-AAR           PIC B99.                                        
017800     03  JR-MAN           PIC B99.                                        
017900     03  JR-DAG           PIC B99.                                        
018000     03  FILLER           PIC X(6)           VALUE ' PAGE '.              
018100     03  JR-SIDA          PIC Z(4).                                       
018200     SKIP2                                                                
018300 01  RUBRIK-ONDEMAND.                                                     
018400     03  FILLER           PIC X(9)  VALUE ' W51333-9'.                    
018500     03  ON-RUB1-IDDC     PIC XX             VALUE SPACE.                 
018600     03  FILLER           PIC X(9)   VALUE SPACE.                         
018700     03  FILLER           PIC X(4)   VALUE 'VCCS'.                        
018800     03  FILLER           PIC X(16)  VALUE SPACE.                         
018900     03  FILLER           PIC X(40)                                       
019000                VALUE 'ADJUSTMENT REPORT                       '.         
019100     03  FILLER           PIC X(10)  VALUE SPACE.                         
019200     03  FILLER           PIC X(4)   VALUE 'DC: '.                        
019300     03  ON-RUB1-IDDC2    PIC X(2).                                       
019400     03  FILLER           PIC X(4)   VALUE SPACE.                         
019500     03  FILLER           PIC X(6)   VALUE 'DATE: '.                      
019600     03  ON-AAR           PIC 99.                                         
019700     03  ON-MAN           PIC 99.                                         
019800     03  ON-DAG           PIC 99.                                         
019900                                                                          
020000 01  JR-RUBRIK2.                                                          
020100     03  FILLER           PIC X(47) VALUE                                 
020200               '     PARTNO  DC  DESCRIPTION   SUPPLIER    AREA'.         
020300     03  FILLER           PIC X(38) VALUE                                 
020400               '   P-CODE  ADJ.TYPE  PROCURER  ADJ. LS'.                  
020500     03  FILLER           PIC X(23) VALUE                                 
020600               '  ADJ QTY    ADJ. VALUE'.                                 
020700                                                                          
020800 01  JR-RAD.                                                              
020900     03  JR-IDARTNR       PIC Z(11).                                      
021000     03  FILLER           PIC XX VALUE SPACE.                             
021100     03  JR-IDDC          PIC X(2).                                       
021200     03  FILLER           PIC XX VALUE SPACE.                             
021300     03  JR-BEART         PIC X(15).                                      
021400     03  FILLER           PIC X(2) VALUE SPACE.                           
021500     03  JR-IDLEVNR       PIC X(5).                                       
021600     03  FILLER           PIC X(3) VALUE SPACE.                           
021700     03  JR-ADLAGOMR      PIC ZZ99.                                       
021800     03  JR-KDPRODSL      PIC Z(8).                                       
021900     03  JR-KDINVKAT      PIC Z(9).                                       
022000     03  JR-IDANSKNR      PIC Z(11)-.                                     
022100     03  JR-KVLS          PIC Z(9)9-.                                     
022200     03  JR-KVJUSTKV      PIC Z(7)9-.                                     
022300     03  JR-JUST-VARDE    PIC Z(9)9.99-  BLANK WHEN ZERO.                 
022400     EJECT                                                                
022500*--------------------------------------- NYCKLAR TILL BASERNA             
022600                                                                          
022700 01  W-IDDC-B6-X.                                                         
022800     03 W-IDDC-B6        PIC X(2).                                        
022810                                                                          
022820 01  W-IDARTNR-X.                                                         
022830     03  W-IDARTNR       PIC S9(9)   COMP-3.                              
022840 01  W-IDSKYLT-X.                                                         
022850     03  W-IDSKYLT       PIC X(3)    VALUE SPACE.                         
022900                                                                          
023000     EJECT                                                                
023100*--------------------------------------- ARBETSAREOR TILL                 
023200*                                        IMS-SEKTIONERNA                  
023300 01      IMS-WS.                                                          
023400   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
023500     SKIP3                                                                
023600*--------------------------------------- STATUSKOD FRÅN IMS               
023700   03    STATUS-WS       PIC XX.                                          
023800     88  SEGMENT-FINNS               VALUE '  '.                          
023900     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
024000     88  SEGMENT-FINNS-REDAN         VALUE 'II'.                          
024100     88  IMS-EJ-OK                   VALUE 'XD'.                          
024200     SKIP3                                                                
024300   03    SSA1            PIC X(128).                                      
024310   03    SSA2            PIC X(128).                                      
024400                                                                          
024500                                                                          
024600   03    GODK-STATUSKODER.                                                
024700     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
024800                                                                          
024900     EJECT                                                                
025000********   COPYTEXTER  TILL WEB-LDC                                       
025100**                                                                        
025200 01  FILLER                 PIC X(16)   VALUE 'HDR-AREA'.                 
025300 01  HDR-AREA.                                                            
025400*    03  -COPY WZ01REQU  -PRE HDR-                                        
025500*    03  -COPY WZ04HDR                                                    
025600                                                                          
025700 01  WEB-DOC-AREA.                                                        
025800*    03  -COPY  W513331 -PRE LINE-                                        
025900     EJECT                                                                
026000                                                                          
026100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026200 01   DLI-IO-AREA-B601.                                                   
026300*     03  -COPY WDB601                                                    
026310                                                                          
026320 01  FILLER               PIC X(16)   VALUE 'WDD311 AREA'.                
026330 01  DLI-IO-WDD311.                                                       
026340*    03  -COPY WDD311                                                     
026350                                                                          
026400                                                                          
026500 LINKAGE SECTION.                                                         
026600 *01  -COPY W0009   -PRE MSG-                                             
026700      EJECT                                                               
026800                                                                          
026900 01  DISTRDOC-PCB                PIC X.                                   
027000                                                                          
027100*01  -COPY W0008 -PRE WDB6-                                               
027200     05  FILLER           PIC X.                                          
027210                                                                          
027220*01  -COPY W0008 -PRE WDD3-                                               
027230     05  FILLER           PIC X.                                          
027300                                                                          
027400     EJECT                                                                
027500 PROCEDURE DIVISION USING MSG-PCB DISTRDOC-PCB WDB6-PCB WDD3-PCB.         
027600                                                                          
027700 MAIN SECTION.                                                            
027800     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDB6-PCB WDD3-PCB.        
027900                                                                          
028000     PERFORM A-INIT                                                       
028100                                                                          
028200     SORT SORTER ON ASCENDING  KEY SORT-IDDC                              
028300                    DESCENDING KEY SORT-SORTVAERDE                        
028400                    ASCENDING  KEY SORT-IDARTNR                           
028500                                   SORT-IDANSKNR                          
028600                 USING W51333                                             
028700                                                                          
028800     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
028900     IF SORT-RETURN > +0                                                  
029000       DISPLAY ' W51333 SORT-FEL '                                        
029100       MOVE +16 TO ABEND-CODE                                             
029200       CALL ABEND USING ABEND-CODE                                        
029300     ELSE                                                                 
029400       PERFORM C-AVSLUTA                                                  
029500       MOVE +0 TO RETURN-CODE                                             
029600     END-IF                                                               
029700     GOBACK                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 A-INIT SECTION.                                                          
030100                                                                          
030200     OPEN OUTPUT LISTA9-21                                                
030300                 LISTA9-23                                                
030400                 LISTA9-24                                                
030500                 LISTA9-25                                                
030600                 LISTA9-26                                                
030700                 LISTA9-91                                                
030800                 UTFIL                                                    
030900                                                                          
031000     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
031100     MOVE D-AAR           TO JR-AAR                                       
031200     MOVE D-MAANAD        TO JR-MAN                                       
031300     MOVE D-DAG           TO JR-DAG                                       
031400     MOVE D-AAR           TO ON-AAR DAGENS-DATUM-AAR                      
031500     MOVE D-MAANAD        TO ON-MAN DAGENS-DATUM-MAANAD                   
031600     MOVE D-DAG           TO ON-DAG DAGENS-DATUM-DAG                      
031700     ACCEPT WS-YYMMDD      FROM DATE                                      
031800     ACCEPT WS-HHMMSSTH    FROM TIME                                      
031900     MOVE   WS-HHMM       TO WS-TIME                                      
032000                                                                          
032100                                                                          
032200                                                                          
032300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
032400     .                                                                    
032500     EJECT                                                                
032600 B-SKAPA-LISTOR SECTION.                                                  
032700                                                                          
032800     PERFORM S03-RETURN-SORT                                              
032900     PERFORM UNTIL EOF = JA                                               
033000       PERFORM BA-LISTA9                                                  
033100       PERFORM S03-RETURN-SORT                                            
033200     END-PERFORM                                                          
033300*** KONTROLERA OM DISTRIBUTION AND PRINT ÄR ÖPPEN ****                    
033400     IF WS-DAP-OPEN                                                       
033500       PERFORM S05-SEND-CLOSE                                             
033600       MOVE NEJ TO WS-DAP                                                 
033700       DISPLAY ' SISTA CLOSE'                                             
033800     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 BA-LISTA9  SECTION.                                                      
034200******************************************************************        
034300*         JUSTERINGSRAPPORT         LISTA 9                      *        
034400******************************************************************        
034500                                                                          
034600     MOVE W9-IDARTNR           TO JR-IDARTNR                              
035000     MOVE W9-IDDC              TO JR-IDDC                                 
035200                                                                          
035300     MOVE W9-IDANSKNR          TO JR-IDANSKNR                             
035400     MOVE W9-BEART             TO JR-BEART                                
035500     MOVE W9-IDLEVNR           TO JR-IDLEVNR                              
035600     MOVE W9-ADLAGOMR          TO JR-ADLAGOMR                             
035700     MOVE W9-KDINVKAT          TO JR-KDINVKAT                             
035800     MOVE W9-KDPRODSL          TO JR-KDPRODSL                             
035900     MOVE W9-KVLS              TO JR-KVLS                                 
036000     MOVE W9-KVJUSTKV          TO JR-KVJUSTKV                             
036100     MOVE W9-IDDC              TO WS-IDDC                                 
036200                                  W-IDDC-B6                               
036300                                                                          
036400     PERFORM IMS-GU-WDB601                                                
036500     IF  SEGMENT-FINNS                                                    
036600     AND DCS-FLPRISSPR = JA                                               
036700       MOVE ZERO               TO JR-JUST-VARDE                           
036800     ELSE                                                                 
036900       MOVE W9-SUARTSTD-JUST   TO JR-JUST-VARDE                           
037000     END-IF                                                               
037100***** SKAPA DAP POSTER ********                                           
037200     DISPLAY ' WEB DCFL' DCS-FLWEBDC ' DC ' W9-IDDC                       
037300     IF DCS-FLWEBDC = JA                                                  
037400       DISPLAY ' SKA GÖRA  DAP POSTER '                                   
037500       PERFORM BC-WEB-LISTA                                               
037600     END-IF                                                               
037700     IF W9-IDDC NOT = OLD-IDDC                                            
037800       ADD +99                 TO JR-RADER                                
037900       MOVE +0                 TO JR-SIDOR                                
038000       MOVE W9-IDDC            TO OLD-IDDC                                
038100                                  ON-RUB1-IDDC                            
038200                                  ON-RUB1-IDDC2                           
038600       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
038700       WRITE UTFILEN    FROM JR-RUBRIK2                                   
038800     END-IF                                                               
038900     MOVE W9-IDDC              TO JR-RUB1-IDDC                            
039000                                  JR-RUB1-IDDC2                           
039100     IF JR-RADER > RADMAX                                                 
039200       ADD +1                  TO JR-SIDOR                                
039300       MOVE JR-SIDOR           TO JR-SIDA                                 
040000       EVALUATE TRUE                                                      
040410         WHEN SDC-NL                                                      
040420           WRITE LISTA-9-21 FROM  JR-RUBRIK1 AFTER PAGE                   
040430           WRITE LISTA-9-21 FROM  JR-RUBRIK2 AFTER 2                      
040440           WRITE LISTA-9-21 FROM  BLANKRAD AFTER 1                        
040500*        WHEN SDC-GB                                                      
040600*          WRITE LISTA-9-23 FROM  JR-RUBRIK1 AFTER PAGE                   
040700*          WRITE LISTA-9-23 FROM  JR-RUBRIK2 AFTER 2                      
040800*          WRITE LISTA-9-23 FROM  BLANKRAD AFTER 1                        
040900         WHEN SDC-ES                                                      
041000           WRITE LISTA-9-24 FROM  JR-RUBRIK1 AFTER PAGE                   
041100           WRITE LISTA-9-24 FROM  JR-RUBRIK2 AFTER 2                      
041200           WRITE LISTA-9-24 FROM  BLANKRAD AFTER 1                        
041300         WHEN SDC-IT                                                      
041400           WRITE LISTA-9-25 FROM  JR-RUBRIK1 AFTER PAGE                   
041500           WRITE LISTA-9-25 FROM  JR-RUBRIK2 AFTER 2                      
041600           WRITE LISTA-9-25 FROM  BLANKRAD AFTER 1                        
041700         WHEN SDC-AT                                                      
041800           WRITE LISTA-9-26 FROM  JR-RUBRIK1 AFTER PAGE                   
041900           WRITE LISTA-9-26 FROM  JR-RUBRIK2 AFTER 2                      
042000           WRITE LISTA-9-26 FROM  BLANKRAD AFTER 1                        
042010         WHEN SDC-NL-ET                                                   
042020           WRITE LISTA-9-91   FROM  JR-RUBRIK1 AFTER PAGE                 
042030           WRITE LISTA-9-91   FROM  JR-RUBRIK2 AFTER 2                    
042040           WRITE LISTA-9-91   FROM  BLANKRAD AFTER 1                      
042100      END-EVALUATE                                                        
042300      MOVE +8 TO JR-RADER                                                 
042400     END-IF                                                               
042500                                                                          
042900     EVALUATE TRUE                                                        
043000       WHEN SDC-NL                                                        
043100           WRITE LISTA-9-21 FROM JR-RAD AFTER 1                           
043200*      WHEN SDC-GB                                                        
043300*          WRITE LISTA-9-23 FROM JR-RAD AFTER 1                           
043400       WHEN SDC-ES                                                        
043500           WRITE LISTA-9-24 FROM JR-RAD AFTER 1                           
043600       WHEN SDC-IT                                                        
043700           WRITE LISTA-9-25 FROM JR-RAD AFTER 1                           
043800       WHEN SDC-AT                                                        
043900           WRITE LISTA-9-26 FROM JR-RAD AFTER 1                           
043910       WHEN SDC-NL-ET                                                     
043920           WRITE LISTA-9-91 FROM JR-RAD AFTER 1                           
044000     END-EVALUATE                                                         
044200     WRITE UTFILEN          FROM JR-RAD                                   
044300     ADD +1 TO JR-RADER                                                   
044400     .                                                                    
044500     SKIP3                                                                
044600 BC-WEB-LISTA  SECTION.                                                   
044700     DISPLAY ' BC-WEB-LISTA         '                                     
044800     IF W9-IDDC NOT = WS-IDDC-WEB                                         
044900       MOVE W9-IDDC TO WS-IDDC-WEB                                        
045000       IF WS-DAP-OPEN                                                     
045100         DISPLAY ' CLOSE'                                                 
045200         PERFORM S05-SEND-CLOSE                                           
045300         MOVE NEJ TO WS-DAP                                               
045400       END-IF                                                             
045500       PERFORM BCA-SKAPA-HEADER                                           
045600     END-IF                                                               
045700     PERFORM BCB-SKAPA-LINE                                               
045800     .                                                                    
045900     SKIP3                                                                
046000 BCA-SKAPA-HEADER SECTION.                                                
046100     DISPLAY ' BCA-SKAPA-HEADER     '                                     
046200     PERFORM S05-SEND-OPEN                                                
046300     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
046400     MOVE JA TO WS-DAP                                                    
046500                                                                          
046600     MOVE 001                        TO HDR-REQU-IDMSGVER                 
046700     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
046800     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
046900                                                                          
047000     MOVE SPACE                      TO HDR-IDOUTREC                      
047100     MOVE 'ADJUSTMENTS'              TO HDR-IDOUTTYPE                     
047200     MOVE W9-IDDC                    TO HDR-IDOUTREC(1:2)                 
047300     MOVE 'W51333'                   TO HDR-IDOUTREC(3:8)                 
047400*    MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
047401     MOVE DAGENS-DATUM               TO HDR-IDLIST                        
047410                                                                          
047500*HDR                                                                      
047600     PERFORM S05-PUT-HEADER                                               
047700     DISPLAY 'HEAD AREA' HDR-AREA                                         
047800     .                                                                    
047900     SKIP3                                                                
048000 BCB-SKAPA-LINE SECTION.                                                  
048100     DISPLAY ' BCB-SKAPA-LINE       '                                     
048200     MOVE '1'                        TO LINE-IDAFPRCD                     
048300     MOVE W9-IDDC                    TO LINE-IDDC                         
048400     MOVE W9-IDARTNR                 TO LINE-IDARTNR                      
048500     MOVE W9-BEART                   TO LINE-BEART                        
048510                                                                          
048520*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
048530*    BEFORE DISPLAY OF LINE-BEART                                         
048540                                                                          
048550     MOVE W9-IDARTNR                 TO W-IDARTNR                         
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
048594     PERFORM IMS-GU-WDD311                                                
048595     IF SEGMENT-FINNS                                                     
048596        MOVE TEXT-BEART              TO TRAUTF8-TECONV-FROM               
048597     ELSE                                                                 
048598        MOVE SPACE                   TO TRAUTF8-TECONV-FROM               
048599        MOVE '278 '                  TO TRAUTF8-KDCP                      
048600     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
048601     MOVE 25                         TO TRAUTF8-KVMAXTL                   
048602     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
048603     MOVE TRAUTF8-TECONV-TO          TO LINE-BEART                        
048604                                                                          
048610     MOVE W9-IDLEVNR                 TO LINE-IDLEVNR                      
048700     MOVE W9-ADLAGOMR                TO LINE-ADLAGOMR                     
048800     MOVE W9-KDPRODSL                TO LINE-KDPRODSL                     
048900     MOVE W9-KDINVKAT                TO LINE-KDINVKAT                     
049000     MOVE W9-IDANSKNR                TO LINE-IDANSKNR                     
049100     MOVE W9-KVLS                    TO LINE-KVLS                         
049200     MOVE W9-KVJUSTKV                TO LINE-KVJUSTKV                     
049300     IF DCS-FLPRISSPR = 'J'                                               
049400       MOVE 0                        TO LINE-SUARTSTD                     
049500     ELSE                                                                 
049600       MOVE W9-SUARTSTD-JUST         TO LINE-SUARTSTD                     
049700     END-IF                                                               
049800     PERFORM S05-PUT-REPORT-LINE                                          
049900     PERFORM S04-POST-SUM-DAP                                             
050000     DISPLAY ' LINE ' WEB-DOC-AREA                                        
050100     .                                                                    
050200     EJECT                                                                
050300 C-AVSLUTA  SECTION.                                                      
050400                                                                          
050500     CLOSE LISTA9-21                                                      
050600           LISTA9-23                                                      
050700           LISTA9-24                                                      
050800           LISTA9-25                                                      
050900           LISTA9-26                                                      
051000           LISTA9-91                                                      
051100           UTFIL                                                          
051200                                                                          
051300     MOVE 'S'     TO POSTSUM-OPKOD                                        
051400     CALL POSTSUM USING POSTSUM-PARM                                      
051500     .                                                                    
051600     SKIP3                                                                
051700 S03-RETURN-SORT SECTION.                                                 
051800                                                                          
051900     RETURN SORTER INTO W-AREA                                            
052000     AT END                                                               
052100         MOVE JA TO EOF                                                   
052200     NOT AT END                                                           
052300       MOVE 'W51333'   TO POSTSUM-FDNAMN                                  
052400       MOVE 'W51333D1' TO POSTSUM-DDNAMN2                                 
052500       MOVE W9-IDDC    TO POSTSUM-TRANSTYP                                
052600       CALL POSTSUM USING POSTSUM-PARM                                    
052700     END-RETURN                                                           
052800     .                                                                    
052900     EJECT                                                                
053000 S04-POST-SUM-DAP SECTION.                                                
053100                                                                          
053200       MOVE 'W5133D'   TO POSTSUM-FDNAMN                                  
053300       MOVE 'DAP     ' TO POSTSUM-DDNAMN2                                 
053400       MOVE W9-IDDC    TO POSTSUM-TRANSTYP                                
053500       CALL POSTSUM USING POSTSUM-PARM                                    
053600                                                                          
053700     .                                                                    
053800     EJECT                                                                
053900 S05-SEND-OPEN SECTION.                                                   
054000                                                                          
054100     MOVE 'CARPARTS.DAP.DISTRDOCWEB'      TO SEND-ADDISPABS               
054200     MOVE 'OPEN'                          TO SEND-KDFUNC                  
054300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
054400                         SEND-OPEN-AREA                                   
054500     IF SEND-KDRC > ZERO                                                  
054600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
054700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
054800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
055000     END-IF                                                               
055100     .                                                                    
055200     SKIP3                                                                
055300 S05-PUT-HEADER SECTION.                                                  
055400                                                                          
055500     MOVE 'PUT'                           TO SEND-KDFUNC                  
055600     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
055700     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
055800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
055900                         SEND-KVDLEN                                      
056000                         HDR-AREA                                         
056100     IF SEND-KDRC > ZERO                                                  
056200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
056300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
056400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
056500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 S05-PUT-REPORT-LINE    SECTION.                                          
057000                                                                          
057100     MOVE 'PUT'                           TO SEND-KDFUNC                  
057200     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
057300     MOVE LENGTH OF WEB-DOC-AREA          TO SEND-KVDLEN                  
057400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
057500                         SEND-KVDLEN                                      
057600                         WEB-DOC-AREA                                     
057700     IF SEND-KDRC > ZERO                                                  
057800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
057900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
058000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
058100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058200     END-IF                                                               
058300     .                                                                    
058400     SKIP3                                                                
058500 S05-SEND-CLOSE SECTION.                                                  
058600                                                                          
058700     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
058800     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
058900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
059000     .                                                                    
059100     EJECT                                                                
059200* IMS SECTIONER                                                           
059300     SKIP2                                                                
059400 IMS-GU-WDB601    SECTION.                                                
059500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
059600          DELIMITED BY SIZE INTO SSA1                                     
059700     MOVE '  GE' TO GODK-STATUSKODER                                      
059800     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
059900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
060000     PERFORM IMS-STATUSKONTROLL                                           
060100     .                                                                    
060200     EJECT                                                                
060210 IMS-GU-WDD311 SECTION.                                                   
060220                                                                          
060230     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
060240             DELIMITED BY SIZE INTO SSA1                                  
060250     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
060260             DELIMITED BY SIZE INTO SSA2                                  
060270     MOVE '  GE'                 TO GODK-STATUSKODER                      
060280     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
060290     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
060291     PERFORM IMS-STATUSKONTROLL                                           
060292     .                                                                    
060293     EJECT                                                                
060300 IMS-STATUSKONTROLL SECTION.                                              
060400                                                                          
060500     SET STATUS-IX TO 1                                                   
060600     SEARCH GODK-STATUS AT END CALL FELLOG                                
060700        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
060800        CONTINUE                                                          
060900     END-SEARCH                                                           
061000     .                                                                    
