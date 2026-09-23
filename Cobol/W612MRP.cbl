000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W612MRP.                                                 
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   12/01/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SUPROGRAM TO GET DATA FROM MAIN PROGRAM AND CREATE               
000900*        MRP LABLES FOR INDIA DC 67 ON WEB THRU D&P                       
001000*                                                                         
001100*        PROGRAMMET          READS      WDD3                              
001200*                            READS      WDT4                              
001300                                                                          
001400     SKIP3                                                                
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 DATA DIVISION.                                                           
001900     EJECT                                                                
002000 WORKING-STORAGE SECTION.                                                 
002100                                                                          
002200*    -- CHECKED BY WY2000                                                 
002300 77  IDPGM                       PIC X(08)   VALUE 'W612MRP'.             
002400 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
002500 77  ERROR-TEXT                  PIC X(64)   VALUE 'ERROR-TEXT'.          
002600 77  YES                         PIC X       VALUE 'Y'.                   
002700 77  NOO                         PIC X       VALUE 'N'.                   
002800 77  WS-CURRENT-SECTION          PIC X(16)   VALUE 'MAIN'.                
002900 77  WS-CURRENT-IMS-SECTION      PIC X(16)   VALUE SPACE.                 
003000 77  WS-PRARTBTO-SC              PIC 9(9)V9(3)   VALUE ZERO.              
003100 77  WS-CLAG-PRARTSJK            PIC 9(9)V9(3)   VALUE ZERO.              
003200 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
003300 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
003400 01  WS-KVAVBART-MRP             PIC  9(7).                               
003500 01  WS-KVQPACK-MRP              PIC  9(7).                               
003600 01  WS-PRARTBTO-INT             PIC 9(9).                                
003700 77  SPAR-RAD-KDBENHOM           PIC S9      VALUE ZERO  COMP-3.          
003800                                                                          
003900                                                                          
004000 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
004100 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004200 77  SHIP-INDX                   PIC S9(9)  VALUE +0.                     
004300 77  KIT-TAB-IX                  PIC  9(9)  VALUE  0.                     
004400 77  LABEL-IX                    PIC  9(9)  VALUE  0.                     
004500 77  PARTS-OF-KIT-PART-IX        PIC  9(9)  VALUE  0.                     
004600 77  NUM-PARTS-IN-KIT-PART       PIC  9(9)  VALUE  0.                     
004700                                                                          
004800 77  MAX-NO-OF-CONTAIN-PARTS     PIC S9(9)  VALUE +125  COMP SYNC.        
004900 77  MAX-NO-OF-PARTS-LABEL       PIC S9(9)  VALUE +10   COMP SYNC.        
005000                                                                          
005100 01  WS-DATUM                    PIC  X(6).                               
005200                                                                          
005300 01  WS-TIPRTDAT.                                                         
005400     03 WS-TIMM                  PIC 9(2).                                
005500     03 WS-HYPEN                 PIC X(1) VALUE '-'.                      
005600     03 WS-TIYY                  PIC 9(4).                                
005700*    03 WS-CENTURY               PIC X(2) VALUE '20'.                     
005800                                                                          
005900 01  WS-CURRENT-DATE-TIME.                                                
006000     03  WS-YEAR                 PIC 9(4).                                
006100     03  WS-MONTH                PIC 9(2).                                
006200     03  WS-DAY                  PIC 9(2).                                
006300     03  WS-HOUR                 PIC 9(2).                                
006400     03  WS-MINUTE               PIC 9(2).                                
006500 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
006600     03  FILLER                  PIC X(2).                                
006700     03  WS-TIYYMMDDHHMM         PIC X(10).                               
006800                                                                          
006900*    -- SWITCH FÖR WDJ411 DB                                              
007000 77  SW-WDJ4-DATA                    PIC X       VALUE 'N'.               
007100     88  WDJ4-DATA                               VALUE 'Y'.               
007200                                                                          
007300 77  KIT-DAP-OPEN-SW                 PIC X   VALUE 'C'.                   
007400     88  KIT-DAP-OPEN                        VALUE 'O'.                   
007500     88  KIT-DAP-CLOSE                       VALUE 'C'.                   
007600                                                                          
007700 77  KIT-STATUS-SW                   PIC X.                               
007800     88  KIT-STATUS-OK                       VALUE 'Y'.                   
007900                                                                          
008000 77  KIT-DOC-SW                      PIC X   VALUE 'N'.                   
008100     88  KIT-DOC-WRITTEN                     VALUE 'Y'.                   
008200                                                                          
008300                                                                          
008400*    -COPY WY2000W1                                                       
008500*                                                                         
008600*      --- VALID IDDC CODES                                               
008700*                                                                         
008800*01    -COPY WWDC99                                                       
008900       EJECT                                                              
009000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009100 01  GENERELLA-SUBPROGRAM.                                                
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009800     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
009900     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
010000     EJECT                                                                
010100 01  KDRC-DISPLAY                PIC Z(5).                                
010200 01  FELTEXT.                                                             
010300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010500*                                                                         
010600*    --- PARAMETERS TO ABEND                                              
010700                                                                          
010800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011300     SKIP3                                                                
011400*01  -COPY WZ01SUB                                                        
011500     SKIP3                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
011700*01  -COPY WZ01SEND                                                       
011800*                                                                         
011900* VARIABLER TILL SUBPROGRAM W400ARTU                                      
012000 01  FILLER                      PIC X(08)   VALUE 'W400ARTU'.            
012100*01  -COPY W400ARTU                                                       
012200                                                                          
012300* VARIABLER TILL SUBPROGRAM W510CURR                                      
012400 01  FILLER                      PIC X(08)   VALUE 'W510CURR'.            
012500*01  -COPY W510CURR                                                       
012600                                                                          
012700*                                                                         
012800 01  HDR-AREA.                                                            
012900*    03  -COPY WZ01REQU  -PRE HDR-                                        
013000*    03  -COPY WZ04HDR                                                    
013100                                                                          
013200 01  KIT-HDR-AREA.                                                        
013300*    03 -COPY WZ01REQU -PRE KIT-                                          
013400*    03 -COPY WZ04HDR  -PRE KIT-                                          
013500                                                                          
013600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
013700                                                                          
013800 01  RESP-AREA.                                                           
013900*    03  -COPY WZ01RESP                                                   
014000 01  FILLER                  PIC X(16)   VALUE 'SEND-AREA-MRP'.           
014100 01  MRP-LABEL-AREA.                                                      
014200*    03  -COPY W403MRP                                                    
014300                                                                          
014400*    --- PARAMETERS TO WZ01SEND FOR MRP KIT LABEL                         
014500 01  FILLER                  PIC X(16)   VALUE 'WZ01SEND-FOR-KIT'.        
014600*01  -COPY WZ01SEND   -PRE KIT-                                           
014700 01  FILLER                  PIC X(16)   VALUE 'SENDAREA-MRPKIT'.         
014800 01  MRP-KIT-LABEL-AREA.                                                  
014900*    03  -COPY WMRPKIT2                                                   
015000                                                                          
015100*01  -COPY WMSGAREA                                                       
015200                                                                          
015300 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
015400*01  -COPY WTRAUTF8                                                       
015500                                                                          
015600 01  WS-IDSKYLT-GB           PIC X(3) VALUE 'GB '.                        
015700 01  WS-IDSKYLT-CN           PIC X(3) VALUE 'RCN'.                        
015800                                                                          
015900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016000*                                                                         
016100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016200                                                                          
016300 01  NYCKLAR-TILL-DLI.                                                    
016400                                                                          
016500*   KEYS TO SUGGESTED RETAIL PRICES                                       
016600     03  W-WDC301KY-X.                                                    
016700         05  W-IDARTNR-WDC3      PIC S9(9)   COMP-3 VALUE ZERO.           
016800     03  W-WDC311KY-X.                                                    
016900         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
017000                                                                          
017100**   ARTIKEL-CLAGER INFO                                                  
017200     03  W-IDARTNR-X.                                                     
017300         05 W-IDARTNR    PIC S9(9)   VALUE ZERO  COMP-3.                  
017400                                                                          
017500     03  W-KDSEGKEY-X.                                                    
017600         05 W-KDSEGKEY   PIC X(1)    VALUE '1'.                           
017700                                                                          
017800*   KEYS TO CUSTOMER FILE WDB2       ***********                          
017900                                                                          
018000                                                                          
018100*----> SEKUNDÄR INDEX ARTIKELBENÄMNING                                    
018200                                                                          
018300     03  W-WDD3BSEQ-X.                                                    
018400         05  W-D3BSEQ-IDARTNR    PIC  S9(9) COMP-3.                       
018500                                                                          
018600     03  W-IDSKYLT-X             PIC X(3).                                
018700                                                                          
018800     03  W-BEART-X.                                                       
018900         05  W-BEART             PIC  X(25) VALUE SPACE.                  
019000*NEW                                                                      
019100**   WDJ401-KDP KIT PART                                                  
019200     03  W-IDARTNR-SATS-X.                                                
019300         05  W-IDARTNR-SATS      PIC S9(9)  VALUE ZERO  COMP-3.           
019400*                                                                         
019500 01  FILLER                      PIC X(16)  VALUE 'WNDCADRE '.            
019600*   -COPY WNDCADRE                                                        
019700                                                                          
019800                                                                          
019900 01  FILLER                  PIC X(16) VALUE 'IMS-WS STATUS-WS'.          
020000                                                                          
020100*    --- STATUS-KOD FRÅN IMS                                              
020200 01  STATUS-WS                   PIC XX.                                  
020300     88  SEGMENT-FOUND                       VALUE '  '.                  
020400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
020600                                                                          
020700 01  GODK-STATUSKODER.                                                    
020800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020900                                                                          
021000 01  SSA1                        PIC X(128).                              
021100 01  SSA2                        PIC X(64).                               
021200 01  SSA3                        PIC X(64).                               
021300                                                                          
021400*    --- IMS FUNKTIONSKODER                                               
021500*01  -COPY W0003                                                          
021600                                                                          
021700*    ---  DLI INPUT-OUTPUT AREA                                           
021800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
021900                                                                          
022000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDD301'.        
022100 01  DLI-IO-WDD301.                                                       
022200*    03  -COPY WDD301                                                     
022300*                                                                         
022400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDD311'.        
022500 01  DLI-IO-WDD311.                                                       
022600*    03  -COPY WDD311                                                     
022700*                                                                         
022800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDC301'.         
022900 01  DLI-IO-WDC301.                                                       
023000*    05  -COPY WDC301                                                     
023100*                                                                         
023200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDC311'.         
023300 01  DLI-IO-WDC311.                                                       
023400*    05  -COPY WDC311                                                     
023500*                                                                         
023600**   ARTIKEL-CLAGER INFO                                                  
023700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
023800 01  DLI-IO-WDK601.                                                       
023900*  03  -COPY WDK601.                                                      
024000*                                                                         
024100**   ARTIKEL-CLAGER INFO                                                  
024200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
024300 01  DLI-IO-WDK611.                                                       
024400*  03  -COPY WDK611.                                                      
024500*                                                                         
024600**   KIT PART                                                             
024700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ101'.           
024800 01  DLI-IO-WDJ101.                                                       
024900*  03  -COPY WDJ101.                                                      
025000                                                                          
025100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ111'.           
025200 01  DLI-IO-WDJ111.                                                       
025300*  03  -COPY WDJ111.                                                      
025400                                                                          
025500*                                                                         
025600**   KDP KIT PART                                                         
025700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ401'.           
025800 01  DLI-IO-WDJ401.                                                       
025900*  03  -COPY WDJ401.                                                      
026000*                                                                         
026100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDJ411'.           
026200 01  DLI-IO-WDJ411.                                                       
026300*  03  -COPY WDJ411.                                                      
026400*                                                                         
026500                                                                          
026600 LINKAGE SECTION.                                                         
026700                                                                          
026800*01  -COPY W612LABL                                                       
026900                                                                          
027000*01  -COPY WPLKSUMM                                                       
027100                                                                          
027200*01  -COPY WZ01REQU                                                       
027300                                                                          
027400 01  DISTRWEB-PCB                PIC X.                                   
027500                                                                          
027600 01  DISTRDO2-PCB                PIC X.                                   
027700                                                                          
027800*01  -COPY W0008  -PRE WDD3-                                              
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100*01  -COPY W0008  -PRE WDK6-                                              
028200     05  FILLER                  PIC X.                                   
028300                                                                          
028400*01  -COPY W0008  -PRE WDG2-                                              
028500     05  FILLER                  PIC X.                                   
028600                                                                          
028700*01  -COPY W0008  -PRE WDC3-                                              
028800     05  FILLER                  PIC X.                                   
028900                                                                          
029000*01  -COPY W0008  -PRE WDJ1-                                              
029100     05  FILLER                  PIC X.                                   
029200                                                                          
029300*01  -COPY W0008  -PRE WDJ4-                                              
029400     05  FILLER                  PIC X.                                   
029500                                                                          
029600*01  -COPY W0008      -PRE WDD3A-                                         
029700     05  FILLER                  PIC X.                                   
029800                                                                          
029900 PROCEDURE DIVISION  USING LABL-W612LABL PLK-SUMM-WPLKSUMM                
030000                           REQU-WZ01REQU                                  
030100                           DISTRWEB-PCB  DISTRDO2-PCB                     
030200                           WDD3-PCB WDK6-PCB                              
030300                           WDG2-PCB WDC3-PCB                              
030400                           WDJ1-PCB WDJ4-PCB                              
030500                           WDD3A-PCB.                                     
030600                                                                          
030700 MAIN SECTION.                                                            
030800                                                                          
030900     PERFORM A-INIT                                                       
031000                                                                          
031100     PERFORM S90-OPEN-MRP                                                 
031200     PERFORM S90-PUT-MRP-HEADER                                           
031300                                                                          
031400     PERFORM                                                              
031500     VARYING INDX FROM +1 BY +1                                           
031600       UNTIL INDX > LABL-KVRADER                                          
031700       PERFORM B-PROCESS-DATA                                             
031800     END-PERFORM                                                          
031900                                                                          
032000     IF LABL-KVRADER = ZERO                                               
032100*      Create a dummy document so web can know that document is           
032200*      created, but it doesn't have any parts to pick.                    
032300       MOVE 'DUMMY'              TO MRP-LABEL-AREA                        
032400       PERFORM S90-PUT-MRP                                                
032500     ELSE                                                                 
032600       IF PLK-SUMM-IDAFPRCD-TOT = '2'                                     
032700         PERFORM S90-PUT-MRP-SUMMARY                                      
032800       END-IF                                                             
032900     END-IF                                                               
033000                                                                          
033100     PERFORM S90-CLOSE-MRP                                                
033200                                                                          
033300*    IF KIT-DOC-WRITTEN                                                   
033400*      CONTINUE                                                           
033500*    ELSE                                                                 
033600*      Create a dummy document so web can know that document is           
033700*      created, but it doesn't have any kit parts                         
033800*      MOVE 'DUMMY'              TO MRP-KIT-LABEL-AREA                    
033900*      PERFORM S90-PUT-KIT                                                
034000*    END-IF                                                               
034100     IF KIT-DAP-OPEN                                                      
034200       PERFORM S90-CLOSE-KIT                                              
034300     END-IF                                                               
034400                                                                          
034500     GOBACK                                                               
034600     .                                                                    
034700                                                                          
034800 A-INIT SECTION.                                                          
034900     MOVE 'A-INIT SECTION  '     TO WS-CURRENT-SECTION                    
035000                                                                          
035100     MOVE LABL-IDDC              TO WS-IDDC                               
035200                                                                          
035300     MOVE FUNCTION CURRENT-DATE(1:12)                                     
035400                                 TO WS-CURRENT-DATE-TIME                  
035500                                                                          
035600     MOVE NOO                    TO KIT-DOC-SW                            
035700     SET KIT-DAP-CLOSE           TO TRUE                                  
035800     .                                                                    
035900                                                                          
036000 B-PROCESS-DATA SECTION.                                                  
036100     MOVE 'B-PROCESS-DATA  '     TO WS-CURRENT-SECTION                    
036200                                                                          
036300     MOVE LABL-IDARTNR (INDX)    TO MRP-IDARTNR                           
036400                                    MRP-KIT-IDARTNR                       
036500                                    W-IDARTNR                             
036600                                    W-IDARTNR-SATS                        
036700     MOVE LABL-KVANTAL (INDX)    TO MRP-KVAVBART                          
036800                                    MRP-KIT-KVAVBART                      
036900                                                                          
037000     IF LABL-KVQPACK (INDX) > 0                                           
037100       MOVE LABL-KVQPACK (INDX) TO WS-KVQPACK-MRP                         
037200       MOVE LABL-KVQPACK (INDX) TO MRP-KVQPACK                            
037300     ELSE                                                                 
037400       MOVE 1                   TO WS-KVQPACK-MRP                         
037500       MOVE 1                   TO MRP-KVQPACK                            
037600     END-IF                                                               
037700                                                                          
037800     PERFORM BA-MOVE-MRP-LABEL-INFO                                       
037900     PERFORM S90-PUT-MRP                                                  
038000                                                                          
038100     IF (ART-IDLEVNR = '1001' OR '1002') OR                               
038200        ART-KDSORT = 'SA'                                                 
038300       MOVE MRP-BEART           TO MRP-KIT-BEART-HEAD                     
038400       PERFORM BB-MOVE-KIT-INFO                                           
038500       PERFORM S90-PUT-KIT                                                
038600       MOVE YES                 TO KIT-DOC-SW                             
038700     END-IF                                                               
038800     .                                                                    
038900                                                                          
039000 BA-MOVE-MRP-LABEL-INFO   SECTION.                                        
039100     MOVE 'BA-MOVE-MRP-LABEL '  TO WS-CURRENT-SECTION                     
039200                                                                          
039300*W400ARTU                                                                 
039400     MOVE LABL-KDARTURS (INDX)     TO ARTU-KDARTURS                       
039500     MOVE 6010                     TO ARTU-IDDISTR                        
039600     MOVE LABL-IDDC                TO ARTU-IDDC                           
039700     CALL W400ARTU USING ARTU-W400ARTU                                    
039800     MOVE ARTU-BEARTURS-ENG        TO MRP-BEARTURS                        
039900                                                                          
040000*GB IDSKYLT FOR INDIA.                                                    
040100     MOVE 'GB'         TO W-IDSKYLT-X                                     
040200     MOVE W-IDARTNR    TO W-D3BSEQ-IDARTNR                                
040300                                                                          
040400     PERFORM IMS-GU-WDD311                                                
040500     IF SEGMENT-FOUND                                                     
040600        MOVE TEXT-BEART               TO MRP-BEART                        
040700     ELSE                                                                 
040800        MOVE 'NAME MISSING'           TO MRP-BEART                        
040900     END-IF                                                               
041000*MRP NEW FOR MRP INDIA LABEL                                              
041100*SHIPPER-INFO FINNS I SHIPPER-TAB I PROGRAMMET.                           
041200     MOVE +10                         TO SHIP-INDX                        
041300                                                                          
041400     MOVE SHIPPER-COMPANY (SHIP-INDX) TO MRP-BEGMT-RAD1                   
041500     MOVE SHIPPER-NAME    (SHIP-INDX) TO MRP-BEGMT-RAD2                   
041600     MOVE SHIPPER-STREET  (SHIP-INDX) TO MRP-ADGMT-GATA                   
041700     MOVE SHIPPER-CITY    (SHIP-INDX) TO MRP-ADGMT-PADR                   
041800     MOVE SHIPPER-COUNTRY (SHIP-INDX) TO MRP-ADGMT-LAND                   
041900                                                                          
042000*TELEFAX CONTAINS CITY INFO IN WNDCADRE.                                  
042100**   MOVE SHIPPER-TELEFAX (SHIP-INDX) TO MRP-ADCITY                       
042200     MOVE SHIPPER-TEL     (SHIP-INDX) TO MRP-IDTFN                        
042300     MOVE SHIPPER-IDMAIL  (SHIP-INDX) TO MRP-IDMAIL                       
042400                                                                          
042500     MOVE  WS-YEAR                    TO WS-TIYY                          
042600                                         W-DATE-AAMM(1:2)                 
042700     MOVE  WS-MONTH                   TO WS-TIMM                          
042800                                         W-DATE-AAMM(3:2)                 
042900     MOVE  WS-TIPRTDAT                TO MRP-TIPRTDAT                     
043000                                                                          
043100     MOVE W-IDARTNR                   TO MRP-IDARTNR                      
043200     MOVE '1'                         TO MRP-IDAFPRCD                     
043300     MOVE W-IDARTNR                   TO W-IDARTNR-WDC3                   
043400     MOVE 'IN'                        TO W-IDLANDX2                       
043500                                                                          
043600     PERFORM IMS-GU-WDK601                                                
043700*                                                                         
043800*    PERFORM BAA-KONVERTERA-KDSORT                                        
043900*                                                                         
044000                                                                          
044100*SUGGESTED RETAIL PRICE                                                   
044200     PERFORM IMS-GU-WDC311                                                
044300     IF SEGMENT-MISSING                                                   
044400                                                                          
044500       PERFORM IMS-GNP-WDK611                                             
044600                                                                          
044700       IF LABL-KVQPACK (INDX) > 0                                         
044800         MOVE CLAG-PRARTSJK           TO WS-CLAG-PRARTSJK                 
044900         COMPUTE WS-CLAG-PRARTSJK ROUNDED =                               
045000                 WS-CLAG-PRARTSJK * WS-KVQPACK-MRP                        
045100         END-COMPUTE                                                      
045200                                                                          
045300       ELSE                                                               
045400         MOVE CLAG-PRARTSJK           TO WS-CLAG-PRARTSJK                 
045500       END-IF                                                             
045600                                                                          
045700*      ARTIKELNS SJÄLVKOSTNAD (WDK611)                                    
045800       COMPUTE WS-CLAG-PRARTSJK ROUNDED =                                 
045900               WS-CLAG-PRARTSJK * 4                                       
046000       END-COMPUTE                                                        
046100                                                                          
046200*      TAX @ 18% ( ON SUGGESTED RETAIL PRICE)                             
046300       COMPUTE WS-CLAG-PRARTSJK ROUNDED = WS-CLAG-PRARTSJK * 1.18         
046400       END-COMPUTE                                                        
046500                                                                          
046600*      MARK-UP @10%                                                       
046700       COMPUTE WS-CLAG-PRARTSJK ROUNDED = WS-CLAG-PRARTSJK * 1.10         
046800       END-COMPUTE                                                        
046900                                                                          
047000       MOVE WS-CLAG-PRARTSJK          TO WS-PRARTBTO-SC                   
047100                                                                          
047200       MOVE 'INR'                     TO CURR-KDVALISO-ROW                
047300       MOVE W-DATE-AAMM               TO CURR-TIAAMM                      
047400       MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV                
047500       MOVE 'M'                       TO CURR-KDVALTYP                    
047600                                                                          
047700       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
047800       IF CURR-KDSVAR = ' '                                               
047900         COMPUTE WS-PRARTBTO-SC ROUNDED =                                 
048000                 WS-PRARTBTO-SC / CURR-PRKURS-NEW                         
048100         END-COMPUTE                                                      
048200                                                                          
048300*SUGGESTED   RETAIL PRICE IN INR (INDIAN RUPIES)                          
048400         MOVE WS-PRARTBTO-SC          TO WS-PRARTBTO-INT                  
048500         MOVE WS-PRARTBTO-INT         TO MRP-PRARTBTO-SC                  
048600       ELSE                                                               
048700         MOVE 0                       TO MRP-PRARTBTO-SC                  
048800         MOVE 0                       TO WS-PRARTBTO-SC                   
048900       END-IF                                                             
049000                                                                          
049100     ELSE                                                                 
049200*FETCH SUGGESTED RETAIL ON WDC301 FOR A FIXED MARKET (INR)                
049300*                                                                         
049400       MOVE SRP-PRARTBTO-SC           TO WS-PRARTBTO-SC                   
049500       IF LABL-KVQPACK (INDX) > 0                                         
049600         COMPUTE WS-PRARTBTO-SC  ROUNDED =                                
049700                 WS-PRARTBTO-SC * WS-KVQPACK-MRP                          
049800         END-COMPUTE                                                      
049900       END-IF                                                             
050000                                                                          
050100*      TAX @ 18% ( ON SUGGESTED RETAIL PRICE)                             
050200       COMPUTE WS-PRARTBTO-SC ROUNDED = WS-PRARTBTO-SC * 1.18             
050300       END-COMPUTE                                                        
050400                                                                          
050500*      MARK-UP @10%                                                       
050600       COMPUTE WS-PRARTBTO-SC ROUNDED = WS-PRARTBTO-SC * 1.10             
050700       END-COMPUTE                                                        
050800                                                                          
050900       MOVE WS-PRARTBTO-SC            TO WS-PRARTBTO-INT                  
051000       MOVE WS-PRARTBTO-INT           TO MRP-PRARTBTO-SC                  
051100     END-IF                                                               
051200     .                                                                    
051300                                                                          
051400*BAA-KONVERTERA-KDSORT    SECTION.                                        
051500*    MOVE 'BAA-KONVERTERA-KDSORT' TO WS-CURRENT-SECTION                   
051600*                                                                         
051700*    EVALUATE ART-KDSORT                                                  
051800*       WHEN 'ST'                                                         
051900*             MOVE 'PCS  '       TO MRP-KDSORT                            
052000*       WHEN 'PA'                                                         
052100*             MOVE 'PAIR '       TO MRP-KDSORT                            
052200*       WHEN 'MS'                                                         
052300*             MOVE 'AVREG'       TO MRP-KDSORT                            
052400*       WHEN 'KG'                                                         
052500*             MOVE 'LBS  '       TO MRP-KDSORT                            
052600*             MOVE 'KG   '       TO MRP-KDSORT                            
052700*       WHEN 'M '                                                         
052800*             MOVE 'FT   '       TO MRP-KDSORT                            
052900*             MOVE 'METER'       TO MRP-KDSORT                            
053000*       WHEN 'L '                                                         
053100*             MOVE 'GAL  '       TO MRP-KDSORT                            
053200*             MOVE 'LITRE'       TO MRP-KDSORT                            
053300*       WHEN 'SA'                                                         
053400*             MOVE 'KIT  '       TO MRP-KDSORT                            
053500*       WHEN 'MM'                                                         
053600*             MOVE 'INCH '       TO MRP-KDSORT                            
053700*             MOVE 'MM   '       TO MRP-KDSORT                            
053800*       WHEN 'G '                                                         
053900*             MOVE 'OZ   '       TO MRP-KDSORT                            
054000*             MOVE 'GRAM '       TO MRP-KDSORT                            
054100*       WHEN 'C2'                                                         
054200*             MOVE 'SQ.IN'       TO MRP-KDSORT                            
054300*             MOVE 'CM2  '       TO MRP-KDSORT                            
054400*       WHEN 'M2'                                                         
054500*             MOVE 'SQ.FT'       TO MRP-KDSORT                            
054600*             MOVE 'M2   '       TO MRP-KDSORT                            
054700*       WHEN 'M3'                                                         
054800*             MOVE 'QU.FT'       TO MRP-KDSORT                            
054900*             MOVE 'M3   '       TO MRP-KDSORT                            
055000*       WHEN 'ML'                                                         
055100*             MOVE 'CU.IN'       TO MRP-KDSORT                            
055200*             MOVE 'ML   '       TO MRP-KDSORT                            
055300*       WHEN 'RA'                                                         
055400*             MOVE 'LINES'       TO MRP-KDSORT                            
055500*       WHEN 'SW'                                                         
055600*             MOVE 'SOFTW'       TO MRP-KDSORT                            
055700*       WHEN 'HW'                                                         
055800*             MOVE 'HARDW'       TO MRP-KDSORT                            
055900*       WHEN 'TM'                                                         
056000*             MOVE 'TMO  '       TO MRP-KDSORT                            
056100*       WHEN OTHER                                                        
056200*             MOVE SPACE         TO MRP-KDSORT                            
056300*    END-EVALUATE                                                         
056400*    .                                                                    
056500*                                                                         
056600 BB-MOVE-KIT-INFO SECTION.                                                
056700                                                                          
056800     MOVE 1                      TO KIT-TAB-IX                            
056900     PERFORM UNTIL KIT-TAB-IX > MAX-NO-OF-CONTAIN-PARTS                   
057000                                                                          
057100       MOVE ZERO                 TO MRP-KIT-REANTPSA(KIT-TAB-IX)          
057200       MOVE SPACE                TO MRP-KIT-BEART(KIT-TAB-IX)             
057300       MOVE SPACE                TO MRP-KIT-KDSORT(KIT-TAB-IX)            
057400       ADD  1                    TO KIT-TAB-IX                            
057500     END-PERFORM                                                          
057600     MOVE 1                      TO KIT-TAB-IX                            
057700                                                                          
057800     MOVE NOO                    TO SW-WDJ4-DATA                          
057900                                                                          
058000     PERFORM IMS-GU-WDJ101                                                
058100     IF SEGMENT-FOUND                                                     
058200       PERFORM IMS-GNP-WDJ111                                             
058300     END-IF                                                               
058400                                                                          
058500     IF SEGMENT-MISSING AND                                               
058600        ART-KDSORT = 'SA'                                                 
058700       PERFORM IMS-GU-WDJ401                                              
058800       IF SEGMENT-FOUND                                                   
058900         PERFORM IMS-GNP-WDJ411                                           
059000         IF SEGMENT-FOUND                                                 
059100           MOVE YES              TO SW-WDJ4-DATA                          
059200         END-IF                                                           
059300       END-IF                                                             
059400     END-IF                                                               
059500                                                                          
059600     MOVE 0                      TO NUM-PARTS-IN-KIT-PART                 
059700     MOVE +1                     TO KIT-TAB-IX                            
059800                                                                          
059900     IF SEGMENT-FOUND                                                     
060000       PERFORM                                                            
060100         UNTIL SEGMENT-MISSING OR                                         
060200               KIT-TAB-IX > MAX-NO-OF-CONTAIN-PARTS                       
060300         IF WDJ4-DATA                                                     
060400           PERFORM BBA-GET-KDP-DATA                                       
060500           ADD +1                TO KIT-TAB-IX                            
060600         ELSE                                                             
060700           IF RAD-IDARTNR > ZERO                                          
060800             PERFORM BBB-CHECK-KIT-ROW-STATUS                             
060900             IF KIT-STATUS-OK                                             
061000               PERFORM BBC-GET-KIT-DESCRIPTION                            
061100               ADD +1            TO KIT-TAB-IX                            
061200             END-IF                                                       
061300           END-IF                                                         
061400         END-IF                                                           
061500                                                                          
061600         IF WDJ4-DATA                                                     
061700           PERFORM IMS-GNP-WDJ411                                         
061800         ELSE                                                             
061900           PERFORM IMS-GNP-WDJ111                                         
062000         END-IF                                                           
062100       END-PERFORM                                                        
062200     END-IF                                                               
062300                                                                          
062400     MOVE NUM-PARTS-IN-KIT-PART  TO MRP-KIT-KVRADER                       
062500     .                                                                    
062600                                                                          
062700 BBA-GET-KDP-DATA SECTION.                                                
062800                                                                          
062900     MOVE 'BBA-GET-KDP-DATA '    TO WS-CURRENT-SECTION                    
063000                                                                          
063100     MOVE KSRAD-KVANTAL          TO MRP-KIT-REANTPSA(KIT-TAB-IX)          
063200     MOVE ART-KDSORT             TO MRP-KIT-KDSORT  (KIT-TAB-IX)          
063300                                                                          
063400     EVALUATE ART-KDSORT                                                  
063500       WHEN '  '                                                          
063600         MOVE 'N    '            TO MRP-KIT-KDSORT  (KIT-TAB-IX)          
063700       WHEN 'ST'                                                          
063800         MOVE 'N    '            TO MRP-KIT-KDSORT  (KIT-TAB-IX)          
063900     END-EVALUATE                                                         
064000                                                                          
064100     ADD 1                       TO NUM-PARTS-IN-KIT-PART                 
064200                                                                          
064300     IF KSRAD-BEART           > SPACE                                     
064400       MOVE KSRAD-BEART          TO MRP-KIT-BEART(KIT-TAB-IX)             
064500     ELSE                                                                 
064600       MOVE 'TEXT MISSIN2'       TO MRP-KIT-BEART(KIT-TAB-IX)             
064700     END-IF                                                               
064800     .                                                                    
064900                                                                          
065000 BBB-CHECK-KIT-ROW-STATUS SECTION.                                        
065100                                                                          
065200     MOVE 'BBB-CHECK-KIT-ROW-STATUS'                                      
065300                                 TO WS-CURRENT-SECTION                    
065400                                                                          
065500     MOVE NOO                    TO KIT-STATUS-SW                         
065600                                                                          
065700     IF RAD-KDISATS = 'N' OR 'T' OR 'E'                                   
065800       MOVE YES                  TO KIT-STATUS-SW                         
065900     ELSE                                                                 
066000       MOVE RAD-TISTODAT         TO TMP1-YYMMDD                           
066100       MOVE WS-DATUM             TO TMP2-YYMMDD                           
066200       PERFORM WY2000P1                                                   
066300                                                                          
066400       IF (RAD-KDISATS = 'U' OR ' ') AND                                  
066500          TMP1-YYMMDD > TMP2-YYMMDD                                       
066600         MOVE YES                TO KIT-STATUS-SW                         
066700       END-IF                                                             
066800     END-IF                                                               
066900     .                                                                    
067000                                                                          
067100 BBC-GET-KIT-DESCRIPTION            SECTION.                              
067200     MOVE 'BBC-GET-KIT-DESCRIPTION'                                       
067300                                 TO  WS-CURRENT-SECTION                   
067400*                                                                         
067500*BEART FRÅN WDK6 ELLER WDD3                                               
067600*                                                                         
067700     MOVE RAD-IDARTNR            TO W-IDARTNR                             
067800     MOVE RAD-REANTPSA           TO MRP-KIT-REANTPSA(KIT-TAB-IX)          
067900     MOVE RAD-KDSORT             TO MRP-KIT-KDSORT  (KIT-TAB-IX)          
068000                                                                          
068100     EVALUATE RAD-KDSORT                                                  
068200       WHEN '  '                                                          
068300         MOVE 'N    '            TO MRP-KIT-KDSORT  (KIT-TAB-IX)          
068400       WHEN 'ST'                                                          
068500         MOVE 'N    '            TO MRP-KIT-KDSORT  (KIT-TAB-IX)          
068600     END-EVALUATE                                                         
068700                                                                          
068800*GB IDSKYLT FOR INDIA.                                                    
068900     MOVE 'GB'                   TO W-IDSKYLT-X                           
069000     MOVE RAD-IDARTNR            TO W-D3BSEQ-IDARTNR                      
069100     PERFORM IMS-GU-WDD311                                                
069200     IF SEGMENT-FOUND                                                     
069300       MOVE TEXT-BEART           TO MRP-KIT-BEART(KIT-TAB-IX)             
069400     ELSE                                                                 
069500       MOVE 'TEXT MISSING'       TO MRP-KIT-BEART(KIT-TAB-IX)             
069600       PERFORM BBCA-GET-BEART                                             
069700     END-IF                                                               
069800                                                                          
069900     ADD 1                       TO NUM-PARTS-IN-KIT-PART                 
070000     .                                                                    
070100                                                                          
070200 BBCA-GET-BEART            SECTION.                                       
070300     MOVE 'BBCA-GET-BEART '      TO WS-CURRENT-SECTION                    
070400                                                                          
070500     MOVE RAD-IDARTNR            TO W-IDARTNR                             
070600                                                                          
070700     IF RAD-BEART-SVE = SPACE                                             
070800       PERFORM IMS-GU-WDK601                                              
070900       IF SEGMENT-FOUND                                                   
071000         MOVE 'GB'               TO W-IDSKYLT-X                           
071100         MOVE RAD-IDARTNR        TO W-D3BSEQ-IDARTNR                      
071200         PERFORM IMS-GU-WDD311                                            
071300         IF SEGMENT-FOUND                                                 
071400           MOVE TEXT-BEART       TO MRP-KIT-BEART(KIT-TAB-IX)             
071500         END-IF                                                           
071600       END-IF                                                             
071700     ELSE                                                                 
071800       IF W-IDSKYLT-X NOT = 'S  '                                         
071900         MOVE 'S'                TO W-IDSKYLT-X                           
072000         MOVE RAD-BEART-SVE      TO W-BEART                               
072100         MOVE RAD-KDBENHOM       TO SPAR-RAD-KDBENHOM                     
072200         PERFORM IMS-GU-WDD301-ASEQ                                       
072300         PERFORM                                                          
072400           UNTIL (SEGMENT-MISSING) OR                                     
072500                 (BEN-KDHOMONYM = SPAR-RAD-KDBENHOM)                      
072600           IF SEGMENT-FOUND                                               
072700             IF BEN-KDHOMONYM = SPAR-RAD-KDBENHOM                         
072800               CONTINUE                                                   
072900             ELSE                                                         
073000               PERFORM IMS-GN-WDD301-ASEQ                                 
073100             END-IF                                                       
073200           END-IF                                                         
073300         END-PERFORM                                                      
073400                                                                          
073500         IF SEGMENT-FOUND                                                 
073600           MOVE 'GB'           TO W-IDSKYLT-X                             
073700           PERFORM IMS-GNP-WDD311-ASEQ                                    
073800           IF SEGMENT-FOUND                                               
073900             MOVE TEXT-BEART   TO MRP-KIT-BEART(KIT-TAB-IX)               
074000           END-IF                                                         
074100         END-IF                                                           
074200       END-IF                                                             
074300     END-IF                                                               
074400     .                                                                    
074500                                                                          
074600 S90-PUT-MRP-HEADER     SECTION.                                          
074700     MOVE 'S90-PUT-MRP-HEADER  ' TO WS-CURRENT-SECTION                    
074800                                                                          
074900     MOVE 1                      TO HDR-REQU-IDMSGVER                     
075000     MOVE REQU-KDPGMACT          TO HDR-REQU-KDPGMACT                     
075100     MOVE REQU-IDUSER            TO HDR-REQU-IDUSER                       
075200     MOVE 'MRP-LABEL'            TO HDR-IDOUTTYPE                         
075300     MOVE LABL-IDDC              TO HDR-IDOUTREC(1:2)                     
075400     MOVE REQU-IDUSER            TO HDR-IDOUTREC(3:)                      
075500     MOVE 001                    TO HDR-IDLIST                            
075600                                                                          
075700     IF LABL-IDLIST = SPACES                                              
075800       MOVE WS-TIYYMMDDHHMM      TO HDR-IDLIST                            
075900     ELSE                                                                 
076000       MOVE LABL-IDLIST          TO HDR-IDLIST                            
076100     END-IF                                                               
076200                                                                          
076300     MOVE 'PUT'                  TO SEND-KDFUNC                           
076400     MOVE LENGTH OF HDR-AREA     TO SEND-KVDLEN                           
076500                                                                          
076600     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
076700                                    SEND-KVDLEN                           
076800                                    HDR-AREA                              
076900                                                                          
077000     IF SEND-KDRC > ZERO                                                  
077100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
077200       STRING 'WZ01SEND GET    ERROR RC= ' KDRC-DISPLAY                   
077300         DELIMITED BY SIZE     INTO ERROR-TEXT                            
077400       DISPLAY ERROR-TEXT                                                 
077500       CALL FELLOG                                                        
077600     END-IF                                                               
077700     .                                                                    
077800                                                                          
077900 S90-OPEN-MRP   SECTION.                                                  
078000     MOVE 'S90-OPEN-MRP '                                                 
078100                                 TO WS-CURRENT-SECTION                    
078200*MRP INDIA LABEL                                                          
078300*                                                                         
078400     MOVE 'CARPARTS.DAP.DISTRDOCWEB'                                      
078500                                 TO SEND-ADDISPABS                        
078600     MOVE 'OPEN'                 TO SEND-KDFUNC                           
078700     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
078800                                    SEND-OPEN-AREA                        
078900     IF SEND-KDRC > ZERO                                                  
079000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
079100       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
079200         DELIMITED BY SIZE     INTO ERROR-TEXT                            
079300       DISPLAY ERROR-TEXT                                                 
079400       CALL FELLOG                                                        
079500     END-IF                                                               
079600     .                                                                    
079700                                                                          
079800 S90-CLOSE-MRP   SECTION.                                                 
079900     MOVE 'S90-CLOSE-MRP'   TO WS-CURRENT-SECTION                         
080000                                                                          
080100*MRP INDIA LABEL                                                          
080200                                                                          
080300     MOVE 'CLOSE'                TO SEND-KDFUNC                           
080400     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
080500                                                                          
080600     IF SEND-KDRC > 0                                                     
080700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
080800       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
080900       DELIMITED BY SIZE       INTO ERROR-TEXT                            
081000       DISPLAY ERROR-TEXT                                                 
081100       CALL FELLOG                                                        
081200     END-IF                                                               
081300     .                                                                    
081400                                                                          
081500 S90-PUT-MRP SECTION.                                                     
081600     MOVE 'S90-PUT-MRP '         TO WS-CURRENT-SECTION                    
081700                                                                          
081800     MOVE 'PUT'                  TO SEND-KDFUNC                           
081900     MOVE LENGTH OF MRP-LABEL-AREA                                        
082000                                 TO SEND-KVDLEN                           
082100                                                                          
082200     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
082300                                    SEND-KVDLEN                           
082400                                    MRP-LABEL-AREA                        
082500                                                                          
082600     IF SEND-KDRC > ZERO                                                  
082700       MOVE SEND-KDRC        TO KDRC-DISPLAY                              
082800       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
082900       DELIMITED BY SIZE       INTO ERROR-TEXT                            
083000       DISPLAY ERROR-TEXT                                                 
083100       CALL FELLOG                                                        
083200     END-IF                                                               
083300     .                                                                    
083400                                                                          
083500 S90-PUT-MRP-SUMMARY SECTION.                                             
083600     MOVE 'S90-PUT-MRP-SUMMARY'  TO WS-CURRENT-SECTION                    
083700                                                                          
083800     MOVE 'PUT'                  TO SEND-KDFUNC                           
083900     MOVE LENGTH OF PLK-SUMM-WPLKSUMM                                     
084000                                 TO SEND-KVDLEN                           
084100                                                                          
084200     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
084300                                    SEND-KVDLEN                           
084400                                    PLK-SUMM-WPLKSUMM                     
084500                                                                          
084600     IF SEND-KDRC > ZERO                                                  
084700       MOVE SEND-KDRC        TO KDRC-DISPLAY                              
084800       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
084900       DELIMITED BY SIZE       INTO ERROR-TEXT                            
085000       DISPLAY ERROR-TEXT                                                 
085100       CALL FELLOG                                                        
085200     END-IF                                                               
085300     .                                                                    
085400                                                                          
085500 S90-PUT-KIT       SECTION.                                               
085600     MOVE 'S90-PUT-KIT'          TO WS-CURRENT-SECTION                    
085700                                                                          
085800     IF KIT-DAP-CLOSE                                                     
085900       PERFORM S90-OPEN-KIT                                               
086000       PERFORM S90-PUT-KIT-HEADER                                         
086100       SET KIT-DAP-OPEN          TO TRUE                                  
086200     END-IF                                                               
086300                                                                          
086400     MOVE 'PUT'                  TO KIT-SEND-KDFUNC                       
086500     MOVE LENGTH OF MRP-KIT-LABEL-AREA                                    
086600                                 TO KIT-SEND-KVDLEN                       
086700                                                                          
086800     CALL WZ01SEND            USING KIT-SEND-CONTROL-AREA                 
086900                                    KIT-SEND-KVDLEN                       
087000                                    MRP-KIT-LABEL-AREA                    
087100                                                                          
087200     IF KIT-SEND-KDRC > ZERO                                              
087300       MOVE KIT-SEND-KDRC        TO KDRC-DISPLAY                          
087400       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
087500         DELIMITED BY SIZE     INTO ERROR-TEXT                            
087600       DISPLAY ERROR-TEXT                                                 
087700       CALL FELLOG                                                        
087800     END-IF                                                               
087900     .                                                                    
088000                                                                          
088100 S90-OPEN-KIT   SECTION.                                                  
088200     MOVE 'S90-OPEN-KIT '                                                 
088300                                 TO WS-CURRENT-SECTION                    
088400*MRP INDIA LABEL                                                          
088500*                                                                         
088600     MOVE 'CARPARTS.DAP.DISTRDO2'                                         
088700                                 TO KIT-SEND-ADDISPABS                    
088800     MOVE 'OPEN'                 TO KIT-SEND-KDFUNC                       
088900     CALL WZ01SEND            USING KIT-SEND-CONTROL-AREA                 
089000                                    KIT-SEND-OPEN-AREA                    
089100     IF KIT-SEND-KDRC > ZERO                                              
089200       MOVE KIT-SEND-KDRC            TO KDRC-DISPLAY                      
089300       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
089400         DELIMITED BY SIZE     INTO ERROR-TEXT                            
089500       DISPLAY ERROR-TEXT                                                 
089600       CALL FELLOG                                                        
089700     END-IF                                                               
089800     .                                                                    
089900                                                                          
090000 S90-PUT-KIT-HEADER SECTION.                                              
090100     MOVE 'S90-PUT-KIT-HEADER  '                                          
090200                                 TO WS-CURRENT-SECTION                    
090300                                                                          
090400     MOVE 1                      TO KIT-REQU-IDMSGVER                     
090500     MOVE REQU-KDPGMACT          TO KIT-REQU-KDPGMACT                     
090600     MOVE REQU-IDUSER            TO KIT-REQU-IDUSER                       
090700     MOVE 'MRP-KIT-LABEL2'       TO KIT-HDR-IDOUTTYPE                     
090800     MOVE LABL-IDDC              TO KIT-HDR-IDOUTREC(1:2)                 
090900     MOVE REQU-IDUSER            TO KIT-HDR-IDOUTREC(3:)                  
091000     MOVE 001                    TO KIT-HDR-IDLIST                        
091100                                                                          
091200     IF LABL-IDLIST = SPACES                                              
091300       MOVE WS-TIYYMMDDHHMM      TO KIT-HDR-IDLIST                        
091400     ELSE                                                                 
091500       MOVE LABL-IDLIST          TO KIT-HDR-IDLIST                        
091600     END-IF                                                               
091700                                                                          
091800     MOVE 'PUT'                  TO KIT-SEND-KDFUNC                       
091900     MOVE LENGTH OF KIT-HDR-AREA TO KIT-SEND-KVDLEN                       
092000                                                                          
092100     CALL WZ01SEND            USING KIT-SEND-CONTROL-AREA                 
092200                                    KIT-SEND-KVDLEN                       
092300                                    KIT-HDR-AREA                          
092400                                                                          
092500     IF KIT-SEND-KDRC > ZERO                                              
092600       MOVE KIT-SEND-KDRC            TO KDRC-DISPLAY                      
092700       STRING 'WZ01SEND GET    ERROR RC= ' KDRC-DISPLAY                   
092800         DELIMITED BY SIZE     INTO ERROR-TEXT                            
092900       DISPLAY ERROR-TEXT                                                 
093000       CALL FELLOG                                                        
093100     END-IF                                                               
093200     .                                                                    
093300                                                                          
093400 S90-CLOSE-KIT   SECTION.                                                 
093500     MOVE 'S90-CLOSE-KIT'        TO WS-CURRENT-SECTION                    
093600                                                                          
093700*MRP INDIA LABEL                                                          
093800                                                                          
093900     MOVE 'CLOSE'                TO KIT-SEND-KDFUNC                       
094000     CALL WZ01SEND            USING KIT-SEND-CONTROL-AREA                 
094100     SET KIT-DAP-CLOSE           TO TRUE                                  
094200                                                                          
094300     IF KIT-SEND-KDRC > 0                                                 
094400       MOVE KIT-SEND-KDRC            TO KDRC-DISPLAY                      
094500       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
094600       DELIMITED BY SIZE       INTO ERROR-TEXT                            
094700       DISPLAY ERROR-TEXT                                                 
094800       CALL FELLOG                                                        
094900     END-IF                                                               
095000     .                                                                    
095100                                                                          
095200* --- IMS SEKTIONER ---                                                   
095300                                                                          
095400 IMS-GU-WDC311 SECTION.                                                   
095500     STRING 'WDC301  (IDARTNR  =' W-WDC301KY-X ')'                        
095600          DELIMITED BY SIZE INTO SSA1                                     
095700     STRING 'WDC311  (IDLANDX2 =' W-WDC311KY-X ')'                        
095800          DELIMITED BY SIZE INTO SSA2                                     
095900     MOVE '  GE' TO GODK-STATUSKODER                                      
096000     CALL CBLTDLI USING GU WDC3-PCB DLI-IO-WDC311 SSA1 SSA2               
096100     MOVE WDC3-STATUS-CODE TO STATUS-WS                                   
096200     PERFORM IMS-STATUSKONTROLL                                           
096300     .                                                                    
096400                                                                          
096500     SKIP3                                                                
096600                                                                          
096700 IMS-GU-WDK601 SECTION.                                                   
096800                                                                          
096900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
097000     DELIMITED BY SIZE INTO SSA1                                          
097100     MOVE '  GE' TO GODK-STATUSKODER                                      
097200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
097300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
097400     PERFORM IMS-STATUSKONTROLL                                           
097500     .                                                                    
097600                                                                          
097700 IMS-GNP-WDK611 SECTION.                                                  
097800                                                                          
097900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
098000     DELIMITED BY SIZE INTO SSA1                                          
098100     MOVE '  GE' TO GODK-STATUSKODER                                      
098200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
098300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
098400     PERFORM IMS-STATUSKONTROLL                                           
098500     .                                                                    
098600                                                                          
098700 IMS-GU-WDD311 SECTION.                                                   
098800                                                                          
098900     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
099000          DELIMITED BY SIZE INTO SSA1                                     
099100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
099200          DELIMITED BY SIZE INTO SSA2                                     
099300     MOVE '  GE' TO GODK-STATUSKODER                                      
099400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
099500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
099600     PERFORM IMS-STATUSKONTROLL                                           
099700     .                                                                    
099800                                                                          
099900*WDD3-ASEQ IMS-SECTION TO GET DESCRIPTION                                 
100000 IMS-GU-WDD301-ASEQ SECTION.                                              
100100                                                                          
100200     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X                             
100300                                  W-BEART-X ')'                           
100400             DELIMITED BY SIZE INTO SSA1                                  
100500     MOVE '  GE'                 TO GODK-STATUSKODER                      
100600     CALL CBLTDLI             USING GU                                    
100700                                    WDD3A-PCB                             
100800                                    DLI-IO-WDD301                         
100900                                    SSA1                                  
101000     MOVE WDD3A-STATUS-CODE      TO STATUS-WS                             
101100     PERFORM IMS-STATUSKONTROLL                                           
101200     .                                                                    
101300                                                                          
101400 IMS-GN-WDD301-ASEQ      SECTION.                                         
101500                                                                          
101600     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X                             
101700                                  W-BEART-X ')'                           
101800             DELIMITED BY SIZE INTO SSA1                                  
101900     MOVE '  GE'                 TO GODK-STATUSKODER                      
102000     CALL CBLTDLI             USING GN                                    
102100                                    WDD3A-PCB DLI-IO-WDD301               
102200                                    SSA1                                  
102300     MOVE WDD3A-STATUS-CODE      TO STATUS-WS                             
102400     PERFORM IMS-STATUSKONTROLL                                           
102500     .                                                                    
102600                                                                          
102700 IMS-GNP-WDD311-ASEQ SECTION.                                             
102800                                                                          
102900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
103000             DELIMITED BY SIZE INTO SSA1                                  
103100     MOVE '  GE'                 TO GODK-STATUSKODER                      
103200     CALL CBLTDLI             USING GNP                                   
103300                                    WDD3A-PCB                             
103400                                    DLI-IO-WDD311                         
103500                                    SSA1                                  
103600     MOVE WDD3A-STATUS-CODE      TO STATUS-WS                             
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     .                                                                    
103900                                                                          
104000 IMS-GU-WDJ101 SECTION.                                                   
104100     MOVE 'IMS-GU-WDJ101       ' TO WS-CURRENT-IMS-SECTION                
104200                                                                          
104300     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
104400             DELIMITED BY SIZE INTO SSA1                                  
104500     MOVE '  GE'                 TO GODK-STATUSKODER                      
104600     CALL CBLTDLI             USING GU                                    
104700                                    WDJ1-PCB                              
104800                                    DLI-IO-WDJ101                         
104900                                    SSA1                                  
105000     MOVE WDJ1-STATUS-CODE       TO STATUS-WS                             
105100     PERFORM IMS-STATUSKONTROLL                                           
105200     .                                                                    
105300     SKIP3                                                                
105400                                                                          
105500 IMS-GNP-WDJ111 SECTION.                                                  
105600     MOVE 'IMS-GNP-WDJ111      ' TO WS-CURRENT-IMS-SECTION                
105700                                                                          
105800     MOVE 'WDJ111  '             TO SSA1                                  
105900     MOVE '  GE'                 TO GODK-STATUSKODER                      
106000     CALL CBLTDLI             USING GNP                                   
106100                                    WDJ1-PCB                              
106200                                    DLI-IO-WDJ111                         
106300                                    SSA1                                  
106400     MOVE WDJ1-STATUS-CODE       TO STATUS-WS                             
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700                                                                          
106800                                                                          
106900*NEW                                                                      
107000 IMS-GU-WDJ401 SECTION.                                                   
107100     MOVE 'IMS-GU-WDJ401       ' TO WS-CURRENT-IMS-SECTION                
107200                                                                          
107300     STRING 'WDJ401  (IDARTNRS =' W-IDARTNR-SATS-X ')'                    
107400             DELIMITED BY SIZE INTO SSA1                                  
107500     MOVE '  GE'                 TO GODK-STATUSKODER                      
107600     CALL CBLTDLI             USING GU                                    
107700                                    WDJ4-PCB                              
107800                                    DLI-IO-WDJ401                         
107900                                    SSA1                                  
108000     MOVE WDJ4-STATUS-CODE       TO STATUS-WS                             
108100     PERFORM IMS-STATUSKONTROLL                                           
108200     .                                                                    
108300                                                                          
108400 IMS-GNP-WDJ411 SECTION.                                                  
108500     MOVE 'IMS-GNP-WDJ411      ' TO WS-CURRENT-IMS-SECTION                
108600                                                                          
108700     MOVE 'WDJ411  '             TO SSA1                                  
108800     MOVE '  GE'                 TO GODK-STATUSKODER                      
108900     CALL CBLTDLI             USING GNP                                   
109000                                    WDJ4-PCB                              
109100                                    DLI-IO-WDJ411                         
109200                                    SSA1                                  
109300     MOVE WDJ4-STATUS-CODE       TO STATUS-WS                             
109400     PERFORM IMS-STATUSKONTROLL                                           
109500     .                                                                    
109600                                                                          
109700 IMS-STATUSKONTROLL SECTION.                                              
109800                                                                          
109900     SET STATUS-IX TO 1                                                   
110000     SEARCH GODK-STATUS                                                   
110100       AT END                                                             
110200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
110300         DELIMITED BY SIZE INTO FELTEXT                                   
110400         CALL FELLOG                                                      
110500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
110600         CONTINUE                                                         
110700     END-SEARCH                                                           
110800     .                                                                    
110900                                                                          
111000*    -COPY WY2000P1                                                       
